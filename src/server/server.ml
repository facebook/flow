(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open Utils_js

let sample_init_memory profiling =
  let open SharedMem_js in
  let dep_stats = dep_stats () in
  let hash_stats = hash_stats () in
  let heap_size = heap_size () in
  let memory_metrics = [
    "heap.size", heap_size;
    "dep_table.nonempty_slots", dep_stats.nonempty_slots;
    "dep_table.used_slots", dep_stats.used_slots;
    "dep_table.slots", dep_stats.slots;
    "hash_table.nonempty_slots", hash_stats.nonempty_slots;
    "hash_table.used_slots", hash_stats.used_slots;
    "hash_table.slots", hash_stats.slots;
  ] in
  List.iter (fun (metric, value) ->
    Profiling_js.sample_memory
      ~metric:("init_done." ^ metric)
      ~value:(float_of_int value)
       profiling
  ) memory_metrics

let init ~focus_targets genv =
  (* write binary path and version to server log *)
  Hh_logger.info "executable=%s" (Sys_utils.executable_path ());
  Hh_logger.info "version=%s" Flow_version.version;

  let workers = genv.ServerEnv.workers in
  let options = genv.ServerEnv.options in

  MonitorRPC.status_update ~event:ServerStatus.Init_start;

  let should_print_summary = Options.should_profile options in
  let%lwt env = Profiling_js.with_profiling_lwt ~should_print_summary begin fun profiling ->
    let%lwt parsed, libs, libs_ok, errors = Types_js.init ~profiling ~workers options in

    (* If any libs errored, skip typechecking and just show lib errors. Note
     * that `init` above has done all parsing, not just lib parsing, resolved
     * and committed modules, etc.
     *
     * Furthermore, if we're in lazy mode, we forego typechecking until later,
     * when it proceeds on an as-needed basis. *)
    let%lwt checked, errors =
      if not libs_ok || Options.is_lazy_mode options then
        Lwt.return (CheckedSet.empty, errors)
      else
        Types_js.full_check ~profiling ~workers ~focus_targets ~options parsed errors
    in

    sample_init_memory profiling;

    SharedMem_js.init_done();

    (* Return an env that initializes invariants required and maintained by
       recheck, namely that `files` contains files that parsed successfully, and
       `errors` contains the current set of errors. *)
    let env = { ServerEnv.
      files = parsed;
      checked_files = checked;
      libs;
      errors;
      collated_errors = ref None;
      connections = Persistent_connection.empty;
    } in
    Lwt.return env
  end in
  MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
  Lwt.return env

let get_watch_paths options =
  Files.watched_paths (Options.file_options options)

let listen_for_messages, get_next_message =
  let message_stream, push_new_message = Lwt_stream.create () in

  (* This is a thread that just keeps looping in the background. It reads messages from the
   * monitor process and adds them to a stream *)
  let module ListenLoop = LwtLoop.Make (struct
    type acc = unit
    let main () =
      let%lwt message = MonitorRPC.read () in
      push_new_message (Some message);
      Lwt.return_unit
    let catch () exn = raise exn
  end)
  in

  let listen_for_messages = ListenLoop.run
  in

  (* This function will return the next message from the monitor process, with a 1 second timeout
   * if there are no messages *)
  let get_next_message () =
    (* A thread that returns None after 1 second *)
    let timeout =
      let%lwt () = Lwt_unix.sleep 1.0 in
      Lwt.return None
    in
    (* A thread that returns the next message when it's available *)
    let get_message =
      let%lwt message = Lwt_stream.next message_stream in
      Lwt.return (Some message)
    in
    (* Pick the first thread that completes and cancel the other *)
    Lwt.pick [ timeout; get_message ]
  in

  listen_for_messages, get_next_message

let exit_due_to_dfind_dying ~genv e =
  let root = Options.root genv.options in
  let tmp_dir = Options.temp_dir genv.options in
  let dfind_logs = Sys_utils.cat_no_fail (Server_files_js.dfind_log_file ~tmp_dir root) in
  let logs_len = String.length dfind_logs in
  (* Let's limit how much of the log we stick in the exit message *)
  let max_len = 2000 in
  let dfind_logs = if logs_len > max_len
    then String.sub dfind_logs (logs_len - max_len) max_len
    else dfind_logs in
  let msg = spf
    "dfind died (got exception: %s)\ndfind logs:\n%s"
    (Printexc.to_string e)
    dfind_logs in
  FlowExitStatus.(exit Dfind_died ~msg)

(* When a rebase occurs, dfind takes a while to give us the full list of
 * updates, and it often comes in batches. To get an accurate measurement
 * of rebase time, we use the heuristic that any changes that come in
 * right after one rechecking round finishes to be part of the same
 * rebase, and we don't log the recheck_end event until the update list
 * is no longer getting populated. *)
let rec recheck_loop ~dfind genv env =
  let raw_updates =
    if Options.use_file_watcher genv.options
    then
      try DfindLib.get_changes dfind
      with
      | Sys_error msg as e when msg = "Broken pipe" -> exit_due_to_dfind_dying ~genv e
      | End_of_file as e -> exit_due_to_dfind_dying ~genv e
    else SSet.empty
  in
  if SSet.is_empty raw_updates then Lwt.return env else begin
    let updates = Rechecker.process_updates genv env raw_updates in
    let%lwt _profiling, env = Rechecker.recheck genv env updates in
    recheck_loop ~dfind genv env
  end

let process_message genv env request =
  ServerPeriodical.stamp_connection ();
  match request with
  | MonitorProt.Request (request_id, command) ->
    CommandHandler.handle_ephemeral genv env (request_id, command)
  | MonitorProt.NewPersistentConnection (client_id, logging_context, lsp) ->
    Lwt.return
      { env with
        connections = Persistent_connection.add_client env.connections client_id logging_context lsp
      }
  | MonitorProt.PersistentConnectionRequest (client_id, request) ->
    CommandHandler.handle_persistent genv env client_id request
  | MonitorProt.DeadPersistentConnection client_id ->
    Lwt.return
      { env with connections = Persistent_connection.remove_client env.connections client_id }

let rec serve ~dfind ~genv ~env =
  MonitorRPC.status_update ~event:ServerStatus.Ready;

  ServerPeriodical.call_before_sleeping ();
  let%lwt message = get_next_message () in

  let%lwt env = recheck_loop ~dfind genv env in

  let%lwt env = Option.value_map message
    ~default:(Lwt.return env)
    ~f:(process_message genv env) in

  EventLogger.flush ();
  serve ~dfind ~genv ~env

(* This code is only executed when the options --check is NOT present *)
let with_init_lock init_fun =
  let t = Unix.gettimeofday () in
  Hh_logger.info "Initializing Server (This might take some time)";
  let%lwt _profiling, env = init_fun () in
  Hh_logger.info "Server is READY";
  let t' = Unix.gettimeofday () in
  Hh_logger.info "Took %f seconds to initialize." (t' -. t);
  Lwt.return env

let init_dfind options =
  let tmp_dir = Options.temp_dir options in
  let root = Options.root options in
  let in_fd = Daemon.null_fd () in
  let log_file = Server_files_js.dfind_log_file ~tmp_dir root in
  let log_fd = Daemon.fd_of_path log_file in
  let fds = (in_fd, log_fd, log_fd) in
  let watch_paths = get_watch_paths options in
  Hh_logger.info "Watching paths: \n%s" (String.concat "\n" (List.map (fun p -> spf "\t%s" (Path.to_string p)) watch_paths));
  DfindLib.init fds ("flow_server_events", watch_paths)


(* The main entry point of the daemon
* the only trick to understand here, is that env.modified is the set
* of files that changed, it is only set back to SSet.empty when the
* type-checker succeeded. So to know if there is some work to be done,
* we look if env.modified changed.
*)
let create_program_init ~shared_mem_config ~focus_targets options =
  let handle = SharedMem_js.init shared_mem_config in
  let genv = ServerEnvBuild.make_genv options handle in
  (* Only set up lwt after the workers are created *)
  LwtInit.set_engine ();
  let program_init = fun () ->
    let%lwt profiling, env = init ~focus_targets genv in
    FlowEventLogger.init_done ~profiling;
    Lwt.return (profiling, env)
  in
  genv, program_init

let run ~monitor_channels ~shared_mem_config options =
  MonitorRPC.init ~channels:monitor_channels;
  let genv, program_init =
    create_program_init ~shared_mem_config ~focus_targets:None options in

  let dfind = init_dfind options in

  let initial_lwt_thread =
    (* Read messages from the server monitor and add them to a stream as they come in *)
    let listening_thread = listen_for_messages () in

    let%lwt env = with_init_lock (fun () ->
      ServerPeriodical.init ();
      let%lwt env = program_init () in
      DfindLib.wait_until_ready dfind;
      Lwt.return env
    ) in

    (* Run both these threads *)
    Lwt.join [
      listening_thread;
      serve ~dfind ~genv ~env
    ]
  in
  Lwt_main.run initial_lwt_thread

let run_from_daemonize ~monitor_channels ~shared_mem_config options =
  try run ~monitor_channels ~shared_mem_config options
  with
  | SharedMem_js.Out_of_shared_memory ->
      FlowExitStatus.(exit Out_of_shared_memory)
  | e ->
      let bt = Printexc.get_backtrace () in
      let msg = Utils.spf "Unhandled exception: %s%s"
        (Printexc.to_string e)
        (if bt = "" then bt else "\n"^bt)
      in
      FlowExitStatus.(exit ~msg Unknown_error)

let check_once ~shared_mem_config ~client_include_warnings ?focus_targets options =
  PidLog.disable ();
  MonitorRPC.disable ();

  let initial_lwt_thread =
    let _, program_init =
      create_program_init ~shared_mem_config ~focus_targets options in
    let%lwt profiling, env = program_init () in

    let errors, warnings, suppressed_errors = ErrorCollator.get env in
    let warnings = if client_include_warnings || Options.should_include_warnings options
      then warnings
      else Errors.ErrorSet.empty
    in
    Lwt.return (profiling, errors, warnings, suppressed_errors)
  in
  Lwt_main.run initial_lwt_thread

let daemonize =
  let entry = Server_daemon.register_entry_point run_from_daemonize in
  fun ~log_file ~shared_mem_config ~argv options ->
    Server_daemon.daemonize ~log_file ~shared_mem_config ~argv ~options entry
