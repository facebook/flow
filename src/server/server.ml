(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv

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
    let%lwt parsed, unparsed, dependency_graph, ordered_libs, libs, libs_ok, errors =
      Types_js.init ~profiling ~workers options in

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
        Types_js.full_check ~profiling ~workers ~focus_targets ~options parsed dependency_graph errors
    in

    sample_init_memory profiling;

    SharedMem_js.init_done();

    (* Return an env that initializes invariants required and maintained by
       recheck, namely that `files` contains files that parsed successfully, and
       `errors` contains the current set of errors. *)
    let env = { ServerEnv.
      files = parsed;
      unparsed;
      dependency_graph;
      checked_files = checked;
      ordered_libs;
      libs;
      errors;
      collated_errors = ref None;
      connections = Persistent_connection.empty;
    } in
    Lwt.return env
  end in
  MonitorRPC.status_update ~event:ServerStatus.Finishing_up;
  Lwt.return env

module Monitor : sig
  type workload = env -> env Lwt.t
  val listen_for_messages: genv -> unit Lwt.t
  val wait_for_anything: genv -> env -> unit Lwt.t
  val get_next_workload: unit -> workload option
  val update_env: env -> env
  val get_updates_for_recheck: genv -> env -> Utils_js.FilenameSet.t
end = struct
  type workload = env -> env Lwt.t

  (* Workloads are client requests which we processes FIFO *)
  let workload_stream, push_new_workload = Lwt_stream.create ()
  (* Env updates are...well...updates to our env. They must be handled in the main thread. Also FIFO
   * but are quick to handle *)
  let env_update_stream, push_new_env_update = Lwt_stream.create ()
  (* Files which have changed *)
  let recheck_stream, push_files_to_recheck = Lwt_stream.create ()

  (* This is a thread that just keeps looping in the background. It reads messages from the
   * monitor process and adds them to a stream *)
  module ListenLoop = LwtLoop.Make (struct
    type acc = genv

    let handle_message genv = function
    | MonitorProt.Request (request_id, command) ->
      push_new_workload (Some (fun env -> CommandHandler.handle_ephemeral genv env (request_id, command)))
    | MonitorProt.PersistentConnectionRequest (client_id, request) ->
      push_new_workload (Some (fun env -> CommandHandler.handle_persistent genv env client_id request))
    | MonitorProt.NewPersistentConnection (client_id, logging_context, lsp) ->
      push_new_env_update (Some (fun env -> { env with
        connections = Persistent_connection.add_client env.connections client_id logging_context lsp
      }))
    | MonitorProt.DeadPersistentConnection client_id ->
      push_new_env_update (Some (fun env -> { env with
        connections = Persistent_connection.remove_client env.connections client_id
      }))
    | MonitorProt.FileWatcherNotification changed_files ->
      push_files_to_recheck (Some changed_files)
    | MonitorProt.PleaseDie please_die_reason ->
      (* TODO - find a way to gracefully kill the workers. At the moment, if the workers are in the
       * middle of a job this will lead to some log spew. We probably should send SIGTERM to each
       * worker and set up a signal handler to kill the fork and exit gracefully. Might also want
       * to use the SharedMem.cancel thingy *)
      Hh_logger.info "Killing the worker processes";
      WorkerController.killall ();
      let msg = match please_die_reason with
      | MonitorProt.MonitorExiting (monitor_exit_status, monitor_msg) ->
        Utils.spf
          "Monitor is exiting with status %s (%s)"
          (FlowExitStatus.to_string monitor_exit_status)
          monitor_msg
      in
      FlowExitStatus.(exit ~msg Killed_by_monitor)

    let main genv =
      let%lwt message = MonitorRPC.read () in
      handle_message genv message;
      Lwt.return genv

    external reraise : exn -> 'a = "%reraise"

    let catch _ exn = reraise exn
  end)

  let listen_for_messages genv = ListenLoop.run genv

  let get_next_workload () =
    match Lwt_stream.get_available_up_to 1 workload_stream with
    | [ workload ] -> Some workload
    | [] -> None
    | _ -> failwith "Unreachable"

  let update_env env =
    Lwt_stream.get_available env_update_stream
    |> List.fold_left (fun env f -> f env) env

  let recheck_acc = ref Utils_js.FilenameSet.empty
  let recheck_fetch genv env =
    recheck_acc :=
      Lwt_stream.get_available recheck_stream (* Get all the files which have changed *)
      |> List.fold_left SSet.union SSet.empty (* Flatten the set *)
      |> Rechecker.process_updates genv env (* Process the changes *)
      |> Utils_js.FilenameSet.union (!recheck_acc) (* Union them with the acc *)
  let get_updates_for_recheck genv env =
    recheck_fetch genv env;
    let files = !recheck_acc in
    recheck_acc := Utils_js.FilenameSet.empty;
    files
  let rec wait_for_updates_for_recheck genv env =
    let%lwt _ = Lwt_stream.is_empty recheck_stream in
    recheck_fetch genv env;
    if Utils_js.FilenameSet.is_empty !recheck_acc
    then wait_for_updates_for_recheck genv env
    else Lwt.return_unit

  (* Block until any stream receives something *)
  let wait_for_anything genv env =
    let%lwt () = Lwt.pick [
      (let%lwt _ = Lwt_stream.is_empty workload_stream in Lwt.return_unit);
      (let%lwt _ = Lwt_stream.is_empty env_update_stream in Lwt.return_unit);
      (let%lwt _ = Lwt_stream.is_empty recheck_stream in Lwt.return_unit);
      wait_for_updates_for_recheck genv env;
    ] in
    Lwt.return_unit
end

let rec recheck_and_update_env_loop genv env =
  let env = Monitor.update_env env in
  let updates = Monitor.get_updates_for_recheck genv env in
  if Utils_js.FilenameSet.is_empty updates
  then Lwt.return env
  else begin
    let%lwt _profiling, env = Rechecker.recheck genv env updates in
    recheck_and_update_env_loop genv env
  end

let rec serve ~genv ~env =
  Hh_logger.debug "Starting aggressive shared mem GC";
  SharedMem_js.collect `aggressive;
  Hh_logger.debug "Finished aggressive shared mem GC";

  MonitorRPC.status_update ~event:ServerStatus.Ready;

  (* Ok, server is settled. Let's go to sleep until we get a message from the monitor *)
  let%lwt () = Monitor.wait_for_anything genv env in

  (* If there's anything to recheck or updates to the env from the monitor, let's consume them *)
  let%lwt env = recheck_and_update_env_loop genv env in

  (* Run a workload (if there is one) *)
  let%lwt env = Option.value_map (Monitor.get_next_workload ())
    ~default:(Lwt.return env)
    ~f:(fun workload -> workload env) in

  (* Flush the logs asynchronously *)
  Lwt.async EventLoggerLwt.flush;

  serve ~genv ~env

(* The main entry point of the daemon
* the only trick to understand here, is that env.modified is the set
* of files that changed, it is only set back to SSet.empty when the
* type-checker succeeded. So to know if there is some work to be done,
* we look if env.modified changed.
*)
let create_program_init ~shared_mem_config ~focus_targets options =
  let handle = SharedMem_js.init shared_mem_config in
  let genv = ServerEnvBuild.make_genv options handle in

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

  let initial_lwt_thread () =
    (* Read messages from the server monitor and add them to a stream as they come in *)
    let listening_thread = Monitor.listen_for_messages genv in

    (* Initialize *)
    let%lwt env =
      let t = Unix.gettimeofday () in
      Hh_logger.info "Initializing Server (This might take some time)";
      let%lwt _profiling, env = program_init () in
      Hh_logger.info "Server is READY";
      let t' = Unix.gettimeofday () in
      Hh_logger.info "Took %f seconds to initialize." (t' -. t);
      Lwt.return env
    in

    (* Run both these threads. If either of them fail, return immediately *)
    LwtUtils.iter_all [
      listening_thread;
      serve ~genv ~env
    ]
  in
  LwtInit.run_lwt initial_lwt_thread

let run_from_daemonize ~monitor_channels ~shared_mem_config options =
  try run ~monitor_channels ~shared_mem_config options
  with
  | SharedMem_js.Out_of_shared_memory ->
      let bt = Printexc.get_backtrace () in
      let msg = Utils.spf "Out of shared memory%s" (if bt = "" then bt else ":\n"^bt) in
      FlowExitStatus.(exit ~msg Out_of_shared_memory)
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

  let initial_lwt_thread () =
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
  LwtInit.run_lwt initial_lwt_thread

let daemonize =
  let entry = Server_daemon.register_entry_point run_from_daemonize in
  fun ~log_file ~shared_mem_config ~argv options ->
    Server_daemon.daemonize ~log_file ~shared_mem_config ~argv ~options entry
