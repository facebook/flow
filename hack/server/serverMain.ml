(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Sys_utils
open ServerEnv
open ServerUtils
open Utils

exception State_not_found
exception Load_state_disabled

let sprintf = Printf.sprintf

type recheck_loop_stats = {
  rechecked_batches : int;
  rechecked_count : int;
  (* includes dependencies *)
  total_rechecked_count : int;
}

let empty_recheck_loop_stats = {
  rechecked_batches = 0;
  rechecked_count = 0;
  total_rechecked_count = 0;
}

(*****************************************************************************)
(* Main initialization *)
(*****************************************************************************)

module MainInit : sig
  val go:
    ServerArgs.options ->
    (unit -> env) ->    (* init function to run while we have init lock *)
    env
end = struct
  let grab_init_lock root =
    ignore(Lock.grab (ServerFiles.init_file root))

  let release_init_lock root =
    ignore(Lock.release (ServerFiles.init_file root))

  let wakeup_client oc msg =
    Option.iter oc begin fun oc ->
      try
        output_string oc (msg ^ "\n");
        flush oc
      with
      (* The client went away *)
      | Sys_error ("Broken pipe") -> ()
      | e ->
          Printf.eprintf "wakeup_client: %s\n%!" (Printexc.to_string e)
    end

  let close_waiting_channel oc =
    Option.iter oc begin fun oc ->
      try close_out oc
      with
      (* The client went away *)
      | Sys_error ("Broken pipe") -> ()
      | e ->
          Printf.eprintf "close_waiting_channel: %s\n%!" (Printexc.to_string e)
    end

  (* This code is only executed when the options --check is NOT present *)
  let go options init_fun =
    let waiting_channel =
      Option.map
        (ServerArgs.waiting_client options)
        ~f:Unix.out_channel_of_descr in
    let root = ServerArgs.root options in
    let t = Unix.gettimeofday () in
    Hh_logger.log "Initializing Server (This might take some time)";
    grab_init_lock root;
    wakeup_client waiting_channel "starting";
    (* note: we only run periodical tasks on the root, not extras *)
    ServerIdle.init root;
    let init_id = Random_id.short_string () in
    Hh_logger.log "Init id: %s" init_id;
    let env = HackEventLogger.with_id ~stage:`Init init_id init_fun in
    release_init_lock root;
    Hh_logger.log "Server is READY";
    wakeup_client waiting_channel "ready";
    let t' = Unix.gettimeofday () in
    Hh_logger.log "Took %f seconds to initialize." (t' -. t);
    close_waiting_channel waiting_channel;
    env
end

module Program =
  struct
    let preinit () =
      (* Warning: Global references inited in this function, should
         be 'restored' in the workers, because they are not 'forked'
         anymore. See `ServerWorker.{save/restore}_state`. *)
      HackSearchService.attach_hooks ();
      (* Force hhi files to be extracted and their location saved before workers
       * fork, so everyone can know about the same hhi path. *)
      ignore (Hhi.get_hhi_root());
      if not Sys.win32 then
        ignore @@
        Sys.signal Sys.sigusr1 (Sys.Signal_handle Typing.debug_print_last_pos)

    let run_once_and_exit genv env =
      ServerError.print_errorl
        (ServerArgs.json_mode genv.options)
        (List.map env.errorl Errors.to_absolute) stdout;
      match ServerArgs.convert genv.options with
      | None ->
         Worker.killall ();
         exit (if env.errorl = [] then 0 else 1)
      | Some dirname ->
         ServerConvert.go genv env dirname;
         Worker.killall ();
         exit 0

    (* filter and relativize updated file paths *)
    let process_updates genv _env updates =
      let root = Path.to_string @@ ServerArgs.root genv.options in
      (* Because of symlinks, we can have updates from files that aren't in
       * the .hhconfig directory *)
      let updates = SSet.filter (fun p -> str_starts_with p root) updates in
      Relative_path.(relativize_set Root updates)

    let recheck genv old_env typecheck_updates =
      if Relative_path.Set.is_empty typecheck_updates then
        old_env, 0
      else begin
        let failed_parsing =
          Relative_path.Set.union typecheck_updates old_env.failed_parsing in
        let check_env = { old_env with failed_parsing = failed_parsing } in
        let new_env, total_rechecked = ServerTypeCheck.check genv check_env in
        ServerStamp.touch_stamp_errors old_env.errorl new_env.errorl;
        new_env, total_rechecked
      end

    (* This is a hack for us to save / restore the global state that is not
     * already captured by ServerEnv *)
    let marshal chan =
      Typing_deps.marshal chan;
      HackSearchService.SS.MasterApi.marshal chan

    let unmarshal chan =
      Typing_deps.unmarshal chan;
      HackSearchService.SS.MasterApi.unmarshal chan

  end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)

let sleep_and_check socket =
  let ready_socket_l, _, _ = Unix.select [socket] [] [] (1.0) in
  ready_socket_l <> []

let handle_connection_ genv env socket =
  let cli, _ = Unix.accept socket in
  let ic = Unix.in_channel_of_descr cli in
  let oc = Unix.out_channel_of_descr cli in
  try
    let client_build_id = input_line ic in
    if client_build_id <> Build_id.build_id_ohai then
      (msg_to_channel oc Build_id_mismatch;
       HackEventLogger.out_of_date ();
       Printf.eprintf "Status: Error\n";
       Printf.eprintf "%s is out of date. Exiting.\n" GlobalConfig.program_name;
       Exit_status.exit Exit_status.Build_id_mismatch)
    else
      msg_to_channel oc Connection_ok;
    ServerCommand.handle genv env (ic, oc)
  with
  | Sys_error("Broken pipe") ->
    shutdown_client (ic, oc)
  | e ->
    let msg = Printexc.to_string e in
    EventLogger.master_exception msg;
    Printf.fprintf stderr "Error: %s\n%!" msg;
    Printexc.print_backtrace stderr;
    shutdown_client (ic, oc)

let handle_connection genv env socket =
  ServerIdle.stamp_connection ();
  try handle_connection_ genv env socket
  with
  | Unix.Unix_error (e, _, _) ->
     Printf.fprintf stderr "Unix error: %s\n" (Unix.error_message e);
     Printexc.print_backtrace stderr;
     flush stderr
  | e ->
     Printf.fprintf stderr "Error: %s\n" (Printexc.to_string e);
     Printexc.print_backtrace stderr;
     flush stderr

let recheck genv old_env updates =
  let to_recheck =
    Relative_path.Set.filter begin fun update ->
      ServerEnv.file_filter (Relative_path.suffix update)
    end updates in
  let config_in_updates =
    Relative_path.Set.mem ServerConfig.filename updates in
  if config_in_updates then begin
    let new_config = ServerConfig.(load filename) in
    if not (ServerConfig.is_compatible genv.config new_config) then begin
      Hh_logger.log
        "%s changed in an incompatible way; please restart %s.\n"
        (Relative_path.suffix ServerConfig.filename)
        GlobalConfig.program_name;
       exit 4
    end;
  end;
  let env, total_rechecked = Program.recheck genv old_env to_recheck in
  BuildMain.incremental_update genv old_env env updates;
  env, to_recheck, total_rechecked

(* When a rebase occurs, dfind takes a while to give us the full list of
 * updates, and it often comes in batches. To get an accurate measurement
 * of rebase time, we use the heuristic that any changes that come in
 * right after one rechecking round finishes to be part of the same
 * rebase, and we don't log the recheck_end event until the update list
 * is no longer getting populated. *)
let rec recheck_loop acc genv env =
  let t = Unix.gettimeofday () in
  let raw_updates = genv.notifier () in
  if SSet.is_empty raw_updates then
    acc, env
  else begin
    HackEventLogger.notifier_returned t (SSet.cardinal raw_updates);
    let updates = Program.process_updates genv env raw_updates in
    let env, rechecked, total_rechecked = recheck genv env updates in
    let acc = {
      rechecked_batches = acc.rechecked_batches + 1;
      rechecked_count =
        acc.rechecked_count + Relative_path.Set.cardinal rechecked;
      total_rechecked_count = acc.total_rechecked_count + total_rechecked;
    } in
    recheck_loop acc genv env
  end

let recheck_loop = recheck_loop {
  rechecked_batches = 0;
  rechecked_count = 0;
  total_rechecked_count = 0;
}

let serve genv env socket =
  let root = ServerArgs.root genv.options in
  let env = ref env in
  let last_stats = ref empty_recheck_loop_stats in
  let recheck_id = ref (Random_id.short_string ()) in
  while true do
    let lock_file = ServerFiles.lock_file root in
    if not (Lock.grab lock_file) then
      (Hh_logger.log "Lost lock; terminating.\n%!";
       HackEventLogger.lock_stolen lock_file;
       Exit_status.(exit Lock_stolen));
    let has_client = sleep_and_check socket in
    let has_parsing_hook = !ServerTypeCheck.hook_after_parsing <> None in
    if not has_client && not has_parsing_hook
    then begin
      (* Ugly hack: We want GC_SHAREDMEM_RAN to record the last rechecked
       * count so that we can figure out if the largest reclamations
       * correspond to massive rebases. However, the logging call is done in
       * the SharedMem module, which doesn't know anything about Server stuff.
       * So we wrap the call here. *)
      HackEventLogger.with_rechecked_stats
        !last_stats.rechecked_batches
        !last_stats.rechecked_count
        !last_stats.total_rechecked_count
        ServerIdle.go;
      recheck_id := Random_id.short_string ();
    end;
    let start_t = Unix.gettimeofday () in
    HackEventLogger.with_id ~stage:`Recheck !recheck_id @@ fun () ->
    let stats, new_env = recheck_loop genv !env in
    env := new_env;
    if stats.rechecked_count > 0 then begin
      HackEventLogger.recheck_end start_t has_parsing_hook
        stats.rechecked_batches
        stats.rechecked_count
        stats.total_rechecked_count;
      Hh_logger.log "Recheck id: %s" !recheck_id
    end;
    last_stats := stats;
    if has_client then handle_connection genv !env socket;
  done

let load genv filename to_recheck =
  let t = Unix.gettimeofday () in
  let chan = open_in filename in
  let env = Marshal.from_channel chan in
  Program.unmarshal chan;
  close_in chan;
  SharedMem.load (filename^".sharedmem");
  HackEventLogger.load_read_end t filename;
  let t = Hh_logger.log_duration "Loading saved state" t in

  let to_recheck =
    List.rev_append (BuildMain.get_all_targets ()) to_recheck in
  let paths_to_recheck =
    List.map to_recheck (Relative_path.concat Relative_path.Root) in
  let updates = Relative_path.set_of_list paths_to_recheck in
  let env, rechecked, total_rechecked = recheck genv env updates in
  let rechecked_count = Relative_path.Set.cardinal rechecked in
  HackEventLogger.load_recheck_end t rechecked_count total_rechecked;
  let _t = Hh_logger.log_duration "Post-load rechecking" t in
  env

let run_load_script genv cmd =
  try
    let t = Unix.gettimeofday () in
    let str_cmd =
      sprintf
        "%s %s %s"
        (Filename.quote (Path.to_string cmd))
        (Filename.quote (Path.to_string (ServerArgs.root genv.options)))
        (Filename.quote Build_id.build_id_ohai) in
    Hh_logger.log "Running load script: %s\n%!" str_cmd;
    let state_fn, to_recheck =
      let do_fn () =
        let ic = Unix.open_process_in str_cmd in
        let state_fn =
          try input_line ic
          with End_of_file -> raise State_not_found
        in
        if state_fn = "DISABLED" then raise Load_state_disabled;
        let to_recheck = ref [] in
        begin
          try
            while true do
              to_recheck := input_line ic :: !to_recheck
            done
          with End_of_file -> ()
        end;
        assert (Unix.close_process_in ic = Unix.WEXITED 0);
        state_fn, !to_recheck
      in
      with_timeout
        (ServerConfig.load_script_timeout genv.config)
        ~on_timeout:(fun _ -> failwith "Load script timed out")
        ~do_:do_fn
    in
    Hh_logger.log
      "Load state found at %s. %d files to recheck\n%!"
      state_fn (List.length to_recheck);
    HackEventLogger.load_script_done t;
    let init_type = "load" in
    let env = HackEventLogger.with_init_type init_type begin fun () ->
      load genv state_fn to_recheck
    end in
    env, init_type
  with e -> begin
    let init_type = match e with
      | State_not_found ->
        Hh_logger.log "Load state not found!";
        Hh_logger.log "Starting from a fresh state instead...";
        "load_state_not_found"
      | Load_state_disabled ->
        Hh_logger.log "Load state disabled!";
        "load_state_disabled"
      | e ->
        let msg = Printexc.to_string e in
        Hh_logger.log "Load error: %s" msg;
        Printexc.print_backtrace stderr;
        Hh_logger.log "Starting from a fresh state instead...";
        HackEventLogger.load_failed msg;
        "load_error"
    in
    let env = HackEventLogger.with_init_type init_type begin fun () ->
      ServerInit.init genv
    end in
    env, init_type
  end

let program_init genv =
  let env, init_type =
    (* If we are saving, always start from a fresh state -- just in case
     * incremental mode introduces any errors. *)
    if genv.local_config.ServerLocalConfig.use_mini_state &&
      ServerArgs.save_filename genv.options = None then
      match ServerConfig.load_mini_script genv.config with
      | None ->
          let env = ServerInit.init genv in
          env, "fresh"
      | Some load_mini_script ->
          let env = ServerInit.init ~load_mini_script genv in
          env, "mini_load"
    else
      match ServerConfig.load_script genv.config with
      | None ->
          let env = ServerInit.init genv in
          env, "fresh"
      | Some load_script ->
          run_load_script genv load_script
  in
  HackEventLogger.init_end init_type;
  Hh_logger.log "Waiting for daemon(s) to be ready...";
  genv.wait_until_ready ();
  ServerStamp.touch_stamp ();
  HackEventLogger.init_really_end init_type;
  env

let save_complete env fn =
  let chan = open_out_bin_no_fail fn in
  Marshal.to_channel chan env [];
  Program.marshal chan;
  close_out_no_fail fn chan;
  (* We cannot save the shared memory to `chan` because the OCaml runtime
   * does not expose the underlying file descriptor to C code; so we use
   * a separate ".sharedmem" file. *)
  SharedMem.save (fn^".sharedmem")

let save _genv env (kind, fn) =
  match kind with
  | ServerArgs.Complete -> save_complete env fn
  | ServerArgs.Mini -> ServerInit.save_state env fn

(* The main entry point of the daemon
 * the only trick to understand here, is that env.modified is the set
 * of files that changed, it is only set back to SSet.empty when the
 * type-checker succeeded. So to know if there is some work to be done,
 * we look if env.modified changed.
 *)
let daemon_main_exn options =
  let root = ServerArgs.root options in
  (* The OCaml default is 500, but we care about minimizing the memory
   * overhead *)
  let gc_control = Gc.get () in
  Gc.set {gc_control with Gc.max_overhead = 200};
  Relative_path.set_path_prefix Relative_path.Root root;
  (* Make sure to lock the lockfile before doing *anything*, especially
   * opening the socket. *)
  if not (Lock.grab (ServerFiles.lock_file root)) then begin
    Hh_logger.log "Error: another server is already running?\n";
    Exit_status.(exit Server_already_exists);
  end;
  let config = ServerConfig.(load filename) in
  let {ServerLocalConfig.cpu_priority; io_priority; _} as local_config =
    ServerLocalConfig.load () in
  if Sys_utils.is_test_mode ()
  then EventLogger.init (Daemon.devnull ()) 0.0
  else HackEventLogger.init root (Unix.gettimeofday ());
  Option.iter
    (ServerArgs.waiting_client options)
    ~f:Unix.set_close_on_exec;
  Program.preinit ();
  Sys_utils.set_priorities ~cpu_priority ~io_priority;
  let handle =
    SharedMem.init
      (ServerConfig.sharedmem_config config)
      local_config.ServerLocalConfig.shm_dir in
  (* this is to transform SIGPIPE in an exception. A SIGPIPE can happen when
   * someone C-c the client.
   *)
  if not Sys.win32 then Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  PidLog.init (ServerFiles.pids_file root);
  PidLog.log ~reason:"main" (Unix.getpid());
  let genv = ServerEnvBuild.make_genv options config local_config handle in
  let is_check_mode = ServerArgs.check_mode genv.options in
  if is_check_mode then
    let env = program_init genv in
    Option.iter (ServerArgs.save_filename genv.options) (save genv env);
    Program.run_once_and_exit genv env
  else
    (* Open up a server on the socket before we go into MainInit -- the client
     * will try to connect to the socket as soon as we lock the init lock. We
     * need to have the socket open now (even if we won't actually accept
     * connections until init is done) so that the client can try to use the
     * socket and get blocked on it -- otherwise, trying to open a socket with
     * no server on the other end is an immediate error. *)
    let socket = Socket.init_unix_socket (ServerFiles.socket_file root) in
    let env = MainInit.go options (fun () -> program_init genv) in
    serve genv env socket

let daemon_main options =
  try daemon_main_exn options
  with SharedMem.Out_of_shared_memory ->
    Printf.eprintf "Error: failed to allocate in the shared heap.\n%!";
    Exit_status.(exit Out_of_shared_memory)

let main_entry =
  Daemon.register_entry_point
    "main"
    (fun options (_ic, _oc) -> daemon_main options)

let monitor_entry =
  Daemon.register_entry_point
    "monitor"
    (ServerMonitor.go main_entry)

let monitor_daemon options =
  let log_link = ServerFiles.log_link (ServerArgs.root options) in
  (try Sys.rename log_link (log_link ^ ".old") with _ -> ());
  let log_file = ServerFiles.make_link_of_timestamped log_link in
  let {Daemon.pid; _} = Daemon.spawn monitor_entry (options, log_file) in
  Printf.eprintf "Spawned %s (child pid=%d)\n" GlobalConfig.program_name pid;
  (* We are not using symlink on Windows (see Sys_utils.symlink),
     so we announce the `log_file` to the user. The `log_link`
     is only read by the client. *)
  Printf.eprintf "Logs will go to %s\n%!"
    (if Sys.win32 then log_file else log_link);
  ()

let start () =
  Daemon.check_entry_point (); (* this call might not return *)
  let options = ServerArgs.parse_options () in
  if ServerArgs.should_detach options
  then monitor_daemon options
  else daemon_main options
