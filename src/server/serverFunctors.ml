(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ServerEnv
open ServerUtils
open Utils_js
module List = Core_list
module Server_files = Server_files_js

exception State_not_found

module type SERVER_PROGRAM = sig
  val preinit : Options.t -> unit
  val init : genv -> (Profiling_js.t * env)
  val run_once_and_exit : env -> unit
  (* filter and relativize updated file paths *)
  val process_updates : genv -> env -> SSet.t -> FilenameSet.t
  val recheck: genv -> env -> FilenameSet.t -> env
  val get_watch_paths: Options.t -> Path.t list
  val name: string
  val handle_client : genv -> env -> client -> env
end

let grab_lock ~tmp_dir root =
  if not (Lock.grab (Server_files.lock_file ~tmp_dir root))
  then
    let msg = "Error: another server is already running?\n" in
    FlowExitStatus.(exit ~msg Lock_stolen)

(*****************************************************************************)
(* Main initialization *)
(*****************************************************************************)

type daemon_msg =
  | Starting
  | Ready

module MainInit : sig
  val go:
    Options.t ->
    (unit -> env) ->    (* init function to run while we have init lock *)
    daemon_msg Daemon.out_channel option ->
    env
end = struct

  let grab_init_lock ~tmp_dir root =
    ignore(Lock.grab (Server_files.init_file ~tmp_dir root))

  let release_init_lock ~tmp_dir root =
    ignore(Lock.release (Server_files.init_file ~tmp_dir root))

  let wakeup_client oc msg =
    Option.iter oc begin fun oc ->
      try
        Daemon.to_channel oc msg
      with
      (* The client went away *)
      | Sys_error msg
        when msg = "Broken pipe"  || msg = "Invalid argument" -> ()
      | e ->
        prerr_endlinef "wakeup_client: %s" (Printexc.to_string e)
    end

  let close_waiting_channel oc =
    Option.iter oc begin fun oc ->
      try
        Daemon.close_out oc
      with
      (* The client went away *)
      | Sys_error msg
        when msg = "Broken pipe"  || msg = "Invalid argument" -> ()
      | e ->
        prerr_endlinef "close_waiting_channel: %s" (Printexc.to_string e)
    end

  (* This code is only executed when the options --check is NOT present *)
  let go options init_fun waiting_channel =
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in
    let t = Unix.gettimeofday () in
    grab_lock ~tmp_dir root;
    Flow_logger.log "Initializing Server (This might take some time)";
    grab_init_lock ~tmp_dir root;
    wakeup_client waiting_channel Starting;
    ServerPeriodical.init options;
    let env = init_fun () in
    release_init_lock ~tmp_dir root;
    wakeup_client waiting_channel Ready;
    Flow_logger.log "Server is READY";
    let t' = Unix.gettimeofday () in
    Flow_logger.log "Took %f seconds to initialize." (t' -. t);
    close_waiting_channel waiting_channel;
    env
end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)
let new_entry_point =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    Printf.sprintf "main_%d" !cpt

module ServerMain (Program : SERVER_PROGRAM) : sig
  val start : Options.t -> unit
end = struct
  let sleep_and_check socket =
    let ready_socket_l, _, _ = Unix.select [socket] [] [] (1.0) in
    ready_socket_l <> []

  let handle_connection_ genv env socket =
    let cli, _ = Unix.accept socket in
    let ic = Unix.in_channel_of_descr cli in
    let oc = Unix.out_channel_of_descr cli in
    let close () = ServerUtils.shutdown_client (ic, oc) in
    try
      let client_build_id = input_line ic in
      if client_build_id <> ServerProt.build_revision then begin
        msg_to_channel oc Build_id_mismatch;
        FlowEventLogger.out_of_date ();
        Printf.eprintf "Status: Error\n";
        Printf.eprintf "%s is out of date. Exiting.\n" Program.name;
        FlowExitStatus.exit FlowExitStatus.Build_id_mismatch
      end else msg_to_channel oc Connection_ok;
      let client = { ic; oc; close } in
      Program.handle_client genv env client
    with
    | Sys_error msg when msg = "Broken pipe" ->
      shutdown_client (ic, oc);
      env
    | e ->
      let msg = Printexc.to_string e in
      EventLogger.master_exception msg;
      Printf.fprintf stderr "Error: %s\n%!" msg;
      Printexc.print_backtrace stderr;
      shutdown_client (ic, oc);
      env

  let handle_connection genv env socket =
    ServerPeriodical.stamp_connection ();
    try handle_connection_ genv env socket
    with
    | Unix.Unix_error (e, _, _) ->
        Printf.fprintf stderr "Unix error: %s\n" (Unix.error_message e);
        Printexc.print_backtrace stderr;
        flush stderr;
        env
    | e ->
        Printf.fprintf stderr "Error: %s\n" (Printexc.to_string e);
        Printexc.print_backtrace stderr;
        flush stderr;
        env

  (* When a rebase occurs, dfind takes a while to give us the full list of
   * updates, and it often comes in batches. To get an accurate measurement
   * of rebase time, we use the heuristic that any changes that come in
   * right after one rechecking round finishes to be part of the same
   * rebase, and we don't log the recheck_end event until the update list
   * is no longer getting populated. *)
  let rec recheck_loop genv env =
    let dfind = match genv.dfind with
    | Some dfind -> dfind
    | None -> failwith "recheck_loop called without dfind set"
    in
    let raw_updates = DfindLib.get_changes dfind in
    if SSet.is_empty raw_updates then env else begin
      let updates = Program.process_updates genv env raw_updates in
      let env = Program.recheck genv env updates in
      recheck_loop genv env
    end

  let serve genv env socket =
    let root = Options.root genv.options in
    let tmp_dir = Options.temp_dir genv.options in
    let env = ref env in
    while true do
      let lock_file = Server_files.lock_file ~tmp_dir root in
      if not (Lock.check lock_file) then begin
        Flow_logger.log "Lost %s lock; reacquiring.\n" Program.name;
        FlowEventLogger.lock_lost lock_file;
        if not (Lock.grab lock_file)
        then
          Flow_logger.log "Failed to reacquire lock; terminating.\n";
          FlowEventLogger.lock_stolen lock_file;
          die()
      end;
      ServerPeriodical.call_before_sleeping();
      let has_client = sleep_and_check socket in
      env := recheck_loop genv !env;
      if has_client
      then env := handle_connection genv !env socket;
      ServerEnv.invoke_async_queue ();
      EventLogger.flush ();
    done

  let create_program_init genv = fun () ->
    let profiling, env = Program.init genv in
    FlowEventLogger.init_done ~profiling;
    env

  let open_log_file options =
    (* When opening a new foo.log file, if foo.log already exists, we move it to
     * foo.log.old. On Linux/OSX this is easy, we just call rename. On Windows,
     * the rename can fail if foo.log is open or if foo.log.old already exists.
     * Not a huge problem, we just need to be more intentional *)
    let file = Path.to_string (Options.log_file options) in

    if Sys.file_exists file
    then begin
      let old_file = file ^ ".old" in

      (try
        if Sys.file_exists old_file
        then Sys.remove old_file;
        Sys.rename file old_file
      with e ->
        Utils.prerr_endlinef
          "Log rotate: failed to move '%s' to '%s'\n%s"
          file
          old_file
          (Printexc.to_string e)
      )
    end;
    Unix.openfile file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o666

  (* The main entry point of the daemon
  * the only trick to understand here, is that env.modified is the set
  * of files that changed, it is only set back to SSet.empty when the
  * type-checker succeeded. So to know if there is some work to be done,
  * we look if env.modified changed.
  *)
  let main ?waiting_channel options =
    (* We don't want to spew for flow check *)
    let is_check_mode = Options.is_check_mode options in
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in
    let shm_global_size = Options.shm_global_size options in
    let shm_heap_size = Options.shm_heap_size options in
    let shm_dirs = Options.shm_dirs options in
    let shm_min_avail = Options.shm_min_avail options in
    let shm_log_level = Options.shm_log_level options in
    let dep_table_pow = Options.shm_dep_table_pow options in
    let hash_table_pow = Options.shm_hash_table_pow options in
    (* You need to grab the lock before initializing the pid files
       and before to allocate the shared heap. *)
    begin if not is_check_mode
    then begin
      grab_lock ~tmp_dir root;
      PidLog.init (Server_files.pids_file ~tmp_dir root);
      PidLog.log ~reason:"main" (Unix.getpid())
    end else begin
      PidLog.disable ();
      Flow_logger.disable ()
    end;
    end;
    FlowEventLogger.init_server root;
    Program.preinit options;
    let handle = SharedMem_js.init { SharedMem_js.
      global_size = shm_global_size;
      heap_size = shm_heap_size;
      dep_table_pow;
      hash_table_pow;
      shm_dirs;
      shm_min_avail;
      log_level = shm_log_level;
    } in
    (* this is to transform SIGPIPE in an exception. A SIGPIPE can happen when
    * someone C-c the client.
    *)
    Sys_utils.set_signal Sys.sigpipe Sys.Signal_ignore;
    let watch_paths = root :: Program.get_watch_paths options in
    let genv =
      ServerEnvBuild.make_genv options watch_paths handle in
    let program_init = create_program_init genv in
    if is_check_mode then
      let env = program_init () in
      Program.run_once_and_exit env
    else
      (* Open up a server on the socket before we go into MainInit -- the client
      * will try to connect to the socket as soon as we lock the init lock. We
      * need to have the socket open now (even if we won't actually accept
      * connections until init is done) so that the client can try to use the
      * socket and get blocked on it -- otherwise, trying to open a socket with
      * no server on the other end is an immediate error. *)
      let socket = Socket.init_unix_socket (
        Server_files.socket_file ~tmp_dir root
      ) in
      let env = MainInit.go options program_init waiting_channel in
      let dfind = match genv.dfind with
      | Some dfind -> dfind
      | None -> failwith "dfind not set up in server mode"
      in
      DfindLib.wait_until_ready dfind;
      serve genv env socket

  (* The server can communicate with the process that forked it over a pipe.
   * The current scheme has it write a message when it starts up and has the
   * lock and then write another message when it has finished initializing.
   * It's up to the forking process whether it cares to wait for the
   * initialization to complete *)
  let rec wait_loop child_pid options ic =
    let msg = try
      Daemon.from_channel ic
      with End_of_file ->
      (* The pipe broke before we got the alls-clear from the server. What kind
       * of things could go wrong. Well we check the lock before forking the
       * server, but maybe by the time the server started someone else had
       * grabbed the lock, so it exited. I'm sure there's a million other
       * things that could have gone wrong *)
      let pid, status =
        match Unix.(waitpid [ WNOHANG; WUNTRACED; ] child_pid) with
        | 0, _ ->
            (* Sometimes the End_of_file races the child process actually
             * exiting. In case that's happening here, let's give the child 1
             * second more to die *)
            Unix.sleep 1;
            Unix.(waitpid [ WNOHANG; WUNTRACED; ] child_pid)
        | pid, status -> pid, status in
      let exit_code =  FlowExitStatus.Server_start_failed status in
      let msg, exit_code = if pid = 0
      (* The server is still alive...not sure what happened *)
      then
        "Error: Failed to start server for some unknown reason.", exit_code
      (* The server is dead. Shucks. *)
      else
        let reason, exit_code = match status with
        | Unix.WEXITED code ->
            if code = FlowExitStatus.(error_code Lock_stolen)
            then
              (* Sometimes when we actually go to start the server we find a
               * server already running (race condition). If so, we can just
               * forward that error code *)
              "There is already a server running.",
              FlowExitStatus.Lock_stolen
            else if code = FlowExitStatus.(error_code Out_of_shared_memory)
            then
              "The server is failed to allocate shared memory.",
              FlowExitStatus.Out_of_shared_memory
            else
              spf "exited prematurely with code %d." code, exit_code
        | Unix.WSIGNALED signal ->
            spf "The server was killed prematurely with signal %d." signal,
            exit_code
        | Unix.WSTOPPED signal ->
            spf "The server was stopped prematurely with signal %d." signal,
            exit_code
        in spf "Error: Failed to start server. %s" reason, exit_code
      in FlowExitStatus.(exit ~msg exit_code)
    in
    if Options.should_wait options && msg <> Ready
    then wait_loop child_pid options ic

  let main_entry =
    Daemon.register_entry_point
      (new_entry_point ())
      (fun (options, config, logging_context) (ic, waiting_channel) ->
        ignore(Sys_utils.setsid());
        Daemon.close_in ic;
        FlowConfig.restore config;
        FlowEventLogger.restore_context logging_context;
        FlowEventLogger.init_flow_command ~version:FlowConfig.version;
        main ~waiting_channel options)

  let daemonize options =
    (* Let's make sure this isn't all for naught before we fork *)
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in
    let lock = Server_files.lock_file ~tmp_dir root in
    if not (Lock.check lock)
    then begin
      let msg = spf
        "Error: There is already a server running for %s"
        (Path.to_string root) in
      FlowExitStatus.(exit ~msg Lock_stolen)
    end;

    let log_file = Path.to_string (Options.log_file options) in
    let log_fd = open_log_file options in
    let config_file = Server_files_js.config_file root in
    (* Daemon.spawn is creating a new process with log_fd as both the stdout
     * and stderr. We are NOT leaking stdout and stderr. But the Windows
     * implementation of OCaml does leak stdout and stderr. This means any process
     * that waits for `flow start`'s stdout and stderr to close might wait
     * forever.
     *
     * On Windows 10 (and 8 I think), you can just call `set_close_on_exec` on
     * stdout and stderr and that seems to solve things. However, that call
     * fails on Windows 7. After poking around for a few hours, I can't think
     * of a solution other than manually implementing Unix.create_process
     * correctly.
     *
     * So for now let's make Windows 7 not crash. It seems like `flow start` on
     * Windows 7 doesn't actually leak stdio, so a no op is acceptable
     *)
    if Sys.win32
    then Unix.(try
      set_close_on_exec stdout;
      set_close_on_exec stderr
    with Unix_error (EINVAL, _, _) -> ());
    let {Daemon.pid; channels = (waiting_channel_ic, waiting_channel_oc)} =
      Daemon.spawn
        (log_fd, log_fd)
        main_entry
        (options, (config_file, FlowConfig.get config_file), FlowEventLogger.get_context ()) in
    (* detach ourselves from the parent process *)
    Daemon.close_out waiting_channel_oc;
    (* let original parent exit *)
    let pretty_pid = Sys_utils.pid_of_handle pid in
    if Options.should_output_json options
    then begin
      let open Hh_json in
      let json = json_to_string (JSON_Object [
        "pid", JSON_String (string_of_int pretty_pid);
        "log_file", JSON_String log_file;
      ]) in
      print_string json
    end else if not (Options.is_quiet options) then begin
      Printf.eprintf
        "Spawned %s (pid=%d)\n" (Program.name) pretty_pid;
      Printf.eprintf
        "Logs will go to %s\n%!" log_file
    end;

    wait_loop pid options waiting_channel_ic

  let start options =
    if Options.should_detach options
    then daemonize options
    else main options
end
