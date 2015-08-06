(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Sys_utils
open ServerEnv
open ServerUtils
open Utils
module List = Core_list

exception State_not_found

module type SERVER_PROGRAM = sig
  val preinit : unit -> unit
  val init : genv -> env -> env
  val run_once_and_exit : genv -> env -> unit
  val should_recheck : Path.t -> bool
  (* filter and relativize updated file paths *)
  val process_updates : genv -> env -> SSet.t -> ServerEnv.PathSet.t
  val recheck: genv -> env -> ServerEnv.PathSet.t -> env
  val post_recheck_hook: genv -> env -> env -> ServerEnv.PathSet.t -> unit
  val parse_options: unit -> Options.options
  val get_watch_paths: Options.options -> Path.t list
  val name: string
  val config_path : Path.t -> Path.t
  val validate_config : genv -> bool
  val handle_client : genv -> env -> client -> unit
end

(*****************************************************************************)
(* Main initialization *)
(*****************************************************************************)

module MainInit : sig
  val go:
    Options.options ->
    (unit -> env) ->    (* init function to run while we have init lock *)
    env
end = struct

  let other_server_running() =
    Hh_logger.log "Error: another server is already running?\n";
    exit 1

  let grab_lock ~tmp_dir root =
    if not (Lock.grab (FlowConfig.lock_file ~tmp_dir root))
    then other_server_running()

  let grab_init_lock ~tmp_dir root =
    ignore(Lock.grab (FlowConfig.init_file ~tmp_dir root))

  let release_init_lock ~tmp_dir root =
    ignore(Lock.release (FlowConfig.init_file ~tmp_dir root))

  (* This code is only executed when the options --check is NOT present *)
  let go options init_fun =
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in
    let t = Unix.gettimeofday () in
    grab_lock ~tmp_dir root;
    Hh_logger.log "Initializing Server (This might take some time)";
    grab_init_lock ~tmp_dir root;
    (* note: we only run periodical tasks on the root, not extras *)
    ServerPeriodical.init root;
    let env = init_fun () in
    release_init_lock ~tmp_dir root;
    Hh_logger.log "Server is READY";
    let t' = Unix.gettimeofday () in
    Hh_logger.log "Took %f seconds to initialize." (t' -. t);
    env
end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)

module ServerMain (Program : SERVER_PROGRAM) : sig
  val start : unit -> unit
end = struct
  let sleep_and_check socket =
    let ready_socket_l, _, _ = Unix.select [socket] [] [] (1.0) in
    ready_socket_l <> []

  let handle_connection_ genv env socket =
    let cli, _ = Unix.accept socket in
    let ic = Unix.in_channel_of_descr cli in
    let oc = Unix.out_channel_of_descr cli in
    let close () =
      Unix.shutdown cli Unix.SHUTDOWN_ALL;
      Unix.close cli in
    try
      let client_build_id = input_line ic in
      if client_build_id <> Build_id.build_id_ohai then begin
        msg_to_channel oc Build_id_mismatch;
        FlowEventLogger.out_of_date ();
        Printf.eprintf "Status: Error\n";
        Printf.eprintf "%s is out of date. Exiting.\n" Program.name;
        exit 4
      end else msg_to_channel oc Connection_ok;
      let client = { ic; oc; close } in
      Program.handle_client genv env client
    with e ->
      let msg = Printexc.to_string e in
      EventLogger.master_exception msg;
      Printf.fprintf stderr "Error: %s\n%!" msg;
      Printexc.print_backtrace stderr;
      close ()

  let handle_connection genv env socket =
    ServerPeriodical.stamp_connection ();
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
      ServerEnv.PathSet.filter Program.should_recheck updates in
    let config = Program.config_path (Options.root genv.ServerEnv.options) in
    if ServerEnv.PathSet.mem config updates &&
      not (Program.validate_config genv) then begin
      Hh_logger.log
        "%s changed in an incompatible way; please restart %s.\n"
        (Path.to_string config)
        Program.name;
      exit 4;
    end;
    let env = Program.recheck genv old_env to_recheck in
    Program.post_recheck_hook genv old_env env updates;
    env, to_recheck

  (* When a rebase occurs, dfind takes a while to give us the full list of
   * updates, and it often comes in batches. To get an accurate measurement
   * of rebase time, we use the heuristic that any changes that come in
   * right after one rechecking round finishes to be part of the same
   * rebase, and we don't log the recheck_end event until the update list
   * is no longer getting populated. *)
  let rec recheck_loop i rechecked_count genv env =
    let raw_updates = DfindLib.get_changes (unsafe_opt genv.dfind) in
    if SSet.is_empty raw_updates then i, rechecked_count, env else begin
      let updates = Program.process_updates genv env raw_updates in
      let env, rechecked = recheck genv env updates in
      let rechecked_count = rechecked_count +
        (ServerEnv.PathSet.cardinal rechecked) in
      recheck_loop (i + 1) rechecked_count genv env
    end

  let recheck_loop = recheck_loop 0 0

  let serve genv env socket =
    let root = Options.root genv.options in
    let tmp_dir = Options.temp_dir genv.options in
    let env = ref env in
    while true do
      let lock_file = FlowConfig.lock_file ~tmp_dir root in
      if not (Lock.check lock_file) then begin
        Hh_logger.log "Lost %s lock; reacquiring.\n" Program.name;
        FlowEventLogger.lock_lost lock_file;
        if not (Lock.grab lock_file)
        then
          Hh_logger.log "Failed to reacquire lock; terminating.\n";
          FlowEventLogger.lock_stolen lock_file;
          die()
      end;
      ServerPeriodical.call_before_sleeping();
      let has_client = sleep_and_check socket in
      let loop_count, rechecked_count, new_env = recheck_loop genv !env in
      env := new_env;
      if has_client then handle_connection genv !env socket;
      ServerEnv.invoke_async_queue ();
      EventLogger.flush ();
    done

  let create_program_init genv env = fun () ->
    let env = Program.init genv env in
    FlowEventLogger.init_done ();
    env

  (* The main entry point of the daemon
  * the only trick to understand here, is that env.modified is the set
  * of files that changed, it is only set back to SSet.empty when the
  * type-checker succeeded. So to know if there is some work to be done,
  * we look if env.modified changed.
  *)
  let main options =
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in
    FlowEventLogger.init_server root;
    Program.preinit ();
    SharedMem.(init default_config);
    (* this is to transform SIGPIPE in an exception. A SIGPIPE can happen when
    * someone C-c the client.
    *)
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    PidLog.init (FlowConfig.pids_file ~tmp_dir root);
    PidLog.log ~reason:"main" (Unix.getpid());
    let watch_paths = root :: Program.get_watch_paths options in
    let genv =
      ServerEnvBuild.make_genv ~multicore:true options watch_paths in
    let env = ServerEnvBuild.make_env options in
    let program_init = create_program_init genv env in
    let is_check_mode = Options.is_check_mode options in
    if is_check_mode then
      let env = program_init () in
      Program.run_once_and_exit genv env
    else
      let env = MainInit.go options program_init in
      let socket = Socket.init_unix_socket (FlowConfig.socket_file ~tmp_dir root) in
      serve genv env socket

  let daemonize options =
    (* detach ourselves from the parent process *)
    let pid = Fork.fork() in
    if pid == 0
    then begin
      ignore(Unix.setsid());
      with_umask 0o111 begin fun () ->
        (* close stdin/stdout/stderr *)
        let fd = Unix.openfile "/dev/null" [Unix.O_RDONLY; Unix.O_CREAT] 0o777 in
        Unix.dup2 fd Unix.stdin;
        Unix.close fd;
        let file = Path.to_string (Options.log_file options) in
        (try Sys.rename file (file ^ ".old") with _ -> ());
        let fd = Unix.openfile file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o666 in
        Unix.dup2 fd Unix.stdout;
        Unix.dup2 fd Unix.stderr;
        Unix.close fd
      end
      (* child process is ready *)
    end else begin
      (* let original parent exit *)
      Printf.eprintf "Spawned %s (child pid=%d)\n" (Program.name) pid;
      Printf.eprintf
        "Logs will go to %s\n%!" (Path.to_string (Options.log_file options));
      raise Exit
    end

  let start () =
    let options = Program.parse_options () in
    Relative_path.set_path_prefix Relative_path.Root (Options.root options);
    try
      if Options.should_detach options
      then daemonize options;
      main options
    with Exit ->
      ()
end
