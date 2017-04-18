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
module Persistent_connection_prot = ServerProt.Persistent_connection_prot

exception State_not_found

module type SERVER_PROGRAM = sig
  val preinit : Options.t -> unit
  val init : genv -> (Profiling_js.t * env)
  val run_once_and_exit : profiling:Profiling_js.t -> genv -> env -> unit
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

module MainInit : sig
  val go:
    Options.t ->
    (unit -> Profiling_js.t * env) -> (* init function to run while we have init lock *)
    Server_daemon.waiting_channel option ->
    env
end = struct

  let grab_init_lock ~tmp_dir root =
    ignore(Lock.grab (Server_files.init_file ~tmp_dir root))

  let release_init_lock ~tmp_dir root =
    ignore(Lock.release (Server_files.init_file ~tmp_dir root))

  (* This code is only executed when the options --check is NOT present *)
  let go options init_fun waiting_channel =
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in
    let t = Unix.gettimeofday () in
    grab_lock ~tmp_dir root;
    Hh_logger.info "Initializing Server (This might take some time)";
    grab_init_lock ~tmp_dir root;
    Server_daemon.wakeup_client waiting_channel Server_daemon.Starting;
    ServerPeriodical.init options;
    let _profiling, env = init_fun () in
    release_init_lock ~tmp_dir root;
    Server_daemon.wakeup_client waiting_channel Server_daemon.Ready;
    Hh_logger.info "Server is READY";
    let t' = Unix.gettimeofday () in
    Hh_logger.info "Took %f seconds to initialize." (t' -. t);
    Server_daemon.close_waiting_channel waiting_channel;
    env
end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)
module ServerMain (Program : SERVER_PROGRAM) : sig
  val run : Options.t -> unit
  val daemonize : Options.t -> unit
end = struct
  type ready_socket =
    | New_client of Unix.file_descr
    | Existing_client of Persistent_connection.single_client

  let sleep_and_check socket persistent_connections =
    let client_fds = Persistent_connection.client_fd_list persistent_connections in
    let ready_socket_l, _, _ = Unix.select (socket::client_fds) [] [] (1.0) in
    List.map ready_socket_l (fun ready_socket ->
      if ready_socket = socket then
        New_client socket
      else if List.mem client_fds ready_socket then
        let client = Persistent_connection.client_of_fd persistent_connections ready_socket in
        Existing_client client
      else
        failwith "Internal server error: select returned an unknown fd"
    )

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
      EventLogger.master_exception e;
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

  let respond_to_client env client =
    let msg, env =
      try
        Some (Persistent_connection.input_value client), env
      with
        | End_of_file ->
            print_endline "Lost connection to client";
            let new_connections = Persistent_connection.remove_client env.connections client in
            None, {env with connections = new_connections}
    in
    match msg with
      | Some Persistent_connection_prot.Subscribe ->
          let new_connections =
            Persistent_connection.subscribe_client env.connections client env.errorl
          in
          { env with connections = new_connections }
      | None -> env

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
      (* This will result in some false positives *)
      let did_change = not (FilenameSet.is_empty updates) in
      Persistent_connection.send_start_recheck env.connections;
      let env = Program.recheck genv env updates in
      if did_change then Persistent_connection.update_clients env.connections env.errorl;
      Persistent_connection.send_end_recheck env.connections;
      recheck_loop genv env
    end

  let serve genv env socket =
    let root = Options.root genv.options in
    let tmp_dir = Options.temp_dir genv.options in
    let env = ref env in
    while true do
      let lock_file = Server_files.lock_file ~tmp_dir root in
      if not (Lock.check lock_file) then begin
        Hh_logger.warn "Lost %s lock; reacquiring.\n" Program.name;
        FlowEventLogger.lock_lost lock_file;
        if not (Lock.grab lock_file)
        then
          Hh_logger.fatal "Failed to reacquire lock; terminating.\n";
          FlowEventLogger.lock_stolen lock_file;
          die()
      end;
      ServerPeriodical.call_before_sleeping();
      let ready_sockets = sleep_and_check socket !env.connections in
      env := recheck_loop genv !env;
      List.iter ready_sockets (function
        | New_client fd ->
            env := handle_connection genv !env fd;
        (* TODO handle other cases *)
        | Existing_client client ->
            env := respond_to_client !env client;
      );
      EventLogger.flush ();
    done

  let create_program_init genv () =
    let profiling, env = Program.init genv in
    FlowEventLogger.init_done ~profiling;
    profiling, env

  let shared_mem_config_of_options options =
    { SharedMem_js.
      global_size = Options.shm_global_size options;
      heap_size = Options.shm_heap_size options;
      dep_table_pow = Options.shm_dep_table_pow options;
      hash_table_pow = Options.shm_hash_table_pow options;
      shm_dirs = Options.shm_dirs options;
      shm_min_avail = Options.shm_min_avail options;
      log_level = Options.shm_log_level options;
    }


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
    if is_check_mode then begin
      PidLog.disable ();
    end else begin
      (* You need to grab the lock before initializing the pid files
         and before allocating the shared heap. *)
      grab_lock ~tmp_dir root;
      PidLog.init (Server_files.pids_file ~tmp_dir root);
      PidLog.log ~reason:"main" (Unix.getpid())
    end;
    FlowEventLogger.init_server root;
    let handle = SharedMem_js.init (shared_mem_config_of_options options) in
    (* this is to transform SIGPIPE into an exception. A SIGPIPE can happen when
     * someone C-c the client. *)
    Sys_utils.set_signal Sys.sigpipe Sys.Signal_ignore;
    let watch_paths = Program.get_watch_paths options in
    let genv =
      ServerEnvBuild.make_genv options watch_paths handle in
    let program_init = create_program_init genv in
    if is_check_mode then
      let profiling, env = program_init () in
      Program.run_once_and_exit ~profiling genv env
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

  let run options = main options

  let daemonize =
    let entry = Server_daemon.register_entry_point main in
    fun options -> Server_daemon.daemonize ~options entry
end
