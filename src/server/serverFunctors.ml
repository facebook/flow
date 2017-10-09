(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open ServerEnv
open ServerUtils
open Utils_js
module List = Core_list
module Server_files = Server_files_js

exception State_not_found

module type SERVER_PROGRAM = sig
  val init : focus_targets:FilenameSet.t option -> genv -> (Profiling_js.finished * env)
  val check_once : genv -> env ->
    Errors.ErrorSet.t * (* errors *)
    Errors.ErrorSet.t * (* warnings *)
    (Errors.error * Loc.LocSet.t) list (* suppressed errors *)
  (* filter and relativize updated file paths *)
  val process_updates : genv -> env -> SSet.t -> FilenameSet.t
  val recheck: genv -> env ->
    ?force_focus:bool -> serve_ready_clients:(unit -> unit) -> FilenameSet.t -> env
  val get_watch_paths: Options.t -> Path.t list
  val name: string
  val handle_client : genv -> env ->
    serve_ready_clients:(unit -> unit) ->
    waiting_requests:(ServerEnv.env -> ServerEnv.env) list ref ->
    client -> env
  val handle_persistent_client : genv -> env ->
    serve_ready_clients:(unit -> unit) ->
    Persistent_connection.single_client -> env
end

(*****************************************************************************)
(* The main loop *)
(*****************************************************************************)
module ServerMain (Program : SERVER_PROGRAM) : sig
  val run :
    shared_mem_config:SharedMem_js.config ->
    log_file:string ->
    Options.t ->
    unit
  val check_once :
    shared_mem_config:SharedMem_js.config ->
    client_include_warnings:bool ->
    ?focus_targets:FilenameSet.t ->
    Options.t ->
    Profiling_js.finished *
      Errors.ErrorSet.t * (* errors *)
      Errors.ErrorSet.t * (* warnings *)
      (Errors.error * Loc.LocSet.t) list (* suppressed errors *)
  val daemonize :
    wait:bool ->
    log_file:string ->
    shared_mem_config:SharedMem_js.config ->
    ?on_spawn:(int -> unit) ->
    Options.t -> unit
end = struct
  type ready_socket =
    | New_client of Unix.file_descr
    | Existing_client of Persistent_connection.single_client


  let grab_lock ~tmp_dir root =
    if not (Lock.grab (Server_files.lock_file ~tmp_dir root))
    then
      let msg = "Error: another server is already running?\n" in
      FlowExitStatus.(exit ~msg Lock_stolen)

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

  (* Quickly check whether there is an outstanding request (while
     rechecking). TODO: handle persistent connections. *)
  let quick_check socket =
    let ready_socket_l, _, _ = Unix.select [socket] [] [] (0.0) in
    List.map ready_socket_l (fun ready_socket ->
      if ready_socket = socket then
        New_client socket
      else failwith "Internal server error: select returned an unknown fd"
    )

  let handle_connection_ genv env ~serve_ready_clients ~waiting_requests socket =
    let cli, _ = Unix.accept socket in
    let ic = Unix.in_channel_of_descr cli in
    let oc = Unix.out_channel_of_descr cli in
    let close () = ServerUtils.shutdown_client (ic, oc) in
    try
      let client_build_id = input_line ic in
      if client_build_id <> ServerProt.build_revision then begin
        msg_to_channel oc Build_id_mismatch;
        FlowEventLogger.out_of_date ();
        Hh_logger.fatal "Status: Error";
        Hh_logger.fatal "flow server is out of date. Exiting.";
        FlowExitStatus.exit FlowExitStatus.Build_id_mismatch
      end else msg_to_channel oc Connection_ok;
      let client = { ic; oc; close } in
      Program.handle_client genv env ~serve_ready_clients ~waiting_requests client
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

  let handle_connection genv env ~serve_ready_clients ~waiting_requests socket =
    ServerPeriodical.stamp_connection ();
    try handle_connection_ genv env ~serve_ready_clients ~waiting_requests socket
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
  let rec recheck_loop ~dfind genv env ~serve_ready_clients =
    let raw_updates = DfindLib.get_changes dfind in
    if SSet.is_empty raw_updates then env else begin
      let updates = Program.process_updates genv env raw_updates in
      let env = Program.recheck genv env ~serve_ready_clients updates in
      recheck_loop ~dfind genv env ~serve_ready_clients
    end

  let assert_lock genv =
    let root = Options.root genv.options in
    let tmp_dir = Options.temp_dir genv.options in
    let lock_file = Server_files.lock_file ~tmp_dir root in
    if not (Lock.check lock_file) then begin
      Hh_logger.warn "Lost %s lock; reacquiring.\n" Program.name;
      FlowEventLogger.lock_lost lock_file;
      if not (Lock.grab lock_file)
      then
        Hh_logger.fatal "Failed to reacquire lock; terminating.\n";
        FlowEventLogger.lock_stolen lock_file;
        die()
    end

  (* NOTE: This function never calls serve_ready_clients. Instead, it stores it
     in a continuation, which is called later. *)
  let serve_client genv env ~serve_ready_clients ~waiting_requests = function
    | New_client fd ->
      env := handle_connection genv !env ~serve_ready_clients ~waiting_requests fd
    | Existing_client client ->
      env := Program.handle_persistent_client genv !env ~serve_ready_clients client

  (* Waiting connections must be processed in a loop, since processing a waiting
     connection can add more waiting connections. *)
  let rec process_waiting_requests env ~waiting_requests =
    let continuations = List.rev !waiting_requests in
    waiting_requests := [];
    List.iter continuations (fun continuation -> env := continuation !env);
    if !waiting_requests <> []
    then process_waiting_requests env ~waiting_requests

  let serve_queue ~genv ~env ~ready_sockets ~serve_ready_clients ~waiting_requests =
    while not (Queue.is_empty ready_sockets) do
      let socket = Queue.pop ready_sockets in
      serve_client genv env ~serve_ready_clients ~waiting_requests socket;
    done

  let serve ~dfind ~genv ~env socket =
    let env = ref env in
    let waiting_requests = ref [] in
    while true do
      assert_lock genv;
      (* we want to defer processing certain commands until recheck is done *)
      ServerPeriodical.call_before_sleeping();
      let ready_sockets = Queue.create () in
      let add_ready_socket socket = Queue.push socket ready_sockets in
      env := { !env with connections = Persistent_connection.filter_broken !env.connections };
      sleep_and_check socket !env.connections
      |> List.iter ~f:add_ready_socket;

      (* serve_ready_clients could in theory keep serving clients as they pop up until there are no
       * more clients to serve. However, we likely have a recheck waiting, so we don't really want
       * a while loop in here. So instead we'd like to just serve the clients who are ready when
       * serve_ready_clients is called.
       *
       * Though deduping clients is a little tricky. So we're going to fudge this one more time and
       * say that "serve_ready_clients" is going to serve all the clients who are ready when
       * serve_ready_clients is called and maybe a few more who are ready after the existing queue
       * is processed
       *)
      let rec serve_ready_clients () =
        (* Drain the queue *)
        serve_queue ~genv ~env ~ready_sockets ~serve_ready_clients ~waiting_requests;

        (* If serve_queue is not empty, quick check will add duplicate sockets to the queue *)
        assert (Queue.is_empty ready_sockets);

        (* Add any waiting clients *)
        quick_check socket
        |> List.iter ~f:add_ready_socket;

        (* Drain the queue again *)
        serve_queue ~genv ~env ~ready_sockets ~serve_ready_clients ~waiting_requests;
      in
      env := recheck_loop ~dfind genv !env ~serve_ready_clients;

      serve_queue ~genv ~env ~ready_sockets ~serve_ready_clients ~waiting_requests;

      (* Done processing ready sockets! Now, process waiting connections that
         were collected when processing ready sockets. *)
      process_waiting_requests env ~waiting_requests;
      EventLogger.flush ();
    done

  (* This code is only executed when the options --check is NOT present *)
  let with_init_lock ~options ?waiting_channel init_fun =
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in
    let init_lock = Server_files.init_file ~tmp_dir root in
    let t = Unix.gettimeofday () in
    Hh_logger.info "Initializing Server (This might take some time)";
    ignore (Lock.grab init_lock);
    Server_daemon.wakeup_client waiting_channel Server_daemon.Starting;
    let _profiling, env = init_fun () in
    ignore (Lock.release init_lock);
    Server_daemon.wakeup_client waiting_channel Server_daemon.Ready;
    Hh_logger.info "Server is READY";
    let t' = Unix.gettimeofday () in
    Hh_logger.info "Took %f seconds to initialize." (t' -. t);
    Server_daemon.close_waiting_channel waiting_channel;
    env

  let init_dfind options =
    let tmp_dir = Options.temp_dir options in
    let root = Options.root options in
    let in_fd = Daemon.null_fd () in
    let log_file = Server_files_js.dfind_log_file ~tmp_dir root in
    let log_fd = Daemon.fd_of_path log_file in
    let fds = (in_fd, log_fd, log_fd) in
    let watch_paths = Program.get_watch_paths options in
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
    let program_init = fun () ->
      let profiling, env = Program.init ~focus_targets genv in
      FlowEventLogger.init_done ~profiling;
      profiling, env
    in
    genv, program_init

  let run_internal ?waiting_channel ~shared_mem_config options =
    let root = Options.root options in
    let tmp_dir = Options.temp_dir options in

    (* You need to grab the lock before initializing the pid files
       and before allocating the shared heap. *)
    grab_lock ~tmp_dir root;
    PidLog.init (Server_files.pids_file ~tmp_dir root);
    PidLog.log ~reason:"main" (Unix.getpid());

    let genv, program_init = create_program_init ~shared_mem_config ~focus_targets:None options in

    (* Open up a server on the socket before we go into program_init -- the
       client will try to connect to the socket as soon as we lock the init
       lock. We need to have the socket open now (even if we won't actually
       accept connections until init is done) so that the client can try to
       use the socket and get blocked on it -- otherwise, trying to open a
       socket with no server on the other end is an immediate error. *)
    let socket = Socket.init_unix_socket (
      let tmp_dir = Options.temp_dir options in
      Server_files.socket_file ~tmp_dir root
    ) in

    let dfind = init_dfind options in

    let env = with_init_lock ~options ?waiting_channel (fun () ->
      ServerPeriodical.init options;
      let env = program_init () in
      DfindLib.wait_until_ready dfind;
      env
    ) in

    serve ~dfind ~genv ~env socket

  let run ~shared_mem_config ~log_file options =
    let log_fd = Server_daemon.open_log_file log_file in
    Hh_logger.set_log (Unix.out_channel_of_descr log_fd);
    Hh_logger.info "Logs will go to %s" log_file;
    run_internal ~shared_mem_config options

  let run_from_daemonize ?waiting_channel ~shared_mem_config options =
    run_internal ?waiting_channel ~shared_mem_config options

  let check_once ~shared_mem_config ~client_include_warnings ?focus_targets options =
    PidLog.disable ();
    let genv, program_init = create_program_init ~shared_mem_config ~focus_targets options in
    let profiling, env = program_init () in
    let errors, warnings, suppressed_errors = Program.check_once genv env in
    let warnings = if client_include_warnings || Options.should_include_warnings options
      then warnings
      else Errors.ErrorSet.empty
    in
    profiling, errors, warnings, suppressed_errors

  let daemonize =
    let entry = Server_daemon.register_entry_point run_from_daemonize in
    fun ~wait ~log_file ~shared_mem_config ?on_spawn options ->
      Server_daemon.daemonize ~wait ~log_file ~shared_mem_config ?on_spawn ~options entry
end
