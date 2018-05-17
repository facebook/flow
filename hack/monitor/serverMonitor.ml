(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
*)

(**
 * The server monitor is the parent process for a server. It
 * listens to a socket for client connections and passes the connections
 * to the server and serves the following objectives:
 *
   * 1) Readily accepts client connections
   * 2) Confirms a Build ID match (killing itself and the server quickly
   *    on mismatch)
   * 3) Hands the client connection to the daemon server
   * 4) Tracks when the server process crashes or OOMs and echos
   *    its fate to the next client.
*)

open Hh_core
open ServerProcess
open ServerMonitorUtils


module Sent_fds_collector = struct

  (**
   This module exists to fix an issue with libancillary (passing a file descriptor
   to another process with sendmsg over Unix Domain Sockets) and certain operating
   systems. It allows us to delay closing of a File Descriptor inside the Monitor
   until it is safe to do so.

   Normally:
     Monitor sends client FD to Server process, and immediately closes the FD.
     This is fine even if the Server is busy and hasn't "recv_fd" the FD yet
     because this doesn't really "close" the file. The kernel still considers
     it to be open by the receiving process. If the server closes the FD
     then reads on the client will get an EOF. If the client closes the FD
     then reads on the server will get an EOF.

   Mac OS X:
     EOF isn't showing up correctly on file descriptors passed between
     processes.
     When the Monitor closes the FD after sending it to the Server (and
     before the Server receives it), the kernel thinks it is the last open
     descriptor on the file and actually closes it. After the server
     receieves the FD, it gets an EOF when reading from it (which it shouldn't
     because the client is still there; aside: oddly enough, writing to it
     succeeds instead of getting EPIPE). The server then closes that FD after
     reading the EOF. Normally (as noted above) the client would read an
     EOF after this. But (this is the bug) this EOF never shows up and the
     client blocks forever on "select" instead.

   To get around this problem, we want to close the FD in the monitor only
   after the server has received it. Unfortunately, we don't actually
   have a way to reliably detect that it has been received. So we just delay
   closing by 2 seconds.

   Note: It's not safe to detect the receiving by reading the
   Hello message from the server (since it could/would be consumed
   here instead of by the Client) nor by "select" (by a race condition
   with the client, the select might miss the Hello, and could prevent
   an EOF from being read by the server).
   *)

  module Fd_scheduler = Scheduler.Make(struct
    type t = (** Unix.time *) float
  end)

  let cleanup_fd fd =
    if Sys_utils.is_apple_os () then
      (** Close it 2 seconds later. *)
      let trigger = Unix.gettimeofday () +. 2.0 in
      Fd_scheduler.wait_for_fun
        ~once:true
        ~priority:1
        (fun time -> time >= trigger)
        (fun x ->
          let () = Printf.eprintf "Closing client fd\n" in
          let () = Unix.close fd in x)
    else
      Unix.close fd

  let collect_garbage () =
    if Sys_utils.is_apple_os () then
      ignore (Fd_scheduler.wait_and_run_ready (Unix.gettimeofday ()))
    else
      ()
end;;


exception Malformed_build_id
exception Send_fd_failure of int

module Make_monitor (SC : ServerMonitorUtils.Server_config)
(Informant : Informant_sig.S) = struct

  type env = {
    informant: Informant.t;
    server: ServerProcess.server_process;
    server_start_options: SC.server_start_options;
    (** How many times have we tried to relaunch it? *)
    retries: int;
    max_purgatory_clients: int;
    (** After sending a Server_not_alive_dormant during Prehandoff,
     * clients are put here waiting for a server to come alive, at
     * which point they get pushed through the rest of prehandoff and
     * then sent to the living server.
     *
     * String is the server name it wants to connect to. *)
    purgatory_clients : (MonitorRpc.handoff_options * Unix.file_descr) Queue.t;
    (** Whether to ignore hh version mismatches *)
    ignore_hh_version : bool;
  }

  type t = env * ServerMonitorUtils.monitor_config * Unix.file_descr

  let fd_to_int (x: Unix.file_descr) : int = Obj.magic x

  let msg_to_channel fd msg =
    (* This FD will be passed to a server process, so avoid using Ocaml's
     * channels which have built-in buffering. Even though we are only writing
     * to the FD here, it seems using Ocaml's channels also causes read
     * buffering to happen here, so the server process doesn't get what was
     * meant for it. *)
    Marshal_tools.to_fd_with_preamble fd msg
    |> ignore

  let setup_handler_for_signals handler signals =
    List.iter signals begin fun signal ->
      Sys_utils.set_signal signal (Sys.Signal_handle handler)
    end

  let setup_autokill_server_on_exit process =
    try
      setup_handler_for_signals begin fun _ ->
        Hh_logger.log "Got an exit signal. Killing server and exiting.";
        SC.kill_server process;
        Exit_status.exit Exit_status.Interrupted
      end [Sys.sigint; Sys.sigquit; Sys.sigterm; Sys.sighup];
    with
    | _ ->
      Hh_logger.log "Failed to set signal handler"

  let sleep_and_check socket =
    let ready_socket_l, _, _ = Unix.select [socket] [] [] (1.0) in
    ready_socket_l <> []

  (** Kill command from client is handled by server server, so the monitor
   * needs to check liveness of the server process to know whether
   * to stop itself. *)
  let update_status_ (env: env) monitor_config =
    let env = match env.server with
      | Alive process ->
        let pid, proc_stat = SC.wait_pid process in
        (match pid, proc_stat with
          | 0, _ ->
            (* "pid=0" means the pid we waited for (i.e. process) hasn't yet died/stopped *)
            env
          | _, _ ->
            (* "pid<>0" means the pid has died or received a stop signal *)
            let oom_code = Exit_status.(exit_code Out_of_shared_memory) in
            let was_oom = match proc_stat with
            | Unix.WEXITED code when code = oom_code -> true
            | _ -> Sys_utils.check_dmesg_for_oom process.pid "hh_server" in
            SC.on_server_exit monitor_config;
            ServerProcessTools.check_exit_status proc_stat process monitor_config;
            { env with server = Died_unexpectedly (proc_stat, was_oom) })
      | _ -> env
    in
    let exit_status, server_state = match env.server with
      | Alive _ ->
        None, Informant_sig.Server_alive
      | Died_unexpectedly ((Unix.WEXITED c), _) ->
        Some c, Informant_sig.Server_dead
      | Not_yet_started ->
        None, Informant_sig.Server_not_yet_started
      | Died_unexpectedly ((Unix.WSIGNALED _| Unix.WSTOPPED _), _)
      | Informant_killed ->
        None, Informant_sig.Server_dead in
    env, exit_status, server_state

  let start_server ?target_mini_state ~informant_managed options exit_status =
    let server_process = SC.start_server
      ?target_mini_state
      ~prior_exit_status:exit_status
      ~informant_managed options in
    setup_autokill_server_on_exit server_process;
    Alive server_process

  let maybe_start_first_server options informant =
    if Informant.should_start_first_server informant then begin
      Hh_logger.log "Starting first server";
      HackEventLogger.starting_first_server ();
      start_server ~informant_managed:(Informant.is_managing informant)
        options None
    end
    else begin
      Hh_logger.log ("Not starting first server. " ^^
        "Starting will be triggered by informant later.");
      Not_yet_started
    end

  let kill_server_with_check = function
      | Alive server ->
        SC.kill_server server
      | _ -> ()

  let wait_for_server_exit_with_check server kill_signal_time =
    match server with
      | Alive server ->
        SC.wait_for_server_exit server kill_signal_time
      | _ -> ()

  let kill_server_and_wait_for_exit env =
    kill_server_with_check env.server;
    let kill_signal_time = Unix.gettimeofday () in
    wait_for_server_exit_with_check env.server kill_signal_time

  let restart_server ?target_mini_state env exit_status =
    kill_server_and_wait_for_exit env;
    let informant_managed = Informant.is_managing env.informant in
    let new_server = start_server ?target_mini_state
      ~informant_managed env.server_start_options exit_status in
    { env with
      server = new_server;
      retries = env.retries + 1;
    }

  let update_status env monitor_config =
     let env, exit_status, server_state = update_status_ env monitor_config in
     let informant_report = Informant.report env.informant server_state in
     let is_watchman_fresh_instance = match exit_status with
       | Some c when c = Exit_status.(exit_code Watchman_fresh_instance) -> true
       | _ -> false in
     let is_watchman_failed = match exit_status with
       | Some c when c = Exit_status.(exit_code Watchman_failed) -> true
       | _ -> false in
     let is_config_changed = match exit_status with
       | Some c when c = Exit_status.(exit_code Hhconfig_changed) -> true
       | _ -> false in
     let is_file_heap_stale = match exit_status with
       | Some c when c = Exit_status.(exit_code File_heap_stale) -> true
       | _ -> false in
     let is_sql_assertion_failure = match exit_status with
       | Some c
          when c = Exit_status.(exit_code Sql_assertion_failure) ||
               c = Exit_status.(exit_code Sql_cantopen) ||
               c = Exit_status.(exit_code Sql_corrupt) ||
               c = Exit_status.(exit_code Sql_misuse) -> true
       | _ -> false in
     let max_watchman_retries = 3 in
     let max_sql_retries = 3 in
     if (is_watchman_failed || is_watchman_fresh_instance)
       && (env.retries < max_watchman_retries) then begin
       Hh_logger.log "Watchman died. Restarting hh_server (attempt: %d)"
         (env.retries + 1);
       restart_server env exit_status
     end
     else match informant_report with
     | Informant_sig.Restart_server target_mini_state ->
       Hh_logger.log "Informant directed server restart. Restarting server.";
       HackEventLogger.informant_induced_restart ();
       restart_server ?target_mini_state env exit_status
     | Informant_sig.Move_along ->
       if is_config_changed then begin
         Hh_logger.log "hh_server died from hh config change. Restarting";
         restart_server env exit_status
       end else if is_file_heap_stale then begin
         Hh_logger.log
          "Several large rebases caused FileHeap to be stale. Restarting";
         restart_server env exit_status
      end else if is_sql_assertion_failure
      && (env.retries < max_sql_retries) then begin
        Hh_logger.log
          "Sql failed. Restarting hh_server in fresh mode (attempt: %d)"
          (env.retries + 1);
        restart_server env exit_status
      end
       else
         env

  let read_version fd =
    let client_build_id: string = Marshal_tools.from_fd_with_preamble fd in
    let newline_byte = String.create 1 in
    let _ = Unix.read fd newline_byte 0 1 in
    if newline_byte <> "\n" then
      (Hh_logger.log "Did not find newline character after version";
       raise Malformed_build_id);
    client_build_id

  let rec handle_monitor_rpc env client_fd =
    let cmd : MonitorRpc.command =
      Marshal_tools.from_fd_with_preamble client_fd in
    match cmd with
    | MonitorRpc.HANDOFF_TO_SERVER handoff_options ->
      client_prehandoff env handoff_options client_fd
    | MonitorRpc.SHUT_DOWN ->
      Hh_logger.log "Got shutdown RPC. Shutting down.";
      let kill_signal_time = Unix.gettimeofday () in
      kill_server_with_check env.server;
      wait_for_server_exit_with_check env.server kill_signal_time;
      Exit_status.(exit No_error)

  and hand_off_client_connection server_fd client_fd =
    let status = Libancillary.ancil_send_fd server_fd client_fd in
    if (status <> 0) then
      (Hh_logger.log "Failed to handoff FD to server.";
       raise (Send_fd_failure status))
    else
      Sent_fds_collector.cleanup_fd client_fd

  (** Sends the client connection FD to the server process then closes the
   * FD. *)
  and hand_off_client_connection_with_retries server_fd retries client_fd =
    let _, ready_l, _ = Unix.select [] [server_fd] [] (0.5) in
    if ready_l <> [] then
      try hand_off_client_connection server_fd client_fd
      with
      | e ->
        if retries > 0 then
          (Hh_logger.log "Retrying FD handoff";
           hand_off_client_connection_with_retries
             server_fd (retries - 1) client_fd)
        else
          (Hh_logger.log "No more retries. Ignoring request.";
           HackEventLogger.send_fd_failure e;
           Unix.close client_fd;)
    else if retries > 0 then
      (Hh_logger.log "server socket not yet ready. Retrying.";
       hand_off_client_connection_with_retries
         server_fd (retries - 1) client_fd)
    else begin
      Hh_logger.log
        "server socket not yet ready. No more retries. Ignoring request.";
      Unix.close client_fd
    end

  (** Does not return. *)
  and client_out_of_date_ client_fd mismatch_info =
    msg_to_channel client_fd (Build_id_mismatch_ex mismatch_info);
    HackEventLogger.out_of_date ()

  (** Kills servers, sends build ID mismatch message to client, and exits.
   *
   * Does not return. Exits after waiting for server processes to exit. So
   * the client can wait for socket closure as indication that both the monitor
   * and server have exited.
  *)
  and client_out_of_date env client_fd mismatch_info =
    Hh_logger.log "Client out of date. Killing server.";
    kill_server_with_check env.server;
    let kill_signal_time = Unix.gettimeofday () in
    (** If we detect out of date client, should always kill server and exit
     * monitor, even if messaging to channel or event logger fails. *)
    (try client_out_of_date_ client_fd mismatch_info with
     | e -> Hh_logger.log
         "Handling client_out_of_date threw with: %s" (Printexc.to_string e));
    wait_for_server_exit_with_check env.server kill_signal_time;
    Exit_status.exit Exit_status.Build_id_mismatch

  (** Send (possibly empty) sequences of messages before handing off to
   * server. *)
  and client_prehandoff env handoff_options client_fd =
    let module PH = Prehandoff in
    match env.server with
    | Alive server ->
      let server_fd = snd @@ List.find_exn server.out_fds
        ~f:(fun x -> fst x = handoff_options.MonitorRpc.pipe_name) in
      let since_last_request =
        (Unix.time ()) -. !(server.last_request_handoff) in
      (** TODO: Send this to client so it is visible. *)
      Hh_logger.log "Got %s request for typechecker. Prior request %.1f seconds ago"
        handoff_options.MonitorRpc.pipe_name since_last_request;
      msg_to_channel client_fd PH.Sentinel;
      hand_off_client_connection_with_retries server_fd 8 client_fd;
      HackEventLogger.client_connection_sent ();
      server.last_request_handoff := Unix.time ();
      { env with server = (Alive server) }
    | Died_unexpectedly (status, was_oom) ->
      (** Server has died; notify the client *)
      msg_to_channel client_fd (PH.Server_died {PH.status; PH.was_oom});
      (** Next client to connect starts a new server. *)
      Exit_status.exit Exit_status.No_error
    | Not_yet_started
    | Informant_killed ->
      let env =
        if handoff_options.MonitorRpc.force_dormant_start then begin
          msg_to_channel client_fd (PH.Server_not_alive_dormant
            "Warning - starting a server by force-dormant-start option...");
          restart_server env None
        end else begin
          msg_to_channel client_fd (PH.Server_not_alive_dormant
            "Server killed by informant. Waiting for next server...");
          env
        end
      in
      if (Queue.length env.purgatory_clients) >= env.max_purgatory_clients then
        let () = msg_to_channel
          client_fd PH.Server_dormant_connections_limit_reached in
        env
      else
        let () = Queue.add (handoff_options, client_fd)
          env.purgatory_clients in
        env

  and ack_and_handoff_client env client_fd =
    try
      let client_version = read_version client_fd in
      if (not env.ignore_hh_version) && client_version <> Build_id.build_revision
      then
        client_out_of_date env client_fd ServerMonitorUtils.current_build_info
      else (
        msg_to_channel client_fd Connection_ok;
        handle_monitor_rpc env client_fd
      )
    with
    | Malformed_build_id as e ->
      HackEventLogger.malformed_build_id ();
      Hh_logger.log "Malformed Build ID";
      raise e

  and push_purgatory_clients env =
    (** We create a queue and transfer all the purgatory clients to it before
     * processing to avoid repeatedly retrying the same client even after
     * an EBADF. Control flow is easier this way than trying to manage an
     * immutable env in the face of exceptions. *)
    let clients = Queue.create () in
    Queue.transfer env.purgatory_clients clients;
    let env = Queue.fold begin
      fun env (handoff_options, client_fd) ->
        try client_prehandoff env handoff_options client_fd with
        | Unix.Unix_error(Unix.EPIPE, _, _)
        | Unix.Unix_error(Unix.EBADF, _, _) ->
          Hh_logger.log "Purgatory client disconnected. Dropping.";
          env
    end env clients in
    env

  and maybe_push_purgatory_clients env =
    match env.server, Queue.length env.purgatory_clients with
    | Alive _, 0 ->
      env
    | Alive _, _ ->
      push_purgatory_clients env
    | Not_yet_started, _ | Informant_killed, _ | Died_unexpectedly _, _ ->
      env

  let rec check_and_run_loop ?(consecutive_throws=0) env monitor_config
      (socket: Unix.file_descr) =
    let env, consecutive_throws =
      try check_and_run_loop_ env monitor_config socket, 0 with
      | Unix.Unix_error (Unix.ECHILD, _, _) ->
        let stack = Printexc.get_backtrace () in
        ignore (Hh_logger.log
          "check_and_run_loop_ threw with Unix.ECHILD. Exiting. - %s" stack);
        Exit_status.exit Exit_status.No_server_running
      | Exit_status.Exit_with _ as e -> raise e
      | e ->
        let stack = Printexc.get_backtrace () in
        if consecutive_throws > 500 then begin
          Hh_logger.log "Too many consecutive exceptions.";
          Hh_logger.log "Probably an uncaught exception rethrown each retry. Exiting. %s" stack;
          HackEventLogger.uncaught_exception e;
          Exit_status.exit Exit_status.Uncaught_exception
        end;
        Hh_logger.log "check_and_run_loop_ threw with exception: %s - %s"
          (Printexc.to_string e) stack;
        env, consecutive_throws + 1
      in
      check_and_run_loop ~consecutive_throws env monitor_config socket

  and check_and_run_loop_ env monitor_config
      (socket: Unix.file_descr) =
    let lock_file = monitor_config.lock_file in
    if not (Lock.grab lock_file) then
      (Hh_logger.log "Lost lock; terminating.\n%!";
       HackEventLogger.lock_stolen lock_file;
       Exit_status.(exit Lock_stolen));
    let env = maybe_push_purgatory_clients env in
    let () = Sent_fds_collector.collect_garbage () in
    let has_client = sleep_and_check socket in
    let env = update_status env monitor_config in
    if (not has_client) then
      env
    else
    try
      let fd, _ = Unix.accept socket in
      try
        HackEventLogger.accepted_client_fd (fd_to_int fd);
        ack_and_handoff_client env fd
      with
      | Exit_status.Exit_with _ as e -> raise e
      | e ->
        (HackEventLogger.ack_and_handoff_exception e;
         Hh_logger.log
           "Handling client connection failed. Ignoring connection attempt.";
         Unix.close fd;
         env)
    with
    | Exit_status.Exit_with _ as e -> raise e
    | e ->
      (HackEventLogger.accepting_on_socket_exception e;
       Hh_logger.log
         "Accepting on socket failed. Ignoring client connection attempt.";
         env)

  let check_and_run_loop_once (env, monitor_config, socket) =
    let env = check_and_run_loop_ env monitor_config socket in
    env, monitor_config, socket

  let start_monitor ~waiting_client ~max_purgatory_clients
    server_start_options informant_init_env
    monitor_config =
    let socket = Socket.init_unix_socket monitor_config.socket_file in
    (* If the client started the server, it opened an FD before forking, so it
     * can be notified when the monitor socket is ready. The FD number was
     * passed in program args. *)
    Option.iter waiting_client begin fun fd ->
      let oc = Unix.out_channel_of_descr fd in
      try
        output_string oc (ServerMonitorUtils.ready^"\n");
        close_out oc;
      with
      | Sys_error _
      | Unix.Unix_error _ as e ->
        Printf.eprintf "Caught exception while waking client: %s\n%!"
          (Printexc.to_string e)
    end;
    (** It is essential that we initiate the Informant before the server if we
     * want to give the opportunity for the Informant to truly take
     * ownership over the lifetime of the server.
     *
     * This is because start_server won't actually start a server if it sees
     * a hg update sentinel file indicating an hg update is in-progress.
     * Starting the informant first ensures that its Watchman watch is started
     * before we check for the hgupdate sentinel file - this is required
     * for the informant to properly observe an update is complete without
     * hitting race conditions. *)
    let informant = Informant.init informant_init_env in
    let server_process = maybe_start_first_server
      server_start_options informant in
    let env = {
      informant;
      max_purgatory_clients;
      purgatory_clients = Queue.create ();
      server = server_process;
      server_start_options;
      retries = 0;
      ignore_hh_version = Informant.should_ignore_hh_version informant_init_env;
    } in
    env, monitor_config, socket

  let start_monitoring ~waiting_client ~max_purgatory_clients
    server_start_options informant_init_env
    monitor_config =
    let env, monitor_config, socket = start_monitor ~waiting_client ~max_purgatory_clients
      server_start_options informant_init_env monitor_config in
    check_and_run_loop env monitor_config socket
end
