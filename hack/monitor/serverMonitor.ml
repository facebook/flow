(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
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

open Core
open Utils
open ServerProcess
open ServerProcessTools
open ServerMonitorUtils

exception Malformed_build_id
exception Misconfigured_monitor
exception Send_fd_failure of int

type env = {
  servers: ServerProcess.server_process SMap.t;
  (** Invoke this to re-launch the processes. *)
  starter: ServerMonitorUtils.monitor_starter;
  (** How many times have we tried to relaunch it? *)
  retries: int;
}

let fd_to_int (x: Unix.file_descr) : int = Obj.magic x

let msg_to_channel fd msg =
  (* This FD will be passed to a server process, so avoid using Ocaml's
   * channels which have built-in buffering. Even though we are only writing
   * to the FD here, it seems using Ocaml's channels also causes read buffering
   * to happen here, so the server process doesn't get what was meant for it. *)
  Marshal_tools.to_fd_with_preamble fd msg

let kill_server process =
  try Unix.kill process.pid Sys.sigusr2 with
  | _ ->
    Hh_logger.log
      "Failed to send sigusr2 signal to server process. Trying \
       violently";
    try Unix.kill process.pid Sys.sigkill with e ->
      Hh_logger.exc ~prefix: "Failed to violently kill server process: " e

let rec wait_for_server_exit process start_t =
  let exit_status = Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] process.pid in
  match exit_status with
  | 0, _ ->
    Unix.sleep 1;
    wait_for_server_exit process start_t
  | _ ->
    ignore (
      Hh_logger.log_duration (Printf.sprintf
        "%s has exited. Time since sigterm: " process.name) start_t)

let setup_handler_for_signals handler signals =
  List.iter signals begin fun signal ->
    Sys_utils.set_signal signal (Sys.Signal_handle handler)
  end

let setup_autokill_servers_on_exit processes =
  try
    setup_handler_for_signals begin fun _ ->
      Hh_logger.log "Got an exit signal. Killing server and exiting.";
      List.iter processes ~f:kill_server;
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
let update_status_ (server: ServerProcess.server_process) = match server with
  | Alive process ->
    let pid, proc_stat =
      Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] process.pid in
    (match pid, proc_stat with
      | 0, _ ->
        server
      | _, _ ->
        ServerProcessTools.check_exit_status proc_stat process;
        Died_unexpectedly (proc_stat, (check_dmesg_for_oom process)))
  | _ -> server

let start_servers monitor_starter =
  let server_processes = monitor_starter () in
  setup_autokill_servers_on_exit server_processes;
  List.fold_left server_processes ~init:SMap.empty
    ~f:begin fun acc x -> match SMap.get x.name acc with
      | None -> SMap.add x.name (Alive x) acc
      | Some _ ->
        Hh_logger.log
          "Monitored server names must be unique. Got %s more than once."
          x.name;
        raise Misconfigured_monitor
    end

let kill_servers servers =
  SMap.iter begin fun _ server -> match server with
    | Alive server ->
      kill_server server
    | _ -> ()
  end servers

let wait_for_servers_exit servers kill_signal_time =
  SMap.iter begin fun _ server -> match server with
    | Alive server ->
      wait_for_server_exit server kill_signal_time
    | _ -> ()
  end servers

let restart_servers env =
  kill_servers env.servers;
  let kill_signal_time = Unix.gettimeofday () in
  wait_for_servers_exit env.servers kill_signal_time;
  let new_servers = start_servers env.starter in
  { env with
    servers = new_servers;
    retries = env.retries + 1;
  }

let update_status env =
   let servers = SMap.map update_status_ env.servers in
   let env = { env with servers = servers } in
   let watchman_failed _ status = match status with
     | Died_unexpectedly ((Unix.WEXITED c), _)
        when c = Exit_status.(ec Watchman_failed) -> true
     | _ -> false in
   let config_changed _ status = match status with
     | Died_unexpectedly ((Unix.WEXITED c), _)
        when c = Exit_status.(ec Hhconfig_changed) -> true
     | _ -> false in
   let max_watchman_retries = 3 in
   if (SMap.exists watchman_failed servers)
     && (env.retries < max_watchman_retries) then begin
     Hh_logger.log "Watchman died. Restarting hh_server (attempt: %d)"
       (env.retries + 1);
     restart_servers env
   end
   else if SMap.exists config_changed servers then begin
     Hh_logger.log "hh_server died from hh config change. Restarting";
     restart_servers env
   end
   else
     { env with servers = servers }

let read_build_id_ohai fd =
  let client_build_id: string = Marshal_tools.from_fd_with_preamble fd in
  let newline_byte = String.create 1 in
  let _ = Unix.read fd newline_byte 0 1 in
  if newline_byte <> "\n" then
    (Hh_logger.log "Did not find newline character after build_id ohai";
     raise Malformed_build_id);
  client_build_id

let rec handle_monitor_rpc env client_fd =
  let cmd : MonitorRpc.command =
    Marshal_tools.from_fd_with_preamble client_fd in
  match cmd with
  | MonitorRpc.HANDOFF_TO_SERVER server_name ->
    client_prehandoff env server_name client_fd
  | MonitorRpc.SHUT_DOWN ->
    Hh_logger.log "Got shutdown RPC. Shutting down.";
    let kill_signal_time = Unix.gettimeofday () in
    kill_servers env.servers;
    wait_for_servers_exit env.servers kill_signal_time;
    Exit_status.(exit Ok)

and hand_off_client_connection server client_fd =
  let status = Libancillary.ancil_send_fd server.out_fd client_fd in
  if (status <> 0) then
    (Hh_logger.log "Failed to handoff FD to server.";
     raise (Send_fd_failure status))
  else
    (Unix.close client_fd;
     ())

(** Sends the client connection FD to the server process then closes the
 * FD. *)
and hand_off_client_connection_with_retries server retries client_fd =
  let _, ready_l, _ = Unix.select [] [server.out_fd] [] (0.5) in
  if ready_l <> [] then
    try hand_off_client_connection server client_fd
    with
    | e ->
      if retries > 0 then
        (Hh_logger.log "Retrying FD handoff";
         hand_off_client_connection_with_retries
           server (retries - 1) client_fd)
      else
        (Hh_logger.log "No more retries. Ignoring request.";
         HackEventLogger.send_fd_failure e;
         Unix.close client_fd;)
  else if retries > 0 then
    (Hh_logger.log "server socket not yet ready. Retrying.";
     hand_off_client_connection_with_retries
       server (retries - 1) client_fd)
  else
    (Hh_logger.log
       "server socket not yet ready. No more retries. Ignoring request.")

(** Does not return. *)
and client_out_of_date_ client_fd =
  msg_to_channel client_fd Build_id_mismatch;
  HackEventLogger.out_of_date ()

(** Kills servers, sends build ID mismatch message to client, and exits.
 *
 * Does not return. Exits after waiting for server processes to exit. So
 * the client can wait for socket closure as indication that both the monitor
 * and server have exited.
*)
and client_out_of_date env client_fd =
  kill_servers env.servers;
  let kill_signal_time = Unix.gettimeofday () in
  (** If we detect out of date client, should always kill server and exit
   * monitor, even if messaging to channel or event logger fails. *)
  (try client_out_of_date_ client_fd with
   | e -> Hh_logger.log
       "Handling client_out_of_date threw with: %s" (Printexc.to_string e));
  wait_for_servers_exit env.servers kill_signal_time;
  Exit_status.exit Exit_status.Build_id_mismatch

(** Send (possibly empty) sequences of messages before handing off to
 * server. *)
and client_prehandoff env server_name client_fd =
  let module PH = Prehandoff in
  match SMap.get server_name env.servers with
  | None ->
    msg_to_channel client_fd PH.Server_name_not_found;
    env
  | Some (Alive server) ->
    let since_last_request =
      (Unix.time ()) -. !(server.last_request_handoff) in
    (** TODO: Send this to client so it is visible. *)
    Hh_logger.log "Got request for %s. Prior request %.1f seconds ago"
      server_name since_last_request;
    msg_to_channel client_fd PH.Sentinel;
    hand_off_client_connection_with_retries server 8 client_fd;
    HackEventLogger.client_connection_sent ();
    server.last_request_handoff := Unix.time ();
    { env with servers = SMap.add server_name (Alive server) env.servers }
  | Some (Died_unexpectedly (status, was_oom)) ->
    (** Server has died; notify the client *)
    msg_to_channel client_fd (PH.Server_died {PH.status; PH.was_oom});
    (** Next client to connect starts a new server. *)
    Exit_status.exit Exit_status.Ok

and ack_and_handoff_client env client_fd =
  try
    let client_build_id = read_build_id_ohai client_fd in
    if client_build_id <> Build_id.build_id_ohai
    then
      client_out_of_date env client_fd
    else (
      msg_to_channel client_fd Connection_ok;
      handle_monitor_rpc env client_fd
    )
  with
  | Marshal_tools.Malformed_Preamble_Exception ->
    (** TODO: Remove this after 2 Hack deploys. *)
    (Hh_logger.log "
        Marshal tools read malformed preamble, interpreting as version change.
        ";
     client_out_of_date env client_fd)
  | Malformed_build_id as e ->
    HackEventLogger.malformed_build_id ();
    Hh_logger.log "Malformed Build ID";
    raise e

let rec check_and_run_loop env
    (lock_file: string) (socket: Unix.file_descr) =
  let env = try check_and_run_loop_ env lock_file socket with
  | Unix.Unix_error (Unix.ECHILD, _, _) ->
    ignore (Hh_logger.log
      "check_and_run_loop_ threw with Unix.ECHILD. Exiting");
    Exit_status.exit Exit_status.No_server_running
  | e ->
    Hh_logger.log "check_and_run_loop_ threw with exception: %s"
      (Printexc.to_string e);
    env
  in
    check_and_run_loop env lock_file socket

and check_and_run_loop_ env
    (lock_file: string) (socket: Unix.file_descr) =
  if not (Lock.grab lock_file) then
    (Hh_logger.log "Lost lock; terminating.\n%!";
     HackEventLogger.lock_stolen lock_file;
     Exit_status.(exit Lock_stolen));
  let has_client = sleep_and_check socket in
  let env = update_status env in
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

let start_monitoring monitor_config monitor_starter =
  let socket = Socket.init_unix_socket monitor_config.socket_file in
  let server_processes = start_servers monitor_starter in
  let env = {
    servers = server_processes;
    starter = monitor_starter;
    retries = 0;
  } in
  check_and_run_loop env monitor_config.lock_file socket
