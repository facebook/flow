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
open ServerProcess
open ServerProcessTools
open ServerMonitorUtils

exception Malformed_build_id
exception Send_fd_failure of int

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
      "Failed to send Sig_build_id_mismatch signal to server process. Trying \
       violently";
    Unix.kill process.pid Sys.sigkill

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

let setup_autokill_server_on_exit process =
  try
    setup_handler_for_signals begin fun _ ->
      Hh_logger.log "Got an exit signal. Killing server and exiting.";
      kill_server process;
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
let update_status (server: ServerProcess.server_process)
~has_client =
  match server with
  | Alive process ->
    let pid, proc_stat =
      Unix.waitpid [Unix.WNOHANG; Unix.WUNTRACED] process.pid in
    (match pid, proc_stat with
      | 0, _ ->
        server
      | _, Unix.WEXITED 0 ->
        (** This is rare - server stopped by RPC kill command and another
         * client connected within the same 1-second check loop. This monitor
         * will exit, but has to notify the client that's already connected. *)
        if has_client then Killed_intentionally
        else Exit_status.(exit Ok)
      | _, Unix.WEXITED c
      when c = (Exit_status.ec Exit_status.Hhconfig_changed) ->
        if has_client then Killed_intentionally
        else Exit_status.(exit Ok)
      | _, _ ->
        ServerProcessTools.check_exit_status proc_stat process;
        Died_unexpectedly (proc_stat, (check_dmesg_for_oom process)))
  | _ -> server

let read_build_id_ohai fd =
  let client_build_id: string = Marshal_tools.from_fd_with_preamble fd in
  let newline_byte = String.create 1 in
  let _ = Unix.read fd newline_byte 0 1 in
  if newline_byte <> "\n" then
    (Hh_logger.log "Did not find newline character after build_id ohai";
     raise Malformed_build_id);
  client_build_id

let hand_off_client_connection server client_fd =
  let status = Libancillary.ancil_send_fd server.out_fd client_fd in
  if (status <> 0) then
    (Hh_logger.log "Failed to handoff FD to server.";
     raise (Send_fd_failure status))
  else
    (Unix.close client_fd;
     ())

(** Sends the client connection FD to the server process then closes the
 * FD. *)
let rec hand_off_client_connection_with_retries server retries client_fd =
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
let client_out_of_date_ client_fd =
  msg_to_channel client_fd Build_id_mismatch;
  HackEventLogger.out_of_date ()

(** Kills server, sends build ID mismatch message to client, and exits.
 *
 * Does not return. Exits after waiting for server process to exit. So
 * the client can wait for socket closure as indication that both the monitor
 * and server have exited.
*)
let client_out_of_date server client_fd =
  (match server with
  | Alive server ->
    kill_server server
  | _ -> ());
  let kill_signal_time = Unix.gettimeofday () in
  (** If we detect out of date client, should always kill server and exit
   * monitor, even if messaging to channel or event logger fails. *)
  (try client_out_of_date_ client_fd with
   | e -> Hh_logger.log
       "Handling client_out_of_date threw with: %s" (Printexc.to_string e));
  (match server with
  | Alive server ->
    wait_for_server_exit server kill_signal_time
  | _ -> ());
  Exit_status.exit Exit_status.Build_id_mismatch

(** Send (possibly empty) sequences of messages before handing off to
 * server. *)
let client_prehandoff server client_fd =
  let open Prehandoff in
  match server with
  | Killed_intentionally ->
    msg_to_channel client_fd Shutting_down;
    Exit_status.exit Exit_status.Ok
  | Alive server ->
    let since_last_request =
      (Unix.time ()) -. !(server.last_request_handoff) in
    (** TODO: Send this to client so it is visible. *)
    Hh_logger.log "Got request. Prior request %.1f seconds ago"
      since_last_request;
    msg_to_channel client_fd Sentinel;
    hand_off_client_connection_with_retries server 8 client_fd;
    HackEventLogger.client_connection_sent ();
    server.last_request_handoff := Unix.time ();
    Alive server
  | Died_unexpectedly (status, was_oom) ->
    (** Server has died; notify the client *)
    msg_to_channel client_fd (Server_died {status; was_oom});
    (** Next client to connect starts a new server. *)
    Exit_status.exit Exit_status.Ok

let ack_and_handoff_client server client_fd =
  try
    let client_build_id = read_build_id_ohai client_fd in
    if client_build_id <> Build_id.build_id_ohai
    then
      client_out_of_date server client_fd
    else (
      msg_to_channel client_fd Connection_ok;
      client_prehandoff server client_fd
    )
  with
  | Marshal_tools.Malformed_Preamble_Exception ->
    (** TODO: Remove this after 2 Hack deploys. *)
    (Hh_logger.log "
        Marshal tools read malformed preamble, interpreting as version change.
        ";
     client_out_of_date server client_fd)
  | Malformed_build_id as e ->
    HackEventLogger.malformed_build_id ();
    Hh_logger.log "Malformed Build ID";
    raise e

let rec check_and_run_loop server
    (lock_file: string) (socket: Unix.file_descr) =
  let server = try check_and_run_loop_ server lock_file socket with
  | e ->
    Hh_logger.log "check_and_run_loop_ threw with exception: %s"
      (Printexc.to_string e);
    server
  in
    check_and_run_loop server lock_file socket

and check_and_run_loop_ server
    (lock_file: string) (socket: Unix.file_descr) =
  let server = update_status server ~has_client:false in
  if not (Lock.grab lock_file) then
    (Hh_logger.log "Lost lock; terminating.\n%!";
     HackEventLogger.lock_stolen lock_file;
     Exit_status.(exit Lock_stolen));
  let has_client = sleep_and_check socket in
  let server = update_status server ~has_client in
  if (not has_client) then
    server
  else
  try
    let fd, _ = Unix.accept socket in
    try
      HackEventLogger.accepted_client_fd (fd_to_int fd);
      ack_and_handoff_client server fd
    with
    | e ->
      (HackEventLogger.ack_and_handoff_exception e;
       Hh_logger.log
         "Handling client connection failed. Ignoring connection attempt.";
       Unix.close fd;
       server)
  with
  | e ->
    (HackEventLogger.accepting_on_socket_exception e;
     Hh_logger.log
       "Accepting on socket failed. Ignoring client connection attempt.";
       server)

let start_monitoring monitor_config server_daemon_starter =
  let socket = Socket.init_unix_socket monitor_config.socket_file in
  let server_process = server_daemon_starter () in
  setup_autokill_server_on_exit server_process;
  check_and_run_loop (Alive server_process) monitor_config.lock_file socket
