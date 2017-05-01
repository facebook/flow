(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ServerMonitorUtils

let server_exists lock_file = not (Lock.check lock_file)

let from_channel_without_buffering tic =
  Marshal_tools.from_fd_with_preamble (Timeout.descr_of_in_channel tic)

let wait_on_server_restart ic =
  try
    while true do
      let _ = Timeout.input_char ic in
      ()
    done
  with
  | End_of_file
  | Sys_error _ ->
     (* Server has exited and hung up on us *)
     ()

let send_version oc =
  Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel oc)
    Build_id.build_revision;
  (** For backwards-compatibility, newline has always followed the version *)
  let _ = Unix.write (Unix.descr_of_out_channel oc) "\n" 0 1 in
  ()

let send_server_handoff_rpc handoff_options oc =
  Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel oc)
    (MonitorRpc.HANDOFF_TO_SERVER handoff_options)

let send_shutdown_rpc oc =
  Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel oc)
    MonitorRpc.SHUT_DOWN

let establish_connection ~timeout config =
  let sock_name = Socket.get_path config.socket_file in
  let sockaddr =
    if Sys.win32 then
      let ic = open_in_bin sock_name in
      let port = input_binary_int ic in
      close_in ic;
      Unix.(ADDR_INET (inet_addr_loopback, port))
    else
      Unix.ADDR_UNIX sock_name in
  try Result.Ok (Timeout.open_connection ~timeout sockaddr) with
  | Unix.Unix_error (Unix.ECONNREFUSED, _, _)
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
    if not (server_exists config.lock_file) then Result.Error Server_missing
    else Result.Error Server_busy

let get_cstate config (ic, oc) =
  try
    send_version oc;
    let cstate : connection_state = from_channel_without_buffering ic in
    Result.Ok (ic, oc, cstate)
  with _ ->
    Timeout.shutdown_connection ic;
    Timeout.close_in_noerr ic;
    if not (server_exists config.lock_file) then Result.Error Server_missing
    else Result.Error Monitor_connection_failure

let verify_cstate ic = function
  | Connection_ok -> Result.Ok ()
  | Build_id_mismatch ->
      (* The server is out of date and is going to exit. Subsequent calls
       * to connect on the Unix Domain Socket might succeed, connecting to
       * the server that is about to die, and eventually we will be hung
       * up on while trying to read from our end.
       *
       * To avoid that fate, when we know the server is about to exit, we
       * wait for the connection to be closed, signaling that the server
       * has exited and the OS has cleaned up after it, then we try again.
       *
       * See also: ServerMonitor.client_out_of_date
       *)
      wait_on_server_restart ic;
      Timeout.close_in_noerr ic;
      Result.Error Build_id_mismatched

(** Consume sequence of Prehandoff messages. *)
let rec consume_prehandoff_messages ic oc =
  let module PH = Prehandoff in
  let m: PH.msg = from_channel_without_buffering ic in
  match m with
  | PH.Sentinel -> Result.Ok (ic, oc)
  | PH.Server_name_not_found ->
    Printf.eprintf
      "Requested server name not found. This is probably a bug in Hack.";
    raise (Exit_status.Exit_with (Exit_status.Server_name_not_found));
  | PH.Server_dormant_connections_limit_reached ->
    Printf.eprintf @@ "Connections limit on dormant server reached."^^
      " Be patient waiting for a server to be started.";
    Result.Error Server_dormant
  | PH.Server_not_alive_dormant _ ->
    Printf.eprintf "Waiting for a server to be started...\n%!";
    consume_prehandoff_messages ic oc
  | PH.Server_died {PH.status; PH.was_oom} ->
    (match was_oom, status with
    | true, _ ->
      Printf.eprintf "Last server killed by OOM Manager.\n%!";
    | false, Unix.WEXITED exit_code ->
      Printf.eprintf "Last server exited with code: %d.\n%!" exit_code
    | false, Unix.WSIGNALED signal ->
      Printf.eprintf "Last server killed by signal: %d.\n%!" signal
    | false, Unix.WSTOPPED signal ->
      Printf.eprintf "Last server stopped by signal: %d.\n%!" signal);
    wait_on_server_restart ic;
    Result.Error Server_died

let connect_to_monitor config =
  let open Result in
  try
    Timeout.with_timeout
      ~timeout:1
      ~on_timeout:(fun _ -> raise Timeout.Timeout)
      ~do_:begin fun timeout ->
        establish_connection ~timeout config >>= fun (ic, oc) ->
        get_cstate config (ic, oc)
      end
  with
  | Timeout.Timeout ->
    (* It looks like the socket is not always immediately released after
     * process dies, and subsequent calls to it hang *)
    HackEventLogger.client_connect_to_monitor_timeout ();
    if not (server_exists config.lock_file) then Result.Error Server_missing
    else Result.Error ServerMonitorUtils.Server_busy

let connect_and_shut_down config =
  let open Result in
  connect_to_monitor config >>= fun (ic, oc, cstate) ->
  verify_cstate ic cstate >>= fun () ->
  send_shutdown_rpc oc;
  try Timeout.with_timeout
    ~timeout:3
    ~on_timeout:(fun _ -> ())
    ~do_:begin fun _ ->
      wait_on_server_restart ic;
      Result.Ok ServerMonitorUtils.SHUTDOWN_VERIFIED
    end
  with
  | Timeout.Timeout ->
    if not (server_exists config.lock_file) then Result.Error Server_missing
    else Result.Ok ServerMonitorUtils.SHUTDOWN_UNVERIFIED

let connect_once config handoff_options =
  let open Result in
  connect_to_monitor config >>= fun (ic, oc, cstate) ->
  verify_cstate ic cstate >>= fun () ->
  send_server_handoff_rpc handoff_options oc;
  consume_prehandoff_messages ic oc
