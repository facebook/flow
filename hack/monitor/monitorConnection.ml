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
    else Result.Error Monitor_socket_not_ready

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

let verify_cstate ic cstate =
  match cstate with
  | Connection_ok -> Result.Ok ()
  | Build_id_mismatch | Build_id_mismatch_ex _ ->
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
       *
       * TODO: around September 2017, add an assert that the Build_id_mismatch
       * form will never arise; only the _ex form will be emitted.
       * At that point we can make Build_id_mismatched take a non-optional.
       * Why that date? As of early July, the server started emitting only the
       * _ex form. We'll give the change time to percolate, until no one
       * will realistically be running the old form of the server, and by that
       * time we can add the assert.
       *)
      wait_on_server_restart ic;
      Timeout.close_in_noerr ic;
      let mismatch_info = match cstate with
        | Build_id_mismatch_ex mismatch_info -> Some mismatch_info
        | _ -> None
      in
      Result.Error (Build_id_mismatched mismatch_info)

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

let connect_to_monitor ~timeout config =
  let open Result in
  Timeout.with_timeout
    ~timeout
    ~on_timeout:(fun _ ->
      (**
      * Monitor should always readily accept connections. In theory, this will
      * only timeout if the Monitor is being very heavily DDOS'd, or the Monitor
      * has wedged itself (a bug).
      *
      * The DDOS occurs when the Monitor's new connections (arriving on
      * the socket) queue grows faster than they are being processed. This can
      * happen in two scenarios:
        * 1) Malicious DDOSer fills up new connection queue (incoming
        *    connections on the socket) quicker than the queue is being
        *    consumed.
        * 2) New client connections to the monitor are being created by the
        *    retry logic in hh_client faster than those cancelled connections
        *    (cancelled due to the timeout above) are being discarded by the
        *    monitor. This could happen from thousands of hh_clients being
        *    used to parallelize a job. This is effectively an inadvertent DDOS.
        *    In detail, suppose the timeout above is set to 1 ssecond and that
        *    1000 thousand hh_client have timed out at the line above. Then these
        *    1000 clients will cancel the connection and retry. But the Monitor's
        *    connection queue still has these dead/canceled connections waiting
        *    to be processed. Suppose it takes the monitor longer than 1
        *    millisecond to handle and discard a dead connection. Then the
        *    1000 retrying hh_clients will again add another 1000 dead
        *    connections during retrying even tho the monitor has discarded
        *    fewer than 1000 dead connections. Thus, no progress will be made
        *    on clearing out dead connections and all new connection attempts
        *    will time out.
        *
        *    We ameliorate this by having the timeout be quite large
        *    (many seconds) and by not auto-retrying connections to the Monitor.
      * *)
      HackEventLogger.client_connect_to_monitor_timeout ();
      if not (server_exists config.lock_file) then Result.Error Server_missing
      else Result.Error ServerMonitorUtils.Monitor_establish_connection_timeout
    )
    ~do_:begin fun timeout ->
      establish_connection ~timeout config >>= fun (ic, oc) ->
      get_cstate config (ic, oc)
    end

let connect_and_shut_down config =
  let open Result in
  connect_to_monitor ~timeout:3 config >>= fun (ic, oc, cstate) ->
  verify_cstate ic cstate >>= fun () ->
  send_shutdown_rpc oc;
  Timeout.with_timeout
    ~timeout:3
    ~on_timeout:(fun () ->
      if not (server_exists config.lock_file) then Result.Error Server_missing
      else Result.Ok ServerMonitorUtils.SHUTDOWN_UNVERIFIED
    )
    ~do_:begin fun _ ->
      wait_on_server_restart ic;
      Result.Ok ServerMonitorUtils.SHUTDOWN_VERIFIED
    end

let connect_once ~timeout config handoff_options =
  (***************************************************************************)
  (* CONNECTION HANDSHAKES                                                   *)
  (* Explains what connect_once does+returns, and how callers use the result.*)
  (***************************************************************************)
  (* 1. OPEN SOCKET. After this point we have a working stdin/stdout to the  *)
  (* process. Implemented in establish_connection.                           *)
  (*   | catch EConnRefused/ENoEnt/Timeout 1s when lockfile present ->       *)
  (*     Result.Error Monitor_socket_not_ready.                              *)
  (*       This is unexpected! But can happen if you manage to catch the     *)
  (*       monitor in the short timeframe after it has grabbed its lock but  *)
  (*       before it has started listening in on its socket.                 *)
  (*       -> "hh_client check/ide" -> retry from step 1, up to 800 times.   *)
  (*          The number 800 is hard-coded in 9 places through the codebase. *)
  (*       -> "hh_client start" -> print "replacing unresponsive server"     *)
  (*              kill_server; start_server; exit.                           *)
  (*   | catch Timeout <retries>s when lockfile present ->                   *)
  (*     Result.Error Monitor_establish_connection_timeout                   *)
  (*       This is unexpected! after all the monitor is always responsive,   *)
  (*       and indeed start_server waits until responsive before returning.  *)
  (*       But this can happen during a DDOS.                                *)
  (*       -> "hh_client check/ide" -> Its retry attempts are passed to the  *)
  (*           monitor connection attempt already. So in this timeout all    *)
  (*           the retries have already been consumed. Just exit.            *)
  (*       -> "hh_client start" -> print "replacing unresponsive server"     *)
  (*              kill_server; start_server; exit.                           *)
  (*   | catch EConnRefused/ENoEnt/Timeout when lockfile absent ->           *)
  (*     Result.Error Server_missing.                                        *)
  (*       -> "hh_client ide" -> raise Exit_with IDE_no_server.              *)
  (*       -> "hh_client check" -> start_server; retry step 1, up to 800x.   *)
  (*       -> "hh_client start" -> start_server; exit.                       *)
  (*   | catch other exception -> unhandled.                                 *)
  (*                                                                         *)
  (* 2. SEND VERSION; READ VERSION; CHECK VERSIONS. After this point we can  *)
  (* safely marshal OCaml types back and forth. Implemented in get_cstate    *)
  (* and verify_cstate.                                                      *)
  (*   | catch any exception when lockfile present ->                        *)
  (*     close_connection; Result.Error Monitor_connection_failure.          *)
  (*       This is unexpected!                                               *)
  (*       -> "hh_client check/ide" -> retry from step 1, up to 800 times.   *)
  (*       -> "hh_client start" -> print "replacing unresponsive server"     *)
  (*              kill_server; start_server; exit.                           *)
  (*   | catch any exception when lockfile absent ->                         *)
  (*     close_connection; Result.Error Server_missing.                      *)
  (*       -> "hh_client ide" -> raise Exit_with IDE_no_server               *)
  (*       -> "hh_client check" -> start_server; retry step 1, up to 800x.   *)
  (*       -> "hh_client start" -> start_server; exit.                       *)
  (*   | if version numbers differ ->                                        *)
  (*     Result.Error Build_mismatch.                                        *)
  (*       -> "hh_client ide" -> raise Exit_with IDE_no_server.              *)
  (*       -> "hh_client check" -> close_log_tailer; retry from step 1.      *)
  (*       -> "hh_client start" -> start_server; exit.                       *)
  (*                                                                         *)
  (* 3. SEND HANDOFF; READ RESPONSE. After this point we have a working      *)
  (* connection to a server who we believe is ready to handle our messages.  *)
  (* Handoff is the stage of the protocol when we're speaking to the monitor *)
  (* rather than directly to the server process itself. Implemented in       *)
  (* send_server_handoff_rpc and consume_prehandoff_message.                 *)
  (*   | response Server_name_not_found ->                                   *)
  (*     raise Exit_with Server_name_not_found.                              *)
  (*   | response Server_not_alive_dormant ->                                *)
  (*     print "Waiting for server to start"; retry step 5, unlimited times. *)
  (*   | response Server_dormant_connections_limit_reached ->                *)
  (*     Result.Error Server_dormant.                                        *)
  (*       -> "hh_client ide" -> raise Exit_with IDE_no_server.              *)
  (*       -> "hh_client start" -> print "Server already exists but is       *)
  (*         dormant"; exit.                                                 *)
  (*       -> "hh_client check" -> print "No server running, and connection  *)
  (*         limit reached for waiting  on the next server to be started.    *)
  (*         Please wait patiently." raise Exit_with No_server_running.      *)
  (*   | response Server_died ->                                             *)
  (*     print "Last killed by OOM / signal / stopped by signal / exited";   *)
  (*     wait for server to close; Result.Error Server_died.                 *)
  (*       -> "hh_client ide" -> raise Exit_with IDE_no_server.              *)
  (*       -> "hh_client start" -> start_server.                             *)
  (*       -> "hh_client check" -> retry from step 1, up to 800 times.       *)
  (*   | catch any exception -> unhandled.                                   *)
  (*                                                                         *)
  (* The following two steps aren't implemented inside connect_once but are  *)
  (* typically done by callers after connect_once has succeeded...           *)
  (*                                                                         *)
  (* 4. READ "HELLO" FROM SERVER. After this point we have evidence that the *)
  (* server is ready to handle our messages. We basically gobble whatever    *)
  (* the server sends until it finally sends a line with just "hello".       *)
  (* Implemented in wait_for_server_hello.                                   *)
  (*   | read anything other than "hello" -> retry from step 4, up to 800x.  *)
  (*   | catch Timeout 1s -> retry from step 4, up to 800 times.             *)
  (*   | catch exception EndOfFile/Sys_error ->                              *)
  (*     raise ServerHungUp.                                                 *)
  (*       -> "hh_client ide/check" -> program exit, code=No_server_running. *)
  (*       -> clientStart never actually bothers to do step 4.               *)
  (*   | catch other exception -> unhandled.                                 *)
  (*                                                                         *)
  (* 5. SEND CONNECTION TYPE; READ RESPONSE. After this point we have        *)
  (* evidence that the server is able to handle our connection. The          *)
  (* connection type indicates Persistent vs Non-persistent.                 *)
  (*   | reponse Denied_due_to_existing_persistent_connection.               *)
  (*       -> "hh_client lsp" -> raise Lsp.Error_server_start.               *)
  (*   | catch any exception -> unhandled.                                   *)
  (***************************************************************************)
  let open Result in
  connect_to_monitor ~timeout config >>= fun (ic, oc, cstate) ->
  verify_cstate ic cstate >>= fun () ->
  send_server_handoff_rpc handoff_options oc;
  consume_prehandoff_messages ic oc
