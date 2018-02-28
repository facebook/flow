(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module listens to the socket, accepts new connections, and starts the
 * FlowServerMonitorConnections which handle those connects *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
let spf = Printf.sprintf

module Logger = FlowServerMonitorLogger
module Server = FlowServerMonitorServer

(* Just forward requests to the server *)
let handle_ephemeral_request ~msg ~connection =
  Lwt.return (Server.send_request ~client:connection ~request:msg)

(* Just forward requests to the server *)
let handle_persistent_message ~client_id ~msg ~connection:_ =
  Logger.debug "Persistent connection #%d received a message!" client_id;
  Lwt.return (Server.send_persistent_request ~client_id ~request:msg)

(* A loop that sends the Server's busy status to a waiting ephemeral connection every 0.5 seconds *)
module StatusLoop = LwtLoop.Make (struct
  type acc = EphemeralConnection.t

  let main conn =
    StatusStream.wait_for_signficant_status ~timeout:0.5
    >|= (fun status ->
      EphemeralConnection.write ~msg:(MonitorProt.Please_hold status) conn;
      conn
    )

  let catch _ exn =
    begin match exn with
    (* The connection closed its write stream, likely it is closed or closing *)
    | Lwt_stream.Closed -> ()
    | exn -> Logger.error ~exn "StatusLoop threw and exception"
    end;
    Lwt.return_unit
end)

let create_ephemeral_connection ~client_fd ~close =
  Logger.debug "Creating a new ephemeral connection";

  EphemeralConnection.create
    ~name:"some ephemeral connection"
    ~in_fd:client_fd
    ~out_fd:client_fd
    ~close
    ~on_read:handle_ephemeral_request
  >|= (fun (start, conn) ->
    (* On exit, do our best to send all pending messages to the waiting client *)
    let close_on_exit =
      Lwt_condition.wait ExitSignal.signal
      >>= (fun () -> EphemeralConnection.flush_and_close conn)
    in

    (* Lwt.pick returns the first thread to finish and cancels the rest. *)
    Lwt.async (fun () -> Lwt.pick [ close_on_exit; EphemeralConnection.wait_for_closed conn; ]);

    (* Start the ephemeral connection *)
    start ();

    (* Send the current server state immediate *)
    EphemeralConnection.write ~msg:(MonitorProt.Please_hold (StatusStream.get_status ())) conn;

    (* Start sending the status to the ephemeral connection *)
    Lwt.async (fun () -> StatusLoop.run ~cancel_condition:ExitSignal.signal conn);
  )

(* No lock needed, since the socket acceptor runs serially *)
let create_persistent_id =
  let last_persistent_id = ref 0 in
  fun () ->
    incr last_persistent_id;
    !last_persistent_id

let create_persistent_connection ~client_fd ~close ~logging_context ~lsp =
  let client_id = create_persistent_id () in

  Logger.debug "Creating a persistent connection #%d" client_id;

  Server.notify_new_persistent_connection ~client_id ~logging_context ~lsp;

  let close () =
    Server.notify_dead_persistent_connection ~client_id;
    close ()
  in

  PersistentConnection.create
    ~name:(spf "persistent connection #%d" client_id)
    ~in_fd:client_fd
    ~out_fd:client_fd
    ~close
    ~on_read:(handle_persistent_message ~client_id)
  >|= (fun (start, conn) ->
    (* On exit, do our best to send all pending messages to the waiting client *)
    let close_on_exit =
      Lwt_condition.wait ExitSignal.signal
      >>= (fun () -> PersistentConnection.flush_and_close conn)
    in

    (* Lwt.pick returns the first thread to finish and cancels the rest. *)
    Lwt.async (fun () -> Lwt.pick [ close_on_exit; PersistentConnection.wait_for_closed conn; ]);

    (* Don't start the connection until we add it to the persistent connection map *)
    Lwt.async (fun () ->
      PersistentConnectionMap.add ~client_id ~client:conn;
      start ();
      Lwt.return_unit
    )
  )

(* Well...I mean this is a pretty descriptive function name. It performs the handshake and then
 * returns the client type *)
let perform_handshake_and_get_client_type ~client_fd =
  (* Read the build id *)
  Marshal_tools_lwt.from_fd_with_preamble client_fd
  >>= (fun ({SocketHandshake.client_build_id; client_type}) ->
    let is_lsp = match client_type with
      | SocketHandshake.PersistentLsp _ -> true
      | _ -> false in
    (* If the build id doesn't match, send an error and exit
     *
     * Note: glevi hates this behavior. In the future, glevi plans to instead have the client
     * exec the correct binary and the server survives *)
    if client_build_id <> SocketHandshake.build_revision then begin
      let handshake = SocketHandshake.(Build_id_mismatch {
        server_build_id = build_revision;
        server_bin = Sys.executable_name;
      }) in
      Marshal_tools_lwt.to_fd_with_preamble client_fd handshake
      >>= (fun _size ->
        FlowEventLogger.out_of_date ();
        let msg = "Client and server are different builds. Flow server is out of date. Exiting" in
        Logger.fatal "%s" msg;
        FlowExitStatus.exit ~msg FlowExitStatus.Build_id_mismatch
      )
    end else begin
      (* We currently rely on using Unix.select, which doesn't work for fds >= FD_SETSIZE (1024).
       * So we can't have an unlimited number of clients. So if the new fd is too large, let's
       * reject it. Never reject a kill handshake, though. *)

      (* TODO (glevi): Figure out whether this check is needed for Windows *)
      let fd_as_int: int =
        if Sys.unix
        then client_fd |> Lwt_unix.unix_file_descr |> Obj.magic
        else -1
      in
      let too_many_clients = Sys.unix && (
        client_type <> SocketHandshake.StabbityStabStab && fd_as_int > 500
      ) in
      if too_many_clients
      then
        Marshal_tools_lwt.to_fd_with_preamble client_fd SocketHandshake.Too_many_clients
        >|= (fun _ -> failwith (spf "Too many clients, so rejecting new connection (%d)" fd_as_int))
      else if is_lsp && not (StatusStream.ever_been_free ()) then
        let status = StatusStream.get_status () in
        Marshal_tools_lwt.to_fd_with_preamble client_fd (SocketHandshake.Still_initializing status)
        >|= (fun _ -> failwith (ServerStatus.string_of_status status))
      else begin
        match client_type with
        | SocketHandshake.Ephemeral {fail_on_init=true} when not (StatusStream.ever_been_free ()) ->
          Marshal_tools_lwt.to_fd_with_preamble
            client_fd (SocketHandshake.Still_initializing (StatusStream.get_status ()))
          >|= (fun _ ->
            failwith "Client used --retry-on-init false, and the server is still initializing"
          )
        | _ ->
          (* Nothing else has gone wrong with the handshake. Send Ok and return the client_type *)
          Marshal_tools_lwt.to_fd_with_preamble client_fd SocketHandshake.Connection_ok
          >|= (fun _ -> client_type)
      end
    end
  )

let close client_fd () =
  (* Close the client_fd, regardless of whether or not we were able to shutdown the connection.
   * This prevents fd leaks *)
  Logger.debug "Shutting down and closing a socket client fd";
  begin
    (* To be perfectly honest, it's not clear whether the SHUTDOWN_ALL is really needed. I mean,
     * shutdown is useful to shutdown one direction of the socket, but if you're about to close
     * it, does shutting down first actually make any difference? *)
    try Lwt_unix.(shutdown client_fd SHUTDOWN_ALL)
    with
    (* Already closed *)
    | Unix.Unix_error (Unix.EBADF, _, _) -> ()
    | exn -> Logger.error ~exn "Failed to shutdown socket client"
  end;
  Lwt.catch
    (fun () -> Lwt_unix.close client_fd)
    (function
      (* Already closed *)
      | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
      | exn -> Lwt.return (Logger.error ~exn "Failed to close socket client fd"))

let catch close exn =
  (* We catch all exceptions, since one bad connection shouldn't kill the whole monitor *)
  begin match exn with
  (* Monitor is dying *)
  | Lwt.Canceled -> ()
  | Marshal_tools.Malformed_Preamble_Exception ->
    Logger.error
      ~exn
      "Someone tried to connect to the socket, but spoke a different protocol. Ignoring them"
  | exn ->
    Logger.error
      ~exn
      "Exception while trying to establish new connection over the socket. Closing connection"
  end;
  close ()


module type Handler = sig
  val create_socket_connection:
    autostop:bool -> (Lwt_unix.file_descr * Lwt_unix.sockaddr) ->
    unit Lwt.t
end

module SocketAcceptorLoop (Handler : Handler) = LwtLoop.Make (struct
  type acc = bool * Lwt_unix.file_descr

  let main (autostop, socket_fd) =
    Logger.debug "Waiting for a new socket connection";
    Lwt_unix.accept socket_fd
    >>= Handler.create_socket_connection ~autostop
    >|= (fun () -> (autostop, socket_fd))

  let catch _ exn =
    Logger.fatal ~exn "Uncaught exception in the socket acceptor";
    raise exn
end)

module MonitorSocketAcceptorLoop = SocketAcceptorLoop (struct
  let create_socket_connection ~autostop (client_fd, _) =
    let close_without_autostop = close client_fd in
    let close () =
      close_without_autostop () >>= fun () ->
      let num_persistent = PersistentConnectionMap.cardinal () in
      let num_ephemeral = RequestMap.cardinal () in
      if autostop && num_persistent = 0 && num_ephemeral = 0 then
        Server.exit FlowExitStatus.Autostop ~msg:"Autostop"
      else
        Lwt.return_unit
    in
    Lwt.catch
      (fun () ->
        perform_handshake_and_get_client_type ~client_fd
        >>= (
          function
          | SocketHandshake.Ephemeral _ ->
            create_ephemeral_connection ~client_fd ~close
          | SocketHandshake.Persistent logging_context ->
            create_persistent_connection ~client_fd ~close ~logging_context ~lsp:None
          | SocketHandshake.PersistentLsp (logging_context, params) ->
            create_persistent_connection ~client_fd ~close ~logging_context ~lsp:(Some params)
          | SocketHandshake.StabbityStabStab ->
            (* Ow my face *)
            close_without_autostop ()
            >>= (fun () -> Server.exit ~msg:"Killed by `flow stop`. Exiting." FlowExitStatus.No_error)
        )
      )
      (catch close_without_autostop)
      (* Autostop is meant to be "edge-triggered", i.e. when we transition  *)
      (* from 1 connections to 0 connections then it might stop the server. *)
      (* But this catch clause is fired when an attempt to connect has      *)
      (* failed, and that's why it never triggers an autostop.              *)
end)

let run monitor_socket_fd ~autostop =
  MonitorSocketAcceptorLoop.run ~cancel_condition:ExitSignal.signal (autostop, monitor_socket_fd)


module LegacySocketAcceptorLoop = SocketAcceptorLoop (struct
  let create_socket_connection ~autostop:_ (client_fd, _) =
    let close = close client_fd in
    Lwt.catch
      (fun () ->
        close ()
        >>= (fun () ->
          FlowEventLogger.out_of_date ();
          let msg = "Client and server are different builds. Flow server is out of date. Exiting" in
          Logger.fatal "%s" msg;
          Server.exit FlowExitStatus.Build_id_mismatch
            ~msg:"Killed by legacy client. Exiting."
        )
      )
      (catch close)
end)

let run_legacy legacy_socket_fd =
  LegacySocketAcceptorLoop.run ~cancel_condition:ExitSignal.signal (false, legacy_socket_fd)
