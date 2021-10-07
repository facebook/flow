(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module listens to the socket, accepts new connections, and starts the
 * FlowServerMonitorConnections which handle those connects *)

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

module type STATUS_WRITER = sig
  type t

  val write : ServerStatus.status * FileWatcherStatus.status -> t -> unit
end

(* A loop that sends the Server's busy status to a waiting connection every 0.5 seconds *)
module StatusLoop (Writer : STATUS_WRITER) = LwtLoop.Make (struct
  type acc = Writer.t

  let main conn =
    let%lwt status = StatusStream.wait_for_signficant_status ~timeout:0.5 in
    Writer.write status conn;
    Lwt.return conn

  let catch _ exn =
    begin
      match Exception.unwrap exn with
      (* The connection closed its write stream, likely it is closed or closing *)
      | Lwt_stream.Closed -> ()
      | _ -> Logger.error ~exn:(Exception.to_exn exn) "StatusLoop threw an exception"
    end;
    Lwt.return_unit
end)

module EphemeralStatusLoop = StatusLoop (struct
  type t = EphemeralConnection.t

  let write status conn = EphemeralConnection.write ~msg:(MonitorProt.Please_hold status) conn
end)

module PersistentStatusLoop = StatusLoop (struct
  type t = PersistentConnection.t

  let write status conn =
    PersistentConnection.write ~msg:LspProt.(NotificationFromServer (Please_hold status)) conn
end)

let create_ephemeral_connection ~client_fd ~close =
  Logger.debug "Creating a new ephemeral connection";

  let%lwt (start, conn) =
    EphemeralConnection.create
      ~name:"some ephemeral connection"
      ~in_fd:client_fd
      ~out_fd:client_fd
      ~close
      ~on_read:handle_ephemeral_request
  in
  (* On exit, do our best to send all pending messages to the waiting client *)
  let close_on_exit =
    let%lwt _ = Lwt_condition.wait ExitSignal.signal in
    EphemeralConnection.flush_and_close conn
  in
  (* Lwt.pick returns the first thread to finish and cancels the rest. *)
  Lwt.async (fun () -> Lwt.pick [close_on_exit; EphemeralConnection.wait_for_closed conn]);

  (* Start the ephemeral connection *)
  start ();

  (* Send the current server state immediate *)
  EphemeralConnection.write ~msg:(MonitorProt.Please_hold (StatusStream.get_status ())) conn;

  (* Start sending the status to the ephemeral connection *)
  Lwt.async (fun () -> EphemeralStatusLoop.run ~cancel_condition:ExitSignal.signal conn);

  Lwt.return_unit

(* No lock needed, since the socket acceptor runs serially *)
let create_persistent_id =
  let last_persistent_id = ref 0 in
  fun () ->
    incr last_persistent_id;
    !last_persistent_id

let create_persistent_connection ~client_fd ~close ~lsp_init_params =
  let client_id = create_persistent_id () in
  Logger.debug "Creating a persistent connection #%d" client_id;

  Server.notify_new_persistent_connection ~client_id ~lsp_init_params;

  let close () =
    Server.notify_dead_persistent_connection ~client_id;
    close ()
  in
  let%lwt (start, conn) =
    PersistentConnection.create
      ~name:(spf "persistent connection #%d" client_id)
      ~in_fd:client_fd
      ~out_fd:client_fd
      ~close
      ~on_read:(handle_persistent_message ~client_id)
  in
  (* On exit, do our best to send all pending messages to the waiting client *)
  let close_on_exit =
    let%lwt _ = Lwt_condition.wait ExitSignal.signal in
    (try PersistentConnection.write LspProt.(NotificationFromServer EOF) conn with
    | Lwt_stream.Closed -> ());
    PersistentConnection.flush_and_close conn
  in
  (* Lwt.pick returns the first thread to finish and cancels the rest. *)
  Lwt.async (fun () -> Lwt.pick [close_on_exit; PersistentConnection.wait_for_closed conn]);

  (* Don't start the connection until we add it to the persistent connection map *)
  Lwt.async (fun () ->
      PersistentConnectionMap.add ~client_id ~client:conn;
      start ();
      PersistentConnection.write
        ~msg:LspProt.(NotificationFromServer (Please_hold (StatusStream.get_status ())))
        conn;
      let%lwt () = PersistentStatusLoop.run ~cancel_condition:ExitSignal.signal conn in
      Lwt.return_unit);

  Lwt.return ()

let close client_fd () =
  (* Close the client_fd, regardless of whether or not we were able to shutdown the connection.
   * This prevents fd leaks *)
  Logger.debug "Shutting down and closing a socket client fd";
  (* To be perfectly honest, it's not clear whether the SHUTDOWN_ALL is really needed. I mean,
   * shutdown is useful to shutdown one direction of the socket, but if you're about to close
   * it, does shutting down first actually make any difference? *)
  begin
    try Lwt_unix.(shutdown client_fd SHUTDOWN_ALL) with
    (* Already closed *)
    | Unix.Unix_error ((Unix.EBADF | Unix.ENOTCONN), _, _) -> ()
    | exn -> Logger.error ~exn "Failed to shutdown socket client"
  end;
  try%lwt Lwt_unix.close client_fd with
  (* Already closed *)
  | Unix.Unix_error (Unix.EBADF, _, _) -> Lwt.return_unit
  | exn -> Lwt.return (Logger.error ~exn "Failed to close socket client fd")

(* Well...I mean this is a pretty descriptive function name. It performs the handshake and then
 * returns the client's side of the handshake *)
let perform_handshake_and_get_client_handshake ~client_fd =
  SocketHandshake.(
    let server_build_id = build_revision in
    let server_bin = Sys.executable_name in
    (* handshake step 1: client sends handshake *)
    let%lwt (wire : client_handshake_wire) = Marshal_tools_lwt.from_fd_with_preamble client_fd in
    let client_handshake =
      try fst wire |> Hh_json.json_of_string |> json_to__client_to_monitor_1 with
      | exn ->
        Logger.error ~exn "Failed to parse JSON section of handshake: %s" (fst wire);
        default_client_to_monitor_1
    in
    let client =
      if client_handshake.client_build_id <> server_build_id then
        None
      else
        Some (Marshal.from_string (snd wire) 0 : client_to_monitor_2)
    in
    (* handshake step 2: server sends back handshake *)
    let respond server_intent server2 =
      assert (server2 = None || client_handshake.client_build_id = server_build_id);

      (* the client will trust our invariant that server2=Some means the client *)
      (* can certainly deserialize server2. *)
      let server_version = Flow_version.version in
      let server1 = { server_build_id; server_bin; server_intent; server_version } in
      let wire : server_handshake_wire =
        ( server1 |> monitor_to_client_1__to_json |> Hh_json.json_to_string,
          Base.Option.map server2 ~f:(fun server2 -> Marshal.to_string server2 []) )
      in
      let%lwt _ = Marshal_tools_lwt.to_fd_with_preamble client_fd wire in
      Lwt.return_unit
    in
    let error_client () =
      let%lwt () = respond Server_will_hangup None in
      failwith "Build mismatch, so rejecting attempted connection"
    in
    let stop_server () =
      let%lwt () = respond Server_will_exit None in
      let msg = "Client and server are different builds. Flow server is out of date. Exiting" in
      FlowEventLogger.out_of_date ();
      Logger.fatal "%s" msg;
      Exit.exit ~msg Exit.Build_id_mismatch
    in
    let fd_as_int = client_fd |> Lwt_unix.unix_file_descr |> Obj.magic in
    (* Stop request *)
    if client_handshake.is_stop_request then
      let%lwt () = respond Server_will_exit None in
      let%lwt () = close client_fd () in
      Server.stop Server.Stopped
      (* Binary version mismatch *)
    else if client_handshake.client_build_id <> build_revision then
      match client_handshake.version_mismatch_strategy with
      | Always_stop_server -> stop_server ()
      | Stop_server_if_older ->
        if Semver.compare Flow_version.version client_handshake.client_version < 0 then
          stop_server ()
        (* server < client *)
        else
          error_client ()
      | Error_client -> error_client ()
    (* Too many clients *)
    else if Sys.unix && fd_as_int > 500 then
      (* We currently rely on using Unix.select, which doesn't work for fds >= FD_SETSIZE (1024).
       * So we can't have an unlimited number of clients. So if the new fd is too large, let's
       * reject it.
       * TODO(glevi): Figure out whether this check is needed for Windows *)
      let%lwt () = respond Server_will_hangup (Some Server_has_too_many_clients) in
      failwith (spf "Too many clients, so rejecting new connection (%d)" fd_as_int)
      (* Server still initializing *)
    else if not (StatusStream.ever_been_free ()) then
      let client = Base.Option.value_exn client in
      let status = StatusStream.get_status () in
      if client_handshake.server_should_hangup_if_still_initializing then (
        let%lwt () = respond Server_will_hangup (Some (Server_still_initializing status)) in
        (* In the case of Persistent, lspCommand will retry a second later. *)
        (* The message we log here solely goes to the logs, not the user. *)
        let (server_status, watchman_status) = status in
        Logger.info
          "Server still initializing -> hangup. server_status=%s watchman_status=%s"
          (ServerStatus.string_of_status server_status)
          (FileWatcherStatus.string_of_status watchman_status);
        Lwt.return None
      ) else
        let%lwt () = respond Server_will_continue (Some (Server_still_initializing status)) in
        Lwt.return (Some client)
    (* Success *)
    else
      let client = Base.Option.value_exn client in
      let%lwt () = respond Server_will_continue (Some Server_ready) in
      Lwt.return (Some client))

let catch close exn =
  (* We catch all exceptions, since one bad connection shouldn't kill the whole monitor *)
  begin
    match exn with
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
  val create_socket_connection :
    autostop:bool -> Lwt_unix.file_descr * Lwt_unix.sockaddr -> unit Lwt.t

  val name : string
end

module SocketAcceptorLoop (Handler : Handler) = LwtLoop.Make (struct
  type acc = bool * Lwt_unix.file_descr

  let main (autostop, socket_fd) =
    Logger.debug "Waiting for a new %s" Handler.name;
    let%lwt conn = Lwt_unix.accept socket_fd in
    let%lwt () = Handler.create_socket_connection ~autostop conn in
    Lwt.return (autostop, socket_fd)

  let catch _ exn =
    Logger.fatal ~exn:(Exception.to_exn exn) "Uncaught exception in the socket acceptor";
    Exception.reraise exn
end)

module Autostop : sig
  val cancel_countdown : unit -> unit

  val start_countdown : unit -> unit
end = struct
  let current_countdown = ref Lwt.return_unit

  let cancel_countdown () = Lwt.cancel !current_countdown

  let start_countdown () =
    current_countdown :=
      let%lwt () = Lwt_unix.sleep 60. in
      Server.stop Server.Autostopped
end

module MonitorSocketAcceptorLoop = SocketAcceptorLoop (struct
  let name = "socket connection"

  let create_socket_connection ~autostop (client_fd, _) =
    let close_without_autostop = close client_fd in
    let close () =
      let%lwt () = close_without_autostop () in
      let num_persistent = PersistentConnectionMap.cardinal () in
      let num_ephemeral = RequestMap.cardinal () in
      if autostop && num_persistent = 0 && num_ephemeral = 0 then
        Lwt.return (Autostop.start_countdown ())
      else
        Lwt.return_unit
    in
    try%lwt
      let%lwt client = perform_handshake_and_get_client_handshake ~client_fd in
      Autostop.cancel_countdown ();
      SocketHandshake.(
        match client with
        | Some { client_type = Ephemeral; _ } -> create_ephemeral_connection ~client_fd ~close
        | Some { client_type = Persistent { lsp_init_params }; _ } ->
          create_persistent_connection ~client_fd ~close ~lsp_init_params
        | None -> close_without_autostop ())
    with
    | exn -> catch close_without_autostop exn

  (* Autostop is meant to be "edge-triggered", i.e. when we transition  *)
  (* from 1 connections to 0 connections then it might stop the server. *)
  (* But this catch clause is fired when an attempt to connect has      *)
  (* failed, and that's why it never triggers an autostop.              *)
end)

let run monitor_socket_fd ~autostop =
  MonitorSocketAcceptorLoop.run ~cancel_condition:ExitSignal.signal (autostop, monitor_socket_fd)

module LegacySocketAcceptorLoop = SocketAcceptorLoop (struct
  let name = "legacy socket connection"

  let create_socket_connection ~autostop:_ (client_fd, _) =
    let close = close client_fd in
    try%lwt
      let%lwt () = close () in
      FlowEventLogger.out_of_date ();
      let msg = "Client and server are different builds. Flow server is out of date. Exiting" in
      Logger.fatal "%s" msg;
      Server.stop Server.Legacy_client
    with
    | exn -> catch close exn
end)

let run_legacy legacy_socket_fd =
  LegacySocketAcceptorLoop.run ~cancel_condition:ExitSignal.signal (false, legacy_socket_fd)
