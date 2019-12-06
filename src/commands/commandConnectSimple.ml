(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Server_files = Server_files_js

type busy_reason =
  | Too_many_clients
  | Not_responding
  | Fail_on_init of (ServerStatus.status * FileWatcherStatus.status)

type mismatch_behavior =
  (* The server exited due to the build id mismatch *)
  | Server_exited
  (* The server is still alive but the client should error *)
  | Client_should_error of {
      server_bin: string;
      server_version: string;
    }

type error =
  | Build_id_mismatch of mismatch_behavior
  | Server_busy of busy_reason
  | Server_missing
  | Server_socket_missing

(* pre-server-monitor versions used a different socket *)

type connect_exn =
  | Timeout
  | Missing_socket

exception ConnectError of connect_exn

let server_exists ~flowconfig_name ~tmp_dir root =
  not (Lock.check (Server_files.lock_file ~flowconfig_name ~tmp_dir root))

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

module SockMap = WrappedMap.Make (struct
  type t = Unix.sockaddr

  let compare = Pervasives.compare
end)

(* We used to open a new connection every time we tried to connect to a socket
 * and then shut it down if the connection failed or timed out. However on OSX
 * the shutdown wouldn't really do anything and we were hitting the pending
 * connection limit set by Unix.listen. So instead we can hang on to the
 * connection, since there's nothing wrong with it.
 *)
let connections = ref SockMap.empty

let open_connection ~timeout ~client_handshake sockaddr =
  match SockMap.find_opt sockaddr !connections with
  | Some conn -> conn
  | None ->
    let conn =
      try Timeout.open_connection ~timeout sockaddr
      with Unix.Unix_error (Unix.ENOENT, "connect", _) -> raise (ConnectError Missing_socket)
    in
    connections := SockMap.add sockaddr conn !connections;

    (* It's important that we only write this once per connection *)
    let fd = Unix.descr_of_out_channel (snd conn) in
    SocketHandshake.(
      let wire : client_handshake_wire =
        ( fst client_handshake |> client_to_monitor_1__to_json |> Hh_json.json_to_string,
          Marshal.to_string (snd client_handshake) [] )
      in
      Marshal_tools.to_fd_with_preamble fd wire |> ignore;
      conn)

let close_connection sockaddr =
  match SockMap.find_opt sockaddr !connections with
  | None -> ()
  | Some (ic, _) ->
    connections := SockMap.remove sockaddr !connections;
    Timeout.shutdown_connection ic;
    Timeout.close_in_noerr ic

let establish_connection ~flowconfig_name ~timeout ~client_handshake ~tmp_dir root =
  let sock_name = Socket.get_path (Server_files.socket_file ~flowconfig_name ~tmp_dir root) in
  let sockaddr =
    if Sys.win32 then (
      let ic = open_in_bin sock_name in
      let port = input_binary_int ic in
      close_in ic;
      Unix.(ADDR_INET (inet_addr_loopback, port))
    ) else
      Unix.ADDR_UNIX sock_name
  in
  Ok (sockaddr, open_connection ~timeout ~client_handshake sockaddr)

let get_handshake ~timeout:_ sockaddr ic oc =
  SocketHandshake.(
    (* TODO (glevi) - If we want this read to timeout on Windows, we need to make Marshal_tools
     * respect Timeout. That said, this is a lower priority fix, since we rarely run into
     * trouble right here. *)
    try
      let fd = Timeout.descr_of_in_channel ic in
      let wire = (Marshal_tools.from_fd_with_preamble fd : server_handshake_wire) in
      let server_handshake =
        ( fst wire |> Hh_json.json_of_string |> json_to__monitor_to_client_1,
          snd wire |> Option.map ~f:(fun s -> (Marshal.from_string s 0 : monitor_to_client_2)) )
        (* Server invariant: it only sends us snd=Some if it knows client+server versions match *)
      in
      Ok (sockaddr, ic, oc, server_handshake)
    with
    | ConnectError Timeout as e ->
      (* Timeouts are expected *)
      raise e
    | e ->
      (* Other exceptions may indicate a bad connection, so let's close it *)
      close_connection sockaddr;
      raise e)

let verify_handshake ~client_handshake ~server_handshake sockaddr ic =
  SocketHandshake.(
    let (client1, _client2) = client_handshake in
    let (server1, server2) = server_handshake in
    (* First, let's close the connection as needed *)
    begin
      match server1 with
      | { server_intent = Server_will_continue; _ } -> ()
      | { server_intent = Server_will_hangup; _ } -> close_connection sockaddr
      | { server_intent = Server_will_exit; _ } ->
        (* If the server will exit shortly, we wouldn't want subsequent connection
         * attempts on the Unix Domain Socket to succeed (only to be doomed to failure).
         * To avoid that fate, we'll wait for the connection to be closed. *)
        wait_on_server_restart ic;
        Timeout.close_in_noerr ic
    end;

    (* Next, let's interpret the server's response into our own response code *)
    match (server1, server2) with
    | ({ server_intent = Server_will_continue; _ }, Some Server_ready) -> Ok ()
    | ({ server_intent = Server_will_continue; _ }, Some (Server_still_initializing _)) -> Ok ()
    | ({ server_intent = Server_will_hangup; _ }, Some Server_has_too_many_clients) ->
      Error (Server_busy Too_many_clients)
    | ({ server_intent = Server_will_hangup; _ }, Some (Server_still_initializing status)) ->
      Error (Server_busy (Fail_on_init status))
    | ({ server_intent = Server_will_hangup; server_bin; server_version; _ }, None) ->
      if client1.client_build_id <> server1.server_build_id then
        Error (Build_id_mismatch (Client_should_error { server_bin; server_version }))
      else
        failwith "Don't know why server closed the connection"
    | ({ server_intent = Server_will_exit; _ }, None) ->
      if client1.is_stop_request then
        Ok ()
      else
        (* either the build ids were different, or client1 wasn't valid for server *)
        Error (Build_id_mismatch Server_exited)
    | _ -> failwith "Monitor sent incorrect handshake")

(* Connects to the monitor via a socket. *)
let connect_once ~flowconfig_name ~client_handshake ~tmp_dir root =
  let ( >>= ) = Base.Result.( >>= ) in
  try
    Timeout.with_timeout
      ~timeout:1
      ~on_timeout:(fun _ -> raise (ConnectError Timeout))
      ~do_:
        begin
          fun timeout ->
          establish_connection ~flowconfig_name ~timeout ~client_handshake ~tmp_dir root
          >>= fun (sockaddr, (ic, oc)) -> get_handshake ~timeout sockaddr ic oc
        end
    >>= fun (sockaddr, ic, oc, server_handshake) ->
    verify_handshake ~client_handshake ~server_handshake sockaddr ic >>= fun () -> Ok (ic, oc)
  with
  | ConnectError Missing_socket ->
    if server_exists ~flowconfig_name ~tmp_dir root then
      Error Server_socket_missing
    else
      Error Server_missing
  | ConnectError Timeout
  | _ ->
    if server_exists ~flowconfig_name ~tmp_dir root then
      Error (Server_busy Not_responding)
    else
      Error Server_missing

let busy_reason_to_string (busy_reason : busy_reason) : string =
  match busy_reason with
  | Too_many_clients -> "Too_many_clients"
  | Not_responding -> "Not_responding"
  | Fail_on_init (server_status, watcher_status) ->
    "Fail_on_init("
    ^ "server_status="
    ^ ServerStatus.string_of_status server_status
    ^ ","
    ^ "watcher_status="
    ^ FileWatcherStatus.string_of_status watcher_status
    ^ ")"

let error_to_string (error : error) : string =
  match error with
  | Build_id_mismatch Server_exited -> "Build_id_mismatch(Server_exited)"
  | Build_id_mismatch (Client_should_error _) -> "Build_id_mismatch(Client_should_error)"
  | Server_busy busy_reason -> "Server_busy(" ^ busy_reason_to_string busy_reason ^ ")"
  | Server_missing -> "Server_missing"
  | Server_socket_missing -> "Server_socket_missing"
