(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Server_files = Server_files_js

type busy_reason =
| Too_many_clients
| Not_responding
| Fail_on_init

type error =
  | Build_id_mismatch
  | Server_busy of busy_reason
  | Server_missing
  | Server_socket_missing (* pre-server-monitor versions used a different socket *)

type connect_exn =
  | Timeout
  | Missing_socket
exception ConnectError of connect_exn

let server_exists ~tmp_dir root =
  not (Lock.check (Server_files.lock_file ~tmp_dir root))

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

module SockMap = MyMap.Make (struct
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
let open_connection ~timeout ~client_type sockaddr =
  match SockMap.get sockaddr !connections with
  | Some conn -> conn
  | None ->
      let conn =
        try Timeout.open_connection ~timeout sockaddr
        with Unix.Unix_error (Unix.ENOENT, "connect", _) ->
          raise (ConnectError Missing_socket)
      in
      connections := SockMap.add sockaddr conn !connections;
      (* It's important that we only write this once per connection *)
      let fd = Unix.descr_of_out_channel (snd conn) in
      let handshake = SocketHandshake.({ client_build_id = build_revision; client_type } )in
      Marshal_tools.to_fd_with_preamble fd handshake |> ignore;
      conn

let close_connection sockaddr =
  match SockMap.get sockaddr !connections with
  | None -> ()
  | Some (ic, _) ->
      connections := SockMap.remove sockaddr !connections;
      Timeout.shutdown_connection ic;
      Timeout.close_in_noerr ic

let establish_connection ~timeout ~client_type ~tmp_dir root =
  let sock_name = Socket.get_path (Server_files.socket_file ~tmp_dir root) in
  let sockaddr =
    if Sys.win32 then
      let ic = open_in_bin sock_name in
      let port = input_binary_int ic in
      close_in ic;
      Unix.(ADDR_INET (inet_addr_loopback, port))
    else
      Unix.ADDR_UNIX sock_name in
  Ok (sockaddr, open_connection ~timeout ~client_type sockaddr)

let get_handshake ~timeout:_ sockaddr ic oc =
  try
    (* TODO (glevi) - If we want this read to timeout on Windows, we need to make Marshal_tools
     * respect Timeout. That said, this is a lower priority fix, since we rarely run into
     * trouble right here. *)
    let handshake : SocketHandshake.monitor_to_client =
      Marshal_tools.from_fd_with_preamble (Timeout.descr_of_in_channel ic) in
    Ok (sockaddr, ic, oc, handshake)
  with
  | ConnectError Timeout as e ->
      (* Timeouts are expected *)
      raise e
  | e ->
      (* Other exceptions may indicate a bad connection, so let's close it *)
      close_connection sockaddr;
      raise e

let verify_handshake sockaddr ic = function
  | SocketHandshake.Connection_ok -> Ok ()
  | SocketHandshake.Still_initializing _ -> Error (Server_busy Fail_on_init)
  | SocketHandshake.Build_id_mismatch _ ->
      (* TODO (glevi) - I want to change this behavior. I want the server to survive and the client
       * to exec a new client. *)
      (* The server is out of date and is going to exit. Subsequent calls
       * to connect on the Unix Domain Socket might succeed, connecting to
       * the server that is about to die, and eventually we will be hung
       * up on while trying to read from our end.
       *
       * To avoid that fate, when we know the server is about to exit, we
       * wait for the connection to be closed, signaling that the server
       * has exited and the OS has cleaned up after it, then we try again.
       *)
      wait_on_server_restart ic;
      Timeout.close_in_noerr ic;
      Error Build_id_mismatch
  | SocketHandshake.Too_many_clients ->
    close_connection sockaddr;
    Error (Server_busy Too_many_clients)

(* Connects to the server via a socket. As soon as the server starts up,
 * it opens the socket but it doesn't read or write to it. So during
 * initialization, this function should time out. *)
let connect_once ~client_type ~tmp_dir root =
  let (>>=) = Core_result.(>>=) in
  try
    Timeout.with_timeout
      ~timeout:1
      ~on_timeout:(fun _ -> raise (ConnectError Timeout))
      ~do_:begin fun timeout ->
        establish_connection ~timeout ~client_type ~tmp_dir root >>= fun (sockaddr, (ic, oc)) ->
        get_handshake ~timeout sockaddr ic oc
      end >>= fun (sockaddr, ic, oc, cstate) ->
      verify_handshake sockaddr ic cstate >>= fun () ->
      Ok (ic, oc)
  with
  | ConnectError Missing_socket ->
    if server_exists ~tmp_dir root
    then Error Server_socket_missing
    else Error Server_missing
  | ConnectError Timeout
  | _ ->
    if server_exists ~tmp_dir root
    then Error (Server_busy Not_responding)
    else Error Server_missing

let busy_reason_to_string (busy_reason: busy_reason) : string =
  match busy_reason with
  | Too_many_clients -> "Too_many_clients"
  | Not_responding -> "Not_responding"
  | Fail_on_init -> "Fail_on_init"

let error_to_string (error: error) : string =
  match error with
  | Build_id_mismatch -> "Build_id_mismatch"
  | Server_busy busy_reason -> "Server_busy(" ^ (busy_reason_to_string busy_reason) ^ ")"
  | Server_missing -> "Server_missing"
  | Server_socket_missing -> "Server_socket_missing"
