(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type error =
  | Server_missing
  | Server_initializing
  | Server_rechecking
  | Server_busy
  | Build_id_mismatch

exception ConnectTimeout

let server_exists ~tmp_dir root =
  not (Lock.check (FlowConfig.lock_file ~tmp_dir root))

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

module SockMap = Utils.MyMap(struct
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
let open_connection sockaddr =
  match SockMap.get sockaddr !connections with
  | Some conn -> conn
  | None ->
      let conn = Unix.open_connection sockaddr in
      connections := SockMap.add sockaddr conn !connections;
      (* It's important that we only write this once per connection *)
      Printf.fprintf (snd conn) "%s\n%!" Build_id.build_id_ohai;
      conn

let close_connection sockaddr =
  match SockMap.get sockaddr !connections with
  | None -> ()
  | Some (ic, _) ->
      connections := SockMap.remove sockaddr !connections;
      Unix.shutdown_connection ic;
      close_in_noerr ic

let establish_connection ~timeout ~tmp_dir root =
  let sock_name = Socket.get_path (FlowConfig.socket_file ~tmp_dir root) in
  let sockaddr =
    if Sys.win32 then
      let ic = open_in_bin sock_name in
      let port = input_binary_int ic in
      close_in ic;
      Unix.(ADDR_INET (inet_addr_loopback, port))
    else
      Unix.ADDR_UNIX sock_name in
  Result.Ok (sockaddr, Timeout.open_connection ~timeout sockaddr)

let get_cstate timeout sockaddr (ic, oc) =
  try
    Printf.fprintf oc "%s\n%!" Build_id.build_id_ohai;
    let cstate : ServerUtils.connection_state =
      Timeout.input_value ~timeout ic in
    Result.Ok (ic, oc, cstate)
  with
  | ConnectTimeout as e ->
      (* Timeouts are expected *)
      raise e
  | e ->
      (* Other exceptions may indicate a bad connection, so let's close it *)
      close_connection sockaddr;
      Timeout.shutdown_connection ic;
      Timeout.close_in ic;
      raise e

let verify_cstate ic = function
  | ServerUtils.Connection_ok -> Result.Ok ()
  | ServerUtils.Build_id_mismatch ->
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
      Timeout.close_in ic;
      Result.Error Build_id_mismatch

(* Connects to the server via a socket. As soon as the server starts up,
 * it opens the socket but it doesn't read or write to it. So during
 * initialization, this function should time out. *)
let connect_once ~tmp_dir root =
  let open Result in
  try
    Timeout.with_timeout ~timeout:1
      ~on_timeout:(fun _ -> raise ConnectTimeout)
      ~do_:begin fun t ->
        establish_connection ~timeout:t ~tmp_dir root >>= fun (sockaddr, (ic, oc)) ->
        get_cstate t sockaddr (ic, oc)
      end >>= fun (ic, oc, cstate) ->
      verify_cstate ic cstate >>= fun () ->
      Ok (ic, oc)
  with
  | e ->
    if not (server_exists ~tmp_dir root) then Result.Error Server_missing
    else if not (Lock.check (FlowConfig.init_file ~tmp_dir root))
    then Result.Error Server_initializing
    else if not (Lock.check (FlowConfig.recheck_file ~tmp_dir root))
    then Result.Error Server_rechecking
    else Result.Error Server_busy
