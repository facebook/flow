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
  | Server_busy
  | Build_id_mismatch

let server_exists ~tmp_dir root =
  not (Lock.check (FlowConfig.lock_file ~tmp_dir root))

let wait_on_server_restart ic =
  try
    while true do
      let _ = input_char ic in
      ()
    done
  with
  | End_of_file
  | Sys_error _ ->
     (* Server has exited and hung up on us *)
     ()

module SocketKey = struct
  type t = Unix.sockaddr 
  let compare = Pervasives.compare
end
module SockMap = Utils.MyMap(SocketKey)

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

let establish_connection ~tmp_dir root =
  let sock_name = Socket.get_path (FlowConfig.socket_file ~tmp_dir root) in
  let sockaddr =
    if Sys.win32 then
      let ic = open_in_bin sock_name in
      let port = input_binary_int ic in
      close_in ic;
      Unix.(ADDR_INET (inet_addr_loopback, port))
    else
      Unix.ADDR_UNIX sock_name in
  Result.Ok (open_connection sockaddr)

let get_cstate (ic, oc) =
  let cstate : ServerUtils.connection_state = Marshal.from_channel ic in
  Result.Ok (ic, oc, cstate)

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
      close_in_noerr ic;
      Result.Error Build_id_mismatch

(* Connects to the server via a socket. As soon as the server starts up,
 * it opens the socket but it doesn't read or write to it. So during
 * initialization, this function should time out. *)
let connect_once ~tmp_dir root =
  let open Result in
  try
    Sys_utils.with_timeout 1
      ~on_timeout:(fun _ -> raise Exit)
      ~do_:begin fun () ->
        establish_connection ~tmp_dir root >>= fun (ic, oc) ->
        get_cstate (ic, oc)
      end >>= fun (ic, oc, cstate) ->
      verify_cstate ic cstate >>= fun () ->
      Ok (ic, oc)
  with
  | e ->
    if not (server_exists ~tmp_dir root) then Result.Error Server_missing
    else if not (Lock.check (FlowConfig.init_file ~tmp_dir root))
    then Result.Error Server_initializing
    else Result.Error Server_busy
