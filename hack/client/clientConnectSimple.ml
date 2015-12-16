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

let server_exists root = not (Lock.check
  (ServerFiles.lock_file root))

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

let send_build_id_ohai oc =
  Marshal_tools.to_fd_with_preamble (Unix.descr_of_out_channel oc)
    Build_id.build_id_ohai;
  (** For backwards-compatibility, newline has always followed build id ohai *)
  let _ = Unix.write (Unix.descr_of_out_channel oc) "\n" 0 1 in
  ()


let establish_connection timeout root =
  let sock_name = Socket.get_path (ServerFiles.socket_file root) in
  let sockaddr =
    if Sys.win32 then
      let ic = open_in_bin sock_name in
      let port = input_binary_int ic in
      close_in ic;
      Unix.(ADDR_INET (inet_addr_loopback, port))
    else
      Unix.ADDR_UNIX sock_name in
  Result.Ok (Timeout.open_connection ~timeout sockaddr)

let get_cstate timeout (ic, oc) =
  try
    send_build_id_ohai oc;
    let cstate : ServerUtils.connection_state = Timeout.input_value ic in
    Result.Ok (ic, oc, cstate)
  with e ->
    Timeout.shutdown_connection ic;
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
       *
       * See also: ServerMonitor.client_out_of_date
       *)
      wait_on_server_restart ic;
      Timeout.close_in ic;
      Result.Error Build_id_mismatch

let connect_once root =
  let open Result in
  try
    Timeout.with_timeout
    ~timeout:1
    ~on_timeout:(fun () -> raise Exit)
    ~do_:begin fun timeout ->
      establish_connection timeout root >>= fun (ic, oc) ->
      get_cstate timeout (ic, oc)
    end >>= fun (ic, oc, cstate) ->
    verify_cstate ic cstate >>= fun () ->
    Ok (ic, oc)
  with
  | Exit_status.Exit_with _  as e -> raise e
  | _ ->
    if not (server_exists root) then Result.Error Server_missing
    else if not (Lock.check (ServerFiles.init_complete_file root))
    then Result.Error Server_busy
    else Result.Error Server_initializing
