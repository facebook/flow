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

let server_exists root = not (Lock.check root "lock")

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

let establish_connection root =
  let sock_name = Socket.get_path root in
  let sockaddr = Unix.ADDR_UNIX sock_name in
  Result.Ok (Unix.open_connection sockaddr)

let get_cstate (ic, oc) =
  try
    Printf.fprintf oc "%s\n%!" Build_id.build_id_ohai;
    let cstate : ServerUtils.connection_state = Marshal.from_channel ic in
    Result.Ok cstate
  with e ->
    Unix.shutdown_connection ic;
    close_in_noerr ic;
    Result.Error Server_busy

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

let connect_once root =
  let open Result in
  try
    Sys_utils.with_timeout 1
      ~on_timeout:(fun _ -> raise Exit)
      ~do_:begin fun () ->
        ok_if_true (server_exists root) ~error:Server_missing >>= fun () ->
        establish_connection root >>= fun (ic, oc) ->
        get_cstate (ic, oc) >>= verify_cstate ic >>= fun () ->
        Ok (ic, oc)
      end
  with _ ->
    if not (Lock.check root "init")
    then Result.Error Server_initializing
    else Result.Error Server_busy
