(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let read_stdin_to_string () =
  let buf = Buffer.create 4096 in
  try
    while true do
      Buffer.add_string buf (input_line stdin);
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file ->
    Buffer.contents buf

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
     Printf.printf "Old server has exited\n%!";
     ()

(* Function connecting to hh_server *)
let connect root =
  if not (server_exists root)
  then raise ClientExceptions.Server_missing;
  let ic, oc, cstate =
    try
      let sock_name = Socket.get_path root in
      let sockaddr = Unix.ADDR_UNIX sock_name in
      let ic, oc = Unix.open_connection sockaddr in
      try
        Printf.fprintf oc "%s\n%!" Build_id.build_id_ohai;
        let cstate : ServerUtils.connection_state = Marshal.from_channel ic in
        ic, oc, cstate
      with e ->
        Unix.shutdown_connection ic;
        close_in_noerr ic;
        raise e
    with _ ->
      if not (Lock.check root "init")
      then raise ClientExceptions.Server_initializing
      else raise ClientExceptions.Server_cant_connect
  in
  let () = match cstate with
    | ServerUtils.Connection_ok -> ()
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
        raise ClientExceptions.Server_out_of_date in
  ic, oc
