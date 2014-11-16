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

(* Function connecting to hh_server *)
let connect ?user:(user=None) root =
  try
    let sock_name = Socket.get_path ~user root in
    let sockaddr = Unix.ADDR_UNIX sock_name in
    let domain = Unix.domain_of_sockaddr sockaddr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
    Unix.connect sock sockaddr ;
    let ic = Unix.in_channel_of_descr sock in
    let oc = Unix.out_channel_of_descr sock in
    ic, oc
  with _ ->
    if not (Lock.check ~user root "init")
    then raise ClientExceptions.Server_initializing
    else raise ClientExceptions.Server_cant_connect

let server_exists root = not (Lock.check root "lock")
