(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

let int_to_fd (i : int) : Unix.file_descr = Obj.magic i

exception Receiving_Fd_Exception

external ancil_send_fd :
  Unix.file_descr (* The fd of the socket to send the payload over *) ->
  Unix.file_descr (* The file descriptor you want to send *) ->
  int (* Returns 0 for success, -1 on failure. *) = "stub_ancil_send_fd"

external ancil_recv_fd_ :
  Unix.file_descr (* The fd of the socket to receive the payload over *) ->
  int (* The fd received *) = "stub_ancil_recv_fd"

(** Receives a file descriptor from socket_fd. Throws exception on error. *)
let ancil_recv_fd socket_fd =
  let result = ancil_recv_fd_ socket_fd in
  if result = -1 then
    raise Receiving_Fd_Exception
  else
    int_to_fd result
