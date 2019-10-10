(*
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

exception Receiving_Fd_Exception

(** Returns 0 for success, -1 on failure. *)
val ancil_send_fd :
  Unix.file_descr (** The fd of the socket to send the payload over *) ->
  Unix.file_descr (** The file descriptor you want to send *) ->
  int

(** The fd received *)
val ancil_recv_fd :
  Unix.file_descr (** The fd of the socket to receive the payload over *) ->
  Unix.file_descr
