(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

external ancil_send_fd :
  int (** The fd of the socket to send the payload over *) ->
  int (** The file descriptor you want to send *) ->
  int (** Returns 0 for success, -1 on failure. *) = "stub_ancil_send_fd"

external ancil_recv_fd :
  int (** The fd of the socket to receive the payload over *) ->
  int (** The fd received *) = "stub_ancil_recv_fd"
