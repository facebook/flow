(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type monitor_config =
  {
    (** The socket file on which the monitor is listening for connections. *)
    socket_file: string;
    (** This lock is held when a monitor is alive. *)
    lock_file: string;
  }

type connection_error =
  | Server_missing
  | Server_busy
  | Build_id_mismatched

type connection_state =
  | Connection_ok
  | Build_id_mismatch

exception Server_shutting_down
exception Last_server_died
