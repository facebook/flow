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

(**
 * Function that initializes the common state and returns a list of individual
 * processes starters.
 *)
type monitor_starter =
   (unit -> (ServerProcess.process_data list))

type connection_error =
  | Server_missing
  | Server_busy
  | Server_died
  | Build_id_mismatched
  | Monitor_connection_failure

type connection_state =
  | Connection_ok
  | Build_id_mismatch

(** Result of a shutdown monitor RPC. *)
type shutdown_result =
  (** Request sent and channel hung up, indicating the process has exited. *)
  | SHUTDOWN_VERIFIED
  (** Request sent, but channel hasn't hung up. *)
  | SHUTDOWN_UNVERIFIED

exception Server_shutting_down
exception Last_server_died

let exit_if_parent_dead () =
(** Cross-platform compatible way; parent PID becomes 1 when parent dies. *)
  if Unix.getppid() = 1 then
    (Hh_logger.log "Server's parent has died; exiting.\n";
     Exit_status.exit Exit_status.Lost_parent_monitor);
