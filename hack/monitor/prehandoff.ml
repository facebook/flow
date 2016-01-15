(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type exit_status = {
  status : Unix.process_status;
  was_oom: bool;
}

type msg =
  (** Last of the prehandoff messages. *)
  | Sentinel
  (** Client has connected to a monitor whose typechecker exited normally.
   * This is rare - happens when client establishes connection to a monitor
   * that hasn't discovered its typechecker exited by an RPC Kill command. *)
  | Shutting_down
  (* Client sent a malformed request asking for a server that doesn't exist *)
  | Server_name_not_found
  (** Server process died. Connect another client to start another one. *)
  | Server_died of exit_status
