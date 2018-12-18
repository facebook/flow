(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type handoff_options = {
  (** If server is dormant because it is waiting for Informant to start one,
   * set this to true to start a server anyway. *)
  force_dormant_start : bool;
  (* There can be multiple named channels between server and monitor in order
   * to prioritize some requests over others. Connecting code needs to specify
   * which channel it wants to use. *)
  pipe_name : string;
}

type command =
  | HANDOFF_TO_SERVER of handoff_options
  (** Shut down all servers and then the monitor. *)
  | SHUT_DOWN

type server_to_monitor_message =
  | PROGRESS of string option
  | PROGRESS_WARNING of string option
