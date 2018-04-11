(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type handoff_options = {
  (** The name of the server to connect to. *)
  server_name : string;
  (** If server is dormant because it is waiting for Informant to start one,
   * set this to true to start a server anyway. *)
  force_dormant_start : bool;
}

type command =
  | HANDOFF_TO_SERVER of handoff_options
  (** Shut down all servers and then the monitor. *)
  | SHUT_DOWN
