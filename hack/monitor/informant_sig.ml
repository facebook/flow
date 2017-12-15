(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


type report =
  (** Nothing to see here. *)
  | Move_along
  (** Kill the server (if one is running) and start a new one. *)
  | Restart_server of ServerMonitorUtils.target_mini_state option

type server_state =
  | Server_not_yet_started
  | Server_alive
  | Server_dead

(** The informant collects information to tell the monitor when to
 * intelligently kill and restart the server daemon.
 *
 * For example: An informant may want to watch the repo state and tell
 * the monitor to restart the server when a significant change in
 * repo revision occurs since a fresh initialization could be faster
 * than an incremental type check. *)
module type S = sig
  type t
  type init_env
  val init : init_env -> t
  val report : t -> server_state -> report
  (**
   * Returns true if the informant is actually running and will
   * manage server lifetime.
   *)
  val is_managing : t -> bool
  val should_start_first_server : t -> bool
end
