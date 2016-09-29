(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type env
type dead_env

exception Timeout

type watchman_instance =
  | Watchman_dead of dead_env
  | Watchman_alive of env

type init_settings = {
  subscribe_to_changes: bool;
  init_timeout: int;
  root: Path.t;
}

type 'a changes =
  | Watchman_unavailable
  | Watchman_pushed of 'a
  | Watchman_synchronous of 'a

val crash_marker_path: Path.t -> string

val init: init_settings -> env option

val get_all_files: env -> string list

val get_changes: ?deadline:float ->
  watchman_instance -> watchman_instance * SSet.t changes
val get_changes_synchronously: timeout:int ->
  watchman_instance -> watchman_instance * SSet.t
