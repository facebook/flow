(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type watchman_instance

type init_settings = {
  subscribe_to_changes: bool;
  init_timeout: int;
  root: Path.t;
}

val crash_marker_path: Path.t -> string

val init: init_settings -> watchman_instance option

val get_all_files: watchman_instance -> watchman_instance * string list

val get_changes: watchman_instance -> watchman_instance * SSet.t
