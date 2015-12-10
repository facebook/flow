(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

type env

val crash_marker_path: Path.t -> string

val init: int -> Path.t -> env option

val get_all_files: env -> string list

val get_changes: env -> SSet.t
