(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

val make_next_files:
  (string -> bool) -> ?others: Path.t list -> Path.t ->
  (unit -> string list)

val find_with_name: Path.t list -> string -> string list
