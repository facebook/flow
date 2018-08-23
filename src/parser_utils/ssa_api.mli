(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type read_loc = Loc.t
type write_loc =
  | Write of Loc.t
  | Uninitialized
type write_locs = write_loc list
type values = write_locs LocMap.t

val uninitialized: write_loc
val write_locs_of_read_loc: values -> read_loc -> write_locs
val is_dead_write_loc: values -> Loc.t -> bool
val print_values: values -> string
