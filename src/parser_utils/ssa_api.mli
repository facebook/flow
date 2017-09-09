(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module LocMap = Utils_js.LocMap

type read_loc = Loc.t
type write_loc = Loc.t
type write_locs = Loc.t list
type values = write_locs LocMap.t

val uninitialized: Loc.t (* TODO: replace this with something more robust *)
val write_locs_of_read_loc: values -> read_loc -> write_locs
val is_dead_write_loc: values -> write_loc -> bool
val print_values: values -> string
