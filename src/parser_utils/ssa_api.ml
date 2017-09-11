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
type write_loc =
  | Write of Loc.t
  | Uninitialized
type write_locs = write_loc list
type values = write_locs LocMap.t

let uninitialized = Uninitialized

let write_locs_of_read_loc values read_loc =
  LocMap.find read_loc values

let is_dead_write_loc values loc =
  not (LocMap.exists (fun _read_loc write_locs -> List.mem (Write loc) write_locs) values)

let print_write_loc write_loc =
  match write_loc with
    | Uninitialized -> "(uninitialized)"
    | Write loc -> Loc.to_string loc

let print_values values =
  let kvlist = LocMap.bindings values in
  let strlist = List.map (fun (read_loc, write_locs) ->
    Printf.sprintf "%s => { %s }"
      (Loc.to_string read_loc)
      (String.concat ", " @@ List.map print_write_loc write_locs)
  ) kvlist in
  Printf.sprintf "[ %s ]" (String.concat "; " strlist)
