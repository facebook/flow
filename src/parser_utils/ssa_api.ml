(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make (L: Loc_sig.S) : sig
  type read_loc = L.t
  type write_loc =
    | Write of L.t
    | Uninitialized
  type write_locs = write_loc list
  type values = write_locs L.LMap.t

  val uninitialized: write_loc
  val write_locs_of_read_loc: values -> read_loc -> write_locs
  val is_dead_write_loc: values -> L.t -> bool
end = struct
  type read_loc = L.t
  type write_loc =
    | Write of L.t
    | Uninitialized
  type write_locs = write_loc list
  type values = write_locs L.LMap.t

  let uninitialized = Uninitialized

  let write_locs_of_read_loc values read_loc =
    L.LMap.find read_loc values

  let is_dead_write_loc values loc =
    not (L.LMap.exists (fun _read_loc write_locs -> List.mem (Write loc) write_locs) values)
end

module With_Loc = Make (Loc_sig.LocS)

module With_ALoc = Make (Loc_sig.ALocS)

include With_Loc

let print_values =
  let print_write_loc write_loc =
    match write_loc with
      | Uninitialized -> "(uninitialized)"
      | Write loc -> Loc.to_string loc
  in
  fun values ->
    let kvlist = Utils_js.LocMap.bindings values in
    let strlist = List.map (fun (read_loc, write_locs) ->
      Printf.sprintf "%s => { %s }"
        (Loc.to_string read_loc)
        (String.concat ", " @@ List.map print_write_loc write_locs)
    ) kvlist in
    Printf.sprintf "[ %s ]" (String.concat "; " strlist)
