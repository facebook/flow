(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
open Utils_js

let local_find_refs ast loc =
  let open Scope_api in
  let scope_info = Scope_builder.program ast in
  let all_uses = all_uses scope_info in
  let matching_uses = LocSet.filter (fun use -> Loc.contains use loc) all_uses in
  let num_matching_uses = LocSet.cardinal matching_uses in
  if num_matching_uses = 0 then
    None
  else if num_matching_uses > 1 then
    (* This is unlikely enough that we can just throw *)
    failwith "Multiple identifiers were unexpectedly matched"
  else
    let use = LocSet.choose matching_uses in
    let def = def_of_use scope_info use in
    let sorted_locs = LocSet.elements @@ uses_of_def scope_info ~exclude_def:false def in
    let name = Def.(def.actual_name) in
    let sorted_locs = List.map (fun loc -> (FindRefsTypes.Local, loc)) sorted_locs in
    Some ((name, sorted_locs), Nel.hd def.Def.locs)
