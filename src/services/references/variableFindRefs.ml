(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections
module Scope_api = Scope_api.With_Loc

let local_find_refs scope_info locs =
  Scope_api.(
    let all_uses = all_uses scope_info in
    let matching_uses =
      LocSet.filter (fun use -> Base.List.exists locs ~f:(fun loc -> Loc.contains use loc)) all_uses
    in
    let num_matching_uses = LocSet.cardinal matching_uses in
    if num_matching_uses = 0 then
      None
    else
      let sorted_locs =
        LocSet.fold
          (fun use acc ->
            LocSet.union
              acc
              (use |> def_of_use scope_info |> uses_of_def scope_info ~exclude_def:false))
          matching_uses
          LocSet.empty
        |> LocSet.elements
      in
      let sorted_locs = Base.List.map ~f:(fun loc -> (FindRefsTypes.Local, loc)) sorted_locs in
      Some sorted_locs
  )
