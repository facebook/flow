(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Loc_collections

type search_result = {
  local_locs: Loc.t list;
  remote_locs: Loc.t list;
}

let search ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast def_locs =
  let open File_sig in
  let require_name_locs =
    Base.List.fold file_sig.requires ~init:[] ~f:(fun acc -> function
      | Require { bindings = Some bindings; _ } ->
        let rec loop acc = function
          | BindIdent (loc, _) -> (loc, loc) :: acc
          | BindNamed names ->
            Base.List.fold names ~init:acc ~f:(fun acc (_, bindings) -> loop acc bindings)
        in
        loop acc bindings
      | Import { named; types; _ } ->
        let add map acc =
          SMap.fold
            (fun _ ->
              SMap.fold (fun _ nel acc ->
                  Nel.fold_left (fun acc l -> (l.remote_loc, l.local_loc) :: acc) acc nel
              ))
            map
            acc
        in
        acc |> add named |> add types
      | Require { bindings = None; _ }
      | ImportDynamic _
      | Import0 _
      | ExportFrom _ ->
        acc
    )
  in
  let (local_locs, remote_locs) =
    Base.List.fold
      require_name_locs
      ~init:(LocSet.empty, LocSet.empty)
      ~f:(fun ((local_locs, remote_locs) as acc) (remote_loc, local_loc) ->
        match GetDef_js.get_def ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast remote_loc with
        | GetDef_js.Get_def_result.Def (locs, _)
        | GetDef_js.Get_def_result.Partial (locs, _, _) ->
          if Base.List.exists locs ~f:(Base.List.mem def_locs ~equal:Loc.equal) then
            if Loc.equal remote_loc local_loc then
              (LocSet.add local_loc local_locs, remote_locs)
            else
              (local_locs, LocSet.add remote_loc remote_locs)
          else
            acc
        | GetDef_js.Get_def_result.Bad_loc _
        | GetDef_js.Get_def_result.Def_error _ ->
          acc
    )
  in
  { local_locs = LocSet.elements local_locs; remote_locs = LocSet.elements remote_locs }
