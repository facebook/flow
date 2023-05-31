(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let search ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast def_locs =
  let open File_sig in
  let require_name_locs =
    Base.List.fold file_sig.requires ~init:[] ~f:(fun acc -> function
      | Require { bindings = Some bindings; _ } ->
        let rec loop acc = function
          | BindIdent (loc, _) -> loc :: acc
          | BindNamed names ->
            Base.List.fold names ~init:acc ~f:(fun acc (_, bindings) -> loop acc bindings)
        in
        loop acc bindings
      | Import { named; types; _ } ->
        let add map acc =
          SMap.fold
            (fun _ ->
              SMap.fold (fun _ nel acc -> Nel.fold_left (fun acc l -> l.remote_loc :: acc) acc nel))
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
  Base.List.filter_map require_name_locs ~f:(fun remote_loc ->
      match GetDef_js.get_def ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast remote_loc with
      | GetDef_js.Get_def_result.Def (locs, _)
      | GetDef_js.Get_def_result.Partial (locs, _, _) ->
        if Base.List.exists locs ~f:(Base.List.mem def_locs ~equal:Loc.equal) then
          Some remote_loc
        else
          None
      | GetDef_js.Get_def_result.Bad_loc _
      | GetDef_js.Get_def_result.Def_error _ ->
        None
  )
