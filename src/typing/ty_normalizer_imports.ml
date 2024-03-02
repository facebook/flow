(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig

let add_bind_ident_from_typed_ast cx typed_ast name import_mode loc acc =
  match Typed_ast_finder.find_exact_match_annotation cx typed_ast loc with
  | Some scheme -> (name, loc, import_mode, scheme) :: acc
  | None -> acc

let add_imported_loc_map_bindings cx ~typed_ast ~import_mode ~source:_ map acc =
  SMap.fold
    (fun _remote_name remote_map acc ->
      SMap.fold
        (fun local_name imported_locs_nel acc ->
          Nel.fold_left
            (fun acc { local_loc; remote_loc = _ } ->
              let local_loc = ALoc.of_loc local_loc in
              add_bind_ident_from_typed_ast cx typed_ast local_name import_mode local_loc acc)
            acc
            imported_locs_nel)
        remote_map
        acc)
    map
    acc

let add_require_bindings_from_typed_ast cx ~typed_ast ~import_mode binding acc =
  let rec loop binding acc =
    match binding with
    | BindIdent (loc, name) ->
      let loc = ALoc.of_loc loc in
      add_bind_ident_from_typed_ast cx typed_ast name import_mode loc acc
    | BindNamed map -> List.fold_left (fun acc (_, binding) -> loop binding acc) acc map
  in
  loop binding acc

let add_require_bindings cx ~typed_ast ~import_mode ~source:_ bindings_opt acc =
  Base.Option.fold bindings_opt ~init:acc ~f:(fun acc bindings ->
      add_require_bindings_from_typed_ast cx ~typed_ast ~import_mode bindings acc
  )

let add_import_bindings cx ~typed_ast acc require =
  match require with
  | Require { source; require_loc = _; bindings; prefix = _ } ->
    add_require_bindings cx ~typed_ast ~import_mode:Ty.ValueMode ~source bindings acc
  | Import { import_loc = _; source; named; ns = _; types; typesof; typesof_ns = _ } ->
    (* TODO import namespaces (`ns`) as modules that might contain imported types *)
    acc
    |> add_imported_loc_map_bindings cx ~typed_ast ~import_mode:Ty.ValueMode ~source named
    |> add_imported_loc_map_bindings cx ~typed_ast ~import_mode:Ty.TypeMode ~source types
    |> add_imported_loc_map_bindings cx ~typed_ast ~import_mode:Ty.TypeofMode ~source typesof
  | ImportDynamic _
  | Import0 _
  | ImportSynthetic _
  | ExportFrom _ ->
    acc

let extract_schemes cx file_sig typed_ast =
  let requires = File_sig.requires file_sig in
  let imports = List.fold_left (add_import_bindings cx ~typed_ast) [] requires in
  List.rev imports
