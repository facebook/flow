(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Sort and dedup by loc.

    This will have to be revisited if we ever need to report multiple ref kinds for
    a single location. *)
let sort_and_dedup refs =
  Base.List.dedup_and_sort ~compare:(fun (_, loc1) (_, loc2) -> Loc.compare loc1 loc2) refs

let local_variable_refs scope_info locs =
  match VariableFindRefs.local_find_refs scope_info locs with
  | None -> None
  | Some (var_refs, _) -> Some var_refs

let find_local_refs ~reader ~options ~file_key ~parse_artifacts ~typecheck_artifacts ~line ~col =
  let open Base.Result.Let_syntax in
  let loc = Loc.cursor (Some file_key) line col in
  let ast_info =
    match parse_artifacts with
    | Types_js_types.Parse_artifacts { ast; file_sig; docblock; _ } -> (ast, file_sig, docblock)
  in
  (* Start by running local variable find references *)
  let (Types_js_types.Parse_artifacts { ast; _ }) = parse_artifacts in
  let scope_info =
    Scope_builder.program ~enable_enums:(Options.enums options) ~with_types:true ast
  in
  let var_refs = local_variable_refs scope_info [loc] in
  let%bind refs =
    match var_refs with
    | Some _ -> Ok var_refs
    | None -> PropertyFindRefs.find_local_refs ~reader file_key ast_info typecheck_artifacts loc
  in
  let refs = Base.Option.map ~f:(fun refs -> sort_and_dedup refs) refs in
  Ok refs

let local_refs_for_global_find_refs ~options ~loc_of_aloc ast_info type_info file_key def_info =
  let var_refs () =
    let def_locs = GetDefUtils.all_locs_of_def_info def_info in
    let (ast, file_sig, _) = ast_info in
    let (Types_js_types.Typecheck_artifacts { cx; typed_ast; _ }) = type_info in
    let scope_info =
      Scope_builder.program ~enable_enums:(Options.enums options) ~with_types:true ast
    in
    let import_def_locs =
      LocalImportRefSearcher.search ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast def_locs
    in
    local_variable_refs scope_info (import_def_locs @ def_locs) |> Base.Option.value ~default:[]
  in
  let merge = function
    | ([], prop_refs) -> prop_refs
    | ((_ :: _ as var_refs), Error _) -> Ok var_refs
    | (var_refs, Ok prop_refs) -> Ok (Base.List.unordered_append prop_refs var_refs)
  in
  match def_info with
  | GetDefUtils.VariableDefinition (def_locs, name) ->
    let prop_refs =
      match (Base.List.map def_locs ~f:(fun l -> GetDefUtils.ObjectProperty l), name) with
      | ([], _) -> Ok []
      | (_, None) -> Error "Name not available name to find property refs"
      | (hd :: tl, Some name) ->
        PropertyFindRefs.property_find_refs_in_file
          ~loc_of_aloc
          ast_info
          type_info
          file_key
          ((hd, tl), name)
    in
    merge (var_refs (), prop_refs)
  | GetDefUtils.PropertyDefinition props_info ->
    let prop_refs =
      PropertyFindRefs.property_find_refs_in_file
        ~loc_of_aloc
        ast_info
        type_info
        file_key
        props_info
    in
    merge (var_refs (), prop_refs)
  | GetDefUtils.NoDefinition -> Ok []
