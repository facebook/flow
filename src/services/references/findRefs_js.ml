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

let local_refs_of_find_ref_request
    ~options ~loc_of_aloc ast_info type_info file_key { FindRefsTypes.def_info; kind = _ } =
  let var_refs prop_refs =
    let def_locs = GetDefUtils.all_locs_of_def_info def_info in
    let (ast, file_sig, _) = ast_info in
    let (Types_js_types.Typecheck_artifacts { cx; typed_ast; _ }) = type_info in
    let { LocalImportRefSearcher.local_locs = import_def_locs; remote_locs } =
      LocalImportRefSearcher.search ~options ~loc_of_aloc ~cx ~file_sig ~ast ~typed_ast def_locs
    in
    let scope_info =
      Scope_builder.program ~enable_enums:(Options.enums options) ~with_types:true ast
    in
    (* Property refs might contain binding destructuring pattern identifiers.
     * We should find all local references of them. *)
    let prop_ref_locs = Base.List.map prop_refs ~f:snd in
    let starting_locs = Base.List.join [prop_ref_locs; import_def_locs; def_locs] in
    Base.List.unordered_append
      (Base.List.map remote_locs ~f:(fun l -> (FindRefsTypes.Local, l)))
      (VariableFindRefs.local_find_refs scope_info starting_locs |> Base.Option.value ~default:[])
  in
  let merge_with_var_refs = function
    | Ok prop_refs ->
      let var_refs = var_refs prop_refs in
      Ok (Base.List.unordered_append prop_refs var_refs)
    | Error e ->
      (match var_refs [] with
      | [] -> Error e
      | results -> Ok results)
  in
  match def_info with
  | Get_def_types.VariableDefinition (def_locs, name) ->
    let prop_refs =
      match (Base.List.map def_locs ~f:(fun l -> Get_def_types.ObjectProperty l), name) with
      | ([], _) -> Ok []
      | (_, None) -> Error "No available names to find property refs"
      | (hd :: tl, Some name) ->
        PropertyFindRefs.property_find_refs_in_file
          ~loc_of_aloc
          ast_info
          type_info
          file_key
          ((hd, tl), name)
    in
    merge_with_var_refs prop_refs
  | Get_def_types.PropertyDefinition props_info ->
    let prop_refs =
      PropertyFindRefs.property_find_refs_in_file
        ~loc_of_aloc
        ast_info
        type_info
        file_key
        props_info
    in
    merge_with_var_refs prop_refs
  | Get_def_types.NoDefinition -> Ok []

let find_local_refs
    ~reader ~options ~file_key ~parse_artifacts ~typecheck_artifacts ~kind ~line ~col =
  let open Base.Result.Let_syntax in
  let ast_info =
    match parse_artifacts with
    | Types_js_types.Parse_artifacts { ast; file_sig; docblock; _ } -> (ast, file_sig, docblock)
  in
  let%bind def_info =
    GetDefUtils.get_def_info
      ~options
      ~reader
      ast_info
      typecheck_artifacts
      (Loc.cursor (Some file_key) line col)
  in
  match def_info with
  | Get_def_types.NoDefinition -> Ok None
  | _ ->
    let%bind refs =
      local_refs_of_find_ref_request
        ~options
        ~loc_of_aloc:(Parsing_heaps.Reader.loc_of_aloc ~reader)
        ast_info
        typecheck_artifacts
        file_key
        { FindRefsTypes.def_info; kind }
    in
    Ok (Some (sort_and_dedup refs))
