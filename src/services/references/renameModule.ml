(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let get_dependents ~reader file_key =
  let file = Parsing_heaps.get_file_addr_unsafe file_key in
  Parsing_heaps.Reader.get_haste_info ~reader file
  |> Base.Option.map ~f:(fun haste_info ->
         let haste_module = SharedMem.NewAPI.get_haste_module haste_info in
         let haste_dependents = SharedMem.NewAPI.get_haste_dependents haste_module in
         let dependents = ref [] in
         SharedMem.NewAPI.sklist_iter
           (fun file_addr ->
             let file_key = Parsing_heaps.read_file_key file_addr in
             let ast = Parsing_heaps.Reader.get_ast ~reader file_key in
             dependents := (file_key, ast) :: !dependents)
           haste_dependents;
         !dependents
     )

(* TODO: This only handles global haste paths. It needs to handle relative
   paths, haste package + relative paths, and node requires *)
let get_loc_to_replacement_map ~old_haste_name ~new_haste_name file_sig =
  let open File_sig in
  List.fold_left
    (fun acc require ->
      match require with
      | Require { source = (loc, mref); prefix = Some pre; _ } ->
        if old_haste_name = mref then
          Loc_collections.LocMap.add loc (pre ^ new_haste_name) acc
        else
          acc
      | Require { source = (loc, mref); prefix = None; _ }
      | ImportDynamic { source = (loc, mref); _ }
      | Import0 { source = (loc, mref) }
      | Import { source = (loc, mref); _ }
      | ExportFrom { source = (loc, mref) } ->
        if old_haste_name = mref then
          Loc_collections.LocMap.add loc new_haste_name acc
        else
          acc
      | ImportSynthetic _ -> acc)
    Loc_collections.LocMap.empty
    (File_sig.requires file_sig)

let get_edits_for_file ~old_haste_name ~new_haste_name file_sig =
  let loc_to_replacement_map =
    get_loc_to_replacement_map ~old_haste_name ~new_haste_name file_sig
  in
  Loc_collections.LocMap.fold
    (fun loc replacement acc ->
      let string_layout =
        Js_layout_generator.string_literal
          ~opts:Js_layout_generator.default_opts
          Loc.none
          (Ast_builder.string_literal replacement)
      in
      let newText =
        Source.contents (Pretty_printer.print ~skip_endline:true ~source_maps:None string_layout)
      in
      { Lsp.TextEdit.range = Flow_lsp_conversions.loc_to_lsp_range loc; newText } :: acc)
    loc_to_replacement_map
    []

let get_rename_edits ~reader ~options ~old_haste_name ~new_haste_name old_file_key =
  let opts =
    {
      File_sig.enable_enums = Options.enums options;
      enable_relay_integration = Options.enable_relay_integration options;
      (* This field is only necessary for implicit imports for multiplatform purposes.
       * Renaming will never edit these implicit imports. *)
      explicit_available_platforms = None;
      file_options = Options.file_options options;
      haste_module_ref_prefix = Options.haste_module_ref_prefix options;
      haste_module_ref_prefix_LEGACY_INTEROP =
        Options.haste_module_ref_prefix_LEGACY_INTEROP options;
      relay_integration_module_prefix = Options.relay_integration_module_prefix options;
    }
  in
  let workspace_edit =
    let%map.Base.Option dependents = get_dependents ~reader old_file_key in
    (* TODO: Allow partial edits *)
    let%bind.Base.Result changes =
      List.fold_left
        (fun uri_map_result (file_key, ast') ->
          Base.Option.value_map ast' ~default:uri_map_result ~f:(fun ast ->
              let%bind.Base.Result uri_map = uri_map_result in
              let%bind.Base.Result uri = Flow_lsp_conversions.file_key_to_uri (Some file_key) in
              let dependent_file_sig = File_sig.program ~file_key ~ast ~opts in
              let edits = get_edits_for_file ~old_haste_name ~new_haste_name dependent_file_sig in
              Ok (Lsp.UriMap.add uri edits uri_map)
          ))
        (Ok Lsp.UriMap.empty)
        dependents
    in
    Ok { Lsp.WorkspaceEdit.changes }
  in
  Base.Option.value ~default:(Ok { Lsp.WorkspaceEdit.changes = Lsp.UriMap.empty }) workspace_edit
