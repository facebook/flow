(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open File_sig
open Type_operation_utils

let import_mode_to_import_kind =
  let open Flow_ast.Statement.ImportDeclaration in
  function
  | Ty.ValueMode -> ImportValue
  | Ty.TypeMode -> ImportType
  | Ty.TypeofMode -> ImportTypeof

let add_bind_ident_from_typed_ast cx typed_ast name import_mode loc acc =
  match Typed_ast_finder.find_exact_match_annotation cx typed_ast loc with
  | Some scheme -> (name, loc, import_mode, scheme) :: acc
  | None -> acc

let add_bind_ident_from_imports cx local_name import_mode local_loc source remote_name acc =
  let (source_loc, module_name, source_module_t) = source in
  let import_reason =
    Reason.mk_reason (Reason.RNamedImportedType (module_name, local_name)) source_loc
  in
  let (_, type_) =
    Import_export.import_named_specifier_type
      cx
      import_reason
      (import_mode_to_import_kind import_mode)
      ~module_name
      ~source_module_t
      ~remote_name
      ~local_name
  in
  let scheme = { Type.TypeScheme.tparams_rev = []; type_ } in
  (local_name, local_loc, import_mode, scheme) :: acc

let add_imported_loc_map_bindings cx ~typed_ast ~import_mode ~source map acc =
  let (source_loc, module_name) = source in
  let source_loc = ALoc.of_loc source_loc in
  let source_module_t = lazy (Import_export.get_module_t cx (source_loc, module_name)) in
  SMap.fold
    (fun remote_name remote_map acc ->
      SMap.fold
        (fun local_name imported_locs_nel acc ->
          Nel.fold_left
            (fun acc { local_loc; remote_loc = _ } ->
              let local_loc = ALoc.of_loc local_loc in
              match typed_ast with
              | None ->
                add_bind_ident_from_imports
                  cx
                  local_name
                  import_mode
                  local_loc
                  (source_loc, module_name, Lazy.force source_module_t)
                  remote_name
                  acc
              | Some typed_ast ->
                add_bind_ident_from_typed_ast cx typed_ast local_name import_mode local_loc acc)
            acc
            imported_locs_nel)
        remote_map
        acc)
    map
    acc

let add_require_bindings_from_exports_map cx loc source_name binding acc =
  let reason = Reason.(mk_reason (RCommonJSExports source_name) loc) in
  let module_t =
    Import_export.get_module_t cx ~perform_platform_validation:false (loc, source_name)
  in
  let t = Import_export.cjs_require_type cx reason ~legacy_interop:false module_t in
  match binding with
  | BindIdent (loc, name) ->
    let loc = ALoc.of_loc loc in
    (name, loc, Ty.ValueMode, { Type.TypeScheme.tparams_rev = []; type_ = t }) :: acc
  | BindNamed map -> begin
    let open Type in
    match Import_export.concretize_module_type cx reason module_t with
    | Ok { module_export_types = exports; _ } ->
      let value_exports_tmap = Context.find_exports cx exports.value_exports_tmap in
      List.fold_left
        (fun acc ((_, name), binding) ->
          match binding with
          | BindIdent (loc, name') ->
            (match NameUtils.Map.find_opt (Reason.OrdinaryName name) value_exports_tmap with
            | Some { type_ = t; _ } ->
              (name', ALoc.of_loc loc, Ty.ValueMode, { TypeScheme.tparams_rev = []; type_ = t })
              :: acc
            | None -> acc)
          | BindNamed _ ->
            (* This case should be rare. Not worth collecting imported names from here *)
            acc)
        acc
        map
    | Error _ -> acc
  end

let add_require_bindings_from_typed_ast cx ~typed_ast ~import_mode binding acc =
  let rec loop binding acc =
    match binding with
    | BindIdent (loc, name) ->
      let loc = ALoc.of_loc loc in
      add_bind_ident_from_typed_ast cx typed_ast name import_mode loc acc
    | BindNamed map -> List.fold_left (fun acc (_, binding) -> loop binding acc) acc map
  in
  loop binding acc

let add_require_bindings cx ~typed_ast ~import_mode ~source bindings_opt acc =
  Base.Option.fold bindings_opt ~init:acc ~f:(fun acc bindings ->
      match typed_ast with
      | None ->
        let (loc, name) = source in
        let loc = ALoc.of_loc loc in
        add_require_bindings_from_exports_map cx loc name bindings acc
      | Some typed_ast ->
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
