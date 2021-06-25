(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TypeScheme = Type.TypeScheme

class member_searcher add_member =
  object (this)
    inherit Typed_ast_utils.type_parameter_mapper

    method! on_loc_annot x = x

    method! on_type_annot x = x

    method! member member =
      let open Flow_ast.Expression.Member in
      let { _object = ((_, type_), _); property; _ } = member in
      (match property with
      | PropertyIdentifier ((aloc, _), Flow_ast.Identifier.{ name; _ }) ->
        this#annot_with_tparams (add_member ~type_ ~aloc ~name)
      | _ -> ());
      member
  end

class type_reference_searcher add_reference =
  object
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method! generic_identifier_type git =
      let open Flow_ast.Type.Generic.Identifier in
      (match git with
      | Unqualified id
      | Qualified (_, { id; _ }) ->
        add_reference id);
      super#generic_identifier_type git
  end

open GleanSchema

let remove_dot_flow_suffix = function
  | Module.File file ->
    Module.File (Base.Option.value ~default:file (Base.String.chop_suffix ~suffix:".flow" file))
  | module_ -> module_

let module_of_module_ref ~resolved_requires ~root ~write_root module_ref =
  SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
  |> Module.of_modulename ~root ~write_root

let loc_of_index ~loc_source ~reader (i : Type_sig_collections.Locs.index) : Loc.t =
  (i :> int)
  |> ALoc.ALocRepresentationDoNotUse.make_keyed loc_source
  |> Parsing_heaps.Reader.loc_of_aloc ~reader

let loc_of_def ~loc_source ~reader packed_def =
  let idx = Type_sig.def_id_loc packed_def in
  loc_of_index ~loc_source ~reader idx

let source_of_type_exports ~root ~write_root ~file ~reader ~loc_source ~type_sig ~resolved_requires
    =
  let Packed_type_sig.Module.{ module_kind; module_refs; local_defs; remote_refs; _ } = type_sig in
  let open Base.List.Let_syntax in
  let open Type_sig_pack in
  let source_of_remote_ref = function
    | ImportType { index; remote; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_requires ~root ~write_root module_ref
      in
      let typeExport = TypeExport.Named remote in
      return (SourceOfTypeExport.ModuleTypeExport ModuleTypeExport.{ module_; typeExport })
    | ImportTypeof { id_loc; name; _ } ->
      let loc = loc_of_index ~loc_source ~reader id_loc in
      return (SourceOfTypeExport.TypeDeclaration TypeDeclaration.{ name; loc })
    | ImportTypeofNs { index; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_requires ~root ~write_root module_ref
      in
      return (SourceOfTypeExport.ModuleNamespace module_)
    | Import _
    | ImportNs _ ->
      []
  in
  let source_of_packed_ref = function
    | LocalRef { index; _ } ->
      let packed_def = Type_sig_collections.Local_defs.get local_defs index in
      let name = Type_sig.def_name packed_def in
      let loc = loc_of_def ~loc_source ~reader packed_def in
      return (SourceOfTypeExport.TypeDeclaration TypeDeclaration.{ name; loc })
    | RemoteRef { index; _ } ->
      let remote_ref = Type_sig_collections.Remote_refs.get remote_refs index in
      source_of_remote_ref remote_ref
    | BuiltinRef { ref_loc; name } ->
      let loc = loc_of_index ~loc_source ~reader ref_loc in
      let source = SourceOfTypeExport.TypeDeclaration TypeDeclaration.{ name; loc } in
      return source
  in
  let module_ = Module.of_file_key ~root ~write_root file |> remove_dot_flow_suffix in
  match module_kind with
  | CJSModule { type_exports; info = CJSModuleInfo { type_export_keys; type_stars; _ }; _ }
  | ESModule { type_exports; info = ESModuleInfo { type_export_keys; type_stars; _ }; _ } ->
    let star_type_exports =
      let%bind (_, index) = type_stars in
      let module_ref = Type_sig_collections.Module_refs.get module_refs index in
      let remote_module = module_of_module_ref ~resolved_requires ~root ~write_root module_ref in
      let typeExport = TypeExport.Star remote_module in
      let moduleTypeExport = ModuleTypeExport.{ module_; typeExport } in
      let source = SourceOfTypeExport.ModuleNamespace remote_module in
      return SourceOfTypeExport.{ source; moduleTypeExport }
    in
    let non_star_type_exports =
      let%bind (type_export, type_export_key) =
        Base.List.zip_exn (Array.to_list type_exports) (Array.to_list type_export_keys)
      in
      let typeExport = TypeExport.Named type_export_key in
      let moduleTypeExport = ModuleTypeExport.{ module_; typeExport } in
      let%bind source =
        match type_export with
        | ExportTypeRef packed_ref -> source_of_packed_ref packed_ref
        | ExportTypeBinding index ->
          let packed_def = Type_sig_collections.Local_defs.get local_defs index in
          let name = Type_sig.def_name packed_def in
          let loc = Type_sig.def_id_loc packed_def |> loc_of_index ~loc_source ~reader in
          return (SourceOfTypeExport.TypeDeclaration TypeDeclaration.{ name; loc })
        | ExportTypeFrom index ->
          let remote_ref = Type_sig_collections.Remote_refs.get remote_refs index in
          source_of_remote_ref remote_ref
      in
      return SourceOfTypeExport.{ source; moduleTypeExport }
    in
    Base.List.map
      ~f:(SourceOfTypeExport.to_json ~root ~write_root)
      (star_type_exports @ non_star_type_exports)

let export_of_export_name = function
  | "default" -> Export.Default
  | name -> Export.Named name

let type_import_declarations ~root ~write_root ~reader ~resolved_requires ~file_sig =
  let open File_sig.With_ALoc in
  let open Base.List.Let_syntax in
  let type_declaration_map = ref SMap.empty in
  (match%bind file_sig.module_sig.requires with
  | Import { source = (_, module_ref); types; typesof; typesof_ns; _ } ->
    let module_ = module_of_module_ref ~resolved_requires ~root ~write_root module_ref in
    let types_info =
      let%bind (export_name, local) = SMap.elements types in
      let typeExport = TypeExport.Named export_name in
      let import = TypeImportDeclaration.Type ModuleTypeExport.{ module_; typeExport } in
      let%bind (name, locs) = SMap.elements local in
      let%bind { local_loc; _ } = Nel.to_list locs in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader local_loc in
      let typeDeclaration = TypeDeclaration.{ name; loc } in
      type_declaration_map := SMap.add name loc !type_declaration_map;
      return TypeImportDeclaration.{ import; typeDeclaration }
    in
    let typesof_info =
      let%bind (export_name, local) = SMap.elements typesof in
      let export = export_of_export_name export_name in
      let import = TypeImportDeclaration.Typeof ModuleExport.{ module_; export } in
      let%bind (name, locs) = SMap.elements local in
      let%bind { local_loc; _ } = Nel.to_list locs in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader local_loc in
      let typeDeclaration = TypeDeclaration.{ name; loc } in
      type_declaration_map := SMap.add name loc !type_declaration_map;
      return TypeImportDeclaration.{ import; typeDeclaration }
    in
    let typesof_ns_info =
      let%bind (aloc, name) = Base.Option.to_list typesof_ns in
      let import = TypeImportDeclaration.ModuleTypeof module_ in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      let typeDeclaration = TypeDeclaration.{ loc; name } in
      type_declaration_map := SMap.add name loc !type_declaration_map;
      return TypeImportDeclaration.{ import; typeDeclaration }
    in
    types_info @ typesof_info @ typesof_ns_info
  | Require _
  | ImportDynamic _
  | Import0 _ ->
    [])
  |> Base.List.map ~f:(TypeImportDeclaration.to_json ~root ~write_root)
  |> fun type_import_declarations -> (type_import_declarations, !type_declaration_map)

let type_declaration_references ~root ~write_root ~reader ~full_cx ~typed_ast =
  let results = ref [] in
  let add_reference ((aloc, t), Flow_ast.Identifier.{ name; _ }) =
    let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
    let rec def_loc_of_t t =
      let def_loc = t |> TypeUtil.def_loc_of_t |> Parsing_heaps.Reader.loc_of_aloc ~reader in
      if Loc.contains def_loc loc then
        match Flow_js_utils.possible_types_of_type full_cx t with
        | [t'] -> def_loc_of_t t'
        | _ -> None
      else
        Some def_loc
    in
    Base.Option.iter (def_loc_of_t t) ~f:(fun def_loc ->
        let typeDeclaration = TypeDeclaration.{ loc = def_loc; name } in
        let result = TypeDeclarationReference.{ typeDeclaration; loc } in
        results := result :: !results)
  in
  ignore ((new type_reference_searcher add_reference)#program typed_ast);
  !results |> Base.List.map ~f:(TypeDeclarationReference.to_json ~root ~write_root)

let extract_member_def ~cx ~typed_ast ~file_sig scheme name : ALoc.t option =
  let open Ty_members in
  match extract ~include_proto_members:true ~cx ~typed_ast ~file_sig scheme with
  | Error _ -> None
  | Ok { members; _ } ->
    Base.Option.bind
      (NameUtils.Map.find_opt (Reason.OrdinaryName name) members)
      ~f:(fun { def_loc; _ } -> def_loc)

let member_declaration_references ~root ~write_root ~reader ~full_cx ~typed_ast ~file_sig =
  let results = ref [] in
  let add_member ~type_ ~aloc ~name ~tparams_rev =
    let scheme = TypeScheme.{ tparams_rev; type_ } in
    Base.Option.iter
      (extract_member_def ~cx:full_cx ~typed_ast ~file_sig scheme name)
      ~f:(fun def_aloc ->
        let memberDeclaration =
          let loc = Parsing_heaps.Reader.loc_of_aloc ~reader def_aloc in
          MemberDeclaration.{ name; loc }
        in
        let result =
          let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
          MemberDeclarationReference.{ memberDeclaration; loc }
        in
        results := result :: !results)
  in
  ignore ((new member_searcher add_member)#program typed_ast);
  !results |> Base.List.map ~f:(MemberDeclarationReference.to_json ~root ~write_root)

let import_declarations ~root ~write_root ~reader ~resolved_requires ~file_sig =
  let open File_sig.With_ALoc in
  let open Base.List.Let_syntax in
  (match%bind file_sig.module_sig.requires with
  | Require { source = (_, module_ref); bindings; _ } ->
    let module_ = module_of_module_ref ~resolved_requires ~root ~write_root module_ref in
    (match bindings with
    | None -> []
    | Some (BindIdent (aloc, name)) ->
      let import =
        ImportDeclaration.ModuleExport ModuleExport.{ module_; export = Export.CommonJS }
      in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      let declaration = Declaration.{ loc; name } in
      return ImportDeclaration.{ import; declaration }
    | Some (BindNamed named_bindings) ->
      (match%bind named_bindings with
      | (_, BindNamed _) ->
        (* currently we only track the top-level members of commonJS imports/exports *)
        []
      | ((_, export_name), BindIdent (aloc, name)) ->
        let import =
          let export = Export.CommonJSMember export_name in
          ImportDeclaration.ModuleExport ModuleExport.{ module_; export }
        in
        let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
        let declaration = Declaration.{ loc; name } in
        return ImportDeclaration.{ import; declaration }))
  | ImportDynamic _
  | Import0 _ ->
    []
  | Import { source = (_, module_ref); named; ns; _ } ->
    let module_ = module_of_module_ref ~resolved_requires ~root ~write_root module_ref in
    let named_import_declarations =
      let%bind (export_name, local) = SMap.elements named in
      let export = export_of_export_name export_name in
      let import = ImportDeclaration.ModuleExport ModuleExport.{ module_; export } in
      let%bind (name, locs) = SMap.elements local in
      let%bind { local_loc; _ } = Nel.to_list locs in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader local_loc in
      let declaration = Declaration.{ loc; name } in
      return ImportDeclaration.{ import; declaration }
    in
    let namespace_import_declarations =
      let import = ImportDeclaration.ModuleNamespace module_ in
      let%bind (aloc, name) = Base.Option.to_list ns in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      let declaration = Declaration.{ loc; name } in
      return ImportDeclaration.{ import; declaration }
    in
    namespace_import_declarations @ named_import_declarations)
  |> Base.List.map ~f:(ImportDeclaration.to_json ~root ~write_root)

let loc_of_obj_value_prop ~loc_source ~reader =
  let open Type_sig in
  function
  | ObjValueField (index, _, _)
  | ObjValueAccess (Get (index, _))
  | ObjValueAccess (Set (index, _))
  | ObjValueAccess (GetSet (_, _, index, _))
  | ObjValueMethod { id_loc = index; _ } ->
    loc_of_index ~loc_source ~reader index

let loc_of_obj_annot_prop ~loc_source ~reader =
  let open Type_sig in
  function
  | ObjAnnotField (index, _, _)
  | ObjAnnotAccess (Get (index, _))
  | ObjAnnotAccess (Set (index, _))
  | ObjAnnotAccess (GetSet (_, _, index, _))
  | ObjAnnotMethod { id_loc = index; _ } ->
    loc_of_index ~loc_source ~reader index

let source_of_exports ~root ~write_root ~loc_source ~type_sig ~resolved_requires ~reader =
  let Packed_type_sig.Module.{ module_kind; local_defs; remote_refs; module_refs; _ } = type_sig in
  let open Base.List.Let_syntax in
  let open Type_sig_pack in
  let module_ = Module.of_loc_source ~root ~write_root loc_source |> remove_dot_flow_suffix in
  let source_of_remote_ref = function
    | Import { index; remote; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_requires ~root ~write_root module_ref
      in
      let export = Export.Named remote in
      return (SourceOfExport.ModuleExport ModuleExport.{ module_; export })
    | ImportNs { index; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_requires ~root ~write_root module_ref
      in
      return (SourceOfExport.ModuleNamespace module_)
    | ImportType _
    | ImportTypeof _
    | ImportTypeofNs _ ->
      []
  in
  let source_of_packed_ref = function
    | LocalRef { index; _ } ->
      let source =
        let packed_def = Type_sig_collections.Local_defs.get local_defs index in
        let name = Type_sig.def_name packed_def in
        let loc = loc_of_def ~loc_source ~reader packed_def in
        SourceOfExport.Declaration Declaration.{ name; loc }
      in
      return source
    | RemoteRef { index; _ } ->
      let remote_ref = Type_sig_collections.Remote_refs.get remote_refs index in
      let%bind source = source_of_remote_ref remote_ref in
      return source
    | BuiltinRef { ref_loc; name } ->
      let loc = loc_of_index ~loc_source ~reader ref_loc in
      let source = SourceOfExport.Declaration Declaration.{ name; loc } in
      return source
  in
  let sources_of_all_exports =
    match module_kind with
    | CJSModule { exports; _ } ->
      let open Type_sig in
      let rec soes_of_packed_value = function
        | ObjLit { props; _ } ->
          let%bind (name, prop) = SMap.bindings props in
          let moduleExport =
            let export = Export.CommonJSMember name in
            ModuleExport.{ module_; export }
          in
          let source =
            let loc = loc_of_obj_value_prop ~loc_source ~reader prop in
            SourceOfExport.MemberDeclaration MemberDeclaration.{ name; loc }
          in
          return SourceOfExport.{ moduleExport; source }
        | _ -> []
      and soes_of_packed_def ~seen = function
        | Variable { def; _ } -> soes_of_packed ~seen def
        | _ -> []
      and soes_of_packed_annot = function
        | ObjAnnot { props; _ } ->
          let%bind (name, prop) = SMap.bindings props in
          let moduleExport =
            let export = Export.CommonJSMember name in
            ModuleExport.{ module_; export }
          in
          let source =
            let loc = loc_of_obj_annot_prop ~loc_source ~reader prop in
            SourceOfExport.MemberDeclaration MemberDeclaration.{ name; loc }
          in
          return SourceOfExport.{ moduleExport; source }
        | _ -> []
      and soes_of_packed ~seen = function
        | Value packed_value -> soes_of_packed_value packed_value
        | Ref packed_ref ->
          let overall_source_of_exports =
            let moduleExport =
              let export = Export.CommonJS in
              ModuleExport.{ module_; export }
            in
            let%bind source = source_of_packed_ref packed_ref in
            return SourceOfExport.{ moduleExport; source }
          in
          let member_source_of_exports =
            match packed_ref with
            | LocalRef { index; _ } when not (ISet.mem (index :> int) seen) ->
              let packed_def = Type_sig_collections.Local_defs.get local_defs index in
              soes_of_packed_def ~seen:(ISet.add (index :> int) seen) packed_def
            | _ -> []
          in
          overall_source_of_exports @ member_source_of_exports
        | Annot packed_annot -> soes_of_packed_annot packed_annot
        | TyRef _ ->
          (* if we want to, we could follow TyRefs through to Annots *)
          []
        | _ -> []
      in
      let%bind exports = Base.Option.to_list exports in
      soes_of_packed ~seen:ISet.empty exports
    | ESModule { exports; info = ESModuleInfo { export_keys; stars; _ }; _ } ->
      let sources_of_non_star_exports =
        let%bind (es_export, export_key) =
          exports |> Array.mapi (fun i export -> (export, export_keys.(i))) |> Array.to_list
        in
        match es_export with
        | ExportRef packed_ref ->
          let export = Export.Named export_key in
          let moduleExport = ModuleExport.{ module_; export } in
          let%bind source = source_of_packed_ref packed_ref in
          return SourceOfExport.{ moduleExport; source }
        | ExportBinding index ->
          let packed_def = Type_sig_collections.Local_defs.get local_defs index in
          let name = Type_sig.def_name packed_def in
          let moduleExport =
            let export = Export.Named name in
            ModuleExport.{ module_; export }
          in
          let source =
            let loc = loc_of_def ~loc_source ~reader packed_def in
            SourceOfExport.Declaration Declaration.{ name; loc }
          in
          return SourceOfExport.{ moduleExport; source }
        | ExportDefault { def; default_loc } ->
          let moduleExport =
            let export = Export.Default in
            ModuleExport.{ module_; export }
          in
          let%bind source =
            match def with
            | Ref packed_ref -> source_of_packed_ref packed_ref
            | _ ->
              let name = "default" in
              let loc = loc_of_index ~loc_source ~reader default_loc in
              return (SourceOfExport.Declaration Declaration.{ name; loc })
          in
          return SourceOfExport.{ moduleExport; source }
        | ExportDefaultBinding { index; _ } ->
          let moduleExport =
            let export = Export.Default in
            ModuleExport.{ module_; export }
          in
          let source =
            let packed_def = Type_sig_collections.Local_defs.get local_defs index in
            let name = Type_sig.def_name packed_def in
            let loc = loc_of_def ~loc_source ~reader packed_def in
            SourceOfExport.Declaration Declaration.{ name; loc }
          in
          return SourceOfExport.{ moduleExport; source }
        | ExportFrom index ->
          let moduleExport =
            let export = Export.Named export_key in
            ModuleExport.{ module_; export }
          in
          let remote_ref = Type_sig_collections.Remote_refs.get remote_refs index in
          let%bind source = source_of_remote_ref remote_ref in
          return SourceOfExport.{ moduleExport; source }
      in
      let sources_of_star_exports =
        let%bind (_, index) = stars in
        let star_module =
          let module_ref = Type_sig_collections.Module_refs.get module_refs index in
          module_of_module_ref ~resolved_requires ~root ~write_root module_ref
        in
        let moduleExport =
          let export = Export.Star star_module in
          ModuleExport.{ module_; export }
        in
        let source = SourceOfExport.ModuleNamespace star_module in
        return SourceOfExport.{ moduleExport; source }
      in
      sources_of_star_exports @ sources_of_non_star_exports
  in
  Base.List.map ~f:(SourceOfExport.to_json ~root ~write_root) sources_of_all_exports

let local_declaration_references ~root ~write_root ~scope_info =
  let add_uses_of_def def uses acc =
    let jsons =
      let open Base.List.Let_syntax in
      let%bind loc = Nel.to_list def.Scope_builder.Api.Def.locs in
      let declaration = Declaration.{ name = def.Scope_builder.Api.Def.actual_name; loc } in
      let%bind use_loc = Loc_sig.LocS.LSet.elements uses in
      let%bind loc =
        if Loc.equal use_loc loc then
          []
        else
          return use_loc
      in
      let ref = LocalDeclarationReference.{ declaration; loc } in
      let json = LocalDeclarationReference.to_json ~root ~write_root ref in
      return json
    in
    jsons @ acc
  in
  let uses_of_all_defs = Scope_builder.Api.uses_of_all_defs scope_info in
  Scope_builder.Api.DefMap.fold add_uses_of_def uses_of_all_defs []

class declaration_info_collector ~scope_info ~reader ~add_var_info ~add_member_info ~add_type_info =
  object (this)
    inherit Typed_ast_utils.type_parameter_mapper as super

    method! t_identifier (((aloc, type_), Flow_ast.Identifier.{ name; _ }) as ident) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      if
        Scope_builder.Api.is_local_use scope_info loc && Scope_builder.Api.use_is_def scope_info loc
      then
        this#annot_with_tparams add_var_info name loc type_;
      ident

    method! object_key_identifier (((aloc, type_), Flow_ast.Identifier.{ name; _ }) as ident) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      this#annot_with_tparams add_member_info name loc type_;
      ident

    method! type_alias
        ( Flow_ast.Statement.TypeAlias.{ id = ((aloc, type_), Flow_ast.Identifier.{ name; _ }); _ }
        as ident ) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      this#annot_with_tparams add_type_info name loc type_;
      ident

    method! opaque_type
        ( Flow_ast.Statement.OpaqueType.{ id = ((aloc, type_), Flow_ast.Identifier.{ name; _ }); _ }
        as ident ) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      this#annot_with_tparams add_type_info name loc type_;
      ident

    method! interface
        ( Flow_ast.Statement.Interface.{ id = ((aloc, type_), Flow_ast.Identifier.{ name; _ }); _ }
        as ident ) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      this#annot_with_tparams add_type_info name loc type_;
      ident

    method! class_identifier (((aloc, type_), Flow_ast.Identifier.{ name; _ }) as ident) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      this#annot_with_tparams add_type_info name loc type_;
      super#class_identifier ident

    method! enum_declaration enum =
      let open Flow_ast.Statement.EnumDeclaration in
      let { id = ((aloc, type_), Flow_ast.Identifier.{ name; _ }); body = (_, body); _ } = enum in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      this#annot_with_tparams add_type_info name loc type_;
      let defaulted_member (aloc, { DefaultedMember.id = (_, Flow_ast.Identifier.{ name; _ }); _ })
          =
        let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
        this#annot_with_tparams add_member_info name loc type_
      in
      let initialized_member
          (aloc, { InitializedMember.id = (_, Flow_ast.Identifier.{ name; _ }); _ }) =
        let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
        this#annot_with_tparams add_member_info name loc type_
      in
      (match body with
      | BooleanBody BooleanBody.{ members; _ } -> Base.List.iter members ~f:initialized_member
      | NumberBody NumberBody.{ members; _ } -> Base.List.iter members ~f:initialized_member
      | StringBody StringBody.{ members = Defaulted members; _ } ->
        Base.List.iter members ~f:defaulted_member
      | StringBody StringBody.{ members = Initialized members; _ } ->
        Base.List.iter members ~f:initialized_member
      | SymbolBody SymbolBody.{ members; _ } -> Base.List.iter members ~f:defaulted_member);
      super#enum_declaration enum
  end

let declaration_infos ~root ~write_root ~scope_info ~file ~file_sig ~full_cx ~reader ~typed_ast ~ast
    =
  let infos = ref [] in
  let type_declaration_map = ref SMap.empty in
  let add_info kind ~tparams_rev name loc type_ =
    let scheme = TypeScheme.{ tparams_rev; type_ } in
    infos := ((kind, name, loc), scheme) :: !infos
  in
  let add_var_info = add_info `Declaration in
  let add_member_info = add_info `MemberDeclaration in
  let add_type_info ~tparams_rev name loc type_ =
    type_declaration_map := SMap.add name loc !type_declaration_map;
    add_info `TypeDeclaration ~tparams_rev name loc type_
  in
  ignore
    ((new declaration_info_collector
        ~scope_info
        ~reader
        ~add_var_info
        ~add_member_info
        ~add_type_info)
       #program
       typed_ast);
  let genv = Ty_normalizer_env.mk_genv ~full_cx ~file ~typed_ast ~file_sig in
  let options =
    {
      Ty_normalizer_env.expand_internal_types = false;
      expand_type_aliases = false;
      flag_shadowed_type_params = false;
      preserve_inferred_literal_types = false;
      evaluate_type_destructors = false;
      optimize_types = true;
      omit_targ_defaults = false;
      merge_bot_and_any_kinds = true;
      verbose_normalizer = false;
      max_depth = Some 50;
    }
  in
  let exact_by_default = Context.exact_by_default full_cx in
  let documentations = Find_documentation.def_loc_to_comment_loc_map ast in
  ( Base.List.fold
      (Ty_normalizer.from_schemes ~options ~genv !infos)
      ~init:([], [], [])
      ~f:(fun (var_infos, member_infos, type_infos) ((kind, name, loc), elt_result) ->
        let documentation = Loc_sig.LocS.LMap.find_opt loc documentations in
        match elt_result with
        | Error _ -> (var_infos, member_infos, type_infos)
        | Ok elt ->
          let type_ = Ty_printer.string_of_elt ~exact_by_default elt in
          (match kind with
          | `Declaration ->
            let declaration = Declaration.{ name; loc } in
            let var_info =
              DeclarationInfo.{ declaration; type_; documentation }
              |> DeclarationInfo.to_json ~root ~write_root
            in
            (var_info :: var_infos, member_infos, type_infos)
          | `MemberDeclaration ->
            let memberDeclaration = MemberDeclaration.{ name; loc } in
            let member_info =
              MemberDeclarationInfo.{ memberDeclaration; type_; documentation }
              |> MemberDeclarationInfo.to_json ~root ~write_root
            in
            (var_infos, member_info :: member_infos, type_infos)
          | `TypeDeclaration ->
            let typeDeclaration = TypeDeclaration.{ name; loc } in
            let type_info =
              TypeDeclarationInfo.{ typeDeclaration; type_; documentation }
              |> TypeDeclarationInfo.to_json ~root ~write_root
            in
            (var_infos, member_infos, type_info :: type_infos))),
    !type_declaration_map )

let file_of_string_modules ~root ~write_root ~options ~docblock ~file:file_key =
  let open Base.List.Let_syntax in
  let%bind file =
    match Module.of_file_key ~root ~write_root file_key |> remove_dot_flow_suffix with
    | Module.File file -> return file
    | _ -> []
  in
  let%bind string =
    match Module_js.exported_module ~options file_key docblock with
    | Modulename.String string -> return string
    | Modulename.Filename _ -> []
  in
  return FileOfStringModule.(to_json { file; string })

let file_liness ~root ~write_root ~file:file_key =
  let open Base.List.Let_syntax in
  let%bind file =
    match Module.of_file_key ~root ~write_root file_key with
    | Module.File file -> return file
    | _ -> []
  in
  let%bind table = Offset_cache.offset_table_of_file_key file_key |> Base.Option.to_list in
  let lengths = Offset_utils.line_lengths table in
  let hasUnicodeOrTabs = Offset_utils.contains_multibyte_character table in
  let%bind endsInNewline =
    Offset_cache.ends_in_newline_of_file_key file_key |> Base.Option.to_list
  in
  return Src.FileLines.(to_json { file; lengths; hasUnicodeOrTabs; endsInNewline })

let make ~output_dir ~write_root =
  ( module Codemod_runner.MakeSimpleTypedRunner (struct
    type accumulator = {
      files_analyzed: int;
      json_filenames: SSet.t;
    }

    let reporter =
      let open Codemod_report in
      let report =
        StringReporter
          (fun _ { files_analyzed; json_filenames } ->
            SSet.iter
              (fun output_file ->
                let oc =
                  open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666 output_file
                in
                output_string oc "]";
                close_out oc)
              json_filenames;
            Printf.sprintf "Wrote facts about %d JavaScript files." files_analyzed)
      in
      let combine
          { files_analyzed = fa1; json_filenames = jf1 }
          { files_analyzed = fa2; json_filenames = jf2 } =
        { files_analyzed = fa1 + fa2; json_filenames = SSet.union jf1 jf2 }
      in
      let empty = { files_analyzed = 0; json_filenames = SSet.empty } in
      { report; combine; empty }

    let visit ~options ast ctx =
      let Codemod_context.Typed.{ typed_ast; full_cx; file; file_sig; docblock; type_sig; _ } =
        ctx
      in
      let root = Options.root options in
      let reader = State_reader.create () in
      let resolved_requires =
        Module_heaps.Reader.get_resolved_requires_unsafe ~reader ~audit:Expensive.warn file
      in
      let scope_info = Scope_builder.program ~with_types:false ast in
      let ( (declaration_info, member_declaration_info, type_declaration_info),
            type_declaration_map_1 ) =
        declaration_infos
          ~scope_info
          ~root
          ~write_root
          ~file
          ~file_sig
          ~full_cx
          ~reader
          ~typed_ast
          ~ast
      in
      let (type_import_declaration, type_declaration_map_2) =
        type_import_declarations ~root ~write_root ~reader ~resolved_requires ~file_sig
      in
      let type_declaration_map = SMap.union type_declaration_map_1 type_declaration_map_2 in
      (* TODO: remove all type_declaration_map code *)
      ignore type_declaration_map;
      let loc_source = fst ast |> Loc.source in
      let source_of_export =
        source_of_exports ~root ~write_root ~loc_source ~type_sig ~resolved_requires ~reader
      in
      let source_of_type_export =
        source_of_type_exports
          ~root
          ~write_root
          ~file
          ~reader
          ~loc_source
          ~type_sig
          ~resolved_requires
      in
      let local_declaration_reference =
        local_declaration_references ~root ~write_root ~scope_info
      in
      let import_declaration =
        import_declarations ~root ~write_root ~reader ~resolved_requires ~file_sig
      in
      let member_declaration_reference =
        member_declaration_references ~root ~write_root ~reader ~full_cx ~typed_ast ~file_sig
      in
      let type_declaration_reference =
        type_declaration_references ~root ~write_root ~reader ~full_cx ~typed_ast
      in
      let file_of_string_module =
        file_of_string_modules ~root ~write_root ~options ~docblock ~file
      in
      let file_lines = file_liness ~root ~write_root ~file in
      let output_file =
        let file_name = Printf.sprintf "%d.json" (Unix.getpid ()) in
        Filename.concat output_dir file_name
      in
      let is_first_write_to_file = not (Sys.file_exists output_file) in
      let out_channel =
        open_out_gen [Open_wronly; Open_creat; Open_append; Open_text] 0o666 output_file
      in
      let output_facts predicate facts =
        let open Hh_json in
        json_to_output
          out_channel
          (JSON_Object [("predicate", JSON_String predicate); ("facts", JSON_Array facts)])
      in
      if is_first_write_to_file then
        output_string out_channel "["
      else
        output_string out_channel ",";
      output_facts "flow.LocalDeclarationReference.3" local_declaration_reference;
      output_string out_channel ",";
      output_facts "flow.DeclarationInfo.3" declaration_info;
      output_string out_channel ",";
      output_facts "flow.SourceOfExport.3" source_of_export;
      output_string out_channel ",";
      output_facts "flow.ImportDeclaration.3" import_declaration;
      output_string out_channel ",";
      output_facts "flow.MemberDeclarationReference.3" member_declaration_reference;
      output_string out_channel ",";
      output_facts "flow.MemberDeclarationInfo.3" member_declaration_info;
      output_string out_channel ",";
      output_facts "flow.TypeDeclarationReference.3" type_declaration_reference;
      output_string out_channel ",";
      output_facts "flow.TypeDeclarationInfo.3" type_declaration_info;
      output_string out_channel ",";
      output_facts "flow.TypeImportDeclaration.3" type_import_declaration;
      output_string out_channel ",";
      output_facts "flow.SourceOfTypeExport.3" source_of_type_export;
      output_string out_channel ",";
      output_facts "flow.FileOfStringModule.3" file_of_string_module;
      output_string out_channel ",";
      output_facts "src.FileLines.1" file_lines;
      close_out out_channel;
      { files_analyzed = 1; json_filenames = SSet.singleton output_file }
  end) : Codemod_runner.RUNNABLE )
