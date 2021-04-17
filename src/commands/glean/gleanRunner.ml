(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TypeScheme = Type.TypeScheme

module TypeOfLoc = struct
  exception Found of Type.t

  class type_of_loc_searcher ~reader search_loc =
    object
      inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

      method on_loc_annot x = x

      method on_type_annot (aloc, t) =
        if Parsing_heaps.Reader.loc_of_aloc ~reader aloc = search_loc then
          raise (Found t)
        else
          (aloc, t)
    end

  let f ~reader ~typed_ast loc =
    try
      ignore ((new type_of_loc_searcher ~reader loc)#program typed_ast);
      []
    with Found t -> [t]
end

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

let source_of_type_exports
    ~root ~write_root ~file ~file_sig_with_exports_info ~type_declaration_map ~resolved_requires =
  let open File_sig.With_Loc in
  let {
    module_sig = { info = { type_exports_named_info; _ }; type_exports_named; type_exports_star; _ };
    _;
  } =
    file_sig_with_exports_info
  in
  let module_ = Module.of_file_key ~root ~write_root file |> remove_dot_flow_suffix in
  let open Base.List.Let_syntax in
  (* export type foo = bar *)
  let export_declarations_info =
    match%bind type_exports_named_info with
    | DeclareExportDef
        Flow_ast.Statement.(
          DeclareExportDeclaration.(
            ( NamedType (_, TypeAlias.{ id; _ })
            | NamedOpaqueType (_, OpaqueType.{ id; _ })
            | Interface (_, Interface.{ id; _ }) )))
    | ExportNamedDef
        Flow_ast.Statement.
          ( _,
            ( TypeAlias TypeAlias.{ id; _ }
            | OpaqueType OpaqueType.{ id; _ }
            | InterfaceDeclaration Interface.{ id; _ } ) ) ->
      let (loc, Flow_ast.Identifier.{ name; _ }) = id in
      let source = SourceOfTypeExport.TypeDeclaration TypeDeclaration.{ name; loc } in
      let moduleTypeExport =
        let typeExport = TypeExport.Named name in
        ModuleTypeExport.{ module_; typeExport }
      in
      return SourceOfTypeExport.{ source; moduleTypeExport }
    | DeclareExportDef
        Flow_ast.Statement.DeclareExportDeclaration.(
          Variable _ | Function _ | Class _ | DefaultType _)
    | ExportDefaultDef _
    | ExportNamedDef _ ->
      []
  in
  (* export type {foo} *)
  let named_exports_info =
    let%bind (export_name, (_, TypeExportNamed { kind; _ })) = type_exports_named in
    let moduleTypeExport =
      ModuleTypeExport.{ module_; typeExport = TypeExport.Named export_name }
    in
    match kind with
    | NamedDeclaration -> [] (* info for this export was collected in `export_declarations_info` *)
    | NamedSpecifier { local = (_, name); source = None } ->
      let%bind source =
        let%bind loc = SMap.find_opt name type_declaration_map |> Base.Option.to_list in
        return (SourceOfTypeExport.TypeDeclaration TypeDeclaration.{ loc; name })
      in
      return SourceOfTypeExport.{ moduleTypeExport; source }
    | NamedSpecifier { local = (_, local_name); source = Some (_, module_ref) } ->
      let source =
        let module_ =
          SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
          |> Module.of_modulename ~root ~write_root
        in
        let typeExport = TypeExport.Named local_name in
        SourceOfTypeExport.ModuleTypeExport ModuleTypeExport.{ module_; typeExport }
      in
      return SourceOfTypeExport.{ moduleTypeExport; source }
  in
  (* export type * from 'module' *)
  let star_exports_info =
    let%bind (_, ExportStar { source = (_, module_ref); _ }) = type_exports_star in
    let remote_module =
      SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
      |> Module.of_modulename ~root ~write_root
    in
    let typeExport = TypeExport.Star remote_module in
    let moduleTypeExport = ModuleTypeExport.{ module_; typeExport } in
    let source = SourceOfTypeExport.ModuleNamespace remote_module in
    return SourceOfTypeExport.{ source; moduleTypeExport }
  in
  export_declarations_info @ named_exports_info @ star_exports_info
  |> Base.List.map ~f:(SourceOfTypeExport.to_json ~root ~write_root)

let export_of_export_name = function
  | "default" -> Export.Default
  | name -> Export.Named name

let type_import_declarations ~root ~write_root ~reader ~resolved_requires ~file_sig =
  let open File_sig.With_ALoc in
  let open Base.List.Let_syntax in
  let type_declaration_map = ref SMap.empty in
  (match%bind file_sig.module_sig.requires with
  | Import { source = (_, module_ref); types; typesof; typesof_ns; _ } ->
    let module_ =
      SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
      |> Module.of_modulename ~root ~write_root
    in
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
    let module_ =
      SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
      |> Module.of_modulename ~root ~write_root
    in
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
    let module_ =
      SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
      |> Module.of_modulename ~root ~write_root
    in
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

let source_of_exports
    ~root
    ~write_root
    ~ast_loc
    ~file_sig_with_exports_info
    ~file_sig
    ~scope_info
    ~resolved_requires
    ~typed_ast
    ~full_cx
    ~reader =
  let open File_sig.With_Loc in
  let { module_sig = { info = { module_kind_info; _ }; module_kind; _ }; _ } =
    file_sig_with_exports_info
  in
  let def_of_variable loc =
    Base.Option.map
      (Scope_builder.Api.def_of_use_opt scope_info loc)
      ~f:(fun Scope_builder.Api.Def.{ locs; _ } -> Nel.hd locs)
  in
  (* if an expression is just a variable, return the declaration of that variable *)
  let rec decl_of_exp = function
    | Flow_ast.(_, Expression.Identifier (id_loc, Identifier.{ name; _ })) ->
      Base.Option.map (def_of_variable id_loc) ~f:(fun loc -> Declaration.{ loc; name })
    | Flow_ast.(_, Expression.(TypeCast TypeCast.{ expression; _ })) -> decl_of_exp expression
    | _ -> None
  in
  let module_ =
    Module.of_loc_source ~root ~write_root ast_loc.Loc.source |> remove_dot_flow_suffix
  in
  let open Base.List.Let_syntax in
  (match module_kind_info with
  | CommonJSInfo cjs_exports ->
    let member_exports_of_loc loc =
      let%bind module_exports_t = TypeOfLoc.f ~reader ~typed_ast loc in
      let%bind Ty_members.{ members; _ } =
        Ty_members.extract
          ~include_proto_members:false
          ~cx:full_cx
          ~typed_ast
          ~file_sig
          TypeScheme.{ tparams_rev = []; type_ = module_exports_t }
        |> Base.Result.ok
        |> Base.Option.to_list
      in
      let%bind (name, Ty_members.{ def_loc; _ }) = NameUtils.Map.elements members in
      (* TODO consider excluding internal names *)
      let name = Reason.display_string_of_name name in
      let moduleExport = ModuleExport.{ module_; export = Export.CommonJSMember name } in
      let%bind aloc = Base.Option.to_list def_loc in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      let source = SourceOfExport.MemberDeclaration MemberDeclaration.{ name; loc } in
      return SourceOfExport.{ moduleExport; source }
    in
    (match%bind cjs_exports with
    | DeclareModuleExportsDef (_, (loc, _)) ->
      let overall_export =
        let moduleExport = ModuleExport.{ module_; export = Export.CommonJS } in
        let source = SourceOfExport.MemberDeclaration MemberDeclaration.{ name = "exports"; loc } in
        SourceOfExport.{ moduleExport; source }
      in
      let member_exports = member_exports_of_loc loc in
      overall_export :: member_exports
    | SetModuleExportsDef ((loc, _) as exp) ->
      let overall_export =
        let moduleExport = ModuleExport.{ module_; export = Export.CommonJS } in
        let source =
          match decl_of_exp exp with
          | None -> SourceOfExport.MemberDeclaration MemberDeclaration.{ name = "exports"; loc }
          | Some decl -> SourceOfExport.Declaration decl
        in
        SourceOfExport.{ moduleExport; source }
      in
      let member_exports = member_exports_of_loc loc in
      overall_export :: member_exports
    | AddModuleExportsDef ((loc, name), exp) ->
      let export = Export.CommonJSMember name in
      let moduleExport = ModuleExport.{ module_; export } in
      let source =
        match decl_of_exp exp with
        | None -> SourceOfExport.MemberDeclaration MemberDeclaration.{ name; loc }
        | Some decl -> SourceOfExport.Declaration decl
      in
      return SourceOfExport.{ moduleExport; source })
  | ESInfo es_exports ->
    (* info about exports like `export <declaration>` *)
    let export_declarations_info =
      let defs_of_stmt_loc stmt_loc =
        let%bind scope = Scope_builder.Api.scope_of_loc scope_info ast_loc in
        let Scope_builder.Api.Scope.{ defs; _ } = Scope_builder.Api.scope scope_info scope in
        let%bind (name, Scope_builder.Api.Def.{ locs; _ }) = SMap.elements defs in
        let%bind loc =
          Base.List.filter (Nel.to_list locs) ~f:(fun def_loc -> Loc.contains stmt_loc def_loc)
        in
        return (loc, name)
      in
      match%bind es_exports with
      | DeclareExportDef
          Flow_ast.Statement.(
            DeclareExportDeclaration.(
              ( Variable (_, DeclareVariable.{ id; _ })
              | Function (_, DeclareFunction.{ id; _ })
              | Class (_, DeclareClass.{ id; _ }) ))) ->
        let (loc, Flow_ast.Identifier.{ name; _ }) = id in
        let moduleExport = ModuleExport.{ module_; export = Export.Named name } in
        let source = SourceOfExport.Declaration Declaration.{ loc; name } in
        return SourceOfExport.{ moduleExport; source }
      | DeclareExportDef Flow_ast.Statement.DeclareExportDeclaration.(DefaultType (loc, _)) ->
        let moduleExport = ModuleExport.{ module_; export = Export.Default } in
        let source = SourceOfExport.Declaration Declaration.{ loc; name = "default" } in
        return SourceOfExport.{ moduleExport; source }
      | DeclareExportDef
          Flow_ast.Statement.DeclareExportDeclaration.(
            NamedType _ | NamedOpaqueType _ | Interface _) ->
        []
      | ExportDefaultDef export_default_declaration ->
        let export = Export.Default in
        (match export_default_declaration with
        | Flow_ast.Statement.ExportDefaultDeclaration.Expression exp ->
          let moduleExport = ModuleExport.{ module_; export = Export.Default } in
          let%bind declaration =
            match decl_of_exp exp with
            | None ->
              let%bind loc =
                match module_kind with
                | CommonJS _ -> failwith "unreachable"
                | ES { named; _ } ->
                  (match%bind named with
                  | (_, (_, ExportDefault { default_loc; _ })) -> return default_loc
                  | _ -> [])
              in
              return Declaration.{ loc; name = "default" }
            | Some decl -> return decl
          in
          let source = SourceOfExport.Declaration declaration in
          return SourceOfExport.{ moduleExport; source }
        | Flow_ast.Statement.ExportDefaultDeclaration.Declaration (stmt_loc, _) ->
          let%bind (loc, name) = defs_of_stmt_loc stmt_loc in
          let source = SourceOfExport.Declaration Declaration.{ loc; name } in
          let moduleExport = ModuleExport.{ module_; export } in
          return SourceOfExport.{ moduleExport; source })
      | ExportNamedDef (stmt_loc, _) ->
        let%bind (loc, name) = defs_of_stmt_loc stmt_loc in
        let source = SourceOfExport.Declaration Declaration.{ loc; name } in
        let export = Export.Named name in
        let moduleExport = ModuleExport.{ module_; export } in
        return SourceOfExport.{ moduleExport; source }
    in
    (* info about other exports, like `export { foo as bar }` *)
    let export_specifiers_info =
      match module_kind with
      | CommonJS _ -> failwith "unreachable"
      | ES { named; star } ->
        let named_exports =
          let%bind (remote, (_, export)) = named in
          let moduleExport = ModuleExport.{ module_; export = Export.Named remote } in
          match export with
          | ExportDefault _
          | ExportNamed { kind = NamedDeclaration; _ } ->
            (* already collected info for this export in `export_declarations_info` *)
            []
          | ExportNamed { kind = NamedSpecifier { local = (local_loc, local_name); source }; _ } ->
            (match source with
            | None ->
              let%bind source =
                let%bind loc = def_of_variable local_loc |> Base.Option.to_list in
                let name = local_name in
                return (SourceOfExport.Declaration Declaration.{ loc; name })
              in
              return SourceOfExport.{ moduleExport; source }
            | Some (_, module_ref) ->
              let source =
                let module_ =
                  SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
                  |> Module.of_modulename ~root ~write_root
                in
                let export = Export.Named local_name in
                SourceOfExport.ModuleExport ModuleExport.{ module_; export }
              in
              return SourceOfExport.{ moduleExport; source })
          | ExportNs { source = (_, module_ref); _ } ->
            let source =
              let module_ =
                SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
                |> Module.of_modulename ~root ~write_root
              in
              SourceOfExport.ModuleNamespace module_
            in
            return SourceOfExport.{ moduleExport; source }
        in
        let star_exports =
          let%bind (_, ExportStar { source = (_, module_ref); _ }) = star in
          let namespace_module =
            SMap.find module_ref resolved_requires.Module_heaps.resolved_modules
            |> Module.of_modulename ~root ~write_root
          in
          let moduleExport = ModuleExport.{ module_; export = Export.Star namespace_module } in
          let source = SourceOfExport.ModuleNamespace namespace_module in
          return SourceOfExport.{ moduleExport; source }
        in
        named_exports @ star_exports
    in
    export_declarations_info @ export_specifiers_info)
  |> Base.List.map ~f:(SourceOfExport.to_json ~root ~write_root)

let local_declaration_references ~root ~write_root ~scope_info =
  let add_uses_of_def def uses acc =
    let jsons =
      let open Base.List.Let_syntax in
      let%bind loc = Nel.to_list def.Scope_builder.Api.Def.locs in
      let declaration = Declaration.{ name = def.Scope_builder.Api.Def.actual_name; loc } in
      let%bind loc = Loc_sig.LocS.LSet.elements uses in
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
      let Codemod_context.Typed.{ typed_ast; full_cx; file; file_sig; docblock; _ } = ctx in
      let root = Options.root options in
      let module_ref_prefix = Options.haste_module_ref_prefix options in
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
      let (source_of_export, source_of_type_export) =
        match File_sig.With_Loc.program_with_exports_info ~ast ~module_ref_prefix with
        | Error _ -> ([], [])
        | Ok (file_sig_with_exports_info, _) ->
          ( source_of_exports
              ~root
              ~write_root
              ~ast_loc:(fst ast)
              ~file_sig_with_exports_info
              ~file_sig
              ~scope_info
              ~resolved_requires
              ~typed_ast
              ~reader
              ~full_cx,
            source_of_type_exports
              ~root
              ~write_root
              ~file
              ~file_sig_with_exports_info
              ~type_declaration_map
              ~resolved_requires )
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
