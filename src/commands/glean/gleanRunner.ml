(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

module DocumentationFullspanMap = struct
  type doc_span = {
    documentation: Loc.t option;
    span: Loc.t option;
  }

  (* A map from symbol location to the documentation loc
     and span of the entity defined by the symbol *)

  type t = doc_span Loc_sig.LocS.LMap.t

  let combine ds ds' =
    let or_ a b =
      match a with
      | None -> b
      | _ -> a
    in
    { documentation = or_ ds.documentation ds'.documentation; span = or_ ds.span ds'.span }

  (* Map is created by merging two data structures, [Find_documentation]
     provides the doc map, and [DocumentSymbolProvider] the full spans. *)
  let create ast source : t =
    let open Loc_sig.LocS in
    let open Lsp in
    let comment_map = Find_documentation.def_loc_to_comment_loc_map ast in
    let comment_loc_map = LMap.map (fun d -> { documentation = Some d; span = None }) comment_map in
    let uri = Lsp.DocumentUri.DocumentUri "" in
    let symbol_spans = DocumentSymbolProvider.provide_symbol_information ~uri ast in

    let comment_loc_map_ref = ref comment_loc_map in
    let add_to_map SymbolInformation.{ selectionRange; location = Location.{ range; _ }; _ } =
      let documentation = None in
      let loc = Lsp.lsp_range_to_flow_loc selectionRange ~source in
      let span = Some (Lsp.lsp_range_to_flow_loc range ~source) in
      comment_loc_map_ref := LMap.add ~combine loc { documentation; span } !comment_loc_map_ref
    in
    List.iter add_to_map symbol_spans;
    !comment_loc_map_ref
end

class member_searcher add_member =
  object (_this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method! member annot member =
      let open Ast.Expression.Member in
      let { _object = ((_, type_), _); property; _ } = member in
      (match property with
      | PropertyIdentifier ((aloc, _), Ast.Identifier.{ name; _ }) -> add_member ~type_ ~aloc ~name
      | _ -> ());
      super#member annot member
  end

class type_reference_searcher add_reference =
  object
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method! generic_identifier_type git =
      let open Ast.Type.Generic.Identifier in
      (match git with
      | Unqualified id
      | Qualified (_, { id; _ }) ->
        add_reference id);
      super#generic_identifier_type git
  end

module Type_ = Type
open GleanSchema
module Type = Type_

let remove_dot_flow_suffix = function
  | Module.File file ->
    Module.File (Base.Option.value ~default:file (Base.String.chop_suffix ~suffix:".flow" file))
  | module_ -> module_

let module_of_module_ref ~resolved_modules ~root ~write_root module_ref =
  match SMap.find module_ref resolved_modules with
  | Ok m -> Module.of_modulename ~root ~write_root m
  | Error mapped_name ->
    (* TODO: We reach this codepath for requires that might resolve to builtin
     * modules. During check we check the master context, which we can also do
     * here. *)
    let name = Option.value mapped_name ~default:module_ref in
    Module.String name

let loc_of_index ~loc_source ~reader (i : Type_sig_collections.Locs.index) : Loc.t =
  (i :> int)
  |> ALoc.ALocRepresentationDoNotUse.make_keyed loc_source
  |> Parsing_heaps.Reader.loc_of_aloc ~reader

let loc_of_def ~loc_source ~reader packed_def =
  let idx = Type_sig.def_id_loc packed_def in
  loc_of_index ~loc_source ~reader idx

let source_of_type_exports ~root ~write_root ~file ~reader ~loc_source ~type_sig ~resolved_modules =
  let Packed_type_sig.Module.{ module_kind; module_refs; local_defs; remote_refs; _ } = type_sig in
  let open Base.List.Let_syntax in
  let open Type_sig_pack in
  let source_of_remote_ref = function
    | ImportType { index; remote; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_modules ~root ~write_root module_ref
      in
      let typeExport = TypeExport.Named remote in
      return (SourceOfTypeExport.ModuleTypeExport ModuleTypeExport.{ module_; typeExport })
    | ImportTypeof { id_loc; name; _ } ->
      let loc = loc_of_index ~loc_source ~reader id_loc in
      return (SourceOfTypeExport.TypeDeclaration TypeDeclaration.{ name; loc })
    | ImportTypeofNs { index; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_modules ~root ~write_root module_ref
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
    | BuiltinRef { ref_loc; type_ref = _; name } ->
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
      let remote_module = module_of_module_ref ~resolved_modules ~root ~write_root module_ref in
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

let type_import_declarations ~root ~write_root ~resolved_modules ~file_sig =
  let open File_sig in
  let open Base.List.Let_syntax in
  (match%bind requires file_sig with
  | Import { source = (_, module_ref); types; typesof; typesof_ns; _ } ->
    let module_ = module_of_module_ref ~resolved_modules ~root ~write_root module_ref in
    let types_info =
      let%bind (export_name, local) = SMap.elements types in
      let typeExport = TypeExport.Named export_name in
      let import = TypeImportDeclaration.Type ModuleTypeExport.{ module_; typeExport } in
      let%bind (name, locs) = SMap.elements local in
      let%bind { local_loc; _ } = Nel.to_list locs in
      let typeDeclaration = TypeDeclaration.{ name; loc = local_loc } in
      return TypeImportDeclaration.{ import; typeDeclaration }
    in
    let typesof_info =
      let%bind (export_name, local) = SMap.elements typesof in
      let export = export_of_export_name export_name in
      let import = TypeImportDeclaration.Typeof ModuleExport.{ module_; export } in
      let%bind (name, locs) = SMap.elements local in
      let%bind { local_loc; _ } = Nel.to_list locs in
      let typeDeclaration = TypeDeclaration.{ name; loc = local_loc } in
      return TypeImportDeclaration.{ import; typeDeclaration }
    in
    let typesof_ns_info =
      let%bind (loc, name) = Base.Option.to_list typesof_ns in
      let import = TypeImportDeclaration.ModuleTypeof module_ in
      let typeDeclaration = TypeDeclaration.{ loc; name } in
      return TypeImportDeclaration.{ import; typeDeclaration }
    in
    types_info @ typesof_info @ typesof_ns_info
  | Require _
  | Import0 _
  | ImportDynamic _
  | ImportSynthetic _
  | ExportFrom _ ->
    [])
  |> Base.List.map ~f:(TypeImportDeclaration.to_json ~root ~write_root)

let type_declaration_references ~root ~write_root ~reader ~cx ~typed_ast =
  let results = ref [] in
  let add_reference ((aloc, t), Ast.Identifier.{ name; _ }) =
    let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
    let rec def_loc_of_t t =
      let def_loc = t |> TypeUtil.def_loc_of_t |> Parsing_heaps.Reader.loc_of_aloc ~reader in
      if Loc.contains def_loc loc then
        match Flow_js_utils.possible_types_of_type cx t with
        | [t'] -> def_loc_of_t t'
        | _ -> None
      else
        Some def_loc
    in
    Base.Option.iter (def_loc_of_t t) ~f:(fun def_loc ->
        let typeDeclaration = TypeDeclaration.{ loc = def_loc; name } in
        let result = TypeDeclarationReference.{ typeDeclaration; loc } in
        results := result :: !results
    )
  in
  ignore ((new type_reference_searcher add_reference)#program typed_ast);
  !results |> Base.List.map ~f:(TypeDeclarationReference.to_json ~root ~write_root)

let extract_member_def ~cx ~typed_ast ~file_sig scheme name : ALoc.t list =
  let open Ty_members in
  match extract ~cx ~typed_ast_opt:(Some typed_ast) ~file_sig ~max_depth:40 scheme with
  | Error _ -> []
  | Ok { members; _ } ->
    Base.Option.value
      ~default:[]
      (Base.Option.map
         (NameUtils.Map.find_opt (Reason.OrdinaryName name) members)
         ~f:(fun { def_locs; _ } -> def_locs
       )
      )

let member_declaration_references ~root ~write_root ~reader ~cx ~typed_ast ~file_sig =
  let results = ref [] in
  let add_member ~type_ ~aloc ~name =
    Base.List.iter (extract_member_def ~cx ~typed_ast ~file_sig type_ name) ~f:(fun def_aloc ->
        let memberDeclaration =
          let loc = Parsing_heaps.Reader.loc_of_aloc ~reader def_aloc in
          MemberDeclaration.{ name; loc }
        in
        let result =
          let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
          MemberDeclarationReference.{ memberDeclaration; loc }
        in
        results := result :: !results
    )
  in
  ignore ((new member_searcher add_member)#program typed_ast);
  !results |> Base.List.map ~f:(MemberDeclarationReference.to_json ~root ~write_root)

let import_declarations ~root ~write_root ~resolved_modules ~file_sig =
  let open File_sig in
  let open Base.List.Let_syntax in
  (match%bind requires file_sig with
  | Require { source = (_, module_ref); bindings; _ } ->
    let module_ = module_of_module_ref ~resolved_modules ~root ~write_root module_ref in
    (match bindings with
    | None -> []
    | Some (BindIdent (loc, name)) ->
      let import =
        ImportDeclaration.ModuleExport ModuleExport.{ module_; export = Export.CommonJS }
      in
      let declaration = Declaration.{ loc; name } in
      return ImportDeclaration.{ import; declaration }
    | Some (BindNamed named_bindings) ->
      (match%bind named_bindings with
      | (_, BindNamed _) ->
        (* currently we only track the top-level members of commonJS imports/exports *)
        []
      | ((_, export_name), BindIdent (loc, name)) ->
        let import =
          let export = Export.CommonJSMember export_name in
          ImportDeclaration.ModuleExport ModuleExport.{ module_; export }
        in
        let declaration = Declaration.{ loc; name } in
        return ImportDeclaration.{ import; declaration }))
  | ImportDynamic _
  | Import0 _
  | ImportSynthetic _
  | ExportFrom _ ->
    []
  | Import { source = (_, module_ref); named; ns; _ } ->
    let module_ = module_of_module_ref ~resolved_modules ~root ~write_root module_ref in
    let named_import_declarations =
      let%bind (export_name, local) = SMap.elements named in
      let export = export_of_export_name export_name in
      let import = ImportDeclaration.ModuleExport ModuleExport.{ module_; export } in
      let%bind (name, locs) = SMap.elements local in
      let%bind { local_loc; _ } = Nel.to_list locs in
      let declaration = Declaration.{ loc = local_loc; name } in
      return ImportDeclaration.{ import; declaration }
    in
    let namespace_import_declarations =
      let import = ImportDeclaration.ModuleNamespace module_ in
      let%bind (loc, name) = Base.Option.to_list ns in
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

let source_of_exports ~root ~write_root ~loc_source ~type_sig ~resolved_modules ~reader =
  let Packed_type_sig.Module.{ module_kind; local_defs; remote_refs; module_refs; _ } = type_sig in
  let open Base.List.Let_syntax in
  let open Type_sig_pack in
  let module_ = Module.of_loc_source ~root ~write_root loc_source |> remove_dot_flow_suffix in
  let source_of_remote_ref = function
    | Import { index; remote; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_modules ~root ~write_root module_ref
      in
      let export = Export.Named remote in
      return (SourceOfExport.ModuleExport ModuleExport.{ module_; export })
    | ImportNs { index; _ } ->
      let module_ =
        let module_ref = Type_sig_collections.Module_refs.get module_refs index in
        module_of_module_ref ~resolved_modules ~root ~write_root module_ref
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
    | BuiltinRef { ref_loc; type_ref = _; name } ->
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
          module_of_module_ref ~resolved_modules ~root ~write_root module_ref
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
  object (_this)
    inherit
      [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper as super

    method on_loc_annot x = x

    method on_type_annot x = x

    method! t_identifier (((aloc, type_), Ast.Identifier.{ name; _ }) as ident) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      if
        Scope_builder.Api.is_local_use scope_info loc && Scope_builder.Api.use_is_def scope_info loc
      then
        add_var_info name loc type_;
      ident

    method! object_key_identifier (((aloc, type_), Ast.Identifier.{ name; _ }) as ident) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      add_member_info name loc type_;
      ident

    method! type_alias _loc alias =
      let Ast.Statement.TypeAlias.{ id = ((aloc, type_), Ast.Identifier.{ name; _ }); _ } = alias in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      add_type_info name loc type_;
      alias

    method! opaque_type _loc otype =
      let Ast.Statement.OpaqueType.{ id = ((aloc, type_), Ast.Identifier.{ name; _ }); _ } =
        otype
      in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      add_type_info name loc type_;
      otype

    method! interface _loc iface =
      let Ast.Statement.Interface.{ id = ((aloc, type_), Ast.Identifier.{ name; _ }); _ } = iface in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      add_type_info name loc type_;
      iface

    method! class_identifier (((aloc, type_), Ast.Identifier.{ name; _ }) as ident) =
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      add_type_info name loc type_;
      super#class_identifier ident

    method! enum_declaration enum =
      let open Ast.Statement.EnumDeclaration in
      let { id = ((aloc, type_), Ast.Identifier.{ name; _ }); body = (_, body); _ } = enum in
      let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
      add_type_info name loc type_;
      let defaulted_member (aloc, { DefaultedMember.id = (_, Ast.Identifier.{ name; _ }); _ }) =
        let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
        add_member_info name loc type_
      in
      let initialized_member (aloc, { InitializedMember.id = (_, Ast.Identifier.{ name; _ }); _ }) =
        let loc = Parsing_heaps.Reader.loc_of_aloc ~reader aloc in
        add_member_info name loc type_
      in
      (match body with
      | BooleanBody BooleanBody.{ members; _ } -> Base.List.iter members ~f:initialized_member
      | NumberBody NumberBody.{ members; _ } -> Base.List.iter members ~f:initialized_member
      | StringBody StringBody.{ members = Defaulted members; _ } ->
        Base.List.iter members ~f:defaulted_member
      | StringBody StringBody.{ members = Initialized members; _ } ->
        Base.List.iter members ~f:initialized_member
      | SymbolBody SymbolBody.{ members; _ } -> Base.List.iter members ~f:defaulted_member
      | BigIntBody BigIntBody.{ members; _ } -> Base.List.iter members ~f:initialized_member);
      super#enum_declaration enum
  end

let module_documentations ~root ~write_root ~ast ~file : Hh_json.json list =
  match (Module.of_file_key ~root ~write_root file, Find_documentation.module_doc_loc ast) with
  | (Module.File file, Some documentation) ->
    [ModuleDoc.{ documentation; file } |> ModuleDoc.to_json ~root ~write_root]
  | _ -> []

let declaration_infos
    ~root ~write_root ~glean_log ~scope_info ~file ~file_sig ~cx ~reader ~typed_ast ~ast =
  let infos = ref [] in
  let add_info kind name loc (type_ : Type.t) = infos := ((kind, name, loc), type_) :: !infos in
  let add_var_info = add_info `Declaration in
  let add_member_info = add_info `MemberDeclaration in
  let add_type_info = add_info `TypeDeclaration in
  ignore
    ((new declaration_info_collector
        ~scope_info
        ~reader
        ~add_var_info
        ~add_member_info
        ~add_type_info
     )
       #program
       typed_ast
    );
  let options = Ty_normalizer_env.default_options in
  let genv = Ty_normalizer_flow.mk_genv ~options ~cx ~typed_ast_opt:(Some typed_ast) ~file_sig in
  let exact_by_default = Context.exact_by_default cx in
  let docs_and_spans = DocumentationFullspanMap.create ast file in
  Base.List.fold
    (Ty_normalizer_flow.from_types
       ~f:(fun (_, _, loc) ->
         if glean_log then Utils_js.prerr_endlinef "normalizing: %s" (Reason.string_of_loc loc))
       genv
       !infos
    )
    ~init:([], [], [])
    ~f:(fun (var_infos, member_infos, type_infos) ((kind, name, loc), elt_result) ->
      let (documentation, span) =
        match Loc_sig.LocS.LMap.find_opt loc docs_and_spans with
        | None -> (None, None)
        | Some DocumentationFullspanMap.{ documentation; span = None } -> (documentation, Some loc)
        | Some DocumentationFullspanMap.{ documentation; span } -> (documentation, span)
      in
      match elt_result with
      | Error _ -> (var_infos, member_infos, type_infos)
      | Ok elt ->
        let type_ = Ty_printer.string_of_elt ~exact_by_default elt in
        (match kind with
        | `Declaration ->
          let declaration = Declaration.{ name; loc } in
          let var_info =
            DeclarationInfo.{ declaration; type_; documentation; span }
            |> DeclarationInfo.to_json ~root ~write_root
          in
          (var_info :: var_infos, member_infos, type_infos)
        | `MemberDeclaration ->
          let memberDeclaration = MemberDeclaration.{ name; loc } in
          let member_info =
            MemberDeclarationInfo.{ memberDeclaration; type_; documentation; span }
            |> MemberDeclarationInfo.to_json ~root ~write_root
          in
          (var_infos, member_info :: member_infos, type_infos)
        | `TypeDeclaration ->
          let typeDeclaration = TypeDeclaration.{ name; loc } in
          let type_info =
            TypeDeclarationInfo.{ typeDeclaration; type_; documentation; span }
            |> TypeDeclarationInfo.to_json ~root ~write_root
          in
          (var_infos, member_infos, type_info :: type_infos)))

let file_of_string_modules ~root ~write_root ~options ~file:file_key =
  let open Base.List.Let_syntax in
  let%bind file =
    match Module.of_file_key ~root ~write_root file_key |> remove_dot_flow_suffix with
    | Module.File file -> return file
    | _ -> []
  in
  let%bind string =
    match Module_js.exported_module ~options file_key ~package_info:None with
    | Some string -> return string
    | None -> []
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

(* Latest version of the 'all' schema supported by this indexer *)
let all_schema_version = 7

let flow_schema_version = 3

let create_typed_runner_config
    ~output_dir ~write_root ~include_direct_deps ~glean_log ~glean_timeout =
  (module struct
    type accumulator = {
      files_analyzed: int;
      json_filenames: SSet.t;
    }

    let check_options o = o

    let expand_roots ~env files =
      if include_direct_deps then
        Pure_dep_graph_operations.calc_direct_dependencies
          (Dependency_info.implementation_dependency_graph env.ServerEnv.dependency_info)
          files
      else
        files

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
      let Codemod_context.Typed.{ typed_ast; cx; file; file_sig; type_sig; _ } = ctx in
      if glean_log then Utils_js.prerr_endlinef "visiting: %s" (File_key.to_string file);
      let f () =
        let root = Options.root options in
        let reader = State_reader.create () in
        let resolved_modules =
          Parsing_heaps.get_file_addr_unsafe file
          |> Parsing_heaps.Reader.get_typed_parse_unsafe ~reader file
          |> Parsing_heaps.Reader.get_resolved_modules_unsafe
               ~reader
               Parsing_heaps.read_dependency
               file
        in
        let log msg =
          if glean_log then
            Utils_js.prerr_endlinef "%s: %s" msg (File_key.to_string file)
          else
            ()
        in
        log "scope info";
        let scope_info =
          Scope_builder.program ~enable_enums:(Options.enums options) ~with_types:false ast
        in
        log "module documentations";
        let module_documentation = module_documentations ~root ~write_root ~ast ~file in
        log "declaration info";
        let (declaration_info, member_declaration_info, type_declaration_info) =
          declaration_infos
            ~scope_info
            ~root
            ~write_root
            ~glean_log
            ~file
            ~file_sig
            ~cx
            ~reader
            ~typed_ast
            ~ast
        in
        log "type import declaration";
        let type_import_declaration =
          type_import_declarations ~root ~write_root ~resolved_modules ~file_sig
        in
        let loc_source = fst ast |> Loc.source in
        log "source of exports";
        let source_of_export =
          source_of_exports ~root ~write_root ~loc_source ~type_sig ~resolved_modules ~reader
        in
        log "source of type exports";
        let source_of_type_export =
          source_of_type_exports
            ~root
            ~write_root
            ~file
            ~reader
            ~loc_source
            ~type_sig
            ~resolved_modules
        in
        log "local declaration reference";
        let local_declaration_reference =
          local_declaration_references ~root ~write_root ~scope_info
        in
        log "import declaration";
        let import_declaration =
          import_declarations ~root ~write_root ~resolved_modules ~file_sig
        in
        log "member declaration reference";
        let member_declaration_reference =
          member_declaration_references ~root ~write_root ~reader ~cx ~typed_ast ~file_sig
        in
        log "type declaration reference";
        let type_declaration_reference =
          type_declaration_references ~root ~write_root ~reader ~cx ~typed_ast
        in
        log "file of string module";
        let file_of_string_module = file_of_string_modules ~root ~write_root ~options ~file in
        let file_lines = file_liness ~root ~write_root ~file in
        log "outputting";
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
        let flow_pred pred = Printf.sprintf "flow.%s.%d" pred flow_schema_version in
        output_facts (flow_pred "LocalDeclarationReference") local_declaration_reference;
        output_string out_channel ",";
        output_facts (flow_pred "DeclarationInfo") declaration_info;
        output_string out_channel ",";
        output_facts (flow_pred "SourceOfExport") source_of_export;
        output_string out_channel ",";
        output_facts (flow_pred "ImportDeclaration") import_declaration;
        output_string out_channel ",";
        output_facts (flow_pred "MemberDeclarationReference") member_declaration_reference;
        output_string out_channel ",";
        output_facts (flow_pred "MemberDeclarationInfo") member_declaration_info;
        output_string out_channel ",";
        output_facts (flow_pred "TypeDeclarationReference") type_declaration_reference;
        output_string out_channel ",";
        output_facts (flow_pred "TypeDeclarationInfo") type_declaration_info;
        output_string out_channel ",";
        output_facts (flow_pred "TypeImportDeclaration") type_import_declaration;
        output_string out_channel ",";
        output_facts (flow_pred "SourceOfTypeExport") source_of_type_export;
        output_string out_channel ",";
        output_facts (flow_pred "FileOfStringModule") file_of_string_module;
        output_string out_channel ",";
        output_facts (flow_pred "ModuleDoc") module_documentation;
        output_string out_channel ",";
        output_facts "src.FileLines.1" file_lines;
        close_out out_channel;
        log "done";
        { files_analyzed = 1; json_filenames = SSet.singleton output_file }
      in
      if glean_timeout > 0 then
        Timeout.with_timeout
          ~timeout:glean_timeout
          ~on_timeout:(fun _ ->
            failwith (Utils_js.spf "Timed out visiting: %s" (File_key.to_string file)))
          ~do_:(fun _ -> f ())
      else
        f ()
  end : Codemod_runner.SIMPLE_TYPED_RUNNER_CONFIG
  )

let make
    ~output_dir ~write_root ~include_direct_deps ~include_reachable_deps ~glean_log ~glean_timeout =
  let module C = ( val create_typed_runner_config
                         ~output_dir
                         ~write_root
                         ~include_direct_deps
                         ~glean_log
                         ~glean_timeout : Codemod_runner.SIMPLE_TYPED_RUNNER_CONFIG
                 )
  in
  if include_reachable_deps then
    (module Codemod_runner.MakeSimpleTypedTwoPassRunner (C) : Codemod_runner.RUNNABLE)
  else
    (module Codemod_runner.MakeSimpleTypedRunner (C) : Codemod_runner.RUNNABLE)
