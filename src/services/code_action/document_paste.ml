(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

class imported_def_collector ~scope ~ranges =
  object (this)
    inherit [Loc.t] Flow_ast_mapper.mapper

    val mutable import_def_locs : (string * Loc.t Nel.t) list = []

    method import_def_locs = import_def_locs

    method private loc_within_range l =
      Base.List.exists ranges ~f:(fun range -> Loc.contains range l)

    method private collect_relevant_def_loc_of_imported_identifier use_loc =
      if this#loc_within_range use_loc then
        match Scope_api.With_Loc.def_of_use_opt scope use_loc with
        | Some { Scope_api.With_Loc.Def.name = _; locs; kind = _; actual_name } ->
          (* All uses within the range, but definition outside of the range is potential
           * unbound import after paste candidate. In import_information_extractor, we will
           * further filter it down to imports. *)
          if not (Nel.exists this#loc_within_range locs) then
            import_def_locs <- (actual_name, locs) :: import_def_locs
        | None -> ()

    method! identifier ((loc, _) as id) =
      this#collect_relevant_def_loc_of_imported_identifier loc;
      id
  end

class import_information_extractor ~cx ~loc_of_aloc ~relevant_imported_defs =
  object (_this)
    inherit [ALoc.t, ALoc.t * Type.t, ALoc.t, ALoc.t * Type.t] Flow_polymorphic_ast_mapper.mapper

    val mutable import_items = []

    method import_items = import_items

    method on_loc_annot l = l

    method on_type_annot l = l

    method! import_declaration _loc decl =
      let open Ast.Statement.ImportDeclaration in
      let { import_kind; source; specifiers; default; comments = _ } = decl in
      let lazy_import_source_info =
        lazy
          (let ((_, source_t), { Ast.StringLiteral.value; _ }) = source in
           match TypeUtil.def_loc_of_t source_t |> ALoc.source with
           | Some f ->
             if Context.file cx = f || File_key.is_lib_file f then
               (value, false)
             else
               (File_key.to_string f, true)
           | None -> (value, false)
          )
      in
      let collect ~import_type ~remote ~local_opt =
        let ((l, _), { Ast.Identifier.name; comments = _ }) =
          Base.Option.value ~default:remote local_opt
        in
        if
          Base.List.exists relevant_imported_defs ~f:(fun (n, locs) ->
              name = n && Nel.mem ~equal:Loc.equal (loc_of_aloc l) locs
          )
        then
          let (import_source, import_source_is_resolved) = Lazy.force lazy_import_source_info in
          let name_of_id (_, { Ast.Identifier.name; comments = _ }) = name in
          let import_item =
            {
              Lsp.DocumentPaste.remote_name = name_of_id remote;
              local_name = Option.map name_of_id local_opt;
              import_type;
              import_source;
              import_source_is_resolved;
            }
          in
          import_items <- import_item :: import_items
      in
      Base.Option.iter specifiers ~f:(function
          | ImportNamedSpecifiers specifiers ->
            Base.List.iter specifiers ~f:(fun { kind; local; remote; remote_name_def_loc = _ } ->
                let import_type =
                  match Base.Option.value ~default:import_kind kind with
                  | ImportType -> Lsp.DocumentPaste.ImportNamedType
                  | ImportTypeof -> Lsp.DocumentPaste.ImportNamedTypeOf
                  | ImportValue -> Lsp.DocumentPaste.ImportNamedValue
                in
                collect ~import_type ~remote ~local_opt:local
            )
          | ImportNamespaceSpecifier (_, id) ->
            (match import_kind with
            | ImportType -> ()
            | ImportTypeof ->
              collect
                ~import_type:Lsp.DocumentPaste.ImportTypeOfAsNamespace
                ~remote:id
                ~local_opt:None
            | ImportValue ->
              collect
                ~import_type:Lsp.DocumentPaste.ImportValueAsNamespace
                ~remote:id
                ~local_opt:None)
          );
      Base.Option.iter default ~f:(fun { identifier; remote_default_name_def_loc = _ } ->
          let import_type =
            match import_kind with
            | ImportType -> Lsp.DocumentPaste.ImportNamedType
            | ImportTypeof -> Lsp.DocumentPaste.ImportNamedTypeOf
            | ImportValue -> Lsp.DocumentPaste.ImportNamedValue
          in
          collect
            ~import_type
            ~remote:(fst identifier, { Ast.Identifier.name = "default"; comments = None })
            ~local_opt:(Some identifier)
      );
      decl
  end

let prepare_document_paste cx ~loc_of_aloc ~ast ~typed_ast ~ranges =
  let relevant_imported_defs =
    let collector =
      new imported_def_collector
        ~scope:(Scope_builder.program ~enable_enums:(Context.enable_enums cx) ~with_types:true ast)
        ~ranges
    in
    ignore @@ collector#program ast;
    collector#import_def_locs
  in
  let extractor = new import_information_extractor ~cx ~loc_of_aloc ~relevant_imported_defs in
  ignore @@ extractor#program typed_ast;
  extractor#import_items

let provide_document_paste_edits ~layout_options ~module_system_info ~src_dir ast import_items =
  let scope_info = Scope_builder.program ~enable_enums:true ~with_types:true ast in
  let module_scope_defs =
    Scope_api.With_Loc.toplevel_scopes
    |> Base.List.fold ~init:SSet.empty ~f:(fun acc scope_id ->
           let { Scope_api.With_Loc.Scope.defs; _ } =
             Scope_api.With_Loc.scope scope_info scope_id
           in
           Base.List.fold ~init:acc ~f:(fun acc s -> SSet.add s acc) (SMap.keys defs)
       )
  in
  let added_imports =
    Base.List.filter_map
      import_items
      ~f:(fun
           {
             Lsp.DocumentPaste.remote_name;
             local_name;
             import_type;
             import_source;
             import_source_is_resolved;
           }
         ->
        if SSet.mem (Base.Option.value ~default:remote_name local_name) module_scope_defs then
          (* If the name is already bound locally, then we won't try to import it here.
           * The already bound name might not be the import, but error on the side of not
           * introducing duplicate bindings. *)
          None
        else
          match
            Lsp_import_edits.from_of_source
              ~module_system_info
              ~src_dir
              ( if import_source_is_resolved then
                Export_index.File_key (File_key.SourceFile import_source)
              else
                Export_index.Builtin import_source
              )
          with
          | None -> None
          | Some from ->
            (match import_type with
            | Lsp.DocumentPaste.ImportNamedType ->
              if remote_name = "default" then
                Some (from, Autofix_imports.DefaultType (Base.Option.value_exn local_name))
              else
                Some (from, Autofix_imports.(NamedType [{ remote_name; local_name }]))
            | Lsp.DocumentPaste.ImportNamedTypeOf ->
              if remote_name = "default" then
                Some (from, Autofix_imports.DefaultTypeof (Base.Option.value_exn local_name))
              else
                Some (from, Autofix_imports.(NamedTypeof [{ remote_name; local_name }]))
            | Lsp.DocumentPaste.ImportNamedValue ->
              if remote_name = "default" then
                Some (from, Autofix_imports.Default (Base.Option.value_exn local_name))
              else
                Some (from, Autofix_imports.(Named [{ remote_name; local_name }]))
            | Lsp.DocumentPaste.ImportTypeOfAsNamespace ->
              Some (from, Autofix_imports.TypeofNamespace remote_name)
            | Lsp.DocumentPaste.ImportValueAsNamespace ->
              Some (from, Autofix_imports.Namespace remote_name))
    )
  in
  Autofix_imports.add_imports ~options:layout_options ~added_imports ast
