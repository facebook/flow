(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Reason

let export_specifiers cx loc source export_kind =
  let open Ast.Statement in
  let module E = ExportNamedDeclaration in
  (* [declare] export [type] {foo [as bar]}; *)
  let export_ref loc local_type remote_name =
    match export_kind with
    | Ast.Statement.ExportType -> Import_export.export_type cx remote_name ~name_loc:loc local_type
    | Ast.Statement.ExportValue ->
      Import_export.export cx remote_name ~name_loc:loc ~is_type_only_export:false local_type
  in
  (* [declare] export [type] {foo [as bar]} from 'module' *)
  let export_from loc local_type remote_name =
    match export_kind with
    | Ast.Statement.ExportType -> Import_export.export_type cx remote_name ~name_loc:loc local_type
    | Ast.Statement.ExportValue ->
      Import_export.export cx remote_name ~name_loc:loc ~is_type_only_export:false local_type
  in
  let export_specifier export (_, { E.ExportSpecifier.local; exported }) =
    let ((local_loc, local_type), { Ast.Identifier.name = local_name; comments = _ }) = local in
    let remote_name =
      match exported with
      | None -> OrdinaryName local_name
      | Some (_, { Ast.Identifier.name = remote_name; comments = _ }) -> OrdinaryName remote_name
    in
    export local_loc local_type remote_name
  in
  function
  (* [declare] export [type] {foo [as bar]} [from ...]; *)
  | E.ExportSpecifiers specifiers ->
    let export =
      match source with
      | Some _ -> export_from
      | None -> export_ref
    in
    Base.List.iter ~f:(export_specifier export) specifiers
  (* [declare] export [type] * as id from "source"; *)
  | E.ExportBatchSpecifier (_, Some (_, { Ast.Identifier.name; _ })) ->
    let ((_, module_t), _) = Base.Option.value_exn source in
    let is_type_only_export =
      match export_kind with
      | Ast.Statement.ExportValue -> false
      | Ast.Statement.ExportType -> true
    in
    Import_export.export cx (OrdinaryName name) ~name_loc:loc ~is_type_only_export module_t
  (* [declare] export [type] * from "source"; *)
  | E.ExportBatchSpecifier (_, None) ->
    let ((_, module_t), _) = Base.Option.value_exn source in
    (match export_kind with
    | Ast.Statement.ExportValue -> Import_export.export_star cx loc module_t
    | Ast.Statement.ExportType -> Import_export.export_type_star cx loc module_t)

let visit_toplevel_statement cx : (ALoc.t, ALoc.t * Type.t) Ast.Statement.t -> unit =
  let open Ast.Statement in
  function
  | (_, Empty _)
  | (_, Block _)
  | (_, If _)
  | (_, Labeled _)
  | (_, Break _)
  | (_, Continue _)
  | (_, Throw _)
  | (_, Try _)
  | (_, With _)
  | (_, DeclareTypeAlias _)
  | (_, TypeAlias _)
  | (_, DeclareOpaqueType _)
  | (_, OpaqueType _)
  | (_, Switch _)
  | (_, Return _)
  | (_, While _)
  | (_, DoWhile _)
  | (_, For _)
  | (_, ForIn _)
  | (_, ForOf _)
  | (_, Debugger _)
  | (_, FunctionDeclaration _)
  | (_, ComponentDeclaration _)
  | (_, EnumDeclaration _)
  | (_, DeclareVariable _)
  | (_, DeclareFunction _)
  | (_, VariableDeclaration _)
  | (_, ClassDeclaration _)
  | (_, DeclareClass _)
  | (_, DeclareComponent _)
  | (_, DeclareEnum _)
  | (_, DeclareInterface _)
  | (_, InterfaceDeclaration _)
  | (_, DeclareModule _)
  | (_, DeclareNamespace _)
  | (_, ImportDeclaration _) ->
    ()
  | (_, Expression { Expression.expression = _; directive = _; comments = _ }) ->
    (* TODO: also handle cjs exports here *)
    ()
  | (loc, DeclareExportDeclaration decl) ->
    let module D = DeclareExportDeclaration in
    let { D.default; declaration; specifiers; source; comments = _ } = decl in
    let export_maybe_default_binding id =
      let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
      match default with
      | None -> Import_export.export cx (OrdinaryName name) ~name_loc ~is_type_only_export:false t
      | Some default_loc ->
        Import_export.export
          cx
          (OrdinaryName "default")
          ~name_loc:default_loc
          ~is_type_only_export:false
          t
    in
    let f = function
      | D.Variable (_, { DeclareVariable.id; _ }) ->
        let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
        Import_export.export cx (OrdinaryName name) ~name_loc ~is_type_only_export:false t
      | D.Function (_, f) -> export_maybe_default_binding f.DeclareFunction.id
      | D.Class (_, c) -> export_maybe_default_binding c.DeclareClass.id
      | D.Component (_, c) -> export_maybe_default_binding c.DeclareComponent.id
      | D.DefaultType ((_, t), _) ->
        let default_loc = Base.Option.value_exn default in
        Import_export.export
          cx
          (OrdinaryName "default")
          ~name_loc:default_loc
          ~is_type_only_export:true
          t
      | D.NamedType (_, { TypeAlias.id; _ })
      | D.NamedOpaqueType (_, { OpaqueType.id; _ })
      | D.Interface (_, { Interface.id; _ }) ->
        let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
        Import_export.export_type cx (OrdinaryName name) ~name_loc t
      | D.Enum (_, { EnumDeclaration.id; _ }) ->
        let ((name_loc, t), { Ast.Identifier.name; comments = _ }) = id in
        if Context.enable_enums cx then
          Import_export.export cx (OrdinaryName name) ~name_loc ~is_type_only_export:false t
    in
    Option.iter f declaration;
    let export_kind = Ast.Statement.ExportValue in
    Option.iter (export_specifiers cx loc source export_kind) specifiers
  | ( loc,
      DeclareModuleExports
        { Ast.Statement.DeclareModuleExports.annot = (_, ((_, t), _)); comments = _ }
    ) ->
    Import_export.cjs_clobber cx loc t
  | ( loc,
      ExportNamedDeclaration
        { ExportNamedDeclaration.declaration; specifiers; source; export_kind; comments = _ }
    ) ->
    let export_id ((name_loc, t), { Ast.Identifier.name; comments = _ }) =
      match export_kind with
      | Ast.Statement.ExportValue ->
        Import_export.export cx (OrdinaryName name) ~name_loc ~is_type_only_export:false t
      | Ast.Statement.ExportType -> Import_export.export_type cx (OrdinaryName name) ~name_loc t
    in
    let () =
      match declaration with
      | None -> ()
      | Some (_, stmt) -> begin
        match stmt with
        | FunctionDeclaration { Ast.Function.id = Some id; _ }
        | ClassDeclaration { Ast.Class.id = Some id; _ }
        | TypeAlias { TypeAlias.id; _ }
        | OpaqueType { OpaqueType.id; _ }
        | InterfaceDeclaration { Interface.id; _ }
        | ComponentDeclaration { ComponentDeclaration.id; _ }
        | EnumDeclaration { EnumDeclaration.id; _ } ->
          export_id id
        | VariableDeclaration { VariableDeclaration.declarations; _ } ->
          Flow_ast_utils.fold_bindings_of_variable_declarations
            (fun _ () id -> export_id id)
            ()
            declarations
        | _ -> (* Parser Error: Invalid export-declaration type! *) ()
      end
    in
    Option.iter (export_specifiers cx loc source export_kind) specifiers
  | ( _,
      ExportDefaultDeclaration
        { ExportDefaultDeclaration.default = (_, t); declaration; comments = _ }
    ) ->
    let module D = ExportDefaultDeclaration in
    let export_loc =
      match declaration with
      | D.Declaration (loc, stmt) ->
        (match stmt with
        | FunctionDeclaration { Ast.Function.id = Some ((id_loc, _), _); _ }
        | ClassDeclaration { Ast.Class.id = Some ((id_loc, _), _); _ }
        | EnumDeclaration { EnumDeclaration.id = ((id_loc, _), _); _ }
        | ComponentDeclaration { ComponentDeclaration.id = ((id_loc, _), _); _ } ->
          id_loc
        | _ -> loc)
      | D.Expression ((loc, _), _) -> loc
    in
    Import_export.export
      cx
      (OrdinaryName "default")
      ~name_loc:export_loc
      ~is_type_only_export:false
      t

let visit_program cx (_, { Flow_ast.Program.statements; _ }) =
  Base.List.iter ~f:(visit_toplevel_statement cx) statements
