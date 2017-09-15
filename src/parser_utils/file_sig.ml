(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Flow_ast_visitor

module LocMap = Utils_js.LocMap

type t = {
  module_sig: module_sig;
  declare_modules: (Loc.t * module_sig) SMap.t
}

and module_sig = {
  requires: require SMap.t;
  module_kind: module_kind;
  type_exports: Loc.t SMap.t;
}

and require = {
  loc: Loc.t;
  cjs_requires: Loc.t list;
  es_imports: Loc.t list;
}

and module_kind =
  | CommonJS of { clobbered: Loc.t option }
  | ES of { named: Loc.t SMap.t; batch: Loc.t SMap.t }

let empty_module_sig = {
  requires = SMap.empty;
  module_kind = CommonJS { clobbered = None };
  type_exports = SMap.empty;
}

let empty_file_sig = {
  module_sig = empty_module_sig;
  declare_modules = SMap.empty;
}

let require_loc_map msig =
  SMap.fold (fun name {loc; _} acc ->
    SMap.add name loc acc
  ) msig.requires SMap.empty

let add_declare_module name m loc fsig = {
  fsig with
  declare_modules = SMap.add name (loc, m) fsig.declare_modules;
}

let update_sig f fsig = { fsig with module_sig = f fsig.module_sig }

let set_module_kind module_kind msig = { msig with module_kind }

let add_cjs_require name loc msig =
  let require =
    match SMap.get name msig.requires with
    | Some r -> { r with loc; cjs_requires = loc::r.cjs_requires }
    | None -> { loc; cjs_requires = [loc]; es_imports = [] }
  in
  { msig with requires = SMap.add name require msig.requires;
}

let add_es_import name loc msig =
  let require =
    match SMap.get name msig.requires with
    | Some r -> { r with loc; es_imports = loc::r.es_imports }
    | None -> { loc; cjs_requires = []; es_imports = [loc] }
  in
  { msig with requires = SMap.add name require msig.requires }

let add_type_export name loc msig = {
  msig with
  type_exports = SMap.add name loc msig.type_exports;
}

let add_es_exports named_bindings batch_bindings msig =
  let named, batch = match msig.module_kind with
  | CommonJS _ -> SMap.empty, SMap.empty
  | ES { named; batch } -> named, batch
  in
  let named = List.fold_left (fun acc (loc, x) ->
    SMap.add x loc acc
  ) named named_bindings in
  let batch = List.fold_left (fun acc (loc, x) ->
    SMap.add x loc acc
  ) batch batch_bindings in
  set_module_kind (ES { named; batch }) msig

(* Subclass of the AST visitor class that calculates requires. Initializes with
   the scope builder class.
*)
class requires_calculator ~ast = object(this)
  inherit [t] visitor ~init:empty_file_sig as super

  val locals =
    let { Scope_api.locals; _ } = Scope_builder.program ast in
    locals

  val mutable curr_declare_module: module_sig option = None;

  method private update_module_sig f =
    match curr_declare_module with
    | Some m ->
      curr_declare_module <- Some (f m)
    | None ->
      this#update_acc (update_sig f)

  method private add_cjs_require r loc =
    this#update_module_sig (add_cjs_require r loc)

  method private add_es_import r loc =
    this#update_module_sig (add_es_import r loc)

  method private add_type_export name loc =
    this#update_module_sig (add_type_export name loc)

  method private add_es_exports named_bindings batch_bindings =
    this#update_module_sig (add_es_exports named_bindings batch_bindings)

  method private set_module_kind module_kind =
    this#update_module_sig (set_module_kind module_kind)

  method! call (expr: Loc.t Ast.Expression.Call.t) =
    let open Ast.Expression in
    let { Call.callee; arguments } = expr in
    begin match callee, arguments with
    | ((_, Identifier (loc, "require")),
       [Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })])
      ->
      if not (LocMap.mem loc locals)
      then this#add_cjs_require v require_loc
    | ((_, Identifier (loc, "requireLazy")),
       [Expression (_, Array ({ Array.elements })); Expression (_);])
      ->
      let element = function
        | Some (Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })) ->
          if not (LocMap.mem loc locals)
          then this#add_cjs_require v require_loc
        | _ -> () in
      List.iter element elements
    | _ -> ()
    end;
    super#call expr

  method! import (expr: Loc.t Ast.Expression.t) =
    let open Ast.Expression in
    begin match expr with
    | import_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ } ->
      this#add_es_import v import_loc
    | _ -> ()
    end;
    super#expression expr

  method! import_declaration (decl: Loc.t Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { importKind = _; source; specifiers = _ } = decl in
    begin match source with
    | import_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ } ->
      this#add_es_import v import_loc
    | _ -> ()
    end;
    super#import_declaration decl

  method! export_default_declaration_decl (decl: Loc.t Ast.Statement.ExportDefaultDeclaration.declaration) =
    let open Ast.Statement.ExportDefaultDeclaration in
    begin match decl with
    | Declaration (loc, _)
    | Expression (loc, _) ->
      this#add_es_exports [loc, "default"] []
    end;
    super#export_default_declaration_decl  decl

  method! export_named_declaration (decl: Loc.t Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind = _; source; specifiers; declaration} = decl in
    begin match source with
    | Some (import_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      this#add_es_import v import_loc
    | _ -> ()
    end;
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some (loc, stmt) ->
      let open Ast.Statement in
      match stmt with
      | FunctionDeclaration { Ast.Function.id = Some id; _ }
      | ClassDeclaration { Ast.Class.id = Some id; _ } ->
        this#add_es_exports [loc, snd id] []
      | VariableDeclaration { VariableDeclaration.declarations = decls; _ } ->
        let bindings = Ast_utils.bindings_of_variable_declarations decls in
        this#add_es_exports bindings []
      | TypeAlias { TypeAlias.id; _ }
      | OpaqueType { OpaqueType.id; _ }
      | InterfaceDeclaration { Interface.id; _ } ->
        this#add_type_export (snd id) loc;
      | _ -> failwith "unsupported declaration"
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      this#export_specifiers source specifiers
    end;
    super#export_named_declaration decl

  method! declare_module_exports loc (annot: Loc.t Ast.Type.annotation) =
    this#set_module_kind (CommonJS { clobbered = Some loc });
    super#declare_module_exports loc annot

  method! declare_export_declaration (decl: Loc.t Ast.Statement.DeclareExportDeclaration.t) =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default; source; specifiers; declaration } = decl in
    begin match source with
    | Some (import_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      this#add_es_import v import_loc
    | _ -> ()
    end;
    begin match declaration with
    | None -> () (* assert specifiers <> None *)
    | Some declaration ->
      let open Ast.Statement in
      match declaration with
      | Variable (loc, { DeclareVariable.id; _ })
      | Function (loc, { DeclareFunction.id; _ })
      | Class (loc, { Interface.id; _ }) ->
        let name = if default then "default" else snd id in
        this#add_es_exports [loc, name] []
      | DefaultType (loc, _) ->
        this#add_es_exports [loc, "default"] []
      | NamedType (loc, { TypeAlias.id; _ })
      | NamedOpaqueType (loc, { OpaqueType.id; _ })
      | Interface (loc, { Interface.id; _ }) ->
        let name = if default then "default" else snd id in
        this#add_type_export name loc
    end;
    begin match specifiers with
    | None -> () (* assert declaration <> None *)
    | Some specifiers ->
      assert (not default);
      this#export_specifiers source specifiers
    end;
    super#declare_export_declaration decl

  method! assignment (expr: Loc.t Ast.Expression.Assignment.t) =
    let open Ast.Expression in
    let open Ast.Expression.Assignment in
    (* module.exports = e *)
    let { operator; left; _ } = expr in
    begin match operator, left with
    | Assign, (assign_loc, Ast.Pattern.Expression (_, Member { Member.
        _object = module_loc, Ast.Expression.Identifier (_, "module");
        property = Member.PropertyIdentifier (_, "exports"); _
      })) ->
      (* expressions not allowed in declare module body *)
      assert (curr_declare_module = None);
      if not (LocMap.mem module_loc locals)
      then this#set_module_kind (CommonJS { clobbered = Some assign_loc })
    | _ -> ()
    end;
    super#assignment expr

  method! declare_module loc (m: Loc.t Ast.Statement.DeclareModule.t) =
    let name = Ast.Statement.DeclareModule.(match m.id with
    | Identifier (_, name) -> name
    | Literal (_, { Ast.Literal.value = Ast.Literal.String name; _ }) -> name
    | Literal _ -> failwith "declare module literal id must be a string"
    ) in
    curr_declare_module <- Some (empty_module_sig);
    let ret = super#declare_module loc m in
    begin match curr_declare_module with
    | None -> failwith "lost curr_declare_module"
    | Some m -> this#update_acc (add_declare_module name m loc)
    end;
    curr_declare_module <- None;
    ret

  method private export_specifiers source =
    let open Ast.Statement.ExportNamedDeclaration in
    function
    | ExportBatchSpecifier (loc, Some (_, name)) ->
      this#add_es_exports [loc, name] []
    | ExportBatchSpecifier (loc, None) ->
      let require = match source with
      | Some (_, { Ast.Literal.value = Ast.Literal.String v; _ }) -> v
      | _ -> failwith "batch export missing source"
      in
      this#add_es_exports [] [loc, require]
    | ExportSpecifiers specs ->
      let bindings = Ast_utils.bindings_of_export_specifiers specs in
      this#add_es_exports bindings []
end

let program ~ast =
  let walk = new requires_calculator ~ast in
  walk#eval walk#program ast
