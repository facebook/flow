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

(* Subclass of the AST visitor class that calculates requires. Initializes with
   the scope builder class.
*)
class requires_calculator ~ast = object(this)
  inherit [Loc.t SMap.t] visitor ~init:SMap.empty as super

  val locals =
    let { Scope_api.locals; _ } = Scope_builder.program ast in
    locals

  method private add_require s loc =
    this#update_acc (SMap.add s loc)

  method! call (expr: Ast.Expression.Call.t) =
    let open Ast.Expression in
    let { Call.callee; arguments } = expr in
    begin match callee, arguments with
    | ((_, Identifier (loc, "require")),
       [Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })])
      ->
      if not (LocMap.mem loc locals)
      then this#add_require v require_loc
    | ((_, Identifier (loc, "requireLazy")),
       [Expression (_, Array ({ Array.elements })); Expression (_);])
      ->
      let element = function
        | Some (Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })) ->
          if not (LocMap.mem loc locals)
          then this#add_require v require_loc
        | _ -> () in
      List.iter element elements
    | _ -> ()
    end;
    super#call expr

  method! import (expr: Ast.Expression.t) =
    let open Ast.Expression in
    begin match expr with
    | require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ } ->
      this#add_require v require_loc
    | _ -> ()
    end;
    super#expression expr

  method! import_declaration (decl: Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { importKind = _; source; specifiers = _ } = decl in
    begin match source with
    | require_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ } ->
      this#add_require v require_loc
    | _ -> ()
    end;
    super#import_declaration decl

  method! export_named_declaration (decl: Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind = _; source; specifiers = _; declaration = _ } = decl in
    begin match source with
    | Some (require_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      this#add_require v require_loc
    | _ -> ()
    end;
    super#export_named_declaration decl

  method! declare_export_declaration (decl: Ast.Statement.DeclareExportDeclaration.t) =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default = _; source; specifiers = _; declaration = _ } = decl in
    begin match source with
    | Some (require_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      this#add_require v require_loc
    | _ -> ()
    end;
    super#declare_export_declaration decl
end

let program ~ast =
  let walk = new requires_calculator ~ast in
  walk#eval walk#program ast
