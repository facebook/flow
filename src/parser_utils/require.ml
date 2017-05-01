(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Mapper class that performs actual renaming. Uses the walker class above.
*)
class mapper is_react = object
  inherit Flow_ast_mapper.mapper as super

  (* TODO: these are not really mutable, they're re-initialized below *)
  val mutable renamings = Scope_builder.LocMap.empty
  val mutable requires = SMap.empty
  method requires =
    requires

  method! program (program: Ast.program) =
    let walk = new Scope_builder.walker in
    let hoist = new Scope_builder.hoister in
    ignore (hoist#program program);
    let saved_state = walk#push hoist#bindings in
    let _ = walk#program program in
    walk#pop saved_state;
    renamings <- walk#renamings;
    super#program program

  method! call (expr: Ast.Expression.Call.t) =
    let open Ast.Expression in
    let { Call.callee; arguments } = expr in
    begin match callee, arguments with
    | ((_, Identifier (loc, "require")),
       [Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })])
      ->
      if not (Scope_builder.LocMap.mem loc renamings)
      then requires <- SMap.add v require_loc requires
    | ((_, Identifier (loc, "requireLazy")),
       [Expression (_, Array ({ Array.elements })); Expression (_);])
      ->
      let element = function
        | Some (Expression (require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ })) ->
          if not (Scope_builder.LocMap.mem loc renamings)
          then requires <- SMap.add v require_loc requires
        | _ -> () in
      List.iter element elements
    | _ -> ()
    end;
    super#call expr

  method! import (expr: Ast.Expression.t) =
    let open Ast.Expression in
    begin match expr with
    | require_loc, Literal { Ast.Literal.value = Ast.Literal.String v; raw = _ } ->
      requires <- SMap.add v require_loc requires
    | _ -> ()
    end;
    super#expression expr

  method! import_declaration (decl: Ast.Statement.ImportDeclaration.t) =
    let open Ast.Statement.ImportDeclaration in
    let { importKind = _; source; specifiers = _ } = decl in
    begin match source with
    | require_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ } ->
      requires <- SMap.add v require_loc requires
    | _ -> ()
    end;
    super#import_declaration decl

  method! export_named_declaration (decl: Ast.Statement.ExportNamedDeclaration.t) =
    let open Ast.Statement.ExportNamedDeclaration in
    let { exportKind = _; source; specifiers = _; declaration = _ } = decl in
    begin match source with
    | Some (require_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      requires <- SMap.add v require_loc requires
    | _ -> ()
    end;
    super#export_named_declaration decl

  method! declare_export_declaration (decl: Ast.Statement.DeclareExportDeclaration.t) =
    let open Ast.Statement.DeclareExportDeclaration in
    let { default = _; source; specifiers = _; declaration = _ } = decl in
    begin match source with
    | Some (require_loc, { Ast.Literal.value = Ast.Literal.String v; raw = _ }) ->
      requires <- SMap.add v require_loc requires
    | _ -> ()
    end;
    super#declare_export_declaration decl

  method! jsx_element (expr: Ast.JSX.element) =
    let open Ast.JSX in
    let require_loc, { Opening.name; _ } = expr.openingElement in
    begin match name with
    | Identifier (_, { Identifier.name }) ->
      if name = "fbt" then ()
      else begin
        if is_react then requires <- SMap.add "react" require_loc requires
      end
    | _ -> ()
    end;
    super#jsx_element expr

end
