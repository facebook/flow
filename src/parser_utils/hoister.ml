(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


module LocMap = Map.Make (Loc)
open Flow_ast_visitor

(* Hoister class. Does a shallow visit of statements, looking for binding
   declarations (currently, variable declarations, parameters, and function
   declarations) and recording the corresponding bindings in a list. The list
   can have duplicates, which are handled elsewhere.

   TODO: Ideally implemented as a fold, not a map.
*)
module Bindings = struct
  type t = (Loc.t * string) list
end
class hoister = object(this)
  inherit [Bindings.t] visitor ~init:[] as super

  method private add_binding (loc, x) =
    (* `event` is a global in old IE and jsxmin lazily avoids renaming it. it
       should be safe to shadow it, i.e. `function(event){event.target}` can be
       renamed to `function(a){a.target}`, because code relying on the global
       would have to have written `function(){event.target}` or
       `function(event) {(event || window.event).target}`, both of which are
       compatible with renaming.

       TODO[jsxmin]: remove this. *)
    if x = "event" then () else
      this#update_acc (List.cons (loc, x))

  val mutable bad_catch_params = []
  method bad_catch_params = bad_catch_params

  (* Ignore expressions. This includes, importantly, function expressions (whose
     ids should not be hoisted) and assignment expressions (whose targets should
     not be hoisted). *)
  method! expression (expr: Ast.Expression.t) =
    expr

  (* The scoping rule for catch clauses is special. Hoisting for the current
     scope continues in catch blocks, but the catch pattern also introduces a
     local scope. *)
  method! catch_clause (clause: Ast.Statement.Try.CatchClause.t') =
    let open Ast.Statement.Try.CatchClause in
    let { param; body } = clause in
    let saved_bindings = this#acc in
    this#set_acc [];
    let _, block = body in
    let _ = this#block block in
    let local_bindings = this#acc in
    let open Ast.Pattern in
    let _, patt = param in
    begin match patt with
    | Identifier { Identifier.name; _ } ->
      let loc, x = name in
      if List.exists (fun (_loc, x') -> x = x') local_bindings
      then bad_catch_params <- loc :: bad_catch_params
    | _ -> ();
    end;
    this#set_acc (local_bindings @ saved_bindings);
    clause

  (* Ignore class declarations, since they are lexical bindings (thus not
     hoisted). *)
  method! class_ (cls: Ast.Class.t) =
    cls

  (* Ignore import declarations, since they are lexical bindings (thus not
     hoisted). *)
  method! import_declaration (decl: Ast.Statement.ImportDeclaration.t) =
    decl

  (* This is visited by function parameters and variable declarations (but not
     assignment expressions or catch patterns). *)
  method! pattern ?kind (expr: Ast.Pattern.t) =
    match Utils.unsafe_opt kind with
    | Ast.Statement.VariableDeclaration.Var ->
      let open Ast.Pattern in
      let _, patt = expr in
      begin match patt with
      | Identifier { Identifier.name; _ } ->
        this#add_binding name
      | Object _
      | Array _
      | Assignment _ -> run (super#pattern ?kind) expr
      | Expression _ -> ()
      end;
      expr
    | Ast.Statement.VariableDeclaration.Let | Ast.Statement.VariableDeclaration.Const ->
      expr (* don't hoist let/const bindings *)

  method! function_declaration (expr: Ast.Function.t) =
    let open Ast.Function in
    let { id; _ } = expr in
    begin match id with
    | Some name ->
      this#add_binding name
    | None -> ()
    end;
    expr

end

class lexical_hoister = object(this)
  inherit [Bindings.t] visitor ~init:[] as super

  method private add_binding (loc, x) =
    this#update_acc (List.cons (loc, x))

  (* Ignore all statements except variable declarations, class declarations, and
     import declarations. The ignored statements cannot contain lexical
     bindings in the current scope. *)
  method! statement (stmt: Ast.Statement.t) =
    let open Ast.Statement in
    match stmt with
    | (_, VariableDeclaration _)
    | (_, ClassDeclaration _)
    | (_, ImportDeclaration _) -> super#statement stmt
    | _ -> stmt

  (* Ignore expressions. This includes, importantly, initializers of variable
     declarations. *)
  method! expression (expr: Ast.Expression.t) =
    expr

  (* This is visited by variable declarations. *)
  method! variable_declarator_pattern ~kind (expr: Ast.Pattern.t) =
    match kind with
    | Ast.Statement.VariableDeclaration.Let | Ast.Statement.VariableDeclaration.Const ->
      let open Ast.Pattern in
      let _, patt = expr in
      begin match patt with
      | Identifier { Identifier.name; _ } ->
        this#add_binding name
      | Object _
      | Array _
      | Assignment _ -> run (super#variable_declarator_pattern ~kind) expr
      | _ -> ()
      end;
      expr
    | Ast.Statement.VariableDeclaration.Var -> expr

  method! class_ (cls: Ast.Class.t) =
    let open Ast.Class in
    let {
      id; body = _; superClass = _;
      typeParameters = _; superTypeParameters = _; implements = _; classDecorators = _;
    } = cls in
    begin match id with
    | Some name ->
      this#add_binding name
    | None -> ()
    end;
    cls

  method! import_named_specifier ~ident (local: Ast.Identifier.t option) =
    this#add_binding ident;
    local

  method! import_default_specifier (id: Ast.Identifier.t) =
    this#add_binding id;
    id

  method! import_namespace_specifier (id: Ast.Identifier.t) =
    this#add_binding id;
    id

end
