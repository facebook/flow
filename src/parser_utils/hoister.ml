(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast

open Flow_ast_visitor

(* Hoister class. Does a shallow visit of statements, looking for binding
   declarations (currently, variable declarations, parameters, and function
   declarations) and recording the corresponding bindings in a list. The list
   can have duplicates, which are handled elsewhere.

   TODO: Ideally implemented as a fold, not a map.
*)
module Bindings: sig
  type t
  type entry = Loc.t Ast.Identifier.t
  val empty: t
  val singleton: entry -> t
  val add: entry -> t -> t
  val push: t -> t -> t
  val exists: (entry -> bool) -> t -> bool
  val to_assoc: t -> (string * Loc.t Nel.t) list
  val to_map: t -> Loc.t list SMap.t
end = struct
  type entry = Loc.t Ast.Identifier.t
  type t = entry list
  let empty = []
  let singleton x = [x]
  let add = List.cons
  let push = List.append
  let exists = List.exists
  let to_assoc t =
    let xs, map = List.fold_left (fun (xs, map) (loc, x) ->
      match SMap.get x map with
        | Some locs -> xs, SMap.add x (Nel.cons loc locs) map
        | None -> x::xs, SMap.add x (Nel.one loc) map
    ) ([], SMap.empty) (List.rev t) in
    List.rev_map (fun x -> x, Nel.rev @@ SMap.find x map) xs
  let to_map t =
    let map = List.fold_left (fun map (loc, x) ->
      match SMap.get x map with
        | Some locs -> SMap.add x (loc::locs) map
        | None -> SMap.add x [loc] map
    ) SMap.empty (List.rev t) in
    SMap.map List.rev map
end

(* TODO: It should be possible to vastly simplify hoisting by overriding the
   general method `pattern_identifier ?kind` for kind = Var | Let | Const that
   was recently introduced to distinguish bindings and assignments from other
   occurrences (`identifier`).

   Instead, it is implemented below by overriding several specific methods that
   are known to introduce bindings. The logic here is sufficiently tricky that
   we probably should not change it without extensive testing. *)

class hoister = object(this)
  inherit [Bindings.t] visitor ~init:Bindings.empty as super

  method private add_binding entry =
    (* `event` is a global in old IE and jsxmin lazily avoids renaming it. it
       should be safe to shadow it, i.e. `function(event){event.target}` can be
       renamed to `function(a){a.target}`, because code relying on the global
       would have to have written `function(){event.target}` or
       `function(event) {(event || window.event).target}`, both of which are
       compatible with renaming.

       TODO[jsxmin]: remove this. *)
    let _loc, x = entry in
    if x = "event" then () else
      this#update_acc (Bindings.add entry)

  (* Ignore expressions. This includes, importantly, function expressions (whose
     ids should not be hoisted). *)
  method! expression (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    expr

  (* Ignore assignment patterns, whose targets should not be hoisted. *)
  method! assignment_pattern (patt: (Loc.t, Loc.t) Ast.Pattern.t) =
    patt

  (* Ignore class declarations, since they are lexical bindings (thus not
     hoisted). *)
  method! class_ _loc (cls: (Loc.t, Loc.t) Ast.Class.t) =
    cls

  (* Ignore import declarations, since they are lexical bindings (thus not
     hoisted). *)
  method! import_declaration _loc (decl: (Loc.t, Loc.t) Ast.Statement.ImportDeclaration.t) =
    decl

  (* This is visited by function parameters, variable declarations, and catch patterns (but not
     assignment expressions). *)
  method! pattern ?kind (expr: (Loc.t, Loc.t) Ast.Pattern.t) =
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

  method! declare_variable loc (decl: (Loc.t, Loc.t) Ast.Statement.DeclareVariable.t) =
    let open Ast.Statement.DeclareVariable in
    this#add_binding decl.id;
    super#declare_variable loc decl

  method! declare_class loc (decl: (Loc.t, Loc.t) Ast.Statement.DeclareClass.t) =
    let open Ast.Statement.DeclareClass in
    this#add_binding decl.id;
    super#declare_class loc decl

  method! declare_function loc (decl: (Loc.t, Loc.t) Ast.Statement.DeclareFunction.t) =
    let open Ast.Statement.DeclareFunction in
    this#add_binding decl.id;
    super#declare_function loc decl

  method! function_declaration _loc (expr: (Loc.t, Loc.t) Ast.Function.t) =
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
  inherit [Bindings.t] visitor ~init:Bindings.empty as super

  method private add_binding entry =
    this#update_acc (Bindings.add entry)

  (* Ignore all statements except variable declarations, class declarations, and
     import declarations. The ignored statements cannot contain lexical
     bindings in the current scope. *)
  method! statement (stmt: (Loc.t, Loc.t) Ast.Statement.t) =
    let open Ast.Statement in
    match stmt with
    | (_, VariableDeclaration _)
    | (_, ClassDeclaration _)
    | (_, ExportNamedDeclaration _)
    | (_, ExportDefaultDeclaration _)
    | (_, ImportDeclaration _) -> super#statement stmt
    | _ -> stmt

  (* Ignore expressions. This includes, importantly, initializers of variable
     declarations. *)
  method! expression (expr: (Loc.t, Loc.t) Ast.Expression.t) =
    expr

  (* This is visited by variable declarations, as well as other kinds of
     patterns that we ignore. *)
  method! pattern ?kind (expr: (Loc.t, Loc.t) Ast.Pattern.t) =
    match kind with
    | None -> expr
    | Some (Ast.Statement.VariableDeclaration.Let | Ast.Statement.VariableDeclaration.Const) ->
      let open Ast.Pattern in
      let _, patt = expr in
      begin match patt with
      | Identifier { Identifier.name; _ } ->
        this#add_binding name
      | Object _
      | Array _
      | Assignment _ -> run (super#pattern ?kind) expr
      | _ -> ()
      end;
      expr
    | Some Ast.Statement.VariableDeclaration.Var -> expr

  method! class_ _loc (cls: (Loc.t, Loc.t) Ast.Class.t) =
    let open Ast.Class in
    let {
      id; body = _; tparams = _;
      extends = _; implements = _;
      classDecorators = _;
    } = cls in
    begin match id with
    | Some name ->
      this#add_binding name
    | None -> ()
    end;
    cls

  method! import_named_specifier
    (specifier: Loc.t Ast.Statement.ImportDeclaration.named_specifier) =
    let open Ast.Statement.ImportDeclaration in
    let binding = match specifier with
    | { local = Some binding; remote = _; kind = _ }
    | { local = None; remote = binding; kind = _ } ->
      binding
    in
    this#add_binding binding;
    specifier

  method! import_default_specifier (id: Loc.t Ast.Identifier.t) =
    this#add_binding id;
    id

  method! import_namespace_specifier _loc (id: Loc.t Ast.Identifier.t) =
    this#add_binding id;
    id

end
