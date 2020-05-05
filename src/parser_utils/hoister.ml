(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
module Bindings : sig
  type 'loc t

  type 'loc entry = ('loc, 'loc) Ast.Identifier.t

  val empty : 'loc t

  val singleton : 'loc entry -> 'loc t

  val add : 'loc entry -> 'loc t -> 'loc t

  val push : 'loc t -> 'loc t -> 'loc t

  val exists : ('loc entry -> bool) -> 'loc t -> bool

  val to_assoc : 'loc t -> (string * 'loc Nel.t) list

  val to_map : 'loc t -> 'loc list SMap.t
end = struct
  type 'loc entry = ('loc, 'loc) Ast.Identifier.t

  type 'loc t = 'loc entry list

  let empty = []

  let singleton x = [x]

  let add = List.cons

  let push = List.append

  let exists = List.exists

  let to_assoc t =
    let (xs, map) =
      List.fold_left
        (fun (xs, map) (loc, { Ast.Identifier.name = x; comments = _ }) ->
          match SMap.find_opt x map with
          | Some locs -> (xs, SMap.add x (Nel.cons loc locs) map)
          | None -> (x :: xs, SMap.add x (Nel.one loc) map))
        ([], SMap.empty)
        (List.rev t)
    in
    List.rev_map (fun x -> (x, Nel.rev @@ SMap.find x map)) xs

  let to_map t =
    let map =
      List.fold_left
        (fun map (loc, { Ast.Identifier.name = x; comments = _ }) ->
          match SMap.find_opt x map with
          | Some locs -> SMap.add x (loc :: locs) map
          | None -> SMap.add x [loc] map)
        SMap.empty
        (List.rev t)
    in
    SMap.map List.rev map
end

(* TODO: It should be possible to vastly simplify hoisting by overriding the
   general method `pattern_identifier ?kind` for kind = Var | Let | Const that
   was recently introduced to distinguish bindings and assignments from other
   occurrences (`identifier`).

   Instead, it is implemented below by overriding several specific methods that
   are known to introduce bindings. The logic here is sufficiently tricky that
   we probably should not change it without extensive testing. *)

class ['loc] hoister =
  object (this)
    inherit ['loc Bindings.t, 'loc] visitor ~init:Bindings.empty as super

    method private add_binding entry = this#update_acc (Bindings.add entry)

    (* Ignore expressions. This includes, importantly, function expressions (whose
     ids should not be hoisted). *)
    method! expression (expr : ('loc, 'loc) Ast.Expression.t) = expr

    (* Ignore assignment patterns, whose targets should not be hoisted. *)
    method! assignment_pattern (patt : ('loc, 'loc) Ast.Pattern.t) = patt

    (* Ignore class declarations, since they are lexical bindings (thus not
     hoisted). *)
    method! class_ _loc (cls : ('loc, 'loc) Ast.Class.t) = cls

    (* Ignore import declarations, since they are lexical bindings (thus not
     hoisted). *)
    method! import_declaration _loc (decl : ('loc, 'loc) Ast.Statement.ImportDeclaration.t) = decl

    (* This is visited by function parameters, variable declarations, and catch patterns (but not
     assignment expressions). *)
    method! pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      match Utils.unsafe_opt kind with
      | Ast.Statement.VariableDeclaration.Var ->
        let open Ast.Pattern in
        let (_, patt) = expr in
        begin
          match patt with
          | Identifier { Identifier.name; _ } -> this#add_binding name
          | Object _
          | Array _ ->
            run (super#pattern ?kind) expr
          | Expression _ -> ()
        end;
        expr
      | Ast.Statement.VariableDeclaration.Let
      | Ast.Statement.VariableDeclaration.Const ->
        expr

    (* don't hoist let/const bindings *)
    method! declare_variable loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
      let open Ast.Statement.DeclareVariable in
      this#add_binding decl.id;
      super#declare_variable loc decl

    method! declare_class loc (decl : ('loc, 'loc) Ast.Statement.DeclareClass.t) =
      let open Ast.Statement.DeclareClass in
      this#add_binding decl.id;
      super#declare_class loc decl

    method! declare_function loc (decl : ('loc, 'loc) Ast.Statement.DeclareFunction.t) =
      let open Ast.Statement.DeclareFunction in
      this#add_binding decl.id;
      super#declare_function loc decl

    method! function_declaration _loc (expr : ('loc, 'loc) Ast.Function.t) =
      let open Ast.Function in
      let { id; _ } = expr in
      begin
        match id with
        | Some name -> this#add_binding name
        | None -> ()
      end;
      expr

    method! type_alias loc (alias : ('loc, 'loc) Ast.Statement.TypeAlias.t) =
      let open Ast.Statement.TypeAlias in
      this#add_binding alias.id;
      super#type_alias loc alias

    method! opaque_type loc (alias : ('loc, 'loc) Ast.Statement.OpaqueType.t) =
      let open Ast.Statement.OpaqueType in
      this#add_binding alias.id;
      super#opaque_type loc alias

    method! interface loc (interface : ('loc, 'loc) Ast.Statement.Interface.t) =
      let open Ast.Statement.Interface in
      this#add_binding interface.id;
      super#interface loc interface
  end

class ['loc] lexical_hoister =
  object (this)
    inherit ['loc Bindings.t, 'loc] visitor ~init:Bindings.empty as super

    method private add_binding entry = this#update_acc (Bindings.add entry)

    (* Ignore all statements except variable declarations, class declarations, and
     import declarations. The ignored statements cannot contain lexical
     bindings in the current scope. *)
    method! statement (stmt : ('loc, 'loc) Ast.Statement.t) =
      let open Ast.Statement in
      match stmt with
      | (_, VariableDeclaration _)
      | (_, ClassDeclaration _)
      | (_, ExportNamedDeclaration _)
      | (_, ExportDefaultDeclaration _)
      | (_, ImportDeclaration _) ->
        super#statement stmt
      | _ -> stmt

    (* Ignore expressions. This includes, importantly, initializers of variable
     declarations. *)
    method! expression (expr : ('loc, 'loc) Ast.Expression.t) = expr

    (* This is visited by variable declarations, as well as other kinds of
     patterns that we ignore. *)
    method! pattern ?kind (expr : ('loc, 'loc) Ast.Pattern.t) =
      match kind with
      | None -> expr
      | Some (Ast.Statement.VariableDeclaration.Let | Ast.Statement.VariableDeclaration.Const) ->
        let open Ast.Pattern in
        let (_, patt) = expr in
        begin
          match patt with
          | Identifier { Identifier.name; _ } -> this#add_binding name
          | Object _
          | Array _ ->
            run (super#pattern ?kind) expr
          | _ -> ()
        end;
        expr
      | Some Ast.Statement.VariableDeclaration.Var -> expr

    method! class_ _loc (cls : ('loc, 'loc) Ast.Class.t) =
      let open Ast.Class in
      let {
        id;
        body = _;
        tparams = _;
        extends = _;
        implements = _;
        classDecorators = _;
        comments = _;
      } =
        cls
      in
      begin
        match id with
        | Some name -> this#add_binding name
        | None -> ()
      end;
      cls

    method! import_named_specifier
        (specifier : ('loc, 'loc) Ast.Statement.ImportDeclaration.named_specifier) =
      let open Ast.Statement.ImportDeclaration in
      let binding =
        match specifier with
        | { local = Some binding; remote = _; kind = _ }
        | { local = None; remote = binding; kind = _ } ->
          binding
      in
      this#add_binding binding;
      specifier

    method! import_default_specifier (id : ('loc, 'loc) Ast.Identifier.t) =
      this#add_binding id;
      id

    method! import_namespace_specifier _loc (id : ('loc, 'loc) Ast.Identifier.t) =
      this#add_binding id;
      id
  end
