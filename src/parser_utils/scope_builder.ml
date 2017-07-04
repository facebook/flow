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

class with_or_eval_visitor = object(this)
  inherit [bool] visitor ~init:false as super

  method! expression (expr: Ast.Expression.t) =
    let open Ast.Expression in
    if this#acc = true then expr else match expr with
    | (_, Call { Call.callee = (_, Identifier (_, "eval")); _}) ->
      this#set_acc true;
      expr
    | _ -> super#expression expr

  method! statement (stmt: Ast.Statement.t) =
    if this#acc = true then stmt else super#statement stmt

  method! with_ (stuff: Ast.Statement.With.t) =
    this#set_acc true;
    stuff
end

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

(* Walker class that prepares renamings for bindings, hoisting bindings one
   scope at a time.

   We do not generate the scope tree for the entire program, because it is not
   clear where to hang scopes for function expressions, catch clauses,
   etc. One possibility is to augment the AST with scope identifiers.

   As we move into a nested scope, we generate bindings for the new scope, map
   the bindings to names generated by a factory, and augment the existing
   environment with this map before visiting the nested scope.

   Because globals can appear deep in the program, we cannot actually perform
   any renaming until we have walked the entire program. Instead, we compute (1)
   a map from identifier locations to positions in a name stream and (2) a map
   from positions in the name stream to globals they conflict with. Later, we
   generate names (avoiding conflicts with globals) and rename those locations.
*)
module Acc = struct
  type t = {
    (* map of identifier locations to positions in the name stream *)
    renamings: (Loc.t * int) LocMap.t;
    (* map of positions in the name stream to globals they conflict with *)
    globals: SSet.t IMap.t;
    (* [derivable] maximum number of positions in name stream *)
    max_counter: int;
  }
  let init = {
    max_counter = 0;
    globals = IMap.empty;
    renamings = LocMap.empty;
  }
end
class scope_builder = object(this)
  inherit [Acc.t] visitor ~init:Acc.init as super

  val mutable env = SMap.empty

  val mutable counter = 0
  method private next =
    let result = counter in
    counter <- counter + 1;
    this#update_acc (fun acc -> Acc.{ acc with
      max_counter = max counter acc.max_counter
    });
    result

  method private mk_env =
    List.fold_left (fun map (loc, x) ->
      match SMap.get x map with
      | Some _ -> map
      | None -> SMap.add x (loc, this#next) map
    ) SMap.empty

  method private push bindings =
    let save_counter = counter in
    let old_env = env in
    env <- SMap.fold SMap.add (this#mk_env (List.rev bindings)) old_env;
    old_env, save_counter

  method private pop (old_env, save_counter) =
    env <- old_env;
    counter <- save_counter

  method with_bindings: 'a. Bindings.t -> ('a -> 'a) -> 'a -> 'a = fun bindings visit node ->
    let saved_state = this#push bindings in
    let node' = visit node in
    this#pop saved_state;
    node'

  method private add_global x (_loc, i) =
    this#update_acc (fun acc -> Acc.{ acc with
      globals =
        let iglobals = try IMap.find_unsafe i acc.globals with _ -> SSet.empty in
        IMap.add i (SSet.add x iglobals) acc.globals
    })

  method private add_renaming loc (def_loc, i) =
    this#update_acc (fun acc -> Acc.{ acc with
      renamings = LocMap.add loc (def_loc, i) acc.renamings
    })

  (* catch params for which their catch blocks introduce bindings, and those
     bindings conflict with the catch params *)
  val mutable bad_catch_params = []

  method! identifier (expr: Ast.Identifier.t) =
    let loc, x = expr in
    begin match SMap.get x env with
      | Some (def_loc, i) -> this#add_renaming loc (def_loc, i)
      | None -> SMap.iter (fun _ -> this#add_global x) env
    end;
    expr

  (* don't rename the `foo` in `x.foo` *)
  method! member_property_identifier (id: Ast.Identifier.t) = id

  (* don't rename the `foo` in `{ foo: ... }` *)
  method! object_key_identifier (id: Ast.Identifier.t) = id

  method! block (stmt: Ast.Statement.Block.t) =
    let lexical_hoist = new lexical_hoister in
    let lexical_bindings = lexical_hoist#eval lexical_hoist#block stmt in
    this#with_bindings lexical_bindings super#block stmt

  method! for_in_statement (stmt: Ast.Statement.ForIn.t) =
    let open Ast.Statement.ForIn in
    let { left; right = _; body = _; each = _ } = stmt in

    let lexical_hoist = new lexical_hoister in
    let lexical_bindings = match left with
    | LeftDeclaration (_, decl) ->
      lexical_hoist#eval lexical_hoist#variable_declaration decl
    | _ -> []
    in
    this#with_bindings lexical_bindings super#for_in_statement stmt

  method! for_of_statement (stmt: Ast.Statement.ForOf.t) =
    let open Ast.Statement.ForOf in
    let { left; right = _; body = _; async = _ } = stmt in

    let lexical_hoist = new lexical_hoister in
    let lexical_bindings = match left with
    | LeftDeclaration (_, decl) ->
      lexical_hoist#eval lexical_hoist#variable_declaration decl
    | _ -> []
    in
    this#with_bindings lexical_bindings super#for_of_statement stmt

  method! for_statement (stmt: Ast.Statement.For.t) =
    let open Ast.Statement.For in
    let { init; test = _; update = _; body = _ } = stmt in

    let lexical_hoist = new lexical_hoister in
    let lexical_bindings = match init with
    | Some (InitDeclaration (_, decl)) ->
      lexical_hoist#eval lexical_hoist#variable_declaration decl
    | _ -> []
    in
    this#with_bindings lexical_bindings super#for_statement stmt

  method! catch_clause (clause: Ast.Statement.Try.CatchClause.t') =
    let open Ast.Statement.Try.CatchClause in
    let { param; body = _ } = clause in

    this#with_bindings (
      let open Ast.Pattern in
      let _, patt = param in
      match patt with
      | Identifier { Identifier.name; _ } ->
        let loc, x = name in
        if List.mem loc bad_catch_params then [] else [loc, x]
      | _ -> (* TODO *)
        []
    ) super#catch_clause clause

  method! function_declaration (expr: Ast.Function.t) =
    let contains_with_or_eval =
      let visit = new with_or_eval_visitor in
      visit#eval visit#function_declaration expr
    in

    if not contains_with_or_eval then begin
      let open Ast.Function in
      let {
        id; params; body; async = _; generator = _; expression = _;
        predicate = _; returnType = _; typeParameters = _;
      } = expr in

      run_opt this#identifier id;

      (* hoisting *)
      let hoist = new hoister in
      begin
        let param_list, _rest = params in
        run_list hoist#function_param_pattern param_list;
        match body with
        | BodyBlock (_loc, block) ->
          run hoist#block block
        | _ ->
          ()
      end;

      (* pushing *)
      let saved_bad_catch_params = bad_catch_params in
      bad_catch_params <- hoist#bad_catch_params;
      let saved_state = this#push hoist#acc in

      let (param_list, rest) = params in
      run_list this#function_param_pattern param_list;
      run_opt this#function_rest_element rest;

      begin match body with
        | BodyBlock (_, block) ->
          run this#block block;
        | BodyExpression expr ->
          run this#expression expr;
      end;

      (* popping *)
      this#pop saved_state;
      bad_catch_params <- saved_bad_catch_params;
    end;

    expr

  (* Almost the same as function_declaration, except that the name of the
     function expression is locally in scope. *)
  method! function_ (expr: Ast.Function.t) =
    let contains_with_or_eval =
      let visit = new with_or_eval_visitor in
      visit#eval visit#function_ expr
    in

    if not contains_with_or_eval then begin
      let open Ast.Function in
      let {
        id; params; body; async = _; generator = _; expression = _;
        predicate = _; returnType = _; typeParameters = _;
      } = expr in

      (* pushing *)
      let saved_state = this#push (match id with Some (loc, x) -> [loc, x] | None -> []) in
      run_opt this#identifier id;

      (* hoisting *)
      let hoist = new hoister in
      begin
        let param_list, _rest = params in
        run_list hoist#function_param_pattern param_list;
        match body with
        | BodyBlock (_loc, block) ->
          run hoist#block block
        | _ ->
          ()
      end;

      (* more pushing *)
      let saved_bad_catch_params = bad_catch_params in
      bad_catch_params <- hoist#bad_catch_params;
      let _saved_state = this#push hoist#acc in

      let (param_list, rest) = params in
      run_list this#function_param_pattern param_list;
      run_opt this#function_rest_element rest;

      begin match body with
        | BodyBlock (_, block) ->
          run this#block block
        | BodyExpression expr ->
          run this#expression expr
      end;

      (* popping *)
      this#pop saved_state;
      bad_catch_params <- saved_bad_catch_params;
    end;

    expr
end

let program ?(ignore_toplevel=false) program =
  let walk = new scope_builder in
  if ignore_toplevel then walk#eval walk#program program
  else
    let hoist = new hoister in
    let bindings = hoist#eval hoist#program program in
    let lexical_hoist = new lexical_hoister in
    let lexical_bindings = lexical_hoist#eval lexical_hoist#program program in
    walk#eval (walk#with_bindings (lexical_bindings @ bindings) walk#program) program
