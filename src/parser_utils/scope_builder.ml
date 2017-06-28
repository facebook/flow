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

class with_or_eval_mapper result_ref = object
  inherit Flow_ast_mapper.mapper as super

  method! expression (expr: Ast.Expression.t) =
    let open Ast.Expression in
    if !result_ref = true then expr else match expr with
    | (_, Call { Call.callee = (_, Identifier (_, "eval")); _}) ->
      result_ref := true;
      expr
    | _ -> super#expression expr

  method! statement (stmt: Ast.Statement.t) =
    if !result_ref = true then stmt else super#statement stmt

  method! with_ (stuff: Ast.Statement.With.t) =
    result_ref := true;
    stuff
end

(* Hoister class. Does a shallow visit of statements, looking for binding
   declarations (currently, variable declarations, parameters, and function
   declarations) and recording the corresponding bindings in a list. The list
   can have duplicates, which are handled elsewhere.

   TODO: Ideally implemented as a fold, not a map.
*)
class hoister = object(this)
  inherit Flow_ast_mapper.mapper

  val mutable bindings = []
  method private add_binding (loc, x) =
    (* `event` is a global in old IE and jsxmin lazily avoids renaming it. it
       should be safe to shadow it, i.e. `function(event){event.target}` can be
       renamed to `function(a){a.target}`, because code relying on the global
       would have to have written `function(){event.target}` or
       `function(event) {(event || window.event).target}`, both of which are
       compatible with renaming.

       TODO[jsxmin]: remove this. *)
    if x = "event" then () else
    bindings <- (loc, x) :: bindings

  (* result *)
  method bindings =
    List.rev bindings

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
    let saved_bindings = bindings in
    bindings <- [];
    let _, block = body in
    let _ = this#block block in
    let local_bindings = bindings in
    let open Ast.Pattern in
    let _, patt = param in
    begin match patt with
    | Identifier { Identifier.name; _ } ->
      let loc, x = name in
      if List.exists (fun (_loc, x') -> x = x') local_bindings
      then bad_catch_params <- loc :: bad_catch_params
    | _ -> ();
    end;
    bindings <- local_bindings @ saved_bindings;
    clause

  (* Ignore let/const declarations. *)
  method! lexical_variable_declarator_pattern (expr: Ast.Pattern.t) =
    expr

  (* Ignore class declarations. *)
  method! class_ (cls: Ast.Class.t) =
    cls

  (* This is visited by function parameters and var declarations (but not
     assignment expressions or catch patterns or let/const declarations). *)
  method! pattern (expr: Ast.Pattern.t) =
    let open Ast.Pattern in
    let _, patt = expr in
    begin match patt with
    | Identifier { Identifier.name; _ } ->
      this#add_binding name
    | _ -> (* TODO *)
      ()
    end;
    expr

  method! function_declaration (expr: Ast.Function.t) =
    let open Ast.Function in
    let { id; _ } = expr in
    begin match id with
    | Some name ->
      this#add_binding name
    | None -> (* TODO *)
      ()
    end;
    expr

end

class lexical_hoister = object(this)
  inherit Flow_ast_mapper.mapper as super

  val mutable bindings = []
  method private add_binding (loc, x) =
    bindings <- (loc, x) :: bindings

  (* result *)
  method bindings =
    List.rev bindings

  (* Ignore all statements except variable declarations and class
     declarations. *)
  method! statement (stmt: Ast.Statement.t) =
    let open Ast.Statement in
    match stmt with
    | (_, VariableDeclaration _)
    | (_, ClassDeclaration _) -> super#statement stmt
    | _ -> stmt

  (* Ignore expressions. This includes, importantly, initializers of variable
     declarations. *)
  method! expression (expr: Ast.Expression.t) =
    expr

  (* Ignore var declarations. *)
  method! variable_declarator_pattern (expr: Ast.Pattern.t) =
    expr

  (* This is visited by let and const declarations. *)
  method! lexical_variable_declarator_pattern (expr: Ast.Pattern.t) =
    let open Ast.Pattern in
    let _, patt = expr in
    begin match patt with
    | Identifier { Identifier.name; _ } ->
      this#add_binding name
    | _ -> (* TODO *)
      ()
    end;
    expr

  method! class_ (cls: Ast.Class.t) =
    let open Ast.Class in
    let {
      id; body = _; superClass = _;
      typeParameters = _; superTypeParameters = _; implements = _; classDecorators = _;
    } = cls in
    begin match id with
    | Some name ->
      this#add_binding name
    | None -> (* TODO *)
      ()
    end;
    cls

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
class walker = object(this)
  inherit Flow_ast_mapper.mapper as super

  val mutable env = SMap.empty

  val mutable counter = 0
  val mutable max_counter = 0
  method max_counter = max_counter
  method private next =
    let result = counter in
    counter <- counter + 1;
    max_counter <- max counter max_counter;
    result

  method private mk_env =
    List.fold_left (fun map (loc, x) ->
      match SMap.get x map with
      | Some _ -> map
      | None -> SMap.add x (loc, this#next) map
    ) SMap.empty

  method push bindings =
    let save_counter = counter in
    let old_env = env in
    env <- SMap.fold SMap.add (this#mk_env bindings) old_env;
    old_env, save_counter

  method pop (old_env, save_counter) =
    env <- old_env;
    counter <- save_counter

  (* map of identifier locations to positions in the name stream *)
  val mutable renamings = LocMap.empty
  method renamings = renamings

  (* map of positions in the name stream to globals they conflict with *)
  val mutable globals = IMap.empty
  method private add_global x (_loc, i) =
    let iglobals = try IMap.find_unsafe i globals with _ -> SSet.empty in
    globals <- IMap.add i (SSet.add x iglobals) globals
  method globals = globals

  (* catch params for which their catch blocks introduce bindings, and those
     bindings conflict with the catch params *)
  val mutable bad_catch_params = []

  method! identifier (expr: Ast.Identifier.t) =
    let loc, x = expr in
    begin match SMap.get x env with
    | Some (def_loc, i) -> renamings <- LocMap.add loc (def_loc, i) renamings
    | None -> SMap.iter (fun _ -> this#add_global x) env
    end;
    expr

  (* don't rename the `foo` in `x.foo` *)
  method! member_property_identifier (id: Ast.Identifier.t) = id

  (* don't rename the `foo` in `{ foo: ... }` *)
  method! object_key_identifier (id: Ast.Identifier.t) = id

  method! block (stmt: Ast.Statement.Block.t) =
    let lexical_hoist = new lexical_hoister in
    ignore (lexical_hoist#block stmt);
    let saved_state = this#push lexical_hoist#bindings in
    ignore (super#block stmt);
    this#pop saved_state;
    stmt

  method! catch_clause (clause: Ast.Statement.Try.CatchClause.t') =
    let open Ast.Statement.Try.CatchClause in
    let { param; body } = clause in

    (* pushing *)
    let saved_state = this#push (
      let open Ast.Pattern in
      let _, patt = param in
      match patt with
      | Identifier { Identifier.name; _ } ->
        let loc, x = name in
        if List.mem loc bad_catch_params then [] else [loc, x]
      | _ ->
        []
    ) in

    let _ = this#pattern param in
    let _, block = body in
    let _ = this#block block in

    this#pop saved_state;
    clause

  method! function_declaration (expr: Ast.Function.t) =
    let contains_with_or_eval =
      let result = ref false in
      let mapper = new with_or_eval_mapper result in
      let _ = mapper#function_declaration expr in
      !result
    in

    if not contains_with_or_eval then begin
      let open Ast.Function in
      let {
        id; params; body; async = _; generator = _; expression = _;
        predicate = _; returnType = _; typeParameters = _;
      } = expr in

      ignore (Flow_ast_mapper.opt this#identifier id);

      (* hoisting *)
      let hoist = new hoister in
      begin
        let param_list, _ = params in
        List.iter (fun p -> ignore (hoist#pattern p)) param_list;
        match body with
        | BodyBlock (_loc, block) ->
          ignore (hoist#block block)
        | _ ->
          ()
      end;

      (* pushing *)
      let saved_bad_catch_params = bad_catch_params in
      bad_catch_params <- hoist#bad_catch_params;
      let saved_state = this#push hoist#bindings in

      let (param_list, rest) = params in
      List.iter (fun p -> ignore (this#pattern p)) param_list;
      ignore (Flow_ast_mapper.opt this#function_rest_element rest);

      begin match body with
        | BodyBlock (_, block) ->
          ignore (this#block block);
        | BodyExpression expr ->
          ignore (this#expression expr);
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
      let result = ref false in
      let mapper = new with_or_eval_mapper result in
      let _ = mapper#function_ expr in
      !result
    in

    if not contains_with_or_eval then begin
      let open Ast.Function in
      let {
        id; params; body; async = _; generator = _; expression = _;
        predicate = _; returnType = _; typeParameters = _;
      } = expr in

      (* pushing *)
      let saved_state = this#push (match id with Some (loc, x) -> [loc, x] | None -> []) in
      ignore (Flow_ast_mapper.opt this#identifier id);

      (* hoisting *)
      let hoist = new hoister in
      begin
        let param_list, _ = params in
        List.iter (fun p -> ignore (hoist#pattern p)) param_list;
        match body with
        | BodyBlock (_loc, block) ->
          ignore (hoist#block block)
        | _ ->
          ()
      end;

      (* more pushing *)
      let saved_bad_catch_params = bad_catch_params in
      bad_catch_params <- hoist#bad_catch_params;
      let _saved_state = this#push hoist#bindings in

      let (param_list, rest) = params in
      List.iter (fun p -> ignore (this#pattern p)) param_list;
      ignore (Flow_ast_mapper.opt this#function_rest_element rest);

      begin match body with
        | BodyBlock (_, block) ->
          ignore (this#block block)
        | BodyExpression expr ->
          ignore (this#expression expr)
      end;

      (* popping *)
      this#pop saved_state;
      bad_catch_params <- saved_bad_catch_params;
    end;

    expr
end

let program ?(ignore_toplevel=false) (program: Ast.program) =
  let walk = new walker in
  let f () = ignore (walk#program program) in
  begin
    if ignore_toplevel then f ()
    else
      let hoist = new hoister in
      ignore (hoist#program program);
      let lexical_hoist = new lexical_hoister in
      ignore (lexical_hoist#program program);
      let saved_state = walk#push (lexical_hoist#bindings @ hoist#bindings) in
      f ();
      walk#pop saved_state
  end;
  walk
