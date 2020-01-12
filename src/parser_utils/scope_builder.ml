(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Ast = Flow_ast
open Flow_ast_visitor
open Hoister

module Make (L : Loc_sig.S) (Api : Scope_api_sig.S with module L = L) :
  Scope_builder_sig.S with module L = L and module Api = Api = struct
  module L = L
  module Api = Api
  open Api

  class with_or_eval_visitor =
    object (this)
      inherit [bool, L.t] visitor ~init:false as super

      method! expression (expr : (L.t, L.t) Ast.Expression.t) =
        Ast.Expression.(
          if this#acc = true then
            expr
          else
            match expr with
            | ( _,
                Call
                  {
                    Call.callee = (_, Identifier (_, { Ast.Identifier.name = "eval"; comments = _ }));
                    _;
                  } ) ->
              this#set_acc true;
              expr
            | _ -> super#expression expr)

      method! statement (stmt : (L.t, L.t) Ast.Statement.t) =
        if this#acc = true then
          stmt
        else
          super#statement stmt

      method! with_ _loc (stuff : (L.t, L.t) Ast.Statement.With.t) =
        this#set_acc true;
        stuff
    end

  (* Visitor class that prepares use-def info, hoisting bindings one scope at a
     time. This info can be used for various purposes, e.g. variable renaming.

     We do not generate the scope tree for the entire program, because it is not
     clear where to hang scopes for function expressions, catch clauses,
     etc. One possibility is to augment the AST with scope identifiers.

     As we move into a nested scope, we generate bindings for the new scope, map
     the bindings to names generated by a factory, and augment the existing
     environment with this map before visiting the nested scope.
  *)
  module Acc = struct
    type t = info

    let init = { max_distinct = 0; scopes = IMap.empty }
  end

  module Env : sig
    type t

    val empty : t

    val mk_env : (unit -> int) -> t -> L.t Bindings.t -> t

    val get : string -> t -> Def.t option

    val defs : t -> Def.t SMap.t
  end = struct
    type t = Def.t SMap.t list

    let empty = []

    let rec get x t =
      match t with
      | [] -> None
      | hd :: rest ->
        begin
          match SMap.find_opt x hd with
          | Some def -> Some def
          | None -> get x rest
        end

    let defs = function
      | [] -> SMap.empty
      | hd :: _ -> hd

    let mk_env next parent_env bindings =
      let bindings = Bindings.to_assoc bindings in
      let env =
        List.fold_left
          (fun env (x, locs) ->
            let name =
              match get x parent_env with
              | Some def -> def.Def.name
              | None -> next ()
            in
            SMap.add x { Def.locs; name; actual_name = x } env)
          SMap.empty
          bindings
      in
      env :: parent_env
  end

  class scope_builder =
    object (this)
      inherit [Acc.t, L.t] visitor ~init:Acc.init as super

      val mutable env = Env.empty

      val mutable current_scope_opt = None

      val mutable scope_counter = 0

      val mutable uses = []

      method private new_scope =
        let new_scope = scope_counter in
        scope_counter <- scope_counter + 1;
        new_scope

      val mutable counter = 0

      method private next =
        let result = counter in
        counter <- counter + 1;
        this#update_acc (fun acc -> { acc with max_distinct = max counter acc.max_distinct });
        result

      method with_bindings : 'a. ?lexical:bool -> L.t -> L.t Bindings.t -> ('a -> 'a) -> 'a -> 'a =
        fun ?(lexical = false) loc bindings visit node ->
          let save_counter = counter in
          let save_uses = uses in
          let old_env = env in
          let parent = current_scope_opt in
          let child = this#new_scope in
          uses <- [];
          current_scope_opt <- Some child;
          env <- Env.mk_env (fun () -> this#next) old_env bindings;
          let result = Base.Result.try_with (fun () -> visit node) in
          this#update_acc (fun acc ->
              let defs = Env.defs env in
              let locals =
                SMap.fold
                  (fun _ def locals ->
                    Nel.fold_left (fun locals loc -> L.LMap.add loc def locals) locals def.Def.locs)
                  defs
                  L.LMap.empty
              in
              let (locals, globals) =
                List.fold_left
                  (fun (locals, globals) (loc, { Ast.Identifier.name = x; comments = _ }) ->
                    match Env.get x env with
                    | Some def -> (L.LMap.add loc def locals, globals)
                    | None -> (locals, SSet.add x globals))
                  (locals, SSet.empty)
                  uses
              in
              let scopes =
                IMap.add child { Scope.lexical; parent; defs; locals; globals; loc } acc.scopes
              in
              { acc with scopes });
          uses <- save_uses;
          current_scope_opt <- parent;
          env <- old_env;
          counter <- save_counter;
          Base.Result.ok_exn result

      method! identifier (expr : (L.t, L.t) Ast.Identifier.t) =
        uses <- expr :: uses;
        expr

      method! jsx_identifier (id : L.t Ast.JSX.Identifier.t) =
        Ast.JSX.Identifier.(
          let (loc, { name }) = id in
          uses <- Flow_ast_utils.ident_of_source (loc, name) :: uses;
          id)

      (* don't rename the `foo` in `x.foo` *)
      method! member_property_identifier (id : (L.t, L.t) Ast.Identifier.t) = id

      (* don't rename the `foo` in `const {foo: bar} = x` *)
      method! pattern_object_property_identifier_key ?kind id =
        ignore kind;
        id

      (* don't rename the `foo` in `{ foo: ... }` *)
      method! object_key_identifier (id : (L.t, L.t) Ast.Identifier.t) = id

      method! block loc (stmt : (L.t, L.t) Ast.Statement.Block.t) =
        let lexical_hoist = new lexical_hoister in
        let lexical_bindings = lexical_hoist#eval (lexical_hoist#block loc) stmt in
        this#with_bindings ~lexical:true loc lexical_bindings (super#block loc) stmt

      (* like block *)
      method! program (program : (L.t, L.t) Ast.program) =
        let (loc, _, _) = program in
        let lexical_hoist = new lexical_hoister in
        let lexical_bindings = lexical_hoist#eval lexical_hoist#program program in
        this#with_bindings ~lexical:true loc lexical_bindings super#program program

      method private scoped_for_in_statement loc (stmt : (L.t, L.t) Ast.Statement.ForIn.t) =
        super#for_in_statement loc stmt

      method! for_in_statement loc (stmt : (L.t, L.t) Ast.Statement.ForIn.t) =
        Ast.Statement.ForIn.(
          let { left; right = _; body = _; each = _ } = stmt in
          let lexical_hoist = new lexical_hoister in
          let lexical_bindings =
            match left with
            | LeftDeclaration (loc, decl) ->
              lexical_hoist#eval (lexical_hoist#variable_declaration loc) decl
            | LeftPattern _ -> Bindings.empty
          in
          this#with_bindings
            ~lexical:true
            loc
            lexical_bindings
            (this#scoped_for_in_statement loc)
            stmt)

      method private scoped_for_of_statement loc (stmt : (L.t, L.t) Ast.Statement.ForOf.t) =
        super#for_of_statement loc stmt

      method! for_of_statement loc (stmt : (L.t, L.t) Ast.Statement.ForOf.t) =
        Ast.Statement.ForOf.(
          let { left; right = _; body = _; async = _ } = stmt in
          let lexical_hoist = new lexical_hoister in
          let lexical_bindings =
            match left with
            | LeftDeclaration (loc, decl) ->
              lexical_hoist#eval (lexical_hoist#variable_declaration loc) decl
            | LeftPattern _ -> Bindings.empty
          in
          this#with_bindings
            ~lexical:true
            loc
            lexical_bindings
            (this#scoped_for_of_statement loc)
            stmt)

      method private scoped_for_statement loc (stmt : (L.t, L.t) Ast.Statement.For.t) =
        super#for_statement loc stmt

      method! for_statement loc (stmt : (L.t, L.t) Ast.Statement.For.t) =
        Ast.Statement.For.(
          let { init; test = _; update = _; body = _ } = stmt in
          let lexical_hoist = new lexical_hoister in
          let lexical_bindings =
            match init with
            | Some (InitDeclaration (loc, decl)) ->
              lexical_hoist#eval (lexical_hoist#variable_declaration loc) decl
            | _ -> Bindings.empty
          in
          this#with_bindings ~lexical:true loc lexical_bindings (this#scoped_for_statement loc) stmt)

      method! catch_clause loc (clause : (L.t, L.t) Ast.Statement.Try.CatchClause.t') =
        Ast.Statement.Try.CatchClause.(
          let { param; body = _; comments = _ } = clause in
          (* hoisting *)
          let lexical_bindings =
            match param with
            | Some p ->
              let lexical_hoist = new lexical_hoister in
              lexical_hoist#eval lexical_hoist#catch_clause_pattern p
            | None -> Bindings.empty
          in
          this#with_bindings ~lexical:true loc lexical_bindings (super#catch_clause loc) clause)

      (* helper for function params and body *)
      method private lambda loc params body =
        (* function params and bindings within the function body share the same scope *)
        let bindings =
          let hoist = new hoister in
          run hoist#function_params params;
          run hoist#function_body_any body;
          hoist#acc
        in
        this#with_bindings
          loc
          bindings
          (fun () ->
            run this#function_params params;
            run this#function_body_any body)
          ()

      method! function_declaration loc (expr : (L.t, L.t) Ast.Function.t) =
        let contains_with_or_eval =
          let visit = new with_or_eval_visitor in
          visit#eval (visit#function_declaration loc) expr
        in
        if not contains_with_or_eval then (
          Ast.Function.(
            let {
              id;
              params;
              body;
              async = _;
              generator = _;
              predicate = _;
              return = _;
              tparams = _;
              sig_loc = _;
            } =
              expr
            in
            run_opt this#function_identifier id;

            this#lambda loc params body)
        );

        expr

      (* Almost the same as function_declaration, except that the name of the
       function expression is locally in scope. *)
      method! function_ loc (expr : (L.t, L.t) Ast.Function.t) =
        let contains_with_or_eval =
          let visit = new with_or_eval_visitor in
          visit#eval (visit#function_ loc) expr
        in
        ( if not contains_with_or_eval then
          Ast.Function.(
            let {
              id;
              params;
              body;
              async = _;
              generator = _;
              predicate = _;
              return = _;
              tparams = _;
              sig_loc = _;
            } =
              expr
            in
            let bindings =
              match id with
              | Some name -> Bindings.singleton name
              | None -> Bindings.empty
            in
            this#with_bindings
              loc
              ~lexical:true
              bindings
              (fun () ->
                run_opt this#function_identifier id;
                this#lambda loc params body)
              ()) );

        expr
    end

  let program ?(ignore_toplevel = false) program =
    let (loc, _, _) = program in
    let walk = new scope_builder in
    let bindings =
      if ignore_toplevel then
        Bindings.empty
      else
        let hoist = new hoister in
        hoist#eval hoist#program program
    in
    walk#eval (walk#with_bindings loc bindings walk#program) program
end

module With_Loc = Make (Loc_sig.LocS) (Scope_api.With_Loc)
module With_ALoc = Make (Loc_sig.ALocS) (Scope_api.With_ALoc)
include With_Loc
