(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_mapper
open Utils_js
module Ast = Flow_ast

module FindProviders (L : Loc_sig.S) : sig
  module Id : sig
    type t
  end

  type state =
    | AnnotatedVar
    | InitializedVar
    | NullInitializedVar
    | UninitializedVar

  type ('locs, 'state) base_entry = {
    entry_id: Id.t;
    name: string;
    state: 'state;
    declare_locs: L.LSet.t;
    def_locs: L.LSet.t;
    provider_locs: 'locs;
  }

  type entry = (L.LSet.t, state) base_entry

  type env

  module EntrySet : Flow_set.S with type elt = entry

  val empty_env : env

  val compute_provider_env : (L.t, L.t) Ast.Program.t' -> env

  val all_entries : env -> EntrySet.t

  val get_providers_for_toplevel_var : string -> env -> L.LSet.t option

  val print_full_env : env -> string
end = struct
  module Id : sig
    type t

    val new_id : unit -> t

    val compare : t -> t -> int
  end = struct
    type t = int

    let cur = ref 0

    let new_id () =
      let id = !cur in
      cur := !cur + 1;
      id

    let compare = Int.compare
  end

  (**** Data structures for environments ****)
  (* This describes the state of a variable DURING the provider process, including the scope depths at which
     initializers were discovered. They will be simplified later on. *)
  type intermediate_state =
    (* Annotations are always on declarations, so they don't need depth *)
    | Annotated
    (* number of var scope levels deep that the initializer, and null initializer if applicable, was found from the
       scope in which it was declared--lets us prioritize initializers from nearer
       scopes even if they're lexically later in the program.

       If a variable was initialized to null in one scope, and a non-null value in a deeper scope, the int option
       records the depth of the null. This depth should always be <= the depth of the nonnull initializer, since
       if we initialize something without null in a parent scope, an assignment of null in a child scope should
       not be a provider, even if it's lexically earlier *)
    | Initialized of int * int option
    (* For variables that have, so far, only been initialized to null, this records the depth of the assignment *)
    | NullInitialized of int
    | Uninitialized

  (* This describes the state of a variable AFTER the provider analysis, suitable for external consumption *)
  type state =
    | AnnotatedVar
    | InitializedVar
    | NullInitializedVar
    | UninitializedVar

  (* This describes a single assingment/initialization of a var (rather than the var's state as a whole). ints are
     number of scopes deeper than the variable's declaration that the assignment occurs *)
  type write_state =
    | Annotation
    | Value of int
    | Null of int
    | Nothing

  type kind =
    | Var
    | Lex

  (* Scope entry for a single variable, recording both its declaration site(s) and providers.
      * state is the overall current state of the variable. While we're computing the providers,
        this is the `state` type defined above, including information about the shallowest depth at which
        the variable's providers have been discovered. Later on in provider_api, this is replaced
        with a final state that excludes the depth information.
      * declare_locs are locations where a variable is declared with `let`, `var`, `const`, etc.
        In the vast majority of cases only one location will exist, but there can be multiple in
        the case of `vars` with branching control flow (e.g. `if (c) { var x = 10 } else { var x = 20 }`
        and overridden functions.
      * def_locs are locations where a variable is written to, typically assignments and updates. In this
        case, we exclude declarations--declare_locs and def_locs should be disjoint.
      * provider_locs are the candidate providers we're calculating in this module, and can be either declares or defs.
        Initially, in this module, 'locs will be instantiated as `write_state L.LMap.t`, recording what kind
        of write an indvidual candidate provider is. Later on, in provider_api, this will be simplified and filtered
        into a single `L.LSet.t` of providers

        We expect that the keys/elements of provider_locs are a subset of the union of def_locs and declare_locs.
  *)
  type ('locs, 'state) base_entry = {
    entry_id: Id.t;
    name: string;
    state: 'state;
    declare_locs: L.LSet.t;
    def_locs: L.LSet.t;
    provider_locs: 'locs;
  }

  type intermediate_entry = (write_state L.LMap.t, intermediate_state) base_entry

  type entry = (L.LSet.t, state) base_entry

  module EntrySet = Flow_set.Make (struct
    type t = entry

    let compare { entry_id = id1; _ } { entry_id = id2; _ } = Id.compare id1 id2
  end)

  (* This records, per variable and per scope, the locations of providers for that variable within the scope or any child scopes.
      The exact_locs field records the precise location of the variable being provided, while relative_locs records the location of
      the statement *within the current scope* in which the provider lives. For example, in a program like

      function f() {
        if (condition) { var x = 42 };
      }

     The lexical scope for `f` will contain a `local_providers` for `x`, which contains an `exact_locs` pointing to the actual VariableDeclaration
     node, and a `relative_locs` pointing to the IfStatement.
  *)
  type local_providers = {
    exact_locs: L.LSet.t;
    relative_locs: L.LSet.t;
  }

  (* Individual lexical scope. Entries are variables "native" to this scope,
     children are the child scopes keyed by their locations, and local_providers
     are as described above *)
  type scope = {
    kind: kind;
    entries: intermediate_entry SMap.t;
    children: scope L.LMap.t;
    providers: local_providers SMap.t;
  }

  type env = scope Nel.t

  (* Compute the joined state of a variable, when modified in separate branches *)
  let combine_states init1 init2 =
    match (init1, init2) with
    | (Annotated, _)
    | (_, Annotated) ->
      Annotated
    | (Initialized (n, i), Initialized (m, j)) ->
      let p = min n m in
      let k =
        match (i, j) with
        | (Some k, None)
        | (None, Some k)
          when k <= p ->
          Some k
        | (Some i, Some j) when i <= p || j <= p -> Some (min i j)
        | _ -> None
      in
      Initialized (p, k)
    | (Initialized (n, i), NullInitialized j)
    | (NullInitialized j, Initialized (n, i)) ->
      let k = Base.Option.value_map ~f:(min j) ~default:j i in
      if k <= n then
        Initialized (n, Some k)
      else
        Initialized (n, None)
    | (Initialized (n, i), _)
    | (_, Initialized (n, i)) ->
      Initialized (n, i)
    | (NullInitialized n, NullInitialized m) -> NullInitialized (min n m)
    | (NullInitialized n, _)
    | (_, NullInitialized n) ->
      NullInitialized n
    | (Uninitialized, Uninitialized) -> Uninitialized

  (* This function decides if an incoming write to a variable can possibly be a provider.
     If this function returns None, then the incoming write described by `write_state` cannot
     possibly be a provider (e.g. an assignment to an already `Initialized` variable). If it
     returns Some new state, then the write could be a provider, and the resulting state is
     the new state of the overall variable after including the write. *)
  let extended_state_opt ~write_state ~state =
    match (state, write_state) with
    | (_, Nothing) ->
      (*
      var x;
      *)
      None
    | (Annotated, _) ->
      (*
      var x: string; var x: number; provider is string
      *)
      None
    | (_, Annotation) ->
      (*
       var x = 42; var x: string, provider is string
      *)
      Some Annotated
    | (Uninitialized, Null d) ->
      (*
       var x; x = null provider is null
       *)
      Some (NullInitialized d)
    | (Uninitialized, Value d) ->
      (*
       var x; x = 42 provider is 42
       *)
      Some (Initialized (d, None))
    | (NullInitialized n, Value d) when n <= d ->
      (* var x = null; x = 42; provider is null and 42*)
      Some (Initialized (d, Some n))
    | (NullInitialized _, Value d) ->
      (* var x; (function() { x = null }); x = 42; provider is 42 *)
      Some (Initialized (d, None))
    | (NullInitialized n, Null d) when n <= d ->
      (* var x = null; x = null provider is first null *)
      None
    | (NullInitialized _, Null d) ->
      (* var x; (function() { x = null }); x = null, provider is second null *)
      Some (NullInitialized d)
    | (Initialized (_, Some m), Null d) when m <= d ->
      (* var x = null; x = 42; x = null, providers are first null and 42 *)
      None
    | (Initialized (n, Some _), Null d) ->
      (* var x; (function () { x = null; x = 42; }); x = null, providers are second null and 42 *)
      Some (Initialized (n, Some d))
    | (Initialized (n, None), Null d) when n <= d ->
      (* var x = 42; x = null, provider is 42 *)
      None
    | (Initialized (n, None), Null d) ->
      (* var x; (function () { x = 42; }); x = null, provider is 42 and null *)
      Some (Initialized (n, Some d))
    | (Initialized (n, _), Value d) when n <= d ->
      (* var x = 42; x = "a", provider is 42 *)
      None
    | (Initialized (_, Some m), Value d) when m <= d ->
      (* var x = null; (function () { x = 42; }); x = "a", provider is null and "a" *)
      Some (Initialized (d, Some m))
    | (Initialized (_, _), Value d) ->
      (* var x; (function () { x = null; x = 42; }); x = "a", provider is "a" *)
      (* var x; (function () { x = 42; }); x = "a", provider is "a" *)
      Some (Initialized (d, None))

  (**** Functions for manipulating environments ****)
  let empty_entry name =
    {
      entry_id = Id.new_id ();
      name;
      state = Uninitialized;
      provider_locs = L.LMap.empty;
      declare_locs = L.LSet.empty;
      def_locs = L.LSet.empty;
    }

  let env_invariant_violated s = failwith ("Environment invariant violated: " ^ s)

  (* This function finds the right set of entries to add a new variable to in the scope chain
      based on whether it's a let, const, or var; it also returns a function to rebuild an environment
      with a new set of entries for this place. This lets us update the environment at any point
      in the stack functionally. For example,

       let (entries, set_entries) = find_entries_for_new_variable kind env in
       let new_entries = modify_entries entries in
       let env = set_entries new_entries

     This would produce a new version of `env` where the `entries` field in the scope where a new variable with the
     appropriate `kind` would be added has been modified, but is otherwise the same as the original env.*)
  let find_entries_for_new_variable kind env =
    let rec loop env rev_head =
      match (env, kind) with
      | (({ entries; kind = Lex; _ } as hd) :: tl, Ast.Statement.VariableDeclaration.(Let | Const))
      | (({ entries; kind = Var; _ } as hd) :: tl, _) ->
        ( entries,
          fun entries ->
            (* This produces a version of the existing overall env with a new set of entries, replacing
               the entries returned above. *)
            List.append (List.rev rev_head) (Nel.to_list ({ hd with entries }, tl))
            |> Nel.of_list_exn
        )
      | (hd :: tl, _) -> loop tl (hd :: rev_head)
      | ([], _) -> env_invariant_violated "No valid scope for variable"
    in
    loop (Nel.to_list env) []

  (* Similar to the above function, but rather than finding the set of entries and a replacement function for it
     based on where a new variable would live, this finds the set of entries in which some particular variable
     already exists. *)
  let find_entry_for_existing_variable var env =
    let rec loop var_scopes_off env rev_head =
      match env with
      | ({ entries; _ } as hd) :: tl when SMap.mem var entries ->
        ( SMap.find var entries,
          var_scopes_off,
          fun entry ->
            List.append
              (List.rev rev_head)
              (Nel.to_list ({ hd with entries = SMap.add var entry entries }, tl))
            |> Nel.of_list_exn
        )
      (* If we don't see the variable in the root var scope, it's a global--create a dummy entry for it *)
      | [({ entries; kind = Var; _ } as hd)] ->
        ( empty_entry var,
          var_scopes_off,
          fun entry ->
            List.append (List.rev rev_head) [{ hd with entries = SMap.add var entry entries }]
            |> Nel.of_list_exn
        )
      | [{ kind = Lex; _ }] -> env_invariant_violated "Root environment should always be Var"
      | ({ kind = Var; _ } as hd) :: tl -> loop (var_scopes_off + 1) tl (hd :: rev_head)
      | hd :: tl -> loop var_scopes_off tl (hd :: rev_head)
      | [] -> env_invariant_violated "Unreachable"
    in

    loop 0 (Nel.to_list env) []

  let get_entry var entries = SMap.find_opt var entries |> Option.value ~default:(empty_entry var)

  let empty_provider_info = { exact_locs = L.LSet.empty; relative_locs = L.LSet.empty }

  let get_provider_info var providers =
    SMap.find_opt var providers |> Base.Option.value ~default:empty_provider_info

  let update_provider_info var loc (({ providers; _ } as hd), tl) =
    let { exact_locs; relative_locs } = get_provider_info var providers in
    let providers =
      SMap.add
        var
        { exact_locs = L.LSet.add loc exact_locs; relative_locs = L.LSet.add loc relative_locs }
        providers
    in
    ({ hd with providers }, tl)

  let join_providers prov1 prov2 =
    if prov1 == prov2 then
      prov1
    else
      match (prov1, prov2) with
      | ({ exact_locs = pl1; relative_locs = cl1 }, { exact_locs = pl2; relative_locs = cl2 }) ->
        { exact_locs = L.LSet.union pl1 pl2; relative_locs = L.LSet.union cl1 cl2 }

  let join_envs env1 env2 =
    let join_entries entry1 entry2 =
      if entry1 == entry2 then
        entry1
      else
        match (entry1, entry2) with
        | ( {
              entry_id;
              name = name1;
              state = state1;
              provider_locs = providers1;
              declare_locs = declares1;
              def_locs = defs1;
            },
            {
              entry_id = _;
              name = name2;
              state = state2;
              provider_locs = providers2;
              declare_locs = declares2;
              def_locs = defs2;
            }
          ) ->
          assert (name1 = name2);
          {
            entry_id;
            name = name1;
            state = combine_states state1 state2;
            provider_locs =
              L.LMap.union
                ~combine:(fun _ i j ->
                  if i = j then
                    Some i
                  else
                    failwith "Inconsistent states")
                providers1
                providers2;
            declare_locs = L.LSet.union declares1 declares2;
            def_locs = L.LSet.union defs1 defs2;
          }
    in
    let rec join_scopes scope1 scope2 =
      if scope1 == scope2 then
        scope1
      else
        match (scope1, scope2) with
        | ( { kind = kind1; entries = entries1; children = children1; providers = providers1 },
            { kind = kind2; entries = entries2; children = children2; providers = providers2 }
          ) ->
          assert (kind1 = kind2);
          {
            kind = kind1;
            entries =
              SMap.union ~combine:(fun _ e1 e2 -> Some (join_entries e1 e2)) entries1 entries2;
            providers =
              SMap.union ~combine:(fun _ p1 p2 -> Some (join_providers p1 p2)) providers1 providers2;
            children =
              L.LMap.union ~combine:(fun _ c1 c2 -> Some (join_scopes c1 c2)) children1 children2;
          }
    in

    if env1 == env2 then
      env1
    else begin
      assert (Nel.length env1 = Nel.length env2);
      Base.List.map2_exn ~f:join_scopes (Nel.to_list env1) (Nel.to_list env2) |> Nel.of_list_exn
    end

  let exit_lex_child loc env =
    match env with
    | ( ({ providers = pchild; entries; _ } as child),
        ({ children; providers = pparent; _ } as parent) :: rest
      ) ->
      (* For all variables that aren't native to the child scope, update the parent scope's
         provider_info with whatever providing information the child scope contains,
         coarsening the relative_locs to point at the child scope as a whole. *)
      let pchild_promoted =
        SMap.filter (fun k _ -> not (SMap.mem k entries)) pchild
        |> SMap.map (fun { relative_locs = _; exact_locs } ->
               { relative_locs = L.LSet.singleton loc; exact_locs }
           )
      in
      ( {
          parent with
          children = L.LMap.add loc child children;
          providers =
            SMap.union ~combine:(fun _ p1 p2 -> Some (join_providers p1 p2)) pparent pchild_promoted;
        },
        rest
      )
    | (_, []) -> env_invariant_violated "Popping to empty stack"

  (* Root visitor. Uses the `enter_lex_child` function parameter to manipulate the environment when it dives into a scope,
     and calls `exit_lex_child` when it leaves a scope. For branching statements -- If, Try, and Switch -- it uses the same
     initial env when it explores each branch, and then joins them using `join_envs`. It also has a "context" `cx type, which
     is not accumulated (unlike the env) but stores contextual information using `in_context`. *)
  class ['cx] finder ~(env : env) ~(cx : 'cx) ~enter_lex_child =
    object (this)
      inherit [env * 'cx, L.t] Flow_ast_visitor.visitor ~init:(env, cx) as super

      (* Join environment information from visiting one branch with existing information *)
      method accumulate_branch_env : 'a. env * 'cx -> (unit -> 'a) -> env -> 'a * env =
        fun (env, cx) visit_branch acc_env ->
          this#set_acc (env, cx);
          let branch' = visit_branch () in
          let (env', _) = this#acc in
          (branch', join_envs env' acc_env)

      (* Save the `cx` information and restore it after calling `f`, possibly having modified it with `mod_cx`. *)
      method in_context : 'a. ?mod_cx:('cx -> 'cx) -> (unit -> 'a) -> 'a =
        fun ?mod_cx f ->
          let (env, cx) = this#acc in
          Base.Option.iter ~f:(fun mod_cx -> this#set_acc (env, mod_cx cx)) mod_cx;
          let res = f () in
          let (env, _) = this#acc in
          this#set_acc (env, cx);
          res

      (* Use the `enter_lex_child` parameter to push a new scope onto the environment,
         and then pop it off after calling `meth` using `exit_lex_child` (which is not a
         parameter, but refers to the function defined above) *)
      method enter_scope : 'a. kind -> (L.t -> 'a -> 'a) -> L.t -> 'a -> 'a =
        fun kind meth loc item ->
          let (env, cx) = this#acc in
          let env' = enter_lex_child kind loc env in
          this#set_acc (env', cx);
          let res = this#in_context (fun () -> meth loc item) in
          let (env', cx) = this#acc in
          this#set_acc (exit_lex_child loc env', cx);
          res

      method! declare_function loc expr =
        match Declare_function_utils.declare_function_to_function_declaration_simple loc expr with
        | Some stmt ->
          let _ = this#statement (loc, stmt) in
          expr
        | None -> super#declare_function loc expr

      method! block = this#enter_scope Lex super#block

      method! catch_clause = this#enter_scope Lex super#catch_clause

      method! do_while = this#enter_scope Lex super#do_while

      method! for_statement = this#enter_scope Lex super#for_statement

      method! for_in_statement = this#enter_scope Lex super#for_in_statement

      method! for_of_statement = this#enter_scope Lex super#for_of_statement

      method! while_ = this#enter_scope Lex super#while_

      method! with_ = this#enter_scope Lex super#with_

      method! class_body ((loc, _) as body) =
        this#enter_scope Var (fun _ body -> super#class_body body) loc body

      method! class_expression = this#enter_scope Lex super#class_expression

      method! arrow_function = this#enter_scope Var super#arrow_function

      method! function_expression_or_method =
        this#enter_scope Var super#function_expression_or_method

      (* The identifier of a function declaration belongs to the outer scope, but its parameters and body belong to its own scope--hence the annoying
         need to write out the full visitor and only enter a var scope for its parameters and body *)
      method! function_declaration loc (expr : ('loc, 'loc) Ast.Function.t) =
        let open Ast.Function in
        let {
          id = ident;
          params;
          body;
          async = _;
          generator = _;
          predicate;
          return;
          tparams;
          sig_loc = _;
          comments;
        } =
          expr
        in
        let _ident' = map_opt this#function_identifier ident in
        this#enter_scope
          Var
          (fun _ _ ->
            let _params' = this#function_params params in
            let _return' = this#type_annotation_hint return in
            let _body' = this#function_body_any body in
            let _predicate' = map_opt this#predicate predicate in
            let _tparams' = map_opt this#type_params tparams in
            let _comments' = this#syntax_opt comments in
            expr)
          loc
          expr

      (* For the purposes of this analysis, we don't need to consider `if` statements without alternatives,
         and similarly elsewhere we don't worry about merging the initial environment of e.g. loops with the
         final environment from their bodies. It's ok if we're basing typechecking on statements that might not
         be executed at runtime--if the only assignment to a variable is in a loop or a single-armed if, we still
         consider it outside that context to have the type it was assigned to in context, even if at runtime the
         assignment might not occur.

         Because of this, we only need to join environments for multiply-branching statements, where different explicit
         choices can be made by the programmer in each branch. *)
      method! if_statement _loc (stmt : ('loc, 'loc) Ast.Statement.If.t) =
        let open Ast.Statement.If in
        let { test; consequent; alternate; comments } = stmt in
        let _test' = this#predicate_expression test in

        let (env0, cx) = this#acc in
        let _consequent' = this#if_consequent_statement ~has_else:(alternate <> None) consequent in
        let (env1, _) = this#acc in
        let (_alternate', env2) =
          this#accumulate_branch_env
            (env0, cx)
            (fun () -> map_opt (map_loc this#if_alternate_statement) alternate)
            env1
        in
        this#set_acc (env2, cx);

        let _comments' = this#syntax_opt comments in
        stmt

      method! switch _loc (switch : ('loc, 'loc) Ast.Statement.Switch.t) =
        let open Ast.Statement.Switch in
        let { discriminant; cases = _; comments = _ } = switch in
        let _discriminant' = this#expression discriminant in
        this#enter_scope
          Lex
          (fun _loc switch ->
            let { discriminant = _; cases; comments } = switch in
            let (env0, cx) = this#acc in
            let (rev_cases', env') =
              Base.List.fold cases ~init:([], env0) ~f:(fun (acc_cases, acc_env) case ->
                  let (case', acc_env) =
                    this#accumulate_branch_env (env0, cx) (fun () -> this#switch_case case) acc_env
                  in
                  (case' :: acc_cases, acc_env)
              )
            in
            let _cases' = List.rev rev_cases' in
            this#set_acc (env', cx);
            let _comments' = this#syntax_opt comments in
            switch)
          _loc
          switch

      method! try_catch _loc (stmt : ('loc, 'loc) Ast.Statement.Try.t) =
        let open Ast.Statement.Try in
        let { block; handler; finalizer; comments } = stmt in
        let (env0, cx) = this#acc in
        let _block' = map_loc this#block block in
        let (env', _) = this#acc in
        let (_handler', env') =
          this#accumulate_branch_env
            (env0, cx)
            (fun () ->
              match handler with
              | Some (loc, clause) ->
                id_loc this#catch_clause loc clause handler (fun clause -> Some (loc, clause))
              | None -> handler)
            env'
        in
        let (_finalizer', env') =
          this#accumulate_branch_env
            (env0, cx)
            (fun () ->
              match finalizer with
              | Some (finalizer_loc, block) ->
                id_loc this#block finalizer_loc block finalizer (fun block ->
                    Some (finalizer_loc, block)
                )
              | None -> finalizer)
            env'
        in
        this#set_acc (env', cx);
        let _comments' = this#syntax_opt comments in
        stmt

      (* read and write (when the argument is an identifier) *)
      method! update_expression _loc (expr : (L.t, L.t) Ast.Expression.Update.t) =
        let open Ast.Expression.Update in
        let { argument; operator = _; prefix = _; comments = _ } = expr in
        begin
          match argument with
          | (_, Ast.Expression.Identifier x) ->
            (* given `x++`, read x then write x *)
            ignore @@ this#identifier x;
            ignore @@ this#pattern_identifier x
          | _ ->
            (* given `o.x++`, read o *)
            ignore @@ this#expression argument
        end;
        expr

      (* Don't call pattern_identifier on property keys--either it will have been called twice, or incorrectly.
         The example to consider is
           { a1: a } = ...
         pattern_object_property_identifier_key will be called on a1, but a1 is not a variable in scope, its a property on the
         RHS object. On the other hand, a is a variable in scope, which will be visited by pattern_object_property_pattern.
         If we didn't rename a1, we'd visit a1 with both pattern_object_property_identifier_key and
         pattern_object_property_pattern, so it's safe to only visit it with the latter. *)
      method! pattern_object_property_identifier_key ?kind (key : ('loc, 'loc) Ast.Identifier.t) =
        ignore kind;
        key
    end

  (****** pass 1 *******)

  let new_scope ~kind =
    { kind; entries = SMap.empty; children = L.LMap.empty; providers = SMap.empty }

  let empty_env = (new_scope ~kind:Var, [])

  let enter_new_lex_child kind _ parent =
    let child = new_scope ~kind in
    Nel.cons child parent

  type find_declarations_cx = { init_state: write_state }

  (* This visitor finds variable declarations and records them, and if the declaration includes an initialization or an annotation,
     also marks them as providers. *)
  class find_declarations ~env () =
    object (this)
      inherit
        [find_declarations_cx] finder
          ~env
          ~cx:{ init_state = Value 0 }
          ~enter_lex_child:enter_new_lex_child as super

      (* Add a new variable declaration to the scope, which may or may not be a provider as well. *)
      method new_entry var kind loc =
        let (env, ({ init_state = write_state } as cx)) = this#acc in
        let (entries, reconstruct_env) = find_entries_for_new_variable kind env in
        let ({ declare_locs; state = cur_state; provider_locs; _ } as entry) =
          get_entry var entries
        in
        let declare_locs = L.LSet.add loc declare_locs in
        let new_state = extended_state_opt ~state:cur_state ~write_state in
        let (state, provider_locs) =
          Base.Option.value_map
            ~f:(fun state -> (state, L.LMap.add loc write_state provider_locs))
            ~default:(cur_state, provider_locs)
            new_state
        in
        let entries = SMap.add var { entry with declare_locs; state; provider_locs } entries in
        let env = reconstruct_env entries in
        let env =
          if Base.Option.is_some new_state then
            update_provider_info var loc env
          else
            env
        in
        this#set_acc (env, cx)

      method! declare_variable _loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
        let open Ast.Statement.DeclareVariable in
        let { id = ident; annot; comments = _ } = decl in
        let (_ : ('a, 'b) Ast.Type.annotation_or_hint) = this#type_annotation_hint annot in
        let (_ : ('a, 'b) Ast.Identifier.t) =
          this#in_context
            ~mod_cx:(fun _cx -> { init_state = Annotation })
            (fun () -> this#pattern_identifier ~kind:Ast.Statement.VariableDeclaration.Var ident)
        in
        decl

      method! variable_declarator
          ~kind (decl : ('loc, 'loc) Ast.Statement.VariableDeclaration.Declarator.t) =
        let (loc, { Ast.Statement.VariableDeclaration.Declarator.id; init }) = decl in
        let annot =
          let open Ast.Pattern in
          match id with
          | (_, Array { Array.annot; _ })
          | (_, Object { Object.annot; _ })
          | (_, Identifier { Identifier.annot; _ }) ->
            Some annot
          | _ -> None
        in
        let init_state =
          match (init, annot) with
          | (_, Some (Ast.Type.Available _)) -> Annotation
          | (None, _) -> Nothing
          | (Some (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Null; _ }), _) ->
            Null 0
          | _ -> Value 0
        in

        let id' =
          this#in_context
            ~mod_cx:(fun _cx -> { init_state })
            (fun () -> this#variable_declarator_pattern ~kind id)
        in

        let init' = map_opt this#expression init in
        if id == id' && init == init' then
          decl
        else
          (loc, { Ast.Statement.VariableDeclaration.Declarator.id = id'; init = init' })

      method! function_declaration loc (expr : ('loc, 'loc) Ast.Function.t) =
        let open Ast.Function in
        let {
          id = ident;
          params;
          body;
          async = _;
          generator = _;
          predicate;
          return;
          tparams;
          sig_loc = _;
          comments;
        } =
          expr
        in
        let init_state =
          match return with
          | Ast.Type.Available _ -> Annotation
          | _ -> Value 0
        in

        let _ident' =
          map_opt
            (fun id ->
              this#in_context
                ~mod_cx:(fun _cx -> { init_state })
                (fun () -> this#function_identifier id))
            ident
        in
        this#enter_scope
          Var
          (fun _ _ ->
            let _params' = this#function_params params in
            let _return' = this#type_annotation_hint return in
            let _body' = this#function_body_any body in
            let _predicate' = map_opt this#predicate predicate in
            let _tparams' = map_opt this#type_params tparams in
            let _comments' = this#syntax_opt comments in
            expr)
          loc
          expr

      method! function_ _loc (expr : ('loc, 'loc) Ast.Function.t) =
        let open Ast.Function in
        let {
          id = ident;
          params;
          body;
          async = _;
          generator = _;
          predicate;
          return;
          tparams;
          sig_loc = _;
          comments;
        } =
          expr
        in
        let init_state =
          match return with
          | Ast.Type.Available _ -> Annotation
          | _ -> Value 0
        in

        let _ident' =
          map_opt
            (fun id ->
              this#in_context
                ~mod_cx:(fun _cx -> { init_state })
                (fun () -> this#function_identifier id))
            ident
        in
        let _params' = this#function_params params in
        let _return' = this#type_annotation_hint return in
        let _body' = this#function_body_any body in
        let _predicate' = map_opt this#predicate predicate in
        let _tparams' = map_opt this#type_params tparams in
        let _comments' = this#syntax_opt comments in
        expr

      method! pattern_identifier ?kind ((loc, { Ast.Identifier.name; comments = _ }) as ident) =
        begin
          match kind with
          | Some kind -> this#new_entry name kind loc
          | _ -> ()
        end;
        super#identifier ident

      method! function_identifier ((loc, { Ast.Identifier.name; comments = _ }) as ident) =
        this#new_entry name Flow_ast.Statement.VariableDeclaration.Let loc;
        super#identifier ident

      method! for_in_left_declaration left =
        let (_, decl) = left in
        let { Flow_ast.Statement.VariableDeclaration.declarations; kind; comments = _ } = decl in
        (match declarations with
        | [(_, { Flow_ast.Statement.VariableDeclaration.Declarator.id; init = _ })] ->
          let open Flow_ast.Pattern in
          (match id with
          | (_, (Identifier _ | Object _ | Array _)) ->
            let init_state =
              let open Ast.Pattern in
              match id with
              | (_, Array { Array.annot = Ast.Type.Available _; _ })
              | (_, Object { Object.annot = Ast.Type.Available _; _ })
              | (_, Identifier { Identifier.annot = Ast.Type.Available _; _ }) ->
                Annotation
              | _ -> Value 0
            in
            ignore
            @@ this#in_context
                 ~mod_cx:(fun _cx -> { init_state })
                 (fun () -> this#variable_declarator_pattern ~kind id)
          | _ -> failwith "unexpected AST node")
        | _ -> failwith "Syntactically valid for-in loops must have exactly one left declaration");
        left

      method! for_of_left_declaration left =
        let (_, decl) = left in
        let { Flow_ast.Statement.VariableDeclaration.declarations; kind; comments = _ } = decl in
        (match declarations with
        | [(_, { Flow_ast.Statement.VariableDeclaration.Declarator.id; init = _ })] ->
          let open Flow_ast.Pattern in
          (match id with
          | (_, (Identifier _ | Object _ | Array _)) ->
            let init_state =
              let open Ast.Pattern in
              match id with
              | (_, Array { Array.annot = Ast.Type.Available _; _ })
              | (_, Object { Object.annot = Ast.Type.Available _; _ })
              | (_, Identifier { Identifier.annot = Ast.Type.Available _; _ }) ->
                Annotation
              | _ -> Value 0
            in
            ignore
            @@ this#in_context
                 ~mod_cx:(fun _cx -> { init_state })
                 (fun () -> this#variable_declarator_pattern ~kind id)
          | _ -> failwith "unexpected AST node")
        | _ -> failwith "Syntactically valid for-in loops must have exactly one left declaration");
        left
    end

  let find_declaration_statements { Ast.Program.statements; _ } =
    Base.List.fold
      ~init:(new_scope ~kind:Var |> Nel.one)
      ~f:(fun env stmt ->
        let decl_find = new find_declarations ~env () in
        let (env, _) = decl_find#eval decl_find#statement stmt in
        env)
      statements

  (****** pass 2 *******)

  let enter_existing_lex_child _ loc (({ children; _ }, _) as env) =
    Nel.cons (L.LMap.find loc children) env

  type find_providers_cx = { null_assign: bool }

  (* This visitor finds variable assignments that are not declarations and adds them to the providers for that variable
     if appropriate. *)
  class find_providers ~env () =
    object (this)
      inherit
        [find_providers_cx] finder
          ~env
          ~cx:{ null_assign = false }
          ~enter_lex_child:enter_existing_lex_child as super

      (* Add a new variable provider to the scope, which is not a declaration. *)
      method add_provider var loc =
        let (env, ({ null_assign } as cx)) = this#acc in
        let ( ({ state = cur_state; provider_locs; def_locs; _ } as entry),
              var_scopes_off,
              reconstruct_env
            ) =
          find_entry_for_existing_variable var env
        in
        let write_state =
          if null_assign then
            Null var_scopes_off
          else
            Value var_scopes_off
        in
        let extended_state = extended_state_opt ~state:cur_state ~write_state in
        let def_locs = L.LSet.add loc def_locs in
        let (state, provider_locs) =
          Base.Option.value_map
            ~f:(fun state -> (state, L.LMap.add loc write_state provider_locs))
            ~default:(cur_state, provider_locs)
            extended_state
        in
        let env = reconstruct_env { entry with state; provider_locs; def_locs } in
        let env =
          if Base.Option.is_some extended_state then
            update_provider_info var loc env
          else
            env
        in
        this#set_acc (env, cx)

      method! assignment
          _loc ({ Ast.Expression.Assignment.operator = _; left; right; comments; _ } as expr) =
        let null_assign =
          match right with
          | (_, Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Null; _ }) -> true
          | _ -> false
        in
        let _left' =
          this#in_context
            ~mod_cx:(fun _cx -> { null_assign })
            (fun () -> this#assignment_pattern left)
        in
        let _right' = this#expression right in
        let _comments' = this#syntax_opt comments in
        expr

      method! pattern_identifier ?kind ((loc, { Ast.Identifier.name; comments = _ }) as ident) =
        begin
          match kind with
          | None -> this#add_provider name loc
          | Some _ -> ()
        end;
        super#identifier ident
    end

  let find_provider_statements env { Ast.Program.statements; _ } =
    Base.List.fold
      ~init:env
      ~f:(fun env stmt ->
        let prov_find = new find_providers ~env () in
        let (env, _) = prov_find#eval prov_find#statement stmt in
        env)
      statements

  (* This simplifies an `intermediate_entry` to an `entry` for external use *)
  let simplify_providers ({ provider_locs; state; _ } as entry) =
    let provider_locs =
      L.LMap.fold
        (fun loc write_state acc ->
          match (write_state, state) with
          | (Annotation, Annotated) -> L.LSet.add loc acc
          | (Annotation, _) -> assert_false "Invariant violated"
          | (Null n, (NullInitialized m | Initialized (_, Some m)))
          | (Value n, Initialized (m, _)) ->
            if n = m then
              L.LSet.add loc acc
            else if n < m then
              assert_false "Invariant violated"
            else
              acc
          | (Nothing, _) -> assert_false "Invariant violated"
          | _ -> acc)
        provider_locs
        L.LSet.empty
    in
    let state =
      match state with
      | Annotated -> AnnotatedVar
      | Initialized _ -> InitializedVar
      | NullInitialized _ -> NullInitializedVar
      | Uninitialized -> UninitializedVar
    in
    { entry with provider_locs; state }

  let compute_provider_env program =
    let env = find_declaration_statements program in
    find_provider_statements env program

  let all_entries (hd, _) =
    let rec all_entries_in_scope { entries; children; _ } =
      L.LMap.fold
        (fun _ child acc -> EntrySet.union (all_entries_in_scope child) acc)
        children
        EntrySet.empty
      |> SMap.fold (fun _ entry acc -> EntrySet.add (simplify_providers entry) acc) entries
    in
    all_entries_in_scope hd

  let get_providers_for_toplevel_var var ({ entries; _ }, _) =
    let entry = SMap.find_opt var entries in
    Base.Option.map ~f:(fun entry -> (simplify_providers entry).provider_locs) entry

  let print_full_env env =
    let rec ptabs count =
      if count = 0 then
        ""
      else
        spf " %s" (ptabs (count - 1))
    in
    let rec print_rec label tabs { providers; entries = _; children; _ } =
      let msg = spf "%s%s:\n" (ptabs tabs) label in
      let tabs = tabs + 1 in
      let t = ptabs tabs in
      let msg =
        spf
          "%s%sproviders: \n%s\n"
          msg
          t
          (SMap.bindings providers
          |> Base.List.map ~f:(fun (k, { relative_locs; exact_locs }) ->
                 spf
                   "%s %s:\n%s  relative: (%s)\n%s  exact: (%s)"
                   t
                   k
                   t
                   (L.LSet.elements relative_locs
                   |> Base.List.map ~f:(L.debug_to_string ~include_source:false)
                   |> String.concat "), ("
                   )
                   t
                   (L.LSet.elements exact_locs
                   |> Base.List.map ~f:(L.debug_to_string ~include_source:false)
                   |> String.concat "), ("
                   )
             )
          |> String.concat "\n"
          )
      in
      spf
        "%s%schildren:\n%s"
        msg
        t
        (L.LMap.bindings children
        |> Base.List.map ~f:(fun (loc, scope) ->
               print_rec (L.debug_to_string ~include_source:false loc) (tabs + 1) scope
           )
        |> String.concat "\n"
        )
    in
    match env with
    | (top, []) -> print_rec "toplevel" 0 top
    | _ -> env_invariant_violated "Final environment has depth =/= 1"
end
