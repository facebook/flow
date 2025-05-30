(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Flow_ast_visitor
module Ast = Flow_ast

exception ImpossibleState of string

(* This describes the state of a variable AFTER the provider analysis, suitable for external consumption *)
type state =
  | AnnotatedVar of { contextual: bool }
  | InitializedVar
  | ArrayInitializedVar
  | EmptyArrayInitializedVar
  | NullInitializedVar
  | UninitializedVar

type write_kind =
  | EmptyArray
  | Ordinary

module FindProviders (L : Loc_sig.S) : sig
  module Id : sig
    type t
  end

  type ('locs, 'state) base_entry = {
    entry_id: Id.t;
    name: string;
    state: 'state;
    declare_locs: L.LSet.t;
    def_locs: L.LSet.t;
    provider_locs: 'locs;
    possible_generic_escape_locs: L.LSet.t;
    binding_kind: Bindings.kind;
  }

  type providers = {
    writes: write_kind L.LMap.t;
    array_writes: L.LSet.t;
  }

  type entry = (providers, state) base_entry

  type env

  module EntrySet : Flow_set.S with type elt = entry

  val empty_env : env

  val compute_provider_env : (L.t, L.t) Ast.Program.t' -> env

  val all_entries : env -> EntrySet.t

  val get_providers_for_toplevel_var : string -> env -> write_kind L.LMap.t option
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
    | Annotated of { contextual: bool }
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
    | EmptyArrInitialized
    | ArrInitialized of int
    | Uninitialized

  (* This describes a single assingment/initialization of a var (rather than the var's state as a whole). ints are
     number of scopes deeper than the variable's declaration that the assignment occurs *)
  type write_state =
    | Annotation of { contextual: bool }
    | Value of int
    | ArrayValue of int
    | Null of int
    | EmptyArr
    | ArrWrite of int
    | Nothing

  type kind =
    | Var
    | Lex
    | Polymorphic

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
    possible_generic_escape_locs: L.LSet.t;
    binding_kind: Bindings.kind;
  }

  type intermediate_entry = (write_state L.LMap.t, intermediate_state) base_entry

  type providers = {
    writes: write_kind L.LMap.t;
    array_writes: L.LSet.t;
  }

  type entry = (providers, state) base_entry

  module EntrySet = Flow_set.Make (struct
    type t = entry

    let compare { entry_id = id1; _ } { entry_id = id2; _ } = Id.compare id1 id2
  end)

  (* Individual lexical scope. Entries are variables "native" to this scope,
     children are the child scopes keyed by their locations, and local_providers
     are as described above *)
  type scope = {
    kind: kind;
    entries: intermediate_entry SMap.t;
    children: scope L.LMap.t;
  }

  type env = scope Nel.t

  (* Compute the joined state of a variable, when modified in separate branches *)
  let combine_states init1 init2 =
    match (init1, init2) with
    | (Annotated { contextual = c1 }, Annotated { contextual = c2 }) ->
      Annotated { contextual = c1 && c2 }
    | (Annotated { contextual }, _) -> Annotated { contextual }
    | (_, Annotated { contextual }) -> Annotated { contextual }
    | (Uninitialized, other)
    | (other, Uninitialized) ->
      other
    | (ArrInitialized n, ArrInitialized m) -> ArrInitialized (min m n)
    | (ArrInitialized n, EmptyArrInitialized)
    | (EmptyArrInitialized, ArrInitialized n) ->
      ArrInitialized n
    | (other, (EmptyArrInitialized | ArrInitialized _))
    | ((EmptyArrInitialized | ArrInitialized _), other) ->
      other
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
    | (NullInitialized n, NullInitialized m) -> NullInitialized (min n m)

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
    | (Annotated _, _) ->
      (*
      var x: string; var x: number; provider is string
      *)
      None
    | (_, Annotation { contextual }) ->
      (*
       var x = 42; var x: string, provider is string
      *)
      Some (Annotated { contextual })
    | (Uninitialized, Null d) ->
      (*
       var x; x = null provider is null
       *)
      Some (NullInitialized d)
    | (Uninitialized, (Value d | ArrayValue d)) ->
      (*
       var x; x = 42 provider is 42
       *)
      Some (Initialized (d, None))
    | (Uninitialized, EmptyArr) ->
      (*
       var x; var x = [], [] provider
       *)
      Some EmptyArrInitialized
    | (Uninitialized, ArrWrite _) ->
      (*
       var x; x.push(42) no provider
       *)
      None
    | (NullInitialized n, (Value d | ArrayValue d)) when n <= d ->
      (* var x = null; x = 42; provider is null and 42*)
      Some (Initialized (d, Some n))
    | (NullInitialized _, (Value d | ArrayValue d)) ->
      (* var x; (function() { x = null }); x = 42; provider is 42 *)
      Some (Initialized (d, None))
    | (NullInitialized n, Null d) when n <= d ->
      (* var x = null; x = null provider is first null *)
      None
    | (NullInitialized _, Null d) ->
      (* var x; (function() { x = null }); x = null, provider is second null *)
      Some (NullInitialized d)
    | (NullInitialized _, EmptyArr) ->
      (*
       var x = null; var x = [], [] provider
       *)
      Some EmptyArrInitialized
    | (NullInitialized _, ArrWrite _) ->
      (*
       var x = null; x.push(42), null provider
       *)
      None
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
    | (Initialized (n, _), (Value d | ArrayValue d)) when n <= d ->
      (* var x = 42; x = "a", provider is 42 *)
      None
    | (Initialized (_, Some m), (Value d | ArrayValue d)) when m <= d ->
      (* var x = null; (function () { x = 42; }); x = "a", provider is null and "a" *)
      Some (Initialized (d, Some m))
    | (Initialized (_, _), (Value d | ArrayValue d)) ->
      (* var x; (function () { x = null; x = 42; }); x = "a", provider is "a" *)
      (* var x; (function () { x = 42; }); x = "a", provider is "a" *)
      Some (Initialized (d, None))
    | (Initialized _, EmptyArr) -> None
    | (Initialized (_, _), ArrWrite _) -> None
    | (EmptyArrInitialized, (Null _ | EmptyArr)) -> None
    | (EmptyArrInitialized, Value _) -> None
    | (EmptyArrInitialized, ArrayValue n) -> Some (ArrInitialized n)
    | (EmptyArrInitialized, ArrWrite n) -> Some (ArrInitialized n)
    | (ArrInitialized _, (Null _ | EmptyArr)) -> None
    | (ArrInitialized _, Value _) -> None
    | (ArrInitialized n, ArrayValue d) when n <= d -> None
    | (ArrInitialized _, ArrayValue d) -> Some (ArrInitialized d)
    | (ArrInitialized n, ArrWrite d) when n <= d -> None
    | (ArrInitialized _, ArrWrite d) -> Some (ArrInitialized d)

  (**** Functions for manipulating environments ****)
  let empty_entry name binding_kind =
    {
      entry_id = Id.new_id ();
      name;
      state = Uninitialized;
      provider_locs = L.LMap.empty;
      declare_locs = L.LSet.empty;
      def_locs = L.LSet.empty;
      possible_generic_escape_locs = L.LSet.empty;
      binding_kind;
    }

  let env_invariant_violated s = raise (ImpossibleState ("Environment invariant violated: " ^ s))

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
      | (({ entries; kind = Lex; _ } as hd) :: tl, Ast.Variable.(Let | Const))
      | (({ entries; kind = Var | Polymorphic; _ } as hd) :: tl, _) ->
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
          Some (hd :: tl),
          fun entry ->
            List.append
              (List.rev rev_head)
              (Nel.to_list ({ hd with entries = SMap.add var entry entries }, tl))
            |> Nel.of_list_exn
        )
      (* If we don't see the variable in the root var scope, it's a global--create a dummy entry for it *)
      | [({ entries; kind = Var; _ } as hd)] ->
        ( empty_entry var Bindings.Var,
          var_scopes_off,
          None,
          fun entry ->
            List.append (List.rev rev_head) [{ hd with entries = SMap.add var entry entries }]
            |> Nel.of_list_exn
        )
      | [{ kind = Lex | Polymorphic; _ }] ->
        env_invariant_violated "Root environment should always be in Var"
      | ({ kind = Var; _ } as hd) :: tl -> loop (var_scopes_off + 1) tl (hd :: rev_head)
      | hd :: tl -> loop var_scopes_off tl (hd :: rev_head)
      | [] -> env_invariant_violated "Unreachable"
    in
    loop 0 (Nel.to_list env) []

  let state_of_var var env =
    let rec loop env =
      match env with
      | { entries; _ } :: _ when SMap.mem var entries -> Some (SMap.find var entries).state
      | [{ kind = Var; _ }] -> None
      | [{ kind = Lex | Polymorphic; _ }] ->
        env_invariant_violated "Root environment should always be Var"
      | _ :: tl -> loop tl
      | [] -> env_invariant_violated "Unreachable"
    in
    loop (Nel.to_list env)

  let get_entry var default_binding entries =
    SMap.find_opt var entries |> Option.value ~default:(empty_entry var default_binding)

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
              possible_generic_escape_locs = possible_generic_escape_locs1;
              binding_kind = binding_kind1;
            },
            {
              entry_id = _;
              name = name2;
              state = state2;
              provider_locs = providers2;
              declare_locs = declares2;
              def_locs = defs2;
              possible_generic_escape_locs = possible_generic_escape_locs2;
              binding_kind = binding_kind2;
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
                    raise (ImpossibleState "Inconsistent states"))
                providers1
                providers2;
            declare_locs = L.LSet.union declares1 declares2;
            def_locs = L.LSet.union defs1 defs2;
            possible_generic_escape_locs =
              L.LSet.union possible_generic_escape_locs1 possible_generic_escape_locs2;
            binding_kind =
              (* We care about the binding kind so that we can report declared functions as
               * providers. Declared functions aren't typically declared inside a branching
               * statement, but there is no syntax error preventing someone from doing that.
               * To account for this case, we consider a binding to be for a declared function if
               * at least one branch was a declared function. In all other non-equal cases we
               * just use Var, since we're only concerned with declared functions here *)
              ( if binding_kind1 = binding_kind2 then
                binding_kind1
              else
                match (binding_kind1, binding_kind2) with
                | (Bindings.DeclaredFunction, _)
                | (_, Bindings.DeclaredFunction) ->
                  Bindings.DeclaredFunction
                | _ -> Bindings.Var
              );
          }
    in
    let rec join_scopes scope1 scope2 =
      if scope1 == scope2 then
        scope1
      else
        match (scope1, scope2) with
        | ( { kind = kind1; entries = entries1; children = children1 },
            { kind = kind2; entries = entries2; children = children2 }
          ) ->
          assert (kind1 = kind2);
          {
            kind = kind1;
            entries =
              SMap.union ~combine:(fun _ e1 e2 -> Some (join_entries e1 e2)) entries1 entries2;
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
    | (child, ({ children; _ } as parent) :: rest) ->
      ({ parent with children = L.LMap.add loc child children }, rest)
    | (_, []) -> env_invariant_violated "Popping to empty stack"

  (* Root visitor. Uses the `enter_lex_child` function parameter to manipulate the environment when it dives into a scope,
     and calls `exit_lex_child` when it leaves a scope. For branching statements -- If, Try, and Switch -- it uses the same
     initial env when it explores each branch, and then joins them using `join_envs`. It also has a "context" `cx type, which
     is not accumulated (unlike the env) but stores contextual information using `in_context`. *)
  class ['cx] finder ~(env : env) ~(cx : 'cx) ~enter_lex_child =
    object (this)
      inherit [env * 'cx, L.t] visitor ~init:(env, cx) as super

      (* Join environment information from visiting one branch with existing information *)
      method accumulate_branch_env : 'a. env * 'cx -> (unit -> unit) -> env -> env =
        fun (env, cx) visit_branch acc_env ->
          this#set_acc (env, cx);
          visit_branch ();
          let (env', _) = this#acc in
          join_envs env' acc_env

      (* Save the `cx` information and restore it after calling `f`, possibly having modified it with `mod_cx`. *)
      method in_context : 'a. ?mod_cx:('cx -> 'cx) -> (unit -> 'a) -> 'a =
        fun ?mod_cx f ->
          let (env, cx) = this#acc in
          Base.Option.iter ~f:(fun mod_cx -> this#set_acc (env, mod_cx cx)) mod_cx;
          let res = f () in
          let (env, _) = this#acc in
          this#set_acc (env, cx);
          res

      method visit_in_context : 'a. ?mod_cx:('cx -> 'cx) -> (unit -> 'a) -> unit =
        (fun ?mod_cx f -> ignore @@ this#in_context ?mod_cx f)

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

      method enter_possibly_polymorphic_scope
          : 'a. is_polymorphic:bool -> kind:kind -> (L.t -> 'a -> 'a) -> L.t -> 'a -> 'a =
        fun ~is_polymorphic ~kind meth ->
          if is_polymorphic then
            this#enter_scope Polymorphic (this#enter_scope kind meth)
          else
            this#enter_scope kind meth

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

      method! class_expression loc c =
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some c.Ast.Class.tparams)
          ~kind:Lex
          super#class_expression
          loc
          c

      method! class_ loc c =
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some c.Ast.Class.tparams)
          ~kind:Lex
          super#class_
          loc
          c

      method! arrow_function loc func =
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some func.Ast.Function.tparams)
          ~kind:Var
          super#arrow_function
          loc
          func

      method! function_expression_or_method loc func =
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some func.Ast.Function.tparams)
          ~kind:Var
          super#function_expression_or_method
          loc
          func

      (* The identifier of a function declaration belongs to the outer scope, but its parameters and body belong to its own scope--hence the annoying
         need to write out the full visitor and only enter a var scope for its parameters and body *)
      method! function_declaration loc (expr : ('loc, 'loc) Ast.Function.t) =
        let open Ast.Function in
        let {
          id = ident;
          params;
          body;
          predicate;
          return;
          tparams;
          async = _;
          generator = _;
          effect_ = _;
          sig_loc = _;
          comments = _;
        } =
          expr
        in
        run_opt this#function_identifier ident;
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some tparams)
          ~kind:Var
          (fun _ _ ->
            run this#function_params params;
            run this#function_return_annotation return;
            run this#function_body_any body;
            run_opt this#predicate predicate;
            run_opt (this#type_params ~kind:Flow_ast_mapper.FunctionTP) tparams;
            expr)
          loc
          expr

      (* As above, since components compile to function declarations *)
      method! component_declaration loc (expr : ('loc, 'loc) Ast.Statement.ComponentDeclaration.t) =
        let open Ast.Statement.ComponentDeclaration in
        let { id = ident; tparams; params; body; renders; comments = _; sig_loc = _ } = expr in
        run this#component_identifier ident;
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some tparams)
          ~kind:Var
          (fun _ _ ->
            run_opt (this#type_params ~kind:Flow_ast_mapper.ComponentDeclarationTP) tparams;
            run this#component_params params;
            run this#component_body body;
            run this#component_renders_annotation renders;
            expr)
          loc
          expr

      (* Declared modules provide their own var scope *)
      method! declare_module loc m =
        this#enter_scope Var (fun _ _ -> super#declare_module loc m) loc m

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
        let { test; consequent; alternate; comments = _ } = stmt in
        run this#predicate_expression test;

        let (env0, cx) = this#acc in
        run (this#if_consequent_statement ~has_else:(alternate <> None)) consequent;
        let (env1, _) = this#acc in
        let env2 =
          this#accumulate_branch_env
            (env0, cx)
            (fun () -> run_loc_opt this#if_alternate_statement alternate)
            env1
        in
        this#set_acc (env2, cx);
        stmt

      method! switch loc (switch : ('loc, 'loc) Ast.Statement.Switch.t) =
        let open Ast.Statement.Switch in
        let { discriminant; cases = _; comments = _; exhaustive_out = _ } = switch in
        run this#expression discriminant;
        this#enter_scope
          Lex
          (fun _loc switch ->
            let { discriminant = _; cases; comments = _; exhaustive_out = _ } = switch in
            let (env0, cx) = this#acc in
            let env' =
              Base.List.fold cases ~init:env0 ~f:(fun acc_env case ->
                  this#accumulate_branch_env
                    (env0, cx)
                    (fun () -> run this#switch_case case)
                    acc_env
              )
            in
            this#set_acc (env', cx);
            switch)
          loc
          switch

      method! try_catch _loc (stmt : ('loc, 'loc) Ast.Statement.Try.t) =
        let open Ast.Statement.Try in
        let { block; handler; finalizer; comments = _ } = stmt in
        let (env0, cx) = this#acc in
        run_loc this#block block;
        let (env', _) = this#acc in
        let env' =
          this#accumulate_branch_env
            (env0, cx)
            (fun () -> run_loc_opt this#catch_clause handler)
            env'
        in
        let env' =
          this#accumulate_branch_env (env0, cx) (fun () -> run_loc_opt this#block finalizer) env'
        in
        this#set_acc (env', cx);
        stmt

      (* read and write (when the argument is an identifier) *)
      method! update_expression _loc (expr : (L.t, L.t) Ast.Expression.Update.t) =
        let open Ast.Expression.Update in
        let { argument; operator = _; prefix = _; comments = _ } = expr in
        begin
          match argument with
          | (_, Ast.Expression.Identifier x) ->
            (* given `x++`, read x then write x *)
            run this#identifier x;
            run this#pattern_identifier x
          | _ ->
            (* given `o.x++`, read o *)
            run this#expression argument
        end;
        expr

      method! match_ loc ~on_case_body x =
        let open Ast.Match in
        let { arg; match_keyword_loc; cases = _; comments = _ } = x in
        run this#expression arg;

        this#enter_scope
          Lex
          (fun _ ({ cases; _ } as x) ->
            run
              (this#pattern_identifier ~kind:Ast.Variable.Const)
              (Flow_ast_utils.match_root_ident match_keyword_loc);
            (match cases with
            | [] -> ()
            | first_case :: rest_cases ->
              let (env0, cx) = this#acc in
              run (this#match_case ~on_case_body) first_case;
              let (env1, _) = this#acc in
              let env2 =
                Base.List.fold rest_cases ~init:env1 ~f:(fun env_acc case ->
                    this#accumulate_branch_env
                      (env0, cx)
                      (fun () -> run (this#match_case ~on_case_body) case)
                      env_acc
                )
              in
              this#set_acc (env2, cx));
            x)
          loc
          x

      method! match_case ~on_case_body ((loc, _) as case) =
        this#enter_scope Lex (fun _ case -> super#match_case ~on_case_body case) loc case

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

      method! match_object_pattern_property_key key = key

      method! match_member_pattern_property prop = prop
    end

  (****** pass 1 *******)

  let new_scope ~kind = { kind; entries = SMap.empty; children = L.LMap.empty }

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
      method new_entry var binding_kind kind loc =
        let (env, ({ init_state = write_state } as cx)) = this#acc in
        let (entries, reconstruct_env) = find_entries_for_new_variable kind env in
        let ( {
                declare_locs;
                state = cur_state;
                provider_locs;
                binding_kind = stored_binding_kind;
                _;
              } as entry
            ) =
          get_entry var binding_kind entries
        in
        let declare_locs = L.LSet.add loc declare_locs in
        let (state, provider_locs) =
          match (binding_kind, stored_binding_kind) with
          | (Bindings.DeclaredFunction, Bindings.DeclaredFunction) ->
            (* TODO: It would be better if we modeled providers as Inter | UnionLike. This would
             * make it clear that certain providers are meant to be intersected and others
             * are meant to be unioned. *)
            ( Annotated { contextual = false },
              L.LMap.add loc (Annotation { contextual = false }) provider_locs
            )
          | (_, Bindings.DeclaredFunction) -> (cur_state, provider_locs)
          | _ ->
            let new_state = extended_state_opt ~state:cur_state ~write_state in
            Base.Option.value_map
              ~f:(fun state -> (state, L.LMap.add loc write_state provider_locs))
              ~default:(cur_state, provider_locs)
              new_state
        in
        let entries = SMap.add var { entry with declare_locs; state; provider_locs } entries in
        let env = reconstruct_env entries in
        this#set_acc (env, cx)

      method! declare_variable _loc (decl : ('loc, 'loc) Ast.Statement.DeclareVariable.t) =
        let { Ast.Statement.DeclareVariable.id = ident; annot; kind; comments = _ } = decl in
        let (_ : ('a, 'b) Ast.Type.annotation) = this#type_annotation annot in
        this#visit_in_context
          ~mod_cx:(fun _cx -> { init_state = Annotation { contextual = false } })
          (fun () -> this#pattern_identifier ~kind ident);
        decl

      method! variable_declarator
          ~kind (decl : ('loc, 'loc) Ast.Statement.VariableDeclaration.Declarator.t) =
        let (_, { Ast.Statement.VariableDeclaration.Declarator.id; init }) = decl in
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
          | (_, Some (Ast.Type.Available _)) -> Annotation { contextual = false }
          | (None, _) -> Nothing
          | (Some (_, Ast.Expression.Array { Ast.Expression.Array.elements = []; _ }), _) ->
            EmptyArr
          | (Some (_, Ast.Expression.NullLiteral _), _) -> Null 0
          | _ -> Value 0
        in
        this#visit_in_context
          ~mod_cx:(fun _cx -> { init_state })
          (fun () -> this#variable_declarator_pattern ~kind id);
        run_opt this#expression init;
        decl

      method! function_declaration loc (expr : ('loc, 'loc) Ast.Function.t) =
        let open Ast.Function in
        let {
          id = ident;
          params;
          body;
          predicate;
          return;
          tparams;
          async = _;
          generator = _;
          effect_ = _;
          sig_loc = _;
          comments = _;
        } =
          expr
        in
        let init_state =
          match return with
          | Ast.Function.ReturnAnnot.Available _ -> Annotation { contextual = false }
          | _ -> Value 0
        in
        Base.Option.iter ident ~f:(fun id ->
            this#visit_in_context
              ~mod_cx:(fun _cx -> { init_state })
              (fun () -> this#function_identifier id)
        );
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some tparams)
          ~kind:Var
          (fun _ _ ->
            run this#function_params params;
            run this#function_return_annotation return;
            run this#function_body_any body;
            run_opt this#predicate predicate;
            run_opt (this#type_params ~kind:Flow_ast_mapper.FunctionTP) tparams;
            expr)
          loc
          expr

      method! function_ _loc (expr : ('loc, 'loc) Ast.Function.t) =
        let open Ast.Function in
        let {
          id = ident;
          params;
          body;
          predicate;
          return;
          tparams;
          async = _;
          generator = _;
          effect_ = _;
          sig_loc = _;
          comments = _;
        } =
          expr
        in
        let init_state =
          match return with
          | Ast.Function.ReturnAnnot.Available _ -> Annotation { contextual = false }
          | _ -> Value 0
        in
        Base.Option.iter ident ~f:(fun id ->
            this#visit_in_context
              ~mod_cx:(fun _cx -> { init_state })
              (fun () -> this#function_identifier id)
        );
        run this#function_params params;
        run this#function_return_annotation return;
        run this#function_body_any body;
        run_opt this#predicate predicate;
        run_opt (this#type_params ~kind:Flow_ast_mapper.FunctionTP) tparams;
        expr

      method! component_declaration loc (expr : ('loc, 'loc) Ast.Statement.ComponentDeclaration.t) =
        let open Ast.Statement.ComponentDeclaration in
        let { id = ident; tparams; params; body; renders; comments = _; sig_loc = _ } = expr in
        let init_state = Annotation { contextual = false } in
        this#visit_in_context
          ~mod_cx:(fun _cx -> { init_state })
          (fun () -> this#component_identifier ident);
        this#enter_possibly_polymorphic_scope
          ~is_polymorphic:(Option.is_some tparams)
          ~kind:Var
          (fun _ _ ->
            run_opt (this#type_params ~kind:Flow_ast_mapper.ComponentDeclarationTP) tparams;
            run this#component_params params;
            run this#component_body body;
            run this#component_renders_annotation renders;
            expr)
          loc
          expr

      method! pattern_identifier ?kind ((loc, { Ast.Identifier.name; comments = _ }) as ident) =
        begin
          match kind with
          | Some kind ->
            let binding_kind =
              match kind with
              | Ast.Variable.Var -> Bindings.Var
              | Ast.Variable.Let -> Bindings.Let
              | Ast.Variable.Const -> Bindings.Const
            in
            this#new_entry name binding_kind kind loc
          | _ -> ()
        end;
        super#identifier ident

      method! function_this_param ((loc, _) as this_) =
        this#new_entry "this" Bindings.Const Ast.Variable.Const loc;
        super#function_this_param this_

      method! function_identifier ((loc, { Ast.Identifier.name; comments = _ }) as ident) =
        this#new_entry name Bindings.Function Flow_ast.Variable.Let loc;
        super#identifier ident

      method! component_identifier ((loc, { Ast.Identifier.name; comments = _ }) as ident) =
        this#new_entry name Bindings.Component Flow_ast.Variable.Let loc;
        super#identifier ident

      method! declare_function stmt_loc ({ Flow_ast.Statement.DeclareFunction.id; _ } as stmt) =
        let (loc, { Flow_ast.Identifier.name; _ }) = id in
        this#new_entry name Bindings.DeclaredFunction Flow_ast.Variable.Let loc;
        super#declare_function stmt_loc stmt

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
                Annotation { contextual = false }
              | _ -> Value 0
            in
            this#visit_in_context
              ~mod_cx:(fun _cx -> { init_state })
              (fun () -> this#variable_declarator_pattern ~kind id)
          | _ -> raise (ImpossibleState "unexpected AST node"))
        | _ ->
          raise
            (ImpossibleState
               "Syntactically valid for-in loops must have exactly one left declaration"
            ));
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
                Annotation { contextual = false }
              | _ -> Value 0
            in
            this#visit_in_context
              ~mod_cx:(fun _cx -> { init_state })
              (fun () -> this#variable_declarator_pattern ~kind id)
          | _ -> raise (ImpossibleState "unexpected AST node"))
        | _ ->
          raise
            (ImpossibleState
               "Syntactically valid for-in loops must have exactly one left declaration"
            ));
        left

      method! function_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
        (* NOTE: All function parameters are considered annotated, whether this
         * annotation is explicitly provided, is contextually inferred, or is
         * implicitly `any` when missing. *)
        let contextual =
          let open Ast.Pattern in
          match expr with
          | (_, Array { Array.annot = Ast.Type.Available _; _ })
          | (_, Object { Object.annot = Ast.Type.Available _; _ })
          | (_, Identifier { Identifier.annot = Ast.Type.Available _; _ }) ->
            false
          | _ -> true
        in
        let init_state = Annotation { contextual } in
        this#visit_in_context
          ~mod_cx:(fun _cx -> { init_state })
          (fun () -> super#function_param_pattern expr);
        expr

      method! component_param_pattern (expr : ('loc, 'loc) Ast.Pattern.t) =
        let init_state = Annotation { contextual = false } in
        this#visit_in_context
          ~mod_cx:(fun _cx -> { init_state })
          (fun () -> super#component_param_pattern expr);
        expr
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
    try Nel.cons (L.LMap.find loc children) env with
    | Not_found -> raise (ImpossibleState "Missing lexical child at expected position")

  type find_providers_cx = { mk_state: int -> write_state }

  (* This visitor finds variable assignments that are not declarations and adds them to the providers for that variable
     if appropriate. *)
  class find_providers ~env:init_env () =
    object (this)
      inherit
        [find_providers_cx] finder
          ~env:init_env
          ~cx:{ mk_state = (fun n -> Value n) }
          ~enter_lex_child:enter_existing_lex_child as super

      (* Add a new variable provider to the scope, which is not a declaration. *)
      method add_provider var loc =
        let (env, ({ mk_state } as cx)) = this#acc in
        let find_polymorphic_scope scopes =
          Base.List.find scopes ~f:(fun scope -> scope.kind = Polymorphic)
        in
        let ( ({ state = cur_state; provider_locs; def_locs; _ } as entry),
              var_scopes_off,
              def_scopes_stack,
              reconstruct_env
            ) =
          find_entry_for_existing_variable var env
        in
        (* Adding providers in generic functions can potentially cause generic-escape issues,
           so we consevatively prevent them from being providers. *)
        let possible_generic_escape =
          match def_scopes_stack with
          | None -> false (* Not a generic escape if the variable is a global *)
          | Some def_scopes_stack ->
            (match
               (find_polymorphic_scope (Nel.to_list env), find_polymorphic_scope def_scopes_stack)
             with
            | (Some _, None) -> true
            | (Some assign_scope, Some declare_scope)
              when assign_scope.kind = Polymorphic && assign_scope != declare_scope ->
              true
            | _ -> false)
        in
        let write_state = mk_state var_scopes_off in
        let extended_state =
          if possible_generic_escape then
            Some cur_state
          else
            extended_state_opt ~state:cur_state ~write_state
        in
        let def_locs = L.LSet.add loc def_locs in
        let (state, provider_locs) =
          Base.Option.value_map
            ~f:(fun state ->
              ( state,
                if possible_generic_escape then
                  provider_locs
                else
                  L.LMap.add loc write_state provider_locs
              ))
            ~default:(cur_state, provider_locs)
            extended_state
        in
        let possible_generic_escape_locs =
          if possible_generic_escape then
            L.LSet.add loc entry.possible_generic_escape_locs
          else
            entry.possible_generic_escape_locs
        in
        let env =
          reconstruct_env
            { entry with state; provider_locs; def_locs; possible_generic_escape_locs }
        in
        this#set_acc (env, cx)

      method! assignment
          _loc ({ Ast.Expression.Assignment.operator = _; left; right; comments = _ } as expr) =
        let mk_state n =
          match right with
          | (_, Ast.Expression.NullLiteral _) -> Null n
          | (_, Ast.Expression.Array { Ast.Expression.Array.elements = _ :: _; _ }) -> ArrayValue n
          | _ -> Value n
        in
        let (left, _) = Flow_ast_utils.unwrap_nonnull_lhs left in
        this#visit_in_context
          ~mod_cx:(fun _cx -> { mk_state })
          (fun () -> this#assignment_pattern left);
        run this#expression right;
        expr

      method! pattern_identifier ?kind ((loc, { Ast.Identifier.name; comments = _ }) as ident) =
        begin
          match kind with
          | None -> this#add_provider name loc
          | Some _ -> ()
        end;
        super#identifier ident

      method! unary_expression loc expr =
        let { Ast.Expression.Unary.argument; operator; _ } = expr in
        match (argument, operator) with
        | ( (_, Ast.Expression.Identifier (loc, { Ast.Identifier.name; _ })),
            Ast.Expression.Unary.Delete
          ) ->
          this#add_provider name loc;
          expr
        | _ -> super#unary_expression loc expr

      method! expression expr =
        let open Ast.Expression.Member in
        let open Ast.Expression.Assignment in
        let open Ast.Expression.Call in
        let expr =
          match expr with
          | (loc, Ast.Expression.Assignment ({ Ast.Expression.Assignment.left; _ } as assignment))
            ->
            let (left, _) = Flow_ast_utils.unwrap_nonnull_lhs left in
            (loc, Ast.Expression.Assignment { assignment with Ast.Expression.Assignment.left })
          | _ -> expr
        in
        begin
          match expr with
          | ( _,
              Ast.Expression.Call
                {
                  callee =
                    ( _,
                      Ast.Expression.Member
                        {
                          _object = (_, Ast.Expression.Identifier (_, { Ast.Identifier.name; _ }));
                          property = PropertyIdentifier (_, { Ast.Identifier.name = "push"; _ });
                          _;
                        }
                    );
                  targs = None;
                  arguments =
                    ( _,
                      {
                        Ast.Expression.ArgList.arguments = [Ast.Expression.Expression (arg_loc, _)];
                        _;
                      }
                    );
                  _;
                }
            )
          | ( _,
              Ast.Expression.Assignment
                {
                  operator = None;
                  left =
                    ( _,
                      Ast.Pattern.Expression
                        ( _,
                          Ast.Expression.Member
                            {
                              _object =
                                (_, Ast.Expression.Identifier (_, { Ast.Identifier.name; _ }));
                              property = PropertyExpression _;
                              _;
                            }
                        )
                    );
                  right = (arg_loc, _);
                  _;
                }
            ) ->
            let (env, _) = this#acc in
            let state = state_of_var name env in
            begin
              match state with
              | Some (EmptyArrInitialized | ArrInitialized _) ->
                let mk_state n = ArrWrite n in
                this#visit_in_context
                  ~mod_cx:(fun _cx -> { mk_state })
                  (fun () -> this#add_provider name arg_loc)
              | _ -> ()
            end
          | _ -> ()
        end;
        super#expression expr
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
          | (Annotation _, Annotated _) -> { acc with writes = L.LMap.add loc Ordinary acc.writes }
          | (Annotation _, _) -> raise (ImpossibleState "Invariant violated")
          | (Null n, (NullInitialized m | Initialized (_, Some m)))
          | ((ArrayValue n | Value n), Initialized (m, _)) ->
            if n = m then
              { acc with writes = L.LMap.add loc Ordinary acc.writes }
            else if n < m then
              raise (ImpossibleState "Invariant violated")
            else
              acc
          | (ArrayValue n, ArrInitialized m) ->
            if n = m then
              { acc with writes = L.LMap.add loc Ordinary acc.writes }
            else if n < m then
              raise (ImpossibleState "Invariant violated")
            else
              acc
          | (ArrWrite n, ArrInitialized m) ->
            if n = m then
              { acc with array_writes = L.LSet.add loc acc.array_writes }
            else if n < m then
              raise (ImpossibleState "Invariant violated")
            else
              acc
          | (EmptyArr, (EmptyArrInitialized | ArrInitialized _)) ->
            { acc with writes = L.LMap.add loc EmptyArray acc.writes }
          | (Nothing, _)
          | (ArrWrite _, Initialized _)
          | (Value _, ArrInitialized _) ->
            raise (ImpossibleState "Invariant violated")
          | (Null _, (Initialized (_, None) | ArrInitialized _))
          | (EmptyArr, (Uninitialized | Annotated _ | Initialized _ | NullInitialized _))
          | ( (Value _ | ArrayValue _ | ArrWrite _ | Null _),
              (EmptyArrInitialized | Uninitialized | Annotated _ | NullInitialized _)
            ) ->
            acc)
        provider_locs
        { writes = L.LMap.empty; array_writes = L.LSet.empty }
    in
    let state =
      match state with
      | Annotated { contextual } -> AnnotatedVar { contextual }
      | ArrInitialized _ -> ArrayInitializedVar
      | EmptyArrInitialized -> EmptyArrayInitializedVar
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
    Base.Option.map ~f:(fun entry -> (simplify_providers entry).provider_locs.writes) entry
end
