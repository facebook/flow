(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is responsible for building a mapping from variable reads to the
 * writes and refinements that reach those reads. It is based on the implementation of the
 * ssa_builder, but with enough divergent behavior that the ssa_builder and env_builder don't
 * actually share much code. If you're here to add support for a new syntax feature, you'll likely
 * need to modify the ssa_builder as well, but not necessarily with identical changes.*)

let statement_error = ()

let global_TODO = ()

open Reason
open Hoister

(* These functions are adapted from typing/refinement.ml. Eventually, this will be the only place
 * where refinement logic lives, so jmbrown is ok with this temporary duplication while he is
 * fleshing out the refinement features of EnvBuilder
 *
 * The purpose of these functions is to extract _what_ is being refined when we have something like
 * expr != null. What in expr does this refine? *)
let rec key =
  let open Flow_ast.Expression in
  function
  | (_, Identifier id) -> key_of_identifier id
  | _ ->
    (* other LHSes unsupported currently/here *)
    None

and key_of_identifier (_, { Flow_ast.Identifier.name; comments = _ }) =
  if name = "undefined" then
    None
  else
    Some name

type optional_chain_state =
  | NewChain
  | ContinueChain
  | NonOptional

let is_call_to_invariant callee =
  match callee with
  | (_, Flow_ast.Expression.Identifier (_, { Flow_ast.Identifier.name = "invariant"; _ })) -> true
  | _ -> false

let property_of_sentinel_refinement { Flow_ast.Expression.Member.property; _ } =
  let open Flow_ast in
  match property with
  | Expression.Member.PropertyIdentifier (_, { Identifier.name = prop_name; _ })
  | Expression.Member.PropertyExpression
      (_, Expression.Literal { Literal.value = Literal.String prop_name; _ }) ->
    Some prop_name
  | _ -> None

let rec key_of_optional_chain expr =
  let open Flow_ast.Expression in
  match expr with
  | (_, Call _) -> None
  | (_, Member _) -> None
  | ( _,
      OptionalMember
        {
          OptionalMember.member =
            { Member._object = (_, Identifier (_, { Flow_ast.Identifier.name; _ })); _ };
          _;
        } ) ->
    Some name
  | (_, OptionalMember { OptionalMember.member = { Member._object = subject; _ }; _ })
  | (_, OptionalCall { OptionalCall.call = { Call.callee = subject; _ }; _ }) ->
    key_of_optional_chain subject
  | _ -> None

let is_number_literal node =
  let open Flow_ast in
  match node with
  | Expression.Literal { Literal.value = Literal.Number _; _ }
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.Literal { Literal.value = Literal.Number _; _ });
        comments = _;
      } ->
    true
  | _ -> false

let extract_number_literal node =
  let open Flow_ast in
  match node with
  | Expression.Literal { Literal.value = Literal.Number lit; raw; comments = _ } -> (lit, raw)
  | Expression.Unary
      {
        Expression.Unary.operator = Expression.Unary.Minus;
        argument = (_, Expression.Literal { Literal.value = Literal.Number lit; raw; _ });
        comments = _;
      } ->
    (-.lit, "-" ^ raw)
  | _ -> Utils_js.assert_false "not a number literal"

let error_todo = ()

module type C = sig
  type t
end

module type F = sig
  type cx

  type loc

  val add_output : cx -> ?trace:Type.trace -> loc Error_message.t' -> unit
end

module type S = sig
  module L : Loc_sig.S

  module Env_api : Env_api.S with module L = L

  type cx

  type abrupt_kind

  exception AbruptCompletionExn of abrupt_kind

  val program_with_scope :
    cx -> (L.t, L.t) Flow_ast.Program.t -> abrupt_kind option * Env_api.env_info

  val program : cx -> (L.t, L.t) Flow_ast.Program.t -> Env_api.values * (int -> Env_api.refinement)
end

module Make
    (L : Loc_sig.S)
    (Scope_api : Scope_api_sig.S with module L = L)
    (Ssa_api : Ssa_api.S with module L = L)
    (Env_api : Env_api.S
                 with module L = L
                  and module Scope_api = Scope_api
                  and module Ssa_api = Ssa_api)
    (Context : C)
    (FlowAPIUtils : F with type cx = Context.t) :
  S with module L = L and module Env_api = Env_api and type cx = Context.t = struct
  let _f = FlowAPIUtils.add_output
  (* To make ocaml not complain, will be removed when FlowAPIUtils module is used *)

  module L = L

  module Scope_builder : Scope_builder_sig.S with module L = L and module Api = Scope_api =
    Scope_builder.Make (L) (Scope_api)

  module Ssa_builder = Ssa_builder.Make (L) (Ssa_api) (Scope_builder)
  module Invalidation_api = Invalidation_api.Make (L) (Scope_api) (Ssa_api)
  module Env_api = Env_api

  module Provider_api : Provider_api.S with type info = Env_api.Provider_api.info and module L = L =
    Env_api.Provider_api

  open Scope_builder
  open Env_api.Refi

  type cx = Context.t

  type refinement_chain =
    | BASE of refinement
    | AND of int * int
    | OR of int * int
    | NOT of int

  type cond_context =
    | SwitchTest
    | OtherTest

  let merge_and ref1 ref2 = AND (ref1, ref2)

  let merge_or ref1 ref2 = OR (ref1, ref2)

  (* For every read of a variable x, we are interested in tracking writes to x
     that can reach that read. Ultimately the writes are going to be represented
     as a list of locations, where each location corresponds to a "single static
     assignment" of the variable in the code. But for the purposes of analysis, it
     is useful to represent these writes with a data type that contains either a
     single write, or a "join" of writes (in compiler terminology, a PHI node), or
     a reference to something that is unknown at a particular point in the AST
     during traversal, but will be known by the time traversal is complete. *)
  module Val : sig
    type t = {
      id: int;
      write_state: write_state;
    }

    and write_state

    module WriteSet : Flow_set.S with type elt = write_state

    val mk_unresolved : unit -> t

    val empty : unit -> t

    val uninitialized : unit -> t

    val merge : t -> t -> t

    val global : string -> t

    val one : L.t virtual_reason -> t

    val all : L.t virtual_reason list -> t

    val resolve : unresolved:t -> t -> unit

    val simplify : t -> Env_api.write_loc list

    val id_of_val : t -> int

    val refinement : int -> t -> t

    val unrefine : int -> t -> t

    val unrefine_deeply : int -> t -> t

    val normalize_through_refinements : write_state -> WriteSet.t

    val is_global_undefined : t -> bool
  end = struct
    let curr_id = ref 0

    type ref_state =
      (* different unresolved vars are distinguished by their ids, which enables using structural
         equality for computing normal forms: see below *)
      | Unresolved of int
      | Resolved of write_state

    and write_state =
      | Uninitialized
      | Global of string
      | Loc of L.t virtual_reason
      | PHI of write_state list
      | Refinement of {
          refinement_id: int;
          val_t: t;
        }
      (* TODO: These are only used to model havoc locs. If bindings propagated the location of the
       * binding along with the name then we could directly query the Provider_api at the
       * mk_env call instead of relying on this indirection *)
      | REF of ref_state ref

    and t = {
      id: int;
      write_state: write_state;
    }

    let is_global_undefined t =
      match t.write_state with
      | Global "undefined" -> true
      | _ -> false

    let new_id () =
      let id = !curr_id in
      curr_id := !curr_id + 1;
      id

    let mk_with_write_state write_state =
      let id = new_id () in
      { id; write_state }

    let mk_unresolved () =
      let id = new_id () in
      mk_with_write_state @@ REF (ref (Unresolved id))

    let empty () = mk_with_write_state @@ PHI []

    let uninitialized () = mk_with_write_state Uninitialized

    let refinement refinement_id val_t = mk_with_write_state @@ Refinement { refinement_id; val_t }

    let rec unrefine_deeply_write_state id write_state =
      match write_state with
      | Refinement { refinement_id; val_t } when refinement_id = id ->
        unrefine_deeply_write_state id val_t.write_state
      | Refinement { refinement_id; val_t } ->
        let val_t' = mk_with_write_state @@ unrefine_deeply_write_state id val_t.write_state in
        Refinement { refinement_id; val_t = val_t' }
      | PHI ts ->
        let ts' = ListUtils.ident_map (unrefine_deeply_write_state id) ts in
        if ts' == ts then
          write_state
        else
          PHI ts'
      | _ -> write_state

    let unrefine_deeply id t = mk_with_write_state @@ unrefine_deeply_write_state id t.write_state

    let unrefine id t =
      match t.write_state with
      | Refinement { refinement_id; val_t } when refinement_id = id -> val_t
      | _ -> t

    let join = function
      | [] -> PHI []
      | [t] -> t
      | ts -> PHI ts

    module WriteSet = Flow_set.Make (struct
      type t = write_state

      let compare = Stdlib.compare
    end)

    let rec normalize (t : write_state) : WriteSet.t =
      match t with
      | Uninitialized
      | Global _
      | Loc _
      | REF { contents = Unresolved _ }
      | Refinement _ ->
        WriteSet.singleton t
      | PHI ts ->
        List.fold_left
          (fun vals' t ->
            let vals = normalize t in
            WriteSet.union vals' vals)
          WriteSet.empty
          ts
      | REF ({ contents = Resolved t } as r) ->
        let vals = normalize t in
        let t' = join (WriteSet.elements vals) in
        r := Resolved t';
        vals

    let merge t1 t2 =
      if t1.id = t2.id then
        t1
      else
        (* Merging can easily lead to exponential blowup in size of terms if we're not careful. We
           amortize costs by computing normal forms as sets of "atomic" terms, so that merging would
           correspond to set union. (Atomic terms include Uninitialized, Loc _, and REF { contents =
           Unresolved _ }.) Note that normal forms might change over time, as unresolved refs become
           resolved; thus, we do not shortcut normalization of previously normalized terms. Still, we
           expect (and have experimentally validated that) the cost of computing normal forms becomes
           smaller over time as terms remain close to their final normal forms. *)
        let vals = WriteSet.union (normalize t1.write_state) (normalize t2.write_state) in
        mk_with_write_state @@ join (WriteSet.elements vals)

    let rec normalize_through_refinements (t : write_state) : WriteSet.t =
      match t with
      | Uninitialized
      | Global _
      | Loc _
      | REF { contents = Unresolved _ } ->
        WriteSet.singleton t
      | PHI ts ->
        List.fold_left
          (fun vals' t ->
            let vals = normalize t in
            WriteSet.union vals' vals)
          WriteSet.empty
          ts
      | REF ({ contents = Resolved t } as r) ->
        let vals = normalize t in
        let t' = join (WriteSet.elements vals) in
        r := Resolved t';
        vals
      | Refinement { val_t; _ } -> normalize_through_refinements val_t.write_state

    let global name = mk_with_write_state @@ Global name

    let one reason = mk_with_write_state @@ Loc reason

    let all locs = mk_with_write_state @@ join (Base.List.map ~f:(fun reason -> Loc reason) locs)

    (* Resolving unresolved to t essentially models an equation of the form
       unresolved = t, where unresolved is a reference to an unknown and t is the
       known. Since the only non-trivial operation in t is joining, it is OK to
       erase any occurrences of unresolved in t: if t = unresolved | t' then
       unresolved = t is the same as unresolved = t'.

       This can all go away if we can get rid of unresolved/REF *)
    let rec resolve ~unresolved t =
      match unresolved.write_state with
      | REF ({ contents = Unresolved _ } as r) -> r := Resolved (erase r t.write_state)
      | _ -> failwith "Only an unresolved REF can be resolved"

    and erase r t =
      match t with
      | Uninitialized -> t
      | Global _ -> t
      | Loc _ -> t
      | Refinement _ -> failwith "refinements cannot appear inside a REF"
      | PHI ts ->
        let ts' = ListUtils.ident_map (erase r) ts in
        if ts' == ts then
          t
        else
          PHI ts'
      | REF r' ->
        if r == r' then
          PHI []
        else
          let t_opt = !r' in
          let t_opt' =
            match t_opt with
            | Unresolved _ -> t_opt
            | Resolved t ->
              let t' = erase r t in
              if t == t' then
                t_opt
              else
                Resolved t'
          in
          if t_opt != t_opt' then r' := t_opt';
          t

    (* Simplification converts a Val.t to a list of locations. *)
    let rec simplify t =
      let vals = normalize t.write_state in
      Base.List.map
        ~f:(function
          | Uninitialized -> Env_api.Uninitialized
          | Loc r -> Env_api.Write r
          | Refinement { refinement_id; val_t } ->
            Env_api.Refinement { writes = simplify val_t; refinement_id }
          | Global name -> Env_api.Global name
          | REF _
          | PHI _ ->
            failwith "A normalized value cannot be a PHI or REF")
        (WriteSet.elements vals)

    let id_of_val { id; write_state = _ } = id
  end

  (* An environment is a map from variables to values. *)
  module Env = struct
    type t = Val.t SMap.t
  end

  (* Abrupt completions induce control flows, so modeling them accurately is
     necessary for soundness. *)
  module AbruptCompletion = struct
    type label = string

    type t =
      | Break of label option
      | Continue of label option
      | Return
      | Throw

    let label_opt = Base.Option.map ~f:Flow_ast_utils.name_of_ident

    let break x = Break (label_opt x)

    let continue x = Continue (label_opt x)

    let return = Return

    let throw = Throw

    (* match particular abrupt completions *)
    let mem list : t -> bool = (fun t -> List.mem t list)

    (* match all abrupt completions *)
    let all : t -> bool = (fun _t -> true)

    (* Model an abrupt completion as an OCaml exception. *)
    exception Exn of t

    (* An abrupt completion carries an environment, which is the current
       environment at the point where the abrupt completion is "raised." This
       environment is merged wherever the abrupt completion is "handled." *)
    type env = t * Env.t
  end

  (* Collect all values assigned to a variable, as a conservative fallback when we
     don't have precise information. *)
  module Havoc = struct
    type t = {
      unresolved: Val.t;
      mutable locs: L.t Reason.virtual_reason list;
    }
  end

  let rec list_iter3 f l1 l2 l3 =
    match (l1, l2, l3) with
    | ([], [], []) -> ()
    | (x1 :: l1, x2 :: l2, x3 :: l3) ->
      f x1 x2 x3;
      list_iter3 f l1 l2 l3
    | _ -> assert false

  type abrupt_kind = AbruptCompletion.t

  exception AbruptCompletionExn = AbruptCompletion.Exn

  type env_val = {
    val_ref: Val.t ref;
    havoc: Havoc.t;
  }

  type latest_refinement = {
    ssa_id: int;
    refinement_id: int;
  }

  type env_builder_state = {
    (* We maintain a map of read locations to raw Val.t terms, which are
       simplified to lists of write locations once the analysis is done. *)
    values: Val.t L.LMap.t;
    (* We also maintain a list of all write locations, for use in populating the env with
       types. *)
    write_entries: L.t virtual_reason L.LMap.t;
    curr_id: int;
    (* Maps refinement ids to refinements. This mapping contains _all_ the refinements reachable at
     * any point in the code. The latest_refinement maps keep track of which entries to read. *)
    refinement_heap: refinement_chain IMap.t;
    latest_refinements: latest_refinement SMap.t list;
    env: env_val SMap.t;
    (* When an abrupt completion is raised, it falls through any subsequent
       straight-line code, until it reaches a merge point in the control-flow
       graph. At that point, it can be re-raised if and only if all other reaching
       control-flow paths also raise the same abrupt completion.

       When re-raising is not possible, we have to save the abrupt completion and
       the current environment in a list, so that we can merge such environments
       later (when that abrupt completion and others like it are handled).

       Even when raising is possible, we still have to save the current
       environment, since the current environment will have to be cleared to model
       that the current values of all variables are unreachable.

       NOTE that raising is purely an optimization: we can have more precise
       results with raising, but even if we never raised we'd still be sound. *)
    abrupt_completion_envs: AbruptCompletion.env list;
    (* Track the list of labels that might describe a loop. Used to detect which
       labeled continues need to be handled by the loop.

       The idea is that a labeled statement adds its label to the list before
       entering its child, and if the child is not a loop or another labeled
       statement, the list will be cleared. A loop will consume the list, so we
       also clear the list on our way out of any labeled statement. *)
    possible_labeled_continues: AbruptCompletion.t list;
  }

  let initialize_globals unbound_names =
    SSet.fold
      (fun name acc ->
        let unresolved = Val.mk_unresolved () in
        (* When we havoc we go back to the original global definition *)
        Val.resolve ~unresolved (Val.global name);
        let entry = { val_ref = ref (Val.global name); havoc = { Havoc.unresolved; locs = [] } } in
        SMap.add name entry acc)
      unbound_names
      SMap.empty

  class env_builder _cx (prepass_info, prepass_values, unbound_names) provider_info =
    object (this)
      inherit Scope_builder.scope_builder ~flowmin_compatibility:false ~with_types:true as super

      val invalidation_caches = Invalidation_api.mk_caches ()

      val mutable env_state : env_builder_state =
        {
          values = L.LMap.empty;
          write_entries = L.LMap.empty;
          curr_id = 0;
          refinement_heap = IMap.empty;
          latest_refinements = [];
          env = initialize_globals unbound_names;
          abrupt_completion_envs = [];
          possible_labeled_continues = [];
        }

      method values : Env_api.values = L.LMap.map Val.simplify env_state.values

      method write_entries : L.t virtual_reason L.LMap.t = env_state.write_entries

      method private new_id () =
        let new_id = env_state.curr_id in
        let curr_id = new_id + 1 in
        env_state <- { env_state with curr_id };
        new_id

      method env : Env.t = SMap.map (fun { val_ref; _ } -> !val_ref) env_state.env

      (* We often want to merge the refinement scopes and writes of two environments with
       * different strategies, especially in logical refinement scopes. In order to do that, we
       * need to be able to get the writes in our env without the refinement writes. Then we
       * can merge the refinements from two environments using either AND or OR, and then we can
       * merge the writes and reapply the merged refinement if the ssa_id in unchanged.
       *
       * An alternative implementation here might have just used PHI nodes to model disjunctions
       * and successive refinement writes to model conjunctions, but it's not clear that that
       * approach is simpler than this one. *)
      method env_without_latest_refinements : Env.t =
        SMap.mapi
          (fun name { val_ref; _ } ->
            let head = List.hd env_state.latest_refinements in
            match SMap.find_opt name head with
            | None -> !val_ref
            | Some { refinement_id; _ } -> Val.unrefine refinement_id !val_ref)
          env_state.env

      method merge_remote_env (env : Env.t) : unit =
        (* NOTE: env might have more keys than env, since the environment it
           describes might be nested inside the current environment *)
        SMap.iter
          (fun x { val_ref; _ } -> val_ref := Val.merge !val_ref (SMap.find x env))
          env_state.env

      method merge_env (env1 : Env.t) (env2 : Env.t) : unit =
        let env1 = SMap.values env1 in
        let env2 = SMap.values env2 in
        let env = SMap.values env_state.env in
        list_iter3
          (fun { val_ref; _ } value1 value2 -> val_ref := Val.merge value1 value2)
          env
          env1
          env2

      method merge_self_env (other_env : Env.t) : unit =
        let other_env = SMap.values other_env in
        let env = SMap.values env_state.env in
        List.iter2 (fun { val_ref; _ } value -> val_ref := Val.merge !val_ref value) env other_env

      method reset_env (env0 : Env.t) : unit =
        let env0 = SMap.values env0 in
        let env = SMap.values env_state.env in
        List.iter2 (fun { val_ref; _ } value -> val_ref := value) env env0

      method empty_env : Env.t = SMap.map (fun _ -> Val.empty ()) env_state.env

      method havoc_current_env ~all =
        SMap.iter
          (fun _x { val_ref; havoc = { Havoc.unresolved; locs } } ->
            match locs with
            | loc :: _
              when Invalidation_api.should_invalidate
                     ~all
                     invalidation_caches
                     prepass_info
                     prepass_values
                     loc ->
              (* NOTE: havoc_env should already have all writes to x, so the only
                 additional thing that could come from ssa_env is "uninitialized." On
                 the other hand, we *dont* want to include "uninitialized" if it's no
                 longer in ssa_env, since that means that x has been initialized (and
                 there's no going back). *)
              val_ref := unresolved
            | [] ->
              (* If we haven't yet seen a write to this variable, we always havoc *)
              val_ref := unresolved
            | _ -> ())
          env_state.env

      method havoc_uninitialized_env =
        SMap.iter (fun _x { val_ref; havoc } -> val_ref := havoc.Havoc.unresolved) env_state.env

      method private mk_env =
        SMap.map (fun (_, (loc, _)) ->
            let providers =
              Base.Option.value ~default:[] (Provider_api.providers_of_def provider_info loc)
            in
            let write_entries =
              Base.List.fold
                ~f:(fun acc r -> L.LMap.add (poly_loc_of_reason r) r acc)
                ~init:env_state.write_entries
                providers
            in
            env_state <- { env_state with write_entries };
            {
              val_ref = ref (Val.uninitialized ());
              havoc = Havoc.{ unresolved = Val.mk_unresolved (); locs = providers };
            })

      method private push_env bindings =
        let old_env = env_state.env in
        let bindings = Bindings.to_map bindings in
        let env = SMap.fold SMap.add (this#mk_env bindings) old_env in
        env_state <- { env_state with env };
        (bindings, old_env)

      method private resolve_havocs =
        SMap.iter (fun x _loc ->
            let { havoc = { Havoc.unresolved; locs }; _ } = SMap.find x env_state.env in
            Val.resolve ~unresolved (Val.all locs))

      method private pop_env (bindings, old_env) =
        this#resolve_havocs bindings;
        env_state <- { env_state with env = old_env }

      method! with_bindings : 'a. ?lexical:bool -> L.t -> L.t Bindings.t -> ('a -> 'a) -> 'a -> 'a =
        fun ?lexical loc bindings visit node ->
          let saved_state = this#push_env bindings in
          this#run
            (fun () -> ignore @@ super#with_bindings ?lexical loc bindings visit node)
            ~finally:(fun () -> this#pop_env saved_state);
          node

      (* Run some computation, catching any abrupt completions; do some final work,
         and then re-raise any abrupt completions that were caught. *)
      method run f ~finally =
        let completion_state = this#run_to_completion f in
        finally ();
        this#from_completion completion_state

      method run_to_completion f =
        try
          f ();
          None
        with
        | AbruptCompletion.Exn abrupt_completion -> Some abrupt_completion

      method from_completion =
        function
        | None -> ()
        | Some abrupt_completion -> raise (AbruptCompletion.Exn abrupt_completion)

      method raise_abrupt_completion : 'a. AbruptCompletion.t -> 'a =
        fun abrupt_completion ->
          let env = this#env in
          this#reset_env this#empty_env;
          let abrupt_completion_envs =
            (abrupt_completion, env) :: env_state.abrupt_completion_envs
          in
          env_state <- { env_state with abrupt_completion_envs };
          raise (AbruptCompletion.Exn abrupt_completion)

      method expecting_abrupt_completions f =
        let saved = env_state.abrupt_completion_envs in
        let saved_latest_refinements = env_state.latest_refinements in
        env_state <- { env_state with abrupt_completion_envs = [] };
        this#run f ~finally:(fun () ->
            let abrupt_completion_envs = List.rev_append saved env_state.abrupt_completion_envs in
            env_state <-
              {
                env_state with
                abrupt_completion_envs;
                latest_refinements = saved_latest_refinements;
              })

      (* Given multiple completion states, (re)raise if all of them are the same
         abrupt completion. This function is called at merge points. *)
      method merge_completion_states (hd_completion_state, tl_completion_states) =
        match hd_completion_state with
        | None -> ()
        | Some abrupt_completion ->
          if
            List.for_all
              (function
                | None -> false
                | Some abrupt_completion' -> abrupt_completion = abrupt_completion')
              tl_completion_states
          then
            raise (AbruptCompletion.Exn abrupt_completion)

      (* Given a filter for particular abrupt completions to expect, find the saved
         environments corresponding to them, and merge those environments with the
         current environment. This function is called when exiting ASTs that
         introduce (and therefore expect) particular abrupt completions. *)
      method commit_abrupt_completion_matching filter completion_state =
        let (matching, non_matching) =
          List.partition
            (fun (abrupt_completion, _env) -> filter abrupt_completion)
            env_state.abrupt_completion_envs
        in
        if matching <> [] then (
          List.iter (fun (_abrupt_completion, env) -> this#merge_remote_env env) matching;
          env_state <- { env_state with abrupt_completion_envs = non_matching }
        ) else
          match completion_state with
          | Some abrupt_completion when not (filter abrupt_completion) ->
            raise (AbruptCompletion.Exn abrupt_completion)
          | _ -> ()

      method! pattern_identifier ?kind ident =
        ignore kind;
        let (loc, { Flow_ast.Identifier.name = x; comments = _ }) = ident in
        let reason = Reason.(mk_reason (RIdentifier (OrdinaryName x))) loc in
        begin
          match SMap.find_opt x env_state.env with
          | Some { val_ref; _ } ->
            val_ref := Val.one reason;
            let write_entries = L.LMap.add loc reason env_state.write_entries in
            env_state <- { env_state with write_entries }
          | _ -> ()
        end;
        super#identifier ident

      (* This method is called during every read of an identifier. We need to ensure that
       * if the identifier is refined that we record the refiner as the write that reaches
       * this read *)
      method any_identifier loc name =
        match SMap.find_opt name env_state.env with
        | Some { val_ref; _ } ->
          let values = L.LMap.add loc !val_ref env_state.values in
          env_state <- { env_state with values }
        | None -> ()

      method! identifier (ident : (L.t, L.t) Ast.Identifier.t) =
        let (loc, { Ast.Identifier.name = x; comments = _ }) = ident in
        this#any_identifier loc x;
        super#identifier ident

      method! jsx_element_name_identifier (ident : (L.t, L.t) Ast.JSX.Identifier.t) =
        let (loc, { Ast.JSX.Identifier.name; comments = _ }) = ident in
        this#any_identifier loc name;
        super#jsx_identifier ident

      method! jsx_element_name_namespaced ns =
        (* TODO: what identifiers does `<foo:bar />` read? *)
        super#jsx_element_name_namespaced ns

      (* Order of evaluation matters *)
      method! assignment _loc (expr : (L.t, L.t) Ast.Expression.Assignment.t) =
        let open Ast.Expression.Assignment in
        let { operator; left; right; comments = _ } = expr in
        begin
          match operator with
          | None ->
            let open Ast.Pattern in
            begin
              match left with
              | (_, (Identifier _ | Object _ | Array _)) ->
                (* given `x = e`, read e then write x *)
                ignore @@ this#expression right;
                ignore @@ this#assignment_pattern left
              | (_, Expression _) ->
                (* given `o.x = e`, read o then read e *)
                ignore @@ this#assignment_pattern left;
                ignore @@ this#expression right
            end
          | Some _ ->
            let open Ast.Pattern in
            begin
              match left with
              | (_, Identifier { Identifier.name; _ }) ->
                (* given `x += e`, read x then read e then write x *)
                ignore @@ this#identifier name;
                ignore @@ this#expression right;
                ignore @@ this#assignment_pattern left
              | (_, Expression _) ->
                (* given `o.x += e`, read o then read e *)
                ignore @@ this#assignment_pattern left;
                ignore @@ this#expression right
              | (_, (Object _ | Array _)) -> statement_error
            end
        end;
        expr

      (* Order of evaluation matters *)
      method! variable_declarator
          ~kind (decl : (L.t, L.t) Ast.Statement.VariableDeclaration.Declarator.t) =
        let open Ast.Statement.VariableDeclaration.Declarator in
        let (_loc, { id; init }) = decl in
        let open Ast.Pattern in
        begin
          match id with
          | (_, (Identifier _ | Object _ | Array _)) ->
            begin
              match init with
              | Some init ->
                (* given `var x = e`, read e then write x *)
                ignore @@ this#expression init;
                ignore @@ this#variable_declarator_pattern ~kind id
              | None ->
                (* `var x;` is not a write of `x` *)
                ()
            end
          | (_, Expression _) -> statement_error
        end;
        decl

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

      (* things that cause abrupt completions *)
      method! break _loc (stmt : L.t Ast.Statement.Break.t) =
        let open Ast.Statement.Break in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.break label)

      method! continue _loc (stmt : L.t Ast.Statement.Continue.t) =
        let open Ast.Statement.Continue in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.continue label)

      method! return _loc (stmt : (L.t, L.t) Ast.Statement.Return.t) =
        let open Ast.Statement.Return in
        let { argument; comments = _ } = stmt in
        ignore @@ Flow_ast_mapper.map_opt this#expression argument;
        this#raise_abrupt_completion AbruptCompletion.return

      method! throw _loc (stmt : (L.t, L.t) Ast.Statement.Throw.t) =
        let open Ast.Statement.Throw in
        let { argument; comments = _ } = stmt in
        ignore @@ this#expression argument;
        this#raise_abrupt_completion AbruptCompletion.throw

      (** Control flow **)
      method! if_statement _loc stmt =
        let open Flow_ast.Statement.If in
        let { test; consequent; alternate; _ } = stmt in
        this#push_refinement_scope SMap.empty;
        ignore @@ this#expression_refinement test;
        let test_refinements = this#peek_new_refinements () in
        let env0 = this#env_without_latest_refinements in
        (* collect completions and environments of every branch *)
        let then_completion_state =
          this#run_to_completion (fun () ->
              ignore @@ this#if_consequent_statement ~has_else:(alternate <> None) consequent)
        in
        let then_env_no_refinements = this#env_without_latest_refinements in
        let then_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#push_refinement_scope test_refinements;
        this#negate_new_refinements ();
        let else_completion_state =
          this#run_to_completion (fun () ->
              ignore
              @@ Flow_ast_mapper.map_opt
                   (fun (loc, { Alternate.body; comments }) ->
                     (loc, { Alternate.body = this#statement body; comments }))
                   alternate)
        in
        (* merge environments *)
        let else_env_no_refinements = this#env_without_latest_refinements in
        let else_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#merge_conditional_branches_with_refinements
          (then_env_no_refinements, then_env_with_refinements)
          (else_env_no_refinements, else_env_with_refinements);

        (* merge completions *)
        let if_completion_states = (then_completion_state, [else_completion_state]) in
        this#merge_completion_states if_completion_states;
        stmt

      method! conditional _loc (expr : (L.t, L.t) Flow_ast.Expression.Conditional.t) =
        let open Flow_ast.Expression.Conditional in
        let { test; consequent; alternate; comments = _ } = expr in
        this#push_refinement_scope SMap.empty;
        ignore @@ this#expression_refinement test;
        let test_refinements = this#peek_new_refinements () in
        let env0 = this#env_without_latest_refinements in
        let consequent_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#expression consequent)
        in
        let consequent_env_no_refinements = this#env_without_latest_refinements in
        let consequent_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#push_refinement_scope test_refinements;
        this#negate_new_refinements ();
        let alternate_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#expression alternate)
        in
        let alternate_env_no_refinements = this#env_without_latest_refinements in
        let alternate_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#merge_conditional_branches_with_refinements
          (consequent_env_no_refinements, consequent_env_with_refinements)
          (alternate_env_no_refinements, alternate_env_with_refinements);

        (* merge completions *)
        let conditional_completion_states =
          (consequent_completion_state, [alternate_completion_state])
        in
        this#merge_completion_states conditional_completion_states;
        expr

      method merge_conditional_branches_with_refinements (env1, refined_env1) (env2, refined_env2)
          : unit =
        (* We only want to merge the refined environments from the two branches of an if-statement
         * if there was an assignment in one of the branches. Otherwise, merging the positive and
         * negative branches of the refinement into a union would be unnecessary work to
         * reconstruct the original type *)
        SMap.iter
          (fun name { val_ref; _ } ->
            let value1 = SMap.find name env1 in
            let value2 = SMap.find name env2 in
            if Val.id_of_val value1 = Val.id_of_val value2 then
              val_ref := value1
            else
              let refined_value1 = SMap.find name refined_env1 in
              let refined_value2 = SMap.find name refined_env2 in
              val_ref := Val.merge refined_value1 refined_value2)
          env_state.env

      method with_env_state f =
        let pre_state = env_state in
        let pre_env = this#env in
        let result = f () in
        env_state <- pre_state;
        (* It's not enough to just restore the old env_state, since the env itself contains
         * refs. We need to call reset_env to _fully_ reset the env_state *)
        this#reset_env pre_env;
        result

      (* Functions called inside scout_changed_vars are responsible for popping any refinement
       * scopes they may introduce
       *)
      method scout_changed_vars ~scout =
        (* Calling scout may have side effects, like adding new abrupt completions. We
         * need to be sure to restore the old abrupt completion envs after scouting,
         * because a scout should be followed-up by a run that revisits everything visited by
         * the scout. with_env_state will ensure that all mutable state is restored. *)
        this#with_env_state (fun () ->
            let pre_env = this#env in
            scout ();
            let post_env = this#env in
            SMap.fold
              (fun name env_val1 acc ->
                let env_val2 = SMap.find name pre_env in
                let normalized_val1 = Val.normalize_through_refinements env_val1.Val.write_state in
                let normalized_val2 = Val.normalize_through_refinements env_val2.Val.write_state in
                if Val.WriteSet.equal normalized_val1 normalized_val2 then
                  acc
                else
                  name :: acc)
              post_env
              [])

      method havoc_changed_vars changed_vars =
        List.iter
          (fun name ->
            let { val_ref; havoc = { Havoc.unresolved; _ } } = SMap.find name env_state.env in
            val_ref := unresolved)
          changed_vars

      method handle_continues loop_completion_state continues =
        this#run_to_completion (fun () ->
            this#commit_abrupt_completion_matching
              (AbruptCompletion.mem continues)
              loop_completion_state)

      (* After a loop we need to negate the loop guard and apply the refinement. The
       * targets of those refinements may have been changed by the loop, but that
       * doesn't matter. The only way to get out of the loop is for the negation of
       * the refinement to hold, so we apply that negation even though the ssa_id might
       * not match.
       *
       * The exception here is, of course, if we break out of the loop. If we break
       * inside the loop then we should not negate the refinements because it is
       * possible that we just exited the loop by breaking.
       *
       * We don't need to check for continues because they are handled before this point.
       * We don't check for throw/return because then we wouldn't proceed to the line
       * after the loop anyway. *)
      method post_loop_refinements refinements =
        if not AbruptCompletion.(mem (List.map fst env_state.abrupt_completion_envs) (break None))
        then
          refinements
          |> SMap.iter (fun name { refinement_id; ssa_id = _ } ->
                 let { val_ref; _ } = SMap.find name env_state.env in
                 let new_refinement_id = this#new_id () in
                 env_state <-
                   {
                     env_state with
                     refinement_heap =
                       IMap.add new_refinement_id (NOT refinement_id) env_state.refinement_heap;
                   };
                 val_ref := Val.refinement new_refinement_id !val_ref)

      (*
       * Unlike the ssa_builder, the env_builder does not create REF unresolved
       * Val.ts to model the write states of variables in loops. This approach
       * would cause a lot of cycles in the ordering algorithm, which means
       * we'd need to ask for a lot of annotations. Moreover, it's not clear where
       * those annotations should go.
       *
       * Instead, we scout the body of the loop to find which variables are
       * written to. If a variable is written, then we havoc that variable
       * before entering the loop. This does not apply to variables that are
       * only refined.
       *
       * After visiting the body, we reset the state in the ssa environment,
       * havoc any vars that need to be havoced, and then visit the body again.
       * After that we negate the refinements on the loop guard.
       *
       * Here's how each param should be used:
       * scout: Visit the guard and any updaters if applicable, then visit the body
       * visit_guard_and_body: Visit the guard with a refinement scope, any updaters
       *   if applicable, and then visit the body. Return a tuple of the guard
       *   refinement scope, the env after the guard with no refinements, and the loop
       *   completion state.
       * make_completion_states: given the loop completion state, give the list of
       *   possible completion states for the loop. For do while loops this is different
       *   than regular while loops, so those two implementations may be instructive.
       * auto_handle_continues: Every loop needs to filter out continue completion states.
       *   The default behavior is to do that filtering at the end of the body.
       *   If you need to handle continues before that, like in a do/while loop, then
       *   set this to false. Ensure that you handle continues in both the scouting and
       *   main passes.
       *)
      method env_loop
          ~scout ~visit_guard_and_body ~make_completion_states ~auto_handle_continues ~continues =
        this#expecting_abrupt_completions (fun () ->
            (* Scout the body for changed vars *)
            let changed_vars = this#scout_changed_vars ~scout in

            (* We havoc the changed vars in order to prevent loops in the EnvBuilder writes-graph,
             * which would require a fix-point analysis that would not be compatible with
             * local type inference *)
            this#havoc_changed_vars changed_vars;

            (* Now we push a refinement scope and visit the guard/body. At the end, we completely
             * get rid of refinements introduced by the guard, even if they occur in a PHI node, to
             * ensure that the refinement does not escape the loop via something like
             * control flow. For example:
             * while (x != null) {
             *   if (x == 3) {
             *     x = 4;
             *   }
             * }
             * x; // Don't want x to be a PHI of x != null and x = 4.
             *)
            this#push_refinement_scope SMap.empty;
            let (guard_refinements, env_after_test_no_refinements, loop_completion_state) =
              visit_guard_and_body ()
            in
            let loop_completion_state =
              if auto_handle_continues then
                this#handle_continues loop_completion_state continues
              else
                loop_completion_state
            in
            this#pop_refinement_scope_after_loop ();

            (* We either enter the loop body or we don't *)
            (match env_after_test_no_refinements with
            | None -> ()
            | Some env -> this#merge_self_env env);
            this#post_loop_refinements guard_refinements;

            let completion_states = make_completion_states loop_completion_state in
            let completion_state =
              this#run_to_completion (fun () -> this#merge_completion_states completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state)

      method! while_ _loc (stmt : (L.t, L.t) Flow_ast.Statement.While.t) =
        let open Flow_ast.Statement.While in
        let { test; body; comments = _ } = stmt in
        let scout () =
          ignore @@ this#expression test;
          ignore @@ this#run_to_completion (fun () -> ignore @@ this#statement body)
        in
        let visit_guard_and_body () =
          ignore @@ this#expression_refinement test;
          let guard_refinements = this#peek_new_refinements () in
          let post_guard_no_refinements_env = this#env_without_latest_refinements in
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          (guard_refinements, Some post_guard_no_refinements_env, loop_completion_state)
        in
        let make_completion_states loop_completion_state = (None, [loop_completion_state]) in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        this#env_loop
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:true
          ~continues;
        stmt

      method! do_while _loc stmt =
        let open Flow_ast.Statement.DoWhile in
        let { test; body; comments = _ } = stmt in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        let scout () =
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          ignore @@ this#handle_continues loop_completion_state continues;
          match loop_completion_state with
          | None -> ignore @@ this#expression test
          | Some _ -> ()
        in
        let visit_guard_and_body () =
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          let loop_completion_state = this#handle_continues loop_completion_state continues in
          (match loop_completion_state with
          | None -> ignore @@ this#expression_refinement test
          | Some _ -> ());
          (this#peek_new_refinements (), None, loop_completion_state)
        in
        let make_completion_states loop_completion_state = (loop_completion_state, []) in
        this#env_loop
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:false
          ~continues;
        stmt

      method! scoped_for_statement _loc stmt =
        let open Flow_ast.Statement.For in
        let { init; test; update; body; comments = _ } = stmt in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        let scout () =
          ignore @@ Flow_ast_mapper.map_opt this#for_statement_init init;
          ignore @@ Flow_ast_mapper.map_opt this#expression test;
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          let loop_completion_state = this#handle_continues loop_completion_state continues in
          match loop_completion_state with
          | None -> ignore @@ Flow_ast_mapper.map_opt this#expression update
          | Some _ -> ()
        in
        let visit_guard_and_body () =
          ignore @@ Flow_ast_mapper.map_opt this#for_statement_init init;
          ignore @@ Flow_ast_mapper.map_opt this#expression_refinement test;
          let guard_refinements = this#peek_new_refinements () in
          let post_guard_no_refinements_env = this#env_without_latest_refinements in
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          let loop_completion_state = this#handle_continues loop_completion_state continues in
          (match loop_completion_state with
          | None -> ignore @@ Flow_ast_mapper.map_opt this#expression update
          | Some _ -> ());
          (guard_refinements, Some post_guard_no_refinements_env, loop_completion_state)
        in
        let make_completion_states loop_completion_state = (None, [loop_completion_state]) in
        this#env_loop
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:false
          ~continues;
        stmt

      method for_in_or_of_left_declaration left =
        let (_, decl) = left in
        let open Flow_ast.Statement.VariableDeclaration in
        let { declarations; kind; comments = _ } = decl in
        match declarations with
        | [(_, { Flow_ast.Statement.VariableDeclaration.Declarator.id; init = _ })] ->
          let open Flow_ast.Pattern in
          (match id with
          | (_, (Identifier _ | Object _ | Array _)) ->
            ignore @@ this#variable_declarator_pattern ~kind id
          | _ -> failwith "unexpected AST node")
        | _ -> failwith "Syntactically valid for-in loops must have exactly one left declaration"

      method! for_in_left_declaration left =
        this#for_in_or_of_left_declaration left;
        left

      method! for_of_left_declaration left =
        this#for_in_or_of_left_declaration left;
        left

      method scoped_for_in_or_of_statement traverse_left right body =
        (* This is only evaluated once and so does not need to be scouted
         * You might be wondering why the lhs has to be scouted-- the LHS can be a pattern that
         * includes a default write with a variable that is written to inside the loop. It's
         * critical that we catch loops in the dependency graph with such variables, since the
         * ordering algorithm will not have a good place to ask for an annotation in that case.
         *)
        ignore @@ this#expression right;
        let scout () =
          traverse_left ();
          ignore @@ this#run_to_completion (fun () -> ignore @@ this#statement body)
        in
        let visit_guard_and_body () =
          traverse_left ();
          let env = this#env in
          let loop_completion_state =
            this#run_to_completion (fun () -> ignore @@ this#statement body)
          in
          (this#peek_new_refinements (), Some env, loop_completion_state)
        in
        let make_completion_states loop_completion_state = (None, [loop_completion_state]) in
        let continues = AbruptCompletion.continue None :: env_state.possible_labeled_continues in
        this#env_loop
          ~scout
          ~visit_guard_and_body
          ~make_completion_states
          ~auto_handle_continues:true
          ~continues

      method! scoped_for_in_statement _loc stmt =
        let open Flow_ast.Statement.ForIn in
        let { left; right; body; each = _; comments = _ } = stmt in
        let traverse_left () = ignore (this#for_in_statement_lhs left) in
        this#scoped_for_in_or_of_statement traverse_left right body;
        stmt

      method! scoped_for_of_statement _loc stmt =
        let open Flow_ast.Statement.ForOf in
        let { left; right; body; await = _; comments = _ } = stmt in
        let traverse_left () = ignore (this#for_of_statement_lhs left) in
        this#scoped_for_in_or_of_statement traverse_left right body;
        stmt

      (***********************************************************)
      (* [PRE] switch (e) { case e1: s1 ... case eN: sN } [POST] *)
      (***********************************************************)
      (*     |                                                   *)
      (*     e                                                   *)
      (*    /                                                    *)
      (*   e1                                                    *)
      (*   | \                                                   *)
      (*   .  s1                                                 *)
      (*   |   |                                                 *)
      (*   ei  .                                                 *)
      (*   | \ |                                                 *)
      (*   .  si                                                 *)
      (*   |   |                                                 *)
      (*   eN  .                                                 *)
      (*   | \ |                                                 *)
      (*   |  sN                                                 *)
      (*    \  |                                                 *)
      (*      \|                                                 *)
      (*       |                                                 *)
      (***********************************************************)
      (* [PRE] e [ENV0]                                          *)
      (* ENV0' = empty                                           *)
      (* \forall i = 0..N-1:                                     *)
      (*   [ENVi] ei+1 [ENVi+1]                                  *)
      (*   [ENVi+1 | ENVi'] si+1 [ENVi+1']                       *)
      (* POST = ENVN | ENVN'                                     *)
      (***********************************************************)
      method! switch_cases discriminant cases =
        this#expecting_abrupt_completions (fun () ->
            let (env, case_completion_states, _total_refinements, has_default) =
              List.fold_left
                (fun acc stuff ->
                  let (_loc, case) = stuff in
                  this#env_switch_case discriminant acc case)
                (this#empty_env, [], [], false)
                cases
            in
            (* Only merge the pre-env if the switch was non-exhaustive
             * TODO: Each refinement that ends in a break will be reachable via
             * the PHI node at the end of the switch. Should we get rid of these refinements?
             *)
            if has_default then
              this#reset_env env
            else
              this#merge_self_env env;

            (* In general, cases are non-exhaustive, but if it has a default case then it is! *)
            let completion_state =
              if has_default then
                (* Since there is a default we know there is at least one element in this
                 * list, which means calling List.hd or tail will not fail *)
                let first_state = List.hd case_completion_states in
                let remaining_states = List.tl case_completion_states in
                this#run_to_completion (fun () ->
                    this#merge_completion_states (first_state, remaining_states))
              else
                None
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        cases

      method private env_switch_case
          discriminant
          (env, case_completion_states, total_refinements, has_default)
          (case : (L.t, L.t) Ast.Statement.Switch.Case.t') =
        let open Ast.Statement.Switch.Case in
        let { test; consequent; comments = _ } = case in
        let (has_default, total_refinements) =
          match test with
          | None ->
            (* In the default case we negate the refinements introduced by all the other cases and
             * AND them together. Much of Flow's "exhaustiveness" checking relies on the final
             * refinement generated here *)
            let negated_total_refinements = List.map this#negate_refinements total_refinements in
            let conjuncted_negated_total_refinements =
              this#conjunct_all_refinements_for_key (key discriminant) negated_total_refinements
            in
            this#push_refinement_scope conjuncted_negated_total_refinements;
            (true, total_refinements)
          | Some test ->
            this#push_refinement_scope SMap.empty;
            ignore @@ this#expression test;
            let (loc, _) = test in
            this#eq_test ~strict:true ~sense:true ~cond_context:SwitchTest loc discriminant test;
            (has_default, this#peek_new_refinements () :: total_refinements)
        in
        (* The refinement scope for this case is a disjunction of the refinement scope left
         * over from the previous case and the refinement introduced by this case. If the previous
         * case ended in a break then the previous refinement scope left over is None.
         *
         * This disjunction is modeled entirely via PHI nodes. We take the env left over from the
         * previous case and then merge it with the refined env0 we generated here.
         *)
        let env0 = this#env in
        let env0_no_refinements = this#env_without_latest_refinements in
        this#merge_env env0 env;
        let case_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#statement_list consequent)
        in
        let env' = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0_no_refinements;
        (env', case_completion_state :: case_completion_states, total_refinements, has_default)

      (****************************************)
      (* [PRE] try { s1 } catch { s2 } [POST] *)
      (****************************************)
      (*    |                                 *)
      (*    s1 ..~                            *)
      (*    |    |                            *)
      (*    |   s2                            *)
      (*     \./                              *)
      (*      |                               *)
      (****************************************)
      (* [PRE] s1 [ENV1]                      *)
      (* [HAVOC] s2 [ENV2 ]                   *)
      (* POST = ENV1 | ENV2                   *)
      (****************************************)
      (*******************************************************)
      (* [PRE] try { s1 } catch { s2 } finally { s3 } [POST] *)
      (*******************************************************)
      (*    |                                                *)
      (*    s1 ..~                                           *)
      (*    |    |                                           *)
      (*    |   s2 ..~                                       *)
      (*     \./     |                                       *)
      (*      |______|                                       *)
      (*             |                                       *)
      (*            s3                                       *)
      (*             |                                       *)
      (*******************************************************)
      (* [PRE] s1 [ENV1]                                     *)
      (* [HAVOC] s2 [ENV2 ]                                  *)
      (* [HAVOC] s3 [ENV3 ]                                  *)
      (* POST = ENV3                                         *)
      (*******************************************************)
      method! try_catch _loc (stmt : (L.t, L.t) Ast.Statement.Try.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Try in
            let { block = (loc, block); handler; finalizer; comments = _ } = stmt in
            let try_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#block loc block)
            in
            let env1 = this#env in
            let (catch_completion_state_opt, env2) =
              match handler with
              | Some (loc, clause) ->
                (* NOTE: Havoc-ing the state when entering the handler is probably
                   overkill. We can be more precise but still correct by collecting all
                   possible writes in the try-block and merging them with the state when
                   entering the try-block. *)
                this#havoc_current_env ~all:false;
                let catch_completion_state =
                  this#run_to_completion (fun () -> ignore @@ this#catch_clause loc clause)
                in
                ([catch_completion_state], this#env)
              | None -> ([], this#empty_env)
            in
            this#merge_env env1 env2;
            let try_catch_completion_states = (try_completion_state, catch_completion_state_opt) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states try_catch_completion_states)
            in
            this#commit_abrupt_completion_matching AbruptCompletion.all completion_state;
            begin
              match finalizer with
              | Some (_loc, block) ->
                (* NOTE: Havoc-ing the state when entering the finalizer is probably
                   overkill. We can be more precise but still correct by collecting
                   all possible writes in the handler and merging them with the state
                   when entering the handler (which in turn should already account for
                   any contributions by the try-block). *)
                this#havoc_current_env ~all:false;
                ignore @@ this#block loc block
              | None -> ()
            end;
            this#from_completion completion_state);
        stmt

      (* We also havoc state when entering functions and exiting calls. *)
      method! lambda params body =
        this#expecting_abrupt_completions (fun () ->
            let env = this#env in
            this#run
              (fun () ->
                this#havoc_uninitialized_env;
                let completion_state =
                  this#run_to_completion (fun () -> super#lambda params body)
                in
                this#commit_abrupt_completion_matching
                  AbruptCompletion.(mem [return; throw])
                  completion_state)
              ~finally:(fun () -> this#reset_env env))

      method! declare_function loc expr =
        match Declare_function_utils.declare_function_to_function_declaration_simple loc expr with
        | Some stmt ->
          let _ = this#statement (loc, stmt) in
          expr
        | None -> super#declare_function loc expr

      method! call loc (expr : (L.t, L.t) Ast.Expression.Call.t) =
        (* Traverse everything up front. Now we don't need to worry about missing any reads
         * of identifiers in sub-expressions *)
        ignore @@ super#call loc expr;

        let open Ast.Expression.Call in
        let { callee; targs; arguments; _ } = expr in
        if is_call_to_invariant callee then
          match (targs, arguments) with
          (* invariant() and invariant(false, ...) are treated like throw *)
          | (None, (_, { Ast.Expression.ArgList.arguments = []; comments = _ })) ->
            this#raise_abrupt_completion AbruptCompletion.throw
          | ( None,
              ( _,
                {
                  Ast.Expression.ArgList.arguments =
                    Ast.Expression.Expression
                      ( _,
                        Ast.Expression.Literal { Ast.Literal.value = Ast.Literal.Boolean false; _ }
                      )
                    :: other_args;
                  comments = _;
                } ) ) ->
            let _ = List.map this#expression_or_spread other_args in
            this#raise_abrupt_completion AbruptCompletion.throw
          | ( None,
              ( _,
                {
                  Ast.Expression.ArgList.arguments = Ast.Expression.Expression cond :: other_args;
                  comments = _;
                } ) ) ->
            this#push_refinement_scope SMap.empty;
            ignore @@ this#expression_refinement cond;
            let _ = List.map this#expression_or_spread other_args in
            this#pop_refinement_scope_invariant ()
          | ( _,
              (_, { Ast.Expression.ArgList.arguments = Ast.Expression.Spread _ :: _; comments = _ })
            ) ->
            error_todo
          | (Some _, _) -> error_todo
        else
          this#havoc_current_env ~all:false;
        expr

      method! new_ _loc (expr : (L.t, L.t) Ast.Expression.New.t) =
        let open Ast.Expression.New in
        let { callee; targs = _; arguments; comments = _ } = expr in
        ignore @@ this#expression callee;
        ignore @@ Flow_ast_mapper.map_opt this#call_arguments arguments;
        this#havoc_current_env ~all:false;
        expr

      method! unary_expression _loc (expr : (L.t, L.t) Ast.Expression.Unary.t) =
        Ast.Expression.Unary.(
          let { argument; operator; comments = _ } = expr in
          ignore @@ this#expression argument;
          begin
            match operator with
            | Await -> this#havoc_current_env ~all:false
            | _ -> ()
          end;
          expr)

      method! yield loc (expr : ('loc, 'loc) Ast.Expression.Yield.t) =
        ignore @@ super#yield loc expr;
        this#havoc_current_env ~all:true;
        expr

      (* Labeled statements handle labeled breaks, but also push labeled continues
         that are expected to be handled by immediately nested loops. *)
      method! labeled_statement _loc (stmt : (L.t, L.t) Ast.Statement.Labeled.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Labeled in
            let { label; body; comments = _ } = stmt in
            env_state <-
              {
                env_state with
                possible_labeled_continues =
                  AbruptCompletion.continue (Some label) :: env_state.possible_labeled_continues;
              };
            let completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            env_state <- { env_state with possible_labeled_continues = [] };
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break (Some label)])
              completion_state);
        stmt

      method! statement (stmt : (L.t, L.t) Ast.Statement.t) =
        let open Ast.Statement in
        begin
          match stmt with
          | (_, While _)
          | (_, DoWhile _)
          | (_, For _)
          | (_, ForIn _)
          | (_, ForOf _)
          | (_, Labeled _) ->
            ()
          | _ -> env_state <- { env_state with possible_labeled_continues = [] }
        end;
        super#statement stmt

      (* Function declarations are hoisted to the top of a block, so that they may be considered
         initialized before they are read. *)
      method! statement_list (stmts : (L.t, L.t) Ast.Statement.t list) =
        let open Ast.Statement in
        let (function_decls, other_stmts) =
          List.partition
            (function
              | (_, FunctionDeclaration _) -> true
              | _ -> false)
            stmts
        in
        ignore @@ super#statement_list (function_decls @ other_stmts);
        stmts

      (* WHen the refinement scope we push is non-empty we want to make sure that the variables
       * that scope refines are given their new refinement writes in the environment *)
      method private push_refinement_scope new_latest_refinements =
        env_state <-
          {
            env_state with
            latest_refinements = new_latest_refinements :: env_state.latest_refinements;
          };
        new_latest_refinements
        |> SMap.iter (fun name latest_refinement ->
               let { val_ref; _ } = SMap.find name env_state.env in
               let ssa_id = Val.id_of_val !val_ref in
               if ssa_id = latest_refinement.ssa_id then
                 val_ref := Val.refinement latest_refinement.refinement_id !val_ref)

      (* See pop_refinement_scope. The only difference here is that we unrefine values deeply
       * instead of just at the top level. The reason for this is that intermediate control-flow
       * can introduce refinement writes into phi nodes, and we don't want those refinements to
       * escape the scope of the loop. You may find it instructive to change the calls to
       * just pop_refinement_scope to see the behavioral differences *)
      method private pop_refinement_scope_after_loop () =
        let refinements = List.hd env_state.latest_refinements in
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements };
        refinements
        |> SMap.iter (fun name latest_refinement ->
               let { val_ref; _ } = SMap.find name env_state.env in
               val_ref := Val.unrefine_deeply latest_refinement.refinement_id !val_ref)

      (* Invariant refinement scopes can be popped, but the refinement should continue living on.
       * To model that, we pop the refinement scope but do not unrefine the refinements. The
       * refinements live on in the Refinement writes in the env. *)
      method private pop_refinement_scope_invariant () =
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements }

      (* When a refinement scope ends, we need to undo the refinement applied to the
       * variables mentioned in the latest_refinements head. Some of these values may no
       * longer be the refined value, in which case Val.unrefine will be a no-op. Otherwise,
       * the Refinement Val.t is replaced with the original Val.t that was being refined, with
       * the same original ssa_id. That means that if for some reason you needed to push the refinement
       * scope again that you would re-refine the unrefined variables, which is desirable in cases
       * where we juggle refinement scopes like we do for nullish coalescing *)
      method private pop_refinement_scope () =
        let refinements = List.hd env_state.latest_refinements in
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements };
        refinements
        |> SMap.iter (fun name latest_refinement ->
               let { val_ref; _ } = SMap.find name env_state.env in
               val_ref := Val.unrefine latest_refinement.refinement_id !val_ref)

      method private peek_new_refinements () = List.hd env_state.latest_refinements

      method private negate_refinements refinements =
        SMap.map
          (fun latest_refinement ->
            let new_id = this#new_id () in
            let new_ref = NOT latest_refinement.refinement_id in
            env_state <-
              { env_state with refinement_heap = IMap.add new_id new_ref env_state.refinement_heap };
            { latest_refinement with refinement_id = new_id })
          refinements

      method private conjunct_all_refinements_for_key key refinement_scopes =
        match key with
        | None -> SMap.empty
        | Some key ->
          let (total_refinement_opt, _) =
            List.fold_left
              (fun (total_refinement, mismatched_ids) refinement_scope ->
                (* ids can be mismatched if the case expression contains an assignment. This should
                 * be exceedingly rare, but we have to account for it nonetheless. *)
                if mismatched_ids then
                  (None, true)
                else
                  match (total_refinement, SMap.find_opt key refinement_scope) with
                  | (None, Some refinement)
                  | (Some refinement, None) ->
                    (Some refinement, mismatched_ids)
                  | (None, None) -> (None, mismatched_ids)
                  | (Some ref1, Some ref2) ->
                    if ref1.ssa_id = ref2.ssa_id then (
                      let new_refinement = AND (ref1.refinement_id, ref2.refinement_id) in
                      let new_refinement_id = this#new_id () in
                      env_state <-
                        {
                          env_state with
                          refinement_heap =
                            IMap.add new_refinement_id new_refinement env_state.refinement_heap;
                        };
                      let new_latest_refinement =
                        { ssa_id = ref1.ssa_id; refinement_id = new_refinement_id }
                      in
                      (Some new_latest_refinement, mismatched_ids)
                    ) else
                      (None, false))
              (None, false)
              refinement_scopes
          in
          (match total_refinement_opt with
          | None -> SMap.empty
          | Some refinement -> SMap.singleton key refinement)

      method private negate_new_refinements () =
        let head = List.hd env_state.latest_refinements in
        let new_latest_refinements = this#negate_refinements head in
        this#pop_refinement_scope ();
        this#push_refinement_scope new_latest_refinements

      method private merge_self_refinement_scope new_refinements =
        let head = List.hd env_state.latest_refinements in
        let head' =
          SMap.merge
            (fun _ latest1 latest2 ->
              match (latest1, latest2) with
              | (_, None) -> latest1
              | (_, Some _) -> latest2)
            head
            new_refinements
        in
        this#pop_refinement_scope ();
        this#push_refinement_scope head'

      method private add_refinement name refinement =
        let refinement_id = this#new_id () in
        env_state <-
          {
            env_state with
            refinement_heap = IMap.add refinement_id (BASE refinement) env_state.refinement_heap;
          };
        match SMap.find_opt name env_state.env with
        | Some { val_ref; _ } ->
          let ssa_id = Val.id_of_val !val_ref in
          let head = List.hd env_state.latest_refinements in
          let latest_refinement =
            match SMap.find_opt name head with
            | None -> { ssa_id; refinement_id }
            | Some { ssa_id = prev_ssa_id; refinement_id = prev_refinement_id } ->
              let unrefined = Val.unrefine prev_refinement_id !val_ref in
              val_ref := unrefined;
              let unrefined_id = Val.id_of_val unrefined in
              if unrefined_id = prev_ssa_id then (
                let new_refinement_id = this#new_id () in
                let new_chain = AND (prev_refinement_id, refinement_id) in
                env_state <-
                  {
                    env_state with
                    refinement_heap = IMap.add new_refinement_id new_chain env_state.refinement_heap;
                  };
                { ssa_id = prev_ssa_id; refinement_id = new_refinement_id }
              ) else
                { ssa_id; refinement_id }
          in
          let head' = SMap.add name latest_refinement head in
          env_state <-
            { env_state with latest_refinements = head' :: List.tl env_state.latest_refinements };
          val_ref := Val.refinement latest_refinement.refinement_id !val_ref
        | None -> global_TODO

      method identifier_refinement ((loc, ident) as identifier) =
        ignore @@ this#identifier identifier;
        let { Flow_ast.Identifier.name; _ } = ident in
        this#add_refinement name (L.LSet.singleton loc, TruthyR loc)

      method assignment_refinement loc assignment =
        ignore @@ this#assignment loc assignment;
        let open Flow_ast.Expression.Assignment in
        match assignment.left with
        | ( id_loc,
            Flow_ast.Pattern.Identifier
              { Flow_ast.Pattern.Identifier.name = (_, { Flow_ast.Identifier.name; _ }); _ } ) ->
          this#add_refinement name (L.LSet.singleton loc, TruthyR id_loc)
        | _ -> ()

      method private merge_refinement_scopes
          ~merge
          (lhs_latest_refinements : latest_refinement SMap.t)
          (rhs_latest_refinements : latest_refinement SMap.t) =
        let new_latest_refinements =
          SMap.merge
            (fun _ ref1 ref2 ->
              match (ref1, ref2) with
              | (None, None) -> None
              | (Some ref, None) -> Some ref
              | (None, Some ref) -> Some ref
              | (Some ref1, Some ref2) ->
                let new_ref = merge ref1.refinement_id ref2.refinement_id in
                let new_id = this#new_id () in
                env_state <-
                  {
                    env_state with
                    refinement_heap = IMap.add new_id new_ref env_state.refinement_heap;
                  };
                Some { ref1 with refinement_id = new_id })
            lhs_latest_refinements
            rhs_latest_refinements
        in
        this#merge_self_refinement_scope new_latest_refinements

      method logical_refinement expr =
        let { Flow_ast.Expression.Logical.operator; left = (loc, _) as left; right; comments = _ } =
          expr
        in
        this#push_refinement_scope SMap.empty;
        (* The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
         * x || invariant(false) should propagate the truthy refinement to the next line. We keep track
         * of the completion state on the rhs to do that. If the LHS throws then the entire expression
         * throws, so there's no need to catch the exception from the LHS *)
        let (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state) =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.And ->
            ignore @@ this#expression_refinement left;
            let lhs_latest_refinements = this#peek_new_refinements () in
            let env1 = this#env_without_latest_refinements in
            (match operator with
            | Flow_ast.Expression.Logical.Or -> this#negate_new_refinements ()
            | _ -> ());
            this#push_refinement_scope SMap.empty;
            let rhs_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#expression_refinement right)
            in
            let rhs_latest_refinements = this#peek_new_refinements () in
            (* Pop LHS refinement scope *)
            this#pop_refinement_scope ();
            (* Pop RHS refinement scope *)
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state)
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            (* If this overall expression is truthy, then either the LHS or the RHS has to be truthy.
               If it's because the LHS is truthy, then the LHS also has to be non-maybe (this is of course
               true by definition, but it's also true because of the nature of ??).
               But if we're evaluating the RHS, the LHS doesn't have to be truthy, it just has to be
               non-maybe. As a result, we do this weird dance of refinements so that when we traverse the
               RHS we have done the null-test but the overall result of this expression includes both the
               truthy and non-maybe qualities. *)
            ignore (this#null_test ~strict:false ~sense:false loc left);
            let nullish = this#peek_new_refinements () in
            let env1 = this#env_without_latest_refinements in
            this#negate_new_refinements ();
            this#push_refinement_scope SMap.empty;
            let rhs_completion_state =
              this#run_to_completion (fun () -> ignore (this#expression_refinement right))
            in
            let rhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#pop_refinement_scope ();
            this#push_refinement_scope SMap.empty;
            (match key left with
            | None -> ()
            | Some name -> this#add_refinement name (L.LSet.singleton loc, TruthyR loc));
            let truthy_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#push_refinement_scope SMap.empty;
            this#merge_refinement_scopes merge_and nullish truthy_refinements;
            let lhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state)
        in
        let merge =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            merge_or
          | Flow_ast.Expression.Logical.And -> merge_and
        in
        match rhs_completion_state with
        | Some AbruptCompletion.Throw ->
          let env2 = this#env in
          this#reset_env env1;
          this#push_refinement_scope lhs_latest_refinements;
          this#pop_refinement_scope_invariant ();
          this#merge_self_env env2
        | _ ->
          this#merge_self_env env1;
          this#merge_refinement_scopes merge lhs_latest_refinements rhs_latest_refinements

      method null_test ~strict ~sense loc expr =
        ignore @@ this#expression expr;
        let optional_chain_refinement = this#maybe_sentinel_and_chain_refinement ~sense loc expr in
        match key expr with
        | None -> ()
        | Some name ->
          let refinement =
            if strict then
              NullR
            else
              MaybeR
          in
          let refinement =
            if sense then
              refinement
            else
              NotR refinement
          in
          this#add_refinement name (L.LSet.singleton loc, refinement);
          (match optional_chain_refinement with
          | Some name ->
            (* Optional chaining with ==/=== null can be tricky. If the value before ? is
             * null or undefined then the entire chain evaluates to undefined. That leaves us
             * with these cases:
             * a?.b === null THEN a non-maybe and a.b null ELSE a maybe or a.b non-null
             * a?.b == null THEN no refinement ELSE a non maybe and a.b non-null
             * TODO: figure out how to model the negation of an optional chain refinement without
             * introducing a second mapping for the negation of refinements.
             *)
            if (strict && sense) || ((not sense) && not strict) then
              this#add_refinement name (L.LSet.singleton loc, NotR MaybeR)
          | None -> ())

      method void_test ~sense ~strict ~check_for_bound_undefined loc expr =
        ignore @@ this#expression expr;
        let optional_chain_refinement = this#maybe_sentinel_and_chain_refinement ~sense loc expr in
        let is_global_undefined () =
          match SMap.find_opt "undefined" env_state.env with
          | None -> false
          | Some { val_ref = v; _ } -> Val.is_global_undefined !v
        in
        match key expr with
        | None -> ()
        | Some name ->
          (* Only add the refinement if undefined is not re-bound *)
          if (not check_for_bound_undefined) || is_global_undefined () then (
            let refinement =
              if strict then
                UndefinedR
              else
                MaybeR
            in
            let refinement =
              if sense then
                refinement
              else
                NotR refinement
            in
            this#add_refinement name (L.LSet.singleton loc, refinement);
            match optional_chain_refinement with
            | None -> ()
            | Some name ->
              (* Optional chaining against void is also difficult... (see null_test)
               * a?.b === undefined THEN a maybe or a.b is undefined ELSE a non-maybe and a.b not undefined
               * a?.b == undefined THEN a maybe or a.b maybe ELSE a non-maybe and a.b not undefined
               * TODO: we can't model this disjunction without heap refinements, so until then we
               * won't add any refinements for the sense && strict and sense && not strict cases *)
              if not sense then this#add_refinement name (L.LSet.singleton loc, NotR MaybeR)
          )

      method default_optional_chain_refinement_handler ~sense loc expr =
        match this#maybe_sentinel_and_chain_refinement ~sense loc expr with
        | None -> ()
        | Some name -> this#add_refinement name (L.LSet.singleton loc, NotR MaybeR)

      method typeof_test loc arg typename sense =
        ignore @@ this#expression arg;
        this#default_optional_chain_refinement_handler ~sense loc arg;
        let refinement =
          match typename with
          | "boolean" -> Some (BoolR loc)
          | "function" -> Some FunctionR
          | "number" -> Some (NumberR loc)
          | "object" -> Some ObjectR
          | "string" -> Some (StringR loc)
          | "symbol" -> Some (SymbolR loc)
          | "undefined" -> Some UndefinedR
          | _ -> None
        in
        match (refinement, key arg) with
        | (Some ref, Some name) ->
          let refinement =
            if sense then
              ref
            else
              NotR ref
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)
        | _ -> ()

      method literal_test ~strict ~sense loc expr refinement =
        ignore @@ this#expression expr;
        this#default_optional_chain_refinement_handler ~sense loc expr;
        match key expr with
        | Some name when strict ->
          let refinement =
            if sense then
              refinement
            else
              NotR refinement
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)
        | _ -> ()

      method maybe_sentinel_and_chain_refinement ~sense loc expr =
        let open Flow_ast in
        let expr' =
          match expr with
          | (loc, Expression.OptionalMember { Expression.OptionalMember.member; _ }) ->
            (loc, Expression.Member member)
          | _ -> expr
        in
        (match expr' with
        | ( _,
            Expression.Member
              {
                Expression.Member._object;
                property =
                  ( Expression.Member.PropertyIdentifier (ploc, { Identifier.name = prop_name; _ })
                  | Expression.Member.PropertyExpression
                      (ploc, Expression.Literal { Literal.value = Literal.String prop_name; _ }) );
                _;
              } ) ->
          (match key _object with
          | Some name ->
            let refinement = SentinelR (prop_name, ploc) in
            let refinement =
              if sense then
                refinement
              else
                NotR refinement
            in
            this#add_refinement name (L.LSet.singleton loc, refinement)
          | None -> ())
        | _ -> ());
        (* We return the refinement to the callers to handle specially. null_test and void_test have the
         * most interesting behaviors *)
        match key_of_optional_chain expr with
        | Some name -> Some name
        | None -> None

      method eq_test ~strict ~sense ~cond_context loc left right =
        let open Flow_ast in
        match (left, right) with
        (* typeof expr ==/=== string *)
        | ( ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ),
            (_, Expression.Literal { Literal.value = Literal.String s; _ }) )
        | ( (_, Expression.Literal { Literal.value = Literal.String s; _ }),
            ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ) )
        | ( ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ),
            ( _,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = s; _ };
                          _;
                        } );
                    ];
                  expressions = [];
                  comments = _;
                } ) )
        | ( ( _,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = s; _ };
                          _;
                        } );
                    ];
                  expressions = [];
                  comments = _;
                } ),
            ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ } ) )
          ->
          this#typeof_test loc argument s sense
        (* bool equality *)
        | ((lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ }), expr)
        | (expr, (lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ })) ->
          this#literal_test ~strict ~sense loc expr (SingletonBoolR { loc = lit_loc; sense; lit })
        (* string equality *)
        | ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }), expr)
        | (expr, (lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }))
        | ( expr,
            ( lit_loc,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = lit; _ };
                          _;
                        } );
                    ];
                  _;
                } ) )
        | ( ( lit_loc,
              Expression.TemplateLiteral
                {
                  Expression.TemplateLiteral.quasis =
                    [
                      ( _,
                        {
                          Expression.TemplateLiteral.Element.value =
                            { Expression.TemplateLiteral.Element.cooked = lit; _ };
                          _;
                        } );
                    ];
                  _;
                } ),
            expr ) ->
          this#literal_test ~strict ~sense loc expr (SingletonStrR { loc = lit_loc; sense; lit })
        (* number equality *)
        | ((lit_loc, number_literal), expr) when is_number_literal number_literal ->
          let raw = extract_number_literal number_literal in
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonNumR { loc = lit_loc; sense; lit = raw })
        | (expr, (lit_loc, number_literal)) when is_number_literal number_literal ->
          let raw = extract_number_literal number_literal in
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonNumR { loc = lit_loc; sense; lit = raw })
        (* expr op null *)
        | ((_, Expression.Literal { Literal.value = Literal.Null; _ }), expr)
        | (expr, (_, Expression.Literal { Literal.value = Literal.Null; _ })) ->
          this#null_test ~sense ~strict loc expr
        (* expr op undefined *)
        | ( ((_, Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ }))
            as undefined),
            expr )
        | ( expr,
            ((_, Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ }))
            as undefined) ) ->
          ignore @@ this#expression undefined;
          this#void_test ~sense ~strict ~check_for_bound_undefined:true loc expr
        (* expr op void(...) *)
        | ((_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ }), expr)
        | (expr, (_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ })) ->
          this#void_test ~sense ~strict ~check_for_bound_undefined:false loc expr
        (* Member expressions compared against non-literals that include
         * an optional chain cannot refine like we do in literal cases. The
         * non-literal value we are comparing against may be null or undefined,
         * in which case we'd need to use the special case behavior. Since we can't
         * know at this point, we conservatively do not refine at all based on optional
         * chains by ignoring the output of maybe_sentinel_and_chain_refinement.
         *
         * NOTE: Switch statements do not introduce sentinel refinements *)
        | (((_, Expression.Member _) as expr), _) ->
          ignore @@ this#expression expr;
          ignore @@ this#maybe_sentinel_and_chain_refinement ~sense loc expr
        | (_, ((_, Expression.Member _) as expr)) when not (cond_context = SwitchTest) ->
          ignore @@ this#expression expr;
          ignore @@ this#maybe_sentinel_and_chain_refinement ~sense loc expr
        | _ ->
          ignore @@ this#expression left;
          ignore @@ this#expression right

      method instance_test loc expr instance =
        ignore @@ this#expression expr;
        ignore @@ this#expression instance;
        match key expr with
        | None -> ()
        | Some name ->
          let (inst_loc, _) = instance in
          this#add_refinement name (L.LSet.singleton loc, InstanceOfR inst_loc)

      method binary_refinement loc expr =
        let open Flow_ast.Expression.Binary in
        let { operator; left; right; comments = _ } = expr in
        let eq_test = this#eq_test ~cond_context:OtherTest in
        match operator with
        (* == and != refine if lhs or rhs is an ident and other side is null *)
        | Equal -> eq_test ~strict:false ~sense:true loc left right
        | NotEqual -> eq_test ~strict:false ~sense:false loc left right
        | StrictEqual -> eq_test ~strict:true ~sense:true loc left right
        | StrictNotEqual -> eq_test ~strict:true ~sense:false loc left right
        | Instanceof -> this#instance_test loc left right
        | LessThan
        | LessThanEqual
        | GreaterThan
        | GreaterThanEqual
        | In
        | LShift
        | RShift
        | RShift3
        | Plus
        | Minus
        | Mult
        | Exp
        | Div
        | Mod
        | BitOr
        | Xor
        | BitAnd ->
          ignore @@ this#binary loc expr

      method call_refinement loc call =
        match call with
        | {
         Flow_ast.Expression.Call.callee =
           ( _,
             Flow_ast.Expression.Member
               {
                 Flow_ast.Expression.Member._object =
                   ( _,
                     Flow_ast.Expression.Identifier
                       (_, { Flow_ast.Identifier.name = "Array"; comments = _ }) );
                 property =
                   Flow_ast.Expression.Member.PropertyIdentifier
                     (_, { Flow_ast.Identifier.name = "isArray"; comments = _ });
                 comments = _;
               } );
         targs = _;
         arguments =
           ( _,
             {
               Flow_ast.Expression.ArgList.arguments = [Flow_ast.Expression.Expression arg];
               comments = _;
             } );
         comments = _;
        } ->
          ignore @@ this#expression arg;
          (match key arg with
          | None -> ()
          | Some name -> this#add_refinement name (L.LSet.singleton loc, IsArrayR))
        | _ -> ignore @@ this#call loc call

      method unary_refinement
          loc ({ Flow_ast.Expression.Unary.operator; argument; comments = _ } as unary) =
        match operator with
        | Flow_ast.Expression.Unary.Not ->
          this#push_refinement_scope SMap.empty;
          ignore @@ this#expression_refinement argument;
          this#negate_new_refinements ();
          let negated_refinements = this#peek_new_refinements () in
          this#pop_refinement_scope ();
          this#merge_self_refinement_scope negated_refinements
        | _ -> ignore @@ this#unary_expression loc unary

      method expression_refinement ((loc, expr) as expression) =
        let open Flow_ast.Expression in
        match expr with
        | Identifier ident ->
          this#identifier_refinement ident;
          expression
        | Logical logical ->
          this#logical_refinement logical;
          expression
        | Assignment assignment ->
          this#assignment_refinement loc assignment;
          expression
        | Binary binary ->
          this#binary_refinement loc binary;
          expression
        | Call call ->
          this#call_refinement loc call;
          expression
        | Unary unary ->
          this#unary_refinement loc unary;
          expression
        | Member _
        | OptionalMember _ ->
          (* TODO: this refinement is technically incorrect when negated until we also track that the
           * property access is truthy, but that is a heap refinement. *)
          this#default_optional_chain_refinement_handler ~sense:true loc (loc, expr);
          expression
        | Array _
        | ArrowFunction _
        | Class _
        | Comprehension _
        | Conditional _
        | Function _
        | Generator _
        | Import _
        | JSXElement _
        | JSXFragment _
        | Literal _
        | MetaProperty _
        | New _
        | Object _
        | OptionalCall _
        | Sequence _
        | Super _
        | TaggedTemplate _
        | TemplateLiteral _
        | TypeCast _
        | This _
        | Update _
        | Yield _ ->
          this#expression expression

      method! logical _loc (expr : (L.t, L.t) Flow_ast.Expression.Logical.t) =
        let open Flow_ast.Expression.Logical in
        let { operator; left = (loc, _) as left; right; comments = _ } = expr in
        this#push_refinement_scope SMap.empty;
        (* THe LHS is unconditionally evaluated, so we don't run-to-completion and catch the
         * error here *)
        (match operator with
        | Flow_ast.Expression.Logical.Or
        | Flow_ast.Expression.Logical.And ->
          ignore (this#expression_refinement left)
        | Flow_ast.Expression.Logical.NullishCoalesce ->
          ignore (this#null_test ~strict:false ~sense:false loc left));
        let env1 = this#env_without_latest_refinements in
        let env1_with_refinements = this#env in
        (match operator with
        | Flow_ast.Expression.Logical.NullishCoalesce
        | Flow_ast.Expression.Logical.Or ->
          this#negate_new_refinements ()
        | Flow_ast.Expression.Logical.And -> ());
        (* The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
         * x || invariant(false) should propagate the truthy refinement to the next line. We keep track
         * of the completion state on the rhs to do that. If the LHS throws then the entire expression
         * throws, so there's no need to catch the exception from the LHS *)
        let rhs_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#expression right)
        in
        (match rhs_completion_state with
        | Some AbruptCompletion.Throw ->
          let env2 = this#env in
          this#reset_env env1_with_refinements;
          this#pop_refinement_scope_invariant ();
          this#merge_self_env env2
        | _ ->
          this#pop_refinement_scope ();
          this#merge_self_env env1);
        expr

      method private chain_to_refinement =
        function
        | BASE refinement -> refinement
        | AND (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id1 env_state.refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (IMap.find id2 env_state.refinement_heap) in
          (L.LSet.union locs1 locs2, AndR (ref1, ref2))
        | OR (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id1 env_state.refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (IMap.find id2 env_state.refinement_heap) in
          (L.LSet.union locs1 locs2, OrR (ref1, ref2))
        | NOT id ->
          let (locs, ref) = this#chain_to_refinement (IMap.find id env_state.refinement_heap) in
          (locs, NotR ref)

      method refinement_of_id id =
        let chain = IMap.find id env_state.refinement_heap in
        this#chain_to_refinement chain
    end

  let program_with_scope cx program =
    let open Hoister in
    let (loc, _) = program in
    let (_ssa_completion_state, ((scopes, ssa_values, _) as prepass)) =
      Ssa_builder.program_with_scope ~flowmin_compatibility:false program
    in
    let providers = Provider_api.find_providers program in
    let env_walk = new env_builder cx prepass providers in
    let bindings =
      let hoist = new hoister ~flowmin_compatibility:false ~with_types:true in
      hoist#eval hoist#program program
    in
    let completion_state =
      env_walk#run_to_completion (fun () ->
          ignore @@ env_walk#with_bindings loc bindings env_walk#program program)
    in
    ( completion_state,
      {
        Env_api.scopes;
        ssa_values;
        env_values = env_walk#values;
        env_entries = env_walk#write_entries;
        providers;
        refinement_of_id = env_walk#refinement_of_id;
      } )

  let program cx program =
    let (_, { Env_api.env_values; refinement_of_id; _ }) = program_with_scope cx program in
    (env_values, refinement_of_id)
end

module DummyCx = struct
  type t = unit
end

module DummyFlow = struct
  type cx = unit

  type loc = Loc.t

  let add_output _ ?trace _ = ignore trace
end

module With_Loc =
  Make (Loc_sig.LocS) (Scope_api.With_Loc) (Ssa_api.With_Loc) (Env_api.With_Loc) (DummyCx)
    (DummyFlow)
module Make_of_flow =
  Make (Loc_sig.ALocS) (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)
