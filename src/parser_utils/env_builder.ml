(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let statement_error = ()

open Reason
open Hoister

(* This module is responsible for building a mapping from variable reads to the
 * writes and refinements that reach those reads. It is based on the implementation of the
 * ssa_builder, but with enough divergent behavior that the ssa_builder and env_builder don't
 * actually share much code. If you're here to add support for a new syntax feature, you'll likely
 * need to modify the ssa_builder as well, but not necessarily with identical changes.*)

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

module type S = sig
  module L : Loc_sig.S

  module Ssa_api : Ssa_api.S with module L = L

  module Scope_api : Scope_api_sig.S with module L = L

  module Provider_api : Provider_api.S with module L = L

  type abrupt_kind

  exception AbruptCompletionExn of abrupt_kind

  type refinement_kind =
    | And of refinement_kind * refinement_kind
    | Or of refinement_kind * refinement_kind
    | Not of refinement_kind
    | Truthy of L.t
    | Null
    | Undefined
    | Maybe
    | InstanceOf of L.t
    | IsArray
    | BoolR of L.t
    | FunctionR
    | NumberR of L.t
    | ObjectR
    | StringR of L.t
    | SymbolR of L.t
    | SingletonBoolR of {
        loc: L.t;
        sense: bool;
        lit: bool;
      }
    | SingletonStrR of {
        loc: L.t;
        sense: bool;
        lit: string;
      }
    | SingletonNumR of {
        loc: L.t;
        sense: bool;
        lit: float * string;
      }
  [@@deriving show { with_path = false }]

  type refinement = L.LSet.t * refinement_kind

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: Ssa_api.values;
    refinements: refinement L.LMap.t;
    providers: Provider_api.info;
  }

  val program_with_scope : ?ignore_toplevel:bool -> (L.t, L.t) Flow_ast.Program.t -> env_info

  val program : (L.t, L.t) Flow_ast.Program.t -> refinement L.LMap.t

  val sources_of_use : env_info -> L.t -> L.LSet.t

  val source_bindings : env_info -> L.LSet.t L.LMap.t
end

module Make
    (L : Loc_sig.S)
    (Ssa_api : Ssa_api.S with module L = L)
    (Scope_api : Scope_api_sig.S with module L = Ssa_api.L) :
  S with module L = L and module Ssa_api = Ssa_api and module Scope_api = Scope_api = struct
  module L = L
  module Ssa_api = Ssa_api
  module Scope_api = Scope_api
  module Scope_builder = Scope_builder.Make (L) (Scope_api)
  module Ssa_builder = Ssa_builder.Make (L) (Ssa_api) (Scope_builder)
  module Invalidation_api = Invalidation_api.Make (L) (Scope_api) (Ssa_api)
  module Provider_api = Provider_api.Make (L)
  open Scope_builder

  type abrupt_kind = Ssa_builder.AbruptCompletion.t

  exception AbruptCompletionExn = Ssa_builder.AbruptCompletion.Exn

  type refinement_kind =
    | And of refinement_kind * refinement_kind
    | Or of refinement_kind * refinement_kind
    | Not of refinement_kind
    | Truthy of L.t
    | Null
    | Undefined
    | Maybe
    | InstanceOf of L.t
    | IsArray
    | BoolR of L.t
    | FunctionR
    | NumberR of L.t
    | ObjectR
    | StringR of L.t
    | SymbolR of L.t
    | SingletonBoolR of {
        loc: L.t;
        sense: bool;
        lit: bool;
      }
    | SingletonStrR of {
        loc: L.t;
        sense: bool;
        lit: string;
      }
    | SingletonNumR of {
        loc: L.t;
        sense: bool;
        lit: float * string;
      }
  [@@deriving show { with_path = false }]

  type refinement = L.LSet.t * refinement_kind

  type env_info = {
    scopes: Scope_api.info;
    ssa_values: Ssa_api.values;
    env_values: Ssa_api.values;
    refinements: refinement L.LMap.t;
    providers: Provider_api.info;
  }

  type refinement_chain =
    | BASE of refinement
    | SUCCESSIVE of refinement_chain * int
    | AND of int * int
    | OR of int * int
    | NOT of int

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
    type t

    val mk_unresolved : int -> t

    val empty : unit -> t

    val uninitialized : unit -> t

    val new_id : unit -> int

    val merge : t -> t -> t

    val one : L.t virtual_reason -> t

    val all : L.t virtual_reason list -> t

    val resolve : unresolved:t -> t -> unit

    val simplify : t -> Ssa_api.write_loc list

    val id_of_val : t -> int
  end = struct
    let curr_id = ref 0

    type ref_state =
      (* different unresolved vars are distinguished by their ids, which enables using structural
         equality for computing normal forms: see below *)
      | Unresolved of int
      | Resolved of write_state

    and write_state =
      | Uninitialized
      | Loc of L.t virtual_reason
      | PHI of write_state list
      | REF of ref_state ref

    and t = {
      id: int;
      write_state: write_state;
    }

    let new_id () =
      let id = !curr_id in
      curr_id := !curr_id + 1;
      id

    let mk_with_write_state write_state =
      let id = new_id () in
      { id; write_state }

    let mk_unresolved id = mk_with_write_state @@ REF (ref (Unresolved id))

    let empty () = mk_with_write_state @@ PHI []

    let uninitialized () = mk_with_write_state Uninitialized

    let join = function
      | [] -> PHI []
      | [t] -> t
      | ts -> PHI ts

    module WriteSet = Set.Make (struct
      type t = write_state

      let compare = Stdlib.compare
    end)

    let rec normalize (t : write_state) : WriteSet.t =
      match t with
      | Uninitialized
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

    let one reason = mk_with_write_state @@ Loc reason

    let all locs = mk_with_write_state @@ join (Base.List.map ~f:(fun reason -> Loc reason) locs)

    (* Resolving unresolved to t essentially models an equation of the form
       unresolved = t, where unresolved is a reference to an unknown and t is the
       known. Since the only non-trivial operation in t is joining, it is OK to
       erase any occurrences of unresolved in t: if t = unresolved | t' then
       unresolved = t is the same as unresolved = t'. *)
    let rec resolve ~unresolved t =
      match unresolved.write_state with
      | REF ({ contents = Unresolved _ } as r) -> r := Resolved (erase r t.write_state)
      | _ -> failwith "Only an unresolved REF can be resolved"

    and erase r t =
      match t with
      | Uninitialized -> t
      | Loc _ -> t
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
    let simplify t =
      let vals = normalize t.write_state in
      Base.List.map
        ~f:(function
          | Uninitialized -> Ssa_api.Uninitialized
          | Loc r -> Ssa_api.Write r
          | REF { contents = Unresolved _ } -> failwith "An unresolved REF cannot be simplified"
          | PHI _
          | REF { contents = Resolved _ } ->
            failwith "A normalized value cannot be a PHI or a resolved REF")
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
      (* always REF *)
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

  type ssa = {
    val_ref: Val.t ref;
    havoc: Havoc.t;
  }

  class env_builder (prepass_info, prepass_values) provider_info =
    object (this)
      inherit Scope_builder.scope_builder ~with_types:true as super

      (* We maintain a map of read locations to raw Val.t terms, which are
         simplified to lists of write locations once the analysis is done. *)
      val mutable values : Val.t L.LMap.t = L.LMap.empty

      method values : Ssa_api.values = L.LMap.map Val.simplify values

      val invalidation_caches = Invalidation_api.mk_caches ()

      val mutable globals_env = SMap.empty

      val mutable id = 0

      method mk_unresolved =
        id <- id + 1;
        Val.mk_unresolved id

      (* Maps SSA ids to refinement ids. This mapping contains _all_ the refinements reachable at
       * any point in the code. The latest_refinement maps keep track of which entries to read. *)
      val mutable refinement_heap = IMap.empty

      (* Stack mapping SSA ids to refinement ids. TODO: Use a ValMap.t list instead of an IMap.t list so that
       * ocaml warns us if we conflate ssa and refinement ids. 
       * We push new entries onto the stack when we enter a new refinement scope. This lets us
       * easily do things like negate refinements. It is critical that refinements in one layer of this stack
       * do not refer to refinements in other layers of this stack so that negations do not propagate to
       * refinements that appeared before the new scope. This invariant is maintained by add_refinement. *)
      val mutable latest_refinements = []

      val mutable refined_reads = L.LMap.empty

      method refined_reads : refinement L.LMap.t = refined_reads

      val mutable curr_id = 0

      method private new_id () =
        let new_id = curr_id in
        curr_id <- curr_id + 1;
        new_id

      (* Utils to manipulate single-static-assignment (SSA) environments.

         TODO: These low-level operations should probably be replaced by
         higher-level "control-flow-graph" operations that can be implemented using
         them, e.g., those that deal with branches and loops. *)
      val mutable ssa_env : ssa SMap.t = SMap.empty

      method ssa_env : Env.t = SMap.map (fun { val_ref; _ } -> !val_ref) ssa_env

      method merge_remote_ssa_env (env : Env.t) : unit =
        (* NOTE: env might have more keys than ssa_env, since the environment it
           describes might be nested inside the current environment *)
        SMap.iter (fun x { val_ref; _ } -> val_ref := Val.merge !val_ref (SMap.find x env)) ssa_env

      method merge_ssa_env (env1 : Env.t) (env2 : Env.t) : unit =
        let env1 = SMap.values env1 in
        let env2 = SMap.values env2 in
        let ssa_env = SMap.values ssa_env in
        list_iter3
          (fun { val_ref; _ } value1 value2 -> val_ref := Val.merge value1 value2)
          ssa_env
          env1
          env2

      method merge_self_ssa_env (env : Env.t) : unit =
        let env = SMap.values env in
        let ssa_env = SMap.values ssa_env in
        List.iter2 (fun { val_ref; _ } value -> val_ref := Val.merge !val_ref value) ssa_env env

      method reset_ssa_env (env0 : Env.t) : unit =
        let env0 = SMap.values env0 in
        let ssa_env = SMap.values ssa_env in
        List.iter2 (fun { val_ref; _ } value -> val_ref := value) ssa_env env0

      method fresh_ssa_env : Env.t = SMap.map (fun _ -> this#mk_unresolved) ssa_env

      method assert_ssa_env (env0 : Env.t) : unit =
        let env0 = SMap.values env0 in
        let ssa_env = SMap.values ssa_env in
        List.iter2 (fun { val_ref; _ } value -> Val.resolve ~unresolved:value !val_ref) ssa_env env0

      method empty_ssa_env : Env.t = SMap.map (fun _ -> Val.empty ()) ssa_env

      method havoc_current_ssa_env ~all =
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
          ssa_env;
        globals_env <- SMap.empty

      method havoc_uninitialized_ssa_env =
        SMap.iter
          (fun _x { val_ref; havoc } ->
            val_ref := Val.merge (Val.uninitialized ()) havoc.Havoc.unresolved)
          ssa_env;
        globals_env <- SMap.empty

      method private mk_ssa_env =
        SMap.map (fun _ ->
            {
              val_ref = ref (Val.uninitialized ());
              havoc = Havoc.{ unresolved = this#mk_unresolved; locs = [] };
            })

      method private push_ssa_env bindings =
        let old_ssa_env = ssa_env in
        let bindings = Bindings.to_map bindings in
        ssa_env <- SMap.fold SMap.add (this#mk_ssa_env bindings) old_ssa_env;
        (bindings, old_ssa_env)

      method private resolve_havocs =
        SMap.iter (fun x _loc ->
            let { havoc = { Havoc.unresolved; locs }; _ } = SMap.find x ssa_env in
            Val.resolve ~unresolved (Val.all locs))

      method private pop_ssa_env (bindings, old_ssa_env) =
        this#resolve_havocs bindings;
        ssa_env <- old_ssa_env

      method! with_bindings : 'a. ?lexical:bool -> L.t -> L.t Bindings.t -> ('a -> 'a) -> 'a -> 'a =
        fun ?lexical loc bindings visit node ->
          let saved_state = this#push_ssa_env bindings in
          this#run
            (fun () -> ignore @@ super#with_bindings ?lexical loc bindings visit node)
            ~finally:(fun () -> this#pop_ssa_env saved_state);
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
        with AbruptCompletion.Exn abrupt_completion -> Some abrupt_completion

      method from_completion =
        function
        | None -> ()
        | Some abrupt_completion -> raise (AbruptCompletion.Exn abrupt_completion)

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
      val mutable abrupt_completion_envs : AbruptCompletion.env list = []

      method raise_abrupt_completion : 'a. AbruptCompletion.t -> 'a =
        fun abrupt_completion ->
          let env = this#ssa_env in
          this#reset_ssa_env this#empty_ssa_env;
          abrupt_completion_envs <- (abrupt_completion, env) :: abrupt_completion_envs;
          raise (AbruptCompletion.Exn abrupt_completion)

      method expecting_abrupt_completions f =
        let saved = abrupt_completion_envs in
        abrupt_completion_envs <- [];
        this#run f ~finally:(fun () ->
            abrupt_completion_envs <- List.rev_append saved abrupt_completion_envs)

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
            abrupt_completion_envs
        in
        if matching <> [] then (
          List.iter (fun (_abrupt_completion, env) -> this#merge_remote_ssa_env env) matching;
          abrupt_completion_envs <- non_matching
        ) else
          match completion_state with
          | Some abrupt_completion when not (filter abrupt_completion) ->
            raise (AbruptCompletion.Exn abrupt_completion)
          | _ -> ()

      (* Track the list of labels that might describe a loop. Used to detect which
         labeled continues need to be handled by the loop.

         The idea is that a labeled statement adds its label to the list before
         entering its child, and if the child is not a loop or another labeled
         statement, the list will be cleared. A loop will consume the list, so we
         also clear the list on our way out of any labeled statement. *)
      val mutable possible_labeled_continues = []

      method! pattern_identifier ?kind ident =
        ignore kind;
        let (loc, { Flow_ast.Identifier.name = x; comments = _ }) = ident in
        let reason = Reason.(mk_reason (RIdentifier (OrdinaryName x))) loc in
        begin
          match SMap.find_opt x ssa_env with
          | Some { val_ref; havoc } ->
            val_ref := Val.one reason;
            Havoc.(
              havoc.locs <- Base.Option.value_exn (Provider_api.providers_of_def provider_info loc))
          | _ -> ()
        end;
        super#identifier ident

      (* This method is called during every read of an identifier. We need to ensure that
       * if the identifier is refined that we record the refiner as the write that reaches
       * this read *)
      method any_identifier loc name =
        (match SMap.find_opt name ssa_env with
        | Some { val_ref; _ } -> values <- L.LMap.add loc !val_ref values
        | None -> ());
        match this#find_refinement name with
        | None -> ()
        | Some refinement_chain ->
          let refinement = this#chain_to_refinement refinement_chain in
          refined_reads <- L.LMap.add loc refinement refined_reads

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

      (** We describe the effect on the environment of evaluating node n using Hoare
          triples of the form [PRE] n [POST], where PRE is the environment before
          and POST is the environment after the evaluation of node n. Environments
          must be joined whenever a node is reachable from multiple nodes, as can
          happen after a branch or before a loop. **)

      (******************************************)
      (* [PRE] if (e) { s1 } else { s2 } [POST] *)
      (******************************************)
      (*    |                                   *)
      (*    e                                   *)
      (*   / \                                  *)
      (* s1   s2                                *)
      (*   \./                                  *)
      (*    |                                   *)
      (******************************************)
      (* [PRE] e [ENV0]                         *)
      (* [ENV0] s1 [ENV1]                       *)
      (* [ENV0] s2 [ENV2]                       *)
      (* POST = ENV1 | ENV2                     *)
      (******************************************)
      method! if_statement _loc (stmt : (L.t, L.t) Ast.Statement.If.t) =
        let open Ast.Statement.If in
        let { test; consequent; alternate; _ } = stmt in
        ignore @@ this#expression test;
        let env0 = this#ssa_env in
        (* collect completions and environments of every branch *)
        let then_completion_state =
          this#run_to_completion (fun () ->
              ignore @@ this#if_consequent_statement ~has_else:(alternate <> None) consequent)
        in
        let env1 = this#ssa_env in
        this#reset_ssa_env env0;
        let else_completion_state =
          this#run_to_completion (fun () ->
              ignore
              @@ Flow_ast_mapper.map_opt
                   (fun (loc, { Alternate.body; comments }) ->
                     (loc, { Alternate.body = this#statement body; comments }))
                   alternate)
        in
        (* merge environments *)
        this#merge_self_ssa_env env1;

        (* merge completions *)
        let if_completion_states = (then_completion_state, [else_completion_state]) in
        this#merge_completion_states if_completion_states;
        stmt

      (********************************)
      (* [PRE] while (e) { s } [POST] *)
      (********************************)
      (*    |                         *)
      (*    e <-.                     *)
      (*   / \ /                      *)
      (*  |   s                       *)
      (*   \                          *)
      (*    |                         *)
      (********************************)
      (* PRE = ENV0                   *)
      (* [ENV0 | ENV1] e [ENV2]       *)
      (* [ENV2] s [ENV1]              *)
      (* POST = ENV2                  *)
      (********************************)
      method! while_ _loc (stmt : (L.t, L.t) Ast.Statement.While.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.While in
            let { test; body; comments = _ } = stmt in
            (* placeholder for environment at the end of the loop body *)
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            ignore @@ this#expression test;
            let env2 = this#ssa_env in
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue exits *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            (* end of loop body *)
            this#assert_ssa_env env1;

            (* out of the loop! this always happens right after evaluating the loop test *)
            this#reset_ssa_env env2;

            (* we might also never enter the loop body *)
            let while_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states while_completion_states)
            in
            (* completion_state = None *)
            (* break exits *)
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (***********************************)
      (* [PRE] do { s } while (e) [POST] *)
      (***********************************)
      (*    |                            *)
      (*    s <-.                        *)
      (*     \ /                         *)
      (*      e                          *)
      (*      |                          *)
      (***********************************)
      (* PRE = ENV0                      *)
      (* [ENV0 | ENV1] s; e [ENV1]       *)
      (* POST = ENV1                     *)
      (***********************************)
      method! do_while _loc (stmt : (L.t, L.t) Ast.Statement.DoWhile.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.DoWhile in
            let { body; test; _ } = stmt in
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            begin
              match loop_completion_state with
              | None -> ignore @@ this#expression test
              | _ -> ()
            end;
            this#assert_ssa_env env1;
            let do_while_completion_states = (loop_completion_state, []) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states do_while_completion_states)
            in
            (* completion_state = loop_completion_state *)
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (**************************************)
      (* [PRE] for (e; e1; e2) { s } [POST] *)
      (**************************************)
      (*    |                               *)
      (*    e                               *)
      (*    |                               *)
      (*   e1 <---.                         *)
      (*   / \    |                         *)
      (*  |   s   |                         *)
      (*  |    \ /                          *)
      (*  |    e2                           *)
      (*   \                                *)
      (*    |                               *)
      (**************************************)
      (* [PRE] e [ENV0]                     *)
      (* [ENV0 | ENV1] e1 [ENV2]            *)
      (* [ENV2] s; e2 [ENV1]                *)
      (* POST = ENV2                        *)
      (**************************************)
      method! scoped_for_statement _loc (stmt : (L.t, L.t) Ast.Statement.For.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.For in
            let { init; test; update; body; comments = _ } = stmt in
            ignore @@ Flow_ast_mapper.map_opt this#for_statement_init init;
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            ignore @@ Flow_ast_mapper.map_opt this#expression test;
            let env2 = this#ssa_env in
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            begin
              match loop_completion_state with
              | None -> ignore @@ Flow_ast_mapper.map_opt this#expression update
              | _ -> ()
            end;
            this#assert_ssa_env env1;
            this#reset_ssa_env env2;
            let for_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () -> this#merge_completion_states for_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (*************************************)
      (* [PRE] for (e1 in e2) { s } [POST] *)
      (*************************************)
      (*    |                              *)
      (*    e2                             *)
      (*    |                              *)
      (*    . <---.                        *)
      (*   / \    |                        *)
      (*  |   e1  |                        *)
      (*  |    \ /                         *)
      (*  |     s                          *)
      (*   \                               *)
      (*    |                              *)
      (*************************************)
      (* [PRE] e2 [ENV0]                   *)
      (* ENV2 = ENV0 | ENV1                *)
      (* [ENV2] e2 [ENV0]                  *)
      (* [ENV0 | ENV1] e1; s [ENV1]        *)
      (* POST = ENV2                       *)
      (*************************************)
      method! scoped_for_in_statement _loc (stmt : (L.t, L.t) Ast.Statement.ForIn.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.ForIn in
            let { left; right; body; each = _; comments = _ } = stmt in
            ignore @@ this#expression right;
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            let env2 = this#ssa_env in
            ignore @@ this#for_in_statement_lhs left;
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            this#assert_ssa_env env1;
            this#reset_ssa_env env2;
            let for_in_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states for_in_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        stmt

      (*************************************)
      (* [PRE] for (e1 of e2) { s } [POST] *)
      (*************************************)
      (*    |                              *)
      (*    e2                             *)
      (*    |                              *)
      (*    . <---.                        *)
      (*   / \    |                        *)
      (*  |   e1  |                        *)
      (*  |    \ /                         *)
      (*  |     s                          *)
      (*   \                               *)
      (*    |                              *)
      (*************************************)
      (* [PRE] e2 [ENV0]                   *)
      (* ENV2 = ENV0 | ENV1                *)
      (* [ENV2] e2 [ENV0]                  *)
      (* [ENV0 | ENV1] e1; s [ENV1]        *)
      (* POST = ENV2                       *)
      (*************************************)
      method! scoped_for_of_statement _loc (stmt : (L.t, L.t) Ast.Statement.ForOf.t) =
        this#expecting_abrupt_completions (fun () ->
            let continues = AbruptCompletion.continue None :: possible_labeled_continues in
            let open Ast.Statement.ForOf in
            let { left; right; body; await = _; comments = _ } = stmt in
            ignore @@ this#expression right;
            let env1 = this#fresh_ssa_env in
            this#merge_self_ssa_env env1;
            let env2 = this#ssa_env in
            ignore @@ this#for_of_statement_lhs left;
            let loop_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            (* continue *)
            let loop_completion_state =
              this#run_to_completion (fun () ->
                  this#commit_abrupt_completion_matching
                    (AbruptCompletion.mem continues)
                    loop_completion_state)
            in
            this#assert_ssa_env env1;
            this#reset_ssa_env env2;
            let for_of_completion_states = (None, [loop_completion_state]) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states for_of_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
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
      method! switch _loc (switch : (L.t, L.t) Ast.Statement.Switch.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Switch in
            let { discriminant; cases; comments = _ } = switch in
            ignore @@ this#expression discriminant;
            let (env, case_completion_states) =
              List.fold_left
                (fun acc stuff ->
                  let (_loc, case) = stuff in
                  this#ssa_switch_case acc case)
                (this#empty_ssa_env, [])
                cases
            in
            this#merge_self_ssa_env env;

            (* In general, cases are non-exhaustive. TODO: optimize with `default`. *)
            let switch_completion_states = (None, case_completion_states) in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states switch_completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state);
        switch

      method private ssa_switch_case
          (env, case_completion_states) (case : (L.t, L.t) Ast.Statement.Switch.Case.t') =
        let open Ast.Statement.Switch.Case in
        let { test; consequent; comments = _ } = case in
        ignore @@ Flow_ast_mapper.map_opt this#expression test;
        let env0 = this#ssa_env in
        this#merge_ssa_env env0 env;
        let case_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#statement_list consequent)
        in
        let env' = this#ssa_env in
        this#reset_ssa_env env0;
        (env', case_completion_state :: case_completion_states)

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
            let env1 = this#ssa_env in
            let (catch_completion_state_opt, env2) =
              match handler with
              | Some (loc, clause) ->
                (* NOTE: Havoc-ing the state when entering the handler is probably
                   overkill. We can be more precise but still correct by collecting all
                   possible writes in the try-block and merging them with the state when
                   entering the try-block. *)
                this#havoc_current_ssa_env ~all:false;
                let catch_completion_state =
                  this#run_to_completion (fun () -> ignore @@ this#catch_clause loc clause)
                in
                ([catch_completion_state], this#ssa_env)
              | None -> ([], this#empty_ssa_env)
            in
            this#merge_ssa_env env1 env2;
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
                this#havoc_current_ssa_env ~all:false;
                ignore @@ this#block loc block
              | None -> ()
            end;
            this#from_completion completion_state);
        stmt

      (* We also havoc state when entering functions and exiting calls. *)
      method! lambda loc params body =
        this#expecting_abrupt_completions (fun () ->
            let env = this#ssa_env in
            this#run
              (fun () ->
                this#havoc_uninitialized_ssa_env;
                let completion_state =
                  this#run_to_completion (fun () -> super#lambda loc params body)
                in
                this#commit_abrupt_completion_matching
                  AbruptCompletion.(mem [return; throw])
                  completion_state)
              ~finally:(fun () -> this#reset_ssa_env env))

      method! call loc (expr : (L.t, L.t) Ast.Expression.Call.t) =
        ignore @@ super#call loc expr;
        this#havoc_current_ssa_env ~all:false;
        expr

      method! new_ _loc (expr : (L.t, L.t) Ast.Expression.New.t) =
        let open Ast.Expression.New in
        let { callee; targs = _; arguments; comments = _ } = expr in
        ignore @@ this#expression callee;
        ignore @@ Flow_ast_mapper.map_opt this#call_arguments arguments;
        this#havoc_current_ssa_env ~all:false;
        expr

      method! unary_expression _loc (expr : (L.t, L.t) Ast.Expression.Unary.t) =
        Ast.Expression.Unary.(
          let { argument; operator; comments = _ } = expr in
          ignore @@ this#expression argument;
          begin
            match operator with
            | Await -> this#havoc_current_ssa_env ~all:false
            | _ -> ()
          end;
          expr)

      method! yield loc (expr : ('loc, 'loc) Ast.Expression.Yield.t) =
        ignore @@ super#yield loc expr;
        this#havoc_current_ssa_env ~all:true;
        expr

      (* Labeled statements handle labeled breaks, but also push labeled continues
         that are expected to be handled by immediately nested loops. *)
      method! labeled_statement _loc (stmt : (L.t, L.t) Ast.Statement.Labeled.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Labeled in
            let { label; body; comments = _ } = stmt in
            possible_labeled_continues <-
              AbruptCompletion.continue (Some label) :: possible_labeled_continues;
            let completion_state =
              this#run_to_completion (fun () -> ignore @@ this#statement body)
            in
            possible_labeled_continues <- [];
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
          | _ -> possible_labeled_continues <- []
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

      method private push_refinement_scope new_latest_refinements =
        latest_refinements <- new_latest_refinements :: latest_refinements

      method private pop_refinement_scope () = latest_refinements <- List.tl latest_refinements

      method private peek_new_refinements () = List.hd latest_refinements

      method private negate_new_refinements () =
        (* For each new ssa_id -> refinement_id we need to make a new_refinement_id and map 
         * ssa_id -> new_refinement_id  and new_refinement_id -> NOT refinement_id *)
        let head = List.hd latest_refinements in
        let new_latest_refinements =
          IMap.map
            (fun ref_id ->
              let new_id = this#new_id () in
              let new_ref = NOT ref_id in
              refinement_heap <- IMap.add new_id new_ref refinement_heap;
              new_id)
            head
        in
        latest_refinements <- new_latest_refinements :: List.tl latest_refinements

      method private merge_self_refinement_scope new_refinements =
        let head = List.hd latest_refinements in
        let head' =
          IMap.merge
            (fun _ latest1 latest2 ->
              match (latest1, latest2) with
              | (_, None) -> latest1
              | (_, Some _) -> latest2)
            head
            new_refinements
        in
        latest_refinements <- head' :: List.tl latest_refinements

      method private find_refinement_ids ssa_id =
        List.fold_left
          (fun acc refs ->
            match IMap.find_opt ssa_id refs with
            | None -> acc
            | Some id -> id :: acc)
          []
          latest_refinements

      method private find_refinement name =
        let writes =
          match SMap.find_opt name this#ssa_env with
          | Some v -> Some (Val.id_of_val v)
          | None -> SMap.find_opt name globals_env
        in
        match writes with
        | None -> None
        | Some ssa_id ->
          let refinement_ids = this#find_refinement_ids ssa_id in
          List.fold_left
            (fun acc id ->
              match (acc, IMap.find_opt id refinement_heap) with
              | (None, x) -> x
              | (x, None) -> x
              | (Some x, Some _) -> Some (SUCCESSIVE (x, id)))
            None
            (List.rev refinement_ids)

      method private add_refinement name refinement =
        let ssa_id =
          match SMap.find_opt name this#ssa_env with
          | Some writes_to_loc -> Val.id_of_val writes_to_loc
          | None -> this#add_global name
        in
        let new_id = this#new_id () in
        let head = List.hd latest_refinements in
        (* Note we are not using find_refinement_ids. We do not want this refinement to
         * refer to refinements in previous scopes. This is an important invariant to maintain:
         * refinements in one scope should not refer to refinements in another scope. If there
         * was a refinement in a previous scope, then when we _read_ the refinement we will combine
         * the refinement from the previous scope into the refinement in the current scope via SUCCESSIVE.
         * TODO: assert this invariant at runtime
         *)
        let old_id = IMap.find_opt ssa_id head in
        latest_refinements <- IMap.add ssa_id new_id head :: List.tl latest_refinements;
        let new_refinement =
          match old_id with
          | Some old_id -> SUCCESSIVE (BASE refinement, old_id)
          | _ -> BASE refinement
        in
        let new_expression_refinements = IMap.add new_id new_refinement refinement_heap in
        refinement_heap <- new_expression_refinements

      method add_global name =
        match SMap.find_opt name globals_env with
        | Some id -> id
        | None ->
          let id = Val.new_id () in
          globals_env <- SMap.add name id globals_env;
          id

      method identifier_refinement ((loc, ident) as identifier) =
        ignore @@ this#identifier identifier;
        let { Flow_ast.Identifier.name; _ } = ident in
        this#add_refinement name (L.LSet.singleton loc, Truthy loc)

      method assignment_refinement loc assignment =
        ignore @@ this#assignment loc assignment;
        let open Flow_ast.Expression.Assignment in
        match assignment.left with
        | ( id_loc,
            Flow_ast.Pattern.Identifier
              { Flow_ast.Pattern.Identifier.name = (_, { Flow_ast.Identifier.name; _ }); _ } ) ->
          this#add_refinement name (L.LSet.singleton loc, Truthy id_loc)
        | _ -> ()

      method private merge_refinement_scopes ~merge lhs_latest_refinements rhs_latest_refinements =
        let new_latest_refinements =
          IMap.merge
            (fun _ ref1 ref2 ->
              match (ref1, ref2) with
              | (None, None) -> None
              | (Some ref, None) -> Some ref
              | (None, Some ref) -> Some ref
              | (Some ref1, Some ref2) ->
                let new_ref = merge ref1 ref2 in
                let new_id = this#new_id () in
                refinement_heap <- IMap.add new_id new_ref refinement_heap;
                Some new_id)
            lhs_latest_refinements
            rhs_latest_refinements
        in
        this#merge_self_refinement_scope new_latest_refinements

      method logical_refinement expr =
        let { Flow_ast.Expression.Logical.operator; left = (loc, _) as left; right; comments = _ } =
          expr
        in
        this#push_refinement_scope IMap.empty;
        let (lhs_latest_refinements, rhs_latest_refinements, env1) =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.And ->
            ignore @@ this#expression_refinement left;
            let lhs_latest_refinements = this#peek_new_refinements () in
            (match operator with
            | Flow_ast.Expression.Logical.Or -> this#negate_new_refinements ()
            | _ -> ());
            let env1 = this#ssa_env in
            this#push_refinement_scope IMap.empty;
            ignore @@ this#expression_refinement right;
            let rhs_latest_refinements = this#peek_new_refinements () in
            (* Pop LHS refinement scope *)
            this#pop_refinement_scope ();
            (* Pop RHS refinement scope *)
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1)
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            (* If this overall expression is truthy, then either the LHS or the RHS has to be truthy.
               If it's because the LHS is truthy, then the LHS also has to be non-maybe (this is of course
               true by definition, but it's also true because of the nature of ??).
               But if we're evaluating the RHS, the LHS doesn't have to be truthy, it just has to be
               non-maybe. As a result, we do this weird dance of refinements so that when we traverse the
               RHS we have done the null-test but the overall result of this expression includes both the
               truthy and non-maybe qualities. *)
            ignore (this#null_test ~strict:false ~sense:false loc left);
            let env1 = this#ssa_env in
            let nullish = this#peek_new_refinements () in
            this#negate_new_refinements ();
            this#push_refinement_scope IMap.empty;
            ignore (this#expression_refinement right);
            let rhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#pop_refinement_scope ();
            this#push_refinement_scope nullish;
            (match key left with
            | None -> ()
            | Some name -> this#add_refinement name (L.LSet.singleton loc, Truthy loc));
            let lhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1)
        in
        let merge =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            merge_or
          | Flow_ast.Expression.Logical.And -> merge_and
        in
        this#merge_refinement_scopes merge lhs_latest_refinements rhs_latest_refinements;
        this#merge_self_ssa_env env1

      method null_test ~strict ~sense loc expr =
        ignore @@ this#expression expr;
        match key expr with
        | None -> ()
        | Some name ->
          let refinement =
            if strict then
              Null
            else
              Maybe
          in
          let refinement =
            if sense then
              refinement
            else
              Not refinement
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)

      method void_test ~sense ~strict ~check_for_bound_undefined loc expr =
        ignore @@ this#expression expr;
        match key expr with
        | None -> ()
        | Some name ->
          (* Only add the refinement if undefined is not re-bound *)
          if (not check_for_bound_undefined) || SMap.find_opt "undefined" this#ssa_env = None then
            let refinement =
              if strict then
                Undefined
              else
                Maybe
            in
            let refinement =
              if sense then
                refinement
              else
                Not refinement
            in
            this#add_refinement name (L.LSet.singleton loc, refinement)

      method typeof_test loc arg typename sense =
        ignore @@ this#expression arg;
        let refinement =
          match typename with
          | "boolean" -> Some (BoolR loc)
          | "function" -> Some FunctionR
          | "number" -> Some (NumberR loc)
          | "object" -> Some ObjectR
          | "string" -> Some (StringR loc)
          | "symbol" -> Some (SymbolR loc)
          | "undefined" -> Some Undefined
          | _ -> None
        in
        match (refinement, key arg) with
        | (Some ref, Some name) ->
          let refinement =
            if sense then
              ref
            else
              Not ref
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)
        | _ -> ()

      method literal_test ~strict ~sense loc expr refinement =
        ignore @@ this#expression expr;
        match key expr with
        | Some name when strict ->
          let refinement =
            if sense then
              refinement
            else
              Not refinement
          in
          this#add_refinement name (L.LSet.singleton loc, refinement)
        | _ -> ()

      method eq_test ~strict ~sense loc left right =
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
        | ( ( ( _,
                Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ })
              ) as undefined ),
            expr )
        | ( expr,
            ( ( _,
                Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ })
              ) as undefined ) ) ->
          ignore @@ this#expression undefined;
          this#void_test ~sense ~strict ~check_for_bound_undefined:true loc expr
        (* expr op void(...) *)
        | ((_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ }), expr)
        | (expr, (_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ })) ->
          this#void_test ~sense ~strict ~check_for_bound_undefined:false loc expr
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
          this#add_refinement name (L.LSet.singleton loc, InstanceOf inst_loc)

      method binary_refinement loc expr =
        let open Flow_ast.Expression.Binary in
        let { operator; left; right; comments = _ } = expr in
        match operator with
        (* == and != refine if lhs or rhs is an ident and other side is null *)
        | Equal -> this#eq_test ~strict:false ~sense:true loc left right
        | NotEqual -> this#eq_test ~strict:false ~sense:false loc left right
        | StrictEqual -> this#eq_test ~strict:true ~sense:true loc left right
        | StrictNotEqual -> this#eq_test ~strict:true ~sense:false loc left right
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
          | Some name -> this#add_refinement name (L.LSet.singleton loc, IsArray))
        | _ -> ignore @@ this#call loc call

      method unary_refinement
          loc ({ Flow_ast.Expression.Unary.operator; argument; comments = _ } as unary) =
        match operator with
        | Flow_ast.Expression.Unary.Not ->
          this#push_refinement_scope IMap.empty;
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
        | Member _
        | New _
        | Object _
        | OptionalCall _
        | OptionalMember _
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
        this#push_refinement_scope IMap.empty;
        (match operator with
        | Flow_ast.Expression.Logical.Or
        | Flow_ast.Expression.Logical.And ->
          ignore (this#expression_refinement left)
        | Flow_ast.Expression.Logical.NullishCoalesce ->
          ignore (this#null_test ~strict:false ~sense:false loc left));
        let env1 = this#ssa_env in
        (match operator with
        | Flow_ast.Expression.Logical.NullishCoalesce
        | Flow_ast.Expression.Logical.Or ->
          this#negate_new_refinements ()
        | Flow_ast.Expression.Logical.And -> ());
        ignore @@ this#expression right;
        this#pop_refinement_scope ();
        this#merge_self_ssa_env env1;
        expr

      method private chain_to_refinement =
        function
        | BASE refinement -> refinement
        | SUCCESSIVE (refinement_chain, id) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement refinement_chain in
          (L.LSet.union locs1 locs2, And (ref1, ref2))
        | AND (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id1 refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (IMap.find id2 refinement_heap) in
          (L.LSet.union locs1 locs2, And (ref1, ref2))
        | OR (id1, id2) ->
          let (locs1, ref1) = this#chain_to_refinement (IMap.find id1 refinement_heap) in
          let (locs2, ref2) = this#chain_to_refinement (IMap.find id2 refinement_heap) in
          (L.LSet.union locs1 locs2, Or (ref1, ref2))
        | NOT id ->
          let (locs, ref) = this#chain_to_refinement (IMap.find id refinement_heap) in
          (locs, Not ref)
    end

  let program_with_scope ?(ignore_toplevel = false) program =
    let open Hoister in
    let (loc, _) = program in
    let ((scopes, ssa_values) as prepass) =
      Ssa_builder.program_with_scope ~ignore_toplevel program
    in
    let providers = Provider_api.find_providers program in
    let ssa_walk = new env_builder prepass providers in
    let bindings =
      if ignore_toplevel then
        Bindings.empty
      else
        let hoist = new hoister ~with_types:true in
        hoist#eval hoist#program program
    in
    ignore @@ ssa_walk#with_bindings loc bindings ssa_walk#program program;
    {
      scopes;
      ssa_values;
      env_values = ssa_walk#values;
      refinements = ssa_walk#refined_reads;
      providers;
    }

  let program program =
    let { refinements; _ } = program_with_scope ~ignore_toplevel:false program in
    refinements

  let sources_of_use { env_values = vals; refinements = refis; _ } loc =
    let write_locs =
      L.LMap.find_opt loc vals
      |> Base.Option.value_map
           ~f:
             (Fn.compose
                L.LSet.of_list
                (Base.List.filter_map ~f:(function
                    | Ssa_api.Uninitialized -> None
                    | Ssa_api.Write r -> Some (Reason.poly_loc_of_reason r))))
           ~default:L.LSet.empty
    in
    let refi_locs =
      L.LMap.find_opt loc refis |> Base.Option.value_map ~f:fst ~default:L.LSet.empty
    in
    L.LSet.union refi_locs write_locs

  let source_bindings ({ env_values = vals; refinements = refis; _ } as info) =
    let keys = L.LSet.of_list (L.LMap.keys vals @ L.LMap.keys refis) in
    L.LSet.fold (fun k acc -> L.LMap.add k (sources_of_use info k) acc) keys L.LMap.empty
end

module With_Loc = Make (Loc_sig.LocS) (Ssa_api.With_Loc) (Scope_api.With_Loc)
module With_ALoc = Make (Loc_sig.ALocS) (Ssa_api.With_ALoc) (Scope_api.With_ALoc)
include With_ALoc
