(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is responsible for building a mapping from variable reads to the
 * writes and refinements that reach those reads. It is based on the implementation of the
 * ssa_builder, but with enough divergent behavior that the ssa_builder and name_resolver don't
 * actually share much code. If you're here to add support for a new syntax feature, you'll likely
 * need to modify the ssa_builder as well, but not necessarily with identical changes.*)

let statement_error = ()

open Reason
open Hoister

let is_call_to_invariant callee =
  match callee with
  | (_, Flow_ast.Expression.Identifier (_, { Flow_ast.Identifier.name = "invariant"; _ })) -> true
  | _ -> false

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

  val enable_enums : t -> bool

  val jsx : t -> Options.jsx_mode

  val react_runtime : t -> Options.react_runtime

  val enable_const_params : t -> bool

  val env_mode : t -> Options.env_mode

  val add_new_env_literal_subtypes : t -> ALoc.t * Env_api.new_env_literal_check -> unit

  val add_new_env_matching_props : t -> string * ALoc.t * ALoc.t -> unit
end

module type F = sig
  type cx

  val add_output : cx -> ?trace:Type.trace -> ALoc.t Error_message.t' -> unit
end

module type S = sig
  module Env_api : Env_api.S with module L = Loc_sig.ALocS

  type cx

  type abrupt_kind

  exception AbruptCompletionExn of abrupt_kind

  val program_with_scope :
    cx -> (ALoc.t, ALoc.t) Flow_ast.Program.t -> abrupt_kind option * Env_api.env_info

  val program :
    cx -> (ALoc.t, ALoc.t) Flow_ast.Program.t -> Env_api.values * (int -> Env_api.refinement)
end

module PostInferenceCheck = Env_api

module Make
    (Scope_api : Scope_api_sig.S with module L = Loc_sig.ALocS)
    (Ssa_api : Ssa_api.S with module L = Loc_sig.ALocS)
    (Env_api : Env_api.S
                 with module L = Loc_sig.ALocS
                  and module Scope_api = Scope_api
                  and module Ssa_api = Ssa_api)
    (Context : C)
    (FlowAPIUtils : F with type cx = Context.t) :
  S with module Env_api = Env_api and type cx = Context.t = struct
  let _f = FlowAPIUtils.add_output
  (* To make ocaml not complain, will be removed when FlowAPIUtils module is used *)

  module Scope_builder :
    Scope_builder_sig.S with module L = Loc_sig.ALocS and module Api = Scope_api =
    Scope_builder.Make (Loc_sig.ALocS) (Scope_api)

  module Provider_api :
    Provider_api.S with type info = Env_api.Provider_api.info and module L = Loc_sig.ALocS =
    Env_api.Provider_api

  module Ssa_builder = Ssa_builder.Make (Loc_sig.ALocS) (Ssa_api) (Scope_builder)
  module Invalidation_api =
    Invalidation_api.Make (Loc_sig.ALocS) (Scope_api) (Ssa_api) (Provider_api)
  module Env_api = Env_api
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

    val empty : unit -> t

    val uninitialized : ALoc.t -> t

    val undefined : L.t virtual_reason -> t

    val number : L.t virtual_reason -> t

    val undeclared_class : L.t virtual_reason -> string -> t

    val merge : t -> t -> t

    val this : t

    val super : t

    val arguments : t

    val global : string -> t

    val one : ALoc.t virtual_reason -> t

    val all : ALoc.t virtual_reason list -> t

    val of_write : write_state -> t

    val simplify : ALoc.t option -> Bindings.kind option -> string option -> t -> Env_api.read

    val id_of_val : t -> int

    val base_id_of_val : t -> int

    val refinement : int -> t -> t

    val projection : ALoc.t -> t

    val undeclared : string -> ALoc.t -> t

    val declared_but_skipped : string -> ALoc.t -> t

    val declared_function : ALoc.t -> t

    (* unwraps a RefinementWrite into just the underlying write *)
    val unrefine : int -> t -> t

    val unrefine_deeply : int -> t -> t

    (* Replace the base write of the refinement with the new base.
       If the write is not a refinement, replace the entire write with the base.

       This is useful for attaching a refinement that is known to be associated with a write, but
       is not attached due to syntactic difference.
       e.g. refinements on obj.x should be attached to x in const {x} = obj *)
    val replace_refinement_base_write : base:t -> t -> t

    val normalize_through_refinements : write_state -> WriteSet.t

    val writes_of_uninitialized : (int -> bool) -> t -> write_state list

    val is_global_undefined : t -> bool

    val is_global : t -> bool

    val is_undeclared : t -> bool

    val is_undeclared_or_skipped : t -> bool

    val is_declared_function : t -> bool
  end = struct
    let curr_id = ref 0

    type write_state =
      | Uninitialized of ALoc.t
      | Undeclared of string * ALoc.t
      | DeclaredButSkipped of string * ALoc.t
      | UndeclaredClass of {
          def: ALoc.t virtual_reason;
          name: string;
        }
      | Projection of ALoc.t
      | This
      | Super
      | Arguments
      | Global of string
      | Loc of ALoc.t virtual_reason
      | PHI of write_state list
      | Refinement of {
          refinement_id: int;
          val_t: t;
        }
      | Undefined of ALoc.t virtual_reason
      | Number of ALoc.t virtual_reason
      | DeclaredFunction of ALoc.t

    and t = {
      id: int;
      write_state: write_state;
    }

    let is_global_undefined t =
      match t.write_state with
      | Global "undefined" -> true
      | _ -> false

    let is_global t =
      match t.write_state with
      | Global _ -> true
      | _ -> false

    let is_undeclared t =
      match t.write_state with
      | Undeclared _ -> true
      | UndeclaredClass _ -> true
      | _ -> false

    let is_undeclared_or_skipped t =
      match t.write_state with
      | Undeclared _ -> true
      | DeclaredButSkipped _ -> true
      | UndeclaredClass _ -> true
      | _ -> false

    let is_declared_function t =
      match t.write_state with
      | DeclaredFunction _ -> true
      | _ -> false

    let new_id () =
      let id = !curr_id in
      curr_id := !curr_id + 1;
      id

    let mk_with_write_state write_state =
      let id = new_id () in
      { id; write_state }

    let of_write = mk_with_write_state

    let empty () = mk_with_write_state @@ PHI []

    let uninitialized r = mk_with_write_state (Uninitialized r)

    let undefined r = mk_with_write_state (Undefined r)

    let number r = mk_with_write_state (Number r)

    let undeclared_class def name = mk_with_write_state (UndeclaredClass { def; name })

    let projection loc = mk_with_write_state @@ Projection loc

    let declared_function loc = mk_with_write_state @@ DeclaredFunction loc

    let refinement refinement_id val_t = mk_with_write_state @@ Refinement { refinement_id; val_t }

    let undeclared name def_loc = mk_with_write_state @@ Undeclared (name, def_loc)

    let declared_but_skipped name def_loc = mk_with_write_state @@ DeclaredButSkipped (name, def_loc)

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

    let rec base_id_of_val { id; write_state } =
      match write_state with
      | Refinement { refinement_id = _; val_t } -> base_id_of_val val_t
      | _ -> id

    let unrefine_deeply id t = mk_with_write_state @@ unrefine_deeply_write_state id t.write_state

    let unrefine id t =
      match t.write_state with
      | Refinement { refinement_id; val_t } when refinement_id = id -> val_t
      | _ -> t

    let replace_refinement_base_write ~base t =
      match t.write_state with
      | Refinement { refinement_id; val_t = _ } -> refinement refinement_id base
      | _ -> base

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
      | Uninitialized _
      | Undefined _
      | Number _
      | DeclaredFunction _
      | Undeclared _
      | DeclaredButSkipped _
      | UndeclaredClass _
      | Projection _
      | This
      | Super
      | Arguments
      | Global _
      | Loc _
      | Refinement _ ->
        WriteSet.singleton t
      | PHI ts ->
        List.fold_left
          (fun vals' t ->
            let vals = normalize t in
            WriteSet.union vals' vals)
          WriteSet.empty
          ts

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
      | Uninitialized _
      | Undefined _
      | Number _
      | DeclaredFunction _
      | Undeclared _
      | DeclaredButSkipped _
      | UndeclaredClass _
      | Projection _
      | Global _
      | This
      | Super
      | Arguments
      | Loc _ ->
        WriteSet.singleton t
      | PHI ts ->
        List.fold_left
          (fun vals' t ->
            let vals = normalize t in
            WriteSet.union vals' vals)
          WriteSet.empty
          ts
      | Refinement { val_t; _ } -> normalize_through_refinements val_t.write_state

    let this = mk_with_write_state This

    let super = mk_with_write_state Super

    let arguments = mk_with_write_state Arguments

    let global name = mk_with_write_state @@ Global name

    let one reason = mk_with_write_state @@ Loc reason

    let all locs = mk_with_write_state @@ join (Base.List.map ~f:(fun reason -> Loc reason) locs)

    let rec simplify_val t =
      let vals = normalize t.write_state in
      Base.List.map
        ~f:(function
          | Uninitialized l when WriteSet.cardinal vals <= 1 ->
            Env_api.Uninitialized (mk_reason RUninitialized l)
          | Undefined r -> Env_api.Undefined r
          | Number r -> Env_api.Number r
          | DeclaredFunction l -> Env_api.DeclaredFunction l
          | Undeclared (name, loc)
          | DeclaredButSkipped (name, loc) ->
            Env_api.Undeclared (name, loc)
          | UndeclaredClass { def; name } -> Env_api.UndeclaredClass { def; name }
          | Uninitialized l -> Env_api.Uninitialized (mk_reason RPossiblyUninitialized l)
          | Projection loc -> Env_api.Projection loc
          | Loc r -> Env_api.Write r
          | Refinement { refinement_id; val_t } ->
            Env_api.Refinement
              {
                writes = simplify_val val_t;
                refinement_id;
                (* We delegate to the old env for this and super,
                   so we shouldn't cache results about them. *)
                write_id =
                  (match val_t with
                  | { id = _; write_state = This | Super } -> None
                  | { id; write_state = _ } -> Some id);
              }
          | This -> Env_api.This
          | Super -> Env_api.Super
          | Arguments -> Env_api.Arguments
          | Global name -> Env_api.Global name
          | PHI _ -> failwith "A normalized value cannot be a PHI")
        (WriteSet.elements vals)

    (* Simplification converts a Val.t to a list of locations. *)
    let simplify def_loc binding_kind_opt name value =
      let write_locs = simplify_val value in
      let val_kind =
        match binding_kind_opt with
        | Some (Bindings.Type { imported }) -> Some (Env_api.Type { imported })
        | Some _ -> Some Env_api.Value
        | None -> None
      in
      let id =
        match value with
        | { id = _; write_state = This | Super } -> None
        | { id; write_state = _ } -> Some id
      in
      { Env_api.def_loc; write_locs; val_kind; name; id }

    let id_of_val { id; write_state = _ } = id

    let writes_of_uninitialized refine_to_undefined { write_state; _ } =
      let rec state_is_uninitialized v =
        match v with
        | Undeclared _ -> []
        | DeclaredButSkipped _ -> []
        | Undefined _ -> []
        | Number _ -> []
        | DeclaredFunction _ -> []
        | Uninitialized _ -> [v]
        | UndeclaredClass _ -> [v]
        | PHI states -> Base.List.concat_map ~f:state_is_uninitialized states
        | Refinement { refinement_id; val_t = { write_state; _ } } ->
          let states = state_is_uninitialized write_state in
          if List.length states = 0 || (not @@ refine_to_undefined refinement_id) then
            []
          else
            states
        | Loc _ -> []
        | This -> []
        | Super -> []
        | Arguments -> []
        | Global _ -> []
        | Projection _ -> []
      in
      state_is_uninitialized write_state
  end

  module RefinementKey = Refinement_key.Make (L)

  module HeapRefinementMap = WrappedMap.Make (struct
    type t = RefinementKey.proj list

    let compare = Stdlib.compare
  end)

  module LookupMap = WrappedMap.Make (struct
    type t = RefinementKey.lookup

    let compare = Stdlib.compare
  end)

  type heap_refinement_map = Val.t HeapRefinementMap.t

  (* An environment is a map from variables to values. *)
  module Env = struct
    type entry = {
      env_val: Val.t;
      heap_refinements: heap_refinement_map;
      def_loc: ALoc.t option;
    }

    type t = entry SMap.t
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
    havoc: Val.t;
    def_loc: ALoc.t option;
    heap_refinements: heap_refinement_map ref;
    kind: Bindings.kind;
  }

  type read_entry = {
    def_loc: ALoc.t option;
    binding_kind_opt: Bindings.kind option;
    value: Val.t;
    name: string option;
  }

  type refinement_id = int

  (* Describes a set of vals that arise from refinements and which should be removed from
     the environment after the refinement scope, e.g. `if (x.y) {}` *)
  type changeset = Val.t LookupMap.t

  type refinement_prop =
    | Refinements of (RefinementKey.lookup * refinement_id) IMap.t * changeset
    | Not of refinement_prop
    | And of refinement_prop * refinement_prop
    | Or of refinement_prop * refinement_prop

  (* The `applied` and `changeset` elements of this record describe the refinements and
     values that have presently been applied to the environment, when this is on the
     latest_refinement stack (see below). The `total` describes the same changeset and
     refinements, but in a propositional form that can be negated. We must maintain the invariant
     that normalizing the `total` (using the normalize_total_refinements method below) produces the
     applied and the changeset. *)
  type refinement_maps = {
    applied: (RefinementKey.lookup * refinement_id) IMap.t;
    changeset: changeset;
    total: refinement_prop option;
  }

  type name_resolver_state = {
    (* We maintain a map of read locations to raw Val.t and their def locs terms, which are
       simplified to lists of write locations once the analysis is done. *)
    values: read_entry L.LMap.t;
    (* We also maintain a list of all write locations, for use in populating the env with
       types. *)
    write_entries: Env_api.env_entry Loc_sig.ALocS.LMap.t;
    curr_id: int;
    (* Maps refinement ids to refinements. This mapping contains _all_ the refinements reachable at
     * any point in the code. The latest_refinement maps keep track of which entries to read. *)
    refinement_heap: refinement_chain IMap.t;
    latest_refinements: refinement_maps list;
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
    visiting_hoisted_type: bool;
    jsx_base_name: string option;
  }

  let error_for_assignment_kind
      cx name assignment_loc def_loc_opt stored_binding_kind pattern_binding_kind v =
    match def_loc_opt with
    (* Identifiers with no binding can never reintroduce "cannot reassign binding" errors *)
    | None -> None
    | Some def_loc ->
      (* Pattern kind is None or Some (Var | Const | Let). It is set to None when we hit a regular
       * assignment, like x = 4, but set to Some x when we are in a declaration, like let x = 3, or
       * class A {}. We use the pattern_binding_kind to decide if we should emit an error saying
       * "you cannot re-declare X" vs. "you cannot reassign const X" *)
      (match (stored_binding_kind, pattern_binding_kind) with
      | (Bindings.Const, None) ->
        Some
          Error_message.(
            EBindingError (EConstReassigned, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Parameter, None) when Context.enable_const_params cx ->
        Some
          Error_message.(
            EBindingError (EConstParamReassigned, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Class, None) ->
        let def_reason = mk_reason (RIdentifier (OrdinaryName name)) def_loc in
        Some
          Error_message.(
            EAssignConstLikeBinding
              {
                loc = assignment_loc;
                definition = def_reason;
                binding_kind = Scope.Entry.ClassNameBinding;
              }
          )
      | (Bindings.Function, None) ->
        let def_reason = mk_reason (RIdentifier (OrdinaryName name)) def_loc in
        Some
          Error_message.(
            EAssignConstLikeBinding
              {
                loc = assignment_loc;
                definition = def_reason;
                binding_kind = Scope.Entry.FunctionBinding;
              }
          )
      | (Bindings.DeclaredFunction _, None) ->
        let def_reason = mk_reason (RIdentifier (OrdinaryName name)) def_loc in
        Some
          Error_message.(
            EAssignConstLikeBinding
              {
                loc = assignment_loc;
                definition = def_reason;
                (* The error message is unaffected by the predicate flag *)
                binding_kind = Scope.Entry.(DeclaredFunctionBinding { predicate = false });
              }
          )
      | (Bindings.Var, Some Flow_ast.Statement.VariableDeclaration.(Let | Const)) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Const, Some _)
      | (Bindings.Let, Some _)
      | (Bindings.Class, Some _)
      | (Bindings.Function, Some _)
      | (Bindings.Type _, Some _)
        when not (Val.is_undeclared v) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Enum, None) ->
        Some
          Error_message.(
            EBindingError (EEnumReassigned, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Type { imported }, None) ->
        Some
          Error_message.(
            EBindingError
              (ETypeInValuePosition { imported; name }, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Parameter, Some _) when Context.enable_const_params cx && not (Val.is_undeclared v)
        ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | (Bindings.Parameter, Some Flow_ast.Statement.VariableDeclaration.(Let | Const))
        when not (Val.is_undeclared v) ->
        Some
          Error_message.(
            EBindingError (ENameAlreadyBound, assignment_loc, OrdinaryName name, def_loc)
          )
      | _ -> None)

  let initialize_globals unbound_names =
    SMap.empty
    |> SSet.fold
         (fun name acc ->
           let entry =
             {
               val_ref = ref (Val.global name);
               havoc = Val.global name;
               def_loc = None;
               heap_refinements = ref HeapRefinementMap.empty;
               kind = Bindings.Var;
             }
           in
           SMap.add name entry acc)
         unbound_names
    (* this has to come later, since this can be thought to be unbound names in SSA builder when it's used as a type. *)
    |> SMap.add
         "this"
         {
           val_ref = ref Val.this;
           havoc = Val.this;
           def_loc = None;
           heap_refinements = ref HeapRefinementMap.empty;
           kind = Bindings.Var;
         }
    |> SMap.add
         "super"
         {
           val_ref = ref Val.super;
           havoc = Val.super;
           def_loc = None;
           heap_refinements = ref HeapRefinementMap.empty;
           kind = Bindings.Var;
         }
    |> SMap.add
         "arguments"
         {
           val_ref = ref Val.arguments;
           havoc = Val.arguments;
           def_loc = None;
           heap_refinements = ref HeapRefinementMap.empty;
           kind = Bindings.Var;
         }

  (* Statement.ml tries to extract the name and traverse at the location of the
   * jsx element if it's an identifier, otherwise it just traverses the
   * jsx_pragma expression *)
  let extract_jsx_basename =
    let open Flow_ast.Expression in
    function
    | (_, Identifier (_, { Flow_ast.Identifier.name; _ })) -> Some name
    | _ -> None

  let initial_env cx unbound_names =
    let globals = initialize_globals unbound_names in
    (* We need to make sure that the base name for jsx is always in scope.
     * statement.ml is going to read these identifiers at jsx calls, even if
     * they haven't been declared locally. *)
    let jsx_base_name =
      match Context.jsx cx with
      | Options.Jsx_react -> Some "React"
      | Options.Jsx_pragma (_, ast) -> extract_jsx_basename ast
    in
    match jsx_base_name with
    | None -> (globals, None)
    | Some jsx_base_name ->
      (* We use a global here so that if the base name is never created locally
       * we first check the globals before emitting an error *)
      let entry =
        {
          val_ref = ref (Val.global jsx_base_name);
          havoc = Val.global jsx_base_name;
          def_loc = None;
          heap_refinements = ref HeapRefinementMap.empty;
          kind = Bindings.Var;
        }
      in
      (SMap.add jsx_base_name entry globals, Some jsx_base_name)

  let conj_total t1 t2 =
    match (t1, t2) with
    | (Some t1, Some t2) -> Some (And (t1, t2))
    | (Some t, _)
    | (_, Some t) ->
      Some t
    | (None, None) -> None

  let empty_refinements = { applied = IMap.empty; changeset = LookupMap.empty; total = None }

  class name_resolver cx (prepass_info, prepass_values, unbound_names) provider_info =
    let add_output =
      match Context.env_mode cx with
      | Options.SSAEnv _ -> FlowAPIUtils.add_output cx
      | _ -> (fun ?trace:_ _ -> ())
    in

    let add_literal_subtype_test =
      let rec f refinee_loc literal =
        match literal with
        | SingletonNumR { loc; lit = (num, raw); sense } ->
          Context.add_new_env_literal_subtypes
            cx
            (refinee_loc, PostInferenceCheck.SingletonNum (loc, sense, num, raw))
        | SingletonBoolR { loc; lit; sense } ->
          Context.add_new_env_literal_subtypes
            cx
            (refinee_loc, PostInferenceCheck.SingletonBool (loc, lit = sense))
        | SingletonStrR { loc; lit; sense } ->
          Context.add_new_env_literal_subtypes
            cx
            (refinee_loc, PostInferenceCheck.SingletonStr (loc, sense, lit))
        | NotR r -> f refinee_loc r
        | AndR (r1, r2)
        | OrR (r1, r2) ->
          f refinee_loc r1;
          f refinee_loc r2
        | _ -> ()
      in
      match Context.env_mode cx with
      | Options.SSAEnv _ -> f
      | _ -> (fun _ _ -> ())
    in

    let enable_enums = Context.enable_enums cx in
    object (this)
      inherit
        Scope_builder.scope_builder ~flowmin_compatibility:false ~enable_enums ~with_types:true as super

      val invalidation_caches = Invalidation_api.mk_caches ()

      val mutable env_state : name_resolver_state =
        let (env, jsx_base_name) = initial_env cx unbound_names in
        {
          values = L.LMap.empty;
          write_entries = L.LMap.empty;
          curr_id = 0;
          refinement_heap = IMap.empty;
          latest_refinements = [];
          env;
          abrupt_completion_envs = [];
          possible_labeled_continues = [];
          visiting_hoisted_type = false;
          jsx_base_name;
        }

      method values : Env_api.values =
        L.LMap.map
          (fun { def_loc; value; binding_kind_opt; name } ->
            Val.simplify def_loc binding_kind_opt name value)
          env_state.values

      method write_entries : Env_api.env_entry L.LMap.t = env_state.write_entries

      method private new_id () =
        let new_id = env_state.curr_id in
        let curr_id = new_id + 1 in
        env_state <- { env_state with curr_id };
        new_id

      method env : Env.t =
        SMap.map
          (fun { val_ref; heap_refinements; def_loc; _ } ->
            { Env.env_val = !val_ref; heap_refinements = !heap_refinements; def_loc })
          env_state.env

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
        let refinements_by_key { applied; _ } =
          IMap.fold
            (fun _ (lookup_key, refinement_id) ->
              LookupMap.update lookup_key (function
                  | Some map -> Some (ISet.add refinement_id map)
                  | None -> Some (ISet.singleton refinement_id)
                  ))
            applied
            LookupMap.empty
        in
        let unrefine refinements_by_key lookup_key v =
          LookupMap.find_opt lookup_key refinements_by_key
          |> Base.Option.value_map
               ~f:(fun refinement_ids -> ISet.fold Val.unrefine refinement_ids v)
               ~default:v
        in
        SMap.mapi
          (fun name { val_ref; heap_refinements; def_loc; _ } ->
            let head = List.hd env_state.latest_refinements in
            let refinements_by_key = refinements_by_key head in
            let lookup_key = RefinementKey.lookup_of_name name in
            let env_val = unrefine refinements_by_key lookup_key !val_ref in
            let unrefined_heap_refinements =
              HeapRefinementMap.mapi
                (fun projections v ->
                  let lookup_key = RefinementKey.lookup_of_name_with_projections name projections in
                  unrefine refinements_by_key lookup_key v)
                !heap_refinements
            in
            { Env.env_val; heap_refinements = unrefined_heap_refinements; def_loc })
          env_state.env

      method merge_heap_refinements =
        (* When we merge the heap refinements from two branches we cannot include
         * keys that did not appear on both sides. Take this example:
         * let obj = {};
         * if (true) {
         *   obj.foo = 3;
         * } else {
         *   obj.bar = 4;
         * }
         * (obj.foo: 3); // Should fail because the else branch does not add this refinement
         *)
        HeapRefinementMap.merge (fun _ refinement1 refinement2 ->
            match (refinement1, refinement2) with
            | (Some v1, Some v2) -> Some (Val.merge v1 v2)
            | _ -> None
        )

      (*
       * See merge_heap_refinements for an explanation of our general strategy.
       *
       * The exception to that rule is when we're merging heap refinements after a loop.
       *
       * See the comment at env_loop for an explanation of how we havoc changed
       * values in a loop before reading on.
       *
       * If we refine over a heap value x.foo in the loop guard then we have 2
       * possible scenarios that affect the post-loop refinement:
       * 1. x.foo changes in the loop
       * 2. x.foo does not change in the loop
       *
       * If x.foo does not change in the loop then there will be an
       * entry for x.foo in both the end-of-loop environment and the never-entered-loop
       * environment. We merge those two states and then apply the negated loop guard
       * in this case.
       *
       * while (x.foo === 3) {
       * }
       * x.foo; // x.foo did not change throughout the lifetime of the loop,
       *        // so we can safely merge the pre-state and post-state and add the negated
       *        // refinement. Both the pre- and post-states will have an entry for
       *        // x.foo because both traverse the refinement
       *
       * If x.foo _does_ change then we're in a more interesting situation. In this
       * case, it is possible for the end-of-loop environment to not contain a heap
       * entry for x.foo, so merging it with the never-entered-loop environment would
       * not add an entry for the heap refinement, and applying the negated refinement
       * would have no effect. To fix this, we take advantage of the fact that if
       * x.foo is changed then we havoc before analyzing the loop guard. That means
       * that the access in the loop guard is a fine location to use for the projection
       * at the base of the refinement. If we did not havoc, then we could end
       * up in a situation where x.foo was already refined at the loop guard and we
       * unsoundly carry that refinement into the post-loop state even though that refinement
       * was invalidated by the loop body.
       *
       * while (x.foo === 3) {
       *  f();
       * } // The post state of the loop has no entry for x.foo because
       *   // the call of f havocs the heap refinement.
       * x.foo; // Since x.foo (and x) is havoced before looking at the guard,
       *        // the x.foo projection in the guard is a "general" type for
       *        // the x.foo projection, so we can use that location to grab
       *        // the type we need to refine with the negation of the loop guard.
       *)
      method merge_loop_guard_env_after_loop env_after_guard_no_refinements =
        let env_after_loop = env_state.env in
        let merge_heap_refinements ~heap_entries_after_loop ~heap_entries_after_guard =
          HeapRefinementMap.merge
            (fun _ refinement1 refinement2 ->
              match (refinement1, refinement2) with
              | (Some v1, Some v2) -> Some (Val.merge v1 v2)
              | (Some v, None) -> Some v (* Keep the projection from the guard! *)
              | _ -> None)
            heap_entries_after_guard
            heap_entries_after_loop
        in
        List.iter2
          (fun {
                 Env.env_val = after_guard;
                 heap_refinements = heap_entries_after_guard;
                 def_loc = _;
               }
               { val_ref = after_loop; heap_refinements = heap_entries_after_loop; _ } ->
            after_loop := Val.merge after_guard !after_loop;
            heap_entries_after_loop :=
              merge_heap_refinements
                ~heap_entries_after_loop:!heap_entries_after_loop
                ~heap_entries_after_guard)
          (SMap.values env_after_guard_no_refinements)
          (SMap.values env_after_loop)

      method merge_remote_env (env : Env.t) : unit =
        (* NOTE: env might have more keys than env_state.env, since the environment it
           describes might be nested inside the current environment *)
        SMap.iter
          (fun x { val_ref; heap_refinements = heap_refinements1; def_loc = def_loc_1; _ } ->
            let { Env.env_val; heap_refinements = heap_refinements2; def_loc = def_loc_2 } =
              SMap.find x env
            in
            if def_loc_1 = def_loc_2 then (
              val_ref := Val.merge !val_ref env_val;
              heap_refinements1 := this#merge_heap_refinements !heap_refinements1 heap_refinements2
            ))
          env_state.env

      method merge_env (env1 : Env.t) (env2 : Env.t) : unit =
        let env1 = SMap.values env1 in
        let env2 = SMap.values env2 in
        let env = SMap.values env_state.env in
        list_iter3
          (fun { val_ref; heap_refinements; _ }
               { Env.env_val = value1; heap_refinements = heap_refinements1; def_loc = _ }
               { Env.env_val = value2; heap_refinements = heap_refinements2; def_loc = _ } ->
            val_ref := Val.merge value1 value2;
            heap_refinements := this#merge_heap_refinements heap_refinements1 heap_refinements2)
          env
          env1
          env2

      method merge_self_env (other_env : Env.t) : unit =
        let other_env = SMap.values other_env in
        let env = SMap.values env_state.env in
        List.iter2
          (fun { val_ref; heap_refinements; _ }
               { Env.env_val = value; heap_refinements = new_heap_refinements; def_loc = _ } ->
            val_ref := Val.merge !val_ref value;
            heap_refinements := this#merge_heap_refinements !heap_refinements new_heap_refinements)
          env
          other_env

      method reset_env (env0 : Env.t) : unit =
        let env0 = SMap.values env0 in
        let env = SMap.values env_state.env in
        List.iter2
          (fun { val_ref; heap_refinements; _ }
               { Env.env_val; heap_refinements = old_heap_refinements; def_loc = _ } ->
            val_ref := env_val;
            heap_refinements := old_heap_refinements)
          env
          env0

      method empty_env : Env.t =
        SMap.map
          (fun _ ->
            {
              Env.env_val = Val.empty ();
              heap_refinements = HeapRefinementMap.empty;
              (* The empty env is always used as a reset value,
                 and the reset_env method (see above) does not mutate def_loc at all.
                 Therefore, the value of def_loc here does not matter. *)
              def_loc = None;
            })
          env_state.env

      (* This method applies a function over the value stored with a refinement key. It is
       * mostly just a convenient helper so that the process of deconstructing the
       * key and finding the appropriate Val.t does not have to be repeated in
       * every method that needs to update an entry. The create_val_for_heap argument can
       * be used to specify what value to apply the function to if the heap entry
       * does not yet exist.

       * In addition to updating the Val.t, if the callback Val.t updating function
       * is called (i.e. if we find a value to update), we'll return any additional
       * data that the function itself returns (as the first element of its return
       * type tuple). In addition, if we end up creating a new val.t as part of a heap
       * refinement, we'll return that val.t as well. In cases where this information
       * isn't necessary, callers can call `map_val_with_lookup` below instead.
       *)
      method map_val_with_lookup_result
          : 'r.
            RefinementKey.lookup ->
            ?create_val_for_heap:Val.t Lazy.t ->
            (Val.t -> 'r * Val.t) ->
            ('r * Val.t option) option =
        fun lookup ?create_val_for_heap f ->
          let { RefinementKey.base; projections } = lookup in
          match SMap.find_opt base env_state.env with
          | None -> None
          | Some { val_ref; heap_refinements; havoc = _; def_loc = _; kind = _ } ->
            (match projections with
            | [] ->
              let (res, val_) = f !val_ref in
              val_ref := val_;
              Some (res, None)
            | _ ->
              let (res, new_heap_refinements) =
                match
                  (HeapRefinementMap.find_opt projections !heap_refinements, create_val_for_heap)
                with
                | (Some heap_val, _) ->
                  let (res, val_) = f heap_val in
                  (Some (res, None), HeapRefinementMap.add projections val_ !heap_refinements)
                | (None, Some (lazy default)) ->
                  let (res, val_) = f default in
                  ( Some (res, Some default),
                    HeapRefinementMap.add projections val_ !heap_refinements
                  )
                | (None, None) -> (None, !heap_refinements)
              in
              heap_refinements := new_heap_refinements;
              res)

      method map_val_with_lookup lookup ?create_val_for_heap f =
        let (_ : (unit * Val.t option) option) =
          this#map_val_with_lookup_result lookup ?create_val_for_heap (fun v -> ((), f v))
        in
        ()

      method get_val_of_expression expr =
        match RefinementKey.of_expression expr with
        | None -> None
        | Some { RefinementKey.loc = _; lookup = { RefinementKey.base; projections } } ->
          (match SMap.find_opt base this#env with
          | None -> None
          | Some { Env.env_val; heap_refinements; def_loc = _ } ->
            (match projections with
            | [] -> Some env_val
            | _ -> HeapRefinementMap.find_opt projections heap_refinements))

      (* Function calls may introduce refinements if the function called is a
       * predicate function. The EnvBuilder has no idea if a function is a
       * predicate function or not. To handle that, we encode that a variable
       * _might_ be havoced by a function call if that variable is passed
       * as an argument. Variables not passed into the function are havoced if
       * the invalidation api says they can be invalidated.
       *)
      method apply_latent_refinements ((callee_loc, _) as callee) refinement_keys_by_arg =
        List.iteri
          (fun index -> function
            | None -> ()
            | Some key ->
              this#add_single_refinement
                key
                (L.LSet.singleton callee_loc, LatentR { func = callee; index = index + 1 }))
          refinement_keys_by_arg

      method havoc_heap_refinements heap_refinements = heap_refinements := HeapRefinementMap.empty

      method havoc_all_heap_refinements () =
        SMap.iter
          (fun _ { heap_refinements; _ } -> this#havoc_heap_refinements heap_refinements)
          env_state.env

      method havoc_env ~force_initialization ~all =
        SMap.iter
          (fun _x { val_ref; havoc; def_loc; heap_refinements; kind = _ } ->
            this#havoc_heap_refinements heap_refinements;
            let uninitialized_writes =
              lazy (Val.writes_of_uninitialized this#refinement_may_be_undefined !val_ref)
            in
            let val_is_undeclared_or_skipped = Val.is_undeclared_or_skipped !val_ref in
            let havoc_ref =
              if force_initialization then
                havoc
              else if val_is_undeclared_or_skipped then
                !val_ref
              else
                Base.List.fold
                  ~init:havoc
                  ~f:(fun acc write -> Val.merge acc (Val.of_write write))
                  (Lazy.force uninitialized_writes)
            in
            if
              Base.Option.is_none def_loc
              || Invalidation_api.should_invalidate
                   ~all
                   invalidation_caches
                   prepass_info
                   prepass_values
                   (Base.Option.value_exn def_loc (* checked against none above *))
              || force_initialization
                 && (List.length (Lazy.force uninitialized_writes) > 0
                    || val_is_undeclared_or_skipped
                    )
            then
              val_ref := havoc_ref)
          env_state.env

      method havoc_current_env ~all = this#havoc_env ~all ~force_initialization:false

      method havoc_uninitialized_env = this#havoc_env ~force_initialization:true ~all:true

      method refinement_may_be_undefined id =
        let rec refine_undefined = function
          | UndefinedR
          | MaybeR ->
            true
          | NotR r -> not @@ refine_undefined r
          | OrR (r1, r2) -> refine_undefined r1 || refine_undefined r2
          | AndR (r1, r2) -> refine_undefined r1 && refine_undefined r2
          | _ -> false
        in
        let (_, id) = this#refinement_of_id id in
        refine_undefined id

      method private providers_of_def_loc def_loc =
        let providers =
          Base.Option.value_map
            ~default:[]
            ~f:snd
            (Provider_api.providers_of_def provider_info def_loc)
        in
        ( ( if Base.List.is_empty providers then
            Val.uninitialized def_loc
          else
            Val.all providers
          ),
          providers
        )

      method private mk_env =
        SMap.mapi (fun name (kind, (loc, _)) ->
            match kind with
            | Bindings.Type _ ->
              let reason = mk_reason (RType (OrdinaryName name)) loc in
              let write_entries =
                L.LMap.add loc (Env_api.AssigningWrite reason) env_state.write_entries
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.one reason);
                havoc = Val.one reason;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.DeclaredClass ->
              let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
              let write_entries =
                L.LMap.add loc (Env_api.AssigningWrite reason) env_state.write_entries
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.one reason);
                havoc = Val.one reason;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.Class ->
              let (havoc, providers) = this#providers_of_def_loc loc in
              let write_entries =
                Base.List.fold
                  ~f:(fun acc r -> L.LMap.add (poly_loc_of_reason r) (Env_api.AssigningWrite r) acc)
                  ~init:env_state.write_entries
                  providers
              in
              let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref (Val.undeclared_class reason name);
                havoc;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.DeclaredFunction _ ->
              let (_, providers) = this#providers_of_def_loc loc in
              let write_entries =
                Base.List.fold
                  ~f:(fun acc r -> L.LMap.add (poly_loc_of_reason r) (Env_api.AssigningWrite r) acc)
                  ~init:env_state.write_entries
                  providers
              in
              env_state <- { env_state with write_entries };
              let declared_function = Val.declared_function loc in
              {
                val_ref = ref declared_function;
                havoc = declared_function;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | Bindings.Import ->
              let reason = mk_reason (RIdentifier (OrdinaryName name)) loc in
              let havoc = Val.one reason in
              {
                val_ref = ref havoc;
                havoc;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
            | _ ->
              let initial_val =
                match kind with
                (* let/const/enum all introduce errors if you try to access or assign them
                 * before syntactically encountering the declaration. All other bindings
                 * do not, so we don't set them to be undeclared *)
                | Bindings.Let
                | Bindings.Const
                | Bindings.Enum
                | Bindings.Parameter
                | Bindings.Function ->
                  Val.undeclared name loc
                | _ -> Val.uninitialized loc
              in
              let (havoc, providers) = this#providers_of_def_loc loc in
              let write_entries =
                Base.List.fold
                  ~f:(fun acc r -> L.LMap.add (poly_loc_of_reason r) (Env_api.AssigningWrite r) acc)
                  ~init:env_state.write_entries
                  providers
              in
              env_state <- { env_state with write_entries };
              {
                val_ref = ref initial_val;
                havoc;
                def_loc = Some loc;
                heap_refinements = ref HeapRefinementMap.empty;
                kind;
              }
        )

      method private push_env bindings =
        let old_env = env_state.env in
        let bindings = Bindings.to_map bindings in
        let env = SMap.fold SMap.add (this#mk_env bindings) old_env in
        env_state <- { env_state with env };
        (bindings, old_env)

      method private pop_env (_, old_env) = env_state <- { env_state with env = old_env }

      method! with_bindings
          : 'a. ?lexical:bool -> ALoc.t -> ALoc.t Bindings.t -> ('a -> 'a) -> 'a -> 'a =
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
              }
        )

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

      method! binding_type_identifier ident =
        let (loc, { Flow_ast.Identifier.name; comments = _ }) = ident in
        let { kind; def_loc; _ } = SMap.find name env_state.env in
        (match def_loc with
        (* Identifiers with no binding can never reintroduce "cannot reassign binding" errors *)
        | None -> ()
        | Some def_loc ->
          (match kind with
          | Bindings.Type _ when not (ALoc.equal loc def_loc) ->
            (* Types are already bind in hoister,
               so we only check for rebind in different locations. *)
            add_output
              Error_message.(EBindingError (ENameAlreadyBound, loc, OrdinaryName name, def_loc))
          | Bindings.Type _ -> ()
          | Bindings.Var
          | Bindings.Const
          | Bindings.Let
          | Bindings.Class
          | Bindings.Function
          | Bindings.Parameter ->
            add_output
              Error_message.(EBindingError (ENameAlreadyBound, loc, OrdinaryName name, def_loc))
          | _ -> ()));
        super#identifier ident

      method! function_identifier ident =
        (* The parent flow_ast_mapper treats functions as Vars, but in Flow
         * (not JS, Flow) they behave more like hoisted lets. For the purpose of
         * reassignment errors, we should consider them to be lets *)
        this#pattern_identifier ~kind:Flow_ast.Statement.VariableDeclaration.Let ident

      (* We want to translate object pattern destructing {a:{b:{c}}} = o into o.a.b.c,
         so the use of refinement can be recorded as a write.
         We use acc to keep track of the current parent expr *)
      method private binding_pattern_track_object_destructuring ?kind ~acc expr =
        let open Ast.Pattern in
        let (_, patt) = expr in
        (match patt with
        | Object { Object.properties; annot; comments = _ } ->
          Base.List.iter properties ~f:(fun prop ->
              let open Ast.Pattern.Object in
              match prop with
              | RestElement prop -> ignore @@ this#pattern_object_rest_property ?kind prop
              | Property ((_, { Property.key; pattern; default; shorthand = _ }) as prop) ->
                (match key with
                | Property.Identifier (loc, { Ast.Identifier.name = x; comments = _ })
                | Property.Literal (loc, { Ast.Literal.value = Ast.Literal.String x; _ }) ->
                  Base.Option.iter default ~f:(fun default -> ignore @@ this#expression default);
                  let acc =
                    let open Ast.Expression in
                    let property =
                      Member.PropertyIdentifier (loc, { Ast.Identifier.name = x; comments = None })
                    in
                    (loc, Member { Member._object = acc; property; comments = None })
                  in
                  (match pattern with
                  | ( _,
                      Identifier
                        {
                          Identifier.name = (loc, { Ast.Identifier.name = x; comments = _ });
                          annot;
                          optional = _;
                        }
                    ) ->
                    ignore @@ this#expression acc;
                    (* Leaf of the object pattern *)
                    (match this#get_val_of_expression acc with
                    | None -> ignore @@ this#pattern ?kind pattern
                    | Some refined_v ->
                      ignore @@ this#type_annotation_hint annot;
                      this#bind_pattern_identifier_customized
                        ?kind
                        ~get_assigned_val:(fun base ->
                          Val.replace_refinement_base_write ~base refined_v)
                        loc
                        x)
                  | _ ->
                    ignore @@ this#binding_pattern_track_object_destructuring ?kind ~acc pattern)
                | _ -> ignore @@ this#pattern_object_property ?kind prop)
          );
          ignore @@ this#type_annotation_hint annot
        | Array _
        | Identifier _
        | Expression _ ->
          ignore @@ this#pattern ?kind expr);
        expr

      method! pattern_identifier ?kind ident =
        let (loc, { Flow_ast.Identifier.name = x; comments = _ }) = ident in
        this#bind_pattern_identifier_customized ?kind loc x;
        super#identifier ident

      method private bind_pattern_identifier_customized ?kind ?(get_assigned_val = Base.Fn.id) loc x
          =
        let reason = Reason.(mk_reason (RIdentifier (OrdinaryName x))) loc in
        let { val_ref; heap_refinements; kind = stored_binding_kind; def_loc; havoc = _ } =
          SMap.find x env_state.env
        in
        match kind with
        (* Assignments to undeclared bindings that aren't part of declarations do not
         * initialize those bindings. *)
        | None when Val.is_undeclared_or_skipped !val_ref ->
          (match def_loc with
          | None -> failwith "Cannot have an undeclared or skipped binding without a def loc"
          | Some def_loc ->
            add_output
              Error_message.(
                EBindingError (EReferencedBeforeDeclaration, loc, OrdinaryName x, def_loc)
              ))
        | _ ->
          (match error_for_assignment_kind cx x loc def_loc stored_binding_kind kind !val_ref with
          | Some err ->
            add_output err;
            let write_entries = L.LMap.add loc Env_api.NonAssigningWrite env_state.write_entries in
            env_state <- { env_state with write_entries }
          | _ ->
            this#havoc_heap_refinements heap_refinements;
            let write_entries =
              if not (Val.is_declared_function !val_ref) then (
                let write_entry =
                  if Val.is_global !val_ref then
                    Env_api.GlobalWrite reason
                  else
                    Env_api.AssigningWrite reason
                in
                val_ref := get_assigned_val (Val.one reason);
                L.LMap.add loc write_entry env_state.write_entries
              ) else
                (* All of the providers are aleady in the map. We don't want to overwrite them with
                 * a non-assigning write. We _do_ want to enter regular function declarations as
                 * non-assigning writes so that they are not checked against the providers in
                * New_env.set_env_entry *)
                L.LMap.update
                  loc
                  (fun x ->
                    match x with
                    | None -> Some Env_api.NonAssigningWrite
                    | _ -> x)
                  env_state.write_entries
            in
            env_state <- { env_state with write_entries })

      (* This method is called during every read of an identifier. We need to ensure that
       * if the identifier is refined that we record the refiner as the write that reaches
       * this read
       *
       * Note that we don't emit EBinding errors for referenced-before-declaration errors here.
       * That is because we may read an UndeclaredClass from a type position and the
       * name_resolver doesn't keep track of whether we are in a type context or not.
       *
       * Instead of augmenting the name_resolver with those capabilities, we emit these errors
       * in the new_env, which does know if it's querying a value or a type.
       * *)
      method any_identifier loc name =
        let { val_ref; havoc; def_loc; kind; _ } = SMap.find name env_state.env in
        let v =
          if env_state.visiting_hoisted_type then
            havoc
          else
            !val_ref
        in
        let values =
          L.LMap.add
            loc
            { def_loc; value = v; binding_kind_opt = Some kind; name = Some name }
            env_state.values
        in
        env_state <- { env_state with values }

      method! this_expression loc this_ =
        this#any_identifier loc "this";
        this_

      method! super_expression loc super_ =
        this#any_identifier loc "super";
        super_

      method! identifier (ident : (ALoc.t, ALoc.t) Ast.Identifier.t) =
        let (loc, { Ast.Identifier.name = x; comments = _ }) = ident in
        this#any_identifier loc x;
        super#identifier ident

      method! generic_identifier_type (git : ('loc, 'loc) Ast.Type.Generic.Identifier.t) =
        let open Ast.Type.Generic.Identifier in
        let rec loop git =
          match git with
          | Unqualified i -> ignore @@ this#type_identifier i
          | Qualified (_, { qualification; _ }) -> loop qualification
        in
        loop git;
        git

      method! jsx_element_name_identifier (ident : (ALoc.t, ALoc.t) Ast.JSX.Identifier.t) =
        let (loc, { Ast.JSX.Identifier.name; comments = _ }) = ident in
        this#any_identifier loc name;
        super#jsx_identifier ident

      method! jsx_element_name_namespaced ns =
        (* TODO: what identifiers does `<foo:bar />` read? *)
        super#jsx_element_name_namespaced ns

      method havoc_heap_refinements_using_name ~private_ name =
        SMap.iter
          (fun _ { heap_refinements; _ } ->
            heap_refinements :=
              HeapRefinementMap.filter
                (fun projections _ ->
                  not (RefinementKey.proj_uses_propname ~private_ name projections))
                !heap_refinements)
          env_state.env

      (* This function should be called _after_ a member expression is assigned a value.
       * It havocs other heap refinements depending on the name of the member and then adds
       * a write to the heap refinement entry for that member expression *)
      method assign_expression ~update_entry lhs rhs =
        ignore @@ this#pattern_expression lhs;
        ignore @@ this#expression rhs;
        match lhs with
        | (loc, Flow_ast.Expression.Member member) ->
          let reason = mk_reason RSomeProperty loc in
          let assigned_val = Val.one reason in
          this#assign_member ~update_entry member loc assigned_val reason
        | _ -> ()

      method assign_member ~update_entry lhs_member lhs_loc assigned_val val_reason =
        this#post_assignment_heap_refinement_havoc lhs_member;
        (* We pass allow_optional:false, but optional chains can't be in the LHS anyway. *)
        let lookup = RefinementKey.lookup_of_member lhs_member ~allow_optional:false in
        let open Flow_ast.Expression in
        match (lhs_member, lookup) with
        | ( { Member.property = Member.PropertyIdentifier _ | Member.PropertyPrivateName _; _ },
            Some lookup
          )
          when update_entry ->
          this#map_val_with_lookup
            lookup
            (fun _ -> assigned_val)
            ~create_val_for_heap:(lazy assigned_val);
          let write_entries =
            L.LMap.add lhs_loc (Env_api.AssigningWrite val_reason) env_state.write_entries
          in
          env_state <- { env_state with write_entries }
        | _ -> ()

      (* This method is called after assigning a member expression but _before_ the refinement for
       * that assignment is recorded. *)
      method post_assignment_heap_refinement_havoc
          (lhs : (ALoc.t, ALoc.t) Flow_ast.Expression.Member.t) =
        let open Flow_ast.Expression in
        match lhs with
        | {
         Member._object;
         property = Member.PropertyPrivateName (_, { Flow_ast.PrivateName.name; _ });
         _;
        } ->
          (* Yes, we want to havoc using the PROPERTY name here. This is because we
           * do not do any alias tracking, so we want to have the following behavior:
           * let x = {};
           * let y = x;
           * x.foo = 3;
           * y.foo = 4;
           * (x.foo: 3) // MUST error!
           *)
          this#havoc_heap_refinements_using_name name ~private_:true
        | {
         Member._object;
         property = Member.PropertyIdentifier (_, { Flow_ast.Identifier.name; _ });
         _;
        } ->
          (* As in the previous case, we can't know if this object is aliased nor what property
           * is being written. We are forced to conservatively havoc ALL heap refinements in this
           * situation. *)
          this#havoc_heap_refinements_using_name name ~private_:false
        | { Member._object; property = Member.PropertyExpression _; _ } ->
          this#havoc_all_heap_refinements ()

      (* Order of evaluation matters *)
      method! assignment _loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Assignment.t) =
        let open Ast.Expression.Assignment in
        let { operator; left = (left_loc, _) as left; right; comments = _ } = expr in
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
              | (_, Expression e) ->
                (* given `o.x = e`, read o then read e *)
                this#assign_expression ~update_entry:true e right
            end
          | Some
              ( PlusAssign | MinusAssign | MultAssign | ExpAssign | DivAssign | ModAssign
              | LShiftAssign | RShiftAssign | RShift3Assign | BitOrAssign | BitXorAssign
              | BitAndAssign ) ->
            let open Ast.Pattern in
            begin
              match left with
              | (_, Identifier { Identifier.name; _ }) ->
                (* given `x += e`, read x then read e then write x *)
                ignore @@ this#identifier name;
                ignore @@ this#expression right;
                ignore @@ this#assignment_pattern left
              | (_, Expression e) ->
                (* given `o.x += e`, read o then read e *)
                this#assign_expression ~update_entry:true e right
              | (_, (Object _ | Array _)) -> statement_error
            end
          | Some ((OrAssign | AndAssign | NullishAssign) as operator) ->
            let left_expr =
              match left with
              | (lhs_loc, Ast.Pattern.Identifier { Ast.Pattern.Identifier.name; _ }) ->
                Some (lhs_loc, Ast.Expression.Identifier name)
              | (lhs_loc, Ast.Pattern.Expression (_, Ast.Expression.Member mem)) ->
                Some (lhs_loc, Ast.Expression.Member mem)
              | _ -> None
            in
            this#push_refinement_scope empty_refinements;
            (match left_expr with
            | None -> statement_error
            | Some left_expr ->
              begin
                match RefinementKey.of_expression left_expr with
                | None -> ()
                | Some refinement_key ->
                  (* If we don't already have a projection val in the environment for this key, we need to create one and commit it. We can't create it
                     via a refinement, because then it would be popped off as part of a changeset at the end of the refinement scope. *)
                  this#add_projection refinement_key
              end;
              (* THe LHS is unconditionally evaluated, so we don't run-to-completion and catch the
               * error here *)
              (match operator with
              | OrAssign
              | AndAssign ->
                ignore (this#expression_refinement left_expr)
              | NullishAssign ->
                ignore (this#expression left_expr);
                this#add_refinement_to_expr left_expr (L.LSet.singleton left_loc, NotR MaybeR)
              | _ -> ()));
            let env1 = this#env_without_latest_refinements in
            let env1_with_refinements = this#env in
            (match operator with
            | NullishAssign
            | OrAssign ->
              this#negate_new_refinements ()
            | _ -> ());
            (* The RHS is _only_ evaluated if the LHS fails its check. That means that patterns like
               * x || invariant(false) should propagate the truthy refinement to the next line. We keep track
               * of the completion state on the rhs to do that. If the LHS throws then the entire expression
               * throws, so there's no need to catch the exception from the LHS *)
            let rhs_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#expression right)
            in
            (match rhs_completion_state with
            | Some AbruptCompletion.Throw ->
              this#reset_env env1_with_refinements;
              this#pop_refinement_scope_without_unrefining ()
            | _ ->
              this#pop_refinement_scope ();
              this#merge_self_env env1);
            ignore @@ this#assignment_pattern left
        end;
        expr

      (* Order of evaluation matters *)
      method! variable_declarator
          ~kind (decl : (ALoc.t, ALoc.t) Ast.Statement.VariableDeclaration.Declarator.t) =
        let open Ast.Statement.VariableDeclaration.Declarator in
        let (_loc, { id; init }) = decl in
        let open Ast.Pattern in
        begin
          match id with
          | ( _,
              ( Identifier { Ast.Pattern.Identifier.annot; _ }
              | Object { Ast.Pattern.Object.annot; _ }
              | Array { Ast.Pattern.Array.annot; _ } )
            ) ->
            begin
              match init with
              | Some init ->
                (* given `var x = e`, read e then write x *)
                ignore @@ this#expression init;
                ignore @@ this#binding_pattern_track_object_destructuring ~kind ~acc:init id
              | None ->
                (* No rhs means no write occurs, but the variable moves from undeclared to
                 * uninitialized. *)
                Flow_ast_utils.fold_bindings_of_pattern
                  (fun () (loc, { Flow_ast.Identifier.name; _ }) ->
                    let { val_ref; kind = stored_binding_kind; def_loc; _ } =
                      SMap.find name env_state.env
                    in
                    let () =
                      error_for_assignment_kind
                        cx
                        name
                        loc
                        def_loc
                        stored_binding_kind
                        (Some kind)
                        !val_ref
                      |> Base.Option.iter ~f:add_output
                    in
                    if Val.is_undeclared !val_ref then val_ref := Val.uninitialized loc)
                  ()
                  id;
                ignore @@ this#type_annotation_hint annot
            end
          | (_, Expression _) -> statement_error
        end;
        decl

      (* read and write (when the argument is an identifier) *)
      method! update_expression _loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Update.t) =
        let open Ast.Expression.Update in
        let { argument; operator = _; prefix = _; comments = _ } = expr in
        begin
          match argument with
          | (_, Ast.Expression.Identifier x) ->
            (* given `x++`, read x then write x *)
            ignore @@ this#identifier x;
            ignore @@ this#pattern_identifier x
          | (loc, Ast.Expression.Member member) ->
            (* given `o.x++`, read o.x then write o.x *)
            ignore @@ this#expression argument;
            ignore @@ this#pattern_expression argument;
            let val_reason = mk_reason RSomeProperty loc in
            let assigned_val = Val.number val_reason in
            (* We explicitly write a number instead of using the location of the write to avoid
             * location clashes between the read of the member and the write of the member. If
             * we don't do this then expressions like o.x++ will attempt to unify the read of o.x
             * with the write of o.x that happens in that update expression *)
            (match RefinementKey.lookup_of_member member ~allow_optional:false with
            | Some lookup ->
              this#map_val_with_lookup
                lookup
                (fun _ -> assigned_val)
                ~create_val_for_heap:(lazy assigned_val)
            | _ -> ())
          | _ -> (* given 'o()++`, read o *) ignore @@ this#expression argument
        end;
        expr

      (* things that cause abrupt completions *)
      method! break _loc (stmt : ALoc.t Ast.Statement.Break.t) =
        let open Ast.Statement.Break in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.break label)

      method! continue _loc (stmt : ALoc.t Ast.Statement.Continue.t) =
        let open Ast.Statement.Continue in
        let { label; comments = _ } = stmt in
        this#raise_abrupt_completion (AbruptCompletion.continue label)

      method! return _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Return.t) =
        let open Ast.Statement.Return in
        let { argument; comments = _ } = stmt in
        ignore @@ Flow_ast_mapper.map_opt this#expression argument;
        this#raise_abrupt_completion AbruptCompletion.return

      method! throw _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Throw.t) =
        let open Ast.Statement.Throw in
        let { argument; comments = _ } = stmt in
        ignore @@ this#expression argument;
        this#raise_abrupt_completion AbruptCompletion.throw

      (** Control flow **)
      method! if_statement _loc stmt =
        let open Flow_ast.Statement.If in
        let { test; consequent; alternate; _ } = stmt in
        this#push_refinement_scope empty_refinements;
        ignore @@ this#expression_refinement test;
        let test_refinements = this#peek_new_refinements () in
        let env0 = this#env_without_latest_refinements in
        (* collect completions and environments of every branch *)
        let then_completion_state =
          this#run_to_completion (fun () ->
              ignore @@ this#if_consequent_statement ~has_else:(alternate <> None) consequent
          )
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
                   alternate
          )
        in
        (* merge environments *)
        let else_env_no_refinements = this#env_without_latest_refinements in
        let else_env_with_refinements = this#env in
        this#pop_refinement_scope ();
        this#reset_env env0;
        this#merge_conditional_branches_with_refinements
          (then_env_no_refinements, then_env_with_refinements, then_completion_state)
          (else_env_no_refinements, else_env_with_refinements, else_completion_state);

        (* merge completions *)
        let if_completion_states = (then_completion_state, [else_completion_state]) in
        this#merge_completion_states if_completion_states;
        stmt

      method! conditional _loc (expr : (ALoc.t, ALoc.t) Flow_ast.Expression.Conditional.t) =
        let open Flow_ast.Expression.Conditional in
        let { test; consequent; alternate; comments = _ } = expr in
        this#push_refinement_scope empty_refinements;
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
          ( consequent_env_no_refinements,
            consequent_env_with_refinements,
            consequent_completion_state
          )
          (alternate_env_no_refinements, alternate_env_with_refinements, alternate_completion_state);

        (* merge completions *)
        let conditional_completion_states =
          (consequent_completion_state, [alternate_completion_state])
        in
        this#merge_completion_states conditional_completion_states;
        expr

      method merge_conditional_branches_with_refinements
          (env1, refined_env1, completion_state1) (env2, refined_env2, completion_state2) : unit =
        (* We only want to merge the refined environments from the two branches of an if-statement
         * if there was an assignment in one of the branches. Otherwise, merging the positive and
         * negative branches of the refinement into a union would be unnecessary work to
         * reconstruct the original type.
         *
         * If one of the branches abnormally completes then we can just take the refinements
         * from the other branch. *)
        match (completion_state1, completion_state2) with
        | (None, Some _) -> this#reset_env refined_env1
        | (Some _, None) -> this#reset_env refined_env2
        | _ ->
          SMap.iter
            (fun name { val_ref; heap_refinements; _ } ->
              let { Env.env_val = value1; heap_refinements = _; def_loc = _ } =
                SMap.find name env1
              in
              let { Env.env_val = value2; heap_refinements = _; def_loc = _ } =
                SMap.find name env2
              in
              let {
                Env.env_val = refined_value1;
                heap_refinements = heap_refinements1;
                def_loc = _;
              } =
                SMap.find name refined_env1
              in
              let {
                Env.env_val = refined_value2;
                heap_refinements = heap_refinements2;
                def_loc = _;
              } =
                SMap.find name refined_env2
              in
              (* If the same key exists on both versions of the object then we can
               * merge the two heap refinements, even though the underlying value
               * has changed. This is because the final object does indeed have
               * one of the two refinements at the merge *)
              heap_refinements := this#merge_heap_refinements heap_refinements1 heap_refinements2;
              if Val.id_of_val value1 = Val.id_of_val value2 then
                val_ref := value1
              else
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
              (fun name { Env.env_val = env_val1; heap_refinements = _; def_loc = _ } acc ->
                let { Env.env_val = env_val2; heap_refinements = _; def_loc = _ } =
                  SMap.find name pre_env
                in
                let normalized_val1 = Val.normalize_through_refinements env_val1.Val.write_state in
                let normalized_val2 = Val.normalize_through_refinements env_val2.Val.write_state in
                if Val.WriteSet.equal normalized_val1 normalized_val2 then
                  acc
                else
                  RefinementKey.lookup_of_name name :: acc)
              post_env
              []
        )

      method havoc_changed_vars changed_vars =
        List.iter
          (fun lookup ->
            let { RefinementKey.base; projections } = lookup in
            let { val_ref; havoc; heap_refinements; def_loc = _; kind } =
              SMap.find base env_state.env
            in
            (* If a var is changed then all the heap refinements on that var should
             * also be havoced. If only heap refinements are havoced then there's no
             * need to havoc the subject of the projection *)
            match projections with
            | [] ->
              this#havoc_heap_refinements heap_refinements;
              if kind <> Bindings.Const && not (Val.is_undeclared_or_skipped !val_ref) then
                val_ref :=
                  Base.List.fold
                    ~init:havoc
                    ~f:(fun acc write -> Val.merge acc (Val.of_write write))
                    (Val.writes_of_uninitialized this#refinement_may_be_undefined !val_ref)
            | _ -> heap_refinements := HeapRefinementMap.remove projections !heap_refinements)
          changed_vars

      method handle_continues loop_completion_state continues =
        this#run_to_completion (fun () ->
            this#commit_abrupt_completion_matching
              (AbruptCompletion.mem continues)
              loop_completion_state
        )

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
      method post_loop_refinements { total; _ } =
        if not AbruptCompletion.(mem (List.map fst env_state.abrupt_completion_envs) (break None))
        then
          match total with
          | None -> ()
          | Some total ->
            let (applied, _) = this#normalize_total_refinements (Not total) in
            applied
            |> IMap.iter (fun _ (lookup, refinement_id) ->
                   let refine_val x = Val.refinement refinement_id x in
                   this#map_val_with_lookup lookup refine_val
               )

      (*
       * Unlike the ssa_builder, the name_resolver does not create REF unresolved
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
            this#push_refinement_scope empty_refinements;
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
            | Some env -> this#merge_loop_guard_env_after_loop env);
            this#post_loop_refinements guard_refinements;

            let completion_states = make_completion_states loop_completion_state in
            let completion_state =
              this#run_to_completion (fun () -> this#merge_completion_states completion_states)
            in
            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state
        )

      method! while_ _loc (stmt : (ALoc.t, ALoc.t) Flow_ast.Statement.While.t) =
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
        let { Flow_ast.Statement.VariableDeclaration.declarations; kind; comments = _ } = decl in
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

      method scoped_for_in_or_of_statement traverse_left body =
        (* You might be wondering why the lhs has to be scouted-- the LHS can be a pattern that
         * includes a default write with a variable that is written to inside the loop. It's
         * critical that we catch loops in the dependency graph with such variables, since the
         * ordering algorithm will not have a good place to ask for an annotation in that case.
         *)
        let scout () =
          traverse_left ();
          ignore @@ this#run_to_completion (fun () -> ignore @@ this#statement body)
        in
        let visit_guard_and_body () =
          let env = this#env in
          traverse_left ();
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
        this#push_refinement_scope empty_refinements;
        ignore @@ this#expression_refinement right;
        this#scoped_for_in_or_of_statement traverse_left body;
        this#pop_refinement_scope ();
        stmt

      method! scoped_for_of_statement _loc stmt =
        let open Flow_ast.Statement.ForOf in
        let { left; right; body; await = _; comments = _ } = stmt in
        (* This is only evaluated once and so does not need to be scouted *)
        ignore @@ this#expression right;
        let traverse_left () = ignore (this#for_of_statement_lhs left) in
        this#scoped_for_in_or_of_statement traverse_left body;
        stmt

      method! switch loc switch =
        let open Flow_ast.Statement.Switch in
        let incoming_env = this#env in
        let { discriminant; cases; comments = _ } = switch in
        let _ = this#expression discriminant in
        let lexical_hoist = new lexical_hoister ~flowmin_compatibility:false ~enable_enums in
        let cases_with_lexical_bindings =
          Base.List.map cases ~f:(fun ((_, { Case.consequent; _ }) as case) ->
              let bindings = lexical_hoist#acc |> Bindings.to_map in
              let _ = lexical_hoist#statement_list consequent in
              (case, bindings)
          )
        in
        this#run
          (fun () ->
            ignore
            @@ this#with_bindings
                 ~lexical:true
                 loc
                 lexical_hoist#acc
                 (this#switch_cases_with_lexical_bindings loc discriminant)
                 cases_with_lexical_bindings)
          ~finally:(fun () ->
            let post_env = this#env in
            (* After all refinements and potential shadowing inside switch,
               we need to re-read the discriminant to restore it. *)
            this#reset_env incoming_env;
            ignore @@ this#expression switch.discriminant;
            this#reset_env post_env);
        switch

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
      method private switch_cases_with_lexical_bindings
          switch_loc discriminant cases_with_lexical_bindings =
        let incoming_env = this#env in
        this#expecting_abrupt_completions (fun () ->
            let (case_starting_env, case_completion_states, fallthrough_env, has_default) =
              List.fold_left
                (fun acc ((loc, case), bindings) ->
                  this#env_switch_case discriminant acc (loc, case, bindings))
                (incoming_env, [], None, false)
                cases_with_lexical_bindings
            in
            this#reset_env case_starting_env;
            ( if not has_default then
              let discriminant_after_all_negated_refinements =
                this#get_val_of_expression discriminant
              in
              match discriminant_after_all_negated_refinements with
              | Some discriminant ->
                env_state <-
                  {
                    env_state with
                    values =
                      L.LMap.add
                        switch_loc
                        {
                          def_loc = None;
                          value = discriminant;
                          binding_kind_opt = None;
                          name = None;
                        }
                        env_state.values;
                  }
              | None -> ()
            );

            (match fallthrough_env with
            (* If the switch has a default then it is exhaustive. Thus, the post-env can be
             * determined by joining all of the breaks with the last fallthrough env. If there
             * was no fallthrough env, then the we can use empty as the base. *)
            | Some env when has_default -> this#reset_env env
            | None when has_default -> this#reset_env this#empty_env
            (* If the switch wasn't exhaustive then merge with the case_starting_env as a base. If
             * the last case fell out then merge that in too. *)
            | Some fallthrough -> this#merge_remote_env fallthrough
            | _ -> ());

            (* In general, cases are non-exhaustive, but if it has a default case then it is! *)
            let completion_state =
              if has_default then
                (* Since there is a default we know there is at least one element in this
                 * list, which means calling List.hd or tail will not fail *)
                let first_state = List.hd case_completion_states in
                let remaining_states = List.tl case_completion_states in
                this#run_to_completion (fun () ->
                    this#merge_completion_states (first_state, remaining_states)
                )
              else
                None
            in

            this#commit_abrupt_completion_matching
              AbruptCompletion.(mem [break None])
              completion_state
        );
        cases_with_lexical_bindings

      method private env_switch_case
          discriminant
          (case_starting_env, case_completion_states, fallthrough_env, has_default)
          (case_loc, case, lexical_bindings) =
        let open Ast.Statement.Switch.Case in
        let { test; consequent; comments = _ } = case in
        this#reset_env case_starting_env;
        (* Reset discriminant *)
        this#push_refinement_scope empty_refinements;
        let (has_default, latest_refinements, case_starting_env) =
          match test with
          | None -> (true, empty_refinements, this#env)
          | Some test ->
            (* As a convention, locate refined versions of the discriminant on
               case locations, in order to read them from the environment for
               filtered TestProp checks. *)
            begin
              match discriminant with
              | (_, Ast.Expression.Member { Ast.Expression.Member._object; _ }) ->
                begin
                  match this#get_val_of_expression _object with
                  | None ->
                    let values = L.LMap.remove case_loc env_state.values in
                    env_state <- { env_state with values }
                  | Some refined_v ->
                    let values =
                      L.LMap.add
                        case_loc
                        { def_loc = None; value = refined_v; binding_kind_opt = None; name = None }
                        env_state.values
                    in
                    env_state <- { env_state with values }
                end
              | _ -> ()
            end;
            ignore @@ this#expression test;
            let (loc, _) = test in
            this#eq_test ~strict:true ~sense:true ~cond_context:SwitchTest loc discriminant test;
            (has_default, this#peek_new_refinements (), this#env_without_latest_refinements)
        in
        (match fallthrough_env with
        | None -> ()
        | Some fallthrough -> this#merge_self_env fallthrough);
        let () =
          lexical_bindings
          |> SMap.iter (fun name (kind, (loc, _)) ->
                 match kind with
                 | Bindings.Let
                 | Bindings.Const ->
                   let { val_ref; heap_refinements; _ } = SMap.find name env_state.env in
                   this#havoc_heap_refinements heap_refinements;
                   val_ref := Val.declared_but_skipped name loc
                 | _ -> ()
             )
        in
        let case_completion_state =
          this#run_to_completion (fun () -> ignore @@ this#statement_list consequent)
        in
        let fallthrough_env =
          match case_completion_state with
          | None -> Some this#env
          | Some _ -> None
        in
        this#pop_refinement_scope ();
        this#reset_env case_starting_env;
        let negated_refinements = this#negate_refinements latest_refinements in
        this#push_refinement_scope negated_refinements;
        let case_starting_env = this#env in
        this#pop_refinement_scope ();
        ( case_starting_env,
          case_completion_state :: case_completion_states,
          fallthrough_env,
          has_default
        )

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
      method! try_catch _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Try.t) =
        this#expecting_abrupt_completions (fun () ->
            let open Ast.Statement.Try in
            let { block = (loc, block); handler; finalizer; comments = _ } = stmt in
            let try_entry_env = this#env in
            let try_completion_state =
              this#run_to_completion (fun () -> ignore @@ this#block loc block)
            in
            let try_exit_env = this#env in
            this#merge_env try_entry_env try_exit_env;
            let catch_entry_env = this#env in
            let catch_completion_state_opt =
              match handler with
              | Some (loc, clause) ->
                this#run_to_completion (fun () -> ignore @@ this#catch_clause loc clause)
              | None ->
                (* No catch is like having a catch that always re-throws the error from the
                 * try block.*)
                Some AbruptCompletion.Throw
            in
            let catch_exit_env = this#env in
            this#merge_env try_exit_env catch_exit_env;
            let try_catch_completion_states =
              (try_completion_state, [catch_completion_state_opt])
            in
            let completion_state =
              this#run_to_completion (fun () ->
                  this#merge_completion_states try_catch_completion_states
              )
            in
            this#commit_abrupt_completion_matching AbruptCompletion.all completion_state;
            begin
              match finalizer with
              | Some (_loc, block) ->
                this#merge_env catch_entry_env catch_exit_env;
                ignore @@ this#block loc block
              | None ->
                (match catch_completion_state_opt with
                | None -> this#merge_env try_exit_env catch_exit_env
                | Some _ -> this#reset_env try_exit_env)
            end;
            this#from_completion completion_state
        );
        stmt

      (* We also havoc state when entering functions and exiting calls. *)
      method! lambda params predicate body =
        this#expecting_abrupt_completions (fun () ->
            let env = this#env in
            this#run
              (fun () ->
                this#havoc_uninitialized_env;
                let completion_state =
                  this#run_to_completion (fun () -> super#lambda params predicate body)
                in
                this#commit_abrupt_completion_matching
                  AbruptCompletion.(mem [return; throw])
                  completion_state)
              ~finally:(fun () -> this#reset_env env)
        )

      method! class_property loc prop =
        let open Ast.Class.Property in
        let { static; _ } = prop in
        if static then
          super#class_property loc prop
        else
          let env = this#env in
          this#run
            (fun () ->
              this#havoc_uninitialized_env;
              ignore @@ super#class_property loc prop)
            ~finally:(fun () -> this#reset_env env);
          prop

      method! class_private_field loc field =
        let open Ast.Class.PrivateField in
        let { static; _ } = field in
        if static then
          super#class_private_field loc field
        else
          let env = this#env in
          this#run
            (fun () ->
              this#havoc_uninitialized_env;
              ignore @@ super#class_private_field loc field)
            ~finally:(fun () -> this#reset_env env);
          field

      method! declare_function loc expr =
        match Declare_function_utils.declare_function_to_function_declaration_simple loc expr with
        | Some stmt ->
          let _ = this#statement (loc, stmt) in
          expr
        | None -> super#declare_function loc expr

      method! call loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Call.t) =
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
                }
              )
            ) ->
            let _ = List.map this#expression_or_spread other_args in
            this#raise_abrupt_completion AbruptCompletion.throw
          | ( None,
              ( _,
                {
                  Ast.Expression.ArgList.arguments = Ast.Expression.Expression cond :: other_args;
                  comments = _;
                }
              )
            ) ->
            this#push_refinement_scope empty_refinements;
            ignore @@ this#expression_refinement cond;
            let _ = List.map this#expression_or_spread other_args in
            this#pop_refinement_scope_without_unrefining ()
          | ( _,
              (_, { Ast.Expression.ArgList.arguments = Ast.Expression.Spread _ :: _; comments = _ })
            ) ->
            error_todo
          | (Some _, _) -> error_todo
        else
          this#havoc_current_env ~all:false;
        expr

      method! new_ loc (expr : (ALoc.t, ALoc.t) Ast.Expression.New.t) =
        ignore @@ super#new_ loc expr;
        this#havoc_current_env ~all:false;
        expr

      method private delete loc argument =
        let undefined_reason = mk_reason RVoid loc in
        let undefined = Val.undefined undefined_reason in
        let update_write_entries ~assigning =
          let write =
            if assigning then
              Env_api.AssigningWrite undefined_reason
            else
              Env_api.NonAssigningWrite
          in
          let write_entries = L.LMap.add (fst argument) write env_state.write_entries in
          env_state <- { env_state with write_entries }
        in
        match argument with
        | (_, Flow_ast.Expression.Identifier (id_loc, { Flow_ast.Identifier.name; _ })) ->
          let { kind; def_loc; val_ref; _ } = SMap.find name env_state.env in
          (match error_for_assignment_kind cx name id_loc def_loc kind None !val_ref with
          | None ->
            val_ref := undefined;
            update_write_entries ~assigning:true
          | Some err ->
            update_write_entries ~assigning:false;
            add_output err)
        | (_, Flow_ast.Expression.Member member) ->
          this#assign_member ~update_entry:true member loc undefined undefined_reason
        | _ -> ()

      method! unary_expression loc (expr : (ALoc.t, ALoc.t) Ast.Expression.Unary.t) =
        Ast.Expression.Unary.(
          let { argument; operator; comments = _ } = expr in
          ignore @@ this#expression argument;
          match operator with
          | Await -> this#havoc_current_env ~all:false
          | Delete -> this#delete loc argument
          | _ -> ()
        );
        expr

      method! yield loc (expr : ('loc, 'loc) Ast.Expression.Yield.t) =
        ignore @@ super#yield loc expr;
        this#havoc_current_env ~all:true;
        expr

      (* Labeled statements handle labeled breaks, but also push labeled continues
         that are expected to be handled by immediately nested loops. *)
      method! labeled_statement _loc (stmt : (ALoc.t, ALoc.t) Ast.Statement.Labeled.t) =
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
              completion_state
        );
        stmt

      method! statement (stmt : (ALoc.t, ALoc.t) Ast.Statement.t) =
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
      method! statement_list (stmts : (ALoc.t, ALoc.t) Ast.Statement.t list) =
        let open Ast.Statement in
        let (function_decls, other_stmts) =
          List.partition
            (function
              | (_, FunctionDeclaration _) -> true
              | (_, DeclareFunction _) -> true
              | ( _,
                  ExportDefaultDeclaration
                    ExportDefaultDeclaration.
                      { declaration = Declaration (_, FunctionDeclaration _); _ }
                ) ->
                true
              | ( _,
                  ExportNamedDeclaration
                    ExportNamedDeclaration.{ declaration = Some (_, FunctionDeclaration _); _ }
                ) ->
                true
              | ( _,
                  DeclareExportDeclaration
                    DeclareExportDeclaration.{ declaration = Some (Function _); _ }
                ) ->
                true
              | _ -> false)
            stmts
        in
        ignore @@ super#statement_list (function_decls @ other_stmts);
        stmts

      (* WHen the refinement scope we push is non-empty we want to make sure that the variables
       * that scope refines are given their new refinement writes in the environment *)
      method private push_refinement_scope
          ({ applied; changeset; total = _ } as new_latest_refinements) =
        env_state <-
          {
            env_state with
            latest_refinements = new_latest_refinements :: env_state.latest_refinements;
          };
        IMap.iter
          (fun ssa_id (lookup, refinement_id) ->
            let refine_val v =
              if Val.base_id_of_val v = ssa_id then
                Val.refinement refinement_id v
              else
                v
            in
            let create_val_for_heap =
              LookupMap.find_opt lookup changeset |> Base.Option.map ~f:Lazy.from_val
            in
            this#map_val_with_lookup lookup ?create_val_for_heap refine_val)
          applied

      (* See pop_refinement_scope. The only difference here is that we unrefine values deeply
       * instead of just at the top level. The reason for this is that intermediate control-flow
       * can introduce refinement writes into phi nodes, and we don't want those refinements to
       * escape the scope of the loop. You may find it instructive to change the calls to
       * just pop_refinement_scope to see the behavioral differences *)
      method private pop_refinement_scope_after_loop () =
        let { applied; _ } = List.hd env_state.latest_refinements in
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements };
        applied
        |> IMap.iter (fun _ (lookup, refinement_id) ->
               let unrefine_deeply x = Val.unrefine_deeply refinement_id x in
               this#map_val_with_lookup lookup unrefine_deeply
           )

      (* Invariant refinement scopes can be popped, but the refinement should continue living on.
       * To model that, we pop the refinement scope but do not unrefine the refinements. The
       * refinements live on in the Refinement writes in the env. *)
      method private pop_refinement_scope_without_unrefining () =
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements }

      (* When a refinement scope ends, we need to undo the refinement applied to the
       * variables mentioned in the latest_refinements head. Some of these values may no
       * longer be the refined value, in which case Val.unrefine will be a no-op. Otherwise,
       * the Refinement Val.t is replaced with the original Val.t that was being refined, with
       * the same original ssa_id. That means that if for some reason you needed to push the refinement
       * scope again that you would re-refine the unrefined variables, which is desirable in cases
       * where we juggle refinement scopes like we do for nullish coalescing *)
      method private pop_refinement_scope () =
        let { applied; changeset; _ } = List.hd env_state.latest_refinements in
        env_state <- { env_state with latest_refinements = List.tl env_state.latest_refinements };
        changeset
        |> LookupMap.iter (fun { RefinementKey.base; projections } v ->
               match (projections, SMap.find_opt base env_state.env) with
               | (_ :: _, Some { heap_refinements; _ })
                 when Base.Option.value_map
                        (HeapRefinementMap.find_opt projections !heap_refinements)
                        ~f:(fun v' -> Val.base_id_of_val v' = Val.base_id_of_val v)
                        ~default:false ->
                 heap_refinements := HeapRefinementMap.remove projections !heap_refinements
               | _ -> ()
           );
        applied
        |> IMap.iter (fun _ (lookup, refinement_id) ->
               let unrefine x = Val.unrefine refinement_id x in
               this#map_val_with_lookup lookup unrefine
           )

      method private peek_new_refinements () = List.hd env_state.latest_refinements

      method private negate_refinements { applied = _; changeset = _; total } =
        match total with
        | None -> empty_refinements
        | Some total ->
          let negated = Not total in
          let (applied, changeset) = this#normalize_total_refinements negated in
          { applied; changeset; total = Some negated }

      method private conjunct_all_refinements refinement_scopes =
        match refinement_scopes with
        | [] -> empty_refinements
        | [x] -> x
        | _ -> List.fold_left (this#merge ~conjunction:true) empty_refinements refinement_scopes

      method private negate_new_refinements () =
        let head = List.hd env_state.latest_refinements in
        let new_latest_refinements = this#negate_refinements head in
        this#pop_refinement_scope ();
        this#push_refinement_scope new_latest_refinements

      method private merge_self_refinement_scope { applied = _; changeset = _; total } =
        let { applied = _; changeset = _; total = head_total } =
          List.hd env_state.latest_refinements
        in
        let merged = conj_total head_total total in
        let (applied, changeset) =
          Base.Option.value_map
            ~f:this#normalize_total_refinements
            ~default:(IMap.empty, LookupMap.empty)
            merged
        in
        let refis = { applied; changeset; total = merged } in
        this#pop_refinement_scope ();
        this#push_refinement_scope refis

      method private normalize_total_refinements total =
        let rec nnf total =
          match total with
          | Not (And (t1, t2)) -> Or (nnf (Not t1), nnf (Not t2))
          | Not (Or (t1, t2)) -> And (nnf (Not t1), nnf (Not t2))
          | Not (Not t) -> nnf t
          | And (t1, t2) -> And (nnf t1, nnf t2)
          | Or (t1, t2) -> Or (nnf t1, nnf t2)
          | Refinements _
          | Not (Refinements _) ->
            total
        in
        let rec recur total =
          match total with
          | Refinements (r, c) -> (r, c)
          | Not (Refinements (r, c)) ->
            ( IMap.map
                (fun (lookup, refinement_id) ->
                  let new_refinement_id = this#new_id () in
                  env_state <-
                    {
                      env_state with
                      refinement_heap =
                        IMap.add new_refinement_id (NOT refinement_id) env_state.refinement_heap;
                    };
                  (lookup, new_refinement_id))
                r,
              c
            )
          | Not _ -> failwith "Negations not resolved"
          | And (t1, t2) ->
            let (r1, c1) = recur t1 in
            let (r2, c2) = recur t2 in
            let r =
              IMap.union
                ~combine:(fun _ (lookup1, rid1) (lookup2, rid2) ->
                  if lookup1 = lookup2 then begin
                    let new_refinement_id = this#new_id () in
                    env_state <-
                      {
                        env_state with
                        refinement_heap =
                          IMap.add new_refinement_id (AND (rid1, rid2)) env_state.refinement_heap;
                      };
                    Some (lookup1, new_refinement_id)
                  end else
                    None)
                r1
                r2
            in
            let c = LookupMap.union c1 c2 in
            (r, c)
          | Or (t1, t2) ->
            let (r1, c1) = recur t1 in
            let (r2, c2) = recur t2 in
            let r =
              IMap.merge
                (fun _ r1 r2 ->
                  match (r1, r2) with
                  | (None, _)
                  | (_, None) ->
                    None
                  | (Some (lookup1, rid1), Some (lookup2, rid2)) ->
                    if lookup1 = lookup2 then begin
                      let new_refinement_id = this#new_id () in
                      env_state <-
                        {
                          env_state with
                          refinement_heap =
                            IMap.add new_refinement_id (OR (rid1, rid2)) env_state.refinement_heap;
                        };
                      Some (lookup1, new_refinement_id)
                    end else
                      None)
                r1
                r2
            in
            let c =
              LookupMap.merge
                (fun _ c1 c2 ->
                  match (c1, c2) with
                  | (None, _)
                  | (_, None) ->
                    None
                  | (Some v1, Some v2) -> Some (Val.merge v1 v2))
                c1
                c2
            in
            (r, c)
        in
        let nnf = nnf total in
        recur nnf

      method private add_projection ({ RefinementKey.lookup; loc } as key) =
        let create_val_for_heap =
          lazy
            (let reason = mk_reason (RefinementKey.reason_desc key) loc in
             let write_entries =
               L.LMap.add loc (Env_api.AssigningWrite reason) env_state.write_entries
             in
             env_state <- { env_state with write_entries };
             Val.projection loc
            )
        in
        this#map_val_with_lookup lookup ~create_val_for_heap (fun x -> x)

      (* Commit a set of refinements, all of which are linked biconditionally. If these refinements are negated,
         then the negation of each refinement should hold; separate calls to this function in the same
         refinement scope produce separate maps that are connected with an AND--and if they are negated, will produce
         an OR of negations of the maps.

         To see how this works, consider two cases:

          First,
            if (x && y) { } else { }
          In this case, we will call commit_refinement twice, each time with a Truthy refinement on either x or y.
          Inside the then case, x and y are both Truthy, but within the else case, we'll end up with a proposition
          like Or (x Falsey, y Falsey). This means that within the else case we don't actually know anything concrete
          about x or y.

          Compare that to
            if {x.y} { } else { }
          For this case, we will call commit_refinement only once, with a map that contains both the refinement x HasProp(y) and
          x.y Truthy. When we negate this, because we only called commit_refinement once and therefore our refinement propositon
          contains just a single set of refinements (not multiple ones conjuncted together), we'll end up with
          x NotHasProp(y) and x.y Falsey--which is what we want.
      *)
      method private commit_refinement (refinements : (ALoc.t * refinement) LookupMap.t) =
        let { applied; changeset = old_changeset; total } = List.hd env_state.latest_refinements in

        let (applied, changeset, map) =
          LookupMap.fold
            (fun key (loc, refinement) (applied, changeset, map) ->
              (* Prevent refinement on undeclared const/let.
                 We should only error if the undeclared const/let is in the same activation scope
                 as the current one. Although we have no information about the current scope, this
                 is not a problem. We will force initialization for all bindings before we visit a
                 lambda. *)
              let should_not_refine =
                let { RefinementKey.base; projections } = key in
                if projections = [] then
                  match SMap.find base env_state.env with
                  | { val_ref; kind = Bindings.Const | Bindings.Let; def_loc = Some def_loc; _ }
                    when Val.is_undeclared_or_skipped !val_ref ->
                    refinement
                    |> fst
                    |> L.LSet.iter (fun loc ->
                           add_output
                             Error_message.(
                               EBindingError
                                 (EReferencedBeforeDeclaration, loc, OrdinaryName base, def_loc)
                             )
                       );
                    true
                  | _ -> false
                else
                  false
              in
              if should_not_refine then
                (applied, changeset, map)
              else
                let add_refinements v =
                  let ssa_id = Val.base_id_of_val v in
                  let refinement_id = this#new_id () in
                  env_state <-
                    {
                      env_state with
                      refinement_heap =
                        IMap.add refinement_id (BASE refinement) env_state.refinement_heap;
                    };
                  let latest_refinement_opt = IMap.find_opt ssa_id applied in
                  let (final_refinement_id, unrefined_v) =
                    match latest_refinement_opt with
                    | Some (_, existing_refinement_id) ->
                      let unrefined_v = Val.unrefine existing_refinement_id v in
                      let new_refinement_id = this#new_id () in
                      let new_chain = AND (existing_refinement_id, refinement_id) in
                      env_state <-
                        {
                          env_state with
                          refinement_heap =
                            IMap.add new_refinement_id new_chain env_state.refinement_heap;
                        };

                      (new_refinement_id, unrefined_v)
                    | None -> (refinement_id, v)
                  in
                  ( (ssa_id, refinement_id, final_refinement_id),
                    Val.refinement final_refinement_id unrefined_v
                  )
                in
                match
                  this#map_val_with_lookup_result
                    key
                    ~create_val_for_heap:
                      ( lazy
                        (let reason =
                           mk_reason
                             (RefinementKey.reason_desc { RefinementKey.lookup = key; loc })
                             loc
                         in
                         let write_entries =
                           L.LMap.add loc (Env_api.AssigningWrite reason) env_state.write_entries
                         in
                         env_state <- { env_state with write_entries };
                         Val.projection loc
                        )
                        )
                    add_refinements
                with
                | Some ((ssa_id, base_refinement_id, final_refinement_id), change) ->
                  ( IMap.add ssa_id (key, final_refinement_id) applied,
                    Base.Option.value_map
                      ~f:(fun change -> LookupMap.add key change changeset)
                      ~default:changeset
                      change,
                    IMap.add ssa_id (key, base_refinement_id) map
                  )
                | None -> (applied, changeset, map))
            refinements
            (applied, LookupMap.empty, IMap.empty)
        in

        env_state <-
          {
            env_state with
            latest_refinements =
              {
                applied;
                changeset = LookupMap.union changeset old_changeset;
                total = conj_total total (Some (Refinements (map, changeset)));
              }
              :: List.tl env_state.latest_refinements;
          }

      method add_single_refinement key refi = this#commit_refinement (this#start_refinement key refi)

      method start_refinement { RefinementKey.loc; lookup } refi =
        LookupMap.singleton lookup (loc, refi)

      method extend_refinement { RefinementKey.loc; lookup } refi refis =
        LookupMap.union
          ~combine:(fun _ (loc1, (locs1, refi1)) (loc2, (locs2, refi2)) ->
            if loc1 <> loc2 then failwith "Loc mismatch";
            Some (loc1, (L.LSet.union locs1 locs2, AndR (refi1, refi2))))
          refis
          (LookupMap.singleton lookup (loc, refi))

      method identifier_refinement ((loc, ident) as identifier) =
        ignore @@ this#identifier identifier;
        let { Flow_ast.Identifier.name; _ } = ident in
        let { val_ref; _ } = SMap.find name env_state.env in
        if not (Val.is_undeclared_or_skipped !val_ref) then
          this#add_single_refinement (RefinementKey.of_name name loc) (L.LSet.singleton loc, TruthyR)

      method assignment_refinement loc assignment =
        ignore @@ this#assignment loc assignment;
        let open Flow_ast.Expression.Assignment in
        match assignment.left with
        | ( id_loc,
            Flow_ast.Pattern.Identifier
              { Flow_ast.Pattern.Identifier.name = (_, { Flow_ast.Identifier.name; _ }); _ }
          ) ->
          this#add_single_refinement
            (RefinementKey.of_name name id_loc)
            (L.LSet.singleton loc, TruthyR)
        | _ -> ()

      method private merge
          ~conjunction
          { applied = _; changeset = _; total = total1 }
          { applied = _; changeset = _; total = total2 } =
        let total =
          if conjunction then
            conj_total total1 total2
          else
            match (total1, total2) with
            | (Some total1, Some total2) -> Some (Or (total1, total2))
            | (Some total, _)
            | (_, Some total) ->
              Some total
            | (None, None) -> None
        in
        let (applied, changeset) =
          Base.Option.value_map
            ~f:this#normalize_total_refinements
            ~default:(IMap.empty, LookupMap.empty)
            total
        in
        { applied; changeset; total }

      method private merge_refinement_scopes
          ~conjunction lhs_latest_refinements rhs_latest_refinements =
        let new_latest_refinements =
          this#merge ~conjunction lhs_latest_refinements rhs_latest_refinements
        in
        this#merge_self_refinement_scope new_latest_refinements

      (* Refines an expr if that expr has a refinement key, othewise does nothing *)
      method add_refinement_to_expr expr refinement =
        match RefinementKey.of_expression expr with
        | None -> ()
        | Some refinement_key -> this#add_single_refinement refinement_key refinement

      method logical_refinement expr =
        let { Flow_ast.Expression.Logical.operator; left = (loc, _) as left; right; comments = _ } =
          expr
        in
        this#push_refinement_scope empty_refinements;
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
            this#push_refinement_scope empty_refinements;
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
               truthy and non-maybe qualities.

               We can't use null_test here because null_test requires some actual null for the
               sentinel refinement it can create. We can add some complexity here to introduce a
               synthetic null Val.t and get sentinel refinements against null here, but that seems
               like an unlikely way for nullish coalescing to be used. Instead, we simply add a
               NotR MaybeR refinement to the left *)
            ignore (this#expression left);
            this#add_refinement_to_expr left (L.LSet.singleton loc, NotR MaybeR);
            let nullish = this#peek_new_refinements () in
            let env1 = this#env_without_latest_refinements in
            this#negate_new_refinements ();
            this#push_refinement_scope empty_refinements;
            let rhs_completion_state =
              this#run_to_completion (fun () -> ignore (this#expression_refinement right))
            in
            let rhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#pop_refinement_scope ();
            this#push_refinement_scope empty_refinements;
            this#add_refinement_to_expr left (L.LSet.singleton loc, TruthyR);
            let truthy_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            this#push_refinement_scope empty_refinements;
            this#merge_refinement_scopes ~conjunction:true nullish truthy_refinements;
            let lhs_latest_refinements = this#peek_new_refinements () in
            this#pop_refinement_scope ();
            (lhs_latest_refinements, rhs_latest_refinements, env1, rhs_completion_state)
        in
        let conjunction =
          match operator with
          | Flow_ast.Expression.Logical.Or
          | Flow_ast.Expression.Logical.NullishCoalesce ->
            false
          | Flow_ast.Expression.Logical.And -> true
        in
        match rhs_completion_state with
        | Some AbruptCompletion.Throw ->
          let env2 = this#env in
          this#reset_env env1;
          this#push_refinement_scope lhs_latest_refinements;
          this#pop_refinement_scope_without_unrefining ();
          this#merge_self_env env2
        | _ ->
          this#merge_self_env env1;
          this#merge_refinement_scopes ~conjunction lhs_latest_refinements rhs_latest_refinements

      method null_test ~strict ~sense loc expr other =
        (* Negating if sense is false is handled by negate_new_refinements. *)
        let refis = this#maybe_sentinel ~sense:true ~strict loc expr other in
        let refis =
          match RefinementKey.of_expression expr with
          | None -> refis
          | Some key ->
            let refinement =
              if strict then
                NullR
              else
                NotR MaybeR
            in
            this#extend_refinement key (L.LSet.singleton loc, refinement) refis
        in
        ignore @@ this#optional_chain expr;
        this#commit_refinement refis;
        if strict <> sense then this#negate_new_refinements ()

      method void_test ~sense ~strict ~check_for_bound_undefined loc expr other =
        (* Negating if sense is true is handled by negate_new_refinements. *)
        let refis = this#maybe_sentinel ~sense:false ~strict loc expr other in
        let is_global_undefined () =
          match SMap.find_opt "undefined" env_state.env with
          | None -> false
          | Some { val_ref = v; _ } -> Val.is_global_undefined !v
        in
        if (not check_for_bound_undefined) || is_global_undefined () then begin
          let refis =
            match RefinementKey.of_expression expr with
            | None -> refis
            | Some key ->
              let refinement =
                if strict then
                  NotR UndefinedR
                else
                  NotR MaybeR
              in
              this#extend_refinement key (L.LSet.singleton loc, refinement) refis
          in
          ignore @@ this#optional_chain expr;
          this#commit_refinement refis;
          if sense then this#negate_new_refinements ()
        end else begin
          ignore @@ this#optional_chain expr;
          this#commit_refinement refis
        end

      method typeof_test loc arg typename sense =
        let (refinement, undef) =
          match typename with
          | "boolean" -> (Some (BoolR loc), false)
          | "function" -> (Some FunctionR, false)
          | "number" -> (Some (NumberR loc), false)
          | "object" -> (Some ObjectR, false)
          | "string" -> (Some (StringR loc), false)
          | "symbol" -> (Some (SymbolR loc), false)
          | "undefined" -> (Some UndefinedR, true)
          | _ -> (None, false)
        in
        match (refinement, RefinementKey.of_expression arg) with
        | (Some ref, Some refinement_key) ->
          ignore @@ this#optional_chain arg;
          let refinement =
            if sense && not undef then
              ref
            else
              NotR ref
          in
          this#add_single_refinement refinement_key (L.LSet.singleton loc, refinement);
          if sense && undef then this#negate_new_refinements ()
        | _ -> ignore @@ this#optional_chain arg

      method literal_test ~strict ~sense loc expr refinement other =
        (* Negating if sense is false is handled by negate_new_refinements. *)
        let refis = this#maybe_sentinel ~sense:true ~strict loc expr other in
        let refis =
          match RefinementKey.of_expression expr with
          | Some ({ RefinementKey.lookup; loc = _ } as key) when strict ->
            (match lookup with
            | { RefinementKey.base; projections = [] } ->
              let { val_ref = _; def_loc; _ } = SMap.find base env_state.env in
              (match def_loc with
              | None -> ()
              | Some def_loc -> add_literal_subtype_test def_loc refinement)
            | _ -> ());
            this#extend_refinement key (L.LSet.singleton loc, refinement) refis
          | _ -> refis
        in
        ignore @@ this#optional_chain expr;
        this#commit_refinement refis;
        if not sense then this#negate_new_refinements ()

      method maybe_sentinel ~sense ~strict loc expr (other_loc, _) =
        let open Flow_ast in
        let expr' =
          match expr with
          | (loc, Expression.OptionalMember { Expression.OptionalMember.member; _ }) ->
            (loc, Expression.Member member)
          | _ -> expr
        in
        let refis =
          match (strict, expr') with
          | ( true,
              ( _,
                Expression.Member
                  {
                    Expression.Member._object = (obj_loc, _) as _object;
                    property =
                      ( Expression.Member.PropertyIdentifier
                          (ploc, { Identifier.name = prop_name; _ })
                      | Expression.Member.PropertyExpression
                          (ploc, Expression.Literal { Literal.value = Literal.String prop_name; _ })
                        );
                    _;
                  }
              )
            ) ->
            (match RefinementKey.of_expression _object with
            | Some refinement_key ->
              let reason = mk_reason (RProperty (Some (OrdinaryName prop_name))) ploc in
              ( if RefinementKey.(refinement_key.lookup.projections) = [] then
                let { val_ref = _; def_loc; _ } =
                  SMap.find RefinementKey.(refinement_key.lookup.base) env_state.env
                in
                Base.Option.iter def_loc ~f:(fun def_loc ->
                    Context.add_new_env_matching_props cx (prop_name, other_loc, def_loc)
                )
              );
              let obj_reason = mk_reason (RefinementKey.reason_desc refinement_key) obj_loc in
              let write_entries =
                L.LMap.add
                  obj_loc
                  (Env_api.AssigningWrite obj_reason)
                  (L.LMap.add other_loc (Env_api.AssigningWrite reason) env_state.write_entries)
              in
              env_state <- { env_state with write_entries };
              let refinement =
                if sense then
                  SentinelR (prop_name, other_loc)
                else
                  NotR (SentinelR (prop_name, other_loc))
              in
              this#start_refinement refinement_key (L.LSet.singleton loc, refinement)
            | None -> LookupMap.empty)
          | _ -> LookupMap.empty
        in
        refis

      method eq_test ~strict ~sense ~cond_context loc left right =
        let open Flow_ast in
        match (left, right) with
        (* typeof expr ==/=== string *)
        | ( ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
            ),
            (_, Expression.Literal { Literal.value = Literal.String s; _ })
          )
        | ( (_, Expression.Literal { Literal.value = Literal.String s; _ }),
            ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
            )
          )
        | ( ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
            ),
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
                        }
                      );
                    ];
                  expressions = [];
                  comments = _;
                }
            )
          )
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
                        }
                      );
                    ];
                  expressions = [];
                  comments = _;
                }
            ),
            ( _,
              Expression.Unary
                { Expression.Unary.operator = Expression.Unary.Typeof; argument; comments = _ }
            )
          ) ->
          this#typeof_test loc argument s sense
        (* bool equality *)
        | (((lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ }) as other), expr)
        | (expr, ((lit_loc, Expression.Literal { Literal.value = Literal.Boolean lit; _ }) as other))
          ->
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonBoolR { loc = lit_loc; sense; lit })
            other
        (* string equality *)
        | (((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }) as other), expr)
        | (expr, ((lit_loc, Expression.Literal { Literal.value = Literal.String lit; _ }) as other))
        | ( expr,
            ( ( lit_loc,
                Expression.TemplateLiteral
                  {
                    Expression.TemplateLiteral.quasis =
                      [
                        ( _,
                          {
                            Expression.TemplateLiteral.Element.value =
                              { Expression.TemplateLiteral.Element.cooked = lit; _ };
                            _;
                          }
                        );
                      ];
                    _;
                  }
              ) as other
            )
          )
        | ( ( ( lit_loc,
                Expression.TemplateLiteral
                  {
                    Expression.TemplateLiteral.quasis =
                      [
                        ( _,
                          {
                            Expression.TemplateLiteral.Element.value =
                              { Expression.TemplateLiteral.Element.cooked = lit; _ };
                            _;
                          }
                        );
                      ];
                    _;
                  }
              ) as other
            ),
            expr
          ) ->
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonStrR { loc = lit_loc; sense; lit })
            other
        (* number equality *)
        | (((lit_loc, number_literal) as other), expr) when is_number_literal number_literal ->
          let raw = extract_number_literal number_literal in
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonNumR { loc = lit_loc; sense; lit = raw })
            other
        | (expr, ((lit_loc, number_literal) as other)) when is_number_literal number_literal ->
          let raw = extract_number_literal number_literal in
          this#literal_test
            ~strict
            ~sense
            loc
            expr
            (SingletonNumR { loc = lit_loc; sense; lit = raw })
            other
        (* expr op null *)
        | (((_, Expression.Literal { Literal.value = Literal.Null; _ }) as other), expr)
        | (expr, ((_, Expression.Literal { Literal.value = Literal.Null; _ }) as other)) ->
          this#null_test ~sense ~strict loc expr other
        (* expr op undefined *)
        | ( ( ( _,
                Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ })
              ) as undefined
            ),
            expr
          )
        | ( expr,
            ( ( _,
                Expression.Identifier (_, { Flow_ast.Identifier.name = "undefined"; comments = _ })
              ) as undefined
            )
          ) ->
          ignore @@ this#expression undefined;
          this#void_test ~sense ~strict ~check_for_bound_undefined:true loc expr undefined
        (* expr op void(...) *)
        | ( ((_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ }) as other),
            expr
          )
        | ( expr,
            ((_, Expression.Unary { Expression.Unary.operator = Expression.Unary.Void; _ }) as other)
          ) ->
          this#void_test ~sense ~strict ~check_for_bound_undefined:false loc expr other
        (* Member expressions compared against non-literals that include
         * an optional chain cannot refine like we do in literal cases. The
         * non-literal value we are comparing against may be null or undefined,
         * in which case we'd need to use the special case behavior. Since we can't
         * know at this point, we conservatively do not refine at all based on optional
         * chains by ignoring the output of maybe_sentinel.
         *
         * NOTE: Switch statements do not introduce sentinel refinements *)
        | (((_, Expression.Member _) as expr), other) ->
          ignore @@ this#expression expr;
          let refis = this#maybe_sentinel ~sense ~strict loc expr other in
          this#commit_refinement refis;
          ignore @@ this#expression other
        | (other, ((_, Expression.Member _) as expr)) when not (cond_context = SwitchTest) ->
          ignore @@ this#expression other;
          ignore @@ this#expression expr;
          let refis = this#maybe_sentinel ~sense ~strict loc expr other in
          this#commit_refinement refis
        | _ ->
          ignore @@ this#expression left;
          ignore @@ this#expression right

      method instance_test loc expr instance =
        ignore @@ this#optional_chain expr;
        ignore @@ this#expression instance;
        match RefinementKey.of_expression expr with
        | None -> ()
        | Some refinement_key ->
          let (instance_loc, _) = instance in
          ( if not (L.LMap.mem instance_loc env_state.values) then
            (* instance is not something the name_resolver can reason about.
               However, we still need to has a read and write entry, so we can
               record it in statement.ml and use it in new-env. *)
            let reason = mk_reason (RefinementKey.reason_desc refinement_key) instance_loc in
            let values =
              L.LMap.add
                instance_loc
                {
                  def_loc = None;
                  value = Val.one reason;
                  binding_kind_opt = Some Bindings.Const;
                  name = None;
                }
                env_state.values
            in
            let write_entries =
              L.LMap.add instance_loc (Env_api.AssigningWrite reason) env_state.write_entries
            in
            env_state <- { env_state with values; write_entries }
          );
          this#add_single_refinement refinement_key (L.LSet.singleton loc, InstanceOfR instance)

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
                       (_, { Flow_ast.Identifier.name = "Array"; comments = _ })
                   );
                 property =
                   Flow_ast.Expression.Member.PropertyIdentifier
                     (_, { Flow_ast.Identifier.name = "isArray"; comments = _ });
                 comments = _;
               }
           ) as callee;
         targs = _;
         arguments =
           ( _,
             {
               Flow_ast.Expression.ArgList.arguments = [Flow_ast.Expression.Expression arg];
               comments = _;
             }
           );
         comments = _;
        } ->
          let refi =
            match RefinementKey.of_expression arg with
            | None -> LookupMap.empty
            | Some refinement_key ->
              this#start_refinement refinement_key (L.LSet.singleton loc, IsArrayR)
          in

          ignore @@ this#expression callee;
          ignore @@ this#optional_chain arg;
          this#commit_refinement refi
        (* Latent refinements are only applied on function calls where the function call is an identifier *)
        | {
         Flow_ast.Expression.Call.callee = (_, Flow_ast.Expression.Identifier _) as callee;
         arguments;
         _;
        }
          when not (is_call_to_invariant callee) ->
          (* This case handles predicate functions. We ensure that this
           * is not a call to invariant and that the callee is an identifier.
           * The only other criterion that must be met for this call to produce
           * a refinement is that the arguments cannot contain a spread.
           *
           * Assuming there are no spreads we create a mapping from each argument
           * index to the refinement key at that index.
           *
           * The semantics for passing the same argument multiple times to predicate
           * function are sketchy. Pre-LTI Flow allows you to do this but it is buggy. See
           * https://fburl.com/vf52s7rb on v0.155.0
           *
           * We should strongly consider disallowing the same refinement key to
           * appear multiple times in the arguments. *)
          let { Flow_ast.Expression.ArgList.arguments = arglist; _ } = snd arguments in
          let is_spread = function
            | Flow_ast.Expression.Spread _ -> true
            | _ -> false
          in
          let refinement_keys =
            if List.exists is_spread arglist then
              []
            else
              List.map (fun arg -> RefinementKey.of_argument arg) arglist
          in
          ignore @@ this#expression callee;
          ignore @@ this#call_arguments arguments;
          this#havoc_current_env ~all:false;
          this#apply_latent_refinements callee refinement_keys
        | _ -> ignore @@ this#call loc call

      method unary_refinement
          loc ({ Flow_ast.Expression.Unary.operator; argument; comments = _ } as unary) =
        match operator with
        | Flow_ast.Expression.Unary.Not ->
          this#push_refinement_scope empty_refinements;
          ignore @@ this#expression_refinement argument;
          this#negate_new_refinements ();
          let negated_refinements = this#peek_new_refinements () in
          this#pop_refinement_scope ();
          this#merge_self_refinement_scope negated_refinements
        | _ -> ignore @@ this#unary_expression loc unary

      method private record_member_read (loc, expr) =
        let open Ast.Expression in
        match expr with
        | OptionalMember _
        | Member _ ->
          (match this#get_val_of_expression (loc, expr) with
          | None ->
            (* In some cases, we may re-visit the same expression multiple times via different
               environments--for example, visiting the discriminant of a switch statement. In
               these cases, it's possible that there was a val for an expression in a previous
               environment which is no longer available when seen through a subsequent. In order
               to prevent old environment values from "leaking" through, we need to actively remove
               values that may have previously existed but no longer do. *)
            let values = L.LMap.remove loc env_state.values in
            env_state <- { env_state with values }
          | Some refined_v ->
            (* We model a heap refinement as a separate const binding. We prefer this over using
             * None so that we can report errors when using this value in a type position *)
            let values =
              L.LMap.add
                loc
                {
                  def_loc = None;
                  value = refined_v;
                  binding_kind_opt = Some Bindings.Const;
                  name = None;
                }
                env_state.values
            in
            env_state <- { env_state with values })
        | _ -> ()

      method optional_chain (loc, expr) =
        let open Ast.Expression in
        this#record_member_read (loc, expr);
        let () =
          match expr with
          | OptionalMember _ -> this#member_expression_refinement loc expr LookupMap.empty
          | OptionalCall
              { OptionalCall.call = { Call.callee; targs; arguments; comments = _ }; optional } ->
            let refi =
              match RefinementKey.of_expression callee with
              | Some refinement_key_obj when optional ->
                this#start_refinement refinement_key_obj (L.LSet.singleton loc, NotR MaybeR)
              | _ -> LookupMap.empty
            in

            ignore @@ this#optional_chain callee;
            this#commit_refinement refi;
            let _targs' = Base.Option.map ~f:this#call_type_args targs in
            let _arguments' = this#call_arguments arguments in
            this#havoc_current_env ~all:false
          | Member mem -> ignore @@ this#member loc mem
          | Call call -> ignore @@ this#call loc call
          | _ -> ignore @@ this#expression (loc, expr)
        in
        (loc, expr)

      method member_expression_refinement loc expr refis =
        let open Flow_ast.Expression in
        let open Flow_ast.Expression.Member in
        let optional =
          match expr with
          | OptionalMember { Ast.Expression.OptionalMember.optional; _ } -> optional
          | _ -> false
        in
        match expr with
        | OptionalMember OptionalMember.{ member = { Member._object; property; _ }; _ }
        | Member Member.{ _object; property; _ } ->
          ignore @@ this#optional_chain _object;
          let propname =
            match property with
            | PropertyIdentifier (_, { Flow_ast.Identifier.name; _ })
            | PropertyExpression (_, Literal { Flow_ast.Literal.value = Ast.Literal.String name; _ })
            | PropertyExpression
                (_, Literal { Flow_ast.Literal.value = Ast.Literal.Number _; raw = name; _ }) ->
              Some name
            | _ -> None
          in
          (match RefinementKey.of_expression _object with
          | None -> ignore @@ this#member_property property
          | Some refinement_key_obj ->
            if optional then
              this#add_single_refinement refinement_key_obj (L.LSet.singleton loc, NotR MaybeR);
            ignore @@ this#member_property property;
            let refis =
              Base.Option.value_map
                ~f:(fun propname ->
                  this#extend_refinement
                    refinement_key_obj
                    (L.LSet.singleton loc, PropExistsR { propname; loc })
                    refis)
                ~default:refis
                propname
            in

            this#commit_refinement refis)
        | _ ->
          failwith "member_expression_refinement can only be called on OptionalMember or Member"

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
          (* Add a PropExists refinement to the object and a
           * Truthy heap refinement to the access *)
          let refis =
            match RefinementKey.of_expression (loc, expr) with
            | None -> LookupMap.empty
            | Some key -> this#start_refinement key (L.LSet.singleton loc, TruthyR)
          in
          ignore @@ this#record_member_read expression;
          this#member_expression_refinement loc expr refis;
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

      method! logical _loc (expr : (ALoc.t, ALoc.t) Flow_ast.Expression.Logical.t) =
        let open Flow_ast.Expression.Logical in
        let { operator; left = (loc, _) as left; right; comments = _ } = expr in
        this#push_refinement_scope empty_refinements;
        (* THe LHS is unconditionally evaluated, so we don't run-to-completion and catch the
         * error here *)
        (match operator with
        | Flow_ast.Expression.Logical.Or
        | Flow_ast.Expression.Logical.And ->
          ignore (this#expression_refinement left)
        | Flow_ast.Expression.Logical.NullishCoalesce ->
          ignore (this#expression left);
          this#add_refinement_to_expr left (L.LSet.singleton loc, NotR MaybeR));
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
          this#pop_refinement_scope_without_unrefining ();
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

      method! expression expr =
        match expr with
        | (_, Flow_ast.Expression.Call _)
        | (_, Flow_ast.Expression.OptionalCall _)
        | (_, Flow_ast.Expression.Member _)
        | (_, Flow_ast.Expression.OptionalMember _) ->
          this#push_refinement_scope empty_refinements;
          let res = this#optional_chain expr in
          this#pop_refinement_scope ();
          res
        | _ -> super#expression expr

      method! private hoist_annotations f =
        let visiting_hoisted_type = env_state.visiting_hoisted_type in
        env_state <- { env_state with visiting_hoisted_type = true };
        f ();
        env_state <- { env_state with visiting_hoisted_type }

      method jsx_function_call loc =
        match (Context.react_runtime cx, env_state.jsx_base_name, Context.jsx cx) with
        | (Options.ReactRuntimeClassic, Some name, _) -> this#any_identifier loc name
        | (Options.ReactRuntimeClassic, None, Options.Jsx_pragma (_, ast)) ->
          ignore @@ this#expression ast
        | _ -> ()

      method! jsx_element loc expr =
        this#jsx_function_call loc;
        super#jsx_element loc expr

      method! jsx_fragment loc expr =
        this#jsx_function_call loc;
        super#jsx_fragment loc expr
    end

  (* The EnvBuilder does not traverse dead code, but statement.ml does. Dead code
   * is an error in Flow, so type checking after that point is not very meaningful.
   * In order to support statement.ml's queries, we must ensure that the value map we
   * send to it has the dead code reads filled in. An alternative approach to this visitor
   * would be to assume that if the entry does not exist in the map then it is unreachable,
   * but that assumes that the EnvBuilder is 100% correct. This approach lets us discriminate
   * between real dead code and issues with the EnvBuilder, which seems far better than
   * the alternative *)
  class dead_code_marker cx env_values =
    object (this)
      inherit
        Scope_builder.scope_builder
          ~flowmin_compatibility:false
          ~enable_enums:(Context.enable_enums cx)
          ~with_types:true as super

      val mutable values = env_values

      method values = values

      method any_identifier loc name =
        values <-
          L.LMap.update
            loc
            (function
              | None ->
                Some
                  {
                    Env_api.def_loc = None;
                    write_locs = [Env_api.Unreachable loc];
                    val_kind = None;
                    name = Some name;
                    id = None;
                  }
              | x -> x)
            values

      method! binding_type_identifier ident = super#identifier ident

      method! identifier (ident : (ALoc.t, ALoc.t) Ast.Identifier.t) =
        let (loc, { Flow_ast.Identifier.name; _ }) = ident in
        this#any_identifier loc name;
        super#identifier ident

      method private jsx_function_call loc =
        match Context.react_runtime cx with
        | Options.ReactRuntimeClassic -> this#any_identifier loc "React"
        | _ -> ()

      method! jsx_element loc expr =
        this#jsx_function_call loc;
        super#jsx_element loc expr

      method! jsx_fragment loc expr =
        this#jsx_function_call loc;
        super#jsx_fragment loc expr

      method! pattern_identifier ?kind e =
        ignore kind;
        e
    end

  let program_with_scope cx program =
    let open Hoister in
    let (loc, _) = program in
    let jsx_ast =
      match Context.jsx cx with
      | Options.Jsx_react -> None
      | Options.Jsx_pragma (_, ast) -> Some ast
    in
    let enable_enums = Context.enable_enums cx in
    let (_ssa_completion_state, ((scopes, ssa_values, _) as prepass)) =
      Ssa_builder.program_with_scope_and_jsx_pragma
        ~flowmin_compatibility:false
        ~enable_enums
        ~jsx_ast
        program
    in
    let providers = Provider_api.find_providers program in
    let env_walk = new name_resolver cx prepass providers in
    let bindings =
      let hoist = new hoister ~flowmin_compatibility:false ~enable_enums ~with_types:true in
      hoist#eval hoist#program program
    in
    let completion_state =
      env_walk#run_to_completion (fun () ->
          ignore @@ env_walk#with_bindings loc bindings env_walk#program program
      )
    in
    (* Fill in dead code reads *)
    let dead_code_marker = new dead_code_marker cx env_walk#values in
    let _ = dead_code_marker#program program in
    ( completion_state,
      {
        Env_api.scopes;
        ssa_values;
        env_values = dead_code_marker#values;
        env_entries = env_walk#write_entries;
        providers;
        refinement_of_id = env_walk#refinement_of_id;
      }
    )

  let program cx program =
    let (_, { Env_api.env_values; refinement_of_id; _ }) = program_with_scope cx program in
    (env_values, refinement_of_id)
end

module DummyFlow (Context : C) = struct
  type cx = Context.t

  let add_output _ ?trace _ = ignore trace
end

module Make_Test_With_Cx (Context : C) =
  Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc) (Context) (DummyFlow (Context))
module Make_of_flow = Make (Scope_api.With_ALoc) (Ssa_api.With_ALoc) (Env_api.With_ALoc)
