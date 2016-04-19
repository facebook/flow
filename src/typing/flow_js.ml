(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* This module describes the subtyping algorithm that forms the core of
   typechecking. The algorithm (in its basic form) is described in Francois
   Pottier's thesis. The main data structures maintained by the algorithm are:
   (1) for every type variable, which type variables form its lower and upper
   bounds (i.e., flow in and out of the type variable); and (2) for every type
   variable, which concrete types form its lower and upper bounds. Every new
   subtyping constraint added to the system is deconstructed into its subparts,
   until basic flows between type variables and other type variables or concrete
   types remain; these flows are then viewed as links in a chain, bringing
   together further concrete types and type variables to participate in
   subtyping. This process continues till a fixpoint is reached---which itself
   is guaranteed to exist, and is usually reached in very few steps. *)

open Utils_js
open Reason_js
open Constraint_js
open Type
open Flow_error

(* The following functions are used as constructors for function types and
   object types, which unfortunately have many fields, not all of which are
   meaningful in all contexts. This part of the design should be revisited:
   perhaps the data types can be refactored to make them more specialized. *)

(* Methods may use a dummy statics object type to carry properties. We do not
   want to encourage this pattern, but we also don't want to block uses of this
   pattern. Thus, we compromise by not tracking the property types. *)
let dummy_static reason =
  AnyFunT (prefix_reason "statics of " reason)

let dummy_prototype =
  MixedT (reason_of_string "empty prototype object", Mixed_everything)

let dummy_this =
  MixedT (reason_of_string "global object", Mixed_everything)

let mk_methodtype this tins ?params_names tout = {
  this_t = this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = 0;
  changeset = Changeset.empty
}

let mk_methodtype2 this tins ?params_names tout j = {
  this_t = this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = j;
  changeset = Changeset.empty
}

let mk_functiontype tins ?params_names tout = {
  this_t = dummy_this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = 0;
  changeset = Changeset.empty
}

let mk_functiontype2 tins ?params_names tout j = {
  this_t = dummy_this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  closure_t = j;
  changeset = Changeset.empty
}

(* An object type has two flags, sealed and exact. A sealed object type cannot
   be extended. An exact object type accurately describes objects without
   "forgeting" any properties: so to extend an object type with optional
   properties, the object type must be exact. Thus, as an invariant, "not exact"
   logically implies "sealed" (and by contrapositive, "not sealed" implies
   "exact"; in other words, exact and sealed cannot both be false).

   Types of object literals are exact, but can be sealed or unsealed. Object
   type annotations are sealed but not exact. *)

let default_flags = {
  sealed = UnsealedInFile None;
  exact = true;
  frozen = false;
}

let mk_objecttype ?(flags=default_flags) dict map proto = {
  flags;
  dict_t = dict;
  props_tmap = map;
  proto_t = proto
}

(**************************************************************)

(* tvars *)

let mk_tvar cx reason =
  let tvar = mk_id () in
  let graph = Context.graph cx in
  Context.add_tvar cx tvar (Constraint_js.new_unresolved_root ());
  (if Context.is_verbose cx then prerr_endlinef
    "TVAR %d (%d): %s" tvar (IMap.cardinal graph)
    (string_of_reason reason));
  OpenT (reason, tvar)

let mk_tvar_where cx reason f =
  let tvar = mk_tvar cx reason in
  f tvar;
  tvar

(* This function is used in lieu of mk_tvar_where or mk_tvar when the reason
   must be marked internal. This has the effect of not forcing annotations where
   this type variable appears. See `assume_ground` and `assert_ground`. *)
let mk_tvar_derivable_where cx reason f =
  let reason = derivable_reason reason in
  mk_tvar_where cx reason f

(* Find the constraints of a type variable in the graph.

   Recall that type variables are either roots or goto nodes. (See
   constraint_js.ml for details.) If the type variable is a root, the
   constraints are stored with the type variable. Otherwise, the type variable
   is a goto node, and it points to another type variable: a linked list of such
   type variables must be traversed until a root is reached. *)
let rec find_graph cx id =
  let _, constraints = find_constraints cx id in
  constraints

and find_constraints cx id =
  let root_id, root = find_root cx id in
  root_id, root.constraints

(* Find the root of a type variable, potentially traversing a chain of type
   variables, while short-circuiting all the type variables in the chain to the
   root during traversal to speed up future traversals. *)
and find_root cx id =
  match IMap.get id (Context.graph cx) with
  | Some (Goto next_id) ->
      let root_id, root = find_root cx next_id in
      if root_id != next_id then replace_node cx id (Goto root_id) else ();
      root_id, root

  | Some (Root root) ->
      id, root

  | None ->
      let msg = spf "find_root: tvar %d not found in file %s" id
        (string_of_filename (Context.file cx))
      in
      failwith msg

(* Replace the node associated with a type variable in the graph. *)
and replace_node cx id node = Context.set_tvar cx id node

(* Check that id1 is not linked to id2. *)
let not_linked (id1, _bounds1) (_id2, bounds2) =
  (* It suffices to check that id1 is not already in the lower bounds of
     id2. Equivalently, we could check that id2 is not already in the upper
     bounds of id1. *)
  not (IMap.mem id1 bounds2.lowertvars)

(**********)
(* frames *)
(**********)

(* note: this is here instead of Env_js because of circular deps:
  Env_js is downstream of Flow_js due general utility funcs such as
  Flow_js.mk_tvar and builtins services. If the flow algorithm can
  be split away from these, then Env_js can be moved upstream and
  this code can be merged into it. *)

(* background:
   - each scope has an id. scope ids are unique, mod cloning
   for path-dependent analysis.
   - an environment is a scope list
   - each context holds a map of environment snapshots, keyed
   by their topmost scope ids
   - every function type contains a frame id, which maps to
   the environment in which it was defined; as well as a
   changeset containing its reads/writes/refinements on
   closed-over variables

   Given frame ids for calling function and called function and
   the changeset of the called function, here we retrieve the
   environment snapshots for the two functions, find the prefix
   of scopes they share, and havoc the variables in the called
   function's write set which live in those scopes.
 *)
let havoc_call_env = Scope.(

  let overlapped_call_scopes func_env call_env =
    let rec loop = function
      | func_scope :: func_scopes, call_scope :: call_scopes
          when func_scope.id = call_scope.id ->
        call_scope :: loop (func_scopes, call_scopes)
      | _ -> []
    in
    loop (List.rev func_env, List.rev call_env)
  in

  let havoc_entry cx scope ((_, name, _) as entry_ref) =
    (if Context.is_verbose cx then
      prerr_endlinef "%d havoc_entry %s %s" (Unix.getpid ())
        (Changeset.string_of_entry_ref entry_ref)
        (Debug_js.string_of_scope cx scope)
      );
    match get_entry name scope with
    | Some _ ->
      havoc_entry name scope;
      Changeset.(if is_active () then change_var entry_ref)
    | None ->
      (* global scopes may lack entries, if function closes over
         path-refined global vars (artifact of deferred lookup) *)
      if is_global scope then ()
      else assert_false (spf "missing entry %S in scope %d: { %s }"
        name scope.id (String.concat ", "
          (SMap.fold (fun n _ acc -> n :: acc) scope.entries [])))
  in

  let havoc_refi cx scope ((_, key, _) as refi_ref) =
    (if Context.is_verbose cx then
      prerr_endlinef "%d havoc_refi %s" (Unix.getpid ())
        (Changeset.string_of_refi_ref refi_ref));
    match get_refi key scope with
    | Some _ ->
      havoc_refi key scope;
      Changeset.(if is_active () then change_refi refi_ref)
    | None ->
      (* global scopes may lack entries, if function closes over
         path-refined global vars (artifact of deferred lookup) *)
      if is_global scope then ()
      else assert_false (spf "missing refi %S in scope %d: { %s }"
        (Key.string_of_key key) scope.id
        (String.concat ", " (KeyMap.fold (
          fun k _ acc -> (Key.string_of_key k) :: acc) scope.refis [])))
  in

  fun cx func_frame call_frame changeset ->
    if func_frame = 0 || call_frame = 0 || Changeset.is_empty changeset
    then ()
    else
      let func_env = IMap.find_unsafe func_frame (Context.envs cx) in
      let call_env = IMap.find_unsafe call_frame (Context.envs cx) in
      overlapped_call_scopes func_env call_env |>
        List.iter (fun ({ id; _ } as scope) ->
          Changeset.include_scopes [id] changeset |>
            Changeset.iter_writes
              (havoc_entry cx scope)
              (havoc_refi cx scope)
      )
)

(********************************************************************)

(* Since type maps use the built-in compare function to compare types,
   we need to be careful to keep the shape of types within the boundaries
   of that function. In particular, comparison behaves in unexpected ways
   for references. To get around these issues, we denote references with
   indices in types, and maintain side tables of those indices to the
   denoted references. *)

let mk_propmap cx pmap = Context.make_property_map cx pmap

let find_props cx id = Context.find_props cx id

let has_prop cx id x =
  find_props cx id |> SMap.mem x

let read_prop cx id x =
  find_props cx id |> SMap.find_unsafe x

(* suitable replacement for has_prop; read_prop *)
let read_prop_opt cx id x =
  find_props cx id |> SMap.get x

let read_and_delete_prop cx id x =
  find_props cx id |> (fun pmap ->
    let t = SMap.find_unsafe x pmap in
    let pmap = SMap.remove x pmap in
    Context.add_property_map cx id pmap;
    t
  )

let write_prop cx id x t =
  let pmap = find_props cx id in
  let pmap = SMap.add x t pmap in
  Context.add_property_map cx id pmap

let iter_props cx id f =
  find_props cx id
  |> SMap.iter f

let iter_real_props cx id f =
  find_props cx id
  |> SMap.filter (fun x _ -> not (is_internal_name x))
  |> SMap.iter f

(* visit an optional evaluated type at an evaluation id *)
let visit_eval_id cx id f =
  match IMap.get id (Context.evaluated cx) with
  | None -> ()
  | Some t -> f t

(***************)
(* strict mode *)
(***************)

(* For any constraints, return a list of def types that form either the lower
   bounds of the solution, or a singleton containing the solution itself. *)
let types_of constraints =
  match constraints with
  | Unresolved { lower; _ } -> TypeMap.keys lower
  | Resolved t -> [t]

(* Def types that describe the solution of a type variable. *)
let possible_types cx id = types_of (find_graph cx id)

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let rec list_map2 f ts1 ts2 = match (ts1,ts2) with
  | ([],_) | (_,[]) -> []
  | (t1::ts1,t2::ts2) -> (f (t1,t2)):: (list_map2 f ts1 ts2)

let create_union ts =
  UnionT (reason_of_string "union", UnionRep.make ts)

let rec merge_type cx = function
  | (NumT _, (NumT _ as t))
  | (StrT _, (StrT _ as t))
  | (BoolT _, (BoolT _ as t))
  | (NullT _, (NullT _ as t))
  | (VoidT _, (VoidT _ as t))
  | (TaintT _, ((TaintT _) as t))
      -> t

  | (AnyT _, t) | (t, AnyT _) -> t

  | (EmptyT _, t) | (t, EmptyT _) -> t
  | (_, (MixedT _ as t)) | ((MixedT _ as t), _) -> t

  | (NullT _, (MaybeT _ as t)) | ((MaybeT _ as t), NullT _)
  | (VoidT _, (MaybeT _ as t)) | ((MaybeT _ as t), VoidT _) ->
      t

  | (FunT (_,_,_,ft1), FunT (_,_,_,ft2)) ->
      let tins =
        try
          List.map2 (fun t1 t2 -> merge_type cx (t1,t2))
            ft1.params_tlist ft2.params_tlist
        with _ ->
          [AnyT.t] (* TODO *)
      in
      let tout = merge_type cx (ft1.return_t, ft2.return_t) in
      (* TODO: How to merge parameter names? *)
      let reason = reason_of_string "function" in
      FunT (
        reason,
        dummy_static reason,
        dummy_prototype,
        mk_functiontype tins tout
      )

  | (ObjT (_,ot1), ObjT (_,ot2)) ->
      (* TODO: How to merge indexer names? *)
      let dict = match ot1.dict_t, ot2.dict_t with
        | None, None -> None
        | Some dict, None | None, Some dict -> Some dict
        | Some dict1, Some dict2 ->
            Some {
              dict_name = None;
              key = merge_type cx (dict1.key, dict2.key);
              value = merge_type cx (dict1.value, dict2.value);
            }
      in
      let pmap =
        let map1 = find_props cx ot1.props_tmap in
        let map2 = find_props cx ot2.props_tmap in
        let map =
          SMap.merge
            (fun _ t1_opt t2_opt -> match (t1_opt,t2_opt) with
              | (None,None) -> None
              | (Some t, None) | (None, Some t) -> Some t
              | (Some t1, Some t2) -> Some (merge_type cx (t1, t2))
            ) map1 map2 in
        mk_propmap cx map
      in
      let proto = AnyT.t in
      ObjT (
        reason_of_string "object",
        mk_objecttype dict pmap proto
      )

  | (ArrT (_,t1,ts1), ArrT (_,t2,ts2)) ->
      ArrT (
        reason_of_string "array",
        merge_type cx (t1, t2),
        list_map2 (merge_type cx) ts1 ts2
      )

  | (MaybeT t1, MaybeT t2) ->
      MaybeT (merge_type cx (t1, t2))

  | (MaybeT t1, t2)
  | (t1, MaybeT t2) ->
      MaybeT (merge_type cx (t1, t2))

  | UnionT (_, rep1), UnionT (_, rep2) ->
      let ts1, ts2 = UnionRep.members rep1, UnionRep.members rep2 in
      create_union (List.rev_append ts1 ts2)

  | (UnionT (_, rep), t)
  | (t, UnionT (_, rep)) ->
      create_union (t :: UnionRep.members rep)

  (* TODO: do we need to do anything special for merging Null with Void,
     Optional with other types, etc.? *)

  | (t1, t2) ->
      create_union [t1; t2]

and resolve_type cx = function
  | OpenT (_, id) ->
      let ts = possible_types cx id in
      (* The list of types returned by possible_types is often empty, and the
         most common reason is that we don't have enough type coverage to
         resolve id. Thus, we take the unit of merging to be `any`. (Something
         similar happens when summarizing exports in ContextOptimizer.)

         In the future, we might report errors in some cases where
         possible_types returns an empty list: e.g., when we detect unreachable
         code, or even we don't have enough type coverage. Irrespective of these
         changes, the above decision would continue to make sense: as errors
         become stricter, type resolution should become even more lenient to
         improve failure tolerance.  *)
      List.fold_left (fun u t ->
        merge_type cx (t, u)
      ) AnyT.t ts
  | t -> t

(* Check whether any of the def types describing a solution for a type variable
   satisfy a predicate. *)
let check_types cx id f =
  let types = possible_types cx id in
  List.exists f types

(** The following functions do "shallow" walks over types, respectively from
    requires and from exports, in order to report missing annotations. There are
    some opportunities for future work:

    - Rewrite these functions using a type visitor class.

    - Consider using gc to crawl the graph further down from requires, and
    maybe also up from exports. Preliminary experiments along those lines
    suggest that a general walk doesn't always give expected results. As an
    example in one direction, the signature of a class is reachable from a
    `require`d superclass, but the corresponding constraint simply checks for
    consistency of overrides, and should not relax reporting missing annotations
    in the signature. As an example in the other direction, an exported function
    may have an open `this` type that we cannot expect to be annotated.
**)

(* To avoid complaining about "missing" annotations where external types are
   used in the exported type, we mark requires and their uses as types. *)

(* TODO: All said and done, this strategy to avoid complaining about missing
   annotations that depend on requires is a hack intended to achieve the ideal
   of being able to "look up" annotations in required modules, when they're
   already provided. The latter should be possible if we switch reporting
   missing annotations from early (during the "infer" phase) to late (during
   the "merge" phase). *)

let rec assume_ground cx ids = function
  | UseT OpenT(_,id) ->
    assume_ground_id cx ids id

  (** The subset of operations to crawl. The type variables denoting the
      results of these operations would be ignored by the is_required check in
     `assert_ground`.

     These are intended to be exactly the operations that might be involved
     when extracting (parts of) requires/imports. As such, they need to be
     kept in sync as module system conventions evolve. *)

  | ReposLowerT (_, use_t) ->
    assume_ground cx ids use_t

  | ImportModuleNsT (_, t)
  | CJSRequireT (_, t)
  | ImportTypeT (_, _, t)
  | ImportTypeofT (_, _, t)

  (** Other common operations that might happen immediately after extracting
      (parts of) requires/imports. *)

  | GetPropT (_, _, t)
  | CallT (_, { return_t = t; _ })
  | MethodT (_, _, { return_t = t; _ })
  | ConstructorT (_, _, t) ->
    assume_ground cx ids (UseT t)

  | _ -> ()

and assume_ground_id cx ids id =
  if not (ISet.mem id !ids) then (
    ids := !ids |> ISet.add id;
    let constraints = find_graph cx id in
    match constraints with
    | Unresolved { upper; uppertvars; _ } ->
      upper |> UseTypeMap.iter (fun t _ ->
        assume_ground cx ids t
      );
      uppertvars |> IMap.iter (fun id _ ->
        assume_ground_id cx ids id
      )
    | Resolved _ ->
      ()
  )

(**************)
(* builtins *)
(**************)

(* Every context has a local reference to builtins (along with local references
   to other modules that are discovered during type checking, such as modules
   required by it, the module it provides, and so on). *)
let mk_builtins cx =
  let builtins = mk_tvar cx (builtin_reason "module") in
  Context.add_module cx Files_js.lib_module builtins

(* Local references to modules can be looked up. *)
let lookup_module cx m = Context.find_module cx m

(* The builtins reference is accessed just like references to other modules. *)
let builtins cx =
  lookup_module cx Files_js.lib_module

(* new contexts are prepared here, so we can install shared tvars *)
let fresh_context metadata file module_name =
  let cx = Context.make metadata file module_name in
  (* add types for pervasive builtins *)
  mk_builtins cx;
  cx

(***********************)
(* instantiation utils *)
(***********************)

module ImplicitTypeArgument = struct
  (* helpers *)
  let add_typeparam_prefix s = spf "type parameter%s" s
  let has_typeparam_prefix s =
    (String.length s) >= 14 && (String.sub s 0 14) = "type parameter"

  (* Make a type argument for a given type parameter, given a reason. Note that
     not all type arguments are tvars; the following function is used only when
     polymorphic types need to be implicitly instantiated, because there was no
     explicit instantiation (via a type application), or when we want to cache a
     unique instantiation and unify it with other explicit instantiations. *)
  let mk_targ cx (typeparam, reason_op) =
    let prefix_desc = add_typeparam_prefix (spf " `%s` of " typeparam.name) in
    mk_tvar cx (prefix_reason prefix_desc reason_op)

  (* Abstract a type argument that is created by implicit instantiation
     above. Sometimes, these type arguments are involved in type expansion loops,
     so we abstract them to detect such loops. *)
  let abstract_targ tvar =
    let reason, _ = open_tvar tvar in
    let desc = desc_of_reason reason in
    if has_typeparam_prefix desc
    then Some (OpenT (reason_of_string desc, 0))
    else None
end

(* We maintain a stack of entries representing type applications processed
   during calls to flow, for the purpose of terminating unbounded expansion of
   type applications. Intuitively, we may have a potential infinite loop when
   processing a type application leads to another type application with the same
   root, but expanding type arguments. The entries in a stack contain
   approximate measurements that allow us to detect such expansion.

   An entry representing a type application with root C and type args T1,...,Tn
   is of the form (C, [A1,...,An]), where each Ai is a list of the roots of type
   applications nested in Ti. We consider a stack to indicate a potential
   infinite loop when the top of the stack is (C, [A1,...,An]) and there is
   another entry (C, [B1,...,Bn]) in the stack, such that each Bi is non-empty
   and is contained in Ai. *)

module TypeAppExpansion : sig
  type entry
  val push_unless_loop : Context.t -> (Type.t * Type.t list) -> bool
  val pop : unit -> unit
  val get : unit -> entry list
  val set : entry list -> unit
end = struct
  type entry = Type.t * TypeSet.t list
  let stack = ref ([]: entry list)

  (* visitor to collect roots of type applications nested in a type *)
  class roots_collector = object
    inherit [TypeSet.t] Type_visitor.t as super

    method! type_ cx acc t = match t with
    | TypeAppT (c, _) -> super#type_ cx (TypeSet.add c acc) t
    | OpenT _ -> (match ImplicitTypeArgument.abstract_targ t with
      | None -> acc
      | Some t -> TypeSet.add t acc
      )
    | _ -> super#type_ cx acc t
  end
  let collect_roots cx = (new roots_collector)#type_ cx TypeSet.empty

  (* Util to stringify a list, given a separator string and a function that maps
     elements of the list to strings. Should probably be moved somewhere else
     for general reuse. *)
  let string_of_list list sep f =
    list |> List.map f |> String.concat sep

  (* show entries in the stack *)
  let show_entry (c, tss) =
    spf "%s<%s>" (desc_of_t c) (
      string_of_list tss "," (fun ts ->
        spf "[%s]" (string_of_list (TypeSet.elements ts) ";" desc_of_t)
      ))

  let _dump_stack () =
    string_of_list !stack "\n" show_entry

  (* Detect whether pushing would cause a loop. Push only if no loop is
     detected, and return whether push happened. *)

  let push_unless_loop =

    (* Say that targs are possibly expanding when, given previous targs and
       current targs, each previously non-empty targ is contained in the
       corresponding current targ. *)
    let possibly_expanding_targs prev_tss tss =
      (* The following helper carries around a bit that indicates whether
         prev_tss contains at least one non-empty set. *)
      let rec loop seen_nonempty_prev_ts = function
        | prev_ts::prev_tss, ts::tss ->
          (* if prev_ts is not a subset of ts, we have found a counterexample
             and we can bail out *)
          TypeSet.subset prev_ts ts &&
            (* otherwise, we recurse on the remaining targs, updating the bit *)
            loop (seen_nonempty_prev_ts || not (TypeSet.is_empty prev_ts))
              (prev_tss, tss)
        | [], [] ->
          (* we have found no counterexamples, so it comes down to whether we've
             seen any non-empty prev_ts *)
          seen_nonempty_prev_ts
        | [], _ | _, [] ->
          (* something's wrong around arities, but that's not our problem, so
             bail out *)
          false
      in loop false (prev_tss, tss)

    in fun cx (c, ts) ->
      let tss = List.map (collect_roots cx) ts in
      let loop = !stack |> List.exists (fun (prev_c, prev_tss) ->
        c = prev_c && possibly_expanding_targs prev_tss tss
      ) in
      if loop then false
      else begin
        stack := (c, tss) :: !stack;
        if Context.is_verbose cx then
          prerr_endlinef "typeapp stack entry: %s" (show_entry (c, tss));
        true
      end

  let pop () = stack := List.tl !stack
  let get () = !stack
  let set _stack = stack := _stack
end

(**
  *)
module Cache = struct

  module TypePairSet : Set.S with type elt = TypeTerm.t * TypeTerm.use_t =
  Set.Make (struct
    type elt = TypeTerm.t * TypeTerm.use_t
    type t = elt
    let compare = Pervasives.compare
  end)

  (* Cache that remembers pairs of types that are passed to __flow__. *)
  module FlowConstraint = struct
    let cache = ref TypePairSet.empty

    (* attempt to read LB/UB pair from cache, add if absent *)
    let get cx (l, u) = match l, u with
      (* Don't cache constraints involving type variables, since the
         corresponding typing rules are already sufficiently robust. *)
      | OpenT _, _ | _, UseT OpenT _ -> false
      | _ ->
        if TypePairSet.mem (l, u) !cache then begin
          if Context.is_verbose cx
          then prerr_endlinef "[%d] FlowConstraint cache hit on (%s, %s)"
            (Unix.getpid ()) (string_of_ctor l) (string_of_use_ctor u);
          true
        end else begin
          cache := TypePairSet.add (l, u) !cache;
          false
        end
  end

  (* Cache that limits instantiation of polymorphic definitions. Intuitively,
     for each operation on a polymorphic definition, we remember the type
     arguments we use to specialize the type parameters. An operation is
     identified by its reason: we don't use the entire operation for caching
     since it may contain the very type variables we are trying to limit the
     creation of with the cache. In other words, the cache would be useless if
     we considered those type variables as part of the identity of the
     operation. *)
  module PolyInstantiation = struct
    let cache = Hashtbl.create 0

    let find cx (typeparam, reason_op) =
      try
        Hashtbl.find cache (typeparam.reason, reason_op)
      with _ ->
        let t = ImplicitTypeArgument.mk_targ cx (typeparam, reason_op) in
        Hashtbl.add cache (typeparam.reason, reason_op) t;
        t
  end

  let clear () =
    FlowConstraint.cache := TypePairSet.empty;
    Hashtbl.clear PolyInstantiation.cache

  let stats () =
    Hashtbl.stats PolyInstantiation.cache

end

(*********************************************************************)

(********************)
(* subtype relation *)
(********************)

(* Sometimes we expect types to be def types. For example, when we see a flow
   constraint from type l to type u, we expect l to be a def type. As another
   example, when we see a unification constraint between t1 and t2, we expect
   both t1 and t2 to be def types. *)

(* Recursion limiter. We proxy recursion depth with trace depth,
   which is either equal or pretty close.
   When check is called with a trace whose depth exceeds a constant
   limit, we throw a LimitExceeded exception.
 *)
module RecursionCheck : sig
  exception LimitExceeded of Trace.t
  val check: Trace.t -> unit

end = struct
  exception LimitExceeded of Trace.t
  let limit = 10000

  (* check trace depth as a proxy for recursion depth
     and throw when limit is exceeded *)
  let check trace =
    if Trace.trace_depth trace >= limit
    then raise (LimitExceeded trace)
end

(* Sometimes we don't expect to see type parameters, e.g. when they should have
   been substituted away. *)
let not_expect_bound t = match t with
  | BoundT _ -> assert_false (spf "Did not expect %s" (string_of_ctor t))
  | _ -> ()

let not_expect_use_bound t =
  lift_to_use not_expect_bound t

(********************** start of slab **********************************)

(** NOTE: Do not call this function directly. Instead, call the wrapper
    functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
    this module, and the function `flow` outside this module. **)
let rec __flow cx ((l: Type.t), (u: Type.use_t)) trace =
  begin match Context.verbose cx with
  | Some num_indent ->
    let indent = String.make ((Trace.trace_depth trace - 1) * num_indent) ' ' in
    let pid = Unix.getpid () in
    prerr_endlinef
      "\n%s[%d] %s (%s) ~>\n%s[%d] %s (%s)"
      indent pid
      (dump_reason (reason_of_t l)) (string_of_ctor l)
      indent pid
      (dump_reason (reason_of_use_t u)) (string_of_use_ctor u)
  | None -> ()
  end;

  if not (ground_subtype (l, u) || Cache.FlowConstraint.get cx (l, u)) then (
    (* limit recursion depth *)
    RecursionCheck.check trace;

    (* Expect that l is a def type. On the other hand, u may be a use type or a
       def type: the latter typically when we have annotations. *)

    (* Type parameters should always be substituted out, and as such they should
       never appear "exposed" in flows. (They can still appear bound inside
       polymorphic definitions.) *)
    not_expect_bound l; not_expect_use_bound u;

    match (l,u) with

    (******************)
    (* process X ~> Y *)
    (******************)

    | (OpenT(_, tvar1), UseT OpenT(_, tvar2)) ->
      let id1, constraints1 = find_constraints cx tvar1 in
      let id2, constraints2 = find_constraints cx tvar2 in

      (match constraints1, constraints2 with
      | Unresolved bounds1, Unresolved bounds2 ->
          if not_linked (id1, bounds1) (id2, bounds2) then (
            add_upper_edges cx trace (id1, bounds1) (id2, bounds2);
            add_lower_edges cx trace (id1, bounds1) (id2, bounds2);
            flows_across cx trace bounds1.lower bounds2.upper;
          );

      | Unresolved bounds1, Resolved t2 ->
          edges_and_flows_to_t cx trace (id1, bounds1) (UseT t2)

      | Resolved t1, Unresolved bounds2 ->
          edges_and_flows_from_t cx trace t1 (id2, bounds2)

      | Resolved t1, Resolved t2 ->
          rec_flow_t cx trace (t1, t2)
      );

    (******************)
    (* process Y ~> U *)
    (******************)

    | (OpenT(_, tvar), t2) ->
      let id1, constraints1 = find_constraints cx tvar in
      (match constraints1 with
      | Unresolved bounds1 ->
          edges_and_flows_to_t cx trace (id1, bounds1) t2

      | Resolved t1 ->
          rec_flow cx trace (t1, t2)
      );

    (******************)
    (* process L ~> X *)
    (******************)

    | (t1, UseT OpenT(_, tvar)) ->
      let id2, constraints2 = find_constraints cx tvar in
      (match constraints2 with
      | Unresolved bounds2 ->
          edges_and_flows_from_t cx trace t1 (id2, bounds2)

      | Resolved t2 ->
          rec_flow_t cx trace (t1, t2)
      );

    (******************)
    (* concretization *)
    (******************)

    (** pairs emitted by in-progress concretize_parts *)
    | t, ConcretizeLowerT (l, todo_ts, done_ts, u) ->
      concretize_lower_parts cx trace l u (t :: done_ts) todo_ts

    (** pairs emitted by in-progress concretize_parts *)
    | t, ConcretizeUpperT (l, todo_ts, done_ts, u) ->
      concretize_upper_parts cx trace l u (t :: done_ts) todo_ts

    (*****************)
    (* destructuring *)
    (*****************)

    | EvalT (t, DestructuringT (reason, s), i), _ ->
      rec_flow cx trace (eval_selector cx reason t s i, u)

    | _, UseT EvalT (t, DestructuringT (reason, s), i) ->
      rec_flow_t cx trace (l, eval_selector cx reason t s i)

    (*************)
    (* Debugging *)
    (*************)

    | (_, DebugPrintT (reason)) ->
      let msg = (spf "!!! DebugPrintT: %s" (Debug_js.jstr_of_t cx l)) in
      add_error cx (mk_info reason [msg]);

    (************)
    (* tainting *)
    (************)

    | (TaintT _, UseT TaintT _) ->
      ()

    | (TaintT _, u) when taint_op u ->
      begin match result_of_taint_op u with
      | Some u -> rec_flow_t cx trace (l, u)
      | None -> ()
      end

    (*************************)
    (* repositioning, part 1 *)
    (*************************)

    (* Repositions a concrete upper bound using the reason stored in the lower
       bound. This can be used to store a reason as it flows through a tvar. *)
    | (ReposUpperT (reason, l), u) ->
      rec_flow cx trace (l, reposition_use cx reason u)

    (***************)
    (* annotations *)
    (***************)

    (* The sink component of an annotation constrains values flowing
       into the annotated site. *)

    | _, UseT AnnotT (sink_t, _) ->
      let reason = reason_of_t sink_t in
      rec_flow_t cx trace (ReposUpperT (reason, l), sink_t)

    (* The source component of an annotation flows out of the annotated
       site to downstream uses. *)

    | AnnotT (_, source_t), u ->
      let reason = reason_of_t source_t in
      rec_flow cx trace (reposition ~trace cx reason source_t, u)

    (****************************************************************)
    (* BecomeT unifies a tvar with an incoming concrete lower bound *)
    (****************************************************************)
    | _, BecomeT (reason, t) ->
      rec_unify cx trace (reposition ~trace cx reason l) t

    (***********************)
    (* guarded unification *)
    (***********************)

    (** Utility to unify a pair of types based on a trigger. Triggers are
        commonly type variables that are set up to record when certain
        operations have been processed: until then, they remain latent. For
        example, we can respond to events such as "a property is added," "a
        refinement succeeds," etc., by setting up unification constraints that
        are processed only when the corresponding triggers fire. *)

    | (_, UnifyT(t,t_other)) ->
      rec_unify cx trace t t_other

    (*********************************************************************)
    (* `import type` creates a properly-parameterized type alias for the *)
    (* remote type -- but only for particular, valid remote types.       *)
    (*********************************************************************)
    | (ClassT(inst), ImportTypeT(reason, _, t)) ->
      rec_flow_t cx trace (TypeT(reason, inst), t)

    (* fix this-abstracted class when used as a type *)
    | (ThisClassT i, ImportTypeT(reason, _, _)) ->
      rec_flow cx trace
        (fix_this_class cx trace reason i, u)

    | (PolyT(typeparams, ClassT(inst)), ImportTypeT(reason, _, t)) ->
      rec_flow_t cx trace (PolyT(typeparams, TypeT(reason, inst)), t)

    (* delay fixing a polymorphic this-abstracted class until it is specialized,
       by transforming the instance type to a type application *)
    | (PolyT(typeparams, ThisClassT _), ImportTypeT _) ->
      let targs = List.map (fun tp -> BoundT tp) typeparams in
      rec_flow cx trace (PolyT(typeparams, ClassT (TypeAppT(l, targs))), u)

    | (FunT(_, _, prototype, _), ImportTypeT(reason, _, t)) ->
      rec_flow_t cx trace (TypeT(reason, prototype), t)

    | (PolyT(typeparams, FunT(_, _, prototype, _)), ImportTypeT(reason, _, t)) ->
      rec_flow_t cx trace (PolyT(typeparams, TypeT(reason, prototype)), t)

    | (TypeT _, ImportTypeT(_, _, t))
    | (PolyT(_, TypeT _), ImportTypeT(_, _, t))
      -> rec_flow_t cx trace (l, t)

    (**
     * TODO: Delete this once the legacy export-type hacks have been eliminated
     *       in favor of the newer, first class export-type feature.
     *
     *       TODO(jeffmo) Task(6860853)
     *)
    | (ObjT _, ImportTypeT(_, "default", t)) ->
      rec_flow_t cx trace (l, t)

    | (_, ImportTypeT(reason, export_name, _)) ->
      let msg_export =
        if export_name = "default" then "default" else spf "`%s`" export_name
      in
      add_error cx (mk_info reason [
        spf
          ("The %s export is a value, but not a type. `import type` only " ^^
           "works on type exports like type aliases, interfaces, and " ^^
           "classes. If you inteded to import the type *of* a value, please " ^^
           "use `import typeof` instead.")
          msg_export
      ])

    (************************************************************************)
    (* `import typeof` creates a properly-parameterized type alias for the  *)
    (* "typeof" the remote export.                                          *)
    (************************************************************************)
    | (PolyT(typeparams, ((ClassT _ | FunT _) as lower_t)), ImportTypeofT(reason, _, t)) ->
      let typeof_t = mk_typeof_annotation cx ~trace lower_t in
      rec_flow_t cx trace (PolyT(typeparams, TypeT(reason, typeof_t)), t)

    | ((TypeT _ | PolyT(_, TypeT _)), ImportTypeofT(reason, export_name, _)) ->
      let msg_export =
        if export_name = "default" then "default" else spf "`%s`" export_name
      in
      add_error cx (mk_info reason [
        spf
          ("The %s export is a type, but not a value. `import typeof` only " ^^
           "works on value exports like classes, vars, lets, etc. If you " ^^
           "intended to import a type alias or interface, please use " ^^
           "`import type` instead.")
          msg_export
      ])

    | (_, ImportTypeofT(reason, _, t)) ->
      let typeof_t = mk_typeof_annotation cx ~trace l in
      rec_flow_t cx trace (TypeT(reason, typeof_t), t)

    (**************************************************************************)
    (* Module exports                                                         *)
    (*                                                                        *)
    (* Flow supports both CommonJS and standard ES modules as well as some    *)
    (* interoperability semantics for communicating between the two module    *)
    (* systems in both directions.                                            *)
    (*                                                                        *)
    (* In order to support both systems at once, Flow abstracts the notion of *)
    (* module exports by storing a type map for each of the exports of a      *)
    (* given module, and for each module there is a ModuleT that maintains    *)
    (* this type map. The exported types are then considered immutable once   *)
    (* the module has finished inference.                                     *)
    (*                                                                        *)
    (* When a type is set for the CommonJS exports value, we store it         *)
    (* separately from the normal named exports tmap that ES exports are      *)
    (* stored within. This allows us to distinguish CommonJS modules from ES  *)
    (* modules when interpreting an ES import statement -- which is important *)
    (* because ES ModuleNamespace objects built from CommonJS exports are a   *)
    (* little bit magic.                                                      *)
    (*                                                                        *)
    (* For example: If a CommonJS module exports an object, we will extract   *)
    (* each of the properties of that object and consider them as "named"     *)
    (* exports for the purposes of an import statement elsewhere:             *)
    (*                                                                        *)
    (*   // CJSModule.js                                                      *)
    (*   module.exports = {                                                   *)
    (*     someNumber: 42                                                     *)
    (*   };                                                                   *)
    (*                                                                        *)
    (*   // ESModule.js                                                       *)
    (*   import {someNumber} from "CJSModule";                                *)
    (*   var a: number = someNumber;                                          *)
    (*                                                                        *)
    (* We also map CommonJS export values to the "default" export for         *)
    (* purposes of import statements in other modules:                        *)
    (*                                                                        *)
    (*   // CJSModule.js                                                      *)
    (*   module.exports = {                                                   *)
    (*     someNumber: 42                                                     *)
    (*   };                                                                   *)
    (*                                                                        *)
    (*   // ESModule.js                                                       *)
    (*   import CJSDefaultExport from "CJSModule";                            *)
    (*   var a: number = CJSDefaultExport.someNumber;                         *)
    (*                                                                        *)
    (* Note that the ModuleT type is not intended to be surfaced to any       *)
    (* userland-visible constructs. Instead it's meant as an internal         *)
    (* construct that is only *mapped* to/from userland constructs (such as a *)
    (* CommonJS exports object or an ES ModuleNamespace object).              *)
    (**************************************************************************)

    (* ES exports *)
    | (ModuleT(_, exports), ExportNamedT(_, tmap, t_out)) ->
      SMap.iter (write_prop cx exports.exports_tmap) tmap;
      rec_flow_t cx trace (l, t_out)

    (* export * from *)
    | (ModuleT(_, source_exports), ExportStarFromT(reason, target_module_t, t_out)) ->
      let source_tmap = find_props cx source_exports.exports_tmap in
      rec_flow cx trace (
        target_module_t,
        ExportNamedT(reason, source_tmap, t_out)
      )

    (**
     * ObjT CommonJS export values have their properties turned into named
     * exports
     *)
    | (ObjT(_, {props_tmap; proto_t; _;}),
       CJSExtractNamedExportsT(reason, module_t, t_out)) ->

      (* Copy props from the prototype *)
      let module_t = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (proto_t, CJSExtractNamedExportsT(reason, module_t, t))
      ) in

      (* Copy own props *)
      rec_flow cx trace (module_t, ExportNamedT(
        reason,
        find_props cx props_tmap,
        t_out
      ))

    (**
     * InstanceT CommonJS export values have their properties turned into named
     * exports
     *)
    | (InstanceT(_, _, _, {fields_tmap; methods_tmap; _;}),
       CJSExtractNamedExportsT(reason, module_t, t_out)) ->

      (* Copy fields *)
      let module_t = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (module_t, ExportNamedT(
          reason,
          find_props cx fields_tmap,
          t
        ))
      ) in

      (* Copy methods *)
      rec_flow cx trace (module_t, ExportNamedT(
        reason,
        find_props cx methods_tmap,
        t_out
      ))

    (**
     * All other CommonJS export value types do not get merged into the named
     * exports tmap in any special way.
     *)
    | (_, CJSExtractNamedExportsT(_, module_t, t_out)) ->
      rec_flow_t cx trace (module_t, t_out)

    (**************************************************************************)
    (* Module imports                                                         *)
    (*                                                                        *)
    (* The process of importing from a module consists of reading from the    *)
    (* foreign ModuleT type and generating a user-visible construct from it.  *)
    (*                                                                        *)
    (* For CommonJS imports (AKA 'require()'), if the foreign module is an ES *)
    (* module we generate an object whose properties correspond to each of    *)
    (* the named exports of the foreign module. If the foreign module is also *)
    (* a CommonJS module, use the type of the foreign CommonJS exports value  *)
    (* directly.                                                              *)
    (*                                                                        *)
    (* For ES imports (AKA `import` statements), simply generate a model of   *)
    (* an ES ModuleNamespace object from the individual named exports of the  *)
    (* foreign module. This object can then be passed up to "userland"        *)
    (* directly (via `import * as`) or it can be used to extract individual   *)
    (* exports from the foreign module (via `import {}` and `import X from`). *)
    (**************************************************************************)

    (* require('SomeModule') *)
    | (ModuleT(_, exports), CJSRequireT(reason, t)) ->
      let cjs_exports = (
        match exports.cjs_export with
        | Some t ->
          (* reposition the export to point at the require(), like the object
             we create below for non-CommonJS exports *)
          reposition ~trace cx reason t
        | None ->
          let proto = MixedT (reason, Mixed_everything) in
          let props_smap = find_props cx exports.exports_tmap in
          mk_object_with_map_proto cx reason
            ~sealed:true ~frozen:true props_smap proto
      ) in
      rec_flow_t cx trace (cjs_exports, t)

    (* import * as X from 'SomeModule'; *)
    | (ModuleT(_, exports), ImportModuleNsT(reason, t)) ->
      let exports_tmap = find_props cx exports.exports_tmap in
      let ns_obj_tmap = (
        match exports.cjs_export with
        | Some(t) -> SMap.add "default" t exports_tmap
        | None -> exports_tmap
      ) in
      let proto = MixedT (reason, Mixed_everything) in
      let ns_obj = mk_object_with_map_proto cx reason
        ~sealed:true ~frozen:true ns_obj_tmap proto
      in
      rec_flow_t cx trace (ns_obj, t)

    (* import [type] X from 'SomeModule'; *)
    | (ModuleT(_, exports), ImportDefaultT(reason, import_kind, (local_name, module_name), t)) ->
      let export_t = match exports.cjs_export with
        | Some t -> t
        | None ->
            let exports_tmap = find_props cx exports.exports_tmap in
            match SMap.get "default" exports_tmap with
              | Some t -> t
              | None ->
                let msg = "This module has no default export." in
                (**
                 * A common error while using `import` syntax is to forget or
                 * misunderstand the difference between `import foo from ...`
                 * and `import {foo} from ...`. The former means to import the
                 * default export to a local var called "foo", and the latter
                 * means to import a named export called "foo" to a local var
                 * called "foo".
                 *
                 * To help guide users here, if we notice that the module being
                 * imported from has no default export (but it does have a named
                 * export that fuzzy-matches the local name specified), we offer
                 * that up as a possible "did you mean?" suggestion.
                 *)
                let known_exports = SMap.keys exports_tmap in
                let suggestions = typo_suggestions known_exports local_name in
                let msg = if List.length suggestions = 0 then msg else msg ^ (
                  spf " Did you mean `import {%s} from \"%s\"`?"
                    (List.hd suggestions)
                    module_name
                ) in
                add_error cx (mk_info reason [msg]);
                AnyT.t
      in

      let import_t = (
        match import_kind with
        | ImportType ->
          mk_tvar_where cx reason (fun tvar ->
            rec_flow cx trace (export_t, ImportTypeT(reason, "default", tvar))
          )
        | ImportTypeof ->
          mk_tvar_where cx reason (fun tvar ->
            rec_flow cx trace (export_t, ImportTypeofT(reason, "default", tvar))
          )
        | ImportValue ->
          rec_flow cx trace (export_t, AssertImportIsValueT(reason, "default"));
          export_t
      ) in
      rec_flow_t cx trace (import_t, t)

    (* import {X} from 'SomeModule'; *)
    | (ModuleT(_, exports), ImportNamedT(reason, import_kind, export_name, t)) ->
        (**
         * When importing from a CommonJS module, we shadow any potential named
         * exports called "default" with a pointer to the raw `module.exports`
         * object
         *)
        let exports_tmap = (
          let exports_tmap = find_props cx exports.exports_tmap in
          match exports.cjs_export with
          | Some t -> SMap.add "default" t exports_tmap
          | None -> exports_tmap
        ) in
        let import_t = (
          match (import_kind, SMap.get export_name exports_tmap) with
          | (ImportType, Some t) ->
            mk_tvar_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeT(reason, export_name, tvar))
            )
          | (ImportTypeof, Some t) ->
            mk_tvar_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeofT(reason, export_name, tvar))
            )
          | (ImportValue, Some t) ->
            rec_flow cx trace (t, AssertImportIsValueT(reason, export_name));
            t
          | (_, None) ->
            let msg =
              spf "This module has no named export called `%s`." export_name
            in

            let known_exports = SMap.keys exports_tmap in
            let suggestions = typo_suggestions known_exports export_name in
            let msg = if List.length suggestions = 0 then msg else msg ^ (
              spf " Did you mean `%s`?" (List.hd suggestions)
            ) in
            add_error cx (mk_info reason [msg]);
            AnyT.why reason
        ) in
        rec_flow_t cx trace (import_t, t)

    | ((PolyT (_, TypeT _) | TypeT _), AssertImportIsValueT(reason, name)) ->
      add_error cx (mk_info reason [
        spf
          ("`%s` is a type, but not a value. In order to import it, please " ^^
           "use `import type`.")
          name
      ])

    | (_, AssertImportIsValueT(_, _)) -> ()

    (********************************************)
    (* summary types forget literal information *)
    (********************************************)

    | (StrT (_, (Literal _ | Truthy)), SummarizeT (reason, t)) ->
      rec_unify cx trace (StrT.why reason) t

    | (NumT (_, (Literal _ | Truthy)), SummarizeT (reason, t)) ->
      rec_unify cx trace (NumT.why reason) t

    | (_, SummarizeT (_, t)) ->
      rec_unify cx trace l t

    (*******************************)
    (* common implicit convertions *)
    (*******************************)

    | (_, UseT NumT _) when numeric l -> ()

    | (_, UseT AnyObjT _) when object_like l -> ()
    | (AnyObjT _, UseT u) when object_like u -> ()
    | (AnyObjT _, UseT AnyObjT _) -> ()

    | (_, UseT (AnyFunT _)) when function_like l -> ()

    | AnyFunT reason, GetPropT (_, (_, x), _)
    | AnyFunT reason, SetPropT (_, (_, x), _)
    | AnyFunT reason, LookupT (_, _, _, x, _)
    | AnyFunT reason, MethodT (_, (_, x), _)
    | AnyFunT reason, HasPropT (_, _, Literal x) when is_function_prototype x ->
      rec_flow cx trace (FunProtoT reason, u)
    | (AnyFunT _, UseT u) when function_like u -> ()
    | (AnyFunT _, UseT u) when object_like u -> ()
    | AnyFunT _, UseT (TypeT _ | AnyFunT _ | AnyObjT _) -> ()

    (***************)
    (* maybe types *)
    (***************)

    (** The type maybe(T) is the same as null | undefined | UseT *)

    | ((NullT _ | VoidT _), UseT MaybeT _) -> ()

    | (MaybeT(t), _) ->
      let reason = reason_of_t t in
      rec_flow cx trace (NullT.why reason, u);
      rec_flow cx trace (VoidT.why reason, u);
      rec_flow cx trace (t, u)

    (******************)
    (* optional types *)
    (******************)

    (** The type optional(T) is the same as undefined | UseT *)

    | (VoidT _, UseT OptionalT _) -> ()

    | (OptionalT(t), _) ->
      let reason = reason_of_t t in
      rec_flow cx trace (VoidT.why reason, u);
      rec_flow cx trace (t, u)

    (*****************)
    (* logical types *)
    (*****************)

    (* !x when x is of unknown truthiness *)
    | BoolT (_, None), NotT (reason, tout)
    | StrT (_, AnyLiteral), NotT (reason, tout)
    | NumT (_, AnyLiteral), NotT (reason, tout) ->
      rec_flow_t cx trace (BoolT.at (loc_of_reason reason), tout)

    (* !x when x is falsy *)
    | BoolT (_, Some false), NotT (reason, tout)
    | SingletonBoolT (_, false), NotT (reason, tout)
    | StrT (_, Literal ""), NotT (reason, tout)
    | SingletonStrT (_, ""), NotT (reason, tout)
    | NumT (_, Literal (0., _)), NotT (reason, tout)
    | SingletonNumT (_, (0., _)), NotT (reason, tout)
    | NullT _, NotT (reason, tout)
    | VoidT _, NotT (reason, tout) ->
      let reason = replace_reason "boolean value `true`" reason in
      rec_flow_t cx trace (BoolT (reason, Some true), tout)

    (* !x when x is truthy *)
    | (_, NotT(reason, tout)) ->
      let reason = replace_reason "boolean value `false`" reason in
      rec_flow_t cx trace (BoolT (reason, Some false), tout)

    | (left, AndT(reason, (left_loc, right_loc), right, u)) ->
      (* a falsy && b ~> a
         a truthy && b ~> b
         a && b ~> a falsy | b *)
      let left_reason = repos_reason left_loc reason in
      let right_reason = repos_reason right_loc reason in
      let truthy_left = filter_exists left in
      (match truthy_left with
      | EmptyT _ ->
        (* falsy *)
        rec_flow cx trace
          (left, ReposLowerT (left_reason, PredicateT (NotP ExistsP, u)))
      | _ ->
        (match filter_not_exists left with
        | EmptyT _ -> (* truthy *)
          rec_flow cx trace (right, ReposLowerT (right_reason, UseT u))
        | _ ->
          rec_flow cx trace
            (left, ReposLowerT (left_reason, PredicateT (NotP ExistsP, u)));
          (match truthy_left with
          | EmptyT _ -> ()
          | _ -> rec_flow cx trace (right, ReposLowerT (right_reason, UseT u)))
        )
      )

    | (left, OrT(reason, (left_loc, right_loc), right, u)) ->
      (* a truthy || b ~> a
         a falsy || b ~> b
         a || b ~> a truthy | b *)
      let left_reason = repos_reason left_loc reason in
      let right_reason = repos_reason right_loc reason in
      let falsy_left = filter_not_exists left in
      (match falsy_left with
      | EmptyT _ ->
        (* truthy *)
        rec_flow cx trace
          (left, ReposLowerT (left_reason, PredicateT (ExistsP, u)))
      | _ ->
        (match filter_exists left with
        | EmptyT _ -> (* falsy *)
          rec_flow cx trace (right, ReposLowerT (right_reason, UseT u))
        | _ ->
          rec_flow cx trace
            (left, ReposLowerT (left_reason, PredicateT (ExistsP, u)));
          (match falsy_left with
          | EmptyT _ -> ()
          | _ -> rec_flow cx trace (right, ReposLowerT (right_reason, UseT u)))
        )
      )

    | (_, ObjTestT(_, default, u)) ->
      if object_like l
      then rec_flow_t cx trace (l, u)
      else rec_flow_t cx trace (default, u)

    (*****************************)
    (* upper and lower any types *)
    (*****************************)

    (** UpperBoundT and AnyWithUpperBoundT are very useful types that concisely
        model subtyping constraints without introducing unwanted effects: they
        can appear on both sides of a type, but only have effects in one of
        those sides. In some sense, they are liked bounded AnyT: indeed, AnyT
        has the same behavior as UpperBoundT(EmptyT) and
        AnyWithUpperBoundT(MixedT). Thus, these types can be used instead of
        AnyT when some precise typechecking is required without overconstraining
        the system. A completely static alternative would be achieved with
        bounded type variables, which Flow does not support yet. **)

    | (AnyWithLowerBoundT t, _) ->
      rec_flow cx trace (t,u)

    | (_, UseT AnyWithLowerBoundT _) ->
      ()

    | (AnyWithUpperBoundT _, _) ->
      ()

    | (_, UseT AnyWithUpperBoundT t) ->
      rec_flow_t cx trace (l, t)

    (*********************)
    (* type applications *)
    (*********************)

    (* Sometimes a polymorphic class may have a polymorphic method whose return
       type is a type application on the same polymorphic class, possibly
       expanded. See Array#map or Array#concat, e.g. It is not unusual for
       programmers to reuse variables, assigning the result of a method call on
       a variable to itself, in which case we could get into cycles of unbounded
       instantiation. We use caching to cut these cycles. Caching relies on
       reasons (see module Cache.I). This is OK since intuitively, there should
       be a unique instantiation of a polymorphic definition for any given use
       of it in the source code.

       In principle we could use caching more liberally, but we don't because
       not all use types arise from source code, and because reasons are not
       perfect. Indeed, if we tried caching for all use types, we'd lose
       precision and report spurious errors.

       Also worth noting is that we can never safely cache def types. This is
       because substitution of type parameters in def types does not affect
       their reasons, so we'd trivially lose precision. *)

    | (ThisTypeAppT(c,this,ts), _) ->
      let reason = reason_of_use_t u in
      let tc = specialize_class cx trace reason c ts in
      let c = instantiate_this_class cx trace reason tc this in
      rec_flow cx trace (mk_instance cx reason ~for_type:false c, u)

    | (_, UseT ThisTypeAppT(c,this,ts)) ->
      let reason = reason_of_t l in
      let tc = specialize_class cx trace reason c ts in
      let c = instantiate_this_class cx trace reason tc this in
      rec_flow_t cx trace (l, mk_instance cx reason ~for_type:false c)

    | (TypeAppT(c,ts), MethodT _) ->
        let reason_op = reason_of_use_t u in
        let t = mk_typeapp_instance cx reason_op ~cache:true c ts in
        rec_flow cx trace (t, u)

    | (TypeAppT(c,ts), _) ->
        if TypeAppExpansion.push_unless_loop cx (c, ts) then (
          let reason = reason_of_use_t u in
          let t = mk_typeapp_instance cx reason c ts in
          rec_flow cx trace (t, u);
          TypeAppExpansion.pop ()
        )

    | (_, UseT TypeAppT(c,ts)) ->
        if TypeAppExpansion.push_unless_loop cx (c, ts) then (
          let reason = reason_of_t l in
          let t = mk_typeapp_instance cx reason c ts in
          rec_flow_t cx trace (l, t);
          TypeAppExpansion.pop ()
        )

    (***********************)
    (* Singletons and keys *)
    (***********************)

    (** Finite keysets over arbitrary objects can be represented by KeysT. While
        it is possible to also represent singleton string types using KeysT (by
        taking the keyset of an object with a single property whose key is that
        string and whose value is ignored), we can model them more directly
        using SingletonStrT. Specifically, SingletonStrT models a type
        annotation that looks like a string literal, which describes a singleton
        set containing that string literal. Going further, other uses of KeysT
        where the underlying object is created solely for the purpose of
        describing a keyset can be modeled using unions of singleton strings.

        One may also legitimately wonder why SingletonStrT(_, key) cannot be
        always replaced by StrT(_, Some key). The reason is that types of the
        latter form (string literal types) are inferred to be the type of string
        literals appearing as values, and we don't want to prematurely narrow
        down the type of the location where such values may appear, since that
        would preclude other strings to be stored in that location. Thus, by
        necessity we allow all string types to flow to StrT (whereas only
        exactly matching string literal types may flow to SingletonStrT).  **)

    (*********************************)
    (* string singleton upper bounds *)
    (*********************************)

    | (StrT (_, Literal x), UseT SingletonStrT (_, key)) ->
        if x <> key then
          let msg = spf "Expected string literal `%s`, got `%s` instead" key x in
          flow_err cx trace msg l u

    | (StrT (_, (Truthy | AnyLiteral)), UseT SingletonStrT (_, key)) ->
      flow_err cx trace (spf "Expected string literal `%s`" key) l u

    (*********************************)
    (* number singleton upper bounds *)
    (*********************************)

    (** Similar to SingletonStrT, SingletonNumT models a type annotation that
        looks like a single number literal. This contrasts with
        `NumT(_, Some num)`, which starts out representing `num` but allows any
        number, whereas `SingletonNumT` accepts only exactly that value. **)
    | (NumT (_, Literal (x, _)), UseT SingletonNumT (_, (y, _))) ->
        (* this equality check is ok for now because we don't do arithmetic *)
        if x <> y then
          let msg = spf "Expected number literal `%.16g`, got `%.16g` instead" y x in
          flow_err cx trace msg l u

    | (NumT (_, (Truthy | AnyLiteral)), UseT SingletonNumT (_, (y, _))) ->
      flow_err cx trace (spf "Expected number literal `%.16g`" y) l u

    (**********************************)
    (* boolean singleton upper bounds *)
    (**********************************)

    (** Similar to SingletonStrT, SingletonBoolT models a type annotation that
        looks like a specific boolean literal (either true or false, but not
        both). This contrasts with `BoolT(_, Some b)`, which starts out
        representing `b` but allows any boolean, whereas `SingletonBoolT`
        accepts only exactly that value. **)
    | (BoolT (_, Some x), UseT SingletonBoolT (_, y)) ->
        if x <> y then
          let msg = spf "Expected boolean literal `%b`, got `%b` instead" y x in
          flow_err cx trace msg l u

    | (BoolT (_, None), UseT SingletonBoolT (_, y)) ->
      flow_err cx trace (spf "Expected boolean literal `%b`" y) l u

    (*****************************************************)
    (* keys (NOTE: currently we only support string keys *)
    (*****************************************************)

    | (StrT (reason_s, literal), UseT KeysT (reason_op, o)) ->
      let reason_next = match literal with
      | Literal x -> replace_reason (spf "property `%s`" x) reason_s
      | _ -> replace_reason "some string with unknown value" reason_s in
      (* check that o has key x *)
      rec_flow cx trace (o, ReposLowerT(reason_op, HasOwnPropT(reason_next, literal)))


    | KeysT (reason1, o1), _ ->
      (* flow all keys of o1 to u *)
      rec_flow cx trace (o1, GetKeysT (reason1,
        match u with
        | UseT t -> t
        | _ -> tvar_with_constraint cx u))

    (* helpers *)

    | (ObjT (reason_o, { props_tmap = mapr; dict_t; _; }), HasOwnPropT (reason_op, x)) ->
      (match x, dict_t with
      (* If we have a literal string and that property exists *)
      | Literal x, _ when has_prop cx mapr x -> ()
      (* If we have a dictionary, try that next *)
      | _, Some { key; _ } -> rec_flow_t cx trace (StrT (reason_op, x), key)
      | _ ->
          flow_err_prop_not_found cx trace (reason_op, reason_o))

    | ObjT (reason_o, { props_tmap = mapr; proto_t = proto; dict_t; _ }),
      HasPropT (reason_op, strict, x) ->
      (match x, dict_t with
      (* If we have a literal string and that property exists *)
      | Literal x, _ when has_prop cx mapr x -> ()
      (* If we have a dictionary, try that next *)
      | _, Some { key; _ } -> rec_flow_t cx trace (StrT (reason_op, x), key)
      | _ ->
        let strict = match strict with
        | Some r -> Some r
        | None -> Some reason_o
        in
        rec_flow cx trace (proto, HasPropT (reason_op, strict, x))
      )

    | (InstanceT (reason_o, _, _, instance), HasOwnPropT(reason_op, Literal x)) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      (match SMap.get x fields with
      | Some _ -> ()
      | None -> flow_err_prop_not_found cx trace (reason_op, reason_o)
      )
    | (InstanceT (reason_o, _, _, _), HasOwnPropT(reason_op, _)) ->
        flow_err_reasons cx trace "Expected string literal" (reason_op, reason_o)

    | InstanceT (reason_o, _, super, instance),
      HasPropT (reason_op, strict, Literal x) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      (match SMap.get x fields with
      | Some _ -> ()
      | None ->
          let strict = match strict with
          | Some r -> Some r
          | None -> Some reason_o
          in
          rec_flow cx trace (super, HasPropT (reason_op, strict, Literal x))
      )
    | (InstanceT (reason_o, _, _, _), HasPropT(reason_op, _, _)) ->
        flow_err_reasons cx trace "Expected string literal" (reason_op, reason_o)

    (* AnyObjT has every prop *)
    | AnyObjT _, HasOwnPropT _
    | AnyObjT _, HasPropT _ -> ()

    | ObjT (reason, { flags; props_tmap = mapr; _ }), GetKeysT (_, keys) ->
      begin match flags.sealed with
      | Sealed ->
        (* flow each key of l to keys *)
        iter_props cx mapr (fun x _ ->
          let t = StrT (reason, Literal x) in
          rec_flow_t cx trace (t, keys)
        );
      | _ ->
        rec_flow_t cx trace (StrT.why reason, keys)
      end

    | InstanceT (reason, _, _, instance), GetKeysT (_, keys) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      fields |> SMap.iter (fun x _ ->
        let t = StrT (reason, Literal x) in
        rec_flow_t cx trace (t, keys)
      )

    | (AnyObjT reason | AnyFunT reason), GetKeysT (_, keys) ->
      rec_flow_t cx trace (StrT.why reason, keys)

    (** In general, typechecking is monotonic in the sense that more constraints
        produce more errors. However, sometimes we may want to speculatively try
        out constraints, backtracking if they produce errors (and removing the
        errors produced). This is useful to typecheck union types and
        intersection types: see below. **)

    (** NOTE: It is important that any def type that simplifies to a union or
        intersection of other def types be processed before we process unions
        and intersections: otherwise we may get spurious errors. **)

    (********************************)
    (* union and intersection types *)
    (********************************)

    (* cases where there is no loss of precision *)

    | UnionT (_, rep), _ ->
      UnionRep.members rep |> List.iter (fun t -> rec_flow cx trace (t,u))

    | _, UseT IntersectionT (_, rep) ->
      InterRep.members rep |> List.iter (fun t -> rec_flow_t cx trace (l,t))

    (* When a subtyping question involves a union appearing on the right or an
       intersection appearing on the left, the simplification rules are
       imprecise: we split the union / intersection into cases and try to prove
       that the subtyping question holds for one of the cases, but each of those
       cases may be unprovable, which might lead to spurious errors. In
       particular, obvious assertions such as (A | B) & C is a subtype of A | B
       cannot be proved if we choose to split the union first (discharging
       unprovable subgoals of (A | B) & C being a subtype of either A or B);
       dually, obvious assertions such as A & B is a subtype of (A & B) | C
       cannot be proved if we choose to simplify the intersection first
       (discharging unprovable subgoals of either A or B being a subtype of (A &
       B) | C). So instead, we try inclusion rules to handle such cases.

       An orthogonal benefit is that for large unions or intersections, checking
       inclusion is significantly faster that splitting for proving simple
       inequalities (O(n) instead of O(n^2) for n cases).  *)

    | IntersectionT (_, rep), UseT u
      when List.mem u (InterRep.members rep) ->
      ()

    (** To check that the concrete type l may flow to UnionT(_, ts), we try each
        type in ts in turn. The types in ts may be type variables, but by
        routing them through SpeculativeMatchT, we ensure that they are
        tried only when their concrete uses are available. The different
        branches of unions are assumed to be disjoint at the top level, so that
        it is always sound to pick the first type in ts that matches the type
        l. *)

    (** pairs emitted by try_union:
        UnionT has been partitioned into head_u (head member)
        and tail_u (union of tail members) *)
    | SpeculativeMatchT(_, l, tail_u), head_u ->
      begin match speculative_match cx trace l head_u with
      | None -> ()
      | Some err ->
        (* rec_flow cx trace (l, tail_u) *)
        let tail_u = match tail_u with
          | UseT UnionT (r, rep) ->
            (* record this error *)
            UseT (UnionT (r, UnionRep.record_error err rep))
          | _ -> tail_u
        in rec_flow cx trace (l, tail_u)
      end

    | _, UseT UnionT (r, rep) -> (
      match UnionRep.quick_mem l rep with
      | Some true -> ()
      | Some false ->
        let r = match UnionRep.enum_base rep with
          | None -> r
          | Some base -> replace_reason (desc_of_t base) r
        in
        rec_flow cx trace (l, UseT (EmptyT r))
      | None ->
        try_union cx trace l r rep
    )

    (* maybe and optional types are just special union types *)

    | (t1, UseT MaybeT(t2)) ->
      rec_flow_t cx trace (t1,t2)

    | (t1, UseT OptionalT(t2)) ->
      rec_flow_t cx trace (t1, t2)

    (** To check that IntersectionT(_, ts) may flow to the concrete type u, we
        try each type in ts in turn. The types in ts may be type variables, but
        by routing them through SpeculativeMatchT, we ensure that they
        are tried only when their concrete definitionss are available. Note that
        unlike unions, the different branches of intersections are usually not
        distinct at the top level (e.g., they may all be function types, or
        object types): instead, they are assumed to be disjoint in their parts,
        so we must suffiently concretize parts of u to make the disjointness
        evident when the different branches are tried; see below. *)

    (** pairs emitted by try_intersection:
        IntersectionT has been partitioned into head_l (head member)
        and tail_l (intersection of tail members) *)
    | head_l, UseT SpeculativeMatchT (_, tail_l, u) -> (
      match speculative_match cx trace head_l u with
      | None -> ()
      | Some err ->
        let tail_l = match tail_l with
          | IntersectionT (r, rep) ->
            (* record this error *)
            IntersectionT (r, InterRep.record_error err rep)
          | _ -> tail_l
        in rec_flow cx trace (tail_l, u)
    )

    (** special treatment for some operations on intersections: these
        rules fire for particular UBs whose constraints can (or must)
        be resolved against intersection LBs as a whole, instead of
        by decomposing the intersection into its parts.
      *)

    (** lookup of properties **)
    | IntersectionT (_, rep),
      LookupT (reason, strict, try_ts_on_failure, s, t) ->
      let ts = InterRep.members rep in
      assert (ts <> []);
      (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
      rec_flow cx trace
        (List.hd ts,
         LookupT (reason, strict, (List.tl ts) @ try_ts_on_failure, s, t))

    (** extends **)
    | IntersectionT (_, rep),
      UseT ExtendsT (try_ts_on_failure, l, u) ->
      let ts = InterRep.members rep in
      assert (ts <> []);
      (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
      rec_flow_t cx trace
        (List.hd ts,
         ExtendsT ((List.tl ts) @ try_ts_on_failure, l, u))

    (** consistent override of properties **)
    | IntersectionT (_, rep), SuperT _ ->
      InterRep.members rep |> List.iter (fun t -> rec_flow cx trace (t, u))

    (** object types: an intersection may satisfy an object UB without
        any particular member of the intersection doing so completely.
        Here we trap object UBs with more than one property, and
        decompose them into singletons.
        Note: should be able to do this with LookupT rather than
        slices, but that approach behaves in nonobvious ways. TODO why?
      *)
    | IntersectionT _,
      UseT ObjT (r, { flags; props_tmap; proto_t; dict_t })
      when SMap.cardinal (find_props cx props_tmap) > 1 ->
      iter_real_props cx props_tmap (fun prop_name prop_type ->
        let slice = mk_propmap cx (SMap.singleton prop_name prop_type) in
        let obj = mk_objecttype ~flags dict_t slice MixedT.t in
        rec_flow cx trace (l, UseT (ObjT (r, obj)))
      );
      rec_flow_t cx trace (l, proto_t)

    (** predicates: prevent a predicate UB from prematurely
        decomposing an intersection LB *)
    | IntersectionT _, PredicateT (pred, tout) ->
      predicate cx trace tout (l, pred)

    (** ObjAssignT copies multiple properties from its incoming LB.
        Here we simulate a merged object type by iterating over the
        entire intersection. *)
    | IntersectionT (_, rep), ObjAssignT (_, _, _, _, false) ->
      InterRep.members rep |> List.iter (fun t -> rec_flow cx trace (t, u))

    (** This duplicates the (_, ReposLowerT u) near the end of this pattern
        match but has to appear here to preempt the (IntersectionT, _) in
        between so that we reposition the entire intersection. *)
    | IntersectionT _, ReposLowerT (reason_op, u) ->
      rec_flow cx trace (reposition cx reason_op l, u)

    (** All other pairs with an intersection LB come here.
        Before processing, we ensure that both the UB target and
        the intersection LB are concretized. Each of these steps
        is a recursion, and the concretization may result in other
        rules (eg the special cases above) firing. If none do, then
        we'll wind up back here with a fully concretized pair, at
        which point we begin the speculative match process via
        try_intersection. *)

    | IntersectionT (r, rep), u ->
      let upper_parts = upper_parts_to_concretize cx u in
      if upper_parts <> []
      then concretize_upper_parts cx trace l u [] upper_parts
      else (
        let lower_parts = lower_parts_to_concretize cx l in
        if lower_parts <> []
        then concretize_lower_parts cx trace l u [] lower_parts
        else try_intersection cx trace u r rep
      )

    (* singleton lower bounds are equivalent to the corresponding
       primitive with a literal constraint. These conversions are
       low precedence to allow equality exploits above, such as
       the UnionT membership check, to fire.
       TODO we can move to a single representation for singletons -
       either SingletonFooT or (FooT <literal foo>) - if we can
       ensure that their meaning as upper bounds is unambiguous.
       Currently a SingletonFooT means the constrained type,
       but the literal in (FooT <literal>) is a no-op.
       Abstractly it should be totally possible to scrub literals
       from the latter kind of flow, but it's unclear how difficult
       it would be in practice.
     *)

    | SingletonStrT (reason, key), _ ->
      rec_flow cx trace (StrT (reason, Literal key), u)

    | SingletonNumT (reason, lit), _ ->
      rec_flow cx trace (NumT (reason, Literal lit), u)

    | SingletonBoolT (reason, b), _ ->
      rec_flow cx trace (BoolT (reason, Some b), u)

    (********************)
    (* mixin conversion *)
    (********************)

    (* A class can be viewed as a mixin by extracting its immediate properties,
       and "erasing" its static and super *)

    | ThisClassT (InstanceT (_, _, _, instance)), MixinT (r, tvar) ->
      let static = MixedT (r, Mixed_everything) in
      let super = MixedT (r, Mixed_everything) in
      rec_flow cx trace (
        ThisClassT (InstanceT (r, static, super, instance)),
        UseT tvar
      )

    | PolyT (xs, ThisClassT (InstanceT (_, _, _, instance))), MixinT (r, tvar) ->
      let static = MixedT (r, Mixed_everything) in
      let super = MixedT (r, Mixed_everything) in
      rec_flow cx trace (
        PolyT (xs, ThisClassT (InstanceT (r, static, super, instance))),
        UseT tvar
      )

    (* TODO: it is conceivable that other things (e.g. functions) could also be
       viewed as mixins (e.g. by extracting properties in their prototypes), but
       such enhancements are left as future work. *)

    (***************************************)
    (* generic function may be specialized *)
    (***************************************)

    (* Instantiate a polymorphic definition using the supplied type
       arguments. Use the instantiation cache if directed to do so by the
       operation. (SpecializeT operations are created when processing TypeAppT
       types, so the decision to cache or not originates there.) *)

    (* NOTE: we consider empty targs specialization for polymorphic definitions
       to be the same as implicit specialization, which is handled by a
       fall-through case below. *)
    | (PolyT (ids,t), SpecializeT(reason,cache,ts,tvar)) when ts <> [] ->
      let t_ = instantiate_poly_with_targs cx trace reason ~cache (ids,t) ts in
      rec_flow_t cx trace (t_, tvar)

    | PolyT (tps, _), VarianceCheckT(_, ts, polarity) ->
      variance_check cx polarity (tps, ts)

    (* empty targs specialization of non-polymorphic classes is a no-op *)
    | (ClassT _ | ThisClassT _), SpecializeT(_,_,[],tvar) ->
      rec_flow_t cx trace (l, tvar)

    (* this-specialize a this-abstracted class by substituting This *)
    | ThisClassT i, ThisSpecializeT(_, this, tvar) ->
      let i = subst cx (SMap.singleton "this" this) i in
      rec_flow_t cx trace (ClassT i, tvar)

    (* this-specialization of non-this-abstracted classes is a no-op *)
    | ClassT i, ThisSpecializeT(_, _this, tvar) ->
      (* TODO: check that this is a subtype of i? *)
      rec_flow_t cx trace (ClassT i, tvar)

    (* PolyT doesn't have a position since it takes on the position of the upper
       bound, so it doesn't need to be repositioned. This case is necessary to
       prevent the wildcard (PolyT, _) below from firing too early. *)
    | (PolyT _, ReposLowerT (_, u)) ->
      rec_flow cx trace (l, u)

    (* get this out of the way too, as above, except that repositioning does
       make sense *)
    | (ThisClassT _, ReposLowerT (reason_op, u)) ->
      rec_flow cx trace (reposition cx reason_op l, u)

    (* When do we consider a polymorphic type <X:U> UseT to be a subtype of another
       polymorphic type <X:U'> T'? This is the subject of a long line of
       research. A rule that works (Cardelli/Wegner) is: force U = U', and prove
       that UseT is a subtype of T' for any X:U'. A more general rule that proves
       that U' is a subtype of U instead of forcing U = U' is known to cause
       undecidable subtyping (Pierce): the counterexamples are fairly
       pathological, but can be reliably constructed by exploiting the "switch"
       of bounds from U' to U (and back, with sufficient trickery), in ways that
       are difficult to detect statically.

       However, these results are somewhat tricky to interpret in Flow, since we
       are not proving stuff inductively: instead we are co-inductively assuming
       what we want to prove, and checking consistency.

       Separately, none of these rules capture the logical interpretation of the
       original subtyping question (interpreting subtyping as implication, and
       polymorphism as universal quantification). What we really want to show is
       that, for all X:U', there is some X:U such that UseT is a subtype of T'. But
       we already deal with statements of this form when checking polymorphic
       definitions! In particular, statements such as "there is some X:U...")
       correspond to "create a type variable with that constraint and ...", and
       statements such as "show that for all X:U" correspond to "show that for
       both X = bottom and X = U, ...".

       Thus, all we need to do when checking that any type flows to a
       polymorphic type is to follow the same principles used when checking that
       a polymorphic definition has a polymorphic type. This has the pleasant
       side effect that the type we're checking does not itself need to be a
       polymorphic type at all! For example, we can let a non-generic method be
       overridden with a generic method, as long as the non-generic signature
       can be derived as a specialization of the generic signature. *)
    | (_, UseT PolyT (ids, t)) ->
        generate_tests cx (reason_of_t l) ids (fun map_ ->
          rec_flow_t cx trace (l, subst cx map_ t)
        )

    (* TODO: ideally we'd do the same when lower bounds flow to a
       this-abstracted class, but fixing the class is easier; might need to
       revisit *)
    | (_, UseT ThisClassT i) ->
      let r = reason_of_t l in
      rec_flow_t cx trace (l, fix_this_class cx trace r i)

    | (PolyT (ids,t), _) ->
      (* Some observed cases of u are listed below. These look legit, as common
         uses of polymorphic classes and functions. However, they may not cover
         all legit uses.

      (* class-like *)
      | TypeT _
      | ConstructorT _
      | GetPropT _
      | SetPropT _
      | MethodT _

      (* function-like *)
      | CallT _
      | FunT _

      (* general uses *)
      | PredicateT _

      *)

      let weak = match u with
        (* Implicitly instantiating polymorphic types in annotations leads to
           confusing errors. In particular, when multiple instantiations of a
           polymorphic type flow to such an annotation, the instantiations are
           unified via the implicit instantiation at the annotation. This
           situation is made worse by the fact that the annotation site is often
           not included as part of the ensuing errors due to such unification.

           Instead, we fall back to using `any` as type arguments when they are
           missing. This conforms to the general intuition that all
           instantiations of a polymorphic type Foo are subtypes of Foo. When
           falling back to `any` is inadequate, we force the programmer to be
           explicit about what the type arguments of a polymorphic type are in
           an annotation. Overall, this scheme reduces false positives as well
           as improves blaming when errors do arise.

           It is always possible to recover implicit instantiation by explicitly
           using `*`. *)
        | UseT TypeT _ -> true
        | _ -> false in
      let reason = reason_of_use_t u in
      let t_ = instantiate_poly ~weak cx trace reason (ids,t) in
      rec_flow cx trace (t_, u)

    (* when a this-abstracted class flows to upper bounds, fix the class *)
    | (ThisClassT i, _) ->
      let r = reason_of_use_t u in
      rec_flow cx trace (fix_this_class cx trace r i, u)

    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    | FunT (_, _, _,
        { this_t = o1; params_tlist = tins1; return_t = t1; _ }),
      UseT FunT (reason, _, _,
        { this_t = o2; params_tlist = tins2; return_t = t2; _ })
      ->
      rec_flow_t cx trace (o2, o1);
      multiflow cx trace reason (tins2, tins1);
      rec_flow_t cx trace (t1, t2);

    | FunT (reason_fundef, _, _,
        { this_t = o1; params_tlist = tins1; return_t = t1;
          closure_t = func_scope_id; changeset; _ }),
      CallT (reason_callsite,
        { this_t = o2; params_tlist = tins2; return_t = t2;
          closure_t = call_scope_id; _})
      ->
      Ops.push reason_callsite;
      rec_flow_t cx trace (o2, o1);
      multiflow cx trace reason_callsite (tins2, tins1);
      (* relocate the function's return type at the call site TODO remove? *)
      let t1 = reposition ~trace cx reason_callsite t1 in
      rec_flow_t cx trace (t1, t2);

      (if Context.is_verbose cx then
        prerr_endlinef "%d havoc_call_env fundef %s callsite %s"
          (Unix.getpid ())
          (string_of_reason reason_fundef)
          (string_of_reason reason_callsite));
      havoc_call_env cx func_scope_id call_scope_id changeset;

      Ops.pop ()

    | (AnyFunT reason_fundef | AnyT reason_fundef),
      CallT (reason_op, { this_t; params_tlist; return_t; _}) ->
      let any = AnyT.why reason_fundef in
      rec_flow_t cx trace (any, this_t);
      multiflow cx trace reason_op (params_tlist, [RestT any]);
      rec_flow_t cx trace (AnyT.why reason_op, return_t);


    (* Special handlers for builtin functions *)

    | CustomFunT (_, ObjectAssign),
      CallT (reason_op, { params_tlist = dest_t::ts; return_t; _ }) ->
      let t = chain_objects cx ~trace reason_op dest_t ts in
      rec_flow_t cx trace (t, return_t)

    | CustomFunT (_, ObjectGetPrototypeOf),
      CallT (reason_op, { params_tlist = obj::_; return_t; _ }) ->
      rec_flow cx trace (
        obj,
        GetPropT(reason_op, (reason_op, "__proto__"), return_t)
      );

    | CustomFunT (_, PromiseAll),
      CallT (reason_op, { params_tlist; return_t; _ }) ->
      let param = match params_tlist with
      | [] ->
        (* e.g., Promise.all()
         * The VoidT type leads to a use type error below, as TupleMapT
         * expects an ArrT as its lower bound. *)
        VoidT (replace_reason "undefined (too few arguments)" reason_op)
      | t::_ ->
        (* e.g., Promise.all(arr, ...)
         * Like all functions, extra arguments are ignored. If this isn't
         * an ArrT, we will realize a use type error in the TupleMapT flow. *)
        t
      in
      (* Build an array holding the unwrapped values of the array parameter
       * and wrap back up into a promise to return. *)
      let t = mk_tvar_where cx reason_op (fun t ->
        let funt = get_builtin cx "$await" reason_op in
        rec_flow cx trace (param, TupleMapT (reason_op, funt, t))
      ) in
      let promise = get_builtin_typeapp cx reason_op "Promise" [t] in
      rec_flow_t cx trace (promise, return_t)


    (* Facebookisms are special Facebook-specific functions that are not
       expressable with our current type syntax, so we've hacked in special
       handling. Terminate with extreme prejudice. *)

    | CustomFunT (_, MergeInto),
      CallT (reason_op, { params_tlist = dest_t::ts; return_t; _ }) ->
      ignore (chain_objects cx ~trace reason_op dest_t ts);
      rec_flow_t cx trace (VoidT.why reason_op, return_t)

    | CustomFunT (_, MergeDeepInto),
      CallT (reason_op, { return_t; _ }) ->
      (* TODO *)
      rec_flow_t cx trace (VoidT.why reason_op, return_t)

    | CustomFunT (_, Merge),
      CallT (reason_op, { params_tlist; return_t; _ }) ->
      rec_flow_t cx trace (spread_objects cx reason_op params_tlist, return_t)

    | CustomFunT (_, Mixin),
      CallT (reason_op, { params_tlist; return_t; _ }) ->
      let t = ClassT (spread_objects cx reason_op params_tlist) in
      rec_flow_t cx trace (t, return_t)

    | CustomFunT (reason, _), _ when function_like_op u ->
      rec_flow cx trace (AnyFunT reason, u)


    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    (* ObjT -> ObjT *)

    | ObjT (lreason, {
        props_tmap = lflds;
        proto_t = lproto;
        flags = lflags;
        dict_t = ldict; _ }),
      UseT ObjT (ureason, {
        props_tmap = uflds;
        proto_t = uproto;
        dict_t = udict; _ }) ->

      (* if inflowing type is literal (thus guaranteed to be
         unaliased), propertywise subtyping is sound *)
      let ldesc = desc_of_reason lreason in
      let lit =
        ldesc = "object literal"
        || ldesc = "function"
        || ldesc = "arrow function"
        || lflags.frozen
        || string_contains_react ldesc
      in

      (* If both are dictionaries, ensure the keys and values are compatible
         with each other. *)
      (match ldict, udict with
        | Some {key = lk; value = lv; _}, Some {key = uk; value = uv; _} ->
            dictionary cx trace lk lv udict;
            dictionary cx trace uk uv ldict
        | _ -> ());

      (* Properties in u must either exist in l, or match l's indexer. *)
      iter_real_props cx uflds (fun s ut ->
        match read_prop_opt cx lflds s with
        | Some lt ->
          if lit then (
            (* prop from unaliased LB: check <:, then make exact *)
            rec_flow_t cx trace (lt, ut);
            write_prop cx lflds s ut
          ) else (
            (* prop from aliased LB must be exact *)
            rec_unify cx trace lt ut
          )
        | None ->
          (* property doesn't exist in inflowing type *)
          let ureason = replace_reason (spf "property `%s`" s) ureason in
          match ut with
          | OptionalT t when lflags.exact ->
            (* if property is marked optional or otherwise has a maybe type,
               and if inflowing type is exact (i.e., it is not an
               annotation), then we add it to the inflowing type as
               an optional property, as well as ensuring compatibility
               with dictionary constraints if present *)
            dictionary cx trace (string_key s ureason) t ldict;
            write_prop cx lflds s ut;
          | _ ->
            (* otherwise, we ensure compatibility with dictionary constraints
               if present, and look up the property in the prototype *)
            dictionary cx trace (string_key s ureason) ut ldict;
            (* if l is NOT a dictionary, then do a strict lookup *)
            let strict = if ldict = None then Some lreason else None in
            rec_flow cx trace (lproto, LookupT (ureason, strict, [], s, ut))
          (* TODO: instead, consider extending inflowing type with s:t2 when it
             is not sealed *)
      );

      (* Any properties in l but not u must match indexer *)
      iter_real_props cx lflds (fun s lt ->
        if not (has_prop cx uflds s)
        then dictionary cx trace (string_key s lreason) lt udict
      );

      rec_flow_t cx trace (l, uproto)

    (* InstanceT -> ObjT *)

    | InstanceT (lreason, _, super, {
        fields_tmap = lflds;
        methods_tmap = lmethods; _ }),
      UseT ObjT (ureason, {
        props_tmap = uflds;
        proto_t = uproto; _ }) ->

      let lflds =
        let fields_tmap = find_props cx lflds in
        let methods_tmap = find_props cx lmethods in
        SMap.union fields_tmap methods_tmap
      in

      iter_real_props cx uflds (fun s ut ->
        match SMap.get s lflds with
        | Some lt ->
          rec_unify cx trace lt ut
        | None ->
          let ureason = replace_reason (spf "property `%s`" s) ureason in
          match ut with
          | OptionalT t ->
            rec_flow cx trace (l, LookupT (ureason, None, [], s, t))
          | _ ->
            rec_flow cx trace
              (super, LookupT (ureason, Some lreason, [], s, ut))
      );

      rec_flow_t cx trace (l, uproto)

    (* For some object `x` and constructor `C`, if `x instanceof C`, then the
       object is a subtype. *)
    | ObjT (lreason, { proto_t; _ }),
      UseT InstanceT (_, _, _, { structural = false; _ }) ->
      rec_flow cx trace (reposition cx ~trace lreason proto_t, u)

    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | (ObjT (reason, _) | InstanceT (reason, _, _, _)),
      (UseT FunT (reason_op, _, _, _) | UseT AnyFunT reason_op | CallT (reason_op, _)) ->
      let tvar = mk_tvar cx (suffix_reason " used as a function" reason) in
      lookup_prop cx trace l reason_op (Some reason) "$call" tvar;
      rec_flow cx trace (tvar, u)

    (******************************)
    (* matching shapes of objects *)
    (******************************)

    (** When something of type ShapeT(o) is used, it behaves like it had type o.

        On the other hand, things that can be passed to something of type
        ShapeT(o) must be "subobjects" of o: they may have fewer properties, but
        those properties should be transferable to o.

        Because a property x with a type OptionalT(t) could be considered
        missing or having type t, we consider such a property to be transferable
        if t is a subtype of x's type in o. Otherwise, the property should be
        assignable to o.

        TODO: The type constructors ShapeT, DiffT, ObjAssignT, ObjRestT express
        related meta-operations on objects. Consolidate these meta-operations
        and ensure consistency of their semantics. **)

    | (ShapeT (o), _) ->
        rec_flow cx trace (o, u)

    | (ObjT (reason, { props_tmap = mapr; _ }), UseT ShapeT (proto)) ->
        iter_props cx mapr (fun x t ->
          let reason = prefix_reason (spf "property `%s` of " x) reason in
          let t = filter_optional cx ~trace reason t in
          rec_flow cx trace (proto, SetPropT (reason, (reason, x), t));
        )

    | (_, UseT ShapeT (o)) ->
        let reason = reason_of_t o in
        rec_flow cx trace (l, ObjAssignT(reason, o, AnyT.t, [], false))

    | (_, UseT DiffT (o1, o2)) ->
        let reason = reason_of_t l in
        let t2 = mk_tvar cx reason in
        (* prerr_endline (string_of_reason reason); *)
        rec_flow cx trace (o2, ObjRestT (reason, [], t2));
        rec_flow cx trace (t2, ObjAssignT(reason, l, o1, [], true))

    (********************************************)
    (* array types deconstruct into their parts *)
    (********************************************)

    | ArrT (r1, t1, ts1), UseT ArrT (_, t2, ts2) ->
      let lit = (desc_of_reason r1) = "array literal" in
      array_flow cx trace lit (ts1, t1, ts2, t2)

    (**************************************************)
    (* instances of classes follow declared hierarchy *)
    (**************************************************)

    | (InstanceT _, UseT (InstanceT _ as u)) ->
      rec_flow_t cx trace (l, ExtendsT([],l,u))

    | (InstanceT (_,_,super,instance),
       UseT ExtendsT(_, _, InstanceT (_,_,_,instance_super))) ->
      if instance.class_id = instance_super.class_id
      then
        flow_type_args cx trace instance instance_super
      else
        rec_flow cx trace (super, u)

    (***************************************************************)
    (* Enable structural subtyping for upperbounds like interfaces *)
    (***************************************************************)

    | (_,
       UseT InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = true;
         _;
       }))
      ->
      structural_subtype cx trace l reason_inst
        (super, fields_tmap, methods_tmap)

    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)

    | (ClassT(it), UseT TypeT(r,t)) ->
      (* a class value annotation becomes the instance type *)
      rec_flow cx trace (it, BecomeT (r, t))

    | (FunT(_, _, prototype, _), UseT TypeT(reason, t)) ->
      (* a function value annotation becomes the prototype type *)
      rec_flow cx trace (prototype, BecomeT (reason, t))

    | (TypeT(_,l), UseT TypeT(_,u)) ->
      rec_unify cx trace l u

    | (TypeT(type_reason, l), GetPropT(get_reason, (prop_reason, name), t_out)) ->
      (**
       * Reify the type, extract the prop from that, then wrap that result in a
       * TypeT again.
       *)
      let prop_t = mk_tvar_where cx get_reason (fun t ->
        rec_flow cx trace (l, GetPropT(get_reason, (prop_reason, name), t))
      ) in
      rec_flow cx trace (TypeT (type_reason, mk_typeof_annotation cx ~trace prop_t), UseT t_out)

    | (TypeT(_, l), ReifyTypeT(_, t_out)) ->
      (* Extract the type denoted by the type expression *)
      rec_flow cx trace (l, UseT t_out)

    (* non-class/function values used in annotations are errors *)
    | _, UseT TypeT _ ->
      flow_err cx trace
        "Ineligible value used in/as type annotation (did you forget 'typeof'?)"
        l u

    | (ClassT(l), UseT ClassT(u)) ->
      rec_unify cx trace l u

    | (FunT(_,static1,prototype,_), UseT ClassT(InstanceT(_,static2,_, _) as u_)) ->
      rec_unify cx trace static1 static2;
      rec_unify cx trace prototype u_

    (*********************************************************)
    (* class types derive instance types (with constructors) *)
    (*********************************************************)

    | ClassT (this),
      ConstructorT (reason_op, args, t) ->
      let reason_o = replace_reason "constructor return" (reason_of_t this) in
      Ops.push reason_op;
      (* call this.constructor(args) *)
      let ret = mk_tvar_where cx reason_o (fun t ->
        let funtype = mk_methodtype this args t in
        rec_flow cx trace (
          this,
          MethodT (reason_op, (reason_o, "constructor"), funtype)
        );
      ) in
      (* return this *)
      rec_flow cx trace (ret, ObjTestT(reason_o, this, t));
      Ops.pop ();

    (****************************************************************)
    (* function types derive objects through explicit instantiation *)
    (****************************************************************)

    | FunT (reason, _, proto, {
        this_t = this;
        params_tlist = params;
        return_t = ret;
        _ }),
      ConstructorT (reason_op, args, t) ->
      (* TODO: closure *)
      (** create new object **)
      let reason_c = replace_reason "new object" reason in
      let proto_reason = reason_of_t proto in
      let sealed = UnsealedInFile (Loc.source (loc_of_reason proto_reason)) in
      let flags = { default_flags with sealed } in
      let dict = None in
      let pmap = mk_propmap cx SMap.empty in
      let new_obj = ObjT (reason_c, mk_objecttype ~flags dict pmap proto) in
      (** call function with this = new_obj, params = args **)
      rec_flow_t cx trace (new_obj, this);
      multiflow cx trace reason_op (args, params);
      (** if ret is object-like, return ret; otherwise return new_obj **)
      let reason_o = replace_reason "constructor return" reason in
      rec_flow cx trace (ret, ObjTestT(reason_o, new_obj, t))

    | AnyFunT reason_fundef, ConstructorT (reason_op, args, t) ->
      let reason_o = replace_reason "constructor return" reason_fundef in
      multiflow cx trace reason_op (args, [RestT (AnyT.t)]);
      rec_flow_t cx trace (AnyObjT reason_o, t);

    (* Since we don't know the signature of a method on AnyFunT, assume every
       parameter is an AnyT. *)
    | (AnyFunT _, MethodT (reason_op, _, {params_tlist; _})) ->
      let any = AnyT.why reason_op in
      List.iter (fun t -> rec_flow_t cx trace (t, any)) params_tlist

    (*************************)
    (* "statics" can be read *)
    (*************************)

    | InstanceT (_, static, _, _), GetPropT (_, (_, "statics"), t) ->
      rec_flow_t cx trace (static, t)

    | MixedT (reason, _), GetPropT(_, (_, "statics"), u) ->
      (* MixedT not only serves as the instance type of the root class, but also
         as the statics of the root class. *)
      rec_flow_t cx trace (
        MixedT (prefix_reason "statics of " reason, Mixed_everything),
        u)

    (********************************************************)
    (* instances of classes may have their fields looked up *)
    (********************************************************)

    | InstanceT(_, _, super, instance),
      LookupT (_, strict, _, x, t)
      ->
      let strict = if instance.mixins then None else strict in
      let pmap = match strict, t with
        (* t = AnyWithLowerBoundT _ means that the lookup is trying to write t,
           rather than read t. Existing places that play a role here are
           set_prop and get_prop, which use AnyWithLowerBoundT and
           AnyWithUpperBoundT, respectively.  The general pattern has been used
           previously, e.g. to distinguish element writes from reads.

           strict = Some _ means that we want to throw errors when x is not
           found. Some lookups are non-strict (e.g. when we want to enforce
           consistency between properties if they exist higher up in the
           inheritance chain), and for methods, we want the consistency to be
           one-way, so we use AnyWithLowerBoundT _, and we don't want methods to
           be excluded from the lookup in that case, obviously. *)
        | Some _, AnyWithLowerBoundT _ ->
          let fields_tmap = find_props cx instance.fields_tmap in
          fields_tmap
        | _ ->
          let fields_tmap = find_props cx instance.fields_tmap in
          let methods_tmap = find_props cx instance.methods_tmap in
          SMap.union fields_tmap methods_tmap
      in
      (match SMap.get x pmap with
      | None ->
        rec_flow cx trace (super, u)
      | Some (AbstractT tx) | Some tx ->
        rec_unify cx trace t tx
      );

    (********************************)
    (* ... and their fields written *)
    (********************************)

    | InstanceT (reason_c, _, super, instance),
      SetPropT (reason_op, (reason_prop, x), tin) ->
      Ops.push reason_op;
      (* methods are immutable, so we hide them from property set operations *)
      let fields = instance.fields_tmap in
      set_prop cx trace reason_prop reason_c super x fields tin;
      Ops.pop ();

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | InstanceT (_, _, super, _), GetPropT (_, (_, "__proto__"), t) ->
      rec_flow_t cx trace (super, t)

    | InstanceT _ as instance, GetPropT (_, (_, "constructor"), t) ->
      rec_flow_t cx trace (ClassT instance, t)

    | InstanceT (reason_c, _, super, instance),
      GetPropT (reason_op, (reason_prop, x), tout) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      let strict = if instance.mixins then None else Some reason_c in
      get_prop cx trace reason_prop reason_op strict super x fields tout

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | InstanceT (reason_c, _, super, instance),
      MethodT (reason_op, (reason_prop, x), funtype)
      -> (* TODO: closure *)
      Ops.push reason_op;
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let methods = SMap.union fields_tmap methods_tmap in
      let funt = mk_tvar cx reason_op in
      let strict = if instance.mixins then None else Some reason_c in
      get_prop cx trace reason_prop reason_op strict super x methods funt;
      let callt = CallT (reason_op, funtype) in
      rec_flow cx trace (funt, callt);
      Ops.pop ();

    (** In traditional type systems, object types are not extensible.  E.g., an
        object {x: 0, y: ""} has type {x: number; y: string}. While it is
        possible to narrow the object's type to hide some of its properties (aka
        width subtyping), extending its type to model new properties is
        impossible. This is not without reason: all object types would then be
        equatable via subtyping, thereby making them unsound.

        In JavaScript, on the other hand, objects can grow dynamically, and
        doing so is a common idiom during initialization (i.e., before they
        become available for general use). Objects that typically grow
        dynamically include not only object literals, but also prototypes,
        export objects, and so on. Thus, it is important to model this idiom.

        To balance utility and soundness, Flow's object types are extensible by
        default, but become sealed as soon as they are subject to width
        subtyping. However, implementing this simple idea needs a lot of care.

        To ensure that aliases have the same underlying type, object types are
        represented indirectly as pointers to records (rather than directly as
        records). And to ensure that typing is independent of the order in which
        fragments of code are analyzed, new property types can be added on gets
        as well as sets (and due to indirection, the new property types become
        immediately available to aliases).

        Looking up properties of an object, e.g. for the purposes of copying,
        when it is not fully initialized is prone to races, and requires careful
        manual reasoning about escape to avoid surprising results.

        Prototypes cause further complications. In JavaScript, objects inherit
        properties of their prototypes, and may override those properties. (This
        is similar to subclasses inheriting and overriding methods of
        superclasses.) At the same time, prototypes are extensible just as much
        as the objects they derive are. In other words, we want to maintain the
        invariant that an object's type is a subtype of its prototype's type,
        while letting them be extensible by default. This invariant is achieved
        by constraints that unify a property's type if and when that property
        exists both on the object and its prototype.

        Here's some example code with type calculations in comments. (We use the
        symbol >=> to denote a flow between a pair of types. The direction of
        flow roughly matches the pattern 'rvalue' >=> 'lvalue'.)

        var o = {}; // o:T, UseT |-> {}
        o.x = 4; // UseT |-> {x:X}, number >=> X
        var s:string = o.x; // ERROR: number >=> string

        function F() { } // F.prototype:P, P |-> {}
        var f = new F(); // f:O, O |-> {}&P

        F.prototype.m = function() { this.y = 4; } // P |-> {m:M}, ... >=> M
        f.m(); // O |-> {y:Y}&P, number >=> Y

    **)

    (**********************************************************************)
    (* objects can be assigned, i.e., their properties can be set in bulk *)
    (**********************************************************************)

    (** When some object-like type O1 flows to ObjAssignT(_,O2,X,_,false), the
        properties of O1 are copied to O2, and O2 is linked to X to signal that
        the copying is done; the intention is that when those properties are
        read through X, they should be found (whereas this cannot be guaranteed
        when those properties are read through O2). However, there is an
        additional twist: this scheme may not work when O2 is unresolved. In
        particular, when O2 is unresolved, the constraints that copy the
        properties from O1 may race with reads of those properties through X as
        soon as O2 is resolved. To avoid this race, we make O2 flow to
        ObjAssignT(_,O1,X,_,true); when O2 is resolved, we make the switch. **)

    | (ObjT (reason_, { props_tmap = mapr; _ }),
       ObjAssignT (reason, proto, t, props_to_skip, false)) ->
      Ops.push reason;
      iter_props cx mapr (fun x t ->
        if not (List.mem x props_to_skip) then (
          (* move the reason to the call site instead of the definition, so that
             it is in the same scope as the Object.assign, so that strictness
             rules apply. *)
          let r = prefix_reason (spf "property `%s` of " x) reason_ in
          let r = repos_reason (loc_of_reason reason) r in
          let t = filter_optional cx ~trace r t in
          rec_flow cx trace (proto, SetPropT (r, (r, x), t));
        );
      );
      Ops.pop ();
      rec_flow_t cx trace (proto, t)

    | (InstanceT (_, _, _, { fields_tmap; methods_tmap; _ }),
       ObjAssignT (reason, proto, t, props_to_skip, false)) ->
      let fields_tmap = find_props cx fields_tmap in
      let methods_tmap = find_props cx methods_tmap in
      let map = SMap.union fields_tmap methods_tmap in
      map |> SMap.iter (fun x t ->
        if not (List.mem x props_to_skip) then (
          rec_flow cx trace (proto, SetPropT (reason, (reason, x), t));
        );
      );
      rec_flow_t cx trace (proto, t)

    (* AnyObjT has every prop, each one typed as `any`, so spreading it into an
       existing object destroys all of the keys, turning the result into an
       AnyObjT as well. TODO: wait for `proto` to be resolved, and then call
       `SetPropT (_, _, AnyT)` on all of its props. *)
    | AnyObjT _, ObjAssignT (reason, _, t, _, false) ->
      rec_flow_t cx trace (AnyObjT reason, t)

    | (MixedT _, ObjAssignT (_, proto, t, _, false)) ->
      rec_flow_t cx trace (proto, t)

    (* Object.assign(o, ...Array<x>) -> Object.assign(o, x) *)
    | RestT l, ObjAssignT (_, _, _, _, false) ->
      rec_flow cx trace (l, u)

    | (_, ObjAssignT(reason, o, t, xs, true)) ->
      rec_flow cx trace (o, ObjAssignT(reason, l, t, xs, false))

    (* Object.assign semantics *)
    | ((NullT _ | VoidT _), ObjAssignT _) -> ()

    (*************************)
    (* objects can be copied *)
    (*************************)

    | (ObjT (_, { props_tmap = mapr; _ }), ObjRestT (reason, xs, t)) ->
      let map = find_props cx mapr in
      let map = List.fold_left (fun map x -> SMap.remove x map) map xs in
      let proto = MixedT (reason, Mixed_everything) in
      let o = mk_object_with_map_proto cx reason map proto in
      rec_flow_t cx trace (o, t)

    | (InstanceT (_, _, super, insttype), ObjRestT (reason, xs, t)) ->
      (* Spread fields from super into an object *)
      let obj_super = mk_tvar_where cx reason (fun tvar ->
        rec_flow cx trace (super, ObjRestT (reason, xs, tvar))
      ) in

      (* Spread fields from the instance into another object *)
      let map = find_props cx insttype.fields_tmap in
      let map = List.fold_left (fun map x -> SMap.remove x map) map xs in
      let proto = MixedT (reason, Mixed_everything) in
      let obj_inst = mk_object_with_map_proto cx reason map proto in

      (* ObjAssign the inst-generated obj into the super-generated obj *)
      let o = mk_tvar_where cx reason (fun tvar ->
        rec_flow cx trace (
          obj_inst,
          ObjAssignT(reason, obj_super, tvar, [], false)
        )
      ) in

      rec_flow_t cx trace (o, t)

    (* ...AnyObjT and AnyFunT yield AnyObjT *)
    | (AnyFunT _ | AnyObjT _), ObjRestT (reason, _, t) ->
      rec_flow_t cx trace (AnyObjT reason, t)

    | (MixedT _, ObjRestT (reason, _, t)) ->
      let obj = mk_object_with_proto cx reason l in
      rec_flow_t cx trace (obj, t)

    | ((NullT _ | VoidT _), ObjRestT (reason, _, t)) ->
      (* mirroring Object.assign semantics, treat null/void as empty objects *)
      let o = mk_object cx reason in
      rec_flow_t cx trace (o, t)

    (*************************************)
    (* objects can be copied-then-sealed *)
    (*************************************)
    | (ObjT (_, { props_tmap = mapr; _ }), ObjSealT (reason, t)) ->
      let src_props = find_props cx mapr in
      let new_obj =
        mk_object_with_map_proto cx reason ~sealed:true src_props l
      in
      rec_flow_t cx trace (new_obj, t)

    (*************************)
    (* objects can be frozen *)
    (*************************)

    | (ObjT (reason_o, objtype), ObjFreezeT (reason_op, t)) ->
      (* make the reason describe the result (e.g. a frozen object literal),
         but point at the entire Object.freeze call. *)
      let desc = desc_of_reason reason_o |> spf "frozen %s" in
      let reason = replace_reason desc reason_op in

      let flags = {frozen = true; sealed = Sealed; exact = true;} in
      let new_obj = ObjT (reason, {objtype with flags}) in
      rec_flow_t cx trace (new_obj, t)

    (*******************************************)
    (* objects may have their fields looked up *)
    (*******************************************)

    | (ObjT (reason_obj, { props_tmap = mapr; proto_t = proto; dict_t; _ }),
       LookupT(reason_op,strict,_,x,t_other))
      ->
      let t = ensure_prop_for_read cx strict mapr x proto dict_t
        reason_obj reason_op trace in
      rec_flow cx trace (t, UnifyT(t, t_other))

    | AnyObjT _, LookupT (reason_op, _, _, _, t_other) ->
      rec_unify cx trace (AnyT.why reason_op) t_other

    (*****************************************)
    (* ... and their fields written *)
    (*****************************************)

    | (ObjT (_, {flags; _}), SetPropT(_, (_, "constructor"), _)) ->
      if flags.frozen then flow_err cx trace "Mutation not allowed on" l u

    (** o.x = ... has the additional effect of o[_] = ... **)

    | ObjT (reason_o, {
        flags;
        props_tmap = mapr;
        proto_t = proto;
        dict_t }),
      SetPropT (reason_op, (reason_prop, x), tin) ->
      if flags.frozen then
        flow_err cx trace "Mutation not allowed on" l u
      else
        let strict_reason = mk_strict_lookup_reason
          flags.sealed (dict_t <> None) reason_o reason_op in
        let t = ensure_prop_for_write cx trace strict_reason mapr x proto
          reason_op reason_prop in
        dictionary cx trace (string_key x reason_op) t dict_t;
        rec_flow_t cx trace (tin, t)

    (* Since we don't know the type of the prop, use AnyT. *)
    | (AnyObjT _, SetPropT (reason_op, _, t)) ->
      rec_flow_t cx trace (t, AnyT.why reason_op)

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | ObjT (_, {proto_t = proto; _}), GetPropT (_, (_, "__proto__"), t) ->
      rec_flow_t cx trace (proto,t)

    | ObjT _, GetPropT(reason_op, (_, "constructor"), tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | ObjT (reason_o, { flags; props_tmap = mapr; proto_t = proto; dict_t }),
      GetPropT (reason_op, (reason_prop, x), tout) ->
      let strict = mk_strict_lookup_reason
        flags.sealed (dict_t <> None) reason_o reason_op in
      let t = ensure_prop_for_read cx strict mapr x proto dict_t
        reason_o reason_prop trace in
      (* move property type to read site *)
      rec_flow cx trace (t, ReposLowerT (reason_op, UseT tout))

    | AnyObjT _, GetPropT (reason_op, _, tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | (ObjT _, MethodT(_, (_, "constructor"), _)) -> ()

    | (ObjT (reason_o, {
      flags;
      props_tmap = mapr;
      proto_t = proto;
      dict_t;
    }),
       MethodT(reason_op, (reason_prop, x), funtype))
      ->
      let strict = mk_strict_lookup_reason
        flags.sealed (dict_t <> None) reason_o reason_op in
      let t = ensure_prop_for_read cx strict mapr x proto dict_t
        reason_o reason_prop trace in
      let callt = CallT (reason_op, funtype) in
      rec_flow cx trace (t, callt)

    (* Since we don't know the signature of a method on AnyObjT, assume every
       parameter is an AnyT. *)
    | (AnyObjT _, MethodT (reason_op, _, {params_tlist; _})) ->
      let any = AnyT.why reason_op in
      List.iter (fun t -> rec_flow_t cx trace (t, any)) params_tlist

    (******************************************)
    (* strings may have their characters read *)
    (******************************************)

    | (StrT (reason_s, _), GetElemT(reason_op,index,tout)) ->
      rec_flow_t cx trace (index, NumT.why reason_s);
      rec_flow_t cx trace (StrT.why reason_op, tout)

    (** Expressions may be used as keys to access objects and arrays. In
        general, we cannot evaluate such expressions at compile time. However,
        in some idiomatic special cases, we can; in such cases, we know exactly
        which strings/numbers the keys may be, and thus, we can use precise
        properties and indices to resolve the accesses. *)

    (**********************************************************************)
    (* objects/arrays may have their properties/elements written and read *)
    (**********************************************************************)

    | (ObjT _, SetElemT(reason_op,key,tin))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, AnyWithLowerBoundT tin))

    | (ObjT _, GetElemT(reason_op,key,tout))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, AnyWithUpperBoundT tout))

    (* Since we don't know the type of the element, flow it to `AnyT`. This
       could go through `ElemT` like `ObjT` does, but this is a shortcut. *)
    | (AnyObjT _, SetElemT (reason_op, _, t)) ->
      rec_flow_t cx trace (t, AnyT.why reason_op)

    | (AnyObjT _, GetElemT (reason_op, _, tout)) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | (ArrT (_, _, []), SetElemT(reason_op, key,tin))
      ->
      let num = NumT.why reason_op in
      rec_flow cx trace (num, ElemT(reason_op, l, AnyWithLowerBoundT tin));
      rec_flow_t cx trace (key, num)

    | (ArrT _, SetElemT(reason_op, key,tin))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, AnyWithLowerBoundT tin))

    | (ArrT (_, _, []), GetElemT(reason_op, key,tout))
      ->
      let num = NumT.why reason_op in
      rec_flow cx trace (num, ElemT(reason_op, l, AnyWithUpperBoundT tout));
      rec_flow_t cx trace (key, num)

    | (ArrT _, GetElemT(reason_op, key,tout))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, AnyWithUpperBoundT tout))

    | (StrT (reason_x, Literal x), ElemT(reason_op, (ObjT _ as o), t)) ->
      let reason_x = replace_reason (spf "property `%s`" x) reason_x in
      (match t with
      | AnyWithLowerBoundT tin ->
          rec_flow cx trace (o, SetPropT(reason_op, (reason_x, x), tin))
      | AnyWithUpperBoundT tout ->
          rec_flow cx trace (o, GetPropT(reason_op, (reason_x, x), tout))
      | _ -> assert false)

    (* if the object is a dictionary, verify it *)
    | (_, ElemT(_, ObjT(_, {dict_t = Some { key; value; _ }; _}), t))
      ->
      rec_flow_t cx trace (l, key);
      rec_flow_t cx trace (value,t);
      rec_flow_t cx trace (t,value)

    (* otherwise, string and number keys are allowed, but there's nothing else
       to flow without knowing their literal values. *)
    | (StrT _, ElemT(_, ObjT _, _))
    | (NumT _, ElemT(_, ObjT _, _)) ->
      ()

    | (NumT (_, literal), ElemT(_, ArrT(_, value, ts), t)) ->
      let value = match literal with
      | Literal (float_value, _) ->
          begin try
            float_value
            |> int_of_float
            |> List.nth ts
          with _ ->
            value
          end
      | _ -> value
      in
      (match t with
      | AnyWithLowerBoundT tin -> rec_flow_t cx trace (tin, value)
      | AnyWithUpperBoundT tout -> rec_flow_t cx trace (value, tout)
      | _ -> assert false)

    | (ArrT _, GetPropT(reason_op, (_, "constructor"), tout)) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | (ArrT _, SetPropT(_, (_, "constructor"), _))
    | (ArrT _, MethodT(_, (_, "constructor"), _)) ->
      ()

    (**************************************************)
    (* array pattern can consume the rest of an array *)
    (**************************************************)

    | (ArrT (_, t, ts), ArrRestT (reason, i, tout)) ->
      let a = ArrT (reason, t, Core_list.drop ts i) in
      rec_flow_t cx trace (a, tout)

    (**************************************************)
    (* function types can be mapped over a tuple      *)
    (**************************************************)

    | ArrT (_, t, ts), TupleMapT (reason, funt, tout) ->
      let f x = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (funt, CallT (reason, mk_functiontype [x] t))
      ) in
      rec_flow_t cx trace (ArrT (reason, f t, List.map f ts), tout)

    (***********************************************)
    (* functions may have their prototypes written *)
    (***********************************************)

    | (FunT (_, _, t, _), SetPropT(reason_op, (_, "prototype"), tin)) ->
      rec_flow cx trace (tin, ObjAssignT(reason_op, t, AnyT.t, [], false))

    (*********************************)
    (* ... and their prototypes read *)
    (*********************************)

    | (FunT (_, _, t, _), GetPropT(_, (_, "prototype"), tout)) ->
      rec_flow_t cx trace (t,tout)

    | (ClassT (instance), GetPropT(_, (_, "prototype"), tout)) ->
      rec_flow_t cx trace (instance, tout)

    (**************************************)
    (* ... and their fields/elements read *)
    (**************************************)

    | (AnyFunT _, (
        GetPropT(reason_op, _, tout)
        | GetElemT(reason_op, _, tout)
        | LookupT(reason_op, _, _, _, tout)
      )) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    (*****************************************)
    (* ... and their fields/elements written *)
    (*****************************************)

    | (AnyFunT _, SetPropT(reason_op, _, t))
    | (AnyFunT _, SetElemT(reason_op, _, t)) ->
      rec_flow_t cx trace (t, AnyT.why reason_op)

    (***************************************************************)
    (* functions may be called by passing a receiver and arguments *)
    (***************************************************************)

    | FunProtoCallT _,
      CallT (reason_op, ({this_t = func; params_tlist; _} as funtype)) ->
      begin match params_tlist with
      (* func.call() *)
      | [] ->
        let funtype = { funtype with
          this_t = VoidT.why reason_op;
          params_tlist = [];
        } in
        rec_flow cx trace (func, CallT (reason_op, funtype))

      (* func.call(this_t, ...params_tlist) *)
      | this_t::params_tlist ->
        let funtype = { funtype with this_t; params_tlist } in
        rec_flow cx trace (func, CallT (reason_op, funtype))
      end

    (*******************************************)
    (* ... or a receiver and an argument array *)
    (*******************************************)

    (* resolves the arguments... *)
    | FunProtoApplyT _,
      CallT (reason_op, ({this_t = func; params_tlist; _} as funtype)) ->
      begin match params_tlist with
      (* func.apply() *)
      | [] ->
          let funtype = { funtype with
            this_t = VoidT.why reason_op;
            params_tlist = [];
          } in
          rec_flow cx trace (func, CallT (reason_op, funtype))

      (* func.apply(this_arg) *)
      | this_arg::[] ->
          let funtype = { funtype with this_t = this_arg; params_tlist = [] } in
          rec_flow cx trace (func, CallT (reason_op, funtype))

      (* func.apply(this_arg, ts) *)
      | _::ts::_ ->
          (* the first param will be extracted later by ApplyT; extra args are
             allowed but ignored, as with all function calls in Flow. *)
          rec_flow cx trace (ts, ApplyT (reason_op, func, funtype))
      end

    (* ... then calls the function *)
    | (ArrT (_, t, ts),
       ApplyT (r, func, ({params_tlist=this_t::_; _} as funtype))) ->
      let funtype = { funtype with this_t; params_tlist = ts @ [RestT t] } in
      rec_flow cx trace (func, CallT (r, funtype))

    | (NullT _ | VoidT _),
      ApplyT (r, func, ({params_tlist=this_t::_; _} as funtype)) ->
      let funtype = { funtype with this_t; params_tlist = [] } in
      rec_flow cx trace (func, CallT (r, funtype))

    (************************************************************************)
    (* functions may be bound by passing a receiver and (partial) arguments *)
    (************************************************************************)

    | FunProtoBindT _,
      CallT (reason_op, ({
        this_t = func;
        params_tlist = this_t::params_tlist;
        _
      } as funtype)) ->
      let funtype = { funtype with this_t; params_tlist } in
      rec_flow cx trace (func, BindT (reason_op, funtype))

    | (FunT (reason,_,_,
             {this_t = o1; params_tlist = tins1; return_t = tout1; _}),
       BindT (reason_op,
             {this_t = o2; params_tlist = tins2; return_t = tout2; _}))
      -> (* TODO: closure *)

        rec_flow_t cx trace (o2,o1);

        let tins1 = multiflow_partial cx trace (tins2,tins1) in

        (* e.g. "bound function type", positioned at reason_op *)
        let bound_reason = replace_reason
          (spf "bound %s" (desc_of_reason reason))
          reason_op in

        rec_flow_t cx trace (
          FunT(
            reason_op,
            dummy_static bound_reason,
            dummy_prototype,
            mk_functiontype tins1 tout1
          ),
          tout2);

    | (AnyFunT _, BindT (reason, {
        this_t;
        params_tlist;
        return_t;
        _;
      })) ->
      rec_flow_t cx trace (AnyT.why reason, this_t);
      params_tlist |> List.iter (fun param_t ->
        rec_flow_t cx trace (AnyT.why reason, param_t)
      );
      rec_flow_t cx trace (l, return_t)

    (***********************************************)
    (* You can use a function as a callable object *)
    (***********************************************)
    | (FunT (reason, statics, _, _) , UseT ObjT _) ->
        let map = SMap.add "$call" l SMap.empty in
        let function_proto = get_builtin_type cx reason "Function" in
        let obj = mk_object_with_map_proto cx reason map function_proto in
        let t = mk_tvar_where cx reason (fun t ->
          rec_flow cx trace (statics, ObjAssignT (reason, obj, t, [], false))
        ) in
        rec_flow cx trace (t, u)

    (*********************************************************************)
    (* class A is a base class of class B iff                            *)
    (* properties in B that override properties in A or its base classes *)
    (* have the same signatures                                          *)
    (*********************************************************************)

    (** The purpose of SuperT is to establish consistency between overriding
        properties with overridden properties. As such, the lookups performed
        for the inherited properties are non-strict: they are not required to
        exist. **)

    | (InstanceT (_,_,_,instance_super),
       SuperT (reason,instance))
      ->
        iter_props cx instance_super.fields_tmap (fun x -> function
          | AbstractT t when not (has_prop cx instance.fields_tmap x) ->
            (* when abstract fields are not implemented, make them void *)
            let reason = reason_of_t t in
            let desc_void = spf "undefined. Did you forget to declare %s?"
              (desc_of_reason reason) in
            let reason_void = replace_reason desc_void reason in
            rec_unify cx trace (VoidT reason_void) t
          | _ -> ()
        );
        iter_props cx instance.fields_tmap (fun x t ->
          lookup_prop cx trace l reason None x t
        );
        iter_props cx instance.methods_tmap (fun x t ->
          if inherited_method x then
            (* we're able to do supertype compatibility in super methods because
               they're immutable *)
            lookup_prop cx trace l reason None x (AnyWithLowerBoundT t)
        )

    | ObjT _, SuperT (reason, instance)
    | AnyObjT _, SuperT (reason, instance)
      ->
        iter_props cx instance.fields_tmap (fun x t ->
          rec_flow cx trace (l, LookupT(reason,None,[],x,t))
        );
        iter_props cx instance.methods_tmap (fun x t ->
          if inherited_method x then
            rec_flow cx trace (l, LookupT(reason,None,[],x,t))
        )

    (***********************************************************)
    (* addition                                                *)
    (***********************************************************)

    | (l, AdderT (reason, r, u)) ->
      Ops.push reason;
      flow_addition cx trace reason l r u;
      Ops.pop ()

    (**************************)
    (* relational comparisons *)
    (**************************)

    | (l, ComparatorT(reason, r)) ->
      Ops.push reason;
      flow_comparator cx trace reason l r;
      Ops.pop ()

    | (l, EqT(reason, r)) ->
      Ops.push reason;
      flow_eq cx trace reason l r;
      Ops.pop ()

    (************************)
    (* unary minus operator *)
    (************************)

    | (NumT (_, lit), UnaryMinusT (reason_op, t_out)) ->
      let num = match lit with
      | Literal (value, raw) ->
        let raw_len = String.length raw in
        let raw = if raw_len > 0 && raw.[0] = '-'
          then String.sub raw 1 (raw_len - 1)
          else "-" ^ raw
        in
        NumT (replace_reason "number" reason_op, Literal (~-. value, raw))
      | AnyLiteral
      | Truthy ->
        l
      in
      rec_flow_t cx trace (num, t_out)

    (**************************************)
    (* types may be refined by predicates *)
    (**************************************)

    | (_, PredicateT(p,t)) ->
      predicate cx trace t (l,p)

    (**********************)
    (* Array library call *)
    (**********************)

    | (ArrT (reason, t, _), HasPropT _) ->
      let arrt = get_builtin_typeapp cx reason "Array" [t] in
      rec_flow cx trace (arrt, u)

    | (ArrT (_, t, _), (GetPropT _ | SetPropT _ | MethodT _ | LookupT _)) ->
      let reason = reason_of_use_t u in
      let arrt = get_builtin_typeapp cx reason "Array" [t] in
      rec_flow cx trace (arrt, u)

    (***********************)
    (* String library call *)
    (***********************)

    | (StrT (reason, _), (GetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "String",u)

    (***********************)
    (* Number library call *)
    (***********************)

    | (NumT (reason, _), (GetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Number",u)

    (***********************)
    (* Boolean library call *)
    (***********************)

    | (BoolT (reason, _), (GetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Boolean",u)

    (*************************)
    (* Function library call *)
    (*************************)

    | (FunProtoT reason, (GetPropT _ | SetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Function",u)

    (*********************)
    (* functions statics *)
    (*********************)

    | (FunT (reason, static, _, _), _) when object_like_op u ->
      rec_flow cx trace (static, ReposLowerT (reason, u))

    (*****************)
    (* class statics *)
    (*****************)

    | (ClassT instance, _) when object_use u || object_like_op u ->
      let reason = prefix_reason "statics of " (reason_of_t instance) in
      let tvar = mk_tvar cx reason in
      rec_flow cx trace (instance, GetPropT(reason, (reason, "statics"), tvar));
      rec_flow cx trace (tvar, ReposLowerT (reason, u))

    (***************************************************************************)
    (* classes can behave like functions, functions can be declared as classes *)
    (***************************************************************************)

    (* When a class value flows to a function annotation or call site, check for
       the presence of a $call property in the former (as a static) compatible
       with the latter. *)
    | (ClassT _, (UseT FunT (reason, _, _, _) | CallT (reason, _))) ->
      rec_flow cx trace (l,
        GetPropT(reason, (reason, "$call"), tvar_with_constraint cx u))

    (* For a function type to be used as a class type, the following must hold:
       - the class's instance type must be a subtype of the function's prototype
       property type and 'this' type
       - the function's statics should be included in the class's statics
       (typically a function's statics are under-specified, so we don't
       enforce equality)
       - the class's static $call property type must be a subtype of the
       function type. *)
    | (FunT (reason, static, prototype, funtype), UseT ClassT instance) ->
      rec_flow_t cx trace (instance, prototype);
      rec_flow_t cx trace (instance, funtype.this_t);
      rec_flow cx trace (instance, GetPropT(reason, (reason, "statics"), static));
      rec_flow cx trace (ClassT instance, GetPropT(reason, (reason, "$call"), l))

    (************)
    (* indexing *)
    (************)

    | (InstanceT _, GetElemT (reason, i, t))
      ->
      rec_flow cx trace (l, SetPropT(reason, (reason, "$key"), i));
      rec_flow cx trace (l, GetPropT(reason, (reason, "$value"), t))

    | (InstanceT _, SetElemT (reason, i, t))
      ->
      rec_flow cx trace (l, SetPropT(reason, (reason, "$key"), i));
      rec_flow cx trace (l, SetPropT(reason, (reason, "$value"), t))

    (*************************)
    (* repositioning, part 2 *)
    (*************************)

    (* waits for a lower bound to become concrete, and then repositions it to
       the location stored in the ReposLowerT, which is usually the location
       where that lower bound was used; the lower bound's location (which is
       being overwritten) is where it was defined. *)
    | (_, ReposLowerT (reason_op, u)) ->
      rec_flow cx trace (reposition cx reason_op l, u)

    (***************)
    (* unsupported *)
    (***************)

    (** Lookups can be strict or non-strict, as denoted by the presence or
        absence of strict_reason in the following two pattern matches.
        Strictness derives from whether the object is sealed and was
        created in the same scope in which the lookup occurs - see
        mk_strict_lookup_reason below. The failure of a strict lookup
        to find the desired property causes an error; a non-strict one
        does not.
     *)

    | (MixedT _,
       LookupT (reason, strict, next::try_ts_on_failure, s, t)) ->
      (* When s is not found, we always try to look it up in the next element in
         the list try_ts_on_failure. *)
      rec_flow cx trace
        (next, LookupT (reason, strict, try_ts_on_failure, s, t))

    | (MixedT (reason, _), LookupT (reason_op, Some strict_reason, [], x, _)) ->

      if is_object_prototype_method x
      then
        (** TODO: These properties should go in Object.prototype. Currently we
            model Object.prototype as a MixedT, as an optimization against a
            possible deluge of shadow properties on Object.prototype, since it
            is shared by every object. **)
        rec_flow cx trace (get_builtin_type cx reason "Object", u)

      (* if we're looking something up on the global/builtin object, then tweak
         the error to say that `x` doesn't exist. We can tell this is the global
         object because that should be the only object created with
         `builtin_reason` instead of an actual location (see `Init_js.init`). *)
      else if is_builtin_reason reason
      then
        let msg =
          if is_internal_module_name x
          then "Required module not found"
          else "Could not resolve name"
        in
        add_error cx (mk_info reason_op [msg])
      else
        let msg =
          if x = "$call"
          then "Callable signature not found in"
          else if x = "$key" || x = "$value"
          then "Indexable signature not found in"
          else "Property not found in"
        in
        flow_err_reasons cx trace msg (reason_op, strict_reason)

    (* LookupT is a non-strict lookup, never fired *)
    | (MixedT _, LookupT _) -> ()

    | (MixedT _, HasPropT (reason_op, Some reason_strict, _)) ->
        flow_err_prop_not_found cx trace (reason_op, reason_strict)

    (* SuperT only involves non-strict lookups *)
    | (MixedT _, SuperT _) -> ()

    (** ExtendsT searches for a nominal superclass. The search terminates with
        either failure at the root or a structural subtype check. **)

    | (MixedT _, UseT ExtendsT (next::try_ts_on_failure, l, u)) ->
      (* When seaching for a nominal superclass fails, we always try to look it
         up in the next element in the list try_ts_on_failure. *)
      rec_flow_t cx trace
        (next, ExtendsT (try_ts_on_failure, l, u))

    | (MixedT _, UseT ExtendsT ([], l, InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = true;
         _;
       })))
      ->
      structural_subtype cx trace l reason_inst
        (super, fields_tmap, methods_tmap)

    | (MixedT _, UseT ExtendsT ([], t, tc)) ->
      let msg = "This type is incompatible with" in
      flow_err_reasons cx trace msg (reason_of_t t, reason_of_t tc)

    (* Special cases of FunT *)
    | FunProtoApplyT reason, _
    | FunProtoBindT reason, _
    | FunProtoCallT reason, _ ->
      rec_flow cx trace (FunProtoT reason, u)

    (* when unexpected types flow into a GetPropT/SetPropT (e.g. void or other
       non-object-ish things), then use `reason_prop`, which represents the
       reason for the prop itself, not the lookup action. *)
    | (_, GetPropT (_, (reason_prop, _), _))
    | (_, SetPropT (_, (reason_prop, _), _)) ->
      flow_err_reasons cx trace (err_msg l u) (reason_prop, reason_of_t l)

    | _ ->
      flow_err cx trace (err_msg l u) l u
  )

(* some types need to be resolved before proceeding further *)
and needs_resolution = function
  | OpenT _ | UnionT _ | OptionalT _ | MaybeT _ | AnnotT _ -> true
  | _ -> false

(**
 * Addition
 *
 * Given l + r:
 *  - if l or r is a string, or a Date, or an object whose
 *    valueOf() returns an object, returns a string.
 *  - otherwise, returns a number
 *
 * Since we don't consider valueOf() right now, Date is no different than
 * any other object. The only things that are neither objects nor strings
 * are numbers, booleans, null, undefined and symbols. Since we can more
 * easily enumerate those things, this implementation inverts the check:
 * anything that is a number, boolean, null or undefined is treated as a
 * number; everything else is a string.
 *
 * We are less permissive than the spec when it comes to string coersion:
 * only numbers can be coerced, to allow things like `num + '%'`.
 *
 * TODO: handle symbols (which raise a TypeError, so should be banned)
 *
 **)
and flow_addition cx trace reason l r u =
  if needs_resolution r then rec_flow cx trace (r, AdderT (reason, l, u))
  else match (l, r) with
  | (StrT _, StrT _)
  | (StrT _, NumT _)
  | (NumT _, StrT _) ->
    rec_flow_t cx trace (StrT.why reason, u)

  | (MixedT _, _)
  | (_, MixedT _) ->
    rec_flow_t cx trace (MixedT.why reason, u)

  | ((NumT _ | SingletonNumT _ | BoolT _ | SingletonBoolT _ | NullT _ | VoidT _),
     (NumT _ | SingletonNumT _ | BoolT _ | SingletonBoolT _ | NullT _ | VoidT _)) ->
    rec_flow_t cx trace (NumT.why reason, u)

  | (_, _) ->
    let fake_str = StrT.why reason in
    rec_flow_t cx trace (l, fake_str);
    rec_flow_t cx trace (r, fake_str);
    rec_flow_t cx trace (fake_str, u);

(**
 * relational comparisons like <, >, <=, >=
 *
 * typecheck iff either of the following hold:
 *   number <> number = number
 *   string <> string = string
 **)
and flow_comparator cx trace reason l r =
  if needs_resolution r then rec_flow cx trace (r, ComparatorT (reason, l))
  else match (l, r) with
  | (StrT _, StrT _) -> ()
  | (_, _) when numeric l && numeric r -> ()
  | (_, _) -> flow_err_use_t cx trace "Cannot be compared to" l r

(**
 * == equality
 *
 * typecheck iff they intersect (otherwise, unsafe coercions may happen).
 *
 * note: any types may be compared with === (in)equality.
 **)
and flow_eq cx trace reason l r =
  if needs_resolution r then rec_flow cx trace (r, EqT(reason, l))
  else match (l, r) with
  | (_, _) when equatable (l, r) -> ()
  | (_, _) -> flow_err_use_t cx trace "Cannot be compared to" l r

and string_contains_react = is_substring "React"

and is_object_prototype_method = function
  | "hasOwnProperty"
  | "propertyIsEnumerable"
  | "toLocaleString"
  | "toString"
  | "valueOf" -> true
  | _ -> false

(* This must list all of the properties on Function.prototype. AnyFunT is a
   function that lets you get/set any property you want on it in an untracked
   way (like AnyObjT, but callable), except for these properties.

   Ideally we'd be able to look these up from the Function lib declaration, but
   we don't have a good way to do that while still allowing AnyFunT to act like
   a dictionary. *)
and is_function_prototype = function
  | "apply"
  | "bind"
  | "call"
  | "arguments"
  | "caller"
  | "length"
  | "name" -> true
  | _ -> false

(* neither object prototype methods nor callable signatures should be
 * implied by an object indexer type *)
and is_dictionary_exempt = function
  | x when is_object_prototype_method x -> true
  | "$call" -> true
  | _ -> false

(* only on use-types - guard calls with is_use t *)
and err_operation = function
  | UseT t ->
    failwith (spf "err_operation called on def type %s" (string_of_ctor t))

  | GetPropT _ -> "Property cannot be accessed on"
  | SetPropT _ -> "Property cannot be assigned on"
  | MethodT _ -> "Method cannot be called on"
  | CallT _ -> "Function cannot be called on"
  | ApplyT _ -> "Expected array of arguments instead of"
  | ConstructorT _ -> "Constructor cannot be called on"
  | GetElemT _ -> "Computed property/element cannot be accessed on"
  | SetElemT _ -> "Computed property/element cannot be assigned on"
  | ElemT (_, ObjT _, AnyWithUpperBoundT _) -> "Computed property cannot be accessed with"
  | ElemT (_, ArrT _, AnyWithUpperBoundT _) -> "Element cannot be accessed with"
  | ElemT (_, ObjT _, AnyWithLowerBoundT _) -> "Computed property cannot be assigned with"
  | ElemT (_, ArrT _, AnyWithLowerBoundT _) -> "Element cannot be assigned with"
  | ObjAssignT _ -> "Expected object instead of"
  | ObjRestT _ -> "Expected object instead of"
  | ObjSealT _ -> "Expected object instead of"
  | ArrRestT _ -> "Expected array instead of"
  | SuperT _ -> "Cannot inherit"
  | MixinT _ -> "Expected class instead of"
  | SpecializeT _ -> "Expected polymorphic type instead of"
  | ThisSpecializeT _ -> "Expected class instead of"
  | VarianceCheckT _ -> "Expected polymorphic type instead of"
  | LookupT _ -> "Property not found in"
  | GetKeysT _ -> "Expected object instead of"
  | HasOwnPropT _ -> "Property not found in"
  | HasPropT _ -> "Property not found in"
  | UnaryMinusT _ -> "Expected number instead of"
  | ReifyTypeT _ -> "Internal Error: Invalid type applied to ReifyTypeT!"
  | TupleMapT _ -> "Expected array instead of"
  (* unreachable or unclassified use-types. until we have a mechanical way
     to verify that all legit use types are listed above, we can't afford
     to throw on a use type, so mark the error instead *)
  | t ->
    (spf "Type is incompatible with (unclassified use type: %s)"
      (string_of_use_ctor t))

and err_value = function
  | NullT _ -> " possibly null value"
  | VoidT _ -> " possibly undefined value"
  | MaybeT _ -> " possibly null or undefined value"
  | IntersectionT _ -> " any member of intersection type"
  | _ -> ""

and err_msg l u =
  if is_use u
  then spf "%s%s" (err_operation u) (err_value l)
  else "This type is incompatible with"

and ground_subtype = function
  (* tvars are not considered ground, so they're not part of this relation *)
  | (OpenT _, _) | (_, UseT OpenT _) -> false

  (* Allow any lower bound to be repositioned *)
  | (_, ReposLowerT _) -> false

  (* Prevents Tainted<any> -> any *)
  | (UnionT _, _) | (TaintT _, _) -> false

  (* Allows call args to propagate  *)
  | (AnyT _, CallT _) -> false

  | (NumT _, UseT NumT _)
  | (StrT _, UseT StrT _)
  | (BoolT _, UseT BoolT _)
  | (NullT _, UseT NullT _)
  | (VoidT _, UseT VoidT _)
  | (EmptyT _, _)
  | (_, UseT MixedT _)
  | (_, UseT FunProtoT _) (* MixedT is used for object protos, this is for funcs *)
  | (AnyT _, _)
  | (_, UseT AnyT _)
    -> true

  | _ ->
    false

and numeric = function
  | NumT _ -> true
  | SingletonNumT _ -> true

  | InstanceT (reason, _, _, _) ->
    desc_of_reason reason = "Date"

  | _ -> false

and object_like = function
  | ObjT _ | InstanceT _ -> true
  | t -> function_like t

and object_use = function
  | UseT ObjT _ -> true
  | _ -> false

and object_like_op = function
  | SetPropT _ | GetPropT _ | MethodT _ | LookupT _
  | SuperT _
  | GetKeysT _ | HasOwnPropT _ | HasPropT _
  | ObjAssignT _ | ObjRestT _
  | SetElemT _ | GetElemT _
  | UseT (AnyObjT _) -> true
  | _ -> false

and function_like = function
  | ClassT _
  | CustomFunT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | FunT _ -> true
  | _ -> false

and function_like_op = function
  | CallT _ | UseT (TypeT _)
  | ConstructorT _
  | UseT (AnyFunT _) -> true
  | t -> object_like_op t

and equatable = function

  | (NumT _,NumT _)

  | (StrT _,StrT _)

  | (BoolT _, BoolT _)

  | (EmptyT _,_) | (_, EmptyT _)

  | (_,MixedT _) | (MixedT _,_)

  | (AnyT _,_) | (_,AnyT _)

  | (VoidT _,_) | (_, VoidT _)

  | (NullT _,_) | (_, NullT _)
    -> true

  | ((NumT _ | StrT _ | BoolT _), _)
  | (_, (NumT _ | StrT _ | BoolT _))
    -> false

  | _ -> true

and taint_op = function
  | AdderT _ | GetPropT _ | GetElemT _ | ComparatorT _ -> true
  | _ -> false

and result_of_taint_op = function
  | AdderT (_, _, u) | GetPropT (_, _, u) | GetElemT (_, _, u) -> Some u
  | ComparatorT _ -> None
  | _ -> assert false

(* generics *)

(* Generate for every type parameter a pair of tests, instantiating that type
   parameter with its bound and Bottom. Run a closure that takes these
   instantiations, each one in turn, and does something with it. We use a
   different value for test_id (see Reason_js.TestID) for every instantiation so
   that re-analyzing the same AST with different instantiations causes different
   reasons to be generated. *)

and generate_tests cx reason typeparams each =
  (* generate 2^|typeparams| maps *)
  let maps = List.fold_left (fun list {name; bound; _ } ->
    let xreason = replace_reason (spf "`%s`" name) reason in
    let bot = EmptyT (
      prefix_reason "some incompatible instantiation of " xreason
    ) in
    (* NOTE: Since the same AST is traversed by each generated test, the order
       of generated tests is important for the proper functioning of hooks that
       record information on the side as ASTs are traversed. Adopting the
       convention that the last traversal "wins" (which would happen, e.g, when
       the recorded information at a location is replaced every time that
       location is encountered), we want the last generated test to always be
       the one where all type parameters are substituted by their bounds
       (instead of Bottom), so that the recorded information is the same as if
       all type parameters were indeed erased and replaced by theirs bounds. *)
    (List.map (SMap.add name bot) list) @
    (List.map (fun map -> SMap.add name (subst cx map bound) map) list)
  ) [SMap.empty] typeparams in
  match maps with
  | [map] -> each map (* no typeparams, so reuse current test_id *)
  | _ -> List.iter (TestID.run each) maps

(*********************)
(* inheritance utils *)
(*********************)

and mk_nominal cx =
  let nominal = mk_id () in
  (if Context.is_verbose cx then prerr_endlinef
      "NOM %d %s" nominal (string_of_filename (Context.file cx)));
  nominal

and flow_type_args cx trace instance instance_super =
  (* with this out of the way, we can assume polaritiy maps are the same *)
  (if instance.class_id != instance_super.class_id then
    assert_false "unexpected difference in class_ids in flow_type_args");
  let { type_args = tmap1; arg_polarities = pmap; _ } = instance in
  let { type_args = tmap2; _ } = instance_super in
  tmap1 |> SMap.iter (fun x t1 ->
    let t2 = SMap.find_unsafe x tmap2 in
    (* type_args contains a mixture of args to type params declared on the
       instance's class, and args to outer-scope type params.
       OTOH arg_polarities only holds polarities of declared params.
       it'll take some upstream refactoring to handle variance to in-scope
       type params - meanwhile, we fall back to neutral (invariant) *)
    match SMap.get x pmap with
    | Some Negative -> rec_flow_t cx trace (t2, t1)
    | Some Positive -> rec_flow_t cx trace (t1, t2)
    | Some Neutral
    | None -> rec_unify cx trace t1 t2
  )

and inherited_method x = x <> "constructor" && x <> "$call"

(* Indicate whether property checking should be strict for a given object and an
   operation on it. Strictness is enforced when the object is not a dictionary,
   and it is sealed (e.g., it is a type annotation) or it and the operation
   originate in different scopes. The enforcement is done via the returned
   "blame token" that is used when looking up properties of objects in the
   prototype chain as part of that operation. *)
and mk_strict_lookup_reason sealed is_dict reason_o reason_op =
  let sealed = not is_dict && match sealed with
  | Sealed -> true
  | UnsealedInFile source -> source <> (Loc.source (loc_of_reason reason_op))
  in
  if sealed then
    Some reason_o
  else
    None

(* dispatch checks to verify that lower satisfies the structural
   requirements given in the tuple. *)
and structural_subtype cx trace lower reason_struct
  (super, fields_tmap, methods_tmap) =
  let lower_reason = reason_of_t lower in
  let fields_tmap = find_props cx fields_tmap in
  let methods_tmap = find_props cx methods_tmap in
  fields_tmap |> SMap.iter (fun s t2 ->
    match t2 with
    | OptionalT t2 ->
      let lookup_reason =
        prefix_reason (spf "optional property `%s` of " s) reason_struct in
      rec_flow cx trace
        (lower,
         LookupT (lookup_reason, None, [], s, t2))
    | _ ->
      let lookup_reason =
        prefix_reason (spf "property `%s` of " s) reason_struct in
      rec_flow cx trace
        (lower,
         LookupT (lookup_reason, Some lower_reason, [], s, t2))
  );
  methods_tmap |> SMap.iter (fun s t2 ->
    if inherited_method s then
      let lookup_reason =
        prefix_reason (spf "property `%s` of " s) reason_struct in
      rec_flow cx trace
        (lower,
         LookupT (lookup_reason, Some lower_reason, [], s, AnyWithUpperBoundT (t2)))
  );
  rec_flow_t cx trace (lower, super)


(*****************)
(* substitutions *)
(*****************)

(** Substitute bound type variables with associated types in a type. Do not
    force substitution under polymorphic types. This ensures that existential
    type variables under a polymorphic type remain unevaluated until the
    polymorphic type is applied. **)
and subst cx ?(force=true) (map: Type.t SMap.t) t =
  if SMap.is_empty map then t
  else match t with
  | BoundT typeparam ->
    begin match SMap.get typeparam.name map with
    | None -> t
    | Some t ->
      (* opportunistically reposition This substitutions; in general
         repositioning may lead to non-termination *)
      if typeparam.name = "this" then reposition cx typeparam.reason t
      else t
    end

  | ExistsT reason ->
    if force then mk_tvar cx reason
    else t

  | OpenT _
  | NumT _
  | StrT _
  | BoolT _
  | EmptyT _
  | NullT _
  | VoidT _
  | MixedT _
  | TaintT _
  | AnyT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | CustomFunT _
    ->
    t

  | FunT (reason, static, proto, {
    this_t = this;
    params_tlist = params;
    params_names = names;
    return_t = ret;
    closure_t = j;
    changeset
  }) ->
    FunT (reason, subst cx ~force map static, subst cx ~force map proto, {
      this_t = subst cx ~force map this;
      params_tlist = List.map (subst cx ~force map) params;
      params_names = names;
      return_t = subst cx ~force map ret;
      closure_t = j;
      changeset
    })

  | PolyT (xs,t) ->
    let xs, map = List.fold_left (fun (xs, map) typeparam ->
      { typeparam with bound = subst cx ~force map typeparam.bound }::xs,
      SMap.remove typeparam.name map
    ) ([], map) xs in
    PolyT (List.rev xs, subst cx ~force:false map t)

  | ThisClassT t ->
    let map = SMap.remove "this" map in
    ThisClassT (subst cx ~force map t)

  | ObjT (reason, {
    flags;
    dict_t;
    props_tmap = id;
    proto_t = proto
  }) ->
    ObjT (reason, {
      flags;
      dict_t = (match dict_t with
      | None -> None
      | Some dict ->
          Some { dict with
            key = subst cx ~force map dict.key;
            value = subst cx ~force map dict.value;
          });
      props_tmap = subst_propmap cx force map id;
      proto_t = subst cx ~force map proto
    })

  | ArrT (reason, t, ts) ->
    ArrT (reason,
          subst cx ~force map t,
          ts |> List.map (subst cx ~force map))

  | ClassT t -> ClassT (subst cx ~force map t)

  | TypeT (reason, t) ->
    TypeT (reason, subst cx ~force map t)

  | AnnotT (sink_t, source_t) ->
    AnnotT (subst cx ~force map sink_t, subst cx ~force map source_t)

  | InstanceT (reason, static, super, instance) ->
    InstanceT (
      reason,
      subst cx ~force map static,
      subst cx ~force map super,
      { instance with
        type_args = instance.type_args |> SMap.map (subst cx ~force map);
        fields_tmap = subst_propmap cx force map instance.fields_tmap;
        methods_tmap = subst_propmap cx force map instance.methods_tmap;
      }
    )

  | OptionalT (t) -> OptionalT (subst cx ~force map t)

  | RestT (t) -> RestT (subst cx ~force map t)

  | AbstractT (t) -> AbstractT (subst cx ~force map t)

  | EvalT (t, defer_use_t, id) ->
      let result_t = subst cx ~force map t in
      let result_defer_use_t = subst_defer_use_t cx ~force map defer_use_t in
      let result_id =
        if t = result_t && defer_use_t = result_defer_use_t
        then id else mk_id () in
      EvalT (result_t, result_defer_use_t, result_id)

  | TypeAppT(c, ts) ->
      let c = subst cx ~force map c in
      let ts = List.map (subst cx ~force map) ts in
      TypeAppT(c, ts)

  | ThisTypeAppT(c, this, ts) ->
      let c = subst cx ~force map c in
      let this = subst cx ~force map this in
      let ts = List.map (subst cx ~force map) ts in
      ThisTypeAppT(c, this, ts)

  | MaybeT(t) ->
    MaybeT(subst cx ~force map t)

  | IntersectionT (reason, rep) ->
    IntersectionT (reason, InterRep.map (subst cx ~force map) rep)

  | UnionT (reason, rep) ->
    UnionT (reason, UnionRep.map (subst cx ~force map) rep)

  | AnyWithLowerBoundT(t) ->
    AnyWithLowerBoundT(subst cx ~force map t)

  | AnyWithUpperBoundT(t) ->
    AnyWithUpperBoundT(subst cx ~force map t)

  | AnyObjT _ -> t
  | AnyFunT _ -> t

  | ShapeT(t) ->
    ShapeT(subst cx ~force map t)

  | DiffT(t1, t2) ->
    DiffT(subst cx ~force map t1, subst cx ~force map t2)

  | KeysT(reason, t) ->
    KeysT(reason, subst cx ~force map t)

  | SingletonNumT _
  | SingletonBoolT _
  | SingletonStrT _ -> t

  | SpeculativeMatchT _
  | ModuleT _
  | ReposUpperT _
  | ExtendsT _
    ->
      failwith (spf "Unhandled type ctor: %s" (string_of_ctor t)) (* TODO *)

and subst_defer_use_t cx ~force map = function
  | DestructuringT (reason, s) ->
      DestructuringT (reason, subst_selector cx force map s)

and eval_selector cx reason curr_t s i =
  let evaluated = Context.evaluated cx in
  match IMap.get i evaluated with
  | None ->
    mk_tvar_where cx reason (fun tvar ->
      Context.set_evaluated cx (IMap.add i tvar evaluated);
      flow_opt cx (curr_t, match s with
      | Prop x -> GetPropT(reason, (reason, x), tvar)
      | Elem key -> GetElemT(reason, key, tvar)
      | ObjRest xs -> ObjRestT(reason, xs, tvar)
      | ArrRest i -> ArrRestT(reason, i, tvar)
      | Default -> PredicateT (NotP VoidP, tvar)
      )
    )
  | Some it ->
    it

and subst_propmap cx force map id =
  let pmap = find_props cx id in
  let pmap_ = SMap.map (subst cx ~force map) pmap in
  if pmap_ = pmap then id
  else mk_propmap cx pmap_

and subst_selector cx force map = function
  | Prop x -> Prop x
  | Elem key -> Elem (subst cx ~force map key)
  | ObjRest xs -> ObjRest xs
  | ArrRest i -> ArrRest i
  | Default -> Default

(* TODO: flesh this out *)
and check_polarity cx polarity = function
  (* base case *)
  | BoundT tp ->
    if not (Polarity.compat (tp.polarity, polarity))
    then polarity_mismatch cx polarity tp

  | OpenT _
  | NumT _
  | StrT _
  | BoolT _
  | EmptyT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _
  | TaintT _
  | ExistsT _
  | AnyObjT _
  | AnyFunT _
  | SingletonStrT _
  | SingletonNumT _
  | SingletonBoolT _
    -> ()

  | OptionalT t
  | RestT t
  | AbstractT t
  | MaybeT t
  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t
    -> check_polarity cx polarity t

  | ClassT t
    -> check_polarity cx Neutral t

  | TypeT (_, t)
    -> check_polarity cx Neutral t

  | InstanceT (_, _, _, instance) ->
    check_polarity_propmap cx Neutral instance.fields_tmap;
    check_polarity_propmap cx polarity instance.methods_tmap

  | FunT (_, _, _, func) ->
    List.iter (check_polarity cx (Polarity.inv polarity)) func.params_tlist;
    check_polarity cx polarity func.return_t

  | ArrT (_, t, _) ->
    check_polarity cx Neutral t

  | ObjT (_, obj) ->
    check_polarity_propmap cx Neutral obj.props_tmap

  | UnionT (_, rep) ->
    List.iter (check_polarity cx polarity) (UnionRep.members rep)

  | IntersectionT (_, rep) ->
    List.iter (check_polarity cx polarity) (InterRep.members rep)

  | PolyT (xs, t) ->
    List.iter (check_polarity_typeparam cx (Polarity.inv polarity)) xs;
    check_polarity cx polarity t

  | ThisTypeAppT (c, _, ts)
  | TypeAppT (c, ts)
    ->
    check_polarity_typeapp cx polarity c ts

  | ThisClassT _
  | ModuleT _
  | AnnotT _
  | ShapeT _
  | DiffT _
  | SpeculativeMatchT _
  | KeysT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | EvalT _
  | ReposUpperT _
  | ExtendsT _
  | CustomFunT _
    -> () (* TODO *)

and check_polarity_propmap cx polarity id =
  let pmap = find_props cx id in
  SMap.iter (fun _ -> check_polarity cx polarity) pmap

and check_polarity_typeparam cx polarity tp =
  check_polarity cx polarity tp.bound

and check_polarity_typeapp cx polarity c ts =
  let reason = prefix_reason "variance check: " (reason_of_t c) in
  flow_opt cx (c, VarianceCheckT(reason, ts, polarity))

and variance_check cx polarity = function
  | [], _ | _, [] ->
    (* ignore typeapp arity mismatch, since it's handled elsewhere *)
    ()
  | tp::tps, t::ts ->
    check_polarity cx (Polarity.mult (polarity, tp.polarity)) t;
    variance_check cx polarity (tps, ts)

and polarity_mismatch cx polarity tp =
  let msg = spf
    "%s position (expected `%s` to occur only %sly)"
    (Polarity.string polarity)
    tp.name
    (Polarity.string tp.polarity) in
  add_error cx (mk_info tp.reason [msg])

and typeapp_arity_mismatch cx expected_num reason =
  let msg = spf "wrong number of type arguments (expected %d)" expected_num in
  add_error cx (mk_info reason [msg])

(* Instantiate a polymorphic definition given type arguments. *)
and instantiate_poly_with_targs cx trace reason_op ?(cache=false) (xs,t) ts =
  let len_xs = List.length xs in
  if len_xs <> List.length ts
  then (
    typeapp_arity_mismatch cx len_xs reason_op;
    AnyT reason_op
  )
  else
    let map =
      List.fold_left2
        (fun map typeparam t ->
          let t_ = cache_instantiate cx trace cache (typeparam, reason_op) t in
          rec_flow_t cx trace (t_, subst cx map typeparam.bound);
          SMap.add typeparam.name t_ map
        )
        SMap.empty xs ts
    in
    subst cx map t

(* Given a type parameter, a supplied type argument for specializing it, and a
   reason for specialization, either return the type argument or, when directed,
   look up the instantiation cache for an existing type argument for the same
   purpose and unify it with the supplied type argument. *)
and cache_instantiate cx trace cache (typeparam, reason_op) t =
  if cache then
    let t_ = Cache.PolyInstantiation.find cx (typeparam, reason_op) in
    rec_unify cx trace t t_;
    t_
  else t

(* Instantiate a polymorphic definition by creating fresh type arguments. *)
and instantiate_poly ?(weak=false) cx trace reason_op (xs,t) =
  let ts = xs |> List.map (fun typeparam ->
    if weak then (
      match typeparam.bound with
      | MixedT _ -> AnyT.why reason_op
      | other_bound -> AnyWithUpperBoundT (other_bound)
    ) else ImplicitTypeArgument.mk_targ cx (typeparam, reason_op)
  )
  in
  instantiate_poly_with_targs cx trace reason_op (xs,t) ts

(* Fix a this-abstracted instance type by tying a "knot": assume that the
   fixpoint is some `this`, substitute it as This in the instance type, and
   finally unify it with the instance type. Return the class type wrapping the
   instance type. *)
and fix_this_class cx trace reason i =
  let this = mk_tvar cx reason in
  let i = subst cx (SMap.singleton "this" this) i in
  rec_unify cx trace this i;
  ClassT(i)

(* Specialize This in a class. Eventually this causes substitution. *)
and instantiate_this_class cx trace reason tc this =
  mk_tvar_where cx reason (fun tvar ->
    rec_flow cx trace (tc, ThisSpecializeT (reason, this, tvar))
  )

(* Specialize targs in a class. This is somewhat different from
   mk_typeapp_instance, in that it returns the specialized class type, not the
   specialized instance type. *)
and specialize_class cx trace reason c ts =
  if ts = [] then c
  else mk_tvar_where cx reason (fun tvar ->
    rec_flow cx trace (c, SpecializeT (reason, false, ts, tvar))
  )

and mk_object_with_proto cx reason ?dict proto =
  mk_object_with_map_proto cx reason ?dict SMap.empty proto

and mk_object_with_map_proto cx reason ?(sealed=false) ?frozen ?dict map proto =
  let sealed =
    if sealed then Sealed
    else UnsealedInFile (Loc.source (loc_of_reason reason))
  in
  let flags = { default_flags with sealed } in
  let flags = match frozen with
  | Some frozen -> { flags with frozen }
  | None -> flags
  in
  let pmap = mk_propmap cx map in
  ObjT (reason, mk_objecttype ~flags dict pmap proto)

and mk_object cx reason =
  mk_object_with_proto cx reason (MixedT (reason, Mixed_everything))


(* Object assignment patterns. In the `Object.assign` model (chain_objects), an
   existing object receives properties from other objects. This pattern suffers
   from "races" in the type checker, since the object supposed to receive
   properties is available even when the other objects supplying the properties
   are not yet available. In the `mergeProperties` model (spread_objects), a new
   object receives properties from other objects and is returned, but the new
   object is made available only when the properties have actually been
   received. Similarly, clone_object makes the receiving object available only
   when the properties have actually been received. These patterns are useful
   when merging properties across modules, e.g., and should eventually replace
   other patterns wherever they are potentially racy. *)

and spread_objects cx reason those =
  let obj = mk_object cx reason in
  chain_objects cx reason obj those

and chain_objects cx ?trace reason this those =
  List.fold_left (fun result that ->
    mk_tvar_where cx reason (fun t ->
      flow_opt cx ?trace (result, ObjAssignT(reason, that, t, [], true));
    )
  ) this those

(* Speculatively match types, returning success. *)
and speculative_match cx trace l u =
  let ops = Ops.get () in
  let typeapp_stack = TypeAppExpansion.get () in
  set_speculative ();
  let restore () =
    restore_speculative ();
    TypeAppExpansion.set typeapp_stack;
    Ops.set ops
  in
  try
    rec_flow cx trace (l, u);
    restore ();
    None
  with
  | SpeculativeError err ->
    restore ();
    Some err
  | exn ->
    restore ();
    raise exn

(* try each branch of a union in turn *)
and try_union cx trace l reason rep =
  match UnionRep.members rep with
  | t :: _ ->
    (* embed reasons of consituent types into outer reason *)
    let rdesc = desc_of_reason reason in
    let tdesc = desc_of_reason (reason_of_t t) in
    let udesc = if not (Utils.str_starts_with rdesc "union:")
      then spf "union: %s" tdesc
      else if Utils.str_ends_with rdesc "..."
      then rdesc
      else if Utils.str_ends_with rdesc (tdesc ^ "(s)")
      then rdesc
      else if String.length rdesc >= 256
      then spf "%s | ..." rdesc
      else if Utils.str_ends_with rdesc tdesc
      then spf "%s(s)" rdesc
      else spf "%s | %s" rdesc tdesc
      in
    (* try head member, package the rest *)
    let r = replace_reason udesc reason in
    let next_attempt = UnionT (r, UnionRep.tail rep) in
    rec_flow_t cx trace (SpeculativeMatchT(r, l, UseT (next_attempt)), t)
  | [] ->
    (* fail *)
    match UnionRep.(history rep, errors rep) with
    | [], [] ->
      (* shouldn't happen, but... if no history, give blanket error *)
      rec_flow_t cx trace (l, UnionT (reason, UnionRep.empty))
    | ts, errs ->
      (* otherwise, build extra info from history *)
      let extra = List.(rev (combine ts errs)) |> List.mapi (
        fun i (t, err) ->
          let header_infos = [
            Loc.none, [spf "Member %d:" (i + 1)];
            info_of_reason (reason_of_t t);
            Loc.none, ["Error:"];
          ] in
          let error_infos, error_extra = Errors.(
            infos_of_error err, extra_of_error err
          ) in
          let info_list = header_infos @ error_infos in
          let info_tree = match error_extra with
            | [] -> Errors.InfoLeaf (info_list)
            | _ -> Errors.InfoNode (info_list, error_extra)
          in
          info_tree
      ) in
      let u = UseT (UnionT (reason, UnionRep.empty)) in
      flow_err cx trace (err_msg l u) ~extra l u

(** try the first member of an intersection, packaging the remainder
    for subsequent attempts. Empty intersection indicates failure *)
and try_intersection cx trace u reason rep =
  match InterRep.members rep with
  | t :: _ ->
    (* try the head member, package the rest *)
    let r = replace_reason "intersection" reason in
    let next_attempt = IntersectionT (r, InterRep.tail rep) in
    rec_flow_t cx trace (t, SpeculativeMatchT (reason, next_attempt, u))
  | [] ->
    (* fail *)
    match InterRep.(history rep, errors rep) with
    | [], [] ->
      (* shouldn't happen, but... if no history, give blanket error *)
      rec_flow cx trace (IntersectionT (reason, InterRep.empty), u)
    | ts, errs ->
      (* otherwise, build extra info from history *)
      let extra = List.(rev (combine ts errs)) |> List.mapi (
        fun i (t, err) ->
          let header_infos = [
            Loc.none, [spf "Member %d:" (i + 1)];
            info_of_reason (reason_of_t t);
            Loc.none, ["Error:"];
          ] in
          let error_infos, error_extra = Errors.(
            infos_of_error err, extra_of_error err
          ) in
          let info_list = header_infos @ error_infos in
          let info_tree = match error_extra with
            | [] -> Errors.InfoLeaf (info_list)
            | _ -> Errors.InfoNode (info_list, error_extra)
          in
          info_tree
      ) in
      let l = IntersectionT (reason, InterRep.empty) in
      flow_err cx trace (err_msg l u) ~extra l u

(* Some types need their parts to be concretized (i.e., type variables may need
   to be replaced by concrete types) so that speculation has a chance to fail
   early (and other branches are tried): otherwise, those failures may remain
   latent, and cause spurious errors to be reported. *)

(** TODO: switch to concretization for other types that need multiple parts
    to be concretized, e.g. AdderT, ComparatorT, ObjAssignT... *)

(** Upper and lower bounds have disjoint concretization processes, and we
    look for different types to concretize in each case. But the processes
    have the same general shape:

    1. A particular rule needs an UB (resp. LB) to be fully concrete to
    proceed, so it calls upper/lower_parts_to_concretize. If the type is fully
    concrete already, an empty list is returned, otherwise the list contains
    component types to be concretized. (The caller should treat this list as
    an opaque value, apart from testing its emptiness.)

    2. If the list from step 1 is non-empty, concretization is needed, and
    the rule calls concretize_upper/lower_parts, passing the the (LB, UB)
    pair along with the concretization list. This kicks off the concretization
    process, which uses ConcretizeLower/UpperT to work through the list of
    types without further intervention.

    (Note that concretization is not quite a simple recursive traversal of
    the concretization list: when a tvar is exposed for concretization, the
    edge from that tvar to the ConcretizeLower/UpperT holding the snapshot
    of the corresponding concretization state persists in the graph: many
    concrete LBs may flow through a tvar, and each induces a different
    overall concretization.)

    3. When the concretization process reaches the end of the todo list,
    replace_upper/lower_concrete_types will build a fresh type using the
    concretized parts, and feed the resulting LB/UB pair back into the flow
    function.

    There's no fundamental need for the finished type to be structurally
    similar to the original: in addition to concretizing, the final step
    can do arbitrary transforms.

    But note the importance of guaranteeing termination: for the typechecker
    not to loop[1], a type must eventually yield an empty concretization list
    if passed repeatedly through this process. Every use-case so far will in
    fact yield an empty list in one step, and there's no reason to think
    that multiple steps will ever be needed.

    But care should be taken to avoid simply returning an incoming type as a
    concretized result when there's nothing to do: instead these types should
    return an empty concretization list (concretization is partial, not
    idempotent).

    [1] The FlowConstraint cache will prevent the typechecker from actually
    looping, but the circuit breaker will also prevent further typechecking
    from taking place, making the error silent. To help trap this situation
    we emit an explicit internal error when this equality is detected.
  *)

(** Concretize an upper bound.

    Currently we only need to concretize FunT and CallT UBs, to support
    overloaded function selection. Here we'll request concretization
    for any such type with tvars in its param list.

    Note: we expose the outflow tvars from AnnotT. There may be other
    wrapper types that need similar treatment.
  *)
and upper_parts_to_concretize _cx u =
  let expose_tvars = List.map (function AnnotT (_, t) | t -> t) in
  let has_tvar = List.exists (function OpenT _ -> true | _ -> false) in
  match u with
  | CallT (_, callt)              (* call of overloaded function *)
  | UseT FunT (_, _, _, callt) -> (* selection of overloaded function *)
    let ts = expose_tvars callt.params_tlist in
    if has_tvar ts then ts else []
  | _ -> []

and replace_upper_concrete_parts ts = function
| CallT (reason, callt) ->
  CallT (reason, { callt with params_tlist = ts })
| UseT FunT (reason, static, prototype, callt) ->
  UseT (FunT (reason, static, prototype, { callt with params_tlist = ts }))
| u -> u

(** move a newly concretized type from the todo list (final param) to
    the done list, and either continue concretizing or call flow on
    the original LB and the fully concretized UB
  *)
and concretize_upper_parts cx trace l u done_list = function
| [] ->
  let done_list = List.rev done_list in
  let concrete_u = replace_upper_concrete_parts done_list u in

  if u = concrete_u then flow_err cx trace
    "Internal error: concretization leaves upper bound unchanged" l u;

  rec_flow cx trace (l, concrete_u)
| t :: todo_list ->
  rec_flow cx trace (t, ConcretizeUpperT (l, todo_list, done_list, u))

(** Concretize a lower bound.

    Currently we concretize intersection LBs, so they can be manipulated
    and checked in various ways. See replace_lower_concrete_parts.
  *)
and lower_parts_to_concretize _cx = function
| IntersectionT (_, rep) ->
  let ts = InterRep.members rep in
  (** trap intersections that contain, or might contain, function types
      which share a return type.
      TODO: similarly we'll want to trap intersections that contain,
      or might contain, incompatible object types.
      Note also that we extract outflow tvars from AnnotT.
    *)
  let tvar,   (* true if tvars present *)
    fpart,    (* funtypes partitioned by return value *)
    ts =      (* concretization list *)
    List.fold_left (fun (tvar, fpart, acc) -> function
      | AnnotT (_, t) | (OpenT _ as t) ->
        true, fpart, t :: acc
      | FunT _ as t ->  tvar, Partition.add t fpart, t :: acc
      | t -> tvar, fpart, t :: acc
    ) (false, FunType.return_type_partition [], []) ts in
  if tvar || not (Partition.is_discrete fpart)
  then List.rev ts
  else []
| _ -> []

(** Note: here we may normalize or otherwise transform the result LB,
    based on the concretized type list.
  *)
and replace_lower_concrete_parts ts = function
| IntersectionT (r, _) -> (
  match FunType.merge_funtypes_by_return_type ts with
  | [t] -> t
  | ts -> IntersectionT (r, InterRep.make ts)
)
| l -> l

(** move a newly concretized type from the todo list (final param) to
    the done list, and either continue concretizing or call flow on
    the fully concretized LB and the original UB
  *)
and concretize_lower_parts cx trace l u done_list = function
| [] ->
  let done_list = List.rev done_list in
  let concrete_l = replace_lower_concrete_parts done_list l in

  if l = concrete_l then flow_err cx trace
    "Internal error: concretization leaves lower bound unchanged" l u;

  rec_flow cx trace (concrete_l, u)
| t :: todo_list ->
  rec_flow cx trace (t, ConcretizeLowerT (l, todo_list, done_list, u))

(* property lookup functions in objects and instances *)

(**
 * Determines whether a property name should be considered "munged"/private when
 * the `munge_underscores` config option is set.
 *)
and is_munged_prop_name cx name =
  (Context.should_munge_underscores cx)
  && (String.length name >= 2)
  && name.[0] = '_'
  && name.[1] <> '_'

and ensure_prop_for_read cx strict mapr x proto dict_t
  reason_obj reason_op trace =
  let ops = Ops.clear () in
  let t = match (read_prop_opt cx mapr x, dict_t) with
  | Some t, _ -> Some t
  | None, Some { key; value; _ } ->
    if is_dictionary_exempt x
    then None
    else (
      rec_flow_t cx trace (string_key x reason_op, key);
      Some value
    )
  | None, None -> None
  in
  let tout = match t with
  (* map contains property x at type t *)
  | Some t -> t
  (* otherwise, check for/maybe add shadow property *)
  | None ->
    let t =
      match read_prop_opt cx mapr (internal_name x) with
      | Some t -> t
      | None -> intro_prop cx reason_obj x mapr
    in
    t |> recurse_proto cx strict proto reason_op x trace
  in
  Ops.set ops;
  tout

and ensure_prop_for_write cx trace strict mapr x proto reason_op reason_prop =
  match read_prop_opt cx mapr x with
  (* map contains property x at type t *)
  | Some t -> t
  (* otherwise, error if strict, else unshadow/add prop *)
  | None -> (
    match strict with
    | Some reason_o ->
      flow_err_prop_not_found cx trace (reason_prop, reason_o);
      AnyT.t
    | None ->
      let t =
        if has_prop cx mapr (internal_name x)
        then read_and_delete_prop cx mapr (internal_name x) |> (fun t ->
          write_prop cx mapr x t; t
        )
        else intro_prop_ cx reason_op x mapr
      in
      t |> recurse_proto cx None proto reason_prop x trace
    )

and lookup_prop cx trace l reason strict x t =
  let l =
    (* munge names beginning with single _ *)
    if is_munged_prop_name cx x
    then MixedT (reason_of_t l, Mixed_everything)
    else l
  in
  rec_flow cx trace (l, LookupT (reason, strict, [], x, t))

and get_prop cx trace reason_prop reason_op strict super x map tout =
  let ops = Ops.clear () in
  let u = ReposLowerT (reason_op, UseT tout) in
  begin match SMap.get x map with
  | Some p -> rec_flow cx trace (p, u)
  | None ->
    let t = tvar_with_constraint cx u in
    lookup_prop cx trace super reason_prop strict x (AnyWithUpperBoundT t)
  end;
  Ops.set ops

and set_prop cx trace reason_op reason_c super x map tin =
  let map = find_props cx map in
  if SMap.mem x map
  then
    rec_flow_t cx trace (tin, SMap.find_unsafe x map)
  else
    lookup_prop cx trace super reason_op (Some reason_c) x (AnyWithLowerBoundT tin)

and intro_prop cx reason_obj x mapr =
  let reason_prop = prefix_reason (spf ".%s of " x) reason_obj in
  mk_tvar_where cx reason_prop (fun tvar ->
    write_prop cx mapr (internal_name x) tvar
  )

and intro_prop_ cx reason_op x mapr =
  mk_tvar_where cx reason_op (fun tvar ->
    write_prop cx mapr x tvar
  )

and recurse_proto cx strict proto reason_op x trace t =
  rec_flow cx trace (proto, LookupT(reason_op,strict,[],x,t));
  t

(* other utils *)

and filter cx trace t l pred =
  if (pred l) then rec_flow_t cx trace (l,t)

and is_string = function StrT _ -> true | _ -> false
and is_number = function NumT _ -> true | _ -> false
and is_function = function AnyFunT _ | FunT _ -> true | _ -> false
and is_object = function (AnyObjT _ | ObjT _ | ArrT _ | NullT _) -> true | _ -> false
and is_array = function ArrT _ -> true | _ -> false
and is_bool = function BoolT _ -> true | _ -> false

and not_ pred x = not(pred x)

and recurse_into_union filter_fn (r, ts) =
  let new_ts = ts |> List.filter (fun t ->
    match filter_fn t with
    | EmptyT _ -> false
    | _ -> true
  ) in
  match new_ts with
  | [] -> EmptyT r
  | [t] -> t
  | _ -> UnionT (r, UnionRep.make new_ts)

and filter_exists = function
  (* falsy things get removed *)
  | NullT r
  | VoidT r
  | SingletonBoolT (r, false)
  | BoolT (r, Some false)
  | SingletonStrT (r, "")
  | StrT (r, Literal "")
  | SingletonNumT (r, (0., _))
  | NumT (r, Literal (0., _)) -> EmptyT r

  (* unknown things become truthy *)
  | MaybeT t -> t
  | OptionalT t -> filter_exists t
  | BoolT (r, None) -> BoolT (r, Some true)
  | StrT (r, AnyLiteral) -> StrT (r, Truthy)
  | NumT (r, AnyLiteral) -> NumT (r, Truthy)
  | MixedT (r, _) -> MixedT (r, Mixed_truthy)

  (* truthy things pass through *)
  | t -> t

and filter_not_exists t = match t with
  (* falsy things pass through *)
  | NullT _
  | VoidT _
  | SingletonBoolT (_, false)
  | BoolT (_, Some false)
  | SingletonStrT (_, "")
  | StrT (_, Literal "")
  | SingletonNumT (_, (0., _))
  | NumT (_, Literal (0., _)) -> t

  (* truthy things get removed *)
  | SingletonBoolT (r, _)
  | BoolT (r, Some _)
  | SingletonStrT (r, _)
  | StrT (r, (Literal _ | Truthy))
  | ArrT (r, _, _)
  | ObjT (r, _)
  | InstanceT (r, _, _, _)
  | AnyObjT r
  | FunT (r, _, _, _)
  | AnyFunT r
  | SingletonNumT (r, _)
  | NumT (r, (Literal _ | Truthy))
  | MixedT (r, Mixed_truthy)
    -> EmptyT r

  | ClassT t -> EmptyT (reason_of_t t)

  (* unknown boolies become falsy *)
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, UnionRep.make [NullT.why reason; VoidT.why reason])
  | BoolT (r, None) -> BoolT (r, Some false)
  | StrT (r, AnyLiteral) -> StrT (r, Literal "")
  | NumT (r, AnyLiteral) -> NumT (r, Literal (0., "0"))

  (* things that don't track truthiness pass through *)
  | t -> t

and filter_maybe = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, UnionRep.make [NullT.why reason; VoidT.why reason])
  | MixedT (r, Mixed_everything) ->
    UnionT (r, UnionRep.make [NullT.why r; VoidT.why r])
  | MixedT (r, Mixed_truthy) -> EmptyT.why r
  | MixedT (r, Mixed_non_maybe) -> EmptyT.why r
  | MixedT (r, Mixed_non_void) -> NullT r
  | MixedT (r, Mixed_non_null) -> VoidT r
  | NullT r -> NullT r
  | VoidT r -> VoidT r
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_maybe = function
  | MaybeT t -> t
  | OptionalT t -> filter_not_maybe t
  | NullT r | VoidT r -> EmptyT r
  | MixedT (r, Mixed_truthy) -> MixedT (r, Mixed_truthy)
  | MixedT (r, Mixed_everything)
  | MixedT (r, Mixed_non_maybe)
  | MixedT (r, Mixed_non_void)
  | MixedT (r, Mixed_non_null) -> MixedT (r, Mixed_non_maybe)
  | t -> t

and filter_null = function
  | OptionalT (MaybeT t)
  | MaybeT t -> NullT.why (reason_of_t t)
  | NullT r -> NullT r
  | MixedT (r, Mixed_everything)
  | MixedT (r, Mixed_non_void) -> NullT.why r
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_null = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, UnionRep.make [VoidT.why reason; t])
  | OptionalT t -> OptionalT (filter_not_null t)
  | UnionT (r, rep) ->
    recurse_into_union filter_not_null (r, UnionRep.members rep)
  | NullT r -> EmptyT r
  | MixedT (r, Mixed_everything) -> MixedT (r, Mixed_non_null)
  | MixedT (r, Mixed_non_void) -> MixedT (r, Mixed_non_maybe)
  | t -> t

and filter_undefined = function
  | MaybeT t -> VoidT.why (reason_of_t t)
  | VoidT r -> VoidT r
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | MixedT (r, Mixed_everything)
  | MixedT (r, Mixed_non_null) -> VoidT.why r
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_undefined = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, UnionRep.make [NullT.why reason; t])
  | OptionalT t -> filter_not_undefined t
  | UnionT (r, rep) ->
    recurse_into_union filter_not_undefined (r, UnionRep.members rep)
  | VoidT r -> EmptyT r
  | MixedT (r, Mixed_everything) -> MixedT (r, Mixed_non_void)
  | MixedT (r, Mixed_non_null) -> MixedT (r, Mixed_non_maybe)
  | t -> t

and filter_string_literal expected t = match t with
  | StrT (_, Literal actual) when actual = expected -> t
  | StrT (r, Truthy) when expected <> "" -> StrT (r, Literal expected)
  | StrT (r, AnyLiteral) -> StrT (r, Literal expected)
  | MixedT (r, _) -> StrT (r, Literal expected)
  | _ -> EmptyT (reason_of_t t)

and filter_not_string_literal expected = function
  | StrT (r, Literal actual) when actual = expected -> EmptyT r
  | t -> t

and filter_number_literal expected t = match t with
  | NumT (_, Literal actual) when snd actual = snd expected -> t
  | NumT (r, Truthy) when snd expected <> "0" -> NumT (r, Literal expected)
  | NumT (r, AnyLiteral) -> NumT (r, Literal expected)
  | MixedT (r, _) -> NumT (r, Literal expected)
  | _ -> EmptyT (reason_of_t t)

and filter_not_number_literal expected = function
  | NumT (r, Literal actual) when snd actual = snd expected -> EmptyT r
  | t -> t

and filter_true = function
  | BoolT (r, Some true)
  | BoolT (r, None) -> BoolT (r, Some true)
  | MixedT (r, _) -> BoolT (replace_reason "boolean" r, Some true)
  | t -> EmptyT (reason_of_t t)

and filter_not_true = function
  | BoolT (r, Some true) -> EmptyT r
  | BoolT (r, None) -> BoolT (r, Some false)
  | t -> t

and filter_false = function
  | BoolT (r, Some false)
  | BoolT (r, None) -> BoolT (r, Some false)
  | MixedT (r, _) -> BoolT (replace_reason "boolean" r, Some false)
  | t -> EmptyT (reason_of_t t)

and filter_not_false = function
  | BoolT (r, Some false) -> EmptyT r
  | BoolT (r, None) -> BoolT (r, Some true)
  | t -> t

(* filter out undefined from a type *)
and filter_optional cx ?trace reason opt_t =
  mk_tvar_where cx reason (fun t ->
    flow_opt_t cx ?trace (opt_t, OptionalT(t))
  )

and predicate cx trace t (l,p) = match (l,p) with

  (************************)
  (* deconstruction of && *)
  (************************)

  | (_, AndP(p1,p2)) ->
    let reason = mk_reason "and" (loc_of_predicate p1) in
    let tvar = mk_tvar cx reason in
    rec_flow cx trace (l,PredicateT(p1,tvar));
    rec_flow cx trace (tvar,PredicateT(p2,t))

  (************************)
  (* deconstruction of || *)
  (************************)

  | (_, OrP(p1,p2)) ->
    rec_flow cx trace (l,PredicateT(p1,t));
    rec_flow cx trace (l,PredicateT(p2,t))

  (*********************************)
  (* deconstruction of binary test *)
  (*********************************)

  (* when left is evaluated, store it and evaluate right *)
  | (_, LeftP(b, r)) ->
    rec_flow cx trace (r, PredicateT(RightP(b, l), t))
  | (_, NotP(LeftP(b, r))) ->
    rec_flow cx trace (r, PredicateT(NotP(RightP(b, l)), t))

  (* when right is evaluated, call appropriate handler *)
  | (r, RightP(b, l)) ->
    binary_predicate cx trace true b l r t
  | (r, NotP(RightP(b, l))) ->
    binary_predicate cx trace false b l r t

  (***********************)
  (* typeof _ ~ "boolean" *)
  (***********************)

  | (MixedT (r, Mixed_truthy), BoolP) ->
    let r = replace_reason BoolT.desc r in
    rec_flow_t cx trace (BoolT (r, Some true), t)

  | (MixedT (r, _), BoolP) ->
    rec_flow_t cx trace (BoolT.why r, t)

  | (_, BoolP) ->
    filter cx trace t l is_bool

  | (_, NotP(BoolP)) ->
    filter cx trace t l (not_ is_bool)

  (***********************)
  (* typeof _ ~ "string" *)
  (***********************)

  | (MixedT (r, Mixed_truthy), StrP) ->
    let r = replace_reason StrT.desc r in
    rec_flow_t cx trace (StrT (r, Truthy), t)

  | (MixedT (r, _), StrP) ->
    rec_flow_t cx trace (StrT.why r, t)

  | (_, StrP) ->
    filter cx trace t l is_string

  | (_, NotP(StrP)) ->
    filter cx trace t l (not_ is_string)

  (*********************)
  (* _ ~ "some string" *)
  (*********************)

  | (_, SingletonStrP lit) ->
    rec_flow_t cx trace (filter_string_literal lit l, t)

  | (_, NotP(SingletonStrP lit)) ->
    rec_flow_t cx trace (filter_not_string_literal lit l, t)

  (*********************)
  (* _ ~ some number n *)
  (*********************)

  | (_, SingletonNumP lit) ->
    rec_flow_t cx trace (filter_number_literal lit l, t)

  | (_, NotP(SingletonNumP lit)) ->
    rec_flow_t cx trace (filter_not_number_literal lit l, t)

  (***********************)
  (* typeof _ ~ "number" *)
  (***********************)

  | (MixedT (r, Mixed_truthy), NumP) ->
    let r = replace_reason NumT.desc r in
    rec_flow_t cx trace (NumT (r, Truthy), t)

  | (MixedT (r, _), NumP) ->
    rec_flow_t cx trace (NumT.why r, t)

  | (_, NumP) ->
    filter cx trace t l is_number

  | (_, NotP(NumP)) ->
    filter cx trace t l (not_ is_number)

  (***********************)
  (* typeof _ ~ "function" *)
  (***********************)

  | (MixedT (r, _), FunP) ->
    rec_flow_t cx trace (AnyFunT (replace_reason "function" r), t)

  | (_, FunP) ->
    filter cx trace t l is_function

  | (_, NotP(FunP)) ->
    filter cx trace t l (not_ is_function)

  (***********************)
  (* typeof _ ~ "object" *)
  (***********************)

  | (MixedT (r, flavor), ObjP) ->
    let reason = replace_reason "object" r in
    let dict = Some {
      key = StrT.why r;
      value = MixedT (replace_reason MixedT.desc r, Mixed_everything);
      dict_name = None;
    } in
    let proto = MixedT (reason, Mixed_everything) in
    let obj = mk_object_with_proto cx reason ?dict proto in
    let filtered_l = match flavor with
    | Mixed_truthy
    | Mixed_non_maybe
    | Mixed_non_null -> obj
    | Mixed_everything
    | Mixed_non_void -> create_union [NullT.why r; obj]
    in
    rec_flow_t cx trace (filtered_l, t)

  | (_, ObjP) ->
    filter cx trace t l is_object

  | (_, NotP(ObjP)) ->
    filter cx trace t l (not_ is_object)

  (*******************)
  (* Array.isArray _ *)
  (*******************)

  | (MixedT (r, _), ArrP) ->
    (* TODO: should use MixedT instead of AnyT, like we do with objects *)
    let filtered_l = ArrT (replace_reason "array" r, AnyT.why r, []) in
    rec_flow_t cx trace (filtered_l, t)

  | (_, ArrP) ->
    filter cx trace t l is_array

  | (_, NotP(ArrP)) ->
    filter cx trace t l (not_ is_array)

  (***********************)
  (* typeof _ ~ "undefined" *)
  (***********************)

  | (_, VoidP) ->
    rec_flow_t cx trace (filter_undefined l, t)

  | (_, NotP(VoidP)) ->
    rec_flow_t cx trace (filter_not_undefined l, t)

  (********)
  (* null *)
  (********)

  | (_, NullP) ->
    rec_flow_t cx trace (filter_null l, t)

  | (_, NotP(NullP)) ->
    rec_flow_t cx trace (filter_not_null l, t)

  (*********)
  (* maybe *)
  (*********)

  | (_, MaybeP) ->
    rec_flow_t cx trace (filter_maybe l, t)

  | (_, NotP(MaybeP)) ->
    rec_flow_t cx trace (filter_not_maybe l, t)

  (********)
  (* true *)
  (********)

  | (_, SingletonBoolP true) ->
    rec_flow_t cx trace (filter_true l, t)

  | (_, NotP(SingletonBoolP true)) ->
    rec_flow_t cx trace (filter_not_true l, t)

  (*********)
  (* false *)
  (*********)

  | (_, SingletonBoolP false) ->
    rec_flow_t cx trace (filter_false l, t)

  | (_, NotP(SingletonBoolP false)) ->
    rec_flow_t cx trace (filter_not_false l, t)

  (************************)
  (* truthyness *)
  (************************)

  | (_, ExistsP) ->
    rec_flow_t cx trace (filter_exists l, t)

  | (_, NotP(ExistsP)) ->
    rec_flow_t cx trace (filter_not_exists l, t)

  | (_, PropExistsP key) ->
    prop_exists_test cx trace key true l t

  | (_, NotP (PropExistsP key)) ->
    prop_exists_test cx trace key false l t

  (* unreachable *)
  | (_, NotP (NotP _))
  | (_, NotP (AndP _))
  | (_, NotP (OrP _)) ->
    assert_false (spf "Unexpected predicate %s" (string_of_predicate p))

and prop_exists_test cx trace key sense obj result =
  match obj with
  | ObjT (_, { props_tmap; _}) ->
      begin match read_prop_opt cx props_tmap key with
      | Some t ->
        let filter = if sense then filter_exists else filter_not_exists in
        begin match filter t with
        | EmptyT _ -> () (* provably unreachable, so prune *)
        | _ -> rec_flow_t cx trace (obj, result)
        end
      | None ->
        rec_flow_t cx trace (obj, result)
      end
  | _ ->
    rec_flow_t cx trace (obj, result)

and binary_predicate cx trace sense test left right result =
  let handler =
    match test with
    | InstanceofTest -> instanceof_test
    | SentinelProp key -> sentinel_prop_test key
  in
  handler cx trace result (sense, left, right)

and instanceof_test cx trace result = function
  (** instanceof on an ArrT is a special case since we treat ArrT as its own
      type, rather than an InstanceT of the Array builtin class. So, we resolve
      the ArrT to an InstanceT of Array, and redo the instanceof check. We do
      it at this stage instead of simply converting (ArrT, InstanceofP c)
      to (InstanceT(Array), InstanceofP c) because this allows c to be resolved
      first. *)
  | (true, (ArrT (reason, elemt, _) as arr), ClassT(InstanceT _ as a)) ->

    let right = ClassT(ExtendsT([], arr, a)) in
    let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
    rec_flow cx trace (arrt, PredicateT(LeftP(InstanceofTest, right), result))

  | (false, (ArrT (reason, elemt, _) as arr), ClassT(InstanceT _ as a)) ->

    let right = ClassT(ExtendsT([], arr, a)) in
    let arrt = get_builtin_typeapp cx reason "Array" [elemt] in
    let pred = NotP(LeftP(InstanceofTest, right)) in
    rec_flow cx trace (arrt, PredicateT (pred, result))

  (** An object is considered `instanceof` a function F when it is constructed
      by F. Note that this is incomplete with respect to the runtime semantics,
      where instanceof is transitive: if F.prototype `instanceof` G, then the
      object is `instanceof` G. There is nothing fundamentally difficult in
      modeling the complete semantics, but we haven't found a need to do it. **)
  | (true, (ObjT (_,{proto_t = proto2; _}) as obj), FunT (_,_,proto1,_))
      when proto1 = proto2 ->

    rec_flow_t cx trace (obj, result)

  (** Suppose that we have an instance x of class C, and we check whether x is
      `instanceof` class A. To decide what the appropriate refinement for x
      should be, we need to decide whether C extends A, choosing either C or A
      based on the result. Thus, we generate a constraint to decide whether C
      extends A (while remembering C), which may recursively generate further
      constraints to decide super(C) extends A, and so on, until we hit the root
      class. (As a technical tool, we use Extends(_, _) to perform this
      recursion; it is also used elsewhere for running similar recursive
      subclass decisions.) **)
  | (true, (InstanceT _ as c), ClassT(InstanceT _ as a)) ->

    predicate cx trace result
      (ClassT(ExtendsT([], c, a)), RightP(InstanceofTest, c))

  (** If C is a subclass of A, then don't refine the type of x. Otherwise,
      refine the type of x to A. (In general, the type of x should be refined to
      C & A, but that's hard to compute.) **)
  | (true, InstanceT (_,_,super_c,instance_c),
     (ClassT(ExtendsT(_, c, InstanceT (_,_,_,instance_a))) as right))
    -> (* TODO: intersection *)

    if instance_a.class_id = instance_c.class_id
    then rec_flow_t cx trace (c, result)
    else
      (** Recursively check whether super(C) extends A, with enough context. **)
      let pred = LeftP(InstanceofTest, right) in
      rec_flow cx trace (super_c, PredicateT(pred, result))

  | (true, MixedT _, ClassT(ExtendsT (_, _, a)))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow_t cx trace (a, result)

  (** Prune the type when any other `instanceof` check succeeds (since this is
      impossible). *)
  | (true, _, _) ->
    ()

  | (false, ObjT (_,{proto_t = proto2; _}), FunT (_,_,proto1,_))
      when proto1 = proto2 ->
    ()

  (** Like above, now suppose that we have an instance x of class C, and we
      check whether x is _not_ `instanceof` class A. To decide what the
      appropriate refinement for x should be, we need to decide whether C
      extends A, choosing either nothing or C based on the result. **)
  | (false, (InstanceT _ as c), ClassT(InstanceT _ as a)) ->

    predicate cx trace result
      (ClassT(ExtendsT([], c, a)), NotP(RightP(InstanceofTest, c)))

  (** If C is a subclass of A, then do nothing, since this check cannot
      succeed. Otherwise, don't refine the type of x. **)
  | (false, InstanceT (_,_,super_c,instance_c),
     (ClassT(ExtendsT(_, _, InstanceT (_,_,_,instance_a))) as right))
    ->

    if instance_a.class_id = instance_c.class_id
    then ()
    else rec_flow cx trace
      (super_c, PredicateT(NotP(LeftP(InstanceofTest, right)), result))

  | (false, MixedT _, ClassT(ExtendsT(_, c, _)))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow_t cx trace (c, result)

  (** Don't refine the type when any other `instanceof` check fails. **)
  | (false, left, _) ->
    rec_flow_t cx trace (left, result)

and sentinel_prop_test key cx trace result = function
  (** Evaluate a refinement predicate of the form

      obj.key eq value

      where eq is === or !==.

      * key is key
      * (sense, obj, value) are the sense of the test, obj and value as above,
      respectively.

      As with other predicate filters, the goal is to statically determine when
      the predicate is definitely satisfied and when it is definitely
      unsatisfied, and narrow the possible types of obj under those conditions,
      while not narrowing in all other cases.

      In this case, the predicate is definitely satisfied (respectively,
      definitely unsatisfied) when the type of the key property in the type obj
      can be statically verified as having (respectively, not having) value as
      its only inhabitant.

      When satisfied, type obj flows to the recipient type result (in other
      words, we allow all such types in the refined type for obj).

      Otherwise, nothing flows to type result (in other words, we don't allow
      any such type in the refined type for obj).

      Overall the filtering process is somewhat tricky to understand. Refer to
      the predicate function and its callers to understand how the context is
      set up so that filtering ultimately only depends on what flows to
      result. **)
  (* obj.key ===/!== string value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), StrT (_, Literal value)) ->
      (match read_prop_opt cx props_tmap key with
        | Some (SingletonStrT (_, v))
        | Some (StrT (_, Literal v)) when (value = v) != sense ->
            (* provably unreachable, so prune *)
            ()
        | _ ->
            (* not enough info to refine: either the property exists but is
               something else that we cannot use as a refinement, or the
               property doesn't exist at all *)
            rec_flow_t cx trace (obj, result)
      )

  (* obj.key ===/!== number value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), NumT (_, Literal (value, _))) ->
      (match read_prop_opt cx props_tmap key with
        | Some (SingletonNumT (_, (v, _)))
        | Some (NumT (_, Literal (v, _))) when (value = v) != sense ->
            (* provably unreachable, so prune *)
            ()
        | _ ->
            (* not enough info to refine: either the property exists but is
               something else that we cannot use as a refinement, or the
               property doesn't exist at all *)
            rec_flow_t cx trace (obj, result)
      )

  (* obj.key ===/!== boolean value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), BoolT (_, Some value)) ->
      (match read_prop_opt cx props_tmap key with
        | Some (SingletonBoolT (_, v))
        | Some (BoolT (_, Some v)) when (value = v) != sense ->
            (* provably unreachable, so prune *)
            ()
        | _ ->
            (* not enough info to refine: either the property exists but is
               something else that we cannot use as a refinement, or the
               property doesn't exist at all *)
            rec_flow_t cx trace (obj, result)
      )

  | (_, obj, _) -> (* not enough info to refine *)
    rec_flow_t cx trace (obj, result)

(***********************)
(* bounds manipulation *)
(***********************)

(** The following general considerations apply when manipulating bounds.

    1. All type variables start out as roots, but some of them eventually become
    goto nodes. As such, bounds of roots may contain goto nodes. However, we
    never perform operations directly on goto nodes; instead, we perform those
    operations on their roots. It is tempting to replace goto nodes proactively
    with their roots to avoid this issue, but doing so may be expensive, whereas
    the union-find data structure amortizes the cost of looking up roots.

    2. Another issue is that while the bounds of a type variable start out
    empty, and in particular do not contain the type variable itself, eventually
    other type variables in the bounds may be unified with the type variable. We
    do not remove these type variables proactively, but instead filter them out
    when considering the bounds. In the future we might consider amortizing the
    cost of this filtering.

    3. When roots are resolved, they act like the corresponding concrete
    types. We maintain the invariant that whenever lower bounds or upper bounds
    contain resolved roots, they also contain the corresponding concrete types.

    4. When roots are unresolved (they have lower bounds and upper bounds,
    possibly consisting of concrete types as well as type variables), we
    maintain the invarant that every lower bound has already been propagated to
    every upper bound. We also maintain the invariant that the bounds are
    transitively closed modulo equivalence: for every type variable in the
    bounds, all the bounds of its root are also included.

**)

(* for each l in ls: l => u *)
and flows_to_t cx trace ls u =
  ls |> TypeMap.iter (fun l trace_l ->
    join_flow cx [trace_l;trace] (l,u)
  )

(* for each u in us: l => u *)
and flows_from_t cx trace l us =
  us |> UseTypeMap.iter (fun u trace_u ->
    join_flow cx [trace;trace_u] (l,u)
  )

(* for each l in ls, u in us: l => u *)
and flows_across cx trace ls us =
  ls |> TypeMap.iter (fun l trace_l ->
    us |> UseTypeMap.iter (fun u trace_u ->
      join_flow cx [trace_l;trace;trace_u] (l,u)
    )
  )

(* bounds.upper += u *)
and add_upper u trace bounds =
  bounds.upper <- UseTypeMap.add u trace bounds.upper

(* bounds.lower += l *)
and add_lower l trace bounds =
  bounds.lower <- TypeMap.add l trace bounds.lower

(* Helper for functions that follow. *)
(* Given a map of bindings from tvars to traces, a tvar to skip, and an `each`
   function taking a tvar and its associated trace, apply `each` to all
   unresolved root constraints reached from the bound tvars, except those of
   skip_tvar. (Typically skip_tvar is a tvar that will be processed separately,
   so we don't want to redo that work. We also don't want to consider any tvar
   that has already been resolved, because the resolved type will be processed
   separately, too, as part of the bounds of skip_tvar. **)
and iter_with_filter cx bindings skip_id each =
  bindings |> IMap.iter (fun id trace ->
    match find_constraints cx id with
    | root_id, Unresolved bounds when root_id <> skip_id ->
        each (root_id, bounds) trace
    | _ ->
        ()
  )

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += t2
*)
(** When going through bounds1.lowertvars, filter out id1. **)
(** As an optimization, skip id1 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_to_t cx trace ?(opt=false) (id1, bounds1) t2 =
  if not opt then add_upper t2 trace bounds1;
  iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) trace_l ->
    add_upper t2 (Trace.concat_trace[trace_l;trace]) bounds
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += t1
*)
(** When going through bounds2.uppertvars, filter out id2. **)
(** As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_from_t cx trace ?(opt=false) t1 (id2, bounds2) =
  if not opt then add_lower t1 trace bounds2;
  iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) trace_u ->
    add_lower t1 (Trace.concat_trace[trace;trace_u]) bounds
  )

(* for each id' in id + bounds.lowertvars:
   id'.bounds.upper += us
*)
and edges_to_ts cx trace ?(opt=false) (id, bounds) us =
  us |> UseTypeMap.iter (fun u trace_u ->
    edges_to_t cx (Trace.concat_trace[trace;trace_u]) ~opt (id, bounds) u
  )

(* for each id' in id + bounds.uppertvars:
   id'.bounds.lower += ls
*)
and edges_from_ts cx trace ?(opt=false) ls (id, bounds) =
  ls |> TypeMap.iter (fun l trace_l ->
    edges_from_t cx (Trace.concat_trace[trace_l;trace]) ~opt l (id, bounds)
  )

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += t2
   for each l in bounds1.lower: l => t2
*)
(** As an invariant, bounds1.lower should already contain id.bounds.lower for
    each id in bounds1.lowertvars. **)
and edges_and_flows_to_t cx trace ?(opt=false) (id1, bounds1) t2 =
  if not (UseTypeMap.mem t2 bounds1.upper) then (
    edges_to_t cx trace ~opt (id1, bounds1) t2;
    flows_to_t cx trace bounds1.lower t2
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += t1
   for each u in bounds2.upper: t1 => u
*)
(** As an invariant, bounds2.upper should already contain id.bounds.upper for
    each id in bounds2.uppertvars. **)
and edges_and_flows_from_t cx trace ?(opt=false) t1 (id2, bounds2) =
  if not (TypeMap.mem t1 bounds2.lower) then (
    edges_from_t cx trace ~opt t1 (id2, bounds2);
    flows_from_t cx trace t1 bounds2.upper
  )

(* bounds.uppertvars += id *)
and add_uppertvar id trace bounds =
  bounds.uppertvars <- IMap.add id trace bounds.uppertvars

(* bounds.lowertvars += id *)
and add_lowertvar id trace bounds =
  bounds.lowertvars <- IMap.add id trace bounds.lowertvars

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.uppertvars += id2
*)
(** When going through bounds1.lowertvars, filter out id1. **)
(** As an optimization, skip id1 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_to_tvar cx trace ?(opt=false) (id1, bounds1) id2 =
  if not opt then add_uppertvar id2 trace bounds1;
  iter_with_filter cx bounds1.lowertvars id1 (fun (_, bounds) trace_l ->
    add_uppertvar id2 (Trace.concat_trace[trace_l;trace]) bounds
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lowertvars += id1
*)
(** When going through bounds2.uppertvars, filter out id2. **)
(** As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_from_tvar cx trace ?(opt=false) id1 (id2, bounds2) =
  if not opt then add_lowertvar id1 trace bounds2;
  iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) trace_u ->
    add_lowertvar id1 (Trace.concat_trace[trace;trace_u]) bounds
  )

(* for each id in id1 + bounds1.lowertvars:
   id.bounds.upper += bounds2.upper
   id.bounds.uppertvars += id2
   id.bounds.uppertvars += bounds2.uppertvars
*)
and add_upper_edges cx trace ?(opt=false) (id1, bounds1) (id2, bounds2) =
  edges_to_ts cx trace ~opt (id1, bounds1) bounds2.upper;
  edges_to_tvar cx trace ~opt (id1, bounds1) id2;
  iter_with_filter cx bounds2.uppertvars id2 (fun (tvar, _) trace_u ->
    let trace = Trace.concat_trace [trace;trace_u] in
    edges_to_tvar cx trace ~opt (id1, bounds1) tvar
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += bounds1.lower
   id.bounds.lowertvars += id1
   id.bounds.lowertvars += bounds1.lowertvars
*)
and add_lower_edges cx trace ?(opt=false) (id1, bounds1) (id2, bounds2) =
  edges_from_ts cx trace ~opt bounds1.lower (id2, bounds2);
  edges_from_tvar cx trace ~opt id1 (id2, bounds2);
  iter_with_filter cx bounds1.lowertvars id1 (fun (tvar, _) trace_l ->
    let trace = Trace.concat_trace [trace_l;trace] in
    edges_from_tvar cx trace ~opt tvar (id2, bounds2)
  )

(***************)
(* unification *)
(***************)

(* Chain a root to another root. If both roots are unresolved, this amounts to
   copying over the bounds of one root to another, and adding all the
   connections necessary when two non-unifiers flow to each other. If one or
   both of the roots are resolved, they effectively act like the corresponding
   concrete types. *)
and goto cx trace (id1, root1) (id2, root2) =
  replace_node cx id1 (Goto id2);
  (match root1.constraints, root2.constraints with

  | Unresolved bounds1, Unresolved bounds2 ->
    let cond1 = not_linked (id1, bounds1) (id2, bounds2) in
    let cond2 = not_linked (id2, bounds2) (id1, bounds1) in
    if cond1 then
      flows_across cx trace bounds1.lower bounds2.upper;
    if cond2 then
      flows_across cx trace bounds2.lower bounds1.upper;
    if cond1 then (
      add_upper_edges cx trace ~opt:true (id1, bounds1) (id2, bounds2);
      add_lower_edges cx trace (id1, bounds1) (id2, bounds2);
    );
    if cond2 then (
      add_upper_edges cx trace (id2, bounds2) (id1, bounds1);
      add_lower_edges cx trace ~opt:true (id2, bounds2) (id1, bounds1);
    );

  | Unresolved bounds1, Resolved t2 ->
    edges_and_flows_to_t cx trace ~opt:true (id1, bounds1) (UseT t2);
    edges_and_flows_from_t cx trace ~opt:true t2 (id1, bounds1);

  | Resolved t1, Unresolved bounds2 ->
    replace_node cx id2 (Root { root2 with constraints = Resolved t1 });
    edges_and_flows_to_t cx trace ~opt:true (id2, bounds2) (UseT t1);
    edges_and_flows_from_t cx trace ~opt:true t1 (id2, bounds2);

  | Resolved t1, Resolved t2 ->
    rec_unify cx trace t1 t2;
  )

(* Unify two type variables. This involves finding their roots, and making one
   point to the other. Ranks are used to keep chains short. *)
and merge_ids cx trace id1 id2 =
  let (id1, root1), (id2, root2) = find_root cx id1, find_root cx id2 in
  if id1 = id2 then ()
  else if root1.rank < root2.rank then goto cx trace (id1, root1) (id2, root2)
  else if root2.rank < root1.rank then goto cx trace (id2, root2) (id1, root1)
  else (
    replace_node cx id2 (Root { root2 with rank = root1.rank+1; });
    goto cx trace (id1, root1) (id2, root2);
  )

(* Resolve a type variable to a type. This involves finding its root, and
   resolving to that type. *)
and resolve_id cx trace id t =
  let id, root = find_root cx id in
  match root.constraints with
  | Unresolved bounds ->
    replace_node cx id (Root { root with constraints = Resolved t });
    edges_and_flows_to_t cx trace ~opt:true (id, bounds) (UseT t);
    edges_and_flows_from_t cx trace ~opt:true t (id, bounds);

  | Resolved t_ ->
    rec_unify cx trace t_ t

(******************)

(* Unification of two types *)

(* It is potentially dangerous to unify a type variable to a type that "forgets"
   constraints during propagation. These types are "any-like": the canonical
   example of such a type is any. Overall, we want unification to be a sound
   "optimization," in the sense that replacing bidirectional flows with
   unification should not miss errors. But consider a scenario where we have a
   type variable with two incoming flows, string and any, and two outgoing
   flows, number and any. If we replace the flows from/to any with an
   unification with any, we will miss the string/number incompatibility error.

   However, unifying with any-like types is sometimes desirable /
   intentional. Thus, we limit the set of types on which unification is banned
   to just AnyWithUpperBoundT and AnyWithLowerBoundT, which are internal types.
*)
and ok_unify = function
  | AnyWithUpperBoundT _ | AnyWithLowerBoundT _ -> false
  | _ -> true

and rec_unify cx trace t1 t2 =
  if t1 = t2 then () else (
  (* Expect that t1 and t2 are def types. *)

  not_expect_bound t1; not_expect_bound t2;

  match t1, t2 with
  | OpenT (_, id1), OpenT (_, id2) ->
    merge_ids cx trace id1 id2

  | OpenT (_, id), t | t, OpenT (_, id) when ok_unify t ->
    resolve_id cx trace id t

  | ArrT (_, t1, ts1), ArrT (_, t2, ts2) ->
    array_unify cx trace (ts1, t1, ts2, t2)

  | ObjT (lreason, { props_tmap = lflds; dict_t = ldict; _ }),
    ObjT (ureason, { props_tmap = uflds; dict_t = udict; _ }) ->

    (* ensure the keys and values are compatible with each other. *)
    begin match ldict, udict with
    | Some {key = lk; value = lv; _}, Some {key = uk; value = uv; _} ->
        dictionary cx trace lk lv udict;
        dictionary cx trace uk uv ldict
    | Some _, None ->
        let lreason = replace_reason "some property" lreason in
        flow_err_prop_not_found cx trace (lreason, ureason)
    | None, Some _ ->
        let ureason = replace_reason "some property" ureason in
        flow_err_prop_not_found cx trace (ureason, lreason)
    | None, None -> ()
    end;

    let lpmap, upmap = find_props cx lflds, find_props cx uflds in
    SMap.merge (fun x lt ut ->
      if not (is_internal_name x)
      then (match lt, ut with
      | Some t1, Some t2 -> rec_unify cx trace t1 t2
      | Some t1, None ->
          (* x exists in obj1 but not obj2; if obj2 is a dictionary make sure
             t1 is allowed, otherwise error *)
          flow_prop_to_dict cx trace x t1 udict lreason ureason
      | None, Some t2 ->
          (* x exists in obj2 but not obj1; if obj1 is a dictionary make sure
             t2 is allowed, otherwise error *)
          flow_prop_to_dict cx trace x t2 ldict ureason lreason
      | None, None -> ());
      None
    ) lpmap upmap |> ignore

  | _ ->
    naive_unify cx trace t1 t2
  )

(* TODO: Unification between concrete types is still implemented as
   bidirectional flows. This means that the destructuring work is duplicated,
   and we're missing some opportunities for nested unification. *)

and naive_unify cx trace t1 t2 =
  rec_flow_t cx trace (t1,t2); rec_flow_t cx trace (t2,t1)

and flow_prop_to_dict cx trace k v dict prop_reason dict_reason =
  match dict with
  | Some _ ->
    dictionary cx trace (string_key k prop_reason) v dict
  | None ->
    let prop_reason = replace_reason (spf "property `%s`" k) prop_reason in
    flow_err_prop_not_found cx trace (prop_reason, dict_reason)

(* mutable sites on parent values (i.e. object properties,
   array elements) must be typed invariantly when a value
   flows to the parent, unless the incoming value is fresh,
   in which case covariant typing is sound (since no alias
   will break if the subtyped child value is replaced by a
   non-subtyped value *)
and flow_to_mutable_child cx trace fresh t1 t2 =
  if fresh
  then rec_flow_t cx trace (t1, t2)
  else rec_unify cx trace t1 t2

and array_flow cx trace lit = function
  | [], e1, _, e2 ->
    (* general element1 = general element2 *)
    flow_to_mutable_child cx trace lit e1 e2

  | _, e1, [], e2 ->
    (* specific element1 < general element2 *)
    rec_flow_t cx trace (e1, e2)

  | [t1], _, t2 :: _, _ ->
    (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace lit t1 t2

  | t1 :: ts1, e1, t2 :: ts2, e2 ->
    (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace lit t1 t2;
    array_flow cx trace lit (ts1,e1, ts2,e2)

(* array helper *)
and array_unify cx trace = function
  | [], e1, [], e2 ->
    (* general element1 = general element2 *)
    rec_unify cx trace e1 e2

  | ts1, _, [], e2
  | [], e2, ts1, _ ->
    (* specific element1 < general element2 *)
    List.iter (fun t1 -> rec_unify cx trace t1 e2) ts1

  | t1 :: ts1, e1, t2 :: ts2, e2 ->
    (* specific element1 = specific element2 *)
    rec_unify cx trace t1 t2;
    array_unify cx trace (ts1, e1, ts2, e2)


(*******************************************************************)
(* subtyping a sequence of arguments with a sequence of parameters *)
(*******************************************************************)

and multiflow cx trace reason_op (arglist, parlist) =
  multiflow_partial cx trace ~strict:reason_op (arglist, parlist) |> ignore

(* Match arguments to parameters, taking an optional parameter 'strict':
   - when strict=None, missing arguments (and unmatched parameters) are
   expected. This is used, e.g., when processing Function.prototype.bind.
   - when strict=Some reason_op, missing arguments are treated as undefined,
   whose types are given a reason derived from reason_op when passing to
   unmatched parameters.
*)
and multiflow_partial cx trace ?strict = function
  (* Do not complain on too many arguments.
     This pattern is ubiqutous and causes a lot of noise when complained about.
     Note: optional/rest parameters do not provide a workaround in this case.
  *)
  | (_, []) -> []

  | ([RestT tin], [RestT tout]) ->
    rec_flow_t cx trace (tin, tout);
    []

  | ([RestT tin], tout::touts) ->
    rec_flow_t cx trace (tin, tout);
    multiflow_partial cx trace ?strict ([RestT tin], touts)

  | (tin::tins, [RestT tout]) ->
    rec_flow_t cx trace (tin, tout);
    multiflow_partial cx trace ?strict (tins, [RestT tout])

  | ([], [RestT tout]) -> [RestT tout]

  | ([], tout::touts) ->
    (match strict with
    | Some reason_op ->
        let reason = replace_reason
          "undefined (too few arguments, expected default/rest parameters)"
          reason_op
        in
        rec_flow_t cx trace (VoidT reason, tout);
        multiflow_partial cx trace ?strict ([], touts)
    | None ->
        tout::touts
    );

  | (tin::tins, tout::touts) ->
    (* flow `tin` (argument) to `tout` (param). normally, `tin` is passed
       through a `ReposLowerT` to make sure that the concrete type points at
       the arg's location. however, if `tin` is an implicit type argument
       (e.g. the `x` in `function foo<T>(x: T)`), then don't reposition it
       because implicit type args have no explicit location to point at.
       instead, let it flow through transparently, so that we point at the
       place that constrained the type arg. this is pretty hacky. *)
    let tout =
      let u = UseT tout in
      if ImplicitTypeArgument.has_typeparam_prefix (desc_of_t tin)
      then u
      else ReposLowerT (reason_of_t tin, u)
    in
    rec_flow cx trace (tin, tout);
    multiflow_partial cx trace ?strict (tins,touts)

and dictionary cx trace keyt valuet = function
  | None -> ()
  | Some { key; value; _ } ->
      rec_flow_t cx trace (keyt, key);
      begin match keyt with
      | StrT (_, Literal str) ->
        if not (is_dictionary_exempt str)
        then rec_flow_t cx trace (valuet, value)
      | _ ->
        rec_flow_t cx trace (valuet, value)
      end

and string_key s reason =
  let key_reason =
    replace_reason (spf "property `%s` is a string" s) reason in
  StrT (key_reason, Literal s)

(* builtins, contd. *)

and get_builtin cx x reason =
  mk_tvar_where cx reason (fun builtin ->
    flow_opt cx (builtins cx, GetPropT(reason, (reason, x), builtin))
  )

and lookup_builtin cx x reason strict builtin =
  flow_opt cx (builtins cx, LookupT(reason,strict,[],x,builtin))

and get_builtin_typeapp cx reason x ts =
  TypeAppT(get_builtin cx x reason, ts)

(* Specialize a polymorphic class, make an instance of the specialized class. *)
and mk_typeapp_instance cx reason ?(cache=false) c ts =
  let t = mk_tvar cx reason in
  flow_opt cx (c, SpecializeT(reason,cache,ts,t));
  mk_instance cx (reason_of_t c) t

(* NOTE: the for_type flag is true when expecting a type (e.g., when processing
   an annotation), and false when expecting a runtime value (e.g., when
   processing an extends). *)
and mk_instance cx instance_reason ?(for_type=true) c =
  if for_type then
    (* Make an annotation. Part of this operation is similar to making
       a runtime value type (see below), except that for annotations,
       we must ensure that values flowing into the annotated site do
       not interact with downstream uses of it. *)

    (* mental picture:

      (incoming values)
                  |
                  | <:
                  v
      AnnotT (  sink_t (tvar),
                  ^
                  |
                  | =: (deferred unification, see note)
                  |
                  |   (type from annotation expression)
                  |      /
                  |     / <:
                  v    v
                source_t (tvar) )
                  |
                  | <:
                  v
                ( downstream sites )

      Note: source and sink are not unified until the
      type derived from the annotation expression has
      become concrete. Because the annotation expr type
      is the unique LB of source, this has the effect
      of constraining inflows to sink correctly, without
      forwarding them on to source (and thus onward to
      sites which the annotation is intended to protect).
     *)

    let source_t = mk_tvar_where cx instance_reason (fun t ->
      (* this part is similar to making a runtime value *)
      flow_opt_t cx (c, TypeT(instance_reason,t))
    ) in
    let sink_t = mk_tvar_where cx instance_reason (fun t ->
      (* when source_t is concrete, unify with sink_t *)
      flow_opt cx (source_t, UnifyT(source_t, t))
    ) in
    AnnotT (sink_t, source_t)
  else
    mk_tvar_derivable_where cx instance_reason (fun t ->
      flow_opt_t cx (c, ClassT(t))
    )

(* We want to match against some t, which may either be a concrete
   type, or a tvar that will be concretized by a single incoming
   lower bound (for instance, a tvar representing an externally
   defined type - the concrete definition will appear as a lower
   bound to that tvar at merge time).

   Given such a t, `become` will return
   * t itself, if it is concrete already, or
   * a new tvar which will behave exactly like the first concrete
    lower bound that flows into t, whenever that happens.
 *)
and become cx ?trace r t = match t with
  | OpenT _ ->
    (* if t is not concrete, create a new tvar within a BecomeT
       operation and add it to t's uppertvars. When a concrete
       lower bound flows to t, it will also flow to this BecomeT,
       and the rule for BecomeT in __flow will unify the new tvar
       with it. *)
    mk_tvar_derivable_where cx r (fun tvar ->
      flow_opt cx ?trace (t, BecomeT (r, tvar)))
  | _ ->
    (* optimization: if t is already concrete, become t immediately :) *)
    t

(* set the position of the given def type from a reason *)
and reposition cx ?trace reason t =
  match t with
  | OpenT (r, id) ->
    let constraints = find_graph cx id in
    begin match constraints with
    | Resolved t -> mod_reason_of_t (repos_reason (loc_of_reason reason)) t
    | _ ->
      let mk_tvar_where = if is_derivable_reason r
        then mk_tvar_derivable_where
        else mk_tvar_where
      in
      mk_tvar_where cx reason (fun tvar ->
        flow_opt cx ?trace (t, ReposLowerT (reason, UseT tvar))
      )
    end
  | EvalT _ ->
      (* Modifying the reason of `EvalT`, as we do for other types, is not
         enough, since it will only affect the reason of the resulting tvar.
         Instead, repositioning a `EvalT` should simulate repositioning the
         resulting tvar, i.e., flowing repositioned *lower bounds* to the
         resulting tvar. (Another way of thinking about this is that a `EvalT`
         is just as transparent as its resulting tvar.) *)
      mk_tvar_where cx reason (fun tvar ->
        flow_opt cx ?trace (t, ReposLowerT (reason, UseT tvar))
      )
  | _ -> mod_reason_of_t (repos_reason (loc_of_reason reason)) t

(* set the position of the given use type from a reason *)
and reposition_use cx ?trace reason t = match t with
  | UseT t -> UseT (reposition cx ?trace reason t)
  | _ -> mod_reason_of_use_t (repos_reason (loc_of_reason reason)) t

(* given the type of a value v, return the type term
   representing the `typeof v` annotation expression *)
and mk_typeof_annotation cx ?trace valtype =
  let r = prefix_reason "typeof " (reason_of_t valtype) in
  become cx ?trace r valtype

and get_builtin_type cx reason x =
  let t = get_builtin cx x reason in
  mk_instance cx reason t

and instantiate_poly_t cx t types =
  if types = [] then (* nothing to do *) t else
  match t with
  | PolyT (type_params, t_) -> (
    try
      let subst_map = List.fold_left2 (fun acc {name; _} type_ ->
        SMap.add name type_ acc
      ) SMap.empty type_params types in
      subst cx subst_map t_
    with _ ->
      prerr_endline "Instantiating poly type failed";
      t
  )
  | _ ->
    assert_false "unexpected args passed to instantiate_poly_t"

and instantiate_type t =
  match t with
  | ThisClassT t | ClassT t -> t
  | _ -> AnyT.why (reason_of_t t) (* ideally, assert false *)

(** TODO: this should rather be moved close to ground_type_impl/resolve_type
    etc. but Ocaml name resolution rules make that require a lot more moving
    code around. **)
and resolve_builtin_class cx = function
  | BoolT (reason, _) ->
    let bool_t = get_builtin_type cx reason "Boolean" in
    resolve_type cx bool_t
  | NumT (reason, _) ->
    let num_t = get_builtin_type cx reason "Number" in
    resolve_type cx num_t
  | StrT (reason, _) ->
    let string_t = get_builtin_type cx reason "String" in
    resolve_type cx string_t
  | ArrT (reason, t, _) ->
    let array_t = get_builtin cx "Array" reason in
    let array_t = resolve_type cx array_t in
    let array_t = instantiate_poly_t cx array_t [t] in
    instantiate_type array_t
  | t ->
    t

and set_builtin cx x t =
  let reason = builtin_reason x in
  flow_opt cx (builtins cx, SetPropT(reason, (reason, x), t))

(* Wrapper functions around __flow that manage traces. Use these functions for
   all recursive calls in the implementation of __flow. *)

(* Call __flow while concatenating traces. Typically this is used in code that
   propagates bounds across type variables, where nothing interesting is going
   on other than concatenating subtraces to make longer traces to describe
   transitive data flows *)
and join_flow cx ts (t1, t2) =
  __flow cx (t1, t2) (Trace.concat_trace ts)

(* Call __flow while embedding traces. Typically this is used in code that
   simplifies a constraint to generate subconstraints: the current trace is
   "pushed" when recursing into the subconstraints, so that when we finally hit
   an error and walk back, we can know why the particular constraints that
   caused the immediate error were generated. *)
and rec_flow cx trace (t1, t2) =
  let max = Context.max_trace_depth cx in
  __flow cx (t1, t2) (Trace.rec_trace ~max t1 t2 trace)

and rec_flow_t cx trace (t1, t2) =
  rec_flow cx trace (t1, UseT t2)

(* Ideally this function would not be required: either we call `flow` from
   outside without a trace (see below), or we call one of the functions above
   with a trace. However, there are some functions that need to call __flow,
   which are themselves called both from outside and inside (with or without
   traces), so they call this function instead. *)
and flow_opt cx ?trace (t1, t2) =
  let trace = match trace with
    | None -> Trace.unit_trace t1 t2
    | Some trace ->
        let max = Context.max_trace_depth cx in
        Trace.rec_trace ~max t1 t2 trace in
  __flow cx (t1, t2) trace

and flow_opt_t cx ?trace (t1, t2) =
  flow_opt cx ?trace (t1, UseT t2)

(* Externally visible function for subtyping. *)
(* Calls internal entry point and traps runaway recursion. *)
and flow cx (lower, upper) =
  try
    flow_opt cx (lower, upper)
  with
  | RecursionCheck.LimitExceeded trace ->
    (* log and continue *)
    let msg = "*** Recursion limit exceeded ***" in
    flow_err cx trace msg lower upper
  | ex ->
    (* rethrow *)
    raise ex

and flow_t cx (t1, t2) =
  flow cx (t1, UseT t2)

and tvar_with_constraint cx u =
  let reason = reason_of_use_t u in
  mk_tvar_where cx reason (fun tvar ->
    flow cx (tvar, u)
  )

(************* end of slab **************************************************)

(* Externally visible function for unification. *)
let unify cx t1 t2 =
  rec_unify cx (Trace.unit_trace t1 (UseT t2)) t1 t2

let intersect_members cx members =
  match members with
  | [] -> SMap.empty
  | _ ->
      let map = SMap.map (fun x -> [x]) (List.hd members) in
      let map = List.fold_left (fun acc x ->
          SMap.merge (fun _ tl t ->
              match (tl, t) with
              | (None, None)      -> None
              | (None, Some _)    -> None
              | (Some _, None)    -> None
              | (Some tl, Some t) -> Some (t :: tl)
            ) acc x
        ) map (List.tl members) in
      SMap.map (List.fold_left (fun acc x ->
          merge_type cx (acc, x)
        ) EmptyT.t) map

(* It's kind of lame that Autocomplete is in this module, but it uses a bunch
 * of internal APIs so for now it's easier to keep it here than to expose those
 * APIs *)
module Autocomplete : sig
  type member_result =
    | Success of Type.t SMap.t
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  val command_result_of_member_result: member_result ->
    (Type.t SMap.t, string) ok_or_err

  val extract_members: Context.t -> Type.t -> member_result

end = struct

  type member_result =
    | Success of Type.t SMap.t
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  let command_result_of_member_result = function
    | Success map ->
        OK map
    | FailureMaybeType ->
        Err "autocomplete on possibly null or undefined value"
    | FailureAnyType ->
        Err "not enough type information to autocomplete"
    | FailureUnhandledType _ ->
        Err "autocomplete on unexpected type of value (please file a task!)"

  let find_props cx fields =
    SMap.filter (fun key _ ->
      (* Filter out keys that start with "$" *)
      not (String.length key >= 1 && key.[0] = '$')
    ) (find_props cx fields)

  (* TODO: Think of a better place to put this *)
  let rec extract_members cx this_t =
    match this_t with
    | MaybeT _ | NullT _ | VoidT _ ->
        FailureMaybeType
    | AnyT _ ->
        FailureAnyType
    | AnyObjT reason ->
        extract_members cx (get_builtin_type cx reason "Object")
    | AnyFunT reason ->
        extract_members cx (IntersectionT (reason, InterRep.make [
          get_builtin_type cx reason "Function";
          get_builtin_type cx reason "Object";
        ]))
    | AnnotT (source, _) ->
        let source_t = resolve_type cx source in
        extract_members cx source_t
    | InstanceT (_, _, super,
                {fields_tmap = fields;
                methods_tmap = methods;
                _}) ->
        let fields = find_props cx fields in
        let methods = find_props cx methods in
        let super_t = resolve_type cx super in
        let members = SMap.union fields methods in
        let super_flds = extract_members_as_map cx super_t in
        Success (SMap.union super_flds members)
    | ObjT (_, {props_tmap = flds; proto_t = proto; _}) ->
        let proto_reason = reason_of_t proto in
        let proto_t = resolve_type cx (IntersectionT (proto_reason,
          InterRep.make [
            proto;
            get_builtin_type cx proto_reason "Object";
        ])) in
        let prot_members = extract_members_as_map cx proto_t in
        let members = find_props cx flds in
        Success (SMap.union prot_members members)
    | ThisTypeAppT (c, _, ts)
    | TypeAppT (c, ts) ->
        let c = resolve_type cx c in
        let inst_t = instantiate_poly_t cx c ts in
        let inst_t = instantiate_type inst_t in
        extract_members cx inst_t
    | PolyT (_, sub_type) ->
        (* TODO: replace type parameters with stable/proper names? *)
        extract_members cx sub_type
    | ThisClassT (InstanceT (_, static, _, _))
    | ClassT (InstanceT (_, static, _, _)) ->
        let static_t = resolve_type cx static in
        extract_members cx static_t
    | FunT (_, static, proto, _) ->
        let static_t = resolve_type cx static in
        let proto_t = resolve_type cx proto in
        let members = extract_members_as_map cx static_t in
        let prot_members = extract_members_as_map cx proto_t in
        Success (SMap.union prot_members members)
    | IntersectionT (_, rep) ->
        (* Intersection type should autocomplete for every property of every type
        * in the intersection *)
        let ts = InterRep.members rep in
        let ts = List.map (resolve_type cx) ts in
        let members = List.map (extract_members_as_map cx) ts in
        Success (List.fold_left SMap.union SMap.empty members)
    | UnionT (_, rep) ->
        (* Union type should autocomplete for only the properties that are in
        * every type in the intersection *)
        let ts = List.map (resolve_type cx) (UnionRep.members rep) in
        let members = ts
          (* Although we'll ignore the any-ish members of the union *)
          |> List.filter (function AnyT _ | AnyObjT _ | AnyFunT _ -> false | _ -> true)
          |> List.map (extract_members_as_map cx)
          |> intersect_members cx in
        Success members
    | SingletonStrT (reason, _)
    | StrT (reason, _) ->
        extract_members cx (get_builtin_type cx reason "String")
    | SingletonNumT (reason, _)
    | NumT (reason, _) ->
        extract_members cx (get_builtin_type cx reason "Number")
    | SingletonBoolT (reason, _)
    | BoolT (reason, _) ->
        extract_members cx (get_builtin_type cx reason "Boolean")
    | _ ->
        (* TODO: What types could come up here which we need to handle? *)
        FailureUnhandledType this_t

  and extract_members_as_map cx this_t =
    let member_result = extract_members cx this_t in
    match command_result_of_member_result member_result with
    | OK map -> map
    | Err _ -> SMap.empty

end

(* Given a type, report missing annotation errors if

   - the given type is a tvar whose id isn't explicitly specified in the given
   skip set, or isn't explicitly marked as derivable, or if

   - the infer flag is true, and such tvars are reachable from the given tvar

   Type variables that are in the skip set are marked in assume_ground as
   depending on `require`d modules. Thus, e.g., when the superclass of an
   exported class is `require`d, we should not insist on an annotation for the
   superclass.
*)
(* need to consider only "def" types *)
let rec assert_ground ?(infer=false) cx skip ids t =
  let recurse ?infer = assert_ground ?infer cx skip ids in
  match t with
  | BoundT _ ->
    ()

  (* Type variables that are not forced to be annotated include those that
     are dependent on requires, or whose reasons indicate that they are
     derivable. The latter category includes annotations and builtins. *)
  | OpenT (reason_open, id)
    when (ISet.mem id skip || is_derivable_reason reason_open) ->
    ()

  (* when the infer flag is set, traverse the types reachable from this tvar,
     rather than stopping here and reporting a missing annotation. Note that
     when this function is called recursively on those types, infer will be
     false. *)
  | OpenT (_, id) when infer ->
    assert_ground_id cx skip ids id

  | OpenT (reason_open, id) ->
    unify cx (OpenT (reason_open, id)) AnyT.t;
    add_error cx (mk_info reason_open ["Missing annotation"])

  | NumT _
  | StrT _
  | BoolT _
  | EmptyT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _
  | TaintT _ ->
    ()

  | FunT (_, static, prototype, { this_t; params_tlist; return_t; _ }) ->
    unify cx static AnyT.t;
    unify cx prototype AnyT.t;
    unify cx this_t AnyT.t;
    List.iter recurse params_tlist;
    recurse ~infer:true return_t

  | PolyT (_, t)
  | ThisClassT t ->
    recurse t

  | ObjT (_, { props_tmap = id; proto_t; _ }) ->
    unify cx proto_t AnyT.t;
    iter_props cx id (fun _ -> assert_ground ~infer:true cx skip ids)

  | ArrT (_, t, ts) ->
    recurse t;
    List.iter recurse ts

  | ClassT t
  | TypeT (_, t) ->
    recurse t

  | InstanceT (_, static, super, instance) ->
    let process_element name t =
      let infer = is_munged_prop_name cx name in
      recurse ~infer t
    in
    iter_props cx instance.fields_tmap process_element;
    iter_props cx instance.methods_tmap process_element;
    unify cx static AnyT.t;
    recurse super

  | RestT t
  | OptionalT t ->
    recurse t

  | TypeAppT (c, ts) ->
    recurse ~infer:true c;
    List.iter recurse ts

  | ThisTypeAppT (c, this, ts) ->
    recurse ~infer:true c;
    recurse ~infer:true this;
    List.iter recurse ts

  | MaybeT t ->
    recurse t

  | IntersectionT (_, rep) ->
    List.iter recurse (InterRep.members rep)

  | UnionT (_, rep) ->
    List.iter recurse (UnionRep.members rep)

  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t ->
    recurse t

  | AnyObjT _
  | AnyFunT _ ->
    ()

  | ShapeT t ->
    recurse t

  | DiffT (t1, t2) ->
    recurse t1;
    recurse t2

  | KeysT (_, t) ->
    recurse t

  | SingletonStrT _
  | SingletonNumT _
  | SingletonBoolT _ ->
    ()

  | ModuleT (_, { exports_tmap; cjs_export }) ->
    iter_props cx exports_tmap (fun _ -> recurse ~infer:true);
    begin match cjs_export with
    | Some t -> recurse ~infer:true t
    | None -> ()
    end

  | AnnotT _ ->
    (* don't ask for an annotation if one is already provided :) *)
    (** TODO: one of the uses of derivable_reason was to mark type variables
        that represented annotations so that they could be ignored. Since we
        can now ignore annotations directly, consider renaming or getting rid
        of derivable entirely. **)
    ()

  | ExistsT _ ->
    ()

  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | AbstractT _
  | EvalT _
  | SpeculativeMatchT _
  | ReposUpperT _
  | ExtendsT _
  | CustomFunT _ ->
    () (* TODO *)

and assert_ground_id cx skip ids id =
  if not (ISet.mem id !ids)
  then (
    ids := !ids |> ISet.add id;
    match find_graph cx id with
    | Unresolved { lower; lowertvars; _ } ->
        TypeMap.keys lower |> List.iter (assert_ground cx skip ids);
        IMap.keys lowertvars |> List.iter (assert_ground_id cx skip ids);
    | Resolved t ->
        assert_ground cx skip ids t
  )

let enforce_strict cx id =
  (* First, compute a set of ids to be skipped by calling `assume_ground`. After
     the call, skip_ids contains precisely those ids that correspond to
     requires/imports. *)
  let skip_ids = ref ISet.empty in
  SSet.iter (fun r ->
    let tvar = lookup_module cx r in
    assume_ground cx skip_ids (UseT tvar)
  ) (Context.required cx);

  (* With the computed skip_ids, call `assert_ground` to force annotations while
     walking the graph starting from id. Typically, id corresponds to
     exports. *)
  assert_ground_id cx !skip_ids (ref ISet.empty) id
