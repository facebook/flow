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
open Reason
open Constraint
open Type

module FlowError = Flow_error
module Ops = FlowError.Ops

(* The following functions are used as constructors for function types and
   object types, which unfortunately have many fields, not all of which are
   meaningful in all contexts. This part of the design should be revisited:
   perhaps the data types can be refactored to make them more specialized. *)

(* Methods may use a dummy statics object type to carry properties. We do not
   want to encourage this pattern, but we also don't want to block uses of this
   pattern. Thus, we compromise by not tracking the property types. *)
let dummy_static reason =
  AnyFunT (replace_reason (fun desc -> RStatics desc) reason)

let dummy_prototype =
  ObjProtoT (locationless_reason RDummyPrototype)

let dummy_this =
  let reason = locationless_reason RDummyThis in
  AnyT reason

let global_this =
  let reason = locationless_reason (RCustom "global object") in
  ObjProtoT reason

(* A method type is a function type with `this` specified. *)
let mk_methodtype
    this tins ?(frame=0) ?params_names ?(is_predicate=false) tout = {
  this_t = this;
  params_tlist = tins;
  params_names;
  return_t = tout;
  is_predicate;
  closure_t = frame;
  changeset = Changeset.empty
}

(* A bound function type is a function type with `this` = `any`. Typically, such
   a type is given to a method when it can be considered bound: in other words,
   when calling that method through any object would be fine, since the object
   would be ignored. *)
let mk_boundfunctiontype = mk_methodtype dummy_this

(* A function type has `this` = `mixed`. Such a type can be given to functions
   that are meant to be called directly. On the other hand, it deliberately
   causes problems when they are given to methods in which `this` is used
   non-trivially: indeed, calling them directly would cause `this` to be bound
   to the global object, which is typically unintended. *)
let mk_functiontype = mk_methodtype global_this

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
  Context.add_tvar cx tvar (Constraint.new_unresolved_root ());
  if Context.output_graphml cx then
    (* only need to remember tvar -> reason for diagnostics *)
    Context.add_tvar_reason cx tvar reason;
  (if Context.is_verbose cx then prerr_endlinef
    "TVAR %d (%d): %s" tvar (IMap.cardinal graph)
    (Debug_js.string_of_reason cx reason));
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
   Constraint for details.) If the type variable is a root, the
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
        (Debug_js.string_of_file cx)
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

(* note: this is here instead of Env because of circular deps:
  Env is downstream of Flow_js due general utility funcs such as
  Flow_js.mk_tvar and builtins services. If the flow algorithm can
  be split away from these, then Env can be moved upstream and
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
      prerr_endlinef "%shavoc_entry %s %s"
        (Context.pid_prefix cx)
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
      prerr_endlinef "%shavoc_refi %s"
        (Context.pid_prefix cx)
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
        (String.concat ", " (Key_map.fold (
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
  |> List.filter is_proper_def

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let rec list_map2 f ts1 ts2 = match (ts1,ts2) with
  | ([],_) | (_,[]) -> []
  | (t1::ts1,t2::ts2) -> (f (t1,t2)):: (list_map2 f ts1 ts2)

let rec merge_type cx =
  let create_union rep =
    UnionT (locationless_reason (RCustom "union"), rep)
  in

  let create_intersection rep =
    IntersectionT (locationless_reason (RCustom "intersection"), rep)
  in

  function
  | (NumT _, (NumT _ as t))
  | (StrT _, (StrT _ as t))
  | (BoolT _, (BoolT _ as t))
  | (NullT _, (NullT _ as t))
  | (VoidT _, (VoidT _ as t))
  | (TaintT _, ((TaintT _) as t))
  | (ObjProtoT _, (ObjProtoT _ as t))
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
      let reason = locationless_reason (RCustom "function") in
      FunT (
        reason,
        dummy_static reason,
        dummy_prototype,
        mk_functiontype tins tout
      )

  | (ObjT (_,o1) as t1), (ObjT (_,o2) as t2) ->
    let map1 = Context.find_props cx o1.props_tmap in
    let map2 = Context.find_props cx o2.props_tmap in

    (* Create an intermediate map of booleans indicating whether two objects can
     * be merged, based on the properties in each map. *)
    let merge_map = SMap.merge (fun _ p1_opt p2_opt ->
      match p1_opt, p2_opt with
      | None, None -> None
      (* In general, even objects with disjoint key sets can not be merged due
       * to width subtyping. For example, {x:T} and {y:U} is not the same as
       * {x:T,y:U}, because {x,y} is a valid inhabitant of {x:T} and the type of
       * y may != U. However, if either object type is exact, disjointness is
       * sufficient. *)
      | Some _, None | None, Some _ -> Some (o1.flags.exact || o2.flags.exact)
      (* Covariant fields can be merged. *)
      | Some (Field (_, Positive)), Some (Field (_, Positive)) -> Some true
      (* Getters are covariant and thus can be merged. *)
      | Some (Get _), Some (Get _) -> Some true
      (* Anything else is can't be merged. *)
      | _ -> Some false
    ) map1 map2 in

    let merge_dict = match o1.dict_t, o2.dict_t with
    (* If neither object has an indexer, neither will the merged object. *)
    | None, None -> Some None
    (* If both objects covariant indexers, we can merge them. However, if the
     * key types are disjoint, the resulting dictionary is not useful. *)
    | Some {key = k1; value = v1; dict_polarity = Positive; _},
      Some {key = k2; value = v2; dict_polarity = Positive; _} ->
      (* TODO: How to merge indexer names? *)
      Some (Some {
        dict_name = None;
        key = create_intersection (InterRep.make k1 k2 []);
        value = merge_type cx (v1, v2);
        dict_polarity = Positive;
      })
    (* Don't merge objects with possibly incompatible indexers. *)
    | _ -> None
    in

    (* Only merge objects if every property can be merged. *)
    let should_merge = SMap.for_all (fun _ x -> x) merge_map in

    (* Don't merge objects with different prototypes. *)
    let should_merge = should_merge && o1.proto_t = o2.proto_t in

    (match should_merge, merge_dict with
    | true, Some dict ->
      let map = SMap.merge (fun _ p1_opt p2_opt ->
        match p1_opt, p2_opt with
        (* Merge disjoint+exact objects. *)
        | Some t, None
        | None, Some t -> Some t
        (* Shouldn't happen, per merge_map above. *)
        | _ -> None
      ) map1 map2 in
      let id = Context.make_property_map cx map in
      let sealed = match o1.flags.sealed, o2.flags.sealed with
      | Sealed, Sealed -> Sealed
      | UnsealedInFile s1, UnsealedInFile s2 when s1 = s2 -> UnsealedInFile s1
      | _ -> UnsealedInFile None
      in
      let flags = {
        sealed;
        exact = o1.flags.exact && o2.flags.exact;
        frozen = o1.flags.frozen && o2.flags.frozen;
      } in
      let objtype = mk_objecttype ~flags dict id o1.proto_t in
      ObjT (locationless_reason (RCustom "object"), objtype)
    | _ ->
      create_union (UnionRep.make t1 t2 []))

  | (ArrT (_,t1,ts1), ArrT (_,t2,ts2)) ->
      ArrT (
        locationless_reason (RCustom "array"),
        merge_type cx (t1, t2),
        list_map2 (merge_type cx) ts1 ts2
      )

  | (MaybeT t1, MaybeT t2) ->
      MaybeT (merge_type cx (t1, t2))

  | (MaybeT t1, t2)
  | (t1, MaybeT t2) ->
      MaybeT (merge_type cx (t1, t2))

  | UnionT (_, rep1), UnionT (_, rep2) ->
      create_union (UnionRep.rev_append rep1 rep2)

  | (UnionT (_, rep), t)
  | (t, UnionT (_, rep)) ->
      create_union (UnionRep.cons t rep)

  (* TODO: do we need to do anything special for merging Null with Void,
     Optional with other types, etc.? *)

  | (t1, t2) ->
      create_union (UnionRep.make t1 t2 [])

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

let rec assume_ground cx ?(depth=1) ids t =
  begin match Context.verbose cx with
  | Some { Verbose.depth = verbose_depth; indent; } ->
    let pid = Context.pid_prefix cx in
    let indent = String.make ((depth - 1) * indent) ' ' in
    prerr_endlinef "\n%s%sassume_ground: %s"
      indent pid (Debug_js.dump_use_t cx ~depth:verbose_depth t)
  | None -> ()
  end;
  begin match t with
  | UseT (_, OpenT(_,id)) ->
    assume_ground_id ~depth:(depth + 1) cx ids id

  (** The subset of operations to crawl. The type variables denoting the
      results of these operations would be ignored by the is_required check in
     `assert_ground`.

     These are intended to be exactly the operations that might be involved
     when extracting (parts of) requires/imports. As such, they need to be
     kept in sync as module system conventions evolve. *)

  | ReposLowerT (_, use_t) ->
    assume_ground cx ~depth:(depth + 1) ids use_t

  | ImportModuleNsT (_, t)
  | CJSRequireT (_, t)
  | ImportTypeT (_, _, t)
  | ImportTypeofT (_, _, t)

  (** Other common operations that might happen immediately after extracting
      (parts of) requires/imports. *)

  | GetPropT (_, _, t)
  | CallT (_, { return_t = t; _ })
  | MethodT (_, _, _, { return_t = t; _ })
  | ConstructorT (_, _, t) ->
    assume_ground cx ~depth:(depth + 1) ids (UseT (UnknownUse, t))

  | _ -> ()
  end;
  if Context.is_verbose cx then
    let pid = Context.pid_prefix cx in
    if depth = 1 then
      prerr_endlinef "\n%sAssumed ground: %s"
        pid
        (!ids |> ISet.elements |> List.map string_of_int |> String.concat ", ")

and assume_ground_id cx ~depth ids id =
  if not (ISet.mem id !ids) then (
    ids := !ids |> ISet.add id;
    let constraints = find_graph cx id in
    match constraints with
    | Unresolved { upper; uppertvars; _ } ->
      upper |> UseTypeMap.iter (fun t _ ->
        assume_ground cx ~depth ids t
      );
      uppertvars |> IMap.iter (fun id _ ->
        assume_ground_id cx ~depth ids id
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
  let builtins = mk_tvar cx (builtin_reason (RCustom "module")) in
  Context.add_module cx Files.lib_module builtins

(* Local references to modules can be looked up. *)
let lookup_module cx m = Context.find_module cx m

(* The builtins reference is accessed just like references to other modules. *)
let builtins cx =
  lookup_module cx Files.lib_module

let restore_builtins cx b =
  Context.add_module cx Files.lib_module b

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
  (* Make a type argument for a given type parameter, given a reason. Note that
     not all type arguments are tvars; the following function is used only when
     polymorphic types need to be implicitly instantiated, because there was no
     explicit instantiation (via a type application), or when we want to cache a
     unique instantiation and unify it with other explicit instantiations. *)
  let mk_targ cx (typeparam, reason_op) =
    let reason = replace_reason (fun desc ->
      RTypeParam (typeparam.name, desc)
    ) reason_op in
    mk_tvar cx reason

  (* Abstract a type argument that is created by implicit instantiation
     above. Sometimes, these type arguments are involved in type expansion
     loops, so we abstract them to detect such loops. *)
  let abstract_targ tvar =
    let reason, _ = open_tvar tvar in
    let desc = desc_of_reason reason in
    match desc with
    | RTypeParam _ -> Some (OpenT (locationless_reason desc, 0))
    | _ -> None
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

  let string_of_desc_of_t t = string_of_desc (desc_of_t t)

  (* show entries in the stack *)
  let show_entry (c, tss) =
    spf "%s<%s>" (string_of_desc_of_t c) (
      string_of_list tss "," (fun ts ->
        let ts = TypeSet.elements ts in
        spf "[%s]" (string_of_list ts ";" string_of_desc_of_t)
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

module Cache = struct

  type type_pair = TypeTerm.t * TypeTerm.use_t
  module TypePairSet : Set.S with type elt = type_pair =
  Set.Make (struct
    type t = type_pair
    let compare = Pervasives.compare
  end)

  (* Cache that remembers pairs of types that are passed to __flow. *)
  module FlowConstraint = struct
    let cache = ref TypePairSet.empty

    (* attempt to read LB/UB pair from cache, add if absent *)
    let get cx (l, u) = match l, u with
      (* Don't cache constraints involving type variables, since the
         corresponding typing rules are already sufficiently robust. *)
      | OpenT _, _ | _, UseT (_, OpenT _) -> false
      | _ ->
        if TypePairSet.mem (l, u) !cache then begin
          if Context.is_verbose cx then
            prerr_endlinef "%sFlowConstraint cache hit on (%s, %s)"
              (Context.pid_prefix cx)
              (string_of_ctor l) (string_of_use_ctor u);
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

  let repos_cache = ref Repos_cache.empty

  (* Cache that records sentinel properties for objects. Cache entries are
     populated before checking against a union of object types, and are used
     while checking against each object type in the union. *)
  module SentinelProp = struct
    let cache = ref Properties.Map.empty

    let add id more_keys =
      match Properties.Map.get id !cache with
      | Some keys ->
        cache := Properties.Map.add id (SSet.union keys more_keys) !cache
      | None ->
        cache := Properties.Map.add id more_keys !cache

    let ordered_iter id f map =
      let map = match Properties.Map.get id !cache with
        | Some keys ->
          SSet.fold (fun s map ->
            match SMap.get s map with
            | Some t -> f s t; SMap.remove s map
            | None -> map
          ) keys map
        | _ -> map in
      SMap.iter f map

  end

  let clear () =
    FlowConstraint.cache := TypePairSet.empty;
    Hashtbl.clear PolyInstantiation.cache;
    repos_cache := Repos_cache.empty;
    SentinelProp.cache := Properties.Map.empty

  let stats_poly_instantiation () =
    Hashtbl.stats PolyInstantiation.cache

  (* debug util: please don't dead-code-eliminate *)
  (* Summarize flow constraints in cache as ctor/reason pairs, and return counts
     for each group. *)
  let summarize_flow_constraint () =
    let group_counts = TypePairSet.fold (fun (l,u) map ->
      let key = spf "[%s] %s => [%s] %s"
        (string_of_ctor l) (string_of_reason (reason_of_t l))
        (string_of_use_ctor u) (string_of_reason (reason_of_use_t u)) in
      match SMap.get key map with
      | None -> SMap.add key 0 map
      | Some i -> SMap.add key (i+1) map
    ) !FlowConstraint.cache SMap.empty in
    SMap.elements group_counts |> List.sort
      (fun (_,i1) (_,i2) -> Pervasives.compare i1 i2)

end

(* Iterate over properties of an object, prioritizing sentinel properties (if
   any) and ignoring shadow properties (if any). *)
let iter_real_props cx id f =
  Context.find_props cx id
  |> SMap.filter (fun x _ -> not (is_internal_name x))
  |> Cache.SentinelProp.ordered_iter id f

(* Helper module for full type resolution as needed to check union and
   intersection types.

   Given a type, we walk it to collect the parts of it we wish to resolve. Once
   these parts are resolved, they must themselves be walked to collect further
   parts to resolve, and so on. In other words, type resolution jobs are created
   and processed in rounds, moving closer and closer to full resolution of the
   original type. Needless to say, these jobs can be recursive, and so must be
   managed carefully for termination and performance. The job management itself
   is done in Graph_explorer. (The jobs are naturally modeled as a graph with
   dynamically created nodes and edges.)

   Here, we define the function that creates a single round of such jobs.
*)

module ResolvableTypeJob = struct

  (* A datatype describing type resolution jobs.

     We unfold types as we go, looking for parts that cannot be unfolded
     immediately (thus needing resolution to proceed).

     The handling of these parts involve calls to `flow` and `unify`, and is
     thus decoupled from the walker itself for clarity. Here, we just create
     different jobs for different parts encountered. These jobs are further
     processed by bindings_of_jobs.

     Briefly, jobs are created for the following cases. (1) Annotation sources
     need to be resolved. (2) So do heads of type applications. (3) Resolved
     tvars are recursively unfolded, but we need to remember which resolved
     tvars have been unfolded to prevent infinite unfolding. (4) Unresolved
     tvars are handled differently based on context: when they are expected
     (e.g., when they are part of inferred types), they are logged; when they
     are unexpected (e.g., when they are part of annotations), they are
     converted to `any`. For more details see bindings_of_jobs.

  *)
  type t =
  | Binding of Type.t
  | OpenResolved
  | OpenUnresolved of int option * Type.t

  (* log_unresolved is a mode that determines whether to log unresolved tvars:
     it is None when resolving annotations, and Some speculation_id when
     resolving inferred types. *)
  let rec collect_of_types ?log_unresolved cx reason =
    List.fold_left (collect_of_type ?log_unresolved cx reason)

  and collect_of_type ?log_unresolved cx reason acc = function
    | OpenT (r, id) as tvar ->
      if IMap.mem id acc then acc
      else if is_constant_property_reason r
      (* It is important to consider reads of constant property names as fully
         resolvable, especially since constant property names are often used to
         store literals that serve as tags for disjoint unions. Unfortunately,
         today we cannot distinguish such reads from others, so we rely on a
         common style convention to recognize constant property names. For now
         this hack pays for itself: we do not ask such reads to be annotated
         with the corresponding literal types to decide membership in those
         disjoint unions. *)
      then IMap.add id (Binding tvar) acc
      else begin match find_graph cx id with
      | Resolved t ->
        let acc = IMap.add id OpenResolved acc in
        collect_of_type ?log_unresolved cx reason acc t
      | Unresolved _ ->
        if is_instantiable_reason r || is_instantiable_reason reason
        (* Instantiable reasons indicate unresolved tvars that are created
           "fresh" for the sole purpose of binding to other types, e.g. as
           instantiations of type parameters or as existentials. Constraining
           them during speculative matching typically do not cause side effects
           across branches, and help make progress. *)
        then acc
        else IMap.add id (OpenUnresolved (log_unresolved, tvar)) acc
      end

    | AnnotT (_, source) ->
      let _, id = open_tvar source in
      if IMap.mem id acc then acc
      else IMap.add id (Binding source) acc

    | ThisTypeAppT (poly_t, _, targs)
    | TypeAppT (poly_t, targs)
      ->
      begin match poly_t with
      | OpenT (_, id) ->
        if IMap.mem id acc then
          collect_of_types ?log_unresolved cx reason acc targs
        else begin
          let acc = IMap.add id (Binding poly_t) acc in
          collect_of_types ?log_unresolved cx reason acc targs
        end

      | _ ->
        let ts = poly_t::targs in
        collect_of_types ?log_unresolved cx reason acc ts
      end

    (* Some common kinds of types are quite overloaded: sometimes they
       correspond to types written by the user, but sometimes they also model
       internal types, and as such carry other bits of information. For now, we
       walk only some parts of these types. These parts are chosen such that
       they directly correspond to parts of the surface syntax of types. It is
       less clear what it means to resolve other "internal" parts of these
       types. In theory, ignoring them *might* lead to bugs, but we've not seen
       examples of such bugs yet. Leaving further investigation of this point as
       future work. *)

    | ObjT (_, { props_tmap; _ }) ->
      let props_tmap = Context.find_props cx props_tmap in
      let ts = SMap.fold (fun x p ts ->
        (* avoid resolving types of shadow properties *)
        if is_internal_name x then ts
        else Property.fold_t (fun ts t -> t::ts) ts p
      ) props_tmap [] in
      collect_of_types ?log_unresolved cx reason acc ts
    | FunT (_, _, _, { params_tlist; return_t; _ }) ->
      let ts = return_t :: params_tlist in
      collect_of_types ?log_unresolved cx reason acc ts
    | ArrT (_, elem_t, ts) ->
      let ts = elem_t::ts in
      collect_of_types ?log_unresolved cx reason acc ts
    | InstanceT (_, static, super,
                 { class_id; type_args; fields_tmap; methods_tmap; _ }) ->
      let ts = if class_id = 0 then [] else [super; static] in
      let ts = SMap.fold (fun _ t ts -> t::ts) type_args ts in
      let props_tmap = SMap.union
        (Context.find_props cx fields_tmap)
        (Context.find_props cx methods_tmap)
      in
      let ts = SMap.fold (fun _ p ts ->
        Property.fold_t (fun ts t -> t::ts) ts p
      ) props_tmap ts in
      collect_of_types ?log_unresolved cx reason acc ts
    | PolyT (_, t) ->
      collect_of_type ?log_unresolved cx reason acc t
    | BoundT _ ->
      acc

    (* TODO: The following kinds of types are not walked out of laziness. It's
       not immediately clear what we'd gain (or lose) by walking them. *)

    | EvalT _
    | ChoiceKitT (_, _)
    | ModuleT (_, _)
    | ExtendsT (_, _, _)
      ->
      acc

    (* The following cases exactly follow Type_visitor (i.e., they do the
       standard walk). TODO: Rewriting this walker as a subclass of Type_visitor
       would be quite nice (as long as we confirm that the resulting
       virtualization of calls to this function doesn't lead to perf
       degradation: this function is expected to be quite hot). *)

    | OptionalT t | MaybeT t | RestT t ->
      collect_of_type ?log_unresolved cx reason acc t
    | UnionT (_, rep) ->
      let ts = UnionRep.members rep in
      collect_of_types ?log_unresolved cx reason acc ts
    | IntersectionT (_, rep) ->
      let ts = InterRep.members rep in
      collect_of_types ?log_unresolved cx reason acc ts

    | AnyWithUpperBoundT t
    | AnyWithLowerBoundT t
    | AbstractT t
    | ExactT (_, t)
    | TypeT (_, t)
    | ClassT t
    | ThisClassT t
      ->
      collect_of_type ?log_unresolved cx reason acc t

    | KeysT (_, t) ->
      collect_of_type ?log_unresolved cx reason acc t

    | ShapeT (t) ->
      collect_of_type ?log_unresolved cx reason acc t

    | DiffT (t1, t2) ->
      let ts = [t1;t2] in
      collect_of_types ?log_unresolved cx reason acc ts

    | IdxWrapper (_, t) ->
      collect_of_type ?log_unresolved cx reason acc t

    | FunProtoBindT _
    | FunProtoCallT _
    | FunProtoApplyT _
    | FunProtoT _
    | ObjProtoT _
    | CustomFunT (_, _)
    | BoolT _
    | NumT _
    | StrT _
    | VoidT _
    | NullT _
    | EmptyT _
    | MixedT _
    | AnyT _
    | TaintT _
    | AnyObjT _
    | AnyFunT _
    | SingletonBoolT _
    | SingletonNumT _
    | SingletonStrT _
    | ExistsT _
    | OpenPredT _
    | TypeMapT _
      ->
      acc

  (* TODO: Support for use types is currently sketchy. Full resolution of use
     types are only needed for choice-making on intersections. We care about
     calls in particular because one of the biggest uses of intersections is
     function overloading. More uses will be added over time. *)
  and collect_of_use ~log_unresolved cx reason acc = function
  | UseT (_, t) ->
    collect_of_type ~log_unresolved cx reason acc t
  | CallT (_, ft) ->
    collect_of_types ~log_unresolved cx reason acc
      (ft.params_tlist @ [ft.return_t])
  | _ -> acc

end

(*********************************************************************)

exception SpeculativeError of FlowError.error_message

let add_output cx trace msg =
  if Speculation.speculating ()
  then raise (SpeculativeError msg)
  else FlowError.add_output cx ~trace msg

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

let not_expect_bound_use t =
  lift_to_use not_expect_bound t

(* Sometimes we expect to see only proper def types. Proper def types make sense
   as use types. *)
let expect_proper_def t =
  if not (is_proper_def t) then
    assert_false (spf "Did not expect %s" (string_of_ctor t))

let expect_proper_def_use t =
  lift_to_use expect_proper_def t

(********************** start of slab **********************************)

(** NOTE: Do not call this function directly. Instead, call the wrapper
    functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
    this module, and the function `flow` outside this module. **)
let rec __flow cx ((l: Type.t), (u: Type.use_t)) trace =
  begin match Context.verbose cx with
  | Some { Verbose.indent; depth } ->
    let indent = String.make ((Trace.trace_depth trace - 1) * indent) ' ' in
    let pid = Context.pid_prefix cx in
    prerr_endlinef
      "\n%s%s%s ~>\n%s%s%s"
      indent pid (Debug_js.dump_t ~depth cx l)
      indent pid (Debug_js.dump_use_t ~depth cx u)
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
    not_expect_bound l;
    not_expect_bound_use u;
    (* Types that are classified as def types but don't make sense as use types
       should not appear as use types. *)
    expect_proper_def_use u;

    (* Before processing the flow action, check that it is not deferred. If it
       is, then when speculation is complete, the action either fires or is
       discarded depending on whether the case that created the action is
       selected or not. *)
    if not Speculation.(defer_action cx (Action.Flow (l, u))) then

    match (l,u) with

    (********)
    (* eval *)
    (********)

    | EvalT (t, TypeDestructorT (reason, s), i), _ ->
      rec_flow cx trace (eval_destructor cx ~trace reason t s i, u)

    | _, UseT (use_op, EvalT (t, TypeDestructorT (reason, s), i)) ->
      (* When checking a lower bound against a destructed type, we need to take
         some care. In particular, we do not want the destructed type to be
         "open" when t is not itself open, i.e., we do not want any "extra"
         lower bounds to be able to flow to the destructed type than what t
         itself allows.

         For example, when t is { x: number }, we want $PropertyType(t) to be
         number, not some open tvar that is a supertype of number (since the
         latter would accept more than number, e.g. string). Similarly, when t
         is ?string, we want the $NonMaybeType(t) to be string. *)
      let result = eval_destructor cx ~trace reason t s i in
      begin match t with
      | OpenT _ ->
        (* TODO: If t itself is an open tvar, we can afford to be looser for
           now. The additional looseness is not entirely justifiable (e.g., we
           should still prevent "extra" lower bounds from flowing into the
           destructed type that did not originate from lower bounds flowing to
           t), and we should do more work to avoid it, but it's at least not as
           egregious as the case when t is not open. *)
        rec_flow cx trace (l, UseT (use_op, result))
      | _ ->
        (* With the same "slingshot" trick used by AnnotT, hold the lower bound
           at bay until result itself gets concretized, and then flow the lower
           bound to that concrete type. Note: this works for type destructors
           since they come from annotations and other types that have the 0->1
           property, but does not work in general because for arbitrary tvars,
           concretization may never happen or may happen more than once. *)
        rec_flow cx trace (result, ReposUseT (reason, use_op, l))
      end

    | EvalT (t, DestructuringT (reason, s), i), _ ->
      rec_flow cx trace (eval_selector cx ~trace reason t s i, u)

    | _, UseT (use_op, EvalT (t, DestructuringT (reason, s), i)) ->
      rec_flow cx trace (l, UseT (use_op, eval_selector cx ~trace reason t s i))


    (******************)
    (* process X ~> Y *)
    (******************)

    | (OpenT(_, tvar1), UseT (use_op, OpenT(_, tvar2))) ->
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
          edges_and_flows_to_t cx trace (id1, bounds1) (UseT (use_op, t2))

      | Resolved t1, Unresolved bounds2 ->
          edges_and_flows_from_t cx trace t1 (id2, bounds2)

      | Resolved t1, Resolved t2 ->
          rec_flow cx trace (t1, UseT (use_op, t2))
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

    | (t1, UseT (use_op, OpenT(_, tvar))) ->
      let id2, constraints2 = find_constraints cx tvar in
      (match constraints2 with
      | Unresolved bounds2 ->
          edges_and_flows_from_t cx trace t1 (id2, bounds2)

      | Resolved t2 ->
          rec_flow cx trace (t1, UseT (use_op, t2))
      );

    (************************)
    (* Full type resolution *)
    (************************)

    (* Full resolution of a type involves (1) walking the type to collect a
       bunch of unresolved tvars (2) emitting constraints that, once those tvars
       are resolved, recursively trigger the process for the resolved types (3)
       finishing when no unresolved tvars remain.

       (1) is covered in ResolvableTypeJob. Below, we cover (2) and (3).

       For (2), we emit a FullyResolveType constraint on any unresolved tvar
       found by (1). These unresolved tvars are chosen so that they have the
       following nice property, called '0->1': they remain unresolved until, at
       some point, they are unified with a concrete type. Moreover, the act of
       resolution coincides with the appearance of one (the first and the last)
       upper bound. (In general, unresolved tvars can accumulate an arbitrary
       number of lower and upper bounds over its lifetime.) More details can be
       found in bindings_of_jobs.

       For (3), we create a special "goal" tvar that acts like a promise for
       fully resolving the original type, and emit a Trigger constraint on the
       goal when no more work remains.

       The main client of full type resolution is checking union and
       intersection types. The check itself is modeled by a TryFlow constraint,
       which is guarded by a goal tvar that corresponds to some full type
       resolution requirement. Eventually, this goal is "triggered," which in
       turn triggers the check. (The name "TryFlow" refers to the technique used
       in the check, which literally tries each branch of the union or
       intersection in turn, maintaining some matching state as it goes: see
       speculative_matches for details). *)

    | t, ChoiceKitUseT (reason, FullyResolveType id) ->
      fully_resolve_type cx trace reason id t

    | ChoiceKitT (_, Trigger), ChoiceKitUseT (reason, TryFlow (i, spec)) ->
      speculative_matches cx trace reason i spec

    (* Intersection types need a preprocessing step before they can be checked;
       this step brings it closer to parity with the checking of union types,
       where the preprocessing effectively happens "automatically." This
       apparent asymmetry is explained in prep_try_intersection.

       Here, it suffices to note that the preprocessing step involves
       concretizing some types. Type concretization is distinct from full type
       resolution. Whereas full type resolution is a recursive process that
       needs careful orchestration, type concretization is a relatively simple
       one-step process: a tvar is concretized when any lower bound appears on
       it. Also, unlike full type resolution, the tvars that are concretized
       don't necessarily have the 0->1 property: they could be concretized at
       different types, as more and more lower bounds appear. *)

    | UnionT (_, urep), IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      UnionRep.members urep |> List.iter (fun t ->
        rec_flow cx trace (t, u)
      )

    | MaybeT t, IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      let lreason = reason_of_t t in
      rec_flow cx trace (NullT.why lreason, u);
      rec_flow cx trace (VoidT.why lreason, u);
      rec_flow cx trace (t, u);

    | OptionalT t, IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      let lreason = reason_of_t t in
      rec_flow cx trace (VoidT.why lreason, u);
      rec_flow cx trace (t, u);

    | AnnotT (_, source_t), IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      rec_flow cx trace (source_t, u)

    | t, IntersectionPreprocessKitT (reason,
        ConcretizeTypes (unresolved, resolved, IntersectionT (r, rep), u)) ->
      prep_try_intersection cx trace reason unresolved (resolved @ [t]) u r rep

    (*****************************)
    (* Refinement type subtyping *)
    (*****************************)

    | _, RefineT (reason, LatentP (fun_t, idx), tvar) ->
      flow cx (fun_t, CallLatentPredT (reason, true, idx, l, tvar))

    (*************)
    (* Debugging *)
    (*************)

    | (_, DebugPrintT (reason)) ->
      add_output cx trace (FlowError.EDebugPrint (reason, l))

    (************)
    (* tainting *)
    (************)

    | (TaintT _, UseT (_, TaintT _)) ->
      ()

    | (TaintT _, u) when taint_op u ->
      begin match result_of_taint_op u with
      | Some u -> rec_flow_t cx trace (l, u)
      | None -> ()
      end

    (*************************)
    (* repositioning, part 1 *)
    (*************************)

    (* Waits for a def type to become concrete, repositions it as an upper UseT
       using the stored reason. This can be used to store a reason as it flows
       through a tvar. *)

    | (UnionT (r, rep), ReposUseT (reason, use_op, l)) ->
      (* Don't reposition union members when the union appears as an upper
         bound. This improves error messages when an incompatible lower bound
         does not satisfy any member of the union. The "overall" error points to
         the reposition target (an annotation) but the detailed member
         information points to the type definition. *)
      let u_def = UnionT (repos_reason (loc_of_reason reason) r, rep) in
      rec_flow cx trace (l, UseT (use_op, u_def))

    | (u_def, ReposUseT (reason, use_op, l)) ->
      rec_flow cx trace (l, UseT (use_op, reposition cx ~trace reason u_def))

    (***************)
    (* annotations *)
    (***************)

    (* The sink component of an annotation constrains values flowing
       into the annotated site. *)

    | _, UseT (use_op, AnnotT (_, source_t)) ->
      let reason = reason_of_t source_t in
      rec_flow cx trace (source_t, ReposUseT (reason, use_op, l))

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

    | PolyT(typeparams, FunT(_, _, prototype, _)),
      ImportTypeT(reason, _, t) ->
      rec_flow_t cx trace (PolyT(typeparams, TypeT(reason, prototype)), t)

    | (TypeT _, ImportTypeT(_, _, t))
    | (PolyT(_, TypeT _), ImportTypeT(_, _, t))
      -> rec_flow_t cx trace (l, t)

    (** TODO: This rule allows interpreting an object as a type!

        It is currently used to work with modules that export named types,
        e.g. 'react' or 'immutable'. For example, one can do

        `import type React from 'react'`

        followed by uses of `React` as a container of types in (say) type
        definitions like

        `type C = React.Component<any,any,any>`

        Fortunately, in that case `React` is stored as a type binding in the
        environment, so it cannot be used as a value.

        However, removing this special case causes no loss of expressibility
        (while making the model simpler). For example, in the above example we
        can write

        `import type { Component } from 'react'`

        followed by (say)

        `type C = Component<any,any,any>`

        Overall, we should be able to (at least conceptually) desugar `import
        type` to `import` followed by `type`.

    **)
    | (ObjT _, ImportTypeT(_, "default", t)) ->
      rec_flow_t cx trace (l, t)

    | (_, ImportTypeT(reason, export_name, _)) ->
      add_output cx trace (FlowError.EImportValueAsType (reason, export_name))

    (************************************************************************)
    (* `import typeof` creates a properly-parameterized type alias for the  *)
    (* "typeof" the remote export.                                          *)
    (************************************************************************)
    | PolyT(typeparams, ((ClassT _ | FunT _) as lower_t)),
      ImportTypeofT(reason, _, t) ->
      let typeof_t = mk_typeof_annotation cx ~trace reason lower_t in
      rec_flow_t cx trace (PolyT(typeparams, TypeT(reason, typeof_t)), t)

    | ((TypeT _ | PolyT(_, TypeT _)), ImportTypeofT(reason, export_name, _)) ->
      add_output cx trace (FlowError.EImportTypeAsTypeof (reason, export_name))

    | (_, ImportTypeofT(reason, _, t)) ->
      let typeof_t = mk_typeof_annotation cx ~trace reason l in
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

    (* In the following rules, ModuleT appears in two contexts: as imported
       modules, and as modules to be exported.

       As a module to be exported, ModuleT denotes a "growing" module. In this
       form, its contents may change: e.g., its named exports may be
       extended. Conversely, the rules that drive this growing phase can expect
       to work only on ModuleT. In particular, modules that are not @flow never
       hit growing rules: they are modeled as `any`.

       On the other hand, as an imported module, ModuleT denotes a "fully
       formed" module. The rules hit by such a module don't grow it: they just
       take it apart and read it. The same rules could also be hit by modules
       that are not @flow, so the rules have to deal with `any`. *)

    (* util that grows a module by adding named exports from a given map *)
    | (ModuleT(_, exports), ExportNamedT(_, tmap, t_out)) ->
      SMap.iter (Context.set_export cx exports.exports_tmap) tmap;
      rec_flow_t cx trace (l, t_out)

    (** Copy the named exports from a source module into a target module. Used
        to implement `export * from 'SomeModule'`, with the current module as
        the target and the imported module as the source. *)
    | (ModuleT(_, source_exports),
       CopyNamedExportsT(reason, target_module_t, t_out)) ->
      let source_tmap = Context.find_exports cx source_exports.exports_tmap in
      rec_flow cx trace (
        target_module_t,
        ExportNamedT(reason, source_tmap, t_out)
      )

    (* There is nothing to copy from a module exporting `any` or `Object`. *)
    | (AnyT _ | AnyObjT _), CopyNamedExportsT(_, target_module, t) ->
      rec_flow_t cx trace (target_module, t)

    (**
     * ObjT CommonJS export values have their properties turned into named
     * exports
     *)
    | ObjT(_, {props_tmap; proto_t; _;}),
      CJSExtractNamedExportsT(
        reason, (module_t_reason, exporttypes), t_out
      ) ->

      (* Copy props from the prototype *)
      let module_t = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (
          proto_t,
          CJSExtractNamedExportsT(reason, (module_t_reason, exporttypes), t)
        )
      ) in

      (* Copy own props *)
      rec_flow cx trace (module_t, ExportNamedT(
        reason,
        Properties.extract_named_exports (Context.find_props cx props_tmap),
        t_out
      ))

    (**
     * InstanceT CommonJS export values have their properties turned into named
     * exports
     *)
    | InstanceT(_, _, _, {fields_tmap; methods_tmap; _;}),
      CJSExtractNamedExportsT(
        reason, (module_t_reason, exporttypes), t_out
      ) ->

      let module_t = ModuleT (module_t_reason, exporttypes) in

      let extract_named_exports id =
        Context.find_props cx id
        |> SMap.filter (fun x _ -> not (is_munged_prop_name cx x))
        |> Properties.extract_named_exports
      in

      (* Copy fields *)
      let module_t = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (module_t, ExportNamedT(
          reason,
          extract_named_exports fields_tmap,
          t
        ))
      ) in

      (* Copy methods *)
      rec_flow cx trace (module_t, ExportNamedT(
        reason,
        extract_named_exports methods_tmap,
        t_out
      ))

    (* If the module is exporting any or Object, then we allow any named
     * import
     *)
    | ((AnyT _ | AnyObjT _),
        CJSExtractNamedExportsT(_, (module_t_reason, exporttypes), t_out)) ->
      let module_t = ModuleT (
        module_t_reason,
        { exporttypes with has_every_named_export = true; }
      ) in
      rec_flow_t cx trace (module_t, t_out)

    (**
     * All other CommonJS export value types do not get merged into the named
     * exports tmap in any special way.
     *)
    | (_, CJSExtractNamedExportsT(_, (module_t_reason, exporttypes), t_out)) ->
      let module_t = ModuleT (module_t_reason, exporttypes) in
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
          (* convert ES module's named exports to an object *)
          let proto = ObjProtoT reason in
          let exports_tmap = Context.find_exports cx exports.exports_tmap in
          let props = SMap.map (Property.field Neutral) exports_tmap in
          mk_object_with_map_proto cx reason
            ~sealed:true ~frozen:true props proto
      ) in
      rec_flow_t cx trace (cjs_exports, t)

    (* import * as X from 'SomeModule'; *)
    | (ModuleT(_, exports), ImportModuleNsT(reason, t)) ->
      let exports_tmap = Context.find_exports cx exports.exports_tmap in
      let props = SMap.map (Property.field Neutral) exports_tmap in
      let props = match exports.cjs_export with
      | Some t ->
        let p = Field (t, Neutral) in
        SMap.add "default" p props
      | None -> props
      in
      let dict = if exports.has_every_named_export
      then Some {
        key = StrT.why reason;
        value = AnyT.why reason;
        dict_name = None;
        dict_polarity = Neutral;
      }
      else None in
      let proto = ObjProtoT reason in
      let ns_obj = mk_object_with_map_proto cx reason
        ~sealed:true ~frozen:true ?dict props proto
      in
      rec_flow_t cx trace (ns_obj, t)

    (* import [type] X from 'SomeModule'; *)
    | ModuleT(_, exports),
      ImportDefaultT(reason, import_kind, (local_name, module_name), t) ->
      let export_t = match exports.cjs_export with
        | Some t -> t
        | None ->
            let exports_tmap = Context.find_exports cx exports.exports_tmap in
            match SMap.get "default" exports_tmap with
              | Some t -> t
              | None ->
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
                let suggestion = typo_suggestion known_exports local_name in
                add_output cx trace (FlowError.ENoDefaultExport
                  (reason, module_name, suggestion));
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
    | ModuleT(_, exports), ImportNamedT(reason, import_kind, export_name, t) ->
        (**
         * When importing from a CommonJS module, we shadow any potential named
         * exports called "default" with a pointer to the raw `module.exports`
         * object
         *)
        let exports_tmap = (
          let exports_tmap = Context.find_exports cx exports.exports_tmap in
          match exports.cjs_export with
          | Some t -> SMap.add "default" t exports_tmap
          | None -> exports_tmap
        ) in
        let has_every_named_export = exports.has_every_named_export in
        let import_t = (
          match (import_kind, SMap.get export_name exports_tmap) with
          | (ImportType, Some t) ->
            mk_tvar_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeT(reason, export_name, tvar))
            )
          | (ImportType, None) when has_every_named_export ->
            let t = AnyT.why reason in
            mk_tvar_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeT(reason, export_name, tvar))
            )
          | (ImportTypeof, Some t) ->
            mk_tvar_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeofT(reason, export_name, tvar))
            )
          | (ImportTypeof, None) when has_every_named_export ->
            let t = AnyT.why reason in
            mk_tvar_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeofT(reason, export_name, tvar))
            )
          | (ImportValue, Some t) ->
            rec_flow cx trace (t, AssertImportIsValueT(reason, export_name));
            t
          | (ImportValue, None) when has_every_named_export ->
            let t = AnyT.why reason in
            rec_flow cx trace (t, AssertImportIsValueT(reason, export_name));
            t
          | (_, None) ->
            let num_exports = SMap.cardinal exports_tmap in
            let has_default_export = SMap.get "default" exports_tmap <> None in

            let msg =
              if num_exports = 1 && has_default_export
              then
                FlowError.EOnlyDefaultExport (reason, export_name)
              else
                let known_exports = SMap.keys exports_tmap in
                let suggestion = typo_suggestion known_exports export_name in
                FlowError.ENoNamedExport (reason, export_name, suggestion)
            in
            add_output cx trace msg;
            AnyT.why reason
        ) in
        rec_flow_t cx trace (import_t, t)

    (* imports are `any`-typed when they are from (1) unchecked modules or (2)
       modules with `any`-typed exports *)
    | (AnyT _ | AnyObjT _),
        ( CJSRequireT(reason, t)
        | ImportModuleNsT(reason, t)
        | ImportDefaultT(reason, _, _, t)
        | ImportNamedT(reason, _, _, t)
        ) ->
      rec_flow_t cx trace (AnyT.why reason, t)

    | ((PolyT (_, TypeT _) | TypeT _), AssertImportIsValueT(reason, name)) ->
      add_output cx trace (FlowError.EImportTypeAsValue (reason, name))

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
    (* common implicit conversions *)
    (*******************************)

    | (_, UseT (_, NumT _)) when numeric l -> ()

    | (_, UseT (_, AnyObjT _)) when object_like l -> ()
    | (AnyObjT _, UseT (_, u)) when object_like u -> ()

    | (_, UseT (_, AnyFunT _)) when function_like l -> ()

    | AnyFunT reason, GetPropT (_, Named (_, x), _)
    | AnyFunT reason, SetPropT (_, Named (_, x), _)
    | AnyFunT reason, LookupT (_, _, _, Named (_, x), _)
    | AnyFunT reason, MethodT (_, _, Named (_, x), _)
    | AnyFunT reason, HasPropT (_, _, Literal x) when is_function_prototype x ->
      rec_flow cx trace (FunProtoT reason, u)
    | (AnyFunT _, UseT (_, u)) when function_like u -> ()
    | (AnyFunT _, UseT (_, u)) when object_like u -> ()
    | AnyFunT _, UseT (_, (TypeT _ | AnyFunT _)) -> ()

    (**
     * Handling for the idx() custom function.
     *
     * idx(a, a => a.b.c) is a 2-arg function with semantics meant to simlify
     * the process of extracting a property from a chain of maybe-typed property
     * accesses.
     *
     * As an example, if you consider an object type such as:
     *
     *   {
     *     me: ?{
     *       firstName: string,
     *       lastName: string,
     *       friends: ?Array<User>,
     *     }
     *   }
     *
     * The process of getting to the friends of my first friend (safely) looks
     * something like this:
     *
     *   let friendsOfFriend = obj.me && obj.me.friends && obj.me.friends[0]
     *                         && obj.me.friends[0].friends;
     *
     * This is verbose to say the least. To simplify, we can define a function
     * called idx() as:
     *
     *   function idx(obj, callback) {
     *     try { return callback(obj); } catch (e) {
     *       if (isNullPropertyAccessError(e)) {
     *         return null;
     *       } else {
     *         throw e;
     *       }
     *     }
     *   }
     *
     * This function can then be used to safely dive into the aforementioned
     * object tersely:
     *
     *  let friendsOfFriend = idx(obj, obj => obj.me.friends[0].friends);
     *
     * If we assume these semantics, then we can model the type of this function
     * by wrapping the `obj` parameter in a special signifying wrapper type that
     * is only valid against use types associated with property accesses. Any
     * time this specially wrapper type flows into a property access operation,
     * we:
     *
     * 1) Strip away any potential MaybeT from the contained type
     * 2) Forward the un-Maybe'd type on to the access operation
     * 3) Wrap the result back in the special wrapper
     *
     * We can then flow this wrapped `obj` to a call on the callback function,
     * remove the wrapper from the return type, and return that value wrapped in
     * a MaybeT.
     *
     * ...of course having a `?.` operator in the language would be a nice
     *    reason to throw all of this clownerous hackery away...
     *)
    | CustomFunT (_, Idx),
      CallT (reason_op, {
        params_tlist;
        return_t;
        this_t;
        closure_t;
        changeset;
        _;
      }) ->
      (match params_tlist with
        | obj::cb::[] ->
          let wrapped_obj = IdxWrapper (reason_op, obj) in
          let callback_result = mk_tvar_where cx reason_op (fun t ->
            rec_flow cx trace (cb, CallT (reason_op, {
              this_t;
              params_tlist = [wrapped_obj];
              params_names = None;
              return_t = t;
              is_predicate = false;
              closure_t;
              changeset
            }))
          ) in
          let unwrapped_t = mk_tvar_where cx reason_op (fun t ->
            rec_flow cx trace (callback_result, IdxUnwrap(reason_op, t))
          ) in
          rec_flow_t cx trace (MaybeT unwrapped_t, return_t)
        | _ ->
          (* Why is idx strict about arity? No other functions are. *)
          add_output cx trace (FlowError.EIdxArity reason_op)
      )

    (* Unwrap idx() callback param *)
    | (IdxWrapper (_, obj), IdxUnwrap (_, t)) -> rec_flow_t cx trace (obj, t)
    | (_, IdxUnwrap (_, t)) -> rec_flow_t cx trace (l, t)

    (* De-maybe-ify an idx() property access *)
    | (MaybeT inner_t, IdxUnMaybeifyT _)
    | (OptionalT inner_t, IdxUnMaybeifyT _)
      -> rec_flow cx trace (inner_t, u)
    | (NullT _, IdxUnMaybeifyT _) -> ()
    | (VoidT _, IdxUnMaybeifyT _) -> ()
    | (_, IdxUnMaybeifyT (_, t)) when (
        match l with
        | UnionT _ | IntersectionT _ -> false
        | _ -> true
      ) ->
      rec_flow_t cx trace (l, t)

    (* The set of valid uses of an idx() callback parameter. In general this
       should be limited to the various forms of property access operations. *)
    | (IdxWrapper (idx_reason, obj), ReposLowerT (reason_op, u)) ->
      let repositioned_obj = mk_tvar_where cx reason_op (fun t ->
        rec_flow cx trace (obj, ReposLowerT (reason_op, UseT (UnknownUse, t)))
      ) in
      rec_flow cx trace (IdxWrapper(idx_reason, repositioned_obj), u)

    | (IdxWrapper (idx_reason, obj), GetPropT (reason_op, propname, t_out)) ->
      let de_maybed_obj = mk_tvar_where cx idx_reason (fun t ->
        rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t))
      ) in
      let prop_type = mk_tvar_where cx reason_op (fun t ->
        rec_flow cx trace (de_maybed_obj, GetPropT (reason_op, propname, t))
      ) in
      rec_flow_t cx trace (IdxWrapper (idx_reason, prop_type), t_out)

    | (IdxWrapper (idx_reason, obj), GetElemT (reason_op, prop, t_out)) ->
      let de_maybed_obj = mk_tvar_where cx idx_reason (fun t ->
        rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t))
      ) in
      let prop_type = mk_tvar_where cx reason_op (fun t ->
        rec_flow cx trace (de_maybed_obj, GetElemT (reason_op, prop, t))
      ) in
      rec_flow_t cx trace (IdxWrapper (idx_reason, prop_type), t_out)

    | (IdxWrapper (reason, _), UseT _) ->
      add_output cx trace (FlowError.EIdxUse1 reason)

    | (IdxWrapper (reason, _), _) ->
      add_output cx trace (FlowError.EIdxUse2 reason)

    (***************)
    (* maybe types *)
    (***************)

    (** The type maybe(T) is the same as null | undefined | UseT *)

    | (NullT r | VoidT r), UseT (use_op, MaybeT tout) ->
      rec_flow cx trace (EmptyT.why r, UseT (use_op, tout))

    | MaybeT _, ReposLowerT (reason_op, u) ->
      (* Don't split the maybe type into its constituent members. Instead,
         reposition the entire maybe type. *)
      rec_flow cx trace (reposition cx ~trace reason_op l, u)

    | MaybeT t1, UseT (_, MaybeT _) ->
      rec_flow cx trace (t1, u)

    | (MaybeT(t), _) ->
      let reason = reason_of_t t in
      rec_flow cx trace (NullT.why reason, u);
      rec_flow cx trace (VoidT.why reason, u);
      rec_flow cx trace (t, u)

    (******************)
    (* optional types *)
    (******************)

    (** The type optional(T) is the same as undefined | UseT *)

    | (VoidT _, UseT (_, OptionalT _)) -> ()

    | OptionalT _, ReposLowerT (reason_op, u) ->
      (* Don't split the optional type into its constituent members. Instead,
         reposition the entire optional type. *)
      rec_flow cx trace (reposition cx ~trace reason_op l, u)

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
      let reason = replace_reason_const (RBooleanLit true) reason in
      rec_flow_t cx trace (BoolT (reason, Some true), tout)

    (* !x when x is truthy *)
    | (_, NotT(reason, tout)) ->
      let reason = replace_reason_const (RBooleanLit false) reason in
      rec_flow_t cx trace (BoolT (reason, Some false), tout)

    | (left, AndT(_, right, u)) ->
      (* a falsy && b ~> a
         a truthy && b ~> b
         a && b ~> a falsy | b *)
      let truthy_left = filter_exists left in
      (match truthy_left with
      | EmptyT _ ->
        (* falsy *)
        rec_flow cx trace (left, PredicateT (NotP ExistsP, u))
      | _ ->
        (match filter_not_exists left with
        | EmptyT _ -> (* truthy *)
          rec_flow cx trace (right, UseT (UnknownUse, u))
        | _ ->
          rec_flow cx trace (left, PredicateT (NotP ExistsP, u));
          begin match truthy_left with
          | EmptyT _ -> ()
          | _ ->
            rec_flow cx trace (right, UseT (UnknownUse, u))
          end
        )
      )

    | (left, OrT(_, right, u)) ->
      (* a truthy || b ~> a
         a falsy || b ~> b
         a || b ~> a truthy | b *)
      let falsy_left = filter_not_exists left in
      (match falsy_left with
      | EmptyT _ ->
        (* truthy *)
        rec_flow cx trace (left, PredicateT (ExistsP, u))
      | _ ->
        (match filter_exists left with
        | EmptyT _ -> (* falsy *)
          rec_flow cx trace (right, UseT (UnknownUse, u))
        | _ ->
          rec_flow cx trace (left, PredicateT (ExistsP, u));
          begin match falsy_left with
          | EmptyT _ -> ()
          | _ ->
            rec_flow cx trace (right, UseT (UnknownUse, u))
          end
        )
      )

    | (_, ObjTestT(reason_op, default, u)) ->
      let u = ReposLowerT(reason_op, UseT (UnknownUse, u)) in
      if object_like l
      then rec_flow cx trace (l, u)
      else rec_flow cx trace (default, u)

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

    | (_, UseT (_, AnyWithLowerBoundT _)) ->
      ()

    | (AnyWithUpperBoundT _, _) ->
      ()

    | (_, UseT (_, AnyWithUpperBoundT t)) ->
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
      let reason_op = reason_of_use_t u in
      let reason_tapp = reason_of_t l in
      let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
      let c = instantiate_this_class cx trace reason_op tc this in
      rec_flow cx trace (mk_instance cx ~trace reason_op ~for_type:false c, u)

    | (_, UseT (use_op, ThisTypeAppT(c,this,ts))) ->
      let reason_op = reason_of_t l in
      let reason_tapp = reason_of_use_t u in
      let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
      let c = instantiate_this_class cx trace reason_op tc this in
      let t_out = mk_instance cx ~trace reason_op ~for_type:false c in
      rec_flow cx trace (l, UseT (use_op, t_out))

    | TypeAppT _, ReposLowerT (reason_op, u) ->
        rec_flow cx trace (reposition cx ~trace reason_op l, u)

    | (TypeAppT(c,ts), MethodT _) ->
        let reason_op = reason_of_use_t u in
        let reason_tapp = reason_of_t l in
        let t = mk_typeapp_instance cx
          ~trace ~reason_op ~reason_tapp ~cache:true c ts in
        rec_flow cx trace (t, u)

    | (TypeAppT (c1, ts1), UseT (_, TypeAppT (c2, ts2)))
      when c1 = c2 && List.length ts1 = List.length ts2 ->
      let reason_op = reason_of_t l in
      let reason_tapp = reason_of_use_t u in
      let targs = List.map2 (fun t1 t2 -> (t1, t2)) ts1 ts2 in
      rec_flow cx trace (c1,
        TypeAppVarianceCheckT (reason_op, reason_tapp, targs))

    | (TypeAppT(c,ts), _) ->
        if TypeAppExpansion.push_unless_loop cx (c, ts) then (
          let reason_op = reason_of_use_t u in
          let reason_tapp = reason_of_t l in
          let t = mk_typeapp_instance cx ~trace ~reason_op ~reason_tapp c ts in
          rec_flow cx trace (t, u);
          TypeAppExpansion.pop ()
        )

    | (_, UseT (use_op, TypeAppT(c,ts))) ->
        if TypeAppExpansion.push_unless_loop cx (c, ts) then (
          let reason_op = reason_of_t l in
          let reason_tapp = reason_of_use_t u in
          let t = mk_typeapp_instance cx ~trace ~reason_op ~reason_tapp c ts in
          rec_flow cx trace (l, UseT (use_op, t));
          TypeAppExpansion.pop ()
        )

    (*****************************************************************)
    (* Intersection type preprocessing for certain object predicates *)
    (*****************************************************************)

    (* Predicate refinements on intersections of object types need careful
       handling. An intersection of object types passes a predicate when any of
       those object types passes the predicate: however, the refined type must
       be the intersection as a whole, not the particular object type that
       passes the predicate! (For example, we may check some condition on
       property x and property y of { x: ... } & { y: ... } in sequence, and not
       expect to get property-not-found errors in the process.)

       Although this seems like a special case, it's not. An intersection of
       object types should behave more or less the same as a "concatenated"
       object type with all the properties of those object types. The added
       complication arises as an implementation detail, because we do not
       concatenate those object types explicitly. *)

    | _, IntersectionPreprocessKitT (_,
        SentinelPropTest (sense, key, t, inter, tvar)) ->
      sentinel_prop_test_generic key cx trace tvar inter (sense, l, t)

    | _, IntersectionPreprocessKitT (reason,
        PropExistsTest (sense, key, inter, tvar)) ->
      prop_exists_test_generic reason key cx trace tvar inter sense l

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

    | StrT (_, actual), UseT (_, SingletonStrT (_, expected)) ->
      if literal_eq expected actual
      then ()
      else
        let reasons = FlowError.ordered_reasons l u in
        add_output cx trace
          (FlowError.EExpectedStringLit (reasons, expected, actual))

    | NumT (_, actual), UseT (_, SingletonNumT (_, expected)) ->
      if number_literal_eq expected actual
      then ()
      else
        let reasons = FlowError.ordered_reasons l u in
        add_output cx trace
          (FlowError.EExpectedNumberLit (reasons, expected, actual))

    | BoolT (_, actual), UseT (_, SingletonBoolT (_, expected)) ->
      if boolean_literal_eq expected actual
      then ()
      else
        let reasons = FlowError.ordered_reasons l u in
        add_output cx trace
          (FlowError.EExpectedBooleanLit (reasons, expected, actual))

    (*****************************************************)
    (* keys (NOTE: currently we only support string keys *)
    (*****************************************************)

    | (StrT (reason_s, literal), UseT (_, KeysT (reason_op, o))) ->
      let reason_next = match literal with
      | Literal x -> replace_reason_const (RProperty (Some x)) reason_s
      | _ -> replace_reason_const RUnknownString reason_s in
      (* check that o has key x *)
      let u = HasOwnPropT(reason_next, literal) in
      rec_flow cx trace (o, ReposLowerT(reason_op, u))


    | KeysT (reason1, o1), _ ->
      (* flow all keys of o1 to u *)
      rec_flow cx trace (o1, GetKeysT (reason1,
        match u with
        | UseT (_, t) -> t
        | _ -> tvar_with_constraint cx ~trace u))

    (* helpers *)

    | ObjT (reason_o, { props_tmap = mapr; dict_t; _; }),
      HasOwnPropT (reason_op, x) ->
      (match x, dict_t with
      (* If we have a literal string and that property exists *)
      | Literal x, _ when Context.has_prop cx mapr x -> ()
      (* If we have a dictionary, try that next *)
      | _, Some { key; _ } -> rec_flow_t cx trace (StrT (reason_op, x), key)
      | _ ->
        add_output cx trace (FlowError.EPropNotFound (reason_op, reason_o)))

    | ObjT (reason_o, { props_tmap = mapr; proto_t = proto; dict_t; _ }),
      HasPropT (reason_op, strict, x) ->
      (match x, dict_t with
      (* If we have a literal string and that property exists *)
      | Literal x, _ when Context.has_prop cx mapr x -> ()
      (* If we have a dictionary, try that next *)
      | _, Some { key; _ } -> rec_flow_t cx trace (StrT (reason_op, x), key)
      | _ ->
        let strict = match strict with
        | Some r -> Some r
        | None -> Some reason_o
        in
        rec_flow cx trace (proto, HasPropT (reason_op, strict, x))
      )

    | InstanceT (reason_o, _, _, instance), HasOwnPropT(reason_op, Literal x) ->
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      (match SMap.get x fields with
      | Some _ -> ()
      | None ->
        add_output cx trace (FlowError.EPropNotFound (reason_op, reason_o)))

    | (InstanceT (reason_o, _, _, _), HasOwnPropT(reason_op, _)) ->
        let msg = "Expected string literal" in
        add_output cx trace (FlowError.ECustom ((reason_op, reason_o), msg))

    | InstanceT (reason_o, _, super, instance),
      HasPropT (reason_op, strict, Literal x) ->
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      (match SMap.get x fields with
      | Some _ -> ()
      | None ->
          let strict = match strict with
          | Some r -> Some r
          | None -> Some reason_o
          in
          rec_flow cx trace (super, HasPropT (reason_op, strict, Literal x)))

    | (InstanceT (reason_o, _, _, _), HasPropT(reason_op, _, _)) ->
        let msg = "Expected string literal" in
        add_output cx trace (FlowError.ECustom ((reason_op, reason_o), msg))

    (* AnyObjT has every prop *)
    | AnyObjT _, HasOwnPropT _
    | AnyObjT _, HasPropT _ -> ()

    | ObjT (reason, { flags; props_tmap = mapr; _ }), GetKeysT (_, keys) ->
      begin match flags.sealed with
      | Sealed ->
        (* flow each key of l to keys *)
        Context.iter_props cx mapr (fun x _ ->
          let t = StrT (reason, Literal x) in
          rec_flow_t cx trace (t, keys)
        );
      | _ ->
        rec_flow_t cx trace (StrT.why reason, keys)
      end

    | InstanceT (reason, _, _, instance), GetKeysT (_, keys) ->
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
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

    | UnionT _, ReposLowerT (reason_op, u) ->
      (* Don't split the union type into its constituent members. Instead,
         reposition the entire union type. *)
      rec_flow cx trace (reposition cx ~trace reason_op l, u)

    (* cases where there is no loss of precision *)

    | UnionT (_, rep), _ ->
      UnionRep.members rep |> List.iter (fun t -> rec_flow cx trace (t,u))

    | _, UseT (use_op, IntersectionT (_, rep)) ->
      InterRep.members rep |> List.iter (fun t ->
        rec_flow cx trace (l, UseT (use_op, t))
      )

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

    | IntersectionT (_, rep), UseT (_, u)
      when List.mem u (InterRep.members rep) ->
      ()

    | _, UseT (use_op, UnionT (r, rep)) -> (
      match UnionRep.quick_mem l rep with
      | Some true -> ()
      | Some false ->
        let r = match UnionRep.enum_base rep with
          | None -> r
          | Some base -> replace_reason_const (desc_of_t base) r
        in
        rec_flow cx trace (l, UseT (use_op, EmptyT r))
      | None ->
        (* Try the branches of the union in turn, with the goal of selecting the
           correct branch. This process is reused for intersections as well. See
           comments on try_union and try_intersection. *)
        try_union cx trace l r rep
    )

    (* maybe and optional types are just special union types *)

    | (t1, UseT (use_op, MaybeT(t2))) ->
      rec_flow cx trace (t1, UseT (use_op, t2))

    | (t1, UseT (use_op, OptionalT(t2))) ->
      rec_flow cx trace (t1, UseT (use_op, t2))

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

    | IntersectionT _, TestPropT (reason, prop, tout) ->
      rec_flow cx trace (l, GetPropT (reason, prop, tout))

    (** extends **)
    | IntersectionT (_, rep),
      UseT (use_op, ExtendsT (try_ts_on_failure, l, u)) ->
      let ts = InterRep.members rep in
      assert (ts <> []);
      (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
      rec_flow cx trace
        (List.hd ts,
         UseT (use_op, ExtendsT ((List.tl ts) @ try_ts_on_failure, l, u)))

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
      UseT (use_op, ObjT (r, { flags; props_tmap; proto_t; dict_t }))
      when SMap.cardinal (Context.find_props cx props_tmap) > 1 ->
      iter_real_props cx props_tmap (fun x p ->
        let pmap = SMap.singleton x p in
        let id = Context.make_property_map cx pmap in
        let obj = mk_objecttype ~flags dict_t id ObjProtoT.t in
        rec_flow cx trace (l, UseT (use_op, ObjT (r, obj)))
      );
      rec_flow cx trace (l, UseT (use_op, proto_t))

    (** predicates: prevent a predicate upper bound from prematurely decomposing
        an intersection lower bound *)
    | IntersectionT _, PredicateT (pred, tout) ->
      predicate cx trace tout l pred

    (* same for guards *)
    | IntersectionT _, GuardT (pred, result, tout) ->
      guard cx trace l pred result tout

    (** ObjAssignT copies multiple properties from its incoming LB.
        Here we simulate a merged object type by iterating over the
        entire intersection. *)
    | IntersectionT (_, rep), ObjAssignT (_, _, _, _, false) ->
      InterRep.members rep |> List.iter (fun t -> rec_flow cx trace (t, u))

    (** This duplicates the (_, ReposLowerT u) near the end of this pattern
        match but has to appear here to preempt the (IntersectionT, _) in
        between so that we reposition the entire intersection. *)
    | IntersectionT _, ReposLowerT (reason_op, u) ->
      rec_flow cx trace (reposition cx ~trace reason_op l, u)

    (** All other pairs with an intersection lower bound come here. Before
        further processing, we ensure that the upper bound is concretized. See
        prep_try_intersection for details. **)

    (* (After the above preprocessing step, try the branches of the intersection
       in turn, with the goal of selecting the correct branch. This process is
       reused for unions as well. See comments on try_union and
       try_intersection.)  *)

    | IntersectionT (r, rep), u ->
      prep_try_intersection cx trace
        (reason_of_use_t u) (parts_to_replace u) [] u r rep

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

    (************************************************************************)
    (* mapping over type structures                                         *)
    (************************************************************************)

    | TypeMapT (r, kind, t1, t2), _ ->
      rec_flow cx trace (t1, MapTypeT (r, kind, t2, Upper u))

    | _, UseT (_, TypeMapT (r, kind, t1, t2)) ->
      rec_flow cx trace (t1, MapTypeT (r, kind, t2, Lower l))

    (************************************************************************)
    (* exact object types *)
    (************************************************************************)

    (* ExactT<X> comes from annotation, may behave as LB or UB *)

    (* when $Exact<LB> ~> UB, forward to MakeExactT *)
    | ExactT (r, t), _ ->
      rec_flow cx trace (t, MakeExactT (r, Upper u))

    (* exact ObjT LB ~> $Exact<UB>. unify *)
    | ObjT (_, { flags; _ }), UseT (_, ExactT (r, t))
      when flags.exact && sealed_in_op r flags.sealed ->
      rec_flow cx trace (t, MakeExactT (r, Lower l))

    (* inexact LB ~> $Exact<UB>. error *)
    | _, UseT (_, ExactT _) ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.EIncompatibleWithExact reasons)

    (* LB ~> MakeExactT (_, UB) exactifies LB, then flows result to UB *)

    (* exactify incoming LB object type, flow to UB *)
    | ObjT (r, obj), MakeExactT (_, Upper u) ->
      let exactobj = { obj with flags = { obj.flags with exact = true } } in
      rec_flow cx trace (ObjT (r, exactobj), u)

    (* exactify incoming UB object type, flow to LB *)
    | ObjT (ru, obj_u), MakeExactT (_, Lower ObjT (rl, obj_l)) ->
      (* check for extra props in LB, then forward to standard obj ~> obj *)
      let xl = { obj_l with flags = { obj_l.flags with exact = true } } in
      let xu = { obj_u with flags = { obj_u.flags with exact = true } } in
      iter_real_props cx obj_l.props_tmap (fun prop_name _ ->
        if not (Context.has_prop cx obj_u.props_tmap prop_name)
        then
          let rl = replace_reason_const (RProperty (Some prop_name)) rl in
          add_output cx trace (FlowError.EPropNotFound (rl, ru))
      );
      rec_flow_t cx trace (ObjT (rl, xl), ObjT (ru, xu))

    (* unsupported kind *)
    | _, MakeExactT _ ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.EUnsupportedExact reasons)

    (**************************************************************************)
    (* TestPropT is emitted for property reads in the context of branch tests.
       Such tests are always non-strict, in that we don't immediately report an
       error if the property is not found not in the object type. Instead, if
       the property is not found, we control the result type of the read based
       on the flags on the object type. For exact sealed object types, the
       result type is `void`; otherwise, it is "unknown". Indeed, if the
       property is not found in an exact sealed object type, we can be sure it
       won't exist at run time, so the read will return undefined; but for other
       object types, the property *might* exist at run time, and since we don't
       know what the type of the property would be, we set things up so that the
       result of the read cannot be used in any interesting way. *)
    (**************************************************************************)

    | _, TestPropT (reason_op, propref, tout) ->
      let t = tvar_with_constraint cx ~trace ~derivable:true
        (ReposLowerT (reason_op, UseT (UnknownUse, tout)))
      in
      let lookup_kind = NonstrictReturning (match l with
        | ObjT (_, { flags; _ })
            when flags.exact ->
          if sealed_in_op reason_op flags.sealed then
            let name = name_of_propref propref in
            let r = replace_reason_const (RMissingProperty name) reason_op in
            Some (VoidT r, t)
          else
            (* unsealed, so don't return anything on lookup failure *)
            None
        | _ ->
          (* Note: a lot of other types could in principle be considered
             "exact". For example, new instances of classes could have exact
             types; so could `super` references (since they are statically
             rather than dynamically bound). However, currently we don't support
             any other exact types. Considering exact types inexact is sound, so
             there is no problem falling back to the same conservative
             approximation we use for inexact types in those cases. *)
          let name = name_of_propref propref in
          let r = replace_reason_const (RUnknownProperty name) reason_op in
          Some (MixedT (r, Mixed_everything), t)
      ) in
      let lookup =
        LookupT (reason_op, lookup_kind, [], propref, RWProp (t, Read))
      in rec_flow cx trace (l, lookup)


    (*******************************************)
    (* Refinement based on function predicates *)
    (*******************************************)

    (** Call to predicated (latent) functions *)

    (* Calls to functions appearing in predicate refinement contexts dispatch
       to this case. Here, the return type of the function holds the predicate
       that will refine the incoming `unrefined_t` and flow a filtered
       (refined) version of this type into `fresh_t`.

       What is important to note here is that `return_t` has no access to the
       function's parameter names. It will simply be an `OpenPredT` containing
       mappings from symbols (Key.t) that are (hopefully) the function's
       parameters to predicates. In other words, it is an "open" predicate over
       (free) variables, which *should* be the function's parameters.

       The `CallLatentPredT` use contains the index of the argument under
       refinement. By combining this information with the names of the
       parameters in `params_names`, we can arrive to the actual name (Key.t)
       of the parameter that gets refined, which can be used as a key into the
       `OpenPredT` that is expected to eventually flow to `return_t`.
       Effectively, we are substituting the actual parameter to the refining
       call (here in the form of the index of the argument to the call) to the
       formal parameter of the function, and this information is stored in
       `CallOpenPredT` of the produced flow.

       Problematic cases (e.g. when the refining index is out of bounds w.r.t.
       `params_names`) raise errors, but also propagate the unrefined types
       (as if the refinement never took place).
    *)
    | FunT (_, _, _, {
        params_names = Some pn;
        return_t;
        is_predicate = true;
        _
      }),
      CallLatentPredT (reason, sense, index, unrefined_t, fresh_t) ->
      (* TODO: for the moment we only support simple keys (empty projection)
         that exactly correspond to the function's parameters *)

      let key_or_err = try
        Utils_js.OK (List.nth pn (index-1), [])
      with
        | Invalid_argument _ ->
          Utils_js.Err ("Negative refinement index.",
            (reason_of_t l, reason_of_use_t u))
        | Failure msg when msg = "nth" ->
          let r1 = replace_reason (fun desc -> RCustom (
            spf "%s that uses predicate on parameter at position %d"
              (string_of_desc desc)
              index
          )) reason in
          let r2 = replace_reason (fun desc -> RCustom (
            spf "%s with %d parameters"
              (string_of_desc desc)
              (List.length pn)
          )) (reason_of_t l) in
          Utils_js.Err ("This is incompatible with", (r1, r2))
      in
      (match key_or_err with
      | Utils_js.OK key -> rec_flow cx trace
          (return_t, CallOpenPredT (reason, sense, key, unrefined_t, fresh_t))
      | Utils_js.Err (msg, reasons) ->
        add_output cx trace (FlowError.ECustom (reasons, msg));
        rec_flow_t cx trace (unrefined_t, fresh_t))


    (* Fall through all the remaining cases *)
    | _, CallLatentPredT (_,_,_,unrefined_t, fresh_t) ->
      rec_flow_t cx trace (unrefined_t, fresh_t)

    (** Trap the return type of a predicated function *)

    | OpenPredT (_, _, p_pos, p_neg),
      CallOpenPredT (_, sense, key, unrefined_t, fresh_t) ->
      begin
        let preds = if sense then p_pos else p_neg in
        match Key_map.get key preds with
        | Some p -> rec_flow cx trace (unrefined_t, PredicateT (p, fresh_t))
        | _ -> rec_flow_t cx trace (unrefined_t, fresh_t)
      end

    (* Any other flow to `CallOpenPredT` does not actually refine the
       type in question so we just fall back to regular flow. *)
    | _, CallOpenPredT (_, _, _, unrefined_t, fresh_t) ->
      rec_flow_t cx trace (unrefined_t, fresh_t)

    (********************************)
    (* Function-predicate subtyping *)
    (********************************)

    (* When decomposing function subtyping for predicated functions we need to
     * pair-up the predicates that each of the two functions established
     * before we can check for predicate implication. The predicates encoded
     * inside the two `OpenPredT`s refer to the formal parameters of the two
     * functions (which are not the same). `SubstOnPredT` is a use that does
     * this matching by carrying a substitution (`subst`) from keys from the
     * function in the left-hand side to keys in the right-hand side.
     *
     * Each matched pair of predicates is subsequently checked for consistency.
     *)
    | OpenPredT (_, t1, _, _),
      SubstOnPredT (_, _, OpenPredT (_, t2, p_pos_2, p_neg_2))
      when Key_map.(is_empty p_pos_2 && is_empty p_neg_2) ->
      rec_flow_t cx trace (t1, t2)

    | OpenPredT _, UseT (_, OpenPredT _) ->
      let loc = loc_of_reason (reason_of_use_t u) in
      add_output cx trace FlowError.(EInternal (loc, OpenPredWithoutSubst))

    (*********************************************)
    (* Using predicate functions as regular ones *)
    (*********************************************)

    | OpenPredT (_, l, _, _), _ -> rec_flow cx trace (l, u)

    (********************)
    (* mixin conversion *)
    (********************)

    (* A class can be viewed as a mixin by extracting its immediate properties,
       and "erasing" its static and super *)

    | ThisClassT (InstanceT (_, _, _, instance)), MixinT (r, tvar) ->
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      rec_flow cx trace (
        ThisClassT (InstanceT (r, static, super, instance)),
        UseT (UnknownUse, tvar)
      )

    | PolyT (xs, ThisClassT (InstanceT (_, _, _, instance))),
      MixinT (r, tvar) ->
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      rec_flow cx trace (
        PolyT (xs, ThisClassT (InstanceT (r, static, super, instance))),
        UseT (UnknownUse, tvar)
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
       fall-through case below.The exception is when the PolyT has type
       parameters with defaults. *)
    | (PolyT (ids,t), SpecializeT(reason_op,reason_tapp,cache,ts,tvar))
        when ts <> [] || (poly_minimum_arity ids < List.length ids) ->
      let t_ = instantiate_poly_with_targs cx trace
        ~reason_op ~reason_tapp ~cache (ids,t) ts in
      rec_flow_t cx trace (t_, tvar)

    | PolyT (tps, _), VarianceCheckT(_, ts, polarity) ->
      variance_check cx polarity (tps, ts)

    | PolyT (tparams, _), TypeAppVarianceCheckT (_, reason_tapp, targs) ->
      let minimum_arity = poly_minimum_arity tparams in
      let maximum_arity = List.length tparams in
      let reason_arity =
        let tp1, tpN = List.hd tparams, List.hd (List.rev tparams) in
        let loc = Loc.btwn (loc_of_reason tp1.reason) (loc_of_reason tpN.reason) in
        mk_reason (RCustom "See type parameters of definition here") loc in
      if List.length targs > maximum_arity
      then add_output cx trace
        (FlowError.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity));
      let unused_targs = List.fold_left (fun targs { default; polarity; _ } ->
        match default, targs with
        | None, [] ->
          (* fewer arguments than params but no default *)
          add_output cx trace (FlowError.ETooFewTypeArgs
            (reason_tapp, reason_arity, minimum_arity));
          []
        | _, [] -> []
        | _, (t1, t2)::targs ->
          (match polarity with
          | Positive -> rec_flow_t cx trace (t1, t2)
          | Negative -> rec_flow_t cx trace (t2, t1)
          | Neutral -> rec_unify cx trace t1 t2);
          targs
      ) targs tparams in
      assert (unused_targs = [])

    (* empty targs specialization of non-polymorphic classes is a no-op *)
    | (ClassT _ | ThisClassT _), SpecializeT(_,_,_,[],tvar) ->
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
      rec_flow cx trace (reposition cx ~trace reason_op l, u)

    (* When do we consider a polymorphic type <X:U> T to be a subtype of another
       polymorphic type <X:U'> T'? This is the subject of a long line of
       research. A rule that works (Cardelli/Wegner) is: force U = U', and prove
       that T is a subtype of T' for any X:U'. A more general rule that proves
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
       that, for all X:U', there is some X:U such that T is a subtype of T'. But
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
    | (_, UseT (use_op, PolyT (ids, t))) ->
        generate_tests cx (reason_of_t l) ids (fun map_ ->
          rec_flow cx trace (l, UseT (use_op, subst cx map_ t))
        )

    (* TODO: ideally we'd do the same when lower bounds flow to a
       this-abstracted class, but fixing the class is easier; might need to
       revisit *)
    | (_, UseT (use_op, ThisClassT i)) ->
      let r = reason_of_t l in
      rec_flow cx trace (l, UseT (use_op, fix_this_class cx trace r i))

    (** This rule is hit when a polymorphic type appears outside a
        type application expression - i.e. not followed by a type argument list
        delimited by angle brackets.
        We want to require full expressions in type positions like annotations,
        but allow use of polymorphically-typed values - for example, in class
        extends clauses and at function call sites - without explicit type
        arguments, since typically they're easily inferred from context.
      *)
    | (PolyT (ids,t), _) ->
      let reason_op = reason_of_use_t u in
      let reason_tapp = reason_of_t l in
      begin match u with
      | UseT (_, TypeT _) ->
        if Context.enforce_strict_type_args cx then
          add_output cx trace (FlowError.EMissingTypeArgs (reason_op, ids))
        else
          let inst = instantiate_poly_default_args
            cx trace ~reason_op ~reason_tapp (ids, t) in
          rec_flow cx trace (inst, u)
      (* Special case for <fun>.apply and <fun>.call, which could be used to
         simulate method calls on instances of generic classes, thereby
         bypassing the specialization cache that is used in TypeAppT ~> MethodT
         to avoid non-termination. So, we must use the specialization cache
         here, too. *)
      | CallT _ when
          is_method_call_reason "apply" reason_op ||
          is_method_call_reason "call" reason_op
          ->
        let t_ = instantiate_poly cx trace
          ~reason_op ~reason_tapp ~cache:true (ids,t) in
        rec_flow cx trace (t_, u)
      | _ ->
        let t_ = instantiate_poly cx trace ~reason_op ~reason_tapp (ids,t) in
        rec_flow cx trace (t_, u)
      end

    (* when a this-abstracted class flows to upper bounds, fix the class *)
    | (ThisClassT i, _) ->
      let r = reason_of_use_t u in
      rec_flow cx trace (fix_this_class cx trace r i, u)

    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    | FunT (_, _, _,
        { this_t = o1; params_tlist = tins1; params_names = p1;
          is_predicate = ip1; return_t = t1; _ }),
      UseT (use_op, FunT (reason, _, _,
        { this_t = o2; params_tlist = tins2; params_names = p2;
          is_predicate = ip2; return_t = t2; _ }))
      ->
      rec_flow cx trace (o2, UseT (use_op, o1));
      multiflow cx trace reason (tins2, tins1);

      (* Well-formedness adjustment: If this is predicate function subtyping,
         make sure to apply a latent substitution on the right-hand use to
         bridge the mismatch of the parameter naming. Otherwise, proceed with
         the subtyping of the return types normally. In general it should
         hold as an invariant that OpenPredTs (where free variables appear)
         should not flow to other OpenPredTs without wrapping the latter in
         SubstOnPredT.
      *)
      if ip2 then
        if not ip1 then
          (* Non-predicate functions are incompatible with predicate ones
             TODO: somehow the original flow needs to be propagated as well *)
          add_output cx trace (FlowError.ECustom (
            (reason_of_t l, reason_of_use_t u),
            "Function is incompatible with"))
        else
          begin match p1, p2 with
          | Some s1, Some s2 ->
            let reason = replace_reason (fun desc ->
              RCustom (spf "predicate of %s" (string_of_desc desc))
            ) (reason_of_t t2) in
            if List.length s1 < List.length s2 then
              (* Flag an error if predicate counts do not coincide
                 TODO: somehow the original flow needs to be propagated
                 as well *)
              let mod_reason n = replace_reason (fun _ ->
                RCustom (spf "predicate function with %d arguments" n)
              ) in
              add_output cx trace (FlowError.ECustom (
                (mod_reason (List.length s1) (reason_of_t l),
                 mod_reason (List.length s2) (reason_of_use_t u)),
                "Predicate function is incompatible with"))
            else
              (* NOTE: do not use List.combine here *)
              let subst = Utils_js.zip s1 s2 |>
                List.fold_left (fun m (k,v) -> SMap.add k (v,[]) m) SMap.empty
              in
              rec_flow cx trace (t1, SubstOnPredT (reason, subst, t2))
          | _ ->
            let loc = loc_of_reason (reason_of_use_t u) in
            add_output cx trace
              FlowError.(EInternal (loc, PredFunWithoutParamNames))
          end
      else
        rec_flow cx trace (t1, UseT (use_op, t2))

    | FunT (reason_fundef, _, _,
        { this_t = o1; params_tlist = tins1; return_t = t1;
          closure_t = func_scope_id; changeset; _ }),
      CallT (reason_callsite,
        { this_t = o2; params_tlist = tins2; return_t = t2;
          closure_t = call_scope_id; _})
      ->
      Ops.push reason_callsite;
      rec_flow cx trace (o2, UseT (FunCallThis reason_callsite, o1));
      multiflow cx trace reason_callsite (tins2, tins1);
      Ops.pop ();

      (* flow return type of function to the tvar holding the return type of the
         call. clears the op stack because the result of the call is not the
         call itself. *)
      let ops = Ops.clear () in
      rec_flow_t cx trace (reposition cx ~trace reason_callsite t1, t2);
      Ops.set ops;

      (if Context.is_verbose cx then
        prerr_endlinef "%shavoc_call_env fundef %s callsite %s"
          (Context.pid_prefix cx)
          (Debug_js.string_of_reason cx reason_fundef)
          (Debug_js.string_of_reason cx reason_callsite));
      havoc_call_env cx func_scope_id call_scope_id changeset;

    | (AnyFunT reason_fundef | AnyT reason_fundef),
      CallT (reason_op, { this_t; params_tlist; return_t; _}) ->
      let any = AnyT.why reason_fundef in
      rec_flow_t cx trace (this_t, any);
      List.iter (fun t -> rec_flow_t cx trace (t, any)) params_tlist;
      rec_flow_t cx trace (AnyT.why reason_op, return_t)

    (* Special handlers for builtin functions *)

    | CustomFunT (_, ObjectAssign),
      CallT (reason_op, { params_tlist = dest_t::ts; return_t; _ }) ->
      let t = chain_objects cx ~trace reason_op dest_t ts in
      rec_flow_t cx trace (t, return_t)

    | CustomFunT (_, ObjectGetPrototypeOf),
      CallT (reason_op, { params_tlist = obj::_; return_t; _ }) ->
      rec_flow cx trace (
        obj,
        GetPropT(reason_op, Named (reason_op, "__proto__"), return_t)
      );

    (* If you haven't heard, React is a pretty big deal. *)

    | CustomFunT (_, ReactCreateElement),
      CallT (reason_op, { params_tlist = c::o::_; return_t; _ }) ->
      Ops.push reason_op;
      rec_flow cx trace (c, ReactCreateElementT (reason_op, o, return_t));
      Ops.pop ()

    | _, ReactCreateElementT (reason_op, config, tout) ->
      let elem_reason = replace_reason_const (RReactElement None) reason_op in
      (match l with
      | ClassT _ ->
        let react_class =
          get_builtin_typeapp cx ~trace reason_op "ReactClass" [config]
        in
        rec_flow_t cx trace (l, react_class)
      | FunT _ ->
        let return_t =
          get_builtin_typeapp cx ~trace elem_reason "React$Element" [AnyT.t]
        in
        let return_t = MaybeT return_t in
        let context_t = AnyT.t in
        let call_t =
          CallT (reason_op, mk_functiontype [config; context_t] return_t)
        in
        rec_flow cx trace (l, call_t)
      | StrT _
      | SingletonStrT _ ->
        let jsx_intrinsics =
          get_builtin_type cx ~trace reason_op "$JSXIntrinsics" in
        rec_flow_t cx trace (l, KeysT (reason_op, jsx_intrinsics))
      | AnyFunT _ -> ()
      | AnyObjT _ -> ()
      | _ ->
        add_output cx trace (FlowError.EIncompatible (l, u))
      );
      rec_flow_t cx trace (
        get_builtin_typeapp cx ~trace elem_reason "React$Element" [config],
        tout
      )

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
      UseT (use_op, ObjT (ureason, {
        props_tmap = uflds;
        proto_t = uproto;
        dict_t = udict; _ })) ->

      (* if inflowing type is literal (thus guaranteed to be
         unaliased), propertywise subtyping is sound *)
      let ldesc = desc_of_reason lreason in
      let lit = match ldesc with
      | RObjectLit
      | RSpreadOf _
      | RObjectPatternRestProp
      | RFunction _
      | RArrowFunction _
      | RReactElementProps _
      | RJSXElementProps _ -> true
      | _ -> lflags.frozen
      in

      (* If both are dictionaries, ensure the keys and values are compatible
         with each other. *)
      (match ldict, udict with
        | Some {key = lk; value = lv; _}, Some {key = uk; value = uv; _} ->
          rec_unify cx trace lk uk;
          rec_unify cx trace lv uv
        | _ -> ());

      (* Properties in u must either exist in l, or match l's indexer. *)
      iter_real_props cx uflds (fun s up ->
        let reason_prop = replace_reason_const (RProperty (Some s)) ureason in
        let propref = Named (reason_prop, s) in
        match Context.get_prop cx lflds s, ldict with
        | Some lp, _ ->
          if lit then (
            (* prop from unaliased LB: check <:, then make exact *)
            (match Property.read_t lp, Property.read_t up with
            | Some lt, Some ut -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ());
            (* Band-aid to avoid side effect in speculation mode. Even in
               non-speculation mode, the side effect here is racy, so it either
               needs to be taken out or replaced with something more
               robust. Tracked by #11299251. *)
            if not (Speculation.speculating ()) then
              Context.set_prop cx lflds s up
          ) else (
            (* prop from aliased LB *)
            rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
          )
        | None, Some { key; value; dict_polarity; _ }
            when not (is_dictionary_exempt s) ->
          rec_flow_t cx trace (string_key s reason_prop, key);
          let lp = Field (value, dict_polarity) in
          let up = match up with
          | Field (OptionalT ut, upolarity) ->
            Field (ut, upolarity)
          | _ -> up
          in
          if lit
          then
            match Property.read_t lp, Property.read_t up with
            | Some lt, Some ut -> rec_flow cx trace (lt, UseT (use_op, ut))
            | _ -> ()
          else
            rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
        | _ ->
          (* property doesn't exist in inflowing type *)
          match up with
          | Field (OptionalT _, _) when lflags.exact ->
            (* if property is marked optional or otherwise has a maybe type,
               and if inflowing type is exact (i.e., it is not an
               annotation), then we add it to the inflowing type as
               an optional property *)
            (* Band-aid to avoid side effect in speculation mode. Even in
               non-speculation mode, the side effect here is racy, so it either
               needs to be taken out or replaced with something more
               robust. Tracked by #11299251. *)
            if not (Speculation.speculating ()) then
              Context.set_prop cx lflds s up;
          | _ ->
            (* otherwise, look up the property in the prototype *)
            let strict = match sealed_in_op ureason lflags.sealed, ldict with
            | false, None -> ShadowRead (Some lreason, Nel.one lflds)
            | true, None -> Strict lreason
            | _ -> NonstrictReturning None
            in
            rec_flow cx trace (lproto,
              LookupT (ureason, strict, [], propref, LookupProp up))
            (* TODO: instead, consider extending inflowing type with s:t2 when it
               is not sealed *)
      );

      (* Any properties in l but not u must match indexer *)
      (match udict with
      | None -> ()
      | Some { key; value; dict_polarity; _ } ->
        iter_real_props cx lflds (fun s lp ->
          if not (Context.has_prop cx uflds s)
          then (
            rec_flow_t cx trace (string_key s lreason, key);
            let lp = match lp with
            | Field (OptionalT lt, lpolarity) ->
              Field (lt, lpolarity)
            | _ -> lp
            in
            let up = Field (value, dict_polarity) in
            if lit
            then
              match Property.read_t lp, Property.read_t up with
              | Some lt, Some ut -> rec_flow cx trace (lt, UseT (use_op, ut))
              | _ -> ()
            else
              let reason_prop = replace_reason_const (RProperty (Some s)) lreason in
              let propref = Named (reason_prop, s) in
              rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
          )));

      rec_flow cx trace (l, UseT (use_op, uproto))

    (* InstanceT -> ObjT *)

    | InstanceT (lreason, _, super, {
        fields_tmap = lflds;
        methods_tmap = lmethods; _ }),
      UseT (use_op, ObjT (ureason, {
        props_tmap = uflds;
        proto_t = uproto; _ })) ->

      let lflds =
        let fields_tmap = Context.find_props cx lflds in
        let methods_tmap = Context.find_props cx lmethods in
        SMap.union fields_tmap methods_tmap
      in

      iter_real_props cx uflds (fun s up ->
        let reason_prop = replace_reason_const (RProperty (Some s)) ureason in
        let propref = Named (reason_prop, s) in
        match SMap.get s lflds with
        | Some lp ->
          rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
        | _ ->
          match up with
          | Field (OptionalT ut, upolarity) ->
            rec_flow cx trace (l,
              LookupT (reason_prop, NonstrictReturning None, [], propref,
                LookupProp (Field (ut, upolarity))))
          | _ ->
            rec_flow cx trace (super,
              LookupT (reason_prop, Strict lreason, [], propref,
                LookupProp up))
      );

      rec_flow cx trace (l, UseT (use_op, uproto))

    (* For some object `x` and constructor `C`, if `x instanceof C`, then the
       object is a subtype. *)
    | ObjT (lreason, { proto_t; _ }),
      UseT (_, InstanceT (_, _, _, { structural = false; _ })) ->
      rec_flow cx trace (reposition cx ~trace lreason proto_t, u)

    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | (ObjT (reason, _) | InstanceT (reason, _, _, _)),
      (UseT (_, FunT (reason_op, _, _, _)) |
       UseT (_, AnyFunT reason_op) |
       CallT (reason_op, _)) ->
      let tvar = mk_tvar cx (
        replace_reason (fun desc ->
          RCustom (spf "%s used as a function" (string_of_desc desc))
        ) reason
      ) in
      lookup_prop cx trace l reason_op reason_op (Strict reason) "$call"
        (RWProp (tvar, Read));
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

    | (ObjT (reason, { props_tmap = mapr; _ }), UseT (_, ShapeT (proto))) ->
        (* TODO: ShapeT should have its own reason *)
        let reason_op = reason_of_t proto in
        Context.iter_props cx mapr (fun x p ->
          match Property.read_t p with
          | Some t ->
            let reason_prop = replace_reason (fun desc ->
              RPropertyOf (x, desc)
            ) reason in
            let propref = Named (reason_prop, x) in
            let t = filter_optional cx ~trace reason_prop t in
            rec_flow cx trace (proto, SetPropT (reason_op, propref, t))
          | None ->
            add_output cx trace
              (FlowError.EPropAccess ((reason, reason_op), Some x, p, Read))
        )

    | (_, UseT (_, ShapeT (o))) ->
        let reason = reason_of_t o in
        rec_flow cx trace (l, ObjAssignT(reason, o, AnyT.t, [], false))

    | (_, UseT (_, DiffT (o1, o2))) ->
        let reason = reason_of_t l in
        let t2 = mk_tvar cx reason in
        rec_flow cx trace (o2, ObjRestT (reason, [], t2));
        rec_flow cx trace (t2, ObjAssignT(reason, l, o1, [], true))

    (********************************************)
    (* array types deconstruct into their parts *)
    (********************************************)

    | ArrT (r1, t1, ts1), UseT (_, ArrT (r2, t2, ts2)) ->
      let lit1 = (desc_of_reason r1) = RArrayLit in
      let arr1 = (desc_of_reason r1) = RArrayType in
      let tup2 = (desc_of_reason r2) = RTupleType in
      array_flow cx trace lit1 arr1 r1 tup2 (ts1, t1, ts2, t2)

    (**************************************************)
    (* instances of classes follow declared hierarchy *)
    (**************************************************)

    | (InstanceT _, UseT (use_op, (InstanceT _ as u))) ->
      rec_flow cx trace (l, UseT (use_op, ExtendsT([],l,u)))

    | (InstanceT (_,_,super,instance),
       UseT (_, ExtendsT(_, _, InstanceT (_,_,_,instance_super)))) ->
      if instance.class_id = instance_super.class_id
      then
        flow_type_args cx trace instance instance_super
      else
        rec_flow cx trace (super, u)

    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)

    | (ClassT(it), UseT (_, TypeT(r,t))) ->
      (* a class value annotation becomes the instance type *)
      rec_flow cx trace (it, BecomeT (r, t))

    | (FunT(_, _, prototype, _), UseT (_, TypeT(reason, t))) ->
      (* a function value annotation becomes the prototype type *)
      rec_flow cx trace (prototype, BecomeT (reason, t))

    | AnyT _, UseT (_, TypeT (reason, t)) ->
      (* any can function as class or function type, hence ok for annotations *)
      rec_flow cx trace (l, BecomeT (reason, t))

    | (TypeT(_,l), UseT (_, TypeT(_,u))) ->
      rec_unify cx trace l u

    (* non-class/function values used in annotations are errors *)
    | _, UseT (_, TypeT _) ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.EValueUsedAsType reasons)

    | (ClassT(l), UseT (use_op, ClassT(u))) ->
      rec_flow cx trace (l, UseT (use_op, u))

    | FunT (_,static1,prototype,_),
      UseT (_, ClassT (InstanceT (_,static2,_, _) as u_)) ->
      rec_unify cx trace static1 static2;
      rec_unify cx trace prototype u_

    (*********************************************************)
    (* class types derive instance types (with constructors) *)
    (*********************************************************)

    | ClassT (this),
      ConstructorT (reason_op, args, t) ->
      let reason_o =
        replace_reason_const RConstructorReturn (reason_of_t this) in
      Ops.push reason_op;
      (* call this.constructor(args) *)
      let ret = mk_tvar_where cx reason_op (fun t ->
        let funtype = mk_methodtype this args t in
        let propref = Named (reason_o, "constructor") in
        rec_flow cx trace (
          this,
          MethodT (reason_op, reason_o, propref, funtype)
        );
      ) in
      (* return this *)
      rec_flow cx trace (ret, ObjTestT(reason_op, this, t));
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
      let reason_c = replace_reason_const RNewObject reason in
      let proto_reason = reason_of_t proto in
      let sealed = UnsealedInFile (Loc.source (loc_of_reason proto_reason)) in
      let flags = { default_flags with sealed } in
      let dict = None in
      let pmap = Context.make_property_map cx SMap.empty in
      let new_obj = ObjT (reason_c, mk_objecttype ~flags dict pmap proto) in
      (** call function with this = new_obj, params = args **)
      rec_flow_t cx trace (new_obj, this);
      multiflow cx trace reason_op (args, params);
      (** if ret is object-like, return ret; otherwise return new_obj **)
      let reason_o = replace_reason_const RConstructorReturn reason in
      rec_flow cx trace (ret, ObjTestT(reason_o, new_obj, t))

    | AnyFunT reason_fundef, ConstructorT (reason_op, args, t) ->
      let reason_o = replace_reason_const RConstructorReturn reason_fundef in
      List.iter (fun t -> rec_flow_t cx trace (t, AnyT.why reason_op)) args;
      rec_flow_t cx trace (AnyObjT reason_o, t);

    (* Since we don't know the signature of a method on AnyFunT, assume every
       parameter is an AnyT. *)
    | (AnyFunT _, MethodT (reason_op, _, _, { params_tlist; return_t; _})) ->
      let any = AnyT.why reason_op in
      List.iter (fun t -> rec_flow_t cx trace (t, any)) params_tlist;
      rec_flow_t cx trace (any, return_t)

    (*************************)
    (* statics can be read   *)
    (*************************)

    | InstanceT (_, static, _, _), GetStaticsT (_, t) ->
      rec_flow_t cx trace (static, t)

    (* GetStaticsT is only ever called on the instance type of a ClassT. There
     * is exactly one place where we create a ClassT with an ObjT instance type:
     * $Facebookism$Mixin. This rule should only fire for that case. *)
    | ObjT _, GetStaticsT _ ->
      (* Mixins don't have statics at all, so we can just prune here. *)
      ()

    | ObjProtoT reason, GetStaticsT (_, t) ->
      (* ObjProtoT not only serves as the instance type of the root class, but
         also as the statics of the root class. *)
      let static_reason = replace_reason (fun desc ->
        RStatics desc
      ) reason in
      let static = ObjProtoT static_reason in
      rec_flow_t cx trace (static, t)

    (********************************************************)
    (* instances of classes may have their fields looked up *)
    (********************************************************)

    | InstanceT (lreason, _, super, instance),
      LookupT (reason_op, kind, try_ts_on_failure, (Named (_, x) as propref), action) ->
      let fields_pmap = Context.find_props cx instance.fields_tmap in
      let methods_pmap = Context.find_props cx instance.methods_tmap in
      let pmap = SMap.union fields_pmap methods_pmap in
      (match SMap.get x pmap with
      | None ->
        (* mixins=true for React.createClass components which have mixins (note:
           this is not the same as mixins for declared classes). If there are
           mixins, then lookup should become nonstrict, as the searched-for
           property may be found in a mixin. *)
        let kind = match instance.mixins, kind with
        | true, Strict _ -> NonstrictReturning None
        | _ -> kind
        in
        rec_flow cx trace (super,
          LookupT (reason_op, kind, try_ts_on_failure, propref, action))
      | Some p ->
        (* TODO: Replace AbstractT with abstract fields, then reuse
           perform_lookup_action here. *)
        (match action with
        | RWProp (t2, rw) ->
          (* The type of the property in the super class is abstract. The type
             of the property in this class may be abstract or not.  We want to
             unify just the underlying types, ignoring the abstract part.  *)
          let p, t2 = match p, t2 with
          | Field (AbstractT t1, polarity), AbstractT t2
          | Field (AbstractT t1, polarity), t2 -> Field (t1, polarity), t2
          | _ -> p, t2
          in
          (match rw, Property.access rw p with
          | Read, Some t1 -> rec_flow_t cx trace (t1, t2)
          | Write, Some t1 -> rec_flow_t cx trace (t2, t1)
          | _, None ->
            add_output cx trace
              (FlowError.EPropAccess ((lreason, reason_op), Some x, p, rw)))
        | LookupProp up ->
          let p, up = match p, up with
          | Field (AbstractT t, polarity), Field (AbstractT ut, upolarity) ->
            Field (t, polarity), Field (ut, upolarity)
          | Field (AbstractT t, polarity), up ->
            Field (t, polarity), up
          | _ -> p, up
          in
          rec_flow_p cx trace lreason reason_op propref (p, up)
        | SuperProp lp ->
          let p, lp = match p, lp with
          | Field (AbstractT t, polarity), Field (AbstractT lt, lpolarity) ->
            Field (t, polarity), Field (lt, lpolarity)
          | Field (AbstractT t, polarity), lp ->
            Field (t, polarity), lp
          | _ -> p, lp
          in
          rec_flow_p cx trace reason_op lreason propref (lp, p)))

    | InstanceT _, LookupT (reason_op, _, _, Computed _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_op in
      add_output cx trace FlowError.(EInternal (loc, InstanceLookupComputed))

    (********************************)
    (* ... and their fields written *)
    (********************************)

    | InstanceT (reason_c, _, super, instance),
      SetPropT (reason_op, Named (reason_prop, x), tin) ->
      Ops.push reason_op;
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      let strict = Strict reason_c in
      set_prop cx trace reason_prop reason_op strict super x fields tin;
      Ops.pop ();

    | InstanceT _, SetPropT (reason_op, Computed _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_op in
      add_output cx trace FlowError.(EInternal (loc, InstanceLookupComputed))

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | InstanceT (_, _, super, _), GetPropT (_, Named (_, "__proto__"), t) ->
      rec_flow_t cx trace (super, t)

    | InstanceT _ as instance, GetPropT (_, Named (_, "constructor"), t) ->
      rec_flow_t cx trace (ClassT instance, t)

    | InstanceT (reason_c, _, super, instance),
      GetPropT (reason_op, Named (reason_prop, x), tout) ->
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      let strict =
        if instance.mixins then NonstrictReturning None
        else Strict reason_c
      in
      get_prop cx trace reason_prop reason_op strict super x fields tout

    | InstanceT _, GetPropT (reason_op, Computed _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_op in
      add_output cx trace FlowError.(EInternal (loc, InstanceLookupComputed))

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | InstanceT (reason_c, _, super, instance),
      MethodT (reason_call, reason_lookup, Named (reason_prop, x), funtype)
      -> (* TODO: closure *)
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let methods = SMap.union fields_tmap methods_tmap in
      let funt = mk_tvar cx reason_lookup in
      let strict =
        if instance.mixins then NonstrictReturning None
        else Strict reason_c
      in
      get_prop cx trace reason_prop reason_lookup strict super x methods funt;

      (* suppress ops while calling the function. if `funt` is a `FunT`, then
         `CallT` will set its own ops during the call. if `funt` is something
         else, then something like `VoidT ~> CallT` doesn't need the op either
         because we want to point at the call and undefined thing. *)
      let ops = Ops.clear () in
      rec_flow cx trace (funt, CallT (reason_call, funtype));
      Ops.set ops

    | InstanceT _, MethodT (reason_call, _, Computed _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_call in
      add_output cx trace FlowError.(EInternal (loc, InstanceLookupComputed))

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

    | (ObjT (lreason, { props_tmap = mapr; _ }),
       ObjAssignT (reason_op, proto, t, props_to_skip, false)) ->
      Ops.push reason_op;
      Context.iter_props cx mapr (fun x p ->
        if not (List.mem x props_to_skip) then (
          (* move the reason to the call site instead of the definition, so
             that it is in the same scope as the Object.assign, so that
             strictness rules apply. *)
          let reason_prop =
            lreason
            |> replace_reason (fun desc -> RPropertyOf (x, desc))
            |> repos_reason (loc_of_reason reason_op)
          in
          match Property.read_t p with
          | Some t ->
            let propref = Named (reason_prop, x) in
            let t = filter_optional cx ~trace reason_prop t in
            rec_flow cx trace (proto, SetPropT (reason_prop, propref, t));
          | None ->
            add_output cx trace
              (FlowError.EPropAccess ((lreason, reason_op), Some x, p, Read))
        )
      );
      Ops.pop ();
      rec_flow_t cx trace (proto, t)

    | (InstanceT (lreason, _, _, { fields_tmap; methods_tmap; _ }),
       ObjAssignT (reason_op, proto, t, props_to_skip, false)) ->
      let fields_pmap = Context.find_props cx fields_tmap in
      let methods_pmap = Context.find_props cx methods_tmap in
      let pmap = SMap.union fields_pmap methods_pmap in
      pmap |> SMap.iter (fun x p ->
        if not (List.mem x props_to_skip) then (
          match Property.read_t p with
          | Some t ->
            let propref = Named (reason_op, x) in
            rec_flow cx trace (proto, SetPropT (reason_op, propref, t))
          | None ->
            add_output cx trace
              (FlowError.EPropAccess ((lreason, reason_op), Some x, p, Read))
        )
      );
      rec_flow_t cx trace (proto, t)

    (* AnyObjT has every prop, each one typed as `any`, so spreading it into an
       existing object destroys all of the keys, turning the result into an
       AnyObjT as well. TODO: wait for `proto` to be resolved, and then call
       `SetPropT (_, _, AnyT)` on all of its props. *)
    | AnyObjT _, ObjAssignT (reason, _, t, _, false) ->
      rec_flow_t cx trace (AnyObjT reason, t)

    | (ObjProtoT _, ObjAssignT (_, proto, t, _, false)) ->
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
      let map = Context.find_props cx mapr in
      let map = List.fold_left (fun map x -> SMap.remove x map) map xs in
      let proto = ObjProtoT reason in
      let o = mk_object_with_map_proto cx reason map proto in
      rec_flow_t cx trace (o, t)

    | (InstanceT (_, _, super, insttype), ObjRestT (reason, xs, t)) ->
      (* Spread fields from super into an object *)
      let obj_super = mk_tvar_where cx reason (fun tvar ->
        rec_flow cx trace (super, ObjRestT (reason, xs, tvar))
      ) in

      (* Spread fields from the instance into another object *)
      let map = Context.find_props cx insttype.fields_tmap in
      let map = List.fold_left (fun map x -> SMap.remove x map) map xs in
      let proto = ObjProtoT reason in
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

    | (ObjProtoT _, ObjRestT (reason, _, t)) ->
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
      let src_props = Context.find_props cx mapr in
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
      let desc = RFrozen (desc_of_reason reason_o) in
      let reason = replace_reason_const desc reason_op in

      let flags = {frozen = true; sealed = Sealed; exact = true;} in
      let new_obj = ObjT (reason, {objtype with flags}) in
      rec_flow_t cx trace (new_obj, t)

    (*******************************************)
    (* objects may have their fields looked up *)
    (*******************************************)

    | ObjT (reason_obj, o),
      LookupT (reason_op, strict, try_ts_on_failure, propref, action) ->
      let ops = Ops.clear () in
      (match get_obj_prop cx trace o propref reason_op with
      | Some p ->
        perform_lookup_action cx trace propref p reason_obj reason_op action
      | None ->
        let strict = match sealed_in_op reason_op o.flags.sealed, strict with
        | false, ShadowRead (strict, ids) ->
          ShadowRead (strict, Nel.cons o.props_tmap ids)
        | false, ShadowWrite ids ->
          ShadowWrite (Nel.cons o.props_tmap ids)
        | _ -> strict
        in
        rec_flow cx trace (o.proto_t,
          LookupT (reason_op, strict, try_ts_on_failure, propref, action)));
      Ops.set ops

    | AnyObjT reason_obj, LookupT (reason_op, _, _, propref, action) ->
      let p = Field (AnyT.why reason_op, Neutral) in
      perform_lookup_action cx trace propref p reason_obj reason_op action

    (*****************************************)
    (* ... and their fields written *)
    (*****************************************)

    | (ObjT (_, {flags; _}), SetPropT(_, Named (_, "constructor"), _)) ->
      if flags.frozen
      then
        let reasons = FlowError.ordered_reasons l u in
        add_output cx trace (FlowError.EMutationNotAllowed reasons)

    (** o.x = ... has the additional effect of o[_] = ... **)

    | (ObjT (_, { flags; _ }), SetPropT _) when flags.frozen ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.EMutationNotAllowed reasons)

    | ObjT (reason_obj, o), SetPropT (reason_op, propref, tin) ->
      write_obj_prop cx trace o propref reason_obj reason_op tin

    (* Since we don't know the type of the prop, use AnyT. *)
    | (AnyObjT _, SetPropT (reason_op, _, t)) ->
      rec_flow_t cx trace (t, AnyT.why reason_op)

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | ObjT (_, {proto_t = proto; _}), GetPropT (_, Named (_, "__proto__"), t) ->
      rec_flow_t cx trace (proto,t)

    | ObjT _, GetPropT (reason_op, Named (_, "constructor"), tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | ObjT (reason_obj, o), GetPropT (reason_op, propref, tout) ->
      let tout = tvar_with_constraint ~trace cx
        (ReposLowerT (reason_op, UseT (UnknownUse, tout)))
      in
      read_obj_prop cx trace o propref reason_obj reason_op tout

    | (AnyObjT _ | AnyT _), GetPropT (reason_op, _, tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | ObjT _, MethodT(_, _, Named (_, "constructor"), _) -> ()

    | ObjT (reason_obj, o),
      MethodT (reason_call, reason_lookup, propref, funtype) ->
      let t = mk_tvar_where cx reason_lookup (fun tout ->
        read_obj_prop cx trace o propref reason_obj reason_lookup tout
      ) in
      rec_flow cx trace (t, CallT (reason_call, funtype))

    (* Since we don't know the signature of a method on AnyObjT, assume every
       parameter is an AnyT. *)
    | (AnyObjT _ | AnyT _),
      MethodT (reason_op, _, _, { params_tlist; return_t; _}) ->
      let any = AnyT.why reason_op in
      List.iter (fun t -> rec_flow_t cx trace (t, any)) params_tlist;
      rec_flow_t cx trace (any, return_t)

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

    | (ObjT _ | AnyObjT _ | ArrT _), SetElemT (reason_op, key, tin) ->
      rec_flow cx trace (key, ElemT (reason_op, l, WriteElem tin))

    | (ObjT _ | AnyObjT _ | ArrT _), GetElemT (reason_op, key, tout) ->
      rec_flow cx trace (key, ElemT (reason_op, l, ReadElem tout))

    | (ObjT _ | AnyObjT _ | ArrT _),
      CallElemT (reason_call, reason_lookup, key, ft) ->
      let action = CallElem (reason_call, ft) in
      rec_flow cx trace (key, ElemT (reason_lookup, l, action))

    | _, ElemT (reason_op, (ObjT _ as o), action) ->
      let propref = match l with
      | StrT (reason_x, Literal x) ->
          let reason_prop = replace_reason_const (RProperty (Some x)) reason_x in
          Named (reason_prop, x)
      | _ -> Computed l
      in
      let u = match action with
      | ReadElem t -> GetPropT (reason_op, propref, t)
      | WriteElem t -> SetPropT (reason_op, propref, t)
      | CallElem (reason_call, ft) ->
        MethodT (reason_call, reason_op, propref, ft)
      in
      rec_flow cx trace (o, u)

    | _, ElemT (reason_op, AnyObjT _, action) ->
      let value = AnyT.why reason_op in
      perform_elem_action cx trace value action

    | AnyT _, ElemT (_, ArrT (_, value, _), action) ->
      perform_elem_action cx trace value action

    | l, ElemT (_, ArrT (_, value, ts), action) when numeric l ->
      let value = match l with
      | NumT (_, Literal (float_value, _)) ->
          begin try
            float_value
            |> int_of_float
            |> List.nth ts
          with _ ->
            value
          end
      | _ -> value
      in
      perform_elem_action cx trace value action

    | (ArrT _, GetPropT(reason_op, Named (_, "constructor"), tout)) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | (ArrT _, SetPropT(_, Named (_, "constructor"), _))
    | (ArrT _, MethodT(_, _, Named (_, "constructor"), _)) ->
      ()

    (**************************************************)
    (* array pattern can consume the rest of an array *)
    (**************************************************)

    | (ArrT (_, t, ts), ArrRestT (reason, i, tout)) ->
      let a = ArrT (reason, t, Core_list.drop ts i) in
      rec_flow_t cx trace (a, tout)

    (**************************************************)
    (* function types can be mapped over a structure  *)
    (**************************************************)

    | ArrT (_, t, ts), MapTypeT (reason_op, TupleMap, funt, k) ->
      let f x = mk_tvar_where cx reason_op (fun t ->
        rec_flow cx trace (funt, CallT (reason_op, mk_functiontype [x] t))
      ) in
      let t =
        let reason = replace_reason_const RArrayType reason_op in
        ArrT (reason, f t, List.map f ts)
      in
      continue cx trace t k

    | _, MapTypeT (reason, TupleMap, funt, k) ->
      let iter = get_builtin cx ~trace "$iterate" reason in
      let elem_t = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (iter, CallT (reason, mk_functiontype [l] t))
      ) in
      let t = ArrT (reason, elem_t, []) in
      rec_flow cx trace (t, MapTypeT (reason, TupleMap, funt, k))

    | ObjT (_, o), MapTypeT (reason_op, ObjectMap, funt, k) ->
      let map_t t = mk_tvar_where cx reason_op (fun t' ->
        let funtype = mk_functiontype [t] t' in
        rec_flow cx trace (funt, CallT (reason_op, funtype))
      ) in
      let props_tmap =
        Context.find_props cx o.props_tmap
        |> Properties.map_fields map_t
        |> Context.make_property_map cx
      in
      let dict_t = Option.map ~f:(fun dict ->
        let value = map_t dict.value in
        {dict with value}
      ) o.dict_t in
      let mapped_t =
        let reason = replace_reason_const RObjectType reason_op in
        ObjT (reason, {o with props_tmap; dict_t})
      in
      continue cx trace mapped_t k

    | ObjT (_, o), MapTypeT (reason_op, ObjectMapi, funt, k) ->
      let mapi_t key t = mk_tvar_where cx reason_op (fun t' ->
        let funtype = mk_functiontype [key; t] t' in
        rec_flow cx trace (funt, CallT (reason_op, funtype))
      ) in
      let mapi_field key t =
        let reason = replace_reason_const (RStringLit key) reason_op in
        mapi_t (SingletonStrT (reason, key)) t
      in
      let props_tmap =
        Context.find_props cx o.props_tmap
        |> Properties.mapi_fields mapi_field
        |> Context.make_property_map cx
      in
      let dict_t = Option.map ~f:(fun dict ->
        let value = mapi_t dict.key dict.value in
        {dict with value}
      ) o.dict_t in
      let mapped_t =
        let reason = replace_reason_const RObjectType reason_op in
        ObjT (reason, {o with props_tmap; dict_t})
      in
      continue cx trace mapped_t k

    (***********************************************)
    (* functions may have their prototypes written *)
    (***********************************************)

    | (FunT (_, _, t, _), SetPropT(reason_op, Named (_, "prototype"), tin)) ->
      rec_flow cx trace (tin, ObjAssignT(reason_op, t, AnyT.t, [], false))

    (*********************************)
    (* ... and their prototypes read *)
    (*********************************)

    | (FunT (_, _, t, _), GetPropT(_, Named (_, "prototype"), tout)) ->
      rec_flow_t cx trace (t,tout)

    | (ClassT (instance), GetPropT(_, Named (_, "prototype"), tout)) ->
      rec_flow_t cx trace (instance, tout)

    (**************************************)
    (* ... and their fields/elements read *)
    (**************************************)

    | (AnyFunT _, (
        GetPropT(reason_op, _, tout)
        | GetElemT(reason_op, _, tout)
      )) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | AnyFunT reason_fun, LookupT (reason_op, _, _, x, action) ->
      let p = Field (AnyT.why reason_op, Neutral) in
      perform_lookup_action cx trace x p reason_fun reason_op action

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
        let bound_reason =
          let desc = RBound (desc_of_reason reason) in
          replace_reason_const desc reason_op
        in

        rec_flow_t cx trace (
          FunT(
            reason_op,
            dummy_static bound_reason,
            dummy_prototype,
            mk_boundfunctiontype tins1 tout1
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
    (* TODO: This rule doesn't interact very well with union-type checking. It
       looks up Function.prototype, which currently doesn't appear structurally
       in the function type, and thus may not be fully resolved when the
       function type is checked with a union containing the object
       type. Ideally, we should either add Function.prototype to function types
       or fully resolve them when resolving function types, but either way we
       might bomb perf without additional work. Meanwhile, we need an immediate
       fix for the common case where this bug shows up. So leaving this comment
       here as a marker for future work, while going with a band-aid solution
       for now, as motivated below.

       Fortunately, it is quite hard for a function type to successfully
       check against an object type, and even more unlikely when the latter
       is part of a union: the object type must only contain
       Function.prototype methods or statics. Quickly confirming that the
       check would fail before looking up Function.prototype (while falling
       back to the general rule when we cannot guarantee failure) is a safe
       optimization in any case, and fixes the commonly observed case where
       the union type contains both a function type and a object type as
       members, clearly intending for function types to match the former
       instead of the latter. *)
    | (FunT (reason, statics, _, _) ,
       UseT (_, ObjT (reason_o, { props_tmap; _ }))) ->
        if not
          (quick_error_fun_as_obj cx trace reason statics reason_o
             (Context.find_props cx props_tmap))
        then
          let callp = Field (l, Positive) in
          let map = SMap.add "$call" callp SMap.empty in
          let function_proto = FunProtoT reason in
          let obj = mk_object_with_map_proto cx reason map function_proto in
          let t = mk_tvar_where cx reason (fun t ->
            rec_flow cx trace (statics, ObjAssignT (reason, obj, t, [], false))
          ) in
          rec_flow cx trace (t, u)

    (* TODO: similar concern as above *)
    | (FunT (reason, statics, _, _) ,
       UseT (_, InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = true;
         _;
       })))
      ->
        if not
          (quick_error_fun_as_obj cx trace reason statics reason_inst
            (SMap.filter (fun x _ -> x = "constructor")
              (Context.find_props cx fields_tmap)))
        then
          structural_subtype cx trace l reason_inst
            (super, fields_tmap, methods_tmap)

    (***************************************************************)
    (* Enable structural subtyping for upperbounds like interfaces *)
    (***************************************************************)

    | (_,
       UseT (_, InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = true;
         _;
       })))
      ->
      structural_subtype cx trace l reason_inst
        (super, fields_tmap, methods_tmap)

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
        Context.iter_props cx instance_super.fields_tmap (fun x p ->
          match p with
          | Field (AbstractT t, _)
            when not (Context.has_prop cx instance.fields_tmap x) ->
            (* when abstract fields are not implemented, make them void *)
            let reason = reason_of_t t in
            let desc_void = RMissingAbstract (desc_of_reason reason) in
            let reason_void = replace_reason_const desc_void reason in
            rec_unify cx trace (VoidT reason_void) t
          | _ -> ()
        );
        let strict = NonstrictReturning None in
        Context.iter_props cx instance.fields_tmap (fun x p ->
          let reason_prop = replace_reason_const (RProperty (Some x)) reason in
          lookup_prop cx trace l reason_prop reason strict x (SuperProp p)
        );
        Context.iter_props cx instance.methods_tmap (fun x p ->
          if inherited_method x then
            let reason_prop = replace_reason_const (RProperty (Some x)) reason in
            lookup_prop cx trace l reason_prop reason strict x (SuperProp p)
        )

    | ObjT _, SuperT (reason, instance)
    | AnyObjT _, SuperT (reason, instance)
      ->
        Context.iter_props cx instance.fields_tmap (fun x p ->
          let reason_prop = replace_reason_const (RProperty (Some x)) reason in
          let propref = Named (reason_prop, x) in
          rec_flow cx trace (l,
            LookupT (reason, NonstrictReturning None, [], propref,
              SuperProp p))
        );
        Context.iter_props cx instance.methods_tmap (fun x p ->
          let reason_prop = replace_reason_const (RProperty (Some x)) reason in
          let propref = Named (reason_prop, x) in
          if inherited_method x then
            rec_flow cx trace (l,
              LookupT (reason, NonstrictReturning None, [], propref,
                SuperProp p))
        )

    (***********************************************************)
    (* addition                                                *)
    (***********************************************************)

    | (l, AdderT (reason, r, u)) ->
      Ops.push reason;
      flow_addition cx trace reason l r u;
      Ops.pop ()

    (*********************************************************)
    (* arithmetic/bitwise/update operations besides addition *)
    (*********************************************************)

    | _, AssertArithmeticOperandT _ when numeric l -> ()
    | _, AssertArithmeticOperandT _ ->
      add_output cx trace (FlowError.EArithmeticOperand (reason_of_t l))

    (***********************************************************)
    (* coercion                                                *)
    (***********************************************************)

    (* string and number can be coerced to strings *)
    | StrT _, UseT (Coercion, StrT _)
    | NumT _, UseT (Coercion, StrT _) -> ()

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
        NumT (replace_reason_const RNumber reason_op, Literal (~-. value, raw))
      | AnyLiteral
      | Truthy ->
        l
      in
      rec_flow_t cx trace (num, t_out)

    (************************)
    (* binary `in` operator *)
    (************************)

    (* the left-hand side of a `(x in y)` expression is a string or number
       TODO: also, symbols *)
    | StrT _, AssertBinaryInLHST _ -> ()
    | NumT _, AssertBinaryInLHST _ -> ()
    | _, AssertBinaryInLHST _ ->
      add_output cx trace (FlowError.EBinaryInLHS (reason_of_t l))

    (* the right-hand side of a `(x in y)` expression must be object-like *)
    | ArrT _, AssertBinaryInRHST _ -> ()
    | _, AssertBinaryInRHST _ when object_like l -> ()
    | _, AssertBinaryInRHST _ ->
      add_output cx trace (FlowError.EBinaryInRHS (reason_of_t l))

    (******************)
    (* `for...in` RHS *)
    (******************)

    (* objects are allowed. arrays _could_ be, but are not because it's
       generally safer to use a for or for...of loop instead. *)
    | _, AssertForInRHST _ when object_like l -> ()
    | (AnyObjT _ | ObjProtoT _), AssertForInRHST _ -> ()

    (* null/undefined are allowed *)
    | (NullT _ | VoidT _), AssertForInRHST _ -> ()

    | _, AssertForInRHST _ ->
      add_output cx trace (FlowError.EForInRHS (reason_of_t l))

    (**************************************)
    (* types may be refined by predicates *)
    (**************************************)

    | _, PredicateT(p,t) ->
      predicate cx trace t l p

    | _, GuardT (pred, result, sink) ->
      guard cx trace l pred result sink

    | StrT (_, Literal value),
      SentinelPropTestT (_, sense, SentinelStr sentinel, _)
      when (value = sentinel) != sense ->
        (* provably unreachable, so prune *)
        ()

    | NumT (_, Literal (value, _)),
      SentinelPropTestT (_, sense, SentinelNum (sentinel, _), _)
      when (value = sentinel) != sense ->
        (* provably unreachable, so prune *)
        ()

    | BoolT (_, Some value),
      SentinelPropTestT (_, sense, SentinelBool sentinel, _)
      when (value = sentinel) != sense ->
        (* provably unreachable, so prune *)
        ()

    | _, SentinelPropTestT (l, _, _, result) ->
        (* property exists, but is not something we can use for refinement *)
        rec_flow_t cx trace (l, result)

    (**********************)
    (* Array library call *)
    (**********************)

    | (ArrT (reason, t, _), (GetPropT _ | SetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_typeapp cx ~trace reason "Array" [t], u)

    (***********************)
    (* String library call *)
    (***********************)

    | (StrT (reason, _), (GetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx ~trace reason "String",u)

    (***********************)
    (* Number library call *)
    (***********************)

    | (NumT (reason, _), (GetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx ~trace reason "Number",u)

    (***********************)
    (* Boolean library call *)
    (***********************)

    | (BoolT (reason, _), (GetPropT _ | MethodT _ | LookupT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx ~trace reason "Boolean",u)

    (*************************)
    (* Function library call *)
    (*************************)

    | (FunProtoT reason, (GetPropT _ | SetPropT _ | MethodT _ | HasPropT _)) ->
      rec_flow cx trace (get_builtin_type cx ~trace reason "Function",u)

    (*********************)
    (* functions statics *)
    (*********************)

    | (FunT (reason, static, _, _), _) when object_like_op u ->
      rec_flow cx trace (static, ReposLowerT (reason, u))

    (*****************)
    (* class statics *)
    (*****************)

    | (ClassT instance, _) when object_use u || object_like_op u ->
      let reason = replace_reason (fun desc ->
        RStatics desc
      ) (reason_of_t instance) in
      let static = mk_tvar cx reason in
      rec_flow cx trace (instance, GetStaticsT (reason, static));
      rec_flow cx trace (static, ReposLowerT (reason, u))

    (**********************************************)
    (* classes as functions, functions as classes *)
    (**********************************************)

    (* When a class value flows to a function annotation or call site, check for
       the presence of a $call property in the former (as a static) compatible
       with the latter. *)
    | (ClassT _, (UseT (_, FunT (reason, _, _, _)) | CallT (reason, _))) ->
      let propref = Named (reason, "$call") in
      rec_flow cx trace (l,
        GetPropT (reason, propref, tvar_with_constraint ~trace cx u))

    (* For a function type to be used as a class type, the following must hold:
       - the class's instance type must be a subtype of the function's prototype
       property type and 'this' type
       - the function's statics should be included in the class's statics
       (typically a function's statics are under-specified, so we don't
       enforce equality)
       - the class's static $call property type must be a subtype of the
       function type. *)
    | FunT (reason, static, prototype, funtype),
      UseT (use_op, ClassT instance) ->
      rec_flow cx trace (instance, UseT (use_op, prototype));
      rec_flow cx trace (instance, UseT (use_op, funtype.this_t));
      rec_flow cx trace (instance, GetStaticsT (reason, static));
      rec_flow cx trace (ClassT instance,
        GetPropT (reason, Named (reason, "$call"), l))

    (************)
    (* indexing *)
    (************)

    | (InstanceT _, GetElemT (reason, i, t)) ->
      rec_flow cx trace (l, SetPropT (reason, Named (reason, "$key"), i));
      rec_flow cx trace (l, GetPropT (reason, Named (reason, "$value"), t))

    | (InstanceT _, SetElemT (reason, i, t)) ->
      rec_flow cx trace (l, SetPropT (reason, Named (reason, "$key"), i));
      rec_flow cx trace (l, SetPropT (reason, Named (reason, "$value"), t))

    (*************************)
    (* repositioning, part 2 *)
    (*************************)

    (* waits for a lower bound to become concrete, and then repositions it to
       the location stored in the ReposLowerT, which is usually the location
       where that lower bound was used; the lower bound's location (which is
       being overwritten) is where it was defined. *)
    | (_, ReposLowerT (reason_op, u)) ->
      rec_flow cx trace (reposition cx ~trace reason_op l, u)

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

    | (ObjProtoT _,
       LookupT (reason, strict, next::try_ts_on_failure, propref, t)) ->
      (* When s is not found, we always try to look it up in the next element in
         the list try_ts_on_failure. *)
      rec_flow cx trace
        (next, LookupT (reason, strict, try_ts_on_failure, propref, t))

    | ObjProtoT _, LookupT (reason_op, _, [], Named (_, x), _)
      when is_object_prototype_method x ->
      (** TODO: These properties should go in Object.prototype. Currently we
          model Object.prototype as a ObjProtoT, as an optimization against a
          possible deluge of shadow properties on Object.prototype, since it
          is shared by every object. **)
      rec_flow cx trace (get_builtin_type cx ~trace reason_op "Object", u)

    | FunProtoT _, LookupT (reason_op, _, [], Named (_, x), _)
      when is_function_prototype x ->
      (** TODO: Ditto above comment for Function.prototype *)
      rec_flow cx trace (get_builtin_type cx ~trace reason_op "Function", u)

    | (ObjProtoT reason | FunProtoT reason),
      LookupT (_, Strict strict_reason, [], Named (reason_prop, x), _) ->
      add_output cx trace (FlowError.EStrictLookupFailed
        ((reason_prop, strict_reason), reason, Some x))

    | (ObjProtoT reason | FunProtoT reason), LookupT (reason_op,
        Strict strict_reason, [], (Computed elem_t as propref), action) ->
      (match elem_t with
      | OpenT _ ->
        let loc = loc_of_t elem_t in
        add_output cx trace FlowError.(EInternal (loc, PropRefComputedOpen))
      | StrT (_, Literal _) ->
        let loc = loc_of_t elem_t in
        add_output cx trace FlowError.(EInternal (loc, PropRefComputedLiteral))
      | AnyT _ | StrT _ | NumT _ ->
        (* any, string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
        let p = Field (AnyT.why reason_op, Neutral) in
        perform_lookup_action cx trace propref p reason reason_op action
      | _ ->
        let reason_prop = reason_of_t elem_t in
        add_output cx trace (FlowError.EStrictLookupFailed
          ((reason_prop, strict_reason), reason, None)))

    | (ObjProtoT reason | FunProtoT reason), LookupT (reason_op,
        ShadowRead (strict, rev_proto_ids), [], (Named (reason_prop, x) as propref), action) ->
      (* Emit error if this is a strict read. See `lookup_kinds` in types.ml. *)
      (match strict with
      | None -> ()
      | Some strict_reason ->
        add_output cx trace (FlowError.EStrictLookupFailed
          ((reason_prop, strict_reason), reason, Some x)));

      (* Install shadow prop (if necessary) and link up proto chain. *)
      let p = find_or_intro_shadow_prop cx trace x (Nel.rev rev_proto_ids) in
      perform_lookup_action cx trace propref p reason reason_op action

    | (ObjProtoT reason | FunProtoT reason), LookupT (reason_op,
        ShadowWrite rev_proto_ids, [], (Named (_, x) as propref), action) ->
      let id, proto_ids = Nel.rev rev_proto_ids in
      let pmap = Context.find_props cx id in
      (* Re-check written-to unsealed object to see if prop was added since we
       * last looked. See comment above `find` in `find_or_intro_shadow_prop`.
       *)
      let p = match SMap.get x pmap with
      | Some p -> p
      | None ->
        match SMap.get (internal_name x) pmap with
        | Some p ->
          (* unshadow *)
          pmap
            |> SMap.remove (internal_name x)
            |> SMap.add x p
            |> Context.add_property_map cx id;
          p
        | None ->
          (* Create prop and link shadow props along the proto chain. *)
          let reason_prop = locationless_reason (RShadowProperty x) in
          let t = mk_tvar cx reason_prop in
          (match proto_ids with
          | [] -> ()
          | id::ids ->
            let p_proto = find_or_intro_shadow_prop cx trace x (id, ids) in
            let t_proto = Property.assert_field p_proto in
            rec_flow cx trace (t_proto, UnifyT (t_proto, t)));
          (* Add prop *)
          let p = Field (t, Neutral) in
          pmap
            |> SMap.add x p
            |> Context.add_property_map cx id;
          p
      in
      perform_lookup_action cx trace propref p reason reason_op action

    | (ObjProtoT _ | FunProtoT _),
      LookupT (_, ShadowRead _, [], Computed elem_t, _) ->
      let loc = loc_of_t elem_t in
      add_output cx trace FlowError.(EInternal (loc, ShadowReadComputed))

    | (ObjProtoT _ | FunProtoT _),
      LookupT (_, ShadowWrite _, [], Computed elem_t, _) ->
      let loc = loc_of_t elem_t in
      add_output cx trace FlowError.(EInternal (loc, ShadowWriteComputed))

    (* LookupT is a non-strict lookup *)
    | (ObjProtoT _ |
       FunProtoT _ |
       MixedT (_, Mixed_truthy) |
       MixedT (_, Mixed_non_maybe)),
      LookupT (_, NonstrictReturning t_opt, [], _, _) ->
      (* don't fire

         ...unless a default return value is given. Two examples:

         1. A failure could arise when an unchecked module was looked up and
         not found declared, in which case we consider that module's exports to
         be `any`.

         2. A failure could arise also when an object property is looked up in
         a condition, in which case we consider the object's property to be
         `mixed`.
      *)
      begin match t_opt with
      | Some (not_found, t) -> rec_unify cx trace t not_found
      | None -> ()
      end

    | (ObjProtoT _, HasPropT (reason_op, Some reason_strict, _)) ->
        add_output cx trace (FlowError.EPropNotFound (reason_op, reason_strict))

    (* SuperT only involves non-strict lookups *)
    | (ObjProtoT _, SuperT _) -> ()

    (** ExtendsT searches for a nominal superclass. The search terminates with
        either failure at the root or a structural subtype check. **)

    | (ObjProtoT _, UseT (use_op, ExtendsT (next::try_ts_on_failure, l, u))) ->
      (* When seaching for a nominal superclass fails, we always try to look it
         up in the next element in the list try_ts_on_failure. *)
      rec_flow cx trace
        (next, UseT (use_op, ExtendsT (try_ts_on_failure, l, u)))

    | ObjProtoT _,
      UseT (_, ExtendsT ([], l, InstanceT (reason_inst, _, super, {
         fields_tmap;
         methods_tmap;
         structural = true;
         _;
       })))
      ->
      structural_subtype cx trace l reason_inst
        (super, fields_tmap, methods_tmap)

    | (ObjProtoT _, UseT (_, ExtendsT ([], t, tc))) ->
      let msg = "This type is incompatible with" in
      let reasons = Flow_error.ordered_reasons t (UseT (UnknownUse, tc)) in
      add_output cx trace (FlowError.ECustom (reasons, msg))

    (* Special cases of FunT *)
    | FunProtoApplyT reason, _
    | FunProtoBindT reason, _
    | FunProtoCallT reason, _ ->
      rec_flow cx trace (FunProtoT reason, u)

    | (_, GetPropT (_, propref, _))
    | (_, SetPropT (_, propref, _))
    | (_, LookupT (_, _, _, propref, _)) ->
      let reason_prop = reason_of_propref propref in
      add_output cx trace (FlowError.EIncompatibleProp (l, u, reason_prop))

    | _, UseT (Addition, _) ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.EAddition reasons)

    | _, UseT (Coercion, _) ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.ECoercion reasons)

    | _, UseT (FunCallParam, u) ->
      add_output cx trace
        (FlowError.EFunCallParam (reason_of_t l, reason_of_t u))

    | _, UseT (FunCallThis reason_call, u) ->
      add_output cx trace
        (FlowError.EFunCallThis (reason_of_t l, reason_of_t u, reason_call))

    | _, UseT (FunImplicitReturn, u) ->
      add_output cx trace
        (FlowError.EFunImplicitReturn (reason_of_t l, reason_of_t u))

    | _, UseT (FunReturn, _) ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.EFunReturn reasons)

    | _, UseT (Internal _, _) ->
      ()

    | _, UseT (MissingTupleElement i, _) ->
      let reasons = FlowError.ordered_reasons l u in
      add_output cx trace (FlowError.EMissingTupleElement (reasons, i))

    | _ ->
      add_output cx trace (FlowError.EIncompatible (l, u))
  )

(* some types need to be resolved before proceeding further *)
and needs_resolution = function
  | OpenT _ | UnionT _ | OptionalT _ | MaybeT _ | AnnotT _ -> true
  | _ -> false

(**
 * Addition
 *
 * According to the spec, given l + r:
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
 * However, if l or r is a number and the other side is invalid, then we assume
 * you were going for a number; generate an error on the invalid side; and flow
 * `number` out as the result of the addition, even though at runtime it will be
 * a string. Fixing the error will make the result type correct. The alternative
 * is that we would error on both l and r, saying neither is compatible with
 * `string`.
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

  (* unreachable additions are unreachable *)
  | EmptyT _, _
  | _, EmptyT _ ->
    rec_flow_t cx trace (EmptyT.why reason, u)

  | (MixedT _, _) ->
    let reasons = FlowError.ordered_reasons l (UseT (UnknownUse, r)) in
    add_output cx trace (FlowError.EAddition reasons)
  | (_, MixedT _) ->
    let reasons = FlowError.ordered_reasons r (UseT (UnknownUse, l)) in
    add_output cx trace (FlowError.EAddition reasons)

  | (NumT _ | BoolT _ | NullT _ | VoidT _),
    (NumT _ | BoolT _ | NullT _ | VoidT _) ->
    rec_flow_t cx trace (NumT.why reason, u)

  | StrT _, _ ->
    rec_flow cx trace (r, UseT (Addition, l));
    rec_flow cx trace (StrT.why reason, UseT (UnknownUse, u));

  | _, StrT _ ->
    rec_flow cx trace (l, UseT (Addition, r));
    rec_flow cx trace (StrT.why reason, UseT (UnknownUse, u));

  | NumT _, _ ->
    rec_flow cx trace (r, UseT (Addition, l));
    rec_flow cx trace (NumT.why reason, UseT (UnknownUse, u));

  | _, NumT _ ->
    rec_flow cx trace (l, UseT (Addition, r));
    rec_flow cx trace (NumT.why reason, UseT (UnknownUse, u));

  | (_, _) ->
    let fake_str = StrT.why reason in
    rec_flow cx trace (l, UseT (Addition, fake_str));
    rec_flow cx trace (r, UseT (Addition, fake_str));
    rec_flow cx trace (fake_str, UseT (Addition, u));

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
  | (_, _) ->
    let reasons = FlowError.ordered_reasons l (UseT (UnknownUse, r)) in
    add_output cx trace (FlowError.EComparison reasons)

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
  | (_, _) ->
    let reasons = FlowError.ordered_reasons l (UseT (UnknownUse, r)) in
    add_output cx trace (FlowError.EComparison reasons)

and is_object_prototype_method = function
  | "isPrototypeOf"
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
  | x -> is_object_prototype_method x

(* neither object prototype methods nor callable signatures should be
 * implied by an object indexer type *)
and is_dictionary_exempt = function
  | x when is_object_prototype_method x -> true
  | "$call" -> true
  | _ -> false

(* common case checking a function as an object *)
and quick_error_fun_as_obj cx trace reason statics reason_o props =
  let statics_own_props = match statics with
    | ObjT (_, { props_tmap; _ }) -> Some (Context.find_props cx props_tmap)
    | AnyFunT _
    | MixedT _ -> Some SMap.empty
    | _ -> None
  in
  match statics_own_props with
  | Some statics_own_props ->
    let props_not_found = SMap.filter (fun x p ->
      let optional = match p with
      | Field (OptionalT _, _) -> true
      |_ -> false
      in
      not (
        optional ||
        x = "$call" ||
        is_function_prototype x ||
        SMap.mem x statics_own_props
      )
    ) props in
    SMap.iter (fun x _ ->
      let reason_prop =
        replace_reason (fun desc -> RPropertyOf (x, desc)) reason_o in
      add_output cx trace (FlowError.EPropNotFound (reason_prop, reason))
    ) props_not_found;
    not (SMap.is_empty props_not_found)
  | None -> false

and ground_subtype = function
  (* tvars are not considered ground, so they're not part of this relation *)
  | (OpenT _, _) | (_, UseT (_, OpenT _)) -> false

  (* Allow any lower bound to be repositioned *)
  | (_, ReposLowerT _) -> false
  | (_, ReposUseT _) -> false

  (* Allow deferred unification with `any` *)
  | (_, UnifyT _) -> false

  (* Allow any propagation to dictionaries *)
  | (AnyT _, ElemT _) -> false

  (* Prevents Tainted<any> -> any *)
  (* NOTE: the union could be narrowed down to ensure it contains taint *)
  | (UnionT _, _) | (TaintT _, _) -> false

  | (NumT _, UseT (_, NumT _))
  | (StrT _, UseT (_, StrT _))
  | (BoolT _, UseT (_, BoolT _))
  | (NullT _, UseT (_, NullT _))
  | (VoidT _, UseT (_, VoidT _))
  | (EmptyT _, _)
  | (_, UseT (_, MixedT _))
  | (_, UseT (_, ObjProtoT _))
  | (_, UseT (_, FunProtoT _))
    -> true

  | (AnyT _, u) -> not (any_propagating_use_t u)
  | (_, UseT (_, AnyT _)) -> true

  | _ ->
    false

and numeric = function
  | NumT _ -> true
  | SingletonNumT _ -> true

  | InstanceT (reason, _, _, _) ->
    string_of_desc (desc_of_reason reason) = "Date"

  | _ -> false

and object_like = function
  | AnyObjT _ | ObjT _ | InstanceT _ -> true
  | t -> function_like t

and object_use = function
  | UseT (_, ObjT _) -> true
  | _ -> false

and object_like_op = function
  | SetPropT _ | GetPropT _ | MethodT _ | LookupT _
  | SuperT _
  | GetKeysT _ | HasOwnPropT _ | HasPropT _
  | ObjAssignT _ | ObjRestT _
  | SetElemT _ | GetElemT _
  | UseT (_, AnyObjT _) -> true
  | _ -> false

(* TODO: why is AnyFunT missing? *)
and function_like = function
  | ClassT _
  | CustomFunT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | FunT _ -> true
  | _ -> false

and function_like_op = function
  | CallT _ | UseT (_, TypeT _)
  | ConstructorT _
  | UseT (_, AnyFunT _) -> true
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

(** Harness for testing parameterized types. Given a test function and a list
    of type params, generate a bunch of argument maps and invoke the test
    function on each, using Reason.TestID to keep the reasons generated by
    each test disjoint from the others.

    In the general case we simply test every combination of p = bot, p = bound
    for each param p. For many parameter lists this will be more than strictly
    necessary, but determining the minimal set of tests for interrelated params
    is subtle. For now, our only refinement is to isolate all params with an
    upper bound of MixedT (making them trivially unrelated to each other) and
    generate a smaller set of argument maps for these which only cover a) bot,
    bound for each param, and b) every pairwise bot/bound combination. These
    maps are then used as seeds for powersets over the remaining params.

    NOTE: Since the same AST is traversed by each generated test, the order
    of generated tests is important for the proper functioning of hooks that
    record information on the side as ASTs are traversed. Adopting the
    convention that the last traversal "wins" (which would happen, e.g, when
    the recorded information at a location is replaced every time that
    location is encountered), we want the last generated test to always be
    the one where all type parameters are substituted by their bounds
    (instead of Bottom), so that the recorded information is the same as if
    all type parameters were indeed erased and replaced by their bounds.
  *)
and generate_tests =
  (* make bot type for given param *)
  let mk_bot reason _ { name; _ } =
    let desc = RIncompatibleInstantiation name in
    EmptyT (replace_reason_const desc reason)
  in
  (* make bound type for given param and argument map *)
  let mk_bound cx prev_args { bound; _ } =
    subst cx prev_args bound
  in
  (* make argument map by folding mk_arg over param list *)
  let mk_argmap mk_arg =
    List.fold_left (fun acc ({ name; _ } as p) ->
      SMap.add name (mk_arg acc p) acc
    ) SMap.empty
  in
  (* for each p, a map with p bot and others bound + map with all bound *)
  let linear cx r = function
  | [] -> [SMap.empty]
  | params ->
    let all = mk_argmap (mk_bound cx) params in
    let each = List.map (fun ({ name; _ } as p) ->
      SMap.add name (mk_bot r SMap.empty p) all
    ) params in
    List.rev (all :: each)
  in
  (* a map for every combo of bot/bound params *)
  let powerset cx r params arg_map =
    let none = mk_argmap (mk_bot r) params in
    List.fold_left (fun maps ({ name; _ } as p) ->
      let bots = List.map (SMap.add name (SMap.find_unsafe name none)) maps in
      let bounds = List.map (fun m -> SMap.add name (mk_bound cx m p) m) maps in
      bots @ bounds
    ) [arg_map] params
  in
  (* main - run f over a collection of arg maps generated for params *)
  fun cx reason params f ->
    if params = [] then f SMap.empty else
    let is_free = function { bound = MixedT _; _ } -> true | _ -> false in
    let free_params, dep_params = List.partition is_free params in
    let free_sets = linear cx reason free_params in
    let powersets = List.map (powerset cx reason dep_params) free_sets in
    List.iter (TestID.run f) (List.flatten powersets)

(*********************)
(* inheritance utils *)
(*********************)

and mk_nominal cx =
  let nominal = mk_id () in
  (if Context.is_verbose cx then prerr_endlinef
      "NOM %d %s" nominal (Debug_js.string_of_file cx));
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
    (match SMap.get x pmap with
    | Some Negative -> rec_flow_t cx trace (t2, t1)
    | Some Positive -> rec_flow_t cx trace (t1, t2)
    | Some Neutral
    | None -> rec_unify cx trace t1 t2)
  )

and inherited_method x = x <> "constructor" && x <> "$call"

and sealed_in_op reason_op = function
  | Sealed -> true
  | UnsealedInFile source -> source <> (Loc.source (loc_of_reason reason_op))

(* dispatch checks to verify that lower satisfies the structural
   requirements given in the tuple. *)
and structural_subtype cx trace lower reason_struct
  (super, fields_pmap, methods_pmap) =
  let lreason = reason_of_t lower in
  let fields_pmap = Context.find_props cx fields_pmap in
  let methods_pmap = Context.find_props cx methods_pmap in
  fields_pmap |> SMap.iter (fun s p ->
    match p with
    | Field (OptionalT t, polarity) ->
      let reason_prop = replace_reason (fun desc ->
        ROptional (RPropertyOf (s, desc))
      ) reason_struct in
      let propref = Named (reason_prop, s) in
      rec_flow cx trace (lower,
        LookupT (reason_prop, NonstrictReturning None, [], propref,
          LookupProp (Field (t, polarity))))
    | _ ->
      let reason_prop = replace_reason (fun desc ->
        RPropertyOf (s, desc)
      ) reason_struct in
      let propref = Named (reason_prop, s) in
      rec_flow cx trace (lower,
        LookupT (reason_prop, Strict lreason, [], propref, LookupProp p))
  );
  methods_pmap |> SMap.iter (fun s p ->
    if inherited_method s then
      let reason_prop = replace_reason (fun desc ->
        RPropertyOf (s, desc)
      ) reason_struct in
      let propref = Named (reason_prop, s) in
      rec_flow cx trace (lower,
        LookupT (reason_prop, Strict lreason, [], propref, LookupProp p))
  );
  rec_flow_t cx trace (lower, super)


(*****************)
(* substitutions *)
(*****************)

and ident_map: 'a. ('a -> 'a) -> 'a list -> 'a list = fun f lst ->
  let rev_lst, changed = List.fold_left (fun (lst_, changed) item ->
    let item_ = f item in
    item_::lst_, changed || item_ != item
  ) ([], false) lst in
  if changed then List.rev rev_lst else lst

and ident_smap: 'a. ('a -> 'a) -> 'a SMap.t -> 'a SMap.t = fun f map ->
  let map_, changed = SMap.fold (fun key item (map_, changed) ->
    let item_ = f item in
    SMap.add key item_ map_, changed || item_ != item
  ) map (SMap.empty, false) in
  if changed then map_ else map

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
    | Some param_t ->
      (* opportunistically reposition This substitutions; in general
         repositioning may lead to non-termination *)
      if typeparam.name = "this" then reposition cx typeparam.reason param_t
      else param_t
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
  | ObjProtoT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | ChoiceKitT _
  | CustomFunT _
    ->
    t

  | IdxWrapper (reason, obj_t) ->
    let obj_t' = subst cx ~force map obj_t in
    if obj_t == obj_t' then t else IdxWrapper (reason, obj_t')

  | FunT (reason, static, proto, {
    this_t = this;
    params_tlist = params;
    params_names;
    return_t;
    is_predicate;
    closure_t;
    changeset
  }) ->
    let static_ = subst cx ~force map static in
    let proto_ = subst cx ~force map proto in
    let this_ = subst cx ~force map this in
    let params_ = ident_map (subst cx ~force map) params in
    let return_t_ = subst cx ~force map return_t in
    if static_ == static &&
       proto_ == proto &&
       this_ == this &&
       params_ == params &&
       return_t_ == return_t
    then t
    else
      FunT (reason, static_, proto_, {
        this_t = this_;
        params_tlist = params_;
        params_names;
        return_t = return_t_;
        is_predicate;
        closure_t;
        changeset
      })

  | PolyT (xs, inner) ->
    let xs, map, changed = List.fold_left (fun (xs, map, changed) typeparam ->
      let bound = subst cx ~force map typeparam.bound in
      let default = match typeparam.default with
      | None -> None
      | Some default ->
        let default_ = subst cx ~force map default in
        if default_ == default then typeparam.default else Some default_
      in
      { typeparam with bound; default; }::xs,
      SMap.remove typeparam.name map,
      changed || bound != typeparam.bound || default != typeparam.default
    ) ([], map, false) xs in
    let inner_ = subst cx ~force:false map inner in
    let changed = changed || inner_ != inner in
    if changed then PolyT (List.rev xs, inner_) else t

  | ThisClassT this ->
    let map = SMap.remove "this" map in
    let this_ = subst cx ~force map this in
    if this_ == this then t else ThisClassT this_

  | ObjT (reason, { flags; dict_t; props_tmap; proto_t; }) ->
    let dict_t_ = match dict_t with
    | None -> None
    | Some dict ->
        let key_ = subst cx ~force map dict.key in
        let value_ = subst cx ~force map dict.value in
        if key_ == dict.key && value_ == dict.value then dict_t
        else Some { dict with key = key_; value = value_; }
    in
    let props_tmap_ = subst_propmap cx force map props_tmap in
    let proto_t_ = subst cx ~force map proto_t in
    if dict_t_ == dict_t &&
       props_tmap_ == props_tmap &&
       proto_t_ == proto_t
    then t
    else
      ObjT (reason, {
        flags;
        dict_t = dict_t_;
        props_tmap = props_tmap_;
        proto_t = proto_t_;
      })

  | ArrT (reason, arr_t, ts) ->
    let arr_t_ = subst cx ~force map arr_t in
    let ts_ = ident_map (subst cx ~force map) ts in
    if arr_t_ == arr_t && ts_ == ts then t
    else ArrT (reason, arr_t_, ts_)

  | ClassT cls ->
    let cls_ = subst cx ~force map cls in
    if cls_ == cls then t else ClassT cls_

  | TypeT (reason, type_t) ->
    let type_t_ = subst cx ~force map type_t in
    if type_t_ == type_t then t else TypeT (reason, type_t_)

  | AnnotT (sink_t, source_t) ->
    let sink_t_ = subst cx ~force map sink_t in
    let source_t_ = subst cx ~force map source_t in
    if sink_t_ == sink_t && source_t_ == source_t then t
    else AnnotT (sink_t_, source_t_)

  | InstanceT (reason, static, super, instance) ->
    let static_ = subst cx ~force map static in
    let super_ = subst cx ~force map super in
    let type_args_ = ident_smap (subst cx ~force map) instance.type_args in
    let fields_tmap_ = subst_propmap cx force map instance.fields_tmap in
    let methods_tmap_ = subst_propmap cx force map instance.methods_tmap in
    if static_ == static &&
       super_ == super &&
       type_args_ == instance.type_args &&
       fields_tmap_ == instance.fields_tmap &&
       methods_tmap_ == instance.methods_tmap
    then t
    else
      InstanceT (
        reason,
        static_,
        super_,
        { instance with
          type_args = type_args_;
          fields_tmap = fields_tmap_;
          methods_tmap = methods_tmap_;
        }
    )

  | OptionalT opt_t ->
    let opt_t_ = subst cx ~force map opt_t in
    if opt_t_ == opt_t then t else OptionalT opt_t_

  | RestT rest_t ->
    let rest_t_ = subst cx ~force map rest_t in
    if rest_t_ == rest_t then t else RestT rest_t_

  | AbstractT abstract_t ->
    let abstract_t_ = subst cx ~force map abstract_t in
    if abstract_t_ == abstract_t then t else AbstractT abstract_t_

  | ExactT (reason, exact_t) ->
    let exact_t_ = subst cx ~force map exact_t in
    if exact_t_ == exact_t then t else ExactT (reason, exact_t_)

  | EvalT (eval_t, defer_use_t, _) ->
    let eval_t_ = subst cx ~force map eval_t in
    let defer_use_t_ = subst_defer_use_t cx ~force map defer_use_t in
    if eval_t_ == eval_t && defer_use_t_ == defer_use_t then t
    else EvalT (eval_t_, defer_use_t_, mk_id ())

  | TypeAppT(c, ts) ->
    let c_ = subst cx ~force map c in
    let ts_ = ident_map (subst cx ~force map) ts in
    if c_ == c && ts_ == ts then t else TypeAppT (c_, ts_)

  | ThisTypeAppT(c, this, ts) ->
    let c_ = subst cx ~force map c in
    let this_ = subst cx ~force map this in
    let ts_ = ident_map (subst cx ~force map) ts in
    if c_ == c && this_ == this && ts_ == ts then t
    else ThisTypeAppT (c_, this_, ts_)

  | MaybeT maybe_t ->
    let maybe_t_ = subst cx ~force map maybe_t in
    if maybe_t_ == maybe_t then t else MaybeT maybe_t_

  | IntersectionT (reason, rep) ->
    let rep_ = InterRep.ident_map (subst cx ~force map) rep in
    if rep_ == rep then t else IntersectionT (reason, rep_)

  | UnionT (reason, rep) ->
    let rep_ = UnionRep.ident_map (subst cx ~force map) rep in
    if rep_ == rep then t else UnionT (reason, rep_)

  | AnyWithLowerBoundT any_t ->
    let any_t_ = subst cx ~force map any_t in
    if any_t_ == any_t then t else AnyWithLowerBoundT any_t_

  | AnyWithUpperBoundT any_t ->
    let any_t_ = subst cx ~force map any_t in
    if any_t_ == any_t then t else AnyWithUpperBoundT any_t_

  | AnyObjT _ -> t
  | AnyFunT _ -> t

  | ShapeT shape_t ->
    let shape_t_ = subst cx ~force map shape_t in
    if shape_t_ == shape_t then t else ShapeT shape_t_

  | DiffT(t1, t2) ->
    let t1_ = subst cx ~force map t1 in
    let t2_ = subst cx ~force map t2 in
    if t1_ == t1 && t2_ == t2 then t else DiffT (t1_, t2_)

  | KeysT (reason, keys_t) ->
    let keys_t_ = subst cx ~force map keys_t in
    if keys_t_ == keys_t then t else KeysT (reason, keys_t_)

  | SingletonNumT _
  | SingletonBoolT _
  | SingletonStrT _ -> t

  | ModuleT _
  | ExtendsT _
    ->
      failwith (spf "Unhandled type ctor: %s" (string_of_ctor t)) (* TODO *)

  | OpenPredT (r, arg, pos_map, neg_map) ->
    let arg' = subst cx ~force map arg in
    if arg == arg' then t else OpenPredT (r, arg', pos_map, neg_map)

  | TypeMapT (r, kind, t1, t2) ->
    let t1' = subst cx ~force map t1 in
    let t2' = subst cx ~force map t2 in
    if t1 == t1' && t2 == t2' then t else TypeMapT (r, kind, t1', t2')

and subst_defer_use_t cx ~force map t = match t with
  | DestructuringT (reason, s) ->
      let s_ = subst_selector cx force map s in
      if s_ == s then t else DestructuringT (reason, s_)
  | TypeDestructorT (reason, s) ->
      let s_ = subst_destructor cx force map s in
      if s_ == s then t else TypeDestructorT (reason, s_)

and eval_selector cx ?trace reason curr_t s i =
  let evaluated = Context.evaluated cx in
  match IMap.get i evaluated with
  | None ->
    mk_tvar_where cx reason (fun tvar ->
      Context.set_evaluated cx (IMap.add i tvar evaluated);
      flow_opt cx ?trace (curr_t, match s with
      | Prop x -> GetPropT(reason, Named (reason, x), tvar)
      | Elem key -> GetElemT(reason, key, tvar)
      | ObjRest xs -> ObjRestT(reason, xs, tvar)
      | ArrRest i -> ArrRestT(reason, i, tvar)
      | Default -> PredicateT (NotP VoidP, tvar)
      | Become -> BecomeT (reason, tvar)
      | Refine p -> RefineT (reason, p, tvar)
      )
    )
  | Some it ->
    it

and eval_destructor cx ~trace reason curr_t s i =
  let evaluated = Context.evaluated cx in
  match IMap.get i evaluated with
  | None ->
    mk_tvar_where cx reason (fun tvar ->
      Context.set_evaluated cx (IMap.add i tvar evaluated);
      match curr_t with
      (* If we are destructuring a union, evaluating the destructor on the union
         itself may have the effect of splitting the union into separate lower
         bounds, which prevents the speculative match process from working.
         Instead, we preserve the union by pushing down the destructor onto the
         branches of the unions. *)
      | UnionT (r, rep) ->
        rec_flow_t cx trace (UnionT (r, rep |> UnionRep.map (fun t ->
          EvalT (t, TypeDestructorT (reason, s), mk_id ())
        )), tvar)
      | _ ->
        rec_flow cx trace (curr_t, match s with
        | NonMaybeType -> UseT (UnknownUse, MaybeT (tvar))
        | PropertyType x -> GetPropT(reason, Named (reason, x), tvar)
        )
    )
  | Some it ->
    it

and subst_propmap cx force map id =
  let pmap = Context.find_props cx id in
  let pmap_ = ident_smap (Property.ident_map_t (subst cx ~force map)) pmap in
  if pmap_ == pmap then id
  else Context.make_property_map cx pmap_

and subst_selector cx force map s = match s with
  | Elem key ->
    let key_ = subst cx ~force map key in
    if key_ == key then s else Elem key_
  | Prop _
  | ObjRest _
  | ArrRest _
  | Default
  | Become -> s
  | Refine p ->
    let p' = subst_predicate cx ~force map p in
    if p == p' then s else Refine p'

and subst_destructor _cx _force _map s = match s with
  | NonMaybeType
  | PropertyType _
    -> s

and subst_predicate cx ?(force=true) (map: Type.t SMap.t) p = match p with
  | LatentP (t, i) ->
    let t' = subst cx ~force map t in
    if t == t' then p else LatentP (t', i)
  | p -> p

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
  | ExactT (_, t)
  | MaybeT t
  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t
    -> check_polarity cx polarity t

  | ClassT t
    -> check_polarity cx Neutral t

  | TypeT (_, t)
    -> check_polarity cx Neutral t

  | InstanceT (_, _, _, instance) ->
    check_polarity_propmap cx instance.fields_tmap;
    check_polarity_propmap cx instance.methods_tmap

  | FunT (_, _, _, func) ->
    List.iter (check_polarity cx (Polarity.inv polarity)) func.params_tlist;
    check_polarity cx polarity func.return_t

  | ArrT (_, t, _) ->
    check_polarity cx Neutral t

  | ObjT (_, obj) ->
    check_polarity_propmap cx obj.props_tmap;
    (match obj.dict_t with
    | Some { key; value; dict_polarity; _ } ->
      check_polarity cx dict_polarity key;
      check_polarity cx dict_polarity value
    | None -> ())

  | IdxWrapper (_, obj) -> check_polarity cx polarity obj

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
  | KeysT _
  | ObjProtoT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | EvalT _
  | ExtendsT _
  | ChoiceKitT _
  | CustomFunT _
  | OpenPredT _
  | TypeMapT _
    -> () (* TODO *)

and check_polarity_propmap cx id =
  let pmap = Context.find_props cx id in
  SMap.iter (fun _ -> check_polarity_prop cx) pmap

and check_polarity_prop cx = function
  | Field (t, polarity) -> check_polarity cx polarity t
  | Get t -> check_polarity cx Positive t
  | Set t -> check_polarity cx Negative t
  | GetSet (t1, t2) ->
    check_polarity cx Positive t1;
    check_polarity cx Negative t2

and check_polarity_typeparam cx polarity tp =
  check_polarity cx polarity tp.bound

and check_polarity_typeapp cx polarity c ts =
  let reason = replace_reason (fun desc ->
    RVarianceCheck desc
  ) (reason_of_t c) in
  flow_opt cx (c, VarianceCheckT(reason, ts, polarity))

and variance_check cx polarity = function
  | [], _ | _, [] ->
    (* ignore typeapp arity mismatch, since it's handled elsewhere *)
    ()
  | tp::tps, t::ts ->
    check_polarity cx (Polarity.mult (polarity, tp.polarity)) t;
    variance_check cx polarity (tps, ts)

and polarity_mismatch cx polarity tp =
  FlowError.(add_output cx (EPolarityMismatch (tp, polarity)))

and poly_minimum_arity xs =
  List.filter (fun typeparam -> typeparam.default = None) xs
  |> List.length

(* Instantiate a polymorphic definition given type arguments. *)
and instantiate_poly_with_targs
  cx
  trace
  ~reason_op
  ~reason_tapp
  ?(cache=false)
  (xs,t)
  ts
  =
  let minimum_arity = poly_minimum_arity xs in
  let maximum_arity = List.length xs in
  let reason_arity =
    let x1, xN = List.hd xs, List.hd (List.rev xs) in
    let loc = Loc.btwn (loc_of_reason x1.reason) (loc_of_reason xN.reason) in
    mk_reason (RCustom "See type parameters of definition here") loc in
  if List.length ts > maximum_arity
  then add_output cx trace
    (FlowError.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity));
  let map, _ = List.fold_left
    (fun (map, ts) typeparam ->
      let t, ts = match typeparam, ts with
      | {default=Some default; _;}, [] ->
          (* fewer arguments than params and we have a default *)
          subst cx map default, []
      | {default=None; _;}, [] ->
          (* fewer arguments than params but no default *)
          add_output cx trace (FlowError.ETooFewTypeArgs
            (reason_tapp, reason_arity, minimum_arity));
          AnyT reason_op, []
      | _, t::ts ->
          t, ts in
      let t_ = cache_instantiate cx trace cache (typeparam, reason_op) t in
      rec_flow_t cx trace (t_, subst cx map typeparam.bound);
      SMap.add typeparam.name t_ map, ts
    )
    (SMap.empty, ts)
    xs in
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

(* Instantiate a polymorphic definition with stated bound or 'any' for args *)
(* Needed only for experimental.enforce_strict_type_args=false killswitch *)
and instantiate_poly_default_args cx trace ~reason_op ~reason_tapp (xs,t) =
  (* Remember: other_bound might refer to other type params *)
  let ts, _ = List.fold_left
    (fun (ts, map) typeparam ->
      let t = match typeparam.bound with
      | MixedT _ -> AnyT.why reason_op
      | other_bound -> AnyWithUpperBoundT (subst cx map other_bound) in
      (t::ts, SMap.add typeparam.name t map)
    ) ([], SMap.empty)
    xs in
  let ts = List.rev ts in
  instantiate_poly_with_targs cx trace ~reason_op ~reason_tapp (xs,t) ts

(* Instantiate a polymorphic definition by creating fresh type arguments. *)
and instantiate_poly cx trace ~reason_op ~reason_tapp ?(cache=false) (xs,t) =
  let ts = xs |> List.map (fun typeparam ->
    ImplicitTypeArgument.mk_targ cx (typeparam, reason_op)
  ) in
  instantiate_poly_with_targs cx trace ~reason_op ~reason_tapp ~cache (xs,t) ts

(* instantiate each param of a polymorphic type with its upper bound *)
and instantiate_poly_param_upper_bounds cx typeparams =
  let _, revlist = List.fold_left (
    fun (map, list) { name; bound; _ } ->
      let t = subst cx map bound in
      SMap.add name t map, t :: list
    ) (SMap.empty, []) typeparams in
  List.rev revlist

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
and specialize_class cx trace ~reason_op ~reason_tapp c ts =
  if ts = [] then c
  else mk_tvar_where cx reason_op (fun tvar ->
    rec_flow cx trace (c, SpecializeT (reason_op, reason_tapp, false, ts, tvar))
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
  let pmap = Context.make_property_map cx map in
  ObjT (reason, mk_objecttype ~flags dict pmap proto)

and mk_object cx reason =
  mk_object_with_proto cx reason (ObjProtoT reason)


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

(*******************************************************)
(* Entry points into the process of trying different   *)
(* branches of union and intersection types.           *)
(*******************************************************)

(* The problem we're trying to solve here is common to checking unions and
   intersections: how do we make a choice between alternatives, when (i) we have
   only partial information (i.e., while we're in the middle of type inference)
   and when (ii) we want to avoid regret (i.e., by not committing to an
   alternative that might not work out, when alternatives that were not
   considered could have worked out)?

   To appreciate the problem, consider what happens without choice. Partial
   information is not a problem: we emit constraints that must be satisfied for
   something to work, and either those constraints fail (indicating a problem)
   or they don't fail (indicating no problem). With choice and partial
   information, we cannot naively emit constraints as we try alternatives
   *without also having a mechanism to roll back those constraints*. This is
   because those constraints don't *have* to be satisfied; some other
   alternative may end up not needing those constraints to be satisfied for
   things to work out!

   It is not too hard to imagine scary scenarios we can get into without a
   roll-back mechanism. (These scenarios are not theoretical, by the way: with a
   previous implementation of union and intersection types that didn't
   anticipate these scenarios, they consistently caused a lot of problems in
   real-world use cases.)

   * One bad state we can get into is where, when trying an alternative, we emit
   constraints hoping they would be satisfied, and they appear to work. So we
   commit to that particular alternative. Then much later find out that those
   constraints are unsatified, at which point we have lost the ability to try
   other alternatives that could have worked. This leads to a class of bugs
   where a union or intersection type contains cases that should have worked,
   but they don't.

   * An even worse state we can get into is where we do discover that an
   alternative won't work out while we're still in a position of choosing
   another alternative, but in the process of making that discovery we emit
   constraints that linger on in a ghost-like state. Meanwhile, we pick another
   alternative, it works out, and we move on. Except that much later the ghost
   constraints become unsatisfied, leading to much confusion on the source of
   the resulting errors. This leads to a class of bugs where we get spurious
   errors even when a union or intersection type seems to have worked.

   So, we just implement roll-back, right? Basically...yes. But rolling back
   constraints is really hard in the current implementation. Instead, we try to
   avoid processing constraints that have side effects as much as possible while
   trying alternatives: by ensuring that (1) we don't (need to) emit too many
   constraints that have side effects (2) those that we do emit get deferred,
   instead of being processed immediately, until a choice can be made, thereby
   not participating in the choice-making process.

   (1) How do we ensure we don't emit too many constraints that have side
   effects? By fully resolving types before they participate in the
   choice-making process. Basically, we want to have as much information as we
   can before trying alternatives. It is a nice property of our implementation
   that once types are resolved, constraints emitted against them don't have
   (serious) side effects: they get simplified and simplified until we either
   hit success or failure. The details of this process is described in
   ResolvableTypeJob and in resolve_bindings.

   (2) But not all types can be fully resolved. In particular, while union and
   intersection types themselves can be fully resolved, the lower and upper
   bounds we check them against could have still-to-be-inferred types in
   them. How do we ensure that for the potentially side-effectful constraints we
   do emit on these types, we avoid undue side effects? By explicitly marking
   these types as unresolved, and deferring the execution of constraints that
   involved such marked types until a choice can be made. The details of this
   process is described in Speculation.

   There is a necessary trade-off in the approach. In particular, (2) means that
   sometimes choices cannot be made: it is ambiguous which constraints should be
   executed when trying different alternatives. We detect such ambiguities
   (conservatively, but only when a best-effort choice-making strategy doesn't
   work), and ask for additional annotations to disambiguate the relevant
   alternatives. A particularly nice property of this approach is that it is
   complete: with enough annotations it is always possible to make a
   choice. Another "meta-feature" of this approach is that it leaves room for
   incremental improvement: e.g., we would need fewer additional annotations as
   we improve our inference algorithm to detect cases where more unresolved
   tvars can be fully resolved ahead of time (in other words, detect when they
   have the "0->1" property, discussed elsewhere, roughly meaning they are
   determined by annotations).
*)

(** Every choice-making process on a union or intersection type is assigned a
    unique identifier, called the speculation_id. This identifier keeps track of
    unresolved tvars encountered when trying to fully resolve types. **)

and try_union cx trace l reason rep =
  let ts = UnionRep.members rep in
  let speculation_id = mk_id() in
  Speculation.init_speculation cx speculation_id;

  (* collect parts of the union type to be fully resolved *)
  let imap = ResolvableTypeJob.collect_of_types cx reason IMap.empty ts in
  (* collect parts of the lower bound to be fully resolved, while logging
     unresolved tvars *)
  let imap = ResolvableTypeJob.collect_of_type
    ~log_unresolved:speculation_id cx reason imap l in
  (* fully resolve the collected types *)
  resolve_bindings_init cx trace reason (bindings_of_jobs cx trace imap) @@
  (* ...and then begin the choice-making process *)
    try_flow_continuation cx trace reason speculation_id (UnionCases(l, ts))

and try_intersection cx trace u reason rep =
  let ts = InterRep.members rep in
  let speculation_id = mk_id() in
  Speculation.init_speculation cx speculation_id;

  (* collect parts of the intersection type to be fully resolved *)
  let imap = ResolvableTypeJob.collect_of_types cx reason IMap.empty ts in
  (* collect parts of the upper bound to be fully resolved, while logging
     unresolved tvars *)
  let imap = ResolvableTypeJob.collect_of_use
    ~log_unresolved:speculation_id cx reason imap u in
  (* fully resolve the collected types *)
  resolve_bindings_init cx trace reason (bindings_of_jobs cx trace imap) @@
  (* ...and then begin the choice-making process *)
    try_flow_continuation cx trace reason speculation_id (IntersectionCases(ts, u))

(* Preprocessing for intersection types.

   Before feeding into the choice-making machinery described above, we
   preprocess upper bounds of intersection types. This preprocessing seems
   asymmetric, but paradoxically, it is not: the purpose of the preprocessing is
   to bring choice-making on intersections to parity with choice-making on
   unions.

   Consider what happens when a lower bound is checked against a union type. The
   lower bound is always concretized before a choice is made! In other words,
   even if we emit a flow from an unresolved tvar to a union type, the
   constraint fires only when the unresolved tvar has been concretized.

   Now, consider checking an intersection type with an upper bound. As an
   artifact of how tvars and concrete types are processed, the upper bound would
   appear to be concrete even though the actual parts of the upper bound that
   are involved in the choice-making may be unresolved! (These parts are the
   top-level input positions in the upper bound, which end up choosing between
   the top-level input positions in the members of the intersection type.) If we
   did not concretize the parts of the upper bound involved in choice-making, we
   would start the choice-making process at a disadvantage (compared to
   choice-making with a union type and an already concretized lower
   bound). Thus, we do an extra preprocessing step where we collect the parts of
   the upper bound to be concretized, and for each combination of concrete types
   for those parts, call the choice-making process.
*)

(** The following function concretizes each tvar in unresolved in turn,
    recording their corresponding concrete lower bounds in resolved as it
    goes. At each step, it emits a ConcretizeTypes constraint on an unresolved
    tvar, which in turn calls into this function when a concrete lower bound
    appears on that tvar. **)
and prep_try_intersection cx trace reason unresolved resolved u r rep =
  match unresolved with
  | [] -> try_intersection cx trace (replace_parts resolved u) r rep
  | tvar::unresolved ->
    rec_flow cx trace (tvar, intersection_preprocess_kit reason
      (ConcretizeTypes (unresolved, resolved, IntersectionT (r, rep), u)))

(* some patterns need to be concretized before proceeding further *)
and patt_that_needs_concretization = function
  | OpenT _ | UnionT _ | MaybeT _ | OptionalT _ | AnnotT _ -> true
  | _ -> false

and concretize_patt replace patt =
  snd (List.fold_left (fun (replace, result) t ->
    if patt_that_needs_concretization t
    then List.tl replace, result@[List.hd replace]
    else replace, result@[t]
  ) (replace, []) patt)

(* for now, we only care about concretizating parts of functions and calls *)
and parts_to_replace = function
  | UseT (_, FunT (_, _, _, callt)) ->
    List.filter patt_that_needs_concretization callt.params_tlist
  | CallT (_, callt) ->
    List.filter patt_that_needs_concretization callt.params_tlist
  | _ -> []

and replace_parts replace = function
  | UseT (op, FunT (r, t1, t2, callt)) ->
    UseT (op, FunT (r, t1, t2, { callt with
      params_tlist = concretize_patt replace callt.params_tlist;
    }))
  | CallT (r, callt) ->
    CallT (r, { callt with
      params_tlist = concretize_patt replace callt.params_tlist;
    })
  | u -> u

(************************)
(* Full type resolution *)
(************************)

(* Here we continue where we left off at ResolvableTypeJob. Once we have
   collected a set of type resolution jobs, we create so-called bindings from
   these jobs. A binding is a (id, tvar) pair, where tvar is what needs to be
   resolved, and id is an identifier that serves as an index for that job.

   We don't try to fully resolve unresolved tvars that are not annotation
   sources or heads of type applications, since in general they don't satify the
   0->1 property. Instead:

   (1) When we're expecting them, e.g., when we're looking at inferred types, we
   mark them so that we can recognize them later, during speculative matching.

   (2) When we're not expecting them, e.g., when we're fully resolving union /
   intersection type annotations, we unify them as `any`. Ideally we wouldn't be
   worrying about this case, but who knows what cruft we might have accumulated
   on annotation types, so just getting that cruft out of the way.

   These decisions were made in ResolvableTypeJob.collect_of_types and are
   reflected in the use (or not) of OpenUnresolved (see below).
*)

and bindings_of_jobs cx trace jobs =
  IMap.fold ResolvableTypeJob.(fun id job bindings -> match job with
  | OpenResolved -> bindings
  | Binding t -> (id, t)::bindings
  | OpenUnresolved (log_unresolved, t) ->
    begin match log_unresolved with
    | Some speculation_id ->
      Speculation.add_unresolved_to_speculation cx speculation_id t
    | None ->
      rec_unify cx trace t AnyT.t
    end;
    bindings
  ) jobs []

(* Entry point into full type resolution. Create an identifier for the goal
   tvar, and call the general full type resolution function below. *)
and resolve_bindings_init cx trace reason bindings done_tvar =
  let id = create_goal cx done_tvar in
  resolve_bindings cx trace reason id bindings

and create_goal cx tvar =
  let i = mk_id () in
  Graph_explorer.node (Context.type_graph cx) i;
  Context.set_evaluated cx (IMap.add i tvar (Context.evaluated cx));
  i

(* Let id be the identifier associated with a tvar that is not yet
   resolved. (Here, resolved/unresolved refer to the state of the tvar in the
   context graph: does it point to Resolved _ or Unresolved _?) As soon as the
   tvar is resolved to some type, we generate some bindings by walking that
   type. Full type resolution at id now depends on full resolution of the
   ids/tvars in those bindings. The following function ensures that those
   dependencies are recorded and processed.

   Dependency management happens in Graph_explorer, using efficient data
   structures discussed therein. All we need to do here is to connect id to
   bindings in that graph, while taking care that (1) the conditions of adding
   edges to the graph are satisfied, and (2) cleaning up the effects of adding
   those edges to the graph. Finally (3) we request full type resolution of the
   bindings themselves.

   For (1), note that the graph only retains transitively closed dependencies
   from one kind of tvars to another kind of tvars. The former kind includes
   tvars that are resolved but not yet fully resolved. The latter kind includes
   tvars that are not yet resolved. Thus, in particular we must filter out
   bindings that correspond to fully resolved tvars (see
   is_unfinished_target). On the other hand, the fully_resolve_type function
   below already ensures that id is not yet fully resolved (via
   is_unexplored_source).

   For (2), after adding edges we might discover that some tvars are now fully
   resolved: this happens when, e.g., no new transitively closed dependencies
   get added on id, and full type resolution of some tvars depended only on id.
   If any of these fully resolved tvars were goal tvars, we trigger them.

   For (3) we emit a ResolveType constraint for each binding; when the
   corresponding tvar is resolved, the function fully_resolve_type below is
   called, which in turn calls back into this function (thus closing the
   recursive loop).
*)

and resolve_bindings cx trace reason id bindings =
  let bindings = filter_bindings cx bindings in
  let fully_resolve_ids = connect_id_to_bindings cx id bindings in
  ISet.iter (fun id ->
    match IMap.get id (Context.evaluated cx) with
    | None -> ()
    | Some tvar -> trigger cx trace reason tvar
  ) fully_resolve_ids;
  List.iter (resolve_binding cx trace reason) bindings

and fully_resolve_type cx trace reason id t =
  if is_unexplored_source cx id then
    let imap = ResolvableTypeJob.collect_of_type cx reason IMap.empty t in
    resolve_bindings cx trace reason id (bindings_of_jobs cx trace imap)

and filter_bindings cx =
  List.filter (fun (id, _) -> is_unfinished_target cx id)

and connect_id_to_bindings cx id bindings =
  let ids, _ = List.split bindings in
  Graph_explorer.edges (Context.type_graph cx) (id, ids)

(* Sanity conditions on source and target before adding edges to the
   graph. Nodes are in one of three states, described in Graph_explorer:
   Not_found (corresponding to unresolved tvars), Found _ (corresponding to
   resolved but not yet fully resolved tvars), and Finished (corresponding to
   fully resolved tvars). *)

and is_unexplored_source cx id =
  match Graph_explorer.stat_graph id (Context.type_graph cx) with
  | Graph_explorer.Finished -> false
  | Graph_explorer.Not_found -> false
  | Graph_explorer.Found node -> Graph_explorer.is_unexplored_node node

and is_unfinished_target cx id =
  let type_graph = Context.type_graph cx in
  match Graph_explorer.stat_graph id type_graph with
  | Graph_explorer.Finished -> false
  | Graph_explorer.Not_found ->
    Graph_explorer.node type_graph id;
    true
  | Graph_explorer.Found node ->
    not (Graph_explorer.is_finished_node node)

(** utils for creating toolkit types **)

and choice_kit reason k =
  ChoiceKitT (reason, k)

and choice_kit_use reason k =
  ChoiceKitUseT (reason, k)

and intersection_preprocess_kit reason k =
  IntersectionPreprocessKitT (reason, k)

(** utils for emitting toolkit constraints **)

and trigger cx trace reason done_tvar =
  rec_flow cx trace (choice_kit reason Trigger, UseT (UnknownUse, done_tvar))

and try_flow_continuation cx trace reason speculation_id spec =
  tvar_with_constraint cx ~trace
    (choice_kit_use reason (TryFlow (speculation_id, spec)))

and resolve_binding cx trace reason (id, t) =
  rec_flow cx trace (
    t,
    choice_kit_use reason (FullyResolveType id)
  )

(************************)
(* Speculative matching *)
(************************)

(* Speculatively match a pair of types, returning whether some error was
   encountered or not. Speculative matching happens in the context of a
   particular "branch": this context controls how some constraints emitted
   during the matching might be processed. See comments in Speculation for
   details on branches. See also speculative_matches, which calls this function
   iteratively and processes its results. *)
and speculative_match cx trace branch l u =
  let ops = Ops.get () in
  let typeapp_stack = TypeAppExpansion.get () in
  let cache = !Cache.FlowConstraint.cache in
  Speculation.set_speculative branch;
  let restore () =
    Speculation.restore_speculative ();
    Cache.FlowConstraint.cache := cache;
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

(* Speculatively match several alternatives in turn, as presented when checking
   a union or intersection type. This process maintains a so-called "match
   state" that describes the best possible choice found so far, and can
   terminate in various ways:

   (1) One of the alternatives definitely succeeds. This is straightforward: we
   can safely discard any later alternatives.

   (2) All alternatives fail. This is also straightforward: we emit an
   appropriate error message.

   (3) One of the alternatives looks promising (i.e., it doesn't immediately
   fail, but it doesn't immediately succeed either: some potentially
   side-effectful constraints, called actions, were emitted while trying the
   alternative, whose execution has been deferred), and all the later
   alternatives fail. In this scenario, we pick the promising alternative, and
   then fire the deferred actions. This is fine, because the choice cannot cause
   regret: the chosen alternative was the only one that had any chance of
   succeeding.

   (4) Multiple alternatives look promising, but the set of deferred actions
   emitted while trying the first of those alternatives form a subset of those
   emitted by later trials. Here we pick the first promising alternative (and
   fire the deferred actions). The reason this is fine is similar to (3): once
   again, the choice cannot cause any regret, because if it failed, then the
   later alternatives would have failed too. So the chosen alternative had the
   best chance of succeeding.

   (5) But sometimes, multiple alternatives look promising and we really can't
   decide which is best. This happens when the set of deferred actions emitted
   by them are incomparable, or later trials have more chances of succeeding
   than previous trials. Such scenarios typically point to real ambiguities, and
   so we ask for additional annotations on unresolved tvars to disambiguate.

   See Speculation for more details on terminology and low-level mechanisms used
   here, including what bits of information are carried by match_state and case,
   how actions are deferred and diff'd, etc.

   Because this process is common to checking union and intersection types, we
   abstract the latter into a so-called "spec." The spec is used to customize
   error messages and to ignore unresolved tvars that are deemed irrelevant to
   choice-making.
*)
and speculative_matches cx trace r speculation_id spec = Speculation.Case.(
  (* explore optimization opportunities *)
  optimize_spec cx spec;
  (* extract stuff to ignore while considering actions *)
  let ignore = ignore_of_spec spec in
  (* split spec into a list of pairs of types to try speculative matching on *)
  let trials = trials_of_spec spec in

  let rec loop match_state = function
    (* Here match_state can take on various values:

       (a) (NoMatch errs) indicates that everything has failed up to this point,
       with errors recorded in errs. Note that the initial value of acc is
       Some (NoMatch []).

       (b) (ConditionalMatch case) indicates the a promising alternative has
       been found, but not chosen yet.
    *)
    | [] -> return match_state

    | (case_id, case_r, l, u)::trials ->
      let case = { case_id; unresolved = TypeSet.empty; actions = []} in
      (* speculatively match the pair of types in this trial *)
      let error = speculative_match cx trace
        { Speculation.ignore; speculation_id; case } l u in
      match error with
      | None ->
        (* no error, looking great so far... *)
        begin match match_state with
        | Speculation.NoMatch _ ->
          (* everything had failed up to this point. so no ambiguity yet... *)
          if TypeSet.is_empty case.unresolved
          (* ...and no unresolved tvars encountered during the speculative
             match! This is great news. It means that this alternative will
             definitely succeed. Fire any deferred actions and short-cut. *)
          then fire_actions cx trace case.actions
          (* Otherwise, record that we've found a promising alternative. *)
          else loop (Speculation.ConditionalMatch case) trials

        | Speculation.ConditionalMatch prev_case ->
          (* umm, there's another previously found promising alternative *)
          (* so compute the difference in side effects between that alternative
             and this *)
          let ts = diff prev_case case in
          (* if the side effects of the previously found promising alternative
             are fewer, then keep holding on to that alternative *)
          if ts = [] then loop match_state trials
          (* otherwise, we have an ambiguity; blame the unresolved tvars and
             short-cut *)
          else begin
            let prev_case_id = prev_case.case_id in
            let cases: Type.t list = choices_of_spec spec in
            blame_unresolved cx trace prev_case_id case_id cases case_r r ts
          end
        end
      | Some err ->
        (* if an error is found, then throw away this alternative... *)
        begin match match_state with
        | Speculation.NoMatch errs ->
          (* ...adding to the error list if no promising alternative has been
             found yet *)
          loop (Speculation.NoMatch (err::errs)) trials
        | _ -> loop match_state trials
        end

  and return = function
  | Speculation.ConditionalMatch case ->
    (* best choice that survived, congrats! fire deferred actions  *)
    fire_actions cx trace case.actions
  | Speculation.NoMatch msgs ->
    (* everything failed; make a really detailed error message listing out the
       error found for each alternative *)
    let ts = choices_of_spec spec in
    let msgs = List.rev msgs in
    assert (List.length ts = List.length msgs);
    let extra = List.mapi (fun i t ->
      let reason = reason_of_t t in
      let msg = List.nth msgs i in
      reason, msg
    ) ts in
    let l,u = match spec with
      | UnionCases (l, us) ->
        let r = mk_union_reason r us in
        l, UseT (UnknownUse, EmptyT r)

      | IntersectionCases (ls, u) ->
        let r = mk_intersection_reason r ls in
        MixedT (r, Empty_intersection), u
    in
    add_output cx trace
      (FlowError.ESpeculationFailed (l, u, extra))

  in loop (Speculation.NoMatch []) trials
)

(* Make an informative error message that points out the ambiguity, and where
   additional annotations can help disambiguate. Recall that an ambiguity
   arises precisely when:

   (1) one alternative looks promising, but has some chance of failing

   (2) a later alternative also looks promising, and has some chance of not
   failing even if the first alternative fails

   ...with the caveat that "looks promising" and "some chance of failing" are
   euphemisms for some pretty conservative approximations made by Flow when it
   encounters potentially side-effectful constraints involving unresolved tvars
   during a trial.
*)
and blame_unresolved cx trace prev_i i cases case_r r ts =
  let rs = ts |> List.map (fun t ->
    rec_unify cx trace t AnyT.t;
    reason_of_t t
  ) in
  let prev_case = reason_of_t (List.nth cases prev_i) in
  let case = reason_of_t (List.nth cases i) in
  add_output cx trace (FlowError.ESpeculationAmbiguous (
    (case_r, r),
    (prev_i, prev_case),
    (i, case),
    rs
  ))

and trials_of_spec = function
  | UnionCases (l, us) ->
    List.mapi (fun i u -> (i, reason_of_t l, l, UseT (UnknownUse, u))) us
  | IntersectionCases (ls, u) ->
    List.mapi (fun i l -> (i, reason_of_use_t u, l, u)) ls

and choices_of_spec = function
  | UnionCases (_, ts)
  | IntersectionCases (ts, _)
    -> ts

and ignore_of_spec = function
  | IntersectionCases (_, CallT (_, callt)) -> Some (callt.return_t)
  | _ -> None

(* spec optimization *)
(* Currently, the only optimization we do is for disjoint unions. Specifically,
   when an object type is checked against an union of object types, we try to
   guess and record sentinel properties across object types in the union. By
   checking sentinel properties first, we force immediate match failures in the
   vast majority of cases without having to do any useless additional work. *)
and optimize_spec cx = function
  | UnionCases (l, ts) -> begin match l with
    | ObjT _ -> guess_and_record_sentinel_prop cx ts
    | _ -> ()
    end
  | IntersectionCases _ -> ()

and guess_and_record_sentinel_prop cx ts =

  let props_of_object = function
    | AnnotT (OpenT (_, id), _) ->
      let constraints = find_graph cx id in
      begin match constraints with
      | Resolved (ObjT (_, { props_tmap; _ })) ->
        Context.find_props cx props_tmap
      | _ -> SMap.empty
      end
    | ObjT (_, { props_tmap; _ }) -> Context.find_props cx props_tmap
    | _ -> SMap.empty in

  let is_singleton_type = function
    | AnnotT (OpenT (_, id), _) ->
      let constraints = find_graph cx id in
      begin match constraints with
      | Resolved (SingletonStrT _ | SingletonNumT _ | SingletonBoolT _) -> true
      | _ -> false
      end
    | SingletonStrT _ | SingletonNumT _ | SingletonBoolT _ -> true
    | _ -> false in

  (* Compute the intersection of properties of objects *)
  let prop_maps = List.map props_of_object ts in
  let acc = List.fold_left (fun acc map ->
    SMap.filter (fun s _ -> SMap.mem s map) acc
  ) (List.hd prop_maps) (List.tl prop_maps) in

  (* Keep only fields that have singleton types *)
  let acc = SMap.filter (fun _ p ->
    match p with
    | Field (t, _) -> is_singleton_type t
    | _ -> false
  ) acc in

  if not (SMap.is_empty acc) then
    (* Record the guessed sentinel properties for each object *)
    let keys = SMap.fold (fun s _ keys -> SSet.add s keys) acc SSet.empty in
    List.iter (function
      | AnnotT (OpenT (_, id), _) ->
        let constraints = find_graph cx id in
        begin match constraints with
        | Resolved (ObjT (_, { props_tmap; _ })) ->
          Cache.SentinelProp.add props_tmap keys
        | _ -> ()
        end
      | ObjT (_, { props_tmap; _ }) ->
        Cache.SentinelProp.add props_tmap keys
      | _ -> ()
    ) ts

and fire_actions cx trace = List.iter (function
  | _, Speculation.Action.Flow (l, u) -> rec_flow cx trace (l, u)
  | _, Speculation.Action.Unify (t1, t2) -> rec_unify cx trace t1 t2
)

and mk_union_reason r us =
  List.fold_left (fun reason t ->
    let rdesc = string_of_desc (desc_of_reason reason) in
    let tdesc = string_of_desc (desc_of_reason (reason_of_t t)) in
    let udesc = if not (String_utils.string_starts_with rdesc "union:")
      then spf "union: %s" tdesc
      else if String_utils.string_ends_with rdesc "..."
      then rdesc
      else if String_utils.string_ends_with rdesc (tdesc ^ "(s)")
      then rdesc
      else if String.length rdesc >= 256
      then spf "%s | ..." rdesc
      else if String_utils.string_ends_with rdesc tdesc
      then spf "%s(s)" rdesc
      else spf "%s | %s" rdesc tdesc
    in
    replace_reason_const (RCustom udesc) reason
  ) r us

and mk_intersection_reason r _ls =
  replace_reason_const RIntersection r

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

and lookup_prop cx trace l reason_prop reason_op strict x action =
  let l =
    (* munge names beginning with single _ *)
    if is_munged_prop_name cx x
    then ObjProtoT (reason_of_t l)
    else l
  in
  let propref = Named (reason_prop, x) in
  rec_flow cx trace (l, LookupT (reason_op, strict, [], propref, action))

and get_prop cx trace reason_prop reason_op strict super x map tout =
  let ops = Ops.clear () in
  let u = ReposLowerT (reason_op, UseT (UnknownUse, tout)) in
  begin match SMap.get x map with
  | Some p ->
    (match Property.read_t p with
    | Some t ->
      rec_flow cx trace (t, u)
    | None ->
      add_output cx trace
        (FlowError.EPropAccess ((reason_op, reason_op), Some x, p, Read)))
  | None ->
    let tout = tvar_with_constraint cx ~trace u in
    lookup_prop cx trace super reason_prop reason_op strict x
      (RWProp (tout, Read))
  end;
  Ops.set ops

and set_prop cx trace reason_prop reason_op strict super x pmap tin =
  match SMap.get x pmap with
  | Some p ->
    (match Property.write_t p with
    | Some t ->
      rec_flow_t cx trace (tin, t)
    | None ->
      add_output cx trace
        (FlowError.EPropAccess ((reason_prop, reason_op), Some x, p, Write)))
  | None ->
    lookup_prop cx trace super reason_prop reason_op strict x
      (RWProp (tin, Write))

and get_obj_prop cx trace o propref reason_op =
  let named_prop = match propref with
  | Named (_, x) -> Context.get_prop cx o.props_tmap x
  | Computed _ -> None
  in
  match propref, named_prop, o.dict_t with
  | _, Some _, _ ->
    (* Property exists on this property map *)
    named_prop
  | Named (_, x), None, Some { key; value; dict_polarity; _ }
    when not (is_dictionary_exempt x) ->
    (* Dictionaries match all property reads *)
    rec_flow_t cx trace (string_key x reason_op, key);
    Some (Field (value, dict_polarity))
  | Computed k, None, Some { key; value; dict_polarity; _ } ->
    rec_flow_t cx trace (k, key);
    Some (Field (value, dict_polarity))
  | _ -> None

and read_obj_prop cx trace o propref reason_obj reason_op tout =
  let ops = Ops.clear () in
  (match get_obj_prop cx trace o propref reason_op with
  | Some p ->
    let up = Field (tout, Positive) in
    rec_flow_p cx trace reason_obj reason_op propref (p, up)
  | None ->
    match propref with
    | Named _ ->
      let strict =
        if sealed_in_op reason_op o.flags.sealed
        then Strict reason_obj
        else ShadowRead (None, Nel.one o.props_tmap)
      in
      rec_flow cx trace (o.proto_t,
        LookupT (reason_op, strict, [], propref, RWProp (tout, Read)))
    | Computed elem_t ->
      match elem_t with
      | OpenT _ ->
        let loc = loc_of_t elem_t in
        add_output cx trace FlowError.(EInternal (loc, PropRefComputedOpen))
      | StrT (_, Literal _) ->
        let loc = loc_of_t elem_t in
        add_output cx trace FlowError.(EInternal (loc, PropRefComputedLiteral))
      | AnyT _ | StrT _ | NumT _ ->
        (* any, string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
        rec_flow_t cx trace (AnyT.why reason_op, tout)
      | _ ->
        let reason_prop = reason_of_t elem_t in
        add_output cx trace (FlowError.EObjectComputedPropertyAccess
          (reason_op, reason_prop)));
  Ops.set ops

and write_obj_prop cx trace o propref reason_obj reason_op tin =
  match get_obj_prop cx trace o propref reason_op with
  | Some p ->
    let up = Field (tin, Negative) in
    rec_flow_p cx trace reason_obj reason_op propref (p, up)
  | None ->
    match propref with
    | Named (reason_prop, _) ->
      if sealed_in_op reason_op o.flags.sealed
      then
        add_output cx trace (FlowError.EPropNotFound (reason_prop, reason_obj))
      else
        let strict = ShadowWrite (Nel.one o.props_tmap) in
        rec_flow cx trace (o.proto_t,
          LookupT (reason_op, strict, [], propref, RWProp (tin, Write)))
    | Computed elem_t ->
      match elem_t with
      | OpenT _ ->
        let loc = loc_of_t elem_t in
        add_output cx trace FlowError.(EInternal (loc, PropRefComputedOpen))
      | StrT (_, Literal _) ->
        let loc = loc_of_t elem_t in
        add_output cx trace FlowError.(EInternal (loc, PropRefComputedLiteral))
      | AnyT _ | StrT _ | NumT _ ->
        (* any, string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
        rec_flow_t cx trace (tin, AnyT.why reason_op)
      | _ ->
        let reason_prop = reason_of_t elem_t in
        add_output cx trace (FlowError.EObjectComputedPropertyAssign
          (reason_op, reason_prop));

and find_or_intro_shadow_prop cx trace x =
  let intro_shadow_prop id =
    let reason_prop = locationless_reason (RShadowProperty x) in
    let t = mk_tvar cx reason_prop in
    let p = Field (t, Neutral) in
    Context.set_prop cx id (internal_name x) p;
    t, p
  in

  (* Given some shadow property type and a prototype chain (o.proto,
   * o.proto.proto, ...), link all types along the prototype chain together.
   * If there is a write to the prototype later on, we unify the property types
   * together. If there is no write, the property types are safely independent.
   *)
  let rec chain_link t = function
  | [] -> ()
  | id::ids ->
    let t_proto = Property.assert_field (find (id, ids)) in
    rec_flow cx trace (t_proto, UnifyT (t_proto, t))

  (* Check at each step to see if a prop was added since we looked.
   *
   * Imports and builtins are merged in after local inference, potentially
   * deferring multiple shadow reads/writes on a tvar. If this shadow read
   * follow a deferred shadow write, a property will exist. If it follows a
   * deferred shadow read, a shadow property will exist. In either case, we
   * don't need to create a shadow property, nor do we need to continue
   * unifying up the proto chain, as the work is necessarily already done.
   *)
  and find (id, proto_ids) =
    match Context.get_prop cx id x with
    | Some p -> p
    | None ->
      match Context.get_prop cx id (internal_name x) with
      | Some p -> p
      | None ->
        let t, p = intro_shadow_prop id in
        chain_link t proto_ids;
        p

  in find

(* other utils *)

and filter cx trace t l pred =
  if (pred l) then rec_flow_t cx trace (l,t)

and is_string = function AnyT _ | StrT _ -> true | _ -> false
and is_number = function AnyT _ | NumT _ -> true | _ -> false
and is_function = function AnyT _ | AnyFunT _ | FunT _ -> true | _ -> false
and is_object = function
  | AnyT _
  | AnyObjT _
  | ObjT _
  | ArrT _
  | NullT _ -> true
  | _ -> false
and is_array = function AnyT _ | ArrT _ -> true | _ -> false
and is_bool = function AnyT _ | BoolT _ -> true | _ -> false

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
  | t0::t1::ts -> UnionT (r, UnionRep.make t0 t1 ts)

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
    UnionT (reason, UnionRep.make (NullT.why reason) (VoidT.why reason) [])
  | BoolT (r, None) -> BoolT (r, Some false)
  | StrT (r, AnyLiteral) -> StrT (r, Literal "")
  | NumT (r, AnyLiteral) -> NumT (r, Literal (0., "0"))

  (* things that don't track truthiness pass through *)
  | t -> t

and filter_maybe = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, UnionRep.make (NullT.why reason) (VoidT.why reason) [])
  | MixedT (r, Mixed_everything) ->
    UnionT (r, UnionRep.make (NullT.why r) (VoidT.why r) [])
  | MixedT (r, Mixed_truthy) -> EmptyT.why r
  | MixedT (r, Mixed_non_maybe) -> EmptyT.why r
  | MixedT (r, Mixed_non_void) -> NullT r
  | MixedT (r, Mixed_non_null) -> VoidT r
  | NullT _ as t -> t
  | VoidT _ as t -> t
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | AnyT _ as t -> t
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
  | NullT _ as t -> t
  | MixedT (r, Mixed_everything)
  | MixedT (r, Mixed_non_void) -> NullT.why r
  | AnyT _ as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_null = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, UnionRep.make (VoidT.why reason) t [])
  | OptionalT t -> OptionalT (filter_not_null t)
  | UnionT (r, rep) ->
    recurse_into_union filter_not_null (r, UnionRep.members rep)
  | NullT r -> EmptyT r
  | MixedT (r, Mixed_everything) -> MixedT (r, Mixed_non_null)
  | MixedT (r, Mixed_non_void) -> MixedT (r, Mixed_non_maybe)
  | t -> t

and filter_undefined = function
  | MaybeT t -> VoidT.why (reason_of_t t)
  | VoidT _ as t -> t
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | MixedT (r, Mixed_everything)
  | MixedT (r, Mixed_non_null) -> VoidT.why r
  | AnyT _ as t -> t
  | t ->
    let reason = reason_of_t t in
    EmptyT.why reason

and filter_not_undefined = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, UnionRep.make (NullT.why reason) t [])
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
  | AnyT _ as t -> t
  | _ -> EmptyT (reason_of_t t)

and filter_not_string_literal expected = function
  | StrT (r, Literal actual) when actual = expected -> EmptyT r
  | t -> t

and filter_number_literal expected t = match t with
  | NumT (_, Literal actual) when snd actual = snd expected -> t
  | NumT (r, Truthy) when snd expected <> "0" -> NumT (r, Literal expected)
  | NumT (r, AnyLiteral) -> NumT (r, Literal expected)
  | MixedT (r, _) -> NumT (r, Literal expected)
  | AnyT _ as t -> t
  | _ -> EmptyT (reason_of_t t)

and filter_not_number_literal expected = function
  | NumT (r, Literal actual) when snd actual = snd expected -> EmptyT r
  | t -> t

and filter_true = function
  | BoolT (r, Some true)
  | BoolT (r, None) -> BoolT (r, Some true)
  | MixedT (r, _) -> BoolT (replace_reason_const RBoolean r, Some true)
  | AnyT _ as t -> t
  | t -> EmptyT (reason_of_t t)

and filter_not_true = function
  | BoolT (r, Some true) -> EmptyT r
  | BoolT (r, None) -> BoolT (r, Some false)
  | t -> t

and filter_false = function
  | BoolT (r, Some false)
  | BoolT (r, None) -> BoolT (r, Some false)
  | MixedT (r, _) -> BoolT (replace_reason_const RBoolean r, Some false)
  | AnyT _ as t -> t
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

(**********)
(* guards *)
(**********)

and guard cx trace source pred result sink = match pred with

| ExistsP ->
  begin match filter_exists source with
  | EmptyT _ -> ()
  | _ -> rec_flow_t cx trace (result, sink)
  end

| NotP ExistsP ->
  begin match filter_not_exists source with
  | EmptyT _ -> ()
  | _ -> rec_flow_t cx trace (result, sink)
  end

| _ ->
  let loc = loc_of_reason (reason_of_t sink) in
  let pred_str = string_of_predicate pred in
  add_output cx trace
    FlowError.(EInternal (loc, UnsupportedGuardPredicate pred_str))

(**************)
(* predicates *)
(**************)

(* t - predicate output recipient (normally a tvar)
   l - incoming concrete LB (predicate input)
   result - guard result in case of success
   p - predicate *)
and predicate cx trace t l p = match p with

  (************************)
  (* deconstruction of && *)
  (************************)

  | AndP (p1,p2) ->
    let reason = replace_reason_const RAnd (reason_of_t t) in
    let tvar = mk_tvar cx reason in
    rec_flow cx trace (l,PredicateT(p1,tvar));
    rec_flow cx trace (tvar,PredicateT(p2,t))

  (************************)
  (* deconstruction of || *)
  (************************)

  | OrP (p1, p2) ->
    rec_flow cx trace (l,PredicateT(p1,t));
    rec_flow cx trace (l,PredicateT(p2,t))

  (*********************************)
  (* deconstruction of binary test *)
  (*********************************)

  (* when left is evaluated, store it and evaluate right *)
  | LeftP (b, r) ->
    rec_flow cx trace (r, PredicateT(RightP(b, l), t))
  | NotP LeftP (b, r) ->
    rec_flow cx trace (r, PredicateT(NotP(RightP(b, l)), t))

  (* when right is evaluated, call appropriate handler *)
  | RightP (b, actual_l) ->
    let r = l in
    let l = actual_l in
    binary_predicate cx trace true b l r t
  | NotP RightP (b, actual_l) ->
    let r = l in
    let l = actual_l in
    binary_predicate cx trace false b l r t

  (***********************)
  (* typeof _ ~ "boolean" *)
  (***********************)

  | BoolP ->
    begin match l with
    | MixedT (r, Mixed_truthy) ->
      let r = replace_reason_const BoolT.desc r in
      rec_flow_t cx trace (BoolT (r, Some true), t)

    | MixedT (r, _) ->
      rec_flow_t cx trace (BoolT.why r, t)

    | _ ->
      filter cx trace t l is_bool
    end

  | NotP BoolP ->
    filter cx trace t l (not_ is_bool)

  (***********************)
  (* typeof _ ~ "string" *)
  (***********************)

  | StrP ->
    begin match l with
    | MixedT (r, Mixed_truthy) ->
      let r = replace_reason_const StrT.desc r in
      rec_flow_t cx trace (StrT (r, Truthy), t)

    | MixedT (r, _) ->
      rec_flow_t cx trace (StrT.why r, t)

    | _ ->
      filter cx trace t l is_string
    end

  | NotP StrP ->
    filter cx trace t l (not_ is_string)

  (*********************)
  (* _ ~ "some string" *)
  (*********************)

  | SingletonStrP lit ->
    rec_flow_t cx trace (filter_string_literal lit l, t)

  | NotP SingletonStrP lit ->
    rec_flow_t cx trace (filter_not_string_literal lit l, t)

  (*********************)
  (* _ ~ some number n *)
  (*********************)

  | SingletonNumP lit ->
    rec_flow_t cx trace (filter_number_literal lit l, t)

  | NotP SingletonNumP lit ->
    rec_flow_t cx trace (filter_not_number_literal lit l, t)

  (***********************)
  (* typeof _ ~ "number" *)
  (***********************)

  | NumP ->
    begin match l with
    | MixedT (r, Mixed_truthy) ->
      let r = replace_reason_const NumT.desc r in
      rec_flow_t cx trace (NumT (r, Truthy), t)

    | MixedT (r, _) ->
      rec_flow_t cx trace (NumT.why r, t)

    | _ ->
      filter cx trace t l is_number
    end

  | NotP NumP ->
    filter cx trace t l (not_ is_number)

  (***********************)
  (* typeof _ ~ "function" *)
  (***********************)

  | FunP ->
    begin match l with
    | MixedT (r, _) ->
      let desc = RFunction RNormal in
      rec_flow_t cx trace (AnyFunT (replace_reason_const desc r), t)

    | _ ->
      filter cx trace t l is_function
    end

  | NotP FunP ->
    filter cx trace t l (not_ is_function)

  (***********************)
  (* typeof _ ~ "object" *)
  (***********************)

  | ObjP ->
    begin match l with
    | MixedT (r, flavor) ->
      let reason = replace_reason_const RObject r in
      let dict = Some {
        key = StrT.why r;
        value = MixedT (replace_reason_const MixedT.desc r, Mixed_everything);
        dict_name = None;
        dict_polarity = Neutral;
      } in
      let proto = ObjProtoT reason in
      let obj = mk_object_with_proto cx reason ?dict proto in
      let filtered_l = match flavor with
      | Mixed_truthy
      | Mixed_non_maybe
      | Mixed_non_null -> obj
      | Mixed_everything
      | Mixed_non_void ->
        let reason = replace_reason_const RUnion (reason_of_t t) in
        UnionT (reason, UnionRep.make (NullT.why r) obj [])
      | Empty_intersection -> EmptyT r
      in
      rec_flow_t cx trace (filtered_l, t)

    | _ ->
      filter cx trace t l is_object
    end

  | NotP ObjP ->
    filter cx trace t l (not_ is_object)

  (*******************)
  (* Array.isArray _ *)
  (*******************)

  | ArrP ->
    begin match l with
    | MixedT (r, _) ->
      let filtered_l = ArrT (
        replace_reason_const RArray r,
        MixedT (r, Mixed_everything),
        []
      ) in
      rec_flow_t cx trace (filtered_l, t)

    | _ ->
      filter cx trace t l is_array
    end

  | NotP ArrP ->
    filter cx trace t l (not_ is_array)

  (***********************)
  (* typeof _ ~ "undefined" *)
  (***********************)

  | VoidP ->
    rec_flow_t cx trace (filter_undefined l, t)

  | NotP VoidP ->
    rec_flow_t cx trace (filter_not_undefined l, t)

  (********)
  (* null *)
  (********)

  | NullP ->
    rec_flow_t cx trace (filter_null l, t)

  | NotP NullP ->
    rec_flow_t cx trace (filter_not_null l, t)

  (*********)
  (* maybe *)
  (*********)

  | MaybeP ->
    rec_flow_t cx trace (filter_maybe l, t)

  | NotP MaybeP ->
    rec_flow_t cx trace (filter_not_maybe l, t)

  (********)
  (* true *)
  (********)

  | SingletonBoolP true ->
    rec_flow_t cx trace (filter_true l, t)

  | NotP (SingletonBoolP true) ->
    rec_flow_t cx trace (filter_not_true l, t)

  (*********)
  (* false *)
  (*********)

  | SingletonBoolP false ->
    rec_flow_t cx trace (filter_false l, t)

  | NotP (SingletonBoolP false) ->
    rec_flow_t cx trace (filter_not_false l, t)

  (************************)
  (* truthyness *)
  (************************)

  | ExistsP ->
    rec_flow_t cx trace (filter_exists l, t)

  | NotP ExistsP ->
    rec_flow_t cx trace (filter_not_exists l, t)

  | PropExistsP (reason, key) ->
    prop_exists_test cx trace reason key true l t

  | NotP (PropExistsP (reason, key)) ->
    prop_exists_test cx trace reason key false l t

  (* unreachable *)
  | NotP (NotP _)
  | NotP (AndP _)
  | NotP (OrP _) ->
    assert_false (spf "Unexpected predicate %s" (string_of_predicate p))

  (********************)
  (* Latent predicate *)
  (********************)

  | LatentP (fun_t, idx) ->
    let reason = replace_reason (fun desc ->
      RPredicateCall desc
    ) (reason_of_t fun_t) in
    rec_flow cx trace (fun_t, CallLatentPredT (reason, true, idx, l, t))

  | NotP (LatentP (fun_t, idx)) ->
      let neg_reason = replace_reason (fun desc ->
        RPredicateCallNeg desc
      ) (reason_of_t fun_t) in
      rec_flow cx trace (fun_t,
        CallLatentPredT (neg_reason, false, idx, l, t))

and prop_exists_test cx trace reason key sense obj result =
  prop_exists_test_generic reason key cx trace result obj sense obj

and prop_exists_test_generic
    reason key cx trace result orig_obj sense = function
  | ObjT (lreason, { flags; props_tmap; _}) as obj ->
    (match Context.get_prop cx props_tmap key with
    | Some p ->
      (match Property.read_t p with
      | Some t ->
        (* prop is present on object type *)
        let pred = if sense then ExistsP else NotP ExistsP in
        rec_flow cx trace (t, GuardT (pred, orig_obj, result))
      | None ->
        (* prop cannot be read *)
        add_output cx trace
          (FlowError.EPropAccess ((lreason, reason), Some key, p, Read)))
    | None when flags.exact && sealed_in_op (reason_of_t result) flags.sealed ->
      (* prop is absent from exact object type *)
      if sense
      then ()
      else rec_flow_t cx trace (orig_obj, result)
    | None ->
      (* prop is absent from inexact object type *)
      (* TODO: possibly unsound to filter out orig_obj here, but if we don't,
         case elimination based on prop existence checking doesn't work for
         (disjoint unions of) intersections of objects, where the prop appears
         in a different branch of the intersection. It is easy to avoid this
         unsoundness with slightly more work, but will wait until a
         refactoring of property lookup lands to revisit. Tracked by
         #11301092. *)
      if orig_obj = obj then rec_flow_t cx trace (orig_obj, result))

  | IntersectionT (_, rep) ->
    (* For an intersection of object types, try the test for each object type in
       turn, while recording the original intersection so that we end up with
       the right refinement. See the comment on the implementation of
       IntersectionPreprocessKit for more details. *)
    let reason = reason_of_t result in
    InterRep.members rep |> List.iter (fun obj ->
      rec_flow cx trace (obj,
        intersection_preprocess_kit reason
          (PropExistsTest(sense, key, orig_obj, result))))

  | _ ->
    rec_flow_t cx trace (orig_obj, result)

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
    let arrt = get_builtin_typeapp cx ~trace reason "Array" [elemt] in
    rec_flow cx trace (arrt, PredicateT(LeftP(InstanceofTest, right), result))

  | (false, (ArrT (reason, elemt, _) as arr), ClassT(InstanceT _ as a)) ->

    let right = ClassT(ExtendsT([], arr, a)) in
    let arrt = get_builtin_typeapp cx ~trace reason "Array" [elemt] in
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
      (ClassT (ExtendsT([], c, a)))
      (RightP (InstanceofTest, c))

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

  | (true, ObjProtoT _, ClassT(ExtendsT (_, _, a)))
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
      (ClassT(ExtendsT([], c, a)))
      (NotP(RightP(InstanceofTest, c)))

  (** If C is a subclass of A, then do nothing, since this check cannot
      succeed. Otherwise, don't refine the type of x. **)
  | (false, InstanceT (_,_,super_c,instance_c),
     (ClassT(ExtendsT(_, _, InstanceT (_,_,_,instance_a))) as right))
    ->

    if instance_a.class_id = instance_c.class_id
    then ()
    else rec_flow cx trace
      (super_c, PredicateT(NotP(LeftP(InstanceofTest, right)), result))

  | (false, ObjProtoT _, ClassT(ExtendsT(_, c, _)))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow_t cx trace (c, result)

  (** Don't refine the type when any other `instanceof` check fails. **)
  | (false, left, _) ->
    rec_flow_t cx trace (left, result)

and sentinel_prop_test key cx trace result (sense, obj, t) =
  sentinel_prop_test_generic key cx trace result obj (sense, obj, t)

and sentinel_prop_test_generic key cx trace result orig_obj =
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

  let flow_sentinel sense props_tmap obj sentinel =
    match Context.get_prop cx props_tmap key with
    | Some p ->
      (match Property.read_t p with
      | Some t ->
        let test = SentinelPropTestT (orig_obj, sense, sentinel, result) in
        rec_flow cx trace (t, test)
      | None ->
        let reason_obj = reason_of_t obj in
        let reason = reason_of_t result in
        add_output cx trace
          (FlowError.EPropAccess ((reason_obj, reason), Some key, p, Read)))
    | None ->
      (* TODO: possibly unsound to filter out orig_obj here, but if we
         don't, case elimination based on sentinel prop checking doesn't
         work for (disjoint unions of) intersections of objects, where the
         sentinel prop and the payload appear in different branches of the
         intersection. It is easy to avoid this unsoundness with slightly
         more work, but will wait until a refactoring of property lookup
         lands to revisit. Tracked by #11301092. *)
      if orig_obj = obj then rec_flow_t cx trace (orig_obj, result)
  in
  function
  (* obj.key ===/!== string value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), StrT (_, Literal value)) ->
      flow_sentinel sense props_tmap obj (SentinelStr value)

  (* instance.key ===/!== string value *)
  | (sense, (InstanceT (_, _, _, { fields_tmap; _}) as obj),
      StrT (_, Literal value)) ->
      flow_sentinel sense fields_tmap obj (SentinelStr value)

  (* obj.key ===/!== number value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), NumT (_, Literal value)) ->
      flow_sentinel sense props_tmap obj (SentinelNum value)

  (* obj.key ===/!== boolean value *)
  | (sense, (ObjT (_, { props_tmap; _}) as obj), BoolT (_, Some value)) ->
      flow_sentinel sense props_tmap obj (SentinelBool value)

  | sense, IntersectionT (_, rep),
     (StrT (_, Literal _) | NumT (_, Literal (_, _)) | BoolT (_, Some _) as t)
    ->
    (* For an intersection of object types, try the test for each object type in
       turn, while recording the original intersection so that we end up with
       the right refinement. See the comment on the implementation of
       IntersectionPreprocessKit for more details. *)
    let reason = reason_of_t result in
    InterRep.members rep |> List.iter (fun obj ->
      rec_flow cx trace (
        obj,
        intersection_preprocess_kit reason
          (SentinelPropTest(sense, key, t, orig_obj, result))
      )
    )

  | _ -> (* not enough info to refine *)
    rec_flow_t cx trace (orig_obj, result)

(*******************************************************************)
(* /predicate *)
(*******************************************************************)

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
    let t2_use = UseT (UnknownUse, t2) in
    edges_and_flows_to_t cx trace ~opt:true (id1, bounds1) t2_use;
    edges_and_flows_from_t cx trace ~opt:true t2 (id1, bounds1);

  | Resolved t1, Unresolved bounds2 ->
    let t1_use = UseT (UnknownUse, t1) in
    replace_node cx id2 (Root { root2 with constraints = Resolved t1 });
    edges_and_flows_to_t cx trace ~opt:true (id2, bounds2) t1_use;
    edges_and_flows_from_t cx trace ~opt:true t1 (id2, bounds2);

  | Resolved t1, Resolved t2 ->
    rec_unify cx trace t1 t2;
  );
  replace_node cx id1 (Goto id2)

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
    edges_and_flows_to_t cx trace ~opt:true (id, bounds) (UseT (UnknownUse, t));
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

and __unify cx t1 t2 trace =
  begin match Context.verbose cx with
  | Some { Verbose.indent; depth } ->
    let indent = String.make ((Trace.trace_depth trace - 1) * indent) ' ' in
    let pid = Context.pid_prefix cx in
    prerr_endlinef
      "\n%s%s%s =\n%s%s%s"
      indent pid (Debug_js.dump_t ~depth cx t1)
      indent pid (Debug_js.dump_t ~depth cx t2)
  | None -> ()
  end;

  if t1 = t2 then () else (

  (* In general, unifying t1 and t2 should have similar effects as flowing t1 to
     t2 and flowing t2 to t1. This also means that any restrictions on such
     flows should also be enforced here. In particular, we don't expect t1 or t2
     to be type parameters, and we don't expect t1 or t2 to be def types that
     don't make sense as use types. See __flow for more details. *)
  not_expect_bound t1;
  not_expect_bound t2;
  expect_proper_def t1;
  expect_proper_def t2;

  (* Before processing the unify action, check that it is not deferred. If it
     is, then when speculation is complete, the action either fires or is
     discarded depending on whether the case that created the action is
     selected or not. *)
  if not Speculation.(defer_action cx (Action.Unify (t1, t2))) then

  match t1, t2 with

  | OpenT (_, id1), OpenT (_, id2) ->
    merge_ids cx trace id1 id2

  | OpenT (_, id), t when ok_unify t ->
    resolve_id cx trace id t
  | t, OpenT (_, id) when ok_unify t ->
    resolve_id cx trace id t

  | PolyT (params1, t1), PolyT (params2, t2)
    when List.length params1 = List.length params2 ->
    (** for equal-arity polymorphic types, unify param upper bounds
        with each other, then instances parameterized by these *)
    let args1 = instantiate_poly_param_upper_bounds cx params1 in
    let args2 = instantiate_poly_param_upper_bounds cx params2 in
    List.iter2 (rec_unify cx trace) args1 args2;
    let inst1 =
      let r = reason_of_t t1 in
      instantiate_poly_with_targs cx trace
        ~reason_op:r ~reason_tapp:r (params1, t1) args1 in
    let inst2 =
      let r = reason_of_t t2 in
      instantiate_poly_with_targs cx trace
        ~reason_op:r ~reason_tapp:r (params2, t2) args2 in
    rec_unify cx trace inst1 inst2

  | ArrT (_, t1, ts1), ArrT (_, t2, ts2) ->
    array_unify cx trace (ts1, t1, ts2, t2)

  | ObjT (lreason, { props_tmap = lflds; dict_t = ldict; _ }),
    ObjT (ureason, { props_tmap = uflds; dict_t = udict; _ }) ->

    (* ensure the keys and values are compatible with each other. *)
    begin match ldict, udict with
    | Some {key = lk; value = lv; _}, Some {key = uk; value = uv; _} ->
        rec_unify cx trace lk uk;
        rec_unify cx trace lv uv
    | Some _, None ->
        let lreason = replace_reason_const RSomeProperty lreason in
        add_output cx trace (FlowError.EPropNotFound (lreason, ureason))
    | None, Some _ ->
        let ureason = replace_reason_const RSomeProperty ureason in
        add_output cx trace (FlowError.EPropNotFound (ureason, lreason))
    | None, None -> ()
    end;

    let lpmap = Context.find_props cx lflds in
    let upmap = Context.find_props cx uflds in
    SMap.merge (fun x lp up ->
      if not (is_internal_name x || is_dictionary_exempt x)
      then (match lp, up with
      | Some p1, Some p2 ->
          unify_props cx trace x lreason ureason p1 p2
      | Some p1, None ->
          unify_prop_with_dict cx trace x p1 lreason ureason udict
      | None, Some p2 ->
          unify_prop_with_dict cx trace x p2 ureason lreason ldict
      | None, None -> ());
      None
    ) lpmap upmap |> ignore

  | FunT (_, _, _, funtype1), FunT (_, _, _, funtype2)
      when List.length funtype1.params_tlist =
           List.length funtype2.params_tlist ->
    rec_unify cx trace funtype1.this_t funtype2.this_t;
    List.iter2 (rec_unify cx trace) funtype1.params_tlist funtype2.params_tlist;
    rec_unify cx trace funtype1.return_t funtype2.return_t

  | TypeAppT (c1, ts1), TypeAppT (c2, ts2)
    when c1 = c2 && List.length ts1 = List.length ts2 ->
    List.iter2 (rec_unify cx trace) ts1 ts2

  | RestT t1, RestT t2
  | RestT t1, t2 | t1, RestT t2 ->
    rec_unify cx trace t1 t2

  | _ ->
    naive_unify cx trace t1 t2
  )

and unify_props cx trace x r1 r2 p1 p2 =
  (* If both sides are neutral fields, we can just unify once *)
  match p1, p2 with
  | Field (t1, Neutral),
    Field (t2, Neutral) ->
    rec_unify cx trace t1 t2;
  | _ ->
    (* Otherwise, unify read/write sides separately. *)
    (match Property.read_t p1, Property.read_t p2 with
    | Some t1, Some t2 ->
        rec_unify cx trace t1 t2;
    | _ -> ());
    (match Property.write_t p1, Property.write_t p2 with
    | Some t1, Some t2 ->
        rec_unify cx trace t1 t2;
    | _ -> ());
    (* Error if polarity is not compatible both ways. *)
    let polarity1 = Property.polarity p1 in
    let polarity2 = Property.polarity p2 in
    if not (
      Polarity.compat (polarity1, polarity2) &&
      Polarity.compat (polarity2, polarity1)
    ) then
      add_output cx trace
        (FlowError.EPropPolarityMismatch ((r1, r2), Some x, (polarity1, polarity2)))

(* If some property `x` exists in one object but not another, ensure the
   property is compatible with a dictionary, or error if none. *)
and unify_prop_with_dict cx trace x p prop_reason dict_reason dict =
  let prop_reason = replace_reason_const (RProperty (Some x)) prop_reason in
  match dict with
  | Some { key; value; dict_polarity; _ } ->
    rec_flow_t cx trace (string_key x prop_reason, key);
    let p2 = Field (value, dict_polarity) in
    unify_props cx trace x prop_reason dict_reason p p2
  | None ->
    add_output cx trace (FlowError.EPropNotFound (prop_reason, dict_reason))

(* TODO: Unification between concrete types is still implemented as
   bidirectional flows. This means that the destructuring work is duplicated,
   and we're missing some opportunities for nested unification. *)

and naive_unify cx trace t1 t2 =
  rec_flow_t cx trace (t1,t2); rec_flow_t cx trace (t2,t1)

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

(* Subtyping of arrays is complicated by tuples. Currently, there are three
   different kinds of types, all encoded by arrays:

   1. Array<T> (array type)
   2. [T1, T2] (tuple type)
   3. "internal" Array<X>[T1, T2] where T1 | T2 ~> X (array literal type)

   We have the following rules:

   (1) When checking types against Array<U>, the rules are not surprising. Array
   literal types behave like array types in these checks.

   * Array<T> ~> Array<U> checks T <~> U
   * [T1, T2] ~> Array<U> checks T1 | T2 ~> U
   * Array<X>[T1, T2] ~> Array<U> checks Array<X> ~> Array<U>

   (2) When checking types against [T1, T2], the rules are again not
   surprising. Array literal types behave like tuple types in these checks. We
   consider missing tuple elements to be undefined, following common usage (and
   consistency with missing call arguments).

   * Array<T> ~> [U1, U2] checks T ~> U1, T ~> U2
   * [T1, T2] ~> [U1, U2] checks T1 ~> U1 and T2 ~> U2
   * [T1, T2] ~> [U1] checks T1 ~> U1
   * [T1] ~> [U1, U2] checks T1 ~> U1 and void ~> U2
   * Array<X>[T1, T2] ~> [U1, U2] checks [T1, T2] ~> [U1, U2]

   (3) When checking types against Array<Y>[U1, U2], the rules are a bit
   unsound. Array literal types were not designed to appear as upper bounds. In
   particular, their summary element types are often overly precise. Checking
   individual element types of one array literal type against the summary
   element type of another array literal type can lead to crazy errors, so we
   currently drop such checks.

   TODO: Make these rules great again by computing more reasonable summary
   element types for array literal types.

   * Array<T> ~> Array<Y>[U1, U2] checks Array<T> ~> Array<Y>
   * [T1, T2] ~> Array<Y>[U1, U2] checks T1 ~> U1, T2 ~> U2
   * [T1, T2] ~> Array<Y>[U1] checks T1 ~> U1
   * [T1] ~> Array<Y>[U1, U2] checks T1 ~> U1
   * Array<X>[T1, T2] ~> Array<Y>[U1, U2] checks [T1, T2] ~> Array<Y>[U1, U2]

*)
and array_flow cx trace lit1 arr1 r1 tup2 ?(index=0) = function
  (* empty array / array literal / tuple flowing to array / array literal /
     tuple (includes several cases, analyzed below) *)
  | [], e1, ts2, e2 ->
    (* if upper bound is a tuple *)
    if tup2 then
      (* if lower bound is an empty array *)
      if arr1 then
        (* We allow extraction of elements at arbitrary positions from an array
           already (even if the array could have undefined in those positions),
           so here we just ensure that the extracted elements satisfy each
           position of the tuple *)
        List.iter (fun t2 ->
          rec_flow_t cx trace (e1, t2)
        ) ts2
      (* otherwise, lower bound is an empty array literal / tuple *)
      else
        (* We flow undefined to each position of the tuple *)
        List.iteri (fun i t2 ->
          rec_flow cx trace
            (VoidT r1,
             UseT (MissingTupleElement (index+i+1), t2))
        ) ts2
    else
      (* if lower bound is an empty array / array literal *)
      if index = 0 then
        (* general element1 = general element2 *)
        flow_to_mutable_child cx trace lit1 e1 e2
      (* otherwise, lower bound is an empty tuple (nothing to do) *)

  (* non-empty array literal / tuple ~> empty array / array literal / tuple *)
  | _, e1, [], e2 ->
    (* general element1 < general element2 *)
    rec_flow_t cx trace (e1, e2)

  (* non-empty array literal / tuple ~> non-empty array literal / tuple *)
  | t1 :: ts1, e1, t2 :: ts2, e2 ->
    (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace lit1 t1 t2;
    array_flow cx trace lit1 arr1 r1 tup2 ~index:(index+1) (ts1,e1, ts2,e2)

(* TODO: either ensure that array_unify is the same as array_flow both ways, or
   document why not. *)
(* array helper *)
and array_unify cx trace = function
  | [], e1, [], e2 ->
    (* general element1 = general element2 *)
    rec_unify cx trace e1 e2

  | ts1, _, [], e2
  | [], e2, ts1, _ ->
    (* specific element1 = general element2 *)
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
        let reason = replace_reason_const RTooFewArgsExpectedRest reason_op in
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
      let u = UseT (FunCallParam, tout) in
      match desc_of_t tin with
      | RTypeParam _ -> u
      | _ -> ReposLowerT (reason_of_t tin, u)
    in
    rec_flow cx trace (tin, tout);
    multiflow_partial cx trace ?strict (tins,touts)

and perform_lookup_action cx trace propref p lreason ureason = function
  | LookupProp up ->
    rec_flow_p cx trace lreason ureason propref (p, up)
  | SuperProp lp ->
    rec_flow_p cx trace ureason lreason propref (lp, p)
  | RWProp (tout, rw) ->
    match rw, Property.access rw p with
    | Read, Some t -> rec_flow_t cx trace (t, tout)
    | Write, Some t -> rec_flow_t cx trace (tout, t)
    | _, None ->
      let x = match propref with Named (_, x) -> Some x | Computed _ -> None in
      add_output cx trace
        (FlowError.EPropAccess ((lreason, ureason), x, p, rw))

and perform_elem_action cx trace value = function
  | ReadElem t -> rec_flow_t cx trace (value, t)
  | WriteElem t -> rec_flow_t cx trace (t, value)
  | CallElem (reason_call, ft) ->
    rec_flow cx trace (value, CallT (reason_call, ft))

and string_key s reason =
  let key_reason = replace_reason_const (RPropertyIsAString s) reason in
  StrT (key_reason, Literal s)

(* builtins, contd. *)

and get_builtin cx ?trace x reason =
  mk_tvar_where cx reason (fun builtin ->
    let propref = Named (reason, x) in
    flow_opt cx ?trace (builtins cx, GetPropT (reason, propref, builtin))
  )

and lookup_builtin cx ?trace x reason strict builtin =
  let propref = Named (reason, x) in
  flow_opt cx ?trace (builtins cx,
    LookupT (reason, strict, [], propref, RWProp (builtin, Read)))

and get_builtin_typeapp cx ?trace reason x ts =
  TypeAppT(get_builtin cx ?trace x reason, ts)

(* Specialize a polymorphic class, make an instance of the specialized class. *)
and mk_typeapp_instance cx ?trace ~reason_op ~reason_tapp ?(cache=false) c ts =
  let t = mk_tvar cx reason_op in
  flow_opt cx ?trace (c, SpecializeT(reason_op,reason_tapp,cache,ts,t));
  mk_instance cx ?trace (reason_of_t c) t

(* NOTE: the for_type flag is true when expecting a type (e.g., when processing
   an annotation), and false when expecting a runtime value (e.g., when
   processing an extends). *)
and mk_instance cx ?trace instance_reason ?(for_type=true) c =
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
      flow_opt_t cx ?trace (c, TypeT(instance_reason,t))
    ) in
    (* TODO: Actually, sink_t is redundant now with ReposUseT, so we can remove
       it. Basically ReposUseT takes care of capturing lower bounds and flowing
       them to the resolved annotation when it is ready. *)
    let sink_t = source_t in
    AnnotT (sink_t, source_t)
  else
    mk_tvar_derivable_where cx instance_reason (fun t ->
      flow_opt_t cx ?trace (c, ClassT(t))
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
    reposition cx ?trace r t

(* set the position of the given def type from a reason *)
and reposition cx ?trace reason t =
  let rec recurse seen = function
  | OpenT (r, id) as t ->
    let constraints = find_graph cx id in
    begin match constraints with
    | Resolved t ->
      (* A tvar may be resolved to a type that has special repositioning logic,
         like UnionT. We want to recurse to pick up that logic, but must be
         careful as the union may refer back to the tvar itself, causing a loop.
         To break the loop, we pass down a map of "already seen" tvars. *)
      (match IMap.get id seen with
      | Some t -> t
      | None ->
        (* Create a fresh tvar which can be passed in `seen` *)
        let mk_tvar_where = if is_derivable_reason r
          then mk_tvar_derivable_where
          else mk_tvar_where
        in
        mk_tvar_where cx reason (fun tvar ->
          let t' = recurse (IMap.add id tvar seen) t in
          (* All `t` in `Resolved t` are concrete. Because `t` is a concrete
             type, `t'` is also necessarily concrete (i.e., reposition preserves
             open -> open, concrete -> concrete). The unification below thus
             results in resolving `tvar` to `t'`, so we end up with a resolved
             tvar whenever we started with one. *)
          unify_opt cx ?trace tvar t';
        ))
    | _ ->
      (* Try to re-use an already created repositioning tvar.
         See repos_cache.ml for details. *)
      match Repos_cache.find id reason !Cache.repos_cache with
      | Some t -> t
      | None ->
        let mk_tvar_where = if is_derivable_reason r
          then mk_tvar_derivable_where
          else mk_tvar_where
        in
        mk_tvar_where cx reason (fun tvar ->
          Cache.(repos_cache := Repos_cache.add reason t tvar !repos_cache);
          flow_opt cx ?trace (t, ReposLowerT (reason, UseT (UnknownUse, tvar)))
        )
    end
  | EvalT _ as t ->
      (* Modifying the reason of `EvalT`, as we do for other types, is not
         enough, since it will only affect the reason of the resulting tvar.
         Instead, repositioning a `EvalT` should simulate repositioning the
         resulting tvar, i.e., flowing repositioned *lower bounds* to the
         resulting tvar. (Another way of thinking about this is that a `EvalT`
         is just as transparent as its resulting tvar.) *)
      mk_tvar_where cx reason (fun tvar ->
        flow_opt cx ?trace (t, ReposLowerT (reason, UseT (UnknownUse, tvar)))
      )
  | MaybeT t ->
      MaybeT (recurse seen t)
  | OptionalT t ->
      OptionalT (recurse seen t)
  | UnionT (r, rep) ->
      let r = repos_reason (loc_of_reason reason) r in
      let rep = UnionRep.map (recurse seen) rep in
      UnionT (r, rep)
  | t ->
      mod_reason_of_t (repos_reason (loc_of_reason reason)) t
  in
  recurse IMap.empty t

(* given the type of a value v, return the type term
   representing the `typeof v` annotation expression *)
and mk_typeof_annotation cx ?trace reason t =
  become cx ?trace reason t

and get_builtin_type cx ?trace reason x =
  let t = get_builtin cx ?trace x reason in
  mk_instance cx ?trace reason t

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
and resolve_builtin_class cx ?trace = function
  | BoolT (reason, _) ->
    let bool_t = get_builtin_type cx ?trace reason "Boolean" in
    resolve_type cx bool_t
  | NumT (reason, _) ->
    let num_t = get_builtin_type cx ?trace reason "Number" in
    resolve_type cx num_t
  | StrT (reason, _) ->
    let string_t = get_builtin_type cx ?trace reason "String" in
    resolve_type cx string_t
  | ArrT (reason, t, _) ->
    let array_t = get_builtin cx ?trace "Array" reason in
    let array_t = resolve_type cx array_t in
    let array_t = instantiate_poly_t cx array_t [t] in
    instantiate_type array_t
  | t ->
    t

and set_builtin cx ?trace x t =
  let reason = builtin_reason (RCustom x) in
  let propref = Named (reason, x) in
  flow_opt cx ?trace (builtins cx, SetPropT (reason, propref, t))

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
  rec_flow cx trace (t1, UseT (UnknownUse, t2))

and rec_flow_p cx trace ?(use_op=UnknownUse) lreason ureason propref = function
  (* unification cases *)
  | Field (lt, Neutral),
    Field (ut, Neutral) ->
    rec_unify cx trace lt ut
  (* directional cases *)
  | lp, up ->
    let x = match propref with Named (_, x) -> Some x | Computed _ -> None in
    (match Property.read_t lp, Property.read_t up with
    | Some lt, Some ut ->
      rec_flow cx trace (lt, UseT (use_op, ut))
    | None, Some _ ->
      add_output cx trace (FlowError.EPropPolarityMismatch (
        (lreason, ureason), x,
        (Property.polarity lp, Property.polarity up)))
    | _ -> ());
    (match Property.write_t lp, Property.write_t up with
    | Some lt, Some ut ->
      rec_flow cx trace (ut, UseT (use_op, lt))
    | None, Some _ ->
      add_output cx trace (FlowError.EPropPolarityMismatch (
        (lreason, ureason), x,
        (Property.polarity lp, Property.polarity up)))
    | _ -> ())

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
  flow_opt cx ?trace (t1, UseT (UnknownUse, t2))

(* Externally visible function for subtyping. *)
(* Calls internal entry point and traps runaway recursion. *)
and flow cx (lower, upper) =
  try
    flow_opt cx (lower, upper)
  with
  | RecursionCheck.LimitExceeded trace ->
    (* log and continue *)
    let reasons = FlowError.ordered_reasons lower upper in
    add_output cx trace (FlowError.ERecursionLimit reasons)
  | ex ->
    (* rethrow *)
    raise ex

and flow_t cx (t1, t2) =
  flow cx (t1, UseT (UnknownUse, t2))

and tvar_with_constraint cx ?trace ?(derivable=false) u =
  let reason = reason_of_use_t u in
  let mk_tvar_where =
    if derivable
    then mk_tvar_derivable_where
    else mk_tvar_where
  in
  mk_tvar_where cx reason (fun tvar ->
    flow_opt cx ?trace (tvar, u)
  )

(* Wrapper functions around __unify that manage traces. Use these functions for
   all recursive calls in the implementation of __unify. *)

and rec_unify cx trace t1 t2 =
  let max = Context.max_trace_depth cx in
  __unify cx t1 t2 (Trace.rec_trace ~max t1 (UseT (UnknownUse, t2)) trace)

and unify_opt cx ?trace t1 t2 =
  let trace = match trace with
  | None -> Trace.unit_trace t1 (UseT (UnknownUse, t2))
  | Some trace ->
    let max = Context.max_trace_depth cx in
    Trace.rec_trace ~max t1 (UseT (UnknownUse, t2)) trace
  in
  __unify cx t1 t2 trace

(* Externally visible function for unification. *)
(* Calls internal entry point and traps runaway recursion. *)
and unify cx t1 t2 =
  try
    unify_opt cx t1 t2
  with
  | RecursionCheck.LimitExceeded trace ->
    (* log and continue *)
    let reasons = FlowError.ordered_reasons t1 (UseT (UnknownUse, t2)) in
    add_output cx trace (FlowError.ERecursionLimit reasons)
  | ex ->
    (* rethrow *)
    raise ex

and continue cx trace t = function
  | Upper u -> rec_flow cx trace (t, u)
  | Lower l -> rec_flow_t cx trace (l, t)

(************* end of slab **************************************************)

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
    | SuccessModule of Type.t SMap.t * (Type.t option)
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  val command_result_of_member_result: member_result ->
    (Type.t SMap.t, string) ok_or_err

  val extract_members: Context.t -> Type.t -> member_result

end = struct

  type member_result =
    | Success of Type.t SMap.t
    | SuccessModule of Type.t SMap.t * (Type.t option)
    | FailureMaybeType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  let command_result_of_member_result = function
    | Success map
    | SuccessModule (map, None) ->
        OK map
    | SuccessModule (named_exports, Some cjs_export) ->
        OK (SMap.add "default" cjs_export named_exports)
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
    ) (Context.find_props cx fields)

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
        let rep = InterRep.make
          (get_builtin_type cx reason "Function")
          (get_builtin_type cx reason "Object")
          []
        in
        extract_members cx (IntersectionT (reason, rep))
    | AnnotT (source, _) ->
        let source_t = resolve_type cx source in
        extract_members cx source_t
    | InstanceT (_, _, super,
                {fields_tmap = fields;
                methods_tmap = methods;
                _}) ->
        let members = SMap.fold (fun x p acc ->
          (* TODO: It isn't currently possible to return two types for a given
           * property in autocomplete, so for now we just return the getter
           * type. *)
          let t = match p with
          | Field (t, _) | Get t | Set t | GetSet (t, _) -> t
          in
          SMap.add x t acc
        ) (find_props cx fields) SMap.empty in
        let members = SMap.fold (fun x p acc ->
          match Property.read_t p with
          | Some t -> SMap.add x t acc
          | None -> acc
        ) (find_props cx methods) members in
        let super_t = resolve_type cx super in
        let super_flds = extract_members_as_map cx super_t in
        Success (AugmentableSMap.augment super_flds ~with_bindings:members)
    | ObjT (_, {props_tmap = flds; proto_t = proto; _}) ->
        let proto_reason = reason_of_t proto in
        let rep = InterRep.make
          proto
          (get_builtin_type cx proto_reason "Object")
          []
        in
        let proto_t = resolve_type cx (IntersectionT (proto_reason, rep)) in
        let prot_members = extract_members_as_map cx proto_t in
        let members = SMap.fold (fun x p acc ->
          match Property.read_t p with
          | Some t -> SMap.add x t acc
          | None -> acc
        ) (find_props cx flds) SMap.empty in
        Success (AugmentableSMap.augment prot_members ~with_bindings:members)
    | ModuleT (_, {exports_tmap; cjs_export; has_every_named_export = _;}) ->
        let named_exports = Context.find_exports cx exports_tmap in
        let cjs_export =
          match cjs_export with
          | Some t -> Some (resolve_type cx t)
          | None -> None
        in
        SuccessModule (named_exports, cjs_export)
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
        Success (AugmentableSMap.augment prot_members ~with_bindings:members)
    | IntersectionT (_, rep) ->
        (* Intersection type should autocomplete for every property of
           every type in the intersection *)
        let ts = InterRep.members rep in
        let ts = List.map (resolve_type cx) ts in
        let members = List.map (extract_members_as_map cx) ts in
        Success (List.fold_left (fun acc members ->
          AugmentableSMap.augment acc ~with_bindings:members
        ) SMap.empty members)
    | UnionT (_, rep) ->
        (* Union type should autocomplete for only the properties that are in
        * every type in the intersection *)
        let ts = List.map (resolve_type cx) (UnionRep.members rep) in
        let members = ts
          (* Although we'll ignore the any-ish members of the union *)
          |> List.filter (function
             | AnyT _ | AnyObjT _ | AnyFunT _ -> false
             | _ -> true
             )
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

    | AbstractT _
    | AnyWithLowerBoundT _
    | AnyWithUpperBoundT _
    | ArrT (_, _, _)
    | BoundT _
    | ChoiceKitT (_, _)
    | ClassT _
    | CustomFunT (_, _)
    | DiffT (_, _)
    | EmptyT _
    | EvalT (_, _, _)
    | ExactT (_, _)
    | ExistsT _
    | ExtendsT (_, _, _)
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _
    | FunProtoT _
    | IdxWrapper (_, _)
    | KeysT (_, _)
    | MixedT _
    | ObjProtoT _
    | OpenPredT (_, _, _, _)
    | OpenT _
    | OptionalT _
    | RestT _
    | ShapeT _
    | TaintT _
    | ThisClassT _
    | TypeMapT (_, _, _, _)
    | TypeT (_, _)
      ->
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
let rec assert_ground ?(infer=false) ?(depth=1) cx skip ids t =
  begin match Context.verbose cx with
  | Some { Verbose.depth = verbose_depth; indent; } ->
    let pid = Context.pid_prefix cx in
    let indent = String.make ((depth - 1) * indent) ' ' in
    prerr_endlinef "\n%s%sassert_ground: %s"
      indent pid (Debug_js.dump_t cx ~depth:verbose_depth t)
  | None -> ()
  end;
  let recurse ?infer = assert_ground ?infer ~depth:(depth + 1) cx skip ids in
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
    assert_ground_id cx ~depth:(depth + 1) skip ids id

  | OpenT (reason_open, id) ->
    unify_opt cx (OpenT (reason_open, id)) AnyT.t;
    FlowError.(add_output cx (EMissingAnnotation reason_open))

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

  | FunT (reason, static, prototype, { this_t; params_tlist; return_t; _ }) ->
    unify_opt cx static AnyT.t;
    unify_opt cx prototype AnyT.t;
    unify_opt cx this_t AnyT.t;
    List.iter (recurse ~infer:(is_derivable_reason reason)) params_tlist;
    recurse ~infer:true return_t

  | PolyT (_, t)
  | ThisClassT t ->
    recurse t

  | ObjT (_, { props_tmap = id; proto_t; _ }) ->
    unify_opt cx proto_t AnyT.t;
    Context.iter_props cx id (fun _ -> Property.iter_t (recurse ~infer:true))

  | IdxWrapper (_, obj) -> recurse ~infer obj

  | ArrT (_, t, ts) ->
    recurse ~infer:true t;
    List.iter (recurse ~infer:true) ts

  | ClassT t
  | TypeT (_, t) ->
    recurse t

  | InstanceT (_, static, super, instance) ->
    let process_element ?(is_field=false) name t =
      let munged = is_munged_prop_name cx name in
      let initialized = SSet.mem name instance.initialized_field_names in
      let infer = munged || is_field && initialized in

      let t =
        if munged && (not is_field || initialized)
        then mod_reason_of_t (fun r -> derivable_reason r) t
        else t
      in

      recurse ~infer t
    in
    Context.iter_props cx instance.fields_tmap
      (fun x -> Property.iter_t (process_element ~is_field:true x));
    Context.iter_props cx instance.methods_tmap
      (fun x -> Property.iter_t (process_element x));
    unify_opt cx static AnyT.t;
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

  | ExactT (_, t)
  | MaybeT t ->
    recurse t

  | IntersectionT (_, rep) ->
    List.iter recurse (InterRep.members rep)

  | UnionT (_, rep) ->
    List.iter (recurse ~infer:true) (UnionRep.members rep)

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

  | ModuleT (_, { exports_tmap; cjs_export; has_every_named_export=_; }) ->
    Context.find_exports cx exports_tmap
      |> SMap.iter (fun _ -> recurse ~infer:true);
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

  | TypeMapT (_, _, t1, t2) ->
    recurse t1;
    recurse t2

  | ObjProtoT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | AbstractT _
  | EvalT _
  | ExtendsT _
  | ChoiceKitT _
  | CustomFunT _
  | OpenPredT _
  ->
    () (* TODO *)

and assert_ground_id cx ?(depth=1) skip ids id =
  if not (ISet.mem id !ids)
  then (
    ids := !ids |> ISet.add id;
    match find_graph cx id with
    | Unresolved { lower; _ } ->
        TypeMap.keys lower |> List.iter (assert_ground cx ~depth skip ids);

        (* note: previously we were also recursing into lowertvars as follows:

        IMap.keys lowertvars |> List.iter (assert_ground_id cx skip ids);

         ...but this simply retraverses concrete lower bounds already
         collected in `lower`, without checking the ids of the lowertvars
         themselves. Correct behavior may require that those be checked via
         assert_ground, but for now we just avoid the redundant traversals.
        *)
    | Resolved t ->
        assert_ground cx ~depth skip ids t
  )

let enforce_strict cx id =
  (* First, compute a set of ids to be skipped by calling `assume_ground`. After
     the call, skip_ids contains precisely those ids that correspond to
     requires/imports. *)
  let skip_ids = ref ISet.empty in
  SSet.iter (fun r ->
    let tvar = lookup_module cx r in
    assume_ground cx skip_ids (UseT (UnknownUse, tvar))
  ) (Context.required cx);

  (* With the computed skip_ids, call `assert_ground` to force annotations while
     walking the graph starting from id. Typically, id corresponds to
     exports. *)
  assert_ground_id cx !skip_ids (ref ISet.empty) id

(* Would rather this live elsewhere, but here because module DAG. *)
let mk_default cx reason ~expr = Default.fold
  ~expr:(expr cx)
  ~cons:(fun t1 t2 ->
    mk_tvar_where cx reason (fun tvar ->
      flow_t cx (t1, tvar);
      flow_t cx (t2, tvar)))
  ~selector:(fun r t sel ->
    let id = mk_id () in
    eval_selector cx r t sel id)
