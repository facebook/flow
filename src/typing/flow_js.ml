(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

(* type exemplar set - reasons are not considered in compare *)
module TypeExSet = Set.Make(struct
  include Type
  let compare = reasonless_compare
end)

let matching_sentinel_prop reason key sentinel_value =
  MatchingPropT (reason, key, DefT (reason, sentinel_value))

(**************************************************************)

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
  Tvar.mk and builtins services. If the flow algorithm can
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
      let func_env = IMap.get func_frame (Context.envs cx) in
      let call_env = IMap.get call_frame (Context.envs cx) in
      Option.iter (Option.both func_env call_env) ~f:(fun (func_env, call_env) ->
        overlapped_call_scopes func_env call_env |>
          List.iter (fun ({ id; _ } as scope) ->
            Changeset.include_scopes [id] changeset |>
              Changeset.iter_writes
                (havoc_entry cx scope)
                (havoc_refi cx scope)
        )
      );
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
let possible_types cx id = types_of (Context.find_graph cx id)
  |> List.filter is_proper_def

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let uses_of constraints =
  match constraints with
  | Unresolved { upper; _ } -> UseTypeMap.keys upper
  | Resolved t -> [UseT (unknown_use, t)]

let possible_uses cx id = uses_of (Context.find_graph cx id)

let rec list_map2 f ts1 ts2 = match (ts1,ts2) with
  | ([],_) | (_,[]) -> []
  | (t1::ts1,t2::ts2) -> (f (t1,t2)):: (list_map2 f ts1 ts2)

let rec merge_type cx =
  let create_union rep =
    DefT (locationless_reason (RCustom "union"), UnionT rep)
  in

  let create_intersection rep =
    DefT (locationless_reason (RCustom "intersection"), IntersectionT rep)
  in

  function
  | DefT (_, NumT _), (DefT (_, NumT _) as t)
  | DefT (_, StrT _), (DefT (_, StrT _) as t)
  | DefT (_, BoolT _), (DefT (_, BoolT _) as t)
  | DefT (_, NullT), (DefT (_, NullT) as t)
  | DefT (_, VoidT), (DefT (_, VoidT) as t)
  | DefT (_, AnyObjT), (DefT (_, AnyObjT) as t)
    -> t

  | (ObjProtoT _, (ObjProtoT _ as t))
     -> t

  | DefT (_, AnyT), t | t, DefT (_, AnyT) -> t

  | DefT (_, EmptyT), t | t, DefT (_, EmptyT) -> t
  | _, (DefT (_, MixedT _) as t) | (DefT (_, MixedT _) as t), _ -> t

  | DefT (_, NullT), (DefT (_, MaybeT _) as t) | (DefT (_, MaybeT _) as t), DefT (_, NullT)
  | DefT (_, VoidT), (DefT (_, MaybeT _) as t) | (DefT (_, MaybeT _) as t), DefT (_, VoidT)
    -> t

  | (DefT (_, FunT (_,_,ft1)) as fun1), (DefT (_, FunT (_,_,ft2)) as fun2) ->
      (* Functions with different number of parameters cannot be merged into a
       * single function type. Instead, we should turn them into a union *)
      let params =
        if List.length ft1.params <> List.length ft2.params then None else
        let params = List.map2 (fun (name1, t1) (name2, t2) ->
          (* TODO: How to merge param names? *)
          let name = match name1, name2 with
          | None, None -> None
          | Some name, _
          | _, Some name -> Some name
          in
          name, merge_type cx (t1, t2)
        ) ft1.params ft2.params in
        match ft1.rest_param, ft2.rest_param with
        | None, Some _
        | Some _, None -> None
        | None, None -> Some (params, None)
        | Some r1, Some r2 -> Some (params, Some (r1, r2))
      in
      begin match params with
      | None -> create_union (UnionRep.make fun1 fun2 [])
      | Some (params, rest_params) ->
          let params_names, tins = List.split params in
          let rest_param = match rest_params with
          | None -> None
          | Some ((name1, loc, rest_t1), (name2, _, rest_t2)) ->
            (* TODO: How to merge rest names and locs? *)
            let name = match name1, name2 with
            | None, None -> None
            | Some name, _
            | _, Some name -> Some name in
            Some (name, loc, merge_type cx (rest_t1, rest_t2))
          in
          let tout = merge_type cx (ft1.return_t, ft2.return_t) in
          let reason = locationless_reason (RCustom "function") in
          DefT (reason, FunT (
            dummy_static reason,
            dummy_prototype,
            mk_functiontype reason tins tout ~rest_param
              ~def_reason:reason ~params_names
          ))
      end

  | (DefT (_, ObjT o1) as t1), (DefT (_, ObjT o2) as t2) ->
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
      | Some (Field (_, _, Positive)), Some (Field (_, _, Positive)) -> Some true
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

    let merge_call = match o1.call_t, o2.call_t with
    | None, None -> Some None
    | Some _, None -> if o2.flags.exact then Some o1.call_t else None
    | None, Some _ -> if o1.flags.exact then Some o2.call_t else None
    | Some c1, Some c2 -> Some (Some (create_union (UnionRep.make c1 c2 [])))
    in

    (* Only merge objects if every property can be merged. *)
    let should_merge = SMap.for_all (fun _ x -> x) merge_map in

    (* Don't merge objects with different prototypes. *)
    let should_merge = should_merge && o1.proto_t = o2.proto_t in

    (match should_merge, merge_dict, merge_call with
    | true, Some dict, Some call ->
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
      let objtype = mk_objecttype ~flags ~dict ~call id o1.proto_t in
      DefT (locationless_reason (RCustom "object"), ObjT objtype)
    | _ ->
      create_union (UnionRep.make t1 t2 []))

  | DefT (_, ArrT (ArrayAT (t1, ts1))),
    DefT (_, ArrT (ArrayAT (t2, ts2))) ->
    let tuple_types = match ts1, ts2 with
      | None, _
      | _, None -> None
      | Some ts1, Some ts2 -> Some (list_map2 (merge_type cx) ts1 ts2) in

    DefT (locationless_reason (RCustom "array"),
      ArrT (ArrayAT( merge_type cx (t1, t2), tuple_types))
    )

  | DefT (_, ArrT (TupleAT (t1, ts1))),
    DefT (_, ArrT (TupleAT(t2, ts2))) when List.length ts1 = List.length ts2 ->

    DefT (locationless_reason (RCustom "tuple"),
      ArrT (TupleAT (merge_type cx (t1, t2), list_map2 (merge_type cx) ts1 ts2))
    )

  | DefT (_, ArrT (ROArrayAT elemt1)),
    DefT (_, ArrT (ROArrayAT elemt2)) ->

    DefT (locationless_reason (RCustom "read only array"),
      ArrT (ROArrayAT (merge_type cx (elemt1, elemt2)))
    )

  | DefT (_, ArrT EmptyAT),
    DefT (_, ArrT EmptyAT) ->

    DefT (locationless_reason (RCustom "empty array"), ArrT EmptyAT)

  | (DefT (_, MaybeT t1), DefT (_, MaybeT t2))
  | (DefT (_, MaybeT t1), t2)
  | (t1, DefT (_, MaybeT t2)) ->
      let t = merge_type cx (t1, t2) in
      let reason = locationless_reason (RMaybe (desc_of_t t)) in
      DefT (reason, MaybeT t)

  | DefT (_, UnionT rep1), DefT (_, UnionT rep2) ->
      create_union (UnionRep.rev_append rep1 rep2)

  | (DefT (_, UnionT rep), t)
  | (t, DefT (_, UnionT rep)) ->
      create_union (UnionRep.cons t rep)

  (* TODO: do we need to do anything special for merging Null with Void,
     Optional with other types, etc.? *)

  | (t1, t2) ->
      create_union (UnionRep.make t1 t2 [])

and resolve_type cx = function
  | OpenT tvar -> resolve_tvar cx tvar
  | t -> t

and resolve_tvar cx (_, id) =
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
  ) Locationless.AnyT.t ts

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

  | ReposLowerT (_, _, use_t) ->
    assume_ground cx ~depth:(depth + 1) ids use_t

  | ImportModuleNsT (_, t, _)
  | CJSRequireT (_, t, _)
  | ImportTypeT (_, _, t)
  | ImportTypeofT (_, _, t)

  (** Other common operations that might happen immediately after extracting
      (parts of) requires/imports. *)

  | GetPropT (_, _, _, t)
  | CallT (_, _, { call_tout = t; _ })
  | MethodT (_, _, _, _, { call_tout = t; _ }, _)
  | ConstructorT (_, _, _, _, t) ->
    assume_ground cx ~depth:(depth + 1) ids (UseT (unknown_use, t))

  | _ -> ()
  end;
  if Context.is_verbose cx then
    let pid = Context.pid_prefix cx in
    if depth = 1 then
      prerr_endlinef "\n%sAssumed ground: %s"
        pid
        (!ids |> ISet.elements |> List.map string_of_int |> String.concat ", ")

and assume_ground_id cx ~depth ids_ref id =
  let root_id, constraints = Context.find_constraints cx id in
  let ids = !ids_ref in
  let ids' = ISet.add root_id ids in
  if ids' != ids then (
    ids_ref := ids';
    match constraints with
    | Unresolved { upper; uppertvars; _ } ->
      upper |> UseTypeMap.iter (fun t _ ->
        assume_ground cx ~depth ids_ref t
      );
      uppertvars |> IMap.iter (fun id _ ->
        assume_ground_id cx ~depth ids_ref id
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
  let builtins = Tvar.mk cx (builtin_reason (RCustom "module")) in
  Context.add_module cx Files.lib_module_ref builtins

(* Local references to modules can be looked up. *)
let lookup_module cx m = Context.find_module cx m

(* The builtins reference is accessed just like references to other modules. *)
let builtins cx =
  lookup_module cx Files.lib_module_ref

(***********************)
(* instantiation utils *)
(***********************)

module ImplicitTypeArgument = struct
  (* Make a type argument for a given type parameter, given a reason. Note that
     not all type arguments are tvars; the following function is used only when
     polymorphic types need to be implicitly instantiated, because there was no
     explicit instantiation (via a type application), or when we want to cache a
     unique instantiation and unify it with other explicit instantiations. *)
  let mk_targ cx typeparam reason_op =
    (* Create a reason that is positioned at reason_op, but has a def_loc at
     * typeparam.reason. *)
    let loc_op = loc_of_reason reason_op in
    let desc = RTypeParam (typeparam.name, desc_of_reason reason_op, loc_op) in
    let reason = mk_reason desc (def_loc_of_reason typeparam.reason) in
    let reason = repos_reason loc_op reason in
    Tvar.mk cx reason

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
  let roots_collector = object
    inherit [TypeSet.t] Type_visitor.t as super

    method! type_ cx pole acc t = match t with
    | DefT (_, TypeAppT (_, c, _)) -> super#type_ cx pole (TypeSet.add c acc) t
    | OpenT _ -> (match ImplicitTypeArgument.abstract_targ t with
      | None -> acc
      | Some t -> TypeSet.add t acc
      )
    | _ -> super#type_ cx pole acc t
  end

  let collect_roots cx = roots_collector#type_ cx Neutral TypeSet.empty

  (* Util to stringify a list, given a separator string and a function that maps
     elements of the list to strings. Should probably be moved somewhere else
     for general reuse. *)
  let string_of_list list sep f =
    list |> List.map f |> String.concat sep

  let string_of_desc_of_t t = DescFormat.name_of_instance_reason (reason_of_t t)

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

  module FlowSet = struct
    let empty = TypeMap.empty

    let add_not_found l us setr =
      setr := TypeMap.add l us !setr; false

    let cache (l, u) setr =
      match TypeMap.get l !setr with
      | None -> add_not_found l (UseTypeSet.singleton u) setr
      | Some us ->
        (* add returns ref eq set if found *)
        let us' = UseTypeSet.add u us in
        us' == us || add_not_found l us' setr

    let fold f =
      TypeMap.fold (fun l -> UseTypeSet.fold (fun u -> f (l, u)))
  end

  (* Cache that remembers pairs of types that are passed to __flow. *)
  module FlowConstraint = struct
    let cache = ref FlowSet.empty

    (* attempt to read LB/UB pair from cache, add if absent *)
    let get cx (l, u) = match l, u with
      (* Don't cache constraints involving type variables, since the
         corresponding typing rules are already sufficiently robust. *)
      | OpenT _, _ | _, UseT (_, OpenT _) -> false
      | _ ->
        (* Use ops are purely for better error messages: they should have no
           effect on type checking. However, recursively nested use ops can pose
           non-termination problems. To ensure proper caching, we hash use ops
           to just their top-level structure. *)
        let u = mod_use_op_of_use_t (function
        | Frame (frame, use_op) when use_op <> unknown_use -> Frame (frame, unknown_use)
        | Op (Speculation use_op) when use_op <> unknown_use -> Op (Speculation unknown_use)
        | use_op -> use_op) u in
        let found = FlowSet.cache (l, u) cache in
        if found && Context.is_verbose cx then
          prerr_endlinef "%sFlowConstraint cache hit on (%s, %s)"
            (Context.pid_prefix cx)
            (string_of_ctor l) (string_of_use_ctor u);
        found
  end

  (* Cache that maps TypeApp(Poly (...id), ts) to its result. *)
  module Subst = struct
    let cache = Hashtbl.create 0
    let find = Hashtbl.find_opt cache
    let add = Hashtbl.replace cache
  end

  (* Cache that limits instantiation of polymorphic definitions. Intuitively,
     for each operation on a polymorphic definition, we remember the type
     arguments we use to specialize the type parameters. An operation is
     identified by its reason, and possibly the reasons of its arguments. We
     don't use the entire operation for caching since it may contain the very
     type variables we are trying to limit the creation of with the cache (e.g.,
     those representing the result): the cache would be useless if we considered
     those type variables as part of the identity of the operation. *)
  module PolyInstantiation = struct
    type cache_key = Loc.t * reason * op_reason
    and op_reason = reason Nel.t

    let cache: (cache_key, Type.t) Hashtbl.t = Hashtbl.create 0

    let find cx reason_tapp typeparam op_reason =
      let loc = def_loc_of_reason reason_tapp in
      try
        Hashtbl.find cache (loc, typeparam.reason, op_reason)
      with _ ->
        let t = ImplicitTypeArgument.mk_targ cx typeparam (Nel.hd op_reason) in
        Hashtbl.add cache (loc, typeparam.reason, op_reason) t;
        t
  end

  let repos_cache = ref Repos_cache.empty

  module Eval = struct
    type id_cache_key = Type.t * Type.defer_use_t
    type repos_cache_key = Type.t * Type.defer_use_t * int

    let id_cache: (id_cache_key, int) Hashtbl.t = Hashtbl.create 0
    let repos_cache: (repos_cache_key, Type.t) Hashtbl.t = Hashtbl.create 0

    let id t defer_use =
      let cache_key = t, defer_use in
      try
        Hashtbl.find id_cache cache_key
      with _ ->
        let i = mk_id () in
        Hashtbl.add id_cache cache_key i;
        i

    let find_repos t defer_use id =
      let cache_key = t, defer_use, id in
      try Some (Hashtbl.find repos_cache cache_key)
      with _ -> None

    let add_repos t defer_use id tvar =
      let cache_key = t, defer_use, id in
      Hashtbl.add repos_cache cache_key tvar
  end

  module Fix = struct
    type cache_key = reason * Type.t

    let cache: (cache_key, Type.t) Hashtbl.t = Hashtbl.create 0

    let find reason i =
      let cache_key = reason, i in
      try Some (Hashtbl.find cache cache_key)
      with _ -> None

    let add reason i tvar =
      let cache_key = reason, i in
      Hashtbl.add cache cache_key tvar
  end

  let clear () =
    FlowConstraint.cache := FlowSet.empty;
    Hashtbl.clear Subst.cache;
    Hashtbl.clear PolyInstantiation.cache;
    repos_cache := Repos_cache.empty;
    Hashtbl.clear Eval.id_cache;
    Hashtbl.clear Eval.repos_cache;
    Hashtbl.clear Fix.cache;
    ()

  let stats_poly_instantiation () =
    Hashtbl.stats PolyInstantiation.cache

  (* debug util: please don't dead-code-eliminate *)
  (* Summarize flow constraints in cache as ctor/reason pairs, and return counts
     for each group. *)
  let summarize_flow_constraint () =
    let group_counts = FlowSet.fold (fun (l,u) map ->
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
   any) and ignoring shadow properties (if any).

   The first argument to f is a boolean which denotes if the property is a
   sentinel property. *)
let iter_real_props cx id f =
  Context.find_props cx id
  |> SMap.filter (fun x _ -> not (is_internal_name x))
  |> SMap.iter (f ~is_sentinel:false)

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
  | Binding of Type.tvar
  | OpenResolved
  | OpenUnresolved of int option * reason * Constraint.ident

  (* log_unresolved is a mode that determines whether to log unresolved tvars:
     it is None when resolving annotations, and Some speculation_id when
     resolving inferred types. *)
  let rec collect_of_types ?log_unresolved cx reason =
    List.fold_left (collect_of_type ?log_unresolved cx reason)

  and collect_of_type ?log_unresolved cx reason acc = function
    | OpenT tvar ->
      let r, id = tvar in
      if IMap.mem id acc then acc
      else if is_constant_reason r
      (* It is important to consider reads of constant property names as fully
         resolvable, especially since constant property names are often used to
         store literals that serve as tags for disjoint unions. Unfortunately,
         today we cannot distinguish such reads from others, so we rely on a
         common style convention to recognize constant property names. For now
         this hack pays for itself: we do not ask such reads to be annotated
         with the corresponding literal types to decide membership in those
         disjoint unions. *)
      then IMap.add id (Binding tvar) acc
      else begin match Context.find_graph cx id with
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
        else IMap.add id (OpenUnresolved (log_unresolved, r, id)) acc
      end

    | AnnotT (t, _) ->
      begin match t with
      | OpenT ((_, id) as tvar) ->
        if IMap.mem id acc then acc
        else IMap.add id (Binding tvar) acc
      | _ ->
        collect_of_type ?log_unresolved cx reason acc t
      end

    | ThisTypeAppT (_, poly_t, _, targs_opt) ->
      let targs = match targs_opt with | None -> [] | Some targs -> targs in
      begin match poly_t with
      | OpenT tvar ->
        let _, id = tvar in
        if IMap.mem id acc then
          collect_of_types ?log_unresolved cx reason acc targs
        else begin
          let acc = IMap.add id (Binding tvar) acc in
          collect_of_types ?log_unresolved cx reason acc targs
        end

      | _ ->
        let ts = poly_t::targs in
        collect_of_types ?log_unresolved cx reason acc ts
      end

    | DefT (_, TypeAppT (_, poly_t, targs))
      ->
      begin match poly_t with
      | OpenT tvar ->
        let _, id = tvar in
        if IMap.mem id acc then
          collect_of_types ?log_unresolved cx reason acc targs
        else begin
          let acc = IMap.add id (Binding tvar) acc in
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

    | DefT (_, ObjT { props_tmap; dict_t; call_t; _ }) ->
      let props_tmap = Context.find_props cx props_tmap in
      let ts = SMap.fold (fun x p ts ->
        (* avoid resolving types of shadow properties *)
        if is_internal_name x then ts
        else Property.fold_t (fun ts t -> t::ts) ts p
      ) props_tmap [] in
      let ts = match dict_t with
      | None -> ts
      | Some { key; value; _ } -> key::value::ts
      in
      let ts = match call_t with
      | None -> ts
      | Some t -> t::ts
      in
      collect_of_types ?log_unresolved cx reason acc ts
    | DefT (_, FunT (_, _, { params; return_t; _ })) ->
      let ts = List.fold_left (fun acc (_, t) -> t::acc) [return_t] params in
      collect_of_types ?log_unresolved cx reason acc ts
    | DefT (_, ArrT (ArrayAT (elemt, tuple_types))) ->
      let ts = Option.value ~default:[] tuple_types in
      let ts = elemt::ts in
      collect_of_types ?log_unresolved cx reason acc ts
    | DefT (_, ArrT (TupleAT (elemt, tuple_types))) ->
      collect_of_types ?log_unresolved cx reason acc (elemt::tuple_types)
    | DefT (_, ArrT (ROArrayAT (elemt))) ->
      collect_of_type ?log_unresolved cx reason acc elemt
    | DefT (_, ArrT EmptyAT) -> acc
    | DefT (_, InstanceT (static, super, _,
        { class_id; type_args; fields_tmap; methods_tmap; inst_call_t; _ })) ->
      let ts = if class_id = 0 then [] else [super; static] in
      let ts = SMap.fold (fun _ (_, t) ts -> t::ts) type_args ts in
      let props_tmap = SMap.union
        (Context.find_props cx fields_tmap)
        (Context.find_props cx methods_tmap)
      in
      let ts = SMap.fold (fun _ p ts ->
        Property.fold_t (fun ts t -> t::ts) ts p
      ) props_tmap ts in
      let ts = match inst_call_t with
      | None -> ts
      | Some t -> t::ts
      in
      collect_of_types ?log_unresolved cx reason acc ts
    | DefT (_, PolyT (_, t, _)) ->
      collect_of_type ?log_unresolved cx reason acc t
    | BoundT _ ->
      acc

    | EvalT (_, TypeDestructorT _, id) ->
      (match IMap.get id (Context.evaluated cx) with
      | Some (OpenT ((_, id) as tvar)) ->
        IMap.add id (Binding tvar) acc
      | _ -> acc)

    (* TODO: The following kinds of types are not walked out of laziness. It's
       not immediately clear what we'd gain (or lose) by walking them. *)

    | EvalT _
    | InternalT (ChoiceKitT (_, _))
    | TypeDestructorTriggerT _
    | ModuleT (_, _, _)
    | InternalT (ExtendsT _)
      ->
      acc

    (* The following cases exactly follow Type_visitor (i.e., they do the
       standard walk). TODO: Rewriting this walker as a subclass of Type_visitor
       would be quite nice (as long as we confirm that the resulting
       virtualization of calls to this function doesn't lead to perf
       degradation: this function is expected to be quite hot). *)

    | DefT (_, OptionalT t) | DefT (_, MaybeT t) ->
      collect_of_type ?log_unresolved cx reason acc t
    | DefT (_, UnionT rep) ->
      let ts = UnionRep.members rep in
      collect_of_types ?log_unresolved cx reason acc ts
    | DefT (_, IntersectionT rep) ->
      let ts = InterRep.members rep in
      collect_of_types ?log_unresolved cx reason acc ts

    | OpaqueT (_, {underlying_t; super_t; _}) ->
      let acc = Option.fold underlying_t ~init:acc ~f:(collect_of_type ?log_unresolved cx reason) in
      let acc = Option.fold super_t ~init:acc ~f:(collect_of_type ?log_unresolved cx reason) in
      acc

    | AnyWithUpperBoundT t
    | AnyWithLowerBoundT t
    | ExactT (_, t)
    | DefT (_, TypeT t)
    | DefT (_, ClassT t)
    | ThisClassT (_, t)
      ->
      collect_of_type ?log_unresolved cx reason acc t

    | KeysT (_, t) ->
      collect_of_type ?log_unresolved cx reason acc t

    | ShapeT (t) ->
      collect_of_type ?log_unresolved cx reason acc t

    | MatchingPropT (_, _, t) ->
      collect_of_type ?log_unresolved cx reason acc t

    | InternalT (IdxWrapper (_, t)) ->
      collect_of_type ?log_unresolved cx reason acc t

    | ReposT (_, t)
    | InternalT (ReposUpperT (_, t)) ->
      collect_of_type ?log_unresolved cx reason acc t

    | InternalT (OptionalChainVoidT _) -> acc

    | DefT (_, NumT _)
    | DefT (_, StrT _)
    | DefT (_, BoolT _)
    | DefT (_, VoidT)
    | DefT (_, NullT)
    | DefT (_, EmptyT)
    | DefT (_, MixedT _)
    | DefT (_, SingletonBoolT _)
    | DefT (_, SingletonNumT _)
    | DefT (_, SingletonStrT _)
    | DefT (_, AnyT)
    | DefT (_, AnyObjT)
    | DefT (_, AnyFunT)
    | DefT (_, CharSetT _)
      -> acc

    | MergedT (_, uses) ->
      List.fold_left (collect_of_use ?log_unresolved cx reason) acc uses

    | FunProtoBindT _
    | FunProtoCallT _
    | FunProtoApplyT _
    | FunProtoT _
    | NullProtoT _
    | ObjProtoT _
    | CustomFunT (_, _)

    | ExistsT _
    | OpenPredT _
      ->
      acc

  (* TODO: Support for use types is currently sketchy. Full resolution of use
     types are only needed for choice-making on intersections. We care about
     calls in particular because one of the biggest uses of intersections is
     function overloading. More uses will be added over time. *)
  and collect_of_use ?log_unresolved cx reason acc = function
  | UseT (_, t) ->
    collect_of_type ?log_unresolved cx reason acc t
  | CallT (_, _, fct) ->
    let arg_types =
      List.map (function Arg t | SpreadArg t -> t) fct.call_args_tlist in
    collect_of_types ?log_unresolved cx reason acc (arg_types @ [fct.call_tout])
  | GetPropT (_, _, _, t_out) ->
    collect_of_type ?log_unresolved cx reason acc t_out
  | _ -> acc

end

(*********************************************************************)

exception SpeculativeError of FlowError.error_message

let add_output cx ?trace msg =
  if Speculation.speculating ()
  then
    if (FlowError.is_lint_error msg)
    then ignore @@ Speculation.(defer_action cx (Action.Error msg))
    else begin
      if Context.is_verbose cx then
        prerr_endlinef "\nspeculative_error: %s" (Debug_js.dump_flow_error cx msg);
      raise (SpeculativeError msg)
    end
  else begin
    if Context.is_verbose cx then
      prerr_endlinef "\nadd_output: %s" (Debug_js.dump_flow_error cx msg);

    let trace_reasons = match trace with
    | None -> []
    | Some trace ->
      (* format a trace into list of (reason, desc) pairs used
       downstream for obscure reasons, and then to messages *)
      let max_trace_depth = Context.max_trace_depth cx in
      if max_trace_depth = 0 then [] else
        Trace.reasons_of_trace ~level:max_trace_depth trace
    in

    let error = FlowError.error_of_msg ~trace_reasons ~source_file:(Context.file cx) msg in

    (* catch no-loc errors early, before they get into error map *)
    if Loc.source (Errors.loc_of_error error) = None then
      assert_false (
        spf "add_output: no source for error: %s"
        (Debug_js.dump_flow_error cx msg));

    Context.add_error cx error
  end

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

(* The main problem with constant folding is infinite recursion. Consider a loop
 * that keeps adding 1 to a variable x, which is initialized to 0. If we
 * constant fold x naively, we'll recurse forever, inferring that x has the type
 * (0 | 1 | 2 | 3 | 4 | etc). What we need to do is recognize loops and stop
 * doing constant folding.
 *
 * One solution is for constant-folding-location to keep count of how many times
 * we have seen a reason. Then, when we've seen it multiple times, we can decide
 * to stop doing constant folding.
 *)
module ConstFoldExpansion : sig
  val guard: int -> reason -> (int -> 't) -> 't
end = struct
  let rmaps: int ReasonMap.t IMap.t ref = ref IMap.empty

  let get_rmap id = Option.value ~default:ReasonMap.empty (IMap.get id !rmaps)

  let increment reason rmap =
    match ReasonMap.get reason rmap with
    | None -> 0, ReasonMap.add reason 1 rmap
    | Some count -> count, ReasonMap.add reason (count + 1) rmap

  let decrement reason rmap =
    match ReasonMap.get reason rmap with
    | Some count ->
      if count > 1
      then ReasonMap.add reason (count - 1) rmap
      else ReasonMap.remove reason rmap
    | None -> rmap

  let push id reason =
    let rmap = get_rmap id in
    let old_value, new_reason_map = increment reason rmap in
    rmaps := IMap.add id new_reason_map !rmaps;
    old_value

  let pop id reason =
    let rmap =
      get_rmap id
      |> decrement reason in
    if ReasonMap.is_empty rmap
    then rmaps := IMap.remove id !rmaps
    else rmaps := IMap.add id rmap !rmaps

  let guard id reason f =
    let count = push id reason in
    let ret = f count in
    pop id reason;
    ret
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

let check_nonstrict_import cx trace is_strict imported_is_strict reason =
  if is_strict && (not imported_is_strict) then
    let loc = Reason.loc_of_reason reason in
    let message = FlowError.ENonstrictImport loc in
    add_output cx ~trace message

let print_if_verbose_lazy cx trace
    ?(delim = "")
    ?(indent = 0)
    (lines: string Lazy.t list) =
  match Context.verbose cx with
  | Some { Verbose.indent = num_spaces; _ } ->
    let indent = indent + Trace.trace_depth trace - 1 in
    let prefix = String.make (indent * num_spaces) ' ' in
    let pid = Context.pid_prefix cx in
    let add_prefix line = spf "\n%s%s%s" prefix pid (Lazy.force line) in
    let lines = List.map add_prefix lines in
    prerr_endline (String.concat delim lines)
  | None ->
    ()

let print_if_verbose cx trace ?(delim = "") ?(indent = 0) (lines: string list) =
  match Context.verbose cx with
  | Some _ ->
    let lines = List.map (fun line -> lazy line) lines in
    print_if_verbose_lazy cx trace ~delim ~indent lines
  | None ->
    ()

let print_types_if_verbose cx trace
    ?(note: string option)
    ((l: Type.t), (u: Type.use_t)) =
  let delim = match note with Some x -> spf " ~> %s" x | None -> " ~>" in
  match Context.verbose cx with
  | Some { Verbose.depth; _ } ->
    print_if_verbose cx trace ~delim [
      Debug_js.dump_t ~depth cx l;
      Debug_js.dump_use_t ~depth cx u;
    ]
  | None ->
    ()

(********************** start of slab **********************************)

(** NOTE: Do not call this function directly. Instead, call the wrapper
    functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
    this module, and the function `flow` outside this module. **)
let rec __flow cx ((l: Type.t), (u: Type.use_t)) trace =
  if ground_subtype (l, u) then
    print_types_if_verbose cx trace (l, u)
  else if Cache.FlowConstraint.get cx (l, u) then
    print_types_if_verbose cx trace ~note:"(cached)" (l, u)
  else (
    print_types_if_verbose cx trace (l, u);

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
    if Speculation.(defer_action cx (Action.Flow (l, u))) then
      print_if_verbose cx trace ~indent:1 ["deferred during speculation"]

    else match (l,u) with

    (********)
    (* eval *)
    (********)

    | EvalT (t, TypeDestructorT (use_op', reason, d), id), _ ->
      let _, result = mk_type_destructor cx ~trace use_op' reason t d id in
      rec_flow cx trace (result, u)

    | _, UseT (use_op, EvalT (t, TypeDestructorT (use_op', reason, d), id)) ->
      let slingshot, result = mk_type_destructor cx ~trace use_op' reason t d id in
      if slingshot
        then rec_flow cx trace (result, ReposUseT (reason, false, use_op, l))
        else rec_flow cx trace (l, UseT (use_op, result))

    | EvalT (t, DestructuringT (reason, s), i), _ ->
      rec_flow cx trace (eval_selector cx ~trace reason t s i, u)

    (** NOTE: the rule with EvalT (_, DestructuringT _, _) as upper bound is
        moved below the OpenT rules, so that we can take advantage of the
        caching inherent in those rules (in particular, when OpenT is a lower
        bound). This caching seems necessary to avoid non-termination. There
        could be other, better ways of achieving the same effect. **)

    (******************)
    (* process X ~> Y *)
    (******************)

    | (OpenT(_, tvar1), UseT (use_op, OpenT(_, tvar2))) ->
      let id1, constraints1 = Context.find_constraints cx tvar1 in
      let id2, constraints2 = Context.find_constraints cx tvar2 in

      (match constraints1, constraints2 with
      | Unresolved bounds1, Unresolved bounds2 ->
          if not_linked (id1, bounds1) (id2, bounds2) then (
            add_upper_edges cx trace (id1, bounds1) (id2, bounds2);
            add_lower_edges cx trace (id1, bounds1) (id2, bounds2);
            flows_across cx trace ~use_op bounds1.lower bounds2.upper;
          );

      | Unresolved bounds1, Resolved t2 ->
          edges_and_flows_to_t cx trace (id1, bounds1) (UseT (use_op, t2))

      | Resolved t1, Unresolved bounds2 ->
          edges_and_flows_from_t cx trace ~use_op t1 (id2, bounds2)

      | Resolved t1, Resolved t2 ->
          rec_flow cx trace (t1, UseT (use_op, t2))
      );

    (******************)
    (* process Y ~> U *)
    (******************)

    | (OpenT(r, tvar), t2) ->
      let t2 = match desc_of_reason r with
      | RTypeParam (_, _, loc) ->
        mod_use_op_of_use_t (fun op -> Frame (ImplicitTypeParam loc, op)) t2
      | _ -> t2
      in

      let id1, constraints1 = Context.find_constraints cx tvar in
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
      let id2, constraints2 = Context.find_constraints cx tvar in
      (match constraints2 with
      | Unresolved bounds2 ->
          edges_and_flows_from_t cx trace ~use_op t1 (id2, bounds2)

      | Resolved t2 ->
          rec_flow cx trace (t1, UseT (use_op, t2))
      );

    (*****************)
    (* any with uses *)
    (*****************)

    | _, UseT (_, MergedT (_, uses)) ->
      List.iter (fun u -> rec_flow cx trace (l, u)) uses

    | MergedT _, ReposUseT (reason, use_desc, use_op, l) ->
      let loc = loc_of_reason reason in
      let desc = if use_desc then Some (desc_of_reason reason) else None in
      let u = reposition cx ~trace loc ?desc l in
      rec_flow cx trace (l, UseT (use_op, u))

    | MergedT (reason, _), _ ->
      rec_flow cx trace (EmptyT.why reason, u)

    (****************)
    (* eval, contd. *)
    (****************)

    | _, UseT (use_op, EvalT (t, DestructuringT (reason, s), i)) ->
      rec_flow cx trace (l, UseT (use_op, eval_selector cx ~trace reason t s i))

    (***************************)
    (* type destructor trigger *)
    (***************************)

    (* For evaluating type destructors we add a trigger, TypeDestructorTriggerT,
     * to both sides of a type. When TypeDestructorTriggerT sees a new upper or
     * lower bound we destruct that bound and flow the result in the same
     * direction to some tout type. *)

    (* Don't let two TypeDestructorTriggerTs reach each other or else we quickly
     * run into non-termination scenarios. *)
    | TypeDestructorTriggerT _, UseT (_, TypeDestructorTriggerT _) -> ()

    | l, UseT (_, TypeDestructorTriggerT (use_op', reason, repos, d, tout)) ->
      let l = match repos with
      | None -> l
      | Some (reason, use_desc) -> reposition_reason cx ~trace reason ~use_desc l
      in
      eval_destructor cx ~trace use_op' reason l d tout

    | TypeDestructorTriggerT (use_op', reason, _, d, tout), UseT (use_op, AnnotT (annot_t, use_desc)) ->
      let tout' = Tvar.mk_where cx reason (fun tout' ->
        let repos = Some (reason_of_t annot_t, use_desc) in
        rec_flow cx trace (annot_t, UseT (use_op, TypeDestructorTriggerT (use_op', reason, repos, d, tout')))
      ) in
      rec_flow cx trace (tout', ReposUseT (reason, false, use_op, tout))

    | TypeDestructorTriggerT (use_op', reason, _, d, tout), UseT (use_op, u) ->
      (* With the same "slingshot" trick used by AnnotT, hold the lower bound
       * at bay until result itself gets concretized, and then flow the lower
       * bound to that concrete type. *)
      let t = Tvar.mk_where cx reason (fun t -> eval_destructor cx ~trace use_op' reason u d t) in
      let use_desc = false in
      rec_flow cx trace (t, ReposUseT (reason, use_desc, use_op, tout))

    (* Ignore any non-type uses. The implementation of type destructors operate
     * solely on types and not arbitrary uses. We also don't want to add errors
     * for arbitrary uses that get added to the subject of our trigger in type
     * destruction evaluation.
     *
     * This may be a risky behavior when considering tvars with *only* non-type
     * uses. However, such tvars are rare and often come from non-sensical
     * programs.
     *
     * Type destructors, currently, may only be created as type annotations.
     * This means that the type is either always 0->1, or it is a polymorphic
     * type argument which will be instantiated with an open tvar. Polymorphic
     * type arguments will also always get some type upper bound with the
     * default type being MixedT. We destruct these upper bounds. *)
    | TypeDestructorTriggerT _, _ -> ()

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

    | InternalT (ChoiceKitT (_, Trigger)), ChoiceKitUseT (reason, TryFlow (i, spec)) ->
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

    | DefT (_, UnionT urep), IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      UnionRep.members urep |> List.iter (fun t ->
        rec_flow cx trace (t, u)
      )

    | DefT (lreason, MaybeT t), IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      let lreason = replace_reason_const RNullOrVoid lreason in
      rec_flow cx trace (NullT.make lreason, u);
      rec_flow cx trace (VoidT.make lreason, u);
      rec_flow cx trace (t, u);

    | DefT (r, OptionalT t), IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      rec_flow cx trace (VoidT.why r, u);
      rec_flow cx trace (t, u);

    | AnnotT (source_t, use_desc), IntersectionPreprocessKitT (_, ConcretizeTypes _) ->
      (* TODO: directly derive loc and desc from the reason of tvar *)
      let loc = loc_of_t source_t in
      let desc = if use_desc then Some (desc_of_t source_t) else None in
      rec_flow cx trace (reposition ~trace cx loc ?desc source_t, u)

    | t, IntersectionPreprocessKitT (reason,
        ConcretizeTypes (unresolved, resolved, DefT (r, IntersectionT rep), u)) ->
      prep_try_intersection cx trace reason unresolved (t::resolved) u r rep

    (*****************************)
    (* Refinement type subtyping *)
    (*****************************)

    | _, RefineT (reason, LatentP (fun_t, idx), tvar) ->
      flow cx (fun_t, CallLatentPredT (reason, true, idx, l, tvar))

    (*************)
    (* Debugging *)
    (*************)

    | _, DebugPrintT reason ->
      let str = Debug_js.jstr_of_t cx l in
      add_output cx ~trace (FlowError.EDebugPrint (reason, str))

    | DefT (_, NumT (Literal (_, (n, _)))), DebugSleepT _ ->
      Unix.sleepf n

    (*************************)
    (* repositioning, part 1 *)
    (*************************)

    (* if a ReposT is used as a lower bound, `reposition` can reposition it *)
    | ReposT (reason, l), _ ->
      rec_flow cx trace (reposition_reason cx ~trace reason l, u)

    (* if a ReposT is used as an upper bound, wrap the now-concrete lower bound
       in a `ReposUpperT`, which will repos `u` when `u` becomes concrete. *)
    | _, UseT (use_op, ReposT (reason, u)) ->
      rec_flow cx trace (InternalT (ReposUpperT (reason, l)), UseT (use_op, u))

    | InternalT (ReposUpperT (reason, l)), UseT (use_op, u) ->
      (* since this guarantees that `u` is not an OpenT, it's safe to use
         `reposition` on the upper bound here. *)
      let u = reposition_reason cx ~trace reason u in
      rec_flow cx trace (l, UseT (use_op, u))

    | InternalT (ReposUpperT (_, l)), _ ->
      rec_flow cx trace (l, u)

    (***************)
    (* annotations *)
    (***************)

    (* Special cases where we want to recursively concretize types within the
       lower bound. *)

    | DefT (r, UnionT rep), ReposUseT (reason, use_desc, use_op, l) ->
      let rep = UnionRep.ident_map (annot use_desc) rep in
      let annot_loc = annot_loc_of_reason reason in
      let r = repos_reason (loc_of_reason reason) ?annot_loc r in
      let r =
        if use_desc
        then replace_reason_const (desc_of_reason reason) r
        else r
      in
      rec_flow cx trace (l, UseT (use_op, DefT (r, UnionT rep)))

    | DefT (r, MaybeT u), ReposUseT (reason, use_desc, use_op, l) ->
      let annot_loc = annot_loc_of_reason reason in
      let r = repos_reason (loc_of_reason reason) ?annot_loc r in
      let r =
        if use_desc
        then replace_reason_const (desc_of_reason reason) r
        else r
      in
      rec_flow cx trace (l, UseT (use_op, DefT (r, MaybeT (annot use_desc u))))

    | DefT (r, OptionalT u), ReposUseT (reason, use_desc, use_op, l) ->
      let annot_loc = annot_loc_of_reason reason in
      let r = repos_reason (loc_of_reason reason) ?annot_loc r in
      let r =
        if use_desc
        then replace_reason_const (desc_of_reason reason) r
        else r
      in
      rec_flow cx trace (l, UseT (use_op, DefT (r, OptionalT (annot use_desc u))))

    (* Waits for a def type to become concrete, repositions it as an upper UseT
       using the stored reason. This can be used to store a reason as it flows
       through a tvar. *)

    | (u_def, ReposUseT (reason, use_desc, use_op, l)) ->
      let u = reposition_reason cx ~trace reason ~use_desc u_def in
      rec_flow cx trace (l, UseT (use_op, u))

    (* The sink component of an annotation constrains values flowing
       into the annotated site. *)

    | _, UseT (use_op, AnnotT (source_t, use_desc)) ->
      let reason = reason_of_t source_t in
      rec_flow cx trace (source_t, ReposUseT (reason, use_desc, use_op, l))

    (* The source component of an annotation flows out of the annotated
       site to downstream uses. *)

    | AnnotT (source_t, use_desc), u ->
      let reason = reason_of_t source_t in
      let t = reposition_reason ~trace cx reason ~use_desc source_t in
      rec_flow cx trace (t, u)

    (****************************************************************)
    (* BecomeT unifies a tvar with an incoming concrete lower bound *)
    (****************************************************************)

    (* MatchingPropT is triggered by a refinement, which means that the
       BecomeT has already fired and become the type being refined. We
       prevent the refined type from unifying with the original type
       because the former is necessarily a subtype of the latter and
       attempting to unify them is a symptom of an issue with BecomeT. *)
    | MatchingPropT _, BecomeT _ ->
      ()

    | _, BecomeT (reason, t) ->
      let l = reposition ~trace cx (loc_of_reason reason) l in
      rec_unify cx trace ~use_op:unknown_use ~unify_any:true l t

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
      rec_unify cx trace ~use_op:unknown_use ~unify_any:true t t_other

    (*********************************************************************)
    (* `import type` creates a properly-parameterized type alias for the *)
    (* remote type -- but only for particular, valid remote types.       *)
    (*********************************************************************)

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
    | DefT (_, ObjT _), ImportTypeT(_, "default", t) ->
      rec_flow_t cx trace (l, t)

    | (exported_type, ImportTypeT(reason, export_name, t)) ->
      (match canonicalize_imported_type cx trace reason exported_type with
      | Some imported_t -> rec_flow_t cx trace (imported_t, t)
      | None -> add_output cx ~trace (
          FlowError.EImportValueAsType (reason, export_name)
        )
      )

    (************************************************************************)
    (* `import typeof` creates a properly-parameterized type alias for the  *)
    (* "typeof" the remote export.                                          *)
    (************************************************************************)
    | DefT (_, PolyT(typeparams, ((DefT (_, ClassT _) | DefT (_, FunT _)) as lower_t), id)),
      ImportTypeofT(reason, _, t) ->
      let typeof_t = mk_typeof_annotation cx ~trace reason lower_t in
      rec_flow_t cx trace (poly_type id typeparams (DefT (reason, TypeT typeof_t)), t)

    | (DefT (_, TypeT _) | DefT (_, PolyT(_, DefT (_, TypeT _), _))),
      ImportTypeofT(reason, export_name, _) ->
      add_output cx ~trace (FlowError.EImportTypeAsTypeof (reason, export_name))

    | (_, ImportTypeofT(reason, _, t)) ->
      let typeof_t = mk_typeof_annotation cx ~trace reason l in
      rec_flow_t cx trace (DefT (reason, TypeT typeof_t), t)

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
    | (ModuleT(_, exports, _), ExportNamedT(_, skip_dupes, tmap, t_out)) ->
      tmap |> SMap.iter (fun name (loc, t) ->
        if skip_dupes && Context.has_export cx exports.exports_tmap name
        then ()
        else Context.set_export cx exports.exports_tmap name (loc, t)
      );
      rec_flow_t cx trace (l, t_out)

    (** Copy the named exports from a source module into a target module. Used
        to implement `export * from 'SomeModule'`, with the current module as
        the target and the imported module as the source. *)
    | (ModuleT(_, source_exports, _),
       CopyNamedExportsT(reason, target_module_t, t_out)) ->
      let source_tmap = Context.find_exports cx source_exports.exports_tmap in
      rec_flow cx trace (
        target_module_t,
        ExportNamedT(reason, (*skip_dupes*)true, source_tmap, t_out)
      )

    (**
     * Copy only the type exports from a source module into a target module.
     * Used to implement `export type * from ...`.
     *)
    | ModuleT(_, source_exports, _),
      CopyTypeExportsT(reason, target_module_t, t_out) ->
      let source_exports = Context.find_exports cx source_exports.exports_tmap in
      (* Remove locations. TODO at some point we may want to include them here. *)
      let source_exports = SMap.map snd source_exports in
      let target_module_t =
        SMap.fold (fun export_name export_t target_module_t ->
          Tvar.mk_where cx reason (fun t -> rec_flow cx trace (
            export_t,
            ExportTypeT(reason, true, export_name, target_module_t, t)
          ))
        ) source_exports target_module_t
      in
      rec_flow_t cx trace (target_module_t, t_out)

    (**
     * Export a type from a given ModuleT, but only if the type is compatible
     * with `import type`/`export type`. When it is not compatible, it is simply
     * not added to the exports map.
     *
     * Note that this is very similar to `ExportNamedT` except that it only
     * exports one type at a time and it takes the type to be exported as a
     * lower (so that the type can be filtered post-resolution).
     *)
    | l, ExportTypeT(reason, skip_dupes, export_name, target_module_t, t_out) ->
      let is_type_export = (
        match l with
        | DefT (_, ObjT _) when export_name = "default" -> true
        | l -> canonicalize_imported_type cx trace reason l <> None
      ) in
      if is_type_export then
        rec_flow cx trace (target_module_t, ExportNamedT(
          reason,
          skip_dupes,
          (* TODO we may want to add location information here *)
          SMap.singleton export_name (None, l),
          t_out
        ))
      else
        rec_flow_t cx trace (target_module_t, t_out)

    (* There is nothing to copy from a module exporting `any` or `Object`. *)
    | DefT (_, (AnyT | AnyObjT)), CopyNamedExportsT(_, target_module, t) ->
      rec_flow_t cx trace (target_module, t)

    (**
     * ObjT CommonJS export values have their properties turned into named
     * exports
     *)
    | DefT (_, ObjT {props_tmap; proto_t; _;}),
      CJSExtractNamedExportsT(
        reason, (module_t_reason, exporttypes, is_strict), t_out
      ) ->

      (* Copy props from the prototype *)
      let module_t = Tvar.mk_where cx reason (fun t ->
        rec_flow cx trace (
          proto_t,
          CJSExtractNamedExportsT(reason, (module_t_reason, exporttypes, is_strict), t)
        )
      ) in

      (* Copy own props *)
      rec_flow cx trace (module_t, ExportNamedT(
        reason,
        false, (* skip_dupes *)
        Properties.extract_named_exports (Context.find_props cx props_tmap),
        t_out
      ))

    (**
     * InstanceT CommonJS export values have their properties turned into named
     * exports
     *)
    | DefT (_, InstanceT(_, _, _, {fields_tmap; methods_tmap; _;})),
      CJSExtractNamedExportsT(
        reason, (module_t_reason, exporttypes, is_strict), t_out
      ) ->

      let module_t = ModuleT (module_t_reason, exporttypes, is_strict) in

      let extract_named_exports id =
        Context.find_props cx id
        |> SMap.filter (fun x _ -> not (is_munged_prop_name cx x))
        |> Properties.extract_named_exports
      in

      (* Copy fields *)
      let module_t = Tvar.mk_where cx reason (fun t ->
        rec_flow cx trace (module_t, ExportNamedT(
          reason,
          false, (* skip_dupes *)
          extract_named_exports fields_tmap,
          t
        ))
      ) in

      (* Copy methods *)
      rec_flow cx trace (module_t, ExportNamedT(
        reason,
        false, (* skip_dupes *)
        extract_named_exports methods_tmap,
        t_out
      ))

    (* If the module is exporting any or Object, then we allow any named
     * import
     *)
    | DefT (_, (AnyT | AnyObjT)),
      CJSExtractNamedExportsT(_, (module_t_reason, exporttypes, is_strict), t_out) ->
      let module_t = ModuleT (
        module_t_reason,
        { exporttypes with has_every_named_export = true; },
        is_strict
      ) in
      rec_flow_t cx trace (module_t, t_out)

    (**
     * All other CommonJS export value types do not get merged into the named
     * exports tmap in any special way.
     *)
    | (_, CJSExtractNamedExportsT(_, (module_t_reason, exporttypes, is_strict), t_out)) ->
      let module_t = ModuleT (module_t_reason, exporttypes, is_strict) in
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
    | (ModuleT(_, exports, imported_is_strict), CJSRequireT(reason, t, is_strict)) ->
      check_nonstrict_import cx trace is_strict imported_is_strict reason;
      let cjs_exports = (
        match exports.cjs_export with
        | Some t ->
          (* reposition the export to point at the require(), like the object
             we create below for non-CommonJS exports *)
          reposition ~trace cx (loc_of_reason reason) t
        | None ->
          (* convert ES module's named exports to an object *)
          let proto = ObjProtoT reason in
          let exports_tmap = Context.find_exports cx exports.exports_tmap in
          let props = SMap.map (fun (loc, t) -> Field (loc, t, Neutral)) exports_tmap in
          Obj_type.mk_with_proto cx reason
            ~sealed:true ~frozen:true ~props proto
      ) in
      rec_flow_t cx trace (cjs_exports, t)

    (* import * as X from 'SomeModule'; *)
    | (ModuleT(_, exports, imported_is_strict), ImportModuleNsT(reason, t, is_strict)) ->
      check_nonstrict_import cx trace is_strict imported_is_strict reason;
      let exports_tmap = Context.find_exports cx exports.exports_tmap in
      let props = SMap.map (fun (loc, t) -> Field (loc, t, Neutral)) exports_tmap in
      let props = match exports.cjs_export with
      | Some t ->
        (* TODO this Field should probably have a location *)
        let p = Field (None, t, Neutral) in
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
      let ns_obj = Obj_type.mk_with_proto cx reason
        ~sealed:true ~frozen:true ?dict ~props proto
      in
      rec_flow_t cx trace (ns_obj, t)

    (* import [type] X from 'SomeModule'; *)
    | ModuleT(module_reason, exports, imported_is_strict),
      ImportDefaultT(reason, import_kind, (local_name, module_name), t, is_strict) ->
      check_nonstrict_import cx trace is_strict imported_is_strict reason;
      let export_t = match exports.cjs_export with
        | Some t -> t
        | None ->
            let exports_tmap = Context.find_exports cx exports.exports_tmap in
            match SMap.get "default" exports_tmap with
              | Some (_, t) -> t
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
                add_output cx ~trace (FlowError.ENoDefaultExport
                  (reason, module_name, suggestion));
                AnyT.why module_reason
      in

      let import_t = (
        match import_kind with
        | ImportType ->
          Tvar.mk_where cx reason (fun tvar ->
            rec_flow cx trace (export_t, ImportTypeT(reason, "default", tvar))
          )
        | ImportTypeof ->
          Tvar.mk_where cx reason (fun tvar ->
            rec_flow cx trace (export_t, ImportTypeofT(reason, "default", tvar))
          )
        | ImportValue ->
          rec_flow cx trace (export_t, AssertImportIsValueT(reason, "default"));
          export_t
      ) in
      rec_flow_t cx trace (import_t, t)

    (* import {X} from 'SomeModule'; *)
    | ModuleT(_, exports, imported_is_strict),
      ImportNamedT(reason, import_kind, export_name, module_name, t, is_strict) ->
        check_nonstrict_import cx trace is_strict imported_is_strict reason;
        (**
         * When importing from a CommonJS module, we shadow any potential named
         * exports called "default" with a pointer to the raw `module.exports`
         * object
         *)
        let exports_tmap = (
          let exports_tmap = Context.find_exports cx exports.exports_tmap in
          (* Drop locations; they are not needed here *)
          let exports_tmap = SMap.map snd exports_tmap in
          match exports.cjs_export with
          | Some t -> SMap.add "default" t exports_tmap
          | None -> exports_tmap
        ) in
        let has_every_named_export = exports.has_every_named_export in
        let import_t = (
          match (import_kind, SMap.get export_name exports_tmap) with
          | (ImportType, Some t) ->
            Tvar.mk_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeT(reason, export_name, tvar))
            )
          | (ImportType, None) when has_every_named_export ->
            let t = AnyT.why reason in
            Tvar.mk_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeT(reason, export_name, tvar))
            )
          | (ImportTypeof, Some t) ->
            Tvar.mk_where cx reason (fun tvar ->
              rec_flow cx trace (t, ImportTypeofT(reason, export_name, tvar))
            )
          | (ImportTypeof, None) when has_every_named_export ->
            let t = AnyT.why reason in
            Tvar.mk_where cx reason (fun tvar ->
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
                FlowError.EOnlyDefaultExport (reason, module_name, export_name)
              else
                let known_exports = SMap.keys exports_tmap in
                let suggestion = typo_suggestion known_exports export_name in
                FlowError.ENoNamedExport (reason, module_name, export_name, suggestion)
            in
            add_output cx ~trace msg;
            AnyT.why reason
        ) in
        rec_flow_t cx trace (import_t, t)

    (* imports are `any`-typed when they are from (1) unchecked modules or (2)
       modules with `any`-typed exports *)
    | DefT (_, AnyObjT),
        ( CJSRequireT(reason, t, _)
        | ImportModuleNsT(reason, t, _)
        | ImportDefaultT(reason, _, _, t, _)
        | ImportNamedT(reason, _, _, _, t, _)
        ) ->
      rec_flow_t cx trace (AnyT.why reason, t)

    | DefT (lreason, AnyT), (CJSRequireT(reason, t, _) | ImportModuleNsT(reason, t, _)) ->
      let () = match desc_of_reason lreason with
        (* Use a special reason so we can tell the difference between an any-typed import
         * from an untyped module and an any-typed import from a nonexistent module. *)
        | RUntypedModule module_name ->
          let loc = Reason.loc_of_reason reason in
          let message = FlowError.EUntypedImport (loc, module_name) in
          add_output cx ~trace message
        | _ -> ()
      in
      rec_flow_t cx trace (AnyT.why reason, t)

    | DefT (lreason, AnyT), ImportDefaultT(reason, import_kind, _, t, _) ->
      let () = match import_kind, desc_of_reason lreason with
        (* Use a special reason so we can tell the difference between an any-typed type import
         * from an untyped module and an any-typed import from a nonexistent module. *)
        | (ImportType | ImportTypeof), RUntypedModule module_name ->
          let loc = Reason.loc_of_reason reason in
          let message = FlowError.EUntypedTypeImport (loc, module_name) in
          add_output cx ~trace message
        | ImportValue, RUntypedModule module_name ->
          let loc = Reason.loc_of_reason reason in
          let message = FlowError.EUntypedImport (loc, module_name) in
          add_output cx ~trace message
        | _ -> ()
      in
      rec_flow_t cx trace (AnyT.why reason, t)

    | DefT (lreason, AnyT), ImportNamedT(reason, import_kind, _, _, t, _) ->
      let () = match import_kind, desc_of_reason lreason with
        (* Use a special reason so we can tell the difference between an any-typed type import
         * from an untyped module and an any-typed type import from a nonexistent module. *)
        | (ImportType | ImportTypeof), RUntypedModule module_name ->
          let loc = Reason.loc_of_reason reason in
          let message = FlowError.EUntypedTypeImport (loc, module_name) in
          add_output cx ~trace message
        | ImportValue, RUntypedModule module_name ->
          let loc = Reason.loc_of_reason reason in
          let message = FlowError.EUntypedImport (loc, module_name) in
          add_output cx ~trace message
        | _ -> ()
      in
      rec_flow_t cx trace (AnyT.why reason, t)

    | (DefT (_, PolyT (_, DefT (_, TypeT _), _)) | DefT (_, TypeT _)),
       AssertImportIsValueT(reason, name) ->
      add_output cx ~trace (FlowError.EImportTypeAsValue (reason, name))

    | (_, AssertImportIsValueT(_, _)) -> ()

    (*******************************)
    (* common implicit conversions *)
    (*******************************)

    | (_, UseT (_, DefT (_, NumT _))) when numeric l -> ()

    | (_, UseT (_, DefT (_, AnyObjT))) when object_like l -> ()
    | (DefT (_, AnyObjT), UseT (_, u)) when object_like u -> ()

    | (_, UseT (_, DefT (_, AnyFunT))) when function_like l -> ()

    | DefT (reason, AnyFunT), GetPropT (_, _, Named (_, x), _)
    | DefT (reason, AnyFunT), SetPropT (_, _, Named (_, x), _, _, _)
    | DefT (reason, AnyFunT), LookupT (_, _, _, Named (_, x), _)
    | DefT (reason, AnyFunT), MethodT (_, _, _, Named (_, x), _, _)
        when is_function_prototype x ->
      rec_flow cx trace (FunProtoT reason, u)
    | (DefT (_, AnyFunT), UseT (_, u)) when function_like u -> ()
    | (DefT (_, AnyFunT), UseT (_, u)) when object_like u -> ()
    | DefT (_, AnyFunT), UseT (_, (DefT (_, TypeT _) | DefT (_, AnyFunT))) -> ()

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
    | CustomFunT (lreason, Idx),
      CallT (use_op, reason_op, {
        call_this_t;
        call_targs;
        call_args_tlist;
        call_tout;
        call_closure_t;
        call_strict_arity;
      }) ->
      let tout = match call_targs, call_args_tlist with
      | None, (Arg obj)::(Arg cb)::[] ->
        let wrapped_obj = InternalT (IdxWrapper (reason_op, obj)) in
        let callback_result = Tvar.mk_where cx reason_op (fun t ->
          rec_flow cx trace (cb, CallT (use_op, reason_op, {
            call_this_t;
            call_targs = None;
            call_args_tlist = [Arg wrapped_obj];
            call_tout = t;
            call_closure_t;
            call_strict_arity;
          }))
        ) in
        let unwrapped_t = Tvar.mk_where cx reason_op (fun t ->
          rec_flow cx trace (callback_result, IdxUnwrap(reason_op, t))
        ) in
        let maybe_r = replace_reason (fun desc -> RMaybe desc) reason_op in
        DefT (maybe_r, MaybeT unwrapped_t)
      | None, (SpreadArg t1)::(SpreadArg t2)::_ ->
        add_output cx ~trace FlowError.(
          EUnsupportedSyntax (loc_of_t t1, SpreadArgument));
        add_output cx ~trace FlowError.(
          EUnsupportedSyntax (loc_of_t t2, SpreadArgument));
        AnyT.why reason_op
      | None, (SpreadArg t)::_
      | None, _::(SpreadArg t)::_ ->
        let spread_loc = loc_of_t t in
        add_output cx ~trace FlowError.(
          EUnsupportedSyntax (spread_loc, SpreadArgument));
        AnyT.why reason_op
      | Some _, _ ->
        add_output cx ~trace FlowError.(ECallTypeArity {
          call_loc = loc_of_reason reason_op;
          is_new = false;
          reason_arity = lreason;
          expected_arity = 0;
        });
        AnyT.why reason_op
      | _ ->
        (* Why is idx strict about arity? No other functions are. *)
        add_output cx ~trace FlowError.(EIdxArity reason_op);
        AnyT.why reason_op
      in
      rec_flow_t cx trace (tout, call_tout)

    (* Unwrap idx() callback param *)
    | InternalT (IdxWrapper (_, obj)), IdxUnwrap (_, t) -> rec_flow_t cx trace (obj, t)
    | (_, IdxUnwrap (_, t)) -> rec_flow_t cx trace (l, t)

    (* De-maybe-ify an idx() property access *)
    | (DefT (_, MaybeT inner_t), IdxUnMaybeifyT _)
    | (DefT (_, OptionalT inner_t), IdxUnMaybeifyT _)
      -> rec_flow cx trace (inner_t, u)
    | DefT (_, NullT), IdxUnMaybeifyT _ -> ()
    | DefT (_, VoidT), IdxUnMaybeifyT _ -> ()
    | _, IdxUnMaybeifyT (_, t) when (
        match l with
        | DefT (_, (UnionT _ | IntersectionT _)) -> false
        | _ -> true
      ) ->
      rec_flow_t cx trace (l, t)

    (* The set of valid uses of an idx() callback parameter. In general this
       should be limited to the various forms of property access operations. *)
    | InternalT (IdxWrapper (idx_reason, obj)), ReposLowerT (reason_op, use_desc, u) ->
      let repositioned_obj = Tvar.mk_where cx reason_op (fun t ->
        rec_flow cx trace (obj, ReposLowerT (reason_op, use_desc, UseT (unknown_use, t)))
      ) in
      rec_flow cx trace (InternalT (IdxWrapper(idx_reason, repositioned_obj)), u)

    | InternalT (IdxWrapper (idx_reason, obj)), GetPropT (use_op, reason_op, propname, t_out) ->
      let de_maybed_obj = Tvar.mk_where cx idx_reason (fun t ->
        rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t))
      ) in
      let prop_type = Tvar.mk_where cx reason_op (fun t ->
        rec_flow cx trace (de_maybed_obj, GetPropT (use_op, reason_op, propname, t))
      ) in
      rec_flow_t cx trace (InternalT (IdxWrapper (idx_reason, prop_type)), t_out)

    | InternalT (IdxWrapper (idx_reason, obj)),
      GetPrivatePropT (use_op, reason_op, name, class_bindings, static, t_out) ->
      let de_maybed_obj = Tvar.mk_where cx idx_reason (fun t ->
        rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t))
      ) in
      let prop_type = Tvar.mk_where cx reason_op (fun t ->
        rec_flow cx trace (de_maybed_obj,
        GetPrivatePropT (use_op, reason_op, name, class_bindings, static, t))
      ) in
      rec_flow_t cx trace (InternalT (IdxWrapper (idx_reason, prop_type)), t_out)

    | InternalT (IdxWrapper (idx_reason, obj)), GetElemT (use_op, reason_op, prop, t_out) ->
      let de_maybed_obj = Tvar.mk_where cx idx_reason (fun t ->
        rec_flow cx trace (obj, IdxUnMaybeifyT (idx_reason, t))
      ) in
      let prop_type = Tvar.mk_where cx reason_op (fun t ->
        rec_flow cx trace (de_maybed_obj, GetElemT (use_op, reason_op, prop, t))
      ) in
      rec_flow_t cx trace (InternalT (IdxWrapper (idx_reason, prop_type)), t_out)

    | InternalT (IdxWrapper (reason, _)), UseT _ ->
      add_output cx ~trace (FlowError.EIdxUse1 reason)

    | InternalT (IdxWrapper (reason, _)), _ ->
      add_output cx ~trace (FlowError.EIdxUse2 reason)

    (*********************)
    (* optional chaining *)
    (*********************)

    | DefT (r, (NullT | VoidT)), OptionalChainT (r', lhs_reason, chain) ->
      Context.mark_optional_chain cx (loc_of_reason r') lhs_reason ~useful:true;
      Nel.iter (fun (_, t_out) -> rec_flow_t cx trace (InternalT (OptionalChainVoidT r), t_out)) chain;

    | InternalT (OptionalChainVoidT _), OptionalChainT (r', lhs_reason, chain) ->
      Context.mark_optional_chain cx (loc_of_reason r') lhs_reason ~useful:false;
      Nel.iter (fun (_, t_out) -> rec_flow_t cx trace (l, t_out)) chain;

    | _, OptionalChainT (r', lhs_reason, chain) when (
        match l with
        | DefT (_, (MaybeT _ | OptionalT _ | UnionT _ | IntersectionT _)) -> false
        | _ -> true
      ) ->
      Context.mark_optional_chain cx (loc_of_reason r') lhs_reason ~useful:(
        match l with
        | DefT (_, (MixedT _ | AnyT | AnyObjT | AnyFunT)) -> true
        | _ -> false
      );
      let lhs_t = ref l in
      Nel.iter (fun (opt_use, t_out) ->
        let t_out' = Tvar.mk cx (reason_of_t t_out) in
        rec_flow cx trace (!lhs_t, apply_opt_use opt_use t_out');
        rec_flow_t cx trace (t_out', t_out);
        lhs_t := t_out';
      ) chain;

    | InternalT (OptionalChainVoidT r), u ->
      rec_flow cx trace (DefT (r, VoidT), u);

    (***************)
    (* maybe types *)
    (***************)

    (** The type maybe(T) is the same as null | undefined | UseT *)

    | DefT (r, (NullT | VoidT)), UseT (use_op, DefT (_, MaybeT tout)) ->
      rec_flow cx trace (EmptyT.why r, UseT (use_op, tout))

    | DefT (_, MaybeT _), ReposLowerT (reason_op, use_desc, u) ->
      (* Don't split the maybe type into its constituent members. Instead,
         reposition the entire maybe type. *)
      let loc = loc_of_reason reason_op in
      let desc = if use_desc then Some (desc_of_reason reason_op) else None in
      rec_flow cx trace (reposition cx ~trace loc ?desc l, u)

    | DefT (_, MaybeT t), ObjAssignFromT (_, _, _, ObjAssign _) ->
      (* This isn't correct, but matches the existing incorrectness of spreads
       * today. In particular, spreading `null` and `void` become {}. The wrong
       * part is that spreads should distribute through unions, so `{...?T}`
       * should be `{...null}|{...void}|{...T}`, which simplifies to `{}`. *)
      rec_flow cx trace (t, u)

    | DefT (_, MaybeT t), UseT (_, DefT (_, MaybeT _)) ->
      rec_flow cx trace (t, u)

    | (DefT (reason, MaybeT t), _) ->
      let reason = replace_reason_const ~keep_def_loc:true RNullOrVoid reason in
      rec_flow cx trace (NullT.make reason, u);
      rec_flow cx trace (VoidT.make reason, u);
      rec_flow cx trace (t, u)

    (******************)
    (* optional types *)
    (******************)

    (** The type optional(T) is the same as undefined | UseT *)

    | DefT (r, VoidT), UseT (use_op, DefT (_, OptionalT tout)) ->
      rec_flow cx trace (EmptyT.why r, UseT (use_op, tout))

    | DefT (_, OptionalT _), ReposLowerT (reason, use_desc, u) ->
      (* Don't split the optional type into its constituent members. Instead,
         reposition the entire optional type. *)
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

    | DefT (_, OptionalT t), ObjAssignFromT (_, _, _, ObjAssign _) ->
      (* This isn't correct, but matches the existing incorrectness of spreads
       * today. In particular, spreading `null` and `void` become {}. The wrong
       * part is that spreads should distribute through unions, so `{...?T}`
       * should be `{...null}|{...void}|{...T}`, which simplifies to `{}`. *)
      rec_flow cx trace (t, u)

    | DefT (_, OptionalT t), UseT (_, DefT (_, OptionalT _))
    | DefT (_, OptionalT t), UseT (_, DefT (_, MaybeT _)) ->
      rec_flow cx trace (t, u)

    | DefT (r, OptionalT t), _ ->
      rec_flow cx trace (VoidT.why r, u);
      rec_flow cx trace (t, u)

    (*****************)
    (* logical types *)
    (*****************)

    | DefT (_, AnyT), NotT (reason, tout) ->
      rec_flow_t cx trace (AnyT.why reason, tout)

    (* !x when x is of unknown truthiness *)
    | DefT (_, BoolT None), NotT (reason, tout)
    | DefT (_, StrT AnyLiteral), NotT (reason, tout)
    | DefT (_, NumT AnyLiteral), NotT (reason, tout) ->
      rec_flow_t cx trace (BoolT.at (loc_of_reason reason), tout)

    (* !x when x is falsy *)
    | DefT (_, BoolT (Some false)), NotT (reason, tout)
    | DefT (_, SingletonBoolT false), NotT (reason, tout)
    | DefT (_, StrT (Literal (_, ""))), NotT (reason, tout)
    | DefT (_, SingletonStrT ""), NotT (reason, tout)
    | DefT (_, NumT (Literal (_, (0., _)))), NotT (reason, tout)
    | DefT (_, SingletonNumT (0., _)), NotT (reason, tout)
    | DefT (_, NullT), NotT (reason, tout)
    | DefT (_, VoidT), NotT (reason, tout) ->
      let reason = replace_reason_const (RBooleanLit true) reason in
      rec_flow_t cx trace (DefT (reason, BoolT (Some true)), tout)

    (* !x when x is truthy *)
    | (_, NotT(reason, tout)) ->
      let reason = replace_reason_const (RBooleanLit false) reason in
      rec_flow_t cx trace (DefT (reason, BoolT (Some false)), tout)

    | (left, AndT(_, right, u)) ->
      (* a falsy && b ~> a
         a truthy && b ~> b
         a && b ~> a falsy | b *)
      let truthy_left = Type_filter.exists left in
      (match truthy_left with
      | DefT (_, EmptyT) ->
        (* falsy *)
        rec_flow cx trace (left, PredicateT (NotP (ExistsP None), u))
      | _ ->
        (match Type_filter.not_exists left with
        | DefT (_, EmptyT) -> (* truthy *)
          rec_flow cx trace (right, UseT (unknown_use, u))
        | _ ->
          rec_flow cx trace (left, PredicateT (NotP (ExistsP None), u));
          begin match truthy_left with
          | DefT (_, EmptyT) -> ()
          | _ ->
            rec_flow cx trace (right, UseT (unknown_use, u))
          end
        )
      )

    | (left, OrT(_, right, u)) ->
      (* a truthy || b ~> a
         a falsy || b ~> b
         a || b ~> a truthy | b *)
      let falsy_left = Type_filter.not_exists left in
      (match falsy_left with
      | DefT (_, EmptyT) ->
        (* truthy *)
        rec_flow cx trace (left, PredicateT (ExistsP None, u))
      | _ ->
        (match Type_filter.exists left with
        | DefT (_, EmptyT) -> (* falsy *)
          rec_flow cx trace (right, UseT (unknown_use, u))
        | _ ->
          rec_flow cx trace (left, PredicateT (ExistsP None, u));
          begin match falsy_left with
          | DefT (_, EmptyT) -> ()
          | _ ->
            rec_flow cx trace (right, UseT (unknown_use, u))
          end
        )
      )

    | (left, NullishCoalesceT(_, right, u)) when (
        match left with
        | DefT (_, (
            OptionalT _
          | MaybeT _
          | UnionT _
          | IntersectionT _
        )) -> false
        | _ -> true
      ) ->
      begin match left with
      | DefT (_, (
          NullT
        | VoidT
      )) -> rec_flow_t cx trace (right, u)
      | _ -> rec_flow_t cx trace (left, u)
      end

    (*****************************)
    (* upper and lower any types *)
    (*****************************)

    (** AnyWithLowerBoundT and AnyWithUpperBoundT are mildly useful types that
        model subtyping constraints without introducing potentially unwanted
        effects: they can appear on both sides of a type, but only constrain one
        of those sides. In some sense, they are liked bounded AnyT: indeed, AnyT
        has the same behavior as AnyWithLowerBound (EmptyT) and
        AnyWithUpperBoundT (MixedT). Thus, these types can be used instead of
        AnyT when some precise typechecking is required without overconstraining
        the system. A completely static alternative would be achieved with
        bounded type variables, which Flow does not support yet. **)

    | AnyWithLowerBoundT t, _ ->
      rec_flow cx trace (t, u)

    | _, UseT (use_op, AnyWithLowerBoundT t) ->
      rec_flow cx trace (l, UseT (use_op, MixedT.why (reason_of_t t)))

    | AnyWithUpperBoundT t, _ ->
      rec_flow cx trace (EmptyT.why (reason_of_t t), u)

    | _, UseT (_, AnyWithUpperBoundT t) ->
      rec_flow_t cx trace (l, t)


    | _, ReactKitT (use_op, reason_op, React.CreateElement0 (clone, config, children, tout)) ->
      let tool = React.CreateElement (clone, l, config, children, tout) in
      rec_flow cx trace (l, ReactKitT (use_op, reason_op, tool))

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

    | (ThisTypeAppT(reason_tapp,c,this,ts), _) ->
      let reason_op = reason_of_use_t u in
      let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
      let c = instantiate_this_class cx trace reason_op tc this in
      rec_flow cx trace (mk_instance cx ~trace reason_op ~for_type:false c, u)

    | (_, UseT (use_op, ThisTypeAppT(reason_tapp,c,this,ts))) ->
      let reason_op = reason_of_t l in
      let tc = specialize_class cx trace ~reason_op ~reason_tapp c ts in
      let c = instantiate_this_class cx trace reason_op tc this in
      let t_out = mk_instance cx ~trace reason_op ~for_type:false c in
      rec_flow cx trace (l, UseT (use_op, t_out))

    | DefT (_, TypeAppT _), ReposLowerT (reason, use_desc, u) ->
        rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

    | DefT (reason_tapp, TypeAppT(use_op, c, ts)), MethodT (_, _, _, _, _, _) ->
        let reason_op = reason_of_use_t u in
        let t = mk_typeapp_instance cx
          ~trace ~use_op ~reason_op ~reason_tapp ~cache:[] c ts in
        rec_flow cx trace (t, u)

    (* If we have a TypeAppT (c, ts) ~> TypeAppT (c, ts) then we want to
     * concretize both cs to PolyTs so that we may referentially compare them.
     * We cannot compare the non-concretized versions since they may have been
     * reposition, they may be two OpenTs from different locations, or any other
     * way you can access the same PolyT via different means that results in a
     * different c being passed to TypeAppT.
     *
     * We use the ConcretizeTypeAppsT use type to concretize both the c of our
     * upper and lower TypeAppT bound. We start by concretizing the upper bound
     * which we signal by setting the final element in ConcretizeTypeAppsT to
     * true. *)
    | DefT (r1, TypeAppT (op1, c1, ts1)),
      UseT (use_op, DefT (r2, TypeAppT (op2, c2, ts2))) ->
      if TypeAppExpansion.push_unless_loop cx (c1, ts1) then (
        if TypeAppExpansion.push_unless_loop cx (c2, ts2) then (
          rec_flow cx trace (c2, ConcretizeTypeAppsT
            (use_op, (ts2, op2, r2), (c1, ts1, op1, r1), true));
            TypeAppExpansion.pop ();
        );
        TypeAppExpansion.pop ();
      );


    (* When we have concretized the c for our upper bound TypeAppT then we want
     * to concretize the lower bound. We flip all our arguments to
     * ConcretizeTypeAppsT and set the final element to false to signal that we
     * have concretized the upper bound's c.
     *
     * If the upper bound's c is not a PolyT then we will fall down to an
     * incompatible use error. *)
    | DefT (_, PolyT _) as c2,
      ConcretizeTypeAppsT (use_op, (ts2, op2, r2), (c1, ts1, op1, r1), true) ->
      rec_flow cx trace (c1, ConcretizeTypeAppsT
        (use_op, (ts1, op1, r1), (c2, ts2, op2, r2), false))

    (* When we have concretized the c for our lower bound TypeAppT then we can
     * finally run our TypeAppT ~> TypeAppT logic. If we have referentially the
     * same PolyT for each TypeAppT then we want to check the type arguments
     * only. (Checked in the when condition.) If we do not have the same PolyT
     * for each TypeAppT then we want to expand our TypeAppTs and compare the
     * expanded results.
     *
     * If the lower bound's c is not a PolyT then we will fall down to an
     * incompatible use error.
     *
     * The upper bound's c should always be a PolyT here since we could not have
     * made it here if it was not given the logic of our earlier case. *)
    | DefT (_, PolyT (_, _, id1)),
      ConcretizeTypeAppsT (use_op, (ts1, _, r1), (DefT (_, PolyT (_, _, id2)), ts2, _, r2), false)
      when id1 = id2 && List.length ts1 = List.length ts2 ->
      let targs = List.map2 (fun t1 t2 -> (t1, t2)) ts1 ts2 in
      rec_flow cx trace (l,
        TypeAppVarianceCheckT (use_op, r1, r2, targs))

    (* This is the case which implements the expansion for our
     * TypeAppT (c, ts) ~> TypeAppT (c, ts) when the cs are unequal. *)
    | DefT (_, PolyT (xs1, t1, id1)),
      ConcretizeTypeAppsT (use_op, (ts1, op1, r1), (DefT (_, PolyT (xs2, t2, id2)), ts2, op2, r2), false) ->
      let t1 = mk_typeapp_instance_of_poly cx trace ~use_op:op2 ~reason_op:r2 ~reason_tapp:r1
        id1 xs1 t1 ts1 in
      let t2 = mk_typeapp_instance_of_poly cx trace ~use_op:op1 ~reason_op:r1 ~reason_tapp:r2
        id2 xs2 t2 ts2 in
      rec_flow cx trace (t1, UseT (use_op, t2))

    | DefT (reason_tapp, TypeAppT (use_op, c, ts)), _ ->
      if TypeAppExpansion.push_unless_loop cx (c, ts) then (
        let reason_op = reason_of_use_t u in
        let t = mk_typeapp_instance cx ~trace ~use_op ~reason_op ~reason_tapp c ts in
        rec_flow cx trace (t, u);
        TypeAppExpansion.pop ();
      );

    | _, UseT (use_op, DefT (reason_tapp, TypeAppT (use_op_tapp, c, ts))) ->
      if TypeAppExpansion.push_unless_loop cx (c, ts) then (
        let reason_op = reason_of_t l in
        let t = mk_typeapp_instance cx ~trace ~use_op:use_op_tapp ~reason_op ~reason_tapp c ts in
        rec_flow cx trace (l, UseT (use_op, t));
        TypeAppExpansion.pop ();
      );

    (**********************)
    (*    opaque types    *)
    (**********************)

    (* If the ids are equal, we use flow_type_args to make sure that the type arguments of each
     * are compatible with each other. If there are no type args, this doesn't do anything *)
    | OpaqueT (lreason, {opaque_id = id1; opaque_arg_polarities = ps; opaque_type_args = ltargs; _}),
      UseT (use_op, OpaqueT (ureason, {opaque_id = id2; opaque_type_args = utargs; _})) when id1 = id2 ->
        flow_type_args cx trace ~use_op lreason ureason ps ltargs utargs

    (* Repositioning should happen before opaque types are considered so that we can
     * have the "most recent" location when we do look at the opaque type *)
    | OpaqueT _, ReposLowerT (reason, use_desc, u) ->
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

    (* If the type is still in the same file it was defined, we allow it to
     * expose its underlying type information *)
    | OpaqueT (r, {underlying_t = Some t; _}), _
      when Loc.source (loc_of_reason r) = Loc.source (def_loc_of_reason r) ->
      rec_flow cx trace (t, u)

    (* If the lower bound is in the same file as where the opaque type was defined,
     * we expose the underlying type information *)
    | _, UseT (use_op, OpaqueT (r, {underlying_t = Some t; _}))
      when Loc.source (loc_of_reason (reason_of_t l)) =
           Loc.source (def_loc_of_reason r) ->
      rec_flow cx trace (l, UseT (use_op, t))

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

    | DefT (rl, StrT actual), UseT (use_op, DefT (ru, SingletonStrT expected)) ->
      if TypeUtil.literal_eq expected actual
      then ()
      else
        let reasons = FlowError.ordered_reasons (rl, ru) in
        add_output cx ~trace
          (FlowError.EExpectedStringLit (reasons, expected, actual, use_op))

    | DefT (rl, NumT actual), UseT (use_op, DefT (ru, SingletonNumT expected)) ->
      if TypeUtil.number_literal_eq expected actual
      then ()
      else
        let reasons = FlowError.ordered_reasons (rl, ru) in
        add_output cx ~trace
          (FlowError.EExpectedNumberLit (reasons, expected, actual, use_op))

    | DefT (rl, BoolT actual), UseT (use_op, DefT (ru, SingletonBoolT expected)) ->
      if TypeUtil.boolean_literal_eq expected actual
      then ()
      else
        let reasons = FlowError.ordered_reasons (rl, ru) in
        add_output cx ~trace
          (FlowError.EExpectedBooleanLit (reasons, expected, actual, use_op))

    (*****************************************************)
    (* keys (NOTE: currently we only support string keys *)
    (*****************************************************)

    | DefT (reason_s, StrT literal), UseT (use_op, KeysT (reason_op, o)) ->
      let reason_next = match literal with
      | Literal (_, x) -> replace_reason_const (RProperty (Some x)) reason_s
      | _ -> replace_reason_const RUnknownString reason_s in
      (* check that o has key x *)
      let u = HasOwnPropT(use_op, reason_next, literal) in
      rec_flow cx trace (o, ReposLowerT(reason_op, false, u))

    | KeysT _, ToStringT (_, t) ->
      (* KeysT outputs strings, so we know ToStringT will be a no-op. *)
      rec_flow cx trace (l, t)

    | KeysT (reason1, o1), _ ->
      (* flow all keys of o1 to u *)
      rec_flow cx trace (o1, GetKeysT (reason1, u))

    (* helpers *)

    | DefT (reason_o, ObjT { props_tmap = mapr; dict_t; _; }),
      HasOwnPropT (use_op, reason_op, x) ->
      (match x, dict_t with
      (* If we have a literal string and that property exists *)
      | Literal (_, x), _ when Context.has_prop cx mapr x -> ()
      (* If we have a dictionary, try that next *)
      | _, Some { key; _ } -> rec_flow_t cx trace (DefT (reason_op, StrT x), key)
      | _ ->
        let prop = match x with
        | Literal (_, prop) -> Some prop
        | _ -> None
        in
        let err = FlowError.EPropNotFound (prop, (reason_op, reason_o), use_op) in
        add_output cx ~trace err)

    | DefT (reason_o, InstanceT (_, _, _, instance)),
      HasOwnPropT(use_op, reason_op, Literal (_, x)) ->
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      (match SMap.get x fields with
      | Some _ -> ()
      | None ->
        let err = FlowError.EPropNotFound (Some x, (reason_op, reason_o), use_op) in
        add_output cx ~trace err)

    | DefT (reason_o, InstanceT (_, _, _, _)), HasOwnPropT(use_op, reason_op, _) ->
        let err = FlowError.EPropNotFound (None, (reason_op, reason_o), use_op) in
        add_output cx ~trace err

    (* AnyObjT has every prop *)
    | DefT (_, AnyObjT), HasOwnPropT _ -> ()

    | DefT (_, ObjT { flags; props_tmap; dict_t; _ }), GetKeysT (reason_op, keys) ->
      begin match flags.sealed with
      | Sealed ->
        (* flow each key of l to keys *)
        Context.iter_props cx props_tmap (fun x _ ->
          let reason = replace_reason_const (RStringLit x) reason_op in
          let t = DefT (reason, StrT (Literal (None, x))) in
          rec_flow cx trace (t, keys)
        );
        Option.iter dict_t (fun { key; _ } ->
          rec_flow cx trace (key, ToStringT (reason_op, keys))
        );
      | _ ->
        rec_flow cx trace (StrT.why reason_op, keys)
      end

    | DefT (_, InstanceT (_, _, _, instance)), GetKeysT (reason_op, keys) ->
      (* methods are not enumerable, so only walk fields *)
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      fields_tmap |> SMap.iter (fun x _ ->
        let reason = replace_reason_const (RStringLit x) reason_op in
        let t = DefT (reason, StrT (Literal (None, x))) in
        rec_flow cx trace (t, keys)
      )

    | DefT (reason, (AnyObjT | AnyFunT)), GetKeysT (_, keys) ->
      rec_flow cx trace (StrT.why reason, keys)

    | DefT (_, AnyT), GetKeysT (reason_op, keys) ->
      rec_flow cx trace (AnyT.why reason_op, keys)

    (** In general, typechecking is monotonic in the sense that more constraints
        produce more errors. However, sometimes we may want to speculatively try
        out constraints, backtracking if they produce errors (and removing the
        errors produced). This is useful to typecheck union types and
        intersection types: see below. **)

    (** NOTE: It is important that any def type that simplifies to a union or
        intersection of other def types be processed before we process unions
        and intersections: otherwise we may get spurious errors. **)

    (**********)
    (* values *)
    (**********)

    | DefT (_, ObjT o), GetValuesT (reason, values) ->
      let {
        flags;
        proto_t = _;
        props_tmap = tmap;
        dict_t;
        call_t = _; (* call props excluded from values *)
      } = o in
      (* Find all of the props. *)
      let props = Context.find_props cx tmap in
      (* Get the read type for all readable properties and discard the rest. *)
      let ts = SMap.fold (fun _ prop ts ->
        match Property.read_t prop with
        | Some t ->
            let t = if flags.frozen then
              match t with
              | DefT (t_reason, StrT (Literal (_, lit))) ->
                let t_reason = replace_reason_const (RStringLit lit) t_reason in
                DefT (t_reason, SingletonStrT lit)
              | DefT (t_reason, NumT (Literal (_, lit))) ->
                let t_reason = replace_reason_const (RNumberLit (snd lit)) t_reason in
                DefT (t_reason, SingletonNumT lit)
              | DefT (t_reason, BoolT (Some lit)) ->
                let t_reason = replace_reason_const (RBooleanLit lit) t_reason in
                DefT (t_reason, SingletonBoolT lit)
              | _ -> t
            else t in
            t :: ts
        | None -> ts
      ) props [] in
      (* If the object has a dictionary value then add that to our types. *)
      let ts = match dict_t with
      | Some { value; _ } -> value :: ts
      | None -> ts in
      (* Create a union type from all our selected types. *)
      let values_l = union_of_ts reason ts in
      rec_flow_t cx trace (values_l, values)

    | DefT (_, InstanceT (_, _, _, { fields_tmap = tmap; _; })), GetValuesT (reason, values) ->
      (* Find all of the props. *)
      let props = Context.find_props cx tmap in
      (* Get the read type for all readable properties and discard the rest. *)
      let ts = SMap.fold (fun key prop ts ->
        match Property.read_t prop with
        (* We don't want to include the property type if its name is the
           internal value "$key" because that will be the type for the instance
           index and not the value. *)
        | Some t when key != "$key" -> t :: ts
        | _ -> ts
      ) props [] in
      (* Create a union type from all our selected types. *)
      let values_l = union_of_ts reason ts in
      rec_flow_t cx trace (values_l, values)

    (* Any will always be ok *)
    | DefT (_, AnyT), GetValuesT (reason, values)
    | DefT (_, AnyObjT), GetValuesT (reason, values) ->
      rec_flow_t cx trace (AnyT.why reason, values)

    (********************************)
    (* union and intersection types *)
    (********************************)

    (* Don't split the union type into its constituent members. Instead,
       reposition the entire union type. *)
    | DefT (_, UnionT _), ReposLowerT (reason, use_desc, u) ->
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

    | DefT (_, UnionT _), ObjKitT (use_op, reason, resolve_tool, tool, tout) ->
      object_kit cx trace ~use_op reason resolve_tool tool tout l

    (* cases where there is no loss of precision *)

    (** Optimization where an union is a subset of another. Equality modulo
        reasons is important for this optimization to be effective, since types
        are repositioned everywhere.

        TODO: (1) Define a more general partial equality, that takes into
        account unified type variables. (2) Get rid of UnionRep.quick_mem. **)
    | DefT (_, UnionT rep1), UseT (_, DefT (_, UnionT rep2)) when
        let ts2 = Type_mapper.union_flatten cx @@ UnionRep.members rep2 in
        Type_mapper.union_flatten cx @@ UnionRep.members rep1 |> List.for_all (fun t1 ->
          List.exists (TypeUtil.quick_subtype t1) ts2
        ) ->
      ()

    | DefT (r, UnionT rep), SentinelPropTestT (_reason, l, _key, sense, sentinel, result) ->
      (* we have the check l.key === sentinel where l.key is a union *)
      if sense then
        match sentinel with
        | Enum.One enum ->
          begin
            let def = match enum with
              | Enum.Str v -> SingletonStrT v
              | Enum.Num v -> SingletonNumT v
              | Enum.Bool v -> SingletonBoolT v
              | Enum.Void -> VoidT
              | Enum.Null -> NullT in
            match UnionRep.quick_mem_enum (DefT (r, def)) rep with
            | UnionRep.No -> ()  (* provably unreachable, so prune *)
            | UnionRep.Yes -> rec_flow_t cx trace (l, result)
            | UnionRep.Conditional _ | UnionRep.Unknown -> (* inconclusive: the union is not concretized *)
              UnionRep.members rep |> List.iter (fun t -> rec_flow cx trace (t,u))
          end
        | Enum.Many enums ->
          let acc = EnumSet.fold (fun enum acc ->
            let def = match enum with
              | Enum.Str v -> SingletonStrT v
              | Enum.Num v -> SingletonNumT v
              | Enum.Bool v -> SingletonBoolT v
              | Enum.Void -> VoidT
              | Enum.Null -> NullT in
            UnionRep.join_quick_mem_results (acc, UnionRep.quick_mem_enum (DefT (r, def)) rep)
          ) enums UnionRep.No in
          begin match acc with
            | UnionRep.No -> ()  (* provably unreachable, so prune *)
            | UnionRep.Yes -> rec_flow_t cx trace (l, result)
            | UnionRep.Conditional _ | UnionRep.Unknown -> (* inconclusive: the union is not concretized *)
              UnionRep.members rep |> List.iter (fun t -> rec_flow cx trace (t,u))
          end
      else
        (* for l.key !== sentinel where l.key is a union, we can't really prove
           that the check is guaranteed to fail (assuming the union doesn't
           degenerate to a singleton) *)
        rec_flow_t cx trace (l, result)

    | DefT (_, UnionT rep), _
      when (match u with
        (* For l.key !== sentinel when sentinel has a union type, don't split the union. This
           prevents a drastic blowup of cases which can cause perf problems. *)
        | PredicateT (RightP (SentinelProp _, _), _)
        | PredicateT (NotP (RightP (SentinelProp _, _)), _) -> false
        | _ -> true
      ) ->
      UnionRep.members rep |> List.iter (fun t -> rec_flow cx trace (t,u))

    | _, UseT (use_op, DefT (_, IntersectionT rep)) ->
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

    | DefT (_, IntersectionT rep), UseT (_, u)
      when List.mem u (InterRep.members rep) ->
      ()

    | _, UseT (_, DefT (_, UnionT rep)) when
        let ts = Type_mapper.union_flatten cx @@ UnionRep.members rep in
        List.exists (TypeUtil.quick_subtype l) ts ->
      ()

    | _, UseT (use_op, DefT (r, UnionT rep)) ->
      (* Try the branches of the union in turn, with the goal of selecting the correct branch. This
         process is reused for intersections as well. See comments on try_union and
         try_intersection. *)
      try_union cx trace use_op l r rep

    (* maybe and optional types are just special union types *)

    | t1, UseT (use_op, DefT (_, MaybeT t2)) ->
      rec_flow cx trace (t1, UseT (use_op, t2))

    | t1, UseT (use_op, DefT (_, OptionalT t2)) ->
      rec_flow cx trace (t1, UseT (use_op, t2))

    (** special treatment for some operations on intersections: these
        rules fire for particular UBs whose constraints can (or must)
        be resolved against intersection LBs as a whole, instead of
        by decomposing the intersection into its parts.
      *)

    (** lookup of properties **)
    | DefT (_, IntersectionT rep),
      LookupT (reason, strict, try_ts_on_failure, s, t) ->
      let ts = InterRep.members rep in
      assert (ts <> []);
      (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
      rec_flow cx trace
        (List.hd ts,
         LookupT (reason, strict, (List.tl ts) @ try_ts_on_failure, s, t))

    | DefT (_, IntersectionT _), TestPropT (reason, _, prop, tout) ->
      rec_flow cx trace (l, GetPropT (unknown_use, reason, prop, tout))

    (** extends **)
    | DefT (_, IntersectionT rep),
      ExtendsUseT (use_op, reason, try_ts_on_failure, l, u) ->
      let t, ts = InterRep.members_nel rep in
      let try_ts_on_failure = (Nel.to_list ts) @ try_ts_on_failure in
      (* Since s could be in any object type in the list ts, we try to look it
         up in the first element of ts, pushing the rest into the list
         try_ts_on_failure (see below). *)
      rec_flow cx trace (t, ExtendsUseT (use_op, reason, try_ts_on_failure, l, u))

    (** consistent override of properties **)
    | DefT (_, IntersectionT rep), SuperT (use_op, reason, derived) ->
      InterRep.members rep |> List.iter (fun t ->
        let u = match use_op with
        | Op (ClassExtendsCheck c) ->
          let use_op = Op (ClassExtendsCheck { c with extends = reason_of_t t }) in
          SuperT (use_op, reason, derived)
        | _ ->
          u
        in
        rec_flow cx trace (t, u))

    (** structural subtype multiple inheritance **)
    | DefT (_, IntersectionT rep), ImplementsT (use_op, this) ->
      InterRep.members rep |> List.iter (fun t ->
        let u = match use_op with
        | Op (ClassImplementsCheck c) ->
          let use_op = Op (ClassImplementsCheck { c with implements = reason_of_t t }) in
          ImplementsT (use_op, this)
        | _ ->
          u
        in
        rec_flow cx trace (t, u))

    (** object types: an intersection may satisfy an object UB without
        any particular member of the intersection doing so completely.
        Here we trap object UBs with more than one property, and
        decompose them into singletons.
        Note: should be able to do this with LookupT rather than
        slices, but that approach behaves in nonobvious ways. TODO why?
      *)
    | DefT (_, IntersectionT _),
      UseT (use_op, DefT (r, ObjT { flags; props_tmap; proto_t; dict_t; call_t }))
      when SMap.cardinal (Context.find_props cx props_tmap) > 1 ->
      iter_real_props cx props_tmap (fun ~is_sentinel:_ x p ->
        let pmap = SMap.singleton x p in
        let id = Context.make_property_map cx pmap in
        let obj = mk_objecttype ~flags ~dict:dict_t ~call:call_t id dummy_prototype in
        rec_flow cx trace (l, UseT (use_op, DefT (r, ObjT obj)))
      );
      rec_flow cx trace (l, UseT (use_op, proto_t))

    (** predicates: prevent a predicate upper bound from prematurely decomposing
        an intersection lower bound *)
    | DefT (_, IntersectionT _), PredicateT (pred, tout) ->
      predicate cx trace tout l pred

    (* same for guards *)
    | DefT (_, IntersectionT _), GuardT (pred, result, tout) ->
      guard cx trace l pred result tout

    (** ObjAssignFromT copies multiple properties from its incoming LB.
        Here we simulate a merged object type by iterating over the
        entire intersection. *)
    | DefT (_, IntersectionT rep),
      ObjAssignFromT (reason_op, proto, tout, kind) ->
      let tvar = List.fold_left (fun tout t ->
        let tvar = match Cache.Fix.find reason_op t with
        | Some tvar -> tvar
        | None ->
          Tvar.mk_where cx reason_op (fun tvar ->
            Cache.Fix.add reason_op t tvar;
            rec_flow cx trace (t, ObjAssignFromT (reason_op, proto, tvar, kind))
          )
        in
        rec_flow_t cx trace (tvar, tout);
        tvar
      ) (Tvar.mk cx reason_op) (InterRep.members rep) in
      rec_flow_t cx trace (tvar, tout)

    (** This duplicates the (_, ReposLowerT u) near the end of this pattern
        match but has to appear here to preempt the (IntersectionT, _) in
        between so that we reposition the entire intersection. *)
    | DefT (_, IntersectionT _), ReposLowerT (reason, use_desc, u) ->
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

    | DefT (_, IntersectionT _), ObjKitT (use_op, reason, resolve_tool, tool, tout) ->
      object_kit cx trace ~use_op reason resolve_tool tool tout l

    (* CallT uses that arise from the CallType type destructor are processed
       without preparation (see below). This is because in these cases, the
       return type is intended to be 0-1, whereas preparation (as implemented
       currently) destroys 0-1 behavior. *)
    | DefT (r, IntersectionT rep), CallT (_, reason, _) when is_calltype_reason reason ->
      try_intersection cx trace u r rep

    (** All other pairs with an intersection lower bound come here. Before
        further processing, we ensure that the upper bound is concretized. See
        prep_try_intersection for details. **)

    (* (After the above preprocessing step, try the branches of the intersection
       in turn, with the goal of selecting the correct branch. This process is
       reused for unions as well. See comments on try_union and
       try_intersection.)  *)

    | DefT (r, IntersectionT rep), u ->
      prep_try_intersection cx trace
        (reason_of_use_t u) (parts_to_replace u) [] u r rep

    (************)
    (* matching *)
    (************)

    | MatchingPropT (reason, x, t), UseT (_, l) ->
      (* Things that can have properties are object-like (objects, instances,
         and their exact versions). Notably, "meta" types like union, annot,
         typeapp, eval, maybe, optional, and intersection should have boiled
         away by this point. *)
      let propref = Named (reason, x) in
      let strict = NonstrictReturning (None, None) in
      let u = LookupT (reason, strict, [], propref, MatchProp t) in
      rec_flow cx trace (l, u)

    | MatchingPropT _, _ when is_use u ->
      () (* TODO: empty? *)

    (*************************)
    (* Resolving rest params *)
    (*************************)

    (* `any` is obviously fine as a spread element. `Object` is fine because
     * any Iterable can be spread, and `Object` is the any type that covers
     * iterable objects. *)
    | DefT (r, (AnyT | AnyObjT)),
      ResolveSpreadT (use_op, reason_op, {
        rrt_resolved;
        rrt_unresolved;
        rrt_resolve_to;
      }) ->

      let rrt_resolved = (ResolvedAnySpreadArg r)::rrt_resolved in
      resolve_spread_list_rec
        cx ~trace ~use_op ~reason_op
        (rrt_resolved, rrt_unresolved) rrt_resolve_to

    | _,
      ResolveSpreadT (use_op, reason_op, {
        rrt_resolved;
        rrt_unresolved;
        rrt_resolve_to;
      }) ->
      let reason = reason_of_t l in

      let r, arrtype = match l with
      | DefT (r, ArrT arrtype) ->
        (* Arrays *)
        r, arrtype
      | _ ->
        (* Non-array non-any iterables *)
        let reason = reason_of_t l in
        let element_tvar = Tvar.mk cx reason in
        let iterable =
          let targs = [element_tvar; AnyT.why reason; AnyT.why reason] in
          get_builtin_typeapp cx
            (replace_reason_const (RCustom "Iterable expected for spread") reason)
            "$Iterable" targs
        in
        flow_t cx (l, iterable);
        reason, ArrayAT (element_tvar, None)
      in

      let elemt = elemt_of_arrtype r arrtype in

      begin match rrt_resolve_to with
      (* Any ResolveSpreadsTo* which does some sort of constant folding needs to
       * carry an id around to break the infinite recursion that constant
       * constant folding can trigger *)
      | ResolveSpreadsToTuple (id, elem_t, tout)
      | ResolveSpreadsToArrayLiteral (id, elem_t, tout) ->
        (* You might come across code like
         *
         * for (let x = 1; x < 3; x++) { foo = [...foo, x]; }
         *
         * where every time you spread foo, you flow another type into foo. So
         * each time `l ~> ResolveSpreadT` is processed, it might produce a new
         * `l ~> ResolveSpreadT` with a new `l`.
         *
         * Here is how we avoid this:
         *
         * 1. We use ConstFoldExpansion to detect when we see a ResolveSpreadT
         *    upper bound multiple times
         * 2. When a ResolveSpreadT upper bound multiple times, we change it into
         *    a ResolveSpreadT upper bound that resolves to a more general type.
         *    This should prevent more distinct lower bounds from flowing in
         * 3. rec_flow caches (l,u) pairs.
         *)


        let reason_elemt = reason_of_t elemt in
        ConstFoldExpansion.guard id reason_elemt (fun recursion_depth ->
          match recursion_depth with
          | 0 ->
            (* The first time we see this, we process it normally *)
            let rrt_resolved =
              ResolvedSpreadArg(reason, arrtype)::rrt_resolved in
            resolve_spread_list_rec
              cx ~trace ~use_op ~reason_op
              (rrt_resolved, rrt_unresolved) rrt_resolve_to
          | 1 ->
            (* To avoid infinite recursion, let's deconstruct to a simpler case
             * where we no longer resolve to a tuple but instead just resolve to
             * an array. *)
            rec_flow cx trace (l, ResolveSpreadT (use_op, reason_op, {
              rrt_resolved;
              rrt_unresolved;
              rrt_resolve_to = ResolveSpreadsToArray (elem_t, tout);
            }))
          | _ ->
            (* We've already deconstructed, so there's nothing left to do *)
            ()
        )

      | ResolveSpreadsToMultiflowCallFull (id, _)
      | ResolveSpreadsToMultiflowSubtypeFull (id, _)
      | ResolveSpreadsToCustomFunCall (id, _, _)
      | ResolveSpreadsToMultiflowPartial (id, _, _, _) ->
        let reason_elemt = reason_of_t elemt in
        ConstFoldExpansion.guard id reason_elemt (fun recursion_depth ->
          match recursion_depth with
          | 0 ->
            (* The first time we see this, we process it normally *)
            let rrt_resolved =
              ResolvedSpreadArg(reason, arrtype)::rrt_resolved in
            resolve_spread_list_rec
              cx ~trace ~use_op ~reason_op
              (rrt_resolved, rrt_unresolved) rrt_resolve_to
          | 1 ->
            (* Consider
             *
             * function foo(...args) { foo(1, ...args); }
             * foo();
             *
             * Because args is unannotated, we try to infer it. However, due to
             * the constant folding we do with spread arguments, we'll first
             * infer that it is [], then [] | [1], then [] | [1] | [1,1] ...etc
             *
             * We can recognize that we're stuck in a constant folding loop. But
             * how to break it?
             *
             * In this case, we are constant folding by recognizing when args is
             * a tuple or an array literal. We can break the loop by turning
             * tuples or array literals into simple arrays.
             *)

            let new_arrtype = match arrtype with
            (* These can get us into constant folding loops *)
            | ArrayAT (elemt, Some _)
            | TupleAT (elemt, _) -> ArrayAT (elemt, None)
            (* These cannot *)
            | ArrayAT (_, None)
            | ROArrayAT _
            | EmptyAT -> arrtype in

            let rrt_resolved =
             ResolvedSpreadArg(reason, new_arrtype)::rrt_resolved in
            resolve_spread_list_rec
             cx ~trace ~use_op ~reason_op
             (rrt_resolved, rrt_unresolved) rrt_resolve_to
          | _ -> ()
        )

      (* no caching *)
      | ResolveSpreadsToArray _
      | ResolveSpreadsToCallT _
        ->
        let rrt_resolved = ResolvedSpreadArg(reason, arrtype)::rrt_resolved in
        resolve_spread_list_rec
          cx ~trace ~use_op ~reason_op
          (rrt_resolved, rrt_unresolved) rrt_resolve_to
      end

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

    | DefT (_, (SingletonStrT _ | SingletonNumT _ | SingletonBoolT _)),
      ReposLowerT (reason, use_desc, u) ->
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

    | DefT (reason, SingletonStrT key), _ ->
      rec_flow cx trace (DefT (reason, StrT (Literal (None, key))), u)

    | DefT (reason, SingletonNumT lit), _ ->
      rec_flow cx trace (DefT (reason, NumT (Literal (None, lit))), u)

    | DefT (reason, SingletonBoolT b), _ ->
      rec_flow cx trace (DefT (reason, BoolT (Some b)), u)

    (* NullProtoT is necessary as an upper bound, to distinguish between
       (ObjT _, NullProtoT _) constraints and (ObjT _, DefT (_, NullT)), but as
       a lower bound, it's the same as DefT (_, NullT) *)
    | NullProtoT reason, _ ->
      rec_flow cx trace (DefT (reason, NullT), u)

    (************************************************************************)
    (* exact object types *)
    (************************************************************************)

    (* ExactT<X> comes from annotation, may behave as LB or UB *)

    (* when $Exact<LB> ~> UB, forward to MakeExactT *)
    | ExactT (r, t), _ ->
      rec_flow cx trace (t, MakeExactT (r, Upper u))

    (* ObjT LB ~> $Exact<UB>. make exact if exact and unsealed *)
    | DefT (_, ObjT { flags; _ }), UseT (use_op, ExactT (r, t)) ->
      if flags.exact && (Obj_type.sealed_in_op r flags.sealed)
      then rec_flow cx trace (t, MakeExactT (r, Lower (use_op, l)))
      else begin
        let reasons = FlowError.ordered_reasons (reason_of_t l, r) in
        add_output cx ~trace (FlowError.EIncompatibleWithExact (reasons, use_op));
        (* Continue the Flow even after we've errored. Often, there is more that
         * is different then just the fact that the upper bound is exact and the
         * lower bound is not. This could easily hide errors in ObjT ~> ExactT *)
        rec_flow cx trace (l, UseT (use_op, t))
      end

    (* any specializations ~> $Exact<UB>. unwrap exact *)
    | DefT (_, AnyObjT), UseT (use_op, ExactT (_, t))
    | DefT (_, AnyFunT), UseT (use_op, ExactT (_, t)) ->
      rec_flow cx trace (l, UseT (use_op, t))

    (* inexact LB ~> $Exact<UB>. error *)
    | _, UseT (use_op, ExactT (ru, _)) ->
      let reasons = FlowError.ordered_reasons (reason_of_t l, ru) in
      add_output cx ~trace (FlowError.EIncompatibleWithExact (reasons, use_op))

    (* LB ~> MakeExactT (_, UB) exactifies LB, then flows result to UB *)

    (* exactify incoming LB object type, flow to UB *)
    | DefT (r, ObjT obj), MakeExactT (_, Upper u) ->
      let exactobj = { obj with flags = { obj.flags with exact = true } } in
      rec_flow cx trace (DefT (r, ObjT exactobj), u)

    (* exactify incoming UB object type, flow to LB *)
    | DefT (ru, ObjT obj_u), MakeExactT (reason_op, Lower (use_op, l)) ->
      (* forward to standard obj ~> obj *)
      let ru = repos_reason (loc_of_reason reason_op) ru in
      let xu = { obj_u with flags = { obj_u.flags with exact = true } } in
      rec_flow cx trace (l, UseT (use_op, DefT (ru, ObjT xu)))

    | DefT (_, AnyT), MakeExactT (reason_op, k) ->
      continue cx trace (AnyT.why reason_op) k

    | DefT (_, VoidT), MakeExactT (reason_op, k) ->
      continue cx trace (VoidT.why reason_op) k

    | DefT (_, EmptyT), MakeExactT (reason_op, k) ->
      continue cx trace (EmptyT.why reason_op) k

    (* unsupported kind *)
    | _, MakeExactT (ru, _) ->
      add_output cx ~trace (FlowError.EUnsupportedExact (ru, reason_of_t l))

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

    | DefT (_, NullT), TestPropT (reason_op, _, propref, tout) ->
      (* The wildcard TestPropT implementation forwards the lower bound to
         LookupT. This is unfortunate, because LookupT is designed to terminate
         (successfully) on NullT, but property accesses on null should be type
         errors. Ideally, we should prevent LookupT constraints from being
         syntax-driven, in order to preserve the delicate invariants that
         surround it. *)
      rec_flow cx trace (l, GetPropT (unknown_use, reason_op, propref, tout))

    | _, TestPropT (reason_op, id, propref, tout) ->
      (* NonstrictReturning lookups unify their result, but we don't want to
         unify with the tout tvar directly, so we create an indirection here to
         ensure we only supply lower bounds to tout. *)
      let lookup_default = Tvar.mk_where cx reason_op (fun tvar ->
        rec_flow_t cx trace (tvar, tout)
      ) in
      let name = name_of_propref propref in
      let test_info = Some (id, (reason_op, reason_of_t l)) in
      let lookup_default = match l with
        | DefT (_, ObjT { flags; _ })
            when flags.exact ->
          if Obj_type.sealed_in_op reason_op flags.sealed then
            let r = replace_reason_const (RMissingProperty name) reason_op in
            Some (DefT (r, VoidT), lookup_default)
          else
            (* This is an unsealed object. We don't now when (or even if) this
             * property access will resolve, since reads and writes can happen
             * in any order.
             *
             * Due to this, we never error on property accesses. TODO: Build a
             * separate mechanism unsealed objects that errors after merge if a
             * shadow prop is read but never written.
             *
             * We also should not return a default type on lookup failure,
             * because a later write could make the lookup succeed.
             *)
            let () = Context.test_prop_hit cx id in
            None
        | _ ->
          (* Note: a lot of other types could in principle be considered
             "exact". For example, new instances of classes could have exact
             types; so could `super` references (since they are statically
             rather than dynamically bound). However, currently we don't support
             any other exact types. Considering exact types inexact is sound, so
             there is no problem falling back to the same conservative
             approximation we use for inexact types in those cases. *)
          let r = replace_reason_const (RUnknownProperty name) reason_op in
          Some (DefT (r, MixedT Mixed_everything), lookup_default)
      in
      let lookup_kind = NonstrictReturning (lookup_default, test_info) in
      rec_flow cx trace (l,
        LookupT (reason_op, lookup_kind, [], propref,
          RWProp (unknown_use, l, tout, Read)))


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
       parameters, we can arrive to the actual name (Key.t) of the parameter
       that gets refined, which can be used as a key into the `OpenPredT` that
       is expected to eventually flow to `return_t`.  Effectively, we are
       substituting the actual parameter to the refining call (here in the form
       of the index of the argument to the call) to the formal parameter of the
       function, and this information is stored in `CallOpenPredT` of the
       produced flow.

       Problematic cases (e.g. when the refining index is out of bounds w.r.t.
       `params`) raise errors, but also propagate the unrefined types (as if the
       refinement never took place).
    *)
    | DefT (lreason, FunT (_, _, {
        params;
        return_t;
        is_predicate = true;
        _
      })),
      CallLatentPredT (reason, sense, index, unrefined_t, fresh_t) ->
      (* TODO: for the moment we only support simple keys (empty projection)
         that exactly correspond to the function's parameters *)
      let name_or_err = try
        let (name, _) = List.nth params (index-1) in
        Ok name
      with
        | Invalid_argument _ ->
          Error ("Negative refinement index.",
            (lreason, reason))
        | Failure msg when msg = "nth" ->
          let r1 = replace_reason (fun desc -> RCustom (
            spf "%s that uses predicate on parameter at position %d"
              (string_of_desc desc)
              index
          )) reason in
          let r2 = replace_reason (fun desc -> RCustom (
            spf "%s with %d parameters"
              (string_of_desc desc)
              (List.length params)
          )) lreason in
          Error ("This is incompatible with", (r1, r2))
      in
      (match name_or_err with
      | Ok (Some name) ->
        let key = name, [] in
        rec_flow cx trace
          (return_t, CallOpenPredT (reason, sense, key, unrefined_t, fresh_t))
      | Ok None ->
        let loc = loc_of_reason lreason in
        add_output cx ~trace FlowError.(EInternal
          (loc, PredFunWithoutParamNames))
      | Error (msg, reasons) ->
        add_output cx ~trace (FlowError.EFunPredCustom (reasons, msg));
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
      add_output cx ~trace FlowError.(EInternal (loc, OpenPredWithoutSubst))

    (*********************************************)
    (* Using predicate functions as regular ones *)
    (*********************************************)

    | OpenPredT (_, l, _, _), _ -> rec_flow cx trace (l, u)

    (********************)
    (* mixin conversion *)
    (********************)

    (* A class can be viewed as a mixin by extracting its immediate properties,
       and "erasing" its static and super *)

    | ThisClassT (_, DefT (_, InstanceT (_, _, _, instance))), MixinT (r, tvar) ->
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      rec_flow cx trace (
        this_class_type (DefT (r, InstanceT (static, super, [], instance))),
        UseT (unknown_use, tvar)
      )

    | DefT (_, PolyT (xs, ThisClassT (_, DefT (_, InstanceT (_, _, _, insttype))), _)),
      MixinT (r, tvar) ->
      let static = ObjProtoT r in
      let super = ObjProtoT r in
      let instance = DefT (r, InstanceT (static, super, [], insttype)) in
      rec_flow cx trace (
        poly_type (Context.make_nominal cx) xs (this_class_type instance),
        UseT (unknown_use, tvar)
      )

    | DefT (_, AnyT), MixinT (r, tvar) ->
      rec_flow_t cx trace (AnyT.why r, tvar)

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

    | DefT (_, PolyT (xs,t,id)), SpecializeT(use_op,reason_op,reason_tapp,cache,ts,tvar) ->
      let ts = Option.value ts ~default:[] in
      let t_ = mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache id xs t ts in
      rec_flow_t cx trace (t_, tvar)

    | DefT (_, PolyT (tps, _, _)), VarianceCheckT(_, ts, polarity) ->
      variance_check cx ~trace polarity (tps, ts)

    (* When we are checking the polarity of a super class where the super class has no type
       args, we end up generating this constraint. Since it has no type args, we never resolve to
       a PolyT, but we still want to check the polarity in this case. *)
    | DefT (_, ClassT _), VarianceCheckT(_, [], polarity) ->
        check_polarity cx ~trace polarity l

    | DefT (_, PolyT (tparams, _, _)),
      TypeAppVarianceCheckT (use_op, reason_op, reason_tapp, targs) ->
      let minimum_arity = poly_minimum_arity tparams in
      let maximum_arity = List.length tparams in
      let reason_arity =
        let tp1, tpN = List.hd tparams, List.hd (List.rev tparams) in
        let loc = Loc.btwn (loc_of_reason tp1.reason) (loc_of_reason tpN.reason) in
        mk_reason (RCustom "See type parameters of definition here") loc in
      if List.length targs > maximum_arity then (
        add_output cx ~trace
          (FlowError.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity));
      ) else (
        let unused_targs = List.fold_left (fun targs { name; default; polarity; reason; _ } ->
          match default, targs with
          | None, [] ->
            (* fewer arguments than params but no default *)
            add_output cx ~trace (FlowError.ETooFewTypeArgs
              (reason_tapp, reason_arity, minimum_arity));
            []
          | _, [] -> []
          | _, (t1, t2)::targs ->
            let use_op = Frame (TypeArgCompatibility {
              name;
              targ = reason;
              lower = reason_op;
              upper = reason_tapp;
              polarity;
            }, use_op) in
            (match polarity with
            | Positive -> rec_flow cx trace (t1, UseT (use_op, t2))
            | Negative -> rec_flow cx trace (t2, UseT (use_op, t1))
            | Neutral -> rec_unify cx trace ~use_op t1 t2);
            targs
        ) targs tparams in
        assert (unused_targs = []);
      )

    (* empty targs specialization of non-polymorphic classes is a no-op *)
    | (DefT (_, ClassT _) | ThisClassT _), SpecializeT(_,_,_,_,None,tvar) ->
      rec_flow_t cx trace (l, tvar)

    | DefT (_, AnyT), SpecializeT (_, _, _, _, _, tvar) ->
      rec_flow_t cx trace (l, tvar)

    (* this-specialize a this-abstracted class by substituting This *)
    | ThisClassT (reason, i), ThisSpecializeT(_, this, tvar) ->
      let i = subst cx (SMap.singleton "this" this) i in
      rec_flow_t cx trace (DefT (reason, ClassT i), tvar)

    (* this-specialization of non-this-abstracted classes is a no-op *)
    | DefT (r, ClassT i), ThisSpecializeT(_, _this, tvar) ->
      (* TODO: check that this is a subtype of i? *)
      rec_flow_t cx trace (DefT (r, ClassT i), tvar)

    | DefT (_, AnyT), ThisSpecializeT (_, _, tvar) ->
      rec_flow_t cx trace (l, tvar)

    | DefT (_, PolyT _), ReposLowerT (reason, use_desc, u) ->
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

    | (ThisClassT _, ReposLowerT (reason, use_desc, u)) ->
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

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

    (** some shortcuts **)
    | DefT (_, PolyT (_, _, id1)), UseT (_, DefT (_, PolyT (_, _, id2)))
      when id1 = id2 -> ()

    | DefT (r1, PolyT (params1, t1, id1)), UseT (use_op, DefT (r2, PolyT (params2, t2, id2))) ->
      let n1 = List.length params1 in
      let n2 = List.length params2 in
      if n2 > n1 then
        add_output cx ~trace (FlowError.ETooManyTypeArgs (r2, r1, n1))
      else if n2 < n1 then
        add_output cx ~trace (FlowError.ETooFewTypeArgs (r2, r1, n1))
      else
        (** for equal-arity polymorphic types, flow param upper bounds, then instances parameterized
            by these *)
        let args1 = instantiate_poly_param_upper_bounds cx params1 in
        let args2 = instantiate_poly_param_upper_bounds cx params2 in
        List.iter2 (fun arg1 arg2 -> rec_flow_t cx trace ~use_op (arg2, arg1)) args1 args2;
        let inst1 =
          let r = reason_of_t t1 in
          mk_typeapp_of_poly cx trace
            ~use_op ~reason_op:r ~reason_tapp:r id1 params1 t1 args1 in
        let inst2 =
          let r = reason_of_t t2 in
          mk_typeapp_of_poly cx trace
            ~use_op ~reason_op:r ~reason_tapp:r id2 params2 t2 args2 in
        rec_flow_t cx trace (inst1, inst2)

    (** general case **)
    | _, UseT (use_op, DefT (_, PolyT (ids, t, _))) ->
        generate_tests cx ids (fun map_ ->
          rec_flow cx trace (l, UseT (use_op, subst cx ~use_op map_ t))
        )

    (* TODO: ideally we'd do the same when lower bounds flow to a
       this-abstracted class, but fixing the class is easier; might need to
       revisit *)
    | (_, UseT (use_op, ThisClassT (r, i))) ->
      let reason = reason_of_t l in
      rec_flow cx trace (l, UseT (use_op, fix_this_class cx trace reason (r, i)))

    (** This rule is hit when a polymorphic type appears outside a
        type application expression - i.e. not followed by a type argument list
        delimited by angle brackets.
        We want to require full expressions in type positions like annotations,
        but allow use of polymorphically-typed values - for example, in class
        extends clauses and at function call sites - without explicit type
        arguments, since typically they're easily inferred from context.
      *)
    | DefT (reason_tapp, (PolyT (ids, t, _))), _ ->
      let reason_op = reason_of_use_t u in
      begin match u with
      | UseT (use_op, DefT (_, TypeT _)) ->
        ignore use_op; (* TODO: add use op to missing type arg error? *)
        add_output cx ~trace (FlowError.EMissingTypeArgs {
          reason_tapp = reason_tapp;
          reason_arity = mk_poly_arity_reason ids;
          min_arity = poly_minimum_arity ids;
          max_arity = List.length ids;
        })
      (* Special case for `_ instanceof C` where C is polymorphic *)
      | PredicateT ((RightP (InstanceofTest, _) | NotP (RightP (InstanceofTest, _))), _) ->
        let l = instantiate_poly_default_args cx trace
          ~use_op:unknown_use ~reason_op ~reason_tapp (ids, t) in
        rec_flow cx trace (l, u)
      (* Special case for React.PropTypes.instanceOf arguments, which are an
         exception to type arg arity strictness, because it's not possible to
         provide args and we need to interpret the value as a type. *)
      | ReactKitT (use_op, reason_op, (React.SimplifyPropType
          (React.SimplifyPropType.InstanceOf, _) as tool)) ->
        let l = instantiate_poly_default_args cx trace
          ~use_op ~reason_op ~reason_tapp (ids, t) in
        react_kit cx trace ~use_op reason_op l tool
      (* Calls to polymorphic functions may cause non-termination, e.g. when the
         results of the calls feed back as subtle variations of the original
         arguments. This is similar to how we may have non-termination with
         method calls on type applications. Thus, it makes sense to replicate
         the specialization caching mechanism used in TypeAppT ~> MethodT to
         avoid non-termination in PolyT ~> CallT.

         As it turns out, we need a bit more work here. A call may invoke
         different cases of an overloaded polymorphic function on different
         arguments, so we use the reasons of arguments in addition to the reason
         of the call as keys for caching instantiations.

         On the other hand, even the reasons of arguments may not offer sufficient
         distinguishing power when the arguments have not been concretized:
         differently typed arguments could be incorrectly summarized by common
         type variables they flow to, causing spurious errors. In particular, we
         don't cache calls involved in the execution of mapped type operations
         ($TupleMap, $ObjectMap, $ObjectMapi) to avoid this problem.

         NOTE: This is probably not the final word on non-termination with
         generics. We need to separate the double duty of reasons in the current
         implementation as error positions and as caching keys. As error
         positions we should be able to subject reasons to arbitrary tweaking,
         without fearing regressions in termination guarantees.
      *)
      | CallT (use_op, _, calltype) when not (is_typemap_reason reason_op) ->
        begin match calltype.call_targs with
        | None ->
          let arg_reasons = List.map (function
            | Arg t -> reason_of_t t
            | SpreadArg t -> reason_of_t t
          ) calltype.call_args_tlist in
          let t_ = instantiate_poly cx trace
            ~use_op ~reason_op ~reason_tapp ~cache:arg_reasons (ids,t) in
          rec_flow cx trace (t_, u)
        | Some targs ->
          let t_ = instantiate_poly_with_targs cx trace (ids, t) targs
            ~use_op ~reason_op ~reason_tapp in
          rec_flow cx trace (t_,
            CallT (use_op, reason_op, {calltype with call_targs = None}))
        end
      | ConstructorT (use_op, reason_op, Some targs, args, tout) ->
        let t_ = instantiate_poly_with_targs cx trace (ids, t) targs
          ~use_op ~reason_op ~reason_tapp in
        rec_flow cx trace (t_, ConstructorT (use_op, reason_op, None, args, tout))
      | _ ->
        let t_ = instantiate_poly cx trace
          ~use_op:unknown_use ~reason_op ~reason_tapp (ids,t) in
        rec_flow cx trace (t_, u)
      end

    (* when a this-abstracted class flows to upper bounds, fix the class *)
    | (ThisClassT (r, i), _) ->
      let reason = reason_of_use_t u in
      rec_flow cx trace (fix_this_class cx trace reason (r, i), u)

    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    (* FunT ~> FunT *)

    | DefT (lreason, FunT (_, _, ft1)),
      UseT (use_op, DefT (ureason, FunT (_, _, ft2))) ->
      let use_op = Frame (
        FunCompatibility { lower = lreason; upper = ureason },
        (* The $call PropertyCompatibility is redundant when we have a
         * FunCompatibility use_op. *)
        match use_op with
        | Frame (PropertyCompatibility {prop = Some "$call"; _}, use_op) -> use_op
        | _ -> use_op
      ) in
      rec_flow cx trace (ft2.this_t, UseT (use_op, ft1.this_t));
      let args = List.rev_map (fun (_, t) -> Arg t) ft2.params in
      let args = List.rev (match ft2.rest_param with
      | Some (_, _, rest) -> (SpreadArg rest) :: args
      | None -> args) in
      multiflow_subtype cx trace ~use_op ureason args ft1;

      (* Well-formedness adjustment: If this is predicate function subtyping,
         make sure to apply a latent substitution on the right-hand use to
         bridge the mismatch of the parameter naming. Otherwise, proceed with
         the subtyping of the return types normally. In general it should
         hold as an invariant that OpenPredTs (where free variables appear)
         should not flow to other OpenPredTs without wrapping the latter in
         SubstOnPredT.
      *)
      if ft2.is_predicate then
        if not ft1.is_predicate then
          (* Non-predicate functions are incompatible with predicate ones
             TODO: somehow the original flow needs to be propagated as well *)
          add_output cx ~trace (FlowError.EFunPredCustom (
            (lreason, ureason),
            "Function is incompatible with"))
        else
          let reason = replace_reason (fun desc ->
            RCustom (spf "predicate of %s" (string_of_desc desc))
          ) (reason_of_t ft2.return_t) in
          let rec subst_map (n, map) = function
            | (Some k, _)::ps1, (Some v, _)::ps2 ->
              subst_map (n+1, SMap.add k (v,[]) map) (ps1, ps2)
            | _, [] -> Ok map
            | [], ps2 ->
              (* Flag an error if predicate counts do not coincide
                 TODO: somehow the original flow needs to be propagated
                 as well *)
              let mod_reason n = replace_reason (fun _ ->
                RCustom (spf "predicate function with %d arguments" n)
              ) in
              let n2 = n + (List.length ps2) in
              Error (FlowError.EFunPredCustom (
                (mod_reason n lreason,
                 mod_reason n2 ureason),
                "Predicate function is incompatible with"))
            | (None, _)::_, _ | _, (None, _)::_ ->
              let loc = loc_of_reason ureason in
              Error (FlowError.(EInternal (loc, PredFunWithoutParamNames)))
          in
          match subst_map (0, SMap.empty) (ft1.params, ft2.params) with
          | Error e -> add_output cx ~trace e
          | Ok map ->
            rec_flow cx trace (ft1.return_t,
              SubstOnPredT (reason, map, ft2.return_t))
      else (
        let use_op = Frame (FunReturn {
          lower = reason_of_t ft1.return_t;
          upper = reason_of_t ft2.return_t;
        }, use_op) in
        rec_flow cx trace (ft1.return_t, UseT (use_op, ft2.return_t))
      )

    (* FunT ~> CallT *)

    | DefT (reason_fundef, FunT (_, _, funtype)),
      CallT (use_op, reason_callsite, calltype) ->
      let {
        this_t = o1;
        params = _;
        return_t = t1;
        closure_t = func_scope_id;
        changeset; _
      } = funtype in
      let {
        call_this_t = o2;
        call_targs;
        call_args_tlist = tins2;
        call_tout = t2;
        call_closure_t = call_scope_id;
        call_strict_arity
      } = calltype in

      rec_flow cx trace (o2, UseT (use_op, o1));

      Option.iter call_targs ~f:(fun _ ->
        add_output cx ~trace FlowError.(ECallTypeArity {
          call_loc = loc_of_reason reason_callsite;
          is_new = false;
          reason_arity = reason_fundef;
          expected_arity = 0;
        }));

      if call_strict_arity
      then multiflow_call cx trace ~use_op reason_callsite tins2 funtype
      else multiflow_subtype cx trace ~use_op reason_callsite tins2 funtype;

      (* flow return type of function to the tvar holding the return type of the
         call. clears the op stack because the result of the call is not the
         call itself. *)
      rec_flow_t cx trace (
        reposition cx ~trace (loc_of_reason reason_callsite) t1,
        t2
      );

      (if Context.is_verbose cx then
        prerr_endlinef "%shavoc_call_env fundef %s callsite %s"
          (Context.pid_prefix cx)
          (Debug_js.string_of_reason cx reason_fundef)
          (Debug_js.string_of_reason cx reason_callsite));
      havoc_call_env cx func_scope_id call_scope_id changeset;

    | DefT (reason_fundef, (AnyFunT | AnyT)),
      CallT (use_op, reason_op, calltype) ->
      let {
        call_this_t;
        call_targs = _; (* An untyped receiver can't do anything with type args *)
        call_args_tlist;
        call_tout;
        call_closure_t = _;
        call_strict_arity = _;
      } = calltype in
      let any = AnyT.why reason_fundef in
      rec_flow_t cx trace (call_this_t, any);
      call_args_iter (fun t -> rec_flow cx trace (t, UseT (use_op, any))) call_args_tlist;
      rec_flow_t cx trace (AnyT.why reason_op, call_tout)

    (* Special handlers for builtin functions *)

    | CustomFunT (_, ObjectAssign),
      CallT (_, reason_op, { call_targs = None; call_args_tlist = dest_t::ts; call_tout; _ }) ->
      let dest_t = extract_non_spread cx ~trace dest_t in
      let t = chain_objects cx ~trace reason_op dest_t ts in
      rec_flow_t cx trace (t, call_tout)

    | CustomFunT (_, ObjectGetPrototypeOf),
      CallT (_, reason_op, { call_targs = None; call_args_tlist = arg::_; call_tout; _ }) ->
      let l = extract_non_spread cx ~trace arg in
      rec_flow cx trace (l, GetProtoT (reason_op, call_tout))

    | CustomFunT (_, ObjectSetPrototypeOf),
      CallT (_, reason_op, { call_targs = None; call_args_tlist = arg1::arg2::_; call_tout; _ }) ->
      let target = extract_non_spread cx ~trace arg1 in
      let proto = extract_non_spread cx ~trace arg2 in
      rec_flow cx trace (target, SetProtoT (reason_op, proto));
      rec_flow_t cx trace (BoolT.why reason_op, call_tout)

    | DefT (reason, StrT (Literal (_, str))),
      UseT (use_op, DefT (reason_op, CharSetT chars)) ->
        let module CharSet = String_utils.CharSet in
        let open Flow_error in
        let invalid, _ = String_utils.fold_left ~f:(fun (invalid, seen) chr ->
          if not (CharSet.mem chr chars) then
            InvalidCharSetSet.add (InvalidChar chr) invalid, seen
          else if CharSet.mem chr seen then
            InvalidCharSetSet.add (DuplicateChar chr) invalid, seen
          else
            invalid, CharSet.add chr seen
        ) ~acc:(InvalidCharSetSet.empty, CharSet.empty) str in
        if not (InvalidCharSetSet.is_empty invalid) then
          add_output cx ~trace (FlowError.EInvalidCharSet {
            invalid = (
              replace_reason_const ~keep_def_loc:true (RStringLit str) reason,
              invalid
            );
            valid = reason_op;
            use_op;
          })

    | DefT (reason, CharSetT _), _ ->
      rec_flow cx trace (StrT.why reason, u)

    | _, UseT (use_op, DefT (reason, CharSetT _)) ->
      rec_flow cx trace (l, UseT (use_op, StrT.why reason))

    (* React prop type functions are modeled as a custom function type in Flow,
       so that Flow can exploit the extra information to gratuitously hardcode
       best-effort static checking of dynamic prop type validation.

       A prop type is either a primitive or some complex type, which is a
       function that simplifies to a primitive prop type when called. *)

    | CustomFunT (_, ReactPropType (React.PropType.Primitive (false, t))),
      GetPropT (_, reason_op, Named (_, "isRequired"), tout) ->
      let prop_type = React.PropType.Primitive (true, t) in
      rec_flow_t cx trace (CustomFunT (reason_op, ReactPropType prop_type), tout)

    | CustomFunT (reason, ReactPropType (React.PropType.Primitive (req, _))), _
      when object_use u || function_use u || function_like_op u ->
      let builtin_name =
        if req
        then "ReactPropsCheckType"
        else "ReactPropsChainableTypeChecker"
      in
      let l = get_builtin_type cx ~trace reason builtin_name in
      rec_flow cx trace (l, u)

    | CustomFunT (_, ReactPropType React.PropType.Complex kind),
      CallT (_, reason_op, { call_targs = None; call_args_tlist = arg1::_; call_tout; _ }) ->
      let open React in
      let tool = match kind with
      | PropType.ArrayOf -> SimplifyPropType.ArrayOf
      | PropType.InstanceOf -> SimplifyPropType.InstanceOf
      | PropType.ObjectOf -> SimplifyPropType.ObjectOf
      | PropType.OneOf -> SimplifyPropType.OneOf ResolveArray
      | PropType.OneOfType -> SimplifyPropType.OneOfType ResolveArray
      | PropType.Shape -> SimplifyPropType.Shape ResolveObject
      in
      let t = extract_non_spread cx ~trace arg1 in
      rec_flow cx trace (t, ReactKitT (unknown_use, reason_op,
        SimplifyPropType (tool, call_tout)))

    | CustomFunT (reason, ReactPropType React.PropType.Complex kind), _
      when object_use u || function_use u || function_like_op u ->
      rec_flow cx trace (get_builtin_prop_type cx ~trace reason kind, u)

    | CustomFunT (_, ReactCreateClass),
      CallT (use_op, reason_op, { call_targs = None; call_args_tlist = arg1::_; call_tout; _ }) ->
      let loc_op = loc_of_reason reason_op in
      let spec = extract_non_spread cx ~trace arg1 in
      let mk_tvar f = Tvar.mk cx (f reason_op) in
      let knot = { React.CreateClass.
        this = mk_tvar (replace_reason_const RThisType);
        static = mk_tvar (replace_reason_const RThisType);
        state_t = mk_tvar (replace_reason (fun d -> RTypeParam ("State", d, loc_op)));
        default_t = mk_tvar (replace_reason (fun d -> RTypeParam ("Default", d, loc_op)));
      } in
      rec_flow cx trace (spec, ReactKitT (use_op, reason_op,
        React.CreateClass (React.CreateClass.Spec [], knot, call_tout)));

    | _, ReactKitT (use_op, reason_op, tool) ->
      react_kit cx trace ~use_op reason_op l tool

    (* Facebookisms are special Facebook-specific functions that are not
       expressable with our current type syntax, so we've hacked in special
       handling. Terminate with extreme prejudice. *)

    | CustomFunT (_, DebugPrint),
      CallT (_, reason_op, { call_targs = None; call_args_tlist; call_tout; _ }) ->
      List.iter (fun arg -> match arg with
        | Arg t -> rec_flow cx trace (t, DebugPrintT reason_op)
        | SpreadArg t ->
          add_output cx ~trace
            (FlowError.(EUnsupportedSyntax (loc_of_t t, SpreadArgument)));
      ) call_args_tlist;
      rec_flow_t cx trace (VoidT.why reason_op, call_tout);

    | CustomFunT (_, DebugThrow), CallT (_, reason_op, _) ->
      raise (Flow_error.EDebugThrow (loc_of_reason reason_op))

    | CustomFunT (_, DebugSleep),
      CallT (_, reason_op, { call_targs = None; call_args_tlist=arg1::_; call_tout; _ }) ->
      let t = extract_non_spread cx ~trace arg1 in
      rec_flow cx trace (t, DebugSleepT reason_op);
      rec_flow_t cx trace (VoidT.why reason_op, call_tout)

    | CustomFunT (lreason, (
          Compose _
        | ReactCreateElement
        | ReactCloneElement
        | ReactElementFactory _
       as kind)),
      CallT (use_op, reason_op, calltype) ->
      let {
        call_targs;
        call_args_tlist = args;
        call_tout = tout;
        call_this_t = _;
        call_closure_t = _;
        call_strict_arity = _;
      } = calltype in

      (* None of the supported custom funs are polymorphic, so error here
         instead of threading targs into spread resolution. *)
      Option.iter call_targs ~f:(fun _ ->
        add_output cx ~trace FlowError.(
          ECallTypeArity {
            call_loc = loc_of_reason reason_op;
            is_new = false;
            reason_arity = lreason;
            expected_arity = 0;
          }));

      resolve_call_list cx ~trace ~use_op reason_op args (
        ResolveSpreadsToCustomFunCall (mk_id (), kind, tout))

    | CustomFunT (reason, _), _ when function_like_op u ->
      rec_flow cx trace (DefT (reason, AnyFunT), u)

    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    (* ObjT -> ObjT *)

    | DefT (lreason, ObjT ({ props_tmap = lflds; _ } as l_obj)),
      UseT (use_op, (DefT (ureason, ObjT ({ props_tmap = uflds; _ } as u_obj)) as u_deft)) ->
      Type_inference_hooks_js.dispatch_obj_to_obj_hook cx l u_deft;
      if lflds = uflds then ()
      else flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj)


    | DefT (_, ObjT _), UseT (_, NullProtoT _) -> ()

    (* InstanceT -> ObjT *)

    | DefT (lreason, InstanceT (_, super, _, {
        fields_tmap = lflds;
        methods_tmap = lmethods;
        inst_call_t = lcall; _ })),
      UseT (use_op, (DefT (ureason, ObjT {
        props_tmap = uflds;
        proto_t = uproto;
        call_t = ucall; _ }) as u_deft)) ->
      Type_inference_hooks_js.dispatch_instance_to_obj_hook cx l u_deft;

      let lflds =
        let fields_tmap = Context.find_props cx lflds in
        let methods_tmap = Context.find_props cx lmethods in
        SMap.union fields_tmap methods_tmap
      in

      Option.iter ucall ~f:(fun ucall ->
        let prop_name = Some "$call" in
        let use_op = Frame (PropertyCompatibility {
          prop = prop_name;
          lower = lreason;
          upper = ureason;
          is_sentinel = false;
        }, use_op) in
        (match lcall with
        | Some lcall -> rec_flow cx trace (lcall, UseT (use_op, ucall))
        | None ->
          let reason_prop = replace_reason_const (RProperty prop_name) ureason in
          add_output cx ~trace (FlowError.EStrictLookupFailed
            ((reason_prop, lreason), lreason, prop_name, Some use_op)))
      );

      iter_real_props cx uflds (fun ~is_sentinel s up ->
        let use_op = Frame (PropertyCompatibility {
          prop = Some s;
          lower = lreason;
          upper = ureason;
          is_sentinel;
        }, use_op) in
        let propref =
          let reason_prop = replace_reason_const (RProperty (Some s)) ureason in
          Named (reason_prop, s)
        in
        match SMap.get s lflds with
        | Some lp ->
          rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
        | _ ->
          match up with
          | Field (_, DefT (_, OptionalT ut), upolarity) ->
            rec_flow cx trace (l,
              LookupT (ureason, NonstrictReturning (None, None), [], propref,
                LookupProp (use_op, Field (None, ut, upolarity))))
          | _ ->
            let u =
              LookupT (ureason, Strict lreason, [], propref,
                LookupProp (use_op, up)) in
            rec_flow cx trace (super, ReposLowerT (lreason, false, u))
      );

      rec_flow cx trace (l, UseT (use_op, uproto))

    (* For some object `x` and constructor `C`, if `x instanceof C`, then the
       object is a subtype. We use `ExtendsT` to walk the proto chain of the
       object, in case it includes a nominal type. *)
    | DefT (_, ObjT _), UseT (use_op, (DefT (_, InstanceT _) as u)) ->
      rec_flow cx trace (l, extends_use_type use_op l u)

    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)

    | DefT (reason, (ObjT _ | InstanceT _)), (
        UseT (use_op, DefT (reason_op, (FunT _ | AnyFunT))) |
        CallT (use_op, reason_op, _)
      ) ->
      let prop_name = Some "$call" in
      let use_op = match u with
      | UseT (_, DefT (_, (FunT _ | AnyFunT))) ->
        Frame (PropertyCompatibility {
          prop = prop_name;
          lower = reason;
          upper = reason_op;
          is_sentinel = false;
        }, use_op)
      | _ -> use_op in
      let fun_t = match l with
      | DefT (_, ObjT {call_t = Some t; _}) -> t
      | DefT (_, InstanceT (_, _, _, {inst_call_t = Some t; _})) -> t
      | _ ->
        let reason_prop = replace_reason_const (RProperty prop_name) reason_op in
        add_output cx ~trace (FlowError.EStrictLookupFailed
          ((reason_prop, reason), reason, prop_name, Some use_op));
        AnyT.why reason_op
      in
      (match u with
      | UseT (_, (DefT (_, (FunT _ | AnyFunT)) as u_def)) ->
        rec_flow cx trace (fun_t, UseT (use_op, u_def))
      | _ -> rec_flow cx trace (fun_t, u))

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

        TODO: The type constructors ShapeT, ObjAssignToT/ObjAssignFromT,
        ObjRestT express related meta-operations on objects. Consolidate these
        meta-operations and ensure consistency of their semantics. **)

    | (ShapeT (o), _) ->
        rec_flow cx trace (o, u)

    | DefT (reason, ObjT { props_tmap = mapr; call_t = None; _ }), UseT (use_op', ShapeT proto) ->
        (* TODO: ShapeT should have its own reason *)
        let reason_op = reason_of_t proto in
        iter_real_props cx mapr (fun ~is_sentinel x p ->
          let use_op = Frame (PropertyCompatibility {
            prop = Some x;
            lower = reason;
            upper = reason_of_t proto;
            is_sentinel;
          }, use_op') in
          let reason_prop = replace_reason (fun desc ->
            RPropertyOf (x, desc)
          ) reason in
          match Property.read_t p with
          | Some t ->
            let propref = Named (reason_prop, x) in
            let t = filter_optional cx ~trace reason_prop t in
            rec_flow cx trace (proto,
              SetPropT (use_op, reason_op, propref, Normal, t, None)
            )
          | None ->
            add_output cx ~trace (FlowError.EPropAccess (
              (reason_prop, reason_op), Some x, Property.polarity p, Read, use_op'
            ))
        )

    (* Function definitions are incompatible with ShapeT. ShapeT is meant to
     * match an object type with a subset of the props in the type being
     * destructured. It would be complicated and confusing to use a function for
     * this.
     *
     * This invariant is important for the React setState() type definition. *)
    | DefT (_, (FunT _ | ObjT {call_t = Some _; _})), UseT (use_op, ShapeT o) ->
        add_output cx ~trace
          (FlowError.EFunctionIncompatibleWithShape (reason_of_t l, reason_of_t o, use_op))

    | (_, UseT (_, ShapeT (o))) ->
        let reason = reason_of_t o in
        rec_flow cx trace (l, ObjAssignFromT (reason, o, Locationless.AnyT.t, default_obj_assign_kind))

    | DefT (_, AnyT), ObjTestT (reason_op, _, u) ->
      rec_flow_t cx trace (AnyT.why reason_op, u)

    | _, ObjTestT (reason_op, default, u) ->
      let u = ReposLowerT (reason_op, false, UseT (unknown_use, u)) in
      if object_like l
      then rec_flow cx trace (l, u)
      else rec_flow cx trace (default, u)

    | DefT (_, (AnyT | AnyObjT)), ObjTestProtoT (reason_op, u) ->
      rec_flow_t cx trace (AnyT.why reason_op, u)

    | DefT (_, NullT), ObjTestProtoT (reason_op, u) ->
      rec_flow_t cx trace (NullProtoT.why reason_op, u)

    | _, ObjTestProtoT (reason_op, u) ->
      let proto =
        if object_like l
        then reposition cx ~trace (loc_of_reason reason_op) l
        else
          let () = add_output cx ~trace
            (FlowError.EInvalidPrototype (reason_of_t l)) in
          ObjProtoT.why reason_op
      in
      rec_flow_t cx trace (proto, u)

    (********************************************)
    (* array types deconstruct into their parts *)
    (********************************************)

    (* Arrays can flow to arrays *)
    | DefT (r1, ArrT (ArrayAT (t1, ts1))),
      UseT (use_op, DefT (r2, ArrT (ArrayAT (t2, ts2)))) ->
      let use_op = Frame (ArrayElementCompatibility {
        lower = r1;
        upper = r2;
      }, use_op) in
      let lit1 = (desc_of_reason r1) = RArrayLit in
      let ts1 = Option.value ~default:[] ts1 in
      let ts2 = Option.value ~default:[] ts2 in
      array_flow cx trace use_op lit1 r1 (ts1, t1, ts2, t2)

    (* Tuples can flow to tuples with the same arity *)
    | DefT (r1, ArrT (TupleAT (_, ts1))),
      UseT (use_op, DefT (r2, ArrT (TupleAT (_, ts2)))) ->
      let fresh = (desc_of_reason r1) = RArrayLit in
      let l1 = List.length ts1 in
      let l2 = List.length ts2 in
      if l1 <> l2 then
        add_output cx ~trace (FlowError.ETupleArityMismatch
          ((r1, r2), l1, l2, use_op));
      let n = ref 0 in
      iter2opt (fun t1 t2 ->
        match t1, t2 with
        | Some t1, Some t2 ->
          n := !n + 1;
          let use_op = Frame (TupleElementCompatibility {
            n = !n;
            lower = r1;
            upper = r2;
          }, use_op) in
          flow_to_mutable_child cx trace use_op fresh t1 t2
        | _ -> ()
      ) (ts1, ts2);

    (* Arrays with known elements can flow to tuples *)
    | DefT (r1, ArrT (ArrayAT (t1, ts1))),
      UseT (use_op, DefT (r2, ArrT (TupleAT _))) ->
      begin match ts1 with
      | None -> add_output cx ~trace (FlowError.ENonLitArrayToTuple ((r1, r2), use_op))
      | Some ts1 ->
          rec_flow cx trace (DefT (r1, ArrT (TupleAT (t1, ts1))), u)
      end

    (* EmptyAT arrays are the subtype of all arrays *)
    | DefT (_, ArrT EmptyAT), UseT (_, DefT (_, ArrT _)) -> ()

    (* Read only arrays are the super type of all tuples and arrays *)
    | DefT (r1, ArrT (ArrayAT (t1, _) | TupleAT (t1, _) | ROArrayAT (t1))),
      UseT (use_op, DefT (r2, ArrT (ROArrayAT (t2)))) ->
      let use_op = Frame (ArrayElementCompatibility {
        lower = r1;
        upper = r2;
      }, use_op) in
      rec_flow cx trace (t1, UseT (use_op, t2))

    (**************************************************)
    (* instances of classes follow declared hierarchy *)
    (**************************************************)

    | DefT (_, InstanceT _), UseT (use_op, (DefT (_, InstanceT _) as u)) ->
      rec_flow cx trace (l, extends_use_type use_op l u)

    | DefT (reason, InstanceT (_, super, implements, instance)),
      ExtendsUseT (use_op, reason_op, try_ts_on_failure, l,
        (DefT (_, InstanceT (_, _, _, instance_super)) as u)) ->
      if instance.class_id = instance_super.class_id
      then begin
          (if instance.class_id != instance_super.class_id then
            assert_false "unexpected difference in class_ids in flow_instts");
          let { type_args = tmap1; arg_polarities = pmap; _ } = instance in
          let { type_args = tmap2; _ } = instance_super in
          let ureason = replace_reason (function RExtends desc -> desc | desc -> desc) reason_op in
          flow_type_args cx trace ~use_op reason ureason pmap tmap1 tmap2
        end
      else
        (* If this instance type has declared implementations, any structural
           tests have already been performed at the declaration site. We can
           then use the ExtendsT use type to search for a nominally matching
           implementation, thereby short-circuiting a potentially expensive
           structural test at the use site. *)
        let u = ExtendsUseT (use_op, reason_op, try_ts_on_failure @ implements, l, u) in
        rec_flow cx trace (super, ReposLowerT (reason, false, u))

    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)

    | DefT (_, ClassT it), UseT (_, DefT (r, TypeT t)) ->
      (* a class value annotation becomes the instance type *)
      rec_flow cx trace (it, BecomeT (r, t))

    | DefT (_, FunT(_, prototype, _)), UseT (_, DefT (reason, TypeT t)) ->
      (* a function value annotation becomes the prototype type *)
      rec_flow cx trace (prototype, BecomeT (reason, t))

    | DefT (_, AnyT), UseT (_, DefT (reason, TypeT t)) ->
      (* any can function as class or function type, hence ok for annotations *)
      rec_flow cx trace (l, BecomeT (reason, t))

    | DefT (_, TypeT l), UseT (use_op, DefT (_, TypeT u)) ->
      rec_unify cx trace ~use_op ~unify_any:true l u

    (* non-class/function values used in annotations are errors *)
    | _, UseT (_, DefT (ru, TypeT _)) ->
      add_output cx ~trace (FlowError.EValueUsedAsType (reason_of_t l, ru))

    | DefT (rl, ClassT l), UseT (use_op, DefT (_, ClassT u)) ->
      rec_flow cx trace (
        reposition cx ~trace (loc_of_reason rl) l,
        UseT (use_op, u))

    | DefT (_, FunT (static1, prototype, _)),
      UseT (use_op, DefT (_, ClassT (DefT (_, InstanceT (static2, _, _, _)) as u_))) ->
      rec_unify cx trace ~use_op static1 static2;
      rec_unify cx trace ~use_op prototype u_

    | DefT (_, AnyT), UseT (use_op, DefT (_, ClassT u)) ->
      rec_flow cx trace (l, UseT (use_op, u))

    (*********************************************************)
    (* class types derive instance types (with constructors) *)
    (*********************************************************)

    | DefT (reason, ClassT this),
      ConstructorT (use_op, reason_op, targs, args, t) ->
      let reason_o = replace_reason_const RConstructorReturn reason in
      (* early error if type args passed to non-polymorphic class *)
      Option.iter targs ~f:(fun _ ->
        add_output cx ~trace FlowError.(ECallTypeArity {
          call_loc = loc_of_reason reason_op;
          is_new = true;
          reason_arity = reason_of_t this;
          expected_arity = 0;
        }));
      (* call this.constructor(args) *)
      let ret = Tvar.mk_where cx reason_op (fun t ->
        let funtype = mk_methodcalltype this None args t in
        let propref = Named (reason_o, "constructor") in
        rec_flow cx trace (
          this,
          MethodT (use_op, reason_op, reason_o, propref, funtype, None)
        );
      ) in
      (* return this *)
      rec_flow cx trace (ret, ObjTestT (annot_reason reason_op, this, t))

    (****************************************************************)
    (* function types derive objects through explicit instantiation *)
    (****************************************************************)

    | DefT (lreason, FunT (_, proto, ({
        this_t = this;
        return_t = ret;
        _ } as ft))),
      ConstructorT (use_op, reason_op, targs, args, t) ->
      (* TODO: closure *)
      (** create new object **)
      let reason_c = replace_reason_const RNewObject reason_op in
      let objtype =
        let sealed = UnsealedInFile (Loc.source (loc_of_t proto)) in
        let flags = { default_flags with sealed } in
        let dict = None in
        let call = None in
        let pmap = Context.make_property_map cx SMap.empty in
        mk_objecttype ~flags ~dict ~call pmap proto
      in
      let new_obj = DefT (reason_c, ObjT objtype) in
      (** error if type arguments are provided to non-polymorphic constructor **)
      Option.iter targs ~f:(fun _ ->
        add_output cx ~trace FlowError.(ECallTypeArity {
          call_loc = loc_of_reason reason_op;
          is_new = true;
          reason_arity = lreason;
          expected_arity = 0;
        }));
      (** call function with this = new_obj, params = args **)
      rec_flow_t cx trace (new_obj, this);
      multiflow_call cx trace ~use_op reason_op args ft;
      (** if ret is object-like, return ret; otherwise return new_obj **)
      let reason_o = replace_reason_const RConstructorReturn reason_op in
      rec_flow cx trace (ret, ObjTestT(reason_o, new_obj, t))

    | DefT (_, AnyFunT), ConstructorT (use_op, reason_op, targs, args, t) ->
      let reason_o = replace_reason_const RConstructorReturn reason_op in
      ignore targs; (* An untyped receiver can't do anything with type args *)
      call_args_iter
        (fun t -> rec_flow cx trace (t, UseT (use_op, AnyT.why reason_op)))
        args;
      rec_flow_t cx trace (DefT (reason_o, AnyObjT), t);

    | DefT (_, AnyT), ConstructorT (use_op, reason_op, targs, args, t) ->
      ignore targs; (* An untyped receiver can't do anything with type args *)
      call_args_iter (fun t ->
        rec_flow cx trace (t, UseT (use_op, AnyT.why reason_op))
      ) args;
      rec_flow_t cx trace (AnyT.why reason_op, t);

    (* Since we don't know the signature of a method on AnyFunT, assume every
       parameter is an AnyT. *)
    | DefT (_, AnyFunT),
      MethodT (use_op, reason_op, _, _, { call_args_tlist; call_tout; _}, prop_t) ->
      let any = AnyT.why reason_op in
      call_args_iter (fun t -> rec_flow cx trace (t, UseT (use_op, any))) call_args_tlist;
      Option.iter ~f:(fun prop_t -> rec_flow_t cx trace (any, prop_t)) prop_t;
      rec_flow_t cx trace (any, call_tout)

    (*************************)
    (* statics can be read   *)
    (*************************)

    | DefT (_, InstanceT (static, _, _, _)), GetStaticsT (reason_op, tout) ->
      rec_flow cx trace (static, ReposLowerT (reason_op, false,
        UseT (unknown_use, tout)))

    | DefT (_, AnyT), GetStaticsT (reason_op, tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | ObjProtoT _, GetStaticsT (reason_op, tout) ->
      (* ObjProtoT not only serves as the instance type of the root class, but
         also as the statics of the root class. *)
      rec_flow cx trace (l, ReposLowerT (reason_op, false,
        UseT (unknown_use, tout)))

    (********************)
    (* __proto__ getter *)
    (********************)

    (* TODO: Fix GetProtoT for InstanceT (and ClassT).
       The __proto__ object of an instance is an ObjT having the properties in
       insttype.methods_tmap, not the super instance.  *)
    | DefT (_, InstanceT (_, super, _, _)), GetProtoT (reason_op, t) ->
      let proto = reposition cx ~trace (loc_of_reason reason_op) super in
      rec_flow_t cx trace (proto, t)

    | DefT (_, ObjT {proto_t; _}), GetProtoT (reason_op, t) ->
      let proto = reposition cx ~trace (loc_of_reason reason_op) proto_t in
      rec_flow_t cx trace (proto, t)

    | ObjProtoT _, GetProtoT (reason_op, t) ->
      let proto = NullT.why reason_op in
      rec_flow_t cx trace (proto, t)

    | FunProtoT reason, GetProtoT (reason_op, t) ->
      let proto = ObjProtoT (repos_reason (loc_of_reason reason_op) reason) in
      rec_flow_t cx trace (proto, t)

    | DefT (_, (AnyT | AnyObjT | AnyFunT)), GetProtoT (reason_op, t) ->
      let proto = AnyT.why reason_op in
      rec_flow_t cx trace (proto, t)

    (********************)
    (* __proto__ setter *)
    (********************)

    | DefT (_, (AnyT | AnyObjT | AnyFunT)), SetProtoT _ -> ()

    | _, SetProtoT (reason_op, _) ->
      add_output cx ~trace (FlowError.EUnsupportedSetProto reason_op)

    (********************************************************)
    (* instances of classes may have their fields looked up *)
    (********************************************************)

    | DefT (lreason, InstanceT (_, super, _, instance)),
      LookupT (reason_op, kind, try_ts_on_failure, (Named (_, x) as propref), action) ->
      let fields_pmap = Context.find_props cx instance.fields_tmap in
      let methods_pmap = Context.find_props cx instance.methods_tmap in
      let pmap = SMap.union fields_pmap methods_pmap in
      (match SMap.get x pmap with
      | None ->
        (* If there are unknown mixins, the lookup should become nonstrict, as
           the searched-for property may be found in a mixin. *)
        let kind = match instance.has_unknown_react_mixins, kind with
        | true, Strict _ -> NonstrictReturning (None, None)
        | _ -> kind
        in
        rec_flow cx trace (super,
          LookupT (reason_op, kind, try_ts_on_failure, propref, action))
      | Some p ->
        (match kind with
        | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
        | _ -> ());
        perform_lookup_action cx trace propref p lreason reason_op action)
    | DefT (_, InstanceT _), LookupT (reason_op, _, _, Computed _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_op in
      add_output cx ~trace FlowError.(EInternal (loc, InstanceLookupComputed))

    (********************************)
    (* ... and their fields written *)
    (********************************)

    | DefT (reason_c, InstanceT (_, super, _, instance)),
      SetPropT (use_op, reason_op, Named (reason_prop, x), wr_ctx, tin, prop_t) ->
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      let strict = Strict reason_c in
      set_prop cx ~wr_ctx trace ~use_op reason_prop reason_op strict l super x
        fields tin prop_t;

    | DefT (reason_c, InstanceT _),
      SetPrivatePropT (use_op, reason_op, x, [], _, _, _) ->
      add_output cx ~trace (FlowError.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))

    | DefT (reason_c, InstanceT (_, _, _, instance)),
      SetPrivatePropT (use_op, reason_op, x, scope::scopes, static, tin, prop_t) ->
      if scope.class_binding_id != instance.class_id then
        rec_flow cx trace (
          l, SetPrivatePropT (use_op, reason_op, x, scopes, static, tin, prop_t)
        )
      else (
        let map =
          if static
          then scope.class_private_static_fields
          else scope.class_private_fields
        in
        match SMap.get x (Context.find_props cx map) with
        | None ->
          add_output cx ~trace (FlowError.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))
        | Some p ->
          let action = RWProp (use_op, l, tin, Write (Normal, prop_t)) in
          let propref = Named (reason_op, x) in
          perform_lookup_action cx trace propref p reason_c reason_op action
      )

    | DefT (_, InstanceT _), SetPropT (_, reason_op, Computed _, _, _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_op in
      add_output cx ~trace FlowError.(EInternal (loc, InstanceLookupComputed))

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | DefT (_, InstanceT _) as instance, GetPropT (_, _, Named (_, "constructor"), t) ->
      rec_flow_t cx trace (class_type instance, t)

    | DefT (reason_c, InstanceT (_, super, _, instance)),
      GetPropT (use_op, reason_op, Named (reason_prop, x), tout) ->
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      let strict =
        if instance.has_unknown_react_mixins then NonstrictReturning (None, None)
        else Strict reason_c
      in
      get_prop cx trace ~use_op reason_prop reason_op strict l super x fields tout

    | DefT (reason_c, InstanceT _),
      GetPrivatePropT (use_op, reason_op, x, [], _, _) ->
      add_output cx ~trace (FlowError.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))

    | DefT (reason_c, InstanceT (_, _, _, instance)),
      GetPrivatePropT (use_op, reason_op, x, scope::scopes, static, tout) ->
      if scope.class_binding_id <> instance.class_id then
        rec_flow cx trace (l, GetPrivatePropT (use_op, reason_op, x, scopes, static, tout))
      else
        let map =
          if static
          then scope.class_private_static_fields
          else scope.class_private_fields
        in
        (match SMap.get x (Context.find_props cx map) with
        | None ->
          add_output cx ~trace (FlowError.EPrivateLookupFailed ((reason_op, reason_c), x, use_op))
        | Some p ->
          let action = RWProp (use_op, l, tout, Read) in
          let propref = Named (reason_op, x) in
          perform_lookup_action cx trace propref p reason_c reason_op action)

    | DefT (_, InstanceT _), GetPropT (_, reason_op, Computed _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_op in
      add_output cx ~trace FlowError.(EInternal (loc, InstanceLookupComputed))

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | DefT (reason_c, InstanceT (_, super, _, instance)),
      MethodT (use_op, reason_call, reason_lookup, Named (reason_prop, x),
        funtype, prop_t)
      -> (* TODO: closure *)
      let fields_tmap = Context.find_props cx instance.fields_tmap in
      let methods_tmap = Context.find_props cx instance.methods_tmap in
      let methods = SMap.union fields_tmap methods_tmap in
      let funt = Tvar.mk cx reason_lookup in
      let strict =
        if instance.has_unknown_react_mixins then NonstrictReturning (None, None)
        else Strict reason_c
      in
      get_prop cx trace ~use_op reason_prop reason_lookup strict l super x methods funt;
      Option.iter ~f:(fun prop_t -> rec_flow_t cx trace (funt, prop_t)) prop_t;

      (* suppress ops while calling the function. if `funt` is a `FunT`, then
         `CallT` will set its own ops during the call. if `funt` is something
         else, then something like `VoidT ~> CallT` doesn't need the op either
         because we want to point at the call and undefined thing. *)
      rec_flow cx trace (funt, CallT (use_op, reason_call, funtype));

    | DefT (_, InstanceT _), MethodT (_, reason_call, _, Computed _, _, _) ->
      (* Instances don't have proper dictionary support. All computed accesses
         are converted to named property access to `$key` and `$value` during
         element resolution in ElemT. *)
      let loc = loc_of_reason reason_call in
      add_output cx ~trace FlowError.(EInternal (loc, InstanceLookupComputed))

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

    | to_obj, ObjAssignToT (reason, from_obj, t, kind) ->
      rec_flow cx trace (from_obj, ObjAssignFromT (reason, to_obj, t, kind))

    (** When some object-like type O1 flows to
        ObjAssignFromT(_,O2,X,ObjAssign), the properties of O1 are copied to
        O2, and O2 is linked to X to signal that the copying is done; the
        intention is that when those properties are read through X, they should
        be found (whereas this cannot be guaranteed when those properties are
        read through O2). However, there is an additional twist: this scheme
        may not work when O2 is unresolved. In particular, when O2 is
        unresolved, the constraints that copy the properties from O1 may race
        with reads of those properties through X as soon as O2 is resolved. To
        avoid this race, we make O2 flow to ObjAssignToT(_,O1,X,ObjAssign);
        when O2 is resolved, we make the switch. **)

    | DefT (lreason, ObjT { props_tmap = mapr; flags; dict_t; _ }),
      ObjAssignFromT (reason_op, to_obj, t, ObjAssign error_flags) ->
      Context.iter_props cx mapr (fun x p ->
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
          rec_flow cx trace (to_obj, SetPropT (
            unknown_use, reason_prop, propref, Normal, t, None
          ))
        | None ->
          add_output cx ~trace (FlowError.EPropAccess (
            (reason_prop, reason_op), Some x, Property.polarity p, Read, unknown_use
          ))
      );
      if dict_t <> None then rec_flow_t cx trace (DefT (reason_op, AnyObjT), t)
      else begin
        if error_flags.assert_exact && not flags.exact
        then add_output cx ~trace (FlowError.EInexactSpread (lreason, reason_op));
        rec_flow_t cx trace (to_obj, t)
      end

    | DefT (lreason, InstanceT (_, _, _, { fields_tmap; methods_tmap; _ })),
      ObjAssignFromT (reason_op, to_obj, t, ObjAssign _) ->
      let fields_pmap = Context.find_props cx fields_tmap in
      let methods_pmap = Context.find_props cx methods_tmap in
      let pmap = SMap.union fields_pmap methods_pmap in
      let props_to_skip = ["$key"; "$value"] in
      pmap |> SMap.iter (fun x p ->
        if not (List.mem x props_to_skip) then (
          match Property.read_t p with
          | Some t ->
            let propref = Named (reason_op, x) in
            rec_flow cx trace (to_obj, SetPropT (
              unknown_use, reason_op, propref, Normal, t, None
            ))
          | None ->
            add_output cx ~trace (FlowError.EPropAccess (
              (lreason, reason_op), Some x, Property.polarity p, Read, unknown_use
            ))
        )
      );
      rec_flow_t cx trace (to_obj, t)

    (* AnyObjT has every prop, each one typed as `any`, so spreading it into an
       existing object destroys all of the keys, turning the result into an
       AnyObjT as well. TODO: wait for `to_obj` to be resolved, and then call
       `SetPropT (_, _, _, AnyT, _)` on all of its props. *)
    | DefT (_, AnyObjT), ObjAssignFromT (reason, _, t, ObjAssign _) ->
      rec_flow_t cx trace (DefT (reason, AnyObjT), t)

    | ObjProtoT _, ObjAssignFromT (_, to_obj, t, ObjAssign _) ->
      rec_flow_t cx trace (to_obj, t)

    (* Object.assign semantics *)
    | DefT (_, (NullT | VoidT)), ObjAssignFromT (_, to_obj, tout, ObjAssign _) ->
      rec_flow_t cx trace (to_obj, tout)

    (* {...mixed} is the equivalent of {...{[string]: mixed}} *)
    | DefT (reason, MixedT _), ObjAssignFromT (_, _, _, ObjAssign _) ->
      let dict = {
        dict_name = None;
        key = StrT.make reason;
        value = l;
        dict_polarity = Neutral;
      } in
      let o = Obj_type.mk_with_proto cx reason
        (ObjProtoT reason)
        ~dict
        ~sealed:true ~exact:true
      in
      rec_flow cx trace (o, u)

    | DefT (arr_r, ArrT arrtype), ObjAssignFromT (r, o, t, ObjSpreadAssign) ->
      begin match arrtype with
      | ArrayAT (elemt, None)
      | ROArrayAT (elemt) ->
        (* Object.assign(o, ...Array<x>) -> Object.assign(o, x) *)
        rec_flow cx trace (elemt, ObjAssignFromT (r, o, t, default_obj_assign_kind))
      | TupleAT (_, ts)
      | ArrayAT (_, Some ts) ->
        (* Object.assign(o, ...[x,y,z]) -> Object.assign(o, x, y, z) *)
        List.iter (fun from ->
          rec_flow cx trace (from, ObjAssignFromT (r, o, t, default_obj_assign_kind))
        ) ts
      | EmptyAT ->
        (* Object.assign(o, ...EmptyAT) -> Object.assign(o, empty) *)
        rec_flow cx trace (DefT (arr_r, EmptyT), ObjAssignFromT (r, o, t, default_obj_assign_kind))
      end

    (*************************)
    (* objects can be copied *)
    (*************************)

    (* Note: The story around unsealed objects and rest is not great. One
       thought is to insert a special kind of shadow property into the host
       object, which directs all writes (other than those in `xs`) to the
       unsealed rest result object. For now, the design here is incomplete. *)

    | DefT (_, ObjT { props_tmap; flags; _ }), ObjRestT (reason, xs, t) ->
      let props = Context.find_props cx props_tmap in
      let props = List.fold_left (fun map x -> SMap.remove x map) props xs in
      (* Remove shadow properties from rest result *)
      let props = SMap.filter (fun x _ -> not (is_internal_name x)) props in
      let proto = ObjProtoT reason in
      let sealed = Obj_type.sealed_in_op reason flags.sealed in
      (* A rest result can not be exact if the source object is unsealed,
         because we may not have seen all the writes yet. *)
      let exact = sealed && flags.exact in
      let o = Obj_type.mk_with_proto cx reason ~props proto ~sealed ~exact in
      rec_flow_t cx trace (o, t)

    | DefT (reason, InstanceT (_, super, _, insttype)),
      ObjRestT (reason_op, xs, t) ->
      (* Spread fields from super into an object *)
      let obj_super = Tvar.mk_where cx reason_op (fun tvar ->
        let u = ObjRestT (reason_op, xs, tvar) in
        rec_flow cx trace (super, ReposLowerT (reason, false, u))
      ) in

      (* Spread fields from the instance into another object *)
      let props = Context.find_props cx insttype.fields_tmap in
      let props = List.fold_left (fun props x -> SMap.remove x props) props xs in
      let proto = ObjProtoT reason_op in
      let obj_inst = Obj_type.mk_with_proto cx reason_op ~props proto in

      (* ObjAssign the inst-generated obj into the super-generated obj *)
      let o = Tvar.mk_where cx reason_op (fun tvar ->
        rec_flow cx trace (
          obj_inst,
          ObjAssignFromT (reason_op, obj_super, tvar, default_obj_assign_kind)
        )
      ) in

      rec_flow_t cx trace (o, t)

    | DefT (_, AnyT), ObjRestT (reason, _, t) ->
      rec_flow_t cx trace (AnyT.why reason, t)

    (* ...AnyObjT and AnyFunT yield AnyObjT *)
    | DefT (_, (AnyFunT | AnyObjT)), ObjRestT (reason, _, t) ->
      rec_flow_t cx trace (DefT (reason, AnyObjT), t)

    | (ObjProtoT _, ObjRestT (reason, _, t)) ->
      let obj = Obj_type.mk_with_proto cx reason l in
      rec_flow_t cx trace (obj, t)

    | DefT (_, (NullT | VoidT)), ObjRestT (reason, _, t) ->
      (* mirroring Object.assign semantics, treat null/void as empty objects *)
      let o = Obj_type.mk cx reason in
      rec_flow_t cx trace (o, t)

    (*************************************)
    (* objects can be copied-then-sealed *)
    (*************************************)
    | DefT (_, ObjT { props_tmap = mapr; _ }), ObjSealT (reason, t) ->
      let props = Context.find_props cx mapr in
      let new_obj =
        Obj_type.mk_with_proto cx reason ~sealed:true ~props l
      in
      rec_flow_t cx trace (new_obj, t)

    | DefT (_, AnyT), ObjSealT (reason, tout) ->
      rec_flow_t cx trace (AnyT.why reason, tout)

    | DefT (_, AnyObjT), ObjSealT (reason, tout) ->
      rec_flow_t cx trace (DefT (reason, AnyObjT), tout)

    (*************************)
    (* objects can be frozen *)
    (*************************)

    | DefT (reason_o, ObjT objtype), ObjFreezeT (reason_op, t) ->
      (* make the reason describe the result (e.g. a frozen object literal),
         but point at the entire Object.freeze call. *)
      let desc = RFrozen (desc_of_reason reason_o) in
      let reason = replace_reason_const desc reason_op in

      let flags = {frozen = true; sealed = Sealed; exact = true;} in
      let new_obj = DefT (reason, ObjT {objtype with flags}) in
      rec_flow_t cx trace (new_obj, t)

    | DefT (_, AnyT), ObjFreezeT (reason_op, t) ->
      rec_flow_t cx trace (AnyT.why reason_op, t)

    | DefT (_, AnyObjT), ObjFreezeT (reason_op, t) ->
      rec_flow_t cx trace (DefT (reason_op, AnyObjT), t)

    (*******************************************)
    (* objects may have their fields looked up *)
    (*******************************************)

    | DefT (reason_obj, ObjT o),
      LookupT (reason_op, strict, try_ts_on_failure, propref, action) ->
      (match get_obj_prop cx trace o propref reason_op with
      | Some p ->
        (match strict with
        | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
        | _ -> ());
        perform_lookup_action cx trace propref p reason_obj reason_op action
      | None ->
        let strict = match Obj_type.sealed_in_op reason_op o.flags.sealed, strict with
        | false, ShadowRead (strict, ids) ->
          ShadowRead (strict, Nel.cons o.props_tmap ids)
        | false, ShadowWrite ids ->
          ShadowWrite (Nel.cons o.props_tmap ids)
        | _ -> strict
        in
        rec_flow cx trace (o.proto_t,
          LookupT (reason_op, strict, try_ts_on_failure, propref, action)));

    | DefT (reason, (AnyT | AnyObjT)),
      LookupT (reason_op, kind, _, propref, action) ->
      (match action with
      | SuperProp (_, lp) when Property.write_t lp = None ->
        (* Without this exception, we will call rec_flow_p where
         * `write_t lp = None` and `write_t up = Some`, which is a polarity
         * mismatch error. Instead of this, we could "read" `mixed` from
         * covariant props, which would always flow into `any`. *)
        ()
      | _ ->
        let p = Field (None, AnyT.why reason_op, Neutral) in
        (match kind with
        | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
        | _ -> ());
        perform_lookup_action cx trace propref p reason reason_op action)

    (*****************************************)
    (* ... and their fields written *)
    (*****************************************)

    | DefT (_, ObjT {flags; _}),
      SetPropT (use_op, reason_op, Named (prop, "constructor"), _, _, _) ->
      if flags.frozen
      then
        add_output cx ~trace
          (FlowError.EPropAccess ((prop, reason_op), Some "constructor",
            Positive, Write (Normal, None), use_op))

    (** o.x = ... has the additional effect of o[_] = ... **)

    | DefT (_, ObjT { flags; _ }), SetPropT (use_op, reason_op, prop, _, _, _)
      when flags.frozen ->
      let reason_prop, prop = match prop with
      | Named (r, prop) -> r, Some prop
      | Computed t -> reason_of_t t, None
      in
      add_output cx ~trace (FlowError.EPropAccess ((reason_prop, reason_op), prop,
        Positive, Write (Normal, None), use_op))

    | DefT (reason_obj, ObjT o), SetPropT (use_op, reason_op, propref, _, tin, prop_t) ->
      write_obj_prop cx trace ~use_op o propref reason_obj reason_op tin prop_t

    (* Since we don't know the type of the prop, use AnyT. *)
    | DefT (_, (AnyT | AnyObjT)), SetPropT (use_op, reason_op, _, _, t, prop_t) ->
      Option.iter ~f:(fun t -> rec_flow_t cx trace (AnyT.why reason_op, t)) prop_t;
      rec_flow cx trace (t, UseT (use_op, AnyT.why reason_op))

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | DefT (_, ObjT _), GetPropT (_, reason_op, Named (_, "constructor"), tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | DefT (reason_obj, ObjT o), GetPropT (use_op, reason_op, propref, tout) ->
      read_obj_prop cx trace ~use_op o propref reason_obj reason_op tout

    | DefT (_, (AnyObjT | AnyT)), GetPropT (_, reason_op, _, tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | DefT (_, ObjT _), MethodT(_, _, _, Named (_, "constructor"), _, _) -> ()

    | DefT (reason_obj, ObjT o),
      MethodT (use_op, reason_call, reason_lookup, propref, funtype, prop_t) ->
      let t = Tvar.mk_where cx reason_lookup (fun tout ->
        read_obj_prop cx trace ~use_op o propref reason_obj reason_lookup tout
      ) in
      Option.iter ~f:(fun prop_t -> rec_flow_t cx trace (t, prop_t)) prop_t;
      rec_flow cx trace (t, CallT (use_op, reason_call, funtype))

    (* Since we don't know the signature of a method on AnyObjT, assume every
       parameter is an AnyT. *)
    | DefT (_, (AnyObjT | AnyT)),
      MethodT (use_op, reason_op, _, _, { call_args_tlist; call_tout; _}, prop_t) ->
      let any = AnyT.why reason_op in
      Option.iter ~f:(fun prop_t -> rec_flow_t cx trace (any, prop_t)) prop_t;
      call_args_iter (fun t -> rec_flow cx trace (t, UseT (use_op, any))) call_args_tlist;
      rec_flow_t cx trace (any, call_tout)

    (******************************************)
    (* strings may have their characters read *)
    (******************************************)

    | DefT (reason_s, StrT _), GetElemT (use_op, reason_op, index, tout) ->
      rec_flow cx trace (index, UseT (use_op, NumT.why reason_s));
      rec_flow_t cx trace (StrT.why reason_op, tout)

    (** Expressions may be used as keys to access objects and arrays. In
        general, we cannot evaluate such expressions at compile time. However,
        in some idiomatic special cases, we can; in such cases, we know exactly
        which strings/numbers the keys may be, and thus, we can use precise
        properties and indices to resolve the accesses. *)

    (**********************************************************************)
    (* objects/arrays may have their properties/elements written and read *)
    (**********************************************************************)

    | DefT (_, (ObjT _ | AnyObjT | ArrT _ | AnyT)), SetElemT (use_op, reason_op, key, tin, tout) ->
      rec_flow cx trace (key, ElemT (use_op, reason_op, l, WriteElem (tin, tout)))

    | DefT (_, (ObjT _ | AnyObjT | ArrT _ | AnyT)), GetElemT (use_op, reason_op, key, tout) ->
      rec_flow cx trace (key, ElemT (use_op, reason_op, l, ReadElem tout))

    | DefT (_, (ObjT _ | AnyObjT | ArrT _ | AnyT)),
      CallElemT (reason_call, reason_lookup, key, ft) ->
      let action = CallElem (reason_call, ft) in
      rec_flow cx trace (key, ElemT (unknown_use, reason_lookup, l, action))

    | _, ElemT (use_op, reason_op, (DefT (_, ObjT _) as obj), action) ->
      let propref = match l with
      | DefT (reason_x, StrT (Literal (_, x))) ->
          let reason_prop = replace_reason_const (RProperty (Some x)) reason_x in
          Named (reason_prop, x)
      | _ -> Computed l
      in
      (match action with
      | ReadElem t ->
        rec_flow cx trace (obj, GetPropT (use_op, reason_op, propref, t))
      | WriteElem (tin, tout) ->
        rec_flow cx trace (obj, SetPropT (use_op, reason_op, propref, Normal, tin, None));
        Option.iter ~f:(fun t -> rec_flow_t cx trace (obj, t)) tout
      | CallElem (reason_call, ft) ->
        rec_flow cx trace (obj, MethodT (use_op, reason_call, reason_op, propref, ft, None)))

    | _, ElemT (use_op, reason_op, (DefT (_, (AnyObjT | AnyT)) as obj), action) ->
      let value = AnyT.why reason_op in
      perform_elem_action cx trace ~use_op reason_op obj value action

    (* It is not safe to write to an unknown index in a tuple. However, any is
     * a source of unsoundness, so that's ok. `tup[(0: any)] = 123` should not
     * error when `tup[0] = 123` does not. *)
    | DefT (_, AnyT),
      ElemT (use_op, reason_op, (DefT (r, ArrT arrtype) as arr), action) ->
      let value = elemt_of_arrtype r arrtype in
      perform_elem_action cx trace ~use_op reason_op arr value action

    | l, ElemT (use_op, reason, (DefT (reason_tup, ArrT arrtype) as arr), action) when numeric l ->
      let value, ts, is_tuple = begin match arrtype with
      | ArrayAT(value, ts) -> value, ts, false
      | TupleAT(value, ts) -> value, Some ts, true
      | ROArrayAT (value) -> value, None, true
      | EmptyAT -> DefT (reason_tup, EmptyT), None, true
      end in
      let exact_index, value = match l with
      | DefT (_, NumT (Literal (_, (float_value, _)))) ->
          begin match ts with
          | None -> false, value
          | Some ts ->
              let index = int_of_float float_value in
              begin
                try true, List.nth ts index
                with _ ->
                if is_tuple then begin
                  let reasons = (reason, reason_tup) in
                  let error =
                    FlowError.ETupleOutOfBounds (reasons, List.length ts, index, use_op)
                  in
                  add_output cx ~trace error;
                  true, DefT (mk_reason RTupleOutOfBoundsAccess (loc_of_reason reason), VoidT)
                end else true, value
              end
          end
      | _ -> false, value
      in
      if is_tuple && not exact_index then begin
        match action with
        (* These are safe to do with tuples and unknown indexes *)
        | ReadElem _ | CallElem _ -> ()
        (* This isn't *)
        | WriteElem _ ->
          let reasons = (reason, reason_tup) in
            add_output
              cx
              ~trace
              (FlowError.ETupleUnsafeWrite (reasons, use_op))
      end;

      perform_elem_action cx trace ~use_op reason arr value action


    | DefT (_, ArrT _), GetPropT (_, reason_op, Named (_, "constructor"), tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | DefT (_, ArrT _), SetPropT (_, _, Named (_, "constructor"), _, _, _)
    | DefT (_, ArrT _), MethodT (_, _, _, Named (_, "constructor"), _, _) ->
      ()

    (**************************************************)
    (* array pattern can consume the rest of an array *)
    (**************************************************)

    | DefT (_, ArrT arrtype), ArrRestT (_, reason, i, tout) ->
      let arrtype = match arrtype with
      | ArrayAT (_, None)
      | ROArrayAT _
      | EmptyAT -> arrtype
      | ArrayAT (elemt, Some ts) -> ArrayAT (elemt, Some (Core_list.drop ts i))
      | TupleAT (elemt, ts) -> TupleAT (elemt, Core_list.drop ts i) in
      let a = DefT (reason, ArrT arrtype) in
      rec_flow_t cx trace (a, tout)

    | DefT (_, AnyT), ArrRestT (_, reason, _, tout) ->
      rec_flow_t cx trace (AnyT.why reason, tout)

    (**************)
    (* object kit *)
    (**************)

    | _, ObjKitT (use_op, reason, resolve_tool, tool, tout) ->
      object_kit cx trace ~use_op reason resolve_tool tool tout l

    (**************************************************)
    (* function types can be mapped over a structure  *)
    (**************************************************)

    | DefT (_, (AnyT | AnyObjT)), MapTypeT (reason_op, _, tout) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | DefT (_, ArrT arrtype), MapTypeT (reason_op, TupleMap funt, tout) ->
      let f x = EvalT (funt, TypeDestructorT (unknown_use, reason_op, CallType [x]), mk_id ()) in
      let arrtype = match arrtype with
      | ArrayAT (elemt, ts) -> ArrayAT (f elemt, Option.map ~f:(List.map f) ts)
      | TupleAT (elemt, ts) -> TupleAT (f elemt, List.map f ts)
      | ROArrayAT (elemt) -> ROArrayAT (f elemt)
      | EmptyAT -> EmptyAT in
      let t =
        let reason = replace_reason_const RArrayType reason_op in
        DefT (reason, ArrT arrtype)
      in
      rec_flow_t cx trace (t, tout)

    | _, MapTypeT (reason, TupleMap funt, tout) ->
      let iter = get_builtin cx ~trace "$iterate" reason in
      let elemt = EvalT (iter, TypeDestructorT
        (unknown_use, reason, CallType [l]), mk_id ()) in
      let t = DefT (reason, ArrT (ROArrayAT elemt)) in
      rec_flow cx trace (t, MapTypeT (reason, TupleMap funt, tout))

    | DefT (_, ObjT o), MapTypeT (reason_op, ObjectMap funt, tout) ->
      let map_t t =
        let t, opt = match t with
        | DefT (_, OptionalT t) -> t, true
        | _ -> t, false
        in
        let t = EvalT (funt, TypeDestructorT
          (unknown_use, reason_op, CallType [t]), mk_id ()) in
        if opt
          then optional t
          else t
      in
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
        DefT (reason, ObjT {o with props_tmap; dict_t})
      in
      rec_flow_t cx trace (mapped_t, tout)

    | DefT (_, ObjT o), MapTypeT (reason_op, ObjectMapi funt, tout) ->
      let mapi_t key t =
        let t, opt = match t with
        | DefT (_, OptionalT t) -> t, true
        | _ -> t, false
        in
        let t = EvalT (funt, TypeDestructorT
          (unknown_use, reason_op, CallType [key; t]), mk_id ()) in
        if opt
          then optional t
          else t
      in
      let mapi_field key t =
        let reason = replace_reason_const (RStringLit key) reason_op in
        mapi_t (DefT (reason, SingletonStrT key)) t
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
        DefT (reason, ObjT {o with props_tmap; dict_t})
      in
      rec_flow_t cx trace (mapped_t, tout)

    (***********************************************)
    (* functions may have their prototypes written *)
    (***********************************************)

    | DefT (_, FunT (_, t, _)), SetPropT (_, reason_op, Named (_, "prototype"), _, tin, _) ->
      rec_flow cx trace (tin, ObjAssignFromT (reason_op, t, Locationless.AnyT.t, default_obj_assign_kind))

    (*********************************)
    (* ... and their prototypes read *)
    (*********************************)

    | DefT (_, FunT (_, t, _)), GetPropT (_, _, Named (_, "prototype"), tout) ->
      rec_flow_t cx trace (t, tout)

    | DefT (reason, ClassT instance), GetPropT (_, _, Named (_, "prototype"), tout) ->
      let instance = reposition cx ~trace (loc_of_reason reason) instance in
      rec_flow_t cx trace (instance, tout)

    (**************************************)
    (* ... and their fields/elements read *)
    (**************************************)

    | DefT (_, AnyFunT), (
          GetPropT (_, reason_op, _, tout)
        | GetElemT (_, reason_op, _, tout)
      ) ->
      rec_flow_t cx trace (AnyT.why reason_op, tout)

    | DefT (reason_fun, AnyFunT), LookupT (reason_op, kind, _, x, action) ->
      (match kind with
      | NonstrictReturning (_, Some (id, _)) -> Context.test_prop_hit cx id
      | _ -> ());
      let p = Field (None, AnyT.why reason_op, Neutral) in
      perform_lookup_action cx trace x p reason_fun reason_op action

    (*****************************************)
    (* ... and their fields/elements written *)
    (*****************************************)

    | DefT (_, AnyFunT), SetPropT (use_op, reason_op, _, _, tin, prop_t) ->
      let any = AnyT.why reason_op in
      Option.iter ~f:(fun t -> rec_flow_t cx trace (any, t)) prop_t;
      rec_flow cx trace (tin, UseT (use_op, any));

    | DefT (_, AnyFunT), SetElemT (use_op, reason_op, _, tin, tout) ->
      rec_flow cx trace (tin, UseT (use_op, AnyT.why reason_op));
      Option.iter ~f:(fun t -> rec_flow_t cx trace (l, t)) tout

    (***************************************************************)
    (* functions may be called by passing a receiver and arguments *)
    (***************************************************************)

    | FunProtoCallT _,
      CallT (use_op, reason_op, ({call_this_t = func; call_args_tlist; _} as funtype)) ->
      (* Drop the first argument in the use_op. *)
      let use_op = match use_op with
      | Op FunCall {op; fn; args = _ :: args} -> Op (FunCall {op; fn; args})
      | Op FunCallMethod {op; fn; prop; args = _ :: args} -> Op (FunCallMethod {op; fn; prop; args})
      | _ -> use_op
      in
      begin match call_args_tlist with
      (* func.call() *)
      | [] ->
        let funtype = { funtype with
          call_this_t = VoidT.why reason_op;
          call_args_tlist = [];
        } in
        rec_flow cx trace (func, CallT (use_op, reason_op, funtype))

      (* func.call(this_t, ...call_args_tlist) *)
      | (Arg call_this_t)::call_args_tlist ->
        let funtype = { funtype with call_this_t; call_args_tlist } in
        rec_flow cx trace (func, CallT (use_op, reason_op, funtype))

      (* func.call(...call_args_tlist) *)
      | (SpreadArg _ as first_arg)::_ ->
        let call_this_t = extract_non_spread cx ~trace first_arg in

        let funtype = { funtype with call_this_t; } in
        rec_flow cx trace (func, CallT (use_op, reason_op, funtype))
      end

    (*******************************************)
    (* ... or a receiver and an argument array *)
    (*******************************************)

    (* resolves the arguments... *)
    | FunProtoApplyT _,
        CallT (use_op, reason_op, ({call_this_t = func; call_args_tlist; _} as funtype)) ->
      (* Drop the specific AST derived argument reasons. Our new arguments come
       * from arbitrary positions in the array. *)
      let use_op = match use_op with
      | Op FunCall {op; fn; args = _} -> Op (FunCall {op; fn; args = []})
      | Op FunCallMethod {op; fn; prop; args = _} -> Op (FunCallMethod {op; fn; prop; args = []})
      | _ -> use_op
      in
      begin match call_args_tlist with
      (* func.apply() *)
      | [] ->
          let funtype = { funtype with
            call_this_t = VoidT.why reason_op;
            call_args_tlist = [];
          } in
          rec_flow cx trace (func, CallT (use_op, reason_op, funtype))

      (* func.apply(this_arg) *)
      | (Arg this_arg)::[] ->
          let funtype = { funtype with call_this_t = this_arg; call_args_tlist = [] } in
          rec_flow cx trace (func, CallT (use_op, reason_op, funtype))

      (* func.apply(this_arg, ts) *)
      | first_arg::(Arg ts)::_ ->
        let call_this_t = extract_non_spread cx ~trace first_arg in
        let call_args_tlist = [ SpreadArg ts ] in
        let funtype = { funtype with call_this_t; call_args_tlist; } in
        (* Ignoring `this_arg`, we're basically doing func(...ts). Normally
         * spread arguments are resolved for the multiflow application, however
         * there are a bunch of special-cased functions like bind(), call(),
         * apply, etc which look at the arguments a little earlier. If we delay
         * resolving the spread argument, then we sabotage them. So we resolve
         * it early *)
        let t = Tvar.mk_where cx reason_op (fun t ->
          let resolve_to = ResolveSpreadsToCallT (funtype, t) in
          resolve_call_list cx ~trace ~use_op reason_op call_args_tlist resolve_to
        ) in
        rec_flow_t cx trace (func, t)

      | (SpreadArg t1)::(SpreadArg t2)::_ ->
          add_output cx ~trace
            (FlowError.(EUnsupportedSyntax (loc_of_t t1, SpreadArgument)));
          add_output cx ~trace
            (FlowError.(EUnsupportedSyntax (loc_of_t t2, SpreadArgument)))
      | (SpreadArg t)::_
      | (Arg _)::(SpreadArg t)::_ ->
          add_output cx ~trace
            (FlowError.(EUnsupportedSyntax (loc_of_t t, SpreadArgument)))
      end

    (************************************************************************)
    (* functions may be bound by passing a receiver and (partial) arguments *)
    (************************************************************************)

    | FunProtoBindT lreason,
      CallT (use_op, reason_op, ({
        call_this_t = func;
        call_targs;
        call_args_tlist = first_arg::call_args_tlist;
        _
      } as funtype)) ->
      Option.iter call_targs ~f:(fun _ ->
        add_output cx ~trace FlowError.(ECallTypeArity {
          call_loc = loc_of_reason reason_op;
          is_new = false;
          reason_arity = lreason;
          expected_arity = 0;
        }));
      let call_this_t = extract_non_spread cx ~trace first_arg in
      let call_targs = None in
      let funtype = { funtype with call_this_t; call_targs; call_args_tlist } in
      rec_flow cx trace (func, BindT (use_op, reason_op, funtype, false))

    | DefT (reason, FunT (_, _, ({this_t = o1; _} as ft))),
      BindT (use_op, reason_op, calltype, _) ->
        let {
          call_this_t = o2;
          call_targs = _; (* always None *)
          call_args_tlist = tins2;
          call_tout;
          call_closure_t = _;
          call_strict_arity = _;
        } = calltype in

        (* TODO: closure *)

        rec_flow_t cx trace (o2,o1);

        let resolve_to =
          ResolveSpreadsToMultiflowPartial (mk_id (), ft, reason_op, call_tout) in
        resolve_call_list cx ~trace ~use_op reason tins2 resolve_to;

    | DefT (_, ObjT {call_t = Some t; _}), BindT _ ->
      rec_flow cx trace (t, u)

    | DefT (_, InstanceT (_, _, _, {inst_call_t = Some t; _})), BindT _ ->
      rec_flow cx trace (t, u)

    | DefT (_, (AnyT | AnyFunT)),
      BindT (use_op, reason, calltype, _) ->
      let {
        call_this_t;
        call_targs = _; (* always None *)
        call_args_tlist;
        call_tout;
        call_closure_t = _;
        call_strict_arity = _;
      } = calltype in
      rec_flow_t cx trace (AnyT.why reason, call_this_t);
      call_args_iter (fun param_t ->
        rec_flow cx trace (AnyT.why reason, UseT (use_op, param_t))
      ) call_args_tlist;
      rec_flow_t cx trace (l, call_tout)

    | _, BindT (_, _, { call_tout; _ }, true) ->
      rec_flow_t cx trace (l, call_tout)

    (***********************************************)
    (* You can use a function as a callable object *)
    (***********************************************)

    (* FunT ~> ObjT *)

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
    | DefT (reason, FunT (statics, _, _)),
      UseT (use_op, DefT (reason_o, ObjT { props_tmap; _ })) ->
        if not
          (quick_error_fun_as_obj cx trace ~use_op reason statics reason_o
             (Context.find_props cx props_tmap))
        then
          rec_flow cx trace (statics, u)

    (* TODO: similar concern as above *)
    | DefT (reason, FunT (statics, _, _)),
      UseT (use_op, DefT (reason_inst, InstanceT (_, _, _, {
        fields_tmap;
        structural = true;
        _;
      }))) ->
      if not
        (quick_error_fun_as_obj cx trace ~use_op reason statics reason_inst
          (SMap.filter (fun x _ -> x = "constructor")
            (Context.find_props cx fields_tmap)))
      then
        rec_flow cx trace (statics, u)

    (***************************************************************)
    (* Enable structural subtyping for upperbounds like interfaces *)
    (***************************************************************)

    | _, UseT (use_op, (DefT (_,InstanceT (_,_,_,{structural=true;_})) as i)) ->
      rec_flow cx trace (i, ImplementsT (use_op, l))

    | (ObjProtoT _ | FunProtoT _ | DefT (_, NullT)), ImplementsT _ -> ()

    | DefT (reason_inst, InstanceT (_, super, _, {
        fields_tmap;
        methods_tmap;
        inst_call_t;
        structural = true;
        _;
      })),
      ImplementsT (use_op, t) ->
      structural_subtype cx trace ~use_op t reason_inst
        (fields_tmap, methods_tmap, inst_call_t);
      rec_flow cx trace (super,
        ReposLowerT (reason_inst, false, ImplementsT (use_op, t)))

    | _, ImplementsT _ ->
      add_output cx ~trace (FlowError.EUnsupportedImplements (reason_of_t l))

    (*********************************************************************)
    (* class A is a base class of class B iff                            *)
    (* properties in B that override properties in A or its base classes *)
    (* have the same signatures                                          *)
    (*********************************************************************)

    (** The purpose of SuperT is to establish consistency between overriding
        properties with overridden properties. As such, the lookups performed
        for the inherited properties are non-strict: they are not required to
        exist. **)

    | DefT (ureason, InstanceT _),
      SuperT (use_op, reason, derived_type) ->
      let check_super = check_super cx trace ~use_op reason ureason l in
      (match derived_type with
      | DerivedInstance {fields_tmap; methods_tmap; _} ->
        Context.iter_props cx fields_tmap check_super;
        Context.iter_props cx methods_tmap (fun x p ->
          if inherited_method x then check_super x p
        )
      | DerivedStatics {props_tmap; _} ->
        Context.iter_props cx props_tmap (fun x p ->
          if inherited_method x then check_super x p
        ))

    | DefT (ureason, ObjT _), SuperT (use_op, reason, derived_type)
    | DefT (ureason, AnyObjT), SuperT (use_op, reason, derived_type) ->
      let check_super = check_super cx trace ~use_op reason ureason l in
      (match derived_type with
      | DerivedInstance {fields_tmap; methods_tmap; _} ->
        Context.iter_props cx fields_tmap check_super;
        Context.iter_props cx methods_tmap (fun x p ->
          if inherited_method x then check_super x p
        )
      | DerivedStatics {props_tmap; _} ->
        Context.iter_props cx props_tmap (fun x p ->
          if inherited_method x then check_super x p
        ))

    (***********************)
    (* opaque types part 2 *)
    (***********************)

    (* Don't refine opaque types based on its bound *)
    | OpaqueT _, PredicateT (p, t) -> predicate cx trace t l p
    | OpaqueT _, GuardT (pred, result, sink) -> guard cx trace l pred result sink

    (* Opaque types may be treated as their supertype when they are a lower bound for a use *)
    | OpaqueT (_, {super_t = Some t; _}), _ ->
        rec_flow cx trace (t, u)

    (***********************************************************)
    (* addition                                                *)
    (***********************************************************)

    | (l, AdderT (use_op, reason, flip, r, u)) ->
      flow_addition cx trace use_op reason flip l r u

    (*********************************************************)
    (* arithmetic/bitwise/update operations besides addition *)
    (*********************************************************)

    | _, AssertArithmeticOperandT _ when numeric l -> ()
    | _, AssertArithmeticOperandT _ ->
      add_output cx ~trace (FlowError.EArithmeticOperand (reason_of_t l))

    (***********************************************************************)
    (* Rest param annotations must be super types of the array bottom type *)
    (***********************************************************************)

    | rest, AssertRestParamT r ->
      (* This allows rest to be things like Iterable<T>, mixed, Array<T>, [1,2]
         but disallows things like number, string, boolean *)
      rec_flow_t cx trace (DefT (r, ArrT EmptyAT), rest)

    (***********************************************************)
    (* coercion                                                *)
    (***********************************************************)

    (* string and number can be coerced to strings *)
    | DefT (_, NumT _), UseT (Op Coercion _, DefT (_, StrT _)) -> ()

    (**************************)
    (* relational comparisons *)
    (**************************)

    | (l, ComparatorT(reason, flip, r)) ->
      flow_comparator cx trace reason flip l r;

    | (l, EqT(reason, flip, r)) ->
      flow_eq cx trace reason flip l r;

    (************************)
    (* unary minus operator *)
    (************************)

    | DefT (_, NumT lit), UnaryMinusT (reason_op, t_out) ->
      let num = match lit with
      | Literal (_, (value, raw)) ->
        let (value, raw) = Ast_utils.negate_number_literal (value, raw) in
        DefT (replace_reason_const RNumber reason_op, NumT (Literal (None, (value, raw))))
      | AnyLiteral
      | Truthy ->
        l
      in
      rec_flow_t cx trace (num, t_out)

    | DefT (_, AnyT), UnaryMinusT (reason_op, t_out) ->
      rec_flow_t cx trace (AnyT.why reason_op, t_out)

    (************************)
    (* binary `in` operator *)
    (************************)

    (* the left-hand side of a `(x in y)` expression is a string or number
       TODO: also, symbols *)
    | DefT (_, StrT _), AssertBinaryInLHST _ -> ()
    | DefT (_, NumT _), AssertBinaryInLHST _ -> ()
    | _, AssertBinaryInLHST _ ->
      add_output cx ~trace (FlowError.EBinaryInLHS (reason_of_t l))

    (* the right-hand side of a `(x in y)` expression must be object-like *)
    | DefT (_, ArrT _), AssertBinaryInRHST _ -> ()
    | _, AssertBinaryInRHST _ when object_like l -> ()
    | _, AssertBinaryInRHST _ ->
      add_output cx ~trace (FlowError.EBinaryInRHS (reason_of_t l))

    (******************)
    (* `for...in` RHS *)
    (******************)

    (* objects are allowed. arrays _could_ be, but are not because it's
       generally safer to use a for or for...of loop instead. *)
    | _, AssertForInRHST _ when object_like l -> ()
    | (DefT (_, AnyObjT) | ObjProtoT _), AssertForInRHST _ -> ()

    (* null/undefined are allowed *)
    | DefT (_, (NullT | VoidT)), AssertForInRHST _ -> ()

    | _, AssertForInRHST _ ->
      add_output cx ~trace (FlowError.EForInRHS (reason_of_t l))

    (**************************************)
    (* types may be refined by predicates *)
    (**************************************)

    | _, PredicateT(p,t) ->
      predicate cx trace t l p

    | _, GuardT (pred, result, sink) ->
      guard cx trace l pred result sink

    | DefT (_, StrT lit),
      SentinelPropTestT (reason, l, key, sense, Enum.(One Str sentinel), result) ->
      begin match lit with
        | Literal (_, value) when (value = sentinel) != sense ->
          if not sense
          then () (* provably unreachable, so prune *)
          else
            let l = matching_sentinel_prop reason key (SingletonStrT sentinel) in
            rec_flow_t cx trace (l, result)
        | _ ->
          rec_flow_t cx trace (l, result)
      end

    | DefT (_, NumT lit),
      SentinelPropTestT (reason, l, key, sense, Enum.(One Num sentinel_lit), result) ->
      let sentinel, _ = sentinel_lit in
      begin match lit with
        | Literal (_, (value, _)) when (value = sentinel) != sense ->
          if not sense
          then () (* provably unreachable, so prune *)
          else
            let l = matching_sentinel_prop reason key (SingletonNumT sentinel_lit) in
            rec_flow_t cx trace (l, result)
        | _ ->
          rec_flow_t cx trace (l, result)
      end

    | DefT (_, BoolT lit),
      SentinelPropTestT (reason, l, key, sense, Enum.(One Bool sentinel), result) ->
        begin match lit with
        | Some value when (value = sentinel) != sense ->
          if not sense
          then () (* provably unreachable, so prune *)
          else
            let l = matching_sentinel_prop reason key (SingletonBoolT sentinel) in
            rec_flow_t cx trace (l, result)
        | _ ->
            rec_flow_t cx trace (l, result)
        end

    | DefT (_, NullT),
      SentinelPropTestT (_reason, l, _key, sense, Enum.(One Null), result) ->
        if not sense
        then ()
        else rec_flow_t cx trace (l, result)

    | DefT (_, VoidT),
      SentinelPropTestT (_reason, l, _key, sense, Enum.(One Void), result) ->
        if not sense
        then ()
        else rec_flow_t cx trace (l, result)

    | DefT (_, (StrT _ | NumT _ | BoolT _ | NullT | VoidT)),
      SentinelPropTestT (reason, obj, key, sense, Enum.Many enums, result) ->
      if sense
      then EnumSet.iter (fun enum ->
        rec_flow cx trace (l, SentinelPropTestT (reason, obj, key, sense, Enum.One enum, result))
      ) enums
      else rec_flow_t cx trace (obj, result)

    | DefT (_, (StrT _ | NumT _ | BoolT _ | NullT | VoidT)),
      SentinelPropTestT (_reason, l, _key, sense, _, result) ->
        (* types don't match (would've been matched above) *)
        (* we don't prune other types like objects or instances, even though
           a test like `if (ObjT === StrT)` seems obviously unreachable, but
           we have to be wary of toString and valueOf on objects/instances. *)
        if sense
        then () (* provably unreachable, so prune *)
        else rec_flow_t cx trace (l, result)

    | _, SentinelPropTestT (_, l, _, _, _, result) ->
        (* property exists, but is not something we can use for refinement *)
        rec_flow_t cx trace (l, result)

    (*********************)
    (* functions statics *)
    (*********************)

    | DefT (reason, FunT (static, _, _)), _ when object_like_op u ->
      rec_flow cx trace (static, ReposLowerT (reason, false, u))

    (*****************)
    (* class statics *)
    (*****************)

    (* For GetPrivatePropT and SetPrivatePropT, the instance id is needed to determine whether
     * or not the private static field exists on that class. Since we look through the scopes for
     * the type of the field, there is no need to look at the static member of the instance.
     * Instead, we just flip the boolean flag to true, indicating that when the
     * InstanceT ~> Set/GetPrivatePropT constraint is processed that we should look at the
     * private static fields instead of the private instance fields. *)
    | DefT (reason, ClassT instance), GetPrivatePropT (use_op, reason_op, x, scopes, _, tout) ->
      let u = GetPrivatePropT (use_op, reason_op, x, scopes, true, tout) in
      rec_flow cx trace (instance, ReposLowerT (reason, false, u))

    | DefT (reason, ClassT instance), SetPrivatePropT (use_op, reason_op, x, scopes, _, tout, tp) ->
      let u = SetPrivatePropT (use_op, reason_op, x, scopes, true, tout, tp) in
      rec_flow cx trace (instance, ReposLowerT (reason, false, u))

    | DefT (reason, ClassT instance), _ when object_use u || object_like_op u ->
      let statics = Tvar.mk cx reason in
      rec_flow cx trace (instance, GetStaticsT (reason, statics));
      rec_flow cx trace (statics, u)

    (************************)
    (* classes as functions *)
    (************************)

    (* When a class value flows to a function annotation or call site, check for
       the presence of a call property in the former (as a static) compatible
       with the latter.

       TODO: Call properties are excluded from the subclass compatibility
       checks, which makes it unsafe to call a Class<T> type like this.
       For example:

           declare class A { static (): string };
           declare class B extends A { static (): number }
           var klass: Class<A> = B;
           var foo: string = klass(); // passes, but `foo` is a number

       The same issue is also true for constructors, which are similarly
       excluded from subclass compatibility checks, but are allowed on ClassT
       types.
    *)
    | DefT (reason, ClassT instance), (UseT (_, DefT (_, FunT _)) | CallT _) ->
      let statics = Tvar.mk cx reason in
      rec_flow cx trace (instance, GetStaticsT (reason, statics));
      rec_flow cx trace (statics, u)

    (************)
    (* indexing *)
    (************)

    | DefT (_, InstanceT _), GetElemT (use_op, reason, i, t) ->
      rec_flow cx trace (l, SetPropT (use_op, reason, Named (reason, "$key"), Normal, i, None));
      rec_flow cx trace (l, GetPropT (use_op, reason, Named (reason, "$value"), t))

    | DefT (_, InstanceT _), SetElemT (use_op, reason, i, tin, tout) ->
      rec_flow cx trace (l, SetPropT (use_op, reason, Named (reason, "$key"), Normal, i, None));
      rec_flow cx trace (l, SetPropT (use_op, reason, Named (reason, "$value"), Normal, tin, None));
      Option.iter ~f:(fun t -> rec_flow_t cx trace (l, t)) tout

    (***************************)
    (* conditional type switch *)
    (***************************)

    (* Use our alternate if our lower bound is empty. *)
    | DefT (_, EmptyT), CondT (_, alt, tout) -> rec_flow_t cx trace (alt, tout)
    (* Otherwise continue by Flowing out lower bound to tout. *)
    | _, CondT (_, _, tout) -> rec_flow_t cx trace (l, tout)

    (*************************)
    (* repositioning, part 2 *)
    (*************************)

    (* waits for a lower bound to become concrete, and then repositions it to
       the location stored in the ReposLowerT, which is usually the location
       where that lower bound was used; the lower bound's location (which is
       being overwritten) is where it was defined. *)
    | _, ReposLowerT (reason, use_desc, u) ->
      rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)

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

    | (DefT (_, NullT) | ObjProtoT _),
      LookupT (reason, strict, next::try_ts_on_failure, propref, t) ->
      (* When s is not found, we always try to look it up in the next element in
         the list try_ts_on_failure. *)
      rec_flow cx trace
        (next, LookupT (reason, strict, try_ts_on_failure, propref, t))

    | (ObjProtoT _ | FunProtoT _),
      LookupT (reason_op, _, [], Named (_, "__proto__"), RWProp (_, l, t, rw)) ->
      (* __proto__ is a getter/setter on Object.prototype *)
      let u = match rw with
      | Read -> GetProtoT (reason_op, t)
      | Write _ -> SetProtoT (reason_op, t)
      in
      rec_flow cx trace (l, u)

    | ObjProtoT _, LookupT (reason_op, _, [], Named (_, x), _)
      when is_object_prototype_method x ->
      (** TODO: These properties should go in Object.prototype. Currently we
          model Object.prototype as a ObjProtoT, as an optimization against a
          possible deluge of shadow properties on Object.prototype, since it
          is shared by every object. **)
      rec_flow cx trace (get_builtin_type cx ~trace reason_op "Object", u)

    | FunProtoT _, LookupT (reason_op, _, _, Named (_, x), _)
      when is_function_prototype x ->
      (** TODO: Ditto above comment for Function.prototype *)
      rec_flow cx trace (get_builtin_type cx ~trace reason_op "Function", u)

    | (DefT (reason, NullT) | ObjProtoT reason | FunProtoT reason),
      LookupT (reason_op, Strict strict_reason, [],
        (Named (reason_prop, x) as propref), action) ->
      let use_op = use_op_of_lookup_action action in
      add_output cx ~trace (FlowError.EStrictLookupFailed
        ((reason_prop, strict_reason), reason, Some x, use_op));
      let p = Field (None, AnyT.why reason_op, Neutral) in
      perform_lookup_action cx trace propref p reason reason_op action

    | (DefT (reason, NullT) | ObjProtoT reason | FunProtoT reason),
      LookupT (reason_op, Strict strict_reason, [],
        (Computed elem_t as propref), action) ->
      (match elem_t with
      | OpenT _ ->
        let loc = loc_of_t elem_t in
        add_output cx ~trace FlowError.(EInternal (loc, PropRefComputedOpen))
      | DefT (_, StrT (Literal _)) ->
        let loc = loc_of_t elem_t in
        add_output cx ~trace FlowError.(EInternal (loc, PropRefComputedLiteral))
      | DefT (_, AnyT) | DefT (_, StrT _) | DefT (_, NumT _) ->
        (* any, string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
        let p = Field (None, AnyT.why reason_op, Neutral) in
        perform_lookup_action cx trace propref p reason reason_op action
      | _ ->
        let reason_prop = reason_of_t elem_t in
        let use_op = use_op_of_lookup_action action in
        add_output cx ~trace (FlowError.EStrictLookupFailed
          ((reason_prop, strict_reason), reason, None, use_op)))

    | (DefT (reason, NullT) | ObjProtoT reason | FunProtoT reason),
      LookupT (reason_op, ShadowRead (strict, rev_proto_ids), [],
        (Named (reason_prop, x) as propref), action) ->
      (* Emit error if this is a strict read. See `lookup_kinds` in types.ml. *)
      (match strict with
      | None -> ()
      | Some strict_reason ->
        let use_op = use_op_of_lookup_action action in
        add_output cx ~trace (FlowError.EStrictLookupFailed
          ((reason_prop, strict_reason), reason, Some x, use_op)));

      (* Install shadow prop (if necessary) and link up proto chain. *)
      let prop_loc = def_loc_of_reason reason_prop in
      let p = find_or_intro_shadow_prop cx trace reason_op x prop_loc (Nel.rev rev_proto_ids) in
      perform_lookup_action cx trace propref p reason reason_op action

    | (DefT (reason, NullT) | ObjProtoT reason | FunProtoT reason), LookupT (reason_op,
        ShadowWrite rev_proto_ids, [], (Named (lookup_reason, x) as propref), action) ->
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
          let reason_prop = replace_reason_const (RShadowProperty x) reason_op in
          let t = Tvar.mk cx reason_prop in
          let prop_loc = def_loc_of_reason lookup_reason in
          (match proto_ids with
          | [] -> ()
          | id::ids ->
            let p_proto = find_or_intro_shadow_prop cx trace reason_op x prop_loc (id, ids) in
            let t_proto = Property.assert_field p_proto in
            rec_flow cx trace (t_proto, UnifyT (t_proto, t)));
          (* Add prop *)
          let p = Field (Some prop_loc, t, Neutral) in
          pmap
            |> SMap.add x p
            |> Context.add_property_map cx id;
          p
      in
      perform_lookup_action cx trace propref p reason reason_op action

    | (DefT (_, NullT) | ObjProtoT _ | FunProtoT _),
      LookupT (_, ShadowRead _, [], Computed elem_t, _) ->
      let loc = loc_of_t elem_t in
      add_output cx ~trace FlowError.(EInternal (loc, ShadowReadComputed))

    | (DefT (_, NullT) | ObjProtoT _ | FunProtoT _),
      LookupT (_, ShadowWrite _, [], Computed elem_t, _) ->
      let loc = loc_of_t elem_t in
      add_output cx ~trace FlowError.(EInternal (loc, ShadowWriteComputed))

    (* LookupT is a non-strict lookup *)
    | (DefT (_, NullT) |
       ObjProtoT _ |
       FunProtoT _ |
       (* TODO: why would mixed appear here? *)
       DefT (_, MixedT (Mixed_truthy | Mixed_non_maybe))),
      LookupT (_, NonstrictReturning (t_opt, test_opt), [], propref, action) ->
      (* don't fire

         ...unless a default return value is given. Two examples:

         1. A failure could arise when an unchecked module was looked up and
         not found declared, in which case we consider that module's exports to
         be `any`.

         2. A failure could arise also when an object property is looked up in
         a condition, in which case we consider the object's property to be
         `mixed`.
      *)
      let use_op = Option.value ~default:unknown_use (use_op_of_lookup_action action) in

      Option.iter test_opt ~f:(fun (id, reasons) ->
        (* TODO - as mentioned above, it's unclear why mixed is included in this case. Since you
         * can read any property from mixed, we don't want to treat it as a miss *)
        match l with
        | DefT (_, MixedT _) -> Context.test_prop_hit cx id
        | _ -> Context.test_prop_miss cx id (name_of_propref propref) reasons use_op
      );

      begin match t_opt with
      | Some (not_found, t) ->
        rec_unify cx trace ~use_op ~unify_any:true t not_found
      | None -> ()
      end

    (* SuperT only involves non-strict lookups *)
    | (DefT (_, NullT), SuperT _)
    | (ObjProtoT _, SuperT _)
    | (FunProtoT _, SuperT _) -> ()

    (** ExtendsT searches for a nominal superclass. The search terminates with
        either failure at the root or a structural subtype check. **)

    | DefT (_, AnyObjT), ExtendsUseT _ -> ()

    | DefT (lreason, ObjT { proto_t; _ }), ExtendsUseT _ ->
      let l = reposition cx ~trace (loc_of_reason lreason) proto_t in
      rec_flow cx trace (l, u)

    | DefT (reason, ClassT instance), ExtendsUseT _ ->
      let statics = Tvar.mk cx reason in
      rec_flow cx trace (instance, GetStaticsT (reason, statics));
      rec_flow cx trace (statics, u)

    | DefT (_, NullT),
      ExtendsUseT (use_op, reason, next::try_ts_on_failure, l, u) ->
      (* When seaching for a nominal superclass fails, we always try to look it
         up in the next element in the list try_ts_on_failure. *)
      rec_flow cx trace
        (next, ExtendsUseT (use_op, reason, try_ts_on_failure, l, u))

    | DefT (_, NullT),
      ExtendsUseT (use_op, _, [], l, DefT (reason_inst, InstanceT (_, super, _, {
        fields_tmap;
        methods_tmap;
        inst_call_t;
        structural = true;
        _;
      }))) ->
      structural_subtype cx trace ~use_op l reason_inst
        (fields_tmap, methods_tmap, inst_call_t);
      rec_flow cx trace (l, UseT (use_op, super))

    | DefT (_, NullT),
      ExtendsUseT (use_op, _, [], t, tc) ->
      let reason_l, reason_u = Flow_error.ordered_reasons (reason_of_t t, reason_of_t tc) in
      add_output cx ~trace (FlowError.EIncompatibleWithUseOp (reason_l, reason_u, use_op))

    (*******************************)
    (* ToString abstract operation *)
    (*******************************)

    (* ToStringT passes through strings unchanged, and flows a generic StrT otherwise *)

    | DefT (_, StrT _), ToStringT (_, t_out) ->
      rec_flow cx trace (l, t_out)

    | _, ToStringT (reason_op, t_out) ->
      rec_flow cx trace (StrT.why reason_op, t_out)

    (**********************)
    (* Array library call *)
    (**********************)

    | DefT (reason, ArrT (ArrayAT(t, _))),
      (GetPropT _ | SetPropT _ | MethodT _ | LookupT _) ->
      rec_flow cx trace (get_builtin_typeapp cx ~trace reason "Array" [t], u)

    | DefT (reason, ArrT (TupleAT _ | ROArrayAT _ | EmptyAT as arrtype)),
      (GetPropT _ | SetPropT _ | MethodT _ | LookupT _) ->
      let t = elemt_of_arrtype reason arrtype in
      rec_flow
        cx trace (get_builtin_typeapp cx ~trace reason "$ReadOnlyArray" [t], u)

    (***********************)
    (* String library call *)
    (***********************)

    | DefT (reason, StrT _), u when primitive_promoting_use_t u ->
      rec_flow cx trace (get_builtin_type cx ~trace reason "String",u)

    (***********************)
    (* Number library call *)
    (***********************)

    | DefT (reason, NumT _), u when primitive_promoting_use_t u ->
      rec_flow cx trace (get_builtin_type cx ~trace reason "Number",u)

    (***********************)
    (* Boolean library call *)
    (***********************)

    | DefT (reason, BoolT _), u when primitive_promoting_use_t u ->
      rec_flow cx trace (get_builtin_type cx ~trace reason "Boolean",u)

    (*************************)
    (* Function library call *)
    (*************************)

    | FunProtoT reason, _ ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx ~trace reason ~use_desc "Function" in
      rec_flow cx trace (fun_proto, u)

    | _, UseT (use_op, FunProtoT reason) ->
      let use_desc = true in
      let fun_proto = get_builtin_type cx ~trace reason ~use_desc "Function" in
      rec_flow cx trace (l, UseT (use_op, fun_proto))

    (***********************)
    (* Object library call *)
    (***********************)

    | ObjProtoT reason, _ ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx ~trace reason ~use_desc "Object" in
      rec_flow cx trace (obj_proto, u)

    | _, UseT (use_op, ObjProtoT reason) ->
      let use_desc = true in
      let obj_proto = get_builtin_type cx ~trace reason ~use_desc "Object" in
      rec_flow cx trace (l, UseT (use_op, obj_proto))

    (* Special cases of FunT *)
    | FunProtoApplyT reason, _
    | FunProtoBindT reason, _
    | FunProtoCallT reason, _ ->
      rec_flow cx trace (FunProtoT reason, u)

    | _, LookupT (_, _, _, propref, lookup_action) ->
      let use_op = use_op_of_lookup_action lookup_action in
      add_output cx ~trace (FlowError.EIncompatibleProp {
        prop = (match propref with Named (_, name) -> Some name | Computed _ -> None);
        reason_prop = reason_of_propref propref;
        reason_obj = reason_of_t l;
        special = flow_error_kind_of_lower l;
        use_op;
      })

    | _, UseT (use_op, u) ->
      add_output cx ~trace (FlowError.EIncompatibleWithUseOp (
        reason_of_t l, reason_of_t u, use_op
      ))

    | _ ->
      add_output cx ~trace (FlowError.EIncompatible {
        lower = (reason_of_t l, flow_error_kind_of_lower l);
        upper = (reason_of_use_t u, flow_error_kind_of_upper u);
        use_op = use_op_of_use_t u;
        branches = [];
      })
  )

and flow_error_kind_of_lower = function
  | DefT (_, NullT) -> Some Flow_error.Possibly_null
  | DefT (_, VoidT) -> Some Flow_error.Possibly_void
  | DefT (_, MaybeT _) -> Some Flow_error.Possibly_null_or_void
  | DefT (_, IntersectionT _)
  | DefT (_, MixedT Empty_intersection) -> Some Flow_error.Incompatible_intersection
  | _ -> None

and flow_error_kind_of_upper = function
  | GetPropT (_, _, Named (r, name), _) -> FlowError.IncompatibleGetPropT (loc_of_reason r, Some name)
  | GetPropT (_, _, Computed t, _) -> FlowError.IncompatibleGetPropT (loc_of_t t, None)
  | GetPrivatePropT (_, _, _, _, _, _) -> FlowError.IncompatibleGetPrivatePropT
  | SetPropT (_, _, Named (r, name), _, _, _) -> FlowError.IncompatibleSetPropT (loc_of_reason r, Some name)
  | SetPropT (_, _, Computed t, _, _, _) -> FlowError.IncompatibleSetPropT (loc_of_t t, None)
  | SetPrivatePropT (_, _, _, _, _, _, _) -> FlowError.IncompatibleSetPrivatePropT
  | MethodT (_, _, _, Named (r, name), _, _) -> FlowError.IncompatibleMethodT (loc_of_reason r, Some name)
  | MethodT (_, _, _, Computed t, _, _) -> FlowError.IncompatibleMethodT (loc_of_t t, None)
  | CallT _ -> FlowError.IncompatibleCallT
  | ConstructorT _ -> FlowError.IncompatibleConstructorT
  | GetElemT (_, _, t, _) -> FlowError.IncompatibleGetElemT (loc_of_t t)
  | SetElemT (_, _, t, _, _) -> FlowError.IncompatibleSetElemT (loc_of_t t)
  | CallElemT (_, _, t, _) -> FlowError.IncompatibleCallElemT (loc_of_t t)
  | ElemT (_, _, DefT (_, ArrT _), _) -> FlowError.IncompatibleElemTOfArrT
  | ObjAssignFromT (_, _, _, ObjSpreadAssign) -> FlowError.IncompatibleObjAssignFromTSpread
  | ObjAssignFromT _ -> FlowError.IncompatibleObjAssignFromT
  | ObjRestT _ -> FlowError.IncompatibleObjRestT
  | ObjSealT _ -> FlowError.IncompatibleObjSealT
  | ArrRestT _ -> FlowError.IncompatibleArrRestT
  | SuperT _ -> FlowError.IncompatibleSuperT
  | MixinT _ -> FlowError.IncompatibleMixinT
  | SpecializeT _ -> FlowError.IncompatibleSpecializeT
  | ConcretizeTypeAppsT _ -> FlowError.IncompatibleSpecializeT
  | ThisSpecializeT _ -> FlowError.IncompatibleThisSpecializeT
  | VarianceCheckT _ -> FlowError.IncompatibleVarianceCheckT
  | GetKeysT _ -> FlowError.IncompatibleGetKeysT
  | HasOwnPropT (_, r, Literal (_, name)) -> FlowError.IncompatibleHasOwnPropT (loc_of_reason r, Some name)
  | HasOwnPropT (_, r, _) -> FlowError.IncompatibleHasOwnPropT (loc_of_reason r, None)
  | GetValuesT _ -> FlowError.IncompatibleGetValuesT
  | UnaryMinusT _ -> FlowError.IncompatibleUnaryMinusT
  | MapTypeT (_, (ObjectMap _ | ObjectMapi _), _) -> FlowError.IncompatibleMapTypeTObject
  | TypeAppVarianceCheckT _ -> FlowError.IncompatibleTypeAppVarianceCheckT
  | GetStaticsT _ -> FlowError.IncompatibleGetStaticsT
  | use_t -> FlowError.IncompatibleUnclassified (string_of_use_ctor use_t)

and use_op_of_lookup_action = function
  | RWProp (use_op, _, _, _) -> Some use_op
  | LookupProp (use_op, _) -> Some use_op
  | SuperProp (use_op, _) -> Some use_op
  | _ -> None

(* some types need to be resolved before proceeding further *)
and needs_resolution = function
  | OpenT _ | DefT (_, UnionT _) | DefT (_, OptionalT _) | DefT (_, MaybeT _) | AnnotT _ -> true
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
and flow_addition cx trace use_op reason flip l r u =
  if needs_resolution r then rec_flow cx trace (r, AdderT (use_op, reason, not flip, l, u)) else
  let (l, r) = if flip then (r, l) else (l, r) in
  (* disable ops because the left and right sides should already be
     repositioned. *)
  begin match l, r with
  | DefT (_, StrT _), DefT (_, StrT _)
  | DefT (_, StrT _), DefT (_, NumT _)
  | DefT (_, NumT _), DefT (_, StrT _) ->
    rec_flow_t cx trace (StrT.why reason, u)

  (* unreachable additions are unreachable *)
  | DefT (_, EmptyT), _
  | _, DefT (_, EmptyT) ->
    rec_flow_t cx trace (EmptyT.why reason, u)

  | DefT (reason, MixedT _), _
  | _, DefT (reason, MixedT _) ->
    add_output cx ~trace (FlowError.EAdditionMixed (reason, use_op))

  | DefT (_, (NumT _ | BoolT _ | NullT | VoidT)),
    DefT (_, (NumT _ | BoolT _ | NullT | VoidT)) ->
    rec_flow_t cx trace (NumT.why reason, u)

  | DefT (_, StrT _), _ ->
    rec_flow cx trace (r, UseT (use_op, l));
    rec_flow_t cx trace (StrT.why reason, u);

  | _, DefT (_, StrT _) ->
    rec_flow cx trace (l, UseT (use_op, r));
    rec_flow_t cx trace (StrT.why reason, u);

  | DefT (_, AnyT), _
  | _, DefT (_, AnyT) ->
    rec_flow_t cx trace (AnyT.why reason, u)

  | DefT (_, NumT _), _ ->
    rec_flow cx trace (r, UseT (use_op, l));
    rec_flow_t cx trace (NumT.why reason, u);

  | _, DefT (_, NumT _) ->
    rec_flow cx trace (l, UseT (use_op, r));
    rec_flow_t cx trace (NumT.why reason, u);

  | (_, _) ->
    let fake_str = StrT.why reason in
    rec_flow cx trace (l, UseT (use_op, fake_str));
    rec_flow cx trace (r, UseT (use_op, fake_str));
    rec_flow cx trace (fake_str, UseT (use_op, u));
  end;

(**
 * relational comparisons like <, >, <=, >=
 *
 * typecheck iff either of the following hold:
 *   number <> number = number
 *   string <> string = string
 **)
and flow_comparator cx trace reason flip l r =
  if needs_resolution r then rec_flow cx trace (r, ComparatorT (reason, not flip, l)) else
  let (l, r) = if flip then (r, l) else (l, r) in
  match l, r with
  | DefT (_, StrT _), DefT (_, StrT _) -> ()
  | (_, _) when numeric l && numeric r -> ()
  | (_, _) ->
    let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
    add_output cx ~trace (FlowError.EComparison reasons)

(**
 * == equality
 *
 * typecheck iff they intersect (otherwise, unsafe coercions may happen).
 *
 * note: any types may be compared with === (in)equality.
 **)
and flow_eq cx trace reason flip l r =
  if needs_resolution r then rec_flow cx trace (r, EqT(reason, not flip, l)) else
  let (l, r) = if flip then (r, l) else (l, r) in
  if equatable (l, r) then ()
  else
    let reasons = FlowError.ordered_reasons (reason_of_t l, reason_of_t r) in
    add_output cx ~trace (FlowError.EComparison reasons)


and flow_obj_to_obj cx trace ~use_op (lreason, l_obj) (ureason, u_obj) =
  let {
    flags = lflags;
    dict_t = ldict;
    call_t = lcall;
    props_tmap = lflds;
    proto_t = lproto;
  } = l_obj in
  let {
    flags = rflags;
    dict_t = udict;
    call_t = ucall;
    props_tmap = uflds;
    proto_t = uproto;
  } = u_obj in

  (* if inflowing type is literal (thus guaranteed to be
     unaliased), propertywise subtyping is sound *)
  let lit = is_literal_object_reason lreason || lflags.frozen in

  (* If both are dictionaries, ensure the keys and values are compatible
     with each other. *)
  (match ldict, udict with
    | Some {key = lk; value = lv; dict_polarity = lpolarity; _},
      Some {key = uk; value = uv; dict_polarity = upolarity; _} ->
      (* Don't report polarity errors when checking the indexer key. We would
       * report these errors again a second time when checking values. *)
      rec_flow_p cx trace ~report_polarity:false ~use_op:(Frame (IndexerKeyCompatibility {
        lower = lreason;
        upper = ureason;
      }, use_op)) lreason ureason (Computed uk)
        (Field (None, lk, lpolarity), Field (None, uk, upolarity));
      rec_flow_p cx trace ~use_op:(Frame (PropertyCompatibility {
        prop = None;
        lower = lreason;
        upper = ureason;
        is_sentinel = false;
      }, use_op)) lreason ureason (Computed uv)
        (Field (None, lv, lpolarity), Field (None, uv, upolarity))
    | _ -> ());

  if rflags.exact && rflags.sealed = Sealed && not (is_literal_object_reason ureason)
  then (
    iter_real_props cx lflds (fun ~is_sentinel s _ ->
      if not (Context.has_prop cx uflds s)
      then (
        let use_op = Frame (PropertyCompatibility {
          prop = Some s;
          (* Lower and upper are reversed in this case since the lower object
           * is the one requiring the prop. *)
          lower = ureason;
          upper = lreason;
          is_sentinel;
        }, use_op) in
        let reason_prop = replace_reason_const (RProperty (Some s)) lreason in
        let err = FlowError.EPropNotFound (Some s, (reason_prop, ureason), use_op) in
        add_output cx ~trace err
      )
    );
    Option.iter lcall ~f:(fun _ ->
      if Option.is_none ucall
      then (
        let prop = Some "$call" in
        let use_op = Frame (PropertyCompatibility {
          prop;
          (* Lower and upper are reversed in this case since the lower object
           * is the one requiring the prop. *)
          lower = ureason;
          upper = lreason;
          is_sentinel = false;
        }, use_op) in
        let reason_prop = replace_reason_const (RProperty prop) lreason in
        let err = FlowError.EPropNotFound (prop, (reason_prop, ureason), use_op) in
        add_output cx ~trace err
      )
    )
  );

  (match ucall with
  | Some ucall ->
    let prop_name = Some "$call" in
    let use_op = Frame (PropertyCompatibility {
      prop = prop_name;
      lower = lreason;
      upper = ureason;
      is_sentinel = false;
    }, use_op) in
    (match lcall with
    | Some lcall -> rec_flow cx trace (lcall, UseT (use_op, ucall))
    | None ->
      let reason_prop = replace_reason_const (RProperty prop_name) ureason in
      add_output cx ~trace (FlowError.EStrictLookupFailed
        ((reason_prop, lreason), lreason, prop_name, Some use_op)))
  | None -> ());

  (* Properties in u must either exist in l, or match l's indexer. *)
  iter_real_props cx uflds (fun ~is_sentinel s up ->
    let reason_prop = replace_reason_const (RProperty (Some s)) ureason in
    let propref = Named (reason_prop, s) in
    let use_op' = use_op in
    let use_op = Frame (PropertyCompatibility {
      prop = Some s;
      lower = lreason;
      upper = ureason;
      is_sentinel;
    }, use_op') in
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
      rec_flow cx trace (string_key s reason_prop, UseT (
        Frame (IndexerKeyCompatibility {lower = lreason; upper = ureason}, use_op'),
        key
      ));
      let lp = Field (None, value, dict_polarity) in
      let up = match up with
      | Field (loc, DefT (_, OptionalT ut), upolarity) ->
        Field (loc, ut, upolarity)
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
      | Field (_, DefT (_, OptionalT _), _) when lit ->
        (* if property is marked optional or otherwise has a maybe type,
           and if inflowing type is a literal (i.e., it is not an
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
        let strict = match Obj_type.sealed_in_op ureason lflags.sealed, ldict with
        | false, None -> ShadowRead (Some lreason, Nel.one lflds)
        | true, None -> Strict lreason
        | _ -> NonstrictReturning (None, None)
        in
        rec_flow cx trace (lproto,
          LookupT (ureason, strict, [], propref,
            LookupProp (use_op, up)))
        (* TODO: instead, consider extending inflowing type with s:t2 when it
           is not sealed *)
  );

  (* Any properties in l but not u must match indexer *)
  (match udict with
  | None -> ()
  | Some { key; value; dict_polarity; _ } ->
    iter_real_props cx lflds (fun ~is_sentinel s lp ->
      if not (Context.has_prop cx uflds s)
      then (
        rec_flow cx trace (string_key s lreason, UseT (
          Frame (IndexerKeyCompatibility {lower = lreason; upper = ureason}, use_op),
          key
        ));
        let use_op = Frame (PropertyCompatibility {
          prop = Some s;
          lower = lreason;
          upper = ureason;
          is_sentinel;
        }, use_op) in
        let lp = match lp with
        | Field (loc, DefT (_, OptionalT lt), lpolarity) ->
          Field (loc, lt, lpolarity)
        | _ -> lp
        in
        let up = Field (None, value, dict_polarity) in
        if lit
        then
          match Property.read_t lp, Property.read_t up with
          | Some lt, Some ut -> rec_flow cx trace (lt, UseT (use_op, ut))
          | _ -> ()
        else
          let reason_prop = replace_reason_const (RProperty (Some s)) lreason in
          let propref = Named (reason_prop, s) in
          rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)));

      (* Previously, call properties were stored in the props map, and were
         checked against dictionary upper bounds. This is wrong, but useful for
         distinguishing between thunk-like types found in graphql-js.

         Now that call properties are stored separately, it is particularly
         egregious to emit this constraint. This only serves to maintain buggy
         behavior, which should be fixed, and this code removed. *)
      (match lcall, ucall with
      | Some lcall, None ->
        let s = "$call" in
        let use_op = Frame (PropertyCompatibility {
          prop = Some s;
          lower = lreason;
          upper = ureason;
          is_sentinel = false;
        }, use_op) in
        let lp = match lcall with
        | DefT (_, OptionalT t) -> Field (None, t, Positive)
        | _ -> Field (None, lcall, Positive)
        in
        let up = Field (None, value, dict_polarity) in
        if lit
        then
          match Property.read_t lp, Property.read_t up with
          | Some lt, Some ut -> rec_flow cx trace (lt, UseT (use_op, ut))
          | _ -> ()
        else
          let reason_prop = replace_reason_const (RProperty (Some s)) lreason in
          let propref = Named (reason_prop, s) in
          rec_flow_p cx trace ~use_op lreason ureason propref (lp, up)
      | _ -> ());
  );

  rec_flow cx trace (uproto,
    ReposUseT (ureason, false, use_op, DefT (lreason, ObjT l_obj)))

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
  | _ -> false

(* common case checking a function as an object *)
and quick_error_fun_as_obj cx trace ~use_op reason statics reason_o props =
  let statics_own_props = match statics with
    | DefT (_, ObjT { props_tmap; _ }) -> Some (Context.find_props cx props_tmap)
    | DefT (_, AnyFunT)
    | DefT (_, MixedT _) -> Some SMap.empty
    | _ -> None
  in
  match statics_own_props with
  | Some statics_own_props ->
    let props_not_found = SMap.filter (fun x p ->
      let optional = match p with
      | Field (_, DefT (_, OptionalT _), _) -> true
      |_ -> false
      in
      not (
        optional ||
        is_function_prototype x ||
        SMap.mem x statics_own_props
      )
    ) props in
    SMap.iter (fun x _ ->
      let use_op = Frame (PropertyCompatibility {
        prop = Some x;
        lower = reason;
        upper = reason_o;
        is_sentinel = false;
      }, use_op) in
      let reason_prop =
        replace_reason (fun desc -> RPropertyOf (x, desc)) reason_o in
      let err = FlowError.EPropNotFound (Some x, (reason_prop, reason), use_op) in
      add_output cx ~trace err
    ) props_not_found;
    not (SMap.is_empty props_not_found)
  | None -> false

(* NOTE: The following function looks similar to TypeUtil.quick_subtype, but is in fact more
   complicated: it avoids deep structural checks, admits `any`, etc. It might be worth it to
   simplify this function later. *)
and ground_subtype = function
  | TypeDestructorTriggerT _, UseT (_, TypeDestructorTriggerT _) -> false
  (* tvars are not considered ground, so they're not part of this relation *)
  | (OpenT _, _) | (_, UseT (_, OpenT _)) -> false

  (* Allow any lower bound to be repositioned *)
  | (_, ReposLowerT _) -> false
  | (_, ReposUseT _) -> false

  | (_, ObjKitT _) -> false

  | (_, ChoiceKitUseT _) -> false

  (* Allow deferred unification with `any` *)
  | (_, UnifyT _) -> false

  (* Allow any propagation to dictionaries *)
  | DefT (_, AnyT), ElemT _ -> false

  | DefT (_, UnionT _), _ -> false

  (* Allow EmptyT ~> CondT *)
  | (_, CondT _) -> false

  | _, MakeExactT _ -> false

  | DefT (_, NumT _), UseT (_, DefT (_, NumT _))
  | DefT (_, StrT _), UseT (_, DefT (_, StrT _))
  | DefT (_, BoolT _), UseT (_, DefT (_, BoolT _))
  | DefT (_, NullT), UseT (_, DefT (_, NullT))
  | DefT (_, VoidT), UseT (_, DefT (_, VoidT))
  | DefT (_, EmptyT), _
  | _, UseT (_, DefT (_, MixedT _))
    -> true

  | DefT (_, AnyT), u -> not (any_propagating_use_t u)
  | _, UseT (_, DefT (_, AnyT)) -> true

  (* opt: avoid builtin lookups *)
  | ObjProtoT _, UseT (_, ObjProtoT _)
  | FunProtoT _, UseT (_, FunProtoT _)
  | FunProtoT _, UseT (_, ObjProtoT _)
  | DefT (_, ObjT {proto_t = ObjProtoT _; _}), UseT (_, ObjProtoT _)
  | DefT (_, ObjT {proto_t = FunProtoT _; _}), UseT (_, FunProtoT _)
  | DefT (_, ObjT {proto_t = FunProtoT _; _}), UseT (_, ObjProtoT _)
    -> true

  | _ ->
    false

and numeric = function
  | DefT (_, NumT _) -> true
  | DefT (_, SingletonNumT _) -> true

  | DefT (reason, InstanceT _) ->
    DescFormat.name_of_instance_reason reason = "Date"

  | _ -> false

and object_like = function
  | DefT (_, (AnyObjT | ObjT _ | InstanceT _)) -> true
  | t -> function_like t

and object_use = function
  | UseT (_, DefT (_, ObjT _)) -> true
  | _ -> false

and object_like_op = function
  | SetPropT _ | GetPropT _ | MethodT _ | LookupT _
  | GetProtoT _ | SetProtoT _
  | SuperT _
  | GetKeysT _ | HasOwnPropT _ | GetValuesT _
  | ObjAssignToT _ | ObjAssignFromT _ | ObjRestT _
  | SetElemT _ | GetElemT _
  | UseT (_, DefT (_, AnyObjT)) -> true
  | _ -> false

and function_use = function
  | UseT (_, DefT (_, FunT _)) -> true
  | _ -> false

(* TODO: why is AnyFunT missing? *)
and function_like = function
  | DefT (_, ClassT _)
  | DefT (_, FunT _)
  | CustomFunT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
    -> true
  | _ -> false

and function_like_op = function
  | CallT _ | UseT (_, DefT (_, TypeT _))
  | ConstructorT _
  | UseT (_, DefT (_, AnyFunT)) -> true
  | t -> object_like_op t

and equatable = function
  | DefT (_, NumT _), DefT (_, NumT _)
  | DefT (_, StrT _), DefT (_, StrT _)
  | DefT (_, BoolT _), DefT (_, BoolT _)
  | DefT (_, EmptyT), _ | _, DefT (_, EmptyT)
  | _, DefT (_, MixedT _) | DefT (_, MixedT _), _
  | DefT (_, AnyT), _ | _, DefT (_, AnyT)
  | DefT (_, VoidT), _ | _, DefT (_, VoidT)
  | DefT (_, NullT), _ | _, DefT (_, NullT)
    -> true

  | DefT (_, (NumT _ | StrT _ | BoolT _)), _
  | _, DefT (_, (NumT _ | StrT _ | BoolT _))
    -> false

  | _ -> true

(* Creates a union from a list of types. Since unions require a minimum of two
   types this function will return an empty type when there are no types in the
   list, or the list head when there is one type in the list. *)
and union_of_ts reason ts =
  match ts with
  (* If we have no types then this is an error. *)
  | [] -> DefT (reason, EmptyT)
  (* If we only have one type then only that should be used. *)
  | t0::[] -> t0
  (* If we have more than one type then we make a union type. *)
  | t0::t1::ts ->
      let union = UnionT (UnionRep.make t0 t1 ts) in
      DefT (reason, union)

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
  let mk_bot _ { name; reason; _ } =
    let desc = RPolyTest (name, RIncompatibleInstantiation name) in
    DefT (replace_reason_const desc reason, EmptyT)
  in
  (* make bound type for given param and argument map *)
  let mk_bound cx prev_args { bound; name; reason = param_reason; _ } =
    (* For the top bound, we match the reason locations that appear in the
     * respective bot bound:
     * - 'loc' is the location of the type parameter (may be repositioned later)
     * - 'def_loc' is the location of the type parameter, and
     * - 'annot_loc_opt' is the location of the bound (if present).
     *)
    mod_reason_of_t (fun bound_reason ->
      let param_loc = Reason.loc_of_reason param_reason in
      let annot_loc = annot_loc_of_reason bound_reason in
      let desc = desc_of_reason ~unwrap:false bound_reason in
      repos_reason param_loc ?annot_loc (mk_reason (RPolyTest (name, desc)) param_loc)
    ) (subst cx prev_args bound)
  in
  (* make argument map by folding mk_arg over param list *)
  let mk_argmap mk_arg =
    List.fold_left (fun acc ({ name; _ } as p) ->
      SMap.add name (mk_arg acc p) acc
    ) SMap.empty
  in
  (* for each p, a map with p bot and others bound + map with all bound *)
  let linear cx = function
  | [] -> [SMap.empty]
  | params ->
    let all = mk_argmap (mk_bound cx) params in
    let each = List.map (fun ({ name; _ } as p) ->
      SMap.add name (mk_bot SMap.empty p) all
    ) params in
    List.rev (all :: each)
  in
  (* a map for every combo of bot/bound params *)
  let powerset cx params arg_map =
    let none = mk_argmap mk_bot params in
    List.fold_left (fun maps ({ name; _ } as p) ->
      let bots = List.map (SMap.add name (SMap.find_unsafe name none)) maps in
      let bounds = List.map (fun m -> SMap.add name (mk_bound cx m p) m) maps in
      bots @ bounds
    ) [arg_map] params
  in
  (* main - run f over a collection of arg maps generated for params *)
  fun cx params f ->
    if params = [] then f SMap.empty else
    let is_free = function { bound = DefT (_, MixedT _); _ } -> true | _ -> false in
    let free_params, dep_params = List.partition is_free params in
    let free_sets = linear cx free_params in
    let powersets = List.map (powerset cx dep_params) free_sets in
    List.iter (TestID.run f) (List.flatten powersets)

(*********************)
(* inheritance utils *)
(*********************)

and flow_type_args cx trace ~use_op lreason ureason pmap tmap1 tmap2 =
  tmap1 |> SMap.iter (fun x (targ_reason, t1) ->
    let (_, t2) = SMap.find_unsafe x tmap2 in
    (* type_args contains a mixture of args to type params declared on the
       instance's class, and args to outer-scope type params.
       OTOH arg_polarities only holds polarities of declared params.
       it'll take some upstream refactoring to handle variance to in-scope
       type params - meanwhile, we fall back to neutral (invariant) *)
    let polarity = Option.value (SMap.get x pmap) ~default:Neutral in
    let use_op = Frame (TypeArgCompatibility {
      name = x;
      targ = targ_reason;
      lower = lreason;
      upper = ureason;
      polarity;
    }, use_op) in
    (match polarity with
    | Negative -> rec_flow cx trace (t2, UseT (use_op, t1))
    | Positive -> rec_flow cx trace (t1, UseT (use_op, t2))
    | Neutral -> rec_unify cx trace ~use_op t1 t2)
  )

and inherited_method x = x <> "constructor"

(* dispatch checks to verify that lower satisfies the structural
   requirements given in the tuple. *)
and structural_subtype cx trace ?(use_op=unknown_use) lower reason_struct
  (fields_pmap, methods_pmap, call_t) =
  let lreason = reason_of_t lower in
  let fields_pmap = Context.find_props cx fields_pmap in
  let methods_pmap = Context.find_props cx methods_pmap in
  fields_pmap |> SMap.iter (fun s p ->
    let use_op = Frame (PropertyCompatibility {
      prop = Some s;
      lower = lreason;
      upper = reason_struct;
      is_sentinel = false;
    }, use_op) in
    match p with
    | Field (_, DefT (_, OptionalT t), polarity) ->
      let propref =
        let reason_prop = replace_reason (fun desc ->
          ROptional (RPropertyOf (s, desc))
        ) reason_struct in
        Named (reason_prop, s)
      in
      rec_flow cx trace (lower,
        LookupT (reason_struct, NonstrictReturning (None, None), [], propref,
          LookupProp (use_op, Field (None, t, polarity))))
    | _ ->
      let propref =
        let reason_prop = replace_reason (fun desc ->
          RPropertyOf (s, desc)
        ) reason_struct in
        Named (reason_prop, s)
      in
      rec_flow cx trace (lower,
        LookupT (reason_struct, Strict lreason, [], propref,
          LookupProp (use_op, p)))
  );
  methods_pmap |> SMap.iter (fun s p ->
    let use_op = Frame (PropertyCompatibility {
      prop = Some s;
      lower = lreason;
      upper = reason_struct;
      is_sentinel = false;
    }, use_op) in
    let propref =
      let reason_prop = replace_reason (fun desc ->
        RPropertyOf (s, desc)
      ) reason_struct in
      Named (reason_prop, s)
    in
    rec_flow cx trace (lower,
      LookupT (reason_struct, Strict lreason, [], propref,
        LookupProp (use_op, p)))
  );
  call_t |> Option.iter ~f:(fun ut ->
    let prop_name = Some "$call" in
    let use_op = Frame (PropertyCompatibility {
      prop = prop_name;
      lower = lreason;
      upper = reason_struct;
      is_sentinel = false;
    }, use_op) in
    match lower with
    | DefT (_, ObjT {call_t = Some lt; _}) ->
      rec_flow cx trace (lt, UseT (use_op, ut))
    | DefT (_, InstanceT (_, _, _, {inst_call_t = Some lt; _})) ->
      rec_flow cx trace (lt, UseT (use_op, ut))
    | _ ->
      let reason_prop = replace_reason (fun desc ->
        RPropertyOf ("$call", desc)
      ) reason_struct in
      add_output cx ~trace (FlowError.EStrictLookupFailed
        ((reason_prop, lreason), lreason, prop_name, Some use_op))
  );

and check_super cx trace ~use_op lreason ureason t x p =
  let use_op = Frame (PropertyCompatibility {
    prop = Some x;
    lower = lreason;
    upper = replace_reason (function
      | RStatics desc -> desc
      | desc -> desc
    ) ureason;
    is_sentinel = false;
  }, use_op) in
  let strict = NonstrictReturning (None, None) in
  let reason_prop = replace_reason_const (RProperty (Some x)) lreason in
  lookup_prop cx trace t reason_prop lreason strict x (SuperProp (use_op, p))

(*****************)
(* substitutions *)
(*****************)

(** Substitute bound type variables with associated types in a type. Do not
    force substitution under polymorphic types. This ensures that existential
    type variables under a polymorphic type remain unevaluated until the
    polymorphic type is applied. **)
and subst =
  let substituter = object(self)
    inherit [Type.t SMap.t * bool * use_op option] Type_mapper.t as super
    method! type_ cx map_cx t =
      let (map, force, use_op) = map_cx in
      if SMap.is_empty map then t
      else match t with
      | BoundT typeparam ->
        begin match SMap.get typeparam.name map with
        | None -> t
        | Some param_t when typeparam.name = "this" ->
          ReposT (annot_reason typeparam.reason, param_t)
        | Some param_t ->
          (match desc_of_reason ~unwrap:false (reason_of_t param_t) with
          | RPolyTest _ ->
            mod_reason_of_t (fun reason ->
              annot_reason (repos_reason (loc_of_reason typeparam.reason) reason)
            ) param_t
          | _ ->
            param_t
          )
        end

      | ExistsT reason ->
        if force then Tvar.mk cx reason
        else t

      | DefT (reason, PolyT (xs, inner, _)) ->
        let xs, map, changed = List.fold_left (fun (xs, map, changed) typeparam ->
          let bound = self#type_ cx (map, force, use_op) typeparam.bound in
          let default = match typeparam.default with
          | None -> None
          | Some default ->
            let default_ = self#type_ cx (map, force, use_op) default in
            if default_ == default then typeparam.default else Some default_
          in
          { typeparam with bound; default; }::xs,
          SMap.remove typeparam.name map,
          changed || bound != typeparam.bound || default != typeparam.default
        ) ([], map, false) xs in
        let inner_ = self#type_ cx (map, false, None) inner in
        let changed = changed || inner_ != inner in
        if changed then DefT (reason, PolyT (List.rev xs, inner_, mk_id ())) else t

      | ThisClassT (reason, this) ->
        let map = SMap.remove "this" map in
        let this_ = self#type_ cx (map, force, use_op) this in
        if this_ == this then t else ThisClassT (reason, this_)

      | DefT (r, TypeAppT (op, c, ts)) ->
        let c' = self#type_ cx map_cx c in
        let ts' = ListUtils.ident_map (self#type_ cx map_cx) ts in
        if c == c' && ts == ts' then t else (
          (* If the TypeAppT changed then one of the type arguments had a
           * BoundT that was substituted. In this case, also change the use_op
           * so we can point at the op which instantiated the types that
           * were substituted. *)
          let use_op = Option.value use_op ~default:op in
          DefT (r, TypeAppT (use_op, c', ts'))
        )

      | EvalT (x, TypeDestructorT (op, r, d), _) ->
        let x' = self#type_ cx map_cx x in
        let d' = self#destructor cx map_cx d in
        if x == x' && d == d' then t
        else (
          (* If the EvalT changed then either the target or destructor had a
           * BoundT that was substituted. In this case, also change the use_op
           * so we can point at the op which instantiated the types that
           * were substituted. *)
          let use_op = Option.value use_op ~default:op in
          EvalT (x', TypeDestructorT (use_op, r, d'), Reason.mk_id ())
        )

      | ModuleT _
      | InternalT (ExtendsT _)
        ->
          failwith (spf "Unhandled type ctor: %s" (string_of_ctor t)) (* TODO *)

      | t -> super#type_ cx map_cx t

    method! predicate cx (map, force, use_op) p = match p with
    | LatentP (t, i) ->
      let t' = self#type_ cx (map, force, use_op) t in
      if t == t' then p else LatentP (t', i)
    | p -> p

    end in
  fun cx ?use_op ?(force=true) (map: Type.t SMap.t) ->
    substituter#type_ cx (map, force, use_op)

and eval_selector cx ?trace reason curr_t s i =
  let evaluated = Context.evaluated cx in
  match IMap.get i evaluated with
  | None ->
    Tvar.mk_where cx reason (fun tvar ->
      Context.set_evaluated cx (IMap.add i tvar evaluated);
      flow_opt cx ?trace (curr_t, match s with
      | Prop x -> GetPropT (unknown_use, reason, Named (reason, x), tvar)
      | Elem key -> GetElemT (unknown_use, reason, key, tvar)
      | ObjRest xs -> ObjRestT (reason, xs, tvar)
      | ArrRest i -> ArrRestT (unknown_use, reason, i, tvar)
      | Default -> PredicateT (NotP VoidP, tvar)
      | Become -> BecomeT (reason, tvar)
      | Refine p -> RefineT (reason, p, tvar)
      )
    )
  | Some it ->
    it

and mk_type_destructor cx ~trace use_op reason t d id =
  let evaluated = Context.evaluated cx in
  (* As an optimization, unwrap resolved tvars so that they are only evaluated
   * once to an annotation instead of a tvar that gets a bound on both sides. *)
  let t = match t with
    | OpenT (_, id) ->
      let _, constraints = Context.find_constraints cx id in
      (match constraints with
        | Resolved t -> t
        | _ -> t)
    | _ -> t
  in
  match t, IMap.get id evaluated with
  (* The OpenT branch is a correct implementation of type destructors for all
   * types. However, because it adds a constraint to both sides of a type we may
   * end up doing some work twice. So as an optimization for concrete types
   * we have a fall-through branch that only evaluates our type destructor once.
   * The second branch then uses AnnotT to both concretize the result for use
   * as a lower or upper bound and prevent new bounds from being added to
   * the result.
   *
   * MergedT should also get this treatment as it is a merged "description" of
   * an OpenT. *)
  | (OpenT _ | MergedT _), Some t -> false, t
  | (OpenT _ | MergedT _), None ->
    false, Tvar.mk_where cx reason (fun tvar ->
      Context.set_evaluated cx (IMap.add id tvar evaluated);
      let x = TypeDestructorTriggerT (use_op, reason, None, d, tvar) in
      rec_flow_t cx trace (t, x);
      rec_flow_t cx trace (x, t);
    )
  | _, Some t -> true, t
  | AnnotT (annot_t, use_desc), None ->
    true, Tvar.mk_where cx reason (fun tvar ->
      Context.set_evaluated cx (IMap.add id tvar evaluated);
      let repos = Some (reason_of_t annot_t, use_desc) in
      let x = TypeDestructorTriggerT (use_op, reason, repos, d, tvar) in
      rec_flow_t cx trace (annot_t, x);
    )
  | _, None ->
    true, Tvar.mk_where cx reason (fun tvar ->
      Context.set_evaluated cx (IMap.add id tvar evaluated);
      eval_destructor cx ~trace use_op reason t d tvar;
    )

and eval_destructor cx ~trace use_op reason t d tout = match t with
(* Specialize TypeAppTs before evaluating them so that we can handle special
   cases. Like the union case below. mk_typeapp_instance will return an AnnotT
   which will be fully resolved using the AnnotT case above. *)
| DefT (reason_tapp, TypeAppT (use_op_tapp, c, ts)) ->
  let destructor = TypeDestructorT (use_op, reason, d) in
  let t = mk_typeapp_instance cx ~trace ~use_op:use_op_tapp ~reason_op:reason ~reason_tapp c ts in
  rec_flow_t cx trace (EvalT (t, destructor, Cache.Eval.id t destructor), tout)
(* If we are destructuring a union, evaluating the destructor on the union
   itself may have the effect of splitting the union into separate lower
   bounds, which prevents the speculative match process from working.
   Instead, we preserve the union by pushing down the destructor onto the
   branches of the unions. *)
| DefT (r, UnionT rep) ->
  rec_flow_t cx trace (DefT (r, UnionT (rep |> UnionRep.ident_map (fun t ->
    let destructor = TypeDestructorT (use_op, reason, d) in
    EvalT (t, destructor, Cache.Eval.id t destructor)
  ))), tout)
| DefT (r, MaybeT t) ->
  let destructor = TypeDestructorT (use_op, reason, d) in
  let reason = replace_reason_const RNullOrVoid r in
  let rep = UnionRep.make
    (let null = NullT.make reason in EvalT (null, destructor, Cache.Eval.id null destructor))
    (let void = VoidT.make reason in EvalT (void, destructor, Cache.Eval.id void destructor))
    [EvalT (t, destructor, Cache.Eval.id t destructor)]
  in
  rec_flow_t cx trace (DefT (r, UnionT rep), tout)
| _ ->
  rec_flow cx trace (t, match d with
  | NonMaybeType ->
    let maybe_r = replace_reason (fun desc -> RMaybe desc) reason in
    (* We intentionally use `unknown_use` here! When we flow to a tout we never
     * want to carry a `use_op`. We want whatever `use_op` the tout is used with
     * to win. *)
    UseT (unknown_use, DefT (maybe_r, MaybeT tout))
  | PropertyType x ->
    let reason_op = replace_reason_const (RProperty (Some x)) reason in
    GetPropT (use_op, reason, Named (reason_op, x), tout)
  | ElementType t -> GetElemT (use_op, reason, t, tout)
  | Bind t -> BindT (use_op, reason, mk_methodcalltype t None [] tout, true)
  | SpreadType (options, todo_rev) ->
    let open Object in
    let open Object.Spread in
    let tool = Resolve Next in
    let state = { todo_rev; acc = [] } in
    ObjKitT (use_op, reason, tool, Spread (options, state), tout)
  | RestType (options, t) ->
    let open Object in
    let open Object.Rest in
    let tool = Resolve Next in
    let state = One t in
    ObjKitT (use_op, reason, tool, Rest (options, state), tout)
  | ReadOnlyType ->
    let open Object in
    ObjKitT (use_op, reason, Resolve (Next), ReadOnly, tout)
  | ValuesType -> GetValuesT (reason, tout)
  | CallType args ->
    let args = List.map (fun arg -> Arg arg) args in
    let call = mk_functioncalltype reason None args tout in
    let call = {call with call_strict_arity = false} in
    CallT (use_op, reason, call)
  | TypeMap tmap -> MapTypeT (reason, tmap, tout)
  | ReactElementPropsType -> ReactKitT (use_op, reason, React.GetProps tout)
  | ReactElementConfigType -> ReactKitT (use_op, reason, React.GetConfig tout)
  | ReactElementRefType -> ReactKitT (use_op, reason, React.GetRef tout)
  )

and match_this_binding map f =
  match SMap.find_unsafe "this" map with
  | ReposT (_, t) -> f t
  | _ -> failwith "not a this binding"

(* TODO: flesh this out *)
and check_polarity cx ?trace polarity = function
  (* base case *)
  | BoundT tp ->
    if not (Polarity.compat (tp.polarity, polarity))
    then add_output cx ?trace (FlowError.EPolarityMismatch {
      reason = tp.reason;
      name = tp.name;
      expected_polarity = tp.polarity;
      actual_polarity = polarity;
    })

  | OpenT _

  | DefT (_, NumT _)
  | DefT (_, StrT _)
  | DefT (_, BoolT _)
  | DefT (_, EmptyT)
  | DefT (_, MixedT _)
  | DefT (_, AnyT)
  | DefT (_, NullT)
  | DefT (_, VoidT)
  | DefT (_, SingletonStrT _)
  | DefT (_, SingletonNumT _)
  | DefT (_, SingletonBoolT _)
  | DefT (_, AnyObjT)
  | DefT (_, AnyFunT)
  | DefT (_, CharSetT _)
    -> ()
  | ExistsT _
    -> ()

  | InternalT (OptionalChainVoidT _) -> ()

  | DefT (_, OptionalT t)
  | ExactT (_, t)
  | DefT (_, MaybeT t)
  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t
  | ReposT (_, t)
  | InternalT (ReposUpperT (_, t))
    -> check_polarity cx ?trace polarity t

  | DefT (_, ClassT t)
    -> check_polarity cx ?trace polarity t

  | DefT (_, TypeT t)
    -> check_polarity cx ?trace polarity t

  | DefT (_, InstanceT (static, super, _, instance)) ->
    check_polarity cx ?trace polarity static;
    check_polarity cx ?trace polarity super;
    check_polarity_propmap cx ?trace polarity instance.fields_tmap;
    check_polarity_propmap cx ?trace ~skip_ctor:true polarity instance.methods_tmap

  | DefT (_, FunT (_, _, func)) ->
    let f = check_polarity cx ?trace (Polarity.inv polarity) in
    List.iter (fun (_, t) -> f t) func.params;
    Option.iter ~f:(fun (_, _, t) -> f t) func.rest_param;
    check_polarity cx ?trace polarity func.return_t

  | DefT (_, ArrT (ArrayAT (elemt, _))) ->
    check_polarity cx ?trace Neutral elemt

  | DefT (_, ArrT (TupleAT (_, tuple_types))) ->
    List.iter (check_polarity cx ?trace Neutral) tuple_types

  | DefT (_, ArrT (ROArrayAT (elemt))) ->
    check_polarity cx ?trace polarity elemt

  | DefT (_, ArrT EmptyAT) -> ()

  | DefT (_, ObjT obj) ->
    check_polarity_propmap cx ?trace polarity obj.props_tmap;
    (match obj.dict_t with
    | Some { key; value; dict_polarity; _ } ->
      check_polarity cx ?trace Neutral key;
      check_polarity cx ?trace (Polarity.mult (polarity, dict_polarity)) value
    | None -> ())

  | InternalT (IdxWrapper (_, obj)) -> check_polarity cx ?trace polarity obj

  | DefT (_, UnionT rep) ->
    List.iter (check_polarity cx ?trace polarity) (UnionRep.members rep)

  | DefT (_, IntersectionT rep) ->
    List.iter (check_polarity cx ?trace polarity) (InterRep.members rep)

  | DefT (_, PolyT (xs, t, _)) ->
    List.iter (check_polarity_typeparam cx ?trace polarity) xs;
    check_polarity cx ?trace polarity t

  | ThisTypeAppT (_, c, _, None) ->
    check_polarity cx ?trace Positive c

  | ThisTypeAppT (_, c, _, Some ts)
  | DefT (_, TypeAppT (_, c, ts))
    ->
    check_polarity cx ?trace Positive c;
    check_polarity_typeapp cx ?trace polarity c ts

  | OpaqueT (_, opaquetype) ->
    Option.iter ~f:(check_polarity cx ?trace polarity) opaquetype.underlying_t;
    Option.iter ~f:(check_polarity cx ?trace polarity) opaquetype.super_t

  | ShapeT t ->
    check_polarity cx ?trace polarity t

  | KeysT (_, t) ->
    check_polarity cx ?trace Positive t

  | ThisClassT _
  | ModuleT _
  | AnnotT _
  | MatchingPropT _
  | NullProtoT _
  | ObjProtoT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | EvalT _
  | InternalT (ExtendsT _)
  | InternalT (ChoiceKitT _)
  | TypeDestructorTriggerT _
  | CustomFunT _
  | OpenPredT _
  | MergedT _
    -> () (* TODO *)

and check_polarity_propmap cx ?trace ?(skip_ctor=false) polarity id =
  let pmap = Context.find_props cx id in
  SMap.iter (fun x p ->
    if skip_ctor && x = "constructor"
    then ()
    else check_polarity_prop cx ?trace polarity p
  ) pmap

and check_polarity_prop cx ?trace polarity = function
  | Field (_, t, p) -> check_polarity cx ?trace (Polarity.mult (polarity, p)) t
  | Get (_, t) -> check_polarity cx ?trace polarity t
  | Set (_, t) -> check_polarity cx ?trace (Polarity.inv polarity) t
  | GetSet (_, t1, _, t2) ->
    check_polarity cx ?trace polarity t1;
    check_polarity cx ?trace (Polarity.inv polarity) t2
  | Method (_, t) -> check_polarity cx ?trace polarity t

and check_polarity_typeparam cx ?trace polarity tp =
  let polarity = Polarity.mult (polarity, tp.polarity) in
  check_polarity cx ?trace polarity tp.bound;
  Option.iter ~f:(check_polarity cx ?trace polarity) tp.default

and check_polarity_typeapp cx ?trace polarity c ts =
  let reason = replace_reason (fun desc ->
    RVarianceCheck desc
  ) (reason_of_t c) in
  flow_opt cx ?trace (c, VarianceCheckT(reason, ts, polarity))

and variance_check cx ?trace polarity = function
  | [], _ | _, [] ->
    (* ignore typeapp arity mismatch, since it's handled elsewhere *)
    ()
  | tp::tps, t::ts ->
    check_polarity cx ?trace (Polarity.mult (polarity, tp.polarity)) t;
    variance_check cx ?trace polarity (tps, ts)

and poly_minimum_arity xs =
  List.filter (fun typeparam -> typeparam.default = None) xs
  |> List.length

(* Instantiate a polymorphic definition given type arguments. *)
and instantiate_poly_with_targs
  cx
  trace
  ~use_op
  ~reason_op
  ~reason_tapp
  ?cache
  ?errs_ref
  (xs,t)
  ts
  =
  let minimum_arity = poly_minimum_arity xs in
  let maximum_arity = List.length xs in
  let reason_arity = mk_poly_arity_reason xs in
  if List.length ts > maximum_arity
  then begin
    add_output cx ~trace (FlowError.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity));
    Option.iter errs_ref
      ~f:(fun errs_ref -> errs_ref := `ETooManyTypeArgs(reason_arity, maximum_arity)::!errs_ref)
  end;
  let map, _ = List.fold_left
    (fun (map, ts) typeparam ->
      let t, ts = match typeparam, ts with
      | {default=Some default; _;}, [] ->
          (* fewer arguments than params and we have a default *)
          subst cx ~use_op map default, []
      | {default=None; _;}, [] ->
          (* fewer arguments than params but no default *)
          add_output cx ~trace (FlowError.ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity));
          Option.iter errs_ref
            ~f:(fun errs_ref -> errs_ref := `ETooFewTypeArgs(reason_arity, minimum_arity)::!errs_ref);
          DefT (reason_op, AnyT), []
      | _, t::ts ->
          t, ts in
      let t_ = cache_instantiate cx trace ?cache typeparam reason_op reason_tapp t in
      let frame = Frame (TypeParamBound {
        name = typeparam.name;
      }, use_op) in
      rec_flow_t cx trace ~use_op:frame (t_, subst cx ~use_op map typeparam.bound);
      SMap.add typeparam.name t_ map, ts
    )
    (SMap.empty, ts)
    xs in
  reposition cx ~trace (loc_of_reason reason_tapp) (subst cx ~use_op map t)

(* Given a type parameter, a supplied type argument for specializing it, and a
   reason for specialization, either return the type argument or, when directed,
   look up the instantiation cache for an existing type argument for the same
   purpose and unify it with the supplied type argument. *)
and cache_instantiate cx trace ?cache typeparam reason_op reason_tapp t =
  match cache with
  | None -> t
  | Some rs ->
    let t_ = Cache.PolyInstantiation.find cx reason_tapp typeparam (reason_op, rs) in
    rec_unify cx trace ~use_op:unknown_use ~unify_any:true t t_;
    t_

(* Instantiate a polymorphic definition with stated bound or 'any' for args *)
(* Needed only for `instanceof` refis and React.PropTypes.instanceOf types *)
and instantiate_poly_default_args cx trace ~use_op ~reason_op ~reason_tapp (xs,t) =
  (* Remember: other_bound might refer to other type params *)
  let ts, _ = List.fold_left
    (fun (ts, map) typeparam ->
      let t = match typeparam.bound with
      | DefT (_, MixedT _) -> AnyT.why reason_op
      | other_bound -> AnyWithUpperBoundT (subst cx ~use_op map other_bound) in
      (t::ts, SMap.add typeparam.name t map)
    ) ([], SMap.empty)
    xs in
  let ts = List.rev ts in
  instantiate_poly_with_targs cx trace ~use_op ~reason_op ~reason_tapp (xs,t) ts

(* Instantiate a polymorphic definition by creating fresh type arguments. *)
and instantiate_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache (xs,t) =
  let ts = xs |> List.map (fun typeparam ->
    ImplicitTypeArgument.mk_targ cx typeparam reason_op
  ) in
  instantiate_poly_with_targs cx trace ~use_op ~reason_op ~reason_tapp ?cache (xs,t) ts

(* instantiate each param of a polymorphic type with its upper bound *)
and instantiate_poly_param_upper_bounds cx typeparams =
  let _, revlist = List.fold_left (
    fun (map, list) { name; bound; _ } ->
      let t = subst cx map bound in
      SMap.add name t map, t :: list
    ) (SMap.empty, []) typeparams in
  List.rev revlist

and mk_poly_arity_reason xs =
  let x1, xN = List.hd xs, List.hd (List.rev xs) in
  let loc = Loc.btwn (loc_of_reason x1.reason) (loc_of_reason xN.reason) in
  mk_reason (RCustom "See type parameters of definition here") loc

(* Fix a this-abstracted instance type by tying a "knot": assume that the
   fixpoint is some `this`, substitute it as This in the instance type, and
   finally unify it with the instance type. Return the class type wrapping the
   instance type. *)
and fix_this_class cx trace reason (r, i) =
  let i' = match Cache.Fix.find reason i with
    | Some i' -> i'
    | None ->
      let this = Tvar.mk cx reason in
      let i' = subst cx (SMap.singleton "this" this) i in
      Cache.Fix.add reason i i';
      rec_unify cx trace ~use_op:unknown_use this i';
      i'
  in
  DefT (r, ClassT i')

and canonicalize_imported_type cx trace reason t =
  match t with
  | DefT (_, ClassT inst) ->
    Some (DefT (reason, TypeT inst))

  | DefT (_, FunT (_, prototype, _)) ->
    Some (DefT (reason, TypeT prototype))

  | DefT (_, PolyT (typeparams, DefT (_, ClassT inst), id)) ->
    Some (poly_type id typeparams (DefT (reason, TypeT inst)))

  | DefT (_, PolyT (typeparams, DefT (_, FunT (_, prototype, _)), id)) ->
    Some (poly_type id typeparams (DefT (reason, TypeT prototype)))

  (* delay fixing a polymorphic this-abstracted class until it is specialized,
     by transforming the instance type to a type application *)
  | DefT (_, PolyT (typeparams, ThisClassT _, _)) ->
    let targs = List.map (fun tp -> BoundT tp) typeparams in
    Some (poly_type (mk_id ()) typeparams (class_type (typeapp t targs)))

  | DefT (_, PolyT (_, DefT (_, TypeT _), _)) ->
    Some t

  (* fix this-abstracted class when used as a type *)
  | ThisClassT(r, i) ->
    Some (fix_this_class cx trace reason (r, i))

  | DefT (_, TypeT _) ->
    Some t

  | DefT (_, AnyT) ->
    Some t

  | _ ->
    None

(* Specialize This in a class. Eventually this causes substitution. *)
and instantiate_this_class cx trace reason tc this =
  Tvar.mk_where cx reason (fun tvar ->
    rec_flow cx trace (tc, ThisSpecializeT (reason, this, tvar))
  )

(* Specialize targs in a class. This is somewhat different from
   mk_typeapp_instance, in that it returns the specialized class type, not the
   specialized instance type. *)
and specialize_class cx trace ~reason_op ~reason_tapp c = function
  | None -> c
  | Some ts ->
    Tvar.mk_where cx reason_op (fun tvar ->
      rec_flow cx trace (c, SpecializeT (unknown_use, reason_op, reason_tapp, None, Some ts, tvar))
    )

(* Object assignment patterns. In the `Object.assign` model (chain_objects), an
   existing object receives properties from other objects. This pattern suffers
   from "races" in the type checker, since the object supposed to receive
   properties is available even when the other objects supplying the properties
   are not yet available. *)

and chain_objects cx ?trace reason this those =
  let result = List.fold_left (fun result that ->
    let that, kind = match that with
    | Arg t -> t, default_obj_assign_kind
    | SpreadArg t ->
        (* If someone does Object.assign({}, ...Array<obj>) we can treat it like
           Object.assign({}, obj). *)
        t, ObjSpreadAssign
    in
    Tvar.mk_where cx reason (fun t ->
      flow_opt cx ?trace (result, ObjAssignToT(reason, that, t, kind));
    )
  ) this those in
  reposition cx ?trace (loc_of_reason reason) result

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

and try_union cx trace use_op l reason rep =
  let ts = UnionRep.members rep in
  let speculation_id = mk_id() in
  Speculation.init_speculation cx speculation_id;

  (* collect parts of the union type to be fully resolved *)
  let imap =
    (* since any final optimization must have happened after full resolution *)
    if UnionRep.is_optimized_finally rep then IMap.empty
    else ResolvableTypeJob.collect_of_types cx reason IMap.empty ts in
  (* collect parts of the lower bound to be fully resolved, while logging
     unresolved tvars *)
  let imap = ResolvableTypeJob.collect_of_type
    ~log_unresolved:speculation_id cx reason imap l in
  (* fully resolve the collected types *)
  resolve_bindings_init cx trace reason (bindings_of_jobs cx trace imap) @@
  (* ...and then begin the choice-making process *)
    try_flow_continuation cx trace reason speculation_id (UnionCases (use_op, l, rep, ts))

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
      (ConcretizeTypes (unresolved, resolved, DefT (r, IntersectionT rep), u)))

(* some patterns need to be concretized before proceeding further *)
and patt_that_needs_concretization = function
  | OpenT _ | DefT (_, UnionT _) | DefT (_, MaybeT _) | DefT (_, OptionalT _) | AnnotT _ -> true
  | _ -> false

(* for now, we only care about concretizating parts of functions and calls *)
and parts_to_replace = function
  | UseT (_, DefT (_, FunT (_, _, ft))) ->
    let ts = List.fold_left (fun acc (_, t) ->
      if patt_that_needs_concretization t
      then t::acc
      else acc
    ) [] ft.params in
    (match ft.rest_param with
    | Some (_, _, t) when patt_that_needs_concretization t -> t::ts
    | _ -> ts)
  | CallT (_, _, callt) ->
    List.fold_left (fun acc -> function
      | Arg t | SpreadArg t when patt_that_needs_concretization t -> t::acc
      | _ -> acc
    ) [] callt.call_args_tlist
  | _ -> []

(* replace unresolved types (xs) with resolved (ys) *)
and replace_parts =
  let rec replace_params acc = function
    | ys, [] -> ys, List.rev acc
    | ys, ((name, x) as param)::params ->
      if patt_that_needs_concretization x
      then replace_params ((name, List.hd ys)::acc) (List.tl ys, params)
      else replace_params (param::acc) (ys, params)
  in
  let replace_rest_param = function
    | ys, None -> ys, None
    | ys, (Some (name, loc, x) as param) ->
      if patt_that_needs_concretization x
      then List.tl ys, Some (name, loc, List.hd ys)
      else ys, param
  in
  let replace_arg ys = function
    | Arg x when patt_that_needs_concretization x ->
      Arg (List.hd ys), List.tl ys
    | SpreadArg x when patt_that_needs_concretization x ->
      SpreadArg (List.hd ys), List.tl ys
    | arg -> arg, ys
  in
  let rec replace_args acc = function
    | ys, [] -> ys, List.rev acc
    | ys, arg::args ->
      let arg, ys = replace_arg ys arg in
      replace_args (arg::acc) (ys, args)
  in
  fun resolved -> function
  | UseT (op, DefT (r, FunT (t1, t2, ft))) ->
    let resolved, params = replace_params [] (resolved, ft.params) in
    let resolved, rest_param = replace_rest_param (resolved, ft.rest_param) in
    assert (resolved = []);
    UseT (op, DefT (r, FunT (t1, t2, { ft with params; rest_param })))
  | CallT (op, r, callt) ->
    let resolved, call_args_tlist = replace_args [] (resolved, callt.call_args_tlist) in
    assert (resolved = []);
    CallT (op, r, { callt with call_args_tlist })
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
  | Binding tvar -> (id, tvar)::bindings
  | OpenUnresolved (log_unresolved, reason, id) ->
    begin match log_unresolved with
    | Some speculation_id ->
      Speculation.add_unresolved_to_speculation cx speculation_id id
    | None ->
      resolve_id cx trace ~use_op:unknown_use id (AnyT.make reason)
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
    let bindings = bindings_of_jobs cx trace imap in
    (* NOTE: bindings_of_jobs might change the state of id because it resolves it, so check
       again. TODO: there must be a better way *)
    if is_unexplored_source cx id then
      resolve_bindings cx trace reason id bindings

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
  InternalT (ChoiceKitT (reason, k))

and choice_kit_use reason k =
  ChoiceKitUseT (reason, k)

and intersection_preprocess_kit reason k =
  IntersectionPreprocessKitT (reason, k)

(** utils for emitting toolkit constraints **)

and trigger cx trace reason done_tvar =
  rec_flow cx trace (choice_kit reason Trigger, UseT (unknown_use, done_tvar))

and try_flow_continuation cx trace reason speculation_id spec =
  tvar_with_constraint cx ~trace
    (choice_kit_use reason (TryFlow (speculation_id, spec)))

and resolve_binding cx trace reason (id, tvar) =
  rec_flow cx trace (
    OpenT tvar,
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
  let typeapp_stack = TypeAppExpansion.get () in
  let cache = !Cache.FlowConstraint.cache in
  Speculation.set_speculative branch;
  let restore () =
    Speculation.restore_speculative ();
    Cache.FlowConstraint.cache := cache;
    TypeAppExpansion.set typeapp_stack
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
and speculative_matches cx trace r speculation_id spec =
  (* explore optimization opportunities *)
  if optimize_spec_try_shortcut cx trace r spec then ()
  else long_path_speculative_matches cx trace r speculation_id spec

and long_path_speculative_matches cx trace r speculation_id spec = Speculation.Case.(
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
      let case = { case_id; unresolved = ISet.empty; actions = []} in
      (* speculatively match the pair of types in this trial *)
      let error = speculative_match cx trace
        { Speculation.ignore; speculation_id; case } l u in
      match error with
      | None ->
        (* no error, looking great so far... *)
        begin match match_state with
        | Speculation.NoMatch _ ->
          (* everything had failed up to this point. so no ambiguity yet... *)
          if ISet.is_empty case.unresolved
          (* ...and no unresolved tvars encountered during the speculative
             match! This is great news. It means that this alternative will
             definitely succeed. Fire any deferred actions and short-cut. *)
          then fire_actions cx trace spec case.actions
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
    fire_actions cx trace spec case.actions
  | Speculation.NoMatch msgs ->
    (* everything failed; make a really detailed error message listing out the
       error found for each alternative *)
    let ts = choices_of_spec spec in
    assert (List.length ts = List.length msgs);
    let branches = List.mapi (fun i msg ->
      let reason = reason_of_t (List.nth ts i) in
      (reason, msg)
    ) msgs in
    (* Add the error. *)
    begin match spec with
      | UnionCases (use_op, l, _rep, us) ->
        let reason = reason_of_t l in
        let reason_op = mk_union_reason r us in
        add_output cx ~trace
          (Flow_error.EUnionSpeculationFailed { use_op; reason; reason_op; branches })

      | IntersectionCases (ls, upper) ->
        let err =
          let reason_lower = mk_intersection_reason r ls in
          match upper with
          | UseT (use_op, t) ->
            Flow_error.EIncompatibleDefs {
              use_op;
              reason_lower;
              reason_upper = reason_of_t t;
              branches;
            }
          | _ ->
            Flow_error.EIncompatible {
              use_op = use_op_of_use_t upper;
              lower = (reason_lower, Some Flow_error.Incompatible_intersection);
              upper = (reason_of_use_t upper, flow_error_kind_of_upper upper);
              branches;
            }
        in
        add_output cx ~trace err
    end

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
and blame_unresolved cx trace prev_i i cases case_r r tvars =
  let rs = tvars |> List.map (fun (_, r) -> r) |> List.sort compare in
  let prev_case = reason_of_t (List.nth cases prev_i) in
  let case = reason_of_t (List.nth cases i) in
  add_output cx ~trace (FlowError.ESpeculationAmbiguous (
    (case_r, r),
    (prev_i, prev_case),
    (i, case),
    rs
  ))

and trials_of_spec = function
  | UnionCases (use_op, l, _rep, us) ->
    (* NB: Even though we know the use_op for the original constraint, don't
       embed it in the nested constraints to avoid unnecessary verbosity. We
       will unwrap the original use_op once in EUnionSpeculationFailed. *)
    List.mapi (fun i u -> (i, reason_of_t l, l, UseT (Op (Speculation use_op), u))) us
  | IntersectionCases (ls, u) ->
    List.mapi (fun i l -> (i, reason_of_use_t u, l,
      mod_use_op_of_use_t (fun use_op -> Op (Speculation use_op)) u)) ls

and choices_of_spec = function
  | UnionCases (_, _, _, ts)
  | IntersectionCases (ts, _)
    -> ts

and ignore_of_spec = function
  | IntersectionCases (_, CallT (_, _, {
      call_tout = OpenT (_, id); _
    })) -> Some id
  | IntersectionCases (_, GetPropT (_, _, _, OpenT (_, id))) -> Some id
  | _ -> None

(* spec optimization *)
(* Currently, the only optimizations we do are for enums and for disjoint unions.

   When a literal type is checked against a union of literal types, we hope the union is an enum and
   try to optimize the representation of the union as such. We also try to use our optimization to
   do a quick membership check, potentially avoiding the speculative matching process altogether.

   When an object type is checked against an union of object types, we hope the union is a disjoint
   union and try to guess and record sentinel properties across object types in the union. Later,
   during speculative matching, by checking sentinel properties first we force immediate match
   failures in the vast majority of cases without having to do any useless additional work.
*)
and optimize_spec_try_shortcut cx trace reason_op = function
  | UnionCases (use_op, l, rep, _ts) ->
    if not (UnionRep.is_optimized_finally rep)
    then UnionRep.optimize rep
      ~flatten:(Type_mapper.union_flatten cx)
      ~find_resolved:(Context.find_resolved cx)
      ~find_props:(Context.find_props cx);
    begin match l with
    | DefT (_,
        (StrT (Literal _) | NumT (Literal _) | BoolT (Some _) |
         SingletonStrT _ | SingletonNumT _ | SingletonBoolT _ |
         VoidT | NullT)) ->
      shortcut_enum cx trace reason_op use_op l rep
    | DefT (_, ObjT _) | ExactT (_, DefT (_, ObjT _)) ->
      shortcut_disjoint_union cx trace reason_op use_op l rep
    | _ -> false
    end
  | IntersectionCases _ -> false

and shortcut_enum cx trace reason_op use_op l rep =
  quick_mem_result cx trace reason_op use_op l rep @@
  UnionRep.quick_mem_enum l rep

and shortcut_disjoint_union cx trace reason_op use_op l rep =
  quick_mem_result cx trace reason_op use_op l rep @@
  UnionRep.quick_mem_disjoint_union l rep
    ~find_resolved:(Context.find_resolved cx)
    ~find_props:(Context.find_props cx)

and quick_mem_result cx trace reason_op use_op l rep = function
  | UnionRep.Yes -> (* membership check succeeded *)
    true (* Our work here is done, so no need to continue. *)
  | UnionRep.No -> (* membership check failed *)
    let r = UnionRep.specialized_reason reason_op rep in
    rec_flow cx trace (l, UseT (use_op, DefT (r, EmptyT)));
    true (* Our work here is done, so no need to continue. *)
  | UnionRep.Conditional t -> (* conditional match *)
    rec_flow cx trace (l, UseT (use_op, t));
    true (* Our work here is done, so no need to continue. *)
  | UnionRep.Unknown -> (* membership check was inconclusive *)
    false (* Continue to speculative matching. *)

(* When we fire_actions we also need to reconstruct the use_op for each action
 * since before beginning speculation we replaced each use_op with
 * an UnknownUse. *)
and fire_actions cx trace spec = List.iter (function
  | _, Speculation.Action.Flow (l, u) -> (match spec with
    | IntersectionCases (_, u') ->
      let use_op = use_op_of_use_t u' in
      (match use_op with
      | None -> rec_flow cx trace (l, u)
      | Some use_op ->
        rec_flow cx trace (l,
          mod_use_op_of_use_t (replace_speculation_root_use_op use_op) u))
    | UnionCases (use_op, _, _, _) ->
      rec_flow cx trace (l,
        mod_use_op_of_use_t (replace_speculation_root_use_op use_op) u)
    )
  | _, Speculation.Action.Unify (use_op, t1, t2) -> (match spec with
    | IntersectionCases (_, u') ->
      let use_op' = use_op_of_use_t u' in
      (match use_op' with
      | None -> rec_unify cx trace t1 t2 ~use_op
      | Some use_op' ->
        rec_unify cx trace t1 t2
          ~use_op:(replace_speculation_root_use_op use_op' use_op)
      )
    | UnionCases (use_op', _, _, _) ->
      rec_unify cx trace t1 t2
        ~use_op:(replace_speculation_root_use_op use_op' use_op)
    )
  | _, Speculation.Action.Error msg ->
    add_output cx ~trace msg
)

and mk_union_reason r us =
  List.fold_left (fun reason t ->
    let rdesc = string_of_desc (desc_of_reason ~unwrap:false reason) in
    let tdesc = string_of_desc (desc_of_reason ~unwrap:false (reason_of_t t)) in
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

and get_prop cx trace ~use_op reason_prop reason_op strict l super x map tout =
  match SMap.get x map with
  | Some p ->
    let action = RWProp (use_op, l, tout, Read) in
    let propref = Named (reason_prop, x) in
    (* TODO: use lreason instead of reason_prop below? *)
    perform_lookup_action cx trace propref p reason_prop reason_op action
  | None ->
    lookup_prop cx trace super reason_prop reason_op strict x
      (RWProp (use_op, l, tout, Read))

and set_prop cx ?(wr_ctx=Normal) trace ~use_op reason_prop reason_op strict l super x pmap tin prop_t =
  match SMap.get x pmap with
  | Some p ->
    let action = RWProp (use_op, l, tin, Write (wr_ctx, prop_t)) in
    let propref = Named (reason_prop, x) in
    (* TODO: use lreason instead of reason_prop below? *)
    perform_lookup_action cx trace propref p reason_prop reason_op action
  | None ->
    lookup_prop cx trace super reason_prop reason_op strict x
      (RWProp (use_op, l, tin, Write (wr_ctx, prop_t)))

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
    Some (Field (None, value, dict_polarity))
  | Computed k, None, Some { key; value; dict_polarity; _ } ->
    rec_flow_t cx trace (k, key);
    Some (Field (None, value, dict_polarity))
  | _ -> None

and read_obj_prop cx trace ~use_op o propref reason_obj reason_op tout =
  let l = DefT (reason_obj, ObjT o) in
  (match get_obj_prop cx trace o propref reason_op with
  | Some p ->
    let action = RWProp (use_op, l, tout, Read) in
    perform_lookup_action cx trace propref p reason_obj reason_op action
  | None ->
    match propref with
    | Named _ ->
      let strict =
        if Obj_type.sealed_in_op reason_op o.flags.sealed
        then Strict reason_obj
        else ShadowRead (None, Nel.one o.props_tmap)
      in
      rec_flow cx trace (o.proto_t,
        LookupT (reason_op, strict, [], propref, RWProp (use_op, l, tout, Read)))
    | Computed elem_t ->
      match elem_t with
      | OpenT _ ->
        let loc = loc_of_t elem_t in
        add_output cx ~trace FlowError.(EInternal (loc, PropRefComputedOpen))
      | DefT (_, StrT Literal _) ->
        let loc = loc_of_t elem_t in
        add_output cx ~trace FlowError.(EInternal (loc, PropRefComputedLiteral))
      | DefT (_, AnyT) | DefT (_, StrT _) | DefT (_, NumT _) ->
        (* any, string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
        rec_flow_t cx trace (AnyT.why reason_op, tout)
      | _ ->
        let reason_prop = reason_of_t elem_t in
        add_output cx ~trace (FlowError.EObjectComputedPropertyAccess
          (reason_op, reason_prop)))

and write_obj_prop cx trace ~use_op o propref reason_obj reason_op tin prop_t =
  let l = DefT (reason_obj, ObjT o) in
  match get_obj_prop cx trace o propref reason_op with
  | Some p ->
    let action = RWProp (use_op, l, tin, Write (Normal, prop_t)) in
    perform_lookup_action cx trace propref p reason_obj reason_op action
  | None ->
    match propref with
    | Named (reason_prop, prop) ->
      let sealed = Obj_type.sealed_in_op reason_op o.flags.sealed in
      if sealed && o.flags.exact
      then
        add_output cx ~trace (FlowError.EPropNotFound
          (Some prop, (reason_prop, reason_obj), use_op))
      else
        let strict =
          if sealed
          then Strict reason_obj
          else ShadowWrite (Nel.one o.props_tmap)
        in
        rec_flow cx trace (o.proto_t, LookupT (reason_op, strict, [], propref,
            RWProp (use_op, l, tin, Write (Normal, prop_t))))
    | Computed elem_t ->
      match elem_t with
      | OpenT _ ->
        let loc = loc_of_t elem_t in
        add_output cx ~trace FlowError.(EInternal (loc, PropRefComputedOpen))
      | DefT (_, StrT Literal _) ->
        let loc = loc_of_t elem_t in
        add_output cx ~trace FlowError.(EInternal (loc, PropRefComputedLiteral))
      | DefT (_, AnyT) | DefT (_, StrT _) | DefT (_, NumT _) ->
        (* any, string, and number keys are allowed, but there's nothing else to
           flow without knowing their literal values. *)
        rec_flow_t cx trace (tin, AnyT.why reason_op)
      | _ ->
        let reason_prop = reason_of_t elem_t in
        add_output cx ~trace (FlowError.EObjectComputedPropertyAssign
          (reason_op, reason_prop));

and find_or_intro_shadow_prop cx trace reason_op x prop_loc =
  let intro_shadow_prop id =
    let reason_prop = replace_reason_const (RShadowProperty x) reason_op in
    let t = Tvar.mk cx reason_prop in
    let p = Field (Some prop_loc, t, Neutral) in
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

(* filter out undefined from a type *)
and filter_optional cx ?trace reason opt_t =
  Tvar.mk_where cx reason (fun t ->
    flow_opt_t cx ?trace (opt_t, DefT (reason, OptionalT t))
  )

(* filter out undefined and null from a type *)
and filter_maybe cx ?trace reason maybe_t =
  Tvar.mk_where cx reason (fun t ->
    flow_opt_t cx ?trace (maybe_t, DefT (reason, MaybeT t))
  )

and update_sketchy_null cx opt_loc t =
  let open ExistsCheck in
  match t with
  (* Ignore AnyTs for sketchy null checks; otherwise they'd always trigger the lint. *)
  | DefT (_, AnyT) -> ()
  | _ ->
    match opt_loc with
    | None -> ()
    | Some loc ->
      let t_loc =
        let reason = reason_of_t t in
        match annot_loc_of_reason reason with
        | Some loc -> Some loc
        | None -> Some (def_loc_of_reason reason)
      in
      let exists_checks = Context.exists_checks cx in
      let exists_check = LocMap.get loc exists_checks |> Option.value ~default:ExistsCheck.empty in
      let exists_check = match Type_filter.maybe t with
        | DefT (_, EmptyT) -> exists_check
        | _ -> {exists_check with null_loc = t_loc}
      in
      let exists_check =
        match t |> Type_filter.not_exists |> Type_filter.not_maybe with
        | DefT (_, BoolT _) -> {exists_check with bool_loc = t_loc}
        | DefT (_, StrT _) -> {exists_check with string_loc = t_loc}
        | DefT (_, NumT _) -> {exists_check with number_loc = t_loc}
        | DefT (_, MixedT _) -> {exists_check with mixed_loc = t_loc}
        | _ -> exists_check
      in
      let exists_checks = if exists_check = ExistsCheck.empty
        then exists_checks
        else LocMap.add loc exists_check exists_checks
      in
      Context.set_exists_checks cx exists_checks

(**********)
(* guards *)
(**********)

and guard cx trace source pred result sink = match pred with

| ExistsP loc ->
  update_sketchy_null cx loc source;
  begin match Type_filter.exists source with
  | DefT (_, EmptyT) -> ()
  | _ -> rec_flow_t cx trace (result, sink)
  end

| NotP (ExistsP loc) ->
  update_sketchy_null cx loc source;
  begin match Type_filter.not_exists source with
  | DefT (_, EmptyT) -> ()
  | _ -> rec_flow_t cx trace (result, sink)
  end

| _ ->
  let loc = loc_of_reason (reason_of_t sink) in
  let pred_str = string_of_predicate pred in
  add_output cx ~trace
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
    let tvar = Tvar.mk cx reason in
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
    rec_flow_t cx trace (Type_filter.boolean l, t)

  | NotP BoolP ->
    rec_flow_t cx trace (Type_filter.not_boolean l, t)

  (***********************)
  (* typeof _ ~ "string" *)
  (***********************)

  | StrP ->
    rec_flow_t cx trace (Type_filter.string l, t)

  | NotP StrP ->
    rec_flow_t cx trace (Type_filter.not_string l, t)

  (*********************)
  (* _ ~ "some string" *)
  (*********************)

  | SingletonStrP (expected_loc, sense, lit) ->
    let filtered_str = Type_filter.string_literal expected_loc sense lit l in
    rec_flow_t cx trace (filtered_str, t)

  | NotP SingletonStrP (_, _, lit) ->
    let filtered_str = Type_filter.not_string_literal lit l in
    rec_flow_t cx trace (filtered_str, t)

  (*********************)
  (* _ ~ some number n *)
  (*********************)

  | SingletonNumP (expected_loc, sense, lit) ->
    let filtered_num = Type_filter.number_literal expected_loc sense lit l in
    rec_flow_t cx trace (filtered_num, t)

  | NotP SingletonNumP (_, _, lit) ->
    let filtered_num = Type_filter.not_number_literal lit l in
    rec_flow_t cx trace (filtered_num, t)

  (***********************)
  (* typeof _ ~ "number" *)
  (***********************)

  | NumP ->
    rec_flow_t cx trace (Type_filter.number l, t)

  | NotP NumP ->
    rec_flow_t cx trace (Type_filter.not_number l, t)

  (***********************)
  (* typeof _ ~ "function" *)
  (***********************)

  | FunP ->
    rec_flow_t cx trace (Type_filter.function_ l, t)

  | NotP FunP ->
    rec_flow_t cx trace (Type_filter.not_function l, t)

  (***********************)
  (* typeof _ ~ "object" *)
  (***********************)

  | ObjP ->
    rec_flow_t cx trace (Type_filter.object_ cx l, t)

  | NotP ObjP ->
    rec_flow_t cx trace (Type_filter.not_object l, t)

  (*******************)
  (* Array.isArray _ *)
  (*******************)

  | ArrP ->
    rec_flow_t cx trace (Type_filter.array l, t)

  | NotP ArrP ->
    rec_flow_t cx trace (Type_filter.not_array l, t)

  (***********************)
  (* typeof _ ~ "undefined" *)
  (***********************)

  | VoidP ->
    let filtered = Type_filter.undefined l in
    rec_flow_t cx trace (filtered, t)

  | NotP VoidP ->
    let filtered = Type_filter.not_undefined l in
    rec_flow_t cx trace (filtered, t)

  (********)
  (* null *)
  (********)

  | NullP ->
    let filtered = Type_filter.null l in
    rec_flow_t cx trace (filtered, t)

  | NotP NullP ->
    let filtered = Type_filter.not_null l in
    rec_flow_t cx trace (filtered, t)

  (*********)
  (* maybe *)
  (*********)

  | MaybeP ->
    let filtered = Type_filter.maybe l in
    rec_flow_t cx trace (filtered, t)

  | NotP MaybeP ->
    let filtered = Type_filter.not_maybe l in
    rec_flow_t cx trace (filtered, t)

  (********)
  (* true *)
  (********)

  | SingletonBoolP true ->
    let filtered = Type_filter.true_ l in
    rec_flow_t cx trace (filtered, t)

  | NotP (SingletonBoolP true) ->
    let filtered = Type_filter.not_true l in
    rec_flow_t cx trace (filtered, t)

  (*********)
  (* false *)
  (*********)

  | SingletonBoolP false ->
    let filtered = Type_filter.false_ l in
    rec_flow_t cx trace (filtered, t)

  | NotP (SingletonBoolP false) ->
    let filtered = Type_filter.not_false l in
    rec_flow_t cx trace (filtered, t)

  (************************)
  (* truthyness *)
  (************************)

  | ExistsP loc ->
    update_sketchy_null cx loc l;
    let filtered = Type_filter.exists l in
    rec_flow_t cx trace (filtered, t)

  | NotP (ExistsP loc) ->
    update_sketchy_null cx loc l;
    let filtered = Type_filter.not_exists l in
    rec_flow_t cx trace (filtered, t)

  | PropExistsP (reason, key, loc) ->
    update_sketchy_null cx loc l;
    prop_exists_test cx trace reason key true l t

  | NotP (PropExistsP (reason, key, loc)) ->
    update_sketchy_null cx loc l;
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
  | DefT (lreason, ObjT { flags; props_tmap; _}) as obj ->
    (match Context.get_prop cx props_tmap key with
    | Some p ->
      (match Property.read_t p with
      | Some t ->
        (* prop is present on object type *)
        let pred = if sense then ExistsP None else NotP (ExistsP None) in
        rec_flow cx trace (t, GuardT (pred, orig_obj, result))
      | None ->
        (* prop cannot be read *)
        add_output cx ~trace (FlowError.EPropAccess (
          (lreason, reason), Some key, Property.polarity p, Read, unknown_use
        ))
      )
    | None when flags.exact && Obj_type.sealed_in_op (reason_of_t result) flags.sealed ->
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

  | DefT (_, IntersectionT rep) ->
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
  | true,
    (DefT (reason, ArrT arrtype) as arr),
    DefT (r, ClassT (DefT (_, (InstanceT _)) as a)) ->

    let elemt = elemt_of_arrtype reason arrtype in

    let right = extends_type r arr a in
    let arrt = get_builtin_typeapp cx ~trace reason "Array" [elemt] in
    rec_flow cx trace (arrt, PredicateT(LeftP(InstanceofTest, right), result))

  | false,
    (DefT (reason, ArrT arrtype) as arr),
    DefT (r, ClassT (DefT (_, (InstanceT _)) as a)) ->

    let elemt = elemt_of_arrtype reason arrtype in

    let right = extends_type r arr a in
    let arrt = get_builtin_typeapp cx ~trace reason "Array" [elemt] in
    let pred = NotP(LeftP(InstanceofTest, right)) in
    rec_flow cx trace (arrt, PredicateT (pred, result))

  (** An object is considered `instanceof` a function F when it is constructed
      by F. Note that this is incomplete with respect to the runtime semantics,
      where instanceof is transitive: if F.prototype `instanceof` G, then the
      object is `instanceof` G. There is nothing fundamentally difficult in
      modeling the complete semantics, but we haven't found a need to do it. **)
  | true,
    (DefT (_, ObjT {proto_t = proto2; _}) as obj),
    DefT (_, FunT (_, proto1, _))
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
  | true,
    (DefT (_, InstanceT _) as c),
    DefT (r, ClassT (DefT (_, (InstanceT _)) as a)) ->
    predicate cx trace result
      (extends_type r c a)
      (RightP (InstanceofTest, c))

  (** If C is a subclass of A, then don't refine the type of x. Otherwise,
      refine the type of x to A. (In general, the type of x should be refined to
      C & A, but that's hard to compute.) **)
  | true,
    DefT (reason, InstanceT (_, super_c, _, instance_c)),
    (InternalT (ExtendsT (_, c, DefT (_, InstanceT (_, _, _, instance_a)))) as right)
    -> (* TODO: intersection *)

    if instance_a.class_id = instance_c.class_id
    then rec_flow_t cx trace (c, result)
    else
      (** Recursively check whether super(C) extends A, with enough context. **)
      let pred = LeftP(InstanceofTest, right) in
      let u = PredicateT(pred, result) in
      rec_flow cx trace (super_c, ReposLowerT (reason, false, u))

  | true,
    ObjProtoT _,
    InternalT (ExtendsT (r, _, a))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow_t cx trace (reposition cx ~trace (loc_of_reason r) a, result)

  (** Prune the type when any other `instanceof` check succeeds (since this is
      impossible). *)
  | true, _, _ ->
    ()

  | false,
    DefT (_, ObjT {proto_t = proto2; _}),
    DefT (_, FunT (_, proto1, _))
      when proto1 = proto2 ->
    ()

  (** Like above, now suppose that we have an instance x of class C, and we
      check whether x is _not_ `instanceof` class A. To decide what the
      appropriate refinement for x should be, we need to decide whether C
      extends A, choosing either nothing or C based on the result. **)
  | false,
    (DefT (_, InstanceT _) as c),
    DefT (r, ClassT (DefT (_, (InstanceT _)) as a)) ->
    predicate cx trace result
      (extends_type r c a)
      (NotP(RightP(InstanceofTest, c)))

  (** If C is a subclass of A, then do nothing, since this check cannot
      succeed. Otherwise, don't refine the type of x. **)
  | false,
    DefT (reason, InstanceT (_, super_c, _, instance_c)),
    (InternalT (ExtendsT(_, _, DefT (_, InstanceT (_, _, _, instance_a)))) as right)
    ->

    if instance_a.class_id = instance_c.class_id
    then ()
    else
      let u = PredicateT(NotP(LeftP(InstanceofTest, right)), result) in
      rec_flow cx trace (super_c, ReposLowerT (reason, false, u))

  | false,
    ObjProtoT _,
    InternalT (ExtendsT(r, c, _))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow_t cx trace (reposition cx ~trace (loc_of_reason r) c, result)

  (** Don't refine the type when any other `instanceof` check fails. **)
  | false, left, _ ->
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
        let desc = RMatchingProp (key, match sentinel with
          | Enum.(One Str s) -> RStringLit s
          | Enum.(One Num (_, n)) -> RNumberLit n
          | Enum.(One Bool b) -> RBooleanLit b
          | Enum.(One Null) -> RNull
          | Enum.(One Void) -> RVoid
          | Enum.(Many _enums) -> REnum
        ) in
        let reason = replace_reason_const desc (reason_of_t result) in
        let test = SentinelPropTestT (reason, orig_obj, key, sense, sentinel, result) in
        rec_flow cx trace (t, test)
      | None ->
        let reason_obj = reason_of_t obj in
        let reason = reason_of_t result in
        add_output cx ~trace (FlowError.EPropAccess (
          (reason_obj, reason), Some key, Property.polarity p, Read, unknown_use
        ))
      )
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
  let sentinel_of_literal = function
    | DefT (_, StrT (Literal (_, value))) -> Some Enum.(One (Str value))
    | DefT (_, NumT (Literal (_, value))) -> Some Enum.(One (Num value))
    | DefT (_, BoolT (Some value)) -> Some Enum.(One (Bool value))
    | DefT (_, VoidT) -> Some Enum.(One Void)
    | DefT (_, NullT) -> Some Enum.(One Null)
    | DefT (_, UnionT rep) ->
      begin match UnionRep.check_enum rep with
        | Some enums -> Some Enum.(Many enums)
        | None -> None
      end
    | _ -> None
  in
  fun (sense, obj, t) -> match sentinel_of_literal t with
  | Some s ->
      begin match obj with
      (* obj.key ===/!== literal value *)
      | DefT (_, ObjT { props_tmap; _}) ->
        flow_sentinel sense props_tmap obj s

      (* instance.key ===/!== literal value *)
      | DefT (_, InstanceT (_, _, _, { fields_tmap; _})) ->
        (* TODO: add test for sentinel test on implements *)
        flow_sentinel sense fields_tmap obj s

      | DefT (_, IntersectionT rep) ->
        (* For an intersection of object types, try the test for each object
           type in turn, while recording the original intersection so that we
           end up with the right refinement. See the comment on the
           implementation of IntersectionPreprocessKit for more details. *)
        let reason = reason_of_t result in
        InterRep.members rep |> List.iter (fun obj ->
          rec_flow cx trace (
            obj,
            intersection_preprocess_kit reason
              (SentinelPropTest(sense, key, t, orig_obj, result))
          )
        )
      | _ ->
        (* not enough info to refine *)
        rec_flow_t cx trace (orig_obj, result)
      end
  | None ->
    (* not enough info to refine *)
    rec_flow_t cx trace (orig_obj, result)

(*******************************************************************)
(* /predicate *)
(*******************************************************************)

and flow_use_op op1 u =
  let ignore_root = function
  | UnknownUse -> true
  (* If we are speculating then a Speculation use_op should be considered
   * "opaque". If we are not speculating then Speculation use_ops that escaped
   * (through benign tvars) should be ignored.
   *
   * Ideally we could replace the Speculation use_ops on benign tvars with their
   * underlying use_op after speculation ends. *)
  | Speculation _ -> not (Speculation.speculating ())
  | _ -> false
  in
  mod_use_op_of_use_t (fun op2 ->
    let alt = fold_use_op
      (* If the root of the previous use_op is UnknownUse and our alternate
       * use_op does not have an UnknownUse root then we use our
       * alternate use_op. *)
      ignore_root
      (fun alt -> function
        (* If the use was added to an implicit type param then we want to use
         * our alternate if the implicit type param use_op chain is inside
         * the implicit type param instantiation. This means we always prefer
         * pointing to the use_op chain outside of the type parameter
         * instantiation. This ensures suppression comments are added to the
         * correct location.
         *
         * Instead of using locs to pick our chain, there may be some type
         * theory rule we can employ. However, such a rule would likely depend
         * on tracking the inputs/outputs when type parameters are instantiated.
         * For now, using locs is cleaner then adding unreliable bits to type
         * parameter uses in inputs/outputs. *)
        | ImplicitTypeParam loc_op when not alt ->
          let loc2 = loc_of_root_use_op (root_of_use_op op2) in
          Loc.contains loc_op loc2
        | _ -> alt)
      op2
    in
    if alt && not (ignore_root (root_of_use_op op1)) then op1 else op2
  ) u

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
  ls |> TypeMap.iter (fun l (trace_l, use_op) ->
    let u = flow_use_op use_op u in
    join_flow cx [trace_l; trace] (l, u)
  )

(* for each u in us: l => u *)
and flows_from_t cx trace ~use_op l us =
  us |> UseTypeMap.iter (fun u trace_u ->
    let u = flow_use_op use_op u in
    join_flow cx [trace; trace_u] (l, u)
  )

(* for each l in ls, u in us: l => u *)
and flows_across cx trace ~use_op ls us =
  ls |> TypeMap.iter (fun l (trace_l, use_op') ->
    us |> UseTypeMap.iter (fun u trace_u ->
      let u = flow_use_op use_op' (flow_use_op use_op u) in
      join_flow cx [trace_l; trace; trace_u] (l, u)
    )
  )

(* bounds.upper += u *)
and add_upper u trace bounds =
  bounds.upper <- UseTypeMap.add u trace bounds.upper

(* bounds.lower += l *)
and add_lower l (trace, use_op) bounds =
  bounds.lower <- TypeMap.add l (trace, use_op) bounds.lower

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
    match Context.find_constraints cx id with
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
    add_upper t2 (Trace.concat_trace [trace_l; trace]) bounds
  )

(* for each id in id2 + bounds2.uppertvars:
   id.bounds.lower += t1
*)
(** When going through bounds2.uppertvars, filter out id2. **)
(** As an optimization, skip id2 when it will become either a resolved root or a
    goto node (so that updating its bounds is unnecessary). **)
and edges_from_t cx trace ~use_op ?(opt=false) t1 (id2, bounds2) =
  if not opt then add_lower t1 (trace, use_op) bounds2;
  iter_with_filter cx bounds2.uppertvars id2 (fun (_, bounds) trace_u ->
    add_lower t1 (Trace.concat_trace [trace; trace_u], use_op) bounds
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
  ls |> TypeMap.iter (fun l (trace_l, use_op) ->
    edges_from_t cx (Trace.concat_trace [trace_l; trace]) ~use_op ~opt l (id, bounds)
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
and edges_and_flows_from_t cx trace ~use_op ?(opt=false) t1 (id2, bounds2) =
  if not (TypeMap.mem t1 bounds2.lower) then (
    edges_from_t cx trace ~use_op ~opt t1 (id2, bounds2);
    flows_from_t cx trace ~use_op t1 bounds2.upper
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

and unify_flip use_op = Frame (UnifyFlip, use_op)

(* Chain a root to another root. If both roots are unresolved, this amounts to
   copying over the bounds of one root to another, and adding all the
   connections necessary when two non-unifiers flow to each other. If one or
   both of the roots are resolved, they effectively act like the corresponding
   concrete types. *)
and goto cx trace ~use_op (id1, root1) (id2, root2) =
  match root1.constraints, root2.constraints with
  | Unresolved bounds1, Unresolved bounds2 ->
    let cond1 = not_linked (id1, bounds1) (id2, bounds2) in
    let cond2 = not_linked (id2, bounds2) (id1, bounds1) in
    if cond1 then
      flows_across cx trace ~use_op bounds1.lower bounds2.upper;
    if cond2 then
      flows_across cx trace ~use_op:(unify_flip use_op) bounds2.lower bounds1.upper;
    if cond1 then (
      add_upper_edges cx trace ~opt:true (id1, bounds1) (id2, bounds2);
      add_lower_edges cx trace (id1, bounds1) (id2, bounds2);
    );
    if cond2 then (
      add_upper_edges cx trace (id2, bounds2) (id1, bounds1);
      add_lower_edges cx trace ~opt:true (id2, bounds2) (id1, bounds1);
    );
    Context.add_tvar cx id1 (Goto id2);

  | Unresolved bounds1, Resolved t2 ->
    let t2_use = UseT (use_op, t2) in
    edges_and_flows_to_t cx trace ~opt:true (id1, bounds1) t2_use;
    edges_and_flows_from_t cx trace ~use_op:(unify_flip use_op) ~opt:true t2 (id1, bounds1);
    Context.add_tvar cx id1 (Goto id2);

  | Resolved t1, Unresolved bounds2 ->
    let t1_use = UseT (unify_flip use_op, t1) in
    edges_and_flows_to_t cx trace ~opt:true (id2, bounds2) t1_use;
    edges_and_flows_from_t cx trace ~use_op ~opt:true t1 (id2, bounds2);
    Context.add_tvar cx id2 (Goto id1);

  | Resolved t1, Resolved t2 ->
    (* replace node first, in case rec_unify recurses back to these tvars *)
    Context.add_tvar cx id1 (Goto id2);
    rec_unify cx trace ~use_op t1 t2;

(* Unify two type variables. This involves finding their roots, and making one
   point to the other. Ranks are used to keep chains short. *)
and merge_ids cx trace ~use_op id1 id2 =
  let (id1, root1), (id2, root2) = Context.find_root cx id1, Context.find_root cx id2 in
  if id1 = id2 then ()
  else if root1.rank < root2.rank
  then goto cx trace ~use_op (id1, root1) (id2, root2)
  else if root2.rank < root1.rank
  then goto cx trace ~use_op:(unify_flip use_op) (id2, root2) (id1, root1)
  else (
    Context.add_tvar cx id2 (Root { root2 with rank = root1.rank+1; });
    goto cx trace ~use_op (id1, root1) (id2, root2);
  )

(* Resolve a type variable to a type. This involves finding its root, and
   resolving to that type. *)
and resolve_id cx trace ~use_op id t =
  let id, root = Context.find_root cx id in
  match root.constraints with
  | Unresolved bounds ->
    Context.add_tvar cx id (Root { root with constraints = Resolved t });
    edges_and_flows_to_t cx trace ~opt:true (id, bounds) (UseT (use_op, t));
    edges_and_flows_from_t cx trace ~use_op ~opt:true t (id, bounds);

  | Resolved t_ ->
    rec_unify cx trace ~use_op t_ t

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
   to just AnyWithUpperBoundT, AnyWithLowerBoundT, and MergedT which are
   internal types.
*)
and ok_unify ~unify_any desc = function
  | DefT (_, AnyT) | AnyWithUpperBoundT _ | AnyWithLowerBoundT _ ->
    (match desc with RExistential -> true | _ -> unify_any)
  | MergedT _ -> false
  | _ -> true

and __unify cx ~use_op ~unify_any t1 t2 trace =
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

  (* If the type is the same type or we have already seen this type pair in our
   * cache then do not continue. *)
  if t1 = t2 then () else (

  (* limit recursion depth *)
  RecursionCheck.check trace;

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
  if not Speculation.(defer_action cx (Action.Unify (use_op, t1, t2))) then

  match t1, t2 with

  | OpenT (_, id1), OpenT (_, id2) ->
    merge_ids cx trace ~use_op id1 id2

  | OpenT (r, id), t when ok_unify ~unify_any (desc_of_reason r) t ->
    resolve_id cx trace ~use_op id t
  | t, OpenT (r, id) when ok_unify ~unify_any (desc_of_reason r) t ->
    resolve_id cx trace ~use_op:(unify_flip use_op) id t

  | DefT (_, PolyT (_, _, id1)), DefT (_, PolyT (_, _, id2))
    when id1 = id2 -> ()

  | DefT (r1, PolyT (params1, t1, id1)), DefT (r2, PolyT (params2, t2, id2)) ->
    let n1 = List.length params1 in
    let n2 = List.length params2 in
    if n2 > n1 then
      add_output cx ~trace (FlowError.ETooManyTypeArgs (r2, r1, n1))
    else if n2 < n1 then
      add_output cx ~trace (FlowError.ETooFewTypeArgs (r2, r1, n1))
    else
      (** for equal-arity polymorphic types, unify param upper bounds
          with each other, then instances parameterized by these *)
      let args1 = instantiate_poly_param_upper_bounds cx params1 in
      let args2 = instantiate_poly_param_upper_bounds cx params2 in
      List.iter2 (rec_unify cx trace ~use_op) args1 args2;
      let inst1 =
        let r = reason_of_t t1 in
        mk_typeapp_of_poly cx trace
          ~use_op ~reason_op:r ~reason_tapp:r id1 params1 t1 args1 in
      let inst2 =
        let r = reason_of_t t2 in
        mk_typeapp_of_poly cx trace
          ~use_op ~reason_op:r ~reason_tapp:r id2 params2 t2 args2 in
      rec_unify cx trace ~use_op inst1 inst2

  | DefT (_, ArrT (ArrayAT(t1, ts1))),
    DefT (_, ArrT (ArrayAT(t2, ts2))) ->
    let ts1 = Option.value ~default:[] ts1 in
    let ts2 = Option.value ~default:[] ts2 in
    array_unify cx trace ~use_op (ts1, t1, ts2, t2)

  | DefT (r1, ArrT (TupleAT (_, ts1))),
    DefT (r2, ArrT (TupleAT (_, ts2))) ->
    let l1 = List.length ts1 in
    let l2 = List.length ts2 in
    if l1 <> l2 then
      add_output cx ~trace (FlowError.ETupleArityMismatch
        ((r1, r2), l1, l2, use_op));
    iter2opt (fun t1 t2 ->
      match t1, t2 with
      | Some t1, Some t2 -> rec_unify cx trace ~use_op t1 t2
      | _ -> ()
    ) (ts1, ts2)

  | DefT (lreason, ObjT { props_tmap = lflds; dict_t = ldict; _ }),
    DefT (ureason, ObjT { props_tmap = uflds; dict_t = udict; _ }) ->

    (* ensure the keys and values are compatible with each other. *)
    begin match ldict, udict with
    | Some {key = lk; value = lv; _}, Some {key = uk; value = uv; _} ->
        rec_unify cx trace lk uk ~use_op:(Frame (IndexerKeyCompatibility {
          lower = lreason;
          upper = ureason;
        }, use_op));
        rec_unify cx trace lv uv ~use_op:(Frame (PropertyCompatibility {
          prop = None;
          lower = lreason;
          upper = ureason;
          is_sentinel = false;
        }, use_op))
    | Some _, None ->
        let use_op = Frame (PropertyCompatibility {
          prop = None;
          lower = ureason;
          upper = lreason;
          is_sentinel = false;
        }, use_op) in
        let lreason = replace_reason_const RSomeProperty lreason in
        let err = FlowError.EPropNotFound (None, (lreason, ureason), use_op) in
        add_output cx ~trace err
    | None, Some _ ->
        let use_op = Frame (PropertyCompatibility {
          prop = None;
          lower = lreason;
          upper = ureason;
          is_sentinel = false;
        }, Frame (UnifyFlip, use_op)) in
        let ureason = replace_reason_const RSomeProperty ureason in
        let err = FlowError.EPropNotFound (None, (ureason, lreason), use_op) in
        add_output cx ~trace err
    | None, None -> ()
    end;

    let lpmap = Context.find_props cx lflds in
    let upmap = Context.find_props cx uflds in
    SMap.merge (fun x lp up ->
      if not (is_internal_name x || is_dictionary_exempt x)
      then (match lp, up with
      | Some p1, Some p2 ->
          unify_props cx trace ~use_op x lreason ureason p1 p2
      | Some p1, None ->
          unify_prop_with_dict cx trace ~use_op x p1 lreason ureason udict
      | None, Some p2 ->
          unify_prop_with_dict cx trace ~use_op x p2 ureason lreason ldict
      | None, None -> ());
      None
    ) lpmap upmap |> ignore

  | DefT (_, FunT (_, _, funtype1)), DefT (_, FunT (_, _, funtype2))
      when List.length funtype1.params =
           List.length funtype2.params ->
    rec_unify cx trace ~use_op funtype1.this_t funtype2.this_t;
    List.iter2 (fun (_, t1) (_, t2) ->
      rec_unify cx trace ~use_op t1 t2
    ) funtype1.params funtype2.params;
    rec_unify cx trace ~use_op funtype1.return_t funtype2.return_t

  | DefT (_, TypeAppT (_, c1, ts1)), DefT (_, TypeAppT (_, c2, ts2))
    when c1 = c2 && List.length ts1 = List.length ts2 ->
    List.iter2 (rec_unify cx trace ~use_op) ts1 ts2

  | _ ->
    naive_unify cx trace ~use_op t1 t2
  )

and unify_props cx trace ~use_op x r1 r2 p1 p2 =
  let use_op = Frame (PropertyCompatibility {
    prop = Some x;
    lower = r1;
    upper = r2;
    is_sentinel = false;
  }, use_op) in

  (* If both sides are neutral fields, we can just unify once *)
  match p1, p2 with
  | Field (_, t1, Neutral),
    Field (_, t2, Neutral) ->
    rec_unify cx trace ~use_op t1 t2;
  | _ ->
    (* Otherwise, unify read/write sides separately. *)
    (match Property.read_t p1, Property.read_t p2 with
    | Some t1, Some t2 ->
        rec_unify cx trace ~use_op t1 t2;
    | _ -> ());
    (match Property.write_t p1, Property.write_t p2 with
    | Some t1, Some t2 ->
        rec_unify cx trace ~use_op t1 t2;
    | _ -> ());
    (* Error if polarity is not compatible both ways. *)
    let polarity1 = Property.polarity p1 in
    let polarity2 = Property.polarity p2 in
    if not (
      Polarity.compat (polarity1, polarity2) &&
      Polarity.compat (polarity2, polarity1)
    ) then
      add_output cx ~trace (FlowError.EPropPolarityMismatch
        ((r1, r2), Some x, (polarity1, polarity2), use_op))

(* If some property `x` exists in one object but not another, ensure the
   property is compatible with a dictionary, or error if none. *)
and unify_prop_with_dict cx trace ~use_op x p prop_obj_reason dict_reason dict =
  (* prop_obj_reason: reason of the object containing the prop
     dict_reason: reason of the object potentially containing a dictionary
     prop_reason: reason of the prop itself *)
  let prop_reason = replace_reason_const (RProperty (Some x)) prop_obj_reason in
  match dict with
  | Some { key; value; dict_polarity; _ } ->
    rec_flow cx trace (string_key x prop_reason, UseT (
      Frame (IndexerKeyCompatibility {lower = dict_reason; upper = prop_obj_reason}, use_op),
      key
    ));
    let p2 = Field (None, value, dict_polarity) in
    unify_props cx trace ~use_op x prop_obj_reason dict_reason p p2
  | None ->
    let use_op = Frame (PropertyCompatibility {
      prop = Some x;
      lower = dict_reason;
      upper = prop_obj_reason;
      is_sentinel = false;
    }, use_op) in
    let err = FlowError.EPropNotFound (Some x, (prop_reason, dict_reason), use_op) in
    add_output cx ~trace err

(* TODO: Unification between concrete types is still implemented as
   bidirectional flows. This means that the destructuring work is duplicated,
   and we're missing some opportunities for nested unification. *)

and naive_unify cx trace ~use_op t1 t2 =
  rec_flow_t cx trace ~use_op (t1, t2);
  rec_flow_t cx trace ~use_op:(unify_flip use_op) (t2, t1)

(* mutable sites on parent values (i.e. object properties,
   array elements) must be typed invariantly when a value
   flows to the parent, unless the incoming value is fresh,
   in which case covariant typing is sound (since no alias
   will break if the subtyped child value is replaced by a
   non-subtyped value *)
and flow_to_mutable_child cx trace use_op fresh t1 t2 =
  if fresh
  then rec_flow cx trace (t1, UseT (use_op, t2))
  else rec_unify cx trace ~use_op t1 t2

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
and array_flow cx trace use_op lit1 r1 ?(index=0) = function
  (* empty array / array literal / tuple flowing to array / array literal /
     tuple (includes several cases, analyzed below) *)
  | [], e1, _, e2 ->
    (* if lower bound is an empty array / array literal *)
    if index = 0 then
      (* general element1 = general element2 *)
      flow_to_mutable_child cx trace use_op lit1 e1 e2
    (* otherwise, lower bound is an empty tuple (nothing to do) *)

  (* non-empty array literal / tuple ~> empty array / array literal / tuple *)
  | _, e1, [], e2 ->
    (* general element1 < general element2 *)
    rec_flow cx trace (e1, UseT (use_op, e2))

  (* non-empty array literal / tuple ~> non-empty array literal / tuple *)
  | t1 :: ts1, e1, t2 :: ts2, e2 ->
    (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace use_op lit1 t1 t2;
    array_flow cx trace use_op lit1 r1 ~index:(index+1) (ts1,e1, ts2,e2)

(* TODO: either ensure that array_unify is the same as array_flow both ways, or
   document why not. *)
(* array helper *)
and array_unify cx trace ~use_op = function
  | [], e1, [], e2 ->
    (* general element1 = general element2 *)
    rec_unify cx trace ~use_op e1 e2

  | ts1, _, [], e2
  | [], e2, ts1, _ ->
    (* specific element1 = general element2 *)
    List.iter (fun t1 -> rec_unify cx trace ~use_op t1 e2) ts1

  | t1 :: ts1, e1, t2 :: ts2, e2 ->
    (* specific element1 = specific element2 *)
    rec_unify cx trace ~use_op t1 t2;
    array_unify cx trace ~use_op (ts1, e1, ts2, e2)


(*******************************************************************)
(* subtyping a sequence of arguments with a sequence of parameters *)
(*******************************************************************)

(* Process spread arguments and then apply the arguments to the parameters *)
and multiflow_call cx trace ~use_op reason_op args ft =
  let resolve_to = ResolveSpreadsToMultiflowCallFull (mk_id (), ft) in
  resolve_call_list cx ~trace ~use_op reason_op args resolve_to

(* Process spread arguments and then apply the arguments to the parameters *)
and multiflow_subtype cx trace ~use_op reason_op args ft =
  let resolve_to = ResolveSpreadsToMultiflowSubtypeFull (mk_id (), ft) in
  resolve_call_list cx ~trace ~use_op reason_op args resolve_to

(* Like multiflow_partial, but if there is no spread argument, it flows VoidT to
 * all unused parameters *)
and multiflow_full
  cx ~trace ~use_op reason_op ~is_strict ~def_reason
  ~spread_arg ~rest_param (arglist, parlist) =

  let unused_parameters, _ = multiflow_partial
    cx ~trace ~use_op reason_op ~is_strict ~def_reason
    ~spread_arg ~rest_param (arglist, parlist) in

  let _ = List.fold_left (fun n (_, param) ->
    let use_op = Frame (FunMissingArg { n; op = reason_op; def = def_reason }, use_op) in
    rec_flow cx trace (VoidT.why reason_op, UseT (use_op, param));
    n + 1
  ) ((List.length parlist - List.length unused_parameters) + 1) unused_parameters in

  ()

(* This is a tricky function. The simple description is that it flows all the
 * arguments to all the parameters. This function is used by
 * Function.prototype.apply, so after the arguments are applied, it returns the
 * unused parameters.
 *
 * It is a little trickier in that there may be a single spread argument after
 * all the regular arguments. There may also be a rest parameter.
 *)
and multiflow_partial =
  let rec multiflow_non_spreads cx ~use_op n (arglist, parlist) =
    match (arglist, parlist) with
    (* Do not complain on too many arguments.
       This pattern is ubiqutous and causes a lot of noise when complained about.
       Note: optional/rest parameters do not provide a workaround in this case.
    *)
    | (_, [])
    (* No more arguments *)
    | ([], _) -> [], arglist, parlist

    | (tin::tins, (name, tout)::touts) ->
      (* flow `tin` (argument) to `tout` (param). normally, `tin` is passed
         through a `ReposLowerT` to make sure that the concrete type points at
         the arg's location. however, if `tin` is an implicit type argument
         (e.g. the `x` in `function foo<T>(x: T)`), then don't reposition it
         because implicit type args have no explicit location to point at.
         instead, let it flow through transparently, so that we point at the
         place that constrained the type arg. this is pretty hacky. *)
      let tout =
        let use_op = Frame (FunParam {
          n;
          name;
          lower=(reason_of_t tin);
          upper=(reason_of_t tout);
        }, use_op) in
        let u = UseT (use_op, tout) in
        match desc_of_t tin with
        | RTypeParam _ -> u
        | _ -> ReposLowerT (reason_of_t tin, false, u)
      in
      let used_pairs, unused_arglist, unused_parlist =
        multiflow_non_spreads cx ~use_op (n + 1) (tins, touts) in
      (tin, tout)::used_pairs, unused_arglist, unused_parlist
  in
  fun cx ~trace ~use_op ~is_strict ~def_reason ~spread_arg ~rest_param
    reason_op (arglist, parlist) ->

    (* Handle all the non-spread arguments and all the non-rest parameters *)
    let used_pairs, unused_arglist, unused_parlist =
      multiflow_non_spreads cx ~use_op 1 (arglist, parlist) in

    (* If there is a spread argument, it will consume all the unused parameters *)
    let used_pairs, unused_parlist = match spread_arg with
    | None -> used_pairs, unused_parlist
    | Some spread_arg_elemt ->
      (* The spread argument may be an empty array and to be 100% correct, we
       * should flow VoidT to every remaining parameter, however we don't. This
       * is consistent with how we treat arrays almost everywhere else *)
      used_pairs @ List.map
        (fun (_, param) ->
          let use_op = Frame (FunRestParam {
            lower=(reason_of_t spread_arg_elemt);
            upper=(reason_of_t param);
          }, use_op) in
          (spread_arg_elemt, UseT (use_op, param)))
        unused_parlist,
      []

    in

    (* If there is a rest parameter, it will consume all the unused arguments *)
    begin match rest_param with
    | None ->
      if is_strict && Context.enforce_strict_call_arity cx
      then begin match unused_arglist with
      | [] -> ()
      | first_unused_arg :: _ ->
        FlowError.EFunctionCallExtraArg (
          mk_reason RFunctionUnusedArgument (loc_of_t first_unused_arg),
          def_reason,
          List.length parlist,
          use_op
        )
        |> add_output cx ~trace
      end;
      (* Flow the args and params after we add the EFunctionCallExtraArg error.
       * This improves speculation error reporting. *)
      List.iter (rec_flow cx trace) used_pairs;

      unused_parlist, rest_param
    | Some (name, loc, rest_param) ->
      List.iter (rec_flow cx trace) used_pairs;

      let orig_rest_reason = repos_reason loc (reason_of_t rest_param) in

      (* We're going to build an array literal with all the unused arguments
       * (and the spread argument if it exists). Then we're going to flow that
       * to the rest parameter *)
      let rev_elems =
        List.rev_map (fun arg -> UnresolvedArg arg) unused_arglist in

      let unused_rest_param = match spread_arg with
      | None ->
        (* If the rest parameter is consuming N elements, then drop N elements
         * from the rest parameter *)
        let rest_reason = reason_of_t rest_param in
        Tvar.mk_derivable_where cx rest_reason (fun tout ->
          let i = List.length rev_elems in
          rec_flow cx trace (rest_param, ArrRestT (use_op, orig_rest_reason, i, tout))
        )
      | Some _ ->
        (* If there is a spread argument, then a tuple rest parameter will error
         * anyway. So let's assume that the rest param is an array with unknown
         * arity. Dropping elements from it isn't worth doing *)
        rest_param
      in

      let elems = match spread_arg with
      | None -> List.rev rev_elems
      | Some spread_arg_elemt ->
        let reason = reason_of_t spread_arg_elemt in
        let spread_array = DefT (reason, ArrT (ArrayAT (spread_arg_elemt, None))) in
        List.rev_append rev_elems [ UnresolvedSpreadArg (spread_array) ]
      in

      let arg_array_reason = replace_reason_const
        (RRestArray (desc_of_reason reason_op)) reason_op in

      let arg_array = Tvar.mk_where cx arg_array_reason (fun tout ->
        let reason_op = arg_array_reason in
        let element_reason = replace_reason_const Reason.inferred_union_elem_array_desc reason_op in
        let elem_t = Tvar.mk cx element_reason in
        let resolve_to = (ResolveSpreadsToArrayLiteral (mk_id (), elem_t, tout)) in
        resolve_spread_list cx ~use_op ~reason_op elems resolve_to
      ) in
      let () =
        let use_op = Frame (FunRestParam {
          lower = reason_of_t arg_array;
          upper = reason_of_t rest_param;
        }, use_op) in
        rec_flow cx trace (arg_array, UseT (use_op, rest_param))
      in

      unused_parlist, Some (name, loc, unused_rest_param)
    end

and resolve_call_list cx ~trace ~use_op reason_op args resolve_to =
  let unresolved = List.map
    (function
    | Arg t -> UnresolvedArg t
    | SpreadArg t -> UnresolvedSpreadArg t)
    args in
  resolve_spread_list_rec cx ~trace ~use_op ~reason_op ([], unresolved) resolve_to

and resolve_spread_list cx ~use_op ~reason_op list resolve_to =
  resolve_spread_list_rec cx ~use_op ~reason_op ([], list) resolve_to

(* This function goes through the unresolved elements to find the next rest
 * element to resolve *)
and resolve_spread_list_rec
  cx ?trace ~use_op ~reason_op (resolved, unresolved) resolve_to =
  match resolved, unresolved with
  | resolved, [] ->
      finish_resolve_spread_list
        cx ?trace ~use_op ~reason_op (List.rev resolved) resolve_to
  | resolved, UnresolvedArg(next)::unresolved ->
      resolve_spread_list_rec
        cx
        ?trace
        ~use_op
        ~reason_op
        (ResolvedArg(next)::resolved, unresolved)
        resolve_to
  | resolved, UnresolvedSpreadArg(next)::unresolved ->
      flow_opt cx ?trace (next, ResolveSpreadT (use_op, reason_op, {
        rrt_resolved = resolved;
        rrt_unresolved = unresolved;
        rrt_resolve_to = resolve_to;
      }))

(* Now that everything is resolved, we can construct whatever type we're trying
 * to resolve to. *)
and finish_resolve_spread_list =
  (* Turn tuple rest params into single params *)
  let flatten_spread_args list =
    list
    |> List.fold_left (fun acc param -> match param with
      | ResolvedSpreadArg (_, arrtype) ->
          begin match arrtype with
          | ArrayAT (_, Some tuple_types)
          | TupleAT (_, tuple_types) ->
              List.fold_left
                (fun acc elem -> ResolvedArg(elem)::acc)
                acc
                tuple_types
          | ArrayAT (_, None)
          | ROArrayAT (_)
          | EmptyAT
            -> param::acc
          end
      | ResolvedAnySpreadArg _
      | ResolvedArg _ -> param::acc
      ) []
    |> List.rev

  in

  let spread_resolved_to_any = List.exists (function
    | ResolvedAnySpreadArg _ -> true
    | ResolvedArg _ | ResolvedSpreadArg _ -> false)

  in

  let finish_array cx ?trace ~reason_op ~resolve_to resolved elemt tout =
    (* Did `any` flow to one of the rest parameters? If so, we need to resolve
     * to a type that is both a subtype and supertype of the desired type. *)
    let result = if spread_resolved_to_any resolved
    then match resolve_to with
      (* Array<any> is a good enough any type for arrays *)
      | `Array -> DefT (reason_op, ArrT (ArrayAT (AnyT.why reason_op, None)))
      (* Array literals can flow to a tuple. Arrays can't. So if the presence
       * of an `any` forces us to degrade an array literal to Array<any> then
       * we might get a new error. Since introducing `any`'s shouldn't cause
       * errors, this is bad. Instead, let's degrade array literals to `any` *)
      | `Literal
      (* There is no AnyTupleT type, so let's degrade to `any`. *)
      | `Tuple -> AnyT.why reason_op
    else begin
      (* Spreads that resolve to tuples are flattened *)
      let elems = flatten_spread_args resolved in

      let tuple_types = match resolve_to with
      | `Literal
      | `Tuple ->
          elems
          (* If no spreads are left, then this is a tuple too! *)
          |> List.fold_left (fun acc elem ->
              match (acc, elem) with
              | None, _ -> None
              | _, ResolvedSpreadArg _ -> None
              | Some tuple_types, ResolvedArg t -> Some (t::tuple_types)
              | _, ResolvedAnySpreadArg _ -> failwith "Should not be hit"
            ) (Some [])
          |> Option.map ~f:List.rev
      | `Array -> None in

      (* We infer the array's general element type by looking at the type of
       * every element in the array *)
      let tset = List.fold_left (fun tset elem ->
        let elemt = match elem with
        | ResolvedSpreadArg (r, arrtype) -> elemt_of_arrtype r arrtype
        | ResolvedArg elemt -> elemt
        | ResolvedAnySpreadArg _ -> failwith "Should not be hit"
        in

        TypeExSet.add elemt tset
      ) TypeExSet.empty elems in

      (* composite elem type is an upper bound of all element types *)
      (* Should the element type of the array be the union of its element types?

         No. Instead of using a union, we use an unresolved tvar to
         represent the least upper bound of each element type. Effectively,
         this keeps the element type "open," at least locally.[*]

         Using a union pins down the element type prematurely, and moreover,
         might lead to speculative matching when setting elements or caling
         contravariant methods (`push`, `concat`, etc.) on the array.

         In any case, using a union doesn't quite work as intended today
         when the element types themselves could be unresolved tvars. For
         example, the following code would work even with unions:

         declare var o: { x: number; }
         var a = ["hey", o.x]; // no error, but is an error if 42 replaces o.x
         declare var i: number;
         a[i] = false;

         [*] Eventually, the element type does get pinned down to a union
         when it is part of the module's exports. In the future we might
         have to do that pinning more carefully, and using an unresolved
         tvar instead of a union here doesn't conflict with those plans.
      *)
      TypeExSet.elements tset |> List.iter (fun t ->
        flow cx (t, UseT (unknown_use, elemt)));

      match tuple_types, resolve_to with
      | _, `Array ->
          DefT (reason_op, ArrT (ArrayAT (elemt, None)))
      | _, `Literal ->
          DefT (reason_op, ArrT (ArrayAT (elemt, tuple_types)))
      | Some tuple_types, `Tuple ->
          DefT (reason_op, ArrT (TupleAT (elemt, tuple_types)))
      | None, `Tuple ->
          DefT (reason_op, ArrT (ArrayAT (elemt, None)))
    end in

    flow_opt_t cx ?trace (result, tout)
  in

  (* If there are no spread elements or if all the spread elements resolved to
   * tuples or array literals, then this is easy. We just flatten them all.
   *
   * However, if we have a spread that resolved to any or to an array of
   * unknown length, then we're in trouble. Basically, any remaining argument
   * might flow to any remaining parameter.
   *)
  let flatten_call_arg =
    let rec flatten r args spread resolved =
      if resolved = []
      then args, spread
      else match spread with
      | None ->
        (match resolved with
        | (ResolvedArg t)::rest ->
          flatten r (t::args) spread rest
        | (ResolvedSpreadArg
            (_, (ArrayAT (_, Some ts) | TupleAT (_, ts))))::rest ->
          let args = List.rev_append ts args in
          flatten r args spread rest
        | ResolvedSpreadArg (r, _)::_
        | ResolvedAnySpreadArg r :: _ ->
          (* We weren't able to flatten the call argument list to remove all
           * spreads. This means we need to build a spread argument, with
           * unknown arity. *)
          let tset = TypeExSet.empty in
          flatten r args (Some (Nel.one r, tset)) resolved
        | [] -> failwith "Empty list already handled"
        )
      | Some (spread_reasons, tset) ->
        let spread_reason, elemt, rest = (match resolved with
        | (ResolvedArg t)::rest ->
          reason_of_t t, t, rest
        | (ResolvedSpreadArg (r, arrtype))::rest ->
          r, elemt_of_arrtype r arrtype, rest
        | (ResolvedAnySpreadArg reason)::rest ->
          reason, AnyT.why reason, rest
        | [] -> failwith "Empty list already handled")
        in
        let spread_reasons = Nel.cons spread_reason spread_reasons in
        let tset = TypeExSet.add elemt tset in
        flatten r args (Some (spread_reasons, tset)) rest

    in
    fun cx r resolved ->
      let args, spread = flatten r [] None resolved in
      let spread = Option.map
        ~f:(fun (spread_reasons, tset) ->
          let last = Nel.hd spread_reasons in
          let first = Nel.(hd (rev spread_reasons)) in
          let loc = Loc.btwn (loc_of_reason first) (loc_of_reason last) in
          let r = mk_reason RArray loc in
          Tvar.mk_where cx r (fun tvar ->
            TypeExSet.elements tset
            |> List.iter (fun t -> flow cx (t, UseT (unknown_use, tvar)))
          )
        )
        spread
      in
      List.rev args, spread

  in

  (* This is used for things like Function.prototype.bind, which partially
   * apply arguments and then return the new function. *)
  let finish_multiflow_partial
    cx ?trace ~use_op ~reason_op ft call_reason resolved tout =
    (* Multiflows always come out of a flow *)
    let trace = match trace with
    | Some trace -> trace
    | None -> failwith "All multiflows show have a trace" in

    let {params; rest_param; return_t; def_reason; _} = ft in

    let args, spread_arg = flatten_call_arg cx reason_op resolved in

    let params, rest_param = multiflow_partial
      cx ~trace ~use_op reason_op ~is_strict:true ~def_reason ~spread_arg ~rest_param
      (args, params) in
    let params_names, params_tlist = List.split params in

    (* e.g. "bound function type", positioned at reason_op *)
    let bound_reason =
      let desc = RBound (desc_of_reason reason_op) in
      replace_reason_const desc call_reason
    in
    let def_reason = reason_op in

    let funt = DefT (reason_op, FunT (
      dummy_static bound_reason,
      dummy_prototype,
      mk_boundfunctiontype params_tlist return_t
        ~rest_param ~def_reason ~params_names
    )) in
    rec_flow_t cx trace (funt, tout)

  in

  (* This is used for things like function application, where all the arguments
   * are applied to a function *)
  let finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict ft resolved =
    (* Multiflows always come out of a flow *)
    let trace = match trace with
    | Some trace -> trace
    | None -> failwith "All multiflows show have a trace" in

    let {params; rest_param; def_reason; _} = ft in

    let args, spread_arg = flatten_call_arg cx reason_op resolved in
    multiflow_full
      cx ~trace ~use_op reason_op ~is_strict ~def_reason
      ~spread_arg ~rest_param (args, params)

  in

  (* Similar to finish_multiflow_full but for custom functions. *)
  let finish_custom_fun_call cx ?trace ~use_op ~reason_op kind tout resolved =
    (* Multiflows always come out of a flow *)
    let trace = match trace with
    | Some trace -> trace
    | None -> failwith "All multiflows show have a trace" in

    let args, spread_arg = flatten_call_arg cx reason_op resolved in
    custom_fun_call cx trace ~use_op reason_op kind args spread_arg tout
  in

  (* This is used for things like Function.prototype.apply, whose second arg is
   * basically a spread argument that we'd like to resolve *)
  let finish_call_t cx ?trace ~use_op ~reason_op funcalltype resolved tin =
    let flattened = flatten_spread_args resolved in
    let call_args_tlist = List.map (function
      | ResolvedArg t -> Arg t
      | ResolvedSpreadArg (r, arrtype) -> SpreadArg (DefT (r, ArrT arrtype))
      | ResolvedAnySpreadArg r -> SpreadArg (AnyT.why r)) flattened in
    let call_t = CallT (use_op, reason_op, { funcalltype with call_args_tlist; }) in
    flow_opt cx ?trace (tin, call_t)

  in
  fun cx ?trace ~use_op ~reason_op resolved resolve_to -> (
    match resolve_to with
    | ResolveSpreadsToTuple (_, elem_t, tout)->
      finish_array cx ?trace ~reason_op ~resolve_to:`Tuple resolved elem_t tout
    | ResolveSpreadsToArrayLiteral (_, elem_t, tout) ->
      finish_array cx ?trace ~reason_op ~resolve_to:`Literal resolved elem_t tout
    | ResolveSpreadsToArray (elem_t, tout) ->
      finish_array cx ?trace ~reason_op ~resolve_to:`Array resolved elem_t tout
    | ResolveSpreadsToMultiflowPartial (_, ft, call_reason, tout) ->
      finish_multiflow_partial cx ?trace ~use_op ~reason_op ft call_reason resolved tout
    | ResolveSpreadsToMultiflowCallFull (_, ft) ->
      finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict:true ft resolved
    | ResolveSpreadsToMultiflowSubtypeFull (_, ft) ->
      finish_multiflow_full cx ?trace ~use_op ~reason_op ~is_strict:false ft resolved
    | ResolveSpreadsToCustomFunCall (_, kind, tout) ->
      finish_custom_fun_call cx ?trace ~use_op ~reason_op kind tout resolved
    | ResolveSpreadsToCallT (funcalltype, tin) ->
      finish_call_t cx ?trace ~use_op ~reason_op funcalltype resolved tin
  )

and perform_lookup_action cx trace propref p lreason ureason = function
  | LookupProp (use_op, up) ->
    rec_flow_p cx trace ~use_op lreason ureason propref (p, up)
  | SuperProp (use_op, lp) ->
    rec_flow_p cx trace ~use_op ureason lreason propref (lp, p)
  | RWProp (use_op, _, tout, rw) ->
    begin match rw, Property.access rw p with
    (* TODO: Sam, comment repositioning logic here *)
    | Read, Some t ->
      let loc = loc_of_reason ureason in
      rec_flow_t cx trace (reposition cx ~trace loc t, tout)
    | Write (_, prop_t), Some t ->
      rec_flow cx trace (tout, UseT (use_op, t));
      Option.iter ~f:(fun prop_t -> rec_flow_t cx trace (t, prop_t)) prop_t
    | _, None ->
      let r, x = match propref with
      | Named (r, x) -> r, Some x
      | Computed t -> reason_of_t t, None
      in
      add_output cx ~trace
        (FlowError.EPropAccess ((r, ureason), x, Property.polarity p, rw, use_op))
    end
  | MatchProp tin ->
    begin match Property.access Read p with
      | Some t -> rec_flow_t cx trace (tin, t)
      | None ->
        let r, x = match propref with
        | Named (r, x) -> r, Some x
        | Computed t -> reason_of_t t, None
        in
        add_output cx ~trace
          (FlowError.EPropAccess ((r, ureason), x, Property.polarity p, Read, unknown_use))
    end

and perform_elem_action cx trace ~use_op reason_op l value = function
  | ReadElem t ->
    let loc = loc_of_reason reason_op in
    rec_flow_t cx trace (reposition cx ~trace loc value, t)
  | WriteElem (tin, tout) ->
    rec_flow cx trace (tin, UseT (use_op, value));
    Option.iter ~f:(fun t -> rec_flow_t cx trace (l, t)) tout
  | CallElem (reason_call, ft) ->
    rec_flow cx trace (value, CallT (use_op, reason_call, ft))

and string_key s reason =
  let key_reason = replace_reason_const (RPropertyIsAString s) reason in
  DefT (key_reason, StrT (Literal (None, s)))

(* builtins, contd. *)

and get_builtin cx ?trace x reason =
  Tvar.mk_where cx reason (fun builtin ->
    let propref = Named (reason, x) in
    flow_opt cx ?trace (builtins cx, GetPropT (unknown_use, reason, propref, builtin))
  )

and lookup_builtin cx ?trace x reason strict builtin =
  let propref = Named (reason, x) in
  let l = builtins cx in
  flow_opt cx ?trace (l,
    LookupT (reason, strict, [], propref, RWProp (unknown_use, l, builtin, Read)))

and get_builtin_typeapp cx ?trace reason x ts =
  typeapp (get_builtin cx ?trace x reason) ts

(* Specialize a polymorphic class, make an instance of the specialized class. *)
and mk_typeapp_instance cx ?trace ~use_op ~reason_op ~reason_tapp ?cache c ts =
  let c = reposition_reason cx ?trace reason_tapp c in
  let t = Tvar.mk cx reason_op in
  flow_opt cx ?trace (c, SpecializeT (use_op, reason_op, reason_tapp, cache, Some ts, t));
  mk_instance cx ?trace (reason_of_t c) t

and mk_typeapp_instance_of_poly cx trace ~use_op ~reason_op ~reason_tapp id xs t ts =
  let t = mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp id xs t ts in
  mk_instance cx ~trace reason_tapp t

and mk_typeapp_of_poly cx trace ~use_op ~reason_op ~reason_tapp ?cache id xs t ts =
  match cache with
  | Some cache ->
    instantiate_poly_with_targs cx trace ~use_op ~reason_op ~reason_tapp ~cache (xs,t) ts
  | None ->
    let key = id, ts in
    match Cache.Subst.find key with
    | None ->
      let errs_ref = ref [] in
      let t = instantiate_poly_with_targs cx trace ~use_op ~reason_op ~reason_tapp
        ~errs_ref (xs,t) ts in
      Cache.Subst.add key (!errs_ref, t);
      t
    | Some (errs, t) ->
      errs |> List.iter (function
        | `ETooManyTypeArgs (reason_arity, maximum_arity) ->
          let msg = FlowError.ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity) in
          add_output cx ~trace msg
        | `ETooFewTypeArgs (reason_arity, maximum_arity) ->
          let msg = FlowError.ETooFewTypeArgs (reason_tapp, reason_arity, maximum_arity) in
          add_output cx ~trace msg
      );
      t

(* NOTE: the for_type flag is true when expecting a type (e.g., when processing
   an annotation), and false when expecting a runtime value (e.g., when
   processing an extends). *)
and mk_instance cx ?trace instance_reason ?(for_type=true) ?(use_desc=false) c =
  if for_type then
    (* Make an annotation. *)
    let source = Tvar.mk_where cx instance_reason (fun t ->
      (* this part is similar to making a runtime value *)
      flow_opt_t cx ?trace (c, DefT (instance_reason, TypeT t))
    ) in
    AnnotT (source, use_desc)
  else
    Tvar.mk_derivable_where cx instance_reason (fun t ->
      flow_opt_t cx ?trace (c, class_type t)
    )

and reposition_reason cx ?trace reason ?(use_desc=false) t =
  reposition
    cx
    ?trace
    (loc_of_reason reason)
    ?desc:(if use_desc then Some (desc_of_reason reason) else None)
    ?annot_loc:(annot_loc_of_reason reason)
    t

(* set the position of the given def type from a reason *)
and reposition cx ?trace loc ?desc ?annot_loc t =
  let mod_reason reason =
    let reason = repos_reason loc ?annot_loc reason in
    match desc with
    | Some d -> replace_reason_const d reason
    | None -> reason
  in
  let rec recurse seen = function
  | OpenT (r, id) as t ->
    let reason = mod_reason r in
    let use_desc = Option.is_some desc in
    let constraints = Context.find_graph cx id in
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
          then Tvar.mk_derivable_where
          else Tvar.mk_where
        in
        mk_tvar_where cx reason (fun tvar ->
          let t' = recurse (IMap.add id tvar seen) t in
          (* All `t` in `Resolved t` are concrete. Because `t` is a concrete
             type, `t'` is also necessarily concrete (i.e., reposition preserves
             open -> open, concrete -> concrete). The unification below thus
             results in resolving `tvar` to `t'`, so we end up with a resolved
             tvar whenever we started with one. *)
          unify_opt cx ?trace ~unify_any:true tvar t';
        ))
    | _ ->
      (* Try to re-use an already created repositioning tvar.
         See repos_cache.ml for details. *)
      match Repos_cache.find id reason !Cache.repos_cache with
      | Some t -> t
      | None ->
        let mk_tvar_where = if is_derivable_reason r
          then Tvar.mk_derivable_where
          else Tvar.mk_where
        in
        mk_tvar_where cx reason (fun tvar ->
          Cache.(repos_cache := Repos_cache.add reason t tvar !repos_cache);
          flow_opt cx ?trace (t, ReposLowerT (reason, use_desc, UseT (unknown_use, tvar)))
        )
    end
  | EvalT (root, defer_use_t, id) as t ->
      (* Modifying the reason of `EvalT`, as we do for other types, is not
         enough, since it will only affect the reason of the resulting tvar.
         Instead, repositioning a `EvalT` should simulate repositioning the
         resulting tvar, i.e., flowing repositioned *lower bounds* to the
         resulting tvar. (Another way of thinking about this is that a `EvalT`
         is just as transparent as its resulting tvar.) *)
      let defer_use_t = mod_reason_of_defer_use_t mod_reason defer_use_t in
      let reason = reason_of_defer_use_t defer_use_t in
      let use_desc = Option.is_some desc in
      begin match Cache.Eval.find_repos root defer_use_t id with
      | Some tvar -> tvar
      | None ->
        Tvar.mk_where cx reason (fun tvar ->
          Cache.Eval.add_repos root defer_use_t id tvar;
          flow_opt cx ?trace (t, ReposLowerT (reason, use_desc, UseT (unknown_use, tvar)))
        )
      end
  | DefT (r, MaybeT t) ->
      (* repositions both the MaybeT and the nested type. MaybeT represets `?T`.
         elsewhere, when we decompose into T | NullT | VoidT, we use the reason
         of the MaybeT for NullT and VoidT but don't reposition `t`, so that any
         errors on the NullT or VoidT point at ?T, but errors on the T point at
         T. *)
      let r = mod_reason r in
      DefT (r, MaybeT (recurse seen t))
  | DefT (r, OptionalT t) ->
      let r = mod_reason r in
      DefT (r, OptionalT (recurse seen t))
  | DefT (r, UnionT rep) ->
      let r = mod_reason r in
      let rep = UnionRep.ident_map (recurse seen) rep in
      DefT (r, UnionT rep)
  | OpaqueT (r, opaquetype) ->
      let r = mod_reason r in
      OpaqueT (r, { opaquetype with
        underlying_t = OptionUtils.ident_map (recurse seen) opaquetype.underlying_t;
        super_t = OptionUtils.ident_map (recurse seen) opaquetype.super_t; })
  | ExactT (r, t) ->
      let r = mod_reason r in
      ExactT (r, recurse seen t)
  | t ->
      mod_reason_of_t mod_reason t
  in
  recurse IMap.empty t

(* given the type of a value v, return the type term
   representing the `typeof v` annotation expression *)
and mk_typeof_annotation cx ?trace reason ?(use_desc=false) t =
  match t with
  | OpenT _ ->
    let source = Tvar.mk_where cx reason (fun t' ->
      flow_opt cx ?trace (t, BecomeT (reason, t'))
    ) in
    AnnotT (source, use_desc)
  | _ ->
    (* Annot will take care of repositioning the source type, using reason_of_t
       of the source type. We don't need to do a full/deep repositioning here,
       since we'll do it then, so just make sure reason_of_t is right. *)
    let t = mod_reason_of_t (
      let loc = loc_of_reason reason in
      repos_reason loc ~annot_loc:loc
    ) t in
    AnnotT (t, use_desc)

and get_builtin_type cx ?trace reason ?(use_desc=false) x =
  let t = get_builtin cx ?trace x reason in
  mk_instance cx ?trace reason ~use_desc t

and get_builtin_prop_type cx ?trace reason tool =
  let x = React.PropType.(match tool with
  | ArrayOf -> "React$PropTypes$arrayOf"
  | InstanceOf -> "React$PropTypes$instanceOf"
  | ObjectOf -> "React$PropTypes$objectOf"
  | OneOf -> "React$PropTypes$oneOf"
  | OneOfType -> "React$PropTypes$oneOfType"
  | Shape -> "React$PropTypes$shape"
  ) in
  get_builtin_type cx ?trace reason x

and instantiate_poly_t cx t = function
  | None -> (* nothing to do *) t
  | Some types -> match t with
      | DefT (_, PolyT (type_params, t_, _)) -> (
        try
          let subst_map = List.fold_left2 (fun acc {name; _} type_ ->
            SMap.add name type_ acc
          ) SMap.empty type_params types in
          subst cx subst_map t_
        with _ ->
          prerr_endline "Instantiating poly type failed";
          t
      )
      | DefT (_, (AnyT | AnyObjT))
      | DefT (_, (TypeT (DefT (_, (AnyT | AnyObjT))))) ->
          t
      | _ ->
        assert_false ("unexpected args passed to instantiate_poly_t: " ^ (string_of_ctor t))

and instantiate_type t =
  match t with
  | ThisClassT (_, t) | DefT (_, ClassT t) -> t
  | _ -> AnyT.why (reason_of_t t) (* ideally, assert false *)

and call_args_iter f = List.iter (function Arg t | SpreadArg t -> f t)

(* There's a lot of code that looks at a call argument list and tries to do
 * something with one or two arguments. Usually this code assumes that the
 * argument is not a spread argument. This utility function helps with that *)
and extract_non_spread cx ~trace = function
| Arg t -> t
| SpreadArg arr ->
    let reason = reason_of_t arr in
    let loc = loc_of_t arr in
    add_output cx ~trace (FlowError.(EUnsupportedSyntax (loc, SpreadArgument)));
    AnyT.why reason

(** TODO: this should rather be moved close to ground_type_impl/resolve_type
    etc. but Ocaml name resolution rules make that require a lot more moving
    code around. **)
and resolve_builtin_class cx ?trace = function
  | DefT (reason, BoolT _) ->
    let bool_t = get_builtin_type cx ?trace reason "Boolean" in
    resolve_type cx bool_t
  | DefT (reason, NumT _) ->
    let num_t = get_builtin_type cx ?trace reason "Number" in
    resolve_type cx num_t
  | DefT (reason, StrT _) ->
    let string_t = get_builtin_type cx ?trace reason "String" in
    resolve_type cx string_t
  | DefT (reason, ArrT arrtype) ->
    let builtin, elemt = match arrtype with
    | ArrayAT (elemt, _) -> get_builtin cx ?trace "Array" reason, elemt
    | TupleAT (elemt, _)
    | ROArrayAT (elemt) -> get_builtin cx ?trace "$ReadOnlyArray" reason, elemt
    | EmptyAT -> get_builtin cx ?trace "$ReadOnlyArray" reason, DefT (reason, EmptyT)
    in
    let array_t = resolve_type cx builtin in
    let array_t = instantiate_poly_t cx array_t (Some [elemt]) in
    instantiate_type array_t
  | t ->
    t

and set_builtin cx ?trace x t =
  let reason = builtin_reason (RCustom x) in
  let propref = Named (reason, x) in
  flow_opt cx ?trace (builtins cx, SetPropT (unknown_use, reason, propref, Normal, t, None))

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

and rec_flow_t cx trace ?(use_op=unknown_use) (t1, t2) =
  rec_flow cx trace (t1, UseT (use_op, t2))

and rec_flow_p cx trace ?(use_op=unknown_use) ?(report_polarity=true)
  lreason ureason propref = function
  (* unification cases *)
  | Field (_, lt, Neutral),
    Field (_, ut, Neutral) ->
    rec_unify cx trace ~use_op lt ut
  (* directional cases *)
  | lp, up ->
    let x = match propref with Named (_, x) -> Some x | Computed _ -> None in
    (match Property.read_t lp, Property.read_t up with
    | Some lt, Some ut ->
      rec_flow cx trace (lt, UseT (use_op, ut))
    | None, Some _ when report_polarity ->
      add_output cx ~trace (FlowError.EPropPolarityMismatch (
        (lreason, ureason), x,
        (Property.polarity lp, Property.polarity up),
        use_op))
    | _ -> ());
    (match Property.write_t lp, Property.write_t up with
    | Some lt, Some ut ->
      rec_flow cx trace (ut, UseT (use_op, lt))
    | None, Some _ when report_polarity ->
      add_output cx ~trace (FlowError.EPropPolarityMismatch (
        (lreason, ureason), x,
        (Property.polarity lp, Property.polarity up),
        use_op))
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
  flow_opt cx ?trace (t1, UseT (unknown_use, t2))

(* Externally visible function for subtyping. *)
(* Calls internal entry point and traps runaway recursion. *)
and flow cx (lower, upper) =
  try
    flow_opt cx (lower, upper)
  with
  | RecursionCheck.LimitExceeded trace ->
    (* log and continue *)
    let rl = reason_of_t lower in
    let ru = reason_of_use_t upper in
    let reasons =
      if is_use upper
      then ru, rl
      else FlowError.ordered_reasons (rl, ru)
    in
    add_output cx ~trace (FlowError.ERecursionLimit reasons)
  | ex ->
    (* rethrow *)
    raise ex

and flow_t cx (t1, t2) =
  flow cx (t1, UseT (unknown_use, t2))

and tvar_with_constraint cx ?trace ?(derivable=false) u =
  let reason = reason_of_use_t u in
  let mk_tvar_where =
    if derivable
    then Tvar.mk_derivable_where
    else Tvar.mk_where
  in
  mk_tvar_where cx reason (fun tvar ->
    flow_opt cx ?trace (tvar, u)
  )

(* Wrapper functions around __unify that manage traces. Use these functions for
   all recursive calls in the implementation of __unify. *)

and rec_unify cx trace ~use_op ?(unify_any=false) t1 t2 =
  let max = Context.max_trace_depth cx in
  __unify cx ~use_op ~unify_any t1 t2
    (Trace.rec_trace ~max t1 (UseT (use_op, t2)) trace)

and unify_opt cx ?trace ?(unify_any=false) t1 t2 =
  let trace = match trace with
  | None -> Trace.unit_trace t1 (UseT (unknown_use, t2))
  | Some trace ->
    let max = Context.max_trace_depth cx in
    Trace.rec_trace ~max t1 (UseT (unknown_use, t2)) trace
  in
  __unify cx ~use_op:unknown_use ~unify_any t1 t2 trace

(* Externally visible function for unification. *)
(* Calls internal entry point and traps runaway recursion. *)
and unify cx t1 t2 =
  try
    unify_opt cx ~unify_any:true t1 t2
  with
  | RecursionCheck.LimitExceeded trace ->
    (* log and continue *)
    let reasons = FlowError.ordered_reasons (reason_of_t t1, reason_of_t t2) in
    add_output cx ~trace (FlowError.ERecursionLimit reasons)
  | ex ->
    (* rethrow *)
    raise ex

and continue cx trace t = function
  | Lower (use_op, l) -> rec_flow cx trace (l, UseT (use_op, t))
  | Upper u -> rec_flow cx trace (t, u)

and react_kit cx trace ~use_op reason_op l u =
  React_kit.run
    ~add_output
    ~reposition
    ~rec_flow
    ~rec_flow_t
    ~get_builtin
    ~get_builtin_type
    ~get_builtin_typeapp
    ~mk_instance
    ~string_key
    ~mk_type_destructor
    ~sealed_in_op:Obj_type.sealed_in_op
    ~union_of_ts
    ~filter_maybe
    cx trace ~use_op reason_op l u

and custom_fun_call cx trace ~use_op reason_op kind args spread_arg tout = match kind with
  | Compose reverse ->
    (* Drop the specific argument reasons since run_compose will emit CallTs
     * with completely unrelated argument reasons. *)
    let use_op = match use_op with
    | Op FunCall {op; fn; args = _} -> Op (FunCall {op; fn; args = []})
    | Op FunCallMethod {op; fn; prop; args = _} -> Op (FunCallMethod {op; fn; prop; args = []})
    | _ -> use_op
    in
    let tin = Tvar.mk cx reason_op in
    let tvar = Tvar.mk cx reason_op in
    run_compose cx trace ~use_op reason_op reverse args spread_arg tin tvar;
    let funt = FunT (
      dummy_static reason_op,
      dummy_prototype,
      mk_functiontype reason_op [tin] ~rest_param:None ~def_reason:reason_op tvar
    ) in
    rec_flow_t cx trace (DefT (reason_op, funt), tout)

  | ReactCreateElement ->
    (match args with
    (* React.createElement(component) *)
    | component::[] ->
      let config =
        let r = replace_reason_const RReactProps reason_op in
        Obj_type.mk_with_proto
          cx r ~sealed:true ~exact:true ~frozen:true (ObjProtoT r)
      in
      rec_flow cx trace (component, ReactKitT (use_op, reason_op,
        React.CreateElement0 (false, config, ([], None), tout)))
    (* React.createElement(component, config, ...children) *)
    | component::config::children ->
      rec_flow cx trace (component, ReactKitT (use_op, reason_op,
        React.CreateElement0 (false, config, (children, spread_arg), tout)))
    (* React.createElement() *)
    | _ ->
      (* If we don't have the arguments we need, add an arity error. *)
      add_output cx ~trace (FlowError.EReactElementFunArity (reason_op, "createElement", 1)))

  | ReactCloneElement -> (match args with
    (* React.cloneElement(element) *)
    | element::[] ->
      (* Create the expected type for our element with a fresh tvar in the
       * component position. *)
      let expected_element =
        get_builtin_typeapp cx ~trace (reason_of_t element)
          "React$Element" [Tvar.mk cx reason_op] in
      (* Flow the element arg to our expected element. *)
      rec_flow_t cx trace (element, expected_element);
      (* Flow our expected element to the return type. *)
      rec_flow_t cx trace (expected_element, tout)
    (* React.cloneElement(element, config, ...children) *)
    | element::config::children ->
      (* Create a tvar for our component. *)
      let component = Tvar.mk cx reason_op in
      (* Flow the element arg to the element type we expect. *)
      rec_flow_t cx trace (
        element,
        get_builtin_typeapp cx ~trace reason_op
          "React$Element" [component]
      );
      (* Create a React element using the config and children. *)
      rec_flow cx trace (component,
        ReactKitT (use_op, reason_op,
          React.CreateElement0 (true, config, (children, spread_arg), tout)))
    (* React.cloneElement() *)
    | _ ->
      (* If we don't have the arguments we need, add an arity error. *)
      add_output cx ~trace (FlowError.EReactElementFunArity (reason_op, "cloneElement", 1)))

  | ReactElementFactory component -> (match args with
    (* React.createFactory(component)() *)
    | [] ->
      let config =
        let r = replace_reason_const RReactProps reason_op in
        Obj_type.mk_with_proto
          cx r ~sealed:true ~exact:true ~frozen:true (ObjProtoT r)
      in
      rec_flow cx trace (component,
        ReactKitT (use_op, reason_op,
          React.CreateElement0 (false, config, ([], None), tout)))
    (* React.createFactory(component)(config, ...children) *)
    | config::children ->
      rec_flow cx trace (component,
        ReactKitT (use_op, reason_op,
          React.CreateElement0 (false, config, (children, spread_arg), tout))))

  | ObjectAssign
  | ObjectGetPrototypeOf
  | ObjectSetPrototypeOf
  | ReactPropType _
  | ReactCreateClass
  | Idx
  | DebugPrint
  | DebugThrow
  | DebugSleep
    -> failwith "implemented elsewhere"

(* Creates the appropriate constraints for the compose() function and its
 * reversed variant. *)
and run_compose cx trace ~use_op reason_op reverse fns spread_fn tin tout =
  match reverse, fns, spread_fn with
    (* Call the tail functions in our array first and call our head function
     * last after that. *)
    | false, fn::fns, _ ->
      let reason = replace_reason_const (RCustom "compose intermediate value")
        (reason_of_t fn) in
      let tvar = Tvar.mk_where cx reason (fun tvar ->
        run_compose cx trace ~use_op reason_op reverse fns spread_fn tin tvar) in
      rec_flow cx trace (fn,
        CallT (use_op, reason, mk_functioncalltype reason_op None [Arg tvar] tout))

    (* If the compose function is reversed then we want to call the tail
     * functions in our array after we call the head function. *)
    | true, fn::fns, _ ->
      let reason = replace_reason_const (RCustom "compose intermediate value")
        (reason_of_t fn) in
      let tvar = Tvar.mk_where cx reason (fun tvar ->
        rec_flow cx trace (fn,
          CallT (use_op, reason, mk_functioncalltype reason_op None [Arg tin] tvar))) in
      run_compose cx trace ~use_op reason_op reverse fns spread_fn tvar tout

    (* If there are no functions and no spread function then we are an identity
     * function. *)
    | _, [], None ->
      rec_flow_t cx trace (tin, tout)

    (* Correctly implementing spreads of unknown arity for the compose function
     * is a little tricky. Let's look at a couple of cases.
     *
     *     const fn = (x: number): string => x.toString();
     *     declare var fns: Array<typeof fn>;
     *     const x = 42;
     *     compose(...fns)(x);
     *
     * This would be invalid. We could have 0 or 1 fn in our fns array, but 2 fn
     * would be wrong because string is incompatible with number. It breaks down
     * as such:
     *
     * 1. x = 42
     * 2. fn(x) = '42'
     * 3. fn(fn(x)) is an error because '42' is not a number.
     *
     * To get an error in this case we would only need to call the spread
     * argument twice. Now let's look at a case where things get recursive:
     *
     *     type Fn = <O>(O) => $PropertyType<O, 'p'>;
     *     declare var fns: Array<Fn>;
     *     const x = { p: { p: 42 } };
     *     compose(...fns)(x);
     *
     * 1. x = { p: { p: 42 } }
     * 2. fn(x) = { p: 42 }
     * 3. fn(fn(x)) = 42
     * 4. fn(fn(fn(x))) throws an error because the p property is not in 42.
     *
     * Here we would need to call fn 3 times before getting an error. Now
     * consider:
     *
     *     type Fn = <O>(O) => $PropertyType<O, 'p'>;
     *     declare var fns: Array<Fn>;
     *     type X = { p: X };
     *     declare var x: X;
     *     compose(...fns)(x);
     *
     * This is valid.
     *
     * To implement spreads in compose functions we first add a constraint based
     * on tin and tout assuming that the spread is empty. Then we emit recursive
     * constraints:
     *
     *     spread_fn(tin) ~> tout
     *     spread_fn(tout) ~> tin
     *
     * The implementation of Flow should be able to terminate these recursive
     * constraints. If it doesn't then we have a bug. *)
    | _, [], Some spread_fn ->
      run_compose cx trace ~use_op reason_op reverse [] None tin tout;
      run_compose cx trace ~use_op reason_op reverse [spread_fn] None tin tout;
      run_compose cx trace ~use_op reason_op reverse [spread_fn] None tout tin

and object_kit =
  let open Object in

  (*******************************)
  (* Shared Object Kit Utilities *)
  (*******************************)

  let read_prop r flags x p =
    let t = match Property.read_t p with
    | Some t -> t
    | None ->
      let reason = replace_reason_const (RUnknownProperty (Some x)) r in
      let t = DefT (reason, MixedT Mixed_everything) in
      t
    in
    t, flags.exact
  in

  let read_dict r {value; dict_polarity; _} =
    if Polarity.compat (dict_polarity, Positive)
    then value
    else
      let reason = replace_reason_const (RUnknownProperty None) r in
      DefT (reason, MixedT Mixed_everything)
  in

  (* Treat dictionaries as optional, own properties. Dictionary reads should
   * be exact. TODO: Forbid writes to indexers through the photo chain.
   * Property accesses which read from dictionaries normally result in a
   * non-optional result, but that leads to confusing spread results. For
   * example, `p` in `{...{|p:T|},...{[]:U}` should `T|U`, not `U`. *)
  let get_prop r p dict =
    match p, dict with
    | Some _, _ -> p
    | None, Some d -> Some (optional (read_dict r d), true)
    | None, None -> None
  in

  (* Lift a pairwise function like spread2 to a function over a resolved list *)
  let merge (f: slice -> slice -> slice) =
    let f' (x0: resolved) (x1: resolved) =
      Nel.map_concat (fun slice1 ->
        Nel.map (f slice1) x0
      ) x1
    in
    let rec loop x0 = function
      | [] -> x0
      | x1::xs -> loop (f' x0 x1) xs
    in
    fun x0 (x1,xs) -> loop (f' x0 x1) xs
  in

  (*****************)
  (* Object Spread *)
  (*****************)

  let object_spread =
    let open Object.Spread in

    (* Compute spread result: slice * slice -> slice *)
    let spread2 reason (r1, props1, dict1, flags1) (r2, props2, dict2, flags2) =
      let union t1 t2 = DefT (reason, UnionT (UnionRep.make t1 t2 [])) in
      let merge_props (t1, own1) (t2, own2) =
        let t1, opt1 = match t1 with DefT (_, OptionalT t) -> t, true | _ -> t1, false in
        let t2, opt2 = match t2 with DefT (_, OptionalT t) -> t, true | _ -> t2, false in
        (* An own, non-optional property definitely overwrites earlier properties.
           Otherwise, the type might come from either side. *)
        let t, own =
          if own2 && not opt2 then t2, own2
          else union t1 t2, own1 || own2
        in
        (* If either property is own, the result is non-optional unless the own
           property is itself optional. Non-own implies optional (see mk_object),
           so we don't need to handle those cases here. *)
        let opt =
          if own1 && own2 then opt1 && opt2
          else own1 && opt1 || own2 && opt2
        in
        let t = if opt then optional t else t in
        t, own
      in
      let props = SMap.merge (fun x p1 p2 ->
        (* Due to width subtyping, failing to read from an inexact object does not
           imply non-existence, but rather an unknown result. *)
        let unknown r =
          let r = replace_reason_const (RUnknownProperty (Some x)) r in
          DefT (r, MixedT Mixed_everything), false
        in
        match get_prop r1 p1 dict1, get_prop r2 p2 dict2 with
        | None, None -> None
        | Some p1, Some p2 -> Some (merge_props p1 p2)
        | Some p1, None ->
          if flags2.exact
          then Some p1
          else Some (merge_props p1 (unknown r2))
        | None, Some p2 ->
          if flags1.exact
          then Some p2
          else Some (merge_props (unknown r1) p2)
      ) props1 props2 in
      let dict = Option.merge dict1 dict2 (fun d1 d2 -> {
        dict_name = None;
        key = union d1.key d2.key;
        value = union (read_dict r1 d1) (read_dict r2 d2);
        dict_polarity = Neutral
      }) in
      let flags = {
        frozen = flags1.frozen && flags2.frozen;
        sealed = Sealed;
        exact =
          flags1.exact && flags2.exact &&
          Obj_type.sealed_in_op reason flags1.sealed &&
          Obj_type.sealed_in_op reason flags2.sealed;
      } in
      reason, props, dict, flags
    in

    let spread reason = function
      | x, [] -> x
      | x0, x1::xs -> merge (spread2 reason) x0 (x1, xs)
    in

    let mk_object cx reason target (r, props, dict, flags) =
      let props = SMap.map (fun (t, own) ->
        (* Spread only copies over own properties. If `not own`, then the property
           might be on a proto object instead, so make the result optional. *)
        let t = match t with
        | DefT (_, OptionalT _) -> t
        | _ -> if own then t else optional t
        in
        Field (None, t, Neutral)
      ) props in
      let id = Context.make_property_map cx props in
      let proto = ObjProtoT reason in
      let flags =
        let exact = match target with
        (* Type spread result is exact if annotated to be exact *)
        | Annot { make_exact } -> make_exact
        (* Value spread result is exact if all inputs are exact *)
        | Value -> flags.exact
        in
        { sealed = Sealed; frozen = false; exact }
      in
      let call = None in
      let t = DefT (r, ObjT (mk_objecttype ~flags ~dict ~call id proto)) in
      (* Wrap the final type in an `ExactT` if we have an exact flag *)
      if flags.exact then ExactT (reason, t) else t
    in

    fun options state cx trace use_op reason tout x ->
      let {todo_rev; acc} = state in
      Nel.iter (fun (r, _, _, {exact; _}) ->
        match options with
        | Annot { make_exact } when make_exact && not exact ->
          add_output cx ~trace (FlowError.
            EIncompatibleWithExact ((r, reason), unknown_use))
        | _ -> ()
      ) x;
      match todo_rev with
      | [] ->
        let t = match spread reason (Nel.rev (x, acc)) with
        | x, [] -> mk_object cx reason options x
        | x0, x1::xs ->
          DefT (reason, UnionT (UnionRep.make
            (mk_object cx reason options x0)
            (mk_object cx reason options x1)
            (List.map (mk_object cx reason options) xs)))
        in
        (* Intentional UnknownUse here. *)
        rec_flow_t cx trace (t, tout)
      | t::todo_rev ->
        let tool = Resolve Next in
        let state = {todo_rev; acc = x::acc} in
        rec_flow cx trace (t, ObjKitT (use_op, reason, tool, Spread (options, state), tout))
  in

  (***************)
  (* Object Rest *)
  (***************)

  let object_rest =
    let open Object.Rest in

    let optional = function
    | (DefT (_, OptionalT _)) as t -> t
    | t -> Type.optional t
    in

    (* Subtract the second slice from the first slice and return the difference
     * slice. The runtime implementation of this type operation is:
     *
     *     const result = {};
     *
     *     for (const p in props1) {
     *       if (hasOwnProperty(props1, p)) {
     *         if (!hasOwnProperty(props2, p)) {
     *           result[p] = props1[p];
     *         }
     *       }
     *     }
     *
     * The resulting object only has a property if the property is own in props1 and
     * it is not an own property of props2.
     *)
    let rest cx trace ~use_op reason merge_mode
      (r1, props1, dict1, flags1)
      (r2, props2, dict2, flags2) =
      let props = SMap.merge (fun k p1 p2 ->
        match merge_mode, get_prop r1 p1 dict1, get_prop r2 p2 dict2, flags2.exact with
        (* If the object we are using to subtract has an optional property, non-own
         * property, or is inexact then we should add this prop to our result, but
         * make it optional as we cannot know for certain whether or not at runtime
         * the property would be subtracted.
         *
         * Sound subtraction also considers exactness and owness to determine
         * optionality. If p2 is maybe-own then sometimes it may not be
         * subtracted and so is optional. If props2 is not exact then we may
         * optionally have some undocumented prop. *)
        | (Sound | IgnoreExactAndOwn),
          Some (t1, _), Some ((DefT (_, OptionalT _) as t2), _), _
        | Sound,
          Some (t1, _), Some (t2, false), _
        | Sound,
          Some (t1, _), Some (t2, _), false ->
          rec_flow cx trace (t1, UseT (use_op, optional t2));
          Some (Field (None, optional t1, Neutral))

        (* Otherwise if the object we are using to subtract has a non-optional own
         * property and the object is exact then we never add that property to our
         * source object. *)
        | (Sound | IgnoreExactAndOwn),
          None, Some (t2, _), _ ->
          let reason = replace_reason_const (RUndefinedProperty k) r1 in
          rec_flow cx trace (VoidT.make reason, UseT (use_op, t2));
          None
        | (Sound | IgnoreExactAndOwn),
          Some (t1, _), Some (t2, _), _ ->
          rec_flow cx trace (t1, UseT (use_op, t2));
          None

        (* If we have some property in our first object and none in our second
         * object, but our second object is inexact then we want to make our
         * property optional and flow that type to mixed. *)
        | Sound,
          Some (t1, _), None, false ->
          rec_flow cx trace (t1, UseT (use_op, MixedT.make r2));
          Some (Field (None, optional t1, Neutral))

        (* If neither object has the prop then we don't add a prop to our
         * result here. *)
        | (Sound | IgnoreExactAndOwn | ReactConfigMerge),
          None, None, _
            -> None

        (* If our first object has a prop and our second object does not have that
         * prop then we will copy over that prop. If the first object's prop is
         * non-own then sometimes we may not copy it over so we mark it
         * as optional. *)
        | IgnoreExactAndOwn, Some (t, _), None, _ -> Some (Field (None, t, Neutral))
        | ReactConfigMerge, Some (t, _), None, _ -> Some (Field (None, t, Positive))
        | Sound, Some (t, true), None, _ -> Some (Field (None, t, Neutral))
        | Sound, Some (t, false), None, _ -> Some (Field (None, optional t, Neutral))

        (* React config merging is special. We are trying to solve for C
         * in the equation (where ... represents spread instead of rest):
         *
         *     {...DP, ...C} = P
         *
         * Where DP and P are known. Consider this case:
         *
         *     {...{p?}, ...C} = {p}
         *
         * The solution for C here is {p} instead of {p?} since
         * {...{p?}, ...{p?}} is {p?} instead of {p}. This is inconsistent with
         * the behavior of other object rest merge modes implemented in this
         * pattern match. *)
        | ReactConfigMerge,
          Some (t1, _), Some (DefT (_, OptionalT t2), _), _ ->
          (* We only test the subtyping relation of t1 and t2 if both t1 and t2
           * are optional types. If t1 is required then t2 will always
           * be overwritten. *)
          (match t1 with
          | DefT (_, OptionalT t1) -> rec_flow_t cx trace (t2, t1)
          | _ -> ());
          Some (Field (None, t1, Positive))
        (* Using our same equation. Consider this case:
         *
         *     {...{p}, ...C} = {p}
         *
         * The solution for C here is {p?}. An empty object, {}, is not a valid
         * solution unless that empty object is exact. Even for exact objects,
         * {|p?|} is the best solution since it accepts more valid
         * programs then {||}. *)
        | ReactConfigMerge,
          Some (t1, _), Some (t2, _), _ ->
          (* The DP type for p must be a subtype of the P type for p. *)
          rec_flow_t cx trace (t2, t1);
          Some (Field (None, optional t1, Positive))
        (* Consider this case:
         *
         *     {...{p}, ...C} = {}
         *
         * For C there will be no prop. However, if the props object is exact
         * then we need to throw an error. *)
        | ReactConfigMerge,
          None, Some (_, _), _ ->
          if flags1.exact then (
            let use_op = Frame (PropertyCompatibility {
              prop = Some k;
              lower = r2;
              upper = r1;
              is_sentinel = false;
            }, unknown_use) in
            let r2 = replace_reason_const (RProperty (Some k)) r2 in
            let err = FlowError.EPropNotFound (Some k, (r2, r1), use_op) in
            add_output cx ~trace err
          );
          None

      ) props1 props2 in
      let dict = match dict1, dict2 with
        | None, None -> None
        | Some dict, None -> Some dict
        | None, Some _ -> None
        (* If our first and second objects have a dictionary then we use our first
         * dictionary, but we make the value optional since any set of keys may have
         * been removed. *)
        | Some dict1, Some dict2 ->
          rec_flow cx trace (dict1.value, UseT (use_op, dict2.value));
          Some ({
            dict_name = None;
            key = dict1.key;
            value = optional dict1.value;
            dict_polarity = Neutral;
          })
      in
      let flags = {
        frozen = false;
        sealed = Sealed;
        exact = flags1.exact && Obj_type.sealed_in_op reason flags1.sealed;
      } in
      let id = Context.make_property_map cx props in
      let proto = ObjProtoT r1 in
      let call = None in
      let t = DefT (r1, ObjT (mk_objecttype ~flags ~dict ~call id proto)) in
      (* Wrap the final type in an `ExactT` if we have an exact flag *)
      if flags.exact then ExactT (r1, t) else t
    in

    fun options state cx trace use_op reason tout x ->
      match state with
      | One t ->
        let tool = Resolve Next in
        let state = Done x in
        rec_flow cx trace (t, ObjKitT (use_op, reason, tool, Rest (options, state), tout))
      | Done base ->
        let xs = Nel.map_concat (fun slice ->
          Nel.map (rest cx trace ~use_op reason options slice) x
        ) base in
        let t = match xs with
          | (x, []) -> x
          | (x0, x1::xs) -> DefT (reason, UnionT (UnionRep.make x0 x1 xs))
        in
        (* Intentional UnknownUse here. *)
        rec_flow_t cx trace (t, tout)
  in

  (********************)
  (* Object Read Only *)
  (********************)

  let object_read_only =
    let polarity = Positive in

    let mk_read_only_object cx reason slice =
      let (r, props, dict, flags) = slice in

      let props = SMap.map (fun (t, _) -> Field (None, t, polarity)) props in
      let dict = Option.map dict (fun dict -> { dict with dict_polarity = polarity }) in
      let call = None in
      let id = Context.make_property_map cx props in
      let proto = ObjProtoT reason in
      let t = DefT (r, ObjT (mk_objecttype ~flags ~dict ~call id proto)) in
      if flags.exact then ExactT (reason, t) else t
    in

    fun cx trace _ reason tout x ->
      let t = match Nel.map (mk_read_only_object cx reason) x with
        | (t, []) -> t
        | (t0, t1::ts) -> DefT (reason, UnionT (UnionRep.make t0 t1 ts))
      in
      (* Intentional UnknownUse here. *)
      rec_flow_t cx trace (t, tout)
  in

  (****************)
  (* React Config *)
  (****************)

  let react_config =
    let open Object.ReactConfig in

    (* All props currently have a neutral polarity. However, they should have a
     * positive polarity (or even better, constant) since React.createElement()
     * freezes the type of props. We use a neutral polarity today because the
     * props type we flow the config into is written by users who very rarely
     * add a positive variance annotation. We may consider marking that type as
     * constant in the future as well. *)
    let prop_polarity = Neutral in

    let finish cx trace reason config defaults children =
      let (config_reason, config_props, config_dict, config_flags) = config in
      (* If we have some type for children then we want to add a children prop
       * to our config props. *)
      let config_props =
        Option.value_map children ~default:config_props ~f:(fun children ->
          SMap.add "children" (children, true) config_props
        )
      in
      (* Remove the key and ref props from our config. We check key and ref
       * independently of our config. So we must remove them so the user can't
       * see them. *)
      let config_props = SMap.remove "key" config_props in
      let config_props = SMap.remove "ref" config_props in
      (* Create the final props map and dict.
       *
       * NOTE: React will copy any enumerable prop whether or not it
       * is own to the config. *)
      let props, dict, flags = match defaults with
        (* If we have some default props then we want to add the types for those
         * default props to our final props object. *)
        | Some (defaults_reason, defaults_props, defaults_dict, defaults_flags) ->
          (* Merge our props and default props. *)
          let props = SMap.merge (fun _ p1 p2 ->
            let p1 = get_prop config_reason p1 config_dict in
            let p2 = get_prop defaults_reason p2 defaults_dict in
            match p1, p2 with
            | None, None -> None
            | Some (t, _), None -> Some (Field (None, t, prop_polarity))
            | None, Some (t, _) -> Some (Field (None, t, prop_polarity))
            (* If a property is defined in both objects, and the first property's
             * type includes void then we want to replace every occurrence of void
             * with the second property's type. This is consistent with the behavior
             * of function default arguments. If you call a function, `f`, like:
             * `f(undefined)` and there is a default value for the first argument,
             * then we will ignore the void type and use the type for the default
             * parameter instead. *)
            | Some (t1, _), Some (t2, _) ->
              (* Use CondT to replace void with t1. *)
              let t = Tvar.mk_where cx reason (fun tvar ->
                rec_flow cx trace (filter_optional cx ~trace reason t1,
                  CondT (reason, t2, tvar))
              ) in
              Some (Field (None, t, prop_polarity))
          ) config_props defaults_props in
          (* Merge the dictionary from our config with the defaults dictionary. *)
          let dict = Option.merge config_dict defaults_dict (fun d1 d2 -> {
            dict_name = None;
            key = DefT (reason, UnionT (UnionRep.make d1.key d2.key []));
            value = DefT (reason, UnionT (UnionRep.make
              (read_dict config_reason d1)
              (read_dict defaults_reason d2) []));
            dict_polarity = prop_polarity;
          }) in
          (* React freezes the config so we set the frozen flag to true. The
           * final object is only exact if both the config and defaults objects
           * are exact. *)
          let flags = {
            frozen = true;
            sealed = Sealed;
            exact =
              config_flags.exact && defaults_flags.exact &&
              Obj_type.sealed_in_op reason config_flags.sealed &&
              Obj_type.sealed_in_op reason defaults_flags.sealed;
          } in
          props, dict, flags
        (* Otherwise turn our slice props map into an object props. *)
        | None ->
          (* All of the fields are read-only so we create positive fields. *)
          let props = SMap.map (fun (t, _) -> Field (None, t, prop_polarity)) config_props in
          (* Create a new dictionary from our config's dictionary with a
           * positive polarity. *)
          let dict = Option.map config_dict (fun d -> {
            dict_name = None;
            key = d.key;
            value = d.value;
            dict_polarity = prop_polarity;
          }) in
          (* React freezes the config so we set the frozen flag to true. The
           * final object is only exact if the config object is exact. *)
          let flags = {
            frozen = true;
            sealed = Sealed;
            exact = config_flags.exact && Obj_type.sealed_in_op reason config_flags.sealed;
          } in
          props, dict, flags
      in
      let call = None in
      (* Finish creating our props object. *)
      let id = Context.make_property_map cx props in
      let proto = ObjProtoT reason in
      let t = DefT (reason, ObjT (mk_objecttype ~flags ~dict ~call id proto)) in
      if flags.exact then ExactT (reason, t) else t
    in

    fun state cx trace use_op reason tout x ->
      match state with
      (* If we have some type for default props then we need to wait for that
       * type to resolve before finishing our props type. *)
      | Config { defaults = Some t; children } ->
        let tool = Resolve Next in
        let state = Defaults { config = x; children } in
        rec_flow cx trace (t, ObjKitT (use_op, reason, tool, ReactConfig state, tout))
      (* If we have no default props then finish our object and flow it to our
       * tout type. *)
      | Config { defaults = None; children } ->
        let ts = Nel.map (fun x -> finish cx trace reason x None children) x in
        let t = match ts with
          | t, [] -> t
          | t0, t1::ts -> DefT (reason, UnionT (UnionRep.make t0 t1 ts))
        in
        rec_flow cx trace (t, UseT (use_op, tout))
      (* If we had default props and those defaults resolved then finish our
       * props object with those default props. *)
      | Defaults { config; children } ->
        let ts = Nel.map_concat (fun c ->
          Nel.map (fun d -> finish cx trace reason c (Some d) children) x
        ) config in
        let t = match ts with
          | t, [] -> t
          | t0, t1::ts -> DefT (reason, UnionT (UnionRep.make t0 t1 ts))
        in
        rec_flow cx trace (t, UseT (use_op, tout))
  in

  (*********************)
  (* Object Resolution *)
  (*********************)

  let next = function
  | Spread (options, state) -> object_spread options state
  | Rest (options, state) -> object_rest options state
  | ReactConfig state -> react_config state
  | ReadOnly -> object_read_only
  in

  (* Intersect two object slices: slice * slice -> slice
   *
   * In general it is unsound to combine intersection types, but since object
   * kit utilities never write to their arguments, it is safe in this specific
   * case.
   *
   * {...{p:T}&{q:U}} = {...{p:T,q:U}}
   * {...{p:T}&{p:U}} = {...{p:T&U}}
   * {...A&(B|C)} = {...(A&B)|(A&C)}
   * {...(A|B)&C} = {...(A&C)|(B&C)}
   *)
  let intersect2 reason (r1, props1, dict1, flags1) (r2, props2, dict2, flags2) =
    let intersection t1 t2 = DefT (reason, IntersectionT (InterRep.make t1 t2 [])) in
    let merge_props (t1, own1) (t2, own2) =
      let t1, t2, opt = match t1, t2 with
      | DefT (_, OptionalT t1), DefT (_, OptionalT t2) -> t1, t2, true
      | DefT (_, OptionalT t1), t2 | t1, DefT (_, OptionalT t2) | t1, t2 -> t1, t2, false
      in
      let t = intersection t1 t2 in
      let t = if opt then optional t else t in
      t, own1 || own2
    in
    let r =
      let loc = Loc.btwn (loc_of_reason r1) (loc_of_reason r2) in
      mk_reason RObjectType loc
    in
    let props = SMap.merge (fun _ p1 p2 ->
      let read_dict r d = optional (read_dict r d), true in
      match p1, p2 with
      | None, None -> None
      | Some p1, Some p2 -> Some (merge_props p1 p2)
      | Some p1, None ->
        (match dict2 with
        | Some d2 -> Some (merge_props p1 (read_dict r2 d2))
        | None -> Some p1)
      | None, Some p2 ->
        (match dict1 with
        | Some d1 -> Some (merge_props (read_dict r1 d1) p2)
        | None -> Some p2)
    ) props1 props2 in
    let dict = Option.merge dict1 dict2 (fun d1 d2 -> {
      dict_name = None;
      key = intersection d1.key d2.key;
      value = intersection (read_dict r1 d1) (read_dict r2 d2);
      dict_polarity = Neutral;
    }) in
    let flags = {
      frozen = flags1.frozen || flags2.frozen;
      sealed = Sealed;
      exact = flags1.exact || flags2.exact;
    } in
    r, props, dict, flags
  in

  let resolved cx trace use_op reason resolve_tool tool tout x =
    match resolve_tool with
    | Next -> next tool cx trace use_op reason tout x
    | List0 ((t, todo), join) ->
      let resolve_tool = Resolve (List (todo, Nel.one x, join)) in
      rec_flow cx trace (t, ObjKitT (use_op, reason, resolve_tool, tool, tout))
    | List (todo, done_rev, join) ->
      match todo with
      | [] ->
        let x = match join with
        | Or -> Nel.cons x done_rev |> Nel.concat
        | And -> merge (intersect2 reason) x done_rev
        in
        next tool cx trace use_op reason tout x
      | t::todo ->
        let done_rev = Nel.cons x done_rev in
        let resolve_tool = Resolve (List (todo, done_rev, join)) in
        rec_flow cx trace (t, ObjKitT (use_op, reason, resolve_tool, tool, tout))
  in

  let object_slice cx r id dict flags =
    let props = Context.find_props cx id in
    let props = SMap.mapi (read_prop r flags) props in
    let dict = Option.map dict (fun d -> {
      dict_name = None;
      key = d.key;
      value = read_dict r d;
      dict_polarity = Neutral;
    }) in
    (r, props, dict, flags)
  in

  let interface_slice cx r id =
    let flags = {frozen=false; exact=false; sealed=Sealed} in
    let id, dict =
      let props = Context.find_props cx id in
      match SMap.get "$key" props, SMap.get "$value" props with
      | Some (Field (_, key, polarity)), Some (Field (_, value, polarity'))
        when polarity = polarity' ->
        let props = props |> SMap.remove "$key" |> SMap.remove "$value" in
        let id = Context.make_property_map cx props in
        let dict = {dict_name = None; key; value; dict_polarity = polarity} in
        id, Some dict
      | _ -> id, None
    in
    object_slice cx r id dict flags
  in

  let resolve cx trace use_op reason resolve_tool tool tout = function
    (* We extract the props from an ObjT. *)
    | DefT (r, ObjT {props_tmap; dict_t; flags; _}) ->
      let x = Nel.one (object_slice cx r props_tmap dict_t flags) in
      resolved cx trace use_op reason resolve_tool tool tout x
    (* We take the fields from an InstanceT excluding methods (because methods
     * are always on the prototype). We also want to resolve fields from the
     * InstanceT's super class so we recurse. *)
    | DefT (r, InstanceT (_, super, _, {fields_tmap; _})) ->
      let resolve_tool = Super (interface_slice cx r fields_tmap, resolve_tool) in
      rec_flow cx trace (super, ObjKitT (use_op, reason, resolve_tool, tool, tout))
    (* Statics of a class. TODO: This logic is unfortunately duplicated from the
     * top-level pattern matching against class lower bounds to object-like
     * uses. This duplication should be removed. *)
    | DefT (r, ClassT i) ->
      let t = Tvar.mk cx r in
      rec_flow cx trace (i, GetStaticsT (r, t));
      rec_flow cx trace (t, ObjKitT (use_op, reason, Resolve resolve_tool, tool, tout))
    (* Resolve each member of a union. *)
    | DefT (_, UnionT rep) ->
      let t, todo = UnionRep.members_nel rep in
      let resolve_tool = Resolve (List0 (todo, Or)) in
      rec_flow cx trace (t, ObjKitT (use_op, reason, resolve_tool, tool, tout))
    (* Resolve each member of an intersection. *)
    | DefT (_, IntersectionT rep) ->
      let t, todo = InterRep.members_nel rep in
      let resolve_tool = Resolve (List0 (todo, And)) in
      rec_flow cx trace (t, ObjKitT (use_op, reason, resolve_tool, tool, tout))
    (* Mirroring Object.assign() and {...null} semantics, treat null/void as
     * empty objects. *)
    | DefT (_, (NullT | VoidT)) ->
      let flags = { frozen = true; sealed = Sealed; exact = true } in
      let x = Nel.one (reason, SMap.empty, None, flags) in
      resolved cx trace use_op reason resolve_tool tool tout x
    (* mixed is treated as {[string]: mixed}. Any JavaScript value may be
     * treated as an object and so this is safe. *)
    | DefT (r, MixedT _) as t ->
      let flags = { frozen = true; sealed = Sealed; exact = true } in
      let x = Nel.one (reason, SMap.empty, Some ({
        dict_name = None;
        key = StrT.make r;
        value = t;
        dict_polarity = Neutral;
      }), flags) in
      resolved cx trace use_op reason resolve_tool tool tout x
    (* If we see an empty then propagate empty to tout. *)
    | DefT (r, EmptyT) ->
      rec_flow cx trace (EmptyT.make r, UseT (use_op, tout))
    (* Propagate any. *)
    | DefT (_, (AnyT | AnyObjT)) ->
      rec_flow cx trace (AnyT.why reason, UseT (use_op, tout))
    (* Other types have reasonable object representations that may be added as
     * new uses of the object kit resolution code is found. *)
    | t ->
      add_output cx ~trace (FlowError.EInvalidObjectKit {
        tool;
        reason = reason_of_t t;
        reason_op = reason;
        use_op;
      })
  in

  let super cx trace use_op reason resolve_tool tool tout acc = function
    | DefT (r, InstanceT (_, super, _, {fields_tmap; _})) ->
      let slice = interface_slice cx r fields_tmap in
      let acc = intersect2 reason acc slice in
      let resolve_tool = Super (acc, resolve_tool) in
      rec_flow cx trace (super, ObjKitT (use_op, reason, resolve_tool, tool, tout))
    | DefT (_, (AnyT | AnyObjT)) ->
      rec_flow cx trace (AnyT.why reason, UseT (use_op, tout))
    | _ ->
      next tool cx trace use_op reason tout (Nel.one acc)
  in

  fun cx trace ~use_op reason resolve_tool tool tout l ->
    match resolve_tool with
    | Resolve resolve_tool -> resolve cx trace use_op reason resolve_tool tool tout l
    | Super (acc, resolve_tool) -> super cx trace use_op reason resolve_tool tool tout acc l

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
      SMap.map (List.fold_left (fun (_, acc) (loc, t) ->
          (* Arbitrarily use the last location encountered *)
          loc, merge_type cx (acc, t)
      ) (None, Locationless.EmptyT.t)) map

(* It's kind of lame that Members is in this module, but it uses a bunch of
   internal APIs so for now it's easier to keep it here than to expose those
   APIs *)
module Members : sig
  type ('success, 'success_module) generic_t =
    | Success of 'success
    | SuccessModule of 'success_module
    | FailureNullishType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  type t = (
    (* Success *) (Loc.t option * Type.t) SMap.t,
    (* SuccessModule *) (Loc.t option * Type.t) SMap.t * (Type.t option)
  ) generic_t

  (* For debugging purposes *)
  val string_of_extracted_type: (Type.t, Type.t) generic_t -> string

  val to_command_result: t -> ((Loc.t option * Type.t) SMap.t, string) result

  val extract: Context.t -> Type.t -> t

  val extract_type: Context.t -> Type.t -> (Type.t, Type.t) generic_t
  val extract_members: Context.t -> (Type.t, Type.t) generic_t -> t

end = struct

  type ('success, 'success_module) generic_t =
    | Success of 'success
    | SuccessModule of 'success_module
    | FailureNullishType
    | FailureAnyType
    | FailureUnhandledType of Type.t

  type t = (
    (* Success *) (Loc.t option * Type.t) SMap.t,
    (* SuccessModule *) (Loc.t option * Type.t) SMap.t * (Type.t option)
  ) generic_t

  let string_of_extracted_type = function
    | Success t -> Printf.sprintf "Success (%s)" (Type.string_of_ctor t)
    | SuccessModule t -> Printf.sprintf "SuccessModule (%s)" (Type.string_of_ctor t)
    | FailureNullishType -> "FailureNullishType"
    | FailureAnyType -> "FailureAnyType"
    | FailureUnhandledType t -> Printf.sprintf "FailureUnhandledType (%s)" (Type.string_of_ctor t)

  let to_command_result = function
    | Success map
    | SuccessModule (map, None) ->
        Ok map
    | SuccessModule (named_exports, Some cjs_export) ->
        Ok (SMap.add "default" (None, cjs_export) named_exports)
    | FailureNullishType ->
        Error "autocomplete on possibly null or undefined value"
    | FailureAnyType ->
        Error "not enough type information to autocomplete"
    | FailureUnhandledType t ->
        Error (spf
          "autocomplete on unexpected type of value %s (please file a task!)"
          (string_of_ctor t))

  let find_props cx fields =
    SMap.filter (fun key _ ->
      (* Filter out keys that start with "$" *)
      not (String.length key >= 1 && key.[0] = '$')
    ) (Context.find_props cx fields)

  let rec extract_type cx this_t = match this_t with
    | DefT (_, MaybeT ty) ->
        extract_type cx ty
    | DefT (_, (NullT | VoidT))
    | InternalT (OptionalChainVoidT _) ->
        FailureNullishType
    | DefT (_, AnyT) ->
        FailureAnyType
    | DefT (reason, AnyObjT) ->
        extract_type cx (get_builtin_type cx reason "Object")
    | DefT (reason, AnyFunT) ->
        let rep = InterRep.make
          (get_builtin_type cx reason "Function")
          (get_builtin_type cx reason "Object")
          []
        in
        extract_type cx (DefT (reason, IntersectionT rep))
    | AnnotT (source_t, _) ->
      let source_t = resolve_type cx source_t in
      extract_type cx source_t
    | DefT (_, InstanceT _ ) as t ->
        Success t
    | DefT (_, ObjT _) as t ->
        Success t
    | ExactT (_, t) ->
        let t = resolve_type cx t in
        extract_type cx t
    | ModuleT _ as t ->
        SuccessModule t
    | ThisTypeAppT (_, c, _, ts_opt) ->
        let c = resolve_type cx c in
        let inst_t = instantiate_poly_t cx c ts_opt in
        let inst_t = instantiate_type inst_t in
        extract_type cx inst_t
    | DefT (_, TypeAppT (_, c, ts)) ->
        let c = resolve_type cx c in
        let inst_t = instantiate_poly_t cx c (Some ts) in
        let inst_t = instantiate_type inst_t in
        extract_type cx inst_t
    | DefT (_, PolyT (_, sub_type, _)) ->
        (* TODO: replace type parameters with stable/proper names? *)
        extract_type cx sub_type
    | ThisClassT (_, DefT (_, InstanceT (static, _, _, _)))
    | DefT (_, ClassT (DefT (_, InstanceT (static, _, _, _)))) ->
        let static_t = resolve_type cx static in
        extract_type cx static_t
    | DefT (_, FunT _) as t ->
        Success t
    | DefT (_, IntersectionT _ ) as t ->
        Success  t
    | DefT (_, UnionT _ ) as t ->
        Success t
    | DefT (reason, SingletonStrT _)
    | DefT (reason, StrT _) ->
        extract_type cx (get_builtin_type cx reason "String")
    | DefT (reason, SingletonNumT _)
    | DefT (reason, NumT _) ->
        extract_type cx (get_builtin_type cx reason "Number")
    | DefT (reason, SingletonBoolT _)
    | DefT (reason, BoolT _) ->
        extract_type cx (get_builtin_type cx reason "Boolean")

    | DefT (reason, CharSetT _) ->
        extract_type cx (get_builtin_type cx reason "String")

    | ReposT (_, t)
    | InternalT (ReposUpperT (_, t)) ->
        extract_type cx t

    | OpaqueT (_, {underlying_t = Some t; _})
    | OpaqueT (_, {super_t = Some t; _})
      -> extract_type cx t

    | AnyWithLowerBoundT _
    | AnyWithUpperBoundT _
    | MergedT _
    | DefT (_, ArrT _)
    | BoundT _
    | InternalT (ChoiceKitT (_, _))
    | TypeDestructorTriggerT _
    | DefT (_, ClassT _)
    | CustomFunT (_, _)
    | MatchingPropT (_, _, _)
    | DefT (_, EmptyT)
    | EvalT (_, _, _)
    | ExistsT _
    | InternalT (ExtendsT _)
    | FunProtoApplyT _
    | FunProtoBindT _
    | FunProtoCallT _
    | FunProtoT _
    | InternalT (IdxWrapper (_, _))
    | KeysT (_, _)
    | DefT (_, MixedT _)
    | NullProtoT _
    | ObjProtoT _
    | OpaqueT _
    | OpenPredT (_, _, _, _)
    | OpenT _
    | DefT (_, OptionalT _)
    | ShapeT _
    | ThisClassT _
    | DefT (_, TypeT _)
      ->
        FailureUnhandledType this_t

  let rec extract_members cx = function
    | FailureNullishType -> FailureNullishType
    | FailureAnyType -> FailureAnyType
    | FailureUnhandledType t -> FailureUnhandledType t
    | Success (DefT (_, InstanceT (_, super, _,
                {fields_tmap = fields;
                methods_tmap = methods;
                _}))) ->
        let members = SMap.fold (fun x p acc ->
          (* TODO: It isn't currently possible to return two types for a given
           * property in autocomplete, so for now we just return the getter
           * type. *)
          let loc, t = match p with
            | Field (loc, t, _)
            | Get (loc, t)
            | Set (loc, t)
            (* arbitrarily use the location for the getter. maybe we can send both in the future *)
            | GetSet (loc, t, _, _)
            | Method (loc, t) ->
                (loc, t)
          in
          SMap.add x (loc, t) acc
        ) (find_props cx fields) SMap.empty in
        let members = SMap.fold (fun x p acc ->
          match Property.read_t p with
          | Some t ->
              let loc = Property.read_loc p in
              SMap.add x (loc, t) acc
          | None -> acc
        ) (find_props cx methods) members in
        let super_t = resolve_type cx super in
        let super_flds = extract_members_as_map cx super_t in
        Success (AugmentableSMap.augment super_flds ~with_bindings:members)
    | Success (DefT (_, ObjT {props_tmap = flds; proto_t = proto; _})) ->
        let proto_reason = reason_of_t proto in
        let rep = InterRep.make
          proto
          (get_builtin_type cx proto_reason "Object")
          []
        in
        let proto_t = resolve_type cx (DefT (proto_reason, IntersectionT rep)) in
        let prot_members = extract_members_as_map cx proto_t in
        let members = SMap.fold (fun x p acc ->
          match Property.read_t p with
          | Some t ->
              let loc = Property.read_loc p in
              SMap.add x (loc, t) acc
          | None -> acc
        ) (find_props cx flds) SMap.empty in
        Success (AugmentableSMap.augment prot_members ~with_bindings:members)
    | SuccessModule (ModuleT (_, {exports_tmap; cjs_export; has_every_named_export = _;}, _)) ->
        let named_exports = Context.find_exports cx exports_tmap in
        let cjs_export =
          match cjs_export with
          | Some t -> Some (resolve_type cx t)
          | None -> None
        in
        SuccessModule (named_exports, cjs_export)
    | Success (DefT (_, FunT (static, proto, _))) ->
        let static_t = resolve_type cx static in
        let proto_t = resolve_type cx proto in
        let members = extract_members_as_map cx static_t in
        let prot_members = extract_members_as_map cx proto_t in
        Success (AugmentableSMap.augment prot_members ~with_bindings:members)
    | Success (DefT (_, IntersectionT rep)) ->
        (* Intersection type should autocomplete for every property of
           every type in the intersection *)
        let ts = InterRep.members rep in
        let ts = List.map (resolve_type cx) ts in
        let members = List.map (extract_members_as_map cx) ts in
        Success (List.fold_left (fun acc members ->
          AugmentableSMap.augment acc ~with_bindings:members
        ) SMap.empty members)
    | Success (DefT (_, UnionT rep)) ->
        (* Union type should autocomplete for only the properties that are in
        * every type in the intersection *)
        let ts = List.map (resolve_type cx) (UnionRep.members rep) in
        let members = ts
          (* Although we'll ignore the any-ish and nullish members of the union *)
          |> List.filter (function
             | DefT (_, (AnyT | AnyObjT | AnyFunT | NullT | VoidT)) -> false
             | _ -> true
             )
          |> List.map (extract_members_as_map cx)
          |> intersect_members cx in
        Success members
    | Success t | SuccessModule t ->
        FailureUnhandledType t

  (* TODO: Think of a better place to put this *)
  and extract cx this_t =
    let t = extract_type cx this_t in
    extract_members cx t

  and extract_members_as_map cx this_t =
    let members = extract cx this_t in
    match to_command_result members with
    | Ok map -> map
    | Error _ -> SMap.empty

end

class assert_ground_visitor skip = object (self)
  inherit [ISet.t] Type_visitor.t as super

  (* Track prop maps which correspond to object literals. We don't ask for
     annotations for object literals which reach exports. Instead, we walk the
     properties covariantly. *)
  val mutable objlits: int Properties.Map.t = Properties.Map.empty

  (* Track prop maps which correspond to instance fields and methods, indicating
     any fields which are initialized. We don't ask for annotations for (a)
     munged property names, which are private and thus not inputs, and (b)
     initialized field names. *)
  val mutable insts: (int * SSet.t) Properties.Map.t = Properties.Map.empty

  val depth = ref 0

  (* Tvars with reasons that match should not be missing annotation errors. *)
  method private skip_reason r =
    match desc_of_reason r with
    (* No possible annotation for `this` type. *)
    | RThis -> true
    | _ -> false

  method private derivable_reason r =
    match desc_of_reason r with
    | RExistential -> true
    | RShadowProperty _ -> true
    | _ -> is_derivable_reason r

  method! tvar cx pole seen r id =
    let root_id, constraints = Context.find_constraints cx id in
    if id != root_id
    then self#tvar cx pole seen r root_id
    else
      let seen' = ISet.add id seen in
      if seen' == seen then seen
      else if ISet.mem id skip then seen'
      else if self#skip_reason r then seen'
      else if
        not (self#derivable_reason r) &&
        Polarity.compat (pole, Negative)
      then (
        unify_opt cx ~unify_any:true (OpenT (r, id)) Locationless.AnyT.t;
        add_output cx (FlowError.EMissingAnnotation r);
        seen'
      )
      else
        let pole = if self#derivable_reason r then Positive else pole in
        List.fold_left (self#type_ cx pole) seen' (types_of constraints)

  method! type_ cx pole seen t =
    Option.iter ~f:(fun { Verbose.depth = verbose_depth; indent} ->
      let pid = Context.pid_prefix cx in
      let indent = String.make (!depth * indent) ' ' in
      prerr_endlinef "\n%s%sassert_ground (%s): %s" indent pid
        (Polarity.string pole)
        (Debug_js.dump_t cx ~depth:verbose_depth t)
    ) (Context.verbose cx);
    incr depth;
    let seen =
      match t with
      | BoundT _ -> seen
      | AnnotT _ -> seen
      | MergedT _ ->
        (* The base class implementation will walk uses here, but there's no
           reasonable way to complain about missing annotations for MergedT,
           which was added to avoid missing annotations. *)
        seen
      | EvalT (_, TypeDestructorT _, _) ->
        (* Type destructors are annotations, so we should never complain about
           missing annotations due them. The default visitor _should_ never
           visit a tvar in an input position, but do to some wacky stuff in
           eval, it's possible today. *)
        seen
      | KeysT _ ->
        (* Same idea as type destructors. *)
        seen
      | DefT (_, TypeAppT (_, c, ts)) ->
        self#typeapp ts cx pole seen c
      | DefT (r, ArrT (ArrayAT (t, ts))) when is_literal_array_reason r ->
        self#arrlit cx pole seen t ts
      | DefT (r, ObjT o) when is_literal_object_reason r ->
        let refcnt =
          try Properties.Map.find_unsafe o.props_tmap objlits
          with Not_found -> 0
        in
        objlits <- Properties.Map.add o.props_tmap (refcnt+1) objlits;
        let seen = super#type_ cx pole seen t in
        objlits <- (
          if refcnt = 0
          then Properties.Map.remove o.props_tmap objlits
          else Properties.Map.add o.props_tmap refcnt objlits
        );
        seen
      | DefT (_, InstanceT (static, _, _, i)) ->
        let static_fields_id = match static with
        | DefT (_, ObjT o) -> Some o.props_tmap
        | _ -> None
        in
        let fields_refcnt =
          try fst (Properties.Map.find_unsafe i.fields_tmap insts)
          with Not_found -> 0
        in
        let methods_refcnt =
          try fst (Properties.Map.find_unsafe i.methods_tmap insts)
          with Not_found -> 0
        in
        let static_refcnt = Option.value_map static_fields_id ~default:0 ~f:(fun id ->
          try fst (Properties.Map.find_unsafe id insts)
          with Not_found -> 0
        ) in
        insts <- Properties.Map.add i.fields_tmap (fields_refcnt+1, i.initialized_field_names) insts;
        insts <- Properties.Map.add i.methods_tmap (methods_refcnt+1, SSet.empty) insts;
        Option.iter static_fields_id (fun id ->
          insts <- Properties.Map.add id (static_refcnt+1, i.initialized_static_field_names) insts
        );
        let seen = super#type_ cx pole seen t in
        insts <- (
          if fields_refcnt = 0
          then Properties.Map.remove i.fields_tmap insts
          else Properties.Map.add i.fields_tmap (fields_refcnt, i.initialized_field_names) insts
        );
        insts <- (
          if methods_refcnt = 0
          then Properties.Map.remove i.methods_tmap insts
          else Properties.Map.add i.methods_tmap (fields_refcnt, SSet.empty) insts
        );
        Option.iter static_fields_id (fun id ->
          insts <- (
            if static_refcnt = 0
            then Properties.Map.remove id insts
            else Properties.Map.add id (static_refcnt, i.initialized_static_field_names) insts
          )
        );
        seen
      | DefT (r, FunT (static, prototype, ft)) ->
        let any = Locationless.AnyT.t in
        unify_opt cx ~unify_any:true static any;
        unify_opt cx ~unify_any:true prototype any;
        unify_opt cx ~unify_any:true ft.this_t any;
        super#type_ cx pole seen
          (DefT (r, FunT (any, any, {ft with this_t = any})))
      | _ -> super#type_ cx pole seen t
    in
    decr depth;
    seen

  method! props cx pole seen id =
    if Properties.Map.mem id objlits
    then self#objlit_props cx pole seen id
    else match Properties.Map.get id insts with
    | Some (_, init) -> self#inst_props cx pole seen id init
    | _ -> super#props cx pole seen id

  method private arrlit cx pole seen t ts =
    let seen = self#type_ cx pole seen t in
    let seen = Option.fold ts ~init:seen ~f:(List.fold_left (self#type_ cx pole)) in
    seen

  method private objlit_props cx pole seen id =
    let props = Context.find_props cx id in
    SMap.fold (fun _ p acc ->
      Property.read_t p |> Option.fold ~f:(self#type_ cx pole) ~init:acc
    ) props seen

  method private inst_props cx pole seen id init =
    let props = Context.find_props cx id in
    SMap.fold (fun x p acc ->
      if is_munged_prop_name cx x
      then acc
      else if SSet.mem x init
      then Property.read_t p |> Option.fold ~f:(self#type_ cx pole) ~init:acc
      else self#prop cx pole acc p
    ) props seen

  method private typeapp =
    let rec loop cx pole seen = function
      | _, [] -> seen
      | [], _ -> seen
      | tparam::tparams, targ::targs ->
        let pole = Polarity.mult (pole, tparam.polarity) in
        let seen = self#type_ cx pole seen targ in
        loop cx pole seen (tparams, targs)
    in
    fun targs cx pole seen -> function
    | OpenT (r, id) ->
      let seen = self#tvar cx Positive seen r id in
      (match Context.find_graph cx id with
      | Resolved t -> self#typeapp targs cx pole seen t
      | Unresolved { lower; _ } ->
        TypeMap.fold (fun t _ acc ->
          self#typeapp targs cx pole acc t
        ) lower seen)
    | AnnotT (t, _) -> self#typeapp targs cx pole seen t
    | DefT (_, PolyT (tparams, _, _)) -> loop cx pole seen (tparams, targs)
    | DefT (_, EmptyT) -> seen
    | DefT (_, AnyT) -> seen
    | c ->
      add_output cx FlowError.(EInternal
        (loc_of_t c, UnexpectedTypeapp (string_of_ctor c)));
      seen
end

let enforce_strict cx id =
  (* First, compute a set of ids to be skipped by calling `assume_ground`. After
     the call, skip_ids contains precisely those ids that correspond to
     requires/imports. *)
  let skip_ids = ref ISet.empty in
  LocMap.iter (fun _ tvar ->
    assume_ground cx skip_ids (UseT (unknown_use, tvar))
  ) (Context.require_map cx);

  (* With the computed skip_ids, call `assert_ground` to force annotations while
     walking the graph starting from id. Typically, id corresponds to
     exports. *)
  let seen =
    let visitor = new assert_ground_visitor !skip_ids in
    let r = locationless_reason (RCustom "") in
    visitor#tvar cx Positive ISet.empty r id
  in
  ignore (seen: ISet.t)

(* Would rather this live elsewhere, but here because module DAG. *)
let mk_default cx reason ~expr = Default.fold
  ~expr:(expr cx)
  ~cons:(fun t1 t2 ->
    Tvar.mk_where cx reason (fun tvar ->
      flow_t cx (t1, tvar);
      flow_t cx (t2, tvar)))
  ~selector:(fun r t sel ->
    let id = mk_id () in
    eval_selector cx r t sel id)
