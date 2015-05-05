(**
 * Copyright (c) 2014, Facebook, Inc.
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

open Utils
open Modes_js
open Reason_js
open Constraint_js
open Type

(* The following functions are used as constructors for function types and
   object types, which unfortunately have many fields, not all of which are
   meaningful in all contexts. This part of the design should be revisited:
   perhaps the data types can be refactored to make them more specialized. *)

(* Methods may use a dummy statics object type to carry properties. We do not
   want to encourage this pattern, but we also don't want to block uses of this
   pattern. Thus, we compromise by not tracking the property types. *)
let dummy_static =
  AnyObjT (reason_of_string "object type for statics")

let dummy_prototype =
  MixedT (reason_of_string "empty prototype object")

let dummy_this =
  MixedT (reason_of_string "global object")

let mk_methodtype this tins pnames tout = {
  this_t = this;
  params_tlist = tins;
  params_names = pnames;
  return_t = tout;
  closure_t = 0
}

let mk_methodtype2 this tins pnames tout j = {
  this_t = this;
  params_tlist = tins;
  params_names = pnames;
  return_t = tout;
  closure_t = j
}

let mk_functiontype tins pnames tout = {
  this_t = dummy_this;
  params_tlist = tins;
  params_names = pnames;
  return_t = tout;
  closure_t = 0
}

let mk_functiontype2 tins pnames tout j = {
  this_t = dummy_this;
  params_tlist = tins;
  params_names = pnames;
  return_t = tout;
  closure_t = j
}

(* An object type has two flags, sealed and exact. A sealed object type cannot
   be extended. An exact object type accurately describes objects without
   "forgeting" any properties: so to extend an object type with optional
   properties, the object type must be exact. Thus, as an invariant, "not exact"
   logically implies "sealed" (and by contrapositive, "not sealed" implies
   "exact"; in other words, exact and sealed cannot both be false).

   Types of object literals are exact, but can be sealed or unsealed. Object
   type annotations are sealed but not exact. *)

let default_flags = { sealed = false; exact = true; frozen = false; }

let mk_objecttype ?(flags=default_flags) dict map proto = {
  flags;
  dict_t = dict;
  props_tmap = map;
  proto_t = proto
}


module GroundIDSet = Set.Make(struct
  type t = Ident.t * bool
  let compare (i1, b1) (i2, b2) =
    let i = Ident.compare i1 i2 in
    if i = 0 then Pervasives.compare b1 b2
    else i
end)


(**************************************************************)

let throw_on_error = ref false

let silent_warnings = false

exception FlowError of (reason * string) list

let add_output cx level message_list =
  if !throw_on_error then raise (FlowError message_list)
  else
    (if modes.debug then
      prerr_endlinef "\nadd_output cx.file = %S\n%s" cx.file (
        String.concat "\n" (
          List.map (fun (r, s) -> spf "r: [%s] s = %S" (dump_reason r) s)
            message_list)));
    let error = level, message_list in
    if level = Errors_js.ERROR || not silent_warnings then
    cx.errors <- Errors_js.ErrorSet.add error cx.errors

(* tvars *)

let mk_id cx = Ident.make ""

let mk_tvar cx reason =
  let tvar = mk_id cx in
  let graph = cx.graph in
  cx.graph <- graph |> IMap.add tvar (new_bounds ());
  (if modes.verbose then prerr_endlinef
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

(* Finds the bounds of a type variable in the graph. If the type variable is not
   a unifier, or is a unifier that is also a root, the bounds are stored with
   the type variable. Otherwise, a chain of type variables may need to be
   traversed to reach such a type variable. *)
let rec find_graph cx id =
  try
    let bounds = IMap.find_unsafe id cx.graph in
    (match bounds.unifier with
      | Some (Goto id) ->
          let id_, bounds_ = find_root cx id in
          bounds.unifier <- Some (Goto id_);
          bounds_
      | _ -> bounds
    )
  with _ ->
    let msg = spf "tvar %d not found in file %s" id cx.file in
    failwith msg

(* Find the root of a unifier, potentially traversing a chain of unifiers, while
   short-circuiting all the unifiers in the chain to the root during traversal
   to speed up future traversals. *)
and find_root cx id =
  let bounds = IMap.find_unsafe id cx.graph in
  match bounds.unifier with
  | Some (Goto id) ->
      let id_, bounds_ = find_root cx id in
      bounds.unifier <- Some (Goto id_);
      id_, bounds_
  | Some (Rank _) -> id, bounds
  | _ -> assert false

let exists_link cx (tvar1, tvar2) =
  let bounds1 = find_graph cx tvar1 in
  IMap.mem tvar2 bounds1.uppertvars

let exists_link_rev cx (tvar1, tvar2) =
  let bounds2 = find_graph cx tvar2 in
  IMap.mem tvar1 bounds2.lowertvars

let exists_upper_bound cx (tvar, t) =
  let bounds = find_graph cx tvar in
  TypeMap.mem t bounds.upper

let exists_lower_bound cx (tvar, t) =
  let bounds = find_graph cx tvar in
  TypeMap.mem t bounds.lower

(**********)
(* frames *)
(**********)

let rec havoc_ctx cx i j =
  if (i = 0 || j = 0) then () else
    let (stack2,_) = IMap.find_unsafe i cx.closures in
    let (stack1,blocks) = IMap.find_unsafe j cx.closures in
    havoc_ctx_ (List.rev blocks, List.rev stack1, List.rev stack2)

and havoc_ctx_ = function
  | (block::blocks_, x1::stack1_, x2::stack2_) when x1 = x2 ->
      (if modes.verbose then prerr_endlinef "HAVOC::%d" x1);
      block := SMap.mapi (fun x {specific;general;def_loc;for_type} ->
        (* internal names (.this, .super, .return, .exports) are read-only *)
        if is_internal_name x
        then create_env_entry ~for_type specific general def_loc
        else create_env_entry ~for_type general general def_loc
      ) !block;
      havoc_ctx_ (blocks_,stack1_,stack2_)
  | _ -> ()

let frames: stack ref = ref []

let mk_frame cx stack ctx =
  let count = mk_id cx in
  let stack = count::stack in
  cx.closures <- IMap.add count (stack, ctx) cx.closures;
  frames := stack

(***************)
(* print utils *)
(***************)

let string_of_flow r1 r2 =
  spf "%s\nis incompatible with\n%s"
    (string_of_reason r1) (string_of_reason r2)

let lib_reason r =
  Files_js.is_lib_file (Relative_path.to_absolute
    (Pos.filename (pos_of_reason r)))

let ordered_reasons l u =
  let rl = reason_of_t l in
  let ru = reason_of_t u in
  if is_use u ||
      pos_of_t l = Pos.none ||
      (lib_reason rl && not (lib_reason ru))
  then ru, rl
  else rl, ru

let prmsg_flow cx level trace msg (r1, r2) =
  let reasons_trace =
    reasons_of_trace ~level:modes.traces trace
    |> List.map (fun r -> r, desc_of_reason r)
  in
  let info =
    if lib_reason r1 && lib_reason r2
    then
      let r = new_reason "" (Pos.make_from
        (Relative_path.create Relative_path.Dummy cx.file)) in
      [r, "inconsistent use of library definitions"]
    else []
  in
  let message_list =
    if (pos_of_reason r2 = Pos.none)
    then [
      r1, spf "%s\n%s %s" (desc_of_reason r1) msg (desc_of_reason r2);
    ]
    else [
      r1, spf "%s\n%s" (desc_of_reason r1) msg;
      r2, (desc_of_reason r2) ^
        (if reasons_trace = [] then ""
       else "\nError path:");
    ]
  in
  add_output cx level (info @ message_list @ reasons_trace)

let prerr_flow cx trace msg l u =
  prmsg_flow cx
    Errors_js.ERROR
    trace
    msg
    (ordered_reasons l u)

let prwarn_flow cx trace msg l u =
  prmsg_flow cx
    Errors_js.WARNING
    trace
    msg
    (ordered_reasons l u)

let tweak_output list =
  List.map (fun (reason, msg) ->
    let desc = desc_of_reason reason in
    let dmsg = if msg = "" then desc else spf "%s\n%s" desc msg in
    reason, dmsg
  ) list

(* for outside calls *)
let new_warning list =
  Errors_js.WARNING, tweak_output list

let add_warning cx list =
  add_output cx Errors_js.WARNING (tweak_output list)

let new_error list =
  Errors_js.ERROR, tweak_output list

let add_error cx list =
  add_output cx Errors_js.ERROR (tweak_output list)

(********************************************************************)

(* Since type maps use the built-in compare function to compare types,
   we need to be careful to keep the shape of types within the boundaries
   of that function. In particular, comparison behaves in unexpected ways
   for references. To get around these issues, we denote references with
   indices in types, and maintain side tables of those indices to the
   denoted references. *)

let mk_propmap cx pmap =
  let id = mk_id cx in
  cx.property_maps <- IMap.add id pmap cx.property_maps;
  id

let find_props cx id =
  IMap.find_unsafe id cx.property_maps

let has_prop cx id x =
  find_props cx id |> SMap.mem x

let read_prop cx id x =
  find_props cx id |> SMap.find_unsafe x

let read_prop_ cx id x =
  find_props cx id |> (fun pmap ->
    let t = SMap.find_unsafe x pmap in
    let pmap = SMap.remove x pmap in
    cx.property_maps <- IMap.add id pmap cx.property_maps;
    t
  )

let write_prop cx id x t =
  let pmap = find_props cx id in
  let pmap = SMap.add x t pmap in
  cx.property_maps <- IMap.add id pmap cx.property_maps

let iter_props cx id f =
  find_props cx id
  |> SMap.iter f

let iter_props_ cx id f =
  find_props cx id
  |> SMap.filter (fun x _ -> not (is_internal_name x))
  |> SMap.iter f

(***************)
(* strict mode *)
(***************)

let possible_types cx id =
  let bounds = find_graph cx id in
  TypeMap.keys bounds.lower

let possible_types_of_type cx = function
  | OpenT (_, id) -> possible_types cx id
  | _ -> []

let distinct_possible_types cx id =
  let bounds = find_graph cx id in
  TypeMap.fold (fun t _ map ->
    let desc = desc_of_reason (reason_of_t t) in
    let info = match SMap.get desc map with
      | Some (t0, count) -> (t0, count + 1)
      | None -> (t, 1)
    in
    SMap.add desc info map
  ) bounds.lower SMap.empty

let suggested_type_cache = ref IMap.empty

let rec list_map2 f ts1 ts2 = match (ts1,ts2) with
  | ([],_) | (_,[]) -> []
  | (t1::ts1,t2::ts2) -> (f (t1,t2)):: (list_map2 f ts1 ts2)

module TypeSet = Set.Make(Type)

let create_union ts =
  UnionT (reason_of_string "union", ts)

let rec merge_type cx = function
  | (NumT _, (NumT _ as t))
  | (StrT _, (StrT _ as t))
  | (BoolT _, (BoolT _ as t))
  | (NullT _, (NullT _ as t))
  | (VoidT _, (VoidT _ as t))
      -> t

  | (AnyT _, t) | (t, AnyT _) -> t

  | (UndefT _, t) | (t, UndefT _) -> t
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
      FunT (
        reason_of_string "function",
        dummy_static, dummy_prototype,
        mk_functiontype tins None tout
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
            (fun x t1_opt t2_opt -> match (t1_opt,t2_opt) with
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

  | (UnionT (_, ts1), UnionT (_, ts2)) ->
      create_union (List.rev_append ts1 ts2)

  | (UnionT (_, ts), t)
  | (t, UnionT (_, ts)) ->
      create_union (t :: ts)

  (* TODO: do we need to do anything special for merging Null with Void,
     Optional with other types, etc.? *)

  | (t1, t2) ->
      create_union [t1; t2]

(* This function does not only resolve every OpenT recursively, but also
   replaces the reasons of types with a uniform ones. It is a left-over bit
   from the old ground_type behavior. *)
and ground_type_impl cx ids t = match t with
  | BoundT _ -> t
  | OpenT (reason, id) ->
      lookup_type cx ids id

  | NumT _ -> NumT.t
  | StrT _ -> StrT.t
  | BoolT _ -> BoolT.t
  | UndefT _ -> UndefT.t
  | NullT _ -> NullT.t
  | VoidT _ -> VoidT.t
  | MixedT _ -> MixedT.t
  | AnyT _ -> AnyT.t

  | FunT (_, _, _, ft) ->
      let tins = List.map (ground_type_impl cx ids) ft.params_tlist in
      let params_names = ft.params_names in
      let tout = ground_type_impl cx ids ft.return_t in
      FunT (
        reason_of_string "function",
        dummy_static, dummy_prototype,
        mk_functiontype tins params_names tout
      )

  | ObjT (_, ot) ->
      let dict = match ot.dict_t with
        | None -> None
        | Some dict ->
            Some { dict with
              key = (ground_type_impl cx ids dict.key);
              value = (ground_type_impl cx ids dict.value);
            }
      in
      let pmap =
        find_props cx ot.props_tmap
        |> SMap.map (ground_type_impl cx ids)
        |> mk_propmap cx
      in
      let proto = AnyT.t in
      ObjT (
        reason_of_string "object",
        mk_objecttype dict pmap proto
      )

  | ArrT (_, t, ts) ->
      ArrT (reason_of_string "array",
            ground_type_impl cx ids t,
            ts |> List.map (ground_type_impl cx ids))

  | MaybeT t ->
      MaybeT (ground_type_impl cx ids t)

  | PolyT (xs, t) ->
      PolyT (xs, ground_type_impl cx ids t)

  | ClassT t ->
      ClassT (ground_type_impl cx ids t)

  | TypeT (_, t) ->
      TypeT (reason_of_string "type",
             ground_type_impl cx ids t)

  | InstanceT (_, static, super, it) ->
      t (* nominal type *)

  | RestT t ->
      RestT (ground_type_impl cx ids t)

  | OptionalT t ->
      OptionalT (ground_type_impl cx ids t)

  | TypeAppT (c, ts) ->
      let c = ground_type_impl cx ids c in
      let ts = List.map (ground_type_impl cx ids) ts in
      TypeAppT (c, ts)

  | IntersectionT (_, ts) ->
      IntersectionT (
        reason_of_string "intersection",
        List.map (ground_type_impl cx ids) ts
      )

  | UnionT (_, ts) ->
      create_union (List.map (ground_type_impl cx ids) ts)

  | LowerBoundT t ->
      LowerBoundT (ground_type_impl cx ids t)

  | UpperBoundT t ->
      UpperBoundT (ground_type_impl cx ids t)

  | AnyObjT _ -> AnyObjT (reason_of_string "any object")
  | AnyFunT _ -> AnyFunT (reason_of_string "any function")

  | ShapeT t ->
      ShapeT (ground_type_impl cx ids t)
  | DiffT (t1, t2) ->
      DiffT (ground_type_impl cx ids t1, ground_type_impl cx ids t2)

  | RecordT (_, t) ->
      RecordT (
        reason_of_string "record",
        ground_type_impl cx ids t
      )

  | _ -> assert false (** TODO **)

and lookup_type_ cx ids id =
  if ISet.mem id ids then assert false
  else
    let ids = ISet.add id ids in
    let bounds = find_graph cx id in
    try
      UndefT.t
      |> TypeMap.fold
          (fun t _ -> fun u -> merge_type cx (ground_type_impl cx ids t, u))
          bounds.lower
    with _ ->
      AnyT.t

and lookup_type cx ids id =
  match IMap.get id !suggested_type_cache with
  | None ->
      let t = lookup_type_ cx ids id in
      suggested_type_cache := !suggested_type_cache |> IMap.add id t;
      t
  | Some t -> t

and resolve_type cx = function
  | OpenT (_, id) ->
      let ts = possible_types cx id in
      List.fold_left (fun u t ->
        merge_type cx (t, u)
      ) UndefT.t ts
  | t -> t

let rec normalize_type cx t =
  match t with
  | FunT (r, static, proto, ft) ->
      let ft =
        { ft with
          return_t = normalize_type cx ft.return_t;
          params_tlist = List.map (normalize_type cx) ft.params_tlist; } in
      FunT (r,
        normalize_type cx static,
        normalize_type cx proto,
        ft)

  | ObjT (r, ot) ->
      let pmap =
        find_props cx ot.props_tmap
        |> SMap.map (normalize_type cx)
        |> mk_propmap cx
      in
      ObjT (r,
        { ot with
          dict_t = (match ot.dict_t with
          | None -> None
          | Some dict ->
              Some { dict with
                key = normalize_type cx dict.key;
                value = normalize_type cx dict.value;
              });
          proto_t = normalize_type cx ot.proto_t;
          props_tmap = pmap; })

  | UnionT (r, ts) ->
      normalize_union cx r ts

  | IntersectionT (r, ts) ->
      normalize_intersection cx r ts

  | MaybeT t ->
      let t = normalize_type cx t in
      (match t with
      | MaybeT _ -> t
      | _ -> MaybeT t)

  | OptionalT t ->
      OptionalT (normalize_type cx t)

  | RestT t ->
      RestT (normalize_type cx t)

  | ArrT (r, t, ts) ->
      ArrT (r, normalize_type cx t, List.map (normalize_type cx) ts)

  | PolyT (xs, t) ->
      PolyT (xs, normalize_type cx t)

  | TypeAppT (c, ts) ->
      TypeAppT (
        normalize_type cx c,
        List.map (normalize_type cx) ts
      )

  | LowerBoundT t ->
      LowerBoundT (normalize_type cx t)

  | UpperBoundT t ->
      UpperBoundT (normalize_type cx t)

  | ShapeT t ->
      ShapeT (normalize_type cx t)
  | DiffT (t1, t2) ->
      DiffT (normalize_type cx t1, normalize_type cx t2)

  | RecordT (r, t) ->
      RecordT (r, normalize_type cx t)

  (* TODO: Normalize all types? *)
  | t -> t

(* TODO: This is not an exhaustive list of normalization steps for unions.
   For example, we might want to get rid of AnyT in the union similar to how
   merge_type gets rid of AnyT. Decide on rules like these and implement them
   if required. *)
and normalize_union cx r ts =
  let ts = List.map (normalize_type cx) ts in
  let ts = collect_union_members ts in
  let (ts, has_void, has_null) =
    TypeSet.fold (fun t (ts, has_void, has_null) ->
      match t with
      | MaybeT (UnionT (_, tlist)) ->
          let ts = List.fold_left (fun acc t -> TypeSet.add t acc) ts tlist in
          (ts, true, true)
      | MaybeT t -> (TypeSet.add t ts, true, true)
      | VoidT _ -> (ts, true, has_null)
      | NullT _ -> (ts, has_void, true)
      (* TODO: We should only get UndefT here when a completely open type
         variable has been in the union before grounding it. This happens when
         "null" is passed to a function parameter. We throw this out because
         it gives no information at all. merge_type also ignores UndefT. *)
      | UndefT _ -> (ts, has_void, has_null)
      | _ -> (TypeSet.add t ts, has_void, has_null)
    ) ts (TypeSet.empty, false, false) in
  let ts =
    match (has_void, has_null) with
    | (true, false) -> TypeSet.add VoidT.t ts
    | (false, true) -> TypeSet.add NullT.t ts
    | _ ->
        (* We should never get an empty set at this point but better safe than
           sorry. Stripping out UndefT above might be unsafe. *)
        if TypeSet.is_empty ts
        then TypeSet.singleton UndefT.t
        else ts
  in
  let ts = TypeSet.elements ts in
  let t =
    match ts with
    | [t] -> t
    | _ -> UnionT (r, ts)
  in
  if has_void && has_null
  then MaybeT t
  else t

and collect_union_members ts =
  List.fold_left (fun acc x ->
      match x with
      | UnionT (_, ts) -> TypeSet.union (collect_union_members ts) acc
      | _ -> TypeSet.add x acc
    ) TypeSet.empty ts

(* TODO: This does not do any real normalization yet, it only flattens the
   intesection. Think about normalization rules and implement them when there
   is need for that. *)
and normalize_intersection cx r ts =
  let ts = List.map (normalize_type cx) ts in
  let ts = collect_intersection_members ts in
  let ts = TypeSet.elements ts in
  match ts with
  | [t] -> t
  | _ -> IntersectionT (r, ts)

and collect_intersection_members ts =
  List.fold_left (fun acc x ->
      match x with
      | IntersectionT (_, ts) ->
          TypeSet.union acc (collect_intersection_members ts)
      | _ ->
          TypeSet.add x acc
    ) TypeSet.empty ts

let ground_type cx type_ =
  ground_type_impl cx ISet.empty type_

let rec printify_type cx t =
  match t with
  | FunT (r, static, proto, ft) ->
      let ft =
        { ft with
          return_t = printify_type cx ft.return_t;
          params_tlist = List.map (printify_type cx) ft.params_tlist; } in
      FunT (r,
        printify_type cx static,
        printify_type cx proto,
        ft)

  | ObjT (r, ot) ->
      let pmap =
        find_props cx ot.props_tmap
        |> SMap.map (printify_type cx)
        |> mk_propmap cx
      in
      ObjT (r,
        { ot with
          dict_t = (match ot.dict_t with
          | None -> None
          | Some dict ->
              Some { dict with
                key = printify_type cx dict.key;
                value = printify_type cx dict.value;
              });
          proto_t = printify_type cx ot.proto_t;
          props_tmap = pmap; })

  | UnionT (r, ts) ->
      let (ts, add_maybe) =
        List.fold_left (fun (ts, add_maybe) t ->
            let t = printify_type cx t in
            match t with
            | NullT _ -> (ts, true)
            | _ -> (t :: ts, add_maybe)
          ) ([], false) ts
      in
      (* strictly speaking this is a combination of normalization and
         transformation for printability, but it allows us to get rid of
         another normalize_type call. *)
      let t =
        match ts with
        | [t] -> t
        | _ -> UnionT (r, ts)
      in
      if add_maybe
      then MaybeT t
      else t

  | IntersectionT (r, ts) ->
      IntersectionT (r, List.map (printify_type cx) ts)

  | MaybeT t ->
      (* strictly speaking this is a combination of normalization and
         transformation for printability, but it allows us to get rid of
         another normalize_type call. *)
      let t = printify_type cx t in
      (match t with
      | MaybeT _ -> t
      | _ -> MaybeT t)

  | OptionalT t ->
      OptionalT (printify_type cx t)

  | RestT t ->
      RestT (printify_type cx t)

  | ArrT (r, t, ts) ->
      ArrT (r, printify_type cx t, List.map (printify_type cx) ts)

  | PolyT (xs, t) ->
      PolyT (xs, printify_type cx t)

  | TypeAppT (c, ts) ->
      TypeAppT (
        printify_type cx c,
        List.map (printify_type cx) ts
      )

  | LowerBoundT t ->
      LowerBoundT (printify_type cx t)

  | UpperBoundT t ->
      UpperBoundT (printify_type cx t)

  | ShapeT t ->
      ShapeT (printify_type cx t)
  | DiffT (t1, t2) ->
      DiffT (printify_type cx t1, printify_type cx t2)

  | RecordT (r, t) ->
      RecordT (r, printify_type cx t)

  | t -> t

let printified_type cx t =
  let t = ground_type cx t in
  let t = normalize_type cx t in
  printify_type cx t

module Exists(M: MapSig) =
  struct
    let exists pred map =
      M.fold (fun k _ found -> found || pred k) map false
  end

module ExistsTypeMap = Exists(TypeMap)

let check_upper_bound cx id f =
  let bounds = find_graph cx id in
  bounds.upper |> ExistsTypeMap.exists f

let check_lower_bound cx id f =
  let bounds = find_graph cx id in
  bounds.lower |> ExistsTypeMap.exists f

let blame_map = ref IMap.empty

(* Mark a type variable if it depends on a `require`d module. This function is
   used in assert_ground to relax missing annotation errors when an exported
   type contains type variables that depend on a required module: e.g., when the
   superclass of an exported class is `require`d, we should not insist on an
   annotation for the superclass! *)
let is_required cx id = IMap.mem id !blame_map

(* need to consider only "def" types *)
let rec assert_ground ?(infer=false) cx ids = function
  | BoundT _ -> ()

  | OpenT (_, id) when GroundIDSet.mem (id, infer) !ids -> ()
  | OpenT (reason_open, id) ->
      ids := !ids |> GroundIDSet.add (id, infer);
      (* Type variables that are not forced to be annotated include those that
         are dependent on requires, or whose reasons indicate that they are
         derivable. The latter category includes annotations and builtins. *)
      if not (is_required cx id || is_derivable_reason reason_open)
      then
        if infer
        then
          let bounds = find_graph cx id in
          bounds.lower |> TypeMap.iter (fun t _ ->
            assert_ground cx ids t
          )
        else
          (* Disabling. This computation is based off inadequate, infer-only
             information. In contrast, a type-at-pos computation would also take
             into account information during the merge phase. *)
          (*
          let tmap = distinct_possible_types cx id in
          let possible_types = List.rev (
            SMap.fold (fun desc (t, count) list ->
              let desc = match count with
                | 1 -> desc
                | 2 -> desc ^ " (1 more site)"
                | _ -> spf "%s (%d more sites)" desc count
              in
              ((reason_of_t t), desc) :: list
            ) tmap []
          ) in
          *)
          let possible_types = [] in
          let msg = if possible_types = []
            then "Missing annotation"
            else "Missing annotation. Possible values:" in
          let message_list = [
            reason_open, spf "%s\n%s" (desc_of_reason reason_open) msg;
          ] @ possible_types in
          add_output cx Errors_js.WARNING message_list

  | NumT _
  | StrT _
  | BoolT _
  | UndefT _
  | MixedT _
  | AnyT _
  | NullT _
  | VoidT _
    ->
      ()

  | FunT (reason, _, _, { params_tlist = params; return_t = ret; _ }) ->
      let f = assert_ground cx ids in
      List.iter f params;
      f ret

  | PolyT (xs,t) ->
      assert_ground cx ids t

  | ObjT (reason, { props_tmap = id; _ }) ->
      iter_props cx id (fun _ -> assert_ground ~infer:true cx ids)

  | ArrT (reason, t, ts) ->
      assert_ground cx ids t;
      ts |> List.iter (assert_ground cx ids)

  | ClassT t -> assert_ground cx ids t

  | TypeT (reason, t) -> assert_ground cx ids t

  | InstanceT (reason, static, super, instance) ->
      let f = assert_ground cx ids in
      iter_props cx instance.fields_tmap (fun _ -> f);
      iter_props cx instance.methods_tmap (fun _ -> f);
      f super

  | RestT (t) -> assert_ground cx ids t

  | OptionalT (t) -> assert_ground cx ids t

  | TypeAppT(c,ts) ->
      assert_ground ~infer:true cx ids c;
      List.iter (assert_ground cx ids) ts

  | MaybeT(t) -> assert_ground cx ids t

  | IntersectionT(reason,ts) ->
      List.iter (assert_ground cx ids) ts

  | UnionT(reason,ts) ->
      List.iter (assert_ground cx ids) ts

  | UpperBoundT(t) ->
      assert_ground cx ids t

  | LowerBoundT(t) ->
      assert_ground cx ids t

  | AnyObjT _ -> ()
  | AnyFunT _ -> ()

  | ShapeT(t) ->
      assert_ground cx ids t
  | DiffT(t1, t2) ->
      assert_ground cx ids t1;
      assert_ground cx ids t2

  | EnumT(reason,t) ->
      assert_ground cx ids t

  | RecordT(reason,t) ->
      assert_ground cx ids t

  | CJSExportDefaultT (reason,t) ->
      assert_ground ~infer:true cx ids t

  | t -> failwith (streason_of_t t) (** TODO **)

(* To avoid complaining about "missing" annotations where external types are
   used in the exported type, we mark requires and their uses as types, and if
   (and only if) we encounter such uses in the exported type, we record strict
   dependencies on those requires instead. Currently, we mark requires or their
   uses as types only when they are directly part of the exported type. In the
   future we may consider using gc to crawl the graph further down from
   requires---and maybe also up from exports---but preliminary experiments along
   those lines suggest that the rules then becomes hard to
   explain/understand/justify. For now, we have simpler but stricter rules,
   leaving further relaxing of these rules for future work. *)
let rec assume_ground cx ids = function
  | OpenT(_,id) ->
      assume_ground_ cx ids id

  (* The subset of operations to crawl. The type variables denoting the results
     of these operations would be ignored by the is_required check in
     `assert_ground`. *)
  | SummarizeT(_,t)
  | CallT(_,{ return_t = t; _ })
  | MethodT(_,_,_,_,t,_)
  | GetT(_,_,t)
  | GetElemT(_,_,t)
  | ConstructorT(_,_,t)
  | TypeT(_, t)
  | AdderT(_,_,t)
  | AndT(_,_,t)
  | OrT(_,_,t)
  | PredicateT(_,t)
  | SpecializeT(_,_,t)
  | ObjAssignT(_,_,t,_,_)
  | ObjRestT(_,_,t)
  | KeyT(_,t)
  | ImportModuleNsT(_,t)

      -> assume_ground cx ids t

  | _ -> ()

and assume_ground_ cx ids id =
  if not (ISet.mem id !ids) then (
    ids := !ids |> ISet.add id;
    let bounds = find_graph cx id in
    bounds.upper |> TypeMap.iter (fun t _ ->
      assume_ground cx ids t
    );
    bounds.uppertvars |> IMap.iter (fun id _ ->
      assume_ground_ cx ids id
    )
  )

let enforce_strict cx id bounds =
  SSet.iter (fun r ->
    let tvar = SMap.find_unsafe r cx.modulemap in
    let ids = ref ISet.empty in
    assume_ground cx ids tvar;
    !ids |> ISet.iter (fun id ->
      let rs = match IMap.get id !blame_map with
        | None -> SSet.singleton r
        | Some rs -> SSet.add r rs
      in
      blame_map := !blame_map |> IMap.add id rs
    );
  ) cx.required;

  (* TODO: finer analysis to infer strict dependencies *)
  cx.strict_required <- cx.required;

  let ids = ref (GroundIDSet.singleton (id, false)) in
  bounds.lower |> TypeMap.iter (fun t _ ->
    assert_ground cx ids t
  );

  blame_map := IMap.empty

(**************)
(* builtins *)
(**************)

(* created in the master process, populated and saved to ContextHeap.
   forked workers will have an empty replica from the master, but it's useless.
   should only be accessed through ContextHeap. *)
let master_cx = new_context (Files_js.get_flowlib_root ()) Files_js.lib_module

(* builtins is similarly created in the master and replicated on fork.
   this is critical under the current somewhat fragile scheme, as all
   contexts agree on the id of this type var, and builtins are imported
   via this agreement between master_cx and others *)
let builtins = mk_tvar master_cx (builtin_reason "module")

(********)

module Union(M: MapSig) =
  struct
    (* This function is used to merge two maps. It is parametric on the key
       type. When the maps overlap, the second map overrides the first map. *)
    let union f map1 map2 =
      M.fold (fun k v map ->
        if M.mem k map then map else M.add k (f v) map
      ) map1 map2
  end

module UnionIMap = Union(IMap)
module UnionTypeMap = Union(TypeMap)

(***********************)
(* instantiation utils *)
(***********************)

let debug_count =
  let count = ref 0 in
  fun f ->
    incr count;
    prerr_endlinef "[%d] %s" !count (f())

let debug_flow (l,u) =
  spf "%s ~> %s" (string_of_ctor l) (string_of_ctor u)

(*********************************************************************)

module Cache = struct

  (*
    CAUTION:

    Before doing any work on the typechecker, it's important to understand
    that the currenct caching strategy has two counterintuitive effects:
    - calls to __flow on arbitrary type pairs may not run
    - simple changes to the internal representation of type terms
      may cause completely unrelated changes to typechecking behavior.

    Superficially this looks like a cache of visited type pairs. It is not.
    The cache keys here are pairs of hashes, not type terms themselves.
    __flow will not typecheck a pair whose hashes are present, thus one
    visited pair may suppress the checking of other, unrelated pairs.

    The algorithm as it currently stands depends on this false positive
    behavior to terminate. When a non-lossy scheme is substituted, the
    algorithm is subject to multiple unbounded recursions, e.g. in our
    test suite.

    The function used to compute type term hashes is created with a call
    to Hashtbl.hash_param below. The arguments to this call are extremely
    important tuning parameters: they specify a fixed amount of internal
    structure to be traversed in the course of generating a hash value.
    The current values appear tuned to avoid endless rucursion, without
    omitting too many typechecks.

    Improving this situation is a high priority. Meanwhile, bear in mind:

    - since there is no precise definition of which typechecks will be
    silently declined, any unexpected typechecking behavior may have
    such an omission at its source. Verify that your typecheck is actually
    making it past the guard at the top of the __flow function.

    - because the hashing function is parameterized by a fixed quantity
    of internal type term structure to consider, typechecking behavior
    is susceptible to changes in the *internal structure* of types.
    In particular, adding structure will cause the quantity of false
    positives to increase, meaning that fewer typechecks will actually
    be performed. Be sure to check that the typechecker still works as
    expected after any such internal change, no matter how seemingly
    innocuous.
  *)

  module F = struct
    let cache = Hashtbl.create 0
    let hash = Hashtbl.hash_param 50 100
    let mem (l,u) =
      let types = hash l, hash u in
      try
        let hits = Hashtbl.find cache types in
        Hashtbl.add cache types (hits+1);
        true
      with _ ->
        Hashtbl.add cache types 0;
        false
  end

  let clear () =
    Hashtbl.clear F.cache
end

(*********************************************************************)

(********************)
(* subtype relation *)
(********************)

(** NOTE: Do not call this function directly. Instead, call the wrapper
    functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
    this module, and the function `flow` outside this module. **)
let rec __flow cx (l,u) trace =
  if not (Cache.F.mem (l,u)) then (

    if (not (is_use l)) then () else failwith (string_of_t cx l);

    (if modes.verbose
     then prerr_endlinef
        "\n# %s ~>\n# %s"
        (dump_reason (reason_of_t l))
        (dump_reason (reason_of_t u)));

    if ground_subtype (l,u) then () else match (l,u) with

    (* if X -> Y exists then that flow has already been processed *)
    | (OpenT(reason1,tvar1),OpenT(_,tvar2))
        when exists_link cx (tvar1,tvar2)
          -> ()

    (**********************************************************************)
    (* Unpack CommonJS exports early to guarantee that bounds are checked *)
    (* on the exports data rather than on the CJSExportDefaultT wrapper.  *)
    (**********************************************************************)
    | (CJSExportDefaultT(_, exported_t), ImportModuleNsT(reason, t)) ->
      (* Copy the exports into a new object, skipping any "default" keys *)
      let ns_obj = mk_object_with_proto cx reason (MixedT reason) in
      let ns_obj = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace
          (exported_t,
           ObjAssignT(reason, ns_obj, t, ["default"], false))
      ) in

      (* Set the 'default' property on the new object to point at the exports *)
      rec_flow cx trace (ns_obj, SetT(reason, "default", exported_t));

      (**
       * Now that we've constructed the derived ES6 module namespace object for
       * these CommonJS exports, flow them to the import to be treated like an
       * ES6 -> ES6 import.
       *)
      rec_flow cx trace (ns_obj, u)

    | (ObjT(reason_o, {props_tmap = mapr; _;}), ExportDefaultT(reason, t)) ->
        let exported_props = find_props cx mapr in
        let exported_props = SMap.add "default" t exported_props in
        cx.property_maps <- IMap.add mapr exported_props cx.property_maps;
        rec_flow cx trace (l, SetT(reason, "default", t))

    (******************)
    (* process X ~> Y *)
    (******************)

    (* initial link bounds: X' -> X, Y -> Y' *)
    (* initial type bounds: L => X, Y => U *)
    | (OpenT(reason1,tvar1),OpenT(reason2,tvar2)) ->

      let bounds1 = find_graph cx tvar1 in
      let bounds2 = find_graph cx tvar2 in

      (* tvar.uppertvars += tvar2
         tvar.uppertvars += tvar2.uppertvars
         tvar.upper += tvar2.upper *)
      let flow_lowertvar tvar tracelist =
        let bounds = find_graph cx tvar in
        (* final: tvar -> Y# *)
        if not bounds2.cleared then
          bounds.uppertvars <-
            IMap.add tvar2 (concat_trace tracelist) bounds.uppertvars;
        (* final: tvar -> Y'# *)
        let f2 = (fun trace_u -> concat_trace (tracelist @ [trace_u])) in
        bounds.uppertvars <-
          UnionIMap.union f2 bounds2.uppertvars bounds.uppertvars;
        (* final: tvar => U# *)
        bounds.upper <-
          UnionTypeMap.union f2 bounds2.upper bounds.upper
      in
      (* final: X -> Y#, X -> Y'#, X => U# *)
      if not bounds1.cleared then
        flow_lowertvar tvar1 [trace];
      (* final: X' -> Y#, X' -> Y'#, X' => U# *)
      bounds1.lowertvars |> IMap.iter (fun tvar trace_l ->
        if tvar != tvar1 then flow_lowertvar tvar [trace_l;trace]);

      (* tvar.lowertvars += tvar1
         tvar.lowertvars += tvar1.lowertvars
         tvar.lower += tvar1.lower *)
      let flow_uppertvar tvar tracelist =
        let bounds = find_graph cx tvar in
        (* final: X# -> tvar *)
        if not bounds1.cleared then
          bounds.lowertvars <-
            IMap.add tvar1 (concat_trace tracelist) bounds.lowertvars;
        (* final: X'# -> tvar *)
        let f1 = (fun trace_l -> concat_trace ([trace_l] @ tracelist)) in
        bounds.lowertvars <-
          UnionIMap.union f1 bounds1.lowertvars bounds.lowertvars;
        (* final: L# => tvar *)
        bounds.lower <-
          UnionTypeMap.union f1 bounds1.lower bounds.lower
      in
      (* final: X# -> Y, X'# -> Y, L# -> Y *)
      if not bounds2.cleared then
        flow_uppertvar tvar2 [trace];
      (* final: X# -> Y', X'# -> Y', L# -> Y' *)
      bounds2.uppertvars |> IMap.iter (fun tvar trace_u ->
        if tvar != tvar2 then flow_uppertvar tvar [trace;trace_u]);

      (* final: process L ~> U *)
      bounds1.lower |> TypeMap.iter
          (fun l trace_l -> bounds2.upper |> TypeMap.iter
              (fun u trace_u ->
                join_flow cx [trace_l;trace;trace_u] (l,u)))

    (******************)
    (* process Y ~> U *)
    (******************)

    (* if Y => U exists then that flow has already been processed *)
    | (OpenT(reason,tvar),t) when exists_upper_bound cx (tvar,t)
        -> ()

    (* initial: Y' -> Y *)
    (* initial: L => Y *)
    | (OpenT(reason,tvar),t) ->
      let bounds = find_graph cx tvar in
      (* final: Y => U *)
      if not bounds.cleared then
        bounds.upper <- TypeMap.add t trace bounds.upper;
      bounds.lowertvars |> IMap.iter (fun tvar' trace_l ->
        let bounds' = find_graph cx tvar' in
        if tvar != tvar' then
          (* final: Y' => U *)
          bounds'.upper <-
            TypeMap.add t (concat_trace [trace_l;trace]) bounds'.upper
      );

      (* final: process L ~> U *)
      bounds.lower |> TypeMap.iter (fun l trace_l ->
        join_flow cx [trace_l;trace] (l,t)
      )

    (******************)
    (* process L ~> X *)
    (******************)

    (* if U => X exists then that flow has already been processed *)
    | (t,OpenT(reason,tvar)) when exists_lower_bound cx (tvar,t)
        -> ()

    (* initial: X -> X' *)
    (* initial: X => U *)
    | (t,OpenT(reason,tvar)) ->
      let bounds = find_graph cx tvar in
      (* final: L => X *)
      if not bounds.cleared then
        bounds.lower <- TypeMap.add t trace bounds.lower;
      bounds.uppertvars |> IMap.iter (fun tvar' trace_u ->
        let bounds' = find_graph cx tvar' in
        if tvar != tvar' then
          (* final: L => X' *)
          bounds'.lower <-
            TypeMap.add t (concat_trace [trace;trace_u]) bounds'.lower
      );

      (* final: process L ~> U *)
      bounds.upper |> TypeMap.iter (fun u trace_u ->
        join_flow cx [trace;trace_u] (t,u)
      )

    (*******************************)
    (* Handle non-CommonJS imports *)
    (*******************************)
    | (_, ImportModuleNsT(reason, t)) ->
      let ns_obj_sealed = mk_tvar_where cx reason (fun t ->
        rec_flow cx trace (l, ObjSealT(reason, t))
      ) in
      rec_flow cx trace (ns_obj_sealed, t)

    (**************************************************************************)
    (* Unwrap any CommonJS export objects that are flowing to something other *)
    (* than a ImportModuleNsT.                                                *)
    (**************************************************************************)
    | (CJSExportDefaultT(_, t), _) ->
      rec_flow cx trace (t, u)

    (*******************************************************************)
    (* Import type creates a properly-parameterized type alias for the *)
    (* remote type -- but only for particular, valid remote types.     *)
    (*******************************************************************)
    | (ClassT(inst), ImportTypeT(reason, t)) ->
      rec_flow cx trace (TypeT(reason, inst), t)

    | (PolyT(typeparams, ClassT(inst)), ImportTypeT(reason, t)) ->
      rec_flow cx trace (PolyT(typeparams, TypeT(reason, inst)), t)

    | (FunT(_, _, prototype, _), ImportTypeT(reason, t)) ->
      rec_flow cx trace (TypeT(reason, prototype), t)

    | (PolyT(typeparams, FunT(_, _, prototype, _)), ImportTypeT(reason, t)) ->
      rec_flow cx trace (PolyT(typeparams, TypeT(reason, prototype)), t)

    | (TypeT _, ImportTypeT(reason, t))
    | (PolyT(_, TypeT _), ImportTypeT(reason, t))
      -> rec_flow cx trace (l, t)

    (**
     * TODO: Delete this once the legacy export-type hacks have been eliminated
     *       in favor of the newer, first class export-type feature.
     *
     *       TODO(jeffmo) Task(6860853)
     *)
    | (ObjT _, ImportTypeT(reason, t)) ->
      rec_flow cx trace (l, t)

    | (_, ImportTypeT(reason, t)) ->
      add_error cx [
        reason,
        "`import type` only works on exported classes, functions, and type aliases!"
      ]

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

    (************************************************)
    (* bound variables are equal only to themselves *)
    (************************************************)

    | (BoundT _, _) | (_, BoundT _) when l = u ->
      ()

    (********************************************)
    (* summary types forget literal information *)
    (********************************************)

    | (StrT (_, Some _), SummarizeT (reason, t)) ->
      rec_unify cx trace (StrT.why reason) t

    | (NumT (_, Some _), SummarizeT (reason, t)) ->
      rec_unify cx trace (NumT.why reason) t

    | (_, SummarizeT (reason, t)) ->
      rec_unify cx trace l t

    (*******************************)
    (* common implicit convertions *)
    (*******************************)

    | (_, NumT _) when numeric l -> ()

    | (_, AnyObjT _) when object_like l -> ()
    | (AnyObjT _, _) when object_like u || object_like_op u -> ()

    | (_, AnyFunT _) when function_like l -> ()
    | (AnyFunT _, _) when function_like u || function_like_op u -> ()

    (***************)
    (* maybe types *)
    (***************)

    (** The type maybe(T) is the same as null | undefined | T *)

    | ((NullT _ | VoidT _), MaybeT _) -> ()

    | (MaybeT(t), _) ->
      let reason = reason_of_t t in
      rec_flow cx trace (NullT.why reason, u);
      rec_flow cx trace (VoidT.why reason, u);
      rec_flow cx trace (t, u)

    (******************)
    (* optional types *)
    (******************)

    (** The type optional(T) is the same as undefined | T *)

    | (VoidT _, OptionalT _) -> ()

    | (OptionalT(t), _) ->
      let reason = reason_of_t t in
      rec_flow cx trace (VoidT.why reason, u);
      rec_flow cx trace (t, u)

    (*****************)
    (* logical types *)
    (*****************)

    | (left, AndT(reason, right, u)) ->
      (* a falsy && b ~> a
         a truthy && b ~> b
         a && b ~> a falsy | b *)
      let truthy_left = filter_exists left in
      (match truthy_left with
      | UndefT _ ->
        (* falsy *)
        rec_flow cx trace (left, PredicateT (NotP ExistsP, u))
      | _ ->
        (match filter_not_exists left with
        | UndefT _ -> (* truthy *) rec_flow cx trace (right, u)
        | _ ->
          rec_flow cx trace (left, PredicateT (NotP ExistsP, u));
          (match truthy_left with
          | UndefT _ -> ()
          | _ -> rec_flow cx trace (right, u))
        )
      )

    | (left, OrT(reason, right, u)) ->
      (* a truthy || b ~> a
         a falsy || b ~> b
         a || b ~> a truthy | b *)
      let falsy_left = filter_not_exists left in
      (match falsy_left with
      | UndefT _ ->
        (* truthy *)
        rec_flow cx trace (left, PredicateT (ExistsP, u))
      | _ ->
        (match filter_exists left with
        | UndefT _ -> (* falsy *) rec_flow cx trace (right, u)
        | _ ->
          rec_flow cx trace (left, PredicateT (ExistsP, u));
          (match falsy_left with
          | UndefT _ -> ()
          | _ -> rec_flow cx trace (right, u))
        )
      )

    (*****************************)
    (* upper and lower any types *)
    (*****************************)

    (** UpperBoundT and LowerBoundT are very useful types that concisely model
        subtyping constraints without introducing unwanted effects: they can
        appear on both sides of a type, but only have effects in one of those
        sides. In some sense, they are liked bounded AnyT: indeed, AnyT has the
        same behavior as UpperBoundT(UndefT) and LowerBoundT(MixedT). Thus,
        these types can be used instead of AnyT when some precise typechecking
        is required without overconstraining the system. A completely static
        alternative would be achieved with bounded type variables, which Flow
        does not support yet. **)

    | (UpperBoundT t, _) ->
      rec_flow cx trace (t,u)

    | (_, UpperBoundT t) ->
      ()

    | (LowerBoundT t, _) ->
      ()

    | (_, LowerBoundT t) ->
      rec_flow cx trace (l,t)

    (** In general, typechecking is monotonic in the sense that more constraints
        produce more errors. However, sometimes we may want to speculatively try
        out constraints, backtracking if they produce errors (and removing the
        errors produced). This is useful to typecheck union types and
        intersection types: see below. **)

    (***************)
    (* union types *)
    (***************)

    | (UnionT(_,ts), _) ->
      ts |> List.iter (fun t -> rec_flow cx trace (t,u))

    (** To check that the concrete type l may flow to some type in ts, we try
        each type in ts in turn. The types in ts may be type variables, but by
        routing them through SpeculativeMatchFailureT, we ensure that they are
        tried only when their concrete uses are available. The different
        branches of unions are assumed to be disjoint at the top level, so that
        it is always sound to pick the first type in ts that matches the type
        l. *)

    | (SpeculativeMatchFailureT(r,t1,t), t2) ->
      (* Reached when trying to match t1 with t2|t. We speculatively match t1
         with t2, and on failure, (recursively) try to match t1 with t. *)
      if speculative_flow_error cx trace t1 t2
      then rec_flow cx trace (t1, t)

    | (_, UnionT(r,ts)) ->
      try_union cx trace l r ts

    | (t1, MaybeT(t2)) ->
      rec_flow cx trace (t1,t2)

    | (t1, OptionalT(t2)) ->
      rec_flow cx trace (t1, t2)

    (**********************)
    (* intersection types *)
    (**********************)

    | (_, IntersectionT(_,ts)) ->
      ts |> List.iter (fun t -> rec_flow cx trace (l,t))

    (** To check that some type in ts may flow to the concrete type u, we try
        each type in ts in turn. The types in ts may be type variables, but by
        routing them through SpeculativeMatchFailureT, we ensure that they are
        tried only when their concrete definitionss are available. Note that
        unlike unions, the different branches of intersections are usually not
        distinct at the top level (e.g., they may all be function types, or
        object types): instead, they are assumed to be disjoint in their parts,
        so we must suffiently concretize parts of u to make the disjointness
        evident when the different branches are tried; see below. *)

    | (t1, SpeculativeMatchFailureT(r,t,t2)) ->
      (* Reached when trying to match t1&t with t2. We speculatively match t1
         with t2, and on failure, (recursively) try to match t with t2. *)
      if speculative_flow_error cx trace t1 t2
      then rec_flow cx trace (t, t2)

    | (IntersectionT (r,ts), ConcreteT u) ->
      try_intersection cx trace u r ts

    | (t, ConcretizeT (l, ts1, ts2, u)) ->
      concretize_parts cx trace l u (t::ts2) ts1

    | (IntersectionT (r,ts), _) ->
      concretize_parts cx trace l u [] (parts_to_concretize cx u)

    (***************************************)
    (* generic function may be specialized *)
    (***************************************)

    | (PolyT (ids,t), SpecializeT(reason,ts,tvar)) ->
      let t_ = instantiate_poly_ cx trace reason (ids,t) ts in
      rec_flow cx trace (t_, tvar)

    | (TypeAppT(c,ts), _) ->
        let reason = reason_of_t u in
        let t = mk_typeapp_instance cx reason c ts in
        rec_flow cx trace (t, u)

    | (_, TypeAppT(c,ts)) ->
        let reason = reason_of_t l in
        let t = mk_typeapp_instance cx reason c ts in
        rec_flow cx trace (l, t)

    | (PolyT _, PolyT _) -> () (* TODO *)

    | (PolyT (ids,t), _) -> (* TODO: limit the scope of this rule *)
      let reason = reason_of_t u in
      let t_ = instantiate_poly cx trace reason (ids,t) in
      rec_flow cx trace (t_, u)

    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    | (FunT (reason1,_,_,
             {this_t = o1; params_tlist = tins1; return_t = t1; closure_t = i; _}),
       FunT (reason2,_,_,
             {this_t = o2; params_tlist = tins2; return_t = t2; closure_t = j; _}))
      ->
      rec_flow cx trace (o2,o1);
        multiflow cx trace (u, l) (tins2,tins1) |> ignore;
        rec_flow cx trace (t1,t2);
        havoc_ctx cx i j

    | (FunT (reason1,_,_,
             {this_t = o1; params_tlist = tins1; return_t = t1; closure_t = i; _}),
       CallT (reason2,
              {this_t = o2; params_tlist = tins2; return_t = t2; closure_t = j; _}))
      ->
      rec_flow cx trace (o2,o1);
        multiflow cx trace (u, l) (tins2,tins1) |> ignore;
        (* relocate the function's return type at the call site *)
        let t1 = repos_t_from_reason reason2 t1 in
        rec_flow cx trace (t1,t2);
        havoc_ctx cx i j

    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    | (ObjT (reason1, {props_tmap = flds1; proto_t; flags; dict_t = dict1; _ }),
       ObjT (reason2, {props_tmap = flds2; proto_t = u_; dict_t = dict2; _ }))
      ->
      (* if inflowing type is literal (thus guaranteed to be
         unaliased), propertywise subtyping is sound *)
      let desc1 = (desc_of_reason reason1) in
      let lit =
        (desc1 = "object literal")
        || (Str.string_match (Str.regexp ".*React") desc1 0)
      in
      iter_props_ cx flds2
        (fun s -> fun t2 ->
          if (not(has_prop cx flds1 s))
          then
            (* property doesn't exist in inflowing type *)
            let reason2 = replace_reason (spf "property %s" s) reason2 in
            match t2 with
            | OptionalT t1 when flags.exact ->
              (* if property is marked optional or otherwise has a maybe type,
                 and if inflowing type is exact (i.e., it is not an
                 annotation), then it is ok to relax the requirement that the
                 property be found immediately; instead, we constrain future
                 lookups of the property in inflowing type *)
              dictionary cx trace (string_key s reason2) t1 dict1;
              rec_flow cx trace (l, LookupT (reason2, None, s, t1))
            | _ ->
              (* otherwise, we do strict lookup of property in prototype *)
              rec_flow cx trace (proto_t, LookupT (reason2, Some reason1, s, t2))
          (* TODO: instead, consider extending inflowing type with s:t2 when it
             is not sealed *)
          else
            let t1 = read_prop cx flds1 s in
            flow_to_mutable_child cx trace lit t1 t2);
      (* Any properties in l but not u must match indexer *)
      iter_props_ cx flds1
        (fun s -> fun t1 ->
          if (not(has_prop cx flds2 s))
          then dictionary cx trace (string_key s reason1) t1 dict2
        );
      rec_flow cx trace (l, u_)

    | (InstanceT (reason1, _, super, { fields_tmap; methods_tmap; _ }),
       ObjT (reason2, {props_tmap = flds2; proto_t = u_; _ }))
      ->
      let fields_tmap = find_props cx fields_tmap in
      let methods_tmap = find_props cx methods_tmap in
      let flds1 = SMap.union fields_tmap methods_tmap in
      iter_props_ cx flds2
        (fun s -> fun t2 ->
          if (not(SMap.mem s flds1))
          then
            let reason2 = replace_reason (spf "property %s" s) reason2 in
            match t2 with
            | OptionalT t1 ->
                rec_flow cx trace (l, LookupT (reason2, None, s, t1))
            | _ ->
                rec_flow cx trace (super, LookupT (reason2, Some reason1, s, t2))
          else
            let t1 = SMap.find_unsafe s flds1 in
            rec_unify cx trace t1 t2
        );
      rec_flow cx trace (l, u_)

    | (ObjT (reason2, _),
       InstanceT (reason1, _, super, { fields_tmap; methods_tmap; _ }))
      ->
      let fields_tmap = find_props cx fields_tmap in
      let methods_tmap = find_props cx methods_tmap in
      let methods_tmap = SMap.remove "constructor" methods_tmap in
      let flds2 = SMap.union fields_tmap methods_tmap in
      flds2 |> SMap.iter
          (fun s t2 ->
            let reason2 = replace_reason (spf "property %s" s) reason2 in
            rec_flow cx trace (l, LookupT (reason2, Some reason1, s, t2))
          );
      rec_flow cx trace (l, super)

    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | (ObjT _, (FunT _ | CallT _)) ->
      let reason = reason_of_t u in
      let tvar = mk_tvar cx reason in
      lookup_prop cx trace l reason (Some (reason_of_t l)) "$call" tvar;
      rec_flow cx trace (tvar, u)

    (******************************)
    (* matching shapes of objects *)
    (******************************)

    | (_, ShapeT (o2)) ->
        let reason = reason_of_t o2 in
        rec_flow cx trace (l, ObjAssignT(reason, o2, AnyT.t, [], false))

    | (ShapeT (o1), _) ->
        rec_flow cx trace (o1, u)

    | (_, DiffT (o1, o2)) ->
        let reason = reason_of_t o2 in
        let t2 = mk_tvar cx reason in
        rec_flow cx trace (o2, ObjRestT (reason, [], t2));
        rec_flow cx trace (l, ObjAssignT(reason, t2, o1, [], false))

    (********************************************)
    (* array types deconstruct into their parts *)
    (********************************************)

    | ArrT (r1, t1, ts1), ArrT (r2, t2, ts2) ->
      let lit = (desc_of_reason r1) = "array literal" in
      array_flow cx trace lit (ts1, t1, ts2, t2)

    (**************************************************)
    (* instances of classes follow declared hierarchy *)
    (**************************************************)

    | (InstanceT _, InstanceT _) ->
      rec_flow cx trace (l, ExtendsT(l,u))

    | (InstanceT (_,_,super,instance),
       ExtendsT(_, InstanceT (_,_,_,instance_super))) ->

      if instance.class_id = instance_super.class_id
      then
        unify_map cx trace instance.type_args instance_super.type_args
      else
        rec_flow cx trace (super, u)

    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)

    | (ClassT(it), TypeT(_,t)) ->
      rec_flow cx trace (it, t);
      rec_flow cx trace (t, it)

    | (FunT(reason,_,prototype,_), TypeT(_,t)) ->
      rec_flow cx trace (prototype, t);
      rec_flow cx trace (t, prototype)

    | (TypeT(_,l), TypeT(_,u)) ->
      rec_unify cx trace l u

    | (ClassT(l), ClassT(u)) ->
      rec_unify cx trace l u

    | (FunT(_,static1,prototype,_), ClassT(InstanceT(_,static2,_, _) as u_)) ->
      rec_unify cx trace static1 static2;
      rec_unify cx trace prototype u_

    (*********************************************************)
    (* class types derive instance types (with constructors) *)
    (*********************************************************)

    | (ClassT(InstanceT(reason_c,static,super,instance)),
       ConstructorT(reason_op,tins2,t))
      ->
      let methods = find_props cx instance.methods_tmap in
      (match SMap.get "constructor" methods with
      | Some (FunT (reason,_,_,{params_tlist = tins1; _})) ->
        (* TODO: closure *)
        multiflow cx trace (u, l) (tins2,tins1) |> ignore

      | _ -> () (* TODO *)
      );
      let it = InstanceT(reason_c,static,super,instance) in
      rec_flow cx trace (it, t)

    (****************************************************************)
    (* function types derive objects through explicit instantiation *)
    (****************************************************************)

    | (FunT (reason,_, proto, {
      this_t = this;
      params_tlist = tins1;
      return_t = tout1;
      _
    }),
       ConstructorT(reason_op,tins2,t))
      -> (* TODO: closure *)
      multiflow cx trace (u, l) (tins2, tins1) |> ignore;
        let reason_rv = replace_reason "return undefined" reason in
        rec_flow cx trace (tout1, VoidT reason_rv);

        let reason_c = replace_reason "new object" reason in
        let o = mk_object_with_proto cx reason_c proto in
        rec_flow cx trace (o, this);
        rec_flow cx trace (o, t)

    (*************************)
    (* "statics" can be read *)
    (*************************)

    | (InstanceT (_,static,_,_), GetT (_,"statics",t))
      ->
      rec_flow cx trace (static, t)

    | (MixedT reason, GetT(_, "statics", u))
      ->
      (* MixedT serves as the instance type of the root class, and also as the
         statics of the root class. *)
      rec_flow cx trace (MixedT (prefix_reason "statics of " reason), u)

    (********************************************************)
    (* instances of classes may have their fields looked up *)
    (********************************************************)

    | (InstanceT(reason,_,super,instance),
       LookupT (_,strict,x, t))
      ->
      let strict = if instance.mixins then None else strict in
      let pmap = match strict, t with
        (* t = UpperBoundT _ means that the lookup is trying to write t, rather
           than read t. Existing places that play a role here are set_prop and
           get_prop, which use UpperBoundT and LowerBoundT, respectively. The
           general pattern has been used previously, e.g. to distinguish element
           writes from reads.

           strict = Some _ means that we want to throw errors when x is not
           found. Some lookups are non-strict (e.g. when we want to enforce
           consistency between properties if they exist higher up in the
           inheritance chain), and for methods, we want the consistency to be
           one-way, so we use UpperBoundT _, and we don't want methods to be
           excluded from the lookup in that case, obviously. *)
        | Some _, UpperBoundT _ ->
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
      | Some tx ->
        rec_flow cx trace (t,tx); rec_flow cx trace (tx,t)
      )

    (********************************)
    (* ... and their fields written *)
    (********************************)

    | (InstanceT (reason_c,static,super,instance),
       SetT(reason_op,x,tin))
      ->
      (* methods are immutable, so we hide them from property set operations *)
      let fields = instance.fields_tmap in
      set_prop cx trace reason_op reason_c super x fields tin

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | (InstanceT (_, _, super, _), GetT (_, "__proto__", t)) ->

      rec_flow cx trace (super,t)

    | (InstanceT (reason_c,static,super,instance),
       GetT(reason_op,x,tout)) ->

      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      let strict = if instance.mixins then None else Some reason_c in
      get_prop cx trace reason_op strict super x fields tout

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | (InstanceT (reason_c,static,super,instance),
       MethodT(reason_op,x,this,tins2,tout2,j))
      -> (* TODO: closure *)

      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let methods = SMap.union fields_tmap methods_tmap in
      let funt = mk_tvar cx reason_op in
      let strict = if instance.mixins then None else Some reason_c in
      get_prop cx trace reason_op strict super x methods funt;
      let callt = CallT (reason_op, mk_methodtype2 this tins2 None tout2 j) in
      rec_flow cx trace (funt, callt)

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

        var o = {}; // o:T, T |-> {}
        o.x = 4; // T |-> {x:X}, number >=> X
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
      iter_props cx mapr (fun x t ->
        if not (List.mem x props_to_skip) then (
          let reason = prefix_reason (spf "prop %s of " x) reason in
          rec_flow cx trace (proto, SetT (reason, x, t));
        );
      );
      rec_flow cx trace (proto, t)

    | (InstanceT (reason_, _, _, { fields_tmap; methods_tmap; _ }),
       ObjAssignT (reason, proto, t, props_to_skip, false)) ->
      let fields_tmap = find_props cx fields_tmap in
      let methods_tmap = find_props cx methods_tmap in
      let map = SMap.union fields_tmap methods_tmap in
      map |> SMap.iter (fun x t ->
        if not (List.mem x props_to_skip) then (
          rec_flow cx trace (proto, SetT (reason, x, t));
        );
      );
      rec_flow cx trace (proto, t)

    | (MixedT _, ObjAssignT (_, proto, t, _, false)) ->
      rec_flow cx trace (proto, t)

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
      let o = mk_object_with_map_proto cx reason map (MixedT reason) in
      rec_flow cx trace (o, t)

    (*************************************)
    (* objects can be copied-then-sealed *)
    (*************************************)
    | (ObjT (_, { props_tmap = mapr; _ }), ObjSealT (reason, t)) ->
      let src_props = find_props cx mapr in
      let new_obj =
        mk_object_with_map_proto cx reason ~sealed:true src_props l
      in
      rec_flow cx trace (new_obj, t)

    (*************************)
    (* objects can be frozen *)
    (*************************)

    | (ObjT (r, objtype), ObjFreezeT (_, t)) ->
      let flags = {frozen = true; sealed = true; exact = true;} in
      let new_obj = ObjT (prefix_reason "frozen " r, {objtype with flags}) in
      rec_flow cx trace (new_obj, t)

    (*******************************************)
    (* objects may have their fields looked up *)
    (*******************************************)

    | (ObjT (reason_obj, {
      props_tmap = mapr;
      proto_t = proto;
      _
    }),
       LookupT(reason_op,strict,x,t_other))
      ->
      let t = ensure_prop cx strict mapr x proto reason_obj reason_op trace in
      rec_flow cx trace (t, UnifyT(t, t_other))

    (*****************************************)
    (* ... and their fields written *)
    (*****************************************)

    | (ObjT (_, {flags; _}), SetT(_, "constructor", _)) ->
      if flags.frozen then prerr_flow cx trace "Mutation not allowed on" l u

    (** o.x = ... has the additional effect of o[_] = ... **)

    | (ObjT (reason_o, {
      flags;
      props_tmap = mapr;
      proto_t = proto;
      dict_t;
    }),
       SetT(reason_op,x,tin))
      ->
      if flags.frozen then
        prerr_flow cx trace "Mutation not allowed on" l u
      else
        let strict = mk_strict
          flags.sealed (dict_t <> None) reason_o reason_op in
        let t = ensure_prop_ cx trace strict mapr x proto reason_o reason_op in
        dictionary cx trace (string_key x reason_op) t dict_t;
        rec_flow cx trace (tin,t)

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | (ObjT (_, {proto_t = proto; _}), GetT (_, "__proto__", t)) ->

      rec_flow cx trace (proto,t)

    | (ObjT _, GetT(reason_op,"constructor",tout)) ->
      rec_flow cx trace (AnyT.why reason_op, tout)

    | (ObjT (reason_o, {
      flags;
      props_tmap = mapr;
      proto_t = proto;
      dict_t;
    }),
       GetT(reason_op,x,tout))
      ->
      let strict = mk_strict flags.sealed (dict_t <> None) reason_o reason_op in
      let t = ensure_prop cx strict mapr x proto reason_o reason_op trace in
      dictionary cx trace (string_key x reason_op) t dict_t;
      (* move property type to read site *)
      let t = repos_t_from_reason reason_op t in
      rec_flow cx trace (t, tout)

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | (ObjT _, MethodT(_, "constructor", _, _, _, _)) -> ()

    | (ObjT (reason_o, {
      flags;
      props_tmap = mapr;
      proto_t = proto;
      dict_t;
    }),
       MethodT(reason_op,x,this,tins,tout,j))
      ->
      let strict = mk_strict flags.sealed (dict_t <> None) reason_o reason_op in
      let t = ensure_prop cx strict mapr x proto reason_o reason_op trace in
      dictionary cx trace (string_key x reason_op) t dict_t;
      let callt = CallT (reason_op, mk_methodtype2 this tins None tout j) in
      rec_flow cx trace (t, callt)

    (******************************************)
    (* strings may have their characters read *)
    (******************************************)

    | (StrT (reason_s, _), GetElemT(reason_op,index,tout)) ->
      rec_flow cx trace (index, NumT.why reason_s);
      rec_flow cx trace (StrT.why reason_op, tout)

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
      rec_flow cx trace (key, ElemT(reason_op, l, UpperBoundT tin))

    | (ObjT _, GetElemT(reason_op,key,tout))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, LowerBoundT tout))

    | (ArrT (_, _, []), SetElemT(reason_op, key,tin))
      ->
      let num = NumT.why reason_op in
      rec_flow cx trace (num, ElemT(reason_op, l, UpperBoundT tin));
      rec_flow cx trace (key, num)

    | (ArrT _, SetElemT(reason_op, key,tin))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, UpperBoundT tin))

    | (ArrT (_, _, []), GetElemT(reason_op, key,tout))
      ->
      let num = NumT.why reason_op in
      rec_flow cx trace (num, ElemT(reason_op, l, LowerBoundT tout));
      rec_flow cx trace (key, num)

    | (ArrT _, GetElemT(reason_op, key,tout))
      ->
      rec_flow cx trace (key, ElemT(reason_op, l, LowerBoundT tout))

    | (StrT (reason_s, literal),
       ElemT(reason_op, (ObjT(_, {dict_t; _}) as o), t))
      ->
      (match literal with
      | Some x -> (match t with
        | UpperBoundT tin -> rec_flow cx trace (o, SetT(reason_op,x,tin))
        | LowerBoundT tout -> rec_flow cx trace (o, GetT(reason_op,x,tout))
        | _ -> assert false)
      | None -> (match dict_t with
        | None -> ()
        | Some { key; value; _ } ->
            rec_flow cx trace (l, key);
            rec_flow cx trace (value,t);
            rec_flow cx trace (t,value))
      )

    | (_, ElemT(_, ObjT(_, {dict_t; _}), t))
      ->
        (match dict_t with
        | None -> ()
        | Some { key; value; _ } ->
            rec_flow cx trace (l, key);
            rec_flow cx trace (value,t);
            rec_flow cx trace (t,value))

    | (NumT (reason_i, literal),
       ElemT(reason_op, ArrT(_, value, ts), t))
      ->
      let value =
        try
          unsafe_opt literal
          |> float_of_string
          |> int_of_float
          |> List.nth ts
        with _ ->
          value
      in
      (match t with
      | UpperBoundT tin -> rec_flow cx trace (tin, value)
      | LowerBoundT tout -> rec_flow cx trace (value, tout)
      | _ -> assert false)

    | (ArrT _, GetT(reason_op,"constructor",tout)) ->
      rec_flow cx trace (AnyT.why reason_op, tout)

    | (ArrT _,
       (SetT(_,"constructor",_) | MethodT(_,"constructor",_,_,_,_))) -> ()

    (***********************************************)
    (* functions may have their prototypes written *)
    (***********************************************)

    | (FunT (reason_f,_,t,_),
       SetT(reason_op,"prototype",tin))
      ->
      rec_flow cx trace (tin, ObjAssignT(reason_op, t, AnyT.t, [], false))

    (*********************************)
    (* ... and their prototypes read *)
    (*********************************)

    | (FunT (reason_f,_,t,_),
       GetT(reason_op,"prototype",tout))
      ->
      rec_flow cx trace (t,tout)

    | (ClassT (instance),
       GetT(reason_op,"prototype",tout))
      ->
      rec_flow cx trace (instance, tout)

    (***************************************************************)
    (* functions may be called by passing a receiver and arguments *)
    (***************************************************************)

    | (FunT _, MethodT(reason_op,"call",_,o2::tins2,tout2,_))
      -> (* TODO: closure *)

      let funtype = mk_methodtype o2 tins2 None tout2 in
      rec_flow cx trace (l, CallT (prefix_reason "call " reason_op, funtype))

    (*******************************************)
    (* ... or a receiver and an argument array *)
    (*******************************************)

    | (FunT _, MethodT(reason_op,"apply",_,[o2;tinsArr2],tout2,_))
      -> (* TODO: closure *)

      let reason = replace_reason "element of arguments" reason_op in
      let elem = mk_tvar cx reason in

      rec_flow cx trace (tinsArr2, ArrT(reason,elem,[]));

      let funtype = mk_methodtype o2 [RestT elem] None tout2 in
      rec_flow cx trace (l, CallT(prefix_reason "apply " reason_op, funtype))

    (************************************************************************)
    (* functions may be bound by passing a receiver and (partial) arguments *)
    (************************************************************************)

    | (FunT (reason,_,_,
             {this_t = o1; params_tlist = tins1; return_t = tout1; _}),
       MethodT(reason_op,"bind",_,o2::tins2,tout2,_))
      -> (* TODO: closure *)

      rec_flow cx trace (o2,o1);

        let tins1 = multiflow cx trace (u, l) ~strict:false (tins2,tins1) in

        rec_flow cx trace (
          FunT(reason_op, dummy_static, dummy_prototype,
               mk_functiontype tins1 None tout1),
          tout2)

    (***********************************************)
    (* You can use a function as a callable object *)
    (***********************************************)
    | (FunT (reason, statics, proto, _) , ObjT _) ->
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
        iter_props cx instance.fields_tmap (fun x t ->
          lookup_prop cx trace l reason None x t
        );
        iter_props cx instance.methods_tmap (fun x t ->
          if (x <> "constructor") then
            (* we're able to do supertype compatibility in super methods because
               they're immutable *)
            lookup_prop cx trace l reason None x (UpperBoundT t)
        )

    | (ObjT _,
       SuperT (reason,instance))
      ->
        iter_props cx instance.fields_tmap (fun x t ->
          rec_flow cx trace (l, LookupT(reason,None,x,t))
        );
        iter_props cx instance.methods_tmap (fun x t ->
          if (x <> "constructor") then
            rec_flow cx trace (l, LookupT(reason,None,x,t))
        )

    (********************************)
    (* mixed acts as the root class *)
    (********************************)

    | (MixedT _, SuperT _) -> ()

    (*********************************************************)
    (* addition typechecks iff either of the following hold: *)
    (*                                                       *)
    (* bool|number + bool|number = number                    *)
    (* _ + _ = string                                        *)
    (*********************************************************)

    | ((BoolT _ | NumT _), AdderT(reason,(BoolT _ | NumT _), t)) ->

      rec_flow cx trace (NumT.why reason, t)

    | (StrT _, AdderT(reason,_, t))
    | (_, AdderT(reason,StrT _, t)) ->

      rec_flow cx trace (StrT.why reason, t)

    | (MixedT _, AdderT(reason,_, t))
    | (_, AdderT(reason,MixedT _, t)) ->

      rec_flow cx trace (MixedT.why reason, t)

    | (_, AdderT(reason,((OpenT _ | UnionT _) as tin), tout)) ->

      rec_flow cx trace (tin, AdderT(reason,l , tout))

    | (_, AdderT(reason,_, t)) ->

      rec_flow cx trace (StrT.why reason, t)

    (***********************************************************)
    (* comparison typechecks iff either of the following hold: *)
    (*                                                         *)
    (* number <> number = number                               *)
    (* string <> string = string                               *)
    (***********************************************************)

    | (_, ComparatorT(reason, ((OpenT _ | UnionT _) as u_))) ->
      rec_flow cx trace (u_, ComparatorT (reason, l))

    | (StrT _, ComparatorT(reason,t)) ->

      rec_flow cx trace (t, StrT.why reason)

    | (l, ComparatorT(reason,t)) when numeric l ->

      rec_flow cx trace (t, NumT.why reason)

    (**************************************)
    (* types may be refined by predicates *)
    (**************************************)

    | (_, PredicateT(p,t)) ->
      predicate cx trace t (l,p)

    (***********************************************************************)
    (* types may be compared with unstrict (in)equality iff they intersect *)
    (* (otherwise, unsafe coercions may happen)                            *)
    (* note: any types may be compared with strict (in)equality            *)
    (***********************************************************************)

    | (_, EqT(reason, ((OpenT _ | UnionT _) as u_))) ->

      rec_flow cx trace (u_, EqT(reason, l))

    | (_, EqT(_, u_)) when equatable cx trace (l,u_) -> ()

    (**********************)
    (* Array library call *)
    (**********************)

    | (ArrT (_, t, _), (GetT _ | SetT _ | MethodT _)) ->
      let reason = reason_of_t u in
      let arrt = get_builtin_typeapp cx reason "Array" [t] in
      rec_flow cx trace (arrt, u)

    (***********************)
    (* String library call *)
    (***********************)

    | (StrT (reason, _), (GetT _ | MethodT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "String",u)

    (***********************)
    (* Number library call *)
    (***********************)

    | (NumT (reason, _), (GetT _ | MethodT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Number",u)

    (***********************)
    (* Boolean library call *)
    (***********************)

    | (BoolT (reason, _), (GetT _ | MethodT _)) ->
      rec_flow cx trace (get_builtin_type cx reason "Boolean",u)

    (***************************)
    (* Keys: enums and records *)
    (***************************)

    | (StrT (reason_s, literal), EnumT (reason_op, o)) ->
      (match literal with
      | Some x ->
        let reason_op =
          replace_reason (spf "string literal %s" x) reason_s in
        rec_flow cx trace (o, HasT(reason_op,x))
      | None ->
        prerr_flow cx trace "Expected string literal" l u
      )

    | (EnumT (reason1, o1), _) ->
      rec_flow cx trace (o1, KeyT (reason1, u))

    | (_, RecordT (reason2, o2)) ->
      rec_flow cx trace (o2, KeyT(reason2, EnumT(reason2, l)))

    | (RecordT (reason, o2),
       (HasT(_,x) |
           LookupT(_,_,x,_) | GetT(_,x,_) | SetT(_,x,_) | MethodT(_,x,_,_,_,_)))
      ->
      rec_flow cx trace (o2, HasT(reason_of_t u,x))

    | (ObjT (reason_o, { props_tmap = mapr; _ }), HasT(reason_op, x)) ->
      if has_prop cx mapr x then ()
      else
        prmsg_flow cx
          Errors_js.ERROR
          trace
          "Property not found in"
          (reason_op, reason_o)

    | (InstanceT (reason_o, _, _, instance), HasT(reason_op, x)) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      (match SMap.get x fields with
      | Some tx -> ()
      | None ->
        prmsg_flow cx
          Errors_js.ERROR
          trace
          "Property not found in"
          (reason_op, reason_o)
      )

    | (RecordT (reason, o2), KeyT(reason_, key)) ->
      rec_flow cx trace (o2, KeyT(reason_, key))

    | (ObjT (reason, { props_tmap = mapr; _ }), KeyT(_,key)) ->
      iter_props cx mapr (fun x tv ->
        let t = StrT (reason, Some x) in
        rec_flow cx trace (t, key)
      )

    | (InstanceT (reason, _, _, instance), KeyT(_,key)) ->
      let fields_tmap = find_props cx instance.fields_tmap in
      let methods_tmap = find_props cx instance.methods_tmap in
      let fields = SMap.union fields_tmap methods_tmap in
      fields |> SMap.iter (fun x tv ->
        let t = StrT (reason, Some x) in
        rec_flow cx trace (t, key)
      )

    | (RecordT (reason, o2), (ObjT _ | InstanceT _)) ->
      let tvar = mk_tvar cx reason in
      rec_flow cx trace (u, KeyT(reason, tvar));
      rec_flow cx trace (tvar, EnumT(reason, o2))

    (*********************)
    (* functions statics *)
    (*********************)

    | (FunT (_,static,_,_), _) when object_like_op u ->
      rec_flow cx trace (static, u)

    (*****************)
    (* class statics *)
    (*****************)

    | (ClassT instance, _) when object_like_op u ->
      let reason = reason_of_t u in
      let tvar = mk_tvar cx reason in
      rec_flow cx trace (instance, GetT(reason,"statics",tvar));
      rec_flow cx trace (tvar,u)

    (********)
    (* cast *)
    (********)

    | (ClassT instance, (FunT _ | CallT _))
      ->
      let reason = reason_of_t u in
      let tvar = mk_tvar cx reason in
      rec_flow cx trace (instance, GetT(reason,"statics",tvar));
      rec_flow cx trace (tvar, GetT(reason,"$call",u))

    (************)
    (* indexing *)
    (************)

    | (InstanceT _, GetElemT (reason, i, t))
      ->
      rec_flow cx trace (l, SetT(reason, "$key", i));
        rec_flow cx trace (l, GetT(reason, "$value", t))

    | (InstanceT _, SetElemT (reason, i, t))
      ->
      rec_flow cx trace (l, SetT(reason, "$key", i));
        rec_flow cx trace (l, SetT(reason, "$value", t))

    (***************)
    (* unsupported *)
    (***************)

    | (MixedT _, TypeT _) ->
      prwarn_flow cx trace
        (spf "instance of class accessing unknown property: %s"
           (desc_of_t u))
        u l

    (** Lookups can be strict or non-strict, as denoted by the presence or
        absence of strict_reason in the following two pattern matches.
        Strictness derives from whether the object is sealed and was
        created in the same scope in which the lookup occurs - see
        mk_strict below (TODO rename). The failure of a strict lookup
        to find the desired property causes an error; a non-strict one
        does not.
     *)

    | (MixedT reason, LookupT (reason_op, Some strict_reason, x, _)) ->

      if is_object_prototype_method x
      then
        (** TODO: These properties should go in Object.prototype. Currently we
            model Object.prototype as a MixedT, as an optimization against a
            possible deluge of shadow properties on Object.prototype, since it
            is shared by every object. **)
        rec_flow cx trace (get_builtin_type cx reason "Object", u)
      else if Files_js.is_lib_file_or_flowlib_root (abs_path_of_reason reason)
      then
        let msg =
          if is_internal_module_name x
          then "Required module not found"
          else "Could not resolve name"
        in
        let message_list = [
          reason_op, msg
        ] in
        add_warning cx message_list
      else
        let msg =
          if x = "$call"
          then "Callable signature not found in"
          else if x = "$key" || x = "$value"
          then "Indexable signature not found in"
          else "Property not found in"
        in
        prmsg_flow cx
          Errors_js.ERROR
          trace
          msg
          (reason_op, strict_reason)

    | (MixedT _, ExtendsT (t, tc)) ->
      let msg = "This type is incompatible with" in
      prmsg_flow cx
        Errors_js.ERROR
        trace
        msg
        (reason_of_t t, reason_of_t tc)

    (* LookupT is a non-strict lookup, never fired *)
    | (MixedT _, LookupT _) -> ()

    | _ ->
      prerr_flow cx trace (err_msg l u) l u

  )

and abs_path_of_reason r =
  r |> pos_of_reason |> Pos.filename |> Relative_path.to_absolute

and is_object_prototype_method = function
  | "hasOwnProperty"
  | "propertyIsEnumerable"
  | "toLocaleString"
  | "toString"
  | "valueOf" -> true
  | _ -> false

(* only on use-types *)
and err_operation = function
  | GetT _ -> "Property cannot be accessed on"
  | SetT _ -> "Property cannot be assigned on"
  | MethodT _ -> "Method cannot be called on"
  | CallT _ -> "Function cannot be called on"
  | ConstructorT _ -> "Constructor cannot be called on"
  | GetElemT _ -> "Computed property/element cannot be accessed on"
  | SetElemT _ -> "Computed property/element cannot be assigned on"
  | ElemT (_, ObjT _, LowerBoundT _) -> "Computed property cannot be accessed with"
  | ElemT (_, ArrT _, LowerBoundT _) -> "Element cannot be accessed with"
  | ElemT (_, ObjT _, UpperBoundT _) -> "Computed property cannot be assigned with"
  | ElemT (_, ArrT _, UpperBoundT _) -> "Element cannot be assigned with"
  | ObjAssignT _ -> "Expected object instead of"
  | ObjRestT _ -> "Expected object instead of"
  | ObjSealT _ -> "Expected object instead of"
  | SuperT _ -> "Cannot inherit"
  | EqT (_, t) -> spf "Non-strict equality comparison with %s may involve unintended type conversions of" (desc_of_t t)
  | ComparatorT (_, t) -> spf "Relational comparison with %s may involve unintended type conversions of" (desc_of_t t)
  | SpecializeT _ -> "Expected polymorphic type instead of"
  | LookupT _ -> "Property not found in"
  | KeyT _ -> "Expected object instead of"
  | HasT _ -> "Property not found in"

  (* unreachable use-types and def-types *)
  | t
    -> failwith (spf "err_operation called on %s" (string_of_ctor t))

and err_value = function
  | NullT _ -> " possibly null value"
  | VoidT _ -> " possibly undefined value"
  | MaybeT t -> spf " possibly null or undefined value"
  | t -> ""

and err_msg l u =
  if is_use u
  then spf "%s%s" (err_operation u) (err_value l)
  else "This type is incompatible with"

and ground_subtype = function

  | (NumT _,NumT _)

  | (StrT _,StrT _)

  | (BoolT _, BoolT _)

  | (NullT _, NullT _)

  | (VoidT _, VoidT _)

  | (UndefT _,_)

  | (_,MixedT _)

  | (AnyT _,_)

  | (_,AnyT _)
    -> true

  | _ ->
    false

and numeric = function
  | NumT _ -> true

  | InstanceT (reason, _, _, _) ->
    desc_of_reason reason = "Date"

  | _ -> false

and object_like = function
  | ObjT _ | InstanceT _ | RecordT _ -> true
  | t -> function_like t

and object_like_op = function
  | SetT _ | GetT _ | MethodT _ | LookupT _
  | SuperT _
  | KeyT _
  | ObjAssignT _ | ObjRestT _
  | SetElemT _ | GetElemT _
  | AnyObjT _ -> true
  | _ -> false

and function_like = function
  | ClassT _
  | FunT _ -> true
  | _ -> false

and function_like_op = function
  | CallT _ | TypeT _
  | ConstructorT _
  | AnyFunT _ -> true
  | t -> object_like_op t

and equatable cx trace = function

  | (NumT _,NumT _)

  | (StrT _,StrT _)

  | (BoolT _, BoolT _)

  | (UndefT _,_) | (_, UndefT _)

  | (_,MixedT _) | (MixedT _,_)

  | (AnyT _,_) | (_,AnyT _)

  | (VoidT _,_) | (_, VoidT _)

  | (NullT _,_) | (_, NullT _)
    -> true

  | ((NumT _ | StrT _ | BoolT _), _)
  | (_, (NumT _ | StrT _ | BoolT _))
    -> false

  | _ -> true

(*********************)
(* inheritance utils *)
(*********************)

and mk_nominal cx =
  let nominal = mk_id cx in
  (if modes.verbose then prerr_endlinef
      "NOM %d %s" nominal cx.file);
  nominal

and unify_map cx trace tmap1 tmap2 =
  tmap1 |> SMap.iter (fun x t1 ->
    let t2 = SMap.find_unsafe x tmap2 in
    rec_unify cx trace t1 t2
  )

(* Indicate whether property checking should be strict for a given object and an
   operation on it. Strictness is enforced when the object is not a dictionary,
   and it is sealed (e.g., it is a type annotation) or it and the operation
   originate in different scopes. The enforcement is done via the returned
   "blame token" that is used when looking up properties of objects in the
   prototype chain as part of that operation. *)
and mk_strict sealed is_dict reason_o reason_op =
  if (is_dict || (not sealed && Reason_js.same_scope reason_o reason_op))
  then None
  else Some reason_o

(*****************)
(* substitutions *)
(*****************)

(* need to consider only "def" types *)

(** Substitute bound type variables with associated types in a type. Do not
    force substitution under polymorphic types. This ensures that existential
    type variables under a polymorphic type remain unevaluated until the
    polymorphic type is applied. **)
and subst cx ?(force=true) (map: Type.t SMap.t) t =
  if SMap.is_empty map then t
  else match t with
  | BoundT typeparam ->
    (match SMap.get typeparam.name map with
    | None -> t
    | Some t -> t)

  | ExistsT reason ->
    if force then mk_tvar cx reason
    else t

  | OpenT _
  | NumT _
  | StrT _
  | BoolT _
  | UndefT _
  | NullT _
  | VoidT _
  | MixedT _
  | AnyT _
    ->
    t

  | FunT (reason, static, proto, {
    this_t = this;
    params_tlist = params;
    params_names = names;
    return_t = ret;
    closure_t = j;
  }) ->
    FunT (reason, subst cx ~force map static, subst cx ~force map proto, {
      this_t = subst cx ~force map this;
      params_tlist = List.map (subst cx ~force map) params;
      params_names = names;
      return_t = subst cx ~force map ret;
      closure_t = j;
    })

  | PolyT (xs,t) ->
    let xs, map = List.fold_left (fun (xs, map) typeparam ->
      { typeparam with bound = subst cx ~force map typeparam.bound }::xs,
      SMap.remove typeparam.name map
    ) ([], map) xs in
    PolyT (List.rev xs, subst cx ~force:false map t)

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
    TypeT (reason,
           subst cx ~force map t)

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

  | RestT (t) -> RestT (subst cx ~force map t)

  | OptionalT (t) -> OptionalT (subst cx ~force map t)

  | TypeAppT(c, ts) ->
      let c = subst cx ~force map c in
      let ts = List.map (subst cx ~force map) ts in
      TypeAppT(c, ts)

  | MaybeT(t) ->
    MaybeT(subst cx ~force map t)

  | IntersectionT(reason, ts) ->
    IntersectionT(reason, List.map (subst cx ~force map) ts)

  | UnionT(reason, ts) ->
    UnionT(reason, List.map (subst cx ~force map) ts)

  | UpperBoundT(t) ->
    UpperBoundT(subst cx ~force map t)

  | LowerBoundT(t) ->
    LowerBoundT(subst cx ~force map t)

  | AnyObjT _ -> t
  | AnyFunT _ -> t

  | ShapeT(t) ->
    ShapeT(subst cx ~force map t)

  | DiffT(t1, t2) ->
    DiffT(subst cx ~force map t1, subst cx ~force map t2)

  | EnumT(reason, t) ->
    EnumT(reason, subst cx ~force map t)

  | RecordT(reason, t) ->
    RecordT(reason, subst cx ~force map t)

  | ObjAssignT(reason, t1, t2, xs, resolve) ->
    ObjAssignT(reason, subst cx ~force map t1, subst cx ~force map t2, xs, resolve)

  | _ -> assert false (** TODO **)

and subst_propmap cx force map id =
  let pmap = find_props cx id in
  let pmap_ = SMap.map (subst cx ~force map) pmap in
  if pmap_ = pmap then id
  else mk_propmap cx pmap_

and instantiate_poly_ cx trace reason_ (xs,t) ts =
  let len_xs = List.length xs in
  if len_xs <> List.length ts
  then
    let msg = spf "wrong number of type arguments (expected %d)" len_xs in
    add_error cx [reason_, msg];
    AnyT reason_
  else
    let map =
      List.fold_left2
        (fun map {reason; name; bound} t ->
          rec_flow cx trace (t, subst cx map bound);
          SMap.add name t map
        )
        SMap.empty xs ts
    in
    subst cx map t

and instantiate_poly cx trace reason_ (xs,t) =
  let ts = xs |> List.map (fun {reason=reason_id; _} ->
    let reason = prefix_reason (
      spf "type parameter %s of " (desc_of_reason reason_id)
    ) reason_ in
    mk_tvar cx reason
  )
  in
  instantiate_poly_ cx trace reason_ (xs,t) ts

and mk_object_with_proto cx reason proto =
  mk_object_with_map_proto cx reason SMap.empty proto

and mk_object_with_map_proto cx reason ?(sealed=false) map proto =
  let flags = { default_flags with sealed } in
  let dict = None in
  let pmap = mk_propmap cx map in
  ObjT (reason, mk_objecttype ~flags dict pmap proto)

(* Speculatively match types, returning whether the match fails. *)
and speculative_flow_error cx trace l u =
  throw_on_error := true;
  let result =
    try rec_flow cx trace (l, u); false
    with
    | FlowError msgs -> true
    | exn -> raise exn
  in
  throw_on_error := false;
  result

(* try each branch of a union in turn *)
and try_union cx trace l reason = function
  | [] -> (* fail: bottom is the unit of unions *)
    rec_flow cx trace (l, UndefT reason)
  | t::ts ->
    rec_flow cx trace
      (SpeculativeMatchFailureT(reason, l, UnionT(reason, ts)),
       t)

(* try each branch of an intersection in turn *)
and try_intersection cx trace u reason = function
  | [] -> (* fail: top is the unit of intersections *)
    rec_flow cx trace (MixedT reason, u)
  | t::ts ->
    rec_flow cx trace
      (t,
       SpeculativeMatchFailureT(reason, IntersectionT(reason, ts), u))

(* Some types need their parts to be concretized (i.e., type variables may need
   to be replaced by concrete types) so that speculation has a chance to fail
   early (and other branches are tried): otherwise, those failures may remain
   latent, and cause spurious errors to be reported. *)

(** TODO: Concretization is a general pattern that could be useful elsewhere as
    well. For example, we often have constraints that model "binary" operations,
    where both arguments need to be concretized for an operation to proceed
    (e.g., see rules involving AdderT, ComparatorT, ObjAssignT); in those cases,
    we effectively concretize the first argument, then the second argument by
    pushing the concretized first argument back into the constraint, and
    signaling that we are done afterwards. A general solution would have to do
    the following: specify parts of a constraint to be concretized, concretize
    those parts, replace the constraint with the concrete parts, and signal that
    we are done. The specification and replacement can be carried out generically
    using type substitution (see rules involving SpecializeT). The signaling
    could be done either by wrapping with ConcreteT, or by generically checking
    that the specified parts to be concretized have indeed been replaced with
    concrete parts. **)

(* The set of type patterns that currently need to be concretized appear
   below. This set needs to be kept in sync across parts_to_concretize and
   replace_with_concrete parts. *)

and parts_to_concretize cx = function
  (* call of overloaded function *)
  | CallT (reason, callt) -> callt.params_tlist
  (* selection of overloaded function *)
  | FunT (reason, _, _, callt) -> callt.params_tlist
  | u -> []

and replace_with_concrete_parts ts = function
  | CallT (reason, callt) ->
      CallT (reason, { callt with params_tlist = ts })
  | FunT (reason, static, prototype, callt) ->
      FunT (reason, static, prototype, { callt with params_tlist = ts })
  | u -> u

(* Take a todo_list of types and recursively concretize them, moving the
   concretized types to a done_list. Types in the todo_list, e.g. t, may be type
   variables, and here we simply set up the requirement that they be
   concretized; when they hit the rule for ConcretizeT, they are concrete, and
   therefore ready to be moved to the done_list. *)
and concretize_parts cx trace l u done_list = function
  | [] ->
      (* items were moved from todo_list to done_list in LIFO order *)
      let done_list = List.rev done_list in
      rec_flow cx trace
        (l, ConcreteT(replace_with_concrete_parts done_list u))
  | t::todo_list ->
      rec_flow cx trace
        (t, ConcretizeT(l, todo_list, done_list, u))

(* property lookup functions in objects and instances *)

and ensure_prop cx strict mapr x proto reason_obj reason_op trace =
  if has_prop cx mapr x
  then read_prop cx mapr x
  else
    let t =
      if has_prop cx mapr (internal_name x)
      then read_prop cx mapr (internal_name x)
      else intro_prop cx reason_obj x mapr
    in
    t |> recurse_proto cx strict proto reason_op x trace

and ensure_prop_ cx trace strict mapr x proto reason_obj reason_op =
  if has_prop cx mapr x
  then read_prop cx mapr x
  else
    match strict with
    | Some reason_o ->
      prmsg_flow cx
        Errors_js.ERROR
        trace
        "Property not found in"
        (reason_op, reason_o);
      MixedT.t
    | None ->
      let t =
        if has_prop cx mapr (internal_name x)
        then read_prop_ cx mapr (internal_name x) |> (fun t ->
          write_prop cx mapr x t; t
        )
        else intro_prop_ cx reason_obj x mapr
      in
      t |> recurse_proto cx None proto reason_op x trace

and lookup_prop cx trace l reason strict x t =
  let l =
    (* munge names beginning with single _ *)
    if (Str.string_match (Str.regexp_string "_") x 0) &&
      not (Str.string_match (Str.regexp_string "__") x 0)
    then MixedT (reason_of_t l)
    else l
  in
  rec_flow cx trace (l, LookupT (reason, strict, x, t))

and get_prop cx trace reason_op strict super x map tout =
  if SMap.mem x map
  then
    rec_flow cx trace (SMap.find_unsafe x map, tout)
  else
    lookup_prop cx trace super reason_op strict x (LowerBoundT tout)

and set_prop cx trace reason_op reason_c super x map tin =
  let map = find_props cx map in
  if SMap.mem x map
  then
    rec_flow cx trace (tin, SMap.find_unsafe x map)
  else
    lookup_prop cx trace super reason_op (Some reason_c) x (UpperBoundT tin)

and intro_prop cx reason_obj x mapr =
  let reason_prop = prefix_reason (spf ".%s of " x) reason_obj in
  mk_tvar_where cx reason_prop (fun tvar ->
    write_prop cx mapr (internal_name x) tvar
  )

and intro_prop_ cx reason_obj x mapr =
  let reason_prop = prefix_reason (spf ".%s of " x) reason_obj in
  mk_tvar_where cx reason_prop (fun tvar ->
    write_prop cx mapr x tvar
  )

and recurse_proto cx strict proto reason_op x trace t =
  rec_flow cx trace (proto, LookupT(reason_op,strict,x,t));
  t

(* other utils *)

and filter cx trace t l pred =
  if (pred l) then rec_flow cx trace (l,t)

and is_string = function StrT _ -> true | _ -> false
and is_number = function NumT _ -> true | _ -> false
and is_function = function FunT _ -> true | _ -> false
and is_object = function (ObjT _ | ArrT _ | NullT _) -> true | _ -> false
and is_array = function ArrT _ -> true | _ -> false
and is_bool = function BoolT _ -> true | _ -> false

and not_ pred x = not(pred x)

and recurse_into_union filter_fn (r, ts) =
  let new_ts = ts |> List.filter (fun t ->
    match filter_fn t with
    | UndefT _ -> false
    | _ -> true
  ) in
  match new_ts with
  | [] -> UndefT r
  | [t] -> t
  | _ -> UnionT (r, new_ts)

and filter_exists = function
  (* falsy things get removed *)
  | NullT r
  | VoidT r
  | BoolT (r, Some false)
  | StrT (r, Some "")
  | NumT (r, Some "0") -> UndefT r

  (* unknown things become truthy *)
  | MaybeT t -> t
  | OptionalT t -> filter_exists t
  | BoolT (r, None) -> BoolT (r, Some true)
  | StrT (r, None) -> StrT (r, Some "truthy") (* hmmmm *)
  | NumT (r, None) -> NumT (r, Some "truthy") (* hmmmm *)

  (* truthy things pass through *)
  | t -> t

and filter_not_exists t = match t with
  (* falsy things pass through *)
  | NullT _
  | VoidT _
  | BoolT (_, Some false)
  | StrT (_, Some "")
  | NumT (_, Some "0") -> t

  (* truthy things get removed *)
  | BoolT (r, Some _)
  | StrT (r, Some _)
  | NumT (r, Some _) -> UndefT r

  (* unknown boolies become falsy *)
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [NullT.why reason; VoidT.why reason])
  | BoolT (r, None) -> BoolT (r, Some false)
  | StrT (r, None) -> StrT (r, Some "")
  | NumT (r, None) -> NumT (r, Some "0")

  (* things that don't track truthiness pass through *)
  | t -> t

and filter_maybe = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [NullT.why reason; VoidT.why reason])
  | MixedT r -> UnionT (r, [NullT.why r; VoidT.why r])
  | NullT r -> NullT r
  | VoidT r -> VoidT r
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | t -> UndefT.t

and filter_not_maybe = function
  | MaybeT t -> t
  | OptionalT t -> filter_not_maybe t
  | NullT r | VoidT r -> UndefT r
  | t -> t

and filter_null = function
  | OptionalT (MaybeT t)
  | MaybeT t -> NullT.why (reason_of_t t)
  | NullT r -> NullT r
  | MixedT r -> NullT.why r
  | t -> UndefT.t

and filter_not_null = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [VoidT.why reason; t])
  | OptionalT t -> OptionalT (filter_not_null t)
  | UnionT (r, ts) -> recurse_into_union filter_not_null (r, ts)
  | NullT r -> UndefT r
  | t -> t

and filter_undefined = function
  | MaybeT t -> VoidT.why (reason_of_t t)
  | VoidT r -> VoidT r
  | OptionalT t ->
    let reason = reason_of_t t in
    VoidT.why reason
  | MixedT r -> VoidT.why r
  | t -> UndefT.t

and filter_not_undefined = function
  | MaybeT t ->
    let reason = reason_of_t t in
    UnionT (reason, [NullT.why reason; t])
  | OptionalT t -> filter_not_undefined t
  | UnionT (r, ts) -> recurse_into_union filter_not_undefined (r, ts)
  | VoidT r -> UndefT r
  | t -> t

and filter_true = function
  | BoolT (r, Some true)
  | BoolT (r, None) -> BoolT (r, Some true)
  | MixedT r -> BoolT (replace_reason "boolean" r, Some true)
  | t -> UndefT (reason_of_t t)

and filter_not_true = function
  | BoolT (r, Some true) -> UndefT r
  | BoolT (r, None) -> BoolT (r, Some false)
  | t -> t

and filter_false = function
  | BoolT (r, Some false)
  | BoolT (r, None) -> BoolT (r, Some false)
  | MixedT r -> BoolT (replace_reason "boolean" r, Some false)
  | t -> UndefT (reason_of_t t)

and filter_not_false = function
  | BoolT (r, Some false) -> UndefT r
  | BoolT (r, None) -> BoolT (r, Some true)
  | t -> t

and predicate cx trace t (l,p) = match (l,p) with

  (************************)
  (* deconstruction of && *)
  (************************)

  | (_, AndP(p1,p2)) ->
    let reason = new_reason "and" (pos_of_predicate p1) in
    let tvar = mk_tvar cx reason in
    rec_flow cx trace (l,PredicateT(p1,tvar));
    rec_flow cx trace (tvar,PredicateT(p2,t))

  (************************)
  (* deconstruction of || *)
  (************************)

  | (_, OrP(p1,p2)) ->
    rec_flow cx trace (l,PredicateT(p1,t));
    rec_flow cx trace (l,PredicateT(p2,t))

  (***********************)
  (* typeof _ ~ "boolean" *)
  (***********************)

  | (MixedT r, IsP "boolean") ->
    rec_flow cx trace (BoolT.why r, t)

  | (_, IsP "boolean") ->
    filter cx trace t l is_bool

  | (_, NotP(IsP "boolean")) ->
    filter cx trace t l (not_ is_bool)

  (***********************)
  (* typeof _ ~ "string" *)
  (***********************)

  | (MixedT r, IsP "string") ->
    rec_flow cx trace (StrT.why r, t)

  | (_, IsP "string") ->
    filter cx trace t l is_string

  | (_, NotP(IsP "string")) ->
    filter cx trace t l (not_ is_string)

  (***********************)
  (* typeof _ ~ "number" *)
  (***********************)

  | (MixedT r, IsP "number") ->
    rec_flow cx trace (NumT.why r, t)

  | (_, IsP "number") ->
    filter cx trace t l is_number

  | (_, NotP(IsP "number")) ->
    filter cx trace t l (not_ is_number)

  (***********************)
  (* typeof _ ~ "function" *)
  (***********************)

  | (MixedT r, IsP "function") ->
    rec_flow cx trace (AnyFunT (replace_reason "function" r), t)

  | (_, IsP "function") ->
    filter cx trace t l is_function

  | (_, NotP(IsP "function")) ->
    filter cx trace t l (not_ is_function)

  (***********************)
  (* typeof _ ~ "object" *)
  (***********************)

  | (MixedT r, IsP "object") ->
    let obj = AnyObjT (replace_reason "object" r) in
    let union = create_union [NullT.why r; obj] in
    rec_flow cx trace (union, t)

  | (_, IsP "object") ->
    filter cx trace t l is_object

  | (_, NotP(IsP "object")) ->
    filter cx trace t l (not_ is_object)

  (*******************)
  (* Array.isArray _ *)
  (*******************)

  | (MixedT r, IsP "array") ->
    let filtered_l = ArrT (replace_reason "array" r, AnyT.why r, []) in
    rec_flow cx trace (filtered_l, t)

  | (_, IsP "array") ->
    filter cx trace t l is_array

  | (_, NotP(IsP "array")) ->
    filter cx trace t l (not_ is_array)

  (***********************)
  (* typeof _ ~ "undefined" *)
  (***********************)

  | (_, IsP "undefined") ->
    rec_flow cx trace (filter_undefined l, t)

  | (_, NotP(IsP "undefined")) ->
    rec_flow cx trace (filter_not_undefined l, t)

  (********)
  (* null *)
  (********)

  | (_, IsP "null") ->
    rec_flow cx trace (filter_null l, t)

  | (_, NotP(IsP "null")) ->
    rec_flow cx trace (filter_not_null l, t)

  (*********)
  (* maybe *)
  (*********)

  | (_, IsP "null or undefined") ->
    rec_flow cx trace (filter_maybe l, t)

  | (_, NotP(IsP "null or undefined")) ->
    rec_flow cx trace (filter_not_maybe l, t)

  (********)
  (* true *)
  (********)

  | (_, IsP "true") ->
    rec_flow cx trace (filter_true l, t)

  | (_, NotP(IsP "true")) ->
    rec_flow cx trace (filter_not_true l, t)

  (*********)
  (* false *)
  (*********)

  | (_, IsP "false") ->
    rec_flow cx trace (filter_false l, t)

  | (_, NotP(IsP "false")) ->
    rec_flow cx trace (filter_not_false l, t)

  (************************)
  (* truthyness *)
  (************************)

  | (_, ExistsP) ->
    rec_flow cx trace (filter_exists l, t)

  | (_, NotP(ExistsP)) ->
    rec_flow cx trace (filter_not_exists l, t)

  (*************************************************************)
  (* instanceof: resolve the constructor, pushing the instance *)
  (*************************************************************)

  | (_, InstanceofP (c)) ->
    rec_flow cx trace (c, PredicateT(ConstructorP(l),t))

  | (_, NotP(InstanceofP (c))) ->
    rec_flow cx trace (c, PredicateT(NotP(ConstructorP(l)),t))

  (*************************************************************)
  (* ... and refine the instance with the resolved constructor *)
  (* (careful: this is backwards)                              *)
  (*************************************************************)

  (** An object is considered `instanceof` a function F when it is constructed
      by F. Note that this is incomplete with respect to the runtime semantics,
      where instanceof is transitive: if F.prototype `instanceof` G, then the
      object is `instanceof` G. There is nothing fundamentally difficult in
      modeling the complete semantics, but we haven't found a need to do it. **)
  | (FunT (_,_,proto1,_),
     ConstructorP (ObjT (_,{proto_t = proto2; _}) as u))
      when proto1 = proto2 ->

    rec_flow cx trace (u,t)

  (** Suppose that we have an instance x of class C, and we check whether x is
      `instanceof` class A. To decide what the appropriate refinement for x
      should be, we need to decide whether C extends A, choosing either C or A
      based on the result. Thus, we generate a constraint to decide whether C
      extends A (while remembering C), which may recursively generate further
      constraints to decide super(C) extends A, and so on, until we hit the root
      class. (As a technical tool, we use Extends(_, _) to perform this
      recursion; it is also used elsewhere for running similar recursive
      subclass decisions.) **)
  | (ClassT(InstanceT _ as a),
     ConstructorP (InstanceT _ as c)) ->

    predicate cx trace t (ClassT(ExtendsT(c, a)), p)

  (** If C is a subclass of A, then don't refine the type of x. Otherwise,
      refine the type of x to A. (In general, the type of x should be refined to
      C & A, but that's hard to compute.) **)
  | (ClassT(ExtendsT(c, InstanceT (_,_,_,instance_a))),
     ConstructorP (InstanceT (_,_,super_c,instance_c)))
    -> (* TODO: intersection *)

    if instance_a.class_id = instance_c.class_id
    then rec_flow cx trace (c, t)
    else
      (** Recursively check whether super(C) extends A, with enough context. **)
      rec_flow cx trace (super_c, PredicateT(InstanceofP(l), t))

  | (ClassT(ExtendsT (_, a)),
     ConstructorP (MixedT _))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow cx trace (a, t)

  (** Prune the type when any other `instanceof` check succeeds (since this is
      impossible). *)
  | (_, ConstructorP _) ->
    ()

  | (FunT (_,_,proto1,_),
     NotP(ConstructorP (ObjT (_,{proto_t = proto2; _}))))
      when proto1 = proto2 ->
    ()

  (** Like above, now suppose that we have an instance x of class C, and we
      check whether x is _not_ `instanceof` class A. To decide what the
      appropriate refinement for x should be, we need to decide whether C
      extends A, choosing either nothing or C based on the result. **)
  | (ClassT(InstanceT _ as a),
     NotP(ConstructorP (InstanceT _ as c))) ->

    predicate cx trace t (ClassT(ExtendsT(c, a)), p)

  (** If C is a subclass of A, then do nothing, since this check cannot
      succeed. Otherwise, don't refine the type of x. **)
  | (ClassT(ExtendsT(c, InstanceT (_,_,_,instance_a))),
     NotP(ConstructorP (InstanceT (_,_,super_c,instance_c))))
    ->

    if instance_a.class_id = instance_c.class_id
    then ()
    else rec_flow cx trace (super_c, PredicateT(NotP(InstanceofP(l)), t))

  | (ClassT(ExtendsT(c, _)),
     NotP(ConstructorP (MixedT _)))
    ->
    (** We hit the root class, so C is not a subclass of A **)
    rec_flow cx trace (c, t)

  (** Don't refine the type when any other `instanceof` check fails. **)
  | (_, NotP(ConstructorP u)) ->
    rec_flow cx trace (u,t)

  (* unknown predicate *)
  | _ ->
    prerr_flow cx trace "Unsatisfied predicate" l (PredicateT(p,t))


(***************)
(* unification *)
(***************)

(* Ensure that a type variable is, or is converted to, a unifier. *)
and ensure_unifier cx id =
  let bounds = IMap.find_unsafe id cx.graph in
  match bounds.unifier with
  | None -> bounds.unifier <- Some (Rank 0)
  | _ -> ()

(* Clear the bounds of a type variable. Either it is a unifier that is no longer
   a root, wherein the solution is None. Or it is a root that is resolved to a
   type t, wherein the solution is Some t. *)
and clear cx ?solution bounds =
  bounds.cleared <- true;
  bounds.lowertvars <- IMap.empty;
  bounds.uppertvars <- IMap.empty;
  match solution with
  | None ->
    bounds.solution <- None;
    bounds.lower <- TypeMap.empty;
    bounds.upper <- TypeMap.empty;
  | Some t ->
    bounds.solution <- None;
    bounds.lower <- TypeMap.singleton t (unit_trace t t);
    bounds.upper <- TypeMap.singleton t (unit_trace t t)

and rank = function
  | Some (Rank r) -> r
  | _ -> assert false

(* Chain a root to another root. If both roots are unresolved, this amounts to
   copying over the bounds of one root to another, and adding all the
   connections necessary when two non-unifiers flow to each other. On the other
   hand, when one or both roots are resolved, we can assume that their bounds are
   cleared. *)
and goto cx trace ?(flag=false) id1 id2 =
  let bounds1, bounds2 =
    IMap.find_unsafe id1 cx.graph, IMap.find_unsafe id2 cx.graph
  in
  bounds1.unifier <- Some (Goto id2);
  if flag then bounds2.unifier <- Some (Rank (1 + (rank bounds2.unifier)));
  (match bounds1.solution, bounds2.solution with

  | None, None ->
    bounds1.lower |> TypeMap.iter (fun l trace_l ->
      bounds2.upper |> TypeMap.iter (fun u trace_u ->
        join_flow cx [trace_l;trace;trace_u] (l,u)
      ));
    bounds2.lower |> TypeMap.iter (fun l trace_l ->
      bounds1.upper |> TypeMap.iter (fun u trace_u ->
        join_flow cx [trace_l;trace;trace_u] (l,u)
      ));
    bounds1.lowertvars |> IMap.iter (fun idl trace_l ->
      let f = (fun trace_u -> concat_trace[trace_l;trace;trace_u]) in
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- UnionTypeMap.union f bounds2.upper bounds_l.upper;
      bounds_l.uppertvars <- UnionIMap.union f bounds2.uppertvars bounds_l.uppertvars
    );
    bounds2.lowertvars |> IMap.iter (fun idl trace_l ->
      let f = (fun trace_u -> concat_trace[trace_l;trace;trace_u]) in
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- UnionTypeMap.union f bounds1.upper bounds_l.upper;
      bounds_l.uppertvars <- UnionIMap.union f bounds1.uppertvars bounds_l.uppertvars
    );
    bounds1.uppertvars |> IMap.iter (fun idu trace_u ->
      let f = (fun trace_l -> concat_trace[trace_l;trace;trace_u]) in
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- UnionTypeMap.union f bounds2.lower bounds_u.lower;
      bounds_u.lowertvars <- UnionIMap.union f bounds2.lowertvars bounds_u.lowertvars
    );
    bounds2.uppertvars |> IMap.iter (fun idu trace_u ->
      let f = (fun trace_l -> concat_trace[trace_l;trace;trace_u]) in
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- UnionTypeMap.union f bounds1.lower bounds_u.lower;
      bounds_u.lowertvars <- UnionIMap.union f bounds1.lowertvars bounds_u.lowertvars
    );
    (* TODO: traces *)
    bounds2.lower <- TypeMap.union bounds1.lower bounds2.lower;
    bounds2.upper <- TypeMap.union bounds1.upper bounds2.upper;
    bounds2.lowertvars <- IMap.union bounds1.lowertvars bounds2.lowertvars;
    bounds2.uppertvars <- IMap.union bounds1.uppertvars bounds2.uppertvars

  | None, Some t2 ->
    bounds1.lower |> TypeMap.iter (fun l trace_l ->
      join_flow cx [trace_l;trace] (l,t2)
    );
    bounds1.upper |> TypeMap.iter (fun u trace_u ->
      join_flow cx [trace;trace_u] (t2,u)
    );
    bounds1.lowertvars |> IMap.iter (fun idl trace_l ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.add t2 (concat_trace[trace_l;trace]) bounds_l.upper
    );
    bounds1.uppertvars |> IMap.iter (fun idu trace_u ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.add t2 (concat_trace[trace;trace_u]) bounds_u.lower
    )

  | Some t1, None ->
    bounds2.lower |> TypeMap.iter (fun l trace_l ->
      join_flow cx [trace_l;trace] (l,t1)
    );
    bounds2.upper |> TypeMap.iter (fun u trace_u ->
      join_flow cx [trace;trace_u] (t1,u)
    );
    bounds2.lowertvars |> IMap.iter (fun idl trace_l ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.add t1 (concat_trace[trace_l;trace]) bounds_l.upper
    );
    bounds2.uppertvars |> IMap.iter (fun idu trace_u ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.add t1 (concat_trace[trace;trace_u]) bounds_u.lower
    );
    clear cx ~solution:t1 bounds2

  | Some t1, Some t2 ->
    rec_unify cx trace t1 t2;
  );
  clear cx bounds1

(* Unify two type variables. This involves ensuring that they are unifiers,
   finding their roots, and making one point to the other. Ranks are used to
   keep chains short. *)
and merge_ids cx trace id1 id2 =
  ensure_unifier cx id1; ensure_unifier cx id2;
  let (id1, bounds1), (id2, bounds2) = find_root cx id1, find_root cx id2 in
  let r1, r2 = rank bounds1.unifier, rank bounds2.unifier in
  if id1 = id2 then ()
  else if r1 < r2 then goto cx trace id1 id2
  else if r2 < r1 then goto cx trace id2 id1
  else goto cx trace ~flag:true id1 id2

(* Resolve a type variable to a type. This involves ensuring that it is a
   unifier, finding its root, and resolving to that type. *)
and resolve_id cx trace id t =
  ensure_unifier cx id;
  let id, _ = find_root cx id in
  let bounds = IMap.find_unsafe id cx.graph in
  match bounds.solution with
  | None ->
    bounds.lower |> TypeMap.iter (fun l trace_l ->
      join_flow cx [trace_l;trace] (l,t)
    );
    bounds.upper |> TypeMap.iter (fun u trace_u ->
      join_flow cx [trace;trace_u] (t,u)
    );
    bounds.lowertvars |> IMap.iter (fun idl trace_l ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.add t (concat_trace[trace_l;trace]) bounds_l.upper
    );
    bounds.uppertvars |> IMap.iter (fun idu trace_u ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.add t (concat_trace[trace;trace_u]) bounds_u.lower
    );
    clear cx ~solution:t bounds
  | Some t_ ->
    rec_unify cx trace t_ t

(******************)

(* TODO: Unification between concrete types is still implemented as
   bidirectional flows. This means that the destructuring work is duplicated,
   and we're missing some opportunities for nested unification. *)
and naive_unify cx trace t1 t2 =
  rec_flow cx trace (t1,t2); rec_flow cx trace (t2,t1)

(* Unification of two types *)
and rec_unify cx trace t1 t2 =
  match (t1,t2) with
  | (OpenT (_,id1), OpenT (_,id2)) ->
    merge_ids cx trace id1 id2

  | (OpenT (_, id), t) | (t, OpenT (_, id)) ->
    resolve_id cx trace id t

  | (ArrT (_, t1, ts1), ArrT (_, t2, ts2)) ->
    array_unify cx trace (ts1,t1, ts2,t2)

  | (ObjT (reason1, {props_tmap = flds1; _}),
     ObjT (reason2, {props_tmap = flds2; _})) ->
    let pmap1, pmap2 =
      find_props cx flds1,
      find_props cx flds2
    in
    let error_ref = ref false in
    SMap.iter (fun x t ->
      match SMap.get x pmap2 with
      | Some t_ -> rec_unify cx trace t t_
      | None -> error_ref := true
    ) pmap1;
    if !error_ref || (SMap.cardinal pmap2 > SMap.cardinal pmap1)
    then
      let message_list = [
        reason1, "Objects do not have the same properties";
        reason2, ""
      ] in
      add_warning cx message_list

  | _ ->
    naive_unify cx trace t1 t2

(* mutable sites on parent values (i.e. object properties,
   array elements) must be typed invariantly when a value
   flows to the parent, unless the incoming value is fresh,
   in which case covariant typing is sound (since no alias
   will break if the subtyped child value is replaced by a
   non-subtyped value *)
and flow_to_mutable_child cx trace fresh t1 t2 =
  if fresh
  then rec_flow cx trace (t1, t2)
  else rec_unify cx trace t1 t2

and array_flow cx trace lit = function
  | ([],e1, _,e2) -> (* general element1 = general element2 *)
    flow_to_mutable_child cx trace lit e1 e2

  | (_,e1, [],e2) -> (* specific element1 < general element2 *)
    rec_flow cx trace (e1, e2)

  | ([t1],_, t2::_,_) -> (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace lit t1 t2

  | (t1::ts1,e1, t2::ts2,e2) -> (* specific element1 = specific element2 *)
    flow_to_mutable_child cx trace lit t1 t2;
    array_flow cx trace lit (ts1,e1, ts2,e2)

(* array helper *)
and array_unify cx trace = function
  | ([],e1, [],e2) -> (* general element1 = general element2 *)
    rec_unify cx trace e1 e2

  | (ts1,_, [],e2)
  | ([],e2, ts1,_) -> (* specific element1 < general element2 *)
    List.iter (fun t1 ->
      rec_unify cx trace t1 e2;
    ) ts1

  | (t1::ts1,e1, t2::ts2,e2) -> (* specific element1 = specific element2 *)
    rec_unify cx trace t1 t2;
    array_unify cx trace (ts1,e1, ts2,e2)


(*******************************************************************)
(* subtyping a sequence of arguments with a sequence of parameters *)
(*******************************************************************)

(** TODO: add arg number to FunArg trace selectors **)
and multiflow cx trace (l, u) ?(strict=true) (arglist, parlist) =
  multiflow_helper cx trace (l, u) ~strict (arglist, parlist)

and multiflow_helper cx trace (u, l) ~strict = function
  (* Do not complain on too many arguments.
     This pattern is ubiqutous and causes a lot of noise when complained about.
     Note: optional/rest parameters do not provide a workaround in this case.
  *)
  | (_,[]) -> []

  | ([],(OptionalT t)::touts) -> (OptionalT t)::touts

  | ([RestT tin],(OptionalT tout)::touts) ->
    rec_flow cx trace (tin, tout);
    multiflow_helper cx trace (u, l) ~strict ([RestT tin],touts)

  | ((OptionalT tin)::tins,[RestT tout]) ->
    rec_flow cx trace (tin,tout);
    multiflow_helper cx trace (u, l) ~strict (tins,[RestT tout])

  | ((OptionalT tin)::tins,(OptionalT tout)::touts)
  | (tin::tins,(OptionalT tout)::touts) ->
    rec_flow cx trace (tin,tout);
    multiflow_helper cx trace (u, l) ~strict (tins,touts)

  | ([RestT tin],[RestT tout]) ->
    rec_flow cx trace (tin,tout);
    []

  | (tins,[RestT tout]) ->
    tins |> List.iter (fun tin ->
      rec_flow cx trace (tin,tout);
    );
    [RestT tout]

  | ([],ts) ->
    (if strict then
        prerr_flow cx trace
          "Too few arguments (expected default/rest parameters in function)"
          u l
    );
    ts

  | (tin::tins,tout::touts) ->
    rec_flow cx trace (tin,tout);
    multiflow_helper cx trace (u, l) ~strict (tins,touts)

and dictionary cx trace keyt valuet = function
  | None -> ()
  | Some { key; value; _ } ->
      rec_flow cx trace (keyt, key);
      begin match keyt with
      | StrT (_, Some str) ->
        (* Object.prototype methods are exempt from the dictionary rules *)
        if not (is_object_prototype_method str)
        then rec_flow cx trace (valuet, value)
      | _ ->
        rec_flow cx trace (valuet, value)
      end

and string_key s reason =
  let key_reason =
    replace_reason (spf "property name \"%s\" is a string" s) reason in
  StrT (key_reason, Some s)

(* builtins, contd. *)

and get_builtin cx x reason =
  mk_tvar_where cx reason (fun builtin ->
    flow_opt cx (builtins, GetT(reason,x,builtin))
  )

and lookup_builtin cx x reason strict builtin =
  flow_opt cx (builtins, LookupT(reason,strict,x,builtin))

and get_builtin_typeapp cx reason x ts =
  TypeAppT(get_builtin cx x reason, ts)

(* Specialize a polymorphic class, make an instance of the specialized class. *)
and mk_typeapp_instance cx reason c ts =
  let t = mk_tvar cx reason in
  flow_opt cx (c, SpecializeT(reason,ts,t));
  mk_instance cx reason t

and mk_instance cx ?trace instance_reason c =
  mk_tvar_derivable_where cx instance_reason (fun t ->
    flow_opt cx ?trace (c, TypeT(instance_reason,t))
  )

and get_builtin_type cx reason x =
  let t = get_builtin cx x reason in
  mk_instance cx reason t

and instantiate_poly_t cx t types =
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
    assert false

and instantiate_type t =
  match t with
  | ClassT t -> t
  | _ -> AnyT.why (reason_of_t t) (* ideally, assert false *)

and static_method_call cx name reason m argts =
  let cls = get_builtin cx name reason in
  mk_tvar_where cx reason (fun tvar ->
    flow_opt cx (cls, MethodT(reason, m, cls, argts, tvar, 0))
  )

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
  flow_opt cx (builtins, SetT(reason,x,t))

and module_t cx m reason =
  match SMap.get m cx.modulemap with
  | Some t -> t
  | None ->
    mk_tvar_where cx reason (fun t ->
      cx.modulemap <- cx.modulemap |> SMap.add m t;
    )

(* Wrapper functions around __flow that manage traces. Use these functions for
   all recursive calls in the implementation of __flow. *)

(* Call __flow while concatenating traces. Typically this is used in code that
   propagates bounds across type variables, where nothing interesting is going
   on other than concatenating subtraces to make longer traces to describe
   transitive data flows *)
and join_flow cx ts (t1, t2) =
  __flow cx (t1, t2) (concat_trace ts)

(* Call __flow while embedding traces. Typically this is used in code that
   simplifies a constraint to generate subconstraints: the current trace is
   "pushed" when recursing into the subconstraints, so that when we finally hit
   an error and walk back, we can know why the particular constraints that
   caused the immediate error were generated. *)
and rec_flow cx trace (t1, t2) =
  __flow cx (t1, t2) (rec_trace t1 t2 trace)

(* Ideally this function would not be required: either we call `flow` from
   outside without a trace (see below), or we call one of the functions above
   with a trace. However, there are some functions that need to call __flow,
   which are themselves called both from outside and inside (with or without
   traces), so they call this function instead. *)
and flow_opt cx ?trace (t1, t2) =
  match trace with
  | None -> __flow cx (t1, t2) (unit_trace t1 t2)
  | Some trace -> rec_flow cx trace (t1, t2)

(* Externally visible function for subtyping. *)
let flow cx (t1, t2) =
  flow_opt cx (t1, t2)

(* Externally visible function for unification. *)
let unify cx t1 t2 =
  rec_unify cx (unit_trace t1 t2) t1 t2

type gc_state = {
  mutable positive: ISet.t;
  mutable negative: ISet.t;
  mutable objs: ISet.t;
}

let gc_state = {
  positive = ISet.empty;
  negative = ISet.empty;
  objs = ISet.empty;
}

(** TODO: this is out of date!!!!!!!!!!!!!!!!!!!!! **)
let rec gc cx polarity = function

  | OpenT(reason, id) ->
      let bounds = cx.graph |> IMap.find_unsafe id in
      if (polarity)
      then
        if (ISet.mem id gc_state.positive)
        then ()
        else (
          gc_state.positive <- gc_state.positive |> ISet.add id;
          bounds.lower |> TypeMap.iter (fun t _ -> gc cx true t)
        )
      else
        if (ISet.mem id gc_state.negative)
        then ()
        else (
          gc_state.negative <- gc_state.negative |> ISet.add id;
          bounds.upper |> TypeMap.iter (fun t _ -> gc cx false t)
        )

  | FunT(_, static, prototype, funtype) ->
      gc cx (not polarity) funtype.this_t;
      funtype.params_tlist |> List.iter (gc cx (not polarity));
      gc cx polarity funtype.return_t;
      gc_undirected cx prototype;
      gc cx polarity static

  | CallT(_, funtype) ->
      gc cx (not polarity) funtype.this_t;
      funtype.params_tlist |> List.iter (gc cx (not polarity));
      gc cx polarity funtype.return_t

  | ObjT(_, objtype) ->
      let id = objtype.props_tmap in
      gc_state.objs <- gc_state.objs |> ISet.add id;
      iter_props cx id (fun _ -> gc_undirected cx);
      (match objtype.dict_t with
      | None -> ()
      | Some { key; value; _ } ->
        gc_undirected cx key;
        gc_undirected cx value);
      gc_undirected cx objtype.proto_t

  | MethodT(_, _, this, params, ret, _) ->
      gc cx (not polarity) this;
      params |> List.iter (gc cx (not polarity));
      gc cx polarity ret

  | SetT(_, _, t) ->
      gc cx (not polarity) t

  | GetT(_, _, t) ->
      gc cx polarity t

  | ClassT(t) ->
      gc cx polarity t

  | InstanceT(_, static, super, instance) ->
      iter_props cx instance.fields_tmap (fun _ -> gc_undirected cx);
      iter_props cx instance.methods_tmap (fun _ -> gc_undirected cx);
      gc cx polarity static;
      gc cx polarity super

  | TypeT(_, t) ->
      gc_undirected cx t

  | SuperT(_, instance) ->
      iter_props cx instance.fields_tmap (fun _ -> gc_undirected cx);
      iter_props cx instance.methods_tmap (fun _ -> gc cx (not polarity))

  | ConstructorT(_, params, obj) ->
      params |> List.iter (gc cx (not polarity));
      gc_undirected cx obj

  | ArrT(_, t, ts) ->
      gc_undirected cx t;
      ts |> List.iter (gc_undirected cx);

  | SetElemT(_, i, t) ->
      gc_undirected cx i;
      gc cx (not polarity) t

  | GetElemT(_, i, t) ->
      gc_undirected cx i;
      gc cx polarity t

  | AdderT(_, t1, t2) ->
      gc cx (not polarity) t1;
      gc cx polarity t2

  | RestT t ->
      gc cx polarity t

  | OptionalT t ->
      gc cx polarity t

  | PredicateT (pred, t) ->
      gc_pred cx polarity pred;
      gc cx polarity t

  | EqT (_, t) ->
      gc cx (not polarity) t

  | MaybeT(t) ->
      gc cx polarity t

  | _ -> ()

and gc_pred cx polarity = function

  | AndP (p1,p2) ->
      gc_pred cx polarity p1;
      gc_pred cx polarity p2

  | OrP (p1,p2) ->
      gc_pred cx polarity p1;
      gc_pred cx polarity p2

  | NotP (p) ->
      gc_pred cx polarity p

  | InstanceofP t ->
      gc cx (not polarity) t

  | ConstructorP t ->
      gc cx (not polarity) t

  | _ -> ()


and gc_undirected cx = function

  | OpenT(reason, id) ->
      let bounds = cx.graph |> IMap.find_unsafe id in
      if (ISet.mem id gc_state.positive)
      then ()
      else (
        gc_state.positive <- gc_state.positive |> ISet.add id;
        bounds.lower |> TypeMap.iter (fun t _ -> gc cx true t)
      );
      if (ISet.mem id gc_state.negative)
      then ()
      else (
        gc_state.negative <- gc_state.negative |> ISet.add id;
        bounds.upper |> TypeMap.iter (fun t _ -> gc cx false t)
      )

  | FunT(_, static, prototype, funtype) ->
      gc_undirected cx funtype.this_t;
      funtype.params_tlist |> List.iter (gc_undirected cx);
      gc_undirected cx funtype.return_t;
      gc_undirected cx prototype;
      gc_undirected cx static

  | CallT(_, funtype) ->
      gc_undirected cx funtype.this_t;
      funtype.params_tlist |> List.iter (gc_undirected cx);
      gc_undirected cx funtype.return_t

  | ObjT(_, objtype) ->
      let id = objtype.props_tmap in
      gc_state.objs <- gc_state.objs |> ISet.add id;
      iter_props cx id (fun _ -> gc_undirected cx);
      (match objtype.dict_t with
      | None -> ()
      | Some { key; value; _ } ->
        gc_undirected cx key;
        gc_undirected cx value);
      gc_undirected cx objtype.proto_t

  | MethodT(_, _, this, params, ret, _) ->
      gc_undirected cx this;
      params |> List.iter (gc_undirected cx);
      gc_undirected cx ret

  | SetT(_, _, t) ->
      gc_undirected cx t

  | GetT(_, _, t) ->
      gc_undirected cx t

  | ClassT(t) ->
      gc_undirected cx t

  | InstanceT(_, static, super, instance) ->
      iter_props cx instance.fields_tmap (fun _ -> gc_undirected cx);
      iter_props cx instance.methods_tmap (fun _ -> gc_undirected cx);
      gc_undirected cx static;
      gc_undirected cx super

  | TypeT(_, t) ->
      gc_undirected cx t

  | SuperT(_, instance) ->
      iter_props cx instance.fields_tmap (fun _ -> gc_undirected cx);
      iter_props cx instance.methods_tmap (fun _ -> gc_undirected cx)

  | ConstructorT(_, params, obj) ->
      params |> List.iter (gc_undirected cx);
      gc_undirected cx obj

  | ArrT(_, t, ts) ->
      gc_undirected cx t;
      ts |> List.iter (gc_undirected cx)

  | SetElemT(_, i, t) ->
      gc_undirected cx i;
      gc_undirected cx t

  | GetElemT(_, i, t) ->
      gc_undirected cx i;
      gc_undirected cx t

  | AdderT(_, t1, t2) ->
      gc_undirected cx t1;
      gc_undirected cx t2

  | RestT t ->
      gc_undirected cx t

  | OptionalT t ->
      gc_undirected cx t

  | PredicateT (pred, t) ->
      gc_undirected_pred cx pred;
      gc_undirected cx t

  | EqT (_, t) ->
      gc_undirected cx t

  | MaybeT(t) ->
      gc_undirected cx t

  | _ -> ()

and gc_undirected_pred cx = function

  | AndP (p1,p2) ->
      gc_undirected_pred cx p1;
      gc_undirected_pred cx p2

  | OrP (p1,p2) ->
      gc_undirected_pred cx p1;
      gc_undirected_pred cx p2

  | NotP (p) ->
      gc_undirected_pred cx p

  | InstanceofP t ->
      gc_undirected cx t

  | ConstructorP t ->
      gc_undirected cx t

  | _ -> ()



let die cx tvar =
  let bounds = IMap.find_unsafe tvar cx.graph in
  bounds.lowertvars |> IMap.iter (fun l _ ->
    if (l = tvar)
    then ()
    else
      let bounds_l = IMap.find_unsafe l cx.graph in
      bounds_l.uppertvars <-
        IMap.remove tvar bounds_l.uppertvars
  );
  bounds.uppertvars |> IMap.iter (fun u _ ->
    if (u = tvar)
    then ()
    else
      let bounds_u = IMap.find_unsafe u cx.graph in
      bounds_u.lowertvars <-
        IMap.remove tvar bounds_u.lowertvars
  );
  (if modes.verbose then prerr_endlinef "DEAD: %d" tvar);
  cx.graph <- cx.graph |> IMap.remove tvar

let kill_lower cx tvar =
  let bounds = IMap.find_unsafe tvar cx.graph in
  bounds.lower <- TypeMap.empty;
  bounds.lowertvars |> IMap.iter (fun l _ ->
    if (l = tvar)
    then ()
    else
      let bounds_l = IMap.find_unsafe l cx.graph in
      bounds_l.uppertvars <-
        IMap.remove tvar bounds_l.uppertvars
  );
  bounds.lowertvars <-
    IMap.singleton tvar (IMap.find_unsafe tvar bounds.lowertvars)

let kill_upper cx tvar =
  let bounds = IMap.find_unsafe tvar cx.graph in
  bounds.upper <- TypeMap.empty;
  bounds.uppertvars |> IMap.iter (fun u _ ->
    if (u = tvar)
    then ()
    else
      let bounds_u = IMap.find_unsafe u cx.graph in
      bounds_u.lowertvars <-
        IMap.remove tvar bounds_u.lowertvars
  );
  bounds.uppertvars <-
    IMap.singleton tvar (IMap.find_unsafe tvar bounds.uppertvars)

let die_obj cx id =
  cx.property_maps <- cx.property_maps |> IMap.remove id

(* flag controls in-module GC *)
let cleanup_enabled = ref true

let clear_gc_state () =
  gc_state.positive <- ISet.empty;
  gc_state.negative <- ISet.empty

let cleanup cx =
  if !cleanup_enabled then (
    cx.graph |> IMap.iter (fun tvar _ ->
      if (ISet.mem tvar gc_state.positive || ISet.mem tvar gc_state.negative)
      then (
        if (not (ISet.mem tvar gc_state.positive))
        then kill_lower cx tvar
        else if (not (ISet.mem tvar gc_state.negative))
        then kill_upper cx tvar
        else
          (if modes.verbose then prerr_endlinef "LIVE: %d" tvar)
      )
      else
        die cx tvar
    );
  (*
    cx.property_maps |> IMap.iter (fun id _ ->
      if (not (ISet.mem id gc_state.objs))
      then die_obj cx id
    )
  *)
  )

let ok_bound = function
  | UnifyT _ -> false
  | _ -> true

let check_properties cx =
  cx.property_maps |> IMap.iter (fun _ ->
    SMap.iter (fun _ -> function

      | OpenT(reason,id) ->
          let bounds = IMap.find_unsafe id cx.graph in
          let no_def =
            not (ISet.mem id gc_state.negative)
            && TypeMap.is_empty
              (bounds.lower |> TypeMap.filter (fun t _ -> ok_bound t))
            && IMap.cardinal bounds.lowertvars = 1 in
          let no_use =
            not (ISet.mem id gc_state.positive)
            && TypeMap.is_empty bounds.upper
            && IMap.cardinal bounds.uppertvars = 1 in

          (* DISABLED
          if no_def && not no_use
          then add_warning cx "This property is never defined." reason;
          if no_use && not no_def
          then add_warning cx "This property is never used." reason;
          *) ignore no_def; ignore no_use

      | _ -> ()
    )
  )

(* Disabling gc so that query/replace has complete information. *)
let do_gc cx neg pos =
  List.iter (gc cx false) neg;
  List.iter (gc cx true) pos;
  (* cleanup cx; *)
  check_properties cx;
  clear_gc_state ()

(* TODO: Think of a better place to put this *)
let rec extract_members cx this_t =
  match this_t with
  | MaybeT t ->
      (* TODO: do we want to autocomplete when the var could be null? *)
      (*extract_members cx t*)
      SMap.empty
  | InstanceT (reason, _, super,
              {fields_tmap = fields;
               methods_tmap = methods;
               _}) ->
      let fields = find_props cx fields in
      let methods = find_props cx methods in
      let super_t = resolve_type cx super in
      let members = SMap.union fields methods in
      let super_flds = extract_members cx super_t in
      SMap.union super_flds members
  | ObjT (_, {props_tmap = flds; proto_t = proto; _}) ->
      let proto_t = resolve_type cx proto in
      let prot_members = extract_members cx proto_t in
      let members = find_props cx flds in
      SMap.union prot_members members
  | TypeAppT (c, ts) ->
      let c = resolve_type cx c in
      let inst_t = instantiate_poly_t cx c ts in
      let inst_t = instantiate_type inst_t in
      extract_members cx inst_t
  | PolyT (type_params, sub_type) ->
      (* TODO: replace type parameters with stable/proper names? *)
      extract_members cx sub_type
  | ClassT (InstanceT (_, static, _, _)) ->
      let static_t = resolve_type cx static in
      extract_members cx static_t
  | ClassT t ->
      (* TODO: Can this happen? *)
      extract_members cx t
  | IntersectionT (r, ts)
  | UnionT (r, ts) ->
      let ts = List.map (resolve_type cx) ts in
      let members = List.map (extract_members cx) ts in
      intersect_members cx members
  | _ ->
      (* TODO: What types could come up here which we need to handle? *)
      SMap.empty

and intersect_members cx members =
  match members with
  | [] -> SMap.empty
  | _ ->
      let map = SMap.map (fun x -> [x]) (List.hd members) in
      let map = List.fold_left (fun acc x ->
          SMap.merge (fun name tl t ->
              match (tl, t) with
              | (None, None)      -> None
              | (None, Some _)    -> None
              | (Some _, None)    -> None
              | (Some tl, Some t) -> Some (t :: tl)
            ) acc x
        ) map (List.tl members) in
      SMap.map (List.fold_left (fun acc x ->
          merge_type cx (acc, x)
        ) UndefT.t) map
