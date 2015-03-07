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

let dummy_static =
  MixedT (reason_of_string "empty statics object")

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

let default_flags = { sealed = false; exact = true; }

let mk_objecttype ?(flags=default_flags) dict map proto = {
  flags;
  dict_t = dict;
  props_tmap = map;
  proto_t = proto
}

(**************************************************************)

let throw_on_error = ref false

let silent_warnings = false

exception FlowError of (reason * string) list

let add_output cx level message_list =
  if modes.debug then
    prerr_endline (spf "\nadd_output\n%s" (
      String.concat "\n" (
        List.map (fun (r, s) -> spf "r: [%s] s = %S" (dump_reason r) s)
          message_list)));

  if !throw_on_error then raise (FlowError message_list)
  else
    let error = level, message_list in
    if level = Errors_js.ERROR || not silent_warnings then
    cx.errors <- Errors_js.ErrorSet.add error cx.errors

(* tvars *)

let mk_var cx = Ident.make ""

let mk_tvar cx reason =
  let tvar = mk_var cx in
  let graph = cx.graph in
  cx.graph <- graph |> IMap.add tvar (new_bounds tvar reason);
  (if modes.debug then prerr_endline
    (spf "TVAR %d (%d): %s" tvar (IMap.cardinal graph)
      (string_of_reason reason)));
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
      (if modes.debug then prerr_endline (spf "HAVOC::%d" x1));
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
  let count = mk_var cx in
  let stack = count::stack in
  cx.closures <- IMap.add count (stack, ctx) cx.closures;
  frames := stack

(***************)
(* print utils *)
(***************)

let string_of_flow r1 r2 =
  spf "%s\nis incompatible with\n%s"
    (string_of_reason r1) (string_of_reason r2)

let fmt_trace trace =
  let traces = modes.traces_enabled in
  let newtraces = modes.newtraces_enabled in
  "\n"
  ^
  (if traces
  then "\n" ^ (string_of_trace_old "" true trace)
  else "")
  ^
  (if newtraces
  then "\n" ^ (string_of_trace "" true trace)
  else "")

let reorder l u =
  if is_use u || pos_of_t l = Pos.none then u, l else l, u

let ordered_reasons l u =
  let (t1,t2) = reorder l u in
  let r1 = reason_of_t t1 in
  let r2 = reason_of_t t2 in
  (r1, r2)

let lib_reason r =
  Files_js.is_lib_file (Relative_path.to_absolute
    (Pos.filename (pos_of_reason r)))

let prmsg_flow cx level trace msg (r1, r2) =
  let reasons_trace =
    reasons_of_trace trace |> List.map (fun r -> r, desc_of_reason2 r)
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
      r1, spf "%s\n%s %s" (desc_of_reason2 r1) msg (desc_of_reason2 r2);
    ]
    else [
      r1, spf "%s\n%s" (desc_of_reason2 r1) msg;
      r2, (desc_of_reason2 r2) ^
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
    let desc = desc_of_reason2 reason in
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
  let id = mk_var cx in
  cx.property_maps <- IMap.add id pmap cx.property_maps;
  id

let has_prop cx id x =
  IMap.find_unsafe id cx.property_maps |> SMap.mem x

let read_prop cx id x =
  IMap.find_unsafe id cx.property_maps |> SMap.find_unsafe x

let read_prop_ cx id x =
  IMap.find_unsafe id cx.property_maps |> (fun pmap ->
    let t = SMap.find_unsafe x pmap in
    let pmap = SMap.remove x pmap in
    cx.property_maps <- IMap.add id pmap cx.property_maps;
    t
  )

let write_prop cx id x t =
  let pmap = IMap.find_unsafe id cx.property_maps in
  let pmap = SMap.add x t pmap in
  cx.property_maps <- IMap.add id pmap cx.property_maps

let iter_props cx id f =
  IMap.find_unsafe id cx.property_maps
  |> SMap.iter f

let iter_props_ cx id f =
  IMap.find_unsafe id cx.property_maps
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
    let desc = desc_of_reason2 (reason_of_t t) in
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
      let dict =
        { dict_name = Some "_";
          key = merge_type cx (ot1.dict_t.key, ot2.dict_t.key);
          value = merge_type cx (ot1.dict_t.value, ot2.dict_t.value); }
      in
      let pmap =
        let map1 = IMap.find_unsafe ot1.props_tmap cx.property_maps in
        let map2 = IMap.find_unsafe ot2.props_tmap cx.property_maps in
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
      let dict =
        { dict_name = ot.dict_t.dict_name;
          key = (ground_type_impl cx ids ot.dict_t.key);
          value = (ground_type_impl cx ids ot.dict_t.value); } in
      let pmap =
        IMap.find_unsafe ot.props_tmap cx.property_maps
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

  | CustomClassT (n, ts, t) ->
      CustomClassT
        (n,
         List.map (ground_type_impl cx ids) ts,
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
      TypeAppT (
        c,
        ts
      )

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

  | ShapeT t ->
      ShapeT (ground_type_impl cx ids t)

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
        IMap.find_unsafe ot.props_tmap cx.property_maps
        |> SMap.map (normalize_type cx)
        |> mk_propmap cx
      in
      ObjT (r,
        { ot with
          dict_t = { ot.dict_t with
                     key = normalize_type cx ot.dict_t.key;
                     value = normalize_type cx ot.dict_t.value; };
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
      TypeAppT (normalize_type cx c, List.map (normalize_type cx) ts)

  | CustomClassT (n, ts, t) ->
      CustomClassT (n, List.map (normalize_type cx) ts, normalize_type cx t)

  | LowerBoundT t ->
      LowerBoundT (normalize_type cx t)

  | UpperBoundT t ->
      UpperBoundT (normalize_type cx t)

  | ShapeT t ->
      ShapeT (normalize_type cx t)

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
        IMap.find_unsafe ot.props_tmap cx.property_maps
        |> SMap.map (printify_type cx)
        |> mk_propmap cx
      in
      ObjT (r,
        { ot with
          dict_t = { ot.dict_t with
                     key = printify_type cx ot.dict_t.key;
                     value = printify_type cx ot.dict_t.value; };
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
      TypeAppT (printify_type cx c, List.map (printify_type cx) ts)

  | CustomClassT (n, ts, t) ->
      CustomClassT (n, List.map (printify_type cx) ts, printify_type cx t)

  | LowerBoundT t ->
      LowerBoundT (printify_type cx t)

  | UpperBoundT t ->
      UpperBoundT (printify_type cx t)

  | ShapeT t ->
      ShapeT (printify_type cx t)

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

let is_required cx id =
  match IMap.get id !blame_map with
  | None -> false
  | Some rs -> (cx.strict_required <- cx.strict_required |> SSet.union rs; true)

(* need to consider only "def" types *)
let rec assert_ground ?(infer=false) cx ids = function
  | BoundT _ -> ()

  | OpenT (_, id) when ISet.mem id !ids -> ()
  | OpenT (reason_open, id) ->
      ids := !ids |> ISet.add id;
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
            reason_open, spf "%s\n%s" (desc_of_reason2 reason_open) msg;
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

  | CustomClassT (_, _, t) -> assert_ground cx ids t

  | InstanceT (reason, static, super, instance) ->
      let f = assert_ground cx ids in
      instance.fields_tmap |> SMap.iter (fun _ -> f);
      instance.methods_tmap |> SMap.iter (fun _ -> f);
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

  | ShapeT(t) ->
      assert_ground cx ids t

  | EnumT(reason,t) ->
      assert_ground cx ids t

  | RecordT(reason,t) ->
      assert_ground cx ids t

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
  | MarkupT(_,_,t)
  | ObjAssignT(_,_,t)
  | ObjRestT(_,_,t)
  | ObjExtendT(_,_,t)
  | KeyT(_,t)

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

  ignore (is_required cx id);

  let ids = ref (ISet.singleton id) in
  bounds.lower |> TypeMap.iter (fun t _ ->
    assert_ground cx ids t
  );

  blame_map := IMap.empty

(**************)
(* builtins *)
(**************)

let master_cx = new_context (Files_js.get_flowlib_root ()) Files_js.lib_module

let builtins = mk_tvar master_cx (builtin_reason "module")

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
    prerr_endline (spf "[%d] %s" !count (f()))

let debug_flow (l,u) =
  spf "%s ~> %s" (string_of_ctor l) (string_of_ctor u)

module Cache = struct
  (* Flow cache: maps pairs of (hash deftype, hash usetype) to hits. This
     ensures that flows between deftype and usetype are only processed once. *)
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

(********************)
(* subtype relation *)
(********************)

let rec flow cx (l,u) trace =
  if not (Cache.F.mem (l,u)) then (

    if (not (is_use l)) then () else failwith (string_of_t cx l);

    (if modes.debug
     then prerr_endline
        (spf "\n# %s ~>\n# %s"
          (dump_reason (reason_of_t l))
          (dump_reason (reason_of_t u))));

    if ground_subtype (l,u) then () else match (l,u) with

    (* if X -> Y exists then that flow has already been processed *)
    | (OpenT(reason1,tvar1),OpenT(_,tvar2))
        when exists_link cx (tvar1,tvar2)
          -> ()

    (******************)
    (* process X ~> Y *)
    (******************)

    (* initial link bounds: X' -> X, Y -> Y' *)
    (* initial type bounds: L => X, Y => U *)
    | (OpenT(reason1,tvar1),OpenT(reason2,tvar2)) ->

      let bounds1 = find_graph cx tvar1 in
      let bounds2 = find_graph cx tvar2 in

      bounds1.lowertvars |> IMap.iter (fun tvar trace_l ->
        let f2 = (fun trace_u -> concat_trace[trace_l;trace;trace_u]) in
        let bounds = find_graph cx tvar in

          (* final: X -> Y#, X -> Y'#, X' -> Y#, X' -> Y'# *)
        bounds.uppertvars <-
          UnionIMap.union f2 bounds2.uppertvars bounds.uppertvars;

          (* final: X => U#, X' => U# *)
        bounds.upper <-
          UnionTypeMap.union f2 bounds2.upper bounds.upper
      );
      bounds2.uppertvars |> IMap.iter (fun tvar trace_u ->
        let f1 = (fun trace_l -> concat_trace[trace_l;trace;trace_u]) in
        let bounds = find_graph cx tvar in

          (* final: X# -> Y, X'# -> Y, X# -> Y', X'# -> Y' *)
        bounds.lowertvars <-
          UnionIMap.union f1 bounds1.lowertvars bounds.lowertvars;
          (* final: L# => Y, L# => Y' *)
        bounds.lower <-
          UnionTypeMap.union f1 bounds1.lower bounds.lower
      );
        (* final: process L ~> U *)
      bounds1.lower |> TypeMap.iter
          (fun l trace_l -> bounds2.upper |> TypeMap.iter
              (fun u trace_u ->
                flow cx (l,u) (concat_trace [trace_l;trace;trace_u])))

    (* if Y => U exists then that flow has already been processed *)
    | (OpenT(reason,tvar),t) when exists_upper_bound cx (tvar,t)
        -> ()

    (******************)
    (* process Y ~> U *)
    (******************)

    (* initial: Y' -> Y *)
    (* initial: L => Y *)
    | (OpenT(reason,tvar),t) ->

      let bounds = find_graph cx tvar in
      bounds.lowertvars |> IMap.iter (fun tvar trace_l ->
        let bounds = find_graph cx tvar in

          (* final: Y => U, Y' => U *)
        bounds.upper <-
          TypeMap.add t (concat_trace [trace_l;trace]) bounds.upper
      );

        (* final: process L ~> U *)
      bounds.lower |> TypeMap.iter (fun l trace_l ->
        flow cx (l,t) (concat_trace [trace_l;trace])
      )

    (* if U => X exists then that flow has already been processed *)
    | (t,OpenT(reason,tvar)) when exists_lower_bound cx (tvar,t)
        -> ()

    (******************)
    (* process L ~> X *)
    (******************)

    (* initial: X -> X' *)
    (* initial: X => U *)
    | (t,OpenT(reason,tvar)) ->

      let bounds = find_graph cx tvar in
      bounds.uppertvars |> IMap.iter (fun tvar trace_u ->
        let bounds = find_graph cx tvar in

          (* final: L => X, L => X' *)
        bounds.lower <-
          TypeMap.add t (concat_trace [trace;trace_u]) bounds.lower
      );

        (* final: process L ~> U *)
      bounds.upper |> TypeMap.iter (fun u trace_u ->
        flow cx (t,u) (concat_trace [trace;trace_u])
      )

    (************************************************)
    (* bound variables are equal only to themselves *)
    (************************************************)

    | (BoundT _, _) | (_, BoundT _) when l = u ->
      ()

    (********************************************)
    (* summary types forget literal information *)
    (********************************************)

    | (StrT (_, Some _), SummarizeT (reason, t)) ->
      unify cx (StrT.why reason) t

    | (NumT (_, Some _), SummarizeT (reason, t)) ->
      unify cx (NumT.why reason) t

    | (_, SummarizeT (reason, t)) ->
      unify cx l t

    (*******************************)
    (* common implicit convertions *)
    (*******************************)

    | (_, BoolT _) -> ()

    | (_, NumT _) when numeric l -> ()

    | (_, AnyObjT _) when object_like l -> ()
    | (AnyObjT _, _) when object_like u || object_like_op u -> ()

    (**************************************)
    (* some properties are always defined *)
    (**************************************)

    (** TODO: These properties should go in Object.prototype. Currently we model
        Object.prototype as a MixedT, as an optimization against a possible
        deluge of shadow properties (see below) on Object.prototype, since it is
        shared by every object. **)

    | (_, MethodT(reason_op,"toString",this,tins,tout,_)) ->
      unit_flow cx (StrT.why reason_op, tout)

    | (_,
       (GetT(_,"toString",_) | SetT(_,"toString",_))) -> ()

    | (_, MethodT(reason_op,"hasOwnProperty",this,tins,tout,_)) ->
      unit_flow cx (BoolT.why reason_op, tout)

    | (_,
       (GetT(_,"hasOwnProperty",_) | SetT(_,"hasOwnProperty",_))) -> ()

    | (_, MethodT(reason_op,"propertyIsEnumerable",this,tins,tout,_)) ->
      unit_flow cx (BoolT.why reason_op, tout)

    | (_,
       (GetT(_,"propertyIsEnumerable",_) | SetT(_,"propertyIsEnumerable",_))) -> ()

    | (_, MethodT(reason_op,"valueOf",this,tins,tout,_)) ->
      unit_flow cx (NumT.why reason_op, tout)

    | (_,
       (GetT(_,"valueOf",_) | SetT(_,"valueOf",_))) -> ()

    | (_, MethodT(reason_op,"toLocaleString",this,tins,tout,_)) ->
      unit_flow cx (StrT.why reason_op, tout)

    | (_,
       (GetT(_,"toLocaleString",_) | SetT(_,"toLocaleString",_))) -> ()

    | ((ObjT _ | ArrT _), GetT(reason_op,"constructor",tout)) ->
      unit_flow cx (AnyT.why reason_op, tout)

    | ((ObjT _ | ArrT _),
       (SetT(_,"constructor",_) | MethodT(_,"constructor",_,_,_,_))) -> ()

    (***************)
    (* maybe types *)
    (***************)

    | ((NullT _ | VoidT _), MaybeT _) -> ()

    | (MaybeT(t1), MaybeT(t2)) ->
      select_flow cx (t1,t2) trace DecomposeNullable

    (******************)
    (* optional types *)
    (******************)

    (** The type optional(T) is not the same as undefined | T. In particular, we
        don't have a rule that lets undefined be a subtype of optional(T): this
        prevents undefined values from flowing to locations typed
        optional(T). On the other hand, optional(T) can be refined to T with
        the same dynamic checks that would rule out undefined values. *)

    | (OptionalT(t1), OptionalT(t2)) ->
        unit_flow cx (t1, t2)

    (*****************)
    (* logical types *)
    (*****************)

    | (left, AndT(reason, right, u)) ->
      (* a falsy && b ~> a
         a truthy && b ~> b
         a && b ~> a falsy | b *)
      let op = Op "&&" in
      let truthy_left = filter_exists left in
      (match truthy_left with
      | UndefT _ ->
        (* falsy *)
        select_flow cx (left, mk_predicate (NotP ExistsP, u)) trace op
      | _ ->
        (match filter_not_exists left with
        | UndefT _ -> (* truthy *) select_flow cx (right, u) trace op
        | _ ->
          select_flow cx (left, mk_predicate (NotP ExistsP, u)) trace op;
          (match truthy_left with
          | UndefT _ -> ()
          | _ -> select_flow cx (right, u) trace op)
        )
      )

    | (left, OrT(reason, right, u)) ->
      (* a truthy || b ~> a
         a falsy || b ~> b
         a || b ~> a truthy | b *)
      let op = Op "||" in
      let falsy_left = filter_not_exists left in
      (match falsy_left with
      | UndefT _ ->
        (* truthy *)
        select_flow cx (left, mk_predicate (ExistsP, u)) trace op
      | _ ->
        (match filter_exists left with
        | UndefT _ -> (* falsy *) select_flow cx (right, u) trace op
        | _ ->
          select_flow cx (left, mk_predicate (ExistsP, u)) trace op;
          (match falsy_left with
          | UndefT _ -> ()
          | _ -> select_flow cx (right, u) trace op)
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
      unit_flow cx (t,u)

    | (_, UpperBoundT t) ->
      ()

    | (LowerBoundT t, _) ->
      ()

    | (_, LowerBoundT t) ->
      unit_flow cx (l,t)

    (** In general, typechecking is monotonic in the sense that more constraints
        produce more errors. However, sometimes we may want to speculatively try
        out constraints, backtracking if they produce errors (and removing the
        errors produced). This is useful to typecheck union types and
        intersection types: see below. **)

    | (_, ConcretizeT (r, ls, tl, tu)) -> (
      concretize_upper_ cx r l;
      match ls with
      | [] ->
            (* Done: trigger flow from l to u *)
        unit_flow cx (tl, tu)
      | _ ->
            (* Not done: recurse *)
        concretize_lower cx r ls tl tu
    )

    | (ConcretizeT _, _) when is_use u ->
      ()

    | (ConcretizeT (r, us, tl, tu), _) -> (
      match us with
      | [] ->
            (* Done: trigger flow from l to u *)
        unit_flow cx (tl, tu)
      | _ ->
            (* Not done: recurse *)
        concretize_upper cx r us tl tu
    )

    (***************)
    (* union types *)
    (***************)

    | (UnionT(_,ts), _) ->
      ts |> List.iter (fun t -> unit_flow cx (t,u))

    | (OptionalT(t1), MaybeT(t2)) ->
      (* You can't unwrap the maybe yet in case this is an
       * OptionalT(MaybeT(t)) *)
      unit_flow cx (t1, u)

    | (t1, MaybeT(t2)) ->
      unit_flow cx (t1,t2)

    | (t1, OptionalT(t2)) ->
      unit_flow cx (t1, t2)

    | (ConcreteT l, UnionT(r,ts)) ->
      speculative_match cx ts (fun t ->
        speculative_flow cx l t
      )

    | (_, UnionT(r,ts)) ->
      concretize_upper cx r ts (ConcreteT l) u

    (**********************)
    (* intersection types *)
    (**********************)

    | (_, IntersectionT(_,ts)) ->
      ts |> List.iter (fun t -> unit_flow cx (l,t))

    | (IntersectionT (_, ts), ConcreteT u)
      ->
      speculative_match cx ts (fun t ->
        speculative_flow cx t u
      )

    | (IntersectionT (r,ts), _)
      ->
        (* Concretize the args of the call site, then match the overloaded
           functions in the intersection with the transformed call site *)
      concretize_lower_ cx r u;
        concretize_lower cx r ts l (ConcreteT u)

    (***************************************)
    (* generic function may be specialized *)
    (***************************************)

    | (PolyT (ids,t), SpecializeT(reason,ts,tvar)) ->
      let ts, t_ = instantiate_poly_ cx reason (ids,t) ts in
      select_flow cx (t_, tvar)
        trace InstantiatePoly

    | (TypeAppT(c,ts), _) ->
      let reason = reason_of_t u in
      let t = mk_tvar cx reason in
      select_flow cx (c, SpecializeT(reason,ts,t))
        trace InstantiatePoly;
      unit_flow cx (mk_annot cx reason t, u)

    | (_, TypeAppT(c,ts)) ->
      let reason = reason_of_t l in
      let t = mk_tvar cx reason in
      select_flow cx (c, SpecializeT(reason,ts,t))
        trace InstantiatePoly;
      unit_flow cx (l, mk_annot cx reason t)

    | (PolyT _, PolyT _) -> () (* TODO *)

    | (PolyT (ids,t), _) ->
      let reason = reason_of_t u in
      let ts, t_ = instantiate_poly cx reason (ids,t) in
      select_flow cx (t_, u)
        trace InstantiatePoly

    (***********************************************)
    (* function types deconstruct into their parts *)
    (***********************************************)

    | (FunT (reason1,_,_,
             {this_t = o1; params_tlist = tins1; return_t = t1; closure_t = i; _}),
       FunT (reason2,_,_,
             {this_t = o2; params_tlist = tins2; return_t = t2; closure_t = j; _}))
      ->
        select_flow cx (o2,o1) trace FunThis;
        multiflow cx trace (u, l) true (tins2,tins1) |> ignore;
        select_flow cx (t1,t2) trace FunRet;
        havoc_ctx cx i j

    | (FunT (reason1,_,_,
             {this_t = o1; params_tlist = tins1; return_t = t1; closure_t = i; _}),
       CallT (reason2,
              {this_t = o2; params_tlist = tins2; return_t = t2; closure_t = j; _}))
      ->
        select_flow cx (o2,o1) trace FunThis;
        multiflow cx trace (u, l) true (tins2,tins1) |> ignore;
        (* relocate the function's return type at the call site *)
        let t1 = repos_t_from_reason reason2 t1 in
        select_flow cx (t1,t2) trace FunRet;
        havoc_ctx cx i j

    (*********************************************)
    (* object types deconstruct into their parts *)
    (*********************************************)

    | (ObjT (reason1, {props_tmap = flds1; proto_t; flags; _ }),
       ObjT (reason2, {props_tmap = flds2; proto_t = u_; dict_t; _ }))
      ->
      (* if inflowing type is literal (thus guaranteed to be
         unaliased), propertywise subtyping is sound *)
      let lit = (desc_of_reason reason1) = "object literal" in
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
                unit_flow cx (l, LookupT (reason2, None, s, t1))
            | _ ->
                (* otherwise, we do strict lookup of property in prototype *)
                unit_flow cx (proto_t, LookupT (reason2, Some reason1, s, t2))
          (* TODO: instead, consider extending inflowing type with s:t2 when it
             is not sealed *)
          else
            let t1 = read_prop cx flds1 s in
            flow_to_mutable_child cx lit t1 t2);
      (* Any properties in l but not u must match indexer *)
      iter_props_ cx flds1
        (fun s -> fun t1 ->
          if (not(has_prop cx flds2 s))
          then begin
            let key_reason = replace_reason StrT.desc (reason_of_t t1) in
            let key_reason =
              prefix_reason (spf "property %s's key is a " s) key_reason in
            unit_flow cx (StrT (key_reason, None), dict_t.key);
            unit_flow cx (t1, dict_t.value)
          end
        );
      unit_flow cx (l, u_)

    | (InstanceT (reason1, _, super, { fields_tmap; methods_tmap; _ }),
       ObjT (reason2, {props_tmap = flds2; proto_t = u_; _ }))
      ->
      let flds1 = SMap.union fields_tmap methods_tmap in
      iter_props_ cx flds2
        (fun s -> fun t2 ->
          if (not(SMap.mem s flds1))
          then
            let reason2 = replace_reason (spf "property %s" s) reason2 in
            unit_flow cx (super, LookupT (reason2, Some reason1, s, t2))
          else
            let t1 = SMap.find_unsafe s flds1 in
            unify cx t1 t2
        );
      unit_flow cx (l, u_)

    | (ObjT (reason2, _),
       InstanceT (reason1, _, super, { fields_tmap; methods_tmap; _ }))
      ->
      let methods_tmap = SMap.remove "constructor" methods_tmap in
      let flds2 = SMap.union fields_tmap methods_tmap in
      flds2 |> SMap.iter
          (fun s t2 ->
            let reason2 = replace_reason (spf "property %s" s) reason2 in
            unit_flow cx (l, LookupT (reason2, Some reason1, s, t2))
          );
      unit_flow cx (l, super)

    (****************************************)
    (* You can cast an object to a function *)
    (****************************************)
    | (ObjT _, (FunT _ | CallT _)) ->
        let reason = reason_of_t u in
        let tvar = mk_tvar cx reason in
        lookup_prop cx l reason (Some (reason_of_t l)) "$call" tvar;
        unit_flow cx (tvar, u)

    (******************************)
    (* matching shapes of objects *)
    (******************************)

    | (_, ShapeT (o2))
      ->
        unit_flow cx (l, ObjAssignT(reason_of_t o2, o2, AnyT.t))

    | (ShapeT (o1), _) ->
        unit_flow cx (o1, u)

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
        unit_flow cx (l, ExtendsT(l,u))

    | (InstanceT (_,_,super,instance),
       ExtendsT(_, InstanceT (_,_,_,instance_super))) ->

      if instance.class_id = instance_super.class_id
      then
        unify_map cx instance.type_args instance_super.type_args
      else
        unit_flow cx (super, u)

    (********************************************************)
    (* runtime types derive static types through annotation *)
    (********************************************************)

    | (ClassT(it), TypeT(_,t)) ->
      select_flow cx (it, t) trace ClassInst;
      select_flow cx (t, it) trace ClassInst

    | (FunT(reason,_,prototype,_), TypeT(_,t)) ->
      select_flow cx (prototype, t) trace FunProto;
      select_flow cx (t, prototype) trace FunProto

    | (TypeT(_,l), TypeT(_,u)) ->
      unify cx l u

    | (ClassT(l), ClassT(u)) ->
      unify cx l u

    | (FunT(_,static1,prototype,_), ClassT(InstanceT(_,static2,_, _) as u_)) ->
      unify cx static1 static2;
      unify cx prototype u_

    | (CustomClassT(n1,ts1,_), CustomClassT(n2,ts2,_)) when n1 = n2 ->
      if List.length ts1 = List.length ts2
      then List.iter2 (unify cx) ts1 ts2
      else prerr_flow cx trace "Number of type arguments differs from" l u

    (*********************************************************)
    (* class types derive instance types (with constructors) *)
    (*********************************************************)

    | (ClassT(InstanceT(reason_c,static,super,instance)),
       ConstructorT(reason_op,tins2,t))
      ->
      let methods = instance.methods_tmap in
      (match SMap.get "constructor" methods with
      | Some (FunT (reason,_,_,{params_tlist = tins1; _})) ->
        (* TODO: closure *)
        multiflow cx trace (u, l) true (tins2,tins1) |> ignore

      | _ -> () (* TODO *)
      );
      let it = InstanceT(reason_c,static,super,instance) in
      select_flow cx (it, t) trace ClassInst

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
      multiflow cx trace (u, l) true (tins2, tins1) |> ignore;
        let reason_rv = replace_reason "return undefined" reason in
        select_flow cx (tout1, VoidT reason_rv) trace FunRet;

        let reason_c = replace_reason "new object" reason in
        let o = mk_object_with_proto cx reason_c proto in
        select_flow cx (o, this) trace FunInst;

        select_flow cx (o, t) trace FunInst

    (*************************)
    (* "statics" can be read *)
    (*************************)

    | (InstanceT (_,static,_,_), GetT (_,"statics",t))
      ->
      select_flow cx (static, t)
        trace ClassStatics

    (********************************************************)
    (* instances of classes may have their fields looked up *)
    (********************************************************)

    | (InstanceT(reason,_,super,instance),
       LookupT (_,strict,x, t))
      ->
      let pmap = SMap.union instance.fields_tmap instance.methods_tmap in
      (match SMap.get x pmap with
      | None ->
        unit_flow cx (super, u)
      | Some tx ->
        unify cx t tx
      )

    (********************************)
    (* ... and their fields written *)
    (********************************)

    | (InstanceT (reason_c,static,super,instance),
       SetT(reason_op,x,tin))
      ->
      let fields = SMap.union instance.fields_tmap instance.methods_tmap in
      set_prop cx reason_op reason_c super x fields tin

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | (InstanceT (_, _, super, _), GetT (_, "__proto__", t)) ->

      select_flow cx (super,t)
        trace ObjProto

    | (InstanceT (reason_c,static,super,instance),
       GetT(reason_op,x,tout)) ->

      let fields = SMap.union instance.fields_tmap instance.methods_tmap in
      get_prop cx reason_op reason_c super x fields tout

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | (InstanceT (reason_c,static,super,instance),
       MethodT(reason_op,x,this,tins2,tout2,j))
      -> (* TODO: closure *)

      let methods = SMap.union instance.fields_tmap instance.methods_tmap in
      let funt = mk_tvar cx reason_op in
      get_prop cx reason_op reason_c super x methods funt;
      let callt = CallT (reason_op, mk_methodtype2 this tins2 None tout2 j) in
      select_flow cx (funt, callt)
        trace (InstanceProp x)

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

    (***********************)
    (* guarded unification *)
    (***********************)

    | (_, UnifyT(t,t_other)) ->
      unify cx t t_other

    (**********************************************************************)
    (* objects can be assigned, i.e., their properties can be set in bulk *)
    (**********************************************************************)

    | (ObjT (_, { props_tmap = mapr; _ }), ObjAssignT (reason, proto, t)) ->
      iter_props cx mapr (fun x t ->
        let reason = prefix_reason (spf "prop %s of " x) reason in
        unit_flow cx (proto, SetT (reason, x, t));
      );
      unit_flow cx (proto, t)

    | (InstanceT (_, _, _, { fields_tmap; methods_tmap; _ }),
       ObjAssignT (reason, proto, t)) ->
      let map = SMap.union fields_tmap methods_tmap in
      map |> SMap.iter (fun x t ->
        unit_flow cx (proto, SetT (reason, x, t));
      );
      unit_flow cx (proto, t)

    (* Object.assign semantics *)
    | ((MaybeT t1 | OptionalT t1), ObjAssignT _) ->
      unit_flow cx (t1, u)

    | (MixedT _, ObjAssignT (_, proto, t)) ->
      unit_flow cx (proto, t)

    (*************************)
    (* objects can be copied *)
    (*************************)

    | (ObjT (_, { props_tmap = mapr; _ }), ObjRestT (reason, xs, t)) ->
      let map = IMap.find_unsafe mapr cx.property_maps in
      let map = List.fold_left (fun map x -> SMap.remove x map) map xs in
      let o = mk_object_with_map_proto cx reason map (MixedT reason) in
      unit_flow cx (o, t)

    (*************************)
    (* objects can be merged *)
    (*************************)

    | (ObjT (_, { props_tmap = mapr; _ }), ObjExtendT (reason, opt_map, t)) ->
      let default_map = IMap.find_unsafe mapr cx.property_maps in
      let ext_map = SMap.fold (fun x t ext_map ->
        match SMap.get x default_map with
        | Some t_ -> unify cx t t_; ext_map
        | None -> SMap.add x (MaybeT t) ext_map
      ) opt_map default_map in
      let o = mk_object_with_map_proto cx reason ext_map (MixedT reason) in
      unit_flow cx (o, t)

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
      select_flow cx (t, UnifyT(t, t_other)) trace (ObjProp x)

    (*****************************************)
    (* ... and their fields written *)
    (*****************************************)

    (** o.x = ... has the additional effect of o[_] = ... **)

    | (ObjT (reason_o, {
        flags;
        props_tmap = mapr;
        proto_t = proto;
        dict_t = { key; value; _ };
      }),
      SetT(reason_op,x,tin))
      ->
      let strict = mk_strict flags.sealed reason_o reason_op in
      let t = ensure_prop_ cx trace strict mapr x proto reason_o reason_op
        trace in
      unit_flow cx (StrT.t,key);
      unit_flow cx (tin,value);
      select_flow cx (tin,t) trace (ObjProp x)

    (*****************************)
    (* ... and their fields read *)
    (*****************************)

    | (ObjT (_, {proto_t = proto; _}), GetT (_, "__proto__", t)) ->

      select_flow cx (proto,t)
        trace ObjProto

    | (ObjT (reason_o, {
          flags;
          props_tmap = mapr;
          proto_t = proto;
          dict_t = { key; value; _ };
        }),
       GetT(reason_op,x,tout))
      ->
      let strict = mk_strict flags.sealed reason_o reason_op in
      let t = ensure_prop cx strict mapr x proto reason_o reason_op trace in
      unit_flow cx (StrT.t,key);
      (* move property type to read site *)
      let t = repos_t_from_reason reason_op t in
      select_flow cx (t, tout) trace (ObjProp x)

    (********************************)
    (* ... and their methods called *)
    (********************************)

    | (ObjT (reason_o, {
          flags;
          props_tmap = mapr;
          proto_t = proto;
          dict_t = { key; value; _ };
        }),
       MethodT(reason_op,x,this,tins,tout,j))
      ->
      let strict = mk_strict flags.sealed reason_o reason_op in
      let t = ensure_prop cx strict mapr x proto reason_o reason_op trace in
      let callt = CallT (reason_op, mk_methodtype2 this tins None tout j) in
      select_flow cx (t, callt) trace (ObjProp x)

    (******************************************)
    (* strings may have their characters read *)
    (******************************************)

    | (StrT (reason_s, _), GetElemT(reason_op,index,tout)) ->
      select_flow cx (index, NumT.why reason_s)
        trace StrIndex;
      select_flow cx (StrT.why reason_op, tout)
        trace StrElem

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
      unit_flow cx (key, ElemT(reason_op, l, UpperBoundT tin))

    | (ObjT _, GetElemT(reason_op,key,tout))
      ->
      unit_flow cx (key, ElemT(reason_op, l, LowerBoundT tout))

    | (ArrT (_, _, []), SetElemT(reason_op, key,tin))
      ->
      let num = NumT.why reason_op in
      unit_flow cx (num, ElemT(reason_op, l, UpperBoundT tin));
      unit_flow cx (key, num)

    | (ArrT _, SetElemT(reason_op, key,tin))
      ->
      unit_flow cx (key, ElemT(reason_op, l, UpperBoundT tin))

    | (ArrT (_, _, []), GetElemT(reason_op, key,tout))
      ->
      let num = NumT.why reason_op in
      unit_flow cx (num, ElemT(reason_op, l, LowerBoundT tout));
      unit_flow cx (key, num)

     | (ArrT _, GetElemT(reason_op, key,tout))
      ->
      unit_flow cx (key, ElemT(reason_op, l, LowerBoundT tout))

    | (StrT (reason_s, literal),
       ElemT(reason_op, (ObjT(_, {dict_t = { key; value; _ }; _}) as o), t))
      ->
      (match literal with
      | Some x ->
        (match t with
        | UpperBoundT tin -> unit_flow cx (o, SetT(reason_op,x,tin))
        | LowerBoundT tout -> unit_flow cx (o, GetT(reason_op,x,tout))
        | _ -> assert false)
      | None ->
        unit_flow cx (l, key);
        unit_flow cx (value,t);
        unit_flow cx (t,value)
      )

    | (_, ElemT(_, ObjT(_, {dict_t = { key; value; _ }; _}), t))
      ->
      unit_flow cx (l, key);
        unit_flow cx (value,t);
        unit_flow cx (t,value)

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
      | UpperBoundT tin -> unit_flow cx (tin, value)
      | LowerBoundT tout -> unit_flow cx (value, tout)
      | _ -> assert false)

    (***********************************************)
    (* functions may have their prototypes written *)
    (***********************************************)

    | (FunT (reason_f,_,t,_),
       SetT(reason_op,"prototype",tin))
      ->
      unit_flow cx (tin, ObjAssignT(reason_op,t,AnyT.t))

    (*********************************)
    (* ... and their prototypes read *)
    (*********************************)

    | (FunT (reason_f,_,t,_),
       GetT(reason_op,"prototype",tout))
      ->
      select_flow cx (t,tout) trace FunProto

    | (ClassT (instance),
       GetT(reason_op,"prototype",tout))
      ->
      unit_flow cx (instance, tout)

    (***************************************************************)
    (* functions may be called by passing a receiver and arguments *)
    (***************************************************************)

    | (FunT _, MethodT(reason_op,"call",_,o2::tins2,tout2,_))
      -> (* TODO: closure *)

      let funtype = mk_methodtype o2 tins2 None tout2 in
      select_flow cx (l, CallT (prefix_reason "call " reason_op, funtype))
        trace (LibMethod "Function")

    (*******************************************)
    (* ... or a receiver and an argument array *)
    (*******************************************)

    | (FunT _, MethodT(reason_op,"apply",_,[o2;tinsArr2],tout2,_))
      -> (* TODO: closure *)

      let reason = replace_reason "element of arguments" reason_op in
      let elem = mk_tvar cx reason in

      select_flow cx (tinsArr2, ArrT(reason,elem,[]))
        trace ArrElem;

      let funtype = mk_methodtype o2 [RestT elem] None tout2 in
      select_flow cx (l, CallT(prefix_reason "apply " reason_op, funtype))
        trace (LibMethod "Function")

    (************************************************************************)
    (* functions may be bound by passing a receiver and (partial) arguments *)
    (************************************************************************)

    | (FunT (reason,_,_,
             {this_t = o1; params_tlist = tins1; return_t = tout1; _}),
       MethodT(reason_op,"bind",_,o2::tins2,tout2,_))
      -> (* TODO: closure *)

      select_flow cx (o2,o1)
        trace FunThis;

        let tins1 = multiflow cx trace (u, l) false (tins2,tins1) in

        select_flow cx (
          FunT(reason_op, dummy_static, dummy_prototype,
               mk_functiontype tins1 None tout1),
          tout2)
          trace (LibMethod "Function")

    (***********************************************)
    (* You can use a function as a callable object *)
    (***********************************************)
    | (FunT (reason, statics, proto, _) , ObjT _) ->
        let map = SMap.add "$call" l SMap.empty in
        let function_proto = get_builtin_type cx reason "Function" in
        let obj = mk_object_with_map_proto cx reason map function_proto in
        let t = mk_tvar_where cx reason (fun t ->
          unit_flow cx (statics, ObjAssignT (reason, obj, t))
        ) in
        unit_flow cx (t, u)

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
      instance.fields_tmap |> SMap.iter (fun x -> fun t ->
        lookup_prop cx l reason None x t
      );
        (* TODO: method lookup should be covariant instead of invariant *)
        instance.methods_tmap |> SMap.iter (fun x -> fun t ->
          if (x <> "constructor") then
            lookup_prop cx l reason None x t
        )

    | (ObjT _,
       SuperT (reason,instance))
      ->
      instance.fields_tmap |> SMap.iter (fun x -> fun t ->
        select_flow cx (l, LookupT(reason,None,x,t)) trace ObjProto
      );
        instance.methods_tmap |> SMap.iter (fun x -> fun t ->
          if (x <> "constructor") then
            select_flow cx (l, LookupT(reason,None,x,t)) trace ObjProto
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

      select_flow cx (NumT.why reason, t)
        trace (Op "+")

    | (StrT _, AdderT(reason,_, t))
    | (_, AdderT(reason,StrT _, t)) ->

      select_flow cx (StrT.why reason, t)
        trace (Op "+")

    | (MixedT _, AdderT(reason,_, t))
    | (_, AdderT(reason,MixedT _, t)) ->

      select_flow cx (MixedT.why reason, t)
        trace (Op "+")

    | (_, AdderT(reason,((OpenT _ | UnionT _) as tin), tout)) ->

      select_flow cx (tin, AdderT(reason,l , tout))
        trace (Op "+")

    | (_, AdderT(reason,_, t)) ->

      select_flow cx (StrT.why reason, t)
        trace (Op "+")

    (***********************************************************)
    (* comparison typechecks iff either of the following hold: *)
    (*                                                         *)
    (* number <> number = number                               *)
    (* string <> string = string                               *)
    (***********************************************************)

    | (_, ComparatorT(reason, ((OpenT _ | UnionT _) as u_))) ->
       unit_flow cx (u_, ComparatorT (reason, l))

    | (StrT _, ComparatorT(reason,t)) ->

      select_flow cx (t, StrT.why reason)
        trace (Op "comparison")

    | (l, ComparatorT(reason,t)) when numeric l ->

      select_flow cx (t, NumT.why reason)
        trace (Op "comparison")

    (*******)
    (* JSX *)
    (*******)

    | (CustomClassT("ReactClass", _, _), MarkupT(reason_op,o,t)) ->
        let react = module_t cx "react" reason_op in
        let x = mk_tvar_where cx reason_op (fun tvar ->
          unit_flow cx (react, MethodT(reason_op, "createElement", react, [l;o], tvar, 0));
        ) in
        unit_flow cx (x, t)

    | (FunT _, MarkupT _) ->

      () (* TODO *)

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

      flow cx (u_, EqT(reason, l))
        trace

    | (_, EqT(_, u_)) when equatable cx trace (l,u_) -> ()

    (**********************)
    (* Array library call *)
    (**********************)

    | (ArrT (_, t, _), (GetT _ | SetT _ | MethodT _)) ->
      let reason = reason_of_t u in
      let arrt = mk_typeapp_instance cx reason "Array" [t] in
      select_flow cx (arrt, u)
        trace (LibMethod "Array")

    (***********************)
    (* String library call *)
    (***********************)

    | (StrT (reason, _), (GetT _ | MethodT _)) ->
      select_flow cx (get_builtin_type cx reason "String",u)
        trace (LibMethod "String")

    (***********************)
    (* Number library call *)
    (***********************)

    | (NumT (reason, _), (GetT _ | MethodT _)) ->
      select_flow cx (get_builtin_type cx reason "Number",u)
        trace (LibMethod "Number")

    (***********************)
    (* Boolean library call *)
    (***********************)

    | (BoolT (reason, _), (GetT _ | MethodT _)) ->
      select_flow cx (get_builtin_type cx reason "Boolean",u)
        trace (LibMethod "Boolean")

    (***************************)
    (* Keys: enums and records *)
    (***************************)

    | (StrT (reason_s, literal), EnumT (reason_op, o)) ->
      (match literal with
      | Some x ->
        let reason_op =
          replace_reason (spf "string literal %s" x) reason_s in
        unit_flow cx (o, HasT(reason_op,x))
      | None ->
        prerr_flow cx trace "Expected string literal" l u
      )

    | (EnumT (reason1, o1), _) ->
      unit_flow cx (o1, KeyT (reason1, u))

    | (_, RecordT (reason2, o2)) ->
      unit_flow cx (o2, KeyT(reason2, EnumT(reason2, l)))

    | (RecordT (reason, o2),
       (HasT(_,x) |
           LookupT(_,_,x,_) | GetT(_,x,_) | SetT(_,x,_) | MethodT(_,x,_,_,_,_)))
      ->
      unit_flow cx (o2, HasT(reason_of_t u,x))

    | (ObjT (reason_o, { props_tmap = mapr; _ }), HasT(reason_op, x)) ->
      if has_prop cx mapr x then ()
      else
        prmsg_flow cx
          Errors_js.ERROR
          trace
          "Property not found in"
          (reason_op, reason_o)

    | (InstanceT (reason_o, _, _, instance), HasT(reason_op, x)) ->
      let fields = SMap.union instance.fields_tmap instance.methods_tmap in
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
      unit_flow cx (o2, KeyT(reason_, key))

    | (ObjT (reason, { props_tmap = mapr; _ }), KeyT(_,key)) ->
      iter_props cx mapr (fun x tv ->
        let t = StrT (reason, Some x) in
        unit_flow cx (t, key)
      )

    | (InstanceT (reason, _, _, instance), KeyT(_,key)) ->
      let fields = SMap.union instance.fields_tmap instance.methods_tmap in
      fields |> SMap.iter (fun x tv ->
        let t = StrT (reason, Some x) in
        unit_flow cx (t, key)
      )

    | (RecordT (reason, o2), (ObjT _ | InstanceT _)) ->
      let tvar = mk_tvar cx reason in
      unit_flow cx (u, KeyT(reason, tvar));
      unit_flow cx (tvar, EnumT(reason, o2))

    (*********************)
    (* functions statics *)
    (*********************)

    | (FunT (_,static,_,_), _) when object_like_op u ->
      select_flow cx (static, u)
        trace FunStatics

    (*****************)
    (* class statics *)
    (*****************)

    | (ClassT instance, _) when object_like_op u ->
      let reason = reason_of_t u in
      let tvar = mk_tvar cx reason in
      unit_flow cx (instance, GetT(reason,"statics",tvar));
      select_flow cx (tvar,u)
        trace ClassStatics

    (****************)
    (* custom class *)
    (****************)

    | (CustomClassT (_, _, instance), _) ->
      unit_flow cx (ClassT instance, u)

    (********)
    (* cast *)
    (********)

    | (ClassT instance, (FunT _ | CallT _))
      ->
      let reason = reason_of_t u in
      let tvar = mk_tvar cx reason in
      unit_flow cx (instance, GetT(reason,"statics",tvar));
      unit_flow cx (tvar, GetT(reason,"$call",u))

    (************)
    (* indexing *)
    (************)

    | (InstanceT _, GetElemT (reason, i, t))
      ->
      unit_flow cx (l, SetT(reason, "$key", i));
        unit_flow cx (l, GetT(reason, "$value", t))

    | (InstanceT _, SetElemT (reason, i, t))
      ->
      unit_flow cx (l, SetT(reason, "$key", i));
        unit_flow cx (l, SetT(reason, "$value", t))

    (***************)
    (* unsupported *)
    (***************)

    | (MixedT _, TypeT _) ->
      prwarn_flow cx trace
        (spf "instance of class accessing unknown property: %s"
           (desc_of_t u))
        u l

    (** Lookups can be strict or non-strict. When they are strict, they reject
        or accept lookups of non-existent properties when the creator of the
        object is different than or same as the accessor, respectively. **)

    | (MixedT reason, LookupT (reason_op,Some reason_o,x,_)) ->

      if (Files_js.is_lib_file_or_flowlib_root (reason |> pos_of_reason |> Pos.filename |>
          Relative_path.to_absolute))
      then
        let msg =
            if Str.string_match (Str.regexp "\\$module__\\(.*\\)") x 0
            then "Required module not found"
            else "Unknown global name"
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
          (reason_op, reason_o)

    | (MixedT _, ExtendsT (t, tc)) ->
       let msg = "This type is incompatible with" in
        prmsg_flow cx
          Errors_js.ERROR
          trace
          msg
          (reason_of_t t, reason_of_t tc)

    (* LookupT is a latent lookup, never fired *)
    | (MixedT _, LookupT _) -> ()

    | (MixedT _, GetT(_, "statics", _)) -> ()

    | _ ->
      prerr_flow cx trace (err_msg l u) l u

  )

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
  | ObjExtendT _ -> "Expected object instead of"
  | SuperT _ -> "Cannot inherit"
  | EqT (_, t) -> spf "Non-strict equality comparison with %s may involve unintended type conversions (use strict equality test instead)" (desc_of_t t)
  | ComparatorT (_, t) -> spf "Relational comparison with %s may involve unintended type conversions" (desc_of_t t)
  | SpecializeT _ -> "Expected polymorphic type instead of"
  | LookupT _ -> "Property not found in"
  | MarkupT _ -> "Expected React class instead of"
  | KeyT _ -> "Expected object instead of"
  | HasT _ -> "Property not found in"

  (* unreachable use-types *)
  | AdderT _
  | AndT _
  | OrT _
  | PredicateT _
  | UnifyT _
  (* def-types *)
  | _
    -> assert false

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
  | _ -> false

and object_like_op = function
  | SetT _ | GetT _ | MethodT _ | LookupT _
  | KeyT _
  | ObjAssignT _ | ObjRestT _
  | SetElemT _ | GetElemT _
  | AnyObjT _ -> true
  | _ -> false

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
  let nominal = mk_var cx in
  (if modes.debug then prerr_endline
      (spf "NOM %d %s" nominal cx.file));
  nominal

and unify_map cx tmap1 tmap2 =
  tmap1 |> SMap.iter (fun x t1 ->
    let t2 = SMap.find_unsafe x tmap2 in
    unify cx t1 t2
  )

(* Indicate whether property checking should be strict for a given object and an
   operation on it. Strictness is enforced when the object is sealed (e.g., it
   is a type annotation) or when the object and the operation originate in
   different scopes. The enforcement is done via the returned "blame token" that
   is used when looking up properties of objects in the prototype chain as part
   of that operation. *)
and mk_strict sealed reason_o reason_op =
  if (not sealed && Reason_js.same_scope reason_o reason_op)
  then None
  else Some reason_o

(*****************)
(* substitutions *)
(*****************)

(* need to consider only "def" types *)

and subst cx (map: Type.t SMap.t) t =
  if SMap.is_empty map then t else match t with
  | BoundT typeparam ->
    (match SMap.get typeparam.name map with
    | None -> t
    | Some t -> t)

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
    FunT (reason, subst cx map static, subst cx map proto, {
      this_t = subst cx map this;
      params_tlist = List.map (subst cx map) params;
      params_names = names;
      return_t = subst cx map ret;
      closure_t = j;
    })

  | PolyT (xs,t) ->
      let xs, map = List.fold_left (fun (xs, map) typeparam ->
        { typeparam with bound = subst cx map typeparam.bound }::xs,
        SMap.remove typeparam.name map
      ) ([], map) xs in
      PolyT (List.rev xs, subst cx map t)

  | ObjT (reason, {
      flags;
      dict_t = { dict_name; key; value; };
      props_tmap = id;
      proto_t = proto
    }) ->
      let pmap = IMap.find_unsafe id cx.property_maps in
      ObjT (reason, {
        flags;
        dict_t = { dict_name;
                   key = subst cx map key;
                   value = subst cx map value; };
        props_tmap = mk_propmap cx (pmap |> SMap.map (subst cx map));
        proto_t = subst cx map proto
      })

  | ArrT (reason, t, ts) ->
    ArrT (reason,
          subst cx map t,
          ts |> List.map (subst cx map))

  | ClassT t -> ClassT (subst cx map t)

  | TypeT (reason, t) ->
    TypeT (reason,
           subst cx map t)

  | CustomClassT (name,ts,t) ->
    CustomClassT (
      name,
      List.map (subst cx map) ts,
      subst cx map t)

  | InstanceT (reason, static, super, instance) ->
    InstanceT (
      reason,
      subst cx map static,
      subst cx map super,
      { class_id = instance.class_id;
        type_args = instance.type_args |> SMap.map (subst cx map);
        fields_tmap = instance.fields_tmap |> SMap.map (subst cx map);
        methods_tmap = instance.methods_tmap |> SMap.map (subst cx map)
      }
    )

  | RestT (t) -> RestT (subst cx map t)

  | OptionalT (t) -> OptionalT (subst cx map t)

  | TypeAppT(c,ts) ->
    TypeAppT(subst cx map c,List.map (subst cx map) ts)

  | MaybeT(t) ->
    MaybeT(subst cx map t)

  | IntersectionT(reason, ts) ->
    IntersectionT(reason, List.map (subst cx map) ts)

  | UnionT(reason, ts) ->
    UnionT(reason, List.map (subst cx map) ts)

  | UpperBoundT(t) ->
    UpperBoundT(subst cx map t)

  | LowerBoundT(t) ->
    LowerBoundT(subst cx map t)

  | AnyObjT _ -> t

  | ShapeT(t) ->
    ShapeT(subst cx map t)

  | EnumT(reason, t) ->
    EnumT(reason, subst cx map t)

  | RecordT(reason, t) ->
    RecordT(reason, subst cx map t)

  | _ -> assert false (** TODO **)

and instantiate_poly_ cx reason_ (xs,t) ts =
  let len_xs = List.length xs in
  if len_xs <> List.length ts
  then
    let msg = spf "wrong number of type arguments (expected %d)" len_xs in
    add_error cx [reason_, msg];
    ts, AnyT reason_
  else
    let map =
      List.fold_left2
        (fun map {reason; name; bound} t ->
          unit_flow cx (t, subst cx map bound);
          SMap.add name t map
        )
        SMap.empty xs ts
    in
    ts, subst cx map t

and instantiate_poly cx reason_ (xs,t) =
  let ts = xs |> List.map (fun {reason=reason_id; _} ->
    let reason = prefix_reason (
      spf "type parameter %s of " (desc_of_reason reason_id)
    ) reason_ in
    mk_tvar cx reason
  )
  in
  instantiate_poly_ cx reason_ (xs,t) ts

and mk_object_with_proto cx reason proto =
  mk_object_with_map_proto cx reason SMap.empty proto

and mk_object_with_map_proto cx reason ?(sealed=false) map proto =
  let flags = { default_flags with sealed } in
  let dict = {
    dict_name = None;
    key = AnyT.t;
    value = AnyT.t;
  } in
  let pmap = mk_propmap cx map in
  ObjT (reason, mk_objecttype ~flags dict pmap proto)

(* Speculatively match types *)
and speculative_flow cx l u =
  throw_on_error := true;
  let result =
    try unit_flow cx (l, u); None
    with
    | FlowError msgs -> Some msgs
    | exn -> raise exn
  in
  throw_on_error := false;
  result

and speculative_match cx ts f =
  let errors = ref [] in
  let matched = ts |> List.exists (fun t ->
    match f t with
    | None -> true
    | Some error -> errors := error :: !errors; false
  ) in
  if not matched then
    !errors |> List.iter (fun msgs ->
      add_output cx Errors_js.ERROR msgs
    )

(* Force types in `ls` to be concretized. When done, the concrete types show up
   in `stack'; these then flow to types in `us`.
   Invariant: |List.rev stack| + |ls| = |us|
*)
and concretize_lower cx reason ls tl tu =
  unit_flow cx (List.hd ls, ConcretizeT(reason, List.tl ls, tl, tu))

and concretize_lower_ cx reason u =
  let ls = sufficiently_concretize cx u in
  if ls <> [] then
    concretize_lower cx reason ls AnyT.t AnyT.t

(* Force types in `us` to be concretized. When done, the concrete types show up
   in `stack'; types in `ls` then flow to these.
   Invariant: |ls| = |List.rev stack| + |us|
*)
and concretize_upper cx reason us tl tu =
  unit_flow cx (ConcretizeT(reason, List.tl us, tl, tu), List.hd us)

and concretize_upper_ cx reason l =
  let us = sufficiently_concretize cx l in
  if us <> [] then
    concretize_upper cx reason us AnyT.t AnyT.t

and sufficiently_concretize cx = function
  | FunT (reason, _, _, callt) -> callt.params_tlist
  | CallT (reason, callt) -> callt.params_tlist
  | u -> []

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

and ensure_prop_ cx trace strict mapr x proto reason_obj reason_op trace =
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

and lookup_prop cx l reason strict x t =
  let l =
    (* munge names beginning with single _ *)
    if (Str.string_match (Str.regexp_string "_") x 0) &&
      not (Str.string_match (Str.regexp_string "__") x 0)
    then MixedT (reason_of_t l)
    else l
  in
  unit_flow cx (l, LookupT (reason, strict, x, t))

and get_prop cx reason_op reason_c super x map tout =
  if SMap.mem x map
  then
    unit_flow cx (SMap.find_unsafe x map, tout)
  else
    lookup_prop cx super reason_op (Some reason_c) x (LowerBoundT tout)

and set_prop cx reason_op reason_c super x map tin =
  if SMap.mem x map
  then
    unit_flow cx (tin, SMap.find_unsafe x map)
  else
    lookup_prop cx super reason_op (Some reason_c) x (UpperBoundT tin)

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
  select_flow cx (proto, LookupT(reason_op,strict,x,t)) trace ObjProto;
  t

(* other utils *)

and filter cx trace t l pred =
  if (pred l) then flow cx (l,t) trace else ()

and is_string = function StrT _ -> true | _ -> false
and is_number = function NumT _ -> true | _ -> false
and is_function = function FunT _ -> true | _ -> false
and is_object = function (ObjT _ | ArrT _) -> true | _ -> false
and is_array = function ArrT _ -> true | _ -> false

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
  | NullT r -> NullT r
  | VoidT r -> VoidT r
  (* NOTE: it is tempting to refine optional(T) to undefined, but we don't
     because the latter is not a subtype of the former. *)
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
  (* NOTE: it is tempting to refine optional(T) to undefined, but we don't
     because the latter is not a subtype of the former. *)
  | t -> UndefT.t

and filter_not_undefined = function
  | MaybeT t ->
      let reason = reason_of_t t in
      UnionT (reason, [NullT.why reason; t])
  | OptionalT t -> filter_not_undefined t
  | UnionT (r, ts) -> recurse_into_union filter_not_undefined (r, ts)
  | VoidT r -> UndefT r
  | t -> t

and predicate cx trace t (l,p) = match (l,p) with

   (************************)
   (* deconstruction of && *)
   (************************)

  | (_, AndP(p1,p2)) ->
    let reason = new_reason "and" (pos_of_predicate p1) in
    let tvar = mk_tvar cx reason in
    flow cx (l,PredicateT(p1,tvar)) trace;
    flow cx (tvar,PredicateT(p2,t)) trace

   (************************)
   (* deconstruction of || *)
   (************************)

  | (_, OrP(p1,p2)) ->
    flow cx (l,PredicateT(p1,t)) trace;
    flow cx (l,PredicateT(p2,t)) trace

   (***********************)
   (* typeof _ ~ "string" *)
   (***********************)

  | (_, IsP "string") ->
    filter cx trace t l is_string

  | (_, NotP(IsP "string")) ->
    filter cx trace t l (not_ is_string)

   (***********************)
   (* typeof _ ~ "number" *)
   (***********************)

  | (_, IsP "number") ->
    filter cx trace t l is_number

  | (_, NotP(IsP "number")) ->
    filter cx trace t l (not_ is_number)

   (***********************)
   (* typeof _ ~ "function" *)
   (***********************)

  | (_, IsP "function") ->
    filter cx trace t l is_function

  | (_, NotP(IsP "function")) ->
    filter cx trace t l (not_ is_function)

   (***********************)
   (* typeof _ ~ "object" *)
   (***********************)

  | (_, IsP "object") ->
    filter cx trace t l is_object

  | (_, NotP(IsP "object")) ->
    filter cx trace t l (not_ is_object)

   (*******************)
   (* Array.isArray _ *)
   (*******************)

  | (_, IsP "array") ->
    filter cx trace t l is_array

  | (_, NotP(IsP "array")) ->
    filter cx trace t l (not_ is_array)

   (***********************)
   (* typeof _ ~ "undefined" *)
   (***********************)

  | (_, IsP "undefined") ->
    flow cx (filter_undefined l, t) trace

  | (_, NotP(IsP "undefined")) ->
    flow cx (filter_not_undefined l, t) trace

   (********)
   (* null *)
   (********)

  | (_, IsP "null") ->
    flow cx (filter_null l, t) trace

  | (_, NotP(IsP "null")) ->
    flow cx (filter_not_null l, t) trace

   (*********)
   (* maybe *)
   (*********)

  | (_, IsP "maybe") ->
    flow cx (filter_maybe l, t) trace

  | (_, NotP(IsP "maybe")) ->
    flow cx (filter_not_maybe l, t) trace

   (************************)
   (* truthyness *)
   (************************)

  | (_, ExistsP) ->
    flow cx (filter_exists l, t) trace

  | (_, NotP(ExistsP)) ->
    flow cx (filter_not_exists l, t) trace

   (*************************************************************)
   (* instanceof: resolve the constructor, pushing the instance *)
   (*************************************************************)

  | (_, InstanceofP (c)) ->
    flow cx (c, PredicateT(ConstructorP(l),t)) trace

  | (_, NotP(InstanceofP (c))) ->
    flow cx (c, PredicateT(NotP(ConstructorP(l)),t)) trace

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

      flow cx (u,t) trace

  | (ClassT(InstanceT _ as l),
     ConstructorP (InstanceT _ as u)) ->

      predicate cx trace t (ClassT(ExtendsT(u,l)),p)

  (** Is an instance x of C `instanceof` class A? This depends on whether C is a
      subclass of A. If it is, then the type of x is refined to C. Otherwise, do
      nothing, i.e., consider this check to fail. (Note that this is somewhat
      unsound, since C may be a superclass of the runtime class of x, so x may
      still be an instance of A; the sound alternative is to let C & A be the
      type of x, but that's hard to compute.) **)
  | (ClassT(ExtendsT(orig, InstanceT (_,static,super,instance))),
     ConstructorP (InstanceT (_,_,super_c,instance_c)))
      -> (* TODO: intersection *)

      if instance.class_id = instance_c.class_id
      then flow cx (orig, t) trace
      else flow cx (super_c, PredicateT(InstanceofP(l), t)) trace

  | (ClassT(ExtendsT _),
     ConstructorP (MixedT _)) ->
      ()

  (** Prune the type when any other `instanceof` check succeeds (since this is
      impossible). *)
  | (_, ConstructorP _) ->
      ()

  | (FunT (_,_,proto1,_),
     NotP(ConstructorP (ObjT (_,{proto_t = proto2; _}))))
      when proto1 = proto2 ->
      ()

  | (ClassT(InstanceT _ as l),
     NotP(ConstructorP (InstanceT _ as u))) ->

      predicate cx trace t (ClassT(ExtendsT(u,l)),p)

  (** Is an instance x of C not `instanceof` class A? Again, this depends on
      whether C is a subclass of A. If it is, then do nothing, since this check
      cannot succeed. Otherwise, don't refine the type of x. **)
  | (ClassT(ExtendsT(orig, InstanceT (reason,static,super,instance))),
     NotP(ConstructorP (InstanceT (reason_c,static_c,super_c,instance_c))))
      -> (* TODO: intersection *)

      if instance.class_id = instance_c.class_id
      then ()
      else flow cx (super_c, PredicateT(NotP(InstanceofP(l)), t)) trace

  | (ClassT(ExtendsT(orig, _)),
     NotP(ConstructorP (MixedT _))) ->

      flow cx (orig,t) trace

  (** Don't refine the type when any other `instanceof` check fails. **)
  | (_, NotP(ConstructorP u)) ->
      flow cx (u,t) trace

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
and goto cx ?(flag=false) id1 id2 =
  let bounds1, bounds2 =
    IMap.find_unsafe id1 cx.graph, IMap.find_unsafe id2 cx.graph
  in
  bounds1.unifier <- Some (Goto id2);
  if flag then bounds2.unifier <- Some (Rank (1 + (rank bounds2.unifier)));
  (match bounds1.solution, bounds2.solution with

  | None, None ->
    bounds1.lower |> TypeMap.iter (fun l _ ->
      bounds2.upper |> TypeMap.iter (fun u _ ->
        unit_flow cx (l,u)
      ));
    bounds2.lower |> TypeMap.iter (fun l _ ->
      bounds1.upper |> TypeMap.iter (fun u _ ->
        unit_flow cx (l,u)
      ));
    bounds1.lowertvars |> IMap.iter (fun idl trace ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.union bounds2.upper bounds_l.upper;
      bounds_l.uppertvars <- IMap.union bounds2.uppertvars bounds_l.uppertvars
    );
    bounds2.lowertvars |> IMap.iter (fun idl trace ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.union bounds1.upper bounds_l.upper;
      bounds_l.uppertvars <- IMap.union bounds1.uppertvars bounds_l.uppertvars
    );
    bounds1.uppertvars |> IMap.iter (fun idu trace ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.union bounds2.lower bounds_u.lower;
      bounds_u.lowertvars <- IMap.union bounds2.lowertvars bounds_u.lowertvars
    );
    bounds2.uppertvars |> IMap.iter (fun idu trace ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.union bounds1.lower bounds_u.lower;
      bounds_u.lowertvars <- IMap.union bounds1.lowertvars bounds_u.lowertvars
    );
    bounds2.lower <- TypeMap.union bounds1.lower bounds2.lower;
    bounds2.upper <- TypeMap.union bounds1.upper bounds2.upper;
    bounds2.lowertvars <- IMap.union bounds1.lowertvars bounds2.lowertvars;
    bounds2.uppertvars <- IMap.union bounds1.uppertvars bounds2.uppertvars

  | None, Some t2 ->
    bounds1.lower |> TypeMap.iter (fun l _ ->
      unit_flow cx (l,t2)
    );
    bounds1.upper |> TypeMap.iter (fun u _ ->
      unit_flow cx (t2,u)
    );
    bounds1.lowertvars |> IMap.iter (fun idl trace ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.add t2 trace bounds_l.upper
    );
    bounds1.uppertvars |> IMap.iter (fun idu trace ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.add t2 trace bounds_u.lower
    )

  | Some t1, None ->
    bounds2.lower |> TypeMap.iter (fun l _ ->
      unit_flow cx (l,t1)
    );
    bounds2.upper |> TypeMap.iter (fun u _ ->
      unit_flow cx (t1,u)
    );
    bounds2.lowertvars |> IMap.iter (fun idl trace ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.add t1 trace bounds_l.upper
    );
    bounds2.uppertvars |> IMap.iter (fun idu trace ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.add t1 trace bounds_u.lower
    );
    clear cx ~solution:t1 bounds2

  | Some t1, Some t2 ->
    unify cx t1 t2;
  );
  clear cx bounds1

(* Unify two type variables. This involves ensuring that they are unifiers,
   finding their roots, and making one point to the other. Ranks are used to
   keep chains short. *)
and merge_ids cx id1 id2 =
  ensure_unifier cx id1; ensure_unifier cx id2;
  let (id1, bounds1), (id2, bounds2) = find_root cx id1, find_root cx id2 in
  let r1, r2 = rank bounds1.unifier, rank bounds2.unifier in
  if id1 = id2 then ()
  else if r1 < r2 then goto cx id1 id2
  else if r2 < r1 then goto cx id2 id1
  else goto cx ~flag:true id1 id2

(* Resolve a type variable to a type. This involves ensuring that it is a
   unifier, finding its root, and resolving to that type. *)
and resolve_id cx id t =
  ensure_unifier cx id;
  let id, _ = find_root cx id in
  let bounds = IMap.find_unsafe id cx.graph in
  match bounds.solution with
  | None ->
    bounds.lower |> TypeMap.iter (fun l _ ->
      unit_flow cx (l,t)
    );
    bounds.upper |> TypeMap.iter (fun u _ ->
      unit_flow cx (t,u)
    );
    bounds.lowertvars |> IMap.iter (fun idl trace ->
      let bounds_l = find_graph cx idl in
      bounds_l.upper <- TypeMap.add t trace bounds_l.upper
    );
    bounds.uppertvars |> IMap.iter (fun idu trace ->
      let bounds_u = find_graph cx idu in
      bounds_u.lower <- TypeMap.add t trace bounds_u.lower
    );
    clear cx ~solution:t bounds
  | Some t_ ->
    unify cx t_ t

(******************)

(* TODO: Unification between concrete types is still implemented as
   bidirectional flows. This means that the destructuring work is duplicated,
   and we're missing some opportunities for nested unification. *)
and naive_unify cx t1 t2 =
  unit_flow cx (t1,t2); unit_flow cx (t2,t1)

(* Unification of two types *)
and unify cx t1 t2 =
  match (t1,t2) with
  | (OpenT (_,id1), OpenT (_,id2)) ->
    merge_ids cx id1 id2

  | (OpenT (_, id), t) | (t, OpenT (_, id)) ->
    resolve_id cx id t

  | (ArrT (_, t1, ts1), ArrT (_, t2, ts2)) ->
    array_unify cx (ts1,t1, ts2,t2)

  | (ObjT (reason1, {props_tmap = flds1; _}),
     ObjT (reason2, {props_tmap = flds2; _})) ->
    let pmap1, pmap2 =
      IMap.find_unsafe flds1 cx.property_maps,
      IMap.find_unsafe flds2 cx.property_maps
    in
    let error_ref = ref false in
    SMap.iter (fun x t ->
      match SMap.get x pmap2 with
      | Some t_ -> unify cx t t_
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
    naive_unify cx t1 t2

(* mutable sites on parent values (i.e. object properties,
   array elements) must be typed invariantly when a value
   flows to the parent, unless the incoming value is fresh,
   in which case covariant typing is sound (since no alias
   will break if the subtyped child value is replaced by a
   non-subtyped value *)
and flow_to_mutable_child cx fresh t1 t2 =
  if fresh
  then unit_flow cx (t1, t2)
  else unify cx t1 t2

and array_flow cx trace lit = function
  | ([],e1, _,e2) -> (* general element1 = general element2 *)
    (* empty array flows in: always unify *)
    unify cx e1 e2

  | (ts1,_, [],e2) -> (* specific element1 < general element2 *)
    List.iter (fun t1 ->
      select_flow cx (t1,e2) trace ArrElem;
    ) ts1

  | ([t1],_, t2::_,_) -> (* specific element1 = specific element2 *)
    flow_to_mutable_child cx lit t1 t2

  | (t1::ts1,e1, t2::ts2,e2) -> (* specific element1 = specific element2 *)
    flow_to_mutable_child cx lit t1 t2;
    array_flow cx trace lit (ts1,e1, ts2,e2)

(* array helper *)
and array_unify cx = function
  | ([],e1, [],e2) -> (* general element1 = general element2 *)
    unify cx e1 e2

  | (ts1,_, [],e2)
  | ([],e2, ts1,_) -> (* specific element1 < general element2 *)
    List.iter (fun t1 ->
      unify cx t1 e2;
    ) ts1

  | (t1::ts1,e1, t2::ts2,e2) -> (* specific element1 = specific element2 *)
    unify cx t1 t2;
    array_unify cx (ts1,e1, ts2,e2)


(*******************************************************************)
(* subtyping a sequence of arguments with a sequence of parameters *)
(*******************************************************************)

(** TODO: add arg number to FunArg trace selectors **)
and multiflow cx trace (l, u) b (arglist, parlist) =
  multiflow_helper cx trace (l, u) b (0, 0) (arglist, parlist)

and multiflow_helper cx trace (u, l) b (argi, pari) = function

  (* Do not complain on too many arguments.
     This pattern is ubiqutous and causes a lot of noise when complained about.
     Note: optional/rest parameters do not provide a workaround in this case.
  *)
  | (_,[]) -> []

  | ([],(OptionalT t)::touts) -> (OptionalT t)::touts

  | ([RestT tin],(OptionalT tout)::touts) ->
    select_flow cx (tin, tout) trace (FunArg (argi, pari));
    multiflow_helper cx trace (u, l) b (argi, pari + 1) ([RestT tin],touts)

  | ((OptionalT tin)::tins,[RestT tout]) ->
    select_flow cx (tin,tout) trace (FunArg (argi, pari));
    multiflow_helper cx trace (u, l) b (argi + 1, pari) (tins,[RestT tout])

  | ((OptionalT tin)::tins,(OptionalT tout)::touts)
  | (tin::tins,(OptionalT tout)::touts) ->
    select_flow cx (tin,tout) trace (FunArg (argi, pari));
    multiflow_helper cx trace (u, l) b (argi + 1, pari + 1) (tins,touts)

  | ([RestT tin],[RestT tout]) ->
    select_flow cx (tin,tout) trace (FunArg (argi, pari));
    []

  | (tins,[RestT tout]) ->
    let n = ref 0 in
    tins |> List.iter (fun tin ->
      select_flow cx (tin,tout) trace (FunArg (argi + !n, pari))
    );
    [RestT tout]

  | ([],ts) ->
    (if b
     then
        prerr_flow cx trace "Too few arguments (expected default/rest parameters in function)" u l
     else ()
    );
    ts

  | (tin::tins,tout::touts) ->
    select_flow cx (tin,tout) trace (FunArg (argi, pari));
    multiflow_helper cx trace (u, l) b (argi + 1, pari + 1) (tins,touts)

(* builtins, contd. *)

and get_builtin cx x reason =
  mk_tvar_where cx reason (fun builtin ->
    unit_flow cx (builtins, GetT(reason,x,builtin))
  )

and lookup_builtin cx x reason strict builtin =
  unit_flow cx (builtins, LookupT(reason,strict,x,builtin))

and mk_instance cx instance_reason c =
  mk_annot cx instance_reason c

and mk_typeapp_instance cx reason x ts =
  TypeAppT(get_builtin cx x reason, ts)

and mk_annot cx instance_reason c =
  mk_tvar_derivable_where cx instance_reason (fun t ->
    unit_flow cx (c, TypeT(instance_reason,t))
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
    unit_flow cx (cls, MethodT(reason, m, cls, argts, tvar, 0));
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
  unit_flow cx (builtins, SetT(reason,x,t))

and select_flow cx (t1, t2) trace rule =
  flow cx (t1, t2) (select_trace t1 t2 trace rule)

and unit_flow cx (t1, t2) =
  flow cx (t1, t2) (unit_trace t1 t2)

and module_t cx m reason =
  match SMap.get m cx.modulemap with
  | Some t -> t
  | None ->
      mk_tvar_where cx reason (fun t ->
        cx.modulemap <- cx.modulemap |> SMap.add m t;
      )

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
      gc_undirected cx objtype.dict_t.key;
      gc_undirected cx objtype.dict_t.value;
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
      instance.fields_tmap |> SMap.iter (fun _ -> gc_undirected cx);
      instance.methods_tmap |> SMap.iter (fun _ -> gc_undirected cx);
      gc cx polarity static;
      gc cx polarity super

  | TypeT(_, t) ->
      gc_undirected cx t

  | SuperT(_, instance) ->
      instance.fields_tmap |> SMap.iter (fun _ -> gc_undirected cx);
      instance.methods_tmap |> SMap.iter (fun _ -> gc cx (not polarity))

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

  | MarkupT(_,o,t) ->
      gc cx (not polarity) o;
      gc cx polarity t

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
      gc_undirected cx objtype.dict_t.key;
      gc_undirected cx objtype.dict_t.value;
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
      instance.fields_tmap |> SMap.iter (fun _ -> gc_undirected cx);
      instance.methods_tmap |> SMap.iter (fun _ -> gc_undirected cx);
      gc_undirected cx static;
      gc_undirected cx super

  | TypeT(_, t) ->
      gc_undirected cx t

  | SuperT(_, instance) ->
      instance.fields_tmap |> SMap.iter (fun _ -> gc_undirected cx);
      instance.methods_tmap |> SMap.iter (fun _ -> gc_undirected cx)

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

  | MarkupT(_,o,t) ->
      gc_undirected cx o;
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
  (if modes.debug then prerr_endline (spf "DEAD: %d" tvar));
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
          (if modes.debug then prerr_endline (spf "LIVE: %d" tvar))
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
      let super_t = resolve_type cx super in
      let members = SMap.union fields methods in
      let super_flds = extract_members cx super_t in
      SMap.union super_flds members
  | ObjT (_, {props_tmap = flds; proto_t = proto; _}) ->
      let proto_t = resolve_type cx proto in
      let prot_members = extract_members cx proto_t in
      let members = IMap.find_unsafe flds cx.property_maps in
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
