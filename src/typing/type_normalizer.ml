(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Reason_js
open Type
open Utils

let suggested_type_cache = ref IMap.empty

(* This function does not only resolve every OpenT recursively, but also
   replaces the reasons of types with a uniform ones. It is a left-over bit
   from the old ground_type behavior. *)
let rec ground_type_impl cx ids t = match t with
  | BoundT _ -> t
  | OpenT (reason, id) ->
      lookup_type cx ids id

  | NumT _ -> NumT.t
  | StrT _ -> StrT.t
  | BoolT _ -> BoolT.t
  | EmptyT _ -> EmptyT.t
  | NullT _ -> NullT.t
  | VoidT _ -> VoidT.t
  | MixedT _ -> MixedT.t
  | AnyT _ -> AnyT.t

  | TaintT _ -> TaintT (reason_of_string "taint")

  | SingletonStrT (_, s) ->
    SingletonStrT (reason_of_string "string singleton", s)
  | SingletonNumT (_, n) ->
    SingletonNumT (reason_of_string "number singleton", n)
  | SingletonBoolT (_, b) ->
    SingletonBoolT (reason_of_string "boolean singleton", b)

  | FunT (_, _, _, ft) ->
      let tins = List.map (ground_type_impl cx ids) ft.params_tlist in
      let params_names = ft.params_names in
      let tout = ground_type_impl cx ids ft.return_t in
      let reason = reason_of_string "function" in
      FunT (
        reason,
        Flow_js.dummy_static reason,
        Flow_js.dummy_prototype,
        Flow_js.mk_functiontype tins ?params_names tout
      )

  (* Fake the signature of Function.prototype.apply: *)
  (* (thisArg: any, argArray?: any): any *)
  | FunProtoApplyT _ ->
      let any = AnyT (reason_of_string "any") in
      let tins = [any; OptionalT any] in
      let params_names = Some ["thisArg"; "argArray"] in
      let reason = reason_of_string "function" in
      FunT (
        reason,
        Flow_js.dummy_static reason,
        Flow_js.dummy_prototype,
        Flow_js.mk_functiontype tins ?params_names any
      )

  (* Fake the signature of Function.prototype.bind: *)
  (* (thisArg: any, ...argArray: Array<any>): any *)
  | FunProtoBindT _ ->
      let any = AnyT (reason_of_string "any") in
      let tins = [any; RestT any] in
      let params_names = Some ["thisArg"; "argArray"] in
      let reason = reason_of_string "function" in
      FunT (
        reason,
        Flow_js.dummy_static reason,
        Flow_js.dummy_prototype,
        Flow_js.mk_functiontype tins ?params_names any
      )

  (* Fake the signature of Function.prototype.call: *)
  (* (thisArg: any, ...argArray: Array<any>): any *)
  | FunProtoCallT _ ->
      let any = AnyT (reason_of_string "any") in
      let tins = [any; RestT any] in
      let params_names = Some ["thisArg"; "argArray"] in
      let reason = reason_of_string "function" in
      FunT (
        reason,
        Flow_js.dummy_static reason,
        Flow_js.dummy_prototype,
        Flow_js.mk_functiontype tins ?params_names any
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
        Context.find_props cx ot.props_tmap
        |> SMap.map (ground_type_impl cx ids)
        |> Context.make_property_map cx
      in
      let proto = AnyT.t in
      ObjT (
        reason_of_string "object",
        Flow_js.mk_objecttype dict pmap proto
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

  | ThisClassT t ->
      ClassT (ground_type_impl cx ids t)

  | TypeT (reason, t) ->
      let reason = reason_of_string (desc_of_reason reason) in
      TypeT (reason, ground_type_impl cx ids t)

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

  | ThisTypeAppT (c, this, ts) ->
      let c = ground_type_impl cx ids c in
      let this = ground_type_impl cx ids this in
      let ts = List.map (ground_type_impl cx ids) ts in
      ThisTypeAppT (c, this, ts)

  | IntersectionT (_, ts) ->
      IntersectionT (
        reason_of_string "intersection",
        List.map (ground_type_impl cx ids) ts
      )

  | UnionT (_, ts) ->
      UnionT (
        reason_of_string "union",
        List.map (ground_type_impl cx ids) ts
      )

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

  | AnnotT (t1, t2) ->
      AnnotT (ground_type_impl cx ids t1, ground_type_impl cx ids t2)

  | KeysT (_, t) ->
      KeysT (reason_of_string "key set", ground_type_impl cx ids t)

  | _ ->
    (** TODO **)
    failwith (spf "Unsupported type in ground_type_impl: %s" (string_of_ctor t))

and lookup_type_ cx ids id =
  if ISet.mem id ids then assert false
  else
    let ids = ISet.add id ids in
    let types = Flow_js.possible_types cx id in
    try
      List.fold_left
        (fun u t -> Flow_js.merge_type cx (ground_type_impl cx ids t, u))
        EmptyT.t types
    with _ ->
      AnyT.t

and lookup_type cx ids id =
  match IMap.get id !suggested_type_cache with
  | None ->
      let t = lookup_type_ cx ids id in
      suggested_type_cache := !suggested_type_cache |> IMap.add id t;
      t
  | Some t -> t


let ground_type cx type_ =
  ground_type_impl cx ISet.empty type_


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
        Context.find_props cx ot.props_tmap
        |> SMap.map (normalize_type cx)
        |> Context.make_property_map cx
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

  | ThisTypeAppT (c, this, ts) ->
      ThisTypeAppT (
        normalize_type cx c,
        normalize_type cx this,
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
      (* TODO: We should only get EmptyT here when a completely open type
         variable has been in the union before grounding it. This happens when
         "null" is passed to a function parameter. We throw this out because
         it gives no information at all. merge_type also ignores EmptyT. *)
      | EmptyT _ -> (ts, has_void, has_null)
      | _ -> (TypeSet.add t ts, has_void, has_null)
    ) ts (TypeSet.empty, false, false) in
  let ts =
    match (has_void, has_null) with
    | (true, false) -> TypeSet.add VoidT.t ts
    | (false, true) -> TypeSet.add NullT.t ts
    | _ ->
        (* We should never get an empty set at this point but better safe than
           sorry. Stripping out EmptyT above might be unsafe. *)
        if TypeSet.is_empty ts
        then TypeSet.singleton EmptyT.t
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
        Context.find_props cx ot.props_tmap
        |> SMap.map (printify_type cx)
        |> Context.make_property_map cx
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

  | ThisTypeAppT (c, this, ts) ->
      ThisTypeAppT (
        printify_type cx c,
        printify_type cx this,
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

  | t -> t

let printified_type cx t =
  let t = ground_type cx t in
  let t = normalize_type cx t in
  printify_type cx t
