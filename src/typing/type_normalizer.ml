(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Reason_js
open Type
open Utils_js

let suggested_type_cache = ref IMap.empty

let fake_fun params_names param_ts ret_t =
  let reason = reason_of_string "function" in
  FunT (
    reason,
    Flow_js.dummy_static reason,
    Flow_js.dummy_prototype,
    Flow_js.mk_functiontype param_ts ?params_names ret_t
  )

let fake_instance name =
  let insttype = {
    class_id = 0;
    type_args = SMap.empty;
    arg_polarities = SMap.empty;
    fields_tmap = 0;
    methods_tmap = 0;
    mixins = false;
    structural = false;
  } in
  InstanceT (
    reason_of_string name,
    MixedT (reason_of_string "dummy static", Mixed_everything),
    MixedT (reason_of_string "dummy super", Mixed_everything),
    insttype
  )

(* This function does not only resolve every OpenT recursively, but also
   replaces the reasons of types with a uniform ones. It is a left-over bit
   from the old normalize_type_impl behavior. *)
let rec normalize_type_impl cx ids t = match t with
  | BoundT _ -> t
  | OpenT (_, id) ->
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
      let tins = List.map (normalize_type_impl cx ids) ft.params_tlist in
      let params_names = ft.params_names in
      let tout = normalize_type_impl cx ids ft.return_t in
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
      fake_fun params_names tins any

  (* Fake the signature of Function.prototype.bind: *)
  (* (thisArg: any, ...argArray: Array<any>): any *)
  | FunProtoBindT _ ->
      let any = AnyT (reason_of_string "any") in
      let tins = [any; RestT any] in
      let params_names = Some ["thisArg"; "argArray"] in
      fake_fun params_names tins any

  (* Fake the signature of Function.prototype.call: *)
  (* (thisArg: any, ...argArray: Array<any>): any *)
  | FunProtoCallT _ ->
      let any = AnyT (reason_of_string "any") in
      let tins = [any; RestT any] in
      let params_names = Some ["thisArg"; "argArray"] in
      fake_fun params_names tins any

  (* Fake the signature of $Facebookism$Merge: *)
  (* (...objects: Array<Object>): Object *)
  | CustomFunT (_, Merge) ->
      let obj = AnyObjT (reason_of_string "object type") in
      let tins = [RestT obj] in
      let params_names = Some ["objects"] in
      fake_fun params_names tins obj

  (* Fake the signature of $Facebookism$MergeDeepInto: *)
  (* (target: Object, ...objects: Array<Object>): void *)
  | CustomFunT (_, MergeDeepInto) ->
      let obj = AnyObjT (reason_of_string "object type") in
      let void = VoidT (reason_of_string "void") in
      let tins = [obj; RestT obj] in
      let params_names = Some ["target"; "objects"] in
      fake_fun params_names tins void

  (* Fake the signature of $Facebookism$MergeInto: *)
  (* (target: Object, ...objects: Array<Object>): void *)
  | CustomFunT (_, MergeInto) ->
      let obj = AnyObjT (reason_of_string "object type") in
      let void = VoidT (reason_of_string "void") in
      let tins = [obj; RestT obj] in
      let params_names = Some ["target"; "objects"] in
      fake_fun params_names tins void

  (* Fake the signature of $Facebookism$Mixin: *)
  (* (...objects: Array<Object>): Class *)
  | CustomFunT (_, Mixin) ->
      let obj = AnyObjT (reason_of_string "object type") in
      let tout = ClassT obj in
      let tins = [RestT obj] in
      let params_names = Some ["objects"] in
      fake_fun params_names tins tout

  (* Fake the signature of Object.assign:
     (target: any, ...sources: Array<any>): any *)
  | CustomFunT (_, ObjectAssign) ->
      let any = AnyT (reason_of_string "any") in
      let tins = [any; RestT any] in
      let params_names = Some ["target"; "sources"] in
      fake_fun params_names tins any

  (* Fake the signature of Object.getPrototypeOf:
     (o: any): any *)
  | CustomFunT (_, ObjectGetPrototypeOf) ->
      let any = AnyT (reason_of_string "any") in
      let tins = [any] in
      let params_names = Some ["o"] in
      fake_fun params_names tins any

  (* Fake the signature of Promise.all:
     (promises: Array<Promise>): Promise *)
  | CustomFunT (_, PromiseAll) ->
      let param_names = Some ["promises"] in
      let promise = fake_instance "Promise" in
      let promises = ArrT (
        reason_of_string "promises",
        promise,
        []
      ) in
      fake_fun param_names [promises] promise

  | ObjT (_, ot) ->
      let dict = match ot.dict_t with
        | None -> None
        | Some dict ->
            Some { dict with
              key = (normalize_type_impl cx ids dict.key);
              value = (normalize_type_impl cx ids dict.value);
            }
      in
      let pmap =
        Context.find_props cx ot.props_tmap
        |> SMap.map (normalize_type_impl cx ids)
        |> Context.make_property_map cx
      in
      let proto = AnyT.t in
      ObjT (
        reason_of_string "object",
        Flow_js.mk_objecttype dict pmap proto
      )

  | ArrT (_, t, ts) ->
      ArrT (reason_of_string "array",
            normalize_type_impl cx ids t,
            ts |> List.map (normalize_type_impl cx ids))

  | MaybeT t ->
      let t = normalize_type_impl cx ids t in
      (match t with
      | MaybeT _ -> t
      | _ -> MaybeT t)

  | PolyT (xs, t) ->
      PolyT (xs, normalize_type_impl cx ids t)

  | ClassT t ->
      ClassT (normalize_type_impl cx ids t)

  | ThisClassT t ->
      ClassT (normalize_type_impl cx ids t)

  | TypeT (reason, t) ->
      let reason = reason_of_string (desc_of_reason reason) in
      TypeT (reason, normalize_type_impl cx ids t)

  | InstanceT _ ->
      t (* nominal type *)

  | RestT t ->
      RestT (normalize_type_impl cx ids t)

  | OptionalT t ->
      OptionalT (normalize_type_impl cx ids t)

  | TypeAppT (c, ts) ->
      let c = normalize_type_impl cx ids c in
      let ts = List.map (normalize_type_impl cx ids) ts in
      TypeAppT (c, ts)

  | ThisTypeAppT (c, this, ts) ->
      let c = normalize_type_impl cx ids c in
      let this = normalize_type_impl cx ids this in
      let ts = List.map (normalize_type_impl cx ids) ts in
      ThisTypeAppT (c, this, ts)

  | IntersectionT (_, rep) ->
      let reason = reason_of_string "intersection" in
      let rep = InterRep.map (normalize_type_impl cx ids) rep in
      normalize_intersection reason rep

  | UnionT (_, rep) ->
      let reason = reason_of_string "union" in
      let rep = UnionRep.map (normalize_type_impl cx ids) rep in
      normalize_union reason rep

  | AnyWithUpperBoundT t ->
      AnyWithUpperBoundT (normalize_type_impl cx ids t)

  | AnyWithLowerBoundT t ->
      AnyWithLowerBoundT (normalize_type_impl cx ids t)

  | AnyObjT _ -> AnyObjT (reason_of_string "any object")
  | AnyFunT _ -> AnyFunT (reason_of_string "any function")

  | ShapeT t ->
      ShapeT (normalize_type_impl cx ids t)
  | DiffT (t1, t2) ->
      DiffT (normalize_type_impl cx ids t1, normalize_type_impl cx ids t2)

  | AnnotT (t1, t2) ->
      AnnotT (normalize_type_impl cx ids t1, normalize_type_impl cx ids t2)

  | KeysT (_, t) ->
      KeysT (reason_of_string "key set", normalize_type_impl cx ids t)

  | AbstractT t ->
      AbstractT (normalize_type_impl cx ids t)

  | EvalT (_, _, id) ->
      let evaluated = Context.evaluated cx in
      begin match IMap.get id evaluated with
      | Some t -> normalize_type_impl cx ids t
      | None ->
        (* this happens when, for example, the RHS of a destructuring is
           unconstrained, so we never evaluate the destructuring. so, make the
           destructured value also unconstrained... *)
        EmptyT.t
      end

  | FunProtoT _
  | ExistsT _
  | ModuleT (_, _)
  | SpeculativeMatchT (_, _, _)
  | ReposUpperT (_, _)
  | ExtendsT (_, _, _) ->
    (** TODO **)
    failwith (spf "Unsupported type in normalize_type_impl: %s" (string_of_ctor t))

and lookup_type_ cx ids id =
  if ISet.mem id ids then assert false
  else
    let ids = ISet.add id ids in
    let types = Flow_js.possible_types cx id in
    try
      List.fold_left
        (fun u t -> Flow_js.merge_type cx (normalize_type_impl cx ids t, u))
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


(* TODO: This is not an exhaustive list of normalization steps for unions.
   For example, we might want to get rid of AnyT in the union similar to how
   merge_type gets rid of AnyT. Decide on rules like these and implement them
   if required. *)
and normalize_union r rep =
  let ts = UnionRep.members rep in
  let ts = collect_union_members ts in
  let (ts, has_void, has_null) =
    TypeSet.fold (fun t (ts, has_void, has_null) ->
      match t with
      | MaybeT (UnionT (_, rep)) ->
          let tlist = UnionRep.members rep in
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
    | _ -> UnionT (r, UnionRep.make ts)
  in
  if has_void && has_null
  then MaybeT t
  else t

and collect_union_members ts =
  List.fold_left (fun acc x ->
      match x with
      | UnionT (_, rep) ->
        let ts = UnionRep.members rep in
        TypeSet.union (collect_union_members ts) acc
      | _ ->
        TypeSet.add x acc
    ) TypeSet.empty ts

(* TODO: This does not do any real normalization yet, it only flattens the
   intesection. Think about normalization rules and implement them when there
   is need for that. *)
and normalize_intersection r rep =
  let ts = InterRep.members rep in
  let ts = collect_intersection_members ts in
  let ts = TypeSet.elements ts in
  match ts with
  | [t] -> t
  | _ -> IntersectionT (r, InterRep.make ts)

and collect_intersection_members ts =
  List.fold_left (fun acc x ->
      match x with
      | IntersectionT (_, rep) ->
        let ts = InterRep.members rep in
        TypeSet.union acc (collect_intersection_members ts)
      | _ ->
        TypeSet.add x acc
    ) TypeSet.empty ts


let normalize_type cx t =
  normalize_type_impl cx ISet.empty t
