(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *)

open Reason
open Type
open Utils_js

let suggested_type_cache = ref IMap.empty

let fake_fun params_names param_ts rest_param ret_t =
  let reason = locationless_reason (RFunction RNormal) in
  let rest_param = Option.map
    ~f:(fun (name, t) -> Some name, Loc.none, t)
    rest_param in
  let def_reason = reason in
  DefT (reason, FunT (
    Flow_js.dummy_static reason,
    Flow_js.dummy_prototype,
    Flow_js.mk_functiontype param_ts ~rest_param ~def_reason ?params_names ret_t
  ))

let fake_instance name =
  let insttype = {
    class_id = 0;
    type_args = SMap.empty;
    arg_polarities = SMap.empty;
    fields_tmap = Properties.fake_id;
    initialized_field_names = SSet.empty;
    methods_tmap = Properties.fake_id;
    mixins = false;
    structural = false;
  } in
  DefT (locationless_reason (RCustom name), InstanceT (
    ObjProtoT (locationless_reason (RCustom "dummy static")),
    ObjProtoT (locationless_reason (RCustom "dummy super")),
    [],
    insttype
  ))

(* This function does not only resolve every OpenT recursively, but also
   replaces the reasons of types with a uniform ones. It is a left-over bit
   from the old normalize_type_impl behavior. *)
let rec normalize_type_impl cx ids t = match t with
  | BoundT _ -> t
  | OpenT (_, id) ->
      lookup_type cx ids id

  | DefT (_, NumT _) -> Locationless.NumT.t
  | DefT (_, StrT _) -> Locationless.StrT.t
  | DefT (_, BoolT _) -> Locationless.BoolT.t
  | DefT (_, EmptyT) -> Locationless.EmptyT.t
  | DefT (_, NullT) -> Locationless.NullT.t
  | DefT (_, VoidT) -> Locationless.VoidT.t
  | DefT (_, MixedT _) -> Locationless.MixedT.t
  | DefT (_, AnyT) -> Locationless.AnyT.t

  | TaintT _ -> TaintT (locationless_reason (RCustom "taint"))

  | ExistsT _ -> ExistsT (locationless_reason (RCustom "exists"))

  | DefT (_, SingletonStrT s) ->
    DefT (locationless_reason (RCustom "string singleton"), SingletonStrT s)
  | DefT (_, SingletonNumT n) ->
    DefT (locationless_reason (RCustom "number singleton"), SingletonNumT n)
  | DefT (_, SingletonBoolT b) ->
    DefT (locationless_reason (RCustom "boolean singleton"), SingletonBoolT b)

  | DefT (_, FunT (_, _, ft)) ->
      let tins = List.map (normalize_type_impl cx ids) ft.params_tlist in
      let rest_param = Option.map
        ~f:(fun (name, loc, t) -> name, loc, normalize_type_impl cx ids t)
        ft.rest_param in
      let params_names = ft.params_names in
      let tout = normalize_type_impl cx ids ft.return_t in
      let reason = locationless_reason (RFunction RNormal) in
      let is_predicate = Some ft.is_predicate in
      let def_reason = ft.def_reason in
      DefT (reason, FunT (
        Flow_js.dummy_static reason,
        Flow_js.dummy_prototype,
        Flow_js.mk_functiontype
          tins ~rest_param ~def_reason ?params_names ?is_predicate tout
      ))

  (* Fake the signature of Function.prototype.apply: *)
  (* (thisArg: any, argArray?: any): any *)
  | FunProtoApplyT _ ->
      let any = DefT (locationless_reason RAny, AnyT) in
      let tins = [any; optional any] in
      let params_names = Some ["thisArg"; "argArray"] in
      fake_fun params_names tins None any

  (* Fake the signature of Function.prototype.bind: *)
  (* (thisArg: any, ...argArray: Array<any>): any *)
  | FunProtoBindT _ ->
      let any = DefT (locationless_reason RAny, AnyT) in
      let arr = DefT (locationless_reason RArray, ArrT (ArrayAT(any, None))) in
      let tins = [any] in
      let rest_param = Some ("argArray", arr) in
      let params_names = Some ["thisArg"] in
      fake_fun params_names tins rest_param any

  (* Fake the signature of Function.prototype.call: *)
  (* (thisArg: any, ...argArray: Array<any>): any *)
  | FunProtoCallT _ ->
      let any = DefT (locationless_reason RAny, AnyT) in
      let arr = DefT (locationless_reason RArray, ArrT (ArrayAT(any, None))) in
      let tins = [any] in
      let rest_param = Some ("argArray", arr) in
      let params_names = Some ["thisArg"] in
      fake_fun params_names tins rest_param any

  | ChoiceKitT (_, _) ->
      Locationless.AnyT.t

  (* Fake the signature of $Facebookism$Merge: *)
  (* (...objects: Array<Object>): Object *)
  | CustomFunT (_, Merge) ->
      let obj = DefT (locationless_reason RObjectType, AnyObjT) in
      let arr = DefT (locationless_reason RArray, ArrT (ArrayAT(obj, None))) in
      let tins = [] in
      let rest_param = Some ("objects", arr) in
      let params_names = Some [] in
      fake_fun params_names tins rest_param obj

  (* Fake the signature of $Facebookism$MergeDeepInto: *)
  (* (target: Object, ...objects: Array<Object>): void *)
  | CustomFunT (_, MergeDeepInto) ->
      let obj = DefT (locationless_reason RObjectType, AnyObjT) in
      let arr = DefT (locationless_reason RArray, ArrT (ArrayAT(obj, None))) in
      let void = DefT (locationless_reason RVoid, VoidT) in
      let tins = [obj] in
      let rest_param = Some ("objects", arr) in
      let params_names = Some ["target"] in
      fake_fun params_names tins rest_param void

  (* Fake the signature of $Facebookism$MergeInto: *)
  (* (target: Object, ...objects: Array<Object>): void *)
  | CustomFunT (_, MergeInto) ->
      let obj = DefT (locationless_reason RObjectType, AnyObjT) in
      let arr = DefT (locationless_reason RArray, ArrT (ArrayAT(obj, None))) in
      let void = DefT (locationless_reason RVoid, VoidT) in
      let tins = [obj] in
      let rest_param = Some ("objects", arr) in
      let params_names = Some ["target"] in
      fake_fun params_names tins rest_param void

  (* Fake the signature of $Facebookism$Mixin: *)
  (* (...objects: Array<Object>): Class *)
  | CustomFunT (_, Mixin) ->
      let obj = DefT (locationless_reason RObjectType, AnyObjT) in
      let arr = DefT (locationless_reason RArray, ArrT (ArrayAT(obj, None))) in
      let tout = class_type obj in
      let tins = [] in
      let rest_param = Some ("objects", arr) in
      let params_names = Some [] in
      fake_fun params_names tins rest_param tout

  (* Fake the signature of Object.assign:
     (target: any, ...sources: Array<any>): any *)
  | CustomFunT (_, ObjectAssign) ->
      let any = DefT (locationless_reason RAny, AnyT) in
      let arr = DefT (locationless_reason RArray, ArrT (ArrayAT(any, None))) in
      let tins = [any] in
      let rest_param = Some ("sources", arr) in
      let params_names = Some ["target"] in
      fake_fun params_names tins rest_param any

  (* Fake the signature of Object.getPrototypeOf:
     (o: any): any *)
  | CustomFunT (_, ObjectGetPrototypeOf) ->
      let any = DefT (locationless_reason RAny, AnyT) in
      let tins = [any] in
      let params_names = Some ["o"] in
      fake_fun params_names tins None any

  | CustomFunT (reason, Idx) ->
      let obj_param = (
        let obj_name = "IdxObject" in
        let obj_reason = locationless_reason (RCustom obj_name) in
        BoundT {
          reason = obj_reason;
          name = obj_name;
          bound = DefT (locationless_reason RObjectType, AnyObjT);
          polarity = Neutral;
          default = None;
        }
      ) in

      let cb_ret = (
        let cb_ret_name = "IdxResult" in
        let cb_ret_reason = locationless_reason (RCustom cb_ret_name) in
        BoundT {
          reason = cb_ret_reason;
          name = cb_ret_name;
          bound = DefT (reason, MixedT Mixed_everything);
          polarity = Neutral;
          default = None;
        }
      ) in

      let cb_param = (
        let cb_param = IdxWrapper (reason, obj_param) in
        fake_fun (Some ["demaybifiedObj"]) [cb_param] None cb_ret
      ) in

      let tins = [obj_param; cb_param] in
      let param_names = Some ["obj"; "pathCallback"] in
      let maybe_ret =
        let reason = reason_of_t cb_ret in
        let reason = replace_reason (fun desc -> RMaybe desc) reason in
        DefT (reason, MaybeT cb_ret)
      in
      fake_fun param_names tins None maybe_ret

  | CustomFunT (_, DebugPrint) ->
      let rest_param = Some (
        "_",
        DefT (locationless_reason RArray, ArrT (ArrayAT(Locationless.AnyT.t, None)))
      ) in
      fake_fun None [] rest_param Locationless.VoidT.t

  | CustomFunT (_, ReactPropType _) ->
    Locationless.AnyT.t (* TODO *)

  (* (spec: any) => ReactClass<any> *)
  | CustomFunT (_, ReactCreateClass) ->
      let component_class =
        let instance = fake_instance "ReactClass" in
        typeapp (class_type instance) [Locationless.AnyT.t]
      in
      fake_fun (Some ["spec"]) [Locationless.AnyT.t] None component_class

  (* Fake the signature of React.createElement (overloaded)
     1. Component class
       <T>(name: ReactClass<T>, config: T, children?: any) => React$Element<T>
     2. Stateless functional component
       type SFC<T> = (config: T, context: any) => React$Element<T>
       <T>(fn: SFC<T>, config: T, children?: any) => React$Element<T>
     3. $JSXIntrinsics
       (no reasonable signature for this) *)
  | CustomFunT (_, ReactCreateElement) ->
      let config_name = "Config" in
      let config_tp =
        let reason = locationless_reason (RCustom config_name) in
        {
          reason;
          name = config_name;
          bound = DefT (reason, MixedT Mixed_everything);
          polarity = Neutral;
          default = None;
        }
      in
      let config = BoundT config_tp in
      let any = DefT (locationless_reason RAny, AnyT) in
      let react_element =
        let instance = fake_instance "React$Element" in
        typeapp (poly_type [config_tp] (class_type instance)) [config]
      in
      let component_class =
        let instance = fake_instance "ReactClass" in
        typeapp (poly_type [config_tp] (class_type instance)) [config]
      in
      let stateless_functional_component =
        let params_names = Some ["config"; "context"] in
        let param_ts = [config; any] in
        fake_fun params_names param_ts None react_element
      in
      let t1 =
        let params_names = Some ["name"; "config"; "children"] in
        let param_ts = [component_class; config; any] in
        poly_type [config_tp] (fake_fun params_names param_ts None react_element)
      in
      let t2 =
        let params_names = Some ["fn"; "config"; "children"] in
        let param_ts = [stateless_functional_component; config; any] in
        poly_type [config_tp] (fake_fun params_names param_ts None react_element)
      in
      DefT (locationless_reason RIntersectionType,
        IntersectionT (InterRep.make t1 t2 [])
      )

  | IdxWrapper (_, obj) ->
    let reason = locationless_reason (RCustom "idx object") in
    IdxWrapper (reason, normalize_type_impl cx ids obj)

  | DefT (_, ObjT ot) ->
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
        |> Properties.map_t (normalize_type_impl cx ids)
        |> Context.make_property_map cx
      in
      let proto = Locationless.AnyT.t in
      DefT (locationless_reason RObject,
        ObjT (Flow_js.mk_objecttype dict pmap proto)
      )

  | DefT (_, ArrT (ArrayAT (elemt, tuple_types))) ->
      DefT (locationless_reason RArray, ArrT (
        ArrayAT (
          normalize_type_impl cx ids elemt,
          Option.map
            ~f:(List.map (normalize_type_impl cx ids))
            tuple_types
        )
      ))

  | DefT (_, ArrT (TupleAT (elemt, tuple_types))) ->
      DefT (locationless_reason RTupleType, ArrT (
        TupleAT (
          normalize_type_impl cx ids elemt,
          List.map (normalize_type_impl cx ids) tuple_types
        )
      ))

  | DefT (_, ArrT (ROArrayAT (elemt))) ->
      DefT (locationless_reason RROArrayType, ArrT (
        ROArrayAT (
          normalize_type_impl cx ids elemt
        )
      ))

  | DefT (_, ArrT EmptyAT) ->
      DefT (locationless_reason RArray, ArrT EmptyAT)

  | ExactT (reason, t) ->
      let reason = locationless_reason (desc_of_reason reason) in
      ExactT (reason, normalize_type_impl cx ids t)

  | DefT (reason, MaybeT t) ->
      let reason = locationless_reason (desc_of_reason reason) in
      let t = normalize_type_impl cx ids t in
      (match t with
      | DefT (_, MaybeT _) -> t
      | _ -> DefT (reason, MaybeT t))

  | DefT (reason, PolyT (xs, t)) ->
      let reason = locationless_reason (desc_of_reason reason) in
      DefT (reason, PolyT (xs, normalize_type_impl cx ids t))

  | DefT (reason, ClassT t) ->
      let reason = locationless_reason (desc_of_reason reason) in
      DefT (reason, ClassT (normalize_type_impl cx ids t))

  | ThisClassT (reason, t) ->
      let reason = locationless_reason (desc_of_reason reason) in
      ThisClassT (reason, normalize_type_impl cx ids t)

  | DefT (reason, TypeT t) ->
      let reason = locationless_reason (desc_of_reason reason) in
      DefT (reason, TypeT (normalize_type_impl cx ids t))

  | DefT (_, InstanceT _) ->
      t (* nominal type *)

  | DefT (reason, OptionalT t) ->
      let reason = locationless_reason (desc_of_reason reason) in
      DefT (reason, OptionalT (normalize_type_impl cx ids t))

  | DefT (reason, TypeAppT (c, ts)) ->
      let reason = locationless_reason (desc_of_reason reason) in
      let c = normalize_type_impl cx ids c in
      let ts = List.map (normalize_type_impl cx ids) ts in
      DefT (reason, TypeAppT (c, ts))

  | ThisTypeAppT (reason, c, this, ts) ->
      let reason = locationless_reason (desc_of_reason reason) in
      let c = normalize_type_impl cx ids c in
      let this = normalize_type_impl cx ids this in
      let ts = List.map (normalize_type_impl cx ids) ts in
      ThisTypeAppT (reason, c, this, ts)

  | DefT (_, IntersectionT rep) ->
      let reason = locationless_reason RIntersection in
      let rep = InterRep.map (normalize_type_impl cx ids) rep in
      normalize_intersection reason rep

  | DefT (_, UnionT rep) ->
      let reason = locationless_reason RUnion in
      let rep = UnionRep.map (normalize_type_impl cx ids) rep in
      normalize_union reason rep

  | AnyWithUpperBoundT t ->
      AnyWithUpperBoundT (normalize_type_impl cx ids t)

  | AnyWithLowerBoundT t ->
      AnyWithLowerBoundT (normalize_type_impl cx ids t)

  | DefT (_, AnyObjT) -> DefT (locationless_reason RAnyObject, AnyObjT)
  | DefT (_, AnyFunT) -> DefT (locationless_reason RAnyFunction, AnyFunT)

  | ShapeT t ->
      ShapeT (normalize_type_impl cx ids t)
  | DiffT (t1, t2) ->
      DiffT (normalize_type_impl cx ids t1, normalize_type_impl cx ids t2)

  | AnnotT t ->
      AnnotT (normalize_type_impl cx ids t)

  | KeysT (_, t) ->
      KeysT (locationless_reason RKeySet, normalize_type_impl cx ids t)

  | AbstractT (reason, t) ->
      let reason = locationless_reason (desc_of_reason reason) in
      AbstractT (reason, normalize_type_impl cx ids t)

  | EvalT (_, _, id) ->
      let evaluated = Context.evaluated cx in
      begin match IMap.get id evaluated with
      | Some t -> normalize_type_impl cx ids t
      | None ->
        (* this happens when, for example, the RHS of a destructuring is
           unconstrained, so we never evaluate the destructuring. so, make the
           destructured value also unconstrained... *)
        Locationless.EmptyT.t
      end

  | OpenPredT (_, t, _, _) ->
      normalize_type_impl cx ids t

  | ModuleT (_, exporttypes) ->
    let reason = locationless_reason (RCustom "module") in
    let exports_tmap =
      Context.find_exports cx exporttypes.exports_tmap
      |> SMap.map (normalize_type_impl cx ids)
      |> Context.make_export_map cx
    in
    let cjs_export = match exporttypes.cjs_export with
      | None -> None
      | Some t -> Some (normalize_type_impl cx ids t) in
    ModuleT (reason, { exporttypes with exports_tmap; cjs_export; })

  | TypeMapT (_, TupleMap, t1, t2) ->
      let t1 = normalize_type_impl cx ids t1 in
      let t2 = normalize_type_impl cx ids t2 in
      TypeMapT (locationless_reason RTupleMap, TupleMap, t1, t2)

  | TypeMapT (_, ObjectMap, t1, t2) ->
      let t1 = normalize_type_impl cx ids t1 in
      let t2 = normalize_type_impl cx ids t2 in
      TypeMapT (locationless_reason RObjectMap, ObjectMap, t1, t2)

  | TypeMapT (_, ObjectMapi, t1, t2) ->
      let t1 = normalize_type_impl cx ids t1 in
      let t2 = normalize_type_impl cx ids t2 in
      TypeMapT (locationless_reason RObjectMapi, ObjectMapi, t1, t2)

  | ObjProtoT _ -> ObjProtoT (locationless_reason RDummyPrototype)

  | ReposT (_, t)
  | ReposUpperT (_, t) ->
      normalize_type_impl cx ids t

  | FunProtoT _
  | ExtendsT (_, _, _, _)
  ->
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
        Locationless.EmptyT.t types
    with _ ->
      Locationless.AnyT.t

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
      | DefT (_, MaybeT (DefT (_, UnionT rep))) ->
          let tlist = UnionRep.members rep in
          let ts = List.fold_left (fun acc t -> TypeSet.add t acc) ts tlist in
          (ts, true, true)
      | DefT (_, MaybeT t) -> (TypeSet.add t ts, true, true)
      | DefT (_, VoidT) -> (ts, true, has_null)
      | DefT (_, NullT) -> (ts, has_void, true)
      (* TODO: We should only get EmptyT here when a completely open type
         variable has been in the union before grounding it. This happens when
         "null" is passed to a function parameter. We throw this out because
         it gives no information at all. merge_type also ignores EmptyT. *)
      | DefT (_, EmptyT) -> (ts, has_void, has_null)
      | _ -> (TypeSet.add t ts, has_void, has_null)
    ) ts (TypeSet.empty, false, false) in
  let ts =
    match (has_void, has_null) with
    | (true, false) -> TypeSet.add Locationless.VoidT.t ts
    | (false, true) -> TypeSet.add Locationless.NullT.t ts
    | _ ->
        (* We should never get an empty set at this point but better safe than
           sorry. Stripping out EmptyT above might be unsafe. *)
        if TypeSet.is_empty ts
        then TypeSet.singleton Locationless.EmptyT.t
        else ts
  in
  let ts = TypeSet.elements ts in
  let t =
    match ts with
    | [] -> DefT (r, EmptyT)
    | [t] -> t
    | t0::t1::ts -> DefT (r, UnionT (UnionRep.make t0 t1 ts))
  in
  if has_void && has_null
  then
    let r = replace_reason (fun desc -> RMaybe desc) r in
    DefT (r, MaybeT t)
  else t

and collect_union_members ts =
  List.fold_left (fun acc x ->
      match x with
      | DefT (_, UnionT rep) ->
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
  | [] -> DefT (r, MixedT Empty_intersection)
  | [t] -> t
  | t0::t1::ts -> DefT (r, IntersectionT (InterRep.make t0 t1 ts))

and collect_intersection_members ts =
  List.fold_left (fun acc x ->
      match x with
      | DefT (_, IntersectionT rep) ->
        let ts = InterRep.members rep in
        TypeSet.union acc (collect_intersection_members ts)
      | _ ->
        TypeSet.add x acc
    ) TypeSet.empty ts


let normalize_type cx t =
  normalize_type_impl cx ISet.empty t
