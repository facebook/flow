(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Type
open Reason
open Flow_js

type ('success, 'success_module) generic_t =
  | Success of 'success
  | SuccessModule of 'success_module
  | FailureNullishType
  | FailureAnyType
  | FailureUnhandledType of Type.t
  | FailureUnhandledMembers of Type.t

type t =
  ( (* Success *)
  (ALoc.t option * Type.t) SMap.t,
    (* SuccessModule *)
  (ALoc.t option * Type.t) SMap.t * Type.t option )
  generic_t

let rec merge_type cx =
  let create_union rep = UnionT (locationless_reason (RCustom "union"), rep) in
  function
  | (DefT (_, _, NumT _), (DefT (_, _, NumT _) as t))
  | (DefT (_, _, StrT _), (DefT (_, _, StrT _) as t))
  | (DefT (_, _, BoolT _), (DefT (_, _, BoolT _) as t))
  | (DefT (_, _, NullT), (DefT (_, _, NullT) as t))
  | (DefT (_, _, VoidT), (DefT (_, _, VoidT) as t)) ->
    t
  | (ObjProtoT _, (ObjProtoT _ as t)) -> t
  | (AnyT _, t)
  | (t, AnyT _) ->
    t
  | (DefT (_, _, EmptyT _), t)
  | (t, DefT (_, _, EmptyT _)) ->
    t
  | (_, (DefT (_, _, MixedT _) as t))
  | ((DefT (_, _, MixedT _) as t), _) ->
    t
  | (DefT (_, _, NullT), (MaybeT _ as t))
  | ((MaybeT _ as t), DefT (_, _, NullT))
  | (DefT (_, _, VoidT), (MaybeT _ as t))
  | ((MaybeT _ as t), DefT (_, _, VoidT)) ->
    t
  | ((DefT (_, _, FunT (_, _, ft1)) as fun1), (DefT (_, _, FunT (_, _, ft2)) as fun2)) ->
    (* Functions with different number of parameters cannot be merged into a
     * single function type. Instead, we should turn them into a union *)
    let params =
      if List.length ft1.params <> List.length ft2.params then
        None
      else
        let params =
          List.map2
            (fun (name1, t1) (name2, t2) ->
              (* TODO: How to merge param names? *)
              let name =
                match (name1, name2) with
                | (None, None) -> None
                | (Some name, _)
                | (_, Some name) ->
                  Some name
              in
              (name, merge_type cx (t1, t2)))
            ft1.params
            ft2.params
        in
        match (ft1.rest_param, ft2.rest_param) with
        | (None, Some _)
        | (Some _, None) ->
          None
        | (None, None) -> Some (params, None)
        | (Some r1, Some r2) -> Some (params, Some (r1, r2))
    in
    begin
      match params with
      | None -> create_union (UnionRep.make fun1 fun2 [])
      | Some (params, rest_params) ->
        let (params_names, tins) = List.split params in
        let rest_param =
          match rest_params with
          | None -> None
          | Some ((name1, loc, rest_t1), (name2, _, rest_t2)) ->
            (* TODO: How to merge rest names and locs? *)
            let name =
              match (name1, name2) with
              | (None, None) -> None
              | (Some name, _)
              | (_, Some name) ->
                Some name
            in
            Some (name, loc, merge_type cx (rest_t1, rest_t2))
        in
        let tout = merge_type cx (ft1.return_t, ft2.return_t) in
        let reason = locationless_reason (RCustom "function") in
        DefT
          ( reason,
            bogus_trust (),
            FunT
              ( dummy_static reason,
                dummy_prototype,
                mk_functiontype reason tins tout ~rest_param ~def_reason:reason ~params_names ) )
    end
  | ((DefT (_, _, ObjT o1) as t1), (DefT (_, _, ObjT o2) as t2)) ->
    let map1 = Context.find_props cx o1.props_tmap in
    let map2 = Context.find_props cx o2.props_tmap in
    (* Create an intermediate map of booleans indicating whether two objects can
     * be merged, based on the properties in each map. *)
    let merge_map =
      SMap.merge
        (fun _ p1_opt p2_opt ->
          match (p1_opt, p2_opt) with
          | (None, None) -> None
          (* In general, even objects with disjoint key sets can not be merged due
           * to width subtyping. For example, {x:T} and {y:U} is not the same as
           * {x:T,y:U}, because {x,y} is a valid inhabitant of {x:T} and the type of
           * y may != U. However, if either object type is exact, disjointness is
           * sufficient. *)
          | (Some _, None)
          | (None, Some _) ->
            Some (o1.flags.exact || o2.flags.exact)
          (* Covariant fields can be merged. *)
          | (Some (Field (_, _, Polarity.Positive)), Some (Field (_, _, Polarity.Positive))) ->
            Some true
          (* Getters are covariant and thus can be merged. *)
          | (Some (Get _), Some (Get _)) -> Some true
          (* Anything else is can't be merged. *)
          | _ -> Some false)
        map1
        map2
    in
    let merge_dict =
      match (o1.dict_t, o2.dict_t) with
      (* If neither object has an indexer, neither will the merged object. *)
      | (None, None) -> Some None
      (* If both objects covariant indexers, we can merge them. However, if the
       * key types are disjoint, the resulting dictionary is not useful. *)
      | ( Some { key = k1; value = v1; dict_polarity = Polarity.Positive; _ },
          Some { key = k2; value = v2; dict_polarity = Polarity.Positive; _ } ) ->
        (* TODO: How to merge indexer names? *)
        Some
          (Some
             {
               dict_name = None;
               key = create_intersection (InterRep.make k1 k2 []);
               value = merge_type cx (v1, v2);
               dict_polarity = Polarity.Positive;
             })
      (* Don't merge objects with possibly incompatible indexers. *)
      | _ -> None
    in
    let merge_call =
      match (o1.call_t, o2.call_t) with
      | (None, None) -> Some None
      | (Some _, None) ->
        if o2.flags.exact then
          Some o1.call_t
        else
          None
      | (None, Some _) ->
        if o1.flags.exact then
          Some o2.call_t
        else
          None
      | (Some id1, Some id2) ->
        let c1 = Context.find_call cx id1 in
        let c2 = Context.find_call cx id2 in
        let id = Context.make_call_prop cx (create_union (UnionRep.make c1 c2 [])) in
        Some (Some id)
    in
    (* Only merge objects if every property can be merged. *)
    let should_merge = SMap.for_all (fun _ x -> x) merge_map in
    (* Don't merge objects with different prototypes. *)
    let should_merge = should_merge && o1.proto_t = o2.proto_t in
    (match (should_merge, merge_dict, merge_call) with
    | (true, Some dict, Some call) ->
      let map =
        SMap.merge
          (fun _ p1_opt p2_opt ->
            match (p1_opt, p2_opt) with
            (* Merge disjoint+exact objects. *)
            | (Some t, None)
            | (None, Some t) ->
              Some t
            (* Shouldn't happen, per merge_map above. *)
            | _ -> None)
          map1
          map2
      in
      let id = Context.generate_property_map cx map in
      let sealed =
        match (o1.flags.sealed, o2.flags.sealed) with
        | (Sealed, Sealed) -> Sealed
        | (UnsealedInFile s1, UnsealedInFile s2) when s1 = s2 -> UnsealedInFile s1
        | _ -> UnsealedInFile None
      in
      let flags =
        {
          sealed;
          exact = o1.flags.exact && o2.flags.exact;
          frozen = o1.flags.frozen && o2.flags.frozen;
        }
      in
      let reason = locationless_reason (RCustom "object") in
      mk_object_def_type ~reason ~flags ~dict ~call id o1.proto_t
    | _ -> create_union (UnionRep.make t1 t2 []))
  | (DefT (_, _, ArrT (ArrayAT (t1, ts1))), DefT (_, _, ArrT (ArrayAT (t2, ts2)))) ->
    let tuple_types =
      match (ts1, ts2) with
      | (None, _)
      | (_, None) ->
        None
      | (Some ts1, Some ts2) -> Some (Base.List.map2_exn ~f:(merge_type cx |> curry) ts1 ts2)
    in
    DefT
      ( locationless_reason (RCustom "array"),
        bogus_trust (),
        ArrT (ArrayAT (merge_type cx (t1, t2), tuple_types)) )
  | (DefT (_, _, ArrT (TupleAT (t1, ts1))), DefT (_, _, ArrT (TupleAT (t2, ts2))))
    when List.length ts1 = List.length ts2 ->
    DefT
      ( locationless_reason (RCustom "tuple"),
        bogus_trust (),
        ArrT
          (TupleAT (merge_type cx (t1, t2), Base.List.map2_exn ~f:(merge_type cx |> curry) ts1 ts2))
      )
  | (DefT (_, _, ArrT (ROArrayAT elemt1)), DefT (_, _, ArrT (ROArrayAT elemt2))) ->
    DefT
      ( locationless_reason (RCustom "read only array"),
        bogus_trust (),
        ArrT (ROArrayAT (merge_type cx (elemt1, elemt2))) )
  | (MaybeT (_, t1), MaybeT (_, t2))
  | (MaybeT (_, t1), t2)
  | (t1, MaybeT (_, t2)) ->
    let t = merge_type cx (t1, t2) in
    let reason = locationless_reason (RMaybe (desc_of_t t)) in
    MaybeT (reason, t)
  | (UnionT (_, rep1), UnionT (_, rep2)) -> create_union (UnionRep.rev_append rep1 rep2)
  | (UnionT (_, rep), t)
  | (t, UnionT (_, rep)) ->
    create_union (UnionRep.cons t rep)
  (* TODO: do we need to do anything special for merging Null with Void,
     Optional with other types, etc.? *)
  | (t1, t2) -> create_union (UnionRep.make t1 t2 [])

let instantiate_poly_t cx t args =
  match t with
  | DefT (_, _, PolyT { tparams = type_params; t_out = t_; _ }) ->
    let args = Option.value ~default:[] args in
    let maximum_arity = Nel.length type_params in
    if List.length args > maximum_arity then (
      Hh_logger.error "Instantiating poly type failed";
      t
    ) else
      let (map, _, too_few_args) =
        Nel.fold_left
          (fun (map, ts, too_few_args) typeparam ->
            let (t, ts, too_few_args) =
              match (typeparam, ts) with
              | ({ default = Some default; _ }, []) ->
                (* fewer arguments than params and we have a default *)
                (subst cx map default, [], too_few_args)
              | ({ default = None; _ }, []) -> (AnyT.error (reason_of_t t), [], true)
              | (_, t :: ts) -> (t, ts, too_few_args)
            in
            (SMap.add typeparam.name t map, ts, too_few_args))
          (SMap.empty, args, false)
          type_params
      in
      if too_few_args then (
        Hh_logger.error "Instantiating poly type failed";
        t
      ) else
        subst cx map t_
  | DefT (_, _, EmptyT _)
  | DefT (_, _, MixedT _)
  | AnyT _
  | DefT (_, _, TypeT (_, AnyT _)) ->
    t
  | _ ->
    (match args with
    | None -> t
    | Some _ -> assert_false ("unexpected args passed to instantiate_poly_t: " ^ string_of_ctor t))

let intersect_members cx members =
  match members with
  | [] -> SMap.empty
  | _ ->
    let map = SMap.map (fun x -> [x]) (List.hd members) in
    let map =
      List.fold_left
        (fun acc x ->
          SMap.merge
            (fun _ tl t ->
              match (tl, t) with
              | (None, None) -> None
              | (None, Some _) -> None
              | (Some _, None) -> None
              | (Some tl, Some t) -> Some (t :: tl))
            acc
            x)
        map
        (List.tl members)
    in
    SMap.map
      (List.fold_left
         (fun (_, acc) (loc, t) ->
           (* Arbitrarily use the last location encountered *)
           (loc, merge_type cx (acc, t)))
         (None, Locationless.EmptyT.t |> with_trust bogus_trust))
      map

and instantiate_type = function
  | ThisClassT (_, t)
  | DefT (_, _, ClassT t)
  | (AnyT _ as t)
  | DefT (_, _, TypeT (_, t))
  | (DefT (_, _, EmptyT _) as t) ->
    t
  | t -> "cannot instantiate non-class type " ^ string_of_ctor t |> assert_false

let possible_types_of_use cx = function
  | UseT (_, t) -> possible_types_of_type cx t
  | _ -> []

let string_of_extracted_type = function
  | Success t -> Printf.sprintf "Success (%s)" (Type.string_of_ctor t)
  | SuccessModule t -> Printf.sprintf "SuccessModule (%s)" (Type.string_of_ctor t)
  | FailureNullishType -> "FailureNullishType"
  | FailureAnyType -> "FailureAnyType"
  | FailureUnhandledType t -> Printf.sprintf "FailureUnhandledType (%s)" (Type.string_of_ctor t)
  | FailureUnhandledMembers t ->
    Printf.sprintf "FailureUnhandledMembers (%s)" (Type.string_of_ctor t)

let to_command_result = function
  | Success map
  | SuccessModule (map, None) ->
    Ok map
  | SuccessModule (named_exports, Some cjs_export) ->
    Ok (SMap.add "default" (None, cjs_export) named_exports)
  | FailureNullishType -> Error "autocomplete on possibly null or undefined value"
  | FailureAnyType -> Error "not enough type information to autocomplete"
  | FailureUnhandledType t ->
    Error
      (spf "autocomplete on unexpected type of value %s (please file a task!)" (string_of_ctor t))
  | FailureUnhandledMembers t ->
    Error
      (spf
         "autocomplete on unexpected members of value %s (please file a task!)"
         (string_of_ctor t))

let find_props cx =
  Context.find_props cx
  %> SMap.filter (fun key _ ->
         (* Filter out keys that start with "$" *)
         not (String.length key >= 1 && key.[0] = '$'))

let resolve_tvar cx (_, id) =
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
  List.fold_left
    (fun u t -> merge_type cx (t, u))
    (RAnyImplicit |> locationless_reason |> Unsoundness.unresolved_any)
    ts

let rec resolve_type cx = function
  | OpenT tvar -> resolve_tvar cx tvar |> resolve_type cx
  | AnnotT (_, t, _) -> resolve_type cx t
  | MergedT (_, uses) ->
    begin
      match Base.List.(uses >>= possible_types_of_use cx) with
      (* The unit of intersection is normally mixed, but MergedT is hacky and empty
      fits better here *)
      | [] -> locationless_reason REmpty |> EmptyT.make |> with_trust bogus_trust
      | [x] -> x
      | x :: y :: ts -> InterRep.make x y ts |> create_intersection
    end
  | t -> t

let rec extract_type cx this_t =
  match this_t with
  | OpenT _
  | AnnotT _
  | MergedT _ ->
    resolve_type cx this_t |> extract_type cx
  | OptionalT { reason = _; type_ = ty; use_desc = _ }
  | MaybeT (_, ty) ->
    extract_type cx ty
  | DefT (_, _, (NullT | VoidT)) -> FailureNullishType
  | AnyT _ -> FailureAnyType
  | DefT (_, _, InstanceT _) as t -> Success t
  | DefT (_, _, ObjT _) as t -> Success t
  | ExactT (_, t) -> extract_type cx t
  | ModuleT _ as t -> SuccessModule t
  | ThisTypeAppT (_, c, _, ts_opt) ->
    let c = resolve_type cx c in
    let inst_t = instantiate_poly_t cx c ts_opt in
    let inst_t = instantiate_type inst_t in
    extract_type cx inst_t
  | TypeAppT (_, _, c, ts) ->
    let c = resolve_type cx c in
    let inst_t = instantiate_poly_t cx c (Some ts) in
    let inst_t = instantiate_type inst_t in
    extract_type cx inst_t
  | DefT (_, _, PolyT { t_out = sub_type; _ }) ->
    (* TODO: replace type parameters with stable/proper names? *)
    extract_type cx sub_type
  | ThisClassT (_, DefT (_, _, InstanceT (static, _, _, _)))
  | DefT (_, _, ClassT (DefT (_, _, InstanceT (static, _, _, _)))) ->
    extract_type cx static
  | DefT (_, _, FunT _) as t -> Success t
  | IntersectionT _ as t -> Success t
  | UnionT _ as t -> Success t
  | DefT (reason, _, SingletonStrT _)
  | DefT (reason, _, StrT _) ->
    get_builtin_type cx reason "String" |> extract_type cx
  | DefT (reason, _, SingletonNumT _)
  | DefT (reason, _, NumT _) ->
    get_builtin_type cx reason "Number" |> extract_type cx
  | DefT (reason, _, SingletonBoolT _)
  | DefT (reason, _, BoolT _) ->
    get_builtin_type cx reason "Boolean" |> extract_type cx
  | DefT (reason, _, SymbolT) -> get_builtin_type cx reason "Symbol" |> extract_type cx
  | DefT (reason, _, CharSetT _) -> get_builtin_type cx reason "String" |> extract_type cx
  | DefT (_, _, IdxWrapper t) -> extract_type cx t
  | DefT (_, _, ReactAbstractComponentT _) as t -> Success t
  | ReposT (_, t)
  | InternalT (ReposUpperT (_, t)) ->
    extract_type cx t
  | OpaqueT (_, { underlying_t = Some t; _ })
  | OpaqueT (_, { super_t = Some t; _ }) ->
    extract_type cx t
  | DefT (reason, _, ArrT arrtype) ->
    let (builtin, elemt) =
      match arrtype with
      | ArrayAT (elemt, _) -> (get_builtin cx "Array" reason, elemt)
      | TupleAT (elemt, _)
      | ROArrayAT elemt ->
        (get_builtin cx "$ReadOnlyArray" reason, elemt)
    in
    let array_t = resolve_type cx builtin in
    Some [elemt] |> instantiate_poly_t cx array_t |> instantiate_type |> extract_type cx
  | EvalT (t, defer, id) -> eval_evalt cx t defer id |> extract_type cx
  | BoundT _
  | InternalT (ChoiceKitT (_, _))
  | TypeDestructorTriggerT _
  | DefT (_, _, ClassT _)
  | CustomFunT (_, _)
  | MatchingPropT (_, _, _)
  | DefT (_, _, EmptyT _)
  | ExistsT _
  | InternalT (ExtendsT _)
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | FunProtoT _
  | KeysT (_, _)
  | DefT (_, _, MixedT _)
  | NullProtoT _
  | ObjProtoT _
  | OpaqueT _
  | OpenPredT _
  | ShapeT _
  | ThisClassT _
  | DefT (_, _, TypeT _)
  | DefT (_, _, EnumObjectT _)
  | DefT (_, _, EnumT _) ->
    FailureUnhandledType this_t

let rec extract_members ?(exclude_proto_members = false) cx = function
  | FailureNullishType -> FailureNullishType
  | FailureAnyType -> FailureAnyType
  | FailureUnhandledType t -> FailureUnhandledType t
  | FailureUnhandledMembers t -> FailureUnhandledMembers t
  | Success (DefT (_, _, InstanceT (_, super, _, { own_props; proto_props; _ }))) ->
    let members =
      SMap.fold
        (fun x p acc ->
          (* TODO: It isn't currently possible to return two types for a given
           * property in autocomplete, so for now we just return the getter
           * type. *)
          let (loc, t) =
            match p with
            | Field (loc, t, _)
            | Get (loc, t)
            | Set (loc, t)
            (* arbitrarily use the location for the getter. maybe we can send both in the future *)
            | GetSet (loc, t, _, _)
            | Method (loc, t) ->
              (loc, t)
          in
          SMap.add x (loc, t) acc)
        (find_props cx own_props)
        SMap.empty
    in
    if exclude_proto_members then
      Success members
    else
      (* TODO: own props should take precedence *)
      let members =
        SMap.fold
          (fun x p acc ->
            match Property.read_t p with
            | Some t ->
              let loc = Property.read_loc p in
              SMap.add x (loc, t) acc
            | None -> acc)
          (find_props cx proto_props)
          members
      in
      let super_flds = extract_members_as_map ~exclude_proto_members cx super in
      Success (AugmentableSMap.augment super_flds ~with_bindings:members)
  | Success (DefT (_, _, ObjT { props_tmap = flds; proto_t = proto; _ })) ->
    let proto_reason = reason_of_t proto in
    let rep = InterRep.make proto (get_builtin_type cx proto_reason "Object") [] in
    let proto_t = IntersectionT (proto_reason, rep) in
    let prot_members =
      if exclude_proto_members then
        SMap.empty
      else
        extract_members_as_map ~exclude_proto_members cx proto_t
    in
    let members =
      SMap.fold
        (fun x p acc ->
          match Property.read_t p with
          | Some t ->
            let loc = Property.read_loc p in
            SMap.add x (loc, t) acc
          | None -> acc)
        (find_props cx flds)
        SMap.empty
    in
    Success (AugmentableSMap.augment prot_members ~with_bindings:members)
  | SuccessModule (ModuleT (_, { exports_tmap; cjs_export; has_every_named_export = _ }, _)) ->
    let named_exports = Context.find_exports cx exports_tmap in
    let cjs_export =
      match cjs_export with
      | Some t -> Some (resolve_type cx t)
      | None -> None
    in
    SuccessModule (named_exports, cjs_export)
  | Success (DefT (_, _, FunT (static, proto, _))) ->
    let members = extract_members_as_map ~exclude_proto_members cx static in
    let prot_members = extract_members_as_map ~exclude_proto_members cx proto in
    Success (AugmentableSMap.augment prot_members ~with_bindings:members)
  | Success (IntersectionT (_, rep)) ->
    (* Intersection type should autocomplete for every property of
         every type in the intersection *)
    let ts = InterRep.members rep in
    let members = Base.List.map ~f:(extract_members_as_map ~exclude_proto_members cx) ts in
    Success
      (List.fold_left
         (fun acc members -> AugmentableSMap.augment acc ~with_bindings:members)
         SMap.empty
         members)
  | Success (UnionT (_, rep)) ->
    (* Union type should autocomplete for only the properties that are in
      * every type in the intersection *)
    let ts = UnionRep.members rep in
    let members =
      ts
      (* Although we'll ignore the any-ish and nullish members of the union *)
      |> List.filter (function
             | DefT (_, _, (NullT | VoidT))
             | AnyT _ ->
               false
             | _ -> true)
      |> Base.List.map ~f:(extract_members_as_map ~exclude_proto_members cx)
      |> intersect_members cx
    in
    Success members
  | Success t
  | SuccessModule t ->
    FailureUnhandledMembers t

and extract ?exclude_proto_members cx = extract_type cx %> extract_members ?exclude_proto_members cx

and extract_members_as_map ~exclude_proto_members cx this_t =
  match extract ~exclude_proto_members cx this_t |> to_command_result with
  | Ok map -> map
  | Error _ -> SMap.empty
