(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Type
open TypeUtil
open Reason
open Flow_js

type ('success, 'success_module, 'success_namespace) generic_t =
  | Success of 'success
  | SuccessModule of 'success_module
  | SuccessNamespace of 'success_namespace
  | FailureNullishType
  | FailureAnyType
  | FailureUnhandledType of Type.t
  | FailureUnhandledMembers of Type.t

type t =
  ( (* Success *)
  (ALoc.t Nel.t option * Type.t) SMap.t,
    (* SuccessModule *)
  (ALoc.t Nel.t option * Type.t) SMap.t * Type.t option,
    (* SuccessNamespace *)
  (ALoc.t Nel.t option * Type.t) SMap.t
  )
  generic_t

let rec merge_type cx =
  let create_union rep = UnionT (locationless_reason (RCustom "union"), rep) in
  function
  | (DefT (_, NumT _), (DefT (_, NumT _) as t))
  | (DefT (_, StrT _), (DefT (_, StrT _) as t))
  | (DefT (_, BoolT _), (DefT (_, BoolT _) as t))
  | (DefT (_, NullT), (DefT (_, NullT) as t))
  | (DefT (_, VoidT), (DefT (_, VoidT) as t)) ->
    t
  | (ObjProtoT _, (ObjProtoT _ as t)) -> t
  | (AnyT _, t)
  | (t, AnyT _) ->
    t
  | (DefT (_, EmptyT), t)
  | (t, DefT (_, EmptyT)) ->
    t
  | (_, (DefT (_, MixedT _) as t))
  | ((DefT (_, MixedT _) as t), _) ->
    t
  | (DefT (_, NullT), (MaybeT _ as t))
  | ((MaybeT _ as t), DefT (_, NullT))
  | (DefT (_, VoidT), (MaybeT _ as t))
  | ((MaybeT _ as t), DefT (_, VoidT)) ->
    t
  | ((DefT (_, FunT (_, ft1)) as fun1), (DefT (_, FunT (_, ft2)) as fun2)) ->
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
        (* TODO merging predicates would require aligning param names as well *)
        let predicate = None in
        DefT
          ( reason,
            FunT
              ( dummy_static reason,
                mk_functiontype
                  reason
                  tins
                  tout
                  ~rest_param
                  ~def_reason:reason
                  ~params_names
                  ~predicate
              )
          )
    end
  | ((DefT (_, ObjT o1) as t1), (DefT (_, ObjT o2) as t2)) ->
    let map1 = Context.find_props cx o1.props_tmap in
    let map2 = Context.find_props cx o2.props_tmap in
    (* Create an intermediate map of booleans indicating whether two objects can
     * be merged, based on the properties in each map. *)
    let merge_map =
      NameUtils.Map.merge
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
            Some (Obj_type.is_exact o1.flags.obj_kind || Obj_type.is_exact o2.flags.obj_kind)
          (* Covariant fields can be merged. *)
          | ( Some (Field { polarity = Polarity.Positive; _ }),
              Some (Field { polarity = Polarity.Positive; _ })
            ) ->
            Some true
          (* Getters are covariant and thus can be merged. *)
          | (Some (Get _), Some (Get _)) -> Some true
          (* Anything else is can't be merged. *)
          | _ -> Some false)
        map1
        map2
    in
    let obj_kind =
      match (o1.flags.obj_kind, o2.flags.obj_kind) with
      | ( Indexed { key = k1; value = v1; dict_polarity = Polarity.Positive; _ },
          Indexed { key = k2; value = v2; dict_polarity = Polarity.Positive; _ }
        ) ->
        Indexed
          {
            dict_name = None;
            key = create_intersection (InterRep.make k1 k2 []);
            value = merge_type cx (v1, v2);
            dict_polarity = Polarity.Positive;
          }
      | (Indexed d, _)
      | (_, Indexed d) ->
        Indexed d
      | (Inexact, _)
      | (_, Inexact) ->
        Inexact
      | (Exact, Exact) -> Exact
    in
    let merge_call =
      match (o1.call_t, o2.call_t) with
      | (None, None) -> Some None
      | (Some _, None) ->
        if Obj_type.is_exact o2.flags.obj_kind then
          Some o1.call_t
        else
          None
      | (None, Some _) ->
        if Obj_type.is_exact o1.flags.obj_kind then
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
    let should_merge = NameUtils.Map.for_all (fun _ x -> x) merge_map in
    (* Don't merge objects with different prototypes. *)
    let should_merge = should_merge && o1.proto_t = o2.proto_t in
    (match (should_merge, obj_kind, merge_call) with
    | (true, Indexed _, Some call) ->
      let map =
        NameUtils.Map.merge
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
      let flags =
        {
          frozen = o1.flags.frozen && o2.flags.frozen;
          obj_kind;
          react_dro =
            ( if Base.Option.is_some o1.flags.react_dro && Base.Option.is_some o1.flags.react_dro
            then
              o1.flags.react_dro
            else
              None
            );
        }
      in
      let reason = locationless_reason (RCustom "object") in
      mk_object_def_type ~reason ~flags ~call id o1.proto_t
    | _ -> create_union (UnionRep.make t1 t2 []))
  | ( DefT
        (_, ArrT (ArrayAT { elem_t = t1; tuple_view = Some (elements1, arity1); react_dro = dro1 })),
      DefT
        (_, ArrT (ArrayAT { elem_t = t2; tuple_view = Some (elements2, arity2); react_dro = dro2 }))
    )
    when arity1 = arity2
         && List.for_all2
              (fun (TupleElement { polarity = p1; optional = o1; _ })
                   (TupleElement { polarity = p2; optional = o2; _ }) ->
                Polarity.equal (p1, p2) && o1 = o2)
              elements1
              elements2 ->
    let elements =
      Base.List.map2_exn
        ~f:
          (fun (TupleElement { name = name1; t = t1; polarity; optional; reason = _ })
               (TupleElement { name = name2; t = t2; polarity = _; optional = _; reason = _ }) ->
          let name =
            if name1 = name2 then
              name1
            else
              None
          in
          let t = merge_type cx (t1, t2) in
          let reason = locationless_reason (RTupleElement { name }) in
          TupleElement { name; t; polarity; optional; reason })
        elements1
        elements2
    in
    DefT
      ( locationless_reason (RCustom "array"),
        ArrT
          (ArrayAT
             {
               elem_t = merge_type cx (t1, t2);
               tuple_view = Some (elements, arity1);
               react_dro =
                 ( if Base.Option.is_some dro1 && Base.Option.is_some dro2 then
                   dro1
                 else
                   None
                 );
             }
          )
      )
  | ( DefT (_, ArrT (ArrayAT { elem_t = t1; tuple_view = _; react_dro = dro1 })),
      DefT (_, ArrT (ArrayAT { elem_t = t2; tuple_view = _; react_dro = dro2 }))
    ) ->
    DefT
      ( locationless_reason (RCustom "array"),
        ArrT
          (ArrayAT
             {
               elem_t = merge_type cx (t1, t2);
               tuple_view = None;
               react_dro =
                 ( if Base.Option.is_some dro1 && Base.Option.is_some dro2 then
                   dro1
                 else
                   None
                 );
             }
          )
      )
  | ( DefT (_, ArrT (TupleAT { elem_t = t1; elements = ts1; arity = arity1; react_dro = dro1 })),
      DefT (_, ArrT (TupleAT { elem_t = t2; elements = ts2; arity = arity2; react_dro = dro2 }))
    )
    when arity1 = arity2
         && List.for_all2
              (fun (TupleElement { polarity = p1; optional = o1; _ })
                   (TupleElement { polarity = p2; optional = o2; _ }) ->
                Polarity.equal (p1, p2) && o1 = o2)
              ts1
              ts2 ->
    DefT
      ( locationless_reason (RCustom "tuple"),
        ArrT
          (TupleAT
             {
               elem_t = merge_type cx (t1, t2);
               react_dro =
                 ( if Base.Option.is_some dro1 && Base.Option.is_some dro2 then
                   dro1
                 else
                   None
                 );
               elements =
                 Base.List.map2_exn
                   ~f:
                     (fun (TupleElement { name = name1; t = t1; polarity; optional; reason = _ })
                          (TupleElement
                            { name = name2; t = t2; polarity = _; optional = _; reason = _ }
                            ) ->
                     let name =
                       if name1 = name2 then
                         name1
                       else
                         None
                     in
                     let t = merge_type cx (t1, t2) in
                     let reason = locationless_reason (RTupleElement { name }) in
                     TupleElement { name; t; polarity; optional; reason })
                   ts1
                   ts2;
               arity = arity1;
             }
          )
      )
  | (DefT (_, ArrT (ROArrayAT (elemt1, dro1))), DefT (_, ArrT (ROArrayAT (elemt2, dro2)))) ->
    DefT
      ( locationless_reason (RCustom "read only array"),
        ArrT
          (ROArrayAT
             ( merge_type cx (elemt1, elemt2),
               if Base.Option.is_some dro1 && Base.Option.is_some dro2 then
                 dro1
               else
                 None
             )
          )
      )
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
  | DefT (_, PolyT { tparams = type_params; t_out = t_; _ }) ->
    let args = Base.Option.value ~default:[] args in
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
            (Subst_name.Map.add typeparam.name t map, ts, too_few_args))
          (Subst_name.Map.empty, args, false)
          type_params
      in
      if too_few_args then (
        Hh_logger.error "Instantiating poly type failed";
        t
      ) else
        subst cx map t_
  | DefT (_, EmptyT)
  | DefT (_, MixedT _)
  | AnyT _
  | DefT (_, TypeT (_, AnyT _)) ->
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
         (None, Locationless.EmptyT.t)
      )
      map

and instantiate_type = function
  | DefT (_, ClassT (ThisInstanceT (r, t, _, _))) -> DefT (r, InstanceT t)
  | DefT (_, ClassT t)
  | (AnyT _ as t)
  | DefT (_, TypeT (_, t))
  | (DefT (_, EmptyT) as t) ->
    t
  | t -> "cannot instantiate non-class type " ^ string_of_ctor t |> assert_false

let string_of_extracted_type = function
  | Success t -> Printf.sprintf "Success (%s)" (Type.string_of_ctor t)
  | SuccessModule t -> Printf.sprintf "SuccessModule (%s)" (Type.string_of_ctor t)
  | SuccessNamespace t -> Printf.sprintf "SuccessNamespace (%s)" (Type.string_of_ctor t)
  | FailureNullishType -> "FailureNullishType"
  | FailureAnyType -> "FailureAnyType"
  | FailureUnhandledType t -> Printf.sprintf "FailureUnhandledType (%s)" (Type.string_of_ctor t)
  | FailureUnhandledMembers t ->
    Printf.sprintf "FailureUnhandledMembers (%s)" (Type.string_of_ctor t)

let to_command_result = function
  | Success map
  | SuccessNamespace map
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
      (spf "autocomplete on unexpected members of value %s (please file a task!)" (string_of_ctor t))

let find_props cx =
  Context.find_props cx
  %> NameUtils.Map.filter (fun key _ ->
         match key with
         | OrdinaryName key ->
           (* Filter out keys that start with "$" *)
           not (String.length key >= 1 && key.[0] = '$')
         | InternalName _
         | InternalModuleName _ ->
           (* TODO we probably should filter out internal names too, but for now keeping behavior the same *)
           true
     )
  %> NameUtils.display_smap_of_namemap

let resolve_tvar cx (_, id) =
  let ts = Flow_js_utils.possible_types cx id in

  (* The list of types returned by possible_types is often empty, and the
     most common reason is that we don't have enough type coverage to
     resolve id. Thus, we take the unit of merging to be `any`. (Something
     similar happens when summarizing exports in ContextOptimizer.)

     In the future, we might report errors in some cases where
     possible_types returns an empty list: e.g., when we detect unreachable
     code, or even we don't have enough type coverage. Irrespective of these
     changes, the above decision would continue to make sense: as errors
     become stricter, type resolution should become even more lenient to
     improve failure tolerance. *)
  List.fold_left
    (fun u t -> merge_type cx (t, u))
    (RAnyImplicit |> locationless_reason |> Unsoundness.unresolved_any)
    ts

let rec resolve_type cx = function
  | OpenT tvar -> resolve_tvar cx tvar |> resolve_type cx
  | AnnotT (_, t, _) -> resolve_type cx t
  | t -> t

let rec extract_type cx this_t =
  match this_t with
  | OpenT _
  | AnnotT _ ->
    resolve_type cx this_t |> extract_type cx
  | OptionalT { reason = _; type_ = ty; use_desc = _ }
  | MaybeT (_, ty) ->
    extract_type cx ty
  | DefT (_, (NullT | VoidT)) -> FailureNullishType
  | AnyT _ -> FailureAnyType
  | ThisInstanceT (r, t, _, _) -> Success (DefT (r, InstanceT t))
  | DefT (_, InstanceT _) as t -> Success t
  | DefT (_, ObjT _) as t -> Success t
  | DefT (_, EnumObjectT _) as t -> Success t
  | ExactT (_, t) -> extract_type cx t
  | GenericT { bound; _ } -> extract_type cx bound
  | ModuleT _ as t -> SuccessModule t
  | NamespaceT _ as t -> SuccessNamespace t
  | ThisTypeAppT (_, c, _, ts_opt) ->
    let c = resolve_type cx c in
    let inst_t = instantiate_poly_t cx c ts_opt in
    let inst_t = instantiate_type inst_t in
    extract_type cx inst_t
  | TypeAppT { reason = _; use_op = _; type_; targs; from_value; use_desc = _ } ->
    let c = resolve_type cx type_ in
    let inst_t = instantiate_poly_t cx c (Some targs) in
    let inst_t =
      if from_value then
        inst_t
      else
        instantiate_type inst_t
    in
    extract_type cx inst_t
  | DefT (_, PolyT { t_out = sub_type; _ }) ->
    (* TODO: replace type parameters with stable/proper names? *)
    extract_type cx sub_type
  | DefT (_, ClassT (ThisInstanceT (_, { static; _ }, _, _)))
  | DefT (_, ClassT (DefT (_, InstanceT { static; _ }))) ->
    extract_type cx static
  | DefT (_, FunT _) as t -> Success t
  | IntersectionT _ as t -> Success t
  | UnionT _ as t -> Success t
  | DefT (reason, SingletonStrT _)
  | DefT (reason, StrT _)
  | DefT (reason, NumericStrKeyT _) ->
    get_builtin_type cx reason "String" |> extract_type cx
  | DefT (reason, SingletonNumT _)
  | DefT (reason, NumT _) ->
    get_builtin_type cx reason "Number" |> extract_type cx
  | DefT (reason, SingletonBoolT _)
  | DefT (reason, BoolT _) ->
    get_builtin_type cx reason "Boolean" |> extract_type cx
  | DefT (reason, SingletonBigIntT _)
  | DefT (reason, BigIntT _) ->
    get_builtin_type cx reason "BigInt" |> extract_type cx
  | DefT (reason, SymbolT) -> get_builtin_type cx reason "Symbol" |> extract_type cx
  | DefT (reason, CharSetT _) -> get_builtin_type cx reason "String" |> extract_type cx
  | DefT (_, ReactAbstractComponentT _) as t -> Success t
  | DefT (_, RendersT _) as t -> Success t
  | OpaqueT (_, { underlying_t = Some t; _ })
  | OpaqueT (_, { super_t = Some t; _ }) ->
    extract_type cx t
  | DefT (reason, ArrT arrtype) ->
    let (builtin, elem_t) =
      match arrtype with
      | ArrayAT { elem_t; _ } -> (Flow_js_utils.lookup_builtin_value cx "Array" reason, elem_t)
      | TupleAT { elem_t; _ }
      | ROArrayAT (elem_t, _) ->
        (Flow_js_utils.lookup_builtin_type cx "$ReadOnlyArray" reason, elem_t)
    in
    let array_t = resolve_type cx builtin in
    Some [elem_t] |> instantiate_poly_t cx array_t |> instantiate_type |> extract_type cx
  | EvalT (t, TypeDestructorT (use_op, reason, d), id) ->
    let result = mk_type_destructor cx use_op reason t d id in
    extract_type cx result
  | InternalT (ChoiceKitT (_, _))
  | DefT (_, ClassT _)
  | CustomFunT (_, _)
  | MatchingPropT (_, _, _)
  | DefT (_, EmptyT)
  | InternalT (ExtendsT _)
  | InternalT (EnforceUnionOptimized _)
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | FunProtoT _
  | KeysT (_, _)
  | DefT (_, MixedT _)
  | NullProtoT _
  | ObjProtoT _
  | OpaqueT _
  | DefT (_, TypeT _)
  | DefT (_, EnumT _) ->
    FailureUnhandledType this_t

let rec extract_members ?(exclude_proto_members = false) cx = function
  | FailureNullishType -> FailureNullishType
  | FailureAnyType -> FailureAnyType
  | FailureUnhandledType t -> FailureUnhandledType t
  | FailureUnhandledMembers t -> FailureUnhandledMembers t
  | Success (GenericT { bound; _ }) -> extract_members ~exclude_proto_members cx (Success bound)
  | Success (ThisInstanceT (_, { super; inst = { own_props; proto_props; _ }; _ }, _, _))
  | Success (DefT (_, InstanceT { super; inst = { own_props; proto_props; _ }; _ })) ->
    let members =
      SMap.fold
        (fun x p acc ->
          (* TODO: It isn't currently possible to return two types for a given
           * property in autocomplete, so for now we just return the getter
           * type. *)
          let t =
            match p with
            | Field { type_ = t; _ }
            | Get { key_loc = _; type_ = t }
            | Set { key_loc = _; type_ = t }
            | GetSet { get_type = t; _ }
            | Method { key_loc = _; type_ = t } ->
              t
          in
          SMap.add x (Property.def_locs p, t) acc)
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
              let loc = Property.def_locs p in
              SMap.add x (loc, t) acc
            | None -> acc)
          (find_props cx proto_props)
          members
      in
      let super_flds = extract_members_as_map ~exclude_proto_members cx super in
      Success (AugmentableSMap.augment super_flds ~with_bindings:members)
  | Success (DefT (_, ObjT { props_tmap = flds; proto_t = proto; _ })) ->
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
          | Some t -> SMap.add x (Property.def_locs p, t) acc
          | None -> acc)
        (find_props cx flds)
        SMap.empty
    in
    Success (AugmentableSMap.augment prot_members ~with_bindings:members)
  | SuccessModule
      (ModuleT
        {
          module_reason = _;
          module_export_types =
            { value_exports_tmap; type_exports_tmap; cjs_export; has_every_named_export = _ };
          module_is_strict = _;
          module_available_platforms = _;
        }
        ) ->
    let named_exports =
      NameUtils.Map.union
        (Context.find_exports cx value_exports_tmap)
        (Context.find_exports cx type_exports_tmap)
    in
    let cjs_export =
      match cjs_export with
      | Some t -> Some (resolve_type cx t)
      | None -> None
    in
    let named_exports =
      NameUtils.display_smap_of_namemap named_exports
      |> SMap.map (fun { name_loc; preferred_def_locs; type_ } ->
             let def_locs =
               match preferred_def_locs with
               | Some _ -> preferred_def_locs
               | None -> Base.Option.map ~f:Nel.one name_loc
             in
             (def_locs, type_)
         )
    in
    SuccessModule (named_exports, cjs_export)
  | SuccessNamespace (NamespaceT { values_type; types_tmap }) ->
    let members =
      SMap.fold
        (fun x p acc ->
          match Property.read_t p with
          | Some t -> SMap.add x (Property.def_locs p, t) acc
          | None -> acc)
        (Context.find_props cx types_tmap |> NameUtils.display_smap_of_namemap)
        (extract_members_as_map ~exclude_proto_members cx values_type)
    in
    SuccessNamespace members
  | Success (DefT (_, FunT (static, _))) ->
    Success (extract_members_as_map ~exclude_proto_members cx static)
  | Success (DefT (enum_reason, EnumObjectT enum) as enum_object_t) ->
    let { members; representation_t; _ } = enum in
    let enum_t = mk_enum_type enum_reason enum in
    let proto_members =
      if exclude_proto_members then
        SMap.empty
      else
        let proto =
          get_builtin_typeapp cx enum_reason "$EnumProto" [enum_object_t; enum_t; representation_t]
        in
        (* `$EnumProto` has a null proto, so we set `exclude_proto_members` to true *)
        extract_members_as_map ~exclude_proto_members:true cx proto
    in
    let result = SMap.map (fun member_loc -> (Some (Nel.one member_loc), enum_t)) members in
    Success (AugmentableSMap.augment proto_members ~with_bindings:result)
  | Success (IntersectionT (_, rep)) ->
    (* Intersection type should autocomplete for every property of
       every type in the intersection *)
    let ts = InterRep.members rep in
    let members = Base.List.map ~f:(extract_members_as_map ~exclude_proto_members cx) ts in
    Success
      (List.fold_left
         (fun acc members -> AugmentableSMap.augment acc ~with_bindings:members)
         SMap.empty
         members
      )
  | Success (UnionT (_, rep)) ->
    (* Union type should autocomplete for only the properties that are in
       * every type in the intersection *)
    let ts = UnionRep.members rep in
    let members =
      ts
      (* Although we'll ignore the any-ish and nullish members of the union *)
      |> List.filter (function
             | DefT (_, (NullT | VoidT))
             | AnyT _ ->
               false
             | _ -> true
             )
      |> Base.List.map ~f:(extract_members_as_map ~exclude_proto_members cx)
      |> intersect_members cx
    in
    Success members
  | Success t
  | SuccessModule t
  | SuccessNamespace t ->
    FailureUnhandledMembers t

and extract ?exclude_proto_members cx = extract_type cx %> extract_members ?exclude_proto_members cx

and extract_members_as_map ~exclude_proto_members cx this_t =
  match extract ~exclude_proto_members cx this_t |> to_command_result with
  | Ok map -> map
  | Error _ -> SMap.empty
