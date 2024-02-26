(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
module P = Polarity

let pole_TODO = Polarity.Neutral

(* We walk types in a lot of places for all kinds of things, but often most of
   the code is boilerplate. The following visitor class for types aims to
   reduce that boilerplate. It is designed as a fold on the structure of types,
   parameterized by an accumulator.

   WARNING: This is only a partial implementation, sufficient for current
   purposes but intended to be completed in a later diff.
*)
class ['a] t =
  object (self)
    method type_ cx pole (acc : 'a) =
      function
      | OpenT (r, id) -> self#tvar cx pole acc r id
      | DefT (_, t) -> self#def_type cx pole acc t
      | InternalT (ChoiceKitT (_, Trigger)) -> acc
      | InternalT (EnforceUnionOptimized _) -> acc
      | FunProtoT _
      | FunProtoApplyT _
      | FunProtoBindT _
      | FunProtoCallT _
      | ObjProtoT _
      | NullProtoT _ ->
        acc
      | CustomFunT (_, kind) -> self#custom_fun_kind cx acc kind
      | EvalT (t, defer_use_t, id) ->
        let acc = self#type_ cx P.Positive acc t in
        let acc = self#defer_use_type cx acc defer_use_t in
        let acc =
          let pole =
            match (defer_use_t, t) with
            | (TypeDestructorT _, OpenT _) -> P.Neutral
            | (TypeDestructorT _, _) -> P.Positive
          in
          self#eval_id cx pole acc id
        in
        acc
      | GenericT { bound; _ } -> self#type_ cx pole acc bound
      | ExactT (_, t) -> self#type_ cx pole acc t
      | MatchingPropT (_, _, t) -> self#type_ cx pole_TODO acc t
      | KeysT (_, t) -> self#type_ cx P.Positive acc t
      | AnnotT (_, t, _) -> self#type_ cx P.Positive acc t
      | OpaqueT (_, ot) ->
        let { opaque_id = _; underlying_t; super_t; opaque_type_args; opaque_name = _ } = ot in
        let acc =
          self#list
            (fun acc (_, _, t, pole') -> self#type_ cx (P.mult (pole, pole')) acc t)
            acc
            opaque_type_args
        in
        let acc = self#opt (self#type_ cx pole) acc underlying_t in
        let acc = self#opt (self#type_ cx pole) acc super_t in
        acc
      | ModuleT
          {
            module_reason = _;
            module_export_types = exporttypes;
            module_is_strict = _;
            module_available_platforms = _;
          } ->
        self#export_types cx pole acc exporttypes
      | NamespaceT namespace_t -> self#namespace_type cx pole acc namespace_t
      | InternalT (ExtendsT (_, t1, t2)) ->
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | ThisInstanceT (_, t, _, _) -> self#instance_type cx pole acc t
      | ThisTypeAppT (_, t, this, ts_opt) ->
        let acc = self#type_ cx P.Positive acc t in
        let acc = self#type_ cx pole acc this in
        (* If we knew what `t` resolved to, we could determine the polarities for
           `ts`, but in general `t` might be unresolved. Subclasses which have more
           information should override this to be more specific. *)
        let acc = self#opt (self#list (self#type_ cx pole_TODO)) acc ts_opt in
        acc
      | TypeAppT { reason = _; use_op = _; type_; targs; from_value = _; use_desc = _ } ->
        let acc = self#type_ cx P.Positive acc type_ in
        (* If we knew what `t` resolved to, we could determine the polarities for
           `ts`, but in general `t` might be unresolved. Subclasses which have more
           information should override this to be more specific. *)
        let acc = self#list (self#type_ cx pole_TODO) acc targs in
        acc
      | AnyT _ -> acc
      | OptionalT { reason = _; type_ = t; use_desc = _ }
      | MaybeT (_, t) ->
        self#type_ cx pole acc t
      | IntersectionT (_, rep) -> self#list (self#type_ cx pole) acc (InterRep.members rep)
      | UnionT (_, rep) -> self#list (self#type_ cx pole) acc (UnionRep.members rep)

    method def_type cx pole acc =
      function
      | NumT _
      | StrT _
      | BoolT _
      | BigIntT _
      | EmptyT
      | MixedT _
      | SymbolT
      | NullT
      | VoidT ->
        acc
      | EnumT enum
      | EnumObjectT enum ->
        let { enum_name = _; enum_id = _; members = _; representation_t; has_unknown_members = _ } =
          enum
        in
        let acc = self#type_ cx pole acc representation_t in
        acc
      | FunT (static, funtype) ->
        let acc = self#type_ cx pole acc static in
        let acc = self#fun_type cx pole acc funtype in
        acc
      | ObjT objtype -> self#obj_type cx pole acc objtype
      | ArrT arrtype -> self#arr_type cx pole acc arrtype
      | CharSetT _ -> acc
      | ClassT t -> self#type_ cx pole acc t
      | InstanceT t -> self#instance_type cx pole acc t
      | NumericStrKeyT _
      | SingletonStrT _
      | SingletonNumT _
      | SingletonBoolT _
      | SingletonBigIntT _ ->
        acc
      | TypeT (_, t) -> self#type_ cx pole acc t
      | PolyT { tparams; t_out; tparams_loc = _; id = _ } ->
        let acc = self#nel (self#type_param cx pole) acc tparams in
        let acc = self#type_ cx pole acc t_out in
        acc
      | ReactAbstractComponentT { config; instance; renders; component_kind = _ } ->
        let acc = self#type_ cx (P.inv pole) acc config in
        let acc = self#type_ cx pole acc instance in
        let acc = self#type_ cx pole acc renders in
        acc
      | RendersT (NominalRenders { renders_id = _; renders_name = _; renders_super }) ->
        self#type_ cx pole acc renders_super
      | RendersT (StructuralRenders { renders_variant = _; renders_structural_type = t }) ->
        self#type_ cx pole acc t

    method targ cx pole acc =
      function
      | ImplicitArg _ -> acc
      | ExplicitArg t -> self#type_ cx pole acc t

    method private defer_use_type cx acc =
      function
      | TypeDestructorT (_, _, d) -> self#destructor cx acc d

    method private selector cx acc =
      function
      | Prop _ -> acc
      | Elem key -> self#type_ cx pole_TODO acc key
      | ObjRest _ -> acc
      | ArrRest _ -> acc
      | Default -> acc

    method private predicate cx acc =
      function
      | AndP (p1, p2) -> self#list (self#predicate cx) acc [p1; p2]
      | OrP (p1, p2) -> self#list (self#predicate cx) acc [p1; p2]
      | NotP p -> self#predicate cx acc p
      | LeftP (_, t) -> self#type_ cx P.Positive acc t
      | RightP (_, t) -> self#type_ cx P.Positive acc t
      | ExistsP -> acc
      | NullP -> acc
      | MaybeP -> acc
      | SingletonBoolP _ -> acc
      | SingletonStrP _ -> acc
      | SingletonNumP _ -> acc
      | SingletonBigIntP _ -> acc
      | BoolP _ -> acc
      | FunP -> acc
      | NumP _ -> acc
      | BigIntP _ -> acc
      | ObjP -> acc
      | StrP _ -> acc
      | SymbolP _ -> acc
      | VoidP -> acc
      | ArrP -> acc
      | PropExistsP _ -> acc
      | PropNonMaybeP _ -> acc
      | NoP -> acc
      | LatentP ((lazy (_, _, t, targs, argts)), _) ->
        let acc = self#type_ cx P.Positive acc t in
        let acc = self#opt (self#list (self#targ cx pole_TODO)) acc targs in
        let acc = self#list (self#call_arg cx pole_TODO) acc argts in
        acc

    method private call_arg cx pole acc a =
      match a with
      | Arg t -> self#type_ cx pole acc t
      | SpreadArg t -> self#type_ cx pole acc t

    method destructor cx acc =
      function
      | NonMaybeType
      | ReactDRO _
      | MakeHooklike
      | OptionalIndexedAccessResultType _
      | OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessStrLitIndex _ }
      | PropertyType _
      | ValuesType
      | ReadOnlyType
      | PartialType
      | RequiredType
      | ReactElementPropsType
      | ReactElementConfigType
      | ReactElementRefType
      | ReactCheckComponentRef ->
        acc
      | ReactPromoteRendersRepresentation
          {
            should_distribute = _;
            promote_structural_components = _;
            renders_variant = _;
            resolved_elem;
          } ->
        self#opt (self#type_ cx pole_TODO) acc resolved_elem
      | ReactCheckComponentConfig map -> self#namemap (self#prop cx pole_TODO) acc map
      | ReactConfigType default_props -> self#type_ cx pole_TODO acc default_props
      | ElementType { index_type; _ } -> self#type_ cx pole_TODO acc index_type
      | OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessTypeIndex index_type } ->
        self#type_ cx pole_TODO acc index_type
      | SpreadType (_, ts, head_slice) ->
        let acc = self#list (self#object_kit_spread_operand cx) acc ts in
        self#opt (self#object_kit_spread_operand_slice cx) acc head_slice
      | SpreadTupleType { resolved; unresolved; reason_tuple = _; reason_spread = _ } ->
        let acc =
          self#list
            (fun acc resolved_el ->
              match resolved_el with
              | ResolvedArg (element, _) -> self#tuple_element cx pole_TODO acc element
              | ResolvedSpreadArg (_, arr, _) -> self#arr_type cx pole_TODO acc arr
              | ResolvedAnySpreadArg _ -> acc)
            acc
            resolved
        in
        let acc =
          self#list
            (fun acc unresolved_el ->
              match unresolved_el with
              | UnresolvedArg (element, _) -> self#tuple_element cx pole_TODO acc element
              | UnresolvedSpreadArg t -> self#type_ cx pole_TODO acc t)
            acc
            unresolved
        in
        acc
      | RestType (_, t) -> self#type_ cx pole_TODO acc t
      | CallType { from_maptype = _; args } -> self#list (self#type_ cx pole_TODO) acc args
      | ConditionalType { distributive_tparam_name = _; infer_tparams; extends_t; true_t; false_t }
        ->
        let acc = self#list (self#type_param cx pole_TODO) acc infer_tparams in
        let acc = self#type_ cx pole_TODO acc extends_t in
        let acc = self#type_ cx pole_TODO acc true_t in
        let acc = self#type_ cx pole_TODO acc false_t in
        acc
      | TypeMap map -> self#type_map cx acc map
      | MappedType
          { property_type; mapped_type_flags = _; homomorphic; distributive_tparam_name = _ } ->
        let acc = self#type_ cx pole_TODO acc property_type in
        (match homomorphic with
        | SemiHomomorphic t -> self#type_ cx pole_TODO acc t
        | Homomorphic
        | Unspecialized ->
          acc)

    method private custom_fun_kind cx acc =
      function
      | ReactElementFactory t -> self#type_ cx pole_TODO acc t
      | ObjectAssign
      | ObjectGetPrototypeOf
      | ObjectSetPrototypeOf
      | Compose _
      | ReactCreateElement
      | ReactCloneElement
      | DebugPrint
      | DebugThrow
      | DebugSleep ->
        acc

    method private tout cx pole acc (r, id) = self#tvar cx pole acc r id

    (* The default behavior here could be fleshed out a bit, to look up the graph,
       handle Resolved and Unresolved cases, etc. *)
    method tvar _cx _pole acc _r _id = acc

    method dict_type cx pole acc d =
      let { dict_name = _; key; value; dict_polarity = p } = d in
      let acc = self#type_ cx pole_TODO acc key in
      let acc = self#type_ cx (P.mult (pole, p)) acc value in
      acc

    method props cx pole acc id = Context.find_props cx id |> self#namemap (self#prop cx pole) acc

    method prop cx pole acc =
      function
      | Field { preferred_def_locs = _; key_loc = _; type_; polarity } ->
        self#type_ cx (P.mult (pole, polarity)) acc type_
      | Method { key_loc = _; type_ } -> self#type_ cx pole acc type_
      | Get { key_loc = _; type_ } -> self#type_ cx pole acc type_
      | Set { key_loc = _; type_ } -> self#type_ cx (P.inv pole) acc type_
      | GetSet { get_key_loc = _; get_type; set_key_loc = _; set_type } ->
        let acc = self#type_ cx pole acc get_type in
        let acc = self#type_ cx (P.inv pole) acc set_type in
        acc

    method call_prop cx pole acc id =
      let t = Context.find_call cx id in
      self#type_ cx pole acc t

    method exports cx pole acc id =
      let visit acc { name_loc = _; preferred_def_locs = _; type_ } =
        self#type_ cx pole acc type_
      in
      Context.find_exports cx id |> self#namemap visit acc

    method eval_id cx pole acc id =
      match Eval.Map.find_opt id (Context.evaluated cx) with
      | None -> acc
      | Some t -> self#type_ cx pole acc t

    method type_param cx pole acc tp =
      let { reason = _; name = _; bound; default; polarity = p; is_this = _ } = tp in
      let pole = P.mult (pole, p) in
      let acc = self#type_ cx pole acc bound in
      self#opt (self#type_ cx pole) acc default

    method fun_type cx pole acc ft =
      let {
        this_t = (this_t, _);
        params;
        rest_param;
        return_t;
        predicate;
        def_reason = _;
        hook = _;
      } =
        ft
      in
      let acc = self#type_ cx pole acc this_t in
      let acc = self#list (fun acc (_, t) -> self#type_ cx (P.inv pole) acc t) acc params in
      let acc = self#opt (fun acc (_, _, t) -> self#type_ cx (P.inv pole) acc t) acc rest_param in
      let acc = self#type_ cx pole acc return_t in
      let acc = self#opt (self#fun_predicate cx pole) acc predicate in
      acc

    method private fun_predicate cx pole acc predicate =
      match predicate with
      | PredBased p -> self#predicate_maps cx acc p
      | TypeGuardBased { param_name = _; type_guard = t } -> self#type_ cx pole acc t

    method private predicate_maps cx acc (_, (lazy (pmap, nmap))) =
      let acc = Key_map.fold (fun _ p acc -> self#predicate cx acc p) pmap acc in
      let acc = Key_map.fold (fun _ p acc -> self#predicate cx acc p) nmap acc in
      acc

    method private obj_flags cx pole acc flags =
      match flags.obj_kind with
      | Indexed dict -> self#dict_type cx pole acc dict
      | Exact
      | Inexact ->
        acc

    method private obj_type cx pole acc o =
      (* We intentionally do not visit reachable_targs. By definition, they are already reachable
       * by traversing the other fields. Until substitution keeps track of polarity, visitng the
       * other fields will be more accurate *)
      let { props_tmap; proto_t; call_t; flags; reachable_targs = _ } = o in
      let acc = self#obj_flags cx pole acc flags in
      let acc = self#props cx pole acc props_tmap in
      let acc = self#type_ cx pole acc proto_t in
      let acc = self#opt (self#call_prop cx pole) acc call_t in
      acc

    method private namespace_type cx pole acc ns =
      let { values_type; types_tmap } = ns in
      let acc = self#type_ cx pole acc values_type in
      let acc = self#props cx pole acc types_tmap in
      acc

    method private arr_type cx pole acc =
      function
      | ArrayAT { elem_t; tuple_view = None; react_dro = _ } -> self#type_ cx P.Neutral acc elem_t
      | ArrayAT { elem_t; tuple_view = Some (elements, _); react_dro = _ }
      | TupleAT { elem_t; elements; arity = _; react_dro = _ } ->
        let acc = self#type_ cx P.Neutral acc elem_t in
        let acc = self#list (self#tuple_element cx pole) acc elements in
        acc
      | ROArrayAT (t, _) -> self#type_ cx pole acc t

    method private tuple_element cx pole acc element =
      let (TupleElement { t; polarity = p; reason = _; name = _; optional = _ }) = element in
      self#type_ cx (P.mult (pole, p)) acc t

    method private inst_type cx pole acc i =
      let {
        class_id = _;
        class_name = _;
        type_args;
        own_props;
        proto_props;
        inst_call_t;
        initialized_fields = _;
        initialized_static_fields = _;
        inst_kind = _;
        inst_dict;
        class_private_fields;
        class_private_static_fields;
        class_private_methods;
        class_private_static_methods;
      } =
        i
      in
      let acc =
        self#list
          (fun acc (_, _, t, pole') -> self#type_ cx (P.mult (pole, pole')) acc t)
          acc
          type_args
      in
      let acc = self#props cx pole acc own_props in
      let acc = self#props cx pole acc proto_props in
      let acc = self#opt (self#call_prop cx pole) acc inst_call_t in
      let acc = self#opt (self#dict_type cx pole_TODO) acc inst_dict in
      let acc = self#props cx pole_TODO acc class_private_fields in
      let acc = self#props cx pole_TODO acc class_private_static_fields in
      let acc = self#props cx pole_TODO acc class_private_methods in
      let acc = self#props cx pole_TODO acc class_private_static_methods in
      acc

    method instance_type cx pole acc { static; super; implements; inst } =
      let acc = self#type_ cx pole acc static in
      let acc = self#type_ cx pole acc super in
      let acc = self#list (self#type_ cx pole_TODO) acc implements in
      let acc = self#inst_type cx pole acc inst in
      acc

    method private export_types cx pole acc e =
      let { value_exports_tmap; type_exports_tmap; cjs_export; has_every_named_export = _ } = e in
      let acc = self#exports cx pole acc value_exports_tmap in
      let acc = self#exports cx pole acc type_exports_tmap in
      let acc = self#opt (self#type_ cx pole) acc cjs_export in
      acc

    method private type_map cx acc =
      function
      | TupleMap t
      | ObjectMap t
      | ObjectMapi t
      | ObjectMapConst t ->
        self#type_ cx pole_TODO acc t
      | ObjectKeyMirror -> acc

    method private object_kit_spread_operand_slice
        cx acc { Object.Spread.reason = _; prop_map; dict; generics = _; reachable_targs = _ } =
      (* See obj_type for why we don't visit reachable targs *)
      let acc = self#namemap (Property.fold_t (self#type_ cx pole_TODO)) acc prop_map in
      let acc = self#opt (self#dict_type cx pole_TODO) acc dict in
      acc

    method private object_kit_spread_operand cx acc =
      Object.Spread.(
        function
        | Slice operand_slice -> self#object_kit_spread_operand_slice cx acc operand_slice
        | Type t -> self#type_ cx pole_TODO acc t
      )

    method private list : 't. ('a -> 't -> 'a) -> 'a -> 't list -> 'a = List.fold_left

    method private nel : 't. ('a -> 't -> 'a) -> 'a -> 't Nel.t -> 'a = Nel.fold_left

    method private opt : 't. ('a -> 't -> 'a) -> 'a -> 't option -> 'a =
      (fun f acc opt -> Base.Option.fold opt ~init:acc ~f)

    method private smap : 't. ('a -> 't -> 'a) -> 'a -> 't SMap.t -> 'a =
      (fun f acc smap -> SMap.fold (fun _ t acc -> f acc t) smap acc)

    method private substmap : 't. ('a -> 't -> 'a) -> 'a -> 't Subst_name.Map.t -> 'a =
      (fun f acc smap -> Subst_name.Map.fold (fun _ t acc -> f acc t) smap acc)

    method private namemap : 't. ('a -> 't -> 'a) -> 'a -> 't NameUtils.Map.t -> 'a =
      (fun f acc smap -> NameUtils.Map.fold (fun _ t acc -> f acc t) smap acc)
  end
