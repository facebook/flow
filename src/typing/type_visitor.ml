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
      | DefT (_, _, t) -> self#def_type cx pole acc t
      | InternalT (ChoiceKitT (_, Trigger)) -> acc
      | TypeDestructorTriggerT (_, _, _, d, tout) ->
        let acc = self#destructor cx acc d in
        let acc = self#tout cx pole acc tout in
        acc
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
            | (LatentPredT _, _) -> pole
            | (TypeDestructorT _, OpenT _) -> P.Neutral
            | (TypeDestructorT _, _) -> P.Positive
          in
          self#eval_id cx pole acc id
        in
        acc
      | GenericT { bound; _ } -> self#type_ cx pole acc bound
      | ExactT (_, t) -> self#type_ cx pole acc t
      | ShapeT (_, t) -> self#type_ cx pole acc t
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
      | ModuleT (_, exporttypes, _) -> self#export_types cx pole acc exporttypes
      | InternalT (ExtendsT (_, t1, t2)) ->
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | OpenPredT { base_t = t; m_pos = p_map; m_neg = n_map; reason = _ } ->
        let acc = self#type_ cx pole acc t in
        let acc = self#list (self#predicate cx) acc (Key_map.values p_map) in
        let acc = self#list (self#predicate cx) acc (Key_map.values n_map) in
        acc
      | ThisClassT (_, t, _, _) -> self#type_ cx pole acc t
      | ThisTypeAppT (_, t, this, ts_opt) ->
        let acc = self#type_ cx P.Positive acc t in
        let acc = self#type_ cx pole acc this in
        (* If we knew what `t` resolved to, we could determine the polarities for
           `ts`, but in general `t` might be unresolved. Subclasses which have more
           information should override this to be more specific. *)
        let acc = self#opt (self#list (self#type_ cx pole_TODO)) acc ts_opt in
        acc
      | TypeAppT (_, _, t, ts) ->
        let acc = self#type_ cx P.Positive acc t in
        (* If we knew what `t` resolved to, we could determine the polarities for
           `ts`, but in general `t` might be unresolved. Subclasses which have more
           information should override this to be more specific. *)
        let acc = self#list (self#type_ cx pole_TODO) acc ts in
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
        let { enum_id = _; members = _; representation_t; has_unknown_members = _ } = enum in
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
      | InstanceT (static, super, implements, insttype) ->
        let acc = self#type_ cx pole acc static in
        let acc = self#type_ cx pole acc super in
        let acc = self#list (self#type_ cx pole_TODO) acc implements in
        let acc = self#inst_type cx pole acc insttype in
        acc
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
      | IdxWrapper t -> self#type_ cx pole acc t
      | ReactAbstractComponentT { config; instance } ->
        let acc = self#type_ cx (P.inv pole) acc config in
        self#type_ cx pole acc instance

    method targ cx pole acc =
      function
      | ImplicitArg _ -> acc
      | ExplicitArg t -> self#type_ cx pole acc t

    method private defer_use_type cx acc =
      function
      | LatentPredT (_, p) -> self#predicate cx acc p
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
      | LatentP (t, _) -> self#type_ cx P.Positive acc t

    method destructor cx acc =
      function
      | NonMaybeType
      | OptionalIndexedAccessResultType _
      | OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessStrLitIndex _ }
      | PropertyType _
      | ValuesType
      | ReadOnlyType
      | PartialType
      | ReactElementPropsType
      | ReactElementConfigType
      | ReactElementRefType
      | IdxUnwrapType ->
        acc
      | ReactConfigType default_props -> self#type_ cx pole_TODO acc default_props
      | ElementType { index_type; _ } -> self#type_ cx pole_TODO acc index_type
      | OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessTypeIndex index_type } ->
        self#type_ cx pole_TODO acc index_type
      | SpreadType (_, ts, head_slice) ->
        let acc = self#list (self#object_kit_spread_operand cx) acc ts in
        self#opt (self#object_kit_spread_operand_slice cx) acc head_slice
      | RestType (_, t) -> self#type_ cx pole_TODO acc t
      | CallType args -> self#list (self#type_ cx pole_TODO) acc args
      | TypeMap map -> self#type_map cx acc map

    method private custom_fun_kind cx acc =
      function
      | ReactPropType (React.PropType.Primitive (_, t))
      | ReactElementFactory t ->
        self#type_ cx pole_TODO acc t
      | ObjectAssign
      | ObjectGetPrototypeOf
      | ObjectSetPrototypeOf
      | Compose _
      | ReactPropType _
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
      | Field (_, t, p) -> self#type_ cx (P.mult (pole, p)) acc t
      | Method (_, t) -> self#type_ cx pole acc t
      | Get (_, t) -> self#type_ cx pole acc t
      | Set (_, t) -> self#type_ cx (P.inv pole) acc t
      | GetSet (_, t1, _, t2) ->
        let acc = self#type_ cx pole acc t1 in
        let acc = self#type_ cx (P.inv pole) acc t2 in
        acc

    method call_prop cx pole acc id =
      let t = Context.find_call cx id in
      self#type_ cx pole acc t

    method exports cx pole acc id =
      let visit_pair acc (_loc, t) = self#type_ cx pole acc t in
      Context.find_exports cx id |> self#namemap visit_pair acc

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
      let { this_t = (this_t, _); params; rest_param; return_t; is_predicate = _; def_reason = _ } =
        ft
      in
      let acc = self#type_ cx pole acc this_t in
      let acc = self#list (fun acc (_, t) -> self#type_ cx (P.inv pole) acc t) acc params in
      let acc = self#opt (fun acc (_, _, t) -> self#type_ cx (P.inv pole) acc t) acc rest_param in
      let acc = self#type_ cx pole acc return_t in
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

    method private arr_type cx pole acc =
      function
      | ArrayAT (t, None) -> self#type_ cx P.Neutral acc t
      | ArrayAT (t, Some ts) ->
        let acc = self#type_ cx P.Neutral acc t in
        let acc = self#list (self#type_ cx P.Neutral) acc ts in
        acc
      | TupleAT { elem_t; elements } ->
        let acc = self#type_ cx P.Neutral acc elem_t in
        let acc =
          self#list
            (fun acc (TupleElement { t; polarity = p; name = _ }) ->
              self#type_ cx (P.mult (pole, p)) acc t)
            acc
            elements
        in
        acc
      | ROArrayAT t -> self#type_ cx pole acc t

    method private inst_type cx pole acc i =
      let {
        class_id = _;
        type_args;
        own_props;
        proto_props;
        inst_call_t;
        initialized_fields = _;
        initialized_static_fields = _;
        has_unknown_react_mixins = _;
        inst_kind = _;
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
      acc

    method private export_types cx pole acc e =
      let { exports_tmap; cjs_export; has_every_named_export = _ } = e in
      let acc = self#exports cx pole acc exports_tmap in
      let acc = self#opt (self#type_ cx pole) acc cjs_export in
      acc

    method class_binding
        cx
        acc
        {
          class_private_fields;
          class_private_static_fields;
          class_private_methods;
          class_private_static_methods;
          _;
        } =
      let acc = self#props cx pole_TODO acc class_private_fields in
      let acc = self#props cx pole_TODO acc class_private_static_fields in
      let acc = self#props cx pole_TODO acc class_private_methods in
      let acc = self#props cx pole_TODO acc class_private_static_methods in
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
        cx acc { Object.Spread.reason = _; prop_map; dict; generics = _ } =
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
