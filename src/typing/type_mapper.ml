(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

(* NOTE: While union flattening could be performed at any time, it is most effective when we know
   that all tvars have been resolved. *)
let union_flatten =
  let rec union_flatten cx seen ts = Base.List.(ts >>= flatten cx seen)
  and flatten cx seen t =
    match t with
    | OpenT (_, id) ->
      if ISet.mem id !seen then
        []
      else (
        seen := ISet.add id !seen;
        Type.Constraint.(
          match Context.find_graph cx id with
          | Resolved (_, t')
          | FullyResolved (_, (lazy t')) ->
            flatten cx seen t'
          | Unresolved _ -> [t]
        )
      )
    | AnnotT (_, t, _) -> flatten cx seen t
    | UnionT (_, rep) -> union_flatten cx seen @@ UnionRep.members rep
    | MaybeT (r, t) ->
      DefT (r, Trust.bogus_trust (), NullT)
      :: DefT (r, Trust.bogus_trust (), VoidT)
      :: flatten cx seen t
    | OptionalT { reason = r; type_ = t; use_desc } ->
      let void_t = VoidT.why_with_use_desc ~use_desc r |> with_trust Trust.bogus_trust in
      void_t :: flatten cx seen t
    | DefT (_, _, EmptyT) -> []
    | _ -> [t]
  in
  (fun cx ts -> union_flatten cx (ref ISet.empty) ts)

(* This class should be used when trying to perform some mapping function on
 * a type. It will recurse through the structure of the type, applying it to
 * each sub-part.
 *)

class virtual ['a] t =
  object (self)
    method type_ cx (map_cx : 'a) t =
      match t with
      | OpenT (r, id) ->
        let id' = self#tvar cx map_cx r id in
        if id' == id then
          t
        else
          OpenT (r, id')
      | DefT (r, trust, t') ->
        let t'' = self#def_type cx map_cx t' in
        if t' == t'' then
          t
        else
          DefT (r, trust, t'')
      | EvalT (t', dt, id) ->
        let t'' = self#type_ cx map_cx t' in
        let dt' = self#defer_use_type cx map_cx dt in
        let id' = self#eval_id cx map_cx id in
        if t' == t'' && dt == dt' && id' == id then
          t
        else
          EvalT (t'', dt', id')
      | ThisClassT (r, t', i, n) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ThisClassT (r, t'', i, n)
      | ThisTypeAppT (r, t1, t2, tlist_opt) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        let tlist_opt' =
          OptionUtils.ident_map (ListUtils.ident_map (self#type_ cx map_cx)) tlist_opt
        in
        if t1' == t1 && t2' == t2 && tlist_opt' == tlist_opt then
          t
        else
          ThisTypeAppT (r, t1', t2', tlist_opt')
      | TypeAppT (r, op, t', ts) ->
        let t'' = self#type_ cx map_cx t' in
        let ts' = ListUtils.ident_map (self#type_ cx map_cx) ts in
        if t' == t'' && ts == ts' then
          t
        else
          TypeAppT (r, op, t'', ts')
      | ExactT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ExactT (r, t'')
      | FunProtoT _
      | ObjProtoT _
      | NullProtoT _
      | FunProtoApplyT _
      | FunProtoBindT _
      | FunProtoCallT _ ->
        t
      | GenericT ({ bound; _ } as generic) ->
        let bound' = self#type_ cx map_cx bound in
        if bound' == bound then
          t
        else
          GenericT { generic with bound = bound' }
      | ShapeT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ShapeT (r, t'')
      | MatchingPropT (r, x, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          MatchingPropT (r, x, t'')
      | KeysT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          KeysT (r, t'')
      | AnnotT (r, t', use_desc) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          AnnotT (r, t'', use_desc)
      | OpaqueT (r, opaquetype) ->
        let underlying_t = OptionUtils.ident_map (self#type_ cx map_cx) opaquetype.underlying_t in
        let super_t = OptionUtils.ident_map (self#type_ cx map_cx) opaquetype.super_t in
        let opaque_type_args =
          ListUtils.ident_map
            (fun x ->
              let (s, r, t, p) = x in
              let t' = self#type_ cx map_cx t in
              if t == t' then
                x
              else
                (s, r, t', p))
            opaquetype.opaque_type_args
        in
        if
          underlying_t == opaquetype.underlying_t
          && super_t == opaquetype.super_t
          && opaque_type_args == opaquetype.opaque_type_args
        then
          t
        else
          OpaqueT (r, { opaquetype with underlying_t; super_t; opaque_type_args })
      | ModuleT (r, exporttypes, is_strict) ->
        let exporttypes' = self#export_types cx map_cx exporttypes in
        if exporttypes == exporttypes' then
          t
        else
          ModuleT (r, exporttypes', is_strict)
      | InternalT (ExtendsT (r, t1, t2)) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          InternalT (ExtendsT (r, t1', t2'))
      | InternalT (ChoiceKitT _) -> t
      | TypeDestructorTriggerT (u, r, repos, d, x) ->
        let d' = self#destructor cx map_cx d in
        let x' = self#tout cx map_cx x in
        if d == d' && x == x' then
          t
        else
          TypeDestructorTriggerT (u, r, repos, d', x')
      | CustomFunT (r, kind) ->
        let kind' = self#custom_fun_kind cx map_cx kind in
        if kind' == kind then
          t
        else
          CustomFunT (r, kind')
      | OpenPredT { reason = r; base_t = t'; m_pos = map1; m_neg = map2 } ->
        let t'' = self#type_ cx map_cx t' in
        let map1' = Key_map.map (self#predicate cx map_cx) map1 in
        let map2' = Key_map.map (self#predicate cx map_cx) map2 in
        if t'' == t' then
          t
        else
          OpenPredT { reason = r; base_t = t''; m_pos = map1'; m_neg = map2' }
      | AnyT _ -> t
      | OptionalT { reason; type_ = t'; use_desc } ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          OptionalT { reason; type_ = t''; use_desc }
      | MaybeT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          MaybeT (r, t'')
      | IntersectionT (r, irep) ->
        let irep' = InterRep.ident_map (self#type_ cx map_cx) irep in
        if irep == irep' then
          t
        else
          IntersectionT (r, irep')
      | UnionT (r, urep) ->
        let urep' = UnionRep.ident_map (self#type_ cx map_cx) urep in
        if urep' == urep then
          t
        else
          UnionT (r, urep')

    method private tout cx map_cx ((r, tvar) as t) =
      let tvar' = self#tvar cx map_cx r tvar in
      if tvar == tvar' then
        t
      else
        (r, tvar')

    method virtual tvar : Context.t -> 'a -> Reason.t -> Type.ident -> Type.ident

    method targ cx map_cx t =
      match t with
      | ImplicitArg _ -> t
      | ExplicitArg t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ExplicitArg t''

    method enum cx map_cx e =
      let { enum_id; members; representation_t; has_unknown_members } = e in
      let representation_t' = self#type_ cx map_cx representation_t in
      if representation_t' = representation_t then
        e
      else
        { enum_id; members; representation_t = representation_t'; has_unknown_members }

    method def_type cx map_cx t =
      match t with
      | NumT _
      | StrT _
      | BoolT _
      | BigIntT _
      | EmptyT
      | MixedT _
      | SymbolT
      | NullT
      | VoidT ->
        t
      | FunT (s, f) ->
        let s' = self#type_ cx map_cx s in
        let f' = self#fun_type cx map_cx f in
        if s == s' && f == f' then
          t
        else
          FunT (s', f')
      | ObjT objtype ->
        let objtype' = self#obj_type cx map_cx objtype in
        if objtype' == objtype then
          t
        else
          ObjT objtype'
      | ArrT arrtype ->
        let arrtype' = self#arr_type cx map_cx arrtype in
        if arrtype == arrtype' then
          t
        else
          ArrT arrtype'
      | CharSetT _ -> t
      | ClassT t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ClassT t''
      | EnumT enum ->
        let enum' = self#enum cx map_cx enum in
        if enum' == enum then
          t
        else
          EnumT enum'
      | EnumObjectT enum ->
        let enum' = self#enum cx map_cx enum in
        if enum' == enum then
          t
        else
          EnumObjectT enum'
      | InstanceT (st, su, impl, instt) ->
        let st' = self#type_ cx map_cx st in
        let su' = self#type_ cx map_cx su in
        let impl' = ListUtils.ident_map (self#type_ cx map_cx) impl in
        let instt' = self#inst_type cx map_cx instt in
        if st' == st && su' == su && impl' == impl && instt' == instt then
          t
        else
          InstanceT (st', su', impl', instt')
      | SingletonStrT _
      | SingletonNumT _
      | SingletonBoolT _
      | SingletonBigIntT _ ->
        t
      | TypeT (s, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          TypeT (s, t'')
      | PolyT { tparams_loc; tparams = tparamlist; t_out = t'; _ } ->
        let tparamlist' = Nel.ident_map (self#type_param cx map_cx) tparamlist in
        let t'' = self#type_ cx map_cx t' in
        if tparamlist == tparamlist' && t' == t'' then
          t
        else
          PolyT { tparams_loc; tparams = tparamlist'; t_out = t''; id = Type.Poly.generate_id () }
      | IdxWrapper t' ->
        let t'' = self#type_ cx map_cx t' in
        if t' == t'' then
          t
        else
          IdxWrapper t''
      | ReactAbstractComponentT { config; instance } ->
        let config' = self#type_ cx map_cx config in
        let instance' = self#type_ cx map_cx instance in
        if config' == config && instance' == instance then
          t
        else
          ReactAbstractComponentT { config = config'; instance = instance' }

    method defer_use_type cx map_cx t =
      match t with
      | LatentPredT (r, p) ->
        let p' = self#predicate cx map_cx p in
        if p' == p then
          t
        else
          LatentPredT (r, p')
      | TypeDestructorT (u, r, d) ->
        let d' = self#destructor cx map_cx d in
        if d == d' then
          t
        else
          TypeDestructorT (u, r, d')

    method export_types cx map_cx ({ exports_tmap; cjs_export; has_every_named_export } as t) =
      let exports_tmap' = self#exports cx map_cx exports_tmap in
      let cjs_export' = OptionUtils.ident_map (self#type_ cx map_cx) cjs_export in
      if exports_tmap == exports_tmap' && cjs_export == cjs_export' then
        t
      else
        { exports_tmap = exports_tmap'; cjs_export = cjs_export'; has_every_named_export }

    method fun_type
        cx
        map_cx
        ({ this_t = (this, subtyping); params; rest_param; return_t; is_predicate; def_reason } as t)
        =
      let this' = self#type_ cx map_cx this in
      let params' =
        ListUtils.ident_map
          (fun ((name, t) as param) ->
            let t' = self#type_ cx map_cx t in
            if t' == t then
              param
            else
              (name, t'))
          params
      in
      let rest_param' =
        match rest_param with
        | None -> rest_param
        | Some (name, loc, t) ->
          let t' = self#type_ cx map_cx t in
          if t' == t then
            rest_param
          else
            Some (name, loc, t')
      in
      let return_t' = self#type_ cx map_cx return_t in
      if this' == this && return_t' == return_t && params' == params && rest_param' == rest_param
      then
        t
      else
        let this_t = (this', subtyping) in
        let return_t = return_t' in
        let params = params' in
        let rest_param = rest_param' in
        { this_t; params; rest_param; return_t; is_predicate; def_reason }

    method inst_type cx map_cx i =
      let {
        class_id;
        type_args;
        own_props;
        proto_props;
        inst_call_t;
        initialized_fields;
        initialized_static_fields;
        has_unknown_react_mixins;
        inst_kind;
      } =
        i
      in
      let type_args' =
        ListUtils.ident_map
          (fun x ->
            let (s, r, t, p) = x in
            let t' = self#type_ cx map_cx t in
            if t == t' then
              x
            else
              (s, r, t', p))
          type_args
      in
      let own_props' = self#props cx map_cx own_props in
      let proto_props' = self#props cx map_cx proto_props in
      let inst_call_t' = OptionUtils.ident_map (self#call_prop cx map_cx) inst_call_t in
      if
        type_args == type_args'
        && own_props == own_props'
        && proto_props == proto_props'
        && inst_call_t == inst_call_t'
      then
        i
      else
        {
          class_id;
          type_args = type_args';
          own_props = own_props';
          proto_props = proto_props';
          inst_call_t = inst_call_t';
          initialized_fields;
          initialized_static_fields;
          has_unknown_react_mixins;
          inst_kind;
        }

    method type_param cx map_cx ({ reason; name; bound; polarity; default; is_this } as t) =
      let bound' = self#type_ cx map_cx bound in
      let default' = OptionUtils.ident_map (self#type_ cx map_cx) default in
      if bound == bound' && default == default' then
        t
      else
        let bound = bound' in
        let default = default' in
        { reason; name; bound; polarity; default; is_this }

    method selector cx map_cx t =
      match t with
      | Prop _ -> t
      | Elem t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          Elem t''
      | ObjRest _
      | ArrRest _
      | Default ->
        t

    method destructor cx map_cx t =
      match t with
      | NonMaybeType
      | PropertyType _
      | OptionalIndexedAccessResultType _
      | OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessStrLitIndex _ } ->
        t
      | ElementType { index_type } ->
        let index_type' = self#type_ cx map_cx index_type in
        if index_type' == index_type then
          t
        else
          ElementType { index_type = index_type' }
      | OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessTypeIndex index_type } ->
        let index_type' = self#type_ cx map_cx index_type in
        if index_type' == index_type then
          t
        else
          OptionalIndexedAccessNonMaybeType { index = OptionalIndexedAccessTypeIndex index_type' }
      | ReadOnlyType -> t
      | PartialType -> t
      | SpreadType (options, tlist, acc) ->
        let tlist' = ListUtils.ident_map (self#object_kit_spread_operand cx map_cx) tlist in
        let acc' = OptionUtils.ident_map (self#object_kit_spread_operand_slice cx map_cx) acc in
        if tlist' == tlist && acc == acc' then
          t
        else
          SpreadType (options, tlist', acc')
      | RestType (options, x) ->
        let x' = self#type_ cx map_cx x in
        if x' == x then
          t
        else
          RestType (options, x')
      | ValuesType -> t
      | CallType args ->
        let args' = ListUtils.ident_map (self#type_ cx map_cx) args in
        if args' == args then
          t
        else
          CallType args'
      | TypeMap tmap ->
        let tmap' = self#type_map cx map_cx tmap in
        if tmap' == tmap then
          t
        else
          TypeMap tmap'
      | ReactConfigType default_props ->
        let default_props' = self#type_ cx map_cx default_props in
        if default_props' == default_props then
          t
        else
          ReactConfigType default_props'
      | ReactElementPropsType
      | ReactElementConfigType
      | ReactElementRefType
      | IdxUnwrapType ->
        t

    method object_kit_spread_operand_slice
        cx map_cx ({ Object.Spread.reason; prop_map; dict; generics } as slice) =
      let prop_map' =
        NameUtils.Map.ident_map (Property.ident_map_t (self#type_ cx map_cx)) prop_map
      in
      let dict' = OptionUtils.ident_map (self#dict_type cx map_cx) dict in
      if prop_map' == prop_map && dict' == dict then
        slice
      else
        { Object.Spread.reason; prop_map = prop_map'; dict = dict'; generics }

    method object_kit_spread_operand cx map_cx operand =
      Object.Spread.(
        match operand with
        | Slice slice ->
          let slice' = self#object_kit_spread_operand_slice cx map_cx slice in
          if slice' == slice then
            operand
          else
            Slice slice'
        | Type t ->
          let t' = self#type_ cx map_cx t in
          if t' == t then
            operand
          else
            Type t'
      )

    method private custom_fun_kind cx map_cx kind =
      match kind with
      | ReactPropType (React.PropType.Primitive (b, t)) ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          kind
        else
          ReactPropType (React.PropType.Primitive (b, t'))
      | ReactElementFactory t ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          kind
        else
          ReactElementFactory t'
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
        kind

    method virtual exports : Context.t -> 'a -> Type.Exports.id -> Type.Exports.id

    method obj_flags cx map_cx flags =
      match flags.obj_kind with
      | Indexed dict ->
        let dict' = self#dict_type cx map_cx dict in
        if dict == dict' then
          flags
        else
          { flags with obj_kind = Indexed dict' }
      | Exact
      | Inexact ->
        flags

    method obj_type cx map_cx t =
      let { flags; props_tmap; proto_t; call_t; reachable_targs } = t in
      let flags' = self#obj_flags cx map_cx flags in
      let props_tmap' = self#props cx map_cx props_tmap in
      let proto_t' = self#type_ cx map_cx proto_t in
      let call_t' = OptionUtils.ident_map (self#call_prop cx map_cx) call_t in
      let reachable_targs' =
        ListUtils.ident_map
          (fun ((t, p) as tup) ->
            let t' = self#type_ cx map_cx t in
            if t == t' then
              tup
            else
              (t', p))
          reachable_targs
      in
      if
        flags' == flags
        && props_tmap' == props_tmap
        && proto_t' == proto_t
        && call_t' == call_t
        && reachable_targs == reachable_targs'
      then
        t
      else
        {
          flags = flags';
          props_tmap = props_tmap';
          proto_t = proto_t';
          call_t = call_t';
          reachable_targs = reachable_targs';
        }

    method virtual call_prop : Context.t -> 'a -> int -> int

    method dict_type cx map_cx ({ dict_name; key; value; dict_polarity } as t) =
      let key' = self#type_ cx map_cx key in
      let value' = self#type_ cx map_cx value in
      if key' == key && value' == value then
        t
      else
        let key = key' in
        let value = value' in
        { dict_name; key; value; dict_polarity }

    method arr_type cx map_cx t =
      match t with
      | ArrayAT (t', tlistopt) ->
        let t'' = self#type_ cx map_cx t' in
        let tlistopt' =
          OptionUtils.ident_map (ListUtils.ident_map (self#type_ cx map_cx)) tlistopt
        in
        if t'' == t' && tlistopt' == tlistopt then
          t
        else
          ArrayAT (t'', tlistopt')
      | TupleAT { elem_t; elements } ->
        let elem_t' = self#type_ cx map_cx elem_t in
        let elements' =
          ListUtils.ident_map
            (fun (TupleElement { name; t; polarity } as element) ->
              let t' = self#type_ cx map_cx t in
              if t' == t then
                element
              else
                TupleElement { name; t = t'; polarity })
            elements
        in
        if elem_t' == elem_t && elements' == elements then
          t
        else
          TupleAT { elem_t = elem_t'; elements = elements' }
      | ROArrayAT t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ROArrayAT t''

    method predicate cx map_cx p =
      match p with
      | AndP (p1, p2) ->
        let p1' = self#predicate cx map_cx p1 in
        let p2' = self#predicate cx map_cx p2 in
        if p1' == p1 && p2' == p2 then
          p
        else
          AndP (p1', p2')
      | OrP (p1, p2) ->
        let p1' = self#predicate cx map_cx p1 in
        let p2' = self#predicate cx map_cx p2 in
        if p1' == p1 && p2' == p2 then
          p
        else
          OrP (p1', p2')
      | NotP p' ->
        let p'' = self#predicate cx map_cx p' in
        if p'' == p' then
          p
        else
          NotP p''
      | LeftP (test, t) ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          p
        else
          LeftP (test, t')
      | RightP (test, t) ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          p
        else
          RightP (test, t')
      | ExistsP
      | NullP
      | MaybeP
      | SingletonBoolP _
      | SingletonStrP _
      | SingletonNumP _
      | SingletonBigIntP _
      | BoolP _
      | FunP
      | NumP _
      | BigIntP _
      | ObjP
      | StrP _
      | SymbolP _
      | VoidP
      | ArrP
      | PropNonMaybeP _
      | PropExistsP _ ->
        p
      | LatentP (t, i) ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          p
        else
          LatentP (t', i)

    method type_map cx map_cx t =
      match t with
      | TupleMap t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          TupleMap t''
      | ObjectMap t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ObjectMap t''
      | ObjectMapi t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ObjectMapi t''
      | ObjectKeyMirror -> ObjectKeyMirror
      | ObjectMapConst t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ObjectMapConst t''

    method virtual props : Context.t -> 'a -> Properties.id -> Properties.id

    method virtual eval_id : Context.t -> 'a -> Eval.id -> Eval.id

    method prop cx map_cx prop =
      match prop with
      | Field (l, t, p) ->
        let t' = self#type_ cx map_cx t in
        if t == t' then
          prop
        else
          Field (l, t', p)
      | Method (l, t) ->
        let t' = self#type_ cx map_cx t in
        if t == t' then
          prop
        else
          Method (l, t')
      | Get (l, t) ->
        let t' = self#type_ cx map_cx t in
        if t == t' then
          prop
        else
          Get (l, t')
      | Set (l, t) ->
        let t' = self#type_ cx map_cx t in
        if t == t' then
          prop
        else
          Set (l, t')
      | GetSet (l1, t1, l2, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1 == t1' && t2 == t2' then
          prop
        else
          GetSet (l1, t1', l2, t2')

    method class_binding cx map_cx binding =
      let class_private_fields = self#props cx map_cx binding.class_private_fields in
      let class_private_static_fields = self#props cx map_cx binding.class_private_static_fields in
      let class_private_methods = self#props cx map_cx binding.class_private_methods in
      let class_private_static_methods =
        self#props cx map_cx binding.class_private_static_methods
      in
      if
        class_private_fields == binding.class_private_fields
        && class_private_static_fields == binding.class_private_static_fields
        && class_private_methods == binding.class_private_methods
        && class_private_static_methods == binding.class_private_static_methods
      then
        binding
      else
        {
          binding with
          class_private_fields;
          class_private_static_fields;
          class_private_methods;
          class_private_static_methods;
        }

    method call_arg cx map_cx t =
      match t with
      | Arg t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          Arg t''
      | SpreadArg t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          SpreadArg t''

    method fun_call_type cx map_cx t =
      let {
        call_this_t;
        call_targs;
        call_args_tlist;
        call_tout;
        call_strict_arity;
        call_speculation_hint_state;
      } =
        t
      in
      let call_this_t' = self#type_ cx map_cx call_this_t in
      let call_targs' =
        OptionUtils.ident_map (ListUtils.ident_map (self#targ cx map_cx)) call_targs
      in
      let call_args_tlist' = ListUtils.ident_map (self#call_arg cx map_cx) call_args_tlist in
      let call_tout' = self#tout cx map_cx call_tout in
      if
        call_this_t' == call_this_t
        && call_targs' == call_targs
        && call_args_tlist' == call_args_tlist
        && call_tout' == call_tout
      then
        t
      else
        {
          call_this_t = call_this_t';
          call_targs = call_targs';
          call_args_tlist = call_args_tlist';
          call_tout = call_tout';
          call_strict_arity;
          call_speculation_hint_state;
        }
  end
