(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

let maybe_known f x =
  React.CreateClass.(
    match x with
    | Known x' ->
      let x'' = f x' in
      if x'' == x' then
        x
      else
        Known x''
    | Unknown x -> Unknown x)

let unwrap_type =
  let rec unwrap seen cx t =
    match t with
    | OpenT (_, id) ->
      if ISet.mem id !seen then
        t
      else (
        seen := ISet.add id !seen;
        Constraint.(
          match Context.find_graph cx id with
          | Resolved (_, t')
          | FullyResolved (_, t') ->
            unwrap seen cx t'
          | Unresolved _ -> t)
      )
    | AnnotT (_, t, _)
    | ReposT (_, t) ->
      unwrap seen cx t
    | t -> t
  in
  (fun cx -> unwrap (ref ISet.empty) cx)

(* NOTE: While union flattening could be performed at any time, it is most effective when we know
   that all tvars have been resolved. *)
let union_flatten =
  let rec union_flatten cx seen ts = Core_list.(ts >>= flatten cx seen)
  and flatten cx seen t =
    match t with
    | OpenT (_, id) ->
      if ISet.mem id !seen then
        []
      else (
        seen := ISet.add id !seen;
        Constraint.(
          match Context.find_graph cx id with
          | Resolved (_, t')
          | FullyResolved (_, t') ->
            flatten cx seen t'
          | Unresolved _ -> [t])
      )
    | AnnotT (_, t, _) -> flatten cx seen t
    | ReposT (_, t) -> flatten cx seen t
    | UnionT (_, rep) -> union_flatten cx seen @@ UnionRep.members rep
    | MaybeT (r, t) ->
      DefT (r, Trust.bogus_trust (), NullT)
      :: DefT (r, Trust.bogus_trust (), VoidT)
      :: flatten cx seen t
    | OptionalT { reason = r; type_ = t; use_desc } ->
      let void_t = VoidT.why_with_use_desc ~use_desc r |> with_trust Trust.bogus_trust in
      void_t :: flatten cx seen t
    | DefT (_, _, EmptyT _) -> []
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
      | BoundT _ -> t
      | ExistsT _ -> t
      | ThisClassT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ThisClassT (r, t'')
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
      | MergedT (r, uses) ->
        let uses' = ListUtils.ident_map (self#use_type cx map_cx) uses in
        if uses == uses' then
          t
        else
          MergedT (r, uses')
      | ShapeT t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ShapeT t''
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
        let x' = self#type_ cx map_cx x in
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
      | ReposT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ReposT (r, t'')
      | InternalT (ReposUpperT (r, t')) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          InternalT (ReposUpperT (r, t''))
      | AnyT _ -> t
      | InternalT (OptionalChainVoidT _) -> t
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

    method virtual tvar : Context.t -> 'a -> Reason.t -> Constraint.ident -> Constraint.ident

    method targ cx map_cx t =
      match t with
      | ImplicitArg _ -> t
      | ExplicitArg t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ExplicitArg t''

    method def_type cx map_cx t =
      match t with
      | NumT _
      | StrT _
      | BoolT _
      | EmptyT _
      | MixedT _
      | NullT
      | VoidT ->
        t
      | FunT (s, p, f) ->
        let s' = self#type_ cx map_cx s in
        let p' = self#type_ cx map_cx p in
        let f' = self#fun_type cx map_cx f in
        if s == s' && p == p' && f == f' then
          t
        else
          FunT (s', p', f')
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
      | SingletonBoolT _ ->
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
          PolyT { tparams_loc; tparams = tparamlist'; t_out = t''; id = Context.make_nominal cx }
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
        ( { this_t; params; rest_param; return_t; closure_t; is_predicate; changeset; def_reason }
        as t ) =
      let this_t' = self#type_ cx map_cx this_t in
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
      if
        this_t' == this_t
        && return_t' == return_t
        && params' == params
        && rest_param' == rest_param
      then
        t
      else
        let this_t = this_t' in
        let return_t = return_t' in
        let params = params' in
        let rest_param = rest_param' in
        { this_t; params; rest_param; return_t; closure_t; is_predicate; changeset; def_reason }

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

    method type_param cx map_cx ({ reason; name; bound; polarity; default } as t) =
      let bound' = self#type_ cx map_cx bound in
      let default' = OptionUtils.ident_map (self#type_ cx map_cx) default in
      if bound == bound' && default == default' then
        t
      else
        let bound = bound' in
        let default = default' in
        { reason; name; bound; polarity; default }

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
      | PropertyType _ ->
        t
      | ElementType t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ElementType t''
      | Bind t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          Bind t''
      | ReadOnlyType -> t
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
      | ReactElementRefType ->
        t

    method object_kit_spread_operand_slice
        cx map_cx ({ Object.Spread.reason; prop_map; dict } as slice) =
      let prop_map' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) prop_map in
      let dict' = OptionUtils.ident_map (self#dict_type cx map_cx) dict in
      if prop_map' == prop_map && dict' == dict then
        slice
      else
        { Object.Spread.reason; prop_map = prop_map'; dict = dict' }

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
            Type t')

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
      | ReactCreateClass
      | ReactCreateElement
      | ReactCloneElement
      | Idx
      | TypeAssertIs
      | TypeAssertThrows
      | TypeAssertWraps
      | DebugPrint
      | DebugThrow
      | DebugSleep ->
        kind

    method virtual exports : Context.t -> 'a -> Type.Exports.id -> Type.Exports.id

    method obj_type cx map_cx t =
      let { flags; dict_t; props_tmap; proto_t; call_t } = t in
      let dict_t' = OptionUtils.ident_map (self#dict_type cx map_cx) dict_t in
      let props_tmap' = self#props cx map_cx props_tmap in
      let proto_t' = self#type_ cx map_cx proto_t in
      let call_t' = OptionUtils.ident_map (self#call_prop cx map_cx) call_t in
      if dict_t' == dict_t && props_tmap' == props_tmap && proto_t' == proto_t && call_t' == call_t
      then
        t
      else
        { flags; dict_t = dict_t'; props_tmap = props_tmap'; proto_t = proto_t'; call_t = call_t' }

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
      | TupleAT (t', tlist) ->
        let t'' = self#type_ cx map_cx t' in
        let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
        if t'' == t' && tlist' == tlist then
          t
        else
          TupleAT (t'', tlist')
      | ROArrayAT t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ROArrayAT t''

    method bounds cx map_cx t =
      Constraint.(
        let lower' = TypeMap.ident_map_key (self#type_ cx map_cx) t.lower in
        if lower' != t.lower then t.lower <- lower';
        let upper' = UseTypeMap.ident_map_key (self#use_type cx map_cx) t.upper in
        if upper' != t.upper then t.upper <- upper';
        t)

    method virtual use_type : Context.t -> 'a -> Type.use_t -> Type.use_t

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
      | ExistsP _
      | NullP
      | MaybeP
      | SingletonBoolP _
      | SingletonStrP _
      | SingletonNumP _
      | BoolP
      | FunP
      | NumP
      | ObjP
      | StrP
      | SymbolP
      | VoidP
      | ArrP
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

    method virtual props : Context.t -> 'a -> Properties.id -> Properties.id

    method virtual eval_id : Context.t -> 'a -> int -> int

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
  end

class virtual ['a] t_with_uses =
  object (self)
    inherit ['a] t as _super

    method use_type cx map_cx t =
      match t with
      | UseT (u, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          UseT (u, t'')
      | BindT (op, r, funcall, passthrough) ->
        let funcall' = self#fun_call_type cx map_cx funcall in
        if funcall == funcall' then
          t
        else
          BindT (op, r, funcall', passthrough)
      | CallT (op, r, funcall) ->
        let funcall' = self#fun_call_type cx map_cx funcall in
        if funcall == funcall' then
          t
        else
          CallT (op, r, funcall')
      | MethodT (op, r1, r2, prop, funcall, prop_t) ->
        let prop' = self#prop_ref cx map_cx prop in
        let funcall' = self#fun_call_type cx map_cx funcall in
        let prop_t' = OptionUtils.ident_map (self#type_ cx map_cx) prop_t in
        if prop' == prop && funcall' == funcall && prop_t' == prop_t then
          t
        else
          MethodT (op, r1, r2, prop', funcall', prop_t')
      | SetPropT (use_op, r, prop, mode, i, t', prop_t) ->
        let prop' = self#prop_ref cx map_cx prop in
        let t'' = self#type_ cx map_cx t' in
        let prop_t' = OptionUtils.ident_map (self#type_ cx map_cx) prop_t in
        if prop' == prop && t'' == t' && prop_t' == prop_t then
          t
        else
          SetPropT (use_op, r, prop', mode, i, t'', prop_t')
      | SetPrivatePropT (use_op, r, prop, mode, scopes, static, t', prop_t) ->
        let t'' = self#type_ cx map_cx t' in
        let scopes' = ListUtils.ident_map (self#class_binding cx map_cx) scopes in
        let prop_t' = OptionUtils.ident_map (self#type_ cx map_cx) prop_t in
        if t'' == t' && scopes' == scopes && prop_t' == prop_t then
          t
        else
          SetPrivatePropT (use_op, r, prop, mode, scopes', static, t'', prop_t')
      | GetPropT (use_op, r, prop, t') ->
        let prop' = self#prop_ref cx map_cx prop in
        let t'' = self#type_ cx map_cx t' in
        if prop' == prop && t'' == t' then
          t
        else
          GetPropT (use_op, r, prop', t'')
      | MatchPropT (use_op, r, prop, t') ->
        let prop' = self#prop_ref cx map_cx prop in
        let t'' = self#type_ cx map_cx t' in
        if prop' == prop && t'' == t' then
          t
        else
          MatchPropT (use_op, r, prop', t'')
      | GetPrivatePropT (use_op, r, prop, scopes, static, t') ->
        let t'' = self#type_ cx map_cx t' in
        let scopes' = ListUtils.ident_map (self#class_binding cx map_cx) scopes in
        if t'' == t' && scopes' == scopes then
          t
        else
          GetPrivatePropT (use_op, r, prop, scopes', static, t'')
      | TestPropT (r, id, prop, t') ->
        let prop' = self#prop_ref cx map_cx prop in
        let t'' = self#type_ cx map_cx t' in
        if prop' == prop && t'' == t' then
          t
        else
          TestPropT (r, id, prop', t'')
      | SetElemT (use_op, r, t1, m, t2, t3) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        let t3' = OptionUtils.ident_map (self#type_ cx map_cx) t3 in
        if t1' == t1 && t2' == t2 && t3' == t3 then
          t
        else
          SetElemT (use_op, r, t1', m, t2', t3')
      | GetElemT (use_op, r, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          GetElemT (use_op, r, t1', t2')
      | CallElemT (r1, r2, t', funcall) ->
        let t'' = self#type_ cx map_cx t' in
        let funcall' = self#fun_call_type cx map_cx funcall in
        if t' == t'' && funcall' == funcall then
          t
        else
          CallElemT (r1, r2, t'', funcall')
      | GetStaticsT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          GetStaticsT (r, t'')
      | GetProtoT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          GetProtoT (r, t'')
      | SetProtoT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          SetProtoT (r, t'')
      | ReposLowerT (r, use_desc, use) ->
        let use' = self#use_type cx map_cx use in
        if use' == use then
          t
        else
          ReposLowerT (r, use_desc, use')
      | ReposUseT (r, use_desc, use_op, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ReposUseT (r, use_desc, use_op, t'')
      | ConstructorT (op, r, targs, args, t') ->
        let targs' = OptionUtils.ident_map (ListUtils.ident_map (self#targ cx map_cx)) targs in
        let args' = ListUtils.ident_map (self#call_arg cx map_cx) args in
        let t'' = self#type_ cx map_cx t' in
        if targs' == targs && args' == args && t'' == t' then
          t
        else
          ConstructorT (op, r, targs', args', t'')
      | SuperT (op, r, Derived { own = o; proto = p; static = s }) ->
        let o' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) o in
        let p' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) p in
        let s' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) s in
        if o' == o && p' == p && s' == s then
          t
        else
          SuperT (op, r, Derived { own = o'; proto = p'; static = s' })
      | ImplementsT (use_op, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ImplementsT (use_op, t'')
      | MixinT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          MixinT (r, t'')
      | ToStringT (r, t') ->
        let t'' = self#use_type cx map_cx t' in
        if t'' == t' then
          t
        else
          ToStringT (r, t'')
      | AdderT (op, r, flip, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          AdderT (op, r, flip, t1', t2')
      | ComparatorT (r, flip, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ComparatorT (r, flip, t'')
      | UnaryMinusT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          UnaryMinusT (r, t'')
      | AssertArithmeticOperandT _
      | AssertBinaryInLHST _
      | AssertBinaryInRHST _
      | AssertForInRHST _ ->
        t
      | PredicateT (p, t') ->
        let p' = self#predicate cx map_cx p in
        let t'' = self#type_ cx map_cx t' in
        if p' == p && t'' == t' then
          t
        else
          PredicateT (p', t'')
      | GuardT (p, t1, t2) ->
        let p' = self#predicate cx map_cx p in
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if p' == p && t1' == t1 && t2' == t2 then
          t
        else
          GuardT (p', t1', t2')
      | EqT (r, flip, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          EqT (r, flip, t'')
      | AndT (r, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          AndT (r, t1', t2')
      | OrT (r, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          OrT (r, t1', t2')
      | NullishCoalesceT (r, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          NullishCoalesceT (r, t1', t2')
      | NotT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          NotT (r, t'')
      | SpecializeT (u, r1, r2, cache, tlist_opt, t') ->
        let tlist_opt' =
          OptionUtils.ident_map (ListUtils.ident_map (self#type_ cx map_cx)) tlist_opt
        in
        let t'' = self#type_ cx map_cx t' in
        if tlist_opt' == tlist_opt && t'' == t' then
          t
        else
          SpecializeT (u, r1, r2, cache, tlist_opt', t'')
      | ThisSpecializeT (r, this, k) ->
        let this' = self#type_ cx map_cx this in
        let k' = self#cont cx map_cx k in
        if this' == this && k' == k then
          t
        else
          ThisSpecializeT (r, this', k')
      | VarianceCheckT (r, tlist, p) ->
        let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
        if tlist' == tlist then
          t
        else
          VarianceCheckT (r, tlist', p)
      | TypeAppVarianceCheckT (use_op, r1, r2, tpairlist) ->
        let tpairlist' =
          ListUtils.ident_map
            (fun ((x, y) as z) ->
              let x' = self#type_ cx map_cx x in
              let y' = self#type_ cx map_cx y in
              if x' == x && y' == y then
                z
              else
                (x', y'))
            tpairlist
        in
        if tpairlist' == tpairlist then
          t
        else
          TypeAppVarianceCheckT (use_op, r1, r2, tpairlist')
      | ConcretizeTypeAppsT (use_op, (ts1, op1, r1), (t2, ts2, op2, r2), flip) ->
        let ts1' = ListUtils.ident_map (self#type_ cx map_cx) ts1 in
        let t2' = self#type_ cx map_cx t2 in
        let ts2' = ListUtils.ident_map (self#type_ cx map_cx) ts2 in
        if ts1' == ts1 && t2' == t2 && ts2' == ts2 then
          t
        else
          ConcretizeTypeAppsT (use_op, (ts1', op1, r1), (t2', ts2', op2, r2), flip)
      | LookupT (r, lookup, tlist, prop, action) ->
        let lookup' = self#lookup_kind cx map_cx lookup in
        let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
        let prop' = self#prop_ref cx map_cx prop in
        let action' = self#lookup_action cx map_cx action in
        if lookup' == lookup && tlist' == tlist && prop' == prop && action' == action then
          t
        else
          LookupT (r, lookup', tlist', prop', action')
      | ObjAssignToT (op, r, t1, t2, obj_assign) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          ObjAssignToT (op, r, t1', t2', obj_assign)
      | ObjAssignFromT (op, r, t1, t2, obj_assign) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          ObjAssignFromT (op, r, t1', t2', obj_assign)
      | ObjFreezeT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ObjFreezeT (r, t'')
      | ObjRestT (r, strings, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ObjRestT (r, strings, t'')
      | ObjSealT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ObjSealT (r, t'')
      | ObjTestT (r, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          ObjTestT (r, t1', t2')
      | ObjTestProtoT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ObjTestProtoT (r, t'')
      | ArrRestT (op, r, i, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ArrRestT (op, r, i, t'')
      | UnifyT (t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          UnifyT (t1', t2')
      | BecomeT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          BecomeT (r, t'')
      | GetKeysT (r, t') ->
        let t'' = self#use_type cx map_cx t' in
        if t'' == t' then
          t
        else
          GetKeysT (r, t'')
      | HasOwnPropT _ -> t
      | GetValuesT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          GetValuesT (r, t'')
      | ReactPropsToOut (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ReactPropsToOut (r, t'')
      | ReactInToProps (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ReactInToProps (r, t'')
      | ElemT (use_op, r, t', action) ->
        let t'' = self#type_ cx map_cx t' in
        let action' = self#elem_action cx map_cx action in
        if t'' == t' && action' == action then
          t
        else
          ElemT (use_op, r, t'', action')
      | MakeExactT (r, cont) ->
        let cont' = self#cont cx map_cx cont in
        if cont' == cont then
          t
        else
          MakeExactT (r, cont')
      | CJSRequireT (r, t', is_strict) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          CJSRequireT (r, t'', is_strict)
      | ImportModuleNsT (r, t', is_strict) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ImportModuleNsT (r, t'', is_strict)
      | ImportDefaultT (r, import, s, t', is_strict) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ImportDefaultT (r, import, s, t'', is_strict)
      | ImportNamedT (r, import, s, m, t', is_strict) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ImportNamedT (r, import, s, m, t'', is_strict)
      | ImportTypeT (r, s, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ImportTypeT (r, s, t'')
      | ImportTypeofT (r, s, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ImportTypeofT (r, s, t'')
      | AssertImportIsValueT _ -> t
      | CJSExtractNamedExportsT (r1, (r2, exports, is_strict), t') ->
        let exports' = self#export_types cx map_cx exports in
        let t'' = self#type_ cx map_cx t' in
        if exports' == exports && t'' == t' then
          t
        else
          CJSExtractNamedExportsT (r1, (r2, exports', is_strict), t'')
      | CopyNamedExportsT (r, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          CopyNamedExportsT (r, t1', t2')
      | CopyTypeExportsT (r, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          CopyTypeExportsT (r, t1', t2')
      | ExportNamedT (r, skip, tmap, export_kind, t') ->
        let map_loc_type_pair ((loc, t) as orig) =
          let t' = self#type_ cx map_cx t in
          if t == t' then
            orig
          else
            (loc, t')
        in
        let tmap' = SMap.ident_map map_loc_type_pair tmap in
        let t'' = self#type_ cx map_cx t' in
        if tmap' == tmap && t'' == t' then
          t
        else
          ExportNamedT (r, skip, tmap', export_kind, t'')
      | ExportTypeT (r, skip, name, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          ExportTypeT (r, skip, name, t1', t2')
      | AssertExportIsTypeT (r, name, t1) ->
        let t1' = self#type_ cx map_cx t1 in
        if t1' == t1 then
          t
        else
          AssertExportIsTypeT (r, name, t1')
      | MapTypeT (use_op, r, tmap, t') ->
        let tmap' = self#type_map cx map_cx tmap in
        let t'' = self#type_ cx map_cx t' in
        if tmap' == tmap && t'' == t' then
          t
        else
          MapTypeT (use_op, r, tmap', t'')
      | ReactKitT (use_op, r, react_tool) ->
        let react_tool' = self#react_tool cx map_cx react_tool in
        if react_tool' == react_tool then
          t
        else
          ReactKitT (use_op, r, react_tool')
      | ObjKitT (use_op, r, resolve_tool, tool, tout) ->
        let resolve_tool' = self#object_kit_resolve_tool cx map_cx resolve_tool in
        let tool' = self#object_kit_tool cx map_cx tool in
        let tout' = self#type_ cx map_cx tout in
        if resolve_tool' == resolve_tool && tool' == tool && tout' == tout then
          t
        else
          ObjKitT (use_op, r, resolve_tool', tool', tout')
      | ChoiceKitUseT (r, choice_use_tool) ->
        let choice_use_tool' = self#choice_use_tool cx map_cx choice_use_tool in
        if choice_use_tool' == choice_use_tool then
          t
        else
          ChoiceKitUseT (r, choice_use_tool')
      | IntersectionPreprocessKitT (r, ipt) ->
        let ipt' = self#intersection_preprocess_tool cx map_cx ipt in
        if ipt' == ipt then
          t
        else
          IntersectionPreprocessKitT (r, ipt')
      | DebugPrintT _ -> t
      | DebugSleepT _ -> t
      | SentinelPropTestT (r, t1, key, b, sentinel, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          SentinelPropTestT (r, t1', key, b, sentinel, t2')
      | IdxUnwrap (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          IdxUnwrap (r, t'')
      | IdxUnMaybeifyT (r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          IdxUnMaybeifyT (r, t'')
      | OptionalChainT (r, lhs_r, uses) ->
        let uses' =
          Nel.map
            (fun (use, tout) -> (self#opt_use_type cx map_cx use, self#type_ cx map_cx tout))
            uses
        in
        if uses' == uses then
          t
        else
          OptionalChainT (r, lhs_r, uses')
      | InvariantT _ -> t
      | CallLatentPredT (r, b, i, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          CallLatentPredT (r, b, i, t1', t2')
      | CallOpenPredT (r, b, key, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          CallOpenPredT (r, b, key, t1', t2')
      | SubstOnPredT (r, sub, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          SubstOnPredT (r, sub, t'')
      | RefineT (r, p, t') ->
        let p' = self#predicate cx map_cx p in
        let t'' = self#type_ cx map_cx t' in
        if p' == p && t'' == t' then
          t
        else
          RefineT (r, p', t'')
      | ResolveSpreadT (op, r, resolve_spread) ->
        let resolve_spread' = self#resolve_spread cx map_cx resolve_spread in
        if resolve_spread' == resolve_spread then
          t
        else
          ResolveSpreadT (op, r, resolve_spread')
      | CondT (r, then_t, else_t, tout) ->
        let then_t' = OptionUtils.ident_map (self#type_ cx map_cx) then_t in
        let else_t' = self#type_ cx map_cx else_t in
        let tout' = self#type_ cx map_cx tout in
        if then_t' == then_t && else_t' == else_t && tout' == tout then
          t
        else
          CondT (r, then_t', else_t', tout')
      | ExtendsUseT (use_op, r, tlist, t1, t2) ->
        let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if tlist' == tlist && t1' == t1 && t2' == t2 then
          t
        else
          ExtendsUseT (use_op, r, tlist', t1', t2')
      | ModuleExportsAssignT (r, t', t_out) ->
        let t'' = self#type_ cx map_cx t' in
        let t_out' = self#type_ cx map_cx t_out in
        if t' == t'' && t_out == t_out' then
          t
        else
          ModuleExportsAssignT (r, t'', t_out')
      | DestructuringT (r, k, s, t') ->
        let s' = self#selector cx map_cx s in
        let t'' = self#type_ cx map_cx t' in
        if s' == s && t'' == t' then
          t
        else
          DestructuringT (r, k, s', t'')
      | CreateObjWithComputedPropT { reason; value; tout_tvar = (r, id) } ->
        let value' = self#type_ cx map_cx value in
        let id' = self#tvar cx map_cx r id in
        if value' == value && id' == id then
          t
        else
          CreateObjWithComputedPropT { reason; value = value'; tout_tvar = (r, id') }

    method private opt_use_type cx map_cx t =
      match t with
      | OptCallT (op, r, funcall) ->
        let funcall' = self#opt_fun_call_type cx map_cx funcall in
        if funcall == funcall' then
          t
        else
          OptCallT (op, r, funcall')
      | OptGetPropT (use_op, r, prop) ->
        let prop' = self#prop_ref cx map_cx prop in
        if prop' == prop then
          t
        else
          OptGetPropT (use_op, r, prop')
      | OptGetPrivatePropT (use_op, r, prop, scopes, static) ->
        let scopes' = ListUtils.ident_map (self#class_binding cx map_cx) scopes in
        if scopes' == scopes then
          t
        else
          OptGetPrivatePropT (use_op, r, prop, scopes', static)
      | OptTestPropT (r, id, prop) ->
        let prop' = self#prop_ref cx map_cx prop in
        if prop' == prop then
          t
        else
          OptTestPropT (r, id, prop')
      | OptGetElemT (use_op, r, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          OptGetElemT (use_op, r, t'')

    method private opt_fun_call_type cx map_cx ((this, targs, args, clos, strict) as t) =
      let this' = self#type_ cx map_cx this in
      let targs' = OptionUtils.ident_map (ListUtils.ident_map (self#targ cx map_cx)) targs in
      let args' = ListUtils.ident_map (self#call_arg cx map_cx) args in
      if this' == this && targs' == targs && args' == args then
        t
      else
        (this', targs', args', clos, strict)

    method prop_ref cx map_cx t =
      match t with
      | Named _ -> t
      | Computed t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          Computed t''

    method class_binding cx map_cx binding =
      let class_private_fields = self#props cx map_cx binding.class_private_fields in
      let class_private_static_fields = self#props cx map_cx binding.class_private_static_fields in
      if
        class_private_fields == binding.class_private_fields
        && class_private_static_fields == binding.class_private_static_fields
      then
        binding
      else
        { binding with class_private_fields; class_private_static_fields }

    method elem_action cx map_cx t =
      match t with
      | ReadElem t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ReadElem t''
      | WriteElem (tin, tout, mode) ->
        let tin' = self#type_ cx map_cx tin in
        let tout' = OptionUtils.ident_map (self#type_ cx map_cx) tout in
        if tin' == tin && tout' == tout then
          t
        else
          WriteElem (tin', tout', mode)
      | CallElem (r, funcall) ->
        let funcall' = self#fun_call_type cx map_cx funcall in
        if funcall' == funcall then
          t
        else
          CallElem (r, funcall')

    method resolve_spread cx map_cx ({ rrt_resolved; rrt_unresolved; rrt_resolve_to } as t) =
      let rrt_resolved' = ListUtils.ident_map (self#resolved_param cx map_cx) rrt_resolved in
      let rrt_unresolved' = ListUtils.ident_map (self#unresolved_param cx map_cx) rrt_unresolved in
      let rrt_resolve_to' = self#spread_resolve cx map_cx rrt_resolve_to in
      if
        rrt_resolved' == rrt_resolved
        && rrt_unresolved' == rrt_unresolved
        && rrt_resolve_to' == rrt_resolve_to
      then
        t
      else
        {
          rrt_resolved = rrt_resolved';
          rrt_unresolved = rrt_unresolved';
          rrt_resolve_to = rrt_resolve_to';
        }

    method spread_resolve cx map_cx t =
      match t with
      | ResolveSpreadsToTuple (i, t1', t2') ->
        let t1'' = self#type_ cx map_cx t1' in
        let t2'' = self#type_ cx map_cx t2' in
        if t1'' == t1' && t2'' == t2' then
          t
        else
          ResolveSpreadsToTuple (i, t1'', t2'')
      | ResolveSpreadsToArrayLiteral (i, t1', t2') ->
        let t1'' = self#type_ cx map_cx t1' in
        let t2'' = self#type_ cx map_cx t2' in
        if t1'' == t1' && t2'' == t2' then
          t
        else
          ResolveSpreadsToArrayLiteral (i, t1'', t2'')
      | ResolveSpreadsToArray (t1', t2') ->
        let t1'' = self#type_ cx map_cx t1' in
        let t2'' = self#type_ cx map_cx t2' in
        if t1'' == t1' && t2'' == t2' then
          t
        else
          ResolveSpreadsToArray (t1'', t2'')
      | ResolveSpreadsToMultiflowCallFull (i, funtype) ->
        let funtype' = self#fun_type cx map_cx funtype in
        if funtype' == funtype then
          t
        else
          ResolveSpreadsToMultiflowCallFull (i, funtype')
      | ResolveSpreadsToMultiflowSubtypeFull (i, funtype) ->
        let funtype' = self#fun_type cx map_cx funtype in
        if funtype' == funtype then
          t
        else
          ResolveSpreadsToMultiflowSubtypeFull (i, funtype')
      | ResolveSpreadsToCustomFunCall (i, kind, tout) ->
        let tout' = self#type_ cx map_cx tout in
        if tout' == tout then
          t
        else
          ResolveSpreadsToCustomFunCall (i, kind, tout')
      | ResolveSpreadsToMultiflowPartial (i, funtype, r, t') ->
        let funtype' = self#fun_type cx map_cx funtype in
        let t'' = self#type_ cx map_cx t' in
        if funtype' == funtype && t'' == t' then
          t
        else
          ResolveSpreadsToMultiflowPartial (i, funtype', r, t'')
      | ResolveSpreadsToCallT (funcalltype, t') ->
        let funcalltype' = self#fun_call_type cx map_cx funcalltype in
        let t'' = self#type_ cx map_cx t' in
        if funcalltype' == funcalltype && t'' == t' then
          t
        else
          ResolveSpreadsToCallT (funcalltype', t'')

    method fun_call_type cx map_cx t =
      let {
        call_this_t;
        call_targs;
        call_args_tlist;
        call_tout;
        call_closure_t;
        call_strict_arity;
      } =
        t
      in
      let call_this_t' = self#type_ cx map_cx call_this_t in
      let call_targs' =
        OptionUtils.ident_map (ListUtils.ident_map (self#targ cx map_cx)) call_targs
      in
      let call_args_tlist' = ListUtils.ident_map (self#call_arg cx map_cx) call_args_tlist in
      let call_tout' = self#type_ cx map_cx call_tout in
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
          call_closure_t;
          call_strict_arity;
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

    method lookup_kind cx map_cx t =
      match t with
      | Strict _ -> t
      | NonstrictReturning (tpairopt, testopt) ->
        begin
          match tpairopt with
          | Some (t1, t2) ->
            let t1' = self#type_ cx map_cx t1 in
            let t2' = self#type_ cx map_cx t2 in
            if t1' == t1 && t2' == t2 then
              t
            else
              NonstrictReturning (Some (t1', t2'), testopt)
          | None -> t
        end
      | ShadowRead (r, pidlist) ->
        let pidlist' =
          Nel.ident_map (fun property_id -> self#props cx map_cx property_id) pidlist
        in
        if pidlist == pidlist' then
          t
        else
          ShadowRead (r, pidlist')
      | ShadowWrite pidlist ->
        let pidlist' =
          Nel.ident_map (fun property_id -> self#props cx map_cx property_id) pidlist
        in
        if pidlist == pidlist' then
          t
        else
          ShadowWrite pidlist'

    method lookup_action cx map_cx t =
      match t with
      | ReadProp { use_op; obj_t; tout } ->
        let obj_t' = self#type_ cx map_cx obj_t in
        let tout' = self#type_ cx map_cx tout in
        if obj_t' == obj_t && tout' == tout then
          t
        else
          ReadProp { use_op; obj_t = obj_t'; tout = tout' }
      | WriteProp { use_op; obj_t; prop_tout; tin; write_ctx; mode } ->
        let obj_t' = self#type_ cx map_cx obj_t in
        let tin' = self#type_ cx map_cx tin in
        let prop_tout' = OptionUtils.ident_map (self#type_ cx map_cx) prop_tout in
        if obj_t' == obj_t && tin' == tin && prop_tout' == prop_tout then
          t
        else
          WriteProp { use_op; obj_t = obj_t'; prop_tout = prop_tout'; tin = tin'; write_ctx; mode }
      | LookupProp (use, prop) ->
        let prop' = Property.ident_map_t (self#type_ cx map_cx) prop in
        if prop == prop' then
          t
        else
          LookupProp (use, prop')
      | SuperProp (op, prop) ->
        let prop' = Property.ident_map_t (self#type_ cx map_cx) prop in
        if prop == prop' then
          t
        else
          SuperProp (op, prop')
      | MatchProp (use, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          MatchProp (use, t')

    method cont cx map_cx t =
      match t with
      | Lower (use_op, t') ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          Lower (use_op, t'')
      | Upper use_t ->
        let use_t' = self#use_type cx map_cx use_t in
        if use_t' == use_t then
          t
        else
          Upper use_t'

    method react_tool cx map_cx t =
      React.(
        match t with
        | CreateElement0 (clone, config, (children, children_spread), tout) ->
          let config' = self#type_ cx map_cx config in
          let children' = ListUtils.ident_map (self#type_ cx map_cx) children in
          let children_spread' = OptionUtils.ident_map (self#type_ cx map_cx) children_spread in
          let tout' = self#type_ cx map_cx tout in
          if
            config' == config
            && children' == children
            && children_spread' == children_spread
            && tout' == tout
          then
            t
          else
            CreateElement0 (clone, config', (children', children_spread'), tout')
        | CreateElement (clone, component, config, (children, children_spread), tout) ->
          let component' = self#type_ cx map_cx component in
          let config' = self#type_ cx map_cx config in
          let children' = ListUtils.ident_map (self#type_ cx map_cx) children in
          let children_spread' = OptionUtils.ident_map (self#type_ cx map_cx) children_spread in
          let tout' = self#type_ cx map_cx tout in
          if
            component' == component
            && config' == config
            && children' == children
            && children_spread' == children_spread
            && tout' == tout
          then
            t
          else
            CreateElement (clone, component', config', (children', children_spread'), tout')
        | ConfigCheck config ->
          let config' = self#type_ cx map_cx config in
          if config' == config then
            t
          else
            ConfigCheck config'
        | GetProps tout ->
          let tout' = self#type_ cx map_cx tout in
          if tout' == tout then
            t
          else
            GetProps tout'
        | GetConfig tout ->
          let tout' = self#type_ cx map_cx tout in
          if tout' == tout then
            t
          else
            GetConfig tout'
        | GetConfigType (default_props, tout) ->
          let default_props' = self#type_ cx map_cx default_props in
          let tout' = self#type_ cx map_cx tout in
          if tout' == tout && default_props' == default_props then
            t
          else
            GetConfigType (default_props', tout')
        | GetRef tout ->
          let tout' = self#type_ cx map_cx tout in
          if tout' == tout then
            t
          else
            GetRef tout'
        | SimplifyPropType (tool, t') ->
          let tool' = self#simplify_prop_type_tool cx map_cx tool in
          let t'' = self#type_ cx map_cx t' in
          if tool' == tool && t'' == t' then
            t
          else
            SimplifyPropType (tool', t'')
        | CreateClass (tool, knot, t') ->
          let tool' = self#create_class_tool cx map_cx tool in
          let knot' = self#create_class_knot cx map_cx knot in
          let t'' = self#type_ cx map_cx t' in
          if tool' == tool && knot' == knot && t'' == t' then
            t
          else
            CreateClass (tool', knot', t''))

    method object_kit_resolve_tool cx map_cx t =
      Object.(
        match t with
        | Resolve r ->
          let r' = self#resolve cx map_cx r in
          if r' == r then
            t
          else
            Resolve r'
        | Super ({ Object.reason; props; dict; flags }, r) ->
          let props' = SMap.ident_map (fun (t, b) -> (self#type_ cx map_cx t, b)) props in
          let dict' = OptionUtils.ident_map (self#dict_type cx map_cx) dict in
          let r' = self#resolve cx map_cx r in
          if r' == r && props' == props then
            t
          else
            Super ({ reason; Object.props = props'; dict = dict'; flags }, r'))

    method object_kit_tool cx map_cx tool =
      Object.(
        match tool with
        | ReadOnly -> tool
        | ObjectRep -> tool
        | ObjectWiden _ -> tool
        | Spread
            (options, { Object.Spread.todo_rev; acc; spread_id; union_reason; curr_resolve_idx })
          ->
          let todo_rev' =
            ListUtils.ident_map (self#object_kit_spread_operand cx map_cx) todo_rev
          in
          let acc' = ListUtils.ident_map (self#object_kit_acc_element cx map_cx) acc in
          if todo_rev' == todo_rev && acc' == acc then
            tool
          else
            Spread
              ( options,
                {
                  Object.Spread.todo_rev = todo_rev';
                  acc = acc';
                  spread_id;
                  union_reason;
                  curr_resolve_idx;
                } )
        | Rest (options, state) ->
          Object.Rest.(
            let state' =
              match state with
              | One t ->
                let t' = self#type_ cx map_cx t in
                if t == t' then
                  state
                else
                  One t'
              | Done o ->
                let o' = self#resolved cx map_cx o in
                if o == o' then
                  state
                else
                  Done o'
            in
            if state == state' then
              tool
            else
              Rest (options, state'))
        | ReactConfig state ->
          Object.ReactConfig.(
            let state' =
              match state with
              | Config { defaults; children } ->
                let defaults' = OptionUtils.ident_map (self#type_ cx map_cx) defaults in
                let children' = OptionUtils.ident_map (self#type_ cx map_cx) children in
                if defaults == defaults' && children == children' then
                  state
                else
                  Config { defaults = defaults'; children = children' }
              | Defaults { config; children } ->
                let config' = self#resolved cx map_cx config in
                let children' = OptionUtils.ident_map (self#type_ cx map_cx) children in
                if config == config' && children == children' then
                  state
                else
                  Defaults { config = config'; children = children' }
            in
            if state == state' then
              tool
            else
              ReactConfig state'))

    method choice_use_tool cx map_cx t =
      match t with
      | FullyResolveType _ -> t
      | TryFlow (i, spec) ->
        let spec' = self#spec cx map_cx spec in
        if spec' == spec then
          t
        else
          TryFlow (i, spec')

    method intersection_preprocess_tool cx map_cx t =
      match t with
      | ConcretizeTypes (tlist1, tlist2, t', use_t) ->
        let tlist1' = ListUtils.ident_map (self#type_ cx map_cx) tlist1 in
        let tlist2' = ListUtils.ident_map (self#type_ cx map_cx) tlist2 in
        let t'' = self#type_ cx map_cx t' in
        let use_t' = self#use_type cx map_cx use_t in
        if tlist1' == tlist1 && tlist2' == tlist2 && t'' == t' && use_t' == use_t then
          t
        else
          ConcretizeTypes (tlist1', tlist2', t'', use_t')
      | SentinelPropTest (b, s, t1, t2, t3) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        let t3' = self#type_ cx map_cx t3 in
        if t1' == t1 && t2' == t2 && t3' == t3 then
          t
        else
          SentinelPropTest (b, s, t1', t2', t3')
      | PropExistsTest (b, s, t1, t2) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t2 && t2' == t2 then
          t
        else
          PropExistsTest (b, s, t1', t2')

    method simplify_prop_type_tool cx map_cx tool =
      React.SimplifyPropType.(
        match tool with
        | ArrayOf
        | InstanceOf
        | ObjectOf ->
          tool
        | OneOf resolve_array ->
          let resolve_array' = self#resolve_array cx map_cx resolve_array in
          if resolve_array' == resolve_array then
            tool
          else
            OneOf resolve_array'
        | OneOfType resolve_array ->
          let resolve_array' = self#resolve_array cx map_cx resolve_array in
          if resolve_array' == resolve_array then
            tool
          else
            OneOfType resolve_array'
        | Shape resolve_object ->
          let resolve_object' = self#resolve_object cx map_cx resolve_object in
          if resolve_object' == resolve_object then
            tool
          else
            Shape resolve_object')

    method create_class_tool cx map_cx tool =
      React.CreateClass.(
        match tool with
        | Spec tail ->
          let tail' = self#stack_tail cx map_cx tail in
          if tail' == tail then
            tool
          else
            Spec tail'
        | Mixins (head, tail) ->
          let head' = self#stack_head cx map_cx head in
          let tail' = self#stack_tail cx map_cx tail in
          if head' == head && tail' == tail then
            tool
          else
            Mixins (head', tail')
        | Statics (head, tail) ->
          let head' = self#stack_head cx map_cx head in
          let tail' = self#stack_tail cx map_cx tail in
          if head' == head && tail' == tail then
            tool
          else
            Statics (head', tail')
        | PropTypes ((head, tail), resolve_object) ->
          let head' = self#stack_head cx map_cx head in
          let tail' = self#stack_tail cx map_cx tail in
          let resolve_object' = self#resolve_object cx map_cx resolve_object in
          if head' == head && tail' == tail && resolve_object' == resolve_object then
            tool
          else
            PropTypes ((head', tail'), resolve_object')
        | DefaultProps (tlist, default_props) ->
          let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
          let default_props' =
            OptionUtils.ident_map (self#default_props cx map_cx) default_props
          in
          if tlist' == tlist && default_props' == default_props then
            tool
          else
            DefaultProps (tlist', default_props')
        | InitialState (tlist, initial_state) ->
          let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
          let initial_state' =
            OptionUtils.ident_map (self#initial_state cx map_cx) initial_state
          in
          if tlist' == tlist && initial_state' == initial_state then
            tool
          else
            InitialState (tlist', initial_state'))

    method resolved_param cx map_cx t =
      match t with
      | ResolvedArg t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ResolvedArg t''
      | ResolvedSpreadArg (r, arrtype) ->
        let arrtype' = self#arr_type cx map_cx arrtype in
        if arrtype' == arrtype then
          t
        else
          ResolvedSpreadArg (r, arrtype')
      | ResolvedAnySpreadArg _ -> t

    method unresolved_param cx map_cx t =
      match t with
      | UnresolvedArg t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          UnresolvedArg t''
      | UnresolvedSpreadArg t' ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          UnresolvedSpreadArg t''

    method resolve_array cx map_cx t =
      React.(
        match t with
        | ResolveArray -> t
        | ResolveElem (tlist1, tlist2) ->
          let tlist1' = ListUtils.ident_map (self#type_ cx map_cx) tlist1 in
          let tlist2' = ListUtils.ident_map (self#type_ cx map_cx) tlist2 in
          if tlist1' == tlist1 && tlist2' == tlist2 then
            t
          else
            ResolveElem (tlist1', tlist2'))

    method resolve_object cx map_cx t =
      React.(
        match t with
        | ResolveObject -> t
        | ResolveDict (dict, props, obj) ->
          let dict' = self#dict_type cx map_cx dict in
          let props' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) props in
          let obj' = self#resolved_object cx map_cx obj in
          if dict' == dict && props' == props && obj' == obj then
            t
          else
            ResolveDict (dict', props', obj')
        | ResolveProp (s, props, obj) ->
          let props' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) props in
          let obj' = self#resolved_object cx map_cx obj in
          if props' == props && obj' == obj then
            t
          else
            ResolveProp (s, props', obj'))

    method create_class_knot cx map_cx t =
      React.CreateClass.(
        let this' = self#type_ cx map_cx t.this in
        let static' = self#type_ cx map_cx t.static in
        let state_t' = self#type_ cx map_cx t.state_t in
        let default_t' = self#type_ cx map_cx t.default_t in
        if
          this' == t.this
          && static' == t.static
          && state_t' == t.state_t
          && default_t' == t.default_t
        then
          t
        else
          { this = this'; static = static'; state_t = state_t'; default_t = default_t' })

    method resolve cx map_cx t =
      Object.(
        match t with
        | Next -> t
        | List0 (tnelist, join) ->
          let tnelist' = Nel.ident_map (self#type_ cx map_cx) tnelist in
          if tnelist' == tnelist then
            t
          else
            List0 (tnelist', join)
        | List (tlist, resolvednelist, join) ->
          let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
          let resolvednelist' = Nel.ident_map (self#resolved cx map_cx) resolvednelist in
          if tlist' == tlist && resolvednelist' == resolvednelist then
            t
          else
            List (tlist', resolvednelist', join))

    method resolved_prop cx map_cx ((t, own) as prop) =
      let t' = self#type_ cx map_cx t in
      if t' == t then
        prop
      else
        (t', own)

    method object_kit_slice cx map_cx ({ Object.reason = _; props; dict; flags = _ } as slice) =
      let props' = SMap.ident_map (self#resolved_prop cx map_cx) props in
      let dict' = OptionUtils.ident_map (self#dict_type cx map_cx) dict in
      if props' == props && dict' == dict then
        slice
      else
        { slice with Object.props = props'; dict = dict' }

    method object_kit_acc_element cx map_cx el =
      Object.Spread.(
        match el with
        | InlineSlice slice ->
          let slice' = self#object_kit_spread_operand_slice cx map_cx slice in
          if slice' == slice then
            el
          else
            InlineSlice slice'
        | ResolvedSlice resolved ->
          let resolved' = self#resolved cx map_cx resolved in
          if resolved' == resolved then
            el
          else
            ResolvedSlice resolved')

    method resolved cx map_cx t =
      let t' = Nel.ident_map (self#object_kit_slice cx map_cx) t in
      if t' == t then
        t
      else
        t'

    method spec cx map_cx t =
      match t with
      | UnionCases (use_op, t', rep, tlist) ->
        let t'' = self#type_ cx map_cx t' in
        let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
        if t'' == t' && tlist' == tlist then
          t
        else
          UnionCases (use_op, t'', rep, tlist')
      | IntersectionCases (tlist, use_t) ->
        let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
        let use_t' = self#use_type cx map_cx use_t in
        if tlist' == tlist && use_t' == use_t then
          t
        else
          IntersectionCases (tlist', use_t')

    method stack_tail cx map_cx tail = ListUtils.ident_map (self#stack_tail_elem cx map_cx) tail

    method stack_tail_elem cx map_cx ((head, tlist, maybespeclist) as t) =
      let head' = self#stack_head cx map_cx head in
      let tlist' = ListUtils.ident_map (self#type_ cx map_cx) tlist in
      let maybespeclist' =
        ListUtils.ident_map (maybe_known (self#create_class_spec cx map_cx)) maybespeclist
      in
      if head' == head && tlist' == tlist && maybespeclist' == maybespeclist then
        t
      else
        (head', tlist', maybespeclist')

    method create_class_spec cx map_cx t =
      React.CreateClass.(
        let obj = self#resolved_object cx map_cx t.obj in
        let statics =
          OptionUtils.ident_map (maybe_known (self#resolved_object cx map_cx)) t.statics
        in
        let prop_types =
          OptionUtils.ident_map (maybe_known (self#resolved_object cx map_cx)) t.prop_types
        in
        let get_default_props = ListUtils.ident_map (self#type_ cx map_cx) t.get_default_props in
        let get_initial_state = ListUtils.ident_map (self#type_ cx map_cx) t.get_initial_state in
        if
          obj == t.obj
          && statics == t.statics
          && prop_types == t.prop_types
          && get_default_props == t.get_default_props
          && get_initial_state == t.get_initial_state
        then
          t
        else
          {
            obj;
            statics;
            prop_types;
            get_default_props;
            get_initial_state;
            unknown_mixins = t.unknown_mixins;
          })

    method stack_head cx map_cx ((obj, spec) as t) =
      let obj' = self#resolved_object cx map_cx obj in
      let spec' = self#create_class_spec cx map_cx spec in
      if obj' == obj && spec' == spec then
        t
      else
        (obj', spec')

    method default_props cx map_cx default_props =
      maybe_known (self#resolved_object cx map_cx) default_props

    method resolved_object cx map_cx ((r, props, dictopt, flags) as t) =
      let props' = SMap.ident_map (Property.ident_map_t (self#type_ cx map_cx)) props in
      let dictopt' = OptionUtils.ident_map (self#dict_type cx map_cx) dictopt in
      if props' == props && dictopt' == dictopt then
        t
      else
        (r, props', dictopt', flags)

    method initial_state cx map_cx t =
      React.CreateClass.(
        maybe_known
          (fun x ->
            match x with
            | NotNull obj ->
              let obj' = self#resolved_object cx map_cx obj in
              if obj' == obj then
                x
              else
                NotNull obj'
            | Null _ -> x)
          t)
  end
