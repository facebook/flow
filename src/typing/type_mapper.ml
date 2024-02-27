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
          | Resolved t'
          | FullyResolved (lazy t') ->
            flatten cx seen t'
          | Unresolved _ -> [t]
        )
      )
    | AnnotT (_, t, _) -> flatten cx seen t
    | UnionT (_, rep) -> union_flatten cx seen @@ UnionRep.members rep
    | MaybeT (r, t) -> DefT (r, NullT) :: DefT (r, VoidT) :: flatten cx seen t
    | OptionalT { reason = r; type_ = t; use_desc } ->
      let void_t = VoidT.why_with_use_desc ~use_desc r in
      void_t :: flatten cx seen t
    | DefT (_, EmptyT) -> []
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
      | DefT (r, t') ->
        let t'' = self#def_type cx map_cx t' in
        if t' == t'' then
          t
        else
          DefT (r, t'')
      | EvalT (t', dt, id) ->
        let t'' = self#type_ cx map_cx t' in
        let dt' = self#defer_use_type cx map_cx dt in
        let id' = self#eval_id cx map_cx id in
        if t' == t'' && dt == dt' && id' == id then
          t
        else
          EvalT (t'', dt', id')
      | ThisInstanceT (r, t', i, n) ->
        let t'' = self#instance_type cx map_cx t' in
        if t'' == t' then
          t
        else
          ThisInstanceT (r, t'', i, n)
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
      | TypeAppT { reason; use_op; type_; targs; from_value; use_desc } ->
        let type_' = self#type_ cx map_cx type_ in
        let targs' = ListUtils.ident_map (self#type_ cx map_cx) targs in
        if type_ == type_' && targs == targs' then
          t
        else
          TypeAppT { reason; use_op; type_ = type_'; targs = targs'; from_value; use_desc }
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
      | ModuleT { module_reason; module_export_types; module_is_strict; module_available_platforms }
        ->
        let module_export_types' = self#export_types cx map_cx module_export_types in
        if module_export_types == module_export_types' then
          t
        else
          ModuleT
            {
              module_reason;
              module_export_types = module_export_types';
              module_is_strict;
              module_available_platforms;
            }
      | NamespaceT namespace_t ->
        let namespace_t' = self#namespace_type cx map_cx namespace_t in
        if namespace_t' == namespace_t then
          t
        else
          NamespaceT namespace_t'
      | InternalT (ExtendsT (r, t1, t2)) ->
        let t1' = self#type_ cx map_cx t1 in
        let t2' = self#type_ cx map_cx t2 in
        if t1' == t1 && t2' == t2 then
          t
        else
          InternalT (ExtendsT (r, t1', t2'))
      | InternalT (ChoiceKitT _) -> t
      | InternalT (EnforceUnionOptimized _) -> t
      | CustomFunT (r, kind) ->
        let kind' = self#custom_fun_kind cx map_cx kind in
        if kind' == kind then
          t
        else
          CustomFunT (r, kind')
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

    method private call_arg cx map_cx a =
      match a with
      | Arg t ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          a
        else
          Arg t'
      | SpreadArg t ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          a
        else
          SpreadArg t'

    method enum cx map_cx e =
      let { enum_name; enum_id; members; representation_t; has_unknown_members } = e in
      let representation_t' = self#type_ cx map_cx representation_t in
      if representation_t' = representation_t then
        e
      else
        { enum_name; enum_id; members; representation_t = representation_t'; has_unknown_members }

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
      | InstanceT instance_t ->
        let instance_t' = self#instance_type cx map_cx instance_t in
        if instance_t' == instance_t then
          t
        else
          InstanceT instance_t'
      | NumericStrKeyT _
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
      | ReactAbstractComponentT { config; instance; renders; component_kind } ->
        let config' = self#type_ cx map_cx config in
        let instance' = self#type_ cx map_cx instance in
        let renders' = self#type_ cx map_cx renders in
        if config' == config && instance' == instance && renders' == renders then
          t
        else
          ReactAbstractComponentT
            { config = config'; instance = instance'; renders = renders'; component_kind }
      | RendersT canonical_form ->
        let canonical_form' = self#canonical_renders_form cx map_cx canonical_form in
        if canonical_form' == canonical_form then
          t
        else
          RendersT canonical_form'

    method private canonical_renders_form cx map_cx form =
      match form with
      | NominalRenders { renders_id; renders_name; renders_super } ->
        let renders_super' = self#type_ cx map_cx renders_super in
        if renders_super' == renders_super then
          form
        else
          NominalRenders { renders_id; renders_name; renders_super = renders_super' }
      | StructuralRenders { renders_variant; renders_structural_type } ->
        let renders_structural_type' = self#type_ cx map_cx renders_structural_type in
        if renders_structural_type' == renders_structural_type then
          form
        else
          StructuralRenders { renders_variant; renders_structural_type = renders_structural_type' }

    method defer_use_type cx map_cx t =
      match t with
      | TypeDestructorT (u, r, d) ->
        let d' = self#destructor cx map_cx d in
        if d == d' then
          t
        else
          TypeDestructorT (u, r, d')

    method export_types
        cx
        map_cx
        ({ value_exports_tmap; type_exports_tmap; cjs_export; has_every_named_export } as t) =
      let value_exports_tmap' = self#exports cx map_cx value_exports_tmap in
      let type_exports_tmap' = self#exports cx map_cx type_exports_tmap in
      let cjs_export' = OptionUtils.ident_map (self#type_ cx map_cx) cjs_export in
      if
        value_exports_tmap == value_exports_tmap'
        && type_exports_tmap == type_exports_tmap'
        && cjs_export == cjs_export'
      then
        t
      else
        {
          value_exports_tmap = value_exports_tmap';
          type_exports_tmap = type_exports_tmap';
          cjs_export = cjs_export';
          has_every_named_export;
        }

    method fun_type
        cx
        map_cx
        ( { this_t = (this, subtyping); params; rest_param; return_t; predicate; def_reason; hook }
        as t
        ) =
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
      let predicate' = OptionUtils.ident_map (self#func_predicate cx map_cx) predicate in
      let return_t' = self#type_ cx map_cx return_t in
      if
        this' == this
        && return_t' == return_t
        && params' == params
        && rest_param' == rest_param
        && predicate' == predicate
      then
        t
      else
        let this_t = (this', subtyping) in
        let return_t = return_t' in
        let params = params' in
        let rest_param = rest_param' in
        let predicate = predicate' in
        { this_t; params; rest_param; return_t; predicate; def_reason; hook }

    method inst_type cx map_cx i =
      let {
        class_id;
        class_name;
        type_args;
        own_props;
        proto_props;
        inst_call_t;
        initialized_fields;
        initialized_static_fields;
        inst_kind;
        inst_dict;
        class_private_fields;
        class_private_static_fields;
        class_private_methods;
        class_private_static_methods;
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
      let inst_dict' = OptionUtils.ident_map (self#dict_type cx map_cx) inst_dict in
      let class_private_fields' = self#props cx map_cx class_private_fields in
      let class_private_static_fields' = self#props cx map_cx class_private_static_fields in
      let class_private_methods' = self#props cx map_cx class_private_methods in
      let class_private_static_methods' = self#props cx map_cx class_private_static_methods in
      if
        type_args == type_args'
        && own_props == own_props'
        && proto_props == proto_props'
        && inst_call_t == inst_call_t'
        && inst_dict == inst_dict'
        && class_private_fields == class_private_fields'
        && class_private_static_fields == class_private_static_fields'
        && class_private_methods == class_private_methods'
        && class_private_static_methods == class_private_static_methods'
      then
        i
      else
        {
          class_id;
          class_name;
          type_args = type_args';
          own_props = own_props';
          proto_props = proto_props';
          inst_call_t = inst_call_t';
          initialized_fields;
          initialized_static_fields;
          inst_kind;
          inst_dict = inst_dict';
          class_private_fields = class_private_fields';
          class_private_static_fields = class_private_static_fields';
          class_private_methods = class_private_methods';
          class_private_static_methods = class_private_static_methods';
        }

    method instance_type cx map_cx t =
      let { static; super; implements; inst } = t in
      let static' = self#type_ cx map_cx static in
      let super' = self#type_ cx map_cx super in
      let implements' = ListUtils.ident_map (self#type_ cx map_cx) implements in
      let inst' = self#inst_type cx map_cx inst in
      if static' == static && super' == super && implements' == implements && inst' == inst then
        t
      else
        { static = static'; super = super'; implements = implements'; inst = inst' }

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
      | ReactCheckComponentConfig map ->
        let map' = NameUtils.Map.ident_map (self#prop cx map_cx) map in
        if map' == map then
          t
        else
          ReactCheckComponentConfig map'
      | ReactCheckComponentRef
      | ReactDRO _
      | MakeHooklike
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
      | RequiredType
      | PartialType ->
        t
      | SpreadType (options, tlist, acc) ->
        let tlist' = ListUtils.ident_map (self#object_kit_spread_operand cx map_cx) tlist in
        let acc' = OptionUtils.ident_map (self#object_kit_spread_operand_slice cx map_cx) acc in
        if tlist' == tlist && acc == acc' then
          t
        else
          SpreadType (options, tlist', acc')
      | SpreadTupleType { reason_tuple; reason_spread; resolved; unresolved } ->
        let unresolved' =
          ListUtils.ident_map
            (fun unresolved_el ->
              match unresolved_el with
              | UnresolvedArg (element, generic) ->
                let element' = self#tuple_element cx map_cx element in
                if element' == element then
                  unresolved_el
                else
                  UnresolvedArg (element', generic)
              | UnresolvedSpreadArg t ->
                let t' = self#type_ cx map_cx t in
                if t' == t then
                  unresolved_el
                else
                  UnresolvedSpreadArg t)
            unresolved
        in
        let resolved' =
          ListUtils.ident_map
            (fun resolved_el ->
              match resolved_el with
              | ResolvedArg (element, generic) ->
                let element' = self#tuple_element cx map_cx element in
                if element' == element then
                  resolved_el
                else
                  ResolvedArg (element', generic)
              | ResolvedSpreadArg (reason, arr, generic) ->
                let arr' = self#arr_type cx map_cx arr in
                if arr' == arr then
                  resolved_el
                else
                  ResolvedSpreadArg (reason, arr', generic)
              | ResolvedAnySpreadArg _ -> resolved_el)
            resolved
        in
        if resolved' == resolved && unresolved' == unresolved then
          t
        else
          SpreadTupleType
            { reason_tuple; reason_spread; resolved = resolved'; unresolved = unresolved' }
      | RestType (options, x) ->
        let x' = self#type_ cx map_cx x in
        if x' == x then
          t
        else
          RestType (options, x')
      | ValuesType -> t
      | CallType { from_maptype; args } ->
        let args' = ListUtils.ident_map (self#type_ cx map_cx) args in
        if args' == args then
          t
        else
          CallType { from_maptype; args = args' }
      | ConditionalType { distributive_tparam_name; infer_tparams; extends_t; true_t; false_t } ->
        let infer_tparams' = ListUtils.ident_map (self#type_param cx map_cx) infer_tparams in
        let extends_t' = self#type_ cx map_cx extends_t in
        let true_t' = self#type_ cx map_cx true_t in
        let false_t' = self#type_ cx map_cx false_t in
        if
          infer_tparams' == infer_tparams
          && extends_t' == extends_t
          && true_t' == true_t
          && false_t' == false_t
        then
          t
        else
          ConditionalType
            {
              distributive_tparam_name;
              infer_tparams = infer_tparams';
              extends_t = extends_t';
              true_t = true_t';
              false_t = false_t';
            }
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
      | MappedType { property_type; mapped_type_flags; homomorphic; distributive_tparam_name } ->
        let property_type' = self#type_ cx map_cx property_type in
        let homomorphic' =
          match homomorphic with
          | SemiHomomorphic t ->
            let t' = self#type_ cx map_cx t in
            if t' == t then
              homomorphic
            else
              SemiHomomorphic t'
          | Homomorphic
          | Unspecialized ->
            homomorphic
        in
        if property_type' == property_type && homomorphic' == homomorphic then
          t
        else
          MappedType
            {
              distributive_tparam_name;
              property_type = property_type';
              mapped_type_flags;
              homomorphic = homomorphic';
            }
      | ReactPromoteRendersRepresentation
          { should_distribute; promote_structural_components; renders_variant; resolved_elem } ->
        let resolved_elem' = OptionUtils.ident_map (self#type_ cx map_cx) resolved_elem in
        if resolved_elem' == resolved_elem then
          t
        else
          ReactPromoteRendersRepresentation
            { should_distribute; promote_structural_components; renders_variant; resolved_elem }
      | ReactElementPropsType
      | ReactElementConfigType
      | ReactElementRefType ->
        t

    method object_kit_spread_operand_slice
        cx map_cx ({ Object.Spread.reason; prop_map; dict; generics; reachable_targs } as slice) =
      let prop_map' = NameUtils.Map.ident_map (self#prop cx map_cx) prop_map in
      let dict' = OptionUtils.ident_map (self#dict_type cx map_cx) dict in
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
      if prop_map' == prop_map && dict' == dict && reachable_targs' == reachable_targs then
        slice
      else
        {
          Object.Spread.reason;
          prop_map = prop_map';
          dict = dict';
          generics;
          reachable_targs = reachable_targs';
        }

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
      | ReactCreateElement
      | ReactCloneElement
      | DebugPrint
      | DebugThrow
      | DebugSleep ->
        kind

    method private func_predicate cx map_cx predicate =
      match predicate with
      | PredBased p ->
        let p' = self#predicate_maps cx map_cx p in
        if p == p' then
          predicate
        else
          PredBased p'
      | TypeGuardBased { param_name; type_guard = t } ->
        let t' = self#type_ cx map_cx t in
        if t' == t then
          predicate
        else
          TypeGuardBased { param_name; type_guard = t' }

    method private predicate_maps cx map_cx predicate =
      let (reason, (lazy (pmap, nmap))) = predicate in
      let pmap' = Key_map.ident_map (self#predicate cx map_cx) pmap in
      let nmap' = Key_map.ident_map (self#predicate cx map_cx) nmap in
      if pmap == pmap' && nmap == nmap' then
        predicate
      else
        (reason, lazy (pmap', nmap'))

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

    method namespace_type cx map_cx t =
      let { values_type; types_tmap } = t in
      let values_type' = self#type_ cx map_cx values_type in
      let types_tmap' = self#props cx map_cx types_tmap in
      if values_type' == values_type && types_tmap' == types_tmap then
        t
      else
        { values_type = values_type'; types_tmap = types_tmap' }

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
      | ArrayAT { elem_t; tuple_view; react_dro } ->
        let elem_t' = self#type_ cx map_cx elem_t in
        let tuple_view' =
          OptionUtils.ident_map
            (fun tuple_view ->
              let (elements, arity) = tuple_view in
              let elements' = ListUtils.ident_map (self#tuple_element cx map_cx) elements in
              if elements' == elements then
                tuple_view
              else
                (elements', arity))
            tuple_view
        in
        if elem_t' == elem_t && tuple_view' == tuple_view then
          t
        else
          ArrayAT { elem_t = elem_t'; tuple_view = tuple_view'; react_dro }
      | TupleAT { elem_t; elements; arity; react_dro } ->
        let elem_t' = self#type_ cx map_cx elem_t in
        let elements' = ListUtils.ident_map (self#tuple_element cx map_cx) elements in
        if elem_t' == elem_t && elements' == elements then
          t
        else
          TupleAT { elem_t = elem_t'; elements = elements'; arity; react_dro }
      | ROArrayAT (t', dro) ->
        let t'' = self#type_ cx map_cx t' in
        if t'' == t' then
          t
        else
          ROArrayAT (t'', dro)

    method private tuple_element cx map_cx element =
      let (TupleElement { reason; name; t; polarity; optional }) = element in
      let t' = self#type_ cx map_cx t in
      if t' == t then
        element
      else
        TupleElement { reason; name; t = t'; polarity; optional }

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
      | PropExistsP _
      | NoP ->
        p
      | LatentP ((lazy (use_op, loc, t, targs, argts)), i) ->
        let t' = self#type_ cx map_cx t in
        let targs' = OptionUtils.ident_map (ListUtils.ident_map (self#targ cx map_cx)) targs in
        let argts' = ListUtils.ident_map (self#call_arg cx map_cx) argts in
        if t == t' && targs' == targs && argts' == argts then
          p
        else
          LatentP (lazy (use_op, loc, t', targs', argts'), i)

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

    method prop cx map_cx prop = Property.ident_map_t (self#type_ cx map_cx) prop
  end
