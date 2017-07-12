(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Type

let maybe_known f x =
  let open React.CreateClass in
  begin match x with
    | Known x' ->
        let x'' = f x' in
        if x'' == x' then x
        else Known x''
    | Unknown x -> Unknown x
  end

(* This class should be used when trying to perform some mapping function on
 * a type. It will recurse through the structure of the type, applying it to
 * each sub-part.
 *)

class t = object(self)
  val mutable seen_tvars = ISet.empty

  method type_ cx t =
    match t with
      | OpenT (_, id) ->
          if not (ISet.mem id seen_tvars)
          then begin
            seen_tvars <- ISet.add id seen_tvars;
            let open Constraint in
            let rid, root = Flow_js.find_root cx id in
            match root.constraints with
              | Resolved t' ->
                  let t'' = self#type_ cx t' in
                  if t'' != t' then
                    let node = Resolved t'' in
                    Context.set_tvar cx rid (Root {rank = root.rank; constraints = node})
              | Unresolved bounds ->
                  let bounds' = self#bounds cx bounds in
                  if bounds != bounds' then
                    let node = Unresolved bounds' in
                    Context.set_tvar cx rid (Root {rank = root.rank; constraints = node})
          end;
          t
      | DefT (r, t') ->
          let t'' = self#def_type cx t' in
          if t' == t'' then t else DefT (r, t'')
      | EvalT (t', dt, i) ->
          let t'' = self#type_ cx t' in
          let dt' = self#defer_use_type cx dt in
          if t' == t'' && dt == dt' then t
          else EvalT (t'', dt', i)
      | BoundT t' ->
          let t'' = self#type_param cx t' in
          if t'' == t' then t
          else BoundT t''
      | ExistsT _ -> t
      | ThisClassT (r, t') ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else ThisClassT (r, t'')
      | ThisTypeAppT (r, t1, t2, tlist) ->
          let t1' = self#type_ cx t1 in
          let t2' = self#type_ cx t2 in
          let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
          if t1' == t1 && t2' == t2 && tlist' == tlist then t
          else ThisTypeAppT(r, t1', t2', tlist')
      | ExactT (r, t') ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else ExactT (r, t'')
      | TaintT _
      | FunProtoT _
      | ObjProtoT _
      | FunProtoApplyT _
      | FunProtoBindT _
      | FunProtoCallT _ -> t
      | AnyWithLowerBoundT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else AnyWithLowerBoundT t''
      | AnyWithUpperBoundT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else AnyWithUpperBoundT t''
      | ShapeT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else ShapeT t''
      | DiffT (t1, t2) ->
          let t1' = self#type_ cx t1 in
          let t2' = self#type_ cx t2 in
          if t1 == t1' && t2 == t2' then t
          else DiffT (t1', t2')
      | KeysT (r, t') ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else KeysT (r, t'')
      | AbstractT (r, t') ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else AbstractT (r, t'')
      | AnnotT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else AnnotT t''
      | ModuleT (r, exporttypes) ->
          let exporttypes' = self#export_types cx exporttypes in
          if exporttypes == exporttypes' then t
          else ModuleT (r, exporttypes')
      | ExtendsT (r, tlist, t1, t2) ->
          let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
          let t1' = self#type_ cx t1 in
          let t2' = self#type_ cx t2 in
          if tlist' == tlist && t1' == t1 && t2' == t2 then t
          else ExtendsT (r, tlist', t1', t2')
      | ChoiceKitT _
      | CustomFunT _ -> t
      | IdxWrapper (r, t') ->
          let t'' = self#type_ cx t' in
          if t' == t'' then t
          else IdxWrapper (r, t'')
      | OpenPredT (r, t', map1, map2) ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else OpenPredT (r, t'', map1, map2)
      | TypeMapT (r, tmap, t1, t2) ->
          let t1' = self#type_ cx t1 in
          let t2' = self#type_ cx t2 in
          if t1 == t1' && t2 == t2' then t
          else TypeMapT (r, tmap, t1', t2')
      | ReposT (r, t') ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else ReposT (r, t'')
      | ReposUpperT (r, t') ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else ReposUpperT (r, t'')

  method def_type cx t =
    match t with
      | NumT _
      | StrT _
      | BoolT _
      | EmptyT
      | MixedT _
      | NullT
      | VoidT -> t
      | FunT (s, p, f) ->
          let s' = self#type_ cx s in
          let p' = self#type_ cx p in
          let f' = self#fun_type cx f in
          if s == s' && p == p' && f == f' then t
          else FunT (s', p', f')
      | ObjT objtype ->
          let objtype' = self#obj_type cx objtype in
          if objtype' == objtype then t
          else ObjT objtype'
      | ArrT arrtype ->
          let arrtype' = self#arr_type cx arrtype in
          if arrtype == arrtype' then t
          else ArrT arrtype'
      | ClassT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else ClassT t''
      | InstanceT (st, su, impl, instt) ->
          let st' = self#type_ cx st in
          let su' = self#type_ cx su in
          let impl' = ListUtils.ident_map (self#type_ cx) impl in
          let instt' = self#inst_type cx instt in
          if st' == st' && su' == su && impl' == impl && instt' == instt then t
          else InstanceT (st', su', impl', instt')
      | SingletonStrT _
      | SingletonNumT _
      | SingletonBoolT _ -> t
      | TypeT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else TypeT t''
      | AnyT -> t
      | OptionalT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else OptionalT t''
      | PolyT (tparamlist, t') ->
          let tparamlist' = ListUtils.ident_map (self#type_param cx) tparamlist in
          let t'' = self#type_ cx t' in
          if tparamlist == tparamlist' && t' == t'' then t
          else PolyT (tparamlist', t'')
      | TypeAppT (t', ts) ->
          let t'' = self#type_ cx t' in
          let ts' = ListUtils.ident_map (self#type_ cx) ts in
          if t' == t'' && ts == ts' then t
          else TypeAppT (t'', ts')
      | MaybeT t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else MaybeT t''
      | IntersectionT irep ->
          let irep' = InterRep.ident_map (self#type_ cx) irep in
          if irep == irep' then t
          else IntersectionT irep'
      | UnionT urep ->
          let urep' = UnionRep.ident_map (self#type_ cx) urep in
          if urep' == urep then t
          else UnionT urep'
      | AnyObjT
      | AnyFunT -> t

  method defer_use_type cx t =
    match t with
    | DestructuringT (r, s) ->
        let s' = self#selector cx s in
        if s' == s then t
        else DestructuringT (r, s')
    | TypeDestructorT (r, d) ->
        let d' = self#destructor cx d in
        if d == d' then t
        else TypeDestructorT (r, d')

  method export_types cx ({exports_tmap; cjs_export; has_every_named_export} as t) =
    let exports_tmap' = self#exports cx exports_tmap in
    let cjs_export' = OptionUtils.ident_map (self#type_ cx) cjs_export in
    if exports_tmap == exports_tmap' && cjs_export == cjs_export' then t
    else {exports_tmap = exports_tmap'; cjs_export = cjs_export'; has_every_named_export}

  method fun_type cx ({ this_t;
                       params_tlist;
                       params_names;
                       rest_param;
                       return_t;
                       closure_t;
                       is_predicate;
                       changeset;
                       def_reason } as t) =
    let this_t' = self#type_ cx this_t in
    let return_t' = self#type_ cx return_t in
    let params_tlist' = ListUtils.ident_map (self#type_ cx) params_tlist in
    if this_t' == this_t && return_t' == return_t && params_tlist' == params_tlist then t
    else
      let this_t = this_t' in
      let return_t = return_t' in
      let params_tlist = params_tlist' in
      {this_t; params_tlist; params_names; rest_param; return_t;
       closure_t; is_predicate; changeset; def_reason}

  method inst_type cx ({ class_id;
                        type_args;
                        arg_polarities;
                        fields_tmap;
                        initialized_field_names;
                        methods_tmap;
                        mixins;
                        structural } as t) =
    let type_args' = SMap.ident_map (self#type_ cx) type_args in
    let f_tmap = Context.find_props cx fields_tmap in
    let f_tmap' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) f_tmap in
    let fields_tmap' =
      if f_tmap == f_tmap' then fields_tmap
      else Context.make_property_map cx f_tmap' in
    let m_tmap = Context.find_props cx methods_tmap in
    let m_tmap' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) m_tmap in
    let methods_tmap' =
      if m_tmap == m_tmap' then methods_tmap
      else Context.make_property_map cx m_tmap in
    if type_args == type_args' && methods_tmap == methods_tmap' && fields_tmap == fields_tmap'
    then t
    else
      {class_id; type_args = type_args'; arg_polarities; fields_tmap = fields_tmap';
     initialized_field_names; methods_tmap = methods_tmap'; mixins; structural}

  method type_param cx ({reason; name; bound; polarity; default} as t) =
    let bound' = self#type_ cx bound in
    let default' = OptionUtils.ident_map (self#type_ cx) default in
    if bound == bound' && default == default' then t
    else
      let bound = bound' in
      let default = default' in
      {reason; name; bound; polarity; default}

  method selector cx t =
    match t with
      | Prop _ -> t
      | Elem t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else Elem t''
      | ObjRest _
      | ArrRest _
      | Default
      | Become
      | Refine _ -> t

  method destructor cx t =
    match t with
      | NonMaybeType
      | PropertyType _ -> t
      | ElementType t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else ElementType t''
      | Bind t' ->
          let t'' = self#type_ cx t' in
          if t'' == t' then t
          else Bind t''
      | SpreadType (b, tlist) ->
          let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
          if tlist' == tlist then t
          else SpreadType (b, tlist')
      | ValuesType -> t

  method exports cx id =
    let exps = Context.find_exports cx id in
    let exps' = SMap.ident_map (self#type_ cx) exps in
    if exps == exps' then id
    else Context.make_export_map cx exps'

  method obj_type cx ({ flags; dict_t; props_tmap; proto_t} as t) =
    let dict_t' = OptionUtils.ident_map (self#dict_type cx) dict_t in
    let p_tmap = Context.find_props cx props_tmap in
    let p_tmap' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) p_tmap in
    let props_tmap' = if p_tmap == p_tmap' then props_tmap
          else Context.make_property_map cx p_tmap' in
    let proto_t' = self#type_ cx proto_t in
    if dict_t' == dict_t && props_tmap' == props_tmap && proto_t' == proto_t then t
    else
      { flags; dict_t = dict_t'; props_tmap = props_tmap'; proto_t = proto_t' }

  method dict_type cx ({dict_name; key; value; dict_polarity} as t) =
    let key' = self#type_ cx key in
    let value' = self#type_ cx value in
    if key' == key && value' == value then t
    else
      let key = key' in
      let value = value' in
      {dict_name; key; value; dict_polarity}

  method arr_type cx t =
    match t with
    | ArrayAT (t', tlistopt) ->
        let t'' = self#type_ cx t' in
        let tlistopt' = OptionUtils.ident_map (ListUtils.ident_map (self#type_ cx)) tlistopt in
        if t'' == t' && tlistopt' == tlistopt then t
        else ArrayAT (t'', tlistopt')
    | TupleAT (t', tlist) ->
        let t'' = self#type_ cx t' in
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        if t'' == t' && tlist' == tlist then t
        else TupleAT(t'', tlist')
    | ROArrayAT t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ROArrayAT t''
    | EmptyAT -> t

  method bounds cx t =
    let open Constraint in
    let lower' = TypeMap.ident_map_key (self#type_ cx) t.lower in
    if lower' != t.lower then
      t.lower <- lower';
    let upper' = UseTypeMap.ident_map_key (self#use_type cx) t.upper in
    if upper' != t.upper then
      t.upper <- upper';
    t

  method use_type cx t =
    match t with
    | UseT (u, t') ->
        let t'' = self#type_ cx t'; in
        if t'' == t' then t
        else UseT (u, t'')
    | BindT (r, funcall, passthrough) ->
        let funcall' = self#fun_call_type cx funcall in
        if funcall == funcall' then t
        else BindT (r, funcall', passthrough)
    | CallT (r, funcall) ->
        let funcall' = self#fun_call_type cx funcall in
        if funcall == funcall' then t
        else CallT (r, funcall')
    | MethodT (r1, r2, prop, funcall) ->
        let prop' = self#prop_ref cx prop in
        let funcall' = self#fun_call_type cx funcall in
        if prop' == prop && funcall' == funcall then t
        else MethodT (r1, r2, prop', funcall')
    | SetPropT (r, prop, t') ->
        let prop' = self#prop_ref cx prop in
        let t'' = self#type_ cx t' in
        if prop' == prop && t'' == t' then t
        else SetPropT (r, prop', t'')
    | GetPropT (r, prop, t') ->
        let prop' = self#prop_ref cx prop in
        let t'' = self#type_ cx t' in
        if prop' == prop && t'' == t' then t
        else GetPropT (r, prop', t'')
    | TestPropT (r, prop, t') ->
        let prop' = self#prop_ref cx prop in
        let t'' = self#type_ cx t' in
        if prop' == prop && t'' == t' then t
        else TestPropT (r, prop', t'')
    | SetElemT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else SetElemT (r, t1', t2')
    | GetElemT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else GetElemT (r, t1', t2')
    | CallElemT (r1, r2, t', funcall) ->
        let t'' = self#type_ cx t' in
        let funcall' = self#fun_call_type cx funcall in
        if t' == t'' && funcall' == funcall then t
        else CallElemT (r1, r2, t'', funcall')
    | GetStaticsT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else GetStaticsT (r, t'')
    | GetProtoT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else GetProtoT (r, t'')
    | SetProtoT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else SetProtoT (r, t'')
    | ReposLowerT (r, use) ->
        let use' = self#use_type cx use in
        if use' == use then t
        else ReposLowerT (r, use')
    | ReposUseT (r, use_op, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ReposUseT (r, use_op, t'')
    | ConstructorT (r, callargs, t') ->
        let callargs' = ListUtils.ident_map (self#call_arg cx) callargs in
        let t'' = self#type_ cx t' in
        if callargs' == callargs && t'' == t' then t
        else ConstructorT (r, callargs', t'')
    | SuperT (r, instt) ->
        let instt' = self#inst_type cx instt in
        if instt' == instt then t
        else SuperT (r, instt')
    | ImplementsT t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ImplementsT t''
    | MixinT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else MixinT (r, t'')
    | AdderT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else AdderT (r, t1', t2')
    | ComparatorT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ComparatorT (r, t'')
    | UnaryMinusT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else UnaryMinusT (r, t'')
    | AssertArithmeticOperandT _
    | AssertBinaryInLHST _
    | AssertBinaryInRHST _
    | AssertForInRHST _
    | AssertRestParamT _ -> t
    | PredicateT (p, t') ->
        let p' = self#predicate cx p in
        let t'' = self#type_ cx t' in
        if p' == p && t'' == t' then t
        else PredicateT (p', t'')
    | GuardT (p, t1, t2) ->
        let p' = self#predicate cx p in
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if p' == p && t1' == t1 && t2' == t2 then t
        else GuardT (p', t1', t2')
    | EqT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else EqT (r, t'')
    | AndT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else AndT (r, t1', t2')
    | OrT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else OrT (r, t1', t2')
    | NotT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else NotT (r, t'')
    | SpecializeT (r1, r2, cache, tlist, t') ->
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        let t'' = self#type_ cx t' in
        if tlist' == tlist && t'' == t' then t
        else SpecializeT (r1, r2, cache, tlist', t'')
    | ThisSpecializeT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else ThisSpecializeT (r, t1', t2')
    | VarianceCheckT (r, tlist, p) ->
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        if tlist' == tlist then t
        else VarianceCheckT (r, tlist', p)
    | TypeAppVarianceCheckT (r1, r2, tpairlist) ->
        let tpairlist' = ListUtils.ident_map (fun ((x,y) as z) ->
          let x' = self#type_ cx x in
          let y' = self#type_ cx y in
          if x' == x && y' == y then z
          else (x', y'))
          tpairlist in
        if tpairlist' == tpairlist then t
        else TypeAppVarianceCheckT (r1, r2, tpairlist')
    | LookupT (r, lookup, tlist, prop, action) ->
        let lookup' = self#lookup_kind cx lookup in
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        let prop' = self#prop_ref cx prop in
        let action' = self#lookup_action cx action in
        if lookup' == lookup && tlist' == tlist && prop' == prop && action' == action then t
        else LookupT (r, lookup', tlist', prop', action')
    | ObjAssignToT (r, t1, t2, strings, obj_assign) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else ObjAssignToT (r, t1', t2', strings, obj_assign)
    | ObjAssignFromT (r, t1, t2, strings, obj_assign) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else ObjAssignFromT (r, t1', t2', strings, obj_assign)
    | ObjFreezeT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ObjFreezeT (r, t'')
    | ObjRestT (r, strings, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ObjRestT (r, strings, t'')
    | ObjSealT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ObjSealT (r, t'')
    | ObjTestT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else ObjTestT (r, t1', t2')
    | ArrRestT (r, i, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ArrRestT (r, i, t'')
    | UnifyT (t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else UnifyT (t1', t2')
    | BecomeT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else BecomeT (r, t'')
    | GetKeysT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else GetKeysT (r, t'')
    | HasOwnPropT _ -> t
    | GetValuesT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else GetValuesT (r, t'')
    | ElemT (r, t', action) ->
        let t'' = self#type_ cx t' in
        let action' = self#elem_action cx action in
        if t'' == t' && action' == action then t
        else ElemT (r, t'', action')
    | MakeExactT (r, cont) ->
        let cont' = self#cont cx cont in
        if cont' == cont then t
        else MakeExactT (r, cont')
    | CJSRequireT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else CJSRequireT (r, t'')
    | ImportModuleNsT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ImportModuleNsT (r, t'')
    | ImportDefaultT (r, import, s, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ImportDefaultT (r, import, s, t'')
    | ImportNamedT (r, import, s, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ImportNamedT (r, import, s, t'')
    | ImportTypeT (r, s, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ImportTypeT (r, s, t'')
    | ImportTypeofT (r, s, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ImportTypeofT (r, s, t'')
    | AssertImportIsValueT _ -> t
    | CJSExtractNamedExportsT (r1, (r2, exports), t') ->
        let exports' = self#export_types cx exports in
        let t'' = self#type_ cx t' in
        if exports' == exports && t'' == t' then t
        else CJSExtractNamedExportsT (r1, (r2, exports'), t'')
    | CopyNamedExportsT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else CopyNamedExportsT (r, t1', t2')
    | CopyTypeExportsT (r, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else CopyTypeExportsT (r, t1', t2')
    | ExportNamedT (r, skip, tmap, t') ->
        let tmap' = SMap.ident_map (self#type_ cx) tmap in
        let t'' = self#type_ cx t' in
        if tmap' == tmap && t'' == t' then t
        else ExportNamedT (r, skip, tmap', t'')
    | ExportTypeT (r, skip, name, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else ExportTypeT (r, skip, name, t1', t2')
    | MapTypeT (r, typemap, t', cont) ->
        let t'' = self#type_ cx t' in
        let cont' = self#cont cx cont in
        if t'' == t' && cont' == cont then t
        else MapTypeT (r, typemap, t'', cont')
    | ReactKitT (r, react_tool) ->
        let react_tool' = self#react_tool cx react_tool in
        if react_tool' == react_tool then t
        else ReactKitT (r, react_tool')
    | ObjSpreadT (r, tool, state, t') ->
        let tool' = self#object_spread_tool cx tool in
        let state' = self#object_spread_state cx state in
        let t'' = self#type_ cx t' in
        if tool' == tool && state' == state && t'' == t' then t
        else ObjSpreadT (r, tool', state', t'')
    | ChoiceKitUseT (r, choice_use_tool) ->
        let choice_use_tool' = self#choice_use_tool cx choice_use_tool in
        if choice_use_tool' == choice_use_tool then t
        else ChoiceKitUseT (r, choice_use_tool')
    | IntersectionPreprocessKitT (r, ipt) ->
        let ipt' = self#intersection_preprocess_tool cx ipt in
        if ipt' == ipt then t
        else IntersectionPreprocessKitT (r, ipt')
    | DebugPrintT _ -> t
    | SentinelPropTestT (t1, b, sentinel, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else SentinelPropTestT (t1', b, sentinel, t2')
    | IdxUnwrap (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else IdxUnwrap (r, t'')
    | IdxUnMaybeifyT (r, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else IdxUnMaybeifyT (r, t'')
    | CallLatentPredT (r, b, i, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else CallLatentPredT (r, b, i, t1', t2')
    | CallOpenPredT (r, b, key, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else CallOpenPredT (r, b, key, t1', t2')
    | SubstOnPredT (r, sub, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else SubstOnPredT (r, sub, t'')
    | RefineT (r, p, t') ->
        let p' = self#predicate cx p in
        let t'' = self#type_ cx t' in
        if p' == p && t'' == t' then t
        else RefineT (r, p', t'')
    | ResolveSpreadT (r, resolve_spread) ->
        let resolve_spread' = self#resolve_spread cx resolve_spread in
        if resolve_spread' == resolve_spread then t
        else ResolveSpreadT (r, resolve_spread')

  method fun_call_type cx ({call_this_t; call_args_tlist; call_tout;
      call_closure_t; call_strict_arity} as t) =
    let call_this_t' = self#type_ cx call_this_t in
    let call_args_tlist' = ListUtils.ident_map (self#call_arg cx) call_args_tlist in
    let call_tout' = self#type_ cx call_tout in
    if call_this_t' == call_this_t && call_args_tlist' == call_args_tlist
      && call_tout' == call_tout
    then t
    else {call_this_t = call_this_t'; call_args_tlist = call_args_tlist';
      call_tout = call_tout'; call_closure_t; call_strict_arity}

  method prop_ref cx t =
    match t with
    | Named _ -> t
    | Computed t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else Computed t''

  method call_arg cx t =
    match t with
    | Arg t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else Arg t''
    | SpreadArg t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else SpreadArg t''

  method predicate cx p =
    match p with
    | AndP (p1, p2) ->
        let p1' = self#predicate cx p1 in
        let p2' = self#predicate cx p2 in
        if p1' == p1 && p2' == p2 then p
        else AndP (p1', p2')
    | OrP (p1, p2) ->
        let p1' = self#predicate cx p1 in
        let p2' = self#predicate cx p2 in
        if p1' == p1 && p2' == p2 then p
        else OrP (p1', p2')
    | NotP p' ->
        let p'' = self#predicate cx p' in
        if p'' == p' then p
        else NotP p''
    | LeftP (test, t) ->
        let t' = self#type_ cx t in
        if t' == t then p
        else LeftP (test, t')
    | RightP (test, t) ->
        let t' = self#type_ cx t in
        if t' == t then p
        else RightP (test, t')
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
    | VoidP
    | ArrP
    | PropExistsP _ -> p
    | LatentP (t, i) ->
        let t' = self#type_ cx t in
        if t' == t then p
        else LatentP (t', i)

  method lookup_kind cx t =
    match t with
    | Strict _ -> t
    | NonstrictReturning tpairopt ->
        begin match tpairopt with
        | Some (t1, t2) ->
            let t1' = self#type_ cx t1 in
            let t2' = self#type_ cx t2 in
            if t1' == t1 && t2' == t2 then t
            else NonstrictReturning (Some (t1', t2'))
        | None -> t
        end
    | ShadowRead (r, pidlist) ->
        let pidlist' = Nel.ident_map (fun property_id ->
          let tmap = Context.find_props cx property_id in
          let tmap' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) tmap in
          if tmap' == tmap then property_id
          else Context.make_property_map cx tmap'
        ) pidlist in
        if pidlist == pidlist' then t
        else ShadowRead (r, pidlist')
    | ShadowWrite pidlist ->
        let pidlist' = Nel.ident_map (fun property_id ->
          let tmap = Context.find_props cx property_id in
          let tmap' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) tmap in
          if tmap' == tmap then property_id
          else Context.make_property_map cx tmap'
        ) pidlist in
        if pidlist == pidlist' then t
        else ShadowWrite pidlist'

  method lookup_action cx t =
    match t with
    | RWProp (t1, t2, rw) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t1 && t2' == t2 then t
        else RWProp (t1', t2', rw)
    | LookupProp (use, prop) ->
        let prop' = Property.ident_map_t (self#type_ cx) prop in
        if prop == prop' then t
        else LookupProp (use, prop')
    | SuperProp prop ->
        let prop' = Property.ident_map_t (self#type_ cx) prop in
        if prop == prop' then t
        else SuperProp prop'

  method elem_action cx t =
    match t with
    | ReadElem t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ReadElem t''
    | WriteElem t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else WriteElem t''
    | CallElem (r, funcall) ->
        let funcall' = self#fun_call_type cx funcall in
        if funcall' == funcall then t
        else CallElem (r, funcall')

  method cont cx t =
    match t with
    | Lower t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else Lower t''
    | Upper use_t ->
        let use_t' = self#use_type cx use_t in
        if use_t' == use_t then t
        else Upper use_t'

  method react_tool cx t =
    let open React in
    match t with
    | CreateElement (t1, t2) ->
      let t1' = self#type_ cx t1 in
      let t2' = self#type_ cx t2 in
      if t1' == t1 && t2' == t2 then t
      else CreateElement (t1', t2')
    | SimplifyPropType (tool, t') ->
        let tool' = self#simplify_prop_type_tool cx tool in
        let t'' = self#type_ cx t' in
        if tool' == tool && t'' == t' then t
        else SimplifyPropType (tool', t'')
    | CreateClass (tool, knot, t') ->
        let tool' = self#create_class_tool cx tool in
        let knot' = self#create_class_knot cx knot in
        let t'' = self#type_ cx t' in
        if tool' == tool && knot' == knot && t'' == t' then t
        else CreateClass (tool', knot', t'')

  method object_spread_tool cx t =
    let open ObjectSpread in
    match t with
    | Resolve r ->
        let r' = self#resolve cx r in
        if r' == r then t
        else Resolve r'
    | Super ((reason, props, dict, flags), r) ->
        let props' = SMap.ident_map (fun (t, b) -> (self#type_ cx t, b)) props in
        let dict' = OptionUtils.ident_map (self#dict_type cx) dict in
        let r' = self#resolve cx r in
        if r' == r && props' == props then t
        else Super ((reason, props', dict', flags), r')

  method object_spread_state cx t =
    let open ObjectSpread in
    let todo_rev' = ListUtils.ident_map (self#type_ cx) t.todo_rev in
    let acc' = ListUtils.ident_map (self#resolved cx) t.acc in
    if todo_rev' == t.todo_rev && acc' == t.acc then t
    else {todo_rev = todo_rev'; acc = acc'; make_exact = t.make_exact}

  method intersection_preprocess_tool cx t =
    match t with
    | ConcretizeTypes (tlist1, tlist2, t', use_t) ->
        let tlist1' = ListUtils.ident_map (self#type_ cx) tlist1 in
        let tlist2' = ListUtils.ident_map (self#type_ cx) tlist2 in
        let t'' = self#type_ cx t' in
        let use_t' = self#use_type cx use_t in
        if tlist1' == tlist1 && tlist2' == tlist2 && t'' == t' && use_t' == use_t then t
        else ConcretizeTypes (tlist1', tlist2', t'', use_t')
    | SentinelPropTest (b, s, t1, t2, t3) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        let t3' = self#type_ cx t3 in
        if t1' == t1 && t2' == t2 && t3' == t3 then t
        else SentinelPropTest (b, s, t1', t2', t3')
    | PropExistsTest (b, s, t1, t2) ->
        let t1' = self#type_ cx t1 in
        let t2' = self#type_ cx t2 in
        if t1' == t2 && t2' == t2 then t
        else PropExistsTest (b, s, t1', t2')

  method choice_use_tool cx t =
    match t with
    | FullyResolveType _ -> t
    | TryFlow (i, spec) ->
        let spec' = self#spec cx spec in
        if spec' == spec then t
        else TryFlow (i, spec')

  method resolve_spread cx ({rrt_resolved; rrt_unresolved; rrt_resolve_to} as t)=
    let rrt_resolved' = ListUtils.ident_map (self#resolved_param cx) rrt_resolved in
    let rrt_unresolved' = ListUtils.ident_map (self#unresolved_param cx) rrt_unresolved in
    let rrt_resolve_to' = self#spread_resolve cx rrt_resolve_to in
    if rrt_resolved' == rrt_resolved && rrt_unresolved' == rrt_unresolved
      && rrt_resolve_to' == rrt_resolve_to
    then t
    else {rrt_resolved = rrt_resolved'; rrt_unresolved = rrt_unresolved';
      rrt_resolve_to = rrt_resolve_to'}

  method simplify_prop_type_tool cx tool =
    let open React.SimplifyPropType in
    match tool with
    | ArrayOf
    | InstanceOf
    | ObjectOf -> tool
    | OneOf resolve_array ->
        let resolve_array' = self#resolve_array cx resolve_array in
        if resolve_array' == resolve_array then tool
        else OneOf resolve_array'
    | OneOfType resolve_array ->
        let resolve_array' = self#resolve_array cx resolve_array in
        if resolve_array' == resolve_array then tool
        else OneOfType resolve_array'
    | Shape resolve_object ->
        let resolve_object' = self#resolve_object cx resolve_object in
        if resolve_object' == resolve_object then tool
        else Shape resolve_object'

  method create_class_tool cx tool =
    let open React.CreateClass in
    match tool with
    | Spec tail ->
        let tail' = self#stack_tail cx tail in
        if tail' == tail then tool
        else Spec tail'
    | Mixins (head, tail) ->
        let head' = self#stack_head cx head in
        let tail' = self#stack_tail cx tail in
        if head' == head && tail' == tail then tool
        else Mixins (head', tail')
    | Statics (head, tail) ->
        let head' = self#stack_head cx head in
        let tail' = self#stack_tail cx tail in
        if head' == head && tail' == tail then tool
        else Statics (head', tail')
    | PropTypes ((head, tail), resolve_object) ->
        let head' = self#stack_head cx head in
        let tail' = self#stack_tail cx tail in
        let resolve_object' = self#resolve_object cx resolve_object in
        if head' == head && tail' == tail && resolve_object' == resolve_object then tool
        else PropTypes ((head', tail'), resolve_object')
    | DefaultProps (tlist, default_props) ->
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        let default_props' = OptionUtils.ident_map (self#default_props cx) default_props in
        if tlist' == tlist && default_props' == default_props then tool
        else DefaultProps (tlist', default_props')
    | InitialState (tlist, initial_state) ->
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        let initial_state' = OptionUtils.ident_map (self#initial_state cx) initial_state in
        if tlist' == tlist && initial_state' == initial_state then tool
        else InitialState (tlist', initial_state')

  method create_class_knot cx t =
    let open React.CreateClass in
    let this' = self#type_ cx t.this in
    let static' = self#type_ cx t.static in
    let state_t' = self#type_ cx t.state_t in
    let default_t' = self#type_ cx t.default_t in
    if this' == t.this && static' == t.static && state_t' == t.state_t
      && default_t' == t.default_t
    then t
    else {this = this'; static = static'; state_t = state_t'; default_t = default_t'}

  method resolve cx t =
    let open ObjectSpread in
    match t with
    | Next -> t
    | List0 (tnelist, join) ->
        let tnelist' = Nel.ident_map (self#type_ cx) tnelist in
        if tnelist' == tnelist then t
        else List0 (tnelist', join)
    | List (tlist, resolvednelist, join) ->
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        let resolvednelist' = Nel.ident_map (self#resolved cx) resolvednelist in
        if tlist' == tlist && resolvednelist' == resolvednelist then t
        else List (tlist', resolvednelist', join)

  method resolved cx t =
    let t' = Nel.ident_map (fun ((r, props, dict, flags) as slice) ->
      let props' = SMap.ident_map (fun (x, b) -> (self#type_ cx x, b)) props in
      let dict' = OptionUtils.ident_map (self#dict_type cx) dict in
      if props' == props && dict' == dict then slice
      else (r, props', dict', flags)) t in
    if t' == t then t
    else t'

  method spec cx t =
    match t with
    | UnionCases (t', tlist) ->
        let t'' = self#type_ cx t' in
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        if t'' == t' && tlist' == tlist then t
        else UnionCases (t'', tlist')
    | IntersectionCases (tlist, use_t) ->
        let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
        let use_t' = self#use_type cx use_t in
        if tlist' == tlist && use_t' == use_t then t
        else IntersectionCases (tlist', use_t')

  method resolved_param cx t =
    match t with
    | ResolvedArg t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ResolvedArg t''
    | ResolvedSpreadArg (r, arrtype) ->
        let arrtype' = self#arr_type cx arrtype in
        if arrtype' == arrtype then t
        else ResolvedSpreadArg (r, arrtype')
    | ResolvedAnySpreadArg _ -> t


  method unresolved_param cx t =
    match t with
    | UnresolvedArg t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else UnresolvedArg t''
    | UnresolvedSpreadArg t' ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else UnresolvedSpreadArg t''

  method spread_resolve cx t =
    match t with
    | ResolveSpreadsToTuple (i, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ResolveSpreadsToTuple (i, t'')
    | ResolveSpreadsToArrayLiteral (i, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ResolveSpreadsToArrayLiteral (i, t'')
    | ResolveSpreadsToArray (i, t') ->
        let t'' = self#type_ cx t' in
        if t'' == t' then t
        else ResolveSpreadsToArray (i, t'')
    | ResolveSpreadsToMultiflowCallFull (i, funtype) ->
        let funtype' = self#fun_type cx funtype in
        if funtype' == funtype then t
        else ResolveSpreadsToMultiflowCallFull (i, funtype')
    | ResolveSpreadsToMultiflowSubtypeFull (i, funtype) ->
        let funtype' = self#fun_type cx funtype in
        if funtype' == funtype then t
        else ResolveSpreadsToMultiflowSubtypeFull (i, funtype')
    | ResolveSpreadsToMultiflowPartial (i, funtype, r, t') ->
        let funtype' = self#fun_type cx funtype in
        let t'' = self#type_ cx t' in
        if funtype' == funtype && t'' == t' then t
        else ResolveSpreadsToMultiflowPartial (i, funtype', r, t'')
    | ResolveSpreadsToCallT (funcalltype, t') ->
        let funcalltype' = self#fun_call_type cx funcalltype in
        let t'' = self#type_ cx t' in
        if funcalltype' == funcalltype && t'' == t' then t
        else ResolveSpreadsToCallT (funcalltype', t'')

  method resolve_array cx t =
    let open React in
    match t with
    | ResolveArray -> t
    | ResolveElem (tlist1, tlist2) ->
        let tlist1' = ListUtils.ident_map (self#type_ cx) tlist1 in
        let tlist2' = ListUtils.ident_map (self#type_ cx) tlist2 in
        if tlist1' == tlist1 && tlist2' == tlist2 then t
        else ResolveElem (tlist1', tlist2')

  method resolve_object cx t =
    let open React in
    match t with
    | ResolveObject -> t
    | ResolveDict (dict, props, obj) ->
        let dict' = self#dict_type cx dict in
        let props' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) props in
        let obj' = self#resolved_object cx obj in
        if dict' == dict && props' == props && obj' == obj then t
        else ResolveDict (dict', props', obj')
    | ResolveProp (s, props, obj) ->
        let props' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) props in
        let obj' = self#resolved_object cx obj in
        if props' == props && obj' == obj then t
        else ResolveProp (s, props', obj')

  method stack_tail cx tail = ListUtils.ident_map (self#stack_tail_elem cx) tail

  method stack_tail_elem cx ((head, tlist, maybespeclist) as t) =
    let head' = self#stack_head cx head in
    let tlist' = ListUtils.ident_map (self#type_ cx) tlist in
    let maybespeclist' =
      ListUtils.ident_map (maybe_known (self#create_class_spec cx)) maybespeclist in
    if head' == head && tlist' == tlist && maybespeclist' == maybespeclist then t
    else (head', tlist', maybespeclist')

  method create_class_spec cx t =
    let open React.CreateClass in
    let obj = self#resolved_object cx t.obj in
    let statics = OptionUtils.ident_map (maybe_known (self#resolved_object cx)) t.statics in
    let prop_types = OptionUtils.ident_map (maybe_known (self#resolved_object cx)) t.prop_types in
    let get_default_props = ListUtils.ident_map (self#type_ cx) t.get_default_props in
    let get_initial_state = ListUtils.ident_map (self#type_ cx) t.get_initial_state in
    if obj == t.obj && statics == t.statics && prop_types == t.prop_types
      && get_default_props == t.get_default_props && get_initial_state == t.get_initial_state
    then t
    else {obj; statics; prop_types; get_default_props; get_initial_state;
      unknown_mixins = t.unknown_mixins}

  method stack_head cx ((obj, spec) as t) =
    let obj' = self#resolved_object cx obj in
    let spec' = self#create_class_spec cx spec in
    if obj' == obj && spec' == spec then t
    else (obj', spec')

  method default_props cx default_props =
    maybe_known (self#resolved_object cx) default_props

  method resolved_object cx ((r, props, dictopt, flags) as t) =
    let props' = SMap.ident_map (Property.ident_map_t (self#type_ cx)) props in
    let dictopt' = OptionUtils.ident_map (self#dict_type cx) dictopt in
    if props' == props && dictopt' == dictopt then t
    else (r, props', dictopt', flags)

  method initial_state cx t =
    let open React.CreateClass in
    maybe_known (fun x -> match x with
    | NotNull obj ->
        let obj' = self#resolved_object cx obj in
        if obj' == obj then x
        else NotNull obj'
    | Null _ -> x) t
end
