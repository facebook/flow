(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

(* We walk types in a lot of places for all kinds of things, but often most of
   the code is boilerplate. The following visitor class for types aims to
   reduce that boilerplate. It is designed as a fold on the structure of types,
   parameterized by an accumulator.

   WARNING: This is only a partial implementation, sufficient for current
   purposes but intended to be completed in a later diff.
*)
class ['a] t = object(self)
  method type_ cx (acc: 'a) = function
  | OpenT (r, id) -> self#tvar cx acc r id

  | DefT (_, t) -> self#def_type cx acc t

  | ChoiceKitT (_, Trigger) -> acc

  | TypeDestructorTriggerT (_, d, t) ->
    let acc = self#destructor cx acc d in
    let acc = self#type_ cx acc t in
    acc

  | TaintT _
  | FunProtoT _
  | FunProtoApplyT _
  | FunProtoBindT _
  | FunProtoCallT _
  | ObjProtoT _
  | NullProtoT _
    -> acc

  | CustomFunT (_, kind) -> self#custom_fun_kind cx acc kind

  | EvalT (t, defer_use_t, id) ->
    let acc = self#type_ cx acc t in
    let acc = self#defer_use_type cx acc defer_use_t in
    let acc = self#eval_id cx acc id in
    acc

  | BoundT typeparam -> self#type_param cx acc typeparam

  | ExistsT _ -> acc

  | ExactT (_, t) -> self#type_ cx acc t

  | AnyWithLowerBoundT t
  | AnyWithUpperBoundT t -> self#type_ cx acc t

  | MergedT (_, uses) ->
    List.fold_left (self#use_type_ cx) acc uses

  | ShapeT t -> self#type_ cx acc t

  | DiffT (t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | MatchingPropT (_, _, t) -> self#type_ cx acc t

  | KeysT (_, t) -> self#type_ cx acc t

  | AnnotT (t, _) ->
    self#type_ cx acc t

  | OpaqueT (_, opaquetype) ->
    let acc = self#opt (self#type_ cx) acc opaquetype.underlying_t in
    let acc = self#opt (self#type_ cx) acc opaquetype.super_t in
    acc

  | ModuleT (_, exporttypes) ->
    self#export_types cx acc exporttypes

  | ExtendsT (_, ts, t1, t2) ->
    let acc = self#list (self#type_ cx) acc ts in
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | IdxWrapper (_, t) ->
    self#type_ cx acc t

  | OpenPredT (_ , t, p_map, n_map) ->
    let acc = self#type_ cx acc t in
    let acc = self#list (self#predicate cx) acc (Key_map.values p_map) in
    let acc = self#list (self#predicate cx) acc (Key_map.values n_map) in
    acc

  | ThisClassT (_, t) -> self#type_ cx acc t

  | ThisTypeAppT (_, t, this, ts_opt) ->
    let acc = self#type_ cx acc t in
    let acc = self#type_ cx acc this in
    let acc = self#opt (self#list (self#type_ cx)) acc ts_opt in
    acc

  | ReposT (_, t)
  | ReposUpperT (_, t) ->
    self#type_ cx acc t

  method def_type cx acc = function
  | AnyT
  | NumT _
  | StrT _
  | BoolT _
  | EmptyT
  | MixedT _
  | NullT
  | VoidT
  | AnyObjT
  | AnyFunT
    -> acc

  | FunT (static, prototype, funtype) ->
    let acc = self#type_ cx acc static in
    let acc = self#type_ cx acc prototype in
    let acc = self#fun_type cx acc funtype in
    acc

  | ObjT objtype -> self#obj_type cx acc objtype

  | ArrT (arrtype) -> self#arr_type cx acc arrtype

  | CharSetT _ -> acc

  | ClassT t -> self#type_ cx acc t

  | InstanceT (static, super, implements, insttype) ->
    let acc = self#type_ cx acc static in
    let acc = self#type_ cx acc super in
    let acc = self#list (self#type_ cx) acc implements in
    let acc = self#inst_type cx acc insttype in
    acc

  | SingletonStrT _
  | SingletonNumT _
  | SingletonBoolT _ -> acc

  | TypeT t -> self#type_ cx acc t

  | OptionalT t -> self#type_ cx acc t

  | PolyT (typeparams, t, _) ->
    let acc = self#list (self#type_param cx) acc typeparams in
    let acc = self#type_ cx acc t in
    acc

  | TypeAppT (t, ts) ->
    let acc = self#type_ cx acc t in
    let acc = self#list (self#type_ cx) acc ts in
    acc

  | MaybeT t -> self#type_ cx acc t

  | IntersectionT rep ->
    self#list (self#type_ cx) acc (InterRep.members rep)

  | UnionT rep ->
    self#list (self#type_ cx) acc (UnionRep.members rep)

  method private defer_use_type cx acc = function
  | DestructuringT (_, s) -> self#selector cx acc s
  | TypeDestructorT (_, d) -> self#destructor cx acc d

  method private selector cx acc = function
  | Prop _ -> acc
  | Elem key -> self#type_ cx acc key
  | ObjRest _ -> acc
  | ArrRest _ -> acc
  | Default -> acc
  | Become -> acc
  | Refine p -> self#predicate cx acc p

  method private predicate cx acc = function
  | AndP (p1, p2) -> self#list (self#predicate cx) acc [p1;p2]
  | OrP (p1, p2) -> self#list (self#predicate cx) acc [p1;p2]
  | NotP p -> self#predicate cx acc p
  | LeftP (_, t) -> self#type_ cx acc t
  | RightP (_, t) -> self#type_ cx acc t
  | ExistsP _ -> acc
  | NullP -> acc
  | MaybeP -> acc
  | SingletonBoolP _ -> acc
  | SingletonStrP _ -> acc
  | SingletonNumP _ -> acc
  | BoolP -> acc
  | FunP -> acc
  | NumP -> acc
  | ObjP -> acc
  | StrP -> acc
  | VoidP -> acc
  | ArrP -> acc
  | PropExistsP _ -> acc
  | LatentP (t, _) -> self#type_ cx acc t

  method private destructor cx acc = function
  | NonMaybeType
  | PropertyType _
  | ValuesType
  | ReactElementPropsType
  | ReactElementRefType
    -> acc
  | ElementType t -> self#type_ cx acc t
  | Bind t -> self#type_ cx acc t
  | SpreadType (_, ts) -> self#list (self#type_ cx) acc ts
  | RestType (_, t) -> self#type_ cx acc t
  | CallType args -> self#list (self#type_ cx) acc args
  | TypeMap (TupleMap t | ObjectMap t | ObjectMapi t) -> self#type_ cx acc t

  method private custom_fun_kind cx acc = function
  | ReactPropType (React.PropType.Primitive (_, t))
  | ReactElementFactory t
    -> self#type_ cx acc t
  | ObjectAssign
  | ObjectGetPrototypeOf
  | ObjectSetPrototypeOf
  | Compose _
  | ReactPropType _
  | ReactCreateClass
  | ReactCreateElement
  | ReactCloneElement
  | Merge
  | MergeDeepInto
  | MergeInto
  | Mixin
  | Idx
  | DebugPrint
    -> acc

  method use_type_ cx (acc: 'a) = function
  | UseT (_, t) ->
    self#type_ cx acc t

  | BindT (_, fn, _)
  | CallT (_, fn) ->
    self#fun_call_type cx acc fn

  | MethodT (_, _, p, fn) ->
    let acc = self#propref cx acc p in
    let acc = self#fun_call_type cx acc fn in
    acc

  | SetPropT (_, p, t)
  | GetPropT (_, p, t)
  | TestPropT (_, p, t) ->
    let acc = self#propref cx acc p in
    let acc = self#type_ cx acc t in
    acc

  | SetPrivatePropT (_, _, scopes, _, t)
  | GetPrivatePropT (_, _, scopes, _, t) ->
    let acc = List.fold_left (self#class_binding cx) acc scopes in
    let acc = self#type_ cx acc t in
    acc

  | SetElemT (_, e, t)
  | GetElemT (_, e, t) ->
    let acc = self#type_ cx acc e in
    let acc = self#type_ cx acc t in
    acc

  | CallElemT (_, _, t, fn) ->
    let acc = self#type_ cx acc t in
    let acc = self#fun_call_type cx acc fn in
    acc

  | GetStaticsT (_, t)
  | GetProtoT (_, t)
  | SetProtoT (_, t) ->
    self#type_ cx acc t

  | ReposLowerT (_, _, u) -> self#use_type_ cx acc u
  | ReposUseT (_, _, _, t) -> self#type_ cx acc t

  | ConstructorT (_, args, t) ->
    let acc = List.fold_left (self#call_arg cx) acc args in
    let acc = self#type_ cx acc t in
    acc

  | SuperT (_, DerivedInstance i) -> self#inst_type cx acc i
  | SuperT (_, DerivedStatics o) -> self#obj_type cx acc o
  | ImplementsT (_, t) -> self#type_ cx acc t
  | MixinT (_, t) -> self#type_ cx acc t

  | AdderT (_, a, b) ->
    let acc = self#type_ cx acc a in
    let acc = self#type_ cx acc b in
    acc

  | ComparatorT (_, t) -> self#type_ cx acc t
  | UnaryMinusT (_, t) -> self#type_ cx acc t

  | AssertArithmeticOperandT _
  | AssertBinaryInLHST _
  | AssertBinaryInRHST _
  | AssertForInRHST _
  | AssertRestParamT _ -> acc

  | PredicateT (predicate, t) ->
    let acc = self#predicate cx acc predicate in
    let acc = self#type_ cx acc t in
    acc

  | GuardT (predicate, t1, t2) ->
    let acc = self#predicate cx acc predicate in
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | EqT (_, t)
  | NotT (_, t) ->
    self#type_ cx acc t

  | AndT (_, a, b)
  | OrT (_, a, b) ->
    let acc = self#type_ cx acc a in
    let acc = self#type_ cx acc b in
    acc

  | SpecializeT (_, _, _, ts, t) ->
    let acc = self#opt (List.fold_left (self#type_ cx)) acc ts in
    let acc = self#type_ cx acc t in
    acc

  | ThisSpecializeT (_, t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | VarianceCheckT (_, ts, _) -> List.fold_left (self#type_ cx) acc ts

  | TypeAppVarianceCheckT (_, _, _, ts) ->
    List.fold_left (fun acc (a, b) ->
      let acc = self#type_ cx acc a in
      let acc = self#type_ cx acc b in
      acc
    ) acc ts

  | ConcretizeTypeAppsT (_, (ts1, _), (t2, ts2, _), _) ->
    let acc = List.fold_left (self#type_ cx) acc ts1 in
    let acc = self#type_ cx acc t2 in
    let acc = List.fold_left (self#type_ cx) acc ts2 in
    acc

  | LookupT (_, kind, ts, prop, action) ->
    let acc = self#lookup_kind cx acc kind in
    let acc = List.fold_left (self#type_ cx) acc ts in
    let acc = self#propref cx acc prop in
    let acc = self#lookup_action cx acc action in
    acc

  | ObjAssignToT (_, t1, t2, _)
  | ObjAssignFromT (_, t1, t2, _)
  | ObjTestT (_, t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | ObjTestProtoT (_, t) -> self#type_ cx acc t

  | ObjFreezeT (_, t)
  | ObjRestT (_, _, t)
  | ObjSealT (_, t)
  | ArrRestT (_, _, t) ->
    self#type_ cx acc t

  | UnifyT (t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | BecomeT (_, t) -> self#type_ cx acc t

  | GetKeysT (_, t)
  | GetValuesT (_, t) ->
    self#type_ cx acc t

  | HasOwnPropT _ -> acc

  | ElemT (_, t, action) ->
    let acc = self#type_ cx acc t in
    let acc = self#elem_action cx acc action in
    acc

  | MakeExactT (_, cont) -> self#cont cx acc cont

  | CJSRequireT (_, t)
  | ImportModuleNsT (_, t)
  | ImportDefaultT (_, _, _, t)
  | ImportNamedT (_, _, _, t)
  | ImportTypeT (_, _, t)
  | ImportTypeofT (_, _, t)
    -> self#type_ cx acc t

  | AssertImportIsValueT _ -> acc

  | CJSExtractNamedExportsT (_, (_, ts), t) ->
    let acc = self#export_types cx acc ts in
    let acc = self#type_ cx acc t in
    acc

  | CopyNamedExportsT (_, t, tout)
  | CopyTypeExportsT (_, t, tout)
  | ExportTypeT (_, _, _, t, tout) ->
    let acc = self#type_ cx acc t in
    let acc = self#type_ cx acc tout in
    acc

  | ExportNamedT (_, _, ts, tout) ->
    let acc = self#smap (self#type_ cx) acc ts in
    let acc = self#type_ cx acc tout in
    acc

  | MapTypeT (_, map, tout) ->
    let acc = self#type_map cx acc map in
    let acc = self#type_ cx acc tout in
    acc

  | ReactKitT (_, tool) -> (match tool with
    | React.GetProps t | React.GetRef t
      -> self#type_ cx acc t
    | React.CreateElement (_, t1, (ts, t2), t3) ->
      let acc = self#type_ cx acc t1 in
      let acc = List.fold_left (self#type_ cx) acc ts in
      let acc = self#opt (self#type_ cx) acc t2 in
      let acc = self#type_ cx acc t3 in
      acc
    | React.SimplifyPropType (tool, t) ->
      let open React in
      let open React.SimplifyPropType in
      let acc = match tool with
        | ArrayOf | InstanceOf | ObjectOf -> acc
        | OneOf r | OneOfType r -> (match r with
          | ResolveArray -> acc
          | ResolveElem (ts1, ts2) ->
            let acc = List.fold_left (self#type_ cx) acc ts1 in
            let acc = List.fold_left (self#type_ cx) acc ts2 in
            acc)
        | Shape o -> self#react_resolve_object cx acc o
      in
      let acc = self#type_ cx acc t in
      acc
    | React.CreateClass (tool, knot, tout) ->
      let acc = self#react_create_class_tool cx acc tool in
      let acc = self#react_create_class_knot cx acc knot in
      let acc = self#type_ cx acc tout in
      acc)

  | ObjKitT (_, resolve_tool, tool, tout) ->
    let open Object in
    let acc =
      match resolve_tool with
      | Resolve r -> self#object_kit_resolve cx acc r
      | Super (s, r) ->
        let acc = self#object_kit_slice cx acc s in
        let acc = self#object_kit_resolve cx acc r in
        acc
    in
    let acc = match tool with
      | Spread (_, state) ->
        let open Object.Spread in
        let { todo_rev; acc = object_spread_acc } = state in
        let acc = List.fold_left (self#type_ cx) acc todo_rev in
        let acc = List.fold_left
          (Nel.fold_left (self#object_kit_slice cx))
          acc object_spread_acc
        in
        acc
      | Rest (_, state) ->
        let open Object.Rest in
        (match state with
          | One t -> self#type_ cx acc t
          | Done o -> Nel.fold_left (self#object_kit_slice cx) acc o)
    in
    let acc = self#type_ cx acc tout in
    acc

  | DebugPrintT _ -> acc

  | SentinelPropTestT (_, t, _, _, _, tout) ->
    let acc = self#type_ cx acc t in
    let acc = self#type_ cx acc tout in
    acc

  | IdxUnwrap (_, tout)
  | IdxUnMaybeifyT (_, tout) ->
    self#type_ cx acc tout

  | CallLatentPredT (_, _, _, t1, t2)
  | CallOpenPredT (_, _, _, t1, t2) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc

  | SubstOnPredT (_, _, t) -> self#type_ cx acc t

  | RefineT (_, predicate, t) ->
    let acc = self#predicate cx acc predicate in
    let acc = self#type_ cx acc t in
    acc

  | ResolveSpreadT (_, { rrt_resolved; rrt_unresolved; rrt_resolve_to }) ->
    let acc = List.fold_left (fun (acc: 'a) -> function
      | ResolvedArg t -> self#type_ cx acc t
      | ResolvedAnySpreadArg _ -> acc
      | ResolvedSpreadArg (_, arr) -> self#arr_type cx acc arr
    ) acc rrt_resolved in
    let acc = List.fold_left (fun acc -> function
      | UnresolvedArg t
      | UnresolvedSpreadArg t ->
        self#type_ cx acc t
    ) acc rrt_unresolved in
    let acc = match rrt_resolve_to with
      | ResolveSpreadsToTuple (_, t)
      | ResolveSpreadsToArrayLiteral (_, t)
      | ResolveSpreadsToArray (_, t)
        -> self#type_ cx acc t
      | ResolveSpreadsToMultiflowCallFull (_, fn)
      | ResolveSpreadsToMultiflowSubtypeFull (_, fn)
        -> self#fun_type cx acc fn
      | ResolveSpreadsToCustomFunCall (_, kind, t) ->
        let acc = self#custom_fun_kind cx acc kind in
        let acc = self#type_ cx acc t in
        acc
      | ResolveSpreadsToMultiflowPartial (_, fn, _, t) ->
        let acc = self#fun_type cx acc fn in
        let acc = self#type_ cx acc t in
        acc
      | ResolveSpreadsToCallT (fn, t) ->
        let acc = self#fun_call_type cx acc fn in
        let acc = self#type_ cx acc t in
        acc
    in
    acc

  | CondT (_, t, tout) ->
    let acc = self#type_ cx acc t in
    let acc = self#type_ cx acc tout in
    acc

  | ChoiceKitUseT (_, tool) -> (match tool with
    | FullyResolveType _ -> acc
    | TryFlow (_, spec) -> (match spec with
      | UnionCases (_, t, ts) ->
        let acc = self#type_ cx acc t in
        let acc = List.fold_left (self#type_ cx) acc ts in
        acc
      | IntersectionCases (ts, use) ->
        let acc = List.fold_left (self#type_ cx) acc ts in
        let acc = self#use_type_ cx acc use in
        acc)
    | EvalDestructor (_, d, tout) ->
      let acc = self#destructor cx acc d in
      let acc = self#type_ cx acc tout in
      acc)

  | IntersectionPreprocessKitT (_, tool) -> (match tool with
    | ConcretizeTypes (ts1, ts2, t, use) ->
      let acc = List.fold_left (self#type_ cx) acc ts1 in
      let acc = List.fold_left (self#type_ cx) acc ts2 in
      let acc = self#type_ cx acc t in
      let acc = self#use_type_ cx acc use in
      acc
    | SentinelPropTest (_, _, t1, t2, t3) ->
      let acc = self#type_ cx acc t1 in
      let acc = self#type_ cx acc t2 in
      let acc = self#type_ cx acc t3 in
      acc
    | PropExistsTest (_, _, t1, t2) ->
      let acc = self#type_ cx acc t1 in
      let acc = self#type_ cx acc t2 in
      acc)

  (* The default behavior here could be fleshed out a bit, to look up the graph,
     handle Resolved and Unresolved cases, etc. *)
  method tvar _cx acc _r _id = acc

  method dict_type cx acc { key; value; _ } =
    let acc = self#type_ cx acc key in
    let acc = self#type_ cx acc value in
    acc

  method props cx acc id =
    Context.find_props cx id
    |> self#smap (self#prop cx) acc

  method private prop cx acc p =
    Property.fold_t (self#type_ cx) acc p

  method exports cx acc id =
    Context.find_exports cx id
    |> self#smap (self#type_ cx) acc

  method eval_id cx acc id =
    match IMap.get id (Context.evaluated cx) with
    | None -> acc
    | Some t -> self#type_ cx acc t

  method private type_param cx acc { bound; default; _ } =
    let acc = self#type_ cx acc bound in
    self#opt (self#type_ cx) acc default

  method fun_type cx acc { this_t; params_tlist; rest_param; return_t; _ } =
    let acc = self#type_ cx acc this_t in
    let acc = self#list (self#type_ cx) acc params_tlist in
    let acc = self#opt (fun acc (_, _, t) -> self#type_ cx acc t) acc rest_param in
    let acc = self#type_ cx acc return_t in
    acc

  method private obj_type cx acc { dict_t; props_tmap; proto_t; _ } =
    let acc = self#opt (self#dict_type cx) acc dict_t in
    let acc = self#props cx acc props_tmap in
    let acc = self#type_ cx acc proto_t in
    acc

  method private arr_type cx acc = function
  | ArrayAT (elemt, None)
  | ROArrayAT (elemt) ->
    self#type_ cx acc elemt
  | ArrayAT (elemt, Some tuple_types)
  | TupleAT (elemt, tuple_types) ->
    let acc = self#type_ cx acc elemt in
    let acc = self#list (self#type_ cx) acc tuple_types in
    acc
  | EmptyAT -> acc

  method private inst_type cx acc { type_args; fields_tmap; methods_tmap; _ } =
    let acc = self#smap (self#type_ cx) acc type_args in
    let acc = self#props cx acc fields_tmap in
    let acc = self#props cx acc methods_tmap in
    acc

  method private export_types cx acc { exports_tmap; cjs_export; has_every_named_export=_; } =
    let acc = self#exports cx acc exports_tmap in
    let acc = self#opt (self#type_ cx) acc cjs_export in
    acc

  method private fun_call_type cx acc { call_this_t; call_args_tlist; call_tout; _ } =
    let acc = self#type_ cx acc call_this_t in
    let acc = List.fold_left (self#call_arg cx) acc call_args_tlist in
    let acc = self#type_ cx acc call_tout in
    acc

  method private propref cx acc = function
  | Named _ -> acc
  | Computed t -> self#type_ cx acc t

  method private class_binding cx acc { class_private_fields; class_private_static_fields; _ } =
    let acc = self#props cx acc class_private_fields in
    let acc = self#props cx acc class_private_static_fields in
    acc

  method private call_arg cx acc = function
  | Arg t -> self#type_ cx acc t
  | SpreadArg t -> self#type_ cx acc t

  method private lookup_kind cx acc = function
  | Strict _ -> acc
  | NonstrictReturning (Some (t1, t2)) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc
  | NonstrictReturning None -> acc
  | ShadowRead (_, props)
  | ShadowWrite props ->
    Nel.fold_left (self#props cx) acc props

  method private lookup_action cx acc = function
  | RWProp (t1, t2, _) ->
    let acc = self#type_ cx acc t1 in
    let acc = self#type_ cx acc t2 in
    acc
  | LookupProp (_, prop)
  | SuperProp prop ->
    self#prop cx acc prop
  | MatchProp t ->
    self#type_ cx acc t

  method private elem_action cx acc = function
  | ReadElem t
  | WriteElem t
    -> self#type_ cx acc t
  | CallElem (_, fn) -> self#fun_call_type cx acc fn

  method private cont cx acc = function
  | Lower (_, l) -> self#type_ cx acc l
  | Upper u -> self#use_type_ cx acc u

  method private type_map cx acc = function
  | TupleMap t
  | ObjectMap t
  | ObjectMapi t -> self#type_ cx acc t

  method private object_kit_resolve cx acc =
    let open Object in
    function
    | Next -> acc
    | List0 (ts, _) -> Nel.fold_left (self#type_ cx) acc ts
    | List (ts, rs, _) ->
      let acc = List.fold_left (self#type_ cx) acc ts in
      let acc = Nel.fold_left (Nel.fold_left (self#object_kit_slice cx)) acc rs in
      acc

  method private object_kit_slice cx acc (_, props, dict, _) =
    let acc = self#smap (fun acc (t, _) -> self#type_ cx acc t) acc props in
    let acc = self#opt (self#dict_type cx) acc dict in
    acc

  method private react_resolved_object cx acc (_, props, dict, _) =
    let acc = self#smap (self#prop cx) acc props in
    let acc = self#opt (self#dict_type cx) acc dict in
    acc

  method private react_resolve_object cx acc o =
    let open React in
    match o with
      | ResolveObject -> acc
      | ResolveDict (dict, props, o) ->
        let acc = self#dict_type cx acc dict in
        let acc = self#smap (self#prop cx) acc props in
        let acc = self#react_resolved_object cx acc o in
        acc
      | ResolveProp (_, props, o) ->
        let acc = self#smap (self#prop cx) acc props in
        let acc = self#react_resolved_object cx acc o in
        acc

  method private react_create_class_tool cx acc tool =
    let open React.CreateClass in
    match tool with
      | Spec tail -> self#react_create_class_stack_tail cx acc tail
      | Mixins stack -> self#react_create_class_stack cx acc stack
      | Statics stack -> self#react_create_class_stack cx acc stack
      | PropTypes (stack, o) ->
        let acc = self#react_create_class_stack cx acc stack in
        let acc = self#react_resolve_object cx acc o in
        acc
      | DefaultProps (ts, dp) ->
        let acc = List.fold_left (self#type_ cx) acc ts in
        let acc = self#opt (self#maybe_known (self#react_resolved_object cx)) acc dp in
        acc
      | InitialState (ts, s) ->
        let acc = List.fold_left (self#type_ cx) acc ts in
        let acc = self#opt (self#maybe_known (self#or_null
          (self#react_resolved_object cx))) acc s in
        acc

  method private react_create_class_stack cx acc (head, tail) =
    let acc = self#react_create_class_stack_head cx acc head in
    let acc = self#react_create_class_stack_tail cx acc tail in
    acc

  method private react_create_class_stack_head cx acc (o, spec) =
    let acc = self#react_resolved_object cx acc o in
    let acc = self#react_create_class_spec cx acc spec in
    acc

  method private react_create_class_stack_tail cx acc =
    List.fold_left (fun acc (head, ts, specs) ->
      let acc = self#react_create_class_stack_head cx acc head in
      let acc = List.fold_left (self#type_ cx) acc ts in
      let acc = List.fold_left (self#maybe_known
        (self#react_create_class_spec cx)) acc specs in
      acc
    ) acc

  method private react_create_class_spec cx acc spec =
    let open React.CreateClass in
    let { obj; statics; prop_types; get_default_props; get_initial_state; _ } = spec in
    let acc = self#react_resolved_object cx acc obj in
    let acc = self#opt (self#maybe_known (self#react_resolved_object cx)) acc statics in
    let acc = self#opt (self#maybe_known (self#react_resolved_object cx)) acc prop_types in
    let acc = List.fold_left (self#type_ cx) acc get_default_props in
    let acc = List.fold_left (self#type_ cx) acc get_initial_state in
    acc

  method private maybe_known: 't. ('a -> 't -> 'a) -> 'a -> 't React.CreateClass.maybe_known -> 'a =
    let open React.CreateClass in
    fun f acc x -> match x with
      | Known a -> f acc a
      | Unknown _ -> acc

  method private or_null: 't. ('a -> 't -> 'a) -> 'a -> 't React.CreateClass.or_null -> 'a =
    let open React.CreateClass in
    fun f acc x -> match x with
      | NotNull a -> f acc a
      | Null _ -> acc

  method private react_create_class_knot cx acc knot =
    let open React.CreateClass in
    let { this; static; state_t; default_t } = knot in
    let acc = self#type_ cx acc this in
    let acc = self#type_ cx acc static in
    let acc = self#type_ cx acc state_t in
    let acc = self#type_ cx acc default_t in
    acc

  method private list: 't. ('a -> 't -> 'a) -> 'a -> 't list -> 'a =
    List.fold_left

  method private opt: 't. ('a -> 't -> 'a) -> 'a -> 't option -> 'a =
    fun f acc opt -> Option.fold opt ~init:acc ~f

  method private smap: 't. ('a -> 't -> 'a) -> 'a -> 't SMap.t -> 'a =
    fun f acc smap -> SMap.fold (fun _ t acc -> f acc t) smap acc
end
