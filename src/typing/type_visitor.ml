(*
 * Copyright (c) Facebook, Inc. and its affiliates.
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
      | TypeDestructorTriggerT (_, _, _, d, t) ->
        let acc = self#destructor cx acc d in
        let acc = self#type_ cx pole acc t in
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
      | BoundT _ -> acc
      | ExistsT _ -> acc
      | ExactT (_, t) -> self#type_ cx pole acc t
      | MergedT (_, uses) -> List.fold_left (self#use_type_ cx) acc uses
      | ShapeT t -> self#type_ cx pole acc t
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
      | ThisClassT (_, t) -> self#type_ cx pole acc t
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
      | ReposT (_, t)
      | InternalT (ReposUpperT (_, t)) ->
        self#type_ cx pole acc t
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
      | EmptyT _
      | MixedT _
      | SymbolT
      | NullT
      | VoidT ->
        acc
      | EnumT enum
      | EnumObjectT enum ->
        let { enum_id = _; enum_name = _; members = _; representation_t } = enum in
        let acc = self#type_ cx pole acc representation_t in
        acc
      | FunT (static, prototype, funtype) ->
        let acc = self#type_ cx pole acc static in
        let acc = self#type_ cx pole_TODO acc prototype in
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
      | SingletonBoolT _ ->
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
      | SymbolP -> acc
      | VoidP -> acc
      | ArrP -> acc
      | PropExistsP _ -> acc
      | PropNonMaybeP _ -> acc
      | LatentP (t, _) -> self#type_ cx P.Positive acc t

    method destructor cx acc =
      function
      | NonMaybeType
      | PropertyType _
      | ValuesType
      | ReadOnlyType
      | ReactElementPropsType
      | ReactElementConfigType
      | ReactElementRefType ->
        acc
      | ReactConfigType default_props -> self#type_ cx pole_TODO acc default_props
      | ElementType t -> self#type_ cx pole_TODO acc t
      | Bind t -> self#type_ cx pole_TODO acc t
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
        acc

    method use_type_ cx (acc : 'a) =
      function
      | UseT (_, t) -> self#type_ cx P.Negative acc t
      | BindT (_, _, fn, _)
      | CallT (_, _, fn) ->
        self#fun_call_type cx acc fn
      | MethodT (_, _, _, p, fn, prop_t) ->
        let acc = self#propref cx acc p in
        let acc = self#method_action cx acc fn in
        let acc = self#opt (self#type_ cx pole_TODO) acc prop_t in
        acc
      | SetPropT (_, _, p, _, _, t, prop_t) ->
        let acc = self#propref cx acc p in
        let acc = self#type_ cx pole_TODO acc t in
        let acc = self#opt (self#type_ cx pole_TODO) acc prop_t in
        acc
      | GetPropT (_, _, p, t)
      | MatchPropT (_, _, p, t)
      | TestPropT (_, _, p, t) ->
        let acc = self#propref cx acc p in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | SetPrivatePropT (_, _, _, _, scopes, _, t, prop_t) ->
        let acc = List.fold_left (self#class_binding cx) acc scopes in
        let acc = self#type_ cx pole_TODO acc t in
        let acc = self#opt (self#type_ cx pole_TODO) acc prop_t in
        acc
      | GetPrivatePropT (_, _, _, scopes, _, t) ->
        let acc = List.fold_left (self#class_binding cx) acc scopes in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | SetElemT (_, _, e, _, tin, tout) ->
        let acc = self#type_ cx pole_TODO acc e in
        let acc = self#type_ cx pole_TODO acc tin in
        let acc = self#opt (self#type_ cx pole_TODO) acc tout in
        acc
      | GetElemT (_, _, e, t) ->
        let acc = self#type_ cx pole_TODO acc e in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | CallElemT (_, _, t, fn) ->
        let acc = self#type_ cx pole_TODO acc t in
        let acc = self#method_action cx acc fn in
        acc
      | GetStaticsT (_, t)
      | GetProtoT (_, t)
      | SetProtoT (_, t) ->
        self#type_ cx pole_TODO acc t
      | ReposLowerT (_, _, u) -> self#use_type_ cx acc u
      | ReposUseT (_, _, _, t) -> self#type_ cx pole_TODO acc t
      | ConstructorT (_, _, targs, args, t) ->
        let acc = Option.fold ~init:acc ~f:(List.fold_left (self#targ cx pole_TODO)) targs in
        let acc = List.fold_left (self#call_arg cx) acc args in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | SuperT (_, _, Derived { own; proto; static }) ->
        let acc = self#smap (self#prop cx pole_TODO) acc own in
        let acc = self#smap (self#prop cx pole_TODO) acc proto in
        let acc = self#smap (self#prop cx pole_TODO) acc static in
        acc
      | ImplementsT (_, t) -> self#type_ cx pole_TODO acc t
      | MixinT (_, t) -> self#type_ cx pole_TODO acc t
      | ToStringT (_, t) -> self#use_type_ cx acc t
      | AdderT (_, _, _, a, b) ->
        let acc = self#type_ cx pole_TODO acc a in
        let acc = self#type_ cx pole_TODO acc b in
        acc
      | ComparatorT (_, _, t) -> self#type_ cx pole_TODO acc t
      | UnaryMinusT (_, t) -> self#type_ cx pole_TODO acc t
      | AssertArithmeticOperandT _
      | AssertBinaryInLHST _
      | AssertBinaryInRHST _
      | AssertForInRHST _ ->
        acc
      | PredicateT (predicate, t) ->
        let acc = self#predicate cx acc predicate in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | GuardT (predicate, t1, t2) ->
        let acc = self#predicate cx acc predicate in
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | StrictEqT { arg; _ } -> self#type_ cx pole_TODO acc arg
      | EqT (_, _, t)
      | NotT (_, t) ->
        self#type_ cx pole_TODO acc t
      | AndT (_, a, b)
      | OrT (_, a, b)
      | NullishCoalesceT (_, a, b) ->
        let acc = self#type_ cx pole_TODO acc a in
        let acc = self#type_ cx pole_TODO acc b in
        acc
      | SpecializeT (_, _, _, _, ts, t) ->
        let acc = self#opt (List.fold_left (self#type_ cx pole_TODO)) acc ts in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | ThisSpecializeT (_, this, k) ->
        let acc = self#type_ cx pole_TODO acc this in
        let acc = self#cont cx acc k in
        acc
      | VarianceCheckT (_, tparams, targs, _) ->
        let acc = self#smap (self#type_param cx pole_TODO) acc tparams in
        let acc = List.fold_left (self#type_ cx pole_TODO) acc targs in
        acc
      | TypeAppVarianceCheckT (_, _, _, ts) ->
        List.fold_left
          (fun acc (a, b) ->
            let acc = self#type_ cx pole_TODO acc a in
            let acc = self#type_ cx pole_TODO acc b in
            acc)
          acc
          ts
      | TypeCastT (_, t) -> self#type_ cx pole_TODO acc t
      | EnumCastT { enum = (_, _, { representation_t; _ }); _ } ->
        self#type_ cx pole_TODO acc representation_t
      | FilterOptionalT (_, t) -> self#type_ cx pole_TODO acc t
      | FilterMaybeT (_, t) -> self#type_ cx pole_TODO acc t
      | ConcretizeTypeAppsT (_, (ts1, _, _), (t2, ts2, _, _), _) ->
        let acc = List.fold_left (self#type_ cx pole_TODO) acc ts1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        let acc = List.fold_left (self#type_ cx pole_TODO) acc ts2 in
        acc
      | LookupT (_, kind, ts, prop, action) ->
        let acc = self#lookup_kind cx acc kind in
        let acc = List.fold_left (self#type_ cx pole_TODO) acc ts in
        let acc = self#propref cx acc prop in
        let acc = self#lookup_action cx acc action in
        acc
      | ObjAssignToT (_, _, t1, t2, _)
      | ObjAssignFromT (_, _, t1, t2, _)
      | ObjTestT (_, t1, t2) ->
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | ObjTestProtoT (_, t) -> self#type_ cx pole_TODO acc t
      | ObjFreezeT (_, t)
      | ObjRestT (_, _, t)
      | ObjSealT (_, t)
      | ArrRestT (_, _, _, t) ->
        self#type_ cx pole_TODO acc t
      | UnifyT (t1, t2) ->
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | BecomeT (_, t) -> self#type_ cx pole_TODO acc t
      | GetKeysT (_, t) -> self#use_type_ cx acc t
      | GetValuesT (_, t) -> self#type_ cx pole_TODO acc t
      | HasOwnPropT _ -> acc
      | ElemT (_, _, t, action) ->
        let acc = self#type_ cx pole_TODO acc t in
        let acc = self#elem_action cx acc action in
        acc
      | MakeExactT (_, cont) -> self#cont cx acc cont
      | CJSRequireT (_, t, _)
      | ImportModuleNsT (_, t, _)
      | ImportDefaultT (_, _, _, t, _)
      | ImportNamedT (_, _, _, _, t, _)
      | ImportTypeT (_, _, t)
      | ImportTypeofT (_, _, t) ->
        self#type_ cx P.Negative acc t
      | AssertImportIsValueT _ -> acc
      | CJSExtractNamedExportsT (_, (_, ts, _), t) ->
        let acc = self#export_types cx pole_TODO acc ts in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | CopyNamedExportsT (_, t, tout)
      | CopyTypeExportsT (_, t, tout)
      | ExportTypeT (_, _, t, tout) ->
        let acc = self#type_ cx pole_TODO acc t in
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | AssertExportIsTypeT (_, _, tout) ->
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | ExportNamedT (_, ts, _, tout) ->
        let visit_pair acc (_loc, t) = self#type_ cx pole_TODO acc t in
        let acc = self#smap visit_pair acc ts in
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | MapTypeT (_, _, map, tout) ->
        let acc = self#type_map cx acc map in
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | ReactKitT (_, _, tool) ->
        (match tool with
        | React.GetProps t
        | React.GetConfig t
        | React.GetRef t ->
          self#type_ cx pole_TODO acc t
        | React.GetConfigType (default_props, t) ->
          let acc = self#type_ cx pole_TODO acc default_props in
          self#type_ cx pole_TODO acc t
        | React.CreateElement0 (_, config, (children, children_spread), tout) ->
          let acc = self#type_ cx pole_TODO acc config in
          let acc = List.fold_left (self#type_ cx pole_TODO) acc children in
          let acc = self#opt (self#type_ cx pole_TODO) acc children_spread in
          let acc = self#type_ cx pole_TODO acc tout in
          acc
        | React.CreateElement (_, component, config, (children, children_spread), tout) ->
          let acc = self#type_ cx pole_TODO acc component in
          let acc = self#type_ cx pole_TODO acc config in
          let acc = List.fold_left (self#type_ cx pole_TODO) acc children in
          let acc = self#opt (self#type_ cx pole_TODO) acc children_spread in
          let acc = self#type_ cx pole_TODO acc tout in
          acc
        | React.ConfigCheck config -> self#type_ cx pole_TODO acc config
        | React.SimplifyPropType (tool, t) ->
          React.(
            React.SimplifyPropType.(
              let acc =
                match tool with
                | ArrayOf
                | InstanceOf
                | ObjectOf ->
                  acc
                | OneOf r
                | OneOfType r ->
                  (match r with
                  | ResolveArray -> acc
                  | ResolveElem (ts1, ts2) ->
                    let acc = List.fold_left (self#type_ cx pole_TODO) acc ts1 in
                    let acc = List.fold_left (self#type_ cx pole_TODO) acc ts2 in
                    acc)
                | Shape o -> self#react_resolve_object cx acc o
              in
              let acc = self#type_ cx pole_TODO acc t in
              acc))
        | React.CreateClass (tool, knot, tout) ->
          let acc = self#react_create_class_tool cx acc tool in
          let acc = self#react_create_class_knot cx acc knot in
          let acc = self#type_ cx pole_TODO acc tout in
          acc)
      | ObjKitT (_, _, resolve_tool, tool, tout) ->
        Object.(
          let acc =
            match resolve_tool with
            | Resolve r -> self#object_kit_resolve cx acc r
            | Super (s, r) ->
              let acc = self#object_kit_slice cx acc s in
              let acc = self#object_kit_resolve cx acc r in
              acc
          in
          let acc =
            match tool with
            | ReadOnly -> acc
            | ObjectRep -> acc
            | ObjectWiden _ -> acc
            | Spread (_, state) ->
              Object.Spread.(
                let {
                  todo_rev;
                  acc = object_spread_acc;
                  spread_id = _;
                  union_reason = _;
                  curr_resolve_idx = _;
                } =
                  state
                in
                let acc = List.fold_left (self#object_kit_spread_operand cx) acc todo_rev in
                let acc = List.fold_left (self#object_kit_acc_element cx) acc object_spread_acc in
                acc)
            | Rest (_, state) ->
              Object.Rest.(
                (match state with
                | One t -> self#type_ cx pole_TODO acc t
                | Done o -> Nel.fold_left (self#object_kit_slice cx) acc o))
            | ReactConfig state ->
              Object.ReactConfig.(
                (match state with
                | Config { defaults; children } ->
                  let acc = self#opt (self#type_ cx pole_TODO) acc defaults in
                  let acc = self#opt (self#type_ cx pole_TODO) acc children in
                  acc
                | Defaults { config; children } ->
                  let acc = Nel.fold_left (self#object_kit_slice cx) acc config in
                  let acc = self#opt (self#type_ cx pole_TODO) acc children in
                  acc))
          in
          let acc = self#type_ cx pole_TODO acc tout in
          acc)
      | DebugPrintT _ -> acc
      | DebugSleepT _ -> acc
      | SentinelPropTestT (_, t, _, _, _, tout) ->
        let acc = self#type_ cx pole_TODO acc t in
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | IdxUnwrap (_, tout)
      | IdxUnMaybeifyT (_, tout) ->
        self#type_ cx pole_TODO acc tout
      | OptionalChainT (_, _, this, tout, void_out) ->
        let acc = self#type_ cx pole_TODO acc this in
        let acc = self#use_type_ cx acc tout in
        self#type_ cx pole_TODO acc void_out
      | InvariantT _ -> acc
      | CallLatentPredT (_, _, _, t1, t2)
      | CallOpenPredT (_, _, _, t1, t2) ->
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | SubstOnPredT (_, _, t) -> self#type_ cx pole_TODO acc t
      | RefineT (_, predicate, t) ->
        let acc = self#predicate cx acc predicate in
        let acc = self#type_ cx pole_TODO acc t in
        acc
      | ReactPropsToOut (_, t)
      | ReactInToProps (_, t) ->
        self#type_ cx pole_TODO acc t
      | ResolveSpreadT (_, _, { rrt_resolved; rrt_unresolved; rrt_resolve_to }) ->
        let acc =
          List.fold_left
            (fun (acc : 'a) -> function
              | ResolvedArg t -> self#type_ cx pole_TODO acc t
              | ResolvedAnySpreadArg _ -> acc
              | ResolvedSpreadArg (_, arr) -> self#arr_type cx pole_TODO acc arr)
            acc
            rrt_resolved
        in
        let acc =
          List.fold_left
            (fun acc -> function
              | UnresolvedArg t
              | UnresolvedSpreadArg t ->
                self#type_ cx pole_TODO acc t)
            acc
            rrt_unresolved
        in
        let acc =
          match rrt_resolve_to with
          | ResolveSpreadsToArrayLiteral (_, t1, t2)
          | ResolveSpreadsToArray (t1, t2) ->
            let acc = self#type_ cx pole_TODO acc t1 in
            let acc = self#type_ cx pole_TODO acc t2 in
            acc
          | ResolveSpreadsToMultiflowCallFull (_, fn)
          | ResolveSpreadsToMultiflowSubtypeFull (_, fn) ->
            self#fun_type cx pole_TODO acc fn
          | ResolveSpreadsToCustomFunCall (_, kind, t) ->
            let acc = self#custom_fun_kind cx acc kind in
            let acc = self#type_ cx pole_TODO acc t in
            acc
          | ResolveSpreadsToMultiflowPartial (_, fn, _, t) ->
            let acc = self#fun_type cx pole_TODO acc fn in
            let acc = self#type_ cx pole_TODO acc t in
            acc
          | ResolveSpreadsToCallT (fn, t) ->
            let acc = self#fun_call_type cx acc fn in
            let acc = self#type_ cx pole_TODO acc t in
            acc
        in
        acc
      | CondT (_, then_t_opt, else_t, tout) ->
        let acc = self#opt (self#type_ cx pole_TODO) acc then_t_opt in
        let acc = self#type_ cx pole_TODO acc else_t in
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | ChoiceKitUseT (_, tool) -> self#choice_use_tool cx acc tool
      | ExtendsUseT (_, _, ts, t1, t2) ->
        let acc = self#list (self#type_ cx pole_TODO) acc ts in
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | IntersectionPreprocessKitT (_, tool) ->
        (match tool with
        | ConcretizeTypes (ts1, ts2, t, use) ->
          let acc = List.fold_left (self#type_ cx pole_TODO) acc ts1 in
          let acc = List.fold_left (self#type_ cx pole_TODO) acc ts2 in
          let acc = self#type_ cx pole_TODO acc t in
          let acc = self#use_type_ cx acc use in
          acc
        | SentinelPropTest (_, _, t1, t2, t3) ->
          let acc = self#type_ cx pole_TODO acc t1 in
          let acc = self#type_ cx pole_TODO acc t2 in
          let acc = self#type_ cx pole_TODO acc t3 in
          acc
        | PropExistsTest (_, _, _, t1, t2, (pred, not_pred)) ->
          let acc = self#type_ cx pole_TODO acc t1 in
          let acc = self#type_ cx pole_TODO acc t2 in
          let acc = self#predicate cx acc pred in
          let acc = self#predicate cx acc not_pred in
          acc)
      | DestructuringT (_, _, s, tout) ->
        let acc = self#selector cx acc s in
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | CreateObjWithComputedPropT { reason = _; value; tout_tvar = (r, id) } ->
        let acc = self#type_ cx pole_TODO acc value in
        let acc = self#tvar cx pole_TODO acc r id in
        acc
      | ModuleExportsAssignT (_, t, tout) ->
        let acc = self#type_ cx pole_TODO acc t in
        let acc = self#type_ cx pole_TODO acc tout in
        acc
      | ResolveUnionT { reason = _; resolved; unresolved; upper; id = _ } ->
        let acc = List.fold_left (self#type_ cx pole_TODO) acc resolved in
        let acc = List.fold_left (self#type_ cx pole_TODO) acc unresolved in
        let acc = self#use_type_ cx acc upper in
        acc

    (* The default behavior here could be fleshed out a bit, to look up the graph,
     handle Resolved and Unresolved cases, etc. *)
    method tvar _cx _pole acc _r _id = acc

    method dict_type cx pole acc d =
      let { dict_name = _; key; value; dict_polarity = p } = d in
      let acc = self#type_ cx pole_TODO acc key in
      let acc = self#type_ cx (P.mult (pole, p)) acc value in
      acc

    method props cx pole acc id = Context.find_props cx id |> self#smap (self#prop cx pole) acc

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
      Context.find_exports cx id |> self#smap visit_pair acc

    method eval_id cx pole acc id =
      match Eval.Map.find_opt id (Context.evaluated cx) with
      | None -> acc
      | Some t -> self#type_ cx pole acc t

    method private type_param cx pole acc tp =
      let { reason = _; name = _; bound; default; polarity = p } = tp in
      let pole = P.mult (pole, p) in
      let acc = self#type_ cx pole acc bound in
      self#opt (self#type_ cx pole) acc default

    method fun_type cx pole acc ft =
      let {
        this_t;
        params;
        rest_param;
        return_t;
        closure_t = _;
        is_predicate = _;
        changeset = _;
        def_reason = _;
      } =
        ft
      in
      let acc = self#type_ cx pole acc this_t in
      let acc = self#list (fun acc (_, t) -> self#type_ cx (P.inv pole) acc t) acc params in
      let acc = self#opt (fun acc (_, _, t) -> self#type_ cx (P.inv pole) acc t) acc rest_param in
      let acc = self#type_ cx pole acc return_t in
      acc

    method private obj_type cx pole acc o =
      let { dict_t; props_tmap; proto_t; call_t; flags = _ } = o in
      let acc = self#opt (self#dict_type cx pole) acc dict_t in
      let acc = self#props cx pole acc props_tmap in
      let acc = self#type_ cx pole acc proto_t in
      let acc = self#opt (self#call_prop cx pole) acc call_t in
      acc

    method private arr_type cx pole acc =
      function
      | ArrayAT (t, None) -> self#type_ cx P.Neutral acc t
      | ArrayAT (t, Some ts)
      | TupleAT (t, ts) ->
        let acc = self#type_ cx P.Neutral acc t in
        let acc = self#list (self#type_ cx P.Neutral) acc ts in
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

    method private fun_call_type cx acc call =
      let {
        call_this_t;
        call_targs;
        call_args_tlist;
        call_tout;
        call_closure_t = _;
        call_strict_arity = _;
      } =
        call
      in
      let acc = self#type_ cx pole_TODO acc call_this_t in
      let acc = self#opt (self#list (self#targ cx pole_TODO)) acc call_targs in
      let acc = self#list (self#call_arg cx) acc call_args_tlist in
      let acc = self#type_ cx pole_TODO acc call_tout in
      acc

    method private opt_fun_call_type cx acc (call_this_t, call_targs, call_args_tlist, _, _) =
      let acc = self#type_ cx pole_TODO acc call_this_t in
      let acc = self#opt (self#list (self#targ cx pole_TODO)) acc call_targs in
      let acc = self#list (self#call_arg cx) acc call_args_tlist in
      acc

    method private method_action cx acc =
      function
      | CallM call -> self#fun_call_type cx acc call
      | ChainM (_, _, this, call, void_out) ->
        let acc = self#type_ cx pole_TODO acc this in
        let acc = self#fun_call_type cx acc call in
        self#type_ cx pole_TODO acc void_out

    method private opt_method_action cx acc =
      function
      | OptCallM call -> self#opt_fun_call_type cx acc call
      | OptChainM (_, _, this, call, void_out) ->
        let acc = self#type_ cx pole_TODO acc this in
        let acc = self#opt_fun_call_type cx acc call in
        self#type_ cx pole_TODO acc void_out

    method private propref cx acc =
      function
      | Named _ -> acc
      | Computed t -> self#type_ cx pole_TODO acc t

    method private class_binding cx acc { class_private_fields; class_private_static_fields; _ } =
      let acc = self#props cx pole_TODO acc class_private_fields in
      let acc = self#props cx pole_TODO acc class_private_static_fields in
      acc

    method private call_arg cx acc =
      function
      | Arg t -> self#type_ cx pole_TODO acc t
      | SpreadArg t -> self#type_ cx pole_TODO acc t

    method private lookup_kind cx acc =
      function
      | Strict _ -> acc
      | NonstrictReturning (Some (t1, t2), _) ->
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | NonstrictReturning (None, _) -> acc
      | ShadowRead (_, props)
      | ShadowWrite props ->
        Nel.fold_left (self#props cx pole_TODO) acc props

    method private lookup_action cx acc =
      function
      | ReadProp { use_op = _; obj_t = t1; tout = t2 } ->
        let acc = self#type_ cx pole_TODO acc t1 in
        let acc = self#type_ cx pole_TODO acc t2 in
        acc
      | WriteProp { use_op = _; obj_t; prop_tout; tin; write_ctx = _; mode = _ } ->
        let acc = self#type_ cx pole_TODO acc obj_t in
        let acc = self#opt (self#type_ cx pole_TODO) acc prop_tout in
        let acc = self#type_ cx pole_TODO acc tin in
        acc
      | LookupProp (_, prop)
      | SuperProp (_, prop) ->
        self#prop cx pole_TODO acc prop
      | MatchProp (_, t) -> self#type_ cx pole_TODO acc t

    method private elem_action cx acc =
      function
      | ReadElem t -> self#type_ cx pole_TODO acc t
      | WriteElem (tin, tout, _) ->
        let acc = self#type_ cx pole_TODO acc tin in
        let acc = self#opt (self#type_ cx pole_TODO) acc tout in
        acc
      | CallElem (_, fn) -> self#method_action cx acc fn

    method private cont cx acc =
      function
      | Lower (_, l) -> self#type_ cx pole_TODO acc l
      | Upper u -> self#use_type_ cx acc u

    method private type_map cx acc =
      function
      | TupleMap t
      | ObjectMap t
      | ObjectMapi t ->
        self#type_ cx pole_TODO acc t

    method private choice_use_tool cx acc =
      function
      | FullyResolveType id ->
        let (_, acc) = self#type_graph cx (ISet.empty, acc) id in
        acc
      | TryFlow (_, spec) -> self#try_flow_spec cx acc spec

    method private goal_id cx pole acc id =
      match Context.goals cx |> IMap.find_opt id with
      | None -> acc
      | Some t -> self#type_ cx pole acc t

    method private type_graph cx (seen, acc) id =
      Graph_explorer.(
        let seen' = ISet.add id seen in
        if seen' == seen then
          (seen, acc)
        else
          let graph = Context.type_graph cx in
          let acc = (seen', self#goal_id cx pole_TODO acc id) in
          match Tbl.find graph id with
          | exception Not_found -> acc (* shouldn't happen *)
          | Unexplored { rev_deps = deps }
          | Explored { deps } ->
            ISet.fold (fun id acc -> self#type_graph cx acc id) deps acc)

    method private try_flow_spec cx acc =
      function
      | UnionCases (_, t, _rep, ts) ->
        let acc = self#type_ cx pole_TODO acc t in
        let acc = List.fold_left (self#type_ cx pole_TODO) acc ts in
        acc
      | IntersectionCases (ts, use) ->
        let acc = List.fold_left (self#type_ cx pole_TODO) acc ts in
        let acc = self#use_type_ cx acc use in
        acc

    method private object_kit_resolve cx acc =
      Object.(
        function
        | Next -> acc
        | List0 (ts, _) -> Nel.fold_left (self#type_ cx pole_TODO) acc ts
        | List (ts, rs, _) ->
          let acc = List.fold_left (self#type_ cx pole_TODO) acc ts in
          let acc = Nel.fold_left (Nel.fold_left (self#object_kit_slice cx)) acc rs in
          acc)

    method private object_kit_slice cx acc { Object.reason = _; props; dict; flags = _ } =
      let acc = self#smap (fun acc (t, _) -> self#type_ cx pole_TODO acc t) acc props in
      let acc = self#opt (self#dict_type cx pole_TODO) acc dict in
      acc

    method private object_kit_spread_operand_slice
        cx acc { Object.Spread.reason = _; prop_map; dict } =
      let acc = self#smap (Property.fold_t (self#type_ cx pole_TODO)) acc prop_map in
      let acc = self#opt (self#dict_type cx pole_TODO) acc dict in
      acc

    method private object_kit_acc_element cx acc =
      Object.Spread.(
        function
        | InlineSlice slice -> self#object_kit_spread_operand_slice cx acc slice
        | ResolvedSlice resolved -> Nel.fold_left (self#object_kit_slice cx) acc resolved)

    method private object_kit_spread_operand cx acc =
      Object.Spread.(
        function
        | Slice operand_slice -> self#object_kit_spread_operand_slice cx acc operand_slice
        | Type t -> self#type_ cx pole_TODO acc t)

    method private react_resolved_object cx acc (_, props, dict, _) =
      let acc = self#smap (self#prop cx pole_TODO) acc props in
      let acc = self#opt (self#dict_type cx pole_TODO) acc dict in
      acc

    method private react_resolve_object cx acc o =
      React.(
        match o with
        | ResolveObject -> acc
        | ResolveDict (dict, props, o) ->
          let acc = self#dict_type cx pole_TODO acc dict in
          let acc = self#smap (self#prop cx pole_TODO) acc props in
          let acc = self#react_resolved_object cx acc o in
          acc
        | ResolveProp (_, props, o) ->
          let acc = self#smap (self#prop cx pole_TODO) acc props in
          let acc = self#react_resolved_object cx acc o in
          acc)

    method private react_create_class_tool cx acc tool =
      React.CreateClass.(
        match tool with
        | Spec tail -> self#react_create_class_stack_tail cx acc tail
        | Mixins stack -> self#react_create_class_stack cx acc stack
        | Statics stack -> self#react_create_class_stack cx acc stack
        | PropTypes (stack, o) ->
          let acc = self#react_create_class_stack cx acc stack in
          let acc = self#react_resolve_object cx acc o in
          acc
        | DefaultProps (ts, dp) ->
          let acc = List.fold_left (self#type_ cx pole_TODO) acc ts in
          let acc = self#opt (self#maybe_known (self#react_resolved_object cx)) acc dp in
          acc
        | InitialState (ts, s) ->
          let acc = List.fold_left (self#type_ cx pole_TODO) acc ts in
          let acc =
            self#opt (self#maybe_known (self#or_null (self#react_resolved_object cx))) acc s
          in
          acc)

    method private react_create_class_stack cx acc (head, tail) =
      let acc = self#react_create_class_stack_head cx acc head in
      let acc = self#react_create_class_stack_tail cx acc tail in
      acc

    method private react_create_class_stack_head cx acc (o, spec) =
      let acc = self#react_resolved_object cx acc o in
      let acc = self#react_create_class_spec cx acc spec in
      acc

    method private react_create_class_stack_tail cx acc =
      List.fold_left
        (fun acc (head, ts, specs) ->
          let acc = self#react_create_class_stack_head cx acc head in
          let acc = List.fold_left (self#type_ cx pole_TODO) acc ts in
          let acc = List.fold_left (self#maybe_known (self#react_create_class_spec cx)) acc specs in
          acc)
        acc

    method private react_create_class_spec cx acc spec =
      React.CreateClass.(
        let { obj; statics; prop_types; get_default_props; get_initial_state; _ } = spec in
        let acc = self#react_resolved_object cx acc obj in
        let acc = self#opt (self#maybe_known (self#react_resolved_object cx)) acc statics in
        let acc = self#opt (self#maybe_known (self#react_resolved_object cx)) acc prop_types in
        let acc = List.fold_left (self#type_ cx pole_TODO) acc get_default_props in
        let acc = List.fold_left (self#type_ cx pole_TODO) acc get_initial_state in
        acc)

    method private maybe_known
        : 't. ('a -> 't -> 'a) -> 'a -> 't React.CreateClass.maybe_known -> 'a =
      React.CreateClass.(
        fun f acc x ->
          match x with
          | Known a -> f acc a
          | Unknown _ -> acc)

    method private or_null : 't. ('a -> 't -> 'a) -> 'a -> 't React.CreateClass.or_null -> 'a =
      React.CreateClass.(
        fun f acc x ->
          match x with
          | NotNull a -> f acc a
          | Null _ -> acc)

    method private react_create_class_knot cx acc knot =
      React.CreateClass.(
        let { this; static; state_t; default_t } = knot in
        let acc = self#type_ cx pole_TODO acc this in
        let acc = self#type_ cx pole_TODO acc static in
        let acc = self#type_ cx pole_TODO acc state_t in
        let acc = self#type_ cx pole_TODO acc default_t in
        acc)

    method private list : 't. ('a -> 't -> 'a) -> 'a -> 't list -> 'a = List.fold_left

    method private nel : 't. ('a -> 't -> 'a) -> 'a -> 't Nel.t -> 'a = Nel.fold_left

    method private opt : 't. ('a -> 't -> 'a) -> 'a -> 't option -> 'a =
      (fun f acc opt -> Option.fold opt ~init:acc ~f)

    method private smap : 't. ('a -> 't -> 'a) -> 'a -> 't SMap.t -> 'a =
      (fun f acc smap -> SMap.fold (fun _ t acc -> f acc t) smap acc)
  end
