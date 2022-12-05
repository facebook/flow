(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

let rec default_resolve_touts ~flow cx loc u =
  let _TODO = () in
  let any = AnyT.at (AnyError None) loc in
  let resolve t = flow (any, t) in
  let resolve_tvar t = flow (any, OpenT t) in
  let map_opt f t = Base.Option.iter ~f t in
  let resolve_method_action action =
    match action with
    | ChainM
        {
          exp_reason = _;
          lhs_reason = _;
          this = _;
          methodcalltype = { meth_tout; _ };
          voided_out = tout;
          return_hint = _;
        } ->
      resolve tout;
      resolve_tvar meth_tout
    | CallM { methodcalltype = { meth_tout; _ }; return_hint = _ } -> resolve_tvar meth_tout
    | NoMethodAction -> ()
  in
  let resolve_lookup_action action =
    match action with
    | ReadProp { tout; _ } -> resolve_tvar tout
    | WriteProp { prop_tout; _ } -> map_opt resolve prop_tout
    | LookupProp _
    | SuperProp _
    | MatchProp _ ->
      ()
  in
  let resolve_elem_action action =
    match action with
    | ReadElem (_, tvar) -> resolve_tvar tvar
    | WriteElem (_, topt, _) -> map_opt resolve topt
    | CallElem (_, action) -> resolve_method_action action
  in
  let resolve_react_tool tool =
    let open React in
    match tool with
    | CreateElement0 { tout; _ }
    | CreateElement { tout; _ }
    | ConfigCheck tout
    | GetProps tout
    | GetConfig tout
    | GetConfigType (_, tout)
    | GetRef tout
    | SimplifyPropType (_, tout) ->
      resolve tout
  in
  let resolve_spread_resolve resolve_tool =
    match resolve_tool with
    | ResolveSpreadsToArrayLiteral (_, _, t)
    | ResolveSpreadsToArray (_, t) ->
      resolve t
    | ResolveSpreadsToMultiflowCallFull _ -> _TODO
    | ResolveSpreadsToMultiflowPartial (_, _, _, t) -> resolve t
    | ResolveSpreadsToMultiflowSubtypeFull _ -> _TODO
    | ResolveSpreadsToCustomFunCall (_, _, t, _) -> resolve t
    | ResolveSpreadsToCallT ({ call_tout; _ }, _) -> resolve_tvar call_tout
  in
  let resolve_cont cont =
    match cont with
    | Upper use -> default_resolve_touts ~flow cx loc use
    | Lower _ -> _TODO
  in
  match u with
  | UseT _ -> _TODO
  | BindT (_, _, { call_tout; _ })
  | CallT { use_op = _; reason = _; call_action = Funcalltype { call_tout; _ }; return_hint = _ } ->
    resolve_tvar call_tout
  | CallT { use_op = _; reason = _; call_action = ConcretizeCallee tout; return_hint = _ } ->
    resolve_tvar tout
  | MethodT (_, _, _, _, action, tout)
  | PrivateMethodT (_, _, _, _, _, _, action, tout) ->
    resolve tout;
    resolve_method_action action
  | SetPropT (_, _, _, _, _, _, topt)
  | SetPrivatePropT (_, _, _, _, _, _, _, _, topt) ->
    map_opt resolve topt
  | GetPropT (_, _, _, _, tvar)
  | MatchPropT (_, _, _, tvar)
  | GetPrivatePropT (_, _, _, _, _, tvar)
  | TestPropT (_, _, _, _, tvar) ->
    resolve_tvar tvar
  | SetElemT (_, _, _, _, _, topt) -> map_opt resolve topt
  | GetElemT (_, _, _, _, t) -> resolve_tvar t
  | CallElemT (_, _, _, action) -> resolve_method_action action
  | GetStaticsT tvar
  | GetProtoT (_, tvar) ->
    resolve_tvar tvar
  | SetProtoT _ -> ()
  | ReposLowerT _
  | ReposUseT _ ->
    _TODO
  | ConstructorT { use_op = _; reason = _; targs = _; args = _; tout; return_hint = _ } ->
    resolve tout
  | SuperT _ -> ()
  | ImplementsT _ -> ()
  | MixinT (_, t) -> resolve t
  | ToStringT (_, use) -> default_resolve_touts ~flow cx loc use
  | ArithT { result_t; _ } -> resolve result_t
  | ComparatorT _ -> ()
  | UnaryArithT { result_t; _ } -> resolve result_t
  | AssertBinaryInLHST _
  | AssertBinaryInRHST _
  | AssertForInRHST _
  | AssertInstanceofRHST _ ->
    ()
  | AssertIterableT { targs; _ } -> Base.List.iter ~f:resolve targs
  | PredicateT (_, tvar)
  | GuardT (_, _, tvar) ->
    resolve_tvar tvar
  | StrictEqT _
  | EqT _ ->
    ()
  | AndT (_, _, tvar)
  | OrT (_, _, tvar)
  | NullishCoalesceT (_, _, tvar)
  | NotT (_, tvar) ->
    resolve_tvar tvar
  | SpecializeT (_, _, _, _, _, tout) -> resolve tout
  | ThisSpecializeT (_, _, k) -> resolve_cont k
  | VarianceCheckT _ -> ()
  | TypeAppVarianceCheckT _
  | ConcretizeTypeAppsT _ ->
    _TODO
  | LookupT { lookup_action; _ } -> resolve_lookup_action lookup_action
  | ObjAssignToT (_, _, _, t, _)
  | ObjAssignFromT (_, _, _, t, _)
  | ObjRestT (_, _, t, _)
  | ObjTestProtoT (_, t)
  | ObjTestT (_, _, t)
  | ArrRestT (_, _, _, t) ->
    resolve t
  | BecomeT { t; _ } -> resolve t
  | GetDictValuesT (_, use) -> default_resolve_touts ~flow cx loc use
  | GetKeysT (_, use) -> default_resolve_touts ~flow cx loc use
  | HasOwnPropT _ -> ()
  | GetValuesT (_, t) -> resolve t
  | ElemT (_, _, _, action) -> resolve_elem_action action
  | MakeExactT (_, k) -> resolve_cont k
  | CJSRequireT (_, t, _)
  | ImportModuleNsT { t; _ }
  | ImportDefaultT (_, _, _, t, _)
  | ImportNamedT (_, _, _, _, t, _)
  | ImportTypeT (_, _, t)
  | ImportTypeofT (_, _, t) ->
    resolve t
  | AssertImportIsValueT _ -> ()
  | CJSExtractNamedExportsT (_, _, t)
  | CopyNamedExportsT (_, _, t)
  | CopyTypeExportsT (_, _, t) ->
    resolve t
  | CheckUntypedImportT _ -> ()
  | ExportNamedT (_, _, _, t)
  | ExportTypeT (_, _, _, t)
  | AssertExportIsTypeT (_, _, t)
  | MapTypeT (_, _, _, t)
  | ObjKitT (_, _, _, _, t) ->
    resolve t
  | ReactKitT (_, _, tool) -> resolve_react_tool tool
  | ChoiceKitUseT _ -> _TODO
  | PreprocessKitT _ -> _TODO
  | DebugPrintT _
  | DebugSleepT _ ->
    ()
  | SentinelPropTestT (_, _, _, _, _, tvar) -> resolve_tvar tvar
  | IdxUnwrap (_, t)
  | IdxUnMaybeifyT (_, t) ->
    resolve t
  | OptionalChainT { t_out; voided_out; _ } ->
    resolve voided_out;
    default_resolve_touts ~flow cx loc t_out
  | InvariantT _ -> ()
  | CallLatentPredT (_, _, _, _, tvar)
  | CallOpenPredT (_, _, _, _, tvar) ->
    resolve_tvar tvar
  | SubstOnPredT (_, _, _, OpenPredT { base_t; _ }) -> resolve base_t
  | SubstOnPredT (_, _, _, t) -> resolve t
  | RefineT (_, _, tvar) -> resolve_tvar tvar
  | ResolveSpreadT (_, _, { rrt_resolve_to; _ }) -> resolve_spread_resolve rrt_resolve_to
  | CondT (_, _, _, t) -> resolve t
  | ExtendsUseT _ -> ()
  | ReactPropsToOut (_, t)
  | ReactInToProps (_, t) ->
    resolve t
  | DestructuringT (_, _, _, tvar, _) -> resolve_tvar tvar
  | CreateObjWithComputedPropT { tout_tvar; _ } -> resolve_tvar tout_tvar
  | ResolveUnionT { upper; _ } -> default_resolve_touts ~flow cx loc upper
  | TypeCastT (_, t) -> resolve t
  | EnumCastT _
  | EnumExhaustiveCheckT _ ->
    ()
  | FilterOptionalT (_, t)
  | FilterMaybeT (_, t) ->
    resolve t
  | FunImplicitVoidReturnT { return; _ } -> resolve return
  | SealGenericT { cont; _ } -> resolve_cont cont
  | OptionalIndexedAccessT { tout_tvar; _ } -> resolve_tvar tout_tvar
  | CheckUnusedPromiseT _ -> _TODO
