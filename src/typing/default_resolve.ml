(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Flow_js_utils

let rec default_resolve_touts ~flow ?resolve_callee cx loc u =
  let _TODO = () in
  let any = AnyT.at (AnyError None) loc in
  let resolve t = flow (any, t) in
  let resolve_tvar t = flow (any, OpenT t) in
  let map_opt f t = Base.Option.iter ~f t in
  let resolve_specialized_callee x =
    let resolve_callee =
      match resolve_callee with
      | None -> any
      | Some (r, []) -> DefT (r, MixedT Mixed_everything)
      | Some (_, [t]) -> t
      | Some (r, t1 :: t2 :: ts) -> IntersectionT (r, InterRep.make t1 t2 ts)
    in
    CalleeRecorder.add_callee cx CalleeRecorder.All resolve_callee x
  in
  let resolve_method_action action =
    match action with
    | ChainM
        {
          exp_reason = _;
          lhs_reason = _;
          methodcalltype = { meth_tout; _ };
          voided_out = tout;
          return_hint = _;
          specialized_callee;
        } ->
      resolve tout;
      resolve_tvar meth_tout;
      resolve_specialized_callee specialized_callee
    | CallM { methodcalltype = { meth_tout; _ }; return_hint = _; specialized_callee } ->
      resolve_tvar meth_tout;
      resolve_specialized_callee specialized_callee
    | NoMethodAction tout -> resolve tout
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
    | ReadElem { tout; _ } -> resolve_tvar tout
    | WriteElem { tout; _ } -> map_opt resolve tout
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
    | GetRef tout ->
      resolve tout
  in
  let resolve_spread_resolve resolve_tool =
    match resolve_tool with
    | ResolveSpreadsToTupleType (_, _, t)
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
  | BindT (_, _, { call_tout; _ }) -> resolve_tvar call_tout
  | CallT
      {
        use_op = _;
        reason = _;
        call_action = Funcalltype { call_tout; call_specialized_callee; _ };
        return_hint = _;
      } ->
    resolve_tvar call_tout;
    resolve_specialized_callee call_specialized_callee
  | CallT { use_op = _; reason = _; call_action = ConcretizeCallee tout; return_hint = _ } ->
    resolve_tvar tout
  | ConditionalT { tout; _ } -> resolve_tvar tout
  | MethodT (_, _, _, _, action)
  | PrivateMethodT (_, _, _, _, _, _, action) ->
    resolve_method_action action
  | SetPropT (_, _, _, _, _, _, topt)
  | SetPrivatePropT (_, _, _, _, _, _, _, _, topt) ->
    map_opt resolve topt
  | GetTypeFromNamespaceT { tout = tvar; _ }
  | GetPrivatePropT (_, _, _, _, _, tvar)
  | GetPropT { use_op = _; reason = _; id = _; from_annot = _; propref = _; tout = tvar; hint = _ }
  | TestPropT { use_op = _; reason = _; id = _; propref = _; tout = tvar; hint = _ } ->
    resolve_tvar tvar
  | SetElemT (_, _, _, _, _, topt) -> map_opt resolve topt
  | GetElemT { tout; _ } -> resolve_tvar tout
  | CallElemT (_, _, _, _, action) -> resolve_method_action action
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
  | ToStringT { t_out; _ } -> default_resolve_touts ~flow cx loc t_out
  | ArithT { result_t; _ } -> resolve result_t
  | ComparatorT _ -> ()
  | UnaryArithT { result_t; _ } -> resolve result_t
  | AssertBinaryInLHST _
  | AssertBinaryInRHST _
  | AssertForInRHST _
  | AssertInstanceofRHST _
  | AssertNonComponentLikeT _ ->
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
  | ValueToTypeReferenceT (_, _, _, t) -> resolve t
  | VarianceCheckT _ -> ()
  | ConcretizeTypeAppsT _ -> _TODO
  | LookupT { lookup_action; _ } -> resolve_lookup_action lookup_action
  | ObjAssignToT (_, _, _, t, _)
  | ObjAssignFromT (_, _, _, t, _)
  | ObjRestT (_, _, t, _)
  | ObjTestProtoT (_, t)
  | ObjTestT (_, _, t)
  | ArrRestT (_, _, _, t) ->
    resolve t
  | GetDictValuesT (_, use) -> default_resolve_touts ~flow cx loc use
  | GetKeysT (_, use) -> default_resolve_touts ~flow cx loc use
  | HasOwnPropT _ -> ()
  | GetValuesT (_, t) -> resolve t
  | ElemT (_, _, _, action) -> resolve_elem_action action
  | MakeExactT (_, k) -> resolve_cont k
  | AssertImportIsValueT _ -> ()
  | CJSExtractNamedExportsT (_, _, t)
  | CopyNamedExportsT (_, _, t)
  | CopyTypeExportsT (_, _, t) ->
    resolve t
  | CheckUntypedImportT _ -> ()
  | ExportNamedT { tout = t; _ }
  | ExportTypeT { tout = t; _ }
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
  | SentinelPropTestT (_, _, _, _, tvar) -> resolve_tvar tvar
  | OptionalChainT { t_out; voided_out; _ } ->
    resolve voided_out;
    default_resolve_touts ~flow cx loc t_out
  | InvariantT _ -> ()
  | CallLatentPredT { tout; _ } -> resolve_tvar tout
  | ResolveSpreadT (_, _, { rrt_resolve_to; _ }) -> resolve_spread_resolve rrt_resolve_to
  | CondT (_, _, _, t) -> resolve t
  | ExtendsUseT _ -> ()
  | ReactPropsToOut (_, t)
  | ReactInToProps (_, t) ->
    resolve t
  | DestructuringT (_, _, _, tvar, _) -> resolve_tvar tvar
  | ResolveUnionT { upper; _ } -> default_resolve_touts ~flow cx loc upper
  | TypeCastT (_, t) -> resolve t
  | EnumCastT _
  | EnumExhaustiveCheckT _ ->
    ()
  | HooklikeT tvar
  | DeepReadOnlyT (tvar, _, _) ->
    resolve_tvar tvar
  | ExtractReactRefT (_, t)
  | FilterOptionalT (_, t)
  | FilterMaybeT (_, t) ->
    resolve t
  | ImplicitVoidReturnT { action = PropagateVoid { return; _ }; _ } -> resolve return
  | ImplicitVoidReturnT { action = NoImplicitReturns _; _ } -> ()
  | SealGenericT { cont; _ } -> resolve_cont cont
  | OptionalIndexedAccessT { tout_tvar; _ } -> resolve_tvar tout_tvar
  | CheckUnusedPromiseT _ -> _TODO
  | WriteComputedObjPropCheckT _ -> ()
  | PromoteRendersRepresentationT { tout; _ } -> resolve tout
  | ConvertEmptyPropsToMixedT (_, tout) -> resolve tout
  | TryRenderTypePromotionT _ -> ()
  | ExitRendersT { renders_reason = _; u } -> default_resolve_touts ~flow cx loc u
  | EvalTypeDestructorT _ -> ()
