(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Reason
open Type

(* Usually types carry enough information about the "reason" for their
   existence (e.g., position in code, introduction/elimination rules in
   the type system), so printing the reason provides a good idea of what the
   type means to the programmer. *)

let rec reason_of_t = function
  | OpenT (reason, _) -> reason
  | AnnotT (reason, _, _) -> reason
  | BoundT (reason, _) -> reason
  | InternalT (ChoiceKitT (reason, _)) -> reason
  | TypeDestructorTriggerT (_, reason, _, _, _) -> reason
  | CustomFunT (reason, _) -> reason
  | DefT (reason, _, _) -> reason
  | EvalT (_, defer_use_t, _) -> reason_of_defer_use_t defer_use_t
  | ExactT (reason, _) -> reason
  | ExistsT reason -> reason
  | GenericT { reason; _ } -> reason
  | InternalT (ExtendsT (reason, _, _)) -> reason
  | FunProtoT reason -> reason
  | FunProtoApplyT reason -> reason
  | FunProtoBindT reason -> reason
  | FunProtoCallT reason -> reason
  | KeysT (reason, _) -> reason
  | ModuleT (reason, _, _) -> reason
  | NullProtoT reason -> reason
  | ObjProtoT reason -> reason
  | MatchingPropT (reason, _, _) -> reason
  | OpaqueT (reason, _) -> reason
  | OpenPredT { reason; m_pos = _; m_neg = _; base_t = _ } -> reason
  | ReposT (reason, _) -> reason
  | InternalT (ReposUpperT (reason, _)) -> reason (* HUH? cf. mod_reason below *)
  | ShapeT (reason, _) -> reason
  | ThisClassT (reason, _, _) -> reason
  | ThisTypeAppT (reason, _, _, _) -> reason
  | TypeAppT (reason, _, _, _) -> reason
  | AnyT (reason, _) -> reason
  | UnionT (reason, _) -> reason
  | IntersectionT (reason, _) -> reason
  | MaybeT (reason, _) -> reason
  | OptionalT { reason; type_ = _; use_desc = _ } -> reason

and reason_of_defer_use_t = function
  | LatentPredT (reason, _)
  | TypeDestructorT (_, reason, _) ->
    reason

and reason_of_use_t = function
  | UseT (_, t) -> reason_of_t t
  | AdderT (_, reason, _, _, _) -> reason
  | AndT (reason, _, _) -> reason
  | ArrRestT (_, reason, _, _) -> reason
  | AssertArithmeticOperandT reason -> reason
  | AssertBinaryInLHST reason -> reason
  | AssertBinaryInRHST reason -> reason
  | AssertForInRHST reason -> reason
  | AssertIterableT { reason; _ } -> reason
  | AssertImportIsValueT (reason, _) -> reason
  | BecomeT { reason; _ } -> reason
  | BindT (_, reason, _, _) -> reason
  | CallElemT (reason, _, _, _) -> reason
  | CallLatentPredT (reason, _, _, _, _) -> reason
  | CallOpenPredT (reason, _, _, _, _) -> reason
  | CallT (_, reason, _) -> reason
  | ChoiceKitUseT (reason, _) -> reason
  | CJSExtractNamedExportsT (reason, _, _) -> reason
  | CJSRequireT (reason, _, _) -> reason
  | ComparatorT { reason; _ } -> reason
  | ConstructorT (_, reason, _, _, _) -> reason
  | CopyNamedExportsT (reason, _, _) -> reason
  | CopyTypeExportsT (reason, _, _) -> reason
  | DebugPrintT reason -> reason
  | DebugSleepT reason -> reason
  | ElemT (_, reason, _, _) -> reason
  | EnumCastT { enum = (reason, _, _); _ } -> reason
  | EnumExhaustiveCheckT { reason; _ } -> reason
  | EqT { reason; _ } -> reason
  | ExportNamedT (reason, _, _, _) -> reason
  | ExportTypeT (reason, _, _, _) -> reason
  | FunImplicitVoidReturnT { reason; _ } -> reason
  | AssertExportIsTypeT (reason, _, _) -> reason
  | ExtendsUseT (_, reason, _, _, _) -> reason
  | GetElemT (_, reason, _, _) -> reason
  | GetKeysT (reason, _) -> reason
  | GetValuesT (reason, _) -> reason
  | GetPropT (_, reason, _, _) -> reason
  | GetPrivatePropT (_, reason, _, _, _, _) -> reason
  | GetProtoT (reason, _) -> reason
  | GetStaticsT (reason, _) -> reason
  | GuardT (_, _, (r, _)) -> r
  | HasOwnPropT (_, reason, _) -> reason
  | IdxUnMaybeifyT (reason, _) -> reason
  | IdxUnwrap (reason, _) -> reason
  | ImplementsT (_, t) -> reason_of_t t
  | ImportDefaultT (reason, _, _, _, _) -> reason
  | ImportModuleNsT (reason, _, _) -> reason
  | ImportNamedT (reason, _, _, _, _, _) -> reason
  | ImportTypeofT (reason, _, _) -> reason
  | ImportTypeT (reason, _, _) -> reason
  | IntersectionPreprocessKitT (reason, _) -> reason
  | InvariantT reason -> reason
  | LookupT { reason; _ } -> reason
  | MakeExactT (reason, _) -> reason
  | MapTypeT (_, reason, _, _) -> reason
  | MethodT (_, reason, _, _, _, _) -> reason
  | MixinT (reason, _) -> reason
  | NotT (reason, _) -> reason
  | NullishCoalesceT (reason, _, _) -> reason
  | ObjAssignToT (_, reason, _, _, _) -> reason
  | ObjAssignFromT (_, reason, _, _, _) -> reason
  | ObjRestT (reason, _, _, _) -> reason
  | ObjSealT (reason, _) -> reason
  | ObjTestProtoT (reason, _) -> reason
  | ObjTestT (reason, _, _) -> reason
  | OptionalChainT { reason; _ } -> reason
  | OrT (reason, _, _) -> reason
  | PredicateT (_, (reason, _)) -> reason
  | ReactKitT (_, reason, _) -> reason
  | RefineT (reason, _, _) -> reason
  | ReposLowerT (reason, _, _) -> reason
  | ReposUseT (reason, _, _, _) -> reason
  | ResolveSpreadT (_, reason, _) -> reason
  | SentinelPropTestT (_, _, _, _, _, (reason, _)) -> reason
  | SetElemT (_, reason, _, _, _, _) -> reason
  | SetPropT (_, reason, _, _, _, _, _) -> reason
  | SetPrivatePropT (_, reason, _, _, _, _, _, _) -> reason
  | SetProtoT (reason, _) -> reason
  | SpecializeT (_, _, reason, _, _, _) -> reason
  | StrictEqT { reason; _ } -> reason
  | ObjKitT (_, reason, _, _, _) -> reason
  | ModuleExportsAssignT (reason, _, _) -> reason
  | SubstOnPredT (_, reason, _, _) -> reason
  | SuperT (_, reason, _) -> reason
  | TestPropT (reason, _, _, _) -> reason
  | ThisSpecializeT (reason, _, _) -> reason
  | ToStringT (reason, _) -> reason
  | UnaryMinusT (reason, _) -> reason
  | UnifyT (_, t) -> reason_of_t t
  | VarianceCheckT (reason, _, _, _) -> reason
  | TypeAppVarianceCheckT (_, reason, _, _) -> reason
  | TypeCastT (_, t) -> reason_of_t t
  | FilterOptionalT (_, t) -> reason_of_t t
  | FilterMaybeT (_, t) -> reason_of_t t
  | ConcretizeTypeAppsT (_, _, (_, _, _, reason), _) -> reason
  | CondT (reason, _, _, _) -> reason
  | MatchPropT (_, reason, _, _) -> reason
  | ReactPropsToOut (reason, _)
  | ReactInToProps (reason, _)
  | SealGenericT { reason; _ } ->
    reason
  | DestructuringT (reason, _, _, _, _) -> reason
  | CreateObjWithComputedPropT { reason; value = _; tout_tvar = _ } -> reason
  | ResolveUnionT { reason; _ } -> reason

(* helper: we want the tvar id as well *)
(* NOTE: uncalled for now, because ids are nondetermistic
   due to parallelism, which messes up test diffs. Should
   add a config, but for now must uncomment impl to use *)
let reason_of_t_add_id = reason_of_t

(* function
   | OpenT (r, id) -> prefix_reason (spf "%d: " id) r
   | t -> reason_of_t t *)
let reason_of_use_t_add_id = reason_of_use_t

let desc_of_t = reason_of_t %> desc_of_reason

let loc_of_t = reason_of_t %> aloc_of_reason

let def_loc_of_t = reason_of_t %> def_aloc_of_reason

(* TODO make a type visitor *)
let rec mod_reason_of_t f = function
  | OpenT (reason, id) -> OpenT (f reason, id)
  | AnnotT (reason, t, use_desc) -> AnnotT (f reason, t, use_desc)
  | BoundT (reason, name) -> BoundT (f reason, name)
  | InternalT (ChoiceKitT (reason, tool)) -> InternalT (ChoiceKitT (f reason, tool))
  | TypeDestructorTriggerT (use_op, reason, repos, d, t) ->
    TypeDestructorTriggerT (use_op, f reason, repos, d, t)
  | CustomFunT (reason, kind) -> CustomFunT (f reason, kind)
  | DefT (reason, trust, t) -> DefT (f reason, trust, t)
  | AnyT (reason, src) -> AnyT (f reason, src)
  | UnionT (reason, src) -> UnionT (f reason, src)
  | IntersectionT (reason, src) -> IntersectionT (f reason, src)
  | MaybeT (reason, src) -> MaybeT (f reason, src)
  | OptionalT { reason; type_; use_desc } -> OptionalT { reason = f reason; type_; use_desc }
  | EvalT (t, defer_use_t, id) -> EvalT (t, mod_reason_of_defer_use_t f defer_use_t, id)
  | ExactT (reason, t) -> ExactT (f reason, t)
  | ExistsT reason -> ExistsT (f reason)
  | GenericT ({ reason; _ } as generic) -> GenericT { generic with reason = f reason }
  | InternalT (ExtendsT (reason, t1, t2)) -> InternalT (ExtendsT (f reason, t1, t2))
  | FunProtoApplyT reason -> FunProtoApplyT (f reason)
  | FunProtoT reason -> FunProtoT (f reason)
  | FunProtoBindT reason -> FunProtoBindT (f reason)
  | FunProtoCallT reason -> FunProtoCallT (f reason)
  | KeysT (reason, t) -> KeysT (f reason, t)
  | ModuleT (reason, exports, is_strict) -> ModuleT (f reason, exports, is_strict)
  | NullProtoT reason -> NullProtoT (f reason)
  | ObjProtoT reason -> ObjProtoT (f reason)
  | MatchingPropT (reason, k, v) -> MatchingPropT (f reason, k, v)
  | OpaqueT (reason, opaquetype) -> OpaqueT (f reason, opaquetype)
  | OpenPredT { reason; base_t; m_pos; m_neg } ->
    OpenPredT { reason = f reason; base_t; m_pos; m_neg }
  | ReposT (reason, t) -> ReposT (f reason, t)
  | InternalT (ReposUpperT (reason, t)) -> InternalT (ReposUpperT (reason, mod_reason_of_t f t))
  | ShapeT (reason, t) -> ShapeT (f reason, t)
  | ThisClassT (reason, t, is_this) -> ThisClassT (f reason, t, is_this)
  | ThisTypeAppT (reason, t1, t2, t3) -> ThisTypeAppT (f reason, t1, t2, t3)
  | TypeAppT (reason, t1, t2, t3) -> TypeAppT (f reason, t1, t2, t3)

and mod_reason_of_defer_use_t f = function
  | LatentPredT (reason, p) -> LatentPredT (f reason, p)
  | TypeDestructorT (use_op, reason, s) -> TypeDestructorT (use_op, f reason, s)

and mod_reason_of_use_t f = function
  | UseT (_, t) -> UseT (Op UnknownUse, mod_reason_of_t f t)
  | AdderT (use_op, reason, flip, rt, lt) -> AdderT (use_op, f reason, flip, rt, lt)
  | AndT (reason, t1, t2) -> AndT (f reason, t1, t2)
  | ArrRestT (use_op, reason, i, t) -> ArrRestT (use_op, f reason, i, t)
  | AssertArithmeticOperandT reason -> AssertArithmeticOperandT (f reason)
  | AssertBinaryInLHST reason -> AssertBinaryInLHST (f reason)
  | AssertBinaryInRHST reason -> AssertBinaryInRHST (f reason)
  | AssertForInRHST reason -> AssertForInRHST (f reason)
  | AssertIterableT ({ reason; _ } as contents) ->
    AssertIterableT { contents with reason = f reason }
  | AssertImportIsValueT (reason, name) -> AssertImportIsValueT (f reason, name)
  | BecomeT { reason; t; empty_success } -> BecomeT { reason = f reason; t; empty_success }
  | BindT (use_op, reason, ft, pass) -> BindT (use_op, f reason, ft, pass)
  | CallElemT (reason_call, reason_lookup, t, ft) -> CallElemT (f reason_call, reason_lookup, t, ft)
  | CallLatentPredT (reason, b, k, l, t) -> CallLatentPredT (f reason, b, k, l, t)
  | CallOpenPredT (reason, sense, key, l, t) -> CallOpenPredT (f reason, sense, key, l, t)
  | CallT (use_op, reason, ft) -> CallT (use_op, f reason, ft)
  | ChoiceKitUseT (reason, tool) -> ChoiceKitUseT (f reason, tool)
  | CJSExtractNamedExportsT (reason, exports, t2) -> CJSExtractNamedExportsT (f reason, exports, t2)
  | CJSRequireT (reason, t, is_strict) -> CJSRequireT (f reason, t, is_strict)
  | ComparatorT ({ reason; _ } as x) -> ComparatorT { x with reason = f reason }
  | ConstructorT (use_op, reason, targs, args, tout) ->
    ConstructorT (use_op, f reason, targs, args, tout)
  | CopyNamedExportsT (reason, target_module_t, t_out) ->
    CopyNamedExportsT (f reason, target_module_t, t_out)
  | CopyTypeExportsT (reason, target_module_t, t_out) ->
    CopyTypeExportsT (f reason, target_module_t, t_out)
  | DebugPrintT reason -> DebugPrintT (f reason)
  | DebugSleepT reason -> DebugSleepT (f reason)
  | ElemT (use_op, reason, t, action) -> ElemT (use_op, f reason, t, action)
  | EnumCastT { use_op; enum = (reason, trust, enum) } ->
    EnumCastT { use_op; enum = (f reason, trust, enum) }
  | EnumExhaustiveCheckT { reason; check; incomplete_out; discriminant_after_check } ->
    EnumExhaustiveCheckT { reason = f reason; check; incomplete_out; discriminant_after_check }
  | EqT ({ reason; _ } as x) -> EqT { x with reason = f reason }
  | ExportNamedT (reason, tmap, export_kind, t_out) ->
    ExportNamedT (f reason, tmap, export_kind, t_out)
  | ExportTypeT (reason, name, t, t_out) -> ExportTypeT (f reason, name, t, t_out)
  | AssertExportIsTypeT (reason, export_name, t_out) ->
    AssertExportIsTypeT (f reason, export_name, t_out)
  | ExtendsUseT (use_op, reason, ts, t1, t2) -> ExtendsUseT (use_op, f reason, ts, t1, t2)
  | FunImplicitVoidReturnT ({ reason; _ } as contents) ->
    FunImplicitVoidReturnT { contents with reason = f reason }
  | GetElemT (use_op, reason, it, et) -> GetElemT (use_op, f reason, it, et)
  | GetKeysT (reason, t) -> GetKeysT (f reason, t)
  | GetValuesT (reason, t) -> GetValuesT (f reason, t)
  | GetPropT (use_op, reason, n, t) -> GetPropT (use_op, f reason, n, t)
  | GetPrivatePropT (use_op, reason, name, bindings, static, t) ->
    GetPrivatePropT (use_op, f reason, name, bindings, static, t)
  | GetProtoT (reason, t) -> GetProtoT (f reason, t)
  | GetStaticsT (reason, t) -> GetStaticsT (f reason, t)
  | GuardT (pred, result, (reason, tvar)) -> GuardT (pred, result, (f reason, tvar))
  | HasOwnPropT (use_op, reason, t) -> HasOwnPropT (use_op, f reason, t)
  | IdxUnMaybeifyT (reason, t_out) -> IdxUnMaybeifyT (f reason, t_out)
  | IdxUnwrap (reason, t_out) -> IdxUnwrap (f reason, t_out)
  | ImplementsT (use_op, t) -> ImplementsT (use_op, mod_reason_of_t f t)
  | ImportDefaultT (reason, import_kind, name, t, is_strict) ->
    ImportDefaultT (f reason, import_kind, name, t, is_strict)
  | ImportModuleNsT (reason, t, is_strict) -> ImportModuleNsT (f reason, t, is_strict)
  | ImportNamedT (reason, import_kind, name, t, module_name, is_strict) ->
    ImportNamedT (f reason, import_kind, name, t, module_name, is_strict)
  | ImportTypeofT (reason, name, t) -> ImportTypeofT (f reason, name, t)
  | ImportTypeT (reason, name, t) -> ImportTypeT (f reason, name, t)
  | IntersectionPreprocessKitT (reason, tool) -> IntersectionPreprocessKitT (f reason, tool)
  | InvariantT reason -> InvariantT (f reason)
  | LookupT { reason; lookup_kind; ts; propref; lookup_action; ids } ->
    LookupT { reason = f reason; lookup_kind; ts; propref; lookup_action; ids }
  | MakeExactT (reason, t) -> MakeExactT (f reason, t)
  | MapTypeT (use_op, reason, kind, t) -> MapTypeT (use_op, f reason, kind, t)
  | MethodT (use_op, reason_call, reason_lookup, name, ft, tm) ->
    MethodT (use_op, f reason_call, reason_lookup, name, ft, tm)
  | MixinT (reason, inst) -> MixinT (f reason, inst)
  | NotT (reason, t) -> NotT (f reason, t)
  | NullishCoalesceT (reason, t1, t2) -> NullishCoalesceT (f reason, t1, t2)
  | ObjAssignToT (op, reason, t, t2, kind) -> ObjAssignToT (op, f reason, t, t2, kind)
  | ObjAssignFromT (op, reason, t, t2, kind) -> ObjAssignFromT (op, f reason, t, t2, kind)
  | ObjRestT (reason, t, t2, id) -> ObjRestT (f reason, t, t2, id)
  | ObjSealT (reason, t) -> ObjSealT (f reason, t)
  | ObjTestProtoT (reason, t) -> ObjTestProtoT (f reason, t)
  | ObjTestT (reason, t1, t2) -> ObjTestT (f reason, t1, t2)
  | OptionalChainT ({ reason; _ } as opt_chain) ->
    OptionalChainT { opt_chain with reason = f reason }
  | OrT (reason, t1, t2) -> OrT (f reason, t1, t2)
  | PredicateT (pred, (reason, t)) -> PredicateT (pred, (f reason, t))
  | ReactKitT (use_op, reason, tool) -> ReactKitT (use_op, f reason, tool)
  | RefineT (reason, p, t) -> RefineT (f reason, p, t)
  | ReposLowerT (reason, use_desc, t) -> ReposLowerT (f reason, use_desc, t)
  | ReposUseT (reason, use_desc, use_op, t) -> ReposUseT (f reason, use_desc, use_op, t)
  | ResolveSpreadT (use_op, reason_op, resolve) -> ResolveSpreadT (use_op, f reason_op, resolve)
  | SealGenericT ({ reason; _ } as generic) -> SealGenericT { generic with reason = f reason }
  | SentinelPropTestT (reason_op, l, key, sense, sentinel, (reason, result)) ->
    SentinelPropTestT (reason_op, l, key, sense, sentinel, (f reason, result))
  | SetElemT (use_op, reason, it, mode, et, t) -> SetElemT (use_op, f reason, it, mode, et, t)
  | SetPropT (use_op, reason, n, mode, i, t, tp) -> SetPropT (use_op, f reason, n, mode, i, t, tp)
  | SetPrivatePropT (use_op, reason, n, mode, scopes, static, t, tp) ->
    SetPrivatePropT (use_op, f reason, n, mode, scopes, static, t, tp)
  | SetProtoT (reason, t) -> SetProtoT (f reason, t)
  | SpecializeT (use_op, reason_op, reason_tapp, cache, ts, t) ->
    SpecializeT (use_op, f reason_op, reason_tapp, cache, ts, t)
  | StrictEqT { reason; cond_context; flip; arg } ->
    StrictEqT { reason = f reason; cond_context; flip; arg }
  | ObjKitT (use_op, reason, resolve_tool, tool, tout) ->
    ObjKitT (use_op, f reason, resolve_tool, tool, tout)
  | ModuleExportsAssignT (reason, ts, t) -> ModuleExportsAssignT (f reason, ts, t)
  | SubstOnPredT (use_op, reason, subst, t) -> SubstOnPredT (use_op, f reason, subst, t)
  | SuperT (op, reason, inst) -> SuperT (op, f reason, inst)
  | TestPropT (reason, id, n, t) -> TestPropT (f reason, id, n, t)
  | ThisSpecializeT (reason, this, k) -> ThisSpecializeT (f reason, this, k)
  | ToStringT (reason, t) -> ToStringT (f reason, t)
  | UnaryMinusT (reason, t) -> UnaryMinusT (f reason, t)
  | UnifyT (t, t2) -> UnifyT (mod_reason_of_t f t, mod_reason_of_t f t2)
  | VarianceCheckT (reason, tparams, targs, polarity) ->
    VarianceCheckT (f reason, tparams, targs, polarity)
  | TypeAppVarianceCheckT (use_op, reason_op, reason_tapp, targs) ->
    TypeAppVarianceCheckT (use_op, f reason_op, reason_tapp, targs)
  | TypeCastT (use_op, t) -> TypeCastT (use_op, mod_reason_of_t f t)
  | FilterOptionalT (use_op, t) -> FilterOptionalT (use_op, mod_reason_of_t f t)
  | FilterMaybeT (use_op, t) -> FilterMaybeT (use_op, mod_reason_of_t f t)
  | ConcretizeTypeAppsT (use_op, t1, (t2, ts2, op2, r2), targs) ->
    ConcretizeTypeAppsT (use_op, t1, (t2, ts2, op2, f r2), targs)
  | CondT (reason, then_t, else_t, tout) -> CondT (f reason, then_t, else_t, tout)
  | MatchPropT (op, reason, prop, t) -> MatchPropT (op, f reason, prop, t)
  | ReactPropsToOut (reason, t) -> ReactPropsToOut (f reason, t)
  | ReactInToProps (reason, t) -> ReactInToProps (f reason, t)
  | DestructuringT (reason, a, s, t, id) -> DestructuringT (f reason, a, s, t, id)
  | CreateObjWithComputedPropT { reason; value; tout_tvar } ->
    CreateObjWithComputedPropT { reason = f reason; value; tout_tvar }
  | ResolveUnionT { reason; resolved; unresolved; upper; id } ->
    ResolveUnionT { reason = f reason; resolved; unresolved; upper; id }

and mod_reason_of_opt_use_t f = function
  | OptCallT (use_op, reason, ft) -> OptCallT (use_op, reason, ft)
  | OptMethodT (op, r1, r2, ref, action, prop_tout) ->
    OptMethodT (op, f r1, r2, ref, action, prop_tout)
  | OptGetPropT (use_op, reason, n) -> OptGetPropT (use_op, f reason, n)
  | OptGetPrivatePropT (use_op, reason, name, bindings, static) ->
    OptGetPrivatePropT (use_op, f reason, name, bindings, static)
  | OptTestPropT (reason, id, n) -> OptTestPropT (f reason, id, n)
  | OptGetElemT (use_op, reason, it) -> OptGetElemT (use_op, f reason, it)
  | OptCallElemT (r1, r2, elt, call) -> OptCallElemT (f r1, r2, elt, call)

let rec util_use_op_of_use_t :
          'a. (use_t -> 'a) -> (use_t -> use_op -> (use_op -> use_t) -> 'a) -> use_t -> 'a =
 fun nope util u ->
  let util = util u in
  let nested_util u2 make2 =
    let result = util_use_op_of_use_t (fun _ -> None) (fun _ op make -> Some (op, make)) u2 in
    match result with
    | None -> nope u
    | Some (op, make) -> util op (fun op -> make2 (make op))
  in
  match u with
  | UseT (op, t) -> util op (fun op -> UseT (op, t))
  | BindT (op, r, f, b) -> util op (fun op -> BindT (op, r, f, b))
  | CallT (op, r, f) -> util op (fun op -> CallT (op, r, f))
  | MethodT (op, r1, r2, p, f, tm) -> util op (fun op -> MethodT (op, r1, r2, p, f, tm))
  | SetPropT (op, r, p, m, w, t, tp) -> util op (fun op -> SetPropT (op, r, p, m, w, t, tp))
  | SetPrivatePropT (op, r, s, m, c, b, t, tp) ->
    util op (fun op -> SetPrivatePropT (op, r, s, m, c, b, t, tp))
  | GetPropT (op, r, p, t) -> util op (fun op -> GetPropT (op, r, p, t))
  | MatchPropT (op, r, p, t) -> util op (fun op -> MatchPropT (op, r, p, t))
  | GetPrivatePropT (op, r, s, c, b, t) -> util op (fun op -> GetPrivatePropT (op, r, s, c, b, t))
  | SetElemT (op, r, t1, m, t2, t3) -> util op (fun op -> SetElemT (op, r, t1, m, t2, t3))
  | GetElemT (op, r, t1, t2) -> util op (fun op -> GetElemT (op, r, t1, t2))
  | ReposLowerT (r, d, u2) -> nested_util u2 (fun u2 -> ReposLowerT (r, d, u2))
  | ReposUseT (r, d, op, t) -> util op (fun op -> ReposUseT (r, d, op, t))
  | ConstructorT (op, r, targs, args, t) -> util op (fun op -> ConstructorT (op, r, targs, args, t))
  | SuperT (op, r, i) -> util op (fun op -> SuperT (op, r, i))
  | AdderT (op, d, f, l, r) -> util op (fun op -> AdderT (op, d, f, l, r))
  | ImplementsT (op, t) -> util op (fun op -> ImplementsT (op, t))
  | ToStringT (r, u2) -> nested_util u2 (fun u2 -> ToStringT (r, u2))
  | SpecializeT (op, r1, r2, c, ts, t) -> util op (fun op -> SpecializeT (op, r1, r2, c, ts, t))
  | TypeAppVarianceCheckT (op, r1, r2, ts) ->
    util op (fun op -> TypeAppVarianceCheckT (op, r1, r2, ts))
  | TypeCastT (op, t) -> util op (fun op -> TypeCastT (op, t))
  | EnumCastT { use_op; enum } -> util use_op (fun use_op -> EnumCastT { use_op; enum })
  | FunImplicitVoidReturnT ({ use_op; _ } as contents) ->
    util use_op (fun use_op -> FunImplicitVoidReturnT { contents with use_op })
  | FilterOptionalT (op, t) -> util op (fun op -> FilterOptionalT (op, t))
  | FilterMaybeT (op, t) -> util op (fun op -> FilterMaybeT (op, t))
  | ConcretizeTypeAppsT (u, (ts1, op, r1), x2, b) ->
    util op (fun op -> ConcretizeTypeAppsT (u, (ts1, op, r1), x2, b))
  | ArrRestT (op, r, i, t) -> util op (fun op -> ArrRestT (op, r, i, t))
  | HasOwnPropT (op, r, t) -> util op (fun op -> HasOwnPropT (op, r, t))
  | GetKeysT (r, u2) -> nested_util u2 (fun u2 -> GetKeysT (r, u2))
  | ElemT (op, r, t, a) -> util op (fun op -> ElemT (op, r, t, a))
  | ObjKitT (op, r, x, y, t) -> util op (fun op -> ObjKitT (op, r, x, y, t))
  | ReactKitT (op, r, t) -> util op (fun op -> ReactKitT (op, r, t))
  | ResolveSpreadT (op, r, s) -> util op (fun op -> ResolveSpreadT (op, r, s))
  | ExtendsUseT (op, r, ts, a, b) -> util op (fun op -> ExtendsUseT (op, r, ts, a, b))
  | MapTypeT (op, r, k, t) -> util op (fun op -> MapTypeT (op, r, k, t))
  | ObjAssignToT (op, r, t1, t2, k) -> util op (fun op -> ObjAssignToT (op, r, t1, t2, k))
  | ObjAssignFromT (op, r, t1, t2, k) -> util op (fun op -> ObjAssignFromT (op, r, t1, t2, k))
  | MakeExactT (r, Lower (op, t)) -> util op (fun op -> MakeExactT (r, Lower (op, t)))
  | AssertIterableT ({ use_op; _ } as contents) ->
    util use_op (fun use_op -> AssertIterableT { contents with use_op })
  | MakeExactT (_, _)
  | TestPropT (_, _, _, _)
  | CallElemT (_, _, _, _)
  | GetStaticsT (_, _)
  | GetProtoT (_, _)
  | SetProtoT (_, _)
  | MixinT (_, _)
  | ComparatorT _
  | UnaryMinusT (_, _)
  | AssertArithmeticOperandT _
  | AssertBinaryInLHST _
  | AssertBinaryInRHST _
  | AssertForInRHST _
  | PredicateT (_, _)
  | GuardT (_, _, _)
  | StrictEqT _
  | EqT _
  | AndT (_, _, _)
  | OrT (_, _, _)
  | NullishCoalesceT (_, _, _)
  | NotT (_, _)
  | ThisSpecializeT (_, _, _)
  | VarianceCheckT (_, _, _, _)
  | LookupT _
  | ObjRestT (_, _, _, _)
  | ObjSealT (_, _)
  | ObjTestProtoT (_, _)
  | ObjTestT (_, _, _)
  | UnifyT (_, _)
  | BecomeT { reason = _; t = _; empty_success = _ }
  | GetValuesT (_, _)
  | CJSRequireT (_, _, _)
  | ImportModuleNsT (_, _, _)
  | ImportDefaultT (_, _, _, _, _)
  | ImportNamedT (_, _, _, _, _, _)
  | ImportTypeT (_, _, _)
  | ImportTypeofT (_, _, _)
  | AssertImportIsValueT (_, _)
  | CJSExtractNamedExportsT (_, _, _)
  | CopyNamedExportsT (_, _, _)
  | CopyTypeExportsT (_, _, _)
  | ExportNamedT (_, _, _, _)
  | ExportTypeT (_, _, _, _)
  | AssertExportIsTypeT (_, _, _)
  | ChoiceKitUseT (_, _)
  | IntersectionPreprocessKitT (_, _)
  | DebugPrintT _
  | DebugSleepT _
  | SentinelPropTestT (_, _, _, _, _, _)
  | IdxUnwrap (_, _)
  | IdxUnMaybeifyT (_, _)
  | OptionalChainT _
  | InvariantT _
  | CallLatentPredT (_, _, _, _, _)
  | CallOpenPredT (_, _, _, _, _)
  | SubstOnPredT (_, _, _, _)
  | RefineT (_, _, _)
  | CondT (_, _, _, _)
  | ReactPropsToOut _
  | ReactInToProps _
  | DestructuringT _
  | ModuleExportsAssignT _
  | CreateObjWithComputedPropT _
  | ResolveUnionT _
  | EnumExhaustiveCheckT _
  | SealGenericT _ ->
    nope u

let use_op_of_use_t = util_use_op_of_use_t (fun _ -> None) (fun _ op _ -> Some op)

let mod_use_op_of_use_t f =
  util_use_op_of_use_t
    (fun u -> u)
    (fun u op make ->
      let op' = f op in
      if op' == op then
        u
      else
        make op')

let rec mod_root_of_use_op f = function
  | Op op -> Op (f op)
  | Frame (fr, o) -> Frame (fr, mod_root_of_use_op f o)

let rec mod_loc_of_virtual_use_op f =
  let mod_reason = Reason.map_reason_locs f in
  let mod_loc_of_root_use_op f = function
    | InitField { op; body } -> InitField { op = mod_reason op; body = mod_reason body }
    | ObjectSpread { op } -> ObjectSpread { op = mod_reason op }
    | ObjectChain { op } -> ObjectChain { op = mod_reason op }
    | Addition { op; left; right } ->
      Addition { op = mod_reason op; left = mod_reason left; right = mod_reason right }
    | AssignVar { var; init } ->
      AssignVar { var = Base.Option.map ~f:mod_reason var; init = mod_reason init }
    | Cast { lower; upper } -> Cast { lower = mod_reason lower; upper = mod_reason upper }
    | ClassExtendsCheck { def; name; extends } ->
      ClassExtendsCheck
        { def = mod_reason def; name = mod_reason name; extends = mod_reason extends }
    | ClassMethodDefinition { def; name } ->
      ClassMethodDefinition { def = mod_reason def; name = mod_reason name }
    | ClassImplementsCheck { def; name; implements } ->
      ClassImplementsCheck
        { def = mod_reason def; name = mod_reason name; implements = mod_reason implements }
    | ClassOwnProtoCheck { own_loc; proto_loc; prop } ->
      ClassOwnProtoCheck
        { prop; own_loc = Base.Option.map ~f own_loc; proto_loc = Base.Option.map ~f proto_loc }
    | Coercion { from; target } -> Coercion { from = mod_reason from; target = mod_reason target }
    | DeleteProperty { lhs; prop } ->
      DeleteProperty { lhs = mod_reason lhs; prop = mod_reason prop }
    | DeleteVar { var } -> DeleteVar { var = mod_reason var }
    | FunCall { op; fn; args; local } ->
      FunCall
        { local; op = mod_reason op; fn = mod_reason fn; args = Base.List.map ~f:mod_reason args }
    | FunCallMethod { op; fn; args; prop; local } ->
      FunCallMethod
        {
          local;
          op = mod_reason op;
          fn = mod_reason fn;
          prop = mod_reason prop;
          args = Base.List.map ~f:mod_reason args;
        }
    | FunReturnStatement { value } -> FunReturnStatement { value = mod_reason value }
    | FunImplicitReturn { fn; upper } ->
      FunImplicitReturn { fn = mod_reason fn; upper = mod_reason upper }
    | GeneratorYield { value } -> GeneratorYield { value = mod_reason value }
    | GetProperty reason -> GetProperty (mod_reason reason)
    | IndexedTypeAccess { _object; index } ->
      IndexedTypeAccess { _object = mod_reason _object; index = mod_reason index }
    | Internal o -> Internal o
    | JSXCreateElement { op; component } ->
      JSXCreateElement { op = mod_reason op; component = mod_reason component }
    | ReactCreateElementCall { op; component; children } ->
      ReactCreateElementCall
        { op = mod_reason op; component = mod_reason component; children = f children }
    | ReactGetIntrinsic { literal } -> ReactGetIntrinsic { literal = mod_reason literal }
    | Speculation op -> Speculation (mod_loc_of_virtual_use_op f op)
    | TypeApplication { type' } -> TypeApplication { type' = mod_reason type' }
    | SetProperty { lhs; prop; value } ->
      SetProperty { lhs = mod_reason lhs; prop = mod_reason prop; value = mod_reason value }
    | UpdateProperty { lhs; prop } ->
      UpdateProperty { lhs = mod_reason lhs; prop = mod_reason prop }
    | SwitchCheck { case_test; switch_discriminant } ->
      SwitchCheck
        { case_test = mod_reason case_test; switch_discriminant = mod_reason switch_discriminant }
    | MatchingProp { op; obj; key; sentinel_reason } ->
      MatchingProp
        {
          op = mod_reason op;
          obj = mod_reason obj;
          key;
          sentinel_reason = mod_reason sentinel_reason;
        }
    | UnknownUse -> UnknownUse
  in
  let mod_loc_of_frame_use_op = function
    | ArrayElementCompatibility { lower; upper } ->
      ArrayElementCompatibility { lower = mod_reason lower; upper = mod_reason upper }
    | FunCompatibility { lower; upper } ->
      FunCompatibility { lower = mod_reason lower; upper = mod_reason upper }
    | FunMissingArg { n; op; def } -> FunMissingArg { n; op = mod_reason op; def = mod_reason def }
    | FunParam { n; name; lower; upper } ->
      FunParam { n; name; lower = mod_reason lower; upper = mod_reason upper }
    | FunRestParam { lower; upper } ->
      FunRestParam { lower = mod_reason lower; upper = mod_reason upper }
    | FunReturn { lower; upper } -> FunReturn { lower = mod_reason lower; upper = mod_reason upper }
    | ImplicitTypeParam -> ImplicitTypeParam
    | IndexerKeyCompatibility { lower; upper } ->
      IndexerKeyCompatibility { lower = mod_reason lower; upper = mod_reason upper }
    | CallFunCompatibility { n } -> CallFunCompatibility { n }
    | TupleMapFunCompatibility { value } -> TupleMapFunCompatibility { value = mod_reason value }
    | ObjMapFunCompatibility { value } -> ObjMapFunCompatibility { value = mod_reason value }
    | ObjMapiFunCompatibility { key; value } ->
      ObjMapiFunCompatibility { key = mod_reason key; value = mod_reason value }
    | PropertyCompatibility { prop; lower; upper } ->
      PropertyCompatibility { prop; lower = mod_reason lower; upper = mod_reason upper }
    | ReactConfigCheck -> ReactConfigCheck
    | ReactGetConfig o -> ReactGetConfig o
    | TupleElementCompatibility { n; lower; upper } ->
      TupleElementCompatibility { n; lower = mod_reason lower; upper = mod_reason upper }
    | TypeArgCompatibility { name; targ; lower; upper; polarity } ->
      TypeArgCompatibility
        {
          name;
          polarity;
          targ = mod_reason targ;
          lower = mod_reason lower;
          upper = mod_reason upper;
        }
    | TypeParamBound o -> TypeParamBound o
    | UnifyFlip -> UnifyFlip
  in
  function
  | Op op -> Op (mod_loc_of_root_use_op f op)
  | Frame (fr, o) -> Frame (mod_loc_of_frame_use_op fr, mod_loc_of_virtual_use_op f o)

(* type comparison mod reason *)
let reasonless_compare =
  let rec swap_reason t2 t1 =
    match (t2, t1) with
    (* In reposition we also recurse and reposition some nested types. We need
     * to make sure we swap the types for these reasons as well. Otherwise our
     * optimized union ~> union check will not pass. *)
    | (MaybeT (_, t2), MaybeT (r, t1)) -> MaybeT (r, swap_reason t2 t1)
    | ( OptionalT { reason = _; type_ = t2; use_desc = _ },
        OptionalT { reason; type_ = t1; use_desc } ) ->
      OptionalT { reason; type_ = swap_reason t2 t1; use_desc }
    | (ExactT (_, t2), ExactT (r, t1)) -> ExactT (r, swap_reason t2 t1)
    | _ -> mod_reason_of_t (fun _ -> reason_of_t t1) t2
  in
  fun t1 t2 ->
    if t1 == t2 then
      0
    else
      compare t1 (swap_reason t2 t1)

let reasonless_eq t1 t2 = reasonless_compare t1 t2 = 0

let literal_eq x = function
  | Literal (_, y) -> x = y
  | Truthy -> false
  | AnyLiteral -> false

let number_literal_eq (x, _) = function
  | Literal (_, (y, _)) -> x = y
  | Truthy -> false
  | AnyLiteral -> false

let boolean_literal_eq x = function
  | Some y -> x = y
  | None -> false

let trust_subtype_fixed tr1 tr2 =
  match (Trust.expand tr1, Trust.expand tr2) with
  | (Trust.QualifiedTrust trust1, Trust.QualifiedTrust trust2) -> Trust.subtype_trust trust1 trust2
  | _ -> false

let quick_subtype trust_checked t1 t2 =
  Trust.(
    match (t1, t2) with
    | (DefT (_, ltrust, NumT _), DefT (_, rtrust, NumT _))
    | (DefT (_, ltrust, SingletonNumT _), DefT (_, rtrust, NumT _))
    | (DefT (_, ltrust, StrT _), DefT (_, rtrust, StrT _))
    | (DefT (_, ltrust, SingletonStrT _), DefT (_, rtrust, StrT _))
    | (DefT (_, ltrust, BoolT _), DefT (_, rtrust, BoolT _))
    | (DefT (_, ltrust, SingletonBoolT _), DefT (_, rtrust, BoolT _))
    | (DefT (_, ltrust, NullT), DefT (_, rtrust, NullT))
    | (DefT (_, ltrust, VoidT), DefT (_, rtrust, VoidT))
    | (DefT (_, ltrust, SymbolT), DefT (_, rtrust, SymbolT))
    | (DefT (_, ltrust, EmptyT), DefT (_, rtrust, _))
    | (DefT (_, ltrust, _), DefT (_, rtrust, MixedT _)) ->
      (not trust_checked) || trust_subtype_fixed ltrust rtrust
    | (DefT (_, ltrust, EmptyT), _) ->
      (not trust_checked) || trust_value_map ~f:is_public ~default:false ltrust
    | (_, DefT (_, rtrust, MixedT _)) ->
      (not trust_checked) || trust_value_map ~f:is_tainted ~default:false rtrust
    | (DefT (_, ltrust, StrT actual), DefT (_, rtrust, SingletonStrT expected)) ->
      ((not trust_checked) || trust_subtype_fixed ltrust rtrust) && literal_eq expected actual
    | (DefT (_, ltrust, NumT actual), DefT (_, rtrust, SingletonNumT expected)) ->
      ((not trust_checked) || trust_subtype_fixed ltrust rtrust)
      && number_literal_eq expected actual
    | (DefT (_, ltrust, BoolT actual), DefT (_, rtrust, SingletonBoolT expected)) ->
      ((not trust_checked) || trust_subtype_fixed ltrust rtrust)
      && boolean_literal_eq expected actual
    | (DefT (_, _, ObjT { flags = { obj_kind = Exact; _ }; _ }), ExactT (_, t2')) ->
      reasonless_eq t1 t2'
    | _ -> reasonless_eq t1 t2)

let reason_of_propref = function
  | Named (r, _) -> r
  | Computed t -> reason_of_t t

and tuple_length reason trust ts =
  let r =
    let desc = RTupleLength (List.length ts) in
    replace_desc_reason desc reason
  in
  let t =
    let n = List.length ts in
    let float = Base.Float.of_int n in
    let string = Base.Int.to_string n in
    SingletonNumT (float, string)
  in
  DefT (r, trust, t)

let optional ?annot_loc ?(use_desc = false) t =
  let reason = update_desc_new_reason (fun desc -> ROptional desc) (reason_of_t t) in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  OptionalT { reason; type_ = t; use_desc }

let maybe t =
  let reason = update_desc_new_reason (fun desc -> RMaybe desc) (reason_of_t t) in
  MaybeT (reason, t)

let exact t = ExactT (reason_of_t t, t)

let class_type ?(structural = false) ?annot_loc t =
  let reason =
    if structural then
      reason_of_t t
    else
      update_desc_new_reason (fun desc -> RClass desc) (reason_of_t t)
  in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  DefT (reason, bogus_trust (), ClassT t)

let this_class_type t is_this =
  let reason = update_desc_new_reason (fun desc -> RClass desc) (reason_of_t t) in
  ThisClassT (reason, t, is_this)

let extends_type r l u =
  let reason = update_desc_reason (fun desc -> RExtends desc) r in
  InternalT (ExtendsT (reason, l, u))

let extends_use_type use_op l u =
  let reason = update_desc_new_reason (fun desc -> RExtends desc) (reason_of_t u) in
  ExtendsUseT (use_op, reason, [], l, u)

let poly_type id tparams_loc tparams t =
  let reason = update_desc_new_reason (fun desc -> RPolyType desc) (reason_of_t t) in
  let reason = annot_reason ~annot_loc:(aloc_of_reason reason) reason in
  DefT (reason, bogus_trust (), PolyT { tparams_loc; tparams; t_out = t; id })

let poly_type_of_tparam_list id tparams_loc tparams t =
  match tparams with
  | [] -> t
  | hd :: tl ->
    let tparams_nel = (hd, tl) in
    poly_type id tparams_loc tparams_nel t

let poly_type_of_tparams id tparams t =
  match tparams with
  | None -> t
  | Some (tparams_loc, tparams_nel) -> poly_type id tparams_loc tparams_nel t

let typeapp reason t targs =
  let reason = replace_desc_reason (RTypeApp (desc_of_t t)) reason in
  let use_op = Op (TypeApplication { type' = reason }) in
  TypeAppT (reason, use_op, t, targs)

let typeapp_annot loc t targs =
  let desc = RTypeApp (desc_of_t t) in
  let reason = mk_annot_reason desc loc in
  let use_op = Op (TypeApplication { type' = reason }) in
  TypeAppT (reason, use_op, t, targs)

(* An implicit typeapp is not a product of some source level type application,
 * but merely a tool for some other functionality, e.g.
 * canonicalize_imported_type in flow_js.ml. *)
let implicit_typeapp ?annot_loc t targs =
  let reason = update_desc_new_reason (fun desc -> RTypeAppImplicit desc) (reason_of_t t) in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  let use_op = Op (TypeApplication { type' = reason }) in
  TypeAppT (reason, use_op, t, targs)

let this_typeapp ?annot_loc t this targs =
  let reason =
    match targs with
    | Some _ -> update_desc_new_reason (fun desc -> RTypeApp desc) (reason_of_t t)
    | None -> reason_of_t t
  in
  let reason =
    match annot_loc with
    | Some annot_loc -> annot_reason ~annot_loc @@ repos_reason annot_loc reason
    | None -> reason
  in
  ThisTypeAppT (reason, t, this, targs)

let push_type_alias_reason r t =
  match desc_of_reason ~unwrap:false r with
  | RTypeAlias (n, _, _) ->
    mod_reason_of_t (update_desc_reason (fun desc -> RTypeAlias (n, None, desc))) t
  | _ -> t

let rec eq_predicate (p1, p2) =
  match (p1, p2) with
  (* trivial *)
  | (ExistsP _, ExistsP _)
  | (NullP, NullP)
  | (MaybeP, MaybeP)
  | (BoolP _, BoolP _)
  | (FunP, FunP)
  | (NumP _, NumP _)
  | (ObjP, ObjP)
  | (StrP _, StrP _)
  | (SymbolP _, SymbolP _)
  | (VoidP, VoidP)
  | (ArrP, ArrP) ->
    true
  (* Recursive *)
  | (AndP (p1a, p1b), AndP (p2a, p2b)) -> eq_predicate (p1a, p2a) && eq_predicate (p1b, p2b)
  | (OrP (p1a, p1b), OrP (p2a, p2b)) -> eq_predicate (p1a, p2a) && eq_predicate (p1b, p2b)
  | (NotP p1, NotP p2) -> eq_predicate (p1, p2)
  | (SingletonBoolP (_, s1), SingletonBoolP (_, s2)) -> s1 = s2
  | (SingletonStrP (_, s1, v1), SingletonStrP (_, s2, v2)) -> s1 = s2 && v1 = v2
  | (SingletonNumP (_, s1, v1), SingletonNumP (_, s2, v2)) -> s1 = s2 && v1 = v2
  | (LatentP (t1, i1), LatentP (t2, i2)) -> t1 == t2 && i1 = i2
  | (PropExistsP (s1, _), PropExistsP (s2, _)) -> s1 = s2
  | (PropNonMaybeP (s1, _), PropNonMaybeP (s2, _)) -> s1 = s2
  (* Complex *)
  | (LeftP _, LeftP _)
  | (RightP _, RightP _) ->
    p1 = p2
  | _ -> false

let pred_map_implies p1 p2 =
  Key_map.for_all
    (fun k2 v2 ->
      match Key_map.find_opt k2 p1 with
      | None -> false
      | Some v1 -> eq_predicate (v1, v2))
    p2

let type_t_of_annotated_or_inferred (x : Type.annotated_or_inferred) =
  match x with
  | Inferred t
  | Annotated t ->
    t

let map_annotated_or_inferred f = function
  | Inferred t -> Inferred (f t)
  | Annotated t -> Annotated (f t)

(* Creates a union from a list of types. Since unions require a minimum of two
   types this function will return an empty type when there are no types in the
   list, or the list head when there is one type in the list. *)
let union_of_ts reason ts =
  match ts with
  (* If we have no types then this is an error. *)
  | [] -> DefT (reason, bogus_trust (), EmptyT)
  (* If we only have one type then only that should be used. *)
  | [t0] -> t0
  (* If we have more than one type then we make a union type. *)
  | t0 :: t1 :: ts -> UnionT (reason, UnionRep.make t0 t1 ts)

let annotated_or_inferred_of_option ~default = function
  | Some t -> Annotated t
  | None -> Inferred default
