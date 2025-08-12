(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  | DefT (reason, _) -> reason
  | EvalT (_, defer_use_t, _) -> reason_of_defer_use_t defer_use_t
  | GenericT { reason; _ } -> reason
  | FunProtoT reason -> reason
  | FunProtoBindT reason -> reason
  | KeysT (reason, _) -> reason
  | StrUtilT { reason; _ } -> reason
  | NamespaceT { values_type; _ } -> reason_of_t values_type
  | NullProtoT reason -> reason
  | ObjProtoT reason -> reason
  | OpaqueT (reason, _) -> reason
  | ThisInstanceT (reason, _, _, _) -> reason
  | ThisTypeAppT (reason, _, _, _) -> reason
  | TypeAppT { reason; _ } -> reason
  | AnyT (reason, _) -> reason
  | UnionT (reason, _) -> reason
  | IntersectionT (reason, _) -> reason
  | MaybeT (reason, _) -> reason
  | OptionalT { reason; type_ = _; use_desc = _ } -> reason

and reason_of_defer_use_t = function
  | TypeDestructorT (_, reason, _) -> reason

and reason_of_use_t = function
  | UseT (_, t) -> reason_of_t t
  | ArrRestT (_, reason, _, _) -> reason
  | BindT (_, reason, _) -> reason
  | CallElemT (_, reason, _, _, _) -> reason
  | CallT { reason; _ } -> reason
  | ConstructorT { reason; _ } -> reason
  | ElemT { reason; _ } -> reason
  | EnumCastT { enum = (reason, _); _ } -> reason
  | EnumExhaustiveCheckT { reason; _ } -> reason
  | GetEnumT { reason; _ } -> reason
  | ConditionalT { reason; _ } -> reason
  | ExtendsUseT (_, reason, _, _, _) -> reason
  | GetElemT { reason; _ } -> reason
  | GetKeysT (reason, _) -> reason
  | GetValuesT (reason, _) -> reason
  | GetDictValuesT (reason, _) -> reason
  | GetTypeFromNamespaceT { reason; _ } -> reason
  | GetPropT { reason; _ } -> reason
  | GetPrivatePropT (_, reason, _, _, _, _) -> reason
  | GetProtoT (reason, _) -> reason
  | GetStaticsT (reason, _) -> reason
  | HasOwnPropT (_, reason, _) -> reason
  | ImplementsT (_, t) -> reason_of_t t
  | ConcretizeT { reason; _ } -> reason
  | LookupT { reason; _ } -> reason
  | MapTypeT (_, reason, _, _) -> reason
  | MethodT (_, reason, _, _, _) -> reason
  | MixinT (reason, _) -> reason
  | ObjRestT (reason, _, _, _) -> reason
  | ObjTestProtoT (reason, _) -> reason
  | ObjTestT (reason, _, _) -> reason
  | OptionalChainT { reason; _ } -> reason
  | OptionalIndexedAccessT { reason; _ } -> reason
  | PrivateMethodT (_, reason, _, _, _, _, _) -> reason
  | ReactKitT (_, reason, _) -> reason
  | ReposLowerT { reason; _ } -> reason
  | ReposUseT (reason, _, _, _) -> reason
  | ResolveSpreadT (_, reason, _) -> reason
  | SetElemT (_, reason, _, _, _, _) -> reason
  | SetPropT (_, reason, _, _, _, _, _) -> reason
  | SetPrivatePropT (_, reason, _, _, _, _, _, _, _) -> reason
  | SetProtoT (reason, _) -> reason
  | SpecializeT (_, _, reason, _, _) -> reason
  | ObjKitT (_, reason, _, _, _) -> reason
  | SuperT (_, reason, _) -> reason
  | TestPropT { reason; _ } -> reason
  | ThisSpecializeT (reason, _, _) -> reason
  | ToStringT { reason; _ } -> reason
  | ValueToTypeReferenceT (_, reason, _, _) -> reason
  | TypeCastT (_, t) -> reason_of_t t
  | FilterOptionalT (_, t) -> reason_of_t t
  | ExtractReactRefT (reason, _) -> reason
  | FilterMaybeT (_, t) -> reason_of_t t
  | DeepReadOnlyT ((r, _), _) -> r
  | HooklikeT (r, _) -> r
  | ConcretizeTypeAppsT (_, _, (_, _, _, _, reason), _) -> reason
  | CondT (reason, _, _, _) -> reason
  | SealGenericT { reason; _ } -> reason
  | DestructuringT (reason, _, _, _, _) -> reason
  | ResolveUnionT { reason; _ } -> reason
  | CheckUnusedPromiseT { reason; _ } -> reason
  | WriteComputedObjPropCheckT { reason; _ } -> reason
  | CheckReactImmutableT { lower_reason = reason; _ } -> reason
  | ConvertEmptyPropsToMixedT (reason, _) -> reason
  | ExitRendersT { renders_reason; _ } -> renders_reason
  | EvalTypeDestructorT { reason; _ } -> reason

(* When the type is a singleton and not from annotation, convert it to literal reason. *)
let singleton_reason_of_t = function
  | DefT (reason, SingletonStrT { value; from_annot }) when from_annot = false ->
    let loc = Reason.loc_of_reason reason in
    Reason.mk_reason (Reason.RStringLit value) loc
  | DefT (reason, SingletonNumT { value; from_annot }) when from_annot = false ->
    let loc = Reason.loc_of_reason reason in
    Reason.mk_reason (Reason.RNumberLit (snd value)) loc
  | DefT (reason, SingletonBoolT { value; from_annot }) when from_annot = false ->
    let loc = Reason.loc_of_reason reason in
    Reason.mk_reason (Reason.RBooleanLit value) loc
  | DefT (reason, SingletonBigIntT { value; from_annot }) when from_annot = false ->
    let loc = Reason.loc_of_reason reason in
    Reason.mk_reason (Reason.RBigIntLit (snd value)) loc
  | t -> reason_of_t t

let generalized_reason_of_t =
  let not_a_string = function
    | DefT (_, SingletonStrT _)
    | DefT (_, StrGeneralT _) ->
      false
    | _ -> true
  in
  let not_a_number = function
    | DefT (_, SingletonNumT _)
    | DefT (_, NumGeneralT _) ->
      false
    | _ -> true
  in
  let not_a_boolean = function
    | DefT (_, SingletonBoolT _)
    | DefT (_, BoolGeneralT) ->
      false
    | _ -> true
  in
  let not_a_bigint = function
    | DefT (_, SingletonBigIntT _)
    | DefT (_, BigIntGeneralT _) ->
      false
    | _ -> true
  in
  fun ~compared_with_t:u l ->
    match l with
    | DefT (r, SingletonStrT { from_annot = false; _ }) when not_a_string u ->
      replace_desc_reason RString r
    | DefT (r, SingletonNumT { from_annot = false; _ }) when not_a_number u ->
      replace_desc_reason RNumber r
    | DefT (r, SingletonBoolT { from_annot = false; _ }) when not_a_boolean u ->
      replace_desc_reason RBoolean r
    | DefT (r, SingletonBigIntT { from_annot = false; _ }) when not_a_bigint u ->
      replace_desc_reason RBigInt r
    | l -> reason_of_t l

let desc_of_t = reason_of_t %> desc_of_reason

let loc_of_t = reason_of_t %> loc_of_reason

let def_loc_of_t = reason_of_t %> def_loc_of_reason

(* TODO make a type visitor *)
let rec mod_reason_of_t f = function
  | OpenT (reason, id) -> OpenT (f reason, id)
  | AnnotT (reason, t, use_desc) -> AnnotT (f reason, t, use_desc)
  | DefT (reason, t) -> DefT (f reason, t)
  | AnyT (reason, src) -> AnyT (f reason, src)
  | UnionT (reason, src) -> UnionT (f reason, src)
  | IntersectionT (reason, src) -> IntersectionT (f reason, src)
  | MaybeT (reason, src) -> MaybeT (f reason, src)
  | OptionalT { reason; type_; use_desc } -> OptionalT { reason = f reason; type_; use_desc }
  | EvalT (t, defer_use_t, id) -> EvalT (t, mod_reason_of_defer_use_t f defer_use_t, id)
  | GenericT ({ reason; _ } as generic) -> GenericT { generic with reason = f reason }
  | FunProtoT reason -> FunProtoT (f reason)
  | FunProtoBindT reason -> FunProtoBindT (f reason)
  | KeysT (reason, t) -> KeysT (f reason, t)
  | StrUtilT { reason; op; remainder } -> StrUtilT { reason = f reason; op; remainder }
  | NamespaceT { namespace_symbol; values_type; types_tmap } ->
    NamespaceT { namespace_symbol; values_type = mod_reason_of_t f values_type; types_tmap }
  | NullProtoT reason -> NullProtoT (f reason)
  | ObjProtoT reason -> ObjProtoT (f reason)
  | OpaqueT (reason, opaquetype) -> OpaqueT (f reason, opaquetype)
  | ThisInstanceT (reason, t, is_this, this_name) -> ThisInstanceT (f reason, t, is_this, this_name)
  | ThisTypeAppT (reason, t1, t2, t3) -> ThisTypeAppT (f reason, t1, t2, t3)
  | TypeAppT { reason; use_op; type_; targs; from_value; use_desc } ->
    TypeAppT { reason = f reason; use_op; type_; targs; from_value; use_desc }

and mod_reason_of_defer_use_t f = function
  | TypeDestructorT (use_op, reason, s) -> TypeDestructorT (use_op, f reason, s)

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
  | BindT (op, r, f) -> util op (fun op -> BindT (op, r, f))
  | ConditionalT
      { use_op; reason; distributive_tparam_name; infer_tparams; extends_t; true_t; false_t; tout }
    ->
    util use_op (fun use_op ->
        ConditionalT
          {
            use_op;
            reason;
            distributive_tparam_name;
            infer_tparams;
            extends_t;
            true_t;
            false_t;
            tout;
          }
    )
  | CallT { use_op; reason; call_action; return_hint } ->
    util use_op (fun use_op -> CallT { use_op; reason; call_action; return_hint })
  | MethodT (op, r1, r2, p, f) -> util op (fun op -> MethodT (op, r1, r2, p, f))
  | PrivateMethodT (op, r1, r2, x, c, s, a) ->
    util op (fun op -> PrivateMethodT (op, r1, r2, x, c, s, a))
  | SetPropT (op, r, p, m, w, t, tp) -> util op (fun op -> SetPropT (op, r, p, m, w, t, tp))
  | SetPrivatePropT (op, r, s, m, c, b, x, t, tp) ->
    util op (fun op -> SetPrivatePropT (op, r, s, m, c, b, x, t, tp))
  | GetTypeFromNamespaceT { use_op; reason; prop_ref; tout } ->
    util use_op (fun use_op -> GetTypeFromNamespaceT { use_op; reason; prop_ref; tout })
  | GetPropT ({ use_op; _ } as x) -> util use_op (fun use_op -> GetPropT { x with use_op })
  | TestPropT ({ use_op; _ } as x) -> util use_op (fun use_op -> TestPropT { x with use_op })
  | GetPrivatePropT (op, r, s, c, b, t) -> util op (fun op -> GetPrivatePropT (op, r, s, c, b, t))
  | SetElemT (op, r, t1, m, t2, t3) -> util op (fun op -> SetElemT (op, r, t1, m, t2, t3))
  | GetElemT ({ use_op; _ } as x) -> util use_op (fun use_op -> GetElemT { x with use_op })
  | OptionalIndexedAccessT ({ use_op; _ } as x) ->
    util use_op (fun use_op -> OptionalIndexedAccessT { x with use_op })
  | ReposLowerT { reason; use_desc; use_t } ->
    nested_util use_t (fun use_t -> ReposLowerT { reason; use_desc; use_t })
  | ReposUseT (r, d, op, t) -> util op (fun op -> ReposUseT (r, d, op, t))
  | ConstructorT { use_op; reason; targs; args; tout; return_hint; specialized_ctor } ->
    util use_op (fun use_op ->
        ConstructorT { use_op; reason; targs; args; tout; return_hint; specialized_ctor }
    )
  | SuperT (op, r, i) -> util op (fun op -> SuperT (op, r, i))
  | ImplementsT (op, t) -> util op (fun op -> ImplementsT (op, t))
  | ToStringT { orig_t; reason; t_out } ->
    nested_util t_out (fun t_out -> ToStringT { orig_t; reason; t_out })
  | SpecializeT (op, r1, r2, ts, t) -> util op (fun op -> SpecializeT (op, r1, r2, ts, t))
  | TypeCastT (op, t) -> util op (fun op -> TypeCastT (op, t))
  | EnumCastT { use_op; enum } -> util use_op (fun use_op -> EnumCastT { use_op; enum })
  | FilterOptionalT (op, t) -> util op (fun op -> FilterOptionalT (op, t))
  | FilterMaybeT (op, t) -> util op (fun op -> FilterMaybeT (op, t))
  | ConcretizeTypeAppsT (u, (ts1, b1, op, r1), x2, b2) ->
    util op (fun op -> ConcretizeTypeAppsT (u, (ts1, b1, op, r1), x2, b2))
  | ArrRestT (op, r, i, t) -> util op (fun op -> ArrRestT (op, r, i, t))
  | HasOwnPropT (op, r, t) -> util op (fun op -> HasOwnPropT (op, r, t))
  | GetKeysT (r, u2) -> nested_util u2 (fun u2 -> GetKeysT (r, u2))
  | GetDictValuesT (r, u2) -> nested_util u2 (fun u2 -> GetDictValuesT (r, u2))
  | ElemT { use_op; reason; obj; action } ->
    util use_op (fun use_op -> ElemT { use_op; reason; obj; action })
  | ObjKitT (op, r, x, y, t) -> util op (fun op -> ObjKitT (op, r, x, y, t))
  | ReactKitT (op, r, t) -> util op (fun op -> ReactKitT (op, r, t))
  | ResolveSpreadT (op, r, s) -> util op (fun op -> ResolveSpreadT (op, r, s))
  | ExtendsUseT (op, r, ts, a, b) -> util op (fun op -> ExtendsUseT (op, r, ts, a, b))
  | MapTypeT (op, r, k, t) -> util op (fun op -> MapTypeT (op, r, k, t))
  | ValueToTypeReferenceT (use_op, reason, kind, t) ->
    util use_op (fun use_op -> ValueToTypeReferenceT (use_op, reason, kind, t))
  | GetEnumT ({ use_op; _ } as x) -> util use_op (fun use_op -> GetEnumT { x with use_op })
  | CheckReactImmutableT ({ use_op; _ } as x) ->
    util use_op (fun use_op -> CheckReactImmutableT { x with use_op })
  | CallElemT (_, _, _, _, _)
  | GetStaticsT (_, _)
  | GetProtoT (_, _)
  | SetProtoT (_, _)
  | MixinT (_, _)
  | ConvertEmptyPropsToMixedT _
  | DeepReadOnlyT _
  | HooklikeT _
  | ThisSpecializeT (_, _, _)
  | LookupT _
  | ObjRestT (_, _, _, _)
  | ObjTestProtoT (_, _)
  | ObjTestT (_, _, _)
  | GetValuesT (_, _)
  | ConcretizeT _
  | ExtractReactRefT _
  | OptionalChainT _
  | CondT (_, _, _, _)
  | DestructuringT _
  | ResolveUnionT _
  | ExitRendersT _
  | EnumExhaustiveCheckT _
  | SealGenericT _
  | CheckUnusedPromiseT _
  | WriteComputedObjPropCheckT _
  | EvalTypeDestructorT _ ->
    nope u

let is_in_common_interface_conformance_check use_op =
  match root_of_use_op use_op with
  | ConformToCommonInterface _ -> true
  | _ -> false

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
    | ObjectAddComputedProperty { op } -> ObjectAddComputedProperty { op = mod_reason op }
    | ObjectSpread { op } -> ObjectSpread { op = mod_reason op }
    | ObjectRest { op } -> ObjectRest { op = mod_reason op }
    | ObjectChain { op } -> ObjectChain { op = mod_reason op }
    | AssignVar { var; init } ->
      AssignVar { var = Base.Option.map ~f:mod_reason var; init = mod_reason init }
    | Cast { lower; upper } -> Cast { lower = mod_reason lower; upper = mod_reason upper }
    | ClassExtendsCheck { def; extends } ->
      ClassExtendsCheck { def = mod_reason def; extends = mod_reason extends }
    | ClassMethodDefinition { def; name } ->
      ClassMethodDefinition { def = mod_reason def; name = mod_reason name }
    | ClassImplementsCheck { def; name; implements } ->
      ClassImplementsCheck
        { def = mod_reason def; name = mod_reason name; implements = mod_reason implements }
    | ClassOwnProtoCheck { own_loc; proto_loc; prop } ->
      ClassOwnProtoCheck
        { prop; own_loc = Base.Option.map ~f own_loc; proto_loc = Base.Option.map ~f proto_loc }
    | Coercion { from; target } -> Coercion { from = mod_reason from; target = mod_reason target }
    | ConformToCommonInterface { self_sig_loc; self_module_loc; originate_from_import } ->
      ConformToCommonInterface
        {
          self_sig_loc = f self_sig_loc;
          self_module_loc = f self_module_loc;
          originate_from_import;
        }
    | DeclareComponentRef { op } -> DeclareComponentRef { op = mod_reason op }
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
    | FunImplicitReturn { fn; upper; type_guard } ->
      FunImplicitReturn { fn = mod_reason fn; upper = mod_reason upper; type_guard }
    | GeneratorYield { value } -> GeneratorYield { value = mod_reason value }
    | GetProperty reason -> GetProperty (mod_reason reason)
    | IndexedTypeAccess { _object; index } ->
      IndexedTypeAccess { _object = mod_reason _object; index = mod_reason index }
    | InferBoundCompatibilityCheck { bound; infer } ->
      InferBoundCompatibilityCheck { bound = mod_reason bound; infer = mod_reason infer }
    | JSXCreateElement { op; component } ->
      JSXCreateElement { op = mod_reason op; component = mod_reason component }
    | ReactCreateElementCall { op; component; children } ->
      ReactCreateElementCall
        { op = mod_reason op; component = mod_reason component; children = f children }
    | ReactGetIntrinsic { literal } -> ReactGetIntrinsic { literal = mod_reason literal }
    | Speculation op -> Speculation (mod_loc_of_virtual_use_op f op)
    | TypeApplication { type_ } -> TypeApplication { type_ = mod_reason type_ }
    | SetProperty { lhs; prop; value } ->
      SetProperty { lhs = mod_reason lhs; prop = mod_reason prop; value = mod_reason value }
    | UpdateProperty { lhs; prop } ->
      UpdateProperty { lhs = mod_reason lhs; prop = mod_reason prop }
    | RefinementCheck { test; discriminant } ->
      RefinementCheck { test = mod_reason test; discriminant = mod_reason discriminant }
    | SwitchRefinementCheck { test; discriminant } ->
      SwitchRefinementCheck { test = f test; discriminant = f discriminant }
    | MatchingProp { op; obj; key; sentinel_reason } ->
      MatchingProp
        {
          op = mod_reason op;
          obj = mod_reason obj;
          key;
          sentinel_reason = mod_reason sentinel_reason;
        }
    | EvalMappedType { mapped_type } -> EvalMappedType { mapped_type = mod_reason mapped_type }
    | TypeGuardIncompatibility { guard_type; param_name } ->
      TypeGuardIncompatibility { guard_type = mod_reason guard_type; param_name }
    | RenderTypeInstantiation { render_type } ->
      RenderTypeInstantiation { render_type = mod_reason render_type }
    | ComponentRestParamCompatibility { rest_param } ->
      ComponentRestParamCompatibility { rest_param = mod_reason rest_param }
    | PositiveTypeGuardConsistency
        { reason; return_reason; param_reason; guard_type_reason; is_return_false_statement } ->
      PositiveTypeGuardConsistency
        {
          reason = mod_reason reason;
          return_reason = mod_reason return_reason;
          param_reason = mod_reason param_reason;
          guard_type_reason = mod_reason guard_type_reason;
          is_return_false_statement;
        }
    | UnknownUse -> UnknownUse
  in
  let mod_loc_of_frame_use_op = function
    | ConstrainedAssignment { name; declaration; providers } ->
      ConstrainedAssignment { name; declaration = f declaration; providers = List.map f providers }
    | ReactDeepReadOnly (loc, l) -> ReactDeepReadOnly (f loc, l)
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
    | OpaqueTypeLowerBoundCompatibility { lower; upper } ->
      OpaqueTypeLowerBoundCompatibility { lower = mod_reason lower; upper = mod_reason upper }
    | OpaqueTypeUpperBoundCompatibility { lower; upper } ->
      OpaqueTypeUpperBoundCompatibility { lower = mod_reason lower; upper = mod_reason upper }
    | MappedTypeKeyCompatibility { source_type; mapped_type } ->
      MappedTypeKeyCompatibility
        { source_type = mod_reason source_type; mapped_type = mod_reason mapped_type }
    | PropertyCompatibility { prop; lower; upper } ->
      PropertyCompatibility { prop; lower = mod_reason lower; upper = mod_reason upper }
    | ReactConfigCheck -> ReactConfigCheck
    | ReactGetConfig o -> ReactGetConfig o
    | TupleElementCompatibility { n; lower; upper; lower_optional; upper_optional } ->
      TupleElementCompatibility
        { n; lower = mod_reason lower; upper = mod_reason upper; lower_optional; upper_optional }
    | TupleAssignment { upper_optional } -> TupleAssignment { upper_optional }
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
    | OpaqueTypeLowerBound { opaque_t_reason } ->
      OpaqueTypeLowerBound { opaque_t_reason = mod_reason opaque_t_reason }
    | OpaqueTypeUpperBound { opaque_t_reason } ->
      OpaqueTypeUpperBound { opaque_t_reason = mod_reason opaque_t_reason }
    | TypeGuardCompatibility -> TypeGuardCompatibility
    | RendersCompatibility -> RendersCompatibility
    | UnifyFlip -> UnifyFlip
    | EnumRepresentationTypeCompatibility { lower; upper } ->
      EnumRepresentationTypeCompatibility { lower = mod_reason lower; upper = mod_reason upper }
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
        OptionalT { reason; type_ = t1; use_desc }
      ) ->
      OptionalT { reason; type_ = swap_reason t2 t1; use_desc }
    | _ -> mod_reason_of_t (fun _ -> reason_of_t t1) t2
  in
  fun t1 t2 ->
    if t1 == t2 then
      0
    else
      compare t1 (swap_reason t2 t1)

let reasonless_eq t1 t2 = reasonless_compare t1 t2 = 0

(* type exemplar set - reasons are not considered in compare *)
module TypeExSet = Flow_set.Make (struct
  include Type

  let compare = reasonless_compare
end)

let nominal_id_have_same_logical_module
    ~file_options
    ~projects_options:_
    ((a_id, a_name) : ALoc.id * string option)
    ((b_id, b_name) : ALoc.id * string option) =
  let haste_name_opt = Files.haste_name_opt ~options:file_options in
  let matching_platform_specific_impl_and_interface_file_key a_src b_src =
    Files.has_flow_ext a_src
    && (* A.js.flow, A.ios.js in the same directory *)
    Files.chop_flow_ext a_src = Files.chop_platform_suffix_for_file ~options:file_options b_src
    || (* Regardless of which namespace the Haste module has or what platform they have, if they have
        * the same name, we assume it's the same logical module. It's impossible to happen in normal
        * circumstances due to uniqueness guarantee. It's only possible to happen during multiplatform
        * conformance check, but in this case we already enforced uniqueness guarantee elsewhere. *)
    a_src <> b_src
    &&
    match (haste_name_opt a_src, haste_name_opt b_src) with
    | (Some n1, Some n2) ->
      Files.chop_platform_suffix_for_file ~options:file_options (File_key.SourceFile (n1 ^ ".js"))
      = Files.chop_platform_suffix_for_file ~options:file_options (File_key.SourceFile (n2 ^ ".js"))
    | _ -> false
  in
  match (a_name, b_name, ALoc.source (a_id :> ALoc.t), ALoc.source (b_id :> ALoc.t)) with
  | (Some a_name, Some b_name, Some a_src, Some b_src) ->
    a_name = b_name
    && (matching_platform_specific_impl_and_interface_file_key a_src b_src
       || matching_platform_specific_impl_and_interface_file_key b_src a_src
       )
  | _ -> false

let is_falsy = function
  | DefT
      ( _,
        ( NullT | VoidT
        | SingletonBoolT { value = false; _ }
        | EnumValueT
            ( ConcreteEnum { representation_t = DefT (_, SingletonBoolT { value = false; _ }); _ }
            | AbstractEnum { representation_t = DefT (_, SingletonBoolT { value = false; _ }) } )
        | SingletonStrT { value = OrdinaryName ""; _ }
        | SingletonNumT { value = (0., _); _ } )
      ) ->
    true
  | _ -> false

(* We use this predicate below to defensively prevent some shortcuts taken when
 * we might not have enough information about the type in the LHS. *)
let is_concrete = function
  | OpenT _
  | EvalT _
  | TypeAppT _
  | KeysT _
  | IntersectionT _
  | UnionT _
  | OpaqueT _ ->
    false
  | _ -> true

let is_mixed_subtype l mixed_flavor =
  match (l, mixed_flavor) with
  | (DefT (_, MixedT flavor), _) when flavor = mixed_flavor -> true
  | (OptionalT _, (Mixed_non_maybe | Mixed_non_void))
  | (MaybeT _, (Mixed_non_maybe | Mixed_non_void | Mixed_non_null))
  | (DefT (_, NullT), (Mixed_non_maybe | Mixed_non_null))
  | (DefT (_, VoidT), (Mixed_non_maybe | Mixed_non_void)) ->
    false
  | (l, (Mixed_non_maybe | Mixed_non_null | Mixed_non_void)) -> is_concrete l
  | (DefT (_, FunT _), Mixed_function) -> true
  | (DefT (_, PolyT { t_out = DefT (_, FunT _); _ }), Mixed_function) -> true
  | (_, Mixed_function) -> false
  | (l, Mixed_truthy) -> is_concrete l && not (is_falsy l)
  | (_, Mixed_everything) -> true

let ground_subtype ~on_singleton_eq (l, u) =
  match (l, u) with
  | (OpenT _, _)
  | (_, OpenT _)
  | (UnionT _, _) ->
    false
  | (DefT (_, (NumGeneralT _ | SingletonNumT _)), DefT (_, NumGeneralT _))
  | (DefT (_, (StrGeneralT _ | SingletonStrT _)), DefT (_, StrGeneralT _))
  | (DefT (_, (BoolGeneralT | SingletonBoolT _)), DefT (_, BoolGeneralT))
  | (DefT (_, (BigIntGeneralT _ | SingletonBigIntT _)), DefT (_, BigIntGeneralT _))
  | (DefT (_, SymbolT), DefT (_, SymbolT))
  | (DefT (_, NullT), DefT (_, NullT))
  | (DefT (_, VoidT), DefT (_, VoidT)) ->
    true
  | (DefT (_, SingletonStrT { value = actual; _ }), DefT (_, SingletonStrT { value = expected; _ }))
    ->
    let result = expected = actual in
    if result then on_singleton_eq l;
    result
  | ( DefT (_, SingletonNumT { value = (actual, _); _ }),
      DefT (_, SingletonNumT { value = (expected, _); _ })
    ) ->
    let result = expected = actual in
    if result then on_singleton_eq l;
    result
  | ( DefT (_, SingletonBoolT { value = actual; _ }),
      DefT (_, SingletonBoolT { value = expected; _ })
    ) ->
    let result = expected = actual in
    if result then on_singleton_eq l;
    result
  | ( DefT (_, SingletonBigIntT { value = (actual, _); _ }),
      DefT (_, SingletonBigIntT { value = (expected, _); _ })
    ) ->
    let result = expected = actual in
    if result then on_singleton_eq l;
    result
  | ( StrUtilT { reason = _; op = StrPrefix prefix1; remainder = _ },
      StrUtilT { reason = _; op = StrPrefix prefix2; remainder = None }
    )
    when String.starts_with ~prefix:prefix2 prefix1 ->
    true
  | ( DefT (_, SingletonStrT { value = OrdinaryName s; _ }),
      StrUtilT { reason = _; op = StrPrefix prefix; remainder = None }
    )
    when String.starts_with ~prefix s ->
    on_singleton_eq l;
    true
  | ( StrUtilT { reason = _; op = StrSuffix suffix1; remainder = _ },
      StrUtilT { reason = _; op = StrSuffix suffix2; remainder = None }
    )
    when String.ends_with ~suffix:suffix2 suffix1 ->
    true
  | ( DefT (_, SingletonStrT { value = OrdinaryName s; _ }),
      StrUtilT { reason = _; op = StrSuffix suffix; remainder = None }
    )
    when String.ends_with ~suffix s ->
    on_singleton_eq l;
    true
  | ( StrUtilT { reason = _; op = StrPrefix arg | StrSuffix arg; remainder = _ },
      DefT (_, StrGeneralT Truthy)
    )
    when arg <> "" ->
    true
  | (StrUtilT _, DefT (_, StrGeneralT AnyLiteral))
  | (DefT (_, NumericStrKeyT _), DefT (_, (NumGeneralT _ | StrGeneralT _))) ->
    true
  | (DefT (_, NumericStrKeyT (actual, _)), DefT (_, SingletonNumT { value = (expected, _); _ })) ->
    actual = expected
  | (DefT (_, NumericStrKeyT (_, actual)), DefT (_, SingletonStrT { value = expected; _ })) ->
    OrdinaryName actual = expected
  | (l, DefT (_, MixedT mixed_flavor)) -> is_mixed_subtype l mixed_flavor
  (* we handle the any propagation check later *)
  | (AnyT _, _) -> false
  | (_, AnyT _) -> false
  (* opt: avoid builtin lookups *)
  | (ObjProtoT _, ObjProtoT _)
  | (FunProtoT _, FunProtoT _)
  | (FunProtoT _, ObjProtoT _)
  | (DefT (_, ObjT { proto_t = ObjProtoT _; _ }), ObjProtoT _)
  | (DefT (_, ObjT { proto_t = FunProtoT _; _ }), FunProtoT _)
  | (DefT (_, ObjT { proto_t = FunProtoT _; _ }), ObjProtoT _) ->
    true
  | _ -> false

let ground_subtype_use_t ~on_singleton_eq (l, u) =
  match u with
  | UseT (_, u) -> ground_subtype ~on_singleton_eq (l, u)
  | _ -> false

let quick_subtype ?(on_singleton_eq = (fun _ -> ())) t1 t2 =
  ground_subtype ~on_singleton_eq (t1, t2)
  ||
  match (t1, t2) with
  | (DefT (_, EmptyT), _) -> true
  | (DefT (_, StrGeneralT _), DefT (_, SingletonStrT _))
  | (DefT (_, NumGeneralT _), DefT (_, SingletonNumT _))
  | (DefT (_, BigIntGeneralT _), DefT (_, SingletonBigIntT _))
  | (DefT (_, BoolGeneralT), DefT (_, SingletonBoolT _)) ->
    false
  | _ -> reasonless_eq t1 t2

let reason_of_propref = function
  | Named { reason; _ } -> reason
  | Computed t -> reason_of_t t

let mk_named_prop ~reason ?(from_indexed_access = false) name =
  Named { reason; name; from_indexed_access }

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

let make_exact_object ~reason_obj obj ~reason_op =
  let obj_kind =
    match obj.flags.obj_kind with
    | Inexact -> Exact
    | k -> k
  in
  (* This case analysis aims at recovering a potential type alias associated
   * with an $Exact<> constructor. *)
  let reason_obj =
    match desc_of_reason ~unwrap:false reason_op with
    | RTypeAlias (n, loc, _) ->
      update_desc_reason
        (function
          | RTypeAlias (_, _, desc) -> RTypeAlias (n, loc, desc)
          | desc -> RTypeAlias (n, loc, desc))
        reason_obj
    | _ ->
      (* If [r] is an RTypeAlias, then this alias is no longer valid. *)
      update_desc_reason invalidate_rtype_alias reason_obj
  in
  DefT (reason_obj, ObjT { obj with flags = { obj.flags with obj_kind } })

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
  DefT (reason, ClassT t)

let extends_use_type use_op l u =
  let reason = update_desc_new_reason (fun desc -> RExtends desc) (reason_of_t u) in
  ExtendsUseT (use_op, reason, [], l, u)

let poly_type id tparams_loc tparams t =
  let reason = update_desc_new_reason (fun desc -> RPolyType desc) (reason_of_t t) in
  let reason = annot_reason ~annot_loc:(loc_of_reason reason) reason in
  DefT (reason, PolyT { tparams_loc; tparams; t_out = t; id })

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

let typeapp_with_use_op ~from_value ~use_desc reason use_op t targs =
  let reason = replace_desc_reason (RTypeApp (desc_of_t t)) reason in
  TypeAppT { reason; use_op; type_ = t; targs; from_value; use_desc }

let typeapp ~from_value ~use_desc reason t targs =
  let use_op = Op (TypeApplication { type_ = reason }) in
  typeapp_with_use_op ~from_value ~use_desc reason use_op t targs

let typeapp_annot ~from_value ~use_desc loc t targs =
  let desc = RTypeApp (desc_of_t t) in
  let reason = mk_annot_reason desc loc in
  let use_op = Op (TypeApplication { type_ = reason }) in
  TypeAppT { reason; use_op; type_ = t; targs; from_value; use_desc }

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
  let use_op = Op (TypeApplication { type_ = reason }) in
  TypeAppT { reason; use_op; type_ = t; targs; from_value = false; use_desc = false }

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

let typeof_annotation reason t targs =
  let annot_loc = loc_of_reason reason in
  let t = AnnotT (opt_annot_reason ~annot_loc reason, t, false) in
  match targs with
  | None -> t
  | Some targs ->
    let reason_tapp = mk_annot_reason (RTypeApp (desc_of_reason reason)) annot_loc in
    let use_op = Op (TypeApplication { type_ = reason_tapp }) in
    typeapp_with_use_op ~from_value:true ~use_desc:false reason use_op t targs

let push_type_alias_reason r t =
  match desc_of_reason ~unwrap:false r with
  | RTypeAlias (n, _, _) ->
    mod_reason_of_t (update_desc_reason (fun desc -> RTypeAlias (n, None, desc))) t
  | _ -> t

let rec eq_predicate (p1, p2) =
  match (p1, p2) with
  (* trivial *)
  | (TruthyP, TruthyP)
  | (NullP, NullP)
  | (MaybeP, MaybeP)
  | (BoolP _, BoolP _)
  | (FunP, FunP)
  | (NumP _, NumP _)
  | (ObjP, ObjP)
  | (StrP _, StrP _)
  | (SymbolP _, SymbolP _)
  | (VoidP, VoidP)
  | (ArrP, ArrP)
  | (ImpossibleP, ImpossibleP) ->
    true
  (* Recursive *)
  | (AndP (p1a, p1b), AndP (p2a, p2b)) -> eq_predicate (p1a, p2a) && eq_predicate (p1b, p2b)
  | (OrP (p1a, p1b), OrP (p2a, p2b)) -> eq_predicate (p1a, p2a) && eq_predicate (p1b, p2b)
  | (NotP p1, NotP p2) -> eq_predicate (p1, p2)
  | (SingletonBoolP (_, s1), SingletonBoolP (_, s2)) -> s1 = s2
  | (SingletonStrP (_, s1, v1), SingletonStrP (_, s2, v2)) -> s1 = s2 && v1 = v2
  | (SingletonNumP (_, s1, v1), SingletonNumP (_, s2, v2)) -> s1 = s2 && v1 = v2
  | (LatentP (c1, i1), LatentP (c2, i2)) -> c1 == c2 && i1 = i2
  | (LatentThisP c1, LatentThisP c2) -> c1 == c2
  | (PropExistsP { propname = s1; _ }, PropExistsP { propname = s2; _ }) -> s1 = s2
  | (PropTruthyP (s1, _), PropTruthyP (s2, _)) -> s1 = s2
  | (PropNonMaybeP (s1, _), PropNonMaybeP (s2, _)) -> s1 = s2
  | (ArrLenP { op = op1; n = n1 }, ArrLenP { op = op2; n = n2 }) -> op1 = op2 && n1 = n2
  (* Complex *)
  | (BinaryP (b1, OpenT (_, id1)), BinaryP (b2, OpenT (_, id2))) -> b1 = b2 && id1 = id2
  | (BinaryP _, BinaryP _) -> p1 = p2
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
let union_of_ts ?(kind = UnionRep.UnknownKind) reason ts =
  match ts with
  (* If we have no types then this is an error. *)
  | [] -> DefT (reason, EmptyT)
  (* If we only have one type then only that should be used. *)
  | [t0] -> t0
  (* If we have more than one type then we make a union type. *)
  | t0 :: t1 :: ts -> UnionT (reason, UnionRep.make ~kind t0 t1 ts)

let union_of_ts_opt ?(kind = UnionRep.UnknownKind) reason ts =
  match ts with
  | [] -> None
  | [t0] -> Some t0
  (* If we have more than one type then we make a union type. *)
  | t0 :: t1 :: ts -> Some (UnionT (reason, UnionRep.make ~kind t0 t1 ts))

let annotated_or_inferred_of_option ~default = function
  | Some t -> Annotated t
  | None -> Inferred default

let subtype_this_of_function { this_t = (this, subtyping); _ } =
  match subtyping with
  | This_Function -> this
  | This_Method _ -> reason_of_t this |> implicit_mixed_this

let all_explicit_targs = function
  | None -> None
  | Some targs ->
    Base.List.fold_right targs ~init:(Some []) ~f:(fun targ acc ->
        match (targ, acc) with
        | (ExplicitArg _, Some acc) -> Some (targ :: acc)
        | _ -> None
    )

let all_explicit_targ_ts = function
  | None -> None
  | Some targs ->
    Base.List.fold_right targs ~init:(Some []) ~f:(fun targ acc ->
        match (targ, acc) with
        | (ExplicitArg t, Some acc) -> Some (t :: acc)
        | _ -> None
    )

let dro_strict (_, dro_t) =
  match dro_t with
  | HookArg
  | HookReturn
  | Props
  | DebugAnnot ->
    false
  | ImmutableAnnot -> true

let tuple_length reason ~inexact (num_req, num_total) =
  if inexact then
    let r = replace_desc_reason RNumber reason in
    DefT (r, NumGeneralT AnyLiteral)
  else
    let t_of_n n =
      let r =
        let desc = RTupleLength n in
        replace_desc_reason desc reason
      in
      let t =
        let float = Base.Float.of_int n in
        let string = Base.Int.to_string n in
        SingletonNumT { from_annot = false; value = (float, string) }
      in
      DefT (r, t)
    in
    Base.List.range num_req ~stop:`inclusive num_total
    |> Base.List.map ~f:t_of_n
    |> union_of_ts reason

let tuple_ts_of_elements elements = Base.List.map ~f:(fun (TupleElement { t; _ }) -> t) elements

let mk_tuple_element ?name ?(optional = false) ?(polarity = Polarity.Neutral) reason t =
  TupleElement { reason; name; t; polarity; optional }

let reason_of_resolved_param = function
  | ResolvedSpreadArg (reason, _, _)
  | ResolvedAnySpreadArg (reason, _)
  | ResolvedArg (TupleElement { reason; _ }, _) ->
    reason

let dro_of_type t =
  match t with
  | DefT
      ( _,
        ( ObjT { flags = { react_dro; _ }; _ }
        | InstanceT { inst = { inst_react_dro = react_dro; _ }; _ }
        | ArrT (ArrayAT { react_dro; _ } | TupleAT { react_dro; _ } | ROArrayAT (_, react_dro)) )
      ) ->
    react_dro
  | _ -> None

(* Create the optional children input type from the children arguments. *)
let normalize_jsx_children_prop loc_children jsx_children =
  match jsx_children with
  (* If we have no children then React will not pass in any value for children. *)
  | [] -> None
  (* If we know that we have exactly one argument then React will pass in that single value.
   * Notable we do not wrap the type in an array as React returns the single value. *)
  | [t] -> Some t
  (* If we have two or more known arguments then we want to create a tuple array type for our children. *)
  | t :: ts ->
    (* Create a reason where the location is between our first and last known
     * argument. *)
    let r = mk_reason RReactChildren loc_children in
    let ts = t :: ts in
    let arity = Base.List.length ts in
    let elements = Base.List.map ~f:(fun t -> mk_tuple_element (reason_of_t t) t) ts in
    Some
      (DefT
         ( r,
           ArrT
             (ArrayAT
                {
                  elem_t = union_of_ts r ts;
                  tuple_view = Some (TupleView { elements; arity = (arity, arity); inexact = false });
                  react_dro = None;
                }
             )
         )
      )

let map_property ~f prop =
  match prop with
  | OrdinaryField { type_; polarity } -> OrdinaryField { type_ = f type_; polarity }
  | SyntheticField { get_type; set_type } ->
    SyntheticField
      { get_type = Base.Option.map ~f get_type; set_type = Base.Option.map ~f set_type }

let mk_possibly_generic_render_type ~variant reason t =
  let singleton_or_union_of_generic_t =
    let rec loop acc t =
      match (acc, t) with
      | (None, _) -> None
      | (Some l, GenericT _) -> Some (t :: l)
      | (acc, UnionT (_, rep)) -> UnionRep.members rep |> Base.List.fold ~init:acc ~f:loop
      | _ -> None
    in
    fun t ->
      match loop (Some []) t with
      | None
      | Some [] ->
        None
      | Some (hd :: tl) -> Some (hd, tl)
  in
  if variant <> Flow_ast.Type.Renders.Star then
    match singleton_or_union_of_generic_t t with
    | Some generic_ts ->
      let t =
        let open Flow_ast.Type in
        match variant with
        | Renders.Normal -> t
        | Renders.Maybe ->
          Nel.to_list generic_ts
          |> List.cons (DefT (reason, NullT))
          |> List.cons (DefT (reason, VoidT))
          |> List.cons (DefT (reason, SingletonBoolT { value = false; from_annot = true }))
          |> List.rev
          |> union_of_ts reason
        | Renders.Star -> failwith "Already banned above"
      in
      Some
        (DefT
           ( reason,
             RendersT
               (StructuralRenders { renders_variant = RendersNormal; renders_structural_type = t })
           )
        )
    | None -> None
  else
    None
