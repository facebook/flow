(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Utils_js
open Reason

exception EDebugThrow of Loc.t
exception EMergeTimeout of float

type invalid_char_set =
  | DuplicateChar of Char.t
  | InvalidChar of Char.t

module InvalidCharSetSet = Set.Make(struct
  type t = invalid_char_set
  let compare = Pervasives.compare
end)

type error_message =
  | EIncompatible of {
      lower: reason * lower_kind option;
      upper: reason * upper_kind;
      extras: (Reason.t * error_message) list;
    }
  | EIncompatibleDefs of {
      reason_lower: reason;
      reason_upper: reason;
      extras: (Reason.t * error_message) list;
    }
  | EIncompatibleProp of {
      reason_prop: reason;
      reason_obj: reason;
      special: lower_kind option;
      use_op: use_op option;
    }
  | EIncompatibleGetProp of {
      reason_prop: reason;
      reason_obj: reason;
      special: lower_kind option;
    }
  | EIncompatibleSetProp of {
      reason_prop: reason;
      reason_obj: reason;
      special: lower_kind option;
    }
  | EDebugPrint of reason * string
  | EImportValueAsType of reason * string
  | EImportTypeAsTypeof of reason * string
  | EImportTypeAsValue of reason * string
  | ENoDefaultExport of reason * string * string option
  | EOnlyDefaultExport of reason * string
  | ENoNamedExport of reason * string * string option
  | EMissingTypeArgs of { reason: reason; min_arity: int; max_arity: int }
  | EValueUsedAsType of (reason * reason)
  | EMutationNotAllowed of { reason: reason; reason_op: reason }
  | EExpectedStringLit of (reason * reason) * string * string Type.literal * use_op
  | EExpectedNumberLit of
      (reason * reason) *
      Type.number_literal *
      Type.number_literal Type.literal *
      use_op
  | EExpectedBooleanLit of (reason * reason) * bool * bool option * use_op
  | EPropNotFound of (reason * reason) * use_op
  | EPropAccess of (reason * reason) * string option * Type.polarity * Type.rw
  | EPropPolarityMismatch of (reason * reason) * string option * (Type.polarity * Type.polarity) * use_op
  | EPolarityMismatch of {
      reason: reason;
      name: string;
      expected_polarity: Type.polarity;
      actual_polarity: Type.polarity;
    }
  | EStrictLookupFailed of (reason * reason) * reason * string option * use_op option
  | EPrivateLookupFailed of (reason * reason)
  | EFunCallParam of (reason * reason)
  | EFunCallThis of reason * reason * reason
  | EFunReturn of (reason * reason)
  | EFunImplicitReturn of (reason * reason)
  | EAddition of (reason * reason)
  | EAdditionMixed of reason
  | ECoercion of (reason * reason)
  | EComparison of (reason * reason)
  | ETupleArityMismatch of (reason * reason) * int * int * use_op
  | ENonLitArrayToTuple of (reason * reason)
  | ETupleOutOfBounds of (reason * reason) * int * int
  | ETupleUnsafeWrite of (reason * reason)
  | EUnionSpeculationFailed of {
      use_op: use_op;
      reason: reason;
      reason_op: reason;
      branches: (reason * error_message) list
    }
  | ESpeculationAmbiguous of (reason * reason) * (int * reason) * (int * reason) * reason list
  | EIncompatibleWithExact of (reason * reason) * use_op
  | EUnsupportedExact of (reason * reason)
  | EIdxArity of reason
  | EIdxUse1 of reason
  | EIdxUse2 of reason
  | EUnexpectedThisType of Loc.t
  | EInvalidRestParam of reason
  | ETypeParamArity of Loc.t * int
  | ETypeParamMinArity of Loc.t * int
  | ETooManyTypeArgs of reason * reason * int
  | ETooFewTypeArgs of reason * reason * int
  | EPropertyTypeAnnot of Loc.t
  | EExportsAnnot of Loc.t
  | ECharSetAnnot of Loc.t
  | EInvalidCharSet of { invalid: reason * InvalidCharSetSet.t; valid: reason }
  | EUnsupportedKeyInObjectType of Loc.t
  | EPredAnnot of Loc.t
  | ERefineAnnot of Loc.t
  | EUnexpectedTypeof of Loc.t
  | ECustom of (reason * reason) * string
  | EInternal of Loc.t * internal_error
  | EUnsupportedSyntax of Loc.t * unsupported_syntax
  | EIllegalName of Loc.t
  | EUseArrayLiteral of Loc.t
  | EMissingAnnotation of reason
  | EBindingError of binding_error * Loc.t * string * Scope.Entry.t
  | ERecursionLimit of (reason * reason)
  | EModuleOutsideRoot of Loc.t * string
  | EExperimentalDecorators of Loc.t
  | EExperimentalClassProperties of Loc.t * bool
  | EUnsafeGetSet of Loc.t
  | EExperimentalExportStarAs of Loc.t
  | EIndeterminateModuleType of Loc.t
  | EUnreachable of Loc.t
  | EInvalidObjectKit of { tool: Object.tool; reason: reason; reason_op: reason; use_op: use_op }
  | EInvalidTypeof of Loc.t * string
  | EBinaryInLHS of reason
  | EBinaryInRHS of reason
  | EArithmeticOperand of reason
  | EForInRHS of reason
  | EObjectComputedPropertyAccess of (reason * reason)
  | EObjectComputedPropertyAssign of (reason * reason)
  | EInvalidLHSInAssignment of Loc.t
  | EIncompatibleWithUseOp of reason * reason * use_op
  | EUnsupportedImplements of reason
  | EReactKit of (reason * reason) * React.tool * use_op
  | EReactElementFunArity of reason * string * int
  | EFunctionCallExtraArg of reason * reason * int * use_op
  | EUnsupportedSetProto of reason
  | EDuplicateModuleProvider of {
      module_name: string;
      provider: File_key.t;
      conflict: File_key.t
    }
  | EParseError of Loc.t * Parse_error.t
  | EDocblockError of Loc.t * docblock_error
  (* The string is either the name of a module or "the module that exports `_`". *)
  | EUntypedTypeImport of Loc.t * string
  | EUntypedImport of Loc.t * string
  | ENonstrictImport of Loc.t
  | EUnclearType of Loc.t
  | EUnsafeGettersSetters of Loc.t
  | EUnusedSuppression of Loc.t
  | ELintSetting of LintSettings.lint_parse_error
  | ESketchyNullLint of {
      kind: Lints.sketchy_null_kind;
      loc: Loc.t;
      null_loc: Loc.t;
      falsy_loc: Loc.t;
    }
  | EInvalidPrototype of reason
  | EDeprecatedDeclareExports of Loc.t

and binding_error =
  | ENameAlreadyBound
  | EReferencedBeforeDeclaration
  | ETypeInValuePosition
  | ETypeAliasInValuePosition
  | EConstReassigned
  | EConstParamReassigned
  | EImportReassigned

and docblock_error =
  | MultipleFlowAttributes
  | MultipleProvidesModuleAttributes
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option

and internal_error =
  | PackageHeapNotFound of string
  | AbnormalControlFlow
  | MethodNotAFunction
  | OptionalMethod
  | OpenPredWithoutSubst
  | PredFunWithoutParamNames
  | UnsupportedGuardPredicate of string
  | BreakEnvMissingForCase
  | PropertyDescriptorPropertyCannotBeRead
  | ForInLHS
  | ForOfLHS
  | InstanceLookupComputed
  | PropRefComputedOpen
  | PropRefComputedLiteral
  | ShadowReadComputed
  | ShadowWriteComputed
  | RestParameterNotIdentifierPattern
  | InterfaceTypeSpread
  | DebugThrow
  | MergeTimeout of float
  | MergeJobException of exn

and unsupported_syntax =
  | ComprehensionExpression
  | GeneratorExpression
  | MetaPropertyExpression
  | ObjectPropertyLiteralNonString
  | ObjectPropertyGetSet
  | ObjectPropertyComputedGetSet
  | InvariantSpreadArgument
  | ClassPropertyLiteral
  | ClassPropertyComputed
  | ReactCreateClassPropertyNonInit
  | RequireDynamicArgument
  | RequireLazyDynamicArgument
  | CatchParameterAnnotation
  | CatchParameterDeclaration
  | DestructuringObjectPropertyLiteralNonString
  | DestructuringExpressionPattern
  | PredicateDeclarationForImplementation
  | PredicateDeclarationWithoutExpression
  | PredicateDeclarationAnonymousParameters
  | PredicateInvalidBody
  | PredicateVoidReturn
  | MultipleIndexers
  | SpreadArgument
  | ImportDynamicArgument

and lower_kind =
  | Possibly_null
  | Possibly_void
  | Possibly_null_or_void
  | Incompatible_intersection

and upper_kind =
  | IncompatibleGetPropT
  | IncompatibleSetPropT
  | IncompatibleMethodT
  | IncompatibleCallT
  | IncompatibleConstructorT
  | IncompatibleGetElemT
  | IncompatibleSetElemT
  | IncompatibleCallElemT
  | IncompatibleElemTRead
  | IncompatibleElemTWrite
  | IncompatibleElemTCall
  | IncompatibleObjAssignFromTSpread
  | IncompatibleObjAssignFromT
  | IncompatibleObjRestT
  | IncompatibleObjSealT
  | IncompatibleArrRestT of use_op
  | IncompatibleSuperT
  | IncompatibleMixinT
  | IncompatibleSpecializeT
  | IncompatibleThisSpecializeT
  | IncompatibleVarianceCheckT
  | IncompatibleGetKeysT
  | IncompatibleHasOwnPropT
  | IncompatibleGetValuesT
  | IncompatibleUnaryMinusT
  | IncompatibleMapTypeTTuple
  | IncompatibleMapTypeTObject
  | IncompatibleTypeAppVarianceCheckT
  | IncompatibleUnclassified of string

let desc_of_reason r = Reason.desc_of_reason ~unwrap_alias:(is_scalar_reason r) r

(* Decide reason order based on UB's flavor and blamability.
   If the order is unchanged, maintain reference equality. *)
let ordered_reasons ((rl, ru) as reasons) =
  if (is_blamable_reason ru && not (is_blamable_reason rl))
  then ru, rl
  else reasons

let is_useless_op op_reason error_reason =
  match desc_of_reason op_reason with
  | RMethodCall _ -> reasons_overlap op_reason error_reason
  | _ -> false

let rec error_of_msg ~trace_reasons ~source_file =
  let open Errors in

  let mk_info reason extras =
    let desc = string_of_desc (desc_of_reason reason) in
    (* For descriptions that are an identifier wrapped in primes, e.g. `A`, then
     * we want to unwrap the primes and just show A. This looks better in infos.
     * However, when an identifier wrapped with primes is inside some other text
     * then we want to keep the primes since they help with readability. *)
    let desc = if (
      (String.length desc > 2) &&
      ((String.get desc 0) = '`') &&
      ((String.get desc ((String.length desc) - 1)) = '`') &&
      not (String.contains desc ' ')
    ) then (
      String.sub desc 1 ((String.length desc) - 2)
    ) else desc in
    loc_of_reason reason, desc :: extras
  in

  let info_of_reason r = mk_info r [] in

  let trace_infos = List.map info_of_reason trace_reasons in

  let special_suffix = function
    | Some Possibly_null -> " possibly null value"
    | Some Possibly_void -> " possibly undefined value"
    | Some Possibly_null_or_void -> " possibly null or undefined value"
    | Some Incompatible_intersection -> " any member of intersection type"
    | None -> ""
  in

  (* only on use-types - guard calls with is_use t *)
  let err_msg_use special u =
    let msg = match u with
    | IncompatibleGetPropT -> "Property cannot be accessed on"
    | IncompatibleSetPropT -> "Property cannot be assigned on"
    | IncompatibleMethodT -> "Method cannot be called on"
    | IncompatibleCallT -> "Function cannot be called on"
    | IncompatibleConstructorT -> "Constructor cannot be called on"
    | IncompatibleGetElemT -> "Computed property/element cannot be accessed on"
    | IncompatibleSetElemT -> "Computed property/element cannot be assigned on"
    | IncompatibleCallElemT -> "Computed property/element cannot be called on"
    | IncompatibleElemTRead -> "Element cannot be accessed with"
    | IncompatibleElemTWrite -> "Element cannot be assigned with"
    | IncompatibleElemTCall -> "Element cannot be called with"
    | IncompatibleObjAssignFromTSpread -> "Expected array instead of"
    | IncompatibleObjAssignFromT -> "Expected object instead of"
    | IncompatibleObjRestT -> "Expected object instead of"
    | IncompatibleObjSealT -> "Expected object instead of"
    | IncompatibleArrRestT _ -> "Expected array instead of"
    | IncompatibleSuperT -> "Cannot inherit"
    | IncompatibleMixinT -> "Expected class instead of"
    | IncompatibleSpecializeT -> "Expected polymorphic type instead of"
    | IncompatibleThisSpecializeT -> "Expected class instead of"
    | IncompatibleVarianceCheckT -> "Expected polymorphic type instead of"
    | IncompatibleGetKeysT -> "Expected object instead of"
    | IncompatibleHasOwnPropT -> "Property not found in"
    | IncompatibleGetValuesT -> "Expected object instead of"
    | IncompatibleUnaryMinusT -> "Expected number instead of"
    | IncompatibleMapTypeTTuple -> "Expected Iterable instead of"
    | IncompatibleMapTypeTObject -> "Expected object instead of"
    | IncompatibleTypeAppVarianceCheckT -> "Expected polymorphic type instead of"
    (* unreachable or unclassified use-types. until we have a mechanical way
       to verify that all legit use types are listed above, we can't afford
       to throw on a use type, so mark the error instead *)
    | IncompatibleUnclassified ctor ->
      spf "Type is incompatible with (unclassified use type: %s)" ctor
    in
    spf "%s%s" msg (special_suffix special) in

  let msg_export export_name =
    if export_name = "default"
    then export_name
    else spf "`%s`" export_name
  in

  let typecheck_error_with_core_infos ?kind ?extra core_msgs =
    let core_reasons = List.map fst core_msgs in
    let core_infos = List.map (fun (r, msgs) -> mk_info r msgs) core_msgs in

    (* Since pointing to endpoints in the library without any information on
       the code that uses those endpoints inconsistently is useless, we point
       to the file containing that code instead. Ideally, improvements in
       error reporting would cause this case to never arise.

       Additionally, we never suppress ops when this happens, because that is
       our last chance at relevant context. *)
    let lib_infos = if List.for_all is_lib_reason core_reasons then
        let loc = Loc.({ none with source = Some source_file }) in
        [loc, ["inconsistent use of library definitions"]]
      else []
    in
    (* main info is core info with optional lib line prepended, and optional
       extra info appended. ops/trace info is held separately in error *)
    let msg_infos = lib_infos @ core_infos in
    mk_error ?kind ~trace_infos ?extra msg_infos
  in

  let typecheck_msgs msg (r1, r2) = [r1, [msg]; r2, []] in

  let typecheck_error msg ?kind ?extra reasons =
    (* make core info from reasons, message, and optional extra infos *)
    let core_msgs = typecheck_msgs msg reasons in
    typecheck_error_with_core_infos ?kind ?extra core_msgs
  in

  let prop_polarity_error_msg x reasons p1 p2 =
    let prop_name = match x with
    | Some x -> spf "property `%s`" x
    | None -> "computed property"
    in
    let reasons' = ordered_reasons reasons in
    let msg =
      if reasons' == reasons then
        spf "%s %s incompatible with %s use in"
          (String.capitalize_ascii (Polarity.string p1))
          prop_name
          (Polarity.string p2)
      else
        spf "Incompatible with %s %s"
          (Polarity.string p1)
          prop_name
    in
    reasons', msg
  in

  let extra_info_of_use_op (rl, ru) extra msg wrapper_msg =
    let infos = [mk_info rl [msg]; mk_info ru []] in
    [InfoNode (
      [Loc.none, [wrapper_msg]],
      [if extra = []
       then InfoLeaf infos
       else InfoNode (infos, extra)]
    )]
  in

  let speculation_extras branches =
    List.mapi (fun i (r, msg) ->
      let err = error_of_msg ~trace_reasons:[] ~source_file msg in
      let header_infos = [
        Loc.none, [spf "Member %d:" (i + 1)];
        info_of_reason r;
        Loc.none, ["Error:"];
      ] in
      let error_infos = infos_of_error err in
      let error_extra = extra_of_error err in
      let info_list = header_infos @ error_infos in
      let info_tree = match error_extra with
        | [] -> Errors.InfoLeaf (info_list)
        | _ -> Errors.InfoNode (info_list, error_extra)
      in
      info_tree
    ) branches
  in

  (* Unification produces two errors. One for both sides. For example,
   * {p: number} ~> {p: string} errors on both number ~> string and
   * string ~> number. Showing both errors to our user is often redundant.
   * So we use this utility to flip the string ~> number case and produce an
   * error identical to one we've produced before. These two errors will be
   * deduped in our ErrorSet. *)
  let dedupe_by_flip =
    (* Flip the lower/upper reasons of a frame_use_op. *)
    let flip_frame = function
    | FunCompatibility c -> FunCompatibility {lower = c.upper; upper = c.lower}
    | FunParam c -> FunParam {c with lower = c.upper; upper = c.lower}
    | FunReturn c -> FunReturn {lower = c.upper; upper = c.lower}
    | IndexerKeyCompatibility c -> IndexerKeyCompatibility {lower = c.upper; upper = c.lower}
    | PropertyCompatibility c -> PropertyCompatibility {c with lower = c.upper; upper = c.lower}
    | TupleElementCompatibility c ->
      TupleElementCompatibility {c with lower = c.upper; upper = c.lower}
    | TypeArgCompatibility (name, lower, upper) -> TypeArgCompatibility (name, upper, lower)
    | TypeParamBound _
    | FunMissingArg _
    | ImplicitTypeParam _
    | UnifyFlip
      as use_op -> use_op
    in
    (* Loop over through the use_op chain. *)
    let rec loop = function
    (* Roots don't flip. *)
    | Op _ as use_op -> (false, use_op)
    (* Start flipping if we are on the reverse side of unification. *)
    | Frame (UnifyFlip, use_op) ->
      let (flip, use_op) = loop use_op in
      (not flip, use_op)
    (* If we are in flip mode then flip our frame. *)
    | Frame (frame, use_op) ->
      let (flip, use_op) = loop use_op in
      if flip
        then (true, Frame (flip_frame frame, use_op))
        else (false, Frame (frame, use_op))
    in
    fun (lower, upper) use_op ->
      let (flip, use_op) = loop use_op in
      if flip
        then ((upper, lower), use_op)
        else ((lower, upper), use_op)
  in

  (* NB: Some use_ops, like FunReturn, completely replace the `msg` argument.
     Sometimes we call unwrap_use_ops from an error variant that has some
     interesting information in `msg`, like EIncompatibleWithExact. In those
     cases, it's more valuable to preserve the input msg than to replace it.

     To support those cases, callers can pass a `force` argument. A better
     alternative would be to somehow combine the messages, so an exact error
     with a function param would say something like "inexact argument
     incompatible with exact parameter."

     Note that `force` is not recursive, as the input message is consumed by
     `extra_info_of_use_op` and will appear in the output. *)
  let rec unwrap_use_ops ?(force=false) (reasons, extra, msg) = function
  | Frame (PropertyCompatibility {prop=x; lower=rl'; upper=ru'; _}, use_op) ->
    let extra =
      let prop =
        match x with
        | Some "$call" -> "Callable property"
        | None | Some "$key" | Some "$value" -> "Indexable signature"
        | Some x -> spf "Property `%s`" x
      in
      extra_info_of_use_op reasons extra msg
        (spf "%s is incompatible:" prop)
    in
    let obj_reasons = ordered_reasons (rl', ru') in
    let msg = "This type is incompatible with" in
    unwrap_use_ops (obj_reasons, extra, msg) use_op
  | Frame (IndexerKeyCompatibility {lower=rl'; upper=ru'}, use_op) ->
    let extra =
      extra_info_of_use_op reasons extra msg "Indexer key is incompatible:"
    in
    let obj_reasons = ordered_reasons (rl', ru') in
    let msg = "This type is incompatible with" in
    unwrap_use_ops (obj_reasons, extra, msg) use_op
  | Frame (TupleElementCompatibility {n; lower; upper}, use_op) ->
    let extra =
      extra_info_of_use_op reasons extra msg
        (spf "The %s tuple element is incompatible:" (Utils_js.ordinal n))
    in
    let msg = "Has some incompatible tuple element with" in
    unwrap_use_ops ((lower, upper), extra, msg) use_op
  | Frame (TypeArgCompatibility (x, reason_op, reason_tapp), use_op) ->
    let extra =
      extra_info_of_use_op reasons extra msg
        (spf "Type argument `%s` is incompatible:" x)
    in
    let msg = "Has some incompatible type argument with" in
    unwrap_use_ops ((reason_op, reason_tapp), extra, msg) use_op
  | Frame (TypeParamBound { name }, use_op) ->
    let msg = spf "This type is incompatible with the bound on type parameter `%s`:" name in
    unwrap_use_ops (reasons, extra, msg) use_op
  | Op FunReturnStatement when not force ->
    let msg = "This type is incompatible with the expected return type of" in
    extra, typecheck_msgs msg reasons
  | Op FunImplicitReturn when not force ->
    let lreason, ureason = reasons in
    let msg = spf "This type is incompatible with an implicitly-returned %s"
      (string_of_desc (desc_of_reason lreason))
    in
    extra, [ureason, [msg]]
  | Frame (FunParam {lower; _}, Op (FunCall _ | FunCallMethod _)) ->
    let reasons, msg =
      if not force then
        let reasons' = ordered_reasons reasons in
        let msg =
          if reasons' == reasons
          then "This type is incompatible with the expected param type of"
          else "This type is incompatible with an argument type of"
        in
        reasons', msg
      else
        reasons, msg
    in
    (* Always prefer the location from our use_op. use_ops should never have the
     * wrong primary location. *)
    let msgs = if Loc.contains (loc_of_reason lower) (loc_of_reason (fst reasons))
      then typecheck_msgs msg reasons
      else (lower, [])::(typecheck_msgs msg reasons)
    in
    extra, msgs
  | Frame (FunParam {n; _}, Frame (FunCompatibility {lower; upper}, use_op)) ->
    let extra =
      extra_info_of_use_op reasons extra msg
        (spf "The %s parameter is incompatible:" (Utils_js.ordinal n))
    in
    let msg = "This type is incompatible with" in
    unwrap_use_ops ((lower, upper), extra, msg) use_op
  | Frame (FunReturn _, Frame (FunCompatibility {lower; upper}, use_op)) ->
    let extra =
      extra_info_of_use_op reasons extra msg "The return is incompatible:"
    in
    let msg = "This type is incompatible with" in
    unwrap_use_ops ((lower, upper), extra, msg) use_op
  | Frame (FunMissingArg (op, def), use_op) ->
    let msg = "Too few arguments passed to" in
    unwrap_use_ops ~force:true ((op, def), [], msg) use_op
  | Op (ReactCreateElementCall _) ->
    extra, typecheck_msgs msg reasons
  | Op (ReactGetIntrinsic {literal}) ->
    let msg = "Is not a valid React JSX intrinsic" in
    extra, [literal, [msg]]
  | Op (SetProperty reason_op) ->
    let rl, ru = reasons in
    let ru = replace_reason_const (desc_of_reason ru) reason_op in
    extra, typecheck_msgs msg (rl, ru)
  | Op (ClassExtendsCheck _) ->
    let msg = if not force
      then "Cannot extend"
      else msg
    in
    extra, typecheck_msgs msg reasons
  | Op (ClassImplementsCheck _) ->
    let msg = if not force
      then "Cannot implement"
      else msg
    in
    extra, typecheck_msgs msg reasons
  (* Passthrough some frame use_ops that don't participate in the
   * error message. *)
  | Frame (FunCompatibility _, use_op)
  | Frame (ImplicitTypeParam _, use_op)
  | Frame (UnifyFlip, use_op)
    -> unwrap_use_ops ~force (reasons, extra, msg) use_op
  (* Some use_ops always have the definitive location for an error message.
   * When we have one of these use_ops, make sure that its location is always
   * the primary location. *)
  | Op (AssignVar {init=op; _})
  | Op (Cast {lower=op; _})
  | Op (FunCall {op; _})
  | Op (FunCallMethod {op; _})
    ->
    let rl, ru = reasons in
    if Loc.contains (loc_of_reason op) (loc_of_reason rl) then (
      extra, typecheck_msgs msg reasons
    ) else (
      extra, [op, []; rl, [msg]; ru, []]
    )
  | _ ->
    extra, typecheck_msgs msg reasons
  in

  function
  | EIncompatible {
      lower = (reason_lower, lower_kind);
      upper = (reason_upper, upper_kind);
      extras;
    } ->
      (match upper_kind with
      | IncompatibleArrRestT use_op
        ->
        let extra = speculation_extras extras in
        let extra, msgs =
          let msg = err_msg_use lower_kind upper_kind in
          unwrap_use_ops ((reason_upper, reason_lower), extra, msg) use_op in
        typecheck_error_with_core_infos ~extra msgs
      | _ ->
        let extra = speculation_extras extras in
        typecheck_error ~extra (err_msg_use lower_kind upper_kind) (reason_upper, reason_lower)
      )

  | EIncompatibleDefs { reason_lower; reason_upper; extras } ->
      let reasons = ordered_reasons (reason_lower, reason_upper) in
      let extra = speculation_extras extras in
      typecheck_error ~extra "This type is incompatible with" reasons

  | EIncompatibleProp { reason_prop; reason_obj; special; use_op } ->
      let reasons = (reason_prop, reason_obj) in
      let msg = spf "Property not found in%s" (special_suffix special) in
      begin match use_op with
      | Some use_op ->
        let extra, msgs = unwrap_use_ops (reasons, [], msg) use_op in
        typecheck_error_with_core_infos ~extra msgs
      | None ->
        typecheck_error msg reasons
      end

  | EIncompatibleGetProp { reason_prop; reason_obj; special } ->
      let msg = spf "Property cannot be accessed on%s" (special_suffix special) in
      typecheck_error msg (reason_prop, reason_obj)

  | EIncompatibleSetProp { reason_prop; reason_obj; special } ->
      let msg = spf "Property cannot be assigned on%s" (special_suffix special) in
      typecheck_error msg (reason_prop, reason_obj)

  | EDebugPrint (r, str) ->
      mk_error ~trace_infos [mk_info r [str]]

  | EImportValueAsType (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "The %s export is a value, but not a type. `import type` only works \
         on type exports like type aliases, interfaces, and classes. If you \
         intended to import the type *of* a value, please use `import \
         typeof` instead."
        (msg_export export_name)]]

  | EImportTypeAsTypeof (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "The %s export is a type, but not a value. `import typeof` only \
         works on value exports like classes, vars, lets, etc. If you \
         intended to import a type alias or interface, please use `import \
         type` instead."
        (msg_export export_name)]]

  | EImportTypeAsValue (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "`%s` is a type, but not a value. In order to import it, please use \
         `import type`."
        export_name]]

  | ENoDefaultExport (r, module_name, suggestion) ->
      let msg = "This module has no default export." in
      let msg = match suggestion with
      | None -> msg
      | Some x ->
        msg ^ (spf " Did you mean `import {%s} from \"%s\"`?" x module_name)
      in
      mk_error ~trace_infos [mk_info r [msg]]

  | EOnlyDefaultExport (r, export_name) ->
      mk_error ~trace_infos [mk_info r [spf
        "This module only has a default export. Did you mean \
         `import %s from ...`?"
        export_name]]

  | ENoNamedExport (r, export_name, suggestion) ->
      let msg =
        spf "This module has no named export called `%s`." export_name in
      let msg = match suggestion with
      | None -> msg
      | Some x -> msg ^ (spf " Did you mean `%s`?" x)
      in
      mk_error ~trace_infos [mk_info r [msg]]

  | EMissingTypeArgs { reason; min_arity; max_arity } ->
      let arity, args = if min_arity = max_arity
        then spf "%d" max_arity, if max_arity = 1 then "argument" else "arguments"
        else spf "%d-%d" min_arity max_arity, "arguments"
      in
      mk_error ~trace_infos [mk_info reason [spf
        "Application of polymorphic type needs \
         <list of %s %s>. (Can use `*` for inferrable ones)"
        arity args]]

  | EValueUsedAsType reasons ->
      typecheck_error
        "Ineligible value used in/as type annotation \
         (did you forget 'typeof'?)"
        reasons

  | EMutationNotAllowed { reason; reason_op } ->
      typecheck_error "Mutation not allowed on" (reason_op, reason)

  | EExpectedStringLit (reasons, expected, actual, use_op) ->
      let msg = match actual with
      | Literal (None, actual) ->
          spf "Expected string literal `%s`, got `%s` instead"
            expected actual
      | Truthy | AnyLiteral ->
          spf "Expected string literal `%s`" expected
      | Literal (Some sense, actual) ->
          spf "This %s check always %s because `%s` is not the same string as `%s`"
            (if sense then "===" else "!==")
            (if sense then "fails" else "succeeds")
            actual
            expected
      in
      let extra, msgs =
        unwrap_use_ops ~force:true (reasons, [], msg) use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EExpectedNumberLit (reasons, (expected, _), actual, use_op) ->
      let msg = match actual with
      | Literal (None, (actual, _)) ->
          spf "Expected number literal `%.16g`, got `%.16g` instead"
            expected actual
      | Truthy | AnyLiteral ->
          spf "Expected number literal `%.16g`" expected
      | Literal (Some sense, (actual, _)) ->
          spf "This %s check always %s because `%.16g` is not the same number as `%.16g`"
            (if sense then "===" else "!==")
            (if sense then "fails" else "succeeds")
            actual
            expected
      in
      let extra, msgs =
        unwrap_use_ops ~force:true (reasons, [], msg) use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EExpectedBooleanLit (reasons, expected, actual, use_op) ->
      let msg = match actual with
      | Some actual ->
          spf "Expected boolean literal `%b`, got `%b` instead"
            expected actual
      | None -> spf "Expected boolean literal `%b`" expected
      in
      let extra, msgs =
        unwrap_use_ops ~force:true (reasons, [], msg) use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EPropNotFound (reasons, use_op) ->
      let use_op = match use_op with Op (SetProperty _) -> unknown_use | _ -> use_op in
      let extra, msgs =
        unwrap_use_ops (reasons, [], "Property not found in") use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EPropAccess (reasons, x, polarity, rw) ->
      let reasons, msg = prop_polarity_error_msg x reasons polarity (Polarity.of_rw rw) in
      typecheck_error msg reasons

  | EPropPolarityMismatch (reasons, x, (p1, p2), use_op) ->
      let reasons, msg = prop_polarity_error_msg x reasons p1 p2 in
      let extra, msgs = unwrap_use_ops (reasons, [], msg) use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EPolarityMismatch { reason; name; expected_polarity; actual_polarity } ->
      mk_error ~trace_infos [mk_info reason [spf
        "%s position (expected `%s` to occur only %sly)"
        (Polarity.string actual_polarity)
        name
        (Polarity.string expected_polarity)]]

  | EStrictLookupFailed (reasons, lreason, x, use_op) ->
    (* if we're looking something up on the global/builtin object, then tweak
       the error to say that `x` doesn't exist. We can tell this is the
       global object because that should be the only object created with
       `builtin_reason` instead of an actual location (see `Init_js.init`). *)
    if is_builtin_reason lreason
    then
      let msg = match x with
      | Some x when is_internal_module_name x -> "Required module not found"
      | _ -> "Could not resolve name"
      in
      mk_error ~trace_infos [mk_info (fst reasons) [msg]]
    else
      let msg = match x with
      | Some "$call" -> "Callable signature not found in"
      | Some "$key" | Some "$value" -> "Indexable signature not found in"
      | _ -> "Property not found in"
      in
      begin match use_op with
      | Some use_op ->
        let use_op = match use_op with Op (SetProperty _) -> unknown_use | _ -> use_op in
        let extra, msgs =
          unwrap_use_ops ~force:true (reasons, [], msg) use_op in
        typecheck_error_with_core_infos ~extra msgs
      | None ->
        typecheck_error msg reasons
      end

  | EPrivateLookupFailed reasons ->
      typecheck_error "Property not found in" reasons

  | EFunCallParam (lreason, ureason) ->
      (* Special case: if the lower bound is from a libdef, then flip
         the message and the reasons, so we point at something in user code.

         For example, consider a libdef like:

           declare function foo(): Promise<string>

         and user code like:

           let callback = (x: number) => ...;
           foo().then(callback)

         The user's `callback` gets "called" with a `string` arg. `lreason` is
         the `string` from `Promise<string>` in the libdef, and `ureason` is
         `number` inside `callback`. *)
      let r1, r2, msg = if is_lib_reason lreason then
        ureason, lreason, "This type is incompatible with an argument type of"
      else
        lreason, ureason,
          "This type is incompatible with the expected param type of"
      in
      typecheck_error msg (r1, r2)

  | EFunCallThis (lreason, ureason, reason_call) ->
      let msg = "This function call's `this` type is incompatible with" in
      let extra = [InfoLeaf ([
        Loc.none, ["The call's `this` type is:"];
        info_of_reason (lreason);
      ])] in
      typecheck_error msg ~extra (reason_call, ureason)

  | EFunReturn reasons ->
      typecheck_error
        "This type is incompatible with the expected return type of"
        reasons

  | EFunImplicitReturn (lreason, ureason) ->
      mk_error ~trace_infos [mk_info ureason [spf
        "This type is incompatible with an implicitly-returned %s."
        (string_of_desc (desc_of_reason lreason))]]

  | EAddition reasons ->
      typecheck_error "This type cannot be added to" reasons

  | EAdditionMixed reason ->
      mk_error ~trace_infos [mk_info reason [
        "This type cannot be used in an addition because it is unknown \
         whether it behaves like a string or a number."]]

  | ECoercion reasons ->
      typecheck_error "This type cannot be coerced to" reasons

  | EComparison reasons ->
      typecheck_error "This type cannot be compared to" reasons

  | ETupleArityMismatch (reasons, l1, l2, use_op) ->
      let msg = spf
        "Tuple arity mismatch. This tuple has %d elements and cannot flow to \
        the %d elements of"
        l1
        l2 in
      let extra, msgs =
        unwrap_use_ops ~force:true (reasons, [], msg) use_op
      in
      typecheck_error_with_core_infos ~extra msgs

  | ENonLitArrayToTuple reasons ->
      let msg =
        "Only tuples and array literals with known elements can flow to" in
      typecheck_error msg reasons

  | ETupleOutOfBounds (reasons, length, index) ->
      let msg = spf
        "Out of bound access. This tuple has %d elements and you tried to \
        access index %d of"
        length
        index in
      typecheck_error msg reasons

  | ETupleUnsafeWrite (reasons) ->
      let msg = spf
        "Flow will only let you modify a tuple if it knows exactly which \
        element of the tuple you are mutating. Unsafe mutation of" in
      typecheck_error msg reasons

  | EUnionSpeculationFailed { use_op; reason; reason_op; branches } ->
      let extra, msgs =
        let reasons = ordered_reasons (reason, reason_op) in
        let extra = speculation_extras branches in
        let msg = "This type is incompatible with" in
        unwrap_use_ops (reasons, extra, msg) use_op
      in
      typecheck_error_with_core_infos ~extra msgs

  | ESpeculationAmbiguous ((case_r, r), (prev_i, prev_case), (i, case), case_rs) ->
      let infos = List.map info_of_reason case_rs in
      let extra = [
        InfoLeaf [
          Loc.none, [spf "Case %d may work:" (prev_i + 1)];
          info_of_reason prev_case;
        ];
        InfoLeaf [
          Loc.none, [spf
            "But if it doesn't, case %d looks promising too:"
            (i + 1)];
          info_of_reason case;
        ];
        InfoLeaf (
          (Loc.none, [spf
            "Please provide additional annotation(s) to determine whether \
             case %d works (or consider merging it with case %d):"
            (prev_i + 1)
            (i + 1)]
          )::infos
        )
      ] in
      mk_error ~trace_infos ~extra [
        (mk_info case_r ["Could not decide which case to select"]);
        (info_of_reason r)
      ]

  | EIncompatibleWithExact (reasons, use_op) ->
      let msg = "Inexact type is incompatible with exact type" in
      let extra, msgs =
        unwrap_use_ops ~force:true (reasons, [], msg) use_op
      in
      typecheck_error_with_core_infos ~extra msgs

  | EUnsupportedExact reasons ->
      typecheck_error "Unsupported exact type" reasons

  | EIdxArity reason ->
      mk_error ~trace_infos [mk_info reason [
        "idx() function takes exactly two params!"
      ]]

  | EIdxUse1 reason ->
      mk_error ~trace_infos [mk_info reason [
        "idx() callback functions may not be annotated and they may only \
         access properties on the callback parameter!"
      ]]

  | EIdxUse2 reason ->
      mk_error ~trace_infos [mk_info reason [
        "idx() callbacks may only access properties on the callback \
         parameter!"
      ]]

  | EUnexpectedThisType loc ->
      mk_error ~trace_infos [loc, ["Unexpected use of `this` type"]]

  | EInvalidRestParam reason ->
      mk_error ~trace_infos ~kind:InferWarning [mk_info reason [
        "rest parameter should have an array type"
      ]]

  | ETypeParamArity (loc, n) ->
      let msg = spf "Incorrect number of type parameters (expected %n)" n in
      mk_error ~trace_infos [loc, [msg]]

  | ETypeParamMinArity (loc, n) ->
      let msg = spf
        "Incorrect number of type parameters (expected at least %n)" n
      in
      mk_error ~trace_infos [loc, [msg]]

  | ETooManyTypeArgs (reason_tapp, reason_arity, maximum_arity) ->
      let msg = spf
        "Too many type arguments. Expected at most %d"
        maximum_arity
      in
      mk_error ~trace_infos [
        mk_info reason_tapp [msg];
        mk_info reason_arity [];
      ]

  | ETooFewTypeArgs (reason_tapp, reason_arity, minimum_arity) ->
      let msg = spf
        "Too few type arguments. Expected at least %d"
        minimum_arity
      in
      mk_error ~trace_infos [
        mk_info reason_tapp [msg];
        mk_info reason_arity [];
      ]

  | EPropertyTypeAnnot loc ->
      let msg =
        "expected object type and string literal as arguments to \
         $PropertyType"
      in
      mk_error ~trace_infos [loc, [msg]]

  | EExportsAnnot loc ->
      mk_error ~trace_infos [loc, ["$Exports requires a string literal"]]

  | ECharSetAnnot loc ->
      mk_error ~trace_infos [loc, ["$CharSet requires a string literal"]]

  | EInvalidCharSet {
      invalid = (invalid_reason, invalid_chars);
      valid = valid_reason;
    } ->
      let def_loc = def_loc_of_reason invalid_reason in
      let extra =
        InvalidCharSetSet.fold (fun c acc ->
          match c with
          | InvalidChar c -> InfoLeaf [def_loc, [spf "`%c` is not a member of the set" c]]::acc
          | DuplicateChar c -> InfoLeaf [def_loc, [spf "`%c` is duplicated" c]]::acc
        ) invalid_chars []
        |> List.rev
      in
      mk_error ~trace_infos ~extra [
        mk_info invalid_reason ["This type is incompatible with"];
        mk_info valid_reason [];
      ]

  | EUnsupportedKeyInObjectType loc ->
      mk_error ~trace_infos [loc, ["Unsupported key in object type"]]

  | EPredAnnot loc ->
      let msg =
        "expected number of refined variables (currently only supporting \
         one variable)"
      in
      mk_error ~trace_infos [loc, [msg]]

  | ERefineAnnot loc ->
      let msg =
        "expected base type and predicate type as arguments to $Refine"
      in
      mk_error ~trace_infos [loc, [msg]]

  | EUnexpectedTypeof loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Unexpected typeof expression"
      ]]

  | ECustom (reasons, msg) ->
      typecheck_error msg reasons

  | EInternal (loc, internal_error) ->
      let msg = match internal_error with
      | PackageHeapNotFound pkg ->
          spf "Package %S was not found in the PackageHeap!" pkg
      | AbnormalControlFlow ->
          "abnormal control flow"
      | MethodNotAFunction ->
          "expected function type"
      | OptionalMethod ->
          "optional methods are not supported"
      | OpenPredWithoutSubst ->
          "OpenPredT ~> OpenPredT without substitution"
      | PredFunWithoutParamNames ->
          "FunT -> FunT no params"
      | UnsupportedGuardPredicate pred ->
          spf "Unsupported guard predicate (%s)" pred
      | BreakEnvMissingForCase ->
          "break env missing for case"
      | PropertyDescriptorPropertyCannotBeRead ->
          "Unexpected property in properties object"
      | ForInLHS ->
          "unexpected LHS in for...in"
      | ForOfLHS ->
          "unexpected LHS in for...of"
      | InstanceLookupComputed ->
          "unexpected computed property lookup on InstanceT"
      | PropRefComputedOpen ->
          "unexpected open computed property element type"
      | PropRefComputedLiteral ->
          "unexpected literal computed property element type"
      | ShadowReadComputed ->
          "unexpected shadow read on computed property"
      | ShadowWriteComputed ->
          "unexpected shadow write on computed property"
      | RestParameterNotIdentifierPattern ->
          "unexpected rest parameter, expected an identifier pattern"
      | InterfaceTypeSpread ->
          "unexpected spread property in interface"
      | DebugThrow ->
          "debug throw"
      | MergeTimeout s ->
          spf "merge job timed out after %0.2f seconds" s
      | MergeJobException exc ->
          "uncaught exception: "^(Utils_js.fmt_exc exc)
      in
      mk_error ~trace_infos ~kind:InternalError [loc, [
        spf "Internal error: %s" msg
      ]]

  | EUnsupportedSyntax (loc, unsupported_syntax) ->
      let msg = match unsupported_syntax with
        | ComprehensionExpression
        | GeneratorExpression
        | MetaPropertyExpression ->
            "not (sup)ported"
        | ObjectPropertyLiteralNonString ->
            "non-string literal property keys not supported"
        | ObjectPropertyGetSet ->
            "get/set properties not yet supported"
        | ObjectPropertyComputedGetSet ->
            "computed getters and setters are not yet supported"
        | InvariantSpreadArgument ->
            "unsupported arguments in call to invariant()"
        | ClassPropertyLiteral ->
            "literal properties not yet supported"
        | ClassPropertyComputed ->
            "computed property keys not supported"
        | ReactCreateClassPropertyNonInit ->
            "unsupported property specification in createClass"
        | RequireDynamicArgument ->
            "The parameter passed to require() must be a literal string."
        | ImportDynamicArgument ->
            "The parameter passed to import() must be a literal string."
        | RequireLazyDynamicArgument ->
            "The first arg to requireLazy() must be a literal array of \
             string literals!"
        | CatchParameterAnnotation ->
            "type annotations for catch params not yet supported"
        | CatchParameterDeclaration ->
            "unsupported catch parameter declaration"
        | DestructuringObjectPropertyLiteralNonString ->
            "unsupported non-string literal object property in destructuring"
        | DestructuringExpressionPattern ->
            "unsupported expression pattern in destructuring"
        | PredicateDeclarationForImplementation ->
            "Cannot declare predicate when a function body is present."
        | PredicateDeclarationWithoutExpression ->
            "Predicate function declarations need to declare a predicate \
             expression."
        | PredicateDeclarationAnonymousParameters ->
            "Predicate function declarations cannot use anonymous function \
             parameters."
        | PredicateInvalidBody ->
            "Invalid body for predicate function. Expected a simple return \
             statement as body."
        | PredicateVoidReturn ->
            "Predicate functions need to return non-void."
        | MultipleIndexers ->
            "multiple indexers are not supported"
        | SpreadArgument ->
            "A spread argument is unsupported here"
      in
      mk_error ~trace_infos [loc, [msg]]

  | EIllegalName loc ->
      mk_error ~trace_infos [loc, ["illegal name"]]

  | EUseArrayLiteral loc ->
      mk_error ~trace_infos [loc, [
        "Use array literal instead of new Array(..)"
      ]]

  | EMissingAnnotation reason ->
      mk_error ~trace_infos [mk_info reason ["Missing annotation"]]

  | EBindingError (binding_error, loc, x, entry) ->
      let msg =
        match binding_error with
        | ENameAlreadyBound ->
            "name is already bound"
        | EReferencedBeforeDeclaration ->
            spf
              "%s referenced before declaration, or after skipped declaration"
              (Scope.Entry.string_of_kind entry)
        | ETypeInValuePosition ->
            "type referenced from value position"
        | ETypeAliasInValuePosition ->
            "type alias referenced from value position"
        | EConstReassigned
        | EImportReassigned ->
            spf "%s cannot be reassigned" (Scope.Entry.string_of_kind entry)
        | EConstParamReassigned ->
            spf
              "%s cannot be reassigned \
               (see experimental.const_params=true in .flowconfig)"
              (Scope.Entry.string_of_kind entry)
      in
      mk_error ~trace_infos [
        loc, [x; msg];
        Scope.Entry.entry_loc entry, [
          spf "%s %s" (Scope.Entry.string_of_kind entry) x
        ]
      ]

  | ERecursionLimit reasons ->
      typecheck_error ~kind:RecursionLimitError "*** Recursion limit exceeded ***" reasons

  | EModuleOutsideRoot (loc, package_relative_to_root) ->
      let msg = spf
        "This modules resolves to %S, which is outside both your root \
         directory and all of the entries in the [include] section of your \
         .flowconfig. You should either add this directory to the [include] \
         section of your .flowconfig, move your .flowconfig file higher in \
         the project directory tree, or move this package under your Flow \
         root directory."
        package_relative_to_root
      in
      mk_error ~trace_infos [loc, [msg]]

  | EExperimentalDecorators loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Experimental decorator usage";
        "Decorators are an early stage proposal that may change. \
         Additionally, Flow does not account for the type implications \
         of decorators at this time."
      ]]

  | EExperimentalClassProperties (loc, static) ->
      let config_name, config_key =
        if static
        then "class static field", "class_static_fields"
        else "class instance field", "class_instance_fields"
      in
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        spf "Experimental %s usage" config_name;
        spf
          "%ss are an active early stage feature proposal that may change. \
           You may opt-in to using them anyway in Flow by putting \
           `esproposal.%s=enable` into the [options] section of your \
           .flowconfig."
          (String.capitalize_ascii config_name)
          config_key
      ]]

  | EUnsafeGetSet loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Potentially unsafe get/set usage";
        "Getters and setters with side effects are potentially unsafe and \
         disabled by default. You may opt-in to using them anyway by putting \
         `unsafe.enable_getters_and_setters=true` into the [options] section \
         of your .flowconfig.";
      ]]

  | EExperimentalExportStarAs loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Experimental `export * as` usage";
        "`export * as` is an active early stage feature proposal that may \
         change. You may opt-in to using it anyway by putting \
         `esproposal.export_star_as=enable` into the [options] section \
         of your .flowconfig";
      ]]

  | EIndeterminateModuleType loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        "Unable to determine module type (CommonJS vs ES) if both an export \
         statement and module.exports are used in the same module!"
      ]]

  | EUnreachable loc ->
      mk_error ~trace_infos ~kind:InferWarning [loc, ["unreachable code"]]

  | EInvalidObjectKit { tool; reason; reason_op; use_op } ->
      let open Object in
      let msg = match tool with
        | ReadOnly -> "Cannot create an object with read-only properties from"
        | Spread _ -> "Cannot spread properties from"
        | Rest (_, state) ->
          let open Object.Rest in
          (match state with
            | One _ -> "Cannot remove properties from"
            | Done _ -> "Cannot remove properties with")
        | ReactConfig state ->
          let open Object.ReactConfig in
          (match state with
            | Config _ -> "Cannot compare React props with"
            | Defaults _ -> "Cannot use React default props from")
      in
      let extra, msgs = unwrap_use_ops ((reason_op, reason), [], msg) use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EInvalidTypeof (loc, typename) ->
      mk_error ~trace_infos ~kind:InferWarning [loc, [
        spf "string literal `%s`" typename;
        "This value is not a valid `typeof` return value"
      ]]

  | EArithmeticOperand reason ->
      let msg = "The operand of an arithmetic operation must be a number." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EBinaryInLHS reason ->
      (* TODO: or symbol *)
      let msg =
        "The left-hand side of an `in` expression must be a \
         string or number." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EBinaryInRHS reason ->
      let msg =
        "The right-hand side of an `in` expression must be an \
         object or array." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EForInRHS reason ->
      let msg =
        "The right-hand side of a `for...in` statement must be an \
         object, null or undefined." in
      mk_error ~trace_infos [mk_info reason [msg]]

  | EObjectComputedPropertyAccess reasons ->
      typecheck_error "Computed property cannot be accessed with" reasons

  | EObjectComputedPropertyAssign reasons ->
      typecheck_error "Computed property cannot be assigned with" reasons

  | EInvalidLHSInAssignment loc ->
      let msg = "Invalid left-hand side in assignment expression" in
      mk_error ~trace_infos [loc, [msg]]

  | EIncompatibleWithUseOp (l_reason, u_reason, use_op) ->
      let ((l_reason, u_reason), use_op) =
        dedupe_by_flip (l_reason, u_reason) use_op in
      let extra, msgs =
        let msg = "This type is incompatible with" in
        unwrap_use_ops ((l_reason, u_reason), [], msg) use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EUnsupportedImplements reason ->
      mk_error ~trace_infos [mk_info reason [
        "Argument to implements clause must be an interface"]]

  | EReactKit (reasons, tool, use_op) ->
      let open React in
      let expected_prop_type = "Expected a React PropType instead of" in
      let resolve_object prop = function
      | ResolveObject -> "Expected an object instead of"
      | ResolveDict _ -> prop
      | ResolveProp _ -> prop
      in
      let resolve_array elem = function
      | ResolveArray -> "Expected an array instead of"
      | ResolveElem _ -> elem
      in
      let simplify_prop_type = SimplifyPropType.(function
      | ArrayOf -> expected_prop_type
      | InstanceOf -> "Expected a class type instead of"
      | ObjectOf -> expected_prop_type
      | OneOf tool -> resolve_array "Expected a literal type instead of" tool
      | OneOfType tool -> resolve_array expected_prop_type tool
      | Shape tool -> resolve_object expected_prop_type tool
      ) in
      let create_class = CreateClass.(function
      | Spec _ ->
        "Expected an exact object instead of"
      | Mixins _ ->
        "`mixins` should be a tuple instead of"
      | Statics _ ->
        "`statics` should be an object instead of"
      | PropTypes (_, tool) ->
        resolve_object expected_prop_type tool
      | DefaultProps _ ->
        "`defaultProps` should be an object instead of"
      | InitialState _ ->
        "`initialState` should be an object or null instead of"
      ) in
      let msg = match tool with
      | SimplifyPropType (tool, _) -> simplify_prop_type tool
      | GetProps _ -> "Expected React component instead of"
      | GetConfig _ -> "Expected React component instead of"
      | GetRef _ -> "Expected React component instead of"
      | CreateClass (tool, _, _) -> create_class tool
      | CreateElement _ -> "Expected React component instead of"
      in
      let extra, msgs = unwrap_use_ops (reasons, [], msg) use_op in
      typecheck_error_with_core_infos ~extra msgs

  | EReactElementFunArity (reason, fn, n) ->
      mk_error ~trace_infos [mk_info reason [
        "React." ^ fn ^ "() must be passed at least " ^ (string_of_int n) ^ " arguments."
      ]]

  | EFunctionCallExtraArg (unused_reason, def_reason, param_count, use_op) ->
    let msg = match param_count with
    | 0 -> "No arguments are expected by"
    | 1 -> "No more than 1 argument is expected by"
    | n -> spf "No more than %d arguments are expected by" n
    in
    let extra, msgs = unwrap_use_ops ((unused_reason, def_reason), [], msg) use_op in
    typecheck_error_with_core_infos ~extra msgs

  | EUnsupportedSetProto reason ->
      mk_error ~trace_infos [mk_info reason [
        "Prototype mutation not allowed"]]

  | EDuplicateModuleProvider {module_name; provider; conflict} ->
      mk_error ~kind:DuplicateProviderError [
        Loc.({ none with source = Some conflict }), [
          module_name; "Duplicate module provider"];
        Loc.({ none with source = Some provider }), [
          "current provider"]
      ]

  | EParseError (loc, parse_error) ->
    mk_error ~kind:ParseError [loc, [Parse_error.PP.error parse_error]]

  | EDocblockError (loc, err) ->
    let msg = match err with
    | MultipleFlowAttributes ->
      "Unexpected @flow declaration. Only one per file is allowed."
    | MultipleProvidesModuleAttributes ->
      "Unexpected @providesModule declaration. Only one per file is allowed."
    | MultipleJSXAttributes ->
      "Unexpected @jsx declaration. Only one per file is allowed."
    | InvalidJSXAttribute first_error ->
      "Invalid @jsx declaration. Should have form `@jsx LeftHandSideExpression` "^
      "with no spaces."^
      (match first_error with
      | None -> ""
      | Some first_error -> spf " Parse error: %s" first_error)
    in
    mk_error ~kind:ParseError [loc, [msg]]

  | EUntypedTypeImport (loc, module_name) ->
    mk_error
      ~kind:(LintError Lints.UntypedTypeImport)
      [loc, [spf (
        "Importing a type from an untyped module makes it `any` and is not safe! "^^
        "Did you mean to add `// @flow` to the top of `%s`?"
      ) module_name]]

  | EUntypedImport (loc, module_name) ->
    mk_error
      ~kind:(LintError Lints.UntypedImport)
      [loc, [spf (
        "Importing from an untyped module makes it `any` and is not safe! "^^
        "Did you mean to add `// @flow` to the top of `%s`?"
      ) module_name]]

  | ENonstrictImport loc ->
    mk_error
      ~kind:(LintError Lints.NonstrictImport)
      [loc, ["Dependencies of a strict module must also be strict!"]]

  | EUnclearType loc ->
    mk_error
      ~kind:(LintError Lints.UnclearType)
      [loc, ["Unclear type. Using `any`, `Object` or `Function` types is not safe!"]]
  | EUnsafeGettersSetters loc ->
    mk_error
      ~kind:(LintError Lints.UnsafeGettersSetters)
      [loc, ["Getters and Setters can have side effects and are unsafe"]]

  | EDeprecatedDeclareExports loc ->
    mk_error
      ~kind:(LintError Lints.DeprecatedDeclareExports)
      [loc, ["Deprecated syntax. Use `declare module.exports` instead."]]

  | EUnusedSuppression loc ->
    mk_error [loc, ["Error suppressing comment"; "Unused suppression"]]

  | ELintSetting (loc, kind) ->
    let msg = match kind with
    | LintSettings.Redundant_argument ->
      "Redundant argument. This argument doesn't change any lint settings."
    | LintSettings.Overwritten_argument ->
      "Redundant argument. "
        ^ "The values set by this argument are overwritten later in this comment."
    | LintSettings.Naked_comment ->
      "Malformed lint rule. At least one argument is required."
    | LintSettings.Nonexistent_rule ->
      "Nonexistent/misspelled lint rule. Perhaps you have a missing/extra ','?"
    | LintSettings.Invalid_setting ->
      "Invalid setting. Valid settings are error, warn, and off."
    | LintSettings.Malformed_argument ->
      "Malformed lint rule. Properly formed rules contain a single ':' character. " ^
        "Perhaps you have a missing/extra ','?"
    in
    mk_error ~kind: ParseError [loc, [msg]]

  | ESketchyNullLint { kind; loc; null_loc; falsy_loc } ->
    let type_str, value_str = match kind with
    | Lints.SketchyBool -> "boolean", "Potentially false"
    | Lints.SketchyNumber -> "number", "Potentially 0"
    | Lints.SketchyString -> "string", "Potentially \"\""
    | Lints.SketchyMixed -> "mixed", "Mixed"
    in
    mk_error
      ~kind:(LintError (Lints.SketchyNull kind))
      [loc, [(spf "Sketchy null check on %s value." type_str)
        ^ " Perhaps you meant to check for null instead of for existence?"]]
      ~extra:[InfoLeaf [
        null_loc, ["Potentially null/undefined value."];
        falsy_loc, [spf "%s value." value_str]
      ]]
  | EInvalidPrototype reason ->
      mk_error ~trace_infos [mk_info reason [
        "Invalid prototype. Expected an object or null."]]
