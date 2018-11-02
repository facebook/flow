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
      use_op: use_op option;
      branches: (Reason.t * error_message) list;
    }
  | EIncompatibleDefs of {
      use_op: use_op;
      reason_lower: reason;
      reason_upper: reason;
      branches: (Reason.t * error_message) list;
    }
  | EIncompatibleProp of {
      prop: string option;
      reason_prop: reason;
      reason_obj: reason;
      special: lower_kind option;
      use_op: use_op option;
    }
  | EDebugPrint of reason * string
  | EImportValueAsType of reason * string
  | EImportTypeAsTypeof of reason * string
  | EImportTypeAsValue of reason * string
  | ERefineAsValue of reason * string
  | ENoDefaultExport of reason * string * string option
  | EOnlyDefaultExport of reason * string * string
  | ENoNamedExport of reason * string * string * string option
  | EMissingTypeArgs of { reason_tapp: reason; reason_arity: reason; min_arity: int; max_arity: int }
  | EValueUsedAsType of (reason * reason)
  | EExpectedStringLit of (reason * reason) * string * string Type.literal * use_op
  | EExpectedNumberLit of
      (reason * reason) *
      Type.number_literal *
      Type.number_literal Type.literal *
      use_op
  | EExpectedBooleanLit of (reason * reason) * bool * bool option * use_op
  | EPropNotFound of string option * (reason * reason) * use_op
  | EPropAccess of (reason * reason) * string option * Type.polarity * Type.rw * use_op
  | EPropPolarityMismatch of (reason * reason) * string option * (Type.polarity * Type.polarity) * use_op
  | EPolarityMismatch of {
      reason: reason;
      name: string;
      expected_polarity: Type.polarity;
      actual_polarity: Type.polarity;
    }
  | EStrictLookupFailed of (reason * reason) * reason * string option * use_op option
  | EPrivateLookupFailed of (reason * reason) * string * use_op
  | EAdditionMixed of reason * use_op
  | EComparison of (reason * reason)
  | ETupleArityMismatch of (reason * reason) * int * int * use_op
  | ENonLitArrayToTuple of (reason * reason) * use_op
  | ETupleOutOfBounds of (reason * reason) * int * int * use_op
  | ETupleUnsafeWrite of (reason * reason) * use_op
  | EUnionSpeculationFailed of {
      use_op: use_op;
      reason: reason;
      reason_op: reason;
      branches: (reason * error_message) list;
    }
  | ESpeculationAmbiguous of (reason * reason) * (int * reason) * (int * reason) * reason list
  | EIncompatibleWithExact of (reason * reason) * use_op
  | EUnsupportedExact of (reason * reason)
  | EIdxArity of reason
  | EIdxUse1 of reason
  | EIdxUse2 of reason
  | EUnexpectedThisType of Loc.t
  | ETypeParamArity of Loc.t * int
  | ECallTypeArity of {
      call_loc: Loc.t;
      is_new: bool;
      reason_arity: reason;
      expected_arity: int;
    }
  | ETypeParamMinArity of Loc.t * int
  | ETooManyTypeArgs of reason * reason * int
  | ETooFewTypeArgs of reason * reason * int
  | EInvalidTypeArgs of reason * reason
  | EPropertyTypeAnnot of Loc.t
  | EExportsAnnot of Loc.t
  | ECharSetAnnot of Loc.t
  | EInvalidCharSet of { invalid: reason * InvalidCharSetSet.t; valid: reason; use_op: use_op }
  | EUnsupportedKeyInObjectType of Loc.t
  | EPredAnnot of Loc.t
  | ERefineAnnot of Loc.t
  | EUnexpectedTypeof of Loc.t
  | EFunPredCustom of (reason * reason) * string
  | EFunctionIncompatibleWithShape of reason * reason * use_op
  | EInternal of Loc.t * internal_error
  | EUnsupportedSyntax of Loc.t * unsupported_syntax
  | EUseArrayLiteral of Loc.t
  | EMissingAnnotation of reason * reason list
  | EBindingError of binding_error * Loc.t * string * Scope.Entry.t
  | ERecursionLimit of (reason * reason)
  | EModuleOutsideRoot of Loc.t * string
  | EExperimentalDecorators of Loc.t
  | EExperimentalClassProperties of Loc.t * bool
  | EUnsafeGetSet of Loc.t
  | EExperimentalExportStarAs of Loc.t
  | EIndeterminateModuleType of Loc.t
  | EBadExportPosition of Loc.t
  | EBadExportContext of string * Loc.t
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
  | EDeprecatedType of Loc.t
  | EUnsafeGettersSetters of Loc.t
  | EUnusedSuppression of Loc.t
  | ELintSetting of LintSettings.lint_parse_error
  | ESketchyNullLint of {
      kind: Lints.sketchy_null_kind;
      loc: Loc.t;
      null_loc: Loc.t;
      falsy_loc: Loc.t;
    }
  | ESketchyNumberLint of Lints.sketchy_number_kind * reason
  | EInvalidPrototype of reason
  | EExperimentalOptionalChaining of Loc.t
  | EOptionalChainingMethods of Loc.t
  | EUnnecessaryOptionalChain of Loc.t * reason
  | EUnnecessaryInvariant of Loc.t * reason
  | EInexactSpread of reason * reason
  | EDeprecatedCallSyntax of Loc.t
  | EUnexpectedTemporaryBaseType of Loc.t
  | ESignatureVerification of Signature_builder_deps.With_Loc.Error.t

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
  | UnexpectedTypeapp of string

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
  | IllegalName
  | UnsupportedInternalSlot of { name: string; static: bool }

and lower_kind =
  | Possibly_null
  | Possibly_void
  | Possibly_null_or_void
  | Incompatible_intersection

and upper_kind =
  | IncompatibleGetPropT of Loc.t * string option
  | IncompatibleSetPropT of Loc.t * string option
  | IncompatibleMatchPropT of Loc.t * string option
  | IncompatibleGetPrivatePropT
  | IncompatibleSetPrivatePropT
  | IncompatibleMethodT of Loc.t * string option
  | IncompatibleCallT
  | IncompatibleConstructorT
  | IncompatibleGetElemT of Loc.t
  | IncompatibleSetElemT of Loc.t
  | IncompatibleCallElemT of Loc.t
  | IncompatibleElemTOfArrT
  | IncompatibleObjAssignFromTSpread
  | IncompatibleObjAssignFromT
  | IncompatibleObjRestT
  | IncompatibleObjSealT
  | IncompatibleArrRestT
  | IncompatibleSuperT
  | IncompatibleMixinT
  | IncompatibleSpecializeT
  | IncompatibleThisSpecializeT
  | IncompatibleVarianceCheckT
  | IncompatibleGetKeysT
  | IncompatibleHasOwnPropT of Loc.t * string option
  | IncompatibleGetValuesT
  | IncompatibleUnaryMinusT
  | IncompatibleMapTypeTObject
  | IncompatibleTypeAppVarianceCheckT
  | IncompatibleGetStaticsT
  | IncompatibleUnclassified of string

let desc_of_reason r = Reason.desc_of_reason ~unwrap:(is_scalar_reason r) r

(* A utility function for getting and updating the use_op in error messages. *)
let util_use_op_of_msg nope util = function
| EIncompatible {use_op; lower; upper; branches} ->
  Option.value_map use_op ~default:nope ~f:(fun use_op ->
    util use_op (fun use_op ->
      EIncompatible {use_op=Some use_op; lower; upper; branches}))
| EIncompatibleDefs {use_op; reason_lower; reason_upper; branches} ->
  util use_op (fun use_op ->
    EIncompatibleDefs {use_op; reason_lower; reason_upper; branches})
| EIncompatibleProp {use_op; prop; reason_prop; reason_obj; special} ->
  Option.value_map use_op ~default:nope ~f:(fun use_op ->
    util use_op (fun use_op ->
      EIncompatibleProp {use_op=Some use_op; prop; reason_prop; reason_obj; special}))
| EExpectedStringLit (rs, u, l, op) -> util op (fun op -> EExpectedStringLit (rs, u, l, op))
| EExpectedNumberLit (rs, u, l, op) -> util op (fun op -> EExpectedNumberLit (rs, u, l, op))
| EExpectedBooleanLit (rs, u, l, op) -> util op (fun op -> EExpectedBooleanLit (rs, u, l, op))
| EPropNotFound (prop, rs, op) -> util op (fun op -> EPropNotFound (prop, rs, op))
| EPropAccess (rs, prop, p, rw, op) -> util op (fun op -> EPropAccess (rs, prop, p, rw, op))
| EPropPolarityMismatch (rs, p, ps, op) -> util op (fun op -> EPropPolarityMismatch (rs, p, ps, op))
| EStrictLookupFailed (rs, r, p, Some op) ->
  util op (fun op -> EStrictLookupFailed (rs, r, p, Some op))
| EPrivateLookupFailed (rs, x, op) -> util op (fun op -> EPrivateLookupFailed (rs, x, op))
| EAdditionMixed (r, op) -> util op (fun op -> EAdditionMixed (r, op))
| ETupleArityMismatch (rs, x, y, op) -> util op (fun op -> ETupleArityMismatch (rs, x, y, op))
| ENonLitArrayToTuple (rs, op) -> util op (fun op -> ENonLitArrayToTuple (rs, op))
| ETupleOutOfBounds (rs, l, i, op) -> util op (fun op -> ETupleOutOfBounds (rs, l, i, op))
| ETupleUnsafeWrite (rs, op) -> util op (fun op -> ETupleUnsafeWrite (rs, op))
| EUnionSpeculationFailed {use_op; reason; reason_op; branches} ->
  util use_op (fun use_op -> EUnionSpeculationFailed {use_op; reason; reason_op; branches})
| EIncompatibleWithExact (rs, op) -> util op (fun op -> EIncompatibleWithExact (rs, op))
| EInvalidCharSet {invalid; valid; use_op} ->
  util use_op (fun use_op -> EInvalidCharSet {invalid; valid; use_op})
| EFunctionIncompatibleWithShape (l, u, use_op) ->
  util use_op (fun use_op -> EFunctionIncompatibleWithShape (l, u, use_op))
| EInvalidObjectKit {tool; reason; reason_op; use_op} ->
  util use_op (fun use_op -> EInvalidObjectKit {tool; reason; reason_op; use_op})
| EIncompatibleWithUseOp (rl, ru, op) -> util op (fun op -> EIncompatibleWithUseOp (rl, ru, op))
| EReactKit (rs, t, op) -> util op (fun op -> EReactKit (rs, t, op))
| EFunctionCallExtraArg (rl, ru, n, op) -> util op (fun op -> EFunctionCallExtraArg (rl, ru, n, op))
| EDebugPrint (_, _)
| EImportValueAsType (_, _)
| EImportTypeAsTypeof (_, _)
| EImportTypeAsValue (_, _)
| ERefineAsValue (_, _)
| ENoDefaultExport (_, _, _)
| EOnlyDefaultExport (_, _, _)
| ENoNamedExport (_, _, _, _)
| EMissingTypeArgs {reason_tapp=_; reason_arity=_; min_arity=_; max_arity=_}
| EValueUsedAsType (_, _)
| EPolarityMismatch {reason=_; name=_; expected_polarity=_; actual_polarity=_}
| EStrictLookupFailed (_, _, _, None)
| EComparison (_, _)
| ESpeculationAmbiguous (_, _, _, _)
| EUnsupportedExact (_, _)
| EIdxArity (_)
| EIdxUse1 (_)
| EIdxUse2 (_)
| EUnexpectedThisType (_)
| ETypeParamArity (_, _)
| ECallTypeArity _
| ETypeParamMinArity (_, _)
| ETooManyTypeArgs (_, _, _)
| ETooFewTypeArgs (_, _, _)
| EInvalidTypeArgs (_, _)
| EPropertyTypeAnnot (_)
| EExportsAnnot (_)
| ECharSetAnnot (_)
| EUnsupportedKeyInObjectType (_)
| EPredAnnot (_)
| ERefineAnnot (_)
| EUnexpectedTypeof (_)
| EFunPredCustom (_, _)
| EInternal (_, _)
| EUnsupportedSyntax (_, _)
| EUseArrayLiteral (_)
| EMissingAnnotation (_)
| EBindingError (_, _, _, _)
| ERecursionLimit (_, _)
| EModuleOutsideRoot (_, _)
| EExperimentalDecorators (_)
| EExperimentalClassProperties (_, _)
| EUnsafeGetSet (_)
| EExperimentalExportStarAs (_)
| EIndeterminateModuleType (_)
| EBadExportPosition (_)
| EBadExportContext (_)
| EUnreachable (_)
| EInvalidTypeof (_, _)
| EBinaryInLHS (_)
| EBinaryInRHS (_)
| EArithmeticOperand (_)
| EForInRHS (_)
| EObjectComputedPropertyAccess (_, _)
| EObjectComputedPropertyAssign (_, _)
| EInvalidLHSInAssignment (_)
| EUnsupportedImplements (_)
| EReactElementFunArity (_, _, _)
| EUnsupportedSetProto (_)
| EDuplicateModuleProvider {module_name=_; provider=_; conflict=_}
| EParseError (_, _)
| EDocblockError (_, _)
| EUntypedTypeImport (_, _)
| EUntypedImport (_, _)
| ENonstrictImport (_)
| EUnclearType (_)
| EDeprecatedType (_)
| EUnsafeGettersSetters (_)
| EUnusedSuppression (_)
| ELintSetting (_)
| ESketchyNullLint {kind=_; loc=_; null_loc=_; falsy_loc=_}
| ESketchyNumberLint _
| EInvalidPrototype (_)
| EExperimentalOptionalChaining _
| EOptionalChainingMethods _
| EUnnecessaryOptionalChain _
| EUnnecessaryInvariant _
| EInexactSpread _
| EDeprecatedCallSyntax _
| EUnexpectedTemporaryBaseType _
| ESignatureVerification _
  -> nope

(* Rank scores for signals of different strength on an x^2 scale so that greater
 * signals dominate lesser signals. *)
let reason_score = 100
let frame_score = reason_score * 2
let type_arg_frame_score = frame_score * 2
let tuple_element_frame_score = type_arg_frame_score * 2
let property_sentinel_score = tuple_element_frame_score * 2

(* Gets the score of a use_op. Used in score_of_msg. See the comment on
 * score_of_msg to learn more about scores.
 *
 * Calculated by taking the count of all the frames. *)
let score_of_use_op use_op =
  let score = fold_use_op
    (* Comparing the scores of use_ops only works when they all have the same
     * root_use_op! If two use_ops have different roots, we can't realistically
     * compare the number of frames since the basis is completely different.
     *
     * So we require a Speculation root use_op to be passed into score_of_use_op
     * and we perform a structural equality check using that.
     *
     * Otherwise, the total score from score_of_use_op is -1. This way, errors
     * which match Speculation will be promoted. It is more likely the user was
     * trying to target these branches. *)
    (function
    | Speculation _ -> Ok 0
    | _ -> Error (-1))

    (fun acc frame -> match acc with Error _ -> acc | Ok acc ->
      Ok (acc + (match frame with
      (* Later params that error get a higher score. This roughly represents how
       * much type-checking work Flow successfully completed before erroring.
       * Useful for basically only overloaded function error messages.
       *
       * The signal that this gives us is that we successfully type checked n
       * params in the call before erroring. If there was no error, Flow may
       * have gone to successfully check another m params. However, we will
       * never know that. n is our best approximation. It rewards errors near
       * the end of a call and punishes (slightly) errors near the beginning of
       * a call.
       *
       * This, however, turns out to be consistent with code style in modern
       * JavaScript. As an unspoken convention, more complex arguments usually
       * go last. For overloaded functions, the switching generally happens on
       * the first argument. The "tag". This gives us confidence that n on
       * FunParam is a good heuristic for the score.
       *
       * FunRestParam is FunParam, but at the end. So give it a larger score
       * then FunParam after adding n.
       *
       * We do _not_ add n to the score if this use_op was added to an implicit type parameter. *)
      | FunParam {n; _} -> frame_score + n
      | FunRestParam _ -> frame_score + frame_score - 1
      (* FunCompatibility is generally followed by another use_op. So let's not
       * count FunCompatibility. *)
      | FunCompatibility _ -> 0
      (* FunMissingArg means the error is *less* likely to be correct. *)
      | FunMissingArg _ -> 0
      (* Higher signal then PropertyCompatibility, for example. *)
      | TypeArgCompatibility _ -> type_arg_frame_score
      | ArrayElementCompatibility _ -> type_arg_frame_score
      (* Higher signal then TypeArgCompatibility. *)
      | TupleElementCompatibility _ -> tuple_element_frame_score
      (* If we error-ed on a sentinel prop compatibility then tank the score of
       * this use_op. This is so that the score of errors which passed sentinel
       * compatibility are always picked relative to the score of errors which
       * failed their sentinel prop checks. *)
      | PropertyCompatibility {is_sentinel=true; _} -> -property_sentinel_score
      (* ImplicitTypeParam is an internal marker use_op that doesn't get
       * rendered in error messages. So it doesn't necessarily signal anything
       * about the user's intent. *)
      | ImplicitTypeParam _ -> 0
      | _ -> frame_score)))
    use_op
  in
  match score with
  | Ok n -> n
  | Error n -> n

(* Gets the score of an error message. The score is an approximation of how
 * close the user was to getting their code right. A higher score means the user
 * was closer then a lower score. A score of 0 means we have no signal about
 * how close the user was. For example, consider the following two flows:
 *
 *     number ~> {p: string}
 *
 *     {p: number} ~> {p: string}
 *
 * Clearly, the user was closer to being correct with the second flow. So this
 * function should assign the number ~> string error a higher score then the
 * number ~> object error.
 *
 * Now consider:
 *
 *     number ~> string
 *
 *     number ~> {p: string}
 *
 * This time we kept the lower bound the same and changed the upper bound. The
 * first flow is this time is closer to the user's intent then the second flow.
 * So we give the number ~> string message a higher score then the
 * number ~> object message.
 *
 * This scoring mechanism is useful for union and intersection error messages
 * where we want to approximate which branch the user meant to target with
 * their code. Branches with higher scores have a higher liklihood of being
 * the branch the user was targeting. *)
let score_of_msg msg =
  (* Start by getting the score based off the use_op of our error message. If
   * the message does not have a use_op then we return 0. This score
   * contribution declares that greater complexity in the use is more likely to
   * cause a match. *)
  let score = util_use_op_of_msg 0 (fun op _ -> score_of_use_op op) msg in
  (* Special cases for messages which increment the score. *)
  let score = score + match msg with
  (* If a property doesn't exist, we still use a PropertyCompatibility use_op.
   * This PropertyCompatibility when counted in our score is dishonest since
   * a missing prop does not increase the likelihood that the user was close to
   * the right types. *)
  | EIncompatibleProp {use_op=Some (Frame (PropertyCompatibility _, _)); _}
  | EPropNotFound (_, _, Frame (PropertyCompatibility _, _))
  | EStrictLookupFailed (_, _, _, Some (Frame (PropertyCompatibility _, _)))
    -> -frame_score
  | _
    -> 0
  in
  (* If we have two incompatible types and both incompatible types are scalar or
   * both types are arrays then increment our score. This is based on the belief
   * that the solutions with the lowest possible complexity are closest to each
   * other. e.g. number ~> string. If one type is a scalar or array and the
   * other type is not then we decrement our score. *)
  let score = score + (
    let reasons = match msg with
    | EIncompatibleDefs {reason_lower=rl; reason_upper=ru; branches=[]; use_op=_}
    | EIncompatibleWithUseOp (rl, ru, _)
    | EIncompatibleWithExact ((rl, ru), _)
      -> Some (rl, ru)
    | _
      -> None
    in
    match reasons with
    | Some ((rl, ru)) ->
      if is_nullish_reason rl && is_nullish_reason ru then reason_score else
      (* T ~> null should have a lower score then T ~> scalar *)
      if is_nullish_reason rl || is_nullish_reason ru then 0 else

      if is_scalar_reason rl && is_scalar_reason ru then reason_score else
      if is_scalar_reason rl || is_scalar_reason ru then 1 else

      if is_array_reason rl && is_array_reason ru then reason_score else
      if is_array_reason rl || is_array_reason ru then 1 else
      reason_score
    | None ->
      reason_score
  ) in
  score

(* Decide reason order based on UB's flavor and blamability.
   If the order is unchanged, maintain reference equality. *)
let ordered_reasons ((rl, ru) as reasons) =
  if (is_blamable_reason ru && not (is_blamable_reason rl))
  then ru, rl
  else reasons

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
    aloc_of_reason reason, desc :: extras
  in

  let info_of_reason r = mk_info r [] in

  let trace_infos = List.map info_of_reason trace_reasons in

  (* Flip the lower/upper reasons of a frame_use_op. *)
  let flip_frame = function
  | ArrayElementCompatibility c -> ArrayElementCompatibility {lower = c.upper; upper = c.lower}
  | FunCompatibility c -> FunCompatibility {lower = c.upper; upper = c.lower}
  | FunParam c -> FunParam {c with lower = c.upper; upper = c.lower}
  | FunRestParam c -> FunRestParam {lower = c.upper; upper = c.lower}
  | FunReturn c -> FunReturn {lower = c.upper; upper = c.lower}
  | IndexerKeyCompatibility c -> IndexerKeyCompatibility {lower = c.upper; upper = c.lower}
  | PropertyCompatibility c -> PropertyCompatibility {c with lower = c.upper; upper = c.lower}
  | ReactConfigCheck -> ReactConfigCheck
  | TupleElementCompatibility c ->
    TupleElementCompatibility {c with lower = c.upper; upper = c.lower}
  | TypeArgCompatibility c -> TypeArgCompatibility {c with lower = c.upper; upper = c.lower}
  | TypeParamBound _
  | FunMissingArg _
  | ImplicitTypeParam _
  | UnifyFlip
    as use_op -> use_op
  in

  (* Unification produces two errors. One for both sides. For example,
   * {p: number} ~> {p: string} errors on both number ~> string and
   * string ~> number. Showing both errors to our user is often redundant.
   * So we use this utility to flip the string ~> number case and produce an
   * error identical to one we've produced before. These two errors will be
   * deduped in our ErrorSet. *)
  let dedupe_by_flip =
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

  (* In friendly error messages, we always want to point to a value as the
   * primary location. Or an annotation on a value. Normally, values are found
   * in the lower bound. However, in contravariant positions this flips. In this
   * function we normalize the lower/upper variables in use_ops so that lower
   * always points to the value. Example:
   *
   *     ((x: number) => {}: (x: string) => void);
   *
   * We want to point to number. However, number is in the upper position since
   * number => void ~> string => void flips arguments to string ~> number. This
   * function flips contravariant positions like function arguments back. *)
  let flip_contravariant =
    (* Is this frame part of a contravariant position? *)
    let is_contravariant = function
    | FunParam _, Frame (FunCompatibility _, _) -> (true, true)
    | FunRestParam _, Frame (FunCompatibility _, _) -> (true, true)
    | TypeArgCompatibility {polarity = Negative; _}, _ -> (true, false)
    | _ -> (false, false)
    in
    let is_contravariant_root = function
    | FunImplicitReturn _ -> true
    | _ -> false
    in
    (* Loop through the use_op and flip the contravariants. *)
    let rec loop = function
    | Op root_use_op as use_op -> (is_contravariant_root root_use_op, use_op)
    (* If the frame is contravariant then flip. *)
    | Frame (frame, use_op) ->
      let (flip, use_op) = loop use_op in
      let (contravariant, flip_self) = is_contravariant (frame, use_op) in
      let flip = if contravariant then not flip else flip in
      let flip_self = flip && (not contravariant || flip_self) in
      let frame = if flip_self then flip_frame frame else frame in
      (flip, Frame (frame, use_op))
    in
    fun (lower, upper) use_op ->
      let (flip, use_op) = loop use_op in
      if flip
        then ((upper, lower), use_op)
        else ((lower, upper), use_op)
  in

  let text = Friendly.text in
  let code = Friendly.code in
  let ref = Friendly.ref in
  let desc = Friendly.ref ~loc:false in

  (* Unwrap a use_op for the friendly error format. Takes the smallest location
   * where we found the error and a use_op which we will unwrap. *)
  let unwrap_use_ops =
    let open Friendly in
    let rec loop loc frames use_op =
      let action = match use_op with
      | Op UnknownUse
      | Op (Internal _)
        -> `UnknownRoot false

      | Op (Speculation _) ->
        `UnknownRoot true

      | Op (Addition {op; left; right}) ->
        `Root (op, None,
          [text "Cannot add "; desc left; text " and "; desc right])

      | Op (AssignVar {var; init}) ->
        `Root (init, None, match var with
        | Some var -> [text "Cannot assign "; desc init; text " to "; desc var]
        | None -> [text "Cannot assign "; desc init; text " to variable"])

      | Op Cast {lower; upper} ->
        `Root (lower, None,
          [text "Cannot cast "; desc lower; text " to "; desc upper])

      | Op ClassExtendsCheck {extends; def; _} ->
        `Root (def, None,
          [text "Cannot extend "; ref extends; text " with "; desc def])

      | Op ClassImplementsCheck {implements; def; _} ->
        `Root (def, None,
          [text "Cannot implement "; ref implements; text " with "; desc def])

      | Op ClassOwnProtoCheck {prop; own_loc; proto_loc} ->
        (match own_loc, proto_loc with
        | None, None -> `UnknownRoot true
        | Some loc, None ->
          let def = mk_reason (RProperty (Some prop)) (loc |> ALoc.of_loc) in
          `Root (def, None, [text "Cannot shadow proto property"])
        | None, Some loc ->
          let def = mk_reason (RProperty (Some prop)) (loc |> ALoc.of_loc) in
          `Root (def, None, [text "Cannot define shadowed proto property"])
        | Some own_loc, Some proto_loc ->
          let def = mk_reason (RProperty (Some prop)) (own_loc |> ALoc.of_loc) in
          let proto = mk_reason (RIdentifier prop) (proto_loc |> ALoc.of_loc) in
          `Root (def, None, [text "Cannot shadow proto property "; ref proto]))

      | Op Coercion {from; target} ->
        `Root (from, None,
          [text "Cannot coerce "; desc from; text " to "; desc target])

      | Op (FunCall {op; fn; _}) ->
        `Root (op, Some fn, [text "Cannot call "; desc fn])

      | Op (FunCallMethod {op; fn; prop; _}) ->
        `Root (op, Some prop, [text "Cannot call "; desc fn])

      | Frame (FunParam _, ((Op (Speculation
          (Op (FunCall _ | FunCallMethod _ | JSXCreateElement _)))) as use_op))
        -> `Next use_op

      | Frame (FunParam {n; name; lower = lower'; _},
          Op (FunCall {args; fn; _} | FunCallMethod {args; fn; _})) ->
        let lower = if List.length args > n - 1 then List.nth args (n - 1) else lower' in
        let param = match name with
        | Some name -> code name
        | None -> text (spf "the %s parameter" (Utils_js.ordinal n))
        in
        `Root (lower, None,
          [text "Cannot call "; desc fn; text " with "; desc lower; text " bound to "; param])

      | Op (FunReturnStatement {value}) ->
        `Root (value, None,
          [text "Cannot return "; desc value])

      | Op (FunImplicitReturn {upper; fn}) ->
        `Root (upper, None,
          [text "Cannot expect "; desc upper; text " as the return type of "; desc fn])

      | Op (GeneratorYield {value}) ->
        `Root (value, None,
          [text "Cannot yield "; desc value])

      | Op (GetProperty prop) ->
        `Root (prop, None,
          [text "Cannot get "; desc prop])

      | Frame (FunParam _, Op (JSXCreateElement {op; component; _}))
      | Op (JSXCreateElement {op; component; _}) ->
        `Root (op, Some component,
          [text "Cannot create "; desc component; text " element"])

      | Op (ReactCreateElementCall {op; component; _}) ->
        `Root (op, Some component,
          [text "Cannot create "; desc component; text " element"])

      | Op (ReactGetIntrinsic {literal}) ->
        `Root (literal, None,
          [text "Cannot create "; desc literal; text " element"])

      | Op (TypeApplication {type'}) ->
        `Root (type', None,
          [text "Cannot instantiate "; desc type'])

      | Op (SetProperty {prop; value; lhs; _}) ->
        let loc_reason = if Loc.contains (aloc_of_reason lhs |> ALoc.to_loc) (ALoc.to_loc loc) then lhs else value in
        `Root (loc_reason, None,
          [text "Cannot assign "; desc value; text " to "; desc prop])

      | Frame (ArrayElementCompatibility {lower; _}, use_op) ->
        `Frame (lower, use_op,
          [text "array element"])

      | Frame (FunParam {n; lower; _}, use_op) ->
        `Frame (lower, use_op,
          [text "the "; text (Utils_js.ordinal n); text " argument"])

      | Frame (FunRestParam _, use_op) ->
        `Next use_op

      | Frame (FunReturn {lower; _}, use_op) ->
        `Frame (repos_reason (ALoc.to_loc loc) lower, use_op,
          [text "the return value"])

      | Frame (IndexerKeyCompatibility {lower; _}, use_op) ->
        `Frame (lower, use_op,
          [text "the indexer property's key"])

      | Frame (PropertyCompatibility {prop=None | Some "$key" | Some "$value"; lower; _}, use_op) ->
        `Frame (lower, use_op,
          [text "the indexer property"])

      | Frame (PropertyCompatibility {prop=Some "$call"; lower; _}, use_op) ->
        `Frame (lower, use_op,
          [text "the callable signature"])

      | Frame (PropertyCompatibility {prop=Some prop; lower; _}, use_op) ->
        let repos_small_reason loc reason = function
        (* If we are checking class extensions or implementations then the
         * object reason will point to the class name. So don't reposition with
         * this reason. *)
        | Op (ClassExtendsCheck _) ->  repos_reason loc reason
        | Op (ClassImplementsCheck _) ->  repos_reason loc reason
        | _ -> reason
        in
        let lower = repos_small_reason (ALoc.to_loc loc) lower use_op in
        let rec loop lower = function
        (* Don't match $key/$value/$call properties since they have special
         * meaning. As defined above. *)
        | Frame (PropertyCompatibility {prop=Some prop; lower=lower'; _}, use_op)
            when prop <> "$key" && prop <> "$value" && prop <> "$call" ->
          let lower' = repos_small_reason (aloc_of_reason lower |> ALoc.to_loc) lower' use_op in
          (* Perform the same frame location unwrapping as we do in our
           * general code. *)
          let lower = if
            Loc.contains
              (aloc_of_reason lower' |> ALoc.to_loc)
              (aloc_of_reason lower |> ALoc.to_loc)
            then lower else lower' in
          let (lower, props, use_op) = loop lower use_op in
          (lower, prop::props, use_op)
        (* Perform standard iteration through these use_ops. *)
        | use_op -> (lower, [], use_op)
        in
        (* Loop through our parent use_op to get our property path. *)
        let (lower, props, use_op) = loop lower use_op in
        (* Create our final action. *)
        `Frame (lower, use_op,
          [text "property "; code
            (List.fold_left (fun acc prop -> prop ^ "." ^ acc) prop props)])

      | Frame (TupleElementCompatibility {n; lower; _}, use_op) ->
        `Frame (lower, use_op,
          [text "index "; text (string_of_int (n - 1))])

      | Frame (TypeArgCompatibility {targ; lower; _}, use_op) ->
        `Frame (lower, use_op,
          [text "type argument "; ref targ])

      | Frame (TypeParamBound {name}, use_op) ->
        `FrameWithoutLoc (use_op,
          [text "type argument "; code name])

      | Frame (FunCompatibility {lower; _}, use_op) ->
        `NextWithLoc (lower, use_op)

      | Frame (FunMissingArg _, use_op)
      | Frame (ImplicitTypeParam _, use_op)
      | Frame (ReactConfigCheck, use_op)
      | Frame (UnifyFlip, use_op)
        -> `Next use_op
      in
      match action with
      (* Skip this use_op and go to the next one. *)
      | `Next use_op -> loop loc frames use_op
      (* Skip this use_op, don't add a frame, but do use the loc to reposition
       * our primary location. *)
      | `NextWithLoc (frame_reason, use_op) ->
        (* If our current loc is inside our frame_loc then use our current loc
         * since it is the smallest possible loc in our frame_loc. *)
        let frame_loc = aloc_of_reason frame_reason in
        let loc = if Loc.contains (ALoc.to_loc frame_loc) (ALoc.to_loc loc) then loc else frame_loc in
        loop loc frames use_op
      (* Add our frame message and reposition the location if appropriate. *)
      | `Frame (frame_reason, use_op, frame) ->
        (* If our current loc is inside our frame_loc then use our current loc
         * since it is the smallest possible loc in our frame_loc. *)
        let frame_loc = aloc_of_reason frame_reason in
        let frame_contains_loc = Loc.contains (ALoc.to_loc frame_loc) (ALoc.to_loc loc) in
        let loc = if frame_contains_loc then loc else frame_loc in
        (* Add our frame and recurse with the next use_op. *)
        let (all_frames, local_frames) = frames in
        let frames = (frame::all_frames,
          if frame_contains_loc then local_frames else frame::local_frames) in
        loop loc frames use_op
      (* Same logic as `Frame except we don't have a frame location. *)
      | `FrameWithoutLoc (use_op, frame) ->
        let (all_frames, local_frames) = frames in
        let frames = (frame::all_frames, frame::local_frames) in
        loop loc frames use_op
      (* We don't know what our root is! Return what we do know. *)
      | `UnknownRoot show_all_frames ->
        let (all_frames, local_frames) = frames in
        (None, loc, if show_all_frames then all_frames else local_frames)
      (* Finish up be returning our root location, root message, primary loc,
       * and frames. *)
      | `Root (root_reason, root_specific_reason, root_message) ->
        (* If our current loc is inside our root_loc then use our current loc
         * since it is the smallest possible loc in our root_loc. *)
        let root_loc = aloc_of_reason root_reason in
        let root_specific_loc = Option.map root_specific_reason aloc_of_reason in
        let loc = if Loc.contains (ALoc.to_loc root_loc) (ALoc.to_loc loc) && ALoc.compare root_loc loc <> 0
          then loc
          else Option.value root_specific_loc ~default:root_loc
        in
        (* Return our root loc and message in addition to the true primary loc
         * and frames. *)
        let (all_frames, _) = frames in
        (Some (root_loc, root_message), loc, all_frames)
    in
    fun loc use_op ->
      let (root, loc, frames) = loop loc ([], []) use_op in
      let root = Option.map root (fun (root_loc, root_message) ->
        (root_loc, root_message @ [text " because"])) in
      (root, loc, frames)
  in

  (* Make a friendly error based on a use_op. The message we are provided should
   * not have any punctuation. Punctuation will be provided after the frames of
   * an error message. *)
  let mk_use_op_error loc use_op message =
    let (root, loc, frames) = unwrap_use_ops loc use_op in
    mk_error
      ~trace_infos
      ?root
      ~frames
      loc
      message
  in

  (* Make a friendly error based on failed speculation. *)
  let mk_use_op_speculation_error loc use_op branches =
    let (root, loc, frames) = unwrap_use_ops loc use_op in
    let speculation_errors = List.map (fun (_, msg) ->
      let score = score_of_msg msg in
      let error = error_of_msg ~trace_reasons:[] ~source_file msg in
      (score, error)
    ) branches in
    mk_speculation_error
      ~kind:InferError
      ~trace_infos
      ~loc
      ~root
      ~frames
      ~speculation_errors
  in

  (* An error between two incompatible types. A "lower" type and an "upper"
   * type. The use_op describes the path which we followed to find
   * this incompatibility.
   *
   * This is a specialization of mk_incompatible_use_error. *)
  let mk_incompatible_error lower upper use_op =
    let ((lower, upper), use_op) = dedupe_by_flip (lower, upper) use_op in
    let ((lower, upper), use_op) = flip_contravariant (lower, upper) use_op in
    match use_op with
    (* Add a custom message for Coercion root_use_ops that does not include the
     * upper bound. *)
    | Op (Coercion {from; _}) ->
      mk_use_op_error (aloc_of_reason from) use_op
        [ref lower; text " should not be coerced"]
    (* Ending with FunMissingArg gives us a different error message. Even though
     * this error was generated by an incompatibility, we want to show a more
     * descriptive error message. *)
    | Frame (FunMissingArg { def; op; _ }, use_op) ->
      let message = match use_op with
      | Op (FunCall _ | FunCallMethod _) ->
        let def = replace_reason (function
        | RFunctionType -> RFunction RNormal
        | desc -> desc
        ) def in
        [ref def; text " requires another argument"]
      | _ ->
        [ref def; text " requires another argument from "; ref op]
      in
      mk_use_op_error (aloc_of_reason op) use_op message
    | _ ->
      let root_use_op = root_of_use_op use_op in
      (match root_use_op with
      (* Further customize functions with an implicit return. Functions with an
       * implicit return have a lower position which is not valuable. Also
       * clarify that the type was implicitly-returned.
       *
       * In flip_contravariant we flip upper/lower for all FunImplicitReturn. So
       * reverse those back as well. *)
      | FunImplicitReturn {upper=return; _} ->
        mk_use_op_error (aloc_of_reason lower) use_op (
          [ref lower; text " is incompatible with "] @
          if
            Loc.compare (aloc_of_reason return |> ALoc.to_loc) (aloc_of_reason upper |> ALoc.to_loc)
              = 0
          then
            [text "implicitly-returned "; desc upper]
          else
            [ref upper]
        )
      (* Default incompatibility. *)
      | _ ->
        begin match desc_of_reason lower, desc_of_reason upper with
          | RPolyTest _, RPolyTest _ when aloc_of_reason lower = aloc_of_reason upper ->
            mk_use_op_error (aloc_of_reason lower) use_op
              [text "the expected type is not parametric in "; ref upper;
               text ", perhaps due to the use of "; code "*";
               text " or the lack of a type annotation";]
          | _ ->
            mk_use_op_error (aloc_of_reason lower) use_op
              [ref lower; text " is incompatible with "; ref upper]
        end
      )
  in

  let mk_prop_message = function
  | None | Some "$key" | Some "$value" -> [text "an indexer property"]
  | Some "$call" -> [text "a callable signature"]
  | Some prop -> [text "property "; code prop]
  in

  (* When we fail to find a property on an object we use this function to create
   * an error. prop_loc should be the position of the use which caused this
   * error. The use_op represents how we got to this error.
   *
   * If the use_op is a PropertyCompatibility frame then we encountered this
   * error while subtyping two objects. In this case we add a bit more
   * information to the error message. *)
  let mk_prop_missing_error prop_loc prop lower use_op =
    let (loc, lower, upper, use_op) = match use_op with
    (* If we are missing a property while performing property compatibility
     * then we are subtyping. Record the upper reason. *)
    | Frame (PropertyCompatibility {prop=compat_prop; lower; upper; _}, use_op)
        when prop = compat_prop ->
      (aloc_of_reason lower, lower, Some upper, use_op)
    (* Otherwise this is a general property missing error. *)
    | _ -> (prop_loc, lower, None, use_op)
    in
    (* If we were subtyping that add to the error message so our user knows what
     * object required the missing property. *)
    let prop_message = mk_prop_message prop in
    let message = match upper with
    | Some upper ->
      prop_message @ [text " is missing in "; ref lower; text " but exists in "] @
      [ref upper]
    | None ->
      prop_message @ [text " is missing in "; ref lower]
    in
    (* Finally, create our error message. *)
    mk_use_op_error loc use_op message
  in

  (* An error that occurs when some arbitrary "use" is incompatible with the
   * "lower" type. The use_op describes the path which we followed to find this
   * incompatibility.
   *
   * Similar to mk_incompatible_error except with any arbitrary *use*
   * instead of specifically an upper type. This error handles all use
   * incompatibilities in general. *)
  let mk_incompatible_use_error use_loc use_kind lower use_op =
    let nope msg =
      mk_use_op_error use_loc use_op
        [ref lower; text (" " ^ msg)]
    in
    match use_kind with
    | IncompatibleElemTOfArrT
      -> nope "is not an array index"
    | IncompatibleGetPrivatePropT
    | IncompatibleSetPrivatePropT
      -> nope "is not a class with private properties"
    | IncompatibleCallT
    | IncompatibleConstructorT
      -> nope "is not a function"
    | IncompatibleObjAssignFromTSpread
    | IncompatibleArrRestT
      -> nope "is not an array"
    | IncompatibleObjAssignFromT
    | IncompatibleObjRestT
    | IncompatibleObjSealT
    | IncompatibleGetKeysT
    | IncompatibleGetValuesT
    | IncompatibleMapTypeTObject
      -> nope "is not an object"
    | IncompatibleMixinT
    | IncompatibleThisSpecializeT
      -> nope "is not a class"
    | IncompatibleSpecializeT
    | IncompatibleVarianceCheckT
    | IncompatibleTypeAppVarianceCheckT
      -> nope "is not a polymorphic type"
    | IncompatibleSuperT
      -> nope "is not inheritable"
    | IncompatibleUnaryMinusT
      -> nope "is not a number"
    | IncompatibleGetPropT (prop_loc, prop)
    | IncompatibleSetPropT (prop_loc, prop)
    | IncompatibleMatchPropT (prop_loc, prop)
    | IncompatibleHasOwnPropT (prop_loc, prop)
    | IncompatibleMethodT (prop_loc, prop)
      -> mk_prop_missing_error (prop_loc |> ALoc.of_loc) prop lower use_op
    | IncompatibleGetElemT prop_loc
    | IncompatibleSetElemT prop_loc
    | IncompatibleCallElemT prop_loc
      -> mk_prop_missing_error (prop_loc |> ALoc.of_loc) None lower use_op
    | IncompatibleGetStaticsT
      -> nope "is not an instance type"
    (* unreachable or unclassified use-types. until we have a mechanical way
       to verify that all legit use types are listed above, we can't afford
       to throw on a use type, so mark the error instead *)
    | IncompatibleUnclassified ctor
      -> nope (spf "is not supported by unclassified use %s" ctor)
  in

  (* When an object property has a polarity that is incompatible with another
   * error then we create one of these errors. We use terms like "read-only" and
   * "write-only" to better reflect how the user thinks about these properties.
   * Other terminology could include "contravariant", "covariant", and
   * "invariant". Generally these terms are impenatrable to the average
   * JavaScript developer. If we had more documentation explaining these terms
   * it may be fair to use them in error messages. *)
  let mk_prop_polarity_mismatch_error prop (lower, lpole) (upper, upole) use_op =
    (* Remove redundant PropertyCompatibility if one exists. *)
    let use_op = match use_op with
    | Frame (PropertyCompatibility c, use_op) when c.prop = prop -> use_op
    | _ ->
      use_op
    in
    let expected = match lpole with
    | Positive -> "read-only"
    | Negative -> "write-only"
    | Neutral ->
      (match upole with
      | Negative -> "readable"
      | Positive -> "writable"
      | Neutral -> failwith "unreachable")
    in
    let actual = match upole with
    | Positive -> "read-only"
    | Negative -> "write-only"
    | Neutral ->
      (match lpole with
      | Negative -> "readable"
      | Positive -> "writable"
      | Neutral -> failwith "unreachable")
    in
    mk_use_op_error (aloc_of_reason lower) use_op (
      mk_prop_message prop @
      [text (" is " ^ expected ^ " in "); ref lower; text " but "] @
      [text (actual ^ " in "); ref upper]
    )
  in

  let msg_export export_name =
    if export_name = "default" then
      text "the default export"
    else
      code export_name
  in

  let mk_signature_verification_error loc msgs =
    mk_error ~trace_infos loc
      ((text "Could not build a typed interface for this module. ")::msgs)
  in

  function
  | EIncompatible {
      lower = (reason_lower, _);
      upper = (reason_upper, upper_kind);
      use_op;
      branches;
    } ->
    if branches = [] then
      mk_incompatible_use_error
        (aloc_of_reason reason_upper)
        upper_kind
        reason_lower
        (Option.value ~default:unknown_use use_op)
    else
      mk_use_op_speculation_error
        (aloc_of_reason reason_upper)
        (Option.value ~default:unknown_use use_op)
        branches

  | EIncompatibleDefs { use_op; reason_lower; reason_upper; branches } ->
    if branches = [] then
      mk_incompatible_error
        reason_lower
        reason_upper
        use_op
    else
      mk_use_op_speculation_error
        (aloc_of_reason reason_upper)
        use_op
        branches

  | EIncompatibleProp { prop; reason_prop; reason_obj; special=_; use_op } ->
    mk_prop_missing_error
      (aloc_of_reason reason_prop) prop reason_obj (Option.value ~default:unknown_use use_op)

  | EDebugPrint (r, str) ->
    mk_error ~trace_infos (aloc_of_reason r) [text str]

  | EImportValueAsType (r, export_name) ->
    mk_error ~trace_infos (aloc_of_reason r) [
      text "Cannot import the value "; msg_export export_name; text " as a type. ";
      code "import type"; text " only works on type exports. Like type aliases, ";
      text "interfaces, and classes. If you intended to import the type of a ";
      text "value use "; code "import typeof"; text " instead.";
    ]

  | EImportTypeAsTypeof (r, export_name) ->
    mk_error ~trace_infos (aloc_of_reason r) [
      text "Cannot import the type "; msg_export export_name; text " as a type. ";
      code "import typeof"; text " only works on value exports. Like variables, ";
      text "functions, and classes. If you intended to import a type use ";
      code "import type"; text " instead.";
    ]

  | EImportTypeAsValue (r, export_name) ->
    mk_error ~trace_infos (aloc_of_reason r) [
      text "Cannot import the type "; msg_export export_name; text " as a value. ";
      text "Use "; code "import type"; text " instead.";
    ]

  | ERefineAsValue (r, name) ->
    mk_error ~trace_infos (aloc_of_reason r) [
      text "Cannot refine "; msg_export name; text " as a value. ";
      (* text "Use "; code "import type"; text " instead."; *)
    ]

  | ENoDefaultExport (r, module_name, suggestion) ->
    mk_error ~trace_infos (aloc_of_reason r) (
      [
        text "Cannot import a default export because there is no default export ";
        text "in "; code module_name; text ".";
      ] @
      match suggestion with
      | None -> []
      | Some suggestion -> [text " ";
          text "Did you mean ";
          code (spf "import {%s} from \"%s\"" suggestion module_name);
          text "?";
        ]
    )

  | EOnlyDefaultExport (r, module_name, export_name) ->
    mk_error ~trace_infos (aloc_of_reason r) [
      text "Cannot import "; code export_name; text " because ";
      text "there is no "; code export_name; text " export in ";
      code module_name; text ". Did you mean ";
      code (spf "import %s from \"...\"" export_name); text "?";
    ]

  | ENoNamedExport (r, module_name, export_name, suggestion) ->
    mk_error ~trace_infos (aloc_of_reason r) (
      [
        text "Cannot import "; code export_name; text " because ";
        text "there is no "; code export_name; text " export in ";
        code module_name; text ".";
      ] @
      match suggestion with
      | None -> []
      | Some suggestion -> [text " Did you mean "; code suggestion; text "?"]
    )

  | EMissingTypeArgs { reason_tapp; reason_arity; min_arity; max_arity } ->
    let arity, args =
      if min_arity = max_arity then
        spf "%d" max_arity, if max_arity = 1 then "argument" else "arguments"
      else
        spf "%d-%d" min_arity max_arity, "arguments"
    in
    let reason_arity = replace_reason_const (desc_of_reason reason_tapp) reason_arity in
    mk_error ~trace_infos (aloc_of_reason reason_tapp)
      [text "Cannot use "; ref reason_arity; text (spf " without %s type %s." arity args)]

  | ETooManyTypeArgs (reason_tapp, reason_arity, n) ->
    let reason_arity = replace_reason_const (desc_of_reason reason_tapp) reason_arity in
    mk_error ~trace_infos (aloc_of_reason reason_tapp) [
      text "Cannot use "; ref reason_arity; text " with more than ";
      text (spf "%n type %s." n (if n == 1 then "argument" else "arguments"))
    ]

  | ETooFewTypeArgs (reason_tapp, reason_arity, n) ->
    let reason_arity = replace_reason_const (desc_of_reason reason_tapp) reason_arity in
    mk_error ~trace_infos (aloc_of_reason reason_tapp) [
      text "Cannot use "; ref reason_arity; text " with fewer than ";
      text (spf "%n type %s." n (if n == 1 then "argument" else "arguments"))
    ]

  | EInvalidTypeArgs (reason_main, reason_tapp) ->
    mk_error ~trace_infos (aloc_of_reason reason_tapp) [
      text "Cannot use "; ref reason_main; text " with "; ref reason_tapp; text " argument";
    ]

  | ETypeParamArity (loc, n) ->
    if n = 0 then
      mk_error ~trace_infos (loc |> ALoc.of_loc)
        [text "Cannot apply type because it is not a polymorphic type."]
    else
      mk_error ~trace_infos (loc |> ALoc.of_loc) [
        text "Cannot use type without exactly ";
        text (spf "%n type %s." n (if n == 1 then "argument" else "arguments"));
      ]

  | ETypeParamMinArity (loc, n) ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "Cannot use type without at least ";
      text (spf "%n type %s." n (if n == 1 then "argument" else "arguments"));
    ]

  | ECallTypeArity { call_loc; is_new; reason_arity; expected_arity = n } ->
    let use = if is_new then "construct " else "call " in
    if n = 0 then
      mk_error ~trace_infos (call_loc |> ALoc.of_loc) [
        text "Cannot "; text use; text "non-polymorphic "; ref reason_arity;
        text " with type arguments.";
      ]
    else
      mk_error ~trace_infos (call_loc |> ALoc.of_loc) [
        text "Cannot "; text use; ref reason_arity; text " without exactly ";
        text (spf "%n type argument%s." n (if n == 1 then "" else "s"));
      ]

  | EValueUsedAsType reasons ->
    let (value, _) = reasons in
    mk_error ~trace_infos (aloc_of_reason value) [
      text "Cannot use "; desc value; text " as a type because ";
      desc value; text " is a value. To get the type of ";
      text "a value use "; code "typeof"; text ".";
    ]

  | EExpectedStringLit (reasons, _, _, use_op) ->
    let (reason_lower, reason_upper) = reasons in
    mk_incompatible_error reason_lower reason_upper use_op

  | EExpectedNumberLit (reasons, _, _, use_op) ->
    let (reason_lower, reason_upper) = reasons in
    mk_incompatible_error reason_lower reason_upper use_op

  | EExpectedBooleanLit (reasons, _, _, use_op) ->
    let (reason_lower, reason_upper) = reasons in
    mk_incompatible_error reason_lower reason_upper use_op

  | EPropNotFound (prop, reasons, use_op) ->
    let (reason_prop, reason_obj) = reasons in
    mk_prop_missing_error
      (aloc_of_reason reason_prop) prop reason_obj use_op

  | EPropAccess (reasons, x, _, rw, use_op) ->
    let (reason_prop, _) = reasons in
    let rw = match rw with
    | Read -> "readable"
    | Write _ -> "writable"
    in
    mk_use_op_error (aloc_of_reason reason_prop) use_op
      (mk_prop_message x @ [text (spf " is not %s" rw)])

  | EPropPolarityMismatch (reasons, x, (p1, p2), use_op) ->
    let (lreason, ureason) = reasons in
    mk_prop_polarity_mismatch_error
      x (lreason, p1) (ureason, p2) use_op

  | EPolarityMismatch { reason; name; expected_polarity; actual_polarity } ->
    let polarity_string = function
    | Positive -> "output"
    | Negative -> "input"
    | Neutral -> "input/output"
    in
    let expected_polarity = polarity_string expected_polarity in
    let actual_polarity = polarity_string actual_polarity in
    let reason_targ = mk_reason (RIdentifier name) (def_aloc_of_reason reason) in
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot use "; ref reason_targ; text (" in an " ^ actual_polarity ^ " ");
      text "position because "; ref reason_targ; text " is expected to occur only in ";
      text (expected_polarity ^ " positions.");
    ]

  | EStrictLookupFailed (reasons, lreason, x, use_op) ->
    (* if we're looking something up on the global/builtin object, then tweak
       the error to say that `x` doesn't exist. We can tell this is the
       global object because that should be the only object created with
       `builtin_reason` instead of an actual location (see `Init_js.init`). *)
    if is_builtin_reason lreason then
      let (reason, _) = reasons in
      let msg = match x with
      | Some x when is_internal_module_name x ->
        [text "Cannot resolve module "; code (uninternal_module_name x); text "."]
      | None -> [text "Cannot resolve name "; desc reason; text "."]
      | Some x when is_internal_name x -> [text "Cannot resolve name "; desc reason; text "."]
      | Some x -> [text "Cannot resolve name "; code x; text "."]
      in
      mk_error ~trace_infos (aloc_of_reason reason) msg
    else
      let (reason_prop, reason_obj) = reasons in
      mk_prop_missing_error
        (aloc_of_reason reason_prop) x reason_obj (Option.value ~default:unknown_use use_op)

  | EPrivateLookupFailed (reasons, x, use_op) ->
    mk_prop_missing_error
      (aloc_of_reason (fst reasons)) (Some ("#" ^ x)) (snd reasons) use_op

  | EAdditionMixed (reason, use_op) ->
    mk_use_op_error (aloc_of_reason reason) use_op
      [ref reason; text " could either behave like a string or like a number"]

  | EComparison (lower, upper) ->
    mk_error ~trace_infos (aloc_of_reason lower)
      [text "Cannot compare "; ref lower; text " to "; ref upper; text "."]

  | ETupleArityMismatch (reasons, l1, l2, use_op) ->
    let (lower, upper) = reasons in
    mk_use_op_error (aloc_of_reason lower) use_op [
      ref lower; text (spf " has an arity of %d but " l1); ref upper;
      text (spf " has an arity of %d" l2);
    ]

  | ENonLitArrayToTuple (reasons, use_op) ->
    let (lower, upper) = reasons in
    mk_use_op_error (aloc_of_reason lower) use_op [
      ref lower; text " has an unknown number of elements, so is ";
      text "incompatible with "; ref upper;
    ]

  | ETupleOutOfBounds (reasons, length, index, use_op) ->
    let (lower, upper) = reasons in
    mk_use_op_error (aloc_of_reason lower) use_op [
      ref upper;
      text (spf " only has %d element%s, so index %d is out of bounds"
        length (if length == 1 then "" else "s") index);
    ]

  | ETupleUnsafeWrite (reasons, use_op) ->
    let (lower, _) = reasons in
    mk_use_op_error (aloc_of_reason lower) use_op
      [text "the index must be statically known to write a tuple element"]

  | EUnionSpeculationFailed { use_op; reason; reason_op=_; branches } ->
    mk_use_op_speculation_error
      (aloc_of_reason reason)
      use_op
      branches

  | ESpeculationAmbiguous ((union_r, _), (prev_i, prev_case), (i, case), case_rs) ->
    let open Friendly in
    let prev_case_r =
      mk_reason (RCustom
        ("case " ^ string_of_int (prev_i + 1))) (aloc_of_reason prev_case)
    in
    let case_r =
      mk_reason (RCustom
        ("case " ^ string_of_int (i + 1))) (aloc_of_reason case)
    in
    mk_error (aloc_of_reason union_r) (
      [
        text "Could not decide which case to select. Since "; ref prev_case_r; text " ";
        text "may work but if it doesn't "; ref case_r; text " looks promising ";
        text "too. To fix add a type annotation ";
      ] @
      (conjunction_concat ~conjunction:"or" (List.map (fun case_r ->
        let text = "to " ^ (string_of_desc (desc_of_reason case_r)) in
        [ref (mk_reason (RCustom text) (aloc_of_reason case_r))]
      ) case_rs)) @
      [text "."]
    )

  | EIncompatibleWithExact (reasons, use_op) ->
    let (lower, upper) = reasons in
    mk_use_op_error (aloc_of_reason lower) use_op
      [text "inexact "; ref lower; text " is incompatible with exact "; ref upper]

  | EUnsupportedExact (_, lower) ->
    mk_error ~trace_infos (aloc_of_reason lower)
      [text "Cannot create exact type from "; ref lower; text "."]

  | EIdxArity reason ->
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot call "; code "idx(...)"; text " because only exactly two ";
      text "arguments are allowed."
    ]

  | EIdxUse1 reason ->
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot call "; code "idx(...)"; text " because the callback ";
      text "argument must not be annotated.";
    ]

  | EIdxUse2 reason ->
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot call "; code "idx(...)"; text " because the callback must ";
      text "only access properties on the callback parameter.";
    ]

  | EUnexpectedThisType loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc)
      [text "Unexpected use of "; code "this"; text " type."]

  | EPropertyTypeAnnot loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "Cannot use "; code "$PropertyType"; text " because the second ";
      text "type argument must be a string literal.";
    ]

  | EExportsAnnot loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "Cannot use "; code "$Exports"; text " because the first type ";
      text "argument must be a string literal.";
    ]

  | ECharSetAnnot loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "Cannot use "; code "$CharSet"; text " because the first type ";
      text "argument must be a string literal.";
    ]

  | EInvalidCharSet {
      invalid = (invalid_reason, invalid_chars);
      valid = valid_reason;
      use_op;
    } ->
    let valid_reason = mk_reason (desc_of_reason valid_reason) (def_aloc_of_reason valid_reason) in
    let invalids =
      InvalidCharSetSet.fold (fun c acc ->
        match c with
        | InvalidChar c ->
          [code (String.make 1 c); text " is not a member of the set"] :: acc
        | DuplicateChar c ->
          [code (String.make 1 c); text " is duplicated"] :: acc
      ) invalid_chars []
      |> List.rev
    in
    mk_use_op_error (aloc_of_reason invalid_reason) use_op (
      [ref invalid_reason; text " is incompatible with "; ref valid_reason; text " since "] @
      Friendly.conjunction_concat ~conjunction:"and" invalids
    )

  | EUnsupportedKeyInObjectType loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc)
      [text "Unsupported key in object type."]

  | EPredAnnot loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "Cannot use "; code "$Pred"; text " because the first ";
      text "type argument must be a number literal.";
    ]

  | ERefineAnnot loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "Cannot use "; code "$Refine"; text " because the third ";
      text "type argument must be a number literal.";
    ]

  | EUnexpectedTypeof loc ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc)
      [code "typeof"; text " can only be used to get the type of variables."]

  | EFunPredCustom ((a, b), msg) ->
    mk_error (aloc_of_reason a)
      [ref a; text ". "; text msg; text " "; ref b; text "."]

  | EFunctionIncompatibleWithShape (lower, upper, use_op) ->
    mk_use_op_error (aloc_of_reason lower) use_op [
      ref lower; text " is incompatible with "; code "$Shape"; text " of ";
      ref upper;
    ]

  | EInternal (loc, internal_error) ->
    let msg = match internal_error with
    | PackageHeapNotFound pkg ->
        spf "package %S was not found in the PackageHeap!" pkg
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
        spf "unsupported guard predicate (%s)" pred
    | BreakEnvMissingForCase ->
        "break env missing for case"
    | PropertyDescriptorPropertyCannotBeRead ->
        "unexpected property in properties object"
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
    | UnexpectedTypeapp s ->
        "unexpected typeapp: "^s
    in
    mk_error ~trace_infos ~kind:InternalError (loc |> ALoc.of_loc)
      [text (spf "Internal error: %s" msg)]

  | EUnsupportedSyntax (loc, unsupported_syntax) ->
    let msg = match unsupported_syntax with
      | ComprehensionExpression
      | GeneratorExpression
      | MetaPropertyExpression ->
        [text "Not supported."]
      | ObjectPropertyLiteralNonString ->
        [text "Non-string literal property keys not supported."]
      | ObjectPropertyGetSet ->
        [text "Get/set properties not yet supported."]
      | ObjectPropertyComputedGetSet ->
        [text "Computed getters and setters are not yet supported."]
      | InvariantSpreadArgument ->
        [text "Unsupported arguments in call to "; code "invariant"; text "."]
      | ClassPropertyLiteral ->
        [text "Literal properties not yet supported."]
      | ClassPropertyComputed ->
        [text "Computed property keys not supported."]
      | ReactCreateClassPropertyNonInit ->
        [text "Unsupported property specification in "; code "createClass"; text "."]
      | RequireDynamicArgument ->
        [text "The parameter passed to "; code "require"; text " must be a string literal."]
      | ImportDynamicArgument ->
        [text "The parameter passed to "; code "import"; text " must be a string literal."]
      | RequireLazyDynamicArgument -> [
          text "The first argument to "; code "requireLazy"; text " must be an ";
          text "array literal of string literals and the second argument must ";
          text "be a callback.";
        ]
      | CatchParameterAnnotation ->
        [text "Type annotations for catch parameters are not yet supported."]
      | CatchParameterDeclaration ->
        [text "Unsupported catch parameter declaration."]
      | DestructuringObjectPropertyLiteralNonString ->
        [text "Unsupported non-string literal object property in destructuring."]
      | DestructuringExpressionPattern ->
        [text "Unsupported expression pattern in destructuring."]
      | PredicateDeclarationForImplementation ->
        [text "Cannot declare predicate when a function body is present."]
      | PredicateDeclarationWithoutExpression -> [
          text "Predicate function declarations need to declare a ";
          text "predicate expression."
        ]
      | PredicateDeclarationAnonymousParameters -> [
          text "Predicate function declarations cannot use anonymous ";
          text "function parameters.";
        ]
      | PredicateInvalidBody -> [
          text "Invalid body for predicate function. Expected a simple return ";
          text "statement as body."
        ]
      | PredicateVoidReturn ->
        [text "Predicate functions need to return non-void."]
      | MultipleIndexers ->
        [text "Multiple indexers are not supported."]
      | SpreadArgument ->
        [text "A spread argument is unsupported here."]
      | IllegalName ->
        [text "Illegal name."]
      | UnsupportedInternalSlot {name; static = false} ->
        [text "Unsupported internal slot "; code name; text "."]
      | UnsupportedInternalSlot {name; static = true} ->
        [text "Unsupported static internal slot "; code name; text "."]
    in
    mk_error ~trace_infos (loc |> ALoc.of_loc) msg

  | EUseArrayLiteral loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc)
      [text "Use an array literal instead of "; code "new Array(...)"; text "."]

  | EMissingAnnotation (reason, trace_reasons) ->
    let default = [text "Missing type annotation for "; desc reason; text "."] in
    let msg = match (desc_of_reason reason) with
    | RTypeParam (_, (RImplicitInstantiation, _), _) ->
        [text "Please use a concrete type annotation instead of "; code "_";
      text " in this position."]
    | RTypeParam (_, (reason_op_desc, reason_op_loc), (reason_tapp_desc, reason_tapp_loc)) ->
        let reason_op = mk_reason reason_op_desc (reason_op_loc |> ALoc.of_loc) in
        let reason_tapp = mk_reason reason_tapp_desc (reason_tapp_loc |> ALoc.of_loc) in
        default @ [text " "; desc reason; text " is a type parameter declared in "; ref reason_tapp;
         text " and was implicitly instantiated at "; ref reason_op; text "."]
    | _ -> default in

    (* We don't collect trace info in the assert_ground_visitor because traces
     * represent tests of lower bounds to upper bounds, and the assert_ground
     * visitor is just visiting types. Instead, we collect a list of types we
     * visited to get to the missing annotation error and report that as the
     * trace *)
    let trace_infos = List.map info_of_reason trace_reasons in
    mk_error ~trace_infos (aloc_of_reason reason) msg

  | EBindingError (binding_error, loc, x, entry) ->
    let desc =
      if x = internal_name "this" then RThis
      else if x = internal_name "super" then RSuper
      else RIdentifier x
    in
    let x = mk_reason desc (Scope.Entry.entry_loc entry |> ALoc.of_loc) in
    let msg = match binding_error with
    | ENameAlreadyBound ->
      [text "Cannot declare "; ref x; text " because the name is already bound."]
    | EReferencedBeforeDeclaration ->
      if desc = RThis || desc = RSuper then [
        text "Must call "; code "super"; text " before accessing "; ref x;
        text " in a derived constructor."
      ] else [
        text "Cannot use variable "; ref x; text " because the declaration ";
        text "either comes later or was skipped.";
      ]
    | ETypeInValuePosition
    | ETypeAliasInValuePosition
      -> [text "Cannot reference type "; ref x; text " from a value position."]
    | EConstReassigned
    | EConstParamReassigned
      -> [text "Cannot reassign constant "; ref x; text "."]
    | EImportReassigned ->
      [text "Cannot reassign import "; ref x; text "."]
    in
    mk_error ~trace_infos (loc |> ALoc.of_loc) msg

  | ERecursionLimit (r, _) ->
    mk_error ~kind:RecursionLimitError (aloc_of_reason r)
      [text "*** Recursion limit exceeded ***"]

  | EModuleOutsideRoot (loc, package_relative_to_root) ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "This module resolves to "; code package_relative_to_root; text " which ";
      text "is outside both your root directory and all of the entries in the ";
      code "[include]"; text " section of your "; code ".flowconfig"; text ". ";
      text "You should either add this directory to the "; code "[include]"; text " ";
      text "section of your "; code ".flowconfig"; text ", move your ";
      code ".flowconfig"; text " file higher in the project directory tree, or ";
      text "move this package under your Flow root directory.";
    ]

  | EExperimentalDecorators loc ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      text "Experimental decorator usage. Decorators are an early stage ";
      text "proposal that may change. Additionally, Flow does not account for ";
      text "the type implications of decorators at this time.";
    ]

  | EExperimentalClassProperties (loc, static) ->
    let config_name, config_key =
      if static
      then "class static field", "class_static_fields"
      else "class instance field", "class_instance_fields"
    in
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      text ("Experimental " ^ config_name ^ " usage. ");
      text (String.capitalize_ascii config_name ^ "s are an active early stage ");
      text "feature proposal that may change. You may opt-in to using them ";
      text "anyway in Flow by putting "; code ("esproposal." ^ config_key ^ "=enable"); text " ";
      text "into the "; code "[options]"; text " section of your ";
      code ".flowconfig"; text ".";
    ]

  | EUnsafeGetSet loc ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      text "Potentially unsafe get/set usage. Getters and setters with side ";
      text "effects are potentially unsafe and so disabled by default. You may ";
      text "opt-in to using them anyway by putting ";
      code "unsafe.enable_getters_and_setters"; text " into the ";
      code "[options]"; text " section of your "; code ".flowconfig"; text ".";
    ]

  | EExperimentalExportStarAs loc ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      text "Experimental "; code "export * as"; text " usage. ";
      code "export * as"; text " is an active early stage feature propsal that ";
      text "may change. You may opt-in to using it anyway by putting ";
      code "esproposal.export_star_as=enable"; text " into the ";
      code "[options]"; text " section of your "; code ".flowconfig"; text ".";
    ]

  | EIndeterminateModuleType loc ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      text "Unable to determine module type (CommonJS vs ES) if both an export ";
      text "statement and "; code "module.exports"; text " are used in the ";
      text "same module!";
    ]

  | EBadExportPosition loc ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      text "Exports can only appear at the top level"
    ]

  | EBadExportContext (name, loc) ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      code name;
      text " may only be used as part of a legal top level export statement";
    ]

  | EUnexpectedTemporaryBaseType loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc) [
      text "The type argument of a temporary base type must be a compatible literal type";
    ]

  | ESignatureVerification sve ->
    let open Signature_builder_deps.With_Loc.Error in
    begin match sve with
      | ExpectedSort (sort, x, loc) ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          code x;
          text (spf " is not a %s." (Signature_builder_kind.Sort.to_string sort))
        ]
      | ExpectedAnnotation loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Missing type annotation:"
        ]
      | InvalidTypeParamUse loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Invalid use of type parameter:"
        ]
      | UnexpectedObjectKey loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Expected simple object key:"
        ]
      | UnexpectedObjectSpread loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Unexpected object spread:"
        ]
      | UnexpectedArraySpread loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Unexpected array spread:"
        ]
      | UnexpectedArrayHole loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Unexpected array hole:"
        ]
      | EmptyArray loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Cannot determine element type of empty array, try using a type cast."
        ]
      | EmptyObject loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Cannot determine types of initialized properties of empty object, try using a type cast."
        ]
      | UnexpectedExpression (loc, esort) ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text (
            spf "Expected literal expression instead of %s, try using a type cast."
              (Flow_ast_utils.ExpressionSort.to_string esort)
          )
        ]
      | SketchyToplevelDef loc ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text "Unexpected toplevel definition that needs hoisting:"
        ]
      | TODO (msg, loc) ->
        mk_signature_verification_error (loc |> ALoc.of_loc) [
          text (spf "TODO: %s is not supported yet, try using a type cast." msg)
        ]
    end

  | EUnreachable loc ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc)
      [text "Unreachable code."]

  | EInvalidObjectKit { tool=_; reason; reason_op=_; use_op } ->
    mk_use_op_error (aloc_of_reason reason) use_op
      [ref reason; text " is not an object"]

  | EInvalidTypeof (loc, typename) ->
    mk_error ~trace_infos ~kind:InferWarning (loc |> ALoc.of_loc) [
      text "Cannot compare the result of "; code "typeof"; text " to string ";
      text "literal "; code typename; text " because it is not a valid ";
      code "typeof"; text " return value.";
    ]

  | EArithmeticOperand reason ->
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot perform arithmetic operation because "; ref reason; text " ";
      text "is not a number.";
    ]

  | EBinaryInLHS reason ->
    (* TODO: or symbol *)
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot use "; code "in"; text " because on the left-hand side, ";
      ref reason; text " must be a string or number.";
    ]

  | EBinaryInRHS reason ->
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot use "; code "in"; text " because on the right-hand side, ";
      ref reason; text " must be an object or array.";
    ]

  | EForInRHS reason ->
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot iterate using a "; code "for...in"; text " statement ";
      text "because "; ref reason; text " is not an object, null, or undefined.";
    ]

  | EObjectComputedPropertyAccess (_, reason_prop) ->
    mk_error ~trace_infos (aloc_of_reason reason_prop)
      [text "Cannot access computed property using "; ref reason_prop; text "."]

  | EObjectComputedPropertyAssign (_, reason_prop) ->
    mk_error ~trace_infos (aloc_of_reason reason_prop)
      [text "Cannot assign computed property using "; ref reason_prop; text "."]

  | EInvalidLHSInAssignment loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc)
      [text "Invalid left-hand side in assignment expression."]

  | EIncompatibleWithUseOp (l_reason, u_reason, use_op) ->
    mk_incompatible_error l_reason u_reason use_op

  | EUnsupportedImplements reason ->
    mk_error ~trace_infos (aloc_of_reason reason)
      [text "Cannot implement "; desc reason; text " because it is not an interface."]

  | EReactKit (reasons, tool, use_op) ->
    let open React in
    let (_, reason) = reasons in
    let is_not_prop_type = "is not a React propType" in
    let msg = match tool with
    | GetProps _
    | GetConfig _
    | GetRef _
    | CreateElement0 _
    | CreateElement _
      -> "is not a React component"
    | SimplifyPropType (tool, _) ->
      SimplifyPropType.(match tool with
      | ArrayOf -> is_not_prop_type
      | InstanceOf -> "is not a class"
      | ObjectOf -> is_not_prop_type
      | OneOf ResolveArray -> "is not an array"
      | OneOf (ResolveElem _) -> "is not a literal"
      | OneOfType ResolveArray -> "is not an array"
      | OneOfType (ResolveElem _) -> is_not_prop_type
      | Shape ResolveObject -> "is not an object"
      | Shape (ResolveDict _) -> is_not_prop_type
      | Shape (ResolveProp _) -> is_not_prop_type
      )
    | CreateClass (tool, _, _) ->
      CreateClass.(match tool with
      | Spec _ -> "is not an exact object"
      | Mixins _ -> "is not a tuple"
      | Statics _ -> "is not an object"
      | PropTypes (_, ResolveObject) -> "is not an object"
      | PropTypes (_, ResolveDict _) -> is_not_prop_type
      | PropTypes (_, ResolveProp _) -> is_not_prop_type
      | DefaultProps _ -> "is not an object"
      | InitialState _ -> "is not an object or null"
      )
    in
    mk_use_op_error (aloc_of_reason reason) use_op
      [ref reason; text (" " ^ msg)]

  | EReactElementFunArity (reason, fn, n) ->
    mk_error ~trace_infos (aloc_of_reason reason) [
      text "Cannot call "; code ("React." ^ fn); text " ";
      text (spf "without at least %d argument%s." n (if n == 1 then "" else "s"));
    ]

  | EFunctionCallExtraArg (unused_reason, def_reason, param_count, use_op) ->
    let msg = match param_count with
    | 0 -> "no arguments are expected by"
    | 1 -> "no more than 1 argument is expected by"
    | n -> spf "no more than %d arguments are expected by" n
    in
    mk_use_op_error (aloc_of_reason unused_reason) use_op
      [text msg; text " "; ref def_reason]

  | EUnsupportedSetProto reason ->
    mk_error ~trace_infos (aloc_of_reason reason)
      [text "Mutating this prototype is unsupported."]

  | EDuplicateModuleProvider {module_name; provider; conflict} ->
    let (loc1, loc2) = Loc.(
      let pos = { line = 1; column = 0; offset = 0 } in
      let loc1 = { source = Some conflict; start = pos; _end = pos } in
      let loc2 = { source = Some provider; start = pos; _end = pos } in
      (loc1, loc2)
    ) in
    mk_error ~trace_infos ~kind:DuplicateProviderError (loc1 |> ALoc.of_loc) [
      text "Duplicate module provider for "; code module_name; text ". Change ";
      text "either this module provider or the ";
      ref (mk_reason (RCustom "current module provider") (loc2 |> ALoc.of_loc));
      text ".";
    ]

  | EParseError (loc, parse_error) ->
    mk_error ~kind:ParseError (loc |> ALoc.of_loc)
      (Friendly.message_of_string (Parse_error.PP.error parse_error))

  | EDocblockError (loc, err) ->
    let msg = match err with
    | MultipleFlowAttributes -> [
        text "Unexpected "; code "@flow"; text " declaration. Only one per ";
        text "file is allowed.";
      ]
    | MultipleProvidesModuleAttributes -> [
        text "Unexpected "; code "@providesModule"; text " declaration. ";
        text "Only one per file is allowed.";
      ]
    | MultipleJSXAttributes -> [
        text "Unexpected "; code "@jsx"; text " declaration. Only one per ";
        text "file is allowed.";
      ]
    | InvalidJSXAttribute first_error -> [
        text "Invalid "; code "@jsx"; text " declaration. Should have the form ";
        code "@jsx LeftHandSideExpression"; text " with no spaces.";
      ] @
      match first_error with
      | None -> []
      | Some first_error -> [text (spf " Parse error: %s." first_error)]
    in
    mk_error ~kind:ParseError (loc |> ALoc.of_loc) msg

  | EUntypedTypeImport (loc, module_name) ->
    mk_error ~trace_infos ~kind:(LintError Lints.UntypedTypeImport) (loc |> ALoc.of_loc) [
      text "Importing a type from an untyped module makes it "; code "any"; text " ";
      text "and is not safe! Did you mean to add "; code "// @flow"; text " to ";
      text "the top of "; code module_name; text "?";
    ]

  | EUntypedImport (loc, module_name) ->
    mk_error ~trace_infos ~kind:(LintError Lints.UntypedImport) (loc |> ALoc.of_loc) [
      text "Importing from an untyped module makes it "; code "any"; text " ";
      text "and is not safe! Did you mean to add "; code "// @flow"; text " ";
      text "to the top of "; code module_name; text "?";
    ]

  | ENonstrictImport loc ->
    mk_error ~trace_infos ~kind:(LintError Lints.NonstrictImport) (loc |> ALoc.of_loc) [
      text "Dependencies of a "; code "@flow strict"; text " module must ";
      text "also be "; code "@flow strict"; text "!"
    ]

  | EUnclearType loc ->
    mk_error ~trace_infos ~kind:(LintError Lints.UnclearType) (loc |> ALoc.of_loc) [
      text "Unclear type. Using "; code "any"; text ", ";
      code "Object"; text ", "; code "Function"; text ", ";
      code "$Subtype<...>"; text ", or "; code "$Supertype<...>"; text " types is not safe!"
    ]

  | EDeprecatedType loc ->
    mk_error ~trace_infos ~kind:(LintError Lints.DeprecatedType) (loc |> ALoc.of_loc) [
      text "Deprecated type. Using "; code "*"; text " types is not recommended!"
    ]

  | EUnsafeGettersSetters loc ->
    mk_error ~trace_infos ~kind:(LintError Lints.UnsafeGettersSetters) (loc |> ALoc.of_loc)
      [text "Getters and setters can have side effects and are unsafe."]

  | EDeprecatedCallSyntax loc ->
    mk_error ~trace_infos ~kind:(LintError Lints.DeprecatedCallSyntax) (loc |> ALoc.of_loc)
      [text "Deprecated $call syntax. Use callable property syntax instead."]

  | EUnusedSuppression loc ->
    mk_error ~trace_infos (loc |> ALoc.of_loc)
      [text "Unused suppression comment."]

  | ELintSetting (loc, kind) ->
    let msg = match kind with
    | LintSettings.Redundant_argument -> [
        text "Redundant argument. This argument doesn't change any lint settings."
      ]
    | LintSettings.Overwritten_argument -> [
        text "Redundant argument. The values set by this argument are ";
        text "overwritten later in this comment.";
      ]
    | LintSettings.Naked_comment -> [
        text "Malformed lint rule. At least one argument is required."
      ]
    | LintSettings.Nonexistent_rule -> [
        text "Nonexistent/misspelled lint rule. Perhaps you have a ";
        text "missing/extra "; code ","; text "?";
      ]
    | LintSettings.Invalid_setting -> [
        text "Invalid setting. Valid settings are error, warn, and off."
      ]
    | LintSettings.Malformed_argument -> [
        text "Malformed lint rule. Properly formed rules contain a single ";
        code ":"; text " character. Perhaps you have a missing/extra ";
        code ","; text "?";
      ]
    in
    mk_error ~trace_infos ~kind:ParseError (loc |> ALoc.of_loc) msg

  | ESketchyNullLint { kind; loc; falsy_loc; null_loc } ->
    let type_str, value_str = match kind with
    | Lints.SketchyNullBool -> "boolean", "false"
    | Lints.SketchyNullNumber -> "number", "0"
    | Lints.SketchyNullString -> "string", "an empty string"
    | Lints.SketchyNullMixed -> "mixed", "false"
    in
    mk_error ~trace_infos ~kind:(LintError (Lints.SketchyNull kind)) (loc |> ALoc.of_loc) [
      text "Sketchy null check on "; ref (mk_reason (RCustom type_str) (falsy_loc |> ALoc.of_loc)); text " ";
      text "which is potentially "; text value_str; text ". Perhaps you meant to ";
      text "check for "; ref (mk_reason RNullOrVoid (null_loc |> ALoc.of_loc)); text "?";
    ]

  | ESketchyNumberLint (Lints.SketchyNumberAnd, reason) ->
    mk_error ~trace_infos ~kind:Lints.(LintError (SketchyNumber SketchyNumberAnd)) (aloc_of_reason reason) [
      text "Avoid using "; code "&&"; text " to check the value of "; ref reason; text ". ";
      text "Consider handling falsy values (0 and NaN) by using a conditional to choose an ";
      text "explicit default instead.";
    ]

  | EInvalidPrototype reason ->
    mk_error ~trace_infos (aloc_of_reason reason)
      [text "Cannot use "; ref reason; text " as a prototype. Expected an object or null."]

  | EExperimentalOptionalChaining loc ->
    mk_error ~trace_infos ~kind:ParseError (loc |> ALoc.of_loc) [
      text "Experimental optional chaining ("; code "?."; text ") usage. ";
      text "Optional chaining is an active early-stage feature proposal that ";
      text "may change. You may opt in to using it anyway by putting ";
      code "esproposal.optional_chaining=enable"; text " into the ";
      code "[options]"; text " section of your "; code ".flowconfig"; text ".";
    ]

  | EOptionalChainingMethods loc ->
    mk_error ~trace_infos ~kind:ParseError (loc |> ALoc.of_loc) [
      text "Flow does not yet support method or property calls in optional chains."
    ]

  | EUnnecessaryOptionalChain (loc, lhs_reason) ->
    mk_error ~trace_infos ~kind:(LintError Lints.UnnecessaryOptionalChain) (loc |> ALoc.of_loc) [
      text "This use of optional chaining ("; code "?."; text ") is unnecessary because ";
      ref lhs_reason; text " cannot be nullish or because an earlier "; code "?.";
      text " will short-circuit the nullish case.";
    ]

  | EUnnecessaryInvariant (loc, reason) ->
    mk_error ~trace_infos ~kind:(LintError Lints.UnnecessaryInvariant) (loc |> ALoc.of_loc) [
      text "This use of `invariant` is unnecessary because "; ref reason;
      text " is always truthy."
    ]

  | EInexactSpread (reason, reason_op) ->
    mk_error ~kind:(LintError Lints.InexactSpread) (aloc_of_reason reason) [
      text "Cannot determine the type of "; ref reason_op; text " because ";
      text "it contains a spread of inexact "; ref reason; text ". ";
      text "Being inexact, "; ref reason;
      text " might be missing the types of some properties that are being copied. ";
      text "Perhaps you could make it exact?"
    ]

let is_lint_error = function
  | EUntypedTypeImport _
  | EUntypedImport _
  | ENonstrictImport _
  | EUnclearType _
  | EDeprecatedType _
  | EUnsafeGettersSetters _
  | ESketchyNullLint _
  | ESketchyNumberLint _
  | EInexactSpread _
  | EUnnecessaryOptionalChain _
  | EUnnecessaryInvariant _
      -> true
  | _ -> false
