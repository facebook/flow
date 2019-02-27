(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason

exception EDebugThrow of ALoc.t
exception EMergeTimeout of float

type invalid_char_set =
  | DuplicateChar of Char.t
  | InvalidChar of Char.t

module InvalidCharSetSet = Set.Make(struct
  type t = invalid_char_set
  let compare = Pervasives.compare
end)

type t =
  | EIncompatible of {
      lower: reason * lower_kind option;
      upper: reason * upper_kind;
      use_op: use_op option;
      branches: (Reason.t * t) list;
    }
  | EIncompatibleDefs of {
      use_op: use_op;
      reason_lower: reason;
      reason_upper: reason;
      branches: (Reason.t * t) list;
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
      branches: (reason * t) list;
    }
  | ESpeculationAmbiguous of (reason * reason) * (int * reason) * (int * reason) * reason list
  | EIncompatibleWithExact of (reason * reason) * use_op
  | EUnsupportedExact of (reason * reason)
  | EIdxArity of reason
  | EIdxUse1 of reason
  | EIdxUse2 of reason
  | EUnexpectedThisType of ALoc.t
  | ETypeParamArity of ALoc.t * int
  | ECallTypeArity of {
      call_loc: ALoc.t;
      is_new: bool;
      reason_arity: reason;
      expected_arity: int;
    }
  | ETypeParamMinArity of ALoc.t * int
  | ETooManyTypeArgs of reason * reason * int
  | ETooFewTypeArgs of reason * reason * int
  | EInvalidTypeArgs of reason * reason
  | EPropertyTypeAnnot of ALoc.t
  | EExportsAnnot of ALoc.t
  | ECharSetAnnot of ALoc.t
  | EInvalidCharSet of { invalid: reason * InvalidCharSetSet.t; valid: reason; use_op: use_op }
  | EUnsupportedKeyInObjectType of ALoc.t
  | EPredAnnot of ALoc.t
  | ERefineAnnot of ALoc.t
  | ETrustedAnnot of ALoc.t
  | EPrivateAnnot of ALoc.t
  | EUnexpectedTypeof of ALoc.t
  | EFunPredCustom of (reason * reason) * string
  | EIncompatibleWithShape of reason * reason * use_op
  | EInternal of ALoc.t * internal_error
  | EUnsupportedSyntax of ALoc.t * unsupported_syntax
  | EUseArrayLiteral of ALoc.t
  | EMissingAnnotation of reason * reason list
  | EBindingError of binding_error * ALoc.t * string * Scope.Entry.t
  | ERecursionLimit of (reason * reason)
  | EModuleOutsideRoot of ALoc.t * string
  | EMalformedPackageJson of ALoc.t * string
  | EExperimentalDecorators of ALoc.t
  | EExperimentalClassProperties of ALoc.t * bool
  | EUnsafeGetSet of ALoc.t
  | EExperimentalExportStarAs of ALoc.t
  | EIndeterminateModuleType of ALoc.t
  | EBadExportPosition of ALoc.t
  | EBadExportContext of string * ALoc.t
  | EUnreachable of ALoc.t
  | EInvalidObjectKit of { tool: Object.tool; reason: reason; reason_op: reason; use_op: use_op }
  | EInvalidTypeof of ALoc.t * string
  | EBinaryInLHS of reason
  | EBinaryInRHS of reason
  | EArithmeticOperand of reason
  | EForInRHS of reason
  | EObjectComputedPropertyAccess of (reason * reason)
  | EObjectComputedPropertyAssign of (reason * reason)
  | EInvalidLHSInAssignment of ALoc.t
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
  | EParseError of ALoc.t * Parse_error.t
  | EDocblockError of ALoc.t * docblock_error
  (* The string is either the name of a module or "the module that exports `_`". *)
  | EUntypedTypeImport of ALoc.t * string
  | EUntypedImport of ALoc.t * string
  | ENonstrictImport of ALoc.t
  | EUnclearType of ALoc.t
  | EDeprecatedType of ALoc.t
  | EDeprecatedUtility of ALoc.t * string
  | EDynamicExport of reason * reason
  | EUnsafeGettersSetters of ALoc.t
  | EUnusedSuppression of ALoc.t
  | ELintSetting of LintSettings.lint_parse_error
  | ESketchyNullLint of {
      kind: Lints.sketchy_null_kind;
      loc: ALoc.t;
      null_loc: ALoc.t;
      falsy_loc: ALoc.t;
    }
  | ESketchyNumberLint of Lints.sketchy_number_kind * reason
  | EInvalidPrototype of reason
  | EExperimentalOptionalChaining of ALoc.t
  | EOptionalChainingMethods of ALoc.t
  | EUnnecessaryOptionalChain of ALoc.t * reason
  | EUnnecessaryInvariant of ALoc.t * reason
  | EInexactSpread of reason * reason
  | EDeprecatedCallSyntax of ALoc.t
  | EUnexpectedTemporaryBaseType of ALoc.t
  | ESignatureVerification of Signature_builder_deps.With_ALoc.Error.t

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
  | MergeJobException of Exception.t
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
  | IncompatibleGetPropT of ALoc.t * string option
  | IncompatibleSetPropT of ALoc.t * string option
  | IncompatibleMatchPropT of ALoc.t * string option
  | IncompatibleGetPrivatePropT
  | IncompatibleSetPrivatePropT
  | IncompatibleMethodT of ALoc.t * string option
  | IncompatibleCallT
  | IncompatibleMixedCallT
  | IncompatibleConstructorT
  | IncompatibleGetElemT of ALoc.t
  | IncompatibleSetElemT of ALoc.t
  | IncompatibleCallElemT of ALoc.t
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
  | IncompatibleHasOwnPropT of ALoc.t * string option
  | IncompatibleGetValuesT
  | IncompatibleUnaryMinusT
  | IncompatibleMapTypeTObject
  | IncompatibleTypeAppVarianceCheckT
  | IncompatibleGetStaticsT
  | IncompatibleUnclassified of string

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
| EIncompatibleWithShape (l, u, use_op) ->
  util use_op (fun use_op -> EIncompatibleWithShape (l, u, use_op))
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
| ETrustedAnnot (_)
| EPrivateAnnot (_)
| EUnexpectedTypeof (_)
| EFunPredCustom (_, _)
| EInternal (_, _)
| EUnsupportedSyntax (_, _)
| EUseArrayLiteral (_)
| EMissingAnnotation (_)
| EBindingError (_, _, _, _)
| ERecursionLimit (_, _)
| EModuleOutsideRoot (_, _)
| EMalformedPackageJson (_, _)
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
| EDeprecatedType _
| EDeprecatedUtility _
| EDynamicExport _
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

let is_lint_error = function
  | EUntypedTypeImport _
  | EUntypedImport _
  | ENonstrictImport _
  | EUnclearType _
  | EDeprecatedType _
  | EDeprecatedUtility _
  | EDynamicExport _
  | EUnsafeGettersSetters _
  | ESketchyNullLint _
  | ESketchyNumberLint _
  | EInexactSpread _
  | EUnnecessaryOptionalChain _
  | EUnnecessaryInvariant _
      -> true
  | _ -> false
