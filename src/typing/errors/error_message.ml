(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type
open Reason
open Utils_js
open Flow_intermediate_error_types

exception EDebugThrow of ALoc.t

exception ECheckTimeout of float * string

type t = ALoc.t t'

and 'loc t' =
  | EIncompatible of {
      lower: 'loc virtual_reason * lower_kind option;
      upper: 'loc virtual_reason * 'loc upper_kind;
      use_op: 'loc virtual_use_op option;
    }
  | EIncompatibleSpeculation of {
      loc: 'loc;
      use_op: 'loc virtual_use_op option;
      branches: 'loc t' list;
    }
  | EIncompatibleDefs of {
      use_op: 'loc virtual_use_op;
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      branches: 'loc t' list;
    }
  | EIncompatibleProp of {
      prop: name option;
      reason_prop: 'loc virtual_reason;
      reason_obj: 'loc virtual_reason;
      special: lower_kind option;
      use_op: 'loc virtual_use_op option;
    }
  | EExportValueAsType of 'loc virtual_reason * name
  | EImportValueAsType of 'loc virtual_reason * string
  | EImportTypeAsTypeof of 'loc virtual_reason * string
  | EImportTypeAsValue of 'loc virtual_reason * string
  | ENoDefaultExport of 'loc virtual_reason * Flow_import_specifier.userland * string option
  | EOnlyDefaultExport of 'loc virtual_reason * Flow_import_specifier.userland * string
  | ENoNamedExport of 'loc virtual_reason * Flow_import_specifier.userland * string * string option
  | EMissingTypeArgs of {
      reason_op: 'loc virtual_reason;
      reason_tapp: 'loc virtual_reason;
      arity_loc: 'loc;
      min_arity: int;
      max_arity: int;
    }
  | EAnyValueUsedAsType of { reason_use: 'loc virtual_reason }
  | EValueUsedAsType of { reason_use: 'loc virtual_reason }
  | EExpectedStringLit of {
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EExpectedNumberLit of {
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EExpectedBooleanLit of {
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EExpectedBigIntLit of {
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EPropNotFound of {
      prop_name: name option;
      reason_prop: 'loc virtual_reason;
      reason_obj: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
      suggestion: string option;
    }
  | EIndexerCheckFailed of {
      prop_name: name;
      reason_prop: 'loc virtual_reason;
      reason_obj: 'loc virtual_reason;
      reason_indexer: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EPropNotReadable of {
      reason_prop: 'loc virtual_reason;
      prop_name: name option;
      use_op: 'loc virtual_use_op;
    }
  | EPropNotWritable of {
      reason_prop: 'loc virtual_reason;
      prop_name: name option;
      use_op: 'loc virtual_use_op;
    }
  | EPropPolarityMismatch of
      ('loc virtual_reason * 'loc virtual_reason)
      * name option
      * (Polarity.t * Polarity.t)
      * 'loc virtual_use_op
  | EPolarityMismatch of {
      reason: 'loc virtual_reason;
      name: string;
      expected_polarity: Polarity.t;
      actual_polarity: Polarity.t;
    }
  | EBuiltinNameLookupFailed of {
      loc: 'loc;
      name: string;
    }
  | EBuiltinModuleLookupFailed of {
      loc: 'loc;
      name: string;
      potential_generator: string option;
    }
  | EExpectedModuleLookupFailed of {
      loc: 'loc;
      name: string;
      expected_module_purpose: expected_module_purpose;
    }
  | EPrivateLookupFailed of ('loc virtual_reason * 'loc virtual_reason) * name * 'loc virtual_use_op
  | EPlatformSpecificImplementationModuleLookupFailed of {
      loc: 'loc;
      name: string;
    }
  | EComparison of ('loc virtual_reason * 'loc virtual_reason)
  | ENonStrictEqualityComparison of ('loc virtual_reason * 'loc virtual_reason)
  | ETupleArityMismatch of {
      use_op: 'loc virtual_use_op;
      lower_reason: 'loc virtual_reason;
      lower_arity: int * int;
      lower_inexact: bool;
      upper_reason: 'loc virtual_reason;
      upper_arity: int * int;
      upper_inexact: bool;
      unify: bool;
    }
  | ENonLitArrayToTuple of ('loc virtual_reason * 'loc virtual_reason) * 'loc virtual_use_op
  | ETupleOutOfBounds of {
      use_op: 'loc virtual_use_op;
      reason: 'loc virtual_reason;
      reason_op: 'loc virtual_reason;
      inexact: bool;
      length: int;
      index: string;
    }
  | ETupleNonIntegerIndex of {
      use_op: 'loc virtual_use_op;
      reason: 'loc virtual_reason;
      index: string;
    }
  | ETupleUnsafeWrite of {
      reason: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | ETupleElementNotReadable of {
      reason: 'loc virtual_reason;
      index: int;
      name: string option;
      use_op: 'loc virtual_use_op;
    }
  | ETupleElementNotWritable of {
      reason: 'loc virtual_reason;
      index: int;
      name: string option;
      use_op: 'loc virtual_use_op;
    }
  | ETupleElementPolarityMismatch of {
      index: int;
      reason_lower: 'loc Reason.virtual_reason;
      polarity_lower: Polarity.t;
      reason_upper: 'loc Reason.virtual_reason;
      polarity_upper: Polarity.t;
      use_op: 'loc Type.virtual_use_op;
    }
  | ETupleRequiredAfterOptional of {
      reason_tuple: 'loc virtual_reason;
      reason_required: 'loc virtual_reason;
      reason_optional: 'loc virtual_reason;
    }
  | ETupleInvalidTypeSpread of {
      reason_spread: 'loc virtual_reason;
      reason_arg: 'loc virtual_reason;
    }
  | ETupleElementAfterInexactSpread of 'loc virtual_reason
  | EROArrayWrite of ('loc virtual_reason * 'loc virtual_reason) * 'loc virtual_use_op
  | EUnionSpeculationFailed of {
      use_op: 'loc virtual_use_op;
      reason: 'loc virtual_reason;
      op_reasons: 'loc virtual_reason Nel.t;
      branches: 'loc t' list;
    }
  | EIncompatibleWithExact of
      ('loc virtual_reason * 'loc virtual_reason) * 'loc virtual_use_op * exactness_error_kind
  | EFunctionIncompatibleWithIndexer of
      ('loc virtual_reason * 'loc virtual_reason) * 'loc virtual_use_op
  | EUnsupportedExact of ('loc virtual_reason * 'loc virtual_reason)
  | EUnexpectedThisType of 'loc
  | ETypeParamArity of 'loc * int
  | ECallTypeArity of {
      call_loc: 'loc;
      is_new: bool;
      reason_arity: 'loc virtual_reason;
      expected_arity: int;
    }
  | ETypeParamMinArity of 'loc * int
  | ETooManyTypeArgs of {
      reason_tapp: 'loc virtual_reason;
      arity_loc: 'loc;
      maximum_arity: int;
    }
  | ETooFewTypeArgs of {
      reason_tapp: 'loc virtual_reason;
      arity_loc: 'loc;
      minimum_arity: int;
    }
  | EInvalidInfer of 'loc
  | EInvalidTypeArgs of 'loc virtual_reason * 'loc virtual_reason
  | EInvalidExtends of 'loc virtual_reason
  | EStrUtilTypeNonLiteralArg of 'loc
  | EExportsAnnot of 'loc
  | EInvalidConstructor of 'loc virtual_reason
  | EUnsupportedKeyInObject of {
      loc: 'loc;
      obj_kind: [ `Type | `Literal ];
      key_error_kind: InvalidObjKey.t;
    }
  | EAmbiguousNumericKeyWithVariance of 'loc
  | ETypeGuardFuncIncompatibility of {
      use_op: 'loc virtual_use_op;
      reasons: 'loc virtual_reason * 'loc virtual_reason;
    }
  | ETypeGuardInvalidParameter of {
      type_guard_reason: 'loc virtual_reason;
      binding_reason: 'loc virtual_reason;
    }
  | ETypeGuardIndexMismatch of {
      use_op: 'loc virtual_use_op;
      reasons: 'loc virtual_reason * 'loc virtual_reason;
    }
  | ETypeGuardImpliesMismatch of {
      use_op: 'loc virtual_use_op;
      reasons: 'loc virtual_reason * 'loc virtual_reason;
    }
  | ETypeGuardParamUnbound of 'loc virtual_reason
  | ETypeGuardThisParam of 'loc virtual_reason
  | ETypeGuardFunctionInvalidWrites of {
      reason: 'loc virtual_reason;
      type_guard_reason: 'loc virtual_reason;
      write_locs: 'loc list;
    }
  | ETypeGuardFunctionParamHavoced of {
      type_guard_reason: 'loc virtual_reason;
      param_reason: 'loc virtual_reason;
      call_locs: 'loc list;
    }
  | ETypeGuardIncompatibleWithFunctionKind of {
      loc: 'loc;
      kind: string;
    }
  | ENegativeTypeGuardConsistency of {
      reason: 'loc virtual_reason;
      return_reason: 'loc virtual_reason;
      type_reason: 'loc virtual_reason;
    }
  | ETypeParamConstIncompatibility of {
      use_op: 'loc virtual_use_op;
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | ETypeParamConstInvalidPosition of 'loc virtual_reason
  | EInternal of 'loc * internal_error
  | EUnsupportedSyntax of 'loc * unsupported_syntax
  | EUseArrayLiteral of 'loc
  | EMissingLocalAnnotation of {
      reason: 'loc virtual_reason;
      hint_available: bool;
      from_generic_function: bool;
    }
  | EBindingError of binding_error * 'loc * name * ALoc.t
  | ERecursionLimit of ('loc virtual_reason * 'loc virtual_reason)
  | EUninitializedInstanceProperty of 'loc * Lints.property_assignment_kind
  | EEnumsNotEnabled of 'loc
  | EIndeterminateModuleType of 'loc
  | EBadExportPosition of 'loc
  | EBadExportContext of string * 'loc
  | EBadDefaultImportAccess of 'loc * 'loc virtual_reason
  | EBadDefaultImportDestructuring of 'loc
  | EInvalidImportStarUse of 'loc * 'loc virtual_reason
  | ENonConstVarExport of 'loc * 'loc virtual_reason option
  | EThisInExportedFunction of 'loc
  | EMixedImportAndRequire of 'loc * 'loc virtual_reason
  | EUnsupportedVarianceAnnotation of 'loc * string
  | EExportRenamedDefault of {
      loc: 'loc;
      name: string option;
      is_reexport: bool;
    }
  | EUnreachable of 'loc
  | EInvalidObjectKit of {
      reason: 'loc virtual_reason;
      reason_op: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EInvalidTypeof of 'loc * string
  | EBinaryInLHS of 'loc virtual_reason
  | EBinaryInRHS of 'loc virtual_reason
  | EArithmeticOperand of 'loc virtual_reason
  | EForInRHS of 'loc virtual_reason
  | EInstanceofRHS of 'loc virtual_reason
  | EObjectComputedPropertyAccess of ('loc virtual_reason * 'loc virtual_reason * InvalidObjKey.t)
  | EObjectComputedPropertyAssign of
      ('loc virtual_reason * 'loc virtual_reason option * InvalidObjKey.t)
  | EObjectComputedPropertyPotentialOverwrite of {
      key_loc: 'loc;
      overwritten_locs: 'loc list;
    }
  | EInvalidLHSInAssignment of 'loc
  | EIncompatibleWithUseOp of {
      use_op: 'loc virtual_use_op;
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      explanation: 'loc explanation option;
    }
  | EUnsupportedImplements of 'loc virtual_reason
  | ENotAReactComponent of {
      reason: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EInvalidReactCreateElement of {
      create_element_loc: 'loc;
      invalid_react: 'loc virtual_reason;
    }
  | EReactElementFunArity of 'loc virtual_reason * string * int
  | EReactRefInRender of {
      usage: 'loc virtual_reason;
      kind: ref_in_render_kind;
      in_hook: bool;
    }
  | EFunctionCallExtraArg of 'loc virtual_reason * 'loc virtual_reason * int * 'loc virtual_use_op
  | EUnsupportedSetProto of 'loc virtual_reason
  | EDuplicateModuleProvider of {
      module_name: string;
      provider: 'loc;
      conflict: 'loc;
    }
  | EParseError of 'loc * Parse_error.t
  | EDocblockError of 'loc * docblock_error
  | EImplicitInexactObject of 'loc
  | EAmbiguousObjectType of 'loc
  (* The string is either the name of a module or "the module that exports `_`". *)
  | EUntypedTypeImport of 'loc * Flow_import_specifier.userland
  | EUntypedImport of 'loc * Flow_import_specifier.userland
  | ENonstrictImport of 'loc
  | EUnclearType of 'loc
  | EDeprecatedBool of 'loc
  | EInternalType of 'loc * internal_type
  | EIncorrectTypeWithReplacement of {
      loc: 'loc;
      kind: IncorrectType.t;
    }
  | EUnsafeGettersSetters of 'loc
  | EUnsafeObjectAssign of 'loc
  | EUnusedSuppression of 'loc
  | ECodelessSuppression of 'loc * string
  | ELintSetting of 'loc * LintSettings.lint_parse_error
  | ESketchyNullLint of {
      kind: Lints.sketchy_null_kind;
      loc: 'loc;
      null_loc: 'loc;
      falsy_loc: 'loc;
    }
  | ESketchyNumberLint of Lints.sketchy_number_kind * 'loc virtual_reason
  | EInvalidPrototype of 'loc * 'loc virtual_reason
  | EUnnecessaryOptionalChain of 'loc * 'loc virtual_reason
  | EUnnecessaryInvariant of 'loc * 'loc virtual_reason
  | EUnnecessaryDeclareTypeOnlyExport of 'loc
  | EUnexpectedTemporaryBaseType of 'loc
  | ECannotDelete of 'loc * 'loc virtual_reason
  | ESignatureBindingValidation of 'loc Signature_error.binding_validation_t
  | ESignatureVerification of 'loc Signature_error.t
  | EPrimitiveAsInterface of {
      use_op: 'loc virtual_use_op;
      reason: 'loc virtual_reason;
      interface_reason: 'loc virtual_reason;
      kind: [ `Boolean | `Number | `String ];
    }
  | ECannotSpreadInterface of {
      spread_reason: 'loc virtual_reason;
      interface_reason: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | ECannotSpreadIndexerOnRight of {
      spread_reason: 'loc virtual_reason;
      object_reason: 'loc virtual_reason;
      key_reason: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EUnableToSpread of {
      spread_reason: 'loc virtual_reason;
      object1_reason: 'loc virtual_reason;
      object2_reason: 'loc virtual_reason;
      propname: name;
      error_kind: exactness_error_kind;
      use_op: 'loc virtual_use_op;
    }
  | EInexactMayOverwriteIndexer of {
      spread_reason: 'loc virtual_reason;
      key_reason: 'loc virtual_reason;
      value_reason: 'loc virtual_reason;
      object2_reason: 'loc virtual_reason;
      use_op: 'loc virtual_use_op;
    }
  | EExponentialSpread of {
      reason: 'loc virtual_reason;
      reasons_for_operand1: 'loc exponential_spread_reason_group;
      reasons_for_operand2: 'loc exponential_spread_reason_group;
    }
  | EComputedPropertyWithUnion of 'loc virtual_reason
  (* enums *)
  | EEnumInvalidMemberAccess of {
      member_name: name option;
      suggestion: string option;
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumModification of {
      loc: 'loc;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumMemberDuplicateValue of {
      loc: 'loc;
      prev_use_loc: 'loc;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumInvalidObjectUtilType of {
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumInvalidObjectFunction of {
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumNotIterable of {
      reason: 'loc virtual_reason;
      for_in: bool;
    }
  | EEnumMemberAlreadyChecked of {
      case_test_loc: 'loc;
      prev_check_loc: 'loc;
      enum_reason: 'loc virtual_reason;
      member_name: string;
    }
  | EEnumAllMembersAlreadyChecked of {
      loc: 'loc;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumNotAllChecked of {
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
      left_to_check: string list;
      default_case_loc: 'loc option;
    }
  | EEnumUnknownNotChecked of {
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumInvalidCheck of {
      loc: 'loc;
      enum_reason: 'loc virtual_reason;
      example_member: string option;
    }
  | EEnumMemberUsedAsType of {
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
    }
  | EEnumIncompatible of {
      use_op: 'loc virtual_use_op;
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      enum_kind: enum_kind;
      representation_type: string option;
      casting_syntax: Options.CastingSyntax.t;
    }
  | EEnumInvalidAbstractUse of {
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
    }
  (* end enum error messages *)
  | EAssignConstLikeBinding of {
      loc: 'loc;
      definition: 'loc virtual_reason;
      binding_kind: assigned_const_like_binding_type;
    }
  | EMalformedCode of 'loc
  | EImplicitInstantiationUnderconstrainedError of {
      reason_call: 'loc virtual_reason;
      reason_tparam: 'loc virtual_reason;
      bound: string;
      use_op: 'loc virtual_use_op;
    }
  | EClassToObject of 'loc virtual_reason * 'loc virtual_reason * 'loc virtual_use_op
  | EMethodUnbinding of {
      use_op: 'loc virtual_use_op;
      reason_prop: 'loc virtual_reason;
      reason_op: 'loc virtual_reason;
    }
  | EHookIncompatible of {
      use_op: 'loc virtual_use_op;
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      lower_is_hook: bool;
      hook_is_annot: bool;
    }
  | EHookUniqueIncompatible of {
      use_op: 'loc virtual_use_op;
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | EHookRuleViolation of {
      hook_rule: 'loc hook_rule;
      callee_loc: 'loc;
      call_loc: 'loc;
    }
  | EHookNaming of 'loc
  | EIncompatibleReactDeepReadOnly of {
      use_op: 'loc virtual_use_op;
      dro_loc: 'loc;
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | EObjectThisSuperReference of 'loc * 'loc virtual_reason * This_finder.kind
  | EComponentThisReference of {
      component_loc: 'loc;
      this_loc: 'loc;
    }
  | EComponentCase of 'loc
  | EComponentMissingReturn of 'loc virtual_reason
  | ENestedComponent of 'loc virtual_reason
  | EInvalidDeclaration of {
      declaration: 'loc virtual_reason;
      null_write: 'loc null_write option;
      possible_generic_escape_locs: 'loc list;
    }
  | EInvalidGraphQL of 'loc * Graphql.error
  | EAnnotationInference of 'loc * 'loc virtual_reason * 'loc virtual_reason * string option
  | ETrivialRecursiveDefinition of 'loc * 'loc virtual_reason
  | EDefinitionCycle of ('loc virtual_reason * 'loc list * 'loc Env_api.annot_loc list) Nel.t
  | ERecursiveDefinition of {
      reason: 'loc virtual_reason;
      recursion: 'loc list;
      annot_locs: 'loc Env_api.annot_loc list;
    }
  | EReferenceInAnnotation of ('loc * string * 'loc)
  | EDuplicateClassMember of {
      loc: 'loc;
      name: string;
      static: bool;
    }
  | EEmptyArrayNoProvider of { loc: 'loc }
  | EUnusedPromise of {
      loc: 'loc;
      async: bool;
    }
  | EBigIntRShift3 of 'loc virtual_reason
  | EBigIntNumCoerce of 'loc virtual_reason
  | EInvalidCatchParameterAnnotation of 'loc
  | ETSSyntax of {
      kind: ts_syntax_kind;
      loc: 'loc;
    }
  | EInvalidBinaryArith of {
      reason_out: 'loc virtual_reason;
      reason_l: 'loc virtual_reason;
      reason_r: 'loc virtual_reason;
      kind: ArithKind.t;
    }
  | EInvalidMappedType of {
      loc: 'loc;
      kind: invalid_mapped_type_error_kind;
    }
  | EDuplicateComponentProp of {
      spread: 'loc;
      duplicates: ('loc * name * 'loc) Nel.t;
    }
  | ERefComponentProp of {
      spread: 'loc;
      loc: 'loc;
    }
  | EKeySpreadProp of {
      spread: 'loc;
      loc: 'loc;
    }
  | EReactIntrinsicOverlap of {
      use: 'loc virtual_reason;
      def: 'loc;
      type_: 'loc;
      mixed: bool;
    }
  | EInvalidComponentRestParam of 'loc
  | EInvalidRendersTypeArgument of {
      loc: 'loc;
      renders_variant: Flow_ast.Type.Renders.variant;
      invalid_render_type_kind: 'loc invalid_render_type_kind;
      invalid_type_reasons: 'loc virtual_reason Nel.t;
    }
  | EInvalidTypeCastSyntax of {
      loc: 'loc;
      enabled_casting_syntax: Options.CastingSyntax.t;
    }
  | EMissingPlatformSupport of {
      loc: 'loc;
      available_platforms: SSet.t;
      required_platforms: SSet.t;
    }
  | EUnionPartialOptimizationNonUniqueKey of {
      loc: 'loc;
      non_unique_keys: 'loc virtual_reason Nel.t Type.UnionRep.UnionEnumMap.t NameUtils.Map.t;
    }
  | EUnionOptimization of {
      loc: 'loc;
      kind: 'loc Type.UnionRep.optimized_error;
    }
  | EUnionOptimizationOnNonUnion of {
      loc: 'loc;
      arg: 'loc virtual_reason;
    }
  | ECannotCallReactComponent of { reason: 'loc virtual_reason }
  (* Match *)
  | EMatchNotExhaustive of {
      loc: 'loc;
      reason: 'loc virtual_reason;
    }
  | EMatchInvalidBindingKind of {
      loc: 'loc;
      kind: Flow_ast.Variable.kind;
    }
  | EMatchInvalidObjectPropertyLiteral of { loc: 'loc }
  | EMatchInvalidUnaryZero of { loc: 'loc }
  | EMatchInvalidUnaryPlusBigInt of { loc: 'loc }
  | EMatchDuplicateObjectProperty of {
      loc: 'loc;
      name: string;
    }
  | EMatchBindingInOrPattern of { loc: 'loc }
  | EMatchInvalidAsPattern of { loc: 'loc }
  | EMatchInvalidPatternReference of {
      loc: 'loc;
      binding_reason: 'loc virtual_reason;
    }
  | EMatchInvalidObjectShorthand of {
      loc: 'loc;
      name: string;
    }
  | EMatchStatementInvalidBody of { loc: 'loc }
  | EUndocumentedFeature of { loc: 'loc }
  | EIllegalAssertOperator of {
      op: 'loc virtual_reason;
      obj: 'loc virtual_reason;
      specialized: bool;
    }
  (* Dev only *)
  | EDevOnlyRefinedLocInfo of {
      refined_loc: 'loc;
      refining_locs: 'loc list;
    }
  | EDevOnlyInvalidatedRefinementInfo of {
      read_loc: 'loc;
      invalidation_info: ('loc * Refinement_invalidation.reason) list;
    }
  (* As the name suggest, don't use this for production purposes, but feel free to use it to
   * quickly test out some ideas. *)
  | ETemporaryHardcodedErrorForPrototyping of 'loc virtual_reason * string

and enum_kind =
  | ConcreteEnumKind
  | AbstractEnumKind

and 'loc null_write = {
  null_loc: 'loc;
  initialized: bool;
}

and ref_in_render_kind =
  | Argument
  | Access

and binding_error =
  | EGlobalAlreadyDeclared
  | ENameAlreadyBound
  | EVarRedeclaration
  | EReferencedBeforeDeclaration
  | ETypeInValuePosition of {
      imported: bool;
      type_only_namespace: bool;
      name: string;
    }
  | EConstReassigned
  | EConstParamReassigned
  | EImportReassigned
  | EEnumReassigned

and internal_error =
  | MethodNotAFunction
  | OptionalMethod
  | PropertyDescriptorPropertyCannotBeRead
  | ForInLHS
  | ForOfLHS
  | PropRefComputedOpen
  | PropRefComputedLiteral
  | RestParameterNotIdentifierPattern
  | InterfaceTypeSpread
  | DebugThrow
  | ParseJobException of Exception.t
  | CheckTimeout of float
  | CheckJobException of Exception.t
  | UnexpectedAnnotationInference of string
  | MissingEnvRead of ALoc.t
  | MissingEnvWrite of ALoc.t
  | ReadOfUnreachedTvar of Env_api.def_loc_type
  | ReadOfUnresolvedTvar of Env_api.def_loc_type
  | ForcedReadOfUnderResolutionTvar of Env_api.def_loc_type
  | EnvInvariant of Env_api.env_invariant_failure
  | ImplicitInstantiationInvariant of string

and lower_kind =
  | Possibly_null
  | Possibly_void
  | Possibly_null_or_void
  | Incompatible_intersection

and 'loc upper_kind =
  | IncompatibleGetPropT of 'loc * name option
  | IncompatibleSetPropT of 'loc * name option
  | IncompatibleGetPrivatePropT
  | IncompatibleSetPrivatePropT
  | IncompatibleMethodT of 'loc * name option
  | IncompatibleCallT
  | IncompatibleMixedCallT
  | IncompatibleGetElemT of 'loc
  | IncompatibleSetElemT of 'loc
  | IncompatibleCallElemT of 'loc
  | IncompatibleElemTOfArrT
  | IncompatibleObjAssignFromTSpread
  | IncompatibleObjAssignFromT
  | IncompatibleObjRestT
  | IncompatibleArrRestT
  | IncompatibleSuperT
  | IncompatibleMixinT
  | IncompatibleSpecializeT
  | IncompatibleThisSpecializeT
  | IncompatibleVarianceCheckT
  | IncompatibleGetKeysT
  | IncompatibleHasOwnPropT of 'loc * name option
  | IncompatibleGetValuesT
  | IncompatibleMapTypeTObject
  | IncompatibleGetStaticsT
  | IncompatibleBindT
  | IncompatibleUnclassified of string

and ts_syntax_kind =
  | TSUnknown
  | TSNever
  | TSUndefined
  | TSKeyof
  | TSTypeParamExtends
  | TSReadonlyVariance
  | TSInOutVariance of [ `In | `Out | `InOut ]
  | TSSatisfiesType of Options.CastingSyntax.t
  | TSReadonlyType of [ `Tuple | `Array ] option

and invalid_mapped_type_error_kind =
  | InterfaceOrDeclaredClass
  | ExtraProperties
  | ExplicitExactOrInexact
  | RemoveOptionality
  | VarianceOnArrayInput

and 'l hook_rule =
  | ConditionalHook
  | HookHasIllegalName
  | NotHookSyntaxHook
  | MaybeHook of {
      hooks: 'l list;
      non_hooks: 'l list;
    }
  | HookDefinitelyNotInComponentOrHook
  | HookInUnknownContext
  | HookNotInComponentSyntaxComponentOrHookSyntaxHook

let string_of_invalid_render_type_kind = function
  | InvalidRendersNullVoidFalse -> "null | void | false"
  | InvalidRendersIterable -> "iterable"
  | InvalidRendersStructural _ -> "non-nominal-return"
  | InvalidRendersNonNominalElement _ -> "non-nominal"
  | InvalidRendersGenericT -> "generic"
  | UncategorizedInvalidRenders -> "uncategorized"

let map_loc_of_exponential_spread_reason_group f { first_reason; second_reason } =
  { first_reason = f first_reason; second_reason = Base.Option.map ~f second_reason }

let map_loc_of_invalid_render_type_kind f = function
  | InvalidRendersNullVoidFalse -> InvalidRendersNullVoidFalse
  | InvalidRendersIterable -> InvalidRendersIterable
  | InvalidRendersStructural r -> InvalidRendersStructural (f r)
  | InvalidRendersNonNominalElement r -> InvalidRendersNonNominalElement (f r)
  | InvalidRendersGenericT -> InvalidRendersGenericT
  | UncategorizedInvalidRenders -> UncategorizedInvalidRenders

let map_loc_of_explanation (f : 'a -> 'b) =
  let map_reason = Reason.map_reason_locs f in
  function
  | ExplanationAbstractEnumCasting -> ExplanationAbstractEnumCasting
  | ExplanationArrayInvariantTyping -> ExplanationArrayInvariantTyping
  | ExplanationConstrainedAssign { name; declaration; providers } ->
    ExplanationConstrainedAssign
      { name; declaration = f declaration; providers = Base.List.map ~f providers }
  | ExplanationConcreteEnumCasting { representation_type; casting_syntax } ->
    ExplanationConcreteEnumCasting { representation_type; casting_syntax }
  | ExplanationFunctionsWithStaticsToObject -> ExplanationFunctionsWithStaticsToObject
  | ExplanationMultiplatform -> ExplanationMultiplatform
  | ExplanationNonCallableObjectToFunction -> ExplanationNonCallableObjectToFunction
  | ExplanationPropertyInvariantTyping -> ExplanationPropertyInvariantTyping
  | ExplanationReactComponentPropsDeepReadOnly loc ->
    ExplanationReactComponentPropsDeepReadOnly (f loc)
  | ExplanationReactComponentRefRequirement -> ExplanationReactComponentRefRequirement
  | ExplanationReactHookArgsDeepReadOnly loc -> ExplanationReactHookArgsDeepReadOnly (f loc)
  | ExplanationReactHookIncompatibleWithEachOther -> ExplanationReactHookIncompatibleWithEachOther
  | ExplanationReactHookIncompatibleWithNormalFunctions ->
    ExplanationReactHookIncompatibleWithNormalFunctions
  | ExplanationReactHookReturnDeepReadOnly loc -> ExplanationReactHookReturnDeepReadOnly (f loc)
  | ExplanationReactImmutable loc -> ExplanationReactImmutable (f loc)
  | ExplanationIncompatibleReactDeepReadOnly -> ExplanationIncompatibleReactDeepReadOnly
  | ExplanationRenderTypeRequirement -> ExplanationRenderTypeRequirement
  | ExplanationTypeGuardCompatibility -> ExplanationTypeGuardCompatibility
  | ExplanationTypeGuardPositiveConsistency { return; param; guard_type; is_return_false_statement }
    ->
    ExplanationTypeGuardPositiveConsistency
      {
        return = map_reason return;
        param = map_reason param;
        guard_type = map_reason guard_type;
        is_return_false_statement;
      }
  | ExplanationAdditionalUnionMembers { left; right; members; extra_number } ->
    ExplanationAdditionalUnionMembers
      { left = map_reason left; right = map_reason right; members; extra_number }

let rec map_loc_of_error_message (f : 'a -> 'b) : 'a t' -> 'b t' =
  let map_use_op = TypeUtil.mod_loc_of_virtual_use_op f in
  let map_reason = Reason.map_reason_locs f in
  let map_branch e = map_loc_of_error_message f e in
  let map_upper_kind = function
    | IncompatibleGetPropT (loc, s) -> IncompatibleGetPropT (f loc, s)
    | IncompatibleSetPropT (loc, s) -> IncompatibleSetPropT (f loc, s)
    | IncompatibleMethodT (loc, s) -> IncompatibleMethodT (f loc, s)
    | IncompatibleHasOwnPropT (loc, s) -> IncompatibleHasOwnPropT (f loc, s)
    | IncompatibleGetElemT loc -> IncompatibleGetElemT (f loc)
    | IncompatibleSetElemT loc -> IncompatibleSetElemT (f loc)
    | IncompatibleCallElemT loc -> IncompatibleCallElemT (f loc)
    | ( IncompatibleGetPrivatePropT | IncompatibleSetPrivatePropT | IncompatibleCallT
      | IncompatibleMixedCallT | IncompatibleElemTOfArrT | IncompatibleObjAssignFromTSpread
      | IncompatibleObjAssignFromT | IncompatibleObjRestT | IncompatibleArrRestT
      | IncompatibleSuperT | IncompatibleMixinT | IncompatibleSpecializeT
      | IncompatibleThisSpecializeT | IncompatibleVarianceCheckT | IncompatibleGetKeysT
      | IncompatibleGetValuesT | IncompatibleMapTypeTObject | IncompatibleGetStaticsT
      | IncompatibleBindT | IncompatibleUnclassified _ ) as u ->
      u
  in
  function
  | EIncompatible { use_op; lower = (lreason, lkind); upper = (ureason, ukind) } ->
    EIncompatible
      {
        use_op = Base.Option.map ~f:map_use_op use_op;
        lower = (map_reason lreason, lkind);
        upper = (map_reason ureason, map_upper_kind ukind);
      }
  | EIncompatibleSpeculation { use_op; loc; branches } ->
    EIncompatibleSpeculation
      {
        use_op = Base.Option.map ~f:map_use_op use_op;
        loc = f loc;
        branches = Base.List.map ~f:map_branch branches;
      }
  | EIncompatibleDefs { use_op; reason_lower; reason_upper; branches } ->
    EIncompatibleDefs
      {
        use_op = map_use_op use_op;
        reason_lower = map_reason reason_lower;
        reason_upper = map_reason reason_upper;
        branches = Base.List.map ~f:map_branch branches;
      }
  | EIncompatibleProp { use_op; prop; reason_prop; reason_obj; special } ->
    EIncompatibleProp
      {
        use_op = Base.Option.map ~f:map_use_op use_op;
        prop;
        reason_prop = map_reason reason_prop;
        reason_obj = map_reason reason_obj;
        special;
      }
  | EExpectedStringLit { reason_lower; reason_upper; use_op } ->
    EExpectedStringLit
      {
        reason_lower = map_reason reason_lower;
        reason_upper = map_reason reason_upper;
        use_op = map_use_op use_op;
      }
  | EExpectedNumberLit { reason_lower; reason_upper; use_op } ->
    EExpectedNumberLit
      {
        reason_lower = map_reason reason_lower;
        reason_upper = map_reason reason_upper;
        use_op = map_use_op use_op;
      }
  | EExpectedBooleanLit { reason_lower; reason_upper; use_op } ->
    EExpectedBooleanLit
      {
        reason_lower = map_reason reason_lower;
        reason_upper = map_reason reason_upper;
        use_op = map_use_op use_op;
      }
  | EExpectedBigIntLit { reason_lower; reason_upper; use_op } ->
    EExpectedBigIntLit
      {
        reason_lower = map_reason reason_lower;
        reason_upper = map_reason reason_upper;
        use_op = map_use_op use_op;
      }
  | EPropNotFound { prop_name; reason_prop; reason_obj; use_op; suggestion } ->
    EPropNotFound
      {
        prop_name;
        reason_prop = map_reason reason_prop;
        reason_obj = map_reason reason_obj;
        use_op = map_use_op use_op;
        suggestion;
      }
  | EIndexerCheckFailed { prop_name; reason_prop; reason_obj; reason_indexer; use_op } ->
    EIndexerCheckFailed
      {
        prop_name;
        reason_prop = map_reason reason_prop;
        reason_obj = map_reason reason_obj;
        reason_indexer = map_reason reason_indexer;
        use_op = map_use_op use_op;
      }
  | EPropNotReadable { reason_prop; prop_name; use_op } ->
    EPropNotReadable { reason_prop = map_reason reason_prop; prop_name; use_op = map_use_op use_op }
  | EPropNotWritable { reason_prop; prop_name; use_op } ->
    EPropNotWritable { reason_prop = map_reason reason_prop; prop_name; use_op = map_use_op use_op }
  | EPropPolarityMismatch ((r1, r2), p, ps, op) ->
    EPropPolarityMismatch ((map_reason r1, map_reason r2), p, ps, map_use_op op)
  | EBuiltinNameLookupFailed { loc; name } -> EBuiltinNameLookupFailed { loc = f loc; name }
  | EBuiltinModuleLookupFailed { loc; name; potential_generator } ->
    EBuiltinModuleLookupFailed { loc = f loc; name; potential_generator }
  | EExpectedModuleLookupFailed { loc; name; expected_module_purpose } ->
    EExpectedModuleLookupFailed { loc = f loc; name; expected_module_purpose }
  | EPrivateLookupFailed ((r1, r2), x, op) ->
    EPrivateLookupFailed ((map_reason r1, map_reason r2), x, map_use_op op)
  | EPlatformSpecificImplementationModuleLookupFailed { loc; name } ->
    EPlatformSpecificImplementationModuleLookupFailed { loc = f loc; name }
  | ETupleArityMismatch
      {
        use_op;
        lower_reason;
        lower_arity;
        lower_inexact;
        upper_reason;
        upper_arity;
        upper_inexact;
        unify;
      } ->
    ETupleArityMismatch
      {
        use_op = map_use_op use_op;
        lower_reason = map_reason lower_reason;
        lower_arity;
        lower_inexact;
        upper_reason = map_reason upper_reason;
        upper_arity;
        upper_inexact;
        unify;
      }
  | ENonLitArrayToTuple ((r1, r2), op) ->
    ENonLitArrayToTuple ((map_reason r1, map_reason r2), map_use_op op)
  | ETupleOutOfBounds { use_op; reason; reason_op; inexact; length; index } ->
    ETupleOutOfBounds
      {
        use_op = map_use_op use_op;
        reason = map_reason reason;
        reason_op = map_reason reason_op;
        inexact;
        length;
        index;
      }
  | ETupleNonIntegerIndex { use_op; reason; index } ->
    ETupleNonIntegerIndex { use_op = map_use_op use_op; reason = map_reason reason; index }
  | ETupleUnsafeWrite { reason; use_op } ->
    ETupleUnsafeWrite { reason = map_reason reason; use_op = map_use_op use_op }
  | ETupleElementNotReadable { reason; index; name; use_op } ->
    ETupleElementNotReadable { reason = map_reason reason; index; name; use_op = map_use_op use_op }
  | ETupleElementNotWritable { reason; index; name; use_op } ->
    ETupleElementNotWritable { reason = map_reason reason; index; name; use_op = map_use_op use_op }
  | ETupleElementPolarityMismatch
      { index; reason_lower; polarity_lower; reason_upper; polarity_upper; use_op } ->
    ETupleElementPolarityMismatch
      {
        index;
        reason_lower = map_reason reason_lower;
        polarity_lower;
        reason_upper = map_reason reason_upper;
        polarity_upper;
        use_op = map_use_op use_op;
      }
  | ETupleRequiredAfterOptional { reason_tuple; reason_required; reason_optional } ->
    ETupleRequiredAfterOptional
      {
        reason_tuple = map_reason reason_tuple;
        reason_required = map_reason reason_required;
        reason_optional = map_reason reason_optional;
      }
  | ETupleInvalidTypeSpread { reason_spread; reason_arg } ->
    ETupleInvalidTypeSpread
      { reason_spread = map_reason reason_spread; reason_arg = map_reason reason_arg }
  | ETupleElementAfterInexactSpread reason -> ETupleElementAfterInexactSpread (map_reason reason)
  | EROArrayWrite ((r1, r2), op) -> EROArrayWrite ((map_reason r1, map_reason r2), map_use_op op)
  | EUnionSpeculationFailed { use_op; reason; op_reasons; branches } ->
    EUnionSpeculationFailed
      {
        use_op = map_use_op use_op;
        reason = map_reason reason;
        op_reasons = Nel.map map_reason op_reasons;
        branches = Base.List.map ~f:map_branch branches;
      }
  | EIncompatibleWithExact ((r1, r2), op, kind) ->
    EIncompatibleWithExact ((map_reason r1, map_reason r2), map_use_op op, kind)
  | EFunctionIncompatibleWithIndexer ((r1, r2), op) ->
    EFunctionIncompatibleWithIndexer ((map_reason r1, map_reason r2), map_use_op op)
  | EInvalidConstructor r -> EInvalidConstructor (map_reason r)
  | EInvalidObjectKit { reason; reason_op; use_op } ->
    EInvalidObjectKit
      { reason = map_reason reason; reason_op = map_reason reason_op; use_op = map_use_op use_op }
  | EIncompatibleWithUseOp { use_op; reason_lower; reason_upper; explanation } ->
    EIncompatibleWithUseOp
      {
        use_op = map_use_op use_op;
        reason_lower = map_reason reason_lower;
        reason_upper = map_reason reason_upper;
        explanation = Base.Option.map ~f:(map_loc_of_explanation f) explanation;
      }
  | ENotAReactComponent { reason; use_op } ->
    ENotAReactComponent { reason = map_reason reason; use_op = map_use_op use_op }
  | EInvalidReactCreateElement { create_element_loc; invalid_react } ->
    EInvalidReactCreateElement
      { create_element_loc = f create_element_loc; invalid_react = map_reason invalid_react }
  | EFunctionCallExtraArg (rl, ru, n, op) ->
    EFunctionCallExtraArg (map_reason rl, map_reason ru, n, map_use_op op)
  | EExportValueAsType (r, s) -> EExportValueAsType (map_reason r, s)
  | EImportValueAsType (r, s) -> EImportValueAsType (map_reason r, s)
  | EImportTypeAsTypeof (r, s) -> EImportTypeAsTypeof (map_reason r, s)
  | EImportTypeAsValue (r, s) -> EImportTypeAsValue (map_reason r, s)
  | ENoDefaultExport (r, s1, s2) -> ENoDefaultExport (map_reason r, s1, s2)
  | EOnlyDefaultExport (r, s1, s2) -> EOnlyDefaultExport (map_reason r, s1, s2)
  | ENoNamedExport (r, s1, s2, s3) -> ENoNamedExport (map_reason r, s1, s2, s3)
  | EMissingTypeArgs { reason_op; reason_tapp; arity_loc; min_arity; max_arity } ->
    EMissingTypeArgs
      {
        reason_op = map_reason reason_op;
        reason_tapp = map_reason reason_tapp;
        arity_loc = f arity_loc;
        min_arity;
        max_arity;
      }
  | EAnyValueUsedAsType { reason_use } -> EAnyValueUsedAsType { reason_use = map_reason reason_use }
  | EValueUsedAsType { reason_use } -> EValueUsedAsType { reason_use = map_reason reason_use }
  | EPolarityMismatch { reason; name; expected_polarity; actual_polarity } ->
    EPolarityMismatch { reason = map_reason reason; name; expected_polarity; actual_polarity }
  | EComparison (r1, r2) -> EComparison (map_reason r1, map_reason r2)
  | ENonStrictEqualityComparison (r1, r2) ->
    ENonStrictEqualityComparison (map_reason r1, map_reason r2)
  | EUnsupportedExact (r1, r2) -> EUnsupportedExact (map_reason r1, map_reason r2)
  | EUnexpectedThisType loc -> EUnexpectedThisType (f loc)
  | ETypeParamArity (loc, i) -> ETypeParamArity (f loc, i)
  | ECallTypeArity { call_loc; is_new; reason_arity; expected_arity } ->
    ECallTypeArity
      { call_loc = f call_loc; is_new; expected_arity; reason_arity = map_reason reason_arity }
  | ETypeParamMinArity (loc, i) -> ETypeParamMinArity (f loc, i)
  | ETooManyTypeArgs { reason_tapp; arity_loc; maximum_arity } ->
    ETooManyTypeArgs
      { reason_tapp = map_reason reason_tapp; arity_loc = f arity_loc; maximum_arity }
  | ETooFewTypeArgs { reason_tapp; arity_loc; minimum_arity } ->
    ETooFewTypeArgs { reason_tapp = map_reason reason_tapp; arity_loc = f arity_loc; minimum_arity }
  | EInvalidTypeArgs (r1, r2) -> EInvalidTypeArgs (map_reason r1, map_reason r2)
  | EInvalidInfer l -> EInvalidInfer (f l)
  | EInvalidExtends r -> EInvalidExtends (map_reason r)
  | EStrUtilTypeNonLiteralArg loc -> EStrUtilTypeNonLiteralArg (f loc)
  | EExportsAnnot loc -> EExportsAnnot (f loc)
  | EUnsupportedKeyInObject { loc; obj_kind; key_error_kind } ->
    EUnsupportedKeyInObject { loc = f loc; obj_kind; key_error_kind }
  | EAmbiguousNumericKeyWithVariance loc -> EAmbiguousNumericKeyWithVariance (f loc)
  | ETypeGuardFuncIncompatibility { use_op; reasons = (r1, r2) } ->
    ETypeGuardFuncIncompatibility
      { use_op = map_use_op use_op; reasons = (map_reason r1, map_reason r2) }
  | ETypeGuardInvalidParameter { type_guard_reason; binding_reason } ->
    ETypeGuardInvalidParameter
      {
        type_guard_reason = map_reason type_guard_reason;
        binding_reason = map_reason binding_reason;
      }
  | ETypeGuardIndexMismatch { use_op; reasons = (r1, r2) } ->
    ETypeGuardIndexMismatch { use_op = map_use_op use_op; reasons = (map_reason r1, map_reason r2) }
  | ETypeGuardImpliesMismatch { use_op; reasons = (r1, r2) } ->
    ETypeGuardImpliesMismatch
      { use_op = map_use_op use_op; reasons = (map_reason r1, map_reason r2) }
  | ETypeGuardParamUnbound reason -> ETypeGuardParamUnbound (map_reason reason)
  | ETypeGuardThisParam reason -> ETypeGuardThisParam (map_reason reason)
  | ETypeGuardFunctionInvalidWrites { reason; type_guard_reason; write_locs } ->
    ETypeGuardFunctionInvalidWrites
      {
        reason = map_reason reason;
        type_guard_reason = map_reason type_guard_reason;
        write_locs = Base.List.map ~f write_locs;
      }
  | ETypeGuardFunctionParamHavoced { type_guard_reason; param_reason; call_locs } ->
    ETypeGuardFunctionParamHavoced
      {
        type_guard_reason = map_reason type_guard_reason;
        param_reason = map_reason param_reason;
        call_locs = Base.List.map ~f call_locs;
      }
  | ETypeGuardIncompatibleWithFunctionKind { loc; kind } ->
    ETypeGuardIncompatibleWithFunctionKind { loc = f loc; kind }
  | ENegativeTypeGuardConsistency { reason; return_reason; type_reason } ->
    ENegativeTypeGuardConsistency
      {
        reason = map_reason reason;
        return_reason = map_reason return_reason;
        type_reason = map_reason type_reason;
      }
  | ETypeParamConstIncompatibility { use_op; lower; upper } ->
    ETypeParamConstIncompatibility
      { use_op = map_use_op use_op; lower = map_reason lower; upper = map_reason upper }
  | ETypeParamConstInvalidPosition reason -> ETypeParamConstInvalidPosition (map_reason reason)
  | EInternal (loc, i) -> EInternal (f loc, i)
  | EUnsupportedSyntax (loc, u) -> EUnsupportedSyntax (f loc, u)
  | EUseArrayLiteral loc -> EUseArrayLiteral (f loc)
  | EMissingLocalAnnotation { reason; hint_available; from_generic_function } ->
    EMissingLocalAnnotation { reason = map_reason reason; hint_available; from_generic_function }
  | EBindingError (b, loc, s, scope) -> EBindingError (b, f loc, s, scope)
  | ERecursionLimit (r1, r2) -> ERecursionLimit (map_reason r1, map_reason r2)
  | EUninitializedInstanceProperty (loc, e) -> EUninitializedInstanceProperty (f loc, e)
  | EEnumsNotEnabled loc -> EEnumsNotEnabled (f loc)
  | EIndeterminateModuleType loc -> EIndeterminateModuleType (f loc)
  | EBadExportPosition loc -> EBadExportPosition (f loc)
  | EBadExportContext (s, loc) -> EBadExportContext (s, f loc)
  | EBadDefaultImportAccess (loc, r) -> EBadDefaultImportAccess (f loc, map_reason r)
  | EBadDefaultImportDestructuring loc -> EBadDefaultImportDestructuring (f loc)
  | EInvalidImportStarUse (loc, r) -> EInvalidImportStarUse (f loc, map_reason r)
  | ENonConstVarExport (loc, r) -> ENonConstVarExport (f loc, Base.Option.map ~f:map_reason r)
  | EThisInExportedFunction loc -> EThisInExportedFunction (f loc)
  | EMixedImportAndRequire (loc, r) -> EMixedImportAndRequire (f loc, map_reason r)
  | EUnsupportedVarianceAnnotation (loc, k) -> EUnsupportedVarianceAnnotation (f loc, k)
  | EExportRenamedDefault { loc; name; is_reexport } ->
    EExportRenamedDefault { loc = f loc; name; is_reexport }
  | EUnreachable loc -> EUnreachable (f loc)
  | EInvalidTypeof (loc, s) -> EInvalidTypeof (f loc, s)
  | EBinaryInLHS r -> EBinaryInLHS (map_reason r)
  | EBinaryInRHS r -> EBinaryInRHS (map_reason r)
  | EArithmeticOperand r -> EArithmeticOperand (map_reason r)
  | EForInRHS r -> EForInRHS (map_reason r)
  | EInstanceofRHS r -> EInstanceofRHS (map_reason r)
  | EObjectComputedPropertyAccess (r1, r2, kind) ->
    EObjectComputedPropertyAccess (map_reason r1, map_reason r2, kind)
  | EObjectComputedPropertyAssign (r1, r2, kind) ->
    EObjectComputedPropertyAssign (map_reason r1, Base.Option.map ~f:map_reason r2, kind)
  | EObjectComputedPropertyPotentialOverwrite { key_loc; overwritten_locs } ->
    EObjectComputedPropertyPotentialOverwrite
      { key_loc = f key_loc; overwritten_locs = List.map f overwritten_locs }
  | EInvalidLHSInAssignment l -> EInvalidLHSInAssignment (f l)
  | EUnsupportedImplements r -> EUnsupportedImplements (map_reason r)
  | EReactElementFunArity (r, s, i) -> EReactElementFunArity (map_reason r, s, i)
  | EReactRefInRender { usage; kind; in_hook } ->
    EReactRefInRender { usage = map_reason usage; kind; in_hook }
  | EUnsupportedSetProto r -> EUnsupportedSetProto (map_reason r)
  | EDuplicateModuleProvider { module_name; provider; conflict } ->
    EDuplicateModuleProvider { module_name; provider = f provider; conflict = f conflict }
  | EParseError (loc, p) -> EParseError (f loc, p)
  | EDocblockError (loc, e) -> EDocblockError (f loc, e)
  | EImplicitInexactObject loc -> EImplicitInexactObject (f loc)
  | EAmbiguousObjectType loc -> EAmbiguousObjectType (f loc)
  | EUntypedTypeImport (loc, s) -> EUntypedTypeImport (f loc, s)
  | EUntypedImport (loc, s) -> EUntypedImport (f loc, s)
  | ENonstrictImport loc -> ENonstrictImport (f loc)
  | EUnclearType loc -> EUnclearType (f loc)
  | EDeprecatedBool loc -> EDeprecatedBool (f loc)
  | EInternalType (loc, kind) -> EInternalType (f loc, kind)
  | EIncorrectTypeWithReplacement { loc; kind } ->
    EIncorrectTypeWithReplacement { loc = f loc; kind }
  | EUnsafeGettersSetters loc -> EUnsafeGettersSetters (f loc)
  | EUnsafeObjectAssign loc -> EUnsafeObjectAssign (f loc)
  | EUnusedSuppression loc -> EUnusedSuppression (f loc)
  | ECodelessSuppression (loc, c) -> ECodelessSuppression (f loc, c)
  | ELintSetting (loc, err) -> ELintSetting (f loc, err)
  | ESketchyNullLint { kind; loc; null_loc; falsy_loc } ->
    ESketchyNullLint { kind; loc = f loc; null_loc = f null_loc; falsy_loc = f falsy_loc }
  | ESketchyNumberLint (kind, r) -> ESketchyNumberLint (kind, map_reason r)
  | EInvalidPrototype (loc, r) -> EInvalidPrototype (f loc, map_reason r)
  | EUnnecessaryOptionalChain (loc, r) -> EUnnecessaryOptionalChain (f loc, map_reason r)
  | EUnnecessaryInvariant (loc, r) -> EUnnecessaryInvariant (f loc, map_reason r)
  | EUnnecessaryDeclareTypeOnlyExport loc -> EUnnecessaryDeclareTypeOnlyExport (f loc)
  | EUnexpectedTemporaryBaseType loc -> EUnexpectedTemporaryBaseType (f loc)
  | ECannotDelete (l1, r1) -> ECannotDelete (f l1, map_reason r1)
  | ESignatureBindingValidation sve ->
    ESignatureBindingValidation (Signature_error.map_binding_validation_t f sve)
  | ESignatureVerification sve -> ESignatureVerification (Signature_error.map f sve)
  | EPrimitiveAsInterface { use_op; reason; interface_reason; kind } ->
    EPrimitiveAsInterface
      {
        use_op = map_use_op use_op;
        reason = map_reason reason;
        interface_reason = map_reason interface_reason;
        kind;
      }
  | ECannotSpreadInterface { spread_reason; interface_reason; use_op } ->
    ECannotSpreadInterface
      {
        spread_reason = map_reason spread_reason;
        interface_reason = map_reason interface_reason;
        use_op = map_use_op use_op;
      }
  | ECannotSpreadIndexerOnRight { spread_reason; object_reason; key_reason; use_op } ->
    ECannotSpreadIndexerOnRight
      {
        spread_reason = map_reason spread_reason;
        object_reason = map_reason object_reason;
        key_reason = map_reason key_reason;
        use_op = map_use_op use_op;
      }
  | EUnableToSpread { spread_reason; object1_reason; object2_reason; propname; error_kind; use_op }
    ->
    EUnableToSpread
      {
        spread_reason = map_reason spread_reason;
        object1_reason = map_reason object1_reason;
        object2_reason = map_reason object2_reason;
        propname;
        error_kind;
        use_op = map_use_op use_op;
      }
  | EInexactMayOverwriteIndexer { spread_reason; key_reason; value_reason; object2_reason; use_op }
    ->
    EInexactMayOverwriteIndexer
      {
        spread_reason = map_reason spread_reason;
        key_reason = map_reason key_reason;
        value_reason = map_reason value_reason;
        object2_reason = map_reason object2_reason;
        use_op = map_use_op use_op;
      }
  | EExponentialSpread { reason; reasons_for_operand1; reasons_for_operand2 } ->
    EExponentialSpread
      {
        reason = map_reason reason;
        reasons_for_operand1 =
          map_loc_of_exponential_spread_reason_group map_reason reasons_for_operand1;
        reasons_for_operand2 =
          map_loc_of_exponential_spread_reason_group map_reason reasons_for_operand2;
      }
  | EComputedPropertyWithUnion reason -> EComputedPropertyWithUnion (map_reason reason)
  | EEnumInvalidMemberAccess { member_name; suggestion; reason; enum_reason } ->
    EEnumInvalidMemberAccess
      { member_name; suggestion; reason = map_reason reason; enum_reason = map_reason enum_reason }
  | EEnumModification { loc; enum_reason } ->
    EEnumModification { loc = f loc; enum_reason = map_reason enum_reason }
  | EEnumMemberDuplicateValue { loc; prev_use_loc; enum_reason } ->
    EEnumMemberDuplicateValue
      { loc = f loc; prev_use_loc = f prev_use_loc; enum_reason = map_reason enum_reason }
  | EEnumInvalidObjectUtilType { reason; enum_reason } ->
    EEnumInvalidObjectUtilType { reason = map_reason reason; enum_reason = map_reason enum_reason }
  | EEnumInvalidObjectFunction { reason; enum_reason } ->
    EEnumInvalidObjectFunction { reason = map_reason reason; enum_reason = map_reason enum_reason }
  | EEnumNotIterable { reason; for_in } -> EEnumNotIterable { reason = map_reason reason; for_in }
  | EEnumMemberAlreadyChecked { case_test_loc; prev_check_loc; enum_reason; member_name } ->
    EEnumMemberAlreadyChecked
      {
        case_test_loc = f case_test_loc;
        prev_check_loc = f prev_check_loc;
        enum_reason = map_reason enum_reason;
        member_name;
      }
  | EEnumAllMembersAlreadyChecked { loc; enum_reason } ->
    EEnumAllMembersAlreadyChecked { loc = f loc; enum_reason = map_reason enum_reason }
  | EEnumNotAllChecked { reason; enum_reason; left_to_check; default_case_loc } ->
    EEnumNotAllChecked
      {
        reason = map_reason reason;
        enum_reason = map_reason enum_reason;
        left_to_check;
        default_case_loc = Option.map f default_case_loc;
      }
  | EEnumUnknownNotChecked { reason; enum_reason } ->
    EEnumUnknownNotChecked { reason = map_reason reason; enum_reason = map_reason enum_reason }
  | EEnumInvalidCheck { loc; enum_reason; example_member } ->
    EEnumInvalidCheck { loc = f loc; enum_reason = map_reason enum_reason; example_member }
  | EEnumMemberUsedAsType { reason; enum_reason } ->
    EEnumMemberUsedAsType { reason = map_reason reason; enum_reason = map_reason enum_reason }
  | EEnumIncompatible
      { use_op; reason_lower; reason_upper; enum_kind; representation_type; casting_syntax } ->
    EEnumIncompatible
      {
        use_op = map_use_op use_op;
        reason_lower = map_reason reason_lower;
        reason_upper = map_reason reason_upper;
        enum_kind;
        representation_type;
        casting_syntax;
      }
  | EEnumInvalidAbstractUse { reason; enum_reason } ->
    EEnumInvalidAbstractUse { reason = map_reason reason; enum_reason = map_reason enum_reason }
  | EAssignConstLikeBinding { loc; definition; binding_kind } ->
    EAssignConstLikeBinding { loc = f loc; definition = map_reason definition; binding_kind }
  | EMalformedCode loc -> EMalformedCode (f loc)
  | EImplicitInstantiationUnderconstrainedError { reason_call; reason_tparam; bound; use_op } ->
    EImplicitInstantiationUnderconstrainedError
      {
        reason_call = map_reason reason_call;
        reason_tparam = map_reason reason_tparam;
        bound;
        use_op = map_use_op use_op;
      }
  | EClassToObject (r1, r2, op) -> EClassToObject (map_reason r1, map_reason r2, map_use_op op)
  | EMethodUnbinding { use_op; reason_op; reason_prop } ->
    EMethodUnbinding
      {
        use_op = map_use_op use_op;
        reason_op = map_reason reason_op;
        reason_prop = map_reason reason_prop;
      }
  | EHookIncompatible { use_op; lower; upper; lower_is_hook; hook_is_annot } ->
    EHookIncompatible
      {
        use_op = map_use_op use_op;
        lower = map_reason lower;
        upper = map_reason upper;
        lower_is_hook;
        hook_is_annot;
      }
  | EHookUniqueIncompatible { use_op; lower; upper } ->
    EHookUniqueIncompatible
      { use_op = map_use_op use_op; lower = map_reason lower; upper = map_reason upper }
  | EIncompatibleReactDeepReadOnly { use_op; lower; upper; dro_loc } ->
    EIncompatibleReactDeepReadOnly
      {
        use_op = map_use_op use_op;
        lower = map_reason lower;
        upper = map_reason upper;
        dro_loc = f dro_loc;
      }
  | EHookRuleViolation { callee_loc; call_loc; hook_rule } ->
    let hook_rule =
      match hook_rule with
      | MaybeHook { hooks; non_hooks } ->
        MaybeHook { hooks = List.map f hooks; non_hooks = List.map f non_hooks }
      | HookHasIllegalName -> HookHasIllegalName
      | NotHookSyntaxHook -> NotHookSyntaxHook
      | HookDefinitelyNotInComponentOrHook -> HookDefinitelyNotInComponentOrHook
      | HookInUnknownContext -> HookInUnknownContext
      | HookNotInComponentSyntaxComponentOrHookSyntaxHook ->
        HookNotInComponentSyntaxComponentOrHookSyntaxHook
      | ConditionalHook -> ConditionalHook
    in
    EHookRuleViolation { callee_loc = f callee_loc; call_loc = f call_loc; hook_rule }
  | EHookNaming l -> EHookNaming (f l)
  | EObjectThisSuperReference (loc, r, k) -> EObjectThisSuperReference (f loc, map_reason r, k)
  | EComponentThisReference { component_loc; this_loc } ->
    EComponentThisReference { component_loc = f component_loc; this_loc = f this_loc }
  | EComponentCase loc -> EComponentCase (f loc)
  | EComponentMissingReturn r -> EComponentMissingReturn (map_reason r)
  | ENestedComponent r -> ENestedComponent (map_reason r)
  | EInvalidDeclaration { declaration; null_write; possible_generic_escape_locs } ->
    EInvalidDeclaration
      {
        declaration = map_reason declaration;
        null_write =
          Base.Option.map
            ~f:(fun ({ null_loc; _ } as nw) -> { nw with null_loc = f null_loc })
            null_write;
        possible_generic_escape_locs = List.map f possible_generic_escape_locs;
      }
  | EInvalidGraphQL (loc, err) -> EInvalidGraphQL (f loc, err)
  | EAnnotationInference (loc, r1, r2, suggestion) ->
    EAnnotationInference (f loc, map_reason r1, map_reason r2, suggestion)
  | ETrivialRecursiveDefinition (loc, r) -> ETrivialRecursiveDefinition (f loc, map_reason r)
  | EDefinitionCycle elts ->
    let open Env_api in
    EDefinitionCycle
      (Nel.map
         (fun (reason, recur, annot) ->
           ( map_reason reason,
             Base.List.map ~f recur,
             Base.List.map
               ~f:(function
                 | Loc l -> Loc (f l)
                 | Object { loc; props } -> Object { loc = f loc; props = Base.List.map ~f props })
               annot
           ))
         elts
      )
  | ERecursiveDefinition { reason; recursion; annot_locs } ->
    let open Env_api in
    ERecursiveDefinition
      {
        reason = map_reason reason;
        annot_locs =
          Base.List.map
            ~f:(function
              | Loc l -> Loc (f l)
              | Object { loc; props } -> Object { loc = f loc; props = Base.List.map ~f props })
            annot_locs;
        recursion = Base.List.map ~f recursion;
      }
  | EReferenceInAnnotation (bind_loc, name, loc) -> EReferenceInAnnotation (f bind_loc, name, f loc)
  | EDuplicateClassMember { loc; name; static } ->
    EDuplicateClassMember { loc = f loc; name; static }
  | EEmptyArrayNoProvider { loc } -> EEmptyArrayNoProvider { loc = f loc }
  | EUnusedPromise { loc; async } -> EUnusedPromise { loc = f loc; async }
  | EReactIntrinsicOverlap { use; def; type_; mixed } ->
    EReactIntrinsicOverlap { def = f def; use = map_reason use; type_ = f type_; mixed }
  | EInvalidComponentRestParam loc -> EInvalidComponentRestParam (f loc)
  | EBigIntRShift3 r -> EBigIntRShift3 (map_reason r)
  | EBigIntNumCoerce r -> EBigIntNumCoerce (map_reason r)
  | EInvalidCatchParameterAnnotation loc -> EInvalidCatchParameterAnnotation (f loc)
  | ETSSyntax { kind; loc } -> ETSSyntax { kind; loc = f loc }
  | EInvalidBinaryArith { reason_out; reason_l; reason_r; kind } ->
    EInvalidBinaryArith
      {
        reason_out = map_reason reason_out;
        reason_l = map_reason reason_l;
        reason_r = map_reason reason_r;
        kind;
      }
  | EInvalidMappedType { loc; kind } -> EInvalidMappedType { loc = f loc; kind }
  | EDuplicateComponentProp { spread; duplicates } ->
    EDuplicateComponentProp
      {
        spread = f spread;
        duplicates = Nel.map (fun (first, name, second) -> (f first, name, f second)) duplicates;
      }
  | ERefComponentProp { spread; loc } -> ERefComponentProp { spread = f spread; loc = f loc }
  | EKeySpreadProp { spread; loc } -> EKeySpreadProp { spread = f spread; loc = f loc }
  | EInvalidRendersTypeArgument
      { loc; renders_variant; invalid_render_type_kind; invalid_type_reasons } ->
    EInvalidRendersTypeArgument
      {
        loc = f loc;
        renders_variant;
        invalid_render_type_kind =
          map_loc_of_invalid_render_type_kind map_reason invalid_render_type_kind;
        invalid_type_reasons = Nel.map map_reason invalid_type_reasons;
      }
  | EInvalidTypeCastSyntax { loc; enabled_casting_syntax } ->
    EInvalidTypeCastSyntax { loc = f loc; enabled_casting_syntax }
  | EMissingPlatformSupport { loc; available_platforms; required_platforms } ->
    EMissingPlatformSupport { loc = f loc; available_platforms; required_platforms }
  | EUnionPartialOptimizationNonUniqueKey { loc; non_unique_keys } ->
    EUnionPartialOptimizationNonUniqueKey
      {
        loc = f loc;
        non_unique_keys =
          NameUtils.Map.map (Type.UnionRep.UnionEnumMap.map (Nel.map map_reason)) non_unique_keys;
      }
  | EUnionOptimization { loc; kind } ->
    let kind =
      let open UnionRep in
      match kind with
      | ContainsUnresolved r -> ContainsUnresolved (map_reason r)
      | NoCandidateMembers -> NoCandidateMembers
      | NoCommonKeys -> NoCommonKeys
    in
    EUnionOptimization { loc = f loc; kind }
  | EUnionOptimizationOnNonUnion { loc; arg } ->
    EUnionOptimizationOnNonUnion { loc = f loc; arg = map_reason arg }
  | ECannotCallReactComponent { reason } -> ECannotCallReactComponent { reason = map_reason reason }
  | EDevOnlyRefinedLocInfo { refined_loc; refining_locs } ->
    EDevOnlyRefinedLocInfo { refined_loc = f refined_loc; refining_locs = List.map f refining_locs }
  | EMatchNotExhaustive { loc; reason } ->
    EMatchNotExhaustive { loc = f loc; reason = map_reason reason }
  | EMatchInvalidBindingKind { loc; kind } -> EMatchInvalidBindingKind { loc = f loc; kind }
  | EMatchInvalidObjectPropertyLiteral { loc } -> EMatchInvalidObjectPropertyLiteral { loc = f loc }
  | EMatchInvalidUnaryZero { loc } -> EMatchInvalidUnaryZero { loc = f loc }
  | EMatchInvalidUnaryPlusBigInt { loc } -> EMatchInvalidUnaryPlusBigInt { loc = f loc }
  | EMatchDuplicateObjectProperty { loc; name } ->
    EMatchDuplicateObjectProperty { loc = f loc; name }
  | EMatchBindingInOrPattern { loc } -> EMatchBindingInOrPattern { loc = f loc }
  | EMatchInvalidAsPattern { loc } -> EMatchInvalidAsPattern { loc = f loc }
  | EMatchInvalidPatternReference { loc; binding_reason } ->
    EMatchInvalidPatternReference { loc = f loc; binding_reason = map_reason binding_reason }
  | EMatchInvalidObjectShorthand { loc; name } -> EMatchInvalidObjectShorthand { loc = f loc; name }
  | EMatchStatementInvalidBody { loc } -> EMatchStatementInvalidBody { loc = f loc }
  | EUndocumentedFeature { loc } -> EUndocumentedFeature { loc = f loc }
  | EIllegalAssertOperator { op; obj; specialized } ->
    EIllegalAssertOperator { op = map_reason op; obj = map_reason obj; specialized }
  | EDevOnlyInvalidatedRefinementInfo { read_loc; invalidation_info } ->
    EDevOnlyInvalidatedRefinementInfo
      {
        read_loc = f read_loc;
        invalidation_info = List.map (fun (l, r) -> (f l, r)) invalidation_info;
      }
  | ETemporaryHardcodedErrorForPrototyping (r, s) ->
    ETemporaryHardcodedErrorForPrototyping (map_reason r, s)

let desc_of_reason r = Reason.desc_of_reason ~unwrap:(is_scalar_reason r) r

(* A utility function for getting and updating the use_op in error messages. *)
let util_use_op_of_msg nope util = function
  | EIncompatible { use_op; lower; upper } ->
    Base.Option.value_map use_op ~default:nope ~f:(fun use_op ->
        util use_op (fun use_op -> EIncompatible { use_op = Some use_op; lower; upper })
    )
  | EIncompatibleSpeculation { use_op; loc; branches } ->
    Base.Option.value_map use_op ~default:nope ~f:(fun use_op ->
        util use_op (fun use_op -> EIncompatibleSpeculation { use_op = Some use_op; loc; branches })
    )
  | EIncompatibleDefs { use_op; reason_lower; reason_upper; branches } ->
    util use_op (fun use_op -> EIncompatibleDefs { use_op; reason_lower; reason_upper; branches })
  | EIncompatibleProp { use_op; prop; reason_prop; reason_obj; special } ->
    Base.Option.value_map use_op ~default:nope ~f:(fun use_op ->
        util use_op (fun use_op ->
            EIncompatibleProp { use_op = Some use_op; prop; reason_prop; reason_obj; special }
        )
    )
  | EExpectedStringLit { reason_lower; reason_upper; use_op } ->
    util use_op (fun use_op -> EExpectedStringLit { reason_lower; reason_upper; use_op })
  | EExpectedNumberLit { reason_lower; reason_upper; use_op } ->
    util use_op (fun use_op -> EExpectedNumberLit { reason_lower; reason_upper; use_op })
  | EExpectedBooleanLit { reason_lower; reason_upper; use_op } ->
    util use_op (fun use_op -> EExpectedBooleanLit { reason_lower; reason_upper; use_op })
  | EExpectedBigIntLit { reason_lower; reason_upper; use_op } ->
    util use_op (fun use_op -> EExpectedBigIntLit { reason_lower; reason_upper; use_op })
  | EPropNotFound { prop_name = prop; reason_prop; reason_obj; use_op; suggestion } ->
    util use_op (fun use_op ->
        EPropNotFound { prop_name = prop; reason_prop; reason_obj; use_op; suggestion }
    )
  | EIndexerCheckFailed { prop_name; reason_prop; reason_obj; reason_indexer; use_op } ->
    util use_op (fun use_op ->
        EIndexerCheckFailed { prop_name; reason_prop; reason_obj; reason_indexer; use_op }
    )
  | EPropNotReadable { reason_prop; prop_name; use_op } ->
    util use_op (fun use_op -> EPropNotReadable { reason_prop; prop_name; use_op })
  | EPropNotWritable { reason_prop; prop_name; use_op } ->
    util use_op (fun use_op -> EPropNotWritable { reason_prop; prop_name; use_op })
  | EPropPolarityMismatch (rs, p, ps, op) ->
    util op (fun op -> EPropPolarityMismatch (rs, p, ps, op))
  | EPrivateLookupFailed (rs, x, op) -> util op (fun op -> EPrivateLookupFailed (rs, x, op))
  | ETupleArityMismatch
      {
        use_op;
        lower_reason;
        lower_arity;
        lower_inexact;
        upper_reason;
        upper_arity;
        upper_inexact;
        unify;
      } ->
    util use_op (fun use_op ->
        ETupleArityMismatch
          {
            use_op;
            lower_reason;
            lower_arity;
            lower_inexact;
            upper_reason;
            upper_arity;
            upper_inexact;
            unify;
          }
    )
  | ENonLitArrayToTuple (rs, op) -> util op (fun op -> ENonLitArrayToTuple (rs, op))
  | ETupleOutOfBounds { use_op; reason; reason_op; inexact; length; index } ->
    util use_op (fun use_op ->
        ETupleOutOfBounds { use_op; reason; reason_op; inexact; length; index }
    )
  | ETupleNonIntegerIndex { use_op; reason; index } ->
    util use_op (fun use_op -> ETupleNonIntegerIndex { use_op; reason; index })
  | ETupleUnsafeWrite { reason; use_op } ->
    util use_op (fun use_op -> ETupleUnsafeWrite { reason; use_op })
  | ETupleElementNotReadable { reason; index; name; use_op } ->
    util use_op (fun use_op -> ETupleElementNotReadable { reason; index; name; use_op })
  | ETupleElementNotWritable { reason; index; name; use_op } ->
    util use_op (fun use_op -> ETupleElementNotWritable { reason; index; name; use_op })
  | ETupleElementPolarityMismatch
      { index; reason_lower; polarity_lower; reason_upper; polarity_upper; use_op } ->
    util use_op (fun use_op ->
        ETupleElementPolarityMismatch
          { index; reason_lower; polarity_lower; reason_upper; polarity_upper; use_op }
    )
  | EROArrayWrite (rs, op) -> util op (fun op -> EROArrayWrite (rs, op))
  | EUnionSpeculationFailed { use_op; reason; op_reasons; branches } ->
    util use_op (fun use_op -> EUnionSpeculationFailed { use_op; reason; op_reasons; branches })
  | EIncompatibleWithExact (rs, op, kind) ->
    util op (fun op -> EIncompatibleWithExact (rs, op, kind))
  | EFunctionIncompatibleWithIndexer (rs, op) ->
    util op (fun op -> EFunctionIncompatibleWithIndexer (rs, op))
  | EInvalidObjectKit { reason; reason_op; use_op } ->
    util use_op (fun use_op -> EInvalidObjectKit { reason; reason_op; use_op })
  | EIncompatibleWithUseOp ({ use_op; _ } as contents) ->
    util use_op (fun use_op -> EIncompatibleWithUseOp { contents with use_op })
  | EEnumIncompatible ({ use_op; _ } as contents) ->
    util use_op (fun use_op -> EEnumIncompatible { contents with use_op })
  | ENotAReactComponent { reason; use_op } ->
    util use_op (fun use_op -> ENotAReactComponent { reason; use_op })
  | EFunctionCallExtraArg (rl, ru, n, op) ->
    util op (fun op -> EFunctionCallExtraArg (rl, ru, n, op))
  | EPrimitiveAsInterface { use_op; reason; interface_reason; kind } ->
    util use_op (fun use_op -> EPrimitiveAsInterface { use_op; reason; interface_reason; kind })
  | ECannotSpreadInterface { spread_reason; interface_reason; use_op } ->
    util use_op (fun use_op -> ECannotSpreadInterface { spread_reason; interface_reason; use_op })
  | ECannotSpreadIndexerOnRight { spread_reason; object_reason; key_reason; use_op } ->
    util use_op (fun use_op ->
        ECannotSpreadIndexerOnRight { spread_reason; object_reason; key_reason; use_op }
    )
  | EUnableToSpread { spread_reason; object1_reason; object2_reason; propname; error_kind; use_op }
    ->
    util use_op (fun use_op ->
        EUnableToSpread
          { spread_reason; object1_reason; object2_reason; propname; error_kind; use_op }
    )
  | EInexactMayOverwriteIndexer { spread_reason; key_reason; value_reason; object2_reason; use_op }
    ->
    util use_op (fun use_op ->
        EInexactMayOverwriteIndexer
          { spread_reason; key_reason; value_reason; object2_reason; use_op }
    )
  | EImplicitInstantiationUnderconstrainedError { reason_call; reason_tparam; bound; use_op } ->
    util use_op (fun use_op ->
        EImplicitInstantiationUnderconstrainedError { reason_call; reason_tparam; bound; use_op }
    )
  | EIncompatibleReactDeepReadOnly { lower; upper; use_op; dro_loc } ->
    util use_op (fun use_op -> EIncompatibleReactDeepReadOnly { lower; upper; use_op; dro_loc })
  | EDevOnlyRefinedLocInfo { refined_loc = _; refining_locs = _ }
  | EDevOnlyInvalidatedRefinementInfo { read_loc = _; invalidation_info = _ }
  | ETemporaryHardcodedErrorForPrototyping (_, _)
  | EExportValueAsType (_, _)
  | EImportValueAsType (_, _)
  | EImportTypeAsTypeof (_, _)
  | EImportTypeAsValue (_, _)
  | ENoDefaultExport (_, _, _)
  | EOnlyDefaultExport (_, _, _)
  | ENoNamedExport (_, _, _, _)
  | EMissingTypeArgs { reason_op = _; reason_tapp = _; arity_loc = _; min_arity = _; max_arity = _ }
  | EAnyValueUsedAsType _
  | EValueUsedAsType _
  | EPolarityMismatch { reason = _; name = _; expected_polarity = _; actual_polarity = _ }
  | EBuiltinNameLookupFailed _
  | EBuiltinModuleLookupFailed _
  | EExpectedModuleLookupFailed _
  | EPlatformSpecificImplementationModuleLookupFailed _
  | EComparison (_, _)
  | ENonStrictEqualityComparison _
  | EUnsupportedExact (_, _)
  | EUnexpectedThisType _
  | ETypeParamArity (_, _)
  | ECallTypeArity _
  | ETypeParamMinArity (_, _)
  | ETooFewTypeArgs _
  | ETooManyTypeArgs _
  | EInvalidTypeArgs (_, _)
  | EInvalidInfer _
  | EInvalidExtends _
  | EInvalidReactCreateElement _
  | EStrUtilTypeNonLiteralArg _
  | EExportsAnnot _
  | EUnsupportedKeyInObject _
  | EAmbiguousNumericKeyWithVariance _
  | ETypeGuardFuncIncompatibility _
  | ETypeGuardInvalidParameter _
  | ETypeGuardIndexMismatch _
  | ETypeGuardImpliesMismatch _
  | EInternal (_, _)
  | EUnsupportedSyntax (_, _)
  | EUseArrayLiteral _
  | EMissingLocalAnnotation _
  | EBindingError (_, _, _, _)
  | ERecursionLimit (_, _)
  | EUninitializedInstanceProperty _
  | EEnumsNotEnabled _
  | EIndeterminateModuleType _
  | EBadExportPosition _
  | EBadExportContext _
  | EBadDefaultImportAccess _
  | EBadDefaultImportDestructuring _
  | EInvalidImportStarUse _
  | ENonConstVarExport _
  | EThisInExportedFunction _
  | EMixedImportAndRequire _
  | EUnsupportedVarianceAnnotation _
  | EExportRenamedDefault _
  | EUnreachable _
  | EInvalidTypeof (_, _)
  | EBinaryInLHS _
  | EBinaryInRHS _
  | EArithmeticOperand _
  | EForInRHS _
  | EInstanceofRHS _
  | EObjectComputedPropertyAccess _
  | EObjectComputedPropertyAssign _
  | EObjectComputedPropertyPotentialOverwrite _
  | EInvalidConstructor _
  | EInvalidLHSInAssignment _
  | EUnsupportedImplements _
  | EReactElementFunArity (_, _, _)
  | EReactRefInRender _
  | EUnsupportedSetProto _
  | EDuplicateModuleProvider { module_name = _; provider = _; conflict = _ }
  | EParseError (_, _)
  | EDocblockError (_, _)
  | EImplicitInexactObject _
  | EAmbiguousObjectType _
  | EUntypedTypeImport (_, _)
  | EUntypedImport (_, _)
  | ENonstrictImport _
  | EUnclearType _
  | EDeprecatedBool _
  | EInternalType _
  | EIncorrectTypeWithReplacement _
  | EUnsafeGettersSetters _
  | EUnsafeObjectAssign _
  | EUnusedSuppression _
  | ECodelessSuppression _
  | ELintSetting _
  | ESketchyNullLint { kind = _; loc = _; null_loc = _; falsy_loc = _ }
  | ESketchyNumberLint _
  | EInvalidPrototype _
  | EUnnecessaryOptionalChain _
  | EUnnecessaryInvariant _
  | EUnnecessaryDeclareTypeOnlyExport _
  | EUnexpectedTemporaryBaseType _
  | ECannotDelete _
  | ESignatureBindingValidation _
  | ESignatureVerification _
  | EExponentialSpread _
  | EComputedPropertyWithUnion _
  | EEnumInvalidMemberAccess _
  | EEnumModification _
  | EEnumMemberDuplicateValue _
  | EEnumInvalidObjectUtilType _
  | EEnumInvalidObjectFunction _
  | EEnumNotIterable _
  | EEnumMemberAlreadyChecked _
  | EEnumAllMembersAlreadyChecked _
  | EEnumNotAllChecked _
  | EEnumUnknownNotChecked _
  | EEnumInvalidAbstractUse _
  | EEnumInvalidCheck _
  | EEnumMemberUsedAsType _
  | EAssignConstLikeBinding _
  | EMalformedCode _
  | EClassToObject _
  | EMethodUnbinding _
  | EHookIncompatible _
  | EHookUniqueIncompatible _
  | EHookRuleViolation _
  | EHookNaming _
  | EObjectThisSuperReference _
  | EComponentThisReference _
  | EComponentCase _
  | EComponentMissingReturn _
  | ENestedComponent _
  | EInvalidDeclaration _
  | EInvalidGraphQL _
  | EDefinitionCycle _
  | ERecursiveDefinition _
  | EReferenceInAnnotation _
  | EAnnotationInference _
  | ETrivialRecursiveDefinition _
  | EDuplicateClassMember _
  | EEmptyArrayNoProvider _
  | EUnusedPromise _
  | EReactIntrinsicOverlap _
  | EInvalidComponentRestParam _
  | EBigIntRShift3 _
  | EBigIntNumCoerce _
  | EInvalidCatchParameterAnnotation _
  | ETSSyntax _
  | EInvalidBinaryArith _
  | EInvalidMappedType _
  | ETupleRequiredAfterOptional _
  | ETupleInvalidTypeSpread _
  | ETupleElementAfterInexactSpread _
  | ETypeGuardParamUnbound _
  | ETypeGuardThisParam _
  | ETypeGuardFunctionParamHavoced _
  | ETypeGuardIncompatibleWithFunctionKind _
  | ETypeGuardFunctionInvalidWrites _
  | ENegativeTypeGuardConsistency _
  | ETypeParamConstIncompatibility _
  | ETypeParamConstInvalidPosition _
  | EDuplicateComponentProp _
  | ERefComponentProp _
  | EKeySpreadProp _
  | EInvalidRendersTypeArgument _
  | EInvalidTypeCastSyntax _
  | EMissingPlatformSupport _
  | EUnionPartialOptimizationNonUniqueKey _
  | EUnionOptimization _
  | EUnionOptimizationOnNonUnion _
  | ECannotCallReactComponent _
  | EMatchNotExhaustive _
  | EMatchInvalidBindingKind _
  | EMatchInvalidObjectPropertyLiteral _
  | EMatchInvalidUnaryZero _
  | EMatchInvalidUnaryPlusBigInt _
  | EMatchDuplicateObjectProperty _
  | EMatchBindingInOrPattern _
  | EMatchInvalidAsPattern _
  | EMatchInvalidPatternReference _
  | EMatchInvalidObjectShorthand _
  | EMatchStatementInvalidBody _
  | EUndocumentedFeature _
  | EIllegalAssertOperator _ ->
    nope

(* Not all messages (i.e. those whose locations are based on use_ops) have locations that can be
   determined while locations are abstract. We just return None in this case. *)
let loc_of_msg : 'loc t' -> 'loc option = function
  | EAnyValueUsedAsType { reason_use = primary }
  | EValueUsedAsType { reason_use = primary }
  | EComparison (primary, _)
  | ENonStrictEqualityComparison (primary, _)
  | EInvalidTypeArgs (_, primary)
  | ETooFewTypeArgs { reason_tapp = primary; _ }
  | ETooManyTypeArgs { reason_tapp = primary; _ } ->
    Some (loc_of_reason primary)
  | ESketchyNumberLint (_, reason)
  | EInvalidExtends reason
  | EUnsupportedSetProto reason
  | EReactElementFunArity (reason, _, _)
  | EReactRefInRender { usage = reason; _ }
  | EUnsupportedImplements reason
  | EObjectComputedPropertyAssign (reason, _, _)
  | EObjectComputedPropertyAccess (_, reason, _)
  | EForInRHS reason
  | EBinaryInRHS reason
  | EBinaryInLHS reason
  | EInstanceofRHS reason
  | EArithmeticOperand reason
  | ERecursionLimit (reason, _)
  | EMissingLocalAnnotation { reason; _ }
  | EComponentMissingReturn reason
  | ENestedComponent reason
  | EUnsupportedExact (_, reason)
  | EPolarityMismatch { reason; _ }
  | ENoNamedExport (reason, _, _, _)
  | EOnlyDefaultExport (reason, _, _)
  | ENoDefaultExport (reason, _, _)
  | EImportTypeAsValue (reason, _)
  | EImportTypeAsTypeof (reason, _)
  | EExportValueAsType (reason, _)
  | EImportValueAsType (reason, _)
  | ETemporaryHardcodedErrorForPrototyping (reason, _)
  | EComputedPropertyWithUnion reason
  | ETypeParamConstInvalidPosition reason ->
    Some (loc_of_reason reason)
  | EObjectComputedPropertyPotentialOverwrite { key_loc = loc; overwritten_locs = _ }
  | EEnumAllMembersAlreadyChecked { loc; _ }
  | EEnumMemberAlreadyChecked { case_test_loc = loc; _ }
  | EEnumInvalidCheck { loc; _ } ->
    Some loc
  | EEnumNotAllChecked { reason; _ }
  | EEnumUnknownNotChecked { reason; _ }
  | EEnumInvalidAbstractUse { reason; _ }
  | EEnumMemberUsedAsType { reason; _ }
  | EEnumInvalidMemberAccess { reason; _ }
  | EEnumInvalidObjectUtilType { reason; _ }
  | EEnumInvalidObjectFunction { reason; _ }
  | EEnumNotIterable { reason; _ }
  | ERecursiveDefinition { reason; _ }
  | EDefinitionCycle ((reason, _, _), _)
  | EInvalidConstructor reason
  | EInvalidDeclaration { declaration = reason; _ }
  | EBigIntRShift3 reason
  | EBigIntNumCoerce reason
  | EInvalidBinaryArith { reason_out = reason; _ }
  | ETupleRequiredAfterOptional { reason_tuple = reason; _ }
  | ETupleInvalidTypeSpread { reason_spread = reason; _ }
  | ETupleElementAfterInexactSpread reason
  | ETypeGuardInvalidParameter { type_guard_reason = reason; _ }
  | ETypeGuardParamUnbound reason
  | ETypeGuardThisParam reason
  | ETypeGuardFunctionInvalidWrites { reason; _ }
  | ENegativeTypeGuardConsistency { return_reason = reason; _ }
  | ETypeGuardFunctionParamHavoced { type_guard_reason = reason; _ }
  | EIllegalAssertOperator { op = reason; _ } ->
    Some (loc_of_reason reason)
  | EExponentialSpread
      {
        reason = _;
        reasons_for_operand1 =
          { first_reason = first_reason_group1; second_reason = second_reason_group1 };
        reasons_for_operand2 =
          { first_reason = first_reason_group2; second_reason = second_reason_group2 };
      } ->
    (* Ideally, we have an actual annotated union in here somewhere. This function tries to find
     * it, otherwise our primary location will be around the first reason in the list of reasons
     * for the first spread operand.
     *
     * It's important that we don't position around the location of the spread because that
     * may be a polymorphic type variable.
     *
     * TODO (jmbrown): Maybe we should have two separate errors here-- one for type spread and
     * one for value spread. It's always safe to position the error around the value spread.
     * The same is true for all of the other spread errors.
     *)
    let union_reason =
      match (second_reason_group1, second_reason_group2) with
      | (None, _) -> first_reason_group1
      | (_, None) -> first_reason_group2
      | (Some r, _) -> r
    in
    Some (loc_of_reason union_reason)
  | EInvalidPrototype (loc, _)
  | EUntypedTypeImport (loc, _)
  | EUntypedImport (loc, _)
  | EInvalidInfer loc
  | EInvalidReactCreateElement { create_element_loc = loc; _ }
  | ENonstrictImport loc
  | EUnclearType loc
  | EDeprecatedBool loc
  | EInternalType (loc, _)
  | EIncorrectTypeWithReplacement { loc; _ }
  | EUnsafeGettersSetters loc
  | EUnsafeObjectAssign loc
  | EUnnecessaryOptionalChain (loc, _)
  | EUnnecessaryInvariant (loc, _)
  | EUnnecessaryDeclareTypeOnlyExport loc
  | EUnusedSuppression loc
  | ECodelessSuppression (loc, _)
  | EDocblockError (loc, _)
  | EImplicitInexactObject loc
  | EReactIntrinsicOverlap { def = loc; _ }
  | EInvalidComponentRestParam loc
  | EAmbiguousObjectType loc
  | EParseError (loc, _)
  | EInvalidLHSInAssignment loc
  | EInvalidTypeof (loc, _)
  | EUnreachable loc
  | EUnexpectedTemporaryBaseType loc
  | ECannotDelete (loc, _)
  | EBadExportContext (_, loc)
  | EBadExportPosition loc
  | EBadDefaultImportAccess (loc, _)
  | EBadDefaultImportDestructuring loc
  | EInvalidImportStarUse (loc, _)
  | ENonConstVarExport (loc, _)
  | EThisInExportedFunction loc
  | EMixedImportAndRequire (loc, _)
  | EUnsupportedVarianceAnnotation (loc, _)
  | EExportRenamedDefault { loc; _ }
  | EIndeterminateModuleType loc
  | EEnumsNotEnabled loc
  | EUninitializedInstanceProperty (loc, _)
  | EUseArrayLiteral loc
  | EUnsupportedSyntax (loc, _)
  | EInternal (loc, _)
  | EUnsupportedKeyInObject { loc; _ }
  | EAmbiguousNumericKeyWithVariance loc
  | EHookRuleViolation { call_loc = loc; _ }
  | EHookNaming loc
  | EExportsAnnot loc
  | EStrUtilTypeNonLiteralArg loc
  | EUnexpectedThisType loc
  | ETypeParamMinArity (loc, _)
  | EAssignConstLikeBinding { loc; _ }
  | EMalformedCode loc
  | EObjectThisSuperReference (loc, _, _)
  | EComponentThisReference { this_loc = loc; _ }
  | EComponentCase loc
  | EInvalidGraphQL (loc, _)
  | EAnnotationInference (loc, _, _, _)
  | ETrivialRecursiveDefinition (loc, _)
  | EInvalidCatchParameterAnnotation loc
  | EInvalidMappedType { loc; _ }
  | ETSSyntax { loc; _ }
  | EReferenceInAnnotation (loc, _, _)
  | EDuplicateComponentProp { spread = loc; _ }
  | ERefComponentProp { spread = loc; _ }
  | EKeySpreadProp { spread = loc; _ }
  | ETypeGuardIncompatibleWithFunctionKind { loc; _ }
  | EMissingPlatformSupport { loc; _ }
  | EUnionPartialOptimizationNonUniqueKey { loc; _ }
  | EUnionOptimization { loc; _ }
  | EUnionOptimizationOnNonUnion { loc; _ } ->
    Some loc
  | ELintSetting (loc, _) -> Some loc
  | ETypeParamArity (loc, _) -> Some loc
  | ESketchyNullLint { loc; _ } -> Some loc
  | ECallTypeArity { call_loc; _ } -> Some call_loc
  | EMissingTypeArgs { reason_op; _ } -> Some (loc_of_reason reason_op)
  | EInvalidRendersTypeArgument { loc; _ } -> Some loc
  | EInvalidTypeCastSyntax { loc; _ } -> Some loc
  | ESignatureBindingValidation e ->
    Signature_error.(
      (match e with
      | ModuleOverride { override_binding_loc; _ } -> Some override_binding_loc
      | NameOverride { override_binding_loc; _ } -> Some override_binding_loc
      | NamespacedNameAlreadyBound { invalid_binding_loc; _ } -> Some invalid_binding_loc)
    )
  | ESignatureVerification sve ->
    Signature_error.(
      (match sve with
      | ExpectedAnnotation (loc, _)
      | UnexpectedObjectKey (loc, _)
      | UnexpectedArraySpread (loc, _)
      | UnexpectedArrayHole loc
      | EmptyArray loc
      | EmptyObject loc
      | UnexpectedExpression (loc, _) ->
        Some loc)
    )
  | EDuplicateModuleProvider { conflict; _ } -> Some conflict
  | EBindingError (_, loc, _, _) -> Some loc
  | EEnumModification { loc; _ } -> Some loc
  | EEnumMemberDuplicateValue { loc; _ } -> Some loc
  | EBuiltinNameLookupFailed { loc; _ } -> Some loc
  | EBuiltinModuleLookupFailed { loc; _ } -> Some loc
  | EExpectedModuleLookupFailed { loc; _ } -> Some loc
  | ECannotCallReactComponent { reason } -> Some (loc_of_reason reason)
  | EPlatformSpecificImplementationModuleLookupFailed { loc; _ } -> Some loc
  | EDuplicateClassMember { loc; _ } -> Some loc
  | EEmptyArrayNoProvider { loc } -> Some loc
  | EUnusedPromise { loc; _ } -> Some loc
  | EMatchNotExhaustive { loc; _ } -> Some loc
  | EMatchInvalidBindingKind { loc; _ } -> Some loc
  | EMatchInvalidObjectPropertyLiteral { loc } -> Some loc
  | EMatchInvalidUnaryZero { loc } -> Some loc
  | EMatchInvalidUnaryPlusBigInt { loc } -> Some loc
  | EMatchDuplicateObjectProperty { loc; _ } -> Some loc
  | EMatchBindingInOrPattern { loc } -> Some loc
  | EMatchInvalidAsPattern { loc } -> Some loc
  | EMatchInvalidPatternReference { loc; _ } -> Some loc
  | EMatchInvalidObjectShorthand { loc; _ } -> Some loc
  | EMatchStatementInvalidBody { loc } -> Some loc
  | EUndocumentedFeature { loc } -> Some loc
  | EDevOnlyRefinedLocInfo { refined_loc; refining_locs = _ } -> Some refined_loc
  | EDevOnlyInvalidatedRefinementInfo { read_loc; invalidation_info = _ } -> Some read_loc
  | EUnableToSpread _
  | ECannotSpreadInterface _
  | ECannotSpreadIndexerOnRight _
  | EInexactMayOverwriteIndexer _
  | EFunctionCallExtraArg _
  | ENotAReactComponent _
  | EIncompatibleWithUseOp _
  | EEnumIncompatible _
  | EIncompatibleDefs _
  | EInvalidObjectKit _
  | EIncompatibleWithExact _
  | EFunctionIncompatibleWithIndexer _
  | EUnionSpeculationFailed _
  | ETupleUnsafeWrite _
  | EROArrayWrite _
  | ETupleElementNotReadable _
  | ETupleElementNotWritable _
  | ETupleElementPolarityMismatch _
  | ETupleOutOfBounds _
  | ETupleNonIntegerIndex _
  | ENonLitArrayToTuple _
  | ETupleArityMismatch _
  | EPrivateLookupFailed _
  | EPropPolarityMismatch _
  | EPropNotReadable _
  | EPropNotWritable _
  | EPropNotFound _
  | EIndexerCheckFailed _
  | EExpectedBooleanLit _
  | EExpectedNumberLit _
  | EExpectedStringLit _
  | EExpectedBigIntLit _
  | EIncompatibleProp _
  | EIncompatible _
  | EIncompatibleSpeculation _
  | EMethodUnbinding _
  | EHookIncompatible _
  | EIncompatibleReactDeepReadOnly _
  | EHookUniqueIncompatible _
  | EImplicitInstantiationUnderconstrainedError _
  | EClassToObject _
  | EPrimitiveAsInterface _
  | ETypeGuardFuncIncompatibility _
  | ETypeGuardIndexMismatch _
  | ETypeGuardImpliesMismatch _
  | ETypeParamConstIncompatibility _ ->
    None

let kind_of_msg =
  Flow_errors_utils.(
    function
    | EUntypedTypeImport _ -> LintError Lints.UntypedTypeImport
    | EUntypedImport _ -> LintError Lints.UntypedImport
    | ENonstrictImport _ -> LintError Lints.NonstrictImport
    | EInternalType _ -> LintError Lints.InternalType
    | EUnclearType _ -> LintError Lints.UnclearType
    | EDeprecatedBool _ -> LintError Lints.(DeprecatedType DeprecatedBool)
    | EUnsafeGettersSetters _ -> LintError Lints.UnsafeGettersSetters
    | EUnsafeObjectAssign _ -> LintError Lints.UnsafeObjectAssign
    | ESketchyNullLint { kind; _ } -> LintError (Lints.SketchyNull kind)
    | ESketchyNumberLint (kind, _) -> LintError (Lints.SketchyNumber kind)
    | EUnnecessaryOptionalChain _ -> LintError Lints.UnnecessaryOptionalChain
    | EUnnecessaryInvariant _ -> LintError Lints.UnnecessaryInvariant
    | EImplicitInexactObject _ -> LintError Lints.ImplicitInexactObject
    | EAmbiguousObjectType _ -> LintError Lints.AmbiguousObjectType
    | EEnumNotAllChecked { default_case_loc = Some _; _ } ->
      LintError Lints.RequireExplicitEnumSwitchCases
    | EUninitializedInstanceProperty _ -> LintError Lints.UninitializedInstanceProperty
    | EBadDefaultImportAccess _ -> LintError Lints.DefaultImportAccess
    | EBadDefaultImportDestructuring _ -> LintError Lints.DefaultImportAccess
    | EInvalidImportStarUse _ -> LintError Lints.InvalidImportStarUse
    | ENonConstVarExport _ -> LintError Lints.NonConstVarExport
    | EThisInExportedFunction _ -> LintError Lints.ThisInExportedFunction
    | EMixedImportAndRequire _ -> LintError Lints.MixedImportAndRequire
    | EExportRenamedDefault _ -> LintError Lints.ExportRenamedDefault
    | EUnusedPromise _ -> LintError Lints.UnusedPromise
    | EReactIntrinsicOverlap _ -> LintError Lints.ReactIntrinsicOverlap
    | ENestedComponent _ -> LintError Lints.NestedComponent
    | ESignatureBindingValidation (Signature_error.ModuleOverride _ | Signature_error.NameOverride _)
      ->
      LintError Lints.LibdefOverride
    | EBadExportPosition _
    | EBadExportContext _ ->
      InferWarning ExportKind
    | EEnumsNotEnabled _
    | EIndeterminateModuleType _
    | EUnreachable _
    | EInvalidTypeof _ ->
      InferWarning OtherKind
    | EInternal _ -> InternalError
    | ERecursionLimit _ -> RecursionLimitError
    | EDuplicateModuleProvider _ -> DuplicateProviderError
    | EParseError _ -> ParseError
    | EDocblockError _
    | ELintSetting _ ->
      PseudoParseError
    | _ -> InferError
  )

let polarity_explanation = function
  | (Polarity.Positive, _) -> "read-only"
  | (Polarity.Negative, _) -> "write-only"
  | (Polarity.Neutral, Polarity.Negative) -> "readable"
  | (Polarity.Neutral, Polarity.Positive) -> "writable"
  | (Polarity.Neutral, Polarity.Neutral) -> failwith "unreachable"

let mk_prop_message =
  Flow_errors_utils.Friendly.(
    function
    | None -> [text "an index signature declaring the expected key / value type"]
    | Some "$call" -> [text "a call signature declaring the expected parameter / return type"]
    | Some prop -> [text "property "; code prop]
  )

let mk_tuple_element_error_message loc_of_aloc ~reason ~index ~name kind =
  let open Flow_errors_utils.Friendly in
  let index_ref =
    Reference ([Code (string_of_int index)], loc_of_aloc (def_loc_of_reason reason))
  in
  let label =
    Base.Option.value_map name ~default:[] ~f:(fun name -> [text " labeled "; code name])
  in
  [text "tuple element at index "; index_ref] @ label @ [text " is not "; text kind]

let enum_name_of_reason reason =
  match desc_of_reason reason with
  | REnum { name = Some name }
  | RType (OrdinaryName name) ->
    Some name
  | _ -> None

let string_of_internal_error = function
  | ReadOfUnreachedTvar k ->
    spf "read of %s entry which has not been prepared for typechecking" (Env_api.show_def_loc_type k)
  | ReadOfUnresolvedTvar k ->
    spf "read of %s entry from previous component is not FullyResolved" (Env_api.show_def_loc_type k)
  | ForcedReadOfUnderResolutionTvar k ->
    spf
      "forced read of %s entry from component is not yet FullyResolved"
      (Env_api.show_def_loc_type k)
  | MethodNotAFunction -> "expected function type"
  | OptionalMethod -> "optional methods are not supported"
  | PropertyDescriptorPropertyCannotBeRead -> "unexpected property in properties object"
  | ForInLHS -> "unexpected LHS in for...in"
  | ForOfLHS -> "unexpected LHS in for...of"
  | PropRefComputedOpen -> "unexpected open computed property element type"
  | PropRefComputedLiteral -> "unexpected literal computed property element type"
  | RestParameterNotIdentifierPattern -> "unexpected rest parameter, expected an identifier pattern"
  | InterfaceTypeSpread -> "unexpected spread property in interface"
  | DebugThrow -> "debug throw"
  | ParseJobException exc -> "uncaught exception: " ^ Exception.to_string exc
  | CheckTimeout s -> spf "check job timed out after %0.2f seconds" s
  | CheckJobException exc -> "uncaught exception: " ^ Exception.to_string exc
  | UnexpectedAnnotationInference s -> "unexpected " ^ s ^ " in annotation inference"
  | MissingEnvRead l -> "missing env entry for read at " ^ ALoc.debug_to_string l
  | MissingEnvWrite loc -> "expected env entry for write location" ^ ALoc.debug_to_string loc
  | EnvInvariant (Env_api.NameDefOrderingFailure { all; roots; missing_roots }) ->
    let all = Base.List.map ~f:ALoc.debug_to_string all |> String.concat "," in
    let roots = Base.List.map ~f:ALoc.debug_to_string roots |> String.concat "," in
    let missing_roots = Base.List.map ~f:ALoc.debug_to_string missing_roots |> String.concat "," in
    spf
      "Please report this error to the Flow team: Env_api tarjan failure, all: { %s } roots: { %s } missing_roots: { %s }"
      all
      roots
      missing_roots
  | EnvInvariant (Env_api.Impossible str) ->
    "Internal state should be impossible, please report this to the Flow team: " ^ str
  | EnvInvariant (Env_api.ASTStructureOverride str) ->
    "AST visitor issue, please report this to the Flow team: " ^ str
  | EnvInvariant Env_api.NameDefGraphMismatch ->
    "EnvMap.find missed, please report this to the Flow team"
  | EnvInvariant Env_api.(MissingEnvEntry x) ->
    spf "Did not find %s in name_resolver environment, please report this to the Flow team" x
  | ImplicitInstantiationInvariant str ->
    "Implicit instantiation issue, please report this to the Flow team: " ^ str

let type_casting_examples enabled_casting_syntax =
  let example_as = "<expr> as <type>" in
  let example_colon = "(<expr>: <type>)" in
  let open Options.CastingSyntax in
  match enabled_casting_syntax with
  | Colon -> (example_colon, example_as)
  | Both
  | As ->
    (example_as, example_colon)

(* Friendly messages are created differently based on the specific error they come from, so
   we collect the ingredients here and pass them to make_error_printable *)
type 'loc friendly_message_recipe =
  | IncompatibleUse of {
      loc: 'loc;
      upper_kind: 'loc upper_kind;
      reason_lower: 'loc Reason.virtual_reason;
      reason_upper: 'loc Reason.virtual_reason;
      use_op: 'loc Type.virtual_use_op;
    }
  | Speculation of {
      loc: 'loc;
      use_op: 'loc Type.virtual_use_op;
      branches: 'loc t' list;
    }
  | Incompatible of {
      reason_lower: 'loc Reason.virtual_reason;
      reason_upper: 'loc Reason.virtual_reason;
      use_op: 'loc Type.virtual_use_op;
      explanation: 'loc explanation option;
    }
  | IncompatibleEnum of {
      reason_lower: 'loc Reason.virtual_reason;
      reason_upper: 'loc Reason.virtual_reason;
      use_op: 'loc Type.virtual_use_op;
      enum_kind: enum_kind;
      representation_type: string option;
      casting_syntax: Options.CastingSyntax.t;
    }
  | PropMissing of {
      loc: 'loc;
      prop: string option;
      suggestion: string option;
      reason_obj: 'loc Reason.virtual_reason;
      use_op: 'loc Type.virtual_use_op;
      (* Adds a reference to the indexer in the error message *)
      reason_indexer: 'loc Reason.virtual_reason option;
    }
  | Normal of 'loc message
  | UseOp of {
      loc: 'loc;
      message: 'loc message;
      use_op: 'loc Type.virtual_use_op;
      explanation: 'loc explanation option;
    }
  | PropPolarityMismatch of {
      prop: string option;
      reason_lower: 'loc Reason.virtual_reason;
      polarity_lower: Polarity.t;
      reason_upper: 'loc Reason.virtual_reason;
      polarity_upper: Polarity.t;
      use_op: 'loc Type.virtual_use_op;
    }

let friendly_message_of_msg = function
  | EIncompatible { lower = (reason_lower, _); upper = (reason_upper, upper_kind); use_op } ->
    IncompatibleUse
      {
        loc = loc_of_reason reason_upper;
        upper_kind;
        reason_lower;
        reason_upper;
        use_op = Base.Option.value ~default:unknown_use use_op;
      }
  | EIncompatibleSpeculation { loc; use_op; branches } ->
    Speculation { loc; use_op = Base.Option.value ~default:unknown_use use_op; branches }
  | EIncompatibleDefs { use_op; reason_lower; reason_upper; branches } ->
    if branches = [] then
      Incompatible { reason_lower; reason_upper; use_op; explanation = None }
    else
      Speculation { loc = loc_of_reason reason_upper; use_op; branches }
  | EIncompatibleProp { prop; reason_prop; reason_obj; special = _; use_op } ->
    PropMissing
      {
        loc = loc_of_reason reason_prop;
        prop = Base.Option.map ~f:display_string_of_name prop;
        reason_obj;
        suggestion = None;
        use_op = Base.Option.value ~default:unknown_use use_op;
        reason_indexer = None;
      }
  | EDevOnlyRefinedLocInfo { refined_loc = _; refining_locs } ->
    Normal (MessageDevOnlyRefinedLocInfo { refining_locs })
  | EDevOnlyInvalidatedRefinementInfo { read_loc = _; invalidation_info } ->
    Normal (MessageDevOnlyInvalidatedRefinementInfo invalidation_info)
  | ETemporaryHardcodedErrorForPrototyping (_, str) ->
    Normal (MessagePlainTextReservedForInternalErrorOnly str)
  | EExportValueAsType (_, export_name) ->
    Normal (MessageExportValueAsType (display_string_of_name export_name))
  | EImportValueAsType (_, export_name) -> Normal (MessageImportValueAsType export_name)
  | EImportTypeAsTypeof (_, export_name) -> Normal (MessageImportTypeAsTypeof export_name)
  | EImportTypeAsValue (_, export_name) -> Normal (MessageImportTypeAsValue export_name)
  | ENoDefaultExport (_, module_name, suggestion) ->
    Normal
      (MessageNoDefaultExport
         { module_name = Flow_import_specifier.display_userland module_name; suggestion }
      )
  | EOnlyDefaultExport (_, module_name, export_name) ->
    Normal
      (MessageOnlyDefaultExport
         { module_name = Flow_import_specifier.display_userland module_name; export_name }
      )
  | ENoNamedExport (_, module_name, export_name, suggestion) ->
    Normal
      (MessageNoNamedExport
         {
           module_name = Flow_import_specifier.display_userland module_name;
           export_name;
           suggestion;
         }
      )
  | EMissingTypeArgs { reason_op = _; reason_tapp; arity_loc; min_arity; max_arity } ->
    let reason_arity = mk_reason (desc_of_reason reason_tapp) arity_loc in
    Normal (MessageCannotUseTypeWithoutAnyTypeArgs { reason_arity; min_arity; max_arity })
  | ETooManyTypeArgs { reason_tapp; arity_loc; maximum_arity } ->
    let reason_arity = mk_reason (desc_of_reason reason_tapp) arity_loc in
    Normal (MessageCannotUseTypeWithTooManyTypeArgs { reason_arity; n = maximum_arity })
  | ETooFewTypeArgs { reason_tapp; arity_loc; minimum_arity } ->
    let reason_arity = mk_reason (desc_of_reason reason_tapp) arity_loc in
    Normal (MessageCannotUseTypeWithTooFewTypeArgs { reason_arity; n = minimum_arity })
  | EInvalidTypeArgs (reason_main, reason_tapp) ->
    Normal (MessageCannotUseTypeWithInvalidTypeArgs { reason_main; reason_tapp })
  | EInvalidInfer _ -> Normal MessageInvalidInferType
  | EInvalidExtends reason -> Normal (MessageCannotUseAsSuperClass reason)
  | EInvalidReactCreateElement { create_element_loc = _; invalid_react } ->
    Normal (MessageInvalidReactCreateElement invalid_react)
  | ETypeParamArity (_, n) ->
    if n = 0 then
      Normal MessageCannotApplyNonPolymorphicType
    else
      Normal (MessageCannotUseTypeWithoutExactlyNTypeArgs n)
  | ETypeParamMinArity (_, n) -> Normal (MessageCannotUseTypeWithoutAtLeastNTypeArgs n)
  | ECallTypeArity { call_loc = _; is_new; reason_arity; expected_arity } ->
    Normal (MessageCannotUseNonPolymorphicTypeWithTypeArgs { is_new; reason_arity; expected_arity })
  | EAnyValueUsedAsType { reason_use } ->
    Normal (MessageAnyValueUsedAsType (desc_of_reason reason_use))
  | EValueUsedAsType { reason_use } -> Normal (MessageValueUsedAsType (desc_of_reason reason_use))
  | EExpectedStringLit { reason_lower; reason_upper; use_op } ->
    Incompatible { reason_lower; reason_upper; use_op; explanation = None }
  | EExpectedNumberLit { reason_lower; reason_upper; use_op } ->
    Incompatible { reason_lower; reason_upper; use_op; explanation = None }
  | EExpectedBooleanLit { reason_lower; reason_upper; use_op } ->
    Incompatible { reason_lower; reason_upper; use_op; explanation = None }
  | EExpectedBigIntLit { reason_lower; reason_upper; use_op } ->
    Incompatible { reason_lower; reason_upper; use_op; explanation = None }
  | EPropNotFound { prop_name; reason_obj; reason_prop; use_op; suggestion } ->
    PropMissing
      {
        loc = loc_of_reason reason_prop;
        prop = Base.Option.map ~f:display_string_of_name prop_name;
        reason_obj;
        use_op;
        suggestion;
        reason_indexer = None;
      }
  | EIndexerCheckFailed { prop_name; reason_obj; reason_prop; reason_indexer; use_op } ->
    PropMissing
      {
        loc = loc_of_reason reason_prop;
        prop = Some (display_string_of_name prop_name);
        reason_obj;
        use_op;
        suggestion = None;
        reason_indexer = Some reason_indexer;
      }
  | EPropNotReadable { reason_prop; prop_name = x; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason_prop;
        message = MessagePropNotReadable x;
        use_op;
        explanation = None;
      }
  | EPropNotWritable { reason_prop; prop_name = x; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason_prop;
        message = MessagePropNotWritable x;
        use_op;
        explanation = None;
      }
  | EPropPolarityMismatch
      ((reason_lower, reason_upper), prop, (polarity_lower, polarity_upper), use_op) ->
    PropPolarityMismatch
      {
        prop = Base.Option.map ~f:display_string_of_name prop;
        reason_lower;
        polarity_lower;
        reason_upper;
        polarity_upper;
        use_op;
      }
  | EPolarityMismatch { reason; name; expected_polarity; actual_polarity } ->
    let reason_targ = mk_reason (RIdentifier (OrdinaryName name)) (def_loc_of_reason reason) in
    Normal
      (MessageCannotUseTypeDueToPolarityMismatch { reason_targ; expected_polarity; actual_polarity })
  | EBuiltinNameLookupFailed { loc = _; name } -> Normal (MessageCannotResolveBuiltinName name)
  | EBuiltinModuleLookupFailed { loc = _; name; potential_generator } ->
    Normal (MessageCannotResolveBuiltinModule { name; potential_generator })
  | EExpectedModuleLookupFailed { loc = _; name; expected_module_purpose } ->
    Normal (MessageCannotResolveExpectedModule { name; expected_module_purpose })
  | EPrivateLookupFailed (reasons, x, use_op) ->
    PropMissing
      {
        loc = loc_of_reason (fst reasons);
        prop = Some ("#" ^ display_string_of_name x);
        reason_obj = snd reasons;
        use_op;
        suggestion = None;
        reason_indexer = None;
      }
  | EPlatformSpecificImplementationModuleLookupFailed { loc = _; name } ->
    Normal (MessagePlatformSpecificImplementationModuleLookupFailed name)
  | EComparison (lower, upper) -> Normal (MessageCannotCompare { lower; upper })
  | ENonStrictEqualityComparison (lower, upper) ->
    Normal (MessageCannotCompareNonStrict { lower; upper })
  | ETupleArityMismatch
      {
        use_op;
        lower_reason;
        lower_arity;
        lower_inexact;
        upper_reason;
        upper_arity;
        upper_inexact;
        unify;
      } ->
    UseOp
      {
        loc = loc_of_reason lower_reason;
        message =
          MessageIncompatibleTupleArity
            {
              lower_reason;
              lower_arity;
              lower_inexact;
              upper_reason;
              upper_arity;
              upper_inexact;
              unify;
            };
        use_op;
        explanation = None;
      }
  | ETupleRequiredAfterOptional { reason_tuple; reason_required; reason_optional } ->
    Normal
      (MessageInvalidTupleRequiredAfterOptional { reason_tuple; reason_required; reason_optional })
  | ETupleInvalidTypeSpread { reason_arg; reason_spread = _ } ->
    Normal (MessageInvalidTupleTypeSpread reason_arg)
  | ETupleElementAfterInexactSpread _ -> Normal MessageTupleElementAfterInexactSpread
  | ENonLitArrayToTuple ((lower, upper), use_op) ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageIncompatibleNonLiteralArrayToTuple { lower; upper };
        use_op;
        explanation = None;
      }
  | ETupleOutOfBounds { reason; reason_op; inexact; length; index; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageTupleIndexOutOfBound { reason_op; inexact; length; index };
        use_op;
        explanation = None;
      }
  | ETupleNonIntegerIndex { reason; index; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageTupleNonIntegerIndex { index_def_loc = def_loc_of_reason reason; index };
        use_op;
        explanation = None;
      }
  | ETupleUnsafeWrite { reason; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageTupleNonStaticallyKnownIndex;
        use_op;
        explanation = None;
      }
  | ETupleElementNotReadable { reason; index; name; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageTupleElementNotReadable { reason; index; name };
        use_op;
        explanation = None;
      }
  | ETupleElementNotWritable { reason; index; name; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageTupleElementNotWritable { reason; index; name };
        use_op;
        explanation = None;
      }
  | ETupleElementPolarityMismatch
      { index; reason_lower; polarity_lower; reason_upper; polarity_upper; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason_lower;
        message =
          MessageTuplePolarityMismatch
            { index; reason_lower; reason_upper; polarity_lower; polarity_upper };
        use_op;
        explanation = None;
      }
  | EROArrayWrite (reasons, use_op) ->
    let (lower, _) = reasons in
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageReadonlyArraysCannotBeWrittenTo;
        use_op;
        explanation = None;
      }
  | EUnionSpeculationFailed { use_op; reason; op_reasons = _; branches } ->
    Speculation { loc = loc_of_reason reason; use_op; branches }
  | EIncompatibleWithExact ((lower, upper), use_op, kind) ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageIncompatibleWithExact { kind; lower; upper };
        use_op;
        explanation = None;
      }
  | EFunctionIncompatibleWithIndexer ((lower, upper), use_op) ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageIncompatibleWithIndexed { lower; upper };
        use_op;
        explanation = None;
      }
  | EUnsupportedExact (_, lower) -> Normal (MessageCannotCreateExactType lower)
  | EUnexpectedThisType _ -> Normal MessageUnexpectedUseOfThisType
  | EStrUtilTypeNonLiteralArg _ -> Normal MessageCannotUseStrUtilType
  | EExportsAnnot _ -> Normal MessageCannotUseDollarExports
  | EUnsupportedKeyInObject { key_error_kind; obj_kind; _ } ->
    Normal (MessageUnsupportedKeyInObject { key_error_kind; obj_kind })
  | EAmbiguousNumericKeyWithVariance _ -> Normal MessageAmbiguousNumericKeyWithVariance
  | ETypeGuardFuncIncompatibility { use_op; reasons = (lower, upper) } ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageIncompatibleNonTypeGuardToTypeGuard { lower; upper };
        use_op;
        explanation = None;
      }
  | ETypeGuardInvalidParameter { type_guard_reason; binding_reason } ->
    Normal (MessageCannotReferenceTypeGuardParameter { type_guard_reason; binding_reason })
  | ETypeGuardIndexMismatch { use_op; reasons = (lower, upper) } ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageTypeGuardIndexMismatch { lower; upper };
        use_op;
        explanation = None;
      }
  | ETypeGuardImpliesMismatch { use_op; reasons = (lower, upper) } ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageTypeGuardImpliesMismatch { lower; upper };
        use_op;
        explanation = None;
      }
  | ETypeGuardParamUnbound reason -> Normal (MessageInvalidTypeGuardParamUnbound reason)
  | ETypeGuardThisParam reason -> Normal (MessageInvalidTypeGuardThisParam reason)
  | ETypeGuardFunctionInvalidWrites { reason = _; type_guard_reason; write_locs } ->
    Normal (MessageInvalidTypeGuardFunctionWritten { type_guard_reason; write_locs })
  | ENegativeTypeGuardConsistency { reason; return_reason; type_reason } ->
    Normal (MessageNegativeTypeGuardConsistency { reason; return_reason; type_reason })
  | ETypeParamConstIncompatibility { use_op; lower; upper } ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageIncompatiblETypeParamConstIncompatibility { lower; upper };
        use_op;
        explanation = None;
      }
  | ETypeParamConstInvalidPosition reason -> Normal (MessageTypeParamConstInvalidPosition reason)
  | ETypeGuardFunctionParamHavoced { type_guard_reason; param_reason; call_locs } ->
    Normal
      (MessageCannotUseTypeGuardWithFunctionParamHavoced
         { type_guard_desc = desc_of_reason type_guard_reason; param_reason; call_locs }
      )
  | ETypeGuardIncompatibleWithFunctionKind { kind; _ } ->
    Normal (MessageInvalidTypeGuardFunctionKind kind)
  | EInternal (_, internal_error) ->
    let msg = string_of_internal_error internal_error in
    Normal (MessagePlainTextReservedForInternalErrorOnly (spf "Internal error: %s" msg))
  | EUnsupportedSyntax (_, unsupported_syntax) ->
    Normal (MessageUnsupportedSyntax unsupported_syntax)
  | EUseArrayLiteral _ -> Normal MessageShouldUseArrayLiteral
  | EMissingLocalAnnotation { reason; hint_available; from_generic_function } ->
    if hint_available then
      Normal (MessageMissingAnnotationDueToContextualTypingFailure (desc_of_reason reason))
    else if from_generic_function then
      Normal (MessageMissingAnnotationForGenericFunction (desc_of_reason reason))
    else
      Normal (MessageMissingAnnotation (desc_of_reason reason))
  | EBindingError (binding_error, _, x, entry_loc) ->
    let desc =
      match x with
      | InternalName "this" -> RThis
      | InternalName "super" -> RSuper
      | _ -> RIdentifier x
    in
    (* We can call to_loc here because reaching this point requires that everything else
       in the error message is concretized already; making Scopes polymorphic is not a good idea *)
    let x = mk_reason desc (ALoc.to_loc_exn entry_loc) in
    let msg =
      match binding_error with
      | EGlobalAlreadyDeclared -> MessageCannotDeclareAlreadyBoundGlobal x
      | ENameAlreadyBound -> MessageCannotDeclareAlreadyBoundName x
      | EVarRedeclaration -> MessageCannotRedeclareVar x
      | EReferencedBeforeDeclaration -> MessageCannotUseBeforeDeclaration x
      | ETypeInValuePosition { imported = true; type_only_namespace; name } ->
        MessageCannotUseTypeInValuePosition
          { reason = x; type_only_namespace; imported_name = Some name }
      | ETypeInValuePosition { imported = false; type_only_namespace; name = _ } ->
        MessageCannotUseTypeInValuePosition
          { reason = x; type_only_namespace; imported_name = None }
      | EConstReassigned
      | EConstParamReassigned ->
        MessageCannotReassignConstant x
      | EImportReassigned -> MessageCannotReassignImport x
      | EEnumReassigned -> MessageCannotReassignEnum x
    in
    Normal msg
  | ERecursionLimit _ -> Normal MessageRecursionLimitExceeded
  | EUninitializedInstanceProperty (_loc, err) -> Normal (MessageUninitializedInstanceProperty err)
  | EEnumsNotEnabled _ -> Normal MessageEnumsNotEnabled
  | EIndeterminateModuleType _ -> Normal MessageCannotDetermineModuleType
  | EBadExportPosition _ -> Normal MessageNonToplevelExport
  | EBadExportContext (name, _) -> Normal (MessageCannotUseExportInNonLegalToplevelContext name)
  | EBadDefaultImportAccess (_, import_star_reason) ->
    Normal (MessageInvalidImportStarUse import_star_reason)
  | EBadDefaultImportDestructuring _ -> Normal MessageCannotUseDefaultImportWithDestrucuturing
  | EInvalidImportStarUse (_, import_star_reason) ->
    Normal (MessageCannotUseImportStar import_star_reason)
  | ENonConstVarExport (_, decl_reason) -> Normal (MessageNonConstVarExport decl_reason)
  | EThisInExportedFunction _ -> Normal MessageThisInExportedFunction
  | EMixedImportAndRequire (_, import_reason) ->
    Normal (MessageCannotUseMixedImportAndRequire import_reason)
  | EUnsupportedVarianceAnnotation (_, kind) -> Normal (MessageUnsupportedVarianceAnnotation kind)
  | EExportRenamedDefault { loc = _; name; is_reexport } ->
    Normal (MessageCannotExportRenamedDefault { name; is_reexport })
  | EUnexpectedTemporaryBaseType _ -> Normal MessageUnexpectedTemporaryBaseType
  | ECannotDelete (_, expr) -> Normal (MessageCannotDelete expr)
  | ESignatureBindingValidation (Signature_error.ModuleOverride { name; existing_binding_loc; _ })
    ->
    let x = mk_reason (RIdentifier (OrdinaryName name)) existing_binding_loc in
    Normal (MessageBadLibdefModuleOverride x)
  | ESignatureBindingValidation (Signature_error.NameOverride { name; existing_binding_loc; _ }) ->
    let x = mk_reason (RIdentifier (OrdinaryName name)) existing_binding_loc in
    Normal (MessageBadLibdefNameOverride x)
  | ESignatureBindingValidation
      (Signature_error.NamespacedNameAlreadyBound { name; existing_binding_loc; _ }) ->
    let x = mk_reason (RIdentifier (OrdinaryName name)) existing_binding_loc in
    Normal (MessageCannotDeclareAlreadyBoundNameInNamespace x)
  | ESignatureVerification sve -> Normal (MessageCannotBuildTypedInterface sve)
  | EUnreachable _ -> Normal MessageUnreachableCode
  | EInvalidObjectKit { reason; reason_op = _; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageLowerIsNotObject reason;
        explanation = None;
        use_op;
      }
  | EInvalidTypeof (_, typename) -> Normal (MessageInvalidGenericRef typename)
  | EArithmeticOperand reason -> Normal (MessageCannotPerformArithOnNonNumbersOrBigInt reason)
  | EBinaryInLHS reason -> Normal (MessageCannotUseInOperatorDueToBadLHS reason)
  | EBinaryInRHS reason -> Normal (MessageCannotUseInOperatorDueToBadRHS reason)
  | EForInRHS reason -> Normal (MessageCannotIterateWithForIn reason)
  | EInstanceofRHS reason -> Normal (MessageCannotUseInstanceOfOperatorDueToBadRHS reason)
  | EObjectComputedPropertyAccess (_, reason_prop, kind) ->
    Normal (MessageCannotAccessObjectWithComputedProp { reason_prop; kind })
  | EObjectComputedPropertyAssign (reason_prop, Some reason_key, kind) ->
    Normal (MessageCannotAssignToObjectWithComputedPropWithKey { reason_prop; reason_key; kind })
  | EObjectComputedPropertyAssign (reason_prop, None, _) ->
    Normal (MessageCannotAssignToObjectWithComputedProp reason_prop)
  | EObjectComputedPropertyPotentialOverwrite { key_loc; overwritten_locs } ->
    Normal (MessageCannotAddComputedPropertyDueToPotentialOverwrite { key_loc; overwritten_locs })
  | EInvalidLHSInAssignment _ -> Normal MessageCannotAssignToInvalidLHS
  | EIncompatibleWithUseOp { reason_lower; reason_upper; use_op; explanation } ->
    Incompatible { reason_lower; reason_upper; use_op; explanation }
  | EUnsupportedImplements reason ->
    Normal (MessageCannotImplementNonInterface (desc_of_reason reason))
  | ENotAReactComponent { reason; use_op } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageLowerIsNotReactComponent reason;
        use_op;
        explanation = None;
      }
  | EReactElementFunArity (_, fn_name, n) ->
    Normal (MessageCannotCallReactFunctionWithoutAtLeastNArgs { fn_name; n })
  | EReactRefInRender { usage; kind = Argument; in_hook } ->
    Normal (MessageCannotPassReactRefAsArgument { usage; in_hook })
  | EReactRefInRender { usage; kind = Access; in_hook } ->
    Normal (MessageCannotAccessReactRefInRender { usage; in_hook })
  | EFunctionCallExtraArg (unused_reason, def_reason, param_count, use_op) ->
    UseOp
      {
        loc = loc_of_reason unused_reason;
        message = MessageCannotCallFunctionWithExtraArg { def_reason; param_count };
        explanation = None;
        use_op;
      }
  | EUnsupportedSetProto _ -> Normal MessageCannotMutateThisPrototype
  | EDuplicateModuleProvider { module_name; provider; conflict } ->
    Normal (MessageDuplicateModuleProvider { module_name; provider; conflict })
  | EParseError (_, parse_error) -> Normal (MessageParseError parse_error)
  | EDocblockError (_, err) -> Normal (MessageDocblockError err)
  | EImplicitInexactObject _ -> Normal MessageImplicitInexactObject
  | EAmbiguousObjectType _ -> Normal MessageAmbiguousObjectType
  | EUntypedTypeImport (_, module_name) ->
    Normal (MessageUntypedTypeImport (Flow_import_specifier.display_userland module_name))
  | EUntypedImport (_, module_name) ->
    Normal (MessageUntypedImport (Flow_import_specifier.display_userland module_name))
  | ENonstrictImport _ -> Normal MessageNonStrictImport
  | EUnclearType _ -> Normal MessageUnclearType
  | EDeprecatedBool _ -> Normal MessageDeprecatedBool
  | EInternalType (_, kind) -> Normal (MessageInternalType kind)
  | EIncorrectTypeWithReplacement { kind; _ } -> Normal (MessageIncorrectType kind)
  | EUnsafeGettersSetters _ -> Normal MessageUnsafeGetterSetter
  | EUnsafeObjectAssign _ -> Normal MessageUnsafeObjectAssign
  | EUnusedSuppression _ -> Normal MessageUnusedSuppression
  | ECodelessSuppression (_, c) -> Normal (MessageSuppressionMissingCode c)
  | ELintSetting (_, kind) -> Normal (MessageInvalidLintSettings kind)
  | ESketchyNullLint { kind; loc = _; falsy_loc; null_loc } ->
    Normal (MessageSketchyNullCheck { kind; falsy_loc; null_loc })
  | ESketchyNumberLint (_, reason) -> Normal (MessageSketchyNumber reason)
  | EInvalidConstructor reason -> Normal (MessageCannotUseAsConstructor reason)
  | EInvalidPrototype (_, reason) -> Normal (MessageCannotUseAsPrototype reason)
  | EUnnecessaryOptionalChain (_, lhs_reason) -> Normal (MessageUnnecessaryOptionalChain lhs_reason)
  | EUnnecessaryInvariant (_, reason) -> Normal (MessageUnnecessaryInvariant reason)
  | EUnnecessaryDeclareTypeOnlyExport _ -> Normal MessageUnnecessaryDeclareTypeOnlyExport
  | EPrimitiveAsInterface { use_op; reason; interface_reason; kind } ->
    UseOp
      {
        loc = loc_of_reason reason;
        message = MessageCannotUsePrimitiveAsInterface { reason; interface_reason; kind };
        explanation = None;
        use_op;
      }
  | ECannotSpreadInterface { spread_reason; interface_reason; use_op } ->
    UseOp
      {
        loc = loc_of_reason spread_reason;
        message = MessageCannotSpreadInterface { spread_reason; interface_reason };
        explanation = None;
        use_op;
      }
  | ECannotSpreadIndexerOnRight { spread_reason; object_reason; key_reason; use_op } ->
    UseOp
      {
        loc = loc_of_reason spread_reason;
        message =
          MessageCannotSpreadDueToPotentialOverwrite { spread_reason; object_reason; key_reason };
        explanation = None;
        use_op;
      }
  | EUnableToSpread { spread_reason; object1_reason; object2_reason; propname; error_kind; use_op }
    ->
    UseOp
      {
        loc = loc_of_reason spread_reason;
        message =
          MessageCannotSpreadGeneral
            { spread_reason; object1_reason; object2_reason; propname; error_kind };
        explanation = None;
        use_op;
      }
  | EInexactMayOverwriteIndexer { spread_reason; key_reason; value_reason; object2_reason; use_op }
    ->
    UseOp
      {
        loc = loc_of_reason spread_reason;
        message =
          MessageCannotSpreadInexactMayOverwriteIndexer
            { spread_reason; key_reason; value_reason; object2_reason };
        explanation = None;
        use_op;
      }
  | EExponentialSpread { reason; reasons_for_operand1; reasons_for_operand2 } ->
    Normal (MessageExponentialSpread { reason; reasons_for_operand1; reasons_for_operand2 })
  | EComputedPropertyWithUnion reason -> Normal (MessageCannotUseComputedPropertyWithUnion reason)
  | EEnumInvalidMemberAccess { member_name; suggestion; reason; enum_reason } ->
    Normal
      (MessageCannotAccessEnumMember
         { member_name; suggestion; description = desc_of_reason reason; enum_reason }
      )
  | EEnumModification { enum_reason; _ } -> Normal (MessageCannotChangeEnumMember enum_reason)
  | EEnumMemberDuplicateValue { prev_use_loc; enum_reason; _ } ->
    Normal (MessageDuplicateEnumMember { prev_use_loc; enum_reason })
  | EEnumInvalidObjectUtilType { reason; enum_reason } ->
    Normal
      (MessageCannotInstantiateObjectUtilTypeWithEnum
         { description = desc_of_reason reason; enum_reason }
      )
  | EEnumInvalidObjectFunction { reason; enum_reason } ->
    Normal (MessageCannotCallObjectFunctionOnEnum { reason; enum_reason })
  | EEnumNotIterable { reason; for_in } -> Normal (MessageCannotIterateEnum { reason; for_in })
  | EEnumMemberAlreadyChecked { case_test_loc = _; prev_check_loc; enum_reason; member_name } ->
    Normal
      (MessageAlreadyExhaustivelyCheckOneEnumMember { prev_check_loc; enum_reason; member_name })
  | EEnumAllMembersAlreadyChecked { loc = _; enum_reason } ->
    Normal (MessageAlreadyExhaustivelyCheckAllEnumMembers { enum_reason })
  | EEnumNotAllChecked { reason; enum_reason; left_to_check; default_case_loc } ->
    Normal
      (MessageIncompleteExhausiveCheckEnum
         { description = desc_of_reason reason; enum_reason; left_to_check; default_case_loc }
      )
  | EEnumUnknownNotChecked { reason; enum_reason } ->
    Normal
      (MessageCannotExhaustivelyCheckEnumWithUnknowns
         { description = desc_of_reason reason; enum_reason }
      )
  | EEnumInvalidCheck { loc = _; enum_reason; example_member } ->
    Normal (MessageInvalidEnumMemberCheck { enum_reason; example_member })
  | EEnumMemberUsedAsType { reason; enum_reason } ->
    Normal
      (MessageCannotUseEnumMemberUsedAsType { description = desc_of_reason reason; enum_reason })
  | EEnumIncompatible
      { reason_lower; reason_upper; use_op; enum_kind; representation_type; casting_syntax } ->
    IncompatibleEnum
      { reason_lower; reason_upper; use_op; enum_kind; representation_type; casting_syntax }
  | EEnumInvalidAbstractUse { reason; enum_reason } ->
    Normal
      (MessageCannotExhaustivelyCheckAbstractEnums
         { description = desc_of_reason reason; enum_reason }
      )
  | EAssignConstLikeBinding { definition; binding_kind; _ } ->
    Normal (MessageCannotReassignConstantLikeBinding { definition; binding_kind })
  | EMalformedCode _ -> Normal MessageSuppressionMalformedCode
  | EObjectThisSuperReference (_, reason, k) -> Normal (MessageThisSuperInObject (reason, k))
  | EComponentThisReference { component_loc; this_loc = _ } ->
    Normal (MessageThisInComponent component_loc)
  | EComponentCase _ -> Normal MessageComponentNonUpperCase
  | EComponentMissingReturn reason -> Normal (MessageComponentMissingReturn reason)
  | ENestedComponent _ -> Normal MessageCannotNestComponents
  | EDuplicateClassMember { name; static; _ } ->
    Normal (MessageDuplicateClassMember { name; static })
  | EEmptyArrayNoProvider { loc = _ } -> Normal MessageCannotDetermineEmptyArrayLiteralType
  | EInvalidDeclaration
      { declaration = reason; null_write = None; possible_generic_escape_locs = [] } ->
    Normal (MessageVariableNeverInitAssignedAnnotated reason)
  | EInvalidDeclaration { declaration = reason; null_write = None; possible_generic_escape_locs } ->
    Normal
      (MessageShouldAnnotateVariableOnlyInitializedInGenericContext
         { reason; possible_generic_escape_locs }
      )
  | EInvalidDeclaration
      {
        declaration = reason;
        null_write = Some { null_loc; initialized };
        possible_generic_escape_locs = [];
      } ->
    let null_loc =
      if initialized then
        None
      else
        Some null_loc
    in
    Normal (MessageVariableOnlyAssignedByNull { reason; null_loc })
  | EInvalidDeclaration
      {
        declaration = reason;
        null_write = Some { null_loc; initialized };
        possible_generic_escape_locs;
      } ->
    Normal
      (MessageShouldAnnotateVariableUsedInGenericContext
         { reason; null_loc; initialized; possible_generic_escape_locs }
      )
  | EImplicitInstantiationUnderconstrainedError { reason_call; reason_tparam; use_op; bound = _ } ->
    UseOp
      {
        use_op;
        message = MessageUnderconstrainedImplicitInstantiaton { reason_call; reason_tparam };
        loc = loc_of_reason reason_call;
        explanation = None;
      }
  | EClassToObject (reason_class, reason_obj, use_op) ->
    UseOp
      {
        loc = loc_of_reason reason_class;
        message = MessageIncompatibleClassToObject { reason_class; reason_obj };
        explanation = None;
        use_op;
      }
  | EMethodUnbinding { use_op; reason_op; reason_prop } ->
    UseOp
      {
        loc = loc_of_reason reason_op;
        message = MessageMethodUnbinding { reason_op; context_loc = def_loc_of_reason reason_prop };
        use_op;
        explanation = None;
      }
  | EHookIncompatible { use_op; lower; upper; lower_is_hook; hook_is_annot } ->
    UseOp
      {
        loc = loc_of_reason lower;
        message =
          MessageIncompatibleReactHooksWithNonReactHook
            { lower; upper; lower_is_hook; hook_is_annot };
        use_op;
        explanation = Some ExplanationReactHookIncompatibleWithNormalFunctions;
      }
  | EIncompatibleReactDeepReadOnly { use_op; lower; upper; dro_loc } ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageIncompatibleReactDeepReadOnly { lower; upper; dro_loc };
        use_op;
        explanation = Some ExplanationIncompatibleReactDeepReadOnly;
      }
  | EHookUniqueIncompatible { use_op; lower; upper } ->
    UseOp
      {
        loc = loc_of_reason lower;
        message = MessageIncompatibleReactHooksDueToUniqueness { lower; upper };
        use_op;
        explanation = Some ExplanationReactHookIncompatibleWithEachOther;
      }
  | EHookNaming _ -> Normal MessageInvalidHookNaming
  | EHookRuleViolation { callee_loc; hook_rule = ConditionalHook; call_loc = _ } ->
    Normal (MessageCannotCallReactHookConditionally callee_loc)
  | EHookRuleViolation { callee_loc; hook_rule = HookHasIllegalName; call_loc = _ } ->
    Normal (MessageCannotCallReactHookWithIllegalName callee_loc)
  | EHookRuleViolation { callee_loc; hook_rule = MaybeHook { hooks; non_hooks }; call_loc = _ } ->
    Normal (MessageCannotCallMaybeReactHook { callee_loc; hooks; non_hooks })
  | EHookRuleViolation { callee_loc; hook_rule = NotHookSyntaxHook; call_loc = _ } ->
    Normal (MessageCannotCallNonHookSyntaxHook callee_loc)
  | EHookRuleViolation { callee_loc; hook_rule = HookDefinitelyNotInComponentOrHook; call_loc = _ }
    ->
    Normal (MessageCannotCallReactHookInDefinitelyNonComponentOrHook callee_loc)
  | EHookRuleViolation
      { callee_loc; hook_rule = HookNotInComponentSyntaxComponentOrHookSyntaxHook; call_loc = _ } ->
    Normal (MessageCannotCallReactHookInNonComponentSyntaxComponentOrHookSyntaxHook callee_loc)
  | EHookRuleViolation { callee_loc; hook_rule = HookInUnknownContext; call_loc = _ } ->
    Normal (MessageCannotCallReactHookInUnknownContext callee_loc)
  | EInvalidGraphQL (_, err) -> Normal (MessageInvalidGraphQL err)
  | EAnnotationInference (_, reason_op, reason, suggestion) ->
    Normal (MessageCannotUseTypeForAnnotationInference { reason_op; reason; suggestion })
  | ETrivialRecursiveDefinition (_, reason) ->
    Normal (MessageInvalidTrivialRecursiveDefinition (desc_of_reason reason))
  | ERecursiveDefinition { reason; recursion; annot_locs } ->
    Normal
      (MessageDefinitionInvalidRecursive
         { description = desc_of_reason reason; recursion; annot_locs }
      )
  | EDefinitionCycle dependencies -> Normal (MessageDefinitionCycle dependencies)
  | EReferenceInAnnotation (_, name, loc) ->
    Normal (MessageInvalidSelfReferencingTypeAnnotation { name; loc })
  | EUnusedPromise { async = true; _ } -> Normal MessageUnusedPromiseInAsyncScope
  | EUnusedPromise { async = false; _ } -> Normal MessageUnusedPromiseInSyncScope
  | EReactIntrinsicOverlap { use; def; type_; mixed } ->
    Normal (MessageReactIntrinsicOverlap { use; def; type_; mixed })
  | EInvalidComponentRestParam _ -> Normal MessageInvalidComponentRestParam
  | EBigIntRShift3 reason -> Normal (MessageCannotPerformBigIntRShift3 reason)
  | EBigIntNumCoerce reason -> Normal (MessageCannotPerformBigIntUnaryPlus reason)
  | EInvalidCatchParameterAnnotation _ -> Normal MessageInvalidCatchParameterAnnotation
  | ETSSyntax { kind; _ } -> begin
    match kind with
    | TSUnknown -> Normal MessageTSUnknownType
    | TSNever -> Normal MessageTSNeverType
    | TSUndefined -> Normal MessageTSUndefinedType
    | TSKeyof -> Normal MessageTSKeyofType
    | TSTypeParamExtends -> Normal MessageTSParamExtends
    | TSReadonlyVariance -> Normal MessageTSVarianceReadOnly
    | TSInOutVariance `In -> Normal MessageTSVarianceIn
    | TSInOutVariance `Out -> Normal MessageTSVarianceOut
    | TSInOutVariance `InOut -> Normal MessageTSVarianceInOut
    | TSSatisfiesType enabled_casting_syntax ->
      Normal (MessageTSSatisfiesType enabled_casting_syntax)
    | TSReadonlyType (Some arg_kind) ->
      (match arg_kind with
      | `Tuple -> Normal MessageTSReadonlyOperatorOnTuple
      | `Array -> Normal MessageTSReadonlyOperatorOnArray)
    | TSReadonlyType None -> Normal MessageTSReadonlyType
  end
  | EInvalidBinaryArith { reason_out = _; reason_l; reason_r; kind } ->
    Normal (MessageCannotPerformBinaryArith { kind; reason_l; reason_r })
  | EInvalidMappedType { kind; _ } ->
    let msg =
      match kind with
      | InterfaceOrDeclaredClass -> MessageInvalidMappedTypeInInterfaceOrDeclaredClass
      | ExtraProperties -> MessageInvalidMappedTypeWithExtraProps
      | ExplicitExactOrInexact -> MessageInvalidMappedTypeWithExactOrInexact
      | RemoveOptionality -> MessageInvalidMappedTypeWithOptionalityRemoval
      | VarianceOnArrayInput -> MessageInvalidMappedTypeWithVarianceOnArrayInput
    in
    Normal msg
  | EDuplicateComponentProp { spread; duplicates } ->
    Normal (MessageRedeclareComponentProp { duplicates; spread_loc = spread })
  | ERefComponentProp { spread = spread_loc; loc = ref_loc } ->
    Normal (MessageInvalidRefPropertyInSpread { ref_loc; spread_loc })
  | EKeySpreadProp { spread = spread_loc; loc = key_loc } ->
    Normal (MessageInvalidKeyPropertyInSpread { spread_loc; key_loc })
  | EInvalidRendersTypeArgument
      { loc = _; renders_variant; invalid_render_type_kind; invalid_type_reasons } ->
    Normal
      (MessageInvalidRendersTypeArgument
         { renders_variant; invalid_render_type_kind; invalid_type_reasons }
      )
  | EInvalidTypeCastSyntax { enabled_casting_syntax; _ } ->
    Normal (MessageInvalidTypeCastingSyntax enabled_casting_syntax)
  | EMissingPlatformSupport { loc = _; available_platforms; required_platforms } ->
    Normal (MessageMissingPlatformSupport { available_platforms; required_platforms })
  | EUnionPartialOptimizationNonUniqueKey { loc = _; non_unique_keys } ->
    Normal (MessageCannotOptimizeUnionDueToNonUniqueKeys non_unique_keys)
  | EUnionOptimization { loc = _; kind } -> Normal (MessageCannotOptimizeUnionInternally kind)
  | EUnionOptimizationOnNonUnion { loc = _; arg } ->
    Normal (MessageInvalidUseOfFlowEnforceOptimized arg)
  | ECannotCallReactComponent { reason } -> Normal (MessageCannotCallReactComponent reason)
  | EMatchNotExhaustive { loc = _; reason } -> Normal (MessageMatchNotExhaustive reason)
  | EMatchInvalidBindingKind { loc = _; kind } -> Normal (MessageMatchInvalidBindingKind { kind })
  | EMatchInvalidObjectPropertyLiteral { loc = _ } ->
    Normal MessageMatchInvalidObjectPropertyLiteral
  | EMatchInvalidUnaryZero { loc = _ } -> Normal MessageMatchInvalidUnaryZero
  | EMatchInvalidUnaryPlusBigInt { loc = _ } -> Normal MessageMatchInvalidUnaryPlusBigInt
  | EMatchDuplicateObjectProperty { loc = _; name } ->
    Normal (MessageMatchDuplicateObjectProperty { name })
  | EMatchBindingInOrPattern { loc = _ } -> Normal MessageMatchBindingInOrPattern
  | EMatchInvalidAsPattern { loc = _ } -> Normal MessageMatchInvalidAsPattern
  | EMatchInvalidPatternReference { loc = _; binding_reason } ->
    Normal (MessageMatchInvalidPatternReference { binding_reason })
  | EMatchInvalidObjectShorthand { loc = _; name } ->
    Normal (MessageMatchInvalidObjectShorthand { name })
  | EMatchStatementInvalidBody _ -> Normal MessageMatchStatementInvalidBody
  | EUndocumentedFeature { loc = _ } -> Normal MessageUndocumentedFeature
  | EIllegalAssertOperator { obj; specialized; _ } ->
    Normal (MessageIllegalAssertOperator { obj; specialized })

let defered_in_speculation = function
  | EUntypedTypeImport _
  | EMethodUnbinding _
  | EUntypedImport _
  | ENonstrictImport _
  | EUnclearType _
  | EDeprecatedBool _
  | EInternalType _
  | EUnsafeGettersSetters _
  | EUnsafeObjectAssign _
  | ESketchyNullLint _
  | ESketchyNumberLint _
  | EUnnecessaryOptionalChain _
  | EUnnecessaryInvariant _
  | EUnnecessaryDeclareTypeOnlyExport _
  | EImplicitInexactObject _
  | EAmbiguousObjectType _
  | EEnumNotAllChecked { default_case_loc = Some _; _ }
  | EUninitializedInstanceProperty _
  | ETrivialRecursiveDefinition _
  | EAnyValueUsedAsType _
  | EValueUsedAsType _
  | EUnusedPromise _
  | EImplicitInstantiationUnderconstrainedError _
  | EDuplicateComponentProp _
  | ERefComponentProp _
  | EPropNotReadable { use_op = Frame (ReactDeepReadOnly _, _); _ } ->
    true
  | _ -> false

open Error_codes

let react_rule_of_use_op use_op ~default =
  let code_of_frame acc = function
    | ReactDeepReadOnly (_, HookReturn) -> Some ReactRuleHookMutation
    | ReactDeepReadOnly (_, (Props | HookArg | ImmutableAnnot)) -> Some ReactRuleUnsafeMutation
    | _ -> acc
  in
  Base.Option.first_some (fold_use_op (fun _ -> None) code_of_frame use_op) (Some default)

let error_code_of_use_op use_op ~default =
  let code_of_root = function
    | Cast _ -> Some IncompatibleCast
    | ClassExtendsCheck _ -> Some IncompatibleExtend
    | FunCall _
    | FunCallMethod _ ->
      Some IncompatibleCall
    | FunReturnStatement _
    | FunImplicitReturn _ ->
      Some IncompatibleReturn
    | TypeGuardIncompatibility _
    | PositiveTypeGuardConsistency _ ->
      Some IncompatibleTypeGuard
    | _ -> None
  in
  let code_of_frame acc frame =
    match (acc, frame) with
    | (Some _, _) -> acc
    | (_, TypeArgCompatibility _)
    | (_, TypeParamBound _) ->
      Some IncompatibleTypeArg
    | (_, CallFunCompatibility _) -> Some InvalidCallUtil
    | (None, _) -> None
  in
  Base.Option.first_some (fold_use_op code_of_root code_of_frame use_op) (Some default)

let error_code_of_upper_kind = function
  | IncompatibleCallT -> Some NotAFunction
  | IncompatibleObjAssignFromTSpread
  | IncompatibleArrRestT ->
    Some NotAnArray
  | IncompatibleObjAssignFromT
  | IncompatibleObjRestT
  | IncompatibleGetKeysT
  | IncompatibleGetValuesT ->
    Some NotAnObject
  | IncompatibleMixinT
  | IncompatibleThisSpecializeT ->
    Some NotAClass
  | _ -> Some Error_codes.IncompatibleUse

let error_code_of_message err : error_code option =
  match err with
  | EArithmeticOperand _ -> Some UnsafeArith
  | EInvalidBinaryArith { kind = (_, op); _ } -> begin
    match op with
    | Type.ArithKind.Plus -> Some UnsafeAddition
    | _ -> Some UnsafeArith
  end
  | EAssignConstLikeBinding _ -> Some CannotReassignConstLike
  | EBadExportContext _ -> Some InvalidExport
  | EBadExportPosition _ -> Some InvalidExport
  | EBadDefaultImportAccess _ -> Some DefaultImportAccess
  | EBadDefaultImportDestructuring _ -> Some DefaultImportAccess
  | EInvalidImportStarUse _ -> Some InvalidImportStarUse
  | EBinaryInLHS _ -> Some InvalidInLhs
  | EBinaryInRHS _ -> Some InvalidInRhs
  | EBindingError (binding_error, _, _, _) -> begin
    match binding_error with
    | EGlobalAlreadyDeclared -> Some NameAlreadyBound
    | ENameAlreadyBound -> Some NameAlreadyBound
    | EVarRedeclaration -> Some NameAlreadyBound
    | EReferencedBeforeDeclaration -> Some ReferenceBeforeDeclaration
    | ETypeInValuePosition _ -> Some TypeAsValue
    | EConstReassigned
    | EConstParamReassigned ->
      Some ReassignConst
    | EImportReassigned -> Some ReassignImport
    | EEnumReassigned -> Some ReassignEnum
  end
  | EBuiltinNameLookupFailed _ -> Some CannotResolveName
  | EBuiltinModuleLookupFailed _ -> Some CannotResolveModule
  | EExpectedModuleLookupFailed _ -> Some CannotResolveModule
  | EPlatformSpecificImplementationModuleLookupFailed _ -> Some CannotResolveModule
  | ECallTypeArity _ -> Some NonpolymorphicTypeArg
  | ECannotDelete _ -> Some CannotDelete
  | ECannotSpreadIndexerOnRight _ -> Some CannotSpreadIndexer
  | ECannotSpreadInterface _ -> Some CannotSpreadInterface
  | ECodelessSuppression _ -> None
  | ENonStrictEqualityComparison _
  | EComparison _ ->
    Some InvalidCompare
  | EComputedPropertyWithUnion _ -> Some InvalidComputedProp
  | EDevOnlyRefinedLocInfo _ -> None
  | EDevOnlyInvalidatedRefinementInfo _ -> None
  | ETemporaryHardcodedErrorForPrototyping (_, _) -> None
  | EIncorrectTypeWithReplacement { kind; _ } ->
    (match IncorrectType.error_type_of_kind kind with
    | IncorrectType.DeprecatedUtility -> Some DeprecatedUtility
    | IncorrectType.TSType -> Some TSSyntax)
  | EDocblockError (_, err) -> begin
    match err with
    | MultipleFlowAttributes -> Some DuplicateFlowDecl
    | InvalidFlowMode _ -> Some InvalidFlowModeDecl
    | MultipleJSXAttributes -> Some DuplicateJsxDecl
    | InvalidJSXAttribute _ -> Some InvalidJsxDecl
    | MultipleJSXRuntimeAttributes -> Some DuplicateJsxRuntimeDecl
    | InvalidJSXRuntimeAttribute -> Some InvalidJsxRuntimeDecl
    | InvalidSupportsPlatform _ -> Some InvalidSupportsPlatformDecl
    | DisallowedSupportsPlatform -> Some InvalidSupportsPlatformDecl
  end
  | EDuplicateModuleProvider _ -> Some DuplicateModule
  | EEnumAllMembersAlreadyChecked _ -> Some InvalidExhaustiveCheck
  | EEnumInvalidAbstractUse _ -> Some InvalidExhaustiveCheck
  | EEnumInvalidCheck _ -> Some InvalidExhaustiveCheck
  | EEnumInvalidMemberAccess _ -> Some InvalidEnumAccess
  | EEnumInvalidObjectUtilType _ -> Some NotAnObject
  | EEnumInvalidObjectFunction _ -> Some NotAnObject
  | EEnumNotIterable _ -> Some NotIterable
  | EEnumMemberAlreadyChecked _ -> Some InvalidExhaustiveCheck
  | EEnumMemberDuplicateValue _ -> Some DuplicateEnumInit
  | EEnumMemberUsedAsType _ -> Some EnumValueAsType
  | EEnumModification _ -> Some CannotWriteEnum
  | EEnumNotAllChecked { default_case_loc = None; _ } -> Some InvalidExhaustiveCheck
  | EEnumNotAllChecked { default_case_loc = Some _; _ } -> Some RequireExplicitEnumSwitchCases
  | EEnumUnknownNotChecked _ -> Some InvalidExhaustiveCheck
  | EExpectedBooleanLit { use_op; _ } -> error_code_of_use_op use_op ~default:IncompatibleType
  | EExpectedNumberLit { use_op; _ } -> error_code_of_use_op use_op ~default:IncompatibleType
  | EExpectedStringLit { use_op; _ } -> error_code_of_use_op use_op ~default:IncompatibleType
  | EExpectedBigIntLit { use_op; _ } -> error_code_of_use_op use_op ~default:IncompatibleType
  | EEnumsNotEnabled _ -> Some IllegalEnum
  | EExponentialSpread _ -> Some ExponentialSpread
  | EExportsAnnot _ -> Some InvalidExportsTypeArg
  | EExportValueAsType (_, _) -> Some ExportValueAsType
  | EForInRHS _ -> Some InvalidInRhs
  | EInstanceofRHS _ -> Some InvalidInRhs
  | EFunctionCallExtraArg _ -> Some ExtraArg
  | ETypeGuardFuncIncompatibility _
  | ETypeGuardInvalidParameter _
  | ETypeGuardIndexMismatch _
  | ETypeGuardImpliesMismatch _
  | ETypeGuardParamUnbound _
  | ETypeGuardThisParam _
  | ETypeGuardFunctionInvalidWrites _
  | ETypeGuardFunctionParamHavoced _
  | ETypeGuardIncompatibleWithFunctionKind _ ->
    Some FunctionPredicate
  | ENegativeTypeGuardConsistency _ -> Some IncompatibleTypeGuard
  | ETypeParamConstIncompatibility _
  | ETypeParamConstInvalidPosition _ ->
    Some TypeParamConstCode
  | EImportTypeAsTypeof (_, _) -> Some InvalidImportType
  | EImportTypeAsValue (_, _) -> Some ImportTypeAsValue
  | EImportValueAsType (_, _) -> Some ImportValueAsType
  | EIncompatible { upper = (_, upper_kind); _ } -> error_code_of_upper_kind upper_kind
  | EIncompatibleSpeculation { use_op = Some use_op; _ } ->
    error_code_of_use_op use_op ~default:Error_codes.IncompatibleUse
  | EIncompatibleSpeculation _ -> Some Error_codes.IncompatibleUse
  | EIncompatibleDefs { use_op; _ } -> error_code_of_use_op use_op ~default:IncompatibleType
  | EIncompatibleProp { use_op = Some use_op; _ } ->
    error_code_of_use_op use_op ~default:IncompatibleType
  | EIncompatibleProp { use_op = None; _ } -> Some IncompatibleType
  | EIncompatibleWithExact (_, _, UnexpectedInexact) -> Some IncompatibleExact
  | EIncompatibleWithExact (_, _, UnexpectedIndexer) -> Some IncompatibleIndexer
  | EFunctionIncompatibleWithIndexer _ -> Some IncompatibleFunctionIndexer
  | EEnumIncompatible { use_op; _ }
  | EIncompatibleWithUseOp { use_op; _ } ->
    error_code_of_use_op use_op ~default:IncompatibleType
  | EIndeterminateModuleType _ -> Some ModuleTypeConflict
  | EInexactMayOverwriteIndexer _ -> Some CannotSpreadInexact
  (* We don't want these to be suppressible *)
  | EInternal (_, _) -> None
  | EInvalidConstructor _ -> Some InvalidConstructor
  | EInvalidLHSInAssignment _ -> Some InvalidLhs
  | EInvalidObjectKit _ -> Some NotAnObject
  | EInvalidPrototype _ -> Some NotAnObject
  | EInvalidReactCreateElement _ -> Some InvalidReactCreateElement
  | EInvalidTypeArgs (_, _) -> Some InvalidTypeArg
  | EInvalidTypeof _ -> Some IllegalTypeof
  | EInvalidInfer _ -> Some InvalidInfer
  | EInvalidExtends _ -> Some InvalidExtends
  | ELintSetting _ -> Some LintSetting
  | EMissingLocalAnnotation { reason; hint_available = _; from_generic_function = _ } -> begin
    match desc_of_reason reason with
    | RImplicitThis _ -> Some MissingThisAnnot
    | _ -> Some MissingLocalAnnot
  end
  | EMissingTypeArgs _ -> Some MissingTypeArg
  | EMixedImportAndRequire _ -> Some MixedImportAndRequire
  | EUnsupportedVarianceAnnotation _ -> Some UnsupportedVarianceAnnotation
  | ENoDefaultExport (_, _, _) -> Some MissingExport
  | ENoNamedExport (_, _, _, _) -> Some MissingExport
  | ENonConstVarExport _ -> Some NonConstVarExport
  | ENonLitArrayToTuple _ -> Some InvalidTupleArity
  | ENotAReactComponent _ -> Some NotAComponent
  | EObjectComputedPropertyAccess _ -> Some InvalidComputedProp
  | EObjectComputedPropertyAssign _ -> Some InvalidComputedProp
  | EObjectComputedPropertyPotentialOverwrite _ -> Some InvalidComputedProp
  | EOnlyDefaultExport (_, _, _) -> Some MissingExport
  (* We don't want these to be suppressible *)
  | EParseError (_, _) -> None
  | EPolarityMismatch _ -> Some IncompatibleVariance
  | EPrimitiveAsInterface _ -> Some IncompatibleType
  | EPrivateLookupFailed _ -> Some Error_codes.PropMissing
  | EStrUtilTypeNonLiteralArg _ -> Some InvalidTypeArg
  | EPropNotFound { use_op; _ } -> react_rule_of_use_op use_op ~default:Error_codes.PropMissing
  | EIndexerCheckFailed { use_op; _ } ->
    react_rule_of_use_op use_op ~default:Error_codes.IncompatibleType
  | EPropNotReadable { use_op; _ } -> react_rule_of_use_op use_op ~default:CannotRead
  | EPropNotWritable { use_op; _ } -> react_rule_of_use_op use_op ~default:CannotWrite
  | EPropPolarityMismatch _ -> Some IncompatibleVariance
  | EReactElementFunArity (_, _, _) -> Some MissingArg
  | EReactRefInRender _ -> Some ReactRuleRef
  (* We don't want these to be suppressible *)
  | ERecursionLimit (_, _) -> None
  | EROArrayWrite (_, use_op) -> react_rule_of_use_op use_op ~default:CannotWrite
  | ESignatureBindingValidation (Signature_error.ModuleOverride _ | Signature_error.NameOverride _)
    ->
    Some LibdefOverride
  | ESignatureBindingValidation (Signature_error.NamespacedNameAlreadyBound _) ->
    Some SignatureVerificationFailure
  | ESignatureVerification _ -> Some SignatureVerificationFailure
  | EThisInExportedFunction _ -> Some ThisInExportedFunction
  | EExportRenamedDefault _ -> Some ExportRenamedDefault
  | ETooFewTypeArgs _ -> Some MissingTypeArg
  | ETooManyTypeArgs _ -> Some ExtraTypeArg
  | ETupleArityMismatch _ -> Some InvalidTupleArity
  | ETupleRequiredAfterOptional _ -> Some TupleRequiredAfterOptional
  | ETupleInvalidTypeSpread _ -> Some TupleInvalidTypeSpread
  | ETupleElementAfterInexactSpread _ -> Some ElementAfterInexactTupleSpread
  | ETupleElementNotReadable _ -> Some CannotRead
  | ETupleElementNotWritable _ -> Some CannotWrite
  | ETupleElementPolarityMismatch _ -> Some IncompatibleVariance
  | ETupleNonIntegerIndex _ -> Some InvalidTupleIndex
  | ETupleOutOfBounds _ -> Some InvalidTupleIndex
  | ETupleUnsafeWrite _ -> Some InvalidTupleIndex
  | ETypeParamArity (_, _) -> Some NonpolymorphicTypeApp
  | ETypeParamMinArity (_, _) -> Some MissingTypeArg
  | EUnableToSpread { error_kind; _ } -> begin
    match error_kind with
    | UnexpectedInexact -> Some CannotSpreadInexact
    | UnexpectedIndexer -> Some CannotSpreadIndexer
  end
  | EUnexpectedTemporaryBaseType _ -> Some InvalidTempType
  | EUnexpectedThisType _ -> Some IllegalThis
  | EUnionSpeculationFailed { use_op; _ } -> error_code_of_use_op use_op ~default:IncompatibleType
  | EUnreachable _ -> Some UnreachableCode
  | EUnsupportedExact (_, _) -> Some InvalidExact
  | EUnsupportedImplements _ -> Some CannotImplement
  | EUnsupportedKeyInObject _ -> Some IllegalKey
  | EAmbiguousNumericKeyWithVariance _ -> Some IllegalKey
  | EUnsupportedSetProto _ -> Some CannotWrite
  | EUnsupportedSyntax (_, _) -> Some UnsupportedSyntax
  | EImplicitInstantiationUnderconstrainedError _ -> Some UnderconstrainedImplicitInstantiation
  | EObjectThisSuperReference _ -> Some ObjectThisReference
  | EComponentThisReference _ -> Some ComponentThisReference
  | EComponentCase _ -> Some ComponentCase
  | EComponentMissingReturn _ -> Some ComponentMissingReturn
  | EInvalidDeclaration _ -> Some InvalidDeclaration
  | EInvalidMappedType _ -> Some InvalidMappedType
  | EDuplicateComponentProp _ -> Some InvalidComponentProp
  | ERefComponentProp _ -> Some InvalidComponentProp
  | EKeySpreadProp _ -> Some InvalidSpreadProp
  | EInvalidComponentRestParam _ -> Some InvalidComponentProp
  | EMalformedCode _
  | EUnusedSuppression _ ->
    None
  | EUseArrayLiteral _ -> Some IllegalNewArray
  | EAnyValueUsedAsType _
  | EValueUsedAsType _ ->
    Some ValueAsType
  | EClassToObject _ -> Some ClassObject
  | EMethodUnbinding _ -> Some MethodUnbinding
  | EHookIncompatible _
  | EHookUniqueIncompatible _ ->
    Some ReactRuleHookIncompatible
  | EIncompatibleReactDeepReadOnly _ -> Some ReactRuleImmutableIncompatible
  | EHookNaming _ -> Some ReactRuleHookNamingConvention
  | EHookRuleViolation { hook_rule = ConditionalHook; _ } -> Some ReactRuleHookConditional
  | EHookRuleViolation { hook_rule = HookHasIllegalName; _ } -> Some ReactRuleHookNamingConvention
  | EHookRuleViolation { hook_rule = HookDefinitelyNotInComponentOrHook; _ } ->
    Some ReactRuleHookDefinitelyNotInComponentOrHook
  | EHookRuleViolation { hook_rule = MaybeHook _; _ } -> Some ReactRuleHookMixedWithNonHoook
  | EHookRuleViolation { hook_rule = NotHookSyntaxHook; _ } -> Some ReactRuleHookNonHookSyntax
  | EHookRuleViolation
      { hook_rule = HookInUnknownContext | HookNotInComponentSyntaxComponentOrHookSyntaxHook; _ } ->
    Some ReactRuleHook
  | EInvalidGraphQL _ -> Some InvalidGraphQL
  | EAnnotationInference _ -> Some InvalidExportedAnnotation
  | ETrivialRecursiveDefinition _ -> Some RecursiveDefinition
  | EDefinitionCycle _ -> Some DefinitionCycle
  | ERecursiveDefinition _ -> Some RecursiveDefinition
  | EReferenceInAnnotation _ -> Some RecursiveDefinition
  | EDuplicateClassMember _ -> Some DuplicateClassMember
  | EEmptyArrayNoProvider _ -> Some EmptyArrayNoAnnot
  | EBigIntRShift3 _ -> Some BigIntRShift3
  | EBigIntNumCoerce _ -> Some BigIntNumCoerce
  | EInvalidCatchParameterAnnotation _ -> Some InvalidCatchParameterAnnotation
  | EInvalidRendersTypeArgument _ -> Some InvalidRendersTypeArgument
  | EUnnecessaryDeclareTypeOnlyExport _ -> Some UnnecessaryDeclareTypeOnlyExport
  (* lints should match their lint name *)
  | EUntypedTypeImport _
  | EUntypedImport _
  | ENonstrictImport _
  | EInternalType _
  | EUnclearType _
  | EDeprecatedBool _
  | EUnsafeObjectAssign _
  | EUnsafeGettersSetters _
  | ESketchyNullLint _
  | ESketchyNumberLint _
  | EUnnecessaryOptionalChain _
  | EUnnecessaryInvariant _
  | EImplicitInexactObject _
  | EAmbiguousObjectType _
  | EReactIntrinsicOverlap _
  | EUninitializedInstanceProperty _
  | ENestedComponent _
  | EUnusedPromise _ -> begin
    match kind_of_msg err with
    | Flow_errors_utils.LintError kind -> Some (Error_codes.code_of_lint kind)
    | _ -> None
  end
  | ETSSyntax _ -> Some TSSyntax
  | EInvalidTypeCastSyntax _ -> Some InvalidTypeCastSyntax
  | EMissingPlatformSupport _ -> Some MissingPlatformSupport
  | EUnionPartialOptimizationNonUniqueKey _ -> Some UnionPartiallyOptimizableNonUniqueKeys
  | EUnionOptimization _ -> Some UnionUnoptimizable
  | EUnionOptimizationOnNonUnion _ -> Some UnionUnoptimizable
  | ECannotCallReactComponent _ -> Some ReactRuleCallComponent
  | EMatchNotExhaustive _ -> Some MatchNotExhaustive
  | EMatchInvalidBindingKind _ -> Some MatchInvalidPattern
  | EMatchInvalidObjectPropertyLiteral _ -> Some MatchInvalidPattern
  | EMatchInvalidUnaryZero _ -> Some MatchInvalidPattern
  | EMatchInvalidUnaryPlusBigInt _ -> Some MatchInvalidPattern
  | EMatchDuplicateObjectProperty _ -> Some MatchInvalidPattern
  | EMatchBindingInOrPattern _ -> Some MatchInvalidPattern
  | EMatchInvalidAsPattern _ -> Some MatchInvalidPattern
  | EMatchInvalidPatternReference _ -> Some MatchInvalidPattern
  | EMatchInvalidObjectShorthand _ -> Some MatchInvalidPattern
  | EMatchStatementInvalidBody _ -> Some MatchStatementInvalidBody
  | EUndocumentedFeature _ -> Some UndocumentedFeature
  | EIllegalAssertOperator _ -> Some IllegalAssertOperator
