(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

type assigned_const_like_binding_type =
  | ClassNameBinding
  | FunctionNameBinding
  | DeclaredFunctionNameBinding
  | ComponentNameBinding

let string_of_assigned_const_like_binding_type = function
  | ClassNameBinding -> "class"
  | FunctionNameBinding -> "function"
  | DeclaredFunctionNameBinding -> "declared function"
  | ComponentNameBinding -> "component"

type docblock_error =
  | MultipleFlowAttributes
  | InvalidFlowMode of string
  | MultipleJSXAttributes
  | InvalidJSXAttribute of string option
  | MultipleJSXRuntimeAttributes
  | InvalidJSXRuntimeAttribute
  | InvalidSupportsPlatform of string
  | DisallowedSupportsPlatform

type exactness_error_kind =
  | UnexpectedIndexer
  | UnexpectedInexact

type expected_module_purpose =
  | ReactModuleForJSXFragment
  | ReactModuleForReactClassComponent
  | ReactModuleForReactMixedElementType
  | ReactModuleForReactNodeType
  | ReactModuleForReactRefSetterType

type 'loc exponential_spread_reason_group = {
  first_reason: 'loc virtual_reason;
  second_reason: 'loc virtual_reason option;
}

type context_dependent_unsupported_statement =
  | ToplevelLibraryImport
  | NonLibdefToplevelDeclareModule
  | UnsupportedStatementInLibdef of string
  | UnsupportedStatementInDeclareModule of string
  | UnsupportedStatementInDeclareNamespace of string

type internal_type =
  | DollarReactDeepReadOnly
  | DollarUtilityTypeWithNonDollarAliases of string
  | ReactDollarElement
  | ReactDollarUtilityTypesWithNonDollarAliases of string

type unsupported_syntax =
  | AnnotationInsideDestructuring
  | AsConstOnNonLiteral
  | ExistsType
  | MetaPropertyExpression
  | ObjectPropertyGetSet
  | ObjectPropertyComputedGetSet
  | InvariantSpreadArgument
  | ClassPropertyLiteral
  | ClassPropertyComputed
  | ClassStaticBlock
  | RequireDynamicArgument
  | CatchParameterDeclaration
  | DestructuringObjectPropertyInvalidLiteral
  | DestructuringExpressionPattern
  | JSXTypeArgs
  | PredicateFunction
  | PredicateDeclarationAnonymousParameters
  | MatchExpression
  | MatchStatement
  | MultipleIndexers
  | MultipleProtos
  | ExplicitCallAfterProto
  | ExplicitProtoAfterCall
  | SpreadArgument
  | ImportDynamicArgument
  | IllegalName
  | UserDefinedTypeGuards of { kind: Flow_ast.Type.TypeGuard.kind }
  | UnsupportedInternalSlot of {
      name: string;
      static: bool;
    }
  | ContextDependentUnsupportedStatement of context_dependent_unsupported_statement
  | WithStatement
  | ComponentSyntax
  | DeclareGlobal
  | NonnullAssertion

type 'loc invalid_render_type_kind =
  | InvalidRendersNullVoidFalse
  | InvalidRendersIterable
  | InvalidRendersStructural of 'loc virtual_reason
  | InvalidRendersNonNominalElement of 'loc virtual_reason
  | InvalidRendersGenericT
  | UncategorizedInvalidRenders

module IncorrectType = struct
  type t =
    | Partial
    | Shape
    | TSReadonly
    | TSReadonlyArray
    | TSReadonlyMap
    | TSReadonlySet
    | TSNonNullable

  let incorrect_of_kind = function
    | Partial -> "$Partial"
    | Shape -> "$Shape"
    | TSReadonly -> "Readonly"
    | TSReadonlyArray -> "ReadonlyArray"
    | TSReadonlyMap -> "ReadonlyMap"
    | TSReadonlySet -> "ReadonlySet"
    | TSNonNullable -> "NonNullable"

  let replacement_of_kind = function
    | Partial -> "Partial"
    | Shape -> "Partial"
    | TSReadonly -> "$ReadOnly"
    | TSReadonlyArray -> "$ReadOnlyArray"
    | TSReadonlyMap -> "$ReadOnlyMap"
    | TSReadonlySet -> "$ReadOnlySet"
    | TSNonNullable -> "$NonMaybeType"

  type error_type =
    | DeprecatedUtility
    | TSType

  let error_type_of_kind = function
    | Partial
    | Shape ->
      DeprecatedUtility
    | TSReadonly
    | TSReadonlyArray
    | TSReadonlyMap
    | TSReadonlySet
    | TSNonNullable ->
      TSType
end

module InvalidObjKey = struct
  type t =
    | Other
    | NumberNonInt
    | NumberTooLarge
    | NumberTooSmall

  let kind_of_num_value value =
    if not (Float.is_integer value) then
      NumberNonInt
    else if value > Js_number.max_safe_integer then
      NumberTooLarge
    else if value < Js_number.min_safe_integer then
      NumberTooSmall
    else
      Other

  let str_of_kind = function
    | Other -> "other"
    | NumberNonInt -> "number non-int"
    | NumberTooLarge -> "number too large"
    | NumberTooSmall -> "number too small"
end

type 'loc explanation =
  | ExplanationAbstractEnumCasting
  | ExplanationArrayInvariantTyping
  | ExplanationConstrainedAssign of {
      name: string;
      declaration: 'loc;
      providers: 'loc list;
    }
  | ExplanationConcreteEnumCasting of {
      representation_type: string;
      casting_syntax: Options.CastingSyntax.t;
    }
  | ExplanationFunctionsWithStaticsToObject
  | ExplanationMultiplatform
  | ExplanationNonCallableObjectToFunction
  | ExplanationPropertyInvariantTyping
  | ExplanationReactComponentPropsDeepReadOnly of 'loc
  | ExplanationReactComponentRefRequirement
  | ExplanationReactHookArgsDeepReadOnly of 'loc
  | ExplanationReactHookIncompatibleWithEachOther
  | ExplanationReactHookIncompatibleWithNormalFunctions
  | ExplanationReactHookReturnDeepReadOnly of 'loc
  | ExplanationReactImmutable of 'loc
  | ExplanationIncompatibleReactDeepReadOnly
  | ExplanationRenderTypeRequirement
  | ExplanationTypeGuardCompatibility
  | ExplanationTypeGuardPositiveConsistency of {
      return: 'loc virtual_reason;
      param: 'loc virtual_reason;
      guard_type: 'loc virtual_reason;
      is_return_false_statement: bool;
    }
  | ExplanationAdditionalUnionMembers of {
      left: 'loc virtual_reason;
      right: 'loc virtual_reason;
      members: string list;
      extra_number: int;
    }

type 'loc frame =
  | FrameAnonymous
  | FrameArrayElement
  | FrameCallableSignature
  | FrameEnumRepresentationType
  | FrameFunNthArgument of int
  | FrameFunThisArgument
  | FrameFunNthParam of int
  | FrameFunThisParam
  | FrameIndexerProperty
  | FrameIndexerPropertyKey
  | FrameProperty of name Nel.t
  | FrameTupleIndex of int
  | FrameTypeArgument of 'loc virtual_reason
  | FrameTypeParameterBound of string
  | FrameTypePredicate
  | FrameReturnValue

type 'loc root_message =
  | RootCannotAccessIndex of {
      index: 'loc virtual_reason_desc;
      object_: 'loc virtual_reason_desc;
    }
  | RootCannotAddComputedProperty
  | RootCannotAssign of {
      init: 'loc virtual_reason_desc;
      target: 'loc virtual_reason_desc option;
    }
  | RootCannotCall of 'loc virtual_reason_desc
  | RootCannotCallWithNamedParam of {
      fn: 'loc virtual_reason_desc;
      lower: 'loc virtual_reason_desc;
      name: string;
    }
  | RootCannotCallWithNthParam of {
      fn: 'loc virtual_reason_desc;
      lower: 'loc virtual_reason_desc;
      n: int;
    }
  | RootCannotCallObjectAssign of 'loc virtual_reason_desc
  | RootCannotCast of {
      lower: 'loc virtual_reason_desc;
      upper: 'loc virtual_reason_desc;
    }
  | RootCannotCheckAgainst of {
      test: 'loc virtual_reason_desc;
      discriminant: 'loc virtual_reason;
    }
  | RootCannotCheckAgainstSwitchDiscriminant of 'loc
  | RootCannotCoerce of {
      from: 'loc virtual_reason_desc;
      target: 'loc virtual_reason_desc;
    }
  | RootCannotConformToCommonInterface
  | RootCannotCompareWithProperty of {
      sentinel: 'loc virtual_reason;
      obj: 'loc virtual_reason;
      key: string;
    }
  | RootCannotCreateElement of 'loc virtual_reason_desc
  | RootCannotDeclareRef
  | RootCannotDeclareTypeGuard of {
      type_guard_loc: 'loc;
      fn: 'loc virtual_reason;
    }
  | RootCannotDefineClassMethod of {
      method_: 'loc virtual_reason;
      name: 'loc virtual_reason_desc;
    }
  | RootCannotDefineShadowedProtoProperty
  | RootCannotDelete of 'loc virtual_reason_desc
  | RootCannotExpectImplicitReturn of {
      upper: 'loc virtual_reason_desc;
      fn: 'loc virtual_reason_desc;
    }
  | RootCannotExtendClass of {
      extends: 'loc virtual_reason;
      def: 'loc virtual_reason_desc;
    }
  | RootCannotGetProp of 'loc virtual_reason_desc
  | RootCannotGetRest of 'loc virtual_reason_desc
  | RootCannotImplementClass of {
      implements: 'loc virtual_reason;
      def: 'loc virtual_reason_desc;
    }
  | RootCannotInitializeField of {
      field: 'loc virtual_reason_desc;
      body: 'loc virtual_reason_desc;
    }
  | RootCannotInstantiateEval of 'loc virtual_reason
  | RootCannotInstantiateRenderType of 'loc virtual_reason
  | RootCannotInstantiateTypeApp of 'loc virtual_reason_desc
  | RootCannotReturn of 'loc virtual_reason_desc
  | RootCannotShadowProto of 'loc virtual_reason
  | RootCannotShadowProtoProperty
  | RootCannotSpread of 'loc virtual_reason_desc
  | RootCannotUpdate of 'loc virtual_reason_desc
  | RootCannotUseInferTypeBound of {
      bound: 'loc virtual_reason_desc;
      infer: 'loc virtual_reason_desc;
    }
  | RootCannotUseTypeGuard of {
      guard_type: 'loc virtual_reason;
      param_name: string;
    }
  | RootCannotYield of 'loc virtual_reason_desc
  | RootPositiveTypeGuardConsistency

type 'loc message =
  | MessagePlainTextReservedForInternalErrorOnly of string
  | MessageAlreadyExhaustivelyCheckOneEnumMember of {
      member_name: string;
      prev_check_loc: 'loc;
      enum_reason: 'loc virtual_reason;
    }
  | MessageAlreadyExhaustivelyCheckAllEnumMembers of { enum_reason: 'loc virtual_reason }
  | MessageAmbiguousNumericKeyWithVariance
  | MessageAmbiguousObjectType
  | MessageAnyValueUsedAsType of 'loc virtual_reason_desc
  | MessageBadLibdefModuleOverride of 'loc virtual_reason
  | MessageBadLibdefNameOverride of 'loc virtual_reason
  | MessageCannotAccessEnumMember of {
      member_name: name option;
      suggestion: string option;
      description: 'loc virtual_reason_desc;
      enum_reason: 'loc virtual_reason;
    }
  | MessageCannotAccessObjectWithComputedProp of {
      reason_prop: 'loc virtual_reason;
      kind: InvalidObjKey.t;
    }
  | MessageCannotAccessReactRefInRender of {
      usage: 'loc virtual_reason;
      in_hook: bool;
    }
  | MessageCannotAddComputedPropertyDueToPotentialOverwrite of {
      key_loc: 'loc;
      overwritten_locs: 'loc list;
    }
  | MessageCannotApplyNonPolymorphicType
  | MessageCannotAssignToObjectWithComputedProp of 'loc virtual_reason
  | MessageCannotAssignToObjectWithComputedPropWithKey of {
      reason_prop: 'loc virtual_reason;
      reason_key: 'loc virtual_reason;
      kind: InvalidObjKey.t;
    }
  | MessageCannotAssignToOptionalTupleElement of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageCannotAssignToInvalidLHS
  | MessageCannotBuildTypedInterface of 'loc Signature_error.t
  | MessageCannotCallMaybeReactHook of {
      callee_loc: 'loc;
      hooks: 'loc list;
      non_hooks: 'loc list;
    }
  | MessageCannotCallNonHookSyntaxHook of 'loc
  | MessageCannotCallObjectFunctionOnEnum of {
      reason: 'loc virtual_reason;
      enum_reason: 'loc virtual_reason;
    }
  | MessageCannotCallReactComponent of 'loc virtual_reason
  | MessageCannotCallReactFunctionWithoutAtLeastNArgs of {
      fn_name: string;
      n: int;
    }
  | MessageCannotCallReactHookConditionally of 'loc
  | MessageCannotCallReactHookInDefinitelyNonComponentOrHook of 'loc
  | MessageCannotCallReactHookInNonComponentSyntaxComponentOrHookSyntaxHook of 'loc
  | MessageCannotCallReactHookInUnknownContext of 'loc
  | MessageCannotCallReactHookWithIllegalName of 'loc
  | MessageCannotCallFunctionWithExtraArg of {
      def_reason: 'loc virtual_reason;
      param_count: int;
    }
  | MessageCannotChangeEnumMember of 'loc virtual_reason
  | MessageCannotCompare of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageCannotCompareNonStrict of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageCannotCreateExactType of 'loc virtual_reason
  | MessageCannotDeclareAlreadyBoundGlobal of concrete_reason
  | MessageCannotDeclareAlreadyBoundName of concrete_reason
  | MessageCannotDeclareAlreadyBoundNameInNamespace of 'loc virtual_reason
  | MessageCannotDelete of 'loc virtual_reason
  | MessageCannotDetermineEmptyArrayLiteralType
  | MessageCannotDetermineModuleType
  | MessageCannotExportRenamedDefault of {
      name: string option;
      is_reexport: bool;
    }
  | MessageCannotExhaustivelyCheckAbstractEnums of {
      description: 'loc virtual_reason_desc;
      enum_reason: 'loc virtual_reason;
    }
  | MessageCannotExhaustivelyCheckEnumWithUnknowns of {
      description: 'loc virtual_reason_desc;
      enum_reason: 'loc virtual_reason;
    }
  | MessageCannotImplementNonInterface of 'loc virtual_reason_desc
  | MessageCannotInstantiateObjectUtilTypeWithEnum of {
      description: 'loc virtual_reason_desc;
      enum_reason: 'loc virtual_reason;
    }
  | MessageCannotIterateEnum of {
      reason: 'loc virtual_reason;
      for_in: bool;
    }
  | MessageCannotIterateWithForIn of 'loc virtual_reason
  | MessageCannotMutateThisPrototype
  | MessageCannotNestComponents
  | MessageCannotOptimizeUnionDueToNonUniqueKeys of
      'loc virtual_reason Nel.t Type.UnionRep.UnionEnumMap.t NameUtils.Map.t
  | MessageCannotOptimizeUnionInternally of 'loc Type.UnionRep.optimized_error
  | MessageCannotPassReactRefAsArgument of {
      usage: 'loc virtual_reason;
      in_hook: bool;
    }
  | MessageCannotPerformArithOnNonNumbersOrBigInt of 'loc virtual_reason
  | MessageCannotPerformBigIntRShift3 of 'loc virtual_reason
  | MessageCannotPerformBigIntUnaryPlus of 'loc virtual_reason
  | MessageCannotPerformBinaryArith of {
      kind: Type.ArithKind.t;
      reason_l: 'loc virtual_reason;
      reason_r: 'loc virtual_reason;
    }
  | MessageCannotReassignConstant of concrete_reason
  | MessageCannotReassignConstantLikeBinding of {
      definition: 'loc virtual_reason;
      binding_kind: assigned_const_like_binding_type;
    }
  | MessageCannotReassignEnum of concrete_reason
  | MessageCannotReassignImport of concrete_reason
  | MessageCannotRedeclareVar of concrete_reason
  | MessageCannotReferenceTypeGuardParameter of {
      type_guard_reason: 'loc virtual_reason;
      binding_reason: 'loc virtual_reason;
    }
  | MessageCannotResolveBuiltinName of string
  | MessageCannotResolveBuiltinModule of {
      name: string;
      potential_generator: string option;
    }
  | MessageCannotResolveExpectedModule of {
      name: string;
      expected_module_purpose: expected_module_purpose;
    }
  | MessageCannotSpreadDueToPotentialOverwrite of {
      spread_reason: 'loc virtual_reason;
      object_reason: 'loc virtual_reason;
      key_reason: 'loc virtual_reason;
    }
  | MessageCannotSpreadGeneral of {
      spread_reason: 'loc virtual_reason;
      object1_reason: 'loc virtual_reason;
      object2_reason: 'loc virtual_reason;
      propname: name;
      error_kind: exactness_error_kind;
    }
  | MessageCannotSpreadInexactMayOverwriteIndexer of {
      spread_reason: 'loc virtual_reason;
      object2_reason: 'loc virtual_reason;
      key_reason: 'loc virtual_reason;
      value_reason: 'loc virtual_reason;
    }
  | MessageCannotSpreadInterface of {
      spread_reason: 'loc virtual_reason;
      interface_reason: 'loc virtual_reason;
    }
  | MessageCannotUseAsConstructor of 'loc virtual_reason
  | MessageCannotUseAsPrototype of 'loc virtual_reason
  | MessageCannotUseAsSuperClass of 'loc virtual_reason
  | MessageCannotUseBeforeDeclaration of concrete_reason
  | MessageCannotUseComputedPropertyWithUnion of 'loc virtual_reason
  | MessageCannotUseDefaultImportWithDestrucuturing
  | MessageCannotUseDollarExports
  | MessageCannotUseEnumMemberUsedAsType of {
      description: 'loc virtual_reason_desc;
      enum_reason: 'loc virtual_reason;
    }
  | MessageCannotUseExportInNonLegalToplevelContext of string
  | MessageCannotUseImportStar of 'loc virtual_reason
  | MessageCannotUseInOperatorDueToBadLHS of 'loc virtual_reason
  | MessageCannotUseInOperatorDueToBadRHS of 'loc virtual_reason
  | MessageCannotUseInstanceOfOperatorDueToBadRHS of 'loc virtual_reason
  | MessageCannotUseMixedImportAndRequire of 'loc virtual_reason
  | MessageCannotUseNonPolymorphicTypeWithTypeArgs of {
      is_new: bool;
      reason_arity: 'loc virtual_reason;
      expected_arity: int;
    }
  | MessageCannotUsePrimitiveAsInterface of {
      reason: 'loc virtual_reason;
      interface_reason: 'loc virtual_reason;
      kind: [ `Boolean | `Number | `String ];
    }
  | MessageCannotUseStrUtilType
  | MessageCannotUseTypeDueToPolarityMismatch of {
      reason_targ: 'loc virtual_reason;
      expected_polarity: Polarity.t;
      actual_polarity: Polarity.t;
    }
  | MessageCannotUseTypeForAnnotationInference of {
      reason_op: 'loc virtual_reason;
      reason: 'loc virtual_reason;
      suggestion: string option;
    }
  | MessageCannotUseTypeGuardWithFunctionParamHavoced of {
      type_guard_desc: 'loc virtual_reason_desc;
      param_reason: 'loc virtual_reason;
      call_locs: 'loc list;
    }
  | MessageCannotUseTypeInValuePosition of {
      reason: concrete_reason;
      type_only_namespace: bool;
      imported_name: string option;
    }
  | MessageCannotUseTypeWithInvalidTypeArgs of {
      reason_main: 'loc virtual_reason;
      reason_tapp: 'loc virtual_reason;
    }
  | MessageCannotUseTypeWithoutAnyTypeArgs of {
      reason_arity: 'loc virtual_reason;
      min_arity: int;
      max_arity: int;
    }
  | MessageCannotUseTypeWithoutAtLeastNTypeArgs of int
  | MessageCannotUseTypeWithoutExactlyNTypeArgs of int
  | MessageCannotUseTypeWithTooFewTypeArgs of {
      reason_arity: 'loc virtual_reason;
      n: int;
    }
  | MessageCannotUseTypeWithTooManyTypeArgs of {
      reason_arity: 'loc virtual_reason;
      n: int;
    }
  | MessageComponentMissingReturn of 'loc virtual_reason
  | MessageComponentNonUpperCase
  | MessageDefinitionCycle of ('loc virtual_reason * 'loc list * 'loc Env_api.annot_loc list) Nel.t
  | MessageDefinitionInvalidRecursive of {
      description: 'loc virtual_reason_desc;
      recursion: 'loc list;
      annot_locs: 'loc Env_api.annot_loc list;
    }
  | MessageDeprecatedBool
  | MessageDevOnlyRefinedLocInfo of { refining_locs: 'loc list }
  | MessageDevOnlyInvalidatedRefinementInfo of ('loc * Refinement_invalidation.reason) list
  | MessageDocblockError of docblock_error
  | MessageDoesNotRender of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageDollarCallArity of {
      op: 'loc virtual_reason;
      def: 'loc virtual_reason;
      n: int;
    }
  | MessageDuplicateClassMember of {
      name: string;
      static: bool;
    }
  | MessageDuplicateEnumMember of {
      enum_reason: 'loc virtual_reason;
      prev_use_loc: 'loc;
    }
  | MessageDuplicateModuleProvider of {
      module_name: string;
      provider: 'loc;
      conflict: 'loc;
    }
  | MessageEnumsNotEnabled
  | MessageExponentialSpread of {
      reason: 'loc virtual_reason;
      reasons_for_operand1: 'loc exponential_spread_reason_group;
      reasons_for_operand2: 'loc exponential_spread_reason_group;
    }
  | MessageExportValueAsType of string
  | MessageFunctionRequiresAnotherArgument of {
      def: 'loc virtual_reason;
      from: 'loc virtual_reason option;
    }
  | MessageImplicitInexactObject
  | MessageImportTypeAsTypeof of string
  | MessageImportTypeAsValue of string
  | MessageImportValueAsType of string
  | MessageIncompatibleArity of {
      lower: 'loc virtual_reason;
      lower_arity: int;
      upper: 'loc virtual_reason;
      upper_arity: int;
    }
  | MessageIncompatibleTupleArity of {
      lower_reason: 'loc virtual_reason;
      lower_arity: int * int;
      lower_inexact: bool;
      upper_reason: 'loc virtual_reason;
      upper_arity: int * int;
      upper_inexact: bool;
      unify: bool;
    }
  | MessageIncompatibleImplicitReturn of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      return: 'loc virtual_reason;
    }
  | MessageIncompatibleClassToObject of {
      reason_class: 'loc virtual_reason;
      reason_obj: 'loc virtual_reason;
    }
  | MessageIncompatibleComponentRestParam of 'loc virtual_reason
  | MessageIncompatibleGeneral of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompatibleMappedTypeKey of {
      source_type: 'loc virtual_reason;
      mapped_type: 'loc virtual_reason;
    }
  | MessageIncompatibleNonLiteralArrayToTuple of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompatibleNonTypeGuardToTypeGuard of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompatibleReactDeepReadOnly of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      dro_loc: 'loc;
    }
  | MessageIncompatibleReactHooksDueToUniqueness of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompatibleReactHooksWithNonReactHook of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      lower_is_hook: bool;
      hook_is_annot: bool;
    }
  | MessageIncompatibleWithExact of {
      kind: exactness_error_kind;
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompatibleWithIndexed of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompleteExhausiveCheckEnum of {
      description: 'loc virtual_reason_desc;
      enum_reason: 'loc virtual_reason;
      left_to_check: string list;
      default_case_loc: 'loc option;
    }
  | MessageIncorrectType of IncorrectType.t
  | MessageInvalidArgument of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageInvalidCatchParameterAnnotation
  | MessageInvalidComponentRestParam
  | MessageInvalidEnumMemberCheck of {
      enum_reason: 'loc virtual_reason;
      example_member: string option;
    }
  | MessageInvalidGenericRef of string
  | MessageInvalidGraphQL of Graphql.error
  | MessageInvalidHookNaming
  | MessageInvalidImportStarUse of 'loc virtual_reason
  | MessageInvalidInferType
  | MessageInvalidLintSettings of LintSettings.lint_parse_error
  | MessageInvalidMappedTypeInInterfaceOrDeclaredClass
  | MessageInvalidMappedTypeWithExactOrInexact
  | MessageInvalidMappedTypeWithExtraProps
  | MessageInvalidMappedTypeWithOptionalityRemoval
  | MessageInvalidMappedTypeWithVarianceOnArrayInput
  | MessageInvalidReactCreateElement of 'loc virtual_reason
  | MessageInvalidRefPropertyInSpread of {
      ref_loc: 'loc;
      spread_loc: 'loc;
    }
  | MessageInvalidKeyPropertyInSpread of {
      key_loc: 'loc;
      spread_loc: 'loc;
    }
  | MessageInvalidRendersTypeArgument of {
      renders_variant: Flow_ast.Type.Renders.variant;
      invalid_render_type_kind: 'loc invalid_render_type_kind;
      invalid_type_reasons: 'loc virtual_reason Nel.t;
    }
  | MessageInvalidSelfReferencingTypeAnnotation of {
      name: string;
      loc: 'loc;
    }
  | MessageInvalidTrivialRecursiveDefinition of 'loc virtual_reason_desc
  | MessageInvalidTupleRequiredAfterOptional of {
      reason_tuple: 'loc virtual_reason;
      reason_required: 'loc virtual_reason;
      reason_optional: 'loc virtual_reason;
    }
  | MessageInvalidTupleTypeSpread of 'loc virtual_reason
  | MessageTupleElementAfterInexactSpread
  | MessageInternalType of internal_type
  | MessageInvalidTypeCastingSyntax of Options.CastingSyntax.t
  | MessageInvalidTypeGuardFunctionKind of string
  | MessageInvalidTypeGuardFunctionWritten of {
      type_guard_reason: 'loc virtual_reason;
      write_locs: 'loc list;
    }
  | MessageNegativeTypeGuardConsistency of {
      reason: 'loc virtual_reason;
      return_reason: 'loc virtual_reason;
      type_reason: 'loc virtual_reason;
    }
  | MessageInvalidTypeGuardParamUnbound of 'loc virtual_reason
  | MessageInvalidTypeGuardThisParam of 'loc virtual_reason
  | MessageInvalidUseOfFlowEnforceOptimized of 'loc virtual_reason
  | MessageLowerIsNotArray of 'loc virtual_reason
  | MessageLowerIsNotArrayIndex of 'loc virtual_reason
  | MessageLowerIsNotClass of 'loc virtual_reason
  | MessageLowerIsNotClassWithPrivateProps of 'loc virtual_reason
  | MessageLowerIsNotFunction of 'loc virtual_reason
  | MessageLowerIsNotFunctionType of 'loc virtual_reason
  | MessageLowerIsNotInheritable of 'loc virtual_reason
  | MessageLowerIsNotInstanceType of 'loc virtual_reason
  | MessageLowerIsNotObject of 'loc virtual_reason
  | MessageLowerIsNotPolymorphicType of 'loc virtual_reason
  | MessageLowerIsNotReactComponent of 'loc virtual_reason
  | MessageLowerIsNotSupportedByUnclassifiedUse of {
      lower: 'loc virtual_reason;
      ctor: string;
    }
  | MessageMethodUnbinding of {
      reason_op: 'loc virtual_reason;
      context_loc: 'loc;
    }
  | MessageMissingAnnotation of 'loc virtual_reason_desc
  | MessageMissingAnnotationDueToContextualTypingFailure of 'loc virtual_reason_desc
  | MessageMissingAnnotationForGenericFunction of 'loc virtual_reason_desc
  | MessageMissingPlatformSupport of {
      available_platforms: SSet.t;
      required_platforms: SSet.t;
    }
  | MessageNoDefaultExport of {
      module_name: string;
      suggestion: string option;
    }
  | MessageNoNamedExport of {
      module_name: string;
      export_name: string;
      suggestion: string option;
    }
  | MessageNonLiteralString of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      n: int;
    }
  | MessageNonConstVarExport of 'loc virtual_reason option
  | MessageNonStrictImport
  | MessageNonToplevelExport
  | MessageOnlyDefaultExport of {
      module_name: string;
      export_name: string;
    }
  | MessageParseError of Parse_error.t
  | MessagePlatformSpecificImplementationModuleLookupFailed of string
  | MessagePropMissing of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason option;
      prop: string option;
      suggestion: string option;
      reason_indexer: 'loc virtual_reason option;
    }
  | MessagePropPolarityMismatch of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
      lpole: Polarity.t;
      upole: Polarity.t;
      prop: string option;
    }
  | MessagePropNotReadable of name option
  | MessagePropNotWritable of name option
  | MessageReactIntrinsicOverlap of {
      use: 'loc virtual_reason;
      def: 'loc;
      type_: 'loc;
      mixed: bool;
    }
  | MessageReadonlyArraysCannotBeWrittenTo
  | MessageRecursionLimitExceeded
  | MessageRedeclareComponentProp of {
      duplicates: ('loc * name * 'loc) Nel.t;
      spread_loc: 'loc;
    }
  | MessageShouldAnnotateVariableOnlyInitializedInGenericContext of {
      reason: 'loc virtual_reason;
      possible_generic_escape_locs: 'loc list;
    }
  | MessageShouldAnnotateVariableUsedInGenericContext of {
      reason: 'loc virtual_reason;
      null_loc: 'loc;
      initialized: bool;
      possible_generic_escape_locs: 'loc list;
    }
  | MessageShouldNotBeCoerced of 'loc virtual_reason
  | MessageShouldUseArrayLiteral
  | MessageSketchyNumber of 'loc virtual_reason
  | MessageSketchyNullCheck of {
      kind: Lints.sketchy_null_kind;
      falsy_loc: 'loc;
      null_loc: 'loc;
    }
  | MessageSuppressionMalformedCode
  | MessageSuppressionMissingCode of string
  | MessageThisInComponent of 'loc
  | MessageThisInExportedFunction
  | MessageThisSuperInObject of 'loc virtual_reason * This_finder.kind
  | MessageTSKeyofType
  | MessageTSNeverType
  | MessageTSParamExtends
  | MessageTSReadonlyOperatorOnArray
  | MessageTSReadonlyOperatorOnTuple
  | MessageTSReadonlyType
  | MessageTSSatisfiesType of Options.CastingSyntax.t
  | MessageTSVarianceIn
  | MessageTSVarianceInOut
  | MessageTSVarianceOut
  | MessageTSVarianceReadOnly
  | MessageTSUndefinedType
  | MessageTSUnknownType
  | MessageTupleElementNotReadable of {
      reason: 'loc virtual_reason;
      index: int;
      name: string option;
    }
  | MessageTupleElementNotWritable of {
      reason: 'loc virtual_reason;
      index: int;
      name: string option;
    }
  | MessageTupleIndexOutOfBound of {
      reason_op: 'loc virtual_reason;
      inexact: bool;
      length: int;
      index: string;
    }
  | MessageTupleNonIntegerIndex of {
      index_def_loc: 'loc;
      index: string;
    }
  | MessageTupleNonStaticallyKnownIndex
  | MessageTuplePolarityMismatch of {
      index: int;
      reason_lower: 'loc virtual_reason;
      reason_upper: 'loc virtual_reason;
      polarity_lower: Polarity.t;
      polarity_upper: Polarity.t;
    }
  | MessageTypeGuardIndexMismatch of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageTypeGuardImpliesMismatch of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageIncompatiblETypeParamConstIncompatibility of {
      lower: 'loc virtual_reason;
      upper: 'loc virtual_reason;
    }
  | MessageTypeParamConstInvalidPosition of 'loc virtual_reason
  | MessageUnclearType
  | MessageUnderconstrainedImplicitInstantiaton of {
      reason_call: 'loc virtual_reason;
      reason_tparam: 'loc virtual_reason;
    }
  | MessageUndocumentedFeature
  | MessageIllegalAssertOperator of {
      obj: 'loc virtual_reason;
      specialized: bool;
    }
  | MessageUnexpectedTemporaryBaseType
  | MessageUnexpectedUseOfThisType
  | MessageUninitializedInstanceProperty of Lints.property_assignment_kind
  | MessageUnknownParameterTypes of 'loc virtual_reason
  | MessageUnnecessaryDeclareTypeOnlyExport
  | MessageUnnecessaryInvariant of 'loc virtual_reason
  | MessageUnnecessaryOptionalChain of 'loc virtual_reason
  | MessageUnreachableCode
  | MessageUnsafeGetterSetter
  | MessageUnsafeObjectAssign
  | MessageUnsupportedKeyInObject of {
      key_error_kind: InvalidObjKey.t;
      obj_kind: [ `Type | `Literal ];
    }
  | MessageUnsupportedSyntax of unsupported_syntax
  | MessageUnsupportedVarianceAnnotation of string
  | MessageUntypedImport of string
  | MessageUntypedTypeImport of string
  | MessageUnusedPromiseInAsyncScope
  | MessageUnusedPromiseInSyncScope
  | MessageUnusedSuppression
  | MessageValueUsedAsType of 'loc virtual_reason_desc
  | MessageVariableNeverInitAssignedAnnotated of 'loc virtual_reason
  | MessageVariableOnlyAssignedByNull of {
      reason: 'loc virtual_reason;
      null_loc: 'loc option;
    }
  | MessageMatchNotExhaustive of 'loc virtual_reason
  | MessageMatchInvalidBindingKind of { kind: Flow_ast.Variable.kind }
  | MessageMatchInvalidObjectPropertyLiteral
  | MessageMatchInvalidUnaryZero
  | MessageMatchInvalidUnaryPlusBigInt
  | MessageMatchDuplicateObjectProperty of { name: string }
  | MessageMatchBindingInOrPattern
  | MessageMatchInvalidAsPattern
  | MessageMatchInvalidPatternReference of { binding_reason: 'loc virtual_reason }
  | MessageMatchInvalidObjectShorthand of { name: string }
  | MessageMatchStatementInvalidBody

type 'loc intermediate_error = {
  kind: Flow_errors_utils.error_kind;
  loc: Loc.t;
  error_code: Error_codes.error_code option;
  root: (Loc.t * 'loc root_message) option;
  message: 'loc error_message;
  misplaced_source_file: File_key.t option;
  unsuppressable: bool;
}

and 'loc error_message =
  | SingletonMessage of {
      message: 'loc message;
      frames: 'loc frame list option;
      explanations: 'loc explanation list option;
    }
  | SpeculationMessage of {
      frames: 'loc frame list;
      explanations: 'loc explanation list;
      branches: (int * 'loc intermediate_error) list;
    }
