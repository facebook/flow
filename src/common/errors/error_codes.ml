(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type error_code =
  | AmbiguousObjectType
  | ReactRulePropsMutation
  | ReactRuleHookMutation
  | ReactRuleHookIncompatible
  | ReactRuleHook
  | ReactRuleRef
  | CannotDelete
  | CannotImplement
  | CannotInferType
  | CannotRead
  | CannotReassignConstLike
  | CannotResolveModule
  | CannotResolveName
  | CannotSpreadIndexer
  | CannotSpreadInexact
  | CannotSpreadInterface
  | CannotWrite
  | CannotWriteEnum
  | ClassObject
  | ComponentCase
  | ComponentMissingReturn
  | ComponentThisReference
  | DefaultImportAccess
  | DeprecatedType
  | DeprecatedUtility
  | DuplicateClassMember
  | DuplicateEnumInit
  | DuplicateFlowDecl
  | DuplicateJsxDecl
  | DuplicateJsxRuntimeDecl
  | DuplicateModule
  | EmptyArrayNoAnnot
  | EnumValueAsType
  | EscapedGeneric
  | ExponentialSpread
  | ExportRenamedDefault
  | ExportValueAsType
  | ExtraArg
  | ExtraTypeArg
  | FunctionPredicate
  | IllegalEnum
  | IllegalGetSet
  | IllegalKey
  | IllegalNewArray
  | IllegalThis
  | IllegalTypeof
  | ImplicitInexactObject
  | ImportTypeAsValue
  | ImportValueAsType
  | IncompatibleCall
  | IncompatibleCast
  | IncompatibleExact
  | IncompatibleExtend
  | IncompatibleFunctionIndexer
  | IncompatibleIndexer
  | IncompatibleReturn
  | IncompatibleType
  | IncompatibleTypeArg
  | IncompatibleTypeGuard
  | IncompatibleUse
  | IncompatibleVariance
  | InvalidCallUtil
  | InvalidCatchParameterAnnotation
  | InvalidCharsetTypeArg
  | InvalidCompare
  | InvalidComputedProp
  | InvalidConstructor
  | InvalidEnumAccess
  | InvalidExact
  | InvalidExhaustiveCheck
  | InvalidExport
  | InvalidExportsTypeArg
  | InvalidExtends
  | InvalidFlowModeDecl
  | InvalidGraphQL
  | InvalidExportedAnnotation
  | InvalidIdx
  | InvalidImportStarUse
  | InvalidImportType
  | InvalidInfer
  | InvalidInLhs
  | InvalidInRhs
  | InvalidJsxDecl
  | InvalidJsxRuntimeDecl
  | InvalidLhs
  | InvalidMappedType
  | InvalidModule
  | InvalidObjMap
  | InvalidObjMapi
  | InvalidPropertyTypeArg
  | InvalidPropType
  | InvalidReactConfig
  | InvalidRendersTypeArgument
  | InvalidSupportsPlatformDecl
  | InvalidTupleArity
  | InvalidTupleIndex
  | InvalidTupleMap
  | InvalidTypeArg
  | InvalidTypeCastSyntax
  | InvalidTypeOf
  | InvalidTempType
  | LintSetting
  | MalformedPackage
  | MethodUnbinding
  | MissingAnnot
  | MissingLocalAnnot
  | MissingThisAnnot
  | MissingArg
  | MissingExport
  | MissingPlatformSupport
  | MissingTypeArg
  | MixedImportAndRequire
  | ModuleTypeConflict
  | NameAlreadyBound
  | NonConstVarExport
  | NonpolymorphicTypeApp
  | NonpolymorphicTypeArg
  | NonstrictImport
  | NotAClass
  | NotAComponent
  | NotAFunction
  | NotAnArray
  | NotAnObject
  | NotIterable
  | ObjectThisReference
  | PropMissing
  | ReassignConst
  | ReassignEnum
  | ReassignImport
  | ReferenceBeforeDeclaration
  | RequireExplicitEnumSwitchCases
  | SignatureVerificationFailure
  | SketchyNullBool
  | SketchyNullMixed
  | SketchyNullNumber
  | SketchyNullString
  | SketchyNullBigInt
  | SketchyNumberAnd
  | Speculation
  | SpeculationAmbiguous
  | TSSyntax
  | ThisInExportedFunction
  | TupleInvalidTypeSpread
  | TupleRequiredAfterOptional
  | TypeAsValue
  | UnclearAddition
  | UnclearType
  | UnderconstrainedImplicitInstantiation
  | UninitializedInstanceProperty
  | UnionUnoptimizable
  | UnnecessaryInvariant
  | UnnecessaryOptionalChain
  | UnnecessaryDeclareTypeOnlyExport
  | UnreachableCode
  | UnsafeAddition
  | UnsafeArith
  | UnsafeGettersSetters
  | UnsupportedSyntax
  | UnsupportedVarianceAnnotation
  | UntypedImport
  | UntypedTypeImport
  | ValueAsType
  | InvalidDeclaration
  | DefinitionCycle
  | RecursiveDefinition
  | LogicalAssignmentOperatorsNotSupported
  | UnusedPromise
  | BigIntRShift3
  | BigIntNumCoerce
  | InvalidComponentProp
  | ReactIntrinsicOverlap
  | NestedComponent
  | InvalidRef

let code_of_lint : Lints.lint_kind -> error_code = function
  | Lints.ReactIntrinsicOverlap -> ReactIntrinsicOverlap
  | Lints.NestedComponent -> NestedComponent
  | Lints.UntypedTypeImport -> UntypedTypeImport
  | Lints.UntypedImport -> UntypedImport
  | Lints.NonstrictImport -> NonstrictImport
  | Lints.UnclearType -> UnclearType
  | Lints.DeprecatedType _ -> DeprecatedType
  | Lints.UnsafeGettersSetters -> UnsafeGettersSetters
  | Lints.UnnecessaryOptionalChain -> UnnecessaryOptionalChain
  | Lints.UnnecessaryInvariant -> UnnecessaryInvariant
  | Lints.ImplicitInexactObject -> ImplicitInexactObject
  | Lints.UninitializedInstanceProperty -> UninitializedInstanceProperty
  | Lints.AmbiguousObjectType -> AmbiguousObjectType
  | Lints.RequireExplicitEnumSwitchCases -> RequireExplicitEnumSwitchCases
  | Lints.SketchyNumber Lints.SketchyNumberAnd -> SketchyNumberAnd
  | Lints.SketchyNull (Lints.SketchyNullBool | Lints.SketchyNullEnumBool) -> SketchyNullBool
  | Lints.SketchyNull (Lints.SketchyNullString | Lints.SketchyNullEnumString) -> SketchyNullString
  | Lints.SketchyNull (Lints.SketchyNullNumber | Lints.SketchyNullEnumNumber) -> SketchyNullNumber
  | Lints.SketchyNull (Lints.SketchyNullBigInt | Lints.SketchyNullEnumBigInt) -> SketchyNullBigInt
  | Lints.SketchyNull Lints.SketchyNullMixed -> SketchyNullMixed
  | Lints.DefaultImportAccess -> DefaultImportAccess
  | Lints.InvalidImportStarUse -> InvalidImportStarUse
  | Lints.NonConstVarExport -> NonConstVarExport
  | Lints.ThisInExportedFunction -> ThisInExportedFunction
  | Lints.MixedImportAndRequire -> MixedImportAndRequire
  | Lints.ExportRenamedDefault -> ExportRenamedDefault
  | Lints.UnusedPromise -> UnusedPromise

let require_specific : error_code -> bool = function
  | ReactRulePropsMutation
  | ReactRuleHookMutation
  | ReactRuleHookIncompatible
  | ReactRuleHook
  | ReactRuleRef ->
    true
  | _ -> false

let string_of_code : error_code -> string = function
  | ReactRulePropsMutation -> "react-rule-unsafe-mutation"
  | ReactRuleHookMutation -> "react-rule-hook-mutation"
  | ReactRuleHookIncompatible -> "react-rule-hook-incompatible"
  | ReactRuleHook -> "react-rule-hook"
  | ReactRuleRef -> "react-rule-unsafe-ref"
  | AmbiguousObjectType -> "ambiguous-object-type"
  | CannotDelete -> "cannot-delete"
  | CannotImplement -> "cannot-implement"
  | CannotInferType -> "cannot-infer-type"
  | CannotRead -> "cannot-read"
  | CannotReassignConstLike -> "cannot-reassign"
  | CannotResolveModule -> "cannot-resolve-module"
  | CannotResolveName -> "cannot-resolve-name"
  | CannotSpreadIndexer -> "cannot-spread-indexer"
  | CannotSpreadInexact -> "cannot-spread-inexact"
  | CannotSpreadInterface -> "cannot-spread-interface"
  | CannotWrite -> "cannot-write"
  | CannotWriteEnum -> "cannot-write-enum"
  | ClassObject -> "class-object-subtyping"
  | ComponentThisReference -> "component-this-reference"
  | ComponentCase -> "component-case"
  | ComponentMissingReturn -> "component-missing-return"
  | DefaultImportAccess -> "default-import-access"
  | DeprecatedType -> "deprecated-type"
  | DeprecatedUtility -> "deprecated-utility"
  | DuplicateClassMember -> "duplicate-class-member"
  | EmptyArrayNoAnnot -> "missing-empty-array-annot"
  | DuplicateEnumInit -> "duplicate-enum-init"
  | DuplicateFlowDecl -> "duplicate-flow-decl"
  | DuplicateJsxDecl -> "duplicate-jsx-decl"
  | DuplicateJsxRuntimeDecl -> "duplicate-jsx-runtime-decl"
  | DuplicateModule -> "duplicate-module"
  | EnumValueAsType -> "enum-value-as-type"
  | EscapedGeneric -> "escaped-generic"
  | ExponentialSpread -> "exponential-spread"
  | ExportRenamedDefault -> "export-renamed-default"
  | ExportValueAsType -> "export-value-as-type"
  | ExtraArg -> "extra-arg"
  | ExtraTypeArg -> "extra-type-arg"
  | FunctionPredicate -> "function-predicate"
  | IllegalEnum -> "illegal-enum"
  | IllegalGetSet -> "illegal-get-set"
  | IllegalKey -> "illegal-key"
  | IllegalNewArray -> "illegal-new-array"
  | IllegalThis -> "illegal-this"
  | IllegalTypeof -> "illegal-typeof"
  | ImplicitInexactObject -> "implicit-inexact-object"
  | ImportTypeAsValue -> "import-type-as-value"
  | ImportValueAsType -> "import-value-as-type"
  | IncompatibleCall -> "incompatible-call"
  | IncompatibleCast -> "incompatible-cast"
  | IncompatibleExact -> "incompatible-exact"
  | IncompatibleExtend -> "incompatible-extend"
  | IncompatibleFunctionIndexer -> "incompatible-function-indexer"
  | IncompatibleIndexer -> "incompatible-indexer"
  | IncompatibleReturn -> "incompatible-return"
  | IncompatibleType -> "incompatible-type"
  | IncompatibleTypeArg -> "incompatible-type-arg"
  | IncompatibleTypeGuard -> "incompatible-type-guard"
  | IncompatibleUse -> "incompatible-use"
  | IncompatibleVariance -> "incompatible-variance"
  | InvalidCallUtil -> "invalid-call-util"
  | InvalidCatchParameterAnnotation -> "invalid-catch-parameter-annotation"
  | InvalidCharsetTypeArg -> "invalid-charset-type-arg"
  | InvalidCompare -> "invalid-compare"
  | InvalidComputedProp -> "invalid-computed-prop"
  | InvalidConstructor -> "invalid-constructor"
  | InvalidEnumAccess -> "invalid-enum-access"
  | InvalidExact -> "invalid-exact"
  | InvalidExhaustiveCheck -> "invalid-exhaustive-check"
  | InvalidExport -> "invalid-export"
  | InvalidExportsTypeArg -> "invalid-exports-type-arg"
  | InvalidFlowModeDecl -> "invalid-flow-mode"
  | InvalidGraphQL -> "invalid-graphql"
  | InvalidExportedAnnotation -> "invalid-exported-annotation"
  | InvalidExtends -> "invalid-extends"
  | InvalidIdx -> "invalid-idx"
  | InvalidImportStarUse -> "invalid-import-star-use"
  | InvalidImportType -> "invalid-import-type"
  | InvalidInfer -> "invalid-infer"
  | InvalidInLhs -> "invalid-in-lhs"
  | InvalidInRhs -> "invalid-in-rhs"
  | InvalidJsxDecl -> "invalid-jsx-decl"
  | InvalidJsxRuntimeDecl -> "invalid-jsx-runtime-decl"
  | InvalidLhs -> "invalid-lhs"
  | InvalidMappedType -> "invalid-mapped-type"
  | InvalidModule -> "invalid-module"
  | InvalidObjMap -> "invalid-obj-map"
  | InvalidObjMapi -> "invalid-obj-mapi"
  | InvalidPropertyTypeArg -> "invalid-property-type-arg"
  | InvalidPropType -> "invalid-prop-type"
  | InvalidReactConfig -> "invalid-react-config"
  | InvalidRendersTypeArgument -> "invalid-render"
  | InvalidSupportsPlatformDecl -> "invalid-supports-platform"
  | InvalidTupleArity -> "invalid-tuple-arity"
  | InvalidTupleIndex -> "invalid-tuple-index"
  | InvalidTupleMap -> "invalid-tuple-map"
  | InvalidTypeArg -> "invalid-type-arg"
  | InvalidTypeCastSyntax -> "invalid-type-cast-syntax"
  | InvalidTypeOf -> "invalid-typeof"
  | InvalidTempType -> "invalid-temp-type"
  | LintSetting -> "lint-setting"
  | MalformedPackage -> "malformed-package"
  | MethodUnbinding -> "method-unbinding"
  | MissingAnnot -> "missing-annot"
  | MissingLocalAnnot -> "missing-local-annot"
  | MissingThisAnnot -> "missing-this-annot"
  | MissingArg -> "missing-arg"
  | MissingExport -> "missing-export"
  | MissingPlatformSupport -> "missing-platform-support"
  | MissingTypeArg -> "missing-type-arg"
  | MixedImportAndRequire -> "mixed-import-and-require"
  | ModuleTypeConflict -> "module-type-conflict"
  | NameAlreadyBound -> "name-already-bound"
  | NonConstVarExport -> "non-const-var-export"
  | NonpolymorphicTypeApp -> "nonpolymorphic-type-app"
  | NonpolymorphicTypeArg -> "nonpolymorphic-type-arg"
  | NonstrictImport -> "nonstrict-import"
  | NotAClass -> "not-a-class"
  | NotAComponent -> "not-a-component"
  | NotAFunction -> "not-a-function"
  | NotAnArray -> "not-an-array"
  | NotAnObject -> "not-an-object"
  | NotIterable -> "not-iterable"
  | ObjectThisReference -> "object-this-reference"
  | InvalidRef -> "invalid-ref"
  | PropMissing -> "prop-missing"
  | ReassignConst -> "reassign-const"
  | ReassignEnum -> "reassign-enum"
  | ReassignImport -> "reassign-import"
  | ReferenceBeforeDeclaration -> "reference-before-declaration"
  | RequireExplicitEnumSwitchCases -> "require-explicit-enum-switch-cases"
  | SignatureVerificationFailure -> "signature-verification-failure"
  | SketchyNullBool -> "sketchy-null-bool"
  | SketchyNullMixed -> "sketchy-null-mixed"
  | SketchyNullNumber -> "sketchy-null-number"
  | SketchyNullBigInt -> "sketchy-null-bigint"
  | SketchyNullString -> "sketchy-null-string"
  | SketchyNumberAnd -> "sketchy-number-and"
  | Speculation -> "speculation"
  | SpeculationAmbiguous -> "speculation-ambiguous"
  | TSSyntax -> "ts-syntax"
  | ThisInExportedFunction -> "this-in-exported-function"
  | TupleInvalidTypeSpread -> "tuple-invalid-type-spread"
  | TupleRequiredAfterOptional -> "tuple-required-after-optional"
  | TypeAsValue -> "type-as-value"
  | UnclearAddition -> "unclear-addition"
  | UnclearType -> "unclear-type"
  | UnderconstrainedImplicitInstantiation -> "underconstrained-implicit-instantiation"
  | UninitializedInstanceProperty -> "uninitialized-instance-property"
  | UnionUnoptimizable -> "union-unoptimizable"
  | UnnecessaryInvariant -> "unnecessary-invariant"
  | UnnecessaryOptionalChain -> "unnecessary-optional-chain"
  | UnnecessaryDeclareTypeOnlyExport -> "unnecessary-declare-type-only-export"
  | UnreachableCode -> "unreachable-code"
  | UnsafeAddition -> "unsafe-addition"
  | UnsafeArith -> "unsafe-arithmetic"
  | UnsafeGettersSetters -> "unsafe-getters-setters"
  | UnsupportedSyntax -> "unsupported-syntax"
  | UnsupportedVarianceAnnotation -> "unsupported-variance-annotation"
  | UntypedImport -> "untyped-import"
  | UntypedTypeImport -> "untyped-type-import"
  | ValueAsType -> "value-as-type"
  | InvalidDeclaration -> "invalid-declaration"
  | DefinitionCycle -> "definition-cycle"
  | RecursiveDefinition -> "recursive-definition"
  | LogicalAssignmentOperatorsNotSupported -> "logical-assignment-operators-not-supported"
  | UnusedPromise -> "unused-promise"
  | ReactIntrinsicOverlap -> "react-intrinsic-overlap"
  | NestedComponent -> "nested-component"
  | BigIntRShift3 -> "bigint-unsigned-right-shift"
  | BigIntNumCoerce -> "bigint-num-coerce"
  | InvalidComponentProp -> "invalid-component-prop"
