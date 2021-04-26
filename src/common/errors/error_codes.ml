(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type error_code =
  | AmbiguousObjectType
  | BigIntUnsupported
  | CannotDelete
  | CannotImplement
  | CannotInferType
  | CannotRead
  | CannotReassignExport
  | CannotResolveModule
  | CannotResolveName
  | CannotSpreadIndexer
  | CannotSpreadInexact
  | CannotSpreadInterface
  | CannotWrite
  | CannotWriteEnum
  | DefaultImportAccess
  | DeprecatedType
  | DeprecatedUtility
  | DuplicateEnumInit
  | DuplicateFlowDecl
  | DuplicateJsxDecl
  | DuplicateModule
  | DuplicateProvideModuleDecl
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
  | IllegalThisAnnot
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
  | IncompatibleShape
  | IncompatibleTrust
  | IncompatibleType
  | IncompatibleTypeArg
  | IncompatibleUse
  | IncompatibleVariance
  | IndexedAccessNotEnabled
  | InvalidCallUtil
  | InvalidCharsetTypeArg
  | InvalidCompare
  | InvalidComputedProp
  | InvalidEnumAccess
  | InvalidExact
  | InvalidExhaustiveCheck
  | InvalidExport
  | InvalidExportsTypeArg
  | InvalidIdx
  | InvalidImportStarUse
  | InvalidImportType
  | InvalidInLhs
  | InvalidInRhs
  | InvalidJsxDecl
  | InvalidLhs
  | InvalidModule
  | InvalidObjMap
  | InvalidObjMapi
  | InvalidPredTypeArg
  | InvalidPrivateTypeArg
  | InvalidPropertyTypeArg
  | InvalidPropType
  | InvalidReactConfig
  | InvalidReactCreateClass
  | InvalidRefineTypeArg
  | InvalidTrustedTypeArg
  | InvalidTupleArity
  | InvalidTupleIndex
  | InvalidTupleMap
  | InvalidTypeArg
  | InvalidTypeOf
  | InvalidTempType
  | LintSetting
  | MalformedPackage
  | MissingAnnot
  | MissingLocalAnnot
  | MissingArg
  | MissingExport
  | MissingTypeArg
  | MixedImportAndRequire
  | ToplevelLibraryImport
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
  | PropMissing
  | ReassignConst
  | ReassignEnum
  | ReassignImport
  | ReferenceBeforeDeclaration
  | RefineAsValue
  | SignatureVerificationFailure
  | SketchyNullBool
  | SketchyNullMixed
  | SketchyNullNumber
  | SketchyNullString
  | SketchyNumberAnd
  | Speculation
  | SpeculationAmbiguous
  | SuperOutsideMethod
  | ThisInExportedFunction
  | TypeAsValue
  | UnclearAddition
  | UnclearType
  | UnderconstrainedImplicitInstantiation
  | UninitializedInstanceProperty
  | UnnecessaryInvariant
  | UnnecessaryOptionalChain
  | UnreachableCode
  | UnsafeAddition
  | UnsafeGettersSetters
  | UnsupportedSyntax
  | UntypedImport
  | UntypedTypeImport
  | ValueAsType

let code_of_lint : Lints.lint_kind -> error_code = function
  | Lints.UntypedTypeImport -> UntypedTypeImport
  | Lints.UntypedImport -> UntypedImport
  | Lints.NonstrictImport -> NonstrictImport
  | Lints.UnclearType -> UnclearType
  | Lints.DeprecatedType -> DeprecatedType
  | Lints.DeprecatedUtility -> DeprecatedUtility
  | Lints.UnsafeGettersSetters -> UnsafeGettersSetters
  | Lints.UnnecessaryOptionalChain -> UnnecessaryOptionalChain
  | Lints.UnnecessaryInvariant -> UnnecessaryInvariant
  | Lints.SignatureVerificationFailure -> SignatureVerificationFailure
  | Lints.ImplicitInexactObject -> ImplicitInexactObject
  | Lints.UninitializedInstanceProperty -> UninitializedInstanceProperty
  | Lints.AmbiguousObjectType -> AmbiguousObjectType
  | Lints.SketchyNumber Lints.SketchyNumberAnd -> SketchyNumberAnd
  | Lints.SketchyNull (Lints.SketchyNullBool | Lints.SketchyNullEnumBool) -> SketchyNullBool
  | Lints.SketchyNull (Lints.SketchyNullString | Lints.SketchyNullEnumString) -> SketchyNullString
  | Lints.SketchyNull (Lints.SketchyNullNumber | Lints.SketchyNullEnumNumber) -> SketchyNullNumber
  | Lints.SketchyNull Lints.SketchyNullMixed -> SketchyNullMixed
  | Lints.DefaultImportAccess -> DefaultImportAccess
  | Lints.InvalidImportStarUse -> InvalidImportStarUse
  | Lints.NonConstVarExport -> NonConstVarExport
  | Lints.ThisInExportedFunction -> ThisInExportedFunction
  | Lints.MixedImportAndRequire -> MixedImportAndRequire
  | Lints.ExportRenamedDefault -> ExportRenamedDefault

let string_of_code : error_code -> string = function
  | AmbiguousObjectType -> "ambiguous-object-type"
  | BigIntUnsupported -> "bigint-unsupported"
  | CannotDelete -> "cannot-delete"
  | CannotImplement -> "cannot-implement"
  | CannotInferType -> "cannot-infer-type"
  | CannotRead -> "cannot-read"
  | CannotReassignExport -> "cannot-reassign-export"
  | CannotResolveModule -> "cannot-resolve-module"
  | CannotResolveName -> "cannot-resolve-name"
  | CannotSpreadIndexer -> "cannot-spread-indexer"
  | CannotSpreadInexact -> "cannot-spread-inexact"
  | CannotSpreadInterface -> "cannot-spread-interface"
  | CannotWrite -> "cannot-write"
  | CannotWriteEnum -> "cannot-write-enum"
  | DefaultImportAccess -> "default-import-access"
  | DeprecatedType -> "deprecated-type"
  | DeprecatedUtility -> "deprecated-utility"
  | DuplicateEnumInit -> "duplicate-enum-init"
  | DuplicateFlowDecl -> "duplicate-flow-decl"
  | DuplicateJsxDecl -> "duplicate-jsx-decl"
  | DuplicateModule -> "duplicate-module"
  | DuplicateProvideModuleDecl -> "duplicate-provide-module-decl"
  | EnumValueAsType -> "enum-value-as-type"
  | EscapedGeneric -> "escaped-generic"
  | ExponentialSpread -> "exponential-spread"
  | ExportRenamedDefault -> "export-renamed-default"
  | ExportValueAsType -> "export-value-as-type"
  | ExtraArg -> "extra-arg"
  | ExtraTypeArg -> "extra-type-arg"
  | FunctionPredicate -> "function-predicate"
  | IllegalEnum -> "illegal-enum"
  | IllegalThisAnnot -> "illegal-this-annot"
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
  | IncompatibleShape -> "incompatible-shape"
  | IncompatibleTrust -> "incompatible-trust"
  | IncompatibleType -> "incompatible-type"
  | IncompatibleTypeArg -> "incompatible-type-arg"
  | IncompatibleUse -> "incompatible-use"
  | IncompatibleVariance -> "incompatible-variance"
  | IndexedAccessNotEnabled -> "indexed-access-off"
  | InvalidCallUtil -> "invalid-call-util"
  | InvalidCharsetTypeArg -> "invalid-charset-type-arg"
  | InvalidCompare -> "invalid-compare"
  | InvalidComputedProp -> "invalid-computed-prop"
  | InvalidEnumAccess -> "invalid-enum-access"
  | InvalidExact -> "invalid-exact"
  | InvalidExhaustiveCheck -> "invalid-exhaustive-check"
  | InvalidExport -> "invalid-export"
  | InvalidExportsTypeArg -> "invalid-exports-type-arg"
  | InvalidIdx -> "invalid-idx"
  | InvalidImportStarUse -> "invalid-import-star-use"
  | InvalidImportType -> "invalid-import-type"
  | InvalidInLhs -> "invalid-in-lhs"
  | InvalidInRhs -> "invalid-in-rhs"
  | InvalidJsxDecl -> "invalid-jsx-decl"
  | InvalidLhs -> "invalid-lhs"
  | InvalidModule -> "invalid-module"
  | InvalidObjMap -> "invalid-obj-map"
  | InvalidObjMapi -> "invalid-obj-mapi"
  | InvalidPredTypeArg -> "invalid-pred-type-arg"
  | InvalidPrivateTypeArg -> "invalid-private-type-arg"
  | InvalidPropertyTypeArg -> "invalid-property-type-arg"
  | InvalidPropType -> "invalid-prop-type"
  | InvalidReactConfig -> "invalid-react-config"
  | InvalidReactCreateClass -> "invalid-react-create-class"
  | InvalidRefineTypeArg -> "invalid-refine-type-arg"
  | InvalidTrustedTypeArg -> "invalid-trusted-type-arg"
  | InvalidTupleArity -> "invalid-tuple-arity"
  | InvalidTupleIndex -> "invalid-tuple-index"
  | InvalidTupleMap -> "invalid-tuple-map"
  | InvalidTypeArg -> "invalid-type-arg"
  | InvalidTypeOf -> "invalid-typeof"
  | InvalidTempType -> "invalid-temp-type"
  | LintSetting -> "lint-setting"
  | MalformedPackage -> "malformed-package"
  | MissingAnnot -> "missing-annot"
  | MissingLocalAnnot -> "missing-local-annot"
  | MissingArg -> "missing-arg"
  | MissingExport -> "missing-export"
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
  | PropMissing -> "prop-missing"
  | ReassignConst -> "reassign-const"
  | ReassignEnum -> "reassign-enum"
  | ReassignImport -> "reassign-import"
  | ReferenceBeforeDeclaration -> "reference-before-declaration"
  | RefineAsValue -> "refine-as-value"
  | SignatureVerificationFailure -> "signature-verification-failure"
  | SketchyNullBool -> "sketchy-null-bool"
  | SketchyNullMixed -> "sketchy-null-mixed"
  | SketchyNullNumber -> "sketchy-null-number"
  | SketchyNullString -> "sketchy-null-string"
  | SketchyNumberAnd -> "sketchy-number-and"
  | Speculation -> "speculation"
  | SpeculationAmbiguous -> "speculation-ambiguous"
  | SuperOutsideMethod -> "super-outside-method"
  | ThisInExportedFunction -> "this-in-exported-function"
  | ToplevelLibraryImport -> "toplevel-library-import"
  | TypeAsValue -> "type-as-value"
  | UnclearAddition -> "unclear-addition"
  | UnclearType -> "unclear-type"
  | UnderconstrainedImplicitInstantiation -> "underconstrained-implicit-instantiation"
  | UninitializedInstanceProperty -> "uninitialized-instance-property"
  | UnnecessaryInvariant -> "unnecessary-invariant"
  | UnnecessaryOptionalChain -> "unnecessary-optional-chain"
  | UnreachableCode -> "unreachable-code"
  | UnsafeAddition -> "unsafe-addition"
  | UnsafeGettersSetters -> "unsafe-getters-setters"
  | UnsupportedSyntax -> "unsupported-syntax"
  | UntypedImport -> "untyped-import"
  | UntypedTypeImport -> "untyped-type-import"
  | ValueAsType -> "value-as-type"
