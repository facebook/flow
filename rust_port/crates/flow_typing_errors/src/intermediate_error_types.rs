/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;
use std::str::FromStr;
use std::sync::Arc;

use dupe::Dupe;
pub use flow_common::error_ref::ExpressionFunctionKind;
pub use flow_common::error_ref::ExpressionReferenceData;
pub use flow_common::error_ref::ExpressionReferenceKind;
pub use flow_common::error_ref::FunctionReferenceKind;
use flow_common::flow_import_specifier::Userland;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::VirtualReason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::refinement_invalidation;
use flow_common_errors::error_codes::ErrorCode;
use flow_common_errors::error_utils::ErrorKind;
use flow_common_ty::ty::ALocTy;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_env_builder::env_api::AnnotLoc;
use flow_lint_settings::lint_settings::LintParseError;
use flow_lint_settings::lints::PropertyAssignmentKind;
use flow_lint_settings::lints::SketchyNullKind;
use flow_parser::ast;
use flow_parser::ast::VariableKind;
use flow_parser::ast::types::RendersVariant;
use flow_parser::ast::types::TypeGuardKind;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::parse_error::ParseError;
use flow_parser_utils::graphql::GraphqlError;
use flow_type_sig::signature_error::SignatureError;
use flow_typing_type::type_::ImplicitInstantiationReferenceKind;
use flow_typing_type::type_::MergedDeclarationConflict;
use flow_typing_type::type_::UnionEnum;
use flow_typing_type::type_::aconstraint::AnnotationInferenceOperation;
use flow_typing_type::type_::type_or_type_desc::TypeOrTypeDescT as TypeOrTypeDesc;
use flow_typing_type::type_::union_rep::OptimizedError;
use vec1::Vec1;

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum AssignedConstLikeBindingType {
    ClassNameBinding,
    FunctionNameBinding,
    DeclaredFunctionNameBinding,
    ComponentNameBinding,
    RecordNameBinding,
}

impl AssignedConstLikeBindingType {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::ClassNameBinding => "class",
            Self::FunctionNameBinding => "function",
            Self::DeclaredFunctionNameBinding => "declared function",
            Self::ComponentNameBinding => "component",
            Self::RecordNameBinding => "record",
        }
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum DocblockError {
    MultipleFlowAttributes,
    InvalidFlowMode(FlowSmolStr),
    MultipleJSXAttributes,
    InvalidJSXAttribute(Option<FlowSmolStr>),
    MultipleJSXRuntimeAttributes,
    InvalidJSXRuntimeAttribute,
    InvalidSupportsPlatform(FlowSmolStr),
    DisallowedSupportsPlatform,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ExactnessErrorKind {
    UnexpectedIndexer,
    UnexpectedInexact,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum DeclareComponentInvalidParamKind {
    DeclareComponentParamAsBinding,
    DeclareComponentParamDefaultValue,
    DeclareComponentParamMissingAnnotation,
    DeclareComponentParamStringLiteralWithoutAs,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ExpectedModulePurpose {
    ReactModuleForJSXFragment,
    ReactModuleForReactClassComponent,
    ReactModuleForReactMixedElementType,
    ReactModuleForReactNodeType,
    ReactModuleForReactRefSetterType,
    ReactModuleForReactElementRefType,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExponentialSpreadReasonGroup<L: Dupe> {
    pub first_reason: VirtualReason<L>,
    pub second_reason: Option<VirtualReason<L>>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ContextDependentUnsupportedStatement {
    ToplevelLibraryImport,
    NonLibdefToplevelDeclareModule,
    UnsupportedStatementInLibdef(FlowSmolStr),
    UnsupportedStatementInDeclareModule(FlowSmolStr),
    UnsupportedStatementInDeclareNamespace(FlowSmolStr),
    UnsupportedStatementInDeclareGlobal(FlowSmolStr),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum InternalType {
    DollarReactDeepReadOnly,
    DollarUtilityTypeWithNonDollarAliases(FlowSmolStr),
    ReactDollarUtilityTypesWithNonDollarAliases(FlowSmolStr),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum UnsupportedSyntax {
    AnnotationInsideDestructuring,
    AsConstOnNonLiteral,
    ExistsType,
    MetaPropertyExpression,
    ObjectPropertyGetSet,
    ObjectPropertyComputedGetSet,
    InvariantSpreadArgument,
    ClassPropertyLiteral,
    ClassStaticBlock,
    ClassDeclareMethod,
    ClassIndexSignature,
    RequireDynamicArgument,
    CatchParameterDeclaration,
    DestructuringObjectPropertyInvalidLiteral,
    DestructuringExpressionPattern,
    PredicateFunction,
    MatchExpression,
    MatchStatement,
    MatchInstancePattern,
    MultipleIndexers,
    MultipleProtos,
    ExplicitCallAfterProto,
    ExplicitProtoAfterCall,
    SpreadArgument,
    ImportDynamic,
    ImportDynamicArgument,
    IllegalName,
    UserDefinedTypeGuards { kind: TypeGuardKind },
    UnsupportedInternalSlot { name: FlowSmolStr, static_: bool },
    ContextDependentUnsupportedStatement(ContextDependentUnsupportedStatement),
    WithStatement,
    ComponentSyntax,
    AsyncComponentSyntax,
    AsyncHookSyntax,
    DeclareGlobal,
    NonnullAssertion,
    Records,
    DeclareClassMethodMissingReturnType,
    DeclareVariableNonLiteralInit,
    DeclareVariableDestructuring,
    DeclareVariableMissingAnnotationOrInit,
    DeclareVariableAnnotationAndInit,
    DeclareClassProperty(DeclareClassPropKind),
    TSLibSyntax(TsLibSyntaxKind),
    ExportTypeSpecifierInExportType,
    UniqueSymbolPosition,
    UniqueSymbolNotConst,
    UniqueSymbolNotReadOnly,
    UniqueSymbolNotStaticReadOnly,
    UniqueSymbolLoopBinding,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum DeclareClassPropKind {
    AnnotationAndInit,
    MissingAnnotationOrInit,
    NonLiteralInit,
    InitWithoutReadonly,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum TsLibSyntaxKind {
    DeclarationWithoutDeclare,
    ImportTypeAnnotation,
    DeclareExportNamespace,
    ExportAssignment,
    ExportTypeSpecifier,
    ImportEqualsDeclaration,
    ImportEqualsQualifiedName,
    DeclareVariableMultipleDeclarators,
    DeclareVariableLiteralInit,
    TemplateLiteralType,
    ConstructorType,
    UniqueSymbolType,
    TypeofImport,
    ImplementsDottedPath,
    OptionalClassProperty,
    OptionalShorthandMethod,
    AnonymousDefaultExportFunction,
    MappedTypeKeyRemapping,
    MappedTypeKeyRemappingOnArraySource,
    ReadonlyMappedTypeVarianceOp,
    OptionalUnlabeledTupleElement,
    OptionalIndexer,
    NamespaceExportDeclaration,
    PrivateClassField,
    GenericTaggedTemplate,
    TypeofThis,
    PropertyValueInitializer,
    ClassExtendsCall,
    OverrideModifier,
    Satisfies,
}

// A TypeScript enum member that Flow parses but TypeScript rejects.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum TsEnumInvalidMemberKind {
    // A boolean or bigint initializer (TS enum members must be number or string).
    TSEnumMemberInvalidLiteral,
    // A defaulted member that cannot be auto-numbered (its predecessor is not a
    // numeric constant) in a non-ambient enum.
    TSEnumMemberMissingInitializer,
    // A member with a numeric name, e.g. `enum E { "1" = 5 }` (TS2452).
    TSEnumMemberNumericName,
}

// Enum-level syntax that Flow Enums allow but a TypeScript enum (in a .ts/.d.ts
// file) does not.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum TsEnumInvalidSyntaxKind {
    // Unknown members (`...`).
    TSEnumUnknownMembers,
    // An explicit representation type (`enum E of string {...}`).
    TSEnumExplicitType,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum SubComponentOfInvariantSubtypingError {
    ObjectProps(Vec<Name>),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum MatchInvalidCaseSyntax<L: Dupe> {
    InvalidMatchCaseMultiple {
        invalid_prefix_case_locs: Vec<L>,
        invalid_infix_colon_locs: Vec<L>,
        invalid_suffix_semicolon_locs: Vec<L>,
    },
    InvalidMatchCasePrefixCase,
    InvalidMatchCaseInfixColon,
    InvalidMatchCaseSuffixSemicolon,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum RecordDeclarationInvalidSyntax<L: Dupe> {
    InvalidRecordDeclarationSyntaxMultiple {
        invalid_infix_equals_loc: Option<L>,
        invalid_variance_locs: Vec<L>,
        invalid_optional_locs: Vec<L>,
        invalid_suffix_semicolon_locs: Vec<L>,
    },
    InvalidRecordDeclarationSyntaxVariance,
    InvalidRecordDeclarationSyntaxOptional,
    InvalidRecordDeclarationSyntaxSuffixSemicolon,
    InvalidRecordDeclarationSyntaxInfixEquals,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum AbstractErrorKind<L: Dupe> {
    AbstractClassInstantiation,
    AbstractMemberNotImplemented {
        class_name: Option<FlowSmolStr>,
        member_name: FlowSmolStr,
        member_def_loc: L,
    },
    AbstractMemberOnNonAbstractClass {
        member_name: FlowSmolStr,
    },
    AbstractPrivateMember {
        member_name: FlowSmolStr,
    },
    AbstractSuperCall {
        member_name: FlowSmolStr,
    },
    AbstractConstructorAssignedToNonAbstract,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum OverrideErrorKind<L: Dupe> {
    OverrideWithoutExtends {
        class_name: Option<FlowSmolStr>,
        member_name: FlowSmolStr,
    },
    OverrideOfNonInheritedMember {
        class_name: Option<FlowSmolStr>,
        base_class_name: Option<FlowSmolStr>,
        member_name: FlowSmolStr,
    },
    ImplicitOverrideMissingModifier {
        class_name: Option<FlowSmolStr>,
        base_class_name: Option<FlowSmolStr>,
        member_name: FlowSmolStr,
        inherited_def_loc: L,
    },
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum InvalidRenderTypeKind<T> {
    InvalidRendersNullVoidFalse,
    InvalidRendersIterable,
    InvalidRendersStructural(T),
    InvalidRendersNonNominalElement(T),
    InvalidRendersGenericT,
    UncategorizedInvalidRenders,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ConstantConditionKind {
    ConstCondGeneral,
    UnawaitedPromise,
    UncalledFunction,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ConstantConditionWarning<L: Dupe> {
    Definite,
    Likely { suggested_loc: Option<L> },
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum StrictComparisonInfo<L, T> {
    General { left: T, right: T },
    Null { null_loc: L, other: T },
    Empty { empty: T },
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ClassKind {
    Class,
    Record,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum MatchObjPatternKind {
    Object,
    Instance,
}

impl MatchObjPatternKind {
    pub fn to_string(self) -> &'static str {
        match self {
            Self::Object => "object pattern",
            Self::Instance => "instance pattern",
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum IncorrectType {
    Partial,
    Shape,
    TSReadonly,
    TSReadonlyArray,
    TSReadonlyMap,
    TSReadonlySet,
    TSNonNullable,
    Values,
    DollarNonMaybeType,
    DollarReadOnly,
    DollarReadOnlyArray,
    DollarReadOnlyMap,
    DollarReadOnlySet,
    DollarReadOnlyWeakMap,
    DollarReadOnlyWeakSet,
    DollarKeys,
    DollarValues,
    Mixed,
}

impl IncorrectType {
    pub fn is_type_reserved(self) -> bool {
        matches!(
            self,
            Self::DollarNonMaybeType
                | Self::TSNonNullable
                | Self::DollarReadOnly
                | Self::TSReadonly
                | Self::DollarKeys
                | Self::Values
                | Self::DollarValues
        )
    }

    pub fn incorrect_of_kind(self) -> &'static str {
        match self {
            Self::Partial => "$Partial",
            Self::Shape => "$Shape",
            Self::TSReadonly => "Readonly",
            Self::TSReadonlyArray => "ReadonlyArray",
            Self::TSReadonlyMap => "ReadonlyMap",
            Self::TSReadonlySet => "ReadonlySet",
            Self::TSNonNullable => "NonNullable",
            Self::Values => "Values",
            Self::DollarNonMaybeType => "$NonMaybeType",
            Self::DollarReadOnly => "$ReadOnly",
            Self::DollarReadOnlyArray => "$ReadOnlyArray",
            Self::DollarReadOnlyMap => "$ReadOnlyMap",
            Self::DollarReadOnlySet => "$ReadOnlySet",
            Self::DollarReadOnlyWeakMap => "$ReadOnlyWeakMap",
            Self::DollarReadOnlyWeakSet => "$ReadOnlyWeakSet",
            Self::DollarKeys => "$Keys",
            Self::DollarValues => "$Values",
            Self::Mixed => "mixed",
        }
    }

    pub fn replacement_of_kind(self) -> &'static str {
        match self {
            Self::Partial => "Partial",
            Self::Shape => "Partial",
            Self::TSReadonly => "Readonly",
            Self::TSReadonlyArray => "ReadonlyArray",
            Self::TSReadonlyMap => "ReadonlyMap",
            Self::TSReadonlySet => "ReadonlySet",
            Self::TSNonNullable => "NonNullable",
            Self::Values => "Values",
            Self::DollarNonMaybeType => "NonNullable",
            Self::DollarReadOnly => "Readonly",
            Self::DollarReadOnlyArray => "ReadonlyArray",
            Self::DollarReadOnlyMap => "ReadonlyMap",
            Self::DollarReadOnlySet => "ReadonlySet",
            Self::DollarReadOnlyWeakMap => "ReadonlyWeakMap",
            Self::DollarReadOnlyWeakSet => "ReadonlyWeakSet",
            Self::DollarKeys => "keyof",
            Self::DollarValues => "Values",
            Self::Mixed => "unknown",
        }
    }

    pub fn error_type_of_kind(self) -> IncorrectTypeErrorType {
        match self {
            Self::Partial
            | Self::Shape
            | Self::DollarNonMaybeType
            | Self::DollarReadOnly
            | Self::DollarReadOnlyArray
            | Self::DollarReadOnlyMap
            | Self::DollarReadOnlySet
            | Self::DollarReadOnlyWeakMap
            | Self::DollarReadOnlyWeakSet
            | Self::DollarKeys
            | Self::DollarValues
            | Self::Mixed => IncorrectTypeErrorType::DeprecatedUtility,
            Self::TSReadonly
            | Self::TSReadonlyArray
            | Self::TSReadonlyMap
            | Self::TSReadonlySet
            | Self::TSNonNullable
            | Self::Values => IncorrectTypeErrorType::TSType,
        }
    }
}

impl FromStr for IncorrectType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "$NonMaybeType" => Ok(Self::DollarNonMaybeType),
            "NonNullable" => Ok(Self::TSNonNullable),
            "$ReadOnly" => Ok(Self::DollarReadOnly),
            "Readonly" => Ok(Self::TSReadonly),
            "$Keys" => Ok(Self::DollarKeys),
            "Values" => Ok(Self::Values),
            "$Values" => Ok(Self::DollarValues),
            _ => Err(()),
        }
    }
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum IncorrectTypeErrorType {
    DeprecatedUtility,
    TSType,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum InvalidObjKey {
    Other,
    NumberNonInt,
    NumberTooLarge,
    NumberTooSmall,
    /// A computed key whose type is not a single literal, so it names no one
    /// property.
    ComputedNotLiteral,
    /// A computed key whose name also names a type, which is almost always an
    /// index signature written without a label.
    ComputedTypeName,
}

impl InvalidObjKey {
    pub fn kind_of_num_value(value: f64) -> Self {
        use flow_common::js_number::MAX_SAFE_INTEGER;
        use flow_common::js_number::MIN_SAFE_INTEGER;

        if value.fract() != 0.0 {
            Self::NumberNonInt
        } else if value > MAX_SAFE_INTEGER {
            Self::NumberTooLarge
        } else if value < MIN_SAFE_INTEGER {
            Self::NumberTooSmall
        } else {
            Self::Other
        }
    }

    pub fn str_of_kind(self) -> &'static str {
        match self {
            Self::Other => "other",
            Self::NumberNonInt => "number non-int",
            Self::NumberTooLarge => "number too large",
            Self::NumberTooSmall => "number too small",
            Self::ComputedNotLiteral => "computed not literal",
            Self::ComputedTypeName => "computed type name",
        }
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ExplanationWithLazyParts<L: Dupe> {
    LazyExplanationInvariantSubtypingDueToMutableArray {
        lower_array_loc: L,
        upper_array_loc: L,
        lower_array_desc: TypeOrTypeDesc<L>,
        upper_array_desc: TypeOrTypeDesc<L>,
        upper_array_reason: VirtualReason<L>,
    },
    LazyExplanationInvariantSubtypingDueToMutableProperty {
        lower_obj_loc: L,
        upper_obj_loc: L,
        lower_obj_desc: TypeOrTypeDesc<L>,
        upper_obj_desc: TypeOrTypeDesc<L>,
        upper_object_reason: VirtualReason<L>,
        property_name: Option<FlowSmolStr>,
    },
    LazyExplanationInvariantSubtypingDueToMutableProperties {
        lower_obj_loc: L,
        upper_obj_loc: L,
        lower_obj_desc: TypeOrTypeDesc<L>,
        upper_obj_desc: TypeOrTypeDesc<L>,
        upper_object_reason: VirtualReason<L>,
        properties: Vec<Name>,
    },
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExplanationConstrainedAssignData<L: Dupe> {
    pub name: FlowSmolStr,
    pub declaration: L,
    pub providers: Arc<[L]>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExplanationCustomErrorData<L: Dupe> {
    pub name: FlowSmolStr,
    pub custom_error_loc: L,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExplanationInvariantSubtypingDueToMutableArrayData<L: Dupe> {
    pub lower_array_loc: L,
    pub upper_array_loc: L,
    pub lower_array_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_array_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_array_reason: VirtualReason<L>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExplanationInvariantSubtypingDueToMutablePropertyData<L: Dupe> {
    pub lower_obj_loc: L,
    pub upper_obj_loc: L,
    pub lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_object_reason: VirtualReason<L>,
    pub property_name: Option<FlowSmolStr>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExplanationInvariantSubtypingDueToMutablePropertiesData<L: Dupe> {
    pub lower_obj_loc: L,
    pub upper_obj_loc: L,
    pub lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_object_reason: VirtualReason<L>,
    pub properties: Vec<Name>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExplanationPropertyMissingDueToNeutralOptionalPropertyData<L: Dupe> {
    pub props_plural: bool,
    pub lower_obj_loc: L,
    pub upper_obj_loc: L,
    pub lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_object_reason: VirtualReason<L>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ExplanationAdditionalUnionMembersData<L: Dupe> {
    pub left: VirtualReason<L>,
    pub right: VirtualReason<L>,
    pub members: Vec<FlowSmolStr>,
    pub extra_number: i32,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum Explanation<L: Dupe> {
    ExplanationAbstractEnumCasting,
    ExplanationArrayInvariantTyping,
    ExplanationConstrainedAssign(Box<ExplanationConstrainedAssignData<L>>),
    ExplanationConcreteEnumCasting {
        representation_type: FlowSmolStr,
    },
    ExplanationCustomError(Box<ExplanationCustomErrorData<L>>),
    ExplanationFunctionsWithStaticsToObject,
    ExplanationInvariantSubtypingDueToMutableArray(
        Box<ExplanationInvariantSubtypingDueToMutableArrayData<L>>,
    ),
    ExplanationInvariantSubtypingDueToMutableProperty(
        Box<ExplanationInvariantSubtypingDueToMutablePropertyData<L>>,
    ),
    ExplanationInvariantSubtypingDueToMutableProperties(
        Box<ExplanationInvariantSubtypingDueToMutablePropertiesData<L>>,
    ),
    ExplanationMultiplatform,
    ExplanationPropertyInvariantTyping,
    ExplanationPropertyMissingDueToNeutralOptionalProperty(
        Box<ExplanationPropertyMissingDueToNeutralOptionalPropertyData<L>>,
    ),
    ExplanationReactComponentPropsDeepReadOnly(L),
    ExplanationReactHookArgsDeepReadOnly(L),
    ExplanationReactHookIncompatibleWithEachOther,
    ExplanationReactHookIncompatibleWithNormalFunctions,
    ExplanationReactHookReturnDeepReadOnly(L),
    ExplanationTypeGuardPositiveConsistency {
        return_: VirtualReason<L>,
        param: VirtualReason<L>,
        guard_type: VirtualReason<L>,
        is_return_false_statement: bool,
    },
    ExplanationAdditionalUnionMembers(Box<ExplanationAdditionalUnionMembersData<L>>),
    ExplanationObjectLiteralNeedsRecordSyntax {
        record_name: FlowSmolStr,
        obj_reason: VirtualReason<L>,
    },
    ExplanationStringCasingMustBeCanonical {
        kind_name: FlowSmolStr,
    },
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum AccessChainSegment {
    PropSegment(Name),
    TupleIndexSegment(i32),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum Frame<L: Dupe> {
    FrameAnonymous,
    FrameAccessChain {
        chain: Vec1<AccessChainSegment>,
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameArrayElement {
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameCallableSignature {
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameEnumRepresentationType,
    FrameFunNthArgument {
        n: i32,
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameFunThisArgument {
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameFunNthParam {
        n: i32,
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameFunThisParam {
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameIndexerProperty {
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameIndexerPropertyKey {
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameTypeArgument(VirtualReason<L>),
    FrameTypeParameterBound(FlowSmolStr),
    FrameTypePredicate,
    FrameReturnValue {
        incompatibility_pair: Option<(VirtualReason<L>, VirtualReason<L>)>,
    },
    FrameUnionRepresentative(VirtualReason<L>),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum RootMessage<L: Dupe> {
    RootCannotAccessIndex {
        index: VirtualReasonDesc<L>,
        object_: VirtualReasonDesc<L>,
    },
    RootCannotAddComputedProperty,
    RootCannotAssign {
        init: VirtualReasonDesc<L>,
        target: Option<VirtualReasonDesc<L>>,
    },
    RootCannotCall(VirtualReasonDesc<L>),
    RootCannotCallStandalone(VirtualReasonDesc<L>),
    RootCannotCallWithNamedParam {
        fn_: VirtualReasonDesc<L>,
        lower: VirtualReasonDesc<L>,
        name: FlowSmolStr,
    },
    RootCannotCallWithNthParam {
        fn_: VirtualReasonDesc<L>,
        lower: VirtualReasonDesc<L>,
        n: i32,
    },
    RootCannotCallObjectAssign(VirtualReasonDesc<L>),
    RootCannotCast {
        lower: VirtualReasonDesc<L>,
        upper: VirtualReasonDesc<L>,
    },
    RootCannotCheckAgainst {
        test: VirtualReasonDesc<L>,
        discriminant: VirtualReason<L>,
    },
    RootCannotCheckAgainstSwitchDiscriminant(L),
    RootCannotCoerce {
        from: VirtualReasonDesc<L>,
        target: VirtualReasonDesc<L>,
    },
    RootCannotConformToCommonInterface {
        originate_from_import: bool,
    },
    RootCannotMergeDeclaration {
        conflict: MergedDeclarationConflict,
        first_decl: VirtualReason<L>,
    },
    RootCannotCreateElement(VirtualReasonDesc<L>),
    RootCannotCreateRecord(VirtualReasonDesc<L>),
    RootCannotDeclareRef,
    RootCannotDeclareTypeGuard {
        type_guard_loc: L,
        fn_: VirtualReason<L>,
    },
    RootCannotDefineClassMethod {
        method_: VirtualReason<L>,
        name: VirtualReasonDesc<L>,
    },
    RootCannotDefineShadowedProtoProperty,
    RootCannotDelete(VirtualReasonDesc<L>),
    RootCannotExpectImplicitReturn {
        upper: VirtualReasonDesc<L>,
        fn_: VirtualReasonDesc<L>,
    },
    RootCannotExtendClass {
        extends: VirtualReason<L>,
        def: VirtualReasonDesc<L>,
    },
    RootCannotGetProp(VirtualReasonDesc<L>),
    RootCannotGetRest(VirtualReasonDesc<L>),
    RootCannotImplementClass {
        implements: VirtualReason<L>,
        def: VirtualReasonDesc<L>,
    },
    RootCannotInitializeField {
        field: VirtualReasonDesc<L>,
        body: VirtualReasonDesc<L>,
    },
    RootCannotInstantiateEval(VirtualReason<L>),
    RootCannotInstantiateRenderType,
    RootCannotInstantiateTypeApp(VirtualReasonDesc<L>),
    RootCannotReturn(VirtualReasonDesc<L>),
    RootCannotShadowProto(VirtualReason<L>),
    RootCannotShadowProtoProperty,
    RootCannotSpread(VirtualReasonDesc<L>),
    RootCannotUpdate(VirtualReasonDesc<L>),
    RootCannotUseInferTypeBound {
        infer: VirtualReasonDesc<L>,
    },
    RootCannotUseTypeGuard {
        guard_type: VirtualReason<L>,
        param_name: FlowSmolStr,
    },
    RootCannotYield(VirtualReasonDesc<L>),
}

pub type UnionEnumMap<L> = BTreeMap<UnionEnum, Vec1<MessageTypeReferenceData<L>>>;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ThisFinderKind {
    This,
    Super,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum PrimitiveKind {
    Boolean,
    Number,
    String,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ObjKind {
    Type,
    Literal,
    Interface,
    DeclareClass,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageAlreadyExhaustivelyCheckOneEnumMemberData<L: Dupe> {
    pub member_name: FlowSmolStr,
    pub prev_check_loc: L,
    pub enum_: MessageTypeReferenceData<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotAccessEnumMemberData<L: Dupe> {
    pub member_name: Option<Name>,
    pub suggestion: Option<FlowSmolStr>,
    pub description: Result<ALocTy, VirtualReasonDesc<L>>,
    pub enum_: MessageTypeReferenceData<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotAddComputedPropertyDueToPotentialOverwriteData<L: Dupe> {
    pub key_loc: L,
    pub overwritten_locs: Vec<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotCallMaybeReactHookData<L: Dupe> {
    pub callee_loc: L,
    pub hooks: Vec<L>,
    pub non_hooks: Vec<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotCompareData<L: Dupe> {
    pub lower: MessageTypeReferenceData<L>,
    pub upper: MessageTypeReferenceData<L>,
    pub strict_comparison_opt: Option<StrictComparisonInfo<L, MessageTypeReferenceData<L>>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotExportRenamedDefaultData {
    pub name: Option<FlowSmolStr>,
    pub is_reexport: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotExhaustivelyCheckAbstractEnumsData<L: Dupe> {
    pub description_name: Option<FlowSmolStr>,
    pub description: Result<ALocTy, VirtualReasonDesc<L>>,
    pub enum_: MessageTypeReferenceData<L>,
    pub enum_name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotExhaustivelyCheckEnumWithUnknownsData<L: Dupe> {
    pub description_name: Option<FlowSmolStr>,
    pub description: Result<ALocTy, VirtualReasonDesc<L>>,
    pub enum_: MessageTypeReferenceData<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotInstantiateObjectUtilTypeWithEnumData<L: Dupe> {
    pub description: Result<ALocTy, VirtualReasonDesc<L>>,
    pub enum_: MessageTypeReferenceData<L>,
    pub enum_name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotResolveBuiltinModuleData {
    pub name: FlowSmolStr,
    pub potential_generator: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotImportGlobalLibdefData {
    pub module_name: FlowSmolStr,
    pub libdef_name: FlowSmolStr,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotSpreadGeneralData<L: Dupe> {
    pub spread_reason: VirtualReason<L>,
    pub object1_reason: VirtualReason<L>,
    pub object2_reason: VirtualReason<L>,
    pub propname: Name,
    pub error_kind: ExactnessErrorKind,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotSpreadInexactMayOverwriteIndexerData<L: Dupe> {
    pub spread_reason: VirtualReason<L>,
    pub object2_reason: VirtualReason<L>,
    pub key: Box<MessageTypeReferenceData<L>>,
    pub value: Box<MessageTypeReferenceData<L>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotUseEnumMemberUsedAsTypeData<L: Dupe> {
    pub description_name: Option<FlowSmolStr>,
    pub description: Result<ALocTy, VirtualReasonDesc<L>>,
    pub enum_: MessageTypeReferenceData<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotUseTypeForAnnotationInferenceData<L: Dupe> {
    pub operation: AnnotationInferenceOperation,
    pub operation_loc: L,
    pub target_loc: L,
    pub target_desc: Result<ALocTy, VirtualReasonDesc<L>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotUseTypeGuardWithFunctionParamHavocedData<L: Dupe> {
    pub type_guard_desc: VirtualReasonDesc<L>,
    pub param_reason: MessageTypeReferenceData<L>,
    pub call_locs: Vec<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotUseTypeInValuePositionData<L: Dupe> {
    pub reason: VirtualReason<L>,
    pub type_only_namespace: bool,
    pub imported_name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageDefinitionInvalidRecursiveData<L: Dupe> {
    pub description: VirtualReasonDesc<L>,
    pub recursion: Vec<L>,
    pub annot_locs: Vec<AnnotLoc<L>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageDuplicateModuleProviderData<L: Dupe> {
    pub module_name: FlowSmolStr,
    pub provider: L,
    pub conflict: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageEnumDuplicateMemberNameData<L: Dupe> {
    pub member_name: String,
    pub prev_use_loc: L,
    pub enum_: MessageTypeReferenceData<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageEnumInvalidMemberInitializerData<L: Dupe> {
    pub member_name: String,
    pub explicit_type: Option<flow_parser::ast::statement::enum_declaration::ExplicitType>,
    pub enum_: MessageTypeReferenceData<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageExponentialSpreadData<L: Dupe> {
    pub reason: VirtualReason<L>,
    pub reasons_for_operand1: ExponentialSpreadReasonGroup<L>,
    pub reasons_for_operand2: ExponentialSpreadReasonGroup<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIncompatibleTupleArityData<L: Dupe> {
    pub lower: MessageTypeReferenceData<L>,
    pub lower_arity: (i32, i32),
    pub lower_inexact: bool,
    pub upper: MessageTypeReferenceData<L>,
    pub upper_arity: (i32, i32),
    pub upper_inexact: bool,
    pub unify: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIncompatibleGeneralWithPrintedTypesData<L: Dupe> {
    pub lower_loc: L,
    pub upper_loc: L,
    pub lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub struct MessageTypeReferenceData<L: Dupe> {
    pub loc: L,
    pub desc: Result<ALocTy, VirtualReasonDesc<L>>,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub struct NamedReferenceData<L: Dupe> {
    pub loc: L,
    pub name: FlowSmolStr,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub struct FunctionReferenceData<L: Dupe> {
    pub loc: L,
    pub kind: FunctionReferenceKind,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub struct ImplicitInstantiationReferenceData<L: Dupe> {
    pub loc: L,
    pub kind: ImplicitInstantiationReferenceKind,
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum MessageIllegalAssertObject<L: Dupe> {
    Typed {
        expression: ExpressionReferenceData<L>,
        type_: MessageTypeReferenceData<L>,
    },
    Expression(ExpressionReferenceData<L>),
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum UnnecessaryInvariantConditionKind {
    Type,
    IntersectionType,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum TypeGuardReferenceKind {
    TypeGuard,
    Parameter(Option<FlowSmolStr>),
    TypeGuardParameter(FlowSmolStr),
    This,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub struct TypeGuardReferenceData<L: Dupe> {
    pub loc: L,
    pub kind: TypeGuardReferenceKind,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ValueAsTypeReference {
    Name(FlowSmolStr),
    ImportType(Userland),
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    serde::Serialize,
    serde::Deserialize
)]
pub struct TupleElementReferenceData<L: Dupe> {
    pub loc: L,
    pub name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum LowerRequirement {
    Array,
    ArrayIndex,
    Class,
    ClassWithPrivateProperties,
    Function,
    FunctionType,
    Inheritable,
    InstanceType,
    Object,
    PolymorphicType,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidArgumentWithPrintedTypeData<L: Dupe> {
    pub lower: MessageTypeReferenceData<L>,
    pub upper: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIncompatibleDueToInvariantSubtypingData<L: Dupe> {
    pub sub_component: Option<SubComponentOfInvariantSubtypingError>,
    pub lower_loc: L,
    pub upper_loc: L,
    pub lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIncompleteExhausiveCheckEnumData<L: Dupe> {
    pub description_name: Option<FlowSmolStr>,
    pub description: Result<ALocTy, VirtualReasonDesc<L>>,
    pub enum_: MessageTypeReferenceData<L>,
    pub left_to_check: Vec<FlowSmolStr>,
    pub default_case_loc: Option<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidEnumMemberCheckData<L: Dupe> {
    pub enum_: MessageTypeReferenceData<L>,
    pub enum_name: Option<FlowSmolStr>,
    pub example_member: Option<FlowSmolStr>,
    pub from_match: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidKeyPropertyInSpreadData<L: Dupe> {
    pub key_loc: L,
    pub spread_loc: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidRendersTypeArgumentData<L: Dupe> {
    pub renders_variant: RendersVariant,
    pub invalid_render_type_kind: InvalidRenderTypeKind<MessageTypeReferenceData<L>>,
    pub invalid_types: Vec1<MessageTypeReferenceData<L>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidSelfReferencingTypeAnnotationData<L: Dupe> {
    pub name: FlowSmolStr,
    pub loc: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidSelfReferencingDefaultData<L: Dupe> {
    pub name: FlowSmolStr,
    pub def_loc: L,
    pub ref_loc: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageMissingPlatformSupportWithAvailablePlatformsData {
    pub available_platforms: BTreeSet<FlowSmolStr>,
    pub required_platforms: BTreeSet<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageNoDefaultExportData {
    pub module_name: FlowSmolStr,
    pub suggestion: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageNoNamedExportData {
    pub module_name: FlowSmolStr,
    pub export_name: FlowSmolStr,
    pub suggestion: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageOnlyDefaultExportData {
    pub module_name: FlowSmolStr,
    pub export_name: FlowSmolStr,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessagePropExtraAgainstExactObjectData<L: Dupe> {
    pub lower: MessageTypeReferenceData<L>,
    pub upper: MessageTypeReferenceData<L>,
    pub upper_is_record: bool,
    pub props: Vec1<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessagePropMissingData<L: Dupe> {
    pub lower: VirtualReason<L>,
    pub upper: Option<VirtualReason<L>>,
    pub prop: Option<FlowSmolStr>,
    pub suggestion: Option<FlowSmolStr>,
    pub indexer: Option<Box<MessageTypeReferenceData<L>>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageConstructSignatureMissingData<L: Dupe> {
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessagePropsMissingData<L: Dupe> {
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
    pub props: Vec1<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIndexerCheckFailedData<L: Dupe> {
    pub prop: FlowSmolStr,
    pub lower: MessageTypeReferenceData<L>,
    pub upper: MessageTypeReferenceData<L>,
    pub indexer: MessageTypeReferenceData<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessagePropPolarityMismatchData<L: Dupe> {
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
    pub props: Vec1<(Option<FlowSmolStr>, Polarity, Polarity)>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageReactIntrinsicOverlapData<L: Dupe> {
    pub use_: VirtualReason<L>,
    pub def: L,
    pub type_: L,
    pub mixed: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageRedeclareComponentPropData<L: Dupe> {
    pub duplicates: Vec1<(L, Name, L)>,
    pub spread_loc: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageShouldAnnotateVariableUsedInGenericContextData<L: Dupe> {
    pub reason: MessageTypeReferenceData<L>,
    pub null_loc: L,
    pub initialized: bool,
    pub possible_generic_escape_locs: Vec<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageSketchyNullCheckData<L: Dupe> {
    pub kind: SketchyNullKind,
    pub falsy_loc: L,
    pub null_loc: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageTupleElementNotReadableData<L: Dupe> {
    pub index_def_loc: L,
    pub index: i32,
    pub name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageTupleElementNotWritableData<L: Dupe> {
    pub index_def_loc: L,
    pub index: i32,
    pub name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageTupleIndexOutOfBoundData<L: Dupe> {
    pub tuple: MessageTypeReferenceData<L>,
    pub inexact: bool,
    pub length: i32,
    pub index: FlowSmolStr,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageTupleNonIntegerIndexData<L: Dupe> {
    pub index_def_loc: L,
    pub index: FlowSmolStr,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageVariableOnlyAssignedByNullData<L: Dupe> {
    pub reason: MessageTypeReferenceData<L>,
    pub null_loc: Option<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageMatchNonExhaustiveObjectPatternData<L: Dupe> {
    pub rest: Option<VirtualReason<L>>,
    pub missing_props: Vec<FlowSmolStr>,
    pub pattern_kind: MatchObjPatternKind,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageMatchNonExplicitEnumCheckData<L: Dupe> {
    pub wildcard_reason: VirtualReason<L>,
    pub unchecked_members: Vec<FlowSmolStr>,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum VarianceSigilParent {
    Property,
    TypeParam,
}

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum VarianceSigilKind {
    Plus(VarianceSigilParent),
    Minus(VarianceSigilParent),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Message<L: Dupe> {
    MessagePlainTextReservedForInternalErrorOnly(FlowSmolStr),

    MessageAlreadyExhaustivelyCheckOneEnumMember(
        Box<MessageAlreadyExhaustivelyCheckOneEnumMemberData<L>>,
    ),

    MessageAlreadyExhaustivelyCheckAllEnumMembers {
        enum_: MessageTypeReferenceData<L>,
    },

    MessageAmbiguousNumericKeyWithVariance,
    MessageAmbiguousObjectType,

    MessageAnyValueUsedAsType {
        reference: Option<ValueAsTypeReference>,
        value: MessageTypeReferenceData<L>,
    },
    MessageBadLibdefModuleOverride(VirtualReason<L>),
    MessageBadLibdefNameOverride(VirtualReason<L>),
    MessageInterfaceMergePropertyConflict(VirtualReason<L>),
    MessageInterfaceMergeTparamMismatch(VirtualReason<L>),

    MessageCannotAccessEnumMember(Box<MessageCannotAccessEnumMemberData<L>>),

    MessageCannotAccessObjectWithComputedProp {
        object: MessageTypeReferenceData<L>,
        property: MessageTypeReferenceData<L>,
        kind: InvalidObjKey,
    },

    MessageCannotAccessReactRefInRender {
        usage: VirtualReason<L>,
        in_hook: bool,
    },

    MessageCannotAddComputedPropertyDueToPotentialOverwrite(
        Box<MessageCannotAddComputedPropertyDueToPotentialOverwriteData<L>>,
    ),

    MessageCannotApplyNonPolymorphicType,

    MessageCannotAssignToObjectWithComputedProp(VirtualReason<L>),

    MessageCannotAssignToObjectWithComputedPropWithKey {
        reason_prop: VirtualReason<L>,
        reason_key: VirtualReason<L>,
        kind: InvalidObjKey,
    },

    MessageCannotAssignToInvalidLHS,

    MessageCannotBuildTypedInterface(SignatureError<L>),

    MessageCannotCallMaybeReactHook(Box<MessageCannotCallMaybeReactHookData<L>>),

    MessageCannotCallNonHookSyntaxHook(L),

    MessageCannotCallObjectFunctionOnEnum {
        reason: MessageTypeReferenceData<L>,
        enum_: MessageTypeReferenceData<L>,
        enum_name: Option<FlowSmolStr>,
    },

    MessageCannotCallReactComponent(MessageTypeReferenceData<L>),

    MessageCannotCallReactHookConditionally(L),
    MessageCannotCallReactHookInDefinitelyNonComponentOrHook(L),
    MessageCannotCallReactHookInNonComponentSyntaxComponentOrHookSyntaxHook(L),
    MessageCannotCallReactHookInUnknownContext(L),
    MessageCannotCallReactHookWithIllegalName(L),

    MessageCannotCallFunctionWithExtraArg {
        function: MessageTypeReferenceData<L>,
        function_reference: FunctionReferenceData<L>,
        param_count: i32,
    },

    MessageCannotChangeEnumMember(MessageTypeReferenceData<L>),

    MessageCannotCompare(Box<MessageCannotCompareData<L>>),

    MessageCannotCompareNonStrict {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
    },

    MessageCannotCreateExactType(Box<MessageTypeReferenceData<L>>),

    MessageCannotDeclareAlreadyBoundName(VirtualReason<L>),
    MessageCannotDeclareAlreadyBoundNameInCoreJs(VirtualReason<L>),

    MessageCannotDeclareAlreadyBoundNameInNamespace(VirtualReason<L>),

    MessageCannotDeclareReservedType {
        keyword: IncorrectType,
        reason: VirtualReason<L>,
    },

    MessageCannotDelete(Box<MessageTypeReferenceData<L>>),
    MessageCannotDetermineEmptyArrayLiteralType,
    MessageCannotDetermineModuleType,

    MessageCannotExportRenamedDefault(Box<MessageCannotExportRenamedDefaultData>),

    MessageCannotExhaustivelyCheckAbstractEnums(
        Box<MessageCannotExhaustivelyCheckAbstractEnumsData<L>>,
    ),

    MessageCannotExhaustivelyCheckEnumWithUnknowns(
        Box<MessageCannotExhaustivelyCheckEnumWithUnknownsData<L>>,
    ),

    MessageCannotImplementNonInterface(Box<MessageTypeReferenceData<L>>),

    MessageCannotInstantiateObjectUtilTypeWithEnum(
        Box<MessageCannotInstantiateObjectUtilTypeWithEnumData<L>>,
    ),

    MessageCannotIterateEnum {
        enum_: MessageTypeReferenceData<L>,
        enum_name: Option<FlowSmolStr>,
    },

    MessageCannotIterateEnumForIn {
        enum_: MessageTypeReferenceData<L>,
        enum_name: Option<FlowSmolStr>,
    },

    MessageCannotIterateWithForIn(Box<MessageTypeReferenceData<L>>),
    MessageCannotMutateThisPrototype,
    MessageCannotNestComponents,
    MessageCannotNestHook,

    MessageCannotOptimizeUnionDueToNonUniqueKeys(BTreeMap<Name, UnionEnumMap<L>>),
    MessageCannotOptimizeUnionInternally(OptimizedError<L>),

    MessageCannotPassReactRefAsArgument {
        usage: VirtualReason<L>,
        in_hook: bool,
    },

    MessageCannotPerformArithOnNonNumbersOrBigInt(Box<MessageTypeReferenceData<L>>),
    MessageCannotPerformBigIntRShift3(Box<MessageTypeReferenceData<L>>),
    MessageCannotPerformBigIntUnaryPlus(Box<MessageTypeReferenceData<L>>),

    MessageCannotPerformBinaryArith {
        kind: flow_typing_type::type_::arith_kind::ArithKind,
        left: Box<MessageTypeReferenceData<L>>,
        right: Box<MessageTypeReferenceData<L>>,
    },

    MessageCannotReassignConstant(VirtualReason<L>),

    MessageCannotReassignConstantLikeBinding {
        definition: MessageTypeReferenceData<L>,
        binding_kind: AssignedConstLikeBindingType,
    },

    MessageCannotReassignEnum(VirtualReason<L>),
    MessageCannotReassignImport(VirtualReason<L>),
    MessageCannotRedeclareVar(VirtualReason<L>),

    MessageCannotReferenceTypeGuardParameter {
        type_guard_reason: MessageTypeReferenceData<L>,
        binding_reason: MessageTypeReferenceData<L>,
    },

    MessageCannotResolveBuiltinName(FlowSmolStr),

    MessageCannotResolveBuiltinModule(Box<MessageCannotResolveBuiltinModuleData>),

    MessageCannotResolveExpectedModule {
        name: FlowSmolStr,
        expected_module_purpose: ExpectedModulePurpose,
    },

    MessageCannotImportGlobalLibdef(Box<MessageCannotImportGlobalLibdefData>),

    MessageCannotSpreadDueToPotentialOverwrite {
        spread_reason: VirtualReason<L>,
        object_reason: VirtualReason<L>,
        key: Box<MessageTypeReferenceData<L>>,
    },

    MessageCannotSpreadGeneral(Box<MessageCannotSpreadGeneralData<L>>),

    MessageCannotSpreadInexactMayOverwriteIndexer(
        Box<MessageCannotSpreadInexactMayOverwriteIndexerData<L>>,
    ),

    MessageCannotSpreadInterface {
        spread_reason: VirtualReason<L>,
        interface: MessageTypeReferenceData<L>,
    },

    MessageCannotUseAsConstructor(Box<MessageTypeReferenceData<L>>),
    MessageCannotUseAsPrototype(Box<MessageTypeReferenceData<L>>),
    MessageCannotUseAsSuperClass(MessageTypeReferenceData<L>),
    MessageCannotUseBeforeDeclaration(VirtualReason<L>),

    MessageCannotUseDefaultImportWithDestrucuturing,
    MessageCannotUseDollarExports,

    MessageCannotUseEnumMemberUsedAsType(Box<MessageCannotUseEnumMemberUsedAsTypeData<L>>),

    MessageCannotUseExportInNonLegalToplevelContext(FlowSmolStr),
    MessageCannotUseImportStar(MessageTypeReferenceData<L>),
    MessageCannotUseInOperatorDueToBadLHS(Box<MessageTypeReferenceData<L>>),
    MessageCannotUseInOperatorDueToBadRHS(Box<MessageTypeReferenceData<L>>),
    MessageCannotUseInstanceOfOperatorDueToBadRHS(Box<MessageTypeReferenceData<L>>),
    MessageCannotUseMixedImportAndRequire(MessageTypeReferenceData<L>),

    MessageCannotUseNonPolymorphicTypeWithTypeArgs {
        is_new: bool,
        callee: Box<MessageTypeReferenceData<L>>,
        expected_arity: i32,
    },

    MessageCannotUsePrimitiveAsInterface {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
        kind: PrimitiveKind,
    },

    MessageCannotUseThisSuperBeforeSuperCall(VirtualReason<L>),

    MessageCannotUseTypeDueToPolarityMismatch {
        reason_targ: VirtualReason<L>,
        expected_polarity: Polarity,
        actual_polarity: Polarity,
    },

    MessageCannotUseTypeForAnnotationInference(
        Box<MessageCannotUseTypeForAnnotationInferenceData<L>>,
    ),

    MessageCannotUseTypeGuardWithFunctionParamHavoced(
        Box<MessageCannotUseTypeGuardWithFunctionParamHavocedData<L>>,
    ),

    MessageCannotUseTypeInValuePosition(Box<MessageCannotUseTypeInValuePositionData<L>>),

    MessageCannotUseTypeWithoutAnyTypeArgs {
        reason_arity: VirtualReason<L>,
        min_arity: i32,
        max_arity: i32,
    },

    MessageCannotUseTypeWithoutExactlyNTypeArgs(i32),

    MessageCannotUseTypeWithTooFewTypeArgs {
        reason_arity: VirtualReason<L>,
        n: i32,
    },

    MessageCannotUseTypeWithTooManyTypeArgs {
        reason_arity: VirtualReason<L>,
        n: i32,
    },

    MessageComponentMissingReturn(VirtualReason<L>),
    MessageComponentMissingBody,
    MessageComponentBodyInAmbientContext,
    MessageComponentNonUpperCase,
    MessageDeclareComponentInvalidParam(DeclareComponentInvalidParamKind),

    MessageDefinitionCycle(Vec1<(MessageTypeReferenceData<L>, Vec<L>, Vec<AnnotLoc<L>>)>),

    MessageDefinitionInvalidRecursive(Box<MessageDefinitionInvalidRecursiveData<L>>),

    MessageDeprecatedBool,

    MessageDeprecatedTypeParamColonBound,

    MessageDevOnlyRefinedLocInfo {
        refining_locs: Vec<L>,
    },

    MessageDevOnlyInvalidatedRefinementInfo(Vec<(L, refinement_invalidation::Reason)>),
    MessageDocblockError(DocblockError),

    MessageDoesNotRender {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageDuplicateClassMember {
        name: FlowSmolStr,
        static_: bool,
        class_kind: ClassKind,
    },

    MessageDuplicateEnumMember {
        enum_: MessageTypeReferenceData<L>,
        prev_use_loc: L,
    },

    MessageDuplicateModuleProvider(Box<MessageDuplicateModuleProviderData<L>>),

    MessageEnumsNotEnabled,

    MessageEnumConstNotSupported,

    MessageEnumDuplicateMemberName(Box<MessageEnumDuplicateMemberNameData<L>>),

    MessageEnumInconsistentMemberValues {
        enum_: MessageTypeReferenceData<L>,
    },

    MessageEnumInvalidMemberInitializer(Box<MessageEnumInvalidMemberInitializerData<L>>),

    MessageEnumBooleanMemberNotInitialized {
        member_name: String,
        enum_: MessageTypeReferenceData<L>,
    },

    MessageEnumNumberMemberNotInitialized {
        member_name: String,
        enum_: MessageTypeReferenceData<L>,
    },

    MessageEnumBigIntMemberNotInitialized {
        member_name: String,
        enum_: MessageTypeReferenceData<L>,
    },

    MessageEnumStringMemberInconsistentlyInitialized {
        enum_: MessageTypeReferenceData<L>,
    },

    MessageTSEnumInvalidMember {
        member_name: String,
        enum_: MessageTypeReferenceData<L>,
        kind: TsEnumInvalidMemberKind,
    },

    MessageTSEnumInvalidSyntax {
        enum_: MessageTypeReferenceData<L>,
        kind: TsEnumInvalidSyntaxKind,
    },

    MessageEnumNonIdentifierMemberName {
        member_name: String,
        enum_: MessageTypeReferenceData<L>,
    },

    MessageInvalidEnumMemberName {
        member_name: String,
        enum_: MessageTypeReferenceData<L>,
    },

    MessageExponentialSpread(Box<MessageExponentialSpreadData<L>>),

    MessageExportValueAsType(FlowSmolStr),

    MessageFunctionRequiresAnotherArgument {
        def: VirtualReason<L>,
        from: Option<VirtualReason<L>>,
    },

    MessageImportTypeAsTypeof(FlowSmolStr),
    MessageImportTypeAsValue(FlowSmolStr),
    MessageImportValueAsType(FlowSmolStr),

    MessageIncompatibleTupleArity(Box<MessageIncompatibleTupleArityData<L>>),

    MessageIncompatibleClassToObject {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
        kind: ClassKind,
    },

    MessageIncompatibleComponentRestParam(VirtualReason<L>),

    MessageIncompatibleGeneral {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageStandaloneCallRequiresReceiver(VirtualReason<L>),

    MessageIncompatibleWithUnionRepresentative {
        union: VirtualReason<L>,
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompatibleGeneralWithPrintedTypes(
        Box<MessageIncompatibleGeneralWithPrintedTypesData<L>>,
    ),

    MessageIncompatibleDueToInvariantSubtyping(
        Box<MessageIncompatibleDueToInvariantSubtypingData<L>>,
    ),

    MessageIncompatibleMappedTypeKey {
        source_type: VirtualReason<L>,
        mapped_type: VirtualReason<L>,
    },

    MessageIncompatibleNonLiteralArrayToTuple {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
    },

    MessageIncompatibleNonTypeGuardToTypeGuard {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
    },

    MessageIncompatibleReactHooksDueToUniqueness {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
    },

    MessageIncompatibleReactHooksWithNonReactHook {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
        lower_is_hook: bool,
        hook_is_annot: bool,
    },

    MessageIncompatibleWithExact {
        kind: ExactnessErrorKind,
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompatibleWithIndexed {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
    },

    MessageIncompleteExhausiveCheckEnum(Box<MessageIncompleteExhausiveCheckEnumData<L>>),

    MessageIncorrectType(IncorrectType),

    MessageInvalidArgument {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageInvalidArgumentWithPrintedType(Box<MessageInvalidArgumentWithPrintedTypeData<L>>),

    MessageInvalidCatchParameterAnnotation {
        ts_utility_syntax: bool,
    },

    MessageInvalidComponentRestParam,

    MessageInvalidEnumMemberCheck(Box<MessageInvalidEnumMemberCheckData<L>>),

    MessageInvalidGenericRef(FlowSmolStr),
    MessageInvalidGraphQL(GraphqlError),
    MessageInvalidHookNaming,

    MessageInvalidImportStarUse(MessageTypeReferenceData<L>),
    MessageInvalidInferType,

    MessageInvalidLintSettings(LintParseError),
    MessageInvalidMappedTypeInInterfaceOrDeclaredClass,
    MessageInvalidMappedTypeWithExactOrInexact,
    MessageInvalidMappedTypeWithExtraProps,
    MessageInvalidMappedTypeWithVarianceOnArrayInput,
    MessageInvalidTemplateLiteralTypeComplexity,
    MessageInvalidTemplateLiteralTypePlaceholder,

    MessageInvalidReactCreateElement(Box<MessageTypeReferenceData<L>>),

    MessageInvalidThisArgMissingReceiver {
        name: FlowSmolStr,
        callee: MessageTypeReferenceData<L>,
        callee_expression: ExpressionReferenceData<L>,
    },

    MessageInvalidThisArgReceiverMismatch {
        name: FlowSmolStr,
        callee: MessageTypeReferenceData<L>,
        receiver_expression: ExpressionReferenceData<L>,
    },

    MessageInvalidKeyPropertyInSpread(Box<MessageInvalidKeyPropertyInSpreadData<L>>),

    MessageInvalidRendersTypeArgument(Box<MessageInvalidRendersTypeArgumentData<L>>),

    MessageInvalidSelfReferencingTypeAnnotation(
        Box<MessageInvalidSelfReferencingTypeAnnotationData<L>>,
    ),

    MessageInvalidSelfReferencingDefault(Box<MessageInvalidSelfReferencingDefaultData<L>>),

    MessageInvalidTrivialRecursiveDefinition(VirtualReasonDesc<L>),

    MessageInvalidTupleRequiredAfterOptional {
        tuple_loc: L,
        required: TupleElementReferenceData<L>,
        optional: TupleElementReferenceData<L>,
    },

    MessageInvalidTupleTypeSpread(Box<MessageTypeReferenceData<L>>),
    MessageTupleElementAfterInexactSpread,

    MessageInternalType(InternalType),

    MessageInvalidTypeCastingSyntax,

    MessageInvalidTypeGuardFunctionKind(FlowSmolStr),

    MessageInvalidTypeGuardFunctionWritten {
        type_guard: NamedReferenceData<L>,
        write_locs: Vec<L>,
    },

    MessageNegativeTypeGuardConsistency {
        return_desc: VirtualReasonDesc<L>,
        type_: Box<MessageTypeReferenceData<L>>,
    },

    MessageInvalidTypeGuardParamUnbound(MessageTypeReferenceData<L>),
    MessageInvalidTypeGuardThisParam(MessageTypeReferenceData<L>),
    MessageInvalidUseOfFlowEnforceOptimized(Box<MessageTypeReferenceData<L>>),

    MessageLowerIsNotArray(VirtualReason<L>),
    MessageLowerIsNotArrayIndex(VirtualReason<L>),
    MessageLowerIsNotClass(VirtualReason<L>),
    MessageLowerIsNotClassWithPrivateProps(VirtualReason<L>),
    MessageLowerIsNotFunction(VirtualReason<L>),
    MessageLowerIsNotFunctionType(VirtualReason<L>),
    MessageLowerIsNotInheritable(VirtualReason<L>),
    MessageLowerIsNotInstanceType(VirtualReason<L>),
    MessageLowerIsNotObject(VirtualReason<L>),
    MessageLowerIsNotPolymorphicType(VirtualReason<L>),
    MessageLowerIsNotReactComponent(Box<MessageTypeReferenceData<L>>),

    MessageLowerIsNotWithPrintedType {
        lower: Box<MessageTypeReferenceData<L>>,
        requirement: LowerRequirement,
    },

    MessageLowerIsNotSupportedByUnclassifiedUse {
        lower: VirtualReason<L>,
        ctor: FlowSmolStr,
    },

    MessageLowerIsNotSupportedByUnclassifiedUseWithPrintedType {
        lower: Box<MessageTypeReferenceData<L>>,
        ctor: FlowSmolStr,
    },

    MessageMissingAnnotation(VirtualReasonDesc<L>),
    MessageMissingAnnotationDueToContextualTypingFailure(VirtualReasonDesc<L>),
    MessageMissingAnnotationForGenericFunction(VirtualReasonDesc<L>),

    MessageMissingPlatformSupportWithAvailablePlatforms(
        Box<MessageMissingPlatformSupportWithAvailablePlatformsData>,
    ),

    MessageNoDefaultExport(Box<MessageNoDefaultExportData>),

    MessageNoNamedExport(Box<MessageNoNamedExportData>),

    MessageNonConstVarExport(Option<MessageTypeReferenceData<L>>),
    MessageNonStrictImport,
    MessageNonToplevelExport,

    MessageOnlyDefaultExport(Box<MessageOnlyDefaultExportData>),

    MessageParseError(ParseError),
    MessagePlatformSpecificImplementationModuleLookupFailed(FlowSmolStr),

    MessagePropExtraAgainstExactObject(Box<MessagePropExtraAgainstExactObjectData<L>>),

    MessagePropMissing(Box<MessagePropMissingData<L>>),

    MessagePrivatePropMissing {
        object: MessageTypeReferenceData<L>,
        prop: FlowSmolStr,
    },

    MessageConstructSignatureMissing(Box<MessageConstructSignatureMissingData<L>>),

    MessagePropsMissing(Box<MessagePropsMissingData<L>>),

    MessageIndexerCheckFailed(Box<MessageIndexerCheckFailedData<L>>),

    MessagePropPolarityMismatch(Box<MessagePropPolarityMismatchData<L>>),

    MessagePropNotReadable(Option<Name>),
    MessagePropNotWritable(Option<Name>),

    MessageReactIntrinsicOverlap(Box<MessageReactIntrinsicOverlapData<L>>),

    MessageReadonlyArraysCannotBeWrittenTo,
    MessageRecursionLimitExceeded,

    MessageRedeclareComponentProp(Box<MessageRedeclareComponentPropData<L>>),

    MessageShouldAnnotateVariableOnlyInitializedInGenericContext {
        reason: MessageTypeReferenceData<L>,
        possible_generic_escape_locs: Vec<L>,
    },

    MessageShouldAnnotateVariableUsedInGenericContext(
        Box<MessageShouldAnnotateVariableUsedInGenericContextData<L>>,
    ),

    MessageShouldNotBeCoerced(VirtualReason<L>),
    MessageShouldUseArrayLiteral,

    MessageSketchyNumber(MessageTypeReferenceData<L>),

    MessageSketchyNullCheck(Box<MessageSketchyNullCheckData<L>>),

    MessageSuppressionMalformedCode,
    MessageSuppressionMissingCode,

    MessageThisInComponent(L),
    MessageThisInExportedFunction,

    MessageThisSuperInObject(MessageTypeReferenceData<L>, ThisFinderKind),

    MessageTSNeverType,
    MessageTSReadonlyOperatorOnArray,
    MessageTSReadonlyOperatorOnTuple,
    MessageTSReadonlyType,
    MessageTSVarianceInOut,
    MessageDeprecatedVarianceSigil(VarianceSigilKind),
    MessageTSClassAccessibility(ast::class::ts_accessibility::Kind),
    MessageTSParameterProperty,
    MessageAbstractClass,
    MessageAbstractMethod,
    MessageAbstract(AbstractErrorKind<L>),
    MessageOverride(OverrideErrorKind<L>),
    MessageTSUndefinedType,

    MessageTupleElementNotReadable(Box<MessageTupleElementNotReadableData<L>>),

    MessageTupleElementNotWritable(Box<MessageTupleElementNotWritableData<L>>),

    MessageTupleIndexOutOfBound(Box<MessageTupleIndexOutOfBoundData<L>>),

    MessageTupleNonIntegerIndex(Box<MessageTupleNonIntegerIndexData<L>>),

    MessageTupleNonStaticallyKnownIndex,

    MessageTuplePolarityMismatch {
        index: i32,
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
        polarity_lower: Polarity,
        polarity_upper: Polarity,
    },

    MessageTypeGuardIndexMismatch {
        lower: MessageTypeReferenceData<L>,
        upper: MessageTypeReferenceData<L>,
    },

    MessageTypeGuardImpliesMismatch {
        lower: TypeGuardReferenceData<L>,
        upper: TypeGuardReferenceData<L>,
    },

    MessageIncompatiblETypeParamConstIncompatibility {
        lower: NamedReferenceData<L>,
        upper: NamedReferenceData<L>,
    },

    MessageTypeParamConstInvalidPosition(MessageTypeReferenceData<L>),
    MessageUnclearType,

    MessageUnderconstrainedImplicitInstantiaton {
        call: ImplicitInstantiationReferenceData<L>,
        reason_tparam: MessageTypeReferenceData<L>,
    },

    MessageUndocumentedFeature,

    MessageIllegalAssertOperator {
        obj: Box<MessageIllegalAssertObject<L>>,
        specialized: bool,
    },

    MessageUnexpectedUseOfThisType,

    MessageUninitializedInstanceProperty(PropertyAssignmentKind),

    MessageUnknownParameterTypes(VirtualReason<L>),
    MessageUnknownParameterTypesWithPrintedType(Box<MessageTypeReferenceData<L>>),
    MessageUnnecessaryDeclareTypeOnlyExport,
    MessageUnnecessaryInvariant {
        condition: Box<MessageTypeReferenceData<L>>,
        condition_kind: UnnecessaryInvariantConditionKind,
    },
    MessageUnnecessaryOptionalChain {
        lhs: MessageTypeReferenceData<L>,
        lhs_expression: ExpressionReferenceData<L>,
    },
    MessageUnreachableCode,
    MessageUnsafeGetterSetter,
    MessageUnsafeObjectAssign,

    MessageUnsupportedKeyInObject {
        key_error_kind: InvalidObjKey,
        obj_kind: ObjKind,
    },

    MessageUnsupportedComputedKeyInClass,

    MessageUnsupportedSyntax(UnsupportedSyntax),
    MessageUnsupportedVarianceAnnotation(FlowSmolStr),

    MessageUntypedImport(FlowSmolStr),
    MessageUntypedTypeImport(FlowSmolStr),

    MessageUnusedPromiseInAsyncScope,
    MessageUnusedPromiseInSyncScope,
    MessageUnusedSuppression,

    MessageValueUsedAsType {
        reference: Option<ValueAsTypeReference>,
        value: MessageTypeReferenceData<L>,
    },
    MessageVariableNeverInitAssignedAnnotated(MessageTypeReferenceData<L>),

    MessageVariableOnlyAssignedByNull(Box<MessageVariableOnlyAssignedByNullData<L>>),

    MessageMatchNotExhaustive {
        examples: Vec<(FlowSmolStr, Vec<MessageTypeReferenceData<L>>)>,
    },

    MessageMatchUnnecessaryPattern {
        reason: VirtualReason<L>,
        already_seen: Option<VirtualReason<L>>,
    },

    MessageMatchNonExhaustiveObjectPattern(Box<MessageMatchNonExhaustiveObjectPatternData<L>>),

    MessageMatchNonExplicitEnumCheck(Box<MessageMatchNonExplicitEnumCheckData<L>>),

    MessageMatchInvalidGuardedWildcard,

    MessageMatchInvalidIdentOrMemberPattern {
        type_: Box<MessageTypeReferenceData<L>>,
    },

    MessageMatchInvalidBindingKind {
        kind: VariableKind,
    },

    MessageMatchInvalidObjectPropertyLiteral {
        pattern_kind: MatchObjPatternKind,
    },

    MessageMatchInvalidUnaryZero,
    MessageMatchInvalidUnaryPlusBigInt,

    MessageMatchDuplicateObjectProperty {
        name: FlowSmolStr,
        pattern_kind: MatchObjPatternKind,
    },

    MessageMatchBindingInOrPattern,
    MessageMatchInvalidAsPattern,

    MessageMatchInvalidPatternReference {
        binding_reason: MessageTypeReferenceData<L>,
    },

    MessageMatchInvalidObjectShorthand {
        name: FlowSmolStr,
        pattern_kind: MatchObjPatternKind,
    },

    MessageMatchStatementInvalidBody,

    MessageMatchInvalidCaseSyntax(MatchInvalidCaseSyntax<L>),
    MessageMatchInvalidWildcardSyntax,
    MessageMatchInvalidInstancePattern,

    MessageRecordBannedTypeUtil {
        reason_op: VirtualReason<L>,
        record: MessageTypeReferenceData<L>,
    },

    MessageRecordInvalidNew {
        record_name: FlowSmolStr,
    },

    MessageRecordInvalidName {
        name: FlowSmolStr,
    },

    MessageRecordDeclarationInvalidSyntax(RecordDeclarationInvalidSyntax<L>),

    MessageConstantCondition {
        is_truthy: bool,
        warning: ConstantConditionWarning<L>,
        constant_condition_kind: ConstantConditionKind,
    },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct IntermediateError<L: Dupe> {
    pub kind: ErrorKind,
    pub loc: Loc,
    pub error_code: Option<ErrorCode>,
    pub root: Option<(Loc, RootMessage<L>)>,
    pub message: ErrorMessage<L>,
    pub misplaced_source_file: Option<FileKey>,
    pub unsuppressable: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum ErrorMessage<L: Dupe> {
    SingletonMessage {
        message: Message<L>,
        frames: Option<Vec<Frame<L>>>,
        explanations: Option<Vec<Explanation<L>>>,
    },
    SingletonMessageWithExample {
        message: Message<L>,
        frames: Option<Vec<Frame<L>>>,
        explanations: Option<Vec<Explanation<L>>>,
        example: Box<IntermediateError<L>>,
    },
    SpeculationMessage {
        frames: Vec<Frame<L>>,
        explanations: Vec<Explanation<L>>,
        branches: Vec<(i32, IntermediateError<L>)>,
    },
}

impl Default for IntermediateError<Loc> {
    fn default() -> Self {
        IntermediateError {
            kind: ErrorKind::InferError,
            loc: Loc::default(),
            error_code: None,
            root: None,
            message: ErrorMessage::SingletonMessage {
                message: Message::MessagePlainTextReservedForInternalErrorOnly(
                    "default error".into(),
                ),
                frames: None,
                explanations: None,
            },
            misplaced_source_file: None,
            unsuppressable: false,
        }
    }
}
