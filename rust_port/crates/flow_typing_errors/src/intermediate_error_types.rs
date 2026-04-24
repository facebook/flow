/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::str::FromStr;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::options::CastingSyntax;
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
use flow_typing_type::type_::UnionEnum;
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
    ClassPropertyComputed,
    ClassStaticBlock,
    ClassDeclareMethod,
    ClassIndexSignature,
    RequireDynamicArgument,
    CatchParameterDeclaration,
    DestructuringObjectPropertyInvalidLiteral,
    DestructuringExpressionPattern,
    JSXTypeArgs,
    OpaqueTypeExtendsBound,
    OpaqueTypeSuperBound,
    PredicateFunction,
    PredicateDeclarationAnonymousParameters,
    MatchExpression,
    MatchStatement,
    MatchInstancePattern,
    MultipleIndexers,
    MultipleProtos,
    ExplicitCallAfterProto,
    ExplicitProtoAfterCall,
    SpreadArgument,
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
pub enum InvalidRenderTypeKind<L: Dupe> {
    InvalidRendersNullVoidFalse,
    InvalidRendersIterable,
    InvalidRendersStructural(VirtualReason<L>),
    InvalidRendersNonNominalElement(VirtualReason<L>),
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
    Dupe,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum NullSide {
    Left,
    Right,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum EmptySide {
    Left,
    Right,
}

#[derive(
    Debug,
    Clone,
    Dupe,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum StrictComparisonKind {
    StrictComparisonGeneral,
    StrictComparisonNull { null_side: NullSide },
    StrictComparisonEmpty { empty_side: EmptySide },
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
pub struct StrictComparisonInfo<L: Dupe> {
    pub left_precise_reason: VirtualReason<L>,
    pub right_precise_reason: VirtualReason<L>,
    pub strict_comparison_kind: StrictComparisonKind,
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
            Self::TSReadonly => "$ReadOnly",
            Self::TSReadonlyArray => "$ReadOnlyArray",
            Self::TSReadonlyMap => "$ReadOnlyMap",
            Self::TSReadonlySet => "$ReadOnlySet",
            Self::TSNonNullable => "$NonMaybeType",
            Self::Values => "$Values",
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
        casting_syntax: CastingSyntax,
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

pub type UnionEnumMap<L> = BTreeMap<UnionEnum, Vec1<VirtualReason<L>>>;

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
    pub enum_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotAccessEnumMemberData<L: Dupe> {
    pub member_name: Option<Name>,
    pub suggestion: Option<FlowSmolStr>,
    pub description: VirtualReasonDesc<L>,
    pub enum_reason: VirtualReason<L>,
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
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
    pub strict_comparison_opt: Option<StrictComparisonInfo<L>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotExportRenamedDefaultData {
    pub name: Option<FlowSmolStr>,
    pub is_reexport: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotExhaustivelyCheckAbstractEnumsData<L: Dupe> {
    pub description: VirtualReasonDesc<L>,
    pub enum_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotExhaustivelyCheckEnumWithUnknownsData<L: Dupe> {
    pub description: VirtualReasonDesc<L>,
    pub enum_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotInstantiateObjectUtilTypeWithEnumData<L: Dupe> {
    pub description: VirtualReasonDesc<L>,
    pub enum_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotResolveBuiltinModuleData {
    pub name: FlowSmolStr,
    pub potential_generator: Option<FlowSmolStr>,
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
    pub key_reason: VirtualReason<L>,
    pub value_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotUseEnumMemberUsedAsTypeData<L: Dupe> {
    pub description: VirtualReasonDesc<L>,
    pub enum_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotUseTypeForAnnotationInferenceData<L: Dupe> {
    pub reason_op: VirtualReason<L>,
    pub reason: VirtualReason<L>,
    pub suggestion: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageCannotUseTypeGuardWithFunctionParamHavocedData<L: Dupe> {
    pub type_guard_desc: VirtualReasonDesc<L>,
    pub param_reason: VirtualReason<L>,
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
    pub enum_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageEnumInvalidMemberInitializerData<L: Dupe> {
    pub member_name: String,
    pub explicit_type: Option<flow_parser::ast::statement::enum_declaration::ExplicitType>,
    pub enum_reason: VirtualReason<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageExponentialSpreadData<L: Dupe> {
    pub reason: VirtualReason<L>,
    pub reasons_for_operand1: ExponentialSpreadReasonGroup<L>,
    pub reasons_for_operand2: ExponentialSpreadReasonGroup<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIncompatibleTupleArityData<L: Dupe> {
    pub lower_reason: VirtualReason<L>,
    pub lower_arity: (i32, i32),
    pub lower_inexact: bool,
    pub upper_reason: VirtualReason<L>,
    pub upper_arity: (i32, i32),
    pub upper_inexact: bool,
    pub unify: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIncompatibleImplicitReturnData<L: Dupe> {
    pub lower: VirtualReason<L>,
    pub upper: VirtualReasonDesc<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageIncompatibleGeneralWithPrintedTypesData<L: Dupe> {
    pub lower_loc: L,
    pub upper_loc: L,
    pub lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
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
    pub description: VirtualReasonDesc<L>,
    pub enum_reason: VirtualReason<L>,
    pub left_to_check: Vec<FlowSmolStr>,
    pub default_case_loc: Option<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidEnumMemberCheckData<L: Dupe> {
    pub enum_reason: VirtualReason<L>,
    pub example_member: Option<FlowSmolStr>,
    pub from_match: bool,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidRefPropertyInSpreadData<L: Dupe> {
    pub ref_loc: L,
    pub spread_loc: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidKeyPropertyInSpreadData<L: Dupe> {
    pub key_loc: L,
    pub spread_loc: L,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageInvalidRendersTypeArgumentData<L: Dupe> {
    pub renders_variant: RendersVariant,
    pub invalid_render_type_kind: InvalidRenderTypeKind<L>,
    pub invalid_type_reasons: Vec1<VirtualReason<L>>,
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
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
    pub props: Vec1<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessagePropMissingData<L: Dupe> {
    pub lower: VirtualReason<L>,
    pub upper: Option<VirtualReason<L>>,
    pub prop: Option<FlowSmolStr>,
    pub suggestion: Option<FlowSmolStr>,
    pub reason_indexer: Option<VirtualReason<L>>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessagePropsMissingData<L: Dupe> {
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
    pub props: Vec1<FlowSmolStr>,
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
    pub reason: VirtualReason<L>,
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
    pub reason: VirtualReason<L>,
    pub index: i32,
    pub name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageTupleElementNotWritableData<L: Dupe> {
    pub reason: VirtualReason<L>,
    pub index: i32,
    pub name: Option<FlowSmolStr>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MessageTupleIndexOutOfBoundData<L: Dupe> {
    pub reason_op: VirtualReason<L>,
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
    pub reason: VirtualReason<L>,
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

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Message<L: Dupe> {
    MessagePlainTextReservedForInternalErrorOnly(FlowSmolStr),

    MessageAlreadyExhaustivelyCheckOneEnumMember(
        Box<MessageAlreadyExhaustivelyCheckOneEnumMemberData<L>>,
    ),

    MessageAlreadyExhaustivelyCheckAllEnumMembers {
        enum_reason: VirtualReason<L>,
    },

    MessageAmbiguousNumericKeyWithVariance,
    MessageAmbiguousObjectType,

    MessageAnyValueUsedAsType(VirtualReasonDesc<L>),
    MessageBadLibdefModuleOverride(VirtualReason<L>),
    MessageBadLibdefNameOverride(VirtualReason<L>),
    MessageInterfaceMergePropertyConflict(VirtualReason<L>),
    MessageInterfaceMergeTparamMismatch(VirtualReason<L>),

    MessageCannotAccessEnumMember(Box<MessageCannotAccessEnumMemberData<L>>),

    MessageCannotAccessObjectWithComputedProp {
        reason_obj: VirtualReason<L>,
        reason_prop: VirtualReason<L>,
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

    MessageCannotAssignToOptionalTupleElement {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageCannotAssignToInvalidLHS,

    MessageCannotBuildTypedInterface(SignatureError<L>),

    MessageCannotCallMaybeReactHook(Box<MessageCannotCallMaybeReactHookData<L>>),

    MessageCannotCallNonHookSyntaxHook(L),

    MessageCannotCallObjectFunctionOnEnum {
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
    },

    MessageCannotCallReactComponent(VirtualReason<L>),

    MessageCannotCallReactFunctionWithoutAtLeastNArgs {
        fn_name: FlowSmolStr,
        n: i32,
    },

    MessageCannotCallReactHookConditionally(L),
    MessageCannotCallReactHookInDefinitelyNonComponentOrHook(L),
    MessageCannotCallReactHookInNonComponentSyntaxComponentOrHookSyntaxHook(L),
    MessageCannotCallReactHookInUnknownContext(L),
    MessageCannotCallReactHookWithIllegalName(L),

    MessageCannotCallFunctionWithExtraArg {
        def_reason: VirtualReason<L>,
        param_count: i32,
    },

    MessageCannotChangeEnumMember(VirtualReason<L>),

    MessageCannotCompare(Box<MessageCannotCompareData<L>>),

    MessageCannotCompareNonStrict {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageCannotCreateExactType(VirtualReason<L>),

    MessageCannotDeclareAlreadyBoundGlobal(VirtualReason<L>),
    MessageCannotDeclareAlreadyBoundName(VirtualReason<L>),
    MessageCannotDeclareAlreadyBoundNameInCoreJs(VirtualReason<L>),

    MessageCannotDeclareAlreadyBoundNameInNamespace(VirtualReason<L>),

    MessageCannotDeclareReservedType {
        keyword: IncorrectType,
        reason: VirtualReason<L>,
    },

    MessageCannotDelete(VirtualReason<L>),
    MessageCannotDetermineEmptyArrayLiteralType,
    MessageCannotDetermineModuleType,

    MessageCannotExportRenamedDefault(Box<MessageCannotExportRenamedDefaultData>),

    MessageCannotExhaustivelyCheckAbstractEnums(
        Box<MessageCannotExhaustivelyCheckAbstractEnumsData<L>>,
    ),

    MessageCannotExhaustivelyCheckEnumWithUnknowns(
        Box<MessageCannotExhaustivelyCheckEnumWithUnknownsData<L>>,
    ),

    MessageCannotImplementNonInterface(VirtualReasonDesc<L>),

    MessageCannotInstantiateObjectUtilTypeWithEnum(
        Box<MessageCannotInstantiateObjectUtilTypeWithEnumData<L>>,
    ),

    MessageCannotIterateEnum {
        reason: VirtualReason<L>,
        for_in: bool,
    },

    MessageCannotIterateWithForIn(VirtualReason<L>),
    MessageCannotMutateThisPrototype,
    MessageCannotNestComponents,
    MessageCannotNestHook,

    MessageCannotOptimizeUnionDueToNonUniqueKeys(BTreeMap<Name, UnionEnumMap<L>>),
    MessageCannotOptimizeUnionInternally(OptimizedError<L>),

    MessageCannotPassReactRefAsArgument {
        usage: VirtualReason<L>,
        in_hook: bool,
    },

    MessageCannotPerformArithOnNonNumbersOrBigInt(VirtualReason<L>),
    MessageCannotPerformBigIntRShift3(VirtualReason<L>),
    MessageCannotPerformBigIntUnaryPlus(VirtualReason<L>),

    MessageCannotPerformBinaryArith {
        kind: flow_typing_type::type_::arith_kind::ArithKind,
        reason_l: VirtualReason<L>,
        reason_r: VirtualReason<L>,
    },

    MessageCannotReassignConstant(VirtualReason<L>),

    MessageCannotReassignConstantLikeBinding {
        definition: VirtualReason<L>,
        binding_kind: AssignedConstLikeBindingType,
    },

    MessageCannotReassignEnum(VirtualReason<L>),
    MessageCannotReassignImport(VirtualReason<L>),
    MessageCannotRedeclareVar(VirtualReason<L>),

    MessageCannotReferenceTypeGuardParameter {
        type_guard_reason: VirtualReason<L>,
        binding_reason: VirtualReason<L>,
    },

    MessageCannotResolveBuiltinName(FlowSmolStr),

    MessageCannotResolveBuiltinModule(Box<MessageCannotResolveBuiltinModuleData>),

    MessageCannotResolveExpectedModule {
        name: FlowSmolStr,
        expected_module_purpose: ExpectedModulePurpose,
    },

    MessageCannotSpreadDueToPotentialOverwrite {
        spread_reason: VirtualReason<L>,
        object_reason: VirtualReason<L>,
        key_reason: VirtualReason<L>,
    },

    MessageCannotSpreadGeneral(Box<MessageCannotSpreadGeneralData<L>>),

    MessageCannotSpreadInexactMayOverwriteIndexer(
        Box<MessageCannotSpreadInexactMayOverwriteIndexerData<L>>,
    ),

    MessageCannotSpreadInterface {
        spread_reason: VirtualReason<L>,
        interface_reason: VirtualReason<L>,
    },

    MessageCannotUseAsConstructor(VirtualReason<L>),
    MessageCannotUseAsPrototype(VirtualReason<L>),
    MessageCannotUseAsSuperClass(VirtualReason<L>),
    MessageCannotUseBeforeDeclaration(VirtualReason<L>),

    MessageCannotUseComputedPropertyWithUnion(VirtualReason<L>),
    MessageCannotUseDefaultImportWithDestrucuturing,
    MessageCannotUseDollarExports,

    MessageCannotUseEnumMemberUsedAsType(Box<MessageCannotUseEnumMemberUsedAsTypeData<L>>),

    MessageCannotUseExportInNonLegalToplevelContext(FlowSmolStr),
    MessageCannotUseImportStar(VirtualReason<L>),
    MessageCannotUseInOperatorDueToBadLHS(VirtualReason<L>),
    MessageCannotUseInOperatorDueToBadRHS(VirtualReason<L>),
    MessageCannotUseInstanceOfOperatorDueToBadRHS(VirtualReason<L>),
    MessageCannotUseMixedImportAndRequire(VirtualReason<L>),

    MessageCannotUseNonPolymorphicTypeWithTypeArgs {
        is_new: bool,
        reason_arity: VirtualReason<L>,
        expected_arity: i32,
    },

    MessageCannotUsePrimitiveAsInterface {
        reason: VirtualReason<L>,
        interface_reason: VirtualReason<L>,
        kind: PrimitiveKind,
    },

    MessageCannotUseStrUtilType,
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

    MessageCannotUseTypeWithInvalidTypeArgs {
        reason_main: VirtualReason<L>,
        reason_tapp: VirtualReason<L>,
    },

    MessageCannotUseTypeWithoutAnyTypeArgs {
        reason_arity: VirtualReason<L>,
        min_arity: i32,
        max_arity: i32,
    },

    MessageCannotUseTypeWithoutAtLeastNTypeArgs(i32),
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

    MessageDefinitionCycle(Vec1<(VirtualReason<L>, Vec<L>, Vec<AnnotLoc<L>>)>),

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
        enum_reason: VirtualReason<L>,
        prev_use_loc: L,
    },

    MessageDuplicateModuleProvider(Box<MessageDuplicateModuleProviderData<L>>),

    MessageEnumsNotEnabled,

    MessageEnumConstNotSupported,

    MessageEnumDuplicateMemberName(Box<MessageEnumDuplicateMemberNameData<L>>),

    MessageEnumInconsistentMemberValues {
        enum_reason: VirtualReason<L>,
    },

    MessageEnumInvalidMemberInitializer(Box<MessageEnumInvalidMemberInitializerData<L>>),

    MessageEnumBooleanMemberNotInitialized {
        member_name: String,
        enum_reason: VirtualReason<L>,
    },

    MessageEnumNumberMemberNotInitialized {
        member_name: String,
        enum_reason: VirtualReason<L>,
    },

    MessageEnumBigIntMemberNotInitialized {
        member_name: String,
        enum_reason: VirtualReason<L>,
    },

    MessageEnumStringMemberInconsistentlyInitialized {
        enum_reason: VirtualReason<L>,
    },

    MessageEnumNonIdentifierMemberName {
        member_name: String,
        enum_reason: VirtualReason<L>,
    },

    MessageInvalidEnumMemberName {
        member_name: String,
        enum_reason: VirtualReason<L>,
    },

    MessageExponentialSpread(Box<MessageExponentialSpreadData<L>>),

    MessageExportValueAsType(FlowSmolStr),

    MessageFunctionRequiresAnotherArgument {
        def: VirtualReason<L>,
        from: Option<VirtualReason<L>>,
    },

    MessageImplicitInexactObject,
    MessageImportTypeAsTypeof(FlowSmolStr),
    MessageImportTypeAsValue(FlowSmolStr),
    MessageImportValueAsType(FlowSmolStr),

    MessageIncompatibleArity {
        lower: VirtualReason<L>,
        lower_arity: i32,
        upper: VirtualReason<L>,
        upper_arity: i32,
    },

    MessageIncompatibleTupleArity(Box<MessageIncompatibleTupleArityData<L>>),

    MessageIncompatibleImplicitReturn(Box<MessageIncompatibleImplicitReturnData<L>>),

    MessageIncompatibleClassToObject {
        reason_class: VirtualReason<L>,
        reason_obj: VirtualReason<L>,
        kind: ClassKind,
    },

    MessageIncompatibleComponentRestParam(VirtualReason<L>),

    MessageIncompatibleGeneral {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

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
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompatibleNonTypeGuardToTypeGuard {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompatibleReactHooksDueToUniqueness {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompatibleReactHooksWithNonReactHook {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        lower_is_hook: bool,
        hook_is_annot: bool,
    },

    MessageIncompatibleWithExact {
        kind: ExactnessErrorKind,
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompatibleWithIndexed {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompleteExhausiveCheckEnum(Box<MessageIncompleteExhausiveCheckEnumData<L>>),

    MessageIncorrectType(IncorrectType),

    MessageInvalidArgument {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageInvalidCatchParameterAnnotation {
        ts_utility_syntax: bool,
    },

    MessageInvalidComponentRestParam,

    MessageInvalidEnumMemberCheck(Box<MessageInvalidEnumMemberCheckData<L>>),

    MessageInvalidGenericRef(FlowSmolStr),
    MessageInvalidGraphQL(GraphqlError),
    MessageInvalidHookNaming,

    MessageInvalidImportStarUse(VirtualReason<L>),
    MessageInvalidInferType,

    MessageInvalidLintSettings(LintParseError),
    MessageInvalidMappedTypeInInterfaceOrDeclaredClass,
    MessageInvalidMappedTypeWithExactOrInexact,
    MessageInvalidMappedTypeWithExtraProps,
    MessageInvalidMappedTypeWithOptionalityRemoval,
    MessageInvalidMappedTypeWithVarianceOnArrayInput,

    MessageInvalidReactCreateElement(VirtualReason<L>),

    MessageInvalidRefPropertyInSpread(Box<MessageInvalidRefPropertyInSpreadData<L>>),

    MessageInvalidKeyPropertyInSpread(Box<MessageInvalidKeyPropertyInSpreadData<L>>),

    MessageInvalidRendersTypeArgument(Box<MessageInvalidRendersTypeArgumentData<L>>),

    MessageInvalidSelfReferencingTypeAnnotation(
        Box<MessageInvalidSelfReferencingTypeAnnotationData<L>>,
    ),

    MessageInvalidSelfReferencingDefault(Box<MessageInvalidSelfReferencingDefaultData<L>>),

    MessageInvalidTrivialRecursiveDefinition(VirtualReasonDesc<L>),

    MessageInvalidTupleRequiredAfterOptional {
        reason_tuple: VirtualReason<L>,
        reason_required: VirtualReason<L>,
        reason_optional: VirtualReason<L>,
    },

    MessageInvalidTupleTypeSpread(VirtualReason<L>),
    MessageTupleElementAfterInexactSpread,

    MessageInternalType(InternalType),

    MessageInvalidTypeCastingSyntax(CastingSyntax),

    MessageInvalidTypeGuardFunctionKind(FlowSmolStr),

    MessageInvalidTypeGuardFunctionWritten {
        type_guard_reason: VirtualReason<L>,
        write_locs: Vec<L>,
    },

    MessageNegativeTypeGuardConsistency {
        reason: VirtualReason<L>,
        return_reason: VirtualReason<L>,
        type_reason: VirtualReason<L>,
    },

    MessageInvalidTypeGuardParamUnbound(VirtualReason<L>),
    MessageInvalidTypeGuardThisParam(VirtualReason<L>),
    MessageInvalidUseOfFlowEnforceOptimized(VirtualReason<L>),

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
    MessageLowerIsNotReactComponent(VirtualReason<L>),

    MessageLowerIsNotSupportedByUnclassifiedUse {
        lower: VirtualReason<L>,
        ctor: FlowSmolStr,
    },

    MessageMethodUnbinding {
        reason_op: VirtualReason<L>,
        context_loc: L,
    },

    MessageMissingAnnotation(VirtualReasonDesc<L>),
    MessageMissingAnnotationDueToContextualTypingFailure(VirtualReasonDesc<L>),
    MessageMissingAnnotationForGenericFunction(VirtualReasonDesc<L>),

    MessageMissingPlatformSupportWithAvailablePlatforms(
        Box<MessageMissingPlatformSupportWithAvailablePlatformsData>,
    ),

    MessageMissingPlatformSupport {
        missing_platforms: BTreeSet<FlowSmolStr>,
    },

    MessageNoDefaultExport(Box<MessageNoDefaultExportData>),

    MessageNoNamedExport(Box<MessageNoNamedExportData>),

    MessageNonConstVarExport(Option<VirtualReason<L>>),
    MessageNonStrictImport,
    MessageNonToplevelExport,

    MessageOnlyDefaultExport(Box<MessageOnlyDefaultExportData>),

    MessageParseError(ParseError),
    MessagePlatformSpecificImplementationModuleLookupFailed(FlowSmolStr),

    MessagePropExtraAgainstExactObject(Box<MessagePropExtraAgainstExactObjectData<L>>),

    MessagePropMissing(Box<MessagePropMissingData<L>>),

    MessagePropsMissing(Box<MessagePropsMissingData<L>>),

    MessagePropPolarityMismatch(Box<MessagePropPolarityMismatchData<L>>),

    MessagePropNotReadable(Option<Name>),
    MessagePropNotWritable(Option<Name>),

    MessageReactIntrinsicOverlap(Box<MessageReactIntrinsicOverlapData<L>>),

    MessageReadonlyArraysCannotBeWrittenTo,
    MessageRecursionLimitExceeded,

    MessageRedeclareComponentProp(Box<MessageRedeclareComponentPropData<L>>),

    MessageShouldAnnotateVariableOnlyInitializedInGenericContext {
        reason: VirtualReason<L>,
        possible_generic_escape_locs: Vec<L>,
    },

    MessageShouldAnnotateVariableUsedInGenericContext(
        Box<MessageShouldAnnotateVariableUsedInGenericContextData<L>>,
    ),

    MessageShouldNotBeCoerced(VirtualReason<L>),
    MessageShouldUseArrayLiteral,

    MessageSketchyNumber(VirtualReason<L>),

    MessageSketchyNullCheck(Box<MessageSketchyNullCheckData<L>>),

    MessageSuppressionMalformedCode,
    MessageSuppressionMissingCode,

    MessageThisInComponent(L),
    MessageThisInExportedFunction,

    MessageThisSuperInObject(VirtualReason<L>, ThisFinderKind),

    MessageTSKeyofType,
    MessageTSNeverType,
    MessageTSParamExtends,
    MessageTSReadonlyOperatorOnArray,
    MessageTSReadonlyOperatorOnTuple,
    MessageTSReadonlyType,
    MessageTSSatisfiesType(CastingSyntax),
    MessageTSVarianceIn,
    MessageTSVarianceInOut,
    MessageTSVarianceOut,
    MessageTSVarianceReadOnly,
    MessageVarianceKeywordWriteonly,
    MessageTSClassAccessibility(ast::class::ts_accessibility::Kind),
    MessageTSParameterProperty,
    MessageAbstractClass,
    MessageAbstractMethod,
    MessageTSUndefinedType,
    MessageTSUnknownType,

    MessageTupleElementNotReadable(Box<MessageTupleElementNotReadableData<L>>),

    MessageTupleElementNotWritable(Box<MessageTupleElementNotWritableData<L>>),

    MessageTupleIndexOutOfBound(Box<MessageTupleIndexOutOfBoundData<L>>),

    MessageTupleNonIntegerIndex(Box<MessageTupleNonIntegerIndexData<L>>),

    MessageTupleNonStaticallyKnownIndex,

    MessageTuplePolarityMismatch {
        index: i32,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        polarity_lower: Polarity,
        polarity_upper: Polarity,
    },

    MessageTypeGuardIndexMismatch {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageTypeGuardImpliesMismatch {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageIncompatiblETypeParamConstIncompatibility {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageTypeParamConstInvalidPosition(VirtualReason<L>),
    MessageUnclearType,

    MessageUnderconstrainedImplicitInstantiaton {
        reason_call: VirtualReason<L>,
        reason_tparam: VirtualReason<L>,
    },

    MessageUndocumentedFeature,

    MessageIllegalAssertOperator {
        obj: VirtualReason<L>,
        specialized: bool,
    },

    MessageUnexpectedUseOfThisType,

    MessageUninitializedInstanceProperty(PropertyAssignmentKind),

    MessageUnknownParameterTypes(VirtualReason<L>),
    MessageUnnecessaryDeclareTypeOnlyExport,
    MessageUnnecessaryInvariant(VirtualReason<L>),
    MessageUnnecessaryOptionalChain(VirtualReason<L>),
    MessageUnreachableCode,
    MessageUnsafeGetterSetter,
    MessageUnsafeObjectAssign,

    MessageUnsupportedKeyInObject {
        key_error_kind: InvalidObjKey,
        obj_kind: ObjKind,
    },

    MessageUnsupportedSyntax(UnsupportedSyntax),
    MessageUnsupportedVarianceAnnotation(FlowSmolStr),

    MessageUntypedImport(FlowSmolStr),
    MessageUntypedTypeImport(FlowSmolStr),

    MessageUnusedPromiseInAsyncScope,
    MessageUnusedPromiseInSyncScope,
    MessageUnusedSuppression,

    MessageValueUsedAsType(VirtualReasonDesc<L>),
    MessageVariableNeverInitAssignedAnnotated(VirtualReason<L>),

    MessageVariableOnlyAssignedByNull(Box<MessageVariableOnlyAssignedByNullData<L>>),

    MessageMatchNotExhaustive {
        examples: Vec<(FlowSmolStr, Vec<VirtualReason<L>>)>,
    },

    MessageMatchUnnecessaryPattern {
        reason: VirtualReason<L>,
        already_seen: Option<VirtualReason<L>>,
    },

    MessageMatchNonExhaustiveObjectPattern(Box<MessageMatchNonExhaustiveObjectPatternData<L>>),

    MessageMatchNonExplicitEnumCheck(Box<MessageMatchNonExplicitEnumCheckData<L>>),

    MessageMatchInvalidGuardedWildcard,

    MessageMatchInvalidIdentOrMemberPattern {
        type_reason: VirtualReason<L>,
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
        binding_reason: VirtualReason<L>,
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
        reason_record: VirtualReason<L>,
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
        show_warning: bool,
        constant_condition_kind: ConstantConditionKind,
        reason: Option<VirtualReason<L>>,
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
