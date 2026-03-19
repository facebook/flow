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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExactnessErrorKind {
    UnexpectedIndexer,
    UnexpectedInexact,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DeclareComponentInvalidParamKind {
    DeclareComponentParamAsBinding,
    DeclareComponentParamDefaultValue,
    DeclareComponentParamMissingAnnotation,
    DeclareComponentParamStringLiteralWithoutAs,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExpectedModulePurpose {
    ReactModuleForJSXFragment,
    ReactModuleForReactClassComponent,
    ReactModuleForReactMixedElementType,
    ReactModuleForReactNodeType,
    ReactModuleForReactRefSetterType,
    ReactModuleForReactElementRefType,
}

#[derive(Debug, Clone, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExponentialSpreadReasonGroup<L: Dupe> {
    pub first_reason: VirtualReason<L>,
    pub second_reason: Option<VirtualReason<L>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ContextDependentUnsupportedStatement {
    ToplevelLibraryImport,
    NonLibdefToplevelDeclareModule,
    UnsupportedStatementInLibdef(FlowSmolStr),
    UnsupportedStatementInDeclareModule(FlowSmolStr),
    UnsupportedStatementInDeclareNamespace(FlowSmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InternalType {
    DollarReactDeepReadOnly,
    DollarUtilityTypeWithNonDollarAliases(FlowSmolStr),
    ReactDollarUtilityTypesWithNonDollarAliases(FlowSmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    TSLibSyntax(TsLibSyntaxKind),
    ExportTypeSpecifierInExportType,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubComponentOfInvariantSubtypingError {
    ObjectProps(Vec<Name>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InvalidRenderTypeKind<L: Dupe> {
    InvalidRendersNullVoidFalse,
    InvalidRendersIterable,
    InvalidRendersStructural(VirtualReason<L>),
    InvalidRendersNonNominalElement(VirtualReason<L>),
    InvalidRendersGenericT,
    UncategorizedInvalidRenders,
}

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ConstantConditionKind {
    ConstCondGeneral,
    UnawaitedPromise,
    UncalledFunction,
}

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NullSide {
    Left,
    Right,
}

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EmptySide {
    Left,
    Right,
}

#[derive(Debug, Clone, Dupe, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StrictComparisonKind {
    StrictComparisonGeneral,
    StrictComparisonNull { null_side: NullSide },
    StrictComparisonEmpty { empty_side: EmptySide },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StrictComparisonInfo<L: Dupe> {
    pub left_precise_reason: VirtualReason<L>,
    pub right_precise_reason: VirtualReason<L>,
    pub strict_comparison_kind: StrictComparisonKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ClassKind {
    Class,
    Record,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum IncorrectTypeErrorType {
    DeprecatedUtility,
    TSType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Explanation<L: Dupe> {
    ExplanationAbstractEnumCasting,
    ExplanationArrayInvariantTyping,
    ExplanationConstrainedAssign {
        name: FlowSmolStr,
        declaration: L,
        providers: Arc<[L]>,
    },
    ExplanationConcreteEnumCasting {
        representation_type: FlowSmolStr,
        casting_syntax: CastingSyntax,
    },
    ExplanationCustomError {
        name: FlowSmolStr,
        custom_error_loc: L,
    },
    ExplanationFunctionsWithStaticsToObject,
    ExplanationInvariantSubtypingDueToMutableArray {
        lower_array_loc: L,
        upper_array_loc: L,
        lower_array_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_array_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_array_reason: VirtualReason<L>,
    },
    ExplanationInvariantSubtypingDueToMutableProperty {
        lower_obj_loc: L,
        upper_obj_loc: L,
        lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_object_reason: VirtualReason<L>,
        property_name: Option<FlowSmolStr>,
    },
    ExplanationInvariantSubtypingDueToMutableProperties {
        lower_obj_loc: L,
        upper_obj_loc: L,
        lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_object_reason: VirtualReason<L>,
        properties: Vec<Name>,
    },
    ExplanationMultiplatform,
    ExplanationPropertyInvariantTyping,
    ExplanationPropertyMissingDueToNeutralOptionalProperty {
        props_plural: bool,
        lower_obj_loc: L,
        upper_obj_loc: L,
        lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_object_reason: VirtualReason<L>,
    },
    ExplanationReactComponentPropsDeepReadOnly(L),
    ExplanationReactHookArgsDeepReadOnly(L),
    ExplanationReactHookIncompatibleWithEachOther,
    ExplanationReactHookIncompatibleWithNormalFunctions,
    ExplanationReactHookReturnDeepReadOnly(L),
    ExplanationIncompatibleReactDeepReadOnly,
    ExplanationTypeGuardPositiveConsistency {
        return_: VirtualReason<L>,
        param: VirtualReason<L>,
        guard_type: VirtualReason<L>,
        is_return_false_statement: bool,
    },
    ExplanationAdditionalUnionMembers {
        left: VirtualReason<L>,
        right: VirtualReason<L>,
        members: Vec<FlowSmolStr>,
        extra_number: i32,
    },
    ExplanationObjectLiteralNeedsRecordSyntax {
        record_name: FlowSmolStr,
        obj_reason: VirtualReason<L>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AccessChainSegment {
    PropSegment(Name),
    TupleIndexSegment(i32),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ThisFinderKind {
    This,
    Super,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PrimitiveKind {
    Boolean,
    Number,
    String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ObjKind {
    Type,
    Literal,
}

#[derive(Debug, Clone)]
pub enum Message<L: Dupe> {
    MessagePlainTextReservedForInternalErrorOnly(FlowSmolStr),

    MessageAlreadyExhaustivelyCheckOneEnumMember {
        member_name: FlowSmolStr,
        prev_check_loc: L,
        enum_reason: VirtualReason<L>,
    },

    MessageAlreadyExhaustivelyCheckAllEnumMembers {
        enum_reason: VirtualReason<L>,
    },

    MessageAmbiguousNumericKeyWithVariance,
    MessageAmbiguousObjectType,

    MessageAnyValueUsedAsType(VirtualReasonDesc<L>),
    MessageBadLibdefModuleOverride(VirtualReason<L>),
    MessageBadLibdefNameOverride(VirtualReason<L>),

    MessageCannotAccessEnumMember {
        member_name: Option<Name>,
        suggestion: Option<FlowSmolStr>,
        description: VirtualReasonDesc<L>,
        enum_reason: VirtualReason<L>,
    },

    MessageCannotAccessObjectWithComputedProp {
        reason_obj: VirtualReason<L>,
        reason_prop: VirtualReason<L>,
        kind: InvalidObjKey,
    },

    MessageCannotAccessReactRefInRender {
        usage: VirtualReason<L>,
        in_hook: bool,
    },

    MessageCannotAddComputedPropertyDueToPotentialOverwrite {
        key_loc: L,
        overwritten_locs: Vec<L>,
    },

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

    MessageCannotCallMaybeReactHook {
        callee_loc: L,
        hooks: Vec<L>,
        non_hooks: Vec<L>,
    },

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

    MessageCannotCompare {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        strict_comparison_opt: Option<StrictComparisonInfo<L>>,
    },

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

    MessageCannotExportRenamedDefault {
        name: Option<FlowSmolStr>,
        is_reexport: bool,
    },

    MessageCannotExhaustivelyCheckAbstractEnums {
        description: VirtualReasonDesc<L>,
        enum_reason: VirtualReason<L>,
    },

    MessageCannotExhaustivelyCheckEnumWithUnknowns {
        description: VirtualReasonDesc<L>,
        enum_reason: VirtualReason<L>,
    },

    MessageCannotImplementNonInterface(VirtualReasonDesc<L>),

    MessageCannotInstantiateObjectUtilTypeWithEnum {
        description: VirtualReasonDesc<L>,
        enum_reason: VirtualReason<L>,
    },

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

    MessageCannotResolveBuiltinModule {
        name: FlowSmolStr,
        potential_generator: Option<FlowSmolStr>,
    },

    MessageCannotResolveExpectedModule {
        name: FlowSmolStr,
        expected_module_purpose: ExpectedModulePurpose,
    },

    MessageCannotSpreadDueToPotentialOverwrite {
        spread_reason: VirtualReason<L>,
        object_reason: VirtualReason<L>,
        key_reason: VirtualReason<L>,
    },

    MessageCannotSpreadGeneral {
        spread_reason: VirtualReason<L>,
        object1_reason: VirtualReason<L>,
        object2_reason: VirtualReason<L>,
        propname: Name,
        error_kind: ExactnessErrorKind,
    },

    MessageCannotSpreadInexactMayOverwriteIndexer {
        spread_reason: VirtualReason<L>,
        object2_reason: VirtualReason<L>,
        key_reason: VirtualReason<L>,
        value_reason: VirtualReason<L>,
    },

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

    MessageCannotUseEnumMemberUsedAsType {
        description: VirtualReasonDesc<L>,
        enum_reason: VirtualReason<L>,
    },

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

    MessageCannotUseTypeForAnnotationInference {
        reason_op: VirtualReason<L>,
        reason: VirtualReason<L>,
        suggestion: Option<FlowSmolStr>,
    },

    MessageCannotUseTypeGuardWithFunctionParamHavoced {
        type_guard_desc: VirtualReasonDesc<L>,
        param_reason: VirtualReason<L>,
        call_locs: Vec<L>,
    },

    MessageCannotUseTypeInValuePosition {
        reason: VirtualReason<L>,
        type_only_namespace: bool,
        imported_name: Option<FlowSmolStr>,
    },

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

    MessageDefinitionInvalidRecursive {
        description: VirtualReasonDesc<L>,
        recursion: Vec<L>,
        annot_locs: Vec<AnnotLoc<L>>,
    },

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

    MessageDuplicateModuleProvider {
        module_name: FlowSmolStr,
        provider: L,
        conflict: L,
    },

    MessageEnumsNotEnabled,

    MessageEnumConstNotSupported,

    MessageInvalidEnumMemberName {
        member_name: String,
        enum_reason: VirtualReason<L>,
    },

    MessageExponentialSpread {
        reason: VirtualReason<L>,
        reasons_for_operand1: ExponentialSpreadReasonGroup<L>,
        reasons_for_operand2: ExponentialSpreadReasonGroup<L>,
    },

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

    MessageIncompatibleTupleArity {
        lower_reason: VirtualReason<L>,
        lower_arity: (i32, i32),
        lower_inexact: bool,
        upper_reason: VirtualReason<L>,
        upper_arity: (i32, i32),
        upper_inexact: bool,
        unify: bool,
    },

    MessageIncompatibleImplicitReturn {
        lower: VirtualReason<L>,
        upper: VirtualReasonDesc<L>,
    },

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

    MessageIncompatibleGeneralWithPrintedTypes {
        lower_loc: L,
        upper_loc: L,
        lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    },

    MessageIncompatibleDueToInvariantSubtyping {
        sub_component: Option<SubComponentOfInvariantSubtypingError>,
        lower_loc: L,
        upper_loc: L,
        lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    },

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

    MessageIncompatibleReactDeepReadOnly {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        dro_loc: L,
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

    MessageIncompleteExhausiveCheckEnum {
        description: VirtualReasonDesc<L>,
        enum_reason: VirtualReason<L>,
        left_to_check: Vec<FlowSmolStr>,
        default_case_loc: Option<L>,
    },

    MessageIncorrectType(IncorrectType),

    MessageInvalidArgument {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    MessageInvalidCatchParameterAnnotation {
        ts_utility_syntax: bool,
    },

    MessageInvalidComponentRestParam,

    MessageInvalidEnumMemberCheck {
        enum_reason: VirtualReason<L>,
        example_member: Option<FlowSmolStr>,
        from_match: bool,
    },

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

    MessageInvalidRefPropertyInSpread {
        ref_loc: L,
        spread_loc: L,
    },

    MessageInvalidKeyPropertyInSpread {
        key_loc: L,
        spread_loc: L,
    },

    MessageInvalidRendersTypeArgument {
        renders_variant: RendersVariant,
        invalid_render_type_kind: InvalidRenderTypeKind<L>,
        invalid_type_reasons: Vec1<VirtualReason<L>>,
    },

    MessageInvalidSelfReferencingTypeAnnotation {
        name: FlowSmolStr,
        loc: L,
    },

    MessageInvalidSelfReferencingDefault {
        name: FlowSmolStr,
        def_loc: L,
        ref_loc: L,
    },

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

    MessageMissingPlatformSupportWithAvailablePlatforms {
        available_platforms: BTreeSet<FlowSmolStr>,
        required_platforms: BTreeSet<FlowSmolStr>,
    },

    MessageMissingPlatformSupport {
        missing_platforms: BTreeSet<FlowSmolStr>,
    },

    MessageNoDefaultExport {
        module_name: FlowSmolStr,
        suggestion: Option<FlowSmolStr>,
    },

    MessageNoNamedExport {
        module_name: FlowSmolStr,
        export_name: FlowSmolStr,
        suggestion: Option<FlowSmolStr>,
    },

    MessageNonConstVarExport(Option<VirtualReason<L>>),
    MessageNonStrictImport,
    MessageNonToplevelExport,

    MessageOnlyDefaultExport {
        module_name: FlowSmolStr,
        export_name: FlowSmolStr,
    },

    MessageParseError(ParseError),
    MessagePlatformSpecificImplementationModuleLookupFailed(FlowSmolStr),

    MessagePropExtraAgainstExactObject {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        props: Vec1<FlowSmolStr>,
    },

    MessagePropMissing {
        lower: VirtualReason<L>,
        upper: Option<VirtualReason<L>>,
        prop: Option<FlowSmolStr>,
        suggestion: Option<FlowSmolStr>,
        reason_indexer: Option<VirtualReason<L>>,
    },

    MessagePropsMissing {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        props: Vec1<FlowSmolStr>,
    },

    MessagePropPolarityMismatch {
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        props: Vec1<(Option<FlowSmolStr>, Polarity, Polarity)>,
    },

    MessagePropNotReadable(Option<Name>),
    MessagePropNotWritable(Option<Name>),

    MessageReactIntrinsicOverlap {
        use_: VirtualReason<L>,
        def: L,
        type_: L,
        mixed: bool,
    },

    MessageReadonlyArraysCannotBeWrittenTo,
    MessageRecursionLimitExceeded,

    MessageRedeclareComponentProp {
        duplicates: Vec1<(L, Name, L)>,
        spread_loc: L,
    },

    MessageShouldAnnotateVariableOnlyInitializedInGenericContext {
        reason: VirtualReason<L>,
        possible_generic_escape_locs: Vec<L>,
    },

    MessageShouldAnnotateVariableUsedInGenericContext {
        reason: VirtualReason<L>,
        null_loc: L,
        initialized: bool,
        possible_generic_escape_locs: Vec<L>,
    },

    MessageShouldNotBeCoerced(VirtualReason<L>),
    MessageShouldUseArrayLiteral,

    MessageSketchyNumber(VirtualReason<L>),

    MessageSketchyNullCheck {
        kind: SketchyNullKind,
        falsy_loc: L,
        null_loc: L,
    },

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
    MessageTSClassAccessibility(ast::class::ts_accessibility::Kind),
    MessageTSParameterProperty,
    MessageAbstractClass,
    MessageAbstractMethod,
    MessageTSUndefinedType,
    MessageTSUnknownType,

    MessageTupleElementNotReadable {
        reason: VirtualReason<L>,
        index: i32,
        name: Option<FlowSmolStr>,
    },

    MessageTupleElementNotWritable {
        reason: VirtualReason<L>,
        index: i32,
        name: Option<FlowSmolStr>,
    },

    MessageTupleIndexOutOfBound {
        reason_op: VirtualReason<L>,
        inexact: bool,
        length: i32,
        index: FlowSmolStr,
    },

    MessageTupleNonIntegerIndex {
        index_def_loc: L,
        index: FlowSmolStr,
    },

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

    MessageVariableOnlyAssignedByNull {
        reason: VirtualReason<L>,
        null_loc: Option<L>,
    },

    MessageMatchNotExhaustive {
        examples: Vec<(FlowSmolStr, Vec<VirtualReason<L>>)>,
    },

    MessageMatchUnnecessaryPattern {
        reason: VirtualReason<L>,
        already_seen: Option<VirtualReason<L>>,
    },

    MessageMatchNonExhaustiveObjectPattern {
        rest: Option<VirtualReason<L>>,
        missing_props: Vec<FlowSmolStr>,
        pattern_kind: MatchObjPatternKind,
    },

    MessageMatchNonExplicitEnumCheck {
        wildcard_reason: VirtualReason<L>,
        unchecked_members: Vec<FlowSmolStr>,
    },

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

#[derive(Debug, Clone)]
pub struct IntermediateError<L: Dupe> {
    pub kind: ErrorKind,
    pub loc: Loc,
    pub error_code: Option<ErrorCode>,
    pub root: Option<(Loc, RootMessage<L>)>,
    pub message: ErrorMessage<L>,
    pub misplaced_source_file: Option<FileKey>,
    pub unsuppressable: bool,
}

#[derive(Debug, Clone)]
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
