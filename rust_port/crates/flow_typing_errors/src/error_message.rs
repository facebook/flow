/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::flow_import_specifier::Userland;
use flow_common::options::CastingSyntax;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::VirtualReason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::is_scalar_reason;
use flow_common::refinement_invalidation;
use flow_common_errors::error_codes::ErrorCode;
use flow_common_errors::error_utils::ErrorKind;
use flow_common_errors::error_utils::InferWarningKind;
use flow_common_errors::error_utils::friendly::MessageFeature;
use flow_common_errors::error_utils::friendly::code;
use flow_common_errors::error_utils::friendly::text;
use flow_common_ty::ty::ALocTy;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_env_builder::env_api::AnnotLoc;
use flow_env_builder::env_api::DefLocType;
use flow_env_builder::env_api::EnvInvariantFailure;
use flow_lint_settings::lint_settings::LintParseError;
use flow_lint_settings::lints::DeprecatedTypeKind;
use flow_lint_settings::lints::LintKind;
use flow_lint_settings::lints::PropertyAssignmentKind;
use flow_lint_settings::lints::SketchyNullKind;
use flow_lint_settings::lints::SketchyNumberKind;
use flow_parser::ast;
use flow_parser::ast::VariableKind;
use flow_parser::ast::match_pattern::MatchPattern;
use flow_parser::ast::types::RendersVariant;
use flow_parser::loc::Loc;
use flow_parser::parse_error::ParseError;
use flow_parser_utils::graphql::GraphqlError;
use flow_type_sig::signature_error::BindingValidation;
use flow_type_sig::signature_error::SignatureError;
use flow_typing_type::type_::DroType;
use flow_typing_type::type_::UnionEnum;
use flow_typing_type::type_::VirtualFrameUseOp;
use flow_typing_type::type_::VirtualRootUseOp;
use flow_typing_type::type_::VirtualUseOp;
use flow_typing_type::type_::fold_virtual_use_op;
use flow_typing_type::type_::type_or_type_desc;
pub use flow_typing_type::type_::type_or_type_desc::TypeOrTypeDescT as TypeOrTypeDesc;
use flow_typing_type::type_::union_rep::OptimizedError;
use flow_typing_type::type_util::mod_loc_of_virtual_use_op;
use vec1::Vec1;

use crate::intermediate_error_types::AssignedConstLikeBindingType;
use crate::intermediate_error_types::ClassKind;
use crate::intermediate_error_types::ConstantConditionKind;
use crate::intermediate_error_types::DeclareComponentInvalidParamKind;
use crate::intermediate_error_types::DocblockError;
use crate::intermediate_error_types::ExactnessErrorKind;
use crate::intermediate_error_types::ExpectedModulePurpose;
use crate::intermediate_error_types::Explanation;
use crate::intermediate_error_types::ExplanationWithLazyParts;
use crate::intermediate_error_types::ExponentialSpreadReasonGroup;
use crate::intermediate_error_types::IncorrectType;
use crate::intermediate_error_types::IncorrectTypeErrorType;
use crate::intermediate_error_types::InternalType;
use crate::intermediate_error_types::InvalidObjKey;
use crate::intermediate_error_types::InvalidRenderTypeKind;
use crate::intermediate_error_types::MatchInvalidCaseSyntax;
use crate::intermediate_error_types::MatchObjPatternKind;
use crate::intermediate_error_types::Message;
use crate::intermediate_error_types::ObjKind;
use crate::intermediate_error_types::PrimitiveKind;
use crate::intermediate_error_types::RecordDeclarationInvalidSyntax;
use crate::intermediate_error_types::StrictComparisonInfo;
use crate::intermediate_error_types::SubComponentOfInvariantSubtypingError;
use crate::intermediate_error_types::UnsupportedSyntax;

/// Data struct for boxed `ErrorMessage::EIncompatibleSpeculation` variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EIncompatibleSpeculationData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub use_op: Option<VirtualUseOp<L>>,
    pub branches: Vec<ErrorMessage<L>>,
}

/// Data struct for boxed `ErrorMessage::EIncompatibleDefs` variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EIncompatibleDefsData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub branches: Vec<ErrorMessage<L>>,
}

/// Data struct for boxed `ErrorMessage::EPropsNotFoundInInvariantSubtyping` variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EPropsNotFoundInInvariantSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop_names: Vec1<Name>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub lower_obj_loc: L,
    pub upper_obj_loc: L,
    pub lower_obj_desc: TypeOrTypeDesc<L>,
    pub upper_obj_desc: TypeOrTypeDesc<L>,
    pub use_op: VirtualUseOp<L>,
}

/// Data struct for boxed `ErrorMessage::EUnionSpeculationFailed` variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EUnionSpeculationFailedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason: VirtualReason<L>,
    pub op_reasons: Vec1<VirtualReason<L>>,
    pub branches: Vec<ErrorMessage<L>>,
}

/// Data struct for boxed `ErrorMessage::EInvariantSubtypingWithUseOp` variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EInvariantSubtypingWithUseOpData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub sub_component: Option<SubComponentOfInvariantSubtypingError>,
    pub use_op: VirtualUseOp<L>,
    pub lower_loc: L,
    pub upper_loc: L,
    pub lower_desc: TypeOrTypeDesc<L>,
    pub upper_desc: TypeOrTypeDesc<L>,
    pub explanation: Option<ExplanationWithLazyParts<L>>,
}

/// Data struct for boxed `ErrorMessage::EUnionPartialOptimizationNonUniqueKey` variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EUnionPartialOptimizationNonUniqueKeyData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub non_unique_keys: BTreeMap<Name, BTreeMap<UnionEnum, Vec1<VirtualReason<L>>>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EnumKind {
    ConcreteEnumKind,
    AbstractEnumKind,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum EnumErrorKind<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    EnumsNotEnabled(L),
    EnumConstNotSupported(L),
    EnumInvalidMemberAccess {
        member_name: Option<Name>,
        suggestion: Option<FlowSmolStr>,
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
    },
    EnumModification {
        loc: L,
        enum_reason: VirtualReason<L>,
    },
    EnumMemberDuplicateValue {
        loc: L,
        prev_use_loc: L,
        enum_reason: VirtualReason<L>,
    },
    EnumInvalidObjectUtilType {
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
    },
    EnumInvalidObjectFunction {
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
    },
    EnumNotIterable {
        reason: VirtualReason<L>,
        for_in: bool,
    },
    EnumMemberAlreadyChecked {
        case_test_loc: L,
        prev_check_loc: L,
        enum_reason: VirtualReason<L>,
        member_name: FlowSmolStr,
    },
    EnumAllMembersAlreadyChecked {
        loc: L,
        enum_reason: VirtualReason<L>,
    },
    EnumNotAllChecked {
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
        left_to_check: Vec<FlowSmolStr>,
        default_case_loc: Option<L>,
    },
    EnumUnknownNotChecked {
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
    },
    EnumInvalidCheck {
        loc: L,
        enum_reason: VirtualReason<L>,
        example_member: Option<FlowSmolStr>,
        from_match: bool,
    },
    EnumMemberUsedAsType {
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
    },
    EnumIncompatible {
        use_op: VirtualUseOp<L>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        enum_kind: EnumKind,
        representation_type: Option<FlowSmolStr>,
        casting_syntax: CastingSyntax,
    },
    EnumInvalidAbstractUse {
        reason: VirtualReason<L>,
        enum_reason: VirtualReason<L>,
    },
    EnumInvalidMemberName {
        loc: L,
        enum_reason: VirtualReason<L>,
        member_name: String,
    },
    EnumNonIdentifierMemberName {
        loc: L,
        enum_reason: VirtualReason<L>,
        member_name: String,
    },
    EnumDuplicateMemberName {
        loc: L,
        prev_use_loc: L,
        enum_reason: VirtualReason<L>,
        member_name: String,
    },
    EnumInconsistentMemberValues {
        loc: L,
        enum_reason: VirtualReason<L>,
    },
    EnumInvalidMemberInitializer {
        loc: L,
        enum_reason: VirtualReason<L>,
        explicit_type: Option<flow_parser::ast::statement::enum_declaration::ExplicitType>,
        member_name: String,
    },
    EnumBooleanMemberNotInitialized {
        loc: L,
        enum_reason: VirtualReason<L>,
        member_name: String,
    },
    EnumNumberMemberNotInitialized {
        loc: L,
        enum_reason: VirtualReason<L>,
        member_name: String,
    },
    EnumBigIntMemberNotInitialized {
        loc: L,
        enum_reason: VirtualReason<L>,
        member_name: String,
    },
    EnumStringMemberInconsistentlyInitialized {
        loc: L,
        enum_reason: VirtualReason<L>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum MatchErrorKind<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    MatchNotExhaustive {
        loc: L,
        examples: Vec<(FlowSmolStr, Vec<VirtualReason<L>>)>,
        missing_pattern_asts: Vec<MatchPattern<Loc, Loc>>,
    },
    MatchUnusedPattern {
        reason: VirtualReason<L>,
        already_seen: Option<VirtualReason<L>>,
    },
    MatchNonExhaustiveObjectPattern {
        loc: L,
        rest: Option<VirtualReason<L>>,
        missing_props: Vec<FlowSmolStr>,
        pattern_kind: MatchObjPatternKind,
    },
    MatchNonExplicitEnumCheck {
        loc: L,
        wildcard_reason: VirtualReason<L>,
        unchecked_members: Vec<FlowSmolStr>,
    },
    MatchInvalidGuardedWildcard(L),
    MatchInvalidIdentOrMemberPattern {
        loc: L,
        type_reason: VirtualReason<L>,
    },
    MatchInvalidBindingKind {
        loc: L,
        kind: VariableKind,
    },
    MatchInvalidObjectPropertyLiteral {
        loc: L,
        pattern_kind: MatchObjPatternKind,
    },
    MatchInvalidUnaryZero {
        loc: L,
    },
    MatchInvalidUnaryPlusBigInt {
        loc: L,
    },
    MatchDuplicateObjectProperty {
        loc: L,
        name: FlowSmolStr,
        pattern_kind: MatchObjPatternKind,
    },
    MatchBindingInOrPattern {
        loc: L,
    },
    MatchInvalidAsPattern {
        loc: L,
    },
    MatchInvalidPatternReference {
        loc: L,
        binding_reason: VirtualReason<L>,
    },
    MatchInvalidObjectShorthand {
        loc: L,
        name: FlowSmolStr,
        pattern_kind: MatchObjPatternKind,
    },
    MatchStatementInvalidBody {
        loc: L,
    },
    MatchInvalidCaseSyntax {
        loc: L,
        kind: MatchInvalidCaseSyntax<L>,
    },
    MatchInvalidWildcardSyntax(L),
    MatchInvalidInstancePattern(L),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RecordErrorKind<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    RecordBannedTypeUtil {
        reason_op: VirtualReason<L>,
        reason_record: VirtualReason<L>,
    },
    RecordInvalidName {
        loc: L,
        name: FlowSmolStr,
    },
    RecordInvalidNew {
        loc: L,
        record_name: FlowSmolStr,
    },
    RecordDeclarationInvalidSyntax {
        loc: L,
        kind: RecordDeclarationInvalidSyntax<L>,
    },
}

/// Error message types for Flow type errors.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ErrorMessage<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    EIncompatible {
        lower: (VirtualReason<L>, Option<LowerKind>),
        upper: (VirtualReason<L>, UpperKind<L>),
        use_op: Option<VirtualUseOp<L>>,
    },

    EIncompatibleSpeculation(Box<EIncompatibleSpeculationData<L>>),

    EIncompatibleDefs(Box<EIncompatibleDefsData<L>>),

    EIncompatibleProp {
        prop: Option<Name>,
        reason_prop: VirtualReason<L>,
        reason_obj: VirtualReason<L>,
        special: Option<LowerKind>,
        use_op: Option<VirtualUseOp<L>>,
    },

    EExportValueAsType(VirtualReason<L>, Name),

    EImportValueAsType(VirtualReason<L>, FlowSmolStr),

    EImportTypeAsTypeof(VirtualReason<L>, FlowSmolStr),

    EImportTypeAsValue(VirtualReason<L>, FlowSmolStr),

    ENoDefaultExport(VirtualReason<L>, Userland, Option<FlowSmolStr>),

    EOnlyDefaultExport(VirtualReason<L>, Userland, FlowSmolStr),

    ENoNamedExport(VirtualReason<L>, Userland, FlowSmolStr, Option<FlowSmolStr>),

    EMissingTypeArgs {
        reason_op: VirtualReason<L>,
        reason_tapp: VirtualReason<L>,
        arity_loc: L,
        min_arity: i32,
        max_arity: i32,
    },

    EAnyValueUsedAsType {
        reason_use: VirtualReason<L>,
    },

    EValueUsedAsType {
        reason_use: VirtualReason<L>,
    },

    EExpectedStringLit {
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EExpectedNumberLit {
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EExpectedBooleanLit {
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EExpectedBigIntLit {
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EPropNotFoundInLookup {
        prop_name: Option<Name>,
        reason_prop: VirtualReason<L>,
        reason_obj: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
        suggestion: Option<FlowSmolStr>,
    },

    EPropNotFoundInSubtyping {
        prop_name: Option<Name>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
        suggestion: Option<FlowSmolStr>,
    },

    EPropsNotFoundInSubtyping {
        prop_names: Vec1<Name>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EPropsNotFoundInInvariantSubtyping(Box<EPropsNotFoundInInvariantSubtypingData<L>>),

    EPropsExtraAgainstExactObject {
        prop_names: Vec1<Name>,
        reason_l_obj: VirtualReason<L>,
        reason_r_obj: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EIndexerCheckFailed {
        prop_name: Name,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        reason_indexer: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EPropNotReadable {
        reason_prop: VirtualReason<L>,
        prop_name: Option<Name>,
        use_op: VirtualUseOp<L>,
    },

    EPropNotWritable {
        reason_prop: VirtualReason<L>,
        prop_name: Option<Name>,
        use_op: VirtualUseOp<L>,
    },

    EPropPolarityMismatch {
        lreason: VirtualReason<L>,
        ureason: VirtualReason<L>,
        props: Vec1<(Option<Name>, (Polarity, Polarity))>,
        use_op: VirtualUseOp<L>,
    },

    EPolarityMismatch {
        reason: VirtualReason<L>,
        name: FlowSmolStr,
        expected_polarity: Polarity,
        actual_polarity: Polarity,
    },

    EBuiltinNameLookupFailed {
        loc: L,
        name: FlowSmolStr,
    },

    EBuiltinModuleLookupFailed {
        loc: L,
        name: FlowSmolStr,
        potential_generator: Option<FlowSmolStr>,
    },

    EExpectedModuleLookupFailed {
        loc: L,
        name: FlowSmolStr,
        expected_module_purpose: ExpectedModulePurpose,
    },

    EPrivateLookupFailed((VirtualReason<L>, VirtualReason<L>), Name, VirtualUseOp<L>),

    EPlatformSpecificImplementationModuleLookupFailed {
        loc: L,
        name: FlowSmolStr,
    },

    EComparison {
        r1: VirtualReason<L>,
        r2: VirtualReason<L>,
        loc_opt: Option<L>,
        strict_comparison_opt: Option<StrictComparisonInfo<L>>,
    },

    ENonStrictEqualityComparison(VirtualReason<L>, VirtualReason<L>),

    ETupleArityMismatch {
        use_op: VirtualUseOp<L>,
        lower_reason: VirtualReason<L>,
        lower_arity: (i32, i32),
        lower_inexact: bool,
        upper_reason: VirtualReason<L>,
        upper_arity: (i32, i32),
        upper_inexact: bool,
        unify: bool,
    },

    ENonLitArrayToTuple((VirtualReason<L>, VirtualReason<L>), VirtualUseOp<L>),

    ETupleOutOfBounds {
        use_op: VirtualUseOp<L>,
        reason: VirtualReason<L>,
        reason_op: VirtualReason<L>,
        inexact: bool,
        length: i32,
        index: FlowSmolStr,
    },

    ETupleNonIntegerIndex {
        use_op: VirtualUseOp<L>,
        reason: VirtualReason<L>,
        index: FlowSmolStr,
    },

    ETupleUnsafeWrite {
        reason: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    ETupleElementNotReadable {
        reason: VirtualReason<L>,
        index: i32,
        name: Option<FlowSmolStr>,
        use_op: VirtualUseOp<L>,
    },

    ETupleElementNotWritable {
        reason: VirtualReason<L>,
        index: i32,
        name: Option<FlowSmolStr>,
        use_op: VirtualUseOp<L>,
    },

    ETupleElementPolarityMismatch {
        index: i32,
        reason_lower: VirtualReason<L>,
        polarity_lower: Polarity,
        reason_upper: VirtualReason<L>,
        polarity_upper: Polarity,
        use_op: VirtualUseOp<L>,
    },

    ETupleRequiredAfterOptional {
        reason_tuple: VirtualReason<L>,
        reason_required: VirtualReason<L>,
        reason_optional: VirtualReason<L>,
    },

    ETupleInvalidTypeSpread {
        reason_spread: VirtualReason<L>,
        reason_arg: VirtualReason<L>,
    },

    ETupleElementAfterInexactSpread(VirtualReason<L>),

    EROArrayWrite((VirtualReason<L>, VirtualReason<L>), VirtualUseOp<L>),

    EUnionSpeculationFailed(Box<EUnionSpeculationFailedData<L>>),

    EIncompatibleWithExact(
        (VirtualReason<L>, VirtualReason<L>),
        VirtualUseOp<L>,
        ExactnessErrorKind,
    ),

    EFunctionIncompatibleWithIndexer((VirtualReason<L>, VirtualReason<L>), VirtualUseOp<L>),

    EUnsupportedExact(VirtualReason<L>, VirtualReason<L>),

    EUnexpectedThisType(L),

    ETypeParamArity(L, i32),

    ECallTypeArity {
        call_loc: L,
        is_new: bool,
        reason_arity: VirtualReason<L>,
        expected_arity: i32,
    },

    ETypeParamMinArity(L, i32),

    ETooManyTypeArgs {
        reason_tapp: VirtualReason<L>,
        arity_loc: L,
        maximum_arity: i32,
    },

    ETooFewTypeArgs {
        reason_tapp: VirtualReason<L>,
        arity_loc: L,
        minimum_arity: i32,
    },

    EInvalidInfer(L),

    EConstantCondition {
        loc: L,
        is_truthy: bool,
        show_warning: bool,
        constant_condition_kind: ConstantConditionKind,
        reason: Option<VirtualReason<L>>,
    },

    EInvalidTypeArgs(VirtualReason<L>, VirtualReason<L>),

    EInvalidExtends(VirtualReason<L>),

    EStrUtilTypeNonLiteralArg(L),

    EExportsAnnot(L),

    EInvalidConstructor(VirtualReason<L>),

    EUnsupportedKeyInObject {
        loc: L,
        obj_kind: ObjKind,
        key_error_kind: InvalidObjKey,
    },

    EAmbiguousNumericKeyWithVariance(L),

    ETypeGuardFuncIncompatibility {
        use_op: VirtualUseOp<L>,
        reasons: (VirtualReason<L>, VirtualReason<L>),
    },

    ETypeGuardInvalidParameter {
        type_guard_reason: VirtualReason<L>,
        binding_reason: VirtualReason<L>,
    },

    ETypeGuardIndexMismatch {
        use_op: VirtualUseOp<L>,
        reasons: (VirtualReason<L>, VirtualReason<L>),
    },

    ETypeGuardImpliesMismatch {
        use_op: VirtualUseOp<L>,
        reasons: (VirtualReason<L>, VirtualReason<L>),
    },

    ETypeGuardParamUnbound(VirtualReason<L>),

    ETypeGuardThisParam(VirtualReason<L>),

    ETypeGuardFunctionInvalidWrites {
        reason: VirtualReason<L>,
        type_guard_reason: VirtualReason<L>,
        write_locs: Vec<L>,
    },

    ETypeGuardFunctionParamHavoced {
        type_guard_reason: VirtualReason<L>,
        param_reason: VirtualReason<L>,
        call_locs: Vec<L>,
    },

    ETypeGuardIncompatibleWithFunctionKind {
        loc: L,
        kind: FlowSmolStr,
    },

    ENegativeTypeGuardConsistency {
        reason: VirtualReason<L>,
        return_reason: VirtualReason<L>,
        type_reason: VirtualReason<L>,
    },

    ETypeParamConstIncompatibility {
        use_op: VirtualUseOp<L>,
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    ETypeParamConstInvalidPosition(VirtualReason<L>),

    EInternal(L, InternalError),

    EUnsupportedSyntax(L, UnsupportedSyntax),

    EUseArrayLiteral(L),

    EMissingLocalAnnotation {
        reason: VirtualReason<L>,
        hint_available: bool,
        from_generic_function: bool,
    },

    EBindingError(BindingError, L, Name, L),

    ERecursionLimit(VirtualReason<L>, VirtualReason<L>),

    EUninitializedInstanceProperty(L, PropertyAssignmentKind),

    EEnumError(EnumErrorKind<L>),

    EIndeterminateModuleType(L),

    EBadExportPosition(L),

    EBadExportContext(FlowSmolStr, L),

    EBadDefaultImportAccess(L, VirtualReason<L>),

    EBadDefaultImportDestructuring(L),

    EInvalidImportStarUse(L, VirtualReason<L>),

    ENonConstVarExport(L, Option<VirtualReason<L>>),

    EThisInExportedFunction(L),

    EMixedImportAndRequire(L, VirtualReason<L>),

    EUnsupportedVarianceAnnotation(L, FlowSmolStr),

    EExportRenamedDefault {
        loc: L,
        name: Option<FlowSmolStr>,
        is_reexport: bool,
    },

    EUnreachable(L),

    EInvalidObjectKit {
        reason: VirtualReason<L>,
        reason_op: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EInvalidTypeof(L, FlowSmolStr),

    EBinaryInLHS(VirtualReason<L>),

    EBinaryInRHS(VirtualReason<L>),

    EArithmeticOperand(VirtualReason<L>),

    EForInRHS(VirtualReason<L>),

    EInstanceofRHS(VirtualReason<L>),

    EObjectComputedPropertyAccess {
        reason_obj: VirtualReason<L>,
        reason_prop: VirtualReason<L>,
        kind: InvalidObjKey,
    },

    EObjectComputedPropertyAssign(VirtualReason<L>, Option<VirtualReason<L>>, InvalidObjKey),

    EObjectComputedPropertyPotentialOverwrite {
        key_loc: L,
        overwritten_locs: Vec<L>,
    },

    EInvalidLHSInAssignment(L),

    EIncompatibleWithUseOp {
        use_op: VirtualUseOp<L>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        explanation: Option<Explanation<L>>,
    },

    EInvariantSubtypingWithUseOp(Box<EInvariantSubtypingWithUseOpData<L>>),

    EUnsupportedImplements(VirtualReason<L>),

    ENotAReactComponent {
        reason: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EInvalidReactCreateElement {
        create_element_loc: L,
        invalid_react: VirtualReason<L>,
    },

    EReactElementFunArity(VirtualReason<L>, FlowSmolStr, i32),

    EReactRefInRender {
        usage: VirtualReason<L>,
        kind: RefInRenderKind,
        in_hook: bool,
    },

    EFunctionCallExtraArg(VirtualReason<L>, VirtualReason<L>, i32, VirtualUseOp<L>),

    EUnsupportedSetProto(VirtualReason<L>),

    EDuplicateModuleProvider {
        module_name: FlowSmolStr,
        provider: L,
        conflict: L,
    },

    EParseError(L, ParseError),

    EDocblockError(L, DocblockError),

    EImplicitInexactObject(L),

    EAmbiguousObjectType(L),

    // The string is either the name of a module or "the module that exports `_`".
    EUntypedTypeImport(L, Userland),

    EUntypedImport(L, Userland),

    ENonstrictImport(L),

    EUnclearType(L),

    EDeprecatedBool(L),

    EInternalType(L, InternalType),

    EIncorrectTypeWithReplacement {
        loc: L,
        kind: IncorrectType,
    },

    EUnsafeGettersSetters(L),

    EUnsafeObjectAssign(L),

    EUnusedSuppression(L),

    ECodelessSuppression(L),

    EMalformedCode(L),

    ELintSetting(L, LintParseError),

    ESketchyNullLint {
        kind: SketchyNullKind,
        loc: L,
        null_loc: L,
        falsy_loc: L,
    },

    ESketchyNumberLint(SketchyNumberKind, VirtualReason<L>),

    EInvalidPrototype(L, VirtualReason<L>),

    EUnnecessaryOptionalChain(L, VirtualReason<L>),

    EUnnecessaryInvariant(L, VirtualReason<L>),

    EUnnecessaryDeclareTypeOnlyExport(L),

    ECannotDelete(L, VirtualReason<L>),

    ESignatureBindingValidation(BindingValidation<L>),

    ESignatureVerification(SignatureError<L>),

    EPrimitiveAsInterface {
        use_op: VirtualUseOp<L>,
        reason: VirtualReason<L>,
        interface_reason: VirtualReason<L>,
        kind: PrimitiveKind,
    },

    ECannotSpreadInterface {
        spread_reason: VirtualReason<L>,
        interface_reason: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    ECannotSpreadIndexerOnRight {
        spread_reason: VirtualReason<L>,
        object_reason: VirtualReason<L>,
        key_reason: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EUnableToSpread {
        spread_reason: VirtualReason<L>,
        object1_reason: VirtualReason<L>,
        object2_reason: VirtualReason<L>,
        propname: Name,
        error_kind: ExactnessErrorKind,
        use_op: VirtualUseOp<L>,
    },

    EInexactMayOverwriteIndexer {
        spread_reason: VirtualReason<L>,
        key_reason: VirtualReason<L>,
        value_reason: VirtualReason<L>,
        object2_reason: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EExponentialSpread {
        reason: VirtualReason<L>,
        reasons_for_operand1: ExponentialSpreadReasonGroup<L>,
        reasons_for_operand2: ExponentialSpreadReasonGroup<L>,
    },

    EComputedPropertyWithUnion(VirtualReason<L>),

    EAssignConstLikeBinding {
        loc: L,
        definition: VirtualReason<L>,
        binding_kind: AssignedConstLikeBindingType,
    },

    EImplicitInstantiationUnderconstrainedError {
        reason_call: VirtualReason<L>,
        reason_tparam: VirtualReason<L>,
        bound: FlowSmolStr,
        use_op: VirtualUseOp<L>,
    },

    EClassToObject {
        reason_class: VirtualReason<L>,
        reason_obj: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
        kind: ClassKind,
    },

    EMethodUnbinding {
        use_op: VirtualUseOp<L>,
        reason_prop: VirtualReason<L>,
        reason_op: VirtualReason<L>,
    },

    EHookIncompatible {
        use_op: VirtualUseOp<L>,
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        lower_is_hook: bool,
        hook_is_annot: bool,
    },

    EHookUniqueIncompatible {
        use_op: VirtualUseOp<L>,
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    EHookRuleViolation {
        hook_rule: HookRule<L>,
        callee_loc: L,
        call_loc: L,
    },

    EHookNaming(L),

    EIncompatibleReactDeepReadOnly {
        use_op: VirtualUseOp<L>,
        dro_loc: L,
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
    },

    EObjectThisSuperReference(L, VirtualReason<L>, ThisFinderKind),

    EComponentThisReference {
        component_loc: L,
        this_loc: L,
    },

    EComponentCase(L),

    EDeclareComponentInvalidParam {
        loc: L,
        kind: DeclareComponentInvalidParamKind,
    },

    EComponentMissingReturn(VirtualReason<L>),

    EComponentMissingBody(L),

    EComponentBodyInAmbientContext(L),

    ENestedComponent(VirtualReason<L>),

    ENestedHook(VirtualReason<L>),

    EInvalidDeclaration {
        declaration: VirtualReason<L>,
        null_write: Option<NullWrite<L>>,
        possible_generic_escape_locs: Vec<L>,
    },

    EInvalidGraphQL(L, GraphqlError),

    EAnnotationInference(L, VirtualReason<L>, VirtualReason<L>, Option<FlowSmolStr>),

    ETrivialRecursiveDefinition(L, VirtualReason<L>),

    EDefinitionCycle(Vec1<(VirtualReason<L>, Vec<L>, Vec<AnnotLoc<L>>)>),

    ERecursiveDefinition {
        reason: VirtualReason<L>,
        recursion: Vec<L>,
        annot_locs: Vec<AnnotLoc<L>>,
    },

    EReferenceInAnnotation(L, FlowSmolStr, L),

    EDuplicateClassMember {
        loc: L,
        name: FlowSmolStr,
        is_static: bool,
        class_kind: ClassKind,
    },

    EEmptyArrayNoProvider {
        loc: L,
    },

    EUnusedPromise {
        loc: L,
        async_: bool,
    },

    EBigIntRShift3(VirtualReason<L>),

    EBigIntNumCoerce(VirtualReason<L>),

    EInvalidCatchParameterAnnotation {
        loc: L,
        ts_utility_syntax: bool,
    },

    ETSSyntax {
        kind: TSSyntaxKind,
        loc: L,
    },

    EInvalidBinaryArith {
        reason_out: VirtualReason<L>,
        reason_l: VirtualReason<L>,
        reason_r: VirtualReason<L>,
        kind: flow_typing_type::type_::arith_kind::ArithKind,
    },

    EInvalidMappedType {
        loc: L,
        kind: InvalidMappedTypeErrorKind,
    },

    EDuplicateComponentProp {
        spread: L,
        duplicates: Vec1<(L, Name, L)>,
    },

    ERefComponentProp {
        spread: L,
        loc: L,
    },

    EKeySpreadProp {
        spread: L,
        loc: L,
    },

    EReactIntrinsicOverlap {
        use_loc: VirtualReason<L>,
        def: L,
        type_: L,
        mixed: bool,
    },

    EInvalidComponentRestParam(L),

    EInvalidRendersTypeArgument {
        loc: L,
        renders_variant: RendersVariant,
        invalid_render_type_kind: InvalidRenderTypeKind<L>,
        invalid_type_reasons: Vec1<VirtualReason<L>>,
    },

    EInvalidTypeCastSyntax {
        loc: L,
        enabled_casting_syntax: CastingSyntax,
    },

    EMissingPlatformSupportWithAvailablePlatforms {
        loc: L,
        available_platforms: BTreeSet<FlowSmolStr>,
        required_platforms: BTreeSet<FlowSmolStr>,
    },

    EMissingPlatformSupport {
        loc: L,
        missing_platforms: BTreeSet<FlowSmolStr>,
    },

    EUnionPartialOptimizationNonUniqueKey(Box<EUnionPartialOptimizationNonUniqueKeyData<L>>),

    EUnionOptimization {
        loc: L,
        kind: OptimizedError<L>,
    },

    EUnionOptimizationOnNonUnion {
        loc: L,
        arg: VirtualReason<L>,
    },

    ECannotCallReactComponent {
        reason: VirtualReason<L>,
    },

    EMatchError(MatchErrorKind<L>),

    ERecordError(RecordErrorKind<L>),

    EReferenceInDefault(L, FlowSmolStr, L),

    EUndocumentedFeature {
        loc: L,
    },

    EIllegalAssertOperator {
        op: VirtualReason<L>,
        obj: VirtualReason<L>,
        specialized: bool,
    },

    // Dev only
    EDevOnlyRefinedLocInfo {
        refined_loc: L,
        refining_locs: Vec<L>,
    },

    EDevOnlyInvalidatedRefinementInfo {
        read_loc: L,
        invalidation_info: Vec<(L, refinement_invalidation::Reason)>,
    },

    // As the name suggest, don't use this for production purposes, but feel free to use it to
    // quickly test out some ideas.
    ETemporaryHardcodedErrorForPrototyping(VirtualReason<L>, FlowSmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NullWrite<L: Dupe> {
    pub null_loc: L,
    pub initialized: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RefInRenderKind {
    Argument,
    Access,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindingError {
    EGlobalAlreadyDeclared,
    ENameAlreadyBound,
    ENameAlreadyBoundInCoreJs,
    EVarRedeclaration,
    EReferencedBeforeDeclaration,
    EReferencedThisSuperBeforeSuperCall,
    ETypeInValuePosition {
        imported: bool,
        type_only_namespace: bool,
        name: FlowSmolStr,
    },
    EConstReassigned,
    EConstParamReassigned,
    EImportReassigned,
    EEnumReassigned,
    EReservedKeyword {
        keyword: IncorrectType,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum InternalError {
    MethodNotAFunction,
    OptionalMethod,
    PropertyDescriptorPropertyCannotBeRead,
    ForInLHS,
    ForOfLHS,
    PropRefComputedOpen,
    PropRefComputedLiteral,
    RestParameterNotIdentifierPattern,
    InterfaceTypeSpread,
    DebugThrow,
    ParseJobException(FlowSmolStr),
    CheckTimeout(String),
    CheckJobException(FlowSmolStr),
    UnexpectedAnnotationInference(FlowSmolStr),
    MissingSwitchExhaustiveCheck,
    MissingEnvRead(ALoc),
    MissingEnvWrite(ALoc),
    ReadOfUnreachedTvar(DefLocType),
    ReadOfUnresolvedTvar(DefLocType),
    ForcedReadOfUnderResolutionTvar(DefLocType),
    EnvInvariant(EnvInvariantFailure<ALoc>),
    ImplicitInstantiationInvariant(FlowSmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LowerKind {
    PossiblyNull,
    PossiblyVoid,
    PossiblyNullOrVoid,
    IncompatibleIntersection,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UpperKind<L: Dupe> {
    IncompatibleGetPropT(L, Option<Name>),
    IncompatibleSetPropT(L, Option<Name>),
    IncompatibleGetPrivatePropT,
    IncompatibleSetPrivatePropT,
    IncompatibleMethodT(L, Option<Name>),
    IncompatibleCallT,
    IncompatibleMixedCallT,
    IncompatibleGetElemT(L),
    IncompatibleSetElemT(L),
    IncompatibleCallElemT(L),
    IncompatibleElemTOfArrT,
    IncompatibleObjAssignFromTSpread,
    IncompatibleObjAssignFromT,
    IncompatibleObjRestT,
    IncompatibleArrRestT,
    IncompatibleSuperT,
    IncompatibleMixinT,
    IncompatibleSpecializeT,
    IncompatibleThisSpecializeT,
    IncompatibleVarianceCheckT,
    IncompatibleGetKeysT,
    IncompatibleHasOwnPropT(L, Option<Name>),
    IncompatibleGetValuesT,
    IncompatibleMapTypeTObject,
    IncompatibleGetStaticsT,
    IncompatibleBindT,
    IncompatibleUnclassified(FlowSmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TSSyntaxKind {
    TSUnknown,
    TSNever,
    TSUndefined,
    TSKeyof,
    TSTypeParamExtends,
    TSReadonlyVariance,
    TSInOutVariance(InOutVariance),
    TSSatisfiesType(CastingSyntax),
    TSReadonlyType(Option<ReadonlyTypeKind>),
    TSClassAccessibility(ast::class::ts_accessibility::Kind),
    TSParameterProperty,
    AbstractClass,
    AbstractMethod,
    DeprecatedTypeParamColon,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InvalidMappedTypeErrorKind {
    InterfaceOrDeclaredClass,
    ExtraProperties,
    ExplicitExactOrInexact,
    RemoveOptionality,
    VarianceOnArrayInput,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum InOutVariance {
    In,
    Out,
    InOut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ReadonlyTypeKind {
    Tuple,
    Array,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HookRule<L: Dupe> {
    ConditionalHook,
    HookHasIllegalName,
    NotHookSyntaxHook,
    MaybeHook { hooks: Vec<L>, non_hooks: Vec<L> },
    HookDefinitelyNotInComponentOrHook,
    HookInUnknownContext,
    HookNotInComponentSyntaxComponentOrHookSyntaxHook,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum ThisFinderKind {
    This,
    Super,
}

pub fn string_of_invalid_render_type_kind<L: Dupe>(
    kind: &InvalidRenderTypeKind<L>,
) -> &'static str {
    match kind {
        InvalidRenderTypeKind::InvalidRendersNullVoidFalse => "null | void | false",
        InvalidRenderTypeKind::InvalidRendersIterable => "iterable",
        InvalidRenderTypeKind::InvalidRendersStructural(_) => "non-nominal-return",
        InvalidRenderTypeKind::InvalidRendersNonNominalElement(_) => "non-nominal",
        InvalidRenderTypeKind::InvalidRendersGenericT => "generic",
        InvalidRenderTypeKind::UncategorizedInvalidRenders => "uncategorized",
    }
}

fn map_loc_of_exponential_spread_reason_group<L: Dupe, M: Dupe, F>(
    f: F,
    group: ExponentialSpreadReasonGroup<L>,
) -> ExponentialSpreadReasonGroup<M>
where
    F: Fn(VirtualReason<L>) -> VirtualReason<M>,
{
    ExponentialSpreadReasonGroup {
        first_reason: f(group.first_reason),
        second_reason: group.second_reason.map(f),
    }
}

fn map_loc_of_invalid_render_type_kind<
    L: Dupe,
    M: Dupe,
    F: Fn(VirtualReason<L>) -> VirtualReason<M>,
>(
    f: F,
    kind: InvalidRenderTypeKind<L>,
) -> InvalidRenderTypeKind<M> {
    match kind {
        InvalidRenderTypeKind::InvalidRendersNullVoidFalse => {
            InvalidRenderTypeKind::InvalidRendersNullVoidFalse
        }
        InvalidRenderTypeKind::InvalidRendersIterable => {
            InvalidRenderTypeKind::InvalidRendersIterable
        }
        InvalidRenderTypeKind::InvalidRendersStructural(r) => {
            InvalidRenderTypeKind::InvalidRendersStructural(f(r))
        }
        InvalidRenderTypeKind::InvalidRendersNonNominalElement(r) => {
            InvalidRenderTypeKind::InvalidRendersNonNominalElement(f(r))
        }
        InvalidRenderTypeKind::InvalidRendersGenericT => {
            InvalidRenderTypeKind::InvalidRendersGenericT
        }
        InvalidRenderTypeKind::UncategorizedInvalidRenders => {
            InvalidRenderTypeKind::UncategorizedInvalidRenders
        }
    }
}

fn map_loc_of_lazy_explanation<L: Dupe, M: Dupe, F: Fn(&L) -> M>(
    f: &F,
    explanation: ExplanationWithLazyParts<L>,
) -> ExplanationWithLazyParts<M> {
    let map_reason = |r: VirtualReason<L>| -> VirtualReason<M> { r.map_locs(f) };

    match explanation {
        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableArray {
            lower_array_loc,
            upper_array_loc,
            lower_array_desc,
            upper_array_desc,
            upper_array_reason,
        } => ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableArray {
            lower_array_loc: f(&lower_array_loc),
            upper_array_loc: f(&upper_array_loc),
            lower_array_desc: type_or_type_desc::map_loc(f, lower_array_desc),
            upper_array_desc: type_or_type_desc::map_loc(f, upper_array_desc),
            upper_array_reason: map_reason(upper_array_reason),
        },
        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
            lower_obj_loc,
            upper_obj_loc,
            lower_obj_desc,
            upper_obj_desc,
            upper_object_reason,
            property_name,
        } => ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
            lower_obj_loc: f(&lower_obj_loc),
            upper_obj_loc: f(&upper_obj_loc),
            lower_obj_desc: type_or_type_desc::map_loc(f, lower_obj_desc),
            upper_obj_desc: type_or_type_desc::map_loc(f, upper_obj_desc),
            upper_object_reason: map_reason(upper_object_reason),
            property_name,
        },
        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperties {
            lower_obj_loc,
            upper_obj_loc,
            lower_obj_desc,
            upper_obj_desc,
            upper_object_reason,
            properties,
        } => ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperties {
            lower_obj_loc: f(&lower_obj_loc),
            upper_obj_loc: f(&upper_obj_loc),
            lower_obj_desc: type_or_type_desc::map_loc(f, lower_obj_desc),
            upper_obj_desc: type_or_type_desc::map_loc(f, upper_obj_desc),
            upper_object_reason: map_reason(upper_object_reason),
            properties,
        },
    }
}

fn map_loc_of_explanation<L: Dupe, M: Dupe, F: Fn(&L) -> M>(
    f: &F,
    explanation: Explanation<L>,
) -> Explanation<M> {
    let map_reason = |r: VirtualReason<L>| -> VirtualReason<M> { r.map_locs(f) };
    let map_desc = |d: VirtualReasonDesc<L>| -> VirtualReasonDesc<M> { d.map_locs(f) };

    match explanation {
        Explanation::ExplanationAbstractEnumCasting => Explanation::ExplanationAbstractEnumCasting,
        Explanation::ExplanationArrayInvariantTyping => {
            Explanation::ExplanationArrayInvariantTyping
        }
        Explanation::ExplanationConstrainedAssign {
            name,
            declaration,
            providers,
        } => Explanation::ExplanationConstrainedAssign {
            name,
            declaration: f(&declaration),
            providers: providers.iter().map(f).collect(),
        },
        Explanation::ExplanationConcreteEnumCasting {
            representation_type,
            casting_syntax,
        } => Explanation::ExplanationConcreteEnumCasting {
            representation_type,
            casting_syntax,
        },
        Explanation::ExplanationCustomError {
            name,
            custom_error_loc,
        } => Explanation::ExplanationCustomError {
            name,
            custom_error_loc: f(&custom_error_loc),
        },
        Explanation::ExplanationFunctionsWithStaticsToObject => {
            Explanation::ExplanationFunctionsWithStaticsToObject
        }
        Explanation::ExplanationInvariantSubtypingDueToMutableArray {
            lower_array_loc,
            upper_array_loc,
            lower_array_desc,
            upper_array_desc,
            upper_array_reason,
        } => Explanation::ExplanationInvariantSubtypingDueToMutableArray {
            lower_array_loc: f(&lower_array_loc),
            upper_array_loc: f(&upper_array_loc),
            lower_array_desc: lower_array_desc.map_err(map_desc),
            upper_array_desc: upper_array_desc.map_err(map_desc),
            upper_array_reason: map_reason(upper_array_reason),
        },
        Explanation::ExplanationInvariantSubtypingDueToMutableProperty {
            lower_obj_loc,
            upper_obj_loc,
            lower_obj_desc,
            upper_obj_desc,
            upper_object_reason,
            property_name,
        } => Explanation::ExplanationInvariantSubtypingDueToMutableProperty {
            lower_obj_loc: f(&lower_obj_loc),
            upper_obj_loc: f(&upper_obj_loc),
            lower_obj_desc: lower_obj_desc.map_err(map_desc),
            upper_obj_desc: upper_obj_desc.map_err(map_desc),
            upper_object_reason: map_reason(upper_object_reason),
            property_name,
        },
        Explanation::ExplanationInvariantSubtypingDueToMutableProperties {
            lower_obj_loc,
            upper_obj_loc,
            lower_obj_desc,
            upper_obj_desc,
            upper_object_reason,
            properties,
        } => Explanation::ExplanationInvariantSubtypingDueToMutableProperties {
            lower_obj_loc: f(&lower_obj_loc),
            upper_obj_loc: f(&upper_obj_loc),
            lower_obj_desc: lower_obj_desc.map_err(map_desc),
            upper_obj_desc: upper_obj_desc.map_err(map_desc),
            upper_object_reason: map_reason(upper_object_reason),
            properties,
        },
        Explanation::ExplanationMultiplatform => Explanation::ExplanationMultiplatform,
        Explanation::ExplanationPropertyInvariantTyping => {
            Explanation::ExplanationPropertyInvariantTyping
        }
        Explanation::ExplanationPropertyMissingDueToNeutralOptionalProperty {
            props_plural,
            lower_obj_loc,
            upper_obj_loc,
            lower_obj_desc,
            upper_obj_desc,
            upper_object_reason,
        } => Explanation::ExplanationPropertyMissingDueToNeutralOptionalProperty {
            props_plural,
            lower_obj_loc: f(&lower_obj_loc),
            upper_obj_loc: f(&upper_obj_loc),
            lower_obj_desc: lower_obj_desc.map_err(map_desc),
            upper_obj_desc: upper_obj_desc.map_err(map_desc),
            upper_object_reason: map_reason(upper_object_reason),
        },
        Explanation::ExplanationReactComponentPropsDeepReadOnly(loc) => {
            Explanation::ExplanationReactComponentPropsDeepReadOnly(f(&loc))
        }
        Explanation::ExplanationReactHookArgsDeepReadOnly(loc) => {
            Explanation::ExplanationReactHookArgsDeepReadOnly(f(&loc))
        }
        Explanation::ExplanationReactHookIncompatibleWithEachOther => {
            Explanation::ExplanationReactHookIncompatibleWithEachOther
        }
        Explanation::ExplanationReactHookIncompatibleWithNormalFunctions => {
            Explanation::ExplanationReactHookIncompatibleWithNormalFunctions
        }
        Explanation::ExplanationReactHookReturnDeepReadOnly(loc) => {
            Explanation::ExplanationReactHookReturnDeepReadOnly(f(&loc))
        }
        Explanation::ExplanationIncompatibleReactDeepReadOnly => {
            Explanation::ExplanationIncompatibleReactDeepReadOnly
        }
        Explanation::ExplanationTypeGuardPositiveConsistency {
            return_,
            param,
            guard_type,
            is_return_false_statement,
        } => Explanation::ExplanationTypeGuardPositiveConsistency {
            return_: map_reason(return_),
            param: map_reason(param),
            guard_type: map_reason(guard_type),
            is_return_false_statement,
        },
        Explanation::ExplanationAdditionalUnionMembers {
            left,
            right,
            members,
            extra_number,
        } => Explanation::ExplanationAdditionalUnionMembers {
            left: map_reason(left),
            right: map_reason(right),
            members,
            extra_number,
        },
        Explanation::ExplanationObjectLiteralNeedsRecordSyntax {
            record_name,
            obj_reason,
        } => Explanation::ExplanationObjectLiteralNeedsRecordSyntax {
            record_name,
            obj_reason: map_reason(obj_reason),
        },
    }
}

impl<L: Dupe + PartialEq + Eq + PartialOrd + Ord> ErrorMessage<L> {
    pub fn map_loc_of_error_message<F, M>(f: F, msg: ErrorMessage<L>) -> ErrorMessage<M>
    where
        F: Fn(L) -> M + Clone,
        L: Clone + Dupe + PartialEq + Eq + PartialOrd + Ord,
        M: Clone + Dupe + PartialEq + Eq + PartialOrd + Ord,
    {
        use ErrorMessage::*;

        let map_use_op =
            |use_op: VirtualUseOp<L>| -> VirtualUseOp<M> { mod_loc_of_virtual_use_op(&f, use_op) };
        let map_reason =
            |r: VirtualReason<L>| -> VirtualReason<M> { r.map_locs(|l: &L| f(l.dupe())) };
        let map_branch = |e: ErrorMessage<L>| -> ErrorMessage<M> {
            Self::map_loc_of_error_message(f.clone(), e)
        };
        let map_upper_kind = |uk: UpperKind<L>| -> UpperKind<M> {
            match uk {
                UpperKind::IncompatibleGetPropT(loc, s) => {
                    UpperKind::IncompatibleGetPropT(f(loc), s)
                }
                UpperKind::IncompatibleSetPropT(loc, s) => {
                    UpperKind::IncompatibleSetPropT(f(loc), s)
                }
                UpperKind::IncompatibleMethodT(loc, s) => UpperKind::IncompatibleMethodT(f(loc), s),
                UpperKind::IncompatibleHasOwnPropT(loc, s) => {
                    UpperKind::IncompatibleHasOwnPropT(f(loc), s)
                }
                UpperKind::IncompatibleGetElemT(loc) => UpperKind::IncompatibleGetElemT(f(loc)),
                UpperKind::IncompatibleSetElemT(loc) => UpperKind::IncompatibleSetElemT(f(loc)),
                UpperKind::IncompatibleCallElemT(loc) => UpperKind::IncompatibleCallElemT(f(loc)),
                UpperKind::IncompatibleGetPrivatePropT => UpperKind::IncompatibleGetPrivatePropT,
                UpperKind::IncompatibleSetPrivatePropT => UpperKind::IncompatibleSetPrivatePropT,
                UpperKind::IncompatibleCallT => UpperKind::IncompatibleCallT,
                UpperKind::IncompatibleMixedCallT => UpperKind::IncompatibleMixedCallT,
                UpperKind::IncompatibleElemTOfArrT => UpperKind::IncompatibleElemTOfArrT,
                UpperKind::IncompatibleObjAssignFromTSpread => {
                    UpperKind::IncompatibleObjAssignFromTSpread
                }
                UpperKind::IncompatibleObjAssignFromT => UpperKind::IncompatibleObjAssignFromT,
                UpperKind::IncompatibleObjRestT => UpperKind::IncompatibleObjRestT,
                UpperKind::IncompatibleArrRestT => UpperKind::IncompatibleArrRestT,
                UpperKind::IncompatibleSuperT => UpperKind::IncompatibleSuperT,
                UpperKind::IncompatibleMixinT => UpperKind::IncompatibleMixinT,
                UpperKind::IncompatibleSpecializeT => UpperKind::IncompatibleSpecializeT,
                UpperKind::IncompatibleThisSpecializeT => UpperKind::IncompatibleThisSpecializeT,
                UpperKind::IncompatibleVarianceCheckT => UpperKind::IncompatibleVarianceCheckT,
                UpperKind::IncompatibleGetKeysT => UpperKind::IncompatibleGetKeysT,
                UpperKind::IncompatibleGetValuesT => UpperKind::IncompatibleGetValuesT,
                UpperKind::IncompatibleMapTypeTObject => UpperKind::IncompatibleMapTypeTObject,
                UpperKind::IncompatibleGetStaticsT => UpperKind::IncompatibleGetStaticsT,
                UpperKind::IncompatibleBindT => UpperKind::IncompatibleBindT,
                UpperKind::IncompatibleUnclassified(s) => UpperKind::IncompatibleUnclassified(s),
            }
        };

        match msg {
            EIncompatible {
                use_op,
                lower: (lreason, lkind),
                upper: (ureason, ukind),
            } => EIncompatible {
                use_op: use_op.map(map_use_op),
                lower: (map_reason(lreason), lkind),
                upper: (map_reason(ureason), map_upper_kind(ukind)),
            },

            EIncompatibleSpeculation(box EIncompatibleSpeculationData {
                use_op,
                loc,
                branches,
            }) => EIncompatibleSpeculation(Box::new(EIncompatibleSpeculationData {
                use_op: use_op.map(map_use_op),
                loc: f(loc),
                branches: branches.into_iter().map(map_branch).collect(),
            })),

            EIncompatibleDefs(box EIncompatibleDefsData {
                use_op,
                reason_lower,
                reason_upper,
                branches,
            }) => EIncompatibleDefs(Box::new(EIncompatibleDefsData {
                use_op: map_use_op(use_op),
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                branches: branches.into_iter().map(map_branch).collect(),
            })),

            EIncompatibleProp {
                use_op,
                prop,
                reason_prop,
                reason_obj,
                special,
            } => EIncompatibleProp {
                use_op: use_op.map(map_use_op),
                prop,
                reason_prop: map_reason(reason_prop),
                reason_obj: map_reason(reason_obj),
                special,
            },

            EExpectedStringLit {
                reason_lower,
                reason_upper,
                use_op,
            } => EExpectedStringLit {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            },

            EExpectedNumberLit {
                reason_lower,
                reason_upper,
                use_op,
            } => EExpectedNumberLit {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            },

            EExpectedBooleanLit {
                reason_lower,
                reason_upper,
                use_op,
            } => EExpectedBooleanLit {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            },

            EExpectedBigIntLit {
                reason_lower,
                reason_upper,
                use_op,
            } => EExpectedBigIntLit {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            },

            EPropNotFoundInLookup {
                prop_name,
                reason_prop,
                reason_obj,
                use_op,
                suggestion,
            } => EPropNotFoundInLookup {
                prop_name,
                reason_prop: map_reason(reason_prop),
                reason_obj: map_reason(reason_obj),
                use_op: map_use_op(use_op),
                suggestion,
            },

            EPropNotFoundInSubtyping {
                prop_name,
                reason_lower,
                reason_upper,
                use_op,
                suggestion,
            } => EPropNotFoundInSubtyping {
                prop_name,
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
                suggestion,
            },

            EPropsNotFoundInSubtyping {
                prop_names,
                reason_lower,
                reason_upper,
                use_op,
            } => EPropsNotFoundInSubtyping {
                prop_names,
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            },

            EPropsNotFoundInInvariantSubtyping(box EPropsNotFoundInInvariantSubtypingData {
                prop_names,
                reason_lower,
                reason_upper,
                lower_obj_loc,
                upper_obj_loc,
                lower_obj_desc,
                upper_obj_desc,
                use_op,
            }) => EPropsNotFoundInInvariantSubtyping(Box::new(
                EPropsNotFoundInInvariantSubtypingData {
                    prop_names,
                    reason_lower: map_reason(reason_lower),
                    reason_upper: map_reason(reason_upper),
                    lower_obj_loc: f(lower_obj_loc),
                    upper_obj_loc: f(upper_obj_loc),
                    lower_obj_desc: type_or_type_desc::map_loc(|l: &L| f(l.dupe()), lower_obj_desc),
                    upper_obj_desc: type_or_type_desc::map_loc(|l: &L| f(l.dupe()), upper_obj_desc),
                    use_op: map_use_op(use_op),
                },
            )),

            EPropsExtraAgainstExactObject {
                prop_names,
                reason_l_obj,
                reason_r_obj,
                use_op,
            } => EPropsExtraAgainstExactObject {
                prop_names,
                reason_l_obj: map_reason(reason_l_obj),
                reason_r_obj: map_reason(reason_r_obj),
                use_op: map_use_op(use_op),
            },

            EIndexerCheckFailed {
                prop_name,
                reason_lower,
                reason_upper,
                reason_indexer,
                use_op,
            } => EIndexerCheckFailed {
                prop_name,
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                reason_indexer: map_reason(reason_indexer),
                use_op: map_use_op(use_op),
            },

            EPropNotReadable {
                reason_prop,
                prop_name,
                use_op,
            } => EPropNotReadable {
                reason_prop: map_reason(reason_prop),
                prop_name,
                use_op: map_use_op(use_op),
            },

            EPropNotWritable {
                reason_prop,
                prop_name,
                use_op,
            } => EPropNotWritable {
                reason_prop: map_reason(reason_prop),
                prop_name,
                use_op: map_use_op(use_op),
            },

            EPropPolarityMismatch {
                lreason,
                ureason,
                props,
                use_op,
            } => EPropPolarityMismatch {
                lreason: map_reason(lreason),
                ureason: map_reason(ureason),
                props,
                use_op: map_use_op(use_op),
            },

            EBuiltinNameLookupFailed { loc, name } => {
                EBuiltinNameLookupFailed { loc: f(loc), name }
            }

            EBuiltinModuleLookupFailed {
                loc,
                name,
                potential_generator,
            } => EBuiltinModuleLookupFailed {
                loc: f(loc),
                name,
                potential_generator,
            },

            EExpectedModuleLookupFailed {
                loc,
                name,
                expected_module_purpose,
            } => EExpectedModuleLookupFailed {
                loc: f(loc),
                name,
                expected_module_purpose,
            },

            EPrivateLookupFailed((r1, r2), x, op) => {
                EPrivateLookupFailed((map_reason(r1), map_reason(r2)), x, map_use_op(op))
            }

            EPlatformSpecificImplementationModuleLookupFailed { loc, name } => {
                EPlatformSpecificImplementationModuleLookupFailed { loc: f(loc), name }
            }

            ETupleArityMismatch {
                use_op,
                lower_reason,
                lower_arity,
                lower_inexact,
                upper_reason,
                upper_arity,
                upper_inexact,
                unify,
            } => ETupleArityMismatch {
                use_op: map_use_op(use_op),
                lower_reason: map_reason(lower_reason),
                lower_arity,
                lower_inexact,
                upper_reason: map_reason(upper_reason),
                upper_arity,
                upper_inexact,
                unify,
            },

            ENonLitArrayToTuple((r1, r2), op) => {
                ENonLitArrayToTuple((map_reason(r1), map_reason(r2)), map_use_op(op))
            }

            ETupleOutOfBounds {
                use_op,
                reason,
                reason_op,
                inexact,
                length,
                index,
            } => ETupleOutOfBounds {
                use_op: map_use_op(use_op),
                reason: map_reason(reason),
                reason_op: map_reason(reason_op),
                inexact,
                length,
                index,
            },

            ETupleNonIntegerIndex {
                use_op,
                reason,
                index,
            } => ETupleNonIntegerIndex {
                use_op: map_use_op(use_op),
                reason: map_reason(reason),
                index,
            },

            ETupleUnsafeWrite { reason, use_op } => ETupleUnsafeWrite {
                reason: map_reason(reason),
                use_op: map_use_op(use_op),
            },

            ETupleElementNotReadable {
                reason,
                index,
                name,
                use_op,
            } => ETupleElementNotReadable {
                reason: map_reason(reason),
                index,
                name,
                use_op: map_use_op(use_op),
            },

            ETupleElementNotWritable {
                reason,
                index,
                name,
                use_op,
            } => ETupleElementNotWritable {
                reason: map_reason(reason),
                index,
                name,
                use_op: map_use_op(use_op),
            },

            ETupleElementPolarityMismatch {
                index,
                reason_lower,
                polarity_lower,
                reason_upper,
                polarity_upper,
                use_op,
            } => ETupleElementPolarityMismatch {
                index,
                reason_lower: map_reason(reason_lower),
                polarity_lower,
                reason_upper: map_reason(reason_upper),
                polarity_upper,
                use_op: map_use_op(use_op),
            },

            ETupleRequiredAfterOptional {
                reason_tuple,
                reason_required,
                reason_optional,
            } => ETupleRequiredAfterOptional {
                reason_tuple: map_reason(reason_tuple),
                reason_required: map_reason(reason_required),
                reason_optional: map_reason(reason_optional),
            },

            ETupleInvalidTypeSpread {
                reason_spread,
                reason_arg,
            } => ETupleInvalidTypeSpread {
                reason_spread: map_reason(reason_spread),
                reason_arg: map_reason(reason_arg),
            },

            ETupleElementAfterInexactSpread(reason) => {
                ETupleElementAfterInexactSpread(map_reason(reason))
            }

            EROArrayWrite((r1, r2), op) => {
                EROArrayWrite((map_reason(r1), map_reason(r2)), map_use_op(op))
            }

            EUnionSpeculationFailed(box EUnionSpeculationFailedData {
                use_op,
                reason,
                op_reasons,
                branches,
            }) => EUnionSpeculationFailed(Box::new(EUnionSpeculationFailedData {
                use_op: map_use_op(use_op),
                reason: map_reason(reason),
                op_reasons: Vec1::try_from_vec(op_reasons.into_iter().map(map_reason).collect())
                    .unwrap(),
                branches: branches.into_iter().map(map_branch).collect(),
            })),

            EIncompatibleWithExact((r1, r2), op, kind) => {
                EIncompatibleWithExact((map_reason(r1), map_reason(r2)), map_use_op(op), kind)
            }

            EFunctionIncompatibleWithIndexer((r1, r2), op) => {
                EFunctionIncompatibleWithIndexer((map_reason(r1), map_reason(r2)), map_use_op(op))
            }

            EInvalidConstructor(r) => EInvalidConstructor(map_reason(r)),

            EInvalidObjectKit {
                reason,
                reason_op,
                use_op,
            } => EInvalidObjectKit {
                reason: map_reason(reason),
                reason_op: map_reason(reason_op),
                use_op: map_use_op(use_op),
            },

            EIncompatibleWithUseOp {
                use_op,
                reason_lower,
                reason_upper,
                explanation,
            } => EIncompatibleWithUseOp {
                use_op: map_use_op(use_op),
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                explanation: explanation.map(|e| map_loc_of_explanation(&|l: &L| f(l.dupe()), e)),
            },

            EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
                sub_component,
                use_op,
                lower_loc,
                lower_desc,
                upper_loc,
                upper_desc,
                explanation,
            }) => EInvariantSubtypingWithUseOp(Box::new(EInvariantSubtypingWithUseOpData {
                sub_component,
                use_op: map_use_op(use_op),
                lower_loc: f(lower_loc),
                lower_desc: type_or_type_desc::map_loc(|l: &L| f(l.dupe()), lower_desc),
                upper_loc: f(upper_loc),
                upper_desc: type_or_type_desc::map_loc(|l: &L| f(l.dupe()), upper_desc),
                explanation: explanation
                    .map(|e| map_loc_of_lazy_explanation(&|l: &L| f(l.dupe()), e)),
            })),

            ENotAReactComponent { reason, use_op } => ENotAReactComponent {
                reason: map_reason(reason),
                use_op: map_use_op(use_op),
            },

            EInvalidReactCreateElement {
                create_element_loc,
                invalid_react,
            } => EInvalidReactCreateElement {
                create_element_loc: f(create_element_loc),
                invalid_react: map_reason(invalid_react),
            },

            EFunctionCallExtraArg(rl, ru, n, op) => {
                EFunctionCallExtraArg(map_reason(rl), map_reason(ru), n, map_use_op(op))
            }

            EExportValueAsType(r, s) => EExportValueAsType(map_reason(r), s),
            EImportValueAsType(r, s) => EImportValueAsType(map_reason(r), s),
            EImportTypeAsTypeof(r, s) => EImportTypeAsTypeof(map_reason(r), s),
            EImportTypeAsValue(r, s) => EImportTypeAsValue(map_reason(r), s),

            ENoDefaultExport(r, s1, s2) => ENoDefaultExport(map_reason(r), s1, s2),
            EOnlyDefaultExport(r, s1, s2) => EOnlyDefaultExport(map_reason(r), s1, s2),
            ENoNamedExport(r, s1, s2, s3) => ENoNamedExport(map_reason(r), s1, s2, s3),

            EMissingTypeArgs {
                reason_op,
                reason_tapp,
                arity_loc,
                min_arity,
                max_arity,
            } => EMissingTypeArgs {
                reason_op: map_reason(reason_op),
                reason_tapp: map_reason(reason_tapp),
                arity_loc: f(arity_loc),
                min_arity,
                max_arity,
            },

            EAnyValueUsedAsType { reason_use } => EAnyValueUsedAsType {
                reason_use: map_reason(reason_use),
            },

            EValueUsedAsType { reason_use } => EValueUsedAsType {
                reason_use: map_reason(reason_use),
            },

            EPolarityMismatch {
                reason,
                name,
                expected_polarity,
                actual_polarity,
            } => EPolarityMismatch {
                reason: map_reason(reason),
                name,
                expected_polarity,
                actual_polarity,
            },

            EComparison {
                r1,
                r2,
                loc_opt,
                strict_comparison_opt,
            } => EComparison {
                r1: map_reason(r1),
                r2: map_reason(r2),
                loc_opt: loc_opt.map(&f),
                strict_comparison_opt: strict_comparison_opt.map(|sc| StrictComparisonInfo {
                    left_precise_reason: map_reason(sc.left_precise_reason),
                    right_precise_reason: map_reason(sc.right_precise_reason),
                    strict_comparison_kind: sc.strict_comparison_kind,
                }),
            },

            ENonStrictEqualityComparison(r1, r2) => {
                ENonStrictEqualityComparison(map_reason(r1), map_reason(r2))
            }

            EUnsupportedExact(r1, r2) => EUnsupportedExact(map_reason(r1), map_reason(r2)),

            EUnexpectedThisType(loc) => EUnexpectedThisType(f(loc)),

            ETypeParamArity(loc, i) => ETypeParamArity(f(loc), i),

            ECallTypeArity {
                call_loc,
                is_new,
                reason_arity,
                expected_arity,
            } => ECallTypeArity {
                call_loc: f(call_loc),
                is_new,
                reason_arity: map_reason(reason_arity),
                expected_arity,
            },

            ETypeParamMinArity(loc, i) => ETypeParamMinArity(f(loc), i),

            ETooManyTypeArgs {
                reason_tapp,
                arity_loc,
                maximum_arity,
            } => ETooManyTypeArgs {
                reason_tapp: map_reason(reason_tapp),
                arity_loc: f(arity_loc),
                maximum_arity,
            },

            ETooFewTypeArgs {
                reason_tapp,
                arity_loc,
                minimum_arity,
            } => ETooFewTypeArgs {
                reason_tapp: map_reason(reason_tapp),
                arity_loc: f(arity_loc),
                minimum_arity,
            },

            EInvalidTypeArgs(r1, r2) => EInvalidTypeArgs(map_reason(r1), map_reason(r2)),

            EInvalidInfer(l) => EInvalidInfer(f(l)),

            EConstantCondition {
                loc,
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
            } => EConstantCondition {
                loc: f(loc),
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason: reason.map(map_reason),
            },

            EInvalidExtends(r) => EInvalidExtends(map_reason(r)),

            EStrUtilTypeNonLiteralArg(loc) => EStrUtilTypeNonLiteralArg(f(loc)),

            EExportsAnnot(loc) => EExportsAnnot(f(loc)),

            EUnsupportedKeyInObject {
                loc,
                obj_kind,
                key_error_kind,
            } => EUnsupportedKeyInObject {
                loc: f(loc),
                obj_kind,
                key_error_kind,
            },

            EAmbiguousNumericKeyWithVariance(loc) => EAmbiguousNumericKeyWithVariance(f(loc)),

            ETypeGuardFuncIncompatibility {
                use_op,
                reasons: (r1, r2),
            } => ETypeGuardFuncIncompatibility {
                use_op: map_use_op(use_op),
                reasons: (map_reason(r1), map_reason(r2)),
            },

            ETypeGuardInvalidParameter {
                type_guard_reason,
                binding_reason,
            } => ETypeGuardInvalidParameter {
                type_guard_reason: map_reason(type_guard_reason),
                binding_reason: map_reason(binding_reason),
            },

            ETypeGuardIndexMismatch {
                use_op,
                reasons: (r1, r2),
            } => ETypeGuardIndexMismatch {
                use_op: map_use_op(use_op),
                reasons: (map_reason(r1), map_reason(r2)),
            },

            ETypeGuardImpliesMismatch {
                use_op,
                reasons: (r1, r2),
            } => ETypeGuardImpliesMismatch {
                use_op: map_use_op(use_op),
                reasons: (map_reason(r1), map_reason(r2)),
            },

            ETypeGuardParamUnbound(reason) => ETypeGuardParamUnbound(map_reason(reason)),

            ETypeGuardThisParam(reason) => ETypeGuardThisParam(map_reason(reason)),

            ETypeGuardFunctionInvalidWrites {
                reason,
                type_guard_reason,
                write_locs,
            } => ETypeGuardFunctionInvalidWrites {
                reason: map_reason(reason),
                type_guard_reason: map_reason(type_guard_reason),
                write_locs: write_locs.into_iter().map(&f).collect(),
            },

            ETypeGuardFunctionParamHavoced {
                type_guard_reason,
                param_reason,
                call_locs,
            } => ETypeGuardFunctionParamHavoced {
                type_guard_reason: map_reason(type_guard_reason),
                param_reason: map_reason(param_reason),
                call_locs: call_locs.into_iter().map(&f).collect(),
            },

            ETypeGuardIncompatibleWithFunctionKind { loc, kind } => {
                ETypeGuardIncompatibleWithFunctionKind { loc: f(loc), kind }
            }

            ENegativeTypeGuardConsistency {
                reason,
                return_reason,
                type_reason,
            } => ENegativeTypeGuardConsistency {
                reason: map_reason(reason),
                return_reason: map_reason(return_reason),
                type_reason: map_reason(type_reason),
            },

            ETypeParamConstIncompatibility {
                use_op,
                lower,
                upper,
            } => ETypeParamConstIncompatibility {
                use_op: map_use_op(use_op),
                lower: map_reason(lower),
                upper: map_reason(upper),
            },

            ETypeParamConstInvalidPosition(reason) => {
                ETypeParamConstInvalidPosition(map_reason(reason))
            }

            EInternal(loc, i) => EInternal(f(loc), i),
            EUnsupportedSyntax(loc, u) => EUnsupportedSyntax(f(loc), u),
            EUseArrayLiteral(loc) => EUseArrayLiteral(f(loc)),

            EMissingLocalAnnotation {
                reason,
                hint_available,
                from_generic_function,
            } => EMissingLocalAnnotation {
                reason: map_reason(reason),
                hint_available,
                from_generic_function,
            },

            EBindingError(b, loc, s, scope) => EBindingError(b, f(loc), s, f(scope)),

            ERecursionLimit(r1, r2) => ERecursionLimit(map_reason(r1), map_reason(r2)),

            EUninitializedInstanceProperty(loc, e) => EUninitializedInstanceProperty(f(loc), e),

            EEnumError(enum_error) => {
                use EnumErrorKind::*;
                EEnumError(match enum_error {
                    EnumsNotEnabled(loc) => EnumsNotEnabled(f(loc)),
                    EnumConstNotSupported(loc) => EnumConstNotSupported(f(loc)),
                    EnumInvalidMemberAccess {
                        member_name,
                        suggestion,
                        reason,
                        enum_reason,
                    } => EnumInvalidMemberAccess {
                        member_name,
                        suggestion,
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumModification { loc, enum_reason } => EnumModification {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumMemberDuplicateValue {
                        loc,
                        prev_use_loc,
                        enum_reason,
                    } => EnumMemberDuplicateValue {
                        loc: f(loc),
                        prev_use_loc: f(prev_use_loc),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumInvalidObjectUtilType {
                        reason,
                        enum_reason,
                    } => EnumInvalidObjectUtilType {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumInvalidObjectFunction {
                        reason,
                        enum_reason,
                    } => EnumInvalidObjectFunction {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumNotIterable { reason, for_in } => EnumNotIterable {
                        reason: map_reason(reason),
                        for_in,
                    },
                    EnumMemberAlreadyChecked {
                        case_test_loc,
                        prev_check_loc,
                        enum_reason,
                        member_name,
                    } => EnumMemberAlreadyChecked {
                        case_test_loc: f(case_test_loc),
                        prev_check_loc: f(prev_check_loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    },
                    EnumAllMembersAlreadyChecked { loc, enum_reason } => {
                        EnumAllMembersAlreadyChecked {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                        }
                    }
                    EnumNotAllChecked {
                        reason,
                        enum_reason,
                        left_to_check,
                        default_case_loc,
                    } => EnumNotAllChecked {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                        left_to_check,
                        default_case_loc: default_case_loc.map(&f),
                    },
                    EnumUnknownNotChecked {
                        reason,
                        enum_reason,
                    } => EnumUnknownNotChecked {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumInvalidCheck {
                        loc,
                        enum_reason,
                        example_member,
                        from_match,
                    } => EnumInvalidCheck {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        example_member,
                        from_match,
                    },
                    EnumMemberUsedAsType {
                        reason,
                        enum_reason,
                    } => EnumMemberUsedAsType {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumIncompatible {
                        use_op,
                        reason_lower,
                        reason_upper,
                        enum_kind,
                        representation_type,
                        casting_syntax,
                    } => EnumIncompatible {
                        use_op: map_use_op(use_op),
                        reason_lower: map_reason(reason_lower),
                        reason_upper: map_reason(reason_upper),
                        enum_kind,
                        representation_type,
                        casting_syntax,
                    },
                    EnumInvalidAbstractUse {
                        reason,
                        enum_reason,
                    } => EnumInvalidAbstractUse {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    },
                    EnumInvalidMemberName {
                        loc,
                        enum_reason,
                        member_name,
                    } => EnumInvalidMemberName {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    },
                    EnumNonIdentifierMemberName {
                        loc,
                        enum_reason,
                        member_name,
                    } => EnumNonIdentifierMemberName {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    },
                    EnumDuplicateMemberName {
                        loc,
                        prev_use_loc,
                        enum_reason,
                        member_name,
                    } => EnumDuplicateMemberName {
                        loc: f(loc),
                        prev_use_loc: f(prev_use_loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    },
                    EnumInconsistentMemberValues { loc, enum_reason } => {
                        EnumInconsistentMemberValues {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                        }
                    }
                    EnumInvalidMemberInitializer {
                        loc,
                        enum_reason,
                        explicit_type,
                        member_name,
                    } => EnumInvalidMemberInitializer {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        explicit_type,
                        member_name,
                    },
                    EnumBooleanMemberNotInitialized {
                        loc,
                        enum_reason,
                        member_name,
                    } => EnumBooleanMemberNotInitialized {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    },
                    EnumNumberMemberNotInitialized {
                        loc,
                        enum_reason,
                        member_name,
                    } => EnumNumberMemberNotInitialized {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    },
                    EnumBigIntMemberNotInitialized {
                        loc,
                        enum_reason,
                        member_name,
                    } => EnumBigIntMemberNotInitialized {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    },
                    EnumStringMemberInconsistentlyInitialized { loc, enum_reason } => {
                        EnumStringMemberInconsistentlyInitialized {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                        }
                    }
                })
            }
            EIndeterminateModuleType(loc) => EIndeterminateModuleType(f(loc)),
            EBadExportPosition(loc) => EBadExportPosition(f(loc)),
            EBadExportContext(s, loc) => EBadExportContext(s, f(loc)),

            EBadDefaultImportAccess(loc, r) => EBadDefaultImportAccess(f(loc), map_reason(r)),
            EBadDefaultImportDestructuring(loc) => EBadDefaultImportDestructuring(f(loc)),
            EInvalidImportStarUse(loc, r) => EInvalidImportStarUse(f(loc), map_reason(r)),
            ENonConstVarExport(loc, r) => ENonConstVarExport(f(loc), r.map(map_reason)),
            EThisInExportedFunction(loc) => EThisInExportedFunction(f(loc)),
            EMixedImportAndRequire(loc, r) => EMixedImportAndRequire(f(loc), map_reason(r)),
            EUnsupportedVarianceAnnotation(loc, k) => EUnsupportedVarianceAnnotation(f(loc), k),

            EExportRenamedDefault {
                loc,
                name,
                is_reexport,
            } => EExportRenamedDefault {
                loc: f(loc),
                name,
                is_reexport,
            },

            EUnreachable(loc) => EUnreachable(f(loc)),
            EInvalidTypeof(loc, s) => EInvalidTypeof(f(loc), s),
            EBinaryInLHS(r) => EBinaryInLHS(map_reason(r)),
            EBinaryInRHS(r) => EBinaryInRHS(map_reason(r)),
            EArithmeticOperand(r) => EArithmeticOperand(map_reason(r)),
            EForInRHS(r) => EForInRHS(map_reason(r)),
            EInstanceofRHS(r) => EInstanceofRHS(map_reason(r)),

            EObjectComputedPropertyAccess {
                reason_obj,
                reason_prop,
                kind,
            } => EObjectComputedPropertyAccess {
                reason_obj: map_reason(reason_obj),
                reason_prop: map_reason(reason_prop),
                kind,
            },

            EObjectComputedPropertyAssign(r1, r2, kind) => {
                EObjectComputedPropertyAssign(map_reason(r1), r2.map(map_reason), kind)
            }

            EObjectComputedPropertyPotentialOverwrite {
                key_loc,
                overwritten_locs,
            } => EObjectComputedPropertyPotentialOverwrite {
                key_loc: f(key_loc),
                overwritten_locs: overwritten_locs.into_iter().map(&f).collect(),
            },

            EInvalidLHSInAssignment(l) => EInvalidLHSInAssignment(f(l)),
            EUnsupportedImplements(r) => EUnsupportedImplements(map_reason(r)),

            EReactElementFunArity(r, s, i) => EReactElementFunArity(map_reason(r), s, i),

            EReactRefInRender {
                usage,
                kind,
                in_hook,
            } => EReactRefInRender {
                usage: map_reason(usage),
                kind,
                in_hook,
            },

            EUnsupportedSetProto(r) => EUnsupportedSetProto(map_reason(r)),

            EDuplicateModuleProvider {
                module_name,
                provider,
                conflict,
            } => EDuplicateModuleProvider {
                module_name,
                provider: f(provider),
                conflict: f(conflict),
            },

            EParseError(loc, p) => EParseError(f(loc), p),
            EDocblockError(loc, e) => EDocblockError(f(loc), e),
            EImplicitInexactObject(loc) => EImplicitInexactObject(f(loc)),
            EAmbiguousObjectType(loc) => EAmbiguousObjectType(f(loc)),
            EUntypedTypeImport(loc, s) => EUntypedTypeImport(f(loc), s),
            EUntypedImport(loc, s) => EUntypedImport(f(loc), s),
            ENonstrictImport(loc) => ENonstrictImport(f(loc)),
            EUnclearType(loc) => EUnclearType(f(loc)),
            EDeprecatedBool(loc) => EDeprecatedBool(f(loc)),
            EInternalType(loc, kind) => EInternalType(f(loc), kind),

            EIncorrectTypeWithReplacement { loc, kind } => {
                EIncorrectTypeWithReplacement { loc: f(loc), kind }
            }

            EUnsafeGettersSetters(loc) => EUnsafeGettersSetters(f(loc)),
            EUnsafeObjectAssign(loc) => EUnsafeObjectAssign(f(loc)),
            EUnusedSuppression(loc) => EUnusedSuppression(f(loc)),
            ECodelessSuppression(loc) => ECodelessSuppression(f(loc)),
            ELintSetting(loc, err) => ELintSetting(f(loc), err),

            ESketchyNullLint {
                kind,
                loc,
                null_loc,
                falsy_loc,
            } => ESketchyNullLint {
                kind,
                loc: f(loc),
                null_loc: f(null_loc),
                falsy_loc: f(falsy_loc),
            },

            ESketchyNumberLint(kind, r) => ESketchyNumberLint(kind, map_reason(r)),
            EInvalidPrototype(loc, r) => EInvalidPrototype(f(loc), map_reason(r)),
            EUnnecessaryOptionalChain(loc, r) => EUnnecessaryOptionalChain(f(loc), map_reason(r)),
            EUnnecessaryInvariant(loc, r) => EUnnecessaryInvariant(f(loc), map_reason(r)),
            EUnnecessaryDeclareTypeOnlyExport(loc) => EUnnecessaryDeclareTypeOnlyExport(f(loc)),
            ECannotDelete(l1, r1) => ECannotDelete(f(l1), map_reason(r1)),

            ESignatureBindingValidation(sve) => {
                ESignatureBindingValidation(map_binding_validation(&f, sve))
            }

            ESignatureVerification(sve) => ESignatureVerification(map_signature_error(&f, sve)),

            EPrimitiveAsInterface {
                use_op,
                reason,
                interface_reason,
                kind,
            } => EPrimitiveAsInterface {
                use_op: map_use_op(use_op),
                reason: map_reason(reason),
                interface_reason: map_reason(interface_reason),
                kind,
            },

            ECannotSpreadInterface {
                spread_reason,
                interface_reason,
                use_op,
            } => ECannotSpreadInterface {
                spread_reason: map_reason(spread_reason),
                interface_reason: map_reason(interface_reason),
                use_op: map_use_op(use_op),
            },

            ECannotSpreadIndexerOnRight {
                spread_reason,
                object_reason,
                key_reason,
                use_op,
            } => ECannotSpreadIndexerOnRight {
                spread_reason: map_reason(spread_reason),
                object_reason: map_reason(object_reason),
                key_reason: map_reason(key_reason),
                use_op: map_use_op(use_op),
            },

            EUnableToSpread {
                spread_reason,
                object1_reason,
                object2_reason,
                propname,
                error_kind,
                use_op,
            } => EUnableToSpread {
                spread_reason: map_reason(spread_reason),
                object1_reason: map_reason(object1_reason),
                object2_reason: map_reason(object2_reason),
                propname,
                error_kind,
                use_op: map_use_op(use_op),
            },

            EInexactMayOverwriteIndexer {
                spread_reason,
                key_reason,
                value_reason,
                object2_reason,
                use_op,
            } => EInexactMayOverwriteIndexer {
                spread_reason: map_reason(spread_reason),
                key_reason: map_reason(key_reason),
                value_reason: map_reason(value_reason),
                object2_reason: map_reason(object2_reason),
                use_op: map_use_op(use_op),
            },

            EExponentialSpread {
                reason,
                reasons_for_operand1,
                reasons_for_operand2,
            } => EExponentialSpread {
                reason: map_reason(reason),
                reasons_for_operand1: map_loc_of_exponential_spread_reason_group(
                    map_reason,
                    reasons_for_operand1,
                ),
                reasons_for_operand2: map_loc_of_exponential_spread_reason_group(
                    map_reason,
                    reasons_for_operand2,
                ),
            },

            EComputedPropertyWithUnion(reason) => EComputedPropertyWithUnion(map_reason(reason)),

            EAssignConstLikeBinding {
                loc,
                definition,
                binding_kind,
            } => EAssignConstLikeBinding {
                loc: f(loc),
                definition: map_reason(definition),
                binding_kind,
            },

            EMalformedCode(loc) => EMalformedCode(f(loc)),

            EImplicitInstantiationUnderconstrainedError {
                reason_call,
                reason_tparam,
                bound,
                use_op,
            } => EImplicitInstantiationUnderconstrainedError {
                reason_call: map_reason(reason_call),
                reason_tparam: map_reason(reason_tparam),
                bound,
                use_op: map_use_op(use_op),
            },

            EClassToObject {
                reason_class,
                reason_obj,
                use_op,
                kind,
            } => EClassToObject {
                reason_class: map_reason(reason_class),
                reason_obj: map_reason(reason_obj),
                use_op: map_use_op(use_op),
                kind,
            },

            EMethodUnbinding {
                use_op,
                reason_op,
                reason_prop,
            } => EMethodUnbinding {
                use_op: map_use_op(use_op),
                reason_op: map_reason(reason_op),
                reason_prop: map_reason(reason_prop),
            },

            EHookIncompatible {
                use_op,
                lower,
                upper,
                lower_is_hook,
                hook_is_annot,
            } => EHookIncompatible {
                use_op: map_use_op(use_op),
                lower: map_reason(lower),
                upper: map_reason(upper),
                lower_is_hook,
                hook_is_annot,
            },

            EHookUniqueIncompatible {
                use_op,
                lower,
                upper,
            } => EHookUniqueIncompatible {
                use_op: map_use_op(use_op),
                lower: map_reason(lower),
                upper: map_reason(upper),
            },

            EIncompatibleReactDeepReadOnly {
                use_op,
                lower,
                upper,
                dro_loc,
            } => EIncompatibleReactDeepReadOnly {
                use_op: map_use_op(use_op),
                lower: map_reason(lower),
                upper: map_reason(upper),
                dro_loc: f(dro_loc),
            },

            EHookRuleViolation {
                callee_loc,
                call_loc,
                hook_rule,
            } => {
                let hook_rule = match hook_rule {
                    HookRule::MaybeHook { hooks, non_hooks } => HookRule::MaybeHook {
                        hooks: hooks.into_iter().map(&f).collect(),
                        non_hooks: non_hooks.into_iter().map(&f).collect(),
                    },
                    HookRule::HookHasIllegalName => HookRule::HookHasIllegalName,
                    HookRule::NotHookSyntaxHook => HookRule::NotHookSyntaxHook,
                    HookRule::HookDefinitelyNotInComponentOrHook => {
                        HookRule::HookDefinitelyNotInComponentOrHook
                    }
                    HookRule::HookInUnknownContext => HookRule::HookInUnknownContext,
                    HookRule::HookNotInComponentSyntaxComponentOrHookSyntaxHook => {
                        HookRule::HookNotInComponentSyntaxComponentOrHookSyntaxHook
                    }
                    HookRule::ConditionalHook => HookRule::ConditionalHook,
                };
                EHookRuleViolation {
                    callee_loc: f(callee_loc),
                    call_loc: f(call_loc),
                    hook_rule,
                }
            }

            EHookNaming(l) => EHookNaming(f(l)),

            EObjectThisSuperReference(loc, r, k) => {
                EObjectThisSuperReference(f(loc), map_reason(r), k)
            }

            EComponentThisReference {
                component_loc,
                this_loc,
            } => EComponentThisReference {
                component_loc: f(component_loc),
                this_loc: f(this_loc),
            },

            EComponentCase(loc) => EComponentCase(f(loc)),
            EDeclareComponentInvalidParam { loc, kind } => {
                EDeclareComponentInvalidParam { loc: f(loc), kind }
            }
            EComponentMissingReturn(r) => EComponentMissingReturn(map_reason(r)),
            EComponentMissingBody(loc) => EComponentMissingBody(f(loc)),
            EComponentBodyInAmbientContext(loc) => EComponentBodyInAmbientContext(f(loc)),
            ENestedComponent(r) => ENestedComponent(map_reason(r)),
            ENestedHook(r) => ENestedHook(map_reason(r)),

            EInvalidDeclaration {
                declaration,
                null_write,
                possible_generic_escape_locs,
            } => EInvalidDeclaration {
                declaration: map_reason(declaration),
                null_write: null_write.map(|nw| NullWrite {
                    null_loc: f(nw.null_loc),
                    initialized: nw.initialized,
                }),
                possible_generic_escape_locs: possible_generic_escape_locs
                    .into_iter()
                    .map(&f)
                    .collect(),
            },

            EInvalidGraphQL(loc, err) => EInvalidGraphQL(f(loc), err),

            EAnnotationInference(loc, r1, r2, suggestion) => {
                EAnnotationInference(f(loc), map_reason(r1), map_reason(r2), suggestion)
            }

            ETrivialRecursiveDefinition(loc, r) => {
                ETrivialRecursiveDefinition(f(loc), map_reason(r))
            }

            EDefinitionCycle(elts) => EDefinitionCycle(
                Vec1::try_from_vec(
                    elts.into_iter()
                        .map(|(reason, recur, annot)| {
                            (
                                map_reason(reason),
                                recur.into_iter().map(&f).collect(),
                                annot
                                    .into_iter()
                                    .map(|a| match a {
                                        AnnotLoc::Loc(l) => AnnotLoc::Loc(f(l)),
                                        AnnotLoc::Object { loc, props } => AnnotLoc::Object {
                                            loc: f(loc),
                                            props: props.into_iter().map(&f).collect(),
                                        },
                                    })
                                    .collect(),
                            )
                        })
                        .collect(),
                )
                .unwrap(),
            ),

            ERecursiveDefinition {
                reason,
                recursion,
                annot_locs,
            } => ERecursiveDefinition {
                reason: map_reason(reason),
                recursion: recursion.into_iter().map(&f).collect(),
                annot_locs: annot_locs
                    .into_iter()
                    .map(|a| match a {
                        AnnotLoc::Loc(l) => AnnotLoc::Loc(f(l)),
                        AnnotLoc::Object { loc, props } => AnnotLoc::Object {
                            loc: f(loc),
                            props: props.into_iter().map(&f).collect(),
                        },
                    })
                    .collect(),
            },

            EReferenceInAnnotation(bind_loc, name, loc) => {
                EReferenceInAnnotation(f(bind_loc), name, f(loc))
            }
            EReferenceInDefault(bind_loc, name, loc) => {
                EReferenceInDefault(f(bind_loc), name, f(loc))
            }

            EDuplicateClassMember {
                loc,
                name,
                is_static,
                class_kind,
            } => EDuplicateClassMember {
                loc: f(loc),
                name,
                is_static,
                class_kind,
            },

            EEmptyArrayNoProvider { loc } => EEmptyArrayNoProvider { loc: f(loc) },
            EUnusedPromise { loc, async_ } => EUnusedPromise {
                loc: f(loc),
                async_,
            },

            EReactIntrinsicOverlap {
                use_loc: use_reason,
                def,
                type_,
                mixed,
            } => EReactIntrinsicOverlap {
                use_loc: map_reason(use_reason),
                def: f(def),
                type_: f(type_),
                mixed,
            },

            EInvalidComponentRestParam(loc) => EInvalidComponentRestParam(f(loc)),
            EBigIntRShift3(r) => EBigIntRShift3(map_reason(r)),
            EBigIntNumCoerce(r) => EBigIntNumCoerce(map_reason(r)),

            EInvalidCatchParameterAnnotation {
                loc,
                ts_utility_syntax,
            } => EInvalidCatchParameterAnnotation {
                loc: f(loc),
                ts_utility_syntax,
            },

            ETSSyntax { kind, loc } => ETSSyntax { kind, loc: f(loc) },

            EInvalidBinaryArith {
                reason_out,
                reason_l,
                reason_r,
                kind,
            } => EInvalidBinaryArith {
                reason_out: map_reason(reason_out),
                reason_l: map_reason(reason_l),
                reason_r: map_reason(reason_r),
                kind,
            },

            EInvalidMappedType { loc, kind } => EInvalidMappedType { loc: f(loc), kind },

            EDuplicateComponentProp { spread, duplicates } => EDuplicateComponentProp {
                spread: f(spread),
                duplicates: Vec1::try_from_vec(
                    duplicates
                        .into_iter()
                        .map(|(first, name, second)| (f(first), name, f(second)))
                        .collect(),
                )
                .unwrap(),
            },

            ERefComponentProp { spread, loc } => ERefComponentProp {
                spread: f(spread),
                loc: f(loc),
            },

            EKeySpreadProp { spread, loc } => EKeySpreadProp {
                spread: f(spread),
                loc: f(loc),
            },

            EInvalidRendersTypeArgument {
                loc,
                renders_variant,
                invalid_render_type_kind,
                invalid_type_reasons,
            } => EInvalidRendersTypeArgument {
                loc: f(loc),
                renders_variant,
                invalid_render_type_kind: map_loc_of_invalid_render_type_kind(
                    map_reason,
                    invalid_render_type_kind,
                ),
                invalid_type_reasons: Vec1::try_from_vec(
                    invalid_type_reasons.into_iter().map(map_reason).collect(),
                )
                .unwrap(),
            },

            EInvalidTypeCastSyntax {
                loc,
                enabled_casting_syntax,
            } => EInvalidTypeCastSyntax {
                loc: f(loc),
                enabled_casting_syntax,
            },

            EMissingPlatformSupportWithAvailablePlatforms {
                loc,
                available_platforms,
                required_platforms,
            } => EMissingPlatformSupportWithAvailablePlatforms {
                loc: f(loc),
                available_platforms,
                required_platforms,
            },

            EMissingPlatformSupport {
                loc,
                missing_platforms,
            } => EMissingPlatformSupport {
                loc: f(loc),
                missing_platforms,
            },

            EUnionPartialOptimizationNonUniqueKey(
                box EUnionPartialOptimizationNonUniqueKeyData {
                    loc,
                    non_unique_keys,
                },
            ) => EUnionPartialOptimizationNonUniqueKey(Box::new(
                EUnionPartialOptimizationNonUniqueKeyData {
                    loc: f(loc),
                    non_unique_keys: non_unique_keys
                        .into_iter()
                        .map(|(name, inner_map)| {
                            (
                                name,
                                inner_map
                                    .into_iter()
                                    .map(|(enum_key, reasons)| {
                                        (enum_key, reasons.mapped(map_reason))
                                    })
                                    .collect(),
                            )
                        })
                        .collect(),
                },
            )),

            EUnionOptimization { loc, kind } => {
                let kind = match kind {
                    OptimizedError::ContainsUnresolved(r) => {
                        OptimizedError::ContainsUnresolved(map_reason(r))
                    }
                    OptimizedError::NoCandidateMembers => OptimizedError::NoCandidateMembers,
                    OptimizedError::NoCommonKeys => OptimizedError::NoCommonKeys,
                };
                EUnionOptimization { loc: f(loc), kind }
            }

            EUnionOptimizationOnNonUnion { loc, arg } => EUnionOptimizationOnNonUnion {
                loc: f(loc),
                arg: map_reason(arg),
            },

            ECannotCallReactComponent { reason } => ECannotCallReactComponent {
                reason: map_reason(reason),
            },

            EDevOnlyRefinedLocInfo {
                refined_loc,
                refining_locs,
            } => EDevOnlyRefinedLocInfo {
                refined_loc: f(refined_loc),
                refining_locs: refining_locs.into_iter().map(&f).collect(),
            },

            EMatchError(match_error) => {
                use MatchErrorKind::*;
                EMatchError(match match_error {
                    MatchNotExhaustive {
                        loc,
                        examples,
                        missing_pattern_asts,
                    } => MatchNotExhaustive {
                        loc: f(loc),
                        examples: examples
                            .into_iter()
                            .map(|(pattern, reasons)| {
                                (pattern, reasons.into_iter().map(&map_reason).collect())
                            })
                            .collect(),
                        missing_pattern_asts,
                    },
                    MatchUnusedPattern {
                        reason,
                        already_seen,
                    } => MatchUnusedPattern {
                        reason: map_reason(reason),
                        already_seen: already_seen.map(map_reason),
                    },
                    MatchNonExhaustiveObjectPattern {
                        loc,
                        rest,
                        missing_props,
                        pattern_kind,
                    } => MatchNonExhaustiveObjectPattern {
                        loc: f(loc),
                        rest: rest.map(map_reason),
                        missing_props,
                        pattern_kind,
                    },
                    MatchNonExplicitEnumCheck {
                        loc,
                        wildcard_reason,
                        unchecked_members,
                    } => MatchNonExplicitEnumCheck {
                        loc: f(loc),
                        wildcard_reason: map_reason(wildcard_reason),
                        unchecked_members,
                    },
                    MatchInvalidGuardedWildcard(loc) => MatchInvalidGuardedWildcard(f(loc)),
                    MatchInvalidIdentOrMemberPattern { loc, type_reason } => {
                        MatchInvalidIdentOrMemberPattern {
                            loc: f(loc),
                            type_reason: map_reason(type_reason),
                        }
                    }
                    MatchInvalidBindingKind { loc, kind } => {
                        MatchInvalidBindingKind { loc: f(loc), kind }
                    }
                    MatchInvalidObjectPropertyLiteral { loc, pattern_kind } => {
                        MatchInvalidObjectPropertyLiteral {
                            loc: f(loc),
                            pattern_kind,
                        }
                    }
                    MatchInvalidUnaryZero { loc } => MatchInvalidUnaryZero { loc: f(loc) },
                    MatchInvalidUnaryPlusBigInt { loc } => {
                        MatchInvalidUnaryPlusBigInt { loc: f(loc) }
                    }
                    MatchDuplicateObjectProperty {
                        loc,
                        name,
                        pattern_kind,
                    } => MatchDuplicateObjectProperty {
                        loc: f(loc),
                        name,
                        pattern_kind,
                    },
                    MatchBindingInOrPattern { loc } => MatchBindingInOrPattern { loc: f(loc) },
                    MatchInvalidAsPattern { loc } => MatchInvalidAsPattern { loc: f(loc) },
                    MatchInvalidPatternReference {
                        loc,
                        binding_reason,
                    } => MatchInvalidPatternReference {
                        loc: f(loc),
                        binding_reason: map_reason(binding_reason),
                    },
                    MatchInvalidObjectShorthand {
                        loc,
                        name,
                        pattern_kind,
                    } => MatchInvalidObjectShorthand {
                        loc: f(loc),
                        name,
                        pattern_kind,
                    },
                    MatchStatementInvalidBody { loc } => MatchStatementInvalidBody { loc: f(loc) },
                    MatchInvalidCaseSyntax { loc, kind } => {
                        use crate::intermediate_error_types::MatchInvalidCaseSyntax as MICS;
                        let kind = match kind {
                            MICS::InvalidMatchCaseMultiple {
                                invalid_prefix_case_locs,
                                invalid_infix_colon_locs,
                                invalid_suffix_semicolon_locs,
                            } => MICS::InvalidMatchCaseMultiple {
                                invalid_prefix_case_locs: invalid_prefix_case_locs
                                    .into_iter()
                                    .map(&f)
                                    .collect(),
                                invalid_infix_colon_locs: invalid_infix_colon_locs
                                    .into_iter()
                                    .map(&f)
                                    .collect(),
                                invalid_suffix_semicolon_locs: invalid_suffix_semicolon_locs
                                    .into_iter()
                                    .map(&f)
                                    .collect(),
                            },
                            MICS::InvalidMatchCasePrefixCase => MICS::InvalidMatchCasePrefixCase,
                            MICS::InvalidMatchCaseInfixColon => MICS::InvalidMatchCaseInfixColon,
                            MICS::InvalidMatchCaseSuffixSemicolon => {
                                MICS::InvalidMatchCaseSuffixSemicolon
                            }
                        };
                        MatchInvalidCaseSyntax { loc: f(loc), kind }
                    }
                    MatchInvalidWildcardSyntax(loc) => MatchInvalidWildcardSyntax(f(loc)),
                    MatchInvalidInstancePattern(loc) => MatchInvalidInstancePattern(f(loc)),
                })
            }

            ERecordError(record_error) => ERecordError(match record_error {
                RecordErrorKind::RecordBannedTypeUtil {
                    reason_op,
                    reason_record,
                } => RecordErrorKind::RecordBannedTypeUtil {
                    reason_op: map_reason(reason_op),
                    reason_record: map_reason(reason_record),
                },
                RecordErrorKind::RecordInvalidName { loc, name } => {
                    RecordErrorKind::RecordInvalidName { loc: f(loc), name }
                }
                RecordErrorKind::RecordInvalidNew { loc, record_name } => {
                    RecordErrorKind::RecordInvalidNew {
                        loc: f(loc),
                        record_name,
                    }
                }
                RecordErrorKind::RecordDeclarationInvalidSyntax { loc, kind } => {
                    let kind = match kind {
                            RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxMultiple {
                                invalid_infix_equals_loc,
                                invalid_variance_locs,
                                invalid_optional_locs,
                                invalid_suffix_semicolon_locs,
                            } => RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxMultiple {
                                invalid_infix_equals_loc: invalid_infix_equals_loc.map(&f),
                                invalid_variance_locs: invalid_variance_locs.into_iter().map(&f).collect(),
                                invalid_optional_locs: invalid_optional_locs.into_iter().map(&f).collect(),
                                invalid_suffix_semicolon_locs: invalid_suffix_semicolon_locs.into_iter().map(&f).collect(),
                            },
                            RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxVariance => {
                                RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxVariance
                            },
                            RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxOptional => {
                                RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxOptional
                            },
                            RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxSuffixSemicolon => {
                                RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxSuffixSemicolon
                            },
                            RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxInfixEquals => {
                                RecordDeclarationInvalidSyntax::InvalidRecordDeclarationSyntaxInfixEquals
                            },
                        };
                    RecordErrorKind::RecordDeclarationInvalidSyntax { loc: f(loc), kind }
                }
            }),

            EUndocumentedFeature { loc } => EUndocumentedFeature { loc: f(loc) },

            EIllegalAssertOperator {
                op,
                obj,
                specialized,
            } => EIllegalAssertOperator {
                op: map_reason(op),
                obj: map_reason(obj),
                specialized,
            },

            EDevOnlyInvalidatedRefinementInfo {
                read_loc,
                invalidation_info,
            } => EDevOnlyInvalidatedRefinementInfo {
                read_loc: f(read_loc),
                invalidation_info: invalidation_info
                    .into_iter()
                    .map(|(l, r)| (f(l), r))
                    .collect(),
            },

            ETemporaryHardcodedErrorForPrototyping(r, s) => {
                ETemporaryHardcodedErrorForPrototyping(map_reason(r), s)
            }
        }
    }

    pub fn convert_type_to_type_desc<F>(f: F, msg: ErrorMessage<L>) -> ErrorMessage<L>
    where
        F: Fn(TypeOrTypeDesc<L>) -> TypeOrTypeDesc<L> + Clone,
        L: Clone,
    {
        use self::ErrorMessage::*;
        fn map_use_op<
            L: Dupe + PartialEq + Eq + PartialOrd + Ord,
            F: Fn(TypeOrTypeDesc<L>) -> TypeOrTypeDesc<L> + Clone,
        >(
            f: &F,
            use_op: VirtualUseOp<L>,
        ) -> VirtualUseOp<L> {
            match use_op {
                VirtualUseOp::Op(_) => use_op,
                VirtualUseOp::Frame(frame_rc, inner_op)
                    if matches!(
                        frame_rc.as_ref(),
                        VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility { .. }
                    ) =>
                {
                    let (lower, upper, lower_t, upper_t, name, custom_error_loc) =
                        if let VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility {
                            lower,
                            upper,
                            lower_t,
                            upper_t,
                            name,
                            custom_error_loc,
                        } = frame_rc.as_ref()
                        {
                            (lower, upper, lower_t, upper_t, name, custom_error_loc)
                        } else {
                            unreachable!()
                        };
                    VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility {
                            lower: lower.dupe(),
                            upper: upper.dupe(),
                            lower_t: f(lower_t.clone()),
                            upper_t: f(upper_t.clone()),
                            name: name.dupe(),
                            custom_error_loc: custom_error_loc.dupe(),
                        }),
                        Arc::new(map_use_op(f, inner_op.as_ref().clone())),
                    )
                }
                VirtualUseOp::Frame(frame, inner_op) => VirtualUseOp::Frame(
                    frame.clone(),
                    Arc::new(map_use_op(f, inner_op.as_ref().clone())),
                ),
            }
        }
        let map_explanation =
            |explanation: Option<ExplanationWithLazyParts<L>>| -> Option<ExplanationWithLazyParts<L>> {
                explanation.map(|e| match e {
                    ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableArray {
                        lower_array_loc,
                        upper_array_loc,
                        lower_array_desc,
                        upper_array_desc,
                        upper_array_reason,
                    } => ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableArray {
                        lower_array_loc,
                        upper_array_loc,
                        lower_array_desc: f(lower_array_desc),
                        upper_array_desc: f(upper_array_desc),
                        upper_array_reason,
                    },
                    ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
                        lower_obj_loc,
                        upper_obj_loc,
                        lower_obj_desc,
                        upper_obj_desc,
                        upper_object_reason,
                        property_name,
                    } => ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
                        lower_obj_loc,
                        upper_obj_loc,
                        lower_obj_desc: f(lower_obj_desc),
                        upper_obj_desc: f(upper_obj_desc),
                        upper_object_reason,
                        property_name,
                    },
                    ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperties {
                        lower_obj_loc,
                        upper_obj_loc,
                        lower_obj_desc,
                        upper_obj_desc,
                        upper_object_reason,
                        properties,
                    } => ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperties {
                        lower_obj_loc,
                        upper_obj_loc,
                        lower_obj_desc: f(lower_obj_desc),
                        upper_obj_desc: f(upper_obj_desc),
                        upper_object_reason,
                        properties,
                    },
                })
            };

        match msg {
            EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
                sub_component,
                lower_loc,
                upper_loc,
                lower_desc,
                upper_desc,
                use_op,
                explanation,
            }) => EInvariantSubtypingWithUseOp(Box::new(EInvariantSubtypingWithUseOpData {
                sub_component,
                lower_loc,
                upper_loc,
                lower_desc: f(lower_desc),
                upper_desc: f(upper_desc),
                use_op: map_use_op(&f, use_op),
                explanation: map_explanation(explanation),
            })),

            EIncompatibleWithUseOp {
                use_op,
                reason_lower,
                reason_upper,
                explanation,
            } => EIncompatibleWithUseOp {
                use_op: map_use_op(&f, use_op),
                reason_lower,
                reason_upper,
                explanation,
            },

            EPropsNotFoundInInvariantSubtyping(box EPropsNotFoundInInvariantSubtypingData {
                prop_names,
                reason_lower,
                reason_upper,
                lower_obj_loc,
                upper_obj_loc,
                lower_obj_desc,
                upper_obj_desc,
                use_op,
            }) => EPropsNotFoundInInvariantSubtyping(Box::new(
                EPropsNotFoundInInvariantSubtypingData {
                    prop_names,
                    reason_lower,
                    reason_upper,
                    lower_obj_loc,
                    upper_obj_loc,
                    lower_obj_desc: f(lower_obj_desc),
                    upper_obj_desc: f(upper_obj_desc),
                    use_op: map_use_op(&f, use_op),
                },
            )),

            EIncompatibleSpeculation(box EIncompatibleSpeculationData {
                use_op,
                loc,
                branches,
            }) => {
                let use_op = use_op.map(|u| map_use_op(&f, u));
                let branches = branches
                    .into_iter()
                    .map(|b| Self::convert_type_to_type_desc(f.clone(), b))
                    .collect();
                EIncompatibleSpeculation(Box::new(EIncompatibleSpeculationData {
                    use_op,
                    loc,
                    branches,
                }))
            }

            EIncompatibleDefs(box EIncompatibleDefsData {
                use_op,
                reason_lower,
                reason_upper,
                branches,
            }) => {
                let use_op = map_use_op(&f, use_op);
                let branches = branches
                    .into_iter()
                    .map(|b| Self::convert_type_to_type_desc(f.clone(), b))
                    .collect();
                EIncompatibleDefs(Box::new(EIncompatibleDefsData {
                    use_op,
                    reason_lower,
                    reason_upper,
                    branches,
                }))
            }

            EUnionSpeculationFailed(box EUnionSpeculationFailedData {
                use_op,
                reason,
                op_reasons,
                branches,
            }) => {
                let use_op = map_use_op(&f, use_op);
                let branches = branches
                    .into_iter()
                    .map(|b| Self::convert_type_to_type_desc(f.clone(), b))
                    .collect();
                EUnionSpeculationFailed(Box::new(EUnionSpeculationFailedData {
                    use_op,
                    reason,
                    op_reasons,
                    branches,
                }))
            }

            e => e,
        }
    }
}

fn desc_of_reason<L: Dupe + PartialOrd + Ord + PartialEq + Eq>(
    r: &VirtualReason<L>,
) -> &VirtualReasonDesc<L> {
    r.desc(is_scalar_reason(r))
}

/// A utility function for getting and updating the use_op in error messages.
pub fn util_use_op_of_msg<L, T, F>(nope: T, util: F, msg: &ErrorMessage<L>) -> T
where
    L: Dupe + PartialEq + Eq + PartialOrd + Ord,
    F: Fn(&VirtualUseOp<L>) -> T,
{
    match msg {
        ErrorMessage::EIncompatible { use_op, .. } => use_op.as_ref().map_or_else(|| nope, util),
        ErrorMessage::EIncompatibleSpeculation(box EIncompatibleSpeculationData {
            use_op, ..
        }) => use_op.as_ref().map_or_else(|| nope, util),
        ErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData { use_op, .. }) => util(use_op),
        ErrorMessage::EIncompatibleProp { use_op, .. } => {
            use_op.as_ref().map_or_else(|| nope, util)
        }
        ErrorMessage::EExpectedStringLit { use_op, .. } => util(use_op),
        ErrorMessage::EExpectedNumberLit { use_op, .. } => util(use_op),
        ErrorMessage::EExpectedBooleanLit { use_op, .. } => util(use_op),
        ErrorMessage::EExpectedBigIntLit { use_op, .. } => util(use_op),
        ErrorMessage::EPropNotFoundInLookup { use_op, .. } => util(use_op),
        ErrorMessage::EPropNotFoundInSubtyping { use_op, .. } => util(use_op),
        ErrorMessage::EPropsNotFoundInSubtyping { use_op, .. } => util(use_op),
        ErrorMessage::EPropsNotFoundInInvariantSubtyping(
            box EPropsNotFoundInInvariantSubtypingData { use_op, .. },
        ) => util(use_op),
        ErrorMessage::EPropsExtraAgainstExactObject { use_op, .. } => util(use_op),
        ErrorMessage::EIndexerCheckFailed { use_op, .. } => util(use_op),
        ErrorMessage::EPropNotReadable { use_op, .. } => util(use_op),
        ErrorMessage::EPropNotWritable { use_op, .. } => util(use_op),
        ErrorMessage::EPropPolarityMismatch { use_op, .. } => util(use_op),
        ErrorMessage::EPrivateLookupFailed(_, _, use_op) => util(use_op),
        ErrorMessage::ETupleArityMismatch { use_op, .. } => util(use_op),
        ErrorMessage::ENonLitArrayToTuple(_, use_op) => util(use_op),
        ErrorMessage::ETupleOutOfBounds { use_op, .. } => util(use_op),
        ErrorMessage::ETupleNonIntegerIndex { use_op, .. } => util(use_op),
        ErrorMessage::ETupleUnsafeWrite { use_op, .. } => util(use_op),
        ErrorMessage::ETupleElementNotReadable { use_op, .. } => util(use_op),
        ErrorMessage::ETupleElementNotWritable { use_op, .. } => util(use_op),
        ErrorMessage::ETupleElementPolarityMismatch { use_op, .. } => util(use_op),
        ErrorMessage::EROArrayWrite(_, use_op) => util(use_op),
        ErrorMessage::EUnionSpeculationFailed(box EUnionSpeculationFailedData {
            use_op, ..
        }) => util(use_op),
        ErrorMessage::EIncompatibleWithExact(_, use_op, _) => util(use_op),
        ErrorMessage::EFunctionIncompatibleWithIndexer(_, use_op) => util(use_op),
        ErrorMessage::EInvalidObjectKit { use_op, .. } => util(use_op),
        ErrorMessage::EIncompatibleWithUseOp { use_op, .. } => util(use_op),
        ErrorMessage::EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
            use_op,
            ..
        }) => util(use_op),
        ErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible { use_op, .. }) => util(use_op),
        ErrorMessage::ENotAReactComponent { use_op, .. } => util(use_op),
        ErrorMessage::EFunctionCallExtraArg(_, _, _, use_op) => util(use_op),
        ErrorMessage::EPrimitiveAsInterface { use_op, .. } => util(use_op),
        ErrorMessage::ECannotSpreadInterface { use_op, .. } => util(use_op),
        ErrorMessage::ECannotSpreadIndexerOnRight { use_op, .. } => util(use_op),
        ErrorMessage::EUnableToSpread { use_op, .. } => util(use_op),
        ErrorMessage::EInexactMayOverwriteIndexer { use_op, .. } => util(use_op),
        ErrorMessage::EImplicitInstantiationUnderconstrainedError { use_op, .. } => util(use_op),
        ErrorMessage::EIncompatibleReactDeepReadOnly { use_op, .. } => util(use_op),
        _ => nope,
    }
}

impl<L: Dupe + PartialOrd + Ord + PartialEq + Eq> ErrorMessage<L> {
    /// Not all messages (i.e. those whose locations are based on use_ops) have locations that can be
    /// determined while locations are abstract. We just return None in this case.
    pub fn loc_of_msg(&self) -> Option<L>
    where
        L: Clone,
    {
        match self {
            Self::EAnyValueUsedAsType { reason_use, .. }
            | Self::EValueUsedAsType { reason_use, .. } => Some(reason_use.loc.dupe()),

            Self::ENonStrictEqualityComparison(primary, _)
            | Self::EInvalidTypeArgs(_, primary)
            | Self::ETooFewTypeArgs {
                reason_tapp: primary,
                ..
            }
            | Self::ETooManyTypeArgs {
                reason_tapp: primary,
                ..
            } => Some(primary.loc.dupe()),

            Self::EComparison { r1, loc_opt, .. } => {
                loc_opt.as_ref().or(Some(&r1.loc)).map(|l| l.dupe())
            }

            Self::ESketchyNumberLint(_, reason)
            | Self::EInvalidExtends(reason)
            | Self::EUnsupportedSetProto(reason)
            | Self::EReactElementFunArity(reason, _, _)
            | Self::EReactRefInRender { usage: reason, .. }
            | Self::EUnsupportedImplements(reason)
            | Self::EObjectComputedPropertyAssign(reason, _, _)
            | Self::EObjectComputedPropertyAccess {
                reason_prop: reason,
                ..
            }
            | Self::EForInRHS(reason)
            | Self::EBinaryInRHS(reason)
            | Self::EBinaryInLHS(reason)
            | Self::EInstanceofRHS(reason)
            | Self::EArithmeticOperand(reason)
            | Self::ERecursionLimit(reason, _)
            | Self::EMissingLocalAnnotation { reason, .. }
            | Self::EComponentMissingReturn(reason)
            | Self::ENestedComponent(reason)
            | Self::ENestedHook(reason)
            | Self::EUnsupportedExact(_, reason)
            | Self::EPolarityMismatch { reason, .. }
            | Self::ENoNamedExport(reason, _, _, _)
            | Self::EOnlyDefaultExport(reason, _, _)
            | Self::ENoDefaultExport(reason, _, _)
            | Self::EImportTypeAsValue(reason, _)
            | Self::EImportTypeAsTypeof(reason, _)
            | Self::EExportValueAsType(reason, _)
            | Self::EImportValueAsType(reason, _)
            | Self::ETemporaryHardcodedErrorForPrototyping(reason, _)
            | Self::EComputedPropertyWithUnion(reason)
            | Self::ETypeParamConstInvalidPosition(reason) => Some(reason.loc.dupe()),

            Self::EObjectComputedPropertyPotentialOverwrite { key_loc, .. } => Some(key_loc.dupe()),

            Self::EEnumError(
                EnumErrorKind::EnumAllMembersAlreadyChecked { loc: key_loc, .. }
                | EnumErrorKind::EnumMemberAlreadyChecked {
                    case_test_loc: key_loc,
                    ..
                }
                | EnumErrorKind::EnumInvalidCheck { loc: key_loc, .. }
                | EnumErrorKind::EnumInvalidMemberName { loc: key_loc, .. }
                | EnumErrorKind::EnumNonIdentifierMemberName { loc: key_loc, .. },
            ) => Some(key_loc.dupe()),

            Self::EEnumError(
                EnumErrorKind::EnumNotAllChecked { reason, .. }
                | EnumErrorKind::EnumUnknownNotChecked { reason, .. }
                | EnumErrorKind::EnumInvalidAbstractUse { reason, .. }
                | EnumErrorKind::EnumMemberUsedAsType { reason, .. }
                | EnumErrorKind::EnumInvalidMemberAccess { reason, .. }
                | EnumErrorKind::EnumInvalidObjectUtilType { reason, .. }
                | EnumErrorKind::EnumInvalidObjectFunction { reason, .. }
                | EnumErrorKind::EnumNotIterable { reason, .. },
            )
            | Self::ERecursiveDefinition { reason, .. }
            | Self::EInvalidConstructor(reason)
            | Self::EInvalidDeclaration {
                declaration: reason,
                ..
            }
            | Self::EBigIntRShift3(reason)
            | Self::EBigIntNumCoerce(reason)
            | Self::EInvalidBinaryArith {
                reason_out: reason, ..
            }
            | Self::ETupleRequiredAfterOptional {
                reason_tuple: reason,
                ..
            }
            | Self::ETupleInvalidTypeSpread {
                reason_spread: reason,
                ..
            }
            | Self::ETupleElementAfterInexactSpread(reason)
            | Self::ETypeGuardInvalidParameter {
                type_guard_reason: reason,
                ..
            }
            | Self::ETypeGuardParamUnbound(reason)
            | Self::ETypeGuardThisParam(reason)
            | Self::ETypeGuardFunctionInvalidWrites { reason, .. }
            | Self::ENegativeTypeGuardConsistency {
                return_reason: reason,
                ..
            }
            | Self::ETypeGuardFunctionParamHavoced {
                type_guard_reason: reason,
                ..
            }
            | Self::EIllegalAssertOperator { op: reason, .. } => Some(reason.loc.dupe()),

            Self::EDefinitionCycle(dependencies) => Some(dependencies.first().0.loc.dupe()),

            Self::EExponentialSpread {
                reasons_for_operand1,
                reasons_for_operand2,
                ..
            } => {
                let union_reason = match (
                    &reasons_for_operand1.second_reason,
                    &reasons_for_operand2.second_reason,
                ) {
                    (None, _) => &reasons_for_operand1.first_reason,
                    (_, None) => &reasons_for_operand2.first_reason,
                    (Some(r), _) => r,
                };
                Some(union_reason.loc.dupe())
            }

            Self::EBindingError(_, loc, _, _) => Some(loc.dupe()),

            Self::EComponentCase(loc)
            | Self::EComponentMissingBody(loc)
            | Self::EComponentBodyInAmbientContext(loc)
            | Self::EDeclareComponentInvalidParam { loc, .. }
            | Self::EHookNaming(loc)
            | Self::EIncorrectTypeWithReplacement { loc, .. }
            | Self::EUnsafeGettersSetters(loc)
            | Self::EUnsafeObjectAssign(loc)
            | Self::ETSSyntax { kind: _, loc } => Some(loc.dupe()),

            Self::EInvalidTypeof(loc, _) => Some(loc.dupe()),
            Self::EReactIntrinsicOverlap { def, .. } => Some(def.dupe()),
            Self::EStrUtilTypeNonLiteralArg(loc) => Some(loc.dupe()),

            Self::EInvalidPrototype(loc, _)
            | Self::EUntypedTypeImport(loc, _)
            | Self::EUntypedImport(loc, _)
            | Self::EInvalidInfer(loc)
            | Self::EConstantCondition { loc, .. }
            | Self::EInvalidReactCreateElement {
                create_element_loc: loc,
                ..
            }
            | Self::ENonstrictImport(loc)
            | Self::EUnclearType(loc)
            | Self::EDeprecatedBool(loc)
            | Self::EInternalType(loc, _)
            | Self::EUnnecessaryOptionalChain(loc, _)
            | Self::EUnnecessaryInvariant(loc, _)
            | Self::EUnnecessaryDeclareTypeOnlyExport(loc)
            | Self::EUnusedSuppression(loc)
            | Self::ECodelessSuppression(loc)
            | Self::EDocblockError(loc, _)
            | Self::EImplicitInexactObject(loc)
            | Self::EInvalidComponentRestParam(loc)
            | Self::EAmbiguousObjectType(loc)
            | Self::EParseError(loc, _)
            | Self::EInvalidLHSInAssignment(loc)
            | Self::EUnreachable(loc)
            | Self::ECannotDelete(loc, _)
            | Self::EBadExportContext(_, loc)
            | Self::EBadExportPosition(loc)
            | Self::EBadDefaultImportAccess(loc, _)
            | Self::EBadDefaultImportDestructuring(loc)
            | Self::EInvalidImportStarUse(loc, _)
            | Self::ENonConstVarExport(loc, _)
            | Self::EThisInExportedFunction(loc)
            | Self::EMixedImportAndRequire(loc, _)
            | Self::EUnsupportedVarianceAnnotation(loc, _)
            | Self::EExportRenamedDefault { loc, .. }
            | Self::EIndeterminateModuleType(loc)
            | Self::EEnumError(
                EnumErrorKind::EnumsNotEnabled(loc) | EnumErrorKind::EnumConstNotSupported(loc),
            )
            | Self::EUninitializedInstanceProperty(loc, _)
            | Self::EUseArrayLiteral(loc)
            | Self::EUnsupportedSyntax(loc, _)
            | Self::EInternal(loc, _)
            | Self::EUnsupportedKeyInObject { loc, .. }
            | Self::EAmbiguousNumericKeyWithVariance(loc)
            | Self::EHookRuleViolation { call_loc: loc, .. }
            | Self::EExportsAnnot(loc)
            | Self::EUnexpectedThisType(loc)
            | Self::ETypeParamMinArity(loc, _)
            | Self::EAssignConstLikeBinding { loc, .. }
            | Self::EMalformedCode(loc)
            | Self::EObjectThisSuperReference(loc, _, _)
            | Self::EComponentThisReference { this_loc: loc, .. }
            | Self::EInvalidGraphQL(loc, _)
            | Self::EAnnotationInference(loc, _, _, _)
            | Self::ETrivialRecursiveDefinition(loc, _)
            | Self::EInvalidCatchParameterAnnotation { loc, .. }
            | Self::EInvalidMappedType { loc, .. }
            | Self::EReferenceInAnnotation(loc, _, _)
            | Self::EReferenceInDefault(_, _, loc)
            | Self::EDuplicateComponentProp { spread: loc, .. }
            | Self::ERefComponentProp { spread: loc, .. }
            | Self::EKeySpreadProp { spread: loc, .. }
            | Self::ETypeGuardIncompatibleWithFunctionKind { loc, .. }
            | Self::EMissingPlatformSupportWithAvailablePlatforms { loc, .. }
            | Self::EMissingPlatformSupport { loc, .. }
            | Self::EUnionOptimization { loc, .. }
            | Self::EUnionOptimizationOnNonUnion { loc, .. }
            | Self::ELintSetting(loc, _)
            | Self::ETypeParamArity(loc, _)
            | Self::ESketchyNullLint { loc, .. } => Some(loc.dupe()),

            Self::EUnionPartialOptimizationNonUniqueKey(
                box EUnionPartialOptimizationNonUniqueKeyData { loc, .. },
            ) => Some(loc.dupe()),

            Self::ECallTypeArity { call_loc, .. } => Some(call_loc.dupe()),
            Self::EMissingTypeArgs { reason_op, .. } => Some(reason_op.loc.dupe()),
            Self::EInvalidRendersTypeArgument { loc, .. } => Some(loc.dupe()),
            Self::EInvalidTypeCastSyntax { loc, .. } => Some(loc.dupe()),

            Self::ESignatureBindingValidation(e) => match e {
                BindingValidation::ModuleOverride {
                    override_binding_loc,
                    ..
                }
                | BindingValidation::NameOverride {
                    override_binding_loc,
                    ..
                } => Some(override_binding_loc.dupe()),
                BindingValidation::NamespacedNameAlreadyBound {
                    invalid_binding_loc,
                    ..
                } => Some(invalid_binding_loc.dupe()),
            },

            Self::ESignatureVerification(sve) => match sve {
                SignatureError::ExpectedAnnotation(loc, _)
                | SignatureError::UnexpectedObjectKey(loc, _)
                | SignatureError::UnexpectedArraySpread(loc, _)
                | SignatureError::UnexpectedArrayHole(loc)
                | SignatureError::EmptyArray(loc)
                | SignatureError::EmptyObject(loc)
                | SignatureError::UnexpectedExpression(loc, _) => Some(loc.dupe()),
            },

            Self::EDuplicateModuleProvider { conflict, .. } => Some(conflict.dupe()),
            Self::EEnumError(
                EnumErrorKind::EnumModification { loc, .. }
                | EnumErrorKind::EnumMemberDuplicateValue { loc, .. }
                | EnumErrorKind::EnumDuplicateMemberName { loc, .. }
                | EnumErrorKind::EnumInconsistentMemberValues { loc, .. }
                | EnumErrorKind::EnumInvalidMemberInitializer { loc, .. }
                | EnumErrorKind::EnumBooleanMemberNotInitialized { loc, .. }
                | EnumErrorKind::EnumNumberMemberNotInitialized { loc, .. }
                | EnumErrorKind::EnumBigIntMemberNotInitialized { loc, .. }
                | EnumErrorKind::EnumStringMemberInconsistentlyInitialized { loc, .. },
            )
            | Self::EBuiltinNameLookupFailed { loc, .. }
            | Self::EBuiltinModuleLookupFailed { loc, .. }
            | Self::EExpectedModuleLookupFailed { loc, .. }
            | Self::EPlatformSpecificImplementationModuleLookupFailed { loc, .. }
            | Self::EDuplicateClassMember { loc, .. }
            | Self::EEmptyArrayNoProvider { loc }
            | Self::EUnusedPromise { loc, .. } => Some(loc.dupe()),

            Self::ECannotCallReactComponent { reason } => Some(reason.loc.dupe()),

            Self::EMatchError(e) => match e {
                MatchErrorKind::MatchNotExhaustive { loc, .. }
                | MatchErrorKind::MatchNonExhaustiveObjectPattern { loc, .. }
                | MatchErrorKind::MatchNonExplicitEnumCheck { loc, .. }
                | MatchErrorKind::MatchInvalidBindingKind { loc, .. }
                | MatchErrorKind::MatchInvalidObjectPropertyLiteral { loc, .. }
                | MatchErrorKind::MatchInvalidUnaryZero { loc }
                | MatchErrorKind::MatchInvalidUnaryPlusBigInt { loc }
                | MatchErrorKind::MatchDuplicateObjectProperty { loc, .. }
                | MatchErrorKind::MatchBindingInOrPattern { loc }
                | MatchErrorKind::MatchInvalidAsPattern { loc }
                | MatchErrorKind::MatchInvalidPatternReference { loc, .. }
                | MatchErrorKind::MatchInvalidObjectShorthand { loc, .. }
                | MatchErrorKind::MatchStatementInvalidBody { loc }
                | MatchErrorKind::MatchInvalidCaseSyntax { loc, .. }
                | MatchErrorKind::MatchInvalidIdentOrMemberPattern { loc, .. } => Some(loc.dupe()),
                MatchErrorKind::MatchInvalidWildcardSyntax(loc)
                | MatchErrorKind::MatchInvalidInstancePattern(loc)
                | MatchErrorKind::MatchInvalidGuardedWildcard(loc) => Some(loc.dupe()),
                MatchErrorKind::MatchUnusedPattern { reason, .. } => Some(reason.loc.dupe()),
            },

            Self::ERecordError(e) => match e {
                RecordErrorKind::RecordBannedTypeUtil {
                    reason_record: reason,
                    ..
                } => Some(reason.loc.dupe()),
                RecordErrorKind::RecordInvalidName { loc, .. }
                | RecordErrorKind::RecordInvalidNew { loc, .. }
                | RecordErrorKind::RecordDeclarationInvalidSyntax { loc, .. } => Some(loc.dupe()),
            },

            Self::EUndocumentedFeature { loc } => Some(loc.dupe()),
            Self::EDevOnlyRefinedLocInfo { refined_loc, .. } => Some(refined_loc.dupe()),
            Self::EDevOnlyInvalidatedRefinementInfo { read_loc, .. } => Some(read_loc.dupe()),

            Self::EUnableToSpread { .. }
            | Self::ECannotSpreadInterface { .. }
            | Self::ECannotSpreadIndexerOnRight { .. }
            | Self::EInexactMayOverwriteIndexer { .. }
            | Self::EFunctionCallExtraArg { .. }
            | Self::ENotAReactComponent { .. }
            | Self::EIncompatibleWithUseOp { .. }
            | Self::EInvariantSubtypingWithUseOp(..)
            | Self::EEnumError(EnumErrorKind::EnumIncompatible { .. })
            | Self::EIncompatibleDefs(..)
            | Self::EInvalidObjectKit { .. }
            | Self::EIncompatibleWithExact { .. }
            | Self::EFunctionIncompatibleWithIndexer { .. }
            | Self::EUnionSpeculationFailed(..)
            | Self::ETupleUnsafeWrite { .. }
            | Self::EROArrayWrite { .. }
            | Self::ETupleElementNotReadable { .. }
            | Self::ETupleElementNotWritable { .. }
            | Self::ETupleElementPolarityMismatch { .. }
            | Self::ETupleOutOfBounds { .. }
            | Self::ETupleNonIntegerIndex { .. }
            | Self::ENonLitArrayToTuple { .. }
            | Self::ETupleArityMismatch { .. }
            | Self::EPrivateLookupFailed { .. }
            | Self::EPropPolarityMismatch { .. }
            | Self::EPropNotReadable { .. }
            | Self::EPropNotWritable { .. }
            | Self::EPropNotFoundInLookup { .. }
            | Self::EPropNotFoundInSubtyping { .. }
            | Self::EPropsNotFoundInSubtyping { .. }
            | Self::EPropsNotFoundInInvariantSubtyping(..)
            | Self::EPropsExtraAgainstExactObject { .. }
            | Self::EIndexerCheckFailed { .. }
            | Self::EExpectedBooleanLit { .. }
            | Self::EExpectedNumberLit { .. }
            | Self::EExpectedStringLit { .. }
            | Self::EExpectedBigIntLit { .. }
            | Self::EIncompatibleProp { .. }
            | Self::EIncompatible { .. }
            | Self::EIncompatibleSpeculation(..)
            | Self::EMethodUnbinding { .. }
            | Self::EHookIncompatible { .. }
            | Self::EIncompatibleReactDeepReadOnly { .. }
            | Self::EHookUniqueIncompatible { .. }
            | Self::EImplicitInstantiationUnderconstrainedError { .. }
            | Self::EClassToObject { .. }
            | Self::EPrimitiveAsInterface { .. }
            | Self::ETypeGuardFuncIncompatibility { .. }
            | Self::ETypeGuardIndexMismatch { .. }
            | Self::ETypeGuardImpliesMismatch { .. }
            | Self::ETypeParamConstIncompatibility { .. } => None,
        }
    }

    pub fn kind_of_msg(&self) -> ErrorKind {
        use ErrorKind::*;
        use LintKind::*;

        match self {
            // Lint errors
            ErrorMessage::EUntypedTypeImport(_, _) => LintError(UntypedTypeImport),
            ErrorMessage::EUntypedImport(_, _) => LintError(UntypedImport),
            ErrorMessage::ENonstrictImport(_) => LintError(NonstrictImport),
            ErrorMessage::EInternalType(_, _) => LintError(InternalType),
            ErrorMessage::EUnclearType(_) => LintError(UnclearType),
            ErrorMessage::EDeprecatedBool(_) => LintError(DeprecatedType(DeprecatedTypeKind::Bool)),
            ErrorMessage::EUnsafeGettersSetters(_) => LintError(UnsafeGettersSetters),
            ErrorMessage::EUnsafeObjectAssign(_) => LintError(UnsafeObjectAssign),
            ErrorMessage::ESketchyNullLint { kind, .. } => LintError(SketchyNull(*kind)),
            ErrorMessage::ESketchyNumberLint(kind, _) => LintError(SketchyNumber(*kind)),
            ErrorMessage::EUnnecessaryOptionalChain(_, _) => LintError(UnnecessaryOptionalChain),
            ErrorMessage::EUnnecessaryInvariant(_, _) => LintError(UnnecessaryInvariant),
            ErrorMessage::EImplicitInexactObject(_) => LintError(ImplicitInexactObject),
            ErrorMessage::EAmbiguousObjectType(_) => LintError(AmbiguousObjectType),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked {
                default_case_loc: Some(_),
                ..
            }) => LintError(RequireExplicitEnumSwitchCases),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck { .. }) => {
                LintError(RequireExplicitEnumChecks)
            }
            ErrorMessage::EUninitializedInstanceProperty(_, _) => {
                LintError(UninitializedInstanceProperty)
            }
            ErrorMessage::EBadDefaultImportAccess(_, _) => LintError(DefaultImportAccess),
            ErrorMessage::EBadDefaultImportDestructuring(_) => LintError(DefaultImportAccess),
            ErrorMessage::EInvalidImportStarUse(_, _) => LintError(InvalidImportStarUse),
            ErrorMessage::ENonConstVarExport(_, _) => LintError(NonConstVarExport),
            ErrorMessage::EThisInExportedFunction(_) => LintError(ThisInExportedFunction),
            ErrorMessage::EMixedImportAndRequire(_, _) => LintError(MixedImportAndRequire),
            ErrorMessage::EExportRenamedDefault { .. } => LintError(ExportRenamedDefault),
            ErrorMessage::EUnusedPromise { .. } => LintError(UnusedPromise),
            ErrorMessage::EReactIntrinsicOverlap { .. } => LintError(ReactIntrinsicOverlap),
            ErrorMessage::ENestedComponent(_) => LintError(NestedComponent),
            ErrorMessage::ENestedHook(_) => LintError(NestedHook),
            ErrorMessage::ESignatureBindingValidation(BindingValidation::ModuleOverride {
                ..
            })
            | ErrorMessage::ESignatureBindingValidation(BindingValidation::NameOverride {
                ..
            }) => LintError(LibdefOverride),

            // Infer warnings
            ErrorMessage::EBadExportPosition(_) | ErrorMessage::EBadExportContext(_, _) => {
                InferWarning(InferWarningKind::ExportKind)
            }
            ErrorMessage::EEnumError(
                EnumErrorKind::EnumsNotEnabled(_) | EnumErrorKind::EnumConstNotSupported(_),
            )
            | ErrorMessage::EIndeterminateModuleType(_)
            | ErrorMessage::EUnreachable(_)
            | ErrorMessage::EInvalidTypeof(_, _) => InferWarning(InferWarningKind::OtherKind),

            // Other error kinds
            ErrorMessage::EInternal(_, _) => InternalError,
            ErrorMessage::ERecursionLimit(_, _) => RecursionLimitError,
            ErrorMessage::EDuplicateModuleProvider { .. } => DuplicateProviderError,
            ErrorMessage::EParseError(_, _) => ParseError,
            ErrorMessage::EDocblockError(_, _) | ErrorMessage::ELintSetting(_, _) => {
                PseudoParseError
            }

            // Default case: all other error messages are InferError
            _ => InferError,
        }
    }
}

pub fn polarity_explanation(pol: Polarity, pol2: Polarity) -> &'static str {
    match (pol, pol2) {
        (Polarity::Positive, _) => "read-only",
        (Polarity::Negative, _) => "write-only",
        (Polarity::Neutral, Polarity::Negative) => "readable",
        (Polarity::Neutral, Polarity::Positive) => "writable",
        (Polarity::Neutral, Polarity::Neutral) => panic!("unreachable"),
    }
}

pub fn mk_prop_message<L>(prop: Option<&str>) -> Vec<MessageFeature<L>> {
    match prop {
        None => vec![text(
            "an index signature declaring the expected key / value type",
        )],
        Some("$call") => {
            vec![text(
                "a call signature declaring the expected parameter / return type",
            )]
        }
        Some(prop) => vec![text("property "), code(prop)],
    }
}

pub fn enum_name_of_reason<L: Dupe + PartialOrd + Ord + PartialEq + Eq>(
    reason: &VirtualReason<L>,
) -> Option<FlowSmolStr> {
    use flow_common::reason::VirtualReasonDesc::*;
    match desc_of_reason(reason) {
        REnum { name: Some(name) } => Some(name.dupe()),
        RType(name) => Some(name.dupe().into_smol_str()),
        _ => None,
    }
}

pub fn string_of_internal_error(error: &InternalError) -> FlowSmolStr {
    match error {
        InternalError::ReadOfUnreachedTvar(k) => format!(
            "read of {:?} entry which has not been prepared for typechecking",
            k
        )
        .into(),
        InternalError::ReadOfUnresolvedTvar(k) => format!(
            "read of {:?} entry from previous component is not FullyResolved",
            k
        )
        .into(),
        InternalError::ForcedReadOfUnderResolutionTvar(k) => format!(
            "forced read of {:?} entry from component is not yet FullyResolved",
            k
        )
        .into(),
        InternalError::MethodNotAFunction => "expected function type".into(),
        InternalError::OptionalMethod => "optional methods are not supported".into(),
        InternalError::PropertyDescriptorPropertyCannotBeRead => {
            "unexpected property in properties object".into()
        }
        InternalError::ForInLHS => "unexpected LHS in for...in".into(),
        InternalError::ForOfLHS => "unexpected LHS in for...of".into(),
        InternalError::PropRefComputedOpen => {
            "unexpected open computed property element type".into()
        }
        InternalError::PropRefComputedLiteral => {
            "unexpected literal computed property element type".into()
        }
        InternalError::RestParameterNotIdentifierPattern => {
            "unexpected rest parameter, expected an identifier pattern".into()
        }
        InternalError::InterfaceTypeSpread => "unexpected spread property in interface".into(),
        InternalError::DebugThrow => "debug throw".into(),
        InternalError::ParseJobException(exc) => format!("uncaught exception: {}", exc).into(),
        InternalError::CheckTimeout(s) => {
            format!("check job timed out after {:.2} seconds", s).into()
        }
        InternalError::CheckJobException(exc) => format!("uncaught exception: {}", exc).into(),
        InternalError::UnexpectedAnnotationInference(s) => {
            format!("unexpected {} in annotation inference", s).into()
        }
        InternalError::MissingSwitchExhaustiveCheck => "missing exhaustive check entry".into(),
        InternalError::MissingEnvRead(l) => format!("missing env entry for read at {:?}", l).into(),
        InternalError::MissingEnvWrite(loc) => {
            format!("expected env entry for write location{:?}", loc).into()
        }
        InternalError::EnvInvariant(failure) => match failure {
            EnvInvariantFailure::NameDefOrderingFailure {
                all,
                roots,
                missing_roots,
            } => {
                let all_str = all
                    .iter()
                    .map(|l| format!("{:?}", l))
                    .collect::<Vec<_>>()
                    .join(",");
                let roots_str = roots
                    .iter()
                    .map(|l| format!("{:?}", l))
                    .collect::<Vec<_>>()
                    .join(",");
                let missing_roots_str = missing_roots
                    .iter()
                    .map(|l| format!("{:?}", l))
                    .collect::<Vec<_>>()
                    .join(",");
                format!(
                    "Please report this error to the Flow team: Env_api tarjan failure, all: {{ {} }} roots: {{ {} }} missing_roots: {{ {} }}",
                    all_str, roots_str, missing_roots_str
                )
                .into()
            }
            EnvInvariantFailure::Impossible(s) => format!(
                "Internal state should be impossible, please report this to the Flow team: {}",
                s
            )
            .into(),
            EnvInvariantFailure::ASTStructureOverride(s) => format!(
                "AST visitor issue, please report this to the Flow team: {}",
                s
            )
            .into(),
            EnvInvariantFailure::NameDefGraphMismatch => {
                "EnvMap.find missed, please report this to the Flow team".into()
            }
            EnvInvariantFailure::MissingEnvEntry(x) => format!(
                "Did not find {} in name_resolver environment, please report this to the Flow team",
                x
            )
            .into(),
        },
        InternalError::ImplicitInstantiationInvariant(s) => format!(
            "Implicit instantiation issue, please report this to the Flow team: {}",
            s
        )
        .into(),
    }
}

pub fn type_casting_examples(
    enabled_casting_syntax: CastingSyntax,
) -> (&'static str, &'static str) {
    let example_as = "<expr> as <type>";
    let example_colon = "(<expr>: <type>)";
    match enabled_casting_syntax {
        CastingSyntax::Both | CastingSyntax::As => (example_as, example_colon),
    }
}

/// Friendly messages are created differently based on the specific error they come from.
/// We collect the ingredients here and pass them to make_error_printable.
#[derive(Debug, Clone)]
pub enum FriendlyMessageRecipe<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    IncompatibleUse {
        loc: L,
        upper_kind: UpperKind<L>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },
    Speculation {
        loc: L,
        use_op: VirtualUseOp<L>,
        branches: Vec<ErrorMessage<L>>,
    },
    IncompatibleSubtyping {
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
        explanation: Option<Explanation<L>>,
    },
    IncompatibleInvariantSubtyping {
        sub_component: Option<SubComponentOfInvariantSubtypingError>,
        lower_loc: L,
        upper_loc: L,
        lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        use_op: VirtualUseOp<L>,
        explanation: Option<Explanation<L>>,
    },
    IncompatibleEnum {
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
        enum_kind: EnumKind,
        representation_type: Option<FlowSmolStr>,
        casting_syntax: CastingSyntax,
    },
    PropMissingInLookup {
        loc: L,
        prop: Option<FlowSmolStr>,
        suggestion: Option<FlowSmolStr>,
        reason_obj: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
        reason_indexer: Option<VirtualReason<L>>,
    },
    PropMissingInSubtyping {
        prop: Option<FlowSmolStr>,
        suggestion: Option<FlowSmolStr>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        reason_indexer: Option<VirtualReason<L>>,
        use_op: VirtualUseOp<L>,
    },
    PropsMissingInSubtyping {
        props: Vec1<FlowSmolStr>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },
    PropsMissingInInvariantSubtyping {
        props: Vec1<FlowSmolStr>,
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        lower_obj_loc: L,
        upper_obj_loc: L,
        lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
        use_op: VirtualUseOp<L>,
    },
    PropsExtraAgainstExactObject {
        props: Vec1<FlowSmolStr>,
        reason_l_obj: VirtualReason<L>,
        reason_r_obj: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },
    Normal(Message<L>),
    UseOp {
        loc: L,
        message: Message<L>,
        use_op: VirtualUseOp<L>,
        explanation: Option<Explanation<L>>,
    },
    PropPolarityMismatch {
        reason_lower: VirtualReason<L>,
        reason_upper: VirtualReason<L>,
        props: Vec1<(Option<FlowSmolStr>, Polarity, Polarity)>,
        use_op: VirtualUseOp<L>,
    },
}

fn expect_type_desc<L: Dupe>(
    type_or_desc: TypeOrTypeDesc<L>,
) -> Result<ALocTy, VirtualReasonDesc<L>> {
    match type_or_desc {
        TypeOrTypeDesc::Type(_) => {
            panic!("At this point, we should no longer have TypeOrTypeDesc::Type")
        }
        TypeOrTypeDesc::TypeDesc(desc) => desc,
    }
}

impl<L: Dupe + PartialEq + Eq + PartialOrd + Ord> ErrorMessage<L> {
    /// Transform an ErrorMessage into a FriendlyMessageRecipe.
    ///
    /// This function consumes the ErrorMessage because OCaml callers never use
    /// the message after calling this function - they pattern match on the result.
    pub fn friendly_message_of_msg(self) -> FriendlyMessageRecipe<L> {
        use FriendlyMessageRecipe::*;

        match self {
            ErrorMessage::EIncompatible {
                lower: (reason_lower, _),
                upper: (reason_upper, upper_kind),
                use_op,
            } => {
                let loc = reason_upper.loc.dupe();
                IncompatibleUse {
                    loc,
                    upper_kind,
                    reason_lower,
                    reason_upper,
                    use_op: use_op
                        .unwrap_or(VirtualUseOp::Op(Arc::new(VirtualRootUseOp::UnknownUse))),
                }
            }

            ErrorMessage::EIncompatibleSpeculation(box EIncompatibleSpeculationData {
                loc,
                use_op,
                branches,
            }) => Speculation {
                loc,
                use_op: use_op.unwrap_or(VirtualUseOp::Op(Arc::new(VirtualRootUseOp::UnknownUse))),
                branches,
            },

            ErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData {
                use_op,
                reason_lower,
                reason_upper,
                branches,
            }) => {
                if branches.is_empty() {
                    IncompatibleSubtyping {
                        reason_lower,
                        reason_upper,
                        use_op,
                        explanation: None,
                    }
                } else {
                    let loc = reason_upper.loc.dupe();
                    Speculation {
                        loc,
                        use_op,
                        branches,
                    }
                }
            }

            ErrorMessage::EExportValueAsType(_, export_name) => Normal(
                Message::MessageExportValueAsType(export_name.into_smol_str()),
            ),

            ErrorMessage::EImportValueAsType(_, export_name) => {
                Normal(Message::MessageImportValueAsType(export_name))
            }

            ErrorMessage::EImportTypeAsTypeof(_, export_name) => {
                Normal(Message::MessageImportTypeAsTypeof(export_name))
            }

            ErrorMessage::EImportTypeAsValue(_, export_name) => {
                Normal(Message::MessageImportTypeAsValue(export_name))
            }

            ErrorMessage::EInvalidInfer { .. } => Normal(Message::MessageInvalidInferType),

            ErrorMessage::EInvalidExtends(reason) => {
                Normal(Message::MessageCannotUseAsSuperClass(reason))
            }

            ErrorMessage::EInvalidReactCreateElement { invalid_react, .. } => {
                Normal(Message::MessageInvalidReactCreateElement(invalid_react))
            }

            ErrorMessage::EAnyValueUsedAsType { reason_use } => {
                Normal(Message::MessageAnyValueUsedAsType(reason_use.desc.clone()))
            }

            ErrorMessage::EValueUsedAsType { reason_use } => {
                Normal(Message::MessageValueUsedAsType(reason_use.desc.clone()))
            }

            ErrorMessage::EExpectedStringLit {
                reason_lower,
                reason_upper,
                use_op,
            } => IncompatibleSubtyping {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            },

            ErrorMessage::EExpectedNumberLit {
                reason_lower,
                reason_upper,
                use_op,
            } => IncompatibleSubtyping {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            },

            ErrorMessage::EExpectedBooleanLit {
                reason_lower,
                reason_upper,
                use_op,
            } => IncompatibleSubtyping {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            },

            ErrorMessage::EExpectedBigIntLit {
                reason_lower,
                reason_upper,
                use_op,
            } => IncompatibleSubtyping {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            },

            ErrorMessage::EPropNotFoundInLookup {
                prop_name,
                reason_obj,
                reason_prop,
                use_op,
                suggestion,
            } => {
                let loc = reason_prop.loc.dupe();
                PropMissingInLookup {
                    loc,
                    prop: prop_name.as_ref().map(|n| n.dupe().into_smol_str()),
                    reason_obj,
                    use_op,
                    suggestion,
                    reason_indexer: None,
                }
            }

            ErrorMessage::EPropNotFoundInSubtyping {
                prop_name,
                suggestion,
                reason_lower,
                reason_upper,
                use_op,
            } => PropMissingInSubtyping {
                prop: prop_name.as_ref().map(|n| n.dupe().into_smol_str()),
                suggestion,
                reason_lower,
                reason_upper,
                reason_indexer: None,
                use_op,
            },

            ErrorMessage::EPropsNotFoundInSubtyping {
                prop_names,
                reason_lower,
                reason_upper,
                use_op,
            } => PropsMissingInSubtyping {
                props: Vec1::try_from_vec(
                    prop_names
                        .iter()
                        .map(|n| n.dupe().into_smol_str())
                        .collect(),
                )
                .expect("prop_names is non-empty Vec1"),
                reason_lower,
                reason_upper,
                use_op,
            },

            ErrorMessage::EPropsExtraAgainstExactObject {
                prop_names,
                reason_l_obj,
                reason_r_obj,
                use_op,
            } => PropsExtraAgainstExactObject {
                props: Vec1::try_from_vec(
                    prop_names
                        .iter()
                        .map(|n| n.dupe().into_smol_str())
                        .collect(),
                )
                .expect("prop_names is non-empty Vec1"),
                reason_l_obj,
                reason_r_obj,
                use_op,
            },

            ErrorMessage::EIndexerCheckFailed {
                prop_name,
                reason_lower,
                reason_upper,
                reason_indexer,
                use_op,
            } => PropMissingInSubtyping {
                prop: Some(prop_name.into_smol_str()),
                suggestion: None,
                reason_lower,
                reason_upper,
                reason_indexer: Some(reason_indexer),
                use_op,
            },

            ErrorMessage::EPropNotReadable {
                reason_prop,
                prop_name,
                use_op,
            } => {
                let loc = reason_prop.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessagePropNotReadable(prop_name),
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EPropNotWritable {
                reason_prop,
                prop_name,
                use_op,
            } => {
                let loc = reason_prop.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessagePropNotWritable(prop_name),
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EPropPolarityMismatch {
                lreason,
                ureason,
                props,
                use_op,
            } => PropPolarityMismatch {
                reason_lower: lreason,
                reason_upper: ureason,
                props: props
                    .iter()
                    .map(|(name, (lower, upper))| {
                        (
                            name.as_ref().map(|n| n.dupe().into_smol_str()),
                            *lower,
                            *upper,
                        )
                    })
                    .collect::<Vec<_>>()
                    .try_into()
                    .expect("props is non-empty Vec1"),
                use_op,
            },

            ErrorMessage::EUnionSpeculationFailed(box EUnionSpeculationFailedData {
                use_op,
                reason,
                branches,
                ..
            }) => {
                let loc = reason.loc.dupe();
                Speculation {
                    loc,
                    use_op,
                    branches,
                }
            }

            ErrorMessage::EUnsupportedExact(_, lower) => {
                Normal(Message::MessageCannotCreateExactType(lower))
            }

            ErrorMessage::EUnexpectedThisType(_) => Normal(Message::MessageUnexpectedUseOfThisType),

            ErrorMessage::EExportsAnnot(_) => Normal(Message::MessageCannotUseDollarExports),

            ErrorMessage::EUseArrayLiteral(_) => Normal(Message::MessageShouldUseArrayLiteral),

            ErrorMessage::ERecursionLimit(..) => Normal(Message::MessageRecursionLimitExceeded),

            ErrorMessage::EEnumError(EnumErrorKind::EnumsNotEnabled(_)) => {
                Normal(Message::MessageEnumsNotEnabled)
            }

            ErrorMessage::EEnumError(EnumErrorKind::EnumConstNotSupported(_)) => {
                Normal(Message::MessageEnumConstNotSupported)
            }

            ErrorMessage::EIndeterminateModuleType(_) => {
                Normal(Message::MessageCannotDetermineModuleType)
            }

            ErrorMessage::EBadExportPosition(_) => Normal(Message::MessageNonToplevelExport),

            ErrorMessage::EBadDefaultImportDestructuring(_) => {
                Normal(Message::MessageCannotUseDefaultImportWithDestrucuturing)
            }

            ErrorMessage::EThisInExportedFunction(_) => {
                Normal(Message::MessageThisInExportedFunction)
            }

            ErrorMessage::EUnreachable(_) => Normal(Message::MessageUnreachableCode),

            ErrorMessage::EInvalidLHSInAssignment(_) => {
                Normal(Message::MessageCannotAssignToInvalidLHS)
            }

            ErrorMessage::EIncompatibleWithUseOp {
                reason_lower,
                reason_upper,
                use_op,
                explanation,
            } => IncompatibleSubtyping {
                reason_lower,
                reason_upper,
                use_op,
                explanation,
            },

            ErrorMessage::EUnsupportedSetProto(_) => {
                Normal(Message::MessageCannotMutateThisPrototype)
            }

            ErrorMessage::EImplicitInexactObject(_) => {
                Normal(Message::MessageImplicitInexactObject)
            }

            ErrorMessage::EAmbiguousObjectType(_) => Normal(Message::MessageAmbiguousObjectType),

            ErrorMessage::ENonstrictImport(_) => Normal(Message::MessageNonStrictImport),

            ErrorMessage::EUnclearType(_) => Normal(Message::MessageUnclearType),

            ErrorMessage::EDeprecatedBool(_) => Normal(Message::MessageDeprecatedBool),

            ErrorMessage::EUnsafeGettersSetters(_) => Normal(Message::MessageUnsafeGetterSetter),

            ErrorMessage::EUnsafeObjectAssign(_) => Normal(Message::MessageUnsafeObjectAssign),

            ErrorMessage::EUnusedSuppression(_) => Normal(Message::MessageUnusedSuppression),

            ErrorMessage::ECodelessSuppression(_) => Normal(Message::MessageSuppressionMissingCode),

            ErrorMessage::EUnnecessaryDeclareTypeOnlyExport(_) => {
                Normal(Message::MessageUnnecessaryDeclareTypeOnlyExport)
            }

            ErrorMessage::EInvalidConstructor(reason) => {
                Normal(Message::MessageCannotUseAsConstructor(reason))
            }

            ErrorMessage::EInvalidPrototype(_, reason) => {
                Normal(Message::MessageCannotUseAsPrototype(reason))
            }

            ErrorMessage::EUnnecessaryOptionalChain(_, lhs_reason) => {
                Normal(Message::MessageUnnecessaryOptionalChain(lhs_reason))
            }

            ErrorMessage::EUnnecessaryInvariant(_, reason) => {
                Normal(Message::MessageUnnecessaryInvariant(reason))
            }

            ErrorMessage::EArithmeticOperand(reason) => Normal(
                Message::MessageCannotPerformArithOnNonNumbersOrBigInt(reason),
            ),

            ErrorMessage::EBinaryInLHS(reason) => {
                Normal(Message::MessageCannotUseInOperatorDueToBadLHS(reason))
            }

            ErrorMessage::EBinaryInRHS(reason) => {
                Normal(Message::MessageCannotUseInOperatorDueToBadRHS(reason))
            }

            ErrorMessage::EForInRHS(reason) => {
                Normal(Message::MessageCannotIterateWithForIn(reason))
            }

            ErrorMessage::EInstanceofRHS(reason) => Normal(
                Message::MessageCannotUseInstanceOfOperatorDueToBadRHS(reason),
            ),

            ErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible {
                reason_lower,
                reason_upper,
                use_op,
                enum_kind,
                representation_type,
                casting_syntax,
            }) => IncompatibleEnum {
                reason_lower,
                reason_upper,
                use_op,
                enum_kind,
                representation_type,
                casting_syntax,
            },

            ErrorMessage::EMalformedCode(_) => Normal(Message::MessageSuppressionMalformedCode),

            ErrorMessage::EComponentCase(_) => Normal(Message::MessageComponentNonUpperCase),

            ErrorMessage::EDeclareComponentInvalidParam { kind, .. } => {
                Normal(Message::MessageDeclareComponentInvalidParam(kind))
            }

            ErrorMessage::EComponentMissingReturn(reason) => {
                Normal(Message::MessageComponentMissingReturn(reason))
            }
            ErrorMessage::EComponentMissingBody(_) => Normal(Message::MessageComponentMissingBody),
            ErrorMessage::EComponentBodyInAmbientContext(_) => {
                Normal(Message::MessageComponentBodyInAmbientContext)
            }

            ErrorMessage::ENestedComponent(_) => Normal(Message::MessageCannotNestComponents),

            ErrorMessage::ENestedHook(_) => Normal(Message::MessageCannotNestHook),

            ErrorMessage::EEmptyArrayNoProvider { .. } => {
                Normal(Message::MessageCannotDetermineEmptyArrayLiteralType)
            }

            ErrorMessage::EHookNaming(_) => Normal(Message::MessageInvalidHookNaming),

            ErrorMessage::EUnusedPromise { async_: true, .. } => {
                Normal(Message::MessageUnusedPromiseInAsyncScope)
            }

            ErrorMessage::EUnusedPromise { async_: false, .. } => {
                Normal(Message::MessageUnusedPromiseInSyncScope)
            }

            ErrorMessage::EInvalidComponentRestParam(_) => {
                Normal(Message::MessageInvalidComponentRestParam)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidGuardedWildcard(_)) => {
                Normal(Message::MessageMatchInvalidGuardedWildcard)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchStatementInvalidBody { .. }) => {
                Normal(Message::MessageMatchStatementInvalidBody)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidWildcardSyntax(_)) => {
                Normal(Message::MessageMatchInvalidWildcardSyntax)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidInstancePattern(_)) => {
                Normal(Message::MessageMatchInvalidInstancePattern)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidUnaryZero { .. }) => {
                Normal(Message::MessageMatchInvalidUnaryZero)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidUnaryPlusBigInt { .. }) => {
                Normal(Message::MessageMatchInvalidUnaryPlusBigInt)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchBindingInOrPattern { .. }) => {
                Normal(Message::MessageMatchBindingInOrPattern)
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidAsPattern { .. }) => {
                Normal(Message::MessageMatchInvalidAsPattern)
            }

            ErrorMessage::EUndocumentedFeature { .. } => {
                Normal(Message::MessageUndocumentedFeature)
            }

            ErrorMessage::EFunctionCallExtraArg(unused_reason, def_reason, param_count, use_op) => {
                UseOp {
                    loc: unused_reason.loc.dupe(),
                    message: Message::MessageCannotCallFunctionWithExtraArg {
                        def_reason,
                        param_count,
                    },
                    explanation: None,
                    use_op,
                }
            }
            ErrorMessage::EExponentialSpread {
                reason,
                reasons_for_operand1,
                reasons_for_operand2,
            } => Normal(Message::MessageExponentialSpread {
                reason,
                reasons_for_operand1,
                reasons_for_operand2,
            }),
            ErrorMessage::EComputedPropertyWithUnion(reason) => {
                Normal(Message::MessageCannotUseComputedPropertyWithUnion(reason))
            }

            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberAccess {
                member_name,
                suggestion,
                reason,
                enum_reason,
            }) => Normal(Message::MessageCannotAccessEnumMember {
                member_name,
                suggestion,
                description: reason.desc.clone(),
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumModification { enum_reason, .. }) => {
                Normal(Message::MessageCannotChangeEnumMember(enum_reason))
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberDuplicateValue {
                prev_use_loc,
                enum_reason,
                ..
            }) => Normal(Message::MessageDuplicateEnumMember {
                prev_use_loc,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotIterable { reason, for_in }) => {
                Normal(Message::MessageCannotIterateEnum { reason, for_in })
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberAlreadyChecked {
                prev_check_loc,
                enum_reason,
                member_name,
                ..
            }) => Normal(Message::MessageAlreadyExhaustivelyCheckOneEnumMember {
                prev_check_loc,
                enum_reason,
                member_name,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumAllMembersAlreadyChecked {
                enum_reason,
                ..
            }) => Normal(Message::MessageAlreadyExhaustivelyCheckAllEnumMembers { enum_reason }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked {
                reason,
                enum_reason,
                left_to_check,
                default_case_loc,
            }) => Normal(Message::MessageIncompleteExhausiveCheckEnum {
                description: reason.desc.clone(),
                enum_reason,
                left_to_check,
                default_case_loc,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumUnknownNotChecked {
                reason,
                enum_reason,
            }) => Normal(Message::MessageCannotExhaustivelyCheckEnumWithUnknowns {
                description: reason.desc.clone(),
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidCheck {
                enum_reason,
                example_member,
                from_match,
                ..
            }) => Normal(Message::MessageInvalidEnumMemberCheck {
                enum_reason,
                example_member,
                from_match,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberUsedAsType {
                reason,
                enum_reason,
            }) => Normal(Message::MessageCannotUseEnumMemberUsedAsType {
                description: reason.desc.clone(),
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidAbstractUse {
                reason,
                enum_reason,
            }) => Normal(Message::MessageCannotExhaustivelyCheckAbstractEnums {
                description: reason.desc.clone(),
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberName {
                enum_reason,
                member_name,
                ..
            }) => Normal(Message::MessageInvalidEnumMemberName {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNonIdentifierMemberName {
                enum_reason,
                member_name,
                ..
            }) => Normal(Message::MessageEnumNonIdentifierMemberName {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumDuplicateMemberName {
                prev_use_loc,
                enum_reason,
                member_name,
                ..
            }) => Normal(Message::MessageEnumDuplicateMemberName {
                member_name,
                prev_use_loc,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInconsistentMemberValues {
                enum_reason,
                ..
            }) => Normal(Message::MessageEnumInconsistentMemberValues { enum_reason }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberInitializer {
                enum_reason,
                explicit_type,
                member_name,
                ..
            }) => Normal(Message::MessageEnumInvalidMemberInitializer {
                member_name,
                explicit_type,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumBooleanMemberNotInitialized {
                enum_reason,
                member_name,
                ..
            }) => Normal(Message::MessageEnumBooleanMemberNotInitialized {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNumberMemberNotInitialized {
                enum_reason,
                member_name,
                ..
            }) => Normal(Message::MessageEnumNumberMemberNotInitialized {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumBigIntMemberNotInitialized {
                enum_reason,
                member_name,
                ..
            }) => Normal(Message::MessageEnumBigIntMemberNotInitialized {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(
                EnumErrorKind::EnumStringMemberInconsistentlyInitialized { enum_reason, .. },
            ) => Normal(Message::MessageEnumStringMemberInconsistentlyInitialized { enum_reason }),

            ErrorMessage::EDuplicateClassMember {
                name,
                is_static,
                class_kind,
                ..
            } => Normal(Message::MessageDuplicateClassMember {
                name,
                static_: is_static,
                class_kind,
            }),

            ErrorMessage::EInvalidDeclaration {
                declaration,
                null_write: None,
                possible_generic_escape_locs,
            } if possible_generic_escape_locs.is_empty() => Normal(
                Message::MessageVariableNeverInitAssignedAnnotated(declaration),
            ),
            ErrorMessage::EInvalidDeclaration {
                declaration,
                null_write: None,
                possible_generic_escape_locs,
            } => Normal(
                Message::MessageShouldAnnotateVariableOnlyInitializedInGenericContext {
                    reason: declaration,
                    possible_generic_escape_locs,
                },
            ),
            ErrorMessage::EInvalidDeclaration {
                declaration,
                null_write: Some(null_write),
                possible_generic_escape_locs,
            } if possible_generic_escape_locs.is_empty() => {
                let null_loc = if null_write.initialized {
                    None
                } else {
                    Some(null_write.null_loc)
                };
                Normal(Message::MessageVariableOnlyAssignedByNull {
                    reason: declaration,
                    null_loc,
                })
            }
            ErrorMessage::EInvalidDeclaration {
                declaration,
                null_write: Some(null_write),
                possible_generic_escape_locs,
            } => Normal(Message::MessageShouldAnnotateVariableUsedInGenericContext {
                reason: declaration,
                null_loc: null_write.null_loc,
                initialized: null_write.initialized,
                possible_generic_escape_locs,
            }),

            ErrorMessage::EAnnotationInference(_, reason_op, reason, suggestion) => {
                Normal(Message::MessageCannotUseTypeForAnnotationInference {
                    reason_op,
                    reason,
                    suggestion,
                })
            }
            ErrorMessage::ETrivialRecursiveDefinition(_, reason) => Normal(
                Message::MessageInvalidTrivialRecursiveDefinition(reason.desc.clone()),
            ),
            ErrorMessage::ERecursiveDefinition {
                reason,
                recursion,
                annot_locs,
            } => Normal(Message::MessageDefinitionInvalidRecursive {
                description: reason.desc.clone(),
                recursion,
                annot_locs,
            }),
            ErrorMessage::EDefinitionCycle(dependencies) => {
                Normal(Message::MessageDefinitionCycle(dependencies))
            }
            ErrorMessage::EReferenceInAnnotation(_, name, loc) => {
                Normal(Message::MessageInvalidSelfReferencingTypeAnnotation { name, loc })
            }

            ErrorMessage::EBigIntRShift3(reason) => {
                Normal(Message::MessageCannotPerformBigIntRShift3(reason))
            }
            ErrorMessage::EBigIntNumCoerce(reason) => {
                Normal(Message::MessageCannotPerformBigIntUnaryPlus(reason))
            }

            ErrorMessage::EInvalidCatchParameterAnnotation {
                ts_utility_syntax, ..
            } => Normal(Message::MessageInvalidCatchParameterAnnotation { ts_utility_syntax }),

            ErrorMessage::EInvalidMappedType { kind, .. } => {
                let msg = match kind {
                    InvalidMappedTypeErrorKind::InterfaceOrDeclaredClass => {
                        Message::MessageInvalidMappedTypeInInterfaceOrDeclaredClass
                    }
                    InvalidMappedTypeErrorKind::ExtraProperties => {
                        Message::MessageInvalidMappedTypeWithExtraProps
                    }
                    InvalidMappedTypeErrorKind::ExplicitExactOrInexact => {
                        Message::MessageInvalidMappedTypeWithExactOrInexact
                    }
                    InvalidMappedTypeErrorKind::RemoveOptionality => {
                        Message::MessageInvalidMappedTypeWithOptionalityRemoval
                    }
                    InvalidMappedTypeErrorKind::VarianceOnArrayInput => {
                        Message::MessageInvalidMappedTypeWithVarianceOnArrayInput
                    }
                };
                Normal(msg)
            }

            ErrorMessage::EDuplicateComponentProp { spread, duplicates } => {
                Normal(Message::MessageRedeclareComponentProp {
                    duplicates,
                    spread_loc: spread,
                })
            }
            ErrorMessage::ERefComponentProp {
                spread: spread_loc,
                loc: ref_loc,
            } => Normal(Message::MessageInvalidRefPropertyInSpread {
                ref_loc,
                spread_loc,
            }),
            ErrorMessage::EKeySpreadProp {
                spread: spread_loc,
                loc: key_loc,
            } => Normal(Message::MessageInvalidKeyPropertyInSpread {
                spread_loc,
                key_loc,
            }),

            ErrorMessage::EInvalidRendersTypeArgument {
                renders_variant,
                invalid_render_type_kind,
                invalid_type_reasons,
                ..
            } => Normal(Message::MessageInvalidRendersTypeArgument {
                renders_variant,
                invalid_render_type_kind,
                invalid_type_reasons,
            }),

            ErrorMessage::EInvalidTypeCastSyntax {
                enabled_casting_syntax,
                ..
            } => Normal(Message::MessageInvalidTypeCastingSyntax(
                enabled_casting_syntax,
            )),

            ErrorMessage::ECannotCallReactComponent { reason } => {
                Normal(Message::MessageCannotCallReactComponent(reason))
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchNotExhaustive { examples, .. }) => {
                Normal(Message::MessageMatchNotExhaustive { examples })
            }
            ErrorMessage::EMatchError(MatchErrorKind::MatchUnusedPattern {
                reason,
                already_seen,
            }) => Normal(Message::MessageMatchUnnecessaryPattern {
                reason,
                already_seen,
            }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExhaustiveObjectPattern {
                rest,
                missing_props,
                pattern_kind,
                ..
            }) => Normal(Message::MessageMatchNonExhaustiveObjectPattern {
                rest,
                missing_props: missing_props.to_vec(),
                pattern_kind,
            }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck {
                wildcard_reason,
                unchecked_members,
                ..
            }) => Normal(Message::MessageMatchNonExplicitEnumCheck {
                wildcard_reason,
                unchecked_members: unchecked_members.to_vec(),
            }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidIdentOrMemberPattern {
                type_reason,
                ..
            }) => Normal(Message::MessageMatchInvalidIdentOrMemberPattern { type_reason }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidBindingKind { kind, .. }) => {
                Normal(Message::MessageMatchInvalidBindingKind { kind })
            }
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidObjectPropertyLiteral {
                pattern_kind,
                ..
            }) => Normal(Message::MessageMatchInvalidObjectPropertyLiteral { pattern_kind }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchDuplicateObjectProperty {
                name,
                pattern_kind,
                ..
            }) => Normal(Message::MessageMatchDuplicateObjectProperty { name, pattern_kind }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidPatternReference {
                binding_reason,
                ..
            }) => Normal(Message::MessageMatchInvalidPatternReference { binding_reason }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidObjectShorthand {
                name,
                pattern_kind,
                ..
            }) => Normal(Message::MessageMatchInvalidObjectShorthand { name, pattern_kind }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidCaseSyntax { kind, .. }) => {
                Normal(Message::MessageMatchInvalidCaseSyntax(kind))
            }

            ErrorMessage::ERecordError(record_error) => match record_error {
                RecordErrorKind::RecordBannedTypeUtil {
                    reason_op,
                    reason_record,
                } => Normal(Message::MessageRecordBannedTypeUtil {
                    reason_op,
                    reason_record,
                }),
                RecordErrorKind::RecordInvalidName { name, .. } => {
                    Normal(Message::MessageRecordInvalidName { name })
                }
                RecordErrorKind::RecordInvalidNew { record_name, .. } => {
                    Normal(Message::MessageRecordInvalidNew { record_name })
                }
                RecordErrorKind::RecordDeclarationInvalidSyntax { kind, .. } => {
                    Normal(Message::MessageRecordDeclarationInvalidSyntax(kind))
                }
            },

            ErrorMessage::EIllegalAssertOperator {
                obj, specialized, ..
            } => Normal(Message::MessageIllegalAssertOperator { obj, specialized }),

            ErrorMessage::EReferenceInDefault(def_loc, name, ref_loc) => {
                Normal(Message::MessageInvalidSelfReferencingDefault {
                    name,
                    def_loc,
                    ref_loc,
                })
            }

            ErrorMessage::EIncompatibleProp {
                prop,
                reason_prop,
                reason_obj,
                use_op,
                ..
            } => PropMissingInLookup {
                loc: reason_prop.loc.dupe(),
                prop: prop.map(|n| n.into_smol_str()),
                reason_obj,
                use_op: use_op.unwrap_or(VirtualUseOp::Op(Arc::new(VirtualRootUseOp::UnknownUse))),
                suggestion: None,
                reason_indexer: None,
            },

            ErrorMessage::EDevOnlyRefinedLocInfo { refining_locs, .. } => {
                Normal(Message::MessageDevOnlyRefinedLocInfo { refining_locs })
            }

            ErrorMessage::EDevOnlyInvalidatedRefinementInfo {
                invalidation_info, ..
            } => Normal(Message::MessageDevOnlyInvalidatedRefinementInfo(
                invalidation_info,
            )),

            ErrorMessage::ETemporaryHardcodedErrorForPrototyping(_, str) => {
                Normal(Message::MessagePlainTextReservedForInternalErrorOnly(str))
            }

            ErrorMessage::ENoDefaultExport(_, module_name, suggestion) => {
                Normal(Message::MessageNoDefaultExport {
                    module_name: module_name.into_inner(),
                    suggestion,
                })
            }

            ErrorMessage::EOnlyDefaultExport(_, module_name, export_name) => {
                Normal(Message::MessageOnlyDefaultExport {
                    module_name: module_name.into_inner(),
                    export_name,
                })
            }

            ErrorMessage::ENoNamedExport(_, module_name, export_name, suggestion) => {
                Normal(Message::MessageNoNamedExport {
                    module_name: module_name.into_inner(),
                    export_name,
                    suggestion,
                })
            }

            ErrorMessage::EMissingTypeArgs {
                reason_tapp,
                arity_loc,
                min_arity,
                max_arity,
                ..
            } => Normal(Message::MessageCannotUseTypeWithoutAnyTypeArgs {
                reason_arity: VirtualReason::new(reason_tapp.desc.clone(), arity_loc),
                min_arity,
                max_arity,
            }),

            ErrorMessage::ETooManyTypeArgs {
                reason_tapp,
                arity_loc,
                maximum_arity,
                ..
            } => Normal(Message::MessageCannotUseTypeWithTooManyTypeArgs {
                reason_arity: VirtualReason::new(reason_tapp.desc.clone(), arity_loc),
                n: maximum_arity,
            }),

            ErrorMessage::ETooFewTypeArgs {
                reason_tapp,
                arity_loc,
                minimum_arity,
                ..
            } => Normal(Message::MessageCannotUseTypeWithTooFewTypeArgs {
                reason_arity: VirtualReason::new(reason_tapp.desc.clone(), arity_loc),
                n: minimum_arity,
            }),

            ErrorMessage::EInvalidTypeArgs(reason_main, reason_tapp) => {
                Normal(Message::MessageCannotUseTypeWithInvalidTypeArgs {
                    reason_main,
                    reason_tapp,
                })
            }

            ErrorMessage::EConstantCondition {
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
                ..
            } => Normal(Message::MessageConstantCondition {
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
            }),

            ErrorMessage::ETypeParamArity(_, n) => {
                if n == 0 {
                    Normal(Message::MessageCannotApplyNonPolymorphicType)
                } else {
                    Normal(Message::MessageCannotUseTypeWithoutExactlyNTypeArgs(n))
                }
            }

            ErrorMessage::ETypeParamMinArity(_, n) => {
                Normal(Message::MessageCannotUseTypeWithoutAtLeastNTypeArgs(n))
            }

            ErrorMessage::ECallTypeArity {
                is_new,
                reason_arity,
                expected_arity,
                ..
            } => Normal(Message::MessageCannotUseNonPolymorphicTypeWithTypeArgs {
                is_new,
                reason_arity,
                expected_arity,
            }),

            ErrorMessage::EPolarityMismatch {
                reason,
                name,
                expected_polarity,
                actual_polarity,
            } => {
                let reason_targ = VirtualReason::new(
                    flow_common::reason::VirtualReasonDesc::RIdentifier(Name::new(name)),
                    reason.def_loc_opt.as_ref().unwrap_or(&reason.loc).dupe(),
                );
                Normal(Message::MessageCannotUseTypeDueToPolarityMismatch {
                    reason_targ,
                    expected_polarity,
                    actual_polarity,
                })
            }

            ErrorMessage::EBuiltinNameLookupFailed { name, .. } => {
                Normal(Message::MessageCannotResolveBuiltinName(name))
            }

            ErrorMessage::EBuiltinModuleLookupFailed {
                name,
                potential_generator,
                ..
            } => Normal(Message::MessageCannotResolveBuiltinModule {
                name,
                potential_generator,
            }),

            ErrorMessage::EExpectedModuleLookupFailed {
                name,
                expected_module_purpose,
                ..
            } => Normal(Message::MessageCannotResolveExpectedModule {
                name,
                expected_module_purpose,
            }),

            ErrorMessage::EPrivateLookupFailed(reasons, x, use_op) => PropMissingInLookup {
                loc: reasons.0.loc.dupe(),
                prop: Some(format!("#{}", x).into()),
                reason_obj: reasons.1,
                use_op,
                suggestion: None,
                reason_indexer: None,
            },

            ErrorMessage::EPlatformSpecificImplementationModuleLookupFailed { name, .. } => {
                Normal(Message::MessagePlatformSpecificImplementationModuleLookupFailed(name))
            }

            ErrorMessage::EComparison {
                r1,
                r2,
                strict_comparison_opt,
                ..
            } => Normal(Message::MessageCannotCompare {
                lower: r1,
                upper: r2,
                strict_comparison_opt,
            }),

            ErrorMessage::ENonStrictEqualityComparison(lower, upper) => {
                Normal(Message::MessageCannotCompareNonStrict { lower, upper })
            }

            ErrorMessage::ETupleArityMismatch {
                use_op,
                lower_reason,
                lower_arity,
                lower_inexact,
                upper_reason,
                upper_arity,
                upper_inexact,
                unify,
            } => {
                let loc = lower_reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleTupleArity {
                        lower_reason,
                        lower_arity,
                        lower_inexact,
                        upper_reason,
                        upper_arity,
                        upper_inexact,
                        unify,
                    },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETupleRequiredAfterOptional {
                reason_tuple,
                reason_required,
                reason_optional,
            } => Normal(Message::MessageInvalidTupleRequiredAfterOptional {
                reason_tuple,
                reason_required,
                reason_optional,
            }),

            ErrorMessage::ETupleInvalidTypeSpread { reason_arg, .. } => {
                Normal(Message::MessageInvalidTupleTypeSpread(reason_arg))
            }

            ErrorMessage::ETupleElementAfterInexactSpread(_) => {
                Normal(Message::MessageTupleElementAfterInexactSpread)
            }

            ErrorMessage::ENonLitArrayToTuple((lower, upper), use_op) => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleNonLiteralArrayToTuple { lower, upper },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETupleOutOfBounds {
                reason,
                reason_op,
                inexact,
                length,
                index,
                use_op,
            } => UseOp {
                loc: reason.loc.dupe(),
                message: Message::MessageTupleIndexOutOfBound {
                    reason_op,
                    inexact,
                    length,
                    index,
                },
                use_op,
                explanation: None,
            },

            ErrorMessage::ETupleNonIntegerIndex {
                reason,
                index,
                use_op,
            } => {
                let loc = reason.loc.dupe();
                let index_def_loc = reason.def_loc_opt.as_ref().unwrap_or(&reason.loc).dupe();
                UseOp {
                    loc,
                    message: Message::MessageTupleNonIntegerIndex {
                        index_def_loc,
                        index,
                    },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETupleUnsafeWrite { reason, use_op } => UseOp {
                loc: reason.loc.dupe(),
                message: Message::MessageTupleNonStaticallyKnownIndex,
                use_op,
                explanation: None,
            },

            ErrorMessage::ETupleElementNotReadable {
                reason,
                index,
                name,
                use_op,
            } => {
                let loc = reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageTupleElementNotReadable {
                        reason,
                        index,
                        name,
                    },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETupleElementNotWritable {
                reason,
                index,
                name,
                use_op,
            } => {
                let loc = reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageTupleElementNotWritable {
                        reason,
                        index,
                        name,
                    },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETupleElementPolarityMismatch {
                index,
                reason_lower,
                polarity_lower,
                reason_upper,
                polarity_upper,
                use_op,
            } => {
                let loc = reason_lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageTuplePolarityMismatch {
                        index,
                        reason_lower,
                        reason_upper,
                        polarity_lower,
                        polarity_upper,
                    },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EROArrayWrite(reasons, use_op) => {
                let (lower, _) = reasons;
                UseOp {
                    loc: lower.loc.dupe(),
                    message: Message::MessageReadonlyArraysCannotBeWrittenTo,
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EIncompatibleWithExact((lower, upper), use_op, kind) => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleWithExact { kind, lower, upper },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EFunctionIncompatibleWithIndexer((lower, upper), use_op) => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleWithIndexed { lower, upper },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EStrUtilTypeNonLiteralArg(_) => {
                Normal(Message::MessageCannotUseStrUtilType)
            }

            ErrorMessage::EUnsupportedKeyInObject {
                key_error_kind,
                obj_kind,
                ..
            } => Normal(Message::MessageUnsupportedKeyInObject {
                key_error_kind,
                obj_kind,
            }),

            ErrorMessage::EAmbiguousNumericKeyWithVariance(_) => {
                Normal(Message::MessageAmbiguousNumericKeyWithVariance)
            }

            ErrorMessage::ETypeGuardFuncIncompatibility {
                use_op,
                reasons: (lower, upper),
            } => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleNonTypeGuardToTypeGuard { lower, upper },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETypeGuardInvalidParameter {
                type_guard_reason,
                binding_reason,
            } => Normal(Message::MessageCannotReferenceTypeGuardParameter {
                type_guard_reason,
                binding_reason,
            }),

            ErrorMessage::ETypeGuardIndexMismatch {
                use_op,
                reasons: (lower, upper),
            } => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageTypeGuardIndexMismatch { lower, upper },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETypeGuardImpliesMismatch {
                use_op,
                reasons: (lower, upper),
            } => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageTypeGuardImpliesMismatch { lower, upper },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETypeGuardParamUnbound(reason) => {
                Normal(Message::MessageInvalidTypeGuardParamUnbound(reason))
            }

            ErrorMessage::ETypeGuardThisParam(reason) => {
                Normal(Message::MessageInvalidTypeGuardThisParam(reason))
            }

            ErrorMessage::ETypeGuardFunctionInvalidWrites {
                type_guard_reason,
                write_locs,
                ..
            } => Normal(Message::MessageInvalidTypeGuardFunctionWritten {
                type_guard_reason,
                write_locs,
            }),

            ErrorMessage::ENegativeTypeGuardConsistency {
                reason,
                return_reason,
                type_reason,
            } => Normal(Message::MessageNegativeTypeGuardConsistency {
                reason,
                return_reason,
                type_reason,
            }),

            ErrorMessage::ETypeParamConstIncompatibility {
                use_op,
                lower,
                upper,
            } => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatiblETypeParamConstIncompatibility {
                        lower,
                        upper,
                    },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::ETypeParamConstInvalidPosition(reason) => {
                Normal(Message::MessageTypeParamConstInvalidPosition(reason))
            }

            ErrorMessage::ETypeGuardFunctionParamHavoced {
                type_guard_reason,
                param_reason,
                call_locs,
            } => Normal(Message::MessageCannotUseTypeGuardWithFunctionParamHavoced {
                type_guard_desc: type_guard_reason.desc.clone(),
                param_reason,
                call_locs,
            }),

            ErrorMessage::ETypeGuardIncompatibleWithFunctionKind { kind, .. } => {
                Normal(Message::MessageInvalidTypeGuardFunctionKind(kind))
            }

            ErrorMessage::EInternal(_, internal_error) => {
                let msg = format!(
                    "Internal error: {}",
                    string_of_internal_error(&internal_error)
                );
                Normal(Message::MessagePlainTextReservedForInternalErrorOnly(
                    msg.into(),
                ))
            }

            ErrorMessage::EUnsupportedSyntax(_, unsupported_syntax) => {
                Normal(Message::MessageUnsupportedSyntax(unsupported_syntax))
            }

            ErrorMessage::EMissingLocalAnnotation {
                reason,
                hint_available,
                from_generic_function,
            } => {
                if hint_available {
                    Normal(
                        Message::MessageMissingAnnotationDueToContextualTypingFailure(
                            reason.desc.clone(),
                        ),
                    )
                } else if from_generic_function {
                    Normal(Message::MessageMissingAnnotationForGenericFunction(
                        reason.desc.clone(),
                    ))
                } else {
                    Normal(Message::MessageMissingAnnotation(reason.desc.clone()))
                }
            }

            ErrorMessage::EBindingError(binding_error, _, x, entry_loc) => {
                let x_reason = VirtualReason::new(
                    flow_common::reason::VirtualReasonDesc::RIdentifier(x),
                    entry_loc,
                );

                let msg = match binding_error {
                    BindingError::EGlobalAlreadyDeclared => {
                        Message::MessageCannotDeclareAlreadyBoundGlobal(x_reason.dupe())
                    }
                    BindingError::ENameAlreadyBound => {
                        Message::MessageCannotDeclareAlreadyBoundName(x_reason.dupe())
                    }
                    BindingError::ENameAlreadyBoundInCoreJs => {
                        Message::MessageCannotDeclareAlreadyBoundNameInCoreJs(x_reason.dupe())
                    }
                    BindingError::EVarRedeclaration => {
                        Message::MessageCannotRedeclareVar(x_reason.dupe())
                    }
                    BindingError::EReferencedBeforeDeclaration => {
                        Message::MessageCannotUseBeforeDeclaration(x_reason.dupe())
                    }
                    BindingError::EReferencedThisSuperBeforeSuperCall => {
                        Message::MessageCannotUseThisSuperBeforeSuperCall(x_reason.dupe())
                    }
                    BindingError::ETypeInValuePosition {
                        imported,
                        type_only_namespace,
                        name,
                    } => {
                        let imported_name = if imported { Some(name) } else { None };
                        Message::MessageCannotUseTypeInValuePosition {
                            reason: x_reason,
                            type_only_namespace,
                            imported_name,
                        }
                    }
                    BindingError::EConstReassigned | BindingError::EConstParamReassigned => {
                        Message::MessageCannotReassignConstant(x_reason)
                    }
                    BindingError::EImportReassigned => {
                        Message::MessageCannotReassignImport(x_reason)
                    }
                    BindingError::EEnumReassigned => Message::MessageCannotReassignEnum(x_reason),
                    BindingError::EReservedKeyword { keyword } => {
                        Message::MessageCannotDeclareReservedType {
                            keyword,
                            reason: x_reason,
                        }
                    }
                };
                Normal(msg)
            }

            ErrorMessage::EUninitializedInstanceProperty(_, err) => {
                Normal(Message::MessageUninitializedInstanceProperty(err))
            }

            ErrorMessage::EBadExportContext(name, _) => Normal(
                Message::MessageCannotUseExportInNonLegalToplevelContext(name),
            ),

            ErrorMessage::EBadDefaultImportAccess(_, import_star_reason) => {
                Normal(Message::MessageInvalidImportStarUse(import_star_reason))
            }

            ErrorMessage::EInvalidImportStarUse(_, import_star_reason) => {
                Normal(Message::MessageCannotUseImportStar(import_star_reason))
            }

            ErrorMessage::ENonConstVarExport(_, decl_reason) => {
                Normal(Message::MessageNonConstVarExport(decl_reason))
            }

            ErrorMessage::EMixedImportAndRequire(_, import_reason) => Normal(
                Message::MessageCannotUseMixedImportAndRequire(import_reason),
            ),

            ErrorMessage::EUnsupportedVarianceAnnotation(_, kind) => {
                Normal(Message::MessageUnsupportedVarianceAnnotation(kind))
            }

            ErrorMessage::EExportRenamedDefault {
                name, is_reexport, ..
            } => Normal(Message::MessageCannotExportRenamedDefault { name, is_reexport }),

            ErrorMessage::ECannotDelete(_, expr) => Normal(Message::MessageCannotDelete(expr)),

            ErrorMessage::ESignatureBindingValidation(sve) => {
                use flow_type_sig::signature_error::BindingValidation as BV;
                let msg = match sve {
                    BV::ModuleOverride {
                        name,
                        existing_binding_loc,
                        ..
                    } => {
                        let x = VirtualReason::new(
                            flow_common::reason::VirtualReasonDesc::RIdentifier(Name::new(name)),
                            existing_binding_loc,
                        );
                        Message::MessageBadLibdefModuleOverride(x)
                    }
                    BV::NameOverride {
                        name,
                        existing_binding_loc,
                        ..
                    } => {
                        let x = VirtualReason::new(
                            flow_common::reason::VirtualReasonDesc::RIdentifier(Name::new(name)),
                            existing_binding_loc,
                        );
                        Message::MessageBadLibdefNameOverride(x)
                    }
                    BV::NamespacedNameAlreadyBound {
                        name,
                        existing_binding_loc,
                        ..
                    } => {
                        let x = VirtualReason::new(
                            flow_common::reason::VirtualReasonDesc::RIdentifier(Name::new(name)),
                            existing_binding_loc,
                        );
                        Message::MessageCannotDeclareAlreadyBoundNameInNamespace(x)
                    }
                };
                Normal(msg)
            }

            ErrorMessage::ESignatureVerification(sve) => {
                Normal(Message::MessageCannotBuildTypedInterface(sve))
            }

            ErrorMessage::EInvalidObjectKit { reason, use_op, .. } => {
                let loc = reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageLowerIsNotObject(reason),
                    explanation: None,
                    use_op,
                }
            }

            ErrorMessage::EInvalidTypeof(_, typename) => {
                Normal(Message::MessageInvalidGenericRef(typename))
            }

            ErrorMessage::EObjectComputedPropertyAccess {
                reason_obj,
                reason_prop,
                kind,
            } => Normal(Message::MessageCannotAccessObjectWithComputedProp {
                reason_obj,
                reason_prop,
                kind,
            }),

            ErrorMessage::EObjectComputedPropertyAssign(reason_prop, Some(reason_key), kind) => {
                Normal(
                    Message::MessageCannotAssignToObjectWithComputedPropWithKey {
                        reason_prop,
                        reason_key,
                        kind,
                    },
                )
            }

            ErrorMessage::EObjectComputedPropertyAssign(reason_prop, None, _) => Normal(
                Message::MessageCannotAssignToObjectWithComputedProp(reason_prop),
            ),

            ErrorMessage::EObjectComputedPropertyPotentialOverwrite {
                key_loc,
                overwritten_locs,
            } => Normal(
                Message::MessageCannotAddComputedPropertyDueToPotentialOverwrite {
                    key_loc,
                    overwritten_locs,
                },
            ),

            ErrorMessage::EUnsupportedImplements(reason) => Normal(
                Message::MessageCannotImplementNonInterface(reason.desc.clone()),
            ),

            ErrorMessage::ENotAReactComponent { reason, use_op } => {
                let loc = reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageLowerIsNotReactComponent(reason),
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EReactElementFunArity(_, fn_name, n) => {
                Normal(Message::MessageCannotCallReactFunctionWithoutAtLeastNArgs { fn_name, n })
            }

            ErrorMessage::EReactRefInRender {
                usage,
                kind,
                in_hook,
            } => {
                use crate::error_message::RefInRenderKind;
                match kind {
                    RefInRenderKind::Argument => {
                        Normal(Message::MessageCannotPassReactRefAsArgument { usage, in_hook })
                    }
                    RefInRenderKind::Access => {
                        Normal(Message::MessageCannotAccessReactRefInRender { usage, in_hook })
                    }
                }
            }

            ErrorMessage::EDuplicateModuleProvider {
                module_name,
                provider,
                conflict,
            } => Normal(Message::MessageDuplicateModuleProvider {
                module_name,
                provider,
                conflict,
            }),

            ErrorMessage::EParseError(_, parse_error) => {
                Normal(Message::MessageParseError(parse_error))
            }

            ErrorMessage::EDocblockError(_, err) => Normal(Message::MessageDocblockError(err)),

            ErrorMessage::EUntypedTypeImport(_, module_name) => Normal(
                Message::MessageUntypedTypeImport(module_name.display().to_string().into()),
            ),

            ErrorMessage::EUntypedImport(_, module_name) => Normal(Message::MessageUntypedImport(
                module_name.display().to_string().into(),
            )),

            ErrorMessage::EInternalType(_, kind) => Normal(Message::MessageInternalType(kind)),

            ErrorMessage::EIncorrectTypeWithReplacement { kind, .. } => {
                Normal(Message::MessageIncorrectType(kind))
            }

            ErrorMessage::ELintSetting(_, kind) => {
                Normal(Message::MessageInvalidLintSettings(kind))
            }

            ErrorMessage::ESketchyNullLint {
                kind,
                falsy_loc,
                null_loc,
                ..
            } => Normal(Message::MessageSketchyNullCheck {
                kind,
                falsy_loc,
                null_loc,
            }),

            ErrorMessage::ESketchyNumberLint(_, reason) => {
                Normal(Message::MessageSketchyNumber(reason))
            }

            ErrorMessage::EPrimitiveAsInterface {
                use_op,
                reason,
                interface_reason,
                kind,
            } => {
                let loc = reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageCannotUsePrimitiveAsInterface {
                        reason,
                        interface_reason,
                        kind,
                    },
                    explanation: None,
                    use_op,
                }
            }

            ErrorMessage::ECannotSpreadInterface {
                spread_reason,
                interface_reason,
                use_op,
            } => {
                let loc = spread_reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageCannotSpreadInterface {
                        spread_reason,
                        interface_reason,
                    },
                    explanation: None,
                    use_op,
                }
            }

            ErrorMessage::ECannotSpreadIndexerOnRight {
                spread_reason,
                object_reason,
                key_reason,
                use_op,
            } => {
                let loc = spread_reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageCannotSpreadDueToPotentialOverwrite {
                        spread_reason,
                        object_reason,
                        key_reason,
                    },
                    explanation: None,
                    use_op,
                }
            }

            ErrorMessage::EUnableToSpread {
                spread_reason,
                object1_reason,
                object2_reason,
                propname,
                error_kind,
                use_op,
            } => {
                let loc = spread_reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageCannotSpreadGeneral {
                        spread_reason,
                        object1_reason,
                        object2_reason,
                        propname,
                        error_kind,
                    },
                    explanation: None,
                    use_op,
                }
            }

            ErrorMessage::EInexactMayOverwriteIndexer {
                spread_reason,
                key_reason,
                value_reason,
                object2_reason,
                use_op,
            } => {
                let loc = spread_reason.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageCannotSpreadInexactMayOverwriteIndexer {
                        spread_reason,
                        key_reason,
                        value_reason,
                        object2_reason,
                    },
                    explanation: None,
                    use_op,
                }
            }

            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectUtilType {
                reason,
                enum_reason,
            }) => Normal(Message::MessageCannotInstantiateObjectUtilTypeWithEnum {
                description: reason.desc.clone(),
                enum_reason,
            }),

            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectFunction {
                reason,
                enum_reason,
            }) => Normal(Message::MessageCannotCallObjectFunctionOnEnum {
                reason,
                enum_reason,
            }),

            ErrorMessage::EAssignConstLikeBinding {
                definition,
                binding_kind,
                ..
            } => Normal(Message::MessageCannotReassignConstantLikeBinding {
                definition,
                binding_kind,
            }),

            ErrorMessage::EObjectThisSuperReference(_, reason, k) => {
                let converted_kind = match k {
                    ThisFinderKind::This => crate::intermediate_error_types::ThisFinderKind::This,
                    ThisFinderKind::Super => crate::intermediate_error_types::ThisFinderKind::Super,
                };
                Normal(Message::MessageThisSuperInObject(reason, converted_kind))
            }

            ErrorMessage::EComponentThisReference { component_loc, .. } => {
                Normal(Message::MessageThisInComponent(component_loc))
            }

            ErrorMessage::EImplicitInstantiationUnderconstrainedError {
                reason_call,
                reason_tparam,
                use_op,
                ..
            } => {
                let loc = reason_call.loc.dupe();
                UseOp {
                    use_op,
                    message: Message::MessageUnderconstrainedImplicitInstantiaton {
                        reason_call,
                        reason_tparam,
                    },
                    loc,
                    explanation: None,
                }
            }

            ErrorMessage::EClassToObject {
                reason_class,
                reason_obj,
                use_op,
                kind,
            } => {
                let loc = reason_class.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleClassToObject {
                        reason_class,
                        reason_obj,
                        kind,
                    },
                    explanation: None,
                    use_op,
                }
            }

            ErrorMessage::EMethodUnbinding {
                use_op,
                reason_op,
                reason_prop,
            } => {
                let loc = reason_op.loc.dupe();
                let context_loc = reason_prop
                    .def_loc_opt
                    .as_ref()
                    .unwrap_or(&reason_prop.loc)
                    .dupe();
                UseOp {
                    loc,
                    message: Message::MessageMethodUnbinding {
                        reason_op,
                        context_loc,
                    },
                    use_op,
                    explanation: None,
                }
            }

            ErrorMessage::EHookIncompatible {
                use_op,
                lower,
                upper,
                lower_is_hook,
                hook_is_annot,
            } => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleReactHooksWithNonReactHook {
                        lower,
                        upper,
                        lower_is_hook,
                        hook_is_annot,
                    },
                    use_op,
                    explanation: Some(
                        Explanation::ExplanationReactHookIncompatibleWithNormalFunctions,
                    ),
                }
            }

            ErrorMessage::EIncompatibleReactDeepReadOnly {
                use_op,
                lower,
                upper,
                dro_loc,
            } => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleReactDeepReadOnly {
                        lower,
                        upper,
                        dro_loc,
                    },
                    use_op,
                    explanation: Some(Explanation::ExplanationIncompatibleReactDeepReadOnly),
                }
            }

            ErrorMessage::EHookUniqueIncompatible {
                use_op,
                lower,
                upper,
            } => {
                let loc = lower.loc.dupe();
                UseOp {
                    loc,
                    message: Message::MessageIncompatibleReactHooksDueToUniqueness { lower, upper },
                    use_op,
                    explanation: Some(Explanation::ExplanationReactHookIncompatibleWithEachOther),
                }
            }

            ErrorMessage::EHookRuleViolation {
                callee_loc,
                hook_rule,
                ..
            } => {
                use crate::error_message::HookRule;
                match hook_rule {
                    HookRule::ConditionalHook => {
                        Normal(Message::MessageCannotCallReactHookConditionally(callee_loc))
                    }
                    HookRule::HookHasIllegalName => {
                        Normal(Message::MessageCannotCallReactHookWithIllegalName(callee_loc))
                    }
                    HookRule::MaybeHook { hooks, non_hooks } => {
                        Normal(Message::MessageCannotCallMaybeReactHook {
                            callee_loc,
                            hooks,
                            non_hooks,
                        })
                    }
                    HookRule::NotHookSyntaxHook => {
                        Normal(Message::MessageCannotCallNonHookSyntaxHook(callee_loc))
                    }
                    HookRule::HookDefinitelyNotInComponentOrHook => {
                        Normal(Message::MessageCannotCallReactHookInDefinitelyNonComponentOrHook(
                            callee_loc,
                        ))
                    }
                    HookRule::HookNotInComponentSyntaxComponentOrHookSyntaxHook => {
                        Normal(Message::MessageCannotCallReactHookInNonComponentSyntaxComponentOrHookSyntaxHook(
                            callee_loc,
                        ))
                    }
                    HookRule::HookInUnknownContext => {
                        Normal(Message::MessageCannotCallReactHookInUnknownContext(
                            callee_loc,
                        ))
                    }
                }
            }

            ErrorMessage::EInvalidGraphQL(_, err) => Normal(Message::MessageInvalidGraphQL(err)),

            ErrorMessage::EReactIntrinsicOverlap {
                use_loc: use_reason,
                def,
                type_,
                mixed,
            } => Normal(Message::MessageReactIntrinsicOverlap {
                use_: use_reason,
                def,
                type_,
                mixed,
            }),

            ErrorMessage::EInvalidBinaryArith {
                reason_l,
                reason_r,
                kind,
                ..
            } => Normal(Message::MessageCannotPerformBinaryArith {
                kind,
                reason_l,
                reason_r,
            }),

            ErrorMessage::EMissingPlatformSupportWithAvailablePlatforms {
                available_platforms,
                required_platforms,
                ..
            } => Normal(
                Message::MessageMissingPlatformSupportWithAvailablePlatforms {
                    available_platforms: available_platforms.clone(),
                    required_platforms: required_platforms.clone(),
                },
            ),

            ErrorMessage::EMissingPlatformSupport {
                missing_platforms, ..
            } => Normal(Message::MessageMissingPlatformSupport {
                missing_platforms: missing_platforms.clone(),
            }),

            ErrorMessage::EUnionPartialOptimizationNonUniqueKey(
                box EUnionPartialOptimizationNonUniqueKeyData {
                    non_unique_keys, ..
                },
            ) => Normal(Message::MessageCannotOptimizeUnionDueToNonUniqueKeys(
                non_unique_keys,
            )),

            ErrorMessage::EUnionOptimization { kind, .. } => {
                Normal(Message::MessageCannotOptimizeUnionInternally(kind))
            }

            ErrorMessage::EUnionOptimizationOnNonUnion { arg, .. } => {
                Normal(Message::MessageInvalidUseOfFlowEnforceOptimized(arg))
            }

            ErrorMessage::EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
                sub_component,
                lower_loc,
                upper_loc,
                lower_desc,
                upper_desc,
                use_op,
                explanation,
            }) => {
                let explanation = explanation.as_ref().map(|expl| {
                    use crate::intermediate_error_types::ExplanationWithLazyParts;
                    match expl {
                        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableArray {
                            lower_array_loc,
                            upper_array_loc,
                            lower_array_desc,
                            upper_array_desc,
                            upper_array_reason,
                        } => Explanation::ExplanationInvariantSubtypingDueToMutableArray {
                            lower_array_loc: lower_array_loc.dupe(),
                            upper_array_loc: upper_array_loc.dupe(),
                            lower_array_desc: expect_type_desc(lower_array_desc.clone()),
                            upper_array_desc: expect_type_desc(upper_array_desc.clone()),
                            upper_array_reason: upper_array_reason.dupe(),
                        },
                        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
                            lower_obj_loc,
                            upper_obj_loc,
                            lower_obj_desc,
                            upper_obj_desc,
                            upper_object_reason,
                            property_name,
                        } => Explanation::ExplanationInvariantSubtypingDueToMutableProperty {
                            lower_obj_loc: lower_obj_loc.dupe(),
                            upper_obj_loc: upper_obj_loc.dupe(),
                            lower_obj_desc: expect_type_desc(lower_obj_desc.clone()),
                            upper_obj_desc: expect_type_desc(upper_obj_desc.clone()),
                            upper_object_reason: upper_object_reason.dupe(),
                            property_name: property_name.dupe(),
                        },
                        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperties {
                            lower_obj_loc,
                            upper_obj_loc,
                            lower_obj_desc,
                            upper_obj_desc,
                            upper_object_reason,
                            properties,
                        } => Explanation::ExplanationInvariantSubtypingDueToMutableProperties {
                            lower_obj_loc: lower_obj_loc.dupe(),
                            upper_obj_loc: upper_obj_loc.dupe(),
                            lower_obj_desc: expect_type_desc(lower_obj_desc.clone()),
                            upper_obj_desc: expect_type_desc(upper_obj_desc.clone()),
                            upper_object_reason: upper_object_reason.dupe(),
                            properties: properties.clone(),
                        },
                    }
                });
                IncompatibleInvariantSubtyping {
                    sub_component,
                    lower_loc,
                    upper_loc,
                    lower_desc: expect_type_desc(lower_desc),
                    upper_desc: expect_type_desc(upper_desc),
                    use_op,
                    explanation,
                }
            }

            ErrorMessage::EPropsNotFoundInInvariantSubtyping(
                box EPropsNotFoundInInvariantSubtypingData {
                    prop_names,
                    reason_lower,
                    reason_upper,
                    lower_obj_loc,
                    upper_obj_loc,
                    lower_obj_desc,
                    upper_obj_desc,
                    use_op,
                },
            ) => PropsMissingInInvariantSubtyping {
                props: Vec1::try_from_vec(
                    prop_names
                        .iter()
                        .map(|n| n.dupe().into_smol_str())
                        .collect(),
                )
                .expect("prop_names is Vec1"),
                reason_lower,
                reason_upper,
                lower_obj_loc,
                upper_obj_loc,
                lower_obj_desc: expect_type_desc(lower_obj_desc),
                upper_obj_desc: expect_type_desc(upper_obj_desc),
                use_op,
            },

            ErrorMessage::ETSSyntax { kind, .. } => {
                use crate::intermediate_error_types::Message;
                let msg = match kind {
                    TSSyntaxKind::TSUnknown => Message::MessageTSUnknownType,
                    TSSyntaxKind::TSNever => Message::MessageTSNeverType,
                    TSSyntaxKind::TSUndefined => Message::MessageTSUndefinedType,
                    TSSyntaxKind::TSKeyof => Message::MessageTSKeyofType,
                    TSSyntaxKind::TSTypeParamExtends => Message::MessageTSParamExtends,
                    TSSyntaxKind::TSReadonlyVariance => Message::MessageTSVarianceReadOnly,
                    TSSyntaxKind::TSInOutVariance(InOutVariance::In) => {
                        Message::MessageTSVarianceIn
                    }
                    TSSyntaxKind::TSInOutVariance(InOutVariance::Out) => {
                        Message::MessageTSVarianceOut
                    }
                    TSSyntaxKind::TSInOutVariance(InOutVariance::InOut) => {
                        Message::MessageTSVarianceInOut
                    }
                    TSSyntaxKind::TSSatisfiesType(enabled_casting_syntax) => {
                        Message::MessageTSSatisfiesType(enabled_casting_syntax)
                    }
                    TSSyntaxKind::TSReadonlyType(Some(arg_kind)) => match arg_kind {
                        ReadonlyTypeKind::Tuple => Message::MessageTSReadonlyOperatorOnTuple,
                        ReadonlyTypeKind::Array => Message::MessageTSReadonlyOperatorOnArray,
                    },
                    TSSyntaxKind::TSReadonlyType(None) => Message::MessageTSReadonlyType,
                    TSSyntaxKind::TSClassAccessibility(kind) => {
                        Message::MessageTSClassAccessibility(kind)
                    }
                    TSSyntaxKind::TSParameterProperty => Message::MessageTSParameterProperty,
                    TSSyntaxKind::AbstractClass => Message::MessageAbstractClass,
                    TSSyntaxKind::AbstractMethod => Message::MessageAbstractMethod,
                    TSSyntaxKind::DeprecatedTypeParamColon => {
                        Message::MessageDeprecatedTypeParamColonBound
                    }
                };
                Normal(msg)
            }
        }
    }
}

impl<L: Dupe + PartialEq + Eq + PartialOrd + Ord> ErrorMessage<L> {
    pub fn defered_in_speculation(&self) -> bool {
        match self {
            Self::EUntypedTypeImport(_, _)
            | Self::EMethodUnbinding { .. }
            | Self::EUntypedImport(_, _)
            | Self::ENonstrictImport(_)
            | Self::EUnclearType(_)
            | Self::EDeprecatedBool(_)
            | Self::EInternalType(_, _)
            | Self::EUnsafeGettersSetters(_)
            | Self::EUnsafeObjectAssign(_)
            | Self::ESketchyNullLint { .. }
            | Self::ESketchyNumberLint(_, _)
            | Self::EUnnecessaryOptionalChain(_, _)
            | Self::EUnnecessaryInvariant(_, _)
            | Self::EUnnecessaryDeclareTypeOnlyExport(_)
            | Self::EImplicitInexactObject(_)
            | Self::EAmbiguousObjectType(_)
            | Self::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck { .. })
            | Self::EUninitializedInstanceProperty(_, _)
            | Self::ETrivialRecursiveDefinition(_, _)
            | Self::EAnyValueUsedAsType { .. }
            | Self::EValueUsedAsType { .. }
            | Self::EUnusedPromise { .. }
            | Self::EImplicitInstantiationUnderconstrainedError { .. }
            | Self::EDuplicateComponentProp { .. }
            | Self::ERefComponentProp { .. }
            | Self::EConstantCondition { .. }
            | Self::EBuiltinNameLookupFailed { .. } => true,

            Self::EBindingError(BindingError::EReferencedBeforeDeclaration, _, _, _) => true,

            Self::EEnumError(EnumErrorKind::EnumNotAllChecked {
                default_case_loc: Some(_),
                ..
            }) => true,

            Self::EPropNotReadable { use_op, .. } => {
                matches!(
                    use_op,
                    VirtualUseOp::Frame(frame, _) if matches!(frame.as_ref(), VirtualFrameUseOp::ReactDeepReadOnly(_, _))
                )
            }

            _ => false,
        }
    }

    fn react_rule_of_use_op(
        use_op: &Option<VirtualUseOp<L>>,
        default: ErrorCode,
    ) -> Option<ErrorCode> {
        fn code_of_frame<L: Dupe + PartialEq + Eq + PartialOrd + Ord>(
            acc: Option<ErrorCode>,
            frame: &VirtualFrameUseOp<L>,
        ) -> Option<ErrorCode> {
            use flow_common_errors::error_codes::ErrorCode::*;
            match frame {
                VirtualFrameUseOp::ReactDeepReadOnly(_, DroType::HookReturn) => {
                    Some(ReactRuleHookMutation)
                }
                VirtualFrameUseOp::ReactDeepReadOnly(_, DroType::Props | DroType::HookArg) => {
                    Some(ReactRuleUnsafeMutation)
                }
                _ => acc,
            }
        }

        let result = use_op
            .as_ref()
            .map(|op| fold_virtual_use_op(op, |_| None, &code_of_frame));
        result.flatten().or(Some(default))
    }

    fn error_code_of_use_op(
        use_op: &Option<VirtualUseOp<L>>,
        default: ErrorCode,
    ) -> Option<ErrorCode> {
        fn code_of_root<L: Dupe + PartialEq + Eq + PartialOrd + Ord>(
            root: &VirtualRootUseOp<L>,
        ) -> Option<ErrorCode> {
            use flow_common_errors::error_codes::ErrorCode::*;
            match root {
                VirtualRootUseOp::Cast { .. }
                | VirtualRootUseOp::ClassExtendsCheck { .. }
                | VirtualRootUseOp::FunCall { .. }
                | VirtualRootUseOp::FunCallMethod { .. }
                | VirtualRootUseOp::FunReturnStatement { .. }
                | VirtualRootUseOp::FunImplicitReturn { .. } => Some(IncompatibleType),
                VirtualRootUseOp::TypeGuardIncompatibility { .. }
                | VirtualRootUseOp::PositiveTypeGuardConsistency { .. } => {
                    Some(IncompatibleTypeGuard)
                }
                _ => None,
            }
        }

        fn code_of_frame<L: Dupe + PartialEq + Eq + PartialOrd + Ord>(
            acc: Option<ErrorCode>,
            frame: &VirtualFrameUseOp<L>,
        ) -> Option<ErrorCode> {
            use flow_common_errors::error_codes::ErrorCode::*;
            match (&acc, frame) {
                (Some(_), _) => acc,
                (_, VirtualFrameUseOp::TypeArgCompatibility { .. })
                | (_, VirtualFrameUseOp::TypeParamBound { .. }) => Some(IncompatibleType),
                (None, _) => None,
            }
        }

        let result = use_op
            .as_ref()
            .map(|op| fold_virtual_use_op(op, code_of_root, &code_of_frame));
        result.flatten().or(Some(default))
    }

    fn error_code_of_upper_kind(upper_kind: &UpperKind<L>) -> Option<ErrorCode> {
        use flow_common_errors::error_codes::ErrorCode::*;
        match upper_kind {
            UpperKind::IncompatibleCallT => Some(NotAFunction),
            UpperKind::IncompatibleObjAssignFromTSpread | UpperKind::IncompatibleArrRestT => {
                Some(NotAnArray)
            }
            UpperKind::IncompatibleObjAssignFromT
            | UpperKind::IncompatibleObjRestT
            | UpperKind::IncompatibleGetKeysT
            | UpperKind::IncompatibleGetValuesT => Some(NotAnObject),
            UpperKind::IncompatibleMixinT | UpperKind::IncompatibleThisSpecializeT => {
                Some(NotAClass)
            }
            _ => Some(IncompatibleUse),
        }
    }

    /// Maps error messages to their corresponding error codes
    pub fn error_code_of_message(&self) -> Option<ErrorCode> {
        use BindingError::*;
        use flow_common_errors::error_codes::ErrorCode::*;

        match self {
            ErrorMessage::EArithmeticOperand { .. } => Some(UnsafeArith),

            // | EInvalidBinaryArith { kind = (_, op); _ } -> begin
            //     match op with
            //     | Type.ArithKind.Plus -> Some UnsafeAddition
            //     | _ -> Some UnsafeArith
            //   end
            ErrorMessage::EInvalidBinaryArith { kind, .. } => match kind.1 {
                flow_typing_type::type_::arith_kind::ArithKindInner::Plus => Some(UnsafeAddition),
                _ => Some(UnsafeArith),
            },

            ErrorMessage::EAssignConstLikeBinding { .. } => Some(CannotReassignConstLike),
            ErrorMessage::EBadExportContext(_, _) => Some(InvalidExport),
            ErrorMessage::EBadExportPosition(_) => Some(InvalidExport),
            ErrorMessage::EBadDefaultImportAccess { .. } => Some(DefaultImportAccess),
            ErrorMessage::EBadDefaultImportDestructuring(_) => Some(DefaultImportAccess),
            ErrorMessage::EInvalidImportStarUse { .. } => Some(InvalidImportStarUse),
            ErrorMessage::EBinaryInLHS { .. } => Some(InvalidInLhs),
            ErrorMessage::EBinaryInRHS { .. } => Some(InvalidInRhs),

            ErrorMessage::EBindingError(binding_error, _, _, _) => match binding_error {
                EGlobalAlreadyDeclared => Some(NameAlreadyBound),
                ENameAlreadyBound => Some(NameAlreadyBound),
                ENameAlreadyBoundInCoreJs => Some(NameAlreadyBound),
                EVarRedeclaration => Some(NameAlreadyBound),
                EReferencedBeforeDeclaration => Some(ReferenceBeforeDeclaration),
                EReferencedThisSuperBeforeSuperCall => Some(ReferenceBeforeDeclaration),
                ETypeInValuePosition { .. } => Some(TypeAsValue),
                EConstReassigned | EConstParamReassigned => Some(ReassignConst),
                EImportReassigned => Some(ReassignImport),
                EEnumReassigned => Some(ReassignEnum),
                EReservedKeyword { .. } => Some(ReservedKeyword),
            },

            ErrorMessage::EBuiltinNameLookupFailed { .. } => Some(CannotResolveName),
            ErrorMessage::EBuiltinModuleLookupFailed { .. } => Some(CannotResolveModule),
            ErrorMessage::EExpectedModuleLookupFailed { .. } => Some(CannotResolveModule),
            ErrorMessage::EPlatformSpecificImplementationModuleLookupFailed { .. } => {
                Some(CannotResolveModule)
            }
            ErrorMessage::ECallTypeArity { .. } => Some(NonpolymorphicTypeArg),
            ErrorMessage::ECannotDelete { .. } => Some(CannotDelete),
            ErrorMessage::ECannotSpreadIndexerOnRight { .. } => Some(CannotSpreadIndexer),
            ErrorMessage::ECannotSpreadInterface { .. } => Some(CannotSpreadInterface),
            ErrorMessage::ECodelessSuppression(_) => None,
            ErrorMessage::ENonStrictEqualityComparison { .. }
            | ErrorMessage::EComparison { .. } => Some(InvalidCompare),

            ErrorMessage::EComputedPropertyWithUnion { .. } => Some(InvalidComputedProp),
            ErrorMessage::EDevOnlyRefinedLocInfo { .. } => None,
            ErrorMessage::EDevOnlyInvalidatedRefinementInfo { .. } => None,
            ErrorMessage::ETemporaryHardcodedErrorForPrototyping(_, _) => None,
            ErrorMessage::EIncorrectTypeWithReplacement { kind, .. } => {
                match kind.error_type_of_kind() {
                    IncorrectTypeErrorType::DeprecatedUtility => Some(DeprecatedUtility),
                    IncorrectTypeErrorType::TSType => Some(TSSyntax),
                }
            }
            ErrorMessage::EDocblockError(_, err) => match err {
                DocblockError::MultipleFlowAttributes => Some(DuplicateFlowDecl),
                DocblockError::InvalidFlowMode(_) => Some(InvalidFlowModeDecl),
                DocblockError::MultipleJSXAttributes => Some(DuplicateJsxDecl),
                DocblockError::InvalidJSXAttribute(_) => Some(InvalidJsxDecl),
                DocblockError::MultipleJSXRuntimeAttributes => Some(DuplicateJsxRuntimeDecl),
                DocblockError::InvalidJSXRuntimeAttribute => Some(InvalidJsxRuntimeDecl),
                DocblockError::InvalidSupportsPlatform(_) => Some(InvalidSupportsPlatformDecl),
                DocblockError::DisallowedSupportsPlatform => Some(InvalidSupportsPlatformDecl),
            },
            ErrorMessage::EDuplicateModuleProvider { .. } => Some(DuplicateModule),
            ErrorMessage::EEnumError(EnumErrorKind::EnumAllMembersAlreadyChecked { .. }) => {
                Some(InvalidExhaustiveCheck)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidAbstractUse { .. }) => {
                Some(InvalidExhaustiveCheck)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberName { .. }) => {
                Some(InvalidEnumMemberName)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumNonIdentifierMemberName { .. }) => {
                Some(InvalidEnumMemberName)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidCheck { from_match, .. }) => {
                if *from_match {
                    Some(MatchInvalidPattern)
                } else {
                    Some(InvalidExhaustiveCheck)
                }
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberAccess { .. }) => {
                Some(InvalidEnumAccess)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectUtilType { .. }) => {
                Some(NotAnObject)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectFunction { .. }) => {
                Some(NotAnObject)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotIterable { .. }) => Some(NotIterable),
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberAlreadyChecked { .. }) => {
                Some(InvalidExhaustiveCheck)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberDuplicateValue { .. }) => {
                Some(DuplicateEnumInit)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberUsedAsType { .. }) => {
                Some(EnumValueAsType)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumModification { .. }) => {
                Some(CannotWriteEnum)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked {
                default_case_loc: None,
                ..
            }) => Some(InvalidExhaustiveCheck),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked {
                default_case_loc: Some(_),
                ..
            }) => Some(RequireExplicitEnumSwitchCases),
            ErrorMessage::EEnumError(EnumErrorKind::EnumUnknownNotChecked { .. }) => {
                Some(InvalidExhaustiveCheck)
            }
            ErrorMessage::EExpectedBooleanLit { use_op, .. } => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EExpectedNumberLit { use_op, .. } => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EExpectedStringLit { use_op, .. } => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EExpectedBigIntLit { use_op, .. } => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumsNotEnabled { .. }) => {
                Some(UnsupportedSyntax)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumConstNotSupported { .. }) => {
                Some(UnsupportedSyntax)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumDuplicateMemberName { .. }) => {
                Some(InvalidEnum)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInconsistentMemberValues { .. }) => {
                Some(InvalidEnum)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberInitializer { .. }) => {
                Some(InvalidEnum)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumBooleanMemberNotInitialized { .. }) => {
                Some(InvalidEnum)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumNumberMemberNotInitialized { .. }) => {
                Some(InvalidEnum)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumBigIntMemberNotInitialized { .. }) => {
                Some(InvalidEnum)
            }
            ErrorMessage::EEnumError(
                EnumErrorKind::EnumStringMemberInconsistentlyInitialized { .. },
            ) => Some(InvalidEnum),
            ErrorMessage::EExponentialSpread { .. } => Some(ExponentialSpread),
            ErrorMessage::EExportsAnnot { .. } => Some(InvalidExportsTypeArg),
            ErrorMessage::EExportValueAsType(_, _) => Some(ExportValueAsType),
            ErrorMessage::EForInRHS { .. } => Some(InvalidInRhs),
            ErrorMessage::EInstanceofRHS { .. } => Some(InvalidInRhs),
            ErrorMessage::EFunctionCallExtraArg { .. } => Some(ExtraArg),
            ErrorMessage::ETypeGuardFuncIncompatibility { .. }
            | ErrorMessage::ETypeGuardInvalidParameter { .. }
            | ErrorMessage::ETypeGuardIndexMismatch { .. }
            | ErrorMessage::ETypeGuardImpliesMismatch { .. }
            | ErrorMessage::ETypeGuardParamUnbound { .. }
            | ErrorMessage::ETypeGuardThisParam { .. }
            | ErrorMessage::ETypeGuardFunctionInvalidWrites { .. }
            | ErrorMessage::ETypeGuardFunctionParamHavoced { .. }
            | ErrorMessage::ETypeGuardIncompatibleWithFunctionKind { .. } => {
                Some(FunctionPredicate)
            }
            ErrorMessage::ENegativeTypeGuardConsistency { .. } => Some(IncompatibleTypeGuard),
            ErrorMessage::ETypeParamConstIncompatibility { .. }
            | ErrorMessage::ETypeParamConstInvalidPosition { .. } => Some(TypeParamConstCode),
            ErrorMessage::EImportTypeAsTypeof(_, _) => Some(InvalidImportType),
            ErrorMessage::EImportTypeAsValue(_, _) => Some(ImportTypeAsValue),
            ErrorMessage::EImportValueAsType(_, _) => Some(ImportValueAsType),
            ErrorMessage::EIncompatible {
                upper: (_, upper_kind),
                ..
            } => Self::error_code_of_upper_kind(upper_kind),
            ErrorMessage::EIncompatibleSpeculation(box EIncompatibleSpeculationData {
                use_op: Some(use_op),
                ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleUse),
            ErrorMessage::EIncompatibleSpeculation(..) => Some(IncompatibleUse),
            ErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData { use_op, .. }) => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EIncompatibleProp {
                use_op: Some(use_op),
                ..
            } => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EIncompatibleProp { use_op: None, .. } => Some(IncompatibleType),
            ErrorMessage::EIncompatibleWithExact(_, _, ExactnessErrorKind::UnexpectedInexact) => {
                Some(IncompatibleExact)
            }
            ErrorMessage::EIncompatibleWithExact(_, _, ExactnessErrorKind::UnexpectedIndexer) => {
                Some(IncompatibleIndexer)
            }
            ErrorMessage::EFunctionIncompatibleWithIndexer { .. } => {
                Some(IncompatibleFunctionIndexer)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible { use_op, .. })
            | ErrorMessage::EIncompatibleWithUseOp { use_op, .. } => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
                use_op,
                ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EIndeterminateModuleType { .. } => Some(ModuleTypeConflict),
            ErrorMessage::EInexactMayOverwriteIndexer { .. } => Some(CannotSpreadInexact),
            ErrorMessage::EInternal(_, _) => None,
            ErrorMessage::EInvalidConstructor { .. } => Some(InvalidConstructor),
            ErrorMessage::EInvalidLHSInAssignment { .. } => Some(InvalidLhs),
            ErrorMessage::EInvalidObjectKit { .. } => Some(NotAnObject),
            ErrorMessage::EInvalidPrototype { .. } => Some(NotAnObject),
            ErrorMessage::EInvalidReactCreateElement { .. } => Some(InvalidReactCreateElement),
            ErrorMessage::EInvalidTypeArgs(_, _) => Some(InvalidTypeArg),
            ErrorMessage::EInvalidTypeof { .. } => Some(IllegalTypeof),
            ErrorMessage::EInvalidInfer { .. } => Some(InvalidInfer),
            ErrorMessage::EConstantCondition { .. } => Some(ConstantCondition),
            ErrorMessage::EInvalidExtends { .. } => Some(InvalidExtends),
            ErrorMessage::ELintSetting { .. } => Some(LintSetting),
            ErrorMessage::EMissingLocalAnnotation { reason, .. } => match &reason.desc {
                flow_common::reason::VirtualReasonDesc::RImplicitThis(_) => Some(MissingThisAnnot),
                _ => Some(MissingLocalAnnot),
            },
            ErrorMessage::EMissingTypeArgs { .. } => Some(MissingTypeArg),
            ErrorMessage::EMixedImportAndRequire { .. } => Some(MixedImportAndRequire),
            ErrorMessage::EUnsupportedVarianceAnnotation { .. } => {
                Some(UnsupportedVarianceAnnotation)
            }
            ErrorMessage::ENoDefaultExport(_, _, _) => Some(MissingExport),
            ErrorMessage::ENoNamedExport(_, _, _, _) => Some(MissingExport),
            ErrorMessage::ENonConstVarExport { .. } => Some(NonConstVarExport),
            ErrorMessage::ENonLitArrayToTuple { .. } => Some(InvalidTupleArity),
            ErrorMessage::ENotAReactComponent { .. } => Some(NotAComponent),
            ErrorMessage::EObjectComputedPropertyAccess { .. } => Some(InvalidComputedProp),
            ErrorMessage::EObjectComputedPropertyAssign { .. } => Some(InvalidComputedProp),
            ErrorMessage::EObjectComputedPropertyPotentialOverwrite { .. } => {
                Some(InvalidComputedProp)
            }
            ErrorMessage::EOnlyDefaultExport(_, _, _) => Some(MissingExport),
            ErrorMessage::EParseError(_, _) => None,
            ErrorMessage::EPolarityMismatch { .. } => Some(IncompatibleVariance),
            ErrorMessage::EPrimitiveAsInterface { .. } => Some(IncompatibleType),
            ErrorMessage::EPrivateLookupFailed { .. } => Some(PropMissing),
            ErrorMessage::EStrUtilTypeNonLiteralArg { .. } => Some(InvalidTypeArg),
            ErrorMessage::EPropNotFoundInLookup { use_op, .. } => {
                let default = match use_op {
                    VirtualUseOp::Op(root)
                        if matches!(root.as_ref(), VirtualRootUseOp::GetExport(_)) =>
                    {
                        MissingExport
                    }
                    _ => PropMissing,
                };
                Self::react_rule_of_use_op(&Some(use_op.dupe()), default)
            }
            ErrorMessage::EPropNotFoundInSubtyping { use_op, .. }
            | ErrorMessage::EPropsNotFoundInSubtyping { use_op, .. }
            | ErrorMessage::EPropsExtraAgainstExactObject { use_op, .. } => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EPropsNotFoundInInvariantSubtyping(
                box EPropsNotFoundInInvariantSubtypingData { use_op, .. },
            ) => Self::react_rule_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EIndexerCheckFailed { use_op, .. } => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EPropNotReadable { use_op, .. } => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), CannotRead)
            }
            ErrorMessage::EPropNotWritable { use_op, .. } => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), CannotWrite)
            }
            ErrorMessage::EPropPolarityMismatch { .. } => Some(IncompatibleVariance),
            ErrorMessage::EReactElementFunArity(_, _, _) => Some(MissingArg),
            ErrorMessage::EReactRefInRender { .. } => Some(ReactRuleRef),
            ErrorMessage::ERecursionLimit(_, _) => None,
            ErrorMessage::EROArrayWrite(_, use_op) => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), CannotWrite)
            }
            ErrorMessage::ESignatureBindingValidation(
                flow_type_sig::signature_error::BindingValidation::ModuleOverride { .. },
            )
            | ErrorMessage::ESignatureBindingValidation(
                flow_type_sig::signature_error::BindingValidation::NameOverride { .. },
            ) => Some(LibdefOverride),
            ErrorMessage::ESignatureBindingValidation(
                flow_type_sig::signature_error::BindingValidation::NamespacedNameAlreadyBound {
                    ..
                },
            ) => Some(SignatureVerificationFailure),
            ErrorMessage::ESignatureVerification { .. } => Some(SignatureVerificationFailure),
            ErrorMessage::EThisInExportedFunction { .. } => Some(ThisInExportedFunction),
            ErrorMessage::EExportRenamedDefault { .. } => Some(ExportRenamedDefault),
            ErrorMessage::ETooFewTypeArgs { .. } => Some(MissingTypeArg),
            ErrorMessage::ETooManyTypeArgs { .. } => Some(ExtraTypeArg),
            ErrorMessage::ETupleArityMismatch { .. } => Some(InvalidTupleArity),
            ErrorMessage::ETupleRequiredAfterOptional { .. } => Some(TupleRequiredAfterOptional),
            ErrorMessage::ETupleInvalidTypeSpread { .. } => Some(TupleInvalidTypeSpread),
            ErrorMessage::ETupleElementAfterInexactSpread { .. } => {
                Some(ElementAfterInexactTupleSpread)
            }
            ErrorMessage::ETupleElementNotReadable { .. } => Some(CannotRead),
            ErrorMessage::ETupleElementNotWritable { .. } => Some(CannotWrite),
            ErrorMessage::ETupleElementPolarityMismatch { .. } => Some(IncompatibleVariance),
            ErrorMessage::ETupleNonIntegerIndex { .. } => Some(InvalidTupleIndex),
            ErrorMessage::ETupleOutOfBounds { .. } => Some(InvalidTupleIndex),
            ErrorMessage::ETupleUnsafeWrite { .. } => Some(InvalidTupleIndex),
            ErrorMessage::ETypeParamArity(_, _) => Some(NonpolymorphicTypeApp),
            ErrorMessage::ETypeParamMinArity(_, _) => Some(MissingTypeArg),
            ErrorMessage::EUnableToSpread { error_kind, .. } => match error_kind {
                ExactnessErrorKind::UnexpectedInexact => Some(CannotSpreadInexact),
                ExactnessErrorKind::UnexpectedIndexer => Some(CannotSpreadIndexer),
            },
            ErrorMessage::EUnexpectedThisType { .. } => Some(IllegalThis),
            ErrorMessage::EUnionSpeculationFailed(box EUnionSpeculationFailedData {
                use_op,
                ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EUnreachable { .. } => Some(UnreachableCode),
            ErrorMessage::EUnsupportedExact(_, _) => Some(InvalidExact),
            ErrorMessage::EUnsupportedImplements { .. } => Some(CannotImplement),
            ErrorMessage::EUnsupportedKeyInObject { .. } => Some(IllegalKey),
            ErrorMessage::EAmbiguousNumericKeyWithVariance { .. } => Some(IllegalKey),
            ErrorMessage::EUnsupportedSetProto { .. } => Some(CannotWrite),
            ErrorMessage::EUnsupportedSyntax(_, _) => Some(UnsupportedSyntax),
            ErrorMessage::EImplicitInstantiationUnderconstrainedError { .. } => {
                Some(UnderconstrainedImplicitInstantiation)
            }
            ErrorMessage::EObjectThisSuperReference { .. } => Some(ObjectThisReference),
            ErrorMessage::EComponentThisReference { .. } => Some(ComponentThisReference),
            ErrorMessage::EComponentCase { .. } => Some(ComponentCase),
            ErrorMessage::EDeclareComponentInvalidParam { .. } => Some(InvalidComponentProp),
            ErrorMessage::EComponentMissingReturn { .. } => Some(ComponentMissingReturn),
            ErrorMessage::EComponentMissingBody { .. } => Some(ComponentMissingBody),
            ErrorMessage::EComponentBodyInAmbientContext { .. } => {
                Some(ComponentBodyInAmbientContext)
            }
            ErrorMessage::EInvalidDeclaration { .. } => Some(InvalidDeclaration),
            ErrorMessage::EInvalidMappedType { .. } => Some(InvalidMappedType),
            ErrorMessage::EDuplicateComponentProp { .. } => Some(InvalidComponentProp),
            ErrorMessage::ERefComponentProp { .. } => Some(InvalidComponentProp),
            ErrorMessage::EKeySpreadProp { .. } => Some(InvalidSpreadProp),
            ErrorMessage::EInvalidComponentRestParam { .. } => Some(InvalidComponentProp),
            ErrorMessage::EMalformedCode { .. } | ErrorMessage::EUnusedSuppression { .. } => None,
            ErrorMessage::EUseArrayLiteral { .. } => Some(IllegalNewArray),
            ErrorMessage::EAnyValueUsedAsType { .. } | ErrorMessage::EValueUsedAsType { .. } => {
                Some(ValueAsType)
            }
            ErrorMessage::EClassToObject { .. } => Some(ClassObject),
            ErrorMessage::EMethodUnbinding { .. } => Some(MethodUnbinding),
            ErrorMessage::EHookIncompatible { .. }
            | ErrorMessage::EHookUniqueIncompatible { .. } => Some(ReactRuleHookIncompatible),
            ErrorMessage::EIncompatibleReactDeepReadOnly { .. } => {
                Some(ReactRuleImmutableIncompatible)
            }
            ErrorMessage::EHookNaming { .. } => Some(ReactRuleHookNamingConvention),
            ErrorMessage::EHookRuleViolation {
                hook_rule: HookRule::ConditionalHook,
                ..
            } => Some(ReactRuleHookConditional),
            ErrorMessage::EHookRuleViolation {
                hook_rule: HookRule::HookHasIllegalName,
                ..
            } => Some(ReactRuleHookNamingConvention),
            ErrorMessage::EHookRuleViolation {
                hook_rule: HookRule::HookDefinitelyNotInComponentOrHook,
                ..
            } => Some(ReactRuleHookDefinitelyNotInComponentOrHook),
            ErrorMessage::EHookRuleViolation {
                hook_rule: HookRule::MaybeHook { .. },
                ..
            } => Some(ReactRuleHookMixedWithNonHoook),
            ErrorMessage::EHookRuleViolation {
                hook_rule: HookRule::NotHookSyntaxHook,
                ..
            } => Some(ReactRuleHookNonHookSyntax),
            ErrorMessage::EHookRuleViolation {
                hook_rule:
                    HookRule::HookInUnknownContext
                    | HookRule::HookNotInComponentSyntaxComponentOrHookSyntaxHook,
                ..
            } => Some(ReactRuleHook),
            ErrorMessage::EInvalidGraphQL { .. } => Some(InvalidGraphQL),
            ErrorMessage::EAnnotationInference { .. } => Some(InvalidExportedAnnotation),
            ErrorMessage::ETrivialRecursiveDefinition { .. } => Some(RecursiveDefinition),
            ErrorMessage::EDefinitionCycle { .. } => Some(DefinitionCycle),
            ErrorMessage::ERecursiveDefinition { .. } => Some(RecursiveDefinition),
            ErrorMessage::EReferenceInAnnotation { .. } => Some(RecursiveDefinition),
            ErrorMessage::EReferenceInDefault { .. } => Some(RecursiveDefinition),
            ErrorMessage::EDuplicateClassMember { .. } => Some(DuplicateClassMember),
            ErrorMessage::EEmptyArrayNoProvider { .. } => Some(EmptyArrayNoAnnot),
            ErrorMessage::EBigIntRShift3 { .. } => Some(BigIntRShift3),
            ErrorMessage::EBigIntNumCoerce { .. } => Some(BigIntNumCoerce),
            ErrorMessage::EInvalidCatchParameterAnnotation { .. } => {
                Some(InvalidCatchParameterAnnotation)
            }
            ErrorMessage::EInvalidRendersTypeArgument { .. } => Some(InvalidRendersTypeArgument),
            ErrorMessage::EUnnecessaryDeclareTypeOnlyExport { .. } => {
                Some(UnnecessaryDeclareTypeOnlyExport)
            }
            ErrorMessage::EUntypedTypeImport { .. }
            | ErrorMessage::EUntypedImport { .. }
            | ErrorMessage::ENonstrictImport { .. }
            | ErrorMessage::EInternalType { .. }
            | ErrorMessage::EUnclearType { .. }
            | ErrorMessage::EDeprecatedBool { .. }
            | ErrorMessage::EUnsafeObjectAssign { .. }
            | ErrorMessage::EUnsafeGettersSetters { .. }
            | ErrorMessage::ESketchyNullLint { .. }
            | ErrorMessage::ESketchyNumberLint { .. }
            | ErrorMessage::EUnnecessaryOptionalChain { .. }
            | ErrorMessage::EUnnecessaryInvariant { .. }
            | ErrorMessage::EImplicitInexactObject { .. }
            | ErrorMessage::EAmbiguousObjectType { .. }
            | ErrorMessage::EReactIntrinsicOverlap { .. }
            | ErrorMessage::EUninitializedInstanceProperty { .. }
            | ErrorMessage::ENestedComponent { .. }
            | ErrorMessage::ENestedHook { .. }
            | ErrorMessage::EUnusedPromise { .. } => match self.kind_of_msg() {
                ErrorKind::LintError(kind) => {
                    Some(flow_common_errors::error_codes::code_of_lint(&kind))
                }
                _ => None,
            },
            ErrorMessage::ETSSyntax { .. } => Some(TSSyntax),
            ErrorMessage::EInvalidTypeCastSyntax { .. } => Some(InvalidTypeCastSyntax),
            ErrorMessage::EMissingPlatformSupportWithAvailablePlatforms { .. } => {
                Some(MissingPlatformSupport)
            }
            ErrorMessage::EMissingPlatformSupport { .. } => Some(MissingPlatformSupport),
            ErrorMessage::EUnionPartialOptimizationNonUniqueKey(..) => {
                Some(UnionPartiallyOptimizableNonUniqueKeys)
            }
            ErrorMessage::EUnionOptimization { .. } => Some(UnionUnoptimizable),
            ErrorMessage::EUnionOptimizationOnNonUnion { .. } => Some(UnionUnoptimizable),
            ErrorMessage::ECannotCallReactComponent { .. } => Some(ReactRuleCallComponent),
            ErrorMessage::EMatchError(
                MatchErrorKind::MatchNotExhaustive { .. }
                | MatchErrorKind::MatchNonExhaustiveObjectPattern { .. }
                | MatchErrorKind::MatchInvalidGuardedWildcard(_),
            ) => Some(MatchNotExhaustive),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck { .. }) => {
                Some(RequireExplicitEnumChecks)
            }
            ErrorMessage::EMatchError(MatchErrorKind::MatchUnusedPattern { .. }) => {
                Some(MatchUnusedPattern)
            }
            ErrorMessage::EMatchError(
                MatchErrorKind::MatchInvalidIdentOrMemberPattern { .. }
                | MatchErrorKind::MatchInvalidBindingKind { .. }
                | MatchErrorKind::MatchInvalidObjectPropertyLiteral { .. }
                | MatchErrorKind::MatchInvalidUnaryZero { .. }
                | MatchErrorKind::MatchInvalidUnaryPlusBigInt { .. }
                | MatchErrorKind::MatchDuplicateObjectProperty { .. }
                | MatchErrorKind::MatchBindingInOrPattern { .. }
                | MatchErrorKind::MatchInvalidAsPattern { .. }
                | MatchErrorKind::MatchInvalidPatternReference { .. }
                | MatchErrorKind::MatchInvalidObjectShorthand { .. }
                | MatchErrorKind::MatchInvalidInstancePattern(_),
            ) => Some(MatchInvalidPattern),
            ErrorMessage::EMatchError(MatchErrorKind::MatchStatementInvalidBody { .. }) => {
                Some(MatchStatementInvalidBody)
            }
            ErrorMessage::EMatchError(
                MatchErrorKind::MatchInvalidCaseSyntax { .. }
                | MatchErrorKind::MatchInvalidWildcardSyntax(_),
            ) => Some(UnsupportedSyntax),
            ErrorMessage::ERecordError(e) => match e {
                RecordErrorKind::RecordBannedTypeUtil { .. } => Some(RecordBannedTypeUtil),
                RecordErrorKind::RecordInvalidName { .. } => Some(RecordInvalidName),
                RecordErrorKind::RecordInvalidNew { .. } => Some(RecordInvalidNew),
                RecordErrorKind::RecordDeclarationInvalidSyntax { .. } => Some(RecordInvalidSyntax),
            },
            ErrorMessage::EUndocumentedFeature { .. } => Some(UndocumentedFeature),
            ErrorMessage::EIllegalAssertOperator { .. } => Some(IllegalAssertOperator),
        }
    }
}

/// Map location in SignatureError
fn map_signature_error<L, M, F>(f: &F, error: SignatureError<L>) -> SignatureError<M>
where
    F: Fn(L) -> M,
    L: Clone,
    M: Clone,
{
    use flow_type_sig::signature_error::SignatureError::*;

    match error {
        ExpectedAnnotation(loc, sort) => ExpectedAnnotation(f(loc), sort),
        UnexpectedObjectKey(loc, key_loc) => UnexpectedObjectKey(f(loc), f(key_loc)),
        UnexpectedArraySpread(loc, spread_loc) => UnexpectedArraySpread(f(loc), f(spread_loc)),
        UnexpectedArrayHole(loc) => UnexpectedArrayHole(f(loc)),
        EmptyArray(loc) => EmptyArray(f(loc)),
        EmptyObject(loc) => EmptyObject(f(loc)),
        UnexpectedExpression(loc, kind) => UnexpectedExpression(f(loc), kind),
    }
}

/// Map location in BindingValidation
fn map_binding_validation<L, M, F>(f: &F, validation: BindingValidation<L>) -> BindingValidation<M>
where
    F: Fn(L) -> M,
    L: Clone,
    M: Clone,
{
    use flow_type_sig::signature_error::BindingValidation::*;

    match validation {
        ModuleOverride {
            name,
            override_binding_loc,
            existing_binding_loc,
        } => ModuleOverride {
            name,
            override_binding_loc: f(override_binding_loc),
            existing_binding_loc: f(existing_binding_loc),
        },
        NameOverride {
            name,
            override_binding_loc,
            existing_binding_loc,
        } => NameOverride {
            name,
            override_binding_loc: f(override_binding_loc),
            existing_binding_loc: f(existing_binding_loc),
        },
        NamespacedNameAlreadyBound {
            name,
            invalid_binding_loc,
            existing_binding_loc,
        } => NamespacedNameAlreadyBound {
            name,
            invalid_binding_loc: f(invalid_binding_loc),
            existing_binding_loc: f(existing_binding_loc),
        },
    }
}
