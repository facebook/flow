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
use flow_typing_type::type_::OpaqueTypeCustomErrorCompatibilityData;
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
use crate::intermediate_error_types::ExplanationAdditionalUnionMembersData;
use crate::intermediate_error_types::ExplanationConstrainedAssignData;
use crate::intermediate_error_types::ExplanationCustomErrorData;
use crate::intermediate_error_types::ExplanationInvariantSubtypingDueToMutableArrayData;
use crate::intermediate_error_types::ExplanationInvariantSubtypingDueToMutablePropertiesData;
use crate::intermediate_error_types::ExplanationInvariantSubtypingDueToMutablePropertyData;
use crate::intermediate_error_types::ExplanationPropertyMissingDueToNeutralOptionalPropertyData;
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
use crate::intermediate_error_types::MessageAlreadyExhaustivelyCheckOneEnumMemberData;
use crate::intermediate_error_types::MessageCannotAccessEnumMemberData;
use crate::intermediate_error_types::MessageCannotAddComputedPropertyDueToPotentialOverwriteData;
use crate::intermediate_error_types::MessageCannotCallMaybeReactHookData;
use crate::intermediate_error_types::MessageCannotCompareData;
use crate::intermediate_error_types::MessageCannotExhaustivelyCheckAbstractEnumsData;
use crate::intermediate_error_types::MessageCannotExhaustivelyCheckEnumWithUnknownsData;
use crate::intermediate_error_types::MessageCannotExportRenamedDefaultData;
use crate::intermediate_error_types::MessageCannotInstantiateObjectUtilTypeWithEnumData;
use crate::intermediate_error_types::MessageCannotResolveBuiltinModuleData;
use crate::intermediate_error_types::MessageCannotSpreadGeneralData;
use crate::intermediate_error_types::MessageCannotSpreadInexactMayOverwriteIndexerData;
use crate::intermediate_error_types::MessageCannotUseEnumMemberUsedAsTypeData;
use crate::intermediate_error_types::MessageCannotUseTypeForAnnotationInferenceData;
use crate::intermediate_error_types::MessageCannotUseTypeGuardWithFunctionParamHavocedData;
use crate::intermediate_error_types::MessageCannotUseTypeInValuePositionData;
use crate::intermediate_error_types::MessageDefinitionInvalidRecursiveData;
use crate::intermediate_error_types::MessageDuplicateModuleProviderData;
use crate::intermediate_error_types::MessageEnumDuplicateMemberNameData;
use crate::intermediate_error_types::MessageEnumInvalidMemberInitializerData;
use crate::intermediate_error_types::MessageExponentialSpreadData;
use crate::intermediate_error_types::MessageIncompatibleTupleArityData;
use crate::intermediate_error_types::MessageIncompleteExhausiveCheckEnumData;
use crate::intermediate_error_types::MessageInvalidEnumMemberCheckData;
use crate::intermediate_error_types::MessageInvalidKeyPropertyInSpreadData;
use crate::intermediate_error_types::MessageInvalidRefPropertyInSpreadData;
use crate::intermediate_error_types::MessageInvalidRendersTypeArgumentData;
use crate::intermediate_error_types::MessageInvalidSelfReferencingDefaultData;
use crate::intermediate_error_types::MessageInvalidSelfReferencingTypeAnnotationData;
use crate::intermediate_error_types::MessageMatchNonExhaustiveObjectPatternData;
use crate::intermediate_error_types::MessageMatchNonExplicitEnumCheckData;
use crate::intermediate_error_types::MessageMissingPlatformSupportWithAvailablePlatformsData;
use crate::intermediate_error_types::MessageNoDefaultExportData;
use crate::intermediate_error_types::MessageNoNamedExportData;
use crate::intermediate_error_types::MessageOnlyDefaultExportData;
use crate::intermediate_error_types::MessageReactIntrinsicOverlapData;
use crate::intermediate_error_types::MessageRedeclareComponentPropData;
use crate::intermediate_error_types::MessageShouldAnnotateVariableUsedInGenericContextData;
use crate::intermediate_error_types::MessageSketchyNullCheckData;
use crate::intermediate_error_types::MessageTupleElementNotReadableData;
use crate::intermediate_error_types::MessageTupleElementNotWritableData;
use crate::intermediate_error_types::MessageTupleIndexOutOfBoundData;
use crate::intermediate_error_types::MessageTupleNonIntegerIndexData;
use crate::intermediate_error_types::MessageVariableOnlyAssignedByNullData;
use crate::intermediate_error_types::ObjKind;
use crate::intermediate_error_types::PrimitiveKind;
use crate::intermediate_error_types::RecordDeclarationInvalidSyntax;
use crate::intermediate_error_types::StrictComparisonInfo;
use crate::intermediate_error_types::SubComponentOfInvariantSubtypingError;
use crate::intermediate_error_types::UnsupportedSyntax;

/// Data struct for boxed `ErrorMessage::EIncompatibleSpeculation` variant.
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
pub struct EIncompatibleSpeculationData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub use_op: Option<VirtualUseOp<L>>,
    pub branches: Vec<ErrorMessage<L>>,
}

/// Data struct for boxed `ErrorMessage::EIncompatibleDefs` variant.
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
pub struct EIncompatibleDefsData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub branches: Vec<ErrorMessage<L>>,
}

/// Data struct for boxed `ErrorMessage::EPropsNotFoundInInvariantSubtyping` variant.
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
pub struct EUnionSpeculationFailedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason: VirtualReason<L>,
    pub op_reasons: Vec1<VirtualReason<L>>,
    pub branches: Vec<ErrorMessage<L>>,
}

/// Data struct for boxed `ErrorMessage::EInvariantSubtypingWithUseOp` variant.
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
pub struct EUnionPartialOptimizationNonUniqueKeyData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub non_unique_keys: BTreeMap<Name, BTreeMap<UnionEnum, Vec1<VirtualReason<L>>>>,
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
pub enum EnumKind {
    ConcreteEnumKind,
    AbstractEnumKind,
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
pub struct EnumInvalidMemberAccessData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub member_name: Option<Name>,
    pub suggestion: Option<FlowSmolStr>,
    pub reason: VirtualReason<L>,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumModificationData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumMemberDuplicateValueData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub prev_use_loc: L,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumInvalidObjectUtilTypeData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumInvalidObjectFunctionData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumMemberAlreadyCheckedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub case_test_loc: L,
    pub prev_check_loc: L,
    pub enum_reason: VirtualReason<L>,
    pub member_name: FlowSmolStr,
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
pub struct EnumAllMembersAlreadyCheckedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumNotAllCheckedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub enum_reason: VirtualReason<L>,
    pub left_to_check: Vec<FlowSmolStr>,
    pub default_case_loc: Option<L>,
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
pub struct EnumUnknownNotCheckedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumInvalidCheckData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
    pub example_member: Option<FlowSmolStr>,
    pub from_match: bool,
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
pub struct EnumMemberUsedAsTypeData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumIncompatibleData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub enum_kind: EnumKind,
    pub representation_type: Option<FlowSmolStr>,
    pub casting_syntax: CastingSyntax,
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
pub struct EnumInvalidAbstractUseData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumInvalidMemberNameData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
    pub member_name: String,
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
pub struct EnumNonIdentifierMemberNameData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
    pub member_name: String,
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
pub struct EnumDuplicateMemberNameData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub prev_use_loc: L,
    pub enum_reason: VirtualReason<L>,
    pub member_name: String,
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
pub struct EnumInconsistentMemberValuesData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
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
pub struct EnumInvalidMemberInitializerData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
    pub explicit_type: Option<flow_parser::ast::statement::enum_declaration::ExplicitType>,
    pub member_name: String,
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
pub struct EnumBooleanMemberNotInitializedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
    pub member_name: String,
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
pub struct EnumNumberMemberNotInitializedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
    pub member_name: String,
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
pub struct EnumBigIntMemberNotInitializedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
    pub member_name: String,
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
pub struct EnumStringMemberInconsistentlyInitializedData<
    L: Dupe + PartialOrd + Ord + PartialEq + Eq,
> {
    pub loc: L,
    pub enum_reason: VirtualReason<L>,
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
pub enum EnumErrorKind<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    EnumsNotEnabled(L),
    EnumConstNotSupported(L),
    EnumInvalidMemberAccess(Box<EnumInvalidMemberAccessData<L>>),
    EnumModification(Box<EnumModificationData<L>>),
    EnumMemberDuplicateValue(Box<EnumMemberDuplicateValueData<L>>),
    EnumInvalidObjectUtilType(Box<EnumInvalidObjectUtilTypeData<L>>),
    EnumInvalidObjectFunction(Box<EnumInvalidObjectFunctionData<L>>),
    EnumNotIterable {
        reason: VirtualReason<L>,
        for_in: bool,
    },
    EnumMemberAlreadyChecked(Box<EnumMemberAlreadyCheckedData<L>>),
    EnumAllMembersAlreadyChecked(Box<EnumAllMembersAlreadyCheckedData<L>>),
    EnumNotAllChecked(Box<EnumNotAllCheckedData<L>>),
    EnumUnknownNotChecked(Box<EnumUnknownNotCheckedData<L>>),
    EnumInvalidCheck(Box<EnumInvalidCheckData<L>>),
    EnumMemberUsedAsType(Box<EnumMemberUsedAsTypeData<L>>),
    EnumIncompatible(Box<EnumIncompatibleData<L>>),
    EnumInvalidAbstractUse(Box<EnumInvalidAbstractUseData<L>>),
    EnumInvalidMemberName(Box<EnumInvalidMemberNameData<L>>),
    EnumNonIdentifierMemberName(Box<EnumNonIdentifierMemberNameData<L>>),
    EnumDuplicateMemberName(Box<EnumDuplicateMemberNameData<L>>),
    EnumInconsistentMemberValues(Box<EnumInconsistentMemberValuesData<L>>),
    EnumInvalidMemberInitializer(Box<EnumInvalidMemberInitializerData<L>>),
    EnumBooleanMemberNotInitialized(Box<EnumBooleanMemberNotInitializedData<L>>),
    EnumNumberMemberNotInitialized(Box<EnumNumberMemberNotInitializedData<L>>),
    EnumBigIntMemberNotInitialized(Box<EnumBigIntMemberNotInitializedData<L>>),
    EnumStringMemberInconsistentlyInitialized(
        Box<EnumStringMemberInconsistentlyInitializedData<L>>,
    ),
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
pub struct MatchNotExhaustiveData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub examples: Vec<(FlowSmolStr, Vec<VirtualReason<L>>)>,
    pub missing_pattern_asts: Vec<MatchPattern<Loc, Loc>>,
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
pub struct MatchUnusedPatternData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub already_seen: Option<VirtualReason<L>>,
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
pub struct MatchNonExhaustiveObjectPatternData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub rest: Option<VirtualReason<L>>,
    pub missing_props: Vec<FlowSmolStr>,
    pub pattern_kind: MatchObjPatternKind,
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
pub struct MatchNonExplicitEnumCheckData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub wildcard_reason: VirtualReason<L>,
    pub unchecked_members: Vec<FlowSmolStr>,
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
pub struct MatchInvalidIdentOrMemberPatternData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub type_reason: VirtualReason<L>,
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
pub struct MatchDuplicateObjectPropertyData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub name: FlowSmolStr,
    pub pattern_kind: MatchObjPatternKind,
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
pub struct MatchInvalidPatternReferenceData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub binding_reason: VirtualReason<L>,
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
pub struct MatchInvalidObjectShorthandData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub name: FlowSmolStr,
    pub pattern_kind: MatchObjPatternKind,
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
pub struct MatchInvalidCaseSyntaxData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub kind: MatchInvalidCaseSyntax<L>,
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
pub enum MatchErrorKind<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    MatchNotExhaustive(Box<MatchNotExhaustiveData<L>>),
    MatchUnusedPattern(Box<MatchUnusedPatternData<L>>),
    MatchNonExhaustiveObjectPattern(Box<MatchNonExhaustiveObjectPatternData<L>>),
    MatchNonExplicitEnumCheck(Box<MatchNonExplicitEnumCheckData<L>>),
    MatchInvalidGuardedWildcard(L),
    MatchInvalidIdentOrMemberPattern(Box<MatchInvalidIdentOrMemberPatternData<L>>),
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
    MatchDuplicateObjectProperty(Box<MatchDuplicateObjectPropertyData<L>>),
    MatchBindingInOrPattern {
        loc: L,
    },
    MatchInvalidAsPattern {
        loc: L,
    },
    MatchInvalidPatternReference(Box<MatchInvalidPatternReferenceData<L>>),
    MatchInvalidObjectShorthand(Box<MatchInvalidObjectShorthandData<L>>),
    MatchStatementInvalidBody {
        loc: L,
    },
    MatchInvalidCaseSyntax(Box<MatchInvalidCaseSyntaxData<L>>),
    MatchInvalidWildcardSyntax(L),
    MatchInvalidInstancePattern(L),
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
pub struct EIncompatibleData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub lower: (VirtualReason<L>, Option<LowerKind>),
    pub upper: (VirtualReason<L>, UpperKind<L>),
    pub use_op: Option<VirtualUseOp<L>>,
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
pub struct EIncompatiblePropData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop: Option<Name>,
    pub reason_prop: VirtualReason<L>,
    pub reason_obj: VirtualReason<L>,
    pub special: Option<LowerKind>,
    pub use_op: Option<VirtualUseOp<L>>,
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
pub struct EMissingTypeArgsData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_op: VirtualReason<L>,
    pub reason_tapp: VirtualReason<L>,
    pub arity_loc: L,
    pub min_arity: i32,
    pub max_arity: i32,
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
pub struct EExpectedStringLitData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EExpectedNumberLitData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EExpectedBooleanLitData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EExpectedBigIntLitData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EPropNotFoundInLookupData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop_name: Option<Name>,
    pub reason_prop: VirtualReason<L>,
    pub reason_obj: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
    pub suggestion: Option<FlowSmolStr>,
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
pub struct EPropNotFoundInSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop_name: Option<Name>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
    pub suggestion: Option<FlowSmolStr>,
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
pub struct EPropsNotFoundInSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop_names: Vec1<Name>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EPropsExtraAgainstExactObjectData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop_names: Vec1<Name>,
    pub reason_l_obj: VirtualReason<L>,
    pub reason_r_obj: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EIndexerCheckFailedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop_name: Name,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub reason_indexer: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EPropNotReadableData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_prop: VirtualReason<L>,
    pub prop_name: Option<Name>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EPropNotWritableData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_prop: VirtualReason<L>,
    pub prop_name: Option<Name>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EPropPolarityMismatchData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub lreason: VirtualReason<L>,
    pub ureason: VirtualReason<L>,
    pub props: Vec1<(Option<Name>, (Polarity, Polarity))>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EPolarityMismatchData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub name: FlowSmolStr,
    pub expected_polarity: Polarity,
    pub actual_polarity: Polarity,
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
pub struct EBuiltinNameLookupFailedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub name: FlowSmolStr,
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
pub struct EBuiltinModuleLookupFailedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub name: FlowSmolStr,
    pub potential_generator: Option<FlowSmolStr>,
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
pub struct EExpectedModuleLookupFailedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub name: FlowSmolStr,
    pub expected_module_purpose: ExpectedModulePurpose,
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
pub struct EPlatformSpecificImplementationModuleLookupFailedData<
    L: Dupe + PartialOrd + Ord + PartialEq + Eq,
> {
    pub loc: L,
    pub name: FlowSmolStr,
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
pub struct EComparisonData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub r1: VirtualReason<L>,
    pub r2: VirtualReason<L>,
    pub loc_opt: Option<L>,
    pub strict_comparison_opt: Option<StrictComparisonInfo<L>>,
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
pub struct ETupleArityMismatchData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub lower_reason: VirtualReason<L>,
    pub lower_arity: (i32, i32),
    pub lower_inexact: bool,
    pub upper_reason: VirtualReason<L>,
    pub upper_arity: (i32, i32),
    pub upper_inexact: bool,
    pub unify: bool,
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
pub struct ETupleOutOfBoundsData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason: VirtualReason<L>,
    pub reason_op: VirtualReason<L>,
    pub inexact: bool,
    pub length: i32,
    pub index: FlowSmolStr,
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
pub struct ETupleNonIntegerIndexData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason: VirtualReason<L>,
    pub index: FlowSmolStr,
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
pub struct ETupleElementNotReadableData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub index: i32,
    pub name: Option<FlowSmolStr>,
    pub use_op: VirtualUseOp<L>,
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
pub struct ETupleElementNotWritableData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub index: i32,
    pub name: Option<FlowSmolStr>,
    pub use_op: VirtualUseOp<L>,
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
pub struct ETupleElementPolarityMismatchData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub index: i32,
    pub reason_lower: VirtualReason<L>,
    pub polarity_lower: Polarity,
    pub reason_upper: VirtualReason<L>,
    pub polarity_upper: Polarity,
    pub use_op: VirtualUseOp<L>,
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
pub struct ETupleRequiredAfterOptionalData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_tuple: VirtualReason<L>,
    pub reason_required: VirtualReason<L>,
    pub reason_optional: VirtualReason<L>,
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
pub struct ETupleInvalidTypeSpreadData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_spread: VirtualReason<L>,
    pub reason_arg: VirtualReason<L>,
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
pub struct ECallTypeArityData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub call_loc: L,
    pub is_new: bool,
    pub reason_arity: VirtualReason<L>,
    pub expected_arity: i32,
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
pub struct ETooManyTypeArgsData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_tapp: VirtualReason<L>,
    pub arity_loc: L,
    pub maximum_arity: i32,
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
pub struct ETooFewTypeArgsData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_tapp: VirtualReason<L>,
    pub arity_loc: L,
    pub minimum_arity: i32,
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
pub struct EConstantConditionData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub is_truthy: bool,
    pub show_warning: bool,
    pub constant_condition_kind: ConstantConditionKind,
    pub reason: Option<VirtualReason<L>>,
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
pub struct ETypeGuardInvalidParameterData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub type_guard_reason: VirtualReason<L>,
    pub binding_reason: VirtualReason<L>,
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
pub struct ETypeGuardFunctionInvalidWritesData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub type_guard_reason: VirtualReason<L>,
    pub write_locs: Vec<L>,
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
pub struct ETypeGuardFunctionParamHavocedData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub type_guard_reason: VirtualReason<L>,
    pub param_reason: VirtualReason<L>,
    pub call_locs: Vec<L>,
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
pub struct ETypeGuardIncompatibleWithFunctionKindData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub kind: FlowSmolStr,
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
pub struct ENegativeTypeGuardConsistencyData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub return_reason: VirtualReason<L>,
    pub type_reason: VirtualReason<L>,
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
pub struct ETypeParamConstIncompatibilityData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
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
pub struct EExportRenamedDefaultData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub name: Option<FlowSmolStr>,
    pub is_reexport: bool,
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
pub struct EInvalidObjectKitData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub reason_op: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EObjectComputedPropertyAccessData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_obj: VirtualReason<L>,
    pub reason_prop: VirtualReason<L>,
    pub kind: InvalidObjKey,
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
pub struct EObjectComputedPropertyPotentialOverwriteData<
    L: Dupe + PartialOrd + Ord + PartialEq + Eq,
> {
    pub key_loc: L,
    pub overwritten_locs: Vec<L>,
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
pub struct EIncompatibleWithUseOpData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub explanation: Option<Explanation<L>>,
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
pub struct EInvalidReactCreateElementData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub create_element_loc: L,
    pub invalid_react: VirtualReason<L>,
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
pub struct EDuplicateModuleProviderData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub module_name: FlowSmolStr,
    pub provider: L,
    pub conflict: L,
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
pub struct EIncorrectTypeWithReplacementData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub kind: IncorrectType,
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
pub struct ESketchyNullLintData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub kind: SketchyNullKind,
    pub loc: L,
    pub null_loc: L,
    pub falsy_loc: L,
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
pub struct EPrimitiveAsInterfaceData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason: VirtualReason<L>,
    pub interface_reason: VirtualReason<L>,
    pub kind: PrimitiveKind,
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
pub struct ECannotSpreadInterfaceData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub spread_reason: VirtualReason<L>,
    pub interface_reason: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct ECannotSpreadIndexerOnRightData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub spread_reason: VirtualReason<L>,
    pub object_reason: VirtualReason<L>,
    pub key_reason: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EUnableToSpreadData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub spread_reason: VirtualReason<L>,
    pub object1_reason: VirtualReason<L>,
    pub object2_reason: VirtualReason<L>,
    pub propname: Name,
    pub error_kind: ExactnessErrorKind,
    pub use_op: VirtualUseOp<L>,
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
pub struct EInexactMayOverwriteIndexerData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub spread_reason: VirtualReason<L>,
    pub key_reason: VirtualReason<L>,
    pub value_reason: VirtualReason<L>,
    pub object2_reason: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct EExponentialSpreadData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub reasons_for_operand1: ExponentialSpreadReasonGroup<L>,
    pub reasons_for_operand2: ExponentialSpreadReasonGroup<L>,
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
pub struct EAssignConstLikeBindingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub definition: VirtualReason<L>,
    pub binding_kind: AssignedConstLikeBindingType,
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
pub struct EImplicitInstantiationUnderconstrainedErrorData<
    L: Dupe + PartialOrd + Ord + PartialEq + Eq,
> {
    pub reason_call: VirtualReason<L>,
    pub reason_tparam: VirtualReason<L>,
    pub bound: FlowSmolStr,
    pub use_op: VirtualUseOp<L>,
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
pub struct EClassToObjectData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_class: VirtualReason<L>,
    pub reason_obj: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
    pub kind: ClassKind,
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
pub struct EMethodUnbindingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub reason_prop: VirtualReason<L>,
    pub reason_op: VirtualReason<L>,
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
pub struct EHookIncompatibleData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
    pub lower_is_hook: bool,
    pub hook_is_annot: bool,
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
pub struct EHookUniqueIncompatibleData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_op: VirtualUseOp<L>,
    pub lower: VirtualReason<L>,
    pub upper: VirtualReason<L>,
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
pub struct EHookRuleViolationData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub hook_rule: HookRule<L>,
    pub callee_loc: L,
    pub call_loc: L,
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
pub struct EComponentThisReferenceData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub component_loc: L,
    pub this_loc: L,
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
pub struct EInvalidDeclarationData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub declaration: VirtualReason<L>,
    pub null_write: Option<NullWrite<L>>,
    pub possible_generic_escape_locs: Vec<L>,
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
pub struct ERecursiveDefinitionData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason: VirtualReason<L>,
    pub recursion: Vec<L>,
    pub annot_locs: Vec<AnnotLoc<L>>,
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
pub struct EDuplicateClassMemberData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub name: FlowSmolStr,
    pub is_static: bool,
    pub class_kind: ClassKind,
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
pub struct ETSSyntaxData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub kind: TSSyntaxKind,
    pub loc: L,
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
pub struct EVarianceKeywordData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub kind: VarianceKeywordKind,
    pub loc: L,
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
pub enum VarianceKeywordKind {
    Writeonly,
    Plus,
    Minus,
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
pub struct EInvalidBinaryArithData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_out: VirtualReason<L>,
    pub reason_l: VirtualReason<L>,
    pub reason_r: VirtualReason<L>,
    pub kind: flow_typing_type::type_::arith_kind::ArithKind,
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
pub struct EDuplicateComponentPropData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub spread: L,
    pub duplicates: Vec1<(L, Name, L)>,
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
pub struct ERefComponentPropData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub spread: L,
    pub loc: L,
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
pub struct EKeySpreadPropData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub spread: L,
    pub loc: L,
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
pub struct EReactIntrinsicOverlapData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub use_loc: VirtualReason<L>,
    pub def: L,
    pub type_: L,
    pub mixed: bool,
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
pub struct EInvalidRendersTypeArgumentData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub renders_variant: RendersVariant,
    pub invalid_render_type_kind: InvalidRenderTypeKind<L>,
    pub invalid_type_reasons: Vec1<VirtualReason<L>>,
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
pub struct EMissingPlatformSupportWithAvailablePlatformsData<
    L: Dupe + PartialOrd + Ord + PartialEq + Eq,
> {
    pub loc: L,
    pub available_platforms: BTreeSet<FlowSmolStr>,
    pub required_platforms: BTreeSet<FlowSmolStr>,
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
pub struct EMissingPlatformSupportData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub missing_platforms: BTreeSet<FlowSmolStr>,
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
pub struct EUnionOptimizationData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub kind: OptimizedError<L>,
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
pub struct EUnionOptimizationOnNonUnionData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub arg: VirtualReason<L>,
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
pub struct EIllegalAssertOperatorData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub op: VirtualReason<L>,
    pub obj: VirtualReason<L>,
    pub specialized: bool,
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
pub struct EDevOnlyRefinedLocInfoData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub refined_loc: L,
    pub refining_locs: Vec<L>,
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
pub struct EDevOnlyInvalidatedRefinementInfoData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub read_loc: L,
    pub invalidation_info: Vec<(L, refinement_invalidation::Reason)>,
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
pub enum ErrorMessage<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    EIncompatible(Box<EIncompatibleData<L>>),

    EIncompatibleSpeculation(Box<EIncompatibleSpeculationData<L>>),

    EIncompatibleDefs(Box<EIncompatibleDefsData<L>>),

    EIncompatibleProp(Box<EIncompatiblePropData<L>>),

    EExportValueAsType(Box<(VirtualReason<L>, Name)>),

    EImportValueAsType(Box<(VirtualReason<L>, FlowSmolStr)>),

    EImportTypeAsTypeof(Box<(VirtualReason<L>, FlowSmolStr)>),

    EImportTypeAsValue(Box<(VirtualReason<L>, FlowSmolStr)>),

    ENoDefaultExport(Box<(VirtualReason<L>, Userland, Option<FlowSmolStr>)>),

    EOnlyDefaultExport(Box<(VirtualReason<L>, Userland, FlowSmolStr)>),

    ENoNamedExport(Box<(VirtualReason<L>, Userland, FlowSmolStr, Option<FlowSmolStr>)>),

    EMissingTypeArgs(Box<EMissingTypeArgsData<L>>),

    EAnyValueUsedAsType {
        reason_use: VirtualReason<L>,
    },

    EValueUsedAsType {
        reason_use: VirtualReason<L>,
    },

    EExpectedStringLit(Box<EExpectedStringLitData<L>>),

    EExpectedNumberLit(Box<EExpectedNumberLitData<L>>),

    EExpectedBooleanLit(Box<EExpectedBooleanLitData<L>>),

    EExpectedBigIntLit(Box<EExpectedBigIntLitData<L>>),

    EPropNotFoundInLookup(Box<EPropNotFoundInLookupData<L>>),

    EPropNotFoundInSubtyping(Box<EPropNotFoundInSubtypingData<L>>),

    EPropsNotFoundInSubtyping(Box<EPropsNotFoundInSubtypingData<L>>),

    EPropsNotFoundInInvariantSubtyping(Box<EPropsNotFoundInInvariantSubtypingData<L>>),

    EPropsExtraAgainstExactObject(Box<EPropsExtraAgainstExactObjectData<L>>),

    EIndexerCheckFailed(Box<EIndexerCheckFailedData<L>>),

    EPropNotReadable(Box<EPropNotReadableData<L>>),

    EPropNotWritable(Box<EPropNotWritableData<L>>),

    EPropPolarityMismatch(Box<EPropPolarityMismatchData<L>>),

    EPolarityMismatch(Box<EPolarityMismatchData<L>>),

    EBuiltinNameLookupFailed(Box<EBuiltinNameLookupFailedData<L>>),

    EBuiltinModuleLookupFailed(Box<EBuiltinModuleLookupFailedData<L>>),

    EExpectedModuleLookupFailed(Box<EExpectedModuleLookupFailedData<L>>),

    EPrivateLookupFailed(Box<((VirtualReason<L>, VirtualReason<L>), Name, VirtualUseOp<L>)>),

    EPlatformSpecificImplementationModuleLookupFailed(
        Box<EPlatformSpecificImplementationModuleLookupFailedData<L>>,
    ),

    EComparison(Box<EComparisonData<L>>),

    ENonStrictEqualityComparison(Box<(VirtualReason<L>, VirtualReason<L>)>),

    ETupleArityMismatch(Box<ETupleArityMismatchData<L>>),

    ENonLitArrayToTuple((VirtualReason<L>, VirtualReason<L>), VirtualUseOp<L>),

    ETupleOutOfBounds(Box<ETupleOutOfBoundsData<L>>),

    ETupleNonIntegerIndex(Box<ETupleNonIntegerIndexData<L>>),

    ETupleUnsafeWrite {
        reason: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    ETupleElementNotReadable(Box<ETupleElementNotReadableData<L>>),

    ETupleElementNotWritable(Box<ETupleElementNotWritableData<L>>),

    ETupleElementPolarityMismatch(Box<ETupleElementPolarityMismatchData<L>>),

    ETupleRequiredAfterOptional(Box<ETupleRequiredAfterOptionalData<L>>),

    ETupleInvalidTypeSpread(Box<ETupleInvalidTypeSpreadData<L>>),

    ETupleElementAfterInexactSpread(VirtualReason<L>),

    EROArrayWrite((VirtualReason<L>, VirtualReason<L>), VirtualUseOp<L>),

    EUnionSpeculationFailed(Box<EUnionSpeculationFailedData<L>>),

    EIncompatibleWithExact(
        (VirtualReason<L>, VirtualReason<L>),
        VirtualUseOp<L>,
        ExactnessErrorKind,
    ),

    EFunctionIncompatibleWithIndexer((VirtualReason<L>, VirtualReason<L>), VirtualUseOp<L>),

    EUnsupportedExact(Box<(VirtualReason<L>, VirtualReason<L>)>),

    EUnexpectedThisType(L),

    ETypeParamArity(L, i32),

    ECallTypeArity(Box<ECallTypeArityData<L>>),

    ETypeParamMinArity(L, i32),

    ETooManyTypeArgs(Box<ETooManyTypeArgsData<L>>),

    ETooFewTypeArgs(Box<ETooFewTypeArgsData<L>>),

    EInvalidInfer(L),

    EConstantCondition(Box<EConstantConditionData<L>>),

    EInvalidTypeArgs(Box<(VirtualReason<L>, VirtualReason<L>)>),

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

    ETypeGuardInvalidParameter(Box<ETypeGuardInvalidParameterData<L>>),

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

    ETypeGuardFunctionInvalidWrites(Box<ETypeGuardFunctionInvalidWritesData<L>>),

    ETypeGuardFunctionParamHavoced(Box<ETypeGuardFunctionParamHavocedData<L>>),

    ETypeGuardIncompatibleWithFunctionKind(Box<ETypeGuardIncompatibleWithFunctionKindData<L>>),

    ENegativeTypeGuardConsistency(Box<ENegativeTypeGuardConsistencyData<L>>),

    ETypeParamConstIncompatibility(Box<ETypeParamConstIncompatibilityData<L>>),

    ETypeParamConstInvalidPosition(VirtualReason<L>),

    EInternal(Box<(L, InternalError)>),

    EUnsupportedSyntax(Box<(L, UnsupportedSyntax)>),

    EUseArrayLiteral(L),

    EMissingLocalAnnotation {
        reason: VirtualReason<L>,
        hint_available: bool,
        from_generic_function: bool,
    },

    EBindingError(Box<(BindingError, L, Name, L)>),

    ERecursionLimit(Box<(VirtualReason<L>, VirtualReason<L>)>),

    EUninitializedInstanceProperty(L, PropertyAssignmentKind),

    EEnumError(EnumErrorKind<L>),

    EIndeterminateModuleType(L),

    EBadExportPosition(L),

    EBadExportContext(Box<(FlowSmolStr, L)>),

    EBadDefaultImportAccess(Box<(L, VirtualReason<L>)>),

    EBadDefaultImportDestructuring(L),

    EInvalidImportStarUse(Box<(L, VirtualReason<L>)>),

    ENonConstVarExport(Box<(L, Option<VirtualReason<L>>)>),

    EThisInExportedFunction(L),

    EMixedImportAndRequire(Box<(L, VirtualReason<L>)>),

    EUnsupportedVarianceAnnotation(Box<(L, FlowSmolStr)>),

    EExportRenamedDefault(Box<EExportRenamedDefaultData<L>>),

    EUnreachable(L),

    EInvalidObjectKit(Box<EInvalidObjectKitData<L>>),

    EInvalidTypeof(Box<(L, FlowSmolStr)>),

    EBinaryInLHS(VirtualReason<L>),

    EBinaryInRHS(VirtualReason<L>),

    EArithmeticOperand(VirtualReason<L>),

    EForInRHS(VirtualReason<L>),

    EInstanceofRHS(VirtualReason<L>),

    EObjectComputedPropertyAccess(Box<EObjectComputedPropertyAccessData<L>>),

    EObjectComputedPropertyAssign(Box<(VirtualReason<L>, Option<VirtualReason<L>>, InvalidObjKey)>),

    EObjectComputedPropertyPotentialOverwrite(
        Box<EObjectComputedPropertyPotentialOverwriteData<L>>,
    ),

    EInvalidLHSInAssignment(L),

    EIncompatibleWithUseOp(Box<EIncompatibleWithUseOpData<L>>),

    EInvariantSubtypingWithUseOp(Box<EInvariantSubtypingWithUseOpData<L>>),

    EUnsupportedImplements(VirtualReason<L>),

    ENotAReactComponent {
        reason: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    },

    EInvalidReactCreateElement(Box<EInvalidReactCreateElementData<L>>),

    EReactElementFunArity(Box<(VirtualReason<L>, FlowSmolStr, i32)>),

    EReactRefInRender {
        usage: VirtualReason<L>,
        kind: RefInRenderKind,
        in_hook: bool,
    },

    EFunctionCallExtraArg(Box<(VirtualReason<L>, VirtualReason<L>, i32, VirtualUseOp<L>)>),

    EUnsupportedSetProto(VirtualReason<L>),

    EDuplicateModuleProvider(Box<EDuplicateModuleProviderData<L>>),

    EParseError(Box<(L, ParseError)>),

    EDocblockError(Box<(L, DocblockError)>),

    EImplicitInexactObject(L),

    EAmbiguousObjectType(L),

    // The string is either the name of a module or "the module that exports `_`".
    EUntypedTypeImport(Box<(L, Userland)>),

    EUntypedImport(Box<(L, Userland)>),

    ENonstrictImport(L),

    EUnclearType(L),

    EDeprecatedBool(L),

    EInternalType(L, InternalType),

    EIncorrectTypeWithReplacement(Box<EIncorrectTypeWithReplacementData<L>>),

    EUnsafeGettersSetters(L),

    EUnsafeObjectAssign(L),

    EUnusedSuppression(L),

    ECodelessSuppression(L),

    EMalformedCode(L),

    ELintSetting(Box<(L, LintParseError)>),

    ESketchyNullLint(Box<ESketchyNullLintData<L>>),

    ESketchyNumberLint(SketchyNumberKind, VirtualReason<L>),

    EInvalidPrototype(Box<(L, VirtualReason<L>)>),

    EUnnecessaryOptionalChain(Box<(L, VirtualReason<L>)>),

    EUnnecessaryInvariant(Box<(L, VirtualReason<L>)>),

    EUnnecessaryDeclareTypeOnlyExport(L),

    ECannotDelete(Box<(L, VirtualReason<L>)>),

    ESignatureBindingValidation(BindingValidation<L>),

    ESignatureVerification(SignatureError<L>),

    EPrimitiveAsInterface(Box<EPrimitiveAsInterfaceData<L>>),

    ECannotSpreadInterface(Box<ECannotSpreadInterfaceData<L>>),

    ECannotSpreadIndexerOnRight(Box<ECannotSpreadIndexerOnRightData<L>>),

    EUnableToSpread(Box<EUnableToSpreadData<L>>),

    EInexactMayOverwriteIndexer(Box<EInexactMayOverwriteIndexerData<L>>),

    EExponentialSpread(Box<EExponentialSpreadData<L>>),

    EComputedPropertyWithUnion(VirtualReason<L>),

    EAssignConstLikeBinding(Box<EAssignConstLikeBindingData<L>>),

    EImplicitInstantiationUnderconstrainedError(
        Box<EImplicitInstantiationUnderconstrainedErrorData<L>>,
    ),

    EClassToObject(Box<EClassToObjectData<L>>),

    EMethodUnbinding(Box<EMethodUnbindingData<L>>),

    EHookIncompatible(Box<EHookIncompatibleData<L>>),

    EHookUniqueIncompatible(Box<EHookUniqueIncompatibleData<L>>),

    EHookRuleViolation(Box<EHookRuleViolationData<L>>),

    EHookNaming(L),

    EObjectThisSuperReference(Box<(L, VirtualReason<L>, ThisFinderKind)>),

    EComponentThisReference(Box<EComponentThisReferenceData<L>>),

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

    EInvalidDeclaration(Box<EInvalidDeclarationData<L>>),

    EInvalidGraphQL(Box<(L, GraphqlError)>),

    EAnnotationInference(Box<(L, VirtualReason<L>, VirtualReason<L>, Option<FlowSmolStr>)>),

    ETrivialRecursiveDefinition(Box<(L, VirtualReason<L>)>),

    EDefinitionCycle(Vec1<(VirtualReason<L>, Vec<L>, Vec<AnnotLoc<L>>)>),

    ERecursiveDefinition(Box<ERecursiveDefinitionData<L>>),

    EReferenceInAnnotation(Box<(L, FlowSmolStr, L)>),

    EDuplicateClassMember(Box<EDuplicateClassMemberData<L>>),

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

    ETSSyntax(Box<ETSSyntaxData<L>>),

    EVarianceKeyword(Box<EVarianceKeywordData<L>>),

    EInvalidBinaryArith(Box<EInvalidBinaryArithData<L>>),

    EInvalidMappedType {
        loc: L,
        kind: InvalidMappedTypeErrorKind,
    },

    EDuplicateComponentProp(Box<EDuplicateComponentPropData<L>>),

    ERefComponentProp(Box<ERefComponentPropData<L>>),

    EKeySpreadProp(Box<EKeySpreadPropData<L>>),

    EReactIntrinsicOverlap(Box<EReactIntrinsicOverlapData<L>>),

    EInvalidComponentRestParam(L),

    EInvalidRendersTypeArgument(Box<EInvalidRendersTypeArgumentData<L>>),

    EInvalidTypeCastSyntax {
        loc: L,
        enabled_casting_syntax: CastingSyntax,
    },

    EMissingPlatformSupportWithAvailablePlatforms(
        Box<EMissingPlatformSupportWithAvailablePlatformsData<L>>,
    ),

    EMissingPlatformSupport(Box<EMissingPlatformSupportData<L>>),

    EUnionPartialOptimizationNonUniqueKey(Box<EUnionPartialOptimizationNonUniqueKeyData<L>>),

    EUnionOptimization(Box<EUnionOptimizationData<L>>),

    EUnionOptimizationOnNonUnion(Box<EUnionOptimizationOnNonUnionData<L>>),

    ECannotCallReactComponent {
        reason: VirtualReason<L>,
    },

    EMatchError(MatchErrorKind<L>),

    ERecordError(RecordErrorKind<L>),

    EReferenceInDefault(Box<(L, FlowSmolStr, L)>),

    EUndocumentedFeature {
        loc: L,
    },

    EIllegalAssertOperator(Box<EIllegalAssertOperatorData<L>>),

    // Dev only
    EDevOnlyRefinedLocInfo(Box<EDevOnlyRefinedLocInfoData<L>>),

    EDevOnlyInvalidatedRefinementInfo(Box<EDevOnlyInvalidatedRefinementInfoData<L>>),

    // As the name suggest, don't use this for production purposes, but feel free to use it to
    // quickly test out some ideas.
    ETemporaryHardcodedErrorForPrototyping(Box<(VirtualReason<L>, FlowSmolStr)>),
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
pub struct NullWrite<L: Dupe> {
    pub null_loc: L,
    pub initialized: bool,
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
pub enum RefInRenderKind {
    Argument,
    Access,
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
    WorkerCanceled,
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
pub enum LowerKind {
    PossiblyNull,
    PossiblyVoid,
    PossiblyNullOrVoid,
    IncompatibleIntersection,
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
pub enum InvalidMappedTypeErrorKind {
    InterfaceOrDeclaredClass,
    ExtraProperties,
    ExplicitExactOrInexact,
    RemoveOptionality,
    VarianceOnArrayInput,
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
pub enum InOutVariance {
    In,
    Out,
    InOut,
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
pub enum ReadonlyTypeKind {
    Tuple,
    Array,
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
pub enum HookRule<L: Dupe> {
    ConditionalHook,
    HookHasIllegalName,
    NotHookSyntaxHook,
    MaybeHook { hooks: Vec<L>, non_hooks: Vec<L> },
    HookDefinitelyNotInComponentOrHook,
    HookInUnknownContext,
    HookNotInComponentSyntaxComponentOrHookSyntaxHook,
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
        Explanation::ExplanationConstrainedAssign(data) => {
            let ExplanationConstrainedAssignData {
                name,
                declaration,
                providers,
            } = *data;
            Explanation::ExplanationConstrainedAssign(Box::new(ExplanationConstrainedAssignData {
                name,
                declaration: f(&declaration),
                providers: providers.iter().map(f).collect(),
            }))
        }
        Explanation::ExplanationConcreteEnumCasting {
            representation_type,
            casting_syntax,
        } => Explanation::ExplanationConcreteEnumCasting {
            representation_type,
            casting_syntax,
        },
        Explanation::ExplanationCustomError(data) => {
            let ExplanationCustomErrorData {
                name,
                custom_error_loc,
            } = *data;
            Explanation::ExplanationCustomError(Box::new(ExplanationCustomErrorData {
                name,
                custom_error_loc: f(&custom_error_loc),
            }))
        }
        Explanation::ExplanationFunctionsWithStaticsToObject => {
            Explanation::ExplanationFunctionsWithStaticsToObject
        }
        Explanation::ExplanationInvariantSubtypingDueToMutableArray(data) => {
            let ExplanationInvariantSubtypingDueToMutableArrayData {
                lower_array_loc,
                upper_array_loc,
                lower_array_desc,
                upper_array_desc,
                upper_array_reason,
            } = *data;
            Explanation::ExplanationInvariantSubtypingDueToMutableArray(Box::new(
                ExplanationInvariantSubtypingDueToMutableArrayData {
                    lower_array_loc: f(&lower_array_loc),
                    upper_array_loc: f(&upper_array_loc),
                    lower_array_desc: lower_array_desc.map_err(map_desc),
                    upper_array_desc: upper_array_desc.map_err(map_desc),
                    upper_array_reason: map_reason(upper_array_reason),
                },
            ))
        }
        Explanation::ExplanationInvariantSubtypingDueToMutableProperty(data) => {
            let ExplanationInvariantSubtypingDueToMutablePropertyData {
                lower_obj_loc,
                upper_obj_loc,
                lower_obj_desc,
                upper_obj_desc,
                upper_object_reason,
                property_name,
            } = *data;
            Explanation::ExplanationInvariantSubtypingDueToMutableProperty(Box::new(
                ExplanationInvariantSubtypingDueToMutablePropertyData {
                    lower_obj_loc: f(&lower_obj_loc),
                    upper_obj_loc: f(&upper_obj_loc),
                    lower_obj_desc: lower_obj_desc.map_err(map_desc),
                    upper_obj_desc: upper_obj_desc.map_err(map_desc),
                    upper_object_reason: map_reason(upper_object_reason),
                    property_name,
                },
            ))
        }
        Explanation::ExplanationInvariantSubtypingDueToMutableProperties(data) => {
            let ExplanationInvariantSubtypingDueToMutablePropertiesData {
                lower_obj_loc,
                upper_obj_loc,
                lower_obj_desc,
                upper_obj_desc,
                upper_object_reason,
                properties,
            } = *data;
            Explanation::ExplanationInvariantSubtypingDueToMutableProperties(Box::new(
                ExplanationInvariantSubtypingDueToMutablePropertiesData {
                    lower_obj_loc: f(&lower_obj_loc),
                    upper_obj_loc: f(&upper_obj_loc),
                    lower_obj_desc: lower_obj_desc.map_err(map_desc),
                    upper_obj_desc: upper_obj_desc.map_err(map_desc),
                    upper_object_reason: map_reason(upper_object_reason),
                    properties,
                },
            ))
        }
        Explanation::ExplanationMultiplatform => Explanation::ExplanationMultiplatform,
        Explanation::ExplanationPropertyInvariantTyping => {
            Explanation::ExplanationPropertyInvariantTyping
        }
        Explanation::ExplanationPropertyMissingDueToNeutralOptionalProperty(data) => {
            let ExplanationPropertyMissingDueToNeutralOptionalPropertyData {
                props_plural,
                lower_obj_loc,
                upper_obj_loc,
                lower_obj_desc,
                upper_obj_desc,
                upper_object_reason,
            } = *data;
            Explanation::ExplanationPropertyMissingDueToNeutralOptionalProperty(Box::new(
                ExplanationPropertyMissingDueToNeutralOptionalPropertyData {
                    props_plural,
                    lower_obj_loc: f(&lower_obj_loc),
                    upper_obj_loc: f(&upper_obj_loc),
                    lower_obj_desc: lower_obj_desc.map_err(map_desc),
                    upper_obj_desc: upper_obj_desc.map_err(map_desc),
                    upper_object_reason: map_reason(upper_object_reason),
                },
            ))
        }
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
        Explanation::ExplanationAdditionalUnionMembers(data) => {
            let ExplanationAdditionalUnionMembersData {
                left,
                right,
                members,
                extra_number,
            } = *data;
            Explanation::ExplanationAdditionalUnionMembers(Box::new(
                ExplanationAdditionalUnionMembersData {
                    left: map_reason(left),
                    right: map_reason(right),
                    members,
                    extra_number,
                },
            ))
        }
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
            EIncompatible(box EIncompatibleData {
                use_op,
                lower: (lreason, lkind),
                upper: (ureason, ukind),
            }) => EIncompatible(Box::new(EIncompatibleData {
                use_op: use_op.map(map_use_op),
                lower: (map_reason(lreason), lkind),
                upper: (map_reason(ureason), map_upper_kind(ukind)),
            })),

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

            EIncompatibleProp(box EIncompatiblePropData {
                use_op,
                prop,
                reason_prop,
                reason_obj,
                special,
            }) => EIncompatibleProp(Box::new(EIncompatiblePropData {
                use_op: use_op.map(map_use_op),
                prop,
                reason_prop: map_reason(reason_prop),
                reason_obj: map_reason(reason_obj),
                special,
            })),

            EExpectedStringLit(box EExpectedStringLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => EExpectedStringLit(Box::new(EExpectedStringLitData {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            })),

            EExpectedNumberLit(box EExpectedNumberLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => EExpectedNumberLit(Box::new(EExpectedNumberLitData {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            })),

            EExpectedBooleanLit(box EExpectedBooleanLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => EExpectedBooleanLit(Box::new(EExpectedBooleanLitData {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            })),

            EExpectedBigIntLit(box EExpectedBigIntLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => EExpectedBigIntLit(Box::new(EExpectedBigIntLitData {
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            })),

            EPropNotFoundInLookup(box EPropNotFoundInLookupData {
                prop_name,
                reason_prop,
                reason_obj,
                use_op,
                suggestion,
            }) => EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                prop_name,
                reason_prop: map_reason(reason_prop),
                reason_obj: map_reason(reason_obj),
                use_op: map_use_op(use_op),
                suggestion,
            })),

            EPropNotFoundInSubtyping(box EPropNotFoundInSubtypingData {
                prop_name,
                reason_lower,
                reason_upper,
                use_op,
                suggestion,
            }) => EPropNotFoundInSubtyping(Box::new(EPropNotFoundInSubtypingData {
                prop_name,
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
                suggestion,
            })),

            EPropsNotFoundInSubtyping(box EPropsNotFoundInSubtypingData {
                prop_names,
                reason_lower,
                reason_upper,
                use_op,
            }) => EPropsNotFoundInSubtyping(Box::new(EPropsNotFoundInSubtypingData {
                prop_names,
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                use_op: map_use_op(use_op),
            })),

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

            EPropsExtraAgainstExactObject(box EPropsExtraAgainstExactObjectData {
                prop_names,
                reason_l_obj,
                reason_r_obj,
                use_op,
            }) => EPropsExtraAgainstExactObject(Box::new(EPropsExtraAgainstExactObjectData {
                prop_names,
                reason_l_obj: map_reason(reason_l_obj),
                reason_r_obj: map_reason(reason_r_obj),
                use_op: map_use_op(use_op),
            })),

            EIndexerCheckFailed(box EIndexerCheckFailedData {
                prop_name,
                reason_lower,
                reason_upper,
                reason_indexer,
                use_op,
            }) => EIndexerCheckFailed(Box::new(EIndexerCheckFailedData {
                prop_name,
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                reason_indexer: map_reason(reason_indexer),
                use_op: map_use_op(use_op),
            })),

            EPropNotReadable(box EPropNotReadableData {
                reason_prop,
                prop_name,
                use_op,
            }) => EPropNotReadable(Box::new(EPropNotReadableData {
                reason_prop: map_reason(reason_prop),
                prop_name,
                use_op: map_use_op(use_op),
            })),

            EPropNotWritable(box EPropNotWritableData {
                reason_prop,
                prop_name,
                use_op,
            }) => EPropNotWritable(Box::new(EPropNotWritableData {
                reason_prop: map_reason(reason_prop),
                prop_name,
                use_op: map_use_op(use_op),
            })),

            EPropPolarityMismatch(box EPropPolarityMismatchData {
                lreason,
                ureason,
                props,
                use_op,
            }) => EPropPolarityMismatch(Box::new(EPropPolarityMismatchData {
                lreason: map_reason(lreason),
                ureason: map_reason(ureason),
                props,
                use_op: map_use_op(use_op),
            })),

            EBuiltinNameLookupFailed(box EBuiltinNameLookupFailedData { loc, name }) => {
                EBuiltinNameLookupFailed(Box::new(EBuiltinNameLookupFailedData {
                    loc: f(loc),
                    name,
                }))
            }

            EBuiltinModuleLookupFailed(box EBuiltinModuleLookupFailedData {
                loc,
                name,
                potential_generator,
            }) => EBuiltinModuleLookupFailed(Box::new(EBuiltinModuleLookupFailedData {
                loc: f(loc),
                name,
                potential_generator,
            })),

            EExpectedModuleLookupFailed(box EExpectedModuleLookupFailedData {
                loc,
                name,
                expected_module_purpose,
            }) => EExpectedModuleLookupFailed(Box::new(EExpectedModuleLookupFailedData {
                loc: f(loc),
                name,
                expected_module_purpose,
            })),

            EPrivateLookupFailed(box ((r1, r2), x, op)) => EPrivateLookupFailed(Box::new((
                (map_reason(r1), map_reason(r2)),
                x,
                map_use_op(op),
            ))),

            EPlatformSpecificImplementationModuleLookupFailed(
                box EPlatformSpecificImplementationModuleLookupFailedData { loc, name },
            ) => EPlatformSpecificImplementationModuleLookupFailed(Box::new(
                EPlatformSpecificImplementationModuleLookupFailedData { loc: f(loc), name },
            )),

            ETupleArityMismatch(box ETupleArityMismatchData {
                use_op,
                lower_reason,
                lower_arity,
                lower_inexact,
                upper_reason,
                upper_arity,
                upper_inexact,
                unify,
            }) => ETupleArityMismatch(Box::new(ETupleArityMismatchData {
                use_op: map_use_op(use_op),
                lower_reason: map_reason(lower_reason),
                lower_arity,
                lower_inexact,
                upper_reason: map_reason(upper_reason),
                upper_arity,
                upper_inexact,
                unify,
            })),

            ENonLitArrayToTuple((r1, r2), op) => {
                ENonLitArrayToTuple((map_reason(r1), map_reason(r2)), map_use_op(op))
            }

            ETupleOutOfBounds(box ETupleOutOfBoundsData {
                use_op,
                reason,
                reason_op,
                inexact,
                length,
                index,
            }) => ETupleOutOfBounds(Box::new(ETupleOutOfBoundsData {
                use_op: map_use_op(use_op),
                reason: map_reason(reason),
                reason_op: map_reason(reason_op),
                inexact,
                length,
                index,
            })),

            ETupleNonIntegerIndex(box ETupleNonIntegerIndexData {
                use_op,
                reason,
                index,
            }) => ETupleNonIntegerIndex(Box::new(ETupleNonIntegerIndexData {
                use_op: map_use_op(use_op),
                reason: map_reason(reason),
                index,
            })),

            ETupleUnsafeWrite { reason, use_op } => ETupleUnsafeWrite {
                reason: map_reason(reason),
                use_op: map_use_op(use_op),
            },

            ETupleElementNotReadable(box ETupleElementNotReadableData {
                reason,
                index,
                name,
                use_op,
            }) => ETupleElementNotReadable(Box::new(ETupleElementNotReadableData {
                reason: map_reason(reason),
                index,
                name,
                use_op: map_use_op(use_op),
            })),

            ETupleElementNotWritable(box ETupleElementNotWritableData {
                reason,
                index,
                name,
                use_op,
            }) => ETupleElementNotWritable(Box::new(ETupleElementNotWritableData {
                reason: map_reason(reason),
                index,
                name,
                use_op: map_use_op(use_op),
            })),

            ETupleElementPolarityMismatch(box ETupleElementPolarityMismatchData {
                index,
                reason_lower,
                polarity_lower,
                reason_upper,
                polarity_upper,
                use_op,
            }) => ETupleElementPolarityMismatch(Box::new(ETupleElementPolarityMismatchData {
                index,
                reason_lower: map_reason(reason_lower),
                polarity_lower,
                reason_upper: map_reason(reason_upper),
                polarity_upper,
                use_op: map_use_op(use_op),
            })),

            ETupleRequiredAfterOptional(box ETupleRequiredAfterOptionalData {
                reason_tuple,
                reason_required,
                reason_optional,
            }) => ETupleRequiredAfterOptional(Box::new(ETupleRequiredAfterOptionalData {
                reason_tuple: map_reason(reason_tuple),
                reason_required: map_reason(reason_required),
                reason_optional: map_reason(reason_optional),
            })),

            ETupleInvalidTypeSpread(box ETupleInvalidTypeSpreadData {
                reason_spread,
                reason_arg,
            }) => ETupleInvalidTypeSpread(Box::new(ETupleInvalidTypeSpreadData {
                reason_spread: map_reason(reason_spread),
                reason_arg: map_reason(reason_arg),
            })),

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

            EInvalidObjectKit(box EInvalidObjectKitData {
                reason,
                reason_op,
                use_op,
            }) => EInvalidObjectKit(Box::new(EInvalidObjectKitData {
                reason: map_reason(reason),
                reason_op: map_reason(reason_op),
                use_op: map_use_op(use_op),
            })),

            EIncompatibleWithUseOp(box EIncompatibleWithUseOpData {
                use_op,
                reason_lower,
                reason_upper,
                explanation,
            }) => EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                use_op: map_use_op(use_op),
                reason_lower: map_reason(reason_lower),
                reason_upper: map_reason(reason_upper),
                explanation: explanation.map(|e| map_loc_of_explanation(&|l: &L| f(l.dupe()), e)),
            })),

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

            EInvalidReactCreateElement(box EInvalidReactCreateElementData {
                create_element_loc,
                invalid_react,
            }) => EInvalidReactCreateElement(Box::new(EInvalidReactCreateElementData {
                create_element_loc: f(create_element_loc),
                invalid_react: map_reason(invalid_react),
            })),

            EFunctionCallExtraArg(box (rl, ru, n, op)) => EFunctionCallExtraArg(Box::new((
                map_reason(rl),
                map_reason(ru),
                n,
                map_use_op(op),
            ))),

            EExportValueAsType(box (r, s)) => EExportValueAsType(Box::new((map_reason(r), s))),
            EImportValueAsType(box (r, s)) => EImportValueAsType(Box::new((map_reason(r), s))),
            EImportTypeAsTypeof(box (r, s)) => EImportTypeAsTypeof(Box::new((map_reason(r), s))),
            EImportTypeAsValue(box (r, s)) => EImportTypeAsValue(Box::new((map_reason(r), s))),

            ENoDefaultExport(box (r, s1, s2)) => {
                ENoDefaultExport(Box::new((map_reason(r), s1, s2)))
            }
            EOnlyDefaultExport(box (r, s1, s2)) => {
                EOnlyDefaultExport(Box::new((map_reason(r), s1, s2)))
            }
            ENoNamedExport(box (r, s1, s2, s3)) => {
                ENoNamedExport(Box::new((map_reason(r), s1, s2, s3)))
            }

            EMissingTypeArgs(box EMissingTypeArgsData {
                reason_op,
                reason_tapp,
                arity_loc,
                min_arity,
                max_arity,
            }) => EMissingTypeArgs(Box::new(EMissingTypeArgsData {
                reason_op: map_reason(reason_op),
                reason_tapp: map_reason(reason_tapp),
                arity_loc: f(arity_loc),
                min_arity,
                max_arity,
            })),

            EAnyValueUsedAsType { reason_use } => EAnyValueUsedAsType {
                reason_use: map_reason(reason_use),
            },

            EValueUsedAsType { reason_use } => EValueUsedAsType {
                reason_use: map_reason(reason_use),
            },

            EPolarityMismatch(box EPolarityMismatchData {
                reason,
                name,
                expected_polarity,
                actual_polarity,
            }) => EPolarityMismatch(Box::new(EPolarityMismatchData {
                reason: map_reason(reason),
                name,
                expected_polarity,
                actual_polarity,
            })),

            EComparison(box EComparisonData {
                r1,
                r2,
                loc_opt,
                strict_comparison_opt,
            }) => EComparison(Box::new(EComparisonData {
                r1: map_reason(r1),
                r2: map_reason(r2),
                loc_opt: loc_opt.map(&f),
                strict_comparison_opt: strict_comparison_opt.map(|sc| StrictComparisonInfo {
                    left_precise_reason: map_reason(sc.left_precise_reason),
                    right_precise_reason: map_reason(sc.right_precise_reason),
                    strict_comparison_kind: sc.strict_comparison_kind,
                }),
            })),

            ENonStrictEqualityComparison(box (r1, r2)) => {
                ENonStrictEqualityComparison(Box::new((map_reason(r1), map_reason(r2))))
            }

            EUnsupportedExact(box (r1, r2)) => {
                EUnsupportedExact(Box::new((map_reason(r1), map_reason(r2))))
            }

            EUnexpectedThisType(loc) => EUnexpectedThisType(f(loc)),

            ETypeParamArity(loc, i) => ETypeParamArity(f(loc), i),

            ECallTypeArity(box ECallTypeArityData {
                call_loc,
                is_new,
                reason_arity,
                expected_arity,
            }) => ECallTypeArity(Box::new(ECallTypeArityData {
                call_loc: f(call_loc),
                is_new,
                reason_arity: map_reason(reason_arity),
                expected_arity,
            })),

            ETypeParamMinArity(loc, i) => ETypeParamMinArity(f(loc), i),

            ETooManyTypeArgs(box ETooManyTypeArgsData {
                reason_tapp,
                arity_loc,
                maximum_arity,
            }) => ETooManyTypeArgs(Box::new(ETooManyTypeArgsData {
                reason_tapp: map_reason(reason_tapp),
                arity_loc: f(arity_loc),
                maximum_arity,
            })),

            ETooFewTypeArgs(box ETooFewTypeArgsData {
                reason_tapp,
                arity_loc,
                minimum_arity,
            }) => ETooFewTypeArgs(Box::new(ETooFewTypeArgsData {
                reason_tapp: map_reason(reason_tapp),
                arity_loc: f(arity_loc),
                minimum_arity,
            })),

            EInvalidTypeArgs(box (r1, r2)) => {
                EInvalidTypeArgs(Box::new((map_reason(r1), map_reason(r2))))
            }

            EInvalidInfer(l) => EInvalidInfer(f(l)),

            EConstantCondition(box EConstantConditionData {
                loc,
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
            }) => EConstantCondition(Box::new(EConstantConditionData {
                loc: f(loc),
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason: reason.map(map_reason),
            })),

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

            ETypeGuardInvalidParameter(box ETypeGuardInvalidParameterData {
                type_guard_reason,
                binding_reason,
            }) => ETypeGuardInvalidParameter(Box::new(ETypeGuardInvalidParameterData {
                type_guard_reason: map_reason(type_guard_reason),
                binding_reason: map_reason(binding_reason),
            })),

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

            ETypeGuardFunctionInvalidWrites(box ETypeGuardFunctionInvalidWritesData {
                reason,
                type_guard_reason,
                write_locs,
            }) => ETypeGuardFunctionInvalidWrites(Box::new(ETypeGuardFunctionInvalidWritesData {
                reason: map_reason(reason),
                type_guard_reason: map_reason(type_guard_reason),
                write_locs: write_locs.into_iter().map(&f).collect(),
            })),

            ETypeGuardFunctionParamHavoced(box ETypeGuardFunctionParamHavocedData {
                type_guard_reason,
                param_reason,
                call_locs,
            }) => ETypeGuardFunctionParamHavoced(Box::new(ETypeGuardFunctionParamHavocedData {
                type_guard_reason: map_reason(type_guard_reason),
                param_reason: map_reason(param_reason),
                call_locs: call_locs.into_iter().map(&f).collect(),
            })),

            ETypeGuardIncompatibleWithFunctionKind(
                box ETypeGuardIncompatibleWithFunctionKindData { loc, kind },
            ) => ETypeGuardIncompatibleWithFunctionKind(Box::new(
                ETypeGuardIncompatibleWithFunctionKindData { loc: f(loc), kind },
            )),

            ENegativeTypeGuardConsistency(box ENegativeTypeGuardConsistencyData {
                reason,
                return_reason,
                type_reason,
            }) => ENegativeTypeGuardConsistency(Box::new(ENegativeTypeGuardConsistencyData {
                reason: map_reason(reason),
                return_reason: map_reason(return_reason),
                type_reason: map_reason(type_reason),
            })),

            ETypeParamConstIncompatibility(box ETypeParamConstIncompatibilityData {
                use_op,
                lower,
                upper,
            }) => ETypeParamConstIncompatibility(Box::new(ETypeParamConstIncompatibilityData {
                use_op: map_use_op(use_op),
                lower: map_reason(lower),
                upper: map_reason(upper),
            })),

            ETypeParamConstInvalidPosition(reason) => {
                ETypeParamConstInvalidPosition(map_reason(reason))
            }

            EInternal(box (loc, i)) => EInternal(Box::new((f(loc), i))),
            EUnsupportedSyntax(box (loc, u)) => EUnsupportedSyntax(Box::new((f(loc), u))),
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

            EBindingError(box (b, loc, s, scope)) => {
                EBindingError(Box::new((b, f(loc), s, f(scope))))
            }

            ERecursionLimit(box (r1, r2)) => {
                ERecursionLimit(Box::new((map_reason(r1), map_reason(r2))))
            }

            EUninitializedInstanceProperty(loc, e) => EUninitializedInstanceProperty(f(loc), e),

            EEnumError(enum_error) => {
                use EnumErrorKind::*;
                EEnumError(match enum_error {
                    EnumsNotEnabled(loc) => EnumsNotEnabled(f(loc)),
                    EnumConstNotSupported(loc) => EnumConstNotSupported(f(loc)),
                    EnumInvalidMemberAccess(box EnumInvalidMemberAccessData {
                        member_name,
                        suggestion,
                        reason,
                        enum_reason,
                    }) => EnumInvalidMemberAccess(Box::new(EnumInvalidMemberAccessData {
                        member_name,
                        suggestion,
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    })),
                    EnumModification(box EnumModificationData { loc, enum_reason }) => {
                        EnumModification(Box::new(EnumModificationData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                        }))
                    }
                    EnumMemberDuplicateValue(box EnumMemberDuplicateValueData {
                        loc,
                        prev_use_loc,
                        enum_reason,
                    }) => EnumMemberDuplicateValue(Box::new(EnumMemberDuplicateValueData {
                        loc: f(loc),
                        prev_use_loc: f(prev_use_loc),
                        enum_reason: map_reason(enum_reason),
                    })),
                    EnumInvalidObjectUtilType(box EnumInvalidObjectUtilTypeData {
                        reason,
                        enum_reason,
                    }) => EnumInvalidObjectUtilType(Box::new(EnumInvalidObjectUtilTypeData {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    })),
                    EnumInvalidObjectFunction(box EnumInvalidObjectFunctionData {
                        reason,
                        enum_reason,
                    }) => EnumInvalidObjectFunction(Box::new(EnumInvalidObjectFunctionData {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    })),
                    EnumNotIterable { reason, for_in } => EnumNotIterable {
                        reason: map_reason(reason),
                        for_in,
                    },
                    EnumMemberAlreadyChecked(box EnumMemberAlreadyCheckedData {
                        case_test_loc,
                        prev_check_loc,
                        enum_reason,
                        member_name,
                    }) => EnumMemberAlreadyChecked(Box::new(EnumMemberAlreadyCheckedData {
                        case_test_loc: f(case_test_loc),
                        prev_check_loc: f(prev_check_loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    })),
                    EnumAllMembersAlreadyChecked(box EnumAllMembersAlreadyCheckedData {
                        loc,
                        enum_reason,
                    }) => {
                        EnumAllMembersAlreadyChecked(Box::new(EnumAllMembersAlreadyCheckedData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                        }))
                    }
                    EnumNotAllChecked(box EnumNotAllCheckedData {
                        reason,
                        enum_reason,
                        left_to_check,
                        default_case_loc,
                    }) => EnumNotAllChecked(Box::new(EnumNotAllCheckedData {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                        left_to_check,
                        default_case_loc: default_case_loc.map(&f),
                    })),
                    EnumUnknownNotChecked(box EnumUnknownNotCheckedData {
                        reason,
                        enum_reason,
                    }) => EnumUnknownNotChecked(Box::new(EnumUnknownNotCheckedData {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    })),
                    EnumInvalidCheck(box EnumInvalidCheckData {
                        loc,
                        enum_reason,
                        example_member,
                        from_match,
                    }) => EnumInvalidCheck(Box::new(EnumInvalidCheckData {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        example_member,
                        from_match,
                    })),
                    EnumMemberUsedAsType(box EnumMemberUsedAsTypeData {
                        reason,
                        enum_reason,
                    }) => EnumMemberUsedAsType(Box::new(EnumMemberUsedAsTypeData {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    })),
                    EnumIncompatible(box EnumIncompatibleData {
                        use_op,
                        reason_lower,
                        reason_upper,
                        enum_kind,
                        representation_type,
                        casting_syntax,
                    }) => EnumIncompatible(Box::new(EnumIncompatibleData {
                        use_op: map_use_op(use_op),
                        reason_lower: map_reason(reason_lower),
                        reason_upper: map_reason(reason_upper),
                        enum_kind,
                        representation_type,
                        casting_syntax,
                    })),
                    EnumInvalidAbstractUse(box EnumInvalidAbstractUseData {
                        reason,
                        enum_reason,
                    }) => EnumInvalidAbstractUse(Box::new(EnumInvalidAbstractUseData {
                        reason: map_reason(reason),
                        enum_reason: map_reason(enum_reason),
                    })),
                    EnumInvalidMemberName(box EnumInvalidMemberNameData {
                        loc,
                        enum_reason,
                        member_name,
                    }) => EnumInvalidMemberName(Box::new(EnumInvalidMemberNameData {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    })),
                    EnumNonIdentifierMemberName(box EnumNonIdentifierMemberNameData {
                        loc,
                        enum_reason,
                        member_name,
                    }) => EnumNonIdentifierMemberName(Box::new(EnumNonIdentifierMemberNameData {
                        loc: f(loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    })),
                    EnumDuplicateMemberName(box EnumDuplicateMemberNameData {
                        loc,
                        prev_use_loc,
                        enum_reason,
                        member_name,
                    }) => EnumDuplicateMemberName(Box::new(EnumDuplicateMemberNameData {
                        loc: f(loc),
                        prev_use_loc: f(prev_use_loc),
                        enum_reason: map_reason(enum_reason),
                        member_name,
                    })),
                    EnumInconsistentMemberValues(box EnumInconsistentMemberValuesData {
                        loc,
                        enum_reason,
                    }) => {
                        EnumInconsistentMemberValues(Box::new(EnumInconsistentMemberValuesData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                        }))
                    }
                    EnumInvalidMemberInitializer(box EnumInvalidMemberInitializerData {
                        loc,
                        enum_reason,
                        explicit_type,
                        member_name,
                    }) => {
                        EnumInvalidMemberInitializer(Box::new(EnumInvalidMemberInitializerData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                            explicit_type,
                            member_name,
                        }))
                    }
                    EnumBooleanMemberNotInitialized(box EnumBooleanMemberNotInitializedData {
                        loc,
                        enum_reason,
                        member_name,
                    }) => EnumBooleanMemberNotInitialized(Box::new(
                        EnumBooleanMemberNotInitializedData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                            member_name,
                        },
                    )),
                    EnumNumberMemberNotInitialized(box EnumNumberMemberNotInitializedData {
                        loc,
                        enum_reason,
                        member_name,
                    }) => EnumNumberMemberNotInitialized(Box::new(
                        EnumNumberMemberNotInitializedData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                            member_name,
                        },
                    )),
                    EnumBigIntMemberNotInitialized(box EnumBigIntMemberNotInitializedData {
                        loc,
                        enum_reason,
                        member_name,
                    }) => EnumBigIntMemberNotInitialized(Box::new(
                        EnumBigIntMemberNotInitializedData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                            member_name,
                        },
                    )),
                    EnumStringMemberInconsistentlyInitialized(
                        box EnumStringMemberInconsistentlyInitializedData { loc, enum_reason },
                    ) => EnumStringMemberInconsistentlyInitialized(Box::new(
                        EnumStringMemberInconsistentlyInitializedData {
                            loc: f(loc),
                            enum_reason: map_reason(enum_reason),
                        },
                    )),
                })
            }
            EIndeterminateModuleType(loc) => EIndeterminateModuleType(f(loc)),
            EBadExportPosition(loc) => EBadExportPosition(f(loc)),
            EBadExportContext(box (s, loc)) => EBadExportContext(Box::new((s, f(loc)))),

            EBadDefaultImportAccess(box (loc, r)) => {
                EBadDefaultImportAccess(Box::new((f(loc), map_reason(r))))
            }
            EBadDefaultImportDestructuring(loc) => EBadDefaultImportDestructuring(f(loc)),
            EInvalidImportStarUse(box (loc, r)) => {
                EInvalidImportStarUse(Box::new((f(loc), map_reason(r))))
            }
            ENonConstVarExport(box (loc, r)) => {
                ENonConstVarExport(Box::new((f(loc), r.map(map_reason))))
            }
            EThisInExportedFunction(loc) => EThisInExportedFunction(f(loc)),
            EMixedImportAndRequire(box (loc, r)) => {
                EMixedImportAndRequire(Box::new((f(loc), map_reason(r))))
            }
            EUnsupportedVarianceAnnotation(box (loc, k)) => {
                EUnsupportedVarianceAnnotation(Box::new((f(loc), k)))
            }

            EExportRenamedDefault(box EExportRenamedDefaultData {
                loc,
                name,
                is_reexport,
            }) => EExportRenamedDefault(Box::new(EExportRenamedDefaultData {
                loc: f(loc),
                name,
                is_reexport,
            })),

            EUnreachable(loc) => EUnreachable(f(loc)),
            EInvalidTypeof(box (loc, s)) => EInvalidTypeof(Box::new((f(loc), s))),
            EBinaryInLHS(r) => EBinaryInLHS(map_reason(r)),
            EBinaryInRHS(r) => EBinaryInRHS(map_reason(r)),
            EArithmeticOperand(r) => EArithmeticOperand(map_reason(r)),
            EForInRHS(r) => EForInRHS(map_reason(r)),
            EInstanceofRHS(r) => EInstanceofRHS(map_reason(r)),

            EObjectComputedPropertyAccess(box EObjectComputedPropertyAccessData {
                reason_obj,
                reason_prop,
                kind,
            }) => EObjectComputedPropertyAccess(Box::new(EObjectComputedPropertyAccessData {
                reason_obj: map_reason(reason_obj),
                reason_prop: map_reason(reason_prop),
                kind,
            })),

            EObjectComputedPropertyAssign(box (r1, r2, kind)) => {
                EObjectComputedPropertyAssign(Box::new((map_reason(r1), r2.map(map_reason), kind)))
            }

            EObjectComputedPropertyPotentialOverwrite(
                box EObjectComputedPropertyPotentialOverwriteData {
                    key_loc,
                    overwritten_locs,
                },
            ) => EObjectComputedPropertyPotentialOverwrite(Box::new(
                EObjectComputedPropertyPotentialOverwriteData {
                    key_loc: f(key_loc),
                    overwritten_locs: overwritten_locs.into_iter().map(&f).collect(),
                },
            )),

            EInvalidLHSInAssignment(l) => EInvalidLHSInAssignment(f(l)),
            EUnsupportedImplements(r) => EUnsupportedImplements(map_reason(r)),

            EReactElementFunArity(box (r, s, i)) => {
                EReactElementFunArity(Box::new((map_reason(r), s, i)))
            }

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

            EDuplicateModuleProvider(box EDuplicateModuleProviderData {
                module_name,
                provider,
                conflict,
            }) => EDuplicateModuleProvider(Box::new(EDuplicateModuleProviderData {
                module_name,
                provider: f(provider),
                conflict: f(conflict),
            })),

            EParseError(box (loc, p)) => EParseError(Box::new((f(loc), p))),
            EDocblockError(box (loc, e)) => EDocblockError(Box::new((f(loc), e))),
            EImplicitInexactObject(loc) => EImplicitInexactObject(f(loc)),
            EAmbiguousObjectType(loc) => EAmbiguousObjectType(f(loc)),
            EUntypedTypeImport(box (loc, s)) => EUntypedTypeImport(Box::new((f(loc), s))),
            EUntypedImport(box (loc, s)) => EUntypedImport(Box::new((f(loc), s))),
            ENonstrictImport(loc) => ENonstrictImport(f(loc)),
            EUnclearType(loc) => EUnclearType(f(loc)),
            EDeprecatedBool(loc) => EDeprecatedBool(f(loc)),
            EInternalType(loc, kind) => EInternalType(f(loc), kind),

            EIncorrectTypeWithReplacement(box EIncorrectTypeWithReplacementData { loc, kind }) => {
                EIncorrectTypeWithReplacement(Box::new(EIncorrectTypeWithReplacementData {
                    loc: f(loc),
                    kind,
                }))
            }

            EUnsafeGettersSetters(loc) => EUnsafeGettersSetters(f(loc)),
            EUnsafeObjectAssign(loc) => EUnsafeObjectAssign(f(loc)),
            EUnusedSuppression(loc) => EUnusedSuppression(f(loc)),
            ECodelessSuppression(loc) => ECodelessSuppression(f(loc)),
            ELintSetting(box (loc, err)) => ELintSetting(Box::new((f(loc), err))),

            ESketchyNullLint(box ESketchyNullLintData {
                kind,
                loc,
                null_loc,
                falsy_loc,
            }) => ESketchyNullLint(Box::new(ESketchyNullLintData {
                kind,
                loc: f(loc),
                null_loc: f(null_loc),
                falsy_loc: f(falsy_loc),
            })),

            ESketchyNumberLint(kind, r) => ESketchyNumberLint(kind, map_reason(r)),
            EInvalidPrototype(box (loc, r)) => EInvalidPrototype(Box::new((f(loc), map_reason(r)))),
            EUnnecessaryOptionalChain(box (loc, r)) => {
                EUnnecessaryOptionalChain(Box::new((f(loc), map_reason(r))))
            }
            EUnnecessaryInvariant(box (loc, r)) => {
                EUnnecessaryInvariant(Box::new((f(loc), map_reason(r))))
            }
            EUnnecessaryDeclareTypeOnlyExport(loc) => EUnnecessaryDeclareTypeOnlyExport(f(loc)),
            ECannotDelete(box (l1, r1)) => ECannotDelete(Box::new((f(l1), map_reason(r1)))),

            ESignatureBindingValidation(sve) => {
                ESignatureBindingValidation(map_binding_validation(&f, sve))
            }

            ESignatureVerification(sve) => ESignatureVerification(map_signature_error(&f, sve)),

            EPrimitiveAsInterface(box EPrimitiveAsInterfaceData {
                use_op,
                reason,
                interface_reason,
                kind,
            }) => EPrimitiveAsInterface(Box::new(EPrimitiveAsInterfaceData {
                use_op: map_use_op(use_op),
                reason: map_reason(reason),
                interface_reason: map_reason(interface_reason),
                kind,
            })),

            ECannotSpreadInterface(box ECannotSpreadInterfaceData {
                spread_reason,
                interface_reason,
                use_op,
            }) => ECannotSpreadInterface(Box::new(ECannotSpreadInterfaceData {
                spread_reason: map_reason(spread_reason),
                interface_reason: map_reason(interface_reason),
                use_op: map_use_op(use_op),
            })),

            ECannotSpreadIndexerOnRight(box ECannotSpreadIndexerOnRightData {
                spread_reason,
                object_reason,
                key_reason,
                use_op,
            }) => ECannotSpreadIndexerOnRight(Box::new(ECannotSpreadIndexerOnRightData {
                spread_reason: map_reason(spread_reason),
                object_reason: map_reason(object_reason),
                key_reason: map_reason(key_reason),
                use_op: map_use_op(use_op),
            })),

            EUnableToSpread(box EUnableToSpreadData {
                spread_reason,
                object1_reason,
                object2_reason,
                propname,
                error_kind,
                use_op,
            }) => EUnableToSpread(Box::new(EUnableToSpreadData {
                spread_reason: map_reason(spread_reason),
                object1_reason: map_reason(object1_reason),
                object2_reason: map_reason(object2_reason),
                propname,
                error_kind,
                use_op: map_use_op(use_op),
            })),

            EInexactMayOverwriteIndexer(box EInexactMayOverwriteIndexerData {
                spread_reason,
                key_reason,
                value_reason,
                object2_reason,
                use_op,
            }) => EInexactMayOverwriteIndexer(Box::new(EInexactMayOverwriteIndexerData {
                spread_reason: map_reason(spread_reason),
                key_reason: map_reason(key_reason),
                value_reason: map_reason(value_reason),
                object2_reason: map_reason(object2_reason),
                use_op: map_use_op(use_op),
            })),

            EExponentialSpread(box EExponentialSpreadData {
                reason,
                reasons_for_operand1,
                reasons_for_operand2,
            }) => EExponentialSpread(Box::new(EExponentialSpreadData {
                reason: map_reason(reason),
                reasons_for_operand1: map_loc_of_exponential_spread_reason_group(
                    map_reason,
                    reasons_for_operand1,
                ),
                reasons_for_operand2: map_loc_of_exponential_spread_reason_group(
                    map_reason,
                    reasons_for_operand2,
                ),
            })),

            EComputedPropertyWithUnion(reason) => EComputedPropertyWithUnion(map_reason(reason)),

            EAssignConstLikeBinding(box EAssignConstLikeBindingData {
                loc,
                definition,
                binding_kind,
            }) => EAssignConstLikeBinding(Box::new(EAssignConstLikeBindingData {
                loc: f(loc),
                definition: map_reason(definition),
                binding_kind,
            })),

            EMalformedCode(loc) => EMalformedCode(f(loc)),

            EImplicitInstantiationUnderconstrainedError(
                box EImplicitInstantiationUnderconstrainedErrorData {
                    reason_call,
                    reason_tparam,
                    bound,
                    use_op,
                },
            ) => EImplicitInstantiationUnderconstrainedError(Box::new(
                EImplicitInstantiationUnderconstrainedErrorData {
                    reason_call: map_reason(reason_call),
                    reason_tparam: map_reason(reason_tparam),
                    bound,
                    use_op: map_use_op(use_op),
                },
            )),

            EClassToObject(box EClassToObjectData {
                reason_class,
                reason_obj,
                use_op,
                kind,
            }) => EClassToObject(Box::new(EClassToObjectData {
                reason_class: map_reason(reason_class),
                reason_obj: map_reason(reason_obj),
                use_op: map_use_op(use_op),
                kind,
            })),

            EMethodUnbinding(box EMethodUnbindingData {
                use_op,
                reason_op,
                reason_prop,
            }) => EMethodUnbinding(Box::new(EMethodUnbindingData {
                use_op: map_use_op(use_op),
                reason_op: map_reason(reason_op),
                reason_prop: map_reason(reason_prop),
            })),

            EHookIncompatible(box EHookIncompatibleData {
                use_op,
                lower,
                upper,
                lower_is_hook,
                hook_is_annot,
            }) => EHookIncompatible(Box::new(EHookIncompatibleData {
                use_op: map_use_op(use_op),
                lower: map_reason(lower),
                upper: map_reason(upper),
                lower_is_hook,
                hook_is_annot,
            })),

            EHookUniqueIncompatible(box EHookUniqueIncompatibleData {
                use_op,
                lower,
                upper,
            }) => EHookUniqueIncompatible(Box::new(EHookUniqueIncompatibleData {
                use_op: map_use_op(use_op),
                lower: map_reason(lower),
                upper: map_reason(upper),
            })),

            EHookRuleViolation(box EHookRuleViolationData {
                callee_loc,
                call_loc,
                hook_rule,
            }) => {
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
                EHookRuleViolation(Box::new(EHookRuleViolationData {
                    callee_loc: f(callee_loc),
                    call_loc: f(call_loc),
                    hook_rule,
                }))
            }

            EHookNaming(l) => EHookNaming(f(l)),

            EObjectThisSuperReference(box (loc, r, k)) => {
                EObjectThisSuperReference(Box::new((f(loc), map_reason(r), k)))
            }

            EComponentThisReference(box EComponentThisReferenceData {
                component_loc,
                this_loc,
            }) => EComponentThisReference(Box::new(EComponentThisReferenceData {
                component_loc: f(component_loc),
                this_loc: f(this_loc),
            })),

            EComponentCase(loc) => EComponentCase(f(loc)),
            EDeclareComponentInvalidParam { loc, kind } => {
                EDeclareComponentInvalidParam { loc: f(loc), kind }
            }
            EComponentMissingReturn(r) => EComponentMissingReturn(map_reason(r)),
            EComponentMissingBody(loc) => EComponentMissingBody(f(loc)),
            EComponentBodyInAmbientContext(loc) => EComponentBodyInAmbientContext(f(loc)),
            ENestedComponent(r) => ENestedComponent(map_reason(r)),
            ENestedHook(r) => ENestedHook(map_reason(r)),

            EInvalidDeclaration(box EInvalidDeclarationData {
                declaration,
                null_write,
                possible_generic_escape_locs,
            }) => EInvalidDeclaration(Box::new(EInvalidDeclarationData {
                declaration: map_reason(declaration),
                null_write: null_write.map(|nw| NullWrite {
                    null_loc: f(nw.null_loc),
                    initialized: nw.initialized,
                }),
                possible_generic_escape_locs: possible_generic_escape_locs
                    .into_iter()
                    .map(&f)
                    .collect(),
            })),

            EInvalidGraphQL(box (loc, err)) => EInvalidGraphQL(Box::new((f(loc), err))),

            EAnnotationInference(box (loc, r1, r2, suggestion)) => EAnnotationInference(Box::new(
                (f(loc), map_reason(r1), map_reason(r2), suggestion),
            )),

            ETrivialRecursiveDefinition(box (loc, r)) => {
                ETrivialRecursiveDefinition(Box::new((f(loc), map_reason(r))))
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

            ERecursiveDefinition(box ERecursiveDefinitionData {
                reason,
                recursion,
                annot_locs,
            }) => ERecursiveDefinition(Box::new(ERecursiveDefinitionData {
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
            })),

            EReferenceInAnnotation(box (bind_loc, name, loc)) => {
                EReferenceInAnnotation(Box::new((f(bind_loc), name, f(loc))))
            }
            EReferenceInDefault(box (bind_loc, name, loc)) => {
                EReferenceInDefault(Box::new((f(bind_loc), name, f(loc))))
            }

            EDuplicateClassMember(box EDuplicateClassMemberData {
                loc,
                name,
                is_static,
                class_kind,
            }) => EDuplicateClassMember(Box::new(EDuplicateClassMemberData {
                loc: f(loc),
                name,
                is_static,
                class_kind,
            })),

            EEmptyArrayNoProvider { loc } => EEmptyArrayNoProvider { loc: f(loc) },
            EUnusedPromise { loc, async_ } => EUnusedPromise {
                loc: f(loc),
                async_,
            },

            EReactIntrinsicOverlap(box EReactIntrinsicOverlapData {
                use_loc: use_reason,
                def,
                type_,
                mixed,
            }) => EReactIntrinsicOverlap(Box::new(EReactIntrinsicOverlapData {
                use_loc: map_reason(use_reason),
                def: f(def),
                type_: f(type_),
                mixed,
            })),

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

            ETSSyntax(box ETSSyntaxData { kind, loc }) => {
                ETSSyntax(Box::new(ETSSyntaxData { kind, loc: f(loc) }))
            }

            EVarianceKeyword(box EVarianceKeywordData { kind, loc }) => {
                EVarianceKeyword(Box::new(EVarianceKeywordData { kind, loc: f(loc) }))
            }

            EInvalidBinaryArith(box EInvalidBinaryArithData {
                reason_out,
                reason_l,
                reason_r,
                kind,
            }) => EInvalidBinaryArith(Box::new(EInvalidBinaryArithData {
                reason_out: map_reason(reason_out),
                reason_l: map_reason(reason_l),
                reason_r: map_reason(reason_r),
                kind,
            })),

            EInvalidMappedType { loc, kind } => EInvalidMappedType { loc: f(loc), kind },

            EDuplicateComponentProp(box EDuplicateComponentPropData { spread, duplicates }) => {
                EDuplicateComponentProp(Box::new(EDuplicateComponentPropData {
                    spread: f(spread),
                    duplicates: Vec1::try_from_vec(
                        duplicates
                            .into_iter()
                            .map(|(first, name, second)| (f(first), name, f(second)))
                            .collect(),
                    )
                    .unwrap(),
                }))
            }

            ERefComponentProp(box ERefComponentPropData { spread, loc }) => {
                ERefComponentProp(Box::new(ERefComponentPropData {
                    spread: f(spread),
                    loc: f(loc),
                }))
            }

            EKeySpreadProp(box EKeySpreadPropData { spread, loc }) => {
                EKeySpreadProp(Box::new(EKeySpreadPropData {
                    spread: f(spread),
                    loc: f(loc),
                }))
            }

            EInvalidRendersTypeArgument(box EInvalidRendersTypeArgumentData {
                loc,
                renders_variant,
                invalid_render_type_kind,
                invalid_type_reasons,
            }) => EInvalidRendersTypeArgument(Box::new(EInvalidRendersTypeArgumentData {
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
            })),

            EInvalidTypeCastSyntax {
                loc,
                enabled_casting_syntax,
            } => EInvalidTypeCastSyntax {
                loc: f(loc),
                enabled_casting_syntax,
            },

            EMissingPlatformSupportWithAvailablePlatforms(
                box EMissingPlatformSupportWithAvailablePlatformsData {
                    loc,
                    available_platforms,
                    required_platforms,
                },
            ) => EMissingPlatformSupportWithAvailablePlatforms(Box::new(
                EMissingPlatformSupportWithAvailablePlatformsData {
                    loc: f(loc),
                    available_platforms,
                    required_platforms,
                },
            )),

            EMissingPlatformSupport(box EMissingPlatformSupportData {
                loc,
                missing_platforms,
            }) => EMissingPlatformSupport(Box::new(EMissingPlatformSupportData {
                loc: f(loc),
                missing_platforms,
            })),

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

            EUnionOptimization(box EUnionOptimizationData { loc, kind }) => {
                let kind = match kind {
                    OptimizedError::ContainsUnresolved(r) => {
                        OptimizedError::ContainsUnresolved(map_reason(r))
                    }
                    OptimizedError::NoCandidateMembers => OptimizedError::NoCandidateMembers,
                    OptimizedError::NoCommonKeys => OptimizedError::NoCommonKeys,
                };
                EUnionOptimization(Box::new(EUnionOptimizationData { loc: f(loc), kind }))
            }

            EUnionOptimizationOnNonUnion(box EUnionOptimizationOnNonUnionData { loc, arg }) => {
                EUnionOptimizationOnNonUnion(Box::new(EUnionOptimizationOnNonUnionData {
                    loc: f(loc),
                    arg: map_reason(arg),
                }))
            }

            ECannotCallReactComponent { reason } => ECannotCallReactComponent {
                reason: map_reason(reason),
            },

            EDevOnlyRefinedLocInfo(box EDevOnlyRefinedLocInfoData {
                refined_loc,
                refining_locs,
            }) => EDevOnlyRefinedLocInfo(Box::new(EDevOnlyRefinedLocInfoData {
                refined_loc: f(refined_loc),
                refining_locs: refining_locs.into_iter().map(&f).collect(),
            })),

            EMatchError(match_error) => {
                use MatchErrorKind::*;
                EMatchError(match match_error {
                    MatchNotExhaustive(box MatchNotExhaustiveData {
                        loc,
                        examples,
                        missing_pattern_asts,
                    }) => MatchNotExhaustive(Box::new(MatchNotExhaustiveData {
                        loc: f(loc),
                        examples: examples
                            .into_iter()
                            .map(|(pattern, reasons)| {
                                (pattern, reasons.into_iter().map(&map_reason).collect())
                            })
                            .collect(),
                        missing_pattern_asts,
                    })),
                    MatchUnusedPattern(box MatchUnusedPatternData {
                        reason,
                        already_seen,
                    }) => MatchUnusedPattern(Box::new(MatchUnusedPatternData {
                        reason: map_reason(reason),
                        already_seen: already_seen.map(map_reason),
                    })),
                    MatchNonExhaustiveObjectPattern(box MatchNonExhaustiveObjectPatternData {
                        loc,
                        rest,
                        missing_props,
                        pattern_kind,
                    }) => MatchNonExhaustiveObjectPattern(Box::new(
                        MatchNonExhaustiveObjectPatternData {
                            loc: f(loc),
                            rest: rest.map(map_reason),
                            missing_props,
                            pattern_kind,
                        },
                    )),
                    MatchNonExplicitEnumCheck(box MatchNonExplicitEnumCheckData {
                        loc,
                        wildcard_reason,
                        unchecked_members,
                    }) => MatchNonExplicitEnumCheck(Box::new(MatchNonExplicitEnumCheckData {
                        loc: f(loc),
                        wildcard_reason: map_reason(wildcard_reason),
                        unchecked_members,
                    })),
                    MatchInvalidGuardedWildcard(loc) => MatchInvalidGuardedWildcard(f(loc)),
                    MatchInvalidIdentOrMemberPattern(
                        box MatchInvalidIdentOrMemberPatternData { loc, type_reason },
                    ) => MatchInvalidIdentOrMemberPattern(Box::new(
                        MatchInvalidIdentOrMemberPatternData {
                            loc: f(loc),
                            type_reason: map_reason(type_reason),
                        },
                    )),
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
                    MatchDuplicateObjectProperty(box MatchDuplicateObjectPropertyData {
                        loc,
                        name,
                        pattern_kind,
                    }) => {
                        MatchDuplicateObjectProperty(Box::new(MatchDuplicateObjectPropertyData {
                            loc: f(loc),
                            name,
                            pattern_kind,
                        }))
                    }
                    MatchBindingInOrPattern { loc } => MatchBindingInOrPattern { loc: f(loc) },
                    MatchInvalidAsPattern { loc } => MatchInvalidAsPattern { loc: f(loc) },
                    MatchInvalidPatternReference(box MatchInvalidPatternReferenceData {
                        loc,
                        binding_reason,
                    }) => {
                        MatchInvalidPatternReference(Box::new(MatchInvalidPatternReferenceData {
                            loc: f(loc),
                            binding_reason: map_reason(binding_reason),
                        }))
                    }
                    MatchInvalidObjectShorthand(box MatchInvalidObjectShorthandData {
                        loc,
                        name,
                        pattern_kind,
                    }) => MatchInvalidObjectShorthand(Box::new(MatchInvalidObjectShorthandData {
                        loc: f(loc),
                        name,
                        pattern_kind,
                    })),
                    MatchStatementInvalidBody { loc } => MatchStatementInvalidBody { loc: f(loc) },
                    MatchInvalidCaseSyntax(box MatchInvalidCaseSyntaxData { loc, kind }) => {
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
                        MatchInvalidCaseSyntax(Box::new(MatchInvalidCaseSyntaxData {
                            loc: f(loc),
                            kind,
                        }))
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

            EIllegalAssertOperator(box EIllegalAssertOperatorData {
                op,
                obj,
                specialized,
            }) => EIllegalAssertOperator(Box::new(EIllegalAssertOperatorData {
                op: map_reason(op),
                obj: map_reason(obj),
                specialized,
            })),

            EDevOnlyInvalidatedRefinementInfo(box EDevOnlyInvalidatedRefinementInfoData {
                read_loc,
                invalidation_info,
            }) => {
                EDevOnlyInvalidatedRefinementInfo(Box::new(EDevOnlyInvalidatedRefinementInfoData {
                    read_loc: f(read_loc),
                    invalidation_info: invalidation_info
                        .into_iter()
                        .map(|(l, r)| (f(l), r))
                        .collect(),
                }))
            }

            ETemporaryHardcodedErrorForPrototyping(box (r, s)) => {
                ETemporaryHardcodedErrorForPrototyping(Box::new((map_reason(r), s)))
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
                        VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility(..)
                    ) =>
                {
                    let (lower, upper, lower_t, upper_t, name, custom_error_loc) =
                        if let VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility(
                            box OpaqueTypeCustomErrorCompatibilityData {
                                lower,
                                upper,
                                lower_t,
                                upper_t,
                                name,
                                custom_error_loc,
                            },
                        ) = frame_rc.as_ref()
                        {
                            (lower, upper, lower_t, upper_t, name, custom_error_loc)
                        } else {
                            unreachable!()
                        };
                    VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility(
                            Box::new(OpaqueTypeCustomErrorCompatibilityData {
                                lower: lower.dupe(),
                                upper: upper.dupe(),
                                lower_t: f(lower_t.clone()),
                                upper_t: f(upper_t.clone()),
                                name: name.dupe(),
                                custom_error_loc: custom_error_loc.dupe(),
                            }),
                        )),
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

            EIncompatibleWithUseOp(box EIncompatibleWithUseOpData {
                use_op,
                reason_lower,
                reason_upper,
                explanation,
            }) => EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                use_op: map_use_op(&f, use_op),
                reason_lower,
                reason_upper,
                explanation,
            })),

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
        ErrorMessage::EIncompatible(box EIncompatibleData { use_op, .. }) => {
            use_op.as_ref().map_or_else(|| nope, util)
        }
        ErrorMessage::EIncompatibleSpeculation(box EIncompatibleSpeculationData {
            use_op, ..
        }) => use_op.as_ref().map_or_else(|| nope, util),
        ErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData { use_op, .. }) => util(use_op),
        ErrorMessage::EIncompatibleProp(box EIncompatiblePropData { use_op, .. }) => {
            use_op.as_ref().map_or_else(|| nope, util)
        }
        ErrorMessage::EExpectedStringLit(box EExpectedStringLitData { use_op, .. }) => util(use_op),
        ErrorMessage::EExpectedNumberLit(box EExpectedNumberLitData { use_op, .. }) => util(use_op),
        ErrorMessage::EExpectedBooleanLit(box EExpectedBooleanLitData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::EExpectedBigIntLit(box EExpectedBigIntLitData { use_op, .. }) => util(use_op),
        ErrorMessage::EPropNotFoundInLookup(box EPropNotFoundInLookupData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::EPropNotFoundInSubtyping(box EPropNotFoundInSubtypingData {
            use_op, ..
        }) => util(use_op),
        ErrorMessage::EPropsNotFoundInSubtyping(box EPropsNotFoundInSubtypingData {
            use_op,
            ..
        }) => util(use_op),
        ErrorMessage::EPropsNotFoundInInvariantSubtyping(
            box EPropsNotFoundInInvariantSubtypingData { use_op, .. },
        ) => util(use_op),
        ErrorMessage::EPropsExtraAgainstExactObject(box EPropsExtraAgainstExactObjectData {
            use_op,
            ..
        }) => util(use_op),
        ErrorMessage::EIndexerCheckFailed(box EIndexerCheckFailedData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::EPropNotReadable(box EPropNotReadableData { use_op, .. }) => util(use_op),
        ErrorMessage::EPropNotWritable(box EPropNotWritableData { use_op, .. }) => util(use_op),
        ErrorMessage::EPropPolarityMismatch(box EPropPolarityMismatchData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::EPrivateLookupFailed(box (_, _, use_op)) => util(use_op),
        ErrorMessage::ETupleArityMismatch(box ETupleArityMismatchData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::ENonLitArrayToTuple(_, use_op) => util(use_op),
        ErrorMessage::ETupleOutOfBounds(box ETupleOutOfBoundsData { use_op, .. }) => util(use_op),
        ErrorMessage::ETupleNonIntegerIndex(box ETupleNonIntegerIndexData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::ETupleUnsafeWrite { use_op, .. } => util(use_op),
        ErrorMessage::ETupleElementNotReadable(box ETupleElementNotReadableData {
            use_op, ..
        }) => util(use_op),
        ErrorMessage::ETupleElementNotWritable(box ETupleElementNotWritableData {
            use_op, ..
        }) => util(use_op),
        ErrorMessage::ETupleElementPolarityMismatch(box ETupleElementPolarityMismatchData {
            use_op,
            ..
        }) => util(use_op),
        ErrorMessage::EROArrayWrite(_, use_op) => util(use_op),
        ErrorMessage::EUnionSpeculationFailed(box EUnionSpeculationFailedData {
            use_op, ..
        }) => util(use_op),
        ErrorMessage::EIncompatibleWithExact(_, use_op, _) => util(use_op),
        ErrorMessage::EFunctionIncompatibleWithIndexer(_, use_op) => util(use_op),
        ErrorMessage::EInvalidObjectKit(box EInvalidObjectKitData { use_op, .. }) => util(use_op),
        ErrorMessage::EIncompatibleWithUseOp(box EIncompatibleWithUseOpData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
            use_op,
            ..
        }) => util(use_op),
        ErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible(box EnumIncompatibleData {
            use_op,
            ..
        })) => util(use_op),
        ErrorMessage::ENotAReactComponent { use_op, .. } => util(use_op),
        ErrorMessage::EFunctionCallExtraArg(box (_, _, _, use_op)) => util(use_op),
        ErrorMessage::EPrimitiveAsInterface(box EPrimitiveAsInterfaceData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::ECannotSpreadInterface(box ECannotSpreadInterfaceData { use_op, .. }) => {
            util(use_op)
        }
        ErrorMessage::ECannotSpreadIndexerOnRight(box ECannotSpreadIndexerOnRightData {
            use_op,
            ..
        }) => util(use_op),
        ErrorMessage::EUnableToSpread(box EUnableToSpreadData { use_op, .. }) => util(use_op),
        ErrorMessage::EInexactMayOverwriteIndexer(box EInexactMayOverwriteIndexerData {
            use_op,
            ..
        }) => util(use_op),
        ErrorMessage::EImplicitInstantiationUnderconstrainedError(
            box EImplicitInstantiationUnderconstrainedErrorData { use_op, .. },
        ) => util(use_op),
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

            Self::ENonStrictEqualityComparison(box (primary, _))
            | Self::EInvalidTypeArgs(box (_, primary))
            | Self::ETooFewTypeArgs(box ETooFewTypeArgsData {
                reason_tapp: primary,
                ..
            })
            | Self::ETooManyTypeArgs(box ETooManyTypeArgsData {
                reason_tapp: primary,
                ..
            }) => Some(primary.loc.dupe()),

            Self::EComparison(box EComparisonData { r1, loc_opt, .. }) => {
                loc_opt.as_ref().or(Some(&r1.loc)).map(|l| l.dupe())
            }

            Self::ESketchyNumberLint(_, reason)
            | Self::EInvalidExtends(reason)
            | Self::EUnsupportedSetProto(reason)
            | Self::EReactElementFunArity(box (reason, _, _))
            | Self::EReactRefInRender { usage: reason, .. }
            | Self::EUnsupportedImplements(reason)
            | Self::EObjectComputedPropertyAssign(box (reason, _, _))
            | Self::EObjectComputedPropertyAccess(box EObjectComputedPropertyAccessData {
                reason_prop: reason,
                ..
            })
            | Self::EForInRHS(reason)
            | Self::EBinaryInRHS(reason)
            | Self::EBinaryInLHS(reason)
            | Self::EInstanceofRHS(reason)
            | Self::EArithmeticOperand(reason)
            | Self::ERecursionLimit(box (reason, _))
            | Self::EMissingLocalAnnotation { reason, .. }
            | Self::EComponentMissingReturn(reason)
            | Self::ENestedComponent(reason)
            | Self::ENestedHook(reason)
            | Self::EUnsupportedExact(box (_, reason))
            | Self::EPolarityMismatch(box EPolarityMismatchData { reason, .. })
            | Self::ENoNamedExport(box (reason, _, _, _))
            | Self::EOnlyDefaultExport(box (reason, _, _))
            | Self::ENoDefaultExport(box (reason, _, _))
            | Self::EImportTypeAsValue(box (reason, _))
            | Self::EImportTypeAsTypeof(box (reason, _))
            | Self::EExportValueAsType(box (reason, _))
            | Self::EImportValueAsType(box (reason, _))
            | Self::ETemporaryHardcodedErrorForPrototyping(box (reason, _))
            | Self::EComputedPropertyWithUnion(reason)
            | Self::ETypeParamConstInvalidPosition(reason) => Some(reason.loc.dupe()),

            Self::EObjectComputedPropertyPotentialOverwrite(
                box EObjectComputedPropertyPotentialOverwriteData { key_loc, .. },
            ) => Some(key_loc.dupe()),

            Self::EEnumError(
                EnumErrorKind::EnumAllMembersAlreadyChecked(box EnumAllMembersAlreadyCheckedData {
                    loc: key_loc,
                    ..
                })
                | EnumErrorKind::EnumMemberAlreadyChecked(box EnumMemberAlreadyCheckedData {
                    case_test_loc: key_loc,
                    ..
                })
                | EnumErrorKind::EnumInvalidCheck(box EnumInvalidCheckData { loc: key_loc, .. })
                | EnumErrorKind::EnumInvalidMemberName(box EnumInvalidMemberNameData {
                    loc: key_loc,
                    ..
                })
                | EnumErrorKind::EnumNonIdentifierMemberName(box EnumNonIdentifierMemberNameData {
                    loc: key_loc,
                    ..
                }),
            ) => Some(key_loc.dupe()),

            Self::EEnumError(
                EnumErrorKind::EnumNotAllChecked(box EnumNotAllCheckedData { reason, .. })
                | EnumErrorKind::EnumUnknownNotChecked(box EnumUnknownNotCheckedData {
                    reason, ..
                })
                | EnumErrorKind::EnumInvalidAbstractUse(box EnumInvalidAbstractUseData {
                    reason,
                    ..
                })
                | EnumErrorKind::EnumMemberUsedAsType(box EnumMemberUsedAsTypeData {
                    reason, ..
                })
                | EnumErrorKind::EnumInvalidMemberAccess(box EnumInvalidMemberAccessData {
                    reason,
                    ..
                })
                | EnumErrorKind::EnumInvalidObjectUtilType(box EnumInvalidObjectUtilTypeData {
                    reason,
                    ..
                })
                | EnumErrorKind::EnumInvalidObjectFunction(box EnumInvalidObjectFunctionData {
                    reason,
                    ..
                })
                | EnumErrorKind::EnumNotIterable { reason, .. },
            )
            | Self::ERecursiveDefinition(box ERecursiveDefinitionData { reason, .. })
            | Self::EInvalidConstructor(reason)
            | Self::EInvalidDeclaration(box EInvalidDeclarationData {
                declaration: reason,
                ..
            })
            | Self::EBigIntRShift3(reason)
            | Self::EBigIntNumCoerce(reason)
            | Self::EInvalidBinaryArith(box EInvalidBinaryArithData {
                reason_out: reason, ..
            })
            | Self::ETupleRequiredAfterOptional(box ETupleRequiredAfterOptionalData {
                reason_tuple: reason,
                ..
            })
            | Self::ETupleInvalidTypeSpread(box ETupleInvalidTypeSpreadData {
                reason_spread: reason,
                ..
            })
            | Self::ETupleElementAfterInexactSpread(reason)
            | Self::ETypeGuardInvalidParameter(box ETypeGuardInvalidParameterData {
                type_guard_reason: reason,
                ..
            })
            | Self::ETypeGuardParamUnbound(reason)
            | Self::ETypeGuardThisParam(reason)
            | Self::ETypeGuardFunctionInvalidWrites(box ETypeGuardFunctionInvalidWritesData {
                reason,
                ..
            })
            | Self::ENegativeTypeGuardConsistency(box ENegativeTypeGuardConsistencyData {
                return_reason: reason,
                ..
            })
            | Self::ETypeGuardFunctionParamHavoced(box ETypeGuardFunctionParamHavocedData {
                type_guard_reason: reason,
                ..
            })
            | Self::EIllegalAssertOperator(box EIllegalAssertOperatorData { op: reason, .. }) => {
                Some(reason.loc.dupe())
            }

            Self::EDefinitionCycle(dependencies) => Some(dependencies.first().0.loc.dupe()),

            Self::EExponentialSpread(box EExponentialSpreadData {
                reasons_for_operand1,
                reasons_for_operand2,
                ..
            }) => {
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

            Self::EBindingError(box (_, loc, _, _)) => Some(loc.dupe()),

            Self::EComponentCase(loc)
            | Self::EComponentMissingBody(loc)
            | Self::EComponentBodyInAmbientContext(loc)
            | Self::EDeclareComponentInvalidParam { loc, .. }
            | Self::EHookNaming(loc)
            | Self::EIncorrectTypeWithReplacement(box EIncorrectTypeWithReplacementData {
                loc,
                ..
            })
            | Self::EUnsafeGettersSetters(loc)
            | Self::EUnsafeObjectAssign(loc)
            | Self::ETSSyntax(box ETSSyntaxData { kind: _, loc })
            | Self::EVarianceKeyword(box EVarianceKeywordData { kind: _, loc }) => Some(loc.dupe()),

            Self::EInvalidTypeof(box (loc, _)) => Some(loc.dupe()),
            Self::EReactIntrinsicOverlap(box EReactIntrinsicOverlapData { def, .. }) => {
                Some(def.dupe())
            }
            Self::EStrUtilTypeNonLiteralArg(loc) => Some(loc.dupe()),

            Self::EInvalidPrototype(box (loc, _))
            | Self::EUntypedTypeImport(box (loc, _))
            | Self::EUntypedImport(box (loc, _))
            | Self::EInvalidInfer(loc)
            | Self::EConstantCondition(box EConstantConditionData { loc, .. })
            | Self::EInvalidReactCreateElement(box EInvalidReactCreateElementData {
                create_element_loc: loc,
                ..
            })
            | Self::ENonstrictImport(loc)
            | Self::EUnclearType(loc)
            | Self::EDeprecatedBool(loc)
            | Self::EInternalType(loc, _)
            | Self::EUnnecessaryOptionalChain(box (loc, _))
            | Self::EUnnecessaryInvariant(box (loc, _))
            | Self::EUnnecessaryDeclareTypeOnlyExport(loc)
            | Self::EUnusedSuppression(loc)
            | Self::ECodelessSuppression(loc)
            | Self::EDocblockError(box (loc, _))
            | Self::EImplicitInexactObject(loc)
            | Self::EInvalidComponentRestParam(loc)
            | Self::EAmbiguousObjectType(loc)
            | Self::EParseError(box (loc, _))
            | Self::EInvalidLHSInAssignment(loc)
            | Self::EUnreachable(loc)
            | Self::ECannotDelete(box (loc, _))
            | Self::EBadExportContext(box (_, loc))
            | Self::EBadExportPosition(loc)
            | Self::EBadDefaultImportAccess(box (loc, _))
            | Self::EBadDefaultImportDestructuring(loc)
            | Self::EInvalidImportStarUse(box (loc, _))
            | Self::ENonConstVarExport(box (loc, _))
            | Self::EThisInExportedFunction(loc)
            | Self::EMixedImportAndRequire(box (loc, _))
            | Self::EUnsupportedVarianceAnnotation(box (loc, _))
            | Self::EExportRenamedDefault(box EExportRenamedDefaultData { loc, .. })
            | Self::EIndeterminateModuleType(loc)
            | Self::EEnumError(
                EnumErrorKind::EnumsNotEnabled(loc) | EnumErrorKind::EnumConstNotSupported(loc),
            )
            | Self::EUninitializedInstanceProperty(loc, _)
            | Self::EUseArrayLiteral(loc)
            | Self::EUnsupportedSyntax(box (loc, _))
            | Self::EInternal(box (loc, _))
            | Self::EUnsupportedKeyInObject { loc, .. }
            | Self::EAmbiguousNumericKeyWithVariance(loc)
            | Self::EHookRuleViolation(box EHookRuleViolationData { call_loc: loc, .. })
            | Self::EExportsAnnot(loc)
            | Self::EUnexpectedThisType(loc)
            | Self::ETypeParamMinArity(loc, _)
            | Self::EAssignConstLikeBinding(box EAssignConstLikeBindingData { loc, .. })
            | Self::EMalformedCode(loc)
            | Self::EObjectThisSuperReference(box (loc, _, _))
            | Self::EComponentThisReference(box EComponentThisReferenceData {
                this_loc: loc,
                ..
            })
            | Self::EInvalidGraphQL(box (loc, _))
            | Self::EAnnotationInference(box (loc, _, _, _))
            | Self::ETrivialRecursiveDefinition(box (loc, _))
            | Self::EInvalidCatchParameterAnnotation { loc, .. }
            | Self::EInvalidMappedType { loc, .. }
            | Self::EReferenceInAnnotation(box (loc, _, _))
            | Self::EReferenceInDefault(box (_, _, loc))
            | Self::EDuplicateComponentProp(box EDuplicateComponentPropData {
                spread: loc, ..
            })
            | Self::ERefComponentProp(box ERefComponentPropData { spread: loc, .. })
            | Self::EKeySpreadProp(box EKeySpreadPropData { spread: loc, .. })
            | Self::ETypeGuardIncompatibleWithFunctionKind(
                box ETypeGuardIncompatibleWithFunctionKindData { loc, .. },
            )
            | Self::EMissingPlatformSupportWithAvailablePlatforms(
                box EMissingPlatformSupportWithAvailablePlatformsData { loc, .. },
            )
            | Self::EMissingPlatformSupport(box EMissingPlatformSupportData { loc, .. })
            | Self::EUnionOptimization(box EUnionOptimizationData { loc, .. })
            | Self::EUnionOptimizationOnNonUnion(box EUnionOptimizationOnNonUnionData {
                loc,
                ..
            })
            | Self::ELintSetting(box (loc, _))
            | Self::ETypeParamArity(loc, _)
            | Self::ESketchyNullLint(box ESketchyNullLintData { loc, .. }) => Some(loc.dupe()),

            Self::EUnionPartialOptimizationNonUniqueKey(
                box EUnionPartialOptimizationNonUniqueKeyData { loc, .. },
            ) => Some(loc.dupe()),

            Self::ECallTypeArity(box ECallTypeArityData { call_loc, .. }) => Some(call_loc.dupe()),
            Self::EMissingTypeArgs(box EMissingTypeArgsData { reason_op, .. }) => {
                Some(reason_op.loc.dupe())
            }
            Self::EInvalidRendersTypeArgument(box EInvalidRendersTypeArgumentData {
                loc, ..
            }) => Some(loc.dupe()),
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
                BindingValidation::InterfaceMergePropertyConflict {
                    existing_binding_loc,
                    ..
                } => Some(existing_binding_loc.dupe()),
                BindingValidation::InterfaceMergeTparamMismatch {
                    existing_binding_loc,
                    ..
                } => Some(existing_binding_loc.dupe()),
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

            Self::EDuplicateModuleProvider(box EDuplicateModuleProviderData {
                conflict, ..
            }) => Some(conflict.dupe()),
            Self::EEnumError(
                EnumErrorKind::EnumModification(box EnumModificationData { loc, .. })
                | EnumErrorKind::EnumMemberDuplicateValue(box EnumMemberDuplicateValueData {
                    loc,
                    ..
                })
                | EnumErrorKind::EnumDuplicateMemberName(box EnumDuplicateMemberNameData {
                    loc, ..
                })
                | EnumErrorKind::EnumInconsistentMemberValues(box EnumInconsistentMemberValuesData {
                    loc,
                    ..
                })
                | EnumErrorKind::EnumInvalidMemberInitializer(box EnumInvalidMemberInitializerData {
                    loc,
                    ..
                })
                | EnumErrorKind::EnumBooleanMemberNotInitialized(
                    box EnumBooleanMemberNotInitializedData { loc, .. },
                )
                | EnumErrorKind::EnumNumberMemberNotInitialized(
                    box EnumNumberMemberNotInitializedData { loc, .. },
                )
                | EnumErrorKind::EnumBigIntMemberNotInitialized(
                    box EnumBigIntMemberNotInitializedData { loc, .. },
                )
                | EnumErrorKind::EnumStringMemberInconsistentlyInitialized(
                    box EnumStringMemberInconsistentlyInitializedData { loc, .. },
                ),
            )
            | Self::EBuiltinNameLookupFailed(box EBuiltinNameLookupFailedData { loc, .. })
            | Self::EBuiltinModuleLookupFailed(box EBuiltinModuleLookupFailedData {
                loc, ..
            })
            | Self::EExpectedModuleLookupFailed(box EExpectedModuleLookupFailedData {
                loc, ..
            })
            | Self::EPlatformSpecificImplementationModuleLookupFailed(
                box EPlatformSpecificImplementationModuleLookupFailedData { loc, .. },
            )
            | Self::EDuplicateClassMember(box EDuplicateClassMemberData { loc, .. })
            | Self::EEmptyArrayNoProvider { loc }
            | Self::EUnusedPromise { loc, .. } => Some(loc.dupe()),

            Self::ECannotCallReactComponent { reason } => Some(reason.loc.dupe()),

            Self::EMatchError(e) => match e {
                MatchErrorKind::MatchNotExhaustive(box MatchNotExhaustiveData { loc, .. })
                | MatchErrorKind::MatchNonExhaustiveObjectPattern(
                    box MatchNonExhaustiveObjectPatternData { loc, .. },
                )
                | MatchErrorKind::MatchNonExplicitEnumCheck(box MatchNonExplicitEnumCheckData {
                    loc,
                    ..
                })
                | MatchErrorKind::MatchInvalidBindingKind { loc, .. }
                | MatchErrorKind::MatchInvalidObjectPropertyLiteral { loc, .. }
                | MatchErrorKind::MatchInvalidUnaryZero { loc }
                | MatchErrorKind::MatchInvalidUnaryPlusBigInt { loc }
                | MatchErrorKind::MatchDuplicateObjectProperty(
                    box MatchDuplicateObjectPropertyData { loc, .. },
                )
                | MatchErrorKind::MatchBindingInOrPattern { loc }
                | MatchErrorKind::MatchInvalidAsPattern { loc }
                | MatchErrorKind::MatchInvalidPatternReference(
                    box MatchInvalidPatternReferenceData { loc, .. },
                )
                | MatchErrorKind::MatchInvalidObjectShorthand(
                    box MatchInvalidObjectShorthandData { loc, .. },
                )
                | MatchErrorKind::MatchStatementInvalidBody { loc }
                | MatchErrorKind::MatchInvalidCaseSyntax(box MatchInvalidCaseSyntaxData {
                    loc,
                    ..
                })
                | MatchErrorKind::MatchInvalidIdentOrMemberPattern(
                    box MatchInvalidIdentOrMemberPatternData { loc, .. },
                ) => Some(loc.dupe()),
                MatchErrorKind::MatchInvalidWildcardSyntax(loc)
                | MatchErrorKind::MatchInvalidInstancePattern(loc)
                | MatchErrorKind::MatchInvalidGuardedWildcard(loc) => Some(loc.dupe()),
                MatchErrorKind::MatchUnusedPattern(box MatchUnusedPatternData {
                    reason, ..
                }) => Some(reason.loc.dupe()),
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
            Self::EDevOnlyRefinedLocInfo(box EDevOnlyRefinedLocInfoData {
                refined_loc, ..
            }) => Some(refined_loc.dupe()),
            Self::EDevOnlyInvalidatedRefinementInfo(
                box EDevOnlyInvalidatedRefinementInfoData { read_loc, .. },
            ) => Some(read_loc.dupe()),

            Self::EUnableToSpread(box EUnableToSpreadData { .. })
            | Self::ECannotSpreadInterface(box ECannotSpreadInterfaceData { .. })
            | Self::ECannotSpreadIndexerOnRight(box ECannotSpreadIndexerOnRightData { .. })
            | Self::EInexactMayOverwriteIndexer(box EInexactMayOverwriteIndexerData { .. })
            | Self::EFunctionCallExtraArg { .. }
            | Self::ENotAReactComponent { .. }
            | Self::EIncompatibleWithUseOp(box EIncompatibleWithUseOpData { .. })
            | Self::EInvariantSubtypingWithUseOp(..)
            | Self::EEnumError(EnumErrorKind::EnumIncompatible(box EnumIncompatibleData {
                ..
            }))
            | Self::EIncompatibleDefs(..)
            | Self::EInvalidObjectKit(box EInvalidObjectKitData { .. })
            | Self::EIncompatibleWithExact { .. }
            | Self::EFunctionIncompatibleWithIndexer { .. }
            | Self::EUnionSpeculationFailed(..)
            | Self::ETupleUnsafeWrite { .. }
            | Self::EROArrayWrite { .. }
            | Self::ETupleElementNotReadable(box ETupleElementNotReadableData { .. })
            | Self::ETupleElementNotWritable(box ETupleElementNotWritableData { .. })
            | Self::ETupleElementPolarityMismatch(box ETupleElementPolarityMismatchData {
                ..
            })
            | Self::ETupleOutOfBounds(box ETupleOutOfBoundsData { .. })
            | Self::ETupleNonIntegerIndex(box ETupleNonIntegerIndexData { .. })
            | Self::ENonLitArrayToTuple { .. }
            | Self::ETupleArityMismatch(box ETupleArityMismatchData { .. })
            | Self::EPrivateLookupFailed { .. }
            | Self::EPropPolarityMismatch(box EPropPolarityMismatchData { .. })
            | Self::EPropNotReadable(box EPropNotReadableData { .. })
            | Self::EPropNotWritable(box EPropNotWritableData { .. })
            | Self::EPropNotFoundInLookup(box EPropNotFoundInLookupData { .. })
            | Self::EPropNotFoundInSubtyping(box EPropNotFoundInSubtypingData { .. })
            | Self::EPropsNotFoundInSubtyping(box EPropsNotFoundInSubtypingData { .. })
            | Self::EPropsNotFoundInInvariantSubtyping(..)
            | Self::EPropsExtraAgainstExactObject(box EPropsExtraAgainstExactObjectData {
                ..
            })
            | Self::EIndexerCheckFailed(box EIndexerCheckFailedData { .. })
            | Self::EExpectedBooleanLit(box EExpectedBooleanLitData { .. })
            | Self::EExpectedNumberLit(box EExpectedNumberLitData { .. })
            | Self::EExpectedStringLit(box EExpectedStringLitData { .. })
            | Self::EExpectedBigIntLit(box EExpectedBigIntLitData { .. })
            | Self::EIncompatibleProp(box EIncompatiblePropData { .. })
            | Self::EIncompatible(box EIncompatibleData { .. })
            | Self::EIncompatibleSpeculation(..)
            | Self::EMethodUnbinding(box EMethodUnbindingData { .. })
            | Self::EHookIncompatible(box EHookIncompatibleData { .. })
            | Self::EHookUniqueIncompatible(box EHookUniqueIncompatibleData { .. })
            | Self::EImplicitInstantiationUnderconstrainedError(
                box EImplicitInstantiationUnderconstrainedErrorData { .. },
            )
            | Self::EClassToObject(box EClassToObjectData { .. })
            | Self::EPrimitiveAsInterface(box EPrimitiveAsInterfaceData { .. })
            | Self::ETypeGuardFuncIncompatibility { .. }
            | Self::ETypeGuardIndexMismatch { .. }
            | Self::ETypeGuardImpliesMismatch { .. }
            | Self::ETypeParamConstIncompatibility(box ETypeParamConstIncompatibilityData {
                ..
            }) => None,
        }
    }

    pub fn kind_of_msg(&self) -> ErrorKind {
        use ErrorKind::*;
        use LintKind::*;

        match self {
            // Lint errors
            ErrorMessage::EUntypedTypeImport(box (_, _)) => LintError(UntypedTypeImport),
            ErrorMessage::EUntypedImport(box (_, _)) => LintError(UntypedImport),
            ErrorMessage::ENonstrictImport(_) => LintError(NonstrictImport),
            ErrorMessage::EInternalType(_, _) => LintError(InternalType),
            ErrorMessage::EUnclearType(_) => LintError(UnclearType),
            ErrorMessage::EDeprecatedBool(_) => LintError(DeprecatedType(DeprecatedTypeKind::Bool)),
            ErrorMessage::EUnsafeGettersSetters(_) => LintError(UnsafeGettersSetters),
            ErrorMessage::EUnsafeObjectAssign(_) => LintError(UnsafeObjectAssign),
            ErrorMessage::ESketchyNullLint(box ESketchyNullLintData { kind, .. }) => {
                LintError(SketchyNull(*kind))
            }
            ErrorMessage::ESketchyNumberLint(kind, _) => LintError(SketchyNumber(*kind)),
            ErrorMessage::EUnnecessaryOptionalChain(box (_, _)) => {
                LintError(UnnecessaryOptionalChain)
            }
            ErrorMessage::EUnnecessaryInvariant(box (_, _)) => LintError(UnnecessaryInvariant),
            ErrorMessage::EImplicitInexactObject(_) => LintError(ImplicitInexactObject),
            ErrorMessage::EAmbiguousObjectType(_) => LintError(AmbiguousObjectType),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked(
                box EnumNotAllCheckedData {
                    default_case_loc: Some(_),
                    ..
                },
            )) => LintError(RequireExplicitEnumSwitchCases),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck(
                box MatchNonExplicitEnumCheckData { .. },
            )) => LintError(RequireExplicitEnumChecks),
            ErrorMessage::EUninitializedInstanceProperty(_, _) => {
                LintError(UninitializedInstanceProperty)
            }
            ErrorMessage::EBadDefaultImportAccess(box (_, _)) => LintError(DefaultImportAccess),
            ErrorMessage::EBadDefaultImportDestructuring(_) => LintError(DefaultImportAccess),
            ErrorMessage::EInvalidImportStarUse(box (_, _)) => LintError(InvalidImportStarUse),
            ErrorMessage::ENonConstVarExport(box (_, _)) => LintError(NonConstVarExport),
            ErrorMessage::EThisInExportedFunction(_) => LintError(ThisInExportedFunction),
            ErrorMessage::EMixedImportAndRequire(box (_, _)) => LintError(MixedImportAndRequire),
            ErrorMessage::EExportRenamedDefault(box EExportRenamedDefaultData { .. }) => {
                LintError(ExportRenamedDefault)
            }
            ErrorMessage::EUnusedPromise { .. } => LintError(UnusedPromise),
            ErrorMessage::EReactIntrinsicOverlap(box EReactIntrinsicOverlapData { .. }) => {
                LintError(ReactIntrinsicOverlap)
            }
            ErrorMessage::ENestedComponent(_) => LintError(NestedComponent),
            ErrorMessage::ENestedHook(_) => LintError(NestedHook),
            ErrorMessage::ESignatureBindingValidation(BindingValidation::ModuleOverride {
                ..
            })
            | ErrorMessage::ESignatureBindingValidation(BindingValidation::NameOverride {
                ..
            }) => LintError(LibdefOverride),

            // Infer warnings
            ErrorMessage::EBadExportPosition(_) | ErrorMessage::EBadExportContext(box (_, _)) => {
                InferWarning(InferWarningKind::ExportKind)
            }
            ErrorMessage::EEnumError(
                EnumErrorKind::EnumsNotEnabled(_) | EnumErrorKind::EnumConstNotSupported(_),
            )
            | ErrorMessage::EIndeterminateModuleType(_)
            | ErrorMessage::EUnreachable(_)
            | ErrorMessage::EInvalidTypeof(box (_, _)) => InferWarning(InferWarningKind::OtherKind),

            // Other error kinds
            ErrorMessage::EInternal(box (_, _)) => InternalError,
            ErrorMessage::ERecursionLimit(box (_, _)) => RecursionLimitError,
            ErrorMessage::EDuplicateModuleProvider(box EDuplicateModuleProviderData { .. }) => {
                DuplicateProviderError
            }
            ErrorMessage::EParseError(box (_, _)) => ParseError,
            ErrorMessage::EDocblockError(box (_, _)) | ErrorMessage::ELintSetting(box (_, _)) => {
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
        InternalError::WorkerCanceled => "check job was canceled".into(),
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
pub struct IncompatibleUseData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub upper_kind: UpperKind<L>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct SpeculationData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub use_op: VirtualUseOp<L>,
    pub branches: Vec<ErrorMessage<L>>,
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
pub struct IncompatibleSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
    pub explanation: Option<Explanation<L>>,
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
pub struct IncompatibleInvariantSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub sub_component: Option<SubComponentOfInvariantSubtypingError>,
    pub lower_loc: L,
    pub upper_loc: L,
    pub lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub use_op: VirtualUseOp<L>,
    pub explanation: Option<Explanation<L>>,
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
pub struct IncompatibleEnumData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
    pub enum_kind: EnumKind,
    pub representation_type: Option<FlowSmolStr>,
    pub casting_syntax: CastingSyntax,
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
pub struct PropMissingInLookupData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub prop: Option<FlowSmolStr>,
    pub suggestion: Option<FlowSmolStr>,
    pub reason_obj: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
    pub reason_indexer: Option<VirtualReason<L>>,
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
pub struct PropMissingInSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub prop: Option<FlowSmolStr>,
    pub suggestion: Option<FlowSmolStr>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub reason_indexer: Option<VirtualReason<L>>,
    pub use_op: VirtualUseOp<L>,
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
pub struct PropsMissingInSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub props: Vec1<FlowSmolStr>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
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
pub struct PropsMissingInInvariantSubtypingData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub props: Vec1<FlowSmolStr>,
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub lower_obj_loc: L,
    pub upper_obj_loc: L,
    pub lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
    pub use_op: VirtualUseOp<L>,
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
pub struct PropsExtraAgainstExactObjectData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub props: Vec1<FlowSmolStr>,
    pub reason_l_obj: VirtualReason<L>,
    pub reason_r_obj: VirtualReason<L>,
    pub use_op: VirtualUseOp<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct UseOpData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub loc: L,
    pub message: Message<L>,
    pub use_op: VirtualUseOp<L>,
    pub explanation: Option<Explanation<L>>,
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
pub struct PropPolarityMismatchData<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    pub reason_lower: VirtualReason<L>,
    pub reason_upper: VirtualReason<L>,
    pub props: Vec1<(Option<FlowSmolStr>, Polarity, Polarity)>,
    pub use_op: VirtualUseOp<L>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum FriendlyMessageRecipe<L: Dupe + PartialOrd + Ord + PartialEq + Eq> {
    IncompatibleUse(Box<IncompatibleUseData<L>>),
    Speculation(Box<SpeculationData<L>>),
    IncompatibleSubtyping(Box<IncompatibleSubtypingData<L>>),
    IncompatibleInvariantSubtyping(Box<IncompatibleInvariantSubtypingData<L>>),
    IncompatibleEnum(Box<IncompatibleEnumData<L>>),
    PropMissingInLookup(Box<PropMissingInLookupData<L>>),
    PropMissingInSubtyping(Box<PropMissingInSubtypingData<L>>),
    PropsMissingInSubtyping(Box<PropsMissingInSubtypingData<L>>),
    PropsMissingInInvariantSubtyping(Box<PropsMissingInInvariantSubtypingData<L>>),
    PropsExtraAgainstExactObject(Box<PropsExtraAgainstExactObjectData<L>>),
    Normal(Message<L>),
    UseOp(Box<UseOpData<L>>),
    PropPolarityMismatch(Box<PropPolarityMismatchData<L>>),
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
            ErrorMessage::EIncompatible(box EIncompatibleData {
                lower: (reason_lower, _),
                upper: (reason_upper, upper_kind),
                use_op,
            }) => {
                let loc = reason_upper.loc.dupe();
                IncompatibleUse(Box::new(IncompatibleUseData {
                    loc,
                    upper_kind,
                    reason_lower,
                    reason_upper,
                    use_op: use_op
                        .unwrap_or(VirtualUseOp::Op(Arc::new(VirtualRootUseOp::UnknownUse))),
                }))
            }

            ErrorMessage::EIncompatibleSpeculation(box EIncompatibleSpeculationData {
                loc,
                use_op,
                branches,
            }) => Speculation(Box::new(SpeculationData {
                loc,
                use_op: use_op.unwrap_or(VirtualUseOp::Op(Arc::new(VirtualRootUseOp::UnknownUse))),
                branches,
            })),

            ErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData {
                use_op,
                reason_lower,
                reason_upper,
                branches,
            }) => {
                if branches.is_empty() {
                    IncompatibleSubtyping(Box::new(IncompatibleSubtypingData {
                        reason_lower,
                        reason_upper,
                        use_op,
                        explanation: None,
                    }))
                } else {
                    let loc = reason_upper.loc.dupe();
                    Speculation(Box::new(SpeculationData {
                        loc,
                        use_op,
                        branches,
                    }))
                }
            }

            ErrorMessage::EExportValueAsType(box (_, export_name)) => Normal(
                Message::MessageExportValueAsType(export_name.into_smol_str()),
            ),

            ErrorMessage::EImportValueAsType(box (_, export_name)) => {
                Normal(Message::MessageImportValueAsType(export_name))
            }

            ErrorMessage::EImportTypeAsTypeof(box (_, export_name)) => {
                Normal(Message::MessageImportTypeAsTypeof(export_name))
            }

            ErrorMessage::EImportTypeAsValue(box (_, export_name)) => {
                Normal(Message::MessageImportTypeAsValue(export_name))
            }

            ErrorMessage::EInvalidInfer { .. } => Normal(Message::MessageInvalidInferType),

            ErrorMessage::EInvalidExtends(reason) => {
                Normal(Message::MessageCannotUseAsSuperClass(reason))
            }

            ErrorMessage::EInvalidReactCreateElement(box EInvalidReactCreateElementData {
                invalid_react,
                ..
            }) => Normal(Message::MessageInvalidReactCreateElement(invalid_react)),

            ErrorMessage::EAnyValueUsedAsType { reason_use } => {
                Normal(Message::MessageAnyValueUsedAsType(reason_use.desc.clone()))
            }

            ErrorMessage::EValueUsedAsType { reason_use } => {
                Normal(Message::MessageValueUsedAsType(reason_use.desc.clone()))
            }

            ErrorMessage::EExpectedStringLit(box EExpectedStringLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => IncompatibleSubtyping(Box::new(IncompatibleSubtypingData {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            })),

            ErrorMessage::EExpectedNumberLit(box EExpectedNumberLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => IncompatibleSubtyping(Box::new(IncompatibleSubtypingData {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            })),

            ErrorMessage::EExpectedBooleanLit(box EExpectedBooleanLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => IncompatibleSubtyping(Box::new(IncompatibleSubtypingData {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            })),

            ErrorMessage::EExpectedBigIntLit(box EExpectedBigIntLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => IncompatibleSubtyping(Box::new(IncompatibleSubtypingData {
                reason_lower,
                reason_upper,
                use_op,
                explanation: None,
            })),

            ErrorMessage::EPropNotFoundInLookup(box EPropNotFoundInLookupData {
                prop_name,
                reason_obj,
                reason_prop,
                use_op,
                suggestion,
            }) => {
                let loc = reason_prop.loc.dupe();
                PropMissingInLookup(Box::new(PropMissingInLookupData {
                    loc,
                    prop: prop_name.as_ref().map(|n| n.dupe().into_smol_str()),
                    reason_obj,
                    use_op,
                    suggestion,
                    reason_indexer: None,
                }))
            }

            ErrorMessage::EPropNotFoundInSubtyping(box EPropNotFoundInSubtypingData {
                prop_name,
                suggestion,
                reason_lower,
                reason_upper,
                use_op,
            }) => PropMissingInSubtyping(Box::new(PropMissingInSubtypingData {
                prop: prop_name.as_ref().map(|n| n.dupe().into_smol_str()),
                suggestion,
                reason_lower,
                reason_upper,
                reason_indexer: None,
                use_op,
            })),

            ErrorMessage::EPropsNotFoundInSubtyping(box EPropsNotFoundInSubtypingData {
                prop_names,
                reason_lower,
                reason_upper,
                use_op,
            }) => PropsMissingInSubtyping(Box::new(PropsMissingInSubtypingData {
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
            })),

            ErrorMessage::EPropsExtraAgainstExactObject(
                box EPropsExtraAgainstExactObjectData {
                    prop_names,
                    reason_l_obj,
                    reason_r_obj,
                    use_op,
                },
            ) => PropsExtraAgainstExactObject(Box::new(PropsExtraAgainstExactObjectData {
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
            })),

            ErrorMessage::EIndexerCheckFailed(box EIndexerCheckFailedData {
                prop_name,
                reason_lower,
                reason_upper,
                reason_indexer,
                use_op,
            }) => PropMissingInSubtyping(Box::new(PropMissingInSubtypingData {
                prop: Some(prop_name.into_smol_str()),
                suggestion: None,
                reason_lower,
                reason_upper,
                reason_indexer: Some(reason_indexer),
                use_op,
            })),

            ErrorMessage::EPropNotReadable(box EPropNotReadableData {
                reason_prop,
                prop_name,
                use_op,
            }) => {
                let loc = reason_prop.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessagePropNotReadable(prop_name),
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::EPropNotWritable(box EPropNotWritableData {
                reason_prop,
                prop_name,
                use_op,
            }) => {
                let loc = reason_prop.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessagePropNotWritable(prop_name),
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::EPropPolarityMismatch(box EPropPolarityMismatchData {
                lreason,
                ureason,
                props,
                use_op,
            }) => PropPolarityMismatch(Box::new(PropPolarityMismatchData {
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
            })),

            ErrorMessage::EUnionSpeculationFailed(box EUnionSpeculationFailedData {
                use_op,
                reason,
                branches,
                ..
            }) => {
                let loc = reason.loc.dupe();
                Speculation(Box::new(SpeculationData {
                    loc,
                    use_op,
                    branches,
                }))
            }

            ErrorMessage::EUnsupportedExact(box (_, lower)) => {
                Normal(Message::MessageCannotCreateExactType(lower))
            }

            ErrorMessage::EUnexpectedThisType(_) => Normal(Message::MessageUnexpectedUseOfThisType),

            ErrorMessage::EExportsAnnot(_) => Normal(Message::MessageCannotUseDollarExports),

            ErrorMessage::EUseArrayLiteral(_) => Normal(Message::MessageShouldUseArrayLiteral),

            ErrorMessage::ERecursionLimit(box (..)) => {
                Normal(Message::MessageRecursionLimitExceeded)
            }

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

            ErrorMessage::EIncompatibleWithUseOp(box EIncompatibleWithUseOpData {
                reason_lower,
                reason_upper,
                use_op,
                explanation,
            }) => IncompatibleSubtyping(Box::new(IncompatibleSubtypingData {
                reason_lower,
                reason_upper,
                use_op,
                explanation,
            })),

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

            ErrorMessage::EInvalidPrototype(box (_, reason)) => {
                Normal(Message::MessageCannotUseAsPrototype(reason))
            }

            ErrorMessage::EUnnecessaryOptionalChain(box (_, lhs_reason)) => {
                Normal(Message::MessageUnnecessaryOptionalChain(lhs_reason))
            }

            ErrorMessage::EUnnecessaryInvariant(box (_, reason)) => {
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

            ErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible(
                box EnumIncompatibleData {
                    reason_lower,
                    reason_upper,
                    use_op,
                    enum_kind,
                    representation_type,
                    casting_syntax,
                },
            )) => IncompatibleEnum(Box::new(IncompatibleEnumData {
                reason_lower,
                reason_upper,
                use_op,
                enum_kind,
                representation_type,
                casting_syntax,
            })),

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

            ErrorMessage::EFunctionCallExtraArg(box (
                unused_reason,
                def_reason,
                param_count,
                use_op,
            )) => UseOp(Box::new(UseOpData {
                loc: unused_reason.loc.dupe(),
                message: Message::MessageCannotCallFunctionWithExtraArg {
                    def_reason,
                    param_count,
                },
                explanation: None,
                use_op,
            })),
            ErrorMessage::EExponentialSpread(box EExponentialSpreadData {
                reason,
                reasons_for_operand1,
                reasons_for_operand2,
            }) => Normal(Message::MessageExponentialSpread(Box::new(
                MessageExponentialSpreadData {
                    reason,
                    reasons_for_operand1,
                    reasons_for_operand2,
                },
            ))),
            ErrorMessage::EComputedPropertyWithUnion(reason) => {
                Normal(Message::MessageCannotUseComputedPropertyWithUnion(reason))
            }

            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberAccess(
                box EnumInvalidMemberAccessData {
                    member_name,
                    suggestion,
                    reason,
                    enum_reason,
                },
            )) => Normal(Message::MessageCannotAccessEnumMember(Box::new(
                MessageCannotAccessEnumMemberData {
                    member_name,
                    suggestion,
                    description: reason.desc.clone(),
                    enum_reason,
                },
            ))),
            ErrorMessage::EEnumError(EnumErrorKind::EnumModification(
                box EnumModificationData { enum_reason, .. },
            )) => Normal(Message::MessageCannotChangeEnumMember(enum_reason)),
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberDuplicateValue(
                box EnumMemberDuplicateValueData {
                    prev_use_loc,
                    enum_reason,
                    ..
                },
            )) => Normal(Message::MessageDuplicateEnumMember {
                prev_use_loc,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotIterable { reason, for_in }) => {
                Normal(Message::MessageCannotIterateEnum { reason, for_in })
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberAlreadyChecked(
                box EnumMemberAlreadyCheckedData {
                    prev_check_loc,
                    enum_reason,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageAlreadyExhaustivelyCheckOneEnumMember(
                Box::new(MessageAlreadyExhaustivelyCheckOneEnumMemberData {
                    prev_check_loc,
                    enum_reason,
                    member_name,
                }),
            )),
            ErrorMessage::EEnumError(EnumErrorKind::EnumAllMembersAlreadyChecked(
                box EnumAllMembersAlreadyCheckedData { enum_reason, .. },
            )) => Normal(Message::MessageAlreadyExhaustivelyCheckAllEnumMembers { enum_reason }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked(
                box EnumNotAllCheckedData {
                    reason,
                    enum_reason,
                    left_to_check,
                    default_case_loc,
                },
            )) => Normal(Message::MessageIncompleteExhausiveCheckEnum(Box::new(
                MessageIncompleteExhausiveCheckEnumData {
                    description: reason.desc.clone(),
                    enum_reason,
                    left_to_check,
                    default_case_loc,
                },
            ))),
            ErrorMessage::EEnumError(EnumErrorKind::EnumUnknownNotChecked(
                box EnumUnknownNotCheckedData {
                    reason,
                    enum_reason,
                },
            )) => Normal(Message::MessageCannotExhaustivelyCheckEnumWithUnknowns(
                Box::new(MessageCannotExhaustivelyCheckEnumWithUnknownsData {
                    description: reason.desc.clone(),
                    enum_reason,
                }),
            )),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidCheck(
                box EnumInvalidCheckData {
                    enum_reason,
                    example_member,
                    from_match,
                    ..
                },
            )) => Normal(Message::MessageInvalidEnumMemberCheck(Box::new(
                MessageInvalidEnumMemberCheckData {
                    enum_reason,
                    example_member,
                    from_match,
                },
            ))),
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberUsedAsType(
                box EnumMemberUsedAsTypeData {
                    reason,
                    enum_reason,
                },
            )) => Normal(Message::MessageCannotUseEnumMemberUsedAsType(Box::new(
                MessageCannotUseEnumMemberUsedAsTypeData {
                    description: reason.desc.clone(),
                    enum_reason,
                },
            ))),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidAbstractUse(
                box EnumInvalidAbstractUseData {
                    reason,
                    enum_reason,
                },
            )) => Normal(Message::MessageCannotExhaustivelyCheckAbstractEnums(
                Box::new(MessageCannotExhaustivelyCheckAbstractEnumsData {
                    description: reason.desc.clone(),
                    enum_reason,
                }),
            )),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberName(
                box EnumInvalidMemberNameData {
                    enum_reason,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageInvalidEnumMemberName {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNonIdentifierMemberName(
                box EnumNonIdentifierMemberNameData {
                    enum_reason,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageEnumNonIdentifierMemberName {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumDuplicateMemberName(
                box EnumDuplicateMemberNameData {
                    prev_use_loc,
                    enum_reason,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageEnumDuplicateMemberName(Box::new(
                MessageEnumDuplicateMemberNameData {
                    member_name,
                    prev_use_loc,
                    enum_reason,
                },
            ))),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInconsistentMemberValues(
                box EnumInconsistentMemberValuesData { enum_reason, .. },
            )) => Normal(Message::MessageEnumInconsistentMemberValues { enum_reason }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberInitializer(
                box EnumInvalidMemberInitializerData {
                    enum_reason,
                    explicit_type,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageEnumInvalidMemberInitializer(Box::new(
                MessageEnumInvalidMemberInitializerData {
                    member_name,
                    explicit_type,
                    enum_reason,
                },
            ))),
            ErrorMessage::EEnumError(EnumErrorKind::EnumBooleanMemberNotInitialized(
                box EnumBooleanMemberNotInitializedData {
                    enum_reason,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageEnumBooleanMemberNotInitialized {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNumberMemberNotInitialized(
                box EnumNumberMemberNotInitializedData {
                    enum_reason,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageEnumNumberMemberNotInitialized {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumBigIntMemberNotInitialized(
                box EnumBigIntMemberNotInitializedData {
                    enum_reason,
                    member_name,
                    ..
                },
            )) => Normal(Message::MessageEnumBigIntMemberNotInitialized {
                member_name,
                enum_reason,
            }),
            ErrorMessage::EEnumError(EnumErrorKind::EnumStringMemberInconsistentlyInitialized(
                box EnumStringMemberInconsistentlyInitializedData { enum_reason, .. },
            )) => Normal(Message::MessageEnumStringMemberInconsistentlyInitialized { enum_reason }),

            ErrorMessage::EDuplicateClassMember(box EDuplicateClassMemberData {
                name,
                is_static,
                class_kind,
                ..
            }) => Normal(Message::MessageDuplicateClassMember {
                name,
                static_: is_static,
                class_kind,
            }),

            ErrorMessage::EInvalidDeclaration(box EInvalidDeclarationData {
                declaration,
                null_write: None,
                possible_generic_escape_locs,
            }) if possible_generic_escape_locs.is_empty() => Normal(
                Message::MessageVariableNeverInitAssignedAnnotated(declaration),
            ),
            ErrorMessage::EInvalidDeclaration(box EInvalidDeclarationData {
                declaration,
                null_write: None,
                possible_generic_escape_locs,
            }) => Normal(
                Message::MessageShouldAnnotateVariableOnlyInitializedInGenericContext {
                    reason: declaration,
                    possible_generic_escape_locs,
                },
            ),
            ErrorMessage::EInvalidDeclaration(box EInvalidDeclarationData {
                declaration,
                null_write: Some(null_write),
                possible_generic_escape_locs,
            }) if possible_generic_escape_locs.is_empty() => {
                let null_loc = if null_write.initialized {
                    None
                } else {
                    Some(null_write.null_loc)
                };
                Normal(Message::MessageVariableOnlyAssignedByNull(Box::new(
                    MessageVariableOnlyAssignedByNullData {
                        reason: declaration,
                        null_loc,
                    },
                )))
            }
            ErrorMessage::EInvalidDeclaration(box EInvalidDeclarationData {
                declaration,
                null_write: Some(null_write),
                possible_generic_escape_locs,
            }) => Normal(Message::MessageShouldAnnotateVariableUsedInGenericContext(
                Box::new(MessageShouldAnnotateVariableUsedInGenericContextData {
                    reason: declaration,
                    null_loc: null_write.null_loc,
                    initialized: null_write.initialized,
                    possible_generic_escape_locs,
                }),
            )),

            ErrorMessage::EAnnotationInference(box (_, reason_op, reason, suggestion)) => {
                Normal(Message::MessageCannotUseTypeForAnnotationInference(
                    Box::new(MessageCannotUseTypeForAnnotationInferenceData {
                        reason_op,
                        reason,
                        suggestion,
                    }),
                ))
            }
            ErrorMessage::ETrivialRecursiveDefinition(box (_, reason)) => Normal(
                Message::MessageInvalidTrivialRecursiveDefinition(reason.desc.clone()),
            ),
            ErrorMessage::ERecursiveDefinition(box ERecursiveDefinitionData {
                reason,
                recursion,
                annot_locs,
            }) => Normal(Message::MessageDefinitionInvalidRecursive(Box::new(
                MessageDefinitionInvalidRecursiveData {
                    description: reason.desc.clone(),
                    recursion,
                    annot_locs,
                },
            ))),
            ErrorMessage::EDefinitionCycle(dependencies) => {
                Normal(Message::MessageDefinitionCycle(dependencies))
            }
            ErrorMessage::EReferenceInAnnotation(box (_, name, loc)) => {
                Normal(Message::MessageInvalidSelfReferencingTypeAnnotation(
                    Box::new(MessageInvalidSelfReferencingTypeAnnotationData { name, loc }),
                ))
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

            ErrorMessage::EDuplicateComponentProp(box EDuplicateComponentPropData {
                spread,
                duplicates,
            }) => Normal(Message::MessageRedeclareComponentProp(Box::new(
                MessageRedeclareComponentPropData {
                    duplicates,
                    spread_loc: spread,
                },
            ))),
            ErrorMessage::ERefComponentProp(box ERefComponentPropData {
                spread: spread_loc,
                loc: ref_loc,
            }) => Normal(Message::MessageInvalidRefPropertyInSpread(Box::new(
                MessageInvalidRefPropertyInSpreadData {
                    ref_loc,
                    spread_loc,
                },
            ))),
            ErrorMessage::EKeySpreadProp(box EKeySpreadPropData {
                spread: spread_loc,
                loc: key_loc,
            }) => Normal(Message::MessageInvalidKeyPropertyInSpread(Box::new(
                MessageInvalidKeyPropertyInSpreadData {
                    spread_loc,
                    key_loc,
                },
            ))),

            ErrorMessage::EInvalidRendersTypeArgument(box EInvalidRendersTypeArgumentData {
                renders_variant,
                invalid_render_type_kind,
                invalid_type_reasons,
                ..
            }) => Normal(Message::MessageInvalidRendersTypeArgument(Box::new(
                MessageInvalidRendersTypeArgumentData {
                    renders_variant,
                    invalid_render_type_kind,
                    invalid_type_reasons,
                },
            ))),

            ErrorMessage::EInvalidTypeCastSyntax {
                enabled_casting_syntax,
                ..
            } => Normal(Message::MessageInvalidTypeCastingSyntax(
                enabled_casting_syntax,
            )),

            ErrorMessage::ECannotCallReactComponent { reason } => {
                Normal(Message::MessageCannotCallReactComponent(reason))
            }

            ErrorMessage::EMatchError(MatchErrorKind::MatchNotExhaustive(
                box MatchNotExhaustiveData { examples, .. },
            )) => Normal(Message::MessageMatchNotExhaustive { examples }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchUnusedPattern(
                box MatchUnusedPatternData {
                    reason,
                    already_seen,
                },
            )) => Normal(Message::MessageMatchUnnecessaryPattern {
                reason,
                already_seen,
            }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExhaustiveObjectPattern(
                box MatchNonExhaustiveObjectPatternData {
                    rest,
                    missing_props,
                    pattern_kind,
                    ..
                },
            )) => Normal(Message::MessageMatchNonExhaustiveObjectPattern(Box::new(
                MessageMatchNonExhaustiveObjectPatternData {
                    rest,
                    missing_props: missing_props.to_vec(),
                    pattern_kind,
                },
            ))),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck(
                box MatchNonExplicitEnumCheckData {
                    wildcard_reason,
                    unchecked_members,
                    ..
                },
            )) => Normal(Message::MessageMatchNonExplicitEnumCheck(Box::new(
                MessageMatchNonExplicitEnumCheckData {
                    wildcard_reason,
                    unchecked_members: unchecked_members.to_vec(),
                },
            ))),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidIdentOrMemberPattern(
                box MatchInvalidIdentOrMemberPatternData { type_reason, .. },
            )) => Normal(Message::MessageMatchInvalidIdentOrMemberPattern { type_reason }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidBindingKind { kind, .. }) => {
                Normal(Message::MessageMatchInvalidBindingKind { kind })
            }
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidObjectPropertyLiteral {
                pattern_kind,
                ..
            }) => Normal(Message::MessageMatchInvalidObjectPropertyLiteral { pattern_kind }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchDuplicateObjectProperty(
                box MatchDuplicateObjectPropertyData {
                    name, pattern_kind, ..
                },
            )) => Normal(Message::MessageMatchDuplicateObjectProperty { name, pattern_kind }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidPatternReference(
                box MatchInvalidPatternReferenceData { binding_reason, .. },
            )) => Normal(Message::MessageMatchInvalidPatternReference { binding_reason }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidObjectShorthand(
                box MatchInvalidObjectShorthandData {
                    name, pattern_kind, ..
                },
            )) => Normal(Message::MessageMatchInvalidObjectShorthand { name, pattern_kind }),
            ErrorMessage::EMatchError(MatchErrorKind::MatchInvalidCaseSyntax(
                box MatchInvalidCaseSyntaxData { kind, .. },
            )) => Normal(Message::MessageMatchInvalidCaseSyntax(kind)),

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

            ErrorMessage::EIllegalAssertOperator(box EIllegalAssertOperatorData {
                obj,
                specialized,
                ..
            }) => Normal(Message::MessageIllegalAssertOperator { obj, specialized }),

            ErrorMessage::EReferenceInDefault(box (def_loc, name, ref_loc)) => {
                Normal(Message::MessageInvalidSelfReferencingDefault(Box::new(
                    MessageInvalidSelfReferencingDefaultData {
                        name,
                        def_loc,
                        ref_loc,
                    },
                )))
            }

            ErrorMessage::EIncompatibleProp(box EIncompatiblePropData {
                prop,
                reason_prop,
                reason_obj,
                use_op,
                ..
            }) => PropMissingInLookup(Box::new(PropMissingInLookupData {
                loc: reason_prop.loc.dupe(),
                prop: prop.map(|n| n.into_smol_str()),
                reason_obj,
                use_op: use_op.unwrap_or(VirtualUseOp::Op(Arc::new(VirtualRootUseOp::UnknownUse))),
                suggestion: None,
                reason_indexer: None,
            })),

            ErrorMessage::EDevOnlyRefinedLocInfo(box EDevOnlyRefinedLocInfoData {
                refining_locs,
                ..
            }) => Normal(Message::MessageDevOnlyRefinedLocInfo { refining_locs }),

            ErrorMessage::EDevOnlyInvalidatedRefinementInfo(
                box EDevOnlyInvalidatedRefinementInfoData {
                    invalidation_info, ..
                },
            ) => Normal(Message::MessageDevOnlyInvalidatedRefinementInfo(
                invalidation_info,
            )),

            ErrorMessage::ETemporaryHardcodedErrorForPrototyping(box (_, str)) => {
                Normal(Message::MessagePlainTextReservedForInternalErrorOnly(str))
            }

            ErrorMessage::ENoDefaultExport(box (_, module_name, suggestion)) => Normal(
                Message::MessageNoDefaultExport(Box::new(MessageNoDefaultExportData {
                    module_name: module_name.into_inner(),
                    suggestion,
                })),
            ),

            ErrorMessage::EOnlyDefaultExport(box (_, module_name, export_name)) => Normal(
                Message::MessageOnlyDefaultExport(Box::new(MessageOnlyDefaultExportData {
                    module_name: module_name.into_inner(),
                    export_name,
                })),
            ),

            ErrorMessage::ENoNamedExport(box (_, module_name, export_name, suggestion)) => Normal(
                Message::MessageNoNamedExport(Box::new(MessageNoNamedExportData {
                    module_name: module_name.into_inner(),
                    export_name,
                    suggestion,
                })),
            ),

            ErrorMessage::EMissingTypeArgs(box EMissingTypeArgsData {
                reason_tapp,
                arity_loc,
                min_arity,
                max_arity,
                ..
            }) => Normal(Message::MessageCannotUseTypeWithoutAnyTypeArgs {
                reason_arity: VirtualReason::new(reason_tapp.desc.clone(), arity_loc),
                min_arity,
                max_arity,
            }),

            ErrorMessage::ETooManyTypeArgs(box ETooManyTypeArgsData {
                reason_tapp,
                arity_loc,
                maximum_arity,
                ..
            }) => Normal(Message::MessageCannotUseTypeWithTooManyTypeArgs {
                reason_arity: VirtualReason::new(reason_tapp.desc.clone(), arity_loc),
                n: maximum_arity,
            }),

            ErrorMessage::ETooFewTypeArgs(box ETooFewTypeArgsData {
                reason_tapp,
                arity_loc,
                minimum_arity,
                ..
            }) => Normal(Message::MessageCannotUseTypeWithTooFewTypeArgs {
                reason_arity: VirtualReason::new(reason_tapp.desc.clone(), arity_loc),
                n: minimum_arity,
            }),

            ErrorMessage::EInvalidTypeArgs(box (reason_main, reason_tapp)) => {
                Normal(Message::MessageCannotUseTypeWithInvalidTypeArgs {
                    reason_main,
                    reason_tapp,
                })
            }

            ErrorMessage::EConstantCondition(box EConstantConditionData {
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
                ..
            }) => Normal(Message::MessageConstantCondition {
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

            ErrorMessage::ECallTypeArity(box ECallTypeArityData {
                is_new,
                reason_arity,
                expected_arity,
                ..
            }) => Normal(Message::MessageCannotUseNonPolymorphicTypeWithTypeArgs {
                is_new,
                reason_arity,
                expected_arity,
            }),

            ErrorMessage::EPolarityMismatch(box EPolarityMismatchData {
                reason,
                name,
                expected_polarity,
                actual_polarity,
            }) => {
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

            ErrorMessage::EBuiltinNameLookupFailed(box EBuiltinNameLookupFailedData {
                name,
                ..
            }) => Normal(Message::MessageCannotResolveBuiltinName(name)),

            ErrorMessage::EBuiltinModuleLookupFailed(box EBuiltinModuleLookupFailedData {
                name,
                potential_generator,
                ..
            }) => Normal(Message::MessageCannotResolveBuiltinModule(Box::new(
                MessageCannotResolveBuiltinModuleData {
                    name,
                    potential_generator,
                },
            ))),

            ErrorMessage::EExpectedModuleLookupFailed(box EExpectedModuleLookupFailedData {
                name,
                expected_module_purpose,
                ..
            }) => Normal(Message::MessageCannotResolveExpectedModule {
                name,
                expected_module_purpose,
            }),

            ErrorMessage::EPrivateLookupFailed(box (reasons, x, use_op)) => {
                PropMissingInLookup(Box::new(PropMissingInLookupData {
                    loc: reasons.0.loc.dupe(),
                    prop: Some(format!("#{}", x).into()),
                    reason_obj: reasons.1,
                    use_op,
                    suggestion: None,
                    reason_indexer: None,
                }))
            }

            ErrorMessage::EPlatformSpecificImplementationModuleLookupFailed(
                box EPlatformSpecificImplementationModuleLookupFailedData { name, .. },
            ) => Normal(Message::MessagePlatformSpecificImplementationModuleLookupFailed(name)),

            ErrorMessage::EComparison(box EComparisonData {
                r1,
                r2,
                strict_comparison_opt,
                ..
            }) => Normal(Message::MessageCannotCompare(Box::new(
                MessageCannotCompareData {
                    lower: r1,
                    upper: r2,
                    strict_comparison_opt,
                },
            ))),

            ErrorMessage::ENonStrictEqualityComparison(box (lower, upper)) => {
                Normal(Message::MessageCannotCompareNonStrict { lower, upper })
            }

            ErrorMessage::ETupleArityMismatch(box ETupleArityMismatchData {
                use_op,
                lower_reason,
                lower_arity,
                lower_inexact,
                upper_reason,
                upper_arity,
                upper_inexact,
                unify,
            }) => {
                let loc = lower_reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatibleTupleArity(Box::new(
                        MessageIncompatibleTupleArityData {
                            lower_reason,
                            lower_arity,
                            lower_inexact,
                            upper_reason,
                            upper_arity,
                            upper_inexact,
                            unify,
                        },
                    )),
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETupleRequiredAfterOptional(box ETupleRequiredAfterOptionalData {
                reason_tuple,
                reason_required,
                reason_optional,
            }) => Normal(Message::MessageInvalidTupleRequiredAfterOptional {
                reason_tuple,
                reason_required,
                reason_optional,
            }),

            ErrorMessage::ETupleInvalidTypeSpread(box ETupleInvalidTypeSpreadData {
                reason_arg,
                ..
            }) => Normal(Message::MessageInvalidTupleTypeSpread(reason_arg)),

            ErrorMessage::ETupleElementAfterInexactSpread(_) => {
                Normal(Message::MessageTupleElementAfterInexactSpread)
            }

            ErrorMessage::ENonLitArrayToTuple((lower, upper), use_op) => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatibleNonLiteralArrayToTuple { lower, upper },
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETupleOutOfBounds(box ETupleOutOfBoundsData {
                reason,
                reason_op,
                inexact,
                length,
                index,
                use_op,
            }) => UseOp(Box::new(UseOpData {
                loc: reason.loc.dupe(),
                message: Message::MessageTupleIndexOutOfBound(Box::new(
                    MessageTupleIndexOutOfBoundData {
                        reason_op,
                        inexact,
                        length,
                        index,
                    },
                )),
                use_op,
                explanation: None,
            })),

            ErrorMessage::ETupleNonIntegerIndex(box ETupleNonIntegerIndexData {
                reason,
                index,
                use_op,
            }) => {
                let loc = reason.loc.dupe();
                let index_def_loc = reason.def_loc_opt.as_ref().unwrap_or(&reason.loc).dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageTupleNonIntegerIndex(Box::new(
                        MessageTupleNonIntegerIndexData {
                            index_def_loc,
                            index,
                        },
                    )),
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETupleUnsafeWrite { reason, use_op } => UseOp(Box::new(UseOpData {
                loc: reason.loc.dupe(),
                message: Message::MessageTupleNonStaticallyKnownIndex,
                use_op,
                explanation: None,
            })),

            ErrorMessage::ETupleElementNotReadable(box ETupleElementNotReadableData {
                reason,
                index,
                name,
                use_op,
            }) => {
                let loc = reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageTupleElementNotReadable(Box::new(
                        MessageTupleElementNotReadableData {
                            reason,
                            index,
                            name,
                        },
                    )),
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETupleElementNotWritable(box ETupleElementNotWritableData {
                reason,
                index,
                name,
                use_op,
            }) => {
                let loc = reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageTupleElementNotWritable(Box::new(
                        MessageTupleElementNotWritableData {
                            reason,
                            index,
                            name,
                        },
                    )),
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETupleElementPolarityMismatch(
                box ETupleElementPolarityMismatchData {
                    index,
                    reason_lower,
                    polarity_lower,
                    reason_upper,
                    polarity_upper,
                    use_op,
                },
            ) => {
                let loc = reason_lower.loc.dupe();
                UseOp(Box::new(UseOpData {
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
                }))
            }

            ErrorMessage::EROArrayWrite(reasons, use_op) => {
                let (lower, _) = reasons;
                UseOp(Box::new(UseOpData {
                    loc: lower.loc.dupe(),
                    message: Message::MessageReadonlyArraysCannotBeWrittenTo,
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::EIncompatibleWithExact((lower, upper), use_op, kind) => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatibleWithExact { kind, lower, upper },
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::EFunctionIncompatibleWithIndexer((lower, upper), use_op) => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatibleWithIndexed { lower, upper },
                    use_op,
                    explanation: None,
                }))
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
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatibleNonTypeGuardToTypeGuard { lower, upper },
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETypeGuardInvalidParameter(box ETypeGuardInvalidParameterData {
                type_guard_reason,
                binding_reason,
            }) => Normal(Message::MessageCannotReferenceTypeGuardParameter {
                type_guard_reason,
                binding_reason,
            }),

            ErrorMessage::ETypeGuardIndexMismatch {
                use_op,
                reasons: (lower, upper),
            } => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageTypeGuardIndexMismatch { lower, upper },
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETypeGuardImpliesMismatch {
                use_op,
                reasons: (lower, upper),
            } => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageTypeGuardImpliesMismatch { lower, upper },
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETypeGuardParamUnbound(reason) => {
                Normal(Message::MessageInvalidTypeGuardParamUnbound(reason))
            }

            ErrorMessage::ETypeGuardThisParam(reason) => {
                Normal(Message::MessageInvalidTypeGuardThisParam(reason))
            }

            ErrorMessage::ETypeGuardFunctionInvalidWrites(
                box ETypeGuardFunctionInvalidWritesData {
                    type_guard_reason,
                    write_locs,
                    ..
                },
            ) => Normal(Message::MessageInvalidTypeGuardFunctionWritten {
                type_guard_reason,
                write_locs,
            }),

            ErrorMessage::ENegativeTypeGuardConsistency(
                box ENegativeTypeGuardConsistencyData {
                    reason,
                    return_reason,
                    type_reason,
                },
            ) => Normal(Message::MessageNegativeTypeGuardConsistency {
                reason,
                return_reason,
                type_reason,
            }),

            ErrorMessage::ETypeParamConstIncompatibility(
                box ETypeParamConstIncompatibilityData {
                    use_op,
                    lower,
                    upper,
                },
            ) => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatiblETypeParamConstIncompatibility {
                        lower,
                        upper,
                    },
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::ETypeParamConstInvalidPosition(reason) => {
                Normal(Message::MessageTypeParamConstInvalidPosition(reason))
            }

            ErrorMessage::ETypeGuardFunctionParamHavoced(
                box ETypeGuardFunctionParamHavocedData {
                    type_guard_reason,
                    param_reason,
                    call_locs,
                },
            ) => Normal(Message::MessageCannotUseTypeGuardWithFunctionParamHavoced(
                Box::new(MessageCannotUseTypeGuardWithFunctionParamHavocedData {
                    type_guard_desc: type_guard_reason.desc.clone(),
                    param_reason,
                    call_locs,
                }),
            )),

            ErrorMessage::ETypeGuardIncompatibleWithFunctionKind(
                box ETypeGuardIncompatibleWithFunctionKindData { kind, .. },
            ) => Normal(Message::MessageInvalidTypeGuardFunctionKind(kind)),

            ErrorMessage::EInternal(box (_, internal_error)) => {
                let msg = format!(
                    "Internal error: {}",
                    string_of_internal_error(&internal_error)
                );
                Normal(Message::MessagePlainTextReservedForInternalErrorOnly(
                    msg.into(),
                ))
            }

            ErrorMessage::EUnsupportedSyntax(box (_, unsupported_syntax)) => {
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

            ErrorMessage::EBindingError(box (binding_error, _, x, entry_loc)) => {
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
                        Message::MessageCannotUseTypeInValuePosition(Box::new(
                            MessageCannotUseTypeInValuePositionData {
                                reason: x_reason,
                                type_only_namespace,
                                imported_name,
                            },
                        ))
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

            ErrorMessage::EBadExportContext(box (name, _)) => Normal(
                Message::MessageCannotUseExportInNonLegalToplevelContext(name),
            ),

            ErrorMessage::EBadDefaultImportAccess(box (_, import_star_reason)) => {
                Normal(Message::MessageInvalidImportStarUse(import_star_reason))
            }

            ErrorMessage::EInvalidImportStarUse(box (_, import_star_reason)) => {
                Normal(Message::MessageCannotUseImportStar(import_star_reason))
            }

            ErrorMessage::ENonConstVarExport(box (_, decl_reason)) => {
                Normal(Message::MessageNonConstVarExport(decl_reason))
            }

            ErrorMessage::EMixedImportAndRequire(box (_, import_reason)) => Normal(
                Message::MessageCannotUseMixedImportAndRequire(import_reason),
            ),

            ErrorMessage::EUnsupportedVarianceAnnotation(box (_, kind)) => {
                Normal(Message::MessageUnsupportedVarianceAnnotation(kind))
            }

            ErrorMessage::EExportRenamedDefault(box EExportRenamedDefaultData {
                name,
                is_reexport,
                ..
            }) => Normal(Message::MessageCannotExportRenamedDefault(Box::new(
                MessageCannotExportRenamedDefaultData { name, is_reexport },
            ))),

            ErrorMessage::ECannotDelete(box (_, expr)) => {
                Normal(Message::MessageCannotDelete(expr))
            }

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
                    BV::InterfaceMergePropertyConflict {
                        name,
                        current_binding_loc,
                        ..
                    } => {
                        let x = VirtualReason::new(
                            flow_common::reason::VirtualReasonDesc::RIdentifier(Name::new(name)),
                            current_binding_loc,
                        );
                        Message::MessageInterfaceMergePropertyConflict(x)
                    }
                    BV::InterfaceMergeTparamMismatch {
                        name,
                        current_binding_loc,
                        ..
                    } => {
                        let x = VirtualReason::new(
                            flow_common::reason::VirtualReasonDesc::RIdentifier(Name::new(name)),
                            current_binding_loc,
                        );
                        Message::MessageInterfaceMergeTparamMismatch(x)
                    }
                };
                Normal(msg)
            }

            ErrorMessage::ESignatureVerification(sve) => {
                Normal(Message::MessageCannotBuildTypedInterface(sve))
            }

            ErrorMessage::EInvalidObjectKit(box EInvalidObjectKitData {
                reason, use_op, ..
            }) => {
                let loc = reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageLowerIsNotObject(reason),
                    explanation: None,
                    use_op,
                }))
            }

            ErrorMessage::EInvalidTypeof(box (_, typename)) => {
                Normal(Message::MessageInvalidGenericRef(typename))
            }

            ErrorMessage::EObjectComputedPropertyAccess(
                box EObjectComputedPropertyAccessData {
                    reason_obj,
                    reason_prop,
                    kind,
                },
            ) => Normal(Message::MessageCannotAccessObjectWithComputedProp {
                reason_obj,
                reason_prop,
                kind,
            }),

            ErrorMessage::EObjectComputedPropertyAssign(box (
                reason_prop,
                Some(reason_key),
                kind,
            )) => Normal(
                Message::MessageCannotAssignToObjectWithComputedPropWithKey {
                    reason_prop,
                    reason_key,
                    kind,
                },
            ),

            ErrorMessage::EObjectComputedPropertyAssign(box (reason_prop, None, _)) => Normal(
                Message::MessageCannotAssignToObjectWithComputedProp(reason_prop),
            ),

            ErrorMessage::EObjectComputedPropertyPotentialOverwrite(
                box EObjectComputedPropertyPotentialOverwriteData {
                    key_loc,
                    overwritten_locs,
                },
            ) => Normal(
                Message::MessageCannotAddComputedPropertyDueToPotentialOverwrite(Box::new(
                    MessageCannotAddComputedPropertyDueToPotentialOverwriteData {
                        key_loc,
                        overwritten_locs,
                    },
                )),
            ),

            ErrorMessage::EUnsupportedImplements(reason) => Normal(
                Message::MessageCannotImplementNonInterface(reason.desc.clone()),
            ),

            ErrorMessage::ENotAReactComponent { reason, use_op } => {
                let loc = reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageLowerIsNotReactComponent(reason),
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::EReactElementFunArity(box (_, fn_name, n)) => {
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

            ErrorMessage::EDuplicateModuleProvider(box EDuplicateModuleProviderData {
                module_name,
                provider,
                conflict,
            }) => Normal(Message::MessageDuplicateModuleProvider(Box::new(
                MessageDuplicateModuleProviderData {
                    module_name,
                    provider,
                    conflict,
                },
            ))),

            ErrorMessage::EParseError(box (_, parse_error)) => {
                Normal(Message::MessageParseError(parse_error))
            }

            ErrorMessage::EDocblockError(box (_, err)) => {
                Normal(Message::MessageDocblockError(err))
            }

            ErrorMessage::EUntypedTypeImport(box (_, module_name)) => Normal(
                Message::MessageUntypedTypeImport(module_name.display().to_string().into()),
            ),

            ErrorMessage::EUntypedImport(box (_, module_name)) => Normal(
                Message::MessageUntypedImport(module_name.display().to_string().into()),
            ),

            ErrorMessage::EInternalType(_, kind) => Normal(Message::MessageInternalType(kind)),

            ErrorMessage::EIncorrectTypeWithReplacement(
                box EIncorrectTypeWithReplacementData { kind, .. },
            ) => Normal(Message::MessageIncorrectType(kind)),

            ErrorMessage::ELintSetting(box (_, kind)) => {
                Normal(Message::MessageInvalidLintSettings(kind))
            }

            ErrorMessage::ESketchyNullLint(box ESketchyNullLintData {
                kind,
                falsy_loc,
                null_loc,
                ..
            }) => Normal(Message::MessageSketchyNullCheck(Box::new(
                MessageSketchyNullCheckData {
                    kind,
                    falsy_loc,
                    null_loc,
                },
            ))),

            ErrorMessage::ESketchyNumberLint(_, reason) => {
                Normal(Message::MessageSketchyNumber(reason))
            }

            ErrorMessage::EPrimitiveAsInterface(box EPrimitiveAsInterfaceData {
                use_op,
                reason,
                interface_reason,
                kind,
            }) => {
                let loc = reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageCannotUsePrimitiveAsInterface {
                        reason,
                        interface_reason,
                        kind,
                    },
                    explanation: None,
                    use_op,
                }))
            }

            ErrorMessage::ECannotSpreadInterface(box ECannotSpreadInterfaceData {
                spread_reason,
                interface_reason,
                use_op,
            }) => {
                let loc = spread_reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageCannotSpreadInterface {
                        spread_reason,
                        interface_reason,
                    },
                    explanation: None,
                    use_op,
                }))
            }

            ErrorMessage::ECannotSpreadIndexerOnRight(box ECannotSpreadIndexerOnRightData {
                spread_reason,
                object_reason,
                key_reason,
                use_op,
            }) => {
                let loc = spread_reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageCannotSpreadDueToPotentialOverwrite {
                        spread_reason,
                        object_reason,
                        key_reason,
                    },
                    explanation: None,
                    use_op,
                }))
            }

            ErrorMessage::EUnableToSpread(box EUnableToSpreadData {
                spread_reason,
                object1_reason,
                object2_reason,
                propname,
                error_kind,
                use_op,
            }) => {
                let loc = spread_reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageCannotSpreadGeneral(Box::new(
                        MessageCannotSpreadGeneralData {
                            spread_reason,
                            object1_reason,
                            object2_reason,
                            propname,
                            error_kind,
                        },
                    )),
                    explanation: None,
                    use_op,
                }))
            }

            ErrorMessage::EInexactMayOverwriteIndexer(box EInexactMayOverwriteIndexerData {
                spread_reason,
                key_reason,
                value_reason,
                object2_reason,
                use_op,
            }) => {
                let loc = spread_reason.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageCannotSpreadInexactMayOverwriteIndexer(Box::new(
                        MessageCannotSpreadInexactMayOverwriteIndexerData {
                            spread_reason,
                            key_reason,
                            value_reason,
                            object2_reason,
                        },
                    )),
                    explanation: None,
                    use_op,
                }))
            }

            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectUtilType(
                box EnumInvalidObjectUtilTypeData {
                    reason,
                    enum_reason,
                },
            )) => Normal(Message::MessageCannotInstantiateObjectUtilTypeWithEnum(
                Box::new(MessageCannotInstantiateObjectUtilTypeWithEnumData {
                    description: reason.desc.clone(),
                    enum_reason,
                }),
            )),

            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectFunction(
                box EnumInvalidObjectFunctionData {
                    reason,
                    enum_reason,
                },
            )) => Normal(Message::MessageCannotCallObjectFunctionOnEnum {
                reason,
                enum_reason,
            }),

            ErrorMessage::EAssignConstLikeBinding(box EAssignConstLikeBindingData {
                definition,
                binding_kind,
                ..
            }) => Normal(Message::MessageCannotReassignConstantLikeBinding {
                definition,
                binding_kind,
            }),

            ErrorMessage::EObjectThisSuperReference(box (_, reason, k)) => {
                let converted_kind = match k {
                    ThisFinderKind::This => crate::intermediate_error_types::ThisFinderKind::This,
                    ThisFinderKind::Super => crate::intermediate_error_types::ThisFinderKind::Super,
                };
                Normal(Message::MessageThisSuperInObject(reason, converted_kind))
            }

            ErrorMessage::EComponentThisReference(box EComponentThisReferenceData {
                component_loc,
                ..
            }) => Normal(Message::MessageThisInComponent(component_loc)),

            ErrorMessage::EImplicitInstantiationUnderconstrainedError(
                box EImplicitInstantiationUnderconstrainedErrorData {
                    reason_call,
                    reason_tparam,
                    use_op,
                    ..
                },
            ) => {
                let loc = reason_call.loc.dupe();
                UseOp(Box::new(UseOpData {
                    use_op,
                    message: Message::MessageUnderconstrainedImplicitInstantiaton {
                        reason_call,
                        reason_tparam,
                    },
                    loc,
                    explanation: None,
                }))
            }

            ErrorMessage::EClassToObject(box EClassToObjectData {
                reason_class,
                reason_obj,
                use_op,
                kind,
            }) => {
                let loc = reason_class.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatibleClassToObject {
                        reason_class,
                        reason_obj,
                        kind,
                    },
                    explanation: None,
                    use_op,
                }))
            }

            ErrorMessage::EMethodUnbinding(box EMethodUnbindingData {
                use_op,
                reason_op,
                reason_prop,
            }) => {
                let loc = reason_op.loc.dupe();
                let context_loc = reason_prop
                    .def_loc_opt
                    .as_ref()
                    .unwrap_or(&reason_prop.loc)
                    .dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageMethodUnbinding {
                        reason_op,
                        context_loc,
                    },
                    use_op,
                    explanation: None,
                }))
            }

            ErrorMessage::EHookIncompatible(box EHookIncompatibleData {
                use_op,
                lower,
                upper,
                lower_is_hook,
                hook_is_annot,
            }) => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
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
                }))
            }

            ErrorMessage::EHookUniqueIncompatible(box EHookUniqueIncompatibleData {
                use_op,
                lower,
                upper,
            }) => {
                let loc = lower.loc.dupe();
                UseOp(Box::new(UseOpData {
                    loc,
                    message: Message::MessageIncompatibleReactHooksDueToUniqueness { lower, upper },
                    use_op,
                    explanation: Some(Explanation::ExplanationReactHookIncompatibleWithEachOther),
                }))
            }

            ErrorMessage::EHookRuleViolation(box EHookRuleViolationData {
                callee_loc,
                hook_rule,
                ..
            }) => {
                use crate::error_message::HookRule;
                match hook_rule {
                    HookRule::ConditionalHook => {
                        Normal(Message::MessageCannotCallReactHookConditionally(callee_loc))
                    }
                    HookRule::HookHasIllegalName => {
                        Normal(Message::MessageCannotCallReactHookWithIllegalName(callee_loc))
                    }
                    HookRule::MaybeHook { hooks, non_hooks } => {
                        Normal(Message::MessageCannotCallMaybeReactHook(Box::new(MessageCannotCallMaybeReactHookData {
                            callee_loc,
                            hooks,
                            non_hooks,
                        })))
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

            ErrorMessage::EInvalidGraphQL(box (_, err)) => {
                Normal(Message::MessageInvalidGraphQL(err))
            }

            ErrorMessage::EReactIntrinsicOverlap(box EReactIntrinsicOverlapData {
                use_loc: use_reason,
                def,
                type_,
                mixed,
            }) => Normal(Message::MessageReactIntrinsicOverlap(Box::new(
                MessageReactIntrinsicOverlapData {
                    use_: use_reason,
                    def,
                    type_,
                    mixed,
                },
            ))),

            ErrorMessage::EInvalidBinaryArith(box EInvalidBinaryArithData {
                reason_l,
                reason_r,
                kind,
                ..
            }) => Normal(Message::MessageCannotPerformBinaryArith {
                kind,
                reason_l,
                reason_r,
            }),

            ErrorMessage::EMissingPlatformSupportWithAvailablePlatforms(
                box EMissingPlatformSupportWithAvailablePlatformsData {
                    available_platforms,
                    required_platforms,
                    ..
                },
            ) => Normal(
                Message::MessageMissingPlatformSupportWithAvailablePlatforms(Box::new(
                    MessageMissingPlatformSupportWithAvailablePlatformsData {
                        available_platforms: available_platforms.clone(),
                        required_platforms: required_platforms.clone(),
                    },
                )),
            ),

            ErrorMessage::EMissingPlatformSupport(box EMissingPlatformSupportData {
                missing_platforms,
                ..
            }) => Normal(Message::MessageMissingPlatformSupport {
                missing_platforms: missing_platforms.clone(),
            }),

            ErrorMessage::EUnionPartialOptimizationNonUniqueKey(
                box EUnionPartialOptimizationNonUniqueKeyData {
                    non_unique_keys, ..
                },
            ) => Normal(Message::MessageCannotOptimizeUnionDueToNonUniqueKeys(
                non_unique_keys,
            )),

            ErrorMessage::EUnionOptimization(box EUnionOptimizationData { kind, .. }) => {
                Normal(Message::MessageCannotOptimizeUnionInternally(kind))
            }

            ErrorMessage::EUnionOptimizationOnNonUnion(box EUnionOptimizationOnNonUnionData {
                arg,
                ..
            }) => Normal(Message::MessageInvalidUseOfFlowEnforceOptimized(arg)),

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
                        } => Explanation::ExplanationInvariantSubtypingDueToMutableArray(Box::new(ExplanationInvariantSubtypingDueToMutableArrayData {
                            lower_array_loc: lower_array_loc.dupe(),
                            upper_array_loc: upper_array_loc.dupe(),
                            lower_array_desc: expect_type_desc(lower_array_desc.clone()),
                            upper_array_desc: expect_type_desc(upper_array_desc.clone()),
                            upper_array_reason: upper_array_reason.dupe(),
                        })),
                        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
                            lower_obj_loc,
                            upper_obj_loc,
                            lower_obj_desc,
                            upper_obj_desc,
                            upper_object_reason,
                            property_name,
                        } => Explanation::ExplanationInvariantSubtypingDueToMutableProperty(Box::new(ExplanationInvariantSubtypingDueToMutablePropertyData {
                            lower_obj_loc: lower_obj_loc.dupe(),
                            upper_obj_loc: upper_obj_loc.dupe(),
                            lower_obj_desc: expect_type_desc(lower_obj_desc.clone()),
                            upper_obj_desc: expect_type_desc(upper_obj_desc.clone()),
                            upper_object_reason: upper_object_reason.dupe(),
                            property_name: property_name.dupe(),
                        })),
                        ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperties {
                            lower_obj_loc,
                            upper_obj_loc,
                            lower_obj_desc,
                            upper_obj_desc,
                            upper_object_reason,
                            properties,
                        } => Explanation::ExplanationInvariantSubtypingDueToMutableProperties(Box::new(ExplanationInvariantSubtypingDueToMutablePropertiesData {
                            lower_obj_loc: lower_obj_loc.dupe(),
                            upper_obj_loc: upper_obj_loc.dupe(),
                            lower_obj_desc: expect_type_desc(lower_obj_desc.clone()),
                            upper_obj_desc: expect_type_desc(upper_obj_desc.clone()),
                            upper_object_reason: upper_object_reason.dupe(),
                            properties: properties.clone(),
                        })),
                    }
                });
                IncompatibleInvariantSubtyping(Box::new(IncompatibleInvariantSubtypingData {
                    sub_component,
                    lower_loc,
                    upper_loc,
                    lower_desc: expect_type_desc(lower_desc),
                    upper_desc: expect_type_desc(upper_desc),
                    use_op,
                    explanation,
                }))
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
            ) => PropsMissingInInvariantSubtyping(Box::new(PropsMissingInInvariantSubtypingData {
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
            })),

            ErrorMessage::ETSSyntax(box ETSSyntaxData { kind, .. }) => {
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

            ErrorMessage::EVarianceKeyword(box EVarianceKeywordData { kind, .. }) => {
                use crate::intermediate_error_types::Message;
                use crate::intermediate_error_types::VarianceSigilKind;
                let msg = match kind {
                    VarianceKeywordKind::Writeonly => Message::MessageVarianceKeywordWriteonly,
                    VarianceKeywordKind::Plus => {
                        Message::MessageDeprecatedVarianceSigil(VarianceSigilKind::Plus)
                    }
                    VarianceKeywordKind::Minus => {
                        Message::MessageDeprecatedVarianceSigil(VarianceSigilKind::Minus)
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
            Self::EUntypedTypeImport(box (_, _))
            | Self::EMethodUnbinding(box EMethodUnbindingData { .. })
            | Self::EUntypedImport(box (_, _))
            | Self::ENonstrictImport(_)
            | Self::EUnclearType(_)
            | Self::EDeprecatedBool(_)
            | Self::EInternalType(_, _)
            | Self::EUnsafeGettersSetters(_)
            | Self::EUnsafeObjectAssign(_)
            | Self::ESketchyNullLint(box ESketchyNullLintData { .. })
            | Self::ESketchyNumberLint(_, _)
            | Self::EUnnecessaryOptionalChain(box (_, _))
            | Self::EUnnecessaryInvariant(box (_, _))
            | Self::EUnnecessaryDeclareTypeOnlyExport(_)
            | Self::EImplicitInexactObject(_)
            | Self::EAmbiguousObjectType(_)
            | Self::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck(
                box MatchNonExplicitEnumCheckData { .. },
            ))
            | Self::EUninitializedInstanceProperty(_, _)
            | Self::ETrivialRecursiveDefinition(box (_, _))
            | Self::EAnyValueUsedAsType { .. }
            | Self::EValueUsedAsType { .. }
            | Self::EUnusedPromise { .. }
            | Self::EImplicitInstantiationUnderconstrainedError(
                box EImplicitInstantiationUnderconstrainedErrorData { .. },
            )
            | Self::EDuplicateComponentProp(box EDuplicateComponentPropData { .. })
            | Self::ERefComponentProp(box ERefComponentPropData { .. })
            | Self::EConstantCondition(box EConstantConditionData { .. })
            | Self::EBuiltinNameLookupFailed(box EBuiltinNameLookupFailedData { .. }) => true,

            Self::EBindingError(box (BindingError::EReferencedBeforeDeclaration, _, _, _)) => true,

            Self::EEnumError(EnumErrorKind::EnumNotAllChecked(box EnumNotAllCheckedData {
                default_case_loc: Some(_),
                ..
            })) => true,

            Self::EPropNotReadable(box EPropNotReadableData { use_op, .. }) => {
                matches!(
                    use_op,
                    VirtualUseOp::Frame(frame, _) if matches!(frame.as_ref(), VirtualFrameUseOp::ReactDeepReadOnly(..))
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
                VirtualFrameUseOp::ReactDeepReadOnly(box (_, DroType::HookReturn)) => {
                    Some(ReactRuleHookMutation)
                }
                VirtualFrameUseOp::ReactDeepReadOnly(box (
                    _,
                    DroType::Props | DroType::HookArg,
                )) => Some(ReactRuleUnsafeMutation),
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
                | VirtualRootUseOp::FunCall(..)
                | VirtualRootUseOp::FunCallMethod(..)
                | VirtualRootUseOp::FunReturnStatement { .. }
                | VirtualRootUseOp::FunImplicitReturn(..) => Some(IncompatibleType),
                VirtualRootUseOp::TypeGuardIncompatibility { .. }
                | VirtualRootUseOp::PositiveTypeGuardConsistency(..) => Some(IncompatibleTypeGuard),
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
                (_, VirtualFrameUseOp::TypeArgCompatibility(..))
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

            // | EInvalidBinaryArith(Box::new(EInvalidBinaryArithData { kind = (_, op); _ })) -> begin
            //     match op with
            //     | Type.ArithKind.Plus -> Some UnsafeAddition
            //     | _ -> Some UnsafeArith
            //   end
            ErrorMessage::EInvalidBinaryArith(box EInvalidBinaryArithData { kind, .. }) => {
                match kind.1 {
                    flow_typing_type::type_::arith_kind::ArithKindInner::Plus => {
                        Some(UnsafeAddition)
                    }
                    _ => Some(UnsafeArith),
                }
            }

            ErrorMessage::EAssignConstLikeBinding(box EAssignConstLikeBindingData { .. }) => {
                Some(CannotReassignConstLike)
            }
            ErrorMessage::EBadExportContext(box (_, _)) => Some(InvalidExport),
            ErrorMessage::EBadExportPosition(_) => Some(InvalidExport),
            ErrorMessage::EBadDefaultImportAccess { .. } => Some(DefaultImportAccess),
            ErrorMessage::EBadDefaultImportDestructuring(_) => Some(DefaultImportAccess),
            ErrorMessage::EInvalidImportStarUse { .. } => Some(InvalidImportStarUse),
            ErrorMessage::EBinaryInLHS { .. } => Some(InvalidInLhs),
            ErrorMessage::EBinaryInRHS { .. } => Some(InvalidInRhs),

            ErrorMessage::EBindingError(box (binding_error, _, _, _)) => match binding_error {
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

            ErrorMessage::EBuiltinNameLookupFailed(box EBuiltinNameLookupFailedData { .. }) => {
                Some(CannotResolveName)
            }
            ErrorMessage::EBuiltinModuleLookupFailed(box EBuiltinModuleLookupFailedData {
                ..
            }) => Some(CannotResolveModule),
            ErrorMessage::EExpectedModuleLookupFailed(box EExpectedModuleLookupFailedData {
                ..
            }) => Some(CannotResolveModule),
            ErrorMessage::EPlatformSpecificImplementationModuleLookupFailed(
                box EPlatformSpecificImplementationModuleLookupFailedData { .. },
            ) => Some(CannotResolveModule),
            ErrorMessage::ECallTypeArity(box ECallTypeArityData { .. }) => {
                Some(NonpolymorphicTypeArg)
            }
            ErrorMessage::ECannotDelete { .. } => Some(CannotDelete),
            ErrorMessage::ECannotSpreadIndexerOnRight(box ECannotSpreadIndexerOnRightData {
                ..
            }) => Some(CannotSpreadIndexer),
            ErrorMessage::ECannotSpreadInterface(box ECannotSpreadInterfaceData { .. }) => {
                Some(CannotSpreadInterface)
            }
            ErrorMessage::ECodelessSuppression(_) => None,
            ErrorMessage::ENonStrictEqualityComparison { .. }
            | ErrorMessage::EComparison(box EComparisonData { .. }) => Some(InvalidCompare),

            ErrorMessage::EComputedPropertyWithUnion { .. } => Some(InvalidComputedProp),
            ErrorMessage::EDevOnlyRefinedLocInfo(box EDevOnlyRefinedLocInfoData { .. }) => None,
            ErrorMessage::EDevOnlyInvalidatedRefinementInfo(
                box EDevOnlyInvalidatedRefinementInfoData { .. },
            ) => None,
            ErrorMessage::ETemporaryHardcodedErrorForPrototyping(box (_, _)) => None,
            ErrorMessage::EIncorrectTypeWithReplacement(
                box EIncorrectTypeWithReplacementData { kind, .. },
            ) => match kind.error_type_of_kind() {
                IncorrectTypeErrorType::DeprecatedUtility => Some(DeprecatedUtility),
                IncorrectTypeErrorType::TSType => Some(UnsupportedSyntax),
            },
            ErrorMessage::EDocblockError(box (_, err)) => match err {
                DocblockError::MultipleFlowAttributes => Some(DuplicateFlowDecl),
                DocblockError::InvalidFlowMode(_) => Some(InvalidFlowModeDecl),
                DocblockError::MultipleJSXAttributes => Some(DuplicateJsxDecl),
                DocblockError::InvalidJSXAttribute(_) => Some(InvalidJsxDecl),
                DocblockError::MultipleJSXRuntimeAttributes => Some(DuplicateJsxRuntimeDecl),
                DocblockError::InvalidJSXRuntimeAttribute => Some(InvalidJsxRuntimeDecl),
                DocblockError::InvalidSupportsPlatform(_) => Some(InvalidSupportsPlatformDecl),
                DocblockError::DisallowedSupportsPlatform => Some(InvalidSupportsPlatformDecl),
            },
            ErrorMessage::EDuplicateModuleProvider(box EDuplicateModuleProviderData { .. }) => {
                Some(DuplicateModule)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumAllMembersAlreadyChecked(
                box EnumAllMembersAlreadyCheckedData { .. },
            )) => Some(InvalidExhaustiveCheck),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidAbstractUse(
                box EnumInvalidAbstractUseData { .. },
            )) => Some(InvalidExhaustiveCheck),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberName(
                box EnumInvalidMemberNameData { .. },
            )) => Some(InvalidEnumMemberName),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNonIdentifierMemberName(
                box EnumNonIdentifierMemberNameData { .. },
            )) => Some(InvalidEnumMemberName),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidCheck(
                box EnumInvalidCheckData { from_match, .. },
            )) => {
                if *from_match {
                    Some(MatchInvalidPattern)
                } else {
                    Some(InvalidExhaustiveCheck)
                }
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberAccess(
                box EnumInvalidMemberAccessData { .. },
            )) => Some(InvalidEnumAccess),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectUtilType(
                box EnumInvalidObjectUtilTypeData { .. },
            )) => Some(NotAnObject),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectFunction(
                box EnumInvalidObjectFunctionData { .. },
            )) => Some(NotAnObject),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotIterable { .. }) => Some(NotIterable),
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberAlreadyChecked(
                box EnumMemberAlreadyCheckedData { .. },
            )) => Some(InvalidExhaustiveCheck),
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberDuplicateValue(
                box EnumMemberDuplicateValueData { .. },
            )) => Some(DuplicateEnumInit),
            ErrorMessage::EEnumError(EnumErrorKind::EnumMemberUsedAsType(
                box EnumMemberUsedAsTypeData { .. },
            )) => Some(EnumValueAsType),
            ErrorMessage::EEnumError(EnumErrorKind::EnumModification(
                box EnumModificationData { .. },
            )) => Some(CannotWriteEnum),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked(
                box EnumNotAllCheckedData {
                    default_case_loc: None,
                    ..
                },
            )) => Some(InvalidExhaustiveCheck),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNotAllChecked(
                box EnumNotAllCheckedData {
                    default_case_loc: Some(_),
                    ..
                },
            )) => Some(RequireExplicitEnumSwitchCases),
            ErrorMessage::EEnumError(EnumErrorKind::EnumUnknownNotChecked(
                box EnumUnknownNotCheckedData { .. },
            )) => Some(InvalidExhaustiveCheck),
            ErrorMessage::EExpectedBooleanLit(box EExpectedBooleanLitData { use_op, .. }) => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EExpectedNumberLit(box EExpectedNumberLitData { use_op, .. }) => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EExpectedStringLit(box EExpectedStringLitData { use_op, .. }) => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EExpectedBigIntLit(box EExpectedBigIntLitData { use_op, .. }) => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumsNotEnabled { .. }) => {
                Some(UnsupportedSyntax)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumConstNotSupported { .. }) => {
                Some(UnsupportedSyntax)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumDuplicateMemberName(
                box EnumDuplicateMemberNameData { .. },
            )) => Some(InvalidEnum),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInconsistentMemberValues(
                box EnumInconsistentMemberValuesData { .. },
            )) => Some(InvalidEnum),
            ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberInitializer(
                box EnumInvalidMemberInitializerData { .. },
            )) => Some(InvalidEnum),
            ErrorMessage::EEnumError(EnumErrorKind::EnumBooleanMemberNotInitialized(
                box EnumBooleanMemberNotInitializedData { .. },
            )) => Some(InvalidEnum),
            ErrorMessage::EEnumError(EnumErrorKind::EnumNumberMemberNotInitialized(
                box EnumNumberMemberNotInitializedData { .. },
            )) => Some(InvalidEnum),
            ErrorMessage::EEnumError(EnumErrorKind::EnumBigIntMemberNotInitialized(
                box EnumBigIntMemberNotInitializedData { .. },
            )) => Some(InvalidEnum),
            ErrorMessage::EEnumError(EnumErrorKind::EnumStringMemberInconsistentlyInitialized(
                box EnumStringMemberInconsistentlyInitializedData { .. },
            )) => Some(InvalidEnum),
            ErrorMessage::EExponentialSpread(box EExponentialSpreadData { .. }) => {
                Some(ExponentialSpread)
            }
            ErrorMessage::EExportsAnnot { .. } => Some(InvalidExportsTypeArg),
            ErrorMessage::EExportValueAsType(box (_, _)) => Some(ExportValueAsType),
            ErrorMessage::EForInRHS { .. } => Some(InvalidInRhs),
            ErrorMessage::EInstanceofRHS { .. } => Some(InvalidInRhs),
            ErrorMessage::EFunctionCallExtraArg { .. } => Some(ExtraArg),
            ErrorMessage::ETypeGuardFuncIncompatibility { .. }
            | ErrorMessage::ETypeGuardInvalidParameter(box ETypeGuardInvalidParameterData {
                ..
            })
            | ErrorMessage::ETypeGuardIndexMismatch { .. }
            | ErrorMessage::ETypeGuardImpliesMismatch { .. }
            | ErrorMessage::ETypeGuardParamUnbound { .. }
            | ErrorMessage::ETypeGuardThisParam { .. }
            | ErrorMessage::ETypeGuardFunctionInvalidWrites(
                box ETypeGuardFunctionInvalidWritesData { .. },
            )
            | ErrorMessage::ETypeGuardFunctionParamHavoced(
                box ETypeGuardFunctionParamHavocedData { .. },
            )
            | ErrorMessage::ETypeGuardIncompatibleWithFunctionKind(
                box ETypeGuardIncompatibleWithFunctionKindData { .. },
            ) => Some(FunctionPredicate),
            ErrorMessage::ENegativeTypeGuardConsistency(
                box ENegativeTypeGuardConsistencyData { .. },
            ) => Some(IncompatibleTypeGuard),
            ErrorMessage::ETypeParamConstIncompatibility(
                box ETypeParamConstIncompatibilityData { .. },
            )
            | ErrorMessage::ETypeParamConstInvalidPosition { .. } => Some(TypeParamConstCode),
            ErrorMessage::EImportTypeAsTypeof(box (_, _)) => Some(InvalidImportType),
            ErrorMessage::EImportTypeAsValue(box (_, _)) => Some(ImportTypeAsValue),
            ErrorMessage::EImportValueAsType(box (_, _)) => Some(ImportValueAsType),
            ErrorMessage::EIncompatible(box EIncompatibleData {
                upper: (_, upper_kind),
                ..
            }) => Self::error_code_of_upper_kind(upper_kind),
            ErrorMessage::EIncompatibleSpeculation(box EIncompatibleSpeculationData {
                use_op: Some(use_op),
                ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleUse),
            ErrorMessage::EIncompatibleSpeculation(..) => Some(IncompatibleUse),
            ErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData { use_op, .. }) => {
                Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EIncompatibleProp(box EIncompatiblePropData {
                use_op: Some(use_op),
                ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EIncompatibleProp(box EIncompatiblePropData { use_op: None, .. }) => {
                Some(IncompatibleType)
            }
            ErrorMessage::EIncompatibleWithExact(_, _, ExactnessErrorKind::UnexpectedInexact) => {
                Some(IncompatibleExact)
            }
            ErrorMessage::EIncompatibleWithExact(_, _, ExactnessErrorKind::UnexpectedIndexer) => {
                Some(IncompatibleIndexer)
            }
            ErrorMessage::EFunctionIncompatibleWithIndexer { .. } => {
                Some(IncompatibleFunctionIndexer)
            }
            ErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible(
                box EnumIncompatibleData { use_op, .. },
            ))
            | ErrorMessage::EIncompatibleWithUseOp(box EIncompatibleWithUseOpData {
                use_op, ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
                use_op,
                ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EIndeterminateModuleType { .. } => Some(ModuleTypeConflict),
            ErrorMessage::EInexactMayOverwriteIndexer(box EInexactMayOverwriteIndexerData {
                ..
            }) => Some(CannotSpreadInexact),
            ErrorMessage::EInternal(box (_, _)) => None,
            ErrorMessage::EInvalidConstructor { .. } => Some(InvalidConstructor),
            ErrorMessage::EInvalidLHSInAssignment { .. } => Some(InvalidLhs),
            ErrorMessage::EInvalidObjectKit(box EInvalidObjectKitData { .. }) => Some(NotAnObject),
            ErrorMessage::EInvalidPrototype { .. } => Some(NotAnObject),
            ErrorMessage::EInvalidReactCreateElement(box EInvalidReactCreateElementData {
                ..
            }) => Some(InvalidReactCreateElement),
            ErrorMessage::EInvalidTypeArgs(box (_, _)) => Some(InvalidTypeArg),
            ErrorMessage::EInvalidTypeof { .. } => Some(IllegalTypeof),
            ErrorMessage::EInvalidInfer { .. } => Some(InvalidInfer),
            ErrorMessage::EConstantCondition(box EConstantConditionData { .. }) => {
                Some(ConstantCondition)
            }
            ErrorMessage::EInvalidExtends { .. } => Some(InvalidExtends),
            ErrorMessage::ELintSetting { .. } => Some(LintSetting),
            ErrorMessage::EMissingLocalAnnotation { reason, .. } => match &reason.desc {
                flow_common::reason::VirtualReasonDesc::RImplicitThis(_) => Some(MissingThisAnnot),
                _ => Some(MissingLocalAnnot),
            },
            ErrorMessage::EMissingTypeArgs(box EMissingTypeArgsData { .. }) => Some(MissingTypeArg),
            ErrorMessage::EMixedImportAndRequire { .. } => Some(MixedImportAndRequire),
            ErrorMessage::EUnsupportedVarianceAnnotation { .. } => {
                Some(UnsupportedVarianceAnnotation)
            }
            ErrorMessage::ENoDefaultExport(box (_, _, _)) => Some(MissingExport),
            ErrorMessage::ENoNamedExport(box (_, _, _, _)) => Some(MissingExport),
            ErrorMessage::ENonConstVarExport { .. } => Some(NonConstVarExport),
            ErrorMessage::ENonLitArrayToTuple { .. } => Some(InvalidTupleArity),
            ErrorMessage::ENotAReactComponent { .. } => Some(NotAComponent),
            ErrorMessage::EObjectComputedPropertyAccess(
                box EObjectComputedPropertyAccessData { .. },
            ) => Some(InvalidComputedProp),
            ErrorMessage::EObjectComputedPropertyAssign { .. } => Some(InvalidComputedProp),
            ErrorMessage::EObjectComputedPropertyPotentialOverwrite(
                box EObjectComputedPropertyPotentialOverwriteData { .. },
            ) => Some(InvalidComputedProp),
            ErrorMessage::EOnlyDefaultExport(box (_, _, _)) => Some(MissingExport),
            ErrorMessage::EParseError(box (_, _)) => None,
            ErrorMessage::EPolarityMismatch(box EPolarityMismatchData { .. }) => {
                Some(IncompatibleVariance)
            }
            ErrorMessage::EPrimitiveAsInterface(box EPrimitiveAsInterfaceData { .. }) => {
                Some(IncompatibleType)
            }
            ErrorMessage::EPrivateLookupFailed { .. } => Some(PropMissing),
            ErrorMessage::EStrUtilTypeNonLiteralArg { .. } => Some(InvalidTypeArg),
            ErrorMessage::EPropNotFoundInLookup(box EPropNotFoundInLookupData {
                use_op, ..
            }) => {
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
            ErrorMessage::EPropNotFoundInSubtyping(box EPropNotFoundInSubtypingData {
                use_op,
                ..
            })
            | ErrorMessage::EPropsNotFoundInSubtyping(box EPropsNotFoundInSubtypingData {
                use_op,
                ..
            })
            | ErrorMessage::EPropsExtraAgainstExactObject(
                box EPropsExtraAgainstExactObjectData { use_op, .. },
            ) => Self::react_rule_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EPropsNotFoundInInvariantSubtyping(
                box EPropsNotFoundInInvariantSubtypingData { use_op, .. },
            ) => Self::react_rule_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EIndexerCheckFailed(box EIndexerCheckFailedData { use_op, .. }) => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), IncompatibleType)
            }
            ErrorMessage::EPropNotReadable(box EPropNotReadableData { use_op, .. }) => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), CannotRead)
            }
            ErrorMessage::EPropNotWritable(box EPropNotWritableData { use_op, .. }) => {
                Self::react_rule_of_use_op(&Some(use_op.dupe()), CannotWrite)
            }
            ErrorMessage::EPropPolarityMismatch(box EPropPolarityMismatchData { .. }) => {
                Some(IncompatibleVariance)
            }
            ErrorMessage::EReactElementFunArity(box (_, _, _)) => Some(MissingArg),
            ErrorMessage::EReactRefInRender { .. } => Some(ReactRuleRef),
            ErrorMessage::ERecursionLimit(box (_, _)) => None,
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
                flow_type_sig::signature_error::BindingValidation::InterfaceMergePropertyConflict {
                    ..
                },
            )
            | ErrorMessage::ESignatureBindingValidation(
                flow_type_sig::signature_error::BindingValidation::InterfaceMergeTparamMismatch {
                    ..
                },
            ) => Some(LibdefOverride),
            ErrorMessage::ESignatureBindingValidation(
                flow_type_sig::signature_error::BindingValidation::NamespacedNameAlreadyBound {
                    ..
                },
            ) => Some(SignatureVerificationFailure),
            ErrorMessage::ESignatureVerification { .. } => Some(SignatureVerificationFailure),
            ErrorMessage::EThisInExportedFunction { .. } => Some(ThisInExportedFunction),
            ErrorMessage::EExportRenamedDefault(box EExportRenamedDefaultData { .. }) => {
                Some(ExportRenamedDefault)
            }
            ErrorMessage::ETooFewTypeArgs(box ETooFewTypeArgsData { .. }) => Some(MissingTypeArg),
            ErrorMessage::ETooManyTypeArgs(box ETooManyTypeArgsData { .. }) => Some(ExtraTypeArg),
            ErrorMessage::ETupleArityMismatch(box ETupleArityMismatchData { .. }) => {
                Some(InvalidTupleArity)
            }
            ErrorMessage::ETupleRequiredAfterOptional(box ETupleRequiredAfterOptionalData {
                ..
            }) => Some(TupleRequiredAfterOptional),
            ErrorMessage::ETupleInvalidTypeSpread(box ETupleInvalidTypeSpreadData { .. }) => {
                Some(TupleInvalidTypeSpread)
            }
            ErrorMessage::ETupleElementAfterInexactSpread { .. } => {
                Some(ElementAfterInexactTupleSpread)
            }
            ErrorMessage::ETupleElementNotReadable(box ETupleElementNotReadableData { .. }) => {
                Some(CannotRead)
            }
            ErrorMessage::ETupleElementNotWritable(box ETupleElementNotWritableData { .. }) => {
                Some(CannotWrite)
            }
            ErrorMessage::ETupleElementPolarityMismatch(
                box ETupleElementPolarityMismatchData { .. },
            ) => Some(IncompatibleVariance),
            ErrorMessage::ETupleNonIntegerIndex(box ETupleNonIntegerIndexData { .. }) => {
                Some(InvalidTupleIndex)
            }
            ErrorMessage::ETupleOutOfBounds(box ETupleOutOfBoundsData { .. }) => {
                Some(InvalidTupleIndex)
            }
            ErrorMessage::ETupleUnsafeWrite { .. } => Some(InvalidTupleIndex),
            ErrorMessage::ETypeParamArity(_, _) => Some(NonpolymorphicTypeApp),
            ErrorMessage::ETypeParamMinArity(_, _) => Some(MissingTypeArg),
            ErrorMessage::EUnableToSpread(box EUnableToSpreadData { error_kind, .. }) => {
                match error_kind {
                    ExactnessErrorKind::UnexpectedInexact => Some(CannotSpreadInexact),
                    ExactnessErrorKind::UnexpectedIndexer => Some(CannotSpreadIndexer),
                }
            }
            ErrorMessage::EUnexpectedThisType { .. } => Some(IllegalThis),
            ErrorMessage::EUnionSpeculationFailed(box EUnionSpeculationFailedData {
                use_op,
                ..
            }) => Self::error_code_of_use_op(&Some(use_op.dupe()), IncompatibleType),
            ErrorMessage::EUnreachable { .. } => Some(UnreachableCode),
            ErrorMessage::EUnsupportedExact(box (_, _)) => Some(InvalidExact),
            ErrorMessage::EUnsupportedImplements { .. } => Some(CannotImplement),
            ErrorMessage::EUnsupportedKeyInObject { .. } => Some(IllegalKey),
            ErrorMessage::EAmbiguousNumericKeyWithVariance { .. } => Some(IllegalKey),
            ErrorMessage::EUnsupportedSetProto { .. } => Some(CannotWrite),
            ErrorMessage::EUnsupportedSyntax(box (_, _)) => Some(UnsupportedSyntax),
            ErrorMessage::EImplicitInstantiationUnderconstrainedError(
                box EImplicitInstantiationUnderconstrainedErrorData { .. },
            ) => Some(UnderconstrainedImplicitInstantiation),
            ErrorMessage::EObjectThisSuperReference { .. } => Some(ObjectThisReference),
            ErrorMessage::EComponentThisReference(box EComponentThisReferenceData { .. }) => {
                Some(ComponentThisReference)
            }
            ErrorMessage::EComponentCase { .. } => Some(ComponentCase),
            ErrorMessage::EDeclareComponentInvalidParam { .. } => Some(InvalidComponentProp),
            ErrorMessage::EComponentMissingReturn { .. } => Some(ComponentMissingReturn),
            ErrorMessage::EComponentMissingBody { .. } => Some(ComponentMissingBody),
            ErrorMessage::EComponentBodyInAmbientContext { .. } => {
                Some(ComponentBodyInAmbientContext)
            }
            ErrorMessage::EInvalidDeclaration(box EInvalidDeclarationData { .. }) => {
                Some(InvalidDeclaration)
            }
            ErrorMessage::EInvalidMappedType { .. } => Some(InvalidMappedType),
            ErrorMessage::EDuplicateComponentProp(box EDuplicateComponentPropData { .. }) => {
                Some(InvalidComponentProp)
            }
            ErrorMessage::ERefComponentProp(box ERefComponentPropData { .. }) => {
                Some(InvalidComponentProp)
            }
            ErrorMessage::EKeySpreadProp(box EKeySpreadPropData { .. }) => Some(InvalidSpreadProp),
            ErrorMessage::EInvalidComponentRestParam { .. } => Some(InvalidComponentProp),
            ErrorMessage::EMalformedCode { .. } | ErrorMessage::EUnusedSuppression { .. } => None,
            ErrorMessage::EUseArrayLiteral { .. } => Some(IllegalNewArray),
            ErrorMessage::EAnyValueUsedAsType { .. } | ErrorMessage::EValueUsedAsType { .. } => {
                Some(ValueAsType)
            }
            ErrorMessage::EClassToObject(box EClassToObjectData { .. }) => Some(ClassObject),
            ErrorMessage::EMethodUnbinding(box EMethodUnbindingData { .. }) => {
                Some(MethodUnbinding)
            }
            ErrorMessage::EHookIncompatible(box EHookIncompatibleData { .. })
            | ErrorMessage::EHookUniqueIncompatible(box EHookUniqueIncompatibleData { .. }) => {
                Some(ReactRuleHookIncompatible)
            }
            ErrorMessage::EHookNaming { .. } => Some(ReactRuleHookNamingConvention),
            ErrorMessage::EHookRuleViolation(box EHookRuleViolationData {
                hook_rule: HookRule::ConditionalHook,
                ..
            }) => Some(ReactRuleHookConditional),
            ErrorMessage::EHookRuleViolation(box EHookRuleViolationData {
                hook_rule: HookRule::HookHasIllegalName,
                ..
            }) => Some(ReactRuleHookNamingConvention),
            ErrorMessage::EHookRuleViolation(box EHookRuleViolationData {
                hook_rule: HookRule::HookDefinitelyNotInComponentOrHook,
                ..
            }) => Some(ReactRuleHookDefinitelyNotInComponentOrHook),
            ErrorMessage::EHookRuleViolation(box EHookRuleViolationData {
                hook_rule: HookRule::MaybeHook { .. },
                ..
            }) => Some(ReactRuleHookMixedWithNonHoook),
            ErrorMessage::EHookRuleViolation(box EHookRuleViolationData {
                hook_rule: HookRule::NotHookSyntaxHook,
                ..
            }) => Some(ReactRuleHookNonHookSyntax),
            ErrorMessage::EHookRuleViolation(box EHookRuleViolationData {
                hook_rule:
                    HookRule::HookInUnknownContext
                    | HookRule::HookNotInComponentSyntaxComponentOrHookSyntaxHook,
                ..
            }) => Some(ReactRuleHook),
            ErrorMessage::EInvalidGraphQL { .. } => Some(InvalidGraphQL),
            ErrorMessage::EAnnotationInference { .. } => Some(InvalidExportedAnnotation),
            ErrorMessage::ETrivialRecursiveDefinition { .. } => Some(RecursiveDefinition),
            ErrorMessage::EDefinitionCycle { .. } => Some(DefinitionCycle),
            ErrorMessage::ERecursiveDefinition(box ERecursiveDefinitionData { .. }) => {
                Some(RecursiveDefinition)
            }
            ErrorMessage::EReferenceInAnnotation { .. } => Some(RecursiveDefinition),
            ErrorMessage::EReferenceInDefault { .. } => Some(RecursiveDefinition),
            ErrorMessage::EDuplicateClassMember(box EDuplicateClassMemberData { .. }) => {
                Some(DuplicateClassMember)
            }
            ErrorMessage::EEmptyArrayNoProvider { .. } => Some(EmptyArrayNoAnnot),
            ErrorMessage::EBigIntRShift3 { .. } => Some(BigIntRShift3),
            ErrorMessage::EBigIntNumCoerce { .. } => Some(BigIntNumCoerce),
            ErrorMessage::EInvalidCatchParameterAnnotation { .. } => {
                Some(InvalidCatchParameterAnnotation)
            }
            ErrorMessage::EInvalidRendersTypeArgument(box EInvalidRendersTypeArgumentData {
                ..
            }) => Some(InvalidRendersTypeArgument),
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
            | ErrorMessage::ESketchyNullLint(box ESketchyNullLintData { .. })
            | ErrorMessage::ESketchyNumberLint { .. }
            | ErrorMessage::EUnnecessaryOptionalChain { .. }
            | ErrorMessage::EUnnecessaryInvariant { .. }
            | ErrorMessage::EImplicitInexactObject { .. }
            | ErrorMessage::EAmbiguousObjectType { .. }
            | ErrorMessage::EReactIntrinsicOverlap(box EReactIntrinsicOverlapData { .. })
            | ErrorMessage::EUninitializedInstanceProperty { .. }
            | ErrorMessage::ENestedComponent { .. }
            | ErrorMessage::ENestedHook { .. }
            | ErrorMessage::EUnusedPromise { .. } => match self.kind_of_msg() {
                ErrorKind::LintError(kind) => {
                    Some(flow_common_errors::error_codes::code_of_lint(&kind))
                }
                _ => None,
            },
            ErrorMessage::ETSSyntax(box ETSSyntaxData { .. }) => Some(UnsupportedSyntax),
            ErrorMessage::EVarianceKeyword(box EVarianceKeywordData { .. }) => {
                Some(UnsupportedSyntax)
            }
            ErrorMessage::EInvalidTypeCastSyntax { .. } => Some(InvalidTypeCastSyntax),
            ErrorMessage::EMissingPlatformSupportWithAvailablePlatforms(
                box EMissingPlatformSupportWithAvailablePlatformsData { .. },
            ) => Some(MissingPlatformSupport),
            ErrorMessage::EMissingPlatformSupport(box EMissingPlatformSupportData { .. }) => {
                Some(MissingPlatformSupport)
            }
            ErrorMessage::EUnionPartialOptimizationNonUniqueKey(..) => {
                Some(UnionPartiallyOptimizableNonUniqueKeys)
            }
            ErrorMessage::EUnionOptimization(box EUnionOptimizationData { .. }) => {
                Some(UnionUnoptimizable)
            }
            ErrorMessage::EUnionOptimizationOnNonUnion(box EUnionOptimizationOnNonUnionData {
                ..
            }) => Some(UnionUnoptimizable),
            ErrorMessage::ECannotCallReactComponent { .. } => Some(ReactRuleCallComponent),
            ErrorMessage::EMatchError(
                MatchErrorKind::MatchNotExhaustive(box MatchNotExhaustiveData { .. })
                | MatchErrorKind::MatchNonExhaustiveObjectPattern(
                    box MatchNonExhaustiveObjectPatternData { .. },
                )
                | MatchErrorKind::MatchInvalidGuardedWildcard(_),
            ) => Some(MatchNotExhaustive),
            ErrorMessage::EMatchError(MatchErrorKind::MatchNonExplicitEnumCheck(
                box MatchNonExplicitEnumCheckData { .. },
            )) => Some(RequireExplicitEnumChecks),
            ErrorMessage::EMatchError(MatchErrorKind::MatchUnusedPattern(
                box MatchUnusedPatternData { .. },
            )) => Some(MatchUnusedPattern),
            ErrorMessage::EMatchError(
                MatchErrorKind::MatchInvalidIdentOrMemberPattern(
                    box MatchInvalidIdentOrMemberPatternData { .. },
                )
                | MatchErrorKind::MatchInvalidBindingKind { .. }
                | MatchErrorKind::MatchInvalidObjectPropertyLiteral { .. }
                | MatchErrorKind::MatchInvalidUnaryZero { .. }
                | MatchErrorKind::MatchInvalidUnaryPlusBigInt { .. }
                | MatchErrorKind::MatchDuplicateObjectProperty(
                    box MatchDuplicateObjectPropertyData { .. },
                )
                | MatchErrorKind::MatchBindingInOrPattern { .. }
                | MatchErrorKind::MatchInvalidAsPattern { .. }
                | MatchErrorKind::MatchInvalidPatternReference(
                    box MatchInvalidPatternReferenceData { .. },
                )
                | MatchErrorKind::MatchInvalidObjectShorthand(box MatchInvalidObjectShorthandData {
                    ..
                })
                | MatchErrorKind::MatchInvalidInstancePattern(_),
            ) => Some(MatchInvalidPattern),
            ErrorMessage::EMatchError(MatchErrorKind::MatchStatementInvalidBody { .. }) => {
                Some(MatchStatementInvalidBody)
            }
            ErrorMessage::EMatchError(
                MatchErrorKind::MatchInvalidCaseSyntax(box MatchInvalidCaseSyntaxData { .. })
                | MatchErrorKind::MatchInvalidWildcardSyntax(_),
            ) => Some(UnsupportedSyntax),
            ErrorMessage::ERecordError(e) => match e {
                RecordErrorKind::RecordBannedTypeUtil { .. } => Some(RecordBannedTypeUtil),
                RecordErrorKind::RecordInvalidName { .. } => Some(RecordInvalidName),
                RecordErrorKind::RecordInvalidNew { .. } => Some(RecordInvalidNew),
                RecordErrorKind::RecordDeclarationInvalidSyntax { .. } => Some(RecordInvalidSyntax),
            },
            ErrorMessage::EUndocumentedFeature { .. } => Some(UndocumentedFeature),
            ErrorMessage::EIllegalAssertOperator(box EIllegalAssertOperatorData { .. }) => {
                Some(IllegalAssertOperator)
            }
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
        InterfaceMergePropertyConflict {
            name,
            current_binding_loc,
            existing_binding_loc,
        } => InterfaceMergePropertyConflict {
            name,
            current_binding_loc: f(current_binding_loc),
            existing_binding_loc: f(existing_binding_loc),
        },
        InterfaceMergeTparamMismatch {
            name,
            current_binding_loc,
            existing_binding_loc,
        } => InterfaceMergeTparamMismatch {
            name,
            current_binding_loc: f(current_binding_loc),
            existing_binding_loc: f(existing_binding_loc),
        },
    }
}
