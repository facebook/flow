/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::ReasonDescFunction;
use flow_common::reason::VirtualReason;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::is_array_reason;
use flow_common::reason::is_nullish_reason;
use flow_common::reason::is_scalar_reason;
use flow_common::reason::mk_reason;
use flow_common_errors::error_codes::ErrorCode;
use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::ErrorKind;
use flow_common_errors::error_utils::PrintableError;
use flow_common_ty::ty::ALocTy;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_parser::jsdoc;
use flow_parser::loc::Loc;
use flow_typing_type::type_::ClassImplementsCheckData;
use flow_typing_type::type_::ClassOwnProtoCheckData;
use flow_typing_type::type_::ConformToCommonInterfaceData;
use flow_typing_type::type_::ConstrainedAssignmentData;
use flow_typing_type::type_::DroType;
use flow_typing_type::type_::FunCallData;
use flow_typing_type::type_::FunCallMethodData;
use flow_typing_type::type_::FunImplicitReturnData;
use flow_typing_type::type_::FunMissingArgData;
use flow_typing_type::type_::FunParamData;
use flow_typing_type::type_::OpaqueTypeCustomErrorCompatibilityData;
use flow_typing_type::type_::PositiveTypeGuardConsistencyData;
use flow_typing_type::type_::PropertyCompatibilityData;
use flow_typing_type::type_::ReactCreateElementCallData;
use flow_typing_type::type_::RecordCreateData;
use flow_typing_type::type_::SetPropertyData;
use flow_typing_type::type_::SwitchRefinementCheckData;
use flow_typing_type::type_::TupleElementCompatibilityData;
use flow_typing_type::type_::TypeArgCompatibilityData;
use flow_typing_type::type_::UnionEnum;
use flow_typing_type::type_::VirtualFrameUseOp;
use flow_typing_type::type_::VirtualRootUseOp;
use flow_typing_type::type_::VirtualUseOp;
use flow_typing_type::type_::fold_virtual_use_op;
use flow_typing_type::type_::root_of_use_op;
use flow_typing_type::type_::type_or_type_desc::TypeOrTypeDescT;
use vec1::Vec1;

use super::error_message::EIncompatibleDefsData;
use super::error_message::EnumErrorKind;
use super::error_message::ErrorMessage as FlowErrorMessage;
use super::error_message::util_use_op_of_msg;
use super::flow_error::ErrorSet;
use super::flow_error::FlowError;
use super::intermediate_error_types::AccessChainSegment;
use super::intermediate_error_types::ErrorMessage;
use super::intermediate_error_types::Explanation;
use super::intermediate_error_types::ExplanationAdditionalUnionMembersData;
use super::intermediate_error_types::ExplanationConstrainedAssignData;
use super::intermediate_error_types::ExplanationCustomErrorData;
use super::intermediate_error_types::ExplanationInvariantSubtypingDueToMutableArrayData;
use super::intermediate_error_types::ExplanationInvariantSubtypingDueToMutablePropertiesData;
use super::intermediate_error_types::ExplanationInvariantSubtypingDueToMutablePropertyData;
use super::intermediate_error_types::ExplanationPropertyMissingDueToNeutralOptionalPropertyData;
use super::intermediate_error_types::Frame as ErrorFrame;
use super::intermediate_error_types::IntermediateError;
use super::intermediate_error_types::Message;
use super::intermediate_error_types::MessageAlreadyExhaustivelyCheckOneEnumMemberData;
use super::intermediate_error_types::MessageCannotAccessEnumMemberData;
use super::intermediate_error_types::MessageCannotAddComputedPropertyDueToPotentialOverwriteData;
use super::intermediate_error_types::MessageCannotCallMaybeReactHookData;
use super::intermediate_error_types::MessageCannotCompareData;
use super::intermediate_error_types::MessageCannotExhaustivelyCheckAbstractEnumsData;
use super::intermediate_error_types::MessageCannotExhaustivelyCheckEnumWithUnknownsData;
use super::intermediate_error_types::MessageCannotExportRenamedDefaultData;
use super::intermediate_error_types::MessageCannotInstantiateObjectUtilTypeWithEnumData;
use super::intermediate_error_types::MessageCannotResolveBuiltinModuleData;
use super::intermediate_error_types::MessageCannotSpreadGeneralData;
use super::intermediate_error_types::MessageCannotSpreadInexactMayOverwriteIndexerData;
use super::intermediate_error_types::MessageCannotUseEnumMemberUsedAsTypeData;
use super::intermediate_error_types::MessageCannotUseTypeForAnnotationInferenceData;
use super::intermediate_error_types::MessageCannotUseTypeGuardWithFunctionParamHavocedData;
use super::intermediate_error_types::MessageCannotUseTypeInValuePositionData;
use super::intermediate_error_types::MessageDefinitionInvalidRecursiveData;
use super::intermediate_error_types::MessageDuplicateModuleProviderData;
use super::intermediate_error_types::MessageEnumDuplicateMemberNameData;
use super::intermediate_error_types::MessageEnumInvalidMemberInitializerData;
use super::intermediate_error_types::MessageExponentialSpreadData;
use super::intermediate_error_types::MessageIncompatibleDueToInvariantSubtypingData;
use super::intermediate_error_types::MessageIncompatibleGeneralWithPrintedTypesData;
use super::intermediate_error_types::MessageIncompatibleImplicitReturnData;
use super::intermediate_error_types::MessageIncompatibleReactDeepReadOnlyData;
use super::intermediate_error_types::MessageIncompatibleTupleArityData;
use super::intermediate_error_types::MessageIncompleteExhausiveCheckEnumData;
use super::intermediate_error_types::MessageInvalidEnumMemberCheckData;
use super::intermediate_error_types::MessageInvalidKeyPropertyInSpreadData;
use super::intermediate_error_types::MessageInvalidRefPropertyInSpreadData;
use super::intermediate_error_types::MessageInvalidRendersTypeArgumentData;
use super::intermediate_error_types::MessageInvalidSelfReferencingDefaultData;
use super::intermediate_error_types::MessageInvalidSelfReferencingTypeAnnotationData;
use super::intermediate_error_types::MessageMatchNonExhaustiveObjectPatternData;
use super::intermediate_error_types::MessageMatchNonExplicitEnumCheckData;
use super::intermediate_error_types::MessageMissingPlatformSupportWithAvailablePlatformsData;
use super::intermediate_error_types::MessageNoDefaultExportData;
use super::intermediate_error_types::MessageNoNamedExportData;
use super::intermediate_error_types::MessageOnlyDefaultExportData;
use super::intermediate_error_types::MessagePropExtraAgainstExactObjectData;
use super::intermediate_error_types::MessagePropMissingData;
use super::intermediate_error_types::MessagePropPolarityMismatchData;
use super::intermediate_error_types::MessagePropsMissingData;
use super::intermediate_error_types::MessageReactIntrinsicOverlapData;
use super::intermediate_error_types::MessageRedeclareComponentPropData;
use super::intermediate_error_types::MessageShouldAnnotateVariableUsedInGenericContextData;
use super::intermediate_error_types::MessageSketchyNullCheckData;
use super::intermediate_error_types::MessageTupleElementNotReadableData;
use super::intermediate_error_types::MessageTupleElementNotWritableData;
use super::intermediate_error_types::MessageTupleIndexOutOfBoundData;
use super::intermediate_error_types::MessageTupleNonIntegerIndexData;
use super::intermediate_error_types::MessageVariableOnlyAssignedByNullData;
use super::intermediate_error_types::RootMessage;
use super::intermediate_error_types::SubComponentOfInvariantSubtypingError;
use crate::error_message::EExpectedBigIntLitData;
use crate::error_message::EExpectedBooleanLitData;
use crate::error_message::EExpectedNumberLitData;
use crate::error_message::EExpectedStringLitData;
use crate::error_message::EIncompatiblePropData;
use crate::error_message::EIncompatibleWithUseOpData;
use crate::error_message::EPropNotFoundInLookupData;
use crate::error_message::EnumIncompatibleData;
use crate::error_message::IncompatibleEnumData;
use crate::error_message::IncompatibleInvariantSubtypingData;
use crate::error_message::IncompatibleSubtypingData;
use crate::error_message::IncompatibleUseData;
use crate::error_message::PropMissingInLookupData;
use crate::error_message::PropMissingInSubtypingData;
use crate::error_message::PropPolarityMismatchData;
use crate::error_message::PropsExtraAgainstExactObjectData;
use crate::error_message::PropsMissingInInvariantSubtypingData;
use crate::error_message::PropsMissingInSubtypingData;
use crate::error_message::SpeculationData;
use crate::error_message::UseOpData;

/// Rank scores for signals of different strength on an x^2 scale so that
/// greater signals dominate lesser signals.
const REASON_SCORE: i32 = 100;
const FRAME_SCORE: i32 = REASON_SCORE * 2;
const TYPE_ARG_FRAME_SCORE: i32 = FRAME_SCORE * 2;
const TUPLE_ELEMENT_FRAME_SCORE: i32 = TYPE_ARG_FRAME_SCORE * 2;

/// Gets the score of a use_op. Used in score_of_msg.
///
/// Calculated by taking the count of all the frames.
fn score_of_use_op<L: Dupe + PartialEq + Eq + PartialOrd + Ord>(use_op: &VirtualUseOp<L>) -> i32 {
    fold_virtual_use_op(
        use_op,
        |root| -> i32 {
            match root {
                VirtualRootUseOp::Speculation(inner_use_op) => score_of_use_op(inner_use_op),
                _ => 0,
            }
        },
        &|acc: i32, frame: &VirtualFrameUseOp<L>| -> i32 {
            acc + match frame {
                // Later params that error get a higher score. This roughly represents how
                // much type-checking work Flow successfully completed before erroring.
                // Useful for basically only overloaded function error messages.
                //
                // The signal that this gives us is that we successfully type checked n
                // params in the call before erroring. If there was no error, Flow may
                // have gone to successfully check another m params. However, we will
                // never know that. n is our best approximation. It rewards errors near
                // the end of a call and punishes (slightly) errors near the beginning of
                // a call.
                //
                // This, however, turns out to be consistent with code style in modern
                // JavaScript. As an unspoken convention, more complex arguments usually
                // go last. For overloaded functions, the switching generally happens on
                // the first argument. The "tag". This gives us confidence that n on
                // FunParam is a good heuristic for the score.
                //
                // FunRestParam is FunParam, but at the end. So give it a larger score
                // then FunParam after adding n.
                //
                // We do _not_ add n to the score if this use_op was added to an implicit type parameter.
                VirtualFrameUseOp::FunParam(box FunParamData { n, .. }) => FRAME_SCORE + *n,
                VirtualFrameUseOp::FunRestParam { .. } => FRAME_SCORE + FRAME_SCORE - 1,
                // FunCompatibility is generally followed by another use_op. So let's not
                // count FunCompatibility.
                VirtualFrameUseOp::FunCompatibility { .. } => 0,
                // FunMissingArg means the error is *less* likely to be correct.
                VirtualFrameUseOp::FunMissingArg(..) => 0,
                // Higher signal then PropertyCompatibility, for example.
                VirtualFrameUseOp::TypeArgCompatibility(..) => TYPE_ARG_FRAME_SCORE,
                VirtualFrameUseOp::ArrayElementCompatibility { .. } => TYPE_ARG_FRAME_SCORE,
                // Higher signal then TypeArgCompatibility.
                VirtualFrameUseOp::TupleElementCompatibility(..) => TUPLE_ELEMENT_FRAME_SCORE,
                // ImplicitTypeParam is an internal marker use_op that doesn't get
                // rendered in error messages. So it doesn't necessarily signal anything
                // about the user's intent.
                VirtualFrameUseOp::ImplicitTypeParam => 0,
                VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility(..) => 0,
                _ => FRAME_SCORE,
            }
        },
    )
}

/// Gets the score of an error message.
///
/// The score is an approximation of how close the user was to getting their code right.
/// A higher score means the user was closer than a lower score. A score of 0 means
/// we have no signal about how close the user was.
///
/// For example, consider the following two flows:
///
/// ```text
/// number ~> {p: string}
/// {p: number} ~> {p: string}
/// ```
///
/// Clearly, the user was closer to being correct with the second flow. So this
/// function should assign the `number ~> string` error a higher score than the
/// `number ~> object` error.
///
/// Now consider:
///
/// ```text
/// number ~> string
/// number ~> {p: string}
/// ```
///
/// This time we kept the lower bound the same and changed the upper bound. The
/// first flow is this time closer to the user's intent than the second flow.
/// So we give the `number ~> string` message a higher score than the
/// `number ~> object` message.
///
/// This scoring mechanism is useful for union and intersection error messages
/// where we want to approximate which branch the user meant to target with
/// their code. Branches with higher scores have a higher likelihood of being
/// the branch the user was targeting.
#[allow(dead_code)]
pub fn score_of_msg<L: Dupe + PartialEq + Eq + PartialOrd + Ord>(msg: &FlowErrorMessage<L>) -> i32 {
    // Start by getting the score based off the use_op of our error message. If
    // the message does not have a use_op then we return 0. This score
    // contribution declares that greater complexity in the use is more likely to
    // cause a match.
    let score = util_use_op_of_msg(0, score_of_use_op, msg);

    // Special cases for messages which increment the score.
    // If a property doesn't exist, we still use a PropertyCompatibility use_op.
    // a missing prop does not increase the likelihood that the user was close to
    // the right types.
    let score = score
        + match msg {
            FlowErrorMessage::EIncompatibleProp(box EIncompatiblePropData {
                use_op: Some(use_op),
                ..
            }) => match use_op {
                VirtualUseOp::Frame(frame, _)
                    if matches!(frame.as_ref(), VirtualFrameUseOp::PropertyCompatibility(..)) =>
                {
                    -FRAME_SCORE
                }
                _ => 0,
            },
            FlowErrorMessage::EPropNotFoundInLookup(box EPropNotFoundInLookupData {
                use_op,
                ..
            }) => match use_op {
                VirtualUseOp::Frame(frame, _)
                    if matches!(frame.as_ref(), VirtualFrameUseOp::PropertyCompatibility(..)) =>
                {
                    -FRAME_SCORE
                }
                _ => 0,
            },
            _ => 0,
        };

    // If we have two incompatible types and both incompatible types are scalar or
    // both types are arrays then increment our score. This is based on the belief
    // that the solutions with the lowest possible complexity are closest to each
    // other. e.g. number ~> string. If one type is a scalar or array and the
    // other type is not then we decrement our score.
    score + {
        let reasons: Option<(&VirtualReason<L>, &VirtualReason<L>)> = match msg {
            FlowErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData {
                reason_lower: rl,
                reason_upper: ru,
                branches,
                ..
            }) if branches.is_empty() => Some((rl, ru)),
            FlowErrorMessage::EIncompatibleWithUseOp(box EIncompatibleWithUseOpData {
                reason_lower: rl,
                reason_upper: ru,
                ..
            }) => Some((rl, ru)),
            FlowErrorMessage::EIncompatibleWithExact((rl, ru), _, _) => Some((rl, ru)),
            _ => None,
        };
        match reasons {
            Some((rl, ru)) => {
                if is_nullish_reason(rl) && is_nullish_reason(ru) {
                    REASON_SCORE
                } else if is_nullish_reason(rl) || is_nullish_reason(ru) {
                    // T ~> null should have a lower score then T ~> scalar
                    0
                } else if is_scalar_reason(rl) && is_scalar_reason(ru) {
                    REASON_SCORE
                } else if is_scalar_reason(rl) || is_scalar_reason(ru) {
                    1
                } else if is_array_reason(rl) && is_array_reason(ru) {
                    REASON_SCORE
                } else if is_array_reason(rl) || is_array_reason(ru) {
                    1
                } else {
                    REASON_SCORE
                }
            }
            None => REASON_SCORE,
        }
    }
}

/// Flips the lower and upper bounds of a frame use operation.
///
/// When an error occurs in a contravariant position (like function arguments),
/// we need to flip the lower and upper bounds to point to the correct location.
fn flip_frame<L: Dupe + PartialEq + Eq + PartialOrd + Ord>(
    frame: VirtualFrameUseOp<L>,
) -> VirtualFrameUseOp<L> {
    use VirtualFrameUseOp::*;

    match frame {
        ArrayElementCompatibility { lower, upper } => ArrayElementCompatibility {
            lower: upper,
            upper: lower,
        },
        FunCompatibility { lower, upper } => FunCompatibility {
            lower: upper,
            upper: lower,
        },
        FunParam(box FunParamData {
            n,
            name,
            lower,
            upper,
        }) => FunParam(Box::new(FunParamData {
            n,
            name,
            lower: upper,
            upper: lower,
        })),
        FunRestParam { lower, upper } => FunRestParam {
            lower: upper,
            upper: lower,
        },
        FunReturn { lower, upper } => FunReturn {
            lower: upper,
            upper: lower,
        },
        IndexerKeyCompatibility { lower, upper } => IndexerKeyCompatibility {
            lower: upper,
            upper: lower,
        },
        OpaqueTypeLowerBoundCompatibility { lower, upper } => OpaqueTypeLowerBoundCompatibility {
            lower: upper,
            upper: lower,
        },
        OpaqueTypeUpperBoundCompatibility { lower, upper } => OpaqueTypeUpperBoundCompatibility {
            lower: upper,
            upper: lower,
        },
        PropertyCompatibility(box PropertyCompatibilityData { prop, lower, upper }) => {
            PropertyCompatibility(Box::new(PropertyCompatibilityData {
                prop,
                lower: upper,
                upper: lower,
            }))
        }
        ReactConfigCheck => ReactConfigCheck,
        TupleElementCompatibility(box TupleElementCompatibilityData {
            n,
            lower,
            upper,
            lower_optional,
            upper_optional,
        }) => TupleElementCompatibility(Box::new(TupleElementCompatibilityData {
            n,
            lower: upper,
            upper: lower,
            lower_optional: upper_optional,
            upper_optional: lower_optional,
        })),
        TypeArgCompatibility(box TypeArgCompatibilityData {
            name,
            targ,
            lower,
            upper,
            polarity,
        }) => TypeArgCompatibility(Box::new(TypeArgCompatibilityData {
            name,
            targ,
            lower: upper,
            upper: lower,
            polarity,
        })),
        EnumRepresentationTypeCompatibility { lower, upper } => {
            EnumRepresentationTypeCompatibility {
                lower: upper,
                upper: lower,
            }
        }
        TupleAssignment { .. }
        | TypeParamBound { .. }
        | OpaqueTypeLowerBound { .. }
        | OpaqueTypeUpperBound { .. }
        | FunMissingArg(..)
        | ImplicitTypeParam
        | ReactGetConfig { .. }
        | OpaqueTypeCustomErrorCompatibility(..)
        | UnifyFlip
        | ConstrainedAssignment(..)
        | MappedTypeKeyCompatibility { .. }
        | TypeGuardCompatibility
        | RendersCompatibility
        | ReactDeepReadOnly(..)
        | UnionRepresentative { .. } => frame,
    }
}

#[allow(dead_code)]
pub fn post_process_errors(original_errors: ErrorSet) -> ErrorSet {
    use super::flow_error::error_of_msg;

    ///  Unification produces two errors. One for both sides. For example,
    /// `{p: number} ~> {p: string}` errors on both `number ~> string` and
    /// `string ~> number`. Showing both errors to our user is often redundant.
    /// So we use this utility to flip the `string ~> number` case and produce an
    /// error identical to one we've produced before. These two errors will be
    /// deduped by the filter.
    fn dedupe_by_flip_loop(use_op: VirtualUseOp<ALoc>) -> (bool, VirtualUseOp<ALoc>) {
        match use_op {
            // Roots don't flip
            VirtualUseOp::Op(_) => (false, use_op),
            // Start flipping if we are on the reverse side of unification
            VirtualUseOp::Frame(frame, inner) => {
                if matches!(frame.as_ref(), VirtualFrameUseOp::UnifyFlip) {
                    let (flip, inner_op) = dedupe_by_flip_loop(inner.as_ref().clone());
                    (!flip, inner_op)
                } else {
                    // If we are in flip mode then flip our frame.
                    let (flip, inner_op) = dedupe_by_flip_loop(inner.as_ref().clone());
                    if flip {
                        let flipped_frame = flip_frame(frame.as_ref().clone());
                        (
                            true,
                            VirtualUseOp::Frame(Arc::new(flipped_frame), Arc::new(inner_op)),
                        )
                    } else {
                        (false, VirtualUseOp::Frame(frame, Arc::new(inner_op)))
                    }
                }
            }
        }
    }

    fn dedupe_by_flip(
        lower: VirtualReason<ALoc>,
        upper: VirtualReason<ALoc>,
        use_op: VirtualUseOp<ALoc>,
    ) -> (
        (VirtualReason<ALoc>, VirtualReason<ALoc>),
        VirtualUseOp<ALoc>,
    ) {
        let (flip, use_op) = dedupe_by_flip_loop(use_op);
        if flip {
            ((upper, lower), use_op)
        } else {
            ((lower, upper), use_op)
        }
    }

    fn remove_unify_flip(use_op: VirtualUseOp<ALoc>) -> VirtualUseOp<ALoc> {
        match use_op {
            VirtualUseOp::Op(_) => use_op,
            VirtualUseOp::Frame(frame, inner) => {
                if matches!(frame.as_ref(), VirtualFrameUseOp::UnifyFlip) {
                    remove_unify_flip(inner.as_ref().clone())
                } else {
                    VirtualUseOp::Frame(frame, Arc::new(remove_unify_flip(inner.as_ref().clone())))
                }
            }
        }
    }

    // Clone for use in the closure - the filter will consume the original
    let errors_for_lookup = original_errors.dupe();

    let retain_error = |error: &FlowError<ALoc>| -> bool {
        let is_not_duplicate = |new_msg: FlowErrorMessage<ALoc>| -> bool {
            let e_prime = error_of_msg(error.source_file.dupe(), new_msg);
            !errors_for_lookup.mem(&e_prime)
        };

        match error.msg_of_error() {
            FlowErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData {
                use_op,
                reason_lower,
                reason_upper,
                branches,
            }) => {
                let ((reason_lower_new, reason_upper_new), use_op_new) =
                    dedupe_by_flip(reason_lower.dupe(), reason_upper.dupe(), use_op.clone());
                reason_lower == &reason_lower_new
                    || is_not_duplicate(FlowErrorMessage::EIncompatibleDefs(Box::new(
                        EIncompatibleDefsData {
                            use_op: use_op_new,
                            reason_lower: reason_lower_new,
                            reason_upper: reason_upper_new,
                            branches: branches.clone(),
                        },
                    )))
            }
            FlowErrorMessage::EExpectedStringLit(box EExpectedStringLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => {
                let ((reason_lower_new, reason_upper_new), use_op_new) =
                    dedupe_by_flip(reason_lower.dupe(), reason_upper.dupe(), use_op.clone());
                reason_lower == &reason_lower_new
                    || is_not_duplicate(FlowErrorMessage::EExpectedStringLit(Box::new(
                        EExpectedStringLitData {
                            reason_lower: reason_lower_new,
                            reason_upper: reason_upper_new,
                            use_op: use_op_new,
                        },
                    )))
            }
            FlowErrorMessage::EExpectedNumberLit(box EExpectedNumberLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => {
                let ((reason_lower_new, reason_upper_new), use_op_new) =
                    dedupe_by_flip(reason_lower.dupe(), reason_upper.dupe(), use_op.clone());
                reason_lower == &reason_lower_new
                    || is_not_duplicate(FlowErrorMessage::EExpectedNumberLit(Box::new(
                        EExpectedNumberLitData {
                            reason_lower: reason_lower_new,
                            reason_upper: reason_upper_new,
                            use_op: use_op_new,
                        },
                    )))
            }
            FlowErrorMessage::EExpectedBooleanLit(box EExpectedBooleanLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => {
                let ((reason_lower_new, reason_upper_new), use_op_new) =
                    dedupe_by_flip(reason_lower.dupe(), reason_upper.dupe(), use_op.clone());
                reason_lower == &reason_lower_new
                    || is_not_duplicate(FlowErrorMessage::EExpectedBooleanLit(Box::new(
                        EExpectedBooleanLitData {
                            reason_lower: reason_lower_new,
                            reason_upper: reason_upper_new,
                            use_op: use_op_new,
                        },
                    )))
            }
            FlowErrorMessage::EExpectedBigIntLit(box EExpectedBigIntLitData {
                reason_lower,
                reason_upper,
                use_op,
            }) => {
                let ((reason_lower_new, reason_upper_new), use_op_new) =
                    dedupe_by_flip(reason_lower.dupe(), reason_upper.dupe(), use_op.clone());
                reason_lower == &reason_lower_new
                    || is_not_duplicate(FlowErrorMessage::EExpectedBigIntLit(Box::new(
                        EExpectedBigIntLitData {
                            reason_lower: reason_lower_new,
                            reason_upper: reason_upper_new,
                            use_op: use_op_new,
                        },
                    )))
            }
            FlowErrorMessage::EIncompatibleWithUseOp(box EIncompatibleWithUseOpData {
                use_op,
                reason_lower,
                reason_upper,
                explanation,
            }) => {
                let ((reason_lower_new, reason_upper_new), use_op_new) =
                    dedupe_by_flip(reason_lower.dupe(), reason_upper.dupe(), use_op.clone());
                reason_lower == &reason_lower_new
                    || is_not_duplicate(FlowErrorMessage::EIncompatibleWithUseOp(Box::new(
                        EIncompatibleWithUseOpData {
                            use_op: use_op_new,
                            reason_lower: reason_lower_new,
                            reason_upper: reason_upper_new,
                            explanation: explanation.clone(),
                        },
                    )))
            }
            FlowErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible(
                box EnumIncompatibleData {
                    use_op,
                    reason_lower,
                    reason_upper,
                    enum_kind,
                    representation_type,
                    casting_syntax,
                },
            )) => {
                let ((reason_lower_new, reason_upper_new), use_op_new) =
                    dedupe_by_flip(reason_lower.dupe(), reason_upper.dupe(), use_op.clone());
                reason_lower == &reason_lower_new
                    || is_not_duplicate(FlowErrorMessage::EEnumError(
                        EnumErrorKind::EnumIncompatible(Box::new(EnumIncompatibleData {
                            use_op: use_op_new,
                            reason_lower: reason_lower_new,
                            reason_upper: reason_upper_new,
                            enum_kind: enum_kind.clone(),
                            representation_type: representation_type.clone(),
                            casting_syntax: *casting_syntax,
                        })),
                    ))
            }
            FlowErrorMessage::EPropNotFoundInLookup(box EPropNotFoundInLookupData {
                prop_name,
                reason_obj,
                reason_prop,
                use_op,
                suggestion,
            }) => {
                // PropNotFound error will always display the missing vs existing prop in the same order
                let use_op_new = remove_unify_flip(use_op.clone());
                use_op == &use_op_new
                    || is_not_duplicate(FlowErrorMessage::EPropNotFoundInLookup(Box::new(
                        EPropNotFoundInLookupData {
                            prop_name: prop_name.clone(),
                            reason_obj: reason_obj.dupe(),
                            reason_prop: reason_prop.dupe(),
                            use_op: use_op_new,
                            suggestion: suggestion.clone(),
                        },
                    )))
            }
            _ => true,
        }
    };

    original_errors.filter(retain_error)
}

fn mk_error<L: Dupe>(
    kind: ErrorKind,
    loc: Loc,
    error_code: Option<ErrorCode>,
    message: Message<L>,
    root: Option<(Loc, RootMessage<L>)>,
    frames: Option<Vec<ErrorFrame<L>>>,
    explanations: Option<Vec<Explanation<L>>>,
) -> IntermediateError<L> {
    IntermediateError {
        kind,
        loc,
        root,
        error_code,
        message: ErrorMessage::SingletonMessage {
            message,
            frames,
            explanations,
        },
        misplaced_source_file: None,
        unsuppressable: false,
    }
}

fn mk_speculation_error<L: Dupe + Clone>(
    kind: ErrorKind,
    loc: Loc,
    root: Option<(Loc, RootMessage<L>)>,
    frames: Vec<ErrorFrame<L>>,
    explanations: Vec<Explanation<L>>,
    error_code: Option<ErrorCode>,
    speculation_errors: Vec<(i32, IntermediateError<L>)>,
) -> IntermediateError<L>
where
    ErrorFrame<L>: Clone,
    Explanation<L>: Clone,
{
    // Flatten speculation branches using mutable refs + push/pop to avoid clones.
    // recursing and pop after, eliminating structural clones entirely.
    fn flatten_speculation_branches<L: Dupe + Clone>(
        acc_frames: &mut Vec<Vec<ErrorFrame<L>>>,
        acc_explanations: &mut Vec<Vec<Explanation<L>>>,
        acc: &mut Vec<(i32, IntermediateError<L>)>,
        branches: Vec<(i32, IntermediateError<L>)>,
    ) where
        ErrorFrame<L>: Clone,
        Explanation<L>: Clone,
    {
        for (score, error) in branches {
            match error.message {
                // If we have a speculation error with no root, flatten its branches
                // using push/pop pattern - no cloning needed
                ErrorMessage::SpeculationMessage {
                    branches: nested_branches,
                    frames: inner_frames,
                    explanations: inner_explanations,
                } if error.root.is_none() => {
                    acc_frames.push(inner_frames);
                    acc_explanations.push(inner_explanations);
                    flatten_speculation_branches(
                        acc_frames,
                        acc_explanations,
                        acc,
                        nested_branches,
                    );
                    acc_frames.pop();
                    acc_explanations.pop();
                }
                ErrorMessage::SpeculationMessage {
                    branches: nested_branches,
                    frames: inner_frames,
                    explanations: inner_explanations,
                } => {
                    // Collect accumulated frames by iterating + cloning elements,
                    // then chain with owned inner_frames (moved, not cloned)
                    let frames: Vec<ErrorFrame<L>> = acc_frames
                        .iter()
                        .flat_map(|v| v.iter().cloned())
                        .chain(inner_frames)
                        .collect();

                    let explanations: Vec<Explanation<L>> = acc_explanations
                        .iter()
                        .flat_map(|v| v.iter().cloned())
                        .chain(inner_explanations)
                        .collect();

                    let message = ErrorMessage::SpeculationMessage {
                        branches: nested_branches,
                        frames,
                        explanations,
                    };
                    acc.push((
                        score,
                        IntermediateError {
                            kind: error.kind,
                            loc: error.loc,
                            root: error.root,
                            error_code: error.error_code,
                            message,
                            misplaced_source_file: error.misplaced_source_file,
                            unsuppressable: error.unsuppressable,
                        },
                    ));
                }
                ErrorMessage::SingletonMessage {
                    message,
                    frames: inner_frames,
                    explanations: inner_explanations,
                } => {
                    let frames = match (inner_frames, acc_frames.is_empty()) {
                        (Some(inner), _) => Some(
                            acc_frames
                                .iter()
                                .flat_map(|v| v.iter().cloned())
                                .chain(inner)
                                .collect(),
                        ),
                        (None, false) => {
                            Some(acc_frames.iter().flat_map(|v| v.iter().cloned()).collect())
                        }
                        (None, true) => None,
                    };

                    let explanations = match (inner_explanations, acc_explanations.is_empty()) {
                        (Some(inner), _) => Some(
                            acc_explanations
                                .iter()
                                .flat_map(|v| v.iter().cloned())
                                .chain(inner)
                                .collect(),
                        ),
                        (None, false) => Some(
                            acc_explanations
                                .iter()
                                .flat_map(|v| v.iter().cloned())
                                .collect(),
                        ),
                        (None, true) => None,
                    };

                    let msg = ErrorMessage::SingletonMessage {
                        message,
                        frames,
                        explanations,
                    };
                    acc.push((
                        score,
                        IntermediateError {
                            kind: error.kind,
                            loc: error.loc,
                            root: error.root,
                            error_code: error.error_code,
                            message: msg,
                            misplaced_source_file: error.misplaced_source_file,
                            unsuppressable: error.unsuppressable,
                        },
                    ));
                }
            }
        }
    }

    let mut acc_frames = vec![];
    let mut acc_explanations = vec![];
    let mut branches = vec![];
    flatten_speculation_branches(
        &mut acc_frames,
        &mut acc_explanations,
        &mut branches,
        speculation_errors,
    );
    branches.sort_by_key(|(score, _)| *score);

    IntermediateError {
        kind,
        loc,
        root,
        error_code,
        message: ErrorMessage::SpeculationMessage {
            frames,
            explanations,
            branches,
        },
        misplaced_source_file: None,
        unsuppressable: false,
    }
}

/// Make intermediate errors from FlowError.
///
/// Returns IntermediateError with the same location type as input (ALoc).
/// The loc_of_aloc function is only used for computing the primary loc and for error cases.
pub fn make_intermediate_error<L, F>(
    loc_of_aloc: F,
    speculation: bool,
    error: &FlowError<L>,
) -> IntermediateError<L>
where
    L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
    F: Fn(&L) -> Loc + Clone,
{
    // In friendly error messages, we always want to point to a value as the primary location.
    // Normally, values are in the lower bound, but in contravariant positions this flips.
    fn flip_contravariant<L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug>(
        lower: VirtualReason<L>,
        upper: VirtualReason<L>,
        use_op: VirtualUseOp<L>,
    ) -> ((VirtualReason<L>, VirtualReason<L>), VirtualUseOp<L>) {
        fn is_contravariant<
            L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
        >(
            frame: &VirtualFrameUseOp<L>,
            use_op: &VirtualUseOp<L>,
        ) -> (bool, bool) {
            use VirtualFrameUseOp::*;

            match (frame, use_op) {
                (FunParam(box FunParamData { .. }), VirtualUseOp::Frame(f, _))
                    if matches!(f.as_ref(), FunCompatibility { .. }) =>
                {
                    (true, true)
                }
                (FunRestParam { .. }, VirtualUseOp::Frame(f, _))
                    if matches!(f.as_ref(), FunCompatibility { .. }) =>
                {
                    (true, true)
                }
                (
                    ReactGetConfig {
                        polarity: Polarity::Negative,
                    },
                    _,
                ) => (true, false),
                (
                    TypeArgCompatibility(box TypeArgCompatibilityData {
                        polarity: Polarity::Negative,
                        ..
                    }),
                    _,
                ) => (true, false),
                _ => (false, false),
            }
        }

        fn is_contravariant_root<
            L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
        >(
            root: &VirtualRootUseOp<L>,
        ) -> bool {
            matches!(root, VirtualRootUseOp::FunImplicitReturn(..))
        }

        fn loop_flip<L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug>(
            use_op: VirtualUseOp<L>,
        ) -> (bool, VirtualUseOp<L>) {
            use VirtualUseOp::*;

            match use_op {
                Op(root_use_op) => {
                    let flip = is_contravariant_root(&root_use_op);
                    (flip, Op(root_use_op))
                }
                Frame(frame, inner_use_op) => {
                    let (flip, inner_use_op) = loop_flip(inner_use_op.as_ref().clone());
                    let (contravariant, flip_self_flag) = is_contravariant(&frame, &inner_use_op);

                    let flip = if contravariant { !flip } else { flip };
                    let flip_self = flip && (!contravariant || flip_self_flag);

                    let frame = if flip_self {
                        Arc::new(flip_frame((*frame).clone()))
                    } else {
                        frame
                    };

                    (flip, Frame(frame, Arc::new(inner_use_op)))
                }
            }
        }

        let (flip, use_op) = loop_flip(use_op);
        if flip {
            ((upper, lower), use_op)
        } else {
            ((lower, upper), use_op)
        }
    }

    fn mod_lower_reason_according_to_use_ops<
        L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
    >(
        lower: VirtualReason<L>,
        use_op: &VirtualUseOp<L>,
    ) -> VirtualReason<L> {
        use VirtualFrameUseOp::*;
        use VirtualUseOp::*;

        match use_op {
            Frame(frame_rc, inner_use_op) => match frame_rc.as_ref() {
                OpaqueTypeUpperBound {
                    opaque_t_reason, ..
                } => mod_lower_reason_according_to_use_ops(opaque_t_reason.dupe(), inner_use_op),
                _ => mod_lower_reason_according_to_use_ops(lower, inner_use_op),
            },
            Op(_) => lower,
        }
    }

    fn mod_upper_reason_according_to_use_ops<
        L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
    >(
        upper: VirtualReason<L>,
        use_op: &VirtualUseOp<L>,
    ) -> VirtualReason<L> {
        use VirtualFrameUseOp::*;
        use VirtualUseOp::*;

        match use_op {
            Frame(frame_rc, inner_use_op) => match frame_rc.as_ref() {
                OpaqueTypeLowerBound {
                    opaque_t_reason, ..
                } => mod_upper_reason_according_to_use_ops(opaque_t_reason.dupe(), inner_use_op),
                _ => mod_upper_reason_according_to_use_ops(upper, inner_use_op),
            },
            Op(_) => upper,
        }
    }

    /// Takes the smallest location where we found the error and a use_op which we will unwrap.
    /// Returns: (root with loc, custom error message, primary loc, frames, explanations)
    ///
    /// This is a massive function (~750 lines) with 13 nested helper functions.
    #[allow(unused_variables)]
    fn unwrap_use_ops<L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug>(
        loc_of_aloc: impl Fn(&L) -> Loc + Clone,
        speculation: bool,
    ) -> impl Fn(
        Loc,
        VirtualUseOp<L>,
    ) -> (
        Option<(Loc, RootMessage<L>)>,
        Option<Message<L>>,
        Loc,
        Vec<ErrorFrame<L>>,
        Vec<Explanation<L>>,
    ) {
        move |initial_loc: Loc, initial_use_op: VirtualUseOp<L>| {
            // We will optionally include lower and upper pair to provide a TS-like stacked errors.
            // We only do this if the upper bound has a nice name
            let opt_incompatibility_pair =
                |use_op: &VirtualUseOp<L>,
                 pair: &(VirtualReason<L>, VirtualReason<L>)|
                 -> Option<(VirtualReason<L>, VirtualReason<L>)> {
                    let (_lower, upper) = pair;

                    let is_named_type = |desc: &VirtualReasonDesc<L>| -> bool {
                        use flow_common::reason::VirtualReasonDesc::*;
                        matches!(
                            desc,
                            REnum { name: Some(_), .. } | RType(_) | RTypeAlias(box (_, Some(_), _))
                        )
                    };

                    if is_named_type(&upper.desc) {
                        match use_op {
                            VirtualUseOp::Op(root) => match root.as_ref() {
                                VirtualRootUseOp::AssignVar { var: Some(_), .. }
                                | VirtualRootUseOp::Cast { .. }
                                | VirtualRootUseOp::ClassExtendsCheck { .. }
                                | VirtualRootUseOp::ClassImplementsCheck(..) => None,
                                _ => Some(pair.dupe()),
                            },
                            _ => Some(pair.dupe()),
                        }
                    } else {
                        None
                    }
                };

            let loc_of_prop_compatibility_reason =
                |loc: Loc, reason: &VirtualReason<L>, use_op: &VirtualUseOp<L>| -> Loc {
                    match use_op {
                        VirtualUseOp::Op(root) => match root.as_ref() {
                            VirtualRootUseOp::ClassExtendsCheck { .. }
                            | VirtualRootUseOp::ClassImplementsCheck(..) => loc,
                            _ => loc_of_aloc(&reason.loc),
                        },
                        _ => loc_of_aloc(&reason.loc),
                    }
                };

            fn loop_to_form_access_chain_impl<
                L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
            >(
                lower_loc: Loc,
                use_op: VirtualUseOp<L>,
                loc_of_aloc: &impl Fn(&L) -> Loc,
                loc_of_prop_compatibility_reason: &impl Fn(
                    Loc,
                    &VirtualReason<L>,
                    &VirtualUseOp<L>,
                ) -> Loc,
                opt_incompatibility_pair: &impl Fn(
                    &VirtualUseOp<L>,
                    &(VirtualReason<L>, VirtualReason<L>),
                )
                    -> Option<(VirtualReason<L>, VirtualReason<L>)>,
            ) -> (Loc, Vec<AccessChainSegment>, VirtualUseOp<L>) {
                match use_op {
                    VirtualUseOp::Frame(frame_rc, inner) => {
                        match frame_rc.as_ref() {
                            VirtualFrameUseOp::PropertyCompatibility(
                                box PropertyCompatibilityData {
                                    prop: Some(prop),
                                    lower,
                                    upper,
                                },
                            ) if prop.as_str() != "$call"
                                && opt_incompatibility_pair(
                                    &inner,
                                    &(lower.dupe(), upper.dupe()),
                                )
                                .is_none() =>
                            {
                                let lower_loc_prime = loc_of_prop_compatibility_reason(
                                    lower_loc.dupe(),
                                    lower,
                                    &inner,
                                );

                                let lower_loc = if lower_loc_prime.contains(&lower_loc) {
                                    lower_loc
                                } else {
                                    lower_loc_prime
                                };

                                // Recurse to build the chain
                                let (final_loc, mut props, final_use_op) =
                                    loop_to_form_access_chain_impl(
                                        lower_loc,
                                        inner.as_ref().clone(),
                                        loc_of_aloc,
                                        loc_of_prop_compatibility_reason,
                                        opt_incompatibility_pair,
                                    );

                                props.push(AccessChainSegment::PropSegment(
                                    flow_common::reason::Name::new(prop.as_str()),
                                ));
                                (final_loc, props, final_use_op)
                            }

                            VirtualFrameUseOp::TupleElementCompatibility(
                                box TupleElementCompatibilityData {
                                    n, lower, upper, ..
                                },
                            ) if opt_incompatibility_pair(
                                &inner,
                                &(lower.dupe(), upper.dupe()),
                            )
                            .is_none() =>
                            {
                                let lower_loc_prime = loc_of_aloc(&lower.loc);

                                let lower_loc = if lower_loc_prime.contains(&lower_loc) {
                                    lower_loc
                                } else {
                                    lower_loc_prime
                                };

                                // Recurse to build the chain
                                let (final_loc, mut props, final_use_op) =
                                    loop_to_form_access_chain_impl(
                                        lower_loc,
                                        inner.as_ref().clone(),
                                        loc_of_aloc,
                                        loc_of_prop_compatibility_reason,
                                        opt_incompatibility_pair,
                                    );

                                props.push(AccessChainSegment::TupleIndexSegment(*n));
                                (final_loc, props, final_use_op)
                            }

                            _ => (lower_loc, vec![], VirtualUseOp::Frame(frame_rc, inner)),
                        }
                    }
                    _ => (lower_loc, vec![], use_op),
                }
            }

            let loop_to_form_access_chain =
                |lower_loc: Loc,
                 use_op: VirtualUseOp<L>|
                 -> (Loc, Vec<AccessChainSegment>, VirtualUseOp<L>) {
                    let (loc, mut props, use_op) = loop_to_form_access_chain_impl(
                        lower_loc,
                        use_op,
                        &loc_of_aloc,
                        &loc_of_prop_compatibility_reason,
                        &opt_incompatibility_pair,
                    );
                    props.reverse();
                    (loc, props, use_op)
                };

            let unknown_root = |loc: Loc,
                                frames: (Vec<ErrorFrame<L>>, Vec<Explanation<L>>),
                                custom_error_message: Option<Message<L>>|
             -> (
                Option<(Loc, RootMessage<L>)>,
                Option<Message<L>>,
                Loc,
                Vec<ErrorFrame<L>>,
                Vec<Explanation<L>>,
            ) {
                let (all_frames, explanations) = frames;
                (None, custom_error_message, loc, all_frames, explanations)
            };

            let root_with_loc_and_specific_loc =
                |loc: Loc,
                 frames: (Vec<ErrorFrame<L>>, Vec<Explanation<L>>),
                 root_loc: Loc,
                 specific_loc: Loc,
                 root_message: RootMessage<L>,
                 custom_error_message: Option<Message<L>>| {
                    let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                        loc
                    } else {
                        specific_loc
                    };

                    let (all_frames, explanations) = frames;
                    (
                        Some((root_loc, root_message)),
                        custom_error_message,
                        final_loc,
                        all_frames,
                        explanations,
                    )
                };

            let root_with_specific_reason =
                |loc: Loc,
                 frames: (Vec<ErrorFrame<L>>, Vec<Explanation<L>>),
                 root_reason: &VirtualReason<L>,
                 specific_reason: &VirtualReason<L>,
                 root_message: RootMessage<L>,
                 custom_error_message: Option<Message<L>>| {
                    let root_loc = loc_of_aloc(&root_reason.loc);
                    let specific_loc = loc_of_aloc(&specific_reason.loc);
                    root_with_loc_and_specific_loc(
                        loc,
                        frames,
                        root_loc,
                        specific_loc,
                        root_message,
                        custom_error_message,
                    )
                };

            let root = |loc: Loc,
                        frames: (Vec<ErrorFrame<L>>, Vec<Explanation<L>>),
                        root_reason: &VirtualReason<L>,
                        root_message: RootMessage<L>,
                        custom_error_message: Option<Message<L>>| {
                let root_loc = loc_of_aloc(&root_reason.loc);
                root_with_loc_and_specific_loc(
                    loc,
                    frames,
                    root_loc.dupe(),
                    root_loc,
                    root_message,
                    custom_error_message,
                )
            };

            fn loop_impl<L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug>(
                loc: Loc,
                frames: (Vec<ErrorFrame<L>>, Vec<Explanation<L>>),
                use_op: VirtualUseOp<L>,
                custom_error_message: Option<Message<L>>,
                loc_of_aloc: &impl Fn(&L) -> Loc,
                loc_of_prop_compatibility_reason: &impl Fn(
                    Loc,
                    &VirtualReason<L>,
                    &VirtualUseOp<L>,
                ) -> Loc,
                loop_to_form_access_chain: &impl Fn(
                    Loc,
                    VirtualUseOp<L>,
                )
                    -> (Loc, Vec<AccessChainSegment>, VirtualUseOp<L>),
                opt_incompatibility_pair: &impl Fn(
                    &VirtualUseOp<L>,
                    &(VirtualReason<L>, VirtualReason<L>),
                )
                    -> Option<(VirtualReason<L>, VirtualReason<L>)>,
                speculation: bool,
            ) -> (
                Option<(Loc, RootMessage<L>)>,
                Option<Message<L>>,
                Loc,
                Vec<ErrorFrame<L>>,
                Vec<Explanation<L>>,
            ) {
                let root = |loc: Loc,
                            frames: (Vec<ErrorFrame<L>>, Vec<Explanation<L>>),
                            root_reason: &VirtualReason<L>,
                            root_message: RootMessage<L>,
                            custom_error_message: Option<Message<L>>|
                 -> (
                    Option<(Loc, RootMessage<L>)>,
                    Option<Message<L>>,
                    Loc,
                    Vec<ErrorFrame<L>>,
                    Vec<Explanation<L>>,
                ) {
                    let root_loc = loc_of_aloc(&root_reason.loc);
                    let specific_loc = root_loc.dupe();

                    let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                        loc
                    } else {
                        specific_loc
                    };

                    let (all_frames, explanations) = frames;
                    (
                        Some((root_loc, root_message)),
                        custom_error_message,
                        final_loc,
                        all_frames,
                        explanations,
                    )
                };

                match use_op {
                    VirtualUseOp::Op(root_op) => {
                        match root_op.as_ref() {
                            VirtualRootUseOp::UnknownUse => {
                                let (all_frames, explanations) = frames;
                                (None, custom_error_message, loc, all_frames, explanations)
                            }

                            VirtualRootUseOp::Speculation(_) if speculation => {
                                let (all_frames, explanations) = frames;
                                (None, custom_error_message, loc, all_frames, explanations)
                            }

                            VirtualRootUseOp::Speculation(inner_use_op) => loop_impl(
                                loc,
                                frames,
                                inner_use_op.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            ),

                            // Object operations
                            VirtualRootUseOp::ObjectAddComputedProperty { op } => root(
                                loc,
                                frames,
                                op,
                                RootMessage::RootCannotAddComputedProperty,
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ObjectSpread { op } => root(
                                loc,
                                frames,
                                op,
                                RootMessage::RootCannotSpread(op.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ObjectRest { op } => root(
                                loc,
                                frames,
                                op,
                                RootMessage::RootCannotGetRest(op.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ObjectChain { op } => root(
                                loc,
                                frames,
                                op,
                                RootMessage::RootCannotCallObjectAssign(op.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::AssignVar { var, init } => root(
                                loc,
                                frames,
                                init,
                                RootMessage::RootCannotAssign {
                                    init: init.desc.clone(),
                                    target: var.as_ref().map(|v| v.desc.clone()),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::DeleteVar { var } => root(
                                loc,
                                frames,
                                var,
                                RootMessage::RootCannotDelete(var.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::InitField { op, body } => root(
                                loc,
                                frames,
                                op,
                                RootMessage::RootCannotInitializeField {
                                    field: op.desc.clone(),
                                    body: body.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            // Cast
                            VirtualRootUseOp::Cast { lower, upper } => root(
                                loc,
                                frames,
                                lower,
                                RootMessage::RootCannotCast {
                                    lower: lower.desc.clone(),
                                    upper: upper.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ClassExtendsCheck { extends, def } => root(
                                loc,
                                frames,
                                def,
                                RootMessage::RootCannotExtendClass {
                                    extends: extends.dupe(),
                                    def: def.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ClassMethodDefinition { name, def } => root(
                                loc,
                                frames,
                                def,
                                RootMessage::RootCannotDefineClassMethod {
                                    method_: def.dupe(),
                                    name: name.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ClassImplementsCheck(
                                box ClassImplementsCheckData {
                                    implements, def, ..
                                },
                            ) => root(
                                loc,
                                frames,
                                def,
                                RootMessage::RootCannotImplementClass {
                                    implements: implements.dupe(),
                                    def: def.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ClassOwnProtoCheck(box ClassOwnProtoCheckData {
                                prop,
                                own_loc,
                                proto_loc,
                            }) => {
                                use flow_common::reason::VirtualReasonDesc::RProperty;
                                match (own_loc, proto_loc) {
                                    (None, None) => {
                                        let (all_frames, explanations) = frames;
                                        (None, custom_error_message, loc, all_frames, explanations)
                                    }
                                    (Some(loc_val), None) => {
                                        let def = mk_reason(
                                            RProperty(Some(flow_common::reason::Name::new(
                                                prop.as_str(),
                                            ))),
                                            loc_val.dupe(),
                                        );
                                        root(
                                            loc_of_aloc(loc_val),
                                            frames,
                                            &def,
                                            RootMessage::RootCannotShadowProtoProperty,
                                            custom_error_message,
                                        )
                                    }
                                    (None, Some(loc_val)) => {
                                        let def = mk_reason(
                                            RProperty(Some(flow_common::reason::Name::new(
                                                prop.as_str(),
                                            ))),
                                            loc_val.dupe(),
                                        );
                                        root(
                                            loc_of_aloc(loc_val),
                                            frames,
                                            &def,
                                            RootMessage::RootCannotDefineShadowedProtoProperty,
                                            custom_error_message,
                                        )
                                    }
                                    (Some(own_loc_val), Some(proto_loc_val)) => {
                                        let def = mk_reason(
                                            RProperty(Some(flow_common::reason::Name::new(
                                                prop.as_str(),
                                            ))),
                                            own_loc_val.dupe(),
                                        );
                                        let proto = mk_reason(
                                            RProperty(Some(flow_common::reason::Name::new(
                                                prop.as_str(),
                                            ))),
                                            proto_loc_val.dupe(),
                                        );
                                        root(
                                            loc,
                                            frames,
                                            &def,
                                            RootMessage::RootCannotShadowProto(proto),
                                            custom_error_message,
                                        )
                                    }
                                }
                            }

                            VirtualRootUseOp::Coercion { from, target } => root(
                                loc,
                                frames,
                                from,
                                RootMessage::RootCannotCoerce {
                                    from: from.desc.clone(),
                                    target: target.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ConformToCommonInterface(
                                box ConformToCommonInterfaceData {
                                    self_sig_loc,
                                    self_module_loc,
                                    originate_from_import,
                                },
                            ) => {
                                let (all_frames, mut explanations) = frames;
                                explanations.insert(0, Explanation::ExplanationMultiplatform);
                                let frames = (all_frames, explanations);

                                let root_loc = loc_of_aloc(self_module_loc);
                                let specific_loc = loc_of_aloc(self_sig_loc);

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((
                                        root_loc,
                                        RootMessage::RootCannotConformToCommonInterface {
                                            originate_from_import: *originate_from_import,
                                        },
                                    )),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }

                            VirtualRootUseOp::MergedDeclaration {
                                first_decl,
                                current_decl,
                            } => root(
                                loc,
                                frames,
                                current_decl,
                                RootMessage::RootCannotMergeDeclaration {
                                    first_decl: first_decl.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::DeclareComponentRef { op } => root(
                                loc,
                                frames,
                                op,
                                RootMessage::RootCannotDeclareRef,
                                custom_error_message,
                            ),

                            VirtualRootUseOp::FunCall(box FunCallData { op, fn_, .. }) => {
                                let root_loc = loc_of_aloc(&op.loc);
                                let specific_loc = loc_of_aloc(&fn_.loc);

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((root_loc, RootMessage::RootCannotCall(fn_.desc.clone()))),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }

                            VirtualRootUseOp::FunCallMethod(box FunCallMethodData {
                                op,
                                fn_,
                                prop,
                                ..
                            }) => {
                                let root_loc = loc_of_aloc(&op.loc);
                                let specific_loc = loc_of_aloc(&prop.loc);

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((root_loc, RootMessage::RootCannotCall(fn_.desc.clone()))),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }

                            VirtualRootUseOp::RenderTypeInstantiation { render_type } => root(
                                loc,
                                frames,
                                render_type,
                                RootMessage::RootCannotInstantiateRenderType,
                                custom_error_message,
                            ),

                            VirtualRootUseOp::FunReturnStatement { value } => root(
                                loc,
                                frames,
                                value,
                                RootMessage::RootCannotReturn(value.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::FunImplicitReturn(box FunImplicitReturnData {
                                upper,
                                fn_,
                                type_guard: true,
                            }) => root(
                                loc,
                                frames,
                                upper,
                                RootMessage::RootCannotDeclareTypeGuard {
                                    type_guard_loc: upper.loc.dupe(),
                                    fn_: fn_.dupe(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::FunImplicitReturn(box FunImplicitReturnData {
                                upper,
                                fn_,
                                ..
                            }) => root(
                                loc,
                                frames,
                                upper,
                                RootMessage::RootCannotExpectImplicitReturn {
                                    upper: upper.desc.clone(),
                                    fn_: fn_.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::GeneratorYield { value } => root(
                                loc,
                                frames,
                                value,
                                RootMessage::RootCannotYield(value.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::GetExport(prop) => root(
                                loc,
                                frames,
                                prop,
                                RootMessage::RootCannotGetProp(prop.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::GetProperty(prop) => root(
                                loc,
                                frames,
                                prop,
                                RootMessage::RootCannotGetProp(prop.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::IndexedTypeAccess { object, index } => root(
                                loc,
                                frames,
                                index,
                                RootMessage::RootCannotAccessIndex {
                                    index: index.desc.clone(),
                                    object_: object.desc.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::InferBoundCompatibilityCheck { bound, infer } => {
                                root(
                                    loc,
                                    frames,
                                    bound,
                                    RootMessage::RootCannotUseInferTypeBound {
                                        infer: infer.desc.clone(),
                                    },
                                    custom_error_message,
                                )
                            }

                            VirtualRootUseOp::JSXCreateElement { op, component, .. }
                            | VirtualRootUseOp::ReactCreateElementCall(
                                box ReactCreateElementCallData { op, component, .. },
                            ) => {
                                let root_loc = loc_of_aloc(&op.loc);
                                let specific_loc = loc_of_aloc(&component.loc);

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((
                                        root_loc,
                                        RootMessage::RootCannotCreateElement(
                                            component.desc.clone(),
                                        ),
                                    )),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }

                            VirtualRootUseOp::ReactGetIntrinsic { literal } => root(
                                loc,
                                frames,
                                literal,
                                RootMessage::RootCannotCreateElement(literal.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::RecordCreate(box RecordCreateData {
                                op,
                                constructor,
                                ..
                            }) => {
                                let root_loc = loc_of_aloc(&op.loc);
                                let specific_loc = loc_of_aloc(&constructor.loc);

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((
                                        root_loc,
                                        RootMessage::RootCannotCreateRecord(
                                            constructor.desc.clone(),
                                        ),
                                    )),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }

                            VirtualRootUseOp::TypeApplication { type_ } => root(
                                loc,
                                frames,
                                type_,
                                RootMessage::RootCannotInstantiateTypeApp(type_.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::SetProperty(box SetPropertyData {
                                prop,
                                value,
                                lhs,
                                ..
                            }) => {
                                let loc_reason = if loc_of_aloc(&lhs.loc).contains(&loc) {
                                    &lhs
                                } else {
                                    &value
                                };
                                root(
                                    loc,
                                    frames,
                                    loc_reason,
                                    RootMessage::RootCannotAssign {
                                        init: value.desc.clone(),
                                        target: Some(prop.desc.clone()),
                                    },
                                    custom_error_message,
                                )
                            }

                            VirtualRootUseOp::UpdateProperty { prop, lhs } => root(
                                loc,
                                frames,
                                lhs,
                                RootMessage::RootCannotUpdate(prop.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::DeleteProperty { prop, lhs } => root(
                                loc,
                                frames,
                                lhs,
                                RootMessage::RootCannotDelete(prop.desc.clone()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::RefinementCheck { test, discriminant } => root(
                                loc,
                                frames,
                                test,
                                RootMessage::RootCannotCheckAgainst {
                                    test: test.desc.clone(),
                                    discriminant: discriminant.dupe(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::SwitchRefinementCheck(
                                box SwitchRefinementCheckData { test, discriminant },
                            ) => {
                                let root_loc = loc_of_aloc(test);
                                let specific_loc = root_loc.dupe();

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((
                                        root_loc,
                                        RootMessage::RootCannotCheckAgainstSwitchDiscriminant(
                                            discriminant.dupe(),
                                        ),
                                    )),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }

                            VirtualRootUseOp::EvalMappedType { mapped_type } => root(
                                loc,
                                frames,
                                mapped_type,
                                RootMessage::RootCannotInstantiateEval(mapped_type.dupe()),
                                custom_error_message,
                            ),

                            VirtualRootUseOp::TypeGuardIncompatibility {
                                guard_type,
                                param_name,
                            } => root(
                                loc,
                                frames,
                                guard_type,
                                RootMessage::RootCannotUseTypeGuard {
                                    guard_type: guard_type.dupe(),
                                    param_name: param_name.clone(),
                                },
                                custom_error_message,
                            ),

                            VirtualRootUseOp::ComponentRestParamCompatibility { .. } => {
                                let (all_frames, explanations) = frames;
                                (None, custom_error_message, loc, all_frames, explanations)
                            }

                            VirtualRootUseOp::PositiveTypeGuardConsistency(
                                box PositiveTypeGuardConsistencyData {
                                    return_reason,
                                    param_reason,
                                    guard_type_reason,
                                    is_return_false_statement,
                                    ..
                                },
                            ) => {
                                let (all_frames, mut explanations) = frames;
                                explanations.insert(
                                    0,
                                    Explanation::ExplanationTypeGuardPositiveConsistency {
                                        return_: return_reason.dupe(),
                                        param: param_reason.dupe(),
                                        guard_type: guard_type_reason.dupe(),
                                        is_return_false_statement: *is_return_false_statement,
                                    },
                                );
                                root(
                                    loc,
                                    (all_frames, explanations),
                                    return_reason,
                                    RootMessage::RootCannotReturn(return_reason.desc.clone()),
                                    custom_error_message,
                                )
                            }
                        }
                    }

                    VirtualUseOp::Frame(frame_rc, inner_use_op)
                        if matches!(frame_rc.as_ref(), VirtualFrameUseOp::FunParam(..))
                            && matches!(
                                inner_use_op.as_ref(),
                                VirtualUseOp::Op(spec_rc) if matches!(
                                    spec_rc.as_ref(),
                                    VirtualRootUseOp::Speculation(spec2_rc) if matches!(
                                        spec2_rc.as_ref(),
                                        VirtualUseOp::Op(inner_rc) if matches!(
                                            inner_rc.as_ref(),
                                            VirtualRootUseOp::FunCall(..)
                                                | VirtualRootUseOp::FunCallMethod(..)
                                                | VirtualRootUseOp::JSXCreateElement { .. }
                                        )
                                    )
                                )
                            ) =>
                    {
                        loop_impl(
                            loc,
                            frames,
                            inner_use_op.as_ref().clone(),
                            custom_error_message,
                            loc_of_aloc,
                            loc_of_prop_compatibility_reason,
                            loop_to_form_access_chain,
                            opt_incompatibility_pair,
                            speculation,
                        )
                    }

                    VirtualUseOp::Frame(ref frame_rc, ref inner_use_op)
                        if let VirtualFrameUseOp::FunParam(box FunParamData {
                            n,
                            name,
                            lower: lower_prime,
                            ..
                        }) = frame_rc.as_ref()
                            && let VirtualUseOp::Op(op_rc) = inner_use_op.as_ref()
                            && matches!(
                                op_rc.as_ref(),
                                VirtualRootUseOp::FunCall(..)
                                    | VirtualRootUseOp::FunCallMethod(..)
                                    | VirtualRootUseOp::JSXCreateElement { .. }
                                    | VirtualRootUseOp::RecordCreate(..)
                            ) =>
                    {
                        match op_rc.as_ref() {
                            VirtualRootUseOp::FunCall(box FunCallData { args, fn_, .. })
                            | VirtualRootUseOp::FunCallMethod(box FunCallMethodData {
                                args,
                                fn_,
                                ..
                            }) => {
                                // Get the actual argument at position n-1, or fallback to lower_prime
                                let lower = if (*n as usize) <= args.len() {
                                    &args[(*n as usize) - 1]
                                } else {
                                    lower_prime
                                };

                                let root_msg = match name {
                                    Some(name_str) => RootMessage::RootCannotCallWithNamedParam {
                                        fn_: fn_.desc.clone(),
                                        lower: lower.desc.clone(),
                                        name: name_str.clone(),
                                    },
                                    None => RootMessage::RootCannotCallWithNthParam {
                                        fn_: fn_.desc.clone(),
                                        lower: lower.desc.clone(),
                                        n: *n,
                                    },
                                };
                                root(loc, frames, lower, root_msg, custom_error_message)
                            }
                            VirtualRootUseOp::JSXCreateElement { op, component, .. } => {
                                let root_loc = loc_of_aloc(&op.loc);
                                let specific_loc = loc_of_aloc(&component.loc);

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((
                                        root_loc,
                                        RootMessage::RootCannotCreateElement(
                                            component.desc.clone(),
                                        ),
                                    )),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }
                            VirtualRootUseOp::RecordCreate(box RecordCreateData {
                                op,
                                constructor,
                                ..
                            }) => {
                                let root_loc = loc_of_aloc(&op.loc);
                                let specific_loc = loc_of_aloc(&constructor.loc);

                                let final_loc = if root_loc.contains(&loc) && root_loc != loc {
                                    loc
                                } else {
                                    specific_loc
                                };

                                let (all_frames, explanations) = frames;
                                (
                                    Some((
                                        root_loc,
                                        RootMessage::RootCannotCreateRecord(
                                            constructor.desc.clone(),
                                        ),
                                    )),
                                    custom_error_message,
                                    final_loc,
                                    all_frames,
                                    explanations,
                                )
                            }
                            _ => {
                                let (all_frames, explanations) = frames;
                                (None, custom_error_message, loc, all_frames, explanations)
                            }
                        }
                    }

                    VirtualUseOp::Frame(frame, inner) => match frame.as_ref() {
                        VirtualFrameUseOp::FunRestParam { .. } => loop_impl(
                            loc,
                            frames,
                            inner.as_ref().clone(),
                            custom_error_message,
                            loc_of_aloc,
                            loc_of_prop_compatibility_reason,
                            loop_to_form_access_chain,
                            opt_incompatibility_pair,
                            speculation,
                        ),

                        VirtualFrameUseOp::UnifyFlip => {
                            if let VirtualUseOp::Frame(inner_frame, _) = inner.as_ref() {
                                if matches!(
                                    inner_frame.as_ref(),
                                    VirtualFrameUseOp::ArrayElementCompatibility { .. }
                                ) {
                                    let (all_frames, mut explanations) = frames;
                                    explanations
                                        .insert(0, Explanation::ExplanationArrayInvariantTyping);
                                    loop_impl(
                                        loc,
                                        (all_frames, explanations),
                                        inner.as_ref().clone(),
                                        custom_error_message,
                                        loc_of_aloc,
                                        loc_of_prop_compatibility_reason,
                                        loop_to_form_access_chain,
                                        opt_incompatibility_pair,
                                        speculation,
                                    )
                                } else if matches!(
                                    inner_frame.as_ref(),
                                    VirtualFrameUseOp::PropertyCompatibility(..)
                                ) {
                                    let (all_frames, mut explanations) = frames;
                                    explanations
                                        .push(Explanation::ExplanationPropertyInvariantTyping);
                                    loop_impl(
                                        loc,
                                        (all_frames, explanations),
                                        inner.as_ref().clone(),
                                        custom_error_message,
                                        loc_of_aloc,
                                        loc_of_prop_compatibility_reason,
                                        loop_to_form_access_chain,
                                        opt_incompatibility_pair,
                                        speculation,
                                    )
                                } else {
                                    loop_impl(
                                        loc,
                                        frames,
                                        inner.as_ref().clone(),
                                        custom_error_message,
                                        loc_of_aloc,
                                        loc_of_prop_compatibility_reason,
                                        loop_to_form_access_chain,
                                        opt_incompatibility_pair,
                                        speculation,
                                    )
                                }
                            } else {
                                loop_impl(
                                    loc,
                                    frames,
                                    inner.as_ref().clone(),
                                    custom_error_message,
                                    loc_of_aloc,
                                    loc_of_prop_compatibility_reason,
                                    loop_to_form_access_chain,
                                    opt_incompatibility_pair,
                                    speculation,
                                )
                            }
                        }

                        VirtualFrameUseOp::OpaqueTypeLowerBound { .. }
                        | VirtualFrameUseOp::OpaqueTypeUpperBound { .. }
                        | VirtualFrameUseOp::FunMissingArg(..)
                        | VirtualFrameUseOp::ImplicitTypeParam
                        | VirtualFrameUseOp::ReactConfigCheck
                        | VirtualFrameUseOp::ReactGetConfig { .. }
                        | VirtualFrameUseOp::MappedTypeKeyCompatibility { .. }
                        | VirtualFrameUseOp::TupleAssignment { .. }
                        | VirtualFrameUseOp::RendersCompatibility => loop_impl(
                            loc,
                            frames,
                            inner.as_ref().clone(),
                            custom_error_message,
                            loc_of_aloc,
                            loc_of_prop_compatibility_reason,
                            loop_to_form_access_chain,
                            opt_incompatibility_pair,
                            speculation,
                        ),

                        VirtualFrameUseOp::ReactDeepReadOnly(box (_, DroType::DebugAnnot)) => {
                            loop_impl(
                                loc,
                                frames,
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::FunReturn { lower, upper } => {
                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair = opt_incompatibility_pair(
                                inner.as_ref(),
                                &(lower.dupe(), upper.dupe()),
                            );
                            all_frames.insert(
                                0,
                                ErrorFrame::FrameReturnValue {
                                    incompatibility_pair,
                                },
                            );
                            loop_impl(
                                loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::TypeParamBound { name } => {
                            let (mut all_frames, explanations) = frames;
                            all_frames.insert(
                                0,
                                ErrorFrame::FrameTypeParameterBound(
                                    name.string_of_subst_name().dupe(),
                                ),
                            );
                            loop_impl(
                                loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::TypeGuardCompatibility => {
                            let (mut all_frames, explanations) = frames;
                            all_frames.insert(0, ErrorFrame::FrameTypePredicate);
                            loop_impl(
                                loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::UnionRepresentative { union } => {
                            let frame_loc = loc_of_aloc(&union.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };
                            let (mut all_frames, explanations) = frames;
                            all_frames
                                .insert(0, ErrorFrame::FrameUnionRepresentative(union.dupe()));
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::ReactDeepReadOnly(box (props_loc, DroType::Props)) => {
                            let (all_frames, mut explanations) = frames;
                            explanations.insert(
                                0,
                                Explanation::ExplanationReactComponentPropsDeepReadOnly(
                                    props_loc.dupe(),
                                ),
                            );
                            loop_impl(
                                loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::ReactDeepReadOnly(box (props_loc, DroType::HookArg)) => {
                            let (all_frames, mut explanations) = frames;
                            explanations.insert(
                                0,
                                Explanation::ExplanationReactHookArgsDeepReadOnly(props_loc.dupe()),
                            );
                            loop_impl(
                                loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::ReactDeepReadOnly(box (
                            hook_loc,
                            DroType::HookReturn,
                        )) => {
                            let (all_frames, mut explanations) = frames;
                            explanations.insert(
                                0,
                                Explanation::ExplanationReactHookReturnDeepReadOnly(
                                    hook_loc.dupe(),
                                ),
                            );
                            loop_impl(
                                loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::ConstrainedAssignment(
                            box ConstrainedAssignmentData {
                                name,
                                declaration,
                                providers,
                            },
                        ) => {
                            let (all_frames, mut explanations) = frames;
                            explanations.insert(
                                0,
                                Explanation::ExplanationConstrainedAssign(Box::new(
                                    ExplanationConstrainedAssignData {
                                        name: name.clone(),
                                        declaration: declaration.dupe(),
                                        providers: providers.clone(),
                                    },
                                )),
                            );
                            loop_impl(
                                loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::FunCompatibility { lower, .. } => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };
                            loop_impl(
                                final_loc,
                                frames,
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::ArrayElementCompatibility { lower, upper } => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair = opt_incompatibility_pair(
                                inner.as_ref(),
                                &(lower.dupe(), upper.dupe()),
                            );
                            all_frames.insert(
                                0,
                                ErrorFrame::FrameArrayElement {
                                    incompatibility_pair,
                                },
                            );
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::IndexerKeyCompatibility { lower, upper } => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair = opt_incompatibility_pair(
                                inner.as_ref(),
                                &(lower.dupe(), upper.dupe()),
                            );
                            all_frames.insert(
                                0,
                                ErrorFrame::FrameIndexerPropertyKey {
                                    incompatibility_pair,
                                },
                            );
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::TypeArgCompatibility(box TypeArgCompatibilityData {
                            targ,
                            lower,
                            ..
                        }) => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            all_frames.insert(0, ErrorFrame::FrameTypeArgument(targ.dupe()));
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::EnumRepresentationTypeCompatibility {
                            lower, ..
                        } => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            all_frames.insert(0, ErrorFrame::FrameEnumRepresentationType);
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::FunParam(box FunParamData {
                            n,
                            lower,
                            upper,
                            name,
                        }) if matches!(
                            inner.as_ref(),
                            VirtualUseOp::Frame(f_rc, _) if matches!(
                                f_rc.as_ref(),
                                VirtualFrameUseOp::FunCompatibility { .. }
                            )
                        ) =>
                        {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair = opt_incompatibility_pair(
                                inner.as_ref(),
                                &(lower.dupe(), upper.dupe()),
                            );

                            let frame = if name.as_deref() == Some("this") {
                                ErrorFrame::FrameFunThisParam {
                                    incompatibility_pair,
                                }
                            } else {
                                ErrorFrame::FrameFunNthParam {
                                    n: *n,
                                    incompatibility_pair,
                                }
                            };

                            all_frames.insert(0, frame);
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::FunParam(box FunParamData {
                            n,
                            lower,
                            upper,
                            name,
                        }) => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair = opt_incompatibility_pair(
                                inner.as_ref(),
                                &(lower.dupe(), upper.dupe()),
                            );

                            let frame = if name.as_deref() == Some("this") {
                                ErrorFrame::FrameFunThisArgument {
                                    incompatibility_pair,
                                }
                            } else {
                                ErrorFrame::FrameFunNthArgument {
                                    n: *n,
                                    incompatibility_pair,
                                }
                            };

                            all_frames.insert(0, frame);
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::PropertyCompatibility(
                            box PropertyCompatibilityData {
                                prop: None,
                                lower,
                                upper,
                            },
                        ) => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair = opt_incompatibility_pair(
                                inner.as_ref(),
                                &(lower.dupe(), upper.dupe()),
                            );

                            all_frames.insert(
                                0,
                                ErrorFrame::FrameIndexerProperty {
                                    incompatibility_pair,
                                },
                            );
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::PropertyCompatibility(
                            box PropertyCompatibilityData {
                                prop: Some(name),
                                lower,
                                upper,
                            },
                        ) if name.as_str() == "$call" => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair = opt_incompatibility_pair(
                                inner.as_ref(),
                                &(lower.dupe(), upper.dupe()),
                            );

                            all_frames.insert(
                                0,
                                ErrorFrame::FrameCallableSignature {
                                    incompatibility_pair,
                                },
                            );
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::OpaqueTypeLowerBoundCompatibility { lower, .. }
                        | VirtualFrameUseOp::OpaqueTypeUpperBoundCompatibility { lower, .. } => {
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            let (mut all_frames, explanations) = frames;
                            all_frames.insert(0, ErrorFrame::FrameAnonymous);
                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility(
                            box OpaqueTypeCustomErrorCompatibilityData {
                                lower,
                                upper,
                                lower_t,
                                upper_t,
                                name,
                                custom_error_loc,
                            },
                        ) => {
                            let lower_desc = match lower_t {
                                TypeOrTypeDescT::Type(_) => Err(lower.desc.clone()),
                                TypeOrTypeDescT::TypeDesc(d) => d.clone(),
                            };
                            let upper_desc = match upper_t {
                                TypeOrTypeDescT::Type(_) => Err(upper.desc.clone()),
                                TypeOrTypeDescT::TypeDesc(d) => d.clone(),
                            };

                            // Keep as Message since we're in the generic context
                            let custom_error_message =
                                Some(Message::MessageIncompatibleGeneralWithPrintedTypes(
                                    Box::new(MessageIncompatibleGeneralWithPrintedTypesData {
                                        lower_loc: lower.loc.dupe(),
                                        upper_loc: upper.loc.dupe(),
                                        lower_desc,
                                        upper_desc,
                                    }),
                                ));

                            // Keep as Explanation
                            let frames = (
                                vec![],
                                vec![Explanation::ExplanationCustomError(Box::new(
                                    ExplanationCustomErrorData {
                                        name: name.clone(),
                                        custom_error_loc: custom_error_loc.dupe(),
                                    },
                                ))],
                            );

                            let loc = loc_of_aloc(&lower.loc);
                            let frame_loc = loc_of_aloc(&lower.loc);
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc
                            };

                            loop_impl(
                                final_loc,
                                frames,
                                inner.as_ref().clone(),
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::PropertyCompatibility(
                            box PropertyCompatibilityData {
                                prop: Some(prop),
                                lower,
                                upper,
                            },
                        ) => {
                            let lower_loc =
                                loc_of_prop_compatibility_reason(loc.dupe(), lower, &inner);

                            let (lower_loc, props, use_op) =
                                loop_to_form_access_chain(lower_loc, inner.as_ref().clone());

                            let frame_loc = lower_loc;
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc.dupe()
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair =
                                opt_incompatibility_pair(&use_op, &(lower.dupe(), upper.dupe()));

                            let mut chain_vec = vec![AccessChainSegment::PropSegment(
                                flow_common::reason::Name::new(prop.as_str()),
                            )];
                            chain_vec.extend(props);
                            let chain =
                                Vec1::try_from_vec(chain_vec).expect("chain must be non-empty");

                            all_frames.insert(
                                0,
                                ErrorFrame::FrameAccessChain {
                                    chain,
                                    incompatibility_pair,
                                },
                            );

                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                use_op,
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }

                        VirtualFrameUseOp::TupleElementCompatibility(
                            box TupleElementCompatibilityData {
                                n, lower, upper, ..
                            },
                        ) => {
                            let lower_loc = loc_of_aloc(&lower.loc);

                            let (lower_loc, props, use_op) =
                                loop_to_form_access_chain(lower_loc, inner.as_ref().clone());

                            let frame_loc = lower_loc;
                            let final_loc = if frame_loc.contains(&loc) {
                                loc
                            } else {
                                frame_loc.dupe()
                            };

                            let (mut all_frames, explanations) = frames;
                            let incompatibility_pair =
                                opt_incompatibility_pair(&use_op, &(lower.dupe(), upper.dupe()));

                            let mut chain_vec = vec![AccessChainSegment::TupleIndexSegment(*n)];
                            chain_vec.extend(props);
                            let chain =
                                Vec1::try_from_vec(chain_vec).expect("chain must be non-empty");

                            all_frames.insert(
                                0,
                                ErrorFrame::FrameAccessChain {
                                    chain,
                                    incompatibility_pair,
                                },
                            );

                            loop_impl(
                                final_loc,
                                (all_frames, explanations),
                                use_op,
                                custom_error_message,
                                loc_of_aloc,
                                loc_of_prop_compatibility_reason,
                                loop_to_form_access_chain,
                                opt_incompatibility_pair,
                                speculation,
                            )
                        }
                    },
                }
            }

            loop_impl(
                initial_loc,
                (vec![], vec![]),
                initial_use_op,
                None,
                &loc_of_aloc,
                &loc_of_prop_compatibility_reason,
                &loop_to_form_access_chain,
                &opt_incompatibility_pair,
                speculation,
            )
        }
    }

    let loc = error.loc_of_error();
    let msg = error.msg_of_error();
    let source_file = error.source_file();
    let kind = msg.kind_of_msg();

    let mk_use_op_error = |loc: Loc,
                           use_op: VirtualUseOp<L>,
                           explanation: Option<Explanation<L>>,
                           message: Message<L>|
     -> IntermediateError<L> {
        let unwrapper = unwrap_use_ops(&loc_of_aloc, speculation);
        let (root, custom_error_msg, loc, frames, explanations) = unwrapper(loc, use_op);

        let code = error.code_of_error();
        let explanations = match explanation {
            Some(exp) => {
                let mut exps = explanations;
                exps.insert(0, exp);
                Some(exps)
            }
            None => Some(explanations),
        };
        let message = match custom_error_msg {
            Some(ref custom) => custom.clone(),
            None => message,
        };
        mk_error(kind, loc, code, message, root, Some(frames), explanations)
    };

    let mk_use_op_error_reason = |reason: &VirtualReason<L>,
                                  use_op: VirtualUseOp<L>,
                                  explanation: Option<Explanation<L>>,
                                  message: Message<L>|
     -> IntermediateError<L> {
        mk_use_op_error(loc_of_aloc(&reason.loc), use_op, explanation, message)
    };

    let mk_no_frame_or_explanation_error =
        |reason: &VirtualReason<L>, message: Message<L>| -> IntermediateError<L> {
            let loc = loc_of_aloc(&reason.loc);
            let code = error.code_of_error();
            mk_error(kind, loc, code, message, None, Some(vec![]), Some(vec![]))
        };

    let mk_prop_missing_in_lookup_error = |loc: Loc,
                                           prop: Option<FlowSmolStr>,
                                           lower: VirtualReason<L>,
                                           use_op: VirtualUseOp<L>,
                                           suggestion: Option<FlowSmolStr>,
                                           reason_indexer: Option<VirtualReason<L>>|
     -> IntermediateError<L> {
        let lower = mod_lower_reason_according_to_use_ops(lower.dupe(), &use_op);
        mk_use_op_error(
            loc,
            use_op,
            None,
            Message::MessagePropMissing(Box::new(MessagePropMissingData {
                lower,
                upper: None,
                prop,
                suggestion,
                reason_indexer,
            })),
        )
    };

    let mk_prop_missing_in_subtyping_error = |prop: Option<FlowSmolStr>,
                                              suggestion: Option<FlowSmolStr>,
                                              lower: VirtualReason<L>,
                                              upper: VirtualReason<L>,
                                              reason_indexer: Option<VirtualReason<L>>,
                                              use_op: VirtualUseOp<L>|
     -> IntermediateError<L> {
        let loc = loc_of_aloc(&lower.loc);
        let lower = mod_lower_reason_according_to_use_ops(lower.dupe(), &use_op);
        mk_use_op_error(
            loc,
            use_op,
            None,
            Message::MessagePropMissing(Box::new(MessagePropMissingData {
                lower,
                upper: Some(upper),
                prop,
                suggestion,
                reason_indexer,
            })),
        )
    };

    let mk_props_missing_in_subtyping_error = |props: Vec1<FlowSmolStr>,
                                               lower: VirtualReason<L>,
                                               upper: VirtualReason<L>,
                                               use_op: VirtualUseOp<L>|
     -> IntermediateError<L> {
        let loc = loc_of_aloc(&lower.loc);
        let lower = mod_lower_reason_according_to_use_ops(lower.dupe(), &use_op);
        mk_use_op_error(
            loc,
            use_op,
            None,
            Message::MessagePropsMissing(Box::new(MessagePropsMissingData {
                lower,
                upper,
                props,
            })),
        )
    };

    let mk_prop_polarity_mismatch_error =
        |lower: VirtualReason<L>,
         upper: VirtualReason<L>,
         props: Vec1<(Option<FlowSmolStr>, Polarity, Polarity)>,
         use_op: VirtualUseOp<L>|
         -> IntermediateError<L> {
            let use_op = match &use_op {
                VirtualUseOp::Frame(frame_rc, inner_use_op) => match frame_rc.as_ref() {
                    VirtualFrameUseOp::PropertyCompatibility(box PropertyCompatibilityData {
                        prop,
                        ..
                    }) => {
                        let prop_str: Option<FlowSmolStr> =
                            prop.as_ref().map(|n| n.as_str().into());
                        if props.iter().any(|(p, _, _)| p == &prop_str) {
                            inner_use_op.dupe().as_ref().clone()
                        } else {
                            use_op
                        }
                    }
                    _ => use_op,
                },
                _ => use_op,
            };

            mk_use_op_error_reason(
                &lower,
                use_op,
                None,
                Message::MessagePropPolarityMismatch(Box::new(MessagePropPolarityMismatchData {
                    lower: lower.dupe(),
                    upper: upper.dupe(),
                    props,
                })),
            )
        };

    let mk_incompatible_invariant_subtyping_error =
        |additional_explanation: Option<Explanation<L>>,
         sub_component: Option<SubComponentOfInvariantSubtypingError>,
         lower_loc: L,
         upper_loc: L,
         lower_desc: Result<ALocTy, VirtualReasonDesc<L>>,
         upper_desc: Result<ALocTy, VirtualReasonDesc<L>>,
         use_op: VirtualUseOp<L>|
         -> IntermediateError<L> {
            mk_use_op_error(
                loc_of_aloc(&lower_loc),
                use_op,
                additional_explanation,
                Message::MessageIncompatibleDueToInvariantSubtyping(Box::new(
                    MessageIncompatibleDueToInvariantSubtypingData {
                        sub_component,
                        lower_loc,
                        upper_loc,
                        lower_desc,
                        upper_desc,
                    },
                )),
            )
        };

    let mk_props_missing_in_invariant_subtyping_error =
        |props: Vec1<FlowSmolStr>,
         reason_lower: VirtualReason<L>,
         reason_upper: VirtualReason<L>,
         lower_obj_loc: L,
         upper_obj_loc: L,
         lower_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
         upper_obj_desc: Result<ALocTy, VirtualReasonDesc<L>>,
         use_op: VirtualUseOp<L>|
         -> IntermediateError<L> {
            let reason_lower = mod_lower_reason_according_to_use_ops(reason_lower.dupe(), &use_op);
            let props_plural = props.len() > 1;
            let explanation = Some(
                Explanation::ExplanationPropertyMissingDueToNeutralOptionalProperty(Box::new(
                    ExplanationPropertyMissingDueToNeutralOptionalPropertyData {
                        props_plural,
                        lower_obj_loc,
                        upper_obj_loc,
                        lower_obj_desc,
                        upper_obj_desc,
                        upper_object_reason: reason_upper.dupe(),
                    },
                )),
            );
            mk_use_op_error(
                loc_of_aloc(&reason_lower.loc),
                use_op,
                explanation,
                Message::MessagePropsMissing(Box::new(MessagePropsMissingData {
                    lower: reason_lower,
                    upper: reason_upper,
                    props,
                })),
            )
        };

    let mk_use_op_speculation_error = |loc: L,
                                       use_op: VirtualUseOp<L>,
                                       branches: Vec<FlowErrorMessage<L>>|
     -> IntermediateError<L> {
        use super::flow_error::error_of_msg;

        if let VirtualUseOp::Frame(frame, inner_use_op) = &use_op {
            if let VirtualFrameUseOp::MappedTypeKeyCompatibility {
                source_type,
                mapped_type,
            } = frame.as_ref()
            {
                return mk_use_op_error(
                    loc_of_aloc(&source_type.loc),
                    inner_use_op.as_ref().clone(),
                    None,
                    Message::MessageIncompatibleMappedTypeKey {
                        source_type: source_type.dupe(),
                        mapped_type: mapped_type.dupe(),
                    },
                );
            }
        }

        let unwrapper = unwrap_use_ops(&loc_of_aloc, speculation);
        let (root, custom_error_msg, unwrapped_loc, frames, explanations) =
            unwrapper(loc_of_aloc(&loc), use_op);

        let error_code = error.code_of_error();

        match custom_error_msg {
            Some(custom_msg) => mk_error(
                kind,
                unwrapped_loc,
                error_code,
                custom_msg,
                root,
                Some(frames),
                Some(explanations),
            ),
            None => {
                let speculation_errors: Vec<(i32, IntermediateError<L>)> = branches
                    .into_iter()
                    .map(|msg| {
                        let score = score_of_msg(&msg);
                        let branch_error = error_of_msg(source_file.dupe(), msg);
                        let intermediate =
                            make_intermediate_error(loc_of_aloc.clone(), true, &branch_error);
                        (score, intermediate)
                    })
                    .collect();

                mk_speculation_error(
                    kind,
                    unwrapped_loc,
                    root,
                    frames,
                    explanations,
                    error_code,
                    speculation_errors,
                )
            }
        }
    };

    //
    // An error between two incompatible types. A "lower" type and an "upper" type.
    let mk_incompatible_error = |additional_explanation: Option<Explanation<L>>,
                                 lower: VirtualReason<L>,
                                 upper: VirtualReason<L>,
                                 use_op: VirtualUseOp<L>|
     -> IntermediateError<L> {
        // Flip contravariant positions
        let ((lower, upper), use_op) = flip_contravariant(lower, upper, use_op);

        // Local helper to create error with use_op context
        let make_error = |reason: &VirtualReason<L>, message: Message<L>| -> IntermediateError<L> {
            mk_use_op_error_reason(
                reason,
                use_op.clone(),
                additional_explanation.clone(),
                message,
            )
        };

        // Modify lower and upper according to use_ops
        let lower = mod_lower_reason_according_to_use_ops(lower, &use_op);
        let upper = mod_upper_reason_according_to_use_ops(upper, &use_op);

        match &use_op {
            VirtualUseOp::Op(root) => match root.as_ref() {
                VirtualRootUseOp::Coercion { from, .. } => mk_use_op_error_reason(
                    from,
                    use_op.clone(),
                    additional_explanation.clone(),
                    Message::MessageShouldNotBeCoerced(lower.dupe()),
                ),
                _ => {
                    let root_use_op = root_of_use_op(&use_op);
                    match root_use_op {
                        VirtualRootUseOp::FunImplicitReturn(box FunImplicitReturnData {
                            upper: return_reason,
                            ..
                        }) => {
                            if upper.loc() == return_reason.loc() {
                                let upper_desc = upper.desc(is_scalar_reason(&upper)).clone();
                                make_error(
                                    &lower,
                                    Message::MessageIncompatibleImplicitReturn(Box::new(
                                        MessageIncompatibleImplicitReturnData {
                                            lower: lower.dupe(),
                                            upper: upper_desc,
                                        },
                                    )),
                                )
                            } else {
                                make_error(
                                    &lower,
                                    Message::MessageIncompatibleGeneral {
                                        lower: lower.dupe(),
                                        upper: upper.dupe(),
                                    },
                                )
                            }
                        }
                        VirtualRootUseOp::ComponentRestParamCompatibility { rest_param } => {
                            mk_no_frame_or_explanation_error(
                                rest_param,
                                Message::MessageIncompatibleComponentRestParam(rest_param.dupe()),
                            )
                        }
                        _ => make_error(
                            &lower,
                            Message::MessageIncompatibleGeneral {
                                lower: lower.dupe(),
                                upper: upper.dupe(),
                            },
                        ),
                    }
                }
            },

            VirtualUseOp::Frame(frame, inner_use_op) => match frame.as_ref() {
                VirtualFrameUseOp::TupleElementCompatibility(
                    box TupleElementCompatibilityData { upper_optional, .. },
                ) if *upper_optional
                    && matches!(
                        lower.desc(is_scalar_reason(&lower)),
                        VirtualReasonDesc::RVoid
                    ) =>
                {
                    let upper_loc = upper.loc().dupe();
                    let new_upper =
                        mk_reason(VirtualReasonDesc::RTupleElement { name: None }, upper_loc);
                    make_error(
                        &lower,
                        Message::MessageCannotAssignToOptionalTupleElement {
                            lower: lower.dupe(),
                            upper: new_upper,
                        },
                    )
                }

                VirtualFrameUseOp::TupleAssignment { upper_optional }
                    if *upper_optional
                        && matches!(
                            lower.desc(is_scalar_reason(&lower)),
                            VirtualReasonDesc::RVoid
                        ) =>
                {
                    let upper_loc = upper.loc().dupe();
                    let new_upper =
                        mk_reason(VirtualReasonDesc::RTupleElement { name: None }, upper_loc);
                    make_error(
                        &lower,
                        Message::MessageCannotAssignToOptionalTupleElement {
                            lower: lower.dupe(),
                            upper: new_upper,
                        },
                    )
                }

                VirtualFrameUseOp::FunMissingArg(box FunMissingArgData { def, op, .. }) => {
                    let message = match inner_use_op.as_ref() {
                        VirtualUseOp::Op(inner_root) => match inner_root.as_ref() {
                            VirtualRootUseOp::FunCall(..) | VirtualRootUseOp::FunCallMethod(..) => {
                                let new_def = def.dupe().update_desc(|desc| match desc {
                                    VirtualReasonDesc::RFunctionType => {
                                        VirtualReasonDesc::RFunction(ReasonDescFunction::RNormal)
                                    }
                                    _ => desc,
                                });
                                Message::MessageFunctionRequiresAnotherArgument {
                                    def: new_def,
                                    from: None,
                                }
                            }
                            _ => Message::MessageFunctionRequiresAnotherArgument {
                                def: def.dupe(),
                                from: Some(op.dupe()),
                            },
                        },
                        _ => Message::MessageFunctionRequiresAnotherArgument {
                            def: def.dupe(),
                            from: Some(op.dupe()),
                        },
                    };
                    mk_use_op_error_reason(
                        op,
                        use_op.clone(),
                        additional_explanation.clone(),
                        message,
                    )
                }

                VirtualFrameUseOp::RendersCompatibility => make_error(
                    &lower,
                    Message::MessageDoesNotRender {
                        lower: lower.dupe(),
                        upper: upper.dupe(),
                    },
                ),

                VirtualFrameUseOp::UnionRepresentative { union } => mk_use_op_error_reason(
                    union,
                    inner_use_op.as_ref().clone(),
                    additional_explanation.clone(),
                    Message::MessageIncompatibleWithUnionRepresentative {
                        union: union.dupe(),
                        lower: lower.dupe(),
                        upper: upper.dupe(),
                    },
                ),

                _ => {
                    let root_use_op = root_of_use_op(&use_op);
                    match root_use_op {
                        VirtualRootUseOp::FunImplicitReturn(box FunImplicitReturnData {
                            upper: return_reason,
                            ..
                        }) => {
                            if upper.loc() == return_reason.loc() {
                                let upper_desc = upper.desc(is_scalar_reason(&upper)).clone();
                                make_error(
                                    &lower,
                                    Message::MessageIncompatibleImplicitReturn(Box::new(
                                        MessageIncompatibleImplicitReturnData {
                                            lower: lower.dupe(),
                                            upper: upper_desc,
                                        },
                                    )),
                                )
                            } else {
                                make_error(
                                    &lower,
                                    Message::MessageIncompatibleGeneral {
                                        lower: lower.dupe(),
                                        upper: upper.dupe(),
                                    },
                                )
                            }
                        }
                        VirtualRootUseOp::ComponentRestParamCompatibility { rest_param } => {
                            mk_no_frame_or_explanation_error(
                                rest_param,
                                Message::MessageIncompatibleComponentRestParam(rest_param.dupe()),
                            )
                        }
                        _ => make_error(
                            &lower,
                            Message::MessageIncompatibleGeneral {
                                lower: lower.dupe(),
                                upper: upper.dupe(),
                            },
                        ),
                    }
                }
            },
        }
    };

    // An error between two incompatible types. The use_kind specifies the kind of incompatibility.
    // Unlike mk_incompatible_error, this handles specific use type incompatibilities.
    let mk_incompatible_use_error = |use_loc: Loc,
                                     use_kind: super::error_message::UpperKind<L>,
                                     lower: VirtualReason<L>,
                                     upper: VirtualReason<L>,
                                     use_op: VirtualUseOp<L>|
     -> IntermediateError<L> {
        use super::error_message::UpperKind;

        let lower = mod_lower_reason_according_to_use_ops(lower, &use_op);

        match use_kind {
            UpperKind::IncompatibleElemTOfArrT => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageLowerIsNotArrayIndex(lower),
            ),

            UpperKind::IncompatibleGetPrivatePropT | UpperKind::IncompatibleSetPrivatePropT => {
                mk_use_op_error(
                    use_loc,
                    use_op,
                    None,
                    Message::MessageLowerIsNotClassWithPrivateProps(lower),
                )
            }

            UpperKind::IncompatibleMixedCallT => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageUnknownParameterTypes(lower),
            ),

            UpperKind::IncompatibleCallT => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageLowerIsNotFunction(lower),
            ),

            UpperKind::IncompatibleObjAssignFromTSpread | UpperKind::IncompatibleArrRestT => {
                mk_use_op_error(
                    use_loc,
                    use_op,
                    None,
                    Message::MessageLowerIsNotArray(lower),
                )
            }

            UpperKind::IncompatibleObjAssignFromT
            | UpperKind::IncompatibleObjRestT
            | UpperKind::IncompatibleGetKeysT
            | UpperKind::IncompatibleGetValuesT => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageLowerIsNotObject(lower),
            ),

            UpperKind::IncompatibleMapTypeTObject => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageInvalidArgument { lower, upper },
            ),

            UpperKind::IncompatibleMixinT | UpperKind::IncompatibleThisSpecializeT => {
                mk_use_op_error(
                    use_loc,
                    use_op,
                    None,
                    Message::MessageLowerIsNotClass(lower),
                )
            }

            UpperKind::IncompatibleSpecializeT | UpperKind::IncompatibleVarianceCheckT => {
                mk_use_op_error(
                    use_loc,
                    use_op,
                    None,
                    Message::MessageLowerIsNotPolymorphicType(lower),
                )
            }

            UpperKind::IncompatibleSuperT => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageLowerIsNotInheritable(lower),
            ),

            UpperKind::IncompatibleGetPropT(prop_loc, prop)
            | UpperKind::IncompatibleSetPropT(prop_loc, prop)
            | UpperKind::IncompatibleHasOwnPropT(prop_loc, prop)
            | UpperKind::IncompatibleMethodT(prop_loc, prop) => mk_prop_missing_in_lookup_error(
                loc_of_aloc(&prop_loc),
                prop.map(|p| p.into_smol_str()),
                lower,
                use_op,
                None,
                None,
            ),

            UpperKind::IncompatibleGetElemT(prop_loc)
            | UpperKind::IncompatibleSetElemT(prop_loc)
            | UpperKind::IncompatibleCallElemT(prop_loc) => mk_prop_missing_in_lookup_error(
                loc_of_aloc(&prop_loc),
                None,
                lower,
                use_op,
                None,
                None,
            ),

            UpperKind::IncompatibleGetStaticsT => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageLowerIsNotInstanceType(lower),
            ),

            UpperKind::IncompatibleBindT => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageLowerIsNotFunctionType(lower),
            ),

            UpperKind::IncompatibleUnclassified(ctor) => mk_use_op_error(
                use_loc,
                use_op,
                None,
                Message::MessageLowerIsNotSupportedByUnclassifiedUse {
                    lower,
                    ctor: ctor.to_string().into(),
                },
            ),
        }
    };

    use super::error_message::FriendlyMessageRecipe;

    let recipe = msg.clone().friendly_message_of_msg();
    let mut intermediate_error = match (loc.as_ref(), recipe) {
        (Some(loc), FriendlyMessageRecipe::Normal(message)) => mk_error(
            kind,
            loc_of_aloc(loc),
            error.code_of_error(),
            message,
            None,
            None,
            None,
        ),

        (
            None,
            FriendlyMessageRecipe::UseOp(box UseOpData {
                loc,
                message,
                use_op,
                explanation,
            }),
        ) => mk_use_op_error(loc_of_aloc(&loc), use_op, explanation, message),

        (
            None,
            FriendlyMessageRecipe::PropMissingInLookup(box PropMissingInLookupData {
                loc,
                prop,
                reason_obj,
                use_op,
                suggestion,
                reason_indexer,
            }),
        ) => mk_prop_missing_in_lookup_error(
            loc_of_aloc(&loc),
            prop.dupe(),
            reason_obj,
            use_op,
            suggestion.dupe(),
            reason_indexer,
        ),

        (
            None,
            FriendlyMessageRecipe::PropMissingInSubtyping(box PropMissingInSubtypingData {
                prop,
                reason_lower,
                reason_upper,
                reason_indexer,
                suggestion,
                use_op,
            }),
        ) => mk_prop_missing_in_subtyping_error(
            prop.dupe(),
            suggestion.dupe(),
            reason_lower,
            reason_upper,
            reason_indexer,
            use_op,
        ),

        (
            None,
            FriendlyMessageRecipe::PropsMissingInSubtyping(box PropsMissingInSubtypingData {
                props,
                reason_lower,
                reason_upper,
                use_op,
            }),
        ) => mk_props_missing_in_subtyping_error(
            Vec1::try_from_vec(props.iter().map(|s| FlowSmolStr::new(s.as_str())).collect())
                .unwrap(),
            reason_lower,
            reason_upper,
            use_op,
        ),

        (
            None,
            FriendlyMessageRecipe::PropsMissingInInvariantSubtyping(
                box PropsMissingInInvariantSubtypingData {
                    props,
                    reason_lower,
                    reason_upper,
                    lower_obj_loc,
                    upper_obj_loc,
                    lower_obj_desc,
                    upper_obj_desc,
                    use_op,
                },
            ),
        ) => mk_props_missing_in_invariant_subtyping_error(
            Vec1::try_from_vec(props.to_vec()).unwrap(),
            reason_lower,
            reason_upper,
            lower_obj_loc,
            upper_obj_loc,
            lower_obj_desc,
            upper_obj_desc,
            use_op,
        ),

        (
            None,
            FriendlyMessageRecipe::PropsExtraAgainstExactObject(
                box PropsExtraAgainstExactObjectData {
                    props,
                    reason_l_obj,
                    reason_r_obj,
                    use_op,
                },
            ),
        ) => mk_use_op_error(
            loc_of_aloc(&reason_l_obj.loc),
            use_op,
            None,
            Message::MessagePropExtraAgainstExactObject(Box::new(
                MessagePropExtraAgainstExactObjectData {
                    lower: reason_l_obj,
                    upper: reason_r_obj,
                    props: Vec1::try_from_vec(
                        props.iter().map(|s| FlowSmolStr::new(s.as_str())).collect(),
                    )
                    .unwrap(),
                },
            )),
        ),

        (
            None,
            FriendlyMessageRecipe::PropPolarityMismatch(box PropPolarityMismatchData {
                reason_lower,
                reason_upper,
                props,
                use_op,
            }),
        ) => mk_prop_polarity_mismatch_error(
            reason_lower,
            reason_upper,
            Vec1::try_from_vec(
                props
                    .iter()
                    .map(|(name, p1, p2)| (name.dupe(), *p1, *p2))
                    .collect(),
            )
            .unwrap(),
            use_op,
        ),

        (
            None,
            FriendlyMessageRecipe::IncompatibleUse(box IncompatibleUseData {
                loc,
                upper_kind,
                reason_lower,
                reason_upper,
                use_op,
            }),
        ) => mk_incompatible_use_error(
            loc_of_aloc(&loc),
            upper_kind,
            reason_lower,
            reason_upper,
            use_op,
        ),

        (
            None,
            FriendlyMessageRecipe::IncompatibleSubtyping(box IncompatibleSubtypingData {
                reason_lower,
                reason_upper,
                use_op,
                explanation,
            }),
        ) => mk_incompatible_error(explanation, reason_lower, reason_upper, use_op),

        (
            None,
            FriendlyMessageRecipe::IncompatibleInvariantSubtyping(
                box IncompatibleInvariantSubtypingData {
                    sub_component,
                    lower_loc,
                    upper_loc,
                    lower_desc,
                    upper_desc,
                    use_op,
                    explanation,
                },
            ),
        ) => mk_incompatible_invariant_subtyping_error(
            explanation,
            sub_component,
            lower_loc,
            upper_loc,
            lower_desc,
            upper_desc,
            use_op,
        ),

        (
            None,
            FriendlyMessageRecipe::IncompatibleEnum(box IncompatibleEnumData {
                reason_lower,
                reason_upper,
                use_op,
                enum_kind,
                representation_type,
                casting_syntax,
            }),
        ) => {
            use super::error_message::EnumKind;

            let additional_explanation = match (&enum_kind, &representation_type) {
                (EnumKind::ConcreteEnumKind, Some(repr_type)) => {
                    Some(Explanation::ExplanationConcreteEnumCasting {
                        representation_type: repr_type.to_string().into(),
                        casting_syntax,
                    })
                }
                (EnumKind::AbstractEnumKind, _) => {
                    Some(Explanation::ExplanationAbstractEnumCasting)
                }
                _ => None,
            };
            mk_incompatible_error(additional_explanation, reason_lower, reason_upper, use_op)
        }

        (
            None,
            FriendlyMessageRecipe::Speculation(box SpeculationData {
                loc,
                use_op,
                branches,
            }),
        ) => mk_use_op_speculation_error(loc, use_op, branches),

        (None, FriendlyMessageRecipe::Normal(_)) | (Some(_), _) => {
            panic!("ImproperlyFormattedError: {:?}", msg)
        }
    };

    intermediate_error.misplaced_source_file = match intermediate_error.loc.source.as_ref() {
        Some(file) if !speculation && !source_file.is_lib_file() && file != source_file => {
            Some(source_file.dupe())
        }
        _ => None,
    };

    intermediate_error
}

#[allow(dead_code)]
pub fn to_printable_error<L, F, G>(
    loc_of_aloc: F,
    get_ast: G,
    strip_root: Option<&std::path::Path>,
    intermediate_error: IntermediateError<L>,
) -> PrintableError<Loc>
where
    L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
    F: Fn(&L) -> Loc + Clone,
    G: Fn(&FileKey) -> Option<Arc<flow_parser::ast::Program<Loc, Loc>>>,
{
    use flow_common_errors::error_utils::RootKind;
    use flow_common_errors::error_utils::friendly;

    let text = |s: &str| -> friendly::MessageFeature<Loc> { friendly::text(s) };

    let code = |s: &str| -> friendly::MessageFeature<Loc> { friendly::code(s) };

    let ref_ = |r: &VirtualReason<L>| -> friendly::MessageFeature<Loc> {
        friendly::ref_map(&loc_of_aloc, r)
    };

    let desc = |r: &VirtualReason<L>| -> friendly::MessageFeature<Loc> {
        friendly::desc_of_reason_desc(&r.desc.map_locs(&loc_of_aloc))
    };

    let msg_export = |prefix: &str,
                      export_name: &str|
     -> (friendly::MessageFeature<Loc>, friendly::MessageFeature<Loc>) {
        if export_name == "default" {
            (text(""), text("the default export"))
        } else {
            (text(prefix), code(export_name))
        }
    };

    let ref_of_ty_or_desc = |loc: &L,
                             ty_or_desc: &Result<ALocTy, VirtualReasonDesc<L>>|
     -> friendly::MessageFeature<Loc> {
        match ty_or_desc {
            // | Ok ty ->
            Ok(ty) => {
                let ty = flow_common_ty::ty_utils::simplify_type(true, None, ty.dupe());
                let ty_str = flow_common_ty::ty_printer::string_of_t_single_line(
                    &ty,
                    &flow_common_ty::ty_printer::PrinterOptions {
                        ts_syntax: true,
                        ..Default::default()
                    },
                );
                let ty_str = if ty_str.len() > 100 {
                    format!("{}...", &ty_str[..100])
                } else {
                    ty_str
                };
                let desc = format!("`{}`", ty_str);
                friendly::hardcoded_string_desc_ref(&desc, loc_of_aloc(loc))
            }
            Err(desc) => {
                use flow_common::reason::mk_reason;
                let reason = mk_reason(desc.clone(), loc.dupe());
                friendly::ref_map(&loc_of_aloc, &reason)
            }
        }
    };

    let explanation_to_friendly_msgs = |explanation: &Explanation<L>| -> friendly::Message<Loc> {
        use super::intermediate_error_types::Explanation::*;

        match explanation {
            ExplanationAbstractEnumCasting => friendly::Message(vec![
                text("You can explicitly cast your enum value to its representation type using "),
                code("<expr>.valueOf()"),
            ]),
            ExplanationArrayInvariantTyping => friendly::Message(vec![
                text("Arrays are invariantly typed. See "),
                text(
                    "https://flow.org/en/docs/faq/#why-cant-i-pass-an-arraystring-to-a-function-that-takes-an-arraystring-number",
                ),
            ]),
            ExplanationFunctionsWithStaticsToObject => friendly::Message(vec![text(
                "Functions without statics are not compatible with objects",
            )]),
            ExplanationMultiplatform => friendly::Message(vec![
                text("Read the docs on Flow's multi-platform support for more information: "),
                text("https://flow.org/en/docs/react/multiplatform"),
            ]),
            ExplanationPropertyInvariantTyping => friendly::Message(vec![
                text("This property is invariantly typed. See "),
                text(
                    "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number",
                ),
            ]),
            ExplanationIncompatibleReactDeepReadOnly => {
                friendly::Message(vec![text("Consider using "), code("React.Immutable<>")])
            }
            ExplanationReactComponentPropsDeepReadOnly(props_loc) => friendly::Message(vec![
                text("React "),
                friendly::hardcoded_string_desc_ref("component properties", loc_of_aloc(props_loc)),
                text(" and their nested props and elements cannot be written to. "),
                text("(https://react.dev/reference/rules/components-and-hooks-must-be-pure#props)"),
            ]),
            ExplanationReactHookArgsDeepReadOnly(props_loc) => friendly::Message(vec![
                text("React "),
                friendly::hardcoded_string_desc_ref("hook arguments", loc_of_aloc(props_loc)),
                text(" and their nested elements cannot be written to. "),
                text(
                    "(https://react.dev/reference/rules/components-and-hooks-must-be-pure#return-values-and-arguments-to-hooks-are-immutable)",
                ),
            ]),
            ExplanationReactHookIncompatibleWithEachOther => friendly::Message(vec![text(
                "Different React hooks are not compatible with each other, because hooks cannot be called conditionally",
            )]),
            ExplanationReactHookIncompatibleWithNormalFunctions => friendly::Message(vec![text(
                "React hooks and other functions are not compatible with each other, because hooks cannot be called conditionally",
            )]),
            ExplanationReactHookReturnDeepReadOnly(hook_loc) => friendly::Message(vec![
                text("The return value of a "),
                friendly::hardcoded_string_desc_ref("React hook", loc_of_aloc(hook_loc)),
                text(" cannot be written to. "),
                text(
                    "(https://react.dev/reference/rules/components-and-hooks-must-be-pure#return-values-and-arguments-to-hooks-are-immutable)",
                ),
            ]),
            ExplanationTypeGuardPositiveConsistency {
                return_,
                param,
                guard_type,
                is_return_false_statement,
            } => {
                let mut features = vec![
                    text("The type of "),
                    ref_(param),
                    text(" at the return expression "),
                    ref_(return_),
                    text(" needs to be compatible with the guard type "),
                    ref_(guard_type),
                    text("."),
                    text(" See 1. in "),
                    text(
                        "https://flow.org/en/docs/types/type-guards/#toc-consistency-checks-of-type-guard-functions",
                    ),
                ];
                if *is_return_false_statement {
                    features.push(text(". Consider replacing the body of this predicate "));
                    features.push(text("function with a single conditional expression"));
                }
                friendly::Message(features)
            }
            ExplanationConstrainedAssign(data) => {
                let ExplanationConstrainedAssignData {
                    name,
                    declaration,
                    providers,
                } = data.as_ref();

                use flow_common::reason::Name as ReasonName;
                use flow_common::reason::VirtualReasonDesc::RIdentifier;
                use flow_common::reason::mk_reason;

                let assignments = if providers.is_empty() {
                    vec![text("one of its initial assignments")]
                } else if providers.len() == 1 && providers[0] == *declaration {
                    vec![
                        text("its "),
                        friendly::hardcoded_string_desc_ref(
                            "initializer",
                            loc_of_aloc(declaration),
                        ),
                    ]
                } else if providers.len() == 1 {
                    vec![
                        text("its "),
                        friendly::hardcoded_string_desc_ref(
                            "initial assignment",
                            loc_of_aloc(&providers[0]),
                        ),
                    ]
                } else {
                    let mut features = vec![text("one of its initial assignments")];
                    for (i, provider) in providers.iter().enumerate() {
                        if i > 0 {
                            features.push(text(","));
                        }
                        features.push(friendly::no_desc_ref(&loc_of_aloc(provider)));
                    }
                    features
                };

                let name_reason = mk_reason(
                    RIdentifier(ReasonName::new(name.clone())),
                    declaration.dupe(),
                );

                let mut features = vec![
                    text("All writes to "),
                    code(name),
                    text(" must be compatible with the type of "),
                ];
                features.extend(assignments);
                features.push(text(". Add an annotation to "));
                features.push(friendly::ref_map(&loc_of_aloc, &name_reason));
                features.push(text(" if a different type is desired"));
                friendly::Message(features)
            }
            ExplanationConcreteEnumCasting {
                representation_type,
                casting_syntax: _,
            } => {
                let example = format!("<expr> as {}", representation_type);
                friendly::Message(vec![
                    text("You can explicitly cast your enum value to a "),
                    text(representation_type),
                    text(" using "),
                    code(&example),
                ])
            }
            ExplanationCustomError(data) => {
                let ExplanationCustomErrorData {
                    name,
                    custom_error_loc,
                } = data.as_ref();
                let loc = loc_of_aloc(custom_error_loc);
                let custom_docs = loc
                    .source
                    .as_ref()
                    .and_then(&get_ast)
                    .and_then(|program| {
                        let all_comments = &program.all_comments;
                        all_comments.iter().find_map(|comment| {
                            if loc == comment.loc
                                && comment.kind == flow_parser::ast::CommentKind::Block
                            {
                                jsdoc::parse(&comment.text)
                            } else {
                                None
                            }
                        })
                    })
                    .and_then(|jsdoc_val| {
                        let description = jsdoc_val
                            .description()
                            .as_deref()
                            .map(|s| s.trim().to_string());
                        let example = jsdoc_val
                            .unrecognized_tags()
                            .0
                            .iter()
                            .find(|(tag_name, _)| tag_name == "example")
                            .and_then(|(_, desc)| desc.as_deref())
                            .map(|s| s.trim().to_string());
                        match (description, example) {
                            (None, None) => None,
                            (Some(d), None) => Some(friendly::Message(vec![
                                text("Description of "),
                                code(name),
                                text(": "),
                                text(&d),
                            ])),
                            (None, Some(e)) => Some(friendly::Message(vec![
                                text("Example for "),
                                code(name),
                                text(": "),
                                code(&e),
                            ])),
                            (Some(d), Some(e)) => Some(friendly::Message(vec![
                                text("Description of "),
                                code(name),
                                text(": "),
                                text(&d),
                                text(". Example for "),
                                code(name),
                                text(": "),
                                code(&e),
                            ])),
                        }
                    });
                match custom_docs {
                    Some(m) => m,
                    None => friendly::Message(vec![
                        text("See "),
                        friendly::hardcoded_string_desc_ref(
                            "relevant docs",
                            loc_of_aloc(custom_error_loc),
                        ),
                    ]),
                }
            }
            ExplanationAdditionalUnionMembers(data) => {
                let ExplanationAdditionalUnionMembersData {
                    left,
                    right,
                    members,
                    extra_number,
                } = data.as_ref();
                let mut features = vec![text("Type "), ref_(left), text(" includes members ")];
                for (i, member) in members.iter().enumerate() {
                    if i > 0 {
                        features.push(text(", "));
                    }
                    features.push(code(member));
                }
                if *extra_number > 0 {
                    features.push(text(&format!(" and {} more", extra_number)));
                }
                features.push(text(" that are not included in type "));
                features.push(ref_(right));
                friendly::Message(features)
            }
            ExplanationObjectLiteralNeedsRecordSyntax {
                record_name,
                obj_reason,
            } => friendly::Message(vec![
                text("Fix by adding the record name "),
                code(record_name),
                text(" before the "),
                ref_(obj_reason),
                text(", e.g. "),
                code(&format!("{} {{...}}", record_name)),
            ]),
            ExplanationPropertyMissingDueToNeutralOptionalProperty(data) => {
                let ExplanationPropertyMissingDueToNeutralOptionalPropertyData {
                    props_plural,
                    lower_obj_loc,
                    upper_obj_loc,
                    lower_obj_desc,
                    upper_obj_desc,
                    upper_object_reason,
                } = data.as_ref();
                use flow_common::reason::VirtualReasonDesc::*;

                let prefix = if *props_plural {
                    vec![
                        text("These optional properties of "),
                        ref_(upper_object_reason),
                        text(" are"),
                    ]
                } else {
                    vec![
                        text("This optional property of "),
                        ref_(upper_object_reason),
                        text(" is"),
                    ]
                };

                let fix_suggestion: Vec<friendly::MessageFeature<Loc>> =
                    match (lower_obj_desc, upper_obj_desc) {
                        // lower is literal, upper is type
                        (Err(lower_desc), Ok(_))
                            if matches!(
                                lower_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(lower_desc.clone(), lower_obj_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(upper_obj_loc, upper_obj_desc),
                            ]
                        }
                        // upper is literal, lower is type
                        (Ok(_), Err(upper_desc))
                            if matches!(
                                upper_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(upper_desc.clone(), upper_obj_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(lower_obj_loc, lower_obj_desc),
                            ]
                        }
                        _ => vec![
                            text("make "),
                            ref_of_ty_or_desc(lower_obj_loc, lower_obj_desc),
                            text(" and "),
                            ref_of_ty_or_desc(upper_obj_loc, upper_obj_desc),
                            text(" exactly the same"),
                        ],
                    };

                let mut features = prefix;
                features.push(text(" invariantly typed. To fix,\n- Either "));
                features.extend(fix_suggestion);
                features.push(text("\n- Or make "));
                features.push(ref_(upper_object_reason));
                features.push(text(" readonly. See "));
                features.push(text(
                    "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number",
                ));
                friendly::Message(features)
            }
            ExplanationInvariantSubtypingDueToMutableArray(data) => {
                let ExplanationInvariantSubtypingDueToMutableArrayData {
                    lower_array_loc,
                    upper_array_loc,
                    lower_array_desc,
                    upper_array_desc,
                    upper_array_reason,
                } = data.as_ref();
                use flow_common::reason::VirtualReasonDesc::*;

                let fix_suggestion: Vec<friendly::MessageFeature<Loc>> =
                    match (lower_array_desc, upper_array_desc) {
                        (Err(lower_desc), Ok(_))
                            if matches!(
                                lower_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(lower_desc.clone(), lower_array_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(upper_array_loc, upper_array_desc),
                            ]
                        }
                        (Ok(_), Err(upper_desc))
                            if matches!(
                                upper_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(upper_desc.clone(), upper_array_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(lower_array_loc, lower_array_desc),
                            ]
                        }
                        _ => vec![
                            text("make "),
                            ref_of_ty_or_desc(lower_array_loc, lower_array_desc),
                            text(" and "),
                            ref_of_ty_or_desc(upper_array_loc, upper_array_desc),
                            text(" exactly the same"),
                        ],
                    };

                let mut features = vec![
                    text(
                        "The above-mentioned two types must be the same because arrays are invariantly typed. ",
                    ),
                    text("To fix the error,\n- Either "),
                ];
                features.extend(fix_suggestion);
                features.push(text("\n- Or make "));
                features.push(ref_(upper_array_reason));
                features.push(text(" a "));
                features.push(code("ReadonlyArray"));
                features.push(text(".\nSee "));
                features.push(text(
                    "https://flow.org/en/docs/faq/#why-cant-i-pass-an-arraystring-to-a-function-that-takes-an-arraystring-number",
                ));
                friendly::Message(features)
            }
            ExplanationInvariantSubtypingDueToMutableProperty(data) => {
                let ExplanationInvariantSubtypingDueToMutablePropertyData {
                    lower_obj_loc,
                    upper_obj_loc,
                    lower_obj_desc,
                    upper_obj_desc,
                    upper_object_reason,
                    property_name,
                } = data.as_ref();
                use flow_common::reason::VirtualReasonDesc::*;

                let prop: Vec<friendly::MessageFeature<Loc>> = match property_name {
                    None => vec![text("the indexer")],
                    Some(name) => vec![text("property "), code(name)],
                };

                let fix_suggestion: Vec<friendly::MessageFeature<Loc>> =
                    match (lower_obj_desc, upper_obj_desc) {
                        (Err(lower_desc), Ok(_))
                            if matches!(
                                lower_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(lower_desc.clone(), lower_obj_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(upper_obj_loc, upper_obj_desc),
                            ]
                        }
                        (Ok(_), Err(upper_desc))
                            if matches!(
                                upper_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(upper_desc.clone(), upper_obj_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(lower_obj_loc, lower_obj_desc),
                            ]
                        }
                        _ => vec![
                            text("make "),
                            ref_of_ty_or_desc(lower_obj_loc, lower_obj_desc),
                            text(" and "),
                            ref_of_ty_or_desc(upper_obj_loc, upper_obj_desc),
                            text(" exactly the same"),
                        ],
                    };

                let mut features = vec![text(
                    "The above-mentioned two types must be the same because ",
                )];
                features.extend(prop.clone());
                features.push(text(" is invariantly typed. To fix the error,\n- Either "));
                features.extend(fix_suggestion);
                features.push(text("\n- Or make "));
                features.extend(prop);
                features.push(text(" in "));
                features.push(ref_(upper_object_reason));
                features.push(text(" readonly. See "));
                features.push(text(
                    "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number",
                ));
                friendly::Message(features)
            }
            ExplanationInvariantSubtypingDueToMutableProperties(data) => {
                let ExplanationInvariantSubtypingDueToMutablePropertiesData {
                    lower_obj_loc,
                    upper_obj_loc,
                    lower_obj_desc,
                    upper_obj_desc,
                    upper_object_reason,
                    properties,
                } = data.as_ref();
                use flow_common::reason::VirtualReasonDesc::*;

                let prop_codes: Vec<friendly::Message<Loc>> = properties
                    .iter()
                    .map(|prop| friendly::Message(vec![code(prop.as_str())]))
                    .collect();
                let mut props_msg: Vec<friendly::MessageFeature<Loc>> = vec![text("properties ")];
                props_msg.extend(friendly::conjunction_concat(prop_codes, "and", None).0);

                let fix_suggestion: Vec<friendly::MessageFeature<Loc>> =
                    match (lower_obj_desc, upper_obj_desc) {
                        (Err(lower_desc), Ok(_))
                            if matches!(
                                lower_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(lower_desc.clone(), lower_obj_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(upper_obj_loc, upper_obj_desc),
                            ]
                        }
                        (Ok(_), Err(upper_desc))
                            if matches!(
                                upper_desc,
                                RObjectLit | RObjectLitUnsound | RArrayLit | RArrayLitUnsound
                            ) =>
                        {
                            vec![
                                text("annotate "),
                                friendly::ref_map(
                                    &loc_of_aloc,
                                    &mk_reason(upper_desc.clone(), upper_obj_loc.dupe()),
                                ),
                                text(" with "),
                                ref_of_ty_or_desc(lower_obj_loc, lower_obj_desc),
                            ]
                        }
                        _ => vec![
                            text("make "),
                            ref_of_ty_or_desc(lower_obj_loc, lower_obj_desc),
                            text(" and "),
                            ref_of_ty_or_desc(upper_obj_loc, upper_obj_desc),
                            text(" exactly the same"),
                        ],
                    };

                let mut features = vec![text(
                    "The above-mentioned two types must be the same because ",
                )];
                features.extend(props_msg.clone());
                features.push(text(" are invariantly typed. To fix the error,\n- Either "));
                features.extend(fix_suggestion);
                features.push(text("\n- Or make "));
                features.extend(props_msg);
                features.push(text(" in "));
                features.push(ref_(upper_object_reason));
                features.push(text(" readonly. See "));
                features.push(text(
                    "https://flow.org/en/docs/faq/#why-cant-i-pass-a-string-to-a-function-that-takes-a-string-number",
                ));
                friendly::Message(features)
            }
        }
    };

    let frame_to_friendly_msgs = |include_incompatibility_pair: bool,
                                  frame: &ErrorFrame<L>|
     -> (Option<friendly::Message<Loc>>, friendly::Message<Loc>) {
        use super::intermediate_error_types::Frame::*;

        // Helper to convert incompatibility_pair to message if include_incompatibility_pair is true
        let map_incompatibility =
            |pair: &Option<(VirtualReason<L>, VirtualReason<L>)>|
             -> Option<friendly::Message<Loc>> {
                if include_incompatibility_pair {
                    pair.as_ref().map(|(l, u)| {
                        friendly::Message(vec![
                            ref_(l),
                            text(" is incompatible with "),
                            ref_(u),
                        ])
                    })
                } else {
                    None
                }
            };

        match frame {
            FrameAnonymous => (None, friendly::Message(vec![])),
            FrameAccessChain {
                chain,
                incompatibility_pair,
            } => {
                let first = chain.first();
                let kind = match first {
                    AccessChainSegment::PropSegment(_) => "property ",
                    AccessChainSegment::TupleIndexSegment(_) => "index ",
                };
                let chain_str: String =
                    chain
                        .iter()
                        .rev()
                        .enumerate()
                        .fold(String::new(), |acc, (i, segment)| match segment {
                            AccessChainSegment::PropSegment(prop) => {
                                if i == 0 {
                                    prop.as_str().to_string()
                                } else {
                                    format!("{}.{}", acc, prop.as_str())
                                }
                            }
                            AccessChainSegment::TupleIndexSegment(n) => {
                                if i == 0 {
                                    n.to_string()
                                } else {
                                    format!("{}[{}]", acc, n)
                                }
                            }
                        });
                (
                    map_incompatibility(incompatibility_pair),
                    friendly::Message(vec![text(kind), code(&chain_str)]),
                )
            }
            FrameArrayElement {
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("array element")]),
            ),
            FrameCallableSignature {
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the callable signature")]),
            ),
            FrameEnumRepresentationType => (
                None,
                friendly::Message(vec![text("the enum's representation type")]),
            ),
            FrameFunNthArgument {
                n,
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the "), text(&ordinal(*n)), text(" argument")]),
            ),
            FrameFunThisArgument {
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the "), code("this"), text(" argument")]),
            ),
            FrameFunNthParam {
                n,
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the "), text(&ordinal(*n)), text(" parameter")]),
            ),
            FrameFunThisParam {
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the "), code("this"), text(" parameter")]),
            ),
            FrameIndexerProperty {
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the indexer property")]),
            ),
            FrameIndexerPropertyKey {
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the indexer property's key")]),
            ),
            FrameTypeArgument(targ) => (
                None,
                friendly::Message(vec![text("type argument "), ref_(targ)]),
            ),
            FrameTypeParameterBound(name) => (
                None,
                friendly::Message(vec![text("type argument "), code(name)]),
            ),
            FrameTypePredicate => (None, friendly::Message(vec![text("the type predicate")])),
            FrameReturnValue {
                incompatibility_pair,
            } => (
                map_incompatibility(incompatibility_pair),
                friendly::Message(vec![text("the return value")]),
            ),
            FrameUnionRepresentative(union) => (
                None,
                friendly::Message(vec![text("at least one member of "), ref_(union)]),
            ),
        }
    };

    let hardcoded_string_desc_ref = |s: &str, loc: &L| -> friendly::MessageFeature<Loc> {
        friendly::hardcoded_string_desc_ref(s, loc_of_aloc(loc))
    };

    fn ordinal(n: i32) -> String {
        match n {
            1 => "first".to_string(),
            2 => "second".to_string(),
            3 => "third".to_string(),
            4 => "fourth".to_string(),
            5 => "fifth".to_string(),
            6 => "sixth".to_string(),
            7 => "seventh".to_string(),
            8 => "eighth".to_string(),
            9 => "ninth".to_string(),
            n => {
                let s = n.to_string();
                let bytes = s.as_bytes();
                let len = bytes.len();
                let tens = if len >= 2 {
                    (bytes[len - 2] - b'0') as i32
                } else {
                    0
                };
                let ones = (bytes[len - 1] - b'0') as i32;
                let suffix = if tens == 1 {
                    "th"
                } else {
                    match ones {
                        1 => "st",
                        2 => "nd",
                        3 => "rd",
                        _ => "th",
                    }
                };
                format!("{}{}", n, suffix)
            }
        }
    }

    let root_msg_to_root_kind_and_friendly_msgs =
        |root_msg: &RootMessage<L>| -> (RootKind, friendly::Message<Loc>) {
            match root_msg {
                RootMessage::RootCannotAccessIndex { index, object_ } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot access "),
                        friendly::desc_of_reason_desc(index),
                        text(" on "),
                        friendly::desc_of_reason_desc(object_),
                    ]),
                ),
                RootMessage::RootCannotAddComputedProperty => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![text("Cannot add computed property")]),
                ),
                RootMessage::RootCannotAssign { init, target } => match target {
                    None => (
                        RootKind::OperationRoot,
                        friendly::Message(vec![
                            text("Cannot assign "),
                            friendly::desc_of_reason_desc(init),
                            text(" to variable"),
                        ]),
                    ),
                    Some(target) => (
                        RootKind::OperationRoot,
                        friendly::Message(vec![
                            text("Cannot assign "),
                            friendly::desc_of_reason_desc(init),
                            text(" to "),
                            friendly::desc_of_reason_desc(target),
                        ]),
                    ),
                },
                RootMessage::RootCannotCall(fn_) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot call "),
                        friendly::desc_of_reason_desc(fn_),
                    ]),
                ),
                RootMessage::RootCannotCallWithNamedParam { fn_, lower, name } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot call "),
                        friendly::desc_of_reason_desc(fn_),
                        text(" with "),
                        friendly::desc_of_reason_desc(lower),
                        text(" bound to "),
                        code(name),
                    ]),
                ),
                RootMessage::RootCannotCallWithNthParam { fn_, lower, n } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot call "),
                        friendly::desc_of_reason_desc(fn_),
                        text(" with "),
                        friendly::desc_of_reason_desc(lower),
                        text(" bound to "),
                        text(&format!("the {} parameter", ordinal(*n))),
                    ]),
                ),
                RootMessage::RootCannotCallObjectAssign(op) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Incorrect arguments passed to "),
                        friendly::desc_of_reason_desc(op),
                    ]),
                ),
                RootMessage::RootCannotCast { lower, upper } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot cast "),
                        friendly::desc_of_reason_desc(lower),
                        text(" to "),
                        friendly::desc_of_reason_desc(upper),
                    ]),
                ),
                RootMessage::RootCannotCheckAgainst { test, discriminant } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Invalid check of "),
                        friendly::desc_of_reason_desc(test),
                        text(" against "),
                        ref_(discriminant),
                    ]),
                ),
                RootMessage::RootCannotCheckAgainstSwitchDiscriminant(discriminant_loc) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Invalid check of case test against "),
                        hardcoded_string_desc_ref("switch discriminant", discriminant_loc),
                    ]),
                ),
                RootMessage::RootCannotCoerce { from, target } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot coerce "),
                        friendly::desc_of_reason_desc(from),
                        text(" to "),
                        friendly::desc_of_reason_desc(target),
                    ]),
                ),
                RootMessage::RootCannotConformToCommonInterface {
                    originate_from_import,
                } => {
                    if *originate_from_import {
                        (
                            RootKind::OperationRoot,
                            friendly::Message(vec![text(
                                "The import resolves to a forked module that has implementations of conflicting types",
                            )]),
                        )
                    } else {
                        (
                            RootKind::OperationRoot,
                            friendly::Message(vec![text(
                                "Cannot conform to common interface module",
                            )]),
                        )
                    }
                }
                RootMessage::RootCannotMergeDeclaration { first_decl } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot merge this interface declaration with previous declaration "),
                        friendly::ref_map(&loc_of_aloc, first_decl),
                        text(" because of conflicting property types"),
                    ]),
                ),
                RootMessage::RootCannotCreateElement(component) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot create "),
                        friendly::desc_of_reason_desc(component),
                        text(" element"),
                    ]),
                ),
                RootMessage::RootCannotCreateRecord(constructor) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot create record "),
                        friendly::desc_of_reason_desc(constructor),
                    ]),
                ),
                RootMessage::RootCannotDeclareRef => (
                    RootKind::ShortExplanationRoot,
                    friendly::Message(vec![
                        text("The "),
                        code("ref"),
                        text(" parameter must be a subtype of "),
                        code("React.RefSetter"),
                    ]),
                ),
                RootMessage::RootCannotDeclareTypeGuard {
                    type_guard_loc,
                    fn_,
                } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot declare a "),
                        hardcoded_string_desc_ref("type guard", type_guard_loc),
                        text(" for "),
                        ref_(fn_),
                    ]),
                ),
                RootMessage::RootCannotDefineClassMethod { method_, name } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot define "),
                        ref_(method_),
                        text(" on "),
                        friendly::desc_of_reason_desc(name),
                    ]),
                ),
                RootMessage::RootCannotDefineShadowedProtoProperty => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![text("Cannot define shadowed proto property")]),
                ),
                RootMessage::RootCannotDelete(prop) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot delete "),
                        friendly::desc_of_reason_desc(prop),
                    ]),
                ),
                RootMessage::RootCannotExpectImplicitReturn { upper, fn_ } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot expect "),
                        friendly::desc_of_reason_desc(upper),
                        text(" as the return type of "),
                        friendly::desc_of_reason_desc(fn_),
                    ]),
                ),
                RootMessage::RootCannotExtendClass { extends, def } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot extend "),
                        ref_(extends),
                        text(" with "),
                        friendly::desc_of_reason_desc(def),
                    ]),
                ),
                RootMessage::RootCannotGetProp(prop) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot get "),
                        friendly::desc_of_reason_desc(prop),
                    ]),
                ),
                RootMessage::RootCannotGetRest(op) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot get rest of "),
                        friendly::desc_of_reason_desc(op),
                    ]),
                ),
                RootMessage::RootCannotImplementClass { implements, def } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot implement "),
                        ref_(implements),
                        text(" with "),
                        friendly::desc_of_reason_desc(def),
                    ]),
                ),
                RootMessage::RootCannotInitializeField { field, body } => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot initialize "),
                        friendly::desc_of_reason_desc(field),
                        text(" with "),
                        friendly::desc_of_reason_desc(body),
                    ]),
                ),
                RootMessage::RootCannotInstantiateEval(type_) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![text("Cannot instantiate "), ref_(type_)]),
                ),
                RootMessage::RootCannotInstantiateTypeApp(type_) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot instantiate "),
                        friendly::desc_of_reason_desc(type_),
                    ]),
                ),
                RootMessage::RootCannotInstantiateRenderType => (
                    RootKind::ShortExplanationRoot,
                    friendly::Message(vec![
                        text("Render types must be a subtype of "),
                        code("React.Node"),
                        text(" or a reference to a component-syntax component"),
                    ]),
                ),
                RootMessage::RootCannotReturn(value) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot return "),
                        friendly::desc_of_reason_desc(value),
                    ]),
                ),
                RootMessage::RootCannotShadowProto(proto) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![text("Cannot shadow proto "), ref_(proto)]),
                ),
                RootMessage::RootCannotShadowProtoProperty => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![text("Cannot shadow proto property")]),
                ),
                RootMessage::RootCannotSpread(op) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot spread "),
                        friendly::desc_of_reason_desc(op),
                    ]),
                ),
                RootMessage::RootCannotUpdate(prop) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot update "),
                        friendly::desc_of_reason_desc(prop),
                    ]),
                ),
                RootMessage::RootCannotUseInferTypeBound { infer } => (
                    RootKind::ShortExplanationRoot,
                    friendly::Message(vec![
                        text("The infer type "),
                        friendly::desc_of_reason_desc(infer),
                        text(" must have consistent bounds"),
                    ]),
                ),
                RootMessage::RootCannotUseTypeGuard {
                    guard_type,
                    param_name,
                } => (
                    RootKind::ShortExplanationRoot,
                    friendly::Message(vec![
                        text("The type predicate "),
                        ref_(guard_type),
                        text(" needs to be compatible with parameter `"),
                        text(param_name),
                        text("`'s type"),
                    ]),
                ),
                RootMessage::RootCannotYield(value) => (
                    RootKind::OperationRoot,
                    friendly::Message(vec![
                        text("Cannot yield "),
                        friendly::desc_of_reason_desc(value),
                    ]),
                ),
            }
        };

    let msg_to_friendly_msgs = |message: &Message<L>| -> friendly::Message<Loc> {
        use super::intermediate_error_types::Message::*;

        match message {
            MessagePlainTextReservedForInternalErrorOnly(s) => friendly::Message(vec![text(s)]),
            MessageAlreadyExhaustivelyCheckOneEnumMember(
                box MessageAlreadyExhaustivelyCheckOneEnumMemberData {
                    prev_check_loc,
                    enum_reason,
                    member_name,
                },
            ) => friendly::Message(vec![
                text("Invalid exhaustive check: "),
                text("case checks for enum member "),
                code(member_name),
                text(" of "),
                ref_(enum_reason),
                text(", but member "),
                code(member_name),
                text(" was already checked at "),
                friendly::hardcoded_string_desc_ref("case", loc_of_aloc(prev_check_loc)),
                text("."),
            ]),
            MessageAlreadyExhaustivelyCheckAllEnumMembers { enum_reason } => {
                friendly::Message(vec![
                    text("Invalid exhaustive check: "),
                    text("default case checks for additional enum members of "),
                    ref_(enum_reason),
                    text(", but all of its members have already been checked."),
                ])
            }
            MessageAmbiguousNumericKeyWithVariance => friendly::Message(vec![
                text("Cannot mix number literal keys and variance annotations, "),
                text("as the variance annotation could be interpreted as negating or "),
                text("making positive the number literal. "),
                text("Consider using a string literal key name to disambiguate."),
            ]),
            MessageAmbiguousObjectType => friendly::Message(vec![
                text("Please write this object type as explicitly exact (use "),
                code("{|"),
                text(" and "),
                code("|}"),
                text(" instead of "),
                code("{"),
                text(" and "),
                code("}"),
                text(") or as explicitly inexact (add "),
                code("..."),
                text(" to the end of the list of properties)."),
            ]),
            MessageAnyValueUsedAsType(use_) => friendly::Message(vec![
                text("Cannot use "),
                friendly::desc_of_reason_desc(use_),
                text(" as a type because it is an "),
                code("any"),
                text("-typed value. "),
                text("Type "),
                friendly::desc_of_reason_desc(use_),
                text(" properly, so it is no longer "),
                code("any"),
                text("-typed, to use it as an annotation."),
            ]),
            MessageBadLibdefModuleOverride(x) => friendly::Message(vec![
                text("This module declaration overrides an existing module "),
                ref_(x),
                text(". Overriding in library definitions can lead to surprising behaviors."),
            ]),
            MessageBadLibdefNameOverride(x) => friendly::Message(vec![
                text("This name declaration overrides an existing binding "),
                ref_(x),
                text(". Overriding in library definitions can lead to surprising behaviors."),
            ]),
            MessageCannotAccessEnumMember(box MessageCannotAccessEnumMemberData {
                member_name,
                suggestion,
                description,
                enum_reason,
            }) => {
                let mut features = vec![
                    text("Cannot access "),
                    friendly::desc_of_reason_desc(description),
                ];
                match member_name {
                    Some(name) => {
                        features.extend(vec![
                            text(" because "),
                            code(name.as_str()),
                            text(" is not a member of "),
                            ref_(enum_reason),
                            text("."),
                        ]);
                        if let Some(sugg) = suggestion {
                            features.extend(vec![
                                text(" Did you mean the member "),
                                code(sugg),
                                text("?"),
                            ]);
                        }
                    }
                    None => {
                        features.extend(vec![
                            text(" on "),
                            ref_(enum_reason),
                            text(" because computed access is not allowed on enums."),
                        ]);
                    }
                }
                friendly::Message(features)
            }
            MessageCannotApplyNonPolymorphicType => friendly::Message(vec![text(
                "Cannot apply type because it is not a polymorphic type.",
            )]),
            MessageCannotAssignToInvalidLHS => friendly::Message(vec![text(
                "Invalid left-hand side in assignment expression.",
            )]),
            MessageCannotAccessReactRefInRender { usage, in_hook } => {
                let context = if *in_hook {
                    text("within hooks")
                } else {
                    text("during render")
                };
                friendly::Message(vec![
                    text("Cannot read "),
                    code("current"),
                    text(" from "),
                    ref_(usage),
                    text(" because "),
                    code("ref"),
                    text(" values may not be read "),
                    context,
                    text(". (https://react.dev/reference/react/useRef)."),
                ])
            }
            MessageCannotAssignToObjectWithComputedProp(reason_prop) => friendly::Message(vec![
                text("Cannot use "),
                ref_(reason_prop),
                text(" to assign a computed property."),
                text(" Computed properties may only be numeric or string literal values."),
                text(
                    " See https://flow.org/en/docs/types/literals/ for more information on literal types.",
                ),
            ]),
            MessageCannotAssignToOptionalTupleElement { lower, upper } => friendly::Message(vec![
                text("you cannot assign "),
                ref_(lower),
                text(" to optional "),
                ref_(upper),
                text(" (to do so, add "),
                code("| void"),
                text(" to the tuple element type)"),
            ]),
            MessageIncompatibleWithExact { kind, lower, upper } => {
                let object_kind = match kind {
                    super::intermediate_error_types::ExactnessErrorKind::UnexpectedIndexer => {
                        "indexed "
                    }
                    super::intermediate_error_types::ExactnessErrorKind::UnexpectedInexact => {
                        "inexact "
                    }
                };
                friendly::Message(vec![
                    text(object_kind),
                    ref_(lower),
                    text(" is incompatible with exact "),
                    ref_(upper),
                ])
            }
            MessageIncompatibleGeneral { lower, upper } => friendly::Message(vec![
                ref_(lower),
                text(" is incompatible with "),
                ref_(upper),
            ]),
            MessageIncompatibleWithUnionRepresentative {
                union,
                lower,
                upper,
            } => friendly::Message(vec![
                text("at least one member of "),
                ref_(union),
                text(", e.g. "),
                ref_(lower),
                text(", is incompatible with "),
                ref_(upper),
            ]),
            MessageIncompatibleImplicitReturn(box MessageIncompatibleImplicitReturnData {
                lower,
                upper,
            }) => friendly::Message(vec![
                ref_(lower),
                text(" is incompatible with "),
                text("implicitly-returned "),
                friendly::desc_of_reason_desc(upper),
            ]),
            MessageIncompatibleReactHooksWithNonReactHook {
                lower,
                upper,
                lower_is_hook,
                hook_is_annot,
            } => {
                let hook_wording = if *hook_is_annot {
                    text("hook type annotation")
                } else {
                    text("hook")
                };
                let (lower_parts, upper_parts) = if *lower_is_hook {
                    (
                        vec![ref_(lower), text(" is a React "), hook_wording],
                        vec![ref_(upper), text(" is not a hook")],
                    )
                } else {
                    (
                        vec![ref_(lower), text(" is not a React hook")],
                        vec![ref_(upper), text(" is a "), hook_wording],
                    )
                };
                let mut parts = lower_parts;
                parts.push(text(" but "));
                parts.extend(upper_parts);
                friendly::Message(parts)
            }
            MessageInvalidArgument { lower, upper } => friendly::Message(vec![
                ref_(lower),
                text(" is not a valid argument of "),
                ref_(upper),
            ]),
            MessageCannotAccessObjectWithComputedProp {
                reason_obj,
                reason_prop,
                kind,
            } => {
                use super::intermediate_error_types::InvalidObjKey;
                let suffix: Vec<friendly::MessageFeature<Loc>> = match kind {
                    InvalidObjKey::Other => vec![],
                    InvalidObjKey::NumberNonInt => {
                        vec![text(" Only integer-like number literals are allowed.")]
                    }
                    InvalidObjKey::NumberTooLarge => vec![
                        text(" Number literals must not be larger than "),
                        code("Number.MAX_SAFE_INTEGER"),
                        text("."),
                    ],
                    InvalidObjKey::NumberTooSmall => vec![
                        text(" Number literals must not be smaller than "),
                        code("Number.MIN_SAFE_INTEGER"),
                        text("."),
                    ],
                };
                let mut features = vec![
                    text("Cannot access "),
                    ref_(reason_obj),
                    text(" with computed property using "),
                    ref_(reason_prop),
                    text("."),
                ];
                features.extend(suffix);
                friendly::Message(features)
            }
            MessageCannotAddComputedPropertyDueToPotentialOverwrite(
                box MessageCannotAddComputedPropertyDueToPotentialOverwriteData {
                    key_loc,
                    overwritten_locs,
                },
            ) => {
                let mut features = vec![
                    text("Cannot add computed property because the indexer"),
                    friendly::no_desc_ref(&loc_of_aloc(key_loc)),
                    text(" may overwrite properties with explicit keys"),
                ];
                for loc in overwritten_locs {
                    features.push(friendly::no_desc_ref(&loc_of_aloc(loc)));
                }
                features.push(text(" in a way that Flow cannot track."));
                friendly::Message(features)
            }
            MessageCannotBuildTypedInterface(sve) => {
                use flow_type_sig::signature_error::SignatureError;
                let features = match sve {
                    SignatureError::ExpectedAnnotation(_, sort) => {
                        vec![text(&format!("Missing type annotation at {}:", sort))]
                    }
                    SignatureError::UnexpectedObjectKey(_, _) => {
                        vec![text("Expected simple key in object:")]
                    }
                    SignatureError::UnexpectedArraySpread(_, _) => {
                        vec![text("Unexpected spread in array:")]
                    }
                    SignatureError::UnexpectedArrayHole(_) => {
                        vec![text("Unexpected array hole:")]
                    }
                    SignatureError::EmptyArray(_) => vec![
                        text("Cannot determine the element type of an empty array. "),
                        text(
                            "Please provide an annotation, e.g., by adding a type cast around this expression.",
                        ),
                    ],
                    SignatureError::EmptyObject(_) => vec![
                        text(
                            "Cannot determine types of initialized properties of an empty object. ",
                        ),
                        text(
                            "Please provide an annotation, e.g., by adding a type cast around this expression.",
                        ),
                    ],
                    SignatureError::UnexpectedExpression(_, esort) => vec![
                        text(&format!(
                            "Cannot determine the type of this {}. ",
                            esort.as_str()
                        )),
                        text(
                            "Please provide an annotation, e.g., by adding a type cast around this expression.",
                        ),
                    ],
                };
                let mut result = vec![
                    text("Cannot build a typed interface for this module. "),
                    text("You should annotate the exports of this module with types. "),
                ];
                result.extend(features);
                friendly::Message(result)
            }
            MessageCannotCallMaybeReactHook(box MessageCannotCallMaybeReactHookData {
                callee_loc,
                hooks,
                non_hooks,
            }) => {
                let hook_blame = if hooks.is_empty() {
                    vec![text("React hook")]
                } else {
                    vec![friendly::hardcoded_string_desc_ref(
                        "React hook",
                        loc_of_aloc(&hooks[0]),
                    )]
                };
                let non_hook_blame = if non_hooks.is_empty() {
                    vec![text("regular function definition")]
                } else {
                    vec![
                        text("regular "),
                        friendly::hardcoded_string_desc_ref(
                            "function definition",
                            loc_of_aloc(&non_hooks[0]),
                        ),
                    ]
                };
                let mut features = vec![
                    text("Cannot call function because "),
                    friendly::hardcoded_string_desc_ref("callee", loc_of_aloc(callee_loc)),
                    text(" may be a "),
                ];
                features.extend(hook_blame);
                features.push(text(" or may be a "));
                features.extend(non_hook_blame);
                features.push(text(
                    ". Function callees must either be definitely a hook or definitely not a hook, because the same hook must be called every time a component renders. ",
                ));
                features.push(text("(https://react.dev/reference/rules/rules-of-hooks)"));
                friendly::Message(features)
            }
            MessageCannotCallNonHookSyntaxHook(callee_loc) => friendly::Message(vec![
                text("Cannot call function because "),
                friendly::hardcoded_string_desc_ref("callee", loc_of_aloc(callee_loc)),
                text(" has a name that indicates it is a React hook (starting with "),
                code("use"),
                text(") but it is not defined with hook syntax. "),
                text("(https://react.dev/reference/rules/rules-of-hooks)"),
            ]),
            MessageCannotCallObjectFunctionOnEnum {
                reason,
                enum_reason,
            } => {
                use super::error_message::enum_name_of_reason;
                let suggestion = match enum_name_of_reason(enum_reason) {
                    Some(enum_name) => vec![
                        text(" "),
                        text("You can use "),
                        code(&format!("{}.members()", enum_name)),
                        text(" to get an iterator of the enum's members. "),
                        text("You can turn that into an array using "),
                        code("Array.from"),
                        text(
                            ", which optionally takes a second argument if you wish to also map over the result.",
                        ),
                    ],
                    None => vec![],
                };
                let mut features = vec![
                    text("Cannot call function "),
                    ref_(reason),
                    text(" with argument "),
                    ref_(enum_reason),
                    text(" because it is not an object."),
                ];
                features.extend(suggestion);
                friendly::Message(features)
            }
            MessageCannotCallReactComponent(reason) => friendly::Message(vec![
                text("Cannot call "),
                ref_(reason),
                text(" because React components cannot be called. Use JSX instead. "),
                text("(https://react.dev/reference/rules/react-calls-components-and-hooks)"),
            ]),
            MessageCannotCallReactFunctionWithoutAtLeastNArgs { fn_name, n } => {
                let suffix = if *n == 1 { "" } else { "s" };
                friendly::Message(vec![
                    text("Cannot call "),
                    code(&format!("React.{}", fn_name)),
                    text(" "),
                    text(&format!("without at least {} argument{}.", n, suffix)),
                ])
            }
            MessageCannotCallReactHookConditionally(callee_loc) => friendly::Message(vec![
                text("Cannot call "),
                friendly::hardcoded_string_desc_ref("hook", loc_of_aloc(callee_loc)),
                text(" because React hooks cannot be called in conditional contexts. "),
                text("(https://react.dev/reference/rules/rules-of-hooks)"),
            ]),
            MessageCannotCallReactHookInDefinitelyNonComponentOrHook(callee_loc) => {
                friendly::Message(vec![
                    text("Cannot call "),
                    friendly::hardcoded_string_desc_ref("hook", loc_of_aloc(callee_loc)),
                    text(" because React hooks can only be called within components or hooks. "),
                    text("This hook is definitely not called in a component or hook. "),
                    text("(https://react.dev/reference/rules/rules-of-hooks)"),
                ])
            }
            MessageCannotCallReactHookInNonComponentSyntaxComponentOrHookSyntaxHook(callee_loc) => {
                friendly::Message(vec![
                    text("Cannot call "),
                    friendly::hardcoded_string_desc_ref("hook", loc_of_aloc(callee_loc)),
                    text(" because React hooks can only be called within components or hooks. "),
                    text("Under the current configuration, "),
                    text("we only infer component-syntax components to be components"),
                    text(" and hook-syntax hooks to be hooks. "),
                    text("(https://react.dev/reference/rules/rules-of-hooks)"),
                ])
            }
            MessageCannotCallReactHookInUnknownContext(callee_loc) => friendly::Message(vec![
                text("Cannot call "),
                friendly::hardcoded_string_desc_ref("hook", loc_of_aloc(callee_loc)),
                text(" because React hooks can only be called within components or hooks. "),
                text("(https://react.dev/reference/rules/rules-of-hooks)"),
            ]),
            MessageCannotCallReactHookWithIllegalName(callee_loc) => friendly::Message(vec![
                text("Cannot call hook because "),
                friendly::hardcoded_string_desc_ref("callee", loc_of_aloc(callee_loc)),
                text("'s name does not conform to React hook rules. Hook names must begin with "),
                code("use"),
                text(" followed by a capitalized letter. "),
                text("(https://react.dev/reference/rules/rules-of-hooks)"),
            ]),
            MessageCannotCallFunctionWithExtraArg {
                def_reason,
                param_count,
            } => {
                let msg = match param_count {
                    0 => "no arguments are expected by".to_string(),
                    1 => "no more than 1 argument is expected by".to_string(),
                    n => format!("no more than {} arguments are expected by", n),
                };
                friendly::Message(vec![text(&msg), text(" "), ref_(def_reason)])
            }
            MessageCannotChangeEnumMember(enum_reason) => friendly::Message(vec![
                text("Cannot change member of "),
                ref_(enum_reason),
                text(" because enums are frozen."),
            ]),
            MessageCannotCompare(box MessageCannotCompareData {
                lower,
                upper,
                strict_comparison_opt,
            }) => {
                let mut features = vec![
                    text("Cannot compare "),
                    ref_(lower),
                    text(" to "),
                    ref_(upper),
                ];
                match strict_comparison_opt {
                    None => features.push(text(".")),
                    Some(info) => {
                        use crate::intermediate_error_types::StrictComparisonKind::*;
                        let left_precise_reason = &info.left_precise_reason;
                        let right_precise_reason = &info.right_precise_reason;
                        match &info.strict_comparison_kind {
                            StrictComparisonGeneral => {
                                features.extend(vec![
                                    text(", because "),
                                    ref_(left_precise_reason),
                                    text(" is not a subtype of "),
                                    ref_(right_precise_reason),
                                    text(" and "),
                                    ref_(right_precise_reason),
                                    text(" is not a subtype of "),
                                    ref_(left_precise_reason),
                                    text(". In **rare** cases, these types may have overlapping values but lack a subtyping relationship. "),
                                    text("If that happens, you can cast one side to the union of both types to pass the flow check. "),
                                ]);
                            }
                            StrictComparisonNull { null_side } => {
                                use crate::intermediate_error_types::NullSide;
                                let (null_side_reason, the_other_side_reason) = match null_side {
                                    NullSide::Left => (left_precise_reason, right_precise_reason),
                                    NullSide::Right => (right_precise_reason, left_precise_reason),
                                };
                                features.extend(vec![
                                    text(", because"),
                                    friendly::no_desc_ref(&loc_of_aloc(&null_side_reason.loc)),
                                    text(" is null and "),
                                    ref_(the_other_side_reason),
                                    text(" does not contain null. "),
                                    text("Perhaps you meant to use "),
                                    code("=="),
                                    text(", which checks for both "),
                                    code("undefined"),
                                    text(" and "),
                                    code("null"),
                                    text("?"),
                                ]);
                            }
                            StrictComparisonEmpty { empty_side } => {
                                use crate::intermediate_error_types::EmptySide;
                                features.extend(vec![
                                    text(", because "),
                                    match empty_side {
                                        EmptySide::Left => ref_(left_precise_reason),
                                        EmptySide::Right => ref_(right_precise_reason),
                                    },
                                    text(" is empty. "),
                                ]);
                            }
                        }
                    }
                }
                friendly::Message(features)
            }
            MessageCannotCompareNonStrict { lower, upper } => friendly::Message(vec![
                text("Cannot compare "),
                ref_(lower),
                text(" to "),
                ref_(upper),
                text(" with a non-strict equality check. "),
                text("Make sure the arguments are valid, "),
                text("or try using strict equality ("),
                code("==="),
                text(" or "),
                code("!=="),
                text(") instead."),
            ]),
            MessageCannotCreateExactType(lower) => friendly::Message(vec![
                text("Cannot create exact type from "),
                ref_(lower),
                text("."),
            ]),
            MessageCannotDeclareAlreadyBoundGlobal(x) => friendly::Message(vec![
                text("Cannot redeclare global "),
                ref_(x),
                text(" because the global is already declared in another file."),
            ]),
            MessageCannotDeclareAlreadyBoundName(x) => friendly::Message(vec![
                text("Cannot declare "),
                ref_(x),
                text(" because the name is already bound."),
            ]),
            MessageCannotDeclareAlreadyBoundNameInCoreJs(x) => friendly::Message(vec![
                text("Cannot declare "),
                ref_(x),
                text(" because the name is a core builtin type."),
            ]),
            MessageCannotDeclareAlreadyBoundNameInNamespace(x) => friendly::Message(vec![
                text("Cannot declare the name in the namespace because the name "),
                ref_(x),
                text(" is already bound."),
            ]),
            MessageInterfaceMergePropertyConflict(x) => friendly::Message(vec![
                text("Duplicate property "),
                ref_(x),
                text(" in a merged interface declaration."),
            ]),
            MessageInterfaceMergeTparamMismatch(x) => friendly::Message(vec![
                text("Cannot merge interface "),
                ref_(x),
                text(" because type parameter lists differ."),
            ]),
            MessageCannotDeclareReservedType { reason, keyword } => friendly::Message(vec![
                text("Cannot declare "),
                ref_(reason),
                text(", because "),
                code(keyword.incorrect_of_kind()),
                text(" is a reserved type name."),
            ]),
            MessageCannotDelete(expr) => friendly::Message(vec![
                text("Cannot delete "),
                ref_(expr),
                text(" because only member expressions and variables can be deleted."),
            ]),
            MessageCannotDetermineEmptyArrayLiteralType => friendly::Message(vec![text(
                "Cannot determine type of empty array literal. Please provide an annotation.",
            )]),
            MessageCannotDetermineModuleType => friendly::Message(vec![
                text("Unable to determine module type (CommonJS vs ES) if both an export "),
                text("statement and "),
                code("module.exports"),
                text(" are used in the "),
                text("same module!"),
            ]),
            MessageCannotExportRenamedDefault(box MessageCannotExportRenamedDefaultData {
                name,
                is_reexport,
            }) => {
                let reexport_msg = vec![
                    text("If you intended to set the default export please "),
                    code("import"),
                    text(" and then "),
                    code("export default"),
                    text(" instead."),
                ];
                match (name, is_reexport) {
                    (None, _) => {
                        let mut features = vec![
                            text("Cannot set the default export of a module by re-exporting the "),
                            code("default"),
                            text(" property. "),
                        ];
                        features.extend(reexport_msg);
                        friendly::Message(features)
                    }
                    (Some(name), true) => {
                        let mut features = vec![
                            text("Cannot set the default export of a module by re-exporting "),
                            code(name),
                            text(" as "),
                            code("default"),
                            text(". "),
                        ];
                        features.extend(reexport_msg);
                        friendly::Message(features)
                    }
                    (Some(name), false) => friendly::Message(vec![
                        text("Cannot set the default export of a module by renaming "),
                        code(name),
                        text(" to "),
                        code("default"),
                        text(". If you intended to set the default export use "),
                        code(&format!("export default {}", name)),
                        text(" instead."),
                    ]),
                }
            }
            MessageCannotExhaustivelyCheckAbstractEnums(
                box MessageCannotExhaustivelyCheckAbstractEnumsData {
                    description,
                    enum_reason,
                },
            ) => friendly::Message(vec![
                text("Cannot exhaustively check "),
                friendly::desc_of_reason_desc(description),
                text(" because "),
                ref_(enum_reason),
                text(" is an abstract enum value, so has no members."),
            ]),
            MessageCannotExhaustivelyCheckEnumWithUnknowns(
                box MessageCannotExhaustivelyCheckEnumWithUnknownsData {
                    description,
                    enum_reason,
                },
            ) => friendly::Message(vec![
                text("Missing "),
                code("default"),
                text(" case in the check of "),
                friendly::desc_of_reason_desc(description),
                text(". "),
                ref_(enum_reason),
                text(" has unknown members (specified using "),
                code("..."),
                text(") so checking it requires the use of a "),
                code("default"),
                text(" case to cover those members."),
            ]),
            MessageCannotImplementNonInterface(i) => friendly::Message(vec![
                text("Cannot implement "),
                friendly::desc_of_reason_desc(i),
                text(" because it is not an interface."),
            ]),
            MessageCannotInstantiateObjectUtilTypeWithEnum(
                box MessageCannotInstantiateObjectUtilTypeWithEnumData {
                    description,
                    enum_reason,
                },
            ) => {
                use super::error_message::enum_name_of_reason;
                let suggestion = match enum_name_of_reason(enum_reason) {
                    Some(enum_name) => vec![
                        text(" "),
                        text("You can use the enum's name "),
                        code(&enum_name),
                        text(" as the type of its members."),
                    ],
                    None => vec![],
                };
                let mut features = vec![
                    text("Cannot instantiate "),
                    friendly::desc_of_reason_desc(description),
                    text(" because "),
                    ref_(enum_reason),
                    text(" is not an object."),
                ];
                features.extend(suggestion);
                friendly::Message(features)
            }
            MessageCannotIterateEnum { reason, for_in } => {
                use super::error_message::enum_name_of_reason;
                if *for_in {
                    let suggestion = match enum_name_of_reason(reason) {
                        Some(enum_name) => vec![
                            text(" "),
                            text("You can use "),
                            code(&format!("for (... of {}.members()) {{ ... }}", enum_name)),
                            text(" to iterate over the enum's members."),
                        ],
                        None => vec![],
                    };
                    let mut features = vec![
                        text("Cannot iterate using a "),
                        code("for...in"),
                        text(" loop because "),
                        ref_(reason),
                        text(" is not an object, null, or undefined."),
                    ];
                    features.extend(suggestion);
                    friendly::Message(features)
                } else {
                    let suggestion = match enum_name_of_reason(reason) {
                        Some(enum_name) => vec![
                            text(" "),
                            text("You can use "),
                            code(&format!("{}.members()", enum_name)),
                            text(" to get an iterator for the enum's members."),
                        ],
                        None => vec![],
                    };
                    let mut features = vec![desc(reason), text(" is not an iterable.")];
                    features.extend(suggestion);
                    friendly::Message(features)
                }
            }
            MessageCannotIterateWithForIn(reason) => friendly::Message(vec![
                text("Cannot iterate using a "),
                code("for...in"),
                text(" statement "),
                text("because "),
                ref_(reason),
                text(" is not an object, null, or undefined."),
            ]),
            MessageCannotMutateThisPrototype => {
                friendly::Message(vec![text("Mutating this prototype is unsupported.")])
            }
            MessageCannotNestComponents => friendly::Message(vec![text(
                "Components may not be nested directly within other components or hooks.",
            )]),
            MessageCannotNestHook => friendly::Message(vec![text(
                "Hooks may not be nested directly within other components or hooks.",
            )]),
            MessageCannotOptimizeUnionDueToNonUniqueKeys(non_unique_keys) => {
                let string_of_union_enum =
                    |x: &UnionEnum| -> friendly::MessageFeature<Loc> { code(&x.to_string()) };
                let string_of_non_unique_key =
                    |name: &Name,
                     map: &BTreeMap<UnionEnum, Vec1<VirtualReason<L>>>|
                     -> Vec<friendly::MessageFeature<Loc>> {
                        let ref_texts =
                            |rs: &Vec1<VirtualReason<L>>| -> Vec<friendly::MessageFeature<Loc>> {
                                let mut result = vec![ref_(&rs[0])];
                                for r in rs.iter().skip(1) {
                                    result.push(text(", "));
                                    result.push(ref_(r));
                                }
                                result
                            };
                        map.iter()
                            .flat_map(|(enum_val, rs)| {
                                let mut features = vec![
                                    text("\n - Key "),
                                    code(name.as_str()),
                                    text(" has value "),
                                    string_of_union_enum(enum_val),
                                    text(" in "),
                                ];
                                features.extend(ref_texts(rs));
                                features.push(text("."));
                                features
                            })
                            .collect()
                    };
                let keys: Vec<friendly::MessageFeature<Loc>> = non_unique_keys
                    .iter()
                    .flat_map(|(name, map)| string_of_non_unique_key(name, map))
                    .collect();
                let mut features = vec![text(
                    "Union could not be fully optimized internally. The following keys have non-unique values:",
                )];
                features.extend(keys);
                friendly::Message(features)
            }
            MessageCannotOptimizeUnionInternally(kind) => {
                use flow_typing_type::type_::union_rep::OptimizedError;
                let kind_msg = match kind {
                    OptimizedError::ContainsUnresolved(r) => vec![
                        text("The form of "),
                        ref_(r),
                        text(" is not supported for optimization. "),
                        text("Try replacing this type with a simpler alternative."),
                    ],
                    OptimizedError::NoCandidateMembers => vec![
                        text("The union needs to include in its members "),
                        text("at least one of: "),
                        text("object type, string literal, numeric literal, "),
                        text("boolean literal, void or null types."),
                    ],
                    OptimizedError::NoCommonKeys => {
                        vec![text(
                            "There are no common keys among the members of the union.",
                        )]
                    }
                };
                let mut features = vec![text("Union could not be optimized internally. ")];
                features.extend(kind_msg);
                friendly::Message(features)
            }
            MessageCannotPassReactRefAsArgument { usage, in_hook } => {
                let context = if *in_hook {
                    text("within hooks")
                } else {
                    text("during render")
                };
                friendly::Message(vec![
                    text("Cannot pass "),
                    ref_(usage),
                    text(" as an argument because "),
                    code("ref"),
                    text(
                        " values may not be passed to functions because they could read the ref value (",
                    ),
                    code("current"),
                    text(") property) "),
                    context,
                    text(". (https://react.dev/reference/react/useRef)."),
                ])
            }
            MessageCannotPerformArithOnNonNumbersOrBigInt(reason) => friendly::Message(vec![
                text("Cannot perform arithmetic operation because "),
                ref_(reason),
                text(" "),
                text("is not a number or bigint."),
            ]),
            MessageCannotPerformBigIntRShift3(reason) => friendly::Message(vec![
                text("Cannot perform unsigned right shift because "),
                ref_(reason),
                text(" "),
                text("is a bigint, and all bigints are signed."),
            ]),
            MessageCannotPerformBigIntUnaryPlus(reason) => friendly::Message(vec![
                text("Cannot perform unary plus because a "),
                ref_(reason),
                text(" "),
                text("cannot be coerced to number."),
            ]),
            MessageCannotPerformBinaryArith {
                kind,
                reason_l,
                reason_r,
            } => {
                let kind_str = kind.to_string();
                friendly::Message(vec![
                    text("Cannot use operator `"),
                    text(kind_str),
                    text("` with operands "),
                    ref_(reason_l),
                    text(" and "),
                    ref_(reason_r),
                ])
            }
            MessageCannotReassignConstant(x) => {
                friendly::Message(vec![text("Cannot reassign constant "), ref_(x), text(".")])
            }
            MessageCannotReassignConstantLikeBinding {
                definition,
                binding_kind,
            } => friendly::Message(vec![
                text("Cannot reassign "),
                text(binding_kind.as_str()),
                text(" binding "),
                ref_(definition),
                text("."),
            ]),
            MessageCannotReassignEnum(x) => {
                friendly::Message(vec![text("Cannot reassign enum "), ref_(x), text(".")])
            }
            MessageCannotReassignImport(x) => {
                friendly::Message(vec![text("Cannot reassign import "), ref_(x), text(".")])
            }
            MessageCannotRedeclareVar(x) => friendly::Message(vec![
                text("Cannot declare "),
                ref_(x),
                text(" because var redeclaration is not supported."),
            ]),
            MessageCannotReferenceTypeGuardParameter {
                type_guard_reason,
                binding_reason,
            } => friendly::Message(vec![
                text("A "),
                ref_(type_guard_reason),
                text(" cannot reference "),
                ref_(binding_reason),
                text("."),
            ]),
            MessageCannotResolveBuiltinName(name) => {
                friendly::Message(vec![text("Cannot resolve name "), code(name), text(".")])
            }
            MessageCannotResolveBuiltinModule(box MessageCannotResolveBuiltinModuleData {
                name,
                potential_generator,
            }) => {
                let mut features = vec![text("Cannot resolve module "), code(name), text(".")];
                if let Some(generator) = potential_generator {
                    features.extend(vec![
                        text(" Try running the command "),
                        code(generator),
                        text(" to generate the missing module."),
                    ]);
                }
                friendly::Message(features)
            }
            MessageCannotResolveExpectedModule {
                name,
                expected_module_purpose,
            } => {
                use super::intermediate_error_types::ExpectedModulePurpose;
                let explanation = match expected_module_purpose {
                    ExpectedModulePurpose::ReactModuleForJSXFragment => vec![
                        text("The "),
                        code("react"),
                        text(" module must exist to type check JSX fragments."),
                    ],
                    ExpectedModulePurpose::ReactModuleForReactClassComponent => vec![
                        text("The "),
                        code("react"),
                        text(" module must exist to type check React class components."),
                    ],
                    ExpectedModulePurpose::ReactModuleForReactMixedElementType => vec![
                        text("The "),
                        code("react"),
                        text(" module must exist to provide a type for "),
                        code("React.MixedElement"),
                        text("."),
                    ],
                    ExpectedModulePurpose::ReactModuleForReactNodeType => vec![
                        text("The "),
                        code("react"),
                        text(" module must exist to provide a type for "),
                        code("React.Node"),
                        text("."),
                    ],
                    ExpectedModulePurpose::ReactModuleForReactRefSetterType => vec![
                        text("The "),
                        code("react"),
                        text(" module must exist to provide a type for "),
                        code("React.RefSetter"),
                        text("."),
                    ],
                    ExpectedModulePurpose::ReactModuleForReactElementRefType => vec![
                        text("The "),
                        code("react"),
                        text(" module must exist to provide a type for "),
                        code("React.ElementRef"),
                        text("."),
                    ],
                };
                let mut features = vec![text("Cannot resolve module "), code(name), text(". ")];
                features.extend(explanation);
                friendly::Message(features)
            }
            MessageCannotSpreadDueToPotentialOverwrite {
                spread_reason,
                object_reason,
                key_reason,
            } => friendly::Message(vec![
                text("Flow cannot determine a type for "),
                ref_(spread_reason),
                text(". "),
                ref_(object_reason),
                text(" cannot be spread because the indexer "),
                ref_(key_reason),
                text(
                    " may overwrite properties with explicit keys in a way that Flow cannot track. ",
                ),
                text("Try spreading "),
                ref_(object_reason),
                text(" first or remove the indexer"),
            ]),
            MessageCannotSpreadGeneral(box MessageCannotSpreadGeneralData {
                spread_reason,
                object1_reason,
                object2_reason,
                propname,
                error_kind,
            }) => {
                use super::intermediate_error_types::ExactnessErrorKind;
                let (error_reason, fix_suggestion) = match error_kind {
                    ExactnessErrorKind::UnexpectedInexact => (
                        "is inexact",
                        vec![text(" Try making "), ref_(object2_reason), text(" exact")],
                    ),
                    ExactnessErrorKind::UnexpectedIndexer => (
                        "has an indexer",
                        vec![
                            text(" Try removing the indexer in "),
                            ref_(object2_reason),
                            text(" or make "),
                            code(propname.as_str()),
                            text(" a required property"),
                        ],
                    ),
                };
                let mut features = vec![
                    text("Flow cannot determine a type for "),
                    ref_(spread_reason),
                    text(". "),
                    ref_(object2_reason),
                    text(" "),
                    text(error_reason),
                    text(", so it may contain "),
                    code(propname.as_str()),
                    text(" with a type that conflicts with "),
                    code(propname.as_str()),
                    text("'s definition in "),
                    ref_(object1_reason),
                    text("."),
                ];
                features.extend(fix_suggestion);
                friendly::Message(features)
            }
            MessageCannotSpreadInexactMayOverwriteIndexer(
                box MessageCannotSpreadInexactMayOverwriteIndexerData {
                    spread_reason,
                    key_reason,
                    value_reason,
                    object2_reason,
                },
            ) => friendly::Message(vec![
                text("Flow cannot determine a type for "),
                ref_(spread_reason),
                text(". "),
                ref_(object2_reason),
                text(" is inexact and may "),
                text("have a property key that conflicts with "),
                ref_(key_reason),
                text(" or a property value that conflicts with "),
                ref_(value_reason),
                text(". Try making "),
                ref_(object2_reason),
                text(" exact"),
            ]),
            MessageCannotSpreadInterface {
                spread_reason,
                interface_reason,
            } => friendly::Message(vec![
                text("Flow cannot determine a type for "),
                ref_(spread_reason),
                text(". "),
                ref_(interface_reason),
                text(" cannot be spread because interfaces do not "),
                text("track the own-ness of their properties. Try using an object type instead"),
            ]),
            MessageCannotUseAsConstructor(reason) => friendly::Message(vec![
                text("Cannot use "),
                code("new"),
                text(" on "),
                ref_(reason),
                text(
                    ". Only classes and interfaces with a construct signature can be constructed.",
                ),
            ]),
            MessageCannotUseAsPrototype(reason) => friendly::Message(vec![
                text("Cannot use "),
                ref_(reason),
                text(" as a prototype. Expected an object or null."),
            ]),
            MessageCannotUseAsSuperClass(reason) => friendly::Message(vec![
                text("Cannot use "),
                ref_(reason),
                text(" as a superclass. Only variables and member expressions may be extended"),
            ]),
            MessageCannotUseBeforeDeclaration(x) => friendly::Message(vec![
                text("Cannot use variable "),
                ref_(x),
                text(" because the declaration "),
                text("either comes later or was skipped."),
            ]),
            MessageCannotUseComputedPropertyWithUnion(computed_property_reason) => {
                friendly::Message(vec![
                    text("Cannot use "),
                    ref_(computed_property_reason),
                    text(" as a computed property."),
                    text(
                        " Computed properties may only be primitive literal values, but the type of ",
                    ),
                    ref_(computed_property_reason),
                    text(" is a union. Can you add a literal type annotation to "),
                    ref_(computed_property_reason),
                    text("?"),
                    text(
                        " See https://flow.org/en/docs/types/literals/ for more information on literal types.",
                    ),
                ])
            }
            MessageCannotUseDefaultImportWithDestrucuturing => friendly::Message(vec![
                text(
                    "The default export of a module cannot be accessed from import destructuring. ",
                ),
                text("To use the default export you must import it directly."),
            ]),
            MessageCannotUseDollarExports => friendly::Message(vec![
                text("Cannot use "),
                code("$Exports"),
                text(" because the first type "),
                text("argument must be a string literal."),
            ]),
            MessageCannotUseEnumMemberUsedAsType(
                box MessageCannotUseEnumMemberUsedAsTypeData {
                    description,
                    enum_reason,
                },
            ) => friendly::Message(vec![
                text("Cannot use "),
                friendly::desc_of_reason_desc(description),
                text(" as a type. "),
                text("Enum members are not separate types. "),
                text("Only the enum itself, "),
                ref_(enum_reason),
                text(", is a type."),
            ]),
            MessageCannotUseExportInNonLegalToplevelContext(name) => friendly::Message(vec![
                code(name),
                text(" may only be used as part of a legal top level export statement"),
            ]),
            MessageCannotUseImportStar(import_star_reason) => friendly::Message(vec![
                ref_(import_star_reason),
                text(" object can only be used by accessing one of its named exports"),
                text(" with a member access or destructuring."),
            ]),
            MessageCannotUseInOperatorDueToBadLHS(reason) => friendly::Message(vec![
                text("Cannot use "),
                code("in"),
                text(" because on the left-hand side, "),
                ref_(reason),
                text(" must be a string or number."),
            ]),
            MessageCannotUseInOperatorDueToBadRHS(reason) => friendly::Message(vec![
                text("Cannot use "),
                code("in"),
                text(" because on the right-hand side, "),
                ref_(reason),
                text(" must be an object or array."),
            ]),
            MessageCannotUseInstanceOfOperatorDueToBadRHS(reason) => friendly::Message(vec![
                text("The right-hand side of an "),
                code("instanceof"),
                text(" expression must be an object, but got "),
                ref_(reason),
                text("."),
            ]),
            MessageCannotUseMixedImportAndRequire(import_reason) => friendly::Message(vec![
                text("Cannot use a mix of non-type toplevel "),
                ref_(import_reason),
                text(" and "),
                code("require"),
                text(" statements in the same file."),
            ]),
            MessageCannotUseNonPolymorphicTypeWithTypeArgs {
                is_new,
                reason_arity,
                expected_arity,
            } => {
                let use_word = if *is_new { "construct " } else { "call " };
                if *expected_arity == 0 {
                    friendly::Message(vec![
                        text("Cannot "),
                        text(use_word),
                        text("non-polymorphic "),
                        ref_(reason_arity),
                        text(" with type arguments."),
                    ])
                } else {
                    let suffix = if *expected_arity == 1 { "" } else { "s" };
                    friendly::Message(vec![
                        text("Cannot "),
                        text(use_word),
                        ref_(reason_arity),
                        text(" without exactly "),
                        text(&format!("{} type argument{}.", expected_arity, suffix)),
                    ])
                }
            }
            MessageCannotUsePrimitiveAsInterface {
                reason,
                interface_reason,
                kind,
            } => {
                let kind_str = match kind {
                    super::intermediate_error_types::PrimitiveKind::Boolean => "Boolean",
                    super::intermediate_error_types::PrimitiveKind::Number => "Number",
                    super::intermediate_error_types::PrimitiveKind::String => "String",
                };
                friendly::Message(vec![
                    ref_(reason),
                    text(", a primitive, cannot be used as a subtype of "),
                    ref_(interface_reason),
                    text(". "),
                    text("You can wrap it in "),
                    code(&format!("new {}(...))", kind_str)),
                    text(
                        " to turn it into an object and attempt to use it as a subtype of an interface",
                    ),
                ])
            }
            MessageCannotUseStrUtilType => friendly::Message(vec![
                text("Cannot use "),
                code("StringPrefix"),
                text(" because the first type argument must be a string literal."),
            ]),
            MessageCannotUseTypeDueToPolarityMismatch {
                reason_targ,
                expected_polarity,
                actual_polarity,
            } => {
                let polarity_string = |p: &Polarity| match p {
                    Polarity::Positive => "output",
                    Polarity::Negative => "input",
                    Polarity::Neutral => "input/output",
                };
                let expected = polarity_string(expected_polarity);
                let actual = polarity_string(actual_polarity);
                friendly::Message(vec![
                    text("Cannot use "),
                    ref_(reason_targ),
                    text(&format!(" in an {} ", actual)),
                    text("position because "),
                    ref_(reason_targ),
                    text(" is expected to occur only in "),
                    text(&format!("{} positions.", expected)),
                ])
            }
            MessageCannotUseTypeForAnnotationInference(
                box MessageCannotUseTypeForAnnotationInferenceData {
                    reason_op,
                    reason,
                    suggestion,
                },
            ) => {
                let unwrapped_reason = reason.dupe().replace_desc(reason.desc(true).clone());
                let mut features = vec![
                    text("Cannot use "),
                    friendly::desc_of_reason_desc(&reason_op.desc(true).map_locs(&loc_of_aloc)),
                    text(" on "),
                    ref_(&unwrapped_reason),
                    text(" in an export position. "),
                    text("Please provide an (alternative) annotation for "),
                    ref_(reason_op),
                    text("."),
                ];
                if let Some(util) = suggestion {
                    features.extend(vec![
                        text(" (Try using the "),
                        code(util),
                        text(" utility type instead.)"),
                    ]);
                }
                friendly::Message(features)
            }
            MessageCannotUseTypeGuardWithFunctionParamHavoced(
                box MessageCannotUseTypeGuardWithFunctionParamHavocedData {
                    type_guard_desc,
                    param_reason,
                    call_locs,
                },
            ) => {
                let loc_str: Vec<_> = if call_locs.is_empty() {
                    vec![text("in this function")]
                } else if call_locs.len() == 1 {
                    vec![
                        text("in"),
                        friendly::no_desc_ref(&loc_of_aloc(&call_locs[0])),
                    ]
                } else {
                    let mut v = vec![text("in the following expressions:")];
                    for loc in call_locs {
                        v.push(friendly::no_desc_ref(&loc_of_aloc(loc)));
                    }
                    v
                };
                let mut features = vec![
                    text("Cannot use "),
                    friendly::desc_of_reason_desc(type_guard_desc),
                    text(", because "),
                    ref_(param_reason),
                    text(" is reassigned "),
                ];
                features.extend(loc_str);
                features.push(text("."));
                friendly::Message(features)
            }
            MessageCannotUseTypeInValuePosition(box MessageCannotUseTypeInValuePositionData {
                reason,
                type_only_namespace,
                imported_name,
            }) => {
                let base = if *type_only_namespace {
                    vec![
                        text("Cannot use type-only namespace "),
                        ref_(reason),
                        text(" as a value. "),
                        text("Type-only namespaces are erased and don't exist at runtime."),
                    ]
                } else {
                    vec![
                        text("Cannot use type "),
                        ref_(reason),
                        text(" as a value. "),
                        text("Types are erased and don't exist at runtime."),
                    ]
                };
                match imported_name {
                    None => friendly::Message(base),
                    Some(name) => {
                        let mut features = base;
                        features.extend(vec![
                            text(" If the exported binding can also be used as a value, try importing it using "),
                            code(&format!("import {}", name)),
                            text(" instead of "),
                            code(&format!("import type {}", name)),
                            text(" and "),
                            code(&format!("import {{{}}}", name)),
                            text(" instead of "),
                            code(&format!("import type {{{}}}", name)),
                            text("."),
                        ]);
                        friendly::Message(features)
                    }
                }
            }
            MessageCannotUseTypeWithInvalidTypeArgs {
                reason_main,
                reason_tapp,
            } => friendly::Message(vec![
                text("Cannot use "),
                ref_(reason_main),
                text(" with "),
                ref_(reason_tapp),
                text(" argument"),
            ]),
            MessageCannotUseTypeWithoutAnyTypeArgs {
                reason_arity,
                min_arity,
                max_arity,
            } => {
                let (arity, args) = if min_arity == max_arity {
                    (
                        format!("{}", max_arity),
                        if *max_arity == 1 {
                            "argument"
                        } else {
                            "arguments"
                        },
                    )
                } else {
                    (format!("{}-{}", min_arity, max_arity), "arguments")
                };
                friendly::Message(vec![
                    text("Cannot use "),
                    ref_(reason_arity),
                    text(&format!(" without {} type {}.", arity, args)),
                ])
            }
            MessageCannotUseTypeWithoutAtLeastNTypeArgs(n) => {
                let suffix = if *n == 1 { "argument" } else { "arguments" };
                friendly::Message(vec![
                    text("Cannot use type without at least "),
                    text(&format!("{} type {}.", n, suffix)),
                ])
            }
            MessageCannotUseTypeWithoutExactlyNTypeArgs(n) => {
                let suffix = if *n == 1 { "argument" } else { "arguments" };
                friendly::Message(vec![
                    text("Cannot use type without exactly "),
                    text(&format!("{} type {}.", n, suffix)),
                ])
            }
            MessageCannotUseTypeWithTooFewTypeArgs { reason_arity, n } => {
                let suffix = if *n == 1 { "argument" } else { "arguments" };
                friendly::Message(vec![
                    text("Cannot use "),
                    ref_(reason_arity),
                    text(" with fewer than "),
                    text(&format!("{} type {}.", n, suffix)),
                ])
            }
            MessageCannotUseTypeWithTooManyTypeArgs { reason_arity, n } => {
                let suffix = if *n == 1 { "argument" } else { "arguments" };
                friendly::Message(vec![
                    text("Cannot use "),
                    ref_(reason_arity),
                    text(" with more than "),
                    text(&format!("{} type {}.", n, suffix)),
                ])
            }
            MessageCannotUseThisSuperBeforeSuperCall(x) => friendly::Message(vec![
                text("Must call "),
                code("super"),
                text(" before accessing "),
                ref_(x),
                text(" in a derived constructor."),
            ]),
            MessageComponentMissingReturn(reason) => friendly::Message(vec![
                text("Cannot declare component because "),
                ref_(reason),
                text(
                    " is not guaranteed to reach a return statement. An explicit return statement must be included for all possible branches.",
                ),
            ]),
            MessageComponentMissingBody => friendly::Message(vec![text(
                "Components in non-ambient contexts must have a body.",
            )]),
            MessageComponentBodyInAmbientContext => friendly::Message(vec![text(
                "Components in ambient contexts (library definitions, .flow files, declare module, declare namespace) cannot have a body.",
            )]),
            MessageComponentNonUpperCase => friendly::Message(vec![text(
                "Component identifiers must begin with an upper-case character",
            )]),
            MessageDeclareComponentInvalidParam(kind) => {
                use crate::intermediate_error_types::DeclareComponentInvalidParamKind::*;
                match kind {
                    DeclareComponentParamAsBinding => friendly::Message(vec![text(
                        "Cannot use `as` binding in `declare component` parameters, unless the param name is a string literal. Use the type-only syntax `paramName: Type` instead.",
                    )]),
                    DeclareComponentParamDefaultValue => friendly::Message(vec![text(
                        "Cannot use default values in `declare component` parameters.",
                    )]),
                    DeclareComponentParamMissingAnnotation => friendly::Message(vec![text(
                        "Missing type annotation on `declare component` parameter.",
                    )]),
                    DeclareComponentParamStringLiteralWithoutAs => friendly::Message(vec![text(
                        "String literal parameter names in `declare component` require an `as` binding. Use `'param-name' as paramName: Type`.",
                    )]),
                }
            }
            MessageDefinitionCycle(dependencies) => {
                use flow_env_builder::env_api::AnnotLoc;
                let compare = |a: &L, b: &L| loc_of_aloc(a).cmp(&loc_of_aloc(b));
                let deps: Vec<friendly::MessageFeature<Loc>> = dependencies
                    .iter()
                    .enumerate()
                    .filter_map(|(i, (reason, dep, _annot_locs))| {
                        if dep.is_empty() {
                            return None;
                        }
                        if i == 10 {
                            return Some(vec![text(" - ...\n")]);
                        }
                        if i > 10 {
                            return None;
                        }
                        let mut sorted_dep = dep.clone();
                        sorted_dep.sort_by(&compare);
                        sorted_dep.dedup_by(|a, b| compare(a, b) == std::cmp::Ordering::Equal);
                        let hd = sorted_dep.remove(0);
                        let tl = sorted_dep;
                        let (suffix, tl): (Vec<friendly::MessageFeature<Loc>>, Vec<L>) =
                            if tl.len() > 4 {
                                (vec![text(", [...]")], tl.into_iter().take(4).collect())
                            } else {
                                (vec![], tl)
                            };
                        let tl_dep: Vec<friendly::MessageFeature<Loc>> = tl
                            .iter()
                            .flat_map(|loc| {
                                vec![text(","), friendly::no_desc_ref(&loc_of_aloc(loc))]
                            })
                            .collect();
                        let mut result = vec![
                            text(" - "),
                            ref_(reason),
                            text(" depends on "),
                            friendly::hardcoded_string_desc_ref(
                                "other definition",
                                loc_of_aloc(&hd),
                            ),
                        ];
                        result.extend(tl_dep);
                        result.extend(suffix);
                        result.push(text("\n"));
                        Some(result)
                    })
                    .flatten()
                    .collect();
                let (mut locs, mut properties): (Vec<L>, Vec<L>) = (vec![], vec![]);
                for (_reason, _dep, annot_locs) in dependencies {
                    for annot_loc in annot_locs {
                        match annot_loc {
                            AnnotLoc::Loc(l) => locs.push(l.dupe()),
                            AnnotLoc::Object { loc, props } => {
                                locs.push(loc.dupe());
                                properties.extend(props.iter().map(|p| p.dupe()));
                            }
                        }
                    }
                }
                locs.sort_by(&compare);
                locs.dedup_by(|a, b| compare(a, b) == std::cmp::Ordering::Equal);
                locs.truncate(10);
                properties.sort_by(&compare);
                properties.dedup_by(|a, b| compare(a, b) == std::cmp::Ordering::Equal);
                let annot_message = |ls: &[L]| -> Vec<friendly::MessageFeature<Loc>> {
                    ls.iter()
                        .map(|l| friendly::no_desc_ref(&loc_of_aloc(l)))
                        .collect()
                };
                let mut features = vec![text(
                    "The following definitions recursively depend on each other, and Flow cannot compute their types:\n",
                )];
                features.extend(deps);
                features.push(text("Please add type annotations to these definitions"));
                features.extend(annot_message(&locs));
                if !properties.is_empty() && properties.len() <= 5 {
                    features.push(text(" or to these object properties"));
                    features.extend(annot_message(&properties));
                }
                friendly::Message(features)
            }
            MessageDefinitionInvalidRecursive(box MessageDefinitionInvalidRecursiveData {
                description,
                recursion,
                annot_locs,
            }) => {
                use flow_env_builder::env_api::AnnotLoc;
                let (itself, tl_recur): (
                    friendly::MessageFeature<Loc>,
                    Vec<friendly::MessageFeature<Loc>>,
                ) = match recursion.as_slice() {
                    [hd, tl @ ..] => {
                        let (suffix, tl): (Vec<friendly::MessageFeature<Loc>>, &[L]) =
                            if tl.len() > 4 {
                                (vec![text(", [...]")], &tl[..4])
                            } else {
                                (vec![], tl)
                            };
                        let mut tl_recur: Vec<friendly::MessageFeature<Loc>> = tl
                            .iter()
                            .flat_map(|loc| {
                                vec![text(", "), friendly::no_desc_ref(&loc_of_aloc(loc))]
                            })
                            .collect();
                        tl_recur.extend(suffix);
                        (
                            friendly::hardcoded_string_desc_ref("itself", loc_of_aloc(hd)),
                            tl_recur,
                        )
                    }
                    [] => (text("itself"), vec![]),
                };
                let annot_message: Vec<friendly::MessageFeature<Loc>> = match annot_locs.as_slice()
                {
                    [] => vec![text("this definition")],
                    [AnnotLoc::Loc(loc)] => {
                        vec![friendly::hardcoded_string_desc_ref(
                            "this definition",
                            loc_of_aloc(loc),
                        )]
                    }
                    [AnnotLoc::Object { loc, props }] if props.is_empty() => {
                        vec![friendly::hardcoded_string_desc_ref(
                            "this definition",
                            loc_of_aloc(loc),
                        )]
                    }
                    [AnnotLoc::Object { loc, props }] if props.len() > 5 => {
                        vec![friendly::hardcoded_string_desc_ref(
                            "this definition",
                            loc_of_aloc(loc),
                        )]
                    }
                    [AnnotLoc::Object { loc, props }] if props.len() == 1 => vec![
                        friendly::hardcoded_string_desc_ref("this definition", loc_of_aloc(loc)),
                        text("or to"),
                        friendly::hardcoded_string_desc_ref("its property", loc_of_aloc(&props[0])),
                    ],
                    [AnnotLoc::Object { loc, props }] => {
                        let mut result = vec![
                            friendly::hardcoded_string_desc_ref(
                                "this definition",
                                loc_of_aloc(loc),
                            ),
                            text(" or to its properties"),
                        ];
                        for prop in props {
                            result.push(friendly::no_desc_ref(&loc_of_aloc(prop)));
                        }
                        result
                    }
                    ls => {
                        let compare = |a: &L, b: &L| loc_of_aloc(a).cmp(&loc_of_aloc(b));
                        let (mut locs, mut properties): (Vec<L>, Vec<L>) = (vec![], vec![]);
                        for annot_loc in ls {
                            match annot_loc {
                                AnnotLoc::Loc(l) => locs.push(l.dupe()),
                                AnnotLoc::Object { loc, props } => {
                                    locs.push(loc.dupe());
                                    properties.extend(props.iter().map(|p| p.dupe()));
                                }
                            }
                        }
                        locs.sort_by(&compare);
                        locs.dedup_by(|a, b| compare(a, b) == std::cmp::Ordering::Equal);
                        locs.truncate(10);
                        properties.sort_by(&compare);
                        properties.dedup_by(|a, b| compare(a, b) == std::cmp::Ordering::Equal);
                        let props: Vec<friendly::MessageFeature<Loc>> =
                            if !properties.is_empty() && properties.len() <= 5 {
                                let these = if properties.len() > 1 {
                                    text("these object properties")
                                } else {
                                    text("this object property")
                                };
                                let mut result = vec![text(" or to "), these];
                                for prop in &properties {
                                    result.push(friendly::no_desc_ref(&loc_of_aloc(prop)));
                                }
                                result
                            } else {
                                vec![]
                            };
                        let mut result = vec![text("these definitions")];
                        for l in &locs {
                            result.push(friendly::no_desc_ref(&loc_of_aloc(l)));
                        }
                        result.extend(props);
                        result
                    }
                };
                let mut features = vec![
                    text("Cannot compute a type for "),
                    friendly::desc_of_reason_desc(description),
                    text(" because its definition includes references to "),
                    itself,
                ];
                features.extend(tl_recur);
                features.push(text(". Please add an annotation to "));
                features.extend(annot_message);
                friendly::Message(features)
            }
            MessageDeprecatedBool => friendly::Message(vec![
                text("Deprecated type. Use "),
                code("boolean"),
                text(" instead."),
            ]),
            MessageDeprecatedTypeParamColonBound => friendly::Message(vec![
                text("Using "),
                code(":"),
                text(" for type parameter bounds is deprecated. Use "),
                code("extends"),
                text(" instead (e.g., "),
                code("<T extends Bound>"),
                text(")."),
            ]),
            MessageDocblockError(err) => {
                use super::intermediate_error_types::DocblockError;
                match err {
                    DocblockError::MultipleFlowAttributes => friendly::Message(vec![
                        text("Unexpected "),
                        code("@flow"),
                        text(" declaration. Only one per "),
                        text("file is allowed."),
                    ]),
                    DocblockError::InvalidFlowMode(s) => friendly::Message(vec![
                        code(&format!("@flow {}", s)),
                        text(" is not a valid "),
                        code("@flow"),
                        text(" mode. Valid ones are: "),
                        code("@flow"),
                        text(", "),
                        code("@flow strict"),
                        text(", and "),
                        code("@flow strict-local"),
                        text("."),
                    ]),
                    DocblockError::MultipleJSXAttributes => friendly::Message(vec![
                        text("Unexpected "),
                        code("@jsx"),
                        text(" declaration. Only one per "),
                        text("file is allowed."),
                    ]),
                    DocblockError::InvalidJSXAttribute(first_error) => {
                        let mut features = vec![
                            text("Invalid "),
                            code("@jsx"),
                            text(" declaration. Should have the form "),
                            code("@jsx LeftHandSideExpression"),
                            text(" with no spaces."),
                        ];
                        if let Some(err) = first_error {
                            features.push(text(&format!(" Parse error: {}.", err)));
                        }
                        friendly::Message(features)
                    }
                    DocblockError::MultipleJSXRuntimeAttributes => friendly::Message(vec![
                        text("Unexpected "),
                        code("@jsxRuntime"),
                        text(" declaration. Only one per "),
                        text("file is allowed."),
                    ]),
                    DocblockError::InvalidJSXRuntimeAttribute => friendly::Message(vec![
                        text("Invalid "),
                        code("@jsxRuntime"),
                        text(" declaration. The only supported values are "),
                        code("classic"),
                        text(" and "),
                        code("automatic"),
                        text("."),
                    ]),
                    DocblockError::InvalidSupportsPlatform(p) => friendly::Message(vec![
                        text("Invalid "),
                        code("@supportsPlatform"),
                        text(" declaration. "),
                        code(p),
                        text(" is not configured in "),
                        code("experimental.multi_platform.extensions"),
                        text(" in your flow config."),
                    ]),
                    DocblockError::DisallowedSupportsPlatform => friendly::Message(vec![
                        code("@supportsPlatform"),
                        text(" declaration is disallowed in platform specific files."),
                    ]),
                }
            }
            MessageDuplicateClassMember {
                name,
                static_,
                class_kind,
            } => {
                let static_str = if *static_ { "static " } else { "" };
                let (class_kind_str, item_name) = match class_kind {
                    super::intermediate_error_types::ClassKind::Class => ("class", "member"),
                    super::intermediate_error_types::ClassKind::Record => ("record", "property"),
                };
                let member_type = format!("{}{} {}", static_str, class_kind_str, item_name);
                let member_type = member_type
                    .chars()
                    .next()
                    .map(|c| c.to_ascii_uppercase().to_string())
                    .unwrap_or_default()
                    + &member_type[1..];
                friendly::Message(vec![
                    code(name.as_str()),
                    text(" has already been declared in this "),
                    text(class_kind_str),
                    text(". "),
                    text(&member_type),
                    text(" names must be unique."),
                ])
            }
            MessageDuplicateEnumMember {
                prev_use_loc,
                enum_reason,
            } => friendly::Message(vec![
                text(
                    "Invalid enum member initializer. Initializers need to be unique, but this one ",
                ),
                text("has already been used for a "),
                friendly::hardcoded_string_desc_ref("previous member", loc_of_aloc(prev_use_loc)),
                text(" of "),
                ref_(enum_reason),
                text("."),
            ]),
            MessageDuplicateModuleProvider(box MessageDuplicateModuleProviderData {
                module_name,
                provider,
                conflict,
            }) => {
                let provider_file = loc_of_aloc(provider).source.clone();
                let conflict_file = loc_of_aloc(conflict).source.clone();
                match (&provider_file, &conflict_file) {
                    (Some(pf), Some(cf))
                        if pf.check_suffix(flow_common::files::FLOW_EXT)
                            && cf.check_suffix(".js") =>
                    {
                        friendly::Message(vec![
                            text("This file is being illegally shadowed by the "),
                            friendly::hardcoded_string_desc_ref(
                                "js.flow file",
                                loc_of_aloc(provider),
                            ),
                            text(". This file can only be shadowed by a js.flow file "),
                            text("in the same directory with the same base name."),
                        ])
                    }
                    _ => friendly::Message(vec![
                        text("Duplicate module provider for "),
                        code(module_name),
                        text(". Change "),
                        text("either the name of this file or the "),
                        friendly::hardcoded_string_desc_ref(
                            "name of the current module provider",
                            loc_of_aloc(provider),
                        ),
                        text("."),
                    ]),
                }
            }
            MessageEnumsNotEnabled => friendly::Message(vec![
                text("Flow Enums are not enabled. "),
                text("You may opt-in to using enums by putting "),
                code("enums=true"),
                text(" into the "),
                code("[options]"),
                text(" section of your "),
                code(".flowconfig"),
                text("."),
            ]),
            MessageEnumConstNotSupported => friendly::Message(vec![
                code("const"),
                text(
                    " enums are not supported. Flow Enums are designed to allow for inlining, however the inlining itself needs to be part of the build system (whatever you use) rather than Flow itself.",
                ),
            ]),
            MessageEnumNonIdentifierMemberName {
                member_name,
                enum_reason,
            } => friendly::Message(vec![
                text("Enum member names must be identifiers, not string literals. "),
                code(member_name),
                text(" is not a valid member name in "),
                ref_(enum_reason),
                text("."),
            ]),
            MessageInvalidEnumMemberName {
                member_name,
                enum_reason,
            } => {
                let suggestion = {
                    let mut chars: Vec<char> = member_name.chars().collect();
                    if !chars.is_empty() {
                        chars[0] = chars[0].to_ascii_uppercase();
                    }
                    chars.into_iter().collect::<String>()
                };
                friendly::Message(vec![
                    text(
                        "Enum member names cannot start with lowercase 'a' through 'z'. Instead of using ",
                    ),
                    code(member_name),
                    text(", consider using "),
                    code(&suggestion),
                    text(", in "),
                    ref_(enum_reason),
                    text("."),
                ])
            }
            MessageEnumDuplicateMemberName(box MessageEnumDuplicateMemberNameData {
                member_name,
                prev_use_loc,
                enum_reason,
            }) => friendly::Message(vec![
                text("Enum member names need to be unique, but the name "),
                code(member_name),
                text(" has already been used for a "),
                friendly::hardcoded_string_desc_ref("previous member", loc_of_aloc(prev_use_loc)),
                text(" of "),
                ref_(enum_reason),
                text("."),
            ]),
            MessageEnumInconsistentMemberValues { enum_reason } => friendly::Message(vec![
                ref_(enum_reason),
                text(
                    " has been specified with inconsistent member initializers. All members need to consistently either use no initializer, or have a literal (boolean, number, bigint, or string) initializer.",
                ),
            ]),
            MessageEnumInvalidMemberInitializer(box MessageEnumInvalidMemberInitializerData {
                member_name,
                explicit_type,
                enum_reason,
            }) => {
                use flow_parser::ast::statement::enum_declaration::ExplicitType;
                match explicit_type {
                    Some(ExplicitType::Symbol) => friendly::Message(vec![
                        text("Symbol enum members cannot be initialized. Use "),
                        code(&format!("{member_name},")),
                        text(" in "),
                        ref_(enum_reason),
                        text("."),
                    ]),
                    Some(t) => {
                        let type_str = t.as_str();
                        friendly::Message(vec![
                            text("The enum member initializer for "),
                            code(member_name),
                            text(" needs to be a "),
                            code(type_str),
                            text(" literal in "),
                            ref_(enum_reason),
                            text("."),
                        ])
                    }
                    None => friendly::Message(vec![
                        text("The enum member initializer for "),
                        code(member_name),
                        text(
                            " needs to be a literal (either a boolean, number, bigint, or string) in ",
                        ),
                        ref_(enum_reason),
                        text("."),
                    ]),
                }
            }
            MessageEnumBooleanMemberNotInitialized {
                member_name,
                enum_reason,
            } => friendly::Message(vec![
                text("The enum member "),
                code(member_name),
                text(" of boolean "),
                ref_(enum_reason),
                text(
                    " has been left uninitialized. Boolean enum members need to be initialized, e.g. ",
                ),
                code(&format!("{member_name} = true,")),
                text("."),
            ]),
            MessageEnumNumberMemberNotInitialized {
                member_name,
                enum_reason,
            } => friendly::Message(vec![
                text("The enum member "),
                code(member_name),
                text(" of number "),
                ref_(enum_reason),
                text(
                    " has been left uninitialized. Number enum members need to be initialized, e.g. ",
                ),
                code(&format!("{member_name} = 1,")),
                text("."),
            ]),
            MessageEnumBigIntMemberNotInitialized {
                member_name,
                enum_reason,
            } => friendly::Message(vec![
                text("The enum member "),
                code(member_name),
                text(" of bigint "),
                ref_(enum_reason),
                text(
                    " has been left uninitialized. BigInt enum members need to be initialized, e.g. ",
                ),
                code(&format!("{member_name} = 1n,")),
                text("."),
            ]),
            MessageEnumStringMemberInconsistentlyInitialized { enum_reason } => {
                friendly::Message(vec![
                    text("String "),
                    ref_(enum_reason),
                    text(
                        " has been specified with inconsistent member initializers. Either all members need a string literal initializer, or none.",
                    ),
                ])
            }
            MessageExponentialSpread(box MessageExponentialSpreadData {
                reason,
                reasons_for_operand1,
                reasons_for_operand2,
            }) => {
                use super::intermediate_error_types::ExponentialSpreadReasonGroup;
                let format_reason_group =
                    |ExponentialSpreadReasonGroup {
                         first_reason,
                         second_reason,
                     }: &ExponentialSpreadReasonGroup<L>|
                     -> Vec<friendly::MessageFeature<Loc>> {
                        match second_reason {
                            None => vec![ref_(first_reason)],
                            Some(second_reason) => vec![
                                text("inferred union from "),
                                ref_(first_reason),
                                text(" | "),
                                ref_(second_reason),
                            ],
                        }
                    };
                let mut union_refs = format_reason_group(reasons_for_operand1);
                union_refs.push(text(" and "));
                union_refs.extend(format_reason_group(reasons_for_operand2));
                let mut features = vec![
                    text("Computing "),
                    ref_(reason),
                    text(
                        " may lead to an exponentially large number of cases to reason about because ",
                    ),
                ];
                features.extend(union_refs);
                features.push(text(" are both unions. Please use at most one union type per spread to simplify reasoning about the spread result."));
                features.push(text(" You may be able to get rid of a union by specifying a more general type that captures all of the branches of the union."));
                friendly::Message(features)
            }
            MessageExportValueAsType(name) => friendly::Message(vec![
                text("Cannot export the value "),
                code(name.as_str()),
                text(" as a type."),
            ]),
            MessageImplicitInexactObject => friendly::Message(vec![
                text("Please add "),
                code("..."),
                text(" to the end of the list of "),
                text("properties to express an inexact object type."),
            ]),
            MessageImportTypeAsTypeof(export_name) => {
                let (prefix, export) = msg_export("the type ", export_name);
                let mut features = vec![text("Cannot import ")];
                features.push(prefix);
                features.push(export);
                features.extend(vec![
                    text(" as a type. "),
                    code("import typeof"),
                    text(" only works on value exports like variables, "),
                    text("functions, and classes. If you intended to import a type use "),
                    code("import type"),
                    text(" instead."),
                ]);
                friendly::Message(features)
            }
            MessageImportTypeAsValue(export_name) => {
                let (prefix, export) = msg_export("the type ", export_name);
                let mut features = vec![text("Cannot import ")];
                features.push(prefix);
                features.push(export);
                features.extend(vec![
                    text(" as a value. "),
                    text("Use "),
                    code("import type"),
                    text(" instead."),
                ]);
                friendly::Message(features)
            }
            MessageImportValueAsType(export_name) => {
                let (prefix, export) = msg_export("the value ", export_name);
                let mut features = vec![text("Cannot import ")];
                features.push(prefix);
                features.push(export);
                features.extend(vec![
                    text(" as a type. "),
                    code("import type"),
                    text(" only works on type exports like type aliases, "),
                    text("interfaces, and classes. If you intended to import the type of a "),
                    text("value use "),
                    code("import typeof"),
                    text(" instead."),
                ]);
                friendly::Message(features)
            }
            MessageIncompatibleArity {
                lower,
                lower_arity,
                upper,
                upper_arity,
            } => friendly::Message(vec![
                text("arity "),
                text(&lower_arity.to_string()),
                text(" of "),
                ref_(lower),
                text(" is incompatible with arity "),
                text(&upper_arity.to_string()),
                text(" of "),
                ref_(upper),
            ]),
            MessageIncompatibleGeneralWithPrintedTypes(
                box MessageIncompatibleGeneralWithPrintedTypesData {
                    lower_loc,
                    upper_loc,
                    lower_desc,
                    upper_desc,
                },
            ) => friendly::Message(vec![
                ref_of_ty_or_desc(lower_loc, lower_desc),
                text(" is incompatible with "),
                ref_of_ty_or_desc(upper_loc, upper_desc),
            ]),
            MessageIncompatibleMappedTypeKey {
                source_type,
                mapped_type,
            } => friendly::Message(vec![
                ref_(source_type),
                text(" is incompatible with "),
                code("string | number | symbol"),
                text(", so it cannot be used to generate keys for "),
                ref_(mapped_type),
            ]),
            MessageIncompatibleTupleArity(box MessageIncompatibleTupleArityData {
                lower_reason,
                lower_arity,
                lower_inexact,
                upper_reason,
                upper_arity,
                upper_inexact,
                ..
            }) => {
                let str_of_arity = |inexact: bool, (num_req, num_total): &(i32, i32)| {
                    let suffix = if inexact {
                        "or more elements (inexact tuple)"
                    } else {
                        "elements"
                    };
                    if num_req == num_total {
                        if *num_total == 1 && !inexact {
                            format!("{} element", num_total)
                        } else {
                            format!("{} {}", num_total, suffix)
                        }
                    } else {
                        format!("{}-{} {}", num_req, num_total, suffix)
                    }
                };
                friendly::Message(vec![
                    ref_(lower_reason),
                    text(" has "),
                    text(&str_of_arity(*lower_inexact, lower_arity)),
                    text(" but "),
                    ref_(upper_reason),
                    text(" has "),
                    text(&str_of_arity(*upper_inexact, upper_arity)),
                ])
            }
            MessageIncompatibleClassToObject {
                reason_class,
                reason_obj,
                kind,
            } => {
                let kind_str = match kind {
                    super::intermediate_error_types::ClassKind::Class => "Class instances",
                    super::intermediate_error_types::ClassKind::Record => "Records",
                };
                friendly::Message(vec![
                    ref_(reason_class),
                    text(" is not a subtype of "),
                    ref_(reason_obj),
                    text(". "),
                    text(kind_str),
                    text(" are not subtypes of object types; consider rewriting "),
                    ref_(reason_obj),
                    text(" as an interface"),
                ])
            }
            MessageIncompatibleNonLiteralArrayToTuple { lower, upper } => friendly::Message(vec![
                ref_(lower),
                text(" has an unknown number of elements, so is "),
                text("incompatible with "),
                ref_(upper),
            ]),
            MessageIncompatibleNonTypeGuardToTypeGuard { lower, upper } => friendly::Message(vec![
                ref_(lower),
                text(", a non-type-guard function, is incompatible with "),
                ref_(upper),
                text(", which is a type-guard function"),
            ]),
            MessageIncompatibleReactDeepReadOnly(
                box MessageIncompatibleReactDeepReadOnlyData { lower, upper, .. },
            ) => friendly::Message(vec![
                ref_(lower),
                text(" is managed by the React runtime and cannot be mutated, while "),
                ref_(upper),
                text(" may allow mutations (possibly in nested values)"),
            ]),
            MessageIncompatibleReactHooksDueToUniqueness { lower, upper } => {
                friendly::Message(vec![
                    ref_(lower),
                    text(" and "),
                    ref_(upper),
                    text(" are different React hooks"),
                ])
            }
            MessageIncompatibleWithIndexed { lower, upper } => friendly::Message(vec![
                ref_(lower),
                text(" is incompatible with indexed "),
                ref_(upper),
            ]),
            MessageIncompleteExhausiveCheckEnum(box MessageIncompleteExhausiveCheckEnumData {
                description,
                enum_reason,
                left_to_check,
                default_case_loc,
            }) => {
                let left_to_check_features: Vec<friendly::MessageFeature<Loc>> =
                    if left_to_check.len() == 1 {
                        vec![
                            text("the member "),
                            code(left_to_check[0].as_str()),
                            text(" of enum "),
                            ref_(enum_reason),
                            text(" has"),
                        ]
                    } else {
                        let number_to_check = left_to_check.len();
                        let members_features: Vec<friendly::MessageFeature<Loc>> =
                            if number_to_check > 5 {
                                let max_display_amount = 4;
                                let mut features: Vec<friendly::MessageFeature<Loc>> =
                                    left_to_check
                                        .iter()
                                        .take(max_display_amount)
                                        .flat_map(|member| vec![code(member.as_str()), text(", ")])
                                        .collect();
                                features.push(text(&format!(
                                    "and {} others",
                                    number_to_check - max_display_amount
                                )));
                                features
                            } else {
                                let member_msgs: Vec<friendly::Message<Loc>> = left_to_check
                                    .iter()
                                    .map(|member| friendly::Message(vec![code(member.as_str())]))
                                    .collect();
                                friendly::conjunction_concat(member_msgs, "and", None).0
                            };
                        let mut features = vec![text("the members ")];
                        features.extend(members_features);
                        features.extend(vec![text(" of enum "), ref_(enum_reason), text(" have")]);
                        features
                    };
                let default_features: Vec<friendly::MessageFeature<Loc>> = match default_case_loc {
                    Some(default_reason) => {
                        use flow_lint_settings::lints::LintKind;
                        vec![
                            text(" The "),
                            friendly::hardcoded_string_desc_ref(
                                "default case",
                                loc_of_aloc(default_reason),
                            ),
                            text(" does not check for the missing members as the "),
                            code(LintKind::RequireExplicitEnumSwitchCases.as_str()),
                            text(" lint has been enabled."),
                        ]
                    }
                    None => vec![],
                };
                let mut features = vec![text("Incomplete exhaustive check: ")];
                features.extend(left_to_check_features);
                features.extend(vec![
                    text(" not been considered in check of "),
                    friendly::desc_of_reason_desc(description),
                    text("."),
                ]);
                features.extend(default_features);
                friendly::Message(features)
            }
            MessageIncorrectType(kind) => {
                let incorrect_name = kind.incorrect_of_kind();
                let replacement_name = kind.replacement_of_kind();
                match kind.error_type_of_kind() {
                    super::intermediate_error_types::IncorrectTypeErrorType::DeprecatedUtility => {
                        let mut features = vec![
                            text("The utility type "),
                            code(incorrect_name),
                            text(" is deprecated, use "),
                            code(replacement_name),
                            text(" instead."),
                        ];
                        if matches!(
                            *kind,
                            super::intermediate_error_types::IncorrectType::DollarKeys
                        ) {
                            features
                                .push(text(" For example, `type KeyType = keyof MyObjectType;` "));
                        }
                        friendly::Message(features)
                    }
                    super::intermediate_error_types::IncorrectTypeErrorType::TSType => {
                        friendly::Message(vec![
                            text("The equivalent of TypeScript's "),
                            code(incorrect_name),
                            text(" type in Flow is "),
                            code(replacement_name),
                            text("."),
                        ])
                    }
                }
            }
            MessageInternalType(kind) => {
                use super::intermediate_error_types::InternalType;
                match kind {
                    InternalType::DollarReactDeepReadOnly => friendly::Message(vec![
                        code("$ReactDeepReadOnly"),
                        text(" is a secret internal Flow type exposed for testing purposes. "),
                        text("There will be no stability guarantees."),
                    ]),
                    InternalType::DollarUtilityTypeWithNonDollarAliases(name) => {
                        friendly::Message(vec![
                            code(&format!("${}", name)),
                            text(" is an internal Flow type. Use "),
                            code(name),
                            text(" instead."),
                        ])
                    }
                    InternalType::ReactDollarUtilityTypesWithNonDollarAliases(name) => {
                        friendly::Message(vec![
                            code(&format!("React${}", name)),
                            text(" is an internal Flow type. Use "),
                            code(&format!("React.{}", name)),
                            text(" instead."),
                        ])
                    }
                }
            }
            MessageInvalidCatchParameterAnnotation { ts_utility_syntax } => {
                let type_name = if *ts_utility_syntax {
                    "unknown"
                } else {
                    "mixed"
                };
                friendly::Message(vec![
                    text("Invalid catch parameter type annotation. "),
                    text("Annotation must be "),
                    code("any"),
                    text(" or "),
                    code(type_name),
                    text(" if specified."),
                ])
            }
            MessageInvalidComponentRestParam => friendly::Message(vec![text(
                "You may only use an identifier or a destructured object as a component rest param.",
            )]),
            MessageInvalidGenericRef(typename) => friendly::Message(vec![
                text("Cannot compare the result of "),
                code("typeof"),
                text(" to string "),
                text("literal "),
                code(typename),
                text(" because it is not a valid "),
                code("typeof"),
                text(" return value."),
            ]),
            MessageInvalidEnumMemberCheck(box MessageInvalidEnumMemberCheckData {
                enum_reason,
                example_member,
                from_match,
            }) => {
                use super::error_message::enum_name_of_reason;
                let suggestion = match enum_name_of_reason(enum_reason) {
                    Some(enum_name) => {
                        let example_member =
                            example_member.as_ref().map(|s| s.as_str()).unwrap_or("A");
                        let (prefix, suffix) = if *from_match {
                            ("", "")
                        } else {
                            ("case ", ":")
                        };
                        vec![
                            text(" "),
                            text("For example "),
                            code(&format!(
                                "{}{}.{}{}",
                                prefix, enum_name, example_member, suffix
                            )),
                            text("."),
                        ]
                    }
                    None => vec![],
                };
                let at = if *from_match { "match pattern" } else { "case" };
                let mut features = vec![
                    text(&format!("Invalid enum member check at {}. ", at)),
                    text("The format must be dot-access of a member of "),
                    ref_(enum_reason),
                    text("."),
                ];
                features.extend(suggestion);
                friendly::Message(features)
            }
            MessageInvalidGraphQL(kind) => {
                use flow_parser_utils::graphql::GraphqlError;
                match kind {
                    GraphqlError::InvalidTaggedTemplate => friendly::Message(vec![text(
                        "Template literal substitutions are not allowed in GraphQL literals.",
                    )]),
                    GraphqlError::InvalidGraphQL => friendly::Message(vec![text(
                        "Expected a GraphQL fragment, query, mutation, or subscription.",
                    )]),
                }
            }
            MessageInvalidHookNaming => friendly::Message(vec![
                text("Hooks must have names that begin with "),
                code("use"),
                text(". "),
                text("(https://react.dev/reference/rules/rules-of-hooks)"),
            ]),
            MessageInvalidImportStarUse(import_star_reason) => friendly::Message(vec![
                text("The default export of a module cannot be accessed from an "),
                ref_(import_star_reason),
                text(" object. To use the default export you must import it directly."),
            ]),
            MessageInvalidMappedTypeInInterfaceOrDeclaredClass => friendly::Message(vec![text(
                "Mapped Types are not supported in interfaces or declared classes.",
            )]),
            MessageInvalidMappedTypeWithExactOrInexact => friendly::Message(vec![
                text(
                    "Mapped Types take on the exactness of the argument passed to keyof. They do not ",
                ),
                text("support explicit exact or inexact syntax."),
            ]),
            MessageInvalidMappedTypeWithExtraProps => friendly::Message(vec![text(
                "Mapped Types cannot be used when other properties or indexers are present.",
            )]),
            MessageInvalidMappedTypeWithOptionalityRemoval => friendly::Message(vec![text(
                "Mapped Types do not yet support optionality removal.",
            )]),
            MessageInvalidMappedTypeWithVarianceOnArrayInput => friendly::Message(vec![text(
                "Mapped Types do not yet support variance annotations on array inputs.",
            )]),
            MessageInvalidInferType => friendly::Message(vec![
                text("Invalid infer type declaration. "),
                code("infer"),
                text(" declarations are only permitted in the "),
                code("extends"),
                text(" clause of a conditional type."),
            ]),
            MessageInvalidLintSettings(kind) => {
                use flow_lint_settings::lint_settings::LintParseError;
                match kind {
                    LintParseError::RedundantArgument => friendly::Message(vec![text(
                        "Redundant argument. This argument doesn't change any lint settings.",
                    )]),
                    LintParseError::OverwrittenArgument => friendly::Message(vec![
                        text("Redundant argument. The values set by this argument are "),
                        text("overwritten later in this comment."),
                    ]),
                    LintParseError::NakedComment => friendly::Message(vec![text(
                        "Malformed lint rule. At least one argument is required.",
                    )]),
                    LintParseError::NonexistentRule => friendly::Message(vec![
                        text("Nonexistent/misspelled lint rule. Perhaps you have a "),
                        text("missing/extra "),
                        code(","),
                        text("?"),
                    ]),
                    LintParseError::InvalidSetting => friendly::Message(vec![text(
                        "Invalid setting. Valid settings are error, warn, and off.",
                    )]),
                    LintParseError::MalformedArgument => friendly::Message(vec![
                        text("Malformed lint rule. Properly formed rules contain a single "),
                        code(":"),
                        text(" character. Perhaps you have a missing/extra "),
                        code(","),
                        text("?"),
                    ]),
                }
            }
            MessageInvalidReactCreateElement(invalid_react) => friendly::Message(vec![
                text("Cannot create react element because the "),
                code("createElement"),
                text(" property of "),
                ref_(invalid_react),
                text(" is incompatible with builtin "),
                code("React.createElement"),
                text(" type. "),
                text("Please check the "),
                ref_(invalid_react),
                text(" identifier in scope to ensure it is the right one."),
            ]),
            MessageInvalidRefPropertyInSpread(box MessageInvalidRefPropertyInSpreadData {
                ref_loc,
                spread_loc,
            }) => friendly::Message(vec![
                text("Components do not support "),
                friendly::hardcoded_string_desc_ref("ref properties", loc_of_aloc(ref_loc)),
                text(" within "),
                friendly::hardcoded_string_desc_ref("spreads", loc_of_aloc(spread_loc)),
            ]),
            MessageInvalidKeyPropertyInSpread(box MessageInvalidKeyPropertyInSpreadData {
                key_loc,
                spread_loc,
            }) => friendly::Message(vec![
                text("Cannot "),
                friendly::hardcoded_string_desc_ref("spread", loc_of_aloc(spread_loc)),
                text(" an object that contains a "),
                friendly::hardcoded_string_desc_ref("`key` property", loc_of_aloc(key_loc)),
            ]),
            MessageInvalidSelfReferencingTypeAnnotation(
                box MessageInvalidSelfReferencingTypeAnnotationData { name, loc },
            ) => friendly::Message(vec![
                text("Invalid type annotation for "),
                code(name.as_str()),
                text(". It contains a "),
                friendly::hardcoded_string_desc_ref("reference", loc_of_aloc(loc)),
                text(" to the binding being declared."),
            ]),
            MessageInvalidSelfReferencingDefault(
                box MessageInvalidSelfReferencingDefaultData {
                    name,
                    def_loc,
                    ref_loc,
                },
            ) => friendly::Message(vec![
                text("Invalid default expression for parameter "),
                friendly::hardcoded_string_desc_ref(name.as_str(), loc_of_aloc(def_loc)),
                text(". It contains a "),
                friendly::hardcoded_string_desc_ref("reference", loc_of_aloc(ref_loc)),
                text(" to the binding being declared."),
            ]),
            MessageInvalidTrivialRecursiveDefinition(description) => friendly::Message(vec![
                text("Invalid trivially recursive definition of "),
                friendly::desc_of_reason_desc(description),
                text(". "),
            ]),
            MessageInvalidTupleRequiredAfterOptional {
                reason_tuple,
                reason_required,
                reason_optional,
            } => friendly::Message(vec![
                text("Invalid "),
                ref_(reason_tuple),
                text(", required "),
                ref_(reason_required),
                text(" must be after optional "),
                ref_(reason_optional),
                text("."),
            ]),
            MessageInvalidTupleTypeSpread(reason_arg) => friendly::Message(vec![
                text("Cannot spread non-tuple ("),
                ref_(reason_arg),
                text(") into tuple type."),
            ]),
            MessageTupleElementAfterInexactSpread => friendly::Message(vec![text(
                "Cannot have element after spread of inexact tuple.",
            )]),
            MessageInvalidRendersTypeArgument(box MessageInvalidRendersTypeArgumentData {
                renders_variant,
                invalid_render_type_kind,
                invalid_type_reasons,
            }) => {
                use flow_parser::ast::types::RendersVariant;

                use super::intermediate_error_types::InvalidRenderTypeKind;
                let additional_explanation: Vec<friendly::MessageFeature<Loc>> = match (
                    invalid_render_type_kind,
                    renders_variant,
                ) {
                    (InvalidRenderTypeKind::InvalidRendersStructural(r), _) => vec![
                        text(" You can only use an element of "),
                        code("AbstractComponent"),
                        text(" when the third type argument is a render type and "),
                        ref_(r),
                        text(" is not a render type."),
                    ],
                    (InvalidRenderTypeKind::InvalidRendersNonNominalElement(r), _) => vec![
                        text(
                            " Only elements of a component-syntax components can appear in renders but ",
                        ),
                        ref_(r),
                        text(" is not a component-syntax component."),
                    ],
                    (InvalidRenderTypeKind::InvalidRendersNullVoidFalse, RendersVariant::Maybe) => {
                        vec![
                            text(
                                " Only elements of a component-syntax components can appear in renders. ",
                            ),
                            code("renders?"),
                            text(" already includes React nodes that render nothing."),
                        ]
                    }
                    (InvalidRenderTypeKind::InvalidRendersNullVoidFalse, RendersVariant::Star) => {
                        vec![
                            text(
                                " Only elements of a component-syntax components can appear in renders. ",
                            ),
                            code("renders*"),
                            text(" already includes React nodes that render nothing."),
                        ]
                    }
                    (InvalidRenderTypeKind::InvalidRendersIterable, RendersVariant::Star) => {
                        vec![
                            text(
                                " Only elements of a component-syntax components can appear in renders. ",
                            ),
                            code("renders*"),
                            text(
                                " already models rendering any amount of children in all possible nesting structures.",
                            ),
                        ]
                    }
                    (InvalidRenderTypeKind::InvalidRendersNullVoidFalse, _) => vec![
                        text(
                            " Only elements of a component-syntax components can appear in renders. ",
                        ),
                        text(
                            "If you want to express the idea of rendering zero or one item, please use ",
                        ),
                        code("renders?"),
                        text(" instead."),
                    ],
                    (InvalidRenderTypeKind::InvalidRendersIterable, _) => vec![
                        text(
                            " Only elements of a component-syntax components can appear in renders. ",
                        ),
                        text(
                            "If you want to express the idea of rendering zero or more items, please use ",
                        ),
                        code("renders*"),
                        text(" instead."),
                    ],
                    (InvalidRenderTypeKind::InvalidRendersGenericT, RendersVariant::Star) => {
                        vec![
                            text(" Generic render types are not allowed with "),
                            code("renders*"),
                            text("."),
                        ]
                    }
                    (InvalidRenderTypeKind::InvalidRendersGenericT, _) => vec![
                        text(" Generic render types are only allowed in rendering declaration of "),
                        text(
                            "component syntax components, and only one or a union of generic types are permitted.",
                        ),
                    ],
                    (InvalidRenderTypeKind::UncategorizedInvalidRenders, _) => vec![],
                };
                let refs =
                    |reasons: &Vec1<VirtualReason<L>>| -> Vec<friendly::MessageFeature<Loc>> {
                        let len = reasons.len();
                        let mut result = Vec::new();
                        for (i, r) in reasons.iter().enumerate() {
                            if i > 0 {
                                if i == len - 1 {
                                    result.push(text(" and "));
                                } else {
                                    result.push(text(", "));
                                }
                            }
                            result.push(ref_(r));
                        }
                        result
                    };
                let mut features = vec![text("Cannot use ")];
                features.extend(refs(invalid_type_reasons));
                features.push(text(" as the type argument of renders type."));
                features.extend(additional_explanation);
                friendly::Message(features)
            }
            MessageInvalidTypeCastingSyntax(enabled_casting_syntax) => {
                use super::error_message::type_casting_examples;
                let (valid, invalid) = type_casting_examples(*enabled_casting_syntax);
                friendly::Message(vec![
                    text("Invalid type cast syntax. Use the form "),
                    code(valid),
                    text(" instead of the form "),
                    code(invalid),
                    text("."),
                ])
            }
            MessageInvalidTypeGuardFunctionKind(kind) => friendly::Message(vec![
                text("Cannot declare a type guard on a(n) "),
                text(kind),
                text(" function."),
            ]),
            MessageInvalidTypeGuardFunctionWritten {
                type_guard_reason,
                write_locs,
            } => {
                let loc_str: Vec<friendly::MessageFeature<Loc>> = match write_locs.as_slice() {
                    [] => vec![text("in this function")],
                    [loc] => vec![text("in"), friendly::no_desc_ref(&loc_of_aloc(loc))],
                    _ => {
                        let mut v = vec![text("in the following statements:")];
                        for loc in write_locs {
                            v.push(friendly::no_desc_ref(&loc_of_aloc(loc)));
                        }
                        v
                    }
                };
                let mut features = vec![
                    text("Cannot use "),
                    ref_(type_guard_reason),
                    text(" because at this return point it is written to "),
                ];
                features.extend(loc_str);
                features.push(text("."));
                friendly::Message(features)
            }
            MessageInvalidTypeGuardParamUnbound(reason) => friendly::Message(vec![
                text("Cannot find "),
                ref_(reason),
                text(" in the parameters of this function (type)."),
            ]),
            MessageInvalidTypeGuardThisParam(reason) => friendly::Message(vec![
                text("Cannot use "),
                ref_(reason),
                text(" as a type guard variable in this context. "),
                code("this"),
                text(
                    " type guards are only supported in non-static declare class or interface methods.",
                ),
            ]),
            MessageInvalidUseOfFlowEnforceOptimized(arg) => friendly::Message(vec![
                text("Invalid use of $Flow$EnforceOptimized on non-union type "),
                ref_(arg),
                text("."),
            ]),
            MessageMissingAnnotation(d) => friendly::Message(vec![
                text("Missing an annotation on "),
                friendly::desc_of_reason_desc(d),
                text("."),
            ]),
            MessageMissingAnnotationDueToContextualTypingFailure(d) => friendly::Message(vec![
                text("An annotation on "),
                friendly::desc_of_reason_desc(d),
                text(" is required because Flow cannot infer its type from local context."),
            ]),
            MessageMissingAnnotationForGenericFunction(d) => friendly::Message(vec![
                text("Missing an annotation on "),
                friendly::desc_of_reason_desc(d),
                text(" because generic functions must be fully annotated."),
            ]),
            MessageLowerIsNotArray(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not an array")])
            }
            MessageLowerIsNotArrayIndex(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not an array index")])
            }
            MessageLowerIsNotClass(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not a class")])
            }
            MessageLowerIsNotClassWithPrivateProps(lower) => friendly::Message(vec![
                ref_(lower),
                text(" is not a class with private properties"),
            ]),
            MessageLowerIsNotFunction(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not a function")])
            }
            MessageLowerIsNotFunctionType(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not a function type")])
            }
            MessageLowerIsNotInheritable(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not inheritable")])
            }
            MessageLowerIsNotInstanceType(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not an instance type")])
            }
            MessageLowerIsNotObject(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not an object")])
            }
            MessageLowerIsNotPolymorphicType(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not a polymorphic type")])
            }
            MessageLowerIsNotReactComponent(lower) => {
                friendly::Message(vec![ref_(lower), text(" is not a React component")])
            }
            MessageLowerIsNotSupportedByUnclassifiedUse { lower, ctor } => friendly::Message(vec![
                ref_(lower),
                text(" is not supported by unclassified use "),
                text(ctor),
            ]),
            MessageMethodUnbinding {
                reason_op,
                context_loc,
            } => friendly::Message(vec![
                ref_(reason_op),
                text(" cannot be unbound from the "),
                friendly::hardcoded_string_desc_ref("context", loc_of_aloc(context_loc)),
                text(" where it was defined"),
            ]),
            MessageMissingPlatformSupport { missing_platforms } => {
                let platforms: Vec<_> = missing_platforms.iter().cloned().collect();
                let platform_features =
                    |platforms: &[FlowSmolStr]| -> Vec<friendly::MessageFeature<Loc>> {
                        match platforms {
                            [] => vec![text("no platforms")],
                            [p] => vec![text("the "), code(p.as_str()), text(" platform")],
                            [first, rest @ ..] => {
                                let mut result =
                                    vec![text("the following platforms: "), code(first.as_str())];
                                for p in rest {
                                    result.push(text(", "));
                                    result.push(code(p.as_str()));
                                }
                                result
                            }
                        }
                    };
                let mut features = vec![text("Support for ")];
                features.extend(platform_features(&platforms));
                features.push(text(" is missing."));
                friendly::Message(features)
            }
            MessageNoDefaultExport(box MessageNoDefaultExportData {
                module_name,
                suggestion,
            }) => {
                let mut features = vec![
                    text("Cannot import a default export because there is no default export "),
                    text("in "),
                    code(module_name),
                    text("."),
                ];
                if let Some(s) = suggestion {
                    features.extend(vec![
                        text(" "),
                        text("Did you mean "),
                        code(&format!("import {{{}}} from \"{}\"", s, module_name)),
                        text("?"),
                    ]);
                }
                friendly::Message(features)
            }
            MessageNoNamedExport(box MessageNoNamedExportData {
                module_name,
                export_name,
                suggestion,
            }) => {
                let mut features = vec![
                    text("Cannot import "),
                    code(export_name),
                    text(" because "),
                    text("there is no "),
                    code(export_name),
                    text(" export in "),
                    code(module_name),
                    text("."),
                ];
                if let Some(s) = suggestion {
                    features.extend(vec![text(" Did you mean "), code(s), text("?")]);
                }
                friendly::Message(features)
            }
            MessageNonConstVarExport(decl_reason) => {
                let reason_part = match decl_reason {
                    Some(reason) => vec![text("variable "), ref_(reason)],
                    None => vec![text("variable")],
                };
                let mut features = vec![text("Cannot export ")];
                features.extend(reason_part);
                features.extend(vec![
                    text(" declared using "),
                    code("var"),
                    text(" or "),
                    code("let"),
                    text(". All exported variables must be "),
                    code("const"),
                    text("."),
                ]);
                friendly::Message(features)
            }
            MessageNonStrictImport => friendly::Message(vec![
                text("Dependencies of a "),
                code("@flow strict"),
                text(" module must "),
                text("also be "),
                code("@flow strict"),
                text("!"),
            ]),
            MessageNonToplevelExport => {
                friendly::Message(vec![text("Exports can only appear at the top level")])
            }
            MessageOnlyDefaultExport(box MessageOnlyDefaultExportData {
                module_name,
                export_name,
            }) => friendly::Message(vec![
                text("Cannot import "),
                code(export_name),
                text(" because "),
                text("there is no "),
                code(export_name),
                text(" export in "),
                code(module_name),
                text(". Did you mean "),
                code(&format!("import {} from \"...\"", export_name)),
                text("?"),
            ]),
            MessagePlatformSpecificImplementationModuleLookupFailed(name) => {
                friendly::Message(vec![
                    text("Cannot resolve platform-specific implementation module "),
                    code(name),
                    text(". "),
                    text("All platform-specific implementations must exist for this interface. "),
                    text("Read the docs on Flow's multi-platform support for more information: "),
                    text("https://flow.org/en/docs/react/multiplatform"),
                ])
            }
            MessagePropMissing(box MessagePropMissingData {
                lower,
                upper,
                prop,
                suggestion,
                reason_indexer,
            }) => {
                use super::error_message::mk_prop_message;
                // If we were subtyping that add to the error message so our user knows what
                // object required the missing property.
                let prop_message = mk_prop_message(prop.as_deref());
                let indexer_message: Vec<friendly::MessageFeature<Loc>> = match reason_indexer {
                    None => vec![],
                    Some(indexer) => vec![
                        text(". Any property that does not exist in "),
                        ref_(lower),
                        text(" must be compatible with its indexer "),
                        ref_(indexer),
                    ],
                };
                let suggestion: Vec<friendly::MessageFeature<Loc>> = match suggestion {
                    Some(s) => vec![text(" (did you mean "), code(s), text("?)")],
                    None => vec![],
                };
                match upper {
                    Some(upper) => {
                        let mut features = prop_message;
                        features.extend(suggestion);
                        features.extend(vec![text(" is missing in "), ref_(lower)]);
                        features.extend(vec![text(" but exists in ")]);
                        features.push(ref_(upper));
                        features.extend(indexer_message);
                        friendly::Message(features)
                    }
                    None => {
                        if prop.is_none() && is_nullish_reason(lower) {
                            friendly::Message(vec![ref_(lower), text(" does not have properties")])
                        } else {
                            let mut features = prop_message;
                            features.extend(suggestion);
                            features.extend(vec![text(" is missing in "), ref_(lower)]);
                            friendly::Message(features)
                        }
                    }
                }
            }
            MessagePropsMissing(box MessagePropsMissingData {
                lower,
                upper,
                props,
            }) => {
                let (first_prop, rest_props) = props.clone().split_off_first();
                if rest_props.is_empty() {
                    friendly::Message(vec![
                        text("property "),
                        code(&first_prop),
                        text(" is missing in "),
                        ref_(lower),
                        text(" but exists in "),
                        ref_(upper),
                    ])
                } else {
                    let all_props: Vec<_> = std::iter::once(first_prop).chain(rest_props).collect();
                    let max_props = 10;
                    let num_props = all_props.len();
                    let (displayed, truncated_count) = if num_props > max_props {
                        (&all_props[..max_props], num_props - max_props)
                    } else {
                        (&all_props[..], 0)
                    };
                    let mut features = vec![text("properties ")];
                    let prop_msgs: Vec<_> = displayed
                        .iter()
                        .map(|p| friendly::Message(vec![code(p.as_str())]))
                        .collect();
                    let friendly::Message(concat_result) =
                        friendly::conjunction_concat(prop_msgs, "and", None);
                    features.extend(concat_result);
                    if truncated_count > 0 {
                        features.push(text(&format!(", ... ({} more)", truncated_count)));
                    }
                    features.extend(vec![
                        text(" are missing in "),
                        ref_(lower),
                        text(" but exist in "),
                        ref_(upper),
                    ]);
                    friendly::Message(features)
                }
            }
            MessagePropPolarityMismatch(box MessagePropPolarityMismatchData {
                lower,
                upper,
                props,
            }) => {
                use super::error_message::mk_prop_message;
                use super::error_message::polarity_explanation;
                let (first_prop, rest_props) = props.clone().split_off_first();
                let all_props: Vec<_> = std::iter::once(first_prop).chain(rest_props).collect();
                let mut sorted_props = all_props.clone();
                sorted_props.sort_by(|(a, _, _), (b, _, _)| a.cmp(b));
                let number_to_check = sorted_props.len();
                let props_msgs: Vec<friendly::Message<Loc>> = if number_to_check > 5 {
                    let max_display = 4;
                    let cropped: Vec<_> = sorted_props.into_iter().take(max_display).collect();
                    let mut msgs: Vec<_> = cropped
                        .into_iter()
                        .map(|(prop, lpole, upole)| {
                            let expected = polarity_explanation(lpole, upole);
                            let actual = polarity_explanation(upole, lpole);
                            let mut f = mk_prop_message(prop.as_deref());
                            f.extend(vec![
                                text(" is "),
                                text(expected),
                                text(" in "),
                                ref_(lower),
                                text(" but "),
                                text(actual),
                                text(" in "),
                                ref_(upper),
                            ]);
                            friendly::Message(f)
                        })
                        .collect();
                    msgs.push(friendly::Message(vec![text(&format!(
                        "{} other properties are incompatible",
                        number_to_check - max_display
                    ))]));
                    msgs
                } else {
                    sorted_props
                        .into_iter()
                        .map(|(prop, lpole, upole)| {
                            let expected = polarity_explanation(lpole, upole);
                            let actual = polarity_explanation(upole, lpole);
                            let mut f = mk_prop_message(prop.as_deref());
                            f.extend(vec![
                                text(" is "),
                                text(expected),
                                text(" in "),
                                ref_(lower),
                                text(" but "),
                                text(actual),
                                text(" in "),
                                ref_(upper),
                            ]);
                            friendly::Message(f)
                        })
                        .collect()
                };
                friendly::conjunction_concat(props_msgs, "and", None)
            }
            MessagePropNotReadable(x) => {
                use super::error_message::mk_prop_message;
                let prop = x.as_ref().map(|n| n.as_str().to_string());
                let mut features = mk_prop_message(prop.as_deref());
                features.push(text(" is not readable"));
                friendly::Message(features)
            }
            MessagePropNotWritable(x) => {
                use super::error_message::mk_prop_message;
                let prop = x.as_ref().map(|n| n.as_str().to_string());
                let mut features = mk_prop_message(prop.as_deref());
                features.push(text(" is not writable"));
                friendly::Message(features)
            }
            MessageReactIntrinsicOverlap(box MessageReactIntrinsicOverlapData {
                use_,
                def,
                type_,
                mixed,
            }) => friendly::Message(vec![
                text("The name of intrinsic element "),
                ref_(use_),
                text(" overlaps with a "),
                friendly::hardcoded_string_desc_ref("local definition", loc_of_aloc(def)),
                text(" which has a "),
                friendly::hardcoded_string_desc_ref("type", loc_of_aloc(type_)),
                text(" that "),
                if *mixed { text("may") } else { text("can") },
                text(
                    " be instantiated as an element. To avoid confusion between this definition and the intrinsic, rename the definition",
                ),
            ]),
            MessageReadonlyArraysCannotBeWrittenTo => {
                friendly::Message(vec![text("read-only arrays cannot be written to")])
            }
            MessageRecursionLimitExceeded => {
                friendly::Message(vec![text("*** Recursion limit exceeded ***")])
            }
            MessageRedeclareComponentProp(box MessageRedeclareComponentPropData {
                duplicates,
                spread_loc,
            }) => {
                let ((first_loc, name, second_loc), rest) = duplicates.clone().split_off_first();
                if rest.is_empty() {
                    let first = mk_reason(
                        VirtualReasonDesc::RIdentifier(name.clone()),
                        first_loc.dupe(),
                    );
                    friendly::Message(vec![
                        text("Component property "),
                        ref_(&first),
                        text(" is "),
                        friendly::hardcoded_string_desc_ref(
                            "re-declared",
                            loc_of_aloc(&second_loc),
                        ),
                        text(" within a "),
                        friendly::hardcoded_string_desc_ref("spread", loc_of_aloc(spread_loc)),
                        text(
                            ". Property names may only be have one definition within a component.",
                        ),
                    ])
                } else {
                    let all_dupes: Vec<_> = std::iter::once((first_loc, name, second_loc))
                        .chain(rest)
                        .collect();
                    let mut features = vec![
                        text("Multiple component properties are re-declared within a "),
                        friendly::hardcoded_string_desc_ref("spread", loc_of_aloc(spread_loc)),
                        text(".\n"),
                    ];
                    for (first_loc, name, second_loc) in all_dupes {
                        let first =
                            mk_reason(VirtualReasonDesc::RIdentifier(name.clone()), first_loc);
                        features.extend(vec![
                            text(" - "),
                            ref_(&first),
                            text(" is re-declared "),
                            friendly::hardcoded_string_desc_ref("here", loc_of_aloc(&second_loc)),
                            text("\n"),
                        ]);
                    }
                    features.push(text(
                        "Property names may only be have one definition within a component.",
                    ));
                    friendly::Message(features)
                }
            }
            MessageShouldAnnotateVariableOnlyInitializedInGenericContext {
                reason,
                possible_generic_escape_locs,
            } => {
                let mut features = vec![
                    text("Variable "),
                    ref_(reason),
                    text(
                        " should be annotated, because it is only initialized in a generic context",
                    ),
                ];
                let locs: Vec<_> = possible_generic_escape_locs
                    .iter()
                    .map(|l| friendly::no_desc_ref(&loc_of_aloc(l)))
                    .collect();
                for (i, loc_ref) in locs.into_iter().enumerate() {
                    if i > 0 {
                        features.push(text(","));
                    }
                    features.push(loc_ref);
                }
                friendly::Message(features)
            }
            MessageShouldAnnotateVariableUsedInGenericContext(
                box MessageShouldAnnotateVariableUsedInGenericContextData {
                    reason,
                    null_loc,
                    initialized,
                    possible_generic_escape_locs,
                },
            ) => {
                let null_ref = if *initialized {
                    code("null")
                } else {
                    ref_(&mk_reason(
                        VirtualReasonDesc::RCode("null".into()),
                        null_loc.dupe(),
                    ))
                };
                let mut features = vec![
                    text("Variable "),
                    ref_(reason),
                    text(" should be annotated, because it is only ever assigned to by "),
                    null_ref,
                    text(" and in generic context"),
                ];
                let locs: Vec<_> = possible_generic_escape_locs
                    .iter()
                    .map(|l| friendly::no_desc_ref(&loc_of_aloc(l)))
                    .collect();
                for (i, loc_ref) in locs.into_iter().enumerate() {
                    if i > 0 {
                        features.push(text(","));
                    }
                    features.push(loc_ref);
                }
                friendly::Message(features)
            }
            MessageShouldNotBeCoerced(lower) => {
                friendly::Message(vec![ref_(lower), text(" should not be coerced")])
            }
            MessageShouldUseArrayLiteral => friendly::Message(vec![
                text("Use an array literal instead of "),
                code("new Array(...)"),
                text("."),
            ]),
            MessageSketchyNumber(reason) => friendly::Message(vec![
                text("Avoid using "),
                code("&&"),
                text(" to check the value of "),
                ref_(reason),
                text(". "),
                text(
                    "Consider handling falsy values (0 and NaN) by using a conditional to choose an ",
                ),
                text("explicit default instead."),
            ]),
            MessageSketchyNullCheck(box MessageSketchyNullCheckData {
                kind,
                falsy_loc,
                null_loc,
            }) => {
                use flow_lint_settings::lints::SketchyNullKind;
                let (type_str, value_str) = match kind {
                    SketchyNullKind::Bool => ("boolean", "false"),
                    SketchyNullKind::Number => ("number", "0"),
                    SketchyNullKind::BigInt => ("bigint", "0n"),
                    SketchyNullKind::String => ("string", "an empty string"),
                    SketchyNullKind::Mixed => ("mixed", "false"),
                    SketchyNullKind::EnumBool => ("boolean enum", "false at runtime"),
                    SketchyNullKind::EnumNumber => ("number enum", "0 at runtime"),
                    SketchyNullKind::EnumBigInt => ("bigint enum", "0n at runtime"),
                    SketchyNullKind::EnumString => ("string enum", "an empty string at runtime"),
                };
                friendly::Message(vec![
                    text("Sketchy null check on "),
                    friendly::hardcoded_string_desc_ref(type_str, loc_of_aloc(falsy_loc)),
                    text(" "),
                    text("which is potentially "),
                    text(value_str),
                    text(". Perhaps you meant to "),
                    text("check for "),
                    ref_(&mk_reason(VirtualReasonDesc::RNullOrVoid, null_loc.dupe())),
                    text("?"),
                ])
            }
            MessageSuppressionMalformedCode => friendly::Message(vec![
                text("Suppression contains a malformed error code. Suppressions with error codes "),
                text("should be formatted as "),
                code("$FlowFixMe[<CODE>]"),
                text("."),
            ]),
            MessageSuppressionMissingCode => friendly::Message(vec![
                text(
                    "Suppression is missing a code. Please update this suppression to use an error code: ",
                ),
                code("$FlowFixMe[<CODE>]"),
            ]),
            MessageThisInComponent(component_loc) => friendly::Message(vec![
                text("Cannot reference "),
                code("this"),
                text(" from within "),
                friendly::hardcoded_string_desc_ref(
                    "component declaration",
                    loc_of_aloc(component_loc),
                ),
            ]),
            MessageThisInExportedFunction => friendly::Message(vec![
                text("Cannot use "),
                code("this"),
                text(" in an exported function."),
            ]),
            MessageThisSuperInObject(reason, kind) => {
                use super::intermediate_error_types::ThisFinderKind;
                let (v, suggestion) = match kind {
                    ThisFinderKind::This => (
                        "this",
                        vec![
                            text(" Consider replacing the reference to "),
                            code("this"),
                            text(
                                " with the name of the object, or rewriting the object as a class.",
                            ),
                        ],
                    ),
                    ThisFinderKind::Super => (
                        "super",
                        vec![text(" Consider rewriting the object as a class.")],
                    ),
                };
                let mut features = vec![
                    text("Cannot reference "),
                    code(v),
                    text(" from within "),
                    ref_(reason),
                    text(". For safety, Flow restricts access to "),
                    code(v),
                    text(" inside object methods since these methods may be unbound and rebound."),
                ];
                features.extend(suggestion);
                friendly::Message(features)
            }
            MessageTSKeyofType => friendly::Message(vec![
                code("keyof"),
                text(" is only supported when used inline in a mapped type. "),
                text("The equivalent of TypeScript's "),
                code("keyof"),
                text(" type operator in Flow is the "),
                code("$Keys"),
                text(" utility type, used in the form "),
                code("$Keys<T>"),
                text("."),
            ]),
            MessageTSNeverType => friendly::Message(vec![
                text("The closest equivalent of TypeScript's "),
                code("never"),
                text(" type in Flow is "),
                code("empty"),
                text("."),
            ]),
            MessageTSParamExtends => friendly::Message(vec![
                text("While TypeScript uses "),
                code("extends"),
                text(" to specify type parameter bounds, Flow uses "),
                code(":"),
                text(" in the form "),
                code("type T<A: B> = ..."),
                text("."),
            ]),
            MessageTSReadonlyOperatorOnArray => friendly::Message(vec![
                text("The equivalent of TypeScript's "),
                code("readonly"),
                text(" type operator applied to an array type is "),
                code("ReadonlyArray<T>"),
                text("."),
            ]),
            MessageTSReadonlyOperatorOnTuple => friendly::Message(vec![
                text("The equivalent of TypeScript's "),
                code("readonly"),
                text(" type operator applied to a tuple type is "),
                code("Readonly<[T, S]>"),
                text("."),
            ]),
            MessageTSReadonlyType => friendly::Message(vec![
                text("TypeScript's "),
                code("readonly"),
                text(" type operator is not valid in Flow. "),
                text("For array types, you can use "),
                code("ReadonlyArray<T>"),
                text(". For object and tuple types you can use "),
                code("Readonly<T>"),
                text("."),
            ]),
            // | MessageTSClassAccessibility kind ->
            MessageTSClassAccessibility(kind) => {
                use flow_parser::ast::class::ts_accessibility::Kind;
                // let (modifier, suffix) = match kind with ...
                let (modifier, suffix) = match kind {
                    // | Flow_ast.Class.TSAccessibility.Private ->
                    Kind::Private => (
                        "private",
                        vec![
                            text("Use JavaScript private elements instead. To fix, change "),
                            code("private foo"),
                            text(" to "),
                            code("#foo"),
                            text("."),
                        ],
                    ),
                    // | Flow_ast.Class.TSAccessibility.Public ->
                    Kind::Public => (
                        "public",
                        vec![
                            text("Fields and methods are public by default. To fix, remove the "),
                            code("public"),
                            text(" modifier."),
                        ],
                    ),
                    // | Flow_ast.Class.TSAccessibility.Protected ->
                    Kind::Protected => (
                        "protected",
                        vec![
                            text("To fix, remove the "),
                            code("protected"),
                            text(" modifier."),
                        ],
                    ),
                };
                // [text "Flow does not support using "; code modifier; text " in classes. "] @ suffix
                let mut parts = vec![
                    text("Flow does not support using "),
                    code(modifier),
                    text(" in classes. "),
                ];
                parts.extend(suffix);
                friendly::Message(parts)
            }
            MessageTSParameterProperty => friendly::Message(vec![
                text("Flow does not support TypeScript parameter properties. "),
                text(
                    "To fix, declare the property in the class body and assign it in the constructor.",
                ),
            ]),
            // | MessageAbstractClass -> [text "Flow does not support "; code "abstract"; text " classes."]
            MessageAbstractClass => friendly::Message(vec![
                text("Flow does not support "),
                code("abstract"),
                text(" classes."),
            ]),
            // | MessageAbstractMethod -> [text "Flow does not support "; code "abstract"; text " methods."]
            MessageAbstractMethod => friendly::Message(vec![
                text("Flow does not support "),
                code("abstract"),
                text(" methods."),
            ]),
            MessageTSSatisfiesType(enabled_casting_syntax) => {
                use super::error_message::type_casting_examples;
                let (example, _) = type_casting_examples(*enabled_casting_syntax);
                friendly::Message(vec![
                    text("The closest equivalent of TypeScript's "),
                    code("satisfies"),
                    text(" expression in Flow is to do a cast in the form "),
                    code(example),
                    text("."),
                ])
            }
            MessageTSVarianceIn => friendly::Message(vec![
                text("The equivalent of TypeScript's "),
                code("in"),
                text(" variance annotation is "),
                code("-"),
                text(" in Flow."),
            ]),
            MessageTSVarianceInOut => friendly::Message(vec![
                text("The equivalent of TypeScript's "),
                code("in out"),
                text(" variance annotation in Flow is to simply leave it out - "),
                text("it's the default if you don't have a variance annotation."),
            ]),
            MessageTSVarianceOut => friendly::Message(vec![
                text("The equivalent of TypeScript's "),
                code("out"),
                text(" variance annotation is "),
                code("+"),
                text(" in Flow."),
            ]),
            MessageTSVarianceReadOnly => friendly::Message(vec![
                text("While TypeScript uses "),
                code("readonly"),
                text(" to specify read only properties, Flow uses "),
                code("+"),
                text(" in the form "),
                code("+foo: T"),
                text(" for class and object type properties, and "),
                code("+[string]: T"),
                text(" for dictionaries."),
            ]),
            MessageVarianceKeywordWriteonly => friendly::Message(vec![
                text("The "),
                code("writeonly"),
                text(" variance keyword is gated behind the "),
                code("experimental.allow_variance_keywords"),
                text(" flowconfig option."),
            ]),
            MessageTSUndefinedType => friendly::Message(vec![
                text("The equivalent of TypeScript's "),
                code("undefined"),
                text(" type in Flow is "),
                code("void"),
                text(". "),
                text("Flow does not have separate "),
                code("void"),
                text(" and "),
                code("undefined"),
                text(" types."),
            ]),
            MessageTSUnknownType => friendly::Message(vec![
                text("The equivalent of TypeScript's "),
                code("unknown"),
                text(" type in Flow is "),
                code("mixed"),
                text("."),
            ]),
            MessageUnclearType => friendly::Message(vec![
                text("Unclear type. Using "),
                code("any"),
                text(", "),
                code("Object"),
                text(", or "),
                code("Function"),
                text(" types is not safe!"),
            ]),
            MessageUnderconstrainedImplicitInstantiaton {
                reason_call,
                reason_tparam,
            } => friendly::Message(vec![
                ref_(reason_tparam),
                text(" is underconstrained by "),
                ref_(reason_call),
                text(
                    ". Either add explicit type arguments or cast the expression to your expected type",
                ),
            ]),
            MessageUndocumentedFeature => friendly::Message(vec![
                text("You are using an undocumented feature. "),
                text("It might be removed in the future, "),
                text("and the behavior might change at any time without warning."),
            ]),
            MessageIllegalAssertOperator { obj, specialized } => {
                let explanation = if *specialized {
                    vec![text(
                        "The assert operator can only be used with arrays and objects with indexers.",
                    )]
                } else {
                    vec![text(
                        "The assert operator can only be applied to values with nullable types.",
                    )]
                };
                let mut features = vec![
                    ref_(obj),
                    text(" is not a valid target of the nonnull assertion operator ("),
                    code("!"),
                    text("). "),
                ];
                features.extend(explanation);
                friendly::Message(features)
            }
            MessageTupleElementNotReadable(box MessageTupleElementNotReadableData {
                reason,
                index,
                name,
            }) => {
                let index_ref = friendly::MessageFeature::Reference(
                    vec![friendly::MessageInline::Code(format!("{}", index))],
                    loc_of_aloc(reason.def_loc()),
                );
                let label = match name {
                    Some(n) => vec![text(" labeled "), code(n)],
                    None => vec![],
                };
                let mut features = vec![text("tuple element at index "), index_ref];
                features.extend(label);
                features.push(text(" is not readable"));
                friendly::Message(features)
            }
            MessageTupleElementNotWritable(box MessageTupleElementNotWritableData {
                reason,
                index,
                name,
            }) => {
                let index_ref = friendly::MessageFeature::Reference(
                    vec![friendly::MessageInline::Code(format!("{}", index))],
                    loc_of_aloc(reason.def_loc()),
                );
                let label = match name {
                    Some(n) => vec![text(" labeled "), code(n)],
                    None => vec![],
                };
                let mut features = vec![text("tuple element at index "), index_ref];
                features.extend(label);
                features.push(text(" is not writable"));
                friendly::Message(features)
            }
            MessageTupleIndexOutOfBound(box MessageTupleIndexOutOfBoundData {
                reason_op,
                inexact,
                length,
                index,
            }) => {
                let plural = if *length == 1 { "" } else { "s" };
                let explicit = if *inexact { " explicitly defined" } else { "" };
                let bounds_msg = if *inexact {
                    "unknown or out of bounds"
                } else {
                    "out of bounds"
                };
                friendly::Message(vec![
                    ref_(reason_op),
                    text(&format!(
                        " only has {} element{}{}, so index {} is {}",
                        length, plural, explicit, index, bounds_msg
                    )),
                ])
            }
            MessageTupleNonIntegerIndex(box MessageTupleNonIntegerIndexData {
                index_def_loc,
                index,
            }) => friendly::Message(vec![
                text("the index into a tuple must be an integer, but "),
                friendly::MessageFeature::Reference(
                    vec![friendly::MessageInline::Code(index.to_string())],
                    loc_of_aloc(index_def_loc),
                ),
                text(" is not an integer"),
            ]),
            MessageTupleNonStaticallyKnownIndex => friendly::Message(vec![text(
                "the index must be statically known to write a tuple element",
            )]),
            MessageTuplePolarityMismatch {
                index,
                reason_lower,
                reason_upper,
                polarity_lower,
                polarity_upper,
            } => {
                use super::error_message::polarity_explanation;
                let expected = polarity_explanation(*polarity_lower, *polarity_upper);
                let actual = polarity_explanation(*polarity_upper, *polarity_lower);
                friendly::Message(vec![
                    text("tuple element at index "),
                    code(&format!("{}", index)),
                    text(" is "),
                    text(expected),
                    text(" in "),
                    ref_(reason_lower),
                    text(" but "),
                    text(actual),
                    text(" in "),
                    ref_(reason_upper),
                ])
            }
            MessageTypeGuardIndexMismatch { lower, upper } => friendly::Message(vec![
                ref_(lower),
                text(" does not appear in the same position as "),
                ref_(upper),
            ]),
            MessageTypeGuardImpliesMismatch { lower, upper } => friendly::Message(vec![
                text("one-sided "),
                ref_(lower),
                text(" is incompatible with default "),
                ref_(upper),
            ]),
            MessageNegativeTypeGuardConsistency {
                reason: _,
                return_reason,
                type_reason,
            } => friendly::Message(vec![
                text("Cannot return "),
                desc(return_reason),
                text(" because the negation of the predicate encoded in this expression"),
                text(" needs to completely refine away the guard type "),
                ref_(type_reason),
                text(". "),
                text("Consider using a one-sided type-guard (`implies x is T`). "),
                text("See 2. in "),
                text(
                    "https://flow.org/en/docs/types/type-guards/#toc-consistency-checks-of-type-guard-functions.",
                ),
            ]),
            MessageUnexpectedUseOfThisType => friendly::Message(vec![
                text("Unexpected use of "),
                code("this"),
                text(" type."),
            ]),
            MessageUninitializedInstanceProperty(kind) => {
                use flow_lint_settings::lints::PropertyAssignmentKind;
                match kind {
                    PropertyAssignmentKind::MethodCallBeforeEverythingInitialized => {
                        friendly::Message(vec![
                            text("It is unsafe to call a method in the constructor before all "),
                            text("class properties are definitely initialized."),
                        ])
                    }
                    PropertyAssignmentKind::PropertyFunctionCallBeforeEverythingInitialized => {
                        friendly::Message(vec![
                            text("It is unsafe to call a property function in the constructor "),
                            text("before all class properties are definitely initialized."),
                        ])
                    }
                    PropertyAssignmentKind::PropertyNotDefinitelyInitialized => {
                        friendly::Message(vec![
                            text("Class property not definitely initialized in the constructor. "),
                            text("Can you add an assignment to the property declaration?"),
                        ])
                    }
                    PropertyAssignmentKind::ReadFromUninitializedProperty => {
                        friendly::Message(vec![
                            text("It is unsafe to read from a class property before it is "),
                            text("definitely initialized."),
                        ])
                    }
                    PropertyAssignmentKind::ThisBeforeEverythingInitialized => {
                        friendly::Message(vec![
                            text("It is unsafe to use "),
                            code("this"),
                            text(" in the constructor "),
                            text("before all class properties are definitely initialized."),
                        ])
                    }
                }
            }
            MessageUnknownParameterTypes(lower) => friendly::Message(vec![
                text("the parameter types of an "),
                ref_(lower),
                text(" are unknown"),
            ]),
            MessageUnnecessaryDeclareTypeOnlyExport => friendly::Message(vec![
                text("The "),
                code("declare"),
                text(" keyword is unnecessary for type exports."),
            ]),
            MessageUnnecessaryInvariant(reason) => friendly::Message(vec![
                text("This use of `invariant` is unnecessary because "),
                ref_(reason),
                text(" is always truthy."),
            ]),
            MessageUnnecessaryOptionalChain(lhs_reason) => friendly::Message(vec![
                text("This use of optional chaining ("),
                code("?."),
                text(") is unnecessary because "),
                ref_(lhs_reason),
                text(" cannot be nullish or because an earlier "),
                code("?."),
                text(" will short-circuit the nullish case."),
            ]),
            MessageUnreachableCode => friendly::Message(vec![text("Unreachable code.")]),
            MessageUnsafeGetterSetter => friendly::Message(vec![text(
                "Getters and setters can have side effects and are unsafe.",
            )]),
            MessageUnsafeObjectAssign => friendly::Message(vec![
                text("Flow's support for "),
                code("Object.assign"),
                text(" is unsafe. Use spreads instead."),
            ]),
            MessageUnsupportedKeyInObject {
                key_error_kind,
                obj_kind,
            } => {
                use super::intermediate_error_types::InvalidObjKey;
                use super::intermediate_error_types::ObjKind;
                let suffix = match key_error_kind {
                    InvalidObjKey::Other => vec![text(
                        " Only identifier, string literal, and number literal keys are allowed.",
                    )],
                    InvalidObjKey::NumberNonInt => {
                        vec![text(" Only integer-like number literals are allowed.")]
                    }
                    InvalidObjKey::NumberTooLarge => vec![
                        text(" Number literals must not be larger than "),
                        code("Number.MAX_SAFE_INTEGER"),
                        text("."),
                    ],
                    InvalidObjKey::NumberTooSmall => vec![
                        text(" Number literals must not be smaller than "),
                        code("Number.MIN_SAFE_INTEGER"),
                        text("."),
                    ],
                };
                let obj_kind_str = match obj_kind {
                    ObjKind::Type => "object type",
                    ObjKind::Literal => "object literal",
                    ObjKind::Interface => "interface",
                    ObjKind::DeclareClass => "declare class",
                };
                let mut features = vec![text("Unsupported key in "), text(obj_kind_str), text(".")];
                features.extend(suffix);
                friendly::Message(features)
            }
            MessageUnsupportedVarianceAnnotation(kind) => friendly::Message(vec![
                text("Variance modifiers cannot appear on a type parameter of a "),
                text(kind),
                text("."),
            ]),
            MessageUntypedImport(module_name) => friendly::Message(vec![
                text("Importing from an untyped module makes it "),
                code("any"),
                text(" "),
                text("and is not safe! Did you mean to add "),
                code("// @flow"),
                text(" "),
                text("to the top of "),
                code(module_name),
                text("?"),
            ]),
            MessageUntypedTypeImport(module_name) => friendly::Message(vec![
                text("Importing a type from an untyped module makes it "),
                code("any"),
                text(" "),
                text("and is not safe! Did you mean to add "),
                code("// @flow"),
                text(" to "),
                text("the top of "),
                code(module_name),
                text("?"),
            ]),
            MessageUnusedPromiseInAsyncScope => friendly::Message(vec![
                code("Promise"),
                text(" in async scope is unused. Did you mean to "),
                code("await"),
                text(" it?"),
            ]),
            MessageUnusedPromiseInSyncScope => friendly::Message(vec![
                code("Promise"),
                text(
                    " in sync scope is unused. Promises must be handled by calling .then with a rejection handler, .catch, or .finally.",
                ),
            ]),
            MessageUnusedSuppression => {
                friendly::Message(vec![text("Unused suppression comment.")])
            }
            MessageValueUsedAsType(description) => friendly::Message(vec![
                text("Cannot use "),
                friendly::desc_of_reason_desc(description),
                text(" as a type. "),
                text("A name can be used as a type only if it refers to "),
                text("a type, interface, class, or enum definition. "),
                text("To get the type of a non-class value, use "),
                code("typeof"),
                text("."),
            ]),
            MessageVariableNeverInitAssignedAnnotated(reason) => friendly::Message(vec![
                text("Variable "),
                ref_(reason),
                text(" is never initialized, annotated, or assigned to."),
            ]),
            MessageVariableOnlyAssignedByNull(box MessageVariableOnlyAssignedByNullData {
                reason,
                null_loc,
            }) => {
                let null_ref = match null_loc {
                    Some(loc) => ref_(&mk_reason(
                        VirtualReasonDesc::RCode("null".into()),
                        loc.dupe(),
                    )),
                    None => code("null"),
                };
                friendly::Message(vec![
                    text("Variable "),
                    ref_(reason),
                    text(" is only ever assigned to by "),
                    null_ref,
                    text(". This is likely unintended; if it is intended, annotate "),
                    desc(reason),
                    text(" with "),
                    code(": null"),
                    text(" to disambiguate."),
                ])
            }
            // MessageUnsupportedSyntax variants
            MessageUnsupportedSyntax(syntax) => {
                use crate::intermediate_error_types::ContextDependentUnsupportedStatement::*;
                use crate::intermediate_error_types::DeclareClassPropKind;
                use crate::intermediate_error_types::TsLibSyntaxKind;
                use crate::intermediate_error_types::UnsupportedSyntax::*;
                match syntax {
                    AnnotationInsideDestructuring => friendly::Message(vec![
                        text("Annotations inside of destructuring are not supported. "),
                        text("Annotate the top-level pattern instead. "),
                        text("For example, instead of the invalid "),
                        code("const [a: number, b: string] = ..."),
                        text(" do "),
                        code("const [a, b]: [number, string] = ..."),
                        text("."),
                    ]),
                    AsConstOnNonLiteral => friendly::Message(vec![
                        text("The "),
                        code("as const"),
                        text(" assertion can only be used on string, numeric, boolean, object, "),
                        text("or array literals, "),
                        text("or const-variables initialized with primitive literals."),
                    ]),
                    CatchParameterDeclaration => {
                        friendly::Message(vec![text("Unsupported catch parameter declaration.")])
                    }
                    ClassPropertyLiteral => {
                        friendly::Message(vec![text("Literal properties not yet supported.")])
                    }
                    ClassPropertyComputed => {
                        friendly::Message(vec![text("Computed property keys not supported.")])
                    }
                    ClassStaticBlock => {
                        friendly::Message(vec![text("Class static blocks are not supported.")])
                    }
                    ClassDeclareMethod => friendly::Message(vec![text(
                        "Declare methods in classes are not supported.",
                    )]),
                    ClassIndexSignature => friendly::Message(vec![text(
                        "Index signatures in classes are not supported.",
                    )]),
                    ComponentSyntax => friendly::Message(vec![
                        text("Component syntax is not enabled. "),
                        text("You may opt-in to using component syntax by putting "),
                        code("component_syntax=true"),
                        text(" into the "),
                        code("[options]"),
                        text(" section of your "),
                        code(".flowconfig"),
                        text("."),
                    ]),
                    ContextDependentUnsupportedStatement(stmt) => match stmt {
                        NonLibdefToplevelDeclareModule => friendly::Message(vec![
                            code("declare module"),
                            text(" statement is only supported at the toplevel of a library file."),
                        ]),
                        ToplevelLibraryImport => friendly::Message(vec![
                            text(
                                "Cannot use an import statement at the toplevel of a library file. ",
                            ),
                            text("Import statements may only appear inside a "),
                            code("declare module"),
                            text(". The statement will be ignored."),
                        ]),
                        UnsupportedStatementInLibdef(kind) => friendly::Message(vec![
                            text("Cannot use "),
                            text(kind),
                            text(" statements in a library file. "),
                            text("The statement will be ignored."),
                        ]),
                        UnsupportedStatementInDeclareModule(kind) => friendly::Message(vec![
                            text("Cannot use "),
                            text(kind),
                            text(" statements with in "),
                            code("declare module"),
                            text(". The statement will be ignored."),
                        ]),
                        UnsupportedStatementInDeclareNamespace(kind) => friendly::Message(vec![
                            text("Cannot use "),
                            text(kind),
                            text(" statements with in "),
                            code("declare namespace"),
                            text(". The statement will be ignored."),
                        ]),
                    },
                    DeclareGlobal => friendly::Message(vec![
                        code("declare global"),
                        text(" statement is not supported yet."),
                    ]),
                    DestructuringExpressionPattern => friendly::Message(vec![text(
                        "Unsupported expression pattern in destructuring.",
                    )]),
                    DestructuringObjectPropertyInvalidLiteral => friendly::Message(vec![text(
                        "Unsupported literal object property in destructuring. String literals and int-like number literals are supported.",
                    )]),
                    ExistsType => friendly::Message(vec![
                        text("The existential type "),
                        code("*"),
                        text(" is deprecated. This syntax is no longer supported."),
                    ]),
                    ExplicitCallAfterProto => friendly::Message(vec![text(
                        "Unexpected call property after explicit prototype.",
                    )]),
                    ExplicitProtoAfterCall => {
                        friendly::Message(vec![text("Unexpected prototype after call property.")])
                    }
                    IllegalName => friendly::Message(vec![text("Illegal name.")]),
                    ImportDynamicArgument => friendly::Message(vec![
                        text("The parameter passed to "),
                        code("import"),
                        text(" must be a string literal."),
                    ]),
                    InvariantSpreadArgument => friendly::Message(vec![
                        text("Unsupported arguments in call to "),
                        code("invariant"),
                        text("."),
                    ]),
                    JSXTypeArgs => {
                        friendly::Message(vec![text("Flow doesn't support JSX type arguments.")])
                    }
                    MatchExpression => friendly::Message(vec![
                        code("match"),
                        text(" expressions are not supported."),
                    ]),
                    MatchStatement => friendly::Message(vec![
                        code("match"),
                        text(" statements are not supported."),
                    ]),
                    MatchInstancePattern => friendly::Message(vec![
                        code("match"),
                        text(" instance patterns are not supported."),
                    ]),
                    MetaPropertyExpression => friendly::Message(vec![text("Not supported.")]),
                    MultipleIndexers => {
                        friendly::Message(vec![text("Multiple indexers are not supported.")])
                    }
                    MultipleProtos => {
                        friendly::Message(vec![text("Multiple prototypes specified.")])
                    }
                    ObjectPropertyGetSet => {
                        friendly::Message(vec![text("Get/set properties not yet supported.")])
                    }
                    ObjectPropertyComputedGetSet => friendly::Message(vec![text(
                        "Computed getters and setters are not yet supported.",
                    )]),
                    OpaqueTypeExtendsBound => friendly::Message(vec![
                        text("Specifying opaque type upper bound using "),
                        code("extends"),
                        text(" keyword is not yet supported"),
                    ]),
                    OpaqueTypeSuperBound => friendly::Message(vec![
                        text("Specifying opaque type lower bound using "),
                        code("super"),
                        text(" keyword is not yet supported"),
                    ]),
                    PredicateFunction => friendly::Message(vec![text(
                        "Support for predicate functions is removed. `%checks` declaration is now ignored.",
                    )]),
                    PredicateDeclarationAnonymousParameters => friendly::Message(vec![
                        text("Predicate function declarations cannot use anonymous "),
                        text("function parameters."),
                    ]),
                    Records => friendly::Message(vec![text("Records are not enabled.")]),
                    DeclareClassMethodMissingReturnType => friendly::Message(vec![
                        text("Return type annotation is required for "),
                        code("declare class"),
                        text(" methods."),
                    ]),
                    DeclareVariableNonLiteralInit => friendly::Message(vec![
                        text("Initializer in a "),
                        code("declare"),
                        text(" variable must be a literal (string, number, bigint, or boolean)."),
                    ]),
                    DeclareVariableDestructuring => friendly::Message(vec![
                        text("Destructuring is not supported in "),
                        code("declare"),
                        text(" variable declarations."),
                    ]),
                    DeclareVariableMissingAnnotationOrInit => friendly::Message(vec![
                        code("declare"),
                        text(
                            " variable declarations require a type annotation or a literal initializer.",
                        ),
                    ]),
                    DeclareVariableAnnotationAndInit => friendly::Message(vec![
                        code("declare"),
                        text(
                            " variable declarations cannot have both a type annotation and an initializer.",
                        ),
                    ]),
                    DeclareClassProperty(kind) => {
                        use DeclareClassPropKind::*;
                        match kind {
                            AnnotationAndInit => friendly::Message(vec![
                                code("declare"),
                                text(
                                    " class properties cannot have both a type annotation and an initializer.",
                                ),
                            ]),
                            MissingAnnotationOrInit => friendly::Message(vec![
                                code("declare"),
                                text(
                                    " class properties require a type annotation or a literal initializer.",
                                ),
                            ]),
                            NonLiteralInit => friendly::Message(vec![
                                text("Initializer in a "),
                                code("declare"),
                                text(
                                    " class property must be a literal (string, number, bigint, or boolean).",
                                ),
                            ]),
                            InitWithoutReadonly => friendly::Message(vec![
                                text("Only "),
                                code("readonly"),
                                text(" properties in "),
                                code("declare"),
                                text(" class can have initializers."),
                            ]),
                        }
                    }
                    ExportTypeSpecifierInExportType => friendly::Message(vec![
                        text("The "),
                        code("type"),
                        text(" keyword on named exports can only be used on regular "),
                        code("export"),
                        text(" statements. It cannot be used with "),
                        code("export type"),
                        text(" statements."),
                    ]),
                    TSLibSyntax(kind) => {
                        use TsLibSyntaxKind::*;
                        let kind_str = match kind {
                            DeclarationWithoutDeclare => "Declaration without `declare`",
                            ImportTypeAnnotation => "`import(\"module\")` type syntax",
                            DeclareExportNamespace => "`declare export namespace`",
                            ExportAssignment => "`export =` syntax",
                            ImportEqualsDeclaration => "`import ... = ...` syntax",
                            ImportEqualsQualifiedName => "`import ... = <QualifiedName>` syntax",
                            DeclareVariableMultipleDeclarators => {
                                "Multiple `declare` variable declarators"
                            }
                            DeclareVariableLiteralInit => {
                                "`declare` variable with literal initializer"
                            }
                            ExportTypeSpecifier => "Inline `type` modifier on export specifier",
                            TemplateLiteralType => "Template literal type",
                            ConstructorType => {
                                "Constructor type expression (`new (...) => T` / `abstract new (...) => T`)"
                            }
                            UniqueSymbolType => "`unique symbol` type",
                            TypeofImport => "`typeof import(\"module\")` syntax",
                            ImplementsDottedPath => "`implements` with dotted path",
                            OptionalClassProperty => "Optional class property (`?:`)",
                            OptionalShorthandMethod => "Optional method signature",
                            AnonymousDefaultExportFunction => {
                                "Anonymous default export function declaration"
                            }
                            MappedTypeKeyRemapping => "Key remapping (`as`) in mapped types",
                            ReadonlyMappedTypeVarianceOp => {
                                "`+readonly`/`-readonly` mapped type modifier"
                            }
                            OptionalUnlabeledTupleElement => "Optional unlabeled tuple element",
                            OptionalIndexer => "Optional indexer (`?:`)",
                            NamespaceExportDeclaration => "`export as namespace`",
                            PrivateClassField => "Private class field (`#private`)",
                            GenericTaggedTemplate => {
                                "Type arguments for tagged template expression"
                            }
                            TypeofThis => "`typeof this` syntax",
                            PropertyValueInitializer => "Property value initializer in declaration",
                            ClassExtendsCall => "Call expression in class `extends` clause",
                            OverrideModifier => "`override` modifier on class members",
                        };
                        friendly::Message(vec![text(kind_str), text(" is not enabled.")])
                    }
                    RequireDynamicArgument => friendly::Message(vec![
                        text("The parameter passed to "),
                        code("require"),
                        text(" must be a string literal."),
                    ]),
                    SpreadArgument => {
                        friendly::Message(vec![text("A spread argument is unsupported here.")])
                    }
                    UserDefinedTypeGuards { kind } => {
                        use flow_parser::ast::types::TypeGuardKind;
                        let kind_str = match kind {
                            TypeGuardKind::Default => "This kind of type guard is",
                            TypeGuardKind::Asserts => "Type guard assertions are",
                            TypeGuardKind::Implies => "One-sided type guards are",
                        };
                        friendly::Message(vec![text(&format!("{} not yet supported.", kind_str))])
                    }
                    UnsupportedInternalSlot { name, static_ } => {
                        if *static_ {
                            friendly::Message(vec![
                                text("Unsupported static internal slot "),
                                code(name),
                                text("."),
                            ])
                        } else {
                            friendly::Message(vec![
                                text("Unsupported internal slot "),
                                code(name),
                                text("."),
                            ])
                        }
                    }
                    WithStatement => friendly::Message(vec![
                        text("Flow doesn't support "),
                        code("with"),
                        text(" statements."),
                    ]),
                    NonnullAssertion => friendly::Message(vec![
                        text("Flow doesn't support non-null assertions (the postfix "),
                        code("!"),
                        text(" operator)."),
                    ]),
                    AsyncComponentSyntax => {
                        friendly::Message(vec![text("Async component syntax is not supported.")])
                    }
                    AsyncHookSyntax => {
                        friendly::Message(vec![text("Async hook syntax is not supported.")])
                    }
                }
            }
            // MessageMatch* variants
            MessageMatchNotExhaustive { examples } => {
                let examples_msgs: Vec<friendly::Message<Loc>> = examples
                    .iter()
                    .map(|(pattern, reasons)| {
                        let reason_refs: Vec<friendly::Message<Loc>> = reasons
                            .iter()
                            .map(|reason| friendly::Message(vec![ref_(reason)]))
                            .collect();
                        let friendly::Message(concat_result) =
                            friendly::conjunction_concat(reason_refs, "and", Some(3));
                        let mut msg_features = vec![code(pattern), text(" to match ")];
                        msg_features.extend(concat_result);
                        friendly::Message(msg_features)
                    })
                    .collect();
                let result = match examples_msgs.len() {
                    0 => vec![],
                    1 => {
                        let friendly::Message(single_example) =
                            examples_msgs.into_iter().next().unwrap();
                        let mut result = vec![text("missing pattern: ")];
                        result.extend(single_example);
                        result.push(text("."));
                        result
                    }
                    _ => {
                        let size = examples_msgs.len();
                        let (limited_examples, suffix) = if size > 6 {
                            let taken: Vec<_> = examples_msgs.into_iter().take(5).collect();
                            (taken, vec![text(&format!("\nand {} others.", size - 5))])
                        } else {
                            (examples_msgs, vec![])
                        };
                        let mut result = vec![text("missing patterns:\n")];
                        for (i, friendly::Message(example)) in
                            limited_examples.into_iter().enumerate()
                        {
                            if i > 0 {
                                result.push(text("\n"));
                            }
                            result.push(text("- "));
                            result.extend(example);
                        }
                        result.extend(suffix);
                        result
                    }
                };
                let mut msg = vec![
                    code("match"),
                    text(" hasn't checked all possible cases of the input type. To fix, add the "),
                ];
                msg.extend(result);
                friendly::Message(msg)
            }
            MessageMatchUnnecessaryPattern {
                reason,
                already_seen,
            } => {
                let detail = match already_seen {
                    Some(already_seen) => vec![
                        text("The values it matches were already covered by a previous "),
                        ref_(already_seen),
                        text(". "),
                    ],
                    None => vec![text(
                        "The values it matches are either already covered by previous patterns, or are not part of the input type. ",
                    )],
                };
                let mut msg = vec![text("This "), ref_(reason), text(" is unused. ")];
                msg.extend(detail);
                msg.push(text(
                    "To fix, either remove this pattern or restructure previous patterns.",
                ));
                friendly::Message(msg)
            }
            MessageMatchNonExhaustiveObjectPattern(
                box MessageMatchNonExhaustiveObjectPatternData {
                    rest,
                    missing_props,
                    pattern_kind,
                },
            ) => {
                let pattern_kind_str = pattern_kind.to_string();
                let prefix = vec![
                    text("This "),
                    text(pattern_kind_str),
                    text(" hasn't considered all possible properties of the input. "),
                ];
                let has_missing_props = !missing_props.is_empty();
                let (properties_text, those_properties_text) = if missing_props.len() > 1 {
                    ("properties", "those properties")
                } else {
                    ("property", "that property")
                };
                let missing_props_msgs: Vec<_> = if has_missing_props {
                    let prop_msgs: Vec<friendly::Message<Loc>> = missing_props
                        .iter()
                        .map(|prop| {
                            let prop =
                                if flow_parser::js_id_unicode::string_is_valid_identifier_name(prop)
                                {
                                    prop.to_string()
                                } else {
                                    let quote =
                                        flow_common_ty::ty_printer::better_quote(true, prop);
                                    let escaped =
                                        flow_common_ty::ty_printer::utf8_escape(&quote, prop);
                                    format!("{}{}{}", quote, escaped, quote)
                                };
                            friendly::Message(vec![code(&prop)])
                        })
                        .collect();
                    let friendly::Message(concat_result) =
                        friendly::conjunction_concat(prop_msgs, "and", Some(5));
                    let mut result = vec![text(&format!("The {} ", properties_text))];
                    result.extend(concat_result);
                    result.push(text(" are missing from the pattern"));
                    result
                } else {
                    vec![]
                };
                let suffix = match rest {
                    Some(reason) => {
                        let base_msg = vec![
                            text("could be additional properties due to "),
                            ref_(reason),
                            text(". To fix, add "),
                        ];
                        if has_missing_props {
                            let mut result = missing_props_msgs;
                            result.push(text(", and there "));
                            result.extend(base_msg);
                            result
                        } else {
                            let mut result = vec![text("There ")];
                            result.extend(base_msg);
                            result
                        }
                    }
                    None => {
                        let mut result = missing_props_msgs;
                        result.push(text(&format!(
                            ". To fix, either add {} to the {}, or add ",
                            those_properties_text, pattern_kind_str
                        )));
                        result
                    }
                };
                let mut msg = prefix;
                msg.extend(suffix);
                msg.push(code("..."));
                msg.push(text(
                    " to the end of the pattern to match all other properties.",
                ));
                friendly::Message(msg)
            }
            MessageMatchNonExplicitEnumCheck(box MessageMatchNonExplicitEnumCheckData {
                wildcard_reason,
                unchecked_members,
            }) => {
                let member_msgs: Vec<friendly::Message<Loc>> = unchecked_members
                    .iter()
                    .map(|member| friendly::Message(vec![code(member)]))
                    .collect();
                let friendly::Message(concat_result) =
                    friendly::conjunction_concat(member_msgs, "and", Some(5));
                let member_word = if unchecked_members.len() > 1 {
                    "members "
                } else {
                    "member "
                };
                let mut msg = vec![
                    text("The "),
                    ref_(wildcard_reason),
                    text(" does not check for missing enum "),
                    text(member_word),
                ];
                msg.extend(concat_result);
                msg.extend(vec![
                    text(", because the "),
                    code("require-explicit-enum-checks"),
                    text(" lint has been enabled. "),
                    text("To fix, add cases covering those enum members."),
                ]);
                friendly::Message(msg)
            }
            MessageMatchInvalidGuardedWildcard => friendly::Message(vec![
                text("Cannot have a wildcard which is guarded using an "),
                code("if"),
                text(" in the last case of a "),
                code("match"),
                text(". "),
                text("Either reorder this case, add a final unguarded (no "),
                code("if"),
                text(") wildcard "),
                code("_"),
                text(" in a case after this one, or remove the pattern."),
            ]),
            MessageMatchInvalidIdentOrMemberPattern { type_reason } => friendly::Message(vec![
                text("Cannot have "),
                ref_(type_reason),
                text(" in a match pattern position. "),
                text("Valid types for match patterns include string literals, number literals, "),
                text("bigint literals, boolean literals, enum members, null, or undefined."),
            ]),
            MessageMatchInvalidBindingKind { kind } => friendly::Message(vec![
                text("Cannot use "),
                code(kind.as_str()),
                text(" for match pattern binding. Only "),
                code("const"),
                text(" is allowed."),
            ]),
            MessageMatchInvalidObjectPropertyLiteral { pattern_kind } => friendly::Message(vec![
                text("Unsupported "),
                text(pattern_kind.to_string()),
                text(" property literal in match pattern. "),
                text("String literals and int-like number literals are supported."),
            ]),
            MessageMatchInvalidUnaryZero => friendly::Message(vec![
                text("Unary pattern on "),
                code("0"),
                text(" is not supported."),
            ]),
            MessageMatchInvalidUnaryPlusBigInt => friendly::Message(vec![
                text("Unary pattern "),
                code("+"),
                text(" on bigint literal is not supported."),
            ]),
            MessageMatchDuplicateObjectProperty { name, pattern_kind } => friendly::Message(vec![
                text("Duplicate property "),
                code(name),
                text(" in "),
                text(pattern_kind.to_string()),
                text("."),
            ]),
            MessageMatchBindingInOrPattern => friendly::Message(vec![text(
                "New bindings in 'or' patterns are not yet supported.",
            )]),
            MessageMatchInvalidAsPattern => friendly::Message(vec![
                text("Invalid "),
                code("as"),
                text(" pattern. Direct use on a binding pattern is not allowed."),
            ]),
            MessageMatchInvalidPatternReference { binding_reason } => friendly::Message(vec![
                text("Can't use variable "),
                ref_(binding_reason),
                text(" within the same match pattern it is defined."),
            ]),
            MessageMatchInvalidObjectShorthand { name, pattern_kind } => {
                let pattern_kind_str = pattern_kind.to_string();
                friendly::Message(vec![
                    code("match"),
                    text(" "),
                    text(pattern_kind_str),
                    text("s don't allow this property shorthand syntax. "),
                    text("To fix, be explicit and either use "),
                    code(&format!("{{const {}}}", name)),
                    text(" if you want to create a new variable with the value of property "),
                    code(name),
                    text(", or use "),
                    code(&format!("{{{}: {}}}", name, name)),
                    text(" if you want to match property "),
                    code(name),
                    text(" against the value of the variable named "),
                    code(name),
                    text("."),
                ])
            }
            MessageMatchStatementInvalidBody => friendly::Message(vec![
                code("match"),
                text(" statements use blocks for each case body. "),
                text("To fix, wrap this statement with "),
                code("{"),
                text(" and "),
                code("}"),
                text("."),
            ]),
            MessageMatchInvalidCaseSyntax(kind) => {
                use crate::intermediate_error_types::MatchInvalidCaseSyntax::*;
                let msg_invalid_prefix_case = vec![
                    text("Drop the "),
                    code("case"),
                    text(". Just <pattern>. It's cleaner."),
                ];
                let msg_invalid_infix_colon = vec![
                    code("match"),
                    text(" cases use "),
                    code("=>"),
                    text(" to separate the pattern and the case body. To fix, replace the "),
                    code(";"),
                    text(" with "),
                    code("=>"),
                    text("."),
                ];
                let msg_invalid_suffix_semicolon = vec![
                    code("match"),
                    text(" uses commas "),
                    code(","),
                    text(" to separate cases. To fix, replace the "),
                    code(";"),
                    text(" with "),
                    code(","),
                    text("."),
                ];
                match kind {
                    InvalidMatchCaseMultiple {
                        invalid_prefix_case_locs,
                        invalid_infix_colon_locs,
                        invalid_suffix_semicolon_locs,
                    } => {
                        let msg_with_locs =
                            |msg: Vec<friendly::MessageFeature<Loc>>,
                             locs: &[L]|
                             -> Option<Vec<friendly::MessageFeature<Loc>>> {
                                if locs.is_empty() {
                                    None
                                } else {
                                    let refs: Vec<_> = locs
                                        .iter()
                                        .map(|loc| friendly::no_desc_ref(&loc_of_aloc(loc)))
                                        .collect();
                                    let mut result = msg;
                                    result.push(text(" At"));
                                    result.extend(refs);
                                    result.push(text("."));
                                    Some(result)
                                }
                            };
                        let errors: Vec<Vec<friendly::MessageFeature<Loc>>> = [
                            msg_with_locs(
                                msg_invalid_prefix_case.clone(),
                                invalid_prefix_case_locs,
                            ),
                            msg_with_locs(
                                msg_invalid_infix_colon.clone(),
                                invalid_infix_colon_locs,
                            ),
                            msg_with_locs(
                                msg_invalid_suffix_semicolon.clone(),
                                invalid_suffix_semicolon_locs,
                            ),
                        ]
                        .into_iter()
                        .flatten()
                        .collect();
                        match errors.len() {
                            0 => friendly::Message(vec![]),
                            1 => friendly::Message(errors.into_iter().next().unwrap()),
                            _ => {
                                let mut result = vec![text("Invalid match case syntax:")];
                                for error in errors {
                                    result.push(text("\n- "));
                                    result.extend(error);
                                }
                                friendly::Message(result)
                            }
                        }
                    }
                    InvalidMatchCasePrefixCase => friendly::Message(msg_invalid_prefix_case),
                    InvalidMatchCaseInfixColon => friendly::Message(msg_invalid_infix_colon),
                    InvalidMatchCaseSuffixSemicolon => {
                        friendly::Message(msg_invalid_suffix_semicolon)
                    }
                }
            }
            MessageMatchInvalidWildcardSyntax => friendly::Message(vec![
                code("match"),
                text(" uses "),
                code("_"),
                text(" for wildcard patterns which match everything. "),
                text("To fix, replace "),
                code("default"),
                text(" with "),
                code("_"),
                text("."),
            ]),
            MessageMatchInvalidInstancePattern => friendly::Message(vec![text(
                "Invalid match instance pattern constructor. It must reference a single class.",
            )]),
            // MessageRecord* variants
            MessageRecordBannedTypeUtil {
                reason_op,
                reason_record,
            } => friendly::Message(vec![
                text("Operation "),
                ref_(reason_op),
                text(" is not allowed on record "),
                ref_(reason_record),
                text(". "),
                text("To fix, turn the record type into an object type first using "),
                code("{...MyRecord}"),
                text(", for example: "),
                code("TypeUtil<{...MyRecord}>"),
                text("."),
            ]),
            MessageRecordInvalidNew { record_name } => friendly::Message(vec![
                text("Use a record expression directly instead of "),
                code("new"),
                text(" with an object literal. For example: "),
                code(&format!("{} {{...}}", record_name)),
                text("."),
            ]),
            MessageRecordInvalidName { name } => friendly::Message(vec![
                text("Invalid record name "),
                code(name),
                text(". Record names cannot start with lowercase 'a' through 'z'."),
            ]),
            MessageRecordDeclarationInvalidSyntax(kind) => {
                use crate::intermediate_error_types::RecordDeclarationInvalidSyntax::*;
                let msg_invalid_infix_equals = vec![
                    text("Record declarations don't need the "),
                    code("="),
                    text(", remove it."),
                ];
                let msg_invalid_variance = vec![
                    text("Record declaration properties are read-only by default, "),
                    text("so don't allow variance annotations. Remove to fix."),
                ];
                let msg_invalid_optional = vec![
                    text("Record declaration properties can't be optional. "),
                    text("Instead, you can specify a default value for the property using "),
                    code("= <expression>,"),
                    text(" after the type annotation. "),
                    text("This default value will be used if the property "),
                    text("is omitted when creating the record. "),
                    text("To closely mimic the existing behavior of an optional property, "),
                    text("you can change "),
                    code("foo?: T,"),
                    text(" to "),
                    code("foo: T | void = undefined,"),
                    text(", though there is likely a better default value you could use instead."),
                ];
                let msg_invalid_suffix_semicolon = vec![
                    text("Record declarations use commas "),
                    code(","),
                    text(" to separate properties. "),
                    text("To fix, replace the "),
                    code(";"),
                    text(" with "),
                    code(","),
                    text("."),
                ];
                match kind {
                    InvalidRecordDeclarationSyntaxMultiple {
                        invalid_infix_equals_loc,
                        invalid_variance_locs,
                        invalid_optional_locs,
                        invalid_suffix_semicolon_locs,
                    } => {
                        let msg_with_locs =
                            |msg: Vec<friendly::MessageFeature<Loc>>,
                             locs: &[L]|
                             -> Option<Vec<friendly::MessageFeature<Loc>>> {
                                if locs.is_empty() {
                                    None
                                } else {
                                    let refs: Vec<_> = locs
                                        .iter()
                                        .map(|loc| friendly::no_desc_ref(&loc_of_aloc(loc)))
                                        .collect();
                                    let mut result = msg;
                                    result.push(text(" At"));
                                    result.extend(refs);
                                    result.push(text("."));
                                    Some(result)
                                }
                            };
                        let msg_with_loc_opt =
                            |msg: Vec<friendly::MessageFeature<Loc>>,
                             loc_opt: &Option<L>|
                             -> Option<Vec<friendly::MessageFeature<Loc>>> {
                                match loc_opt {
                                    None => None,
                                    Some(loc) => {
                                        let mut result = msg;
                                        result.push(text(" At "));
                                        result.push(friendly::no_desc_ref(&loc_of_aloc(loc)));
                                        result.push(text("."));
                                        Some(result)
                                    }
                                }
                            };
                        let errors: Vec<Vec<friendly::MessageFeature<Loc>>> = [
                            msg_with_loc_opt(
                                msg_invalid_infix_equals.clone(),
                                invalid_infix_equals_loc,
                            ),
                            msg_with_locs(msg_invalid_variance.clone(), invalid_variance_locs),
                            msg_with_locs(msg_invalid_optional.clone(), invalid_optional_locs),
                            msg_with_locs(
                                msg_invalid_suffix_semicolon.clone(),
                                invalid_suffix_semicolon_locs,
                            ),
                        ]
                        .into_iter()
                        .flatten()
                        .collect();
                        match errors.len() {
                            0 => friendly::Message(vec![]),
                            1 => friendly::Message(errors.into_iter().next().unwrap()),
                            _ => {
                                let mut result = vec![text("Invalid record declaration syntax:")];
                                for error in errors {
                                    result.push(text("\n- "));
                                    result.extend(error);
                                }
                                friendly::Message(result)
                            }
                        }
                    }
                    InvalidRecordDeclarationSyntaxVariance => {
                        friendly::Message(msg_invalid_variance)
                    }
                    InvalidRecordDeclarationSyntaxOptional => {
                        friendly::Message(msg_invalid_optional)
                    }
                    InvalidRecordDeclarationSyntaxSuffixSemicolon => {
                        friendly::Message(msg_invalid_suffix_semicolon)
                    }
                    InvalidRecordDeclarationSyntaxInfixEquals => {
                        friendly::Message(msg_invalid_infix_equals)
                    }
                }
            }
            // Remaining Message variants
            MessageIncompatiblETypeParamConstIncompatibility { lower, upper } => {
                friendly::Message(vec![
                    text("type parameters "),
                    ref_(lower),
                    text(" and "),
                    ref_(upper),
                    text(" do not have matching const-modifier values"),
                ])
            }
            MessageTypeParamConstInvalidPosition(reason) => friendly::Message(vec![
                text("Type parameter "),
                ref_(reason),
                text(" cannot be declared as 'const'. "),
                text("'const' modifier can only appear on a function or method type parameter."),
            ]),
            MessageConstantCondition {
                is_truthy,
                show_warning,
                constant_condition_kind,
                reason,
            } => {
                use crate::intermediate_error_types::ConstantConditionKind;
                let truthy_str = if *is_truthy { "truthy" } else { "falsy" };
                let likely_str = if *show_warning { " likely" } else { "" };
                let base_message = vec![text(&format!(
                    "This condition is{} {}.",
                    likely_str, truthy_str
                ))];
                let help_message = match constant_condition_kind {
                    ConstantConditionKind::ConstCondGeneral => vec![],
                    ConstantConditionKind::UnawaitedPromise => {
                        vec![text(
                            " Perhaps you meant to use `await` to resolve the promise?",
                        )]
                    }
                    ConstantConditionKind::UncalledFunction => {
                        vec![text(" Perhaps you meant to call the function?")]
                    }
                };
                let warning_message = if *show_warning {
                    let suggested_loc = reason
                        .as_ref()
                        .and_then(|r| r.annot_loc().or_else(|| r.def_loc_opt()));
                    let mut msg = vec![
                        text(
                            "\n[WARNING]: Flow's type inference may be incorrect that it could be null ",
                        ),
                        text(
                            "at runtime (due to `any` annotations, out-of-bounds array accesses, etc.). ",
                        ),
                        text("If the check is valid, you might want to make"),
                    ];
                    match suggested_loc {
                        Some(loc) => msg.push(friendly::no_desc_ref(&loc_of_aloc(loc))),
                        None => msg.push(text(" the type of this expression")),
                    }
                    msg.push(text(" nullable (`T` -> `?T`)."));
                    msg
                } else {
                    vec![]
                };
                let mut msg = base_message;
                msg.extend(help_message);
                msg.extend(warning_message);
                friendly::Message(msg)
            }
            MessageCannotAssignToObjectWithComputedPropWithKey {
                reason_prop,
                reason_key,
                kind,
            } => {
                use crate::intermediate_error_types::InvalidObjKey;
                let suffix = match kind {
                    InvalidObjKey::Other => vec![
                        text(" Computed properties may only be numeric or string literal values,"),
                        text(" but this one is a "),
                        ref_(reason_prop),
                        text(". Can you add an appropriate type annotation to "),
                        ref_(&reason_key.dupe()),
                        text("?"),
                        text(
                            " See https://flow.org/en/docs/types/literals/ for more information on literal types.",
                        ),
                    ],
                    InvalidObjKey::NumberNonInt => {
                        vec![text(" Only integer-like number literals are allowed.")]
                    }
                    InvalidObjKey::NumberTooLarge => vec![
                        text(" Number literals must not be larger than "),
                        code("Number.MAX_SAFE_INTEGER"),
                        text("."),
                    ],
                    InvalidObjKey::NumberTooSmall => vec![
                        text(" Number literals must not be smaller than "),
                        code("Number.MIN_SAFE_INTEGER"),
                        text("."),
                    ],
                };
                let mut features = vec![
                    text("Cannot use "),
                    ref_(reason_key),
                    text(" to assign a computed property."),
                ];
                features.extend(suffix);
                friendly::Message(features)
            }
            MessageDevOnlyRefinedLocInfo { refining_locs } => {
                let mut features = vec![text("Refined at")];
                for loc in refining_locs {
                    features.push(friendly::no_desc_ref(&loc_of_aloc(loc)));
                }
                friendly::Message(features)
            }
            MessageDevOnlyInvalidatedRefinementInfo(invalidation_info) => {
                use flow_common::refinement_invalidation::string_of_reason;
                let info_msgs: Vec<friendly::Message<Loc>> = invalidation_info
                    .iter()
                    .map(|(loc, reason)| {
                        let reason_str = string_of_reason(*reason);
                        friendly::Message(vec![
                            text(reason_str),
                            text(" at"),
                            friendly::no_desc_ref(&loc_of_aloc(loc)),
                        ])
                    })
                    .collect();
                let friendly::Message(concat_result) =
                    friendly::conjunction_concat(info_msgs, "and", None);
                let mut features = vec![text("Refinement invalidated due to ")];
                features.extend(concat_result);
                friendly::Message(features)
            }
            MessageMissingPlatformSupportWithAvailablePlatforms(
                box MessageMissingPlatformSupportWithAvailablePlatformsData {
                    available_platforms,
                    required_platforms,
                },
            ) => {
                let missing_platforms: Vec<_> = required_platforms
                    .difference(available_platforms)
                    .cloned()
                    .collect();
                let platform_features =
                    |platforms: &[FlowSmolStr]| -> Vec<friendly::MessageFeature<Loc>> {
                        match platforms {
                            [] => vec![text("no platforms")],
                            [p] => vec![text("the "), code(p.as_str()), text(" platform")],
                            [first, rest @ ..] => {
                                let mut result =
                                    vec![text("the following platforms: "), code(first.as_str())];
                                for p in rest {
                                    result.push(text(", "));
                                    result.push(code(p.as_str()));
                                }
                                result
                            }
                        }
                    };
                let available: Vec<_> = available_platforms.iter().cloned().collect();
                let required: Vec<_> = required_platforms.iter().cloned().collect();
                let mut features = vec![text("The imported module supports ")];
                features.extend(platform_features(&available));
                features.push(text(", but the current module requires the support of "));
                features.extend(platform_features(&required));
                features.push(text(". Support for "));
                features.extend(platform_features(&missing_platforms));
                features.push(text(" is missing."));
                friendly::Message(features)
            }
            MessageParseError(parse_error) => friendly::message_of_string(&parse_error.to_string()),
            MessageDoesNotRender { lower, upper } => {
                // We replace the desc of the reason so we can say "LHS" does not render "RHS" instead of
                // "LHS" does not render "renders RHS"
                fn loop_render_desc<
                    L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
                >(
                    desc: &VirtualReasonDesc<L>,
                ) -> VirtualReasonDesc<L> {
                    use VirtualReasonDesc::*;
                    match desc {
                        RRenderType(inner) | RRenderMaybeType(inner) | RRenderStarType(inner) => {
                            loop_render_desc(inner)
                        }
                        _ => desc.clone(),
                    }
                }
                let lower_desc = loop_render_desc(&lower.desc);
                let lower_r = lower.dupe().replace_desc(lower_desc);
                let upper_desc = loop_render_desc(&upper.desc);
                let upper_r = upper.dupe().replace_desc(upper_desc);
                friendly::Message(vec![
                    ref_(&lower_r),
                    text(" does not render "),
                    ref_(&upper_r),
                ])
            }
            MessageFunctionRequiresAnotherArgument { def, from } => match from {
                None => friendly::Message(vec![ref_(def), text(" requires another argument")]),
                Some(from) => friendly::Message(vec![
                    ref_(def),
                    text(" requires another argument from "),
                    ref_(from),
                ]),
            },
            MessageIncompatibleComponentRestParam(rest_param) => friendly::Message(vec![
                text("Cannot use "),
                ref_(rest_param),
                text(
                    " as a component rest param. Component rest params must use an object type and cannot be optional",
                ),
            ]),
            MessageIncompatibleDueToInvariantSubtyping(
                box MessageIncompatibleDueToInvariantSubtypingData {
                    sub_component,
                    lower_loc,
                    upper_loc,
                    lower_desc,
                    upper_desc,
                },
            ) => match sub_component {
                None => friendly::Message(vec![
                    ref_of_ty_or_desc(lower_loc, lower_desc),
                    text(" is not exactly the same as "),
                    ref_of_ty_or_desc(upper_loc, upper_desc),
                ]),
                Some(SubComponentOfInvariantSubtypingError::ObjectProps(props)) => {
                    let prop_codes: Vec<friendly::Message<Loc>> = props
                        .iter()
                        .map(|prop| friendly::Message(vec![code(prop.as_str())]))
                        .collect();
                    let mut features = vec![text("properties ")];
                    features.extend(friendly::conjunction_concat(prop_codes, "and", None).0);
                    features.push(text(" of "));
                    features.push(ref_of_ty_or_desc(lower_loc, lower_desc));
                    features.push(text(" are not exactly the same as those of "));
                    features.push(ref_of_ty_or_desc(upper_loc, upper_desc));
                    friendly::Message(features)
                }
            },
            MessagePropExtraAgainstExactObject(box MessagePropExtraAgainstExactObjectData {
                lower,
                upper,
                props,
            }) => {
                let number_to_check = props.len();
                let prop_message: Vec<friendly::MessageFeature<Loc>> = if number_to_check > 5 {
                    let max_display_amount = 4;
                    let mut result: Vec<_> = props
                        .iter()
                        .take(max_display_amount)
                        .flat_map(|prop| vec![text("property "), code(prop), text(", ")])
                        .collect();
                    result.push(text(&format!(
                        "and {} others",
                        number_to_check - max_display_amount
                    )));
                    result
                } else {
                    let items: Vec<friendly::Message<Loc>> = props
                        .iter()
                        .map(|prop| friendly::Message(vec![text("property "), code(prop)]))
                        .collect();
                    friendly::conjunction_concat(items, "and", None).0
                };
                let plural = props.len() > 1;
                let upper_kind = if flow_common::reason::is_record_reason(upper) {
                    "Records"
                } else {
                    "Exact objects"
                };
                let mut result = prop_message;
                result.push(text(" "));
                result.push(text(if plural { "are" } else { "is" }));
                result.push(text(" extra in "));
                result.push(ref_(lower));
                result.push(text(" but missing in "));
                result.push(ref_(upper));
                result.push(text(". "));
                result.push(text(upper_kind));
                result.push(text(" do not accept extra props"));
                friendly::Message(result)
            }
        }
    };

    fn convert_error_message<
        L: Dupe + PartialEq + Eq + PartialOrd + Ord + Clone + std::fmt::Debug,
    >(
        explanation_to_friendly_msgs: &impl Fn(&Explanation<L>) -> friendly::Message<Loc>,
        frame_to_friendly_msgs: &impl Fn(
            bool,
            &ErrorFrame<L>,
        )
            -> (Option<friendly::Message<Loc>>, friendly::Message<Loc>),
        root_msg_to_root_kind_and_friendly_msgs: &impl Fn(
            &RootMessage<L>,
        ) -> (RootKind, friendly::Message<Loc>),
        msg_to_friendly_msgs: &impl Fn(&Message<L>) -> friendly::Message<Loc>,
        error: &IntermediateError<L>,
    ) -> PrintableError<Loc> {
        use flow_common_errors::error_utils::mk_error as flow_mk_error;
        use flow_common_errors::error_utils::mk_speculation_error as flow_mk_speculation_error;

        let root = error.root.as_ref().map(|(loc, root_msg)| {
            let (kind, msg) = root_msg_to_root_kind_and_friendly_msgs(root_msg);
            (loc.dupe(), kind, msg)
        });

        match &error.message {
            ErrorMessage::SingletonMessage {
                message,
                frames,
                explanations,
            } => {
                let friendly_frames: Option<
                    Vec<(Option<friendly::Message<Loc>>, friendly::Message<Loc>)>,
                > = frames
                    .as_ref()
                    .map(|fs| fs.iter().map(|f| frame_to_friendly_msgs(true, f)).collect());

                let friendly_explanations: Option<Vec<friendly::Message<Loc>>> = explanations
                    .as_ref()
                    .map(|exps| exps.iter().map(explanation_to_friendly_msgs).collect());

                let friendly_message = msg_to_friendly_msgs(message);

                flow_mk_error(
                    Some(error.kind),
                    root,
                    friendly_frames,
                    friendly_explanations,
                    error.loc.dupe(),
                    error.error_code,
                    friendly_message,
                )
            }

            ErrorMessage::SpeculationMessage {
                frames,
                explanations,
                branches,
            } => {
                let friendly_frames: Vec<friendly::Message<Loc>> = frames
                    .iter()
                    .map(|f| frame_to_friendly_msgs(false, f).1)
                    .collect();

                let friendly_explanations: Vec<friendly::Message<Loc>> = explanations
                    .iter()
                    .map(explanation_to_friendly_msgs)
                    .collect();

                let friendly_branches: Vec<(i32, PrintableError<Loc>)> = branches
                    .iter()
                    .map(|(score, branch_error)| {
                        let converted = convert_error_message(
                            explanation_to_friendly_msgs,
                            frame_to_friendly_msgs,
                            root_msg_to_root_kind_and_friendly_msgs,
                            msg_to_friendly_msgs,
                            branch_error,
                        );
                        (*score, converted)
                    })
                    .collect();

                flow_mk_speculation_error(
                    Some(error.kind),
                    error.loc.dupe(),
                    root,
                    friendly_frames,
                    friendly_explanations,
                    error.error_code,
                    friendly_branches,
                )
            }
        }
    }

    let printable_error = convert_error_message(
        &explanation_to_friendly_msgs,
        &frame_to_friendly_msgs,
        &root_msg_to_root_kind_and_friendly_msgs,
        &msg_to_friendly_msgs,
        &intermediate_error,
    );

    let printable_error = if intermediate_error.unsuppressable {
        flow_common_errors::error_utils::patch_unsuppressable_error(printable_error)
    } else {
        printable_error
    };

    match &intermediate_error.misplaced_source_file {
        None => printable_error,
        Some(source_file) => flow_common_errors::error_utils::patch_misplaced_error(
            strip_root,
            source_file,
            printable_error,
        ),
    }
}

/// Convert a set of FlowErrors to printable errors.
#[allow(dead_code)]
pub fn make_errors_printable<F, G>(
    loc_of_aloc: F,
    get_ast: G,
    strip_root: Option<&std::path::Path>,
    errors: ErrorSet,
) -> ConcreteLocPrintableErrorSet
where
    F: Fn(&ALoc) -> Loc + Clone,
    G: Fn(&FileKey) -> Option<Arc<flow_parser::ast::Program<Loc, Loc>>> + Clone,
{
    errors.fold(ConcreteLocPrintableErrorSet::empty(), |mut acc, err| {
        let intermediate = make_intermediate_error(loc_of_aloc.clone(), false, &err);
        let printable = to_printable_error(
            loc_of_aloc.clone(),
            get_ast.clone(),
            strip_root,
            intermediate,
        );
        acc.add(printable);
        acc
    })
}
