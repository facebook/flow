/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![feature(box_patterns)]

use std::collections::BTreeSet;
use std::ops::Deref;
use std::rc::Rc;

use flow_aloc::ALoc;
use flow_common::flow_symbol::dump_symbol;
use flow_common::polarity::Polarity;
use flow_common::reason;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::string_of_aloc;
use flow_lint_settings::lint_settings::LintParseError;
use flow_lint_settings::lints::PropertyAssignmentKind;
use flow_lint_settings::lints::SketchyNullKind;
use flow_lint_settings::lints::SketchyNumberKind;
use flow_parser::loc_sig::LocSig;
use flow_type_sig::signature_error::BindingValidation;
use flow_type_sig::signature_error::SignatureError;
use flow_typing_context::Context;
use flow_typing_default::Default;
use flow_typing_errors::error_message::EAssignConstLikeBindingData;
use flow_typing_errors::error_message::EBuiltinModuleLookupFailedData;
use flow_typing_errors::error_message::EBuiltinNameLookupFailedData;
use flow_typing_errors::error_message::ECallTypeArityData;
use flow_typing_errors::error_message::ECannotSpreadIndexerOnRightData;
use flow_typing_errors::error_message::ECannotSpreadInterfaceData;
use flow_typing_errors::error_message::EClassToObjectData;
use flow_typing_errors::error_message::EComparisonData;
use flow_typing_errors::error_message::EComponentThisReferenceData;
use flow_typing_errors::error_message::EConstantConditionData;
use flow_typing_errors::error_message::EDevOnlyInvalidatedRefinementInfoData;
use flow_typing_errors::error_message::EDevOnlyRefinedLocInfoData;
use flow_typing_errors::error_message::EDuplicateClassMemberData;
use flow_typing_errors::error_message::EDuplicateComponentPropData;
use flow_typing_errors::error_message::EDuplicateModuleProviderData;
use flow_typing_errors::error_message::EExpectedBigIntLitData;
use flow_typing_errors::error_message::EExpectedBooleanLitData;
use flow_typing_errors::error_message::EExpectedModuleLookupFailedData;
use flow_typing_errors::error_message::EExpectedNumberLitData;
use flow_typing_errors::error_message::EExpectedStringLitData;
use flow_typing_errors::error_message::EExponentialSpreadData;
use flow_typing_errors::error_message::EExportRenamedDefaultData;
use flow_typing_errors::error_message::EHookIncompatibleData;
use flow_typing_errors::error_message::EHookRuleViolationData;
use flow_typing_errors::error_message::EHookUniqueIncompatibleData;
use flow_typing_errors::error_message::EIllegalAssertOperatorData;
use flow_typing_errors::error_message::EImplicitInstantiationUnderconstrainedErrorData;
use flow_typing_errors::error_message::EIncompatibleData;
use flow_typing_errors::error_message::EIncompatibleDefsData;
use flow_typing_errors::error_message::EIncompatiblePropData;
use flow_typing_errors::error_message::EIncompatibleReactDeepReadOnlyData;
use flow_typing_errors::error_message::EIncompatibleSpeculationData;
use flow_typing_errors::error_message::EIncompatibleWithUseOpData;
use flow_typing_errors::error_message::EIncorrectTypeWithReplacementData;
use flow_typing_errors::error_message::EIndexerCheckFailedData;
use flow_typing_errors::error_message::EInexactMayOverwriteIndexerData;
use flow_typing_errors::error_message::EInvalidBinaryArithData;
use flow_typing_errors::error_message::EInvalidDeclarationData;
use flow_typing_errors::error_message::EInvalidObjectKitData;
use flow_typing_errors::error_message::EInvalidReactCreateElementData;
use flow_typing_errors::error_message::EInvalidRendersTypeArgumentData;
use flow_typing_errors::error_message::EInvariantSubtypingWithUseOpData;
use flow_typing_errors::error_message::EKeySpreadPropData;
use flow_typing_errors::error_message::EMethodUnbindingData;
use flow_typing_errors::error_message::EMissingPlatformSupportData;
use flow_typing_errors::error_message::EMissingPlatformSupportWithAvailablePlatformsData;
use flow_typing_errors::error_message::EMissingTypeArgsData;
use flow_typing_errors::error_message::ENegativeTypeGuardConsistencyData;
use flow_typing_errors::error_message::EObjectComputedPropertyAccessData;
use flow_typing_errors::error_message::EObjectComputedPropertyPotentialOverwriteData;
use flow_typing_errors::error_message::EPlatformSpecificImplementationModuleLookupFailedData;
use flow_typing_errors::error_message::EPolarityMismatchData;
use flow_typing_errors::error_message::EPrimitiveAsInterfaceData;
use flow_typing_errors::error_message::EPropNotFoundInLookupData;
use flow_typing_errors::error_message::EPropNotFoundInSubtypingData;
use flow_typing_errors::error_message::EPropNotReadableData;
use flow_typing_errors::error_message::EPropNotWritableData;
use flow_typing_errors::error_message::EPropPolarityMismatchData;
use flow_typing_errors::error_message::EPropsExtraAgainstExactObjectData;
use flow_typing_errors::error_message::EPropsNotFoundInInvariantSubtypingData;
use flow_typing_errors::error_message::EPropsNotFoundInSubtypingData;
use flow_typing_errors::error_message::EReactIntrinsicOverlapData;
use flow_typing_errors::error_message::ERecursiveDefinitionData;
use flow_typing_errors::error_message::ERefComponentPropData;
use flow_typing_errors::error_message::ESketchyNullLintData;
use flow_typing_errors::error_message::ETSSyntaxData;
use flow_typing_errors::error_message::ETooFewTypeArgsData;
use flow_typing_errors::error_message::ETooManyTypeArgsData;
use flow_typing_errors::error_message::ETupleArityMismatchData;
use flow_typing_errors::error_message::ETupleElementNotReadableData;
use flow_typing_errors::error_message::ETupleElementNotWritableData;
use flow_typing_errors::error_message::ETupleElementPolarityMismatchData;
use flow_typing_errors::error_message::ETupleInvalidTypeSpreadData;
use flow_typing_errors::error_message::ETupleNonIntegerIndexData;
use flow_typing_errors::error_message::ETupleOutOfBoundsData;
use flow_typing_errors::error_message::ETupleRequiredAfterOptionalData;
use flow_typing_errors::error_message::ETypeGuardFunctionInvalidWritesData;
use flow_typing_errors::error_message::ETypeGuardFunctionParamHavocedData;
use flow_typing_errors::error_message::ETypeGuardIncompatibleWithFunctionKindData;
use flow_typing_errors::error_message::ETypeGuardInvalidParameterData;
use flow_typing_errors::error_message::ETypeParamConstIncompatibilityData;
use flow_typing_errors::error_message::EUnableToSpreadData;
use flow_typing_errors::error_message::EUnionOptimizationData;
use flow_typing_errors::error_message::EUnionOptimizationOnNonUnionData;
use flow_typing_errors::error_message::EUnionPartialOptimizationNonUniqueKeyData;
use flow_typing_errors::error_message::EUnionSpeculationFailedData;
use flow_typing_errors::error_message::EVarianceKeywordData;
use flow_typing_errors::error_message::EnumAllMembersAlreadyCheckedData;
use flow_typing_errors::error_message::EnumBigIntMemberNotInitializedData;
use flow_typing_errors::error_message::EnumBooleanMemberNotInitializedData;
use flow_typing_errors::error_message::EnumDuplicateMemberNameData;
use flow_typing_errors::error_message::EnumErrorKind;
use flow_typing_errors::error_message::EnumIncompatibleData;
use flow_typing_errors::error_message::EnumInconsistentMemberValuesData;
use flow_typing_errors::error_message::EnumInvalidAbstractUseData;
use flow_typing_errors::error_message::EnumInvalidCheckData;
use flow_typing_errors::error_message::EnumInvalidMemberAccessData;
use flow_typing_errors::error_message::EnumInvalidMemberInitializerData;
use flow_typing_errors::error_message::EnumInvalidMemberNameData;
use flow_typing_errors::error_message::EnumInvalidObjectFunctionData;
use flow_typing_errors::error_message::EnumInvalidObjectUtilTypeData;
use flow_typing_errors::error_message::EnumKind;
use flow_typing_errors::error_message::EnumMemberAlreadyCheckedData;
use flow_typing_errors::error_message::EnumMemberDuplicateValueData;
use flow_typing_errors::error_message::EnumMemberUsedAsTypeData;
use flow_typing_errors::error_message::EnumModificationData;
use flow_typing_errors::error_message::EnumNonIdentifierMemberNameData;
use flow_typing_errors::error_message::EnumNotAllCheckedData;
use flow_typing_errors::error_message::EnumNumberMemberNotInitializedData;
use flow_typing_errors::error_message::EnumStringMemberInconsistentlyInitializedData;
use flow_typing_errors::error_message::EnumUnknownNotCheckedData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::InternalError;
use flow_typing_errors::error_message::InvalidMappedTypeErrorKind;
use flow_typing_errors::error_message::MatchDuplicateObjectPropertyData;
use flow_typing_errors::error_message::MatchErrorKind;
use flow_typing_errors::error_message::MatchInvalidCaseSyntaxData;
use flow_typing_errors::error_message::MatchInvalidIdentOrMemberPatternData;
use flow_typing_errors::error_message::MatchInvalidObjectShorthandData;
use flow_typing_errors::error_message::MatchInvalidPatternReferenceData;
use flow_typing_errors::error_message::MatchNonExhaustiveObjectPatternData;
use flow_typing_errors::error_message::MatchNonExplicitEnumCheckData;
use flow_typing_errors::error_message::MatchNotExhaustiveData;
use flow_typing_errors::error_message::MatchUnusedPatternData;
use flow_typing_errors::error_message::RecordErrorKind;
use flow_typing_errors::error_message::UpperKind;
use flow_typing_errors::error_message::string_of_invalid_render_type_kind;
use flow_typing_errors::intermediate_error_types::ConstantConditionKind;
use flow_typing_errors::intermediate_error_types::DocblockError;
use flow_typing_errors::intermediate_error_types::ExponentialSpreadReasonGroup;
use flow_typing_errors::intermediate_error_types::ObjKind as IntermediateObjKind;
use flow_typing_type::type_;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::ArrRestTData;
use flow_typing_type::type_::BindTData;
use flow_typing_type::type_::CallArgInner;
use flow_typing_type::type_::CallElemTData;
use flow_typing_type::type_::CallTData;
use flow_typing_type::type_::CanonicalRendersForm;
use flow_typing_type::type_::CondTData;
use flow_typing_type::type_::ConditionalTData;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::DestructorConditionalTypeData;
use flow_typing_type::type_::DestructorMappedTypeData;
use flow_typing_type::type_::DestructorSpreadTupleTypeData;
use flow_typing_type::type_::ElemTData;
use flow_typing_type::type_::EnumInfoInner;
use flow_typing_type::type_::EvalTypeDestructorTData;
use flow_typing_type::type_::ExtendsUseTData;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::GetElemTData;
use flow_typing_type::type_::GetEnumTData;
use flow_typing_type::type_::GetTypeFromNamespaceTData;
use flow_typing_type::type_::HasOwnPropTData;
use flow_typing_type::type_::InstanceT;
use flow_typing_type::type_::Literal;
use flow_typing_type::type_::LookupTData;
use flow_typing_type::type_::MapTypeTData;
use flow_typing_type::type_::MethodTData;
use flow_typing_type::type_::MixedFlavor;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::OptionalIndexedAccessTData;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::PropertyType;
use flow_typing_type::type_::ReactKitTData;
use flow_typing_type::type_::RendersVariant;
use flow_typing_type::type_::ReposUseTData;
use flow_typing_type::type_::ResolveSpreadTData;
use flow_typing_type::type_::ResolveUnionTData;
use flow_typing_type::type_::SealGenericTData;
use flow_typing_type::type_::Selector;
use flow_typing_type::type_::SetElemTData;
use flow_typing_type::type_::SpecializeTData;
use flow_typing_type::type_::StrUtilOp;
use flow_typing_type::type_::SuperTData;
use flow_typing_type::type_::TestPropTData;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::ThisTypeAppTData;
use flow_typing_type::type_::TupleElement;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeMap;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::ValueToTypeReferenceTData;
use flow_typing_type::type_::nominal;
use flow_typing_type::type_::string_of_ctor;
use flow_typing_type::type_::string_of_use_ctor;
use flow_typing_type::type_::string_of_use_op;
use flow_typing_type::type_util::reason_of_t;
use flow_typing_type::type_util::reason_of_use_t;

pub fn string_of_selector(selector: &Selector) -> String {
    match selector {
        // TODO print info about the key
        Selector::Elem(_) => "Elem _".to_string(),
        Selector::Prop(x, _) => format!("Prop {}", x),
        Selector::ArrRest(i) => format!("ArrRest {}", i),
        Selector::ObjRest(xs) => {
            let xs_str: Vec<&str> = xs.iter().map(|s| s.as_str()).collect();
            format!("ObjRest [{}]", xs_str.join("; "))
        }
        Selector::Default => "Default".to_string(),
    }
}

pub fn string_of_destructor(destructor: &Destructor) -> String {
    match destructor {
        Destructor::NonMaybeType => "NonMaybeType".to_string(),
        Destructor::PropertyType { name, .. } => {
            format!("PropertyType {}", name.as_str())
        }
        Destructor::ElementType { .. } => "ElementType".to_string(),
        Destructor::EnumType => "EnumType".to_string(),
        Destructor::OptionalIndexedAccessNonMaybeType { .. } => {
            "OptionalIndexedAccessNonMaybeType".to_string()
        }
        Destructor::OptionalIndexedAccessResultType { .. } => {
            "OptionalIndexedAccessResultType".to_string()
        }
        Destructor::ExactType => "ExactType".to_string(),
        Destructor::ReadOnlyType => "ReadOnly".to_string(),
        Destructor::ReactCheckComponentConfig { .. } => "ReactCheckComponentConfig".to_string(),
        Destructor::PartialType => "PartialType".to_string(),
        Destructor::RequiredType => "RequiredType".to_string(),
        Destructor::SpreadType(..) => "Spread".to_string(),
        Destructor::SpreadTupleType(box DestructorSpreadTupleTypeData { .. }) => {
            "SpreadTupleType".to_string()
        }
        Destructor::RestType(..) => "Rest".to_string(),
        Destructor::ValuesType => "Values".to_string(),
        Destructor::ConditionalType(box DestructorConditionalTypeData { .. }) => {
            "ConditionalType".to_string()
        }
        Destructor::TypeMap(TypeMap::ObjectKeyMirror) => "ObjectKeyMirror".to_string(),
        Destructor::ReactElementConfigType => "ReactElementConfig".to_string(),
        Destructor::MappedType(box DestructorMappedTypeData { .. }) => "MappedType".to_string(),
        Destructor::ReactDRO(_) => "ReactDRO".to_string(),
    }
}

fn bool_of_sealtype(seal: &type_::object::spread::SealType) -> bool {
    match seal {
        type_::object::spread::SealType::Sealed => true,
        _ => false,
    }
}

// ***************************************************************** //

// debug printer

pub fn dump_reason(cx: &Context, reason: &Reason) -> String {
    if cx.should_strip_root() {
        reason::dump_reason(Some(cx.root().to_string_lossy().deref()), reason)
    } else {
        reason::dump_reason(None, reason)
    }
}

fn dump_t_(depth: u32, tvars: &mut BTreeSet<i32>, cx: &Context, t: &Type) -> String {
    if depth == 0 {
        return string_of_ctor(t).to_string();
    }

    fn p(cx: &Context, t: &Type, reason: bool, extra: &str) -> String {
        let ctor = string_of_ctor(t);
        let reason_str = if reason {
            format!("{:?}", dump_reason(cx, reason_of_t(t)))
        } else {
            String::new()
        };
        let sep = if reason && !extra.is_empty() {
            ", "
        } else {
            ""
        };
        format!("{} ({}{}{})", ctor, reason_str, sep, extra)
    }
    let (kid, tvar) = {
        let kid = move |tvars: &mut BTreeSet<i32>, t: &Type| -> String {
            dump_t_(depth - 1, tvars, cx, t)
        };

        let tvar = move |tvars: &mut BTreeSet<i32>, id: i32| -> String {
            dump_tvar_(depth - 1, tvars, cx, id)
        };
        (kid, tvar)
    };

    let defer_use = |expr: &type_::TypeDestructorT, t: &str| -> String {
        let type_::TypeDestructorTInner(use_op, _, destructor) = expr.deref();
        format!(
            "{}, TypeDestruct {} on {}",
            string_of_use_op(use_op),
            string_of_destructor(destructor),
            t
        )
    };

    fn string_of_mixed_flavor(flavor: &MixedFlavor) -> &'static str {
        match flavor {
            MixedFlavor::MixedEverything => "Mixed_everything",
            MixedFlavor::MixedFunction => "Mixed_function",
            MixedFlavor::MixedTruthy => "Mixed_truthy",
            MixedFlavor::MixedNonMaybe => "Mixed_non_maybe",
            MixedFlavor::MixedNonNull => "Mixed_non_null",
            MixedFlavor::MixedNonVoid => "Mixed_non_void",
        }
    }

    fn string_of_any_source(source: &AnySource) -> &'static str {
        match source {
            AnySource::AnnotatedAny => "AnnotatedAny",
            AnySource::CatchAny => "CatchAny",
            AnySource::AnyError(_) => "Error",
            AnySource::Unsound(_) => "Unsound",
            AnySource::Untyped => "Untyped",
            AnySource::Placeholder => "Placeholder",
        }
    }

    let instance_t = |tvars: &mut BTreeSet<i32>, inst: &InstanceT| -> String {
        let type_args_str: Vec<String> = inst
            .inst
            .type_args
            .iter()
            .map(|(n, _, t, _)| format!("{}={}", n, kid(tvars, t)))
            .collect();
        format!(
            "[{}] #{}",
            type_args_str.join(", "),
            inst.inst.class_id.0.debug_to_string(false)
        )
    };

    let tuple_elements =
        |tvars: &mut BTreeSet<i32>, elements: &[TupleElement], inexact: bool| -> String {
            let mut elems: Vec<String> = elements
                .iter()
                .map(|TupleElement { t, .. }| kid(tvars, t))
                .collect();
            if inexact {
                elems.push("...".to_string());
            }
            elems.join(", ")
        };

    match t.deref() {
        TypeInner::OpenT(tv) => {
            let extra = tvar(tvars, tv.id() as i32);
            p(cx, t, true, &extra)
        }
        TypeInner::DefT(_, def) => match def.deref() {
            DefTInner::NumGeneralT(lit) => {
                let extra = match lit {
                    Literal::Truthy => "truthy",
                    Literal::AnyLiteral => "",
                };
                p(cx, t, true, extra)
            }
            DefTInner::StrGeneralT(lit) => {
                let extra = match lit {
                    Literal::Truthy => "truthy",
                    Literal::AnyLiteral => "",
                };
                p(cx, t, true, extra)
            }
            DefTInner::BoolGeneralT => p(cx, t, true, ""),
            DefTInner::BigIntGeneralT(lit) => {
                let extra = match lit {
                    Literal::Truthy => "truthy",
                    Literal::AnyLiteral => "",
                };
                p(cx, t, true, extra)
            }
            DefTInner::FunT(_, fun) => {
                let this_str = kid(tvars, &fun.this_t.0);
                let params_str: Vec<String> =
                    fun.params.iter().map(|fp| kid(tvars, &fp.1)).collect();
                let rest_str = fun
                    .rest_param
                    .as_ref()
                    .map(|rp| format!("...{}", kid(tvars, &rp.2)))
                    .unwrap_or_default();
                let return_str = kid(tvars, &fun.return_t);
                let guard_str = match &fun.type_guard {
                    Some(tg) => {
                        let implies = if tg.one_sided { "implies " } else { "" };
                        format!(
                            " {}{}is {}",
                            implies,
                            tg.param_name.1.as_str(),
                            kid(tvars, &tg.type_guard)
                        )
                    }
                    None => String::new(),
                };
                let extra = format!(
                    "<this: {}>({}{}){} => {}{}",
                    this_str,
                    params_str.join("; "),
                    rest_str,
                    "",
                    return_str,
                    guard_str
                );
                p(cx, t, true, &extra)
            }
            DefTInner::MixedT(flavor) => {
                let extra = string_of_mixed_flavor(flavor);
                p(cx, t, true, extra)
            }
            DefTInner::EmptyT
            | DefTInner::SymbolT
            | DefTInner::UniqueSymbolT(_)
            | DefTInner::NullT
            | DefTInner::VoidT => p(cx, t, true, ""),
            DefTInner::PolyT(box PolyTData {
                tparams, t_out, id, ..
            }) => {
                let c_str = kid(tvars, t_out);
                let tparams_str: Vec<String> =
                    tparams.iter().map(|tp| tp.name.to_string()).collect();
                let extra = format!(
                    "{} [{}] #{}",
                    c_str,
                    tparams_str.join("; "),
                    id.debug_string()
                );
                p(cx, t, true, &extra)
            }
            DefTInner::ObjT(obj) => {
                let obj_kind_str = match &obj.flags.obj_kind {
                    ObjKind::Exact => "Exact".to_string(),
                    ObjKind::Inexact => "Inexact".to_string(),
                    ObjKind::Indexed(d) => {
                        format!(
                            "Indexed {{[{}]: {}}}",
                            dump_t_(depth, tvars, cx, &d.key),
                            dump_t_(depth, tvars, cx, &d.value)
                        )
                    }
                };
                let extra = format!("{}, {}", obj.props_tmap.debug_string(), obj_kind_str);
                p(cx, t, true, &extra)
            }
            DefTInner::ArrT(arr) => {
                use flow_typing_type::type_::ArrType;
                use flow_typing_type::type_::ArrayATData;
                use flow_typing_type::type_::TupleATData;
                match arr.as_ref() {
                    ArrType::ArrayAT(box ArrayATData {
                        elem_t,
                        tuple_view: None,
                        ..
                    }) => {
                        let extra = format!("Array {}", kid(tvars, elem_t));
                        p(cx, t, true, &extra)
                    }
                    ArrType::ArrayAT(box ArrayATData {
                        elem_t,
                        tuple_view: Some(tv),
                        ..
                    }) => {
                        let elems_str = tuple_elements(tvars, &tv.elements, tv.inexact);
                        let extra = format!("Array {}, [{}]", kid(tvars, elem_t), elems_str);
                        p(cx, t, true, &extra)
                    }
                    ArrType::TupleAT(box TupleATData {
                        elements, inexact, ..
                    }) => {
                        let elems_str = tuple_elements(tvars, elements, *inexact);
                        let extra = format!("Tuple [{}]", elems_str);
                        p(cx, t, true, &extra)
                    }
                    ArrType::ROArrayAT(box (elem_t, _)) => {
                        let extra = format!("ReadOnlyArray {}", kid(tvars, elem_t));
                        p(cx, t, true, &extra)
                    }
                }
            }
            DefTInner::ClassT(inst) => {
                let extra = kid(tvars, inst);
                p(cx, t, true, &extra)
            }
            DefTInner::InstanceT(inst) => {
                let extra = instance_t(tvars, inst.as_ref());
                p(cx, t, true, &extra)
            }
            DefTInner::TypeT(kind, arg) => {
                let extra = format!(
                    "{}, {}",
                    type_::string_of_type_t_kind(kind),
                    kid(tvars, arg)
                );
                p(cx, t, true, &extra)
            }
            DefTInner::EnumValueT(enum_info) => match &***enum_info {
                EnumInfoInner::AbstractEnum { representation_t } => {
                    let extra = format!("abstract: {}", kid(tvars, representation_t));
                    p(cx, t, true, &extra)
                }
                EnumInfoInner::ConcreteEnum(info) => {
                    let extra = format!(
                        "enum concrete: {} #{}",
                        info.enum_name.as_str(),
                        info.enum_id.0.debug_to_string(false)
                    );
                    p(cx, t, true, &extra)
                }
            },
            DefTInner::EnumObjectT { enum_info, .. } => match &***enum_info {
                EnumInfoInner::AbstractEnum { representation_t } => {
                    let extra = format!("abstract: {}", kid(tvars, representation_t));
                    p(cx, t, true, &extra)
                }
                EnumInfoInner::ConcreteEnum(info) => {
                    let extra = format!(
                        "enum concrete: {} #{}",
                        info.enum_name.as_str(),
                        info.enum_id.0.debug_to_string(false)
                    );
                    p(cx, t, true, &extra)
                }
            },
            DefTInner::NumericStrKeyT(num) => {
                let extra = num.1.as_str();
                p(cx, t, true, extra)
            }
            DefTInner::SingletonStrT { from_annot, value } => {
                let extra = format!("{:?} (from_annot={})", value.as_str(), from_annot);
                p(cx, t, true, &extra)
            }
            DefTInner::SingletonNumT { from_annot, value } => {
                let extra = format!("{} (from_annot={})", value.1.as_str(), from_annot);
                p(cx, t, true, &extra)
            }
            DefTInner::SingletonBoolT { value, .. } => {
                let extra = format!("{}", value);
                p(cx, t, true, &extra)
            }
            DefTInner::SingletonBigIntT { value, .. } => {
                let extra = value.1.as_str();
                p(cx, t, true, extra)
            }
            DefTInner::ReactAbstractComponentT(_) => p(cx, t, true, ""),
            DefTInner::RendersT(renders) => {
                let extra = match renders.as_ref() {
                    CanonicalRendersForm::IntrinsicRenders(n) => format!("intrinsic {}", n),
                    CanonicalRendersForm::NominalRenders { renders_name, .. } => {
                        format!("Nominal({})", renders_name)
                    }
                    CanonicalRendersForm::StructuralRenders {
                        renders_variant,
                        renders_structural_type,
                    } => {
                        let variant = match renders_variant {
                            RendersVariant::RendersNormal => "Normal",
                            RendersVariant::RendersMaybe => "Maybe",
                            RendersVariant::RendersStar => "Star",
                        };
                        format!(
                            "Structural({}, {})",
                            variant,
                            kid(tvars, renders_structural_type)
                        )
                    }
                    CanonicalRendersForm::DefaultRenders => "Default".to_string(),
                };
                p(cx, t, true, &extra)
            }
        },
        TypeInner::AnnotT(_, arg, use_desc) => {
            let extra = format!("use_desc={}, {}", use_desc, kid(tvars, arg));
            p(cx, t, true, &extra)
        }
        TypeInner::NominalT { nominal_type, .. } => {
            let type_args_str: Vec<String> = nominal_type
                .nominal_type_args
                .iter()
                .map(|(n, _, t, _)| format!("{}={}", n, kid(tvars, t)))
                .collect();
            let underlying_str = match &nominal_type.underlying_t {
                nominal::UnderlyingT::OpaqueWithLocal { t } => {
                    format!(" ({})", kid(tvars, t))
                }
                nominal::UnderlyingT::CustomError(box nominal::CustomErrorData {
                    t,
                    custom_error_loc,
                }) => {
                    format!(
                        " ({}, custom_error_loc={})",
                        kid(tvars, t),
                        custom_error_loc.debug_to_string(false)
                    )
                }
                nominal::UnderlyingT::FullyOpaque => String::new(),
            };
            let extra = format!("[{}]{}", type_args_str.join("; "), underlying_str);
            p(cx, t, true, &extra)
        }
        TypeInner::AnyT(_, src) => {
            let extra = string_of_any_source(src);
            p(cx, t, true, extra)
        }
        TypeInner::NullProtoT(_)
        | TypeInner::ObjProtoT(_)
        | TypeInner::FunProtoT(_)
        | TypeInner::FunProtoBindT(_) => p(cx, t, true, ""),
        TypeInner::ThisInstanceT(box ThisInstanceTData { instance, .. }) => {
            let extra = instance_t(tvars, instance);
            p(cx, t, true, &extra)
        }
        TypeInner::GenericT(box GenericTData { name, bound, .. }) => {
            let extra = format!("{}: {}", name, kid(tvars, bound));
            p(cx, t, true, &extra)
        }
        TypeInner::OptionalT { type_, .. } => {
            let extra = kid(tvars, type_);
            p(cx, t, true, &extra)
        }
        TypeInner::EvalT {
            type_,
            defer_use_t,
            id,
        } => {
            let arg_str = kid(tvars, type_);
            let defer_str = defer_use(defer_use_t, &arg_str);
            let extra = format!("{}, {}", defer_str, id.debug_string());
            p(cx, t, true, &extra)
        }
        TypeInner::TypeAppT(box TypeAppTData { type_, targs, .. }) => {
            let type_str = kid(tvars, type_);
            let targs_str: Vec<String> = targs.iter().map(|t| kid(tvars, t)).collect();
            let extra = format!("{}, [{}]", type_str, targs_str.join("; "));
            p(cx, t, true, &extra)
        }
        TypeInner::ThisTypeAppT(box ThisTypeAppTData {
            type_,
            this_t,
            targs,
            ..
        }) => {
            let base_str = kid(tvars, type_);
            let this_str = kid(tvars, this_t);
            let extra = match targs {
                Some(args) => {
                    let args_str: Vec<String> = args.iter().map(|t| kid(tvars, t)).collect();
                    format!("{}, {}, [{}]", base_str, this_str, args_str.join("; "))
                }
                None => format!("{}, {}", base_str, this_str),
            };
            p(cx, t, false, &extra)
        }
        TypeInner::MaybeT(_, arg) => {
            let extra = kid(tvars, arg);
            p(cx, t, true, &extra)
        }
        TypeInner::IntersectionT(_, rep) => {
            let members: Vec<String> = rep.members_iter().map(|t| kid(tvars, t)).collect();
            let extra = format!("[{}]", members.join("; "));
            p(cx, t, true, &extra)
        }
        TypeInner::UnionT(_, rep) => {
            let kind_str = match rep.union_kind() {
                type_::union_rep::UnionKind::ProvidersKind => "ProvidersKind",
                type_::union_rep::UnionKind::ConditionalKind => "ConditionalKind",
                type_::union_rep::UnionKind::ImplicitInstantiationKind => {
                    "ImplicitInstantiationKind"
                }
                type_::union_rep::UnionKind::ResolvedKind => "ResolvedKind",
                type_::union_rep::UnionKind::LogicalKind => "LogicalKind",
                type_::union_rep::UnionKind::UnknownKind => "UnknownKind",
            };
            let members: Vec<String> = rep.members_iter().map(|t| kid(tvars, t)).collect();
            let spec_str = rep.string_of_specialization();
            let extra = format!("(kind={})[{}]{}", kind_str, members.join("; "), spec_str);
            p(cx, t, true, &extra)
        }
        TypeInner::KeysT(_, arg) => {
            let extra = kid(tvars, arg);
            p(cx, t, true, &extra)
        }
        TypeInner::StrUtilT { op, remainder, .. } => {
            let (op_str, arg) = match op {
                StrUtilOp::StrPrefix(s) => ("prefix", s.as_str()),
                StrUtilOp::StrSuffix(s) => ("suffix", s.as_str()),
            };
            let remainder_str = match remainder {
                Some(t) => kid(tvars, t),
                None => "<None>".to_string(),
            };
            let extra = format!("{}:{:?}, remainder:{}", op_str, arg, remainder_str);
            p(cx, t, true, &extra)
        }
        TypeInner::NamespaceT(ns) => {
            let extra = format!(
                "name={}, values={}, types={}",
                dump_symbol(&ns.namespace_symbol),
                kid(tvars, &ns.values_type),
                ns.types_tmap.debug_string()
            );
            p(cx, t, true, &extra)
        }
    }
}

fn dump_use_t_<CX>(
    depth: u32,
    tvars: &mut BTreeSet<i32>,
    cx: &Context,
    use_t: &UseT<CX>,
) -> String {
    if depth == 0 {
        return string_of_use_ctor(use_t);
    }

    fn p<CX>(cx: &Context, use_t: &UseT<CX>, reason: bool, extra: &str) -> String {
        let ctor = string_of_use_ctor(use_t);
        let reason_str = if reason {
            format!("{:?}", dump_reason(cx, reason_of_use_t(use_t)))
        } else {
            String::new()
        };
        let sep = if reason && !extra.is_empty() {
            ", "
        } else {
            ""
        };
        format!("{}({}{}{})", ctor, reason_str, sep, extra)
    }

    let kid =
        move |tvars: &mut BTreeSet<i32>, t: &Type| -> String { dump_t_(depth - 1, tvars, cx, t) };

    let use_kid = move |tvars: &mut BTreeSet<i32>, u: &UseT<CX>| -> String {
        dump_use_t_(depth - 1, tvars, cx, u)
    };

    let tvar = move |tvars: &mut BTreeSet<i32>, id: i32| -> String {
        dump_tvar_(depth - 1, tvars, cx, id)
    };

    let normalized_prop = move |tvars: &mut BTreeSet<i32>, p: &PropertyType| -> String {
        dump_normalized_prop_(depth - 1, tvars, cx, p)
    };

    let call_arg_kid = |tvars: &mut BTreeSet<i32>, arg: &type_::CallArg| -> String {
        match arg.deref() {
            CallArgInner::Arg(t) => kid(tvars, t),
            CallArgInner::SpreadArg(t) => format!("...{}", kid(tvars, t)),
        }
    };

    let propref = |tvars: &mut BTreeSet<i32>, prop: &type_::PropRef| -> String {
        match prop {
            type_::PropRef::Named { reason, name, .. } => {
                format!("{:?} {}", dump_reason(cx, reason), name)
            }
            type_::PropRef::Computed(t) => kid(tvars, t),
        }
    };

    let lookup_kind = |tvars: &mut BTreeSet<i32>, kind: &type_::LookupKind| -> String {
        match kind {
            type_::LookupKind::NonstrictReturning(box type_::NonstrictReturningData(
                default_opt,
                testid_opt,
            )) => {
                let default_str = match default_opt {
                    Some((t, _)) => format!(" returning {}", kid(tvars, t)),
                    None => String::new(),
                };
                let testid_str = match testid_opt {
                    Some((id, _)) => format!(" for test id {}", id),
                    None => String::new(),
                };
                format!("Nonstrict{}{}", default_str, testid_str)
            }
            type_::LookupKind::Strict(r) => {
                format!("Strict {:?}", dump_reason(cx, r))
            }
        }
    };

    let lookup_action = |tvars: &mut BTreeSet<i32>, action: &type_::LookupAction| -> String {
        match action {
            type_::LookupAction::ReadProp(box type_::ReadPropData { tout, .. }) => {
                format!(
                    "Read ({}, {})",
                    string_of_reason(cx, tout.reason()),
                    tvar(tvars, tout.id() as i32)
                )
            }
            type_::LookupAction::WriteProp(box type_::WritePropData { tin, .. }) => {
                format!("Write {}", kid(tvars, tin))
            }
            type_::LookupAction::LookupPropForTvarPopulation { tout, polarity } => {
                format!(
                    "LookupPropForTvarPopulation ({:?}, {})",
                    polarity,
                    kid(tvars, tout)
                )
            }
            type_::LookupAction::LookupPropForSubtyping(
                box type_::LookupPropForSubtypingData { use_op, prop, .. },
            ) => {
                format!(
                    "LookupPropForSubtyping ({}, {})",
                    string_of_use_op(use_op),
                    normalized_prop(tvars, prop)
                )
            }
            type_::LookupAction::SuperProp(box (_, p)) => {
                format!("Super {}", normalized_prop(tvars, p))
            }
            type_::LookupAction::MatchProp(box type_::LookupActionMatchPropData {
                prop_t, ..
            }) => {
                format!("Match {}", kid(tvars, prop_t))
            }
        }
    };

    let react_kit = |tvars: &mut BTreeSet<i32>, tool: &type_::react::Tool<CX>| -> String {
        match tool {
            type_::react::Tool::CreateElement(box type_::react::CreateElementData {
                jsx_props,
                tout,
                ..
            }) => {
                let jsx_str = kid(tvars, jsx_props);
                let tout_str = tvar(tvars, tout.id() as i32);
                format!("CreateElement ({}) => {}", jsx_str, tout_str)
            }
            type_::react::Tool::ConfigCheck { props } => {
                format!("ConfigCheck ({})", kid(tvars, props))
            }
            type_::react::Tool::GetConfig { tout } => {
                format!("GetConfig ({})", kid(tvars, tout))
            }
        }
    };

    let slice = |s: &type_::object::Slice| -> String {
        let mut xs: Vec<String> = Vec::new();
        match &s.flags.obj_kind {
            ObjKind::Indexed(d) => {
                xs.push(format!("{}[]", d.dict_polarity.sigil()));
            }
            ObjKind::Exact | ObjKind::Inexact => {}
        }
        for (k, prop) in &s.props {
            let opt = match &prop.prop_t.deref() {
                TypeInner::OptionalT { .. } => "?",
                _ => "",
            };
            xs.push(format!("{}{}", k, opt));
        }
        let xs_str = xs.join("; ");
        match &s.flags.obj_kind {
            ObjKind::Exact => format!("{{|{}|}}", xs_str),
            _ => format!("{{{}}}", xs_str),
        }
    };

    let operand_slice =
        |reason: &Reason,
         prop_map: &flow_data_structure_wrapper::ord_map::FlowOrdMap<Name, Property>,
         dict: &type_::object::Dict|
         -> String {
            use type_::property::first_loc;
            use type_::property::read_t;
            use type_::property::write_t;
            let props: type_::object::Props = prop_map
                .iter()
                .filter_map(|(k, p)| match (read_t(p), write_t(p)) {
                    (Some(t), _) | (_, Some(t)) => Some((
                        k.clone(),
                        type_::object::Prop {
                            prop_t: t,
                            is_own: true,
                            is_method: false,
                            polarity: Polarity::Neutral,
                            key_loc: first_loc(p),
                        },
                    )),
                    _ => None,
                })
                .collect();
            let obj_kind = match dict {
                None => ObjKind::Exact,
                Some(d) => ObjKind::Indexed(d.clone()),
            };
            let flags = type_::Flags {
                obj_kind,
                react_dro: None,
            };
            let s = type_::object::Slice {
                reason: reason.clone(),
                props,
                flags,
                frozen: false,
                generics: flow_typing_generics::spread_empty(),
                interface: None,
                reachable_targs: Rc::from([]),
            };
            slice(&s)
        };

    let object_kit = |tvars: &mut BTreeSet<i32>,
                      resolve_tool: &type_::object::ResolveTool,
                      tool: &type_::object::Tool|
     -> String {
        let join = |j: &type_::object::Join| -> &'static str {
            match j.1 {
                type_::object::JoinOp::And => "And",
                type_::object::JoinOp::Or => "Or",
            }
        };
        let resolved = |xs: &type_::object::Resolved| -> String {
            let slices: Vec<String> = xs.0.iter().map(slice).collect();
            format!("[{}]", slices.join("; "))
        };
        let resolve = |tvars: &mut BTreeSet<i32>, r: &type_::object::Resolve| -> String {
            match r {
                type_::object::Resolve::Next => "Next".to_string(),
                type_::object::Resolve::List0(todo, j) => {
                    let kids: Vec<String> = todo.iter().map(|t| kid(tvars, t)).collect();
                    format!("List0 ([{}], {})", kids.join("; "), join(j))
                }
                type_::object::Resolve::List(todo, done_rev, j) => {
                    let kids: Vec<String> = todo.iter().map(|t| kid(tvars, t)).collect();
                    let done_strs: Vec<String> = done_rev.iter().map(resolved).collect();
                    format!(
                        "List ([{}], [{}], {})",
                        kids.join("; "),
                        done_strs.join("; "),
                        join(j)
                    )
                }
            }
        };
        let resolve_tool_str = match resolve_tool {
            type_::object::ResolveTool::Resolve(r) => {
                format!("Resolve {}", resolve(tvars, r))
            }
            type_::object::ResolveTool::Super(s, r) => {
                format!("Super ({}, {})", slice(s), resolve(tvars, r))
            }
        };
        let acc_element = |elem: &type_::object::spread::AccElement| -> String {
            match elem {
                type_::object::spread::AccElement::InlineSlice(os) => {
                    operand_slice(&os.reason, &os.prop_map, &os.dict)
                }
                type_::object::spread::AccElement::ResolvedSlice(xs) => resolved(xs),
            }
        };
        let spread_fn = |tvars: &mut BTreeSet<i32>,
                         target: &type_::object::spread::Target,
                         state: &type_::object::spread::State|
         -> String {
            let target_str = match target {
                type_::object::spread::Target::Annot { make_exact } => {
                    format!("Annot {{ make_exact={} }}", make_exact)
                }
                type_::object::spread::Target::Value { make_seal } => {
                    format!("Value {{make_seal={}", bool_of_sealtype(make_seal))
                }
            };
            let spread_operand =
                |tvars: &mut BTreeSet<i32>, op: &type_::object::spread::Operand| -> String {
                    match op {
                        type_::object::spread::Operand::Slice(os) => {
                            operand_slice(&os.reason, &os.prop_map, &os.dict)
                        }
                        type_::object::spread::Operand::Type(t) => kid(tvars, t),
                    }
                };
            let todo_strs: Vec<String> = state
                .todo_rev
                .iter()
                .map(|op| spread_operand(tvars, op))
                .collect();
            let acc_strs: Vec<String> = state.acc.iter().map(acc_element).collect();
            let union_reason_str = match &state.union_reason {
                Some(r) => dump_reason(cx, r),
                None => "None".to_string(),
            };
            format!(
                "Spread ({}, {{todo_rev=[{}]; acc=[{}]; spread_id={}; curr_resolve_idx={}; union_reason={}}})",
                target_str,
                todo_strs.join("; "),
                acc_strs.join("; "),
                state.spread_id,
                state.curr_resolve_idx,
                union_reason_str
            )
        };
        let rest_fn = |tvars: &mut BTreeSet<i32>,
                       merge_mode: &type_::object::rest::MergeMode,
                       state: &type_::object::rest::State|
         -> String {
            let merge_str = match merge_mode {
                type_::object::rest::MergeMode::SpreadReversal => "SpreadReversal",
                type_::object::rest::MergeMode::Omit => "Omit",
                type_::object::rest::MergeMode::ReactConfigMerge(_) => "ReactConfigMerge",
            };
            let state_str = match state {
                type_::object::rest::State::One(t) => format!("One ({})", kid(tvars, t)),
                type_::object::rest::State::Done(o) => format!("Done ({})", resolved(o)),
            };
            format!("Rest ({{merge_mode={}}}, {})", merge_str, state_str)
        };
        let react_props_fn = |state: &type_::object::react_config::State,
                              ref_manipulation: &type_::object::react_config::RefManipulation|
         -> String {
            let state_str = match state {
                type_::object::react_config::State::Config { .. } => "Config",
                type_::object::react_config::State::Defaults { .. } => "Defaults",
            };
            let ref_str = match ref_manipulation {
                type_::object::react_config::RefManipulation::KeepRef => "keep",
                type_::object::react_config::RefManipulation::AddRef(_) => "add",
            };
            format!("({}, ref_manipulation={})", state_str, ref_str)
        };
        let object_map_fn = |tvars: &mut BTreeSet<i32>, prop_type: &Type| -> String {
            format!("ObjectMap {{prop_type: {}}}", kid(tvars, prop_type))
        };
        let tool_str = match tool {
            type_::object::Tool::MakeExact => "MakeExact".to_string(),
            type_::object::Tool::ReadOnly => "ReadOnly".to_string(),
            type_::object::Tool::ReactCheckComponentConfig { .. } => {
                "ReactCheckComponentConfig".to_string()
            }
            type_::object::Tool::Partial => "Partial".to_string(),
            type_::object::Tool::Required => "Required".to_string(),
            type_::object::Tool::ObjectRep => "ObjectRep".to_string(),
            type_::object::Tool::Spread(box (target, state)) => spread_fn(tvars, target, state),
            type_::object::Tool::Rest(box (merge_mode, state)) => rest_fn(tvars, merge_mode, state),
            type_::object::Tool::ReactConfig(box type_::object::ObjectToolReactConfigData {
                state,
                ref_manipulation,
            }) => react_props_fn(state, ref_manipulation),
            type_::object::Tool::ObjectMap(box type_::object::ObjectToolObjectMapData {
                prop_type,
                ..
            }) => object_map_fn(tvars, prop_type),
        };
        format!("({}, {})", resolve_tool_str, tool_str)
    };

    let method_action = |tvars: &mut BTreeSet<i32>, action: &type_::MethodAction<CX>| -> String {
        match action {
            type_::MethodAction::CallM(box type_::CallMData { methodcalltype, .. })
            | type_::MethodAction::ChainM(box type_::ChainMData { methodcalltype, .. }) => {
                let this_str = match &methodcalltype.meth_generic_this {
                    Some(t) => kid(tvars, t),
                    None => "None".to_string(),
                };
                let args: Vec<String> = methodcalltype
                    .meth_args_tlist
                    .iter()
                    .map(|arg| call_arg_kid(tvars, arg))
                    .collect();
                let tout_str = tvar(tvars, methodcalltype.meth_tout.id() as i32);
                format!(
                    "<this: {}>({}) => ({}, {})",
                    this_str,
                    args.join("; "),
                    string_of_reason(cx, methodcalltype.meth_tout.reason()),
                    tout_str
                )
            }
            type_::MethodAction::NoMethodAction(_) => "NoMethodAction".to_string(),
        }
    };

    match use_t.deref() {
        UseTInner::UseT(use_op, t) => match t.deref() {
            TypeInner::OpenT(tv) => {
                format!(
                    "UseT ({}, OpenT ({:?}, {}))",
                    string_of_use_op(use_op),
                    dump_reason(cx, tv.reason()),
                    tv.id()
                )
            }
            _ => {
                let extra = kid(tvars, t);
                format!("UseT ({}, {})", string_of_use_op(use_op), extra)
            }
        },
        UseTInner::ArrRestT(box ArrRestTData { use_op, .. }) => {
            p(cx, use_t, true, &string_of_use_op(use_op))
        }
        UseTInner::BindT(box BindTData { use_op, .. }) => {
            p(cx, use_t, true, &string_of_use_op(use_op))
        }
        UseTInner::CallElemT(box CallElemTData { .. }) => p(cx, use_t, true, ""),
        UseTInner::CallT(box CallTData {
            use_op,
            call_action,
            ..
        }) => match call_action.as_ref() {
            type_::CallAction::Funcalltype(funcall) => {
                let this_str = kid(tvars, &funcall.call_this_t);
                let args: Vec<String> = funcall
                    .call_args_tlist
                    .iter()
                    .map(|arg| match arg.deref() {
                        CallArgInner::Arg(t) => kid(tvars, t),
                        CallArgInner::SpreadArg(t) => {
                            format!("...{}", kid(tvars, t))
                        }
                    })
                    .collect();
                let tout_str = tvar(tvars, funcall.call_tout.id() as i32);
                let extra = format!(
                    "{}, <this: {}>({}) => ({}, {})",
                    string_of_use_op(use_op),
                    this_str,
                    args.join("; "),
                    string_of_reason(cx, funcall.call_tout.reason()),
                    tout_str
                );
                p(cx, use_t, true, &extra)
            }
            type_::CallAction::ConcretizeCallee(_) => {
                let extra = format!("{} ConcretizeCallee", string_of_use_op(use_op));
                p(cx, use_t, true, &extra)
            }
        },
        UseTInner::ConstructorT(..) => p(cx, use_t, true, ""),
        UseTInner::ElemT(box ElemTData { obj, .. }) => {
            let extra = format!("obj: {}", kid(tvars, obj));
            p(cx, use_t, true, &extra)
        }
        UseTInner::ConditionalT(box ConditionalTData {
            distributive_tparam_name,
            infer_tparams,
            extends_t,
            true_t,
            false_t,
            tout,
            ..
        }) => {
            let dist = distributive_tparam_name
                .as_ref()
                .map(|n| format!("distributive over {}: ", n))
                .unwrap_or_default();
            let infer_str: Vec<String> =
                infer_tparams.iter().map(|tp| tp.name.to_string()).collect();
            let extra = format!(
                "{}[{}] extends {} ? {} : {} => {}",
                dist,
                infer_str.join("; "),
                kid(tvars, extends_t),
                kid(tvars, true_t),
                kid(tvars, false_t),
                tvar(tvars, tout.id() as i32)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::GetElemT(box GetElemTData {
            from_annot,
            access_iterables,
            key_t,
            tout,
            ..
        }) => {
            let extra = format!(
                "{}, ({}, {}), from_annot={}, access_iterables={}",
                kid(tvars, key_t),
                string_of_reason(cx, tout.reason()),
                tvar(tvars, tout.id() as i32),
                from_annot,
                access_iterables
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::GetKeysT(_, _)
        | UseTInner::GetValuesT(_, _)
        | UseTInner::GetDictValuesT(_, _) => p(cx, use_t, true, ""),
        UseTInner::GetPropT(data) => {
            let extra = format!(
                "{}, ({}), ({}, {})",
                string_of_use_op(&data.use_op),
                propref(tvars, &data.propref),
                string_of_reason(cx, data.tout.reason()),
                tvar(tvars, data.tout.id() as i32)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::GetPrivatePropT(data) => {
            let extra = format!(
                "({}), ({}, {})",
                data.name,
                string_of_reason(cx, data.tout.reason()),
                tvar(tvars, data.tout.id() as i32)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::GetProtoT(_, tv) => {
            let extra = tvar(tvars, tv.id() as i32);
            p(cx, use_t, true, &extra)
        }
        UseTInner::GetStaticsT(tv) => {
            let extra = tvar(tvars, tv.id() as i32);
            p(cx, use_t, true, &extra)
        }
        UseTInner::GetTypeFromNamespaceT(box GetTypeFromNamespaceTData {
            use_op,
            prop_ref,
            tout,
            ..
        }) => {
            let extra = format!(
                "{}, ({}), ({}, {})",
                string_of_use_op(use_op),
                prop_ref.1.as_str(),
                string_of_reason(cx, tout.reason()),
                tvar(tvars, tout.id() as i32)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::HasOwnPropT(box HasOwnPropTData { .. }) | UseTInner::ConcretizeT { .. } => {
            p(cx, use_t, true, "")
        }
        UseTInner::LookupT(box LookupTData {
            propref: prop,
            lookup_kind: kind,
            lookup_action: action,
            ids,
            ..
        }) => {
            let ids_str = match ids {
                None => "None".to_string(),
                Some(_ids) => "Some [...]".to_string(),
            };
            let extra = format!(
                "{:?}, {}, {}, [{}]",
                propref(tvars, prop),
                lookup_kind(tvars, kind),
                lookup_action(tvars, action),
                ids_str
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::MapTypeT(box MapTypeTData { .. }) => p(cx, use_t, true, ""),
        UseTInner::MethodT(box MethodTData {
            propref: prop,
            method_action: action,
            ..
        }) => {
            let extra = format!(
                "({}, {})",
                propref(tvars, prop),
                method_action(tvars, action)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::PrivateMethodT(data) => {
            let extra = format!(
                "({}), ({})",
                data.name,
                method_action(tvars, &data.method_action)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::MixinT(_, arg) => {
            let extra = kid(tvars, arg);
            p(cx, use_t, true, &extra)
        }
        UseTInner::ObjRestT(_, xs, arg, _) => {
            let extra = format!("[{}], {}", xs.join("; "), kid(tvars, arg));
            p(cx, use_t, true, &extra)
        }
        UseTInner::ObjTestProtoT(_, _) => p(cx, use_t, true, ""),
        UseTInner::ObjTestT(_, _, _) => p(cx, use_t, true, ""),
        UseTInner::OptionalIndexedAccessT(box OptionalIndexedAccessTData { index, .. }) => {
            let extra = match index {
                type_::OptionalIndexedAccessIndex::OptionalIndexedAccessTypeIndex(t) => {
                    kid(tvars, t)
                }
                type_::OptionalIndexedAccessIndex::OptionalIndexedAccessStrLitIndex(n) => {
                    n.as_str().to_owned()
                }
            };
            p(cx, use_t, true, &extra)
        }
        UseTInner::ReactKitT(box ReactKitTData { use_op, tool, .. }) => {
            let extra = format!("{}, {}", string_of_use_op(use_op), react_kit(tvars, tool));
            p(cx, use_t, true, &extra)
        }
        UseTInner::ReposLowerT {
            use_desc,
            use_t: arg,
            ..
        } => {
            let extra = format!("use_desc={}, {}", use_desc, use_kid(tvars, arg));
            p(cx, use_t, true, &extra)
        }
        UseTInner::ReposUseT(box ReposUseTData {
            use_desc,
            use_op,
            type_: arg,
            ..
        }) => {
            let inner = UseT::new(UseTInner::UseT(use_op.clone(), arg.clone()));
            let extra = format!("use_desc={}, {}", use_desc, use_kid(tvars, &inner));
            p(cx, use_t, true, &extra)
        }
        UseTInner::ResolveSpreadT(box ResolveSpreadTData {
            use_op,
            resolve_spread_type: spread,
            ..
        }) => {
            let extra = match &spread.rrt_resolve_to {
                type_::SpreadResolve::ResolveSpreadsToTupleType { elem_t, tout, .. }
                | type_::SpreadResolve::ResolveSpreadsToArrayLiteral { elem_t, tout, .. } => {
                    format!(
                        "{}, {}, {}",
                        string_of_use_op(use_op),
                        kid(tvars, elem_t),
                        kid(tvars, tout)
                    )
                }
                type_::SpreadResolve::ResolveSpreadsToArray(elem_t, tout) => {
                    format!(
                        "{}, {}, {}",
                        string_of_use_op(use_op),
                        kid(tvars, elem_t),
                        kid(tvars, tout)
                    )
                }
                type_::SpreadResolve::ResolveSpreadsToMultiflowPartial(
                    box type_::ResolveSpreadsToMultiflowPartialData(_, _, _, tout),
                ) => {
                    format!("{}, {}", string_of_use_op(use_op), kid(tvars, tout))
                }
                type_::SpreadResolve::ResolveSpreadsToMultiflowCallFull(_, _)
                | type_::SpreadResolve::ResolveSpreadsToMultiflowSubtypeFull(_, _) => {
                    string_of_use_op(use_op)
                }
            };
            p(cx, use_t, true, &extra)
        }
        UseTInner::SuperT(box SuperTData { .. }) => p(cx, use_t, true, ""),
        UseTInner::ImplementsT(_, arg) => {
            let extra = kid(tvars, arg);
            p(cx, use_t, false, &extra)
        }
        UseTInner::SetElemT(box SetElemTData {
            key_t: ix,
            tin: etype,
            ..
        }) => {
            let extra = format!("{}, {}", kid(tvars, ix), kid(tvars, etype));
            p(cx, use_t, true, &extra)
        }
        UseTInner::SetPropT(use_op, _, prop, _, _, ptype, _) => {
            let extra = format!(
                "{}, ({}), {}",
                string_of_use_op(use_op),
                propref(tvars, prop),
                kid(tvars, ptype)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::SetPrivatePropT(data) => {
            let extra = format!("({}), {}", data.name, kid(tvars, &data.tin));
            p(cx, use_t, true, &extra)
        }
        UseTInner::SetProtoT(_, arg) => {
            let extra = kid(tvars, arg);
            p(cx, use_t, true, &extra)
        }
        UseTInner::SpecializeT(box SpecializeTData {
            targs: args_opt,
            tvar: ret,
            ..
        }) => {
            let extra = match args_opt {
                Some(args) => {
                    let args_str: Vec<String> = args.iter().map(|t| kid(tvars, t)).collect();
                    format!("[{}], {}", args_str.join("; "), kid(tvars, ret))
                }
                None => kid(tvars, ret),
            };
            p(cx, use_t, true, &extra)
        }
        UseTInner::ObjKitT(use_op, _, resolve, tool, tout) => {
            let extra = format!(
                "{}, {}, {}",
                string_of_use_op(use_op),
                object_kit(tvars, resolve, tool),
                kid(tvars, tout)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::TestPropT(box TestPropTData {
            use_op,
            propref: prop,
            tout,
            ..
        }) => {
            let extra = format!(
                "{}, ({}), ({}, {})",
                string_of_use_op(use_op),
                propref(tvars, prop),
                string_of_reason(cx, tout.reason()),
                tvar(tvars, tout.id() as i32)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::ThisSpecializeT(_, this, _) => {
            let extra = kid(tvars, this);
            p(cx, use_t, true, &extra)
        }
        UseTInner::ToStringT { t_out, .. } => {
            let extra = use_kid(tvars, t_out);
            p(cx, use_t, true, &extra)
        }
        UseTInner::ValueToTypeReferenceT(box ValueToTypeReferenceTData {
            use_op,
            kind,
            tout,
            ..
        }) => {
            let extra = format!(
                "{}, reason, {}, {}",
                string_of_use_op(use_op),
                type_::string_of_type_t_kind(kind),
                tvar(tvars, tout.id() as i32)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::ConcretizeTypeAppsT(_, _, _, _) => p(cx, use_t, true, ""),
        UseTInner::GetEnumT(box GetEnumTData { kind, .. }) => {
            let extra = match kind {
                type_::GetEnumKind::GetEnumObject => "get enum object",
                type_::GetEnumKind::GetEnumValue => "get enum value",
            };
            p(cx, use_t, true, extra)
        }
        UseTInner::FilterOptionalT(_, arg) => {
            let extra = kid(tvars, arg);
            p(cx, use_t, false, &extra)
        }
        UseTInner::FilterMaybeT(_, arg) => {
            let extra = kid(tvars, arg);
            p(cx, use_t, false, &extra)
        }
        UseTInner::DeepReadOnlyT(tv, _) => {
            let extra = tvar(tvars, tv.id() as i32);
            p(cx, use_t, true, &extra)
        }
        UseTInner::HooklikeT(tv) => {
            let extra = tvar(tvars, tv.id() as i32);
            p(cx, use_t, true, &extra)
        }
        UseTInner::SealGenericT(box SealGenericTData { name, cont, .. }) => {
            let extra = match cont {
                type_::Cont::Lower(_, l) => {
                    format!("{} <~ {}", name, kid(tvars, l))
                }
                type_::Cont::Upper(u) => {
                    format!("{} ~> {}", name, use_kid(tvars, u))
                }
            };
            p(cx, use_t, true, &extra)
        }
        UseTInner::CondT(box CondTData {
            opt_type: then_t,
            true_t: else_t,
            false_t: tout,
            ..
        }) => {
            let then_str = match then_t {
                Some(t) => format!("Some ({})", kid(tvars, t)),
                None => "None".to_string(),
            };
            let extra = format!("{}, {}, {}", then_str, kid(tvars, else_t), kid(tvars, tout));
            p(cx, use_t, true, &extra)
        }
        UseTInner::ExtendsUseT(box ExtendsUseTData {
            targs: nexts,
            true_t: l,
            false_t: u,
            ..
        }) => {
            let nexts_str: Vec<String> = nexts.iter().map(|t| kid(tvars, t)).collect();
            let extra = format!(
                "[{}], {}, {}",
                nexts_str.join("; "),
                kid(tvars, l),
                kid(tvars, u)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::ResolveUnionT(box ResolveUnionTData {
            resolved,
            unresolved,
            upper,
            id,
            ..
        }) => {
            let resolved_str: Vec<String> = resolved.iter().map(|t| kid(tvars, t)).collect();
            let unresolved_str: Vec<String> = unresolved.iter().map(|t| kid(tvars, t)).collect();
            let extra = format!(
                "{} [{}], [{}], {}",
                id,
                resolved_str.join("; "),
                unresolved_str.join("; "),
                use_kid(tvars, upper)
            );
            p(cx, use_t, true, &extra)
        }
        UseTInner::CheckUnusedPromiseT { reason, .. } => {
            format!("CheckUnusedPromiseT ({})", string_of_reason(cx, reason))
        }
        UseTInner::ConvertEmptyPropsToMixedT(_, _) => "ConvertEmptyPropsToMixedT".to_string(),
        UseTInner::ExitRendersT { .. } => "ExitRendersT".to_string(),
        UseTInner::EvalTypeDestructorT(box EvalTypeDestructorTData {
            destructor, tout, ..
        }) => {
            let extra = format!(
                "{} on upper, ({}, {})",
                string_of_destructor(destructor),
                string_of_reason(cx, tout.reason()),
                tvar(tvars, tout.id() as i32)
            );
            p(cx, use_t, true, &extra)
        }
    }
}

fn dump_tvar_(depth: u32, tvars: &mut BTreeSet<i32>, cx: &Context, id: i32) -> String {
    use flow_utils_union_find::Node;
    use type_::constraint::Constraints;

    if tvars.contains(&id) {
        return format!("{}, ^", id);
    }
    tvars.insert(id);

    let node = cx.find_tvar(id);
    match node {
        Node::Goto { parent } => {
            format!("{}, Goto {}", id, parent)
        }
        Node::Root(root) => match &root.constraints {
            Constraints::Resolved(t) => {
                format!(
                    "{}, Resolved {}",
                    id,
                    dump_t_(depth.saturating_sub(1), tvars, cx, t)
                )
            }
            Constraints::FullyResolved(s) => {
                let payload = match s.get_forced_for_debugging() {
                    None => "unevaluated".to_string(),
                    Some(t) => dump_t_(depth.saturating_sub(1), tvars, cx, &t),
                };
                format!("{}, FullyResolved {}", id, payload)
            }
            Constraints::Unresolved(bounds) => {
                let bounds = bounds.borrow();
                if bounds.lower.is_empty() && bounds.upper.is_empty() {
                    format!("{}", id)
                } else {
                    let lower_strs: Vec<String> = bounds
                        .lower
                        .keys()
                        .map(|t| dump_t_(depth.saturating_sub(1), tvars, cx, t))
                        .collect();
                    let upper_strs: Vec<String> = bounds
                        .upper
                        .keys()
                        .map(|key| dump_use_t_(depth.saturating_sub(1), tvars, cx, &key.use_t))
                        .collect();
                    format!(
                        "{}, [{}], [{}]",
                        id,
                        lower_strs.join("; "),
                        upper_strs.join("; ")
                    )
                }
            }
        },
    }
}

fn dump_prop_(depth: u32, tvars: &mut BTreeSet<i32>, cx: &Context, p: &Property) -> String {
    match p.deref() {
        PropertyInner::Field(fd) => {
            format!(
                "Field ({}) {}",
                fd.polarity.string(),
                dump_t_(depth, tvars, cx, &fd.type_)
            )
        }
        PropertyInner::Get { type_, .. } => {
            format!("Get {}", dump_t_(depth, tvars, cx, type_))
        }
        PropertyInner::Set { type_, .. } => {
            format!("Set {}", dump_t_(depth, tvars, cx, type_))
        }
        PropertyInner::GetSet(gs) => {
            format!(
                "Get {} Set {}",
                dump_t_(depth, tvars, cx, &gs.get_type),
                dump_t_(depth, tvars, cx, &gs.set_type)
            )
        }
        PropertyInner::Method { type_, .. } => {
            format!("Method {}", dump_t_(depth, tvars, cx, type_))
        }
    }
}

fn dump_normalized_prop_(
    depth: u32,
    tvars: &mut BTreeSet<i32>,
    cx: &Context,
    p: &PropertyType,
) -> String {
    let kid_opt = |t: &Option<Type>, tvars: &mut BTreeSet<i32>| -> String {
        match t {
            Some(t) => dump_t_(depth, tvars, cx, t),
            None => "None".to_string(),
        }
    };

    match p {
        PropertyType::OrdinaryField { type_, polarity } => {
            format!(
                "OrdinaryField ({}) {}",
                polarity.string(),
                dump_t_(depth, tvars, cx, type_)
            )
        }
        PropertyType::SyntheticField { get_type, set_type } => {
            format!(
                "SyntheticField({}, {})",
                kid_opt(get_type, tvars),
                kid_opt(set_type, tvars)
            )
        }
    }
}

// This is the type-dump debugging API.
//  We should make sure these are not called recursively to avoid circumventing
// one of the termination mechanisms: depth or tvar-set.

pub fn dump_t(depth: Option<u32>, cx: &Context, t: &Type) -> String {
    dump_t_(depth.unwrap_or(3), &mut BTreeSet::new(), cx, t)
}

pub fn dump_use_t<CX>(depth: Option<u32>, cx: &Context, use_t: &UseT<CX>) -> String {
    dump_use_t_(depth.unwrap_or(3), &mut BTreeSet::new(), cx, use_t)
}

pub fn dump_prop(depth: Option<u32>, cx: &Context, prop: &Property) -> String {
    dump_prop_(depth.unwrap_or(3), &mut BTreeSet::new(), cx, prop)
}

pub fn dump_normalized_prop(depth: Option<u32>, cx: &Context, prop: &PropertyType) -> String {
    dump_normalized_prop_(depth.unwrap_or(3), &mut BTreeSet::new(), cx, prop)
}

pub fn dump_tvar(depth: Option<u32>, cx: &Context, id: i32) -> String {
    dump_tvar_(depth.unwrap_or(3), &mut BTreeSet::new(), cx, id)
}

pub fn dump_flow<CX>(depth: Option<u32>, cx: &Context, t: &Type, use_t: &UseT<CX>) -> String {
    let depth = depth.unwrap_or(3);
    format!(
        "Lower: {} ~>\n Upper: {}",
        dump_t(Some(depth), cx, t),
        dump_use_t(Some(depth), cx, use_t)
    )
}

// types

pub fn string_of_reason(cx: &Context, reason: &Reason) -> String {
    let strip_root = if cx.should_strip_root() {
        Some(cx.root().to_string_lossy().to_string())
    } else {
        None
    };
    reason::string_of_reason(strip_root.as_deref(), reason)
}

pub fn string_of_file(cx: &Context) -> String {
    let filename = cx.file().to_absolute();
    match cx.is_verbose() {
        false => filename,
        true => {
            let root_str = format!(
                "{}{}",
                cx.root().to_string_lossy(),
                std::path::MAIN_SEPARATOR
            );
            if filename.starts_with(&root_str) {
                flow_common::files::relative_path(std::path::Path::new(&root_str), &filename)
            } else {
                filename
            }
        }
    }
}

pub fn string_of_default<L: Clone + std::fmt::Debug>(default: &Default<L>) -> String {
    flow_typing_default::fold(
        default,
        &|loc| format!("Expr {:?}", loc),
        &|str1, str2| format!("Cons ({}) ({})", str1, str2),
        &|_, str, sel| format!("Selector ({}) ({})", str, string_of_selector(&sel)),
    )
}

pub fn string_of_signature_error<L>(
    pp_loc: impl Fn(&L) -> String,
    err: &SignatureError<L>,
) -> String {
    match err {
        SignatureError::ExpectedAnnotation(loc, sort) => {
            format!("Expected annotation at {} @ {}", sort, pp_loc(loc))
        }
        SignatureError::UnexpectedObjectKey(_loc, key_loc) => {
            format!("Expected simple object key @ {}", pp_loc(key_loc))
        }
        SignatureError::UnexpectedArraySpread(_loc, spread_loc) => {
            format!("Unexpected array spread @ {}", pp_loc(spread_loc))
        }
        SignatureError::UnexpectedArrayHole(loc) => {
            format!("Unexpected array hole @ {}", pp_loc(loc))
        }
        SignatureError::EmptyArray(loc) => {
            format!(
                "Cannot determine the element type of an empty array @ {}",
                pp_loc(loc)
            )
        }
        SignatureError::EmptyObject(loc) => {
            format!(
                "Cannot determine types of initialized properties of an empty object @ {}",
                pp_loc(loc)
            )
        }
        SignatureError::UnexpectedExpression(loc, esort) => {
            format!(
                "Cannot determine the type of this {:?} @ {}",
                esort,
                pp_loc(loc)
            )
        }
    }
}

pub fn dump_error_message(cx: &Context, err: &ErrorMessage<ALoc>) -> String {
    fn dump_internal_error(err: &InternalError) -> &'static str {
        match err {
            InternalError::ReadOfUnreachedTvar(_) => "ReadOfUnreachedTvar",
            InternalError::ReadOfUnresolvedTvar(_) => "ReadOfUnresolvedTvar",
            InternalError::ForcedReadOfUnderResolutionTvar(_) => "ForcedReadOfUnderResolutionTvar",
            InternalError::MethodNotAFunction => "MethodNotAFunction",
            InternalError::OptionalMethod => "OptionalMethod",
            InternalError::PropertyDescriptorPropertyCannotBeRead => {
                "PropertyDescriptorPropertyCannotBeRead"
            }
            InternalError::ForInLHS => "ForInLHS",
            InternalError::ForOfLHS => "ForOfLHS",
            InternalError::PropRefComputedOpen => "PropRefComputedOpen",
            InternalError::PropRefComputedLiteral => "PropRefComputedLiteral",
            InternalError::RestParameterNotIdentifierPattern => "RestParameterNotIdentifierPattern",
            InternalError::InterfaceTypeSpread => "InterfaceTypeSpread",
            InternalError::DebugThrow => "DebugThrow",
            InternalError::ParseJobException(_) => "ParseJobException",
            InternalError::CheckTimeout(_) => "CheckTimeout",
            InternalError::CheckJobException(_) => "CheckJobException",
            InternalError::UnexpectedAnnotationInference(_) => "UnexpectedAnnotationInference",
            InternalError::MissingSwitchExhaustiveCheck => "MissingSwitchExhaustiveCheck",
            InternalError::MissingEnvRead(_) => "MissingEnvRead",
            InternalError::MissingEnvWrite(_) => "MissingEnvWrite",
            InternalError::EnvInvariant(_) => "EnvInvariant",
            InternalError::ImplicitInstantiationInvariant(_) => "ImplicitInstantiationInvariant",
        }
    }

    fn dump_upper_kind<L: LocSig>(upper_kind: &UpperKind<L>) -> String {
        match upper_kind {
            UpperKind::IncompatibleGetPropT(_, _) => "IncompatibleGetPropT".to_string(),
            UpperKind::IncompatibleSetPropT(_, _) => "IncompatibleSetPropT".to_string(),
            UpperKind::IncompatibleGetPrivatePropT => "IncompatibleGetPrivatePropT".to_string(),
            UpperKind::IncompatibleSetPrivatePropT => "IncompatibleSetPrivatePropT".to_string(),
            UpperKind::IncompatibleMethodT(_, _) => "IncompatibleMethodT".to_string(),
            UpperKind::IncompatibleCallT => "IncompatibleCallT".to_string(),
            UpperKind::IncompatibleMixedCallT => "IncompatibleMixedCallT".to_string(),
            UpperKind::IncompatibleGetElemT(_) => "IncompatibleGetElemT".to_string(),
            UpperKind::IncompatibleSetElemT(_) => "IncompatibleSetElemT".to_string(),
            UpperKind::IncompatibleCallElemT(_) => "IncompatibleCallElemT".to_string(),
            UpperKind::IncompatibleElemTOfArrT => "IncompatibleElemTOfArrT".to_string(),
            UpperKind::IncompatibleObjAssignFromTSpread => {
                "IncompatibleObjAssignFromTSpread".to_string()
            }
            UpperKind::IncompatibleObjAssignFromT => "IncompatibleObjAssignFromT".to_string(),
            UpperKind::IncompatibleObjRestT => "IncompatibleObjRestT".to_string(),
            UpperKind::IncompatibleArrRestT => "IncompatibleArrRestT".to_string(),
            UpperKind::IncompatibleSuperT => "IncompatibleSuperT".to_string(),
            UpperKind::IncompatibleMixinT => "IncompatibleMixinT".to_string(),
            UpperKind::IncompatibleSpecializeT => "IncompatibleSpecializeT".to_string(),
            UpperKind::IncompatibleThisSpecializeT => "IncompatibleThisSpecializeT".to_string(),
            UpperKind::IncompatibleVarianceCheckT => "IncompatibleVarianceCheckT".to_string(),
            UpperKind::IncompatibleGetKeysT => "IncompatibleGetKeysT".to_string(),
            UpperKind::IncompatibleHasOwnPropT(_, _) => "IncompatibleHasOwnPropT".to_string(),
            UpperKind::IncompatibleGetValuesT => "IncompatibleGetValuesT".to_string(),
            UpperKind::IncompatibleMapTypeTObject => "IncompatibleMapTypeTObject".to_string(),
            UpperKind::IncompatibleGetStaticsT => "IncompatibleGetStaticsT".to_string(),
            UpperKind::IncompatibleBindT => "IncompatibleBindT".to_string(),
            UpperKind::IncompatibleUnclassified(ctor) => {
                format!("IncompatibleUnclassified {:?}", ctor)
            }
        }
    }

    match err {
        ErrorMessage::EIncompatible(box EIncompatibleData {
            lower,
            upper,
            use_op,
        }) => {
            let (reason_lower, _lower_kind) = lower;
            let (reason_upper, upper_kind) = upper;
            format!(
                "EIncompatible(Box::new(EIncompatibleData {{ lower = ({}, _); upper = ({}, {}); use_op = {}; branches = _ }}))",
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                dump_upper_kind(upper_kind),
                match use_op {
                    None => "None".to_string(),
                    Some(use_op) => format!("Some({})", string_of_use_op(use_op)),
                }
            )
        }
        ErrorMessage::EIncompatibleSpeculation(box EIncompatibleSpeculationData {
            loc,
            use_op,
            branches: _,
        }) => {
            format!(
                "EIncompatibleSpeculation {{ upper = {}; use_op = {}; branches = _ }}",
                string_of_aloc(None, loc),
                match use_op {
                    None => "None".to_string(),
                    Some(use_op) => format!("Some({})", string_of_use_op(use_op)),
                }
            )
        }
        ErrorMessage::EIncompatibleDefs(box EIncompatibleDefsData {
            use_op,
            reason_lower,
            reason_upper,
            branches: _,
        }) => {
            format!(
                "EIncompatibleDefs {{ reason_lower = {}; reason_upper = {}; use_op = {}; branches = _ }}",
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EIncompatibleProp(box EIncompatiblePropData {
            reason_prop,
            reason_obj,
            special: _,
            prop: _,
            use_op: _,
        }) => {
            format!(
                "EIncompatibleProp(Box::new(EIncompatiblePropData {{ reason_prop = {}; reason_obj = {}; special = _; prop = _; use_op = _ }}))",
                dump_reason(cx, reason_prop),
                dump_reason(cx, reason_obj)
            )
        }
        ErrorMessage::EExportValueAsType(box (reason, name)) => {
            format!(
                "EExportValueAsType(Box::new(({}, {})))",
                dump_reason(cx, reason),
                name.as_str()
            )
        }
        ErrorMessage::EImportValueAsType(box (reason, s)) => {
            format!(
                "EImportValueAsType(Box::new(({}, {})))",
                dump_reason(cx, reason),
                s
            )
        }
        ErrorMessage::EImportTypeAsTypeof(box (reason, s)) => {
            format!(
                "EImportTypeAsTypeof(Box::new(({}, {})))",
                dump_reason(cx, reason),
                s
            )
        }
        ErrorMessage::EImportTypeAsValue(box (reason, s)) => {
            format!(
                "EImportTypeAsValue(Box::new(({}, {})))",
                dump_reason(cx, reason),
                s
            )
        }
        ErrorMessage::ENoDefaultExport(box (reason, module_name, _)) => {
            format!(
                "ENoDefaultExport(Box::new(({}, {})))",
                dump_reason(cx, reason),
                module_name.display()
            )
        }
        ErrorMessage::EOnlyDefaultExport(box (reason, module_name, export_name)) => {
            format!(
                "EOnlyDefaultExport(Box::new(({}, {}, {})))",
                dump_reason(cx, reason),
                module_name.display(),
                export_name
            )
        }
        ErrorMessage::ENoNamedExport(box (reason, module_name, export_name, _)) => {
            format!(
                "ENoNamedExport(Box::new(({}, {}, {})))",
                dump_reason(cx, reason),
                module_name.display(),
                export_name
            )
        }
        ErrorMessage::EMissingTypeArgs(box EMissingTypeArgsData {
            reason_op,
            reason_tapp,
            arity_loc,
            min_arity,
            max_arity,
        }) => {
            format!(
                "EMissingTypeArgs(Box::new(EMissingTypeArgsData {{ reason_op={}; reason_tapp={}; reason_arity={}; min_arity={}; max_arity={} }}))",
                dump_reason(cx, reason_op),
                dump_reason(cx, reason_tapp),
                string_of_aloc(None, arity_loc),
                min_arity,
                max_arity
            )
        }
        ErrorMessage::EAnyValueUsedAsType { reason_use } => {
            format!(
                "EAnyValueUsedAsType {{ use = {} }}",
                dump_reason(cx, reason_use)
            )
        }
        ErrorMessage::EValueUsedAsType { reason_use } => {
            format!(
                "EValueUsedAsType {{ use = {} }}",
                dump_reason(cx, reason_use)
            )
        }
        ErrorMessage::EExpectedStringLit(box EExpectedStringLitData {
            reason_lower,
            reason_upper,
            use_op,
        }) => {
            format!(
                "EExpectedStringLit(Box::new(EExpectedStringLitData {{ reason_lower = {}; reason_upper = {}; use_op = {} }}))",
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EExpectedNumberLit(box EExpectedNumberLitData {
            reason_lower,
            reason_upper,
            use_op,
        }) => {
            format!(
                "EExpectedNumberLit(Box::new(EExpectedNumberLitData {{ reason_lower = {}; reason_upper = {}; use_op = {} }}))",
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EExpectedBooleanLit(box EExpectedBooleanLitData {
            reason_lower,
            reason_upper,
            use_op,
        }) => {
            format!(
                "EExpectedBooleanLit(Box::new(EExpectedBooleanLitData {{ reason_lower = {}; reason_upper = {}; use_op = {} }}))",
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EExpectedBigIntLit(box EExpectedBigIntLitData {
            reason_lower,
            reason_upper,
            use_op,
        }) => {
            format!(
                "EExpectedBigIntLit(Box::new(EExpectedBigIntLitData {{ reason_lower = {}; reason_upper = {}; use_op = {} }}))",
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EPropNotFoundInLookup(box EPropNotFoundInLookupData {
            prop_name,
            reason_prop,
            reason_obj,
            use_op,
            suggestion,
        }) => {
            let prop_str = match prop_name {
                Some(prop) => format!("Some {}", prop.as_str()),
                None => "None".to_string(),
            };
            let suggestion_str = match suggestion {
                Some(s) => format!("Some {}", s),
                None => "None".to_string(),
            };
            format!(
                "EPropNotFoundInLookup ({}, {}, {}, {}, {})",
                prop_str,
                dump_reason(cx, reason_prop),
                dump_reason(cx, reason_obj),
                string_of_use_op(use_op),
                suggestion_str
            )
        }
        ErrorMessage::EPropNotFoundInSubtyping(box EPropNotFoundInSubtypingData {
            prop_name,
            reason_lower,
            reason_upper,
            use_op,
            suggestion,
        }) => {
            let prop_str = match prop_name {
                Some(prop) => format!("Some {}", prop.as_str()),
                None => "None".to_string(),
            };
            let suggestion_str = match suggestion {
                Some(s) => format!("Some {}", s),
                None => "None".to_string(),
            };
            format!(
                "EPropNotFoundInSubtyping ({}, {}, {}, {}, {})",
                prop_str,
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op),
                suggestion_str
            )
        }
        ErrorMessage::EPropsNotFoundInSubtyping(box EPropsNotFoundInSubtypingData {
            prop_names,
            reason_lower,
            reason_upper,
            use_op,
        }) => {
            let names: Vec<String> = prop_names.iter().map(|n| n.as_str().to_owned()).collect();
            format!(
                "EPropsNotFoundInSubtyping ([{}], {}, {}, {})",
                names.join(","),
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EPropsNotFoundInInvariantSubtyping(
            box EPropsNotFoundInInvariantSubtypingData {
                prop_names,
                reason_lower,
                reason_upper,
                lower_obj_loc,
                upper_obj_loc,
                lower_obj_desc: _,
                upper_obj_desc: _,
                use_op,
            },
        ) => {
            let names: Vec<String> = prop_names.iter().map(|n| n.as_str().to_owned()).collect();
            format!(
                "EPropsNotFoundInSubtyping ([{}], {}, {}, {}, {}, {})",
                names.join(","),
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_aloc(None, lower_obj_loc),
                string_of_aloc(None, upper_obj_loc),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EIndexerCheckFailed(box EIndexerCheckFailedData {
            prop_name,
            reason_lower,
            reason_upper,
            reason_indexer,
            use_op,
        }) => {
            format!(
                "EIndexerCheckFailed ({}, {}, {}, {}, {})",
                prop_name.as_str(),
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                dump_reason(cx, reason_indexer),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EPropsExtraAgainstExactObject(box EPropsExtraAgainstExactObjectData {
            prop_names,
            reason_l_obj,
            reason_r_obj,
            use_op,
        }) => {
            let names: Vec<String> = prop_names.iter().map(|n| n.as_str().to_owned()).collect();
            format!(
                "EPropsExtraAgainstExactObject ([{}], {}, {}, {})",
                names.join(", "),
                dump_reason(cx, reason_l_obj),
                dump_reason(cx, reason_r_obj),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EPropNotReadable(box EPropNotReadableData {
            reason_prop,
            prop_name,
            use_op,
        }) => {
            let prop_str = match prop_name {
                Some(x) => format!("{:?}", x.as_str()),
                None => "(computed)".to_string(),
            };
            format!(
                "EPropNotReadable(Box::new(EPropNotReadableData {{ reason_prop = {}; prop_name = {}; use_op = {} }}))",
                dump_reason(cx, reason_prop),
                prop_str,
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EPropNotWritable(box EPropNotWritableData {
            reason_prop,
            prop_name,
            use_op,
        }) => {
            let prop_str = match prop_name {
                Some(x) => format!("{:?}", x.as_str()),
                None => "(computed)".to_string(),
            };
            format!(
                "EPropNotWritable(Box::new(EPropNotWritableData {{ reason_prop = {}; prop_name = {}; use_op = {} }}))",
                dump_reason(cx, reason_prop),
                prop_str,
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EPropPolarityMismatch(box EPropPolarityMismatchData {
            lreason,
            ureason,
            props,
            use_op: _,
        }) => {
            let props_str: Vec<String> = props
                .iter()
                .map(|(x, _)| match x {
                    Some(x) => format!("{:?}", x.as_str()),
                    None => "(computed)".to_string(),
                })
                .collect();
            format!(
                "EPropPolarityMismatch (({}, {}), {}, _, _)",
                dump_reason(cx, lreason),
                dump_reason(cx, ureason),
                props_str.join(", ")
            )
        }
        ErrorMessage::EPolarityMismatch(box EPolarityMismatchData {
            reason,
            name,
            expected_polarity,
            actual_polarity,
        }) => {
            format!(
                "EPolarityMismatch(Box::new(EPolarityMismatchData {{ reason={}; name={:?}; expected_polarity={}; actual_polarity={} }}))",
                dump_reason(cx, reason),
                name,
                expected_polarity.string(),
                actual_polarity.string()
            )
        }
        ErrorMessage::EBuiltinNameLookupFailed(box EBuiltinNameLookupFailedData { loc, name }) => {
            format!(
                "EBuiltinNameLookupFailed(Box::new(EBuiltinNameLookupFailedData {{ loc = {}; name = {:?} }}))",
                string_of_aloc(None, loc),
                name
            )
        }
        ErrorMessage::EBuiltinModuleLookupFailed(box EBuiltinModuleLookupFailedData {
            loc,
            name,
            potential_generator,
        }) => {
            let gen_str = match potential_generator {
                Some(g) => format!("Some({})", g),
                None => "None".to_string(),
            };
            format!(
                "EBuiltinModuleLookupFailed(Box::new(EBuiltinModuleLookupFailedData {{ loc = {}; name = {:?}; potential_generator = {} }}))",
                string_of_aloc(None, loc),
                name,
                gen_str
            )
        }
        ErrorMessage::EExpectedModuleLookupFailed(box EExpectedModuleLookupFailedData {
            loc,
            name,
            expected_module_purpose: _,
        }) => {
            format!(
                "EExpectedModuleLookupFailed(Box::new(EExpectedModuleLookupFailedData {{ loc = {}; name = {:?} }}))",
                string_of_aloc(None, loc),
                name
            )
        }
        ErrorMessage::EPrivateLookupFailed(box ((reason1, reason2), x, use_op)) => {
            format!(
                "EPrivateLookupFailed(Box::new(({}, {}), {}, {}))",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2),
                x.as_str(),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EPlatformSpecificImplementationModuleLookupFailed(
            box EPlatformSpecificImplementationModuleLookupFailedData { loc: _, name },
        ) => {
            format!(
                "EPlatformSpecificImplementationModuleLookupFailed({})",
                name
            )
        }
        ErrorMessage::EComparison(box EComparisonData {
            r1,
            r2,
            loc_opt,
            strict_comparison_opt,
        }) => {
            let loc_str = match loc_opt {
                Some(loc) => string_of_aloc(None, loc),
                None => "None".to_string(),
            };
            let strict_str = match strict_comparison_opt {
                Some(_) => "Some(...)".to_string(),
                None => "None".to_string(),
            };
            format!(
                "EComparison ({}, {}, {}, {})",
                dump_reason(cx, r1),
                dump_reason(cx, r2),
                loc_str,
                strict_str
            )
        }
        ErrorMessage::ENonStrictEqualityComparison(box (reason1, reason2)) => {
            format!(
                "ENonStrictEqualityComparison(Box::new(({}, {})))",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2)
            )
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
            let (num_req1, num_total1) = lower_arity;
            let (num_req2, num_total2) = upper_arity;
            format!(
                "ETupleArityMismatch ({}, {}, {}-{} inexact:{}, {}-{} inexact:{}, unify:{}, {})",
                dump_reason(cx, lower_reason),
                dump_reason(cx, upper_reason),
                num_req1,
                num_total1,
                lower_inexact,
                num_req2,
                num_total2,
                upper_inexact,
                unify,
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ENonLitArrayToTuple((reason1, reason2), use_op) => {
            format!(
                "ENonLitArrayToTuple (({}, {}), {})",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ETupleRequiredAfterOptional(box ETupleRequiredAfterOptionalData {
            reason_tuple,
            reason_required,
            reason_optional,
        }) => {
            format!(
                "ETupleRequiredAfterOptional(Box::new(ETupleRequiredAfterOptionalData {{reason_tuple = {}; reason_required = {}; reason_optional = {}}}))",
                dump_reason(cx, reason_tuple),
                dump_reason(cx, reason_required),
                dump_reason(cx, reason_optional)
            )
        }
        ErrorMessage::ETupleInvalidTypeSpread(box ETupleInvalidTypeSpreadData {
            reason_spread,
            reason_arg,
        }) => {
            format!(
                "ETupleInvalidTypeSpread(Box::new(ETupleInvalidTypeSpreadData {{reason_spread = {}; reason_arg = {}}}))",
                dump_reason(cx, reason_spread),
                dump_reason(cx, reason_arg)
            )
        }
        ErrorMessage::ETupleOutOfBounds(box ETupleOutOfBoundsData {
            use_op,
            reason,
            reason_op,
            inexact,
            length,
            index,
        }) => {
            format!(
                "ETupleOutOfBounds(Box::new(ETupleOutOfBoundsData {{ use_op = {}; reason = {}; reason_op = {}; inexact = {}; length = {}; index = {} }}))",
                string_of_use_op(use_op),
                dump_reason(cx, reason),
                dump_reason(cx, reason_op),
                inexact,
                length,
                index
            )
        }
        ErrorMessage::ETupleNonIntegerIndex(box ETupleNonIntegerIndexData {
            use_op,
            reason,
            index,
        }) => {
            format!(
                "ETupleNonIntegerIndex(Box::new(ETupleNonIntegerIndexData {{ use_op = {}; reason = {}; index = {} }}))",
                string_of_use_op(use_op),
                dump_reason(cx, reason),
                index
            )
        }
        ErrorMessage::ETupleUnsafeWrite { reason, use_op } => {
            format!(
                "ETupleUnsafeWrite {{ reason = {}; use_op = {} }}",
                dump_reason(cx, reason),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ETupleElementNotReadable(box ETupleElementNotReadableData {
            reason,
            index,
            name: _,
            use_op,
        }) => {
            format!(
                "ETupleElementNotReadable(Box::new(ETupleElementNotReadableData {{ reason = {}; index = {}; use_op = {} }}))",
                dump_reason(cx, reason),
                index,
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ETupleElementNotWritable(box ETupleElementNotWritableData {
            reason,
            index,
            name: _,
            use_op,
        }) => {
            format!(
                "ETupleElementNotWritable(Box::new(ETupleElementNotWritableData {{ reason = {}; index = {}; use_op = {} }}))",
                dump_reason(cx, reason),
                index,
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ETupleElementPolarityMismatch(box ETupleElementPolarityMismatchData {
            index,
            reason_lower,
            polarity_lower,
            reason_upper,
            polarity_upper,
            use_op,
        }) => {
            format!(
                "ETupleElementPolarityMismatch(Box::new(ETupleElementPolarityMismatchData {{ index = {}; reason_lower = {}; polarity_lower = {}; reason_upper = {}; polarity_upper = {}; use_op = {} }}))",
                index,
                dump_reason(cx, reason_lower),
                polarity_lower.string(),
                dump_reason(cx, reason_upper),
                polarity_upper.string(),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ETupleElementAfterInexactSpread(reason) => {
            format!(
                "ETupleElementAfterInexactSpread ({})",
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EROArrayWrite((reason1, reason2), use_op) => {
            format!(
                "EROArrayWrite ({}, {}, {})",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EUnionSpeculationFailed(box EUnionSpeculationFailedData {
            use_op,
            reason,
            op_reasons,
            branches: _,
        }) => {
            let op_reasons_str: Vec<String> =
                op_reasons.iter().map(|r| dump_reason(cx, r)).collect();
            format!(
                "EUnionSpeculationFailed {{ use_op = {}; reason = {}; op_reasons = [{}]; branches = _ }}",
                string_of_use_op(use_op),
                dump_reason(cx, reason),
                op_reasons_str.join(",")
            )
        }
        ErrorMessage::EIncompatibleWithExact((reason1, reason2), use_op, _) => {
            format!(
                "EIncompatibleWithExact (({}, {}), {})",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EFunctionIncompatibleWithIndexer((reason1, reason2), use_op) => {
            format!(
                "EFunctionIncompatibleWithIndexer(({}, {}), {})",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EUnsupportedExact(box (reason1, reason2)) => {
            format!(
                "EUnsupportedExact(Box::new(({}, {})))",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2)
            )
        }
        ErrorMessage::EUnexpectedThisType(loc) => {
            format!("EUnexpectedThisType ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::ETypeParamArity(loc, expected) => {
            format!(
                "ETypeParamArity ({}, {})",
                string_of_aloc(None, loc),
                expected
            )
        }
        ErrorMessage::ETypeParamMinArity(loc, expected) => {
            format!(
                "ETypeParamMinArity ({}, {})",
                string_of_aloc(None, loc),
                expected
            )
        }
        ErrorMessage::ECallTypeArity(box ECallTypeArityData {
            call_loc,
            is_new,
            reason_arity,
            expected_arity,
        }) => {
            format!(
                "ECallTypeArity(Box::new(ECallTypeArityData {{ call_loc={}; is_new={}; reason_arity={}; expected_arity={}; }}))",
                string_of_aloc(None, call_loc),
                is_new,
                dump_reason(cx, reason_arity),
                expected_arity
            )
        }
        ErrorMessage::ETooManyTypeArgs(box ETooManyTypeArgsData {
            reason_tapp,
            arity_loc,
            maximum_arity,
        }) => {
            format!(
                "ETooManyTypeArgs ({}, {}, {})",
                dump_reason(cx, reason_tapp),
                string_of_aloc(None, arity_loc),
                maximum_arity
            )
        }
        ErrorMessage::ETooFewTypeArgs(box ETooFewTypeArgsData {
            reason_tapp,
            arity_loc,
            minimum_arity,
        }) => {
            format!(
                "ETooFewTypeArgs ({}, {}, {})",
                dump_reason(cx, reason_tapp),
                string_of_aloc(None, arity_loc),
                minimum_arity
            )
        }
        ErrorMessage::EInvalidTypeArgs(box (reason_tapp, reason_arity)) => {
            format!(
                "EInvalidTypeArgs(Box::new(({}, {})))",
                dump_reason(cx, reason_tapp),
                dump_reason(cx, reason_arity)
            )
        }
        ErrorMessage::EInvalidReactCreateElement(box EInvalidReactCreateElementData {
            create_element_loc,
            invalid_react: _,
        }) => {
            format!(
                "EInvalidReactCreateElement({})",
                string_of_aloc(None, create_element_loc)
            )
        }
        ErrorMessage::EInvalidInfer(loc) => {
            format!("EInvalidInfer ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EInvalidExtends(reason) => {
            format!("EInvalidExtends ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EStrUtilTypeNonLiteralArg(loc) => {
            format!("EStrUtilTypeNonLiteralArg ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EExportsAnnot(loc) => {
            format!("EExportsAnnot ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EInvalidRendersTypeArgument(box EInvalidRendersTypeArgumentData {
            loc,
            renders_variant,
            invalid_render_type_kind,
            invalid_type_reasons,
        }) => {
            let variant_str = match renders_variant {
                flow_parser::ast::types::RendersVariant::Normal => "renders",
                flow_parser::ast::types::RendersVariant::Maybe => "renders?",
                flow_parser::ast::types::RendersVariant::Star => "renders*",
            };
            let kind_str = string_of_invalid_render_type_kind(invalid_render_type_kind);
            let reasons: Vec<String> = invalid_type_reasons
                .iter()
                .map(|r| dump_reason(cx, r))
                .collect();
            format!(
                "EInvalidRendersTypeArgument(Box::new(EInvalidRendersTypeArgumentData {{ loc = {}; renders_variant = {}, invalid_render_type_kind = {}; invalid_type_reasons = [{}] }}))",
                string_of_aloc(None, loc),
                variant_str,
                kind_str,
                reasons.join(", ")
            )
        }
        ErrorMessage::EUnsupportedKeyInObject {
            loc,
            obj_kind,
            key_error_kind,
        } => {
            let obj_kind_str = match obj_kind {
                IntermediateObjKind::Type => "object type",
                IntermediateObjKind::Literal => "object literal",
                IntermediateObjKind::Interface => "interface",
                IntermediateObjKind::DeclareClass => "declare class",
            };
            format!(
                "EUnsupportedKeyInObject ({}, {}, {})",
                string_of_aloc(None, loc),
                obj_kind_str,
                key_error_kind.str_of_kind()
            )
        }
        ErrorMessage::EAmbiguousNumericKeyWithVariance(loc) => {
            format!(
                "EAmbiguousNumericKeyWithVariance ({})",
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::ETypeGuardFuncIncompatibility { use_op, reasons: _ } => {
            format!(
                "ETypeGuardFuncIncompatibility ({})",
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ETypeGuardInvalidParameter(box ETypeGuardInvalidParameterData {
            type_guard_reason,
            binding_reason: _,
        }) => {
            format!(
                "ETypeGuardInvalidParameter ({})",
                dump_reason(cx, type_guard_reason)
            )
        }
        ErrorMessage::ETypeGuardIndexMismatch { use_op, reasons: _ } => {
            format!("ETypeGuardIndexMismatch ({})", string_of_use_op(use_op))
        }
        ErrorMessage::ETypeGuardImpliesMismatch { use_op, reasons: _ } => {
            format!("ETypeGuardImpliesMismatch ({})", string_of_use_op(use_op))
        }
        ErrorMessage::ETypeGuardParamUnbound(_) => "ETypeGuardParamUnbound".to_string(),
        ErrorMessage::ETypeGuardThisParam(_) => "ETypeGuardThisParam".to_string(),
        ErrorMessage::ETypeGuardFunctionInvalidWrites(
            box ETypeGuardFunctionInvalidWritesData { .. },
        ) => "ETypeGuardFunctionInvalidWrites".to_string(),
        ErrorMessage::ETypeGuardFunctionParamHavoced(box ETypeGuardFunctionParamHavocedData {
            ..
        }) => "ETypeGuardFunctionParamHavoced".to_string(),
        ErrorMessage::ETypeGuardIncompatibleWithFunctionKind(
            box ETypeGuardIncompatibleWithFunctionKindData { .. },
        ) => "ETypeGuardIncompatibleWithFunctionKind".to_string(),
        ErrorMessage::EInternal(box (loc, err)) => {
            format!(
                "EInternal(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                dump_internal_error(err)
            )
        }
        ErrorMessage::EUnsupportedSyntax(box (loc, _)) => {
            format!(
                "EUnsupportedSyntax(Box::new(({}, _)))",
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::EUseArrayLiteral(loc) => {
            format!("EUseArrayLiteral ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EMissingLocalAnnotation {
            reason,
            hint_available: _,
            from_generic_function: _,
        } => {
            format!("EMissingLocalAnnotation ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EBindingError(box (_binding_error, loc, x, entry_loc)) => {
            format!(
                "EBindingError(Box::new((_, {}, {}, {})))",
                string_of_aloc(None, loc),
                x.as_str(),
                string_of_aloc(None, entry_loc)
            )
        }
        ErrorMessage::ERecursionLimit(box (reason1, reason2)) => {
            format!(
                "ERecursionLimit(Box::new(({}, {})))",
                dump_reason(cx, reason1),
                dump_reason(cx, reason2)
            )
        }
        ErrorMessage::EUninitializedInstanceProperty(loc, err) => {
            let err_str = match err {
                PropertyAssignmentKind::PropertyNotDefinitelyInitialized => {
                    "PropertyNotDefinitelyInitialized"
                }
                PropertyAssignmentKind::ReadFromUninitializedProperty => {
                    "ReadFromUninitializedProperty"
                }
                PropertyAssignmentKind::MethodCallBeforeEverythingInitialized => {
                    "MethodCallBeforeEverythingInitialized"
                }
                PropertyAssignmentKind::PropertyFunctionCallBeforeEverythingInitialized => {
                    "PropertyFunctionCallBeforeEverythingInitialized"
                }
                PropertyAssignmentKind::ThisBeforeEverythingInitialized => {
                    "ThisBeforeEverythingInitialized"
                }
            };
            format!(
                "EUninitializedInstanceProperty ({}, {})",
                string_of_aloc(None, loc),
                err_str
            )
        }
        ErrorMessage::EIndeterminateModuleType(loc) => {
            format!("EIndeterminateModuleType ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EBadExportPosition(loc) => {
            format!("EBadExportPosition ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EBadExportContext(box (name, loc)) => {
            format!(
                "EBadExportContext(Box::new(({}, {})))",
                name,
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::EBadDefaultImportAccess(box (loc, reason)) => {
            format!(
                "EBadDefaultImportAccess(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EBadDefaultImportDestructuring(loc) => {
            format!(
                "EBadDefaultImportDestructuring ({})",
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::EInvalidImportStarUse(box (loc, reason)) => {
            format!(
                "EInvalidImportStarUse(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::ENonConstVarExport(box (loc, reason)) => {
            let reason_str = match reason {
                Some(r) => dump_reason(cx, r),
                None => "None".to_string(),
            };
            format!(
                "ENonConstVarExport(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                reason_str
            )
        }
        ErrorMessage::EThisInExportedFunction(loc) => {
            format!("EThisInExportedFunction ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EMixedImportAndRequire(box (loc, reason)) => {
            format!(
                "EMixedImportAndRequire(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EUnsupportedVarianceAnnotation(box (loc, s)) => {
            format!(
                "EUnsupportedVarianceAnnotation(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                s
            )
        }
        ErrorMessage::EExportRenamedDefault(box EExportRenamedDefaultData {
            loc,
            name,
            is_reexport,
        }) => {
            let name_str = match name {
                Some(n) => n.to_string(),
                None => "None".to_string(),
            };
            format!(
                "EExportRenamedDefault(Box::new(EExportRenamedDefaultData {{ loc = {}; name = {}; is_reexport = {} }}))",
                string_of_aloc(None, loc),
                name_str,
                is_reexport
            )
        }
        ErrorMessage::EUnreachable(loc) => {
            format!("EUnreachable ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EInvalidObjectKit(box EInvalidObjectKitData {
            reason,
            reason_op,
            use_op,
        }) => {
            format!(
                "EInvalidObjectKit(Box::new(EInvalidObjectKitData {{ reason = {}; reason_op = {}; use_op = {} }}))",
                dump_reason(cx, reason),
                dump_reason(cx, reason_op),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EInvalidTypeof(box (loc, name)) => {
            format!(
                "EInvalidTypeof(Box::new(({}, {:?})))",
                string_of_aloc(None, loc),
                name
            )
        }
        ErrorMessage::EBinaryInLHS(reason) => {
            format!("EBinaryInLHS ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EBinaryInRHS(reason) => {
            format!("EBinaryInRHS ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EArithmeticOperand(reason) => {
            format!("EArithmeticOperand ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EForInRHS(reason) => {
            format!("EForInRHS ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EInstanceofRHS(reason) => {
            format!("EInstanceofRHS ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EObjectComputedPropertyAccess(box EObjectComputedPropertyAccessData {
            reason_obj,
            reason_prop,
            kind,
        }) => {
            format!(
                "EObjectComputedPropertyAccess (reason_obj={}, reason_prop={}, kind={})",
                dump_reason(cx, reason_obj),
                dump_reason(cx, reason_prop),
                kind.str_of_kind()
            )
        }
        ErrorMessage::EObjectComputedPropertyAssign(box (reason1, reason2, kind)) => {
            let reason2_str = match reason2 {
                Some(r) => dump_reason(cx, r),
                None => "none".to_string(),
            };
            format!(
                "EObjectComputedPropertyAssign(Box::new(({}, {}, {})))",
                dump_reason(cx, reason1),
                reason2_str,
                kind.str_of_kind()
            )
        }
        ErrorMessage::EObjectComputedPropertyPotentialOverwrite(
            box EObjectComputedPropertyPotentialOverwriteData { key_loc, .. },
        ) => {
            format!(
                "EObjectComputedPropertyPotentialOverwrite ({})",
                string_of_aloc(None, key_loc)
            )
        }
        ErrorMessage::EInvalidLHSInAssignment(loc) => {
            format!("EInvalidLHSInAssignment ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EIncompatibleWithUseOp(box EIncompatibleWithUseOpData {
            reason_lower,
            reason_upper,
            use_op,
            explanation: _,
        }) => {
            format!(
                "EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {{ reason_lower = {}; reason_upper = {}; use_op = {} }}))",
                dump_reason(cx, reason_lower),
                dump_reason(cx, reason_upper),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EInvariantSubtypingWithUseOp(box EInvariantSubtypingWithUseOpData {
            lower_loc,
            upper_loc,
            use_op,
            ..
        }) => {
            format!(
                "EInvariantSubtypingWithUseOp {{ lower_loc = {}; upper_loc = {};  use_op = {} }}",
                string_of_aloc(None, lower_loc),
                string_of_aloc(None, upper_loc),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EUnsupportedImplements(reason) => {
            format!("EUnsupportedImplements ({})", dump_reason(cx, reason))
        }
        ErrorMessage::ENotAReactComponent { reason, use_op } => {
            format!(
                "ENotAReactComponent {{ reason = {}; use_op = {} }}",
                dump_reason(cx, reason),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EReactElementFunArity(box (reason, _, _)) => {
            format!("EReactElementFunArity ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EFunctionCallExtraArg(box (
            unused_reason,
            def_reason,
            param_count,
            use_op,
        )) => {
            format!(
                "EFunctionCallExtraArg(Box::new(({}, {}, {}, {})))",
                dump_reason(cx, unused_reason),
                dump_reason(cx, def_reason),
                param_count,
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EUnsupportedSetProto(reason) => {
            format!("EUnsupportedSetProto ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EDuplicateModuleProvider(box EDuplicateModuleProviderData {
            module_name,
            provider,
            conflict,
        }) => {
            format!(
                "EDuplicateModuleProvider ({:?}, {}, {})",
                module_name,
                string_of_aloc(None, provider),
                string_of_aloc(None, conflict)
            )
        }
        ErrorMessage::EParseError(box (loc, _)) => {
            format!("EParseError(Box::new(({}, _)))", string_of_aloc(None, loc))
        }
        ErrorMessage::EDocblockError(box (loc, err)) => {
            let err_str = match err {
                DocblockError::MultipleFlowAttributes => "MultipleFlowAttributes",
                DocblockError::InvalidFlowMode(_) => "InvalidFlowMode",
                DocblockError::MultipleJSXAttributes => "MultipleJSXAttributes",
                DocblockError::InvalidJSXAttribute(_) => "InvalidJSXAttribute",
                DocblockError::MultipleJSXRuntimeAttributes => "MultipleJSXRuntimeAttributes",
                DocblockError::InvalidJSXRuntimeAttribute => "InvalidJSXRuntimeAttribute",
                DocblockError::InvalidSupportsPlatform(_) => "InvalidSupportsPlatform",
                DocblockError::DisallowedSupportsPlatform => "DisallowedSupportsPlatform",
            };
            format!(
                "EDocblockError(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                err_str
            )
        }
        ErrorMessage::EImplicitInexactObject(loc) => {
            format!("EImplicitInexactObject ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EAmbiguousObjectType(loc) => {
            format!("EAmbiguousObjectType ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUntypedTypeImport(box (loc, module_name)) => {
            format!(
                "EUntypedTypeImport(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                module_name.display()
            )
        }
        ErrorMessage::EUntypedImport(box (loc, module_name)) => {
            format!(
                "EUntypedImport(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                module_name.display()
            )
        }
        ErrorMessage::ENonstrictImport(loc) => {
            format!("ENonstrictImport ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EInternalType(loc, _) => {
            format!("EInternalType ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUnclearType(loc) => {
            format!("EUnclearType ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EDeprecatedBool(loc) => {
            format!("EDeprecatedBool ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EIncorrectTypeWithReplacement(box EIncorrectTypeWithReplacementData {
            loc,
            kind,
        }) => {
            format!(
                "EIncorrectTypeWithReplacement ({}) ({:?})",
                string_of_aloc(None, loc),
                kind
            )
        }
        ErrorMessage::EUnsafeGettersSetters(loc) => {
            format!("EUnclearGettersSetters ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUnsafeObjectAssign(loc) => {
            format!("EUnsafeObjectAssign ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUnusedSuppression(loc) => {
            format!("EUnusedSuppression ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::ECodelessSuppression(loc) => {
            format!("ECodelessSuppression ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::ELintSetting(box (loc, kind)) => {
            let kind_str = match kind {
                LintParseError::InvalidSetting => "Invalid_setting",
                LintParseError::MalformedArgument => "Malformed_argument",
                LintParseError::NakedComment => "Naked_comment",
                LintParseError::NonexistentRule => "Nonexistent_rule",
                LintParseError::OverwrittenArgument => "Overwritten_argument",
                LintParseError::RedundantArgument => "Redundant_argument",
            };
            format!(
                "ELintSetting(Box::new(({}, {})))",
                string_of_aloc(None, loc),
                kind_str
            )
        }
        ErrorMessage::ESketchyNullLint(box ESketchyNullLintData {
            kind,
            loc,
            null_loc,
            falsy_loc,
        }) => {
            let kind_str = match kind {
                SketchyNullKind::Bool => "SketchyNullBool",
                SketchyNullKind::String => "SketchyNullString",
                SketchyNullKind::Number => "SketchyNullNumber",
                SketchyNullKind::BigInt => "SketchyNullBigInt",
                SketchyNullKind::Mixed => "SketchyNullMixed",
                SketchyNullKind::EnumBool => "SketchyNullEnumBool",
                SketchyNullKind::EnumString => "SketchyNullEnumString",
                SketchyNullKind::EnumNumber => "SketchyNullEnumNumber",
                SketchyNullKind::EnumBigInt => "SketchyNullEnumBigInt",
            };
            format!(
                "ESketchyNullLint(Box::new(ESketchyNullLintData {{kind={}; loc={}; null_loc={}; falsy_loc={}}}))",
                kind_str,
                string_of_aloc(None, loc),
                string_of_aloc(None, null_loc),
                string_of_aloc(None, falsy_loc)
            )
        }
        ErrorMessage::ESketchyNumberLint(kind, reason) => {
            let kind_str = match kind {
                SketchyNumberKind::And => "SketchyNumberAnd",
            };
            format!(
                "ESketchyNumberLint ({}) ({})",
                kind_str,
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EInvalidConstructor(reason) => {
            format!("EInvalidConstructor ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EInvalidPrototype(box (loc, reason)) => {
            format!(
                "EInvalidPrototype ({}) ({})",
                string_of_aloc(None, loc),
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EUnnecessaryOptionalChain(box (loc, _)) => {
            format!("EUnnecessaryOptionalChain ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUnnecessaryInvariant(box (loc, _)) => {
            format!("EUnnecessaryInvariant ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUnnecessaryDeclareTypeOnlyExport(loc) => {
            format!(
                "EUnnecessaryDeclareTypeOnlyExport ({})",
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::ECannotDelete(box (l1, r1)) => {
            format!(
                "ECannotDelete(Box::new(({}, {})))",
                string_of_aloc(None, l1),
                dump_reason(cx, r1)
            )
        }
        ErrorMessage::ESignatureBindingValidation(validation) => match validation {
            BindingValidation::ModuleOverride {
                override_binding_loc,
                ..
            } => {
                format!(
                    "ESignatureBindingValidation (ModuleOverride {})",
                    string_of_aloc(None, override_binding_loc)
                )
            }
            BindingValidation::NameOverride {
                override_binding_loc,
                ..
            } => {
                format!(
                    "ESignatureBindingValidation (NameOverride {})",
                    string_of_aloc(None, override_binding_loc)
                )
            }
            BindingValidation::NamespacedNameAlreadyBound {
                invalid_binding_loc,
                ..
            } => {
                format!(
                    "ESignatureBindingValidation (NamespacedNameAlreadyBound {})",
                    string_of_aloc(None, invalid_binding_loc)
                )
            }
            BindingValidation::InterfaceMergePropertyConflict {
                current_binding_loc,
                existing_binding_loc,
                ..
            } => {
                format!(
                    "ESignatureBindingValidation (InterfaceMergePropertyConflict {} {})",
                    string_of_aloc(None, current_binding_loc),
                    string_of_aloc(None, existing_binding_loc)
                )
            }
            BindingValidation::InterfaceMergeTparamMismatch {
                current_binding_loc,
                existing_binding_loc,
                ..
            } => {
                format!(
                    "ESignatureBindingValidation (InterfaceMergeTparamMismatch {} {})",
                    string_of_aloc(None, current_binding_loc),
                    string_of_aloc(None, existing_binding_loc)
                )
            }
        },
        ErrorMessage::ESignatureVerification(sve) => {
            let msg = string_of_signature_error(|loc: &ALoc| string_of_aloc(None, loc), sve);
            format!("ESignatureVerification ({})", msg)
        }
        ErrorMessage::EPrimitiveAsInterface(box EPrimitiveAsInterfaceData {
            use_op,
            reason,
            interface_reason,
            kind: _,
        }) => {
            format!(
                "EPrimitiveAsInterface ({}) ({}) ({})",
                string_of_use_op(use_op),
                dump_reason(cx, reason),
                dump_reason(cx, interface_reason)
            )
        }
        ErrorMessage::ECannotSpreadInterface(box ECannotSpreadInterfaceData {
            spread_reason,
            interface_reason,
            use_op,
        }) => {
            format!(
                "ECannotSpreadInterface ({}) ({}) ({})",
                dump_reason(cx, spread_reason),
                dump_reason(cx, interface_reason),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::ECannotSpreadIndexerOnRight(box ECannotSpreadIndexerOnRightData {
            spread_reason,
            object_reason,
            key_reason,
            use_op,
        }) => {
            format!(
                "ECannotSpreadIndexerOnRight ({}) ({}) ({}) ({})",
                dump_reason(cx, spread_reason),
                dump_reason(cx, object_reason),
                dump_reason(cx, key_reason),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EUnableToSpread(box EUnableToSpreadData {
            spread_reason,
            object1_reason,
            object2_reason,
            propname,
            error_kind: _,
            use_op,
        }) => {
            format!(
                "EUnableToSpread ({}) ({}) ({}) ({}) ({})",
                dump_reason(cx, spread_reason),
                dump_reason(cx, object1_reason),
                dump_reason(cx, object2_reason),
                propname.as_str(),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EInexactMayOverwriteIndexer(box EInexactMayOverwriteIndexerData {
            spread_reason,
            key_reason,
            value_reason,
            object2_reason,
            use_op,
        }) => {
            format!(
                "EInexactMayOverwriteIndexer ({}) ({}) ({}) ({}) ({})",
                dump_reason(cx, spread_reason),
                dump_reason(cx, key_reason),
                dump_reason(cx, value_reason),
                dump_reason(cx, object2_reason),
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EExponentialSpread(box EExponentialSpreadData {
            reason,
            reasons_for_operand1,
            reasons_for_operand2,
        }) => {
            let format_reason_group = |g: &ExponentialSpreadReasonGroup<ALoc>| {
                let second = match &g.second_reason {
                    Some(r) => dump_reason(cx, r),
                    None => "None".to_string(),
                };
                format!("[{}; {}]", dump_reason(cx, &g.first_reason), second)
            };
            format!(
                "EExponentialSpread(Box::new(EExponentialSpreadData {})) ({}) ({})",
                dump_reason(cx, reason),
                format_reason_group(reasons_for_operand1),
                format_reason_group(reasons_for_operand2)
            )
        }
        ErrorMessage::EComputedPropertyWithUnion(reason) => {
            format!("EComputedPropertyWithUnion ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EEnumError(enum_error) => match enum_error {
            EnumErrorKind::EnumsNotEnabled(loc) => {
                format!(
                    "EEnumError (EnumsNotEnabled ({}))",
                    string_of_aloc(None, loc)
                )
            }
            EnumErrorKind::EnumConstNotSupported(loc) => {
                format!(
                    "EEnumError (EnumConstNotSupported ({}))",
                    string_of_aloc(None, loc)
                )
            }
            EnumErrorKind::EnumInvalidMemberAccess(box EnumInvalidMemberAccessData {
                member_name,
                suggestion,
                reason,
                enum_reason,
            }) => {
                let member_str = match member_name {
                    Some(n) => n.as_str(),
                    None => "<None>",
                };
                let suggestion_str = match suggestion {
                    Some(s) => s.as_str(),
                    None => "<None>",
                };
                format!(
                    "EEnumError (EnumInvalidMemberAccess ({}) ({}) ({}) ({}))",
                    member_str,
                    suggestion_str,
                    dump_reason(cx, reason),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumModification(box EnumModificationData { loc, enum_reason }) => {
                format!(
                    "EEnumError (EnumModification ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumMemberDuplicateValue(box EnumMemberDuplicateValueData {
                loc,
                prev_use_loc,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumMemberDuplicateValue ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    string_of_aloc(None, prev_use_loc),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumInvalidObjectUtilType(box EnumInvalidObjectUtilTypeData {
                reason,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumInvalidObjectUtilType ({}) ({}))",
                    dump_reason(cx, reason),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumInvalidObjectFunction(box EnumInvalidObjectFunctionData {
                reason,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumInvalidObjectFunction ({}) ({}))",
                    dump_reason(cx, reason),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumNotIterable { reason, for_in } => {
                format!(
                    "EEnumError (EnumNotIterable ({}) (for_in = {}))",
                    dump_reason(cx, reason),
                    for_in
                )
            }
            EnumErrorKind::EnumMemberAlreadyChecked(box EnumMemberAlreadyCheckedData {
                case_test_loc,
                prev_check_loc,
                enum_reason,
                member_name,
            }) => {
                format!(
                    "EEnumError (EnumMemberAlreadyChecked ({}) ({}) ({}) ({}))",
                    string_of_aloc(None, case_test_loc),
                    string_of_aloc(None, prev_check_loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumAllMembersAlreadyChecked(box EnumAllMembersAlreadyCheckedData {
                loc,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumAllMembersAlreadyChecked ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumNotAllChecked(box EnumNotAllCheckedData {
                reason,
                enum_reason,
                left_to_check,
                default_case_loc,
            }) => {
                let default_str = match default_case_loc {
                    Some(loc) => string_of_aloc(None, loc),
                    None => "<None>".to_string(),
                };
                format!(
                    "EEnumError (EnumNotAllChecked ({}) ({}) ({}) ({}))",
                    dump_reason(cx, reason),
                    dump_reason(cx, enum_reason),
                    left_to_check.join(", "),
                    default_str
                )
            }
            EnumErrorKind::EnumUnknownNotChecked(box EnumUnknownNotCheckedData {
                reason,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumUnknownNotChecked ({}) ({}))",
                    dump_reason(cx, reason),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumInvalidCheck(box EnumInvalidCheckData {
                loc,
                enum_reason,
                example_member,
                from_match,
            }) => {
                let member_str = match example_member {
                    Some(m) => m.to_string(),
                    None => "<None>".to_string(),
                };
                format!(
                    "EEnumError (EnumInvalidCheck ({}) ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason),
                    member_str,
                    from_match
                )
            }
            EnumErrorKind::EnumMemberUsedAsType(box EnumMemberUsedAsTypeData {
                reason,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumMemberUsedAsType ({}) ({}))",
                    dump_reason(cx, reason),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumIncompatible(box EnumIncompatibleData {
                reason_lower,
                reason_upper,
                use_op,
                enum_kind,
                representation_type,
                casting_syntax: _,
            }) => {
                let enum_kind_str = match enum_kind {
                    EnumKind::ConcreteEnumKind => "concrete",
                    EnumKind::AbstractEnumKind => "abstract",
                };
                let repr_str = match representation_type {
                    Some(r) => r.to_string(),
                    None => "<None>".to_string(),
                };
                format!(
                    "EEnumError (EnumIncompatible(Box::new(EnumIncompatibleData {{ reason_lower = {}; reason_upper = {}; use_op = {}; enum_kind = {}; representation_type = {} }})))",
                    dump_reason(cx, reason_lower),
                    dump_reason(cx, reason_upper),
                    string_of_use_op(use_op),
                    enum_kind_str,
                    repr_str
                )
            }
            EnumErrorKind::EnumInvalidAbstractUse(box EnumInvalidAbstractUseData {
                reason,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumInvalidAbstractUse ({}) ({}))",
                    dump_reason(cx, reason),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumInvalidMemberName(box EnumInvalidMemberNameData {
                loc,
                enum_reason,
                member_name,
            }) => {
                format!(
                    "EEnumError (EnumInvalidMemberName ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumNonIdentifierMemberName(box EnumNonIdentifierMemberNameData {
                loc,
                enum_reason,
                member_name,
            }) => {
                format!(
                    "EEnumError (EnumNonIdentifierMemberName ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumDuplicateMemberName(box EnumDuplicateMemberNameData {
                loc,
                prev_use_loc,
                enum_reason,
                member_name,
            }) => {
                format!(
                    "EEnumError (EnumDuplicateMemberName ({}) ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    string_of_aloc(None, prev_use_loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumInconsistentMemberValues(box EnumInconsistentMemberValuesData {
                loc,
                enum_reason,
            }) => {
                format!(
                    "EEnumError (EnumInconsistentMemberValues ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason)
                )
            }
            EnumErrorKind::EnumInvalidMemberInitializer(box EnumInvalidMemberInitializerData {
                loc,
                enum_reason,
                member_name,
                ..
            }) => {
                format!(
                    "EEnumError (EnumInvalidMemberInitializer ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumBooleanMemberNotInitialized(
                box EnumBooleanMemberNotInitializedData {
                    loc,
                    enum_reason,
                    member_name,
                },
            ) => {
                format!(
                    "EEnumError (EnumBooleanMemberNotInitialized ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumNumberMemberNotInitialized(
                box EnumNumberMemberNotInitializedData {
                    loc,
                    enum_reason,
                    member_name,
                },
            ) => {
                format!(
                    "EEnumError (EnumNumberMemberNotInitialized ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumBigIntMemberNotInitialized(
                box EnumBigIntMemberNotInitializedData {
                    loc,
                    enum_reason,
                    member_name,
                },
            ) => {
                format!(
                    "EEnumError (EnumBigIntMemberNotInitialized ({}) ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason),
                    member_name
                )
            }
            EnumErrorKind::EnumStringMemberInconsistentlyInitialized(
                box EnumStringMemberInconsistentlyInitializedData { loc, enum_reason },
            ) => {
                format!(
                    "EEnumError (EnumStringMemberInconsistentlyInitialized ({}) ({}))",
                    string_of_aloc(None, loc),
                    dump_reason(cx, enum_reason)
                )
            }
        },
        ErrorMessage::EAssignConstLikeBinding(box EAssignConstLikeBindingData {
            loc,
            definition,
            binding_kind,
        }) => {
            format!(
                "EAssignConstLikeBinding ({}) ({}) ({})",
                string_of_aloc(None, loc),
                dump_reason(cx, definition),
                binding_kind.as_str()
            )
        }
        ErrorMessage::EMalformedCode(loc) => {
            format!("EMalformedCode ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EObjectThisSuperReference(box (loc, r, _)) => {
            format!(
                "EObjectThisSuperReference(Box::new(({}, {}, _)))",
                string_of_aloc(None, loc),
                dump_reason(cx, r)
            )
        }
        ErrorMessage::EComponentThisReference(box EComponentThisReferenceData {
            this_loc,
            component_loc,
        }) => {
            format!(
                "EComponentThisReference (this={}, component={})",
                string_of_aloc(None, this_loc),
                string_of_aloc(None, component_loc)
            )
        }
        ErrorMessage::EComponentCase(loc) => {
            format!("EComponentCase ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EComponentMissingReturn(r) => {
            format!("EComponentMissingReturn ({})", dump_reason(cx, r))
        }
        ErrorMessage::EComponentMissingBody(loc) => {
            format!("EComponentMissingBody ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EComponentBodyInAmbientContext(loc) => {
            format!(
                "EComponentBodyInAmbientContext ({})",
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::ENestedComponent(r) => {
            format!("ENestedComponent ({})", dump_reason(cx, r))
        }
        ErrorMessage::ENestedHook(r) => {
            format!("ENestedHook ({})", dump_reason(cx, r))
        }
        ErrorMessage::EInvalidDeclaration(box EInvalidDeclarationData { declaration, .. }) => {
            format!(
                "EInvalidDeclaration(Box::new(EInvalidDeclarationData {}))",
                dump_reason(cx, declaration)
            )
        }
        ErrorMessage::EImplicitInstantiationUnderconstrainedError(
            box EImplicitInstantiationUnderconstrainedErrorData { use_op, .. },
        ) => {
            format!(
                "EImplicitInstantiationUnderconstrainedError ({})",
                string_of_use_op(use_op)
            )
        }
        ErrorMessage::EClassToObject(box EClassToObjectData { .. }) => "EClassToObject".to_string(),
        ErrorMessage::EMethodUnbinding(box EMethodUnbindingData {
            use_op,
            reason_prop,
            reason_op,
        }) => {
            format!(
                "EMethodUnbinding ({}) ({}) ({})",
                string_of_use_op(use_op),
                dump_reason(cx, reason_op),
                dump_reason(cx, reason_prop)
            )
        }
        ErrorMessage::EHookIncompatible(box EHookIncompatibleData {
            use_op,
            lower,
            upper,
            ..
        }) => {
            format!(
                "EHookIncompatible ({}) ({}) ({})",
                string_of_use_op(use_op),
                dump_reason(cx, lower),
                dump_reason(cx, upper)
            )
        }
        ErrorMessage::EIncompatibleReactDeepReadOnly(box EIncompatibleReactDeepReadOnlyData {
            use_op,
            lower,
            upper,
            ..
        }) => {
            format!(
                "EIncompatibleReactDeepReadOnly ({}) ({}) ({})",
                string_of_use_op(use_op),
                dump_reason(cx, lower),
                dump_reason(cx, upper)
            )
        }
        ErrorMessage::EHookUniqueIncompatible(box EHookUniqueIncompatibleData {
            use_op,
            lower,
            upper,
        }) => {
            format!(
                "EHookUniqueIncompatible ({}) ({}) ({})",
                string_of_use_op(use_op),
                dump_reason(cx, lower),
                dump_reason(cx, upper)
            )
        }
        ErrorMessage::EHookRuleViolation(box EHookRuleViolationData { .. }) => {
            "EHookRuleViolation ( )".to_string()
        }
        ErrorMessage::EHookNaming(_) => "EHookNaming ( )".to_string(),
        ErrorMessage::EInvalidGraphQL(box (loc, err)) => {
            let err_str = match err {
                flow_parser_utils::graphql::GraphqlError::InvalidTaggedTemplate => {
                    "invalid tagged template"
                }
                flow_parser_utils::graphql::GraphqlError::InvalidGraphQL => "invalid graphql",
            };
            format!(
                "EInvalidGraphQL ({}) ({})",
                string_of_aloc(None, loc),
                err_str
            )
        }
        ErrorMessage::EAnnotationInference(box (loc, reason_op, reason, _)) => {
            format!(
                "EAnnotationInference ({}) ({}) ({})",
                string_of_aloc(None, loc),
                dump_reason(cx, reason_op),
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::ETrivialRecursiveDefinition(box (loc, reason)) => {
            format!(
                "ETrivialRecursiveDefinition ({}) ({})",
                string_of_aloc(None, loc),
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EDefinitionCycle(_) => "EDefinitionCycle".to_string(),
        ErrorMessage::ERecursiveDefinition(box ERecursiveDefinitionData { .. }) => {
            "ERecursiveDefinition".to_string()
        }
        ErrorMessage::EDuplicateClassMember(box EDuplicateClassMemberData {
            loc, name, ..
        }) => {
            format!(
                "EDuplicateClassMember ({}) ({})",
                string_of_aloc(None, loc),
                name
            )
        }
        ErrorMessage::EReferenceInAnnotation(box (_, _, _)) => "EReferenceInAnnotation".to_string(),
        ErrorMessage::EReferenceInDefault(box (_, _, _)) => "EReferenceInDefault".to_string(),
        ErrorMessage::EEmptyArrayNoProvider { loc } => {
            format!("EEmptyArrayNoProvider ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUnusedPromise { loc, .. } => {
            format!("EUnusedPromise ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EReactIntrinsicOverlap(box EReactIntrinsicOverlapData { .. }) => {
            "EReactIntrinsicOverlap (_, _, _)".to_string()
        }
        ErrorMessage::EReactRefInRender { .. } => "EReactRefInRender _".to_string(),
        ErrorMessage::EBigIntRShift3(reason) => {
            format!("EBigIntRShift3 ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EBigIntNumCoerce(reason) => {
            format!("EBigIntNumCoerce ({})", dump_reason(cx, reason))
        }
        ErrorMessage::EInvalidCatchParameterAnnotation {
            loc,
            ts_utility_syntax,
        } => {
            format!(
                "EInvalidCatchParameterAnnotation {{{}; {}}}",
                string_of_aloc(None, loc),
                ts_utility_syntax
            )
        }
        ErrorMessage::ETSSyntax(box ETSSyntaxData { loc, .. }) => {
            format!("ETSSyntax ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EVarianceKeyword(box EVarianceKeywordData { loc, .. }) => {
            format!("EVarianceKeyword ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EInvalidBinaryArith(box EInvalidBinaryArithData {
            reason_out,
            reason_l,
            reason_r,
            kind,
        }) => {
            format!(
                "EInvalidBinaryArith ({}, {}, {}, {:?})",
                dump_reason(cx, reason_out),
                dump_reason(cx, reason_l),
                dump_reason(cx, reason_r),
                kind
            )
        }
        ErrorMessage::EInvalidMappedType { loc, kind } => {
            let kind_str = match kind {
                InvalidMappedTypeErrorKind::InterfaceOrDeclaredClass => "InterfaceOrDeclaredClass",
                InvalidMappedTypeErrorKind::ExtraProperties => "ExtraProperties",
                InvalidMappedTypeErrorKind::ExplicitExactOrInexact => "ExplicitExactOrInexact",
                InvalidMappedTypeErrorKind::RemoveOptionality => "RemoveOptionality",
                InvalidMappedTypeErrorKind::VarianceOnArrayInput => "VarianceOnArrayInput",
            };
            format!(
                "EInvalidMappedType ({}, {})",
                string_of_aloc(None, loc),
                kind_str
            )
        }
        ErrorMessage::EDuplicateComponentProp(box EDuplicateComponentPropData {
            spread, ..
        }) => {
            format!("EDuplicateComponentProp ({})", string_of_aloc(None, spread))
        }
        ErrorMessage::ERefComponentProp(box ERefComponentPropData { loc, .. }) => {
            format!("ERefComponentProp ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EKeySpreadProp(box EKeySpreadPropData { loc, .. }) => {
            format!("EKeySpreadProp ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EInvalidComponentRestParam(loc) => {
            format!("EInvalidComponentRestParam ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EInvalidTypeCastSyntax { loc, .. } => {
            format!("EInvalidTypeCastSyntax ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EMissingPlatformSupportWithAvailablePlatforms(
            box EMissingPlatformSupportWithAvailablePlatformsData {
                loc,
                available_platforms,
                required_platforms,
            },
        ) => {
            format!(
                "EMissingPlatformSupportWithAvailablePlatforms({}, {:?}, {:?})",
                string_of_aloc(None, loc),
                available_platforms,
                required_platforms
            )
        }
        ErrorMessage::EMissingPlatformSupport(box EMissingPlatformSupportData {
            loc,
            missing_platforms,
        }) => {
            format!(
                "EMissingPlatformSupport({}, {:?})",
                string_of_aloc(None, loc),
                missing_platforms
            )
        }
        ErrorMessage::EUnionPartialOptimizationNonUniqueKey(
            box EUnionPartialOptimizationNonUniqueKeyData { loc, .. },
        ) => {
            format!(
                "EUnionPartialOptimizationNonUniqueKey ({})",
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::EUnionOptimization(box EUnionOptimizationData { loc, .. }) => {
            format!("EUnionOptimization ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EUnionOptimizationOnNonUnion(box EUnionOptimizationOnNonUnionData {
            loc,
            ..
        }) => {
            format!(
                "EUnionOptimizationOnNonUnion ({})",
                string_of_aloc(None, loc)
            )
        }
        ErrorMessage::ECannotCallReactComponent { reason } => {
            format!("ECannotCallReactComponent ({})", dump_reason(cx, reason))
        }
        ErrorMessage::ENegativeTypeGuardConsistency(box ENegativeTypeGuardConsistencyData {
            reason,
            ..
        }) => {
            format!(
                "ENegativeTypeGuardConsistency ({})",
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EMatchError(e) => match e {
            MatchErrorKind::MatchNotExhaustive(box MatchNotExhaustiveData { loc, .. }) => {
                format!("EMatchNotExhaustive ({})", string_of_aloc(None, loc))
            }
            MatchErrorKind::MatchUnusedPattern(box MatchUnusedPatternData {
                reason,
                already_seen,
            }) => {
                let already_str = match already_seen {
                    Some(r) => dump_reason(cx, r),
                    None => "".to_string(),
                };
                format!(
                    "EMatchUnusedPattern ({}) ({})",
                    dump_reason(cx, reason),
                    already_str
                )
            }
            MatchErrorKind::MatchNonExhaustiveObjectPattern(
                box MatchNonExhaustiveObjectPatternData { loc, rest, .. },
            ) => {
                let rest_str = match rest {
                    Some(r) => dump_reason(cx, r),
                    None => "".to_string(),
                };
                format!(
                    "EMatchNonExhaustiveObjectPattern ({}) ({})",
                    string_of_aloc(None, loc),
                    rest_str
                )
            }
            MatchErrorKind::MatchNonExplicitEnumCheck(box MatchNonExplicitEnumCheckData {
                loc,
                wildcard_reason,
                ..
            }) => {
                format!(
                    "EMatchNonExplicitEnumCheck ({}) ({})",
                    string_of_aloc(None, loc),
                    dump_reason(cx, wildcard_reason)
                )
            }
            MatchErrorKind::MatchInvalidGuardedWildcard(loc) => {
                format!(
                    "EMatchInvalidGuardedWildcard ({})",
                    string_of_aloc(None, loc)
                )
            }
            MatchErrorKind::MatchInvalidIdentOrMemberPattern(
                box MatchInvalidIdentOrMemberPatternData { loc, type_reason },
            ) => {
                format!(
                    "EMatchInvalidIdentOrMemberPattern ({}) ({})",
                    string_of_aloc(None, loc),
                    dump_reason(cx, type_reason)
                )
            }
            MatchErrorKind::MatchInvalidBindingKind { loc, kind } => {
                format!(
                    "EMatchInvalidBindingKind ({}) ({:?})",
                    string_of_aloc(None, loc),
                    kind
                )
            }
            MatchErrorKind::MatchInvalidObjectPropertyLiteral { loc, .. } => {
                format!(
                    "EMatchInvalidObjectPropertyLiteral ({})",
                    string_of_aloc(None, loc)
                )
            }
            MatchErrorKind::MatchInvalidUnaryZero { loc } => {
                format!("EMatchInvalidUnaryZero ({})", string_of_aloc(None, loc))
            }
            MatchErrorKind::MatchInvalidUnaryPlusBigInt { loc } => {
                format!("EMatchInvalidUnaryBigInt ({})", string_of_aloc(None, loc))
            }
            MatchErrorKind::MatchDuplicateObjectProperty(
                box MatchDuplicateObjectPropertyData { loc, name, .. },
            ) => {
                format!(
                    "EMatchDuplicateObjectProperty ({}) ({})",
                    string_of_aloc(None, loc),
                    name
                )
            }
            MatchErrorKind::MatchBindingInOrPattern { loc } => {
                format!("EMatchBindingInOrPattern ({})", string_of_aloc(None, loc))
            }
            MatchErrorKind::MatchInvalidAsPattern { loc } => {
                format!("EMatchInvalidAsPattern ({})", string_of_aloc(None, loc))
            }
            MatchErrorKind::MatchInvalidPatternReference(
                box MatchInvalidPatternReferenceData {
                    loc,
                    binding_reason,
                },
            ) => {
                format!(
                    "EMatchInvalidPatternReference ({}) ({})",
                    string_of_aloc(None, loc),
                    dump_reason(cx, binding_reason)
                )
            }
            MatchErrorKind::MatchInvalidObjectShorthand(box MatchInvalidObjectShorthandData {
                loc,
                name,
                ..
            }) => {
                format!(
                    "EMatchInvalidObjectShorthand ({}) ({})",
                    string_of_aloc(None, loc),
                    name
                )
            }
            MatchErrorKind::MatchStatementInvalidBody { loc } => {
                format!("EMatchStatementInvalidBody ({})", string_of_aloc(None, loc))
            }
            MatchErrorKind::MatchInvalidCaseSyntax(box MatchInvalidCaseSyntaxData {
                loc, ..
            }) => {
                format!("EMatchInvalidCaseSyntax ({})", string_of_aloc(None, loc))
            }
            MatchErrorKind::MatchInvalidWildcardSyntax(loc) => {
                format!(
                    "EMatchInvalidWildcardSyntax ({})",
                    string_of_aloc(None, loc)
                )
            }
            MatchErrorKind::MatchInvalidInstancePattern(loc) => {
                format!(
                    "EMatchInvalidInstancePattern ({})",
                    string_of_aloc(None, loc)
                )
            }
        },
        ErrorMessage::ERecordError(e) => match e {
            RecordErrorKind::RecordBannedTypeUtil {
                reason_op,
                reason_record,
            } => {
                format!(
                    "ERecordBannedTypeUtil ({}) ({})",
                    dump_reason(cx, reason_op),
                    dump_reason(cx, reason_record)
                )
            }
            RecordErrorKind::RecordInvalidName { name, loc } => {
                format!(
                    "ERecordInvalidName {{ name = {}; loc = {} }}",
                    name,
                    string_of_aloc(None, loc)
                )
            }
            RecordErrorKind::RecordInvalidNew { record_name, loc } => {
                format!(
                    "ERecordInvalidNew {{ record_name = {}; loc = {} }}",
                    record_name,
                    string_of_aloc(None, loc)
                )
            }
            RecordErrorKind::RecordDeclarationInvalidSyntax { loc, .. } => {
                format!(
                    "ERecordDeclarationInvalidSyntax ({})",
                    string_of_aloc(None, loc)
                )
            }
        },
        ErrorMessage::EUndocumentedFeature { loc } => {
            format!("EUndocumentedFeature ({})", string_of_aloc(None, loc))
        }
        ErrorMessage::EIllegalAssertOperator(box EIllegalAssertOperatorData {
            obj,
            op,
            specialized,
        }) => {
            format!(
                "EIllegalAssertOperator(Box::new(EIllegalAssertOperatorData {{obj={}, op={}, specialized={}}}))",
                dump_reason(cx, obj),
                dump_reason(cx, op),
                specialized
            )
        }
        ErrorMessage::EDevOnlyRefinedLocInfo(box EDevOnlyRefinedLocInfoData {
            refined_loc,
            ..
        }) => {
            format!(
                "EDevOnlyRefinedLocInfo(Box::new(EDevOnlyRefinedLocInfoData {{refined_loc={}}}))",
                string_of_aloc(None, refined_loc)
            )
        }
        ErrorMessage::EDevOnlyInvalidatedRefinementInfo(
            box EDevOnlyInvalidatedRefinementInfoData { read_loc, .. },
        ) => {
            format!(
                "EDevOnlyInvalidatedRefinementInfo(Box::new(EDevOnlyInvalidatedRefinementInfoData {{read_loc={}}}))",
                string_of_aloc(None, read_loc)
            )
        }
        ErrorMessage::ETemporaryHardcodedErrorForPrototyping(box (reason, _)) => {
            format!(
                "ETemporaryHardcodedErrorForPrototyping(Box::new(({}, _)))",
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::ETypeParamConstIncompatibility(box ETypeParamConstIncompatibilityData {
            lower,
            ..
        }) => {
            format!(
                "ETypeParamConstIncompatibility ({})",
                dump_reason(cx, lower)
            )
        }
        ErrorMessage::ETypeParamConstInvalidPosition(reason) => {
            format!(
                "ETypeParamConstInvalidPosition ({})",
                dump_reason(cx, reason)
            )
        }
        ErrorMessage::EConstantCondition(box EConstantConditionData {
            loc,
            is_truthy,
            show_warning,
            constant_condition_kind,
            reason,
        }) => {
            let kind_str = match constant_condition_kind {
                ConstantConditionKind::UnawaitedPromise => "UnawaitedPromise",
                ConstantConditionKind::UncalledFunction => "UncalledFunction",
                ConstantConditionKind::ConstCondGeneral => "General",
            };
            let reason_str = match reason {
                Some(r) => dump_reason(cx, r),
                None => "None".to_string(),
            };
            format!(
                "EInvalidComparison ({}). is_truthy={} show_warning={} constant_condition_kind={} reason={}",
                string_of_aloc(None, loc),
                is_truthy,
                show_warning,
                kind_str,
                reason_str
            )
        }
        ErrorMessage::EDeclareComponentInvalidParam { loc, kind } => {
            use flow_typing_errors::intermediate_error_types::DeclareComponentInvalidParamKind::*;
            let kind_str = match kind {
                DeclareComponentParamAsBinding => "AsBinding",
                DeclareComponentParamDefaultValue => "DefaultValue",
                DeclareComponentParamMissingAnnotation => "MissingAnnotation",
                DeclareComponentParamStringLiteralWithoutAs => "StringLiteralWithoutAs",
            };
            format!(
                "EDeclareComponentInvalidParam {{ loc={}; kind={} }}",
                string_of_aloc(None, loc),
                kind_str,
            )
        }
    }
}

pub mod verbose {
    use super::*;

    pub fn print_if_verbose_lazy(
        cx: &Context,
        trace: Option<&DepthTrace>,
        delim: Option<&str>,
        indent: Option<i32>,
        lines: impl FnOnce() -> Vec<String>,
    ) {
        if !cx.is_verbose() {
            return;
        }
        let verbose_opt = cx.verbose();
        if let Some(verbose) = verbose_opt {
            let actual_indent = std::cmp::max(
                indent.unwrap_or(0) + (trace.map(|t| t.depth()).unwrap_or(0) as i32) - 1,
                0,
            );
            let prefix = " ".repeat((actual_indent as usize) * (verbose.indent as usize));
            let pid = cx.pid_prefix();
            let add_prefix = |line: &str| format!("\n{}{}{}", prefix, pid, line);
            let lines_vec = lines();
            let prefixed: Vec<String> = lines_vec.iter().map(|l| add_prefix(l)).collect();
            eprintln!("{}", prefixed.join(delim.unwrap_or("")));
        }
    }

    pub fn print_if_verbose(
        cx: &Context,
        trace: Option<&DepthTrace>,
        delim: Option<&str>,
        indent: Option<i32>,
        lines: Vec<String>,
    ) {
        if cx.is_verbose() {
            print_if_verbose_lazy(cx, trace, delim, indent, || lines);
        }
    }

    pub fn print_types_if_verbose<CX>(
        cx: &Context,
        trace: &DepthTrace,
        note: Option<&str>,
        t: &Type,
        use_t: &UseT<CX>,
    ) {
        if !cx.is_verbose() {
            return;
        }
        if let Some(verbose) = cx.verbose() {
            let depth = verbose.depth;
            let delim = match note {
                Some(x) => format!(" ~> {}", x),
                None => " ~>".to_string(),
            };
            print_if_verbose(
                cx,
                Some(trace),
                Some(&delim),
                None,
                vec![
                    super::dump_t(Some(depth), cx, t),
                    super::dump_use_t(Some(depth), cx, use_t),
                ],
            );
        }
    }

    pub fn print_unify_types_if_verbose(
        cx: &Context,
        trace: &DepthTrace,
        note: Option<&str>,
        t1: &Type,
        t2: &Type,
    ) {
        if !cx.is_verbose() {
            return;
        }
        if let Some(verbose) = cx.verbose() {
            let depth = verbose.depth;
            let delim = match note {
                Some(x) => format!(" = {}", x),
                None => " =".to_string(),
            };
            print_if_verbose(
                cx,
                Some(trace),
                Some(&delim),
                None,
                vec![
                    super::dump_t(Some(depth), cx, t1),
                    super::dump_t(Some(depth), cx, t2),
                ],
            );
        }
    }
}
