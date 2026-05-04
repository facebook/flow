/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_context::type_app_expansion;
use flow_typing_errors::error_message::EClassToObjectData;
use flow_typing_errors::error_message::EExpectedBigIntLitData;
use flow_typing_errors::error_message::EExpectedBooleanLitData;
use flow_typing_errors::error_message::EExpectedNumberLitData;
use flow_typing_errors::error_message::EExpectedStringLitData;
use flow_typing_errors::error_message::EHookIncompatibleData;
use flow_typing_errors::error_message::EHookUniqueIncompatibleData;
use flow_typing_errors::error_message::EIncompatibleData;
use flow_typing_errors::error_message::EIncompatibleWithUseOpData;
use flow_typing_errors::error_message::EIndexerCheckFailedData;
use flow_typing_errors::error_message::EInvariantSubtypingWithUseOpData;
use flow_typing_errors::error_message::EMethodUnbindingData;
use flow_typing_errors::error_message::EPrimitiveAsInterfaceData;
use flow_typing_errors::error_message::EPropNotFoundInSubtypingData;
use flow_typing_errors::error_message::EPropPolarityMismatchData;
use flow_typing_errors::error_message::EPropsExtraAgainstExactObjectData;
use flow_typing_errors::error_message::EPropsNotFoundInInvariantSubtypingData;
use flow_typing_errors::error_message::EPropsNotFoundInSubtypingData;
use flow_typing_errors::error_message::ETupleArityMismatchData;
use flow_typing_errors::error_message::ETupleElementPolarityMismatchData;
use flow_typing_errors::error_message::ETypeParamConstIncompatibilityData;
use flow_typing_errors::error_message::EUnionOptimizationOnNonUnionData;
use flow_typing_errors::error_message::EnumErrorKind;
use flow_typing_errors::error_message::EnumIncompatibleData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::flow_error::ordered_reasons;
use flow_typing_errors::intermediate_error_types;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::instantiation_utils;
use flow_typing_flow_common::obj_type;
use flow_typing_flow_common::speculation;
use flow_typing_type::type_::AnyErrorKind;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::ArrType;
use flow_typing_type::type_::ArrayATData;
use flow_typing_type::type_::CallArg;
use flow_typing_type::type_::CanonicalRendersForm;
use flow_typing_type::type_::ComponentKind;
use flow_typing_type::type_::Cont;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::DictType;
use flow_typing_type::type_::EnumInfoInner;
use flow_typing_type::type_::FunParamData;
use flow_typing_type::type_::FunRestParam;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::HasOwnPropTData;
use flow_typing_type::type_::InstType;
use flow_typing_type::type_::InstTypeInner;
use flow_typing_type::type_::InstanceKind;
use flow_typing_type::type_::InstanceT;
use flow_typing_type::type_::InstanceTInner;
use flow_typing_type::type_::Literal;
use flow_typing_type::type_::LookupAction;
use flow_typing_type::type_::LookupKind;
use flow_typing_type::type_::LookupPropForSubtypingData;
use flow_typing_type::type_::LookupTData;
use flow_typing_type::type_::MixedFlavor;
use flow_typing_type::type_::NominalType;
use flow_typing_type::type_::NominalTypeInner;
use flow_typing_type::type_::NonstrictReturningData;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::ObjType;
use flow_typing_type::type_::OpaqueTypeCustomErrorCompatibilityData;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::PropRef;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyCompatibilityData;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::PropertyType;
use flow_typing_type::type_::ReactAbstractComponentTData;
use flow_typing_type::type_::ReactEffectType;
use flow_typing_type::type_::ReactKitTData;
use flow_typing_type::type_::ReposUseTData;
use flow_typing_type::type_::StrUtilOp;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::ThisStatus;
use flow_typing_type::type_::ThisTypeAppTData;
use flow_typing_type::type_::TupleATData;
use flow_typing_type::type_::TupleElement;
use flow_typing_type::type_::TupleElementCompatibilityData;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UnifyCause;
use flow_typing_type::type_::UnionEnum;
use flow_typing_type::type_::UnionEnumSet;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::VirtualFrameUseOp;
use flow_typing_type::type_::VirtualUseOp;
use flow_typing_type::type_::any_t;
use flow_typing_type::type_::constraint;
use flow_typing_type::type_::dummy_prototype;
use flow_typing_type::type_::empty_t;
use flow_typing_type::type_::mixed_t;
use flow_typing_type::type_::mk_functiontype;
use flow_typing_type::type_::name_of_propref;
use flow_typing_type::type_::nominal;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::property;
use flow_typing_type::type_::react;
use flow_typing_type::type_::str_module_t;
use flow_typing_type::type_::type_or_type_desc;
use flow_typing_type::type_::union_rep;
use flow_typing_type::type_::void;
use flow_typing_type::type_util;
use flow_typing_visitors::type_mapper;
use vec1::Vec1;

use crate::flow_js::FlowJs;
use crate::renders_kit;
use crate::speculation_kit;
use crate::type_inference_hooks_js;

fn flow_all_in_union<'cx>(
    cx: &Context<'cx>,
    trace: &DepthTrace,
    rep: &union_rep::UnionRep,
    u: &UseT<Context<'cx>>,
) -> Result<(), FlowJsException> {
    flow_js_utils::iter_union(
        |cx, trace, (t, u)| FlowJs::rec_flow(cx, *trace, t, u),
        (),
        |_, _| (),
        cx,
        trace,
        rep,
        u,
    )
}

// let add_output_prop_polarity_mismatch cx use_op (lreason, ureason) props =
fn add_output_prop_polarity_mismatch<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    lreason: &Reason,
    ureason: &Reason,
    props: Vec<(Option<Name>, (Polarity, Polarity))>,
) -> Result<(), FlowJsException> {
    if let Ok(props) = Vec1::try_from_vec(props) {
        flow_js_utils::add_output(
            cx,
            ErrorMessage::EPropPolarityMismatch(Box::new(EPropPolarityMismatchData {
                lreason: lreason.dupe(),
                ureason: ureason.dupe(),
                props,
                use_op,
            })),
        )?;
    }
    Ok(())
}

fn polarity_error_content(
    propref: &Option<Name>,
    lp: &PropertyType,
    up: &PropertyType,
) -> (Option<Name>, (Polarity, Polarity)) {
    let lpol = property::polarity_of_property_type(lp);
    let upol = property::polarity_of_property_type(up);
    (propref.dupe(), (lpol, upol))
}

fn rec_flow_p_inner<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    use_op: UseOp,
    lower_upper_subtyping_obj_ts: Option<(&Type, &Type)>,
    upper_object_reason: &Reason,
    report_polarity: bool,
    propref: &PropRef,
    lp: &PropertyType,
    up: &PropertyType,
) -> Result<Vec<(Option<Name>, (Polarity, Polarity))>, FlowJsException> {
    match (lp, up) {
        // unification cases
        (
            PropertyType::OrdinaryField {
                type_: lt,
                polarity: Polarity::Neutral,
            },
            PropertyType::OrdinaryField {
                type_: ut,
                polarity: Polarity::Neutral,
            },
        ) => {
            let unify_cause = match lower_upper_subtyping_obj_ts {
                None => UnifyCause::Uncategorized,
                Some((lower_obj_t, upper_obj_t)) => {
                    let property_name = match propref {
                        PropRef::Named { name, .. } => Some(name.as_smol_str().dupe()),
                        PropRef::Computed(_) => None,
                    };
                    UnifyCause::MutableProperty {
                        lower_obj_t: lower_obj_t.dupe(),
                        upper_obj_t: upper_obj_t.dupe(),
                        upper_object_reason: upper_object_reason.dupe(),
                        property_name,
                    }
                }
            };
            FlowJs::unify_opt(cx, trace, use_op, unify_cause, None, lt, ut)?;
            Ok(vec![])
        }
        // directional cases
        (lp, up) => {
            let propref_error = name_of_propref(propref);
            let errs1 = match (
                property::read_t_of_property_type(lp),
                property::read_t_of_property_type(up),
            ) {
                (Some(lt), Some(ut)) => {
                    FlowJs::flow_opt(
                        cx,
                        trace,
                        &lt,
                        &UseT::new(UseTInner::UseT(use_op.dupe(), ut)),
                    )?;
                    vec![]
                }
                (None, Some(_)) if report_polarity => {
                    vec![polarity_error_content(&propref_error, lp, up)]
                }
                _ => vec![],
            };
            let errs2 = match (
                property::write_t_of_property_type(lp, None),
                property::write_t_of_property_type(up, None),
            ) {
                (Some(lt), Some(ut)) => {
                    FlowJs::flow_opt(
                        cx,
                        trace,
                        &ut,
                        &UseT::new(UseTInner::UseT(use_op.dupe(), lt)),
                    )?;
                    vec![]
                }
                (None, Some(_)) if report_polarity => {
                    vec![polarity_error_content(&propref_error, lp, up)]
                }
                _ => vec![],
            };
            let mut result = errs1;
            result.extend(errs2);
            Ok(result)
        }
    }
}

fn index_of_param(params: &[(Option<Name>, Type)], x: &Name) -> Option<usize> {
    params.iter().enumerate().find_map(|(i, p)| match &p.0 {
        Some(x_prime) if x == x_prime => Some(i),
        _ => None,
    })
}

fn func_type_guard_compat<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    grd1: (
        &Reason,
        &[(Option<Name>, Type)],
        bool,
        (&ALoc, &Name),
        &Type,
    ),
    grd2: (
        &Reason,
        &[(Option<Name>, Type)],
        bool,
        (&ALoc, &Name),
        &Type,
    ),
) -> Result<(), FlowJsException> {
    let (reason1, params1, impl1, (loc1, x1), t1) = grd1;
    let (reason2, params2, impl2, (loc2, x2), t2) = grd2;
    if impl1 && !impl2 {
        flow_js_utils::add_output(
            cx,
            ErrorMessage::ETypeGuardImpliesMismatch {
                use_op: use_op.dupe(),
                reasons: (reason1.dupe(), reason2.dupe()),
            },
        )?;
    }
    let idx1 = index_of_param(params1, x1);
    let idx2 = index_of_param(params2, x2);
    let use_op = VirtualUseOp::Frame(
        Arc::new(VirtualFrameUseOp::TypeGuardCompatibility),
        Arc::new(use_op),
    );
    if idx1 != idx2 {
        let lower = flow_common::reason::mk_reason(
            VirtualReasonDesc::RTypeGuardParam(x1.as_smol_str().dupe()),
            loc1.dupe(),
        );
        let upper = flow_common::reason::mk_reason(
            VirtualReasonDesc::RTypeGuardParam(x2.as_smol_str().dupe()),
            loc2.dupe(),
        );
        flow_js_utils::add_output(
            cx,
            ErrorMessage::ETypeGuardIndexMismatch {
                use_op: use_op.dupe(),
                reasons: (lower, upper),
            },
        )?;
    }
    if impl2 {
        FlowJs::rec_flow_t(cx, trace, use_op, t1, t2)
    } else {
        FlowJs::rec_unify(cx, trace, use_op, UnifyCause::Uncategorized, None, t1, t2)
    }
}

fn flow_obj_to_obj<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    lreason: &Reason,
    l_obj: &Rc<ObjType>,
    ureason: &Reason,
    u_obj: &Rc<ObjType>,
) -> Result<(), FlowJsException> {
    let ObjType {
        flags: lflags,
        call_t: lcall,
        props_tmap: lflds,
        proto_t: lproto,
        reachable_targs: _,
    } = &**l_obj;
    let ObjType {
        flags: rflags,
        call_t: ucall,
        props_tmap: uflds,
        proto_t: uproto,
        reachable_targs: _,
    } = &**u_obj;

    // if inflowing type is literal (thus guaranteed to be
    // unaliased), propertywise subtyping is sound
    let lit = flow_common::reason::is_literal_object_reason(lreason);
    // If both are dictionaries, ensure the keys and values are compatible
    // with each other.
    let ldict = obj_type::get_dict_opt(&lflags.obj_kind);
    let udict = obj_type::get_dict_opt(&rflags.obj_kind);

    match (ldict, udict) {
        (Some(ld), Some(ud)) => {
            let DictType {
                key: lk,
                value: lv,
                dict_polarity: lpolarity,
                ..
            } = ld;
            let DictType {
                key: uk,
                value: uv,
                dict_polarity: upolarity,
                ..
            } = ud;
            let use_op_k = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::IndexerKeyCompatibility {
                    lower: lreason.dupe(),
                    upper: ureason.dupe(),
                }),
                Arc::new(use_op.dupe()),
            );
            if lit {
                FlowJs::rec_flow_t(cx, trace, use_op_k.dupe(), lk, uk)?;
            } else {
                let l_t = Type::new(TypeInner::DefT(
                    lreason.dupe(),
                    DefT::new(DefTInner::ObjT(l_obj.dupe())),
                ));
                let u_t = Type::new(TypeInner::DefT(
                    ureason.dupe(),
                    DefT::new(DefTInner::ObjT(u_obj.dupe())),
                ));
                // Don't report polarity errors when checking the indexer key. We would
                // report these errors again a second time when checking values.
                let errs = rec_flow_p_inner(
                    cx,
                    Some(trace),
                    use_op_k.dupe(),
                    Some((&l_t, &u_t)),
                    ureason,
                    false,
                    &PropRef::Computed(uk.dupe()),
                    &PropertyType::OrdinaryField {
                        type_: lk.dupe(),
                        polarity: *lpolarity,
                    },
                    &PropertyType::OrdinaryField {
                        type_: uk.dupe(),
                        polarity: *upolarity,
                    },
                )?;
                add_output_prop_polarity_mismatch(cx, use_op_k, lreason, ureason, errs)?;
            }
            let use_op_v = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                    PropertyCompatibilityData {
                        prop: None,
                        lower: lreason.dupe(),
                        upper: ureason.dupe(),
                    },
                ))),
                Arc::new(use_op.dupe()),
            );
            if lit {
                FlowJs::rec_flow_t(cx, trace, use_op_v, lv, uv)?;
            } else {
                let l_t = Type::new(TypeInner::DefT(
                    lreason.dupe(),
                    DefT::new(DefTInner::ObjT(l_obj.dupe())),
                ));
                let u_t = Type::new(TypeInner::DefT(
                    ureason.dupe(),
                    DefT::new(DefTInner::ObjT(u_obj.dupe())),
                ));
                let errs = rec_flow_p_inner(
                    cx,
                    Some(trace),
                    use_op_v.dupe(),
                    Some((&l_t, &u_t)),
                    ureason,
                    true,
                    &PropRef::Computed(uv.dupe()),
                    &PropertyType::OrdinaryField {
                        type_: lv.dupe(),
                        polarity: *lpolarity,
                    },
                    &PropertyType::OrdinaryField {
                        type_: uv.dupe(),
                        polarity: *upolarity,
                    },
                )?;
                add_output_prop_polarity_mismatch(cx, use_op_v, lreason, ureason, errs)?;
            }
        }
        _ => {}
    }

    if rflags.obj_kind == ObjKind::Exact && !flow_common::reason::is_literal_object_reason(ureason)
    {
        if !obj_type::is_exact(&lflags.obj_kind) {
            let l_t = Type::new(TypeInner::DefT(
                lreason.dupe(),
                DefT::new(DefTInner::ObjT(l_obj.dupe())),
            ));
            flow_js_utils::exact_obj_error(
                cx,
                &lflags.obj_kind,
                use_op.dupe(),
                ureason.dupe(),
                &l_t,
            )?;
        }
        let missing_props: Vec<Name> = cx
            .fold_props(
                lflds.dupe(),
                |name, _prop, mut acc: Vec<Name>| {
                    // if Context.has_prop cx uflds name then acc else name :: acc
                    if !cx.has_prop(uflds.dupe(), name) {
                        acc.push(name.dupe());
                    }
                    acc
                },
                Vec::new(),
            )
            .into_iter()
            .collect();
        if let Ok(missing_props) = Vec1::try_from_vec(missing_props) {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EPropsExtraAgainstExactObject(Box::new(
                    EPropsExtraAgainstExactObjectData {
                        prop_names: missing_props,
                        reason_l_obj: lreason.dupe(),
                        reason_r_obj: ureason.dupe(),
                        use_op: use_op.dupe(),
                    },
                )),
            )?;
        }
        if let Some(_lcall) = lcall {
            if ucall.is_none() {
                let prop = Some(Name::new("$call"));
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                        EPropNotFoundInSubtypingData {
                            prop_name: prop,
                            reason_lower: ureason.dupe(),
                            reason_upper: lreason.dupe(),
                            use_op: use_op.dupe(),
                            suggestion: None,
                        },
                    )),
                )?;
            }
        }
    }

    match ucall {
        Some(ucall_id) => {
            let prop_name = Some(Name::new("$call"));
            match lcall {
                Some(lcall_id) => {
                    FlowJs::rec_flow(
                        cx,
                        trace,
                        &cx.find_call(*lcall_id),
                        &UseT::new(UseTInner::UseT(use_op.dupe(), cx.find_call(*ucall_id))),
                    )?;
                }
                None => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                            EPropNotFoundInSubtypingData {
                                reason_lower: lreason.dupe(),
                                reason_upper: ureason.dupe(),
                                prop_name,
                                use_op: use_op.dupe(),
                                suggestion: None,
                            },
                        )),
                    )?;
                }
            }
        }
        None => {}
    }

    // Properties in u must either exist in l, or match l's indexer.
    type InvSubFailed = Vec<(Name, Type, Type)>;
    type LhsMissing = Vec<(Name, Property)>;
    type PolMismatchErrs = Vec<(Option<Name>, (Polarity, Polarity))>;
    let (invariant_subtyping_failed_prop_names, lhs_missing_props, polarity_mismatch_errs): (
        InvSubFailed,
        LhsMissing,
        PolMismatchErrs,
    ) = {
        let mut invariant_subtyping_failed_prop_names: InvSubFailed = Vec::new();
        let mut lhs_missing_props: LhsMissing = Vec::new();
        let mut polarity_mismatch_errs: PolMismatchErrs = Vec::new();
        for (name, up) in cx.find_props(uflds.dupe()).iter() {
            // Lazily construct reason_prop and propref - only needed in non-literal
            // and non-invariant-subtyping branches.
            let mk_propref = || {
                let reason_prop = ureason
                    .dupe()
                    .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe())));
                type_util::mk_named_prop(reason_prop, false, name.dupe())
            };
            // Lazily construct the Frame use_op - two Arc allocations per property.
            // In the literal case where read_t returns None, this avoids wasted work.
            let mk_use_op = || {
                VirtualUseOp::Frame(
                    Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                        PropertyCompatibilityData {
                            prop: Some(name.dupe()),
                            lower: lreason.dupe(),
                            upper: ureason.dupe(),
                        },
                    ))),
                    Arc::new(use_op.dupe()),
                )
            };
            match (cx.get_prop(lflds.dupe(), name), &ldict) {
                (Some(lp), _) => {
                    if lit {
                        // prop from unaliased LB: check <:
                        match (property::read_t(&lp), property::read_t(up)) {
                            (Some(lt), Some(ut)) => {
                                FlowJs::rec_flow(
                                    cx,
                                    trace,
                                    &lt,
                                    &UseT::new(UseTInner::UseT(mk_use_op(), ut)),
                                )?;
                            }
                            _ => {}
                        }
                    } else {
                        // prop from aliased LB
                        let additional_polarity_mismatch_errs =
                            match (property::property_type(&lp), property::property_type(up)) {
                                (
                                    PropertyType::OrdinaryField {
                                        type_: ref lt,
                                        polarity: Polarity::Neutral,
                                    },
                                    PropertyType::OrdinaryField {
                                        type_: ref ut,
                                        polarity: Polarity::Neutral,
                                    },
                                ) if !speculation::speculating(cx)
                                    && !flow_js_utils::tvar_visitors::has_unresolved_tvars(
                                        cx, lt,
                                    )
                                    && !flow_js_utils::tvar_visitors::has_unresolved_tvars(
                                        cx, ut,
                                    ) =>
                                {
                                    let failed = match speculation_kit::try_unify(
                                        cx,
                                        trace,
                                        lt.dupe(),
                                        mk_use_op(),
                                        ut.dupe(),
                                    ) {
                                        Ok(()) => false,
                                        Err(FlowJsException::SpeculationSingletonError) => true,
                                        Err(other) => return Err(other),
                                    };
                                    if failed {
                                        invariant_subtyping_failed_prop_names.push((
                                            name.dupe(),
                                            lt.dupe(),
                                            ut.dupe(),
                                        ));
                                    }
                                    vec![]
                                }
                                (lp_type, up_type) => {
                                    let l_t = Type::new(TypeInner::DefT(
                                        lreason.dupe(),
                                        DefT::new(DefTInner::ObjT(l_obj.dupe())),
                                    ));
                                    let u_t = Type::new(TypeInner::DefT(
                                        ureason.dupe(),
                                        DefT::new(DefTInner::ObjT(u_obj.dupe())),
                                    ));
                                    rec_flow_p_inner(
                                        cx,
                                        Some(trace),
                                        mk_use_op(),
                                        Some((&l_t, &u_t)),
                                        ureason,
                                        true,
                                        &mk_propref(),
                                        &lp_type,
                                        &up_type,
                                    )?
                                }
                            };
                        polarity_mismatch_errs.extend(additional_polarity_mismatch_errs);
                    }
                }
                (None, Some(dict)) if !flow_js_utils::is_dictionary_exempt(name) => {
                    let DictType {
                        key,
                        value,
                        dict_polarity,
                        ..
                    } = *dict;
                    let lp_for_idx = PropertyType::OrdinaryField {
                        type_: value.dupe(),
                        polarity: *dict_polarity,
                    };
                    let up_for_idx = match up.deref() {
                        PropertyInner::Field(fd)
                            if let TypeInner::OptionalT { type_: ut, .. } = fd.type_.deref() =>
                        {
                            PropertyType::OrdinaryField {
                                type_: ut.dupe(),
                                polarity: fd.polarity,
                            }
                        }
                        _ => property::property_type(up),
                    };
                    let subtype_against_indexer =
                        |polarity_mismatch_errs: &mut PolMismatchErrs|
                         -> Result<(), FlowJsException> {
                        if lit {
                            match (
                                property::read_t_of_property_type(&lp_for_idx),
                                property::read_t_of_property_type(&up_for_idx),
                            ) {
                                (Some(lt), Some(ut)) => {
                                    FlowJs::rec_flow(
                                        cx,
                                        trace,
                                        &lt,
                                        &UseT::new(UseTInner::UseT(mk_use_op(), ut)),
                                    )?;
                                }
                                _ => {}
                            }
                        } else {
                            let l_t = Type::new(TypeInner::DefT(
                                lreason.dupe(),
                                DefT::new(DefTInner::ObjT(l_obj.dupe())),
                            ));
                            let u_t = Type::new(TypeInner::DefT(
                                ureason.dupe(),
                                DefT::new(DefTInner::ObjT(u_obj.dupe())),
                            ));
                            let additional = rec_flow_p_inner(
                                cx,
                                Some(trace),
                                mk_use_op(),
                                Some((&l_t, &u_t)),
                                ureason,
                                true,
                                &mk_propref(),
                                &lp_for_idx,
                                &up_for_idx,
                            )?;
                            polarity_mismatch_errs.extend(additional);
                        }
                        Ok(())
                    };
                    match up.deref() {
                        PropertyInner::Field(fd)
                            if matches!(fd.type_.deref(), TypeInner::OptionalT { .. })
                                && (lit || fd.polarity == Polarity::Positive)
                                && !cx.in_implicit_instantiation() =>
                        {
                            // If the upper property is optional and readonly (or this is a lit check)
                            // then we only need to check the lower indexer type against the upper
                            // property type if the upper property key is covered by the lower property's
                            // indexer, otherwise we can omit the check. We already check elsewhere that
                            // the upper dictionary is compatible with the lower dictionary, so we are
                            // not risking any issues with exactness here.
                            //
                            // We only do this outside of implicit instantiation to avoid accidentally
                            // underconstraining tvars by avoiding flows.
                            let key_type = flow_js_utils::type_of_key_name(
                                cx,
                                name.dupe(),
                                &ureason
                                    .dupe()
                                    .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe()))),
                            );
                            if FlowJs::speculative_subtyping_succeeds(cx, &key_type, key)? {
                                subtype_against_indexer(&mut polarity_mismatch_errs)?;
                            }
                        }
                        _ => {
                            let key_type = flow_js_utils::type_of_key_name(
                                cx,
                                name.dupe(),
                                &ureason
                                    .dupe()
                                    .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe()))),
                            );
                            FlowJs::rec_flow(
                                cx,
                                trace,
                                &key_type,
                                &UseT::new(UseTInner::UseT(
                                    VirtualUseOp::Frame(
                                        Arc::new(VirtualFrameUseOp::IndexerKeyCompatibility {
                                            lower: lreason.dupe(),
                                            upper: ureason.dupe(),
                                        }),
                                        Arc::new(use_op.dupe()),
                                    ),
                                    key.dupe(),
                                )),
                            )?;
                            subtype_against_indexer(&mut polarity_mismatch_errs)?;
                        }
                    }
                }
                _ => {
                    // property doesn't exist in inflowing type
                    match up.deref() {
                        PropertyInner::Field(fd)
                            if matches!(fd.type_.deref(), TypeInner::OptionalT { .. }) && lit => {}
                        PropertyInner::Field(fd)
                            if fd.polarity == Polarity::Positive
                                && matches!(fd.type_.deref(), TypeInner::OptionalT { .. })
                                && obj_type::is_exact(&lflags.obj_kind) =>
                        {
                            FlowJs::rec_flow(
                                cx,
                                trace,
                                lproto,
                                &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                                    reason: ureason.dupe(),
                                    lookup_kind: Box::new(LookupKind::NonstrictReturning(
                                        Box::new(NonstrictReturningData(None, None)),
                                    )),
                                    try_ts_on_failure: vec![].into(),
                                    propref: Box::new(mk_propref()),
                                    lookup_action: Box::new(LookupAction::LookupPropForSubtyping(
                                        Box::new(LookupPropForSubtypingData {
                                            use_op: use_op.dupe(),
                                            prop: PropertyType::OrdinaryField {
                                                type_: fd.type_.dupe(),
                                                polarity: Polarity::Positive,
                                            },
                                            prop_name: name.dupe(),
                                            reason_lower: lreason.dupe(),
                                            reason_upper: ureason.dupe(),
                                        }),
                                    )),
                                    method_accessible: true,
                                    ids: None,
                                    ignore_dicts: false,
                                }))),
                            )?;
                        }
                        _ => match (lproto.deref(), &ldict) {
                            (TypeInner::ObjProtoT(_), None)
                                if !flow_js_utils::is_object_prototype_method(name) =>
                            {
                                lhs_missing_props.push((name.dupe(), up.dupe()));
                            }
                            (TypeInner::FunProtoT(_), None)
                                if !flow_js_utils::is_function_prototype(name) =>
                            {
                                lhs_missing_props.push((name.dupe(), up.dupe()));
                            }
                            _ => {
                                let lookup_kind = match &ldict {
                                    None => LookupKind::Strict(lreason.dupe()),
                                    _ => LookupKind::NonstrictReturning(Box::new(
                                        NonstrictReturningData(None, None),
                                    )),
                                };
                                FlowJs::rec_flow(
                                    cx,
                                    trace,
                                    lproto,
                                    &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                                        reason: ureason.dupe(),
                                        lookup_kind: Box::new(lookup_kind),
                                        try_ts_on_failure: vec![].into(),
                                        propref: Box::new(mk_propref()),
                                        lookup_action: Box::new(
                                            LookupAction::LookupPropForSubtyping(Box::new(
                                                LookupPropForSubtypingData {
                                                    use_op: use_op.dupe(),
                                                    prop: property::property_type(up),
                                                    prop_name: name.dupe(),
                                                    reason_lower: lreason.dupe(),
                                                    reason_upper: ureason.dupe(),
                                                },
                                            )),
                                        ),
                                        method_accessible: true,
                                        ids: None,
                                        ignore_dicts: false,
                                    }))),
                                )?;
                            }
                        },
                    }
                }
            }
        }
        (
            invariant_subtyping_failed_prop_names,
            lhs_missing_props,
            polarity_mismatch_errs,
        )
    };

    match invariant_subtyping_failed_prop_names.as_slice() {
        [] => {}
        [(name, l_prop_t, u_prop_t)] => {
            let lower_loc = match l_prop_t.deref() {
                TypeInner::OpenT(tvar) => {
                    match flow_js_utils::merge_tvar_opt(
                        cx,
                        false,
                        union_rep::UnionKind::ResolvedKind,
                        tvar.reason(),
                        tvar.id() as i32,
                    ) {
                        Some(t) => type_util::loc_of_t(&t).dupe(),
                        None => tvar.reason().loc().dupe(),
                    }
                }
                _ => type_util::loc_of_t(l_prop_t).dupe(),
            };
            let upper_loc = type_util::loc_of_t(u_prop_t).dupe();
            let explanation = Some(
                intermediate_error_types::ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
                    lower_obj_loc: lreason.def_loc().dupe(),
                    upper_obj_loc: ureason.def_loc().dupe(),
                    lower_obj_desc: type_or_type_desc::TypeOrTypeDescT::Type(
                        Type::new(TypeInner::DefT(
                            lreason.dupe(),
                            DefT::new(DefTInner::ObjT(l_obj.dupe())),
                        )),
                    ),
                    upper_obj_desc: type_or_type_desc::TypeOrTypeDescT::Type(
                        Type::new(TypeInner::DefT(
                            ureason.dupe(),
                            DefT::new(DefTInner::ObjT(u_obj.dupe())),
                        )),
                    ),
                    upper_object_reason: ureason.dupe(),
                    property_name: Some(name.as_smol_str().dupe()),
                },
            );
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                    PropertyCompatibilityData {
                        prop: Some(name.dupe()),
                        lower: lreason.dupe(),
                        upper: ureason.dupe(),
                    },
                ))),
                Arc::new(use_op.dupe()),
            );
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EInvariantSubtypingWithUseOp(Box::new(
                    EInvariantSubtypingWithUseOpData {
                        sub_component: None,
                        lower_loc,
                        upper_loc,
                        lower_desc: type_or_type_desc::TypeOrTypeDescT::Type(l_prop_t.dupe()),
                        upper_desc: type_or_type_desc::TypeOrTypeDescT::Type(u_prop_t.dupe()),
                        use_op,
                        explanation,
                    },
                )),
            )?;
        }
        properties => {
            let t1 = Type::new(TypeInner::DefT(
                lreason.dupe(),
                DefT::new(DefTInner::ObjT(l_obj.dupe())),
            ));
            let t2 = Type::new(TypeInner::DefT(
                ureason.dupe(),
                DefT::new(DefTInner::ObjT(u_obj.dupe())),
            ));
            let lower_loc = match t1.deref() {
                TypeInner::OpenT(tvar) => {
                    match flow_js_utils::merge_tvar_opt(
                        cx,
                        false,
                        union_rep::UnionKind::ResolvedKind,
                        tvar.reason(),
                        tvar.id() as i32,
                    ) {
                        Some(t) => type_util::loc_of_t(&t).dupe(),
                        None => tvar.reason().loc().dupe(),
                    }
                }
                _ => type_util::loc_of_t(&t1).dupe(),
            };
            let upper_loc = type_util::loc_of_t(&t2).dupe();
            let properties: Vec<Name> = properties.iter().map(|(p, _, _)| p.dupe()).collect();
            let explanation =
                intermediate_error_types::ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperties {
                    lower_obj_loc: lreason.def_loc().dupe(),
                    upper_obj_loc: ureason.def_loc().dupe(),
                    lower_obj_desc: type_or_type_desc::TypeOrTypeDescT::Type(t1.dupe()),
                    upper_obj_desc: type_or_type_desc::TypeOrTypeDescT::Type(t2.dupe()),
                    upper_object_reason: ureason.dupe(),
                    properties: properties.clone(),
                };
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EInvariantSubtypingWithUseOp(Box::new(EInvariantSubtypingWithUseOpData {
                    sub_component: Some(
                        intermediate_error_types::SubComponentOfInvariantSubtypingError::ObjectProps(
                            properties,
                        ),
                    ),
                    lower_loc,
                    upper_loc,
                    lower_desc: type_or_type_desc::TypeOrTypeDescT::Type(t1),
                    upper_desc: type_or_type_desc::TypeOrTypeDescT::Type(t2),
                    use_op: use_op.dupe(),
                    explanation: Some(explanation),
                })),
            )
            ?;
        }
    }

    let (regular_missing, rhs_neutral_optional, ups_to_flow_any) =
        lhs_missing_props.into_iter().fold(
            (vec![], vec![], vec![]),
            |(mut regular_missing, mut rhs_neutral_optional, mut ups_to_flow_any): (
                Vec<Name>,
                Vec<Name>,
                Vec<Property>,
            ),
             (name, up)| {
                match up.deref() {
                    PropertyInner::Field(fd)
                        if fd.polarity == Polarity::Neutral
                            && matches!(fd.type_.deref(), TypeInner::OptionalT { .. }) =>
                    {
                        rhs_neutral_optional.push(name);
                        ups_to_flow_any.push(up);
                    }
                    _ => {
                        regular_missing.push(name);
                        ups_to_flow_any.push(up);
                    }
                }
                (regular_missing, rhs_neutral_optional, ups_to_flow_any)
            },
        );
    if let Ok(props) = Vec1::try_from_vec(regular_missing) {
        flow_js_utils::add_output(
            cx,
            ErrorMessage::EPropsNotFoundInSubtyping(Box::new(EPropsNotFoundInSubtypingData {
                prop_names: props,
                reason_lower: lreason.dupe(),
                reason_upper: ureason.dupe(),
                use_op: use_op.dupe(),
            })),
        )?;
    }
    if let Ok(props) = Vec1::try_from_vec(rhs_neutral_optional) {
        let t1 = Type::new(TypeInner::DefT(
            lreason.dupe(),
            DefT::new(DefTInner::ObjT(l_obj.dupe())),
        ));
        let t2 = Type::new(TypeInner::DefT(
            ureason.dupe(),
            DefT::new(DefTInner::ObjT(u_obj.dupe())),
        ));
        let lower_obj_loc = lreason.def_loc().dupe();
        let upper_obj_loc = ureason.def_loc().dupe();
        flow_js_utils::add_output(
            cx,
            ErrorMessage::EPropsNotFoundInInvariantSubtyping(Box::new(
                EPropsNotFoundInInvariantSubtypingData {
                    prop_names: props,
                    reason_lower: lreason.dupe(),
                    reason_upper: ureason.dupe(),
                    lower_obj_loc,
                    upper_obj_loc,
                    lower_obj_desc: type_or_type_desc::TypeOrTypeDescT::Type(t1),
                    upper_obj_desc: type_or_type_desc::TypeOrTypeDescT::Type(t2),
                    use_op: use_op.dupe(),
                },
            )),
        )?;
    }
    for up in &ups_to_flow_any {
        let any = any_t::error_of_kind(AnyErrorKind::UnresolvedName, ureason.dupe());
        match property::property_type(up) {
            PropertyType::OrdinaryField {
                type_: ut,
                polarity: Polarity::Neutral,
            } => {
                FlowJs::unify_opt(
                    cx,
                    Some(trace),
                    use_op.dupe(),
                    UnifyCause::Uncategorized,
                    None,
                    &any,
                    &ut,
                )?;
            }
            up_pt => {
                if let Some(ut) = property::read_t_of_property_type(&up_pt) {
                    FlowJs::flow_opt(
                        cx,
                        Some(trace),
                        &any,
                        &UseT::new(UseTInner::UseT(use_op.dupe(), ut)),
                    )?;
                }
                if let Some(ut) = property::write_t_of_property_type(&up_pt, None) {
                    FlowJs::flow_opt(
                        cx,
                        Some(trace),
                        &ut,
                        &UseT::new(UseTInner::UseT(use_op.dupe(), any.dupe())),
                    )?;
                }
            }
        }
    }

    add_output_prop_polarity_mismatch(cx, use_op.dupe(), lreason, ureason, polarity_mismatch_errs)?;

    // Any properties in l but not u must match indexer
    match &udict {
        None => {}
        Some(ud) => {
            let DictType {
                key,
                value,
                dict_polarity,
                ..
            } = *ud;
            let flow_prop_to_indexer =
                |lp: &Property, name: &Name| -> Result<(), FlowJsException> {
                    let use_op = VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                            PropertyCompatibilityData {
                                prop: Some(name.dupe()),
                                lower: lreason.dupe(),
                                upper: ureason.dupe(),
                            },
                        ))),
                        Arc::new(use_op.dupe()),
                    );
                    let lp_type = match lp.deref() {
                        PropertyInner::Field(fd)
                            if let TypeInner::OptionalT { type_: lt, .. } = fd.type_.deref() =>
                        {
                            PropertyType::OrdinaryField {
                                type_: lt.dupe(),
                                polarity: fd.polarity,
                            }
                        }
                        _ => property::property_type(lp),
                    };
                    let up_type = PropertyType::OrdinaryField {
                        type_: value.dupe(),
                        polarity: *dict_polarity,
                    };
                    if lit {
                        match (
                            property::read_t_of_property_type(&lp_type),
                            property::read_t_of_property_type(&up_type),
                        ) {
                            (Some(lt), Some(ut)) => {
                                FlowJs::rec_flow(
                                    cx,
                                    trace,
                                    &lt,
                                    &UseT::new(UseTInner::UseT(use_op.dupe(), ut)),
                                )?;
                            }
                            _ => {}
                        }
                    } else {
                        let propref = type_util::mk_named_prop(
                            lreason
                                .dupe()
                                .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe()))),
                            false,
                            name.dupe(),
                        );
                        let l_t = Type::new(TypeInner::DefT(
                            lreason.dupe(),
                            DefT::new(DefTInner::ObjT(l_obj.dupe())),
                        ));
                        let u_t = Type::new(TypeInner::DefT(
                            ureason.dupe(),
                            DefT::new(DefTInner::ObjT(u_obj.dupe())),
                        ));
                        let errs = rec_flow_p_inner(
                            cx,
                            Some(trace),
                            use_op.dupe(),
                            Some((&l_t, &u_t)),
                            ureason,
                            true,
                            &propref,
                            &lp_type,
                            &up_type,
                        )?;
                        add_output_prop_polarity_mismatch(cx, use_op, lreason, ureason, errs)?;
                    }
                    Ok(())
                };
            // If we are in implicit instantiation then we should always flow missing keys & value types to the
            // upper dictionary because that information may be useful to infer a type. Outside of implicit instantiation,
            // flowing both can cause redundant errors when the key is already not a valid indexer key, so we avoid the
            // value flows when that does not pass
            if !cx.in_implicit_instantiation() {
                for (name, lp) in cx.find_props(lflds.dupe()).iter() {
                    if !cx.has_prop(uflds.dupe(), name) {
                        let key_type = flow_js_utils::type_of_key_name(cx, name.dupe(), lreason);
                        if FlowJs::speculative_subtyping_succeeds(cx, &key_type, key)? {
                            flow_prop_to_indexer(lp, name)?;
                        } else {
                            flow_js_utils::add_output(
                                cx,
                                ErrorMessage::EIndexerCheckFailed(Box::new(
                                    EIndexerCheckFailedData {
                                        prop_name: name.dupe(),
                                        // Lower and upper are reversed in this case since
                                        // the lower object is the one requiring the prop.
                                        reason_lower: ureason.dupe(),
                                        reason_upper: lreason.dupe(),
                                        reason_indexer: type_util::reason_of_t(key).dupe(),
                                        use_op: use_op.dupe(),
                                    },
                                )),
                            )?;
                        }
                    }
                }
            } else {
                let mut keys_list: Vec<Type> = vec![];
                for (name, lp) in cx.find_props(lflds.dupe()).iter() {
                    if !cx.has_prop(uflds.dupe(), name) {
                        flow_prop_to_indexer(lp, name)?;
                        keys_list.push(flow_js_utils::type_of_key_name(cx, name.dupe(), lreason));
                    }
                }
                let keys = type_util::union_of_ts(lreason.dupe(), keys_list, None);
                FlowJs::rec_flow(
                    cx,
                    trace,
                    &keys,
                    &UseT::new(UseTInner::UseT(
                        VirtualUseOp::Frame(
                            Arc::new(VirtualFrameUseOp::IndexerKeyCompatibility {
                                lower: lreason.dupe(),
                                upper: ureason.dupe(),
                            }),
                            Arc::new(use_op.dupe()),
                        ),
                        key.dupe(),
                    )),
                )?;
            }
            // If the left is inexact and the right is indexed, Flow mixed to the indexer
            // value. Mixed represents the possibly unknown properties on the inexact object
            match &lflags.obj_kind {
                ObjKind::Inexact => {
                    let r = flow_common::reason::mk_reason(
                        VirtualReasonDesc::RUnknownUnspecifiedProperty(Arc::new(
                            lreason.desc(true).clone(),
                        )),
                        lreason.loc().dupe(),
                    );
                    let mixed = Type::new(TypeInner::DefT(
                        r,
                        DefT::new(DefTInner::MixedT(MixedFlavor::MixedEverything)),
                    ));
                    FlowJs::rec_flow_t(cx, trace, use_op.dupe(), &mixed, value)?;
                }
                _ => {}
            }
            // Previously, call properties were stored in the props map, and were
            // checked against dictionary upper bounds. This is wrong, but useful for
            // distinguishing between thunk-like types found in graphql-js.
            //
            // Now that call properties are stored separately, it is particularly
            // egregious to emit this constraint. This only serves to maintain buggy
            // behavior, which should be fixed, and this code removed.
            match (lcall, ucall) {
                (Some(lcall_id), None) => {
                    let name = Name::new("$call");
                    let use_op = VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                            PropertyCompatibilityData {
                                prop: Some(name.dupe()),
                                lower: lreason.dupe(),
                                upper: ureason.dupe(),
                            },
                        ))),
                        Arc::new(use_op.dupe()),
                    );
                    let call_t = cx.find_call(*lcall_id);
                    let lp_t = match call_t.deref() {
                        TypeInner::OptionalT { type_: t, .. } => t.dupe(),
                        _ => call_t.dupe(),
                    };
                    let lp_type = PropertyType::OrdinaryField {
                        type_: lp_t,
                        polarity: Polarity::Positive,
                    };
                    let up_type = PropertyType::OrdinaryField {
                        type_: value.dupe(),
                        polarity: *dict_polarity,
                    };
                    if lit {
                        match (
                            property::read_t_of_property_type(&lp_type),
                            property::read_t_of_property_type(&up_type),
                        ) {
                            (Some(lt), Some(ut)) => {
                                FlowJs::rec_flow(
                                    cx,
                                    trace,
                                    &lt,
                                    &UseT::new(UseTInner::UseT(use_op, ut)),
                                )?;
                            }
                            _ => {}
                        }
                    } else {
                        let reason_prop = lreason
                            .dupe()
                            .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe())));
                        let propref = type_util::mk_named_prop(reason_prop, false, name);
                        let l_t = Type::new(TypeInner::DefT(
                            lreason.dupe(),
                            DefT::new(DefTInner::ObjT(l_obj.dupe())),
                        ));
                        let u_t = Type::new(TypeInner::DefT(
                            ureason.dupe(),
                            DefT::new(DefTInner::ObjT(u_obj.dupe())),
                        ));
                        let errs = rec_flow_p_inner(
                            cx,
                            Some(trace),
                            use_op.dupe(),
                            Some((&l_t, &u_t)),
                            ureason,
                            true,
                            &propref,
                            &lp_type,
                            &up_type,
                        )?;
                        add_output_prop_polarity_mismatch(cx, use_op, lreason, ureason, errs)?;
                    }
                }
                _ => {}
            }
        }
    }

    let l_t = Type::new(TypeInner::DefT(
        lreason.dupe(),
        DefT::new(DefTInner::ObjT(l_obj.dupe())),
    ));
    FlowJs::rec_flow(
        cx,
        trace,
        uproto,
        &UseT::new(UseTInner::ReposUseT(Box::new(ReposUseTData {
            reason: ureason.dupe(),
            use_desc: false,
            use_op,
            type_: l_t,
        }))),
    )?;
    Ok(())
}

// mutable sites on parent values (i.e. object properties,
// array elements) must be typed invariantly when a value
// flows to the parent, unless the incoming value is fresh,
// in which case covariant typing is sound (since no alias
// will break if the subtyped child value is replaced by a
// non-subtyped value
fn flow_to_mutable_child<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    unify_cause: UnifyCause,
    fresh: bool,
    t1: &Type,
    t2: &Type,
) -> Result<(), FlowJsException> {
    if fresh {
        FlowJs::rec_flow(
            cx,
            trace,
            t1,
            &UseT::new(UseTInner::UseT(use_op, t2.dupe())),
        )?;
    } else {
        FlowJs::rec_unify(cx, trace, use_op, unify_cause, None, t1, t2)?;
    }
    Ok(())
}

// Subtyping of arrays is complicated by tuples. Currently, there are three
// different kinds of types, all encoded by arrays:
//
//    1. Array<T> (array type)
//    2. [T1, T2] (tuple type)
//    3. "internal" Array<X>[T1, T2] where T1 | T2 ~> X (array literal type)
//
// We have the following rules:
//
// (1) When checking types against Array<U>, the rules are not surprising. Array
// literal types behave like array types in these checks.
//
//    * Array<T> ~> Array<U> checks T <~> U
//    * [T1, T2] ~> Array<U> checks T1 | T2 ~> U
//    * Array<X>[T1, T2] ~> Array<U> checks Array<X> ~> Array<U>
//
// (2) When checking types against [T1, T2], the rules are again not
// surprising. Array literal types behave like tuple types in these checks. We
// consider missing tuple elements to be undefined, following common usage (and
// consistency with missing call arguments).
//
//    * Array<T> ~> [U1, U2] checks T ~> U1, T ~> U2
//    * [T1, T2] ~> [U1, U2] checks T1 ~> U1 and T2 ~> U2
//    * [T1, T2] ~> [U1] checks T1 ~> U1
//    * [T1] ~> [U1, U2] checks T1 ~> U1 and void ~> U2
//    * Array<X>[T1, T2] ~> [U1, U2] checks [T1, T2] ~> [U1, U2]
//
// (3) When checking types against Array<Y>[U1, U2], the rules are a bit
// unsound. Array literal types were not designed to appear as upper bounds. In
// particular, their summary element types are often overly precise. Checking
// individual element types of one array literal type against the summary
// element type of another array literal type can lead to crazy errors, so we
// currently drop such checks.
//
// TODO: Make these rules great again by computing more reasonable summary
// element types for array literal types.
//
//    * Array<T> ~> Array<Y>[U1, U2] checks Array<T> ~> Array<Y>
//    * [T1, T2] ~> Array<Y>[U1, U2] checks T1 ~> U1, T2 ~> U2
//    * [T1, T2] ~> Array<Y>[U1] checks T1 ~> U1
//    * [T1] ~> Array<Y>[U1, U2] checks T1 ~> U1
//    * Array<X>[T1, T2] ~> Array<Y>[U1, U2] checks [T1, T2] ~> Array<Y>[U1, U2]
fn array_flow<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    lit1: bool,
    _r1: &Reason,
    r2: &Reason,
    l: &Type,
    u: &Type,
    ts1: &[Type],
    e1: &Type,
    ts2: &[Type],
    e2: &Type,
) -> Result<(), FlowJsException> {
    let mut iter1 = ts1.iter();
    let mut iter2 = ts2.iter();
    let mut index = 0usize;
    loop {
        match (iter1.next(), iter2.next()) {
            (None, _) => {
                if index == 0 {
                    flow_to_mutable_child(
                        cx,
                        trace,
                        use_op,
                        UnifyCause::MutableArray {
                            lower_array_t: l.dupe(),
                            upper_array_t: u.dupe(),
                            upper_array_reason: r2.dupe(),
                        },
                        lit1,
                        e1,
                        e2,
                    )?;
                }
                // otherwise, lower bound is an empty tuple (nothing to do)
                break;
            }
            (_, None) => {
                flow_to_mutable_child(
                    cx,
                    trace,
                    use_op,
                    UnifyCause::MutableArray {
                        lower_array_t: l.dupe(),
                        upper_array_t: u.dupe(),
                        upper_array_reason: r2.dupe(),
                    },
                    lit1,
                    e1,
                    e2,
                )?;
                break;
            }
            (Some(t1_elem), Some(t2_elem)) => {
                flow_to_mutable_child(
                    cx,
                    trace,
                    use_op.dupe(),
                    UnifyCause::Uncategorized,
                    lit1,
                    t1_elem,
                    t2_elem,
                )?;
                index += 1;
            }
        }
    }
    Ok(())
}

fn take_n_from_set(n: usize, set: &UnionEnumSet) -> Vec<UnionEnum> {
    set.iter().take(n).cloned().collect()
}

pub fn union_to_union<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    l: &Type,
    rep: &union_rep::UnionRep,
    u: &Type,
) -> Result<(), FlowJsException> {
    let guard_result = flow_js_utils::union_optimization_guard(
        cx,
        |t1, t2| type_util::quick_subtype(None::<&fn(&Type)>, t1, t2),
        l,
        u,
    );
    match guard_result {
        flow_js_utils::UnionOptimizationGuardResult::True => {
            if cx.is_verbose() {
                eprintln!("UnionT ~> UnionT fast path (True)");
            }
        }
        flow_js_utils::UnionOptimizationGuardResult::Maybe => {
            flow_all_in_union(
                cx,
                &trace,
                rep,
                &UseT::new(UseTInner::UseT(use_op, u.dupe())),
            )?;
        }
        flow_js_utils::UnionOptimizationGuardResult::False { diff } => {
            if cx.is_verbose() {
                eprintln!("UnionT ~> UnionT fast path (False)");
            }
            let reason_lower = type_util::reason_of_t(l).dupe();
            let reason_upper = type_util::reason_of_t(u).dupe();
            let members: Vec<flow_data_structure_wrapper::smol_str::FlowSmolStr> =
                take_n_from_set(3, &diff)
                    .iter()
                    .map(|e| e.to_string().into())
                    .collect();
            let extra_number = std::cmp::max(0, diff.len() as i32 - 3);
            let explanation = Some(
                intermediate_error_types::Explanation::ExplanationAdditionalUnionMembers(Box::new(
                    intermediate_error_types::ExplanationAdditionalUnionMembersData {
                        left: reason_lower.dupe(),
                        right: reason_upper.dupe(),
                        members,
                        extra_number,
                    },
                )),
            );
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                    reason_lower,
                    reason_upper,
                    use_op,
                    explanation,
                })),
            )?;
        }
    }
    Ok(())
}

pub fn rec_sub_t<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    l: &Type,
    u: &Type,
    trace: DepthTrace,
) -> Result<(), FlowJsException> {
    match (l.deref(), u.deref()) {
        // The sink component of an annotation constrains values flowing
        // into the annotated site.
        (_, TypeInner::AnnotT(r, t, use_desc)) => FlowJs::rec_flow(
            cx,
            trace,
            t,
            &UseT::new(UseTInner::ReposUseT(Box::new(ReposUseTData {
                reason: r.dupe(),
                use_desc: *use_desc,
                use_op,
                type_: l.dupe(),
            }))),
        ),
        (TypeInner::AnnotT(r, t, use_desc), _) => {
            let t = FlowJs::reposition_reason(cx, Some(trace), r, Some(*use_desc), t)?;
            FlowJs::rec_flow_t(cx, trace, use_op, &t, u)
        }

        // *******************************
        // * common implicit conversions *
        // *******************************
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!(
                ld.deref(),
                DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
            ) && matches!(ud.deref(), DefTInner::NumGeneralT(_)) =>
        {
            Ok(())
        }
        (TypeInner::DefT(r, ld), TypeInner::MaybeT(_, tout))
            if matches!(ld.deref(), DefTInner::NullT | DefTInner::VoidT) =>
        {
            let empty = Type::new(TypeInner::DefT(r.dupe(), DefT::new(DefTInner::EmptyT)));
            FlowJs::rec_flow_t(cx, trace, use_op, &empty, tout)
        }
        (TypeInner::DefT(r, ld), TypeInner::MaybeT(_, tout))
            if matches!(ld.deref(), DefTInner::MixedT(MixedFlavor::MixedEverything)) =>
        {
            let mixed = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::MixedT(MixedFlavor::MixedNonMaybe)),
            ));
            FlowJs::rec_flow_t(cx, trace, use_op, &mixed, tout)
        }
        (TypeInner::MaybeT(r, t), TypeInner::MaybeT(_, _)) => {
            let t = type_util::push_type_alias_reason(r, t.dupe());
            FlowJs::rec_flow_t(cx, trace, use_op, &t, u)
        }
        (TypeInner::MaybeT(reason, t), _) => {
            let reason = reason.dupe().replace_desc(VirtualReasonDesc::RNullOrVoid);
            let t = type_util::push_type_alias_reason(&reason, t.dupe());
            let null = Type::new(TypeInner::DefT(reason.dupe(), DefT::new(DefTInner::NullT)));
            let void = Type::new(TypeInner::DefT(reason.dupe(), DefT::new(DefTInner::VoidT)));
            FlowJs::rec_flow_t(cx, trace, use_op.dupe(), &null, u)?;
            FlowJs::rec_flow_t(cx, trace, use_op.dupe(), &void, u)?;
            FlowJs::rec_flow_t(cx, trace, use_op, &t, u)
        }
        (TypeInner::DefT(r, ld), TypeInner::OptionalT { type_: tout, .. })
            if matches!(ld.deref(), DefTInner::VoidT) =>
        {
            let empty = Type::new(TypeInner::DefT(r.dupe(), DefT::new(DefTInner::EmptyT)));
            FlowJs::rec_flow_t(cx, trace, use_op, &empty, tout)
        }
        (TypeInner::OptionalT { type_: t, .. }, TypeInner::OptionalT { .. })
        | (TypeInner::OptionalT { type_: t, .. }, TypeInner::MaybeT(_, _)) => {
            FlowJs::rec_flow_t(cx, trace, use_op, t, u)
        }
        (
            TypeInner::OptionalT {
                reason,
                type_: t,
                use_desc,
            },
            _,
        ) => {
            let void = void::why_with_use_desc(*use_desc, reason.dupe());
            FlowJs::rec_flow_t(cx, trace, use_op.dupe(), &void, u)?;
            FlowJs::rec_flow_t(cx, trace, use_op, t, u)
        }
        (
            TypeInner::ThisTypeAppT(box ThisTypeAppTData {
                reason,
                type_: c,
                this_t,
                targs,
            }),
            _,
        ) => {
            let reason_op = type_util::reason_of_t(u);
            FlowJs::instantiate_this_class(
                cx,
                trace,
                reason_op,
                reason,
                c,
                targs.dupe(),
                this_t,
                &Cont::Upper(Box::new(UseT::new(UseTInner::UseT(use_op, u.dupe())))),
            )?;
            Ok(())
        }
        (
            _,
            TypeInner::ThisTypeAppT(box ThisTypeAppTData {
                reason,
                type_: c,
                this_t,
                targs,
            }),
        ) => {
            let reason_op = type_util::reason_of_t(l);
            FlowJs::instantiate_this_class(
                cx,
                trace,
                reason_op,
                reason,
                c,
                targs.dupe(),
                this_t,
                &Cont::Lower(use_op, l.dupe()),
            )?;
            Ok(())
        }

        // When a subtyping question involves a union appearing on the right or an
        // intersection appearing on the left, the simplification rules are
        // imprecise: we split the union / intersection into cases and try to prove
        // that the subtyping question holds for one of the cases, but each of those
        // cases may be unprovable, which might lead to spurious errors. In
        // particular, obvious assertions such as (A | B) & C is a subtype of A | B
        // cannot be proved if we choose to split the union first (discharging
        // unprovable subgoals of (A | B) & C being a subtype of either A or B);
        // dually, obvious assertions such as A & B is a subtype of (A & B) | C
        // cannot be proved if we choose to simplify the intersection first
        // (discharging unprovable subgoals of either A or B being a subtype of (A &
        // B) | C). So instead, we try inclusion rules to handle such cases.
        //
        // An orthogonal benefit is that for large unions or intersections, checking
        // inclusion is significantly faster that splitting for proving simple
        // inequalities (O(n) instead of O(n^2) for n cases).
        (TypeInner::IntersectionT(_, rep), _)
            if rep
                .members_iter()
                .any(|member| flow_typing_flow_common::concrete_type_eq::eq(cx, member, u)) =>
        {
            Ok(())
        }

        // If we have a TypeAppT (c, ts) ~> TypeAppT (c, ts) then we want to
        // concretize both cs to PolyTs so that we may referentially compare them.
        // We cannot compare the non-concretized versions since they may have been
        // reposition, they may be two OpenTs from different locations, or any other
        // way you can access the same PolyT via different means that results in a
        // different c being passed to TypeAppT.
        //
        // We use the ConcretizeTypeAppsT use type to concretize both the c of our
        // upper and lower TypeAppT bound. We start by concretizing the upper bound
        // which we signal by setting the final element in ConcretizeTypeAppsT to
        // true.
        //
        // The next step happens back in flow_js.ml, at the cases for a
        // ConcretizeTypeAppsT use type.
        (
            TypeInner::TypeAppT(box TypeAppTData {
                reason: r1,
                use_op: op1,
                type_: c1,
                targs: ts1,
                from_value: fv1,
                use_desc: _,
            }),
            TypeInner::TypeAppT(box TypeAppTData {
                reason: r2,
                use_op: op2,
                type_: c2,
                targs: ts2,
                from_value: fv2,
                use_desc: _,
            }),
        ) => {
            if instantiation_utils::type_app_expansion::push_unless_loop(
                cx,
                type_app_expansion::Bound::Lower,
                c1,
                ts1,
            ) {
                if instantiation_utils::type_app_expansion::push_unless_loop(
                    cx,
                    type_app_expansion::Bound::Upper,
                    c2,
                    ts2,
                ) {
                    FlowJs::rec_flow(
                        cx,
                        trace,
                        c2,
                        &UseT::new(UseTInner::ConcretizeTypeAppsT(
                            use_op.dupe(),
                            Box::new((ts2.dupe(), *fv2, op2.dupe(), r2.dupe())),
                            Box::new((c1.dupe(), ts1.dupe(), *fv1, op1.dupe(), r1.dupe())),
                            true,
                        )),
                    )?;
                    instantiation_utils::type_app_expansion::pop(cx);
                }
                instantiation_utils::type_app_expansion::pop(cx);
            }
            Ok(())
        }
        (
            TypeInner::TypeAppT(box TypeAppTData {
                reason: reason_tapp,
                use_op: use_op_tapp,
                type_,
                targs,
                from_value,
                use_desc: type_app_use_desc,
            }),
            _,
        ) => {
            let reason_op = type_util::reason_of_t(u);
            if instantiation_utils::type_app_expansion::push_unless_loop(
                cx,
                type_app_expansion::Bound::Lower,
                type_,
                targs,
            ) {
                let inst = FlowJs::mk_typeapp_instance(
                    cx,
                    Some(trace),
                    use_op_tapp.dupe(),
                    reason_op,
                    reason_tapp,
                    *from_value,
                    type_,
                    targs.dupe(),
                )?;
                let t = FlowJs::reposition_reason(
                    cx,
                    Some(trace),
                    reason_tapp,
                    Some(*type_app_use_desc),
                    &inst,
                )?;
                FlowJs::rec_flow_t(cx, trace, use_op, &t, u)?;
                instantiation_utils::type_app_expansion::pop(cx);
            }
            Ok(())
        }
        (
            _,
            TypeInner::TypeAppT(box TypeAppTData {
                reason: reason_tapp,
                use_op: use_op_tapp,
                type_,
                targs,
                from_value,
                use_desc: type_app_use_desc,
            }),
        ) => {
            let reason_op = type_util::reason_of_t(l);
            if instantiation_utils::type_app_expansion::push_unless_loop(
                cx,
                type_app_expansion::Bound::Upper,
                type_,
                targs,
            ) {
                let t = FlowJs::mk_typeapp_instance(
                    cx,
                    Some(trace),
                    use_op_tapp.dupe(),
                    reason_op,
                    reason_tapp,
                    *from_value,
                    type_,
                    targs.dupe(),
                )?;
                // We do the slingshot trick here so that we flow l to the results of making the typeapp
                // instead of adding another lower bound to t. We can't use an Annot here, which would do
                // that for us, because ts may not be 0->1, so using them to make an Annot would break
                // invariants that we rely on. In particular, it would force us to traverse AnnotTs to
                // do any propagation, which is extremely costly.
                FlowJs::rec_flow(
                    cx,
                    trace,
                    &t,
                    &UseT::new(UseTInner::ReposUseT(Box::new(ReposUseTData {
                        reason: reason_tapp.dupe(),
                        use_desc: *type_app_use_desc,
                        use_op,
                        type_: l.dupe(),
                    }))),
                )?;
                instantiation_utils::type_app_expansion::pop(cx);
            }
            Ok(())
        }

        // *********************
        //    opaque types
        // *********************

        // If the ids are equal, we use flow_type_args to make sure that the type arguments of each
        // are compatible with each other. If there are no type args, this doesn't do anything
        (
            TypeInner::NominalT {
                reason: lreason,
                nominal_type: lnom,
            },
            TypeInner::NominalT {
                reason: ureason,
                nominal_type: unom,
            },
        ) if lnom.nominal_id == unom.nominal_id => {
            match &lnom.nominal_id {
                // When we are subtyping between two stuck EvalT, we just want to give a simple yes/no answer.
                // Complex subtyping on these stuck EvalTs is not supported
                nominal::Id::StuckEval(_) if !cx.in_implicit_instantiation() => {
                    match speculation_kit::try_singleton_custom_throw_on_failure(
                        cx,
                        Box::new({
                            let use_op = use_op.dupe();
                            let lreason = lreason.dupe();
                            let ureason = ureason.dupe();
                            let ltargs = lnom.nominal_type_args.dupe();
                            let utargs = unom.nominal_type_args.dupe();
                            move |cx: &Context| {
                                FlowJs::flow_type_args(
                                    cx, trace, use_op, &lreason, &ureason, ltargs, utargs,
                                )
                            }
                        }),
                    ) {
                        Ok(()) => {}
                        Err(FlowJsException::SpeculationSingletonError) => {
                            flow_js_utils::add_output(
                                cx,
                                ErrorMessage::EIncompatibleWithUseOp(Box::new(
                                    EIncompatibleWithUseOpData {
                                        reason_lower: lreason.dupe(),
                                        reason_upper: ureason.dupe(),
                                        use_op,
                                        explanation: None,
                                    },
                                )),
                            )?;
                        }
                        Err(other) => return Err(other),
                    }
                }
                _ => {
                    FlowJs::flow_type_args(
                        cx,
                        trace,
                        use_op,
                        lreason,
                        ureason,
                        lnom.nominal_type_args.dupe(),
                        unom.nominal_type_args.dupe(),
                    )?;
                }
            }
            Ok(())
        }
        // If the opaque type are from the same logical module, we need to do some structural validation
        // in additional to type_args check.
        (
            TypeInner::NominalT {
                reason: lreason,
                nominal_type: lnom,
            },
            TypeInner::NominalT {
                reason: ureason,
                nominal_type: unom,
            },
        ) if let (
            nominal::Id::UserDefinedOpaqueTypeId(box nominal::UserDefinedOpaqueTypeIdData(
                id1,
                name1,
            )),
            nominal::Id::UserDefinedOpaqueTypeId(box nominal::UserDefinedOpaqueTypeIdData(
                id2,
                name2,
            )),
        ) = (&lnom.nominal_id, &unom.nominal_id)
            && type_util::nominal_id_have_same_logical_module(
                &cx.file_options(),
                cx.projects_options(),
                (id1, Some(name1.as_str())),
                (id2, Some(name2.as_str())),
            )
            && lnom.nominal_type_args.len() == unom.nominal_type_args.len() =>
        {
            // Check super
            if type_util::is_in_common_interface_conformance_check(&use_op) {
                let lower_1 = lnom.lower_t.as_ref().duped().unwrap_or_else(|| {
                    Type::new(TypeInner::DefT(
                        lreason.dupe(),
                        DefT::new(DefTInner::EmptyT),
                    ))
                });
                let lower_2 = unom.lower_t.as_ref().duped().unwrap_or_else(|| {
                    Type::new(TypeInner::DefT(
                        ureason.dupe(),
                        DefT::new(DefTInner::EmptyT),
                    ))
                });
                let lower_1_reason = type_util::reason_of_t(&lower_1).dupe();
                FlowJs::rec_unify(
                    cx,
                    trace,
                    VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::OpaqueTypeLowerBoundCompatibility {
                            lower: lower_1_reason.dupe(),
                            upper: lower_1_reason,
                        }),
                        Arc::new(use_op.dupe()),
                    ),
                    UnifyCause::Uncategorized,
                    None,
                    &lower_1,
                    &lower_2,
                )?;
                let upper_1 = lnom.upper_t.as_ref().duped().unwrap_or_else(|| {
                    Type::new(TypeInner::DefT(
                        lreason.dupe(),
                        DefT::new(DefTInner::MixedT(MixedFlavor::MixedEverything)),
                    ))
                });
                let upper_2 = unom.upper_t.as_ref().duped().unwrap_or_else(|| {
                    Type::new(TypeInner::DefT(
                        ureason.dupe(),
                        DefT::new(DefTInner::MixedT(MixedFlavor::MixedEverything)),
                    ))
                });
                FlowJs::rec_unify(
                    cx,
                    trace,
                    VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::OpaqueTypeUpperBoundCompatibility {
                            lower: type_util::reason_of_t(&upper_1).dupe(),
                            upper: type_util::reason_of_t(&upper_2).dupe(),
                        }),
                        Arc::new(use_op.dupe()),
                    ),
                    UnifyCause::Uncategorized,
                    None,
                    &upper_1,
                    &upper_2,
                )?;
            }
            // Do not check underlying type even if we have access to them, because underlying types
            // are not visible across module boundaries.
            FlowJs::flow_type_args(
                cx,
                trace,
                use_op,
                lreason,
                ureason,
                lnom.nominal_type_args.dupe(),
                unom.nominal_type_args.dupe(),
            )
        }
        // If the type is still in the same file it was defined, we allow it to
        // expose its underlying type information
        (
            TypeInner::NominalT {
                nominal_type: lnom, ..
            },
            _,
        ) if matches!(
            (&lnom.nominal_id, &lnom.underlying_t),
            (
                nominal::Id::UserDefinedOpaqueTypeId(box nominal::UserDefinedOpaqueTypeIdData(
                    _,
                    _,
                )),
                nominal::UnderlyingT::OpaqueWithLocal { .. },
            )
        ) && {
            if let nominal::Id::UserDefinedOpaqueTypeId(box nominal::UserDefinedOpaqueTypeIdData(
                nominal_id,
                _,
            )) = &lnom.nominal_id
            {
                nominal_id.0.source() == Some(cx.file())
            } else {
                false
            }
        } =>
        {
            if let nominal::UnderlyingT::OpaqueWithLocal { t } = &lnom.underlying_t {
                FlowJs::rec_flow_t(cx, trace, use_op, t, u)?;
            }
            Ok(())
        }
        // If the lower bound is in the same file as where the opaque type was defined,
        // we expose the underlying type information
        (
            _,
            TypeInner::NominalT {
                nominal_type: unom, ..
            },
        ) if matches!(
            (&unom.nominal_id, &unom.underlying_t),
            (
                nominal::Id::UserDefinedOpaqueTypeId(box nominal::UserDefinedOpaqueTypeIdData(
                    _,
                    _,
                )),
                nominal::UnderlyingT::OpaqueWithLocal { .. },
            )
        ) && {
            if let nominal::Id::UserDefinedOpaqueTypeId(box nominal::UserDefinedOpaqueTypeIdData(
                nominal_id,
                _,
            )) = &unom.nominal_id
            {
                nominal_id.0.source() == Some(cx.file())
            } else {
                false
            }
        } =>
        {
            if let nominal::UnderlyingT::OpaqueWithLocal { t } = &unom.underlying_t {
                FlowJs::rec_flow_t(cx, trace, use_op, l, t)?;
            }
            Ok(())
        }
        // Opaque type for custom error types are always fully transparent
        (
            TypeInner::NominalT {
                nominal_type: lnom, ..
            },
            _,
        ) if let nominal::UnderlyingT::CustomError(box nominal::CustomErrorData { t, .. }) =
            &lnom.underlying_t =>
        {
            FlowJs::rec_flow_t(cx, trace, use_op, t, u)
        }
        (
            _,
            TypeInner::NominalT {
                reason: ru,
                nominal_type: unom,
            },
        ) if let (
            nominal::Id::UserDefinedOpaqueTypeId(box nominal::UserDefinedOpaqueTypeIdData(_, name)),
            nominal::UnderlyingT::CustomError(box nominal::CustomErrorData {
                t,
                custom_error_loc,
            }),
        ) = (&unom.nominal_id, &unom.underlying_t) =>
        {
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::OpaqueTypeCustomErrorCompatibility(
                    Box::new(OpaqueTypeCustomErrorCompatibilityData {
                        lower: type_util::reason_of_t(l).dupe(),
                        upper: ru.dupe(),
                        lower_t: type_or_type_desc::TypeOrTypeDescT::Type(l.dupe()),
                        upper_t: type_or_type_desc::TypeOrTypeDescT::Type(u.dupe()),
                        name: name.dupe(),
                        custom_error_loc: custom_error_loc.dupe(),
                    }),
                )),
                Arc::new(use_op),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, l, t)
        }

        // **********************
        // Numeric string keys
        // **********************

        // This type is only to be used to represent numeric-like object keys in
        // the context of object-to-object subtyping.
        // It is a subtype of both string and number, so both of these are OK:
        // ```js
        // const o = {1: true};
        // o as {[number]: boolean}: // OK
        // o as {[string]: boolean}: // OK
        // ```
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if let (
                DefTInner::NumericStrKeyT(actual_lit),
                DefTInner::SingletonNumT {
                    value: expected_lit,
                    ..
                },
            ) = (ld.deref(), ud.deref()) =>
        {
            if actual_lit.0 != expected_lit.0 {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EExpectedNumberLit(Box::new(EExpectedNumberLitData {
                        reason_lower: rl.dupe(),
                        reason_upper: ru.dupe(),
                        use_op,
                    })),
                )?;
            }
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if let (
                DefTInner::NumericStrKeyT(actual_lit),
                DefTInner::SingletonStrT {
                    value: expected, ..
                },
            ) = (ld.deref(), ud.deref()) =>
        {
            if Name::new(actual_lit.1.dupe()) != *expected {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EExpectedStringLit(Box::new(EExpectedStringLitData {
                        reason_lower: rl.dupe(),
                        reason_upper: ru.dupe(),
                        use_op,
                    })),
                )?;
            }
            Ok(())
        }
        (_, TypeInner::DefT(r, ud)) if let DefTInner::NumericStrKeyT(num_lit) = ud.deref() => {
            let u_new = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::SingletonStrT {
                    value: Name::new(num_lit.1.as_str()),
                    from_annot: false,
                }),
            ));
            FlowJs::rec_flow_t(cx, trace, use_op, l, &u_new)
        }

        // **********************
        // Singletons and keys
        // **********************

        // Finite keysets over arbitrary objects can be represented by KeysT. While
        // it is possible to also represent singleton string types using KeysT (by
        // taking the keyset of an object with a single property whose key is that
        // string and whose value is ignored), we can model them more directly
        // using SingletonStrT. Specifically, SingletonStrT models a type
        // annotation that looks like a string literal, which describes a singleton
        // set containing that string literal. Going further, other uses of KeysT
        // where the underlying object is created solely for the purpose of
        // describing a keyset can be modeled using unions of singleton strings.
        //
        // One may also legitimately wonder why SingletonStrT(_, key) cannot be
        // always replaced by SingletonStrT(Some key). The reason is that types of the
        // latter form (string literal types) are inferred to be the type of string
        // literals appearing as values, and we don't want to prematurely narrow
        // down the type of the location where such values may appear, since that
        // would preclude other strings to be stored in that location. Thus, by
        // necessity we allow all string types to flow to SingletonStrT (whereas only
        // exactly matching string literal types may flow to SingletonStrT).
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if let (
                DefTInner::SingletonStrT { value: actual, .. },
                DefTInner::SingletonStrT {
                    value: expected, ..
                },
            ) = (ld.deref(), ud.deref()) =>
        {
            if expected == actual {
                flow_js_utils::update_lit_type_from_annot(cx, l);
            } else {
                // TODO: ordered_reasons should not be necessary
                let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EExpectedStringLit(Box::new(EExpectedStringLitData {
                        reason_lower: rl,
                        reason_upper: ru,
                        use_op,
                    })),
                )?;
            }
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if matches!(ld.deref(), DefTInner::StrGeneralT(_))
                && matches!(ud.deref(), DefTInner::SingletonStrT { .. }) =>
        {
            // TODO: ordered_reasons should not be necessary
            let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EExpectedStringLit(Box::new(EExpectedStringLitData {
                    reason_lower: rl,
                    reason_upper: ru,
                    use_op,
                })),
            )?;
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if let (
                DefTInner::SingletonNumT { value: actual, .. },
                DefTInner::SingletonNumT {
                    value: expected, ..
                },
            ) = (ld.deref(), ud.deref()) =>
        {
            if expected.0 == actual.0 {
                flow_js_utils::update_lit_type_from_annot(cx, l);
            } else {
                // TODO: ordered_reasons should not be necessary
                let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EExpectedNumberLit(Box::new(EExpectedNumberLitData {
                        reason_lower: rl,
                        reason_upper: ru,
                        use_op,
                    })),
                )?;
            }
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if matches!(ld.deref(), DefTInner::NumGeneralT(_))
                && matches!(ud.deref(), DefTInner::SingletonNumT { .. }) =>
        {
            // TODO: ordered_reasons should not be necessary
            let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EExpectedNumberLit(Box::new(EExpectedNumberLitData {
                    reason_lower: rl,
                    reason_upper: ru,
                    use_op,
                })),
            )?;
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if let (
                DefTInner::SingletonBoolT { value: actual, .. },
                DefTInner::SingletonBoolT {
                    value: expected, ..
                },
            ) = (ld.deref(), ud.deref()) =>
        {
            if expected != actual {
                // TODO: ordered_reasons should not be necessary
                let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EExpectedBooleanLit(Box::new(EExpectedBooleanLitData {
                        reason_lower: rl,
                        reason_upper: ru,
                        use_op,
                    })),
                )?;
            }
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if matches!(ld.deref(), DefTInner::BoolGeneralT)
                && matches!(ud.deref(), DefTInner::SingletonBoolT { .. }) =>
        {
            // TODO: ordered_reasons should not be necessary
            let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EExpectedBooleanLit(Box::new(EExpectedBooleanLitData {
                    reason_lower: rl,
                    reason_upper: ru,
                    use_op,
                })),
            )
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if let (
                DefTInner::SingletonBigIntT { value: actual, .. },
                DefTInner::SingletonBigIntT {
                    value: expected, ..
                },
            ) = (ld.deref(), ud.deref()) =>
        {
            if expected.0 != actual.0 {
                // TODO: ordered_reasons should not be necessary
                let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EExpectedBigIntLit(Box::new(EExpectedBigIntLitData {
                        reason_lower: rl,
                        reason_upper: ru,
                        use_op,
                    })),
                )?;
            }
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if matches!(ld.deref(), DefTInner::BigIntGeneralT(_))
                && matches!(ud.deref(), DefTInner::SingletonBigIntT { .. }) =>
        {
            // TODO: ordered_reasons should not be necessary
            let (rl, ru) = ordered_reasons((rl.dupe(), ru.dupe()));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EExpectedBigIntLit(Box::new(EExpectedBigIntLitData {
                    reason_lower: rl,
                    reason_upper: ru,
                    use_op,
                })),
            )
        }

        // **********************************
        // * unique symbol ~> unique symbol *
        // **********************************
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if let (DefTInner::UniqueSymbolT(id1), DefTInner::UniqueSymbolT(id2)) =
                (ld.deref(), ud.deref())
                && id1 == id2 =>
        {
            Ok(())
        }
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if matches!(ld.deref(), DefTInner::UniqueSymbolT(_))
                && matches!(ud.deref(), DefTInner::UniqueSymbolT(_)) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                    reason_lower: rl.dupe(),
                    reason_upper: ru.dupe(),
                    use_op,
                    explanation: None,
                })),
            )
        }
        // symbol ~> unique symbol: ERROR
        (TypeInner::DefT(rl, ld), TypeInner::DefT(ru, ud))
            if matches!(ld.deref(), DefTInner::SymbolT)
                && matches!(ud.deref(), DefTInner::UniqueSymbolT(_)) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                    reason_lower: rl.dupe(),
                    reason_upper: ru.dupe(),
                    use_op,
                    explanation: None,
                })),
            )
        }
        // ****************************************************
        // keys (NOTE: currently we only support string keys)
        // ****************************************************
        (TypeInner::DefT(reason_s, ld), TypeInner::KeysT(reason_op, o))
            if let DefTInner::SingletonStrT { value: x, .. } = ld.deref() =>
        {
            flow_js_utils::update_lit_type_from_annot(cx, l);
            let reason_next = reason_s
                .dupe()
                .replace_desc_new(VirtualReasonDesc::RProperty(Some(x.dupe())));
            // check that o has key x
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_next,
                type_: l.dupe(),
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (
            TypeInner::GenericT(box GenericTData {
                reason: reason_s,
                bound,
                ..
            }),
            TypeInner::KeysT(reason_op, o),
        ) if let TypeInner::DefT(_, bd) = bound.deref()
            && let DefTInner::SingletonStrT { value: x, .. } = bd.deref() =>
        {
            flow_js_utils::update_lit_type_from_annot(cx, l);
            let reason_next = reason_s
                .dupe()
                .replace_desc_new(VirtualReasonDesc::RProperty(Some(x.dupe())));
            // check that o has key x
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_next,
                type_: l.dupe(),
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (TypeInner::DefT(reason_s, ld), TypeInner::KeysT(reason_op, o))
            if matches!(ld.deref(), DefTInner::StrGeneralT(_)) =>
        {
            let reason_next = reason_s
                .dupe()
                .replace_desc_new(VirtualReasonDesc::RUnknownString);
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_next,
                type_: l.dupe(),
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (
            TypeInner::GenericT(box GenericTData {
                reason: reason_s,
                bound,
                ..
            }),
            TypeInner::KeysT(reason_op, o),
        ) if matches!(
            bound.deref(),
            TypeInner::DefT(_, d) if matches!(d.deref(), DefTInner::StrGeneralT(_))
        ) =>
        {
            let reason_next = reason_s
                .dupe()
                .replace_desc_new(VirtualReasonDesc::RUnknownString);
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_next,
                type_: l.dupe(),
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (TypeInner::DefT(reason_s, ld), TypeInner::KeysT(reason_op, o))
            if let DefTInner::NumericStrKeyT(num_lit) = ld.deref() =>
        {
            let reason_next = reason_s
                .dupe()
                .replace_desc_new(VirtualReasonDesc::RProperty(Some(Name::new(
                    num_lit.1.as_str(),
                ))));
            let l_new = Type::new(TypeInner::DefT(
                reason_s.dupe(),
                DefT::new(DefTInner::SingletonStrT {
                    value: Name::new(num_lit.1.as_str()),
                    from_annot: false,
                }),
            ));
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_next,
                type_: l_new,
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (
            TypeInner::GenericT(box GenericTData {
                reason: reason_s,
                bound,
                ..
            }),
            TypeInner::KeysT(reason_op, o),
        ) if let TypeInner::DefT(_, bd) = bound.deref()
            && let DefTInner::NumericStrKeyT(num_lit) = bd.deref() =>
        {
            let reason_next = reason_s
                .dupe()
                .replace_desc_new(VirtualReasonDesc::RProperty(Some(Name::new(
                    num_lit.1.as_str(),
                ))));
            let l_new = Type::new(TypeInner::DefT(
                reason_s.dupe(),
                DefT::new(DefTInner::SingletonStrT {
                    value: Name::new(num_lit.1.as_str()),
                    from_annot: false,
                }),
            ));
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_next,
                type_: l_new,
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (
            TypeInner::StrUtilT {
                reason: reason_s,
                op,
                remainder,
            },
            TypeInner::KeysT(reason_op, o),
        ) => {
            let l_new = Type::new(TypeInner::StrUtilT {
                reason: reason_s.dupe(),
                op: op.dupe(),
                remainder: remainder.dupe(),
            });
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_s.dupe(),
                type_: l_new,
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (
            TypeInner::GenericT(box GenericTData {
                reason: reason_s,
                bound,
                ..
            }),
            TypeInner::KeysT(reason_op, o),
        ) if let TypeInner::StrUtilT { op, remainder, .. } = bound.deref() => {
            let l_new = Type::new(TypeInner::StrUtilT {
                reason: reason_s.dupe(),
                op: op.dupe(),
                remainder: remainder.dupe(),
            });
            let u_use = UseT::new(UseTInner::HasOwnPropT(Box::new(HasOwnPropTData {
                use_op: use_op.dupe(),
                reason: reason_s.dupe(),
                type_: l_new,
            })));
            FlowJs::rec_flow(
                cx,
                trace,
                o,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason_op.dupe(),
                    use_desc: false,
                    use_t: Box::new(u_use),
                }),
            )
        }
        (TypeInner::KeysT(reason1, o1), _) => {
            // flow all keys of o1 to u
            FlowJs::rec_flow(
                cx,
                trace,
                o1,
                &UseT::new(UseTInner::GetKeysT(
                    reason1.dupe(),
                    Box::new(UseT::new(UseTInner::UseT(use_op, u.dupe()))),
                )),
            )
        }

        // ********************************************
        // Using predicate functions as regular ones
        // ********************************************
        (TypeInner::UnionT(reason, rep), _)
            if rep.members_iter().any(flow_js_utils::is_union_resolvable) =>
        {
            flow_js_utils::iter_resolve_union(
                |cx, trace, (t, u)| FlowJs::rec_flow(cx, *trace, &t, &u),
                cx,
                &trace,
                reason.dupe(),
                rep,
                UseT::new(UseTInner::UseT(use_op, u.dupe())),
            )
        }
        (TypeInner::UnionT(_, rep), TypeInner::UnionT(_, _)) => {
            union_to_union(cx, trace, use_op, l, rep, u)
        }
        (
            TypeInner::NominalT {
                nominal_type: lnom, ..
            },
            TypeInner::UnionT(_, _),
        ) if lnom.upper_t.as_ref().is_some_and(|upper| {
            matches!(upper.deref(), TypeInner::UnionT(_, _))
                && matches!(
                    flow_js_utils::union_optimization_guard(
                        cx,
                        |t1, t2| type_util::quick_subtype(None::<&fn(&Type)>, t1, t2),
                        upper,
                        u,
                    ),
                    flow_js_utils::UnionOptimizationGuardResult::True
                )
        }) =>
        {
            if cx.is_verbose() {
                eprintln!("UnionT ~> UnionT fast path (via an opaque type)");
            }
            Ok(())
        }
        // Optimization to treat maybe and optional types as special unions for subset comparision
        (TypeInner::UnionT(reason, rep), TypeInner::MaybeT(r, maybe)) => {
            let void = Type::new(TypeInner::DefT(r.dupe(), DefT::new(DefTInner::VoidT)));
            let null = Type::new(TypeInner::DefT(r.dupe(), DefT::new(DefTInner::NullT)));
            let maybe = type_util::push_type_alias_reason(r, maybe.dupe());
            // if the union doesn't contain void or null,
            // then everything in it must be upper-bounded by maybe
            match (
                union_rep::quick_mem_enum(
                    |t1, t2| type_util::quick_subtype(None::<&fn(&Type)>, t1, t2),
                    &void,
                    rep,
                ),
                union_rep::quick_mem_enum(
                    |t1, t2| type_util::quick_subtype(None::<&fn(&Type)>, t1, t2),
                    &null,
                    rep,
                ),
            ) {
                (union_rep::QuickMemResult::No, union_rep::QuickMemResult::No) => {
                    FlowJs::rec_flow_t(cx, trace, use_op, l, &maybe)
                }
                (union_rep::QuickMemResult::Yes, union_rep::QuickMemResult::No) => {
                    let filtered = flow_js_utils::remove_predicate_from_union(
                        reason.dupe(),
                        cx,
                        |t| type_util::quick_subtype(None::<&fn(&Type)>, t, &void),
                        rep,
                    );
                    FlowJs::rec_flow_t(cx, trace, use_op, &filtered, &maybe)
                }
                (union_rep::QuickMemResult::No, union_rep::QuickMemResult::Yes) => {
                    let filtered = flow_js_utils::remove_predicate_from_union(
                        reason.dupe(),
                        cx,
                        |t| type_util::quick_subtype(None::<&fn(&Type)>, t, &null),
                        rep,
                    );
                    FlowJs::rec_flow_t(cx, trace, use_op, &filtered, &maybe)
                }
                (union_rep::QuickMemResult::Yes, union_rep::QuickMemResult::Yes) => {
                    let filtered = flow_js_utils::remove_predicate_from_union(
                        reason.dupe(),
                        cx,
                        |t| {
                            type_util::quick_subtype(None::<&fn(&Type)>, t, &void)
                                || type_util::quick_subtype(None::<&fn(&Type)>, t, &null)
                        },
                        rep,
                    );
                    FlowJs::rec_flow_t(cx, trace, use_op, &filtered, &maybe)
                }
                _ => flow_all_in_union(
                    cx,
                    &trace,
                    rep,
                    &UseT::new(UseTInner::UseT(use_op, u.dupe())),
                ),
            }
        }
        (
            TypeInner::UnionT(reason, rep),
            TypeInner::OptionalT {
                reason: r,
                type_: opt,
                use_desc: opt_use_desc,
            },
        ) => {
            let void = void::why_with_use_desc(*opt_use_desc, r.dupe());
            // if the union doesn't contain void, then everything in it must be upper-bounded by u
            match union_rep::quick_mem_enum(
                |t1, t2| type_util::quick_subtype(None::<&fn(&Type)>, t1, t2),
                &void,
                rep,
            ) {
                union_rep::QuickMemResult::No => FlowJs::rec_flow_t(cx, trace, use_op, l, opt),
                union_rep::QuickMemResult::Yes => {
                    let filtered = flow_js_utils::remove_predicate_from_union(
                        reason.dupe(),
                        cx,
                        |t| type_util::quick_subtype(None::<&fn(&Type)>, t, &void),
                        rep,
                    );
                    FlowJs::rec_flow_t(cx, trace, use_op, &filtered, opt)
                }
                _ => flow_all_in_union(
                    cx,
                    &trace,
                    rep,
                    &UseT::new(UseTInner::UseT(use_op, u.dupe())),
                ),
            }
        }
        (TypeInner::UnionT(_, _), TypeInner::IntersectionT(_, rep)) => {
            if cx.is_verbose() {
                eprintln!("UnionT ~> IntersectionT slow case");
            }
            for t in rep.members_iter() {
                FlowJs::rec_flow(
                    cx,
                    trace,
                    l,
                    &UseT::new(UseTInner::UseT(use_op.dupe(), t.dupe())),
                )?;
            }
            Ok(())
        }
        (TypeInner::UnionT(_, rep), _) => {
            if cx.is_verbose() {
                match u.deref() {
                    TypeInner::UnionT(..) => eprintln!("UnionT ~> UnionT slow case"),
                    TypeInner::IntersectionT(..) => {
                        eprintln!("UnionT ~> IntersectionT slow case")
                    }
                    _ => {}
                }
            }
            if !rep.is_optimized_finally() {
                rep.optimize_enum_only(|t| type_mapper::union_flatten(cx, t.duped()));
            }
            match rep.check_enum_with_tag() {
                Some((enums, Some(tag))) => {
                    let mut members = rep.members_iter();
                    match members.next() {
                        None => {}
                        Some(representative) => {
                            let representative = representative.dupe();
                            drop(members);
                            match speculation_kit::try_singleton_throw_on_failure(
                                cx,
                                trace,
                                representative.dupe(),
                                UseT::new(UseTInner::UseT(use_op.dupe(), u.dupe())),
                            ) {
                                Err(FlowJsException::SpeculationSingletonError) => {
                                    // Flow_js_utils.union_representative_use_op cx ~l ~representative use_op
                                    let use_op = flow_js_utils::union_representative_use_op(
                                        cx,
                                        l,
                                        &representative,
                                        use_op,
                                    );
                                    FlowJs::rec_flow(
                                        cx,
                                        trace,
                                        &representative,
                                        &UseT::new(UseTInner::UseT(use_op, u.dupe())),
                                    )?;
                                }
                                Ok(()) => {
                                    let singleton_check =
                                        |elt: UnionEnum| -> Result<(), FlowJsException> {
                                            if Some(tag) == union_rep::tag_of_member(u) {
                                                if !enums.iter().all(|e| *e == elt) {
                                                    flow_js_utils::add_output(
                                                        cx,
                                                        ErrorMessage::EIncompatibleWithUseOp(
                                                            Box::new(EIncompatibleWithUseOpData {
                                                                reason_lower:
                                                                    type_util::reason_of_t(l).dupe(),
                                                                reason_upper:
                                                                    type_util::reason_of_t(u).dupe(),
                                                                use_op: use_op.dupe(),
                                                                explanation: None,
                                                            }),
                                                        ),
                                                    )?;
                                                }
                                            } else {
                                                flow_all_in_union(
                                                    cx,
                                                    &trace,
                                                    rep,
                                                    &UseT::new(UseTInner::UseT(
                                                        use_op.dupe(),
                                                        u.dupe(),
                                                    )),
                                                )?;
                                            }
                                            Ok(())
                                        };
                                    match u.deref() {
                                        TypeInner::DefT(_, ud)
                                            if let DefTInner::SingletonStrT {
                                                value: x, ..
                                            } = ud.deref() =>
                                        {
                                            singleton_check(UnionEnum::Str(x.dupe()))?;
                                        }
                                        TypeInner::DefT(_, ud)
                                            if let DefTInner::SingletonBoolT {
                                                value: x, ..
                                            } = ud.deref() =>
                                        {
                                            singleton_check(UnionEnum::Bool(*x))?;
                                        }
                                        TypeInner::DefT(_, ud)
                                            if let DefTInner::SingletonNumT {
                                                value: x, ..
                                            } = ud.deref() =>
                                        {
                                            singleton_check(UnionEnum::Num(x.dupe()))?;
                                        }
                                        _ => {
                                            flow_all_in_union(
                                                cx,
                                                &trace,
                                                rep,
                                                &UseT::new(UseTInner::UseT(use_op, u.dupe())),
                                            )?;
                                        }
                                    }
                                }
                                Err(e) => return Err(e),
                            }
                        }
                    }
                    Ok(())
                }
                _ => flow_all_in_union(
                    cx,
                    &trace,
                    rep,
                    &UseT::new(UseTInner::UseT(use_op, u.dupe())),
                ),
            }
        }
        (_, TypeInner::IntersectionT(_, rep)) => {
            if cx.is_verbose() {
                if let TypeInner::UnionT(..) = l.deref() {
                    eprintln!("IntersectionT ~> UnionT slow case");
                }
            }
            for t in rep.members_iter() {
                FlowJs::rec_flow(
                    cx,
                    trace,
                    l,
                    &UseT::new(UseTInner::UseT(use_op.dupe(), t.dupe())),
                )?;
            }
            Ok(())
        }
        // String enum sets can be handled in logarithmic time by just
        // checking for membership in the set.
        (TypeInner::DefT(reason_l, ld), TypeInner::UnionT(reason_u, rep))
            if let DefTInner::SingletonStrT { value: x, .. } = ld.deref()
                && {
                    if let Some(enums) = rep.check_enum() {
                        if !enums.contains(&UnionEnum::Str(x.dupe())) {
                            flow_js_utils::add_output(
                                cx,
                                ErrorMessage::EIncompatibleWithUseOp(Box::new(
                                    EIncompatibleWithUseOpData {
                                        reason_lower: reason_l.dupe(),
                                        reason_upper: reason_u.dupe(),
                                        use_op: use_op.dupe(),
                                        explanation: None,
                                    },
                                )),
                            )?;
                        }
                        true
                    } else {
                        false
                    }
                } =>
        {
            flow_js_utils::update_lit_type_from_annot(cx, l);
            Ok(())
        }
        (_, TypeInner::UnionT(_, rep))
            if {
                let ts: Vec<Type> = type_mapper::union_flatten(cx, rep.members_iter().duped());
                ts.iter().any(|t| {
                    type_util::quick_subtype(
                        Some(&|t: &Type| flow_js_utils::update_lit_type_from_annot(cx, t)),
                        l,
                        t,
                    )
                })
            } =>
        {
            Ok(())
        }
        (TypeInner::DefT(renders_r, ld), TypeInner::UnionT(r, rep))
            if matches!(ld.deref(), DefTInner::RendersT(_)) =>
        {
            // This is a tricky case because there are multiple ways that it could pass. Either
            //  the union contains a supertype of the LHS, or the Union itself is a super type of
            //  React.Node, in which case we can pass without splitting the union. Crucially, if the
            //  union is a super type of React.Node then splitting the union too early will cause
            //  spurious errors.
            //
            //  This is further complicated during implicit instantiation, where the union may contain
            //  implicitly instantiated tvars that should be constrained by the LHS.
            //
            //  To handle these cases, we first check to see if the union contains any implicitly instantiated
            //  tvars. If so, we start speculation. If not, we try to see if the RHS is a supertype of React.Node
            //  before kicking off regular speculation
            let union_contains_instantiable_tvars = if cx.in_implicit_instantiation() {
                rep.members_iter().any(|t| {
                    if let TypeInner::OpenT(tvar) = t.deref() {
                        use constraint::Constraints;
                        match cx.find_graph(tvar.id() as i32) {
                            Constraints::Resolved(_) | Constraints::FullyResolved(_) => false,
                            Constraints::Unresolved(_) => {
                                flow_common::reason::is_instantiable_reason(tvar.reason())
                            }
                        }
                    } else {
                        false
                    }
                })
            } else {
                false
            };
            let node = FlowJs::get_builtin_react_type(
                cx,
                None,
                renders_r,
                Some(true),
                intermediate_error_types::ExpectedModulePurpose::ReactModuleForReactNodeType,
            )?;
            if union_contains_instantiable_tvars
                || !FlowJs::speculative_subtyping_succeeds(cx, &node, u)?
            {
                speculation_kit::try_union(cx, None, trace, use_op, l.dupe(), r.dupe(), rep)?;
            }
            Ok(())
        }
        // The following case distributes the opaque constructor over a union/maybe/optional
        // type in the super type position of the opaque type.
        (
            TypeInner::NominalT {
                reason: lreason,
                nominal_type: lnom,
            },
            TypeInner::UnionT(r, rep),
        ) if lnom.upper_t.is_some() => {
            let upper_t = lnom.upper_t.as_ref().unwrap();
            let ts = FlowJs::possible_concrete_types_for_inspection(
                cx,
                type_util::reason_of_t(upper_t),
                upper_t,
            )?;
            match ts.as_slice() {
                [] | [_] => {
                    // Same as `_ ~> UnionT` case below
                    speculation_kit::try_union(cx, None, trace, use_op, l.dupe(), r.dupe(), rep)
                }
                [lt1, lt2, lts @ ..] => {
                    let make_opaque = |t: Type| -> Type {
                        Type::new(TypeInner::NominalT {
                            reason: lreason.dupe(),
                            nominal_type: Rc::new(NominalType::new(NominalTypeInner {
                                nominal_id: lnom.nominal_id.clone(),
                                underlying_t: lnom.underlying_t.clone(),
                                lower_t: lnom.lower_t.clone(),
                                upper_t: Some(t),
                                nominal_type_args: lnom.nominal_type_args.dupe(),
                            })),
                        })
                    };
                    let members: Rc<[Type]> =
                        lts.iter().map(|t: &Type| make_opaque(t.dupe())).collect();
                    let union_of_opaques = union_rep::make(
                        None,
                        union_rep::UnionKind::UnknownKind,
                        make_opaque(lt1.dupe()),
                        make_opaque(lt2.dupe()),
                        members,
                    );
                    let union_t = Type::new(TypeInner::UnionT(lreason.dupe(), union_of_opaques));
                    FlowJs::rec_flow_t(cx, trace, use_op, &union_t, u)
                }
            }
        }
        (_, TypeInner::UnionT(r, rep)) => {
            // Try the branches of the union in turn, with the goal of selecting the
            // correct branch. This process is reused for intersections as well. See
            // comments on try_union and try_intersection.
            let l_for_annot = l.dupe();
            let on_success: Box<dyn FnOnce(&Context<'cx>)> = Box::new(move |cx| {
                flow_js_utils::update_lit_type_from_annot(cx, &l_for_annot);
            });
            speculation_kit::try_union(cx, Some(on_success), trace, use_op, l.dupe(), r.dupe(), rep)
        }
        // maybe and optional types are just special union types
        (_, TypeInner::MaybeT(r2, t2)) => {
            let t2 = type_util::push_type_alias_reason(r2, t2.dupe());
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op, t2)))
        }
        (_, TypeInner::OptionalT { type_: t2, .. }) => {
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op, t2.dupe())))
        }
        // object types: an intersection may satisfy an object UB without
        // any particular member of the intersection doing so completely.
        // Here we trap object UBs with more than one property, and
        // decompose them into singletons.
        //
        // This trap is skipped for exact objects, since intersections of inexact objects
        // can never satisfy exact objects, but it might cause spurious errors.
        //
        // Note: should be able to do this with LookupT rather than
        // slices, but that approach behaves in nonobvious ways. TODO why?
        (TypeInner::IntersectionT(_, _), TypeInner::DefT(r, ud))
            if let DefTInner::ObjT(obj) = ud.deref()
                && cx.find_props(obj.props_tmap.dupe()).iter().count() > 1
                && obj.flags.obj_kind != ObjKind::Exact =>
        {
            if cx.is_verbose() {
                eprintln!("trapped");
            }
            for (x, p) in cx.find_props(obj.props_tmap.dupe()).iter() {
                let pmap =
                    properties::PropertiesMap::from_btree_map(std::collections::BTreeMap::from([
                        (x.dupe(), p.clone()),
                    ]));
                let id = cx.generate_property_map(pmap);
                let new_obj = ObjType {
                    flags: obj.flags.clone(),
                    props_tmap: id,
                    proto_t: dummy_prototype(),
                    call_t: obj.call_t,
                    reachable_targs: Rc::from([]),
                };
                FlowJs::rec_flow(
                    cx,
                    trace,
                    l,
                    &UseT::new(UseTInner::UseT(
                        use_op.dupe(),
                        Type::new(TypeInner::DefT(
                            r.dupe(),
                            DefT::new(DefTInner::ObjT(Rc::new(new_obj))),
                        )),
                    )),
                )?;
            }
            FlowJs::rec_flow(
                cx,
                trace,
                l,
                &UseT::new(UseTInner::UseT(use_op, obj.proto_t.dupe())),
            )
        }

        // (After the above preprocessing step, try the branches of the intersection
        // in turn, with the goal of selecting the correct branch. This process is
        // reused for unions as well. See comments on try_union and
        // try_intersection.)
        (TypeInner::IntersectionT(r, rep), _) => speculation_kit::try_intersection(
            cx,
            trace,
            UseT::new(UseTInner::UseT(use_op, u.dupe())),
            r.dupe(),
            rep,
        ),
        (TypeInner::NullProtoT(reason), _) => {
            let null = Type::new(TypeInner::DefT(reason.dupe(), DefT::new(DefTInner::NullT)));
            FlowJs::rec_flow_t(cx, trace, use_op, &null, u)
        }

        // ***********
        // StrUtilT
        // ***********

        // prefix
        (
            TypeInner::StrUtilT {
                op: StrUtilOp::StrPrefix(prefix1),
                ..
            },
            TypeInner::StrUtilT {
                op: StrUtilOp::StrPrefix(prefix2),
                remainder: None,
                ..
            },
        ) if prefix1.starts_with(prefix2.as_str()) => Ok(()),
        (
            TypeInner::StrUtilT {
                reason,
                op: StrUtilOp::StrPrefix(prefix1),
                remainder: remainder1,
            },
            TypeInner::StrUtilT {
                op: StrUtilOp::StrPrefix(prefix2),
                remainder: Some(remainder2),
                ..
            },
        ) if prefix1 == prefix2 => {
            let remainder1 = remainder1
                .as_ref()
                .duped()
                .unwrap_or_else(|| str_module_t::why(reason.dupe()));
            FlowJs::rec_flow_t(cx, trace, use_op, &remainder1, remainder2)
        }
        (
            TypeInner::DefT(reason, ld),
            TypeInner::StrUtilT {
                op: StrUtilOp::StrPrefix(prefix),
                remainder,
                ..
            },
        ) if let DefTInner::SingletonStrT { value, .. } = ld.deref()
            && value.as_str().starts_with(prefix.as_str()) =>
        {
            let s = value.as_str();
            flow_js_utils::update_lit_type_from_annot(cx, l);
            if let Some(remainder) = remainder {
                let chopped = &s[prefix.len()..];
                let reason = reason
                    .dupe()
                    .replace_desc(VirtualReasonDesc::RStringWithoutPrefix {
                        prefix: prefix.dupe(),
                    });
                let str_t = Type::new(TypeInner::DefT(
                    reason,
                    DefT::new(DefTInner::SingletonStrT {
                        value: Name::new(chopped),
                        from_annot: true,
                    }),
                ));
                FlowJs::rec_flow_t(cx, trace, use_op, &str_t, remainder)?;
            }
            Ok(())
        }
        // suffix
        (
            TypeInner::StrUtilT {
                op: StrUtilOp::StrSuffix(suffix1),
                ..
            },
            TypeInner::StrUtilT {
                op: StrUtilOp::StrSuffix(suffix2),
                remainder: None,
                ..
            },
        ) if suffix1.ends_with(suffix2.as_str()) => Ok(()),
        (
            TypeInner::StrUtilT {
                reason,
                op: StrUtilOp::StrSuffix(suffix1),
                remainder: remainder1,
            },
            TypeInner::StrUtilT {
                op: StrUtilOp::StrSuffix(suffix2),
                remainder: Some(remainder2),
                ..
            },
        ) if suffix1 == suffix2 => {
            let remainder1 = remainder1
                .as_ref()
                .duped()
                .unwrap_or_else(|| str_module_t::why(reason.dupe()));
            FlowJs::rec_flow_t(cx, trace, use_op, &remainder1, remainder2)
        }
        (
            TypeInner::DefT(reason, ld),
            TypeInner::StrUtilT {
                op: StrUtilOp::StrSuffix(suffix),
                remainder,
                ..
            },
        ) if let DefTInner::SingletonStrT { value, .. } = ld.deref()
            && value.as_str().ends_with(suffix.as_str()) =>
        {
            let s = value.as_str();
            flow_js_utils::update_lit_type_from_annot(cx, l);
            if let Some(remainder) = remainder {
                let chopped = &s[..s.len() - suffix.len()];
                let reason = reason
                    .dupe()
                    .replace_desc(VirtualReasonDesc::RStringWithoutSuffix {
                        suffix: suffix.dupe(),
                    });
                let str_t = Type::new(TypeInner::DefT(
                    reason,
                    DefT::new(DefTInner::SingletonStrT {
                        value: Name::new(chopped),
                        from_annot: true,
                    }),
                ));
                FlowJs::rec_flow_t(cx, trace, use_op, &str_t, remainder)?;
            }
            Ok(())
        }
        // both
        (TypeInner::StrUtilT { reason, op, .. }, _) => {
            let arg = match op {
                StrUtilOp::StrPrefix(s) | StrUtilOp::StrSuffix(s) => s,
            };
            let literal_kind = if arg.is_empty() {
                Literal::AnyLiteral
            } else {
                Literal::Truthy
            };
            let str_t = Type::new(TypeInner::DefT(
                reason.dupe(),
                DefT::new(DefTInner::StrGeneralT(literal_kind)),
            ));
            FlowJs::rec_flow_t(cx, trace, use_op, &str_t, u)
        }

        // When do we consider a polymorphic type <X:U> T to be a subtype of another
        // polymorphic type <X:U'> T'? This is the subject of a long line of
        // research. A rule that works (Cardelli/Wegner) is: force U = U', and prove
        // that T is a subtype of T' for any X:U'. A more general rule that proves
        // that U' is a subtype of U instead of forcing U = U' is known to cause
        // undecidable subtyping (Pierce): the counterexamples are fairly
        // pathological, but can be reliably constructed by exploiting the "switch"
        // of bounds from U' to U (and back, with sufficient trickery), in ways that
        // are difficult to detect statically.
        //
        // However, these results are somewhat tricky to interpret in Flow, since we
        // are not proving stuff inductively: instead we are co-inductively assuming
        // what we want to prove, and checking consistency.
        //
        // Separately, none of these rules capture the logical interpretation of the
        // original subtyping question (interpreting subtyping as implication, and
        // polymorphism as universal quantification). What we really want to show is
        // that, for all X:U', there is some X:U such that T is a subtype of T'. But
        // we already deal with statements of this form when checking polymorphic
        // definitions! In particular, statements such as "there is some X:U...")
        // correspond to "create a type variable with that constraint and ...", and
        // statements such as "show that for all X:U" correspond to "introduce a
        // GenericT with bound U".
        //
        // Thus, all we need to do when checking that any type flows to a
        // polymorphic type is to follow the same principles used when checking that
        // a polymorphic definition has a polymorphic type. This has the pleasant
        // side effect that the type we're checking does not itself need to be a
        // polymorphic type at all! For example, we can let a non-generic method be
        // overridden with a generic method, as long as the non-generic signature
        // can be derived as a specialization of the generic signature.

        // some shortcuts
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!(
                (ld.deref(), ud.deref()),
                (DefTInner::PolyT(_), DefTInner::PolyT(_))
            ) && {
                if let (
                    DefTInner::PolyT(box PolyTData { id: id1, .. }),
                    DefTInner::PolyT(box PolyTData { id: id2, .. }),
                ) = (ld.deref(), ud.deref())
                {
                    id1 == id2
                } else {
                    false
                }
            } =>
        {
            if cx.is_verbose() {
                eprintln!("PolyT ~> PolyT fast path");
            }
            Ok(())
        }
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!(
                (ld.deref(), ud.deref()),
                (DefTInner::PolyT(_), DefTInner::PolyT(_))
            ) && {
                if let (
                    DefTInner::PolyT(box PolyTData {
                        tparams: params1, ..
                    }),
                    DefTInner::PolyT(box PolyTData {
                        tparams: params2, ..
                    }),
                ) = (ld.deref(), ud.deref())
                {
                    params1.len() == params2.len()
                } else {
                    false
                }
            } =>
        {
            if let (
                DefTInner::PolyT(box PolyTData {
                    tparams: params1,
                    t_out: t1,
                    ..
                }),
                DefTInner::PolyT(box PolyTData {
                    tparams: params2,
                    t_out: t2,
                    ..
                }),
            ) = (ld.deref(), ud.deref())
            {
                // A description of this subtyping rule can be found in "Decidable Bounded
                // Quantifcation" by G. Castagna B. Pierce.
                //
                //   G |- T1 <: S1   G, { X: S1 } |- S2 <: T2
                //   ------------------------------------------ (All-local)
                //   G |- forall X:S1 . S2 <: forall X:T1 . T2
                //
                // The code below implements this rule for the slightly more general case of
                // subtyping between two polymorphic types:
                //
                // forall a1:b1  , ... ai:bi  , ..., an:bn   . t
                // forall a1':b1', ... ai':bi', ..., an':bn' . t'
                //
                // 1st Premise
                // -----------
                // For each type parameter pair (ai:bi, ai':bi') we create the constraints:
                //
                // bi[ai, ..., a_(i-1)] <: bi'[ai'/ai, ..., a_(i-1)'/a_(i-1)]
                //
                // where ai is a GenericT
                let mut map1 = flow_data_structure_wrapper::ord_map::FlowOrdMap::new();
                let mut map2 = flow_data_structure_wrapper::ord_map::FlowOrdMap::new();
                for (param1, param2) in params1.iter().zip(params2.iter()) {
                    let bound2 = flow_typing_flow_common::type_subst::subst(
                        cx,
                        Some(use_op.dupe()),
                        true,
                        false,
                        flow_typing_flow_common::type_subst::Purpose::Normal,
                        &map2,
                        param2.bound.dupe(),
                    );
                    FlowJs::rec_flow(
                        cx,
                        trace,
                        &bound2,
                        &UseT::new(UseTInner::UseT(use_op.dupe(), param1.bound.dupe())),
                    )?;
                    if param1.is_const != param2.is_const {
                        flow_js_utils::add_output(
                            cx,
                            ErrorMessage::ETypeParamConstIncompatibility(Box::new(
                                ETypeParamConstIncompatibilityData {
                                    use_op: use_op.dupe(),
                                    lower: param1.reason.dupe(),
                                    upper: param2.reason.dupe(),
                                },
                            )),
                        )?;
                    }
                    let (gen_t, new_map1) = flow_js_utils::generic_bound(cx, map1, param1);
                    map1 = new_map1;
                    map2.insert(param2.name.dupe(), gen_t);
                }
                // 2nd Premise
                // -----------
                // We check t <: t' after substituting ai' for ai
                let t1_subst = flow_typing_flow_common::type_subst::subst(
                    cx,
                    Some(use_op.dupe()),
                    true,
                    false,
                    flow_typing_flow_common::type_subst::Purpose::Normal,
                    &map1,
                    t1.dupe(),
                );
                let t2_subst = flow_typing_flow_common::type_subst::subst(
                    cx,
                    Some(use_op.dupe()),
                    true,
                    false,
                    flow_typing_flow_common::type_subst::Purpose::Normal,
                    &map2,
                    t2.dupe(),
                );
                FlowJs::rec_flow_t(cx, trace, use_op, &t1_subst, &t2_subst)?;
            }
            Ok(())
        }
        // general case
        (_, TypeInner::DefT(_, ud))
            if let DefTInner::PolyT(box PolyTData { t_out: t, .. }) = ud.deref() =>
        {
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op, t.dupe())))
        }
        // TODO: ideally we'd do the same when lower bounds flow to a
        // this-abstracted class, but fixing the class is easier; might need to
        // revisit
        (_, TypeInner::DefT(class_r, ud))
            if let DefTInner::ClassT(this_inst_t) = ud.deref()
                && let TypeInner::ThisInstanceT(box ThisInstanceTData {
                    reason: inst_r,
                    instance: i,
                    is_this: this,
                    subst_name: this_name,
                }) = this_inst_t.deref() =>
        {
            let reason = type_util::reason_of_t(l);
            let fixed = flow_js_utils::fix_this_instance(
                cx,
                reason.dupe(),
                inst_r.dupe(),
                i,
                *this,
                this_name.dupe(),
            );
            let class_t = Type::new(TypeInner::DefT(
                class_r.dupe(),
                DefT::new(DefTInner::ClassT(fixed)),
            ));
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op, class_t)))
        }
        (
            _,
            TypeInner::ThisInstanceT(box ThisInstanceTData {
                reason: r,
                instance: i,
                is_this: this,
                subst_name: this_name,
            }),
        ) => {
            let reason = type_util::reason_of_t(l);
            let fixed = flow_js_utils::fix_this_instance(
                cx,
                reason.dupe(),
                r.dupe(),
                i,
                *this,
                this_name.dupe(),
            );
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op, fixed)))
        }
        // This rule is hit when a polymorphic type appears outside a
        // type application expression - i.e. not followed by a type argument list
        // delimited by angle brackets.
        // We want to require full expressions in type positions like annotations,
        // but allow use of polymorphically-typed values - for example, in class
        // extends clauses and at function call sites - without explicit type
        // arguments, since typically they're easily inferred from context.
        (TypeInner::DefT(reason_tapp, ld), _)
            if let DefTInner::PolyT(box PolyTData {
                tparams_loc,
                tparams: ids,
                t_out: t,
                ..
            }) = ld.deref() =>
        {
            let reason_op = type_util::reason_of_t(u);
            let ids = Vec1::try_from_vec(ids.to_vec()).unwrap();
            let (t_, _) = FlowJs::instantiate_poly(
                cx,
                trace,
                use_op.dupe(),
                reason_op,
                reason_tapp,
                None,
                (tparams_loc.dupe(), ids, t.dupe()),
            )?;
            FlowJs::rec_flow_t(cx, trace, use_op, &t_, u)
        }
        // when a this-abstracted class flows to upper bounds, fix the class
        (TypeInner::DefT(class_r, ld), _)
            if let DefTInner::ClassT(this_inst_t) = ld.deref()
                && let TypeInner::ThisInstanceT(box ThisInstanceTData {
                    reason: inst_r,
                    instance: i,
                    is_this: this,
                    subst_name: this_name,
                }) = this_inst_t.deref() =>
        {
            let reason = type_util::reason_of_t(u);
            let fixed = flow_js_utils::fix_this_instance(
                cx,
                reason.dupe(),
                inst_r.dupe(),
                i,
                *this,
                this_name.dupe(),
            );
            let class_t = Type::new(TypeInner::DefT(
                class_r.dupe(),
                DefT::new(DefTInner::ClassT(fixed)),
            ));
            FlowJs::rec_flow_t(cx, trace, use_op, &class_t, u)
        }
        (
            TypeInner::ThisInstanceT(box ThisInstanceTData {
                reason: r,
                instance: i,
                is_this: this,
                subst_name: this_name,
            }),
            _,
        ) => {
            let reason = type_util::reason_of_t(u);
            let fixed = flow_js_utils::fix_this_instance(
                cx,
                reason.dupe(),
                r.dupe(),
                i,
                *this,
                this_name.dupe(),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, &fixed, u)
        }

        // ****************************
        // React Abstract Components
        // ****************************
        //
        // In all of these cases, we check:
        //  1. configu <: configl
        //  2. default_propsl = default_propsu
        //  3. instancel <: instanceu
        //  4. rendersl <: rendersu
        //
        //  2. is necessary because we allow the default props of a component to be read and
        //  written.
        //
        //  1. Is necessary because we need to ensure that any config object that is passed to u
        //  is compatible with the config of l. This also is sufficient; unification is not required.
        //  We can think of AbstractComponents as some sort of callable that accepts a config object.
        //  The only place that the config object type would appear is in the callable signature, which
        //  is contravariant.
        //
        //  In reality, a component is turned into an element via createElement, which accepts a
        //  component and a config object. From there, it creates an object that will become the
        //  props of a component by combining the config object with the component's default props.
        //  This process creates a new fresh unaliased props object, which is passed to the component.
        //
        //  3. Is necessary because we need to ensure the ref passed in is compatible with the instance
        //  type of the component. React will assign ref.current to the instance of the component, so we
        //  need to ensure that the type we assign is compatible with the type ref.current.
        //
        //  4. Is necessary because we need to ensure the element returned from the render function is
        //  compatible with any places that the component can be rendered. This is often used by
        //  components that only accept specific components as children.
        //

        // Class component ~> AbstractComponent
        (TypeInner::DefT(reasonl, ld), TypeInner::DefT(_, ud))
            if let DefTInner::ClassT(this) = ld.deref()
                && let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                    config,
                    renders,
                    component_kind: ComponentKind::Structural,
                }) = ud.deref() =>
        {
            // Contravariant config check
            FlowJs::react_get_config(
                cx,
                trace,
                l,
                use_op.dupe(),
                reasonl,
                react::Tool::<Context<'cx>>::GetConfig { tout: l.dupe() },
                Polarity::Negative,
                config,
            )?;
            // check rendersl <: rendersu
            FlowJs::react_subtype_class_component_render(
                cx, trace, use_op, this, reasonl, renders,
            )?;
            Ok(())
        }
        // Function Component ~> AbstractComponent
        (TypeInner::DefT(reasonl, ld), TypeInner::DefT(_reasonu, ud))
            if let DefTInner::FunT(_, ft) = ld.deref()
                && let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                    config,
                    renders,
                    component_kind: ComponentKind::Structural,
                }) = ud.deref() =>
        {
            // Function components will not always have an annotation, so the config may
            // never resolve. To determine config compatibility, we instead
            // call createElement on the function with the given component to determine
            // the compatibility.
            //
            // We use ConfigCheck instead of CreateElement because:
            //  1. We can't perform the key check. If config is mixed, which can happen in
            //  polymorphic HOCs then the [string]: mixed indexer causes spurious errors.
            //  2. We check the ref here, so we don't need to check it in the config as well.
            FlowJs::rec_flow(
                cx,
                trace,
                l,
                &UseT::new(UseTInner::ReactKitT(Box::new(ReactKitTData {
                    use_op: use_op.dupe(),
                    reason: reasonl.dupe(),
                    tool: Box::new(react::Tool::<Context<'cx>>::ConfigCheck {
                        props: config.dupe(),
                    }),
                }))),
            )?;
            // check rendered elements are covariant
            FlowJs::rec_flow_t(cx, trace, use_op, &ft.return_t, renders)
        }

        // Object Component ~> AbstractComponent
        (TypeInner::DefT(reasonl, ld), TypeInner::DefT(reasonu, ud))
            if let DefTInner::ObjT(obj) = ld.deref()
                && let Some(call_t) = &obj.call_t
                && let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                    config,
                    renders,
                    component_kind: ComponentKind::Structural,
                }) = ud.deref() =>
        {
            FlowJs::rec_flow(
                cx,
                trace,
                l,
                &UseT::new(UseTInner::ReactKitT(Box::new(ReactKitTData {
                    use_op: use_op.dupe(),
                    reason: reasonl.dupe(),
                    tool: Box::new(react::Tool::<Context<'cx>>::ConfigCheck {
                        props: config.dupe(),
                    }),
                }))),
            )?;
            // Ensure the callable signature's return type is compatible with the rendered element (renders). We
            // do this by flowing it to (...empty): renders
            let funtype = mk_functiontype(
                reasonu.dupe(),
                None,
                None,
                vec![],
                Some(FunRestParam(
                    None,
                    reasonu.loc().dupe(),
                    empty_t::why(reasonu.dupe().replace_desc(VirtualReasonDesc::REmpty)),
                )),
                reasonl.dupe(),
                None,
                None,
                renders.dupe(),
            );
            let mixed = mixed_t::why(reasonu.dupe());
            let call_t = cx.find_call(*call_t);
            let fun_t = Type::new(TypeInner::DefT(
                reasonu.dupe(),
                DefT::new(DefTInner::FunT(mixed, Rc::new(funtype))),
            ));
            FlowJs::rec_flow_t(cx, trace, use_op, &call_t, &fun_t)
        }
        // AbstractComponent ~> AbstractComponent
        (TypeInner::DefT(reasonl, ld), TypeInner::DefT(_reasonu, ud))
            if let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                config: configl,
                renders: rendersl,
                component_kind,
            }) = ld.deref()
                && let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                    config: configu,
                    renders: rendersu,
                    component_kind: ComponentKind::Structural,
                }) = ud.deref() =>
        {
            FlowJs::rec_flow_t(cx, trace, use_op.dupe(), configu, configl)?;
            let rendersl = match component_kind {
                ComponentKind::Nominal(renders_id, renders_name, _) => {
                    let reason =
                        reasonl
                            .dupe()
                            .replace_desc(VirtualReasonDesc::RRenderType(Arc::new(
                                VirtualReasonDesc::RReactElement {
                                    name_opt: Some(Name::new(renders_name.dupe())),
                                    from_component_syntax: true,
                                },
                            )));
                    Type::new(TypeInner::DefT(
                        reason,
                        DefT::new(DefTInner::RendersT(Rc::new(
                            CanonicalRendersForm::NominalRenders {
                                renders_id: renders_id.dupe(),
                                renders_name: renders_name.dupe(),
                                renders_super: rendersl.dupe(),
                            },
                        ))),
                    ))
                }
                ComponentKind::Structural => rendersl.dupe(),
            };
            let use_op = UseOp::Frame(
                Arc::new(VirtualFrameUseOp::RendersCompatibility),
                Arc::new(use_op),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, &rendersl, rendersu)
        }
        (TypeInner::DefT(_reasonl, ld), TypeInner::DefT(_reasonu, ud))
            if let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                component_kind: ComponentKind::Nominal(idl, name_l, _),
                config: configl,
                renders: rendersl,
                ..
            }) = ld.deref()
                && let DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                    component_kind: ComponentKind::Nominal(idu, name_u, _),
                    config: configu,
                    renders: rendersu,
                    ..
                }) = ud.deref()
                && (idl == idu
                    || type_util::nominal_id_have_same_logical_module(
                        &cx.file_options(),
                        cx.projects_options(),
                        (idl, Some(name_l.as_str())),
                        (idu, Some(name_u.as_str())),
                    )) =>
        {
            FlowJs::rec_flow_t(cx, trace, use_op.dupe(), configu, configl)?;
            let use_op = UseOp::Frame(
                Arc::new(VirtualFrameUseOp::RendersCompatibility),
                Arc::new(use_op),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, rendersl, rendersu)
        }
        (TypeInner::DefT(reasonl, ld), TypeInner::DefT(reasonu, ud))
            if let DefTInner::RendersT(r1) = ld.deref()
                && let DefTInner::RendersT(r2) = ud.deref() =>
        {
            renders_kit::rec_renders_to_renders(cx, trace, use_op, (reasonl, r1), (reasonu, r2))
        }
        (_, TypeInner::DefT(renders_r, ud))
            if let DefTInner::RendersT(upper_renders) = ud.deref() =>
        {
            renders_kit::non_renders_to_renders(cx, trace, use_op, l, renders_r, upper_renders)
        }
        // Exiting the renders world
        (TypeInner::DefT(r, ld), _)
            if matches!(
                ld.deref(),
                DefTInner::RendersT(renders)
                    if matches!(renders.deref(), CanonicalRendersForm::IntrinsicRenders(_) | CanonicalRendersForm::NominalRenders { .. })
            ) =>
        {
            let mixed_element = FlowJs::get_builtin_react_type(
                cx,
                None,
                r,
                None,
                intermediate_error_types::ExpectedModulePurpose::ReactModuleForReactMixedElementType,
            )?;
            FlowJs::rec_flow_t(cx, trace, use_op, &mixed_element, u)
        }
        (TypeInner::DefT(r, ld), _)
            if let DefTInner::RendersT(renders) = ld.deref()
                && let CanonicalRendersForm::StructuralRenders {
                    renders_structural_type: t,
                    ..
                } = renders.deref() =>
        {
            let u_prime = UseT::new(UseTInner::ExitRendersT {
                renders_reason: r.dupe(),
                u: Box::new(UseT::new(UseTInner::UseT(use_op, u.dupe()))),
            });
            FlowJs::rec_flow(cx, trace, t, &u_prime)
        }
        (TypeInner::DefT(r, ld), _)
            if matches!(
                ld.deref(),
                DefTInner::RendersT(renders) if matches!(renders.deref(), CanonicalRendersForm::DefaultRenders)
            ) =>
        {
            let u_prime = UseT::new(UseTInner::ExitRendersT {
                renders_reason: r.dupe(),
                u: Box::new(UseT::new(UseTInner::UseT(use_op, u.dupe()))),
            });
            FlowJs::rec_flow(cx, trace, l, &u_prime)
        }

        // ***********************************************
        // * function types deconstruct into their parts *
        // ***********************************************

        // FunT ~> FunT
        (TypeInner::DefT(lreason, ld), TypeInner::DefT(ureason, ud))
            if let (DefTInner::FunT(_, ft1), DefTInner::FunT(_, ft2)) =
                (ld.deref(), ud.deref()) =>
        {
            let inner_use_op = if let VirtualUseOp::Frame(ref frame, ref inner) = use_op {
                if let VirtualFrameUseOp::PropertyCompatibility(box PropertyCompatibilityData {
                    prop: Some(name),
                    ..
                }) = frame.deref()
                {
                    // The $call PropertyCompatibility is redundant when we have a
                    // FunCompatibility use_op.
                    if name.as_str() == "$call" {
                        inner.deref().dupe()
                    } else {
                        use_op
                    }
                } else {
                    use_op
                }
            } else {
                use_op
            };
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::FunCompatibility {
                    lower: lreason.dupe(),
                    upper: ureason.dupe(),
                }),
                Arc::new(inner_use_op),
            );

            {
                let use_op = VirtualUseOp::Frame(
                    Arc::new(VirtualFrameUseOp::FunParam(Box::new(FunParamData {
                        n: 0,
                        name: Some(FlowSmolStr::new_inline("this")),
                        lower: lreason.dupe(),
                        upper: ureason.dupe(),
                    }))),
                    Arc::new(use_op.dupe()),
                );
                let (this_param1, this_status_1) = &ft1.this_t;
                let (this_param2, _this_status_2) = &ft2.this_t;
                match (this_status_1, &ft2.this_t.1) {
                    (ThisStatus::ThisMethod { .. }, ThisStatus::ThisMethod { .. }) => {
                        let sub_this2 = type_util::subtype_this_of_function(ft2);
                        let sub_this1 = type_util::subtype_this_of_function(ft1);
                        FlowJs::rec_flow(
                            cx,
                            trace,
                            &sub_this2,
                            &UseT::new(UseTInner::UseT(use_op, sub_this1)),
                        )?;
                    }
                    // lower bound method, upper bound function
                    // This is always banned, as it would allow methods to be unbound through casting
                    (ThisStatus::ThisMethod { unbound }, ThisStatus::ThisFunction) => {
                        if !unbound && !flow_common::files::has_ts_ext(cx.file()) {
                            flow_js_utils::add_output(
                                cx,
                                ErrorMessage::EMethodUnbinding(Box::new(EMethodUnbindingData {
                                    use_op: use_op.dupe(),
                                    reason_op: lreason.dupe(),
                                    reason_prop: type_util::reason_of_t(this_param1).clone(),
                                })),
                            )?;
                        }
                        let sub_this1 = type_util::subtype_this_of_function(ft1);
                        FlowJs::rec_flow(
                            cx,
                            trace,
                            this_param2,
                            &UseT::new(UseTInner::UseT(use_op, sub_this1)),
                        )?;
                    }
                    // lower bound function, upper bound method/function.
                    // Ok as long as the types match up
                    (ThisStatus::ThisFunction, ThisStatus::ThisMethod { .. })
                    | (ThisStatus::ThisFunction, ThisStatus::ThisFunction) => {
                        FlowJs::rec_flow(
                            cx,
                            trace,
                            this_param2,
                            &UseT::new(UseTInner::UseT(use_op, this_param1.dupe())),
                        )?;
                    }
                }
            }
            let mut args: Vec<CallArg> = ft2
                .params
                .iter()
                .map(|p| CallArg::arg(p.1.dupe()))
                .collect();
            if let Some(FunRestParam(_, _, rest)) = &ft2.rest_param {
                args.push(CallArg::spread_arg(rest.dupe()));
            }
            FlowJs::multiflow_subtype(cx, trace, use_op.dupe(), ureason, &args, ft1)?;

            match (&ft1.effect_, &ft2.effect_) {
                (ReactEffectType::AnyEffect, _)
                | (_, ReactEffectType::AnyEffect)
                | (ReactEffectType::ArbitraryEffect, ReactEffectType::ArbitraryEffect)
                | (
                    ReactEffectType::HookDecl(_) | ReactEffectType::HookAnnot,
                    ReactEffectType::HookAnnot,
                ) => {}
                (ReactEffectType::HookDecl(a), ReactEffectType::HookDecl(b)) if a == b => {}
                (
                    ReactEffectType::HookDecl(_) | ReactEffectType::HookAnnot,
                    ReactEffectType::ArbitraryEffect,
                ) => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EHookIncompatible(Box::new(EHookIncompatibleData {
                            use_op: use_op.dupe(),
                            lower: lreason.dupe(),
                            upper: ureason.dupe(),
                            lower_is_hook: true,
                            hook_is_annot: ft1.effect_ == ReactEffectType::HookAnnot,
                        })),
                    )?;
                }
                (
                    ReactEffectType::ArbitraryEffect,
                    ReactEffectType::HookDecl(_) | ReactEffectType::HookAnnot,
                ) => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EHookIncompatible(Box::new(EHookIncompatibleData {
                            use_op: use_op.dupe(),
                            lower: lreason.dupe(),
                            upper: ureason.dupe(),
                            lower_is_hook: false,
                            hook_is_annot: ft2.effect_ == ReactEffectType::HookAnnot,
                        })),
                    )?;
                }
                (
                    ReactEffectType::HookDecl(_) | ReactEffectType::HookAnnot,
                    ReactEffectType::HookDecl(_),
                ) => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EHookUniqueIncompatible(Box::new(
                            EHookUniqueIncompatibleData {
                                use_op: use_op.dupe(),
                                lower: lreason.dupe(),
                                upper: ureason.dupe(),
                            },
                        )),
                    )?;
                }
            }

            // Return type subtyping
            let ret_use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::FunReturn {
                    lower: type_util::reason_of_t(&ft1.return_t).clone(),
                    upper: type_util::reason_of_t(&ft2.return_t).clone(),
                }),
                Arc::new(use_op.dupe()),
            );
            FlowJs::rec_flow(
                cx,
                trace,
                &ft1.return_t,
                &UseT::new(UseTInner::UseT(ret_use_op, ft2.return_t.dupe())),
            )?;

            match (&ft1.type_guard, &ft2.type_guard) {
                (None, Some(_)) => {
                    // Non-predicate functions are incompatible with predicate ones
                    // TODO: somehow the original flow needs to be propagated as well
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::ETypeGuardFuncIncompatibility {
                            use_op: use_op.dupe(),
                            reasons: (lreason.dupe(), ureason.dupe()),
                        },
                    )?;
                }
                (Some(tg1), Some(tg2)) => {
                    let params1: Vec<(Option<Name>, Type)> = ft1
                        .params
                        .iter()
                        .map(|p| (p.0.as_ref().map(|s| Name::new(s.dupe())), p.1.dupe()))
                        .collect();
                    let params2: Vec<(Option<Name>, Type)> = ft2
                        .params
                        .iter()
                        .map(|p| (p.0.as_ref().map(|s| Name::new(s.dupe())), p.1.dupe()))
                        .collect();
                    let name1 = Name::new(tg1.param_name.1.dupe());
                    let name2 = Name::new(tg2.param_name.1.dupe());
                    func_type_guard_compat(
                        cx,
                        trace,
                        use_op.dupe(),
                        (
                            &tg1.reason,
                            &params1,
                            tg1.one_sided,
                            (&tg1.param_name.0, &name1),
                            &tg1.type_guard,
                        ),
                        (
                            &tg2.reason,
                            &params2,
                            tg2.one_sided,
                            (&tg2.param_name.0, &name2),
                            &tg2.type_guard,
                        ),
                    )?;
                }
                (Some(_), None) | (None, None) => {}
            }
            Ok(())
        }

        // unwrap namespace type into object type, drop all information about types in the namespace
        (TypeInner::NamespaceT(ns), _) => FlowJs::rec_flow_t(cx, trace, use_op, &ns.values_type, u),
        (_, TypeInner::NamespaceT(ns)) => FlowJs::rec_flow_t(cx, trace, use_op, l, &ns.values_type),

        // ObjT -> ObjT
        (TypeInner::DefT(lreason, ld), TypeInner::DefT(ureason, ud))
            if let DefTInner::ObjT(l_obj) = ld.deref()
                && let DefTInner::ObjT(u_obj) = ud.deref() =>
        {
            let u_deft = u;
            type_inference_hooks_js::dispatch_obj_to_obj_hook(cx, l, u_deft);
            let print_fast_path = cx.is_verbose();
            if l_obj.props_tmap == u_obj.props_tmap && l_obj.call_t == u_obj.call_t {
                if print_fast_path {
                    eprintln!("ObjT ~> ObjT fast path: yes");
                }
            } else {
                if print_fast_path {
                    eprintln!("ObjT ~> ObjT fast path: no");
                }
                flow_obj_to_obj(cx, trace, use_op, lreason, l_obj, ureason, u_obj)?;
            }
            Ok(())
        }
        (TypeInner::DefT(_, ld), TypeInner::NullProtoT(_))
            if matches!(ld.deref(), DefTInner::ObjT(_)) =>
        {
            Ok(())
        }
        // InstanceT -> ObjT
        (TypeInner::DefT(lreason, ld), TypeInner::DefT(ureason, ud))
            if matches!(ld.deref(), DefTInner::InstanceT(inst) if matches!(inst.inst.inst_kind, InstanceKind::ClassKind | InstanceKind::InterfaceKind { .. }))
                && matches!(ud.deref(), DefTInner::ObjT(obj) if obj.flags.obj_kind == ObjKind::Exact) =>
        {
            let reasons = ordered_reasons((lreason.dupe(), ureason.dupe()));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleWithExact(
                    reasons,
                    use_op,
                    intermediate_error_types::ExactnessErrorKind::UnexpectedInexact,
                ),
            )?;
            Ok(())
        }
        (TypeInner::DefT(lreason, ld), TypeInner::DefT(ureason, ud))
            if let DefTInner::InstanceT(inst_t) = ld.deref()
                && let DefTInner::ObjT(u_obj) = ud.deref() =>
        {
            let super_ = &inst_t.super_;
            let inst = &inst_t.inst;
            let lown = inst.own_props.dupe();
            let lproto = inst.proto_props.dupe();
            let lcall = inst.inst_call_t;
            let inst_kind = &inst.inst_kind;
            let uflds = u_obj.props_tmap.dupe();
            let uproto = &u_obj.proto_t;
            let ucall = u_obj.call_t;

            let error_kind = match inst_kind {
                InstanceKind::ClassKind | InstanceKind::InterfaceKind { .. } => {
                    intermediate_error_types::ClassKind::Class
                }
                InstanceKind::RecordKind { .. } => intermediate_error_types::ClassKind::Record,
            };
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EClassToObject(Box::new(EClassToObjectData {
                    reason_class: lreason.dupe(),
                    reason_obj: ureason.dupe(),
                    use_op: use_op.dupe(),
                    kind: error_kind,
                })),
            )?;
            let own_props = cx.find_props(lown.dupe());
            let proto_props = cx.find_props(lproto.dupe());
            let mut lflds_map = own_props.clone();
            for (k, v) in proto_props.iter() {
                if !lflds_map.contains_key(k) {
                    lflds_map.insert(k.dupe(), v.clone());
                }
            }
            if let Some(ucall) = ucall {
                let prop_name = Some(Name::new("$call"));
                match lcall {
                    Some(lcall) => {
                        FlowJs::rec_flow(
                            cx,
                            trace,
                            &cx.find_call(lcall),
                            &UseT::new(UseTInner::UseT(use_op.dupe(), cx.find_call(ucall))),
                        )?;
                    }
                    None => {
                        let error_message = ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                            EPropNotFoundInSubtypingData {
                                reason_lower: lreason.dupe(),
                                reason_upper: ureason.dupe(),
                                prop_name,
                                use_op: use_op.dupe(),
                                suggestion: None,
                            },
                        ));
                        flow_js_utils::add_output(cx, error_message)?;
                    }
                }
            }
            let errs = {
                let mut acc: Vec<_> = vec![];
                for (name, up) in cx.find_props(uflds.dupe()).iter() {
                    let propref = type_util::mk_named_prop(
                        ureason
                            .dupe()
                            .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe()))),
                        false,
                        name.dupe(),
                    );
                    match lflds_map.get(name) {
                        Some(lp) => {
                            let prop_use_op = VirtualUseOp::Frame(
                                Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                                    PropertyCompatibilityData {
                                        prop: Some(name.dupe()),
                                        lower: lreason.dupe(),
                                        upper: ureason.dupe(),
                                    },
                                ))),
                                Arc::new(use_op.dupe()),
                            );
                            let new_errs = rec_flow_p_inner(
                                cx,
                                Some(trace),
                                prop_use_op,
                                Some((l, u)),
                                ureason,
                                true,
                                &propref,
                                &property::property_type(lp),
                                &property::property_type(up),
                            )?;
                            acc.extend(new_errs);
                        }
                        None => {
                            let lookup_kind = if let PropertyInner::Field(fd) = up.deref() {
                                if matches!(fd.type_.deref(), TypeInner::OptionalT { .. }) {
                                    LookupKind::NonstrictReturning(Box::new(
                                        NonstrictReturningData(None, None),
                                    ))
                                } else {
                                    LookupKind::Strict(lreason.dupe())
                                }
                            } else {
                                LookupKind::Strict(lreason.dupe())
                            };
                            FlowJs::rec_flow(
                                cx,
                                trace,
                                super_,
                                &UseT::new(UseTInner::ReposLowerT {
                                    reason: lreason.dupe(),
                                    use_desc: false,
                                    use_t: Box::new(UseT::new(UseTInner::LookupT(Box::new(
                                        LookupTData {
                                            reason: ureason.dupe(),
                                            lookup_kind: Box::new(lookup_kind),
                                            try_ts_on_failure: vec![].into(),
                                            propref: Box::new(propref.clone()),
                                            lookup_action: Box::new(
                                                LookupAction::LookupPropForSubtyping(Box::new(
                                                    LookupPropForSubtypingData {
                                                        use_op: use_op.dupe(),
                                                        prop: property::property_type(up),
                                                        prop_name: name.dupe(),
                                                        reason_lower: lreason.dupe(),
                                                        reason_upper: ureason.dupe(),
                                                    },
                                                )),
                                            ),
                                            method_accessible: false,
                                            ids: Some(
                                                [lown.dupe(), lproto.dupe()].into_iter().collect(),
                                            ),
                                            ignore_dicts: false,
                                        },
                                    )))),
                                }),
                            )?;
                        }
                    }
                }
                acc
            };
            add_output_prop_polarity_mismatch(cx, use_op.dupe(), lreason, ureason, errs)?;
            FlowJs::rec_flow(
                cx,
                trace,
                l,
                &UseT::new(UseTInner::UseT(use_op, uproto.dupe())),
            )
        }
        // If passing in an object literal where a record is expected,
        // provide an additional explanation.
        (TypeInner::DefT(lreason, ld), TypeInner::DefT(_, ud))
            if matches!(ld.deref(), DefTInner::ObjT(_))
                && matches!(
                    lreason.desc(true),
                    VirtualReasonDesc::RObjectLit | VirtualReasonDesc::RObjectLitUnsound
                )
                && let DefTInner::InstanceT(inst) = ud.deref()
                && matches!(inst.inst.inst_kind, InstanceKind::RecordKind { .. })
                && let Some(record_name) = &inst.inst.class_name =>
        {
            let (reason_lower, reason_upper) = ordered_reasons((
                type_util::reason_of_t(l).clone(),
                type_util::reason_of_t(u).clone(),
            ));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                    reason_lower,
                    reason_upper,
                    use_op,
                    explanation: Some(
                        intermediate_error_types::Explanation::ExplanationObjectLiteralNeedsRecordSyntax {
                            record_name: record_name.dupe(),
                            obj_reason: lreason.dupe(),
                        },
                    ),
                })),
            )
        }

        // For some object `x` and constructor `C`, if `x instanceof C`, then the
        // object is a subtype. We use `ExtendsUseT` to walk the proto chain of the
        // object, in case it includes a nominal type.
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!(ld.deref(), DefTInner::ObjT(_))
                && matches!(ud.deref(), DefTInner::InstanceT(_)) =>
        {
            FlowJs::rec_flow(
                cx,
                trace,
                l,
                &type_util::extends_use_type(use_op, l.dupe(), u.dupe()),
            )
        }

        // ****************************************
        // You can cast an object to a function
        // ****************************************
        (TypeInner::DefT(reason, ld), TypeInner::DefT(reason_op, ud))
            if matches!(ld.deref(), DefTInner::ObjT(_) | DefTInner::InstanceT(_))
                && matches!(ud.deref(), DefTInner::FunT(_, _)) =>
        {
            let fun_t = match l.deref() {
                TypeInner::DefT(_, ld2)
                    if let DefTInner::ObjT(obj) = ld2.deref()
                        && let Some(call_t) = &obj.call_t =>
                {
                    cx.find_call(*call_t)
                }
                TypeInner::DefT(_, ld2)
                    if let DefTInner::InstanceT(inst) = ld2.deref()
                        && let Some(call_t) = &inst.inst.inst_call_t =>
                {
                    cx.find_call(*call_t)
                }
                _ => {
                    let error_message = ErrorMessage::EIncompatibleWithUseOp(Box::new(
                        EIncompatibleWithUseOpData {
                            reason_lower: reason.dupe(),
                            reason_upper: reason_op.dupe(),
                            use_op: use_op.dupe(),
                            explanation: None,
                        },
                    ));
                    flow_js_utils::add_output(cx, error_message)?;
                    any_t::error(reason_op.dupe())
                }
            };
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                    PropertyCompatibilityData {
                        prop: Some(Name::new("$call")),
                        lower: reason.dupe(),
                        upper: reason_op.dupe(),
                    },
                ))),
                Arc::new(use_op),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, &fun_t, u)
        }

        // ********************************************
        // array types deconstruct into their parts
        // ********************************************
        (TypeInner::DefT(r1, ld), TypeInner::DefT(r2, ud))
            if let DefTInner::ArrT(arr1) = ld.deref()
                && let DefTInner::ArrT(arr2) = ud.deref()
                && let (
                    ArrType::ArrayAT(box ArrayATData {
                        elem_t: t1,
                        tuple_view: tv1,
                        ..
                    }),
                    ArrType::ArrayAT(box ArrayATData {
                        elem_t: t2,
                        tuple_view: tv2,
                        ..
                    }),
                ) = (arr1.as_ref(), arr2.as_ref()) =>
        {
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::ArrayElementCompatibility {
                    lower: r1.dupe(),
                    upper: r2.dupe(),
                }),
                Arc::new(use_op),
            );
            // These cases correspond to calls so the rest array can be considered "literal".
            let lit1 = match r1.desc(true) {
                VirtualReasonDesc::RArrayLitUnsound | VirtualReasonDesc::RReactChildren => true,
                VirtualReasonDesc::RRestArrayLit(desc) => matches!(
                    desc.as_ref(),
                    VirtualReasonDesc::RCode(_)
                        | VirtualReasonDesc::RFunctionCall(_)
                        | VirtualReasonDesc::RConstructorCall(_)
                        | VirtualReasonDesc::RMethodCall(_)
                        | VirtualReasonDesc::RJSXFunctionCall(_)
                        | VirtualReasonDesc::RFunctionType
                ),
                _ => false,
            };
            let ts1 = tv1
                .as_ref()
                .map(|tv| type_util::tuple_ts_of_elements(&tv.elements))
                .unwrap_or_default();
            let ts2 = tv2
                .as_ref()
                .map(|tv| type_util::tuple_ts_of_elements(&tv.elements))
                .unwrap_or_default();
            array_flow(cx, trace, use_op, lit1, r1, r2, l, u, &ts1, t1, &ts2, t2)
        }
        // Tuples can flow to tuples with the same arity
        (TypeInner::DefT(r1, ld), TypeInner::DefT(r2, ud))
            if let DefTInner::ArrT(arr1) = ld.deref()
                && let DefTInner::ArrT(arr2) = ud.deref()
                && let (
                    ArrType::TupleAT(box TupleATData {
                        elements: elements1,
                        arity: lower_arity,
                        inexact: lower_inexact,
                        ..
                    }),
                    ArrType::TupleAT(box TupleATData {
                        elements: elements2,
                        arity: upper_arity,
                        inexact: upper_inexact,
                        ..
                    }),
                ) = (arr1.as_ref(), arr2.as_ref()) =>
        {
            let fresh = flow_common::reason::is_literal_array_reason(r1);
            let (num_req1, num_total1) = *lower_arity;
            let (num_req2, num_total2) = *upper_arity;
            if !((*upper_inexact || !lower_inexact)
                && num_req1 >= num_req2
                && (num_total1 <= num_total2 || *upper_inexact))
            {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::ETupleArityMismatch(Box::new(ETupleArityMismatchData {
                        use_op: use_op.dupe(),
                        lower_reason: r1.dupe(),
                        lower_arity: *lower_arity,
                        lower_inexact: *lower_inexact,
                        upper_reason: r2.dupe(),
                        upper_arity: *upper_arity,
                        upper_inexact: *upper_inexact,
                        unify: false,
                    })),
                )?;
            } else {
                let mut n: i32 = 0;
                let tuple_element_compat = |n: i32,
                                            t1: &Type,
                                            t2: &Type,
                                            p1: Polarity,
                                            p2: Polarity,
                                            optional1: bool,
                                            optional2: bool|
                 -> Result<(), FlowJsException> {
                    if !(fresh || Polarity::compat(p1, p2)) {
                        flow_js_utils::add_output(
                            cx,
                            ErrorMessage::ETupleElementPolarityMismatch(Box::new(
                                ETupleElementPolarityMismatchData {
                                    index: n,
                                    reason_lower: r1.dupe(),
                                    polarity_lower: p1,
                                    reason_upper: r2.dupe(),
                                    polarity_upper: p2,
                                    use_op: use_op.dupe(),
                                },
                            )),
                        )?;
                    }
                    let elem_use_op = VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::TupleElementCompatibility(Box::new(
                            TupleElementCompatibilityData {
                                n,
                                lower: r1.dupe(),
                                upper: r2.dupe(),
                                lower_optional: optional1,
                                upper_optional: optional2,
                            },
                        ))),
                        Arc::new(use_op.dupe()),
                    );
                    // We don't want to allow `undefined` when an element is marked as optional:
                    //
                    // ```js
                    // type T = [number, b?: string];
                    // ([0, undefined]: T); // Should error
                    // ([0]: T); // Should be ok
                    // ([0, 's']: T); // Should be ok
                    // ```
                    //
                    // A user can always add `| void` to the element type if they want to denote this.
                    let t2_inner = if !optional1 {
                        if let (
                            true,
                            TypeInner::OptionalT {
                                type_: inner_t2, ..
                            },
                        ) = (optional2, t2.deref())
                        {
                            inner_t2
                        } else {
                            t2
                        }
                    } else {
                        t2
                    };
                    match (fresh, p2) {
                        (true, _) | (_, Polarity::Positive) => {
                            FlowJs::rec_flow_t(cx, trace, elem_use_op, t1, t2_inner)?;
                        }
                        (_, Polarity::Negative) => {
                            FlowJs::rec_flow_t(cx, trace, elem_use_op, t2_inner, t1)?;
                        }
                        (_, Polarity::Neutral) => {
                            FlowJs::rec_unify(
                                cx,
                                trace,
                                elem_use_op,
                                UnifyCause::Uncategorized,
                                None,
                                t1,
                                t2_inner,
                            )?;
                        }
                    }
                    Ok(())
                };
                let mut i1 = elements1.iter();
                let mut i2 = elements2.iter();
                loop {
                    let e1 = i1.next();
                    let e2 = i2.next();
                    match (e1, e2) {
                        (None, None) => break,
                        (
                            Some(TupleElement {
                                t: t1,
                                polarity: p1,
                                optional: optional1,
                                ..
                            }),
                            Some(TupleElement {
                                t: t2,
                                polarity: p2,
                                optional: optional2,
                                ..
                            }),
                        ) => {
                            tuple_element_compat(n, t1, t2, *p1, *p2, *optional1, *optional2)?;
                            n += 1;
                        }
                        (
                            None,
                            Some(TupleElement {
                                t: t2,
                                polarity: p2,
                                optional: optional2,
                                ..
                            }),
                        ) => {
                            let p1 = Polarity::Neutral;
                            let t1 =
                                void::make(r1.clone().replace_desc_new(
                                    VirtualReasonDesc::RTupleOutOfBoundsAccess(n),
                                ));
                            let optional1 = true;
                            tuple_element_compat(n, &t1, t2, p1, *p2, optional1, *optional2)?;
                            n += 1;
                        }
                        _ => {}
                    }
                }
            }
            Ok(())
        }

        // Arrays with known elements can flow to tuples
        (TypeInner::DefT(r1, ld), TypeInner::DefT(r2, ud))
            if let DefTInner::ArrT(arr1) = ld.deref()
                && let ArrType::ArrayAT(box ArrayATData {
                    elem_t: t1,
                    tuple_view,
                    react_dro,
                }) = arr1.as_ref()
                && matches!(ud.deref(), DefTInner::ArrT(arr) if matches!(arr.as_ref(), ArrType::TupleAT(box TupleATData { .. }))) =>
        {
            match tuple_view {
                None => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::ENonLitArrayToTuple((r1.dupe(), r2.dupe()), use_op),
                    )?;
                }
                Some(tv) => {
                    let tuple_t = Type::new(TypeInner::DefT(
                        r1.dupe(),
                        DefT::new(DefTInner::ArrT(Rc::new(ArrType::TupleAT(Box::new(
                            TupleATData {
                                elem_t: t1.dupe(),
                                elements: tv.elements.dupe(),
                                arity: tv.arity,
                                inexact: tv.inexact,
                                react_dro: react_dro.clone(),
                            },
                        ))))),
                    ));
                    FlowJs::rec_flow_t(cx, trace, use_op, &tuple_t, u)?;
                }
            }
            Ok(())
        }

        // Read only arrays are the super type of all tuples and arrays
        (TypeInner::DefT(r1, ld), TypeInner::DefT(r2, ud))
            if let DefTInner::ArrT(arr1) = ld.deref()
                && let DefTInner::ArrT(arr2) = ud.deref()
                && let ArrType::ROArrayAT(box (t2, _)) = arr2.as_ref() =>
        {
            let t1 = match arr1.as_ref() {
                ArrType::ArrayAT(box ArrayATData { elem_t, .. })
                | ArrType::TupleAT(box TupleATData { elem_t, .. }) => elem_t,
                ArrType::ROArrayAT(box (t, _)) => t,
            };
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::ArrayElementCompatibility {
                    lower: r1.dupe(),
                    upper: r2.dupe(),
                }),
                Arc::new(use_op),
            );
            FlowJs::rec_flow(
                cx,
                trace,
                t1,
                &UseT::new(UseTInner::UseT(use_op, t2.dupe())),
            )
        }
        (TypeInner::DefT(_, ld), TypeInner::DefT(r2, ud))
            if matches!(ld.deref(), DefTInner::InstanceT(_))
                && let DefTInner::ArrT(arr2) = ud.deref()
                && let ArrType::ArrayAT(box ArrayATData { elem_t, .. }) = arr2.as_ref() =>
        {
            let arrt = FlowJs::get_builtin_typeapp(cx, r2, None, "Array", vec![elem_t.dupe()]);
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op, arrt)))
        }
        (TypeInner::DefT(_, ld), TypeInner::DefT(r2, ud))
            if matches!(ld.deref(), DefTInner::InstanceT(_))
                && let DefTInner::ArrT(arr2) = ud.deref()
                && let ArrType::ROArrayAT(box (elemt, _)) = arr2.as_ref() =>
        {
            let arrt =
                FlowJs::get_builtin_typeapp(cx, r2, None, "$ReadOnlyArray", vec![elemt.dupe()]);
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op, arrt)))
        }

        // **************************************************
        // instances of classes follow declared hierarchy
        // **************************************************
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!(ld.deref(), DefTInner::InstanceT(_))
                && matches!(ud.deref(), DefTInner::InstanceT(_)) =>
        {
            FlowJs::rec_flow(
                cx,
                trace,
                l,
                &type_util::extends_use_type(use_op, l.dupe(), u.dupe()),
            )
        }

        // ********************************************************
        // runtime types derive static types through annotation
        // ********************************************************
        (TypeInner::DefT(rl, ld), TypeInner::DefT(_, ud))
            if let (DefTInner::ClassT(l_inner), DefTInner::ClassT(u_inner)) =
                (ld.deref(), ud.deref()) =>
        {
            let repositioned = FlowJs::reposition(
                cx,
                Some(trace),
                rl.loc().clone(),
                None,
                None,
                l_inner.dupe(),
            )?;
            FlowJs::rec_flow(
                cx,
                trace,
                &repositioned,
                &UseT::new(UseTInner::UseT(use_op, u_inner.dupe())),
            )
        }

        // ***********************************************
        // You can use a function as a callable object
        // ***********************************************
        (TypeInner::DefT(_, ld), TypeInner::DefT(reason, ud))
            if matches!(ld.deref(), DefTInner::FunT(_, _))
                && let DefTInner::ObjT(o) = ud.deref()
                && let Some(call_t) = &o.call_t =>
        {
            let t = cx.find_call(*call_t);
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op.dupe(), t)))?;
            let new_obj = ObjType {
                flags: o.flags.clone(),
                props_tmap: o.props_tmap.dupe(),
                proto_t: o.proto_t.dupe(),
                call_t: None,
                reachable_targs: o.reachable_targs.dupe(),
            };
            let obj_t = Type::new(TypeInner::DefT(
                reason.dupe(),
                DefT::new(DefTInner::ObjT(Rc::new(new_obj))),
            ));
            FlowJs::rec_flow_t(cx, trace, use_op, l, &obj_t)
        }

        (TypeInner::DefT(_, ld), TypeInner::DefT(reason, ud))
            if matches!(ld.deref(), DefTInner::FunT(_, _))
                && let DefTInner::InstanceT(inst_t) = ud.deref()
                && let Some(call_t) = &inst_t.inst.inst_call_t =>
        {
            let t = cx.find_call(*call_t);
            FlowJs::rec_flow(cx, trace, l, &UseT::new(UseTInner::UseT(use_op.dupe(), t)))?;
            let inst_type = {
                let mut new_inst_inner: InstTypeInner = inst_t.inst.deref().clone();
                new_inst_inner.inst_call_t = None;
                let new_instance_t = InstanceT::new(InstanceTInner {
                    inst: InstType::new(new_inst_inner),
                    static_: inst_t.static_.dupe(),
                    super_: inst_t.super_.dupe(),
                    implements: inst_t.implements.clone(),
                });
                Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::InstanceT(Rc::new(new_instance_t))),
                ))
            };
            FlowJs::rec_flow_t(cx, trace, use_op, l, &inst_type)
        }

        // FunT ~> ObjT
        //
        // Previously, call properties were stored in the props map, and were
        // checked against dictionary upper bounds. This is wrong, but useful for
        // distinguishing between thunk-like types found in graphql-js.
        //
        // Now that call properties are stored separately, it is particularly
        // egregious to emit this constraint. This only serves to maintain buggy
        // behavior, which should be fixed, and this code removed.
        (TypeInner::DefT(lreason, ld), TypeInner::DefT(ureason, ud))
            if matches!(ld.deref(), DefTInner::FunT(_, _))
                && let DefTInner::ObjT(obj) = ud.deref()
                && matches!(obj.flags.obj_kind, ObjKind::Exact | ObjKind::Indexed(_)) =>
        {
            let reasons = ordered_reasons((lreason.dupe(), ureason.dupe()));
            match &obj.flags.obj_kind {
                ObjKind::Exact => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EIncompatibleWithExact(
                            reasons,
                            use_op,
                            intermediate_error_types::ExactnessErrorKind::UnexpectedInexact,
                        ),
                    )?;
                }
                ObjKind::Indexed(_) => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EFunctionIncompatibleWithIndexer(reasons, use_op),
                    )?;
                }
                _ => {}
            }
            Ok(())
        }
        // TODO: This rule doesn't interact very well with union-type checking. It
        // looks up Function.prototype, which currently doesn't appear structurally
        // in the function type, and thus may not be fully resolved when the
        // function type is checked with a union containing the object
        // type. Ideally, we should either add Function.prototype to function types
        // or fully resolve them when resolving function types, but either way we
        // might bomb perf without additional work. Meanwhile, we need an immediate
        // fix for the common case where this bug shows up. So leaving this comment
        // here as a marker for future work, while going with a band-aid solution
        // for now, as motivated below.
        //
        // Fortunately, it is quite hard for a function type to successfully
        // check against an object type, and even more unlikely when the latter
        // is part of a union: the object type must only contain
        // Function.prototype methods or statics. Quickly confirming that the
        // check would fail before looking up Function.prototype (while falling
        // back to the general rule when we cannot guarantee failure) is a safe
        // optimization in any case, and fixes the commonly observed case where
        // the union type contains both a function type and a object type as
        // members, clearly intending for function types to match the former
        // instead of the latter.
        (TypeInner::DefT(reason, ld), TypeInner::DefT(reason_o, ud))
            if let (DefTInner::FunT(statics, _), DefTInner::ObjT(obj)) =
                (ld.deref(), ud.deref()) =>
        {
            if !flow_js_utils::quick_error_fun_as_obj(
                cx,
                &use_op,
                reason,
                statics,
                reason_o,
                &cx.find_props(obj.props_tmap.dupe()),
            )? {
                FlowJs::rec_flow_t(cx, trace, use_op, statics, u)?;
            }
            Ok(())
        }
        // TODO: similar concern as above
        (TypeInner::DefT(reason, ld), TypeInner::DefT(reason_inst, ud))
            if let (DefTInner::FunT(statics, _), DefTInner::InstanceT(inst_t)) =
                (ld.deref(), ud.deref())
                && matches!(inst_t.inst.inst_kind, InstanceKind::InterfaceKind { .. }) =>
        {
            let mut filtered = cx.find_props(inst_t.inst.own_props.dupe());
            filtered.remove(&Name::new("constructor"));
            if !flow_js_utils::quick_error_fun_as_obj(
                cx,
                &use_op,
                reason,
                statics,
                reason_inst,
                &filtered,
            )? {
                FlowJs::rec_flow_t(cx, trace, use_op, statics, u)?;
            }
            Ok(())
        }

        // ***************************************************
        // classes and arrays can implement some interface
        // ***************************************************
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!(ld.deref(), DefTInner::ClassT(_) | DefTInner::ArrT(_))
                && matches!(ud.deref(), DefTInner::InstanceT(inst) if matches!(inst.inst.inst_kind, InstanceKind::InterfaceKind { .. })) =>
        {
            FlowJs::rec_flow(
                cx,
                trace,
                u,
                &UseT::new(UseTInner::ImplementsT(use_op, l.dupe())),
            )
        }
        (TypeInner::DefT(reason, ld), TypeInner::DefT(interface_reason, ud))
            if matches!(
                ld.deref(),
                DefTInner::BoolGeneralT | DefTInner::SingletonBoolT { .. }
            ) && matches!(ud.deref(), DefTInner::InstanceT(inst) if matches!(inst.inst.inst_kind, InstanceKind::InterfaceKind { .. })) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EPrimitiveAsInterface(Box::new(EPrimitiveAsInterfaceData {
                    use_op,
                    reason: reason.dupe(),
                    interface_reason: interface_reason.dupe(),
                    kind: intermediate_error_types::PrimitiveKind::Boolean,
                })),
            )
        }
        (TypeInner::DefT(reason, ld), TypeInner::DefT(interface_reason, ud))
            if matches!(
                ld.deref(),
                DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
            ) && matches!(ud.deref(), DefTInner::InstanceT(inst) if matches!(inst.inst.inst_kind, InstanceKind::InterfaceKind { .. })) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EPrimitiveAsInterface(Box::new(EPrimitiveAsInterfaceData {
                    use_op,
                    reason: reason.dupe(),
                    interface_reason: interface_reason.dupe(),
                    kind: intermediate_error_types::PrimitiveKind::Number,
                })),
            )
        }
        (TypeInner::DefT(reason, ld), TypeInner::DefT(interface_reason, ud))
            if matches!(
                ld.deref(),
                DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
            ) && matches!(ud.deref(), DefTInner::InstanceT(inst) if matches!(inst.inst.inst_kind, InstanceKind::InterfaceKind { .. })) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EPrimitiveAsInterface(Box::new(EPrimitiveAsInterfaceData {
                    use_op,
                    reason: reason.dupe(),
                    interface_reason: interface_reason.dupe(),
                    kind: intermediate_error_types::PrimitiveKind::String,
                })),
            )
        }

        // ************************************
        // opaque types lower & upper bound
        // ************************************

        // When both bounds are available, we need to do a speculative check, since only one of them
        // needs to pass.
        (
            TypeInner::NominalT {
                reason: opaque_l_reason,
                nominal_type: lnom,
            },
            TypeInner::NominalT {
                reason: opaque_u_reason,
                nominal_type: unom,
            },
        ) if lnom.upper_t.is_some() && unom.lower_t.is_some() => {
            let lower_upper = lnom.upper_t.as_ref().unwrap().dupe();
            let upper_lower = unom.lower_t.as_ref().unwrap().dupe();
            let l_clone = l.dupe();
            let u_clone = u.dupe();
            let opaque_u_reason_c = opaque_u_reason.dupe();
            let opaque_l_reason_c = opaque_l_reason.dupe();
            let use_op_c1 = use_op.dupe();
            let use_op_c2 = use_op.dupe();
            speculation_kit::try_custom(
                cx,
                Some(use_op),
                None,
                None,
                opaque_l_reason.loc().clone(),
                vec![
                    Box::new(move |cx: &Context| {
                        let use_op = VirtualUseOp::Frame(
                            Arc::new(VirtualFrameUseOp::OpaqueTypeLowerBound {
                                opaque_t_reason: opaque_u_reason_c,
                            }),
                            Arc::new(use_op_c1),
                        );
                        FlowJs::rec_flow_t(cx, trace, use_op, &l_clone, &upper_lower)
                    }),
                    Box::new(move |cx: &Context| {
                        let use_op = VirtualUseOp::Frame(
                            Arc::new(VirtualFrameUseOp::OpaqueTypeUpperBound {
                                opaque_t_reason: opaque_l_reason_c,
                            }),
                            Arc::new(use_op_c2),
                        );
                        FlowJs::rec_flow_t(cx, trace, use_op, &lower_upper, &u_clone)
                    }),
                ],
            )
        }
        // Opaque types may be treated as their upper bound when they are a lower bound for a use
        (
            TypeInner::NominalT {
                reason: opaque_t_reason,
                nominal_type: lnom,
            },
            _,
        ) if let Some(t) = &lnom.upper_t => {
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::OpaqueTypeUpperBound {
                    opaque_t_reason: opaque_t_reason.dupe(),
                }),
                Arc::new(use_op),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, t, u)
        }

        // Similar to the case of NominalT { upper_t=Some _ }  ~> NominalT { lower_t=Some _ }
        // We need to do the same for GenericT.
        (
            TypeInner::GenericT(box GenericTData {
                reason,
                bound: lower_upper,
                ..
            }),
            TypeInner::NominalT {
                reason: opaque_u_reason,
                nominal_type: unom,
            },
        ) if unom.lower_t.is_some() => {
            let upper_lower = unom.lower_t.as_ref().unwrap().dupe();
            let l_clone = l.dupe();
            let u_clone = u.dupe();
            let lower_upper = lower_upper.dupe();
            let opaque_u_reason_c = opaque_u_reason.dupe();
            let reason_c = reason.dupe();
            let use_op_c1 = use_op.dupe();
            let use_op_c2 = use_op.dupe();
            speculation_kit::try_custom(
                cx,
                Some(use_op),
                None,
                None,
                reason.loc().clone(),
                vec![
                    Box::new(move |cx: &Context| {
                        let use_op = VirtualUseOp::Frame(
                            Arc::new(VirtualFrameUseOp::OpaqueTypeLowerBound {
                                opaque_t_reason: opaque_u_reason_c,
                            }),
                            Arc::new(use_op_c1),
                        );
                        FlowJs::rec_flow_t(cx, trace, use_op, &l_clone, &upper_lower)
                    }),
                    Box::new(move |cx: &Context| {
                        let repositioned = FlowJs::reposition_reason(
                            cx,
                            Some(trace),
                            &reason_c,
                            None,
                            &lower_upper,
                        )?;
                        FlowJs::rec_flow_t(cx, trace, use_op_c2, &repositioned, &u_clone)
                    }),
                ],
            )
        }

        // Opaque types may be treated as their lower bound when they are a upper bound for a use
        (
            _,
            TypeInner::NominalT {
                reason: opaque_t_reason,
                nominal_type: unom,
            },
        ) if let Some(t) = &unom.lower_t => {
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::OpaqueTypeLowerBound {
                    opaque_t_reason: opaque_t_reason.dupe(),
                }),
                Arc::new(use_op),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, l, t)
        }

        // *********************
        // functions statics
        // *********************
        (TypeInner::DefT(reason, ld), TypeInner::AnyT(_, _))
            if let DefTInner::FunT(static_, _) = ld.deref() =>
        {
            FlowJs::rec_flow(
                cx,
                trace,
                static_,
                &UseT::new(UseTInner::ReposLowerT {
                    reason: reason.dupe(),
                    use_desc: false,
                    use_t: Box::new(UseT::new(UseTInner::UseT(use_op, u.dupe()))),
                }),
            )
        }

        // *****************
        // class statics
        // *****************
        (TypeInner::DefT(reason, ld), _)
            if let DefTInner::ClassT(instance) = ld.deref()
                && (matches!(
                    u.deref(),
                    TypeInner::DefT(_, ud) if matches!(ud.deref(), DefTInner::ObjT(_))
                ) || matches!(u.deref(), TypeInner::AnyT(_, _))) =>
        {
            let tvar_id = flow_typing_tvar::mk_no_wrap(cx, reason);
            let statics_tvar = Tvar::new(reason.dupe(), tvar_id as u32);
            FlowJs::rec_flow(
                cx,
                trace,
                instance,
                &UseT::new(UseTInner::GetStaticsT(Box::new(statics_tvar.dupe()))),
            )?;
            let open_t = Type::new(TypeInner::OpenT(statics_tvar));
            FlowJs::rec_flow_t(cx, trace, use_op, &open_t, u)
        }

        // ************************
        // classes as functions
        // ************************

        // When a class value flows to a function annotation or call site, check for
        // the presence of a call property in the former (as a static) compatible
        // with the latter.
        //
        // TODO: Call properties are excluded from the subclass compatibility
        // checks, which makes it unsafe to call a Class<T> type like this.
        // For example:
        //
        // ```js
        // declare class A { static (): string };
        // declare class B extends A { static (): number }
        // var klass: Class<A> = B;
        // var foo: string = klass(); // passes, but `foo` is a number
        // ```
        //
        // The same issue is also true for constructors, which are similarly
        // excluded from subclass compatibility checks, but are allowed on ClassT
        // types.
        (TypeInner::DefT(reason, ld), TypeInner::DefT(_, ud))
            if let DefTInner::ClassT(instance) = ld.deref()
                && matches!(ud.deref(), DefTInner::FunT(_, _)) =>
        {
            let tvar_id = flow_typing_tvar::mk_no_wrap(cx, reason);
            let statics_tvar = Tvar::new(reason.dupe(), tvar_id as u32);
            FlowJs::rec_flow(
                cx,
                trace,
                instance,
                &UseT::new(UseTInner::GetStaticsT(Box::new(statics_tvar.dupe()))),
            )?;
            let open_t = Type::new(TypeInner::OpenT(statics_tvar));
            FlowJs::rec_flow_t(cx, trace, use_op, &open_t, u)
        }

        // *********
        // Enums
        // *********
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!((ld.deref(), ud.deref()), (
                DefTInner::EnumObjectT { enum_info: ei1, .. },
                DefTInner::EnumObjectT { enum_info: ei2, .. },
            ) if matches!((ei1.deref().deref(), ei2.deref().deref()), (
                EnumInfoInner::ConcreteEnum(c1),
                EnumInfoInner::ConcreteEnum(c2),
            ) if c1.enum_id == c2.enum_id)) =>
        {
            Ok(())
        }
        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if matches!((ld.deref(), ud.deref()), (
                DefTInner::EnumValueT(ei1),
                DefTInner::EnumValueT(ei2),
            ) if matches!((ei1.deref().deref(), ei2.deref().deref()), (
                EnumInfoInner::ConcreteEnum(c1),
                EnumInfoInner::ConcreteEnum(c2),
            ) if c1.enum_id == c2.enum_id)) =>
        {
            Ok(())
        }
        (TypeInner::DefT(enum_reason_l, ld), TypeInner::DefT(enum_reason_u, ud))
            if let Some((c1, c2)) = {
                match (ld.deref(), ud.deref()) {
                    (
                        DefTInner::EnumObjectT { enum_info: ei1, .. },
                        DefTInner::EnumObjectT { enum_info: ei2, .. },
                    ) => match (ei1.deref().deref(), ei2.deref().deref()) {
                        (EnumInfoInner::ConcreteEnum(c1), EnumInfoInner::ConcreteEnum(c2)) => {
                            Some((c1, c2))
                        }
                        _ => None,
                    },
                    (DefTInner::EnumValueT(ei1), DefTInner::EnumValueT(ei2)) => {
                        match (ei1.deref().deref(), ei2.deref().deref()) {
                            (EnumInfoInner::ConcreteEnum(c1), EnumInfoInner::ConcreteEnum(c2)) => {
                                Some((c1, c2))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                }
            } && type_util::nominal_id_have_same_logical_module(
                &cx.file_options(),
                cx.projects_options(),
                (&c1.enum_id, Some(c1.enum_name.as_str())),
                (&c2.enum_id, Some(c2.enum_name.as_str())),
            ) && c1.members.keys().collect::<std::collections::BTreeSet<_>>()
                == c2.members.keys().collect::<std::collections::BTreeSet<_>>()
                && c1.has_unknown_members == c2.has_unknown_members =>
        {
            if type_util::is_in_common_interface_conformance_check(&use_op) {
                let repr_pair = match (ld.deref(), ud.deref()) {
                    (
                        DefTInner::EnumObjectT { enum_info: ei1, .. },
                        DefTInner::EnumObjectT { enum_info: ei2, .. },
                    ) => match (ei1.deref().deref(), ei2.deref().deref()) {
                        (EnumInfoInner::ConcreteEnum(c1), EnumInfoInner::ConcreteEnum(c2)) => {
                            Some((&c1.representation_t, &c2.representation_t))
                        }
                        _ => None,
                    },
                    (DefTInner::EnumValueT(ei1), DefTInner::EnumValueT(ei2)) => {
                        match (ei1.deref().deref(), ei2.deref().deref()) {
                            (EnumInfoInner::ConcreteEnum(c1), EnumInfoInner::ConcreteEnum(c2)) => {
                                Some((&c1.representation_t, &c2.representation_t))
                            }
                            _ => None,
                        }
                    }
                    _ => None,
                };
                if let Some((r1, r2)) = repr_pair {
                    let use_op = VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::EnumRepresentationTypeCompatibility {
                            lower: enum_reason_l.dupe(),
                            upper: enum_reason_u.dupe(),
                        }),
                        Arc::new(use_op),
                    );
                    FlowJs::rec_flow_t(cx, trace, use_op, r1, r2)?;
                }
            }
            Ok(())
        }

        (TypeInner::DefT(_, ld), TypeInner::DefT(_, ud))
            if let (
                DefTInner::EnumObjectT {
                    enum_value_t: ev1, ..
                },
                DefTInner::EnumObjectT {
                    enum_value_t: ev2, ..
                },
            ) = (ld.deref(), ud.deref()) =>
        {
            FlowJs::rec_flow_t(cx, trace, use_op, ev1, ev2)
        }
        (TypeInner::DefT(enum_reason_l, ld), TypeInner::DefT(enum_reason_u, ud))
            if let (DefTInner::EnumValueT(ei_l), DefTInner::EnumValueT(ei_u)) =
                (ld.deref(), ud.deref())
                && let EnumInfoInner::AbstractEnum {
                    representation_t: repr_u,
                } = ei_u.deref().deref() =>
        {
            let repr_l = match ei_l.deref().deref() {
                EnumInfoInner::ConcreteEnum(c) => &c.representation_t,
                EnumInfoInner::AbstractEnum { representation_t } => representation_t,
            };
            let use_op = VirtualUseOp::Frame(
                Arc::new(VirtualFrameUseOp::EnumRepresentationTypeCompatibility {
                    lower: enum_reason_l.dupe(),
                    upper: enum_reason_u.dupe(),
                }),
                Arc::new(use_op),
            );
            FlowJs::rec_flow_t(cx, trace, use_op, repr_l, repr_u)
        }
        (TypeInner::DefT(enum_reason, ld), _)
            if let DefTInner::EnumValueT(ei) = ld.deref()
                && let repr = match ei.deref().deref() {
                    EnumInfoInner::ConcreteEnum(c) => &c.representation_t,
                    EnumInfoInner::AbstractEnum { representation_t } => representation_t,
                }
                && type_util::quick_subtype(None::<&fn(&Type)>, repr, u) =>
        {
            let enum_kind = match ei.deref().deref() {
                EnumInfoInner::ConcreteEnum(_) => {
                    flow_typing_errors::error_message::EnumKind::ConcreteEnumKind
                }
                EnumInfoInner::AbstractEnum { .. } => {
                    flow_typing_errors::error_message::EnumKind::AbstractEnumKind
                }
            };
            let representation_type = match repr.deref() {
                TypeInner::DefT(_, d)
                    if matches!(
                        d.deref(),
                        DefTInner::BoolGeneralT | DefTInner::SingletonBoolT { .. }
                    ) =>
                {
                    Some(FlowSmolStr::new_inline("boolean"))
                }
                TypeInner::DefT(_, d)
                    if matches!(
                        d.deref(),
                        DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
                    ) =>
                {
                    Some(FlowSmolStr::new_inline("number"))
                }
                TypeInner::DefT(_, d)
                    if matches!(
                        d.deref(),
                        DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
                    ) =>
                {
                    Some(FlowSmolStr::new_inline("string"))
                }
                TypeInner::DefT(_, d)
                    if matches!(d.deref(), DefTInner::SymbolT | DefTInner::UniqueSymbolT(_)) =>
                {
                    Some(FlowSmolStr::new_inline("symbol"))
                }
                TypeInner::DefT(_, d)
                    if matches!(
                        d.deref(),
                        DefTInner::BigIntGeneralT(_) | DefTInner::SingletonBigIntT { .. }
                    ) =>
                {
                    Some(FlowSmolStr::new_inline("bigint"))
                }
                _ => None,
            };
            let casting_syntax = cx.casting_syntax();
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumIncompatible(Box::new(
                    EnumIncompatibleData {
                        reason_lower: enum_reason.dupe(),
                        reason_upper: type_util::reason_of_t(u).clone(),
                        use_op,
                        enum_kind,
                        representation_type,
                        casting_syntax,
                    },
                ))),
            )
        }

        (
            TypeInner::GenericT(box GenericTData {
                bound: bound1,
                id: id1,
                reason: reason1,
                name: name1,
                no_infer: no_infer1,
            }),
            TypeInner::GenericT(box GenericTData {
                bound: bound2,
                id: id2,
                reason: reason2,
                name: name2,
                no_infer: no_infer2,
            }),
        ) => {
            use flow_typing_generics::GenericSatResult;
            let result = id1.satisfies(
                &|msgs| {
                    flow_typing_debug::verbose::print_if_verbose_lazy(
                        cx,
                        Some(&trace),
                        None,
                        None,
                        || msgs.to_vec(),
                    );
                },
                id2,
            );
            match result {
                GenericSatResult::Satisfied => {
                    let t1 = FlowJs::reposition_reason(cx, Some(trace), reason1, None, bound1)?;
                    let t2 = FlowJs::reposition_reason(cx, Some(trace), reason2, None, bound2)?;
                    FlowJs::rec_flow_t(cx, trace, use_op, &t1, &t2)
                }
                GenericSatResult::Lower(id) => {
                    let new_l = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                        reason: reason1.dupe(),
                        name: name1.dupe(),
                        bound: bound1.dupe(),
                        no_infer: *no_infer1,
                        id,
                    })));
                    let t2 = FlowJs::reposition_reason(cx, Some(trace), reason2, None, bound2)?;
                    FlowJs::rec_flow_t(cx, trace, use_op, &new_l, &t2)
                }
                GenericSatResult::Upper(id) => {
                    let t1 = FlowJs::reposition_reason(cx, Some(trace), reason1, None, bound1)?;
                    let new_u = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                        reason: reason2.dupe(),
                        name: name2.dupe(),
                        bound: bound2.dupe(),
                        no_infer: *no_infer2,
                        id,
                    })));
                    FlowJs::rec_flow_t(cx, trace, use_op, &t1, &new_u)
                }
            }
        }

        (TypeInner::GenericT(box GenericTData { reason, bound, .. }), _) => {
            let t = FlowJs::reposition_reason(cx, Some(trace), reason, None, bound)?;
            FlowJs::rec_flow_t(cx, trace, use_op, &t, u)
        }
        (_, TypeInner::GenericT(box GenericTData { reason, name, .. })) => {
            let bot = Type::new(TypeInner::DefT(
                reason
                    .dupe()
                    .replace_desc(VirtualReasonDesc::RIncompatibleInstantiation(name.dupe())),
                DefT::new(DefTInner::EmptyT),
            ));
            FlowJs::rec_flow_t(cx, trace, use_op, l, &bot)
        }
        (TypeInner::ObjProtoT(reason), _) => {
            let obj_proto =
                FlowJs::get_builtin_type(cx, Some(trace), reason, Some(true), "Object")?;
            FlowJs::rec_flow_t(cx, trace, use_op, &obj_proto, u)
        }
        (_, TypeInner::ObjProtoT(reason)) => {
            let obj_proto =
                FlowJs::get_builtin_type(cx, Some(trace), reason, Some(true), "Object")?;
            FlowJs::rec_flow_t(cx, trace, use_op, l, &obj_proto)
        }
        (TypeInner::FunProtoT(reason), _) => {
            let fun_proto =
                FlowJs::get_builtin_type(cx, Some(trace), reason, Some(true), "Function")?;
            FlowJs::rec_flow_t(cx, trace, use_op, &fun_proto, u)
        }
        (_, TypeInner::FunProtoT(reason)) => {
            let fun_proto =
                FlowJs::get_builtin_type(cx, Some(trace), reason, Some(true), "Function")?;
            FlowJs::rec_flow_t(cx, trace, use_op, l, &fun_proto)
        }
        (TypeInner::DefT(lreason, ld), TypeInner::DefT(ureason, ud))
            if matches!(ld.deref(), DefTInner::MixedT(MixedFlavor::MixedFunction))
                && matches!(ud.deref(), DefTInner::FunT(_, _)) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatible(Box::new(EIncompatibleData {
                    lower: (lreason.dupe(), None),
                    upper: (
                        ureason.dupe(),
                        flow_typing_errors::error_message::UpperKind::IncompatibleMixedCallT,
                    ),
                    use_op: Some(use_op.dupe()),
                })),
            )?;
            let any = any_t::make(AnySource::AnyError(None), lreason.dupe());
            FlowJs::rec_flow_t(cx, trace, use_op, &any, u)
        }
        (TypeInner::FunProtoBindT(reason), _) => {
            let fun_proto_t = Type::new(TypeInner::FunProtoT(reason.dupe()));
            FlowJs::rec_flow_t(cx, trace, use_op, &fun_proto_t, u)
        }
        (
            TypeInner::NominalT {
                reason,
                nominal_type: lnom,
            },
            _,
        ) if lnom.nominal_id == nominal::Id::InternalEnforceUnionOptimized => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EUnionOptimizationOnNonUnion(Box::new(
                    EUnionOptimizationOnNonUnionData {
                        loc: reason.loc().dupe(),
                        arg: type_util::reason_of_t(u).dupe(),
                    },
                )),
            )
        }

        _ => {
            let reason_lower = type_util::generalized_reason_of_t(u, l);
            let reason_upper = type_util::generalized_reason_of_t(l, u);
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                    reason_lower,
                    reason_upper,
                    use_op,
                    explanation: None,
                })),
            )
        }
    }
}

pub fn rec_flow_p<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    use_op: UseOp,
    report_polarity: bool,
    lreason: &Reason,
    ureason: &Reason,
    propref: &PropRef,
    lp: &PropertyType,
    up: &PropertyType,
) -> Result<(), FlowJsException> {
    let errs = rec_flow_p_inner(
        cx,
        trace,
        use_op.dupe(),
        None,
        ureason,
        report_polarity,
        propref,
        lp,
        up,
    )?;
    add_output_prop_polarity_mismatch(cx, use_op, lreason, ureason, errs)?;
    Ok(())
}
