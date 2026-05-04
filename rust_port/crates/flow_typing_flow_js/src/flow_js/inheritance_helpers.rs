/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;
use std::sync::Arc;

use flow_typing_errors::error_message::EPropNotFoundInSubtypingData;
use flow_typing_type::type_::LookupTData;
use flow_typing_type::type_::NonstrictReturningData;
use flow_typing_type::type_::PropertyCompatibilityData;

use super::helpers::*;
use super::*;

// *********************
// * inheritance utils *
// *********************

pub(super) fn flow_type_args<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    lreason: &Reason,
    ureason: &Reason,
    targs1: Rc<[(SubstName, Reason, Type, Polarity)]>,
    targs2: Rc<[(SubstName, Reason, Type, Polarity)]>,
) -> Result<(), FlowJsException> {
    use flow_typing_type::type_::TypeArgCompatibilityData;
    use flow_typing_type::type_::VirtualFrameUseOp;
    assert_eq!(
        targs1.len(),
        targs2.len(),
        "flow_type_args: mismatched type argument lengths"
    );
    for ((x, targ_reason, t1, polarity), (_, _, t2, _)) in
        targs1.iter().cloned().zip(targs2.iter().cloned())
    {
        let use_op = UseOp::Frame(
            std::sync::Arc::new(VirtualFrameUseOp::TypeArgCompatibility(Box::new(
                TypeArgCompatibilityData {
                    name: x,
                    targ: targ_reason,
                    lower: lreason.dupe(),
                    upper: ureason.dupe(),
                    polarity,
                },
            ))),
            std::sync::Arc::new(use_op.dupe()),
        );
        match polarity {
            Polarity::Negative => {
                rec_flow(cx, trace, (&t2, &UseT::new(UseTInner::UseT(use_op, t1))))?;
            }
            Polarity::Positive => {
                rec_flow(cx, trace, (&t1, &UseT::new(UseTInner::UseT(use_op, t2))))?;
            }
            Polarity::Neutral => {
                rec_unify(cx, trace, use_op, UnifyCause::Uncategorized, None, &t1, &t2)?;
            }
        }
    }
    Ok(())
}

pub(super) fn inst_type_to_obj_type<'cx>(
    cx: &Context<'cx>,
    reason_struct: Reason,
    own_props_id: properties::Id,
    proto_props_id: properties::Id,
    call_id: Option<i32>,
    inst_dict: &Option<DictType>,
) -> Type {
    let own_props = cx.find_props(own_props_id);
    let proto_props = cx.find_props(proto_props_id);
    let props_tmap = properties::Id::generate_id();
    let mut merged = own_props.dupe();
    for (k, v) in proto_props.iter() {
        if !merged.contains_key(k) {
            merged.insert(k.dupe(), v.dupe());
        }
    }
    cx.add_property_map(props_tmap.dupe(), merged);
    // Interfaces with an indexer type are indexed, all others are inexact
    let obj_kind = match inst_dict {
        Some(d) => ObjKind::Indexed(d.clone()),
        None => ObjKind::Inexact,
    };
    let o = ObjType {
        // flags = { obj_kind; react_dro = None };
        flags: Flags {
            obj_kind,
            react_dro: None,
        },
        props_tmap,
        // Interfaces have no prototype
        proto_t: Type::new(TypeInner::ObjProtoT(reason_struct.dupe())),
        call_t: call_id,
        reachable_targs: Rc::from([]),
    };
    Type::new(TypeInner::DefT(
        reason_struct,
        DefT::new(DefTInner::ObjT(Rc::new(o))),
    ))
}

// dispatch checks to verify that lower satisfies the structural
// requirements given in the tuple. *)
// TODO: own_props/proto_props is misleading, since they come from interfaces,
// which don't have an own/proto distinction.
pub(super) fn structural_subtype<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    lower: &Type,
    reason_struct: &Reason,
    (own_props_id, proto_props_id, call_id, inst_dict): (
        properties::Id,
        properties::Id,
        Option<i32>,
        &Option<DictType>,
    ),
) -> Result<(), FlowJsException> {
    match lower.deref() {
        // Object <: Interface subtyping creates an object out of the interface to dispatch to the
        // existing object <: object logic
        TypeInner::DefT(lreason, def_t) if let DefTInner::ObjT(l_obj) = def_t.deref() => {
            let lkind = &l_obj.flags.obj_kind;
            let lprops = l_obj.props_tmap.dupe();
            let lproto = &l_obj.proto_t;
            let lcall = l_obj.call_t;
            let lreachable_targs = &l_obj.reachable_targs;
            let o = inst_type_to_obj_type(
                cx,
                reason_struct.dupe(),
                own_props_id,
                proto_props_id,
                call_id,
                inst_dict,
            );
            let lower = Type::new(TypeInner::DefT(
                lreason.dupe(),
                DefT::new(DefTInner::ObjT(Rc::new(ObjType {
                    flags: Flags {
                        obj_kind: lkind.clone(),
                        react_dro: None,
                    },
                    props_tmap: lprops,
                    proto_t: lproto.dupe(),
                    call_t: lcall,
                    reachable_targs: lreachable_targs.dupe(),
                }))),
            ));
            rec_flow_t(cx, trace, use_op, (&lower, &o))?;
        }
        _ => {
            inst_structural_subtype(
                cx,
                trace,
                use_op,
                lower,
                reason_struct,
                (own_props_id, proto_props_id, call_id, inst_dict),
            )?;
        }
    }
    Ok(())
}

pub(super) fn inst_structural_subtype<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    lower: &Type,
    reason_struct: &Reason,
    (own_props_id, proto_props_id, call_id, inst_dict): (
        properties::Id,
        properties::Id,
        Option<i32>,
        &Option<DictType>,
    ),
) -> Result<(), FlowJsException> {
    let lreason = reason_of_t(lower);
    let lit = is_literal_object_reason(lreason);
    let own_props = cx.find_props(own_props_id);
    let proto_props = cx.find_props(proto_props_id);
    let call_t = call_id.map(|id| cx.find_call(id));
    let read_only_if_lit = |p: &Property| -> PropertyType {
        match p.deref() {
            PropertyInner::Field(fd) if lit => PropertyType::OrdinaryField {
                type_: fd.type_.dupe(),
                polarity: Polarity::Positive,
            },
            _ => property::property_type(p),
        }
    };
    if let Some(dict) = inst_dict {
        let ukey = &dict.key;
        let uvalue = &dict.value;
        let upolarity = &dict.dict_polarity;
        match lower.deref() {
            TypeInner::DefT(_, def_t) if let DefTInner::InstanceT(inst_t) = def_t.deref() => {
                if let Some(ref l_dict) = inst_t.inst.inst_dict {
                    let lkey = &l_dict.key;
                    let lvalue = &l_dict.value;
                    let lpolarity = &l_dict.dict_polarity;
                    subtyping_kit::rec_flow_p(
                        cx,
                        Some(trace),
                        UseOp::Frame(
                            Arc::new(VirtualFrameUseOp::IndexerKeyCompatibility {
                                lower: lreason.dupe(),
                                upper: reason_struct.dupe(),
                            }),
                            Arc::new(use_op.dupe()),
                        ),
                        false,
                        lreason,
                        reason_struct,
                        &PropRef::Computed(ukey.dupe()),
                        &PropertyType::OrdinaryField {
                            type_: lkey.dupe(),
                            polarity: *lpolarity,
                        },
                        &PropertyType::OrdinaryField {
                            type_: ukey.dupe(),
                            polarity: *upolarity,
                        },
                    )?;
                    subtyping_kit::rec_flow_p(
                        cx,
                        Some(trace),
                        UseOp::Frame(
                            Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                                PropertyCompatibilityData {
                                    prop: None,
                                    lower: lreason.dupe(),
                                    upper: reason_struct.dupe(),
                                },
                            ))),
                            Arc::new(use_op.dupe()),
                        ),
                        true,
                        lreason,
                        reason_struct,
                        &PropRef::Computed(uvalue.dupe()),
                        &PropertyType::OrdinaryField {
                            type_: lvalue.dupe(),
                            polarity: *lpolarity,
                        },
                        &PropertyType::OrdinaryField {
                            type_: uvalue.dupe(),
                            polarity: *upolarity,
                        },
                    )?;
                }
            }
            _ => {}
        }
    }
    for (name, p) in own_props.iter() {
        match p.deref() {
            PropertyInner::Field(fd) if matches!(fd.type_.deref(), TypeInner::OptionalT { .. }) => {
                let t = &fd.type_;
                let reason = reason_struct.dupe().update_desc(|desc| {
                    VirtualReasonDesc::ROptional(Arc::new(VirtualReasonDesc::RPropertyOf(
                        name.dupe(),
                        Arc::new(desc),
                    )))
                });
                let propref = mk_named_prop(reason.dupe(), false, name.dupe());
                let polarity = if lit { Polarity::Positive } else { fd.polarity };
                let nonstrict_returning = inst_dict.as_ref().map(|d| (d.value.dupe(), t.dupe()));
                rec_flow(
                    cx,
                    trace,
                    (
                        lower,
                        &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                            reason: reason_struct.dupe(),
                            lookup_kind: Box::new(LookupKind::NonstrictReturning(Box::new(
                                NonstrictReturningData(nonstrict_returning, None),
                            ))),
                            try_ts_on_failure: vec![].into(),
                            propref: Box::new(propref),
                            lookup_action: Box::new(LookupAction::LookupPropForSubtyping(
                                Box::new(LookupPropForSubtypingData {
                                    use_op: use_op.dupe(),
                                    prop: PropertyType::OrdinaryField {
                                        type_: t.dupe(),
                                        polarity,
                                    },
                                    prop_name: name.dupe(),
                                    reason_lower: lreason.dupe(),
                                    reason_upper: reason_struct.dupe(),
                                }),
                            )),
                            method_accessible: true,
                            ids: Some(properties::Set::new()),
                            ignore_dicts: false,
                        }))),
                    ),
                )?;
            }
            _ => {
                let reason = reason_struct.dupe().update_desc(|desc| {
                    VirtualReasonDesc::RPropertyOf(name.dupe(), Arc::new(desc))
                });
                let propref = mk_named_prop(reason.dupe(), false, name.dupe());
                rec_flow(
                    cx,
                    trace,
                    (
                        lower,
                        &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                            reason: reason_struct.dupe(),
                            lookup_kind: Box::new(LookupKind::Strict(lreason.dupe())),
                            try_ts_on_failure: vec![].into(),
                            propref: Box::new(propref),
                            lookup_action: Box::new(LookupAction::LookupPropForSubtyping(
                                Box::new(LookupPropForSubtypingData {
                                    use_op: use_op.dupe(),
                                    prop: read_only_if_lit(p),
                                    prop_name: name.dupe(),
                                    reason_lower: lreason.dupe(),
                                    reason_upper: reason_struct.dupe(),
                                }),
                            )),
                            method_accessible: true,
                            ids: Some(properties::Set::new()),
                            ignore_dicts: false,
                        }))),
                    ),
                )?;
            }
        }
    }
    for (name, p) in proto_props.iter() {
        let reason = reason_struct
            .dupe()
            .update_desc(|desc| VirtualReasonDesc::RPropertyOf(name.dupe(), Arc::new(desc)));
        let propref = mk_named_prop(reason.dupe(), false, name.dupe());
        rec_flow(
            cx,
            trace,
            (
                lower,
                &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                    reason: reason_struct.dupe(),
                    lookup_kind: Box::new(LookupKind::Strict(lreason.dupe())),
                    try_ts_on_failure: vec![].into(),
                    propref: Box::new(propref),
                    lookup_action: Box::new(LookupAction::LookupPropForSubtyping(Box::new(
                        LookupPropForSubtypingData {
                            use_op: use_op.dupe(),
                            prop: read_only_if_lit(p),
                            prop_name: name.dupe(),
                            reason_lower: lreason.dupe(),
                            reason_upper: reason_struct.dupe(),
                        },
                    ))),
                    method_accessible: true,
                    ids: Some(properties::Set::new()),
                    ignore_dicts: false,
                }))),
            ),
        )?;
    }
    if let Some(ut) = call_t {
        let prop_name = Some(Name::new("$call"));
        match lower.deref() {
            TypeInner::DefT(_, def_t) => {
                let lid = match &**def_t {
                    DefTInner::ObjT(obj) => obj.call_t,
                    DefTInner::InstanceT(inst_t) => inst_t.inst.inst_call_t,
                    _ => None,
                };
                match lid {
                    Some(lid) => {
                        let lt = cx.find_call(lid);
                        rec_flow(
                            cx,
                            trace,
                            (&lt, &UseT::new(UseTInner::UseT(use_op.dupe(), ut))),
                        )?;
                    }
                    None => {
                        let error_message = ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                            EPropNotFoundInSubtypingData {
                                reason_lower: lreason.dupe(),
                                reason_upper: reason_struct.dupe(),
                                prop_name,
                                use_op: use_op.dupe(),
                                suggestion: None,
                            },
                        ));
                        add_output(cx, error_message)?;
                    }
                }
            }
            _ => {
                let error_message = ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                    EPropNotFoundInSubtypingData {
                        reason_lower: lreason.dupe(),
                        reason_upper: reason_struct.dupe(),
                        prop_name,
                        use_op: use_op.dupe(),
                        suggestion: None,
                    },
                ));
                add_output(cx, error_message)?;
            }
        }
    }
    Ok(())
}

pub(super) fn check_super<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    lreason: &Reason,
    ureason: &Reason,
    t: &Type,
    x: &Name,
    p: &Property,
) -> Result<(), FlowJsException> {
    let use_op = UseOp::Frame(
        Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
            PropertyCompatibilityData {
                prop: Some(x.dupe()),
                lower: lreason.dupe(),
                upper: ureason.dupe(),
            },
        ))),
        Arc::new(use_op),
    );
    let reason_prop = lreason
        .dupe()
        .replace_desc(VirtualReasonDesc::RProperty(Some(x.dupe())));
    let action = LookupAction::SuperProp(Box::new((use_op.dupe(), property::property_type(p))));
    let t = if flow_js_utils::is_munged_prop_name(cx, x) {
        // munge names beginning with single _
        Type::new(TypeInner::ObjProtoT(reason_of_t(t).dupe()))
    } else {
        t.dupe()
    };
    let propref = mk_named_prop(reason_prop, false, x.dupe());
    rec_flow(
        cx,
        trace,
        (
            &t,
            &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                reason: lreason.dupe(),
                lookup_kind: Box::new(LookupKind::NonstrictReturning(Box::new(
                    NonstrictReturningData(None, None),
                ))),
                try_ts_on_failure: vec![].into(),
                propref: Box::new(propref),
                lookup_action: Box::new(action),
                ids: Some(properties::Set::new()),
                method_accessible: true,
                ignore_dicts: false,
            }))),
        ),
    )
}
