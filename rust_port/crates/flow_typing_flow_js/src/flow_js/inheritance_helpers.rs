/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::rc::Rc;
use std::sync::Arc;

use flow_typing_errors::error_message::EConstructSignatureMissingInSubtypingData;
use flow_typing_errors::error_message::EPropNotFoundInSubtypingData;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_js_env::FlowJsEnv;
use flow_typing_type::type_::LookupTData;
use flow_typing_type::type_::NonstrictReturningData;
use flow_typing_type::type_::PropertyCompatibilityData;
use flow_typing_type::type_::TypeStrictnessKind;

use super::helpers::*;
use super::*;

// *********************
// * inheritance utils *
// *********************

pub(super) fn flow_type_args<'cx>(
    cx: &Context<'cx>,
    env: &FlowJsEnv,
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
                rec_flow(
                    cx,
                    env,
                    trace,
                    (&t2, &UseT::new(UseTInner::UseT(use_op, t1))),
                )?;
            }
            Polarity::Positive => {
                rec_flow(
                    cx,
                    env,
                    trace,
                    (&t1, &UseT::new(UseTInner::UseT(use_op, t2))),
                )?;
            }
            Polarity::Neutral => {
                rec_unify(
                    cx,
                    env,
                    trace,
                    use_op,
                    UnifyCause::Uncategorized,
                    None,
                    &t1,
                    &t2,
                )?;
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
    strictness_kind: TypeStrictnessKind,
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
        strictness_kind,
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
    env: &FlowJsEnv,
    trace: DepthTrace,
    use_op: UseOp,
    upper_inst_abstract: bool,
    strictness_kind: TypeStrictnessKind,
    lower: &Type,
    reason_struct: &Reason,
    (own_props_id, proto_props_id, call_id, construct_id, inst_dict): (
        properties::Id,
        properties::Id,
        Option<i32>,
        Option<i32>,
        &Option<DictType>,
    ),
) -> Result<(), FlowJsException> {
    match lower.deref() {
        // Object <: Interface subtyping creates an object out of the interface to dispatch to the
        // existing object <: object logic
        TypeInner::DefT(lreason, def_t)
            if let DefTInner::ObjT(l_obj) = def_t.deref()
                && construct_id.is_none() =>
        {
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
                strictness_kind,
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
                    strictness_kind: l_obj.strictness_kind,
                }))),
            ));
            rec_flow_t(cx, env, trace, use_op, (&lower, &o))?;
        }
        _ => {
            inst_structural_subtype(
                cx,
                env,
                trace,
                use_op,
                upper_inst_abstract,
                strictness_kind,
                lower,
                reason_struct,
                (
                    own_props_id,
                    proto_props_id,
                    call_id,
                    construct_id,
                    inst_dict,
                ),
            )?;
        }
    }
    Ok(())
}

pub(super) fn inst_structural_subtype<'cx>(
    cx: &Context<'cx>,
    env: &FlowJsEnv,
    trace: DepthTrace,
    use_op: UseOp,
    upper_inst_abstract: bool,
    strictness_kind: TypeStrictnessKind,
    lower: &Type,
    reason_struct: &Reason,
    (own_props_id, proto_props_id, call_id, construct_id, inst_dict): (
        properties::Id,
        properties::Id,
        Option<i32>,
        Option<i32>,
        &Option<DictType>,
    ),
) -> Result<(), FlowJsException> {
    let lreason = reason_of_t(lower);
    let lit = is_literal_object_reason(lreason);
    let own_props = cx.find_props(own_props_id);
    let proto_props = cx.find_props(proto_props_id);
    let call_t = call_id.map(|id| cx.find_call(id));
    let read_only_if_lit = |p: &Property| -> Property {
        match p.deref() {
            PropertyInner::Field(fd) if lit => {
                Property::new(PropertyInner::Field(Box::new(FieldData {
                    preferred_def_locs: fd.preferred_def_locs.clone(),
                    key_loc: fd.key_loc.dupe(),
                    type_: fd.type_.dupe(),
                    polarity: Polarity::Positive,
                })))
            }
            _ => p.dupe(),
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
                        env,
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
                        env,
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

        if cx.interface_dictionary_typing_fix() {
            let indexer_subtyping = subtyping_kit::PropsToIndexerContext {
                cx,
                env,
                trace,
                use_op: use_op.dupe(),
                lreason: lreason.dupe(),
                ureason: reason_struct.dupe(),
                strictness_kind,
                lit: false,
                lower_upper_subtyping_obj_ts: None,
            };
            let mut lowers = vec![lower.dupe()];
            let mut seen = BTreeSet::new();
            while let Some(lower) = lowers.pop() {
                for lower in helpers::possible_concrete_types_for_inspection(
                    cx,
                    env,
                    reason_of_t(&lower),
                    &lower,
                )? {
                    if let TypeInner::DefT(_, def_t) = lower.deref()
                        && let DefTInner::InstanceT(inst_t) = def_t.deref()
                    {
                        if !seen.insert(inst_t.inst.class_id.dupe()) {
                            continue;
                        }
                        let lower_props = cx.find_props(inst_t.inst.own_props.dupe());
                        let matching_props = lower_props.iter().try_fold(
                            BTreeMap::new(),
                            |mut matching_props, (name, prop)| {
                                let key_type = flow_js_utils::type_of_key_name_with_env(
                                    env,
                                    name.dupe(),
                                    lreason,
                                );
                                if helpers::speculative_subtyping_succeeds(
                                    cx, env, &key_type, &dict.key,
                                )? {
                                    matching_props.insert(name.dupe(), prop.dupe());
                                }
                                Ok::<_, FlowJsException>(matching_props)
                            },
                        )?;
                        let lower_props = properties::PropertiesMap::from_btree_map(matching_props);
                        indexer_subtyping.flow_props_to_indexer(
                            &lower_props,
                            &[&own_props, &proto_props],
                            dict,
                        )?;
                        lowers.push(inst_t.super_.dupe());
                    }
                }
            }
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
                    env,
                    trace,
                    (
                        lower,
                        &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                            reason: reason_struct.dupe(),
                            lookup_kind: Box::new(LookupKind::NonstrictReturning(Box::new(
                                NonstrictReturningData(nonstrict_returning, None),
                            ))),
                            indexer_fallback: None,
                            try_ts_on_failure: vec![].into(),
                            propref: Box::new(propref.clone()),
                            lookup_action: Box::new(LookupAction::LookupPropsForSubtyping(
                                Box::new(LookupPropsForSubtypingData {
                                    use_op: use_op.dupe(),
                                    props: Rc::from([(
                                        propref,
                                        Property::new(PropertyInner::Field(Box::new(FieldData {
                                            preferred_def_locs: fd.preferred_def_locs.clone(),
                                            key_loc: fd.key_loc.dupe(),
                                            type_: t.dupe(),
                                            polarity,
                                        }))),
                                    )]),
                                    strictness_kind,
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
                    env,
                    trace,
                    (
                        lower,
                        &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                            reason: reason_struct.dupe(),
                            lookup_kind: Box::new(LookupKind::Strict(lreason.dupe())),
                            indexer_fallback: None,
                            try_ts_on_failure: vec![].into(),
                            propref: Box::new(propref.clone()),
                            lookup_action: Box::new(LookupAction::LookupPropsForSubtyping(
                                Box::new(LookupPropsForSubtypingData {
                                    use_op: use_op.dupe(),
                                    props: Rc::from([(propref, read_only_if_lit(p))]),
                                    strictness_kind,
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
            env,
            trace,
            (
                lower,
                &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                    reason: reason_struct.dupe(),
                    lookup_kind: Box::new(LookupKind::Strict(lreason.dupe())),
                    indexer_fallback: None,
                    try_ts_on_failure: vec![].into(),
                    propref: Box::new(propref.clone()),
                    lookup_action: Box::new(LookupAction::LookupPropsForSubtyping(Box::new(
                        LookupPropsForSubtypingData {
                            use_op: use_op.dupe(),
                            props: Rc::from([(propref, read_only_if_lit(p))]),
                            strictness_kind,
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
                            env,
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
                        add_output_with_env(cx, env, error_message)?;
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
                add_output_with_env(cx, env, error_message)?;
            }
        }
    }
    if let Some(construct_id) = construct_id {
        let ut = flow_js_utils::read_construct_t(cx, construct_id);
        // Lower has no construct sig where the upper interface expects
        // one. Dedicated error variant rather than [EInvalidConstructor]
        // (which reads as "the user tried to use [new] on a non-class"
        // — confusing when the failure is actually structural subtyping)
        // and rather than reusing [EPropNotFoundInSubtyping] with a
        // synthetic prop name (which can't be made collision-proof).
        let not_a_constructor = |cx: &Context<'cx>| -> Result<(), FlowJsException> {
            let error_message = ErrorMessage::EConstructSignatureMissingInSubtyping(Box::new(
                EConstructSignatureMissingInSubtypingData {
                    reason_lower: lreason.dupe(),
                    reason_upper: reason_struct.dupe(),
                    use_op: use_op.dupe(),
                },
            ));
            add_output_with_env(cx, env, error_message)
        };
        // Diamond inheritance (e.g. [B extends X], [C extends X],
        // [D extends B, C]) collects [X]'s sig multiple times via
        // [collect_construct_ts] — overload resolution picks the first
        // match either way, so duplicates are harmless at type-check time.
        // Detect abstract-vs-non-abstract assignment: lower carries an
        // abstract bit (either an abstract class via [ClassT] or an
        // [abstract new () => T] interface) and upper does not.
        let concretize = |t: &Type| -> Result<Vec<Type>, FlowJsException> {
            helpers::possible_concrete_types_for_inspection(cx, env, reason_of_t(t), t)
        };
        if !upper_inst_abstract && flow_js_utils::is_class_abstract(&concretize, lower)? {
            add_output_with_env(
                cx, env,
                ErrorMessage::EAbstractClass(Box::new(
                    flow_typing_errors::error_message::EAbstractClassData {
                        kind: flow_typing_errors::intermediate_error_types::AbstractErrorKind::AbstractConstructorAssignedToNonAbstract,
                        loc: reason_struct.loc().dupe(),
                    },
                )),
            )?;
        }
        for lower in concretize(lower)? {
            match lower.deref() {
                // Interface: own + inherited via [super], in derived-first
                // order.
                TypeInner::DefT(_, def_t) if let DefTInner::InstanceT(_) = def_t.deref() => {
                    match flow_js_utils::combine_construct_ts(flow_js_utils::collect_construct_ts(
                        &concretize,
                        cx,
                        &lower,
                    )?) {
                        Some(lt) => rec_flow(
                            cx,
                            env,
                            trace,
                            (&lt, &UseT::new(UseTInner::UseT(use_op.dupe(), ut.dupe()))),
                        )?,
                        None => not_a_constructor(cx)?,
                    }
                }
                TypeInner::DefT(_, def_t) if let DefTInner::ClassT(this) = def_t.deref() => {
                    match flow_js_utils::extract_class_ctor_t(&concretize, cx, this)? {
                        Some(lt) => rec_flow(
                            cx,
                            env,
                            trace,
                            (&lt, &UseT::new(UseTInner::UseT(use_op.dupe(), ut.dupe()))),
                        )?,
                        None => not_a_constructor(cx)?,
                    }
                }
                TypeInner::DefT(_, def_t)
                    if matches!(def_t.deref(), DefTInner::ObjT(_) | DefTInner::FunT(_, _)) =>
                {
                    not_a_constructor(cx)?;
                }
                // Other lowers (AnyT, NullT, ObjProtoT, FunProtoT, etc.) —
                // silently skip; other rules will produce errors as
                // needed.
                _ => {}
            }
        }
    }
    Ok(())
}

pub(super) fn check_super<'cx>(
    cx: &Context<'cx>,
    env: &FlowJsEnv,
    trace: DepthTrace,
    use_op: UseOp,
    lreason: &Reason,
    ureason: &Reason,
    t: &Type,
    x: &Name,
    p: &Property,
    strictness_kind: TypeStrictnessKind,
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
    let action = LookupAction::SuperProp(Box::new((use_op.dupe(), p.dupe(), strictness_kind)));
    let t = if flow_js_utils::is_munged_prop_name(cx, x) {
        // munge names beginning with single _
        Type::new(TypeInner::ObjProtoT(reason_of_t(t).dupe()))
    } else {
        t.dupe()
    };
    let propref = mk_named_prop(reason_prop, false, x.dupe());
    rec_flow(
        cx,
        env,
        trace,
        (
            &t,
            &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                reason: lreason.dupe(),
                lookup_kind: Box::new(LookupKind::NonstrictReturning(Box::new(
                    NonstrictReturningData(None, None),
                ))),
                indexer_fallback: None,
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
