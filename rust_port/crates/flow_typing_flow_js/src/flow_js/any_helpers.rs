/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_typing_type::type_::GenericTData;

use super::helpers::*;
use super::*;

/// "Expands" any to match the form of a type. Allows us to reuse our propagation rules for any
/// cases. Note that it is not always safe to do this (ie in the case of unions).
/// Note: we can get away with a shallow (i.e. non-recursive) expansion here because the flow between
/// the any-expanded type and the original will handle the any-propagation to any relevant positions,
/// some of which may invoke this function when they hit the any propagation functions in the
/// recusive call to __flow.
pub(super) fn expand_any<'cx>(_cx: &Context<'cx>, any: &Type, t: &Type) -> Type {
    let only_any = |_: &Type| any.dupe();
    //   match t with
    match t.deref() {
        TypeInner::DefT(r, def_t) => match def_t.deref() {
            DefTInner::ArrT(arr) => match arr.deref() {
                ArrType::ArrayAT { .. } => {
                    return Type::new(TypeInner::DefT(
                        r.dupe(),
                        DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT {
                            elem_t: any.dupe(),
                            tuple_view: None,
                            react_dro: None,
                        }))),
                    ));
                }
                ArrType::TupleAT {
                    elements,
                    arity,
                    inexact,
                    react_dro,
                    ..
                } => {
                    let new_elements = elements
                        .iter()
                        .map(|te| {
                            //       TupleElement { name; t = only_any t; polarity; optional; reason }
                            TupleElement {
                                name: te.name.dupe(),
                                t: only_any(&te.t),
                                polarity: te.polarity,
                                optional: te.optional,
                                reason: te.reason.dupe(),
                            }
                        })
                        .collect();
                    return Type::new(TypeInner::DefT(
                        r.dupe(),
                        DefT::new(DefTInner::ArrT(Rc::new(ArrType::TupleAT {
                            react_dro: react_dro.clone(),
                            elem_t: any.dupe(),
                            elements: new_elements,
                            arity: arity.clone(),
                            inexact: *inexact,
                        }))),
                    ));
                }
                _ => {}
            },
            _ => {}
        },
        TypeInner::NominalT {
            reason: r,
            nominal_type,
        } => {
            let underlying_t = match &nominal_type.underlying_t {
                nominal::UnderlyingT::FullyOpaque => nominal::UnderlyingT::FullyOpaque,
                nominal::UnderlyingT::CustomError {
                    t: inner_t,
                    custom_error_loc,
                } => nominal::UnderlyingT::CustomError {
                    t: only_any(inner_t),
                    custom_error_loc: custom_error_loc.dupe(),
                },
                nominal::UnderlyingT::OpaqueWithLocal { t: inner_t } => {
                    nominal::UnderlyingT::OpaqueWithLocal {
                        t: only_any(inner_t),
                    }
                }
            };
            let new_nominal_type = NominalTypeInner {
                nominal_id: nominal_type.nominal_id.clone(),
                underlying_t,
                lower_t: nominal_type.lower_t.as_ref().map(only_any),
                upper_t: nominal_type.upper_t.as_ref().map(only_any),
                nominal_type_args: nominal_type
                    .nominal_type_args
                    .iter()
                    .map(|(str, r_prime, _, polarity)| {
                        (str.dupe(), r_prime.dupe(), any.dupe(), *polarity)
                    })
                    .collect(),
            };
            return Type::new(TypeInner::NominalT {
                reason: r.dupe(),
                nominal_type: Rc::new(NominalType::new(new_nominal_type)),
            });
        }
        _ => {}
    }
    // Just returning any would result in infinite recursion in most cases
    panic!("no any expansion defined for this case")
}

pub(super) fn any_prop_to_function(
    use_op: &UseOp,
    funtype: &FunType,
    covariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    contravariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
) -> Result<(), FlowJsException> {
    for FunParam(_, param_t) in funtype.params.iter() {
        contravariant_flow(use_op, param_t)?;
    }
    if let Some(FunRestParam(_, _, rest_t)) = &funtype.rest_param {
        contravariant_flow(use_op, rest_t)?;
    }
    contravariant_flow(use_op, &funtype.this_t.0)?;
    if let Some(tg) = &funtype.type_guard {
        covariant_flow(use_op, &tg.type_guard)?;
    }
    covariant_flow(use_op, &funtype.return_t)
}

pub(super) fn invariant_any_propagation_flow<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    any: &Type,
    t: &Type,
) -> Result<(), FlowJsException> {
    rec_unify(cx, trace, use_op, UnifyCause::Uncategorized, None, any, t)
}

pub(super) fn any_prop_call_prop<'cx>(
    cx: &Context<'cx>,
    use_op: &UseOp,
    covariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    call_t: &Option<i32>,
) -> Result<(), FlowJsException> {
    match call_t {
        None => {}
        Some(id) => {
            let call_type = cx.find_call(*id);
            covariant_flow(use_op, &call_type)?;
        }
    }
    Ok(())
}

pub(super) fn any_prop_properties<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: &UseOp,
    covariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    contravariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    any: &Type,
    properties: &flow_typing_type::type_::properties::PropertiesMap,
) -> Result<(), FlowJsException> {
    for (_, prop) in properties.iter() {
        let polarity = property::polarity(prop);
        let mut err: Result<(), FlowJsException> = Ok(());
        property::iter_t(
            |t| {
                if err.is_err() {
                    return;
                }
                err = match polarity {
                    Polarity::Positive => covariant_flow(use_op, t),
                    Polarity::Negative => contravariant_flow(use_op, t),
                    Polarity::Neutral => {
                        invariant_any_propagation_flow(cx, trace, use_op.dupe(), any, t)
                    }
                };
            },
            prop,
        );
        err?;
    }
    Ok(())
}

pub(super) fn any_prop_obj<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: &UseOp,
    covariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    contravariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    any: &Type,
    obj: &ObjType,
) -> Result<(), FlowJsException> {
    // NOTE: Doing this always would be correct and desirable, but the
    // performance of doing this always is just not good enough. Instead,
    // we do it only in implicit instantiation to ensure that we do not get
    // spurious underconstrained errors when objects contain type arguments
    // that get any as a lower bound
    if cx.in_implicit_instantiation() {
        for (t, p) in obj.reachable_targs.iter() {
            match p {
                Polarity::Positive => covariant_flow(use_op, t)?,
                Polarity::Negative => contravariant_flow(use_op, t)?,
                Polarity::Neutral => {
                    invariant_any_propagation_flow(cx, trace, use_op.dupe(), any, t)?
                }
            }
        }
    }
    Ok(())
}

// FullyResolved tvars cannot contain non-FullyResolved parts, so there's no need to
// deeply traverse them!
pub(super) fn any_prop_tvar<'cx>(cx: &Context<'cx>, tvar: i32) -> bool {
    let (_, constraints) = cx.find_constraints(tvar);
    matches!(constraints, constraint::Constraints::FullyResolved(_))
}

pub(super) fn any_prop_to_type_args<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: &UseOp,
    any: &Type,
    covariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    contravariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    targs: &[(SubstName, Reason, Type, Polarity)],
) -> Result<(), FlowJsException> {
    for (_, _, t, polarity) in targs {
        match polarity {
            Polarity::Positive => covariant_flow(use_op, t)?,
            Polarity::Negative => contravariant_flow(use_op, t)?,
            Polarity::Neutral => invariant_any_propagation_flow(cx, trace, use_op.dupe(), any, t)?,
        }
    }
    Ok(())
}

// TODO: Proper InstanceT propagation has non-termation issues that requires some
// deep investigation. Punting on it for now. Note that using the type_args polarity
// will likely be stricter than necessary. In practice, most type params do not
// have variance sigils even if they are only used co/contravariantly.
// Inline interfaces are an exception to this rule. The type_args there can be
// empty even if the interface contains type arguments because they would only
// appear in type_args if they are bound at the interface itself. We handle those
// in the more general way, since they are used so rarely that non-termination is not
// an issue (for now!)
pub(super) fn any_prop_inst<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: &UseOp,
    any: &Type,
    covariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    contravariant_flow: &dyn Fn(&UseOp, &Type) -> Result<(), FlowJsException>,
    static_: &Type,
    super_: &Type,
    implements: &[Type],
    inst: &InstType,
) -> Result<(), FlowJsException> {
    let InstTypeInner {
        inst_react_dro: _,
        class_id: _,
        class_name: _,
        ref type_args,
        ref own_props,
        ref proto_props,
        ref inst_call_t,
        initialized_fields: _,
        initialized_static_fields: _,
        ref inst_kind,
        inst_dict: _,
        class_private_fields: _,
        class_private_methods: _,
        class_private_static_fields: _,
        class_private_static_methods: _,
    } = **inst;

    any_prop_to_type_args(
        cx,
        trace,
        use_op,
        any,
        covariant_flow,
        contravariant_flow,
        type_args,
    )?;
    match inst_kind {
        InstanceKind::InterfaceKind { inline: true } => {
            covariant_flow(use_op, static_)?;
            covariant_flow(use_op, super_)?;
            for impl_t in implements.iter() {
                covariant_flow(use_op, impl_t)?;
            }
            any_prop_properties(
                cx,
                trace,
                use_op,
                covariant_flow,
                contravariant_flow,
                any,
                &cx.find_props(own_props.dupe()),
            )?;
            any_prop_properties(
                cx,
                trace,
                use_op,
                covariant_flow,
                contravariant_flow,
                any,
                &cx.find_props(proto_props.dupe()),
            )?;
            any_prop_call_prop(cx, use_op, covariant_flow, inst_call_t)
        }
        _ => Ok(()),
    }
}

// types trapped for any propagation. Returns true if this function handles the any case, either
// by propagating or by doing the trivial case. False if the usetype needs to be handled
// separately.
pub(super) fn any_propagated<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    any: &Type,
    u: &UseT<Context<'cx>>,
) -> Result<bool, FlowJsException> {
    let covariant_flow = |use_op: &UseOp, t: &Type| -> Result<(), FlowJsException> {
        rec_flow_t(cx, trace, use_op.dupe(), (any, t))
    };
    let contravariant_flow = |use_op: &UseOp, t: &Type| -> Result<(), FlowJsException> {
        rec_flow_t(cx, trace, use_op.dupe(), (t, any))
    };

    match u.deref() {
        UseTInner::ExitRendersT {
            renders_reason: _,
            u: inner_u,
        } => any_propagated(cx, trace, any, inner_u),
        UseTInner::UseT(use_op, t) => match t.deref() {
            TypeInner::DefT(_, def_t) => match def_t.deref() {
                DefTInner::ArrT(arr_t) => {
                    // read-only arrays are covariant
                    if let ArrType::ROArrayAT(elem_t, _) = arr_t.deref() {
                        covariant_flow(use_op, elem_t)?;
                        Ok(true)
                    } else {
                        // Some types just need to be expanded and filled with any types
                        rec_flow_t(cx, trace, use_op.dupe(), (&expand_any(cx, any, t), t))?;
                        Ok(true)
                    }
                }
                // mk_instance ~for_type:false
                DefTInner::ClassT(class_t) => {
                    covariant_flow(use_op, class_t)?;
                    Ok(true)
                }
                DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                    config,
                    renders,
                    component_kind: _,
                }) => {
                    contravariant_flow(use_op, config)?;
                    covariant_flow(use_op, renders)?;
                    Ok(true)
                }
                DefTInner::RendersT(renders_form) => match renders_form.deref() {
                    CanonicalRendersForm::StructuralRenders {
                        renders_structural_type,
                        renders_variant: _,
                    } => {
                        covariant_flow(use_op, renders_structural_type)?;
                        Ok(true)
                    }
                    CanonicalRendersForm::IntrinsicRenders(_)
                    | CanonicalRendersForm::NominalRenders { .. }
                    | CanonicalRendersForm::DefaultRenders => Ok(false),
                },
                DefTInner::FunT(_, funtype) => {
                    any_prop_to_function(use_op, funtype, &covariant_flow, &contravariant_flow)?;
                    Ok(true)
                }
                DefTInner::ObjT(obj) => {
                    any_prop_obj(
                        cx,
                        trace,
                        use_op,
                        &covariant_flow,
                        &contravariant_flow,
                        any,
                        obj,
                    )?;
                    Ok(true)
                }
                DefTInner::InstanceT(instance_t) => {
                    any_prop_inst(
                        cx,
                        trace,
                        use_op,
                        any,
                        &covariant_flow,
                        &contravariant_flow,
                        &instance_t.static_,
                        &instance_t.super_,
                        &instance_t.implements,
                        &instance_t.inst,
                    )?;
                    Ok(true)
                }
                DefTInner::TypeT(..) => Ok(false),
                // TODO: Punt on these for now, but figure out whether these should fall through or not
                _ => Ok(true),
            },
            // Some types just need to be expanded and filled with any types
            TypeInner::NominalT { .. } => {
                rec_flow_t(cx, trace, use_op.dupe(), (&expand_any(cx, any, t), t))?;
                Ok(true)
            }
            TypeInner::OpenT(tvar) => Ok(any_prop_tvar(cx, tvar.id() as i32)),
            // AnnotTs are 0->1, so there's no need to propagate any inside them
            TypeInner::AnnotT(..) => Ok(true),
            // used to filter maybe
            TypeInner::MaybeT(..) => Ok(false),
            // used to filter optional
            TypeInner::OptionalT { .. } => Ok(false),
            // Handled in __flow
            TypeInner::ThisTypeAppT(..) | TypeInner::TypeAppT(..) => Ok(false),
            // Ideally, any would pollute every member of the union. However, it should be safe to only
            // taint the type in the branch that flow picks when generating constraints for this, so
            // this can be handled by the pre-existing rules
            TypeInner::UnionT(..) => Ok(false),
            // Already handled in the wildcard case in __flow
            TypeInner::IntersectionT(..) => Ok(false),
            // Any won't interact with the type inside KeysT, so it can't be tainted
            TypeInner::KeysT(..) => Ok(true),
            // TODO: Punt on these for now, but figure out whether these should fall through or not
            _ => Ok(true),
        },
        UseTInner::ArrRestT(..)
        | UseTInner::BindT(..)
        | UseTInner::CallT(..)
        | UseTInner::CallElemT(..)
        | UseTInner::CondT(..)
        | UseTInner::ConstructorT(..)
        | UseTInner::ElemT(..)
        | UseTInner::ExtendsUseT(..)
        | UseTInner::ConditionalT(..)
        | UseTInner::GetElemT(..)
        | UseTInner::GetEnumT(..)
        | UseTInner::GetKeysT(..)
        | UseTInner::GetPrivatePropT(..)
        | UseTInner::GetPropT(..)
        | UseTInner::GetTypeFromNamespaceT(..)
        | UseTInner::GetProtoT(..)
        | UseTInner::GetStaticsT(..)
        | UseTInner::GetValuesT(..)
        | UseTInner::GetDictValuesT(..)
        | UseTInner::FilterOptionalT(..)
        | UseTInner::FilterMaybeT(..)
        | UseTInner::DeepReadOnlyT(..)
        | UseTInner::HooklikeT(..)
        | UseTInner::ConcretizeT(..)
        | UseTInner::ResolveUnionT(..)
        | UseTInner::LookupT(..)
        | UseTInner::MapTypeT(..)
        | UseTInner::MethodT(..)
        | UseTInner::MixinT(..)
        | UseTInner::ObjKitT(..)
        | UseTInner::ObjRestT(..)
        | UseTInner::ObjTestProtoT(..)
        | UseTInner::ObjTestT(..)
        | UseTInner::OptionalIndexedAccessT(..)
        | UseTInner::PrivateMethodT(..)
        | UseTInner::ReactKitT(..)
        | UseTInner::ReposLowerT { .. }
        | UseTInner::ReposUseT(..)
        | UseTInner::ResolveSpreadT(..)
        | UseTInner::SealGenericT(..)
        | UseTInner::SetElemT(..)
        | UseTInner::SetPropT(..)
        | UseTInner::SpecializeT(..)
        | UseTInner::TestPropT(..)
        | UseTInner::ThisSpecializeT(..)
        | UseTInner::ToStringT { .. }
        | UseTInner::ValueToTypeReferenceT(..)
        | UseTInner::EvalTypeDestructorT(..)
        | UseTInner::ConvertEmptyPropsToMixedT(..)
        | UseTInner::CheckUnusedPromiseT { .. } => Ok(false),

        // These types have no t_out, so can't propagate anything.
        // Thus we short-circuit by returning true
        UseTInner::HasOwnPropT(..)
        | UseTInner::ImplementsT(..)
        | UseTInner::SetPrivatePropT(..)
        | UseTInner::SetProtoT(..)
        | UseTInner::SuperT(..)
        | UseTInner::ConcretizeTypeAppsT(..) => Ok(true),
    }
}

/// Propagates any flows in case of contravariant/invariant subtypes: the any must pollute
/// all types in contravariant positions when t <: any.
pub(super) fn any_propagated_use<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: &UseOp,
    any: &Type,
    l: &Type,
) -> Result<bool, FlowJsException> {
    let covariant_flow = |use_op: &UseOp, t: &Type| -> Result<(), FlowJsException> {
        rec_flow_t(cx, trace, use_op.dupe(), (t, any))
    };
    let contravariant_flow = |use_op: &UseOp, t: &Type| -> Result<(), FlowJsException> {
        rec_flow_t(cx, trace, use_op.dupe(), (any, t))
    };

    match l.deref() {
        TypeInner::DefT(_, def_t) => match def_t.deref() {
            DefTInner::FunT(_, funtype) => {
                // function types are contravariant in the arguments
                any_prop_to_function(use_op, funtype, &covariant_flow, &contravariant_flow)?;
                Ok(true)
            }

            DefTInner::ArrT(arr_t) if let ArrType::ROArrayAT(elem_t, _) = arr_t.deref() => {
                covariant_flow(use_op, elem_t)?;
                Ok(true)
            }
            DefTInner::ArrT(_) => {
                rec_flow_t(cx, trace, use_op.dupe(), (l, &expand_any(cx, any, l)))?;
                Ok(true)
            }
            DefTInner::ClassT(class_t) => {
                covariant_flow(use_op, class_t)?;
                Ok(true)
            }
            DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                config,
                renders,
                component_kind: _,
            }) => {
                contravariant_flow(use_op, config)?;
                covariant_flow(use_op, renders)?;
                Ok(true)
            }
            DefTInner::ObjT(obj) => {
                any_prop_obj(
                    cx,
                    trace,
                    use_op,
                    &covariant_flow,
                    &contravariant_flow,
                    any,
                    obj,
                )?;
                Ok(true)
            }
            DefTInner::InstanceT(instance_t) => {
                any_prop_inst(
                    cx,
                    trace,
                    use_op,
                    any,
                    &covariant_flow,
                    &contravariant_flow,
                    &instance_t.static_,
                    &instance_t.super_,
                    &instance_t.implements,
                    &instance_t.inst,
                )?;
                Ok(true)
            }
            // Handled already in __flow
            DefTInner::PolyT(_) => Ok(false),
            // Should never occur as the lower bound of any
            _ => Ok(true),
        },

        TypeInner::NominalT {
            reason: _,
            nominal_type,
        } if nominal_type.nominal_id == nominal::Id::InternalEnforceUnionOptimized => Ok(false),
        // Some types just need to be expanded and filled with any types
        TypeInner::NominalT { .. } => {
            // rec_flow_t cx trace ~use_op (t, expand_any cx any t);
            rec_flow_t(cx, trace, use_op.dupe(), (l, &expand_any(cx, any, l)))?;
            // true
            Ok(true)
        }
        // Keys cannot be tainted by any
        TypeInner::KeysT(..) => Ok(true),
        TypeInner::GenericT(box GenericTData { bound, .. }) => {
            covariant_flow(use_op, bound)?;
            Ok(true)
        }
        // These types have no negative positions in their lower bounds
        TypeInner::FunProtoBindT(_)
        | TypeInner::FunProtoT(_)
        | TypeInner::ObjProtoT(_)
        | TypeInner::NullProtoT(_) => Ok(true),
        // AnnotTs are 0->1, so there's no need to propagate any inside them
        TypeInner::AnnotT(..) => Ok(true),
        TypeInner::OpenT(tvar) => Ok(any_prop_tvar(cx, tvar.id() as i32)),
        // Handled already in __flow
        TypeInner::ThisInstanceT(..)
        | TypeInner::EvalT { .. }
        | TypeInner::OptionalT { .. }
        | TypeInner::MaybeT(..)
        | TypeInner::TypeAppT(..)
        | TypeInner::UnionT(..)
        | TypeInner::IntersectionT(..)
        | TypeInner::ThisTypeAppT(..) => Ok(false),
        // Should never occur as the lower bound of any
        TypeInner::NamespaceT(_) => Ok(false),
        TypeInner::StrUtilT { .. } | TypeInner::AnyT(..) => Ok(true),
    }
}
