/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashSet;
use std::ops::Deref;

use dupe::Dupe;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::subst_name::SubstName;
use flow_typing_context::Context;
use flow_typing_errors::error_message::EIncompatibleData;
use flow_typing_errors::error_message::EPolarityMismatchData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_errors::error_message::UpperKind;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_type::type_;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::Destructor;
use flow_typing_type::type_::DictType;
use flow_typing_type::type_::FunType;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::InstTypeInner;
use flow_typing_type::type_::NamespaceType;
use flow_typing_type::type_::ObjKind;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::Property;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::ReactAbstractComponentTData;
use flow_typing_type::type_::ThisTypeAppTData;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::TypeParamInner;
use flow_typing_type::type_::eval;
use flow_typing_type::type_util::reason_of_t;

use crate::type_operation_utils;

pub fn check_polarity<'cx>(
    cx: &Context<'cx>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    t: &Type,
) -> Result<(), FlowJsException> {
    check_polarity_impl(
        cx,
        Some(DepthTrace::unit_trace()),
        &mut HashSet::new(),
        tparams,
        polarity,
        t,
    )
}

// TODO: flesh this out
// [seen] is the set of visited EvalT ids
fn check_polarity_impl<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    seen: &mut HashSet<eval::Id>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    t: &Type,
) -> Result<(), FlowJsException> {
    match t.deref() {
        // base case
        TypeInner::GenericT(box GenericTData {
            reason,
            name,
            bound,
            ..
        }) => match tparams.get(name) {
            None => check_polarity_impl(cx, trace, seen, tparams, polarity, bound)?,
            Some(tp) => {
                if !Polarity::compat(tp.polarity, polarity) {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPolarityMismatch(Box::new(EPolarityMismatchData {
                            reason: reason.dupe(),
                            name: name.string_of_subst_name().dupe(),
                            expected_polarity: tp.polarity,
                            actual_polarity: polarity,
                        })),
                    )?;
                }
            }
        },
        // No need to walk into tvars
        TypeInner::OpenT(..) => {}
        // The annot will resolve to some type, but it doesn't matter because that
        // type will certainly not contain a GenericT.
        TypeInner::AnnotT(..) => {}
        TypeInner::AnyT(..) => {}
        TypeInner::FunProtoBindT(_)
        | TypeInner::FunProtoT(_)
        | TypeInner::NullProtoT(_)
        | TypeInner::ObjProtoT(_) => {}
        TypeInner::OptionalT { type_, .. } | TypeInner::MaybeT(_, type_) => {
            check_polarity_impl(cx, trace, seen, tparams, polarity, type_)?;
        }
        TypeInner::StrUtilT { remainder, .. } => {
            if let Some(inner_t) = remainder {
                check_polarity_impl(cx, trace, seen, tparams, polarity, inner_t)?;
            }
        }
        TypeInner::NamespaceT(ns) => {
            let NamespaceType {
                namespace_symbol: _,
                values_type,
                types_tmap,
            } = ns.deref();
            check_polarity_impl(cx, trace, seen, tparams, polarity, values_type)?;
            check_polarity_propmap(cx, trace, false, seen, tparams, polarity, types_tmap.dupe())?;
        }
        TypeInner::UnionT(_, rep) => {
            for member in rep.members_iter() {
                check_polarity_impl(cx, trace, seen, tparams, polarity, member)?;
            }
        }
        TypeInner::IntersectionT(_, rep) => {
            for member in rep.members_iter() {
                check_polarity_impl(cx, trace, seen, tparams, polarity, member)?;
            }
        }
        TypeInner::ThisTypeAppT(box ThisTypeAppTData { targs: None, .. }) => {
            // Perhaps surprisingly, there is nothing to do here. This type is used
            // specifically for the extends clause of a class declaration. The root
            // type of the extended class is looked up from the environment, and will
            // not contain any type parameters in scope -- only concrete types.
        }
        TypeInner::ThisTypeAppT(box ThisTypeAppTData {
            type_,
            targs: Some(targs),
            ..
        })
        | TypeInner::TypeAppT(box TypeAppTData { type_, targs, .. }) => {
            // Type arguments in a typeapp might contain a GenericT, but the root type
            // which defines the type parameters is not necessarily resolved at this
            // point. We need to know the polarity of the type parameters in order to
            // know the position of any found GenericTs. This call will continue
            // checking the type args once the root type is resolved.
            type_operation_utils::distribute_union_intersection::distribute(
                cx,
                None,
                &|cx, reason, t| {
                    Ok(flow_typing_flow_js::flow_js::FlowJs::possible_concrete_types_for_inspection(
                        cx, reason, t,
                    )?)
                },
                &|reason| reason.loc().dupe(),
                &|cx, l| {
                    match l.deref() {
                        TypeInner::AnyT(..) => return Ok(()),
                        TypeInner::DefT(_, d) => {
                            if let DefTInner::PolyT(box PolyTData { tparams: tps, .. }) = d.deref()
                            {
                                return variance_check(cx, trace, tparams, polarity, tps, targs);
                            }
                        }
                        // We will encounter this when walking an extends clause which does
                        // not have explicit type arguments. The class has an implicit this type
                        // parameter which needs to be specialized to the inheriting class, but
                        // that is uninteresting for the variance check machinery.
                        TypeInner::ThisInstanceT(..) if targs.is_empty() => return Ok(()),
                        _ => {}
                    }
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EIncompatible(Box::new(EIncompatibleData {
                            lower: (
                                reason_of_t(l).dupe(),
                                flow_js_utils::error_message_kind_of_lower(t),
                            ),
                            upper: (
                                reason_of_t(type_).dupe(),
                                UpperKind::IncompatibleVarianceCheckT,
                            ),
                            use_op: None,
                        })),
                    )
                },
                type_,
            )?;
        }
        TypeInner::KeysT(_, inner_t) => {
            check_polarity_impl(cx, trace, seen, tparams, Polarity::Positive, inner_t)?;
        }
        //   | EvalT { type_ = t; defer_use_t = TypeDestructorT (use_op, r, ReadOnlyType); id } ->
        TypeInner::EvalT {
            type_: eval_t,
            defer_use_t,
            id,
        } if matches!(
            defer_use_t.deref(),
            flow_typing_type::type_::TypeDestructorTInner(
                _,
                _,
                d,
            ) if matches!(d.deref(), flow_typing_type::type_::Destructor::ReadOnlyType)
        ) =>
        {
            if !seen.contains(id) {
                let type_::TypeDestructorTInner(use_op, r, _d) = defer_use_t.deref();
                let trace_val = trace.unwrap_or_else(DepthTrace::dummy_trace);
                let out = flow_typing_tvar::mk_no_wrap_where_result(
                    cx,
                    r.dupe(),
                    |cx, tvar_reason, tvar_id| {
                        let tvar = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                        flow_typing_flow_js::flow_js::FlowJs::eval_destructor(
                            cx,
                            trace_val,
                            use_op.dupe(),
                            r,
                            eval_t,
                            &Destructor::ReadOnlyType,
                            &tvar,
                        )
                    },
                )?;
                seen.insert(id.dupe());
                let concrete_types =
                    flow_typing_flow_js::flow_js::FlowJs::possible_concrete_types_for_inspection(
                        cx, r, &out,
                    )?;
                for ct in &concrete_types {
                    check_polarity_impl(cx, trace, seen, tparams, Polarity::Positive, ct)?;
                }
            }
        }
        TypeInner::NominalT { nominal_type, .. } => {
            let mut tps_list = Vec::new();
            let mut targs_list = Vec::new();
            for (subst_name, reason, arg_t, pol) in nominal_type.nominal_type_args.iter() {
                tps_list.push(TypeParam::new(TypeParamInner {
                    reason: reason.dupe(),
                    name: subst_name.dupe(),
                    bound: type_::mixed_t::why(reason.dupe()),
                    polarity: *pol,
                    default: None,
                    is_this: false,
                    is_const: false,
                }));
                targs_list.push(arg_t.dupe());
            }
            variance_check(cx, trace, tparams, polarity, &tps_list, &targs_list)?;
        }
        // TODO
        TypeInner::EvalT { .. } => {}
        TypeInner::DefT(_, d) => match d.deref() {
            DefTInner::BoolGeneralT
            | DefTInner::BigIntGeneralT(_)
            | DefTInner::EmptyT
            | DefTInner::EnumObjectT { .. }
            | DefTInner::EnumValueT(_)
            | DefTInner::MixedT(_)
            | DefTInner::NullT
            | DefTInner::NumGeneralT(_)
            | DefTInner::NumericStrKeyT(_)
            | DefTInner::SingletonBoolT { .. }
            | DefTInner::SingletonNumT { .. }
            | DefTInner::SingletonStrT { .. }
            | DefTInner::SingletonBigIntT { .. }
            | DefTInner::StrGeneralT(_)
            | DefTInner::VoidT
            | DefTInner::SymbolT
            | DefTInner::UniqueSymbolT(_) => {}
            DefTInner::ClassT(inner_t) => {
                check_polarity_impl(cx, trace, seen, tparams, polarity, inner_t)?;
            }
            DefTInner::InstanceT(instance_t) => {
                let InstTypeInner {
                    inst_react_dro: _,
                    class_id: _,
                    class_name: _,
                    type_args: _,
                    own_props,
                    proto_props,
                    inst_call_t: call_t,
                    initialized_fields: _,
                    initialized_static_fields: _,
                    inst_kind: _,
                    inst_dict,
                    class_private_fields,
                    class_private_static_fields: _,
                    class_private_methods,
                    class_private_static_methods: _,
                } = instance_t.inst.deref();
                check_polarity_impl(cx, trace, seen, tparams, polarity, &instance_t.static_)?;
                check_polarity_impl(cx, trace, seen, tparams, polarity, &instance_t.super_)?;
                for impl_t in instance_t.implements.iter() {
                    check_polarity_impl(cx, trace, seen, tparams, polarity, impl_t)?;
                }
                check_polarity_propmap(
                    cx,
                    trace,
                    false,
                    seen,
                    tparams,
                    polarity,
                    own_props.dupe(),
                )?;
                check_polarity_propmap(
                    cx,
                    trace,
                    true,
                    seen,
                    tparams,
                    polarity,
                    proto_props.dupe(),
                )?;
                check_polarity_propmap(
                    cx,
                    trace,
                    false,
                    seen,
                    tparams,
                    polarity,
                    class_private_fields.dupe(),
                )?;
                check_polarity_propmap(
                    cx,
                    trace,
                    false,
                    seen,
                    tparams,
                    polarity,
                    class_private_methods.dupe(),
                )?;
                if let Some(call_t) = call_t {
                    check_polarity_call(cx, trace, seen, tparams, polarity, *call_t)?;
                }
                if let Some(inst_dict) = inst_dict {
                    check_polarity_dict(cx, trace, seen, tparams, polarity, inst_dict)?;
                }
            }
            // We can ignore the statics of function annotations, since
            // they will always be "uninteresting," never containing a GenericT.
            DefTInner::FunT(_static, f) => {
                let FunType {
                    // Similarly, we can ignore this types, which can not be explicitly
                    // provided, and thus will not contain a GenericT.
                    this_t: _,
                    params,
                    rest_param,
                    return_t,
                    type_guard,
                    def_reason: _,
                    effect_: _,
                } = f.deref();
                let inv_polarity = Polarity::inv(polarity);
                for param in params.iter() {
                    check_polarity_impl(cx, trace, seen, tparams, inv_polarity, &param.1)?;
                }
                if let Some(rest_param) = rest_param {
                    check_polarity_impl(cx, trace, seen, tparams, inv_polarity, &rest_param.2)?;
                }
                check_polarity_impl(cx, trace, seen, tparams, polarity, return_t)?;
                if let Some(tg) = type_guard {
                    check_polarity_impl(cx, trace, seen, tparams, polarity, &tg.type_guard)?;
                }
            }
            DefTInner::ArrT(arr) => {
                use flow_typing_type::type_::ArrType;
                use flow_typing_type::type_::ArrayATData;
                use flow_typing_type::type_::TupleATData;
                match arr.deref() {
                    // This representation signifies a literal, which is not a type.
                    ArrType::ArrayAT(box ArrayATData {
                        tuple_view: Some(_),
                        ..
                    }) => {
                        panic!("UnexpectedType: ArrayAT with tuple_view in check_polarity");
                    }
                    ArrType::ArrayAT(box ArrayATData {
                        elem_t,
                        tuple_view: None,
                        ..
                    }) => {
                        check_polarity_impl(cx, trace, seen, tparams, Polarity::Neutral, elem_t)?;
                    }
                    ArrType::TupleAT(box TupleATData { elements, .. }) => {
                        for elem in elements.iter() {
                            check_polarity_impl(
                                cx,
                                trace,
                                seen,
                                tparams,
                                Polarity::mult(polarity, elem.polarity),
                                &elem.t,
                            )?;
                        }
                    }
                    ArrType::ROArrayAT(box (inner_t, _)) => {
                        check_polarity_impl(cx, trace, seen, tparams, polarity, inner_t)?;
                    }
                }
            }
            DefTInner::ObjT(obj) => {
                let type_::ObjType {
                    flags,
                    props_tmap,
                    proto_t,
                    call_t,
                    reachable_targs: _,
                } = obj.deref();
                check_polarity_propmap(
                    cx,
                    trace,
                    false,
                    seen,
                    tparams,
                    polarity,
                    props_tmap.dupe(),
                )?;
                if let ObjKind::Indexed(ref dict) = flags.obj_kind {
                    check_polarity_dict(cx, trace, seen, tparams, polarity, dict)?;
                }
                check_polarity_impl(cx, trace, seen, tparams, polarity, proto_t)?;
                if let Some(call_t) = call_t {
                    check_polarity_call(cx, trace, seen, tparams, polarity, *call_t)?;
                }
            }
            DefTInner::PolyT(box PolyTData {
                tparams: tps,
                t_out,
                ..
            }) => {
                // We might encounter a polymorphic function type or method inside of an
                // annotation. A newly introduced type parameter's bound or default might
                // refer to one of the tparams we're looking for.
                let mut tparams_acc = tparams.clone();
                for tp in tps.iter() {
                    check_polarity_typeparam(cx, trace, seen, &tparams_acc, polarity, tp)?;
                    tparams_acc.insert(tp.name.dupe(), tp.clone());
                }
                //     check_polarity cx ?trace seen tparams polarity t
                check_polarity_impl(cx, trace, seen, &tparams_acc, polarity, t_out)?;
            }
            DefTInner::ReactAbstractComponentT(box ReactAbstractComponentTData {
                config,
                renders,
                ..
            }) => {
                check_polarity_impl(cx, trace, seen, tparams, Polarity::inv(polarity), config)?;
                check_polarity_impl(cx, trace, seen, tparams, polarity, renders)?;
            }
            DefTInner::RendersT(renders) => {
                use flow_typing_type::type_::CanonicalRendersForm;
                match renders.deref() {
                    CanonicalRendersForm::NominalRenders { renders_super, .. } => {
                        check_polarity_impl(cx, trace, seen, tparams, polarity, renders_super)?;
                    }
                    CanonicalRendersForm::StructuralRenders {
                        renders_structural_type,
                        ..
                    } => {
                        check_polarity_impl(
                            cx,
                            trace,
                            seen,
                            tparams,
                            polarity,
                            renders_structural_type,
                        )?;
                    }
                    CanonicalRendersForm::IntrinsicRenders(_)
                    | CanonicalRendersForm::DefaultRenders => {}
                }
            }
            // We only expect types which can appear in annotations.
            DefTInner::TypeT(..) => {
                panic!("UnexpectedType: TypeT in check_polarity");
            }
        },
        // We only expect types which can appear in annotations.
        TypeInner::ThisInstanceT(..) => {
            panic!("UnexpectedType: ThisInstanceT in check_polarity");
        }
    }
    Ok(())
}

fn check_polarity_propmap<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    skip_ctor: bool,
    seen: &mut HashSet<eval::Id>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    id: type_::properties::Id,
) -> Result<(), FlowJsException> {
    let pmap = cx.find_props(id);
    for (x, p) in pmap.iter() {
        if skip_ctor && *x == Name::new("constructor") {
        } else {
            check_polarity_prop(cx, trace, seen, tparams, polarity, p)?;
        }
    }
    Ok(())
}

fn check_polarity_prop<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    seen: &mut HashSet<eval::Id>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    prop: &Property,
) -> Result<(), FlowJsException> {
    match prop.deref() {
        PropertyInner::Field(fd) => check_polarity_impl(
            cx,
            trace,
            seen,
            tparams,
            Polarity::mult(polarity, fd.polarity),
            &fd.type_,
        ),
        PropertyInner::Get { type_, .. } => {
            check_polarity_impl(cx, trace, seen, tparams, polarity, type_)
        }
        PropertyInner::Set { type_, .. } => {
            check_polarity_impl(cx, trace, seen, tparams, Polarity::inv(polarity), type_)
        }
        PropertyInner::GetSet(gs) => {
            check_polarity_impl(cx, trace, seen, tparams, polarity, &gs.get_type)?;
            check_polarity_impl(
                cx,
                trace,
                seen,
                tparams,
                Polarity::inv(polarity),
                &gs.set_type,
            )
        }
        PropertyInner::Method { type_, .. } => {
            check_polarity_impl(cx, trace, seen, tparams, polarity, type_)
        }
    }
}

// and check_polarity_dict cx ?trace seen tparams polarity d =
fn check_polarity_dict<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    seen: &mut HashSet<eval::Id>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    d: &DictType,
) -> Result<(), FlowJsException> {
    let DictType {
        dict_name: _,
        key,
        value,
        dict_polarity,
    } = d;
    check_polarity_impl(cx, trace, seen, tparams, Polarity::Neutral, key)?;
    check_polarity_impl(
        cx,
        trace,
        seen,
        tparams,
        Polarity::mult(polarity, *dict_polarity),
        value,
    )?;
    Ok(())
}

fn check_polarity_call<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    seen: &mut HashSet<eval::Id>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    id: i32,
) -> Result<(), FlowJsException> {
    let t = cx.find_call(id);
    check_polarity_impl(cx, trace, seen, tparams, polarity, &t)
}

fn check_polarity_typeparam<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    seen: &mut HashSet<eval::Id>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    tp: &TypeParam,
) -> Result<(), FlowJsException> {
    let TypeParamInner {
        reason: _,
        name: _,
        bound,
        polarity: tp_polarity,
        default,
        is_this: _,
        is_const: _,
    } = tp.deref();
    let mult_polarity = Polarity::mult(polarity, *tp_polarity);
    check_polarity_impl(cx, trace, seen, tparams, mult_polarity, bound)?;
    if let Some(default) = default {
        check_polarity_impl(cx, trace, seen, tparams, mult_polarity, default)?;
    }
    Ok(())
}

fn variance_check<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    tparams: &BTreeMap<SubstName, TypeParam>,
    polarity: Polarity,
    tps: &[TypeParam],
    targs: &[Type],
) -> Result<(), FlowJsException> {
    // ignore typeapp arity mismatch, since it's handled elsewhere
    for (tp, targ) in tps.iter().zip(targs.iter()) {
        check_polarity_impl(
            cx,
            trace,
            &mut HashSet::new(),
            tparams,
            Polarity::mult(polarity, tp.polarity),
            targ,
        )?;
    }
    Ok(())
}
