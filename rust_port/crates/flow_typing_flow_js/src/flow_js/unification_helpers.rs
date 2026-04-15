/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use flow_typing_errors::error_message::EPropNotFoundInSubtypingData;
use flow_typing_errors::error_message::EPropPolarityMismatchData;
use flow_typing_errors::error_message::ETupleArityMismatchData;
use flow_typing_errors::error_message::ETupleElementPolarityMismatchData;
use flow_typing_type::type_::PropertyCompatibilityData;
use flow_typing_type::type_::TypeAppTData;

use super::constraint_helpers::*;
use super::helpers::*;
use super::*;

// Unification of two types

// It is potentially dangerous to unify a type variable to a type that "forgets"
// constraints during propagation. These types are "any-like": the canonical
// example of such a type is any. Overall, we want unification to be a sound
// "optimization," in the sense that replacing bidirectional flows with
// unification should not miss errors. But consider a scenario where we have a
// type variable with two incoming flows, string and any, and two outgoing
// flows, number and any. If we replace the flows from/to any with an
// unification with any, we will miss the string/number incompatibility error.
//
// However, unifying with any-like types is sometimes desirable / intentional.
pub(super) fn ok_unify(unify_any: bool, t: &Type) -> bool {
    match t.deref() {
        TypeInner::AnyT(_, _) => unify_any,
        _ => true,
    }
}

pub(super) fn __unify<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    unify_cause: UnifyCause,
    unify_any: bool,
    t1: &Type,
    t2: &Type,
    trace: DepthTrace,
) -> Result<(), FlowJsException> {
    flow_typing_debug::verbose::print_unify_types_if_verbose(cx, &trace, None, t1, t2);
    // If the type is the same type or we have already seen this type pair in our
    // cache then do not continue.
    if t1 == t2 {
        return Ok(());
    }
    // limit recursion depth
    recursion_check::check(cx, trace)?;

    if !matches!(unify_cause, UnifyCause::Uncategorized)
        && !speculation::speculating(cx)
        && !tvar_visitors::has_unresolved_tvars(cx, t1)
        && !tvar_visitors::has_unresolved_tvars(cx, t2)
    {
        match crate::speculation_kit::try_unify(cx, trace, t1.dupe(), use_op.dupe(), t2.dupe()) {
            Ok(()) => Ok(()),
            Err(FlowJsException::SpeculationSingletonError) => {
                let explanation = {
                    match &unify_cause {
                        UnifyCause::MutableArray {
                            lower_array_t,
                            upper_array_t,
                            upper_array_reason,
                        } => {
                            let lower_array_desc = TypeOrTypeDescT::Type(lower_array_t.dupe());
                            let upper_array_desc = TypeOrTypeDescT::Type(upper_array_t.dupe());
                            let lower_array_loc = reason_of_t(lower_array_t).def_loc().dupe();
                            let upper_array_loc = reason_of_t(upper_array_t).def_loc().dupe();
                            Some(
                                ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableArray {
                                    lower_array_loc,
                                    upper_array_loc,
                                    lower_array_desc,
                                    upper_array_desc,
                                    upper_array_reason: upper_array_reason.dupe(),
                                },
                            )
                        }
                        UnifyCause::MutableProperty {
                            lower_obj_t,
                            upper_obj_t,
                            upper_object_reason,
                            property_name,
                        } => {
                            let lower_obj_desc = TypeOrTypeDescT::Type(lower_obj_t.dupe());
                            let upper_obj_desc = TypeOrTypeDescT::Type(upper_obj_t.dupe());
                            let lower_obj_loc = reason_of_t(lower_obj_t).def_loc().dupe();
                            let upper_obj_loc = reason_of_t(upper_obj_t).def_loc().dupe();
                            Some(
                                ExplanationWithLazyParts::LazyExplanationInvariantSubtypingDueToMutableProperty {
                                    lower_obj_loc,
                                    upper_obj_loc,
                                    lower_obj_desc,
                                    upper_obj_desc,
                                    upper_object_reason: upper_object_reason.dupe(),
                                    property_name: property_name.dupe(),
                                },
                            )
                        }
                        UnifyCause::Uncategorized => None,
                    }
                };
                let lower_desc = TypeOrTypeDescT::Type(t1.dupe());
                let upper_desc = TypeOrTypeDescT::Type(t2.dupe());
                let lower_loc = {
                    match t1.deref() {
                        TypeInner::OpenT(tvar) => {
                            let r = tvar.reason();
                            let id = tvar.id() as i32;
                            match flow_js_utils::merge_tvar_opt(
                                cx,
                                false,
                                union_rep::UnionKind::ResolvedKind,
                                r,
                                id,
                            ) {
                                Some(t) => loc_of_t(&t).dupe(),
                                None => r.loc().dupe(),
                            }
                        }
                        _ => loc_of_t(t1).dupe(),
                    }
                };
                let upper_loc = loc_of_t(t2).dupe();
                add_output(
                    cx,
                    ErrorMessage::EInvariantSubtypingWithUseOp(Box::new(
                        EInvariantSubtypingWithUseOpData {
                            sub_component: None,
                            lower_loc,
                            upper_loc,
                            lower_desc,
                            upper_desc,
                            use_op,
                            explanation,
                        },
                    )),
                )
            }
            Err(other) => Err(other),
        }
    } else {
        __unify_inner(cx, use_op, unify_any, t1, t2, trace)
    }
}

// Should only be called by __unify
fn __unify_inner<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    unify_any: bool,
    t1: &Type,
    t2: &Type,
    trace: DepthTrace,
) -> Result<(), FlowJsException> {
    match (t1.deref(), t2.deref()) {
        (TypeInner::OpenT(tvar1), TypeInner::OpenT(tvar2)) => {
            merge_ids(cx, trace, use_op, tvar1.id() as i32, tvar2.id() as i32)?;
        }
        (TypeInner::OpenT(tvar), _) if ok_unify(unify_any, t2) => {
            resolve_id(cx, trace, use_op, tvar.id() as i32, t2)?;
        }
        (_, TypeInner::OpenT(tvar)) if ok_unify(unify_any, t1) => {
            resolve_id(cx, trace, unify_flip(use_op), tvar.id() as i32, t1)?;
        }
        (TypeInner::DefT(r1, def1), TypeInner::DefT(r2, def2)) => {
            match (def1.deref(), def2.deref()) {
                (
                    DefTInner::PolyT(box PolyTData { id: id1, .. }),
                    DefTInner::PolyT(box PolyTData { id: id2, .. }),
                ) if id1 == id2 => {
                    return Ok(());
                }
                //   | ( DefT (_, ArrT (ArrayAT { elem_t = t1; tuple_view = tv1; react_dro = _ })),
                //       DefT (_, ArrT (ArrayAT { elem_t = t2; tuple_view = tv2; react_dro = _ }))
                //     ) ->
                //   | ( DefT (r1, ArrT (TupleAT { ... })),
                //       DefT (r2, ArrT (TupleAT { ... }))
                //     ) ->
                (DefTInner::ArrT(arr1), DefTInner::ArrT(arr2)) => {
                    match (arr1.deref(), arr2.deref()) {
                        (
                            ArrType::ArrayAT(box ArrayATData {
                                elem_t: elem_t1,
                                tuple_view: tv1,
                                react_dro: _,
                            }),
                            ArrType::ArrayAT(box ArrayATData {
                                elem_t: elem_t2,
                                tuple_view: tv2,
                                react_dro: _,
                            }),
                        ) => {
                            let ts1 = tv1
                                .as_ref()
                                .map(|tv| tuple_ts_of_elements(&tv.elements))
                                .unwrap_or_default();
                            let ts2 = tv2
                                .as_ref()
                                .map(|tv| tuple_ts_of_elements(&tv.elements))
                                .unwrap_or_default();
                            array_unify(cx, trace, use_op, (&ts1, elem_t1, &ts2, elem_t2))?;
                            return Ok(());
                        }
                        (
                            ArrType::TupleAT(box TupleATData {
                                elem_t: _,
                                elements: elements1,
                                arity: lower_arity,
                                inexact: lower_inexact,
                                react_dro: _,
                            }),
                            ArrType::TupleAT(box TupleATData {
                                elem_t: _,
                                elements: elements2,
                                arity: upper_arity,
                                inexact: upper_inexact,
                                react_dro: _,
                            }),
                        ) => {
                            let (num_req1, num_total1) = lower_arity;
                            let (num_req2, num_total2) = upper_arity;
                            if lower_inexact != upper_inexact
                                || num_req1 != num_req2
                                || num_total1 != num_total2
                            {
                                add_output(
                                    cx,
                                    ErrorMessage::ETupleArityMismatch(Box::new(
                                        ETupleArityMismatchData {
                                            use_op: use_op.dupe(),
                                            lower_reason: r1.dupe(),
                                            lower_arity: *lower_arity,
                                            lower_inexact: *lower_inexact,
                                            upper_reason: r2.dupe(),
                                            upper_arity: *upper_arity,
                                            upper_inexact: *upper_inexact,
                                            unify: true,
                                        },
                                    )),
                                )?;
                            }
                            let mut n: i32 = 0;
                            {
                                let mut iter1 = elements1.iter();
                                let mut iter2 = elements2.iter();
                                loop {
                                    let e1 = iter1.next();
                                    let e2 = iter2.next();
                                    match (e1, e2) {
                                        (Some(te1), Some(te2)) => {
                                            let p1 = te1.polarity;
                                            let p2 = te2.polarity;
                                            if !Polarity::equal(p1, p2) {
                                                add_output(
                                                    cx,
                                                    ErrorMessage::ETupleElementPolarityMismatch(
                                                        Box::new(
                                                            ETupleElementPolarityMismatchData {
                                                                index: n,
                                                                reason_lower: r1.dupe(),
                                                                polarity_lower: p1,
                                                                reason_upper: r2.dupe(),
                                                                polarity_upper: p2,
                                                                use_op: use_op.dupe(),
                                                            },
                                                        ),
                                                    ),
                                                )?;
                                            }
                                            rec_unify(
                                                cx,
                                                trace,
                                                use_op.dupe(),
                                                UnifyCause::Uncategorized,
                                                None,
                                                &te1.t,
                                                &te2.t,
                                            )?;
                                            n += 1;
                                        }
                                        (None, None) => break,
                                        _ => break,
                                    }
                                }
                            }
                            return Ok(());
                        }
                        _ => {
                            naive_unify(cx, trace, use_op, t1, t2)?;
                            return Ok(());
                        }
                    }
                }
                (DefTInner::ObjT(l_obj), DefTInner::ObjT(u_obj)) => {
                    let lreason = r1;
                    let ureason = r2;
                    let lflds = l_obj.props_tmap.dupe();
                    let uflds = u_obj.props_tmap.dupe();
                    let lflags = &l_obj.flags;
                    let uflags = &u_obj.flags;

                    if !obj_type::is_exact(&lflags.obj_kind)
                        && !is_literal_object_reason(ureason)
                        && obj_type::is_exact(&uflags.obj_kind)
                    {
                        flow_js_utils::exact_obj_error(
                            cx,
                            &lflags.obj_kind,
                            use_op.dupe(),
                            ureason.dupe(),
                            t1,
                        )?;
                    }
                    if !obj_type::is_exact(&uflags.obj_kind)
                        && !is_literal_object_reason(lreason)
                        && obj_type::is_exact(&lflags.obj_kind)
                    {
                        flow_js_utils::exact_obj_error(
                            cx,
                            &uflags.obj_kind,
                            use_op.dupe(),
                            lreason.dupe(),
                            t2,
                        )?;
                    }
                    // ensure the keys and values are compatible with each other.
                    let ldict = obj_type::get_dict_opt(&lflags.obj_kind);
                    let udict = obj_type::get_dict_opt(&uflags.obj_kind);
                    match (ldict, udict) {
                        (Some(ldict_t), Some(udict_t)) => {
                            rec_unify(
                                cx,
                                trace,
                                UseOp::Frame(
                                    Arc::new(VirtualFrameUseOp::IndexerKeyCompatibility {
                                        lower: lreason.dupe(),
                                        upper: ureason.dupe(),
                                    }),
                                    Arc::new(use_op.dupe()),
                                ),
                                UnifyCause::Uncategorized,
                                None,
                                &ldict_t.key,
                                &udict_t.key,
                            )?;
                            rec_unify(
                                cx,
                                trace,
                                UseOp::Frame(
                                    Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
                                        PropertyCompatibilityData {
                                            prop: None,
                                            lower: lreason.dupe(),
                                            upper: ureason.dupe(),
                                        },
                                    ))),
                                    Arc::new(use_op.dupe()),
                                ),
                                UnifyCause::Uncategorized,
                                None,
                                &ldict_t.value,
                                &udict_t.value,
                            )?;
                        }
                        (Some(_), None) => {
                            add_output(
                                cx,
                                ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                                    EPropNotFoundInSubtypingData {
                                        prop_name: None,
                                        reason_lower: ureason.dupe(),
                                        reason_upper: lreason.dupe(),
                                        use_op: use_op.dupe(),
                                        suggestion: None,
                                    },
                                )),
                            )?;
                        }
                        (None, Some(_)) => {
                            let flipped_use_op = unify_flip(use_op.dupe());
                            add_output(
                                cx,
                                ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                                    EPropNotFoundInSubtypingData {
                                        prop_name: None,
                                        reason_lower: lreason.dupe(),
                                        reason_upper: ureason.dupe(),
                                        use_op: flipped_use_op,
                                        suggestion: None,
                                    },
                                )),
                            )?;
                        }
                        (None, None) => {}
                    }

                    let lpmap = cx.find_props(lflds);
                    let upmap = cx.find_props(uflds);
                    {
                        let mut all_keys: BTreeSet<Name> = BTreeSet::new();
                        for (k, _) in lpmap.iter() {
                            all_keys.insert(k.dupe());
                        }
                        for (k, _) in upmap.iter() {
                            all_keys.insert(k.dupe());
                        }
                        // OCaml's Map.merge uses concat_or_join whose arguments are
                        // evaluated right-to-left (ocamlopt), processing the right
                        // (larger) subtree before the left (smaller) subtree. During
                        // speculation, the first failing property raises an exception
                        // and exits. To match OCaml's de facto behavior, iterate in
                        // reverse (descending) order.
                        for x in all_keys.into_iter().rev() {
                            if !flow_js_utils::is_dictionary_exempt(&x) {
                                let lp = lpmap.get(&x);
                                let up = upmap.get(&x);
                                match (lp, up) {
                                    (Some(p1), Some(p2)) => {
                                        unify_props(
                                            cx,
                                            trace,
                                            use_op.dupe(),
                                            &x,
                                            lreason,
                                            ureason,
                                            p1,
                                            p2,
                                        )?;
                                    }
                                    (Some(p1), None) => {
                                        unify_prop_with_dict(
                                            cx,
                                            trace,
                                            use_op.dupe(),
                                            &x,
                                            p1,
                                            lreason,
                                            ureason,
                                            udict,
                                        )?;
                                    }
                                    (None, Some(p2)) => {
                                        unify_prop_with_dict(
                                            cx,
                                            trace,
                                            use_op.dupe(),
                                            &x,
                                            p2,
                                            ureason,
                                            lreason,
                                            ldict,
                                        )?;
                                    }
                                    // | (None, None) -> ()
                                    (None, None) => {}
                                }
                            }
                        }
                    }
                    return Ok(());
                }
                (DefTInner::FunT(_, funtype1), DefTInner::FunT(_, funtype2))
                    if funtype1.type_guard.is_none()
                        && funtype2.type_guard.is_none()
                        && funtype1.params.len() == funtype2.params.len() =>
                {
                    let this1 = subtype_this_of_function(funtype1);
                    let this2 = subtype_this_of_function(funtype2);
                    rec_unify(
                        cx,
                        trace,
                        use_op.dupe(),
                        UnifyCause::Uncategorized,
                        None,
                        &this1,
                        &this2,
                    )?;
                    for (FunParam(_, param_t1), FunParam(_, param_t2)) in
                        funtype1.params.iter().zip(funtype2.params.iter())
                    {
                        rec_unify(
                            cx,
                            trace,
                            use_op.dupe(),
                            UnifyCause::Uncategorized,
                            None,
                            param_t1,
                            param_t2,
                        )?;
                    }
                    return rec_unify(
                        cx,
                        trace,
                        use_op,
                        UnifyCause::Uncategorized,
                        None,
                        &funtype1.return_t,
                        &funtype2.return_t,
                    );
                }
                // Fall through to the outer match for remaining DefT cases
                _ => return naive_unify(cx, trace, use_op, t1, t2),
            }
        }
        (
            TypeInner::TypeAppT(box TypeAppTData {
                reason: _,
                use_op: _,
                type_: c1,
                targs: ts1,
                from_value: fv1,
                use_desc: _,
            }),
            TypeInner::TypeAppT(box TypeAppTData {
                reason: _,
                use_op: _,
                type_: c2,
                targs: ts2,
                from_value: fv2,
                use_desc: _,
            }),
        ) if c1 == c2 && ts1.len() == ts2.len() && fv1 == fv2 => {
            for (t1_elem, t2_elem) in ts1.iter().zip(ts2.iter()) {
                rec_unify(
                    cx,
                    trace,
                    use_op.dupe(),
                    UnifyCause::Uncategorized,
                    None,
                    t1_elem,
                    t2_elem,
                )?;
            }
        }
        (TypeInner::AnnotT(_, inner_t1, _), TypeInner::AnnotT(_, inner_t2, _))
            if let (TypeInner::OpenT(tvar1), TypeInner::OpenT(tvar2)) =
                (inner_t1.deref(), inner_t2.deref()) =>
        {
            let id1 = tvar1.id() as i32;
            let id2 = tvar2.id() as i32;

            // It is tempting to unify the tvars here, but that would be problematic. These tvars should
            // eventually resolve to the type definitions that these annotations reference. By unifying
            // them, we might accidentally resolve one of the tvars to the type definition of the other,
            // which would lead to confusing behavior.
            //
            // On the other hand, if the tvars are already resolved, then we can do something
            // interesting...
            let aloc_tables = cx.aloc_tables();
            match (cx.find_graph(id1), cx.find_graph(id2)) {
                (
                    constraint::Constraints::Resolved(rt1),
                    constraint::Constraints::Resolved(rt2),
                ) if reason_of_t(&rt1).concretize_equal(reason_of_t(&rt2), &aloc_tables) => {
                    naive_unify(cx, trace, use_op, &rt1, &rt2)?;
                }
                (
                    constraint::Constraints::Resolved(rt1),
                    constraint::Constraints::FullyResolved(s2),
                ) if let rt2 = cx.force_fully_resolved_tvar(&s2)
                    && reason_of_t(&rt1).concretize_equal(reason_of_t(&rt2), &aloc_tables) =>
                {
                    naive_unify(cx, trace, use_op, &rt1, &rt2)?;
                }
                (
                    constraint::Constraints::FullyResolved(s1),
                    constraint::Constraints::Resolved(rt2),
                ) if let rt1 = cx.force_fully_resolved_tvar(&s1)
                    && reason_of_t(&rt1).concretize_equal(reason_of_t(&rt2), &aloc_tables) =>
                {
                    naive_unify(cx, trace, use_op, &rt1, &rt2)?;
                }
                (
                    constraint::Constraints::FullyResolved(s1),
                    constraint::Constraints::FullyResolved(s2),
                ) if let rt1 = cx.force_fully_resolved_tvar(&s1)
                    && let rt2 = cx.force_fully_resolved_tvar(&s2)
                    && reason_of_t(&rt1).concretize_equal(reason_of_t(&rt2), &aloc_tables) =>
                {
                    // Can we unify these types? Tempting, again, but annotations can refer to recursive type
                    // definitions, and we might get into an infinite loop (which could perhaps be avoided by
                    // a unification cache, but we'd rather not cache if we can get away with it).
                    //
                    // The alternative is to do naive unification, but we must be careful. In particular, it
                    // could cause confusing errors: recall that the naive unification of annotations goes
                    // through repositioning over these types.
                    //
                    // But if we simulate the same repositioning here, we won't really save anything. For
                    // example, these types could be essentially the same union, and repositioning them would
                    // introduce differences in their representations that would kill other
                    // optimizations. Thus, we focus on the special case where these types have the same
                    // reason, and then do naive unification.
                    naive_unify(cx, trace, use_op, &rt1, &rt2)?;
                }
                _ => {
                    naive_unify(cx, trace, use_op, t1, t2)?;
                }
            }
        }
        _ => {
            naive_unify(cx, trace, use_op, t1, t2)?;
        }
    }
    Ok(())
}

pub(super) fn unify_props<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    x: &Name,
    r1: &Reason,
    r2: &Reason,
    p1: &Property,
    p2: &Property,
) -> Result<(), FlowJsException> {
    let use_op = UseOp::Frame(
        Arc::new(VirtualFrameUseOp::PropertyCompatibility(Box::new(
            PropertyCompatibilityData {
                prop: Some(x.dupe()),
                lower: r1.dupe(),
                upper: r2.dupe(),
            },
        ))),
        Arc::new(use_op),
    );
    // If both sides are neutral fields, we can just unify once
    match (p1.deref(), p2.deref()) {
        (PropertyInner::Field(fd1), PropertyInner::Field(fd2))
            if fd1.polarity == Polarity::Neutral && fd2.polarity == Polarity::Neutral =>
        {
            rec_unify(
                cx,
                trace,
                use_op,
                UnifyCause::Uncategorized,
                None,
                &fd1.type_,
                &fd2.type_,
            )
        }
        _ => {
            // Otherwise, unify read/write sides separately.
            match (property::read_t(p1), property::read_t(p2)) {
                (Some(t1), Some(t2)) => {
                    rec_unify(
                        cx,
                        trace,
                        use_op.dupe(),
                        UnifyCause::Uncategorized,
                        None,
                        &t1,
                        &t2,
                    )?;
                }
                _ => {}
            }
            match (property::write_t(p1), property::write_t(p2)) {
                (Some(t1), Some(t2)) => {
                    rec_unify(
                        cx,
                        trace,
                        use_op.dupe(),
                        UnifyCause::Uncategorized,
                        None,
                        &t1,
                        &t2,
                    )?;
                }
                _ => {}
            }

            // Error if polarity is not compatible both ways.
            let polarity1 = property::polarity(p1);
            let polarity2 = property::polarity(p2);
            if !Polarity::equal(polarity1, polarity2) {
                add_output(
                    cx,
                    ErrorMessage::EPropPolarityMismatch(Box::new(EPropPolarityMismatchData {
                        lreason: r1.dupe(),
                        ureason: r2.dupe(),
                        props: Vec1::new((Some(x.dupe()), (polarity1, polarity2))),
                        use_op,
                    })),
                )?;
            }
            Ok(())
        }
    }
}

// If some property `x` exists in one object but not another, ensure the
// property is compatible with a dictionary, or error if none.
pub(super) fn unify_prop_with_dict<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    x: &Name,
    p: &Property,
    prop_obj_reason: &Reason,
    dict_reason: &Reason,
    dict: Option<&DictType>,
) -> Result<(), FlowJsException> {
    // prop_obj_reason: reason of the object containing the prop
    // dict_reason: reason of the object potentially containing a dictionary
    // prop_reason: reason of the prop itself
    let prop_reason = prop_obj_reason
        .dupe()
        .replace_desc(VirtualReasonDesc::RProperty(Some(x.dupe())));
    match dict {
        Some(dict_t) => {
            let string_key_t = flow_js_utils::string_key(x.dupe(), &prop_reason);
            let indexer_use_op = UseOp::Frame(
                Arc::new(VirtualFrameUseOp::IndexerKeyCompatibility {
                    lower: dict_reason.dupe(),
                    upper: prop_obj_reason.dupe(),
                }),
                Arc::new(use_op.dupe()),
            );
            rec_flow(
                cx,
                trace,
                (
                    &string_key_t,
                    &UseT::new(UseTInner::UseT(indexer_use_op, dict_t.key.dupe())),
                ),
            )?;
            let p2 = Property::new(PropertyInner::Field(Box::new(FieldData {
                preferred_def_locs: None,
                key_loc: None,
                type_: dict_t.value.dupe(),
                polarity: dict_t.dict_polarity,
            })));
            unify_props(cx, trace, use_op, x, prop_obj_reason, dict_reason, p, &p2)
        }
        None => add_output(
            cx,
            ErrorMessage::EPropNotFoundInSubtyping(Box::new(EPropNotFoundInSubtypingData {
                prop_name: Some(x.dupe()),
                reason_lower: dict_reason.dupe(),
                reason_upper: prop_obj_reason.dupe(),
                use_op,
                suggestion: None,
            })),
        ),
    }
}

// TODO: Unification between concrete types is still implemented as
// bidirectional flows. This means that the destructuring work is duplicated,
// and we're missing some opportunities for nested unification.
pub(super) fn naive_unify<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    t1: &Type,
    t2: &Type,
) -> Result<(), FlowJsException> {
    rec_flow_t(cx, trace, use_op.dupe(), (t1, t2))?;
    rec_flow_t(cx, trace, unify_flip(use_op), (t2, t1))
}

// TODO: either ensure that array_unify is the same as array_flow both ways, or
// document why not.
pub(super) fn array_unify<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    (ts1, e1, ts2, e2): (&[Type], &Type, &[Type], &Type),
) -> Result<(), FlowJsException> {
    let mut iter1 = ts1.iter();
    let mut iter2 = ts2.iter();
    loop {
        match (iter1.next(), iter2.next()) {
            (Some(t1), Some(t2)) => {
                // specific element1 = specific element2
                rec_unify(
                    cx,
                    trace,
                    use_op.dupe(),
                    UnifyCause::Uncategorized,
                    None,
                    t1,
                    t2,
                )?;
            }
            (Some(t1), None) => {
                rec_unify(
                    cx,
                    trace,
                    use_op.dupe(),
                    UnifyCause::Uncategorized,
                    None,
                    t1,
                    e2,
                )?;
                for t in iter1 {
                    rec_unify(
                        cx,
                        trace,
                        use_op.dupe(),
                        UnifyCause::Uncategorized,
                        None,
                        t,
                        e2,
                    )?;
                }
                return Ok(());
            }
            (None, Some(t2)) => {
                rec_unify(
                    cx,
                    trace,
                    use_op.dupe(),
                    UnifyCause::Uncategorized,
                    None,
                    t2,
                    e1,
                )?;
                for t in iter2 {
                    rec_unify(
                        cx,
                        trace,
                        use_op.dupe(),
                        UnifyCause::Uncategorized,
                        None,
                        t,
                        e1,
                    )?;
                }
                return Ok(());
            }
            (None, None) => {
                // general element1 = general element2
                return rec_unify(cx, trace, use_op, UnifyCause::Uncategorized, None, e1, e2);
            }
        }
    }
}
