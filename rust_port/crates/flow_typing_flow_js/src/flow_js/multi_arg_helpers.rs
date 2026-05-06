/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::VecDeque;
use std::sync::Arc;

use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::ArrRestTData;
use flow_typing_type::type_::FunMissingArgData;
use flow_typing_type::type_::FunParamData;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::ResolveSpreadTData;
use flow_typing_type::type_::ResolveSpreadsToMultiflowPartialData;
use flow_typing_type::type_::ResolvedArgData;
use flow_typing_type::type_::ResolvedSpreadArgData;
use flow_typing_type::type_::UnresolvedArgData;
use flow_typing_type::type_::dummy_this;
use flow_typing_type::type_::mk_methodtype;

use super::helpers::*;
use super::*;

// *******************************************************************
// * subtyping a sequence of arguments with a sequence of parameters *
// *******************************************************************

pub(super) fn multiflow_call<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason_op: &Reason,
    args: &[CallArg],
    ft: &FunType,
) -> Result<(), FlowJsException> {
    let resolve_to = SpreadResolve::ResolveSpreadsToMultiflowCallFull(
        flow_common::reason::mk_id() as i32,
        Box::new(ft.clone()),
    );
    resolve_call_list(
        cx,
        Some(trace),
        use_op,
        reason_op,
        args.to_vec(),
        resolve_to,
    )
}

pub(super) fn multiflow_subtype<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: VirtualUseOp<ALoc>,
    reason: &Reason,
    call_args: &[CallArg],
    funtype: &FunType,
) -> Result<(), FlowJsException> {
    let resolve_to = SpreadResolve::ResolveSpreadsToMultiflowSubtypeFull(
        flow_common::reason::mk_id() as i32,
        Box::new(funtype.clone()),
    );
    resolve_call_list(
        cx,
        Some(trace),
        use_op,
        reason,
        call_args.to_vec(),
        resolve_to,
    )
}

// Like multiflow_partial, but if there is no spread argument, it flows VoidT to
// all unused parameters
pub(super) fn multiflow_full<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason_op: &Reason,
    is_strict: bool,
    def_reason: &Reason,
    spread_arg: &Option<(Reason, ArrType, Option<GenericId>)>,
    rest_param: &Option<FunRestParam>,
    arglist: Vec<(Type, Option<GenericId>)>,
    parlist: Vec<(Option<FlowSmolStr>, Type)>,
) -> Result<(), FlowJsException> {
    let parlist_len = parlist.len();
    let (unused_parameters, _) = multiflow_partial(
        cx,
        trace,
        use_op.dupe(),
        reason_op,
        is_strict,
        def_reason,
        spread_arg,
        rest_param,
        arglist,
        parlist,
    )?;
    for (n, (_, param)) in ((parlist_len as i32) - (unused_parameters.len() as i32) + 1..)
        .zip(unused_parameters.iter())
    {
        let use_op = VirtualUseOp::Frame(
            Arc::new(VirtualFrameUseOp::FunMissingArg(Box::new(
                FunMissingArgData {
                    n,
                    op: reason_op.dupe(),
                    def: def_reason.dupe(),
                },
            ))),
            Arc::new(use_op.dupe()),
        );
        rec_flow(
            cx,
            trace,
            (
                &void::why(reason_op.dupe()),
                &UseT::new(UseTInner::UseT(use_op, param.dupe())),
            ),
        )?;
    }
    Ok(())
}

/// This is a tricky function. The simple description is that it flows all the
/// arguments to all the parameters. This function is used by
/// Function.prototype.apply, so after the arguments are applied, it returns the
/// unused parameters.
///  
/// It is a little trickier in that there may be a single spread argument after
/// all the regular arguments. There may also be a rest parameter.
pub(super) fn multiflow_partial<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason_op: &Reason,
    is_strict: bool,
    def_reason: &Reason,
    spread_arg: &Option<(Reason, ArrType, Option<GenericId>)>,
    rest_param: &Option<FunRestParam>,
    arglist: Vec<(Type, Option<GenericId>)>,
    parlist: Vec<(Option<FlowSmolStr>, Type)>,
) -> Result<(Vec<(Option<FlowSmolStr>, Type)>, Option<FunRestParam>), FlowJsException> {
    use flow_typing_type::type_::elemt_of_arrtype;

    fn multiflow_non_spreads<'cx>(
        cx: &Context<'cx>,
        use_op: &UseOp,
        n: i32,
        mut arglist: VecDeque<(Type, Option<GenericId>)>,
        mut parlist: VecDeque<(Option<FlowSmolStr>, Type)>,
    ) -> (
        Vec<(Type, UseT<Context<'cx>>)>,
        VecDeque<(Type, Option<GenericId>)>,
        VecDeque<(Option<FlowSmolStr>, Type)>,
    ) {
        match (arglist.front(), parlist.front()) {
            (_, None) | (None, _) => (vec![], arglist, parlist),
            (Some(_), Some(_)) => {
                let (tin, _generic) = arglist.pop_front().unwrap();
                let (name, tout) = parlist.pop_front().unwrap();

                let tout_use = {
                    let use_op = VirtualUseOp::Frame(
                        Arc::new(VirtualFrameUseOp::FunParam(Box::new(FunParamData {
                            n,
                            name: name.dupe(),
                            lower: reason_of_t(&tin).dupe(),
                            upper: reason_of_t(&tout).dupe(),
                        }))),
                        Arc::new(use_op.dupe()),
                    );
                    UseT::new(UseTInner::UseT(use_op, tout.dupe()))
                };
                let (mut used_pairs, unused_arglist, unused_parlist) =
                    multiflow_non_spreads(cx, use_op, n + 1, arglist, parlist);
                let par_def_loc = reason_of_use_t(&tout_use).def_loc().dupe();
                cx.add_missing_local_annot_lower_bound(par_def_loc, tin.dupe());
                used_pairs.push((tin, tout_use));
                (used_pairs, unused_arglist, unused_parlist)
            }
        }
    }

    // Handle all the non-spread arguments and all the non-rest parameters
    let original_parlist_len = parlist.len();
    let (mut used_pairs, unused_arglist, unused_parlist) = multiflow_non_spreads(
        cx,
        &use_op,
        1,
        VecDeque::from(arglist),
        VecDeque::from(parlist),
    );
    used_pairs.reverse();
    // If there is a spread argument, it will consume all the unused parameters
    let (used_pairs, unused_parlist) = match spread_arg {
        None => (used_pairs, unused_parlist),
        Some((reason, arrtype, _)) => {
            let spread_arg_elemt = elemt_of_arrtype(arrtype);
            let mut all_pairs = used_pairs;
            for (_, param) in &unused_parlist {
                let use_op = VirtualUseOp::Frame(
                    Arc::new(VirtualFrameUseOp::FunRestParam {
                        lower: reason.dupe(),
                        upper: reason_of_t(param).dupe(),
                    }),
                    Arc::new(use_op.dupe()),
                );
                all_pairs.push((
                    spread_arg_elemt.dupe(),
                    UseT::new(UseTInner::UseT(use_op, param.dupe())),
                ));
            }
            (all_pairs, VecDeque::new())
        }
    };

    // If there is a rest parameter, it will consume all the unused arguments
    match rest_param {
        None => {
            if is_strict {
                if let Some((first_unused_arg, _)) = unused_arglist.front() {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EFunctionCallExtraArg(Box::new((
                            flow_common::reason::mk_reason(
                                VirtualReasonDesc::RFunctionUnusedArgument,
                                reason_of_t(first_unused_arg).loc().dupe(),
                            ),
                            def_reason.dupe(),
                            original_parlist_len as i32,
                            use_op.dupe(),
                        ))),
                    )?;
                }
            }

            // Flow the args and params after we add the EFunctionCallExtraArg error.
            // This improves speculation error reporting.
            for (tin, tout) in &used_pairs {
                rec_flow(cx, trace, (tin, tout))?;
            }

            Ok((Vec::from(unused_parlist), rest_param.clone()))
        }
        Some(FunRestParam(name, loc, rest_param_t)) => {
            for (tin, tout) in &used_pairs {
                rec_flow(cx, trace, (tin, tout))?;
            }
            let rest_reason = reason_of_t(rest_param_t);
            let orig_rest_reason = rest_reason.dupe().reposition(loc.dupe());

            let mut rev_elems: flow_data_structure_wrapper::list::FlowOcamlList<UnresolvedParam> =
                flow_data_structure_wrapper::list::FlowOcamlList::new();
            for (arg, generic) in unused_arglist.iter() {
                let reason = flow_common::reason::mk_reason(
                    VirtualReasonDesc::RArrayElement,
                    reason_of_t(arg).loc().dupe(),
                );
                rev_elems.push_front(UnresolvedParam::UnresolvedArg(Box::new(UnresolvedArgData(
                    flow_typing_type::type_util::mk_tuple_element(
                        reason,
                        arg.dupe(),
                        None,
                        false,
                        Polarity::Neutral,
                    ),
                    generic.clone(),
                ))));
            }

            let unused_rest_param = match spread_arg {
                None => {
                    // If the rest parameter is consuming N elements, then drop N elements
                    // from the rest parameter
                    let i = rev_elems.len() as i32;
                    let rest_param_t = rest_param_t.dupe();
                    let use_op = use_op.dupe();
                    flow_typing_tvar::mk_where(cx, rest_reason.dupe(), |cx, tout| {
                        rec_flow(
                            cx,
                            trace,
                            (
                                &rest_param_t,
                                &UseT::new(UseTInner::ArrRestT(Box::new(ArrRestTData {
                                    use_op: use_op.dupe(),
                                    reason: orig_rest_reason.dupe(),
                                    index: i,
                                    tout: tout.dupe(),
                                }))),
                            ),
                        )?;
                        Ok::<(), FlowJsException>(())
                    })?
                }
                // If there is a spread argument, then a tuple rest parameter will error
                // anyway. So let's assume that the rest param is an array with unknown
                // arity. Dropping elements from it isn't worth doing *)
                Some(_) => rest_param_t.dupe(),
            };
            let elems: flow_data_structure_wrapper::list::FlowOcamlList<UnresolvedParam> =
                match spread_arg {
                    None => {
                        let mut e = rev_elems;
                        e.reverse();
                        e
                    }
                    Some((reason, arrtype, generic)) => {
                        let mut spread_array = Type::new(TypeInner::DefT(
                            reason.dupe(),
                            DefT::new(DefTInner::ArrT(Rc::new(arrtype.clone()))),
                        ));
                        if let Some(id) = generic {
                            spread_array = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                                id: id.clone(),
                                bound: spread_array,
                                reason: reason.dupe(),
                                name: id.subst_name(),
                                no_infer: false,
                            })));
                        }
                        let mut result = flow_data_structure_wrapper::list::FlowOcamlList::unit(
                            UnresolvedParam::UnresolvedSpreadArg(spread_array),
                        );
                        for elem in rev_elems.iter() {
                            result.push_front(elem.clone());
                        }
                        result
                    }
                };
            let arg_array_reason = reason_op
                .dupe()
                .replace_desc(VirtualReasonDesc::RRestArrayLit(Arc::new(
                    reason_op.desc(true).clone(),
                )));
            let arg_array = {
                let arg_array_reason2 = arg_array_reason.dupe();
                let use_op = use_op.dupe();
                flow_typing_tvar::mk_where(cx, arg_array_reason.dupe(), |cx, tout| {
                    let reason_op = &arg_array_reason2;
                    let instantiable = flow_common::reason::is_instantiable_reason(rest_reason);
                    let element_reason =
                        reason_op
                            .dupe()
                            .replace_desc(VirtualReasonDesc::RInferredUnionElemArray {
                                instantiable,
                                is_empty: elems.is_empty(),
                            });
                    let elem_t = flow_typing_tvar::mk(cx, element_reason);
                    let resolve_to = SpreadResolve::ResolveSpreadsToArrayLiteral {
                        id: flow_common::reason::mk_id() as i32,
                        as_const: false,
                        elem_t,
                        tout: tout.dupe(),
                    };
                    resolve_spread_list(cx, use_op.dupe(), reason_op, elems.dupe(), resolve_to)
                })?
            };
            {
                let use_op = VirtualUseOp::Frame(
                    Arc::new(VirtualFrameUseOp::FunRestParam {
                        lower: reason_of_t(&arg_array).dupe(),
                        upper: reason_of_t(rest_param_t).dupe(),
                    }),
                    Arc::new(use_op.dupe()),
                );
                rec_flow(
                    cx,
                    trace,
                    (
                        &arg_array,
                        &UseT::new(UseTInner::UseT(use_op, rest_param_t.dupe())),
                    ),
                )?;
            }
            Ok((
                Vec::from(unused_parlist),
                Some(FunRestParam(name.dupe(), loc.dupe(), unused_rest_param)),
            ))
        }
    }
}

pub(super) fn resolve_call_list<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    use_op: UseOp,
    reason_op: &Reason,
    args: Vec<CallArg>,
    resolve_to: SpreadResolve,
) -> Result<(), FlowJsException> {
    let unresolved: flow_data_structure_wrapper::list::FlowOcamlList<UnresolvedParam> = args
        .iter()
        .map(|arg| match arg.deref() {
            CallArgInner::Arg(t) => {
                let reason = flow_common::reason::mk_reason(
                    VirtualReasonDesc::RArrayElement,
                    loc_of_t(t).dupe(),
                );
                UnresolvedParam::UnresolvedArg(Box::new(UnresolvedArgData(
                    flow_typing_type::type_util::mk_tuple_element(
                        reason,
                        t.dupe(),
                        None,
                        false,
                        Polarity::Neutral,
                    ),
                    None,
                )))
            }
            CallArgInner::SpreadArg(t) => UnresolvedParam::UnresolvedSpreadArg(t.dupe()),
        })
        .collect();
    resolve_spread_list_rec(
        cx,
        trace,
        use_op,
        reason_op,
        flow_data_structure_wrapper::list::FlowOcamlList::new(),
        unresolved,
        resolve_to,
    )
}

pub(super) fn resolve_spread_list<'cx>(
    cx: &Context<'cx>,
    use_op: UseOp,
    reason_op: &Reason,
    list: flow_data_structure_wrapper::list::FlowOcamlList<UnresolvedParam>,
    resolve_to: SpreadResolve,
) -> Result<(), FlowJsException> {
    resolve_spread_list_rec(
        cx,
        None,
        use_op,
        reason_op,
        flow_data_structure_wrapper::list::FlowOcamlList::new(),
        list,
        resolve_to,
    )
}

pub(super) fn resolve_spread_list_rec<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    use_op: UseOp,
    reason_op: &Reason,
    mut resolved_rev: flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
    mut unresolved: flow_data_structure_wrapper::list::FlowOcamlList<UnresolvedParam>,
    resolve_to: SpreadResolve,
) -> Result<(), FlowJsException> {
    loop {
        let next = match unresolved.first() {
            None => {
                resolved_rev.reverse();
                return finish_resolve_spread_list(
                    cx,
                    trace,
                    use_op,
                    reason_op,
                    resolved_rev,
                    resolve_to,
                );
            }
            Some(n) => n.clone(),
        };
        unresolved.drop_first();
        match next {
            UnresolvedParam::UnresolvedArg(box UnresolvedArgData(next_elem, generic)) => {
                resolved_rev.push_front(ResolvedParam::ResolvedArg(Box::new(ResolvedArgData(
                    next_elem, generic,
                ))));
            }
            UnresolvedParam::UnresolvedSpreadArg(next_t) => {
                return flow_opt(
                    cx,
                    trace,
                    (
                        &next_t,
                        &UseT::new(UseTInner::ResolveSpreadT(Box::new(ResolveSpreadTData {
                            use_op,
                            reason: reason_op.dupe(),
                            resolve_spread_type: Box::new(ResolveSpreadType {
                                rrt_resolved: resolved_rev,
                                rrt_unresolved: unresolved,
                                rrt_resolve_to: resolve_to,
                            }),
                        }))),
                    ),
                );
            }
        }
    }
}

// Now that everything is resolved, we can construct whatever type we're trying to resolve to.
pub(super) fn finish_resolve_spread_list<'cx>(
    cx: &Context<'cx>,
    trace: Option<DepthTrace>,
    use_op: UseOp,
    reason_op: &Reason,
    resolved: flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
    resolve_to: SpreadResolve,
) -> Result<(), FlowJsException> {
    use ResolvedParam;
    use flow_typing_generics::array_spread;
    use flow_typing_type::type_::ArrType;
    use flow_typing_type::type_::ArrayATData;
    use flow_typing_type::type_::DefT;
    use flow_typing_type::type_::SpreadArrayResolveTo;
    use flow_typing_type::type_::TupleATData;
    use flow_typing_type::type_::TupleElement;
    use flow_typing_type::type_::TupleView;
    use flow_typing_type::type_::any_t;
    use flow_typing_type::type_::elemt_of_arrtype;
    use flow_typing_type::type_::mixed_t;
    use flow_typing_type::type_::ro_of_arrtype;
    use flow_typing_type::type_util::reason_of_resolved_param;
    use flow_typing_type::type_util::type_ex_set;

    fn propagate_dro<'cx>(cx: &Context<'cx>, elem: Type, arrtype: &ArrType) -> Type {
        match arrtype {
            ArrType::ROArrayAT(box (_, Some(l)))
            | ArrType::ArrayAT(box ArrayATData {
                react_dro: Some(l), ..
            })
            | ArrType::TupleAT(box TupleATData {
                react_dro: Some(l), ..
            }) => mk_react_dro(cx, unknown_use(), l.clone(), elem),
            _ => elem,
        }
    }

    // Turn tuple rest params into single params
    fn flatten_spread_args<'cx>(
        cx: &Context<'cx>,
        args: &flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
    ) -> Result<(Vec<ResolvedParam>, bool, bool), FlowJsException> {
        let (args, spread_after_opt, _seen_opt, inexact_spread) = args.iter().try_fold(
            (Vec::<ResolvedParam>::new(), false, false, false),
            |acc, arg| -> Result<_, FlowJsException> {
                // let (args_rev, spread_after_opt, seen_opt, inexact_spread) = acc in
                let (mut args, spread_after_opt, seen_opt, inexact_spread) = acc;
                if inexact_spread {
                    // We have an element after an inexact spread
                    let reason = reason_of_resolved_param(arg);
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::ETupleElementAfterInexactSpread(reason.dupe()),
                    )?;
                }
                // match arg with
                match arg {
                    ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                        _,
                        arrtype,
                        generic,
                    )) => {
                        let spread_after_opt = spread_after_opt || seen_opt;
                        let (args, seen_opt, inexact_spread) = match arrtype {
                            ArrType::ArrayAT(box ArrayATData {
                                tuple_view: None, ..
                            }) => {
                                args.push(arg.clone());
                                (args, seen_opt, inexact_spread)
                            }
                            ArrType::ArrayAT(box ArrayATData {
                                tuple_view: Some(tv),
                                ..
                            }) if tv.elements.is_empty() => {
                                // The latter two cases corresponds to the empty array.
                                let inexact = tv.inexact;
                                args.push(arg.clone());
                                (args, seen_opt, inexact_spread || inexact)
                            }
                            ArrType::TupleAT(box TupleATData {
                                elements, inexact, ..
                            }) if elements.is_empty() => {
                                args.push(arg.clone());
                                (args, seen_opt, inexact_spread || *inexact)
                            }
                            ArrType::ArrayAT(box ArrayATData {
                                tuple_view: Some(tv),
                                ..
                            }) => {
                                let elements = &tv.elements;
                                let inexact = tv.inexact;
                                let (args, seen_opt) = elements.iter().fold(
                                    (args, seen_opt),
                                    |(mut args, seen_opt), elem| {
                                        let new_elem = TupleElement {
                                            t: propagate_dro(cx, elem.t.dupe(), arrtype),
                                            ..elem.clone()
                                        };
                                        args.push(ResolvedParam::ResolvedArg(Box::new(
                                            ResolvedArgData(new_elem, generic.clone()),
                                        )));
                                        (args, seen_opt || elem.optional)
                                    },
                                );
                                (args, seen_opt, inexact_spread || inexact)
                            }
                            ArrType::TupleAT(box TupleATData {
                                elements, inexact, ..
                            }) => {
                                let (args, seen_opt) = elements.iter().fold(
                                    (args, seen_opt),
                                    |(mut args, seen_opt), elem| {
                                        let new_elem = TupleElement {
                                            t: propagate_dro(cx, elem.t.dupe(), arrtype),
                                            ..elem.clone()
                                        };
                                        args.push(ResolvedParam::ResolvedArg(Box::new(
                                            ResolvedArgData(new_elem, generic.clone()),
                                        )));
                                        (args, seen_opt || elem.optional)
                                    },
                                );
                                (args, seen_opt, inexact_spread || *inexact)
                            }
                            ArrType::ROArrayAT(box (_, _)) => {
                                args.push(arg.clone());
                                (args, seen_opt, inexact_spread)
                            }
                        };
                        Ok((args, spread_after_opt, seen_opt, inexact_spread))
                    }
                    ResolvedParam::ResolvedAnySpreadArg(_, _) => {
                        args.push(arg.clone());
                        Ok((args, spread_after_opt, seen_opt, inexact_spread))
                    }
                    ResolvedParam::ResolvedArg(box ResolvedArgData(
                        TupleElement { optional, .. },
                        _,
                    )) => {
                        let optional = *optional;
                        args.push(arg.clone());
                        Ok((args, spread_after_opt, seen_opt || optional, inexact_spread))
                    }
                }
            },
        )?;
        Ok((args, spread_after_opt, inexact_spread))
    }

    fn spread_resolved_to_any_src(
        resolved: &flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
    ) -> Option<AnySource> {
        resolved.iter().find_map(|arg| match arg {
            ResolvedParam::ResolvedAnySpreadArg(_, src) => Some(*src),
            ResolvedParam::ResolvedArg(box ResolvedArgData(_, _))
            | ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(_, _, _)) => None,
        })
    }

    fn finish_array<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        trace: Option<DepthTrace>,
        reason_op: &Reason,
        resolve_to: SpreadArrayResolveTo,
        resolved: &flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
        elem_t: &Type,
        tout: &Type,
    ) -> Result<(), FlowJsException> {
        let result = match spread_resolved_to_any_src(resolved) {
            Some(any_src) => match resolve_to {
                // Array<any> is a good enough any type for arrays
                SpreadArrayResolveTo::ResolveToArray => Type::new(TypeInner::DefT(
                    reason_op.dupe(),
                    DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                        ArrayATData {
                            elem_t: any_t::why(any_src, reason_op.dupe()),
                            tuple_view: None,
                            react_dro: None,
                        },
                    ))))),
                )),
                // Array literals can flow to a tuple. Arrays can't. So if the presence
                // of an `any` forces us to degrade an array literal to Array<any> then
                // we might get a new error. Since introducing `any`'s shouldn't cause
                // errors, this is bad. Instead, let's degrade array literals to `any`
                SpreadArrayResolveTo::ResolveToArrayLiteral { .. } => {
                    any_t::why(any_src, reason_op.dupe())
                }
                // There is no AnyTupleT type, so let's degrade to `any`.
                SpreadArrayResolveTo::ResolveToTupleType { .. } => {
                    any_t::why(any_src, reason_op.dupe())
                }
            },
            None => {
                let (elems, spread_after_opt, inexact_spread) = flatten_spread_args(cx, resolved)?;
                let as_const = match resolve_to {
                    SpreadArrayResolveTo::ResolveToArrayLiteral { as_const } => as_const,
                    SpreadArrayResolveTo::ResolveToTupleType { .. }
                    | SpreadArrayResolveTo::ResolveToArray => false,
                };
                // let tuple_elements = match resolve_to with
                let tuple_elements: Option<Vec<TupleElement>> = match resolve_to {
                    // | ResolveToArrayLiteral _ | ResolveToTupleType _ ->
                    SpreadArrayResolveTo::ResolveToArrayLiteral { .. }
                    | SpreadArrayResolveTo::ResolveToTupleType { .. } => {
                        let mut early_break = false;
                        let mut tuple_elements = Vec::new();
                        for elem in &elems {
                            match elem {
                                ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                                    _,
                                    ArrType::ArrayAT(box ArrayATData {
                                        tuple_view: Some(TupleView { elements, .. }),
                                        ..
                                    }),
                                    _,
                                ))
                                | ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                                    _,
                                    ArrType::TupleAT(box TupleATData { elements, .. }),
                                    _,
                                )) if elements.is_empty() => {
                                    // Spread of empty array/tuple results
                                    // in same tuple elements as before.
                                }
                                ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                                    _,
                                    _,
                                    _,
                                )) => {
                                    early_break = true;
                                    break;
                                }
                                ResolvedParam::ResolvedArg(box ResolvedArgData(elem, _)) => {
                                    let TupleElement {
                                        t,
                                        optional,
                                        name,
                                        reason,
                                        polarity: _,
                                    } = elem;
                                    let polarity = if as_const {
                                        Polarity::Positive
                                    } else {
                                        Polarity::Neutral
                                    };
                                    let new_elem = TupleElement {
                                        t: t.dupe(),
                                        optional: *optional,
                                        name: name.dupe(),
                                        reason: reason.dupe(),
                                        polarity,
                                    };
                                    tuple_elements.push(new_elem);
                                }
                                ResolvedParam::ResolvedAnySpreadArg(_, _) => {
                                    panic!("Should not be hit")
                                }
                            }
                        }
                        if early_break {
                            None
                        } else {
                            Some(tuple_elements)
                        }
                    }
                    SpreadArrayResolveTo::ResolveToArray => None,
                };

                // We infer the array's general element type by looking at the type of
                // every element in the array
                let (tset, generic) = elems.iter().fold(
                    (type_ex_set::empty(), array_spread::T::Bottom),
                    |(mut tset, generic_state), elem| {
                        let (elem_t_val, generic_id, ro) = match elem {
                            ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                                _,
                                arrtype,
                                generic,
                            )) => (
                                propagate_dro(cx, elemt_of_arrtype(arrtype), arrtype),
                                generic.as_ref(),
                                ro_of_arrtype(arrtype),
                            ),
                            ResolvedParam::ResolvedArg(box ResolvedArgData(
                                TupleElement { t: et, .. },
                                generic,
                            )) => (
                                et.dupe(),
                                generic.as_ref(),
                                array_spread::RoStatus::NonROSpread,
                            ),
                            ResolvedParam::ResolvedAnySpreadArg(_, _) => {
                                panic!("Should not be hit")
                            }
                        };
                        type_ex_set::add(elem_t_val, &mut tset);
                        let merged = array_spread::merge(
                            &|msgs: &[String]| {
                                flow_typing_debug::verbose::print_if_verbose_lazy(
                                    cx,
                                    trace.as_ref(),
                                    None,
                                    None,
                                    || msgs.to_vec(),
                                );
                            },
                            generic_state,
                            generic_id,
                            ro,
                        );
                        (tset, merged)
                    },
                );
                let generic = array_spread::to_option(&generic);

                // composite elem type is an upper bound of all element types
                /*
                   Should the element type of the array be the union of its element types?

                   No. Instead of using a union, we use an unresolved tvar to
                   represent the least upper bound of each element type. Effectively,
                   this keeps the element type "open," at least locally.[*]

                   Using a union pins down the element type prematurely, and moreover,
                   might lead to speculative matching when setting elements or caling
                   contravariant methods (`push`, `concat`, etc.) on the array.

                   In any case, using a union doesn't quite work as intended today
                   when the element types themselves could be unresolved tvars. For
                   example, the following code would work even with unions:

                   declare var o: { x: number; }
                   var a = ["hey", o.x]; // no error, but is an error if 42 replaces o.x
                   declare var i: number;
                   a[i] = false;

                   [*] Eventually, the element type does get pinned down to a union
                   when the type of the expression is resolved. In the future we might
                   have to do that pinning more carefully, and using an unresolved
                   tvar instead of a union here doesn't conflict with those plans.
                */
                if inexact_spread {
                    let reason_mixed = reason_op
                        .dupe()
                        .replace_desc(VirtualReasonDesc::RTupleUnknownElementFromInexact);
                    let t = mixed_t::make(reason_mixed);
                    helpers::flow(
                        cx,
                        (
                            &t,
                            &UseT::new(UseTInner::UseT(use_op.dupe(), elem_t.dupe())),
                        ),
                    )?;
                } else {
                    for type_ex in tset.iter() {
                        helpers::flow(
                            cx,
                            (
                                &type_ex.0,
                                &UseT::new(UseTInner::UseT(use_op.dupe(), elem_t.dupe())),
                            ),
                        )?;
                    }
                }

                let create_tuple_type =
                    |inexact: bool, elements: Vec<TupleElement>| -> Result<Type, FlowJsException> {
                        let (valid, arity) =
                            flow_js_utils::validate_tuple_elements(cx, reason_op, true, &elements)?;
                        let arity = (arity.0 as i32, arity.1 as i32);
                        let inexact = inexact || inexact_spread;
                        Ok(if valid {
                            Type::new(TypeInner::DefT(
                                reason_op.dupe(),
                                DefT::new(DefTInner::ArrT(Rc::new(ArrType::TupleAT(Box::new(
                                    TupleATData {
                                        elem_t: elem_t.dupe(),
                                        elements: elements.into(),
                                        arity,
                                        inexact,
                                        react_dro: None,
                                    },
                                ))))),
                            ))
                        } else {
                            any_t::error(reason_op.dupe())
                        })
                    };
                let t = match (resolve_to, &tuple_elements, spread_after_opt) {
                    (SpreadArrayResolveTo::ResolveToArray, _, _)
                    | (SpreadArrayResolveTo::ResolveToArrayLiteral { .. }, None, _)
                    | (SpreadArrayResolveTo::ResolveToArrayLiteral { .. }, _, true) => {
                        let arrtype = if as_const {
                            ArrType::ROArrayAT(Box::new((elem_t.dupe(), None)))
                        } else {
                            ArrType::ArrayAT(Box::new(ArrayATData {
                                elem_t: elem_t.dupe(),
                                tuple_view: None,
                                react_dro: None,
                            }))
                        };
                        Type::new(TypeInner::DefT(
                            reason_op.dupe(),
                            DefT::new(DefTInner::ArrT(Rc::new(arrtype))),
                        ))
                    }
                    (
                        SpreadArrayResolveTo::ResolveToArrayLiteral { as_const: false },
                        Some(elements),
                        _,
                    ) => {
                        let (valid, arity) =
                            flow_js_utils::validate_tuple_elements(cx, reason_op, false, elements)?;
                        let arity = (arity.0 as i32, arity.1 as i32);
                        if valid {
                            Type::new(TypeInner::DefT(
                                reason_op.dupe(),
                                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                                    ArrayATData {
                                        elem_t: elem_t.dupe(),
                                        tuple_view: Some(TupleView {
                                            elements: elements.clone().into(),
                                            arity,
                                            inexact: inexact_spread,
                                        }),
                                        react_dro: None,
                                    },
                                ))))),
                            ))
                        } else {
                            Type::new(TypeInner::DefT(
                                reason_op.dupe(),
                                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                                    ArrayATData {
                                        elem_t: elem_t.dupe(),
                                        tuple_view: None,
                                        react_dro: None,
                                    },
                                ))))),
                            ))
                        }
                    }
                    (SpreadArrayResolveTo::ResolveToTupleType { inexact }, Some(elements), _) => {
                        create_tuple_type(inexact, elements.clone())?
                    }
                    (
                        SpreadArrayResolveTo::ResolveToArrayLiteral { as_const: true },
                        Some(elements),
                        _,
                    ) => create_tuple_type(false, elements.clone())?,
                    (SpreadArrayResolveTo::ResolveToTupleType { .. }, None, _) => {
                        any_t::error(reason_op.dupe())
                    }
                };
                match generic {
                    Some(id) => Type::new(TypeInner::GenericT(Box::new(GenericTData {
                        reason: reason_of_t(&t).dupe(),
                        name: id.subst_name(),
                        bound: t,
                        no_infer: false,
                        id: id.clone(),
                    }))),
                    None => t,
                }
            }
        };
        flow_opt_t(cx, use_op, trace, (&result, tout))
    }

    // If there are no spread elements or if all the spread elements resolved to
    // tuples or array literals, then this is easy. We just flatten them all.
    //
    // However, if we have a spread that resolved to any or to an array of
    // unknown length, then we're in trouble. Basically, any remaining argument
    // might flow to any remaining parameter.
    fn flatten_call_arg<'cx>(
        cx: &Context<'cx>,
        use_op: UseOp,
        r: &Reason,
        resolved: flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
    ) -> Result<
        (
            Vec<(Type, Option<flow_typing_generics::GenericId>)>,
            Option<(Reason, ArrType, Option<flow_typing_generics::GenericId>)>,
        ),
        FlowJsException,
    > {
        fn flatten<'cx>(
            cx: &Context<'cx>,
            mut args: Vec<(Type, Option<flow_typing_generics::GenericId>)>,
            spread: Option<(type_ex_set::TypeExSet, Option<ArrType>, array_spread::T)>,
            resolved: flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
        ) -> (
            Vec<(Type, Option<flow_typing_generics::GenericId>)>,
            Option<(type_ex_set::TypeExSet, Option<ArrType>, array_spread::T)>,
        ) {
            if resolved.is_empty() {
                return (args, spread);
            }
            match spread {
                None => {
                    let head = resolved.first().expect("non-empty");
                    match head {
                        ResolvedParam::ResolvedArg(box ResolvedArgData(
                            TupleElement { t, .. },
                            generic,
                        )) => {
                            args.push((t.dupe(), generic.clone()));
                            let mut rest = resolved.dupe();
                            rest.drop_first();
                            flatten(cx, args, None, rest)
                        }
                        ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                            _,
                            ArrType::ArrayAT(box ArrayATData {
                                tuple_view:
                                    Some(TupleView {
                                        elements,
                                        arity: _,
                                        inexact,
                                    }),
                                ..
                            }),
                            generic,
                        ))
                        | ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                            _,
                            ArrType::TupleAT(box TupleATData {
                                elements, inexact, ..
                            }),
                            generic,
                        )) => {
                            let inexact = *inexact;
                            let mapped: Vec<_> = elements
                                .iter()
                                .map(|elem| (elem.t.dupe(), generic.clone()))
                                .collect();
                            for item in mapped {
                                args.push(item);
                            }
                            if inexact {
                                let spread =
                                    Some((type_ex_set::empty(), None, array_spread::T::Bottom));
                                flatten(cx, args, spread, resolved)
                            } else {
                                let mut rest = resolved.dupe();
                                rest.drop_first();
                                flatten(cx, args, None, rest)
                            }
                        }
                        ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(_, _, _))
                        | ResolvedParam::ResolvedAnySpreadArg(_, _) => {
                            // We weren't able to flatten the call argument list to remove all
                            // spreads. This means we need to build a spread argument, with
                            // unknown arity.
                            let spread =
                                Some((type_ex_set::empty(), None, array_spread::T::Bottom));
                            flatten(cx, args, spread, resolved)
                        }
                    }
                }
                // | Some (tset, last_inexact_tuple, generic) ->
                Some((mut tset, last_inexact_tuple, generic)) => {
                    let resolved_len = resolved.len();
                    let head = resolved.first().expect("non-empty");
                    let (tset_new, lit_new, g_prime, ro, rest) = match head {
                        ResolvedParam::ResolvedArg(box ResolvedArgData(
                            TupleElement { t, .. },
                            generic,
                        )) => {
                            type_ex_set::add(t.dupe(), &mut tset);
                            let mut rest = resolved.dupe();
                            rest.drop_first();
                            (
                                tset,
                                last_inexact_tuple,
                                generic.as_ref(),
                                array_spread::RoStatus::NonROSpread,
                                rest,
                            )
                        }
                        ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                            _,
                            arrtype,
                            generic_val,
                        )) if resolved_len == 1
                            && matches!(
                                arrtype,
                                ArrType::TupleAT(box TupleATData { inexact: true, .. })
                            )
                            && tset.is_empty() =>
                        {
                            (
                                tset,
                                Some(arrtype.clone()),
                                generic_val.as_ref(),
                                ro_of_arrtype(arrtype),
                                flow_data_structure_wrapper::list::FlowOcamlList::new(),
                            )
                        }
                        ResolvedParam::ResolvedSpreadArg(box ResolvedSpreadArgData(
                            _,
                            arrtype,
                            generic_val,
                        )) => {
                            type_ex_set::add(elemt_of_arrtype(arrtype), &mut tset);
                            let mut rest = resolved.dupe();
                            rest.drop_first();
                            (
                                tset,
                                last_inexact_tuple,
                                generic_val.as_ref(),
                                ro_of_arrtype(arrtype),
                                rest,
                            )
                        }
                        ResolvedParam::ResolvedAnySpreadArg(reason, any_src) => {
                            type_ex_set::add(any_t::why(*any_src, reason.dupe()), &mut tset);
                            let mut rest = resolved.dupe();
                            rest.drop_first();
                            (
                                tset,
                                last_inexact_tuple,
                                None,
                                array_spread::RoStatus::NonROSpread,
                                rest,
                            )
                        }
                    };
                    let merged_generic = array_spread::merge(
                        &|msgs: &[String]| {
                            flow_typing_debug::verbose::print_if_verbose_lazy(
                                cx,
                                None,
                                None,
                                None,
                                || msgs.to_vec(),
                            );
                        },
                        generic,
                        g_prime,
                        ro,
                    );
                    flatten(cx, args, Some((tset_new, lit_new, merged_generic)), rest)
                }
            }
        }

        let (args, spread) = flatten(cx, Vec::new(), None, resolved);
        let spread = if let Some((tset, last_inexact_tuple, generic)) = spread {
            let generic = array_spread::to_option(&generic).cloned();
            let r = flow_common::reason::mk_reason(VirtualReasonDesc::RArray, r.loc().dupe());
            let arrtype = match last_inexact_tuple {
                Some(arrtype) => arrtype,
                None => {
                    let elem_t = flow_typing_tvar::mk_where(cx, r.dupe(), |cx, tvar| {
                        for type_ex in tset.iter() {
                            helpers::flow(
                                cx,
                                (
                                    &type_ex.0,
                                    &UseT::new(UseTInner::UseT(use_op.dupe(), tvar.dupe())),
                                ),
                            )?;
                        }
                        Ok::<(), FlowJsException>(())
                    })?;
                    ArrType::ArrayAT(Box::new(ArrayATData {
                        elem_t,
                        tuple_view: None,
                        react_dro: None,
                    }))
                }
            };
            Some((r, arrtype, generic))
        } else {
            None
        };
        Ok((args, spread))
    }

    // This is used for things like Function.prototype.bind, which partially
    // apply arguments and then return the new function.
    fn finish_multiflow_partial<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        use_op: UseOp,
        reason_op: &Reason,
        ft: &FunType,
        call_reason: &Reason,
        resolved: flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
        tout: &Type,
    ) -> Result<(), FlowJsException> {
        // Multiflows always come out of a flow
        let trace = trace.expect("All multiflows show have a trace");
        let FunType {
            this_t: _,
            params,
            rest_param,
            return_t,
            type_guard,
            def_reason,
            effect_,
        } = ft;
        let (args, spread_arg) = flatten_call_arg(cx, use_op.dupe(), reason_op, resolved)?;
        let parlist: Vec<(Option<FlowSmolStr>, Type)> = params
            .iter()
            .map(|FunParam(name, t)| (name.dupe(), t.dupe()))
            .collect();
        let (remaining_params, rest_param) = multiflow_partial(
            cx,
            trace,
            use_op.dupe(),
            reason_op,
            true,
            def_reason,
            &spread_arg,
            rest_param,
            args,
            parlist,
        )?;
        let params_names: Vec<Option<Name>> = remaining_params
            .iter()
            .map(|(name, _)| name.as_ref().map(|n| Name::new(n.dupe())))
            .collect();
        let params_tlist: Vec<Type> = remaining_params.iter().map(|(_, t)| t.dupe()).collect();
        // "bound function type", positioned at reason_op
        let bound_reason = call_reason
            .dupe()
            .replace_desc(VirtualReasonDesc::RBound(Arc::new(
                reason_op.desc(true).clone(),
            )));
        let def_reason = reason_op.dupe();
        let funt = Type::new(TypeInner::DefT(
            reason_op.dupe(),
            DefT::new(DefTInner::FunT(
                dummy_static(bound_reason),
                Rc::new(mk_methodtype(
                    dummy_this(reason_op.loc().dupe()),
                    None, // subtyping
                    Some(effect_.clone()),
                    params_tlist,
                    rest_param,
                    def_reason,
                    Some(params_names),
                    type_guard.clone(),
                    return_t.clone(),
                )),
            )),
        ));
        rec_flow_t(cx, trace, unknown_use(), (&funt, tout))
    }

    // This is used for things like function application, where all the arguments
    // are applied to a function
    fn finish_multiflow_full<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        use_op: UseOp,
        reason_op: &Reason,
        is_strict: bool,
        ft: &FunType,
        resolved: flow_data_structure_wrapper::list::FlowOcamlList<ResolvedParam>,
    ) -> Result<(), FlowJsException> {
        // (* Multiflows always come out of a flow *)
        let trace = trace.expect("All multiflows show have a trace");
        let params = &ft.params;
        let rest_param = &ft.rest_param;
        let def_reason = &ft.def_reason;
        let (args, spread_arg) = flatten_call_arg(cx, use_op.dupe(), reason_op, resolved)?;
        let parlist: Vec<(Option<FlowSmolStr>, Type)> = params
            .iter()
            .map(|FunParam(name, t)| (name.dupe(), t.dupe()))
            .collect();
        multiflow_full(
            cx,
            trace,
            use_op,
            reason_op,
            is_strict,
            def_reason,
            &spread_arg,
            rest_param,
            args,
            parlist,
        )
    }

    match resolve_to {
        SpreadResolve::ResolveSpreadsToTupleType {
            id: _,
            inexact,
            elem_t,
            tout,
        } => finish_array(
            cx,
            use_op,
            trace,
            reason_op,
            SpreadArrayResolveTo::ResolveToTupleType { inexact },
            &resolved,
            &elem_t,
            &tout,
        ),
        SpreadResolve::ResolveSpreadsToArrayLiteral {
            id: _,
            as_const,
            elem_t,
            tout,
        } => finish_array(
            cx,
            use_op,
            trace,
            reason_op,
            SpreadArrayResolveTo::ResolveToArrayLiteral { as_const },
            &resolved,
            &elem_t,
            &tout,
        ),
        SpreadResolve::ResolveSpreadsToArray(elem_t, tout) => finish_array(
            cx,
            use_op,
            trace,
            reason_op,
            SpreadArrayResolveTo::ResolveToArray,
            &resolved,
            &elem_t,
            &tout,
        ),
        SpreadResolve::ResolveSpreadsToMultiflowPartial(
            box ResolveSpreadsToMultiflowPartialData(_, ft, call_reason, tout),
        ) => finish_multiflow_partial(
            cx,
            trace,
            use_op,
            reason_op,
            &ft,
            &call_reason,
            resolved,
            &tout,
        ),
        SpreadResolve::ResolveSpreadsToMultiflowCallFull(_, ft) => {
            finish_multiflow_full(cx, trace, use_op, reason_op, true, &ft, resolved)
        }
        SpreadResolve::ResolveSpreadsToMultiflowSubtypeFull(_, ft) => {
            finish_multiflow_full(cx, trace, use_op, reason_op, false, &ft, resolved)
        }
    }
}
