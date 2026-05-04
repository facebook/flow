/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;
use std::sync::Arc;

use flow_typing_debug::verbose::print_types_if_verbose;
use flow_typing_errors::error_message::ECallTypeArityData;
use flow_typing_errors::error_message::EIncompatibleData;
use flow_typing_errors::error_message::EIncompatiblePropData;
use flow_typing_errors::error_message::EIncompatibleWithUseOpData;
use flow_typing_errors::error_message::EPropNotFoundInLookupData;
use flow_typing_errors::error_message::EPropNotFoundInSubtypingData;
use flow_typing_errors::error_message::EPropNotReadableData;
use flow_typing_errors::error_message::EPropNotWritableData;
use flow_typing_errors::error_message::ETupleInvalidTypeSpreadData;
use flow_typing_errors::error_message::EnumInvalidMemberAccessData;
use flow_typing_errors::error_message::EnumInvalidObjectFunctionData;
use flow_typing_errors::error_message::EnumInvalidObjectUtilTypeData;
use flow_typing_errors::error_message::EnumModificationData;
use flow_typing_type::type_::ArrRestTData;
use flow_typing_type::type_::BindTData;
use flow_typing_type::type_::CallElemTData;
use flow_typing_type::type_::CallMData;
use flow_typing_type::type_::CallTData;
use flow_typing_type::type_::ClassImplementsCheckData;
use flow_typing_type::type_::ConcretizeTData;
use flow_typing_type::type_::CondTData;
use flow_typing_type::type_::ConditionalTData;
use flow_typing_type::type_::DestructorSpreadTypeData;
use flow_typing_type::type_::ElemTData;
use flow_typing_type::type_::EvalTypeDestructorTData;
use flow_typing_type::type_::ExtendsUseTData;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::GetElemTData;
use flow_typing_type::type_::GetEnumTData;
use flow_typing_type::type_::GetTypeFromNamespaceTData;
use flow_typing_type::type_::HasOwnPropTData;
use flow_typing_type::type_::LookupTData;
use flow_typing_type::type_::MapTypeTData;
use flow_typing_type::type_::MethodTData;
use flow_typing_type::type_::NonstrictReturningData;
use flow_typing_type::type_::OptionalIndexedAccessTData;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::ReactKitTData;
use flow_typing_type::type_::ReadElemData;
use flow_typing_type::type_::ReposUseTData;
use flow_typing_type::type_::ResolveSpreadTData;
use flow_typing_type::type_::ResolveSpreadsToMultiflowPartialData;
use flow_typing_type::type_::ResolveUnionTData;
use flow_typing_type::type_::ResolvedSpreadArgData;
use flow_typing_type::type_::SealGenericTData;
use flow_typing_type::type_::SetElemTData;
use flow_typing_type::type_::SpecializeTData;
use flow_typing_type::type_::SuperTData;
use flow_typing_type::type_::TestPropTData;
use flow_typing_type::type_::ThisInstanceTData;
use flow_typing_type::type_::ThisTypeAppTData;
use flow_typing_type::type_::TypeAppTData;
use flow_typing_type::type_::ValueToTypeReferenceTData;
use flow_typing_type::type_::WriteElemData;
use flow_typing_type::type_::object::ObjectToolObjectMapData;

use super::any_helpers::*;
use super::constraint_helpers::*;
use super::eval_helpers::*;
use super::get_prop_helpers::*;
use super::helpers::mk_typeapp_instance_annot;
use super::helpers::*;
use super::inheritance_helpers::*;
use super::instantiation_helpers::*;
use super::multi_arg_helpers::*;
use super::*;
use crate::default_resolve;
use crate::implicit_instantiation;
use crate::object_kit;
use crate::react_kit;
use crate::slice_utils;
use crate::speculation_kit;
use crate::tvar_resolver;

/// Tail call continuation for the trampoline in __flow.
///
/// In OCaml, certain calls in __flow_impl are in tail position and are
/// optimized by the compiler to reuse the stack frame. In Rust, we return
/// these as continuations for the trampoline to process iteratively.
enum TailCall<'cx> {
    /// Replaces a tail call to `flow()` (e.g. from resolve_union):
    /// uses unit_trace, catches LimitExceeded.
    Flow(Type, UseT<Context<'cx>>),
    /// Replaces a tail call to `rec_flow()` (e.g. from EvalT, OpenT handlers):
    /// uses given trace, propagates LimitExceeded.
    RecFlow(Type, UseT<Context<'cx>>, DepthTrace),
}

/// NOTE: Do not call this function directly. Instead, call the wrapper
/// functions `rec_flow`, `join_flow`, or `flow_opt` (described below) inside
/// this module, and the function `flow` outside this module.
///
/// This is a trampoline wrapper that handles tail call optimization for
/// mutually recursive chains in __flow_impl. In OCaml, calls like
/// `rec_flow cx trace (result, u)` at the end of a match arm are in tail
/// position and optimized by the compiler. In Rust, we return these as
/// TailCall continuations and process them in a loop to avoid stack overflow.
pub(super) fn __flow<'cx>(
    cx: &Context<'cx>,
    (l, u): (&Type, &UseT<Context<'cx>>),
    trace: DepthTrace,
) -> Result<(), FlowJsException> {
    let mut tc = __flow_impl(cx, (l, u), trace)?;

    // Process tail calls iteratively instead of recursing
    while let Some(tail_call) = tc {
        let (tc_l, tc_u, tc_trace, catch_limit) = match tail_call {
            TailCall::Flow(l, u) => (l, u, DepthTrace::unit_trace(), true),
            TailCall::RecFlow(l, u, t) => (l, u, t, false),
        };
        match __flow_impl(cx, (&tc_l, &tc_u), tc_trace) {
            Ok(next) => tc = next,
            Err(FlowJsException::LimitExceeded) if catch_limit => {
                // flow() handles LimitExceeded by recording an error and continuing
                let rl = reason_of_t(&tc_l).dupe();
                let ru = reason_of_use_t(&tc_u).dupe();
                let reasons = match tc_u.deref() {
                    UseTInner::UseT(_, _) => (ru, rl),
                    _ => flow_error::ordered_reasons((rl, ru)),
                };
                flow_js_utils::add_output_non_speculating(
                    cx,
                    ErrorMessage::ERecursionLimit(Box::new((reasons.0, reasons.1))),
                );
                break;
            }
            Err(e) => return Err(e),
        }
    }
    Ok(())
}

fn __flow_impl<'cx>(
    cx: &Context<'cx>,
    (l, u): (&Type, &UseT<Context<'cx>>),
    trace: DepthTrace,
) -> Result<Option<TailCall<'cx>>, FlowJsException> {
    if flow_typing_type::type_util::ground_subtype_use_t(
        &|t| flow_js_utils::update_lit_type_from_annot(cx, t),
        l,
        u,
    ) {
        print_types_if_verbose(cx, &trace, None, l, u);
        return Ok(None);
    }
    // Fast path: if l is pointer-equal to the Type inside UseT<Context<'cx>>, it's trivially l <: l.
    // This must come after ground_subtype_use_t, which has a side effect of recording
    // singleton literal checks needed by implicit instantiation.
    if let UseTInner::UseT(_, t) = u.deref() {
        if l.ptr_eq(t) {
            return Ok(None);
        }
    }
    if flow_typing_flow_common::flow_cache::flow_constraint::get(cx, l, u) {
        print_types_if_verbose(cx, &trace, Some("(cached)"), l, u);
        return Ok(None);
    }
    print_types_if_verbose(cx, &trace, None, l, u);

    // limit recursion depth
    recursion_check::check(cx, trace)?;

    // Check if this worker has been told to cancel (or per-file timeout exceeded)
    check_canceled(cx)?;

    // Expect that l is a def type. On the other hand, u may be a use type or a
    // def type: the latter typically when we have annotations.
    if match l.deref() {
        TypeInner::AnyT(_, _) => {
            // Either propagate AnyT through the use type, or short-circuit
            any_propagated(cx, trace, l, u)?
        }
        TypeInner::GenericT(box GenericTData {
            bound,
            name,
            reason,
            id,
            no_infer,
        }) => handle_generic(cx, trace, *no_infer, bound, reason, id, name, u)?,
        _ => false,
    } {
        // Either propagate AnyT through the def type, or short-circuit because l <: any trivially
        return Ok(None);
    }
    if match u.deref() {
        UseTInner::UseT(use_op, any) if matches!(any.deref(), TypeInner::AnyT(_, _)) => {
            any_propagated_use(cx, trace, use_op, any, l)?
        }
        _ => false,
    } {
        return Ok(None);
    }
    if match l.deref() {
        TypeInner::DefT(_, def_t) if matches!(def_t.deref(), DefTInner::EmptyT) => empty_success(u),
        _ => false,
    } {
        return Ok(None);
    }
    // START OF PATTERN MATCH
    match (l.deref(), u.deref()) {
        // ********
        // * eval *
        // ********
        (TypeInner::EvalT { id: id1, .. }, UseTInner::UseT(_, t2)) if matches!(t2.deref(), TypeInner::EvalT { id: id2, .. } if id1 == id2) =>
        {
            // if Context.is_verbose cx then prerr_endline "EvalT ~> EvalT fast path"
            if cx.is_verbose() {
                eprintln!("EvalT ~> EvalT fast path");
            }
        }
        (
            TypeInner::EvalT {
                type_: t,
                defer_use_t,
                id,
            },
            _,
        ) => {
            let TypeDestructorTInner(use_op_prime, reason, d) = defer_use_t.deref();
            let result = eval_helpers::mk_type_destructor(
                cx,
                trace,
                use_op_prime.dupe(),
                reason,
                t,
                d,
                id.dupe(),
            )?;
            // rec_flow(cx, trace, (&result, u))
            // Tail call: in OCaml this is the last call in the match arm
            return Ok(Some(TailCall::RecFlow(
                result,
                u.dupe(),
                DepthTrace::rec_trace(trace),
            )));
        }
        (_, UseTInner::UseT(use_op, eval_t))
            if let TypeInner::EvalT {
                type_: t,
                defer_use_t,
                id,
            } = eval_t.deref() =>
        {
            let TypeDestructorTInner(use_op_prime, reason, d) = defer_use_t.deref();
            let result = eval_helpers::mk_type_destructor(
                cx,
                trace,
                use_op_prime.dupe(),
                reason,
                t,
                d,
                id.dupe(),
            )?;
            // rec_flow(cx, trace, (&result, &UseT::new(UseTInner::ReposUseT(...))))
            // Tail call: in OCaml this is the last call in the match arm
            return Ok(Some(TailCall::RecFlow(
                result,
                UseT::new(UseTInner::ReposUseT(Box::new(ReposUseTData {
                    reason: reason.dupe(),
                    use_desc: false,
                    use_op: use_op.dupe(),
                    type_: l.dupe(),
                }))),
                DepthTrace::rec_trace(trace),
            )));
        }

        // ******************
        // * process X ~> Y *
        // ******************
        (TypeInner::OpenT(tvar1), UseTInner::UseT(use_op, t_upper))
            if let TypeInner::OpenT(tvar2) = t_upper.deref() =>
        {
            cx.add_array_or_object_literal_declaration_upper_bound(
                tvar1.id() as i32,
                Type::new(TypeInner::OpenT(tvar2.dupe())),
            );
            let (id1, constraints1) = cx.find_constraints(tvar1.id() as i32);
            let (id2, constraints2) = cx.find_constraints(tvar2.id() as i32);
            match (constraints1, constraints2) {
                (
                    constraint::Constraints::Unresolved(bounds1),
                    constraint::Constraints::Unresolved(bounds2),
                ) => {
                    if not_linked((id1, &bounds1), (id2, &bounds2)) {
                        let (lower, upper) = {
                            let bounds1 = bounds1.borrow();
                            let bounds2 = bounds2.borrow();
                            (bounds1.lower.clone(), bounds2.upper.clone())
                        };
                        add_upper_edges(
                            cx,
                            trace,
                            use_op.dupe(),
                            false,
                            (id1, &bounds1),
                            (id2, &bounds2),
                        );
                        add_lower_edges(
                            cx,
                            trace,
                            use_op.dupe(),
                            false,
                            (id1, &bounds1),
                            (id2, &bounds2),
                        );
                        flows_across(cx, trace, use_op.dupe(), &lower, &upper)?;
                    }
                }
                (
                    constraint::Constraints::Unresolved(bounds1),
                    constraint::Constraints::Resolved(t2),
                ) => {
                    let t2_use = flow_use_op(
                        cx,
                        unknown_use(),
                        UseT::new(UseTInner::UseT(use_op.dupe(), t2.dupe())),
                    );
                    edges_and_flows_to_t(cx, trace, false, (id1, &bounds1), &t2_use)?;
                }
                (
                    constraint::Constraints::Unresolved(bounds1),
                    constraint::Constraints::FullyResolved(s2),
                ) => {
                    let t2_use = flow_use_op(
                        cx,
                        unknown_use(),
                        UseT::new(UseTInner::UseT(
                            use_op.dupe(),
                            cx.force_fully_resolved_tvar(&s2),
                        )),
                    );
                    edges_and_flows_to_t(cx, trace, false, (id1, &bounds1), &t2_use)?;
                }
                (
                    constraint::Constraints::Resolved(t1),
                    constraint::Constraints::Unresolved(bounds2),
                ) => {
                    edges_and_flows_from_t(cx, trace, use_op.dupe(), false, &t1, (id2, &bounds2))?;
                }
                (
                    constraint::Constraints::FullyResolved(s1),
                    constraint::Constraints::Unresolved(bounds2),
                ) => {
                    edges_and_flows_from_t(
                        cx,
                        trace,
                        use_op.dupe(),
                        false,
                        &cx.force_fully_resolved_tvar(&s1),
                        (id2, &bounds2),
                    )?;
                }
                (constraint::Constraints::Resolved(t1), constraint::Constraints::Resolved(t2)) => {
                    let t2_use = flow_use_op(
                        cx,
                        unknown_use(),
                        UseT::new(UseTInner::UseT(use_op.dupe(), t2.dupe())),
                    );
                    rec_flow(cx, trace, (&t1, &t2_use))?;
                }
                (
                    constraint::Constraints::Resolved(t1),
                    constraint::Constraints::FullyResolved(s2),
                ) => {
                    let t2_use = flow_use_op(
                        cx,
                        unknown_use(),
                        UseT::new(UseTInner::UseT(
                            use_op.dupe(),
                            cx.force_fully_resolved_tvar(&s2),
                        )),
                    );
                    rec_flow(cx, trace, (&t1, &t2_use))?;
                }
                (
                    constraint::Constraints::FullyResolved(s1),
                    constraint::Constraints::Resolved(t2),
                ) => {
                    let t2_use = flow_use_op(
                        cx,
                        unknown_use(),
                        UseT::new(UseTInner::UseT(use_op.dupe(), t2.dupe())),
                    );
                    rec_flow(cx, trace, (&cx.force_fully_resolved_tvar(&s1), &t2_use))?;
                }
                (
                    constraint::Constraints::FullyResolved(s1),
                    constraint::Constraints::FullyResolved(s2),
                ) => {
                    let t2_use = flow_use_op(
                        cx,
                        unknown_use(),
                        UseT::new(UseTInner::UseT(
                            use_op.dupe(),
                            cx.force_fully_resolved_tvar(&s2),
                        )),
                    );
                    rec_flow(cx, trace, (&cx.force_fully_resolved_tvar(&s1), &t2_use))?;
                }
            }
        }
        // ******************
        // * process Y ~> U *
        // ******************
        (TypeInner::OpenT(tvar), _) => {
            let r = tvar.reason();
            if !match u.deref() {
                // We have some simple tvar id based concretization. Bad cyclic types can only
                // come from indirections through OpenT, most of them are already defended with
                // Flow_js_utils.InvalidCyclicTypeValidation and turned to any, but there are gaps
                // (especially EvalT from type sig), so we defend it again here.
                UseTInner::ConcretizeT(box ConcretizeTData { seen, .. }) => {
                    let tvar_id = tvar.id() as i32;
                    // ISet.mem tvar !seen
                    if seen.contains(&tvar_id) {
                        true
                    } else {
                        // seen := ISet.add tvar !seen;
                        seen.insert(tvar_id);
                        false
                    }
                }
                _ => false,
            } {
                match u.deref() {
                    UseTInner::UseT(_, t2) => {
                        cx.add_array_or_object_literal_declaration_upper_bound(
                            tvar.id() as i32,
                            t2.dupe(),
                        );
                    }
                    _ => {}
                }
                let u = match r.desc(true) {
                    VirtualReasonDesc::RTypeParam(..) => type_util::mod_use_op_of_use_t(
                        |op| {
                            UseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ImplicitTypeParam),
                                Arc::new(op.dupe()),
                            )
                        },
                        u,
                    ),
                    _ => u.dupe(),
                };
                let (id1, constraints1) = cx.find_constraints(tvar.id() as i32);
                match constraints1 {
                    constraint::Constraints::Unresolved(bounds1) => {
                        edges_and_flows_to_t(cx, trace, false, (id1, &bounds1), &u)?;
                    }
                    constraint::Constraints::Resolved(t1) => {
                        // rec_flow(cx, trace, (&t1, &u))
                        // Tail call: in OCaml this is the last call in the match arm
                        return Ok(Some(TailCall::RecFlow(t1, u, DepthTrace::rec_trace(trace))));
                    }
                    constraint::Constraints::FullyResolved(s1) => {
                        // rec_flow(cx, trace, (&cx.force_fully_resolved_tvar(&s1), &u))
                        // Tail call: in OCaml this is the last call in the match arm
                        return Ok(Some(TailCall::RecFlow(
                            cx.force_fully_resolved_tvar(&s1),
                            u,
                            DepthTrace::rec_trace(trace),
                        )));
                    }
                }
            }
        }
        // ******************
        // * process L ~> X *
        // ******************
        (_, UseTInner::UseT(use_op, t_open)) if let TypeInner::OpenT(tvar) = t_open.deref() => {
            let (id2, constraints2) = cx.find_constraints(tvar.id() as i32);
            match constraints2 {
                constraint::Constraints::Unresolved(bounds2) => {
                    edges_and_flows_from_t(cx, trace, use_op.dupe(), false, l, (id2, &bounds2))?;
                }
                constraint::Constraints::Resolved(t2) => {
                    // rec_flow(cx, trace, (l, &UseT::new(UseTInner::UseT(use_op.dupe(), t2.dupe()))))
                    // Tail call: in OCaml this is the last call in the match arm
                    return Ok(Some(TailCall::RecFlow(
                        l.dupe(),
                        UseT::new(UseTInner::UseT(use_op.dupe(), t2)),
                        DepthTrace::rec_trace(trace),
                    )));
                }
                constraint::Constraints::FullyResolved(s2) => {
                    // rec_flow(cx, trace, (l, &UseT::new(UseTInner::UseT(use_op.dupe(), ...))))
                    // Tail call: in OCaml this is the last call in the match arm
                    return Ok(Some(TailCall::RecFlow(
                        l.dupe(),
                        UseT::new(UseTInner::UseT(
                            use_op.dupe(),
                            cx.force_fully_resolved_tvar(&s2),
                        )),
                        DepthTrace::rec_trace(trace),
                    )));
                }
            }
        }
        // ************************
        // * Eval type destructor *
        // ************************
        (
            _,
            UseTInner::EvalTypeDestructorT(box EvalTypeDestructorTData {
                destructor_use_op,
                reason,
                repos,
                destructor,
                tout,
            }),
        ) => {
            let l_repos = match repos {
                None => l.dupe(),
                Some((reason, use_desc)) => {
                    helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?
                }
            };
            eval_destructor(
                cx,
                trace,
                destructor_use_op.dupe(),
                reason,
                &l_repos,
                destructor,
                tout,
            )?;
        }

        // *************
        // * Subtyping *
        // *************
        (_, UseTInner::UseT(use_op, u_type)) => {
            subtyping_kit::rec_sub_t(cx, use_op.dupe(), l, u_type, trace)?;
        }
        // For l.key !== sentinel when sentinel has a union type, don't split the union. This
        // prevents a drastic blowup of cases which can cause perf problems.
        (
            TypeInner::UnionT(_, _),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForSentinelPropTest,
                collector,
                ..
            }),
        )
        | (
            TypeInner::UnionT(_, _),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind:
                    ConcretizationKind::ConcretizeForPredicate(
                        PredicateConcretetizerVariant::ConcretizeRHSForLiteralPredicateTest,
                    ),
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }
        (
            TypeInner::UnionT(_, rep),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind:
                    ConcretizationKind::ConcretizeForPredicate(
                        PredicateConcretetizerVariant::ConcretizeKeepOptimizedUnions,
                    ),
                collector,
                ..
            }),
        ) if rep.is_optimized_finally() => {
            collector.add(l.dupe());
        }
        (
            TypeInner::UnionT(_, _),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForMatchArg { keep_unions: true },
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }
        (TypeInner::UnionT(_, urep), UseTInner::ConcretizeT(..)) => {
            flow_all_in_union(cx, trace, urep, u)?;
        }
        (TypeInner::MaybeT(lreason, t), UseTInner::ConcretizeT(..)) => {
            let lreason = lreason.dupe().replace_desc(VirtualReasonDesc::RNullOrVoid);
            rec_flow(cx, trace, (&null::make(lreason.dupe()), u))?;
            rec_flow(cx, trace, (&void::make(lreason), u))?;
            rec_flow(cx, trace, (t, u))?;
        }
        (
            TypeInner::OptionalT {
                reason: r,
                type_: t,
                use_desc,
            },
            UseTInner::ConcretizeT(..),
        ) => {
            rec_flow(
                cx,
                trace,
                (&void::why_with_use_desc(*use_desc, r.dupe()), u),
            )?;
            rec_flow(cx, trace, (t, u))?;
        }
        (TypeInner::AnnotT(r, t, use_desc), UseTInner::ConcretizeT(..)) => {
            // TODO: directly derive loc and desc from the reason of tvar
            let loc = r.loc().dupe();
            let desc = if *use_desc {
                Some(r.desc(true).clone())
            } else {
                None
            };
            let annot_loc = r.annot_loc().map(|l| l.dupe());
            let repos_t =
                helpers::reposition(cx, Some(trace), loc, desc.as_ref(), annot_loc, t.dupe())?;
            rec_flow(cx, trace, (&repos_t, u))?;
        }
        (TypeInner::DefT(reason, def_t), UseTInner::ConvertEmptyPropsToMixedT(_, tout))
            if matches!(def_t.deref(), DefTInner::EmptyT) =>
        {
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (&mixed_t::make(reason.dupe()), tout),
            )?;
        }
        (_, UseTInner::ConvertEmptyPropsToMixedT(_, tout)) => {
            rec_flow_t(cx, trace, unknown_use(), (l, tout))?;
        }
        // ***************
        // * annotations *
        // ***************

        // Special cases where we want to recursively concretize types within the lower bound.
        (
            TypeInner::UnionT(r, rep),
            UseTInner::ReposUseT(box ReposUseTData {
                reason,
                use_desc,
                use_op,
                type_: l_inner,
            }),
        ) => {
            let rep = {
                let in_implicit = cx.in_implicit_instantiation();
                rep.ident_map(false, |t| annot(in_implicit, *use_desc, t))
            };
            let loc = reason.loc().dupe();
            let annot_loc = reason.annot_loc().map(|l| l.dupe());
            let r = r.dupe().reposition(loc.dupe()).opt_annotate(annot_loc);
            let r = if *use_desc {
                r.replace_desc(reason.desc(true).clone())
            } else {
                r
            };
            rec_flow(
                cx,
                trace,
                (
                    l_inner,
                    &UseT::new(UseTInner::UseT(
                        use_op.dupe(),
                        Type::new(TypeInner::UnionT(r, rep)),
                    )),
                ),
            )?;
        }
        (
            TypeInner::MaybeT(r, u_inner),
            UseTInner::ReposUseT(box ReposUseTData {
                reason,
                use_desc,
                use_op,
                type_: l_inner,
            }),
        ) => {
            let loc = reason.loc().dupe();
            let annot_loc = reason.annot_loc().map(|l| l.dupe());
            let r = r.dupe().reposition(loc.dupe()).opt_annotate(annot_loc);
            let r = if *use_desc {
                r.replace_desc(reason.desc(true).clone())
            } else {
                r
            };
            let annoted_u = annot(cx.in_implicit_instantiation(), *use_desc, u_inner);
            rec_flow(
                cx,
                trace,
                (
                    l_inner,
                    &UseT::new(UseTInner::UseT(
                        use_op.dupe(),
                        Type::new(TypeInner::MaybeT(r, annoted_u)),
                    )),
                ),
            )?;
        }
        (
            TypeInner::OptionalT {
                reason: r,
                type_: u_inner,
                use_desc: use_desc_optional_t,
            },
            UseTInner::ReposUseT(box ReposUseTData {
                reason,
                use_desc,
                use_op,
                type_: l_inner,
            }),
        ) => {
            let loc = reason.loc().dupe();
            let annot_loc = reason.annot_loc().map(|l| l.dupe());
            let r = r.dupe().reposition(loc.dupe()).opt_annotate(annot_loc);
            let r = if *use_desc {
                r.replace_desc(reason.desc(true).clone())
            } else {
                r
            };
            let annoted_u = annot(cx.in_implicit_instantiation(), *use_desc, u_inner);
            rec_flow(
                cx,
                trace,
                (
                    l_inner,
                    &UseT::new(UseTInner::UseT(
                        use_op.dupe(),
                        Type::new(TypeInner::OptionalT {
                            reason: r,
                            type_: annoted_u,
                            use_desc: *use_desc_optional_t,
                        }),
                    )),
                ),
            )?;
        }
        (
            TypeInner::DefT(r, def_t),
            UseTInner::ReposUseT(box ReposUseTData {
                reason,
                use_desc,
                use_op,
                type_: l_inner,
            }),
        ) if let DefTInner::RendersT(renders_form) = def_t.deref()
            && let CanonicalRendersForm::StructuralRenders {
                renders_variant,
                renders_structural_type,
            } = renders_form.as_ref()
            && let TypeInner::UnionT(_, rep) = renders_structural_type.deref() =>
        {
            let rep = {
                let in_implicit = cx.in_implicit_instantiation();
                rep.ident_map(false, |t| annot(in_implicit, *use_desc, t))
            };
            let loc = reason.loc().dupe();
            let annot_loc = reason.annot_loc().map(|l| l.dupe());
            let r = r.dupe().reposition(loc.dupe()).opt_annotate(annot_loc);
            let r = if *use_desc {
                r.replace_desc(reason.desc(true).clone())
            } else {
                r
            };
            rec_flow(
                cx,
                trace,
                (
                    l_inner,
                    &UseT::new(UseTInner::UseT(
                        use_op.dupe(),
                        Type::new(TypeInner::DefT(
                            r.dupe(),
                            DefT::new(DefTInner::RendersT(Rc::new(
                                CanonicalRendersForm::StructuralRenders {
                                    renders_variant: *renders_variant,
                                    renders_structural_type: Type::new(TypeInner::UnionT(r, rep)),
                                },
                            ))),
                        )),
                    )),
                ),
            )?;
        }
        // Waits for a def type to become concrete, repositions it as an upper UseT<Context<'cx>>
        // using the stored reason. This can be used to store a reason as it flows
        // through a tvar. *)
        (
            _,
            UseTInner::ReposUseT(box ReposUseTData {
                reason,
                use_desc,
                use_op,
                type_: l_inner,
            }),
        ) => {
            let u_repos = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(
                cx,
                trace,
                (l_inner, &UseT::new(UseTInner::UseT(use_op.dupe(), u_repos))),
            )?;
        }

        // The source component of an annotation flows out of the annotated
        // site to downstream uses.
        (TypeInner::AnnotT(r, t, use_desc), _) => {
            let t = helpers::reposition_reason(cx, Some(trace), r, *use_desc, t)?;
            rec_flow(cx, trace, (&t, u))?;
        }

        // ******************
        // * Module exports *
        // ******************
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForCJSExtractNamedExportsAndTypeExports,
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }
        // ***************************
        // * optional indexed access *
        // ***************************
        (
            TypeInner::DefT(r, def_t),
            UseTInner::OptionalIndexedAccessT(box OptionalIndexedAccessTData {
                use_op,
                tout_tvar,
                ..
            }),
        ) if matches!(
            def_t.deref(),
            DefTInner::EmptyT | DefTInner::VoidT | DefTInner::NullT
        ) =>
        {
            rec_flow_t(
                cx,
                trace,
                use_op.dupe(),
                (
                    &empty_t::why(r.dupe()),
                    &Type::new(TypeInner::OpenT((**tout_tvar).dupe())),
                ),
            )?;
        }
        (TypeInner::MaybeT(_, t), UseTInner::OptionalIndexedAccessT(..))
        | (TypeInner::OptionalT { type_: t, .. }, UseTInner::OptionalIndexedAccessT(..)) => {
            rec_flow(cx, trace, (t, u))?;
        }
        (
            TypeInner::UnionT(_, rep),
            UseTInner::OptionalIndexedAccessT(box OptionalIndexedAccessTData {
                use_op,
                reason,
                index,
                tout_tvar,
            }),
        ) => {
            let mut members = rep.members_iter();
            let t0 = members.next().unwrap();
            let t1 = members.next().unwrap();
            let ts: Vec<&Type> = members.collect();
            let f = |t: &Type| -> Result<Type, FlowJsException> {
                flow_typing_tvar::mk_no_wrap_where(cx, reason.dupe(), |cx, tvar_reason, tvar_id| {
                    let tvar = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                    rec_flow(
                        cx,
                        trace,
                        (
                            t,
                            &UseT::new(UseTInner::OptionalIndexedAccessT(Box::new(
                                OptionalIndexedAccessTData {
                                    use_op: use_op.dupe(),
                                    reason: reason.dupe(),
                                    index: index.dupe(),
                                    tout_tvar: Box::new(tvar),
                                },
                            ))),
                        ),
                    )?;
                    Ok(())
                })
            };
            let ft0 = f(t0)?;
            let ft1 = f(t1)?;
            let fts: Rc<[Type]> = ts
                .iter()
                .map(|t| f(t))
                .collect::<Result<Vec<_>, _>>()?
                .into();
            let rep = union_rep::make(None, union_rep::UnionKind::UnknownKind, ft0, ft1, fts);
            rec_unify(
                cx,
                trace,
                unknown_use(),
                UnifyCause::Uncategorized,
                None,
                &Type::new(TypeInner::UnionT(reason.dupe(), rep)),
                &Type::new(TypeInner::OpenT((**tout_tvar).dupe())),
            )?;
        }
        (
            _,
            UseTInner::OptionalIndexedAccessT(box OptionalIndexedAccessTData {
                use_op,
                reason,
                index,
                tout_tvar,
            }),
        ) if !matches!(l.deref(), TypeInner::IntersectionT(_, _)) => {
            let u_new = match index {
                OptionalIndexedAccessIndex::OptionalIndexedAccessStrLitIndex(name) => {
                    let reason_op = reason
                        .dupe()
                        .replace_desc(VirtualReasonDesc::RProperty(Some(name.dupe())));
                    UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        id: None,
                        from_annot: true,
                        skip_optional: false,
                        propref: Box::new(mk_named_prop(reason_op, true, name.dupe())),
                        tout: tout_tvar.clone(),
                        hint: hint_unavailable(),
                    })))
                }
                OptionalIndexedAccessIndex::OptionalIndexedAccessTypeIndex(key_t) => {
                    UseT::new(UseTInner::GetElemT(Box::new(GetElemTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        id: None,
                        from_annot: true,
                        skip_optional: false,
                        access_iterables: false,
                        key_t: key_t.dupe(),
                        tout: tout_tvar.clone(),
                    })))
                }
            };
            rec_flow(cx, trace, (l, &u_new))?;
        }
        // ********************
        // * DRO and hooklike *
        // ********************
        (
            TypeInner::OptionalT {
                reason,
                type_,
                use_desc,
            },
            UseTInner::DeepReadOnlyT(tout, dro),
        ) => {
            let r = tout.reason();
            let ReactDro(dro_loc, dro_type) = dro;
            let new_type_ =
                flow_typing_tvar::mk_no_wrap_where(cx, r.dupe(), |cx, tvar_reason, tvar_id| {
                    let tvar = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                    // rec_flow cx trace (type_, DeepReadOnlyT (tvar, (dro_loc, dro_type)))
                    rec_flow(
                        cx,
                        trace,
                        (
                            type_,
                            &UseT::new(UseTInner::DeepReadOnlyT(
                                Box::new(tvar),
                                ReactDro(dro_loc.dupe(), dro_type.clone()),
                            )),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                })?;
            let new_l = Type::new(TypeInner::OptionalT {
                reason: reason.dupe(),
                type_: new_type_,
                use_desc: *use_desc,
            });
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (
            TypeInner::OptionalT {
                reason,
                type_,
                use_desc,
            },
            UseTInner::HooklikeT(tout),
        ) => {
            let r = tout.reason();
            let new_type_ =
                flow_typing_tvar::mk_no_wrap_where(cx, r.dupe(), |cx, tvar_reason, tvar_id| {
                    let tvar = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                    // rec_flow cx trace (type_, HooklikeT tvar)
                    rec_flow(
                        cx,
                        trace,
                        (type_, &UseT::new(UseTInner::HooklikeT(Box::new(tvar)))),
                    )?;
                    Ok::<(), FlowJsException>(())
                })?;
            let new_l = Type::new(TypeInner::OptionalT {
                reason: reason.dupe(),
                type_: new_type_,
                use_desc: *use_desc,
            });
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::MaybeT(rl, t), UseTInner::DeepReadOnlyT(tout, dro)) => {
            let r = tout.reason();
            let ReactDro(dro_loc, dro_type) = dro;
            let new_t =
                flow_typing_tvar::mk_no_wrap_where(cx, r.dupe(), |cx, tvar_reason, tvar_id| {
                    let tvar = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                    rec_flow(
                        cx,
                        trace,
                        (
                            t,
                            &UseT::new(UseTInner::DeepReadOnlyT(
                                Box::new(tvar),
                                ReactDro(dro_loc.dupe(), dro_type.clone()),
                            )),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                })?;
            let new_l = Type::new(TypeInner::MaybeT(rl.dupe(), new_t));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::MaybeT(rl, t), UseTInner::HooklikeT(tout)) => {
            let r = tout.reason();
            let new_t =
                flow_typing_tvar::mk_no_wrap_where(cx, r.dupe(), |cx, tvar_reason, tvar_id| {
                    let tvar = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                    rec_flow(
                        cx,
                        trace,
                        (t, &UseT::new(UseTInner::HooklikeT(Box::new(tvar)))),
                    )?;
                    Ok::<(), FlowJsException>(())
                })?;
            let new_l = Type::new(TypeInner::MaybeT(rl.dupe(), new_t));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::UnionT(reason, rep), UseTInner::DeepReadOnlyT(tout, dro)) => {
            let ReactDro(dro_loc, dro_type) = dro;
            if !rep.is_optimized_finally() {
                rep.optimize_enum_only(|ts| {
                    flow_typing_visitors::type_mapper::union_flatten(cx, ts.duped())
                });
            }
            if rep.check_enum().is_some() {
                let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
                rec_flow_t(cx, trace, unknown_use(), (l, &tout_t))?;
            } else {
                let dro_loc = dro_loc.dupe();
                let dro_type = dro_type.clone();
                let dro_union = flow_js_utils::map_union(
                    |cx, trace, t, tout| {
                        let tout_tvar = open_tvar(&tout);
                        rec_flow(
                            cx,
                            *trace,
                            (
                                t,
                                &UseT::new(UseTInner::DeepReadOnlyT(
                                    Box::new(tout_tvar.dupe()),
                                    ReactDro(dro_loc.dupe(), dro_type.clone()),
                                )),
                            ),
                        )
                    },
                    cx,
                    &trace,
                    rep,
                    reason.dupe(),
                )?;
                let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
                rec_flow_t(cx, trace, unknown_use(), (&dro_union, &tout_t))?;
            }
        }
        (TypeInner::UnionT(reason, rep), UseTInner::HooklikeT(tout)) => {
            if !rep.is_optimized_finally() {
                rep.optimize_enum_only(|ts| {
                    flow_typing_visitors::type_mapper::union_flatten(cx, ts.duped())
                });
            }
            if rep.check_enum().is_some() {
                let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
                rec_flow_t(cx, trace, unknown_use(), (l, &tout_t))?;
            } else {
                let hook_union = flow_js_utils::map_union(
                    |cx, trace, t, tout| {
                        let tout_tvar = open_tvar(&tout);
                        rec_flow(
                            cx,
                            *trace,
                            (
                                t,
                                &UseT::new(UseTInner::HooklikeT(Box::new(tout_tvar.dupe()))),
                            ),
                        )
                    },
                    cx,
                    &trace,
                    rep,
                    reason.dupe(),
                )?;
                let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
                rec_flow_t(cx, trace, unknown_use(), (&hook_union, &tout_t))?;
            }
        }
        (TypeInner::IntersectionT(reason, rep), UseTInner::DeepReadOnlyT(tout, dro)) => {
            let ReactDro(dro_loc, dro_type) = dro;
            let dro_loc = dro_loc.dupe();
            let dro_type = dro_type.clone();
            let dro_inter = flow_js_utils::map_inter(
                |cx, trace, t, tout| {
                    let tout_tvar = open_tvar(&tout);
                    rec_flow(
                        cx,
                        *trace,
                        (
                            t,
                            &UseT::new(UseTInner::DeepReadOnlyT(
                                Box::new(tout_tvar.dupe()),
                                ReactDro(dro_loc.dupe(), dro_type.clone()),
                            )),
                        ),
                    )
                },
                cx,
                &trace,
                rep,
                reason.dupe(),
            )?;
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&dro_inter, &tout_t))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::DeepReadOnlyT(tout, dro))
            if let DefTInner::ObjT(o) = def_t.deref() =>
        {
            let ReactDro(dro_loc, dro_type) = dro;
            let new_flags = Flags {
                react_dro: Some(ReactDro(dro_loc.dupe(), dro_type.clone())),
                ..o.flags.clone()
            };
            let new_obj = ObjType {
                flags: new_flags,
                ..(**o).clone()
            };
            let new_l = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::ObjT(Rc::new(new_obj))),
            ));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::DeepReadOnlyT(tout, dro))
            if let DefTInner::ArrT(arr) = def_t.deref()
                && let ArrType::TupleAT(box TupleATData {
                    elem_t,
                    elements,
                    arity,
                    inexact,
                    react_dro: _,
                }) = arr.as_ref() =>
        {
            let ReactDro(dro_loc, dro_type) = dro;
            let new_l = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::ArrT(Rc::new(ArrType::TupleAT(Box::new(
                    TupleATData {
                        react_dro: Some(ReactDro(dro_loc.dupe(), dro_type.clone())),
                        elem_t: elem_t.dupe(),
                        elements: elements.dupe(),
                        arity: *arity,
                        inexact: *inexact,
                    },
                ))))),
            ));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::DeepReadOnlyT(tout, dro))
            if let DefTInner::ArrT(arr) = def_t.deref()
                && let ArrType::ArrayAT(box ArrayATData {
                    elem_t,
                    tuple_view,
                    react_dro: _,
                }) = arr.as_ref() =>
        {
            let ReactDro(dro_loc, dro_type) = dro;
            let new_l = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                    ArrayATData {
                        react_dro: Some(ReactDro(dro_loc.dupe(), dro_type.clone())),
                        elem_t: elem_t.dupe(),
                        tuple_view: tuple_view.clone(),
                    },
                ))))),
            ));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::DeepReadOnlyT(tout, dro))
            if let DefTInner::ArrT(arr) = def_t.deref()
                && let ArrType::ROArrayAT(box (t, _)) = arr.as_ref() =>
        {
            let ReactDro(dro_loc, dro_type) = dro;
            let new_l = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ROArrayAT(Box::new((
                    t.dupe(),
                    Some(ReactDro(dro_loc.dupe(), dro_type.clone())),
                )))))),
            ));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::DeepReadOnlyT(tout, react_dro))
            if let DefTInner::InstanceT(instance) = def_t.deref() =>
        {
            let new_inst = InstType::new(InstTypeInner {
                inst_react_dro: Some(react_dro.clone()),
                ..instance.inst.deref().clone()
            });
            let new_instance = InstanceT::new(InstanceTInner {
                inst: new_inst,
                ..instance.deref().deref().clone()
            });
            let new_l = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::InstanceT(Rc::new(new_instance))),
            ));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::HooklikeT(tout))
            if let DefTInner::FunT(s, funtype) = def_t.deref()
                && funtype.effect_ == ReactEffectType::ArbitraryEffect =>
        {
            let new_funtype = FunType {
                effect_: ReactEffectType::AnyEffect,
                ..(**funtype).clone()
            };
            let new_l = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::FunT(s.dupe(), Rc::new(new_funtype))),
            ));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::DefT(rp, def_t), UseTInner::HooklikeT(tout))
            if let DefTInner::PolyT(box PolyTData {
                tparams_loc,
                tparams,
                t_out,
                id,
            }) = def_t.deref()
                && let TypeInner::DefT(r, inner_def) = t_out.deref()
                && let DefTInner::FunT(s, funtype) = inner_def.deref()
                && funtype.effect_ == ReactEffectType::ArbitraryEffect =>
        {
            let new_funtype = FunType {
                effect_: ReactEffectType::AnyEffect,
                ..(**funtype).clone()
            };
            let new_t_out = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::FunT(s.dupe(), Rc::new(new_funtype))),
            ));
            let new_l = Type::new(TypeInner::DefT(
                rp.dupe(),
                DefT::new(DefTInner::PolyT(Box::new(PolyTData {
                    tparams_loc: tparams_loc.dupe(),
                    tparams: tparams.dupe(),
                    t_out: new_t_out,
                    id: id.dupe(),
                }))),
            ));
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&new_l, &tout_t))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::HooklikeT(tout))
            if let DefTInner::ObjT(obj) = def_t.deref()
                && let Some(id) = &obj.call_t =>
        {
            let call_t = cx.find_call(*id);
            let t = match call_t.deref() {
                // | DefT (rf, FunT (s, ({ effect_ = ArbitraryEffect; _ } as funtype))) ->
                TypeInner::DefT(rf, inner_def)
                    if let DefTInner::FunT(s, funtype) = inner_def.deref()
                        && funtype.effect_ == ReactEffectType::ArbitraryEffect =>
                {
                    let call = Type::new(TypeInner::DefT(
                        rf.dupe(),
                        DefT::new(DefTInner::FunT(
                            s.dupe(),
                            Rc::new(FunType {
                                effect_: ReactEffectType::AnyEffect,
                                ..(**funtype).clone()
                            }),
                        )),
                    ));
                    let new_id = cx.make_call_prop(call);
                    Type::new(TypeInner::DefT(
                        r.dupe(),
                        DefT::new(DefTInner::ObjT(Rc::new(ObjType {
                            call_t: Some(new_id),
                            ..(**obj).clone()
                        }))),
                    ))
                }
                // | DefT (rp, PolyT ({ t_out = DefT (rf, FunT (s, ({ effect_ = ArbitraryEffect; _ } as funtype))); _ } as poly)) ->
                TypeInner::DefT(rp, inner_def)
                    if let DefTInner::PolyT(box PolyTData {
                        tparams_loc,
                        tparams,
                        t_out,
                        id: poly_id,
                    }) = inner_def.deref()
                        && let TypeInner::DefT(rf, fun_def) = t_out.deref()
                        && let DefTInner::FunT(s, funtype) = fun_def.deref()
                        && funtype.effect_ == ReactEffectType::ArbitraryEffect =>
                {
                    let new_funtype = FunType {
                        effect_: ReactEffectType::AnyEffect,
                        ..(**funtype).clone()
                    };
                    let new_t_out = Type::new(TypeInner::DefT(
                        rf.dupe(),
                        DefT::new(DefTInner::FunT(s.dupe(), Rc::new(new_funtype))),
                    ));
                    let call = Type::new(TypeInner::DefT(
                        rp.dupe(),
                        DefT::new(DefTInner::PolyT(Box::new(PolyTData {
                            tparams_loc: tparams_loc.dupe(),
                            tparams: tparams.dupe(),
                            t_out: new_t_out,
                            id: poly_id.dupe(),
                        }))),
                    ));
                    let new_id = cx.make_call_prop(call);
                    Type::new(TypeInner::DefT(
                        r.dupe(),
                        DefT::new(DefTInner::ObjT(Rc::new(ObjType {
                            call_t: Some(new_id),
                            ..(**obj).clone()
                        }))),
                    ))
                }
                _ => l.dupe(),
            };
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&t, &tout_t))?;
        }
        (
            TypeInner::IntersectionT(_, _) | TypeInner::NominalT { .. },
            UseTInner::DeepReadOnlyT(tout, _) | UseTInner::HooklikeT(tout),
        ) => {
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (l, &tout_t))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::DeepReadOnlyT(tout, _) | UseTInner::HooklikeT(tout),
        ) if matches!(def_t.deref(), DefTInner::PolyT(box _)) => {
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (l, &tout_t))?;
        }
        (
            TypeInner::DefT(r, def_t),
            UseTInner::GetKeysT { .. }
            | UseTInner::GetValuesT { .. }
            | UseTInner::GetDictValuesT { .. }
            | UseTInner::CallT(..)
            | UseTInner::LookupT(..)
            | UseTInner::SetPropT(..)
            | UseTInner::GetPropT(..)
            | UseTInner::MethodT(box MethodTData {
                use_op: _,
                reason: _,
                prop_reason: _,
                propref: _,
                method_action: _,
            })
            | UseTInner::ObjRestT(_, _, _, _)
            | UseTInner::SetElemT(..)
            | UseTInner::GetElemT(..)
            | UseTInner::CallElemT(..)
            | UseTInner::BindT(..),
        ) if let DefTInner::InstanceT(instance) = def_t.deref()
            && let inst = &instance.inst
            && let Some(ReactDro(dro_loc, dro_type)) = inst.inst_react_dro.as_ref()
            && inst.type_args.len() == 2
            && flow_js_utils::is_builtin_class_id("Map", inst.class_id.dupe(), cx) =>
        {
            let (_, _, key_t, _) = &inst.type_args[0];
            let (_, _, val_t, _) = &inst.type_args[1];

            match u.deref() {
                UseTInner::MethodT(box MethodTData {
                    use_op,
                    reason: _,
                    prop_reason: reason,
                    propref: box PropRef::Named { name, .. },
                    method_action: _,
                }) if matches!(name.as_str(), "clear" | "delete" | "set") => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotReadable(Box::new(EPropNotReadableData {
                            reason_prop: reason.dupe(),
                            prop_name: Some(name.dupe()),
                            use_op: UseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(use_op.dupe()),
                            ),
                        })),
                    )?;
                }
                UseTInner::GetPropT(box data)
                    if let PropRef::Named { name, .. } = data.propref.as_ref()
                        && matches!(name.as_str(), "clear" | "delete" | "set") =>
                {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotReadable(Box::new(EPropNotReadableData {
                            reason_prop: data.reason.dupe(),
                            prop_name: Some(name.dupe()),
                            use_op: UseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(data.use_op.dupe()),
                            ),
                        })),
                    )?;
                }
                _ => {
                    let key_t = mk_react_dro(
                        cx,
                        unknown_use(),
                        ReactDro(dro_loc.dupe(), dro_type.clone()),
                        key_t.dupe(),
                    );
                    let val_t = mk_react_dro(
                        cx,
                        unknown_use(),
                        ReactDro(dro_loc.dupe(), dro_type.clone()),
                        val_t.dupe(),
                    );
                    let ro_map =
                        get_builtin_typeapp(cx, r, Some(true), "$ReadOnlyMap", vec![key_t, val_t]);
                    let dro_loc = dro_loc.dupe();
                    let dro_type = dro_type.clone();
                    let new_u = type_util::mod_use_op_of_use_t(
                        |use_op: &UseOp| {
                            UseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(use_op.dupe()),
                            )
                        },
                        u,
                    );
                    rec_flow(cx, trace, (&ro_map, &new_u))?;
                }
            }
        }
        (
            TypeInner::DefT(r, def_t),
            UseTInner::GetKeysT { .. }
            | UseTInner::GetValuesT { .. }
            | UseTInner::GetDictValuesT { .. }
            | UseTInner::CallT(..)
            | UseTInner::LookupT(..)
            | UseTInner::SetPropT(..)
            | UseTInner::GetPropT(..)
            | UseTInner::MethodT(box MethodTData {
                use_op: _,
                reason: _,
                prop_reason: _,
                propref: _,
                method_action: _,
            })
            | UseTInner::ObjRestT(_, _, _, _)
            | UseTInner::SetElemT(..)
            | UseTInner::GetElemT(..)
            | UseTInner::CallElemT(..)
            | UseTInner::BindT(..),
        ) if let DefTInner::InstanceT(instance) = def_t.deref()
            && let inst = &instance.inst
            && let Some(ReactDro(dro_loc, dro_type)) = inst.inst_react_dro.as_ref()
            && inst.type_args.len() == 1
            && flow_js_utils::is_builtin_class_id("Set", inst.class_id.dupe(), cx) =>
        {
            let (_, _, elem_t, _) = &inst.type_args[0];
            match u.deref() {
                UseTInner::MethodT(box MethodTData {
                    use_op,
                    reason: _,
                    prop_reason: reason,
                    propref: box PropRef::Named { name, .. },
                    method_action: _,
                }) if matches!(name.as_str(), "add" | "clear" | "delete") => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotReadable(Box::new(EPropNotReadableData {
                            reason_prop: reason.dupe(),
                            prop_name: Some(name.dupe()),
                            use_op: UseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(use_op.dupe()),
                            ),
                        })),
                    )?;
                }
                UseTInner::GetPropT(box data)
                    if let PropRef::Named { name, .. } = data.propref.as_ref()
                        && matches!(name.as_str(), "add" | "clear" | "delete") =>
                {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotReadable(Box::new(EPropNotReadableData {
                            reason_prop: data.reason.dupe(),
                            prop_name: Some(name.dupe()),
                            use_op: UseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(data.use_op.dupe()),
                            ),
                        })),
                    )?;
                }
                _ => {
                    let elem_t = mk_react_dro(
                        cx,
                        unknown_use(),
                        ReactDro(dro_loc.dupe(), dro_type.clone()),
                        elem_t.dupe(),
                    );
                    let ro_set =
                        get_builtin_typeapp(cx, r, Some(true), "$ReadOnlySet", vec![elem_t]);
                    let dro_loc = dro_loc.dupe();
                    let dro_type = dro_type.clone();
                    let new_u = type_util::mod_use_op_of_use_t(
                        |use_op: &UseOp| {
                            UseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(use_op.dupe()),
                            )
                        },
                        u,
                    );
                    rec_flow(cx, trace, (&ro_set, &new_u))?;
                }
            }
        }
        // ***************
        // * maybe types *
        // ***************

        // The type maybe(T) is the same as null | undefined | UseT<Context<'cx>>
        (TypeInner::DefT(r, def_t), UseTInner::FilterMaybeT(use_op, tout))
            if matches!(def_t.deref(), DefTInner::NullT | DefTInner::VoidT) =>
        {
            let empty = empty_t::why(r.dupe());
            rec_flow_t(cx, trace, use_op.dupe(), (&empty, tout))?;
        }
        (TypeInner::DefT(r, def_t), UseTInner::FilterMaybeT(use_op, tout))
            if matches!(
                def_t.deref(),
                DefTInner::MixedT(MixedFlavor::MixedEverything)
            ) =>
        {
            let mixed = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::MixedT(MixedFlavor::MixedNonMaybe)),
            ));
            rec_flow_t(cx, trace, use_op.dupe(), (&mixed, tout))?;
        }
        (TypeInner::OptionalT { type_: tout, .. }, UseTInner::FilterMaybeT(_, _))
        | (TypeInner::MaybeT(_, tout), UseTInner::FilterMaybeT(_, _)) => {
            rec_flow(cx, trace, (tout, u))?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::FilterMaybeT(use_op, tout))
            if matches!(def_t.deref(), DefTInner::EmptyT) =>
        {
            rec_flow_t(cx, trace, use_op.dupe(), (l, tout))?;
        }
        (
            TypeInner::MaybeT(_, _),
            UseTInner::ReposLowerT {
                reason: reason_op,
                use_desc,
                use_t: inner_u,
            },
        ) => {
            // Don't split the maybe type into its constituent members. Instead,
            // reposition the entire maybe type. *)
            let loc = reason_op.loc().dupe();
            let desc = if *use_desc {
                Some(reason_op.desc(true).clone())
            } else {
                None
            };
            let repos = helpers::reposition(cx, Some(trace), loc, desc.as_ref(), None, l.dupe())?;
            rec_flow(cx, trace, (&repos, inner_u))?;
        }
        (
            TypeInner::MaybeT(_, _),
            UseTInner::ResolveUnionT(box ResolveUnionTData {
                reason,
                resolved,
                unresolved,
                upper,
                id,
            }),
        ) => {
            if let Some((tc_l, tc_u)) =
                resolve_union(cx, trace, reason, *id, resolved, unresolved, l, upper)?
            {
                return Ok(Some(TailCall::Flow(tc_l, tc_u)));
            }
        }
        (TypeInner::MaybeT(reason, t), _)
            if match u.deref() {
                UseTInner::ConditionalT(box ConditionalTData {
                    distributive_tparam_name,
                    ..
                }) => distributive_tparam_name.is_some(),
                _ => true,
            } =>
        {
            let reason = reason.dupe().replace_desc(VirtualReasonDesc::RNullOrVoid);
            let t = type_util::push_type_alias_reason(&reason, t.dupe());
            rec_flow(cx, trace, (&null::make(reason.dupe()), u))?;
            rec_flow(cx, trace, (&void::make(reason), u))?;
            rec_flow(cx, trace, (&t, u))?;
        }

        // ******************
        // * optional types *
        // ******************

        // The type optional(T) is the same as undefined | UseT<Context<'cx>>
        (TypeInner::DefT(r, def_t), UseTInner::FilterOptionalT(use_op, tout))
            if matches!(def_t.deref(), DefTInner::VoidT) =>
        {
            let empty = empty_t::why(r.dupe());
            rec_flow_t(cx, trace, use_op.dupe(), (&empty, tout))?;
        }
        (TypeInner::OptionalT { type_: tout, .. }, UseTInner::FilterOptionalT(_, _)) => {
            rec_flow(cx, trace, (tout, u))?;
        }
        (
            TypeInner::OptionalT { .. },
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: inner_u,
            },
        ) => {
            // Don't split the optional type into its constituent members. Instead,
            // reposition the entire optional type.
            let repos = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos, inner_u))?;
        }
        (
            TypeInner::OptionalT { .. },
            UseTInner::ResolveUnionT(box ResolveUnionTData {
                reason,
                resolved,
                unresolved,
                upper,
                id,
            }),
        ) => {
            if let Some((tc_l, tc_u)) =
                resolve_union(cx, trace, reason, *id, resolved, unresolved, l, upper)?
            {
                return Ok(Some(TailCall::Flow(tc_l, tc_u)));
            }
        }
        (
            TypeInner::OptionalT {
                reason: r,
                type_: t,
                use_desc,
            },
            _,
        ) if match u.deref() {
            UseTInner::ConditionalT(box ConditionalTData {
                distributive_tparam_name,
                ..
            }) => distributive_tparam_name.is_some(),
            _ => true,
        } =>
        {
            let void_t = void::why_with_use_desc(*use_desc, r.dupe());
            rec_flow(cx, trace, (&void_t, u))?;
            rec_flow(cx, trace, (t, u))?;
        }
        // *********************
        // * type applications *
        // *********************
        (
            TypeInner::ThisTypeAppT(box ThisTypeAppTData {
                reason: reason_tapp,
                this_t: this,
                type_: c,
                targs: ts,
            }),
            _,
        ) => {
            let reason_op = reason_of_use_t(u);
            FlowJs::instantiate_this_class(
                cx,
                trace,
                reason_op,
                reason_tapp,
                c,
                ts.dupe(),
                this,
                &Cont::Upper(Box::new(u.dupe())),
            )?;
        }
        (
            TypeInner::TypeAppT(..),
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: inner_u,
            },
        ) => {
            let repos = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos, inner_u))?;
        }
        (
            TypeInner::TypeAppT(box TypeAppTData {
                reason: reason_tapp,
                use_op,
                type_,
                targs,
                from_value,
                use_desc,
            }),
            UseTInner::MethodT(box MethodTData {
                use_op: _,
                reason: _,
                prop_reason: _,
                propref: _,
                method_action: _,
            })
            | UseTInner::PrivateMethodT(..),
        ) => {
            let reason_op = reason_of_use_t(u);
            let t = mk_typeapp_instance_annot(
                cx,
                Some(trace),
                use_op.dupe(),
                reason_op,
                reason_tapp,
                *from_value,
                Some(*use_desc),
                type_,
                targs.dupe(),
            )?;
            rec_flow(cx, trace, (&t, u))?;
        }
        // This is the second step in checking a TypeAppT (c, ts) ~> TypeAppT (c, ts).
        // The first step is in subtyping_kit.ml, and concretizes the c for our
        // upper bound TypeAppT.
        //
        // When we have done that, then we want to concretize the lower bound. We
        // flip all our arguments to ConcretizeTypeAppsT and set the final element
        // to false to signal that we have concretized the upper bound's c.
        //
        // If the upper bound's c is not a PolyT then we will fall down to an
        // incompatible use error.
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ConcretizeTypeAppsT(
                use_op,
                box (ts2, fv2, op2, r2),
                box (c1, ts1, fv1, op1, r1),
                true,
            ),
        ) if matches!(def_t.deref(), DefTInner::PolyT(box _)) => {
            let c2 = l.dupe();
            rec_flow(
                cx,
                trace,
                (
                    c1,
                    &UseT::new(UseTInner::ConcretizeTypeAppsT(
                        use_op.dupe(),
                        Box::new((ts1.dupe(), *fv1, op1.dupe(), r1.dupe())),
                        Box::new((c2, ts2.dupe(), *fv2, op2.dupe(), r2.dupe())),
                        false,
                    )),
                ),
            )?;
        }
        // When we have concretized the c for our lower bound TypeAppT then we can
        // finally run our TypeAppT ~> TypeAppT logic. If we have referentially the
        // same PolyT for each TypeAppT then we want to check the type arguments
        // only. (Checked in the when condition.) If we do not have the same PolyT
        // for each TypeAppT then we want to expand our TypeAppTs and compare the
        // expanded results.
        //
        // If the lower bound's c is not a PolyT then we will fall down to an
        // incompatible use error.
        //
        // The upper bound's c should always be a PolyT here since we could not have
        // made it here if it was not given the logic of our earlier case.
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ConcretizeTypeAppsT(
                use_op,
                box (ts1, fv1, _, r1),
                box (c2, ts2, fv2, _, r2),
                false,
            ),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc,
            tparams,
            id: id1,
            t_out,
        }) = def_t.deref()
            && let TypeInner::DefT(_, c2_def) = c2.deref()
            && let DefTInner::PolyT(box PolyTData {
                tparams_loc: _,
                tparams: _,
                id: id2,
                t_out: _,
            }) = c2_def.deref()
            && id1 == id2
            && ts1.len() == ts2.len()
            && !flow_js_utils::wraps_utility_type(cx, t_out)
            && fv1 == fv2 =>
        {
            let targs: Vec<_> = ts1
                .iter()
                .zip(ts2.iter())
                .map(|(t1, t2)| (t1.dupe(), t2.dupe()))
                .collect();
            type_app_variance_check(
                cx,
                trace,
                use_op.dupe(),
                r1,
                r2,
                targs,
                tparams_loc.dupe(),
                &Vec1::try_from_vec(tparams.to_vec()).unwrap(),
            )?;
        }
        // This is the case which implements the expansion for our
        // TypeAppT (c, ts) ~> TypeAppT (c, ts) when the cs are unequal.
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ConcretizeTypeAppsT(
                use_op,
                box (ts1, fv1, op1, r1),
                box (c2, ts2, fv2, op2, r2),
                false,
            ),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc: tparams_loc1,
            tparams: xs1,
            t_out: t1,
            id: id1,
        }) = def_t.deref()
            && let TypeInner::DefT(_, c2_def) = c2.deref()
            && let DefTInner::PolyT(box PolyTData {
                tparams_loc: tparams_loc2,
                tparams: xs2,
                t_out: t2,
                id: id2,
            }) = c2_def.deref() =>
        {
            let (op1_eff, op2_eff) = match root_of_use_op(use_op) {
                RootUseOp::UnknownUse => (op1.dupe(), op2.dupe()),
                _ => (use_op.dupe(), use_op.dupe()),
            };
            let t1_inst = mk_typeapp_instance_of_poly(
                cx,
                trace,
                op2_eff,
                r2,
                r1,
                *fv1,
                id1.dupe(),
                tparams_loc1.dupe(),
                Vec1::try_from_vec(xs1.to_vec()).unwrap(),
                t1,
                ts1.dupe(),
            )?;
            let t2_inst = mk_typeapp_instance_of_poly(
                cx,
                trace,
                op1_eff,
                r1,
                r2,
                *fv2,
                id2.dupe(),
                tparams_loc2.dupe(),
                Vec1::try_from_vec(xs2.to_vec()).unwrap(),
                t2,
                ts2.dupe(),
            )?;
            rec_flow(
                cx,
                trace,
                (
                    &t1_inst,
                    &UseT::new(UseTInner::UseT(use_op.dupe(), t2_inst)),
                ),
            )?;
        }
        (
            TypeInner::TypeAppT(box TypeAppTData {
                reason: reason_tapp,
                use_op,
                type_,
                targs,
                from_value,
                use_desc: _,
            }),
            _,
        ) => {
            let reason_op = reason_of_use_t(u);
            if instantiation_utils::type_app_expansion::push_unless_loop(
                cx,
                type_app_expansion::Bound::Lower,
                type_,
                targs,
            ) {
                let t = mk_typeapp_instance_annot(
                    cx,
                    Some(trace),
                    use_op.dupe(),
                    reason_op,
                    reason_tapp,
                    *from_value,
                    None,
                    type_,
                    targs.dupe(),
                )?;
                rec_flow(cx, trace, (&t, u))?;
                instantiation_utils::type_app_expansion::pop(cx);
            }
        }
        // Concretize types for type inspection purpose up to this point. The rest are
        // recorded as lower bound to the target tvar. *
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForImportsExports,
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }

        // Namespace and type qualification
        (
            TypeInner::NamespaceT(ns),
            UseTInner::GetTypeFromNamespaceT(box GetTypeFromNamespaceTData {
                reason: reason_op,
                use_op,
                prop_ref: (prop_ref_reason, prop_name),
                tout,
            }),
        ) => {
            let props = cx.find_props(ns.types_tmap.dupe());
            match props.get(prop_name).and_then(property::read_t) {
                Some(prop) => {
                    let t = helpers::reposition(
                        cx,
                        Some(trace),
                        reason_op.loc().dupe(),
                        None,
                        None,
                        prop,
                    )?;
                    let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
                    rec_flow_t(cx, trace, use_op.dupe(), (&t, &tout_t))?;
                }
                None => {
                    rec_flow(
                        cx,
                        trace,
                        (
                            &ns.values_type,
                            &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                                use_op: use_op.dupe(),
                                reason: reason_op.dupe(),
                                id: None,
                                from_annot: false,
                                skip_optional: false,
                                propref: Box::new(PropRef::Named {
                                    reason: prop_ref_reason.dupe(),
                                    name: prop_name.dupe(),
                                    from_indexed_access: false,
                                }),
                                tout: tout.clone(),
                                hint: hint_unavailable(),
                            }))),
                        ),
                    )?;
                }
            }
        }
        (
            _,
            UseTInner::GetTypeFromNamespaceT(box GetTypeFromNamespaceTData {
                reason: reason_op,
                use_op,
                prop_ref: (prop_ref_reason, prop_name),
                tout,
            }),
        ) => {
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: reason_op.dupe(),
                        id: None,
                        from_annot: false,
                        skip_optional: false,
                        propref: Box::new(PropRef::Named {
                            reason: prop_ref_reason.dupe(),
                            name: prop_name.dupe(),
                            from_indexed_access: false,
                        }),
                        tout: tout.clone(),
                        hint: hint_unavailable(),
                    }))),
                ),
            )?;
        }
        // unwrap namespace type into object type, drop all information about types in the namespace
        (TypeInner::NamespaceT(ns), UseTInner::GetPropT(box data))
            if let UseOp::Op(root_op) = &data.use_op
                && let VirtualRootUseOp::GetProperty(prop_reason) = root_op.deref()
                && ns.namespace_symbol.kind()
                    == flow_common::flow_symbol::SymbolKind::SymbolModule =>
        {
            let new_u = UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                use_op: UseOp::Op(Arc::new(VirtualRootUseOp::GetExport(prop_reason.dupe()))),
                reason: data.reason.dupe(),
                id: data.id,
                from_annot: data.from_annot,
                skip_optional: data.skip_optional,
                propref: data.propref.clone(),
                tout: data.tout.clone(),
                hint: data.hint.clone(),
            })));
            rec_flow(cx, trace, (&ns.values_type, &new_u))?;
        }
        // unwrap namespace type into object type, drop all information about types in the namespace
        (TypeInner::NamespaceT(ns), _) => {
            rec_flow(cx, trace, (&ns.values_type, u))?;
        }

        // ***************************************
        // * transform values to type references *
        // ***************************************
        (
            TypeInner::DefT(reason_tapp, def_t),
            UseTInner::ValueToTypeReferenceT(box ValueToTypeReferenceTData {
                use_op,
                reason: reason_op,
                ..
            }),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc,
            tparams: ids,
            t_out,
            id,
        }) = def_t.deref()
            && flow_common::files::has_ts_ext(cx.file())
            && ids.iter().all(|tp| tp.default.is_some()) =>
        {
            // In .ts files, treat missing type args the same as empty type args (Foo = Foo<>),
            // matching TypeScript behavior where defaults are used.
            // Only when all params have defaults; otherwise fall through to EMissingTypeArgs.
            let t = FlowJs::mk_typeapp_of_poly(
                cx,
                trace,
                use_op.dupe(),
                reason_op,
                reason_tapp,
                id.dupe(),
                tparams_loc.dupe(),
                Vec1::try_from_vec(ids.to_vec()).unwrap(),
                t_out,
                Rc::from([]),
            )?;
            rec_flow(cx, trace, (&t, u))?;
        }
        (
            _,
            UseTInner::ValueToTypeReferenceT(box ValueToTypeReferenceTData {
                use_op,
                reason: reason_op,
                kind: type_t_kind,
                tout,
            }),
        ) => {
            let t = flow_js_utils::value_to_type_reference_transform::run_on_concrete_type(
                cx,
                use_op.dupe(),
                reason_op,
                *type_t_kind,
                l.dupe(),
            )?;
            let tout_t = Type::new(TypeInner::OpenT((**tout).dupe()));
            unify_opt(
                cx,
                Some(trace),
                use_op.dupe(),
                UnifyCause::Uncategorized,
                None,
                &t,
                &tout_t,
            )?;
        }

        // **********************
        // *    opaque types    *
        // **********************

        // Repositioning should happen before opaque types are considered so that we can
        // have the "most recent" location when we do look at the opaque type
        (
            TypeInner::NominalT { .. },
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: inner_u,
            },
        ) => {
            let repos = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos, inner_u))?;
        }

        // Store the opaque type when doing `ToStringT`, so we can use that
        // rather than just `string` if the underlying is `string`.
        (
            TypeInner::NominalT {
                reason: _,
                nominal_type,
            },
            UseTInner::ToStringT {
                orig_t: _,
                reason: to_string_reason,
                t_out,
            },
        ) if let nominal::Id::UserDefinedOpaqueTypeId(
            box nominal::UserDefinedOpaqueTypeIdData(nominal_id, _),
        ) = &nominal_type.nominal_id
            && nominal_id.0.source() == Some(cx.file())
            && let nominal::UnderlyingT::OpaqueWithLocal { t } = &nominal_type.underlying_t =>
        {
            rec_flow(
                cx,
                trace,
                (
                    t,
                    &UseT::new(UseTInner::ToStringT {
                        orig_t: Some(l.dupe()),
                        reason: to_string_reason.dupe(),
                        t_out: t_out.clone(),
                    }),
                ),
            )?;
        }
        (
            TypeInner::NominalT {
                reason: _,
                nominal_type,
            },
            UseTInner::ToStringT {
                orig_t: _,
                reason: to_string_reason,
                t_out,
            },
        ) if let nominal::Id::UserDefinedOpaqueTypeId(_) = &nominal_type.nominal_id
            && let nominal::UnderlyingT::CustomError(box nominal::CustomErrorData {
                t,
                custom_error_loc: _,
            }) = &nominal_type.underlying_t =>
        {
            rec_flow(
                cx,
                trace,
                (
                    t,
                    &UseT::new(UseTInner::ToStringT {
                        orig_t: Some(l.dupe()),
                        reason: to_string_reason.dupe(),
                        t_out: t_out.clone(),
                    }),
                ),
            )?;
        }

        // Use the upper bound of NominalT if it's available, for operations that must be
        // performed on some concretized types.
        (
            TypeInner::NominalT { nominal_type, .. },
            UseTInner::ObjKitT { .. }
            | UseTInner::ReactKitT(box ReactKitTData {
                use_op: _,
                reason: _,
                tool: _,
            }),
        ) if let Some(t) = nominal_type.upper_t.as_ref() => {
            rec_flow(cx, trace, (t, u))?;
        }
        // Store the opaque type when doing `ToStringT`, so we can use that
        // rather than just `string` if the supertype is `string`
        (
            TypeInner::NominalT { nominal_type, .. },
            UseTInner::ToStringT {
                orig_t: _,
                reason: to_string_reason,
                t_out,
            },
        ) if let Some(t) = nominal_type.upper_t.as_ref() => {
            rec_flow(
                cx,
                trace,
                (
                    t,
                    &UseT::new(UseTInner::ToStringT {
                        orig_t: Some(l.dupe()),
                        reason: to_string_reason.dupe(),
                        t_out: t_out.clone(),
                    }),
                ),
            )?;
        }
        // If the type is still in the same file it was defined, we allow it to
        // expose its underlying type information
        (TypeInner::NominalT { nominal_type, .. }, _)
            if let nominal::Id::UserDefinedOpaqueTypeId(
                box nominal::UserDefinedOpaqueTypeIdData(nominal_id, _),
            ) = &nominal_type.nominal_id
                && nominal_id.0.source() == Some(cx.file())
                && let nominal::UnderlyingT::OpaqueWithLocal { t } = &nominal_type.underlying_t =>
        {
            rec_flow(cx, trace, (t, u))?;
        }
        (TypeInner::NominalT { nominal_type, .. }, _)
            if let nominal::Id::UserDefinedOpaqueTypeId(
                box nominal::UserDefinedOpaqueTypeIdData(_, _),
            ) = &nominal_type.nominal_id
                && let nominal::UnderlyingT::CustomError(box nominal::CustomErrorData {
                    t,
                    custom_error_loc: _,
                }) = &nominal_type.underlying_t =>
        {
            rec_flow(cx, trace, (t, u))?;
        }

        // *****************************************************
        // * keys (NOTE: currently we only support string keys *
        // *****************************************************
        (TypeInner::KeysT(_, _), UseTInner::ToStringT { t_out, .. }) => {
            rec_flow(cx, trace, (l, t_out))?;
        }
        (TypeInner::KeysT(reason1, o1), _) => {
            // flow all keys of o1 to u
            rec_flow(
                cx,
                trace,
                (
                    o1,
                    &UseT::new(UseTInner::GetKeysT(reason1.dupe(), Box::new(u.dupe()))),
                ),
            )?;
            if let UseTInner::UseT(_, t) = u.deref() {
                tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, t);
            }
        }
        (
            TypeInner::GenericT(box GenericTData { reason, bound, .. }),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForEnumExhaustiveCheck,
                ..
            }),
        ) => {
            let repos = helpers::reposition_reason(cx, None, reason, false, bound)?;
            rec_flow(cx, trace, (&repos, u))?;
        }
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForInspection,
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }
        // helpers
        (
            TypeInner::DefT(reason_o, def_t),
            UseTInner::HasOwnPropT(box HasOwnPropTData {
                use_op,
                reason: reason_op,
                type_: key,
            }),
        ) if let DefTInner::ObjT(obj) = def_t.deref() => {
            let ObjType {
                props_tmap: mapr,
                flags,
                ..
            } = obj.as_ref();
            let dropped = drop_generic(key.dupe());
            match (dropped.deref(), &flags.obj_kind) {
                // If we have a literal string and that property exists
                (TypeInner::DefT(_, inner_def), _)
                    if let DefTInner::SingletonStrT { value: x, .. } = inner_def.deref()
                        && cx.has_prop(mapr.dupe(), x) => {}
                // If we have a dictionary, try that next
                (
                    _,
                    ObjKind::Indexed(DictType {
                        key: expected_key, ..
                    }),
                ) => {
                    let mod_key = type_util::mod_reason_of_t(&|_| reason_op.dupe(), key);
                    rec_flow_t(cx, trace, use_op.dupe(), (&mod_key, expected_key))?;
                }
                _ => {
                    let dropped2 = drop_generic(key.dupe());
                    let (prop, suggestion) = match dropped2.deref() {
                        TypeInner::DefT(_, inner_def2)
                            if let DefTInner::SingletonStrT { value: prop, .. } =
                                inner_def2.deref() =>
                        {
                            (
                                Some(prop.dupe()),
                                prop_typo_suggestion(cx, &[mapr.dupe()], prop.as_str()),
                            )
                        }
                        _ => (None, None),
                    };
                    let err =
                        ErrorMessage::EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                            prop_name: prop,
                            reason_prop: reason_op.dupe(),
                            reason_obj: reason_o.dupe(),
                            use_op: use_op.dupe(),
                            suggestion,
                        }));
                    flow_js_utils::add_output(cx, err)?;
                }
            }
        }
        (
            TypeInner::DefT(reason_o, def_t),
            UseTInner::HasOwnPropT(box HasOwnPropTData {
                use_op,
                reason: reason_op,
                type_: key,
            }),
        ) if let DefTInner::InstanceT(instance_t) = def_t.deref()
            && let Some(x) = match key.deref() {
                TypeInner::DefT(_, kd)
                    if let DefTInner::SingletonStrT { value, .. } = kd.deref() =>
                {
                    Some(value)
                }
                TypeInner::GenericT(box GenericTData { bound, .. })
                    if let TypeInner::DefT(_, kd) = bound.deref()
                        && let DefTInner::SingletonStrT { value, .. } = kd.deref() =>
                {
                    Some(value)
                }
                _ => None,
            } =>
        {
            let inst = &instance_t.inst;
            let own_props = cx.find_props(inst.own_props.dupe());
            match own_props.get(x) {
                Some(_) => {}
                None => {
                    let err =
                        ErrorMessage::EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                            prop_name: Some(x.dupe()),
                            reason_prop: reason_op.dupe(),
                            reason_obj: reason_o.dupe(),
                            use_op: use_op.dupe(),
                            suggestion: prop_typo_suggestion(
                                cx,
                                &[inst.own_props.dupe()],
                                x.as_str(),
                            ),
                        }));
                    match &inst.inst_dict {
                        Some(DictType { key: dict_key, .. }) => {
                            let mod_key = type_util::mod_reason_of_t(&|_| reason_op.dupe(), key);
                            rec_flow_t(cx, trace, use_op.dupe(), (&mod_key, dict_key))?;
                        }
                        None => {
                            flow_js_utils::add_output(cx, err)?;
                        }
                    }
                }
            }
        }
        (
            TypeInner::DefT(reason_o, def_t),
            UseTInner::HasOwnPropT(box HasOwnPropTData {
                use_op,
                reason: reason_op,
                type_: _,
            }),
        ) if matches!(def_t.deref(), DefTInner::InstanceT(_)) => {
            let err = ErrorMessage::EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                prop_name: None,
                reason_prop: reason_op.dupe(),
                reason_obj: reason_o.dupe(),
                use_op: use_op.dupe(),
                suggestion: None,
            }));
            flow_js_utils::add_output(cx, err)?;
        }
        // AnyT has every prop
        (
            TypeInner::AnyT(_, _),
            UseTInner::HasOwnPropT(box HasOwnPropTData {
                use_op: _,
                reason: _,
                type_: _,
            }),
        ) => {}
        (TypeInner::DefT(_, def_t), UseTInner::GetKeysT(reason_op, keys))
            if let DefTInner::ObjT(obj) = def_t.deref() =>
        {
            let ObjType {
                flags, props_tmap, ..
            } = obj.as_ref();
            let dict_t = obj_type::get_dict_opt(&flags.obj_kind);
            // flow the union of keys of l to keys
            let keylist =
                flow_js_utils::keylist_of_props(&cx.find_props(props_tmap.dupe()), reason_op);
            let union_t = type_util::union_of_ts(reason_op.dupe(), keylist, None);
            rec_flow(cx, trace, (&union_t, keys))?;
            if let Some(DictType { key: dict_key, .. }) = dict_t {
                rec_flow(
                    cx,
                    trace,
                    (
                        dict_key,
                        &UseT::new(UseTInner::ToStringT {
                            orig_t: None,
                            reason: reason_op.dupe(),
                            t_out: keys.clone(),
                        }),
                    ),
                )?;
            }
        }
        (TypeInner::DefT(_, def_t), UseTInner::GetKeysT(reason_op, keys))
            if let DefTInner::InstanceT(instance_t) = def_t.deref() =>
        {
            let inst = &instance_t.inst;
            // methods are not enumerable, so only walk fields
            let own_props = cx.find_props(inst.own_props.dupe());
            let keylist = flow_js_utils::keylist_of_props(&own_props, reason_op);
            let union_t = type_util::union_of_ts(reason_op.dupe(), keylist, None);
            rec_flow(cx, trace, (&union_t, keys))?;
            match &inst.inst_dict {
                Some(DictType { key: dict_key, .. }) => {
                    rec_flow(
                        cx,
                        trace,
                        (
                            dict_key,
                            &UseT::new(UseTInner::ToStringT {
                                orig_t: None,
                                reason: reason_op.dupe(),
                                t_out: keys.clone(),
                            }),
                        ),
                    )?;
                }
                None => {}
            }
        }
        (TypeInner::AnyT(_, _), UseTInner::GetKeysT(reason_op, keys)) => {
            // rec_flow cx trace (StrModuleT.why reason_op, keys)
            let str_mod = str_module_t::why(reason_op.dupe());
            rec_flow(cx, trace, (&str_mod, keys))?;
        }

        // In general, typechecking is monotonic in the sense that more constraints
        // produce more errors. However, sometimes we may want to speculatively try
        // out constraints, backtracking if they produce errors (and removing the
        // errors produced). This is useful to typecheck union types and
        // intersection types: see below.
        // NOTE: It is important that any def type that simplifies to a union or
        // intersection of other def types be processed before we process unions
        // and intersections: otherwise we may get spurious errors.

        // **********
        // * values *
        // **********
        (TypeInner::DefT(_, def_t), UseTInner::GetValuesT(reason, values))
            if let DefTInner::ObjT(o) = def_t.deref() =>
        {
            let values_l = flow_js_utils::get_values_type_of_obj_t(cx, o, reason.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&values_l, values))?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::GetValuesT(reason, values))
            if let DefTInner::InstanceT(instance_t) = def_t.deref() =>
        {
            let inst = &instance_t.inst;
            let values_l = flow_js_utils::get_values_type_of_instance_t(
                cx,
                inst.own_props.dupe(),
                inst.inst_dict.as_ref(),
                reason.dupe(),
            );
            rec_flow_t(cx, trace, unknown_use(), (&values_l, values))?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::GetValuesT(reason, t_out))
            if let DefTInner::ArrT(arr) = def_t.deref() =>
        {
            let elem_t = elemt_of_arrtype(arr);
            let mod_elem = type_util::mod_reason_of_t(&|_| reason.dupe(), &elem_t);
            rec_flow_t(cx, trace, unknown_use(), (&mod_elem, t_out))?;
        }
        // Any will always be ok
        (TypeInner::AnyT(_, src), UseTInner::GetValuesT(reason, values)) => {
            let any = any_t::why(*src, reason.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, values))?;
        }

        // ***********************************************
        // * Values of a dictionary - `mixed` otherwise. *
        // ***********************************************
        (TypeInner::DefT(_, def_t), UseTInner::GetDictValuesT(_, result))
            if let DefTInner::ObjT(obj) = def_t.deref()
                && let ObjType {
                    flags, props_tmap, ..
                } = obj.as_ref()
                && let ObjKind::Indexed(DictType {
                    value,
                    dict_polarity,
                    ..
                }) = &flags.obj_kind
                && Polarity::compat(*dict_polarity, Polarity::Positive)
                && cx.find_props(props_tmap.dupe()).is_empty() =>
        {
            rec_flow(cx, trace, (value, result))?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::GetDictValuesT(reason, result))
            if matches!(def_t.deref(), DefTInner::ObjT(_) | DefTInner::InstanceT(_)) =>
        {
            let mixed = mixed_t::why(reason.dupe());
            rec_flow(cx, trace, (&mixed, result))?;
        }
        // Any will always be ok
        (TypeInner::AnyT(_, src), UseTInner::GetDictValuesT(reason, result)) => {
            let any = any_t::why(*src, reason.dupe());
            rec_flow(cx, trace, (&any, result))?;
        }

        // ********************************
        // * union and intersection types *
        // ********************************
        // We don't want to miss any union optimizations because of unevaluated type destructors, so
        // if our union contains any of these problematic types, we force it to resolve its elements before
        // considering its upper bound
        (
            _,
            UseTInner::ResolveUnionT(box ResolveUnionTData {
                reason,
                resolved,
                unresolved,
                upper,
                id,
            }),
        ) => {
            if let Some((tc_l, tc_u)) =
                resolve_union(cx, trace, reason, *id, resolved, unresolved, l, upper)?
            {
                return Ok(Some(TailCall::Flow(tc_l, tc_u)));
            }
        }
        (TypeInner::UnionT(reason, rep), UseTInner::FilterMaybeT(use_op, tout)) => {
            let void_t = void::why(reason.dupe());
            let null_t = null::why(reason.dupe());
            let filter_null_and_void = |t: &Type| -> bool {
                type_util::quick_subtype(None::<&fn(&Type)>, t, &void_t)
                    || type_util::quick_subtype(None::<&fn(&Type)>, t, &null_t)
            };
            match rep.check_enum() {
                Some(_) => {
                    let filtered = flow_js_utils::remove_predicate_from_union(
                        reason.dupe(),
                        cx,
                        filter_null_and_void,
                        rep,
                    );
                    rec_flow_t(cx, trace, use_op.dupe(), (&filtered, tout))?;
                }
                None => {
                    let use_op_clone = use_op.dupe();
                    let non_maybe_union = flow_js_utils::map_union(
                        |cx_inner, trace_inner, t_inner, tout_inner| {
                            rec_flow(
                                cx_inner,
                                *trace_inner,
                                (
                                    t_inner,
                                    &UseT::new(UseTInner::FilterMaybeT(
                                        use_op_clone.dupe(),
                                        tout_inner.dupe(),
                                    )),
                                ),
                            )?;
                            Ok(())
                        },
                        cx,
                        &trace,
                        rep,
                        reason.dupe(),
                    )?;
                    rec_flow_t(cx, trace, use_op.dupe(), (&non_maybe_union, tout))?;
                }
            }
        }
        (TypeInner::UnionT(reason, rep), _)
            if rep.members_iter().any(flow_js_utils::is_union_resolvable) =>
        {
            flow_js_utils::iter_resolve_union(
                |cx_inner, trace_inner, (t_inner, u_inner)| {
                    rec_flow(cx_inner, *trace_inner, (&t_inner, &u_inner))?;
                    Ok(())
                },
                cx,
                &trace,
                reason.dupe(),
                rep,
                u.dupe(),
            )?;
        }
        // Don't split the union type into its constituent members.
        // Instead, reposition the entire union type.
        (
            TypeInner::UnionT(_, _),
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: u_inner,
            },
        ) => {
            let repos_l = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos_l, u_inner))?;
        }
        (
            TypeInner::UnionT(_, _),
            UseTInner::SealGenericT(box SealGenericTData {
                reason: _,
                id,
                name,
                cont,
                no_infer,
            }),
        ) => {
            let reason = reason_of_t(l);
            let generic = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                reason: reason.dupe(),
                id: id.clone(),
                name: name.dupe(),
                bound: l.dupe(),
                no_infer: *no_infer,
            })));
            continue_(cx, trace, &generic, cont)?;
        }
        (TypeInner::UnionT(_, _), UseTInner::ObjKitT(use_op, reason, resolve_tool, tool, tout)) => {
            object_kit::run(
                trace,
                cx,
                use_op.dupe(),
                reason,
                resolve_tool,
                tool,
                l,
                tout,
            )?;
        }
        // Shortcut for indexed accesses with the same type as the dict key.
        (
            TypeInner::UnionT(_, _),
            UseTInner::ElemT(box ElemTData {
                use_op,
                reason,
                obj,
                action: box ElemAction::ReadElem(box ReadElemData { tout, .. }),
            }),
        ) if let TypeInner::DefT(_, obj_def) = obj.deref()
            && let DefTInner::ObjT(obj_t) = obj_def.deref()
            && {
                let ObjType { flags, .. } = obj_t.as_ref();
                match obj_type::get_dict_opt(&flags.obj_kind) {
                    Some(DictType {
                        key, dict_polarity, ..
                    }) => {
                        flow_typing_flow_common::concrete_type_eq::eq(cx, l, key)
                            && Polarity::compat(*dict_polarity, Polarity::Positive)
                    }
                    None => false,
                }
            } =>
        {
            let ObjType { flags, .. } = obj_t.as_ref();
            let dict = obj_type::get_dict_opt(&flags.obj_kind).unwrap();
            let value = &dict.value;
            let value = helpers::reposition_reason(cx, Some(trace), reason, false, value)?;
            let value = match &flags.react_dro {
                Some(dro) => {
                    let frame_use_op = VirtualUseOp::Frame(
                        Arc::new(FrameUseOp::ReactDeepReadOnly(Box::new((
                            dro.0.dupe(),
                            dro.1.clone(),
                        )))),
                        Arc::new(use_op.dupe()),
                    );
                    mk_react_dro(cx, frame_use_op, dro.clone(), value)
                }
                None => value,
            };
            let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&value, &open_tout))?;
        }
        (
            TypeInner::UnionT(_, rep),
            UseTInner::ElemT(box ElemTData {
                use_op,
                reason,
                obj,
                action:
                    box ElemAction::ReadElem(box ReadElemData {
                        id,
                        from_annot: true,
                        skip_optional,
                        access_iterables,
                        tout,
                    }),
            }),
        ) => {
            let distribute = || -> Result<(), FlowJsException> {
                let reason = reason.dupe().update_desc(|d| d.invalidate_rtype_alias());
                let mut members = rep.members_iter();
                let t0 = members.next().unwrap();
                let t1 = members.next().unwrap();
                let ts: Vec<_> = members.duped().collect();
                let f = |t: &Type| -> Result<Type, FlowJsException> {
                    flow_typing_tvar::mk_no_wrap_where(
                        cx,
                        reason.dupe(),
                        |cx, tvar_reason, tvar_id| {
                            let tvar = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                            let action = ElemAction::ReadElem(Box::new(ReadElemData {
                                id: *id,
                                from_annot: true,
                                skip_optional: *skip_optional,
                                access_iterables: *access_iterables,
                                tout: tvar,
                            }));
                            rec_flow(
                                cx,
                                trace,
                                (
                                    t,
                                    &UseT::new(UseTInner::ElemT(Box::new(ElemTData {
                                        use_op: use_op.dupe(),
                                        reason: reason.dupe(),
                                        obj: obj.dupe(),
                                        action: Box::new(action),
                                    }))),
                                ),
                            )
                        },
                    )
                };
                let ft0 = f(t0)?;
                let ft1 = f(t1)?;
                let fts: Rc<[_]> = ts.iter().map(f).collect::<Result<Vec<_>, _>>()?.into();
                let new_rep =
                    union_rep::make(None, union_rep::UnionKind::UnknownKind, ft0, ft1, fts);
                let union_t = Type::new(TypeInner::UnionT(reason, new_rep));
                let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
                rec_flow_t(cx, trace, unknown_use(), (&union_t, &open_tout))?;
                Ok(())
            };
            // Fast path for large Union-of-keys ~> ElemT on an ObjT without a dict.
            // Instead of distributing the union and flowing each key through
            // GetPropT individually (which is O(n) flows for n keys), directly
            // look up each key in the property map (O(1) hash lookup per key)
            // and collect the value types into a union.
            //
            // We only apply this for large unions (>100 members) to avoid adding
            // overhead to the common small-union case. The semantics are equivalent:
            // skip_optional and no_unchecked_indexed_access are irrelevant here since
            // we require no dict and only match properties that exist in the prop map.
            match obj.deref() {
                TypeInner::DefT(_, def_t)
                    if let DefTInner::ObjT(obj_t) = def_t.deref()
                        && obj_type::get_dict_opt(&obj_t.flags.obj_kind).is_none()
                        && rep.members_iter().count() > 100 =>
                {
                    let props = cx.find_props(obj_t.props_tmap.dupe());
                    let members: Vec<_> = rep.members_iter().collect();
                    let value_types: Vec<_> = members
                        .iter()
                        .filter_map(|t| {
                            lookup_prop_type_direct(cx, use_op, &obj_t.flags.react_dro, &props, t)
                        })
                        .collect();
                    if value_types.len() == members.len() {
                        let reason = reason.dupe().update_desc(|d| d.invalidate_rtype_alias());
                        match value_types.as_slice() {
                            [] => {}
                            [t] => {
                                let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
                                rec_flow_t(cx, trace, unknown_use(), (t, &open_tout))?;
                            }
                            [t0, t1, ts @ ..] => {
                                let new_rep = union_rep::make(
                                    None,
                                    union_rep::UnionKind::UnknownKind,
                                    t0.dupe(),
                                    t1.dupe(),
                                    ts.iter().duped().collect::<Vec<_>>().into(),
                                );
                                let union_t = Type::new(TypeInner::UnionT(reason, new_rep));
                                let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
                                rec_flow_t(cx, trace, unknown_use(), (&union_t, &open_tout))?;
                            }
                        }
                    } else {
                        distribute()?;
                    }
                }
                _ => {
                    distribute()?;
                }
            }
        }
        (TypeInner::UnionT(_, rep), _)
            if match u.deref() {
                UseTInner::ConditionalT(box ConditionalTData {
                    distributive_tparam_name,
                    ..
                }) => distributive_tparam_name.is_some(),
                _ => true,
            } =>
        {
            // For homogeneous enum unions that have already been optimized,
            // try a single representative member speculatively. If it fails,
            // emit just that one error (all members would fail identically).
            // If it succeeds, fall through to normal distribution to catch
            // members that might fail.
            // Note: we intentionally skip optimize_enum_only here (unlike the
            // TypeCastT and UseT handlers) because the catch-all runs on ALL
            // use types and forcing optimization can trigger tvar resolution
            // with side-effect errors.
            let flowed_single = {
                match rep.check_enum_with_tag() {
                    Some((enums, Some(_))) if enums.len() > 1 => {
                        let mut members = rep.members_iter();
                        match members.next() {
                            None => false,
                            Some(representative) => {
                                let representative = representative.dupe();
                                drop(members);
                                match speculation_kit::try_singleton_throw_on_failure(
                                    cx,
                                    trace,
                                    representative.dupe(),
                                    u.dupe(),
                                ) {
                                    Err(FlowJsException::SpeculationSingletonError) => {
                                        let u = type_util::mod_use_op_of_use_t(
                                            |use_op| {
                                                flow_js_utils::union_representative_use_op(
                                                    cx,
                                                    l,
                                                    &representative,
                                                    use_op.dupe(),
                                                )
                                            },
                                            u,
                                        );
                                        rec_flow(cx, trace, (&representative, &u))?;
                                        true
                                    }
                                    Ok(()) => false,
                                    Err(e) => return Err(e),
                                }
                            }
                        }
                    }
                    _ => false,
                }
            };
            if !flowed_single {
                flow_all_in_union(cx, trace, rep, u)?;
            }
        }
        (_, UseTInner::FilterOptionalT(use_op, u_inner)) => {
            rec_flow_t(cx, trace, use_op.dupe(), (l, u_inner))?;
        }
        (_, UseTInner::FilterMaybeT(use_op, u_inner)) => {
            rec_flow_t(cx, trace, use_op.dupe(), (l, u_inner))?;
        }
        // special treatment for some operations on intersections: these
        // rules fire for particular UBs whose constraints can (or must)
        // be resolved against intersection LBs as a whole, instead of
        // by decomposing the intersection into its parts.

        // lookup of properties
        (
            TypeInner::IntersectionT(_, rep),
            UseTInner::LookupT(box LookupTData {
                reason,
                lookup_kind,
                try_ts_on_failure,
                propref,
                lookup_action,
                ids,
                method_accessible,
                ignore_dicts,
            }),
        ) => {
            let ts: Vec<_> = rep.members_iter().duped().collect();
            assert!(!ts.is_empty());

            // Since s could be in any object type in the list ts, we try to look it
            // up in the first element of ts, pushing the rest into the list
            // try_ts_on_failure (see below).
            let hd = &ts[0];
            let mut new_try_ts = ts[1..].to_vec();
            new_try_ts.extend(try_ts_on_failure.iter().duped());
            rec_flow(
                cx,
                trace,
                (
                    hd,
                    &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                        reason: reason.dupe(),
                        lookup_kind: lookup_kind.clone(),
                        try_ts_on_failure: new_try_ts.into(),
                        propref: propref.clone(),
                        lookup_action: lookup_action.clone(),
                        ids: ids.dupe(),
                        method_accessible: *method_accessible,
                        ignore_dicts: *ignore_dicts,
                    }))),
                ),
            )?;
        }
        // Cases of an intersection need to produce errors on non-existent
        // properties instead of a default, so that other cases may be tried
        // instead and succeed.
        (TypeInner::IntersectionT(_, _), UseTInner::GetPropT(box data)) if data.id.is_some() => {
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: data.use_op.dupe(),
                        reason: data.reason.dupe(),
                        id: None,
                        from_annot: data.from_annot,
                        skip_optional: data.skip_optional,
                        propref: data.propref.clone(),
                        tout: data.tout.clone(),
                        hint: data.hint.clone(),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::IntersectionT(_, _),
            UseTInner::TestPropT(box TestPropTData {
                use_op,
                reason,
                id: _,
                propref,
                tout,
                hint,
            }),
        ) => {
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        id: None,
                        from_annot: false,
                        skip_optional: false,
                        propref: propref.clone(),
                        tout: tout.clone(),
                        hint: hint.clone(),
                    }))),
                ),
            )?;
        }
        // extends
        (
            TypeInner::IntersectionT(_, rep),
            UseTInner::ExtendsUseT(box ExtendsUseTData {
                use_op,
                reason,
                targs: try_ts_on_failure,
                true_t: ext_l,
                false_t: ext_u,
            }),
        ) => {
            let mut members = rep.members_iter();
            let t = members.next().unwrap();
            let ts: Vec<_> = members.duped().collect();
            let mut new_try_ts = ts;
            new_try_ts.extend(try_ts_on_failure.iter().duped());
            rec_flow(
                cx,
                trace,
                (
                    t,
                    &UseT::new(UseTInner::ExtendsUseT(Box::new(ExtendsUseTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        targs: new_try_ts.into(),
                        true_t: ext_l.dupe(),
                        false_t: ext_u.dupe(),
                    }))),
                ),
            )?;
        }
        // consistent override of properties
        (
            TypeInner::IntersectionT(_, rep),
            UseTInner::SuperT(box SuperTData {
                use_op,
                reason,
                derived_type: derived,
            }),
        ) => {
            for t in rep.members_iter() {
                let u_inner = match use_op {
                    VirtualUseOp::Op(root)
                        if let VirtualRootUseOp::ClassExtendsCheck { def, extends: _ } =
                            root.deref() =>
                    {
                        let new_use_op =
                            VirtualUseOp::Op(Arc::new(VirtualRootUseOp::ClassExtendsCheck {
                                def: def.dupe(),
                                extends: reason_of_t(t).dupe(),
                            }));
                        UseT::new(UseTInner::SuperT(Box::new(SuperTData {
                            use_op: new_use_op,
                            reason: reason.dupe(),
                            derived_type: derived.clone(),
                        })))
                    }
                    _ => u.dupe(),
                };
                rec_flow(cx, trace, (t, &u_inner))?;
            }
        }
        // structural subtype multiple inheritance
        (TypeInner::IntersectionT(_, rep), UseTInner::ImplementsT(use_op, this)) => {
            for t in rep.members_iter() {
                let u_inner = match use_op {
                    VirtualUseOp::Op(root)
                        if let VirtualRootUseOp::ClassImplementsCheck(
                            box ClassImplementsCheckData {
                                def,
                                name,
                                implements: _,
                            },
                        ) = root.deref() =>
                    {
                        let new_use_op =
                            VirtualUseOp::Op(Arc::new(VirtualRootUseOp::ClassImplementsCheck(
                                Box::new(ClassImplementsCheckData {
                                    def: def.dupe(),
                                    name: name.dupe(),
                                    implements: reason_of_t(t).dupe(),
                                }),
                            )));
                        UseT::new(UseTInner::ImplementsT(new_use_op, this.dupe()))
                    }
                    _ => u.dupe(),
                };
                rec_flow(cx, trace, (t, &u_inner))?;
            }
        }
        // predicates: prevent a predicate upper bound from prematurely decomposing
        // an intersection lower bound
        (
            TypeInner::IntersectionT(_, _),
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind: ConcretizationKind::ConcretizeForPredicate(_),
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }
        // This duplicates the (_, ReposLowerT u) near the end of this pattern
        // match but has to appear here to preempt the (IntersectionT, _) in
        // between so that we reposition the entire intersection.
        (
            TypeInner::IntersectionT(_, _),
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: u_inner,
            },
        ) => {
            let repos_l = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos_l, u_inner))?;
        }
        (
            TypeInner::IntersectionT(_, _),
            UseTInner::ObjKitT(use_op, reason, resolve_tool, tool, tout),
        ) => {
            object_kit::run(
                trace,
                cx,
                use_op.dupe(),
                reason,
                resolve_tool,
                tool,
                l,
                tout,
            )?;
        }
        (
            TypeInner::IntersectionT(_, _),
            UseTInner::SealGenericT(box SealGenericTData {
                reason: _,
                id,
                name,
                cont,
                no_infer,
            }),
        ) => {
            let reason = reason_of_t(l);
            let generic = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                reason: reason.dupe(),
                id: id.clone(),
                name: name.dupe(),
                bound: l.dupe(),
                no_infer: *no_infer,
            })));
            continue_(cx, trace, &generic, cont)?;
        }
        (
            TypeInner::IntersectionT(_, _),
            UseTInner::CallT(box CallTData {
                use_op,
                call_action: box CallAction::ConcretizeCallee(tout),
                ..
            }),
        ) => {
            let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (l, &open_tout))?;
        }
        (
            TypeInner::IntersectionT(_, _),
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind:
                    ConcretizationKind::ConcretizeForOptionalChain
                    | ConcretizationKind::ConcretizeForEnumExhaustiveCheck
                    | ConcretizationKind::ConcretizeForOperatorsChecking
                    | ConcretizationKind::ConcretizeForComputedObjectKeys
                    | ConcretizationKind::ConcretizeForObjectAssign
                    | ConcretizationKind::ConcretizeForDestructuring,
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }
        (TypeInner::IntersectionT(r, rep), _) => {
            // We only call CalleeRecorder here for sig-help information. As far as
            // the typed AST is concerned when dealing with intersections we record
            // the specific branch that was selected. Therefore, we do not record
            // intersections when they hit a CallT constraint. The only time when an
            // intersection is allowed is when we have exhausted the branches of a
            // speculation job (this is a Flow error) and fall back to the
            // intersection as the type for the callee node. (This happens in
            // Default_resolver.)
            callee_recorder::add_callee_use(cx, callee_recorder::Kind::SigHelp, l.dupe(), u);
            speculation_kit::try_intersection(cx, trace, u.dupe(), r.dupe(), rep)?;
        }
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind: ConcretizationKind::ConcretizeForOptionalChain,
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }

        // *************************
        // * Resolving rest params *
        // *************************

        // `any` is obviously fine as a spread element. `Object` is fine because
        // any Iterable can be spread, and `Object` is the any type that covers
        // iterable objects.
        (
            TypeInner::AnyT(r, src),
            UseTInner::ResolveSpreadT(box ResolveSpreadTData {
                use_op,
                reason: reason_op,
                resolve_spread_type: resolve_spread,
            }),
        ) => {
            let mut rrt_resolved = resolve_spread.rrt_resolved.dupe();
            rrt_resolved.push_front(ResolvedParam::ResolvedAnySpreadArg(r.dupe(), *src));
            resolve_spread_list_rec(
                cx,
                Some(trace),
                use_op.dupe(),
                reason_op,
                rrt_resolved,
                resolve_spread.rrt_unresolved.dupe(),
                resolve_spread.rrt_resolve_to.clone(),
            )?;
        }
        (
            _,
            UseTInner::ResolveSpreadT(box ResolveSpreadTData {
                use_op,
                reason: reason_op,
                resolve_spread_type: resolve_spread,
            }),
        ) => {
            let reason = reason_of_t(l);
            let (lt, generic) = match l.deref() {
                TypeInner::GenericT(box GenericTData {
                    bound,
                    id,
                    reason: gen_reason,
                    ..
                }) => {
                    let repos = helpers::reposition_reason(cx, None, gen_reason, false, bound)?;
                    (repos, Some(id.clone()))
                }
                _ => (l.dupe(), None),
            };
            let arrtype = match lt.deref() {
                TypeInner::DefT(_, lt_def) if let DefTInner::ArrT(arrtype) = lt_def.deref() => {
                    // Arrays
                    match (&resolve_spread.rrt_resolve_to, arrtype.as_ref()) {
                        (
                            SpreadResolve::ResolveSpreadsToTupleType { .. },
                            ArrType::ArrayAT(box ArrayATData {
                                tuple_view: None, ..
                            })
                            | ArrType::ROArrayAT(box (_, _)),
                        ) => {
                            // Only tuples can be spread into tuple types.
                            flow_js_utils::add_output(
                                cx,
                                ErrorMessage::ETupleInvalidTypeSpread(Box::new(
                                    ETupleInvalidTypeSpreadData {
                                        reason_spread: reason_op.dupe(),
                                        reason_arg: reason.dupe(),
                                    },
                                )),
                            )?;
                            ArrType::ArrayAT(Box::new(ArrayATData {
                                elem_t: any_t::error(reason.dupe()),
                                tuple_view: None,
                                react_dro: None,
                            }))
                        }
                        _ => (**arrtype).clone(),
                    }
                }
                _ => {
                    // Non-array non-any iterables, opaque arrays, etc
                    enum ResolveTo {
                        Iterable,
                        Array,
                        Tuple,
                    }
                    let resolve_to = match &resolve_spread.rrt_resolve_to {
                        // Spreading iterables in a type context is always OK
                        SpreadResolve::ResolveSpreadsToMultiflowSubtypeFull(_, _) => {
                            ResolveTo::Iterable
                        }
                        // Otherwise we're spreading values
                        SpreadResolve::ResolveSpreadsToArray(_, _)
                        | SpreadResolve::ResolveSpreadsToArrayLiteral { .. }
                        | SpreadResolve::ResolveSpreadsToMultiflowCallFull(_, _)
                        | SpreadResolve::ResolveSpreadsToMultiflowPartial(..) => {
                            // Babel's "loose mode" array spread transform deviates from
                            // the spec by assuming the spread argument is always an
                            // array. If the babel_loose_array_spread option is set, model
                            // this assumption.
                            if cx.babel_loose_array_spread() {
                                ResolveTo::Array
                            } else {
                                ResolveTo::Iterable
                            }
                        }
                        SpreadResolve::ResolveSpreadsToTupleType { .. } => ResolveTo::Tuple,
                    };
                    let elem_t = flow_typing_tvar::mk(cx, reason.dupe());
                    let resolve_to_type = match resolve_to {
                        ResolveTo::Iterable => {
                            let targs = vec![
                                elem_t.dupe(),
                                unsoundness::why(UnsoundnessKind::ResolveSpread, reason.dupe()),
                                unsoundness::why(UnsoundnessKind::ResolveSpread, reason.dupe()),
                            ];
                            let iterable_reason = reason.dupe().replace_desc_new(
                                VirtualReasonDesc::RCustom("Iterable expected for spread".into()),
                            );
                            get_builtin_typeapp(cx, &iterable_reason, None, "$Iterable", targs)
                        }
                        ResolveTo::Array => {
                            let arr_reason = reason.dupe().replace_desc_new(
                                VirtualReasonDesc::RCustom("Array expected for spread".into()),
                            );
                            Type::new(TypeInner::DefT(
                                arr_reason,
                                DefT::new(DefTInner::ArrT(Rc::new(ArrType::ROArrayAT(Box::new(
                                    (elem_t.dupe(), None),
                                ))))),
                            ))
                        }
                        ResolveTo::Tuple => {
                            flow_js_utils::add_output(
                                cx,
                                ErrorMessage::ETupleInvalidTypeSpread(Box::new(
                                    ETupleInvalidTypeSpreadData {
                                        reason_spread: reason_op.dupe(),
                                        reason_arg: reason.dupe(),
                                    },
                                )),
                            )?;
                            any_t::error(reason.dupe())
                        }
                    };
                    rec_flow_t(cx, trace, unknown_use(), (l, &resolve_to_type))?;
                    ArrType::ArrayAT(Box::new(ArrayATData {
                        elem_t,
                        tuple_view: None,
                        react_dro: None,
                    }))
                }
            };
            let elemt = elemt_of_arrtype(&arrtype);
            match &resolve_spread.rrt_resolve_to {
                // Any ResolveSpreadsTo* which does some sort of constant folding needs to
                // carry an id around to break the infinite recursion that constant
                // constant folding can trigger
                SpreadResolve::ResolveSpreadsToTupleType {
                    id: cfe_id,
                    elem_t: cfe_elem_t,
                    tout,
                    ..
                }
                | SpreadResolve::ResolveSpreadsToArrayLiteral {
                    id: cfe_id,
                    elem_t: cfe_elem_t,
                    tout,
                    ..
                } => {
                    // You might come across code like
                    //
                    // for (let x = 1; x < 3; x++) { foo = [...foo, x]; }
                    //
                    // where every time you spread foo, you flow another type into foo. So
                    // each time `l ~> ResolveSpreadT` is processed, it might produce a new
                    // `l ~> ResolveSpreadT` with a new `l`.
                    //
                    // Here is how we avoid this:
                    //
                    // 1. We use ConstFoldExpansion to detect when we see a ResolveSpreadT
                    //    upper bound multiple times
                    // 2. When a ResolveSpreadT upper bound multiple times, we change it into
                    //     a ResolveSpreadT upper bound that resolves to a more general type.
                    //    This should prevent more distinct lower bounds from flowing in
                    // 3. rec_flow caches (l,u) pairs.
                    let reason_elemt = reason_of_t(&elemt);
                    let pos = resolve_spread.rrt_resolved.len() as i32;
                    const_fold_expansion::guard(
                        cx,
                        *cfe_id,
                        (reason_elemt.dupe(), pos),
                        |recursion_depth| -> Result<(), FlowJsException> {
                            match recursion_depth {
                                0 => {
                                    // The first time we see this, we process it normally
                                    let mut rrt_resolved = resolve_spread.rrt_resolved.dupe();
                                    rrt_resolved.push_front(ResolvedParam::ResolvedSpreadArg(
                                        Box::new(ResolvedSpreadArgData(
                                            reason.dupe(),
                                            arrtype.clone(),
                                            generic.clone(),
                                        )),
                                    ));
                                    resolve_spread_list_rec(
                                        cx,
                                        Some(trace),
                                        use_op.dupe(),
                                        reason_op,
                                        rrt_resolved,
                                        resolve_spread.rrt_unresolved.dupe(),
                                        resolve_spread.rrt_resolve_to.clone(),
                                    )?;
                                }
                                1 => {
                                    // To avoid infinite recursion, let's deconstruct to a simpler case
                                    // where we no longer resolve to a tuple but instead just resolve to
                                    // an array.
                                    rec_flow(
                                        cx,
                                        trace,
                                        (
                                            l,
                                            &UseT::new(UseTInner::ResolveSpreadT(Box::new(
                                                ResolveSpreadTData {
                                                    use_op: use_op.dupe(),
                                                    reason: reason_op.dupe(),
                                                    resolve_spread_type: Box::new(
                                                        ResolveSpreadType {
                                                            rrt_resolved: resolve_spread
                                                                .rrt_resolved
                                                                .dupe(),
                                                            rrt_unresolved: resolve_spread
                                                                .rrt_unresolved
                                                                .dupe(),
                                                            rrt_resolve_to:
                                                                SpreadResolve::ResolveSpreadsToArray(
                                                                    cfe_elem_t.dupe(),
                                                                    tout.dupe(),
                                                                ),
                                                        },
                                                    ),
                                                },
                                            ))),
                                        ),
                                    )?;
                                }
                                // We've already deconstructed, so there's nothing left to do
                                _ => {}
                            }
                            Ok(())
                        },
                    )?;
                }
                SpreadResolve::ResolveSpreadsToMultiflowCallFull(cfe_id, _)
                | SpreadResolve::ResolveSpreadsToMultiflowSubtypeFull(cfe_id, _)
                | SpreadResolve::ResolveSpreadsToMultiflowPartial(
                    box ResolveSpreadsToMultiflowPartialData(cfe_id, _, _, _),
                ) => {
                    let reason_elemt = reason_of_t(&elemt);
                    let pos = resolve_spread.rrt_resolved.len() as i32;
                    const_fold_expansion::guard(
                        cx,
                        *cfe_id,
                        (reason_elemt.dupe(), pos),
                        |recursion_depth| -> Result<(), FlowJsException> {
                            match recursion_depth {
                                0 => {
                                    // The first time we see this, we process it normally *)
                                    let mut rrt_resolved = resolve_spread.rrt_resolved.dupe();
                                    rrt_resolved.push_front(ResolvedParam::ResolvedSpreadArg(
                                        Box::new(ResolvedSpreadArgData(
                                            reason.dupe(),
                                            arrtype.clone(),
                                            generic.clone(),
                                        )),
                                    ));
                                    resolve_spread_list_rec(
                                        cx,
                                        Some(trace),
                                        use_op.dupe(),
                                        reason_op,
                                        rrt_resolved,
                                        resolve_spread.rrt_unresolved.dupe(),
                                        resolve_spread.rrt_resolve_to.clone(),
                                    )?;
                                }
                                1 => {
                                    //  Consider
                                    //
                                    // function foo(...args) { foo(1, ...args); }
                                    //  foo();
                                    //
                                    // Because args is unannotated, we try to infer it. However, due to
                                    // the constant folding we do with spread arguments, we'll first
                                    // infer that it is [], then [] | [1], then [] | [1] | [1,1] ...etc
                                    //
                                    // We can recognize that we're stuck in a constant folding loop. But
                                    // how to break it?
                                    //
                                    // In this case, we are constant folding by recognizing when args is
                                    // a tuple or an array literal. We can break the loop by turning
                                    // tuples or array literals into simple arrays.
                                    let new_arrtype = match &arrtype {
                                        // These can get us into constant folding loops
                                        ArrType::ArrayAT(box ArrayATData {
                                            elem_t,
                                            tuple_view: Some(_),
                                            react_dro,
                                        }) => ArrType::ArrayAT(Box::new(ArrayATData {
                                            elem_t: elem_t.dupe(),
                                            tuple_view: None,
                                            react_dro: react_dro.clone(),
                                        })),
                                        ArrType::TupleAT(box TupleATData {
                                            elem_t,
                                            react_dro,
                                            ..
                                        }) => ArrType::ArrayAT(Box::new(ArrayATData {
                                            elem_t: elem_t.dupe(),
                                            tuple_view: None,
                                            react_dro: react_dro.clone(),
                                        })),
                                        // These cannot
                                        ArrType::ArrayAT(box ArrayATData {
                                            tuple_view: None,
                                            ..
                                        })
                                        | ArrType::ROArrayAT(box (_, _)) => arrtype.clone(),
                                    };
                                    let mut rrt_resolved = resolve_spread.rrt_resolved.dupe();
                                    rrt_resolved.push_front(ResolvedParam::ResolvedSpreadArg(
                                        Box::new(ResolvedSpreadArgData(
                                            reason.dupe(),
                                            new_arrtype,
                                            generic.clone(),
                                        )),
                                    ));
                                    resolve_spread_list_rec(
                                        cx,
                                        Some(trace),
                                        use_op.dupe(),
                                        reason_op,
                                        rrt_resolved,
                                        resolve_spread.rrt_unresolved.dupe(),
                                        resolve_spread.rrt_resolve_to.clone(),
                                    )?;
                                }
                                _ => {}
                            }
                            Ok(())
                        },
                    )?;
                }
                // no caching
                SpreadResolve::ResolveSpreadsToArray(_, _) => {
                    let mut rrt_resolved = resolve_spread.rrt_resolved.dupe();
                    rrt_resolved.push_front(ResolvedParam::ResolvedSpreadArg(Box::new(
                        ResolvedSpreadArgData(reason.dupe(), arrtype.clone(), generic.clone()),
                    )));
                    resolve_spread_list_rec(
                        cx,
                        Some(trace),
                        use_op.dupe(),
                        reason_op,
                        rrt_resolved,
                        resolve_spread.rrt_unresolved.dupe(),
                        resolve_spread.rrt_resolve_to.clone(),
                    )?;
                }
            }
        }
        // ********************
        // * conditional type *
        // ********************
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ConditionalT(box ConditionalTData { use_op, tout, .. }),
        ) if matches!(def_t.deref(), DefTInner::EmptyT) => {
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (l, &open_tout))?;
        }
        (
            TypeInner::DefT(reason_tapp, def_t),
            UseTInner::ConditionalT(box ConditionalTData {
                use_op,
                reason: reason_op,
                ..
            }),
        ) if let DefTInner::PolyT(box PolyTData { tparams, t_out, .. }) = def_t.deref() => {
            let t_ = implicit_instantiation::kit::run_monomorphize(
                cx,
                trace,
                use_op.dupe(),
                reason_op,
                reason_tapp,
                Vec1::try_from_vec(tparams.to_vec()).unwrap(),
                t_out,
            )?;
            rec_flow(cx, trace, (&t_, u))?;
        }
        (
            _,
            UseTInner::ConditionalT(box ConditionalTData {
                use_op,
                reason,
                distributive_tparam_name: Some(name),
                infer_tparams,
                extends_t,
                true_t,
                false_t,
                tout,
            }),
        ) => {
            let subst = flow_js_utils::mk_distributive_tparam_subst_fn(
                cx,
                Some(use_op.dupe()),
                name.dupe(),
                l.dupe(),
            );
            let new_infer_tparams: Vec<_> = infer_tparams
                .iter()
                .map(|tparam| {
                    TypeParam::new(TypeParamInner {
                        bound: subst(tparam.bound.dupe()),
                        reason: tparam.reason.dupe(),
                        name: tparam.name.dupe(),
                        polarity: tparam.polarity,
                        default: tparam.default.dupe(),
                        is_this: tparam.is_this,
                        is_const: tparam.is_const,
                    })
                })
                .collect();
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::ConditionalT(Box::new(ConditionalTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        distributive_tparam_name: None,
                        infer_tparams: new_infer_tparams.into(),
                        extends_t: subst(extends_t.dupe()),
                        true_t: subst(true_t.dupe()),
                        false_t: subst(false_t.dupe()),
                        tout: tout.clone(),
                    }))),
                ),
            )?;
        }
        (
            _,
            UseTInner::ConditionalT(box ConditionalTData {
                use_op,
                reason,
                distributive_tparam_name: None,
                infer_tparams,
                extends_t,
                true_t,
                false_t,
                tout,
            }),
        ) => {
            let result = implicit_instantiation::kit::run_conditional(
                cx,
                trace,
                use_op.dupe(),
                reason,
                infer_tparams,
                l,
                extends_t,
                true_t,
                false_t,
            )?;
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (&result, &open_tout))?;
        }
        // singleton lower bounds are equivalent to the corresponding
        // primitive with a literal constraint. These conversions are
        // low precedence to allow equality exploits above, such as
        // the UnionT membership check, to fire.
        // TODO we can move to a single representation for singletons -
        // either SingletonFooT or (FooT <literal foo>) - if we can
        // ensure that their meaning as upper bounds is unambiguous.
        // Currently a SingletonFooT means the constrained type,
        // but the literal in (FooT <literal>) is a no-op.
        // Abstractly it should be totally possible to scrub literals
        // from the latter kind of flow, but it's unclear how difficult
        // it would be in practice.
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: u_inner,
            },
        ) if matches!(
            def_t.deref(),
            DefTInner::SingletonStrT { .. }
                | DefTInner::SingletonNumT { .. }
                | DefTInner::SingletonBoolT { .. }
        ) =>
        {
            let repos_l = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos_l, u_inner))?;
        }
        // NullProtoT is necessary as an upper bound, to distinguish between
        // (ObjT _, NullProtoT _) constraints and (ObjT _, DefT (_, NullT)), but as
        // a lower bound, it's the same as DefT (_, NullT)
        (TypeInner::NullProtoT(reason), _) => {
            let null_t = Type::new(TypeInner::DefT(reason.dupe(), DefT::new(DefTInner::NullT)));
            rec_flow(cx, trace, (&null_t, u))?;
        }
        // ********************
        // * mixin conversion *
        // ********************

        // A class can be viewed as a mixin by extracting its immediate properties,
        // and "erasing" its static and super
        (TypeInner::DefT(class_reason, def_t), UseTInner::MixinT(r, tvar))
            if let DefTInner::ClassT(inner) = def_t.deref()
                && let TypeInner::ThisInstanceT(box ThisInstanceTData {
                    instance,
                    is_this,
                    subst_name,
                    ..
                }) = inner.deref() =>
        {
            let inst = &instance.inst;
            let static_ = Type::new(TypeInner::ObjProtoT(r.dupe()));
            let super_ = Type::new(TypeInner::ObjProtoT(r.dupe()));
            let new_instance = InstanceT::new(InstanceTInner {
                static_,
                super_,
                implements: Rc::from([]),
                inst: inst.clone(),
            });
            let new_this_inst = Type::new(TypeInner::ThisInstanceT(Box::new(ThisInstanceTData {
                reason: r.dupe(),
                instance: new_instance,
                is_this: *is_this,
                subst_name: subst_name.dupe(),
            })));
            let new_l = Type::new(TypeInner::DefT(
                class_reason.dupe(),
                DefT::new(DefTInner::ClassT(new_this_inst)),
            ));
            rec_flow(
                cx,
                trace,
                (
                    &new_l,
                    &UseT::new(UseTInner::UseT(unknown_use(), tvar.dupe())),
                ),
            )?;
        }

        (TypeInner::DefT(_, def_t), UseTInner::MixinT(r, tvar))
            if let DefTInner::PolyT(box PolyTData {
                tparams_loc,
                tparams: xs,
                t_out,
                ..
            }) = def_t.deref()
                && let TypeInner::DefT(class_r, inner_def) = t_out.deref()
                && let DefTInner::ClassT(inner) = inner_def.deref()
                && let TypeInner::ThisInstanceT(box ThisInstanceTData {
                    instance,
                    is_this,
                    subst_name,
                    ..
                }) = inner.deref() =>
        {
            let inst = &instance.inst;
            let static_ = Type::new(TypeInner::ObjProtoT(r.dupe()));
            let super_ = Type::new(TypeInner::ObjProtoT(r.dupe()));
            let new_instance = InstanceT::new(InstanceTInner {
                static_,
                super_,
                implements: Rc::from([]),
                inst: inst.clone(),
            });
            let new_this_inst = Type::new(TypeInner::ThisInstanceT(Box::new(ThisInstanceTData {
                reason: r.dupe(),
                instance: new_instance,
                is_this: *is_this,
                subst_name: subst_name.dupe(),
            })));
            let class_t = Type::new(TypeInner::DefT(
                class_r.dupe(),
                DefT::new(DefTInner::ClassT(new_this_inst)),
            ));
            let poly_t = type_util::poly_type(
                poly::Id::generate_id(),
                tparams_loc.dupe(),
                Vec1::try_from_vec(xs.to_vec()).unwrap(),
                class_t,
            );
            rec_flow(
                cx,
                trace,
                (
                    &poly_t,
                    &UseT::new(UseTInner::UseT(unknown_use(), tvar.dupe())),
                ),
            )?;
        }
        (TypeInner::AnyT(_, src), UseTInner::MixinT(r, tvar)) => {
            let any = any_t::why(*src, r.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, tvar))?;
        }
        // TODO: it is conceivable that other things (e.g. functions) could also be
        // viewed as mixins (e.g. by extracting properties in their prototypes), but
        // such enhancements are left as future work.

        // ***************************************
        // * generic function may be specialized *
        // ***************************************

        // Instantiate a polymorphic definition using the supplied type
        // arguments. Use the instantiation cache if directed to do so by the
        // operation. (SpecializeT operations are created when processing TypeAppT
        // types, so the decision to cache or not originates there.)
        (
            TypeInner::DefT(_, def_t),
            UseTInner::SpecializeT(box SpecializeTData {
                use_op,
                reason: reason_op,
                reason2: reason_tapp,
                targs: ts,
                tvar,
            }),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc,
            tparams: xs,
            t_out: t,
            id,
        }) = def_t.deref() =>
        {
            let ts_val = ts
                .as_ref()
                .map(|rc| rc.dupe())
                .unwrap_or_else(|| Rc::from([]));
            let t_ = FlowJs::mk_typeapp_of_poly(
                cx,
                trace,
                use_op.dupe(),
                reason_op,
                reason_tapp,
                id.dupe(),
                tparams_loc.dupe(),
                Vec1::try_from_vec(xs.to_vec()).unwrap(),
                t,
                ts_val,
            )?;
            rec_flow_t(cx, trace, unknown_use(), (&t_, tvar))?;
        }
        // empty targs specialization of non-polymorphic classes is a no-op
        (
            TypeInner::DefT(_, def_t),
            UseTInner::SpecializeT(box SpecializeTData {
                use_op: _,
                reason: _,
                reason2: _,
                targs: None,
                tvar,
            }),
        ) if matches!(def_t.deref(), DefTInner::ClassT(_)) => {
            rec_flow_t(cx, trace, unknown_use(), (l, tvar))?;
        }
        (
            TypeInner::AnyT(_, _),
            UseTInner::SpecializeT(box SpecializeTData {
                use_op: _,
                reason: _,
                reason2: _,
                targs: _,
                tvar,
            }),
        ) => {
            // rec_flow_t ~use_op:unknown_use cx trace (l, tvar)
            rec_flow_t(cx, trace, unknown_use(), (l, tvar))?;
        }
        // this-specialize a this-abstracted class by substituting This
        (TypeInner::DefT(_, def_t), UseTInner::ThisSpecializeT(r, this, k))
            if let DefTInner::ClassT(inner) = def_t.deref()
                && let TypeInner::ThisInstanceT(box ThisInstanceTData {
                    reason: inst_r,
                    instance: i,
                    subst_name,
                    ..
                }) = inner.deref() =>
        {
            let mut map = FlowOrdMap::new();
            map.insert(subst_name.dupe(), this.dupe());
            let i = type_subst::subst_instance_type(
                cx,
                None,
                true,
                false,
                type_subst::Purpose::Normal,
                &map,
                i,
            );
            let def_t = Type::new(TypeInner::DefT(
                inst_r.dupe(),
                DefT::new(DefTInner::InstanceT(Rc::new(i))),
            ));
            continue_repos(cx, trace, r, false, &def_t, k)?;
        }
        // this-specialization of non-this-abstracted classes is a no-op
        (TypeInner::DefT(_, def_t), UseTInner::ThisSpecializeT(r, _this, k))
            if let DefTInner::ClassT(i) = def_t.deref() =>
        {
            // TODO: check that this is a subtype of i?
            continue_repos(cx, trace, r, false, i, k)?;
        }
        (TypeInner::AnyT(_, _), UseTInner::ThisSpecializeT(r, _, k)) => {
            // continue_repos cx trace r l k
            continue_repos(cx, trace, r, false, l, k)?;
        }
        //   rec_flow cx trace (reposition_reason cx ~trace reason ~use_desc l, u)
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: u_inner,
            },
        ) if matches!(def_t.deref(), DefTInner::PolyT(box _)) => {
            let repos_l = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos_l, u_inner))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: u_inner,
            },
        ) if matches!(def_t.deref(), DefTInner::ClassT(inner) if matches!(inner.deref(), TypeInner::ThisInstanceT(..))) =>
        {
            let repos_l = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos_l, u_inner))?;
        }
        // Special case for `_ instanceof C` where C is polymorphic
        (
            TypeInner::DefT(reason_tapp, def_t),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind:
                    ConcretizationKind::ConcretizeForPredicate(
                        PredicateConcretetizerVariant::ConcretizeRHSForInstanceOfPredicateTest,
                    ),
                ..
            }),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc,
            tparams: ids,
            t_out: t,
            ..
        }) = def_t.deref() =>
        {
            let reason_op = reason_of_use_t(u);
            let new_l = instantiate_poly_default_args(
                cx,
                trace,
                unknown_use(),
                reason_op,
                reason_tapp,
                tparams_loc.dupe(),
                Vec1::try_from_vec(ids.to_vec()).unwrap(),
                t.dupe(),
            )?;
            rec_flow(cx, trace, (&new_l, u))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForPredicate(_),
                collector,
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::PolyT(box _)) => {
            collector.add(l.dupe());
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForDestructuring,
                collector,
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::PolyT(box _)) => {
            collector.add(l.dupe());
        }

        // The rules below are hit when a polymorphic type appears outside a
        // type application expression - i.e. not followed by a type argument list
        // delimited by angle brackets.
        // We want to require full expressions in type positions like annotations,
        // but allow use of polymorphically-typed values - for example, in class
        // extends clauses and at function call sites - without explicit type
        // arguments, since typically they're easily inferred from context.
        //
        // We are calling the static callable method of a class. We need to be careful
        // not to apply the targs at this point, because this PolyT represents the class
        // and not the static function that's being called. We implicitly instantiate
        // the instance's tparams using the bounds and then forward the result original call
        // instead of consuming the method call's type arguments.
        //
        // We use the bounds to explicitly instantiate so that we don't create yet another implicit
        // instantiation here that would be un-annotatable.
        (
            TypeInner::DefT(reason_tapp, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                reason: reason_op,
                ..
            }),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc,
            tparams: ids,
            t_out: t,
            ..
        }) = def_t.deref()
            && let TypeInner::DefT(_, inner_def) = t.deref()
            && let DefTInner::ClassT(inner) = inner_def.deref()
            && let TypeInner::ThisInstanceT(..) = inner.deref() =>
        {
            let targs: Vec<Targ> = ids
                .iter()
                .map(|tparam| Targ::ExplicitArg(tparam.bound.dupe()))
                .collect();
            let t_ = instantiation_helpers::instantiate_with_targs(
                cx,
                trace,
                use_op.dupe(),
                reason_op,
                reason_tapp,
                (
                    tparams_loc.dupe(),
                    Vec1::try_from_vec(ids.to_vec()).unwrap(),
                    t.dupe(),
                ),
                targs,
            )?;
            rec_flow(cx, trace, (&t_, u))?;
        }
        // We use the ConcretizeCallee action to simplify types for hint decomposition.
        // After having instantiated polymorphic classes on static calls (case above),
        // we can just return the remaining polymorphic types, since there is not
        // much we can do about them here. These will be handled by the hint
        // decomposition code that has some knowledge of the call arguments.
        (
            TypeInner::DefT(_, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                call_action: box CallAction::ConcretizeCallee(tout),
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::PolyT(box _)) => {
            let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (l, &open_tout))?;
        }

        // Calls to polymorphic functions may cause non-termination, e.g. when the
        // results of the calls feed back as subtle variations of the original
        // arguments. This is similar to how we may have non-termination with
        // method calls on type applications. Thus, it makes sense to replicate
        // the specialization caching mechanism used in TypeAppT ~> MethodT to
        // avoid non-termination in PolyT ~> CallT.
        //
        // As it turns out, we need a bit more work here. A call may invoke
        // different cases of an overloaded polymorphic function on different
        // arguments, so we use the reasons of arguments in addition to the reason
        // of the call as keys for caching instantiations.
        //
        // On the other hand, even the reasons of arguments may not offer sufficient
        // distinguishing power when the arguments have not been concretized:
        // differently typed arguments could be incorrectly summarized by common
        // type variables they flow to, causing spurious errors.
        //
        // NOTE: This is probably not the final word on non-termination with
        // generics. We need to separate the double duty of reasons in the current
        // implementation as error positions and as caching keys. As error
        // positions we should be able to subject reasons to arbitrary tweaking,
        // without fearing regressions in termination guarantees.
        (
            TypeInner::DefT(reason_tapp, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                reason: reason_op,
                call_action: box CallAction::Funcalltype(box calltype),
                return_hint,
            }),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc,
            tparams: ids,
            t_out: t,
            ..
        }) = def_t.deref() =>
        {
            let lparts = (
                reason_tapp.dupe(),
                tparams_loc.dupe(),
                Vec1::try_from_vec(ids.to_vec()).unwrap(),
                t.dupe(),
            );
            let uparts = (
                use_op.dupe(),
                reason_op.dupe(),
                calltype.call_targs.dupe(),
                return_hint.clone(),
            );
            let l_clone = l.dupe();
            let tparams_loc_c = tparams_loc.dupe();
            let ids_c = ids.dupe();
            let t_c = t.dupe();
            let use_op_c = use_op.dupe();
            let reason_op_c = reason_op.dupe();
            let calltype_c = calltype.clone();
            let check = move || {
                flow_typing_implicit_instantiation_check::ImplicitInstantiationCheck::of_call(
                    l_clone.dupe(),
                    (
                        tparams_loc_c.dupe(),
                        Vec1::try_from_vec(ids_c.to_vec()).unwrap(),
                        t_c.dupe(),
                    ),
                    use_op_c.dupe(),
                    reason_op_c.dupe(),
                    calltype_c.clone(),
                )
            };
            let t_ = instantiate_poly_call_or_new(cx, trace, lparts, uparts, &check)?;
            let new_calltype = FuncallType {
                call_targs: None,
                ..calltype.clone()
            };
            let new_u = UseT::new(UseTInner::CallT(Box::new(CallTData {
                use_op: use_op.dupe(),
                reason: reason_op.dupe(),
                call_action: Box::new(CallAction::Funcalltype(Box::new(new_calltype))),
                return_hint: return_hint.clone(),
            })));
            rec_flow(cx, trace, (&t_, &new_u))?;
        }
        (TypeInner::DefT(reason_tapp, def_t), UseTInner::ConstructorT(box ctor_data))
            if let DefTInner::PolyT(box PolyTData {
                tparams_loc,
                tparams: ids,
                t_out: t,
                ..
            }) = def_t.deref() =>
        {
            let lparts = (
                reason_tapp.dupe(),
                tparams_loc.dupe(),
                Vec1::try_from_vec(ids.to_vec()).unwrap(),
                t.dupe(),
            );
            let uparts = (
                ctor_data.use_op.dupe(),
                ctor_data.reason.dupe(),
                ctor_data.targs.dupe(),
                ctor_data.return_hint.clone(),
            );
            let l_clone = l.dupe();
            let tparams_loc_c = tparams_loc.dupe();
            let ids_c = ids.dupe();
            let t_c = t.dupe();
            let use_op_c = ctor_data.use_op.dupe();
            let reason_op_c = ctor_data.reason.dupe();
            let targs_c = ctor_data.targs.dupe();
            let args_c = ctor_data.args.dupe();
            let check = move || {
                flow_typing_implicit_instantiation_check::ImplicitInstantiationCheck::of_ctor(
                    l_clone.dupe(),
                    (
                        tparams_loc_c.dupe(),
                        Vec1::try_from_vec(ids_c.to_vec()).unwrap(),
                        t_c.dupe(),
                    ),
                    use_op_c.dupe(),
                    reason_op_c.dupe(),
                    targs_c.dupe(),
                    args_c.dupe(),
                )
            };
            let t_ = instantiate_poly_call_or_new(cx, trace, lparts, uparts, &check)?;
            let new_u = UseT::new(UseTInner::ConstructorT(Box::new(ConstructorTData {
                use_op: ctor_data.use_op.dupe(),
                reason: ctor_data.reason.dupe(),
                targs: None,
                args: ctor_data.args.dupe(),
                tout: ctor_data.tout.dupe(),
                return_hint: ctor_data.return_hint.clone(),
                specialized_ctor: ctor_data.specialized_ctor.clone(),
            })));
            rec_flow(cx, trace, (&t_, &new_u))?;
        }
        (
            TypeInner::DefT(reason_tapp, def_t),
            UseTInner::ReactKitT(box ReactKitTData {
                use_op,
                reason: reason_op,
                tool,
            }),
        ) if let DefTInner::PolyT(box PolyTData {
            tparams_loc,
            tparams: ids,
            t_out: t,
            ..
        }) = def_t.deref()
            && let react::Tool::CreateElement(box react::CreateElementData {
                component,
                jsx_props,
                should_generalize,
                return_hint,
                targs,
                tout,
                specialized_component,
                ..
            }) = &**tool =>
        {
            let lparts = (
                reason_tapp.dupe(),
                tparams_loc.dupe(),
                Vec1::try_from_vec(ids.to_vec()).unwrap(),
                t.dupe(),
            );
            let uparts = (
                use_op.dupe(),
                reason_op.dupe(),
                targs.dupe(),
                return_hint.clone(),
            );
            let l_clone = l.dupe();
            let tparams_loc_c = tparams_loc.dupe();
            let ids_c = ids.dupe();
            let t_c = t.dupe();
            let use_op_c = use_op.dupe();
            let reason_op_c = reason_op.dupe();
            let component_c = component.dupe();
            let jsx_props_c = jsx_props.dupe();
            let targs_c = targs.dupe();
            let should_generalize_c = *should_generalize;
            let check = move || {
                flow_typing_implicit_instantiation_check::ImplicitInstantiationCheck::of_react_jsx(
                    l_clone.dupe(),
                    (
                        tparams_loc_c.dupe(),
                        Vec1::try_from_vec(ids_c.to_vec()).unwrap(),
                        t_c.dupe(),
                    ),
                    use_op_c.dupe(),
                    reason_op_c.dupe(),
                    component_c.dupe(),
                    jsx_props_c.dupe(),
                    targs_c.dupe(),
                    should_generalize_c,
                )
            };
            let (t_, inferred_targs) =
                instantiate_poly_call_or_new_with_soln(cx, trace, lparts, uparts, &check)?;
            let new_u = UseT::new(UseTInner::ReactKitT(Box::new(ReactKitTData {
                use_op: use_op.dupe(),
                reason: reason_op.dupe(),
                tool: Box::new(react::Tool::<Context<'cx>>::CreateElement(Box::new(
                    react::CreateElementData {
                        component: component.dupe(),
                        jsx_props: jsx_props.dupe(),
                        should_generalize: *should_generalize,
                        return_hint: return_hint.clone(),
                        targs: None,
                        tout: tout.dupe(),
                        record_monomorphized_result: true,
                        inferred_targs: Some(inferred_targs.into()),
                        specialized_component: specialized_component.clone(),
                    },
                ))),
            })));
            rec_flow(cx, trace, (&t_, &new_u))?;
        }
        (
            TypeInner::DefT(r, def_t),
            UseTInner::ReactKitT(box ReactKitTData {
                use_op: _,
                reason: _,
                tool,
            }),
        ) if let DefTInner::ObjT(obj) = def_t.deref()
            && obj.call_t.is_some()
            && matches!(
                &**tool,
                react::Tool::CreateElement(..)
                    | react::Tool::GetConfig { .. }
                    | react::Tool::ConfigCheck { .. }
            )
            && {
                let id = obj.call_t.as_ref().unwrap();
                let found = cx.find_call(id.dupe());
                match found.deref() {
                    TypeInner::DefT(_, inner_def)
                        if matches!(inner_def.deref(), DefTInner::PolyT(box PolyTData { t_out, .. })
                            if matches!(t_out.deref(), TypeInner::DefT(_, inner2) if matches!(inner2.deref(), DefTInner::FunT(_, _)))) =>
                    {
                        let fun_t_repos = type_util::mod_reason_of_t(&|_| r.dupe(), &found);
                        rec_flow(cx, trace, (&fun_t_repos, u))?;
                        true
                    }
                    _ => false,
                }
            } => {}
        (
            TypeInner::DefT(reason_tapp, def_t),
            UseTInner::ReactKitT(box ReactKitTData {
                use_op,
                reason: reason_op,
                tool,
            }),
        ) if let DefTInner::PolyT(box PolyTData { tparams, t_out, .. }) = def_t.deref()
            && matches!(&**tool, react::Tool::GetConfig { .. }) =>
        {
            let t_ = implicit_instantiation::kit::run_monomorphize(
                cx,
                trace,
                use_op.dupe(),
                reason_op,
                reason_tapp,
                Vec1::try_from_vec(tparams.to_vec()).unwrap(),
                t_out,
            )?;
            rec_flow(cx, trace, (&t_, u))?;
        }

        // ******************************
        // * functions statics - part A *
        // ******************************
        (
            TypeInner::DefT(l_reason, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: reason_call,
                prop_reason: reason_lookup,
                propref,
                method_action: action,
            }),
        ) if let Some((reason, static_)) = match def_t.deref() {
            DefTInner::FunT(static_, _) => Some((l_reason.dupe(), static_.dupe())),
            DefTInner::PolyT(box PolyTData { t_out, .. }) => {
                if let TypeInner::DefT(reason, inner) = t_out.deref()
                    && let DefTInner::FunT(static_, _) = inner.deref()
                {
                    Some((reason.dupe(), static_.dupe()))
                } else {
                    None
                }
            }
            _ => None,
        } =>
        {
            let method_type = flow_typing_tvar::mk_no_wrap_where(
                cx,
                reason_lookup.dupe(),
                |cx, tvar_reason, tvar_id| {
                    let tout = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                    let use_t = UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: reason_lookup.dupe(),
                        id: None,
                        from_annot: false,
                        skip_optional: false,
                        propref: propref.clone(),
                        tout: Box::new(tout),
                        hint: hint_unavailable(),
                    })));
                    rec_flow(
                        cx,
                        trace,
                        (
                            &static_,
                            &UseT::new(UseTInner::ReposLowerT {
                                reason: reason.dupe(),
                                use_desc: false,
                                use_t: Box::new(use_t),
                            }),
                        ),
                    )
                },
            )?;
            apply_method_action(
                cx,
                trace,
                &method_type,
                use_op.dupe(),
                reason_call.dupe(),
                l.dupe(),
                action,
            )?;
        }
        (TypeInner::DefT(reason_tapp, def_t), _)
            if let DefTInner::PolyT(box PolyTData {
                tparams_loc,
                tparams: ids,
                t_out: t,
                ..
            }) = def_t.deref() =>
        {
            let reason_op = reason_of_use_t(u);
            let use_op = match use_op_of_use_t(u) {
                Some(use_op) => use_op,
                None => unknown_use(),
            };
            let unify_bounds = match u.deref() {
                UseTInner::MethodT(box MethodTData {
                    use_op: _,
                    reason: _,
                    prop_reason: _,
                    propref: _,
                    method_action: box MethodAction::NoMethodAction(_),
                }) => true,
                _ => false,
            };
            let (t_, _) = FlowJs::instantiate_poly(
                cx,
                trace,
                use_op,
                reason_op,
                reason_tapp,
                Some(unify_bounds),
                (
                    tparams_loc.dupe(),
                    Vec1::try_from_vec(ids.to_vec()).unwrap(),
                    t.dupe(),
                ),
            )?;
            rec_flow(cx, trace, (&t_, u))?;
        }
        // when a this-abstracted class flows to upper bounds, fix the class
        (TypeInner::DefT(class_r, def_t), _)
            if let DefTInner::ClassT(inner) = def_t.deref()
                && let TypeInner::ThisInstanceT(box ThisInstanceTData {
                    reason: r,
                    instance: i,
                    is_this: this,
                    subst_name,
                }) = inner.deref() =>
        {
            let reason = reason_of_use_t(u);
            let fixed = flow_js_utils::fix_this_instance(
                cx,
                reason.dupe(),
                r.dupe(),
                i,
                *this,
                subst_name.dupe(),
            );
            let new_l = Type::new(TypeInner::DefT(
                class_r.dupe(),
                DefT::new(DefTInner::ClassT(fixed)),
            ));
            rec_flow(cx, trace, (&new_l, u))?;
        }
        (
            TypeInner::ThisInstanceT(box ThisInstanceTData {
                reason: r,
                instance: i,
                is_this: this,
                subst_name,
            }),
            _,
        ) => {
            let reason = reason_of_use_t(u);
            let fixed = flow_js_utils::fix_this_instance(
                cx,
                reason.dupe(),
                r.dupe(),
                i,
                *this,
                subst_name.dupe(),
            );
            rec_flow(cx, trace, (&fixed, u))?;
        }

        // *****************************
        // * React Abstract Components *
        // *****************************
        (TypeInner::DefT(r, def_t), _)
            if matches!(def_t.deref(), DefTInner::ReactAbstractComponentT(box _))
                && matches!(
                    u.deref(),
                    UseTInner::TestPropT(..)
                        | UseTInner::GetPropT(..)
                        | UseTInner::SetPropT(..)
                        | UseTInner::GetElemT(..)
                        | UseTInner::SetElemT(..)
                ) =>
        {
            let statics = FlowJs::get_builtin_type(
                cx,
                Some(trace),
                r,
                None,
                "React$AbstractComponentStatics",
            )?;
            rec_flow(cx, trace, (&statics, u))?;
        }
        // Components can never be called
        (
            TypeInner::DefT(r, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                reason,
                call_action: box CallAction::Funcalltype(box calltype),
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::ReactAbstractComponentT(box _)) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::ECannotCallReactComponent { reason: r.dupe() },
            )?;
            let any_err = any_t::error(reason.dupe());
            let open_tout = Type::new(TypeInner::OpenT(calltype.call_tout.dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (&any_err, &open_tout))?;
        }

        // ***********************************************
        // * function types deconstruct into their parts *
        // ***********************************************

        // FunT ~> CallT
        (
            TypeInner::DefT(_, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                call_action: box CallAction::ConcretizeCallee(tout),
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::FunT(_, _)) => {
            let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (l, &open_tout))?;
        }
        (
            TypeInner::DefT(reason_fundef, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                reason: reason_callsite,
                call_action: box CallAction::Funcalltype(box calltype),
                return_hint: _,
            }),
        ) if let DefTInner::FunT(_, funtype) = def_t.deref() => {
            let return_t = match &funtype.effect_ {
                ReactEffectType::HookDecl(_) | ReactEffectType::HookAnnot => {
                    if cx.react_rule_enabled(
                        flow_common::options::ReactRule::DeepReadOnlyHookReturns,
                    ) {
                        let ret_reason = reason_of_t(&funtype.return_t);
                        let destructor = Destructor::ReactDRO(Box::new(ReactDro(
                            reason_fundef.def_loc().dupe(),
                            DroType::HookReturn,
                        )));
                        Type::new(TypeInner::EvalT {
                            type_: funtype.return_t.dupe(),
                            defer_use_t: TypeDestructorT::new(TypeDestructorTInner(
                                unknown_use(),
                                ret_reason.dupe(),
                                Rc::new(destructor),
                            )),
                            id: eval::Id::generate_id(),
                        })
                    } else {
                        funtype.return_t.dupe()
                    }
                }
                ReactEffectType::ArbitraryEffect | ReactEffectType::AnyEffect => {
                    funtype.return_t.dupe()
                }
            };
            let (o1, _) = &funtype.this_t;
            let t1 = &return_t;
            let o2 = &calltype.call_this_t;
            let call_targs = &calltype.call_targs;
            let tins2 = &calltype.call_args_tlist;
            let t2 = &calltype.call_tout;
            let call_strict_arity = calltype.call_strict_arity;
            let call_specialized_callee = &calltype.call_specialized_callee;
            rec_flow(
                cx,
                trace,
                (o2, &UseT::new(UseTInner::UseT(use_op.dupe(), o1.dupe()))),
            )?;
            callee_recorder::add_callee(
                cx,
                callee_recorder::Kind::All,
                l.dupe(),
                call_specialized_callee.as_ref(),
            );
            if call_targs.is_some() {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                        call_loc: reason_callsite.loc().dupe(),
                        is_new: false,
                        reason_arity: reason_fundef.dupe(),
                        expected_arity: 0,
                    })),
                )?;
            }
            let modified_funtype = FunType {
                return_t: return_t.dupe(),
                ..(**funtype).clone()
            };
            if call_strict_arity {
                multiflow_call(
                    cx,
                    trace,
                    use_op.dupe(),
                    reason_callsite,
                    tins2,
                    &modified_funtype,
                )?;
            } else {
                FlowJs::multiflow_subtype(
                    cx,
                    trace,
                    use_op.dupe(),
                    reason_callsite,
                    tins2,
                    &modified_funtype,
                )?;
            }
            // flow return type of function to the tvar holding the return type of the
            // call. clears the op stack because the result of the call is not the
            // call itself.
            let repos_t1 = helpers::reposition(
                cx,
                Some(trace),
                reason_callsite.loc().dupe(),
                None,
                None,
                t1.dupe(),
            )?;
            let open_t2 = Type::new(TypeInner::OpenT(t2.dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&repos_t1, &open_t2))?;
        }
        (
            TypeInner::AnyT(_, _),
            UseTInner::CallT(box CallTData {
                use_op,
                call_action: box CallAction::ConcretizeCallee(tout),
                ..
            }),
        ) => {
            let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (l, &open_tout))?;
        }
        (
            TypeInner::AnyT(reason_fundef, src),
            UseTInner::CallT(box CallTData {
                use_op,
                reason: reason_op,
                call_action: box CallAction::Funcalltype(box calltype),
                return_hint: _,
            }),
        ) => {
            let call_this_t = &calltype.call_this_t;
            // An untyped receiver can't do anything with type args
            let call_args_tlist = &calltype.call_args_tlist;
            let call_tout = &calltype.call_tout;
            let call_specialized_callee = &calltype.call_specialized_callee;
            callee_recorder::add_callee(
                cx,
                callee_recorder::Kind::All,
                l.dupe(),
                call_specialized_callee.as_ref(),
            );
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, reason_fundef.dupe());
            rec_flow_t(cx, trace, use_op.dupe(), (call_this_t, &any))?;
            call_args_iter(
                |t| {
                    rec_flow(
                        cx,
                        trace,
                        (t, &UseT::new(UseTInner::UseT(use_op.dupe(), any.dupe()))),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
                call_args_tlist,
            )?;
            let any_op = any_t::why(src, reason_op.dupe());
            let open_tout = Type::new(TypeInner::OpenT(call_tout.dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (&any_op, &open_tout))?;
        }
        (
            _,
            UseTInner::ReactKitT(box ReactKitTData {
                use_op,
                reason: reason_op,
                tool,
            }),
        ) => {
            // ReactJs.run cx trace ~use_op reason_op l tool
            react_kit::run(cx, trace, use_op.dupe(), reason_op, l, tool)?;
        }

        // ****************************************
        // * You can cast an object to a function *
        // ****************************************
        (
            TypeInner::DefT(reason, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                reason: reason_op,
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::ObjT(_) | DefTInner::InstanceT(_)) => {
            let prop_name = Some(Name::new(FlowSmolStr::new_inline("$call")));
            let fun_t = match def_t.deref() {
                DefTInner::ObjT(obj) if let Some(id) = obj.call_t.as_ref() => cx.find_call(*id),
                DefTInner::InstanceT(inst_t) if let Some(id) = inst_t.inst.inst_call_t.as_ref() => {
                    cx.find_call(*id)
                }
                _ => {
                    let reason_prop = reason_op
                        .dupe()
                        .replace_desc(VirtualReasonDesc::RProperty(prop_name.dupe()));
                    let error_message =
                        ErrorMessage::EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                            reason_prop: reason_prop.dupe(),
                            reason_obj: reason.dupe(),
                            prop_name,
                            use_op: use_op.dupe(),
                            suggestion: None,
                        }));
                    flow_js_utils::add_output(cx, error_message)?;
                    any_t::error(reason_op.dupe())
                }
            };
            let repos_fun_t =
                helpers::reposition(cx, Some(trace), reason.loc().dupe(), None, None, fun_t)?;
            rec_flow(cx, trace, (&repos_fun_t, u))?;
        }
        (TypeInner::AnyT(_, src), UseTInner::ObjTestT(reason_op, _, u_inner)) => {
            let any = any_t::why(*src, reason_op.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, u_inner))?;
        }
        (_, UseTInner::ObjTestT(reason_op, default, u_inner)) => {
            let repos_use = UseT::new(UseTInner::ReposLowerT {
                reason: reason_op.dupe(),
                use_desc: false,
                use_t: Box::new(UseT::new(UseTInner::UseT(unknown_use(), u_inner.dupe()))),
            });
            if flow_js_utils::object_like(l) {
                rec_flow(cx, trace, (l, &repos_use))?;
            } else {
                rec_flow(cx, trace, (default, &repos_use))?;
            }
        }
        (TypeInner::AnyT(_, src), UseTInner::ObjTestProtoT(reason_op, u_inner)) => {
            let any = any_t::why(*src, reason_op.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, u_inner))?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::ObjTestProtoT(reason_op, u_inner))
            if matches!(def_t.deref(), DefTInner::NullT) =>
        {
            let null_proto = Type::new(TypeInner::NullProtoT(
                reason_op.dupe().replace_desc(VirtualReasonDesc::RNull),
            ));
            rec_flow_t(cx, trace, unknown_use(), (&null_proto, u_inner))?;
        }
        (_, UseTInner::ObjTestProtoT(reason_op, u_inner)) => {
            let proto = if flow_js_utils::object_like(l) {
                helpers::reposition(
                    cx,
                    Some(trace),
                    reason_op.loc().dupe(),
                    None,
                    None,
                    l.dupe(),
                )?
            } else {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::EInvalidPrototype(Box::new((
                        reason_op.loc().dupe(),
                        reason_of_t(l).dupe(),
                    ))),
                )?;
                Type::new(TypeInner::ObjProtoT(
                    reason_op
                        .dupe()
                        .replace_desc(VirtualReasonDesc::RObjectPrototype),
                ))
            };
            rec_flow_t(cx, trace, unknown_use(), (&proto, u_inner))?;
        }

        // **************************************************
        // * instances of classes follow declared hierarchy *
        // **************************************************
        (
            TypeInner::DefT(reason, def_t),
            UseTInner::ExtendsUseT(box ExtendsUseTData {
                use_op,
                reason: reason_op,
                targs: try_ts_on_failure,
                true_t: ext_l,
                false_t: ext_u,
            }),
        ) if let DefTInner::InstanceT(inst_t) = def_t.deref()
            && let TypeInner::DefT(reason_u, inner_def_u) = ext_u.deref()
            && let DefTInner::InstanceT(inst_super_t) = inner_def_u.deref() =>
        {
            let super_ = &inst_t.super_;
            let implements = &inst_t.implements;
            let inst = &inst_t.inst;
            let static_ = &inst_t.static_;
            let inst_super = &inst_super_t.inst;
            let static_super = &inst_super_t.static_;
            let super_super = &inst_super_t.super_;
            if flow_js_utils::is_same_instance_type(inst_t, inst_super_t) {
                let tmap1 = inst.type_args.dupe();
                let tmap2 = inst_super.type_args.dupe();
                let ureason = reason_op.dupe().update_desc_new(|desc| match desc {
                    VirtualReasonDesc::RExtends(inner_desc) => inner_desc.as_ref().clone(),
                    other => other,
                });
                flow_type_args(cx, trace, use_op.dupe(), reason, &ureason, tmap1, tmap2)?;
            } else if type_util::nominal_id_have_same_logical_module(
                &cx.file_options(),
                cx.projects_options(),
                (&inst.class_id, inst.class_name.as_deref()),
                (&inst_super.class_id, inst_super.class_name.as_deref()),
            ) && inst.type_args.len() == inst_super.type_args.len()
            {
                if type_util::is_in_common_interface_conformance_check(use_op) {
                    let implements_use_op =
                        VirtualUseOp::Op(Arc::new(VirtualRootUseOp::ClassImplementsCheck(
                            Box::new(ClassImplementsCheckData {
                                def: reason.dupe(),
                                name: reason.dupe(),
                                implements: reason_u.dupe(),
                            }),
                        )));
                    // We need to ensure that the shape of the class instances match.
                    let obj1 = inst_type_to_obj_type(
                        cx,
                        reason.dupe(),
                        inst.own_props.dupe(),
                        inst.proto_props.dupe(),
                        inst.inst_call_t,
                        &inst.inst_dict,
                    );
                    let obj2 = inst_type_to_obj_type(
                        cx,
                        reason_u.dupe(),
                        inst_super.own_props.dupe(),
                        inst_super.proto_props.dupe(),
                        inst_super.inst_call_t,
                        &inst_super.inst_dict,
                    );
                    rec_unify(
                        cx,
                        trace,
                        implements_use_op.clone(),
                        UnifyCause::Uncategorized,
                        None,
                        &obj1,
                        &obj2,
                    )?;
                    // We need to ensure that the shape of the class statics match.
                    let spread_of = |reason: &Reason, t: &Type| -> Type {
                        let id = eval::Id::generate_id();
                        let destructor =
                            Rc::new(Destructor::SpreadType(Box::new(DestructorSpreadTypeData(
                                object::spread::Target::Annot { make_exact: false },
                                flow_data_structure_wrapper::list::FlowOcamlList::new(),
                                None,
                            ))));
                        Type::new(TypeInner::EvalT {
                            type_: t.dupe(),
                            defer_use_t: TypeDestructorT::new(TypeDestructorTInner(
                                use_op.dupe(),
                                reason.dupe(),
                                destructor,
                            )),
                            id,
                        })
                    };
                    let spread1 = spread_of(reason, static_);
                    let spread2 = spread_of(reason_u, static_super);
                    rec_unify(
                        cx,
                        trace,
                        implements_use_op,
                        UnifyCause::Uncategorized,
                        None,
                        &spread1,
                        &spread2,
                    )?;
                    // We need to ensure that the classes have the same nominal hierarchy
                    rec_flow_t(cx, trace, use_op.dupe(), (super_, super_super))?;
                }
                // We need to ensure that the classes have the matching targs
                flow_type_args(
                    cx,
                    trace,
                    use_op.dupe(),
                    reason,
                    reason_u,
                    inst.type_args.dupe(),
                    inst_super.type_args.dupe(),
                )?;
            } else {
                // If this instance type has declared implementations, any structural
                // tests have already been performed at the declaration site. We can
                // then use the ExtendsUseT use type to search for a nominally matching
                // implementation, thereby short-circuiting a potentially expensive
                // structural test at the use site.
                let mut combined = try_ts_on_failure.to_vec();
                combined.extend(implements.iter().duped());
                let use_t = UseT::new(UseTInner::ExtendsUseT(Box::new(ExtendsUseTData {
                    use_op: use_op.dupe(),
                    reason: reason_op.dupe(),
                    targs: combined.into(),
                    true_t: ext_l.dupe(),
                    false_t: ext_u.dupe(),
                })));
                let repos_use = UseT::new(UseTInner::ReposLowerT {
                    reason: reason.dupe(),
                    use_desc: false,
                    use_t: Box::new(use_t),
                });
                rec_flow(cx, trace, (super_, &repos_use))?;
            }
        }

        // *********************************************************
        // * class types derive instance types (with constructors) *
        // *********************************************************
        (TypeInner::DefT(reason, def_t), UseTInner::ConstructorT(box ctor_data))
            if let DefTInner::ClassT(this) = def_t.deref() =>
        {
            let use_op = &ctor_data.use_op;
            let reason_op = &ctor_data.reason;
            let targs = &ctor_data.targs;
            let args = &ctor_data.args;
            let t = &ctor_data.tout;
            let return_hint = &ctor_data.return_hint;
            let specialized_ctor = &ctor_data.specialized_ctor;
            let reason_o = reason
                .dupe()
                .replace_desc(VirtualReasonDesc::RConstructorVoidReturn);
            let annot_loc = reason_op.loc().dupe();
            // early error if type args passed to non-polymorphic class
            if targs.is_some() {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                        call_loc: annot_loc.dupe(),
                        is_new: true,
                        reason_arity: reason_of_t(this).dupe(),
                        expected_arity: 0,
                    })),
                )?;
            }
            // call this.constructor(args)
            let ret = flow_typing_tvar::mk_no_wrap_where(
                cx,
                reason_op.dupe(),
                |cx, tvar_reason, tvar_id| {
                    let tout = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                    let funtype = MethodCallType {
                        meth_generic_this: None,
                        meth_targs: None,
                        meth_args_tlist: args.dupe(),
                        meth_tout: tout,
                        meth_strict_arity: true,
                    };
                    let propref = mk_named_prop(
                        reason_o.dupe(),
                        false,
                        Name::new(FlowSmolStr::new_inline("constructor")),
                    );
                    rec_flow(
                        cx,
                        trace,
                        (
                            this,
                            &UseT::new(UseTInner::MethodT(Box::new(MethodTData {
                                use_op: use_op.dupe(),
                                reason: reason_op.dupe(),
                                prop_reason: reason_o.dupe(),
                                propref: Box::new(propref),
                                method_action: Box::new(MethodAction::CallM(Box::new(CallMData {
                                    methodcalltype: funtype,
                                    return_hint: return_hint.clone(),
                                    specialized_callee: specialized_ctor.clone(),
                                }))),
                            }))),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
            )?;
            let annot_r = reason_op
                .dupe()
                .reposition(annot_loc.dupe())
                .annotate(annot_loc);
            // return this
            rec_flow(
                cx,
                trace,
                (
                    &ret,
                    &UseT::new(UseTInner::ObjTestT(annot_r, this.dupe(), t.dupe())),
                ),
            )?;
        }
        (TypeInner::AnyT(_, src), UseTInner::ConstructorT(box ctor_data)) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any_op = any_t::why(src, ctor_data.reason.dupe());
            // An untyped receiver can't do anything with type args
            call_args_iter(
                |arg_t| {
                    rec_flow(
                        cx,
                        trace,
                        (
                            arg_t,
                            &UseT::new(UseTInner::UseT(ctor_data.use_op.dupe(), any_op.dupe())),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
                &ctor_data.args,
            )?;
            rec_flow_t(cx, trace, unknown_use(), (&any_op, &ctor_data.tout))?;
        }
        // Interfaces with construct signatures (method named "new") can be constructed.
        // We speculatively check for "new" through the constraint system. If the property
        // exists, dispatch the real method call. If not, produce a clear error.
        (TypeInner::DefT(reason, def_t), UseTInner::ConstructorT(box ctor_data))
            if let DefTInner::InstanceT(_instance_t) = def_t.deref() =>
        {
            let reason_o = reason
                .dupe()
                .replace_desc(VirtualReasonDesc::RConstructorVoidReturn);
            let propref = mk_named_prop(
                reason_o.dupe(),
                false,
                Name::new(FlowSmolStr::new_inline("new")),
            );
            let use_op = &ctor_data.use_op;
            let reason_op = &ctor_data.reason;
            let targs = &ctor_data.targs;
            let args = &ctor_data.args;
            let t = &ctor_data.tout;
            let return_hint = &ctor_data.return_hint;
            let specialized_ctor = &ctor_data.specialized_ctor;
            // Speculatively probe for "new" without calling it
            let probe_u = UseT::new(UseTInner::MethodT(Box::new(MethodTData {
                use_op: use_op.dupe(),
                reason: reason_op.dupe(),
                prop_reason: reason_o.dupe(),
                propref: Box::new(propref.clone()),
                method_action: Box::new(MethodAction::NoMethodAction(flow_typing_tvar::mk(
                    cx,
                    reason_op.dupe(),
                ))),
            })));
            match speculation_kit::try_singleton_throw_on_failure(cx, trace, l.dupe(), probe_u) {
                Ok(()) => {
                    // "new" exists; dispatch the real method call
                    let ret = flow_typing_tvar::mk_no_wrap_where(
                        cx,
                        reason_op.dupe(),
                        |cx, tvar_reason, tvar_id| {
                            let tout = Tvar::new(tvar_reason.dupe(), tvar_id as u32);
                            let funtype = MethodCallType {
                                meth_generic_this: None,
                                meth_targs: targs.clone(),
                                meth_args_tlist: args.dupe(),
                                meth_tout: tout,
                                meth_strict_arity: true,
                            };
                            rec_flow(
                                cx,
                                trace,
                                (
                                    l,
                                    &UseT::new(UseTInner::MethodT(Box::new(MethodTData {
                                        use_op: use_op.dupe(),
                                        reason: reason_op.dupe(),
                                        prop_reason: reason_o.dupe(),
                                        propref: Box::new(propref.clone()),
                                        method_action: Box::new(MethodAction::CallM(Box::new(
                                            CallMData {
                                                methodcalltype: funtype,
                                                return_hint: return_hint.clone(),
                                                specialized_callee: specialized_ctor.clone(),
                                            },
                                        ))),
                                    }))),
                                ),
                            )?;
                            Ok::<(), FlowJsException>(())
                        },
                    )?;
                    rec_flow_t(cx, trace, use_op.dupe(), (&ret, t))?;
                }
                Err(FlowJsException::SpeculationSingletonError) => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EInvalidConstructor(reason_of_t(l).dupe()),
                    )?;
                    let any_err = any_t::error(reason_op.dupe());
                    rec_flow_t(cx, trace, use_op.dupe(), (&any_err, t))?;
                }
                Err(e) => return Err(e),
            }
        }
        // Only classes (and `any`) can be constructed.
        (_, UseTInner::ConstructorT(box ctor_data)) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EInvalidConstructor(reason_of_t(l).dupe()),
            )?;
            let any_err = any_t::error(ctor_data.reason.dupe());
            rec_flow_t(
                cx,
                trace,
                ctor_data.use_op.dupe(),
                (&any_err, &ctor_data.tout),
            )?;
        }
        // Since we don't know the signature of a method on AnyT, assume every
        // parameter is an AnyT.
        (
            TypeInner::AnyT(_, src),
            UseTInner::MethodT(box MethodTData {
                use_op: _,
                reason: _,
                prop_reason: _,
                propref,
                method_action: box MethodAction::NoMethodAction(prop_t),
            }),
        ) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, type_util::reason_of_propref(propref).dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, prop_t))?;
        }
        (TypeInner::AnyT(_, src), UseTInner::PrivateMethodT(box pm_data))
            if let MethodAction::NoMethodAction(prop_t) = pm_data.method_action.as_ref() =>
        {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, pm_data.prop_reason.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, prop_t))?;
        }
        (
            TypeInner::AnyT(_, src),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: reason_op,
                prop_reason: _,
                propref: _,
                method_action:
                    box MethodAction::CallM(box CallMData {
                        methodcalltype,
                        specialized_callee,
                        ..
                    }),
            }),
        ) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, reason_op.dupe());
            call_args_iter(
                |t| {
                    rec_flow(
                        cx,
                        trace,
                        (t, &UseT::new(UseTInner::UseT(use_op.dupe(), any.dupe()))),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
                &methodcalltype.meth_args_tlist,
            )?;
            callee_recorder::add_callee(
                cx,
                callee_recorder::Kind::Tast,
                l.dupe(),
                specialized_callee.as_ref(),
            );
            let open_tout = Type::new(TypeInner::OpenT(methodcalltype.meth_tout.dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&any, &open_tout))?;
        }
        (TypeInner::AnyT(_, src), UseTInner::PrivateMethodT(box pm_data))
            if let MethodAction::CallM(box CallMData {
                methodcalltype,
                specialized_callee,
                ..
            }) = pm_data.method_action.as_ref() =>
        {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, pm_data.reason.dupe());
            call_args_iter(
                |t| {
                    rec_flow(
                        cx,
                        trace,
                        (
                            t,
                            &UseT::new(UseTInner::UseT(pm_data.use_op.dupe(), any.dupe())),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
                &methodcalltype.meth_args_tlist,
            )?;
            callee_recorder::add_callee(
                cx,
                callee_recorder::Kind::Tast,
                l.dupe(),
                specialized_callee.as_ref(),
            );
            let open_tout = Type::new(TypeInner::OpenT(methodcalltype.meth_tout.dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&any, &open_tout))?;
        }
        (
            TypeInner::AnyT(_, src),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: reason_op,
                prop_reason: _,
                propref: _,
                method_action: box chain @ MethodAction::ChainM(..),
            }),
        ) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, reason_op.dupe());
            apply_method_action(
                cx,
                trace,
                &any,
                use_op.dupe(),
                reason_op.dupe(),
                l.dupe(),
                chain,
            )?;
        }
        (TypeInner::AnyT(_, src), UseTInner::PrivateMethodT(box pm_data))
            if let chain @ MethodAction::ChainM(..) = pm_data.method_action.as_ref() =>
        {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, pm_data.reason.dupe());
            apply_method_action(
                cx,
                trace,
                &any,
                pm_data.use_op.dupe(),
                pm_data.reason.dupe(),
                l.dupe(),
                chain,
            )?;
        }
        // *************************
        // * statics can be read   *
        // *************************
        (TypeInner::DefT(_, def_t), UseTInner::GetStaticsT(tout))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            let static_ = &inst_t.static_;
            let reason_op = tout.reason();
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow(
                cx,
                trace,
                (
                    static_,
                    &UseT::new(UseTInner::ReposLowerT {
                        reason: reason_op.dupe(),
                        use_desc: false,
                        use_t: Box::new(UseT::new(UseTInner::UseT(unknown_use(), open_tout))),
                    }),
                ),
            )?;
        }
        (TypeInner::AnyT(_, src), UseTInner::GetStaticsT(tout)) => {
            let reason_op = tout.reason();
            let any = any_t::why(*src, reason_op.dupe());
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&any, &open_tout))?;
        }
        (TypeInner::ObjProtoT(_), UseTInner::GetStaticsT(tout)) => {
            let reason_op = tout.reason();
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::ReposLowerT {
                        reason: reason_op.dupe(),
                        use_desc: false,
                        use_t: Box::new(UseT::new(UseTInner::UseT(unknown_use(), open_tout))),
                    }),
                ),
            )?;
        }
        // ********************
        // * __proto__ getter *
        // ********************

        // TODO: Fix GetProtoT for InstanceT (and ClassT).
        // The __proto__ object of an instance is an ObjT having the properties in
        // insttype.methods_tmap, not the super instance.
        (TypeInner::DefT(_, def_t), UseTInner::GetProtoT(reason_op, t))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            let super_ = &inst_t.super_;
            let proto = helpers::reposition(
                cx,
                Some(trace),
                reason_op.loc().dupe(),
                None,
                None,
                super_.dupe(),
            )?;
            let open_t = Type::new(TypeInner::OpenT((**t).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&proto, &open_t))?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::GetProtoT(reason_op, t))
            if let DefTInner::ObjT(obj) = def_t.deref() =>
        {
            let proto_t = &obj.proto_t;
            let proto = helpers::reposition(
                cx,
                Some(trace),
                reason_op.loc().dupe(),
                None,
                None,
                proto_t.dupe(),
            )?;
            let open_t = Type::new(TypeInner::OpenT((**t).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&proto, &open_t))?;
        }
        (TypeInner::ObjProtoT(_), UseTInner::GetProtoT(reason_op, t)) => {
            let proto = null::why(reason_op.dupe());
            let open_t = Type::new(TypeInner::OpenT((**t).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&proto, &open_t))?;
        }
        (TypeInner::FunProtoT(reason), UseTInner::GetProtoT(reason_op, t)) => {
            let proto = Type::new(TypeInner::ObjProtoT(
                reason.dupe().reposition(reason_op.loc().dupe()),
            ));
            let open_t = Type::new(TypeInner::OpenT((**t).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&proto, &open_t))?;
        }
        (TypeInner::AnyT(_, src), UseTInner::GetProtoT(reason_op, t)) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let proto = any_t::why(src, reason_op.dupe());
            let open_t = Type::new(TypeInner::OpenT((**t).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&proto, &open_t))?;
        }

        // ********************
        // * __proto__ setter *
        // ********************
        (TypeInner::AnyT(_, _), UseTInner::SetProtoT(_, _)) => {}
        (_, UseTInner::SetProtoT(reason_op, _)) => {
            flow_js_utils::add_output(cx, ErrorMessage::EUnsupportedSetProto(reason_op.dupe()))?;
        }

        // ********************************************************
        // * instances of classes may have their fields looked up *
        // ********************************************************
        (
            TypeInner::DefT(lreason, def_t),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind: kind,
                try_ts_on_failure,
                propref,
                lookup_action: action,
                ids,
                method_accessible,
                ignore_dicts,
            }),
        ) if let DefTInner::InstanceT(inst_t) = def_t.deref() => {
            let super_ = &inst_t.super_;
            let inst = &inst_t.inst;
            let use_op = use_op_of_lookup_action(action);
            match flow_js_utils::get_prop_t_kit::get_instance_prop::<FlowJs>(
                cx,
                &trace,
                &use_op,
                *ignore_dicts,
                inst,
                propref,
                reason_op,
            )? {
                Some((p, target_kind)) => {
                    let p = flow_js_utils::check_method_unbinding(
                        cx,
                        &use_op,
                        *method_accessible,
                        reason_op,
                        propref,
                        &hint_unavailable(),
                        p,
                    )?;
                    if let LookupKind::NonstrictReturning(box NonstrictReturningData(
                        _,
                        Some((id, _)),
                    )) = &**kind
                    {
                        cx.test_prop_hit(*id);
                    }
                    let property_type = property::property_type(&p);
                    perform_lookup_action(
                        cx,
                        trace,
                        propref,
                        &property_type,
                        target_kind,
                        lreason,
                        reason_op,
                        action,
                    )?;
                }
                None => {
                    let new_ids = ids.as_ref().map(|s| {
                        if s.contains(&inst.own_props) || s.contains(&inst.proto_props) {
                            s.dupe()
                        } else {
                            let mut new_set = s.dupe();
                            new_set.insert(inst.own_props.dupe());
                            new_set.insert(inst.proto_props.dupe());
                            new_set
                        }
                    });
                    rec_flow(
                        cx,
                        trace,
                        (
                            super_,
                            &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                                reason: reason_op.dupe(),
                                lookup_kind: kind.clone(),
                                try_ts_on_failure: try_ts_on_failure.clone(),
                                propref: propref.clone(),
                                lookup_action: action.clone(),
                                ids: new_ids,
                                method_accessible: *method_accessible,
                                ignore_dicts: *ignore_dicts,
                            }))),
                        ),
                    )?;
                }
            }
        }
        // ********************************
        // * ... and their fields written *
        // ********************************
        (TypeInner::DefT(_, def_t), UseTInner::SetPropT(use_op, _, propref, _, _, _, _))
            if let DefTInner::InstanceT(inst_t) = def_t.deref()
                && inst_t.inst.inst_react_dro.is_some()
                && !flow_js_utils::is_exception_to_react_dro(propref) =>
        {
            let ReactDro(dro_loc, dro_type) = inst_t.inst.inst_react_dro.as_ref().unwrap();
            let reason_prop = type_util::reason_of_propref(propref);
            let prop_name = name_of_propref(propref);
            let use_op = UseOp::Frame(
                std::sync::Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                    dro_loc.dupe(),
                    dro_type.clone(),
                )))),
                Arc::new(use_op.dupe()),
            );
            add_output(
                cx,
                ErrorMessage::EPropNotWritable(Box::new(EPropNotWritableData {
                    reason_prop: reason_prop.dupe(),
                    prop_name,
                    use_op,
                })),
            )?;
        }
        (
            TypeInner::DefT(reason_instance, def_t),
            UseTInner::SetPropT(use_op, reason_op, propref, mode, write_ctx, tin, prop_tout),
        ) if matches!(def_t.deref(), DefTInner::InstanceT(_)) => {
            let lookup_action = LookupAction::WriteProp(Box::new(WritePropData {
                use_op: use_op.dupe(),
                obj_t: l.dupe(),
                prop_tout: prop_tout.as_ref().map(|t| t.dupe()),
                tin: tin.dupe(),
                write_ctx: write_ctx.clone(),
                mode: mode.clone(),
            }));
            let method_accessible = true;
            let lookup_kind = instance_lookup_kind(
                cx,
                trace,
                reason_instance,
                reason_op,
                method_accessible,
                l,
                propref,
                lookup_action.clone(),
            )?;
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                        reason: reason_op.dupe(),
                        lookup_kind: Box::new(lookup_kind),
                        try_ts_on_failure: Rc::from([]),
                        propref: propref.clone(),
                        lookup_action: Box::new(lookup_action),
                        ids: None,
                        method_accessible,
                        ignore_dicts: true,
                    }))),
                ),
            )?;
        }
        (TypeInner::DefT(reason_c, def_t), UseTInner::SetPrivatePropT(box spp_data))
            if matches!(def_t.deref(), DefTInner::InstanceT(_))
                && spp_data.class_bindings.is_empty() =>
        {
            add_output(
                cx,
                ErrorMessage::EPrivateLookupFailed(Box::new((
                    (spp_data.reason.dupe(), reason_c.dupe()),
                    Name::new(spp_data.name.dupe()),
                    spp_data.use_op.dupe(),
                ))),
            )?;
        }
        (TypeInner::DefT(reason_c, def_t), UseTInner::SetPrivatePropT(box spp_data))
            if let DefTInner::InstanceT(inst_t) = def_t.deref()
                && !spp_data.class_bindings.is_empty() =>
        {
            let inst = &inst_t.inst;
            let scope = &spp_data.class_bindings[0];
            let rest_scopes = &spp_data.class_bindings[1..];
            if scope.class_binding_id != inst.class_id {
                rec_flow(
                    cx,
                    trace,
                    (
                        l,
                        &UseT::new(UseTInner::SetPrivatePropT(Box::new(SetPrivatePropTData {
                            use_op: spp_data.use_op.dupe(),
                            reason: spp_data.reason.dupe(),
                            name: spp_data.name.dupe(),
                            set_mode: spp_data.set_mode.clone(),
                            class_bindings: rest_scopes.to_vec().into(),
                            static_: spp_data.static_,
                            write_ctx: spp_data.write_ctx.clone(),
                            tin: spp_data.tin.dupe(),
                            tout: spp_data.tout.as_ref().map(|t| t.dupe()),
                        }))),
                    ),
                )?;
            } else {
                let map = if spp_data.static_ {
                    inst.class_private_static_fields.dupe()
                } else {
                    inst.class_private_fields.dupe()
                };
                let name = Name::new(spp_data.name.dupe());
                let props = cx.find_props(map);
                match props.get(&name) {
                    None => {
                        add_output(
                            cx,
                            ErrorMessage::EPrivateLookupFailed(Box::new((
                                (spp_data.reason.dupe(), reason_c.dupe()),
                                name,
                                spp_data.use_op.dupe(),
                            ))),
                        )?;
                    }
                    Some(p) => {
                        let action = LookupAction::WriteProp(Box::new(WritePropData {
                            use_op: spp_data.use_op.dupe(),
                            obj_t: l.dupe(),
                            prop_tout: spp_data.tout.as_ref().map(|t| t.dupe()),
                            tin: spp_data.tin.dupe(),
                            write_ctx: spp_data.write_ctx.clone(),
                            mode: spp_data.set_mode.clone(),
                        }));
                        let propref = mk_named_prop(spp_data.reason.dupe(), false, name);
                        perform_lookup_action(
                            cx,
                            trace,
                            &propref,
                            &property::property_type(p),
                            PropertySource::PropertyMapProperty,
                            reason_c,
                            &spp_data.reason,
                            &action,
                        )?;
                    }
                }
            }
        }

        // *****************************
        // * ... and their fields read *
        // *****************************
        (TypeInner::DefT(r, def_t), UseTInner::GetPropT(box data))
            if matches!(def_t.deref(), DefTInner::InstanceT(_))
                && matches!(data.propref.as_ref(), PropRef::Named { name, .. } if name.as_str() == "constructor") =>
        {
            let t = type_util::class_type(l.dupe(), false, r.annot_loc().map(|a| a.dupe()));
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (&t, &Type::new(TypeInner::OpenT((*data.tout).dupe()))),
            )?;
        }
        (TypeInner::DefT(reason_instance, def_t), UseTInner::GetPropT(box data))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            let super_ = &inst_t.super_;
            let inst = &inst_t.inst;
            let method_accessible = data.from_annot;
            let lookup_action = LookupAction::ReadProp(Box::new(ReadPropData {
                use_op: data.use_op.dupe(),
                obj_t: l.dupe(),
                tout: (*data.tout).dupe(),
            }));
            let lookup_kind = instance_lookup_kind(
                cx,
                trace,
                reason_instance,
                &data.reason,
                method_accessible,
                l,
                &data.propref,
                lookup_action,
            )?;
            flow_js_utils::get_prop_t_kit::read_instance_prop::<FlowJs>(
                cx,
                &trace,
                &data.use_op,
                l,
                data.id,
                method_accessible,
                super_.dupe(),
                lookup_kind,
                &data.hint,
                data.skip_optional,
                inst,
                &data.propref,
                &data.reason,
            )?(cx, (*data.tout).dupe())?;
        }
        (TypeInner::DefT(reason_c, def_t), UseTInner::GetPrivatePropT(box gpp_data))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            let inst = &inst_t.inst;
            get_private_prop(
                cx,
                false,
                trace,
                l,
                reason_c,
                inst,
                &gpp_data.use_op,
                &gpp_data.reason,
                &gpp_data.name,
                &gpp_data.class_bindings,
                gpp_data.static_,
                &gpp_data.tout,
            )?;
        }
        // ********************************
        // * ... and their methods called *
        // ********************************
        (
            TypeInner::DefT(reason_instance, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: reason_call,
                prop_reason: reason_lookup,
                propref,
                method_action: action,
            }),
        ) if let DefTInner::InstanceT(inst_t) = def_t.deref() => {
            let super_ = &inst_t.super_;
            let inst = &inst_t.inst;
            let funt = flow_typing_tvar::mk_no_wrap_where(
                cx,
                reason_lookup.dupe(),
                |cx, reason_tout, tout| {
                    let tout_tvar = Tvar::new(reason_tout.dupe(), tout as u32);
                    let lookup_action = LookupAction::ReadProp(Box::new(ReadPropData {
                        use_op: use_op.dupe(),
                        obj_t: l.dupe(),
                        tout: tout_tvar.dupe(),
                    }));
                    let method_accessible = true;
                    let lookup_kind = instance_lookup_kind(
                        cx,
                        trace,
                        reason_instance,
                        reason_lookup,
                        method_accessible,
                        l,
                        propref,
                        lookup_action,
                    )?;
                    flow_js_utils::get_prop_t_kit::read_instance_prop::<FlowJs>(
                        cx,
                        &trace,
                        use_op,
                        l,
                        None,
                        true,
                        super_.dupe(),
                        lookup_kind,
                        &hint_unavailable(),
                        false,
                        inst,
                        propref,
                        reason_call,
                    )?(cx, tout_tvar.dupe())?;
                    Ok::<(), FlowJsException>(())
                },
            )?;
            // suppress ops while calling the function. if `funt` is a `FunT`, then
            // `CallT` will set its own ops during the call. if `funt` is something
            // else, then something like `VoidT ~> CallT` doesn't need the op either
            // because we want to point at the call and undefined thing.
            apply_method_action(
                cx,
                trace,
                &funt,
                use_op.dupe(),
                reason_call.dupe(),
                l.dupe(),
                action,
            )?;
        }
        (TypeInner::DefT(reason_c, def_t), UseTInner::PrivateMethodT(box pm_data))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            let inst = &inst_t.inst;
            let tvar = flow_typing_tvar::mk_no_wrap(cx, &pm_data.prop_reason);
            let funt = Type::new(TypeInner::OpenT(Tvar::new(
                pm_data.prop_reason.dupe(),
                tvar as u32,
            )));
            let l_for_private = if pm_data.static_ {
                type_util::class_type(l.dupe(), false, None)
            } else {
                l.dupe()
            };
            get_private_prop(
                cx,
                true,
                trace,
                &l_for_private,
                reason_c,
                inst,
                &pm_data.use_op,
                &pm_data.reason,
                &pm_data.name,
                &pm_data.class_bindings,
                pm_data.static_,
                &Tvar::new(pm_data.prop_reason.dupe(), tvar as u32),
            )?;
            apply_method_action(
                cx,
                trace,
                &funt,
                pm_data.use_op.dupe(),
                pm_data.reason.dupe(),
                l_for_private,
                &pm_data.method_action,
            )?;
        }

        // *****************************************************************
        // * Object.assign logic has been moved to type_operation_utils.ml *
        // *****************************************************************
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForObjectAssign,
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForDestructuring,
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }
        // *************************
        // * objects can be copied *
        // *************************
        (TypeInner::DefT(reason_obj, def_t), UseTInner::ObjRestT(reason_op, xs, t, id))
            if let DefTInner::ObjT(o) = def_t.deref() =>
        {
            let props_tmap = o.props_tmap.dupe();
            let obj_kind = &o.flags.obj_kind;
            let reachable_targs = &o.reachable_targs;
            const_fold_expansion::guard(cx, *id, (reason_obj.dupe(), 0), |count| {
                match count {
                    0 => {
                        let xs_smol: Vec<flow_data_structure_wrapper::smol_str::FlowSmolStr> =
                            xs.iter().map(|s| s.as_str().into()).collect();
                        let rest_obj = flow_js_utils::objt_to_obj_rest(
                            cx,
                            props_tmap.dupe(),
                            Some(reachable_targs.dupe()),
                            obj_kind.clone(),
                            reason_op,
                            reason_obj,
                            &xs_smol,
                        )?;
                        rec_flow_t(cx, trace, unknown_use(), (&rest_obj, t))?;
                    }
                    _ => {}
                }
                Ok::<(), FlowJsException>(())
            })?;
        }
        (TypeInner::DefT(reason, def_t), UseTInner::ObjRestT(reason_op, xs, t, _))
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            let super_ = &inst_t.super_;
            let inst = &inst_t.inst;
            // Spread fields from super into an object
            let obj_super = flow_typing_tvar::mk_where(cx, reason_op.dupe(), |cx, tvar| {
                let use_t = UseT::new(UseTInner::ObjRestT(
                    reason_op.dupe(),
                    xs.clone(),
                    tvar.dupe(),
                    flow_common::reason::mk_id() as i32,
                ));
                rec_flow(
                    cx,
                    trace,
                    (
                        super_,
                        &UseT::new(UseTInner::ReposLowerT {
                            reason: reason.dupe(),
                            use_desc: false,
                            use_t: Box::new(use_t),
                        }),
                    ),
                )?;
                Ok::<(), FlowJsException>(())
            })?;
            let xs_smol: Vec<flow_data_structure_wrapper::smol_str::FlowSmolStr> =
                xs.iter().map(|s| s.as_str().into()).collect();
            let o = flow_js_utils::objt_to_obj_rest(
                cx,
                inst.own_props.dupe(),
                Some(Rc::from([])),
                ObjKind::Exact,
                reason_op,
                reason,
                &xs_smol,
            );
            // Combine super and own props.
            let use_op = UseOp::Op(std::sync::Arc::new(VirtualRootUseOp::ObjectSpread {
                op: reason_op.dupe(),
            }));
            let spread_tool = object::ResolveTool::Resolve(object::Resolve::Next);
            let spread_target = object::spread::Target::Value {
                make_seal: obj_type::mk_seal(false, false),
            };
            let spread_state = object::spread::State {
                todo_rev: flow_data_structure_wrapper::list::FlowOcamlList::unit(
                    object::spread::Operand::Type(o?),
                ),
                acc: flow_data_structure_wrapper::list::FlowOcamlList::new(),
                spread_id: flow_common::reason::mk_id() as i32,
                union_reason: None,
                curr_resolve_idx: 0,
            };
            let o2 = flow_typing_tvar::mk_where(cx, reason_op.dupe(), |cx, tvar| {
                rec_flow(
                    cx,
                    trace,
                    (
                        &obj_super,
                        &UseT::new(UseTInner::ObjKitT(
                            use_op.dupe(),
                            reason_op.dupe(),
                            Box::new(spread_tool.clone()),
                            Box::new(object::Tool::Spread(Box::new((
                                spread_target,
                                spread_state.clone(),
                            )))),
                            tvar.dupe(),
                        )),
                    ),
                )
            })?;
            rec_flow_t(cx, trace, use_op, (&o2, t))?;
        }
        (TypeInner::AnyT(_, src), UseTInner::ObjRestT(reason, _, t, _)) => {
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (&any_t::why(*src, reason.dupe()), t),
            )?;
        }
        (TypeInner::ObjProtoT(_), UseTInner::ObjRestT(reason, _, t, _)) => {
            let obj = obj_type::mk_with_proto(
                cx,
                reason.dupe(),
                ObjKind::Exact,
                None,
                None,
                None,
                None,
                l.dupe(),
            );
            rec_flow_t(cx, trace, unknown_use(), (&obj, t))?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::ObjRestT(reason, _, t, _))
            if matches!(def_t.deref(), DefTInner::NullT | DefTInner::VoidT) =>
        {
            let o = obj_type::mk(ObjKind::Exact, cx, reason.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&o, t))?;
        }
        // *******************************************
        // * objects may have their fields looked up *
        // *******************************************
        (
            TypeInner::DefT(reason_obj, def_t),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind,
                try_ts_on_failure,
                propref,
                lookup_action: action,
                ids,
                method_accessible,
                ignore_dicts,
            }),
        ) if let DefTInner::ObjT(o) = def_t.deref() => {
            match flow_js_utils::get_prop_t_kit::get_obj_prop::<FlowJs>(
                cx,
                &trace,
                &unknown_use(),
                false,
                true,
                o,
                propref,
                reason_op,
            )? {
                Some((p, target_kind)) => {
                    if let LookupKind::NonstrictReturning(box NonstrictReturningData(
                        _,
                        Some((id, _)),
                    )) = &**lookup_kind
                    {
                        cx.test_prop_hit(*id);
                    }
                    perform_lookup_action(
                        cx,
                        trace,
                        propref,
                        &p,
                        target_kind.clone(),
                        reason_obj,
                        reason_op,
                        action,
                    )?;
                }
                None => {
                    let new_ids = ids.as_ref().map(|s| {
                        let mut new_set = s.dupe();
                        new_set.insert(o.props_tmap.dupe());
                        new_set
                    });
                    rec_flow(
                        cx,
                        trace,
                        (
                            &o.proto_t,
                            &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                                reason: reason_op.dupe(),
                                lookup_kind: lookup_kind.clone(),
                                try_ts_on_failure: try_ts_on_failure.clone(),
                                propref: propref.clone(),
                                lookup_action: action.clone(),
                                method_accessible: *method_accessible,
                                ids: new_ids,
                                ignore_dicts: *ignore_dicts,
                            }))),
                        ),
                    )?;
                }
            }
        }
        (
            TypeInner::AnyT(reason, src),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind,
                propref,
                lookup_action: action,
                ..
            }),
        ) => {
            match action {
                box LookupAction::SuperProp(box (_, lp))
                    if property::write_t_of_property_type(lp, None).is_none() =>
                {
                    // Without this exception, we will call rec_flow_p where
                    // `write_t lp = None` and `write_t up = Some`, which is a polarity
                    // mismatch error. Instead of this, we could "read" `mixed` from
                    // covariant props, which would always flow into `any`.
                }
                _ => {
                    let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
                    let p = PropertyType::OrdinaryField {
                        type_: any_t::why(src, reason_op.dupe()),
                        polarity: Polarity::Neutral,
                    };
                    if let LookupKind::NonstrictReturning(box NonstrictReturningData(
                        _,
                        Some((id, _)),
                    )) = &**lookup_kind
                    {
                        cx.test_prop_hit(*id);
                    }
                    perform_lookup_action(
                        cx,
                        trace,
                        propref,
                        &p,
                        PropertySource::DynamicProperty,
                        reason,
                        reason_op,
                        action,
                    )?;
                }
            }
        }

        // ********************************
        // * ... and their fields written *
        // ********************************

        // o.x = ... has the additional effect of o[_] = ...
        (TypeInner::DefT(_, def_t), UseTInner::SetPropT(use_op, _, propref, _, _, _, _))
            if let DefTInner::ObjT(o) = def_t.deref()
                && flow_js_utils::obj_is_readonlyish(o)
                && !flow_js_utils::is_exception_to_react_dro(propref) =>
        {
            let reason_prop = type_util::reason_of_propref(propref);
            let prop_name = name_of_propref(propref);
            let use_op = match &o.flags.react_dro {
                Some(ReactDro(loc, dro_type)) => UseOp::Frame(
                    std::sync::Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                        loc.dupe(),
                        dro_type.clone(),
                    )))),
                    Arc::new(use_op.dupe()),
                ),
                None => use_op.dupe(),
            };
            add_output(
                cx,
                ErrorMessage::EPropNotWritable(Box::new(EPropNotWritableData {
                    reason_prop: reason_prop.dupe(),
                    prop_name,
                    use_op,
                })),
            )?;
        }
        (
            TypeInner::DefT(reason_obj, def_t),
            UseTInner::SetPropT(use_op, reason_op, propref, mode, _, tin, prop_t),
        ) if let DefTInner::ObjT(o) = def_t.deref() => {
            write_obj_prop(
                cx, trace, use_op, mode, o, propref, reason_obj, reason_op, tin, prop_t,
            )?;
        }

        // Since we don't know the type of the prop, use AnyT.
        (TypeInner::AnyT(_, src), UseTInner::SetPropT(use_op, reason_op, _, _, _, t, prop_t)) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            if let Some(prop_t) = prop_t {
                rec_flow_t(
                    cx,
                    trace,
                    unknown_use(),
                    (&any_t::why(src, reason_op.dupe()), prop_t),
                )?;
            }
            rec_flow(
                cx,
                trace,
                (
                    t,
                    &UseT::new(UseTInner::UseT(
                        use_op.dupe(),
                        any_t::why(src, reason_op.dupe()),
                    )),
                ),
            )?;
        }

        // *****************************
        // * ... and their fields read *
        // *****************************
        (TypeInner::DefT(_, def_t), UseTInner::GetPropT(box data))
            if matches!(def_t.deref(), DefTInner::ObjT(_))
                && matches!(data.propref.as_ref(), PropRef::Named { name, .. } if name.as_str() == "constructor") =>
        {
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (
                    &unsoundness::why(UnsoundnessKind::Constructor, data.reason.dupe()),
                    &Type::new(TypeInner::OpenT((*data.tout).dupe())),
                ),
            )?;
        }
        (TypeInner::DefT(reason_obj, def_t), UseTInner::GetPropT(box data))
            if let DefTInner::ObjT(o) = def_t.deref() =>
        {
            let lookup_info = data
                .id
                .map(|id| -> Result<_, FlowJsException> {
                    let lookup_default_tout =
                        flow_typing_tvar::mk_where(cx, data.reason.dupe(), |cx, tvar| {
                            rec_flow_t(
                                cx,
                                trace,
                                data.use_op.dupe(),
                                (tvar, &Type::new(TypeInner::OpenT((*data.tout).dupe()))),
                            )?;
                            Ok::<(), FlowJsException>(())
                        })?;
                    Ok((id, lookup_default_tout))
                })
                .transpose()?;
            flow_js_utils::get_prop_t_kit::read_obj_prop::<FlowJs>(
                cx,
                &trace,
                data.use_op.dupe(),
                data.from_annot,
                data.skip_optional,
                o,
                &data.propref,
                reason_obj.dupe(),
                data.reason.dupe(),
                lookup_info,
            )?(cx, (*data.tout).dupe())?;
        }
        (TypeInner::AnyT(_, src), UseTInner::GetPropT(box data)) => {
            if let Some(id) = data.id {
                cx.test_prop_hit(id);
            }
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (
                    &any_t::why(src, data.reason.dupe()),
                    &Type::new(TypeInner::OpenT((*data.tout).dupe())),
                ),
            )?;
        }
        (TypeInner::AnyT(_, src), UseTInner::GetPrivatePropT(box gpp_data)) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (
                    &any_t::why(src, gpp_data.reason.dupe()),
                    &Type::new(TypeInner::OpenT((*gpp_data.tout).dupe())),
                ),
            )?;
        }
        // ********************************
        // * ... and their methods called *
        // ********************************
        (
            TypeInner::DefT(_, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op: _,
                reason: reason_call,
                prop_reason: _,
                propref,
                method_action: action,
            }),
        ) if matches!(def_t.deref(), DefTInner::ObjT(_))
            && matches!(&**propref, PropRef::Named { name, .. } if name.as_str() == "constructor") =>
        {
            add_specialized_callee_method_action(
                cx,
                trace,
                &any_t::untyped(reason_call.dupe()),
                action,
            )?;
        }
        (
            TypeInner::DefT(reason_obj, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: reason_call,
                prop_reason: reason_lookup,
                propref,
                method_action: action,
            }),
        ) if let DefTInner::ObjT(o) = def_t.deref() => {
            let t = flow_typing_tvar::mk_no_wrap_where(
                cx,
                reason_lookup.dupe(),
                |cx, reason_tout, tout| {
                    flow_js_utils::get_prop_t_kit::read_obj_prop::<FlowJs>(
                        cx,
                        &trace,
                        use_op.dupe(),
                        false,
                        false,
                        o,
                        propref,
                        reason_obj.dupe(),
                        reason_lookup.dupe(),
                        None,
                    )?(cx, Tvar::new(reason_tout.dupe(), tout as u32))?;
                    Ok::<(), FlowJsException>(())
                },
            )?;
            apply_method_action(
                cx,
                trace,
                &t,
                use_op.dupe(),
                reason_call.dupe(),
                l.dupe(),
                action,
            )?;
        }
        // ******************************************
        // * strings may have their characters read *
        // ******************************************
        (
            TypeInner::DefT(reason_s, def_t),
            UseTInner::GetElemT(box GetElemTData {
                use_op,
                reason: reason_op,
                key_t,
                tout,
                ..
            }),
        ) if matches!(
            def_t.deref(),
            DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
        ) =>
        {
            rec_flow(
                cx,
                trace,
                (
                    key_t,
                    &UseT::new(UseTInner::UseT(
                        use_op.dupe(),
                        num_module_t::why(reason_s.dupe()),
                    )),
                ),
            )?;
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (
                    &str_module_t::why(reason_op.dupe()),
                    &Type::new(TypeInner::OpenT((**tout).dupe())),
                ),
            )?;
        }
        // Expressions may be used as keys to access objects and arrays. In
        // general, we cannot evaluate such expressions at compile time. However,
        // in some idiomatic special cases, we can; in such cases, we know exactly
        // which strings/numbers the keys may be, and thus, we can use precise
        // properties and indices to resolve the accesses.

        // **********************************************************************
        // * objects/arrays may have their properties/elements written and read *
        // **********************************************************************
        (
            TypeInner::DefT(_, def_t),
            UseTInner::SetElemT(box SetElemTData {
                use_op,
                reason,
                key_t: key,
                set_mode: mode,
                tin,
                tout,
            }),
        ) if matches!(
            def_t.deref(),
            DefTInner::ObjT(_) | DefTInner::ArrT(_) | DefTInner::InstanceT(_)
        ) =>
        {
            let action = ElemAction::WriteElem(Box::new(WriteElemData {
                tin: tin.dupe(),
                tout: tout.as_ref().map(|t| t.dupe()),
                mode: mode.clone(),
            }));
            rec_flow(
                cx,
                trace,
                (
                    key,
                    &UseT::new(UseTInner::ElemT(Box::new(ElemTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        obj: l.dupe(),
                        action: Box::new(action),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::AnyT(_, _),
            UseTInner::SetElemT(box SetElemTData {
                use_op,
                reason,
                key_t: key,
                set_mode: mode,
                tin,
                tout,
            }),
        ) => {
            let action = ElemAction::WriteElem(Box::new(WriteElemData {
                tin: tin.dupe(),
                tout: tout.as_ref().map(|t| t.dupe()),
                mode: mode.clone(),
            }));
            rec_flow(
                cx,
                trace,
                (
                    key,
                    &UseT::new(UseTInner::ElemT(Box::new(ElemTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        obj: l.dupe(),
                        action: Box::new(action),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::GetElemT(box GetElemTData {
                use_op,
                reason,
                id,
                from_annot,
                skip_optional,
                access_iterables,
                key_t,
                tout,
            }),
        ) if matches!(
            def_t.deref(),
            DefTInner::ObjT(_) | DefTInner::ArrT(_) | DefTInner::InstanceT(_)
        ) =>
        {
            let action = ElemAction::ReadElem(Box::new(ReadElemData {
                id: *id,
                from_annot: *from_annot,
                skip_optional: *skip_optional,
                access_iterables: *access_iterables,
                tout: (**tout).dupe(),
            }));
            rec_flow(
                cx,
                trace,
                (
                    key_t,
                    &UseT::new(UseTInner::ElemT(Box::new(ElemTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        obj: l.dupe(),
                        action: Box::new(action),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::AnyT(_, _),
            UseTInner::GetElemT(box GetElemTData {
                use_op,
                reason,
                id,
                from_annot,
                skip_optional,
                access_iterables,
                key_t,
                tout,
            }),
        ) => {
            let action = ElemAction::ReadElem(Box::new(ReadElemData {
                id: *id,
                from_annot: *from_annot,
                skip_optional: *skip_optional,
                access_iterables: *access_iterables,
                tout: (**tout).dupe(),
            }));
            rec_flow(
                cx,
                trace,
                (
                    key_t,
                    &UseT::new(UseTInner::ElemT(Box::new(ElemTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        obj: l.dupe(),
                        action: Box::new(action),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::CallElemT(box CallElemTData {
                use_op,
                reason: reason_call,
                prop_reason: reason_lookup,
                key_t: key,
                method_action: action,
            }),
        ) if matches!(
            def_t.deref(),
            DefTInner::ObjT(_) | DefTInner::ArrT(_) | DefTInner::InstanceT(_)
        ) =>
        {
            let action = ElemAction::CallElem(reason_call.dupe(), action.clone());
            rec_flow(
                cx,
                trace,
                (
                    key,
                    &UseT::new(UseTInner::ElemT(Box::new(ElemTData {
                        use_op: use_op.dupe(),
                        reason: reason_lookup.dupe(),
                        obj: l.dupe(),
                        action: Box::new(action),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::AnyT(_, _),
            UseTInner::CallElemT(box CallElemTData {
                use_op,
                reason: reason_call,
                prop_reason: reason_lookup,
                key_t: key,
                method_action: action,
            }),
        ) => {
            let action = ElemAction::CallElem(reason_call.dupe(), action.clone());
            rec_flow(
                cx,
                trace,
                (
                    key,
                    &UseT::new(UseTInner::ElemT(Box::new(ElemTData {
                        use_op: use_op.dupe(),
                        reason: reason_lookup.dupe(),
                        obj: l.dupe(),
                        action: Box::new(action),
                    }))),
                ),
            )?;
        }
        // If we are accessing `Iterable<T>` with a number, and have `access_iterables = true`,
        // then output `T`.
        (
            TypeInner::DefT(_, def_t_l),
            UseTInner::ElemT(box ElemTData {
                use_op,
                obj,
                action:
                    box ElemAction::ReadElem(box ReadElemData {
                        access_iterables: true,
                        tout,
                        ..
                    }),
                ..
            }),
        ) if matches!(
            def_t_l.deref(),
            DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
        ) && let TypeInner::DefT(_, obj_def) = obj.deref()
            && let DefTInner::InstanceT(inst) = obj_def.deref()
            && !inst.inst.type_args.is_empty()
            && flow_js_utils::is_builtin_iterable_class_id(inst.inst.class_id.clone(), cx) =>
        {
            let (_, _, t, _) = &inst.inst.type_args[0];
            rec_flow_t(
                cx,
                trace,
                use_op.dupe(),
                (t, &Type::new(TypeInner::OpenT(tout.dupe()))),
            )?;
        }
        (
            _,
            UseTInner::ElemT(box ElemTData {
                use_op,
                reason,
                obj,
                action,
            }),
        ) if matches!(obj.deref(), TypeInner::DefT(_, def_t) if matches!(def_t.deref(), DefTInner::ObjT(_) | DefTInner::InstanceT(_))) =>
        {
            elem_action_on_obj(cx, trace, use_op, l, obj, reason, action)?;
        }
        (
            _,
            UseTInner::ElemT(box ElemTData {
                use_op,
                reason,
                obj,
                action,
            }),
        ) if let TypeInner::AnyT(_, src) = obj.deref() => {
            let value = any_t::why(*src, reason.dupe());
            perform_elem_action(cx, trace, use_op.dupe(), false, reason, obj, &value, action)?;
        }
        // It is not safe to write to an unknown index in a tuple. However, any is
        // a source of unsoundness, so that's ok. `tup[(0: any)] = 123` should not
        // error when `tup[0] = 123` does not.
        (
            TypeInner::AnyT(_, _),
            UseTInner::ElemT(box ElemTData {
                use_op,
                reason: reason_op,
                obj,
                action,
            }),
        ) if let TypeInner::DefT(reason_tup, obj_def_t) = obj.deref()
            && let DefTInner::ArrT(arrtype) = obj_def_t.deref() =>
        {
            let react_dro = match (action, arrtype.as_ref()) {
                (
                    box ElemAction::WriteElem(..),
                    ArrType::ROArrayAT(box (_, _))
                    | ArrType::TupleAT(box TupleATData {
                        react_dro: Some(_), ..
                    })
                    | ArrType::ArrayAT(box ArrayATData {
                        react_dro: Some(_), ..
                    }),
                ) => {
                    let reasons = (reason_op.dupe(), reason_tup.dupe());
                    let use_op_for_err = match arrtype.as_ref() {
                        ArrType::TupleAT(box TupleATData {
                            react_dro: Some(dro),
                            ..
                        })
                        | ArrType::ArrayAT(box ArrayATData {
                            react_dro: Some(dro),
                            ..
                        }) => UseOp::Frame(
                            std::sync::Arc::new(VirtualFrameUseOp::ReactDeepReadOnly(Box::new((
                                dro.0.dupe(),
                                dro.1.clone(),
                            )))),
                            Arc::new(use_op.dupe()),
                        ),
                        _ => use_op.dupe(),
                    };
                    add_output(cx, ErrorMessage::EROArrayWrite(reasons, use_op_for_err))?;
                    None
                }
                (
                    box ElemAction::ReadElem(..),
                    ArrType::ROArrayAT(box (_, react_dro))
                    | ArrType::TupleAT(box TupleATData { react_dro, .. })
                    | ArrType::ArrayAT(box ArrayATData { react_dro, .. }),
                ) => react_dro.clone(),
                _ => None,
            };
            let value = elemt_of_arrtype(arrtype);
            let value = match react_dro {
                Some(dro) => mk_react_dro(cx, use_op.dupe(), dro, value),
                None => value,
            };
            perform_elem_action(
                cx,
                trace,
                use_op.dupe(),
                false,
                reason_op,
                obj,
                &value,
                action,
            )?;
        }
        (
            TypeInner::DefT(_, def_t_l),
            UseTInner::ElemT(box ElemTData {
                use_op,
                reason,
                obj,
                action,
            }),
        ) if matches!(
            def_t_l.deref(),
            DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
        ) && let TypeInner::DefT(reason_tup, obj_def_t) = obj.deref()
            && let DefTInner::ArrT(arrtype) = obj_def_t.deref() =>
        {
            let (write_action, read_action, never_union_void_on_computed_prop_access) = match action
            {
                box ElemAction::ReadElem(box ReadElemData { from_annot, .. }) => {
                    (false, true, *from_annot)
                }
                box ElemAction::CallElem(_, _) => (false, false, false),
                box ElemAction::WriteElem(..) => (true, false, true),
            };
            let (value, is_tuple, use_op_out, react_dro) = flow_js_utils::array_elem_check(
                cx,
                write_action,
                never_union_void_on_computed_prop_access,
                l,
                use_op.dupe(),
                reason,
                reason_tup,
                arrtype,
            )?;
            let value = match react_dro {
                Some(dro) if read_action => mk_react_dro(cx, use_op_out.dupe(), dro, value),
                _ => value,
            };
            perform_elem_action(cx, trace, use_op_out, is_tuple, reason, obj, &value, action)?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::GetPropT(box data))
            if matches!(def_t.deref(), DefTInner::ArrT(_))
                && matches!(data.propref.as_ref(), PropRef::Named { name, .. } if name.as_str() == "constructor") =>
        {
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (
                    &unsoundness::why(UnsoundnessKind::Constructor, data.reason.dupe()),
                    &Type::new(TypeInner::OpenT((*data.tout).dupe())),
                ),
            )?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::SetPropT(_, _, propref, _, _, _, _))
            if matches!(def_t.deref(), DefTInner::ArrT(_))
                && matches!(&**propref, PropRef::Named { name, .. } if name.as_str() == "constructor") =>
            {}
        (
            TypeInner::DefT(_, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op: _,
                reason: reason_call,
                prop_reason: _,
                propref,
                method_action: action,
            }),
        ) if matches!(def_t.deref(), DefTInner::ArrT(_))
            && matches!(&**propref, PropRef::Named { name, .. } if name.as_str() == "constructor") =>
        {
            add_specialized_callee_method_action(
                cx,
                trace,
                &any_t::untyped(reason_call.dupe()),
                action,
            )?;
        }
        // **************************************************
        // * array pattern can consume the rest of an array *
        // **************************************************
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ArrRestT(box ArrRestTData {
                use_op: _,
                reason,
                index: i,
                tout,
            }),
        ) if let DefTInner::ArrT(arrtype) = def_t.deref() => {
            let i = *i as usize;
            let arrtype = match arrtype.deref() {
                ArrType::ArrayAT(box ArrayATData {
                    tuple_view: None, ..
                })
                | ArrType::ROArrayAT(box (_, _)) => arrtype.deref().clone(),
                ArrType::ArrayAT(box ArrayATData {
                    elem_t,
                    tuple_view: Some(tuple_view),
                    react_dro,
                }) => {
                    let elements = tuple_view.elements[i.min(tuple_view.elements.len())..].to_vec();
                    let (num_req, num_total) = tuple_view.arity;
                    let arity = (
                        (num_req as i64 - i as i64).max(0) as i32,
                        (num_total as i64 - i as i64).max(0) as i32,
                    );
                    ArrType::ArrayAT(Box::new(ArrayATData {
                        elem_t: elem_t.dupe(),
                        tuple_view: Some(TupleView {
                            elements: elements.into(),
                            arity,
                            inexact: tuple_view.inexact,
                        }),
                        react_dro: react_dro.clone(),
                    }))
                }
                ArrType::TupleAT(box TupleATData {
                    elem_t,
                    elements,
                    arity: (num_req, num_total),
                    inexact,
                    react_dro,
                }) => ArrType::TupleAT(Box::new(TupleATData {
                    elem_t: elem_t.dupe(),
                    elements: elements[i.min(elements.len())..].to_vec().into(),
                    arity: (
                        (*num_req as i64 - i as i64).max(0) as i32,
                        (*num_total as i64 - i as i64).max(0) as i32,
                    ),
                    inexact: *inexact,
                    react_dro: react_dro.clone(),
                })),
            };
            let a = Type::new(TypeInner::DefT(
                reason.dupe(),
                DefT::new(DefTInner::ArrT(Rc::new(arrtype))),
            ));
            rec_flow_t(cx, trace, unknown_use(), (&a, tout))?;
        }
        (
            TypeInner::AnyT(_, src),
            UseTInner::ArrRestT(box ArrRestTData {
                use_op: _,
                reason,
                index: _,
                tout,
            }),
        ) => {
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (&any_t::why(*src, reason.dupe()), tout),
            )?;
        }
        // **************************************************
        // * function types can be mapped over a structure  *
        // **************************************************
        (
            TypeInner::AnyT(_, src),
            UseTInner::MapTypeT(box MapTypeTData {
                use_op: _,
                reason: reason_op,
                type_map: _,
                tout,
            }),
        ) => {
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (&any_t::why(src, reason_op.dupe()), tout),
            )?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::MapTypeT(box MapTypeTData {
                use_op: _,
                reason: reason_op,
                type_map: TypeMap::ObjectKeyMirror,
                tout,
            }),
        ) if let DefTInner::ObjT(o) = def_t.deref() => {
            let mirrored = flow_js_utils::obj_key_mirror(cx, o, reason_op);
            rec_flow_t(cx, trace, unknown_use(), (&mirrored, tout))?;
        }

        // **************
        // * object kit *
        // **************
        (
            TypeInner::GenericT(box GenericTData {
                reason,
                no_infer,
                bound,
                id,
                name,
            }),
            UseTInner::ObjKitT(use_op, reason_op, resolve_tool, tool, tout),
        ) if let object::ResolveTool::Resolve(object::Resolve::Next) = &**resolve_tool
            && let object::Tool::ObjectMap(box ObjectToolObjectMapData {
                prop_type,
                mapped_type_flags,
                selected_keys_opt: None,
                ..
            }) = &**tool
            && {
                let roarray_bound = Type::new(TypeInner::DefT(
                    reason.dupe(),
                    DefT::new(DefTInner::ArrT(Rc::new(ArrType::ROArrayAT(Box::new((
                        Type::new(TypeInner::DefT(
                            reason.dupe(),
                            DefT::new(DefTInner::MixedT(MixedFlavor::MixedEverything)),
                        )),
                        None,
                    )))))),
                ));
                speculative_subtyping_succeeds(cx, bound, &roarray_bound)?
            } =>
        {
            let (t_generic_id, t) = {
                fn loop_generic(
                    t: &Type,
                    ls: flow_typing_generics::SpreadId,
                ) -> (flow_typing_generics::SpreadId, Type) {
                    match t.deref() {
                        TypeInner::GenericT(box GenericTData {
                            id, bound, reason, ..
                        }) => {
                            let bound_with_reason =
                                type_util::mod_reason_of_t(&|_| reason.dupe(), bound);
                            let new_ls = flow_typing_generics::spread_append(
                                &flow_typing_generics::GenericId::make_spread(id),
                                &ls,
                            );
                            loop_generic(&bound_with_reason, new_ls)
                        }
                        _ => (ls, t.dupe()),
                    }
                }
                loop_generic(l, flow_typing_generics::spread_empty())
            };
            let mapped_bound =
                flow_typing_tvar::mk_where(cx, reason_op.dupe(), |cx, tout_inner| {
                    rec_flow(
                        cx,
                        trace,
                        (
                            &t,
                            &UseT::new(UseTInner::ObjKitT(
                                use_op.dupe(),
                                reason_op.dupe(),
                                Box::new(object::ResolveTool::Resolve(object::Resolve::Next)),
                                Box::new(object::Tool::ObjectMap(Box::new(
                                    ObjectToolObjectMapData {
                                        prop_type: prop_type.dupe(),
                                        mapped_type_flags: mapped_type_flags.clone(),
                                        selected_keys_opt: None,
                                    },
                                ))),
                                tout_inner.dupe(),
                            )),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                })?;
            let mapped_generic_t = match flow_typing_generics::GenericId::make_op_id(
                flow_common::subst_name::OpKind::MappedArray,
                t_generic_id,
            ) {
                Some(new_id) => Type::new(TypeInner::GenericT(Box::new(GenericTData {
                    bound: mapped_bound,
                    reason: reason.dupe(),
                    id: new_id.clone(),
                    name: new_id.subst_name(),
                    no_infer: *no_infer,
                }))),
                None => t,
            };
            rec_flow_t(cx, trace, unknown_use(), (&mapped_generic_t, tout))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ObjKitT(use_op, reason_op, resolve_tool, tool, tout),
        ) if let DefTInner::ArrT(arrtype) = def_t.deref()
            && let object::ResolveTool::Resolve(object::Resolve::Next) = &**resolve_tool
            && let object::Tool::ObjectMap(box ObjectToolObjectMapData {
                prop_type: property_type,
                mapped_type_flags,
                selected_keys_opt: None,
                ..
            }) = &**tool
            && let TypeInner::OpenT(tout_tvar) = tout.deref() =>
        {
            let mapped_type_variance = &mapped_type_flags.variance;
            let mapped_type_optionality = &mapped_type_flags.optional;
            let f = |value_t: &Type, index: Option<usize>, optional: bool| -> Type {
                let r = reason_of_t(value_t).dupe();
                let key_t = match index {
                    None => num_module_t::why(r),
                    Some(i) => Type::new(TypeInner::DefT(
                        r,
                        DefT::new(DefTInner::SingletonNumT {
                            from_annot: true,
                            value: NumberLiteral(i as f64, i.to_string().into()),
                        }),
                    )),
                };
                slice_utils::mk_mapped_prop_type(
                    use_op,
                    mapped_type_optionality,
                    property_type,
                    key_t,
                    optional,
                )
            };
            if !matches!(
                mapped_type_variance,
                flow_typing_type::type_::MappedTypeVariance::KeepVariance
            ) {
                add_output(
                    cx,
                ErrorMessage::EInvalidMappedType {
                        loc: reason_op.loc().dupe(),
                        kind: flow_typing_errors::error_message::InvalidMappedTypeErrorKind::VarianceOnArrayInput,
                    },
                )?;
            }
            let new_arrtype = match arrtype.deref() {
                ArrType::ArrayAT(box ArrayATData {
                    elem_t,
                    tuple_view,
                    react_dro,
                }) => ArrType::ArrayAT(Box::new(ArrayATData {
                    elem_t: f(elem_t, None, false),
                    react_dro: react_dro.clone(),
                    tuple_view: tuple_view.as_ref().map(|tv| {
                        let elements = tv
                            .elements
                            .iter()
                            .enumerate()
                            .map(|(i, elem)| TupleElement {
                                name: elem.name.dupe(),
                                t: f(&elem.t, Some(i), elem.optional),
                                polarity: elem.polarity,
                                optional: elem.optional,
                                reason: elem.reason.dupe(),
                            })
                            .collect();
                        TupleView {
                            elements,
                            arity: tv.arity,
                            inexact: tv.inexact,
                        }
                    }),
                })),
                ArrType::TupleAT(box TupleATData {
                    elem_t,
                    elements,
                    arity,
                    inexact,
                    react_dro,
                }) => ArrType::TupleAT(Box::new(TupleATData {
                    elem_t: f(elem_t, None, false),
                    react_dro: react_dro.clone(),
                    elements: elements
                        .iter()
                        .enumerate()
                        .map(|(i, elem)| TupleElement {
                            name: elem.name.dupe(),
                            t: f(&elem.t, Some(i), elem.optional),
                            polarity: elem.polarity,
                            optional: elem.optional,
                            reason: elem.reason.dupe(),
                        })
                        .collect(),
                    arity: *arity,
                    inexact: *inexact,
                })),
                ArrType::ROArrayAT(box (elemt, dro)) => {
                    ArrType::ROArrayAT(Box::new((f(elemt, None, false), dro.clone())))
                }
            };
            let t = {
                let arr_reason = reason_op
                    .dupe()
                    .replace_desc(flow_common::reason::VirtualReasonDesc::RArrayType);
                Type::new(TypeInner::DefT(
                    arr_reason,
                    DefT::new(DefTInner::ArrT(Rc::new(new_arrtype))),
                ))
            };
            rec_flow_t(
                cx,
                trace,
                unknown_use(),
                (&t, &Type::new(TypeInner::OpenT(tout_tvar.dupe()))),
            )?;
        }
        (_, UseTInner::ObjKitT(use_op, reason, resolve_tool, tool, tout)) => {
            object_kit::run(
                trace,
                cx,
                use_op.dupe(),
                reason,
                resolve_tool,
                tool,
                l,
                tout,
            )?;
        }
        // ************************************************************************
        // * functions may be bound by passing a receiver and (partial) arguments *
        // ************************************************************************
        (
            TypeInner::FunProtoBindT(_),
            UseTInner::CallT(box CallTData {
                use_op,
                call_action: box CallAction::ConcretizeCallee(tout),
                ..
            }),
        ) => {
            let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (l, &open_tout))?;
        }
        (
            TypeInner::FunProtoBindT(lreason),
            UseTInner::CallT(box CallTData {
                use_op,
                reason: reason_op,
                call_action: box CallAction::Funcalltype(box funtype),
                return_hint: _,
            }),
        ) if !funtype.call_args_tlist.is_empty() => {
            let func = &funtype.call_this_t;
            let call_targs = &funtype.call_targs;
            let first_arg = &funtype.call_args_tlist[0];
            let call_args_tlist = funtype.call_args_tlist[1..].to_vec();
            if call_targs.is_some() {
                flow_js_utils::add_output(
                    cx,
                    ErrorMessage::ECallTypeArity(Box::new(ECallTypeArityData {
                        call_loc: reason_op.loc().dupe(),
                        is_new: false,
                        reason_arity: lreason.dupe(),
                        expected_arity: 0,
                    })),
                )?;
            }
            let call_this_t = extract_non_spread(cx, first_arg)?;
            let new_funtype = FuncallType {
                call_this_t,
                call_targs: None,
                call_args_tlist: call_args_tlist.into(),
                call_tout: funtype.call_tout.dupe(),
                call_strict_arity: funtype.call_strict_arity,
                call_speculation_hint_state: funtype.call_speculation_hint_state.dupe(),
                call_specialized_callee: funtype.call_specialized_callee.clone(),
            };
            rec_flow(
                cx,
                trace,
                (
                    func,
                    &UseT::new(UseTInner::BindT(Box::new(BindTData {
                        use_op: use_op.dupe(),
                        reason: reason_op.dupe(),
                        funcall_type: Box::new(new_funtype),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::DefT(reason, def_t),
            UseTInner::BindT(box BindTData {
                use_op,
                reason: reason_op,
                funcall_type: calltype,
            }),
        ) if let DefTInner::FunT(_, ft_rc) = def_t.deref() => {
            let ft = ft_rc.deref();
            let (o1, _) = &ft.this_t;
            let o2 = &calltype.call_this_t;
            let tins2 = calltype.call_args_tlist.dupe();
            let call_tout = &calltype.call_tout;
            let call_specialized_callee = &calltype.call_specialized_callee;
            callee_recorder::add_callee(
                cx,
                callee_recorder::Kind::All,
                l.dupe(),
                call_specialized_callee.as_ref(),
            );
            // TODO: closure
            rec_flow_t(cx, trace, use_op.dupe(), (o2, o1))?;
            let resolve_to = SpreadResolve::ResolveSpreadsToMultiflowPartial(Box::new(
                ResolveSpreadsToMultiflowPartialData(
                    flow_common::reason::mk_id() as i32,
                    ft.clone(),
                    reason_op.dupe(),
                    Type::new(TypeInner::OpenT(call_tout.dupe())),
                ),
            ));
            resolve_call_list(
                cx,
                Some(trace),
                use_op.dupe(),
                reason,
                tins2.to_vec(),
                resolve_to,
            )?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::BindT(box BindTData {
                use_op: _,
                reason: _,
                funcall_type: _,
            }),
        ) if let DefTInner::ObjT(obj) = def_t.deref()
            && obj.call_t.is_some() =>
        {
            let id = obj.call_t.as_ref().unwrap();
            let call_t = cx.find_call(*id);
            rec_flow(cx, trace, (&call_t, u))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::BindT(box BindTData {
                use_op: _,
                reason: _,
                funcall_type: _,
            }),
        ) if let DefTInner::InstanceT(inst_t) = def_t.deref()
            && inst_t.inst.inst_call_t.is_some() =>
        {
            let id = inst_t.inst.inst_call_t.as_ref().unwrap();
            let call_t = cx.find_call(*id);
            rec_flow(cx, trace, (&call_t, u))?;
        }
        (
            TypeInner::AnyT(_, src),
            UseTInner::BindT(box BindTData {
                use_op,
                reason,
                funcall_type: calltype,
            }),
        ) => {
            let call_this_t = &calltype.call_this_t;
            let call_args_tlist = &calltype.call_args_tlist;
            let call_tout = &calltype.call_tout;
            let call_specialized_callee = &calltype.call_specialized_callee;
            callee_recorder::add_callee(
                cx,
                callee_recorder::Kind::All,
                l.dupe(),
                call_specialized_callee.as_ref(),
            );
            let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
            let any = any_t::why(src, reason.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, call_this_t))?;
            call_args_iter(
                |param_t| {
                    let any = any_t::why(src, reason.dupe());
                    rec_flow(
                        cx,
                        trace,
                        (
                            &any,
                            &UseT::new(UseTInner::UseT(use_op.dupe(), param_t.dupe())),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
                call_args_tlist,
            )?;
            let open_tout = Type::new(TypeInner::OpenT(call_tout.dupe()));
            rec_flow_t(cx, trace, unknown_use(), (l, &open_tout))?;
        }

        // ***************************************************************
        // * Enable structural subtyping for upperbounds like interfaces *
        // ***************************************************************
        (TypeInner::ObjProtoT(_), UseTInner::ImplementsT(_, _))
        | (TypeInner::FunProtoT(_), UseTInner::ImplementsT(_, _)) => {}
        (TypeInner::DefT(_, def_t), UseTInner::ImplementsT(_, _))
            if matches!(def_t.deref(), DefTInner::NullT) => {}
        (TypeInner::DefT(reason_inst, def_t), UseTInner::ImplementsT(use_op, t))
            if let DefTInner::InstanceT(inst_t) = def_t.deref()
                && matches!(&inst_t.inst.inst_kind, InstanceKind::InterfaceKind { .. }) =>
        {
            let inst = &inst_t.inst;
            let super_ = &inst_t.super_;
            structural_subtype(
                cx,
                trace,
                use_op.dupe(),
                t,
                reason_inst,
                (
                    inst.own_props.dupe(),
                    inst.proto_props.dupe(),
                    inst.inst_call_t,
                    &inst.inst_dict,
                ),
            )?;
            rec_flow(
                cx,
                trace,
                (
                    super_,
                    &UseT::new(UseTInner::ReposLowerT {
                        reason: reason_inst.dupe(),
                        use_desc: false,
                        use_t: Box::new(UseT::new(UseTInner::ImplementsT(use_op.dupe(), t.dupe()))),
                    }),
                ),
            )?;
        }
        (_, UseTInner::ImplementsT(_, _)) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EUnsupportedImplements(reason_of_t(l).dupe()),
            )?;
        }

        // *********************************************************************
        // * class A is a base class of class B iff                            *
        // * properties in B that override properties in A or its base classes *
        // * have the same signatures                                          *
        // *********************************************************************

        // The purpose of SuperT is to establish consistency between overriding
        // properties with overridden properties. As such, the lookups performed
        // for the inherited properties are non-strict: they are not required to
        // exist.
        (
            TypeInner::DefT(ureason, def_t),
            UseTInner::SuperT(box SuperTData {
                use_op,
                reason,
                derived_type: derived,
            }),
        ) if let DefTInner::InstanceT(inst_t) = def_t.deref()
            && matches!(
                &inst_t.inst.inst_kind,
                InstanceKind::ClassKind | InstanceKind::InterfaceKind { .. }
            ) =>
        {
            let st = &inst_t.static_;
            let DerivedType {
                own,
                proto,
                static_: static_props,
            } = derived;
            for (x, p) in own.iter() {
                check_super(cx, trace, use_op.dupe(), reason, ureason, l, x, p)?;
            }
            for (x, p) in proto.iter() {
                if inherited_method(x) {
                    check_super(cx, trace, use_op.dupe(), reason, ureason, l, x, p)?;
                }
            }
            // TODO: inherited_method logic no longer applies for statics. It used to
            // when call properties were included in the props, but that is no longer
            // the case. All that remains is the "constructor" prop, which has no
            // special meaning on the static object.
            for (x, p) in static_props.iter() {
                if inherited_method(x) {
                    check_super(cx, trace, use_op.dupe(), reason, ureason, st, x, p)?;
                }
            }
        }
        // Keep opaque types in computed object keys
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind: ConcretizationKind::ConcretizeForComputedObjectKeys,
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }

        // ***********************
        // * opaque types part 2 *
        // ***********************

        // Predicate_kit should not see unwrapped opaque type
        (
            TypeInner::NominalT { .. },
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind: ConcretizationKind::ConcretizeForPredicate(_),
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }
        (
            TypeInner::NominalT { .. },
            UseTInner::SealGenericT(box SealGenericTData {
                reason: _,
                id,
                name,
                cont,
                no_infer,
            }),
        ) => {
            let reason = reason_of_t(l);
            let generic = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                reason: reason.dupe(),
                id: id.clone(),
                name: name.dupe(),
                bound: l.dupe(),
                no_infer: *no_infer,
            })));
            continue_(cx, trace, &generic, cont)?;
        }
        // Preserve NominalT as consequent, but branch based on the bound
        (
            TypeInner::NominalT {
                nominal_type: opaque,
                ..
            },
            UseTInner::CondT(box CondTData {
                reason: r,
                opt_type: then_t_opt,
                true_t: else_t,
                false_t: tout,
            }),
        ) if opaque.upper_t.is_some() => {
            let t = opaque.upper_t.as_ref().unwrap();
            let new_then_t_opt = match then_t_opt {
                Some(_) => then_t_opt.dupe(),
                None => Some(l.dupe()),
            };
            rec_flow(
                cx,
                trace,
                (
                    t,
                    &UseT::new(UseTInner::CondT(Box::new(CondTData {
                        reason: r.dupe(),
                        opt_type: new_then_t_opt,
                        true_t: else_t.dupe(),
                        false_t: tout.dupe(),
                    }))),
                ),
            )?;
        }
        // Opaque types may be treated as their supertype when they are a lower bound for a use
        (
            TypeInner::NominalT {
                reason: opaque_t_reason,
                nominal_type: opaque,
            },
            _,
        ) if opaque.upper_t.is_some() => {
            let t = opaque.upper_t.as_ref().unwrap();
            let u_mod = type_util::mod_use_op_of_use_t(
                |use_op: &UseOp| {
                    VirtualUseOp::Frame(
                        Arc::new(FrameUseOp::OpaqueTypeUpperBound {
                            opaque_t_reason: opaque_t_reason.dupe(),
                        }),
                        Arc::new(use_op.dupe()),
                    )
                },
                u,
            );
            rec_flow(cx, trace, (t, &u_mod))?;
        }
        // Concretize types for type operation purpose up to this point. The rest are
        // recorded as lower bound to the target tvar.
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind: ConcretizationKind::ConcretizeForOperatorsChecking,
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }
        // **************************************************************************
        // * final shared concretization point for predicate and sentinel prop test *
        // **************************************************************************
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind:
                    ConcretizationKind::ConcretizeForPredicate(_)
                    | ConcretizationKind::ConcretizeForSentinelPropTest,
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }
        // ******************************
        // * functions statics - part B *
        // ******************************
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::FunT(static_, _) = def_t.deref()
                && flow_js_utils::object_like_op(u) =>
        {
            rec_flow(
                cx,
                trace,
                (
                    static_,
                    &UseT::new(UseTInner::ReposLowerT {
                        reason: reason.dupe(),
                        use_desc: false,
                        use_t: Box::new(u.dupe()),
                    }),
                ),
            )?;
        }
        // *****************************************
        // * classes can have their prototype read *
        // *****************************************
        (TypeInner::DefT(reason, def_t), UseTInner::GetPropT(box data))
            if let DefTInner::ClassT(instance) = def_t.deref()
                && matches!(data.propref.as_ref(), PropRef::Named { name, .. } if name == &Name::new("prototype")) =>
        {
            let instance = helpers::reposition(
                cx,
                Some(trace),
                reason.loc().dupe(),
                None,
                None,
                instance.dupe(),
            )?;
            let open_tout = Type::new(TypeInner::OpenT((*data.tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&instance, &open_tout))?;
        }

        // *****************
        // * class statics *
        // *****************

        // For Get/SetPrivatePropT or PrivateMethodT, the instance id is needed to determine whether
        // or not the private static field exists on that class. Since we look through the scopes for
        // the type of the field, there is no need to look at the static member of the instance.
        // Instead, we just flip the boolean flag to true, indicating that when the
        // InstanceT ~> Set/GetPrivatePropT or PrivateMethodT constraint is processed that we should
        // look at the private static fields instead of the private instance fields.
        (TypeInner::DefT(reason, def_t), UseTInner::GetPrivatePropT(box gpp_data))
            if let DefTInner::ClassT(instance) = def_t.deref() =>
        {
            let new_u = UseT::new(UseTInner::GetPrivatePropT(Box::new(GetPrivatePropTData {
                use_op: gpp_data.use_op.dupe(),
                reason: gpp_data.reason.dupe(),
                name: gpp_data.name.dupe(),
                class_bindings: gpp_data.class_bindings.dupe(),
                static_: true,
                tout: gpp_data.tout.clone(),
            })));
            rec_flow(
                cx,
                trace,
                (
                    instance,
                    &UseT::new(UseTInner::ReposLowerT {
                        reason: reason.dupe(),
                        use_desc: false,
                        use_t: Box::new(new_u),
                    }),
                ),
            )?;
        }
        (TypeInner::DefT(reason, def_t), UseTInner::SetPrivatePropT(box spp_data))
            if let DefTInner::ClassT(instance) = def_t.deref() =>
        {
            let new_u = UseT::new(UseTInner::SetPrivatePropT(Box::new(SetPrivatePropTData {
                use_op: spp_data.use_op.dupe(),
                reason: spp_data.reason.dupe(),
                name: spp_data.name.dupe(),
                set_mode: spp_data.set_mode.clone(),
                class_bindings: spp_data.class_bindings.dupe(),
                static_: true,
                write_ctx: spp_data.write_ctx.clone(),
                tin: spp_data.tin.dupe(),
                tout: spp_data.tout.clone(),
            })));
            rec_flow(
                cx,
                trace,
                (
                    instance,
                    &UseT::new(UseTInner::ReposLowerT {
                        reason: reason.dupe(),
                        use_desc: false,
                        use_t: Box::new(new_u),
                    }),
                ),
            )?;
        }
        (TypeInner::DefT(reason, def_t), UseTInner::PrivateMethodT(box pm_data))
            if let DefTInner::ClassT(instance) = def_t.deref() =>
        {
            let new_u = UseT::new(UseTInner::PrivateMethodT(Box::new(PrivateMethodTData {
                use_op: pm_data.use_op.dupe(),
                reason: pm_data.reason.dupe(),
                prop_reason: pm_data.prop_reason.dupe(),
                name: pm_data.name.dupe(),
                class_bindings: pm_data.class_bindings.dupe(),
                static_: true,
                method_action: pm_data.method_action.clone(),
            })));
            rec_flow(
                cx,
                trace,
                (
                    instance,
                    &UseT::new(UseTInner::ReposLowerT {
                        reason: reason.dupe(),
                        use_desc: false,
                        use_t: Box::new(new_u),
                    }),
                ),
            )?;
        }
        (
            TypeInner::DefT(reason, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: reason_call,
                prop_reason: reason_lookup,
                propref,
                method_action: action,
            }),
        ) if let DefTInner::ClassT(instance) = def_t.deref() => {
            let statics_tvar = flow_typing_tvar::mk_no_wrap(cx, reason);
            let statics = Tvar::new(reason.dupe(), statics_tvar as u32);
            rec_flow(
                cx,
                trace,
                (
                    instance,
                    &UseT::new(UseTInner::GetStaticsT(Box::new(statics.dupe()))),
                ),
            )?;
            let method_type = flow_typing_tvar::mk_no_wrap_where(
                cx,
                reason_lookup.dupe(),
                |cx, tout_reason, tout_id| {
                    let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                    let get_prop_u = UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: reason_lookup.dupe(),
                        id: None,
                        from_annot: false,
                        skip_optional: false,
                        propref: propref.clone(),
                        tout: Box::new(tout),
                        hint: hint_unavailable(),
                    })));
                    let open_statics = Type::new(TypeInner::OpenT(statics.dupe()));
                    rec_flow(
                        cx,
                        trace,
                        (
                            &open_statics,
                            &UseT::new(UseTInner::ReposLowerT {
                                reason: reason.dupe(),
                                use_desc: false,
                                use_t: Box::new(get_prop_u),
                            }),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
            )?;
            apply_method_action(
                cx,
                trace,
                &method_type,
                use_op.dupe(),
                reason_call.dupe(),
                l.dupe(),
                action,
            )?;
        }
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::ClassT(instance) = def_t.deref()
                && flow_js_utils::object_like_op(u) =>
        {
            let statics_tvar = flow_typing_tvar::mk_no_wrap(cx, reason);
            let statics = Tvar::new(reason.dupe(), statics_tvar as u32);
            rec_flow(
                cx,
                trace,
                (
                    instance,
                    &UseT::new(UseTInner::GetStaticsT(Box::new(statics.dupe()))),
                ),
            )?;
            let open_statics = Type::new(TypeInner::OpenT(statics.dupe()));
            rec_flow(cx, trace, (&open_statics, u))?;
        }
        // ************************
        // * classes as functions *
        // ************************

        /*
          When a class value flows to a function annotation or call site, check for
          the presence of a call property in the former (as a static) compatible
          with the latter.

          TODO: Call properties are excluded from the subclass compatibility
          checks, which makes it unsafe to call a Class<T> type like this.
          For example:

              declare class A { static (): string };
              declare class B extends A { static (): number }
              var klass: Class<A> = B;
              var foo: string = klass(); // passes, but `foo` is a number

          The same issue is also true for constructors, which are similarly
          excluded from subclass compatibility checks, but are allowed on ClassT
          types.
        */
        (TypeInner::DefT(reason, def_t), UseTInner::CallT(..))
            if let DefTInner::ClassT(instance) = def_t.deref() =>
        {
            let statics_tvar = flow_typing_tvar::mk_no_wrap(cx, reason);
            let statics = Tvar::new(reason.dupe(), statics_tvar as u32);
            rec_flow(
                cx,
                trace,
                (
                    instance,
                    &UseT::new(UseTInner::GetStaticsT(Box::new(statics.dupe()))),
                ),
            )?;
            let open_statics = Type::new(TypeInner::OpenT(statics.dupe()));
            rec_flow(cx, trace, (&open_statics, u))?;
        }
        // *********
        // * enums *
        // *********
        (TypeInner::DefT(enum_reason, def_t), UseTInner::GetPropT(box data))
            if let DefTInner::EnumObjectT {
                enum_value_t,
                enum_info,
            } = def_t.deref()
                && let EnumInfoInner::ConcreteEnum(enum_info) = EnumInfo::deref(enum_info)
                && let PropRef::Named {
                    reason: prop_reason,
                    name: member_name,
                    ..
                } = data.propref.as_ref() =>
        {
            flow_js_utils::get_prop_t_kit::on_enum_object_t::<FlowJs>(
                cx,
                &trace,
                enum_reason,
                l.dupe(),
                enum_value_t.dupe(),
                enum_info,
                &(
                    data.use_op.dupe(),
                    data.reason.dupe(),
                    None,
                    (prop_reason.dupe(), member_name.dupe()),
                ),
            )?(cx, (*data.tout).dupe())?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::TestPropT(box TestPropTData {
                reason,
                propref,
                tout,
                hint,
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::EnumObjectT { .. }) => {
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: VirtualUseOp::Op(Arc::new(VirtualRootUseOp::GetProperty(
                            reason.dupe(),
                        ))),
                        reason: reason.dupe(),
                        id: None,
                        from_annot: false,
                        skip_optional: false,
                        propref: propref.clone(),
                        tout: tout.clone(),
                        hint: hint.clone(),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: call_reason,
                prop_reason: lookup_reason,
                propref,
                method_action: action,
            }),
        ) if let DefTInner::EnumObjectT {
            enum_value_t,
            enum_info,
        } = def_t.deref()
            && matches!(&**propref, PropRef::Named { .. }) =>
        {
            let t = flow_typing_tvar::mk_no_wrap_where(
                cx,
                lookup_reason.dupe(),
                |cx, tout_reason, tout_id| {
                    let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                    let representation_t = match EnumInfo::deref(enum_info) {
                        EnumInfoInner::ConcreteEnum(ce) => &ce.representation_t,
                        EnumInfoInner::AbstractEnum { representation_t } => representation_t,
                    };
                    let proto = enum_proto(
                        cx,
                        lookup_reason.dupe(),
                        l.dupe(),
                        enum_value_t.dupe(),
                        representation_t.dupe(),
                    );
                    rec_flow(
                        cx,
                        trace,
                        (
                            &proto,
                            &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                                use_op: use_op.dupe(),
                                reason: lookup_reason.dupe(),
                                id: None,
                                from_annot: false,
                                skip_optional: false,
                                propref: propref.clone(),
                                tout: Box::new(tout),
                                hint: hint_unavailable(),
                            }))),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
            )?;
            apply_method_action(
                cx,
                trace,
                &t,
                use_op.dupe(),
                call_reason.dupe(),
                l.dupe(),
                action,
            )?;
        }
        (
            TypeInner::DefT(enum_reason, def_t),
            UseTInner::GetElemT(box GetElemTData { key_t, tout, .. }),
        ) if matches!(def_t.deref(), DefTInner::EnumObjectT { .. }) => {
            let reason = reason_of_t(key_t);
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidMemberAccess(Box::new(
                    EnumInvalidMemberAccessData {
                        member_name: None,
                        suggestion: None,
                        reason: reason.dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))),
            )?;
            let any = any_t::error(reason.dupe());
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (&any, &open_tout))?;
        }
        (
            TypeInner::DefT(enum_reason, def_t),
            UseTInner::SetPropT(_, op_reason, _, _, _, _, tout),
        ) if matches!(def_t.deref(), DefTInner::EnumObjectT { .. }) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumModification(Box::new(
                    EnumModificationData {
                        loc: op_reason.loc().dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))),
            )?;
            if let Some(tout) = tout {
                let any = any_t::error(op_reason.dupe());
                rec_flow_t(cx, trace, unknown_use(), (&any, tout))?;
            }
        }
        (
            TypeInner::DefT(enum_reason, def_t),
            UseTInner::SetElemT(box SetElemTData {
                use_op: _,
                reason: op_reason,
                key_t: _,
                set_mode: _,
                tin: _,
                tout,
            }),
        ) if matches!(def_t.deref(), DefTInner::EnumObjectT { .. }) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumModification(Box::new(
                    EnumModificationData {
                        loc: op_reason.loc().dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))),
            )?;
            if let Some(tout) = tout {
                let any = any_t::error(op_reason.dupe());
                rec_flow_t(cx, trace, unknown_use(), (&any, tout))?;
            }
        }
        (TypeInner::DefT(enum_reason, def_t), UseTInner::GetValuesT(op_reason, tout))
            if matches!(def_t.deref(), DefTInner::EnumObjectT { .. }) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectUtilType(Box::new(
                    EnumInvalidObjectUtilTypeData {
                        reason: op_reason.dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))),
            )?;
            let any = any_t::error(op_reason.dupe());
            rec_flow_t(cx, trace, unknown_use(), (&any, tout))?;
        }
        (TypeInner::DefT(enum_reason, def_t), UseTInner::GetDictValuesT(reason, result))
            if matches!(def_t.deref(), DefTInner::EnumObjectT { .. }) =>
        {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EEnumError(EnumErrorKind::EnumInvalidObjectFunction(Box::new(
                    EnumInvalidObjectFunctionData {
                        reason: reason.dupe(),
                        enum_reason: enum_reason.dupe(),
                    },
                ))),
            )?;
            let any = any_t::error(reason.dupe());
            rec_flow(cx, trace, (&any, result))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: call_reason,
                prop_reason: lookup_reason,
                propref,
                method_action: action,
            }),
        ) if let DefTInner::EnumValueT(enum_info) = def_t.deref()
            && matches!(&**propref, PropRef::Named { .. }) =>
        {
            let representation_t = match EnumInfo::deref(enum_info) {
                EnumInfoInner::ConcreteEnum(ce) => &ce.representation_t,
                EnumInfoInner::AbstractEnum { representation_t } => representation_t,
            };
            let enum_value_proto = get_builtin_typeapp(
                cx,
                lookup_reason,
                None,
                "$EnumValueProto",
                vec![l.dupe(), representation_t.dupe()],
            );
            let t = flow_typing_tvar::mk_no_wrap_where(
                cx,
                lookup_reason.dupe(),
                |cx, tout_reason, tout_id| {
                    let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                    rec_flow(
                        cx,
                        trace,
                        (
                            &enum_value_proto,
                            &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                                use_op: use_op.dupe(),
                                reason: lookup_reason.dupe(),
                                id: None,
                                from_annot: false,
                                skip_optional: false,
                                propref: propref.clone(),
                                tout: Box::new(tout),
                                hint: hint_unavailable(),
                            }))),
                        ),
                    )?;
                    Ok::<(), FlowJsException>(())
                },
            )?;
            apply_method_action(
                cx,
                trace,
                &t,
                use_op.dupe(),
                call_reason.dupe(),
                l.dupe(),
                action,
            )?;
        }
        // **************************************************************************
        // TestPropT is emitted for property reads in the context of branch tests.
        // Such tests are always non-strict, in that we don't immediately report an
        // error if the property is not found not in the object type. Instead, if
        // the property is not found, we control the result type of the read based
        // on the flags on the object type. For exact object types, the
        // result type is `void`; otherwise, it is "unknown". Indeed, if the
        // property is not found in an exact object type, we can be sure it
        // won't exist at run time, so the read will return undefined; but for other
        // object types, the property *might* exist at run time, and since we don't
        // know what the type of the property would be, we set things up so that the
        // result of the read cannot be used in any interesting way. *)
        // **************************************************************************
        (
            TypeInner::DefT(_, def_t),
            UseTInner::TestPropT(box TestPropTData {
                use_op,
                reason,
                id,
                propref,
                tout,
                hint,
            }),
        ) if matches!(def_t.deref(), DefTInner::NullT | DefTInner::VoidT) => {
            // The wildcard TestPropT implementation forwards the lower bound to
            // LookupT. This is unfortunate, because LookupT is designed to terminate
            // (successfully) on NullT, but property accesses on null should be type
            // errors. Ideally, we should prevent LookupT constraints from being
            // syntax-driven, in order to preserve the delicate invariants that
            // surround it.
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        id: Some(*id),
                        from_annot: false,
                        skip_optional: false,
                        propref: propref.clone(),
                        tout: tout.clone(),
                        hint: hint.clone(),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::DefT(r, def_t),
            UseTInner::TestPropT(box TestPropTData {
                use_op, id, tout, ..
            }),
        ) if matches!(
            def_t.deref(),
            DefTInner::MixedT(MixedFlavor::MixedTruthy)
                | DefTInner::MixedT(MixedFlavor::MixedNonMaybe)
        ) =>
        {
            // Special-case property tests of definitely non-null/non-void values to
            // return mixed and treat them as a hit.
            cx.test_prop_hit(*id);
            let mixed = Type::new(TypeInner::DefT(
                r.dupe(),
                DefT::new(DefTInner::MixedT(MixedFlavor::MixedEverything)),
            ));
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, use_op.dupe(), (&mixed, &open_tout))?;
        }
        (
            _,
            UseTInner::TestPropT(box TestPropTData {
                use_op,
                reason,
                id,
                propref,
                tout,
                hint,
            }),
        ) if matches!(&**propref, PropRef::Named { name, .. } if name == &Name::new("constructor")) =>
        {
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: reason.dupe(),
                        id: Some(*id),
                        from_annot: false,
                        skip_optional: false,
                        propref: propref.clone(),
                        tout: tout.clone(),
                        hint: hint.clone(),
                    }))),
                ),
            )?;
        }
        (
            _,
            UseTInner::TestPropT(box TestPropTData {
                use_op,
                reason: reason_op,
                id,
                propref,
                tout,
                hint: _,
            }),
        ) => {
            // NonstrictReturning lookups unify their result, but we don't want to
            // unify with the tout tvar directly, so we create an indirection here to
            // ensure we only supply lower bounds to tout.
            let lookup_default = flow_typing_tvar::mk_where(cx, reason_op.dupe(), |cx, tvar| {
                let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
                rec_flow_t(cx, trace, use_op.dupe(), (tvar, &open_tout))?;
                Ok::<(), FlowJsException>(())
            })?;
            let name = name_of_propref(propref);
            let reason_prop = match &**propref {
                PropRef::Named { reason, .. } => reason,
                PropRef::Computed(_) => reason_op,
            };
            let test_info = Some((*id, (reason_prop.dupe(), reason_of_t(l).dupe())));
            let lookup_default = match l.deref() {
                TypeInner::DefT(_, obj_def) if matches!(obj_def.deref(), DefTInner::ObjT(obj) if obj_type::is_exact(&obj.flags.obj_kind)) =>
                {
                    let r = reason_op
                        .dupe()
                        .replace_desc(VirtualReasonDesc::RMissingProperty(name.dupe()));
                    Some((
                        Type::new(TypeInner::DefT(r, DefT::new(DefTInner::VoidT))),
                        lookup_default,
                    ))
                }
                _ => {
                    // Note: a lot of other types could in principle be considered
                    // "exact". For example, new instances of classes could have exact
                    // types; so could `super` references (since they are statically
                    // rather than dynamically bound). However, currently we don't support
                    // any other exact types. Considering exact types inexact is sound, so
                    // there is no problem falling back to the same conservative
                    // approximation we use for inexact types in those cases.
                    let r = reason_op
                        .dupe()
                        .replace_desc(VirtualReasonDesc::RUnknownProperty(name.dupe()));
                    Some((
                        Type::new(TypeInner::DefT(
                            r,
                            DefT::new(DefTInner::MixedT(MixedFlavor::MixedEverything)),
                        )),
                        lookup_default,
                    ))
                }
            };
            let lookup_kind = LookupKind::NonstrictReturning(Box::new(NonstrictReturningData(
                lookup_default,
                test_info,
            )));
            let method_accessible = match l.deref() {
                TypeInner::DefT(_, d) if matches!(d.deref(), DefTInner::InstanceT(_)) => false,
                _ => true,
            };
            rec_flow(
                cx,
                trace,
                (
                    l,
                    &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                        reason: reason_op.dupe(),
                        lookup_kind: Box::new(lookup_kind),
                        try_ts_on_failure: vec![].into(),
                        propref: propref.clone(),
                        lookup_action: Box::new(LookupAction::ReadProp(Box::new(ReadPropData {
                            use_op: use_op.dupe(),
                            obj_t: l.dupe(),
                            tout: (**tout).dupe(),
                        }))),
                        method_accessible,
                        ids: Some(Default::default()),
                        ignore_dicts: false,
                    }))),
                ),
            )?;
        }

        // ***************************
        // * conditional type switch *
        // ***************************

        // Use our alternate if our lower bound is empty.
        (
            TypeInner::DefT(_, def_t),
            UseTInner::CondT(box CondTData {
                reason: _,
                opt_type: _,
                true_t: else_t,
                false_t: tout,
            }),
        ) if matches!(def_t.deref(), DefTInner::EmptyT) => {
            rec_flow_t(cx, trace, unknown_use(), (else_t, tout))?;
        }
        // Otherwise continue by Flowing out lower bound to tout.
        (
            _,
            UseTInner::CondT(box CondTData {
                reason: _,
                opt_type: then_t_opt,
                true_t: _,
                false_t: tout,
            }),
        ) => {
            //   let then_t =
            //     match then_t_opt with
            //     | Some t -> t
            //     | None -> l
            //   in
            let then_t = match then_t_opt {
                Some(t) => t,
                None => l,
            };
            rec_flow_t(cx, trace, unknown_use(), (then_t, tout))?;
        }
        // *****************
        // * repositioning *
        // *****************

        // waits for a lower bound to become concrete, and then repositions it to
        // the location stored in the ReposLowerT, which is usually the location
        // where that lower bound was used; the lower bound's location (which is
        // being overwritten) is where it was defined.
        (
            _,
            UseTInner::ReposLowerT {
                reason,
                use_desc,
                use_t: u_inner,
            },
        ) => {
            let repos_l = helpers::reposition_reason(cx, Some(trace), reason, *use_desc, l)?;
            rec_flow(cx, trace, (&repos_l, u_inner))?;
        }

        // ***********************************************************
        // * generics                                                *
        // ***********************************************************
        (
            _,
            UseTInner::SealGenericT(box SealGenericTData {
                reason: _,
                id,
                name,
                cont,
                no_infer,
            }),
        ) => {
            let reason = reason_of_t(l);
            let generic = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                reason: reason.dupe(),
                id: id.clone(),
                name: name.dupe(),
                bound: l.dupe(),
                no_infer: *no_infer,
            })));
            continue_(cx, trace, &generic, cont)?;
        }
        (TypeInner::GenericT(box GenericTData { reason, bound, .. }), _) => {
            let repos = helpers::reposition_reason(cx, None, reason, false, bound)?;
            rec_flow(cx, trace, (&repos, u))?;
        }
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                kind: ConcretizationKind::ConcretizeForEnumExhaustiveCheck,
                collector,
                ..
            }),
        ) => {
            collector.add(l.dupe());
        }
        // ************
        // * GetEnumT *
        // ************
        (
            TypeInner::DefT(enum_reason, def_t),
            UseTInner::GetEnumT(box GetEnumTData {
                use_op,
                orig_t,
                kind: GetEnumKind::GetEnumObject,
                tout,
                ..
            }),
        ) if let DefTInner::EnumValueT(enum_info) = def_t.deref() => {
            let enum_value_t = match orig_t {
                Some(t) => t.dupe(),
                None => l.dupe(),
            };
            let enum_obj = Type::new(TypeInner::DefT(
                enum_reason.dupe(),
                DefT::new(DefTInner::EnumObjectT {
                    enum_value_t,
                    enum_info: enum_info.dupe(),
                }),
            ));
            rec_flow(
                cx,
                trace,
                (
                    &enum_obj,
                    &UseT::new(UseTInner::UseT(use_op.dupe(), tout.dupe())),
                ),
            )?;
        }
        (
            _,
            UseTInner::GetEnumT(box GetEnumTData {
                use_op,
                kind: GetEnumKind::GetEnumObject,
                tout,
                ..
            }),
        ) => {
            rec_flow(
                cx,
                trace,
                (l, &UseT::new(UseTInner::UseT(use_op.dupe(), tout.dupe()))),
            )?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::GetEnumT(box GetEnumTData {
                use_op,
                kind: GetEnumKind::GetEnumValue,
                tout,
                ..
            }),
        ) if let DefTInner::EnumObjectT { enum_value_t, .. } = def_t.deref() => {
            rec_flow(
                cx,
                trace,
                (
                    enum_value_t,
                    &UseT::new(UseTInner::UseT(use_op.dupe(), tout.dupe())),
                ),
            )?;
        }
        // **********************************
        // * Flow Enums exhaustive checking *
        // **********************************
        (
            _,
            UseTInner::GetEnumT(box GetEnumTData {
                use_op,
                kind: GetEnumKind::GetEnumValue,
                tout,
                ..
            }),
        ) => {
            rec_flow(
                cx,
                trace,
                (l, &UseT::new(UseTInner::UseT(use_op.dupe(), tout.dupe()))),
            )?;
        }
        // ***************
        // * unsupported *
        // ***************

        // Lookups can be strict or non-strict, as denoted by the presence or
        // absence of strict_reason in the following two pattern matches.
        // Strictness derives from whether the object is sealed and was
        // created in the same scope in which the lookup occurs - see
        // mk_strict_lookup_reason below. The failure of a strict lookup
        // to find the desired property causes an error; a non-strict one
        // does not.
        (
            TypeInner::DefT(_, def_t),
            UseTInner::LookupT(box LookupTData {
                reason,
                lookup_kind,
                try_ts_on_failure,
                propref,
                lookup_action,
                method_accessible,
                ids,
                ignore_dicts,
            }),
        ) if matches!(def_t.deref(), DefTInner::NullT) && !try_ts_on_failure.is_empty() => {
            let next = &try_ts_on_failure[0];
            let rest = try_ts_on_failure[1..].to_vec();
            // When s is not found, we always try to look it up in the next element in
            // the list try_ts_on_failure.
            rec_flow(
                cx,
                trace,
                (
                    next,
                    &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                        reason: reason.dupe(),
                        lookup_kind: lookup_kind.clone(),
                        try_ts_on_failure: rest.into(),
                        propref: propref.clone(),
                        lookup_action: lookup_action.clone(),
                        method_accessible: *method_accessible,
                        ids: ids.dupe(),
                        ignore_dicts: *ignore_dicts,
                    }))),
                ),
            )?;
        }
        (
            TypeInner::ObjProtoT(_),
            UseTInner::LookupT(box LookupTData {
                reason,
                lookup_kind,
                try_ts_on_failure,
                propref,
                lookup_action,
                method_accessible,
                ids,
                ignore_dicts,
            }),
        ) if !try_ts_on_failure.is_empty() => {
            // __proto__ is a getter/setter on Object.prototype
            let next = &try_ts_on_failure[0];
            let rest = try_ts_on_failure[1..].to_vec();
            rec_flow(
                cx,
                trace,
                (
                    next,
                    &UseT::new(UseTInner::LookupT(Box::new(LookupTData {
                        reason: reason.dupe(),
                        lookup_kind: lookup_kind.clone(),
                        try_ts_on_failure: rest.into(),
                        propref: propref.clone(),
                        lookup_action: lookup_action.clone(),
                        method_accessible: *method_accessible,
                        ids: ids.dupe(),
                        ignore_dicts: *ignore_dicts,
                    }))),
                ),
            )?;
        }
        (
            TypeInner::ObjProtoT(_) | TypeInner::FunProtoT(_),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind: _,
                try_ts_on_failure,
                propref: box PropRef::Named { name, .. },
                lookup_action:
                    box LookupAction::ReadProp(box ReadPropData {
                        use_op: _,
                        obj_t: lookup_l,
                        tout,
                    }),
                ids: _,
                method_accessible: _,
                ignore_dicts: _,
            }),
        ) if try_ts_on_failure.is_empty() && name == &Name::new("__proto__") => {
            // __proto__ is a getter/setter on Object.prototype
            rec_flow(
                cx,
                trace,
                (
                    lookup_l,
                    &UseT::new(UseTInner::GetProtoT(
                        reason_op.dupe(),
                        Box::new(tout.clone()),
                    )),
                ),
            )?;
        }
        (
            TypeInner::ObjProtoT(_) | TypeInner::FunProtoT(_),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind: _,
                try_ts_on_failure,
                propref: box PropRef::Named { name, .. },
                lookup_action:
                    box LookupAction::WriteProp(box WritePropData {
                        use_op: _,
                        obj_t: lookup_l,
                        prop_tout: _,
                        tin,
                        write_ctx: _,
                        mode: _,
                    }),
                method_accessible: _,
                ids: _,
                ignore_dicts: _,
            }),
        ) if try_ts_on_failure.is_empty() && name == &Name::new("__proto__") => {
            // __proto__ is a getter/setter on Object.prototype
            rec_flow(
                cx,
                trace,
                (
                    lookup_l,
                    &UseT::new(UseTInner::SetProtoT(reason_op.dupe(), tin.dupe())),
                ),
            )?;
        }
        (
            TypeInner::ObjProtoT(_),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                try_ts_on_failure,
                propref: box PropRef::Named { name, .. },
                ..
            }),
        ) if try_ts_on_failure.is_empty() && flow_js_utils::is_object_prototype_method(name) => {
            // TODO: These properties should go in Object.prototype. Currently we
            // model Object.prototype as a ObjProtoT, as an optimization against a
            // possible deluge of shadow properties on Object.prototype, since it
            // is shared by every object.
            let obj = helpers::get_builtin_type(cx, Some(trace), reason_op, None, "Object")?;
            rec_flow(cx, trace, (&obj, u))?;
        }
        //   rec_flow cx trace (get_builtin_type cx ~trace reason_op "Function", u)
        (
            TypeInner::FunProtoT(_),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                propref: box PropRef::Named { name, .. },
                ..
            }),
        ) if flow_js_utils::is_function_prototype(name) => {
            // TODO: Ditto above comment for Function.prototype
            let fun = helpers::get_builtin_type(cx, Some(trace), reason_op, None, "Function")?;
            rec_flow(cx, trace, (&fun, u))?;
        }
        (
            TypeInner::DefT(reason, def_t),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind: box LookupKind::Strict(strict_reason),
                try_ts_on_failure,
                propref,
                lookup_action: action,
                method_accessible: _,
                ids,
                ignore_dicts: _,
            }),
        ) if matches!(def_t.deref(), DefTInner::NullT)
            && try_ts_on_failure.is_empty()
            && let PropRef::Named {
                reason: reason_prop,
                name,
                ..
            } = &**propref =>
        {
            let suggestion: Option<FlowSmolStr> = ids.as_ref().and_then(|ids| {
                let ids_vec: Vec<_> = ids.iter().duped().collect();
                prop_typo_suggestion(cx, &ids_vec, name.as_str())
            });
            let error_message = match action {
                box LookupAction::LookupPropForSubtyping(box LookupPropForSubtypingData {
                    use_op,
                    prop: _,
                    prop_name,
                    reason_lower,
                    reason_upper,
                }) => {
                    ErrorMessage::EPropNotFoundInSubtyping(Box::new(EPropNotFoundInSubtypingData {
                        prop_name: Some(prop_name.dupe()),
                        suggestion,
                        reason_lower: reason_lower.dupe(),
                        reason_upper: reason_upper.dupe(),
                        use_op: use_op.dupe(),
                    }))
                }
                _ => {
                    let use_op = flow_js_utils::use_op_of_lookup_action(action);
                    ErrorMessage::EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                        reason_prop: reason_prop.dupe(),
                        reason_obj: strict_reason.dupe(),
                        prop_name: Some(name.dupe()),
                        use_op,
                        suggestion,
                    }))
                }
            };
            flow_js_utils::add_output(cx, error_message)?;
            let p = PropertyType::OrdinaryField {
                type_: any_t::error_of_kind(AnyErrorKind::UnresolvedName, reason_op.dupe()),
                polarity: Polarity::Neutral,
            };
            perform_lookup_action(
                cx,
                trace,
                propref,
                &p,
                PropertySource::DynamicProperty,
                reason,
                reason_op,
                action,
            )?;
        }

        (
            TypeInner::ObjProtoT(reason) | TypeInner::FunProtoT(reason),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind: box LookupKind::Strict(strict_reason),
                try_ts_on_failure,
                propref,
                lookup_action: action,
                method_accessible: _,
                ids,
                ignore_dicts: _,
            }),
        ) if try_ts_on_failure.is_empty()
            && let PropRef::Named {
                reason: reason_prop,
                name,
                ..
            } = &**propref =>
        {
            let suggestion: Option<FlowSmolStr> = ids.as_ref().and_then(|ids| {
                let ids_vec: Vec<_> = ids.iter().duped().collect();
                prop_typo_suggestion(cx, &ids_vec, name.as_str())
            });
            let error_message = match action {
                box LookupAction::LookupPropForSubtyping(box LookupPropForSubtypingData {
                    use_op,
                    prop: _,
                    prop_name,
                    reason_lower,
                    reason_upper,
                }) => {
                    ErrorMessage::EPropNotFoundInSubtyping(Box::new(EPropNotFoundInSubtypingData {
                        prop_name: Some(prop_name.dupe()),
                        suggestion,
                        reason_lower: reason_lower.dupe(),
                        reason_upper: reason_upper.dupe(),
                        use_op: use_op.dupe(),
                    }))
                }
                _ => {
                    let use_op = flow_js_utils::use_op_of_lookup_action(action);
                    ErrorMessage::EPropNotFoundInLookup(Box::new(EPropNotFoundInLookupData {
                        reason_prop: reason_prop.dupe(),
                        reason_obj: strict_reason.dupe(),
                        prop_name: Some(name.dupe()),
                        use_op,
                        suggestion,
                    }))
                }
            };
            flow_js_utils::add_output(cx, error_message)?;
            let p = PropertyType::OrdinaryField {
                type_: any_t::error_of_kind(AnyErrorKind::UnresolvedName, reason_op.dupe()),
                polarity: Polarity::Neutral,
            };
            perform_lookup_action(
                cx,
                trace,
                propref,
                &p,
                PropertySource::DynamicProperty,
                reason,
                reason_op,
                action,
            )?;
        }
        (
            TypeInner::DefT(reason, def_t),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind: box LookupKind::Strict(strict_reason),
                try_ts_on_failure,
                propref,
                lookup_action: action,
                method_accessible: _,
                ids: _,
                ignore_dicts: _,
            }),
        ) if matches!(def_t.deref(), DefTInner::NullT)
            && try_ts_on_failure.is_empty()
            && let PropRef::Computed(elem_t) = &**propref =>
        {
            match elem_t.deref() {
                TypeInner::OpenT(_) => {
                    let loc = loc_of_t(elem_t);
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EInternal(Box::new((
                            loc.dupe(),
                            InternalError::PropRefComputedOpen,
                        ))),
                    )?;
                }
                TypeInner::DefT(_, inner_def)
                    if matches!(inner_def.deref(), DefTInner::SingletonStrT { .. }) =>
                {
                    let loc = loc_of_t(elem_t);
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EInternal(Box::new((
                            loc.dupe(),
                            InternalError::PropRefComputedLiteral,
                        ))),
                    )?;
                }
                TypeInner::AnyT(_, src) => {
                    let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
                    let p = PropertyType::OrdinaryField {
                        type_: any_t::why(src, reason_op.dupe()),
                        polarity: Polarity::Neutral,
                    };
                    perform_lookup_action(
                        cx,
                        trace,
                        propref,
                        &p,
                        PropertySource::DynamicProperty,
                        reason,
                        reason_op,
                        action,
                    )?;
                }
                _ => {
                    let reason_prop = reason_op;
                    let error_message = match action {
                        box LookupAction::LookupPropForSubtyping(
                            box LookupPropForSubtypingData {
                                use_op,
                                prop: _,
                                prop_name,
                                reason_lower,
                                reason_upper,
                            },
                        ) => ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                            EPropNotFoundInSubtypingData {
                                prop_name: Some(prop_name.dupe()),
                                suggestion: None,
                                reason_lower: reason_lower.dupe(),
                                reason_upper: reason_upper.dupe(),
                                use_op: use_op.dupe(),
                            },
                        )),
                        _ => {
                            let use_op = flow_js_utils::use_op_of_lookup_action(action);
                            ErrorMessage::EPropNotFoundInLookup(Box::new(
                                EPropNotFoundInLookupData {
                                    reason_prop: reason_prop.dupe(),
                                    reason_obj: strict_reason.dupe(),
                                    prop_name: None,
                                    use_op,
                                    suggestion: None,
                                },
                            ))
                        }
                    };
                    flow_js_utils::add_output(cx, error_message)?;
                }
            }
        }
        (
            TypeInner::ObjProtoT(reason) | TypeInner::FunProtoT(reason),
            UseTInner::LookupT(box LookupTData {
                reason: reason_op,
                lookup_kind: box LookupKind::Strict(strict_reason),
                try_ts_on_failure,
                propref,
                lookup_action: action,
                method_accessible: _,
                ids: _,
                ignore_dicts: _,
            }),
        ) if try_ts_on_failure.is_empty()
            && let PropRef::Computed(elem_t) = &**propref =>
        {
            match elem_t.deref() {
                TypeInner::OpenT(_) => {
                    let loc = loc_of_t(elem_t);
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EInternal(Box::new((
                            loc.dupe(),
                            InternalError::PropRefComputedOpen,
                        ))),
                    )?;
                }
                TypeInner::DefT(_, inner_def)
                    if matches!(inner_def.deref(), DefTInner::SingletonStrT { .. }) =>
                {
                    let loc = loc_of_t(elem_t);
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EInternal(Box::new((
                            loc.dupe(),
                            InternalError::PropRefComputedLiteral,
                        ))),
                    )?;
                }
                TypeInner::AnyT(_, src) => {
                    let src = flow_js_utils::any_mod_src_keep_placeholder(AnySource::Untyped, src);
                    let p = PropertyType::OrdinaryField {
                        type_: any_t::why(src, reason_op.dupe()),
                        polarity: Polarity::Neutral,
                    };
                    perform_lookup_action(
                        cx,
                        trace,
                        propref,
                        &p,
                        PropertySource::DynamicProperty,
                        reason,
                        reason_op,
                        action,
                    )?;
                }
                _ => {
                    let reason_prop = reason_op;
                    let error_message = match action {
                        box LookupAction::LookupPropForSubtyping(
                            box LookupPropForSubtypingData {
                                use_op,
                                prop: _,
                                prop_name,
                                reason_lower,
                                reason_upper,
                            },
                        ) => ErrorMessage::EPropNotFoundInSubtyping(Box::new(
                            EPropNotFoundInSubtypingData {
                                prop_name: Some(prop_name.dupe()),
                                suggestion: None,
                                reason_lower: reason_lower.dupe(),
                                reason_upper: reason_upper.dupe(),
                                use_op: use_op.dupe(),
                            },
                        )),
                        _ => {
                            let use_op = flow_js_utils::use_op_of_lookup_action(action);
                            ErrorMessage::EPropNotFoundInLookup(Box::new(
                                EPropNotFoundInLookupData {
                                    reason_prop: reason_prop.dupe(),
                                    reason_obj: strict_reason.dupe(),
                                    prop_name: None,
                                    use_op,
                                    suggestion: None,
                                },
                            ))
                        }
                    };
                    flow_js_utils::add_output(cx, error_message)?;
                }
            }
        }
        (
            _,
            UseTInner::LookupT(box LookupTData {
                lookup_kind:
                    box LookupKind::NonstrictReturning(box NonstrictReturningData(t_opt, test_opt)),
                try_ts_on_failure,
                propref,
                lookup_action: action,
                ids,
                ..
            }),
        ) if match l.deref() {
            TypeInner::ObjProtoT(_) | TypeInner::FunProtoT(_) => true,
            TypeInner::DefT(_, def_t) => matches!(def_t.deref(), DefTInner::NullT),
            _ => false,
        } && try_ts_on_failure.is_empty() =>
        {
            // don't fire
            //
            //  ...unless a default return value is given. Two examples:
            //
            //  1. A failure could arise when an unchecked module was looked up and
            //  not found declared, in which case we consider that module's exports to
            //  be `any`.
            //
            //  2. A failure could arise also when an object property is looked up in
            //  a condition, in which case we consider the object's property to be
            //  `mixed`.
            let use_op = flow_js_utils::use_op_of_lookup_action(action);
            if let Some((id, reasons)) = test_opt {
                let suggestion: Option<FlowSmolStr> = match &**propref {
                    PropRef::Named { name, .. } => ids.as_ref().and_then(|ids| {
                        let ids_vec: Vec<_> = ids.iter().duped().collect();
                        prop_typo_suggestion(cx, &ids_vec, name.as_str())
                    }),
                    _ => None,
                };
                if !matches!(
                    *cx.typing_mode(),
                    flow_typing_context::TypingMode::HintEvaluationMode
                ) {
                    cx.test_prop_miss(
                        *id,
                        name_of_propref(propref),
                        (reasons.0.dupe(), reasons.1.dupe()),
                        use_op.dupe(),
                        suggestion,
                    );
                }
            }
            match t_opt {
                Some((not_found, t)) => {
                    FlowJs::rec_unify(
                        cx,
                        trace,
                        use_op,
                        UnifyCause::Uncategorized,
                        Some(true),
                        t,
                        not_found,
                    )?;
                }
                None => {}
            }
        }
        // SuperT only involves non-strict lookups
        (TypeInner::DefT(_, def_t), UseTInner::SuperT(..))
            if matches!(def_t.deref(), DefTInner::NullT) => {}
        (TypeInner::ObjProtoT(_), UseTInner::SuperT(..)) => {}
        (TypeInner::FunProtoT(_), UseTInner::SuperT(..)) => {}
        // ExtendsUseT searches for a nominal superclass. The search terminates with
        // either failure at the root or a structural subtype check.
        (
            TypeInner::AnyT(_, src),
            UseTInner::ExtendsUseT(box ExtendsUseTData {
                use_op,
                reason: reason_op,
                targs: ts,
                true_t: t1,
                false_t: t2,
            }),
        ) => {
            for t in ts.iter() {
                let any = any_t::why(*src, reason_op.dupe());
                rec_flow_t(cx, trace, use_op.dupe(), (&any, t))?;
            }
            let any1 = any_t::why(*src, reason_op.dupe());
            rec_flow_t(cx, trace, use_op.dupe(), (&any1, t1))?;
            let any2 = any_t::why(*src, reason_op.dupe());
            rec_flow_t(cx, trace, use_op.dupe(), (&any2, t2))?;
        }
        (TypeInner::DefT(lreason, def_t), UseTInner::ExtendsUseT(..))
            if let DefTInner::ObjT(obj) = def_t.deref() =>
        {
            let proto_t = &obj.proto_t;
            let repositioned = helpers::reposition(
                cx,
                Some(trace),
                lreason.loc().dupe(),
                None,
                None,
                proto_t.dupe(),
            )?;
            rec_flow(cx, trace, (&repositioned, u))?;
        }
        (TypeInner::DefT(reason, def_t), UseTInner::ExtendsUseT(..))
            if let DefTInner::ClassT(instance) = def_t.deref() =>
        {
            let statics_tvar = flow_typing_tvar::mk_no_wrap(cx, reason);
            let statics = Tvar::new(reason.dupe(), statics_tvar as u32);
            rec_flow(
                cx,
                trace,
                (
                    instance,
                    &UseT::new(UseTInner::GetStaticsT(Box::new(statics.dupe()))),
                ),
            )?;
            let open_statics = Type::new(TypeInner::OpenT(statics));
            rec_flow(cx, trace, (&open_statics, u))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ExtendsUseT(box ExtendsUseTData {
                use_op,
                reason: reason_ext,
                targs: try_ts,
                true_t: ext_l,
                false_t: ext_u,
            }),
        ) if matches!(def_t.deref(), DefTInner::NullT) && !try_ts.is_empty() => {
            // When seaching for a nominal superclass fails, we always try to look it
            // up in the next element in the list try_ts_on_failure.
            let next = &try_ts[0];
            let rest = try_ts[1..].to_vec();
            rec_flow(
                cx,
                trace,
                (
                    next,
                    &UseT::new(UseTInner::ExtendsUseT(Box::new(ExtendsUseTData {
                        use_op: use_op.dupe(),
                        reason: reason_ext.dupe(),
                        targs: rest.into(),
                        true_t: ext_l.dupe(),
                        false_t: ext_u.dupe(),
                    }))),
                ),
            )?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ExtendsUseT(box ExtendsUseTData {
                use_op,
                reason: _,
                targs: try_ts,
                true_t: ext_l,
                false_t: ext_u,
            }),
        ) if matches!(def_t.deref(), DefTInner::NullT)
            && try_ts.is_empty()
            && let TypeInner::DefT(reason_inst, inner_def) = ext_u.deref()
            && let DefTInner::InstanceT(inst_t) = inner_def.deref()
            && matches!(inst_t.inst.inst_kind, InstanceKind::InterfaceKind { .. }) =>
        {
            let super_ = &inst_t.super_;
            let own_props = inst_t.inst.own_props.dupe();
            let proto_props = inst_t.inst.proto_props.dupe();
            let inst_call_t = inst_t.inst.inst_call_t;
            let inst_dict = &inst_t.inst.inst_dict;
            structural_subtype(
                cx,
                trace,
                use_op.dupe(),
                ext_l,
                reason_inst,
                (own_props, proto_props, inst_call_t, inst_dict),
            )?;
            rec_flow(
                cx,
                trace,
                (
                    ext_l,
                    &UseT::new(UseTInner::UseT(use_op.dupe(), super_.dupe())),
                ),
            )?;
        }
        // Unwrap deep readonly
        (_, UseTInner::DeepReadOnlyT(tout, _)) | (_, UseTInner::HooklikeT(tout)) => {
            let open_tout = Type::new(TypeInner::OpenT((**tout).dupe()));
            rec_flow_t(cx, trace, unknown_use(), (l, &open_tout))?;
        }
        // Render Type Misc Uses
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ExitRendersT {
                renders_reason,
                u: exit_u,
            },
        ) if let DefTInner::RendersT(renders) = def_t.deref()
            && matches!(
                renders.as_ref(),
                CanonicalRendersForm::IntrinsicRenders(_)
                    | CanonicalRendersForm::NominalRenders { .. }
            ) =>
        {
            let mixed_element = FlowJs::get_builtin_react_type(
                cx,
                Some(trace),
                renders_reason,
                None,
                flow_typing_errors::intermediate_error_types::ExpectedModulePurpose::ReactModuleForReactMixedElementType,
            )?;
            rec_flow(cx, trace, (&mixed_element, exit_u))?;
        }
        (TypeInner::DefT(renders_reason, def_t), _)
            if let DefTInner::RendersT(renders) = def_t.deref()
                && matches!(
                    renders.as_ref(),
                    CanonicalRendersForm::IntrinsicRenders(_)
                        | CanonicalRendersForm::NominalRenders { .. }
                ) =>
        {
            let mixed_element = FlowJs::get_builtin_react_type(
                cx,
                Some(trace),
                renders_reason,
                None,
                flow_typing_errors::intermediate_error_types::ExpectedModulePurpose::ReactModuleForReactMixedElementType,
            )?;
            rec_flow(cx, trace, (&mixed_element, u))?;
        }
        (TypeInner::DefT(r, def_t), _)
            if let DefTInner::RendersT(renders) = def_t.deref()
                && let CanonicalRendersForm::StructuralRenders {
                    renders_variant: RendersVariant::RendersNormal,
                    renders_structural_type,
                } = renders.as_ref() =>
        {
            let t = renders_structural_type;
            let exit_u = UseT::new(UseTInner::ExitRendersT {
                renders_reason: r.dupe(),
                u: Box::new(u.dupe()),
            });
            rec_flow(cx, trace, (t, &exit_u))?;
        }
        (
            _,
            UseTInner::ExitRendersT {
                renders_reason,
                u: exit_u,
            },
        ) => {
            let node = FlowJs::get_builtin_react_type(
                cx,
                Some(trace),
                renders_reason,
                None,
                flow_typing_errors::intermediate_error_types::ExpectedModulePurpose::ReactModuleForReactNodeType,
            )?;
            rec_flow(cx, trace, (&node, exit_u))?;
        }
        // ***********************
        // * Object library call *
        // ***********************
        (TypeInner::ObjProtoT(reason), _) => {
            let obj_proto =
                helpers::get_builtin_type(cx, Some(trace), reason, Some(true), "Object")?;
            rec_flow(cx, trace, (&obj_proto, u))?;
        }
        // *************************
        // * Function library call *
        // *************************
        (TypeInner::FunProtoT(reason), _) => {
            let fun_proto =
                helpers::get_builtin_type(cx, Some(trace), reason, Some(true), "Function")?;
            rec_flow(cx, trace, (&fun_proto, u))?;
        }
        (
            _,
            UseTInner::ExtendsUseT(box ExtendsUseTData {
                use_op,
                reason: _,
                targs: try_ts,
                true_t: t,
                false_t: tc,
            }),
        ) if try_ts.is_empty() => {
            let (reason_l, reason_u) = flow_typing_errors::flow_error::ordered_reasons((
                reason_of_t(t).dupe(),
                reason_of_t(tc).dupe(),
            ));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleWithUseOp(Box::new(EIncompatibleWithUseOpData {
                    reason_lower: reason_l,
                    reason_upper: reason_u,
                    use_op: use_op.dupe(),
                    explanation: None,
                })),
            )?;
        }
        // *********
        // * Match *
        // *********
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind: ConcretizationKind::ConcretizeForMatchArg { .. },
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }

        // ******************************
        // * String utils (e.g. prefix) *
        // ******************************

        // StrUtilT just becomes a StrT so we can access properties and methods.
        (
            TypeInner::StrUtilT {
                reason,
                op: StrUtilOp::StrPrefix(arg) | StrUtilOp::StrSuffix(arg),
                remainder: _,
            },
            _,
        ) => {
            let reason = reason.dupe().replace_desc(VirtualReasonDesc::RString);
            let literal_kind = if arg.is_empty() {
                Literal::AnyLiteral
            } else {
                Literal::Truthy
            };
            let str_t = Type::new(TypeInner::DefT(
                reason,
                DefT::new(DefTInner::StrGeneralT(literal_kind)),
            ));
            rec_flow(cx, trace, (&str_t, u))?;
        }
        // *******************************
        // * ToString abstract operation *
        // *******************************

        // ToStringT passes through strings unchanged, and flows a generic StrT otherwise
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ToStringT {
                orig_t: None,
                t_out,
                ..
            },
        ) if matches!(
            def_t.deref(),
            DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
        ) =>
        {
            rec_flow(cx, trace, (l, t_out))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::ToStringT {
                orig_t: Some(t),
                t_out,
                ..
            },
        ) if matches!(
            def_t.deref(),
            DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
        ) =>
        {
            rec_flow(cx, trace, (t, t_out))?;
        }
        (_, UseTInner::ToStringT { reason, t_out, .. }) => {
            let str_mod = str_module_t::why(reason.dupe());
            rec_flow(cx, trace, (&str_mod, t_out))?;
        }
        // **********************
        // * Array library call *
        // **********************
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::ArrT(arr) = def_t.deref()
                && let ArrType::ArrayAT(box ArrayATData {
                    elem_t,
                    react_dro: Some(ReactDro(dro_loc, dro_type)),
                    tuple_view,
                }) = arr.deref()
                && matches!(
                    u.deref(),
                    UseTInner::GetPropT(..)
                        | UseTInner::SetPropT(..)
                        | UseTInner::MethodT(..)
                        | UseTInner::LookupT(..)
                ) =>
        {
            match u.deref() {
                UseTInner::MethodT(box MethodTData {
                    use_op,
                    reason: _,
                    prop_reason: method_reason,
                    propref: box PropRef::Named { name, .. },
                    method_action: _,
                }) if matches!(
                    name.as_str(),
                    "fill" | "pop" | "push" | "reverse" | "shift" | "sort" | "splice" | "unshift"
                ) =>
                {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotReadable(Box::new(EPropNotReadableData {
                            reason_prop: method_reason.dupe(),
                            prop_name: Some(name.dupe()),
                            use_op: VirtualUseOp::Frame(
                                Arc::new(FrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(use_op.dupe()),
                            ),
                        })),
                    )?;
                    let arr_no_dro = Type::new(TypeInner::DefT(
                        reason.dupe(),
                        DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                            ArrayATData {
                                elem_t: elem_t.dupe(),
                                react_dro: None,
                                tuple_view: tuple_view.clone(),
                            },
                        ))))),
                    ));
                    rec_flow(cx, trace, (&arr_no_dro, u))?;
                }
                UseTInner::GetPropT(box data)
                    if let PropRef::Named { name, .. } = data.propref.as_ref()
                        && matches!(
                            name.as_str(),
                            "fill"
                                | "pop"
                                | "push"
                                | "reverse"
                                | "shift"
                                | "sort"
                                | "splice"
                                | "unshift"
                        ) =>
                {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotReadable(Box::new(EPropNotReadableData {
                            reason_prop: data.reason.dupe(),
                            prop_name: Some(name.dupe()),
                            use_op: VirtualUseOp::Frame(
                                Arc::new(FrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(data.use_op.dupe()),
                            ),
                        })),
                    )?;
                    let arr_no_dro = Type::new(TypeInner::DefT(
                        reason.dupe(),
                        DefT::new(DefTInner::ArrT(Rc::new(ArrType::ArrayAT(Box::new(
                            ArrayATData {
                                elem_t: elem_t.dupe(),
                                react_dro: None,
                                tuple_view: tuple_view.clone(),
                            },
                        ))))),
                    ));
                    rec_flow(cx, trace, (&arr_no_dro, u))?;
                }
                _ => {
                    let dro_elem = mk_react_dro(
                        cx,
                        unknown_use(),
                        ReactDro(dro_loc.dupe(), dro_type.clone()),
                        elem_t.dupe(),
                    );
                    let ro_arr = get_builtin_typeapp(
                        cx,
                        reason,
                        Some(true),
                        "$ReadOnlyArray",
                        vec![dro_elem],
                    );
                    let u_mod = type_util::mod_use_op_of_use_t(
                        |use_op: &UseOp| {
                            VirtualUseOp::Frame(
                                Arc::new(FrameUseOp::ReactDeepReadOnly(Box::new((
                                    dro_loc.dupe(),
                                    dro_type.clone(),
                                )))),
                                Arc::new(use_op.dupe()),
                            )
                        },
                        u,
                    );
                    rec_flow(cx, trace, (&ro_arr, &u_mod))?;
                }
            }
        }
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::ArrT(arr) = def_t.deref()
                && let ArrType::ArrayAT(box ArrayATData { elem_t, .. }) = arr.deref()
                && matches!(
                    u.deref(),
                    UseTInner::GetPropT(..)
                        | UseTInner::SetPropT(..)
                        | UseTInner::MethodT(..)
                        | UseTInner::LookupT(..)
                ) =>
        {
            let arr_t = get_builtin_typeapp(cx, reason, None, "Array", vec![elem_t.dupe()]);
            rec_flow(cx, trace, (&arr_t, u))?;
        }
        // *************************
        // * Tuple "length" access *
        // *************************
        (TypeInner::DefT(reason, def_t), UseTInner::GetPropT(box data))
            if let DefTInner::ArrT(arr) = def_t.deref()
                && let ArrType::TupleAT(box TupleATData { arity, inexact, .. }) = arr.deref()
                && matches!(data.propref.as_ref(), PropRef::Named { name, .. } if name == &Name::new("length")) =>
        {
            flow_js_utils::get_prop_t_kit::on_array_length::<FlowJs>(
                cx,
                &trace,
                reason.dupe(),
                *inexact,
                *arity,
                reason_of_use_t(u),
            )?(cx, (*data.tout).dupe())?;
        }
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::ArrT(arr) = def_t.deref()
                && matches!(
                    arr.deref(),
                    ArrType::TupleAT(box TupleATData {
                        react_dro: Some(_),
                        ..
                    }) | ArrType::ROArrayAT(box (_, Some(_)))
                )
                && matches!(
                    u.deref(),
                    UseTInner::GetPropT(..)
                        | UseTInner::SetPropT(..)
                        | UseTInner::MethodT(..)
                        | UseTInner::LookupT(..)
                ) =>
        {
            let (elem_t, dro) = match arr.deref() {
                ArrType::TupleAT(box TupleATData {
                    elem_t,
                    react_dro: Some(dro),
                    ..
                }) => (elem_t, dro),
                ArrType::ROArrayAT(box (elem_t, Some(dro))) => (elem_t, dro),
                _ => unreachable!(),
            };
            let ReactDro(dro_loc, dro_type) = dro;
            let u_mod = type_util::mod_use_op_of_use_t(
                |use_op: &UseOp| {
                    VirtualUseOp::Frame(
                        Arc::new(FrameUseOp::ReactDeepReadOnly(Box::new((
                            dro_loc.dupe(),
                            dro_type.clone(),
                        )))),
                        Arc::new(use_op.dupe()),
                    )
                },
                u,
            );
            let dro_elem = mk_react_dro(
                cx,
                unknown_use(),
                ReactDro(dro_loc.dupe(), dro_type.clone()),
                elem_t.dupe(),
            );
            let ro_arr =
                get_builtin_typeapp(cx, reason, Some(true), "$ReadOnlyArray", vec![dro_elem]);
            rec_flow(cx, trace, (&ro_arr, &u_mod))?;
        }
        (TypeInner::DefT(reason, def_t), _)
            if let DefTInner::ArrT(arr) = def_t.deref()
                && matches!(
                    arr.deref(),
                    ArrType::TupleAT(box TupleATData { .. }) | ArrType::ROArrayAT(box (_, _))
                )
                && matches!(
                    u.deref(),
                    UseTInner::GetPropT(..)
                        | UseTInner::SetPropT(..)
                        | UseTInner::MethodT(..)
                        | UseTInner::LookupT(..)
                ) =>
        {
            let t = elemt_of_arrtype(arr);
            let ro_arr = get_builtin_typeapp(cx, reason, None, "$ReadOnlyArray", vec![t]);
            rec_flow(cx, trace, (&ro_arr, u))?;
        }
        // ***********************
        // * String library call *
        // ***********************
        (TypeInner::DefT(reason, def_t), _)
            if matches!(
                def_t.deref(),
                DefTInner::StrGeneralT(_) | DefTInner::SingletonStrT { .. }
            ) && primitive_promoting_use_t(u) =>
        {
            let string_t = helpers::get_builtin_type(cx, Some(trace), reason, None, "String")?;
            rec_flow(cx, trace, (&string_t, u))?;
        }
        // ***********************
        // * Number library call *
        // ***********************
        (TypeInner::DefT(reason, def_t), _)
            if matches!(
                def_t.deref(),
                DefTInner::NumGeneralT(_) | DefTInner::SingletonNumT { .. }
            ) && primitive_promoting_use_t(u) =>
        {
            let number_t = helpers::get_builtin_type(cx, Some(trace), reason, None, "Number")?;
            rec_flow(cx, trace, (&number_t, u))?;
        }
        // ************************
        // * Boolean library call *
        // ************************
        (TypeInner::DefT(reason, def_t), _)
            if matches!(
                def_t.deref(),
                DefTInner::BoolGeneralT | DefTInner::SingletonBoolT { .. }
            ) && primitive_promoting_use_t(u) =>
        {
            let bool_t = helpers::get_builtin_type(cx, Some(trace), reason, None, "Boolean")?;
            rec_flow(cx, trace, (&bool_t, u))?;
        }
        // ***********************
        // * BigInt library call *
        // ***********************
        (TypeInner::DefT(reason, def_t), _)
            if matches!(
                def_t.deref(),
                DefTInner::BigIntGeneralT(_) | DefTInner::SingletonBigIntT { .. }
            ) && primitive_promoting_use_t(u) =>
        {
            let bigint_t = helpers::get_builtin_type(cx, Some(trace), reason, None, "BigInt")?;
            rec_flow(cx, trace, (&bigint_t, u))?;
        }
        // ***********************
        // * Symbol library call *
        // ***********************
        (TypeInner::DefT(reason, def_t), _)
            if matches!(
                def_t.deref(),
                DefTInner::SymbolT | DefTInner::UniqueSymbolT(_)
            ) && primitive_promoting_use_t(u) =>
        {
            let symbol_t = helpers::get_builtin_type(cx, Some(trace), reason, None, "Symbol")?;
            rec_flow(cx, trace, (&symbol_t, u))?;
        }
        // *****************************************************
        // * Nice error messages for mixed function refinement *
        // *****************************************************
        (TypeInner::DefT(lreason, def_t), _)
            if matches!(def_t.deref(), DefTInner::MixedT(MixedFlavor::MixedFunction))
                && matches!(
                    u.deref(),
                    UseTInner::MethodT(..)
                        | UseTInner::SetPropT(..)
                        | UseTInner::GetPropT(..)
                        | UseTInner::LookupT(..)
                ) =>
        {
            let fun_proto = Type::new(TypeInner::FunProtoT(lreason.dupe()));
            rec_flow(cx, trace, (&fun_proto, u))?;
        }
        (
            TypeInner::DefT(_, def_t),
            UseTInner::CallT(box CallTData {
                call_action: box CallAction::ConcretizeCallee(tout),
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::MixedT(MixedFlavor::MixedFunction)) => {
            let open_tout = Type::new(TypeInner::OpenT(tout.dupe()));
            rec_flow_t(cx, trace, unknown_use(), (l, &open_tout))?;
        }
        (
            TypeInner::DefT(lreason, def_t),
            UseTInner::CallT(box CallTData {
                use_op,
                reason: ureason,
                ..
            }),
        ) if matches!(def_t.deref(), DefTInner::MixedT(MixedFlavor::MixedFunction)) => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatible(Box::new(EIncompatibleData {
                    lower: (lreason.dupe(), None),
                    upper: (ureason.dupe(), UpperKind::IncompatibleMixedCallT),
                    use_op: Some(use_op.dupe()),
                })),
            )?;
            let any = any_t::make(AnySource::AnyError(None), lreason.dupe());
            rec_flow(cx, trace, (&any, u))?;
        }
        // Special cases of FunT
        (
            TypeInner::FunProtoBindT(reason),
            UseTInner::MethodT(box MethodTData {
                use_op,
                reason: call_r,
                prop_reason: lookup_r,
                propref,
                method_action: action,
            }),
        ) => {
            let method_type = flow_typing_tvar::mk_no_wrap_where(
                cx,
                lookup_r.dupe(),
                |cx, tout_reason, tout_id| {
                    let tout = Tvar::new(tout_reason.dupe(), tout_id as u32);
                    let get_prop_u = UseT::new(UseTInner::GetPropT(Box::new(GetPropTData {
                        use_op: use_op.dupe(),
                        reason: lookup_r.dupe(),
                        id: None,
                        from_annot: false,
                        skip_optional: false,
                        propref: propref.clone(),
                        tout: Box::new(tout),
                        hint: hint_unavailable(),
                    })));
                    let fun_proto = Type::new(TypeInner::FunProtoT(reason.dupe()));
                    rec_flow(cx, trace, (&fun_proto, &get_prop_u))
                },
            )?;
            apply_method_action(
                cx,
                trace,
                &method_type,
                use_op.dupe(),
                call_r.dupe(),
                l.dupe(),
                action,
            )?;
        }
        (TypeInner::FunProtoBindT(reason), _) => {
            let fun_proto = Type::new(TypeInner::FunProtoT(reason.dupe()));
            rec_flow(cx, trace, (&fun_proto, u))?;
        }
        (
            _,
            UseTInner::LookupT(box LookupTData {
                propref,
                lookup_action,
                ..
            }),
        ) => {
            default_resolve::default_resolve_touts(
                &|l_inner: Type, r_inner: Type| {
                    rec_flow_t(cx, trace, unknown_use(), (&l_inner, &r_inner))?;
                    Ok(())
                },
                None,
                cx,
                reason_of_t(l).loc().dupe(),
                u,
            )?;
            let use_op = Some(use_op_of_lookup_action(lookup_action));
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatibleProp(Box::new(EIncompatiblePropData {
                    prop: name_of_propref(propref),
                    reason_prop: reason_of_propref(propref).dupe(),
                    reason_obj: reason_of_t(l).dupe(),
                    special: flow_js_utils::error_message_kind_of_lower(l),
                    use_op,
                })),
            )?;
        }
        (TypeInner::DefT(_, def_t), UseTInner::CheckUnusedPromiseT { reason, async_ })
            if let DefTInner::InstanceT(inst_t) = def_t.deref() =>
        {
            let class_id = &inst_t.inst.class_id;
            let super_ = &inst_t.super_;
            match flow_js_utils::builtin_promise_class_id(cx) {
                None => {}
                Some(promise_class_id) => {
                    if promise_class_id == *class_id {
                        flow_js_utils::add_output(
                            cx,
                            ErrorMessage::EUnusedPromise {
                                loc: reason.loc().dupe(),
                                async_: *async_,
                            },
                        )?;
                    } else {
                        rec_flow(
                            cx,
                            trace,
                            (
                                super_,
                                &UseT::new(UseTInner::CheckUnusedPromiseT {
                                    reason: reason.dupe(),
                                    async_: *async_,
                                }),
                            ),
                        )?;
                    }
                }
            }
        }
        (_, UseTInner::CheckUnusedPromiseT { .. }) => {}
        (
            _,
            UseTInner::ConcretizeT(box ConcretizeTData {
                reason: _,
                kind: ConcretizationKind::ConcretizeAll,
                seen: _,
                collector,
            }),
        ) => {
            collector.add(l.dupe());
        }
        _ => {
            flow_js_utils::add_output(
                cx,
                ErrorMessage::EIncompatible(Box::new(EIncompatibleData {
                    lower: (
                        reason_of_t(l).dupe(),
                        flow_js_utils::error_message_kind_of_lower(l),
                    ),
                    upper: (
                        reason_of_use_t(u).dupe(),
                        flow_js_utils::error_message_kind_of_upper(u),
                    ),
                    use_op: use_op_of_use_t(u),
                })),
            )?;
            let resolve_callee = match u.deref() {
                UseTInner::CallT(..) => Some((reason_of_t(l).dupe(), vec![l.dupe()])),
                _ => None,
            };
            default_resolve::default_resolve_touts(
                &|l_inner: Type, r_inner: Type| {
                    rec_flow_t(cx, trace, unknown_use(), (&l_inner, &r_inner))?;
                    Ok(())
                },
                resolve_callee.as_ref(),
                cx,
                reason_of_t(l).loc().dupe(),
                u,
            )?;
        }
    }
    Ok(None)
}
