/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::IterDupedExt;
use flow_common::polarity::Polarity;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::ReasonDesc;
use flow_common::reason::VirtualReasonDesc;
use flow_common::reason::mk_reason;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_errors::error_message::EPropNotReadableData;
use flow_typing_errors::error_message::ETupleElementNotReadableData;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_common::obj_type;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_type::type_::ArrType;
use flow_typing_type::type_::BigIntLiteral;
use flow_typing_type::type_::BinaryTest;
use flow_typing_type::type_::CallArg;
use flow_typing_type::type_::DefT;
use flow_typing_type::type_::DefTInner;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::FunParam;
use flow_typing_type::type_::GenericTData;
use flow_typing_type::type_::InstanceKind;
use flow_typing_type::type_::NominalType;
use flow_typing_type::type_::NominalTypeInner;
use flow_typing_type::type_::NumberLiteral;
use flow_typing_type::type_::PolyTData;
use flow_typing_type::type_::Predicate;
use flow_typing_type::type_::PredicateConcretetizerVariant;
use flow_typing_type::type_::PredicateInner;
use flow_typing_type::type_::PropertyInner;
use flow_typing_type::type_::Targ;
use flow_typing_type::type_::TupleATData;
use flow_typing_type::type_::TupleElement;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeGuardInner;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::UnionEnum;
use flow_typing_type::type_::UnionEnumStar;
use flow_typing_type::type_::UseOp;
use flow_typing_type::type_::elemt_of_arrtype;
use flow_typing_type::type_::hint_unavailable;
use flow_typing_type::type_::inter_rep;
use flow_typing_type::type_::mk_functioncalltype;
use flow_typing_type::type_::nominal;
use flow_typing_type::type_::properties;
use flow_typing_type::type_::type_collector::TypeCollector;
use flow_typing_type::type_::union_rep;
use flow_typing_type::type_::union_rep::QuickMemResult;
use flow_typing_type::type_::unknown_use;
use flow_typing_type::type_util;
use flow_typing_type::type_util::desc_of_t;
use flow_typing_type::type_util::loc_of_t;
use flow_typing_type::type_util::reason_of_t;
use vec1::Vec1;

use crate::speculation_flow;
use crate::type_filter;
use crate::type_filter::FilterResult;

#[derive(Clone)]
enum InstanceofRhs {
    TypeOperand(Type),
    InternalExtendsOperand(Reason, Type, Type),
}

#[derive(Clone, Copy)]
enum PropGuard {
    PropGuardTruthy,
    PropGuardNotTruthy,
    PropGuardMaybe,
    PropGuardNotMaybe,
    PropGuardNull,
    PropGuardNotNull,
    PropGuardVoid,
    PropGuardNotVoid,
}

fn concretization_variant_of_predicate(p: &Predicate) -> PredicateConcretetizerVariant {
    match p.deref() {
        PredicateInner::MaybeP | PredicateInner::TruthyP | PredicateInner::LatentP(..) => {
            PredicateConcretetizerVariant::ConcretizeKeepOptimizedUnions
        }
        PredicateInner::NotP(inner) => match inner.deref() {
            PredicateInner::MaybeP | PredicateInner::TruthyP | PredicateInner::LatentP(..) => {
                PredicateConcretetizerVariant::ConcretizeKeepOptimizedUnions
            }
            _ => PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
        },
        _ => PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
    }
}

struct PredicateResultCollector {
    collector: TypeCollector,
    changed: Rc<RefCell<bool>>,
}

// Simple wrapper to protect against using non-concretized types in type-guard
// filtering operations (see `intersect` below).
struct ConcretizedType(Type);

impl ConcretizedType {
    fn unwrap(&self) -> &Type {
        &self.0
    }

    fn for_all_concrete_ts<'cx>(
        cx: &Context<'cx>,
        t: &Type,
        f: &dyn Fn(&ConcretizedType) -> bool,
    ) -> Result<bool, FlowJsException> {
        let ts = FlowJs::possible_concrete_types_for_inspection(cx, reason_of_t(t), t)?;
        Ok(ts.iter().all(|t| f(&ConcretizedType(t.dupe()))))
    }

    fn wrap_unsafe(t: Type) -> ConcretizedType {
        ConcretizedType(t)
    }
}

fn report_changes_to_input(result_collector: &PredicateResultCollector) {
    *result_collector.changed.borrow_mut() = true;
}

fn report_filtering_result_to_predicate_result(
    filter_result: FilterResult,
    result_collector: &PredicateResultCollector,
) {
    let FilterResult { type_, changed } = filter_result;
    result_collector.collector.add(type_);
    let prev = *result_collector.changed.borrow();
    *result_collector.changed.borrow_mut() = prev || changed;
}

fn report_unchanged_filtering_result_to_predicate_result(
    type_: Type,
    result_collector: &PredicateResultCollector,
) {
    report_filtering_result_to_predicate_result(
        FilterResult {
            type_,
            changed: false,
        },
        result_collector,
    );
}

fn report_changed_filtering_result_to_predicate_result(
    type_: Type,
    result_collector: &PredicateResultCollector,
) {
    report_filtering_result_to_predicate_result(
        FilterResult {
            type_,
            changed: true,
        },
        result_collector,
    );
}

fn concretize_and_run_predicate<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    l: &Type,
    variant: PredicateConcretetizerVariant,
    result_collector: &PredicateResultCollector,
    predicate_no_concretization: &dyn Fn(
        &Context<'cx>,
        DepthTrace,
        &PredicateResultCollector,
        &Type,
    ) -> Result<(), FlowJsException>,
) -> Result<(), FlowJsException> {
    let reason = reason_of_t(l);
    let ts = FlowJs::possible_concrete_types_for_predicate(variant, cx, reason, l)?;
    for t in ts.iter() {
        match t.deref() {
            TypeInner::GenericT(box GenericTData {
                bound,
                name,
                reason,
                id,
                no_infer,
            }) => {
                let bound_type_collector = TypeCollector::create();
                let changed = Rc::new(RefCell::new(false));
                let bound_result_collector = PredicateResultCollector {
                    collector: bound_type_collector,
                    changed: changed.dupe(),
                };
                let repositioned_bound =
                    FlowJs::reposition_reason(cx, Some(trace), reason, None, bound)?;
                concretize_and_run_predicate(
                    cx,
                    trace,
                    &repositioned_bound,
                    variant.clone(),
                    &bound_result_collector,
                    predicate_no_concretization,
                )?;
                let changed_val = *changed.borrow();
                if changed_val {
                    report_changes_to_input(result_collector);
                }
                let name = name.dupe();
                let no_infer = *no_infer;
                let id = id.clone();
                bound_result_collector.collector.iter(|bound| {
                    let type_ = Type::new(TypeInner::GenericT(Box::new(GenericTData {
                        reason: reason_of_t(bound).dupe(),
                        name: name.dupe(),
                        bound: bound.dupe(),
                        no_infer,
                        id: id.clone(),
                    })));
                    report_filtering_result_to_predicate_result(
                        FilterResult {
                            type_,
                            changed: changed_val,
                        },
                        result_collector,
                    );
                });
            }
            _ => {
                predicate_no_concretization(cx, trace, result_collector, t)?;
            }
        }
    }
    Ok(())
}

fn concretize_binary_rhs_and_run_binary_predicate<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    l: &Type,
    r: &Type,
    sense: bool,
    b: &BinaryTest,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    let reason = reason_of_t(r);
    let variant = match b {
        BinaryTest::InstanceofTest => {
            PredicateConcretetizerVariant::ConcretizeRHSForInstanceOfPredicateTest
        }
        BinaryTest::SentinelProp(_) => {
            PredicateConcretetizerVariant::ConcretizeRHSForLiteralPredicateTest
        }
        BinaryTest::EqTest => PredicateConcretetizerVariant::ConcretizeRHSForLiteralPredicateTest,
    };
    let ts = FlowJs::possible_concrete_types_for_predicate(variant, cx, reason, r)?;
    for r in ts.iter() {
        binary_predicate(cx, trace, sense, b, l, r, result_collector)?;
    }
    Ok(())
}

// t - predicate output recipient (normally a tvar)
// l - incoming concrete LB (predicate input)
// result - guard result in case of success
// p - predicate
fn predicate_no_concretization<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    result_collector: &PredicateResultCollector,
    l: &Type,
    p: &Predicate,
) -> Result<(), FlowJsException> {
    match p.deref() {
        // (************************)
        // (* deconstruction of && *)
        // (************************)
        PredicateInner::AndP(p1, p2) => {
            let intermediate_type_collector = TypeCollector::create();
            let changed = Rc::new(RefCell::new(false));
            let intermediate_result_collector = PredicateResultCollector {
                collector: intermediate_type_collector,
                changed: changed.dupe(),
            };
            let p1_clone = p1.dupe();
            concretize_and_run_predicate(
                cx,
                trace,
                l,
                concretization_variant_of_predicate(p1),
                &intermediate_result_collector,
                &|cx, trace, rc, l| predicate_no_concretization(cx, trace, rc, l, &p1_clone),
            )?;
            if *changed.borrow() {
                report_changes_to_input(result_collector);
            }
            let collected = intermediate_result_collector.collector.collect();
            let p2_clone = p2.dupe();
            for t in collected.iter() {
                concretize_and_run_predicate(
                    cx,
                    trace,
                    t,
                    concretization_variant_of_predicate(p2),
                    result_collector,
                    &|cx, trace, rc, l| predicate_no_concretization(cx, trace, rc, l, &p2_clone),
                )?;
            }
            Ok(())
        }
        // (************************)
        // (* deconstruction of || *)
        // (************************)
        PredicateInner::OrP(p1, p2) => {
            let p1_clone = p1.dupe();
            concretize_and_run_predicate(
                cx,
                trace,
                l,
                concretization_variant_of_predicate(p1),
                result_collector,
                &|cx, trace, rc, l| predicate_no_concretization(cx, trace, rc, l, &p1_clone),
            )?;
            let p2_clone = p2.dupe();
            concretize_and_run_predicate(
                cx,
                trace,
                l,
                concretization_variant_of_predicate(p2),
                result_collector,
                &|cx, trace, rc, l| predicate_no_concretization(cx, trace, rc, l, &p2_clone),
            )?;
            Ok(())
        }
        // (*********************************)
        // (* deconstruction of binary test *)
        // (*********************************)
        PredicateInner::BinaryP(b, r) => concretize_binary_rhs_and_run_binary_predicate(
            cx,
            trace,
            l,
            r,
            true,
            b,
            result_collector,
        ),
        PredicateInner::NotP(inner) => {
            match inner.deref() {
                PredicateInner::BinaryP(b, r) => concretize_binary_rhs_and_run_binary_predicate(
                    cx,
                    trace,
                    l,
                    r,
                    false,
                    b,
                    result_collector,
                ),
                // (***********************)
                // (* typeof _ ~ "boolean" *)
                // (***********************)
                PredicateInner::BoolP(_) => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_boolean(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (***********************)
                // (* typeof _ ~ "string" *)
                // (***********************)
                PredicateInner::StrP(_) => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_string(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (***********************)
                // (* typeof _ ~ "symbol" *)
                // (***********************)
                PredicateInner::SymbolP(_) => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_symbol(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (*********************)
                // (* _ ~ "some string" *)
                // (*********************)
                PredicateInner::SingletonStrP(box (_, _, lit)) => {
                    let filtered_str =
                        type_filter::not_string_literal(Name::new(lit.as_str()), l.dupe());
                    report_filtering_result_to_predicate_result(filtered_str, result_collector);
                    Ok(())
                }
                // (*********************)
                // (* _ ~ some number n *)
                // (*********************)
                PredicateInner::SingletonNumP(box (_, _, lit)) => {
                    let filtered_num = type_filter::not_number_literal(lit.clone(), l.dupe());
                    report_filtering_result_to_predicate_result(filtered_num, result_collector);
                    Ok(())
                }
                // (***********************)
                // (* typeof _ ~ "number" *)
                // (***********************)
                PredicateInner::NumP(_) => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_number(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (*********************)
                // (* _ ~ some bigint n *)
                // (*********************)
                PredicateInner::SingletonBigIntP(box (_, _, lit)) => {
                    let filtered_bigint = type_filter::not_bigint_literal(lit.clone(), l.dupe());
                    report_filtering_result_to_predicate_result(filtered_bigint, result_collector);
                    Ok(())
                }
                // (***********************)
                // (* typeof _ ~ "bigint" *)
                // (***********************)
                PredicateInner::BigIntP(_) => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_bigint(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (***********************)
                // (* typeof _ ~ "function" *)
                // (***********************)
                PredicateInner::FunP => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_function(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (***********************)
                // (* typeof _ ~ "object" *)
                // (***********************)
                PredicateInner::ObjP => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_object(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (*******************)
                // (* Array.isArray _ *)
                // (*******************)
                PredicateInner::ArrP => {
                    report_filtering_result_to_predicate_result(
                        type_filter::not_array(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (*******************)
                // (* array length *)
                // (*******************)
                PredicateInner::ArrLenP { op, n } => {
                    report_filtering_result_to_predicate_result(
                        type_filter::array_length(false, op, *n, l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
                // (***********************)
                // (* typeof _ ~ "undefined" *)
                // (***********************)
                PredicateInner::VoidP => {
                    let filtered = type_filter::not_undefined(cx, l.dupe());
                    report_filtering_result_to_predicate_result(filtered, result_collector);
                    Ok(())
                }
                // (********)
                // (* null *)
                // (********)
                PredicateInner::NullP => {
                    let filtered = type_filter::not_null(cx, l.dupe());
                    report_filtering_result_to_predicate_result(filtered, result_collector);
                    Ok(())
                }
                // (*********)
                // (* maybe *)
                // (*********)
                PredicateInner::MaybeP => {
                    let filtered = type_filter::not_maybe(cx, l.dupe());
                    report_filtering_result_to_predicate_result(filtered, result_collector);
                    Ok(())
                }
                // (********)
                // (* true *)
                // (********)
                PredicateInner::SingletonBoolP(box (_, true)) => {
                    let filtered = type_filter::not_true(l.dupe());
                    report_filtering_result_to_predicate_result(filtered, result_collector);
                    Ok(())
                }
                // (*********)
                // (* false *)
                // (*********)
                PredicateInner::SingletonBoolP(box (_, false)) => {
                    let filtered = type_filter::not_false(l.dupe());
                    report_filtering_result_to_predicate_result(filtered, result_collector);
                    Ok(())
                }
                // (************************)
                // (* truthyness *)
                // (************************)
                PredicateInner::TruthyP => {
                    let filtered = type_filter::not_truthy(cx, l.dupe());
                    report_filtering_result_to_predicate_result(filtered, result_collector);
                    Ok(())
                }
                PredicateInner::PropExistsP {
                    propname,
                    reason: _,
                } => prop_exists_test(cx, propname, false, l, result_collector),
                PredicateInner::PropTruthyP(key, r) => {
                    prop_truthy_test(cx, key, r, false, l, result_collector)
                }
                PredicateInner::PropNonMaybeP(key, r) => {
                    prop_non_maybe_test(cx, key, r, false, l, result_collector)
                }
                PredicateInner::PropIsExactlyNullP(key, r) => {
                    prop_is_exactly_null_test(cx, key, r, false, l, result_collector)
                }
                PredicateInner::PropNonVoidP(key, r) => {
                    prop_non_void_test(cx, key, r, false, l, result_collector)
                }
                // classical logic i guess
                PredicateInner::NotP(inner_p) => {
                    predicate_no_concretization(cx, trace, result_collector, l, inner_p)
                }
                PredicateInner::AndP(p1, p2) => {
                    let de_morgan = Predicate::new(PredicateInner::OrP(
                        Predicate::new(PredicateInner::NotP(p1.dupe())),
                        Predicate::new(PredicateInner::NotP(p2.dupe())),
                    ));
                    predicate_no_concretization(cx, trace, result_collector, l, &de_morgan)
                }
                PredicateInner::OrP(p1, p2) => {
                    let de_morgan = Predicate::new(PredicateInner::AndP(
                        Predicate::new(PredicateInner::NotP(p1.dupe())),
                        Predicate::new(PredicateInner::NotP(p2.dupe())),
                    ));
                    predicate_no_concretization(cx, trace, result_collector, l, &de_morgan)
                }
                // (********************)
                // (* Latent predicate *)
                // (********************)
                PredicateInner::LatentP(pred_funcall_info, idx) => {
                    let fun_t = &pred_funcall_info.2;
                    let loc = pred_funcall_info.1.dupe();
                    let use_op = pred_funcall_info.0.dupe();
                    let targs = &pred_funcall_info.3;
                    let argts = &pred_funcall_info.4;
                    let reason = mk_reason(
                        VirtualReasonDesc::RFunctionCall(Arc::new(desc_of_t(fun_t).clone())),
                        loc,
                    );
                    call_latent_param_pred(
                        cx,
                        trace,
                        fun_t,
                        &use_op,
                        &reason,
                        targs,
                        argts,
                        false,
                        idx,
                        l,
                        result_collector,
                    )
                }
                PredicateInner::LatentThisP(pred_funcall_info) => {
                    let fun_t = &pred_funcall_info.2;
                    let loc = pred_funcall_info.1.dupe();
                    let use_op = pred_funcall_info.0.dupe();
                    let targs = &pred_funcall_info.3;
                    let argts = &pred_funcall_info.4;
                    let reason = mk_reason(
                        VirtualReasonDesc::RFunctionCall(Arc::new(desc_of_t(fun_t).clone())),
                        loc,
                    );
                    call_latent_this_pred(
                        cx,
                        trace,
                        fun_t,
                        &use_op,
                        &reason,
                        targs,
                        argts,
                        false,
                        l,
                        result_collector,
                    )
                }
                // (**************)
                // (* Impossible *)
                // (**************)
                PredicateInner::ImpossibleP => {
                    report_filtering_result_to_predicate_result(
                        type_filter::unchanged_result(l.dupe()),
                        result_collector,
                    );
                    Ok(())
                }
            }
        }
        // (***********************)
        // (* typeof _ ~ "boolean" *)
        // (***********************)
        PredicateInner::BoolP(box loc) => {
            report_filtering_result_to_predicate_result(
                type_filter::boolean(loc.dupe(), l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (***********************)
        // (* typeof _ ~ "string" *)
        // (***********************)
        PredicateInner::StrP(box loc) => {
            report_filtering_result_to_predicate_result(
                type_filter::string(loc.dupe(), l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (***********************)
        // (* typeof _ ~ "symbol" *)
        // (***********************)
        PredicateInner::SymbolP(box loc) => {
            report_filtering_result_to_predicate_result(
                type_filter::symbol(loc.dupe(), l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (*********************)
        // (* _ ~ "some string" *)
        // (*********************)
        PredicateInner::SingletonStrP(box (expected_loc, _sense, lit)) => {
            let filtered_str =
                type_filter::string_literal(expected_loc.dupe(), Name::new(lit.as_str()), l.dupe());
            report_filtering_result_to_predicate_result(filtered_str, result_collector);
            Ok(())
        }
        // (*********************)
        // (* _ ~ some number n *)
        // (*********************)
        PredicateInner::SingletonNumP(box (expected_loc, _sense, lit)) => {
            let filtered_num =
                type_filter::number_literal(expected_loc.dupe(), lit.clone(), l.dupe());
            report_filtering_result_to_predicate_result(filtered_num, result_collector);
            Ok(())
        }
        // (***********************)
        // (* typeof _ ~ "number" *)
        // (***********************)
        PredicateInner::NumP(box loc) => {
            //   report_filtering_result_to_predicate_result (Type_filter.number loc l) result_collector
            report_filtering_result_to_predicate_result(
                type_filter::number(loc.dupe(), l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (*********************)
        // (* _ ~ some bigint n *)
        // (*********************)
        PredicateInner::SingletonBigIntP(box (expected_loc, _sense, lit)) => {
            //   let filtered_bigint = Type_filter.bigint_literal expected_loc lit l in
            let filtered_bigint =
                type_filter::bigint_literal(expected_loc.dupe(), lit.clone(), l.dupe());
            //   report_filtering_result_to_predicate_result filtered_bigint result_collector
            report_filtering_result_to_predicate_result(filtered_bigint, result_collector);
            Ok(())
        }
        // (***********************)
        // (* typeof _ ~ "bigint" *)
        // (***********************)
        PredicateInner::BigIntP(box loc) => {
            report_filtering_result_to_predicate_result(
                type_filter::bigint(loc.dupe(), l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (***********************)
        // (* typeof _ ~ "function" *)
        // (***********************)
        PredicateInner::FunP => {
            report_filtering_result_to_predicate_result(
                type_filter::function_(l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (***********************)
        // (* typeof _ ~ "object" *)
        // (***********************)
        PredicateInner::ObjP => {
            report_filtering_result_to_predicate_result(
                type_filter::object_(cx, l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (*******************)
        // (* Array.isArray _ *)
        // (*******************)
        PredicateInner::ArrP => {
            report_filtering_result_to_predicate_result(
                type_filter::array(l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (*******************)
        // (* array length *)
        // (*******************)
        PredicateInner::ArrLenP { op, n } => {
            report_filtering_result_to_predicate_result(
                type_filter::array_length(true, op, *n, l.dupe()),
                result_collector,
            );
            Ok(())
        }
        // (***********************)
        // (* typeof _ ~ "undefined" *)
        // (***********************)
        PredicateInner::VoidP => {
            let filtered = type_filter::undefined(l.dupe());
            report_filtering_result_to_predicate_result(filtered, result_collector);
            Ok(())
        }
        // (********)
        // (* null *)
        // (********)
        PredicateInner::NullP => {
            let filtered = type_filter::null_filter(l.dupe());
            report_filtering_result_to_predicate_result(filtered, result_collector);
            Ok(())
        }
        // (*********)
        // (* maybe *)
        // (*********)
        PredicateInner::MaybeP => {
            let filtered = type_filter::maybe(cx, l.dupe());
            report_filtering_result_to_predicate_result(filtered, result_collector);
            Ok(())
        }
        // (********)
        // (* true *)
        // (********)
        PredicateInner::SingletonBoolP(box (_, true)) => {
            let filtered = type_filter::true_(l.dupe());
            report_filtering_result_to_predicate_result(filtered, result_collector);
            Ok(())
        }
        // (*********)
        // (* false *)
        // (*********)
        PredicateInner::SingletonBoolP(box (_, false)) => {
            let filtered = type_filter::false_(l.dupe());
            report_filtering_result_to_predicate_result(filtered, result_collector);
            Ok(())
        }
        // (************************)
        // (* truthyness *)
        // (************************)
        PredicateInner::TruthyP => {
            let filtered = type_filter::truthy(cx, l.dupe());
            report_filtering_result_to_predicate_result(filtered, result_collector);
            Ok(())
        }
        PredicateInner::PropExistsP {
            propname,
            reason: _,
        } => prop_exists_test(cx, propname, true, l, result_collector),
        PredicateInner::PropTruthyP(key, r) => {
            prop_truthy_test(cx, key, r, true, l, result_collector)
        }
        PredicateInner::PropNonMaybeP(key, r) => {
            prop_non_maybe_test(cx, key, r, true, l, result_collector)
        }
        PredicateInner::PropIsExactlyNullP(key, r) => {
            prop_is_exactly_null_test(cx, key, r, true, l, result_collector)
        }
        PredicateInner::PropNonVoidP(key, r) => {
            prop_non_void_test(cx, key, r, true, l, result_collector)
        }
        // (********************)
        // (* Latent predicate *)
        // (********************)
        PredicateInner::LatentP(pred_funcall_info, idx) => {
            let fun_t = &pred_funcall_info.2;
            let loc = pred_funcall_info.1.dupe();
            let use_op = pred_funcall_info.0.dupe();
            let targs = &pred_funcall_info.3;
            let argts = &pred_funcall_info.4;
            let reason = mk_reason(
                VirtualReasonDesc::RFunctionCall(Arc::new(desc_of_t(fun_t).clone())),
                loc,
            );
            call_latent_param_pred(
                cx,
                trace,
                fun_t,
                &use_op,
                &reason,
                targs,
                argts,
                true,
                idx,
                l,
                result_collector,
            )
        }
        PredicateInner::LatentThisP(pred_funcall_info) => {
            let fun_t = &pred_funcall_info.2;
            let loc = pred_funcall_info.1.dupe();
            let use_op = pred_funcall_info.0.dupe();
            let targs = &pred_funcall_info.3;
            let argts = &pred_funcall_info.4;
            let reason = mk_reason(
                VirtualReasonDesc::RFunctionCall(Arc::new(desc_of_t(fun_t).clone())),
                loc,
            );
            call_latent_this_pred(
                cx,
                trace,
                fun_t,
                &use_op,
                &reason,
                targs,
                argts,
                true,
                l,
                result_collector,
            )
        }
        // (**************)
        // (* Impossible *)
        // (**************)
        PredicateInner::ImpossibleP => {
            report_filtering_result_to_predicate_result(
                type_filter::empty(l.dupe()),
                result_collector,
            );
            Ok(())
        }
    }
}

/// call_latent_pred connects a predicate function with information available
/// at a call-site appearing in a conditional position (e.g. `if (pred(x))`).
/// [tin] is the incoming type of `x` and [tout] the refined result in the then-
/// branch. Since at the time of processing the call we do not know yet the
/// function's formal parameters, [idx] is the index of the argument that gets
/// refined.
fn call_latent_pred<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    fun_t: &Type,
    use_op: &UseOp,
    reason: &Reason,
    targs: &Option<Rc<[Targ]>>,
    argts: &[CallArg],
    sense: bool,
    is_target: &dyn Fn(&str, &[FunParam]) -> bool,
    tin: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    let ts = FlowJs::possible_concrete_types_for_inspection(cx, reason_of_t(fun_t), fun_t)?;
    for t in ts.iter() {
        match t.deref() {
            TypeInner::IntersectionT(r, rep) => {
                let r = r.dupe();
                let members: Vec<Type> = rep.members_iter().duped().collect();
                let use_op_c = use_op.dupe();
                let reason_c = reason.dupe();
                let targs_c = targs.dupe();
                let argts_c = argts.to_vec();
                let tin_c = tin.dupe();
                let cases: Vec<Box<dyn FnOnce(&Context<'cx>) -> Result<(), FlowJsException> + '_>> =
                    members
                        .into_iter()
                        .map(|member_t| {
                            let use_op_inner = use_op_c.dupe();
                            let reason_inner = reason_c.dupe();
                            let targs_inner = targs_c.dupe();
                            let argts_inner = argts_c.clone();
                            let tin_inner = tin_c.dupe();
                            Box::new(move |_cx: &Context<'cx>| {
                                call_latent_pred(
                                    cx,
                                    trace,
                                    &member_t,
                                    &use_op_inner,
                                    &reason_inner,
                                    &targs_inner,
                                    &argts_inner,
                                    sense,
                                    is_target,
                                    &tin_inner,
                                    result_collector,
                                )
                            })
                                as Box<
                                    dyn FnOnce(&Context<'cx>) -> Result<(), FlowJsException> + '_,
                                >
                        })
                        .collect();
                speculation_flow::try_custom(
                    cx,
                    Some(use_op.dupe()),
                    None,
                    None,
                    r.loc().dupe(),
                    cases,
                )?;
            }
            TypeInner::DefT(r, def_t) => {
                match def_t.deref() {
                    // Calls to functions appearing in predicate refinement contexts dispatch
                    // to this case. Here, the callee function type holds the predicate
                    // that will refine the incoming `unrefined_t` and flow a filtered
                    // (refined) version of this type into `fresh_t`.
                    //
                    // Problematic cases (e.g. when the refining index is out of bounds w.r.t.
                    // `params`) raise errors, but also propagate the unrefined types (as if the
                    // refinement never took place).
                    DefTInner::FunT(_, fun_type)
                        if let Some(TypeGuardInner {
                            one_sided,
                            param_name: (_, param_name),
                            type_guard,
                            ..
                        }) = fun_type.type_guard.as_deref() =>
                    {
                        // TODO: for the moment we only support simple keys (empty projection)
                        // that exactly correspond to the function's parameters
                        if is_target(param_name.as_str(), &fun_type.params) {
                            let filter_result = if sense {
                                let repositioned = FlowJs::reposition_reason(
                                    cx,
                                    Some(trace),
                                    reason,
                                    None,
                                    type_guard,
                                )?;
                                let type_ = intersect(cx, tin.dupe(), repositioned);
                                let changed = !Type::ptr_eq(&type_, tin);
                                FilterResult { type_, changed }
                            } else if !one_sided {
                                let repositioned = FlowJs::reposition_reason(
                                    cx,
                                    Some(trace),
                                    reason,
                                    None,
                                    type_guard,
                                )?;
                                type_guard_diff(cx, tin, &repositioned)
                            } else {
                                // Do not refine else branch on one-sided type-guard
                                FilterResult {
                                    type_: tin.dupe(),
                                    changed: false,
                                }
                            };
                            report_filtering_result_to_predicate_result(
                                filter_result,
                                result_collector,
                            );
                        } else {
                            // This is not the refined parameter.
                            report_unchanged_filtering_result_to_predicate_result(
                                tin.dupe(),
                                result_collector,
                            );
                        }
                    }
                    DefTInner::PolyT(box PolyTData {
                        tparams_loc,
                        tparams: ids,
                        t_out: t_out_inner,
                        id: _,
                    }) => {
                        let reason_tapp = r;
                        let fun_t_c = t.dupe();
                        let tvar_id = flow_typing_tvar::mk_no_wrap(cx, reason);
                        let tvar = Tvar::new(reason.dupe(), tvar_id as u32);
                        let calltype = mk_functioncalltype(
                            reason.dupe(),
                            targs.dupe(),
                            Rc::from(argts),
                            true,
                            tvar,
                        );
                        let tparams_loc_c = tparams_loc.dupe();
                        let ids_c = ids.dupe();
                        let t_out_c = t_out_inner.dupe();
                        let reason_c = reason.dupe();
                        let calltype_c = calltype.clone();
                        let check = move || {
                            flow_typing_implicit_instantiation_check::ImplicitInstantiationCheck::of_call(
                                fun_t_c.dupe(),
                                (tparams_loc_c.dupe(), Vec1::try_from_vec(ids_c.to_vec()).unwrap(), t_out_c.dupe()),
                                unknown_use(),
                                reason_c.dupe(),
                                calltype_c.clone(),
                            )
                        };
                        let lparts = (
                            reason_tapp.dupe(),
                            tparams_loc.dupe(),
                            Vec1::try_from_vec(ids.to_vec()).unwrap(),
                            t_out_inner.dupe(),
                        );
                        let uparts = (
                            use_op.dupe(),
                            reason.dupe(),
                            calltype.call_targs.dupe(),
                            hint_unavailable(),
                        );
                        let t_ = FlowJs::instantiate_poly_call_or_new(
                            cx, trace, lparts, uparts, &check,
                        )?;
                        call_latent_pred(
                            cx,
                            trace,
                            &t_,
                            use_op,
                            reason,
                            &None,
                            argts,
                            sense,
                            is_target,
                            tin,
                            result_collector,
                        )?;
                    }
                    // Fall through all the remaining cases
                    _ => {
                        report_unchanged_filtering_result_to_predicate_result(
                            tin.dupe(),
                            result_collector,
                        );
                    }
                }
            }
            // Fall through all the remaining cases
            _ => {
                report_unchanged_filtering_result_to_predicate_result(tin.dupe(), result_collector);
            }
        }
    }
    Ok(())
}

fn call_latent_param_pred<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    fun_t: &Type,
    use_op: &UseOp,
    reason: &Reason,
    targs: &Option<Rc<[Targ]>>,
    argts: &[CallArg],
    sense: bool,
    idx: &[i32],
    tin: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    let idx_vec: Vec<i32> = idx.to_vec();
    call_latent_pred(
        cx,
        trace,
        fun_t,
        use_op,
        reason,
        targs,
        argts,
        sense,
        &|type_guard_name: &str, params: &[FunParam]| {
            idx_vec.iter().any(|&i| match params.get(i as usize) {
                Some(FunParam(Some(name), _)) => name.as_str() == type_guard_name,
                _ => false,
            })
        },
        tin,
        result_collector,
    )
}

fn call_latent_this_pred<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    fun_t: &Type,
    use_op: &UseOp,
    reason: &Reason,
    targs: &Option<Rc<[Targ]>>,
    argts: &[CallArg],
    sense: bool,
    tin: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    call_latent_pred(
        cx,
        trace,
        fun_t,
        use_op,
        reason,
        targs,
        argts,
        sense,
        &|type_guard_name: &str, _params: &[FunParam]| type_guard_name == "this",
        tin,
        result_collector,
    )
}

fn intersect<'cx>(cx: &Context<'cx>, t1: Type, t2: Type) -> Type {
    fn is_any<'cx>(cx: &Context<'cx>, t: &Type) -> bool {
        let ts = FlowJs::possible_concrete_types_for_inspection(cx, reason_of_t(t), t);
        match ts {
            Ok(ts) => ts.iter().any(|t| matches!(t.deref(), TypeInner::AnyT(..))),
            Err(_) => false,
        }
    }

    fn is_null(t: &ConcretizedType) -> bool {
        match t.unwrap().deref() {
            TypeInner::DefT(_, def_t) => matches!(def_t.deref(), DefTInner::NullT),
            _ => false,
        }
    }

    fn is_void(t: &ConcretizedType) -> bool {
        match t.unwrap().deref() {
            TypeInner::DefT(_, def_t) => matches!(def_t.deref(), DefTInner::VoidT),
            _ => false,
        }
    }

    fn tags_of_t<'cx>(
        cx: &Context<'cx>,
        t: &ConcretizedType,
    ) -> Option<crate::type_filter::TypeTagSet> {
        crate::type_filter::tag_of_t(cx, t.unwrap())
    }

    fn tags_differ<'cx>(cx: &Context<'cx>, t1: &ConcretizedType, t2: &ConcretizedType) -> bool {
        match (tags_of_t(cx, t1), tags_of_t(cx, t2)) {
            (Some(ref t1_tags), Some(ref t2_tags)) => {
                !crate::type_filter::tags_overlap(t1_tags, t2_tags)
            }
            _ => false,
        }
    }

    // Singleton primitive types are not captured in the tag overlap checks. We are
    // handling a few cases here explicitly.
    fn ground_types_differ(t1: &ConcretizedType, t2: &ConcretizedType) -> bool {
        match (t1.unwrap().deref(), t2.unwrap().deref()) {
            (TypeInner::DefT(_, d1), TypeInner::DefT(_, d2)) => match (d1.deref(), d2.deref()) {
                (
                    DefTInner::SingletonStrT { value: v1, .. },
                    DefTInner::SingletonStrT { value: v2, .. },
                ) => v1 != v2,
                (
                    DefTInner::SingletonNumT { value: v1, .. },
                    DefTInner::SingletonNumT { value: v2, .. },
                ) => v1 != v2,
                (
                    DefTInner::SingletonBoolT { value: v1, .. },
                    DefTInner::SingletonBoolT { value: v2, .. },
                ) => v1 != v2,
                _ => false,
            },
            _ => false,
        }
    }

    fn type_tags_differ<'cx>(cx: &Context<'cx>, depth: i32, ts1: &[Type], ts2: &[Type]) -> bool {
        match (ts1, ts2) {
            ([t1, rest1 @ ..], [t2, rest2 @ ..]) => {
                let t2_c = t2.dupe();
                let all_differ = ConcretizedType::for_all_concrete_ts(cx, t1, &|ct1| {
                    types_differ(cx, depth, ct1, &t2_c)
                })
                .unwrap_or(false);
                all_differ || type_tags_differ(cx, depth, rest1, rest2)
            }
            _ => false,
        }
    }

    // C<T> has no overlap with C<S> iff T and S have no overlap
    fn instance_tags_differ<'cx>(
        cx: &Context<'cx>,
        depth: i32,
        t1: &ConcretizedType,
        t2: &ConcretizedType,
    ) -> bool {
        match (t1.unwrap().deref(), t2.unwrap().deref()) {
            (TypeInner::DefT(_, d1), TypeInner::DefT(_, d2)) => match (d1.deref(), d2.deref()) {
                (DefTInner::InstanceT(inst_t1), DefTInner::InstanceT(inst_t2)) => {
                    let inst1 = &inst_t1.inst;
                    let inst2 = &inst_t2.inst;
                    let kind1_ok = matches!(
                        inst1.inst_kind,
                        InstanceKind::ClassKind | InstanceKind::RecordKind { .. }
                    );
                    let kind2_ok = matches!(
                        inst2.inst_kind,
                        InstanceKind::ClassKind | InstanceKind::RecordKind { .. }
                    );
                    if kind1_ok
                        && kind2_ok
                        && flow_js_utils::is_same_instance_type(inst_t1, inst_t2)
                    {
                        let ts1: Vec<Type> = inst1
                            .type_args
                            .iter()
                            .map(|(_, _, t, _)| t.dupe())
                            .collect();
                        let ts2: Vec<Type> = inst2
                            .type_args
                            .iter()
                            .map(|(_, _, t, _)| t.dupe())
                            .collect();
                        type_tags_differ(cx, depth, &ts1, &ts2)
                    } else {
                        false
                    }
                }
                _ => false,
            },
            _ => false,
        }
    }

    fn types_differ<'cx>(cx: &Context<'cx>, depth: i32, t1: &ConcretizedType, t2: &Type) -> bool {
        // To prevent infinite recursion, we use a simple depth mechanism.
        if depth > 2 {
            return false;
        }
        let depth = depth + 1;
        ConcretizedType::for_all_concrete_ts(cx, t2, &|ct2| {
            tags_differ(cx, t1, ct2)
                || ground_types_differ(t1, ct2)
                || instance_tags_differ(cx, depth, t1, ct2)
        })
        .unwrap_or(false)
    }

    fn try_intersect<'cx>(
        cx: &Context<'cx>,
        reason1: &Reason,
        t1_conc: &ConcretizedType,
        t2: &Type,
    ) -> Option<Type> {
        let t1 = t1_conc.unwrap();
        if types_differ(cx, 0, t1_conc, t2) {
            let r = reason1.dupe().update_desc(|d| d.invalidate_rtype_alias());
            Some(Type::new(TypeInner::DefT(r, DefT::new(DefTInner::EmptyT))))
        } else if is_any(cx, t1) {
            Some(t2.dupe())
        } else if is_any(cx, t2) {
            // Filter out null and void types from the input if comparing with any
            let t1_wrapped = ConcretizedType::wrap_unsafe(t1.dupe());
            if is_null(&t1_wrapped) || is_void(&t1_wrapped) {
                let r = reason1.dupe().update_desc(|d| d.invalidate_rtype_alias());
                Some(Type::new(TypeInner::DefT(r, DefT::new(DefTInner::EmptyT))))
            } else {
                Some(t1.dupe())
            }
        } else if type_util::quick_subtype(None::<&fn(&Type)>, t1, t2)
            || FlowJs::speculative_subtyping_succeeds(cx, t1, t2)
        {
            Some(t1.dupe())
        } else if type_util::quick_subtype(None::<&fn(&Type)>, t2, t1)
            || FlowJs::speculative_subtyping_succeeds(cx, t2, t1)
        {
            Some(t2.dupe())
        } else {
            match t1.deref() {
                TypeInner::NominalT {
                    reason: r,
                    nominal_type,
                } => {
                    // Apply the refinement on super and underlying type of opaque type.
                    // Preserve nominal_id to retain compatibility with original type.
                    let upper_t = Some(match &nominal_type.upper_t {
                        Some(upper) => intersect(cx, upper.dupe(), t2.dupe()),
                        None => t2.dupe(),
                    });
                    let underlying_t = match &nominal_type.underlying_t {
                        nominal::UnderlyingT::FullyOpaque => nominal::UnderlyingT::FullyOpaque,
                        nominal::UnderlyingT::CustomError(box nominal::CustomErrorData {
                            custom_error_loc,
                            t: inner_t,
                        }) => {
                            nominal::UnderlyingT::CustomError(Box::new(nominal::CustomErrorData {
                                custom_error_loc: custom_error_loc.dupe(),
                                t: intersect(cx, inner_t.dupe(), t2.dupe()),
                            }))
                        }
                        nominal::UnderlyingT::OpaqueWithLocal { t: inner_t } => {
                            nominal::UnderlyingT::OpaqueWithLocal {
                                t: intersect(cx, inner_t.dupe(), t2.dupe()),
                            }
                        }
                    };
                    Some(Type::new(TypeInner::NominalT {
                        reason: r.dupe(),
                        nominal_type: Rc::new(NominalType::new(NominalTypeInner {
                            nominal_id: nominal_type.nominal_id.clone(),
                            underlying_t,
                            lower_t: nominal_type.lower_t.as_ref().map(|t| t.dupe()),
                            upper_t,
                            nominal_type_args: nominal_type.nominal_type_args.dupe(),
                        })),
                    }))
                }
                _ => None,
            }
        }
    }

    let reason1 = reason_of_t(&t1);
    match try_intersect(cx, reason1, &ConcretizedType::wrap_unsafe(t1.dupe()), &t2) {
        Some(t) => t,
        None => {
            // No definitive refinement found. We fall back to more expensive
            //  concretization that breaks up all unions (including optimized ones)
            let ts = FlowJs::possible_concrete_types_for_inspection(cx, reason1, &t1)
                .unwrap_or_default();
            let mapped: Vec<Type> = ts
                .into_iter()
                .map(|t1_inner| {
                    // t1 was just concretized
                    match try_intersect(
                        cx,
                        reason1,
                        &ConcretizedType::wrap_unsafe(t1_inner.dupe()),
                        &t2,
                    ) {
                        Some(t) => t,
                        None => {
                            let r = reason1.dupe().update_desc(|d| d.invalidate_rtype_alias());
                            Type::new(TypeInner::IntersectionT(
                                r,
                                inter_rep::make(t2.dupe(), t1_inner, Rc::from([])),
                            ))
                        }
                    }
                })
                .collect();
            let r = reason1.dupe().update_desc(|d| d.invalidate_rtype_alias());
            type_util::union_of_ts(r, mapped, None)
        }
    }
}

/// This utility is expected to be used when negating the refinement of a type [t1]
/// with a type guard `x is t2`. The only case considered here is that of t1 <: t2.
/// This means that the positive branch will always be taken, and so we are left with
/// `empty` in the negated case.
fn type_guard_diff<'cx>(cx: &Context<'cx>, t1: &Type, t2: &Type) -> FilterResult {
    let reason1 = reason_of_t(t1);
    if type_util::quick_subtype(None::<&fn(&Type)>, t1, t2)
        || FlowJs::speculative_subtyping_succeeds(cx, t1, t2)
    {
        let r = reason1.dupe().update_desc(|d| d.invalidate_rtype_alias());
        FilterResult {
            type_: Type::new(TypeInner::DefT(r, DefT::new(DefTInner::EmptyT))),
            changed: true,
        }
    } else {
        let t1s_conc =
            FlowJs::possible_concrete_types_for_inspection(cx, reason1, t1).unwrap_or_default();
        let (ts, changed) =
            t1s_conc
                .into_iter()
                .fold((Vec::new(), false), |(mut acc, changed), t1_inner| {
                    if type_util::quick_subtype(None::<&fn(&Type)>, &t1_inner, t2)
                        || FlowJs::speculative_subtyping_succeeds(cx, &t1_inner, t2)
                    {
                        (acc, changed)
                    } else {
                        acc.push(t1_inner);
                        (acc, true)
                    }
                });
        let r1 = reason1.dupe().update_desc(|d| d.invalidate_rtype_alias());
        FilterResult {
            type_: type_util::union_of_ts(r1, ts, None),
            changed,
        }
    }
}

fn prop_exists_test<'cx>(
    cx: &Context<'cx>,
    key: &str,
    sense: bool,
    obj: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    match has_prop(cx, &Name::new(key), obj)? {
        Some(has) => {
            if has == sense {
                report_unchanged_filtering_result_to_predicate_result(obj.dupe(), result_collector);
            } else {
                report_changes_to_input(result_collector);
            }
        }
        None => {
            report_unchanged_filtering_result_to_predicate_result(obj.dupe(), result_collector);
        }
    }
    Ok(())
}

/// If an object has an own or non-own prop, representing `'key' in obj`.
/// Returns `None` if it is unknown whether the object has the prop (for example
/// due to inexact objects)
fn has_prop<'cx>(
    cx: &Context<'cx>,
    key: &Name,
    obj: &Type,
) -> Result<Option<bool>, FlowJsException> {
    fn all_have_prop(xs: Vec<Option<bool>>) -> Option<bool> {
        xs.into_iter().try_fold(true, |acc, x| x.map(|b| acc && b))
    }

    fn some_has_prop(xs: Vec<Option<bool>>) -> Option<bool> {
        let mut acc = Some(false);
        for x in xs {
            acc = match (acc, x) {
                (Some(true), None) | (None, Some(true)) => Some(true),
                (_, None) | (None, _) => None,
                (Some(a), Some(b)) => Some(a || b),
            };
        }
        acc
    }

    fn find_key<'cx>(
        cx: &Context<'cx>,
        exact: bool,
        super_t: &Type,
        props_list: &[properties::Id],
        key: &Name,
    ) -> Result<Option<bool>, FlowJsException> {
        let current_has_prop_results: Vec<Option<bool>> = props_list
            .iter()
            .map(|props| match cx.get_prop(props.dupe(), key) {
                Some(prop) => match prop.deref() {
                    PropertyInner::Field(fd)
                        if flow_typing_flow_js::slice_utils::is_prop_optional(&fd.type_) =>
                    {
                        None
                    }
                    _ => Some(true),
                },
                None => Some(false),
            })
            .collect();
        let current_has_prop = some_has_prop(current_has_prop_results);

        match current_has_prop {
            Some(true) => Ok(Some(true)),
            _ => {
                let super_ts = FlowJs::possible_concrete_types_for_inspection(
                    cx,
                    reason_of_t(super_t),
                    super_t,
                )?;
                let super_results: Vec<Option<bool>> = super_ts
                    .iter()
                    .map(|t| has_prop(cx, key, t))
                    .collect::<Result<Vec<_>, _>>()?;
                let super_has_prop = all_have_prop(super_results);
                match super_has_prop {
                    Some(true) => Ok(Some(true)),
                    Some(false) if !exact => Ok(None),
                    Some(false) => Ok(current_has_prop),
                    None => Ok(None),
                }
            }
        }
    }

    match obj.deref() {
        TypeInner::DefT(_, def_t) => match def_t.deref() {
            DefTInner::ObjT(obj_t) => find_key(
                cx,
                obj_type::is_exact(&obj_t.flags.obj_kind),
                &obj_t.proto_t,
                &[obj_t.props_tmap.dupe()],
                key,
            ),
            DefTInner::InstanceT(inst_t) => find_key(
                cx,
                false,
                &inst_t.super_,
                &[inst_t.inst.own_props.dupe(), inst_t.inst.proto_props.dupe()],
                key,
            ),
            _ => Ok(None),
        },
        TypeInner::NullProtoT(_) => Ok(Some(false)),
        TypeInner::ObjProtoT(_) => Ok(Some(flow_js_utils::is_object_prototype_method(key))),
        TypeInner::FunProtoT(_) => Ok(Some(flow_js_utils::is_function_prototype(key))),
        TypeInner::IntersectionT(reason, rep) => {
            let member_results: Vec<Option<bool>> = rep
                .members_iter()
                .map(|t| {
                    let ts = FlowJs::possible_concrete_types_for_inspection(cx, reason, t)?;
                    let inner_results: Vec<Option<bool>> = ts
                        .iter()
                        .map(|t| has_prop(cx, key, t))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(all_have_prop(inner_results))
                })
                .collect::<Result<Vec<_>, FlowJsException>>()?;
            Ok(some_has_prop(member_results))
        }
        _ => Ok(None),
    }
}

fn prop_truthy_test<'cx>(
    cx: &Context<'cx>,
    key: &str,
    reason: &Reason,
    sense: bool,
    obj: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    prop_exists_test_generic(
        key,
        reason,
        cx,
        result_collector,
        obj,
        sense,
        (&PropGuard::PropGuardTruthy, &PropGuard::PropGuardNotTruthy),
        obj,
    )
}

fn prop_non_maybe_test<'cx>(
    cx: &Context<'cx>,
    key: &str,
    reason: &Reason,
    sense: bool,
    obj: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    prop_exists_test_generic(
        key,
        reason,
        cx,
        result_collector,
        obj,
        sense,
        (&PropGuard::PropGuardNotMaybe, &PropGuard::PropGuardMaybe),
        obj,
    )
}

fn prop_is_exactly_null_test<'cx>(
    cx: &Context<'cx>,
    key: &str,
    reason: &Reason,
    sense: bool,
    obj: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    prop_exists_test_generic(
        key,
        reason,
        cx,
        result_collector,
        obj,
        sense,
        (&PropGuard::PropGuardNull, &PropGuard::PropGuardNotNull),
        obj,
    )
}

fn prop_non_void_test<'cx>(
    cx: &Context<'cx>,
    key: &str,
    reason: &Reason,
    sense: bool,
    obj: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    prop_exists_test_generic(
        key,
        reason,
        cx,
        result_collector,
        obj,
        sense,
        (&PropGuard::PropGuardNotVoid, &PropGuard::PropGuardVoid),
        obj,
    )
}

fn prop_exists_test_generic<'cx>(
    key: &str,
    reason: &Reason,
    cx: &Context<'cx>,
    result_collector: &PredicateResultCollector,
    orig_obj: &Type,
    sense: bool,
    pred_pair: (&PropGuard, &PropGuard),
    obj: &Type,
) -> Result<(), FlowJsException> {
    let (pred, not_pred) = pred_pair;
    match obj.deref() {
        TypeInner::DefT(_, def_t) => {
            match def_t.deref() {
                DefTInner::ObjT(obj_t) => {
                    match cx.get_prop(obj_t.props_tmap.dupe(), &Name::new(key)) {
                        Some(p) => {
                            match flow_typing_type::type_::property::read_t(&p) {
                                Some(t) => {
                                    // prop is present on object type
                                    let pred = if sense { pred } else { not_pred };
                                    concretize_and_guard_prop(
                                        cx,
                                        &t,
                                        pred,
                                        orig_obj,
                                        result_collector,
                                    )?;
                                }
                                None => {
                                    // prop cannot be read
                                    report_unchanged_filtering_result_to_predicate_result(
                                        orig_obj.dupe(),
                                        result_collector,
                                    );
                                    flow_js_utils::add_output(
                                        cx,
                                        ErrorMessage::EPropNotReadable(Box::new(
                                            EPropNotReadableData {
                                                reason_prop: reason.dupe(),
                                                prop_name: Some(Name::new(key)),
                                                use_op: unknown_use(),
                                            },
                                        )),
                                    )?;
                                }
                            }
                        }
                        None if obj_type::is_exact(&obj_t.flags.obj_kind) => {
                            // prop is absent from exact object type
                            if sense {
                                report_changes_to_input(result_collector);
                            } else {
                                report_unchanged_filtering_result_to_predicate_result(
                                    orig_obj.dupe(),
                                    result_collector,
                                );
                            }
                        }
                        None => {
                            // prop is absent from inexact object type
                            // TODO: possibly unsound to filter out orig_obj here, but if we don't,
                            // case elimination based on prop existence checking doesn't work for
                            // (disjoint unions of) intersections of objects, where the prop appears
                            // in a different branch of the intersection. It is easy to avoid this
                            // unsoundness with slightly more work, but will wait until a
                            // refactoring of property lookup lands to revisit. Tracked by
                            // #11301092.
                            if orig_obj == obj {
                                report_unchanged_filtering_result_to_predicate_result(
                                    orig_obj.dupe(),
                                    result_collector,
                                );
                            }
                        }
                    }
                }
                DefTInner::ArrT(arr_t)
                    if let ArrType::TupleAT(box TupleATData { elements, .. }) = arr_t.deref()
                        && flow_js_utils::is_str_intlike(key) =>
                {
                    let i: usize = key.parse().unwrap();
                    match elements.get(i) {
                        Some(TupleElement {
                            t, polarity, name, ..
                        }) => {
                            if Polarity::compat(*polarity, Polarity::Positive) {
                                let pred = if sense { pred } else { not_pred };
                                concretize_and_guard_prop(cx, t, pred, orig_obj, result_collector)?;
                            } else {
                                report_unchanged_filtering_result_to_predicate_result(
                                    orig_obj.dupe(),
                                    result_collector,
                                );
                                flow_js_utils::add_output(
                                    cx,
                                    ErrorMessage::ETupleElementNotReadable(Box::new(
                                        ETupleElementNotReadableData {
                                            use_op: unknown_use(),
                                            reason: reason.dupe(),
                                            index: i as i32,
                                            name: name.as_ref().map(|n| n.dupe()),
                                        },
                                    )),
                                )?;
                            }
                        }
                        None => {
                            // Element is absent from tuple type.
                            if sense {
                                report_changes_to_input(result_collector);
                            } else {
                                report_unchanged_filtering_result_to_predicate_result(
                                    orig_obj.dupe(),
                                    result_collector,
                                );
                            }
                        }
                    }
                }
                _ => {
                    report_unchanged_filtering_result_to_predicate_result(
                        orig_obj.dupe(),
                        result_collector,
                    );
                }
            }
        }
        TypeInner::IntersectionT(inter_reason, rep) => {
            // For an intersection of object types, try the test for each object type in
            // turn, while recording the original intersection so that we end up with
            // the right refinement. See the comment on the implementation of
            // IntersectionPreprocessKit for more details.
            for member_obj in rep.members_iter() {
                let ts =
                    FlowJs::possible_concrete_types_for_inspection(cx, inter_reason, member_obj)?;
                for t in ts.iter() {
                    prop_exists_test_generic(
                        key,
                        reason,
                        cx,
                        result_collector,
                        orig_obj,
                        sense,
                        (pred, not_pred),
                        t,
                    )?;
                }
            }
        }
        _ => {
            report_unchanged_filtering_result_to_predicate_result(
                orig_obj.dupe(),
                result_collector,
            );
        }
    }
    Ok(())
}

fn binary_predicate<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    sense: bool,
    test: &BinaryTest,
    left: &Type,
    right: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    match test {
        BinaryTest::InstanceofTest => instanceof_test(
            cx,
            trace,
            result_collector,
            sense,
            left,
            &InstanceofRhs::TypeOperand(right.dupe()),
        ),
        BinaryTest::SentinelProp(key) => {
            sentinel_prop_test(key, cx, result_collector, sense, left, right)
        }
        BinaryTest::EqTest => Ok(eq_test(cx, result_collector, sense, left, right)),
    }
}

fn instanceof_test<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    result_collector: &PredicateResultCollector,
    sense: bool,
    left: &Type,
    right: &InstanceofRhs,
) -> Result<(), FlowJsException> {
    match (sense, left.deref(), right) {
        // instanceof on an ArrT is a special case since we treat ArrT as its own
        // type, rather than an InstanceT of the Array builtin class. So, we resolve
        // the ArrT to an InstanceT of Array, and redo the instanceof check. We do
        // it at this stage instead of simply converting (ArrT, InstanceofP c)
        // to (InstanceT(Array), InstanceofP c) because this allows c to be resolved
        // first.
        (true, TypeInner::DefT(reason, def_t), InstanceofRhs::TypeOperand(right_t))
            if let DefTInner::ArrT(arrtype) = def_t.deref()
                && let TypeInner::DefT(r, right_def) = right_t.deref()
                && let DefTInner::ClassT(a) = right_def.deref() =>
        {
            let arr = left.dupe();
            let elemt = elemt_of_arrtype(arrtype);
            let right = InstanceofRhs::InternalExtendsOperand(
                r.dupe()
                    .update_desc(|d| VirtualReasonDesc::RExtends(Arc::new(d))),
                arr.dupe(),
                a.dupe(),
            );
            let arrt = FlowJs::get_builtin_typeapp(cx, reason, None, "Array", vec![elemt]);
            concretize_and_run_predicate(
                cx,
                trace,
                &arrt,
                PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
                result_collector,
                &|cx, trace, rc, l| instanceof_test(cx, trace, rc, true, l, &right),
            )?;
        }
        (false, TypeInner::DefT(reason, def_t), InstanceofRhs::TypeOperand(right_t))
            if let DefTInner::ArrT(arrtype) = def_t.deref()
                && let TypeInner::DefT(r, right_def) = right_t.deref()
                && let DefTInner::ClassT(a) = right_def.deref() =>
        {
            let arr = left.dupe();
            let elemt = elemt_of_arrtype(arrtype);
            let right = InstanceofRhs::InternalExtendsOperand(
                r.dupe()
                    .update_desc(|d| VirtualReasonDesc::RExtends(Arc::new(d))),
                arr.dupe(),
                a.dupe(),
            );
            let arrt = FlowJs::get_builtin_typeapp(cx, reason, None, "Array", vec![elemt]);
            concretize_and_run_predicate(
                cx,
                trace,
                &arrt,
                PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
                result_collector,
                &|cx, trace, rc, l| instanceof_test(cx, trace, rc, false, l, &right),
            )?;
        }
        // Suppose that we have an instance x of class C, and we check whether x is
        // `instanceof` class A. To decide what the appropriate refinement for x
        // should be, we need to decide whether C extends A, choosing either C or A
        // based on the result. Thus, we generate a constraint to decide whether C
        // extends A (while remembering C), which may recursively generate further
        // constraints to decide super(C) extends A, and so on, until we hit the root
        // class. (As a technical tool, we use Extends(_, _) to perform this
        // recursion; it is also used elsewhere for running similar recursive
        // subclass decisions.)
        (true, TypeInner::DefT(_, def_t), InstanceofRhs::TypeOperand(right_t))
            if let DefTInner::InstanceT(..) = def_t.deref()
                && let TypeInner::DefT(r, right_def) = right_t.deref()
                && let DefTInner::ClassT(a) = right_def.deref() =>
        {
            let c = left;
            let right = InstanceofRhs::InternalExtendsOperand(
                r.dupe()
                    .update_desc(|d| VirtualReasonDesc::RExtends(Arc::new(d))),
                c.dupe(),
                a.dupe(),
            );
            instanceof_test(cx, trace, result_collector, true, c, &right)?;
        }
        // If C is a subclass of A, then don't refine the type of x. Otherwise,
        // refine the type of x to A. (In general, the type of x should be refined to
        // C & A, but that's hard to compute.)
        (
            true,
            TypeInner::DefT(reason, def_t),
            InstanceofRhs::InternalExtendsOperand(_, c, a_t),
        ) if let DefTInner::InstanceT(instance_t) = def_t.deref()
            && let TypeInner::DefT(_, a_def) = a_t.deref()
            && let DefTInner::InstanceT(a_instance_t) = a_def.deref() =>
        {
            let _instance_c = &instance_t.inst;
            let super_c = &instance_t.super_;
            let _instance_a = &a_instance_t.inst;
            // TODO: intersection
            if flow_js_utils::is_same_instance_type(a_instance_t, instance_t) {
                report_unchanged_filtering_result_to_predicate_result(c.dupe(), result_collector);
            } else {
                // Recursively check whether super(C) extends A, with enough context.
                let repositioned =
                    FlowJs::reposition_reason(cx, Some(trace), reason, None, super_c)?;
                let right_clone = right.clone();
                concretize_and_run_predicate(
                    cx,
                    trace,
                    &repositioned,
                    PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
                    result_collector,
                    &|cx, trace, rc, l| instanceof_test(cx, trace, rc, true, l, &right_clone),
                )?;
            }
        }
        // If we are checking `instanceof Object` or `instanceof Function`, objects
        // with `ObjProtoT` or `FunProtoT` should pass.
        (true, TypeInner::ObjProtoT(reason), right @ InstanceofRhs::InternalExtendsOperand(..)) => {
            let obj_proto =
                FlowJs::get_builtin_type(cx, Some(trace), reason, Some(true), "Object")?;
            let right_clone = right.clone();
            concretize_and_run_predicate(
                cx,
                trace,
                &obj_proto,
                PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
                result_collector,
                &|cx, trace, rc, l| instanceof_test(cx, trace, rc, true, l, &right_clone),
            )?;
        }
        (true, TypeInner::FunProtoT(reason), right @ InstanceofRhs::InternalExtendsOperand(..)) => {
            //   let fun_proto = get_builtin_type cx ~trace reason ~use_desc:true "Function" in
            let fun_proto =
                FlowJs::get_builtin_type(cx, Some(trace), reason, Some(true), "Function")?;
            let right_clone = right.clone();
            concretize_and_run_predicate(
                cx,
                trace,
                &fun_proto,
                PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
                result_collector,
                &|cx, trace, rc, l| instanceof_test(cx, trace, rc, true, l, &right_clone),
            )?;
        }
        // We hit the root class, so C is not a subclass of A
        (true, TypeInner::DefT(_, def_t), InstanceofRhs::InternalExtendsOperand(r, _, a))
            if let DefTInner::NullT = def_t.deref() =>
        {
            // We hit the root class, so C is not a subclass of A
            let repositioned =
                flow_typing_flow_js::flow_js::reposition(cx, r.loc().dupe(), a.dupe())?;
            report_changed_filtering_result_to_predicate_result(repositioned, result_collector);
        }
        // If we're refining `mixed` or `any` with instanceof A, then flow A to the result
        (true, TypeInner::DefT(_, def_t), InstanceofRhs::TypeOperand(right_t))
            if let DefTInner::MixedT(..) = def_t.deref()
                && let TypeInner::DefT(class_reason, right_def) = right_t.deref()
                && let DefTInner::ClassT(a) = right_def.deref() =>
        {
            let desc = reason_of_t(a).desc(true);
            let loc = class_reason.loc().dupe();
            let repositioned =
                FlowJs::reposition(cx, Some(trace), loc, Some(desc), None, a.dupe())?;
            report_changed_filtering_result_to_predicate_result(repositioned, result_collector);
        }
        (true, TypeInner::AnyT(..), InstanceofRhs::TypeOperand(right_t))
            if let TypeInner::DefT(class_reason, right_def) = right_t.deref()
                && let DefTInner::ClassT(a) = right_def.deref() =>
        {
            let desc = reason_of_t(a).desc(true);
            let loc = class_reason.loc().dupe();
            let repositioned =
                FlowJs::reposition(cx, Some(trace), loc, Some(desc), None, a.dupe())?;
            report_changed_filtering_result_to_predicate_result(repositioned, result_collector);
        }
        // Prune the type when any other `instanceof` check succeeds (since this is
        // impossible).
        (true, _, _) => {
            report_changes_to_input(result_collector);
        }
        // Like above, now suppose that we have an instance x of class C, and we
        // check whether x is _not_ `instanceof` class A. To decide what the
        // appropriate refinement for x should be, we need to decide whether C
        // extends A, choosing either nothing or C based on the result.
        (false, TypeInner::DefT(_, def_t), InstanceofRhs::TypeOperand(right_t))
            if let DefTInner::InstanceT(..) = def_t.deref()
                && let TypeInner::DefT(r, right_def) = right_t.deref()
                && let DefTInner::ClassT(a) = right_def.deref()
                && let TypeInner::DefT(_, a_inner_def) = a.deref()
                && let DefTInner::InstanceT(..) = a_inner_def.deref() =>
        {
            let c = left;
            let right = InstanceofRhs::InternalExtendsOperand(
                r.dupe()
                    .update_desc(|d| VirtualReasonDesc::RExtends(Arc::new(d))),
                c.dupe(),
                a.dupe(),
            );
            instanceof_test(cx, trace, result_collector, false, c, &right)?;
        }
        // If C is a subclass of A, then do nothing, since this check cannot
        // succeed. Otherwise, don't refine the type of x.
        (
            false,
            TypeInner::DefT(reason, def_t),
            InstanceofRhs::InternalExtendsOperand(_, _, a_t),
        ) if let DefTInner::InstanceT(instance_t) = def_t.deref()
            && let TypeInner::DefT(_, a_def) = a_t.deref()
            && let DefTInner::InstanceT(a_instance_t) = a_def.deref() =>
        {
            let _instance_c = &instance_t.inst;
            let super_c = &instance_t.super_;
            let _instance_a = &a_instance_t.inst;
            if flow_js_utils::is_same_instance_type(a_instance_t, instance_t) {
                report_changes_to_input(result_collector);
            } else {
                let repositioned =
                    FlowJs::reposition_reason(cx, Some(trace), reason, None, super_c)?;
                let right_clone = right.clone();
                concretize_and_run_predicate(
                    cx,
                    trace,
                    &repositioned,
                    PredicateConcretetizerVariant::ConcretizeForGeneralPredicateTest,
                    result_collector,
                    &|cx, trace, rc, l| instanceof_test(cx, trace, rc, false, l, &right_clone),
                )?;
            }
        }
        (false, TypeInner::ObjProtoT(_), InstanceofRhs::InternalExtendsOperand(r, c, _)) => {
            // We hit the root class, so C is not a subclass of A.
            // In this case, we will refine the input to C
            let repositioned =
                flow_typing_flow_js::flow_js::reposition(cx, r.loc().dupe(), c.dupe())?;
            report_changed_filtering_result_to_predicate_result(repositioned, result_collector);
        }
        (false, _, _) => {
            report_unchanged_filtering_result_to_predicate_result(left.dupe(), result_collector);
        }
    }
    Ok(())
}

fn sentinel_prop_test<'cx>(
    key: &str,
    cx: &Context<'cx>,
    result_collector: &PredicateResultCollector,
    sense: bool,
    obj: &Type,
    t: &Type,
) -> Result<(), FlowJsException> {
    sentinel_prop_test_generic(key, cx, result_collector, obj, sense, obj, t)
}

fn sentinel_prop_test_generic<'cx>(
    key: &str,
    cx: &Context<'cx>,
    result_collector: &PredicateResultCollector,
    orig_obj: &Type,
    sense: bool,
    obj: &Type,
    t: &Type,
) -> Result<(), FlowJsException> {
    fn desc_of_sentinel(sentinel: &UnionEnumStar) -> ReasonDesc {
        match sentinel {
            UnionEnumStar::One(UnionEnum::Str(s)) => VirtualReasonDesc::RStringLit(s.dupe()),
            UnionEnumStar::One(UnionEnum::Num(NumberLiteral(_, n))) => {
                VirtualReasonDesc::RNumberLit(n.dupe())
            }
            UnionEnumStar::One(UnionEnum::Bool(b)) => VirtualReasonDesc::RBooleanLit(*b),
            UnionEnumStar::One(UnionEnum::BigInt(BigIntLiteral(_, n))) => {
                VirtualReasonDesc::RBigIntLit(n.dupe())
            }
            UnionEnumStar::One(UnionEnum::Null) => VirtualReasonDesc::RNull,
            UnionEnumStar::One(UnionEnum::Void) => VirtualReasonDesc::RVoid,
            UnionEnumStar::Many(_) => VirtualReasonDesc::RUnionEnum,
        }
    }

    // Evaluate a refinement predicate of the form
    //
    // obj.key eq value
    //
    // where eq is === or !==.
    //
    // * key is key
    // * (sense, obj, value) are the sense of the test, obj and value as above,
    // respectively.
    //
    // As with other predicate filters, the goal is to statically determine when
    // the predicate is definitely satisfied and when it is definitely
    // // unsatisfied, and narrow the possible types of obj under those conditions,
    // while not narrowing in all other cases.
    //
    // In this case, the predicate is definitely satisfied (respectively,
    // definitely unsatisfied) when the type of the key property in the type obj
    // can be statically verified as having (respectively, not having) value as
    // its only inhabitant.
    //
    // When satisfied, type obj flows to the recipient type result (in other
    // words, we allow all such types in the refined type for obj).
    //
    // Otherwise, nothing flows to type result (in other words, we don't allow
    // any such type in the refined type for obj).
    //
    // Overall the filtering process is somewhat tricky to understand. Refer to
    // the predicate function and its callers to understand how the context is
    // set up so that filtering ultimately only depends on what flows to
    // result.
    let flow_sentinel_obj = |sense: bool,
                             props_tmap: properties::Id,
                             obj: &Type,
                             sentinel: &UnionEnumStar|
     -> Result<(), FlowJsException> {
        match cx.get_prop(props_tmap, &Name::new(key)) {
            Some(p) => match flow_typing_type::type_::property::read_t(&p) {
                Some(t_prop) => {
                    let desc = VirtualReasonDesc::RMatchingProp(
                        FlowSmolStr::new(key),
                        Arc::new(desc_of_sentinel(sentinel)),
                    );
                    let reason = reason_of_t(orig_obj).dupe().replace_desc(desc);
                    concretize_and_run_sentinel_prop_test(
                        cx,
                        &reason,
                        orig_obj,
                        sense,
                        sentinel,
                        &t_prop,
                        result_collector,
                    )?;
                }
                None => {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::EPropNotReadable(Box::new(EPropNotReadableData {
                            reason_prop: reason_of_t(obj).dupe(),
                            prop_name: Some(Name::new(key)),
                            use_op: unknown_use(),
                        })),
                    )?;
                }
            },
            None => {
                // TODO: possibly unsound to filter out orig_obj here, but if we
                // don't, case elimination based on sentinel prop checking doesn't
                // work for (disjoint unions of) intersections of objects, where the
                // sentinel prop and the payload appear in different branches of the
                // intersection. It is easy to avoid this unsoundness with slightly
                // more work, but will wait until a refactoring of property lookup
                // lands to revisit. Tracked by #11301092.
                if orig_obj == obj {
                    report_unchanged_filtering_result_to_predicate_result(
                        orig_obj.dupe(),
                        result_collector,
                    );
                }
            }
        }
        Ok(())
    };

    let flow_sentinel_tuple = |sense: bool,
                               elements: &[TupleElement],
                               tuple: &Type,
                               sentinel: &UnionEnumStar|
     -> Result<(), FlowJsException> {
        let i: usize = key.parse().unwrap();
        match elements.get(i) {
            Some(TupleElement {
                t: elem_t,
                polarity,
                name,
                ..
            }) => {
                if Polarity::compat(*polarity, Polarity::Positive) {
                    let desc = VirtualReasonDesc::RMatchingProp(
                        FlowSmolStr::new(key),
                        Arc::new(desc_of_sentinel(sentinel)),
                    );
                    let reason = reason_of_t(orig_obj).dupe().replace_desc(desc);
                    concretize_and_run_sentinel_prop_test(
                        cx,
                        &reason,
                        orig_obj,
                        sense,
                        sentinel,
                        elem_t,
                        result_collector,
                    )?;
                } else {
                    flow_js_utils::add_output(
                        cx,
                        ErrorMessage::ETupleElementNotReadable(Box::new(
                            ETupleElementNotReadableData {
                                use_op: unknown_use(),
                                reason: reason_of_t(tuple).dupe(),
                                index: i as i32,
                                name: name.as_ref().map(|n| n.dupe()),
                            },
                        )),
                    )?;
                }
            }
            None => {
                if orig_obj == tuple {
                    report_unchanged_filtering_result_to_predicate_result(
                        orig_obj.dupe(),
                        result_collector,
                    );
                }
            }
        }
        Ok(())
    };

    fn sentinel_of_literal(t: &Type) -> Option<UnionEnumStar> {
        match t.deref() {
            TypeInner::DefT(_, def_t) => match def_t.deref() {
                DefTInner::SingletonStrT { value, .. } => {
                    Some(UnionEnumStar::One(UnionEnum::Str(value.dupe())))
                }
                DefTInner::SingletonNumT { value, .. } => {
                    Some(UnionEnumStar::One(UnionEnum::Num(value.clone())))
                }
                DefTInner::SingletonBoolT { value, .. } => {
                    Some(UnionEnumStar::One(UnionEnum::Bool(*value)))
                }
                DefTInner::SingletonBigIntT { value, .. } => {
                    Some(UnionEnumStar::One(UnionEnum::BigInt(value.clone())))
                }
                DefTInner::VoidT => Some(UnionEnumStar::One(UnionEnum::Void)),
                DefTInner::NullT => Some(UnionEnumStar::One(UnionEnum::Null)),
                _ => None,
            },
            TypeInner::UnionT(_, rep) => rep.check_enum().map(UnionEnumStar::Many),
            _ => None,
        }
    }

    match sentinel_of_literal(t) {
        Some(s) => {
            match obj.deref() {
                TypeInner::DefT(_, def_t) => {
                    match def_t.deref() {
                        // obj.key ===/!== literal value
                        DefTInner::ObjT(obj_t) => {
                            flow_sentinel_obj(sense, obj_t.props_tmap.dupe(), obj, &s)?;
                        }
                        // instance.key ===/!== literal value
                        DefTInner::InstanceT(instance_t) => {
                            flow_sentinel_obj(sense, instance_t.inst.own_props.dupe(), obj, &s)?;
                        }
                        // tuple.length ===/!== literal value
                        DefTInner::ArrT(arr_t)
                            if let ArrType::TupleAT(box TupleATData { arity, inexact, .. }) =
                                arr_t.deref()
                                && key == "length" =>
                        {
                            let reason = reason_of_t(obj).dupe();
                            let input =
                                type_util::tuple_length(reason.dupe(), *inexact, arity.0, arity.1);
                            concretize_and_run_sentinel_prop_test(
                                cx,
                                &reason,
                                orig_obj,
                                sense,
                                &s,
                                &input,
                                result_collector,
                            )?;
                        }
                        DefTInner::ArrT(arr_t)
                            if let ArrType::TupleAT(box TupleATData { elements, .. }) =
                                arr_t.deref()
                                && flow_js_utils::is_str_intlike(key) =>
                        {
                            flow_sentinel_tuple(sense, elements, obj, &s)?;
                        }
                        _ => {
                            report_unchanged_filtering_result_to_predicate_result(
                                orig_obj.dupe(),
                                result_collector,
                            );
                        }
                    }
                }
                TypeInner::IntersectionT(inter_reason, rep) => {
                    for member_obj in rep.members_iter() {
                        let ts = FlowJs::possible_concrete_types_for_inspection(
                            cx,
                            inter_reason,
                            member_obj,
                        )?;
                        for l in ts.iter() {
                            sentinel_prop_test_generic(
                                key,
                                cx,
                                result_collector,
                                orig_obj,
                                sense,
                                l,
                                t,
                            )?;
                        }
                    }
                }
                // not enough info to refine
                _ => {
                    report_unchanged_filtering_result_to_predicate_result(
                        orig_obj.dupe(),
                        result_collector,
                    );
                }
            }
        }
        // not enough info to refine
        None => {
            report_unchanged_filtering_result_to_predicate_result(
                orig_obj.dupe(),
                result_collector,
            );
        }
    }
    Ok(())
}

fn concretize_and_run_sentinel_prop_test<'cx>(
    cx: &Context<'cx>,
    reason: &Reason,
    orig_obj: &Type,
    sense: bool,
    sentinel: &UnionEnumStar,
    input: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    fn enum_to_def(e: &UnionEnum) -> DefTInner {
        match e {
            UnionEnum::Str(v) => DefTInner::SingletonStrT {
                from_annot: true,
                value: v.dupe(),
            },
            UnionEnum::Num(v) => DefTInner::SingletonNumT {
                from_annot: true,
                value: v.clone(),
            },
            UnionEnum::Bool(v) => DefTInner::SingletonBoolT {
                from_annot: true,
                value: *v,
            },
            UnionEnum::BigInt(v) => DefTInner::SingletonBigIntT {
                from_annot: true,
                value: v.clone(),
            },
            UnionEnum::Void => DefTInner::VoidT,
            UnionEnum::Null => DefTInner::NullT,
        }
    }

    let ts = FlowJs::possible_concrete_types_for_sentinel_prop_test(cx, reason, input)?;
    for t in ts.iter() {
        match t.deref() {
            TypeInner::UnionT(r, rep) => {
                let l = orig_obj;
                // we have the check l.key === sentinel where l.key is a union
                if sense {
                    match sentinel {
                        UnionEnumStar::One(one_enum) => {
                            let def = enum_to_def(one_enum);
                            match union_rep::quick_mem_enum(
                                |t1: &Type, t2: &Type| {
                                    type_util::quick_subtype(None::<&fn(&Type)>, t1, t2)
                                },
                                &Type::new(TypeInner::DefT(r.dupe(), DefT::new(def))),
                                rep,
                            ) {
                                // provably unreachable, so prune
                                QuickMemResult::No => {
                                    report_changes_to_input(result_collector);
                                }
                                QuickMemResult::Yes => {
                                    report_unchanged_filtering_result_to_predicate_result(
                                        l.dupe(),
                                        result_collector,
                                    );
                                }
                                // inconclusive: the union is not concretized
                                QuickMemResult::Conditional(_) | QuickMemResult::Unknown => {
                                    for member in rep.members_iter() {
                                        concretize_and_run_sentinel_prop_test(
                                            cx,
                                            reason,
                                            orig_obj,
                                            sense,
                                            sentinel,
                                            member,
                                            result_collector,
                                        )?;
                                    }
                                }
                            }
                        }
                        UnionEnumStar::Many(enums) => {
                            let acc = enums.iter().fold(QuickMemResult::No, |acc, one_enum| {
                                let def = enum_to_def(one_enum);
                                let check_t = Type::new(TypeInner::DefT(r.dupe(), DefT::new(def)));
                                let qm_result = union_rep::quick_mem_enum(
                                    |t1: &Type, t2: &Type| {
                                        type_util::quick_subtype(None::<&fn(&Type)>, t1, t2)
                                    },
                                    &check_t,
                                    rep,
                                );
                                union_rep::join_quick_mem_results((acc, qm_result))
                            });
                            match acc {
                                // provably unreachable, so prune
                                QuickMemResult::No => {
                                    report_changes_to_input(result_collector);
                                }
                                QuickMemResult::Yes => {
                                    report_unchanged_filtering_result_to_predicate_result(
                                        l.dupe(),
                                        result_collector,
                                    );
                                }
                                // inconclusive: the union is not concretized
                                QuickMemResult::Conditional(_) | QuickMemResult::Unknown => {
                                    for member in rep.members_iter() {
                                        concretize_and_run_sentinel_prop_test(
                                            cx,
                                            reason,
                                            orig_obj,
                                            sense,
                                            sentinel,
                                            member,
                                            result_collector,
                                        )?;
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // for l.key !== sentinel where l.key is a union, we can't really prove
                    // that the check is guaranteed to fail (assuming the union doesn't
                    // degenerate to a singleton)
                    report_unchanged_filtering_result_to_predicate_result(
                        l.dupe(),
                        result_collector,
                    );
                }
            }
            _ => {
                let filter_result = type_filter::sentinel_refinement(
                    t,
                    reason.dupe(),
                    orig_obj.dupe(),
                    sense,
                    sentinel,
                );
                report_filtering_result_to_predicate_result(filter_result, result_collector);
            }
        }
    }
    Ok(())
}

fn eq_test<'cx>(
    cx: &Context<'cx>,
    result_collector: &PredicateResultCollector,
    sense: bool,
    left: &Type,
    right: &Type,
) {
    let expected_loc = loc_of_t(right);
    match right.deref() {
        TypeInner::DefT(_, def_t) => match def_t.deref() {
            DefTInner::SingletonStrT { value, .. } => {
                let filtered = if sense {
                    type_filter::string_literal(expected_loc.dupe(), value.dupe(), left.dupe())
                } else {
                    type_filter::not_string_literal(value.dupe(), left.dupe())
                };
                report_filtering_result_to_predicate_result(filtered, result_collector);
            }
            DefTInner::SingletonNumT { value, .. } => {
                let filtered = if sense {
                    type_filter::number_literal(expected_loc.dupe(), value.clone(), left.dupe())
                } else {
                    type_filter::not_number_literal(value.clone(), left.dupe())
                };
                report_filtering_result_to_predicate_result(filtered, result_collector);
            }
            DefTInner::SingletonBoolT { value: true, .. } => {
                let filtered = if sense {
                    type_filter::true_(left.dupe())
                } else {
                    type_filter::not_true(left.dupe())
                };
                report_filtering_result_to_predicate_result(filtered, result_collector);
            }
            DefTInner::SingletonBoolT { value: false, .. } => {
                let filtered = if sense {
                    type_filter::false_(left.dupe())
                } else {
                    type_filter::not_false(left.dupe())
                };
                report_filtering_result_to_predicate_result(filtered, result_collector);
            }
            DefTInner::SingletonBigIntT { value, .. } => {
                let filtered = if sense {
                    type_filter::bigint_literal(expected_loc.dupe(), value.clone(), left.dupe())
                } else {
                    type_filter::not_bigint_literal(value.clone(), left.dupe())
                };
                report_filtering_result_to_predicate_result(filtered, result_collector);
            }
            DefTInner::VoidT => {
                let filtered = if sense {
                    type_filter::undefined(left.dupe())
                } else {
                    type_filter::not_undefined(cx, left.dupe())
                };
                report_filtering_result_to_predicate_result(filtered, result_collector);
            }
            DefTInner::NullT => {
                let filtered = if sense {
                    type_filter::null_filter(left.dupe())
                } else {
                    type_filter::not_null(cx, left.dupe())
                };
                report_filtering_result_to_predicate_result(filtered, result_collector);
            }
            _ => {
                report_unchanged_filtering_result_to_predicate_result(
                    left.dupe(),
                    result_collector,
                );
            }
        },
        _ => {
            report_unchanged_filtering_result_to_predicate_result(left.dupe(), result_collector);
        }
    }
}

// (**********)
// (* guards *)
// (**********)
fn concretize_and_guard_prop<'cx>(
    cx: &Context<'cx>,
    source: &Type,
    pred: &PropGuard,
    orig_obj: &Type,
    result_collector: &PredicateResultCollector,
) -> Result<(), FlowJsException> {
    let ts =
        FlowJs::possible_concrete_types_for_operators_checking(cx, reason_of_t(source), source)?;
    let all_changed = ts.iter().all(|t| guard_prop(cx, t, pred));
    if all_changed {
        report_changes_to_input(result_collector);
    } else {
        report_unchanged_filtering_result_to_predicate_result(orig_obj.dupe(), result_collector);
    }
    Ok(())
}

fn guard_prop<'cx>(cx: &Context<'cx>, source: &Type, pred: &PropGuard) -> bool {
    fn is_empty_and_changed(result: FilterResult) -> bool {
        match result.type_.deref() {
            TypeInner::DefT(_, def_t) => match def_t.deref() {
                DefTInner::EmptyT => result.changed,
                _ => false,
            },
            _ => false,
        }
    }

    match pred {
        PropGuard::PropGuardTruthy => is_empty_and_changed(type_filter::truthy(cx, source.dupe())),
        PropGuard::PropGuardNotTruthy => {
            is_empty_and_changed(type_filter::not_truthy(cx, source.dupe()))
        }
        PropGuard::PropGuardMaybe => is_empty_and_changed(type_filter::maybe(cx, source.dupe())),
        PropGuard::PropGuardNotMaybe => {
            is_empty_and_changed(type_filter::not_maybe(cx, source.dupe()))
        }
        PropGuard::PropGuardNull => is_empty_and_changed(type_filter::null_filter(source.dupe())),
        PropGuard::PropGuardNotNull => {
            is_empty_and_changed(type_filter::not_null(cx, source.dupe()))
        }
        PropGuard::PropGuardVoid => is_empty_and_changed(type_filter::undefined(source.dupe())),
        PropGuard::PropGuardNotVoid => {
            is_empty_and_changed(type_filter::not_undefined(cx, source.dupe()))
        }
    }
}

pub enum PredicateResult {
    TypeChanged(Type),
    TypeUnchanged(Type),
}

pub fn run_predicate_track_changes<'cx>(
    cx: &Context<'cx>,
    t: &Type,
    p: &Predicate,
    result_reason: Reason,
) -> PredicateResult {
    let collector = TypeCollector::create();
    let changed = Rc::new(RefCell::new(false));
    let result_collector = PredicateResultCollector {
        collector,
        changed: changed.dupe(),
    };
    let p_clone = p.dupe();
    concretize_and_run_predicate(
        cx,
        DepthTrace::unit_trace(),
        t,
        concretization_variant_of_predicate(p),
        &result_collector,
        &|cx, trace, rc, l| predicate_no_concretization(cx, trace, rc, l, &p_clone),
    )
    .expect("Non speculating");
    let collected: Vec<Type> = result_collector
        .collector
        .collect()
        .into_iter()
        .map(|t| {
            flow_typing_flow_js::tvar_resolver::resolved_t(
                flow_typing_flow_js::tvar_resolver::default_no_lowers,
                true,
                cx,
                t,
            )
        })
        .collect();
    let result_t = type_util::union_of_ts(result_reason, collected, None);
    if *changed.borrow() {
        PredicateResult::TypeChanged(result_t)
    } else {
        PredicateResult::TypeUnchanged(result_t)
    }
}

pub fn run_predicate_for_filtering<'cx>(cx: &Context<'cx>, t: &Type, p: &Predicate, tout: &Tvar) {
    let collector = TypeCollector::create();
    let changed = Rc::new(RefCell::new(false));
    let result_collector = PredicateResultCollector {
        collector,
        changed: changed.dupe(),
    };
    let p_clone = p.dupe();
    concretize_and_run_predicate(
        cx,
        DepthTrace::unit_trace(),
        t,
        concretization_variant_of_predicate(p),
        &result_collector,
        &|cx, trace, rc, l| predicate_no_concretization(cx, trace, rc, l, &p_clone),
    )
    .expect("Non speculating");
    let tout_type = Type::new(TypeInner::OpenT(tout.dupe()));
    let collected: Vec<Type> = result_collector.collector.collect().into_iter().collect();
    for t in collected.iter() {
        FlowJs::flow_t(cx, t, &tout_type).expect("Non speculating");
    }
}
