/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::rc::Rc;
use std::sync::Arc;

use dupe::Dupe;
use dupe::OptionDupedExt;
use flow_aloc::ALoc;
use flow_common::reason::Name;
use flow_common::reason::Reason;
use flow_common::reason::ReasonDesc;
use flow_common::reason::VirtualReasonDesc;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_data_structure_wrapper::vector::FlowVector;
use flow_env_builder::env_api::CacheableEnvError;
use flow_env_builder::env_api::DefLocType;
use flow_env_builder::env_api::EnvEntry;
use flow_env_builder::env_api::EnvInfo;
use flow_env_builder::env_api::EnvKey;
use flow_env_builder::env_api::EnvSet;
use flow_env_builder::env_api::Read as EnvRead;
use flow_env_builder::env_api::Refinement;
use flow_env_builder::env_api::RefinementKind;
use flow_env_builder::env_api::ValKind;
use flow_env_builder::env_api::WriteLoc;
use flow_env_builder::name_def_types::ScopeKind;
use flow_env_builder::provider_api;
use flow_parser::loc_sig::LocSig;
use flow_typing_context::Context;
use flow_typing_context::PossiblyRefinedWriteState;
use flow_typing_context::TypingMode;
use flow_typing_flow_js::flow_js;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_flow_js::tvar_resolver;
use flow_typing_key::Key;
use flow_typing_loc_env::loc_env::LocEnv;
use flow_typing_loc_env::loc_env::TypeEntry;
use flow_typing_type::type_::ClassBinding;
use flow_typing_type::type_::ConstrainedAssignmentData;
use flow_typing_type::type_::HintEvalResult;
use flow_typing_type::type_::LazyHintT;
use flow_typing_type::type_::PredFuncallInfo;
use flow_typing_type::type_::Predicate;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::UseOp;

use crate::predicate_kit;
use crate::type_operation_utils;

/// lookup modes:
///
/// - ForValue is a lookup from a syntactic value location, i.e. standard JS code
/// - ForType is a lookup from a syntactic type location,
///   e.g. annotations, interface declarations etc.
/// - ForTypeof is a lookup from a typeof expression (necessarily in a type location)
///
/// Rules:
///
/// 1. ForValue lookups give errors if they retrieve type aliases (note: we
///    have a single namespace, so any name resolves uniquely to either a
///    value or type)
///
/// 2. ForValue lookups give errors if they forward reference non-hoisted
///    things (lets or consts)
///
/// 3. ForType lookups may return values or type aliases, since some values
///    also denote types - e.g. a generator function F also denotes the type
///    of the objects it creates. Of course many values don't also have a type
///    denotation and thus errors in type position. But we don't know the type
///    of a symbol during local inference as a rule, so errors of this kind are
///    not raised here.
///
/// 4. ForTypeof lookups are in fact ForValue lookups, but due to the order in
///    which AST traversal takes place, these lookups may legitimately violate
///    rule #2, hence the need for a special mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LookupMode {
    ForValue,
    ForType,
    ForTypeof,
}

pub fn get_class_entries<'cx>(cx: &Context<'cx>) -> Vec<ClassBinding> {
    let env = cx.environment();
    let class_stack = &env.class_stack;
    let class_bindings = &env.class_bindings;
    let mut lst = Vec::new();
    for l in class_stack {
        if let Some(c) = class_bindings.get(&EnvKey::new(DefLocType::OrdinaryNameLoc, l.dupe())) {
            lst.push(c.clone());
        }
    }
    lst
}

pub fn with_class_stack<'cx, A>(
    cx: &Context<'cx>,
    class_stack: FlowVector<ALoc>,
    f: impl FnOnce() -> A,
) -> A {
    let old_class_stack = {
        let mut env = cx.environment_mut();
        std::mem::replace(&mut env.class_stack, class_stack)
    };
    let res = f();
    let mut env = cx.environment_mut();
    env.class_stack = old_class_stack;
    res
}

pub fn has_hint<'cx>(cx: &Context<'cx>, loc: ALoc) -> bool {
    if !matches!(&*cx.typing_mode(), TypingMode::CheckingMode) {
        return false;
    }
    let env = cx.environment();
    env.hint_map
        .get(&EnvKey::new(DefLocType::OrdinaryNameLoc, loc))
        .map(|h| h.0)
        .unwrap_or(false)
}

pub fn get_hint<'cx>(cx: &Context<'cx>, loc: ALoc) -> LazyHintT<Context<'cx>> {
    if !matches!(&*cx.typing_mode(), TypingMode::CheckingMode) {
        return LazyHintT(
            false,
            Rc::new(|_cx: &Context<'_>, _e, _s, _r| HintEvalResult::NoHint),
        );
    }
    let env = cx.environment();
    env.hint_map
        .get(&EnvKey::new(DefLocType::OrdinaryNameLoc, loc))
        .cloned()
        .unwrap_or_else(|| {
            LazyHintT(
                false,
                Rc::new(|_cx: &Context<'_>, _e, _s, _r| HintEvalResult::NoHint),
            )
        })
}

pub fn set_scope_kind<'cx>(cx: &Context<'cx>, k: ScopeKind) -> ScopeKind {
    let mut env = cx.environment_mut();
    let old = env.scope_kind;
    env.scope_kind = k;
    old
}

pub fn in_class_scope<'cx, A>(cx: &Context<'cx>, loc: ALoc, f: impl FnOnce() -> A) -> A {
    let old_stack = {
        let mut env = cx.environment_mut();
        let old_stack = env.class_stack.clone();
        env.class_stack.push(loc);
        old_stack
    };
    let res = f();
    let mut env = cx.environment_mut();
    env.class_stack = old_stack;
    res
}

fn is_var_kind<'cx>(cx: &Context<'cx>, k: ScopeKind) -> bool {
    cx.environment().scope_kind == k
}

pub fn in_async_scope<'cx>(cx: &Context<'cx>) -> bool {
    is_var_kind(cx, ScopeKind::Async)
        || is_var_kind(cx, ScopeKind::AsyncGenerator)
        || is_var_kind(cx, ScopeKind::AsyncComponentOrHookBody)
}

pub fn var_scope_kind<'cx>(cx: &Context<'cx>) -> ScopeKind {
    cx.environment().scope_kind
}

pub fn in_global_scope<'cx>(cx: &Context<'cx>) -> bool {
    is_var_kind(cx, ScopeKind::Global)
}

pub fn in_toplevel_scope<'cx>(cx: &Context<'cx>) -> bool {
    is_var_kind(cx, ScopeKind::Module)
}

pub fn is_provider<'cx>(cx: &Context<'cx>, id_loc: ALoc) -> bool {
    let env = cx.environment();
    env.var_info.providers.is_provider(&id_loc)
}

#[cfg(not(debug_assertions))]
fn with_debug_exn<'cx>(_cx: &Context<'cx>, _loc: ALoc, f: impl FnOnce() -> Type) -> Type {
    f()
}

// We don't want the new-env to throw if we encounter some new case in the wild for which we did
// not adequately prepare. Instead, we return `any` in prod mode, but still crash in build mode.
#[cfg(debug_assertions)]
fn with_debug_exn<'cx>(cx: &Context<'cx>, loc: ALoc, f: impl FnOnce() -> Type) -> Type {
    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_errors::error_message::InternalError;
    use flow_typing_type::type_::AnySource;
    use flow_typing_type::type_::any_t;
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(t) => t,
        Err(e) => {
            if cfg!(debug_assertions) {
                std::panic::resume_unwind(e);
            } else {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EInternal(Box::new((
                        loc.dupe(),
                        InternalError::MissingEnvWrite(loc.dupe()),
                    ))),
                );
                any_t::at(AnySource::AnyError(None), loc)
            }
        }
    }
}

#[cfg(not(debug_assertions))]
fn with_debug_exn_error<'cx, A: 'static>(
    _cx: &Context<'cx>,
    _loc: ALoc,
    f: impl FnOnce() -> A,
    _error: impl FnOnce() -> A,
) -> A {
    f()
}

#[cfg(debug_assertions)]
fn with_debug_exn_error<'cx, A: 'static>(
    cx: &Context<'cx>,
    loc: ALoc,
    f: impl FnOnce() -> A,
    error: impl FnOnce() -> A,
) -> A {
    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_errors::error_message::InternalError;
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(a) => a,
        Err(e) => {
            if cfg!(debug_assertions) {
                std::panic::resume_unwind(e);
            } else {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EInternal(Box::new((
                        loc.dupe(),
                        InternalError::MissingEnvWrite(loc),
                    ))),
                );
                error()
            }
        }
    }
}

pub fn t_option_value_exn<'cx>(cx: &Context<'cx>, loc: ALoc, t: Option<Type>) -> Type {
    with_debug_exn(cx, loc.dupe(), || match t {
        Some(t) => t,
        None => panic!("Missing location entry: {}", loc.debug_to_string(true)),
    })
}

// ***********
// * Helpers *
// ***********

pub fn checked_find_loc_env_write_opt<'cx>(
    cx: &Context<'cx>,
    kind: DefLocType,
    loc: ALoc,
) -> Option<Type> {
    let env = cx.environment();
    let entry_key = EnvKey::new(kind, loc.dupe());
    let env_entry = env.var_info.env_entries.get(&entry_key).cloned();
    let write = env.find_write(kind, loc.dupe());
    match (env_entry, write) {
        (Some(EnvEntry::NonAssigningWrite), None) => None,
        (Some(EnvEntry::NonAssigningWrite), Some(TypeEntry { t, .. })) => Some(t.dupe()),
        (_, None) => {
            use flow_typing_errors::error_message::ErrorMessage;
            use flow_typing_errors::error_message::InternalError;
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EInternal(Box::new((
                    loc.dupe(),
                    InternalError::MissingEnvWrite(loc),
                ))),
            );
            None
        }
        (_, Some(TypeEntry { t, .. })) => Some(t.dupe()),
    }
}

pub fn checked_find_loc_env_write<'cx>(cx: &Context<'cx>, kind: DefLocType, loc: ALoc) -> Type {
    let t = checked_find_loc_env_write_opt(cx, kind, loc.dupe());
    t_option_value_exn(cx, loc, t)
}

fn find_var_opt<'a>(var_info: &'a EnvInfo<ALoc>, loc: &ALoc) -> Result<&'a EnvRead<ALoc>, ALoc> {
    match var_info.env_values.get(loc) {
        Some(x) => Ok(x),
        None => Err(loc.dupe()),
    }
}

pub struct LocalExportBinding {
    pub def_loc: Option<ALoc>,
    pub val_kind: ValKind,
}

pub fn local_export_binding_at_loc<'cx>(
    cx: &Context<'cx>,
    loc: ALoc,
) -> Option<LocalExportBinding> {
    let env = cx.environment();
    let var_info = &env.var_info;
    match find_var_opt(var_info, &loc) {
        Ok(EnvRead {
            def_loc, val_kind, ..
        }) => Some(LocalExportBinding {
            def_loc: def_loc.clone(),
            val_kind: *val_kind,
        }),
        Err(_) => None,
    }
}

fn find_refi(var_info: &EnvInfo<ALoc>, id: i32) -> Refinement<ALoc> {
    (var_info.refinement_of_id)(id)
}

fn find_providers(var_info: &EnvInfo<ALoc>, loc: &ALoc) -> Vec<ALoc> {
    var_info
        .providers
        .providers_of_def(loc)
        .map(|def_providers| {
            def_providers
                .providers
                .iter()
                .map(|p| p.reason.loc().dupe())
                .collect()
        })
        .unwrap_or_default()
}

fn is_def_loc_annotated(var_info: &EnvInfo<ALoc>, loc: &ALoc) -> bool {
    use flow_env_builder::find_providers::State;
    match var_info.providers.providers_of_def(loc) {
        Some(provider_api::DefProviders {
            state: State::AnnotatedVar { .. },
            ..
        }) => true,
        _ => false,
    }
}

pub fn provider_type_for_def_loc<'cx>(
    intersect: bool,
    cx: &Context<'cx>,
    env: &LocEnv<'cx, Context<'cx>>,
    def_loc: ALoc,
) -> Type {
    let var_info = &env.var_info;
    let providers: Vec<Type> = find_providers(var_info, &def_loc)
        .into_iter()
        .filter_map(|loc| {
            match checked_find_loc_env_write_opt(cx, DefLocType::OrdinaryNameLoc, loc.dupe()) {
                Some(w) => Some(w),
                None => match var_info.env_entries.get_ordinary(&loc) {
                    None | Some(EnvEntry::NonAssigningWrite) => None,
                    _ => {
                        use flow_env_builder::env_api::EnvInvariantFailure;
                        use flow_typing_errors::error_message::ErrorMessage;
                        use flow_typing_errors::error_message::InternalError;
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EInternal(Box::new((
                                loc.dupe(),
                                InternalError::EnvInvariant(EnvInvariantFailure::Impossible(
                                    format!(
                                        "Missing provider write at {} for {}",
                                        flow_common::reason::string_of_aloc(None, &loc),
                                        flow_common::reason::string_of_aloc(None, &def_loc),
                                    )
                                    .into(),
                                )),
                            ))),
                        );
                        None
                    }
                },
            }
        })
        .collect();

    use flow_common::reason::mk_reason;
    use flow_typing_type::type_::TypeInner;
    use flow_typing_type::type_::inter_rep;
    use flow_typing_type::type_::mixed_t;
    use flow_typing_type::type_::union_rep;
    match providers.len() {
        0 => mixed_t::make(mk_reason(VirtualReasonDesc::RNoProviders, def_loc)),
        1 => providers.into_iter().next().unwrap(),
        _ => {
            let mut iter = providers.into_iter();
            let t1 = iter.next().unwrap();
            let t2 = iter.next().unwrap();
            let ts: Vec<Type> = iter.collect();
            let reason = mk_reason(VirtualReasonDesc::RProviders, def_loc);
            if intersect {
                Type::new(TypeInner::IntersectionT(
                    reason,
                    inter_rep::make(t1, t2, ts.into()),
                ))
            } else {
                Type::new(TypeInner::UnionT(
                    reason,
                    union_rep::make(None, union_rep::UnionKind::ProvidersKind, t1, t2, ts.into()),
                ))
            }
        }
    }
}

// *************
// *  Reading  *
// *************

fn merge_actually_refined_refining_locs(
    pair: (Option<FlowOrdSet<ALoc>>, Option<FlowOrdSet<ALoc>>),
) -> Option<FlowOrdSet<ALoc>> {
    match pair {
        (None, l @ Some(_)) | (l @ Some(_), None) => l,
        (None, None) => None,
        (Some(l1), Some(l2)) => Some(l1.union(l2)),
    }
}

/// Computes the phi type for a node given all its lower bounds
/// Currently, this just produces a new type variable with the types of
/// all the incoming writes as lower bounds. In the future, however, we
/// may want to compute a more specific least upper bound for these writes.
fn phi<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    states: Vec<PossiblyRefinedWriteState>,
) -> PossiblyRefinedWriteState {
    match states.len() {
        1 => states.into_iter().next().unwrap(),
        _ => {
            let tvar = flow_typing_tvar::mk(cx, reason.dupe());
            let mut actually_refined_refining_locs_ref: Option<FlowOrdSet<ALoc>> = None;
            let mut errors = Vec::new();
            for state in states {
                let PossiblyRefinedWriteState {
                    t,
                    errors: state_errors,
                    actually_refined_refining_locs,
                } = state;
                flow_js::flow_t_non_speculating(cx, (&t, &tvar));
                actually_refined_refining_locs_ref = merge_actually_refined_refining_locs((
                    actually_refined_refining_locs_ref,
                    actually_refined_refining_locs,
                ));
                errors.extend(state_errors);
            }
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &tvar);
            PossiblyRefinedWriteState {
                t: tvar,
                errors,
                actually_refined_refining_locs: actually_refined_refining_locs_ref,
            }
        }
    }
}

fn read_pred_func_info_exn<'cx>(
    cx: &Context<'cx>,
    loc: ALoc,
) -> Box<flow_typing_type::type_::PredFuncallInfo> {
    use flow_typing_type::type_::AnySource;
    use flow_typing_type::type_::PredFuncallInfo;
    use flow_typing_type::type_::any_t;
    use flow_typing_type::type_::unknown_use;
    let loc_clone = loc.dupe();
    let loc_err = loc.dupe();

    with_debug_exn_error(
        cx,
        loc,
        move || {
            let lazy_info = {
                let env = cx.environment();
                env.pred_func_map
                    .get(&loc_clone)
                    .unwrap_or_else(|| {
                        panic!(
                            "pred_func_map key not found: {}",
                            loc_clone.debug_to_string(true)
                        )
                    })
                    .dupe()
            };
            Box::new(lazy_info.get_forced(cx).clone())
        },
        move || {
            Box::new(PredFuncallInfo(
                unknown_use(),
                loc_err.dupe(),
                any_t::at(AnySource::AnyError(None), loc_err),
                None,
                Rc::from([]),
            ))
        },
    )
}

/// Returns [true] iff the input type is potentially a predicate function.
fn maybe_predicate_function<'cx>(cx: &Context<'cx>, t: &Type) -> bool {
    use std::ops::Deref;

    use flow_typing_type::type_::DefTInner;
    use flow_typing_type::type_::PolyTData;
    use flow_typing_type::type_::TypeInner;

    fn simplify_callee<'cx>(cx: &Context<'cx>, func_t: &Type) -> Type {
        let errors = cx.errors();
        let result = cx.run_and_rolled_back_cache(|| {
            let reason = flow_typing_type::type_util::reason_of_t(func_t);
            flow_typing_tvar::mk_no_wrap_where(cx, reason.dupe(), |cx, r, tvar_id| {
                use flow_typing_type::type_::Tvar;
                use flow_typing_type::type_::UseT;
                use flow_typing_type::type_::UseTInner;
                let u = UseT::new(UseTInner::CallT(Box::new(
                    flow_typing_type::type_::CallTData {
                        use_op: flow_typing_type::type_::unknown_use(),
                        reason: reason.dupe(),
                        call_action: Box::new(
                            flow_typing_type::type_::CallAction::ConcretizeCallee(Tvar::new(
                                r.dupe(),
                                tvar_id as u32,
                            )),
                        ),
                        return_hint: flow_typing_type::type_::hint_unavailable(),
                    },
                )));
                flow_js::flow_non_speculating(cx, (func_t, &u));
            })
        });
        cx.reset_errors(errors);
        result
    }

    fn on_ground(t: &Type) -> bool {
        match t.deref() {
            TypeInner::AnyT(_, _) => false,
            TypeInner::DefT(_, def_t) => match def_t.deref() {
                DefTInner::FunT(_, fun_t) => fun_t.type_guard.is_some(),
                DefTInner::PolyT(box PolyTData { t_out, .. }) => match t_out.deref() {
                    TypeInner::DefT(_, inner_def_t) => match inner_def_t.deref() {
                        DefTInner::FunT(_, fun_t) => fun_t.type_guard.is_some(),
                        _ => false,
                    },
                    _ => false,
                },
                _ => false,
            },
            _ => true, // yes: safe option
        }
    }

    fn on_non_inter<'cx>(cx: &Context<'cx>, t: &Type) -> bool {
        match t.deref() {
            TypeInner::DefT(_, _) => on_ground(t),
            _ => {
                let simplified = simplify_callee(cx, t);
                match cx.find_resolved(&simplified) {
                    Some(resolved) => on_ground(&resolved),
                    None => true,
                }
            }
        }
    }

    fn on_concrete<'cx>(cx: &Context<'cx>, t: &Type) -> bool {
        match t.deref() {
            TypeInner::IntersectionT(_, rep) => rep.members_iter().any(|m| on_t(cx, m)),
            _ => on_non_inter(cx, t),
        }
    }

    fn on_t<'cx>(cx: &Context<'cx>, t: &Type) -> bool {
        let reason = flow_typing_type::type_util::reason_of_t(t);
        FlowJs::possible_concrete_types_for_inspection(cx, reason, t)
            .expect("Non speculating")
            .iter()
            .any(|ct| on_concrete(cx, ct))
    }

    on_t(cx, t)
}

fn predicate_of_refinement<'cx>(
    cx: &Context<'cx>,
    kind: &RefinementKind<ALoc>,
) -> Option<Predicate> {
    use flow_common::reason::Name;
    use flow_common::reason::mk_reason;
    use flow_env_builder::env_api::InstanceofContext;
    use flow_typing_type::type_::ArrayLengthOp;
    use flow_typing_type::type_::BinaryTest;
    use flow_typing_type::type_::PredicateInner;

    fn pred<'cx>(cx: &Context<'cx>, kind: &RefinementKind<ALoc>) -> Option<Predicate> {
        match kind {
            RefinementKind::AndR(r1, r2) => match (pred(cx, r1), pred(cx, r2)) {
                (Some(p1), Some(p2)) => Some(Predicate::new(PredicateInner::AndP(p1, p2))),
                (Some(p), None) | (None, Some(p)) => Some(p),
                (None, None) => None,
            },
            RefinementKind::OrR(r1, r2) => match (pred(cx, r1), pred(cx, r2)) {
                (Some(p1), Some(p2)) => Some(Predicate::new(PredicateInner::OrP(p1, p2))),
                _ => None,
            },
            RefinementKind::NotR(r) => pred(cx, r).map(|p| Predicate::new(PredicateInner::NotP(p))),
            RefinementKind::TruthyR => Some(Predicate::new(PredicateInner::TruthyP)),
            RefinementKind::NullR => Some(Predicate::new(PredicateInner::NullP)),
            RefinementKind::UndefinedR => Some(Predicate::new(PredicateInner::VoidP)),
            RefinementKind::MaybeR => Some(Predicate::new(PredicateInner::MaybeP)),
            RefinementKind::InstanceOfR { expr, context } => {
                let loc = expr.loc().dupe();
                flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                    vec![format!(
                        "reading from location {} (in instanceof refinement)",
                        flow_common::reason::string_of_aloc(None, &loc)
                    )]
                });
                let t = checked_find_loc_env_write(cx, DefLocType::ExpressionLoc, loc.dupe());
                match context {
                    InstanceofContext::InstanceOfExpr => {
                        type_operation_utils::type_assertions::assert_instanceof_rhs(cx, &t);
                    }
                    InstanceofContext::MatchInstancePattern => {
                        type_operation_utils::type_assertions::assert_match_instance_pattern_constructor(cx, &t);
                    }
                }
                Some(Predicate::new(PredicateInner::BinaryP(
                    BinaryTest::InstanceofTest,
                    t,
                )))
            }
            RefinementKind::IsArrayR => Some(Predicate::new(PredicateInner::ArrP)),
            RefinementKind::ArrLenR { op, n } => {
                let op = match op {
                    flow_env_builder::env_api::ArrayLengthOp::ArrLenEqual => {
                        ArrayLengthOp::ArrLenEqual
                    }
                    flow_env_builder::env_api::ArrayLengthOp::ArrLenGreaterThanEqual => {
                        ArrayLengthOp::ArrLenGreaterThanEqual
                    }
                };
                Some(Predicate::new(PredicateInner::ArrLenP { op, n: *n }))
            }
            RefinementKind::BoolR(loc) => {
                Some(Predicate::new(PredicateInner::BoolP(Box::new(loc.dupe()))))
            }
            RefinementKind::FunctionR => Some(Predicate::new(PredicateInner::FunP)),
            RefinementKind::NumberR(loc) => {
                Some(Predicate::new(PredicateInner::NumP(Box::new(loc.dupe()))))
            }
            RefinementKind::BigIntR(loc) => Some(Predicate::new(PredicateInner::BigIntP(
                Box::new(loc.dupe()),
            ))),
            RefinementKind::ObjectR => Some(Predicate::new(PredicateInner::ObjP)),
            RefinementKind::StringR(loc) => {
                Some(Predicate::new(PredicateInner::StrP(Box::new(loc.dupe()))))
            }
            RefinementKind::SymbolR(loc) => Some(Predicate::new(PredicateInner::SymbolP(
                Box::new(loc.dupe()),
            ))),
            RefinementKind::SingletonBoolR { loc, sense: _, lit } => Some(Predicate::new(
                PredicateInner::SingletonBoolP(Box::new((loc.dupe(), *lit))),
            )),
            RefinementKind::SingletonStrR { loc, sense, lit } => {
                Some(Predicate::new(PredicateInner::SingletonStrP(Box::new((
                    loc.dupe(),
                    *sense,
                    lit.as_str().to_string(),
                )))))
            }
            RefinementKind::SingletonNumR { loc, sense, lit } => {
                use flow_typing_type::type_::NumberLiteral;
                Some(Predicate::new(PredicateInner::SingletonNumP(Box::new((
                    loc.dupe(),
                    *sense,
                    NumberLiteral(lit.0, lit.1.dupe()),
                )))))
            }
            RefinementKind::SingletonBigIntR { loc, sense, lit } => {
                use flow_typing_type::type_::BigIntLiteral;
                Some(Predicate::new(PredicateInner::SingletonBigIntP(Box::new(
                    (loc.dupe(), *sense, BigIntLiteral(lit.0, lit.1.dupe())),
                ))))
            }
            RefinementKind::SentinelR { prop, other_loc } => {
                flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                    vec![format!(
                        "reading from location {} (in sentinel refinement)",
                        flow_common::reason::string_of_aloc(None, other_loc)
                    )]
                });
                let other_t =
                    checked_find_loc_env_write(cx, DefLocType::ExpressionLoc, other_loc.dupe());
                Some(Predicate::new(PredicateInner::BinaryP(
                    BinaryTest::SentinelProp(prop.dupe()),
                    other_t,
                )))
            }
            RefinementKind::EqR(loc) => {
                flow_typing_debug::verbose::print_if_verbose_lazy(cx, None, None, None, || {
                    vec![format!(
                        "reading from location {} (in eq refinement)",
                        flow_common::reason::string_of_aloc(None, loc)
                    )]
                });
                let other_t = checked_find_loc_env_write(cx, DefLocType::ExpressionLoc, loc.dupe());
                Some(Predicate::new(PredicateInner::BinaryP(
                    BinaryTest::EqTest,
                    other_t,
                )))
            }
            RefinementKind::LatentR {
                func,
                index,
                targs: _,
                arguments: _,
            } => {
                let func_loc = func.loc().dupe();
                let lazy_info = {
                    let env = cx.environment();
                    env.pred_func_map.get(&func_loc).duped()
                };
                match lazy_info {
                    Some(info) => {
                        let PredFuncallInfo(_, _, t, _, _) = info.get_forced(cx);
                        if maybe_predicate_function(cx, t) {
                            Some(Predicate::new(PredicateInner::LatentP(
                                read_pred_func_info_exn(cx, func_loc),
                                index.dupe(),
                            )))
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            RefinementKind::LatentThisR {
                func,
                targs: _,
                arguments: _,
            } => {
                let func_loc = func.loc().dupe();
                let lazy_info = {
                    let env = cx.environment();
                    env.pred_func_map.get(&func_loc).duped()
                };
                match lazy_info {
                    Some(info) => {
                        let PredFuncallInfo(_, _, t, _, _) = info.get_forced(cx);
                        if maybe_predicate_function(cx, t) {
                            Some(Predicate::new(PredicateInner::LatentThisP(
                                read_pred_func_info_exn(cx, func_loc),
                            )))
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            RefinementKind::PropNullishR { propname, loc } => {
                let reason = mk_reason(
                    VirtualReasonDesc::RProperty(Some(Name::new(propname.dupe()))),
                    loc.dupe(),
                );
                Some(Predicate::new(PredicateInner::NotP(Predicate::new(
                    PredicateInner::PropNonMaybeP(propname.dupe(), reason),
                ))))
            }
            RefinementKind::PropIsExactlyNullR { propname, loc } => {
                let reason = mk_reason(
                    VirtualReasonDesc::RProperty(Some(Name::new(propname.dupe()))),
                    loc.dupe(),
                );
                Some(Predicate::new(PredicateInner::PropIsExactlyNullP(
                    propname.dupe(),
                    reason,
                )))
            }
            RefinementKind::PropNonVoidR { propname, loc } => {
                let reason = mk_reason(
                    VirtualReasonDesc::RProperty(Some(Name::new(propname.dupe()))),
                    loc.dupe(),
                );
                Some(Predicate::new(PredicateInner::PropNonVoidP(
                    propname.dupe(),
                    reason,
                )))
            }
            RefinementKind::PropTruthyR { propname, loc } => {
                let reason = mk_reason(
                    VirtualReasonDesc::RProperty(Some(Name::new(propname.dupe()))),
                    loc.dupe(),
                );
                Some(Predicate::new(PredicateInner::PropTruthyP(
                    propname.dupe(),
                    reason,
                )))
            }
            RefinementKind::PropExistsR { propname, loc } => {
                let reason = mk_reason(
                    VirtualReasonDesc::RProperty(Some(Name::new(propname.dupe()))),
                    loc.dupe(),
                );
                Some(Predicate::new(PredicateInner::PropExistsP {
                    propname: propname.dupe(),
                    reason,
                }))
            }
            RefinementKind::ImpossibleR => Some(Predicate::new(PredicateInner::ImpossibleP)),
        }
    }
    pred(cx, kind)
}

fn refine<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    loc: ALoc,
    refi: Option<&Refinement<ALoc>>,
    res: PossiblyRefinedWriteState,
) -> PossiblyRefinedWriteState {
    use flow_common::reason::mk_reason;

    match refi {
        None => res,
        Some(refinement) => {
            let Refinement {
                refining_locs,
                kind,
            } = refinement;
            let PossiblyRefinedWriteState {
                t,
                errors,
                actually_refined_refining_locs,
            } = res;
            let predicate = predicate_of_refinement(cx, kind);
            let refined_reason = mk_reason(
                VirtualReasonDesc::RRefined(Arc::new(reason.desc(true).clone())),
                loc,
            );
            let (t, actually_refined_refining_locs_prime) = match predicate {
                None => (t, None),
                Some(predicate) => {
                    match predicate_kit::run_predicate_track_changes(
                        cx,
                        &t,
                        &predicate,
                        refined_reason,
                    ) {
                        predicate_kit::PredicateResult::TypeUnchanged(_) => (t, None),
                        predicate_kit::PredicateResult::TypeChanged(new_t) => {
                            (new_t, Some(refining_locs.dupe()))
                        }
                    }
                }
            };
            PossiblyRefinedWriteState {
                t,
                errors,
                actually_refined_refining_locs: merge_actually_refined_refining_locs((
                    actually_refined_refining_locs,
                    actually_refined_refining_locs_prime,
                )),
            }
        }
    }
}

fn possibly_refined_write_state_of_state<'cx>(
    lookup_mode: LookupMode,
    val_kind: ValKind,
    cx: &Context<'cx>,
    loc: ALoc,
    reason: Reason,
    write_locs: &[WriteLoc<ALoc>],
    val_id: Option<i32>,
    refi: Option<&Refinement<ALoc>>,
) -> PossiblyRefinedWriteState {
    fn base(t: Type, errors: Vec<CacheableEnvError<ALoc>>) -> PossiblyRefinedWriteState {
        PossiblyRefinedWriteState {
            t,
            errors,
            actually_refined_refining_locs: None,
        }
    }

    fn loop_fn<'cx>(
        lookup_mode: LookupMode,
        cx: &Context<'cx>,
        loc: ALoc,
        reason: Reason,
        states: &[WriteLoc<ALoc>],
        val_id: Option<i32>,
        refi: Option<&Refinement<ALoc>>,
    ) -> PossiblyRefinedWriteState {
        use flow_common::reason::mk_reason;
        use flow_typing_type::type_::AnySource;
        use flow_typing_type::type_::TypeInner;
        use flow_typing_type::type_::any_t;

        let find_write_exn = |kind: DefLocType, reason: &Reason| -> Type {
            let wloc = reason.loc().dupe();
            checked_find_loc_env_write(cx, kind, wloc)
        };

        let compute_state = || {
            let mapped: Vec<PossiblyRefinedWriteState> = states
                .iter()
                .map(|entry| match (entry, lookup_mode) {
                    (WriteLoc::Undefined(r) | WriteLoc::Uninitialized(r), _) => {
                        base(flow_typing_type::type_::void::make(r.dupe()), vec![])
                    }
                    (WriteLoc::Number(r), _) => base(
                        flow_typing_type::type_::num_module_t::make(r.dupe()),
                        vec![],
                    ),
                    (WriteLoc::DeclaredFunction(dloc), _) => {
                        let env = cx.environment();
                        base(
                            provider_type_for_def_loc(true, cx, &env, dloc.dupe()),
                            vec![],
                        )
                    }
                    (WriteLoc::Undeclared(_, def_loc), LookupMode::ForType) => base(
                        checked_find_loc_env_write(cx, DefLocType::OrdinaryNameLoc, def_loc.dupe()),
                        vec![],
                    ),
                    (
                        WriteLoc::Undeclared(name, def_loc),
                        LookupMode::ForValue | LookupMode::ForTypeof,
                    ) => base(
                        any_t::make(AnySource::AnyError(None), reason.dupe()),
                        vec![CacheableEnvError::ReferencedBeforeDeclaration {
                            name: name.dupe(),
                            def_loc: def_loc.dupe(),
                        }],
                    ),
                    (WriteLoc::EmptyArray { reason: wr, .. } | WriteLoc::Write(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose_lazy(
                            cx,
                            None,
                            None,
                            None,
                            || {
                                vec![format!(
                                    "reading {} from location {}",
                                    flow_common::reason::string_of_aloc(None, &loc),
                                    flow_common::reason::string_of_aloc(None, wr.loc()),
                                )]
                            },
                        );
                        base(find_write_exn(DefLocType::OrdinaryNameLoc, wr), vec![])
                    }
                    (WriteLoc::IllegalWrite(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose_lazy(
                            cx,
                            None,
                            None,
                            None,
                            || {
                                vec![format!(
                                    "reading {} from illegal write location {}",
                                    flow_common::reason::string_of_aloc(None, &loc),
                                    flow_common::reason::string_of_aloc(None, wr.loc()),
                                )]
                            },
                        );
                        base(any_t::make(AnySource::AnyError(None), wr.dupe()), vec![])
                    }
                    (
                        WriteLoc::Refinement {
                            refinement_id,
                            writes,
                            write_id,
                        },
                        _,
                    ) => {
                        let new_refi = {
                            let env = cx.environment();
                            Some(find_refi(&env.var_info, *refinement_id))
                        };
                        loop_fn(
                            lookup_mode,
                            cx,
                            loc.dupe(),
                            reason.dupe(),
                            writes,
                            *write_id,
                            new_refi.as_ref(),
                        )
                    }
                    (WriteLoc::Global(name), LookupMode::ForValue | LookupMode::ForTypeof) => {
                        match flow_typing_flow_common::flow_js_utils::lookup_builtin_value_result(
                            cx,
                            name.as_str(),
                            reason.dupe(),
                        ) {
                            Ok(t) => base(t, vec![]),
                            Err((t, errs)) => base(t, errs),
                        }
                    }
                    (WriteLoc::Global(name), LookupMode::ForType) => {
                        match flow_typing_flow_common::flow_js_utils::lookup_builtin_type_result(
                            cx,
                            name.as_str(),
                            reason.dupe(),
                        ) {
                            Ok(t) => base(t, vec![]),
                            Err((t, errs)) => base(t, errs),
                        }
                    }
                    (WriteLoc::GlobalThis(r), _) => {
                        base(Type::new(TypeInner::ObjProtoT(r.dupe())), vec![])
                    }
                    (WriteLoc::IllegalThis(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose(
                            cx,
                            None,
                            None,
                            None,
                            vec![format!(
                                "reading illegal this({}) from location {}",
                                flow_common::reason::string_of_aloc(None, &loc),
                                flow_common::reason::string_of_aloc(None, wr.loc()),
                            )],
                        );
                        base(any_t::make(AnySource::AnyError(None), wr.dupe()), vec![])
                    }
                    (WriteLoc::FunctionThis(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose(
                            cx,
                            None,
                            None,
                            None,
                            vec![format!(
                                "reading function this({}) from location {}",
                                flow_common::reason::string_of_aloc(None, &loc),
                                flow_common::reason::string_of_aloc(None, wr.loc()),
                            )],
                        );
                        base(find_write_exn(DefLocType::FunctionThisLoc, wr), vec![])
                    }
                    (WriteLoc::ClassInstanceThis(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose(
                            cx,
                            None,
                            None,
                            None,
                            vec![format!(
                                "reading instance this({}) from location {}",
                                flow_common::reason::string_of_aloc(None, &loc),
                                flow_common::reason::string_of_aloc(None, wr.loc()),
                            )],
                        );
                        base(find_write_exn(DefLocType::ClassInstanceThisLoc, wr), vec![])
                    }
                    (WriteLoc::ClassStaticThis(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose(
                            cx,
                            None,
                            None,
                            None,
                            vec![format!(
                                "reading static this({}) from location {}",
                                flow_common::reason::string_of_aloc(None, &loc),
                                flow_common::reason::string_of_aloc(None, wr.loc()),
                            )],
                        );
                        base(find_write_exn(DefLocType::ClassStaticThisLoc, wr), vec![])
                    }
                    (WriteLoc::ClassInstanceSuper(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose(
                            cx,
                            None,
                            None,
                            None,
                            vec![format!(
                                "reading instance super({}) from location {}",
                                flow_common::reason::string_of_aloc(None, &loc),
                                flow_common::reason::string_of_aloc(None, wr.loc()),
                            )],
                        );
                        base(
                            find_write_exn(DefLocType::ClassInstanceSuperLoc, wr),
                            vec![],
                        )
                    }
                    (WriteLoc::ClassStaticSuper(wr), _) => {
                        flow_typing_debug::verbose::print_if_verbose(
                            cx,
                            None,
                            None,
                            None,
                            vec![format!(
                                "reading {} from illegal write location {}",
                                flow_common::reason::string_of_aloc(None, &loc),
                                flow_common::reason::string_of_aloc(None, wr.loc()),
                            )],
                        );
                        base(find_write_exn(DefLocType::ClassStaticSuperLoc, wr), vec![])
                    }
                    (WriteLoc::ModuleScoped(_), _) => {
                        base(any_t::at(AnySource::AnnotatedAny, loc.dupe()), vec![])
                    }
                    (WriteLoc::Unreachable(uloc), _) => {
                        let r = mk_reason(
                            VirtualReasonDesc::RCustom("unreachable value".into()),
                            uloc.dupe(),
                        );
                        base(flow_typing_type::type_::empty_t::make(r), vec![])
                    }
                    (WriteLoc::Projection(ploc), _) => base(
                        checked_find_loc_env_write(cx, DefLocType::OrdinaryNameLoc, ploc.dupe()),
                        vec![],
                    ),
                })
                .collect();
            phi(cx, reason.dupe(), mapped)
        };

        let state = match val_id {
            Some(id) => {
                if !matches!(&*cx.typing_mode(), TypingMode::CheckingMode) {
                    compute_state()
                } else {
                    let for_value =
                        matches!(lookup_mode, LookupMode::ForValue | LookupMode::ForTypeof);
                    match cx.env_cache_find_opt(for_value, id) {
                        None => {
                            let state = compute_state();
                            cx.add_env_cache_entry(for_value, id, state.clone());
                            state
                        }
                        Some(state) => state,
                    }
                }
            }
            None => compute_state(),
        };
        refine(cx, reason, loc, refi, state)
    }

    let state = loop_fn(
        lookup_mode,
        cx,
        loc.dupe(),
        reason,
        write_locs,
        val_id,
        refi,
    );
    tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &state.t);
    if let Some(ref locs) = state.actually_refined_refining_locs {
        if val_kind != ValKind::Internal {
            cx.add_refined_location(loc, locs.clone());
        }
    }
    state
}

fn type_of_state<'cx>(
    lookup_mode: LookupMode,
    val_kind: ValKind,
    cx: &Context<'cx>,
    loc: ALoc,
    reason: Reason,
    write_locs: &[WriteLoc<ALoc>],
    val_id: Option<i32>,
    refi: Option<&Refinement<ALoc>>,
) -> Type {
    let PossiblyRefinedWriteState {
        t,
        errors,
        actually_refined_refining_locs: _,
    } = possibly_refined_write_state_of_state(
        lookup_mode,
        val_kind,
        cx,
        loc.dupe(),
        reason,
        write_locs,
        val_id,
        refi,
    );
    for err in errors {
        flow_typing_flow_common::flow_js_utils::emit_cacheable_env_error(cx, loc.dupe(), err);
    }
    t
}

fn read_entry<'cx>(
    lookup_mode: LookupMode,
    cx: &Context<'cx>,
    loc: ALoc,
    reason: Reason,
) -> Result<Type, ALoc> {
    use flow_common::reason::Name;
    use flow_typing_errors::error_message::BindingError;
    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_type::type_::AnySource;
    use flow_typing_type::type_::any_t;

    let read = {
        let env = cx.environment();
        let var_info = &env.var_info;
        match find_var_opt(var_info, &loc) {
            Err(loc) => Err(loc),
            Ok(read) => Ok(read.clone()),
        }
    };
    match read {
        Err(loc) => Err(loc),
        Ok(EnvRead {
            def_loc,
            write_locs,
            val_kind,
            name,
            id,
        }) => match (&val_kind, &name, &def_loc, lookup_mode) {
            (
                ValKind::Type {
                    imported,
                    type_only_namespace,
                },
                Some(name),
                Some(def_loc),
                LookupMode::ForValue,
            ) => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EBindingError(Box::new((
                        BindingError::ETypeInValuePosition {
                            imported: *imported,
                            type_only_namespace: *type_only_namespace,
                            name: name.dupe(),
                        },
                        loc.dupe(),
                        Name::new(name.dupe()),
                        def_loc.dupe(),
                    ))),
                );
                Ok(any_t::at(AnySource::AnyError(None), loc))
            }
            (
                ValKind::Type {
                    imported: _,
                    type_only_namespace: true,
                },
                Some(_),
                Some(_),
                LookupMode::ForTypeof,
            ) => {
                let t = type_of_state(
                    lookup_mode,
                    val_kind,
                    cx,
                    loc,
                    reason,
                    &write_locs,
                    id,
                    None,
                );
                Ok(t)
            }
            (
                ValKind::Type {
                    imported,
                    type_only_namespace,
                },
                Some(name),
                Some(def_loc),
                LookupMode::ForTypeof,
            ) => {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EBindingError(Box::new((
                        BindingError::ETypeInValuePosition {
                            imported: *imported,
                            type_only_namespace: *type_only_namespace,
                            name: name.dupe(),
                        },
                        loc.dupe(),
                        Name::new(name.dupe()),
                        def_loc.dupe(),
                    ))),
                );
                Ok(any_t::at(AnySource::AnyError(None), loc))
            }
            (
                ValKind::TsImport,
                Some(name),
                Some(def_loc),
                LookupMode::ForValue | LookupMode::ForTypeof,
            ) => {
                let t = type_of_state(
                    lookup_mode,
                    val_kind,
                    cx,
                    loc.dupe(),
                    reason.dupe(),
                    &write_locs,
                    id,
                    None,
                );
                let ts_import_resolved_to_type_only = match cx.find_resolved(&t) {
                    Some(resolved) => {
                        use std::ops::Deref;

                        use flow_typing_type::type_::DefTInner;
                        use flow_typing_type::type_::PolyTData;
                        use flow_typing_type::type_::TypeInner;
                        match resolved.deref() {
                            TypeInner::DefT(_, def_t) => match def_t.deref() {
                                DefTInner::ClassT(_) => false,
                                DefTInner::PolyT(box PolyTData { t_out, .. })
                                    if matches!(t_out.deref(), TypeInner::DefT(_, d) if matches!(d.deref(), DefTInner::ClassT(_))) =>
                                {
                                    false
                                }
                                DefTInner::EnumObjectT { .. } => false,
                                DefTInner::ReactAbstractComponentT(_) => false,
                                _ => {
                                    flow_typing_flow_common::flow_js_utils::import_type_t_kit::canonicalize_imported_type(cx, reason.dupe(), &resolved).is_some()
                                }
                            },
                            _ => {
                                flow_typing_flow_common::flow_js_utils::import_type_t_kit::canonicalize_imported_type(cx, reason.dupe(), &resolved).is_some()
                            }
                        }
                    }
                    None => false,
                };
                if ts_import_resolved_to_type_only {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EBindingError(Box::new((
                            BindingError::ETypeInValuePosition {
                                imported: false,
                                type_only_namespace: false,
                                name: name.dupe(),
                            },
                            loc.dupe(),
                            Name::new(name.dupe()),
                            def_loc.dupe(),
                        ))),
                    );
                    Ok(any_t::at(AnySource::AnyError(None), loc))
                } else {
                    Ok(t)
                }
            }
            _ => {
                let t = type_of_state(
                    lookup_mode,
                    val_kind,
                    cx,
                    loc,
                    reason,
                    &write_locs,
                    id,
                    None,
                );
                Ok(t)
            }
        },
    }
}

fn read_entry_exn<'cx>(
    lookup_mode: LookupMode,
    cx: &Context<'cx>,
    loc: ALoc,
    reason: Reason,
) -> Type {
    let loc_clone = loc.dupe();
    with_debug_exn(cx, loc, move || {
        match read_entry(lookup_mode, cx, loc_clone.dupe(), reason) {
            Err(err_loc) => panic!(
                "LocEnvEntryNotFound {}",
                flow_common::reason::string_of_aloc(None, &err_loc)
            ),
            Ok(x) => x,
        }
    })
}

pub fn read_to_predicate<'cx>(cx: &Context<'cx>, read: &EnvRead<ALoc>) -> Option<Predicate> {
    use flow_typing_type::type_::PredicateInner;

    let var_info = {
        let env = cx.environment();
        env.var_info.dupe()
    };
    let predicates: Vec<Predicate> = read
        .write_locs
        .iter()
        .filter_map(|wl| match wl {
            WriteLoc::Refinement {
                refinement_id,
                writes: _,
                write_id: _,
            } => {
                let refi = find_refi(&var_info, *refinement_id);
                predicate_of_refinement(cx, &refi.kind)
            }
            _ => None,
        })
        .collect();
    if predicates.is_empty() {
        None
    } else {
        let mut iter = predicates.into_iter();
        let first = iter.next().unwrap();
        let result = iter.fold(first, |acc, p| Predicate::new(PredicateInner::OrP(acc, p)));
        Some(result)
    }
}

pub fn checked_type_guard_at_return<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    param_loc: ALoc,
    return_loc: ALoc,
    pos_write_locs: &[WriteLoc<ALoc>],
    neg_refi: &EnvRead<ALoc>,
) -> Result<(Type, Option<Predicate>), Vec<ALoc>> {
    fn is_invalid(
        param_loc: &ALoc,
        acc: (bool, Vec<ALoc>),
        write_loc: &WriteLoc<ALoc>,
    ) -> (bool, Vec<ALoc>) {
        let (acc_result, mut acc_locs) = acc;
        match write_loc {
            WriteLoc::Write(r) if r.loc() == param_loc => (acc_result, acc_locs),
            WriteLoc::Refinement { writes, .. } => {
                writes.iter().fold((acc_result, acc_locs), |acc, wl| {
                    is_invalid(param_loc, acc, wl)
                })
            }
            WriteLoc::Write(r) => {
                acc_locs.push(r.loc().dupe());
                (true, acc_locs)
            }
            _ => (true, acc_locs),
        }
    }
    let (is_invalid_result, invalid_writes) = pos_write_locs
        .iter()
        .fold((false, vec![]), |acc, wl| is_invalid(&param_loc, acc, wl));
    if is_invalid_result {
        Err(invalid_writes)
    } else {
        let t = type_of_state(
            LookupMode::ForValue,
            ValKind::Internal,
            cx,
            return_loc,
            reason,
            pos_write_locs,
            None,
            None,
        );
        Ok((t, read_to_predicate(cx, neg_refi)))
    }
}

pub fn inferred_type_guard_at_return<'cx>(
    cx: &Context<'cx>,
    reason: Reason,
    return_loc: ALoc,
    write_locs: &[WriteLoc<ALoc>],
) -> Type {
    type_of_state(
        LookupMode::ForValue,
        ValKind::Internal,
        cx,
        return_loc,
        reason,
        write_locs,
        None,
        None,
    )
}

pub fn ref_entry_exn<'cx>(
    lookup_mode: LookupMode,
    cx: &Context<'cx>,
    loc: ALoc,
    reason: Reason,
) -> Type {
    let t = read_entry_exn(lookup_mode, cx, loc.dupe(), reason);
    flow_js::reposition_non_speculating(cx, loc, t)
}

pub fn find_write<'cx>(cx: &Context<'cx>, kind: DefLocType, reason: Reason) -> Type {
    let loc = reason.loc().dupe();
    match checked_find_loc_env_write_opt(cx, kind, loc) {
        Some(t) => t,
        None => flow_typing_type::type_::any_t::error(reason),
    }
}

pub fn get_refinement<'cx>(cx: &Context<'cx>, desc: ReasonDesc, loc: ALoc) -> Option<Type> {
    let reason = flow_common::reason::mk_reason(desc, loc.dupe());
    match read_entry(LookupMode::ForValue, cx, loc.dupe(), reason) {
        Ok(x) => Some(flow_js::reposition_non_speculating(cx, loc.dupe(), x)),
        Err(_) => {
            let env = cx.environment();
            match env.var_info.env_refinement_invalidation_info.get(&loc) {
                None => {}
                Some(info) => cx.add_aggressively_invalidated_location(loc, info.clone()),
            }
            None
        }
    }
}

pub fn get_var<'cx>(
    lookup_mode: Option<LookupMode>,
    cx: &Context<'cx>,
    name: &str,
    loc: ALoc,
) -> Type {
    let ord_name = Name::new(name);
    let lookup_mode = lookup_mode.unwrap_or(LookupMode::ForValue);
    read_entry_exn(
        lookup_mode,
        cx,
        loc.dupe(),
        flow_common::reason::mk_reason(VirtualReasonDesc::RIdentifier(ord_name), loc),
    )
}

pub fn query_var<'cx>(
    lookup_mode: Option<LookupMode>,
    cx: &Context<'cx>,
    name: Name,
    desc: Option<ReasonDesc>,
    loc: ALoc,
) -> Type {
    let desc = match desc {
        Some(d) => d,
        None => VirtualReasonDesc::RIdentifier(name),
    };
    let lookup_mode = lookup_mode.unwrap_or(LookupMode::ForValue);
    read_entry_exn(
        lookup_mode,
        cx,
        loc.dupe(),
        flow_common::reason::mk_reason(desc, loc),
    )
}

pub fn intrinsic_ref<'cx>(
    cx: &Context<'cx>,
    desc: Option<ReasonDesc>,
    name: Name,
    loc: ALoc,
) -> Option<(Type, ALoc)> {
    let desc = match desc {
        Some(d) => d,
        None => VirtualReasonDesc::RIdentifier(name),
    };
    let reason = flow_common::reason::mk_reason(desc, loc.dupe());
    let read = {
        let env = cx.environment();
        let var_info = &env.var_info;
        match find_var_opt(var_info, &loc) {
            Err(loc) => Err(loc),
            Ok(read) => Ok(read.clone()),
        }
    };
    match read {
        Err(_) => None,
        Ok(EnvRead {
            def_loc,
            write_locs,
            val_kind,
            name,
            id,
        }) => match (val_kind, name, def_loc) {
            (ValKind::Type { .. }, Some(_), Some(_)) => None,
            (_, _, None) => None,
            (_, _, Some(def_loc)) => {
                let PossiblyRefinedWriteState { t, .. } = possibly_refined_write_state_of_state(
                    LookupMode::ForValue,
                    val_kind,
                    cx,
                    loc.dupe(),
                    reason,
                    &write_locs,
                    id,
                    None,
                );
                Some((flow_js::reposition_non_speculating(cx, loc, t), def_loc))
            }
        },
    }
}

pub fn var_ref<'cx>(
    lookup_mode: Option<LookupMode>,
    cx: &Context<'cx>,
    desc: Option<ReasonDesc>,
    name: Name,
    loc: ALoc,
) -> Type {
    let lookup_mode = lookup_mode.unwrap_or(LookupMode::ForValue);
    let t = query_var(Some(lookup_mode), cx, name, desc, loc.dupe());
    flow_js::reposition_non_speculating(cx, loc, t)
}

pub fn sig_var_ref<'cx>(
    lookup_mode: Option<LookupMode>,
    cx: &Context<'cx>,
    desc: Option<ReasonDesc>,
    name: Name,
    loc: ALoc,
) -> Type {
    let lookup_mode = lookup_mode.unwrap_or(LookupMode::ForValue);
    let desc = match desc {
        Some(d) => d,
        None => VirtualReasonDesc::RIdentifier(name.dupe()),
    };
    let reason = flow_common::reason::mk_reason(desc, loc.dupe());
    let t = query_var(Some(lookup_mode), cx, name, None, loc);
    Type::new(flow_typing_type::type_::TypeInner::AnnotT(reason, t, true))
}

pub fn read_class_self_type<'cx>(cx: &Context<'cx>, loc: ALoc) -> Type {
    match checked_find_loc_env_write_opt(cx, DefLocType::ClassSelfLoc, loc.dupe()) {
        Some(t) => t,
        None => {
            // When checked_find_loc_env_write_opt returns None, we are reading an unreachable entry
            flow_typing_type::type_::empty_t::why(flow_common::reason::mk_reason(
                VirtualReasonDesc::REmpty,
                loc,
            ))
        }
    }
}

pub fn is_global_var<'cx>(cx: &Context<'cx>, loc: ALoc) -> bool {
    let env = cx.environment();
    match find_var_opt(&env.var_info, &loc) {
        Ok(read) => flow_env_builder::env_api::is_global_var(read),
        Err(_) => false,
    }
}

pub fn local_scope_entry_exists<'cx>(cx: &Context<'cx>, loc: ALoc) -> bool {
    !is_global_var(cx, loc)
}

pub fn has_var_read<'cx>(cx: &Context<'cx>, loc: &ALoc) -> bool {
    let env = cx.environment();
    find_var_opt(&env.var_info, loc).is_ok()
}

pub fn get_var_declared_type<'cx>(
    lookup_mode: Option<LookupMode>,
    is_declared_function: Option<bool>,
    cx: &Context<'cx>,
    name: Name,
    loc: ALoc,
) -> Type {
    let lookup_mode = lookup_mode.unwrap_or(LookupMode::ForValue);
    let is_declared_function = is_declared_function.unwrap_or(false);

    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_errors::error_message::InternalError;
    use flow_typing_type::type_::AnySource;
    use flow_typing_type::type_::any_t;

    match (name, lookup_mode) {
        (_name, LookupMode::ForType) => {
            match checked_find_loc_env_write_opt(cx, DefLocType::OrdinaryNameLoc, loc.dupe()) {
                Some(t) => t,
                None => {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInternal(Box::new((
                            loc.dupe(),
                            InternalError::MissingEnvWrite(loc.dupe()),
                        ))),
                    );
                    any_t::at(AnySource::AnyError(None), loc)
                }
            }
        }
        _ => {
            let env = cx.environment();
            provider_type_for_def_loc(is_declared_function, cx, &env, loc)
        }
    }
}

pub fn constraining_type<'cx>(
    default: Type,
    cx: &Context<'cx>,
    name: &FlowSmolStr,
    loc: ALoc,
) -> Type {
    let env = cx.environment();
    let var_info = &env.var_info;

    match var_info.env_entries.get_ordinary(&loc) {
        Some(EnvEntry::NonAssigningWrite) => default,
        _ => {
            let providers: Option<Vec<Type>> = find_providers(var_info, &loc)
                .into_iter()
                .map(|ploc| checked_find_loc_env_write_opt(cx, DefLocType::OrdinaryNameLoc, ploc))
                .collect();
            match providers {
                None => default,
                Some(ps) if ps.is_empty() => default,
                Some(ps) if ps.len() == 1 => ps.into_iter().next().unwrap(),
                Some(ps) => {
                    let mut iter = ps.into_iter();
                    let t1 = iter.next().unwrap();
                    let t2 = iter.next().unwrap();
                    let ts: Vec<Type> = iter.collect();
                    use flow_typing_type::type_::TypeInner;
                    use flow_typing_type::type_::union_rep;
                    Type::new(TypeInner::UnionT(
                        flow_common::reason::mk_reason(
                            VirtualReasonDesc::RIdentifier(Name::new(name.dupe())),
                            loc,
                        ),
                        union_rep::make(
                            None,
                            union_rep::UnionKind::ProvidersKind,
                            t1,
                            t2,
                            ts.into(),
                        ),
                    ))
                }
            }
        }
    }
}

// *************
// *  Writing  *
// *************

// Subtypes the given type against the providers for a def loc. Should be used on assignments to
// non-import value bindings
fn subtype_against_providers<'cx>(
    cx: &Context<'cx>,
    use_op: &UseOp,
    potential_global_name: Option<&str>,
    t: &Type,
    loc: ALoc,
) {
    use flow_typing_errors::error_message::EBuiltinNameLookupFailedData;
    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_type::type_::UseT;
    use flow_typing_type::type_::UseTInner;
    use flow_typing_type::type_::VirtualFrameUseOp;
    use flow_typing_type::type_::VirtualUseOp;

    let env = cx.environment();
    let var_info = &env.var_info;
    let providers = &var_info.providers;
    let scopes = &var_info.scopes;
    // We only perform a subtyping check if this is an assigning write. We call
    // writes to immutable bindings non-assigning writes. For example:
    // const x: number = 3;
    // x = 'string';
    //
    // Since the x = 'string' doesn't actually assign a value to x, we should
    // not perform a subtyping check and a second error saying string is incompatible
    // with number. We should only emit an error saying that a const cannot be reassigned.
    match var_info.env_entries.get_ordinary(&loc) {
        Some(EnvEntry::NonAssigningWrite) => {}
        Some(EnvEntry::GlobalWrite(_)) => {
            if is_provider(cx, loc.dupe())
                && let Some(name) = potential_global_name
                && cx.builtin_value_opt(name).is_none()
            {
                flow_js::add_output_non_speculating(
                    cx,
                    ErrorMessage::EBuiltinNameLookupFailed(Box::new(
                        EBuiltinNameLookupFailedData {
                            loc,
                            name: name.into(),
                        },
                    )),
                );
            }
        }
        _ => {
            if !is_provider(cx, loc.dupe()) {
                let general = provider_type_for_def_loc(false, cx, &env, loc.dupe());
                if is_def_loc_annotated(var_info, &loc) {
                    flow_js::flow_non_speculating(
                        cx,
                        (t, &UseT::new(UseTInner::UseT(use_op.clone(), general))),
                    );
                } else {
                    let use_op = match scopes.def_of_use_opt(&loc) {
                        Some(def) => {
                            let declaration = def.locs.first().dupe();
                            let name = def.actual_name.dupe();
                            let provider_locs =
                                match provider_api::Info::providers_of_def(providers, &loc) {
                                    Some(def_providers) => def_providers.providers.clone(),
                                    None => vec![],
                                };
                            VirtualUseOp::Frame(
                                Arc::new(VirtualFrameUseOp::ConstrainedAssignment(Box::new(
                                    ConstrainedAssignmentData {
                                        name,
                                        declaration,
                                        providers: provider_locs
                                            .iter()
                                            .map(|p| p.reason.loc().dupe())
                                            .collect(),
                                    },
                                ))),
                                Arc::new(use_op.dupe()),
                            )
                        }
                        None => use_op.dupe(),
                    };
                    cx.add_post_inference_subtyping_check(t.dupe(), use_op, general);
                }
            }
        }
    }
}

pub fn make_env_entries_under_resolution<'cx>(cx: &Context<'cx>, entries: EnvSet<ALoc>) {
    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_errors::error_message::InternalError;
    use flow_typing_type::type_::any_t;
    use flow_typing_type::type_util;

    let mut env = cx.environment_mut();
    let update = |env: &mut LocEnv<'cx, Context<'cx>>, key: &EnvKey<ALoc>| {
        let def_loc_kind = key.def_loc_type;
        let loc = key.loc.dupe();
        match env.types.get(key) {
            None => {}
            Some(type_entry) => {
                let t = type_entry.t.dupe();
                let reason = type_util::reason_of_t(&t);
                let reason_clone = reason.dupe();
                let loc_clone = loc.dupe();
                let new_state: flow_lazy::Lazy<
                    Context<'cx>,
                    Type,
                    Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                > = flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                    flow_js::add_output_non_speculating(
                        cx,
                        ErrorMessage::EInternal(Box::new((
                            loc_clone,
                            InternalError::ForcedReadOfUnderResolutionTvar(def_loc_kind),
                        ))),
                    );
                    any_t::error(reason_clone)
                }));
                *type_entry.state.borrow_mut() = new_state;
            }
        }
    };
    for key in entries.iter() {
        update(&mut env, key);
    }
}

// Resolve `t` with the entry in the loc_env's map. This allows it to be looked up for Write
// entries reported by the name_resolver as well as providers for the provider analysis
pub fn resolve_env_entry<'cx>(
    cx: &Context<'cx>,
    t: Type,
    kind: DefLocType,
    add_array_or_object_literal_declaration_tracking: bool,
    loc: ALoc,
) {
    use std::ops::Deref;

    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_errors::error_message::InternalError;
    use flow_typing_flow_common::flow_js_utils;
    use flow_typing_type::type_::DefT as DefTType;
    use flow_typing_type::type_::DefTInner;
    use flow_typing_type::type_::TypeInner;
    flow_typing_debug::verbose::print_if_verbose(
        cx,
        None,
        None,
        None,
        vec![format!(
            "writing to {:?} {}",
            kind,
            loc.debug_to_string(true)
        )],
    );
    let key = EnvKey::new(kind, loc.dupe());
    let (env_entry, type_entry) = {
        let env = cx.environment();
        (
            env.var_info.env_entries.get(&key).cloned(),
            env.find_write(kind, loc.dupe()).cloned(),
        )
    };
    match (env_entry, type_entry) {
        (Some(EnvEntry::NonAssigningWrite), _) => {}
        (_, None) => {
            flow_js::add_output_non_speculating(
                cx,
                ErrorMessage::EInternal(Box::new((
                    loc.dupe(),
                    InternalError::MissingEnvWrite(loc),
                ))),
            );
        }
        (_, Some(type_entry)) => {
            let existing_t = type_entry.t;
            let state = type_entry.state;
            if add_array_or_object_literal_declaration_tracking {
                if let TypeInner::OpenT(tvar) = existing_t.deref() {
                    cx.add_array_or_object_literal_declaration_tracking(
                        tvar.id() as i32,
                        loc.dupe(),
                    );
                }
            }
            tvar_resolver::resolve(cx, tvar_resolver::default_no_lowers, true, &t);
            let t_clone = t.dupe();
            let new_state: flow_lazy::Lazy<
                Context<'cx>,
                Type,
                Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
            > = flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                // Unwrap possible OpenT so that OpenT doesn't wrap another OpenT.
                // This has to be done lazily, so that we don't force tvars until we
                // resolved all entries in a component.
                match t_clone.deref() {
                    TypeInner::OpenT(tvar) => {
                        let r = tvar.reason();
                        let id = tvar.id() as i32;
                        flow_js_utils::merge_tvar(
                            cx,
                            false,
                            |_cx, r| {
                                Type::new(TypeInner::DefT(
                                    r.dupe(),
                                    DefTType::new(DefTInner::EmptyT),
                                ))
                            },
                            r,
                            id,
                        )
                    }
                    // | t -> t
                    _ => t_clone.dupe(),
                }
            }));
            *state.borrow_mut() = new_state;
        }
    }
}

fn subtype_entry<'cx>(cx: &Context<'cx>, use_op: &UseOp, t: &Type, loc: ALoc) {
    use flow_typing_type::type_::UseT;
    use flow_typing_type::type_::UseTInner;

    let env = cx.environment();
    match checked_find_loc_env_write_opt(cx, DefLocType::OrdinaryNameLoc, loc.dupe()) {
        None => {
            // If we don't see a spot for this write it is because the annotated
            // binding being looked up here is one that caused a redeclaration
            // error
            assert!(matches!(
                env.var_info.env_entries.get_ordinary(&loc),
                Some(EnvEntry::NonAssigningWrite)
            ));
        }
        Some(w) => {
            flow_js::flow_non_speculating(cx, (t, &UseT::new(UseTInner::UseT(use_op.clone(), w))));
        }
    }
}

// init_entry is called on variable declarations (not assignments), and `t`
// is the RHS type. If the variable is annotated, we just need to check t against
// its type; but if it's not annotated, the RHS t becomes the variable's type.
fn init_entry<'cx>(cx: &Context<'cx>, use_op: &UseOp, t: &Type, loc: ALoc) {
    let env = cx.environment();
    let var_info = &env.var_info;
    if is_def_loc_annotated(var_info, &loc) {
        subtype_entry(cx, use_op, t, loc)
    }
}

pub fn set_var<'cx>(cx: &Context<'cx>, use_op: &UseOp, name: &str, t: &Type, loc: ALoc) {
    subtype_against_providers(cx, use_op, Some(name), t, loc)
}

pub fn bind_function_param<'cx>(cx: &Context<'cx>, t: Type, loc: ALoc) {
    resolve_env_entry(cx, t, DefLocType::FunctionParamLoc, false, loc)
}

pub fn bind_function_this<'cx>(cx: &Context<'cx>, t: Type, loc: ALoc) {
    if matches!(&*cx.typing_mode(), TypingMode::CheckingMode) {
        resolve_env_entry(cx, t, DefLocType::FunctionThisLoc, false, loc)
    }
}

pub fn bind_class_instance_this<'cx>(cx: &Context<'cx>, t: Type, loc: ALoc) {
    resolve_env_entry(cx, t, DefLocType::ClassInstanceThisLoc, false, loc)
}

pub fn bind_class_static_this<'cx>(cx: &Context<'cx>, t: Type, loc: ALoc) {
    resolve_env_entry(cx, t, DefLocType::ClassStaticThisLoc, false, loc)
}

pub fn bind_class_instance_super<'cx>(cx: &Context<'cx>, t: Type, loc: ALoc) {
    resolve_env_entry(cx, t, DefLocType::ClassInstanceSuperLoc, false, loc)
}

pub fn bind_class_static_super<'cx>(cx: &Context<'cx>, t: Type, loc: ALoc) {
    resolve_env_entry(cx, t, DefLocType::ClassStaticSuperLoc, false, loc)
}

pub fn bind_class_self_type<'cx>(cx: &Context<'cx>, t: Type, loc: ALoc) {
    resolve_env_entry(cx, t, DefLocType::ClassSelfLoc, false, loc)
}

pub fn init_var<'cx>(cx: &Context<'cx>, use_op: &UseOp, t: &Type, loc: ALoc) {
    init_entry(cx, use_op, t, loc)
}

pub fn init_let<'cx>(cx: &Context<'cx>, use_op: &UseOp, t: &Type, loc: ALoc) {
    init_entry(cx, use_op, t, loc)
}

pub fn init_implicit_let<'cx>(cx: &Context<'cx>, use_op: &UseOp, t: &Type, loc: ALoc) {
    init_entry(cx, use_op, t, loc)
}

pub fn init_const<'cx>(cx: &Context<'cx>, use_op: &UseOp, t: &Type, loc: ALoc) {
    init_entry(cx, use_op, t, loc)
}

pub fn init_implicit_const<'cx>(cx: &Context<'cx>, use_op: &UseOp, t: &Type, loc: ALoc) {
    init_entry(cx, use_op, t, loc)
}

pub fn read_declared_type<'cx>(cx: &Context<'cx>, reason: Reason, loc: ALoc) -> Type {
    use flow_typing_type::type_::empty_t;
    // match checked_find_loc_env_write_opt cx Env_api.OrdinaryNameLoc loc with
    match checked_find_loc_env_write_opt(cx, DefLocType::OrdinaryNameLoc, loc) {
        // | Some t -> t
        Some(t) => t,
        // | None -> EmptyT.make reason
        None => empty_t::make(reason),
    }
}

// (************************)
// (* Variable Declaration *)
// (************************)

pub fn init_env<'cx>(cx: &Context<'cx>, toplevel_scope_kind: ScopeKind) {
    use std::rc::Rc;

    use flow_common::reason::mk_reason;
    use flow_typing_errors::error_message::ErrorMessage;
    use flow_typing_errors::error_message::InternalError;
    use flow_typing_type::type_::any_t;
    let entries: Vec<_> = {
        let env = cx.environment();
        env.var_info
            .env_entries
            .iter()
            .map(|(k, v)| (k.dupe(), v.clone()))
            .collect()
    };

    let initialize_entry =
        |cx: &Context<'cx>, def_loc_type: DefLocType, loc: ALoc, env_entry: &EnvEntry<ALoc>| {
            match env_entry {
                EnvEntry::AssigningWrite(reason) | EnvEntry::GlobalWrite(reason) => {
                    let reason_clone = reason.dupe();
                    let reason_clone2 = reason.dupe();
                    let loc_clone = reason.loc().dupe();
                    let initial_state: flow_lazy::Lazy<
                        Context<'cx>,
                        Type,
                        Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                    > = flow_lazy::Lazy::new(Box::new(move |cx: &Context<'cx>| {
                        flow_js::add_output_non_speculating(
                            cx,
                            ErrorMessage::EInternal(Box::new((
                                loc_clone,
                                InternalError::ReadOfUnreachedTvar(def_loc_type),
                            ))),
                        );
                        match &*cx.typing_mode() {
                            TypingMode::CheckingMode => any_t::error(reason_clone),
                            TypingMode::SynthesisMode { .. } | TypingMode::HintEvaluationMode => {
                                any_t::placeholder(reason_clone)
                            }
                        }
                    }));
                    let state = Rc::new(RefCell::new(initial_state));
                    let state_for_lazy = state.dupe();
                    let lazy_t: Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx> =
                        Box::new(move |cx: &Context<'cx>| {
                            let current_state = state_for_lazy.borrow();
                            current_state.get_forced(cx).dupe()
                        });
                    // During initialization, all these lazy tvars are created, but not all of them are
                    // ready for forcing. The ones that are ready for forcing will be separately added to
                    // the list after each component resolution.
                    let force_post_component = false;
                    let t = flow_typing_tvar::mk_fully_resolved_lazy(
                        cx,
                        reason_clone2,
                        force_post_component,
                        lazy_t,
                    );
                    let mut current_env = cx.environment_mut();
                    current_env.initialize(def_loc_type, loc, TypeEntry { t, state });
                }
                EnvEntry::NonAssigningWrite => {
                    if is_provider(cx, loc.dupe()) {
                        // If an illegal write is considered as a provider, we still need to give it a
                        // slot to prevent crashing in code that queries provider types.
                        let reason = mk_reason(
                            flow_common::reason::VirtualReasonDesc::RAnyImplicit,
                            loc.dupe(),
                        );
                        let reason_clone = reason.dupe();
                        let initial_state: flow_lazy::Lazy<
                            Context<'cx>,
                            Type,
                            Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>,
                        > = flow_lazy::Lazy::new(Box::new(move |_cx: &Context<'cx>| {
                            any_t::error(reason_clone)
                        })
                            as Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx>);
                        let state = Rc::new(RefCell::new(initial_state));
                        let state_for_lazy = state.dupe();
                        let lazy_t: Box<dyn FnOnce(&Context<'cx>) -> Type + 'cx> =
                            Box::new(move |cx: &Context<'cx>| {
                                let current_state = state_for_lazy.borrow();
                                current_state.get_forced(cx).dupe()
                            });
                        let t = flow_typing_tvar::mk_fully_resolved_lazy(cx, reason, false, lazy_t);
                        let mut current_env = cx.environment_mut();
                        current_env.initialize(def_loc_type, loc, TypeEntry { t, state });
                    }
                }
            }
        };

    for (key, env_entry) in &entries {
        // Array providers must be initialized first
        if key.def_loc_type == DefLocType::ArrayProviderLoc {
            initialize_entry(cx, key.def_loc_type, key.loc.dupe(), env_entry);
        }
    }

    for (key, env_entry) in &entries {
        if key.def_loc_type != DefLocType::ArrayProviderLoc {
            initialize_entry(cx, key.def_loc_type, key.loc.dupe(), env_entry);
        }
    }

    let mut env = cx.environment_mut();
    env.scope_kind = toplevel_scope_kind;
}

pub fn discriminant_after_negated_cases<'cx>(
    cx: &Context<'cx>,
    switch_loc: ALoc,
    refinement_key_opt: Option<&Key>,
) -> Option<Type> {
    use flow_common::reason::mk_reason;

    let reason_desc = match refinement_key_opt {
        None => VirtualReasonDesc::RCustom("discriminant of switch".into()),
        Some(refinement_key) => refinement_key.reason_desc::<ALoc>(),
    };
    read_entry(
        LookupMode::ForValue,
        cx,
        switch_loc.dupe(),
        mk_reason(reason_desc, switch_loc),
    )
    .ok()
}

pub fn get_next<'cx>(cx: &Context<'cx>, loc: ALoc) -> Type {
    use flow_common::reason::mk_reason;

    read_entry_exn(
        LookupMode::ForValue,
        cx,
        loc.dupe(),
        mk_reason(VirtualReasonDesc::RNext, loc),
    )
}
