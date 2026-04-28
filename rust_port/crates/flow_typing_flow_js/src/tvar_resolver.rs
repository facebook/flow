/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::ops::Deref;

use dupe::Dupe;
use flow_common::polarity::Polarity;
use flow_common::reason::Reason;
use flow_common::reason::VirtualReasonDesc;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils;
use flow_typing_type::type_::AnySource;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::constraint::Constraints;
use flow_typing_type::type_::constraint::forcing_state::ForcingState;
use flow_typing_type::type_::empty_t;
use flow_typing_type::type_::union_rep::UnionKind;
use flow_typing_visitors::type_visitor;
use flow_typing_visitors::type_visitor::TypeVisitor;

pub fn default_no_lowers(r: &Reason) -> Type {
    let desc = {
        match r.desc(true) {
            VirtualReasonDesc::RIdentifier(name) => {
                let x = name.as_str();
                VirtualReasonDesc::RCustom(format!("`{}` (resolved to type `empty`)", x).into())
            }
            _ => VirtualReasonDesc::REmpty,
        }
    };
    empty_t::make(r.dupe().replace_desc(desc))
}

struct Resolver<'a, 'cx, F>
where
    F: Fn(&Reason) -> Type,
{
    no_lowers: F,
    filter_empty: bool,
    cx: &'a Context<'cx>,
}

type Acc = (BTreeMap<i32, bool>, bool);

impl<F> TypeVisitor<Acc> for Resolver<'_, '_, F>
where
    F: Fn(&Reason) -> Type,
{
    fn type_<'cx>(&mut self, cx: &Context<'cx>, pole: Polarity, seen: Acc, t: &Type) -> Acc {
        match t.deref() {
            TypeInner::AnyT(_, AnySource::Placeholder) => {
                let (seen_tvars, _) = seen;
                (seen_tvars, true)
            }
            _ => type_visitor::type_default(self, cx, pole, seen, t),
        }
    }

    fn tvar<'cx>(
        &mut self,
        cx: &Context<'cx>,
        pole: Polarity,
        seen: Acc,
        r: &Reason,
        id: u32,
    ) -> Acc {
        // Use self.cx (which has &Context<'a>) for operations that
        // require the tied lifetime, like merge_tvar_opt.
        let tied_cx = self.cx;
        let id = id as i32;
        let (root_id, root) = tied_cx.find_root(id);
        match &root.constraints {
            Constraints::FullyResolved(_) => seen,
            Constraints::Unresolved(_) | Constraints::Resolved(_) => {
                let (mut seen_tvars, seen_placeholders) = seen;
                if let Some(&seen_placeholders_in_map) = seen_tvars.get(&root_id) {
                    // Case 1: We have already resolved the tvar earlier.
                    // In this case, seen_placeholders in the map is the answer.
                    //
                    // Case 2: We are in the process of resolving a cyclic tvar
                    // At this point, seen_placeholders in the map will be false. If after visiting the entire
                    // type we do find placeholders, it will be OR-ed to true eventually.
                    return (seen_tvars, seen_placeholders_in_map);
                }
                seen_tvars.insert(root_id, false);
                let t = {
                    match flow_js_utils::merge_tvar_opt(
                        tied_cx,
                        self.filter_empty,
                        UnionKind::ResolvedKind,
                        r,
                        root_id,
                    ) {
                        Some(t) => t,
                        None => (self.no_lowers)(r),
                    }
                };
                let (mut seen_tvars, seen_placeholders_in_t) =
                    self.type_(cx, pole, (seen_tvars, false), &t);
                seen_tvars.insert(root_id, seen_placeholders_in_t);
                let constraints = {
                    if seen_placeholders_in_t {
                        Constraints::Resolved(t)
                    } else {
                        Constraints::FullyResolved(ForcingState::of_non_lazy_t(t))
                    }
                };
                tied_cx.set_root_constraints(root_id, constraints);
                (seen_tvars, seen_placeholders || seen_placeholders_in_t)
            }
        }
    }
}

/// Read-only check: does the type contain any unconstrained tvars (tvars with no lower bounds)?
///
/// This mirrors the OCaml behavior where `fully_resolve_final_result` uses
/// `raise UnconstrainedTvarException` in `no_lowers` to abort resolution immediately.
/// In Rust we can't use exceptions, so we do a non-mutating pre-check instead.
/// This prevents `resolved_t` from permanently mutating tvars to `FullyResolved(EmptyT)`
/// when the result will be discarded anyway (DecompositionError).
struct UnconstrainedChecker<'a, 'cx> {
    cx: &'a Context<'cx>,
}

impl TypeVisitor<(BTreeMap<i32, ()>, bool)> for UnconstrainedChecker<'_, '_> {
    fn type_<'cx>(
        &mut self,
        cx: &Context<'cx>,
        pole: Polarity,
        seen: (BTreeMap<i32, ()>, bool),
        t: &Type,
    ) -> (BTreeMap<i32, ()>, bool) {
        if seen.1 {
            return seen;
        }
        type_visitor::type_default(self, cx, pole, seen, t)
    }

    fn tvar<'cx>(
        &mut self,
        cx: &Context<'cx>,
        pole: Polarity,
        seen: (BTreeMap<i32, ()>, bool),
        r: &Reason,
        id: u32,
    ) -> (BTreeMap<i32, ()>, bool) {
        if seen.1 {
            return seen;
        }
        let tied_cx = self.cx;
        let id = id as i32;
        let (root_id, root) = tied_cx.find_root(id);
        match &root.constraints {
            Constraints::FullyResolved(_) => seen,
            Constraints::Unresolved(_) | Constraints::Resolved(_) => {
                let (mut seen_tvars, _) = seen;
                if seen_tvars.contains_key(&root_id) {
                    return (seen_tvars, false);
                }
                seen_tvars.insert(root_id, ());
                match flow_js_utils::merge_tvar_opt(
                    tied_cx,
                    false,
                    UnionKind::ResolvedKind,
                    r,
                    root_id,
                ) {
                    Some(t) => self.type_(cx, pole, (seen_tvars, false), &t),
                    None => (seen_tvars, true),
                }
            }
        }
    }
}

pub fn has_unconstrained_tvars<'cx>(cx: &Context<'cx>, t: &Type) -> bool {
    let mut checker = UnconstrainedChecker { cx };
    let (_, found) = checker.type_(cx, Polarity::Positive, (BTreeMap::new(), false), t);
    found
}

/// Like `has_unconstrained_tvars`, but allows exempting certain unconstrained tvars
/// based on their reason. This mirrors OCaml's `in_sandbox_cx` where `no_lowers` only
/// raises `UnconstrainedTvarException` for non-exempt tvars (e.g., tvars whose reason
/// is `RInferredUnionElemArray { is_empty = true }` are exempt and silently resolved
/// to empty).
struct UnconstrainedCheckerExcept<'a, 'cx, F: Fn(&Reason) -> bool> {
    exempt: F,
    cx: &'a Context<'cx>,
}

impl<F: Fn(&Reason) -> bool> TypeVisitor<(BTreeMap<i32, ()>, bool)>
    for UnconstrainedCheckerExcept<'_, '_, F>
{
    fn type_<'cx>(
        &mut self,
        cx: &Context<'cx>,
        pole: Polarity,
        seen: (BTreeMap<i32, ()>, bool),
        t: &Type,
    ) -> (BTreeMap<i32, ()>, bool) {
        if seen.1 {
            return seen;
        }
        type_visitor::type_default(self, cx, pole, seen, t)
    }

    fn tvar<'cx>(
        &mut self,
        cx: &Context<'cx>,
        pole: Polarity,
        seen: (BTreeMap<i32, ()>, bool),
        r: &Reason,
        id: u32,
    ) -> (BTreeMap<i32, ()>, bool) {
        if seen.1 {
            return seen;
        }
        let tied_cx = self.cx;
        let id = id as i32;
        let (root_id, root) = tied_cx.find_root(id);
        match &root.constraints {
            Constraints::FullyResolved(_) => seen,
            Constraints::Unresolved(_) | Constraints::Resolved(_) => {
                let (mut seen_tvars, _) = seen;
                if seen_tvars.contains_key(&root_id) {
                    return (seen_tvars, false);
                }
                seen_tvars.insert(root_id, ());
                match flow_js_utils::merge_tvar_opt(
                    tied_cx,
                    false,
                    UnionKind::ResolvedKind,
                    r,
                    root_id,
                ) {
                    Some(t) => self.type_(cx, pole, (seen_tvars, false), &t),
                    None => {
                        if (self.exempt)(r) {
                            (seen_tvars, false)
                        } else {
                            (seen_tvars, true)
                        }
                    }
                }
            }
        }
    }
}

pub fn has_unconstrained_tvars_except<'cx>(
    cx: &Context<'cx>,
    t: &Type,
    exempt: impl Fn(&Reason) -> bool,
) -> bool {
    let mut checker = UnconstrainedCheckerExcept { exempt, cx };
    let (_, found) = checker.type_(cx, Polarity::Positive, (BTreeMap::new(), false), t);
    found
}

pub fn resolve<'cx, F>(cx: &Context<'cx>, no_lowers: F, filter_empty: bool, t: &Type)
where
    F: Fn(&Reason) -> Type,
{
    let mut resolver = Resolver {
        no_lowers,
        filter_empty,
        cx,
    };
    resolver.type_(cx, Polarity::Positive, (BTreeMap::new(), false), t);
}

pub fn resolved_t<'cx, F>(no_lowers: F, filter_empty: bool, cx: &Context<'cx>, t: Type) -> Type
where
    F: Fn(&Reason) -> Type,
{
    resolve(cx, no_lowers, filter_empty, &t);
    t
}

pub fn mk_tvar_and_fully_resolve_where<'cx, E>(
    cx: &Context<'cx>,
    reason: Reason,
    f: impl FnOnce(&Context<'cx>, &Type) -> Result<(), E>,
) -> Result<Type, E> {
    let tvar = flow_typing_tvar::mk_where(cx, reason, f)?;
    resolve(cx, default_no_lowers, true, &tvar);
    Ok(tvar)
}

pub fn mk_tvar_and_fully_resolve_no_wrap_where<'cx, E>(
    cx: &Context<'cx>,
    reason: Reason,
    f: impl FnOnce(&Context<'cx>, &Reason, i32) -> Result<(), E>,
) -> Result<Type, E> {
    let tvar = flow_typing_tvar::mk_no_wrap_where(cx, reason, f)?;
    resolve(cx, default_no_lowers, true, &tvar);
    Ok(tvar)
}
