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

struct AbortableResolver<'a, 'cx, F>
where
    F: Fn(&Reason) -> Option<Type>,
{
    no_lowers: F,
    filter_empty: bool,
    cx: &'a Context<'cx>,
    aborted: bool,
}

impl<F> TypeVisitor<Acc> for AbortableResolver<'_, '_, F>
where
    F: Fn(&Reason) -> Option<Type>,
{
    fn type_<'cx>(&mut self, cx: &Context<'cx>, pole: Polarity, seen: Acc, t: &Type) -> Acc {
        if self.aborted {
            return seen;
        }
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
        if self.aborted {
            return seen;
        }
        let tied_cx = self.cx;
        let id = id as i32;
        let (root_id, root) = tied_cx.find_root(id);
        match &root.constraints {
            Constraints::FullyResolved(_) => seen,
            Constraints::Unresolved(_) | Constraints::Resolved(_) => {
                let (mut seen_tvars, seen_placeholders) = seen;
                if let Some(&seen_placeholders_in_map) = seen_tvars.get(&root_id) {
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
                        None => match (self.no_lowers)(r) {
                            Some(t) => t,
                            None => {
                                self.aborted = true;
                                return (seen_tvars, seen_placeholders);
                            }
                        },
                    }
                };
                let (mut seen_tvars, seen_placeholders_in_t) =
                    self.type_(cx, pole, (seen_tvars, false), &t);
                if self.aborted {
                    return (seen_tvars, seen_placeholders);
                }
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

pub fn resolved_t_abortable<'cx, F>(
    no_lowers: F,
    filter_empty: bool,
    cx: &Context<'cx>,
    t: Type,
) -> Option<Type>
where
    F: Fn(&Reason) -> Option<Type>,
{
    let mut resolver = AbortableResolver {
        no_lowers,
        filter_empty,
        cx,
        aborted: false,
    };
    resolver.type_(cx, Polarity::Positive, (BTreeMap::new(), false), &t);
    if resolver.aborted { None } else { Some(t) }
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
