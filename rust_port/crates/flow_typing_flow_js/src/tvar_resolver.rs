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

struct Resolver<F>
where
    F: Fn(&Reason) -> Type,
{
    no_lowers: F,
    filter_empty: bool,
}

type Acc = (BTreeMap<i32, bool>, bool);

impl<F> TypeVisitor<Acc> for Resolver<F>
where
    F: Fn(&Reason) -> Type,
{
    fn type_(&mut self, cx: &Context, pole: Polarity, seen: Acc, t: &Type) -> Acc {
        match t.deref() {
            TypeInner::AnyT(_, AnySource::Placeholder) => {
                let (seen_tvars, _) = seen;
                (seen_tvars, true)
            }
            _ => type_visitor::type_default(self, cx, pole, seen, t),
        }
    }

    fn tvar(&mut self, cx: &Context, pole: Polarity, seen: Acc, r: &Reason, id: u32) -> Acc {
        let id = id as i32;
        let (root_id, root) = cx.find_root(id);
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
                        cx,
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
                cx.set_root_constraints(root_id, constraints);
                (seen_tvars, seen_placeholders || seen_placeholders_in_t)
            }
        }
    }
}

pub fn resolve<F>(cx: &Context, no_lowers: F, filter_empty: bool, t: &Type)
where
    F: Fn(&Reason) -> Type,
{
    let mut resolver = Resolver {
        no_lowers,
        filter_empty,
    };
    resolver.type_(cx, Polarity::Positive, (BTreeMap::new(), false), t);
}

pub fn resolved_t<F>(no_lowers: F, filter_empty: bool, cx: &Context, t: Type) -> Type
where
    F: Fn(&Reason) -> Type,
{
    resolve(cx, no_lowers, filter_empty, &t);
    t
}

pub fn mk_tvar_and_fully_resolve_where<F>(cx: &Context, reason: Reason, f: F) -> Type
where
    F: FnOnce(&Type),
{
    let tvar = flow_typing_tvar::mk_where(cx, reason, f);
    resolve(cx, default_no_lowers, true, &tvar);
    tvar
}

pub fn mk_tvar_and_fully_resolve_where_result<E>(
    cx: &Context,
    reason: Reason,
    f: impl FnOnce(&Type) -> Result<(), E>,
) -> Result<Type, E> {
    let tvar = flow_typing_tvar::mk_where_result(cx, reason, f)?;
    resolve(cx, default_no_lowers, true, &tvar);
    Ok(tvar)
}

pub fn mk_tvar_and_fully_resolve_no_wrap_where<F>(cx: &Context, reason: Reason, f: F) -> Type
where
    F: FnOnce(&Reason, i32),
{
    let tvar = flow_typing_tvar::mk_no_wrap_where(cx, reason, f);
    resolve(cx, default_no_lowers, true, &tvar);
    tvar
}

pub fn mk_tvar_and_fully_resolve_no_wrap_where_result<E>(
    cx: &Context,
    reason: Reason,
    f: impl FnOnce(&Reason, i32) -> Result<(), E>,
) -> Result<Type, E> {
    let tvar = flow_typing_tvar::mk_no_wrap_where_result(cx, reason, f)?;
    resolve(cx, default_no_lowers, true, &tvar);
    Ok(tvar)
}
