/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;

use dupe::Dupe;
use flow_common::reason::Reason;
use flow_common::reason::mk_id;
use flow_typing_context::Context;
use flow_typing_type::type_::Tvar;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeInner;
use flow_typing_type::type_::constraint;
use flow_typing_type::type_::constraint::forcing_state::ForcingState;
use flow_utils_union_find::Node;
use once_cell::unsync::Lazy;

pub fn mk_no_wrap(cx: &Context, reason: &Reason) -> i32 {
    let tvar = mk_id() as i32;
    cx.add_tvar(
        tvar,
        Node::create_root(constraint::Constraints::Unresolved(Box::default())),
    );
    if cx.is_verbose() {
        eprintln!(
            "TVAR {} ({}): {:?}",
            tvar,
            cx.graph().borrow().len(),
            reason
        );
    }
    tvar
}

pub fn mk(cx: &Context, reason: Reason) -> Type {
    let id = mk_no_wrap(cx, &reason);
    Type::new(TypeInner::OpenT(Tvar::new(reason, id as u32)))
}

pub fn mk_where<F>(cx: &Context, reason: Reason, f: F) -> Type
where
    F: FnOnce(&Type),
{
    let tvar = mk(cx, reason);
    f(&tvar);
    tvar
}

pub fn mk_where_result<E>(
    cx: &Context,
    reason: Reason,
    f: impl FnOnce(&Type) -> Result<(), E>,
) -> Result<Type, E> {
    let tvar = mk(cx, reason);
    f(&tvar)?;
    Ok(tvar)
}

pub fn mk_where_no_wrap<F>(cx: &Context, reason: Reason, f: F) -> i32
where
    F: FnOnce(&Type),
{
    let tvar = mk_no_wrap(cx, &reason);
    let t = Type::new(TypeInner::OpenT(Tvar::new(reason, tvar as u32)));
    f(&t);
    tvar
}

pub fn mk_no_wrap_where<F>(cx: &Context, reason: Reason, f: F) -> Type
where
    F: FnOnce(&Reason, i32),
{
    let tvar = mk_no_wrap(cx, &reason);
    f(&reason, tvar);
    Type::new(TypeInner::OpenT(Tvar::new(reason, tvar as u32)))
}

pub fn mk_no_wrap_where_result<E>(
    cx: &Context,
    reason: Reason,
    f: impl FnOnce(&Reason, i32) -> Result<(), E>,
) -> Result<Type, E> {
    let tvar = mk_no_wrap(cx, &reason);
    f(&reason, tvar)?;
    Ok(Type::new(TypeInner::OpenT(Tvar::new(reason, tvar as u32))))
}

fn mk_fully_resolved_helper(cx: &Context, state: ForcingState) -> i32 {
    let id = mk_id() as i32;
    let node = Node::create_root(constraint::Constraints::FullyResolved(state));
    cx.graph().borrow_mut().insert(id, node);
    id
}

pub fn mk_fully_resolved_lazy(
    cx: &Context,
    reason: Reason,
    force_post_component: bool,
    lazy_t: Rc<Lazy<Type, Box<dyn FnOnce() -> Type>>>,
) -> Type {
    let state = ForcingState::of_lazy_t(reason.dupe(), move || (*lazy_t).dupe());
    let id = mk_fully_resolved_helper(cx, state.clone());
    if force_post_component {
        cx.add_post_component_tvar_forcing_state(id, state);
    }
    if cx.is_verbose() {
        eprintln!(
            "Lazy TVAR {} ({}): {:?}",
            id,
            cx.graph().borrow().len(),
            reason
        );
    }
    Type::new(TypeInner::OpenT(Tvar::new(reason, id as u32)))
}

pub fn mk_fully_resolved_no_wrap(cx: &Context, t: Type) -> i32 {
    mk_fully_resolved_helper(cx, ForcingState::of_non_lazy_t(t))
}

pub fn mk_fully_resolved(cx: &Context, reason: Reason, t: Type) -> Type {
    let id = mk_fully_resolved_helper(cx, ForcingState::of_non_lazy_t(t));
    Type::new(TypeInner::OpenT(Tvar::new(reason, id as u32)))
}

pub fn mk_resolved(cx: &Context, reason: Reason, t: Type) -> Type {
    let id = mk_id() as i32;
    let constraints = constraint::Constraints::Resolved(t);
    let node = Node::create_root(constraints);
    cx.graph().borrow_mut().insert(id, node);
    Type::new(TypeInner::OpenT(Tvar::new(reason, id as u32)))
}
