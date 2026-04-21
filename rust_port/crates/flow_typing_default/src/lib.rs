/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Destructuring in the presence of default values gives rise to a list-like
// structure, which is encapsulated herein. While a destructuring pattern only
// creates bindings at its leaves, defaults can be provided at every level.
// Each default can provide additional lower bounds, and must be accumulated.
//
// For example, consider the following function:
//
// function f({x=0}={x:""}) { return x; }
//
// In the above, x might be a number, a string, or something else provided
// downstream. We can represent the default value associated with x as follows:
//
// Cons (
//   (Expr `0`),
//   (Selector
//     (Expr `{x:""}`)
//     (Prop "x")))
//
// We can fold over the list structure of default values to "evaluate" them.
// In practice, we only ever evaluate defaults to turn them into types. Still,
// the little interpreter here allows us to be careful about the environment in
// which they are evaluated, which is crucial, as later default expressions can
// depend on bindings established by earlier ones.

use flow_common::reason::Reason;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_type::type_::Selector;
use flow_typing_type::type_::Type;

#[derive(Debug, Clone)]
pub enum Default<A> {
    Expr(A),
    Cons(A, Box<Default<A>>),
    Selector(Reason, Box<Default<A>>, Selector),
}

pub fn expr<A>(e: A, default: Option<Default<A>>) -> Default<A> {
    match default {
        Some(d) => Default::Cons(e, Box::new(d)),
        None => Default::Expr(e),
    }
}

pub fn elem<A>(key: Type, reason: Reason, default: Default<A>) -> Default<A> {
    Default::Selector(reason, Box::new(default), Selector::Elem(key))
}

pub fn prop<A>(
    x: FlowSmolStr,
    reason: Reason,
    has_default: bool,
    default: Default<A>,
) -> Default<A> {
    Default::Selector(reason, Box::new(default), Selector::Prop(x, has_default))
}

pub fn arr_rest<A>(i: i32, reason: Reason, default: Default<A>) -> Default<A> {
    Default::Selector(reason, Box::new(default), Selector::ArrRest(i))
}

pub fn obj_rest<A>(xs: &[FlowSmolStr], reason: Reason, default: Default<A>) -> Default<A> {
    Default::Selector(reason, Box::new(default), Selector::ObjRest(xs.into()))
}

pub fn default<A>(reason: Reason, d: Default<A>) -> Default<A> {
    Default::Selector(reason, Box::new(d), Selector::Default)
}

pub fn fold<A, R>(
    d: &Default<A>,
    expr_fn: &impl Fn(&A) -> R,
    cons_fn: &impl Fn(R, R) -> R,
    selector_fn: &impl Fn(Reason, R, Selector) -> R,
) -> R {
    match d {
        Default::Expr(e) => expr_fn(e),
        Default::Cons(e, d) => cons_fn(expr_fn(e), fold(d, expr_fn, cons_fn, selector_fn)),
        Default::Selector(r, d, s) => {
            selector_fn(r.clone(), fold(d, expr_fn, cons_fn, selector_fn), s.clone())
        }
    }
}
