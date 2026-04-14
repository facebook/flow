/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! API for annotation type variables

use flow_common::reason::Reason;
use flow_common::reason::mk_id;
use flow_typing_context::Context;
use flow_typing_type::type_::aconstraint::AConstraint;
use flow_typing_type::type_::aconstraint::AConstraintInner;
use flow_typing_type::type_::aconstraint::Op;

pub fn init_avar_with_id<'cx>(cx: &Context<'cx>, id: i32, constraint_: AConstraint<'cx>) {
    cx.add_avar(id, constraint_);
}

pub fn init_avar<'cx>(cx: &Context<'cx>, constraint_: AConstraint<'cx>) -> i32 {
    let id = mk_id() as i32;
    init_avar_with_id(cx, id, constraint_);
    id
}

pub fn unresolved_with_id<'cx>(cx: &Context<'cx>, id: i32, reason: Reason) {
    init_avar_with_id(
        cx,
        id,
        AConstraint::new(AConstraintInner::AnnotUnresolved {
            reason,
            dependents: Default::default(),
        }),
    );
}

pub fn constrained<'cx>(cx: &Context<'cx>, op: Op<'cx>, id: i32) -> i32 {
    let id_ = init_avar(
        cx,
        AConstraint::new(AConstraintInner::AnnotOp {
            op,
            id,
            dependents: Default::default(),
        }),
    );
    let dep_constraints = cx.find_avar(id);
    dep_constraints.update_deps(|mut deps| {
        deps.insert(id_);
        deps
    });
    id_
}
