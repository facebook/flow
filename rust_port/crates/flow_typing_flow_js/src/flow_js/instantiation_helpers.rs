/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;

use flow_typing_flow_common::instantiation_utils::implicit_type_argument;
use flow_typing_implicit_instantiation_check::ImplicitInstantiationCheck;
use flow_typing_type::type_::SpecializeTData;

use super::helpers::*;
use super::*;

impl flow_js_utils::InstantiationHelper for FlowJs {
    fn reposition<'cx>(
        cx: &Context<'cx>,
        trace: Option<DepthTrace>,
        loc: ALoc,
        t: Type,
    ) -> Result<Type, FlowJsException> {
        helpers::reposition(cx, trace, loc, None, None, t)
    }

    fn is_subtype<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        t1: Type,
        t2: Type,
    ) -> Result<(), FlowJsException> {
        rec_flow_t(cx, trace, use_op, (&t1, &t2))
    }

    fn unify<'cx>(
        cx: &Context<'cx>,
        trace: DepthTrace,
        use_op: UseOp,
        t1: Type,
        t2: Type,
    ) -> Result<(), FlowJsException> {
        rec_unify(
            cx,
            trace,
            use_op,
            UnifyCause::Uncategorized,
            Some(true),
            &t1,
            &t2,
        )
    }

    fn mk_targ<'cx>(
        cx: &Context<'cx>,
        typeparam: &TypeParam,
        reason_op: &Reason,
        reason_tapp: &Reason,
    ) -> Type {
        implicit_type_argument::mk_targ(cx, typeparam, reason_op, reason_tapp)
    }
}

/// Instantiate a polymorphic definition given tparam instantiations in a Call or New expression.  
pub(super) fn instantiate_with_targs_with_soln<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason_op: &Reason,
    reason_tapp: &Reason,
    poly_t: (ALoc, Vec1<TypeParam>, Type),
    targs: Vec<Targ>,
) -> Result<(Type, Vec<(Type, SubstName)>), FlowJsException> {
    let (tparams_loc, xs, t) = poly_t;
    let mut targs_iter = targs.into_iter();
    let mut ts: Vec<Type> = Vec::new();
    for _ in xs.iter() {
        match targs_iter.next() {
            None => break,
            Some(Targ::ExplicitArg(t)) => ts.push(t),
            Some(Targ::ImplicitArg(_)) => {
                panic!(
                    "targs containing ImplicitArg should be handled by ImplicitInstantiationKit instead."
                );
            }
        }
    }
    FlowJs::instantiate_poly_with_targs(
        cx,
        trace,
        use_op,
        reason_op,
        reason_tapp,
        None,
        None,
        (tparams_loc, xs, t),
        ts,
    )
}

pub(super) fn instantiate_with_targs<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason_op: &Reason,
    reason_tapp: &Reason,
    poly_t: (ALoc, Vec1<TypeParam>, Type),
    targs: Vec<Targ>,
) -> Result<Type, FlowJsException> {
    let (t, _) =
        instantiate_with_targs_with_soln(cx, trace, use_op, reason_op, reason_tapp, poly_t, targs)?;
    Ok(t)
}

pub(super) fn instantiate_poly_call_or_new_with_soln<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    lparts: (Reason, ALoc, Vec1<TypeParam>, Type),
    uparts: (UseOp, Reason, Option<Rc<[Targ]>>, LazyHintT<Context<'cx>>),
    check: impl FnOnce() -> ImplicitInstantiationCheck,
) -> Result<(Type, Vec<(Type, SubstName)>), FlowJsException> {
    let (reason_tapp, tparams_loc, xs, t) = lparts;
    let (use_op, reason_op, targs, return_hint) = uparts;
    match type_util::all_explicit_targs(targs.as_deref()) {
        Some(targs) => instantiate_with_targs_with_soln(
            cx,
            trace,
            use_op,
            &reason_op,
            &reason_tapp,
            (tparams_loc, xs, t),
            targs,
        ),
        None => {
            let check = check();
            let result = crate::implicit_instantiation::kit::run_call(
                cx,
                &check,
                &return_hint,
                trace,
                use_op,
                &reason_op,
                &reason_tapp,
            )?;
            Ok(result)
        }
    }
}

// and instantiate_poly_call_or_new cx trace lparts uparts check =
pub(super) fn instantiate_poly_call_or_new<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    lparts: (Reason, ALoc, Vec1<TypeParam>, Type),
    uparts: (UseOp, Reason, Option<Rc<[Targ]>>, LazyHintT<Context<'cx>>),
    check: impl FnOnce() -> ImplicitInstantiationCheck,
) -> Result<Type, FlowJsException> {
    let (reason_tapp, tparams_loc, xs, t) = lparts;
    let (use_op, reason_op, targs, return_hint) = uparts;
    match type_util::all_explicit_targs(targs.as_deref()) {
        Some(targs) => instantiate_with_targs(
            cx,
            trace,
            use_op,
            &reason_op,
            &reason_tapp,
            (tparams_loc, xs, t),
            targs,
        ),
        None => {
            let check = check();
            let (t, _) = crate::implicit_instantiation::kit::run_call(
                cx,
                &check,
                &return_hint,
                trace,
                use_op,
                &reason_op,
                &reason_tapp,
            )?;
            Ok(t)
        }
    }
}

/// Instantiate a polymorphic definition with stated bound or 'any' for args  
/// Needed only for `instanceof` refis and React.PropTypes.instanceOf types  
pub(super) fn instantiate_poly_default_args<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    use_op: UseOp,
    reason_op: &Reason,
    reason_tapp: &Reason,
    tparams_loc: ALoc,
    xs: Vec1<TypeParam>,
    t: Type,
) -> Result<Type, FlowJsException> {
    // Remember: other_bound might refer to other type params
    let mut ts: Vec<Type> = Vec::new();
    for _ in xs.iter() {
        let t = unsoundness::why(UnsoundnessKind::InstanceOfRefinement, reason_op.dupe());
        ts.push(t);
    }
    let (t, _) = FlowJs::instantiate_poly_with_targs(
        cx,
        trace,
        use_op,
        reason_op,
        reason_tapp,
        None,
        None,
        (tparams_loc, xs, t),
        ts,
    )?;
    Ok(t)
}

// Specialize This in a class. Eventually this causes substitution
pub(super) fn instantiate_this_class<'cx>(
    cx: &Context<'cx>,
    trace: DepthTrace,
    reason_op: &Reason,
    reason_tapp: &Reason,
    c: &Type,
    ts: Option<Rc<[Type]>>,
    this: &Type,
    k: &Cont<Context<'cx>>,
) -> Result<(), FlowJsException> {
    let tc = match ts {
        None => c.dupe(),
        Some(ts) => flow_typing_tvar::mk_where(cx, reason_tapp.dupe(), |cx, tout| {
            rec_flow(
                cx,
                trace,
                (
                    c,
                    &UseT::new(UseTInner::SpecializeT(Box::new(SpecializeTData {
                        use_op: unknown_use(),
                        reason: reason_op.dupe(),
                        reason2: reason_tapp.dupe(),
                        targs: Some(ts),
                        tvar: tout.dupe(),
                    }))),
                ),
            )
        })?,
    };
    rec_flow(
        cx,
        trace,
        (
            &tc,
            &UseT::new(UseTInner::ThisSpecializeT(
                reason_tapp.dupe(),
                this.dupe(),
                Box::new(k.clone()),
            )),
        ),
    )
}
