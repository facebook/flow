/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! NOTE The unsafe functions below may throw SpeculationSingletonError exception
use dupe::Dupe;
use flow_common::reason::Reason;
use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_flow_js::flow_js::FlowJs;
use flow_typing_type::type_::DepthTrace;
use flow_typing_type::type_::MethodAction;
use flow_typing_type::type_::MethodTData;
use flow_typing_type::type_::PropRef;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::UseT;
use flow_typing_type::type_::UseTInner;
use flow_typing_type::type_::unknown_use;

pub fn try_custom<'cx, 'a>(
    cx: &Context<'cx>,
    use_op: Option<flow_typing_type::type_::UseOp>,
    use_t: Option<UseT<Context<'cx>>>,
    default_resolve: Option<Box<dyn FnOnce(&Context<'cx>) -> Result<(), FlowJsException> + 'a>>,
    no_match_error_loc: flow_aloc::ALoc,
    cases: Vec<Box<dyn FnOnce(&Context<'cx>) -> Result<(), FlowJsException> + 'a>>,
) -> Result<(), FlowJsException> {
    flow_typing_flow_js::speculation_kit::try_custom(
        cx,
        use_op,
        use_t,
        default_resolve,
        no_match_error_loc,
        cases,
    )
}

pub fn flow_t_unsafe<'cx>(cx: &Context<'cx>, (l, u): (Type, Type)) -> Result<(), FlowJsException> {
    let use_t: UseT<Context<'cx>> = UseT::new(UseTInner::UseT(unknown_use(), u));
    flow_typing_flow_js::speculation_kit::try_singleton_throw_on_failure(
        cx,
        DepthTrace::dummy_trace(),
        l,
        use_t,
    )
}

pub fn is_flow_successful<'cx>(
    cx: &Context<'cx>,
    t: Type,
    u: UseT<Context<'cx>>,
) -> Result<bool, FlowJsException> {
    match flow_typing_flow_js::speculation_kit::try_singleton_throw_on_failure(
        cx,
        DepthTrace::dummy_trace(),
        t,
        u,
    ) {
        Ok(()) => Ok(true),
        Err(FlowJsException::SpeculationSingletonError) => Ok(false),
        Err(other) => Err(other),
    }
}

pub fn is_subtyping_successful<'cx>(
    cx: &Context<'cx>,
    l: Type,
    u: Type,
) -> Result<bool, FlowJsException> {
    let use_t: UseT<Context<'cx>> = UseT::new(UseTInner::UseT(unknown_use(), u));
    is_flow_successful(cx, l, use_t)
}

pub fn resolved_lower_flow_unsafe<'cx>(
    cx: &Context<'cx>,
    r: &Reason,
    (l, u): (&Type, &UseT<Context<'cx>>),
) -> Result<(), FlowJsException> {
    let concrete_types = FlowJs::possible_concrete_types_for_inspection(cx, r, l)?;
    match concrete_types.as_slice() {
        [] => Ok(()),
        [l] => Ok(flow_typing_flow_js::flow_js::flow(cx, (l, u))?),
        [l0, ls @ ..] => {
            let mut successful = is_flow_successful(cx, l0.dupe(), u.dupe())?;
            for l in ls {
                let r = is_flow_successful(cx, l.dupe(), u.dupe())?;
                successful = successful || r;
            }
            if !successful {
                Err(FlowJsException::SpeculationSingletonError)
            } else {
                Ok(())
            }
        }
    }
}

pub fn resolved_lower_flow_t_unsafe<'cx>(
    cx: &Context<'cx>,
    r: &Reason,
    (l, u): (&Type, &Type),
) -> Result<(), FlowJsException> {
    let use_t = UseT::new(UseTInner::UseT(unknown_use(), u.dupe()));
    resolved_lower_flow_unsafe(cx, r, (l, &use_t))
}

pub fn resolved_upper_flow_t_unsafe<'cx>(
    cx: &Context<'cx>,
    r: &Reason,
    (l, u): (&Type, &Type),
) -> Result<(), FlowJsException> {
    let concrete_types = FlowJs::possible_concrete_types_for_inspection(cx, r, u)?;
    match concrete_types.as_slice() {
        [] => Ok(()),
        [u] => Ok(flow_typing_flow_js::flow_js::flow_t(cx, (l, u))?),
        [u0, us @ ..] => {
            let mut successful = is_flow_successful(
                cx,
                l.dupe(),
                UseT::new(UseTInner::UseT(unknown_use(), u0.dupe())),
            )?;
            for u in us {
                let r = is_flow_successful(
                    cx,
                    l.dupe(),
                    UseT::new(UseTInner::UseT(unknown_use(), u.dupe())),
                )?;
                successful = successful || r;
            }
            if !successful {
                Err(FlowJsException::SpeculationSingletonError)
            } else {
                Ok(())
            }
        }
    }
}

pub fn get_method_type_unsafe<'cx>(
    cx: &Context<'cx>,
    t: &Type,
    reason: Reason,
    propref: PropRef,
) -> Result<Type, FlowJsException> {
    let t = t.dupe();
    let reason2 = reason.dupe();
    flow_typing_tvar::mk_where(cx, reason, move |cx, prop_t| {
        let use_t = UseT::new(UseTInner::MethodT(Box::new(MethodTData {
            use_op: unknown_use(),
            reason: reason2.dupe(),
            prop_reason: reason2.dupe(),
            propref: Box::new(propref),
            method_action: Box::new(MethodAction::NoMethodAction(prop_t.dupe())),
        })));
        resolved_lower_flow_unsafe(cx, &reason2, (&t, &use_t))
    })
}

pub fn get_method_type_opt<'cx>(
    cx: &Context<'cx>,
    t: &Type,
    reason: Reason,
    propref: PropRef,
) -> Result<Option<Type>, FlowJsException> {
    match get_method_type_unsafe(cx, t, reason, propref) {
        Err(FlowJsException::SpeculationSingletonError) => Ok(None),
        Ok(t) => Ok(Some(t)),
        Err(e) => Err(e),
    }
}
