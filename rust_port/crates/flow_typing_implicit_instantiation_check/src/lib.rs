/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;

use flow_aloc::ALoc;
use flow_common::reason::Reason;
use flow_typing_type::type_::CallArg;
use flow_typing_type::type_::FuncallType;
use flow_typing_type::type_::Targ;
use flow_typing_type::type_::Type;
use flow_typing_type::type_::TypeParam;
use flow_typing_type::type_::UseOp;
use vec1::Vec1;

pub type PolyT = (ALoc, Vec1<TypeParam>, Type);

#[derive(Debug, Clone)]
pub enum Operation {
    SubtypeLowerPoly(Type),
    Call(FuncallType),
    Constructor(Option<Rc<[Targ]>>, Rc<[CallArg]>),
    ReactJSX {
        component: Type,
        jsx_props: Type,
        targs: Option<Rc<[Targ]>>,
        should_generalize: bool,
    },
}

#[derive(Debug, Clone)]
pub struct ImplicitInstantiationCheck {
    pub lhs: Type,
    pub poly_t: PolyT,
    pub operation: (UseOp, Reason, Operation),
}

impl ImplicitInstantiationCheck {
    pub fn of_call(
        lhs: Type,
        poly_t: PolyT,
        use_op: UseOp,
        reason: Reason,
        funcalltype: FuncallType,
    ) -> Self {
        Self {
            lhs,
            poly_t,
            operation: (use_op, reason, Operation::Call(funcalltype)),
        }
    }

    pub fn of_ctor(
        lhs: Type,
        poly_t: PolyT,
        use_op: UseOp,
        reason_op: Reason,
        targs: Option<Rc<[Targ]>>,
        args: Rc<[CallArg]>,
    ) -> Self {
        Self {
            lhs,
            poly_t,
            operation: (use_op, reason_op, Operation::Constructor(targs, args)),
        }
    }

    pub fn of_react_jsx(
        lhs: Type,
        poly_t: PolyT,
        use_op: UseOp,
        reason_op: Reason,
        component: Type,
        jsx_props: Type,
        targs: Option<Rc<[Targ]>>,
        should_generalize: bool,
    ) -> Self {
        Self {
            lhs,
            poly_t,
            operation: (
                use_op,
                reason_op,
                Operation::ReactJSX {
                    component,
                    targs,
                    jsx_props,
                    should_generalize,
                },
            ),
        }
    }
}
