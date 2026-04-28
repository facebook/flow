/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! This module defines a small data structure that stores function parameters
//! before substitution. This is used as part of Func_sig (and Class_sig) to hold
//! constraints at bay until substitution can occur.
//!
//! Function params serve two purposes: On one hand, they describe the arguments
//! that a function expects. On the other, the bindings that exist within the
//! body of a function. These may not be the same due to default values and
//! destructuring.

use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_typing_context::Context;
use flow_typing_loc_env::func_class_sig_types::ConfigTypes;
use flow_typing_loc_env::func_class_sig_types::param;
use flow_typing_type::type_::FunParam;
use flow_typing_type::type_::FunRestParam;
use flow_typing_type::type_::Type;
use flow_typing_utils::abnormal::CheckExprError;

use crate::func_params_intf::*;
use crate::func_stmt_config;

pub type FuncStmtConfig = flow_typing_loc_env::func_class_sig_types::StmtConfigTypes;

impl Config for FuncStmtConfig {
    fn param_type(param: &Self::Param) -> FunParam {
        func_stmt_config::param_type(param)
    }

    fn rest_type(rest: &Self::Rest) -> FunRestParam {
        func_stmt_config::rest_type(rest)
    }

    fn this_type(this: &Self::ThisParam) -> Type {
        func_stmt_config::this_type(this)
    }

    fn is_param_type_annotated(param: &Self::Param) -> bool {
        func_stmt_config::is_param_type_annotated(param)
    }

    fn is_rest_type_annotated(rest: &Self::Rest) -> bool {
        func_stmt_config::is_rest_type_annotated(rest)
    }

    fn subst_param<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        param: &Self::Param,
    ) -> Self::Param {
        func_stmt_config::subst_param(cx, map, param)
    }

    fn subst_rest<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        rest: &Self::Rest,
    ) -> Self::Rest {
        func_stmt_config::subst_rest(cx, map, rest)
    }

    fn subst_this<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        this: &Self::ThisParam,
    ) -> Self::ThisParam {
        func_stmt_config::subst_this(cx, map, this)
    }

    fn eval_param<'a>(
        cx: &Context<'a>,
        param: &Self::Param,
    ) -> Result<Self::ParamAst, CheckExprError> {
        func_stmt_config::eval_param(cx, param)
    }

    fn eval_rest<'a>(cx: &Context<'a>, rest: &Self::Rest) -> Self::RestAst {
        func_stmt_config::eval_rest(cx, rest)
    }

    fn eval_this<'a>(cx: &Context<'a>, this: &Self::ThisParam) -> Self::ThisAst {
        func_stmt_config::eval_this(cx, this)
    }
}

pub fn empty<C: ConfigTypes>(reconstruct: param::Reconstruct<C>) -> param::Param<C> {
    param::Param {
        params: Vec::new(),
        rest: None,
        this_: None,
        reconstruct,
    }
}

pub fn add_param<C: ConfigTypes>(p: C::Param, x: &mut param::Param<C>) {
    x.params.push(p);
}

pub fn add_rest<C: ConfigTypes>(r: C::Rest, x: &mut param::Param<C>) {
    x.rest = Some(r);
}

pub fn add_this<C: ConfigTypes>(t: C::ThisParam, x: &mut param::Param<C>) {
    x.this_ = Some(t);
}

pub fn all_params_annotated<C: Config>(params: &[C::Param], rest: Option<&C::Rest>) -> bool {
    params.iter().all(C::is_param_type_annotated) && rest.is_none_or(C::is_rest_type_annotated)
}

pub fn value<C: Config>(params: &[C::Param]) -> Vec<FunParam> {
    params.iter().map(C::param_type).collect()
}

pub fn rest<C: Config>(rest: Option<&C::Rest>) -> Option<FunRestParam> {
    rest.map(C::rest_type)
}

pub fn this<C: Config>(this: Option<&C::ThisParam>) -> Option<Type> {
    this.map(C::this_type)
}

pub fn eval<'a, C: Config, R>(
    cx: &Context<'a>,
    params: &[C::Param],
    rest: Option<&C::Rest>,
    this: Option<&C::ThisParam>,
    reconstruct: impl Fn(Vec<C::ParamAst>, Option<C::RestAst>, Option<C::ThisAst>) -> R,
) -> Result<R, CheckExprError> {
    let param_tasts: Vec<_> = params
        .iter()
        .map(|p| C::eval_param(cx, p))
        .collect::<Result<_, _>>()?;
    let rest_tast = rest.map(|r| C::eval_rest(cx, r));
    let this_tast = this.map(|t| C::eval_this(cx, t));
    Ok(reconstruct(param_tasts, rest_tast, this_tast))
}
