/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::subst_name::SubstName;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;
use flow_typing_context::Context;
use flow_typing_loc_env::func_class_sig_types::ConfigTypes;
use flow_typing_type::type_::FunParam;
use flow_typing_type::type_::FunRestParam;
use flow_typing_type::type_::Type;
use flow_typing_utils::abnormal::CheckExprError;

pub trait Config: ConfigTypes {
    fn param_type(param: &Self::Param) -> FunParam;
    fn rest_type(rest: &Self::Rest) -> FunRestParam;
    fn this_type(this: &Self::ThisParam) -> Type;
    fn is_param_type_annotated(param: &Self::Param) -> bool;
    fn is_rest_type_annotated(rest: &Self::Rest) -> bool;
    fn subst_param<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        param: &Self::Param,
    ) -> Self::Param;
    fn subst_rest<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        rest: &Self::Rest,
    ) -> Self::Rest;
    fn subst_this<'a>(
        cx: &Context<'a>,
        map: &FlowOrdMap<SubstName, Type>,
        this: &Self::ThisParam,
    ) -> Self::ThisParam;
    fn eval_param<'a>(
        cx: &Context<'a>,
        param: &Self::Param,
    ) -> Result<Self::ParamAst, CheckExprError>;
    fn eval_rest<'a>(cx: &Context<'a>, rest: &Self::Rest) -> Self::RestAst;
    fn eval_this<'a>(cx: &Context<'a>, this: &Self::ThisParam) -> Self::ThisAst;
}
