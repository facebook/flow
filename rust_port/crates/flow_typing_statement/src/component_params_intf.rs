/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_aloc::ALoc;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_typing_context::Context;
use flow_typing_type::type_::Type;
use flow_typing_utils::abnormal::AbnormalControlFlow;

/// This trait defines the interface for the module that converts from the parameter representation
/// to types. This trait is used by the Params module to reconstruct the typed AST.
///
/// See component_declaration_config.re for an example implementation.
pub trait Config {
    type Param;
    type Rest;
    type ParamAst;
    type RestAst;

    fn eval_param(cx: &Context, param: &Self::Param)
    -> Result<Self::ParamAst, AbnormalControlFlow>;
    fn eval_rest(cx: &Context, rest: &Self::Rest) -> Result<Self::RestAst, AbnormalControlFlow>;
    fn param_type_with_name(param: &Self::Param) -> (ALoc, FlowSmolStr, Type);
    fn rest_type(rest: &Self::Rest) -> Type;
    fn read_react(cx: &Context, loc: ALoc);
}
