/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_typing_context::Context;
use flow_typing_flow_common::flow_js_utils::FlowJsException;
use flow_typing_type::type_::DepthTrace;

/// Recursion limiter. We proxy recursion depth with trace depth,
/// which is either equal or pretty close.
/// When check is called with a trace whose depth exceeds a constant
/// limit, we throw a LimitExceeded exception.
pub fn check(cx: &Context, trace: DepthTrace) -> Result<(), FlowJsException> {
    if trace.depth() >= cx.recursion_limit() as u32 {
        Err(FlowJsException::LimitExceeded)
    } else {
        Ok(())
    }
}
