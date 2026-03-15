/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Various functions used to prepare for and execute speculative matching. *)
//
// Functions used to initialize and add unresolved tvars during type resolution
// of lower/upper bounds of union/intersection types, respectively

use flow_aloc::ALoc;
use flow_typing_context::Context;
use flow_typing_errors::error_message::ErrorMessage;
use flow_typing_speculation_state::Branch as SpeculationBranch;

pub type SpeculationId = i32;

pub fn set_speculative(cx: &Context, branch: SpeculationBranch) {
    cx.speculation_state_mut().0.push(branch);
}

pub fn restore_speculative(cx: &Context) {
    cx.speculation_state_mut().0.pop();
}

pub fn speculating(cx: &Context) -> bool {
    !cx.speculation_state().0.is_empty()
}

pub fn defer_error(cx: &Context, msg: ErrorMessage<ALoc>) {
    let mut state = cx.speculation_state_mut();
    match state.0.last_mut() {
        None => {}
        Some(branch) => {
            branch.case.errors.borrow_mut().push(msg);
        }
    }
}
