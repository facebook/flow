/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::command_spec;

// Mirrors the OCaml split between `src/stubs/extra_commands.ml` (OSS) and
// `src/facebook/extra/extra_commands.ml` (internal). The internal arm is wired
// in once the OCaml `flow_extra_commands_fb` library has been ported into
// `crates/facebook/flow_facebook_extra`.
#[cfg(fbcode_build)]
pub(crate) fn extra_commands() -> Vec<command_spec::Command> {
    vec![]
}

#[cfg(not(fbcode_build))]
pub(crate) fn extra_commands() -> Vec<command_spec::Command> {
    vec![]
}
