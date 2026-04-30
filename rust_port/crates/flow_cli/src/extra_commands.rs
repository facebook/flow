/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::OnceLock;

use crate::command_spec;

static EXTRA_COMMANDS: OnceLock<fn() -> Vec<command_spec::Command>> = OnceLock::new();

pub fn register_extra_commands(f: fn() -> Vec<command_spec::Command>) {
    EXTRA_COMMANDS.set(f).unwrap();
}

pub(crate) fn extra_commands() -> Vec<command_spec::Command> {
    match EXTRA_COMMANDS.get() {
        Some(f) => f(),
        None => vec![],
    }
}

static DOCS_URL: OnceLock<&'static str> = OnceLock::new();

pub fn register_docs_url(url: &'static str) {
    DOCS_URL.set(url).unwrap();
}

pub(crate) fn docs_url() -> &'static str {
    DOCS_URL
        .get()
        .copied()
        .unwrap_or("https://flow.org/en/docs/")
}
