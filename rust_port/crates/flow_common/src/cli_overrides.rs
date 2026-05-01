/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Carries CLI-only overrides that are not directly reflected on `Options` but
// must survive through the daemonized monitor and server processes so that
// `make_options` can re-derive a faithful `Options` in the child. Mirrors the
// subset of OCaml `CommandUtils.Options_flags.t` that is consumed by
// `make_options` but NOT round-trippable from the resulting `Options.t`.
#[derive(Clone, Debug, Default, serde::Serialize, serde::Deserialize)]
pub struct CliOverrides {
    pub max_warnings: Option<i32>,
    pub no_autoimports: bool,
    pub flowconfig_ignores: Vec<String>,
    pub flowconfig_includes: Vec<String>,
    pub flowconfig_libs: Vec<String>,
    pub flowconfig_raw_lint_severities: Vec<String>,
    pub flowconfig_untyped: Vec<String>,
    pub flowconfig_declarations: Vec<String>,
}
