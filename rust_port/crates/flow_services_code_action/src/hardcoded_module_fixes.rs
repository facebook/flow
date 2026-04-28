/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mirrors the OCaml split between `src/stubs/hardcoded_module_fixes.ml` (OSS,
// empty list) and `src/facebook/codemods/annotate_exports_hardcoded/hardcoded_module_fixes.ml`
// (internal, hard-coded RelayModern/etc. mappings). The internal arm is wired
// in once that file has been ported into `crates/facebook/`.

// let files_to_modules = []
#[cfg(fbcode_build)]
pub static FILES_TO_MODULES: &[(&str, &str)] = &[];

#[cfg(not(fbcode_build))]
pub static FILES_TO_MODULES: &[(&str, &str)] = &[];
