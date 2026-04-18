/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mirrors the OCaml split between `src/stubs/startup_initializer.ml` (OSS,
// `init () = ()`) and `src/facebook/startup_initializer/startup_initializer.ml`
// (internal, real one-time monitor startup work). The internal arm is wired
// in once that file has been ported into `crates/facebook/`.
#[cfg(fbcode_build)]
pub fn init() {}

#[cfg(not(fbcode_build))]
pub fn init() {}
