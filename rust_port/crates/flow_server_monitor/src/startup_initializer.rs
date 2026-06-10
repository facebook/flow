/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mirrors the OCaml split between `src/stubs/startup_initializer.ml` (OSS,
// `init () = ()`) and `src/facebook/startup_initializer/startup_initializer.ml`
// (internal, real one-time monitor startup work). The internal arm performs
// `initFacebook` via the `flow_facebook_startup_initializer` crate so the EdenFS
// watcher's Thrift client can obtain the `fbinit::FacebookInit` token it needs.
#[cfg(fbcode_build)]
pub fn init() {
    flow_facebook_startup_initializer::init();
}

#[cfg(not(fbcode_build))]
pub fn init() {}
