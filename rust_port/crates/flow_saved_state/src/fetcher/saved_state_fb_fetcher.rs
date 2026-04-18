/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::options::Options;

use super::saved_state_fetcher::FetchResult;

// Mirrors the OCaml split between `src/stubs/saved_state_fb_fetcher.ml`
// (which `include`s `Saved_state_dummy_fetcher`) and
// `src/facebook/saved_state/saved_state_fb_fetcher.ml` (which fetches via the
// internal HTTP client). The internal arm is wired in once that file is ported
// into `crates/facebook/flow_facebook_saved_state`.
#[cfg(fbcode_build)]
pub fn fetch(_options: &Options) -> FetchResult {
    FetchResult::No_saved_state
}

#[cfg(not(fbcode_build))]
pub fn fetch(_options: &Options) -> FetchResult {
    FetchResult::No_saved_state
}
