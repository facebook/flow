/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::options::Options;

use crate::saved_state_fetcher::FetchResult;

// This saved state fetcher is used by servers which don't intend to use saved state
pub fn fetch(_options: &Options) -> FetchResult {
    FetchResult::No_saved_state
}
