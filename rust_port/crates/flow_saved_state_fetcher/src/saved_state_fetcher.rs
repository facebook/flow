/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::path::PathBuf;

use flow_common::options::Options;

#[allow(non_camel_case_types)]
#[derive(Debug, Clone)]
pub enum FetchResult {
    // We successfully found saved state. Yay!
    Saved_state {
        saved_state_filename: PathBuf,
        changed_files: BTreeSet<String>,
    },
    // We did not attempt to find saved state.
    No_saved_state,
    // We should have been able to find saved state , but for some reason we could not.
    Saved_state_error(String),
}

#[allow(non_camel_case_types)]
pub trait FETCHER {
    fn fetch(options: &Options) -> FetchResult;
}
