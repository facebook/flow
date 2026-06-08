/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use flow_common::files;
use flow_common::options::Options;

use crate::saved_state_fetcher::FetchResult;

/* This saved state fetcher is intended to be used mainly by tests. It assumes that there are 2
 * files next to /path/to/root/.flowconfig:
 *
 * /path/to/root/.flow.saved_state - the saved state file
 * /path/to/root/.flow.saved_state_file_changes - newline separated list of files which have changed
 *
 * if either file doesn't exist then we assume there's no saved state
 */
pub fn fetch(options: &Options) -> FetchResult {
    let root = &*options.root;
    let saved_state_file = root.join(".flow.saved_state");
    let changed_files_input_file = root.join(".flow.saved_state_file_changes");

    let mut errs = Vec::new();
    if !saved_state_file.exists() {
        errs.push(format!("File {:?} does not exist", saved_state_file));
    }
    if !changed_files_input_file.exists() {
        errs.push(format!(
            "File {:?} does not exist",
            changed_files_input_file
        ));
    }
    if !errs.is_empty() {
        return FetchResult::Saved_state_error(errs.join("; "));
    }

    let changed_files = match std::fs::read_to_string(&changed_files_input_file) {
        Ok(contents) => {
            let root_str = root.to_string_lossy();
            let filenames: Vec<String> = contents.lines().map(str::to_owned).collect();
            files::canonicalize_filenames(&root_str, &files::imaginary_realpath, &filenames)
                .into_iter()
                .collect::<BTreeSet<_>>()
        }
        Err(err) => return FetchResult::Saved_state_error(err.to_string()),
    };

    FetchResult::Saved_state {
        saved_state_filename: saved_state_file,
        changed_files,
    }
}
