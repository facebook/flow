/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

pub(crate) fn check_entry_point() {
    let entry = match std::env::var("HH_SERVER_DAEMON") {
        Ok(entry) if !entry.is_empty() => entry,
        _ => return,
    };
    let param_file = std::env::var("HH_SERVER_DAEMON_PARAM")
        .ok()
        .filter(|param| !param.is_empty());
    if let Some(param_file) = param_file {
        let _ = std::fs::remove_file(param_file);
    }
    eprintln!(
        "Unsupported daemon entry point '{}': legacy exec-based daemon dispatch is not available in this Rust CLI path",
        entry
    );
    std::process::exit(2);
}
