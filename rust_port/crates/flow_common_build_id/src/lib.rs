/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::OnceLock;

use flow_common_xx as xx;

static BUILD_ID: OnceLock<String> = OnceLock::new();

pub fn get_build_id() -> String {
    BUILD_ID
        .get_or_init(|| {
            let mut state = xx::State::new(0);
            let executable = std::env::current_exe().expect("failed to get executable path");
            let contents = std::fs::read(&executable).unwrap_or_else(|err| {
                panic!(
                    "failed to read executable at {} for flow build id: {}",
                    executable.display(),
                    err
                )
            });
            state.update(&contents);
            let hash = format!("{:016x}", state.digest());
            hash
        })
        .clone()
}
