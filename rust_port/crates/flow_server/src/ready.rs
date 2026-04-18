/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

pub fn signal_ready_file(ready_path: Option<&str>) {
    if let Some(ready_path) = ready_path {
        if let Some(parent) = Path::new(ready_path).parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                log::warn!(
                    "Failed to create parent directory for ready file {}: {}",
                    ready_path,
                    e
                );
            }
        }
        if let Err(e) = std::fs::write(ready_path, "ready") {
            log::warn!("Failed to write ready file {}: {}", ready_path, e);
        }
    }
}
