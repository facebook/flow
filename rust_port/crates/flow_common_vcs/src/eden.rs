/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

fn is_eden_posix(dir: &Path) -> bool {
    dir.join(".eden").join("root").exists()
}

fn is_eden_win32(recursion_limit: usize, dir: &Path) -> bool {
    let parent_dir = match dir.parent() {
        None => return false,
        Some(p) => p,
    };
    if dir.join(".eden").join("config").exists() {
        true
    } else if dir.join(".hg").exists() || dir.join(".git").exists() || recursion_limit == 0 {
        false
    } else {
        is_eden_win32(recursion_limit - 1, parent_dir)
    }
}

pub fn is_eden(dir: &Path) -> bool {
    if cfg!(windows) {
        is_eden_win32(100, dir)
    } else {
        is_eden_posix(dir)
    }
}
