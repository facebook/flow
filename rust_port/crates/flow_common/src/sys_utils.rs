/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::borrow::Cow;
use std::path;
use std::path::Path;

/// Emulate "mkdir -p", i.e., no error if already exists.
pub fn mkdir_no_fail(dir: &Path) {
    let result = std::fs::DirBuilder::new().create(dir);
    match result {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::AlreadyExists => {}
        Err(e) => panic!("mkdir_no_fail: failed to create directory {:?}: {}", dir, e),
    }
}

/// Like Python's os.path.expanduser, though probably doesn't cover some cases.
/// Roughly follow's bash's tilde expansion:
/// http://www.gnu.org/software/bash/manual/html_node/Tilde-Expansion.html
///
/// ~/foo -> /home/bob/foo if $HOME = "/home/bob"
/// ~joe/foo -> /home/joe/foo if joe's home is /home/joe
pub fn expanduser(path: &str) -> String {
    if let Some(rest) = path.strip_prefix("~/") {
        if let Ok(home) = std::env::var("HOME") {
            return format!("{}/{}", home, rest);
        }
    }
    path.to_string()
}

/// Converts platform-specific directory separators to /
///
/// On Unix systems where the directory separator is already '/', this returns
/// the input without allocation (Cow::Borrowed).
/// On Windows, this converts backslashes to forward slashes (Cow::Owned).
pub fn normalize_filename_dir_sep(path: &str) -> Cow<'_, str> {
    let dir_sep = path::MAIN_SEPARATOR;

    if dir_sep == '/' {
        // No-op on Unix systems - return borrowed string (zero allocation!)
        Cow::Borrowed(path)
    } else {
        // On Windows, convert backslashes to forward slashes
        if path.contains(dir_sep) {
            Cow::Owned(
                path.chars()
                    .map(|c| if c == dir_sep { '/' } else { c })
                    .collect(),
            )
        } else {
            Cow::Borrowed(path)
        }
    }
}
