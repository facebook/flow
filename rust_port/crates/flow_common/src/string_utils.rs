/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// Escapes special characters to make the given string a valid filename
pub fn filename_escape(path: &str) -> String {
    let mut buf = String::with_capacity(path.len());
    for ch in path.chars() {
        match ch {
            '\\' => buf.push_str("zB"),
            ':' => buf.push_str("zC"),
            '/' => buf.push_str("zS"),
            '\x00' => buf.push_str("z0"),
            'z' => buf.push_str("zZ"),
            _ => buf.push(ch),
        }
    }
    buf
}

pub fn truncate(len: usize, s: &str) -> &str {
    if s.len() <= len {
        s
    } else {
        let mut end = len;
        while end > 0 && !s.is_char_boundary(end) {
            end -= 1;
        }
        &s[..end]
    }
}
