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

/// Splits a string into a list of strings using "\n", "\r" or "\r\n" as
/// delimiters. If the string starts or ends with a delimiter, there WILL be an
/// empty string at the beginning or end of the list, like [Str.split_delim] does.
pub fn split_into_lines(str: &str) -> Vec<String> {
    // To avoid unnecessary string allocations, we're going to keep a list of
    // the start index of each line and how long it is. Then, at the end, we can
    // use String.sub to create the actual strings.
    let bytes = str.as_bytes();
    let mut idx: usize = 0;
    let mut start: usize = 0;
    let mut lines: Vec<(usize, usize)> = Vec::new();
    for &c in bytes {
        if c == b'\n' && idx > 0 && bytes[idx - 1] == b'\r' {
            idx += 1;
            start = idx;
        } else if c == b'\n' || c == b'\r' {
            lines.push((start, idx - start));
            idx += 1;
            start = idx;
        } else {
            idx += 1;
        }
    }
    let last_start = start;
    // Reverses the list of start,len and turns them into strings
    let mut entries: Vec<(usize, usize)> = Vec::with_capacity(lines.len() + 1);
    entries.push((last_start, str.len() - last_start));
    for entry in lines.iter().rev() {
        entries.push(*entry);
    }
    let mut acc: Vec<String> = Vec::with_capacity(entries.len());
    for (start, len) in entries {
        acc.push(str[start..start + len].to_string());
    }
    acc
}

/// Splits a string into lines, indents each non-empty line, and concats with newlines
pub fn indent(indent_size: usize, str: &str) -> String {
    let padding: String = " ".repeat(indent_size);
    let lines = split_into_lines(str);
    let mapped: Vec<String> = lines
        .into_iter()
        .map(|s| {
            if s.is_empty() {
                String::new()
            } else {
                format!("{}{}", padding, s)
            }
        })
        .collect();
    mapped.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_truncate_short() {
        let s = "hello world";
        let len = s.len();
        let truncated = truncate(len, s);
        assert_eq!(
            "hello world", truncated,
            "truncated to exact length should return the same string"
        );
        let len = len + 10;
        let truncated = truncate(len, s);
        assert_eq!(
            "hello world", truncated,
            "truncated with room for more should return the same string"
        );
    }

    #[test]
    fn test_truncate_long() {
        let s = "hello world";
        let len = 5;
        let truncated = truncate(len, s);
        assert_eq!("hello", truncated, "truncate cuts the string short");
    }
}
