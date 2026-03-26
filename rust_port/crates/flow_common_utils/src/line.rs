/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Line Separator (0xE2 0x80 0xA8) or Paragraph Separator (0xE2 0x80 0xA9)
fn is_ls_or_ps(bytes: &[u8], len: usize, i: usize) -> bool {
    bytes[i] == 0xE2
        && i + 2 < len
        && bytes[i + 1] == 0x80
        && (bytes[i + 2] == 0xA8 || bytes[i + 2] == 0xA9)
}

fn length_of_line_terminator(bytes: &[u8], len: usize, i: usize) -> usize {
    if bytes[i] == b'\n' {
        1
    } else if bytes[i] == b'\r' {
        if i + 1 < len && bytes[i + 1] == b'\n' {
            2
        } else {
            1
        }
    } else if is_ls_or_ps(bytes, len, i) {
        3
    } else {
        0
    }
}

// Finds the index of the first character of the nth line (0-based).
//
// Assumes a UTF-8 encoding, and treats \n, \r, U+2028 (line separator) and
// U+2029 (paragraph separator) as line terminators, per the ECMAscript spec:
// https://tc39.es/ecma262/#sec-line-terminators
//
// If the line is the last one, and it is empty, then returns [len]. (e.g.
// "foo\n" for n=1, i=0 returns `4` because the index is the end of the string)
//
// If the line is out of bounds, then returns [None].
fn nth_line_opt(n: usize, bytes: &[u8], len: usize, mut i: usize) -> Option<usize> {
    let mut n = n;
    loop {
        if n == 0 {
            return Some(i);
        } else if i >= len {
            return None;
        } else {
            let x = length_of_line_terminator(bytes, len, i);
            if x > 0 {
                n -= 1;
                i += x;
            } else {
                i += 1;
            }
        }
    }
}

// Split string at nth line. If it exists, returns pre, line, post
pub fn split_nth(s: &str, n: usize) -> Option<(&str, &str, &str)> {
    let bytes = s.as_bytes();
    let len = bytes.len();
    match nth_line_opt(n, bytes, len, 0) {
        Some(i) => {
            let j = match nth_line_opt(1, bytes, len, i) {
                Some(j) => j,
                None => len,
            };
            Some((&s[..i], &s[i..j], &s[j..]))
        }
        None => None,
    }
}

// Transform nth line, if it exists. Returns reconstructed string
pub fn transform_nth(s: &str, n: usize, f: impl FnOnce(&str) -> String) -> String {
    match split_nth(s, n) {
        Some((pre, s, post)) => format!("{}{}{}", pre, f(s), post),
        None => s.to_string(),
    }
}

// Find (line, col) of a byte offset. Returns None if offset > string length
pub fn position_of_offset(s: &str, offset: usize) -> Option<(i32, i32)> {
    let bytes = s.as_bytes();
    let len = bytes.len();
    if offset >= len {
        return None;
    }
    let mut line: i32 = 1;
    let mut column: i32 = 0;
    let mut i: usize = 0;
    loop {
        if i == offset {
            return Some((line, column));
        }
        let eol = length_of_line_terminator(bytes, offset, i);
        if eol > 0 {
            i += eol;
            line += 1;
            column = 0;
        } else {
            i += 1;
            column += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_nth_basic() {
        assert_eq!(
            split_nth("abc\ndef\nghi", 0),
            Some(("", "abc\n", "def\nghi"))
        );
        assert_eq!(
            split_nth("abc\ndef\nghi", 1),
            Some(("abc\n", "def\n", "ghi"))
        );
        assert_eq!(
            split_nth("abc\ndef\nghi", 2),
            Some(("abc\ndef\n", "ghi", ""))
        );
        assert_eq!(split_nth("abc\ndef\nghi", 3), None);
    }

    #[test]
    fn test_split_nth_trailing_newline() {
        assert_eq!(split_nth("foo\n", 1), Some(("foo\n", "", "")));
    }

    #[test]
    fn test_split_nth_crlf() {
        assert_eq!(split_nth("abc\r\ndef", 0), Some(("", "abc\r\n", "def")));
        assert_eq!(split_nth("abc\r\ndef", 1), Some(("abc\r\n", "def", "")));
    }

    #[test]
    fn test_position_of_offset() {
        assert_eq!(position_of_offset("abc\ndef", 0), Some((1, 0)));
        assert_eq!(position_of_offset("abc\ndef", 1), Some((1, 1)));
        assert_eq!(position_of_offset("abc\ndef", 2), Some((1, 2)));
        assert_eq!(position_of_offset("abc\ndef", 3), Some((1, 3)));
        assert_eq!(position_of_offset("abc\ndef", 4), Some((2, 0)));
        assert_eq!(position_of_offset("abc\ndef", 5), Some((2, 1)));
        assert_eq!(position_of_offset("abc\ndef", 6), Some((2, 2)));
        assert_eq!(position_of_offset("abc\ndef", 7), None);
    }

    #[test]
    fn test_position_of_offset_ls_ps() {
        // U+2028 (Line Separator) = E2 80 A8
        let s = "ab\u{2028}cd";
        assert_eq!(position_of_offset(s, 0), Some((1, 0)));
        assert_eq!(position_of_offset(s, 1), Some((1, 1)));
        assert_eq!(position_of_offset(s, 2), Some((1, 2)));
        assert_eq!(position_of_offset(s, 5), Some((2, 0)));
    }

    #[test]
    fn test_transform_nth() {
        assert_eq!(
            transform_nth("abc\ndef\nghi", 1, |s| s.to_uppercase()),
            "abc\nDEF\nghi"
        );
        assert_eq!(
            transform_nth("abc\ndef", 5, |s| s.to_uppercase()),
            "abc\ndef"
        );
    }
}
