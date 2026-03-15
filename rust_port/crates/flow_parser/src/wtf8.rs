/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! WTF-8 is a superset of UTF-8 that allows unpaired surrogates.
//!
//! From ES6 6.1.4, "The String Type":
//!
//!   Where ECMAScript operations interpret String values, each element is
//!   interpreted as a single UTF-16 code unit. However, ECMAScript does not
//!   place any restrictions or requirements on the sequence of code units in
//!   a String value, so they may be ill-formed when interpreted as UTF-16 code
//!   unit sequences. Operations that do not interpret String contents treat
//!   them as sequences of undifferentiated 16-bit unsigned integers.
//!
//! If we try to encode these ill-formed code units into UTF-8, we similarly
//! get ill-formed UTF-8. WTF-8 is a fun name for that encoding.
//!
//! https://simonsapin.github.io/wtf-8/

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Codepoint {
    Point(u32),
    Malformed,
}

/// WTF-8 is a variable length encoding. The first byte in each codepoint
/// determines how many other bytes follow.
#[inline]
fn needed_bytes(c: u8) -> usize {
    match c {
        0x00..=0x7F => 1,
        0xC2..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF4 => 4,
        _ => 0,
    }
}

fn codepoint(s: &[u8], i: usize, need: usize) -> u32 {
    match need {
        1 => s[i] as u32,
        2 => {
            let b0 = s[i] as u32;
            let b1 = s[i + 1] as u32;
            ((b0 & 0x1F) << 6) | (b1 & 0x3F)
        }
        3 => {
            let b0 = s[i] as u32;
            let b1 = s[i + 1] as u32;
            let b2 = s[i + 2] as u32;
            ((b0 & 0x0F) << 12) | ((b1 & 0x3F) << 6) | (b2 & 0x3F)
        }
        4 => {
            let b0 = s[i] as u32;
            let b1 = s[i + 1] as u32;
            let b2 = s[i + 2] as u32;
            let b3 = s[i + 3] as u32;
            ((b0 & 0x07) << 18) | ((b1 & 0x3F) << 12) | ((b2 & 0x3F) << 6) | (b3 & 0x3F)
        }
        _ => unreachable!(),
    }
}

/// Iterate over the WTF-8 code units in a string
pub(super) fn iter_wtf_8<A>(
    bytes: &[u8],
    pos: Option<usize>,
    len: Option<usize>,
    init: A,
    mut f: impl FnMut(&mut A, usize, Codepoint),
) -> A {
    let pos = pos.unwrap_or(0);
    let len = len.unwrap_or(bytes.len() - pos);
    let l = pos + len;
    let mut acc = init;

    let mut i = pos;
    while i < l {
        let need = needed_bytes(bytes[i]);
        if need == 0 {
            f(&mut acc, i, Codepoint::Malformed);
            i += 1;
        } else {
            let rem = l - i;
            if rem < need {
                f(&mut acc, i, Codepoint::Malformed);
                break;
            } else {
                f(&mut acc, i, Codepoint::Point(codepoint(bytes, i, need)));
                i += need;
            }
        }
    }

    acc
}
