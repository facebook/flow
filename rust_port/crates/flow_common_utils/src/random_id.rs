/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Do not use / in random ids as they appear in filenames.
pub const ALPHANUMERIC_ALPHABET: &str =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";

/// Generates a random string containing characters from [alphabet].
/// Be sure to initialize [Random] (e.g. [Random.self_init ()]).
pub fn short_string_with_alphabet(alphabet: &str) -> String {
    let mask_30: u64 = (1u64 << 30) - 1;
    let hi: u64 = (rand::random::<u64>()) & mask_30;
    let lo: u64 = (rand::random::<u64>()) & mask_30;
    let mut r: u64 = (hi << 30) | lo;
    let mut cs: String = String::new();
    while r > 0 {
        let c: char = alphabet.as_bytes()[(r as usize) % alphabet.len()] as char;
        cs.push(c);
        r >>= 6;
    }
    cs.chars().rev().collect()
}

/// Generates a random alphanumeric string.
/// Be sure to initialize [Random] (e.g. [Random.self_init ()]).
pub fn short_string() -> String {
    short_string_with_alphabet(ALPHANUMERIC_ALPHABET)
}
