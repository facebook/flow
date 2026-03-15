/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use xxhash_rust::xxh64;

pub struct State(xxh64::Xxh64);

impl State {
    pub fn new(seed: u64) -> Self {
        Self(xxh64::Xxh64::new(seed))
    }

    pub fn update(&mut self, data: &[u8]) {
        self.0.update(data);
    }

    pub fn update_int(&mut self, v: i64) {
        self.0.update(&v.to_ne_bytes());
    }

    pub fn update_int64(&mut self, v: i64) {
        self.0.update(&v.to_ne_bytes());
    }

    pub fn digest(&self) -> u64 {
        self.0.digest()
    }
}

pub fn hash(data: &[u8], seed: u64) -> u64 {
    xxh64::xxh64(data, seed)
}

pub fn modulo(hash: u64, modulus: usize) -> usize {
    assert!(modulus > 0);
    ((hash as i64).unsigned_abs() as usize) % modulus
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn to_string_is_16_chars() {
        // (* let state = Xx.init 0L in *)
        let state = State::new(0);
        let hash = state.digest();
        let str = format!("{:016x}", hash);
        assert_eq!("ef46db3751d8e999", str);
    }

    #[test]
    fn equal_is_equal() {
        // (* let a = *)
        let a = {
            let mut state = State::new(0);
            state.update(b"foo");
            state.digest()
        };
        let b = {
            let mut state = State::new(0);
            state.update(b"foo");
            state.digest()
        };
        assert_eq!(a, b);
    }

    #[test]
    fn equal_not_equal() {
        let a = {
            let mut state = State::new(0);
            state.update(b"foo");
            state.digest()
        };
        let b = {
            let mut state = State::new(0);
            state.update(b"bar");
            state.digest()
        };
        assert_ne!(a, b);
    }
}
