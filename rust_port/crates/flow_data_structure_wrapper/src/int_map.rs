/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Fast HashMap/HashSet for integer keys using an identity hasher.
//!
//! Standard HashMap uses SipHash which is overkill for integer keys.
//! This module provides IntHashMap and IntHashSet that use a simple
//! identity hasher, eliminating hash computation overhead entirely.

use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::BuildHasherDefault;
use std::hash::Hasher;

/// A hasher that uses the integer value directly as the hash.
/// Only valid for integer types (i32, u32, i64, u64, usize, etc.).
#[derive(Default)]
pub struct IdentityHasher(u64);

impl Hasher for IdentityHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.0
    }

    #[inline]
    fn write(&mut self, bytes: &[u8]) {
        // For small integer types, read directly
        match bytes.len() {
            4 => self.0 = u32::from_ne_bytes(bytes.try_into().unwrap()) as u64,
            8 => self.0 = u64::from_ne_bytes(bytes.try_into().unwrap()),
            _ => {
                // Fallback: fold bytes
                for &b in bytes {
                    self.0 = self.0.wrapping_mul(31).wrapping_add(b as u64);
                }
            }
        }
    }

    #[inline]
    fn write_i32(&mut self, i: i32) {
        self.0 = i as u64;
    }

    #[inline]
    fn write_u32(&mut self, i: u32) {
        self.0 = i as u64;
    }

    #[inline]
    fn write_i64(&mut self, i: i64) {
        self.0 = i as u64;
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.0 = i;
    }

    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.0 = i as u64;
    }
}

/// A HashMap optimized for integer keys (i32, u32, etc.).
/// Uses identity hashing instead of SipHash for ~2x faster operations.
pub type IntHashMap<K, V> = HashMap<K, V, BuildHasherDefault<IdentityHasher>>;

/// A HashSet optimized for integer keys.
pub type IntHashSet<K> = HashSet<K, BuildHasherDefault<IdentityHasher>>;
