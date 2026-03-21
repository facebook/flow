/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use dupe::Dupe;

#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    PartialOrd,
    Ord,
    Dupe,
    serde::Serialize,
    serde::Deserialize
)]
pub struct Bitset(u32);

const MAX_SIZE: usize = 32;

impl Bitset {
    /// Asserts that the logical size is within supported bounds
    fn assert_within_supported_size(logical_size: usize) {
        if logical_size > MAX_SIZE {
            panic!("logical_size {} >= {}", logical_size, MAX_SIZE);
        }
    }

    /// Creates a bitset with all bits set to zero
    pub fn all_zero(logical_size: usize) -> Self {
        Self::assert_within_supported_size(logical_size);
        Bitset(0)
    }

    /// Creates a bitset with all bits set to one up to logical_size
    pub fn all_one(logical_size: usize) -> Self {
        Self::assert_within_supported_size(logical_size);
        Bitset((1u32 << logical_size) - 1)
    }

    /// Checks if a bit at the given index is set
    pub fn mem(&self, logical_index: usize) -> bool {
        (self.0 & (1u32 << logical_index)) != 0
    }

    /// Sets a bit at the given index to 1
    pub fn set(&self, logical_index: usize) -> Self {
        Bitset(self.0 | (1u32 << logical_index))
    }

    /// Unsets a bit at the given index (sets to 0)
    pub fn unset(&self, logical_index: usize) -> Self {
        Bitset(self.0 & !(1u32 << logical_index))
    }

    /// Checks if bit_set_a is a subset of bit_set_b
    pub fn is_subset(&self, other: &Bitset) -> bool {
        (self.0 | other.0) == other.0
    }

    /// Checks if two bitsets have no overlapping bits
    pub fn no_overlap(&self, other: &Bitset) -> bool {
        (self.0 & other.0) == 0
    }

    /// Creates a bitset from a u32 without validation (unsafe)
    pub fn from_int_unchecked(value: u32) -> Self {
        Bitset(value)
    }

    /// Converts the bitset to a u32
    pub fn to_int(&self) -> u32 {
        self.0
    }
}

impl fmt::Debug for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bitset({})", self.0)
    }
}

impl fmt::Display for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u32> for Bitset {
    fn from(value: u32) -> Self {
        Bitset(value)
    }
}

impl From<Bitset> for u32 {
    fn from(bitset: Bitset) -> Self {
        bitset.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_all_zero() {
        let bitset = Bitset::all_zero(10);
        assert_eq!(bitset.to_int(), 0);
    }

    #[test]
    fn test_all_one() {
        let bitset = Bitset::all_one(4);
        assert_eq!(bitset.to_int(), 0b1111);
    }

    #[test]
    fn test_mem() {
        let bitset = Bitset::from_int_unchecked(0b1010);
        assert!(!bitset.mem(0));
        assert!(bitset.mem(1));
        assert!(!bitset.mem(2));
        assert!(bitset.mem(3));
    }

    #[test]
    fn test_set() {
        let bitset = Bitset::all_zero(4);
        let bitset = bitset.set(1);
        assert_eq!(bitset.to_int(), 0b0010);
        let bitset = bitset.set(3);
        assert_eq!(bitset.to_int(), 0b1010);
    }

    #[test]
    fn test_unset() {
        let bitset = Bitset::all_one(4);
        let bitset = bitset.unset(1);
        assert_eq!(bitset.to_int(), 0b1101);
        let bitset = bitset.unset(3);
        assert_eq!(bitset.to_int(), 0b0101);
    }

    #[test]
    fn test_is_subset() {
        let bitset_a = Bitset::from_int_unchecked(0b0101);
        let bitset_b = Bitset::from_int_unchecked(0b1111);
        assert!(bitset_a.is_subset(&bitset_b));
        assert!(!bitset_b.is_subset(&bitset_a));
    }

    #[test]
    fn test_no_overlap() {
        let bitset_a = Bitset::from_int_unchecked(0b0101);
        let bitset_b = Bitset::from_int_unchecked(0b1010);
        assert!(bitset_a.no_overlap(&bitset_b));

        let bitset_c = Bitset::from_int_unchecked(0b0001);
        assert!(!bitset_a.no_overlap(&bitset_c));
    }

    #[test]
    #[should_panic(expected = "logical_size 33 >= 32")]
    fn test_size_too_large() {
        Bitset::all_zero(33);
    }

    #[test]
    fn test_all_one_size_16() {
        let bitset = Bitset::all_one(16);
        for i in 0..16 {
            assert!(bitset.mem(i), "Bit {} should be set in size-16 bitset", i);
        }
    }

    #[test]
    fn test_all_one_size_17() {
        let bitset = Bitset::all_one(17);
        for i in 0..17 {
            assert!(bitset.mem(i), "Bit {} should be set in size-17 bitset", i);
        }
    }

    #[test]
    fn test_start_with_all_zero_size_16() {
        let bitset = Bitset::all_zero(16);
        for i in 0..16 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset in zero-initialized size-16 bitset",
                i
            );
        }
        let bitset = bitset.set(3);
        assert!(bitset.mem(3), "Bit 3 should be set after set(3)");
        assert!(!bitset.mem(4), "Bit 4 should remain unset");
    }

    #[test]
    fn test_start_with_all_zero_size_17() {
        let bitset = Bitset::all_zero(17);
        for i in 0..17 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset in zero-initialized size-17 bitset",
                i
            );
        }
        let bitset = bitset.set(16);
        assert!(bitset.mem(16), "Bit 16 should be set after set(16)");
        assert!(!bitset.mem(15), "Bit 15 should remain unset");
    }

    #[test]
    fn test_start_with_all_one_size_16() {
        let bitset = Bitset::all_one(16);
        for i in 0..16 {
            assert!(
                bitset.mem(i),
                "Bit {} should be set in one-initialized size-16 bitset",
                i
            );
        }
        let bitset = bitset.unset(3);
        assert!(!bitset.mem(3), "Bit 3 should be unset after unset(3)");
        assert!(bitset.mem(4), "Bit 4 should remain set");
    }

    #[test]
    fn test_start_with_all_one_size_17() {
        let bitset = Bitset::all_one(17);
        for i in 0..17 {
            assert!(
                bitset.mem(i),
                "Bit {} should be set in one-initialized size-17 bitset",
                i
            );
        }
        let bitset = bitset.unset(16);
        assert!(!bitset.mem(16), "Bit 16 should be unset after unset(16)");
        assert!(bitset.mem(15), "Bit 15 should remain set");
    }

    #[test]
    fn test_out_of_bounds_all_one_bitset_are_unset() {
        // Test that bits beyond logical_size remain unset even with all_one
        let bitset = Bitset::all_one(17);
        for i in 17..24 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset (beyond logical size 17)",
                i
            );
        }

        let bitset = Bitset::all_one(18);
        for i in 18..24 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset (beyond logical size 18)",
                i
            );
        }

        let bitset = Bitset::all_one(19);
        for i in 19..24 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset (beyond logical size 19)",
                i
            );
        }

        let bitset = Bitset::all_one(20);
        for i in 20..24 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset (beyond logical size 20)",
                i
            );
        }

        let bitset = Bitset::all_one(21);
        for i in 21..24 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset (beyond logical size 21)",
                i
            );
        }

        let bitset = Bitset::all_one(22);
        for i in 22..24 {
            assert!(
                !bitset.mem(i),
                "Bit {} should be unset (beyond logical size 22)",
                i
            );
        }

        let bitset = Bitset::all_one(23);
        assert!(
            !bitset.mem(23),
            "Bit 23 should be unset (beyond logical size 23)"
        );
    }
}
