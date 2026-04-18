/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use dupe::Dupe;

// Stay under Sys.int_size (31 under 32-bit) just to be safe.
// This should be enough for all cases using bitsets in flow.
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
    fn assert_within_supported_size(logical_size: usize) {
        if logical_size > MAX_SIZE {
            panic!("logical_size {} >= {}", logical_size, MAX_SIZE);
        }
    }

    pub fn all_zero(logical_size: usize) -> Self {
        Self::assert_within_supported_size(logical_size);
        Bitset(0)
    }

    pub fn all_one(logical_size: usize) -> Self {
        Self::assert_within_supported_size(logical_size);
        Bitset((1u32 << logical_size) - 1)
    }

    pub fn mem(&self, logical_index: usize) -> bool {
        (self.0 & (1u32 << logical_index)) != 0
    }

    pub fn set(&self, logical_index: usize) -> Self {
        Bitset(self.0 | (1u32 << logical_index))
    }

    pub fn unset(&self, logical_index: usize) -> Self {
        Bitset(self.0 & !(1u32 << logical_index))
    }

    pub fn is_subset(&self, other: &Bitset) -> bool {
        (self.0 | other.0) == other.0
    }

    pub fn no_overlap(&self, other: &Bitset) -> bool {
        (self.0 & other.0) == 0
    }

    pub fn from_int_unchecked(value: u32) -> Self {
        Bitset(value)
    }

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
    fn all_one_size_16() {
        let bitset = Bitset::all_one(16);
        for i in 0..=15 {
            assert!(bitset.mem(i), "{} {}", i, bitset.mem(i));
        }
    }

    #[test]
    fn all_one_size_17() {
        let bitset = Bitset::all_one(17);
        for i in 0..=16 {
            assert!(bitset.mem(i));
        }
    }

    #[test]
    fn start_with_all_zero_size_16() {
        let bitset = Bitset::all_zero(16);
        for i in 0..=15 {
            assert!(!bitset.mem(i));
        }
        let bitset = bitset.set(3);
        assert!(bitset.mem(3));
        assert!(!bitset.mem(4));
    }

    #[test]
    fn start_with_all_zero_size_17() {
        let bitset = Bitset::all_zero(17);
        for i in 0..=16 {
            assert!(!bitset.mem(i));
        }
        let bitset = bitset.set(16);
        assert!(bitset.mem(16));
        assert!(!bitset.mem(15));
    }

    #[test]
    fn start_with_all_one_size_16() {
        let bitset = Bitset::all_one(16);
        for i in 0..=15 {
            assert!(bitset.mem(i));
        }
        let bitset = bitset.unset(3);
        assert!(!bitset.mem(3));
        assert!(bitset.mem(4));
    }

    #[test]
    fn start_with_all_one_size_17() {
        let bitset = Bitset::all_one(17);
        for i in 0..=16 {
            assert!(bitset.mem(i));
        }
        let bitset = bitset.unset(16);
        assert!(!bitset.mem(16));
        assert!(bitset.mem(15));
    }

    #[test]
    fn out_of_bounds_all_one_bitset_are_unset() {
        let bitset = Bitset::all_one(17);
        for i in 17..=23 {
            assert!(!bitset.mem(i));
        }
        let bitset = Bitset::all_one(18);
        for i in 18..=23 {
            assert!(!bitset.mem(i));
        }
        let bitset = Bitset::all_one(19);
        for i in 19..=23 {
            assert!(!bitset.mem(i));
        }
        let bitset = Bitset::all_one(20);
        for i in 20..=23 {
            assert!(!bitset.mem(i));
        }
        let bitset = Bitset::all_one(21);
        for i in 21..=23 {
            assert!(!bitset.mem(i));
        }
        let bitset = Bitset::all_one(22);
        for i in 22..=23 {
            assert!(!bitset.mem(i));
        }
        let bitset = Bitset::all_one(23);
        assert!(!bitset.mem(23));
    }

    #[test]
    fn subset_test() {
        assert!(!Bitset::all_one(31).is_subset(&Bitset::all_zero(31)));
        assert!(Bitset::all_zero(31).is_subset(&Bitset::all_one(31)));
        assert!(Bitset::all_zero(31).is_subset(&Bitset::all_zero(31)));
        assert!(Bitset::all_one(31).is_subset(&Bitset::all_one(31)));
        {
            let a = Bitset::all_zero(21).set(10).set(11).set(17);
            let b = Bitset::all_zero(21).set(3).set(10).set(11).set(17).set(18);
            assert!(a.is_subset(&b));
        }
        {
            let a = Bitset::all_zero(21).set(10).set(11).set(17).set(18);
            let b = Bitset::all_zero(21).set(3).set(10).set(11).set(17);
            assert!(!a.is_subset(&b));
        }
        {
            let a = Bitset::all_zero(21).set(3).set(10).set(11).set(17);
            let b = Bitset::all_zero(21).set(10).set(11).set(17).set(18);
            assert!(!a.is_subset(&b));
        }
    }

    #[test]
    fn no_overlap_test() {
        assert!(Bitset::all_one(31).no_overlap(&Bitset::all_zero(31)));
        assert!(Bitset::all_zero(31).no_overlap(&Bitset::all_one(31)));
        assert!(Bitset::all_zero(31).no_overlap(&Bitset::all_zero(31)));
        assert!(!Bitset::all_one(31).no_overlap(&Bitset::all_one(31)));
        {
            let a = Bitset::all_zero(21).set(10).set(17);
            let b = Bitset::all_zero(21).set(3).set(11).set(18);
            assert!(a.no_overlap(&b));
        }
        {
            let a = Bitset::all_zero(21).set(3).set(11).set(17).set(18);
            let b = Bitset::all_zero(21).set(3);
            assert!(!a.is_subset(&b));
        }
    }
}
