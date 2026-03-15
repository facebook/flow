/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;

use dupe::Dupe;

pub trait LocSig: Clone + Eq + Ord + Dupe + std::hash::Hash + std::fmt::Debug {
    fn none() -> Self;
    fn compare(&self, other: &Self) -> Ordering;
    fn equal(&self, other: &Self) -> bool;
    fn debug_to_string(&self, include_source: bool) -> String;
}
