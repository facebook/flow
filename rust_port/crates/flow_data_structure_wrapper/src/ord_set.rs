/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A wrapper around im::OrdSet that implements Dupe for cheap cloning.
//!
//! im::OrdSet is an immutable ordered set that uses structural sharing for efficient
//! cloning. This wrapper adds the Dupe trait, allowing consistent use of `.dupe()`
//! instead of `.clone()` throughout the Flow codebase.

use std::ops::Deref;
use std::ops::DerefMut;

use dupe::Dupe;
pub use im::ordset::ConsumingIter;
// Re-export DiffItem for checked_set.rs usage
pub use im::ordset::DiffItem;
pub use im::ordset::Iter;

/// A wrapper around im::OrdSet that implements Dupe for cheap cloning.
///
/// This type provides the same functionality as im::OrdSet but also implements
/// the Dupe trait, making it consistent with other types in the Flow codebase
/// that use `.dupe()` for cheap copies.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FlowOrdSet<T: Ord + Clone>(im::OrdSet<T>);

impl<T: Ord + Clone> PartialOrd for FlowOrdSet<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T: Ord + Clone> Ord for FlowOrdSet<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.iter().cmp(other.0.iter())
    }
}

impl<T: Ord + Clone> Default for FlowOrdSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Ord + Clone> Dupe for FlowOrdSet<T> {}

impl<T: Ord + Clone> FlowOrdSet<T> {
    pub fn new() -> Self {
        FlowOrdSet(im::OrdSet::new())
    }

    pub fn singleton(v: T) -> Self {
        FlowOrdSet(im::OrdSet::new().update(v))
    }

    pub fn into_inner(self) -> im::OrdSet<T> {
        self.0
    }

    pub fn as_inner(&self) -> &im::OrdSet<T> {
        &self.0
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }

    pub fn union(self, other: Self) -> Self {
        if self.0.is_empty() {
            return other;
        }
        if other.0.is_empty() {
            return self;
        }
        if self.0.ptr_eq(&other.0) {
            return self;
        }
        FlowOrdSet(self.0.union(other.0))
    }
}

impl<T: Ord + Clone> Deref for FlowOrdSet<T> {
    type Target = im::OrdSet<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Ord + Clone> DerefMut for FlowOrdSet<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Ord + Clone> From<im::OrdSet<T>> for FlowOrdSet<T> {
    fn from(s: im::OrdSet<T>) -> Self {
        FlowOrdSet(s)
    }
}

impl<T: Ord + Clone> From<FlowOrdSet<T>> for im::OrdSet<T> {
    fn from(s: FlowOrdSet<T>) -> Self {
        s.0
    }
}

impl<T: Ord + Clone> FromIterator<T> for FlowOrdSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        FlowOrdSet(im::OrdSet::from_iter(iter))
    }
}

impl<T: Ord + Clone> IntoIterator for FlowOrdSet<T> {
    type Item = T;
    type IntoIter = im::ordset::ConsumingIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T: Ord + Clone> IntoIterator for &'a FlowOrdSet<T> {
    type Item = &'a T;
    type IntoIter = im::ordset::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
