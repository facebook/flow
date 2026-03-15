/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A wrapper around im::HashSet that implements Dupe for cheap cloning.
//!
//! im::HashSet is an immutable hash set that uses structural sharing for efficient
//! cloning. This wrapper adds the Dupe trait, allowing consistent use of `.dupe()`
//! instead of `.clone()` throughout the Flow codebase.

use std::hash::Hash;
use std::ops::Deref;
use std::ops::DerefMut;

use dupe::Dupe;
pub use im::hashset::ConsumingIter;
pub use im::hashset::Iter;

/// A wrapper around im::HashSet that implements Dupe for cheap cloning.
///
/// This type provides the same functionality as im::HashSet but also implements
/// the Dupe trait, making it consistent with other types in the Flow codebase
/// that use `.dupe()` for cheap copies.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FlowHashSet<T: Hash + Eq + Clone>(im::HashSet<T>);

impl<T: Hash + Eq + Clone> Default for FlowHashSet<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Hash + Eq + Clone> Dupe for FlowHashSet<T> {}

impl<T: Hash + Eq + Clone> FlowHashSet<T> {
    pub fn new() -> Self {
        FlowHashSet(im::HashSet::new())
    }

    pub fn into_inner(self) -> im::HashSet<T> {
        self.0
    }

    pub fn as_inner(&self) -> &im::HashSet<T> {
        &self.0
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }
}

impl<T: Hash + Eq + Clone> Deref for FlowHashSet<T> {
    type Target = im::HashSet<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Hash + Eq + Clone> DerefMut for FlowHashSet<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Hash + Eq + Clone> From<im::HashSet<T>> for FlowHashSet<T> {
    fn from(s: im::HashSet<T>) -> Self {
        FlowHashSet(s)
    }
}

impl<T: Hash + Eq + Clone> From<FlowHashSet<T>> for im::HashSet<T> {
    fn from(s: FlowHashSet<T>) -> Self {
        s.0
    }
}

impl<T: Hash + Eq + Clone> FromIterator<T> for FlowHashSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        FlowHashSet(im::HashSet::from_iter(iter))
    }
}

impl<T: Hash + Eq + Clone> IntoIterator for FlowHashSet<T> {
    type Item = T;
    type IntoIter = im::hashset::ConsumingIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T: Hash + Eq + Clone> IntoIterator for &'a FlowHashSet<T> {
    type Item = &'a T;
    type IntoIter = im::hashset::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
