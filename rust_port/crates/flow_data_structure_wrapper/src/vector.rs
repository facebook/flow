/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A wrapper around im::Vector that implements Dupe for cheap cloning.
//!
//! im::Vector is a persistent vector that uses structural sharing for efficient
//! cloning. This wrapper adds the Dupe trait, allowing consistent use of `.dupe()`
//! instead of `.clone()` throughout the Flow codebase.

use std::ops::Deref;
use std::ops::DerefMut;

use dupe::Dupe;

/// A wrapper around im::Vector that implements Dupe for cheap cloning.
///
/// This type provides the same functionality as im::Vector but also implements
/// the Dupe trait, making it consistent with other types in the Flow codebase
/// that use `.dupe()` for cheap copies.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FlowVector<T: Clone>(im::Vector<T>);

impl<T: Clone> Default for FlowVector<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone> Dupe for FlowVector<T> {}

impl<T: Clone> FlowVector<T> {
    pub fn new() -> Self {
        FlowVector(im::Vector::new())
    }

    pub fn unit(value: T) -> Self {
        let mut v = im::Vector::new();
        v.push_back(value);
        FlowVector(v)
    }

    pub fn into_inner(self) -> im::Vector<T> {
        self.0
    }

    pub fn as_inner(&self) -> &im::Vector<T> {
        &self.0
    }

    pub fn push(&mut self, value: T) {
        self.0.push_back(value);
    }

    pub fn pop(&mut self) -> Option<T> {
        self.0.pop_back()
    }

    pub fn last_mut(&mut self) -> Option<&mut T> {
        let len = self.0.len();
        if len > 0 {
            self.0.get_mut(len - 1)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.0 = im::Vector::new();
    }
}

impl<T: Clone> Deref for FlowVector<T> {
    type Target = im::Vector<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Clone> DerefMut for FlowVector<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<T: Clone> From<im::Vector<T>> for FlowVector<T> {
    fn from(v: im::Vector<T>) -> Self {
        FlowVector(v)
    }
}

impl<T: Clone> From<FlowVector<T>> for im::Vector<T> {
    fn from(v: FlowVector<T>) -> Self {
        v.0
    }
}

impl<T: Clone> FromIterator<T> for FlowVector<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        FlowVector(im::Vector::from_iter(iter))
    }
}

impl<T: Clone> IntoIterator for FlowVector<T> {
    type Item = T;
    type IntoIter = im::vector::ConsumingIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, T: Clone> IntoIterator for &'a FlowVector<T> {
    type Item = &'a T;
    type IntoIter = im::vector::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
