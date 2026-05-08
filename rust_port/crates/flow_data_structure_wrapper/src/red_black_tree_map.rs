/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A wrapper around `rpds::RedBlackTreeMap` that implements `Dupe` for cheap cloning.
//!
//! `rpds::RedBlackTreeMap` is an immutable ordered map backed by a persistent
//! red-black tree. This wrapper adds the `Dupe` trait, allowing consistent use
//! of `.dupe()` instead of `.clone()` throughout the Flow codebase.

use std::ops::Deref;
use std::ops::DerefMut;

use dupe::Dupe;

/// A wrapper around `rpds::RedBlackTreeMap` that implements `Dupe` for cheap cloning.
///
/// This type provides the same functionality as `rpds::RedBlackTreeMap` but also
/// implements the `Dupe` trait, making it consistent with other types in the Flow
/// codebase that use `.dupe()` for cheap copies.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FlowRedBlackTreeMap<K: Ord + Clone, V: Clone>(rpds::RedBlackTreeMap<K, V>);

impl<K: Ord + Clone, V: Clone> Default for FlowRedBlackTreeMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Ord + Clone, V: Clone> Dupe for FlowRedBlackTreeMap<K, V> {}

impl<K: Ord + Clone, V: Clone> FlowRedBlackTreeMap<K, V> {
    pub fn new() -> Self {
        FlowRedBlackTreeMap(rpds::RedBlackTreeMap::new())
    }

    pub fn len(&self) -> usize {
        self.0.size()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        let old_value = self.0.get(&key).cloned();
        self.0.insert_mut(key, value);
        old_value
    }

    pub fn remove(&mut self, key: &K) -> Option<V> {
        let old_value = self.0.get(key).cloned();
        self.0.remove_mut(key);
        old_value
    }

    pub fn clear(&mut self) {
        self.0 = rpds::RedBlackTreeMap::new();
    }

    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.0.ptr_eq(&other.0)
    }

    pub fn into_inner(self) -> rpds::RedBlackTreeMap<K, V> {
        self.0
    }

    pub fn as_inner(&self) -> &rpds::RedBlackTreeMap<K, V> {
        &self.0
    }
}

impl<K: Ord + Clone, V: Clone> Deref for FlowRedBlackTreeMap<K, V> {
    type Target = rpds::RedBlackTreeMap<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<K: Ord + Clone, V: Clone> DerefMut for FlowRedBlackTreeMap<K, V> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<K: Ord + Clone, V: Clone> From<rpds::RedBlackTreeMap<K, V>> for FlowRedBlackTreeMap<K, V> {
    fn from(m: rpds::RedBlackTreeMap<K, V>) -> Self {
        FlowRedBlackTreeMap(m)
    }
}

impl<K: Ord + Clone, V: Clone> From<FlowRedBlackTreeMap<K, V>> for rpds::RedBlackTreeMap<K, V> {
    fn from(m: FlowRedBlackTreeMap<K, V>) -> Self {
        m.0
    }
}

impl<K: Ord + Clone, V: Clone> FromIterator<(K, V)> for FlowRedBlackTreeMap<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        FlowRedBlackTreeMap(rpds::RedBlackTreeMap::from_iter(iter))
    }
}

impl<K: Ord + Clone, V: Clone> IntoIterator for FlowRedBlackTreeMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;

    fn into_iter(self) -> Self::IntoIter {
        self.0
            .iter()
            .map(|(key, value)| ((*key).clone(), value.clone()))
            .collect::<Vec<_>>()
            .into_iter()
    }
}

impl<'a, K: Ord + Clone, V: Clone> IntoIterator for &'a FlowRedBlackTreeMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = <&'a rpds::RedBlackTreeMap<K, V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        (&self.0).into_iter()
    }
}

pub type RedBlackTreeMapIter<'a, K, V> =
    <&'a rpds::RedBlackTreeMap<K, V> as IntoIterator>::IntoIter;

pub type RedBlackTreeMapConsumingIter<K, V> = std::vec::IntoIter<(K, V)>;
