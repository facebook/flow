/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Concurrent ordered set wrapper.

use std::fmt;
use std::fmt::Debug;

use crossbeam_skiplist::SkipSet;
use dupe::Dupe;

pub struct LockedSet<K> {
    entries: SkipSet<K>,
}

impl<K> Debug for LockedSet<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LockedSet").finish_non_exhaustive()
    }
}

impl<K> Default for LockedSet<K> {
    fn default() -> Self {
        Self {
            entries: SkipSet::new(),
        }
    }
}

impl<K> LockedSet<K> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<K: Ord + Send + Dupe + 'static> LockedSet<K> {
    pub fn insert(&self, key: K) {
        self.entries.insert(key);
    }

    pub fn remove(&self, key: &K) -> bool {
        self.entries.remove(key).is_some()
    }

    pub fn contains(&self, key: &K) -> bool {
        self.entries.contains(key)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = K> + '_ {
        self.entries.iter().map(|entry| entry.value().dupe())
    }
}
