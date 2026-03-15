/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! HashMap with careful locking primitives.

use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

use dupe::Dupe;
use lock_free_hashtable::sharded::ShardedLockFreeRawTable;

use crate::with_hash::WithHash;

pub struct LockedMap<K, V> {
    map: ShardedLockFreeRawTable<Box<(WithHash<K>, V)>, 64>,
    phantom: PhantomData<K>,
}

impl<K, V> Debug for LockedMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LockedMap").finish_non_exhaustive()
    }
}

impl<K, V> Default for LockedMap<K, V> {
    fn default() -> Self {
        Self {
            map: ShardedLockFreeRawTable::new(),
            phantom: PhantomData,
        }
    }
}

impl<K, V> LockedMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }
}

impl<K: Eq + Hash + 'static, V: Dupe + 'static> LockedMap<K, V> {
    fn equals(a: &(WithHash<K>, V), b: &(WithHash<K>, V)) -> bool {
        a.0.key() == b.0.key()
    }

    fn hash(a: &(WithHash<K>, V)) -> u64 {
        a.0.hash()
    }

    /// If `key` doesn't exist, insert it and return `None`.
    /// If `key` does exist, do not change the map and return the argument `value`,
    /// which is different to what a standard `HashMap` does.
    pub fn insert(&self, key: K, value: V) -> Option<V> {
        let x = Box::new((WithHash::new(key), value));
        self.map
            .insert(x.0.hash(), x, Self::equals, Self::hash)
            .1
            .map(|x| x.1)
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        self.map
            .lookup(WithHash::new(key).hash(), |x| x.0.key() == key)
            .map(|x| &x.1)
    }

    /// Ensure that the value `K` has an entry in the map, and return a reference to it.
    /// If the value is not present, create it using the provided function.
    /// Note that the provided function may be called even if we don't create a new entry,
    /// if someone else is simultaneously inserting a value for the same key.
    pub fn ensure(&self, key: &K, value: impl FnOnce() -> V) -> &V
    where
        K: Dupe,
    {
        let hash = WithHash::new(key).hash();
        if let Some(v) = self.map.lookup(hash, |x| x.0.key() == key) {
            return &v.1;
        }
        let res = self.map.insert(
            hash,
            Box::new((WithHash::new_unchecked(hash, key.dupe()), value())),
            Self::equals,
            Self::hash,
        );
        &res.0.1
    }

    pub fn iter_unordered(&self) -> impl Iterator<Item = (&K, &V)> {
        self.map.iter().map(|x| (x.0.key(), &x.1))
    }

    pub fn keys(&self) -> impl Iterator<Item = &K> {
        self.map.iter().map(|x| x.0.key())
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.map.iter().map(|x| &x.1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_twice() {
        let mp = LockedMap::new();
        assert_eq!(mp.insert(1, "foo"), None);
        assert_eq!(mp.insert(1, "bar"), Some("bar"));
        assert_eq!(mp.get(&1), Some(&"foo"))
    }
}
