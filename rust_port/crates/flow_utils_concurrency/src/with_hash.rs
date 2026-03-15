/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Represent a value that has already been hashed.
//! Originally from the `starlark_map` crate, but modified to work with u64 hash.

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;

use dupe::Dupe;
use equivalent::Equivalent;
use starlark_map::StarlarkHasher;

/// A key and its hash.
#[derive(PartialEq, Eq, Debug, Clone, Copy, Dupe)]
pub struct WithHash<K> {
    hash: u64,
    key: K,
}

impl<K: Default + Hash> Default for WithHash<K> {
    fn default() -> Self {
        Self::new(K::default())
    }
}

// We deliberately know that this is a hash and value, so our Eq/Hash are fine
#[allow(clippy::derived_hash_with_manual_eq)]
impl<K> Hash for WithHash<K> {
    fn hash<S: Hasher>(&self, state: &mut S) {
        // Only hash the hash, not the key.
        self.hash.hash(state)
    }
}

impl<K> Deref for WithHash<K> {
    type Target = K;

    fn deref(&self) -> &Self::Target {
        &self.key
    }
}

impl<K: Display> Display for WithHash<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.key.fmt(f)
    }
}

impl<K: PartialOrd> PartialOrd for WithHash<K> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.key.partial_cmp(&other.key)
    }
}

impl<K: Ord> Ord for WithHash<K> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.key.cmp(&other.key)
    }
}

impl<K> WithHash<K> {
    /// Create a new [`WithHash`] value using the [`Hash`] of the key.
    pub fn new(key: K) -> Self
    where
        K: Hash,
    {
        // We use the Starlark hasher because it's faster than the default
        let mut hasher = StarlarkHasher::default();
        key.hash(&mut hasher);
        WithHash::new_unchecked(hasher.finish(), key)
    }

    /// Directly create a new [`WithHash`] using a given hash value.
    /// If the hash does not correspond to the key, its will cause issues.
    pub fn new_unchecked(hash: u64, key: K) -> Self {
        WithHash { hash, key }
    }

    /// Get the underlying key.
    pub fn key(&self) -> &K {
        &self.key
    }

    /// Get the underlying key, as mutable.
    pub fn key_mut(&mut self) -> &mut K {
        &mut self.key
    }

    /// Get the underlying key taking ownership.
    pub fn into_key(self) -> K {
        self.key
    }

    /// Get the underlying hash.
    pub fn hash(&self) -> u64 {
        self.hash
    }

    /// Convert `WithHash<K>` to `WithHash<&K>`.
    pub fn as_ref(&self) -> WithHash<&K> {
        WithHash::new_unchecked(self.hash, &self.key)
    }
}

impl<'a, K> WithHash<&'a K> {
    /// Make `WithHash<K>` from `WithHash<&K>`.
    pub fn copied(self) -> WithHash<K>
    where
        K: Copy,
    {
        WithHash::new_unchecked(self.hash, *self.key)
    }

    /// Make `WithHash<K>` from `WithHash<&K>`, where `K` is `Clone`.
    pub fn cloned(self) -> WithHash<K>
    where
        K: Clone,
    {
        WithHash::new_unchecked(self.hash, self.key.clone())
    }
}

impl<'a, K: ?Sized> WithHash<&'a K> {
    /// Make `WithHash<K>` from `WithHash<&K>`, where `T` is the owned form of `K`.
    pub fn owned<T>(self) -> WithHash<T>
    where
        K: Equivalent<T>,
        K: ToOwned<Owned = T>,
    {
        WithHash::new_unchecked(self.hash, self.key.to_owned())
    }
}
