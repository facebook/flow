/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::future::Future;

use flow_parser::file_key::FileKey;

pub type Key = FileKey;
pub type Cache<V> = crate::cache::Cache<Key, V>;

pub fn make<V>(max_size: usize) -> Cache<V> {
    Cache::make(max_size)
}

pub fn clear<V>(cache: &Cache<V>) {
    cache.clear();
}

pub fn remove_entry<V>(key: &FileKey, cache: &Cache<V>) {
    cache.remove_entry(key);
}

pub fn get_from_cache<V: Clone>(key: &FileKey, cache: &Cache<V>) -> Option<V> {
    cache.get_from_cache(key)
}

pub async fn with_cache<F, V>(
    key: FileKey,
    value: impl FnOnce() -> F,
    cache: &Cache<V>,
) -> (V, bool)
where
    F: Future<Output = V>,
    V: Clone,
{
    cache.with_cache(key, value).await
}

pub fn with_cache_sync<V: Clone>(
    cond: impl Fn(&V) -> bool,
    key: FileKey,
    value: impl FnOnce() -> V,
    cache: &Cache<V>,
) -> (V, bool) {
    cache.with_cache_sync(cond, key, value)
}
