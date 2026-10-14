/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::check_cache::CheckCache;

/// The cache behind `Merge_service.check_contents_context`: merged dependency contexts, reused
/// across requests until the next recheck commit or heap compaction.
///
/// The command executor owns one and lends `&mut CheckContentsCache` to each command. Between
/// commands, the executor trims it, and clears it before a recheck commit or heap compaction
/// mutates Base. Every artifact holding a context made with the cache has to be dropped along with
/// what the cache drops.
pub struct CheckContentsCache {
    cache: CheckCache,
}

impl CheckContentsCache {
    pub fn new() -> Self {
        Self {
            cache: CheckCache::create(10_000),
        }
    }

    pub fn clear(&mut self) {
        self.cache.clear();
    }

    /// Drops the dependency contexts once the cache is full, and tells whether it did.
    pub fn trim(&mut self) -> bool {
        self.cache.trim()
    }

    pub fn check_cache(&mut self) -> &mut CheckCache {
        &mut self.cache
    }
}

impl Default for CheckContentsCache {
    fn default() -> Self {
        Self::new()
    }
}
