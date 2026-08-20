/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;
use std::rc::Rc;

use dupe::Dupe;

use crate::check_cache::CheckCache;

/// The cache behind `Merge_service.check_contents_context`: merged dependency contexts, reused
/// across requests for as long as the server runs.
///
/// It is `Rc`-based, so this value and every borrow of it are pinned to the thread that built it.
/// The command executor owns one and lends `&CheckContentsCache` to each command. Cached contexts may
/// retain the returned `Rc` between commands, but it never leaves that thread. The command executor
/// clears it before a recheck commit or heap compaction mutates Base.
pub struct CheckContentsCache {
    cache: Rc<RefCell<CheckCache<'static>>>,
}

impl CheckContentsCache {
    pub fn new() -> Self {
        Self {
            cache: Rc::new(RefCell::new(CheckCache::create(10_000))),
        }
    }

    pub fn clear(&self) {
        self.cache.borrow_mut().clear();
    }

    /// `check_service::mk_check_file` keeps this alive inside the contexts it caches, which is
    /// what makes a cached dependency reusable by the next request. Those are the `Rc` cycles
    /// `CheckCache::cleanup_all_files` breaks.
    pub fn handle(&self) -> Rc<RefCell<CheckCache<'static>>> {
        self.cache.dupe()
    }
}

impl Default for CheckContentsCache {
    fn default() -> Self {
        Self::new()
    }
}
