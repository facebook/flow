/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::RefCell;

use flow_typing_context::ArenaId;
use flow_typing_context::Context;
use flow_typing_context::ContextArena;
use flow_typing_context::ContextId;

use crate::check_cache::CheckCache;
use crate::check_contents_cache::CheckContentsCache;

/// The context of a file checked with the cache of a [CheckContentsCache], with the arena that
/// owns it and every other context the check made, but for those of the file's dependencies,
/// which the cache owns.
///
/// The artifacts holding a checked context are shared by the requests about the file, and each
/// request works on the context exclusively, so the arena is borrowed for as long as one does.
/// [CheckedContext::with_cx] holds the cache exclusively too, so no two of these borrows overlap.
pub struct CheckedContext {
    arena: RefCell<ContextArena>,
    cx: ContextId,
    dependencies: ArenaId,
}

impl CheckedContext {
    /// The context `cx` of `arena`, checked with `cache`.
    pub fn new(arena: ContextArena, cx: ContextId, cache: &CheckCache) -> Self {
        CheckedContext {
            arena: RefCell::new(arena),
            cx,
            dependencies: cache.dependencies().id(),
        }
    }

    /// Runs `f` with the context, which finds its dependencies in `cache`.
    pub fn with_cx<R>(
        &self,
        cache: &mut CheckContentsCache,
        f: impl FnOnce(&mut Context<'_>) -> R,
    ) -> R {
        let cache = cache.check_cache();
        assert_eq!(
            cache.dependencies().id(),
            self.dependencies,
            "a checked context is dropped whenever its cache drops the dependencies it was checked with"
        );
        let mut arena = self.arena.borrow_mut();
        let mut cx = Context::new(cache.store(&mut arena), self.cx);
        f(&mut cx)
    }
}
