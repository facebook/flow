/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_typing_context::ContextArena;
use flow_typing_context::ContextStore;
use flow_typing_context::MkBuiltins;

/// The contexts that checking a file creates for its dependencies, and the context the builtins
/// are merged in, kept so that checking the next file can reuse them.
///
/// A check borrows the cache, so the cache can only be flushed between checks. Files which form a
/// dependency cycle share a component context, which the dependency arena keys by the cycle's
/// leader.
pub struct CheckCache {
    builtins_arena: ContextArena,
    builtins: Option<MkBuiltins>,
    dependencies: ContextArena,
    capacity: usize,
}

impl CheckCache {
    /// A cache which is flushed by [CheckCache::trim] once it holds `capacity` dependencies.
    pub fn create(capacity: usize) -> Self {
        CheckCache {
            builtins_arena: ContextArena::new(),
            builtins: None,
            dependencies: ContextArena::new(),
            capacity,
        }
    }

    /// The builtins of the contexts made with this cache, which `mk_builtins` merges into the
    /// builtins arena the first time they are asked for. The cached dependency contexts read
    /// these, so the cache has to be cleared whenever the libraries they come from change.
    pub fn builtins(
        &mut self,
        mk_builtins: impl FnOnce(&mut ContextArena) -> MkBuiltins,
    ) -> MkBuiltins {
        self.builtins
            .get_or_insert_with(|| mk_builtins(&mut self.builtins_arena))
            .dupe()
    }

    /// The arena that owns the dependency contexts.
    pub fn dependencies(&self) -> &ContextArena {
        &self.dependencies
    }

    /// A store of `arena`, the arena of a checked file, with the dependencies and the builtins.
    pub fn store<'a>(&'a mut self, arena: &'a mut ContextArena) -> ContextStore<'a> {
        ContextStore::new(vec![
            arena,
            &mut self.dependencies,
            &mut self.builtins_arena,
        ])
    }

    /// Drops the dependency contexts. The builtins are kept.
    pub fn clear_dependencies(&mut self) {
        self.dependencies.clear();
    }

    /// Drops the dependency contexts once there are `capacity` of them, and tells whether it did.
    pub fn trim(&mut self) -> bool {
        let full = self.dependencies.dependency_count() >= self.capacity;
        if full {
            self.clear_dependencies();
        }
        full
    }

    /// Drops everything, the builtins included.
    pub fn clear(&mut self) {
        self.clear_dependencies();
        self.builtins_arena.clear();
        self.builtins = None;
    }
}
