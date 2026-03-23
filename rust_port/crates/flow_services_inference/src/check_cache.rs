/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::Cell;
use std::collections::HashMap;
use std::rc::Rc;

use dupe::Dupe;
use flow_parser::file_key::FileKey;
use flow_typing_context::ComponentT;
use flow_typing_context::make_ccx;
use flow_typing_utils::type_sig_merge;
use linked_hash_map::LinkedHashMap;

/// Files which form a dependency cycle will share the same component context.
/// We will create a component context on demand for the first file in a given
/// component, then re-use it for any future files in the cycle.
///
/// We keep track of a reference count, so we can forget about a cached
/// component context once every file referencing it has been removed.
struct CachedCcx {
    leader: FileKey,
    ccx: Rc<ComponentT>,
    refcount: Cell<usize>,
}

/// Each cached file holds a reference to its associated cached component
/// context so that we can decrement its reference count.
struct CachedFile {
    file: type_sig_merge::File,
    cached_ccx: Rc<CachedCcx>,
}

pub struct CheckCache {
    // This data structure contains a hash table for O(1) lookup as well
    // as a doubly linked list, which we use to keep track of recency of use. When
    // a file is created or accessed, we move it to the front of the list. When we
    // exceed the capacity of the cache, entries at the back of the list are
    // dropped.
    files: LinkedHashMap<FileKey, CachedFile>,
    ccxs: HashMap<FileKey, Rc<CachedCcx>>,
    size: usize,
    capacity: usize,
}

impl CheckCache {
    pub fn create(capacity: usize) -> Self {
        let files = LinkedHashMap::new();
        let ccxs = HashMap::new();
        CheckCache {
            files,
            ccxs,
            size: 0,
            capacity,
        }
    }

    /// When a file is dropped from the cache, we decrement the refcount on its
    /// cached component context. Once no more files reference a given component
    /// context, we remove it from the cache.
    fn release_ccx(&mut self, cached_ccx: &Rc<CachedCcx>) {
        let refcount = cached_ccx.refcount.get();
        if refcount == 1 {
            self.ccxs.remove(&cached_ccx.leader);
        } else {
            cached_ccx.refcount.set(refcount - 1);
        }
    }

    /// Files are added to the front of the cache and moved to the front when
    /// accessed, so the least recently used file(s) are at the back.
    fn drop_least_recently_used(&mut self) {
        match self.files.pop_front() {
            None => {}
            Some((_, CachedFile { file, cached_ccx })) => {
                if cached_ccx.refcount.get() == 1 {
                    // This is the last file using this component context.
                    // Use full cleanup to also break ForcingState cycles in
                    // the shared graph: ForcingState closure → cx → ComponentT → graph → cycle
                    file.cx().post_inference_cleanup();
                } else {
                    // Other files still share this component context.
                    // Only break per-file Rc cycles; shared component data
                    // is still needed by sibling files in the cache.
                    file.cx().post_inference_cleanup_per_file();
                }
                file.drop_closures();
                self.release_ccx(&cached_ccx);
                self.size -= 1;
            }
        }
    }

    /// Files in a cycle share the same component context, so if we are creating a
    /// file in a cycle with an already cached file, its component context will
    /// also be cached.
    fn find_or_create_ccx(&mut self, leader: FileKey) -> Rc<CachedCcx> {
        match self.ccxs.get(&leader) {
            Some(cached_ccx) => {
                cached_ccx.refcount.set(cached_ccx.refcount.get() + 1);
                cached_ccx.dupe()
            }
            None => {
                let ccx = Rc::new(make_ccx());
                let cached_ccx = Rc::new(CachedCcx {
                    leader: leader.dupe(),
                    ccx,
                    refcount: Cell::new(1),
                });
                self.ccxs.insert(leader, cached_ccx.dupe());
                cached_ccx
            }
        }
    }

    /// If a file for the given file key is already in the cache, we move it to the
    /// front of the queue to indicate that it was recently used. Otherwise, we
    /// add a newly created file at the front of the queue.
    pub fn find_or_create(
        &mut self,
        leader: impl FnOnce() -> FileKey,
        create_file: impl FnOnce(Rc<ComponentT>) -> type_sig_merge::File,
        file_key: FileKey,
    ) -> type_sig_merge::File {
        match self.files.get_refresh(&file_key) {
            Some(CachedFile { file, .. }) => file.dupe(),
            None => {
                let cached_ccx = self.find_or_create_ccx(leader());
                let file = create_file(cached_ccx.ccx.dupe());
                if self.size == self.capacity {
                    self.drop_least_recently_used();
                }
                self.files.insert(
                    file_key,
                    CachedFile {
                        file: file.dupe(),
                        cached_ccx,
                    },
                );
                self.size += 1;
                file
            }
        }
    }

    /// Clearing the cache does not need to worry about the reference counts for
    /// cached component contexts, since all cached files are also cleared.
    pub fn clear(&mut self) {
        self.cleanup_all_files();
        self.files.clear();
        self.ccxs.clear();
        self.size = 0;
    }

    /// Break Rc cycles in all cached files. Call before dropping the cache.
    /// Clears both Context Rc cycles (via post_inference_cleanup) and File
    /// closure fields that capture Arc<SharedMem> and Rc<CheckCache>.
    pub fn cleanup_all_files(&mut self) {
        for (_, cached_file) in self.files.iter() {
            cached_file.file.cx().post_inference_cleanup();
            cached_file.file.drop_closures();
        }
    }
}

impl Drop for CheckCache {
    fn drop(&mut self) {
        self.cleanup_all_files();
    }
}
