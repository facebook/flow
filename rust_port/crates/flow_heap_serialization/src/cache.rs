/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::collections::VecDeque;
use std::sync::Arc;

use dupe::Dupe;
use flow_aloc::PackedALocTable;
use flow_parser::ast::Program;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use parking_lot::Mutex;

const CACHE_CAPACITY: usize = 1000;

struct LruCache<V> {
    map: HashMap<FileKey, V>,
    order: VecDeque<FileKey>,
    capacity: usize,
}

impl<V> LruCache<V> {
    fn new(capacity: usize) -> Self {
        Self {
            map: HashMap::with_capacity(capacity),
            order: VecDeque::with_capacity(capacity),
            capacity,
        }
    }

    fn get(&mut self, key: &FileKey) -> Option<&V> {
        if self.map.contains_key(key) {
            // Move to front (most recently used)
            self.order.retain(|k| k != key);
            self.order.push_front(key.dupe());
            self.map.get(key)
        } else {
            None
        }
    }

    fn insert(&mut self, key: FileKey, value: V) {
        if self.map.contains_key(&key) {
            self.order.retain(|k| k != &key);
        } else if self.map.len() >= self.capacity {
            // Evict least recently used
            if let Some(evicted) = self.order.pop_back() {
                self.map.remove(&evicted);
            }
        }
        self.order.push_front(key.dupe());
        self.map.insert(key, value);
    }

    fn remove(&mut self, key: &FileKey) {
        self.map.remove(key);
        self.order.retain(|k| k != key);
    }

    fn clear(&mut self) {
        self.map.clear();
        self.order.clear();
    }
}

/// Deserialization cache mirroring OCaml's Reader_cache.
/// Capacity: 1000 entries per cache type.
pub struct ReaderCache {
    ast_cache: Mutex<LruCache<Arc<Program<Loc, Loc>>>>,
    aloc_table_cache: Mutex<LruCache<Arc<PackedALocTable>>>,
}

impl ReaderCache {
    pub fn new() -> Self {
        Self {
            ast_cache: Mutex::new(LruCache::new(CACHE_CAPACITY)),
            aloc_table_cache: Mutex::new(LruCache::new(CACHE_CAPACITY)),
        }
    }

    pub fn get_ast(&self, key: &FileKey) -> Option<Arc<Program<Loc, Loc>>> {
        self.ast_cache.lock().get(key).map(|v| v.dupe())
    }

    pub fn add_ast(&self, key: FileKey, ast: Arc<Program<Loc, Loc>>) {
        self.ast_cache.lock().insert(key, ast);
    }

    pub fn get_aloc_table(&self, key: &FileKey) -> Option<Arc<PackedALocTable>> {
        self.aloc_table_cache.lock().get(key).map(|v| v.dupe())
    }

    pub fn add_aloc_table(&self, key: FileKey, table: Arc<PackedALocTable>) {
        self.aloc_table_cache.lock().insert(key, table);
    }

    pub fn remove_batch(&self, keys: &[FileKey]) {
        let mut ast_cache = self.ast_cache.lock();
        let mut aloc_cache = self.aloc_table_cache.lock();
        for key in keys {
            ast_cache.remove(key);
            aloc_cache.remove(key);
        }
    }

    pub fn clear(&self) {
        self.ast_cache.lock().clear();
        self.aloc_table_cache.lock().clear();
    }
}

impl Default for ReaderCache {
    fn default() -> Self {
        Self::new()
    }
}
