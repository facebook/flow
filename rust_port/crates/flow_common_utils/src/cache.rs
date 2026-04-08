/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cell::Cell;
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::future::Future;
use std::rc::Rc;
use std::time::SystemTime;

struct Entry<V> {
    cached_value: V,
    last_hit: f64,
}

#[derive(Clone)]
pub struct Cache<K, V> {
    entries: Rc<RefCell<BTreeMap<K, Entry<V>>>>,
    size: Rc<Cell<usize>>,
    max_size: usize,
}

impl<K: Ord + Clone, V> Cache<K, V> {
    pub fn make(max_size: usize) -> Cache<K, V> {
        Cache {
            entries: Rc::new(RefCell::new(BTreeMap::new())),
            size: Rc::new(Cell::new(0)),
            max_size,
        }
    }

    pub fn clear(&self) {
        *self.entries.borrow_mut() = BTreeMap::new();
        self.size.set(0);
    }

    pub fn remove_entry(&self, key: &K) {
        if self.entries.borrow_mut().remove(key).is_some() {
            self.size.set(self.size.get() - 1);
        }
    }

    fn remove_oldest(&self) {
        let oldest = self
            .entries
            .borrow()
            .iter()
            .fold(None, |acc, (key, entry)| {
                let last_hit = entry.last_hit;
                match acc {
                    Some((_, oldest_hit)) if oldest_hit <= last_hit => acc,
                    _ => Some((key.clone(), last_hit)),
                }
            });
        match oldest {
            Some((oldest_key, _)) => self.remove_entry(&oldest_key),
            None => {}
        }
    }

    fn add_after_miss(&self, key: K, value: V) {
        let entry = Entry {
            cached_value: value,
            last_hit: SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap()
                .as_secs_f64(),
        };
        self.entries.borrow_mut().insert(key, entry);
        self.size.set(self.size.get() + 1);
        if self.size.get() > self.max_size {
            self.remove_oldest()
        }
    }
}

impl<K: Ord + Clone, V: Clone> Cache<K, V> {
    pub fn get_from_cache(&self, key: &K) -> Option<V> {
        match self.entries.borrow_mut().get_mut(key) {
            None => None,
            Some(entry) => {
                entry.last_hit = SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64();
                Some(entry.cached_value.clone())
            }
        }
    }

    pub async fn with_cache<F>(&self, key: K, value: impl FnOnce() -> F) -> (V, bool)
    where
        F: Future<Output = V>,
    {
        let cached_result = self.get_from_cache(&key);
        match cached_result {
            None => {
                let value = value().await;
                self.add_after_miss(key, value.clone());
                (value, false)
            }
            Some(result) => (result, true),
        }
    }

    pub fn with_cache_sync(
        &self,
        cond: impl Fn(&V) -> bool,
        key: K,
        value: impl FnOnce() -> V,
    ) -> (V, bool) {
        let cached_result = self.get_from_cache(&key);
        match cached_result {
            Some(result) if cond(&result) => (result, true),
            _ => {
                let value = value();
                self.add_after_miss(key, value.clone());
                (value, false)
            }
        }
    }
}
