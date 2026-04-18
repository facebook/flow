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

    // Eviction is O(n) -- see comment in cache_sig.ml
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

#[cfg(test)]
mod tests {
    use std::thread;
    use std::time::Duration;

    use super::*;

    type StringCache<V> = Cache<String, V>;

    mod lazy_evaluation_tracker {
        use std::cell::Cell;
        use std::rc::Rc;

        pub struct T<A: Clone> {
            value: A,
            pub was_evaluated: Rc<Cell<bool>>,
        }

        pub fn make<A: Clone>(value: A) -> T<A> {
            T {
                was_evaluated: Rc::new(Cell::new(false)),
                value,
            }
        }

        pub fn get<A: Clone + 'static>(t: &T<A>) -> impl FnOnce() -> std::future::Ready<A> {
            let was_evaluated = t.was_evaluated.clone();
            let value = t.value.clone();
            move || {
                was_evaluated.set(true);
                std::future::ready(value)
            }
        }

        pub fn was_evaluated<A: Clone>(t: &T<A>) -> bool {
            t.was_evaluated.get()
        }
    }

    fn make_cache() -> StringCache<i32> {
        StringCache::<i32>::make(3)
    }

    #[tokio::test]
    async fn basic_miss() {
        let cache = make_cache();
        let eval_tracker = lazy_evaluation_tracker::make(42);
        let (result, did_hit) = cache
            .with_cache(
                "foo".to_string(),
                lazy_evaluation_tracker::get(&eval_tracker),
            )
            .await;
        assert_eq!(42, result);
        assert!(lazy_evaluation_tracker::was_evaluated(&eval_tracker));
        assert!(!did_hit);
    }

    #[tokio::test]
    async fn basic_hit() {
        let cache = make_cache();
        let eval_tracker = lazy_evaluation_tracker::make(42);
        let (first_result, first_did_hit) = cache
            .with_cache("foo".to_string(), || std::future::ready(42))
            .await;
        let (second_result, second_did_hit) = cache
            .with_cache(
                "foo".to_string(),
                lazy_evaluation_tracker::get(&eval_tracker),
            )
            .await;
        assert_eq!(42, first_result);
        assert_eq!(42, second_result);
        assert!(!lazy_evaluation_tracker::was_evaluated(&eval_tracker));
        assert!(!first_did_hit);
        assert!(second_did_hit);
    }

    #[tokio::test]
    async fn eviction() {
        let cache = make_cache();
        cache
            .with_cache("foo".to_string(), || std::future::ready(1))
            .await;
        // Without the sleeps I (nmote) observed a spurious failure of this test, since
        // eviction is based on last time of access. A few 1 ms sleeps should be more than
        // enough time to avoid spurious failures, but not enough to noticeably affect the
        // runtime of the tests.
        thread::sleep(Duration::from_millis(1));
        cache
            .with_cache("bar".to_string(), || std::future::ready(2))
            .await;
        thread::sleep(Duration::from_millis(1));
        cache
            .with_cache("baz".to_string(), || std::future::ready(3))
            .await;
        thread::sleep(Duration::from_millis(1));

        // This will evict something
        cache
            .with_cache("qux".to_string(), || std::future::ready(4))
            .await;

        // "foo" should have been evicted
        let eval_tracker = lazy_evaluation_tracker::make(1);
        let (result, did_hit) = cache
            .with_cache(
                "foo".to_string(),
                lazy_evaluation_tracker::get(&eval_tracker),
            )
            .await;
        assert_eq!(1, result);
        assert!(lazy_evaluation_tracker::was_evaluated(&eval_tracker));
        assert!(!did_hit);
    }

    #[tokio::test]
    async fn eviction_last_access() {
        let cache = make_cache();
        cache
            .with_cache("foo".to_string(), || std::future::ready(1))
            .await;
        thread::sleep(Duration::from_millis(1));
        cache
            .with_cache("bar".to_string(), || std::future::ready(2))
            .await;
        thread::sleep(Duration::from_millis(1));
        cache
            .with_cache("baz".to_string(), || std::future::ready(3))
            .await;
        thread::sleep(Duration::from_millis(1));

        // This accesses "foo" which updates the access time
        cache
            .with_cache("foo".to_string(), || std::future::ready(1))
            .await;

        // This will evict something
        cache
            .with_cache("qux".to_string(), || std::future::ready(4))
            .await;

        // "bar" should have been evicted
        let eval_tracker = lazy_evaluation_tracker::make(2);
        let (result, did_hit) = cache
            .with_cache(
                "bar".to_string(),
                lazy_evaluation_tracker::get(&eval_tracker),
            )
            .await;
        assert_eq!(2, result);
        assert!(lazy_evaluation_tracker::was_evaluated(&eval_tracker));
        assert!(!did_hit);
    }

    #[tokio::test]
    async fn clear() {
        let cache = make_cache();
        let eval_tracker = lazy_evaluation_tracker::make(42);
        let (first_result, first_did_hit) = cache
            .with_cache("foo".to_string(), || std::future::ready(42))
            .await;
        cache.clear();
        let (second_result, second_did_hit) = cache
            .with_cache(
                "foo".to_string(),
                lazy_evaluation_tracker::get(&eval_tracker),
            )
            .await;
        assert_eq!(42, first_result);
        assert_eq!(42, second_result);
        assert!(lazy_evaluation_tracker::was_evaluated(&eval_tracker));
        assert!(!first_did_hit);
        assert!(!second_did_hit);
    }

    #[tokio::test]
    async fn remove_entry() {
        let cache = make_cache();
        cache
            .with_cache("foo".to_string(), || std::future::ready(42))
            .await;
        cache
            .with_cache("bar".to_string(), || std::future::ready(43))
            .await;
        cache.remove_entry(&"foo".to_string());

        let foo_eval_tracker = lazy_evaluation_tracker::make(42);
        let bar_eval_tracker = lazy_evaluation_tracker::make(43);
        let (foo_result, foo_did_hit) = cache
            .with_cache(
                "foo".to_string(),
                lazy_evaluation_tracker::get(&foo_eval_tracker),
            )
            .await;
        let (bar_result, bar_did_hit) = cache
            .with_cache(
                "bar".to_string(),
                lazy_evaluation_tracker::get(&bar_eval_tracker),
            )
            .await;
        assert_eq!(42, foo_result);
        assert_eq!(43, bar_result);
        assert!(lazy_evaluation_tracker::was_evaluated(&foo_eval_tracker));
        assert!(!lazy_evaluation_tracker::was_evaluated(&bar_eval_tracker));
        assert!(!foo_did_hit);
        assert!(bar_did_hit);
    }
}
