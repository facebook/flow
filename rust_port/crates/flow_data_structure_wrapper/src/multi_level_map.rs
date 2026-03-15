/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A multi-level map backed by a stack of `BTreeMap` layers.
//!
//! `push_level` pushes a new empty layer. `pop_level` discards it.
//! Lookups search top-down. Inserts go into the top layer.

use std::collections::BTreeMap;

#[derive(Debug)]
pub struct MultiLevelMap<K: Ord, V> {
    /// Stack of map layers. Always has at least one (the base).
    /// levels[0] is the base, levels[last] is the top.
    levels: Vec<BTreeMap<K, V>>,
}

impl<K: Ord, V> Default for MultiLevelMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Ord, V> MultiLevelMap<K, V> {
    pub fn new() -> Self {
        Self {
            levels: vec![BTreeMap::new()],
        }
    }

    pub fn get(&self, key: &K) -> Option<&V> {
        for level in self.levels.iter().rev() {
            if let Some(v) = level.get(key) {
                return Some(v);
            }
        }
        None
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.levels.last_mut().unwrap().insert(key, value);
    }

    /// Push a new empty layer. All subsequent inserts go here.
    pub fn push_level(&mut self) {
        self.levels.push(BTreeMap::new());
    }

    /// Pop the top layer, discarding all inserts since the last `push_level`.
    ///
    /// # Panics
    /// Panics if only the base layer remains.
    pub fn pop_level(&mut self) {
        assert!(
            self.levels.len() > 1,
            "pop_level called without matching push_level"
        );
        self.levels.pop();
    }

    /// Remove all entries across all layers.
    pub fn clear(&mut self) {
        for level in &mut self.levels {
            level.clear();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_insert_get() {
        let mut map = MultiLevelMap::new();
        map.insert(1, "a");
        assert_eq!(map.get(&1), Some(&"a"));
        assert_eq!(map.get(&2), None);
    }

    #[test]
    fn test_push_pop_additions() {
        let mut map = MultiLevelMap::new();
        map.insert(1, "a");
        map.insert(2, "b");

        map.push_level();
        map.insert(3, "c");
        map.insert(4, "d");
        assert_eq!(map.get(&3), Some(&"c"));
        assert_eq!(map.get(&1), Some(&"a"));

        map.pop_level();
        assert_eq!(map.get(&1), Some(&"a"));
        assert_eq!(map.get(&2), Some(&"b"));
        assert_eq!(map.get(&3), None);
        assert_eq!(map.get(&4), None);
    }

    #[test]
    fn test_shadow() {
        let mut map = MultiLevelMap::new();
        map.insert(1, "a");

        map.push_level();
        map.insert(1, "a_new");
        assert_eq!(map.get(&1), Some(&"a_new"));

        map.pop_level();
        assert_eq!(map.get(&1), Some(&"a"));
    }

    #[test]
    fn test_nested_levels() {
        let mut map = MultiLevelMap::new();
        map.insert(1, "a");

        map.push_level();
        map.insert(2, "b");

        map.push_level();
        map.insert(3, "c");
        map.insert(1, "a_overwritten");

        map.pop_level();
        assert_eq!(map.get(&1), Some(&"a"));
        assert_eq!(map.get(&2), Some(&"b"));
        assert_eq!(map.get(&3), None);

        map.pop_level();
        assert_eq!(map.get(&1), Some(&"a"));
        assert_eq!(map.get(&2), None);
    }

    #[test]
    fn test_push_pop_then_continue() {
        let mut map = MultiLevelMap::new();
        map.insert(1, "a");

        map.push_level();
        map.insert(2, "b");
        map.pop_level();

        map.insert(3, "c");
        assert_eq!(map.get(&1), Some(&"a"));
        assert_eq!(map.get(&2), None);
        assert_eq!(map.get(&3), Some(&"c"));
    }

    #[test]
    fn test_shadow_across_three_levels() {
        let mut map = MultiLevelMap::new();
        map.insert(1, "base");

        map.push_level();
        map.insert(1, "level1");

        map.push_level();
        map.insert(1, "level2");
        assert_eq!(map.get(&1), Some(&"level2"));

        map.pop_level();
        assert_eq!(map.get(&1), Some(&"level1"));

        map.pop_level();
        assert_eq!(map.get(&1), Some(&"base"));
    }

    #[test]
    fn test_clear() {
        let mut map = MultiLevelMap::new();
        map.insert(1, "a");
        map.insert(2, "b");

        map.clear();
        assert_eq!(map.get(&1), None);
        assert_eq!(map.get(&2), None);
    }
}
