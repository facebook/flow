/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Concurrent ordered set wrapper.

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering as AtomicOrdering;

use dupe::Dupe;

const SKLIST_NUM_LEVELS: usize = 12;
const NULL: usize = usize::MAX;
const HEAD: usize = usize::MAX - 1;
const RANDOM_INCREMENT: u64 = 0x9E37_79B9_7F4A_7C15;

static NEXT_RANDOM: AtomicU64 = AtomicU64::new(RANDOM_INCREMENT);

pub struct LockedSet<K> {
    entries: parking_lot::RwLock<SkipList<K>>,
}

struct SkipList<K> {
    search_level: u8,
    heads: [usize; SKLIST_NUM_LEVELS],
    nodes: Vec<SkipNode<K>>,
    len: u32,
}

struct SkipNode<K> {
    key: K,
    next0: usize,
    next: Box<[usize]>,
}

impl<K> Debug for LockedSet<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LockedSet").finish_non_exhaustive()
    }
}

impl<K> Default for LockedSet<K> {
    fn default() -> Self {
        Self {
            entries: parking_lot::RwLock::new(SkipList::new()),
        }
    }
}

impl<K> LockedSet<K> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<K: Ord + Send + Dupe + 'static> LockedSet<K> {
    pub fn insert(&self, key: K) {
        self.entries.write().insert(key);
    }

    pub fn remove(&self, key: &K) -> bool {
        self.entries.write().remove(key)
    }

    pub fn contains(&self, key: &K) -> bool {
        self.entries.read().contains(key)
    }

    pub fn is_empty(&self) -> bool {
        self.entries.read().is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = K> + '_ {
        self.entries.read().to_vec().into_iter()
    }
}

impl<K> SkipList<K> {
    fn new() -> Self {
        Self {
            search_level: 0,
            heads: [NULL; SKLIST_NUM_LEVELS],
            nodes: Vec::new(),
            len: 0,
        }
    }

    fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn successor(&self, pred: usize, level: usize) -> usize {
        if pred == HEAD {
            self.heads[level]
        } else if level == 0 {
            self.nodes[pred].next0
        } else {
            self.nodes[pred].next[level - 1]
        }
    }

    fn set_successor(&mut self, pred: usize, level: usize, succ: usize) {
        if pred == HEAD {
            self.heads[level] = succ;
        } else if level == 0 {
            self.nodes[pred].next0 = succ;
        } else {
            self.nodes[pred].next[level - 1] = succ;
        }
    }

    fn random_level(&mut self) -> usize {
        let mut x = NEXT_RANDOM.fetch_add(RANDOM_INCREMENT, AtomicOrdering::Relaxed);
        x ^= x >> 30;
        x = x.wrapping_mul(0xBF58_476D_1CE4_E5B9);
        x ^= x >> 27;
        x = x.wrapping_mul(0x94D0_49BB_1331_11EB);
        x ^= x >> 31;

        let mut level = 0;
        let mut rolls = x;
        while level + 1 < SKLIST_NUM_LEVELS && rolls & 3 == 3 {
            level += 1;
            rolls >>= 2;
        }
        level
    }
}

impl<K: Ord> SkipList<K> {
    fn find(&self, key: &K, preds: &mut [usize; SKLIST_NUM_LEVELS]) -> Option<usize> {
        let mut pred = HEAD;
        for level in (0..=self.search_level as usize).rev() {
            let mut curr = self.successor(pred, level);
            while curr != NULL {
                match self.nodes[curr].key.cmp(key) {
                    Ordering::Less => {
                        pred = curr;
                        curr = self.successor(pred, level);
                    }
                    Ordering::Equal | Ordering::Greater => break,
                }
            }
            preds[level] = pred;
        }
        let curr = self.successor(preds[0], 0);
        (curr != NULL && self.nodes[curr].key.cmp(key) == Ordering::Equal).then_some(curr)
    }

    fn insert(&mut self, key: K) {
        let mut preds = [HEAD; SKLIST_NUM_LEVELS];
        if self.find(&key, &mut preds).is_some() {
            return;
        }

        let level = self.random_level();
        if level > self.search_level as usize {
            for pred in &mut preds[self.search_level as usize + 1..=level] {
                *pred = HEAD;
            }
            self.search_level = level as u8;
        }

        let index = self.nodes.len();
        let next0 = self.successor(preds[0], 0);
        let mut next = Vec::with_capacity(level);
        for (level, pred) in preds.iter().enumerate().take(level + 1).skip(1) {
            next.push(self.successor(*pred, level));
        }
        if self.nodes.len() == self.nodes.capacity() {
            self.nodes.reserve_exact(self.nodes.capacity().max(1));
        }
        self.nodes.push(SkipNode {
            key,
            next0,
            next: next.into_boxed_slice(),
        });
        for (level, pred) in preds.iter().enumerate().take(level + 1) {
            self.set_successor(*pred, level, index);
        }
        self.len += 1;
    }

    fn remove(&mut self, key: &K) -> bool {
        let mut preds = [HEAD; SKLIST_NUM_LEVELS];
        let Some(index) = self.find(key, &mut preds) else {
            return false;
        };

        let node_level = self.nodes[index].next.len();
        for (level, pred) in preds.iter().enumerate().take(node_level + 1) {
            if self.successor(*pred, level) == index {
                let succ = self.successor(index, level);
                self.set_successor(*pred, level, succ);
            }
        }
        self.len -= 1;
        while self.search_level > 0 && self.heads[self.search_level as usize] == NULL {
            self.search_level -= 1;
        }
        true
    }

    fn contains(&self, key: &K) -> bool {
        let mut preds = [HEAD; SKLIST_NUM_LEVELS];
        self.find(key, &mut preds).is_some()
    }
}

impl<K: Dupe> SkipList<K> {
    fn to_vec(&self) -> Vec<K> {
        let mut entries = Vec::with_capacity(self.len as usize);
        let mut curr = self.heads[0];
        while curr != NULL {
            entries.push(self.nodes[curr].key.dupe());
            curr = self.nodes[curr].next0;
        }
        entries
    }
}

impl<K> Default for SkipList<K> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K> Debug for SkipList<K> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SkipList")
            .field("search_level", &self.search_level)
            .field("len", &self.len)
            .finish_non_exhaustive()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert_deduplicates_and_iterates_in_order() {
        let set = LockedSet::new();
        set.insert(3);
        set.insert(1);
        set.insert(2);
        set.insert(2);

        assert_eq!(set.iter().collect::<Vec<_>>(), vec![1, 2, 3]);
    }

    #[test]
    fn test_remove() {
        let set = LockedSet::new();
        set.insert(1);
        set.insert(2);
        set.insert(3);

        assert!(set.remove(&2));
        assert!(!set.remove(&2));
        assert!(!set.contains(&2));
        assert_eq!(set.iter().collect::<Vec<_>>(), vec![1, 3]);
    }

    #[test]
    fn test_empty() {
        let set = LockedSet::new();
        assert!(set.is_empty());
        set.insert(1);
        assert!(!set.is_empty());
        assert!(set.remove(&1));
        assert!(set.is_empty());
    }
}
