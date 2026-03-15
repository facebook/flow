/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use dupe::Dupe;
use flow_parser::loc::Loc;

#[derive(Debug, Clone, Dupe, PartialEq, Eq)]
struct SpanKey(Loc);

impl PartialOrd for SpanKey {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for SpanKey {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let result = Loc::span_compare(&other.0, &self.0);
        match result {
            r if r < 0 => std::cmp::Ordering::Less,
            r if r > 0 => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SpanMap<V>(BTreeMap<SpanKey, V>);

impl<V> Default for SpanMap<V> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<V> SpanMap<V> {
    pub const fn empty() -> Self {
        Self(BTreeMap::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn add<F>(&mut self, loc: Loc, value: V, combine: Option<F>)
    where
        F: FnOnce(V, V) -> V,
        V: Clone,
    {
        let key = SpanKey(loc);
        if let Some(combine_fn) = combine {
            if let Some(existing) = self.0.remove(&key) {
                self.0.insert(key, combine_fn(existing, value));
            } else {
                self.0.insert(key, value);
            }
        } else {
            self.0.insert(key, value);
        }
    }

    pub fn find_opt(&self, loc: &Loc) -> Option<&V> {
        self.0.get(&SpanKey(loc.dupe()))
    }

    pub fn remove(&mut self, loc: &Loc) {
        self.0.remove(&SpanKey(loc.dupe()));
    }

    pub fn union(&mut self, other: Self)
    where
        V: Clone,
    {
        for (k, v) in other.0 {
            self.0.insert(k, v);
        }
    }

    pub fn fold<A, F>(&self, init: A, mut f: F) -> A
    where
        F: FnMut(A, &Loc, &V) -> A,
    {
        self.0
            .iter()
            .fold(init, |acc, (SpanKey(loc), v)| f(acc, loc, v))
    }

    pub fn keys(&self) -> impl Iterator<Item = &Loc> {
        self.0.keys().map(|SpanKey(loc)| loc)
    }
}
