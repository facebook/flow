/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use flow_common::polarity::Polarity;

#[derive(Debug, Clone, Default)]
pub struct Marked<K: Eq + Ord> {
    map: BTreeMap<K, Polarity>,
}

impl<K: Eq + Ord> Marked<K> {
    pub fn new() -> Self {
        Self {
            map: BTreeMap::new(),
        }
    }

    pub fn add(&mut self, id: K, p: Polarity) -> Option<Polarity>
    where
        K: Clone,
    {
        match self.map.get(&id).copied() {
            None => {
                self.map.insert(id, p);
                Some(p)
            }
            Some(p_) => match (p, p_) {
                (Polarity::Positive, Polarity::Negative)
                | (Polarity::Negative, Polarity::Positive) => {
                    self.map.insert(id, Polarity::Neutral);
                    Some(p)
                }
                (Polarity::Neutral, Polarity::Negative) => {
                    self.map.insert(id, p);
                    Some(Polarity::Positive)
                }
                (Polarity::Neutral, Polarity::Positive) => {
                    self.map.insert(id, p);
                    Some(Polarity::Negative)
                }
                _ => None,
            },
        }
    }

    pub fn get(&self, id: &K) -> Option<Polarity> {
        self.map.get(id).copied()
    }

    pub fn mem(&self, id: &K, p: Polarity) -> bool {
        match self.map.get(id) {
            None => false,
            Some(&p_) => Polarity::compat(p_, p),
        }
    }

    pub fn exclude(&mut self, id: K) {
        self.map.insert(id, Polarity::Neutral);
    }
}

pub type IdMarked = Marked<i32>;
