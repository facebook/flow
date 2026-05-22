/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::btree_map::Entry;

use crate::reason::Name;

pub mod map {
    use super::*;

    pub type T<V> = BTreeMap<Name, V>;

    pub fn update<V, F>(key: Name, f: F, map: &mut T<V>)
    where
        F: FnOnce(Option<&V>) -> Option<V>,
    {
        match map.entry(key) {
            Entry::Occupied(mut entry) => match f(Some(entry.get())) {
                Some(value) => {
                    entry.insert(value);
                }
                None => {
                    entry.remove();
                }
            },
            Entry::Vacant(entry) => {
                if let Some(value) = f(None) {
                    entry.insert(value);
                }
            }
        }
    }
}
