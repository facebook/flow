/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::hash::Hash;

use dupe::Dupe;
use flow_common::reason::VirtualReason;
use flow_common::reason::string_of_desc;
use flow_parser::loc::Loc;
use flow_parser::loc_sig::LocSig;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum WriteLoc<L: Dupe + Clone + Eq + Hash> {
    Write(VirtualReason<L>),
    Uninitialized,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Values<L: Dupe + Clone + Eq + Ord + Hash>(pub BTreeMap<L, Vec<WriteLoc<L>>>);

impl<L: Dupe + Clone + Eq + Ord + Hash> Values<L> {
    pub fn empty() -> Self {
        Values(BTreeMap::new())
    }

    pub fn contains_key(&self, key: &L) -> bool {
        self.0.contains_key(key)
    }

    pub fn get(&self, key: &L) -> Option<&Vec<WriteLoc<L>>> {
        self.0.get(key)
    }

    pub fn insert(&mut self, key: L, value: Vec<WriteLoc<L>>) {
        self.0.insert(key, value);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&L, &Vec<WriteLoc<L>>)> {
        self.0.iter()
    }
}

pub fn write_locs_of_read_loc<L: Dupe + Clone + Eq + Ord + Hash>(
    values: &Values<L>,
    read_loc: &L,
) -> Vec<WriteLoc<L>> {
    values.0.get(read_loc).cloned().unwrap()
}

pub fn print_values(values: &Values<Loc>) -> String {
    fn print_write_loc(write_loc: &WriteLoc<Loc>) -> String {
        match write_loc {
            WriteLoc::Uninitialized => "(uninitialized)".to_string(),
            WriteLoc::Write(reason) => {
                let loc = reason.loc();
                format!(
                    "{}: ({})",
                    loc.debug_to_string(true),
                    string_of_desc(&reason.desc)
                )
            }
        }
    }

    let kvlist: Vec<_> = values.0.iter().collect();
    let strlist: Vec<String> = kvlist
        .iter()
        .map(|(read_loc, write_locs)| {
            let write_strs: Vec<String> = write_locs.iter().map(print_write_loc).collect();
            format!(
                "{} => {{ {} }}",
                read_loc.debug_to_string(true),
                write_strs.join(", ")
            )
        })
        .collect();
    format!("[ {} ]", strlist.join("; "))
}
