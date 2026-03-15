/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;

use dupe::Dupe;
use flow_common::bitset::Bitset;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

#[derive(Debug, Clone, Dupe, PartialEq, Eq, Hash)]
pub struct HasteModuleInfo {
    module_name: FlowSmolStr,
    namespace_bitset: Bitset,
}

impl HasteModuleInfo {
    pub fn mk(module_name: FlowSmolStr, namespace_bitset: Bitset) -> Self {
        Self {
            module_name,
            namespace_bitset,
        }
    }

    pub fn module_name(&self) -> &FlowSmolStr {
        &self.module_name
    }

    pub fn namespace_bitset(&self) -> &Bitset {
        &self.namespace_bitset
    }
}

impl Ord for HasteModuleInfo {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.module_name.cmp(&other.module_name) {
            Ordering::Equal => self.namespace_bitset.cmp(&other.namespace_bitset),
            c => c,
        }
    }
}

impl PartialOrd for HasteModuleInfo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for HasteModuleInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.module_name, self.namespace_bitset)
    }
}
