/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

#[derive(Debug, Clone, Dupe, serde::Serialize, serde::Deserialize)]
pub struct HasteModuleInfo {
    module_name: FlowSmolStr,
}

impl HasteModuleInfo {
    pub fn mk(module_name: FlowSmolStr) -> Self {
        Self { module_name }
    }

    pub fn module_name(&self) -> &FlowSmolStr {
        &self.module_name
    }
}

impl PartialEq for HasteModuleInfo {
    fn eq(&self, other: &Self) -> bool {
        self.module_name == other.module_name
    }
}

impl Eq for HasteModuleInfo {}

impl Hash for HasteModuleInfo {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.module_name.hash(state);
    }
}

impl Ord for HasteModuleInfo {
    fn cmp(&self, other: &Self) -> Ordering {
        self.module_name.cmp(&other.module_name)
    }
}

impl PartialOrd for HasteModuleInfo {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Display for HasteModuleInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.module_name)
    }
}
