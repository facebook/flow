/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashSet;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

pub struct GenId {
    next: usize,
    prefix: FlowSmolStr,
    used: HashSet<FlowSmolStr>,
}

impl GenId {
    pub fn new(transform_prefix: &str) -> Self {
        Self {
            next: 0,
            prefix: FlowSmolStr::from(format!("$$gen${transform_prefix}")),
            used: HashSet::new(),
        }
    }

    pub fn id(&mut self) -> FlowSmolStr {
        loop {
            let name = FlowSmolStr::from(format!("{}{}", self.prefix, self.next));
            self.next += 1;
            if self.used.insert(name.dupe()) {
                return name;
            }
        }
    }

    pub fn add_usage(&mut self, name: &FlowSmolStr) {
        if name.starts_with(self.prefix.as_str()) {
            self.used.insert(name.dupe());
        }
    }
}
