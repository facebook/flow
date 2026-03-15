/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::ops::DerefMut;

use dupe::Dupe;
use flow_aloc::ALoc;
use flow_data_structure_wrapper::ord_map::FlowOrdMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Reason {
    FunctionCall,
    ConstructorCall,
    PropertyAssignment,
    Await,
    Yield,
}

#[derive(Debug, Clone, Dupe, PartialEq, Eq)]
pub struct RefinementInvalidation(FlowOrdMap<ALoc, Reason>);

impl RefinementInvalidation {
    pub fn new() -> Self {
        RefinementInvalidation(FlowOrdMap::new())
    }

    pub fn singleton(loc: ALoc, reason: Reason) -> Self {
        let mut map = FlowOrdMap::new();
        map.insert(loc, reason);
        RefinementInvalidation(map)
    }
}

impl Deref for RefinementInvalidation {
    type Target = FlowOrdMap<ALoc, Reason>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for RefinementInvalidation {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for RefinementInvalidation {
    fn default() -> Self {
        Self::new()
    }
}

pub fn singleton(loc: ALoc, reason: Reason) -> RefinementInvalidation {
    RefinementInvalidation::singleton(loc, reason)
}

pub fn string_of_reason(reason: Reason) -> &'static str {
    match reason {
        Reason::FunctionCall => "function call",
        Reason::ConstructorCall => "constructor call",
        Reason::PropertyAssignment => "property assignment",
        Reason::Await => "await expression",
        Reason::Yield => "yield expression",
    }
}

pub fn union(mut m1: RefinementInvalidation, m2: RefinementInvalidation) -> RefinementInvalidation {
    for (k, v) in m2.iter() {
        m1.insert(k.dupe(), *v);
    }
    m1
}

pub fn merge(
    opt1: Option<RefinementInvalidation>,
    opt2: Option<RefinementInvalidation>,
) -> Option<RefinementInvalidation> {
    match (opt1, opt2) {
        (None, None) => None,
        (Some(m), None) | (None, Some(m)) => Some(m),
        (Some(mut m1), Some(m2)) => {
            for (k, v) in m2.iter() {
                m1.insert(k.dupe(), *v);
            }
            Some(m1)
        }
    }
}
