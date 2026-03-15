/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::ast::expression::Expression;

#[derive(Debug, Clone)]
pub enum Selector<L: Dupe, T: Dupe> {
    Elem {
        index: usize,
        has_default: bool,
    },
    Prop {
        prop: FlowSmolStr,
        prop_loc: L,
        has_default: bool,
    },
    Computed {
        expression: Expression<L, T>,
        has_default: bool,
    },
    ObjRest {
        used_props: Vec<FlowSmolStr>,
        after_computed: bool,
    },
    ArrRest(usize),
    Default,
}

impl<L: Dupe, T: Dupe> fmt::Display for Selector<L, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Selector::Elem { index, .. } => write!(f, "[{}]", index),
            Selector::Prop { prop, .. } => write!(f, ".{}", prop),
            Selector::Computed { .. } => write!(f, ".[computed]"),
            Selector::ObjRest { .. } => write!(f, "{{ ... }}"),
            Selector::ArrRest(_) => write!(f, "[...]"),
            Selector::Default => write!(f, "<with default>"),
        }
    }
}
