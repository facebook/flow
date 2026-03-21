/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use dupe::Dupe;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;

#[derive(
    Debug,
    Clone,
    Dupe,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum ExpectedAnnotationSort {
    Identifier,
    ArrayPattern,
    ObjectPattern,
    FunctionReturn,
    Property { name: FlowSmolStr },
    VariableDefinition { name: FlowSmolStr },
}

impl fmt::Display for ExpectedAnnotationSort {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpectedAnnotationSort::Identifier => write!(f, "identifier"),
            ExpectedAnnotationSort::ArrayPattern => write!(f, "array pattern"),
            ExpectedAnnotationSort::ObjectPattern => write!(f, "object pattern"),
            ExpectedAnnotationSort::FunctionReturn => write!(f, "function return"),
            ExpectedAnnotationSort::Property { name } => write!(f, "property `{}`", name),
            ExpectedAnnotationSort::VariableDefinition { name } => {
                write!(f, "declaration of variable `{}`", name)
            }
        }
    }
}
