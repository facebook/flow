/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
pub struct Userland(FlowSmolStr);

impl Userland {
    pub fn from_smol_str(x: FlowSmolStr) -> Self {
        Self(x)
    }

    pub fn map(&self, f: impl FnOnce(&str) -> FlowSmolStr) -> Self {
        Self(f(&self.0))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn into_inner(self) -> FlowSmolStr {
        self.0
    }

    pub fn display(&self) -> &str {
        &self.0
    }
}

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
pub enum FlowImportSpecifier {
    Userland(Userland),
}

impl FlowImportSpecifier {
    pub fn userland(x: FlowSmolStr) -> Self {
        Self::Userland(Userland(x))
    }
}
