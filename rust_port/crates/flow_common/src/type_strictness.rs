/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    Default,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    serde::Serialize,
    serde::Deserialize
)]
pub enum TypeStrictnessKind {
    /// The original Flow semantics.
    /// Stricter interpreration of types (e.g. actual variance checks).
    #[default]
    Flow,
    /// Interpreting types under looser TS semantics (e.g. loose exactness, loose variance, etc),
    /// to make work with existing TS code more practical.
    TypeScriptLoose,
}

impl TypeStrictnessKind {
    pub fn from_is_typescript(is_typescript: bool) -> Self {
        if is_typescript {
            Self::TypeScriptLoose
        } else {
            Self::Flow
        }
    }

    pub fn join(self, other: Self) -> Self {
        match (self, other) {
            (Self::TypeScriptLoose, _) | (_, Self::TypeScriptLoose) => Self::TypeScriptLoose,
            (Self::Flow, Self::Flow) => Self::Flow,
        }
    }

    pub fn is_typescript_loose(self) -> bool {
        self == Self::TypeScriptLoose
    }
}
