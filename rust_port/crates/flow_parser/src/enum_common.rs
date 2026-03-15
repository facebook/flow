/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum ExplicitType {
    Boolean,
    Number,
    String,
    Symbol,
    BigInt,
}

impl ExplicitType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Boolean => "boolean",
            Self::Number => "number",
            Self::String => "string",
            Self::Symbol => "symbol",
            Self::BigInt => "bigint",
        }
    }
}
