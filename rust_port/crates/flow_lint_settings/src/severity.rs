/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Severity {
    Off,
    Warn,
    Err,
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Off => "off",
            Self::Warn => "warn",
            Self::Err => "error",
        }
    }

    pub fn as_output_str(&self) -> &'static str {
        match self {
            Self::Off => "off",
            Self::Warn => "warning",
            Self::Err => "error",
        }
    }

    pub fn severity_of_str(s: &str) -> Option<Self> {
        match s {
            "off" => Some(Self::Off),
            "warn" => Some(Self::Warn),
            "error" => Some(Self::Err),
            _ => None,
        }
    }

    pub fn min_of(a: Self, b: Self) -> Self {
        if a < b { a } else { b }
    }

    pub fn max_of(a: Self, b: Self) -> Self {
        if a > b { a } else { b }
    }
}
