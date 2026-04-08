/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::semver_version;
use crate::semver_version::Version;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Equal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Comparator {
    pub op: Option<Op>,
    pub version: Version,
}

pub fn string_of_op(op: &Op) -> &'static str {
    match op {
        Op::Greater => ">",
        Op::GreaterOrEqual => ">=",
        Op::Less => "<",
        Op::LessOrEqual => "<=",
        Op::Equal => "=",
    }
}

pub fn to_string(c: &Comparator) -> String {
    let Comparator { op, version } = c;
    let op = match op {
        Some(op) => string_of_op(op),
        None => "",
    };
    format!("{}{}", op, semver_version::to_string(version))
}

pub fn satisfies(version: &Version, c: &Comparator) -> bool {
    let Comparator { op, version: range } = c;
    let result = semver_version::compare_precedence(version, range);
    match op {
        Some(Op::Greater) => result > 0,
        Some(Op::GreaterOrEqual) => result >= 0,
        Some(Op::Less) => result < 0,
        Some(Op::LessOrEqual) => result <= 0,
        Some(Op::Equal) | None => result == 0,
    }
}
