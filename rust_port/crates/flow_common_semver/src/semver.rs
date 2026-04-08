/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::semver_parser::Parser;
use crate::semver_range;
use crate::semver_range::Part;
use crate::semver_version;
use crate::semver_version::Version;

#[derive(Debug, Clone)]
pub struct ParseError(pub String);

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for ParseError {}

pub fn version_of_string(s: &str) -> Result<Version, ParseError> {
    let mut parser = Parser::new(s);
    parser
        .version_top()
        .map_err(|_| ParseError(format!("Invalid version number: {}", s)))
}

pub fn range_of_string(s: &str) -> Result<Vec<Part>, ParseError> {
    let mut parser = Parser::new(s);
    parser
        .range_top()
        .map_err(|_| ParseError(format!("Invalid range: {}", s)))
}

pub fn is_valid_range(range: &str) -> bool {
    range_of_string(range).is_ok()
}

pub fn satisfies(
    include_prereleases: Option<bool>,
    range: &str,
    version: &str,
) -> Result<bool, ParseError> {
    let range = range_of_string(range)?;
    let version = version_of_string(version)?;
    let include_prereleases = include_prereleases.unwrap_or(false);
    Ok(semver_range::satisfies(
        include_prereleases,
        &range,
        &version,
    ))
}

pub fn compare(a: &str, b: &str) -> Result<i64, ParseError> {
    let a = version_of_string(a)?;
    let b = version_of_string(b)?;
    Ok(semver_version::compare_precedence(&a, &b))
}
