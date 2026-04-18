/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Basic semantic version parser, as defined by http://semver.org/
//!
//! So far, this implementation only supports individual versions; intersection
//! ranges (e.g. ">=0.13.0 <0.14.0", which are ANDed together); and caret ranges,
//! which allow changes that do not modify the left-most non-zero digit (e.g.
//! "^0.13" expands into ">=0.13.0 <0.14.0", and "^0.13.1" expands into
//! ">=0.13.1 <0.14.0", whereas "^1.2.3" expands into ">=1.2.3 <2.0.0").
//!
//! Further support for features like "||" ("1.2.3 || 1.2.5"), hyphen ranges
//! ("1.2 - 1.3"), X-ranges ("1.2.x" or "1.2.*"), tilde ranges ("~1.2"), and
//! pre-release/build identifiers ("1.2.3-beta.1"), will be added as necessary.

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

#[cfg(test)]
mod tests {
    use super::satisfies;

    #[test]
    fn greater_than_major() {
        let cases: Vec<(&str, &str, bool)> = vec![
            (">2", "2", false),
            (">2", "2.0", false),
            (">2", "2.1", true),
            (">2", "3", true),
            (">2", "3.0", true),
            (">2", "11.0", true),
        ];
        for (range, version, expected) in &cases {
            assert_eq!(
                *expected,
                satisfies(None, range, version).expect("parse should succeed"),
                "satisfies({}, {}) should be {}",
                range,
                version,
                expected
            );
        }
    }
}
