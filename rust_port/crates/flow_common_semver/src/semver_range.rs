/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::semver_comparator;
use crate::semver_comparator::Comparator;
use crate::semver_comparator::Op;
use crate::semver_version;
use crate::semver_version::Version;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Part {
    Comparator(Comparator),
    Caret(Version),
}

// TODO: support unions (`||`).
pub type Range = Vec<Part>;

pub fn expand_caret(version: &Version) -> Vec<Comparator> {
    let upper = match version {
        Version {
            major: 0, minor: 0, ..
        } => semver_version::incr_patch(version),
        Version { major: 0, .. } => semver_version::incr_minor(version),
        _ => semver_version::incr_major(version),
    };
    vec![
        Comparator {
            op: Some(Op::GreaterOrEqual),
            version: version.clone(),
        },
        Comparator {
            op: Some(Op::Less),
            version: upper,
        },
    ]
}

pub fn fold_comparators_of_range<A>(f: &impl Fn(A, &Comparator) -> A, acc: A, t: &[Part]) -> A {
    t.iter().fold(acc, |acc, part| match part {
        Part::Comparator(c) => f(acc, c),
        Part::Caret(v) => {
            let expanded = expand_caret(v);
            expanded.iter().fold(acc, f)
        }
    })
}

pub fn comparators_of_range(t: &[Part]) -> Vec<Comparator> {
    fold_comparators_of_range(
        &|mut acc: Vec<Comparator>, comp| {
            acc.push(comp.clone());
            acc
        },
        vec![],
        t,
    )
}

// Determines if the version is matched by the range.
//
// If the range and the version both have a prerelease, then they must be for
// the same version (major, minor, patch). for example, `>1.2.3-alpha` matches
// `1.2.3-beta` and `1.2.4`, but not `1.2.4-alpha`. this is so that opting into
// one prerelease version does not also opt you into all future prereleases.
// this behavior can be overridden with `~include_prereleases:true`.
pub fn satisfies(include_prereleases: bool, range: &[Part], version: &Version) -> bool {
    let satisfied = fold_comparators_of_range(
        &|acc: bool, comp| {
            if !acc {
                acc
            } else {
                semver_comparator::satisfies(version, comp)
            }
        },
        true,
        range,
    );
    if !satisfied {
        false
    } else {
        let Version {
            major,
            minor,
            patch,
            prerelease,
            build: _,
        } = version;
        if prerelease.is_empty() || include_prereleases {
            true
        } else {
            fold_comparators_of_range(
                &|acc: bool, comp| {
                    let Comparator {
                        version: allowed,
                        op: _,
                    } = comp;
                    if acc {
                        acc
                    } else {
                        match allowed {
                            Version {
                                major: major_,
                                minor: minor_,
                                patch: patch_,
                                prerelease,
                                build: _,
                            } if !prerelease.is_empty() => {
                                major == major_ && minor == minor_ && patch == patch_
                            }
                            _ => false,
                        }
                    }
                },
                false,
                range,
            )
        }
    }
}

pub fn string_of_part(part: &Part) -> String {
    match part {
        Part::Comparator(c) => semver_comparator::to_string(c),
        Part::Caret(ver) => format!("^{}", semver_version::to_string(ver)),
    }
}

pub fn to_string(t: &[Part]) -> String {
    let parts: Vec<String> = t.iter().map(string_of_part).collect();
    parts.join(" ")
}
