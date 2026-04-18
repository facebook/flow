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

#[cfg(test)]
mod tests {
    use super::Part;
    use super::comparators_of_range;
    use super::satisfies;
    use super::to_string;
    use crate::semver_comparator;
    use crate::semver_comparator::Comparator;
    use crate::semver_comparator::Op;
    use crate::semver_version;
    use crate::semver_version::Identifier;
    use crate::semver_version::Version;

    fn v0_0_1() -> Version {
        Version {
            major: 0,
            minor: 0,
            patch: 1,
            ..semver_version::zero()
        }
    }

    fn v0_0_2() -> Version {
        Version {
            major: 0,
            minor: 0,
            patch: 2,
            ..semver_version::zero()
        }
    }

    fn v0_1_0() -> Version {
        Version {
            major: 0,
            minor: 1,
            patch: 0,
            ..semver_version::zero()
        }
    }

    fn v0_1_0_alpha_2() -> Version {
        Version {
            prerelease: vec![Identifier::Str("alpha".to_string()), Identifier::Int(2)],
            ..v0_1_0()
        }
    }

    fn v0_1_2() -> Version {
        Version {
            major: 0,
            minor: 1,
            patch: 2,
            ..semver_version::zero()
        }
    }

    fn v0_2_0() -> Version {
        Version {
            major: 0,
            minor: 2,
            patch: 0,
            ..semver_version::zero()
        }
    }

    fn v0_2_0_alpha_2() -> Version {
        Version {
            prerelease: vec![Identifier::Str("alpha".to_string()), Identifier::Int(2)],
            ..v0_2_0()
        }
    }

    fn v1() -> Version {
        Version {
            major: 1,
            ..semver_version::zero()
        }
    }

    fn v1_2_0() -> Version {
        Version {
            major: 1,
            minor: 2,
            patch: 0,
            ..semver_version::zero()
        }
    }

    fn v1_2_3() -> Version {
        Version {
            major: 1,
            minor: 2,
            patch: 3,
            ..semver_version::zero()
        }
    }

    fn v1_2_3_alpha_3() -> Version {
        Version {
            major: 1,
            minor: 2,
            patch: 3,
            prerelease: vec![Identifier::Str("alpha".to_string()), Identifier::Int(3)],
            ..semver_version::zero()
        }
    }

    fn v1_2_3_alpha_7() -> Version {
        Version {
            major: 1,
            minor: 2,
            patch: 3,
            prerelease: vec![Identifier::Str("alpha".to_string()), Identifier::Int(7)],
            ..semver_version::zero()
        }
    }

    fn v1_2_4() -> Version {
        Version {
            major: 1,
            minor: 2,
            patch: 4,
            ..semver_version::zero()
        }
    }

    fn v2() -> Version {
        Version {
            major: 2,
            ..semver_version::zero()
        }
    }

    fn v3_4_5_alpha_9() -> Version {
        Version {
            major: 3,
            minor: 4,
            patch: 5,
            prerelease: vec![Identifier::Str("alpha".to_string()), Identifier::Int(9)],
            ..semver_version::zero()
        }
    }

    fn ge(version: Version) -> Comparator {
        Comparator {
            op: Some(Op::GreaterOrEqual),
            version,
        }
    }

    fn lt(version: Version) -> Comparator {
        Comparator {
            op: Some(Op::Less),
            version,
        }
    }

    fn string_of_comparators(comparators: &[Comparator]) -> String {
        let parts: Vec<String> = comparators
            .iter()
            .map(semver_comparator::to_string)
            .collect();
        parts.join(" ")
    }

    fn assert_satisfies(
        include_prereleases: Option<bool>,
        range: &[Part],
        version: &Version,
        expected: bool,
    ) {
        let msg = format!(
            "Expected {} {}to satisfy {}",
            semver_version::to_string(version),
            if expected { "" } else { "NOT " },
            to_string(range)
        );
        assert_eq!(
            expected,
            satisfies(include_prereleases.unwrap_or(false), range, version),
            "{}",
            msg
        );
    }

    #[test]
    fn comparators_of_range_test() {
        let cases: Vec<(Vec<Part>, Vec<Comparator>)> = vec![
            (vec![Part::Caret(v1())], vec![ge(v1()), lt(v2())]),
            (vec![Part::Caret(v1_2_0())], vec![ge(v1_2_0()), lt(v2())]),
            (vec![Part::Caret(v1_2_3())], vec![ge(v1_2_3()), lt(v2())]),
            (
                vec![Part::Caret(v0_1_0())],
                vec![ge(v0_1_0()), lt(v0_2_0())],
            ),
            (
                vec![Part::Caret(v0_1_2())],
                vec![ge(v0_1_2()), lt(v0_2_0())],
            ),
            (
                vec![Part::Caret(v0_0_1())],
                vec![ge(v0_0_1()), lt(v0_0_2())],
            ),
        ];
        for (input, expected) in &cases {
            let actual = comparators_of_range(input);
            assert_eq!(
                string_of_comparators(expected),
                string_of_comparators(&actual),
                "Expected {}, got {}",
                string_of_comparators(expected),
                string_of_comparators(&actual),
            );
        }
        #[expect(
            clippy::assertions_on_constants,
            reason = "literal port of OCaml's `assert_bool \"done\" true` ounit workaround"
        )]
        {
            assert!(true);
        }
    }

    #[test]
    fn satisfies_test() {
        let cases: Vec<(Vec<Part>, Version, bool)> = vec![
            (vec![Part::Caret(v1())], v1(), true),
            (vec![Part::Caret(v1())], v2(), false),
            (
                vec![Part::Comparator(ge(v1_2_3_alpha_3()))],
                v1_2_3_alpha_7(),
                true,
            ),
            (vec![Part::Comparator(ge(v1_2_3_alpha_3()))], v1_2_4(), true),
            (
                vec![Part::Comparator(ge(v1_2_3_alpha_3()))],
                v3_4_5_alpha_9(),
                false,
            ),
            (vec![Part::Caret(v1_2_3_alpha_7())], v1_2_3_alpha_3(), false),
            (vec![Part::Caret(v1_2_3_alpha_7())], v1_2_3_alpha_7(), true),
            (vec![Part::Caret(v1_2_3_alpha_7())], v1_2_3(), true),
            (vec![Part::Caret(v1_2_3_alpha_7())], v1_2_4(), true),
            (vec![Part::Caret(v0_1_0())], v0_1_0_alpha_2(), false),
            (vec![Part::Caret(v0_1_0())], v0_1_0(), true),
            (vec![Part::Caret(v0_1_0())], v0_1_2(), true),
            (vec![Part::Caret(v0_1_0())], v0_2_0_alpha_2(), false),
            (vec![Part::Caret(v0_1_0())], v0_2_0(), false),
        ];
        for (range, version, expected) in &cases {
            assert_satisfies(None, range, version, *expected);
        }
        #[expect(
            clippy::assertions_on_constants,
            reason = "literal port of OCaml's `assert_bool \"done\" true` ounit workaround"
        )]
        {
            assert!(true);
        }
    }

    #[test]
    fn satisfies_includes_prereleases() {
        let cases: Vec<(Vec<Part>, Version, bool)> = vec![
            (vec![Part::Comparator(ge(v1_2_3()))], v3_4_5_alpha_9(), true),
            (vec![Part::Comparator(ge(v1_2_3_alpha_3()))], v1_2_4(), true),
            (
                vec![Part::Comparator(ge(v1_2_3_alpha_3()))],
                v3_4_5_alpha_9(),
                true,
            ),
        ];
        let include_prereleases = true;
        for (range, version, expected) in &cases {
            assert_satisfies(Some(include_prereleases), range, version, *expected);
        }
        #[expect(
            clippy::assertions_on_constants,
            reason = "literal port of OCaml's `assert_bool \"done\" true` ounit workaround"
        )]
        {
            assert!(true);
        }
    }
}
