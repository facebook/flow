/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Identifier {
    Str(String),
    Int(i64),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Version {
    pub major: i64,
    pub minor: i64,
    pub patch: i64,
    pub prerelease: Vec<Identifier>,
    pub build: Vec<Identifier>,
}

pub fn zero() -> Version {
    Version {
        major: 0,
        minor: 0,
        patch: 0,
        prerelease: vec![],
        build: vec![],
    }
}

pub fn compare_identifiers(a: &Identifier, b: &Identifier) -> i64 {
    match (a, b) {
        (Identifier::Int(_), Identifier::Str(_)) => -1,
        (Identifier::Str(_), Identifier::Int(_)) => 1,
        (Identifier::Int(a), Identifier::Int(b)) => a - b,
        (Identifier::Str(a), Identifier::Str(b)) => a.cmp(b) as i64,
    }
}

pub fn compare_identifier_lists(a: &[Identifier], b: &[Identifier]) -> i64 {
    fn compare_sets(a: &[Identifier], b: &[Identifier]) -> i64 {
        match (a, b) {
            ([], []) => 0,
            // A more specific prerelease is greater than one with fewer parts.
            ([_, ..], []) => 1,
            ([], [_, ..]) => -1,
            ([a_hd, a_tl @ ..], [b_hd, b_tl @ ..]) => {
                let k = compare_identifiers(a_hd, b_hd);
                if k != 0 { k } else { compare_sets(a_tl, b_tl) }
            }
        }
    }
    match (a, b) {
        ([], []) => 0,
        // Being a prerelease is less than not being a prerelease.
        ([_, ..], []) => -1,
        ([], [_, ..]) => 1,
        (_, _) => compare_sets(a, b),
    }
}

// Compares the precedence of two versions
//
// NOTE: build identifiers are NOT included in precedence! this is the
// difference vs `compare`
//
// From the spec:
//   Precedence refers to how versions are compared to each other when ordered.
//   Precedence MUST be calculated by separating the version into major, minor,
//   patch and pre-release identifiers in that order (Build metadata does not
//   figure into precedence). Precedence is determined by the first difference
//   when comparing each of these identifiers from left to right as follows:
//   Major, minor, and patch versions are always compared numerically.
//   Example: 1.0.0 < 2.0.0 < 2.1.0 < 2.1.1
//
//   When major, minor, and patch are equal, a pre-release version has lower
//   precedence than a normal version. Example: 1.0.0-alpha < 1.0.0. Precedence
//   for two pre-release versions with the same major, minor, and patch version
//   MUST be determined by comparing each dot separated identifier from left to
//   right until a difference is found as follows: identifiers consisting of
//   only digits are compared numerically and identifiers with letters or
//   hyphens are compared lexically in ASCII sort order. Numeric identifiers
//   always have lower precedence than non-numeric identifiers. A larger set of
//   pre-release fields has a higher precedence than a smaller set, if all of
//   the preceding identifiers are equal. Example: 1.0.0-alpha <
//   1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta < 1.0.0-beta.2 <
//   1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0
pub fn compare_precedence(a: &Version, b: &Version) -> i64 {
    let Version {
        major: a_major,
        minor: a_minor,
        patch: a_patch,
        prerelease: a_pre,
        build: _,
    } = a;
    let Version {
        major: b_major,
        minor: b_minor,
        patch: b_patch,
        prerelease: b_pre,
        build: _,
    } = b;

    fn compare_ints(a: i64, b: i64) -> i64 {
        a - b
    }
    fn compare_pre(a: &[Identifier], b: &[Identifier]) -> i64 {
        compare_identifier_lists(a, b)
    }
    fn chain(k: i64, f: impl FnOnce() -> i64) -> i64 {
        if k != 0 { k } else { f() }
    }

    let k = 0;
    let k = chain(k, || compare_ints(*a_major, *b_major));
    let k = chain(k, || compare_ints(*a_minor, *b_minor));
    let k = chain(k, || compare_ints(*a_patch, *b_patch));
    chain(k, || compare_pre(a_pre, b_pre))
}

pub fn compare(a: &Version, b: &Version) -> i64 {
    let k = compare_precedence(a, b);
    if k != 0 {
        k
    } else {
        let Version { build: a_build, .. } = a;
        let Version { build: b_build, .. } = b;
        compare_identifier_lists(a_build, b_build)
    }
}

pub fn incr_major(v: &Version) -> Version {
    let Version { major, .. } = v;
    Version {
        major: major + 1,
        ..zero()
    }
}

pub fn incr_minor(v: &Version) -> Version {
    let Version { major, minor, .. } = v;
    Version {
        major: *major,
        minor: minor + 1,
        ..zero()
    }
}

pub fn incr_patch(v: &Version) -> Version {
    let Version {
        major,
        minor,
        patch,
        ..
    } = v;
    Version {
        major: *major,
        minor: *minor,
        patch: patch + 1,
        ..zero()
    }
}

pub fn string_of_identifier(id: &Identifier) -> String {
    match id {
        Identifier::Int(x) => x.to_string(),
        Identifier::Str(x) => x.clone(),
    }
}

pub fn to_string(v: &Version) -> String {
    let Version {
        major,
        minor,
        patch,
        prerelease,
        build,
    } = v;
    let prerelease = match prerelease.as_slice() {
        [] => String::new(),
        parts => {
            let s: Vec<String> = parts.iter().map(string_of_identifier).collect();
            format!("-{}", s.join("."))
        }
    };
    let build = match build.as_slice() {
        [] => String::new(),
        parts => {
            let s: Vec<String> = parts.iter().map(string_of_identifier).collect();
            format!("+{}", s.join("."))
        }
    };
    format!("{major}.{minor}.{patch}{prerelease}{build}")
}

#[cfg(test)]
mod tests {
    use super::Identifier;
    use super::Version;
    use super::compare;
    use super::compare_precedence;
    use super::to_string;

    fn v(
        major: i64,
        minor: i64,
        patch: i64,
        prerelease: Vec<Identifier>,
        build: Vec<Identifier>,
    ) -> Version {
        Version {
            major,
            minor,
            patch,
            prerelease,
            build,
        }
    }

    fn v1_0_0_alpha() -> Version {
        v(1, 0, 0, vec![Identifier::Str("alpha".to_string())], vec![])
    }

    fn v1_0_0_alpha_1() -> Version {
        v(
            1,
            0,
            0,
            vec![Identifier::Str("alpha".to_string()), Identifier::Int(1)],
            vec![],
        )
    }

    fn v1_0_0_alpha_beta() -> Version {
        v(
            1,
            0,
            0,
            vec![
                Identifier::Str("alpha".to_string()),
                Identifier::Str("beta".to_string()),
            ],
            vec![],
        )
    }

    fn v1_0_0_beta() -> Version {
        v(1, 0, 0, vec![Identifier::Str("beta".to_string())], vec![])
    }

    fn v1_0_0_beta_2() -> Version {
        v(
            1,
            0,
            0,
            vec![Identifier::Str("beta".to_string()), Identifier::Int(2)],
            vec![],
        )
    }

    fn v1_0_0_beta_11() -> Version {
        v(
            1,
            0,
            0,
            vec![Identifier::Str("beta".to_string()), Identifier::Int(11)],
            vec![],
        )
    }

    fn v1_0_0_rc_1() -> Version {
        v(
            1,
            0,
            0,
            vec![Identifier::Str("rc".to_string()), Identifier::Int(1)],
            vec![],
        )
    }

    fn v1_0_0() -> Version {
        v(1, 0, 0, vec![], vec![])
    }

    fn iter_pairs(f: &mut impl FnMut(&Version, &Version), list: &[Version]) {
        match list {
            [] => (),
            [_] => (),
            [a, rest @ ..] => match rest {
                [] => (),
                [b, ..] => {
                    f(a, b);
                    iter_pairs(f, rest);
                }
            },
        }
    }

    #[test]
    fn compare_precedence_prerelease() {
        let ordered = vec![
            v1_0_0_alpha(),
            v1_0_0_alpha_1(),
            v1_0_0_alpha_beta(),
            v1_0_0_beta(),
            v1_0_0_beta_2(),
            v1_0_0_beta_11(),
            v1_0_0_rc_1(),
            v1_0_0(),
        ];
        iter_pairs(
            &mut |a, b| {
                let a_str = to_string(a);
                let b_str = to_string(b);
                assert!(compare_precedence(a, b) < 0, "{} < {} failed", a_str, b_str);
                assert!(compare_precedence(b, a) > 0, "{} > {} failed", b_str, a_str);
            },
            &ordered,
        );
        for a in &ordered {
            let a_str = to_string(a);
            assert!(
                compare_precedence(a, a) == 0,
                "{} not equal to itself",
                a_str
            );
        }
    }

    #[test]
    fn compare_precedence_build() {
        let a = v(1, 0, 0, vec![], vec![Identifier::Int(1)]);
        let b = v(1, 0, 0, vec![], vec![Identifier::Int(2)]);
        assert!(
            compare_precedence(&a, &b) == 0,
            "1.0.0+1 should be = 1.0.0+2"
        );
    }

    #[test]
    fn compare_build() {
        let a = v(1, 0, 0, vec![], vec![Identifier::Int(1)]);
        let b = v(1, 0, 0, vec![], vec![Identifier::Int(2)]);
        assert!(compare(&a, &b) < 0, "1.0.0+1 should NOT be = 1.0.0+2");
    }
}
