/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_data_structure_wrapper::smol_str::FlowSmolStr;

use super::FuzzyPath;
use super::MatchResult;
use super::Options;
use super::default_options;

fn candidates() -> Vec<(FlowSmolStr, i32)> {
    vec![
        (FlowSmolStr::from(""), 0),
        (FlowSmolStr::from("a"), 0),
        (FlowSmolStr::from("ab"), 0),
        (FlowSmolStr::from("abC"), 0),
        (FlowSmolStr::from("abcd"), 0),
        (FlowSmolStr::from("alphabetacappa"), 0),
        (FlowSmolStr::from("AlphaBetaCappa"), 0),
        (FlowSmolStr::from("thisisatestdir"), 0),
        (FlowSmolStr::from("/////ThisIsATestDir"), 0),
        (FlowSmolStr::from("/this/is/a/test/dir"), 0),
        (FlowSmolStr::from("/test/tiatd"), 0),
        (FlowSmolStr::from("/zzz/path2/path3/path4"), 0),
        (FlowSmolStr::from("/path1/zzz/path3/path4"), 0),
        (FlowSmolStr::from("/path1/path2/zzz/path4"), 0),
        (FlowSmolStr::from("/path1/path2/path3/zzz"), 0),
    ]
}

fn values(actual: &[MatchResult]) -> Vec<FlowSmolStr> {
    actual.iter().map(|r| r.value.clone()).collect()
}

fn assert_values(expected: &[&str], actual: &[MatchResult]) {
    let expected: Vec<FlowSmolStr> = expected.iter().map(|s| FlowSmolStr::from(*s)).collect();
    assert_eq!(expected, values(actual));
}

fn options() -> Options {
    default_options()
}

#[test]
fn can_match_strings() {
    let mut matcher = FuzzyPath::init(candidates());

    let result = matcher.search("abc", &options());
    assert_values(
        &["abC", "abcd", "AlphaBetaCappa", "alphabetacappa"],
        &result,
    );

    let result = matcher.search("ABC", &options());
    assert_values(
        &["abC", "abcd", "AlphaBetaCappa", "alphabetacappa"],
        &result,
    );

    let result = matcher.search("t/i/a/t/d", &options());
    assert_values(&["/this/is/a/test/dir"], &result);
}

#[test]
fn finds_strong_match() {
    let candidates = vec![
        (FlowSmolStr::from("xabcabc"), 0),
        (FlowSmolStr::from("xAbcabc"), 0),
        (FlowSmolStr::from("xabcAbc"), 0),
    ];
    let mut matcher = FuzzyPath::init(candidates);
    let result = matcher.search("abc", &options());
    assert_values(&["xAbcabc", "xabcAbc", "xabcabc"], &result);

    let options = Options {
        first_match_can_be_weak: false,
        ..options()
    };
    let result = matcher.search("abc", &options);
    assert_values(&["xAbcabc", "xabcAbc"], &result);
}

#[test]
fn prefers_exact_over_abbr_over_others() {
    let mut matcher = FuzzyPath::init(candidates());
    let result = matcher.search("tiatd", &options());
    assert_values(
        &[
            "/test/tiatd",
            "/this/is/a/test/dir",
            "/////ThisIsATestDir",
            "thisisatestdir",
        ],
        &result,
    );
}

#[test]
fn ignores_spaces() {
    let mut matcher = FuzzyPath::init(candidates());
    let result = matcher.search("a b\tcappa", &options());
    assert_values(&["AlphaBetaCappa", "alphabetacappa"], &result);
}

/* There was a bug where this would result in a match. */
#[test]
fn does_not_match_with_excess_chars() {
    let mut matcher = FuzzyPath::init(candidates());
    let result = matcher.search("abcc", &options());
    assert_values(&[], &result);
}

#[test]
fn stable_ordering() {
    let mut matcher = FuzzyPath::init(vec![
        (FlowSmolStr::from("Foo"), 0),
        (FlowSmolStr::from("foo"), 0),
        (FlowSmolStr::from("far"), 0),
        (FlowSmolStr::from("foobar"), 0),
    ]);
    let result = matcher.search("f", &options());
    assert_values(&["far", "foo", "foobar", "Foo"], &result);
}
