/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::export_service::for_test::inferred_name_of_modulename;

#[test]
fn string_dot() {
    let actual = inferred_name_of_modulename("FooBar.baz");
    let expected = "FooBar";
    assert_eq!(expected, actual.as_str());
}

#[test]
fn filename_dot() {
    let actual = inferred_name_of_modulename("/foo/bar/FooBar.baz.js");
    let expected = "FooBar";
    assert_eq!(expected, actual.as_str());
}

#[test]
fn string_multiple_dots() {
    let actual = inferred_name_of_modulename("FooBar.a.b");
    let expected = "FooBar";
    assert_eq!(expected, actual.as_str());
}

#[test]
fn filename_multiple_dots() {
    let actual = inferred_name_of_modulename("/foo/bar/FooBar.a.b.js");
    let expected = "FooBar";
    assert_eq!(expected, actual.as_str());
}

#[test]
fn string_camelcase_dashes() {
    let actual = inferred_name_of_modulename("foo-bar-baz");
    let expected = "fooBarBaz";
    assert_eq!(expected, actual.as_str());
}

#[test]
fn filename_camelcase_dashes() {
    let actual = inferred_name_of_modulename("/foo/bar/foo-bar-baz.js");
    let expected = "fooBarBaz";
    assert_eq!(expected, actual.as_str());
}

#[test]
fn scoped_package() {
    let actual = inferred_name_of_modulename("@example/xyz");
    let expected = "xyz";
    assert_eq!(expected, actual.as_str());
}
