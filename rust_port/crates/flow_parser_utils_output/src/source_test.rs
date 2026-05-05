/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::source::Source;

fn mk_source() -> Source {
    Source::create()
}

fn assert_contents_equal(expected: &str, source: Source) {
    assert_eq!(expected, source.contents());
}

#[test]
fn simple_string() {
    let mut s = mk_source();
    s.add_string("foo;");
    assert_contents_equal("foo;", s);
}

#[test]
fn two_strings() {
    let mut s = mk_source();
    s.add_string("foo;").add_string("bar;");
    assert_contents_equal("foo;bar;", s);
}
