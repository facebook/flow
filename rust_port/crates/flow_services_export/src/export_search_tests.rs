/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common::flow_import_specifier::Userland;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;

use crate::export_index;
use crate::export_index::Kind;
use crate::export_index::Source;
use crate::export_search;
use crate::export_search::default_options;
use crate::export_search_types::SearchResult;
use crate::export_search_types::SearchResultScored;
use crate::export_search_types::SearchResults;

fn sf(name: &str) -> Source {
    Source::FileKey(FileKey::new(FileKeyInner::SourceFile(name.to_string())))
}

fn global() -> Source {
    Source::Global
}

fn declare_module(name: Userland) -> Source {
    Source::Builtin(name)
}

fn make_index() -> export_index::ExportIndex {
    let mut index = export_index::empty();
    export_index::add("F", sf("/a/foo.js"), Kind::Named, &mut index);
    export_index::add("Fo", sf("/a/foo.js"), Kind::Named, &mut index);
    export_index::add("Foo", sf("/a/foo.js"), Kind::Named, &mut index);
    export_index::add("B", sf("/a/bar.js"), Kind::Named, &mut index);
    export_index::add("Ba", sf("/a/bar.js"), Kind::Named, &mut index);
    export_index::add("Bar", sf("/a/bar.js"), Kind::Named, &mut index);
    export_index::add("FooBar", sf("/a/foobar.js"), Kind::Named, &mut index);
    export_index::add("Baz", sf("/a/baz.js"), Kind::Named, &mut index);
    index
}

fn mk_results(is_incomplete: bool, results: Vec<(&str, Source, Kind, i32, i32)>) -> SearchResults {
    SearchResults {
        results: results
            .into_iter()
            .map(|(name, source, kind, score, weight)| SearchResultScored {
                search_result: SearchResult {
                    name: FlowSmolStr::new(name),
                    source,
                    kind,
                },
                score,
                weight,
            })
            .collect(),
        is_incomplete,
    }
}

fn assert_search_results(expected: &SearchResults, actual: &SearchResults) {
    assert_eq!(
        expected.is_incomplete, actual.is_incomplete,
        "is_incomplete mismatch"
    );
    assert_eq!(
        expected.results.len(),
        actual.results.len(),
        "results length mismatch: expected {} got {}",
        expected.results.len(),
        actual.results.len()
    );
    for (i, (exp, act)) in expected
        .results
        .iter()
        .zip(actual.results.iter())
        .enumerate()
    {
        assert_eq!(
            exp.search_result.name, act.search_result.name,
            "result[{}].name mismatch",
            i
        );
        assert_eq!(
            exp.search_result.source, act.search_result.source,
            "result[{}].source mismatch",
            i
        );
        assert_eq!(
            exp.search_result.kind, act.search_result.kind,
            "result[{}].kind mismatch",
            i
        );
        assert_eq!(exp.score, act.score, "result[{}].score mismatch", i);
        assert_eq!(exp.weight, act.weight, "result[{}].weight mismatch", i);
    }
}

#[test]
fn case_insensitive() {
    let index = make_index();
    let mut t = export_search::init(index);
    let results = export_search::search_values(None, "foobar", &mut t);
    let expected = mk_results(
        false,
        vec![("FooBar", sf("/a/foobar.js"), Kind::Named, 40, 0)],
    );
    assert_search_results(&expected, &results);
}

#[test]
fn is_incomplete() {
    let index = make_index();
    let mut t = export_search::init(index);

    let mut options = default_options();
    options.max_results = 2;
    let SearchResults {
        results,
        is_incomplete,
    } = export_search::search_values(Some(&options), "f", &mut t);
    assert!(is_incomplete);
    assert_eq!(2, results.len());

    let mut options = default_options();
    options.max_results = 4;
    let SearchResults {
        results,
        is_incomplete,
    } = export_search::search_values(Some(&options), "f", &mut t);
    assert!(!is_incomplete);
    assert_eq!(4, results.len());

    let mut options = default_options();
    options.max_results = 5;
    let SearchResults {
        results,
        is_incomplete,
    } = export_search::search_values(Some(&options), "f", &mut t);
    assert!(!is_incomplete);
    assert_eq!(4, results.len());
}

#[test]
fn is_incomplete_multiple_files() {
    // if there are 3 "Foo"s and "FooBar", but max_results = 2, make sure
    // we don't add all 3 "Foo"s.
    let mut index = make_index();
    export_index::add("Foo", sf("/a/foo_2.js"), Kind::Named, &mut index);
    export_index::add("Foo", sf("/a/foo_3.js"), Kind::Named, &mut index);
    let mut options = default_options();
    options.max_results = 2;
    let mut t = export_search::init(index);
    let SearchResults {
        results,
        is_incomplete,
    } = export_search::search_values(Some(&options), "Foo", &mut t);
    assert!(is_incomplete);
    assert_eq!(2, results.len());
}

#[test]
fn same_name_different_file() {
    let mut index = make_index();
    export_index::add("FooBar", sf("/a/f.js"), Kind::Named, &mut index);
    let mut t = export_search::init(index);

    let results = export_search::search_values(None, "FooBar", &mut t);
    let expected = mk_results(
        false,
        vec![
            ("FooBar", sf("/a/f.js"), Kind::Named, 44, 0),
            ("FooBar", sf("/a/foobar.js"), Kind::Named, 44, 0),
        ],
    );
    assert_search_results(&expected, &results);
}

#[test]
fn filter_values_and_types() {
    let mut index = make_index();
    export_index::add("FooBar", sf("/a/f_type.js"), Kind::NamedType, &mut index);
    let mut t = export_search::init(index);

    let results = export_search::search_values(None, "FooBar", &mut t);
    let expected = mk_results(
        false,
        vec![("FooBar", sf("/a/foobar.js"), Kind::Named, 44, 0)],
    );
    assert_search_results(&expected, &results);

    let results = export_search::search_types(None, "FooBar", &mut t);
    let expected = mk_results(
        false,
        vec![("FooBar", sf("/a/f_type.js"), Kind::NamedType, 44, 0)],
    );
    assert_search_results(&expected, &results);
}

#[test]
fn max_results_filtered_by_kind() {
    let mut index = make_index();
    export_index::add("FooBar", sf("/a/foobar_a.js"), Kind::Named, &mut index);
    export_index::add("FooBar", sf("/a/foobar_d.js"), Kind::Named, &mut index);
    export_index::add("FooBar", sf("/a/foobar_b.js"), Kind::NamedType, &mut index);
    export_index::add("FooBar", sf("/a/foobar_c.js"), Kind::NamedType, &mut index);
    export_index::add("FooBar", sf("/a/foobar_e.js"), Kind::NamedType, &mut index);
    let mut t = export_search::init(index);

    let mut options = default_options();
    options.max_results = 2;

    let results = export_search::search_values(Some(&options), "FooBar", &mut t);
    let expected = mk_results(
        true,
        vec![
            ("FooBar", sf("/a/foobar.js"), Kind::Named, 44, 0),
            ("FooBar", sf("/a/foobar_a.js"), Kind::Named, 44, 0),
        ],
    );
    assert_search_results(&expected, &results);

    let results = export_search::search_types(Some(&options), "FooBar", &mut t);
    let expected = mk_results(
        true,
        vec![
            ("FooBar", sf("/a/foobar_b.js"), Kind::NamedType, 44, 0),
            ("FooBar", sf("/a/foobar_c.js"), Kind::NamedType, 44, 0),
        ],
    );
    assert_search_results(&expected, &results);
}

#[test]
fn sorted() {
    let file_a = sf("path/to/a.js");
    let file_b = sf("path/to/b.js");
    let file_foo = sf("path/to/foo.js");
    let builtin_z = declare_module(Userland::from_smol_str(FlowSmolStr::new("z")));

    let mut index = export_index::empty();
    export_index::add(
        "foo",
        declare_module(Userland::from_smol_str(FlowSmolStr::new("z"))),
        Kind::Default,
        &mut index,
    );
    export_index::add("foo", sf("path/to/a.js"), Kind::Named, &mut index);
    export_index::add("foo", sf("path/to/b.js"), Kind::Named, &mut index);
    export_index::add("foo", sf("path/to/foo.js"), Kind::Default, &mut index);
    export_index::add("foo", sf("path/to/foo.js"), Kind::Namespace, &mut index);
    export_index::add("foo", Source::Global, Kind::Named, &mut index);
    let mut t = export_search::init(index);

    let results = export_search::search_values(Some(&default_options()), "foo", &mut t);

    // defaults before named before namespace, then
    // globals before builtins before source files
    let expected = mk_results(
        false,
        vec![
            ("foo", builtin_z, Kind::Default, 20, 0),
            ("foo", file_foo, Kind::Default, 20, 0),
            ("foo", global(), Kind::Named, 20, 0),
            ("foo", file_a, Kind::Named, 20, 0),
            ("foo", file_b, Kind::Named, 20, 0),
            ("foo", sf("path/to/foo.js"), Kind::Namespace, 20, 0),
        ],
    );
    assert_search_results(&expected, &results);
}

#[test]
fn weights() {
    let mut index = export_index::empty();
    export_index::add("bar", sf("bar.js"), Kind::Named, &mut index);
    export_index::add("baz", sf("baz.js"), Kind::Named, &mut index);
    export_index::add("baz", sf("baz.js"), Kind::Named, &mut index);
    export_index::add("biz", sf("biz.js"), Kind::Named, &mut index);
    export_index::add("biz", sf("biz.js"), Kind::Named, &mut index);
    export_index::add("biz", sf("biz.js"), Kind::Named, &mut index);
    export_index::add("biz", sf("biz2.js"), Kind::Named, &mut index);
    let mut t = export_search::init(index);

    // the fuzzy score of "b" is the same for "bar", "baz" and "biz"
    let query = "b";

    // without weights: they all tie and are sorted alphabetically
    let mut options = default_options();
    options.weighted = false;
    let results = export_search::search_values(Some(&options), query, &mut t);
    let expected = mk_results(
        false,
        vec![
            ("bar", sf("bar.js"), Kind::Named, 2, 0),
            ("baz", sf("baz.js"), Kind::Named, 2, 0),
            ("biz", sf("biz.js"), Kind::Named, 2, 0),
            ("biz", sf("biz2.js"), Kind::Named, 2, 0),
        ],
    );
    assert_search_results(&expected, &results);

    // with weights: the fuzzy scores tie, so the weights dominate
    let mut options = default_options();
    options.weighted = true;
    let results = export_search::search_values(Some(&options), query, &mut t);
    let expected = mk_results(
        false,
        vec![
            ("biz", sf("biz.js"), Kind::Named, 2, 3),
            ("baz", sf("baz.js"), Kind::Named, 2, 2),
            ("bar", sf("bar.js"), Kind::Named, 2, 1),
            ("biz", sf("biz2.js"), Kind::Named, 2, 1),
        ],
    );
    assert_search_results(&expected, &results);

    // is_incomplete, without weights
    let mut options = default_options();
    options.weighted = false;
    options.max_results = 2;
    let results = export_search::search_values(Some(&options), query, &mut t);
    let expected = mk_results(
        true,
        vec![
            ("bar", sf("bar.js"), Kind::Named, 2, 0),
            ("baz", sf("baz.js"), Kind::Named, 2, 0),
        ],
    );
    assert_search_results(&expected, &results);

    // is_incomplete, with weights
    let mut options = default_options();
    options.weighted = true;
    options.max_results = 2;
    let results = export_search::search_values(Some(&options), query, &mut t);
    let expected = mk_results(
        true,
        vec![
            ("biz", sf("biz.js"), Kind::Named, 2, 3),
            ("baz", sf("baz.js"), Kind::Named, 2, 2),
        ],
    );
    assert_search_results(&expected, &results);
}
