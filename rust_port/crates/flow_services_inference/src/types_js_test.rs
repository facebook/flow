/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common::options::Options;
use flow_common_utils::checked_set::CheckedSet;
use flow_common_utils::graph::Graph;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;
use vec1::Vec1;

use crate::dep_graph_test_utils::make_dependency_graph;
use crate::dep_graph_test_utils::make_fake_file_key;
use crate::dep_graph_test_utils::make_filename_set;
use crate::pure_dep_graph_operations;
use crate::type_service;
use crate::type_service::DetermineWhatToRecheckResult;

// Like `>::` except it expects the function to return `unit Lwt.t` rather than `unit`
// (Adapter macro is not needed in Rust — `#[test]` covers test wrapping; the underlying Rust
// functions are sync, so no Lwt runtime is required.)

fn assert_checked_sets_equal(expected: &CheckedSet, actual: &CheckedSet) {
    assert!(
        expected.debug_equal(actual),
        "expected: {}\nactual: {}",
        expected.debug_to_string(None),
        actual.debug_to_string(None)
    );
}

type FilenameSetSet = BTreeSet<FlowOrdSet<FileKey>>;

fn debug_string_of_filename_set_set(s: &FilenameSetSet) -> String {
    let lines: Vec<String> = s
        .iter()
        .map(|fs| {
            let inner: Vec<String> = fs.iter().map(|f| format!("{:?}", f)).collect();
            format!("  [{}]", inner.join("; "))
        })
        .collect();
    format!("[\n{}\n]", lines.join("\n"))
}

fn filename_set_set_of_nested_list(lst: Vec<Vec<FileKey>>) -> FilenameSetSet {
    lst.into_iter()
        .map(|inner| inner.into_iter().collect::<FlowOrdSet<FileKey>>())
        .collect()
}

fn assert_components_equal(expected: &[&[&str]], actual: &[Vec1<FileKey>]) {
    // `expected`, for convenience, is just a list of lists. `actual` is a list of Nel.ts.
    let expected = filename_set_set_of_nested_list(
        expected
            .iter()
            .map(|inner| inner.iter().map(|s| make_fake_file_key(s)).collect())
            .collect(),
    );
    let actual = filename_set_set_of_nested_list(actual.iter().map(|nel| nel.to_vec()).collect());
    assert!(
        expected == actual,
        "expected: {}\nactual: {}",
        debug_string_of_filename_set_set(&expected),
        debug_string_of_filename_set_set(&actual),
    );
}

// (Profiling not yet ported — the Rust `debug_*` aliases don't take a profiling parameter, so this
// wrapper is omitted; tests call the underlying functions directly.)

fn make_checked_set(focused: &[&str], dependents: &[&str], dependencies: &[&str]) -> CheckedSet {
    let focused = make_filename_set(focused);
    let dependents = make_filename_set(dependents);
    let dependencies = make_filename_set(dependencies);
    let mut result = CheckedSet::empty();
    result.add(Some(focused), Some(dependents), Some(dependencies));
    result
}

fn make_unchanged_checked(checked_files: CheckedSet, freshparsed: &CheckedSet) -> CheckedSet {
    let mut checked_files = checked_files;
    checked_files.remove(&freshparsed.dupe().all());
    checked_files
}

fn prepare_freshparsed(freshparsed: &[&str]) -> CheckedSet {
    CheckedSet::of_focused_list(freshparsed.iter().map(|s| make_fake_file_key(s)).collect())
}

fn checked_files_of_graph(implementation_dependency_graph: &Graph<FileKey>) -> CheckedSet {
    // Get all the files from the implementation_dependency_graph and consider them focused
    // All files must be present as keys, even if they have no values.
    let focused_set = implementation_dependency_graph.fold(
        |file, _, mut acc: FlowOrdSet<FileKey>| {
            acc.insert(file.dupe());
            acc
        },
        FlowOrdSet::new(),
    );
    let mut result = CheckedSet::empty();
    result.add(Some(focused_set), None, None);
    result
}

// `Test_utils.make_options ()` in OCaml builds an `Options.t` from `FlowConfig.empty_config`,
// `File_path.dummy_path`, and the default `make_options_flags ()` / `make_saved_state_flags ()`.
// The code paths exercised by `debug_determine_what_to_recheck` and
// `debug_include_dependencies_and_dependents` only read `options.profile`, `options.node_modules_errors`,
// `options.root`, and `options.file_options` (via `with_memory_timer`, `filter_out_node_modules`,
// and `unfocused_files_to_infer`). For all of these, `Options::default()` is field-equivalent
// to `Test_utils.make_options ()`:
//   - `profile`: OCaml `make_options_flags` defaults `profile = false` → `opt_profile = false`.
//                Rust `Options` derives `Default`; `bool::default() = false`. Equal.
//   - `node_modules_errors`: OCaml `FlowConfig.empty_config` sets `node_modules_errors = false`
//                            → `opt_node_modules_errors = false`. Rust default is `false`. Equal.
//   - `root` / `file_options`: only consulted inside `filter_out_node_modules` when
//                              `node_modules_errors = false`. Since the test inputs are fake
//                              `/tmp/fake/path/<name>.js` paths that contain no `node_modules`
//                              segment, the filter is a no-op regardless of root/file_options.
// Therefore `Options::default()` is provably observationally equivalent to
// `Test_utils.make_options ()` for these tests.
fn make_options() -> Options {
    Options::default()
}

fn determine_what_to_recheck(
    sig_dependency_graph: &[(&str, Vec<&str>)],
    implementation_dependency_graph: &[(&str, Vec<&str>)],
    freshparsed: &[&str],
) -> DetermineWhatToRecheckResult {
    let sig_dependency_graph = make_dependency_graph(sig_dependency_graph);
    let implementation_dependency_graph = make_dependency_graph(implementation_dependency_graph);
    let checked_files = checked_files_of_graph(&implementation_dependency_graph);
    let freshparsed = prepare_freshparsed(freshparsed);
    let options = make_options();
    let unchanged_checked = make_unchanged_checked(checked_files, &freshparsed);
    type_service::debug_determine_what_to_recheck(
        &options,
        &sig_dependency_graph,
        &implementation_dependency_graph,
        &freshparsed,
        unchanged_checked,
        &CheckedSet::empty(),
        &FlowOrdSet::new(),
    )
}

enum CheckedFilesArg<'a> {
    All,
    Lazy(&'a [&'a str]),
}

fn include_dependencies_and_dependents(
    checked_files: CheckedFilesArg<'_>,
    sig_dependency_graph: &[(&str, Vec<&str>)],
    implementation_dependency_graph: &[(&str, Vec<&str>)],
    input_focused: &[&str],
    input_dependents: &[&str],
    input_dependencies: &[&str],
) -> (
    CheckedSet,
    CheckedSet,
    Vec<Vec1<FileKey>>,
    FlowOrdSet<FileKey>,
) {
    let input = {
        let mut s = CheckedSet::empty();
        s.add(
            Some(make_filename_set(input_focused)),
            Some(make_filename_set(input_dependents)),
            Some(make_filename_set(input_dependencies)),
        );
        s
    };
    let changed_files = input.focused().dupe();
    let sig_dependency_graph = make_dependency_graph(sig_dependency_graph);
    let implementation_dependency_graph = make_dependency_graph(implementation_dependency_graph);
    let checked_files = match checked_files {
        CheckedFilesArg::All => checked_files_of_graph(&implementation_dependency_graph),
        CheckedFilesArg::Lazy(lst) => make_checked_set(lst, &[], &[]),
    };
    let unchanged_checked = make_unchanged_checked(checked_files, &input);
    let options = make_options();
    let all_dependent_files = pure_dep_graph_operations::calc_all_dependents(
        &sig_dependency_graph,
        &implementation_dependency_graph,
        &changed_files,
    );
    type_service::debug_include_dependencies_and_dependents(
        &options,
        unchanged_checked,
        input,
        &implementation_dependency_graph,
        &sig_dependency_graph,
        all_dependent_files,
    )
}

// There is memory sampling embedded throughout the code under test. It polls the shared memory
// system to get information about its usage. If the shared memory system is not initialized, we get
// crashes, so we have to initialize it before running tests.
// (Rust port: the underlying `debug_*` aliases do not perform shared-memory sampling, so no
// SharedMem initialization is required for these tests.)

#[test]
fn types_js_determine_what_to_recheck_simple_test() {
    let sig_dependency_graph = &[
        ("a", vec!["b"]),
        ("b", vec!["c", "d"]),
        ("c", vec![]),
        ("d", vec![]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec!["b"]),
        ("b", vec!["c", "d"]),
        ("c", vec![]),
        ("d", vec![]),
    ];
    let freshparsed = &["b"];
    let DetermineWhatToRecheckResult {
        to_merge,
        to_check,
        components: _,
        recheck_set: _,
        dependent_file_count: _,
    } = determine_what_to_recheck(
        sig_dependency_graph,
        implementation_dependency_graph,
        freshparsed,
    );
    let expected_to_merge = make_checked_set(&["b"], &["a"], &[]);
    let expected_to_check = make_checked_set(&["b"], &["a"], &[]);
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
}

#[test]
fn types_js_determine_what_to_recheck_long_chain() {
    let sig_dependency_graph = &[
        ("a", vec![]),
        ("b", vec!["a"]),
        ("c", vec!["b"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec![]),
        ("b", vec!["a"]),
        ("c", vec!["b"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let freshparsed = &["a"];
    let DetermineWhatToRecheckResult {
        to_merge,
        to_check,
        components: _,
        recheck_set: _,
        dependent_file_count: _,
    } = determine_what_to_recheck(
        sig_dependency_graph,
        implementation_dependency_graph,
        freshparsed,
    );
    let expected_to_merge = make_checked_set(&["a"], &["b", "c", "d", "e"], &[]);
    let expected_to_check = make_checked_set(&["a"], &["b", "c", "d", "e"], &[]);
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
}

#[test]
fn types_js_determine_what_to_recheck_long_chain_no_sig_dependencies() {
    let sig_dependency_graph = &[
        ("a", vec![]),
        ("b", vec![]),
        ("c", vec![]),
        ("d", vec![]),
        ("e", vec![]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec![]),
        ("b", vec!["a"]),
        ("c", vec!["b"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let freshparsed = &["a"];
    let DetermineWhatToRecheckResult {
        to_merge,
        to_check,
        components: _,
        recheck_set: _,
        dependent_file_count: _,
    } = determine_what_to_recheck(
        sig_dependency_graph,
        implementation_dependency_graph,
        freshparsed,
    );
    let expected_to_merge = make_checked_set(&["a"], &["b"], &[]);
    let expected_to_check = make_checked_set(&["a"], &["b"], &[]);
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
}

#[test]
fn types_js_determine_what_to_recheck_simple_cycle() {
    let sig_dependency_graph = &[
        ("a", vec!["e"]),
        ("b", vec!["a"]),
        ("c", vec!["b"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec!["e"]),
        ("b", vec!["a"]),
        ("c", vec!["b"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let freshparsed = &["a"];
    let DetermineWhatToRecheckResult {
        to_merge,
        to_check,
        components: _,
        recheck_set: _,
        dependent_file_count: _,
    } = determine_what_to_recheck(
        sig_dependency_graph,
        implementation_dependency_graph,
        freshparsed,
    );
    let expected_to_merge = make_checked_set(&["a"], &["b", "c", "d", "e"], &[]);
    let expected_to_check = make_checked_set(&["a"], &["b", "c", "d", "e"], &[]);
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
}

#[test]
fn types_js_determine_what_to_recheck_simple_cycle_no_sig_dependencies() {
    let sig_dependency_graph = &[
        ("a", vec![]),
        ("b", vec![]),
        ("c", vec![]),
        ("d", vec![]),
        ("e", vec![]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec!["e"]),
        ("b", vec!["a"]),
        ("c", vec!["b"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let freshparsed = &["a"];
    let DetermineWhatToRecheckResult {
        to_merge,
        to_check,
        components: _,
        recheck_set: _,
        dependent_file_count: _,
    } = determine_what_to_recheck(
        sig_dependency_graph,
        implementation_dependency_graph,
        freshparsed,
    );
    let expected_to_merge = make_checked_set(&["a"], &["b"], &[]);
    let expected_to_check = make_checked_set(&["a"], &["b"], &[]);
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
}

#[test]
fn types_js_include_dependencies_and_dependents_simple() {
    let sig_dependency_graph = &[
        ("a", vec!["b"]),
        ("b", vec!["c", "d"]),
        ("c", vec![]),
        ("d", vec![]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec!["b"]),
        ("b", vec!["c", "d"]),
        ("c", vec![]),
        ("d", vec![]),
    ];
    let (to_merge, to_check, _components, _recheck_set) = include_dependencies_and_dependents(
        CheckedFilesArg::All,
        sig_dependency_graph,
        implementation_dependency_graph,
        &["b"],
        &[],
        &[],
    );
    let expected_to_merge = make_checked_set(&["b"], &["a"], &[]);
    let expected_to_check = make_checked_set(&["b"], &["a"], &[]);
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
}

#[test]
fn types_js_include_dependencies_and_dependents_long_chain_no_sig_dependencies() {
    let sig_dependency_graph = &[
        ("a", vec![]),
        ("b", vec![]),
        ("c", vec![]),
        ("d", vec![]),
        ("e", vec![]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec![]),
        ("b", vec!["a"]),
        ("c", vec!["b"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let (to_merge, to_check, _components, _recheck_set) = include_dependencies_and_dependents(
        CheckedFilesArg::All,
        sig_dependency_graph,
        implementation_dependency_graph,
        &["a"],
        &[],
        &[],
    );
    let expected_to_merge = make_checked_set(&["a"], &["b"], &[]);
    let expected_to_check = make_checked_set(&["a"], &["b"], &[]);
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
}

#[test]
fn types_js_include_dependencies_and_dependents_cycle() {
    let sig_dependency_graph = &[
        ("a", vec!["b"]),
        ("b", vec!["c"]),
        ("c", vec!["a"]),
        ("d", vec!["c"]),
        ("e", vec![]),
    ];
    let implementation_dependency_graph = &[
        ("a", vec!["b"]),
        ("b", vec!["c"]),
        ("c", vec!["a", "e"]),
        ("d", vec!["c"]),
        ("e", vec!["d"]),
    ];
    let (to_merge, to_check, components, _recheck_set) = include_dependencies_and_dependents(
        CheckedFilesArg::All,
        sig_dependency_graph,
        implementation_dependency_graph,
        &["a"],
        &[],
        &[],
    );
    let expected_to_merge = make_checked_set(&["a"], &["b", "c", "d", "e"], &[]);
    let expected_to_check = make_checked_set(&["a"], &["b", "c", "d", "e"], &[]);
    let expected_components: &[&[&str]] = &[&["a", "b", "c"], &["d"], &["e"]];
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
    assert_components_equal(expected_components, &components);
}

#[test]
fn types_js_include_dependencies_and_dependents_lazy() {
    let sig_dependency_graph = &[("a", vec![]), ("b", vec![]), ("c", vec![]), ("d", vec![])];
    let implementation_dependency_graph = &[
        ("a", vec!["b"]),
        ("b", vec!["c"]),
        ("c", vec!["d"]),
        ("d", vec![]),
    ];
    let (to_merge, to_check, components, _recheck_set) = include_dependencies_and_dependents(
        CheckedFilesArg::Lazy(&[]),
        sig_dependency_graph,
        implementation_dependency_graph,
        &["d"],
        &[],
        &[],
    );
    let expected_to_merge = make_checked_set(&["d"], &["c"], &[]);
    let expected_to_check = make_checked_set(&["d"], &["c"], &[]);
    let expected_components: &[&[&str]] = &[&["c"], &["d"]];
    assert_checked_sets_equal(&expected_to_merge, &to_merge);
    assert_checked_sets_equal(&expected_to_check, &to_check);
    assert_components_equal(expected_components, &components);
}
