/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common_utils::graph::Graph;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;

use crate::dep_graph_test_utils::make_dependency_graph;
use crate::dep_graph_test_utils::make_filename_set;
use crate::pure_dep_graph_operations;

fn assert_sets_equal(expected: FlowOrdSet<FileKey>, actual: FlowOrdSet<FileKey>) {
    assert_eq!(expected, actual);
}

fn calc_all_dependents(
    sig_dependency_graph: &[(&str, Vec<&str>)],
    implementation_dependency_graph: &[(&str, Vec<&str>)],
    root_files: &[&str],
) -> FlowOrdSet<FileKey> {
    let sig_dependency_graph: Graph<FileKey> = make_dependency_graph(sig_dependency_graph);
    let implementation_dependency_graph: Graph<FileKey> =
        make_dependency_graph(implementation_dependency_graph);
    let root_files = make_filename_set(root_files);
    pure_dep_graph_operations::calc_all_dependents(
        &sig_dependency_graph,
        &implementation_dependency_graph,
        &root_files,
    )
}

#[test]
fn pure_dep_graph_operations_calc_all_dependents_long_chain() {
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
    let root_files = &["b"];
    let all_dependents = calc_all_dependents(
        sig_dependency_graph,
        implementation_dependency_graph,
        root_files,
    );
    let expected_all_dependents = make_filename_set(&["b", "c", "d", "e"]);
    assert_sets_equal(expected_all_dependents, all_dependents);
}

#[test]
fn pure_dep_graph_operations_calc_all_dependents_long_chain_no_sig_dependencies() {
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
    let root_files = &["b"];
    let all_dependents = calc_all_dependents(
        sig_dependency_graph,
        implementation_dependency_graph,
        root_files,
    );
    // See comments on `calc_all_dependents` for why this is expected behavior.
    let expected_all_dependents = make_filename_set(&["b", "c"]);
    assert_sets_equal(expected_all_dependents, all_dependents);
}
