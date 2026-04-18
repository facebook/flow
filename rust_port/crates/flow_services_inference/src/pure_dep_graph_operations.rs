/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Contains pure functions which perform calculations on the dependency graph

use dupe::Dupe;
use flow_common_utils::graph::Graph;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;

/// `rdep_closure graph files` returns all files in `graph` which are reachable
/// from `files`, directly or indirectly.
fn rdep_closure(graph: &Graph<FileKey>, files: &FlowOrdSet<FileKey>) -> FlowOrdSet<FileKey> {
    fn loop_impl(
        graph: &Graph<FileKey>,
        files: &FlowOrdSet<FileKey>,
        acc: &mut FlowOrdSet<FileKey>,
    ) {
        for file in files {
            if let Some(rdeps) = graph.find_backward_opt(file) {
                let mut new_files = FlowOrdSet::new();
                for rdep in rdeps {
                    if !acc.contains(rdep) {
                        new_files.insert(rdep.dupe());
                    }
                }
                if !new_files.is_empty() {
                    for f in &new_files {
                        acc.insert(f.dupe());
                    }
                    loop_impl(graph, &new_files, acc);
                }
            }
        }
    }

    let mut result = files.dupe();
    loop_impl(graph, files, &mut result);
    result
}

/// `calc_direct_dependencies graph files` will return the set of direct dependencies of
/// `files`. This set includes `files`.
pub fn calc_direct_dependencies(
    dependency_graph: &Graph<FileKey>,
    files: &FlowOrdSet<FileKey>,
) -> FlowOrdSet<FileKey> {
    let mut result = files.dupe();

    for file in files {
        if let Some(deps) = dependency_graph.find_opt(file) {
            for dep in deps {
                result.insert(dep.dupe());
            }
        }
    }

    result
}

/// `calc_direct_dependents graph files` will return the set of direct dependents of
/// `files`. This set includes `files`.
pub fn calc_direct_dependents(
    dependency_graph: &Graph<FileKey>,
    files: &FlowOrdSet<FileKey>,
) -> FlowOrdSet<FileKey> {
    let mut result = files.dupe();

    for file in files {
        if let Some(deps) = dependency_graph.find_backward_opt(file) {
            for dep in deps {
                result.insert(dep.dupe());
            }
        }
    }

    result
}

/// `calc_all_dependents graph files` will return the set of direct and transitive dependents of
/// `files`. This set include `files`.
///
/// A file is a dependent of `files` whenever its code depends on any file whose *signature*, in
/// turn, directly or transitively depends on `files`.
pub fn calc_all_dependents(
    sig_dependency_graph: &Graph<FileKey>,
    implementation_dependency_graph: &Graph<FileKey>,
    files: &FlowOrdSet<FileKey>,
) -> FlowOrdSet<FileKey> {
    let all_sig_dependents = rdep_closure(sig_dependency_graph, files);
    calc_direct_dependents(implementation_dependency_graph, &all_sig_dependents)
}
