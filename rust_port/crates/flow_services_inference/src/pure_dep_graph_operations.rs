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

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::collections::BTreeSet;

    use flow_parser::file_key::FileKeyInner;

    use super::*;

    fn make_fake_file_key(filename: &str) -> FileKey {
        FileKey::new(FileKeyInner::SourceFile(format!(
            "/tmp/fake/path/{}.js",
            filename
        )))
    }

    fn make_filename_set_for_graph(filenames: &[&str]) -> BTreeSet<FileKey> {
        filenames.iter().map(|f| make_fake_file_key(f)).collect()
    }

    fn make_filename_set(filenames: &[&str]) -> FlowOrdSet<FileKey> {
        let mut result = FlowOrdSet::new();
        for filename in filenames {
            result.insert(make_fake_file_key(filename));
        }
        result
    }

    fn make_dependency_graph(lst: &[(&str, Vec<&str>)]) -> Graph<FileKey> {
        let mut map: BTreeMap<FileKey, BTreeSet<FileKey>> = BTreeMap::new();

        for (file, dependencies) in lst {
            let file_key = make_fake_file_key(file);
            if map.contains_key(&file_key) {
                panic!("Duplicate key when constructing map");
            }
            let dependency_set = make_filename_set_for_graph(dependencies);
            map.insert(file_key, dependency_set);
        }

        Graph::of_map(map)
    }

    fn test_calc_all_dependents(
        sig_dependency_graph: &[(&str, Vec<&str>)],
        implementation_dependency_graph: &[(&str, Vec<&str>)],
        root_files: &[&str],
    ) -> FlowOrdSet<FileKey> {
        let sig_dependency_graph = make_dependency_graph(sig_dependency_graph);
        let implementation_dependency_graph =
            make_dependency_graph(implementation_dependency_graph);
        let root_files = make_filename_set(root_files);
        calc_all_dependents(
            &sig_dependency_graph,
            &implementation_dependency_graph,
            &root_files,
        )
    }

    #[test]
    fn test_calc_all_dependents_long_chain() {
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

        let all_dependents = test_calc_all_dependents(
            sig_dependency_graph,
            implementation_dependency_graph,
            root_files,
        );

        let expected_all_dependents = make_filename_set(&["b", "c", "d", "e"]);
        assert_eq!(expected_all_dependents, all_dependents);
    }

    #[test]
    fn test_calc_all_dependents_long_chain_no_sig_dependencies() {
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

        let all_dependents = test_calc_all_dependents(
            sig_dependency_graph,
            implementation_dependency_graph,
            root_files,
        );

        let expected_all_dependents = make_filename_set(&["b", "c"]);
        assert_eq!(expected_all_dependents, all_dependents);
    }
}
