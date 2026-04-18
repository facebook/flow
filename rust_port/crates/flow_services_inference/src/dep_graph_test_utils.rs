/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use flow_common_utils::graph::Graph;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;

pub fn make_fake_file_key(filename: &str) -> FileKey {
    FileKey::new(FileKeyInner::SourceFile(format!(
        "/tmp/fake/path/{}.js",
        filename
    )))
}

pub fn make_filename_set(filenames: &[&str]) -> FlowOrdSet<FileKey> {
    filenames.iter().map(|f| make_fake_file_key(f)).collect()
}

// Note: OCaml's `FilenameSet` is the same type used by both `make_filename_set` and
// `FilenameGraph.of_map`. In Rust, `make_filename_set` returns `FlowOrdSet<FileKey>` (used by
// callers comparing against `calc_all_dependents` output), while `Graph::of_map` requires
// `BTreeSet<FileKey>`. We build the inner dependency set directly as a `BTreeSet` here rather
// than calling `make_filename_set` and converting.
pub fn make_dependency_graph(lst: &[(&str, Vec<&str>)]) -> Graph<FileKey> {
    let map = lst.iter().fold(
        BTreeMap::<FileKey, BTreeSet<FileKey>>::new(),
        |mut map, (file, dependencies)| {
            let file = make_fake_file_key(file);
            if map.contains_key(&file) {
                panic!("Duplicate key when constructing map");
            }
            let dependency_set = dependencies.iter().map(|f| make_fake_file_key(f)).collect();
            map.insert(file, dependency_set);
            map
        },
    );
    Graph::of_map(map)
}
