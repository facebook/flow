/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use flow_common_utils::graph::Graph;
use flow_parser::file_key::FileKey;
use flow_utils_concurrency::thread_pool::ThreadPool;

#[derive(Clone, serde::Serialize, serde::Deserialize)]
pub struct DependencyInfo {
    sig_dependency_graph: Graph<FileKey>,
    implementation_dependency_graph: Graph<FileKey>,
}

pub struct PartialDependencyGraph {
    pub(crate) inner: BTreeMap<FileKey, (BTreeSet<FileKey>, BTreeSet<FileKey>)>,
}

impl PartialDependencyGraph {
    pub fn new() -> Self {
        Self {
            inner: BTreeMap::new(),
        }
    }

    pub fn from_map(map: BTreeMap<FileKey, (BTreeSet<FileKey>, BTreeSet<FileKey>)>) -> Self {
        Self { inner: map }
    }

    pub fn insert(
        &mut self,
        key: FileKey,
        value: (BTreeSet<FileKey>, BTreeSet<FileKey>),
    ) -> Option<(BTreeSet<FileKey>, BTreeSet<FileKey>)> {
        self.inner.insert(key, value)
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&FileKey, &(BTreeSet<FileKey>, BTreeSet<FileKey>))> {
        self.inner.iter()
    }

    pub fn into_inner(self) -> BTreeMap<FileKey, (BTreeSet<FileKey>, BTreeSet<FileKey>)> {
        self.inner
    }
}

fn extract_sig_map(
    map: BTreeMap<FileKey, (BTreeSet<FileKey>, BTreeSet<FileKey>)>,
) -> BTreeMap<FileKey, BTreeSet<FileKey>> {
    map.into_iter()
        .map(|(file, (sig_deps, _impl_deps))| (file, sig_deps))
        .collect()
}

fn extract_impl_map(
    map: BTreeMap<FileKey, (BTreeSet<FileKey>, BTreeSet<FileKey>)>,
) -> BTreeMap<FileKey, BTreeSet<FileKey>> {
    map.into_iter()
        .map(|(file, (_sig_deps, impl_deps))| (file, impl_deps))
        .collect()
}

impl DependencyInfo {
    pub fn from_graphs(
        sig_dependency_graph: Graph<FileKey>,
        implementation_dependency_graph: Graph<FileKey>,
    ) -> Self {
        Self {
            sig_dependency_graph,
            implementation_dependency_graph,
        }
    }

    pub fn empty() -> Self {
        Self {
            sig_dependency_graph: Graph::new(),
            implementation_dependency_graph: Graph::new(),
        }
    }

    pub fn of_map(pool: &ThreadPool, map: PartialDependencyGraph) -> Self {
        let _ = pool;
        let sig_dependency_map = extract_sig_map(map.inner.clone());
        let sig_dependency_graph = Graph::of_map(sig_dependency_map);
        let implementation_dependency_map = extract_impl_map(map.inner);
        let implementation_dependency_graph = Graph::of_map(implementation_dependency_map);
        Self {
            sig_dependency_graph,
            implementation_dependency_graph,
        }
    }

    pub fn update(
        old_dep_info: DependencyInfo,
        partial_dep_graph: PartialDependencyGraph,
        to_remove: BTreeSet<FileKey>,
    ) -> Self {
        let DependencyInfo {
            sig_dependency_graph: old_sig_graph,
            implementation_dependency_graph: old_impl_graph,
        } = old_dep_info;
        let updated_map = partial_dep_graph.inner;
        let updated_sig_map = extract_sig_map(updated_map.clone());
        let updated_impl_map = extract_impl_map(updated_map);
        Self {
            sig_dependency_graph: old_sig_graph.update_from_map(updated_sig_map, to_remove.clone()),
            implementation_dependency_graph: old_impl_graph
                .update_from_map(updated_impl_map, to_remove),
        }
    }

    pub fn implementation_dependency_graph(&self) -> &Graph<FileKey> {
        &self.implementation_dependency_graph
    }

    pub fn sig_dependency_graph(&self) -> &Graph<FileKey> {
        &self.sig_dependency_graph
    }

    pub fn debug_to_string(&self) -> String {
        let DependencyInfo {
            sig_dependency_graph,
            implementation_dependency_graph,
        } = self;
        let sig_map = sig_dependency_graph.to_map();
        let impl_map = implementation_dependency_graph.to_map();
        format!(
            "Sig dependency graph:\n{:?}\nImplementation dependency graph:\n{:?}",
            sig_map, impl_map
        )
    }
}
