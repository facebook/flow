/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use dupe::Dupe;
use flow_common_utils::graph::Graph;
use flow_parser::file_key::FileKey;
use flow_utils_concurrency::thread_pool::ThreadPool;

#[derive(Clone)]
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

impl DependencyInfo {
    pub fn empty() -> Self {
        Self {
            sig_dependency_graph: Graph::new(),
            implementation_dependency_graph: Graph::new(),
        }
    }

    pub fn of_map(pool: &ThreadPool, map: PartialDependencyGraph) -> Self {
        let mut sig_dependency_map = BTreeMap::new();
        let mut implementation_dependency_map = BTreeMap::new();

        for (k, (sig_deps, impl_deps)) in map.inner {
            sig_dependency_map.insert(k.dupe(), sig_deps);
            implementation_dependency_map.insert(k, impl_deps);
        }

        let (implementation_dependency_graph, sig_dependency_graph) = pool.install(|| {
            rayon::join(
                || Graph::of_map(implementation_dependency_map),
                || Graph::of_map(sig_dependency_map),
            )
        });

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
        let mut updated_sig_map = BTreeMap::new();
        let mut updated_impl_map = BTreeMap::new();

        for (k, (sig_deps, impl_deps)) in partial_dep_graph.inner {
            updated_sig_map.insert(k.dupe(), sig_deps);
            updated_impl_map.insert(k, impl_deps);
        }

        Self {
            sig_dependency_graph: old_dep_info
                .sig_dependency_graph
                .update_from_map(updated_sig_map, to_remove.clone()),
            implementation_dependency_graph: old_dep_info
                .implementation_dependency_graph
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
        format!(
            "Sig dependency graph:\n{:?}\nImplementation dependency graph:\n{:?}",
            self.sig_dependency_graph.to_map(),
            self.implementation_dependency_graph.to_map()
        )
    }
}
