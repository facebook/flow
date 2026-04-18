/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// A directional graph which maintains back edges to facilitate fast traversals in either direction.

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::hash::Hash;

use dupe::Dupe;

#[derive(Clone)]
struct Node<K> {
    forward: BTreeSet<K>,
    // These edges are mutable *only* for efficiency during construction. Once the graph is
    // constructed these should never be mutated.
    backward: BTreeSet<K>,
}

#[derive(Clone)]
pub struct Graph<K: Eq + Ord + Hash + Dupe> {
    nodes: BTreeMap<K, Node<K>>,
}

impl<K: Eq + Ord + Hash + Dupe> Graph<K> {
    pub fn new() -> Self {
        Self {
            nodes: BTreeMap::new(),
        }
    }

    pub fn of_map(map: BTreeMap<K, BTreeSet<K>>) -> Self {
        let mut nodes: BTreeMap<K, Node<K>> = BTreeMap::new();

        // Single pass: consume map and build both forward and backward edges
        for (key, forward_edges) in map {
            // Ensure the source node exists
            nodes.entry(key.dupe()).or_insert_with(|| Node {
                forward: BTreeSet::new(),
                backward: BTreeSet::new(),
            });

            // Add backward edges for each forward edge
            for edge in &forward_edges {
                nodes
                    .entry(edge.dupe())
                    .or_insert_with(|| Node {
                        forward: BTreeSet::new(),
                        backward: BTreeSet::new(),
                    })
                    .backward
                    .insert(key.dupe());
            }

            // Set the forward edges
            nodes.get_mut(&key).unwrap().forward = forward_edges;
        }

        Self { nodes }
    }

    pub fn update_from_map(
        mut self,
        map: BTreeMap<K, BTreeSet<K>>,
        to_remove: BTreeSet<K>,
    ) -> Self {
        // First, make changes as needed based on `map`. This includes updating dependency edges and
        // adding entirely new nodes.
        for (key, new_forward_edges) in map {
            // Take ownership of previous forward edges to avoid cloning
            let previous_forward_edges = self
                .nodes
                .get_mut(&key)
                .map(|n| std::mem::take(&mut n.forward))
                .unwrap_or_default();

            // For each new forward edge not in previous, add a backward edge
            for edge in &new_forward_edges {
                if !previous_forward_edges.contains(edge) {
                    self.nodes
                        .entry(edge.dupe())
                        .or_insert_with(|| Node {
                            forward: BTreeSet::new(),
                            backward: BTreeSet::new(),
                        })
                        .backward
                        .insert(key.dupe());
                }
            }

            // For each previous forward edge not in new, remove its backward edge
            for edge in &previous_forward_edges {
                if !new_forward_edges.contains(edge) {
                    if let Some(node) = self.nodes.get_mut(edge) {
                        node.backward.remove(&key);
                    }
                }
            }

            // Ensure the source node exists and set forward edges (move, not clone)
            self.nodes
                .entry(key)
                .or_insert_with(|| Node {
                    forward: BTreeSet::new(),
                    backward: BTreeSet::new(),
                })
                .forward = new_forward_edges;
        }

        // Now, remove nodes as needed based on `to_remove`. This requires fixing up both
        // forward edges and backward edges which point to the entries to remove, as well as removing
        // the entries themselves.
        for key_to_remove in to_remove {
            // In practice we sometimes get asked to remove nodes that aren't present to begin
            // with. That's a bit weird, but let's just tolerate that by doing nothing.
            if let Some(node) = self.nodes.remove(&key_to_remove) {
                // Remove forward dependency edges that refer to this key
                for backward_key in &node.backward {
                    if let Some(backward_node) = self.nodes.get_mut(backward_key) {
                        backward_node.forward.remove(&key_to_remove);
                    }
                }

                // Remove backward dependency edges that refer to this key
                for forward_key in &node.forward {
                    if let Some(forward_node) = self.nodes.get_mut(forward_key) {
                        forward_node.backward.remove(&key_to_remove);
                    }
                }
            }
        }

        self
    }

    pub fn to_map(&self) -> BTreeMap<K, BTreeSet<K>> {
        self.nodes
            .iter()
            .map(|(k, node)| (k.dupe(), node.forward.clone()))
            .collect()
    }

    pub fn to_backward_map(&self) -> BTreeMap<K, BTreeSet<K>> {
        self.nodes
            .iter()
            .map(|(k, node)| (k.dupe(), node.backward.clone()))
            .collect()
    }

    #[allow(dead_code)]
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }

    pub fn find(&self, elt: &K) -> &BTreeSet<K> {
        self.find_opt(elt)
            .unwrap_or_else(|| panic!("Node not found in graph"))
    }

    pub fn find_opt(&self, elt: &K) -> Option<&BTreeSet<K>> {
        self.nodes.get(elt).map(|node| &node.forward)
    }

    pub fn find_backward(&self, elt: &K) -> &BTreeSet<K> {
        self.find_backward_opt(elt)
            .unwrap_or_else(|| panic!("Node not found in graph"))
    }

    pub fn find_backward_opt(&self, elt: &K) -> Option<&BTreeSet<K>> {
        self.nodes.get(elt).map(|node| &node.backward)
    }

    pub fn fold<F, A>(&self, mut f: F, init: A) -> A
    where
        F: FnMut(&K, &BTreeSet<K>, A) -> A,
    {
        self.nodes
            .iter()
            .fold(init, |acc, (key, node)| f(key, &node.forward, acc))
    }

    pub fn map<F>(&self, mut f: F) -> Self
    where
        F: FnMut(&K) -> K,
    {
        let mut new_nodes = BTreeMap::new();

        for (elt, node) in &self.nodes {
            let new_elt = f(elt);
            let new_forward: BTreeSet<K> = node.forward.iter().map(&mut f).collect();
            let new_backward: BTreeSet<K> = node.backward.iter().map(&mut f).collect();

            if new_nodes.contains_key(&new_elt) {
                panic!("Duplicate keys created by function passed to Graph.map");
            }

            new_nodes.insert(
                new_elt,
                Node {
                    forward: new_forward,
                    backward: new_backward,
                },
            );
        }

        Self { nodes: new_nodes }
    }
}

impl<K: Eq + Ord + Hash + Dupe> Default for Graph<K> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;

    type StringGraph = Graph<Arc<str>>;

    fn assert_ssets_equal(expected: &BTreeSet<Arc<str>>, actual: &BTreeSet<Arc<str>>) {
        assert_eq!(expected, actual);
    }

    fn assert_smaps_equal(
        expected: &BTreeMap<Arc<str>, BTreeSet<Arc<str>>>,
        actual: &BTreeMap<Arc<str>, BTreeSet<Arc<str>>>,
    ) {
        assert_eq!(expected, actual);
    }

    fn sset_of_list(items: &[&str]) -> BTreeSet<Arc<str>> {
        items.iter().map(|s| Arc::from(*s)).collect()
    }

    fn smap_of_list(pairs: &[(&str, &[&str])]) -> BTreeMap<Arc<str>, BTreeSet<Arc<str>>> {
        pairs
            .iter()
            .map(|(key, values)| (Arc::from(*key), sset_of_list(values)))
            .collect()
    }

    fn map() -> BTreeMap<Arc<str>, BTreeSet<Arc<str>>> {
        smap_of_list(&[("foo", &["bar", "baz"]), ("bar", &["baz"]), ("baz", &[])])
    }

    fn reverse_map() -> BTreeMap<Arc<str>, BTreeSet<Arc<str>>> {
        smap_of_list(&[("foo", &[]), ("bar", &["foo"]), ("baz", &["foo", "bar"])])
    }

    fn graph() -> StringGraph {
        StringGraph::of_map(map())
    }

    #[test]
    fn basic_construction() {
        let graph = graph();
        let result_forward_map = graph.to_map();
        let result_backward_map = graph.to_backward_map();
        assert_smaps_equal(&map(), &result_forward_map);
        assert_smaps_equal(&reverse_map(), &result_backward_map);
    }

    #[test]
    fn find() {
        let graph = graph();
        let result = graph.find(&Arc::from("foo"));
        let expected = map().get(&Arc::from("foo") as &Arc<str>).unwrap().clone();
        assert_ssets_equal(&expected, result);
    }

    #[test]
    fn find_opt() {
        let graph = graph();
        let result = graph.find_opt(&Arc::from("foo"));
        let result = result.unwrap();
        let expected = map().get(&Arc::from("foo") as &Arc<str>).unwrap().clone();
        assert_ssets_equal(&expected, result);
    }

    #[test]
    fn find_opt_none() {
        let graph = graph();
        let result = graph.find_opt(&Arc::from("qux"));
        let expected: Option<&BTreeSet<Arc<str>>> = None;
        assert_eq!(expected, result);
    }

    #[test]
    fn find_backward() {
        let graph = graph();
        let result = graph.find_backward(&Arc::from("baz"));
        let expected = reverse_map()
            .get(&Arc::from("baz") as &Arc<str>)
            .unwrap()
            .clone();
        assert_ssets_equal(&expected, result);
    }

    #[test]
    fn find_backward_opt() {
        let graph = graph();
        let result = graph.find_backward_opt(&Arc::from("baz"));
        let result = result.unwrap();
        let expected = reverse_map()
            .get(&Arc::from("baz") as &Arc<str>)
            .unwrap()
            .clone();
        assert_ssets_equal(&expected, result);
    }

    #[test]
    fn find_backward_opt_none() {
        let graph = graph();
        let result = graph.find_backward_opt(&Arc::from("qux"));
        let expected: Option<&BTreeSet<Arc<str>>> = None;
        assert_eq!(expected, result);
    }

    #[test]
    fn add() {
        let graph = graph();
        let update_map = smap_of_list(&[("qux", &["foo", "baz"])]);
        let graph = graph.update_from_map(update_map.clone(), BTreeSet::new());
        let result_forward_map = graph.to_map();
        let mut expected_forward_map = map();
        for (k, v) in update_map {
            expected_forward_map.insert(k, v);
        }
        let result_backward_map = graph.to_backward_map();
        let mut expected_backward_map = reverse_map();
        expected_backward_map.insert(Arc::from("foo"), sset_of_list(&["qux"]));
        expected_backward_map.insert(Arc::from("baz"), sset_of_list(&["qux", "foo", "bar"]));
        expected_backward_map.insert(Arc::from("qux"), BTreeSet::new());
        assert_smaps_equal(&expected_forward_map, &result_forward_map);
        assert_smaps_equal(&expected_backward_map, &result_backward_map);
    }

    #[test]
    fn remove() {
        let graph = graph();
        let mut to_remove = BTreeSet::new();
        to_remove.insert(Arc::from("bar"));
        let graph = graph.update_from_map(BTreeMap::new(), to_remove);
        let result_forward_map = graph.to_map();
        let result_backward_map = graph.to_backward_map();
        let expected_forward_map = smap_of_list(&[("foo", &["baz"]), ("baz", &[])]);
        let expected_backward_map = smap_of_list(&[("foo", &[]), ("baz", &["foo"])]);
        assert_smaps_equal(&expected_forward_map, &result_forward_map);
        assert_smaps_equal(&expected_backward_map, &result_backward_map);
    }

    #[test]
    fn remove_all() {
        let graph = graph();
        let to_remove: BTreeSet<Arc<str>> = map().keys().cloned().collect();
        let graph = graph.update_from_map(BTreeMap::new(), to_remove);
        let result_forward_map = graph.to_map();
        let result_backward_map = graph.to_backward_map();
        let expected: BTreeMap<Arc<str>, BTreeSet<Arc<str>>> = BTreeMap::new();
        assert_smaps_equal(&expected, &result_forward_map);
        assert_smaps_equal(&expected, &result_backward_map);
    }

    #[test]
    fn remove_nonexistent() {
        let graph = graph();
        let mut to_remove = BTreeSet::new();
        to_remove.insert(Arc::from("fake"));
        let graph = graph.update_from_map(BTreeMap::new(), to_remove);
        let result_forward_map = graph.to_map();
        let result_backward_map = graph.to_backward_map();
        assert_smaps_equal(&map(), &result_forward_map);
        assert_smaps_equal(&reverse_map(), &result_backward_map);
    }

    #[test]
    fn modify() {
        let graph = graph();
        let update_map = smap_of_list(&[("foo", &[])]);
        let graph = graph.update_from_map(update_map, BTreeSet::new());
        let result_forward_map = graph.to_map();
        let result_backward_map = graph.to_backward_map();
        let mut expected_forward_map = map();
        expected_forward_map.insert(Arc::from("foo"), BTreeSet::new());
        let expected_backward_map = smap_of_list(&[("foo", &[]), ("bar", &[]), ("baz", &["bar"])]);
        assert_smaps_equal(&expected_forward_map, &result_forward_map);
        assert_smaps_equal(&expected_backward_map, &result_backward_map);
    }

    #[test]
    fn modify_complex() {
        let graph = graph();
        let update_map = smap_of_list(&[("foo", &["qux"]), ("qux", &["baz"])]);
        let mut to_remove = BTreeSet::new();
        to_remove.insert(Arc::from("bar"));
        let graph = graph.update_from_map(update_map, to_remove);
        let result_forward_map = graph.to_map();
        let result_backward_map = graph.to_backward_map();
        let expected_forward_map =
            smap_of_list(&[("foo", &["qux"]), ("baz", &[]), ("qux", &["baz"])]);
        let expected_backward_map =
            smap_of_list(&[("foo", &[]), ("baz", &["qux"]), ("qux", &["foo"])]);
        assert_smaps_equal(&expected_forward_map, &result_forward_map);
        assert_smaps_equal(&expected_backward_map, &result_backward_map);
    }

    #[test]
    fn map_test() {
        let graph = graph();
        let mapped_graph = graph.map(|x| Arc::from(format!("foo_{}", x).as_str()));
        let result_forward_map = mapped_graph.to_map();
        let expected_forward_map = smap_of_list(&[
            ("foo_foo", &["foo_bar", "foo_baz"]),
            ("foo_bar", &["foo_baz"]),
            ("foo_baz", &[]),
        ]);
        assert_smaps_equal(&expected_forward_map, &result_forward_map);
    }
}
