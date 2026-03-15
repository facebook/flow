/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::sync::Arc;

use dupe::Dupe;
use flow_common_utils::graph::Graph;
use vec1::Vec1;

use crate::topsort;

// Helper to create a graph from key-value pairs
fn graph_from_pairs(pairs: &[(&str, &[&str])]) -> Graph<Arc<str>> {
    let map: BTreeMap<Arc<str>, BTreeSet<Arc<str>>> = pairs
        .iter()
        .map(|(key, values)| (Arc::from(*key), set_from_slice(values)))
        .collect();
    Graph::of_map(map)
}

// Helper to create a set from a slice
fn set_from_slice(items: &[&str]) -> BTreeSet<Arc<str>> {
    items.iter().map(|s| Arc::from(*s)).collect()
}

// Helper to normalize a component (sort elements)
fn normalize_component(comp: &Vec1<Arc<str>>) -> Vec<String> {
    let mut items: Vec<String> = comp.iter().map(|s| s.to_string()).collect();
    items.sort();
    items
}

// Helper to compare two components for equality (order within doesn't matter)
fn components_equal(a: &Vec1<Arc<str>>, b: &Vec1<Arc<str>>) -> bool {
    normalize_component(a) == normalize_component(b)
}

// Helper to compare two SCC results
fn results_equal(expected: &[Vec1<Arc<str>>], actual: &[Vec1<Arc<str>>]) -> bool {
    if expected.len() != actual.len() {
        return false;
    }
    expected
        .iter()
        .zip(actual.iter())
        .all(|(e, a)| components_equal(e, a))
}

// Helper to create Vec1 from a slice
fn vec1_from_slice(items: &[&str]) -> Vec1<Arc<str>> {
    let v: Vec<Arc<str>> = items.iter().map(|s| Arc::from(*s)).collect();
    Vec1::try_from_vec(v).expect("Expected non-empty vec")
}

#[test]
fn test_empty_roots() {
    let graph = graph_from_pairs(&[]);
    let roots = set_from_slice(&[]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    assert_eq!(0, result.len());
}

#[test]
fn test_single_node_no_edges() {
    let graph = graph_from_pairs(&[("a", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    let expected = vec![vec1_from_slice(&["a"])];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_single_node_self_loop() {
    let graph = graph_from_pairs(&[("a", &["a"])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    let expected = vec![vec1_from_slice(&["a"])];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_two_nodes_no_edges() {
    let graph = graph_from_pairs(&[("a", &[]), ("b", &[])]);
    let roots = set_from_slice(&["a", "b"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Order may vary, check both are singleton SCCs
    assert_eq!(2, result.len());
    let mut nodes: Vec<String> = result
        .iter()
        .flat_map(|comp| comp.iter().map(|s| s.to_string()))
        .collect();
    nodes.sort();
    assert_eq!(vec!["a", "b"], nodes);
}

#[test]
fn test_two_nodes_one_edge() {
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Dependent-first order: A depends on B, so [A], [B]
    let expected = vec![vec1_from_slice(&["a"]), vec1_from_slice(&["b"])];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_two_nodes_cycle() {
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["a"])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Single SCC containing both nodes
    assert_eq!(1, result.len());
    let nodes = normalize_component(&result[0]);
    assert_eq!(vec!["a", "b"], nodes);
}

#[test]
fn test_linear_chain_3_nodes() {
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["c"]), ("c", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Dependent-first order: A→B→C means [A], [B], [C]
    let expected = vec![
        vec1_from_slice(&["a"]),
        vec1_from_slice(&["b"]),
        vec1_from_slice(&["c"]),
    ];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_linear_chain_5_nodes() {
    let graph = graph_from_pairs(&[
        ("a", &["b"]),
        ("b", &["c"]),
        ("c", &["d"]),
        ("d", &["e"]),
        ("e", &[]),
    ]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Dependent-first order: A→B→C→D→E means [A], [B], [C], [D], [E]
    let expected = vec![
        vec1_from_slice(&["a"]),
        vec1_from_slice(&["b"]),
        vec1_from_slice(&["c"]),
        vec1_from_slice(&["d"]),
        vec1_from_slice(&["e"]),
    ];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_triangle_cycle() {
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["c"]), ("c", &["a"])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Single SCC containing all three nodes
    assert_eq!(1, result.len());
    let nodes = normalize_component(&result[0]);
    assert_eq!(vec!["a", "b", "c"], nodes);
}

#[test]
fn test_cycle_with_tail() {
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["c"]), ("c", &["b"])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Two SCCs: A depends on {B,C}, so [A], [{B,C}]
    assert_eq!(2, result.len());
    // First SCC should be {A}
    let expected_a = vec1_from_slice(&["a"]);
    assert!(components_equal(&expected_a, &result[0]));
    // Second SCC should be {B, C}
    let scc2 = normalize_component(&result[1]);
    assert_eq!(vec!["b", "c"], scc2);
}

#[test]
fn test_two_separate_cycles() {
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["a"]), ("c", &["d"]), ("d", &["c"])]);
    let roots = set_from_slice(&["a", "c"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Two SCCs: {A, B} and {C, D}
    assert_eq!(2, result.len());
    let mut sccs: Vec<Vec<String>> = result.iter().map(normalize_component).collect();
    sccs.sort();
    assert_eq!(vec![vec!["a", "b"], vec!["c", "d"]], sccs);
}

#[test]
fn test_diamond_pattern() {
    let graph = graph_from_pairs(&[("a", &["b", "c"]), ("b", &["d"]), ("c", &["d"]), ("d", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Four SCCs: A first (depends on all), then B and C (order may vary), then D last
    assert_eq!(4, result.len());
    // A should be first (depends on all others)
    let expected_a = vec1_from_slice(&["a"]);
    assert!(components_equal(&expected_a, &result[0]));
    // D should be last (no dependencies)
    let expected_d = vec1_from_slice(&["d"]);
    assert!(components_equal(&expected_d, &result[3]));
}

#[test]
fn test_figure_8() {
    // A→B→C, B→A (first cycle), C→D→E, D→C (second cycle)
    let graph = graph_from_pairs(&[
        ("a", &["b"]),
        ("b", &["a", "c"]),
        ("c", &["d"]),
        ("d", &["c"]),
    ]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Two SCCs: {A, B} depends on {C, D}, so [{A,B}], [{C,D}]
    assert_eq!(2, result.len());
    let sccs: Vec<Vec<String>> = result.iter().map(normalize_component).collect();
    // {A, B} should come before {C, D} because {A,B}→{C,D}
    assert_eq!(vec!["a", "b"], sccs[0]);
    assert_eq!(vec!["c", "d"], sccs[1]);
}

#[test]
fn test_nested_cycles() {
    // Outer: A→B→C→A, Inner: B→D→B
    let graph = graph_from_pairs(&[
        ("a", &["b"]),
        ("b", &["c", "d"]),
        ("c", &["a"]),
        ("d", &["b"]),
    ]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // All four nodes in one SCC
    assert_eq!(1, result.len());
    let nodes = normalize_component(&result[0]);
    assert_eq!(vec!["a", "b", "c", "d"], nodes);
}

#[test]
fn test_multiple_sccs_varying_sizes() {
    // {A} alone, {B, C} cycle, {D, E, F} cycle
    let graph = graph_from_pairs(&[
        ("a", &["b"]),
        ("b", &["c"]),
        ("c", &["b", "d"]),
        ("d", &["e"]),
        ("e", &["f"]),
        ("f", &["d"]),
    ]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Three SCCs
    assert_eq!(3, result.len());
    let mut scc_sizes: Vec<usize> = result.iter().map(|comp| comp.len()).collect();
    scc_sizes.sort();
    assert_eq!(vec![1, 2, 3], scc_sizes);
}

#[test]
fn test_large_scc() {
    // Six nodes all in one big cycle: A→B→C→D→E→F→A
    let graph = graph_from_pairs(&[
        ("a", &["b"]),
        ("b", &["c"]),
        ("c", &["d"]),
        ("d", &["e"]),
        ("e", &["f"]),
        ("f", &["a"]),
    ]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Single SCC with 6 nodes
    assert_eq!(1, result.len());
    let nodes = normalize_component(&result[0]);
    assert_eq!(vec!["a", "b", "c", "d", "e", "f"], nodes);
}

#[test]
fn test_partial_reachability() {
    // Graph has A→B, C→D, but only A is a root
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &[]), ("c", &["d"]), ("d", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Only A and B should be visited, dependent-first: [A], [B]
    let expected = vec![vec1_from_slice(&["a"]), vec1_from_slice(&["b"])];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_subset_of_roots() {
    // A→B→C, only A is a root, B and C discovered via edges
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["c"]), ("c", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Dependent-first: [A], [B], [C]
    let expected = vec![
        vec1_from_slice(&["a"]),
        vec1_from_slice(&["b"]),
        vec1_from_slice(&["c"]),
    ];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_all_nodes_as_roots() {
    // A→B, B→C, but all three are roots
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["c"]), ("c", &[])]);
    let roots = set_from_slice(&["a", "b", "c"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Same result as if only A was root: [A], [B], [C]
    let expected = vec![
        vec1_from_slice(&["a"]),
        vec1_from_slice(&["b"]),
        vec1_from_slice(&["c"]),
    ];
    assert!(results_equal(&expected, &result));
}

#[test]
fn test_star_pattern() {
    // Hub A points to B, C, D, E (no back edges)
    let graph = graph_from_pairs(&[
        ("a", &["b", "c", "d", "e"]),
        ("b", &[]),
        ("c", &[]),
        ("d", &[]),
        ("e", &[]),
    ]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Five SCCs, all singleton, A should be first (depends on all)
    assert_eq!(5, result.len());
    let expected_a = vec1_from_slice(&["a"]);
    assert!(components_equal(&expected_a, &result[0]));
    // Last four should be B, C, D, E in some order
    let mut last_four: Vec<String> = result[1..5]
        .iter()
        .flat_map(|comp| comp.iter().map(|s| s.to_string()))
        .collect();
    last_four.sort();
    assert_eq!(vec!["b", "c", "d", "e"], last_four);
}

#[test]
fn test_reverse_star() {
    // B, C, D, E all point to hub A
    let graph = graph_from_pairs(&[
        ("b", &["a"]),
        ("c", &["a"]),
        ("d", &["a"]),
        ("e", &["a"]),
        ("a", &[]),
    ]);
    let roots = set_from_slice(&["b", "c", "d", "e"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Five SCCs, {B,C,D,E} should be first (depend on A), A should be last
    assert_eq!(5, result.len());
    let expected_a = vec1_from_slice(&["a"]);
    assert!(components_equal(&expected_a, &result[4]));
}

#[test]
fn test_verify_reverse_topo_order() {
    // A→B→C→D, verify dependents appear before dependencies
    let graph = graph_from_pairs(&[("a", &["b"]), ("b", &["c"]), ("c", &["d"]), ("d", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Build index map: node -> position in result
    let mut indices: HashMap<Arc<str>, usize> = HashMap::new();
    for (i, comp) in result.iter().enumerate() {
        for node in comp.iter() {
            indices.insert(node.dupe(), i);
        }
    }
    // For each edge u→v in graph, verify index[u] < index[v] (dependent before dependency)
    let check_edge = |u: &str, v: &str| {
        let u_idx = indices.get(&Arc::from(u)).unwrap();
        let v_idx = indices.get(&Arc::from(v)).unwrap();
        assert!(
            u_idx < v_idx,
            "Edge {}→{}: expected {} < {}",
            u,
            v,
            u_idx,
            v_idx
        );
    };
    check_edge("a", "b");
    check_edge("b", "c");
    check_edge("c", "d");
}

#[test]
fn test_cross_edges_dont_create_sccs() {
    // A→B→C with cross-edge A→C (doesn't create cycle)
    let graph = graph_from_pairs(&[("a", &["b", "c"]), ("b", &["c"]), ("c", &[])]);
    let roots = set_from_slice(&["a"]);
    let result = topsort(roots.iter().map(|r| r.dupe()), &graph);
    // Three separate SCCs (cross-edge doesn't form cycle): [A], [B], [C]
    let expected = vec![
        vec1_from_slice(&["a"]),
        vec1_from_slice(&["b"]),
        vec1_from_slice(&["c"]),
    ];
    assert!(results_equal(&expected, &result));
}
