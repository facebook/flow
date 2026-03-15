/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Tarjan's algorithm for finding strongly connected components
//!
//! For a detailed description of the algorithm, see:
//! http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
//!
//! The code below is mostly a transcription of the above.

use std::collections::HashMap;

use dupe::Dupe;
use flow_common_utils::graph::Graph;
use vec1::Vec1;

struct Node {
    // visit order, -1 if unvisited
    index: i32,
    // back edge to earliest visited node, -1 when unvisited
    lowlink: i32,
    on_stack: bool,
}

impl Node {
    fn new() -> Self {
        Self {
            index: -1,
            lowlink: -1,
            on_stack: false,
        }
    }
}

// Nodes are K. Edges are dependencies.
struct TopsortState<'a, K: Eq + Ord + std::hash::Hash + Dupe> {
    graph: &'a Graph<K>,
    // nodes, created on demand
    nodes: HashMap<K, Node>,
    // number of nodes visited
    visit_count: i32,
    // nodes in a strongly connected component
    stack: Vec<K>,
    // accumulated components
    components: Vec<Vec1<K>>,
}

impl<'a, K: Eq + Ord + std::hash::Hash + Dupe> TopsortState<'a, K> {
    fn new(graph: &'a Graph<K>) -> Self {
        Self {
            graph,
            nodes: HashMap::new(),
            visit_count: 0,
            stack: Vec::new(),
            components: Vec::new(),
        }
    }

    fn find_or_create_node(&mut self, value: K) -> &mut Node {
        self.nodes.entry(value.dupe()).or_insert_with(Node::new)
    }

    // Return component strongly connected to v.
    fn collect_scc(&mut self, v_value: K) {
        let mut acc = Vec::new();

        loop {
            let w_value = self.stack.pop().expect("unexpected empty stack");

            if let Some(w) = self.nodes.get_mut(&w_value) {
                w.on_stack = false;
            }

            if w_value == v_value {
                let mut component = vec![w_value];
                component.extend(acc);
                let component =
                    Vec1::try_from_vec(component).expect("component should be non-empty");
                self.components.push(component);
                break;
            } else {
                acc.push(w_value);
            }
        }
    }

    // Compute strongly connected component for node v.
    fn strongconnect(&mut self, v_value: K) {
        let i = self.visit_count;
        self.visit_count += 1;

        // visit node
        {
            let v = self.nodes.get_mut(&v_value).unwrap();
            assert_eq!(v.index, -1);
            v.index = i;
            v.lowlink = i;
            v.on_stack = true;
        }

        // push on stack
        self.stack.push(v_value.dupe());

        let edges = self.graph.find_opt(&v_value).cloned().unwrap_or_default();

        // for each edge e:
        // If the edge has not yet been visited, recurse in a depth-first manner.
        // If the edge has been visited, it is a back-edge iff it is on the stack,
        // otherwise it's a cross-edge and can be ignored.
        for e in edges.iter() {
            let w_exists = self.nodes.contains_key(e);

            if !w_exists {
                self.find_or_create_node(e.dupe());
            }

            let w_index = self.nodes.get(e).unwrap().index;
            let w_on_stack = self.nodes.get(e).unwrap().on_stack;

            if w_index == -1 {
                self.strongconnect(e.dupe());

                let w_lowlink = self.nodes.get(e).unwrap().lowlink;
                let v = self.nodes.get_mut(&v_value).unwrap();
                v.lowlink = v.lowlink.min(w_lowlink);
            } else if w_on_stack {
                let v = self.nodes.get_mut(&v_value).unwrap();
                v.lowlink = v.lowlink.min(w_index);
            }
        }

        let (v_lowlink, v_index) = {
            let v = self.nodes.get(&v_value).unwrap();
            (v.lowlink, v.index)
        };

        if v_lowlink == v_index {
            // strongly connected component
            self.collect_scc(v_value);
        }
    }

    // main loop
    fn tarjan<I: IntoIterator<Item = K>>(&mut self, roots: I) {
        for x in roots {
            let needs_visit = {
                self.find_or_create_node(x.dupe());
                self.nodes.get(&x).unwrap().index == -1
            };

            if needs_visit {
                self.strongconnect(x.dupe());
            }
        }
    }
}

// given a map from keys to dependencies, returns whether the dependencies are
// cyclic, as well as a topologically sorted list of key lists where any keys in
// a list only depend on keys in a subsequent list
pub fn topsort<K, I>(roots: I, graph: &Graph<K>) -> Vec<Vec1<K>>
where
    K: Eq + Ord + std::hash::Hash + Dupe,
    I: IntoIterator<Item = K>,
{
    let mut state = TopsortState::new(graph);
    state.tarjan(roots);
    // Reverse to match OCaml's prepend order (:: operator)
    state.components.reverse();
    state.components
}

#[cfg(test)]
mod tarjan_test;
