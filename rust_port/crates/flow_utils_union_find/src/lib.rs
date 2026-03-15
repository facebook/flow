/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use flow_data_structure_wrapper::int_map::IntHashMap;

pub type Ident = i32;

#[derive(Debug, Clone)]
pub struct TvarNotFound(pub Ident);

impl fmt::Display for TvarNotFound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Tvar not found: {}", self.0)
    }
}

impl std::error::Error for TvarNotFound {}

#[derive(Debug, Clone)]
pub struct Root<C> {
    pub rank: i32,
    pub constraints: C,
}

#[derive(Debug, Clone)]
pub enum Node<C> {
    Goto { parent: Ident },
    Root(Root<C>),
}

impl<C> Node<C> {
    pub fn create_root(constraints: C) -> Self {
        Node::Root(Root {
            rank: 0,
            constraints,
        })
    }

    pub fn create_goto(parent: Ident) -> Self {
        Node::Goto { parent }
    }
}

#[derive(Debug, Clone)]
pub struct Graph<C>(IntHashMap<Ident, Node<C>>);

impl<C> Default for Graph<C> {
    fn default() -> Self {
        Self::new()
    }
}

impl<C> Graph<C> {
    pub fn new() -> Self {
        Graph(IntHashMap::default())
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Graph(IntHashMap::with_capacity_and_hasher(
            capacity,
            Default::default(),
        ))
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn insert(&mut self, id: Ident, node: Node<C>) {
        self.0.insert(id, node);
    }

    pub fn get(&self, id: &Ident) -> Option<&Node<C>> {
        self.0.get(id)
    }

    pub fn get_mut(&mut self, id: &Ident) -> Option<&mut Node<C>> {
        self.0.get_mut(id)
    }

    /// Iterate over all nodes in the graph mutably.
    /// Used to walk the graph for cycle-breaking cleanup.
    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut Node<C>> {
        self.0.values_mut()
    }

    pub fn find_root(&mut self, id: Ident) -> Result<(Ident, &mut Root<C>), TvarNotFound> {
        // First, find the root id without holding any borrows
        let root_id = self.find_root_id_inner(id)?;

        // Single mutable lookup to return the root
        let node = self.0.get_mut(&root_id).unwrap();
        match node {
            Node::Root(root) => Ok((root_id, root)),
            _ => unreachable!(),
        }
    }

    /// Find the root id and perform path compression, without returning a mutable reference.
    /// This avoids the double-lookup problem in find_root.
    fn find_root_id_inner(&mut self, id: Ident) -> Result<Ident, TvarNotFound> {
        let node = self.0.get(&id).ok_or(TvarNotFound(id))?;
        match node {
            Node::Root(_) => Ok(id),
            Node::Goto { parent } => {
                let parent_id = *parent;
                let root_id = self.find_root_id_inner(parent_id)?;
                // Path compression: point directly to root
                if let Some(Node::Goto { parent }) = self.0.get_mut(&id) {
                    *parent = root_id;
                }
                Ok(root_id)
            }
        }
    }

    pub fn find_root_id(&mut self, id: Ident) -> Result<Ident, TvarNotFound> {
        self.find_root_id_inner(id)
    }

    pub fn find_constraints(&mut self, id: Ident) -> Result<(Ident, &mut C), TvarNotFound> {
        let (root_id, root) = self.find_root(id)?;
        Ok((root_id, &mut root.constraints))
    }

    pub fn find_graph(&mut self, id: Ident) -> Result<&mut C, TvarNotFound> {
        let (_, constraints) = self.find_constraints(id)?;
        Ok(constraints)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    type TestConstraints = String;

    #[test]
    fn test_create_root() {
        let node = Node::create_root("test".to_string());
        match node {
            Node::Root(root) => {
                assert_eq!(root.rank, 0);
                assert_eq!(root.constraints, "test");
            }
            _ => panic!("Expected Root node"),
        }
    }

    #[test]
    fn test_create_goto() {
        let node: Node<TestConstraints> = Node::create_goto(42);
        match node {
            Node::Goto { parent } => {
                assert_eq!(parent, 42);
            }
            _ => panic!("Expected Goto node"),
        }
    }

    #[test]
    fn test_find_root_direct() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root1".to_string()));

        let (root_id, root) = graph.find_root(1).unwrap();
        assert_eq!(root_id, 1);
        assert_eq!(root.constraints, "root1");
        assert_eq!(root.rank, 0);
    }

    #[test]
    fn test_find_root_not_found() {
        let mut graph: Graph<TestConstraints> = Graph::new();
        let result = graph.find_root(999);
        assert!(result.is_err());
        match result {
            Err(TvarNotFound(id)) => assert_eq!(id, 999),
            _ => panic!("Expected TvarNotFound error"),
        }
    }

    #[test]
    fn test_find_root_single_goto() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root".to_string()));
        graph.insert(2, Node::create_goto(1));

        let (root_id, root) = graph.find_root(2).unwrap();
        assert_eq!(root_id, 1);
        assert_eq!(root.constraints, "root");
    }

    #[test]
    fn test_find_root_chain() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root".to_string()));
        graph.insert(2, Node::create_goto(1));
        graph.insert(3, Node::create_goto(2));
        graph.insert(4, Node::create_goto(3));

        let (root_id, root) = graph.find_root(4).unwrap();
        assert_eq!(root_id, 1);
        assert_eq!(root.constraints, "root");
    }

    #[test]
    fn test_path_compression() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root".to_string()));
        graph.insert(2, Node::create_goto(1));
        graph.insert(3, Node::create_goto(2));

        // First find_root on node 3
        let (root_id, _) = graph.find_root(3).unwrap();
        assert_eq!(root_id, 1);

        // After path compression, node 3 should point directly to node 1
        match graph.get(&3).unwrap() {
            Node::Goto { parent } => {
                assert_eq!(*parent, 1, "Node 3 should now point directly to root 1");
            }
            _ => panic!("Node 3 should be a Goto node"),
        }

        // Node 2 should also be compressed
        match graph.get(&2).unwrap() {
            Node::Goto { parent } => {
                assert_eq!(*parent, 1, "Node 2 should now point directly to root 1");
            }
            _ => panic!("Node 2 should be a Goto node"),
        }
    }

    #[test]
    fn test_find_root_id() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root".to_string()));
        graph.insert(2, Node::create_goto(1));
        graph.insert(3, Node::create_goto(2));

        assert_eq!(graph.find_root_id(1).unwrap(), 1);
        assert_eq!(graph.find_root_id(2).unwrap(), 1);
        assert_eq!(graph.find_root_id(3).unwrap(), 1);
    }

    #[test]
    fn test_find_constraints() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root1".to_string()));
        graph.insert(2, Node::create_goto(1));

        let (root_id, constraints) = graph.find_constraints(2).unwrap();
        assert_eq!(root_id, 1);
        assert_eq!(*constraints, "root1");

        // Mutate constraints
        *constraints = "modified".to_string();

        // Verify mutation persisted
        let (_, constraints) = graph.find_constraints(2).unwrap();
        assert_eq!(*constraints, "modified");
    }

    #[test]
    fn test_find_graph() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root1".to_string()));
        graph.insert(2, Node::create_goto(1));

        let constraints = graph.find_graph(2).unwrap();
        assert_eq!(*constraints, "root1");

        // Mutate through find_graph
        *constraints = "updated".to_string();

        // Verify mutation
        let constraints = graph.find_graph(1).unwrap();
        assert_eq!(*constraints, "updated");
    }

    #[test]
    fn test_multiple_trees() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("tree1".to_string()));
        graph.insert(2, Node::create_goto(1));
        graph.insert(3, Node::create_goto(1));

        graph.insert(10, Node::create_root("tree2".to_string()));
        graph.insert(11, Node::create_goto(10));
        graph.insert(12, Node::create_goto(10));

        // Verify tree 1
        assert_eq!(graph.find_root_id(1).unwrap(), 1);
        assert_eq!(graph.find_root_id(2).unwrap(), 1);
        assert_eq!(graph.find_root_id(3).unwrap(), 1);

        // Verify tree 2
        assert_eq!(graph.find_root_id(10).unwrap(), 10);
        assert_eq!(graph.find_root_id(11).unwrap(), 10);
        assert_eq!(graph.find_root_id(12).unwrap(), 10);

        // Verify constraints are separate
        let c1 = graph.find_graph(2).unwrap();
        assert_eq!(*c1, "tree1");

        let c2 = graph.find_graph(11).unwrap();
        assert_eq!(*c2, "tree2");
    }

    #[test]
    fn test_tvar_not_found_display() {
        let err = TvarNotFound(123);
        assert_eq!(format!("{}", err), "Tvar not found: 123");
    }

    #[test]
    fn test_graph_clone() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root".to_string()));
        graph.insert(2, Node::create_goto(1));

        let mut graph2 = graph.clone();

        // Modify clone
        let constraints = graph2.find_graph(1).unwrap();
        *constraints = "modified".to_string();

        // Original should be unchanged
        let constraints = graph.find_graph(1).unwrap();
        assert_eq!(*constraints, "root");

        // Clone should have modification
        let constraints = graph2.find_graph(1).unwrap();
        assert_eq!(*constraints, "modified");
    }

    /// Test path compression on long chains (20+ nodes)
    /// Verifies that path compression works correctly for deeply nested structures
    /// and that all intermediate nodes point directly to root after traversal.
    #[test]
    fn test_long_chain_compression() {
        let mut graph = Graph::new();
        graph.insert(0, Node::create_root("root".to_string()));

        // Create a chain of 20 nodes: 1 → 0, 2 → 1, 3 → 2, ..., 20 → 19
        for i in 1..=20 {
            graph.insert(i, Node::create_goto(i - 1));
        }

        // Find root from the deepest node
        let (root_id, _) = graph.find_root(20).unwrap();
        assert_eq!(root_id, 0);

        // After path compression, all intermediate nodes should point directly to root
        for i in 1..=20 {
            match graph.get(&i).unwrap() {
                Node::Goto { parent } => {
                    assert_eq!(
                        *parent, 0,
                        "Node {} should point directly to root after compression",
                        i
                    );
                }
                _ => panic!("Node {} should be a Goto node", i),
            }
        }
    }

    /// Test error handling when a node's parent doesn't exist (broken chain)
    /// This documents that broken chains result in TvarNotFound errors.
    #[test]
    fn test_broken_parent_chain() {
        let mut graph: Graph<TestConstraints> = Graph::new();
        // Create a node pointing to a non-existent parent
        graph.insert(1, Node::create_goto(99)); // Parent 99 doesn't exist

        let result = graph.find_root(1);
        assert!(result.is_err());
        match result {
            Err(TvarNotFound(id)) => {
                assert_eq!(id, 99, "Error should reference the missing parent node");
            }
            _ => panic!("Expected TvarNotFound error for missing parent"),
        }
    }

    /// Test that all API functions properly propagate errors for missing nodes
    /// Ensures consistent error handling across the entire API surface.
    #[test]
    fn test_error_propagation_all_apis() {
        let mut graph: Graph<TestConstraints> = Graph::new();

        // All APIs should return TvarNotFound for missing node
        assert!(graph.find_root(999).is_err(), "find_root should error");
        assert!(
            graph.find_root_id(999).is_err(),
            "find_root_id should error"
        );
        assert!(
            graph.find_constraints(999).is_err(),
            "find_constraints should error"
        );
        assert!(graph.find_graph(999).is_err(), "find_graph should error");

        // Verify error contains correct ID
        match graph.find_root_id(999) {
            Err(TvarNotFound(id)) => assert_eq!(id, 999),
            _ => panic!("Expected TvarNotFound(999)"),
        }
    }

    /// Test that mutations through one API are visible through other APIs
    /// Verifies that all find operations access the same underlying constraint data.
    #[test]
    fn test_find_after_mutation() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("initial".to_string()));
        graph.insert(2, Node::create_goto(1));

        // Mutate through node 2 using find_graph
        let constraints = graph.find_graph(2).unwrap();
        *constraints = "modified".to_string();

        // Verify mutation is visible through node 1
        let constraints = graph.find_graph(1).unwrap();
        assert_eq!(*constraints, "modified");

        // Verify mutation is visible through find_constraints
        let (_, constraints) = graph.find_constraints(2).unwrap();
        assert_eq!(*constraints, "modified");

        // Verify mutation is visible through find_root
        let (_, root) = graph.find_root(1).unwrap();
        assert_eq!(root.constraints, "modified");
    }

    /// Test mutations to constraints through find_constraints persist correctly
    /// Ensures mutable references returned by find_constraints actually mutate the graph.
    #[test]
    fn test_constraint_mutation_persistence() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("original".to_string()));
        graph.insert(2, Node::create_goto(1));
        graph.insert(3, Node::create_goto(2));

        // Mutate through deeply nested node
        {
            let (root_id, constraints) = graph.find_constraints(3).unwrap();
            assert_eq!(root_id, 1);
            *constraints = "updated".to_string();
        }

        // Verify mutation persists through all access paths
        assert_eq!(*graph.find_graph(1).unwrap(), "updated");
        assert_eq!(*graph.find_graph(2).unwrap(), "updated");
        assert_eq!(*graph.find_graph(3).unwrap(), "updated");
    }

    /// Test that path compression doesn't break subsequent finds
    /// Verifies that after compressing a chain, all nodes remain accessible.
    #[test]
    fn test_find_after_compression() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root".to_string()));
        graph.insert(2, Node::create_goto(1));
        graph.insert(3, Node::create_goto(2));
        graph.insert(4, Node::create_goto(3));

        // First find compresses the chain
        assert_eq!(graph.find_root_id(4).unwrap(), 1);

        // All subsequent finds should still work
        assert_eq!(graph.find_root_id(3).unwrap(), 1);
        assert_eq!(graph.find_root_id(2).unwrap(), 1);
        assert_eq!(graph.find_root_id(1).unwrap(), 1);

        // Verify all nodes are accessible through find_graph
        assert_eq!(*graph.find_graph(4).unwrap(), "root");
        assert_eq!(*graph.find_graph(3).unwrap(), "root");
        assert_eq!(*graph.find_graph(2).unwrap(), "root");
        assert_eq!(*graph.find_graph(1).unwrap(), "root");
    }

    /// Test interleaved operations: find, mutate, find again
    /// Simulates real usage patterns with mixed read and write operations.
    #[test]
    fn test_interleaved_operations() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("v1".to_string()));
        graph.insert(2, Node::create_goto(1));
        graph.insert(3, Node::create_goto(2));

        // Find and verify initial state
        assert_eq!(*graph.find_graph(3).unwrap(), "v1");

        // Mutate
        *graph.find_graph(2).unwrap() = "v2".to_string();

        // Find again and verify mutation
        assert_eq!(*graph.find_graph(3).unwrap(), "v2");

        // Another mutation through different node
        let (_, root) = graph.find_root(1).unwrap();
        root.constraints = "v3".to_string();

        // Verify through all nodes
        assert_eq!(*graph.find_graph(1).unwrap(), "v3");
        assert_eq!(*graph.find_graph(2).unwrap(), "v3");
        assert_eq!(*graph.find_graph(3).unwrap(), "v3");
    }

    /// Test empty graph operations
    /// Documents behavior when operating on an empty graph.
    #[test]
    fn test_empty_graph() {
        let mut graph: Graph<TestConstraints> = Graph::new();

        // All operations on empty graph should error
        assert!(graph.find_root(1).is_err());
        assert!(graph.find_root_id(1).is_err());
        assert!(graph.find_constraints(1).is_err());
        assert!(graph.find_graph(1).is_err());
        assert!(graph.get(&1).is_none());
        assert!(graph.get_mut(&1).is_none());
    }

    /// Test that rank is initialized to 0 for new roots
    /// Verifies rank field initialization matches OCaml specification.
    #[test]
    fn test_rank_initialization() {
        let mut graph = Graph::new();
        graph.insert(1, Node::create_root("root1".to_string()));
        graph.insert(2, Node::create_root("root2".to_string()));

        let (_, root1) = graph.find_root(1).unwrap();
        assert_eq!(root1.rank, 0, "New root should have rank 0");

        let (_, root2) = graph.find_root(2).unwrap();
        assert_eq!(root2.rank, 0, "New root should have rank 0");
    }
}
