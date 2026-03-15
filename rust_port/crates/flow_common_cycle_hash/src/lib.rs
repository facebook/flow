/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Given a possibly-cyclic directed graph where each node has a hash, this
//! module propagates bits along dependency edges.
//!
//! For example, traversing the graph (C -> B -> A) has the effect of
//! incorporating A's hash into B and that updated B's hash into C.
//!
//! The traversal handles cycles by finding strongly connected components
//! (Tarjan's algorithm) and computing a component hash, which is assigned to
//! each node in the component.
//!
//! For a detailed description of Tarjan's algorithm, see:
//! http://en.wikipedia.org/wiki/Tarjan's_strongly_connected_components_algorithm
//!
//! The method of computing a cyclic hash takes inspiration from the interning
//! logic from Skip (https://github.com/skiplang/skip) which also computes hashes
//! for potentially cyclic graphs of objects.
use std::cell::Cell;
use std::cell::RefCell;
use std::rc::Rc;

use dupe::Dupe;
use flow_common_xx as xx;
use vec1::Vec1;

pub type ReadHash = Box<dyn Fn() -> u64>;

pub type WriteHash = Box<dyn Fn(u64)>;

pub struct Node {
    index: Cell<i32>,
    lowlink: Cell<i32>,
    on_stack: Cell<bool>,
    visit: Box<dyn Fn(&dyn Fn(Rc<Node>), &dyn Fn(&ReadHash))>,
    read_hash: ReadHash,
    write_hash: WriteHash,
}

pub struct Cx {
    count: Cell<i32>,
    stack: RefCell<Vec<Rc<Node>>>,
}

pub fn create_cx() -> Cx {
    Cx {
        count: Cell::new(0),
        stack: RefCell::new(Vec::new()),
    }
}

pub fn create_node(
    visit: Box<dyn Fn(&dyn Fn(Rc<Node>), &dyn Fn(&ReadHash))>,
    read_hash: ReadHash,
    write_hash: WriteHash,
) -> Node {
    Node {
        index: Cell::new(-1),
        lowlink: Cell::new(-1),
        on_stack: Cell::new(false),
        visit,
        read_hash,
        write_hash,
    }
}

// Note that this function does not unset the on_stack field, which ensures that
// on_stack can be used to identify in-cycle nodes when computing cycle hashes.
fn collect_scc(v: &Node, mut acc: Vec<Rc<Node>>, stack: &mut Vec<Rc<Node>>) -> Vec1<Rc<Node>> {
    loop {
        let w = stack.pop().expect("unexpected empty stack");
        if std::ptr::eq(&*w, v) {
            return Vec1::from_vec_push(acc, w);
        } else {
            acc.push(w);
        }
    }
}

/// Once we have a cycle, we compute it's hash by visiting the cycle, combining
/// hashes from dependency nodes.
///
/// Because we find cycles by a recursive traversal, the order of nodes in the
/// cycle is arbitrary. To keep cycle hashes stable, we sort the component by
/// it's local hash value and take the minimum element as the root of the
/// traversal.
///
/// Recall that nodes in this cycle still have the on_stack field set. We abuse
/// this field as a kind of mark bit here. When we see a node with on_stack set,
/// we know that it is part of the cycle. When we visit the node, we unset the
/// field, to record that we've already visited that part of the cycle.
fn calc_cycle_hash(scc: &Vec1<Rc<Node>>) -> u64 {
    let compare =
        |a: &Node, b: &Node| -> std::cmp::Ordering { (a.read_hash)().cmp(&(b.read_hash)()) };

    fn min_elt<'a>(
        compare: impl Fn(&Node, &Node) -> std::cmp::Ordering,
        init: &'a Node,
        rest: &'a [Rc<Node>],
    ) -> &'a Node {
        let mut min: &'a Node = init;
        for x in rest {
            let x_ref: &Node = x;
            if compare(min, x_ref) == std::cmp::Ordering::Greater {
                min = x_ref;
            }
        }
        min
    }

    // Traverse the cycle, combining hashes for all dependency edges.
    fn visit(xx: &RefCell<xx::State>, v: &Node) {
        // Unset on_stack to indicate that we've visited this node.
        v.on_stack.set(false);
        let edge = |w: Rc<Node>| {
            // If on_stack is set, then we have not yet visited this part of the
            // cycle, so push it on the stack.
            if w.on_stack.get() {
                visit(xx, &w);
            }
            xx.borrow_mut().update_int64((w.read_hash)() as i64);
        };
        let dep_edge = |rh: &ReadHash| {
            xx.borrow_mut().update_int64(rh() as i64);
        };
        (v.visit)(&edge, &dep_edge);
    }

    let root = min_elt(compare, &scc[0], &scc[1..]);
    let xx_state = RefCell::new(xx::State::new((root.read_hash)()));
    visit(&xx_state, root);
    xx_state.into_inner().digest()
}

fn strongconnect(cx: &Cx, v: &Rc<Node>) {
    let i = cx.count.get();
    cx.count.set(i + 1);

    // visit node
    v.index.set(cx.count.get());
    v.lowlink.set(cx.count.get());

    // push on stack
    v.on_stack.set(true);
    cx.stack.borrow_mut().push(v.dupe());

    // visit edges
    let edge = |w: Rc<Node>| {
        if w.index.get() == -1 {
            strongconnect(cx, &w);
            v.lowlink.set(v.lowlink.get().min(w.lowlink.get()));
        } else if w.on_stack.get() {
            v.lowlink.set(v.lowlink.get().min(w.index.get()));
        }
    };
    let ignore_dep_edge = |_: &ReadHash| {};
    (v.visit)(&edge, &ignore_dep_edge);

    if v.lowlink.get() == v.index.get() {
        let scc = collect_scc(v, Vec::new(), &mut cx.stack.borrow_mut());
        let cycle_hash = calc_cycle_hash(&scc);

        for x in &scc {
            (x.write_hash)(cycle_hash);
        }
    }
}

pub fn root(cx: &Cx, node: &Rc<Node>) {
    if node.index.get() == -1 {
        strongconnect(cx, node);
    }
}

pub fn read_hash(node: &Node) -> u64 {
    (node.read_hash)()
}

#[cfg(test)]
mod tests {
    use super::*;

    // Define a node in a graph `g` with edges to nodes at the given offsets.
    fn mk_node(
        g: Rc<RefCell<Vec<Option<Rc<Node>>>>>,
        hash: Rc<Cell<u64>>,
        edges: &[usize],
    ) -> Node {
        let edges = edges.to_vec();
        let hash_r = hash.dupe();
        let read_hash: ReadHash = Box::new(move || hash_r.get());
        let hash_w = hash.dupe();
        let write_hash: WriteHash = Box::new(move |v: u64| hash_w.set(v));
        let visit: Box<dyn Fn(&dyn Fn(Rc<Node>), &dyn Fn(&ReadHash))> =
            Box::new(move |edge, _dep_edge| {
                let g = g.borrow();
                for &i in &edges {
                    let node = g[i].as_ref().unwrap().dupe();
                    edge(node);
                }
            });
        create_node(visit, read_hash, write_hash)
    }

    // special case for unitary graph
    fn run(nodes_data: &[(Rc<Cell<u64>>, &[usize])], roots: &[usize]) {
        let n = nodes_data.len();
        let g: Rc<RefCell<Vec<Option<Rc<Node>>>>> =
            Rc::new(RefCell::new((0..n).map(|_| None).collect()));
        for (i, (hash, edges)) in nodes_data.iter().enumerate() {
            let node = mk_node(g.dupe(), hash.dupe(), edges);
            g.borrow_mut()[i] = Some(Rc::new(node));
        }
        let cx = create_cx();
        for &i in roots {
            let node = g.borrow()[i].as_ref().unwrap().dupe();
            root(&cx, &node);
        }
    }

    fn run1(hash: &Rc<Cell<u64>>) {
        run(&[(hash.dupe(), &[])], &[0]);
    }

    // Test that a unit graph's hash is stable
    #[test]
    fn unit_unchanged() {
        let h1 = Rc::new(Cell::new(0u64));
        run1(&h1);
        let h2 = Rc::new(Cell::new(0u64));
        run1(&h2);
        assert_eq!(h1.get(), h2.get());
    }

    // Test that a unit graph's hash updates when the inputs change
    #[test]
    fn unit_changed() {
        let h1 = Rc::new(Cell::new(0u64));
        run1(&h1);
        let h2 = Rc::new(Cell::new(1u64));
        run1(&h2);
        assert_ne!(h1.get(), h2.get());
    }

    // Test that B is stable in B->A when A is stable
    #[test]
    fn acyclic_unchanged() {
        let h1a = Rc::new(Cell::new(0u64));
        let h1b = Rc::new(Cell::new(0u64));
        run(&[(h1a.dupe(), &[] as &[usize]), (h1b.dupe(), &[0])], &[1]);
        let h2a = Rc::new(Cell::new(0u64));
        let h2b = Rc::new(Cell::new(0u64));
        run(&[(h2a.dupe(), &[] as &[usize]), (h2b.dupe(), &[0])], &[1]);
        assert_eq!(h1a.get(), h2a.get());
        assert_eq!(h1b.get(), h2b.get());
    }

    // Test that B changes in B->A when A changes
    #[test]
    fn acyclic_changed() {
        let h1a = Rc::new(Cell::new(0u64));
        let h1b = Rc::new(Cell::new(0u64));
        run(&[(h1a.dupe(), &[] as &[usize]), (h1b.dupe(), &[0])], &[1]);
        let h2a = Rc::new(Cell::new(1u64));
        let h2b = Rc::new(Cell::new(0u64));
        run(&[(h2a.dupe(), &[] as &[usize]), (h2b.dupe(), &[0])], &[1]);
        assert_ne!(h1a.get(), h2a.get());
        assert_ne!(h1b.get(), h2b.get());
    }

    // Test that A,B is stable in B<->A when inputs do not change
    #[test]
    fn cyclic_unchanged() {
        let h1a = Rc::new(Cell::new(0u64));
        let h1b = Rc::new(Cell::new(0u64));
        run(
            &[(h1a.dupe(), &[1usize] as &[usize]), (h1b.dupe(), &[0])],
            &[1],
        );
        let h2a = Rc::new(Cell::new(0u64));
        let h2b = Rc::new(Cell::new(0u64));
        run(
            &[(h2a.dupe(), &[1usize] as &[usize]), (h2b.dupe(), &[0])],
            &[1],
        );
        assert_eq!(h1a.get(), h2a.get());
        assert_eq!(h1b.get(), h2b.get());
    }

    // Test that A,B change in B<->A when inputs change
    #[test]
    fn cyclic_changed() {
        let h1a = Rc::new(Cell::new(0u64));
        let h1b = Rc::new(Cell::new(0u64));
        run(
            &[(h1a.dupe(), &[1usize] as &[usize]), (h1b.dupe(), &[0])],
            &[1],
        );
        let h2a = Rc::new(Cell::new(1u64));
        let h2b = Rc::new(Cell::new(0u64));
        run(
            &[(h2a.dupe(), &[1usize] as &[usize]), (h2b.dupe(), &[0])],
            &[1],
        );
        assert_ne!(h1a.get(), h2a.get());
        assert_ne!(h1b.get(), h2b.get());
    }

    #[test]
    fn cycle_order() {
        // A = ... C ... *
        // B = ... C ... *
        // C = [A, B]
        let h1a = Rc::new(Cell::new(0u64));
        let h1b = Rc::new(Cell::new(1u64));
        let h1c = Rc::new(Cell::new(2u64));
        run(
            &[
                (h1a.dupe(), &[2]),
                (h1b.dupe(), &[2]),
                (h1c.dupe(), &[0, 1]),
            ],
            &[2],
        );
        // A = ... C ... *
        // B = ... C ... *
        // C = [B, A]
        let h2a = Rc::new(Cell::new(0u64));
        let h2b = Rc::new(Cell::new(1u64));
        let h2c = Rc::new(Cell::new(2u64));
        run(
            &[
                (h2a.dupe(), &[2]),
                (h2b.dupe(), &[2]),
                (h2c.dupe(), &[1, 0]),
            ],
            &[2],
        );
        assert_ne!(h1c.get(), h2c.get());
    }

    #[test]
    fn cycle_canon() {
        // visit A<->B cycle starting at A
        let h1a = Rc::new(Cell::new(0u64));
        let h1b = Rc::new(Cell::new(1u64));
        run(
            &[(h1a.dupe(), &[1usize] as &[usize]), (h1b.dupe(), &[0])],
            &[0],
        );
        // A<->B cycle unchanged, but visit starting at B
        let h2a = Rc::new(Cell::new(0u64));
        let h2b = Rc::new(Cell::new(1u64));
        run(
            &[(h2a.dupe(), &[1usize] as &[usize]), (h2b.dupe(), &[0])],
            &[1],
        );
        assert_eq!(h1a.get(), h2a.get());
        assert_eq!(h1b.get(), h2b.get());
    }
}
