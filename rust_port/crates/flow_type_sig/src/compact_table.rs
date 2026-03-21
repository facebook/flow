/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module is useful for converting a data structure into a compact,
// table-based representation. This module supports cycles and ensures the
// resulting tables preserve insertion order.
//
// For example, we use this to create a compact representation of file
// signatures, where each named definition is stored in source order in an array
// and each reference is an offset into that array.
//
// The way you use this data structure is similar to a copying collector, but
// instead of copying data from an old space to a new space, we are copying from
// one representation to another representation, where the new representation
// may only contain parts of the original.
//
// Usage of this data structure can be broken down into phases:
//
// 1. Construction
//
// For each "table" in the compact output, create a builder. Use that builder to
// construct nodes, which can then be stored in the data structure you will
// eventually compact.
//
// The order of insertion will be preserved in the final compacted table, so if
// you push A and then push B, the final table will be [A, B].
//
// If your data structure contains a cycle, any back edges must be stored within
// a node, as nodes contain the machinery to detect and handle cycles.
//
// For example, in the signature parsing phase, the scope is a mapping from
// names to nodes. Resolved references to those names are also nodes which have
// been looked up from the scope.
//
// 2. Marking
//
// Only nodes which have been marked will appear in the compacted table. To mark
// nodes, visit whatever data structure you are using to store the nodes,
// calling `mark` on each visited node.
//
// For example, in signature parsing we only mark nodes which are reachable from
// the exports of the file. Any nodes which are not reachable correspond to
// definitions which can be excluded from the signature entirely.
//
// 3. Compaction
//
// The `compact` function returns a new value which elides unmarked nodes
// entirely. Furthermore, any marked nodes are now imbued with an index that
// represents the 0-based offset into the table, which can be retrieved using
// the `index_exn` function.
//
// Note that the table has not actually been created at this point. Rather,
// we've only calculated the index of where the data will be, once copied.
//
// Once compacted, the builder is "consumed" and reverts to its initial state.
// The builder can be re-used to build a new table starting at phase 1.
//
// 4. Copy
//
// The `copy` function will allocate an array populated with data. This is the
// fully compacted table. The provided transform function will be applied to
// each marked node, in order, to produce the value stored at each index.

use std::cell::Cell;
use std::cell::Ref;
use std::cell::RefCell;
use std::cell::RefMut;
use std::fmt::Debug;
use std::ops::Deref;

use dupe::Dupe;

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Index<Marker> {
    pub(crate) index: usize,
    pub(crate) _phantom: std::marker::PhantomData<Marker>,
}

impl<Marker> Index<Marker> {
    pub fn new(index: usize) -> Self {
        Index {
            index,
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn as_usize(&self) -> usize {
        self.index
    }
}

impl<Marker> Debug for Index<Marker> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.index)
    }
}

impl<Marker> Clone for Index<Marker> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<Marker> Copy for Index<Marker> {}

impl<Marker> PartialEq for Index<Marker> {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl<Marker> Eq for Index<Marker> {}

impl<Marker> PartialOrd for Index<Marker> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<Marker> Ord for Index<Marker> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}

impl<Marker> std::hash::Hash for Index<Marker> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MarkStatus {
    Unmarked,
    Marked,
    MarkedDirty,
}

// Nodes form a doubly linked list using immutable references with interior mutability.
pub(super) struct NodeInner<'a, T> {
    data: RefCell<T>,
    next: Cell<Option<&'a NodeInner<'a, T>>>,
    prev: Cell<Option<&'a NodeInner<'a, T>>>,
    mark_status: RefCell<MarkStatus>,
    index: Cell<isize>,
}

/// A handle to a node in the builder's arena.
/// This holds an immutable reference to the node.
pub(super) struct Node<'a, T> {
    inner: &'a NodeInner<'a, T>,
}

impl<'a, T> Node<'a, T> {
    pub(super) fn data(&self) -> Ref<'_, T> {
        self.inner.data.borrow()
    }

    pub(super) fn data_mut(&self) -> RefMut<'_, T> {
        self.inner.data.borrow_mut()
    }

    pub(super) fn mark(&self, visitor: impl FnOnce(&T) -> bool) {
        if *self.inner.mark_status.borrow() != MarkStatus::Unmarked {
            return;
        }
        self.inner.mark_status.replace(MarkStatus::Marked);
        let is_dirty = visitor(&self.inner.data.borrow());
        self.inner.mark_status.replace(if is_dirty {
            MarkStatus::MarkedDirty
        } else {
            MarkStatus::Marked
        });
    }
}

impl<'a, T> Clone for Node<'a, T> {
    fn clone(&self) -> Self {
        Self { inner: self.inner }
    }
}

impl<'a, T> Dupe for Node<'a, T> {}

/// Builder for constructing a compact table.
/// Nodes are stored in a typed arena and linked via immutable references.
pub(super) struct Builder<'a, T> {
    arena: &'a bumpalo::Bump,
    head: Option<&'a NodeInner<'a, T>>,
}

impl<'a, T> Builder<'a, T> {
    pub(super) fn new(arena: &'a bumpalo::Bump) -> Self {
        Self { arena, head: None }
    }

    pub(super) fn new_with_shared_arena(&self) -> Self {
        Self {
            arena: self.arena,
            head: None,
        }
    }

    // Note that the builder accumulates nodes in insertion order.
    pub(super) fn push(&mut self, data: T) -> Node<'a, T> {
        let data = RefCell::new(data);
        let mark_status = RefCell::new(MarkStatus::Unmarked);

        match self.head {
            None => {
                // First node - needs circular references to itself
                // We need to use unsafe to create the circular reference
                let node_inner = self.arena.alloc(NodeInner {
                    data,
                    next: Cell::new(None),
                    prev: Cell::new(None),
                    mark_status,
                    index: Cell::new(-1),
                });
                // Now fix up the circular references
                node_inner.next.set(Some(node_inner));
                node_inner.prev.set(Some(node_inner));
                self.head = Some(node_inner);
                Node { inner: node_inner }
            }
            Some(head) => {
                let last = head.prev.get().unwrap();
                let node_inner = self.arena.alloc(NodeInner {
                    data,
                    next: Cell::new(Some(head)),
                    prev: Cell::new(Some(last)),
                    mark_status,
                    index: Cell::new(-1),
                });
                head.prev.set(Some(node_inner));
                last.next.set(Some(node_inner));
                Node { inner: node_inner }
            }
        }
    }

    pub(super) fn tail_exn(&self) -> Node<'a, T> {
        match self.head {
            None => panic!("tail_exn: empty builder"),
            Some(head) => {
                let last = head.prev.get().unwrap();
                Node { inner: last }
            }
        }
    }

    pub(super) fn merge_spliced(&self, into: Node<'a, T>, b: Builder<'a, T>) {
        match b.head {
            None => {}
            Some(head) => {
                let last = head.prev.get().unwrap();
                let next = into.inner.next.get().unwrap();
                head.prev.set(Some(into.inner));
                into.inner.next.set(Some(head));
                last.next.set(Some(next));
                next.prev.set(Some(last));
            }
        }
    }

    #[cfg(test)]
    fn splice<F, R>(&self, into: Node<'a, T>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let mut b = Builder::new(self.arena);
        let x = f(&mut b);
        self.merge_spliced(into, b);
        x
    }

    pub(super) fn compact_with_merge(self, merge: impl Fn(&T, &T) -> Option<T>) -> Indexed<'a, T> {
        self.compact(Some(merge))
    }

    pub(super) fn compact_without_merge(self) -> Indexed<'a, T> {
        self.compact(None::<fn(&T, &T) -> Option<T>>)
    }

    fn compact<F>(mut self, merge: Option<F>) -> Indexed<'a, T>
    where
        F: Fn(&T, &T) -> Option<T>,
    {
        fn loop1<'a, T, F>(
            head: &'a NodeInner<'a, T>,
            mut prev: &'a NodeInner<'a, T>,
            mut node: &'a NodeInner<'a, T>,
            merge: Option<&F>,
        ) -> CompactedNodes<'a, T>
        where
            F: Fn(&T, &T) -> Option<T>,
        {
            loop {
                if std::ptr::eq(node, head) {
                    head.prev.set(Some(prev));
                    prev.next.set(Some(head));
                    let size = (prev.index.get() + 1) as usize;
                    return CompactedNodes { head, size };
                } else if node.mark_status.borrow().deref() != &MarkStatus::Unmarked {
                    match merge {
                        Some(merge_fn) => {
                            let merge_result = {
                                let prev_data = prev.data.borrow();
                                let node_data = node.data.borrow();
                                merge_fn(&prev_data, &node_data)
                            };
                            match merge_result {
                                None => {
                                    node.index.set(prev.index.get() + 1);
                                    node.prev.set(Some(prev));
                                    prev.next.set(Some(node));
                                    prev = node;
                                    node = node.next.get().unwrap();
                                }
                                Some(merged) => {
                                    prev.data.replace(merged);
                                    node.index.set(prev.index.get());
                                    node = node.next.get().unwrap();
                                }
                            }
                        }
                        None => {
                            // No merge function, don't merge
                            node.index.set(prev.index.get() + 1);
                            node.prev.set(Some(prev));
                            prev.next.set(Some(node));
                            prev = node;
                            node = node.next.get().unwrap();
                        }
                    }
                } else {
                    node = node.next.get().unwrap();
                }
            }
        }

        fn loop0<'a, T, F>(
            head: &'a NodeInner<'a, T>,
            prev: &'a NodeInner<'a, T>,
            mut node: &'a NodeInner<'a, T>,
            merge: Option<&F>,
        ) -> Option<CompactedNodes<'a, T>>
        where
            F: Fn(&T, &T) -> Option<T>,
        {
            loop {
                if std::ptr::eq(node, head) {
                    return None;
                } else if node.mark_status.borrow().deref() != &MarkStatus::Unmarked {
                    node.index.set(0);
                    node.prev.set(Some(prev));
                    prev.next.set(Some(node));
                    let next = node.next.get().unwrap();
                    return Some(loop1(node, node, next, merge));
                } else {
                    node = node.next.get().unwrap();
                }
            }
        }

        match self.head.take() {
            None => Indexed { inner: None },
            Some(head) => {
                if head.mark_status.borrow().deref() != &MarkStatus::Unmarked {
                    head.index.set(0);
                    let next = head.next.get().unwrap();
                    Indexed {
                        inner: Some(loop1(head, head, next, merge.as_ref())),
                    }
                } else {
                    let next = head.next.get().unwrap();
                    let inner = loop0(head, head, next, merge.as_ref());
                    Indexed { inner }
                }
            }
        }
    }
}

impl<'a, Marker> Node<'a, Marker> {
    pub fn index_exn<T>(&self) -> Index<T> {
        let index = self.inner.index.get();
        assert!(index >= 0, "Node has not been compacted or is not marked");
        Index {
            index: index as usize,
            _phantom: std::marker::PhantomData,
        }
    }
}

struct CompactedNodes<'a, T> {
    head: &'a NodeInner<'a, T>,
    size: usize,
}

pub(super) struct Indexed<'a, T> {
    inner: Option<CompactedNodes<'a, T>>,
}

impl<'a, T> Indexed<'a, T> {
    pub(super) fn copy<F, U>(self, mut f: F) -> (Table<U>, Vec<usize>)
    where
        F: FnMut(&T) -> U,
    {
        match self.inner {
            None => (Table(Vec::with_capacity(0)), Vec::with_capacity(0)),
            Some(CompactedNodes { head, size }) => {
                let mut dst = Vec::with_capacity(size);
                dst.push(f(&head.data.borrow()));
                let mut dirty_indices =
                    if head.mark_status.borrow().deref() == &MarkStatus::MarkedDirty {
                        vec![head.index.get() as usize]
                    } else {
                        Vec::new()
                    };
                let mut node = head.next.get().unwrap();
                loop {
                    if std::ptr::eq(node, head) {
                        break;
                    } else {
                        dst.push(f(&node.data.borrow()));
                        if node.mark_status.borrow().deref() == &MarkStatus::MarkedDirty {
                            dirty_indices.push(node.index.get() as usize);
                        }
                        node = node.next.get().unwrap();
                    }
                }
                (Table(dst), dirty_indices)
            }
        }
    }
}

#[derive(serde::Serialize, serde::Deserialize)]
pub struct Table<T>(Vec<T>);

impl<T: std::hash::Hash> std::hash::Hash for Table<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl<T> Table<T> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, T> {
        self.0.iter()
    }

    pub fn get<M>(&self, index: Index<M>) -> &T {
        &self.0[index.index]
    }

    pub fn get_mut<M>(&mut self, index: Index<M>) -> &mut T {
        &mut self.0[index.index]
    }

    pub fn into_vec(self) -> Vec<T> {
        self.0
    }

    // let init = Array.init
    pub fn init(size: usize, f: impl FnMut(usize) -> T) -> Self {
        Table((0..size).map(f).collect())
    }

    // let iteri = Array.iteri
    pub fn iteri(&self, mut f: impl FnMut(usize, &T)) {
        for (i, x) in self.0.iter().enumerate() {
            f(i, x);
        }
    }

    // let map = Array.map
    pub fn map<U>(&self, f: impl FnMut(&T) -> U) -> Table<U> {
        Table(self.0.iter().map(f).collect())
    }

    // let mapi = Array.mapi
    pub fn mapi<U>(&self, mut f: impl FnMut(usize, &T) -> U) -> Table<U> {
        Table(self.0.iter().enumerate().map(|(i, x)| f(i, x)).collect())
    }

    // let to_array_map = map
    pub fn to_array_map<U>(&self, f: impl FnMut(&T) -> U) -> Table<U> {
        self.map(f)
    }
}

impl<T: Debug> Debug for Table<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub type IndexSet<Marker> = std::collections::BTreeSet<Index<Marker>>;

/// Interned builder that deduplicates values using a hash table.
/// When the same value is pushed multiple times, it returns the same node.
pub(super) struct InternedBuilder<'a, T>
where
    T: std::hash::Hash + Eq,
{
    table: std::collections::HashMap<T, Node<'a, T>>,
    builder: Builder<'a, T>,
}

impl<'a, T> InternedBuilder<'a, T>
where
    T: std::hash::Hash + Eq + Clone,
{
    pub(super) fn new(arena: &'a bumpalo::Bump) -> Self {
        Self {
            table: std::collections::HashMap::new(),
            builder: Builder::new(arena),
        }
    }

    pub(super) fn push(&mut self, data: T) -> Node<'a, T> {
        if let Some(node) = self.table.get(&data) {
            node.dupe()
        } else {
            let node = self.builder.push(data.clone());
            self.table.insert(data, node.dupe());
            node
        }
    }

    pub(super) fn compact_without_merge(self) -> Indexed<'a, T> {
        self.builder.compact_without_merge()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::rc::Rc;

    use pretty_assertions::assert_eq;

    use super::*;

    type NodeRef<'a> = Option<Node<'a, TestData<'a>>>;

    struct TestData<'a> {
        label: char,
        refs: Vec<Rc<RefCell<NodeRef<'a>>>>,
    }

    fn mark<'a>(data: &TestData<'a>) -> bool {
        for x in &data.refs {
            if let Some(node) = x.borrow().as_ref() {
                node.mark(mark);
            }
        }
        false
    }

    fn merge<'a>(x0: &TestData<'a>, x1: &TestData<'a>) -> Option<TestData<'a>> {
        if x0.label == x1.label {
            let mut refs = x0.refs.clone();
            refs.extend(x1.refs.iter().cloned());
            Some(TestData {
                label: x0.label,
                refs,
            })
        } else {
            None
        }
    }

    fn compact(data: &TestData) -> (char, Vec<Index<()>>) {
        let refs: Vec<Index<()>> = data
            .refs
            .iter()
            .map(|x| x.borrow().as_ref().unwrap().index_exn())
            .collect();
        (data.label, refs)
    }

    fn print_tbl(table: Table<(char, Vec<Index<()>>)>) -> String {
        let mut result = String::new();
        for (i, (label, refs)) in table.iter().enumerate() {
            let refs_str: Vec<String> = refs.iter().map(|idx| idx.as_usize().to_string()).collect();
            result.push_str(&format!("{}| {} -> {}\n", i, label, refs_str.join(" ")));
        }
        result
    }

    // A -> [D]
    // B -> [A]
    // C -> [A]
    // D -> [B]
    #[test]
    fn cycle() {
        let arena = bumpalo::Bump::new();
        let mut builder = Builder::new(&arena);

        let a_ref = Rc::new(RefCell::new(None));
        let b_ref = Rc::new(RefCell::new(None));
        let d_ref = Rc::new(RefCell::new(None));

        let a = builder.push(TestData {
            label: 'A',
            refs: vec![d_ref.dupe()],
        });
        let b = builder.push(TestData {
            label: 'B',
            refs: vec![a_ref.dupe()],
        });
        let _c = builder.push(TestData {
            label: 'C',
            refs: vec![a_ref.dupe()],
        });
        let d = builder.push(TestData {
            label: 'D',
            refs: vec![b_ref.dupe()],
        });

        a_ref.replace(Some(a.dupe()));
        b_ref.replace(Some(b));
        d_ref.replace(Some(d));

        a.mark(mark);
        let indexed = builder.compact_without_merge();
        let (copy, _) = indexed.copy(compact);

        let output = print_tbl(copy);
        let expected = r#"
0| A -> 2
1| B -> 0
2| D -> 1
"#;
        assert_eq!(output.trim(), expected.trim());
    }

    #[test]
    fn empty() {
        let arena = bumpalo::Bump::new();
        let builder = Builder::new(&arena);
        let indexed = builder.compact_without_merge();
        let (copy, _) = indexed.copy(compact);
        assert_eq!(copy.len(), 0);
    }

    #[test]
    fn singleton_unmarked() {
        let arena = bumpalo::Bump::new();
        let mut builder = Builder::new(&arena);
        let _a = builder.push(TestData {
            label: 'A',
            refs: Vec::new(),
        });
        let indexed = builder.compact_without_merge();
        let (copy, _) = indexed.copy(compact);
        assert_eq!(copy.len(), 0);
    }

    #[test]
    fn singleton_marked() {
        let arena = bumpalo::Bump::new();
        let mut builder = Builder::new(&arena);
        let a = builder.push(TestData {
            label: 'A',
            refs: Vec::new(),
        });
        a.mark(mark);
        let indexed = builder.compact_without_merge();
        let (copy, _) = indexed.copy(compact);

        let output = print_tbl(copy);
        let expected = r#"
0| A -> 
"#;
        assert_eq!(output.trim(), expected.trim());
    }

    #[test]
    fn splice() {
        let arena = bumpalo::Bump::new();
        let mut builder = Builder::new(&arena);

        let a = builder.push(TestData {
            label: 'A',
            refs: Vec::new(),
        });
        let d = builder.push(TestData {
            label: 'D',
            refs: Vec::new(),
        });

        let (b, c) = builder.splice(a.dupe(), |b2| {
            let b = b2.push(TestData {
                label: 'B',
                refs: Vec::new(),
            });
            let c = b2.push(TestData {
                label: 'C',
                refs: Vec::new(),
            });
            (b, c)
        });

        a.mark(mark);
        b.mark(mark);
        c.mark(mark);
        d.mark(mark);

        let indexed = builder.compact_without_merge();
        let (copy, _) = indexed.copy(compact);

        let output = print_tbl(copy);
        let expected = r#"
0| A -> 
1| B -> 
2| C -> 
3| D -> 
"#;
        assert_eq!(output.trim(), expected.trim());
    }

    #[test]
    fn compact_merge() {
        let arena = bumpalo::Bump::new();
        let mut builder = Builder::new(&arena);

        let a_ref = Rc::new(RefCell::new(None));
        let b0_ref = Rc::new(RefCell::new(None));
        let b1_ref = Rc::new(RefCell::new(None));
        let c_ref = Rc::new(RefCell::new(None));

        let a = builder.push(TestData {
            label: 'A',
            refs: vec![b0_ref.dupe()],
        });
        let b0 = builder.push(TestData {
            label: 'B',
            refs: vec![a_ref.dupe()],
        });
        let b1 = builder.push(TestData {
            label: 'B',
            refs: vec![c_ref.dupe()],
        });
        let c = builder.push(TestData {
            label: 'C',
            refs: vec![b1_ref.dupe()],
        });

        a_ref.replace(Some(a.dupe()));
        b0_ref.replace(Some(b0.dupe()));
        b1_ref.replace(Some(b1.dupe()));
        c_ref.replace(Some(c.dupe()));

        a.mark(mark);
        b0.mark(mark);
        b1.mark(mark);
        c.mark(mark);

        let indexed = builder.compact(Some(merge));
        let (copy, _) = indexed.copy(compact);

        let output = print_tbl(copy);
        let expected = r#"
0| A -> 1
1| B -> 0 2
2| C -> 1
"#;
        assert_eq!(output.trim(), expected.trim());
    }
}
