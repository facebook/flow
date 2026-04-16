/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Merge stream - schedules parallel merge work with dependency ordering
//!
//! Custom bucketing scheme for dynamically growing and shrinking workloads when
//! merging files.
//!
//! We start out with files that have no dependencies: these files are available
//! for scheduling merge jobs. All other files are "blocked", i.e., they are *not
//! ready* for scheduling.
//!
//! NOTE: Scheduling merge jobs too early will cause crashes, since they will
//! need stuff that has not been computed yet! A more sophisticated scheme may be
//! designed to be tolerant to such failures, but the merge process is
//! complicated enough as is. Also, performance-wise blocking does not seem to be
//! an issue because files get unblocked pretty regularly (see below).
//!
//! Each blocked file maintains a counter on the number of files blocking
//! them. As files are done, they decrement the counters of other files blocked
//! on them. As soon as some of the counters go to zero, the corresponding files
//! are made available for scheduling.
//!
//! Finally, we maintain a counter on the total number of blocked files. When
//! that goes to zero, we prepare to exit!
//!
//! The underlying worker management scheme needs to know when to wait for more
//! work vs. when it can safely exit. We signal the former by returning a `Wait`
//! bucket, and the latter by returning a `Done` bucket.

use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

use crossbeam::channel;
use dupe::Dupe;
use flow_common_utils::graph::Graph;
use flow_data_structure_wrapper::ord_set::FlowOrdSet;
use flow_parser::file_key::FileKey;
use flow_utils_concurrency::lock::Mutex;
use flow_utils_concurrency::map_reduce::Bucket;
use vec1::Vec1;

pub struct Component(pub Arc<Vec1<FileKey>>);

pub struct MergeResult<A>(pub Vec<(FileKey, bool, A)>);

/// The number of leaders this node is currently blocking on
struct Node {
    component: Arc<Vec1<FileKey>>,
    dependents: Mutex<BTreeMap<FileKey, Arc<Node>>>,
    blocking: AtomicUsize,
    recheck: AtomicBool,
    size: usize,
}

pub struct MergeStream<A> {
    graph: BTreeMap<FileKey, Arc<Node>>,

    ready_tx: channel::Sender<Arc<Node>>,
    ready_rx: channel::Receiver<Arc<Node>>,

    num_workers: usize,
    total_files: usize,

    ready_components: AtomicUsize,
    ready_files: AtomicUsize,
    blocked_components: AtomicUsize,
    blocked_files: AtomicUsize,
    merged_components: AtomicUsize,
    merged_files: AtomicUsize,
    skipped_components: AtomicUsize,
    skipped_files: AtomicUsize,

    new_or_changed_files: Mutex<FlowOrdSet<FileKey>>,

    _phantom: std::marker::PhantomData<A>,
}

impl<A> MergeStream<A> {
    pub fn new(
        num_workers: usize,
        sig_dependency_graph: &Graph<FileKey>,
        components: Vec<Vec1<FileKey>>,
        recheck_set: &FlowOrdSet<FileKey>,
    ) -> Self {
        let (ready_tx, ready_rx) = channel::unbounded();

        // create node for each component
        let mut leaders: BTreeMap<FileKey, FileKey> = BTreeMap::new();
        let mut graph: BTreeMap<FileKey, Arc<Node>> = BTreeMap::new();

        for component in components {
            let leader = component.first().dupe();
            let size = component.len();

            let mut recheck = false;
            for file in component.iter() {
                recheck = recheck || recheck_set.contains(file);
                leaders.insert(file.dupe(), leader.dupe());
            }

            let node = Arc::new(Node {
                component: Arc::new(component),
                dependents: Mutex::new(BTreeMap::new()),
                blocking: AtomicUsize::new(0), // computed later
                recheck: AtomicBool::new(recheck),
                size,
            });

            graph.insert(leader, node);
        }

        // calculate dependents, blocking for each node
        let mut ready_components = 0;
        let mut ready_files = 0;
        let mut blocked_components = 0;
        let mut blocked_files = 0;

        for (leader, node) in &graph {
            let mut blocking = 0;

            for file in node.component.iter() {
                let dep_files = sig_dependency_graph.find(file);

                for dep_file in dep_files {
                    if let Some(dep_leader) = leaders.get(dep_file) {
                        if let Some(dep_node) = graph.get(dep_leader) {
                            if Arc::ptr_eq(dep_node, node) {
                                continue;
                            }

                            let mut dep_dependents = dep_node.dependents.lock();
                            if dep_dependents.insert(leader.dupe(), node.dupe()).is_none() {
                                blocking += 1;
                            }
                        }
                    }
                }
            }

            if blocking == 0 {
                ready_tx.send(node.dupe()).unwrap();
                ready_components += 1;
                ready_files += node.size;
            } else {
                node.blocking.store(blocking, Ordering::Relaxed);
                blocked_components += 1;
                blocked_files += node.size;
            }
        }

        Self {
            graph,
            ready_tx,
            ready_rx,
            num_workers,
            total_files: ready_files + blocked_files,
            ready_components: AtomicUsize::new(ready_components),
            ready_files: AtomicUsize::new(ready_files),
            blocked_components: AtomicUsize::new(blocked_components),
            blocked_files: AtomicUsize::new(blocked_files),
            merged_components: AtomicUsize::new(0),
            merged_files: AtomicUsize::new(0),
            skipped_components: AtomicUsize::new(0),
            skipped_files: AtomicUsize::new(0),
            new_or_changed_files: Mutex::new(FlowOrdSet::new()),
            _phantom: std::marker::PhantomData,
        }
    }

    pub fn update_server_status(&self) {
        flow_server_env::monitor_rpc::status_update(
            flow_server_env::server_status::Event::MergingProgress(
                flow_server_env::server_status::Progress {
                    finished: self.merged_files.load(Ordering::Relaxed) as i32,
                    total: Some(self.total_files as i32),
                },
            ),
        );
    }

    pub fn next(&self) -> Bucket<Component> {
        let n = self.bucket_size();
        let mut batch = Vec::new();
        let mut size_taken = 0;

        while size_taken < n {
            match self.ready_rx.try_recv() {
                Ok(node) => {
                    self.ready_components.fetch_sub(1, Ordering::Relaxed);
                    self.ready_files.fetch_sub(node.size, Ordering::Relaxed);

                    size_taken += node.size;
                    batch.push(Component(node.component.dupe()));
                }
                Err(_) => break,
            }
        }

        if batch.is_empty() {
            if self.is_done() {
                Bucket::Done
            } else {
                Bucket::Wait
            }
        } else {
            Bucket::Job(batch)
        }
    }

    pub fn merge(&self, merged: MergeResult<A>, mut acc: Vec<A>) -> Vec<A> {
        for (leader_f, diff, result) in merged.0 {
            // Record that a component was merged (or skipped) and recursively unblock its
            // dependents. If a dependent has no more unmerged dependencies, make it
            // available for scheduling.
            let node = self.graph.get(&leader_f).expect("leader not in graph");

            self.merged_components.fetch_add(1, Ordering::Relaxed);
            self.merged_files.fetch_add(node.size, Ordering::Relaxed);

            if diff {
                let mut new_or_changed = self.new_or_changed_files.lock();
                for file in node.component.iter() {
                    new_or_changed.insert(file.dupe());
                }
            }

            let dependents = node.dependents.lock();
            for dependent in dependents.values() {
                self.unblock(diff, dependent);
            }

            acc.push(result);
        }

        self.update_server_status();
        acc
    }

    /// NOTE: call these functions only at the end of merge, not during.
    pub fn total_files(&self) -> usize {
        self.total_files
    }

    pub fn skipped_count(&self) -> usize {
        self.skipped_files.load(Ordering::Relaxed)
    }

    pub fn sig_new_or_changed(&self) -> FlowOrdSet<FileKey> {
        self.new_or_changed_files.lock().dupe()
    }

    // hard-coded, as in Bucket
    fn bucket_size(&self) -> usize {
        const MAX_BUCKET_SIZE: usize = 500;

        // NB: num_workers can be zero
        let ready_files = self.ready_files.load(Ordering::Relaxed);
        let max_bucket_size = if ready_files < self.num_workers * MAX_BUCKET_SIZE {
            if self.num_workers == 0 {
                MAX_BUCKET_SIZE
            } else {
                1 + (ready_files / self.num_workers)
            }
        } else {
            MAX_BUCKET_SIZE
        };

        std::cmp::min(max_bucket_size, ready_files)
    }

    fn is_done(&self) -> bool {
        self.blocked_components.load(Ordering::SeqCst) == 0
    }

    fn add_ready(&self, node: &Arc<Node>) {
        self.ready_components.fetch_add(1, Ordering::Relaxed);
        self.ready_files.fetch_add(node.size, Ordering::Relaxed);
        self.ready_tx.send(node.dupe()).unwrap();
    }

    fn unblock(&self, diff: bool, node: &Arc<Node>) {
        // dependent blocked on one less
        let blocking = node.blocking.fetch_sub(1, Ordering::SeqCst);

        // dependent should be rechecked if diff
        if diff {
            node.recheck.store(true, Ordering::Relaxed);
        }

        // no more waiting, yay!
        if blocking == 1 {
            self.blocked_components.fetch_sub(1, Ordering::SeqCst);
            self.blocked_files.fetch_sub(node.size, Ordering::SeqCst);

            if node.recheck.load(Ordering::Relaxed) {
                self.add_ready(node);
            } else {
                self.skip(node);
            }
        }
    }

    fn skip(&self, node: &Arc<Node>) {
        self.skipped_components.fetch_add(1, Ordering::Relaxed);
        self.skipped_files.fetch_add(node.size, Ordering::Relaxed);

        self.merged_components.fetch_add(1, Ordering::Relaxed);
        self.merged_files.fetch_add(node.size, Ordering::Relaxed);

        let dependents = node.dependents.lock();
        for dependent in dependents.values() {
            self.unblock(false, dependent);
        }
    }
}
