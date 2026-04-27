/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A WorkloadStream is a datastructure which keeps track of the workloads (aka commands) that the
//! server has queued up to run. The basic operations are pushing new workloads and popping the
//! oldest workloads.
//!
//! We keep parallelizable workloads and nonparallelizable workloads in separate queues. This allows
//! the caller to ask for the next parallelizable workload or any workload.
//!
//! Parallelizable workloads can also be requeued. Requeueing basically sticks the workload at the
//! front of the queue. We do this when parallelizable workloads are canceled due to a recheck.

use std::collections::VecDeque;
use std::sync::Condvar;
use std::sync::Mutex;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Instant;

use crate::server_env::Env;

pub type WorkloadHandler = Box<dyn FnOnce(Env) -> Env + Send>;

pub struct Workload {
    pub workload_should_be_cancelled: Box<dyn Fn() -> bool + Send>,
    pub workload_handler: WorkloadHandler,
}

pub type ParallelizableWorkloadHandler = Box<dyn FnOnce(&Env) + Send>;

pub struct ParallelizableWorkload {
    pub parallelizable_workload_should_be_cancelled: Box<dyn Fn() -> bool + Send>,
    pub parallelizable_workload_handler: ParallelizableWorkloadHandler,
}

struct Item<T> {
    enqueue_time: Instant,
    name: String,
    workload: T,
}

/// INVARIANT: requeued_parallelizable_length = List.length requeued_parallelizable
struct Inner {
    parallelizable: VecDeque<Item<ParallelizableWorkload>>,
    requeued_parallelizable: Vec<Item<ParallelizableWorkload>>,
    nonparallelizable: VecDeque<Item<Workload>>,
}

pub struct WorkloadStream {
    inner: Mutex<Inner>,
    signal: Condvar,
}

impl WorkloadStream {
    pub fn create() -> Self {
        WorkloadStream {
            inner: Mutex::new(Inner {
                parallelizable: VecDeque::new(),
                requeued_parallelizable: Vec::new(),
                nonparallelizable: VecDeque::new(),
            }),
            signal: Condvar::new(),
        }
    }

    fn summarize_length(inner: &Inner) -> String {
        let p = inner.parallelizable.len();
        let r = inner.requeued_parallelizable.len();
        let n = inner.nonparallelizable.len();
        format!("{}P+{}R+{}N={}", p, r, n, p + r + n)
    }

    // Add a non-parallelizable workload to the stream and wake up anyone waiting
    pub fn push(&self, name: &str, workload: Workload) {
        let mut inner = self.inner.lock().unwrap();
        eprintln!(
            "Enqueueing nonparallelizable {} behind {}",
            name,
            Self::summarize_length(&inner)
        );
        let enqueue_time = Instant::now();
        inner.nonparallelizable.push_back(Item {
            enqueue_time,
            name: name.to_string(),
            workload,
        });
        self.signal.notify_all();
    }

    // Add a parallelizable workload to the stream and wake up anyone waiting
    pub fn push_parallelizable(&self, name: &str, workload: ParallelizableWorkload) {
        let mut inner = self.inner.lock().unwrap();
        eprintln!(
            "Enqueueing parallelizable {} behind {}",
            name,
            Self::summarize_length(&inner)
        );
        let enqueue_time = Instant::now();
        inner.parallelizable.push_back(Item {
            enqueue_time,
            name: name.to_string(),
            workload,
        });
        self.signal.notify_all();
    }

    // Add a parallelizable workload to the front of the stream and wake up anyone waiting
    pub fn requeue_parallelizable(&self, name: &str, workload: ParallelizableWorkload) {
        let mut inner = self.inner.lock().unwrap();
        eprintln!(
            "Requeueing {} behind {}",
            name,
            Self::summarize_length(&inner)
        );
        let enqueue_time = Instant::now();
        inner.requeued_parallelizable.push(Item {
            enqueue_time,
            name: name.to_string(),
            workload,
        });
        self.signal.notify_all();
    }

    // Cast a parallelizable workload to a nonparallelizable workload.
    fn workload_of_parallelizable_workload(pw: ParallelizableWorkload) -> Workload {
        let handler = pw.parallelizable_workload_handler;
        Workload {
            workload_should_be_cancelled: pw.parallelizable_workload_should_be_cancelled,
            workload_handler: Box::new(move |env: Env| {
                handler(&env);
                env
            }),
        }
    }

    // Pop the oldest workload
    pub fn pop(&self) -> Option<WorkloadHandler> {
        let mut inner = self.inner.lock().unwrap();
        // Always prefer requeued parallelizable jobs
        if let Some(item) = inner.requeued_parallelizable.pop() {
            eprintln!(
                "Dequeueing requeued {} after {:.3} seconds",
                item.name,
                item.enqueue_time.elapsed().as_secs_f64()
            );
            let w = Self::workload_of_parallelizable_workload(item.workload);
            return Some(w.workload_handler);
        }
        // Pop from the parallelizable queue unless the nonparallelizable queue has an older entry
        let p_time = inner.parallelizable.front().map(|i| i.enqueue_time);
        let n_time = inner.nonparallelizable.front().map(|i| i.enqueue_time);
        let use_parallelizable = match (p_time, n_time) {
            (None, None) | (Some(_), None) => true,
            (Some(p), Some(n)) => p <= n,
            (None, Some(_)) => false,
        };
        if use_parallelizable {
            inner.parallelizable.pop_front().map(|item| {
                eprintln!(
                    "Dequeueing parallelizable {} after {:.3} seconds",
                    item.name,
                    item.enqueue_time.elapsed().as_secs_f64()
                );
                let w = Self::workload_of_parallelizable_workload(item.workload);
                w.workload_handler
            })
        } else {
            inner.nonparallelizable.pop_front().map(|item| {
                eprintln!(
                    "Dequeueing nonparallelizable {} after {:.3} seconds",
                    item.name,
                    item.enqueue_time.elapsed().as_secs_f64()
                );
                item.workload.workload_handler
            })
        }
    }

    // Pop the oldest parallelizable workload
    pub fn pop_parallelizable(&self) -> Option<ParallelizableWorkload> {
        let mut inner = self.inner.lock().unwrap();
        // Always prefer requeued parallelizable jobs
        if let Some(item) = inner.requeued_parallelizable.pop() {
            eprintln!(
                "Dequeueing requeued {} after {:.3} seconds",
                item.name,
                item.enqueue_time.elapsed().as_secs_f64()
            );
            return Some(item.workload);
        }
        inner.parallelizable.pop_front().map(|item| {
            eprintln!(
                "Dequeueing parallelizable {} after {:.3} seconds",
                item.name,
                item.enqueue_time.elapsed().as_secs_f64()
            );
            item.workload
        })
    }

    // Wait until there's a workload in the stream
    pub fn wait_for_workload(&self) {
        let mut inner = self.inner.lock().unwrap();
        while inner.requeued_parallelizable.is_empty()
            && inner.parallelizable.is_empty()
            && inner.nonparallelizable.is_empty()
        {
            inner = self.signal.wait(inner).unwrap();
        }
    }

    // Wait until there's a parallelizable workload in the stream
    pub fn wait_for_parallelizable_workload(&self) {
        let mut inner = self.inner.lock().unwrap();
        while inner.requeued_parallelizable.is_empty() && inner.parallelizable.is_empty() {
            inner = self.signal.wait(inner).unwrap();
        }
    }

    // Like `wait_for_parallelizable_workload`, but also returns when `stop` becomes true. Used to
    // implement the cancellation half of the OCaml `Lwt.pick [wait_for_parallelizable_workload();
    // wait_for_cancel]` in `Parallelizable_workload_loop`.
    pub fn wait_for_parallelizable_workload_or_stop(&self, stop: &AtomicBool) {
        let mut inner = self.inner.lock().unwrap();
        while !stop.load(Ordering::Acquire)
            && inner.requeued_parallelizable.is_empty()
            && inner.parallelizable.is_empty()
        {
            inner = self.signal.wait(inner).unwrap();
        }
    }

    // Wake up anyone blocked in `wait_for_parallelizable_workload` /
    // `wait_for_parallelizable_workload_or_stop`. Callers that flip a stop flag should call this
    // afterwards to ensure the waiter observes the change.
    pub fn wake_waiters(&self) {
        self.signal.notify_all();
    }
}
