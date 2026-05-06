/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Map-reduce utilities for parallel processing.
//!
//! Provides a flexible map-reduce framework inspired by OCaml's MultiWorkerLwt.fold,
//! with support for batching to reduce lock contention and streaming work distribution.

use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Instant;

use crossbeam::channel;
use dupe::Dupe;
use flow_common_utils::measure;
#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "openbsd"))]
use nix::sys::resource;
#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "openbsd"))]
use nix::sys::time::TimeValLike;

use crate::lock::Condvar;
use crate::lock::Mutex;
use crate::thread_pool::ThreadPool;

/// Work bucket returned by a `Next` function.
///
/// Similar to OCaml's `Bucket.bucket`:
/// ```ocaml
/// type 'a bucket = Job of 'a | Wait | Done
/// ```
#[derive(Debug)]
pub enum Bucket<W> {
    /// A batch of work items to process.
    Job(Vec<W>),
    /// Wait for more work to become available.
    /// Workers will retry after other workers complete their jobs.
    Wait,
    /// No more work available.
    Done,
}

/// A function that produces work batches on-demand.
///
/// Similar to OCaml's `'a next = unit -> 'a bucket`.
///
/// This trait allows lazy/streaming work generation:
/// - Work items can be generated on-demand
/// - Can wait for external events (e.g., file discovery)
/// - Lower memory overhead (don't need all work upfront)
///
/// # Example
///
/// ```no_run
/// use std::sync::Arc;
///
/// use flow_utils_concurrency::lock::Mutex;
/// use flow_utils_concurrency::map_reduce::Bucket;
/// use flow_utils_concurrency::map_reduce::Next;
///
/// struct FileScanner {
///     files: Arc<Vec<String>>,
///     index: Arc<Mutex<usize>>,
/// }
///
/// impl FileScanner {
///     fn next_batch(&self) -> Bucket<String> {
///         let mut index = self.index.lock();
///
///         if *index >= self.files.len() {
///             return Bucket::Done;
///         }
///
///         let batch_size = 10;
///         let end = (*index + batch_size).min(self.files.len());
///         let batch = self.files[*index..end].to_vec();
///         *index = end;
///
///         Bucket::Job(batch)
///     }
/// }
/// ```
pub trait Next<W>: Send + Sync {
    /// Get the next batch of work.
    ///
    /// Returns:
    /// - `Bucket::Job(items)` - A batch of work items
    /// - `Bucket::Wait` - No work available now, but more may come later
    /// - `Bucket::Done` - No more work will ever be available
    fn next(&self) -> Bucket<W>;
}

#[cfg(any(target_os = "linux", target_os = "freebsd", target_os = "openbsd"))]
fn thread_times() -> (f64, f64) {
    match resource::getrusage(resource::UsageWho::RUSAGE_THREAD) {
        Ok(usage) => (
            usage.user_time().num_microseconds() as f64 / 1_000_000.0,
            usage.system_time().num_microseconds() as f64 / 1_000_000.0,
        ),
        Err(_) => (0.0, 0.0),
    }
}

#[cfg(not(any(target_os = "linux", target_os = "freebsd", target_os = "openbsd")))]
fn thread_times() -> (f64, f64) {
    (0.0, 0.0)
}

fn sample_worker_run<T>(f: impl FnOnce() -> T) -> T {
    if !measure::is_enabled() {
        return f();
    }
    let wall_start = Instant::now();
    let (user_start, system_start) = thread_times();
    let ret = f();
    let (user_end, system_end) = thread_times();
    measure::sample(
        None,
        None,
        "worker_user_time",
        (user_end - user_start).max(0.0),
    );
    measure::sample(
        None,
        None,
        "worker_system_time",
        (system_end - system_start).max(0.0),
    );
    measure::sample(
        None,
        None,
        "worker_wall_time",
        wall_start.elapsed().as_secs_f64(),
    );
    ret
}

fn sample_worker_read_request<T>(f: impl FnOnce() -> T) -> T {
    if !measure::is_enabled() {
        return f();
    }
    let start = Instant::now();
    let ret = f();
    measure::sample(
        None,
        None,
        "worker_read_request",
        start.elapsed().as_secs_f64(),
    );
    ret
}

fn sample_worker_send_response<T>(f: impl FnOnce() -> T) -> T {
    if !measure::is_enabled() {
        return f();
    }
    let start = Instant::now();
    let ret = f();
    measure::sample(
        None,
        None,
        "worker_send_response",
        start.elapsed().as_secs_f64(),
    );
    ret
}

fn sample_worker_idle<T>(f: impl FnOnce() -> T) -> T {
    if !measure::is_enabled() {
        return f();
    }
    let start = Instant::now();
    let ret = f();
    measure::sample(None, None, "worker_idle", start.elapsed().as_secs_f64());
    ret
}

fn sample_worker_done(done_start_times: &Arc<Mutex<Vec<Instant>>>) {
    // OCaml:
    // let%lwt idle_start_times = LwtUtils.all worker_threads in
    // let idle_end_wall_time = Unix.gettimeofday () in
    // List.iter
    //   (fun idle_start_wall_time ->
    //     Measure.sample "worker_done" (idle_end_wall_time -. idle_start_wall_time))
    //   idle_start_times;
    if !measure::is_enabled() {
        return;
    }
    let idle_end_wall_time = Instant::now();
    let done_start_times = done_start_times.lock();
    for done_start_time in done_start_times.iter() {
        measure::sample(
            None,
            None,
            "worker_done",
            idle_end_wall_time
                .duration_since(*done_start_time)
                .as_secs_f64(),
        );
    }
}

/// Parallel map-reduce over a collection of work items.
///
/// # Type Parameters
///
/// - `W`: Work item type
/// - `A`: Accumulator type
/// - `F`: Job function type `(&mut accumulator, &work_item)`
/// - `M`: Merge function type `(&mut accumulator, accumulator)`
/// - `N`: Neutral producer function type `() -> A`
///
/// # Arguments
///
/// - `pool`: Thread pool to use for parallel execution
/// - `work_items`: Collection of items to process
/// - `job`: Function to process each work item, mutating the accumulator
/// - `neutral`: Producer function that creates fresh accumulator values
/// - `merge`: Function to merge second accumulator into first by mutation (consumes second)
///
/// # Requirements
///
/// The `merge` function must be **associative**:
/// ```text
/// merge(&mut a, merge(&mut neutral(), c)) is equivalent to merge(&mut merge(&mut a, b), c)
/// ```
///
/// # Example
///
/// ```rust
/// use flow_utils_concurrency::map_reduce::fold;
/// use flow_utils_concurrency::thread_pool::ThreadPool;
///
/// let pool = ThreadPool::new();
/// let numbers = vec![1, 2, 3, 4, 5];
///
/// // Sum all numbers
/// let sum = fold(
///     &pool,
///     numbers,
///     |acc: &mut i32, &n| *acc += n, // job: mutate accumulator
///     |a, b| *a += b,                // merge: add b into a
/// );
///
/// assert_eq!(sum, 15);
/// ```
pub fn fold<W, A, F, M>(pool: &ThreadPool, work_items: Vec<W>, job: F, merge: M) -> A
where
    W: Send,
    A: Send + Sync + Default + std::fmt::Debug,
    F: Fn(&mut A, &W) + Send + Sync,
    M: Fn(&mut A, A) + Send + Sync,
{
    if work_items.is_empty() {
        return Default::default();
    }

    let (sender, receiver) = channel::unbounded::<W>();
    let receiver = Arc::new(receiver);
    let results_mutex = Arc::new(Mutex::new(Default::default()));
    let job = Arc::new(job);
    let merge = Arc::new(merge);
    let done_start_times = Arc::new(Mutex::new(Vec::new()));

    for item in work_items {
        sender.send(item).unwrap();
    }
    drop(sender);

    // Spawn worker threads
    pool.spawn_many(|| {
        let receiver = receiver.dupe();
        let results_mutex = results_mutex.dupe();
        let job = job.dupe();
        let merge = merge.dupe();
        let done_start_times = done_start_times.dupe();
        let mut local_acc = Default::default();
        while let Ok(work_item) = sample_worker_read_request(|| receiver.recv()) {
            sample_worker_run(|| {
                job(&mut local_acc, &work_item);
            });
            sample_worker_send_response(|| {
                let mut results = results_mutex.lock();
                let acc_to_merge = std::mem::take(&mut local_acc);
                merge(&mut *results, acc_to_merge);
                drop(results);
            });
        }
        if measure::is_enabled() {
            done_start_times.lock().push(Instant::now());
        }
    });
    sample_worker_done(&done_start_times);

    drop(receiver);
    Arc::try_unwrap(results_mutex)
        .expect("All workers should be done")
        .into_inner()
}

/// Implement `next` for any closure that returns `Bucket<W>`.
impl<W, F> Next<W> for F
where
    F: Fn() -> Bucket<W> + Send + Sync,
{
    fn next(&self) -> Bucket<W> {
        self()
    }
}

/// Streaming map-reduce using a `Next` function for on-demand work generation.
///
/// This function supports **streaming work distribution**:
/// - Work batches are generated on-demand via the `next` function
/// - Lower memory overhead (don't need all work items upfront)
/// - Can wait for external events (file discovery, network responses, etc.)
///
/// # Type Parameters
///
/// - `W`: Work item type
/// - `A`: Accumulator type
/// - `J`: Job function that processes a batch of work items
/// - `M`: Merge function that combines accumulators
/// - `N`: Next function that produces work batches
///
/// # Arguments
///
/// - `pool`: Thread pool for parallel execution
/// - `next`: Function that produces work batches on-demand
/// - `job`: Function `(&mut accumulator, batch)` that mutates accumulator with batch
/// - `neutral`: Producer function `() -> A` that creates initial accumulator values
/// - `merge`: Function `(&mut accumulator, accumulator)` that merges second into first by mutation
///
/// # Example
///
/// ```rust
/// use std::sync::Arc;
///
/// use flow_utils_concurrency::lock::Mutex;
/// use flow_utils_concurrency::map_reduce::Bucket;
/// use flow_utils_concurrency::map_reduce::call;
/// use flow_utils_concurrency::thread_pool::ThreadPool;
///
/// let pool = ThreadPool::new();
/// let files = Arc::new(vec!["a.js", "b.js", "c.js"]);
/// let index = Arc::new(Mutex::new(0));
///
/// // Create a next function that produces batches of 2 files
/// let next = {
///     let files = files.clone();
///     let index = index.clone();
///     move || {
///         let mut i = index.lock();
///         if *i >= files.len() {
///             return Bucket::Done;
///         }
///         let end = (*i + 2).min(files.len());
///         let batch = files[*i..end].iter().map(|s| s.to_string()).collect();
///         *i = end;
///         Bucket::Job(batch)
///     }
/// };
///
/// let total = call(
///     &pool,
///     next,
///     |count: &mut usize, batch| *count += batch.len(), // Mutate count with batch size
///     |a, b| *a += b,                                   // Merge b into a
/// );
///
/// assert_eq!(total, 3);
/// ```
pub fn call<W, A, J, M, N>(pool: &ThreadPool, next: N, job: J, merge: M) -> A
where
    W: Send + 'static,
    A: Send + Sync + Default + std::fmt::Debug,
    J: Fn(&mut A, Vec<W>) + Send + Sync,
    M: Fn(&mut A, A) + Send + Sync,
    N: Next<W> + 'static,
{
    let (sender, receiver) = channel::unbounded::<Vec<W>>();
    let receiver = Arc::new(receiver);
    let results_mutex = Arc::new(Mutex::new(Default::default()));
    let job = Arc::new(job);
    let merge = Arc::new(merge);
    let next = Arc::new(next);
    let done = Arc::new(AtomicBool::new(false));
    let wait_signal = Arc::new(Condvar::new());
    let wait_mutex = Arc::new(Mutex::new(()));
    let done_start_times = Arc::new(Mutex::new(Vec::new()));

    // Producer thread: calls next() and sends batches to workers
    let done_producer = done.dupe();
    let next_producer = next.dupe();
    let sender_clone = sender.clone();
    let wait_signal_producer = wait_signal.dupe();
    let wait_mutex_producer = wait_mutex.dupe();
    std::thread::spawn(move || {
        loop {
            match next_producer.next() {
                Bucket::Job(batch) => {
                    if sender_clone.send(batch).is_err() {
                        break;
                    }
                    // Wake up workers waiting for new work
                    wait_signal_producer.notify_all();
                }
                Bucket::Wait => {
                    // Wait for workers to complete, which may generate new work
                    let guard = wait_mutex_producer.lock();
                    let _ = wait_signal_producer
                        .wait_timeout(guard, std::time::Duration::from_millis(10));
                }
                Bucket::Done => {
                    done_producer.store(true, Ordering::Release);
                    // Wake all workers so they can exit
                    wait_signal_producer.notify_all();
                    break;
                }
            }
        }
    });

    // Spawn worker threads
    pool.spawn_many(|| {
        let receiver = receiver.dupe();
        let results_mutex = results_mutex.dupe();
        let job = job.dupe();
        let merge = merge.dupe();
        let done = done.dupe();
        let wait_signal = wait_signal.dupe();
        let wait_mutex = wait_mutex.dupe();
        let done_start_times = done_start_times.dupe();

        // Each worker has a local accumulator (created once per worker thread)
        let mut local_acc = Default::default();

        loop {
            match sample_worker_read_request(|| receiver.try_recv()) {
                Ok(batch) => {
                    sample_worker_run(|| {
                        job(&mut local_acc, batch);
                    });

                    // Merge local accumulator into global, then reset local
                    sample_worker_send_response(|| {
                        let mut results = results_mutex.lock();
                        let acc_to_merge = std::mem::take(&mut local_acc);
                        merge(&mut *results, acc_to_merge);
                        drop(results);
                    });

                    // Notify producer in case it's waiting
                    wait_signal.notify_all();
                }
                Err(channel::TryRecvError::Empty) => {
                    if done.load(Ordering::Acquire) {
                        // Producer is done and channel is empty
                        break;
                    }
                    // Wait for producer to signal new work or completion
                    let guard = wait_mutex.lock();
                    sample_worker_idle(|| {
                        drop(wait_signal.wait_timeout(guard, std::time::Duration::from_millis(10)));
                    });
                }
                Err(channel::TryRecvError::Disconnected) => {
                    break;
                }
            }
        }
        if measure::is_enabled() {
            done_start_times.lock().push(Instant::now());
        }
    });
    sample_worker_done(&done_start_times);

    drop(sender);
    drop(receiver);

    Arc::try_unwrap(results_mutex)
        .expect("All workers should be done")
        .into_inner()
}

/// Like `call()`, but with work stealing between batches.
///
/// When a worker's channel is empty (no batches available), instead of
/// just waiting, it calls `steal(&mut acc)`. If steal returns true
/// (it did useful work), the worker loops back immediately. If false,
/// the worker waits for new batches as usual.
///
/// This enables intra-batch work stealing: the `job` closure can push
/// batch items onto a shared stealable structure (e.g., crossbeam deque),
/// and the `steal` closure can pop from other workers' structures.
pub fn call_with_stealing<W, A, J, M, N, S>(
    pool: &ThreadPool,
    next: N,
    job: J,
    merge: M,
    steal: S,
) -> A
where
    W: Send + 'static,
    A: Send + Sync + Default + std::fmt::Debug,
    J: Fn(&mut A, Vec<W>) + Send + Sync,
    M: Fn(&mut A, A) + Send + Sync,
    N: Next<W> + 'static,
    S: Fn(&mut A) -> bool + Send + Sync,
{
    let (sender, receiver) = channel::unbounded::<Vec<W>>();
    let receiver = Arc::new(receiver);
    let results_mutex = Arc::new(Mutex::new(Default::default()));
    let job = Arc::new(job);
    let merge = Arc::new(merge);
    let next = Arc::new(next);
    let steal = Arc::new(steal);
    let done = Arc::new(AtomicBool::new(false));
    let wait_signal = Arc::new(Condvar::new());
    let wait_mutex = Arc::new(Mutex::new(()));
    let done_start_times = Arc::new(Mutex::new(Vec::new()));

    // Producer thread: calls next() and sends batches to workers
    let done_producer = done.dupe();
    let next_producer = next.dupe();
    let sender_clone = sender.clone();
    let wait_signal_producer = wait_signal.dupe();
    let wait_mutex_producer = wait_mutex.dupe();
    std::thread::spawn(move || {
        loop {
            match next_producer.next() {
                Bucket::Job(batch) => {
                    if sender_clone.send(batch).is_err() {
                        break;
                    }
                    // Wake up workers waiting for new work
                    wait_signal_producer.notify_all();
                }
                Bucket::Wait => {
                    // Wait for workers to complete, which may generate new work
                    let guard = wait_mutex_producer.lock();
                    let _ = wait_signal_producer
                        .wait_timeout(guard, std::time::Duration::from_millis(10));
                }
                Bucket::Done => {
                    done_producer.store(true, Ordering::Release);
                    // Wake all workers so they can exit
                    wait_signal_producer.notify_all();
                    break;
                }
            }
        }
    });

    // Spawn worker threads
    pool.spawn_many(|| {
        let receiver = receiver.dupe();
        let results_mutex = results_mutex.dupe();
        let job = job.dupe();
        let merge = merge.dupe();
        let steal = steal.dupe();
        let done = done.dupe();
        let wait_signal = wait_signal.dupe();
        let wait_mutex = wait_mutex.dupe();
        let done_start_times = done_start_times.dupe();

        // Each worker has a local accumulator (created once per worker thread)
        let mut local_acc = Default::default();

        loop {
            match sample_worker_read_request(|| receiver.try_recv()) {
                Ok(batch) => {
                    sample_worker_run(|| {
                        job(&mut local_acc, batch);
                    });

                    // Merge local accumulator into global, then reset local
                    sample_worker_send_response(|| {
                        let mut results = results_mutex.lock();
                        let acc_to_merge = std::mem::take(&mut local_acc);
                        merge(&mut *results, acc_to_merge);
                        drop(results);
                    });

                    // Notify producer in case it's waiting
                    wait_signal.notify_all();
                }
                Err(channel::TryRecvError::Empty) => {
                    // Try stealing before waiting
                    if sample_worker_run(|| steal(&mut local_acc)) {
                        // Did useful work — merge and loop back
                        sample_worker_send_response(|| {
                            let mut results = results_mutex.lock();
                            let acc_to_merge = std::mem::take(&mut local_acc);
                            merge(&mut *results, acc_to_merge);
                            drop(results);
                        });

                        wait_signal.notify_all();
                        continue;
                    }
                    if done.load(Ordering::Acquire) {
                        // Producer is done and channel is empty
                        break;
                    }
                    // Wait for producer to signal new work or completion
                    let guard = wait_mutex.lock();
                    sample_worker_idle(|| {
                        drop(wait_signal.wait_timeout(guard, std::time::Duration::from_millis(10)));
                    });
                }
                Err(channel::TryRecvError::Disconnected) => {
                    break;
                }
            }
        }
        if measure::is_enabled() {
            done_start_times.lock().push(Instant::now());
        }
    });
    sample_worker_done(&done_start_times);

    drop(sender);
    drop(receiver);

    Arc::try_unwrap(results_mutex)
        .expect("All workers should be done")
        .into_inner()
}

// ============================================================================
// Helper functions to create Next implementations
// ============================================================================

/// Create a `Next` function from a list of work items.
///
/// Similar to OCaml's `Bucket.make`:
/// ```ocaml
/// val make :
///   num_workers:int ->
///   ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
///   ?max_size:int ->
///   'a list ->
///   'a list next
/// ```
///
/// This creates batches of work items with optimal batch size for the given
/// number of workers.
///
/// # Arguments
///
/// - `num_workers`: Number of workers (threads) available
/// - `progress_fn`: Optional callback `(total, completed, batch_size)` invoked per batch
/// - `max_size`: Maximum batch size (optional, defaults to 500)
/// - `work_items`: All work items to process
///
/// # Example
///
/// ```rust
/// use flow_utils_concurrency::map_reduce::make_next;
/// use flow_utils_concurrency::thread_pool::ThreadPool;
///
/// let pool = ThreadPool::new();
/// let files = vec!["a.js", "b.js", "c.js", "d.js"];
/// let num_workers = 4;
///
/// let next = make_next(num_workers, None::<fn(i32, i32, i32)>, Some(2), files);
///
/// // Will create batches of size 2: ["a.js", "b.js"], ["c.js", "d.js"]
/// ```
pub fn make_next<W: Clone + Send + Sync + 'static>(
    num_workers: usize,
    progress_fn: Option<impl Fn(i32, i32, i32) + Send + Sync + 'static>,
    max_size: Option<usize>,
    work_items: Vec<W>,
) -> impl Next<W> {
    let max_size = max_size.unwrap_or(500);
    let num_jobs = work_items.len();

    // Calculate optimal bucket size (same logic as OCaml's Bucket.calculate_bucket_size)
    let bucket_size = if num_jobs < num_workers * max_size {
        1.max((num_jobs / num_workers) + 1)
    } else {
        max_size
    };

    let items = Arc::new(work_items);
    let index = Arc::new(Mutex::new(0));
    let total = num_jobs as i32;

    move || {
        let mut idx = index.lock();

        if *idx >= items.len() {
            return Bucket::Done;
        }

        let end = (*idx + bucket_size).min(items.len());
        let length = (end - *idx) as i32;
        if let Some(ref progress_fn) = progress_fn {
            progress_fn(total, *idx as i32, length);
        }
        let batch = items[*idx..end].to_vec();
        *idx = end;

        Bucket::Job(batch)
    }
}

/// Parallel iteration over work items using a `Next` function.
///
/// Similar to OCaml's `MultiWorkerLwt.iter`:
/// ```ocaml
/// val iter : worker list option -> job:('a -> unit) -> next:'a list Hh_bucket.next -> unit Lwt.t
/// ```
///
/// This function processes batches of work items in parallel without accumulating results.
/// It's useful when you want side effects (like writing to shared memory) rather than collecting values.
///
/// # Type Parameters
///
/// - `W`: Work item type
/// - `J`: Job function that processes a batch of work items
/// - `N`: Next function that produces work batches
///
/// # Arguments
///
/// - `pool`: Thread pool for parallel execution
/// - `next`: Function that produces work batches on-demand
/// - `job`: Function `(batch)` that processes a batch of work items (for side effects)
///
/// # Example
///
/// ```rust
/// use std::sync::Arc;
///
/// use flow_utils_concurrency::lock::Mutex;
/// use flow_utils_concurrency::map_reduce::Bucket;
/// use flow_utils_concurrency::map_reduce::iter;
/// use flow_utils_concurrency::map_reduce::make_next;
/// use flow_utils_concurrency::thread_pool::ThreadPool;
///
/// let pool = ThreadPool::new();
/// let files = vec!["a.js", "b.js", "c.js", "d.js"];
/// let processed = Arc::new(Mutex::new(Vec::new()));
///
/// let next = make_next(
///     pool.num_workers(),
///     None::<fn(i32, i32, i32)>,
///     Some(2),
///     files,
/// );
///
/// let processed_clone = processed.clone();
/// iter(&pool, next, move |batch| {
///     let mut p = processed_clone.lock();
///     p.extend(batch);
/// });
///
/// assert_eq!(processed.lock().len(), 4);
/// ```
pub fn iter<W, J, N>(pool: &ThreadPool, next: N, job: J)
where
    W: Send + 'static,
    J: Fn(Vec<W>) + Send + Sync,
    N: Next<W> + 'static,
{
    // Use call with unit accumulator - just execute job for side effects
    call(
        pool,
        next,
        |_acc: &mut (), batch| {
            job(batch);
        },
        |_a: &mut (), _b: ()| {
            // No merge needed for unit
        },
    );
}
