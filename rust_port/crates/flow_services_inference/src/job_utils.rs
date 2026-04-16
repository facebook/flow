/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::time::Instant;

use dupe::Dupe;
use flow_common::options::Options;
use flow_parser::file_key::FileKey;
use flow_utils_concurrency::lock::Mutex;
use flow_utils_concurrency::map_reduce::Bucket;
use flow_utils_concurrency::map_reduce::Next;

fn out_of_time(options: &Options, start_time: Instant) -> bool {
    start_time.elapsed().as_secs_f64() > options.max_seconds_for_check_per_worker
}

// Check as many files as it can before it hits the timeout. The timeout is soft,
// so the file which exceeds the timeout won't be canceled. We expect most buckets
// to not hit the timeout
fn job_helper<A, B, C>(
    check: &mut dyn FnMut(FileKey) -> Result<Option<(A, B)>, C>,
    options: &Options,
    start_time: Instant,
    mut acc: Vec<(FileKey, Result<Option<B>, C>)>,
    files: &[FileKey],
) -> (Vec<(FileKey, Result<Option<B>, C>)>, Vec<FileKey>) {
    let mut iter = files.iter();
    loop {
        if out_of_time(options, start_time) {
            eprintln!(
                "Bucket timed out! {} files finished, {} files unfinished",
                acc.len(),
                iter.len()
            );
            return (acc, iter.map(|f| f.dupe()).collect());
        }
        let Some(file) = iter.next() else {
            return (acc, vec![]);
        };
        let result = match check(file.dupe()) {
            Ok(Some((_, acc))) => Ok(Some(acc)),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        };
        acc.push((file.dupe(), result));
    }
}

pub fn mk_job<A, B, C>(
    check: &mut dyn FnMut(FileKey) -> Result<Option<(A, B)>, C>,
    options: &Options,
    files: Vec<FileKey>,
) -> (Vec<(FileKey, Result<Option<B>, C>)>, Vec<FileKey>) {
    let start_time = Instant::now();
    job_helper(check, options, start_time, vec![], &files)
}

/// Like `job_helper`, but pops from a work-stealing deque instead of
/// iterating a slice. On timeout, remaining files are left on the deque
/// so idle workers can steal them immediately rather than re-queuing
/// through the producer.
fn job_helper_stealing<A, B, C>(
    check: &mut dyn FnMut(FileKey) -> Result<Option<(A, B)>, C>,
    options: &Options,
    start_time: Instant,
    mut acc: Vec<(FileKey, Result<Option<B>, C>)>,
    deque: &crossbeam::deque::Worker<FileKey>,
) -> Vec<(FileKey, Result<Option<B>, C>)> {
    loop {
        if out_of_time(options, start_time) {
            // Leave remaining files on the deque for idle workers to steal.
            // Files already stolen by others are gone — not double-counted.
            return acc;
        }
        let Some(file) = deque.pop() else {
            // Deque empty — either all processed locally or some were stolen
            return acc;
        };
        let result = match check(file.dupe()) {
            Ok(Some((_, acc))) => Ok(Some(acc)),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        };
        acc.push((file.dupe(), result));
    }
}

/// Like `mk_job`, but uses a work-stealing deque. The caller pushes
/// files onto the deque before calling this. Other workers can steal
/// from the deque via its Stealer handle while this worker processes.
///
/// On timeout, remaining files stay on the deque rather than being
/// drained and returned. This allows idle workers to steal them
/// immediately via the work-stealing mechanism.
pub fn mk_job_stealing<A, B, C>(
    check: &mut dyn FnMut(FileKey) -> Result<Option<(A, B)>, C>,
    options: &Options,
    deque: &crossbeam::deque::Worker<FileKey>,
) -> Vec<(FileKey, Result<Option<B>, C>)> {
    let start_time = Instant::now();
    job_helper_stealing(check, options, start_time, vec![], deque)
}

/// A stateful (next, merge) pair plus a shared files-completed counter.
///
/// In the OCaml version, next() and merge() run on the same cooperative
/// Lwt event loop, so merge always re-queues unfinished files before
/// next() is called again. In our multi-threaded Rust version, the
/// producer thread calls next() independently from workers, so it can
/// see an empty queue while workers still have files on their
/// work-stealing deques.
///
/// Instead of re-queuing timed-out files through the producer (which
/// adds latency), we leave them on the work-stealing deques and track
/// completion via a shared atomic counter. The `next` function returns
/// `Wait` (not `Done`) when the queue is empty but not all files have
/// been completed, allowing idle workers to steal remaining files
/// directly from busy workers' deques.
///
/// The returned `files_completed` counter must also be incremented by
/// the steal closure (one per stolen file) so that termination is
/// correctly detected.
pub fn mk_next<R>(
    intermediate_result_callback: Arc<dyn Fn(&[R]) + Send + Sync>,
    _quiet: bool,
    max_size: usize,
    num_workers: usize,
    files: Vec<FileKey>,
) -> (
    impl Next<FileKey>,
    impl Fn(&mut Vec<R>, Vec<R>) + Send + Sync,
    Arc<AtomicUsize>,
)
where
    R: 'static,
{
    let total_count = files.len();
    let todo = Arc::new(Mutex::new((files, total_count)));
    let files_completed = Arc::new(AtomicUsize::new(0));
    let num_workers = num_workers.max(1);

    let files_completed_for_status = files_completed.dupe();
    let status_update = move || {
        let finished = files_completed_for_status.load(Ordering::Acquire);
        flow_server_env::monitor_rpc::status_update(
            flow_server_env::server_status::Event::CheckingProgress(
                flow_server_env::server_status::Progress {
                    total: Some(total_count as i32),
                    finished: finished as i32,
                },
            ),
        );
    };

    let todo_for_next = todo.dupe();
    let files_completed_for_next = files_completed.dupe();
    let next = move || {
        let mut todo_guard = todo_for_next.lock();
        let (ref mut remaining_files, ref mut remaining_count) = *todo_guard;
        // When we get near the end of the file list, start using smaller buckets in order
        // to spread the work across the available workers
        let bucket_size = if *remaining_count >= max_size * num_workers {
            max_size
        } else {
            (*remaining_count / num_workers) + 1
        };
        let split_at = bucket_size.min(remaining_files.len());
        let bucket: Vec<FileKey> = remaining_files.drain(..split_at).collect();
        let bucket_size = bucket.len();
        *remaining_count -= bucket_size;
        if bucket_size == 0 {
            // All files have been distributed. Check if they've all been
            // completed (by batch processing or work stealing).
            if files_completed_for_next.load(Ordering::Acquire) >= total_count {
                Bucket::Done
            } else {
                // Files still being processed on work-stealing deques.
                Bucket::Wait
            }
        } else {
            Bucket::Job(bucket)
        }
    };

    let files_completed_for_merge = files_completed.dupe();
    let merge = move |acc: &mut Vec<R>, finished_file_accs: Vec<R>| {
        intermediate_result_callback(&finished_file_accs);
        let finished_len = finished_file_accs.len();
        files_completed_for_merge.fetch_add(finished_len, Ordering::Release);
        status_update();
        acc.extend(finished_file_accs);
    };

    (next, merge, files_completed)
}
