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
// to not hit the timeout. The check closure may also return `Err(WorkerCanceled)`
// to abort the entire batch; in that case the partial accumulator is discarded and
// the remaining files are returned to the caller for re-queueing.
fn job_helper<A, B, C>(
    check: &mut dyn FnMut(
        FileKey,
    ) -> Result<
        Result<Option<(A, B)>, C>,
        flow_utils_concurrency::worker_cancel::WorkerCanceled,
    >,
    options: &Options,
    start_time: Instant,
    mut acc: Vec<(FileKey, Result<Option<B>, C>)>,
    files: &[FileKey],
) -> Result<
    (Vec<(FileKey, Result<Option<B>, C>)>, Vec<FileKey>),
    flow_utils_concurrency::worker_cancel::WorkerCanceled,
> {
    let mut iter = files.iter();
    loop {
        if out_of_time(options, start_time) {
            flow_hh_logger::debug!(
                "Bucket timed out! {} files finished, {} files unfinished",
                acc.len(),
                iter.len()
            );
            return Ok((acc, iter.map(|f| f.dupe()).collect()));
        }
        let Some(file) = iter.next() else {
            return Ok((acc, vec![]));
        };
        let result = match check(file.dupe())? {
            Ok(Some((_, acc))) => Ok(Some(acc)),
            Ok(None) => Ok(None),
            Err(e) => Err(e),
        };
        acc.push((file.dupe(), result));
    }
}

pub fn mk_job<A, B, C>(
    check: &mut dyn FnMut(
        FileKey,
    ) -> Result<
        Result<Option<(A, B)>, C>,
        flow_utils_concurrency::worker_cancel::WorkerCanceled,
    >,
    options: &Options,
    files: Vec<FileKey>,
) -> Result<
    (Vec<(FileKey, Result<Option<B>, C>)>, Vec<FileKey>),
    flow_utils_concurrency::worker_cancel::WorkerCanceled,
> {
    let start_time = Instant::now();
    job_helper(check, options, start_time, vec![], &files)
}

/// Like `mk_job`, but uses a work-stealing deque and a `CheckJobOutcome`-shaped
/// check closure. The caller pushes files onto the deque before calling this.
/// Other workers can steal from the deque via its Stealer handle while this
/// worker processes.
///
/// On timeout, remaining files stay on the deque rather than being
/// drained and returned. This allows idle workers to steal them
/// immediately via the work-stealing mechanism.
///
/// On `CheckJobOutcome::Canceled`, returns `Err(WorkerCanceled)` and discards
/// the partial accumulator. The recheck driver above is responsible for
/// observing the cancel via the global flag and rolling back the in-progress
/// recheck.
pub fn mk_job_stealing<B>(
    check: &mut dyn FnMut(FileKey) -> crate::merge_service::CheckJobOutcome,
    extract_acc: impl Fn(crate::merge_service::CheckFileResult) -> B,
    options: &Options,
    deque: &crossbeam::deque::Worker<FileKey>,
) -> Result<
    Vec<(
        FileKey,
        Result<
            Option<B>,
            (
                flow_aloc::ALoc,
                flow_typing_errors::error_message::InternalError,
            ),
        >,
    )>,
    flow_utils_concurrency::worker_cancel::WorkerCanceled,
> {
    use crate::merge_service::CheckJobOutcome;

    let start_time = Instant::now();
    let mut acc = Vec::new();
    loop {
        if out_of_time(options, start_time) {
            return Ok(acc);
        }
        let Some(file) = deque.pop() else {
            return Ok(acc);
        };
        match check(file.dupe()) {
            CheckJobOutcome::Ok(Some(r)) => {
                acc.push((file, Ok(Some(extract_acc(r)))));
            }
            CheckJobOutcome::Ok(None) => {
                acc.push((file, Ok(None)));
            }
            CheckJobOutcome::PerFileError(e) => {
                acc.push((file, Err(e)));
            }
            CheckJobOutcome::Canceled(c) => {
                return Err(c);
            }
        }
    }
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
        // Bail out early if the recheck has been canceled. Without this,
        // a worker that returns `Err(WorkerCanceled)` from `mk_job_stealing`
        // discards its partial accumulator without incrementing
        // `files_completed`, so the count never reaches `total_count` and
        // the producer would loop forever returning `Bucket::Wait`. In the
        // OCaml original this could not happen because cancellation was
        // implemented via `Lwt.cancel`, which unwound the entire
        // computation; in our flag-based port the producer also has to
        // honor the cancel flag.
        if flow_utils_concurrency::worker_cancel::should_cancel() {
            return Bucket::Done;
        }
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
