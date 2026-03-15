/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;
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

/// A stateful (next, merge) pair. This lets us re-queue unfinished files which are returned
/// when a bucket times out
pub fn mk_next<R>(
    intermediate_result_callback: Arc<dyn Fn(&[R]) + Send + Sync>,
    max_size: usize,
    num_workers: usize,
    files: Vec<FileKey>,
) -> (
    impl Next<FileKey>,
    impl Fn(&mut Vec<R>, (Vec<R>, Vec<FileKey>)) + Send + Sync,
)
where
    R: 'static,
{
    // let total_count = List.length files in
    let total_count = files.len();
    // let todo = ref (files, total_count) in
    let todo = Arc::new(Mutex::new((files, total_count)));
    // let finished_count = ref 0 in
    let finished_count = Arc::new(Mutex::new(0usize));
    let num_workers = num_workers.max(1);

    let finished_count_for_status = finished_count.dupe();
    let status_update = move || {
        let finished = *finished_count_for_status.lock();
        eprintln!(
            "Checking progress: {}/{} files finished",
            finished, total_count
        );
    };

    let todo_for_next = todo.dupe();
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
            Bucket::Done
        } else {
            Bucket::Job(bucket)
        }
    };

    let todo_for_merge = todo;
    let merge =
        move |acc: &mut Vec<R>, (finished_file_accs, unfinished_files): (Vec<R>, Vec<FileKey>)| {
            intermediate_result_callback(&finished_file_accs);
            let mut todo_guard = todo_for_merge.lock();
            let (ref mut remaining_files, ref mut remaining_count) = *todo_guard;
            let unfinished_count = unfinished_files.len();
            let mut new_remaining = unfinished_files;
            new_remaining.append(remaining_files);
            *remaining_files = new_remaining;
            *remaining_count += unfinished_count;
            let finished_len = finished_file_accs.len();
            *finished_count.lock() += finished_len;
            status_update();
            acc.extend(finished_file_accs);
        };

    (next, merge)
}
