/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Worker cancellation support.
//!
//! Port of OCaml's workerCancel.ml. Provides a global cancellation flag
//! that can be set by the server to cancel in-progress type checking work.
//! Workers (e.g., the $Flow$DebugSleep handler) periodically check this flag
//! and bail out when cancellation is requested.

use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

static WORKERS_SHOULD_CANCEL: AtomicBool = AtomicBool::new(false);

/// Signal all workers to cancel their current work.
pub fn stop_workers() {
    WORKERS_SHOULD_CANCEL.store(true, Ordering::Release);
}

/// Resume workers — clear the cancellation flag.
pub fn resume_workers() {
    WORKERS_SHOULD_CANCEL.store(false, Ordering::Release);
}

/// Check whether cancellation has been requested.
/// Returns true if workers should cancel.
pub fn should_cancel() -> bool {
    WORKERS_SHOULD_CANCEL.load(Ordering::Acquire)
}

/// Error type raised when cancellation is detected.
#[derive(Debug, Clone)]
pub struct WorkerCanceled;

impl std::fmt::Display for WorkerCanceled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Worker canceled")
    }
}

impl std::error::Error for WorkerCanceled {}

/// Check if cancellation has been requested, and if so, panic with
/// a message that can be caught to abort the current operation.
/// This mirrors OCaml's WorkerCancel.check_should_cancel which raises
/// an exception.
pub fn check_should_cancel() {
    if should_cancel() {
        // We use panic to unwind the stack, similar to OCaml's exception.
        // The caller should catch this with std::panic::catch_unwind.
        // The message matches merge_service.rs's critical panic detection:
        // s.contains("Worker_should_cancel")
        panic!("Worker_should_cancel");
    }
}
