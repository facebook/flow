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

use std::sync::LazyLock;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
use std::time::Duration;

use crate::lock::Condvar;
use crate::lock::Mutex;

static WORKERS_SHOULD_CANCEL: AtomicBool = AtomicBool::new(false);

/// Wake-up channel for `interruptible_sleep`. Any thread blocked on
/// `WAKE.1.wait_timeout(...)` is notified by `stop_workers()` so the worker
/// returns control to the caller within milliseconds rather than at its next
/// poll boundary.
static WAKE: LazyLock<(Mutex<()>, Condvar)> = LazyLock::new(|| (Mutex::new(()), Condvar::new()));

/// Signal all workers to cancel their current work.
pub fn stop_workers() {
    WORKERS_SHOULD_CANCEL.store(true, Ordering::Release);
    let _g = WAKE.0.lock();
    WAKE.1.notify_all();
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
#[derive(Debug, Clone, dupe::Dupe)]
pub struct WorkerCanceled;

impl std::fmt::Display for WorkerCanceled {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Worker canceled")
    }
}

impl std::error::Error for WorkerCanceled {}

// Check if cancellation has been requested, and if so, return the
// WorkerCanceled error so the caller can `?`-propagate it.
// This mirrors OCaml's WorkerCancel.check_should_cancel which raises
// the Worker_should_cancel exception; in Rust we model the OCaml exception
// as a Result so the type system enforces propagation.
pub fn check_should_cancel() -> Result<(), WorkerCanceled> {
    if should_cancel() {
        Err(WorkerCanceled)
    } else {
        Ok(())
    }
}

/// Sleep up to `d`, returning early on cancel.
///
/// Used by `$Flow$DebugSleep` so a long sleep does not have to wait for its
/// next 1-second poll to observe a cancel. `stop_workers()` notifies the
/// shared condvar, which wakes the sleeping worker; the worker then returns
/// `Err(WorkerCanceled)` essentially immediately.
///
/// The cancel-flag check is performed *while holding* `WAKE.0` so a
/// concurrent `stop_workers()` cannot slip its `notify_all` in between the
/// flag check and the `wait_timeout`. Without that ordering, the notify
/// would arrive when no thread is waiting yet and the worker would then
/// block in `wait_timeout` for the full duration even though the cancel
/// flag is set.
pub fn interruptible_sleep(d: Duration) -> Result<(), WorkerCanceled> {
    let guard = WAKE.0.lock();
    if should_cancel() {
        return Err(WorkerCanceled);
    }
    let (_g, _result) = WAKE.1.wait_timeout(guard, d);
    if should_cancel() {
        Err(WorkerCanceled)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::thread;
    use std::time::Duration;
    use std::time::Instant;

    use super::*;

    /// A `stop_workers` call from another thread must wake an in-progress
    /// `interruptible_sleep` within ~50 ms. The sleep itself requests 5 s.
    #[test]
    fn interruptible_sleep_wakes_on_stop_workers() {
        // Reset state so we don't trip over a sibling test.
        resume_workers();

        let sleeper = thread::spawn(|| {
            let start = Instant::now();
            let result = interruptible_sleep(Duration::from_secs(5));
            (result, start.elapsed())
        });

        // Give the sleeper a moment to actually enter wait_timeout.
        thread::sleep(Duration::from_millis(20));
        stop_workers();

        let (result, elapsed) = sleeper.join().expect("sleeper thread panicked");
        // Restore state so other tests in this binary aren't poisoned.
        resume_workers();

        assert!(result.is_err(), "interruptible_sleep should observe cancel");
        assert!(
            elapsed < Duration::from_millis(500),
            "interruptible_sleep should return within ~50 ms of stop_workers, got {:?}",
            elapsed
        );
    }

    /// In the absence of a cancel signal, `interruptible_sleep` returns Ok
    /// after the requested duration.
    #[test]
    fn interruptible_sleep_returns_ok_on_timeout() {
        resume_workers();
        let start = Instant::now();
        let result = interruptible_sleep(Duration::from_millis(50));
        let elapsed = start.elapsed();
        assert!(result.is_ok(), "no cancel was signaled");
        assert!(
            elapsed >= Duration::from_millis(40),
            "sleep returned too early: {:?}",
            elapsed
        );
    }
}
