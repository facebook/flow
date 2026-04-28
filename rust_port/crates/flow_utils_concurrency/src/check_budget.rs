/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Per-file check budget: combined cancel + timeout check in O(1).
//!
//! A `CheckBudget` is constructed at the start of a per-file check. Each
//! `check()` call observes the global cancel flag and the elapsed wall clock
//! against the optional `merge_timeout`, returning `Err(JobError::TimedOut)`
//! when the budget is exceeded.

use std::time::Duration;
use std::time::Instant;

use crate::job_error::CheckTimeout;
use crate::job_error::JobError;
use crate::worker_cancel;

#[derive(Debug, Clone, Copy, dupe::Dupe)]
pub struct CheckBudget {
    start: Instant,
    timeout: Option<Duration>,
}

impl CheckBudget {
    pub fn new(timeout: Option<Duration>) -> Self {
        Self {
            start: Instant::now(),
            timeout,
        }
    }

    /// Combined cancel + timeout check. Used by `flow_js::check_canceled`
    /// and by `interruptible_sleep`. O(1).
    pub fn check(&self) -> Result<(), JobError> {
        worker_cancel::check_should_cancel().map_err(JobError::Canceled)?;
        if let Some(t) = self.timeout {
            let elapsed = self.start.elapsed();
            if elapsed >= t {
                return Err(JobError::TimedOut(CheckTimeout { elapsed }));
            }
        }
        Ok(())
    }
}
