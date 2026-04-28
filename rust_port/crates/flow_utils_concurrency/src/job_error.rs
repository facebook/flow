/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Unified error type for cancellation and per-file timeout.
//!
//! Cancel and timeout are both modeled as `Result` rather than panics. Low
//! level callsites return `Result<_, WorkerCanceled>`; once a per-file budget
//! is in scope, callsites return `Result<_, JobError>` and `?` upgrades a
//! `WorkerCanceled` into `JobError::Canceled` via the `From` impl below.

use std::time::Duration;

use flow_aloc::ALoc;

use crate::worker_cancel::WorkerCanceled;

/// Per-file check timeout. Carries the elapsed duration so the caller can
/// surface it as an `InternalError::CheckTimeout` without re-parsing a string.
#[derive(Debug, Clone, dupe::Dupe)]
pub struct CheckTimeout {
    pub elapsed: Duration,
}

/// Per-file failure modes that propagate through the cancel/timeout cascade.
/// `DebugThrow` carries the call-site location so the boundary in
/// `merge_service::mk_check` can surface it as an `InternalError::DebugThrow`
/// per-file diagnostic without parsing a panic payload (mirrors OCaml's
/// `Error_message.EDebugThrow` exception).
#[derive(Debug, Clone, dupe::Dupe)]
pub enum JobError {
    Canceled(WorkerCanceled),
    TimedOut(CheckTimeout),
    DebugThrow { loc: ALoc },
}

impl From<WorkerCanceled> for JobError {
    fn from(c: WorkerCanceled) -> Self {
        JobError::Canceled(c)
    }
}

impl From<CheckTimeout> for JobError {
    fn from(t: CheckTimeout) -> Self {
        JobError::TimedOut(t)
    }
}

impl std::fmt::Display for JobError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JobError::Canceled(c) => write!(f, "{}", c),
            JobError::TimedOut(t) => write!(f, "check timed out after {:?}", t.elapsed),
            JobError::DebugThrow { loc } => write!(f, "$Flow$DebugThrow at {:?}", loc),
        }
    }
}

impl std::error::Error for JobError {}
