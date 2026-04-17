/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Utilities for creating the initial thread pool.

use std::env;
use std::num::NonZeroUsize;
use std::str::FromStr;
use std::sync::LazyLock;

use human_bytes::human_bytes;
use tracing::debug;
use tracing::info;

use crate::display::number_thousands;
use crate::lock::Mutex;

/// The stack size for all created threads.
///
/// Can be overridden by setting the `FLOW_STACK_SIZE` environment variable (in bytes).
/// The Flow type checker uses deep recursion (e.g., dispatch::__flow), so we need a
/// large stack.
const DEFAULT_STACK_SIZE: usize = if cfg!(debug_assertions) {
    256 * 1024 * 1024
} else {
    32 * 1024 * 1024
};

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub enum ThreadCount {
    #[default]
    AllThreads,
    NumThreads(NonZeroUsize),
}

impl FromStr for ThreadCount {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<usize>() {
            Ok(n) => match NonZeroUsize::new(n) {
                None => Ok(ThreadCount::AllThreads),
                Some(n) => Ok(ThreadCount::NumThreads(n)),
            },
            Err(e) => Err(format!(
                "Failed to parse thread count, expected number, failed due to {e}"
            )),
        }
    }
}

static THREADS: LazyLock<Mutex<ThreadCount>> = LazyLock::new(|| Mutex::new(ThreadCount::default()));

/// Set up the global thread pool.
pub fn init_thread_pool(threads: ThreadCount) {
    *THREADS.lock() = threads;
}

/// A WASM compatible thread-pool.
pub struct ThreadPool(
    // Will be None on WASM
    Option<rayon::ThreadPool>,
);

impl ThreadPool {
    fn stack_size() -> usize {
        match env::var("FLOW_STACK_SIZE") {
            Ok(s) => {
                let res = s
                    .parse::<usize>()
                    .unwrap_or_else(|_| panic!("$FLOW_STACK_SIZE must be a number, got {s}"));
                info!(
                    "Using stack size of {} bytes (due to `$FLOW_STACK_SIZE`)",
                    number_thousands(res)
                );
                res
            }
            Err(_) => DEFAULT_STACK_SIZE,
        }
    }

    pub fn with_thread_count(count: ThreadCount) -> Self {
        if cfg!(target_arch = "wasm32") {
            // ThreadPool doesn't work on WASM
            return Self(None);
        }

        let stack_size = Self::stack_size();
        let mut builder = rayon::ThreadPoolBuilder::new().stack_size(stack_size);
        if let ThreadCount::NumThreads(threads) = count {
            builder = builder.num_threads(threads.get());
        }
        let pool = builder.build().expect("To be able to build a thread pool");
        // Only print the message once
        debug!(
            "Running with {} threads ({} stack size)",
            pool.current_num_threads(),
            human_bytes(stack_size as f64)
        );
        Self(Some(pool))
    }

    pub fn new() -> Self {
        Self::with_thread_count(*THREADS.lock())
    }

    pub fn spawn_many(&self, f: impl Fn() + Sync) {
        match &self.0 {
            None => f(),
            Some(pool) => {
                pool.scope(|s| {
                    for _ in 0..pool.current_num_threads() {
                        // Only run work on Rayon threads, as we increased their stack limit
                        s.spawn(|_| f());
                    }
                })
            }
        }
    }

    pub fn async_spawn(&self, f: impl FnOnce() + Send + 'static) {
        match &self.0 {
            None => f(),
            Some(pool) => {
                pool.spawn(f);
            }
        }
    }

    /// Run a closure on every thread in the pool exactly once.
    /// Unlike `spawn_many`, this guarantees every thread executes (no work-stealing skips).
    pub fn broadcast<F: Fn(usize) + Sync>(&self, f: F) {
        match &self.0 {
            None => f(0),
            Some(pool) => {
                pool.broadcast(|ctx| f(ctx.index()));
            }
        }
    }

    // See rayon::ThreadPool::install
    pub fn install<OP, R>(&self, op: OP) -> R
    where
        OP: FnOnce() -> R + Send,
        R: Send,
    {
        match &self.0 {
            None => op(),
            Some(pool) => pool.install(op),
        }
    }

    /// Get the number of worker threads in the pool.
    ///
    /// Returns the number of threads for calculating optimal batch sizes.
    /// Returns 1 if running on WASM (no thread pool).
    pub fn num_workers(&self) -> usize {
        match &self.0 {
            None => 1,
            Some(pool) => pool.current_num_threads(),
        }
    }
}
