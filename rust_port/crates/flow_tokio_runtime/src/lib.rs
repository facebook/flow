/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::future::Future;
use std::sync::LazyLock;

use tokio::runtime::Handle;
use tokio::runtime::Runtime;
use tokio::task::JoinHandle;

static RUNTIME: LazyLock<Runtime> = LazyLock::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .thread_name("flow-tokio-runtime")
        .build()
        .expect("failed to create tokio runtime for Flow")
});

static BLOCKING_POOL_PREWARMED: LazyLock<()> = LazyLock::new(|| {
    let runtime_handle = handle();
    let (started_tx, started_rx) = tokio::sync::oneshot::channel::<()>();
    let task = runtime_handle.spawn_blocking(move || {
        started_tx
            .send(())
            .expect("flow tokio runtime prewarm signal should be received");
    });
    runtime_handle
        .block_on(started_rx)
        .expect("flow tokio runtime should prewarm");
    match runtime_handle.block_on(task) {
        Ok(()) => {}
        Err(err) if err.is_panic() => std::panic::resume_unwind(err.into_panic()),
        Err(err) => panic!("flow tokio runtime prewarm task failed: {}", err),
    }
});

/// Returns Flow's shared Tokio runtime handle.
pub fn handle() -> Handle {
    RUNTIME.handle().clone()
}

pub fn prewarm_blocking_pool() {
    LazyLock::force(&BLOCKING_POOL_PREWARMED);
}

pub fn block_on<F: Future>(future: F) -> F::Output {
    handle().block_on(future)
}

/// Spawns on the current Tokio runtime when one is active, otherwise on Flow's shared runtime.
pub fn spawn(future: impl Future<Output = ()> + Send + 'static) {
    let runtime_handle = match Handle::try_current() {
        Ok(handle) => handle,
        Err(_) => handle(),
    };
    runtime_handle.spawn(future);
}

pub fn spawn_blocking<F, R>(f: F) -> JoinHandle<R>
where
    F: FnOnce() -> R + Send + 'static,
    R: Send + 'static,
{
    handle().spawn_blocking(f)
}

#[cfg(test)]
mod tests {
    use std::sync::mpsc;
    use std::time::Duration;

    use super::spawn;

    #[test]
    fn spawn_runs_without_current_tokio_runtime() {
        let (tx, rx) = mpsc::channel();
        spawn(async move {
            tx.send(())
                .expect("test receiver should still be waiting for the task");
        });
        rx.recv_timeout(Duration::from_secs(5))
            .expect("shared runtime should drive spawned task");
    }
}
