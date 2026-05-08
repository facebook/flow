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

static RUNTIME: LazyLock<Runtime> = LazyLock::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .thread_name("flow-tokio-runtime")
        .build()
        .expect("failed to create tokio runtime for Flow")
});

/// Returns Flow's shared Tokio runtime handle.
pub fn handle() -> Handle {
    RUNTIME.handle().clone()
}

/// Spawns on the current Tokio runtime when one is active, otherwise on Flow's shared runtime.
pub fn spawn(future: impl Future<Output = ()> + Send + 'static) {
    let runtime_handle = match Handle::try_current() {
        Ok(handle) => handle,
        Err(_) => handle(),
    };
    runtime_handle.spawn(future);
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
