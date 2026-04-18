/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::LazyLock;

use tokio::runtime::Handle;
use tokio::runtime::Runtime;

static RUNTIME: LazyLock<Runtime> = LazyLock::new(|| {
    tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .thread_name("flow-server-monitor")
        .build()
        .expect("failed to create tokio runtime for flow_server_monitor")
});

pub fn handle() -> Handle {
    RUNTIME.handle().clone()
}
