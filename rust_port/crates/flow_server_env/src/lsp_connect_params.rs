/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[derive(Debug, Clone)]
pub struct SharedMemParams {
    pub shm_heap_size: Option<u64>,
    pub shm_hash_table_pow: Option<u32>,
}

#[derive(Debug, Clone)]
pub enum OnMismatchBehavior {
    ChooseNewest,
    StopServer,
    RestartClient,
    ErrorClient,
}

#[derive(Debug, Clone)]
pub struct ConnectParams {
    pub retries: i32,
    pub timeout: Option<i32>,
    pub no_auto_start: bool,
    pub autostop: bool,
    pub from: Option<String>,
    pub lazy_mode: Option<flow_config::LazyMode>,
    pub temp_dir: Option<String>,
    pub shm_flags: SharedMemParams,
    pub ignore_version: bool,
    pub quiet: bool,
    pub on_mismatch: OnMismatchBehavior,
}

pub fn persistent_client_handshake(
    version_mismatch_strategy: &socket_handshake::VersionMismatchStrategy,
    lsp_init_params: InitializeParams,
) -> socket_handshake::ClientHandshake {
    (
        socket_handshake::ClientToMonitor1 {
            client_build_id: socket_handshake::build_revision(),
            client_version: flow_common::flow_version::VERSION.to_string(),
            is_stop_request: false,
            server_should_hangup_if_still_initializing: true,
            version_mismatch_strategy: version_mismatch_strategy.clone(),
        },
        socket_handshake::ClientToMonitor2 {
            client_type: socket_handshake::ClientType::Persistent { lsp_init_params },
        },
    )
}
use lsp_types::InitializeParams;

use crate::socket_handshake;
