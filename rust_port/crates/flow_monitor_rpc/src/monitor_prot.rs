/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;

use lsp_types::InitializeParams;

use crate::file_watcher_status;
use crate::lsp_prot;
use crate::server_command_with_context::ServerCommandWithContext;
use crate::server_prot::response;
use crate::server_status;

// Ephemeral socket connections expect a response to their requests. We use request_id to indicate
// to which request a given response is replying
pub type RequestId = String;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct FileWatcherMetadata {
    // [Some _] if we checked whether the mergebase changed, [None] if we didn't/can't ask the VCS
    pub changed_mergebase: Option<bool>,
    pub missed_changes: bool,
}

pub fn empty_file_watcher_metadata() -> FileWatcherMetadata {
    FileWatcherMetadata {
        changed_mergebase: None,
        missed_changes: false,
    }
}

pub fn merge_file_watcher_metadata(
    a: &FileWatcherMetadata,
    b: &FileWatcherMetadata,
) -> FileWatcherMetadata {
    let changed_mergebase = match (a.changed_mergebase, b.changed_mergebase) {
        (None, None) => None,
        (Some(x), Some(y)) => Some(x || y),
        (Some(true), None) | (None, Some(true)) => Some(true),
        // we don't know for sure, so return [None]
        (Some(false), None) | (None, Some(false)) => None,
    };
    FileWatcherMetadata {
        changed_mergebase,
        missed_changes: a.missed_changes || b.missed_changes,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum PleaseDieReason {
    MonitorExiting(flow_common_exit_status::FlowExitStatus, String),
}

// These are the messages that the monitor sends to the server
#[derive(serde::Serialize, serde::Deserialize)]
pub enum MonitorToServerMessage {
    // A request from an ephemeral socket connection. It expects a response
    Request(RequestId, ServerCommandWithContext),
    // A notification that there is a new persistent socket connection
    NewPersistentConnection(lsp_prot::ClientId, InitializeParams),
    // A request from a persistent socket connection. It does not expect a response
    PersistentConnectionRequest(lsp_prot::ClientId, lsp_prot::RequestWithMetadata),
    // A notification that a persistent socket connection is dead
    DeadPersistentConnection(lsp_prot::ClientId),
    // The file watcher has noticed changes
    FileWatcherNotification {
        files: BTreeSet<String>,
        metadata: Option<FileWatcherMetadata>,
        initial: bool,
    },
    // Monitor wants to kill the server but first asks nicely for the server to honorably kill itself
    PleaseDie(PleaseDieReason),
}

// These are the messages that the server sends to the monitor
#[derive(serde::Serialize, serde::Deserialize)]
pub enum ServerToMonitorMessage {
    // A response to an ephemeral socket's request
    Response(RequestId, response::Response),
    // An exception was thrown while processing the request
    RequestFailed(RequestId, String),
    // A response to a persistent socket connection
    PersistentConnectionResponse(lsp_prot::ClientId, lsp_prot::MessageFromServer),
    // A notification of the server's current status
    StatusUpdate(server_status::Status),
    // A telemetry notification from the server
    Telemetry(lsp_prot::TelemetryFromServer),
}

// These are the messages that the server sends to an ephemeral socket connection
#[derive(serde::Serialize, serde::Deserialize)]
pub enum MonitorToClientMessage {
    // The response from the server
    Data(response::Response),
    // The server threw an exception while processing the request
    ServerException(String),
    // The server is currently busy. Please wait for a response
    PleaseHold(server_status::Status, file_watcher_status::Status),
}
