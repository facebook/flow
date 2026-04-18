/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_types::InitializeParams;
use serde::Deserialize;
use serde::Serialize;

use crate::file_watcher_status;
use crate::server_status;

pub type BuildId = String;

pub fn build_revision() -> String {
    flow_common::flow_version::VERSION.to_string()
}

pub type ClientHandshakeWire = (String, Vec<u8>);

pub type ServerHandshakeWire = (String, Option<Vec<u8>>);

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum VersionMismatchStrategy {
    AlwaysStopServer,
    StopServerIfOlder,
    ErrorClient,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientToMonitor1 {
    pub client_build_id: BuildId,
    pub client_version: String,
    pub is_stop_request: bool,
    pub server_should_hangup_if_still_initializing: bool,
    pub version_mismatch_strategy: VersionMismatchStrategy,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ServerIntent {
    ServerWillExit,
    ServerWillHangup,
    ServerWillContinue,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MonitorToClient1 {
    pub server_build_id: BuildId,
    pub server_bin: String,
    pub server_intent: ServerIntent,
    pub server_version: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientType {
    Ephemeral,
    Persistent { lsp_init_params: InitializeParams },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientToMonitor2 {
    pub client_type: ClientType,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MonitorToClient2 {
    ServerHasTooManyClients,
    ServerStillInitializing(server_status::Status, file_watcher_status::Status),
    ServerReady,
}

pub type ClientHandshake = (ClientToMonitor1, ClientToMonitor2);

pub type ServerHandshake = (MonitorToClient1, Option<MonitorToClient2>);

pub fn version_mismatch_strategy_to_string(strategy: &VersionMismatchStrategy) -> &'static str {
    match strategy {
        VersionMismatchStrategy::AlwaysStopServer => "Always_stop_server",
        VersionMismatchStrategy::StopServerIfOlder => "Stop_server_if_older",
        VersionMismatchStrategy::ErrorClient => "Error_client",
    }
}

pub fn string_to_version_mismatch_strategy(s: &str) -> VersionMismatchStrategy {
    match s {
        "Always_stop_server" => VersionMismatchStrategy::AlwaysStopServer,
        "Stop_server_if_older" => VersionMismatchStrategy::StopServerIfOlder,
        "Error_client" => VersionMismatchStrategy::ErrorClient,
        _ => VersionMismatchStrategy::ErrorClient,
    }
}

pub fn client_to_monitor_1_to_json(c: &ClientToMonitor1) -> serde_json::Value {
    serde_json::json!({
        "client_build_id": c.client_build_id,
        "is_stop_request": c.is_stop_request,
        "server_should_hangup_if_still_initializing": c.server_should_hangup_if_still_initializing,
        "client_version": c.client_version,
        "version_mismatch_strategy": version_mismatch_strategy_to_string(&c.version_mismatch_strategy),
        "server_should_exit_if_version_mismatch": match c.version_mismatch_strategy {
            VersionMismatchStrategy::AlwaysStopServer => true,
            VersionMismatchStrategy::StopServerIfOlder => true,
            VersionMismatchStrategy::ErrorClient => false,
        },
    })
}

pub fn default_client_to_monitor_1() -> ClientToMonitor1 {
    ClientToMonitor1 {
        client_build_id: "INCOMPATIBLE".to_string(),
        is_stop_request: false,
        server_should_hangup_if_still_initializing: false,
        client_version: "0.0.0".to_string(),
        version_mismatch_strategy: VersionMismatchStrategy::ErrorClient,
    }
}

pub fn json_to_client_to_monitor_1(json: &serde_json::Value) -> ClientToMonitor1 {
    let d = default_client_to_monitor_1();
    let client_build_id = json
        .get("client_build_id")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or(d.client_build_id);
    let is_stop_request = json
        .get("is_stop_request")
        .and_then(|v| v.as_bool())
        .unwrap_or(d.is_stop_request);
    let server_should_hangup_if_still_initializing = json
        .get("server_should_hangup_if_still_initializing")
        .and_then(|v| v.as_bool())
        .unwrap_or(d.server_should_hangup_if_still_initializing);
    let client_version = json
        .get("client_version")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or(d.client_version);
    let version_mismatch_strategy = match json
        .get("version_mismatch_strategy")
        .and_then(|v| v.as_str())
    {
        Some(strategy) => string_to_version_mismatch_strategy(strategy),
        None => {
            match json
                .get("server_should_exit_if_version_mismatch")
                .and_then(|v| v.as_bool())
            {
                Some(true) => VersionMismatchStrategy::AlwaysStopServer,
                Some(false) | None => VersionMismatchStrategy::ErrorClient,
            }
        }
    };
    ClientToMonitor1 {
        client_build_id,
        is_stop_request,
        server_should_hangup_if_still_initializing,
        client_version,
        version_mismatch_strategy,
    }
}

pub fn monitor_to_client_1_to_json(m: &MonitorToClient1) -> serde_json::Value {
    let intent_to_string = |intent: &ServerIntent| -> &'static str {
        match intent {
            ServerIntent::ServerWillExit => "Server_will_exit",
            ServerIntent::ServerWillHangup => "Server_will_hangup",
            ServerIntent::ServerWillContinue => "Server_will_continue",
        }
    };
    serde_json::json!({
        "server_build_id": m.server_build_id,
        "server_bin": m.server_bin,
        "server_intent": intent_to_string(&m.server_intent),
        "server_version": m.server_version,
    })
}

pub fn json_to_monitor_to_client_1(json: &serde_json::Value) -> Result<MonitorToClient1, String> {
    let string_to_intent = |s: &str| -> Result<ServerIntent, String> {
        match s {
            "Server_will_exit" => Ok(ServerIntent::ServerWillExit),
            "Server_will_hangup" => Ok(ServerIntent::ServerWillHangup),
            "Server_will_continue" => Ok(ServerIntent::ServerWillContinue),
            _ => Err(format!("unknown intent {}", s)),
        }
    };
    let server_build_id = json
        .get("server_build_id")
        .and_then(|v| v.as_str())
        .ok_or_else(|| "missing server_build_id".to_string())?
        .to_string();
    let server_bin = json
        .get("server_bin")
        .and_then(|v| v.as_str())
        .ok_or_else(|| "missing server_bin".to_string())?
        .to_string();
    let server_intent = string_to_intent(
        json.get("server_intent")
            .and_then(|v| v.as_str())
            .ok_or_else(|| "missing server_intent".to_string())?,
    )?;
    let server_version = json
        .get("server_version")
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| "0.0.0".to_string());
    Ok(MonitorToClient1 {
        server_build_id,
        server_bin,
        server_intent,
        server_version,
    })
}
