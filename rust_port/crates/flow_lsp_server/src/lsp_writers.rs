/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// open Lsp
// open Lsp_fmt

use std::collections::BTreeSet;

use flow_lsp::lsp_fmt::print_connection_status;
use flow_lsp::lsp_fmt::print_diagnostics;
use flow_lsp::lsp_fmt::print_log_message;
use flow_server_env::lsp::connection_status;
use flow_server_env::lsp_helpers;
use lsp_types::InitializeParams;
use lsp_types::MessageType;
use lsp_types::PublishDiagnosticsParams;
use lsp_types::Url;

use crate::jsonrpc;

// (************************************************************************)
// (* Wrappers for some LSP methods                                        *)
// (************************************************************************)

pub fn telemetry(writer: fn(&serde_json::Value), level: MessageType, message: &str) {
    jsonrpc::notify(
        writer,
        "telemetry/event",
        &print_log_message(level, message),
    );
}

pub fn telemetry_error(writer: fn(&serde_json::Value), message: &str) {
    telemetry(writer, MessageType::ERROR, message);
}

pub fn telemetry_log(writer: fn(&serde_json::Value), message: &str) {
    telemetry(writer, MessageType::LOG, message);
}

pub fn log(writer: fn(&serde_json::Value), level: MessageType, message: &str) {
    jsonrpc::notify(
        writer,
        "window/logMessage",
        &print_log_message(level, message),
    );
}

pub fn log_error(writer: fn(&serde_json::Value), message: &str) {
    log(writer, MessageType::ERROR, message);
}

pub fn log_warning(writer: fn(&serde_json::Value), message: &str) {
    log(writer, MessageType::WARNING, message);
}

pub fn log_info(writer: fn(&serde_json::Value), message: &str) {
    log(writer, MessageType::INFO, message);
}

pub fn dismiss_diagnostics(
    writer: fn(&serde_json::Value),
    diagnostic_uris: BTreeSet<Url>,
) -> BTreeSet<Url> {
    let dismiss_one = |uri: &Url| {
        let message = PublishDiagnosticsParams {
            uri: uri.clone(),
            diagnostics: vec![],
            version: None,
        };
        jsonrpc::notify(
            writer,
            "textDocument/publishDiagnostics",
            &print_diagnostics(&message),
        );
    };
    for uri in &diagnostic_uris {
        dismiss_one(uri);
    }
    BTreeSet::new()
}

pub fn notify_connection_status(
    p: &InitializeParams,
    writer: fn(&serde_json::Value),
    was_connected: bool,
    is_connected: bool,
) -> bool {
    if lsp_helpers::supports_connection_status(p) && was_connected != is_connected {
        let message = connection_status::Params { is_connected };
        jsonrpc::notify(
            writer,
            "telemetry/connectionStatus",
            &print_connection_status(&message),
        );
    }
    is_connected
}
