/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Wrapper for handling JSON-RPC
// Spec: http://www.jsonrpc.org/specification
// Practical readbable guide: https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md#base-protocol-json-structures

// Daemon-process simplification:
// The OCaml implementation uses `Daemon.spawn` + Lwt for the read pipe so it can
// timestamp messages from a separate process. The Rust port instead uses
// `std::thread::spawn` to read from `stdin` on a background thread. This avoids
// Lwt and the cross-process queue plumbing while preserving the same
// behaviour: messages are read and timestamped as soon as they arrive and
// surfaced to the main loop through an `mpsc` channel.

use std::sync::mpsc::Receiver;
use std::sync::mpsc::RecvTimeoutError;
use std::time::Duration;

use flow_server_env::lsp_prot::LspId;
use lsp_types::NumberOrString;

use crate::flow_lsp::FlowLspError;
use crate::flow_lsp::RemoteExceptionData;
use crate::flow_lsp::now;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Request,
    Notification,
    Response,
}

pub fn kind_to_string(kind: Kind) -> &'static str {
    match kind {
        Kind::Request => "Request",
        Kind::Notification => "Notification",
        Kind::Response => "Response",
    }
}

#[derive(Debug, Clone)]
pub struct JsonrpcMessage {
    pub json: serde_json::Value,
    // time this message arrived at stdin
    // Following fields are decompositions of 'json'...
    pub timestamp: f64,
    pub kind: Kind,
    // mandatory for request+notification; empty otherwise
    pub method_: String,
    // mandatory for request+response
    pub id: Option<LspId>,
    // optional for request+notification
    pub params: Option<serde_json::Value>,
    // optional for response
    pub result: Option<serde_json::Value>,
    // optional for response
    pub error: Option<serde_json::Value>,
}

pub fn message_to_short_string(c: &JsonrpcMessage) -> String {
    let disposition = match (c.kind, &c.result, &c.error) {
        (Kind::Response, Some(_), None) => "[result]",
        (Kind::Response, None, Some(_)) => "[error]",
        (_, _, _) => "",
    };
    let method_ = match c.method_.as_str() {
        "" => String::new(),
        s => format!("method={},", s),
    };
    let id = match &c.id {
        Some(NumberOrString::String(s)) => format!("id=\"{}\"", s),
        Some(NumberOrString::Number(n)) => format!("id=#{}", n),
        None => "id=[None]".to_string(),
    };
    format!(
        "{{{}{},{}{}}}",
        kind_to_string(c.kind),
        disposition,
        method_,
        id
    )
}

pub fn parse_message(
    json: &serde_json::Value,
    timestamp: f64,
) -> Result<JsonrpcMessage, FlowLspError> {
    let id = json.get("id").and_then(|v| match v {
        serde_json::Value::Number(n) => {
            Some(NumberOrString::Number(n.as_i64().unwrap_or(0) as i32))
        }
        serde_json::Value::String(s) => Some(NumberOrString::String(s.clone())),
        _ => None,
    });
    let method_opt = json
        .get("method")
        .and_then(|v| v.as_str())
        .map(String::from);
    let method_ = method_opt.clone().unwrap_or_default();
    // is easier to consume
    let params = json.get("params").cloned();
    let result = json.get("result").cloned();
    let error = json.get("error").cloned();
    // Following categorization mostly mirrors that of VSCode except that
    // VSCode allows number+string+null ID for response, but we allow any ID.
    let kind = match (&id, &method_opt, &result, &error) {
        (Some(_), Some(_), _, _) => Kind::Request,
        (None, Some(_), _, _) => Kind::Notification,
        (_, _, Some(_), _) => Kind::Response,
        (_, _, _, Some(_)) => Kind::Response,
        _ => {
            return Err(FlowLspError::ClientRecoverableConnectionException(
                RemoteExceptionData {
                    message: "Syntax_error: Not JsonRPC".to_string(),
                    stack: String::new(),
                },
            ));
        }
    };
    Ok(JsonrpcMessage {
        json: json.clone(),
        timestamp,
        kind,
        method_,
        id,
        params,
        result,
        error,
    })
}

// (***************************************************************)
// (* Internal queue functions that run in the daemon process.    *)
// (***************************************************************)

pub struct JsonrpcQueue {
    receiver: Receiver<Result<JsonrpcMessage, FlowLspError>>,
}

impl JsonrpcQueue {
    pub fn new() -> Self {
        let (sender, receiver) = std::sync::mpsc::channel();
        std::thread::spawn(move || {
            use std::io::BufRead;
            use std::io::Read;

            let mut reader = std::io::BufReader::new(std::io::stdin());
            loop {
                let result = {
                    let mut content_length: Option<usize> = None;
                    loop {
                        let mut line = String::new();
                        match reader.read_line(&mut line) {
                            Ok(0) => {
                                break Err(FlowLspError::ClientFatalConnectionException(
                                    RemoteExceptionData {
                                        message: "End_of_file".to_string(),
                                        stack: String::new(),
                                    },
                                ));
                            }
                            Ok(_) => {}
                            Err(e) => {
                                break Err(FlowLspError::ClientFatalConnectionException(
                                    RemoteExceptionData {
                                        message: format!("Can't read next header: {}", e),
                                        stack: String::new(),
                                    },
                                ));
                            }
                        }
                        let trimmed = line.trim_end_matches('\n').trim_end_matches('\r');
                        if trimmed.is_empty() {
                            break Ok(());
                        }
                        if let Some(pos) = trimmed.find(':') {
                            let key = trimmed[..pos].to_lowercase();
                            let value = trimmed[pos + 1..].trim();
                            if key == "content-length" {
                                content_length = value.parse().ok();
                            }
                        }
                    }
                    .and_then(|_| {
                        let len = content_length.ok_or_else(|| {
                            FlowLspError::ClientFatalConnectionException(RemoteExceptionData {
                                message: "Missing Content-Length".to_string(),
                                stack: String::new(),
                            })
                        })?;
                        let mut body = vec![0u8; len];
                        reader.read_exact(&mut body).map_err(|e| {
                            FlowLspError::ClientFatalConnectionException(RemoteExceptionData {
                                message: format!("Can't read body: {}", e),
                                stack: String::new(),
                            })
                        })?;
                        let body = String::from_utf8(body).map_err(|e| {
                            FlowLspError::ClientFatalConnectionException(RemoteExceptionData {
                                message: format!("Invalid UTF-8: {}", e),
                                stack: String::new(),
                            })
                        })?;
                        let timestamp = now();
                        let json: serde_json::Value = serde_json::from_str(&body).map_err(|e| {
                            FlowLspError::ClientRecoverableConnectionException(
                                RemoteExceptionData {
                                    message: format!("Syntax_error: {}", e),
                                    stack: String::new(),
                                },
                            )
                        })?;
                        parse_message(&json, timestamp)
                    })
                };
                let is_fatal =
                    matches!(result, Err(FlowLspError::ClientFatalConnectionException(_)));
                if sender.send(result).is_err() || is_fatal {
                    break;
                }
            }
        });
        JsonrpcQueue { receiver }
    }

    pub fn get_message(&self) -> Result<JsonrpcMessage, FlowLspError> {
        match self.receiver.recv() {
            Ok(result) => result,
            Err(_) => Err(FlowLspError::ClientFatalConnectionException(
                RemoteExceptionData {
                    message: "End_of_file".to_string(),
                    stack: String::new(),
                },
            )),
        }
    }

    pub fn get_message_timeout(
        &self,
        timeout: Duration,
    ) -> Option<Result<JsonrpcMessage, FlowLspError>> {
        match self.receiver.recv_timeout(timeout) {
            Ok(result) => Some(result),
            Err(RecvTimeoutError::Timeout) => None,
            Err(RecvTimeoutError::Disconnected) => Some(Err(
                FlowLspError::ClientFatalConnectionException(RemoteExceptionData {
                    message: "End_of_file".to_string(),
                    stack: String::new(),
                }),
            )),
        }
    }
}

impl Default for JsonrpcQueue {
    fn default() -> Self {
        Self::new()
    }
}

// (************************************************)
// (* Output functions for respond+notify          *)
// (************************************************)

// respond: sends either a Response or an Error message, according
// to whether the json has an error-code or not.
pub fn respond(
    writer: fn(&serde_json::Value),
    in_response_to: &JsonrpcMessage,
    result_or_error: &serde_json::Value,
) {
    let is_error = match result_or_error {
        serde_json::Value::Object(_) => result_or_error.get("code").is_some(),
        _ => false,
    };
    let id = match &in_response_to.id {
        Some(NumberOrString::Number(n)) => serde_json::Value::Number((*n).into()),
        Some(NumberOrString::String(s)) => serde_json::Value::String(s.clone()),
        None => serde_json::Value::Null,
    };
    let response = serde_json::json!({
        "jsonrpc": "2.0",
        "id": id,
        (if is_error { "error" } else { "result" }): result_or_error,
    });
    writer(&response);
}

// notify: sends a Notify message
pub fn notify(writer: fn(&serde_json::Value), method_: &str, params: &serde_json::Value) {
    let message = serde_json::json!({
        "jsonrpc": "2.0",
        "method": method_,
        "params": params,
    });
    writer(&message);
}

// (************************************************)
// (* Output functions for request                 *)
// (************************************************)

pub fn get_next_request_id() -> i32 {
    static NEXT_ID: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);
    NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}
