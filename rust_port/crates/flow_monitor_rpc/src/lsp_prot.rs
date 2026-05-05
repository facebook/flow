/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;

use crate::file_watcher_status;
use crate::server_status;

mod flow_exit_status_serde {
    use flow_common_exit_status::FlowExitStatus;
    use serde::Deserialize;
    use serde::Deserializer;
    use serde::Serialize;
    use serde::Serializer;

    pub fn serialize<S>(status: &FlowExitStatus, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        flow_common_exit_status::error_code(*status).serialize(serializer)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<FlowExitStatus, D::Error>
    where
        D: Deserializer<'de>,
    {
        let code = i32::deserialize(deserializer)?;
        match code {
            -6 => Ok(FlowExitStatus::Interrupted),
            0 => Ok(FlowExitStatus::NoError),
            1 => Ok(FlowExitStatus::WindowsKilledByTaskManager),
            2 => Ok(FlowExitStatus::TypeError),
            3 => Ok(FlowExitStatus::OutOfTime),
            4 => Ok(FlowExitStatus::KillError),
            5 => Ok(FlowExitStatus::UnusedServer),
            6 => Ok(FlowExitStatus::NoServerRunning),
            7 => Ok(FlowExitStatus::OutOfRetries),
            8 => Ok(FlowExitStatus::InvalidFlowconfig),
            9 => Ok(FlowExitStatus::BuildIdMismatch),
            10 => Ok(FlowExitStatus::InputError),
            11 => Ok(FlowExitStatus::LockStolen),
            12 => Ok(FlowExitStatus::CouldNotFindFlowconfig),
            13 => Ok(FlowExitStatus::ServerOutOfDate),
            15 => Ok(FlowExitStatus::OutOfSharedMemory),
            16 => Ok(FlowExitStatus::FlowconfigChanged),
            17 => Ok(FlowExitStatus::PathIsNotAFile),
            18 => Ok(FlowExitStatus::Autostop),
            19 => Ok(FlowExitStatus::KilledByMonitor),
            20 => Ok(FlowExitStatus::InvalidSavedState),
            21 => Ok(FlowExitStatus::Restart),
            22 => Ok(FlowExitStatus::CouldNotExtractFlowlibs),
            64 => Ok(FlowExitStatus::CommandlineUsageError),
            66 => Ok(FlowExitStatus::NoInput),
            78 => Ok(FlowExitStatus::ServerStartFailed),
            97 => Ok(FlowExitStatus::MissingFlowlib),
            98 => Ok(FlowExitStatus::SocketError),
            99 => Ok(FlowExitStatus::DfindDied),
            101 => Ok(FlowExitStatus::WatchmanError),
            102 => Ok(FlowExitStatus::HashTableFull),
            103 => Ok(FlowExitStatus::HeapFull),
            104 => Ok(FlowExitStatus::WatchmanFailed),
            105 => Ok(FlowExitStatus::FileWatcherMissedChanges),
            108 => Ok(FlowExitStatus::EventLoggerRestartOutOfRetries),
            110 => Ok(FlowExitStatus::UnknownError),
            111 => Ok(FlowExitStatus::EdenfsWatcherFailed),
            112 => Ok(FlowExitStatus::EdenfsWatcherLostChanges),
            _ => Err(serde::de::Error::custom(format!(
                "unknown FlowExitStatus code {}",
                code
            ))),
        }
    }
}

pub type ClientId = i32;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ErrorKind {
    ExpectedError,
    UnexpectedError,
}

pub type ErrorInfo = (ErrorKind, String, String);

pub type Json = serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct LoggingContext {
    pub from: Option<String>,
    pub agent_id: Option<String>,
}

pub type ProfilingFinished = serde_json::Value;

pub type LspId = flow_lsp::lsp::LspId;

pub type LspMessage = flow_lsp::lsp::LspMessage;

pub type DocumentUri = flow_lsp::lsp::DocumentUri;

pub type UriMap<V> = flow_lsp::lsp::UriMap<V>;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Metadata {
    // when did this work-item get triggered?
    pub start_wall_time: f64,
    // What was the thing that triggered this work-item
    pub start_json_truncated: Json,
    // What was the state of the server at the time the work-item was triggered?
    // Might be None e.g. if the server was down at the time or if we don't know
    pub start_server_status: Option<server_status::Status>,
    pub start_watcher_status: Option<file_watcher_status::Status>,
    // And what was the state of the lspCommand client? Is optional only to save
    // space in the obvious cases that don't need explanation.
    pub start_lsp_state: Option<String>,
    pub start_lsp_state_reason: Option<String>,
    // If handling the workitem resulted in error, what was that error?
    pub error_info: Option<ErrorInfo>,
    // If the workitem was handled on the server, how long did it take there?
    pub server_profiling: Option<ProfilingFinished>,
    // and if it had work done on the client, how long there?
    pub client_duration: Option<f64>,
    // Did the handler for this workitem provide any extra data?
    pub extra_data: Vec<(String, Json)>,
    // The logging context for the server
    pub server_logging_context: Option<LoggingContext>,
    // LSP method (e.g. 'textDocument/completion')
    pub lsp_method_name: String,
    // If we're tracking an interaction in the lsp process, this is the id of the interaction
    pub interaction_tracking_id: Option<i32>,
    // JSON-RPC id of corresponding LSP request
    pub lsp_id: Option<LspId>,
    pub activity_key: Option<Json>,
}

pub fn empty_metadata() -> Metadata {
    Metadata {
        start_wall_time: 0.0,
        start_server_status: None,
        start_watcher_status: None,
        start_json_truncated: serde_json::Value::Object(Default::default()),
        start_lsp_state: None,
        start_lsp_state_reason: None,
        error_info: None,
        server_profiling: None,
        client_duration: None,
        extra_data: Vec::new(),
        server_logging_context: None,
        lsp_method_name: String::new(),
        interaction_tracking_id: None,
        lsp_id: None,
        activity_key: None,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Request {
    Subscribe,
    LspToServer(LspMessage),
    LiveErrorsRequest(DocumentUri),
}

pub type RequestWithMetadata = (Request, Metadata);

// requests, notifications, responses from client

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ErrorsReason {
    // Sending all the errors at the end of the recheck
    EndOfRecheck,
    // Streaming errors during recheck
    RecheckStreaming,
    EnvChange,
    NewSubscription,
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum ErrorResponseKind {
    CanceledErrorResponse,
    ErroredErrorResponse,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LiveErrorsFailure {
    pub live_errors_failure_kind: ErrorResponseKind,
    pub live_errors_failure_reason: String,
    pub live_errors_failure_uri: DocumentUri,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LiveErrorsResponse {
    pub live_diagnostics: Vec<Diagnostic>,
    pub live_errors_uri: DocumentUri,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum Response {
    LspFromServer(Option<LspMessage>),
    LiveErrorsResponse(Result<LiveErrorsResponse, LiveErrorsFailure>),
    UncaughtException {
        request: Request,
        exception_constructor: String,
        stack: String,
    },
}

pub type ResponseWithMetadata = (Response, Metadata);

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct RecheckStats {
    pub dependent_file_count: i32,
    pub changed_file_count: i32,
    // name of cycle leader, and size of cycle
    pub top_cycle: Option<(flow_parser::file_key::FileKey, i32)>,
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum TelemetryFromServer {
    InitSummary { duration: f64 },
    RecheckSummary { stats: RecheckStats, duration: f64 },
    CommandSummary { name: String, duration: f64 },
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum NotificationFromServer {
    Errors {
        diagnostics: UriMap<Vec<Diagnostic>>,
        errors_reason: ErrorsReason,
    },
    StartRecheck,
    EndRecheck(crate::server_prot::response::LazyStats),
    // only used for the subset of exits which client handles
    ServerExit(#[serde(with = "flow_exit_status_serde")] flow_common_exit_status::FlowExitStatus),
    PleaseHold(server_status::Status, file_watcher_status::Status),
    Telemetry(TelemetryFromServer),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum MessageFromServer {
    RequestResponse(ResponseWithMetadata),
    NotificationFromServer(NotificationFromServer),
}

pub fn string_of_request(request: &Request) -> String {
    match request {
        Request::Subscribe => "subscribe".to_string(),
        Request::LspToServer(msg) => format!("lspToServer {:?}", msg),
        Request::LiveErrorsRequest(uri) => format!("liveErrorsRequest {}", uri),
    }
}

pub fn string_of_request_with_metadata((request, _): &RequestWithMetadata) -> String {
    string_of_request(request)
}

pub fn json_of_request((request, metadata): &RequestWithMetadata) -> serde_json::Value {
    match request {
        Request::Subscribe => {
            serde_json::json!({ "method": "subscribe" })
        }
        Request::LspToServer(_) => metadata.start_json_truncated.clone(),
        Request::LiveErrorsRequest(uri) => {
            serde_json::json!({
                "method": "liveErrorsRequest",
                "params": { "uri": uri.to_string() },
                "trigger": metadata.start_json_truncated,
            })
        }
    }
}

pub fn string_of_response(response: &Response) -> String {
    match response {
        Response::LspFromServer(None) => "lspFromServer None".to_string(),
        Response::LspFromServer(Some(msg)) => format!("lspFromServer {:?}", msg),
        Response::LiveErrorsResponse(Ok(LiveErrorsResponse {
            live_diagnostics,
            live_errors_uri,
        })) => {
            let (errors, warnings, others) =
                live_diagnostics
                    .iter()
                    .fold((0, 0, 0), |(errors, warnings, others), diag| {
                        match diag.severity {
                            Some(s) if s == DiagnosticSeverity::ERROR => {
                                (errors + 1, warnings, others)
                            }
                            Some(s) if s == DiagnosticSeverity::WARNING => {
                                (errors, warnings + 1, others)
                            }
                            Some(_) | None => (errors, warnings, others + 1),
                        }
                    });
            format!(
                "liveErrorsResponse OK ({} errors, {} warnings, {} other) {}",
                errors, warnings, others, live_errors_uri
            )
        }
        Response::LiveErrorsResponse(Err(LiveErrorsFailure {
            live_errors_failure_kind,
            live_errors_failure_reason,
            live_errors_failure_uri,
        })) => {
            format!(
                "liveErrorsResponse {} {} {}",
                match live_errors_failure_kind {
                    ErrorResponseKind::CanceledErrorResponse => "CANCELED",
                    ErrorResponseKind::ErroredErrorResponse => "ERRORED",
                },
                live_errors_failure_reason,
                live_errors_failure_uri
            )
        }
        Response::UncaughtException {
            request,
            exception_constructor,
            stack,
        } => {
            format!(
                "UncaughtException {} in handling `{}`: {}",
                exception_constructor,
                string_of_request(request),
                stack
            )
        }
    }
}

pub struct MessageFromServerMapper {
    pub of_live_errors_failure:
        Box<dyn Fn(&MessageFromServerMapper, LiveErrorsFailure) -> LiveErrorsFailure>,
    pub of_live_errors_response:
        Box<dyn Fn(&MessageFromServerMapper, LiveErrorsResponse) -> LiveErrorsResponse>,
    pub of_message_from_server:
        Box<dyn Fn(&MessageFromServerMapper, MessageFromServer) -> MessageFromServer>,
    pub of_notification:
        Box<dyn Fn(&MessageFromServerMapper, NotificationFromServer) -> NotificationFromServer>,
    pub of_response: Box<dyn Fn(&MessageFromServerMapper, Response) -> Response>,
}

pub fn default_message_from_server_mapper(
    lsp_mapper: std::sync::Arc<flow_lsp::lsp_mapper::T>,
) -> MessageFromServerMapper {
    let lsp_mapper_clone1 = lsp_mapper.clone();
    let of_live_errors_failure = Box::new(
        move |_mapper: &MessageFromServerMapper,
              LiveErrorsFailure {
                  live_errors_failure_kind,
                  live_errors_failure_reason,
                  live_errors_failure_uri,
              }: LiveErrorsFailure| {
            let live_errors_failure_uri =
                (lsp_mapper_clone1.of_document_uri)(&lsp_mapper_clone1, live_errors_failure_uri);
            LiveErrorsFailure {
                live_errors_failure_kind,
                live_errors_failure_reason,
                live_errors_failure_uri,
            }
        },
    );

    let lsp_mapper_clone2 = lsp_mapper.dupe();
    let of_live_errors_response = Box::new(
        move |_mapper: &MessageFromServerMapper,
              LiveErrorsResponse {
                  live_diagnostics,
                  live_errors_uri,
              }: LiveErrorsResponse| {
            let live_diagnostics = live_diagnostics
                .into_iter()
                .map(|d| (lsp_mapper_clone2.of_diagnostic)(&lsp_mapper_clone2, d))
                .collect();
            let live_errors_uri =
                (lsp_mapper_clone2.of_document_uri)(&lsp_mapper_clone2, live_errors_uri);
            LiveErrorsResponse {
                live_diagnostics,
                live_errors_uri,
            }
        },
    );

    let of_message_from_server = Box::new(
        move |mapper: &MessageFromServerMapper, message: MessageFromServer| match message {
            MessageFromServer::RequestResponse((response, metadata)) => {
                MessageFromServer::RequestResponse((
                    (mapper.of_response)(mapper, response),
                    metadata,
                ))
            }
            MessageFromServer::NotificationFromServer(notification) => {
                MessageFromServer::NotificationFromServer((mapper.of_notification)(
                    mapper,
                    notification,
                ))
            }
        },
    );

    let lsp_mapper_clone3 = lsp_mapper.clone();
    let of_notification = Box::new(
        move |_mapper: &MessageFromServerMapper, notif: NotificationFromServer| match notif {
            NotificationFromServer::Errors {
                diagnostics,
                errors_reason,
            } => {
                let diagnostics =
                    diagnostics
                        .into_iter()
                        .fold(UriMap::new(), |mut acc, (uri, diags)| {
                            let uri = (lsp_mapper_clone3.of_document_uri)(&lsp_mapper_clone3, uri);
                            let diags = diags
                                .into_iter()
                                .map(|d| (lsp_mapper_clone3.of_diagnostic)(&lsp_mapper_clone3, d))
                                .collect();
                            acc.insert(uri, diags);
                            acc
                        });
                NotificationFromServer::Errors {
                    diagnostics,
                    errors_reason,
                }
            }
            NotificationFromServer::StartRecheck => NotificationFromServer::StartRecheck,
            NotificationFromServer::EndRecheck(stats) => NotificationFromServer::EndRecheck(stats),
            NotificationFromServer::ServerExit(exit_status) => {
                NotificationFromServer::ServerExit(exit_status)
            }
            NotificationFromServer::PleaseHold(server_status, file_watcher_status) => {
                NotificationFromServer::PleaseHold(server_status, file_watcher_status)
            }
            NotificationFromServer::Telemetry(t) => NotificationFromServer::Telemetry(t),
        },
    );

    let lsp_mapper_clone4 = lsp_mapper;
    let of_response = Box::new(
        move |mapper: &MessageFromServerMapper, response: Response| match response {
            Response::LspFromServer(lsp) => Response::LspFromServer(
                lsp.map(|msg| (lsp_mapper_clone4.of_lsp_message)(&lsp_mapper_clone4, msg)),
            ),
            Response::LiveErrorsResponse(Ok(live_errors_response)) => {
                Response::LiveErrorsResponse(Ok((mapper.of_live_errors_response)(
                    mapper,
                    live_errors_response,
                )))
            }
            Response::LiveErrorsResponse(Err(live_errors_failure)) => Response::LiveErrorsResponse(
                Err((mapper.of_live_errors_failure)(mapper, live_errors_failure)),
            ),
            Response::UncaughtException {
                request,
                exception_constructor,
                stack,
            } => Response::UncaughtException {
                request,
                exception_constructor,
                stack,
            },
        },
    );

    MessageFromServerMapper {
        of_live_errors_failure,
        of_live_errors_response,
        of_message_from_server,
        of_notification,
        of_response,
    }
}
