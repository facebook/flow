/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::io;
use std::io::Write as _;
use std::net::Shutdown;
use std::net::TcpStream;
use std::sync::Mutex;
use std::sync::mpsc::Receiver;
use std::sync::mpsc::RecvTimeoutError;
use std::time::Duration;

use flow_common_exit_status::FlowExitStatus;

pub type FilePath = std::path::PathBuf;
use flow_server_env::file_watcher_status;
use flow_server_env::lsp_connect_params;
use flow_server_env::lsp_connect_params::ConnectParams;
use flow_server_env::lsp_mapper;
use flow_server_env::lsp_prot;
use flow_server_env::lsp_prot::DocumentUri;
use flow_server_env::lsp_prot::LspId;
use flow_server_env::lsp_prot::Metadata;
use flow_server_env::lsp_prot::UriMap;
use flow_server_env::server_prot;
use flow_server_env::server_socket_rpc;
use flow_server_env::server_status;
use flow_server_env::socket_handshake;
use lsp_types::MessageType;
use lsp_types::NumberOrString;

type RawFd = std::os::raw::c_int;

fn lsp_exit_ok() -> ! {
    std::process::exit(0)
}

fn lsp_exit_bad() -> ! {
    std::process::exit(1)
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WrappedId {
    pub server_id: i32,
    pub message_id: LspId,
}

pub fn encode_wrapped(wrapped_id: &WrappedId) -> LspId {
    let WrappedId {
        server_id,
        message_id,
    } = wrapped_id;
    match message_id {
        NumberOrString::Number(id) => NumberOrString::String(format!("{}:#{}", server_id, id)),
        NumberOrString::String(id) => NumberOrString::String(format!("{}:'{}", server_id, id)),
    }
}

pub fn decode_wrapped(lsp: &LspId) -> WrappedId {
    try_decode_wrapped(lsp)
        .unwrap_or_else(|_| panic!("Invalid message id {}", lsp_fmt_id_to_string(lsp)))
}

fn try_decode_wrapped(lsp: &LspId) -> Result<WrappedId, FlowLspError> {
    let s = match lsp {
        NumberOrString::Number(_) => {
            return Err(parse_error_exception("not a wrapped id"));
        }
        NumberOrString::String(s) => s,
    };
    let colon_pos = s
        .find(':')
        .ok_or_else(|| parse_error_exception(format!("Invalid message id {}", s)))?;
    let server_id: i32 = s[..colon_pos]
        .parse()
        .map_err(|_| parse_error_exception(format!("Invalid message id {}", s)))?;
    let rest = &s[colon_pos + 1..];
    if rest.is_empty() {
        return Err(parse_error_exception(format!("Invalid message id {}", s)));
    }
    let kind = &rest[..1];
    let message_str = &rest[1..];
    let message_id = if kind == "#" {
        NumberOrString::Number(
            message_str
                .parse()
                .map_err(|_| parse_error_exception(format!("Invalid message id {}", s)))?,
        )
    } else {
        NumberOrString::String(message_str.to_string())
    };
    Ok(WrappedId {
        server_id,
        message_id,
    })
}

pub type WrappedMap<V> = HashMap<WrappedId, V>;

pub struct ServerConn {
    pub client_id: lsp_prot::ClientId,
    pub stream: Mutex<TcpStream>,
}

#[derive(Debug, Clone)]
pub enum ShowStatusT {
    NeverShown,
    Shown(Option<LspId>, lsp_mapper::show_status::Params),
}

// o_open_doc is guaranteed to be up-to-date with respect to the editor
// o_ast, if present, is guaranteed to be up-to-date. It gets computed lazily.
// o_unsaved if true means that this open file has unsaved changes to the buffer.
#[derive(Debug, Clone)]
pub struct OpenFileInfo {
    pub o_open_doc: lsp_types::TextDocumentItem,
    pub o_ast: Option<(
        flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
        Option<Vec<lsp_types::Diagnostic>>,
    )>,
    pub o_unsaved: bool,
}

pub struct InitializedEnv {
    pub i_initialize_params: lsp_types::InitializeParams,
    pub i_connect_params: ConnectParams,
    pub i_root: FilePath,
    pub i_version: Option<String>,
    pub i_server_id: i32,
    pub i_can_autostart_after_version_mismatch: bool,
    pub i_outstanding_local_handlers: HashMap<LspId, LspHandler>,
    pub i_outstanding_local_requests: HashMap<LspId, lsp_mapper::LspRequest>,
    pub i_outstanding_requests_from_server: WrappedMap<lsp_mapper::LspRequest>,
    pub i_is_connected: bool,
    pub i_status: ShowStatusT,
    pub i_open_files: UriMap<OpenFileInfo>,
    pub i_errors: crate::lsp_errors::T,
    pub i_config: serde_json::Value,
    pub i_flowconfig: FlowConfig,
    pub i_file_options: FileOptions,
}

impl Clone for InitializedEnv {
    fn clone(&self) -> Self {
        assert!(
            self.i_outstanding_local_handlers.is_empty(),
            "Cannot clone InitializedEnv with {} outstanding local handlers — \
             Box<dyn FnOnce> is not Clone. Restructure to avoid clone with outstanding handlers.",
            self.i_outstanding_local_handlers.len()
        );
        InitializedEnv {
            i_initialize_params: self.i_initialize_params.clone(),
            i_connect_params: self.i_connect_params.clone(),
            i_root: self.i_root.clone(),
            i_version: self.i_version.clone(),
            i_server_id: self.i_server_id,
            i_can_autostart_after_version_mismatch: self.i_can_autostart_after_version_mismatch,
            i_outstanding_local_handlers: HashMap::new(),
            i_outstanding_local_requests: self.i_outstanding_local_requests.clone(),
            i_outstanding_requests_from_server: self.i_outstanding_requests_from_server.clone(),
            i_is_connected: self.i_is_connected,
            i_status: self.i_status.clone(),
            i_open_files: self.i_open_files.clone(),
            i_errors: self.i_errors.clone(),
            i_config: self.i_config.clone(),
            i_flowconfig: self.i_flowconfig.clone(),
            i_file_options: self.i_file_options.clone(),
        }
    }
}

#[derive(Clone)]
pub struct DisconnectedEnv {
    pub d_ienv: InitializedEnv,
    pub d_autostart: bool,
    pub d_server_status: Option<(server_status::Status, file_watcher_status::Status)>,
}

pub struct ConnectedEnv {
    pub c_ienv: InitializedEnv,
    pub c_conn: ServerConn,
    pub c_server_status: (server_status::Status, Option<file_watcher_status::Status>),
    pub c_recent_summaries: Vec<(f64, lsp_prot::TelemetryFromServer)>,
    pub c_about_to_exit_code: Option<FlowExitStatus>,
    pub c_is_rechecking: bool,
    pub c_lazy_stats: Option<server_prot::response::LazyStats>,
    pub c_outstanding_requests_to_server: HashSet<LspId>,
}

pub enum ServerState {
    Disconnected(DisconnectedEnv),
    Connected(ConnectedEnv),
}

pub enum State {
    PreInit(ConnectParams),
    Initialized(ServerState),
    PostShutdown,
}

#[derive(Debug)]
pub struct RemoteExceptionData {
    pub message: String,
    pub stack: String,
}

#[derive(Debug)]
pub enum FlowLspError {
    ClientFatalConnectionException(RemoteExceptionData),
    ClientRecoverableConnectionException(RemoteExceptionData),
    ServerFatalConnectionException(RemoteExceptionData),
    RequestLspException {
        id: LspId,
        error: flow_server_env::lsp_mapper::lsp_error::T,
    },
    LspException(flow_server_env::lsp_mapper::lsp_error::T),
    ChangedFileNotOpen(DocumentUri),
}

impl fmt::Display for FlowLspError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlowLspError::ClientFatalConnectionException(e) => {
                write!(f, "Client fatal connection exception: {}", e.message)
            }
            FlowLspError::ClientRecoverableConnectionException(e) => {
                write!(f, "Client recoverable connection exception: {}", e.message)
            }
            FlowLspError::ServerFatalConnectionException(e) => {
                write!(f, "Server fatal connection exception: {}", e.message)
            }
            FlowLspError::RequestLspException { error, .. } | FlowLspError::LspException(error) => {
                write!(f, "{}", error.message)
            }
            FlowLspError::ChangedFileNotOpen(uri) => {
                write!(f, "Changed file not open: {}", uri)
            }
        }
    }
}

impl std::error::Error for FlowLspError {}

#[derive(Clone)]
pub enum Event {
    ServerMessage(lsp_prot::MessageFromServer),
    ClientMessage(lsp_prot::LspMessage, Metadata),
    Tick,
}

pub use flow_config::FlowConfig;

pub type FileOptions = flow_common::files::FileOptions;

pub struct LspHandler {
    pub on_response:
        Box<dyn FnOnce(lsp_mapper::LspResult, &mut ServerState) -> Result<(), FlowLspError>>,
    pub on_error: Box<
        dyn FnOnce(lsp_mapper::lsp_error::T, String, &mut ServerState) -> Result<(), FlowLspError>,
    >,
}

#[derive(Debug, Clone)]
pub struct JsonrpcMessage {
    pub id: Option<LspId>,
    pub json: serde_json::Value,
    pub timestamp: f64,
    pub method_: String,
}

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
                        Ok(jsonrpc_parse_message(&json, timestamp))
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

fn jsonrpc_parse_message(json: &serde_json::Value, timestamp: f64) -> JsonrpcMessage {
    let id = json.get("id").and_then(|v| match v {
        serde_json::Value::Number(n) => {
            Some(NumberOrString::Number(n.as_i64().unwrap_or(0) as i32))
        }
        serde_json::Value::String(s) => Some(NumberOrString::String(s.clone())),
        _ => None,
    });
    let method_ = json
        .get("method")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    JsonrpcMessage {
        id,
        json: json.clone(),
        timestamp,
        method_,
    }
}

fn annotate_request_parse_error(message: &JsonrpcMessage, error: FlowLspError) -> FlowLspError {
    match (message.id.clone(), message.method_.is_empty(), error) {
        (Some(id), false, FlowLspError::LspException(error)) => {
            FlowLspError::RequestLspException { id, error }
        }
        (_, _, error) => error,
    }
}

pub type FileInput = flow_server_utils::file_input::FileInput;

pub type RageItem = lsp_mapper::rage::RageItem;

pub struct TrackEffect {
    pub changed_live_uri: Option<DocumentUri>,
}

pub enum LogNeeded {
    LogNeeded(Metadata),
    LogDeferred,
    LogNotNeeded,
}

#[derive(Debug, Clone, Default, serde::Serialize)]
pub struct PersistentDelay {
    pub init_duration: f64,
    pub command_count: i32,
    pub command_duration: f64,
    pub command_worst: Option<String>,
    pub command_worst_duration: Option<f64>,
    pub recheck_count: i32,
    pub recheck_dependent_files: i32,
    pub recheck_changed_files: i32,
    pub recheck_duration: f64,
    pub recheck_worst_duration: Option<f64>,
    pub recheck_worst_dependent_file_count: Option<i32>,
    pub recheck_worst_changed_file_count: Option<i32>,
    pub recheck_worst_cycle_leader: Option<String>,
    pub recheck_worst_cycle_size: Option<i32>,
}

pub type InteractionState = crate::lsp_interaction::State;
pub type InteractionServerStatus = crate::lsp_interaction::ServerStatus;
pub type InteractionBufferStatus = crate::lsp_interaction::BufferStatus;
pub type InteractionTrigger = crate::lsp_interaction::Trigger;
pub type InteractionId = crate::lsp_interaction::Id;
pub type InteractionUx = crate::lsp_interaction::Ux;

#[derive(Debug, Clone, PartialEq)]
pub enum ConnectError {
    ServerMissing,
    ServerSocketMissing,
    BuildIdMismatch(BuildIdMismatchKind),
    ServerBusy(ServerBusyKind),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuildIdMismatchKind {
    ServerExited,
    ClientShouldError { server_version: String },
}

#[derive(Debug, Clone, PartialEq)]
pub enum ServerBusyKind {
    NotResponding,
    TooManyClients,
    FailOnInit((server_status::Status, file_watcher_status::Status)),
}

impl fmt::Display for ConnectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConnectError::ServerMissing => write!(f, "Server missing"),
            ConnectError::ServerSocketMissing => write!(f, "Server socket missing"),
            ConnectError::BuildIdMismatch(BuildIdMismatchKind::ServerExited) => {
                write!(f, "Server exited due to version mismatch")
            }
            ConnectError::BuildIdMismatch(BuildIdMismatchKind::ClientShouldError {
                server_version,
            }) => {
                write!(f, "Server is wrong version ({server_version})")
            }
            ConnectError::ServerBusy(ServerBusyKind::NotResponding) => {
                write!(f, "Server busy: not responding")
            }
            ConnectError::ServerBusy(ServerBusyKind::TooManyClients) => {
                write!(f, "Server busy: too many clients")
            }
            ConnectError::ServerBusy(ServerBusyKind::FailOnInit((
                server_status,
                watcher_status,
            ))) => {
                write!(
                    f,
                    "Server busy during init ({server_status:?}, {watcher_status:?})"
                )
            }
        }
    }
}

pub(crate) fn request_name_to_string(request: &lsp_mapper::LspRequest) -> &'static str {
    match request {
        lsp_mapper::LspRequest::ShowMessageRequestRequest(_) => "window/showMessageRequest",
        lsp_mapper::LspRequest::ShowStatusRequest(_) => "window/showStatus",
        lsp_mapper::LspRequest::InitializeRequest(_) => "initialize",
        lsp_mapper::LspRequest::RegisterCapabilityRequest(_) => "client/registerCapability",
        lsp_mapper::LspRequest::ShutdownRequest => "shutdown",
        lsp_mapper::LspRequest::CodeLensResolveRequest(_) => "codeLens/resolve",
        lsp_mapper::LspRequest::HoverRequest(_) => "textDocument/hover",
        lsp_mapper::LspRequest::CodeActionRequest(_) => "textDocument/codeAction",
        lsp_mapper::LspRequest::CompletionRequest(_) => "textDocument/completion",
        lsp_mapper::LspRequest::CompletionItemResolveRequest(_) => "completionItem/resolve",
        lsp_mapper::LspRequest::ConfigurationRequest(_) => "workspace/configuration",
        lsp_mapper::LspRequest::SelectionRangeRequest(_) => "textDocument/selectionRange",
        lsp_mapper::LspRequest::SignatureHelpRequest(_) => "textDocument/signatureHelp",
        lsp_mapper::LspRequest::TextDocumentDiagnosticsRequest(_) => "textDocument/diagnostics",
        lsp_mapper::LspRequest::DefinitionRequest(_) => "textDocument/definition",
        lsp_mapper::LspRequest::TypeDefinitionRequest(_) => "textDocument/typeDefinition",
        lsp_mapper::LspRequest::WorkspaceSymbolRequest(_) => "workspace/symbol",
        lsp_mapper::LspRequest::DocumentSymbolRequest(_) => "textDocument/documentSymbol",
        lsp_mapper::LspRequest::FindReferencesRequest(_) => "textDocument/references",
        lsp_mapper::LspRequest::DocumentHighlightRequest(_) => "textDocument/documentHighlight",
        lsp_mapper::LspRequest::TypeCoverageRequest(_) => "textDocument/typeCoverage",
        lsp_mapper::LspRequest::DocumentFormattingRequest(_) => "textDocument/formatting",
        lsp_mapper::LspRequest::DocumentRangeFormattingRequest(_) => "textDocument/rangeFormatting",
        lsp_mapper::LspRequest::DocumentOnTypeFormattingRequest(_) => {
            "textDocument/onTypeFormatting"
        }
        lsp_mapper::LspRequest::RageRequest => "telemetry/rage",
        lsp_mapper::LspRequest::PingRequest => "telemetry/ping",
        lsp_mapper::LspRequest::PrepareRenameRequest(_) => "textDocument/prepareRename",
        lsp_mapper::LspRequest::RenameRequest(_) => "textDocument/rename",
        lsp_mapper::LspRequest::DocumentCodeLensRequest(_) => "textDocument/codeLens",
        lsp_mapper::LspRequest::ExecuteCommandRequest(_) => "workspace/executeCommand",
        lsp_mapper::LspRequest::ApplyWorkspaceEditRequest(_) => "workspace/applyEdit",
        lsp_mapper::LspRequest::AutoCloseJsxRequest(_) => "flow/autoCloseJsx",
        lsp_mapper::LspRequest::PrepareDocumentPasteRequest(_) => "flow/prepareDocumentPaste",
        lsp_mapper::LspRequest::ProvideDocumentPasteRequest(_) => "flow/provideDocumentPasteEdits",
        lsp_mapper::LspRequest::LinkedEditingRangeRequest(_) => "textDocument/linkedEditingRange",
        lsp_mapper::LspRequest::WillRenameFilesRequest(_) => "workspace/willRenameFiles",
        lsp_mapper::LspRequest::RenameFileImportsRequest(_) => "flow/renameFileImports",
        lsp_mapper::LspRequest::LLMContextRequest(_) => "llm/context",
        lsp_mapper::LspRequest::UnknownRequest(_, _) => "unknown",
    }
}

fn notification_name_to_string(notification: &lsp_mapper::LspNotification) -> &str {
    match notification {
        lsp_mapper::LspNotification::ExitNotification => "exit",
        lsp_mapper::LspNotification::CancelRequestNotification(_) => "$/cancelRequest",
        lsp_mapper::LspNotification::PublishDiagnosticsNotification(_) => {
            "textDocument/publishDiagnostics"
        }
        lsp_mapper::LspNotification::DidOpenNotification(_) => "textDocument/didOpen",
        lsp_mapper::LspNotification::DidCloseNotification(_) => "textDocument/didClose",
        lsp_mapper::LspNotification::DidSaveNotification(_) => "textDocument/didSave",
        lsp_mapper::LspNotification::DidChangeNotification(_) => "textDocument/didChange",
        lsp_mapper::LspNotification::DidChangeConfigurationNotification(_) => {
            "workspace/didChangeConfiguration"
        }
        lsp_mapper::LspNotification::DidChangeWatchedFilesNotification(_) => {
            "workspace/didChangeWatchedFiles"
        }
        lsp_mapper::LspNotification::TelemetryNotification(_) => "telemetry/event",
        lsp_mapper::LspNotification::LogMessageNotification(_) => "window/logMessage",
        lsp_mapper::LspNotification::ShowMessageNotification(_) => "window/showMessage",
        lsp_mapper::LspNotification::ConnectionStatusNotification(_) => {
            "telemetry/connectionStatus"
        }
        lsp_mapper::LspNotification::InitializedNotification => "initialized",
        lsp_mapper::LspNotification::SetTraceNotification => "$/setTraceNotification",
        lsp_mapper::LspNotification::LogTraceNotification => "$/logTraceNotification",
        lsp_mapper::LspNotification::UnknownNotification(method_, _) => method_,
    }
}

fn result_name_to_string(result: &lsp_mapper::LspResult) -> &str {
    match result {
        lsp_mapper::LspResult::ShowMessageRequestResult(_) => "window/showMessageRequest",
        lsp_mapper::LspResult::ShowStatusResult(_) => "window/showStatus",
        lsp_mapper::LspResult::InitializeResult(_) => "initialize",
        lsp_mapper::LspResult::ShutdownResult => "shutdown",
        lsp_mapper::LspResult::CodeLensResolveResult(_) => "codeLens/resolve",
        lsp_mapper::LspResult::HoverResult(_) => "textDocument/hover",
        lsp_mapper::LspResult::CodeActionResult(_) => "textDocument/codeAction",
        lsp_mapper::LspResult::CompletionResult(_) => "textDocument/completion",
        lsp_mapper::LspResult::CompletionItemResolveResult(_) => "completionItem/resolve",
        lsp_mapper::LspResult::ConfigurationResult(_) => "workspace/configuration",
        lsp_mapper::LspResult::SelectionRangeResult(_) => "textDocument/selectionRange",
        lsp_mapper::LspResult::SignatureHelpResult(_) => "textDocument/signatureHelp",
        lsp_mapper::LspResult::TextDocumentDiagnosticsResult(_) => "textDocument/diagnostics",
        lsp_mapper::LspResult::DefinitionResult(_) => "textDocument/definition",
        lsp_mapper::LspResult::TypeDefinitionResult(_) => "textDocument/typeDefinition",
        lsp_mapper::LspResult::WorkspaceSymbolResult(_) => "workspace/symbol",
        lsp_mapper::LspResult::DocumentSymbolResult(_) => "textDocument/documentSymbol",
        lsp_mapper::LspResult::FindReferencesResult(_) => "textDocument/references",
        lsp_mapper::LspResult::GoToImplementationResult(_) => "textDocument/implementation",
        lsp_mapper::LspResult::DocumentHighlightResult(_) => "textDocument/documentHighlight",
        lsp_mapper::LspResult::TypeCoverageResult(_) => "textDocument/typeCoverage",
        lsp_mapper::LspResult::DocumentFormattingResult(_) => "textDocument/formatting",
        lsp_mapper::LspResult::DocumentRangeFormattingResult(_) => "textDocument/rangeFormatting",
        lsp_mapper::LspResult::DocumentOnTypeFormattingResult(_) => "textDocument/onTypeFormatting",
        lsp_mapper::LspResult::RageResult(_) => "telemetry/rage",
        lsp_mapper::LspResult::PingResult(_) => "telemetry/ping",
        lsp_mapper::LspResult::PrepareRenameResult(_) => "textDocument/prepareRename",
        lsp_mapper::LspResult::RenameResult(_) => "textDocument/rename",
        lsp_mapper::LspResult::DocumentCodeLensResult(_) => "textDocument/codeLens",
        lsp_mapper::LspResult::ExecuteCommandResult(_) => "workspace/executeCommand",
        lsp_mapper::LspResult::WillRenameFilesResult(_) => "workspace/willRenameFiles",
        lsp_mapper::LspResult::ApplyWorkspaceEditResult(_) => "workspace/applyEdit",
        lsp_mapper::LspResult::RegisterCapabilityResult => "client/registerCapability",
        lsp_mapper::LspResult::AutoCloseJsxResult(_) => "flow/autoCloseJsx",
        lsp_mapper::LspResult::PrepareDocumentPasteResult(_) => "flow/prepareDocumentPaste",
        lsp_mapper::LspResult::ProvideDocumentPasteResult(_) => "flow/provideDocumentPasteEdits",
        lsp_mapper::LspResult::LinkedEditingRangeResult(_) => "textDocument/linkedEditingRange",
        lsp_mapper::LspResult::RenameFileImportsResult(_) => "flow/renameFileImports",
        lsp_mapper::LspResult::LLMContextResult(_) => "llm/context",
        lsp_mapper::LspResult::ErrorResult(_, _) => "unknown",
    }
}

fn print_id(id: &LspId) -> serde_json::Value {
    match id {
        NumberOrString::Number(n) => serde_json::Value::Number((*n).into()),
        NumberOrString::String(s) => serde_json::Value::String(s.clone()),
    }
}

fn print_error(
    include_error_stack_trace: bool,
    error: &lsp_mapper::lsp_error::T,
    stack: &str,
) -> serde_json::Value {
    use lsp_mapper::lsp_error::Code;
    let code = match error.code {
        Code::ParseError => -32700,
        Code::InvalidRequest => -32600,
        Code::MethodNotFound => -32601,
        Code::InvalidParams => -32602,
        Code::InternalError => -32603,
        Code::ServerErrorStart => -32099,
        Code::ServerErrorEnd => -32000,
        Code::ServerNotInitialized => -32002,
        Code::UnknownErrorCode => -32001,
        Code::RequestCancelled => -32800,
        Code::ContentModified => -32801,
    };
    let mut obj = serde_json::Map::new();
    obj.insert("code".to_string(), serde_json::Value::Number(code.into()));
    obj.insert(
        "message".to_string(),
        serde_json::Value::String(error.message.clone()),
    );
    let data = match (&error.data, include_error_stack_trace, stack) {
        (None, true, s) if !s.is_empty() => Some(serde_json::json!({ "stack": s })),
        (Some(d), true, s) if !s.is_empty() => {
            let mut merged = serde_json::Map::new();
            merged.insert(
                "stack".to_string(),
                serde_json::Value::String(s.to_string()),
            );
            if let serde_json::Value::Object(m) = d {
                for (k, v) in m {
                    merged.insert(k.clone(), v.clone());
                }
            }
            Some(serde_json::Value::Object(merged))
        }
        (Some(d), _, _) => Some(d.clone()),
        _ => None,
    };
    if let Some(d) = data {
        obj.insert("data".to_string(), d);
    }
    serde_json::Value::Object(obj)
}

fn parse_error(error: &serde_json::Value) -> lsp_mapper::lsp_error::T {
    use lsp_mapper::lsp_error::Code;
    let code = error.get("code").and_then(|v| v.as_i64()).unwrap_or(-32603);
    let code = match code {
        -32700 => Code::ParseError,
        -32600 => Code::InvalidRequest,
        -32601 => Code::MethodNotFound,
        -32602 => Code::InvalidParams,
        -32603 => Code::InternalError,
        -32099 => Code::ServerErrorStart,
        -32000 => Code::ServerErrorEnd,
        -32002 => Code::ServerNotInitialized,
        -32001 => Code::UnknownErrorCode,
        -32800 => Code::RequestCancelled,
        -32801 => Code::ContentModified,
        _ => Code::UnknownErrorCode,
    };
    let message = error
        .get("message")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let data = error.get("data").cloned();
    lsp_mapper::lsp_error::T {
        code,
        message,
        data,
    }
}

fn to_json<T: serde::Serialize>(val: &T) -> serde_json::Value {
    serde_json::to_value(val).unwrap_or_default()
}

fn from_json<T: serde::de::DeserializeOwned>(
    val: Option<&serde_json::Value>,
) -> Result<T, FlowLspError> {
    match val {
        Some(v) => serde_json::from_value(v.clone())
            .map_err(|e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e))),
        None => serde_json::from_value(serde_json::Value::Null)
            .map_err(|e| parse_error_exception(format!("Missing LSP params: {}", e))),
    }
}

fn parse_lsp_request(
    method_: &str,
    params: Option<&serde_json::Value>,
) -> Result<lsp_mapper::LspRequest, FlowLspError> {
    Ok(match method_ {
        "initialize" => {
            let mut initialize_params: lsp_types::InitializeParams = from_json(params)?;
            if let Some(status) = params
                .and_then(|params| params.get("capabilities"))
                .and_then(|capabilities| capabilities.get("window"))
                .and_then(|window| window.get("status"))
                .and_then(|status| status.as_bool())
            {
                match initialize_params.capabilities.experimental.as_mut() {
                    Some(serde_json::Value::Object(map)) => {
                        map.insert("window/status".to_string(), serde_json::Value::Bool(status));
                    }
                    _ => {
                        initialize_params.capabilities.experimental =
                            Some(serde_json::json!({ "window/status": status }));
                    }
                }
            }
            if let Some(connection_status) = params
                .and_then(|params| params.get("capabilities"))
                .and_then(|capabilities| capabilities.get("telemetry"))
                .and_then(|telemetry| telemetry.get("connectionStatus"))
                .and_then(|status| status.as_bool())
            {
                match initialize_params.capabilities.experimental.as_mut() {
                    Some(serde_json::Value::Object(map)) => {
                        map.insert(
                            "telemetry/connectionStatus".to_string(),
                            serde_json::Value::Bool(connection_status),
                        );
                    }
                    _ => {
                        initialize_params.capabilities.experimental = Some(serde_json::json!({
                            "telemetry/connectionStatus": connection_status
                        }));
                    }
                }
            }
            lsp_mapper::LspRequest::InitializeRequest(initialize_params)
        }
        "shutdown" => lsp_mapper::LspRequest::ShutdownRequest,
        "codeLens/resolve" => lsp_mapper::LspRequest::CodeLensResolveRequest(from_json(params)?),
        "textDocument/hover" => lsp_mapper::LspRequest::HoverRequest(from_json(params)?),
        "textDocument/codeAction" => lsp_mapper::LspRequest::CodeActionRequest(from_json(params)?),
        "textDocument/completion" => lsp_mapper::LspRequest::CompletionRequest(from_json(params)?),
        "textDocument/definition" => lsp_mapper::LspRequest::DefinitionRequest(from_json(params)?),
        "workspace/symbol" => lsp_mapper::LspRequest::WorkspaceSymbolRequest(from_json(params)?),
        "textDocument/documentSymbol" => {
            lsp_mapper::LspRequest::DocumentSymbolRequest(from_json(params)?)
        }
        "textDocument/references" => {
            lsp_mapper::LspRequest::FindReferencesRequest(from_json(params)?)
        }
        "textDocument/prepareRename" => {
            lsp_mapper::LspRequest::PrepareRenameRequest(from_json(params)?)
        }
        "textDocument/rename" => lsp_mapper::LspRequest::RenameRequest(from_json(params)?),
        "textDocument/documentHighlight" => {
            lsp_mapper::LspRequest::DocumentHighlightRequest(from_json(params)?)
        }
        "textDocument/formatting" => {
            lsp_mapper::LspRequest::DocumentFormattingRequest(from_json(params)?)
        }
        "textDocument/rangeFormatting" => {
            lsp_mapper::LspRequest::DocumentRangeFormattingRequest(from_json(params)?)
        }
        "textDocument/onTypeFormatting" => {
            lsp_mapper::LspRequest::DocumentOnTypeFormattingRequest(from_json(params)?)
        }
        "textDocument/codeLens" => {
            lsp_mapper::LspRequest::DocumentCodeLensRequest(from_json(params)?)
        }
        "textDocument/selectionRange" => {
            lsp_mapper::LspRequest::SelectionRangeRequest(from_json(params)?)
        }
        "textDocument/signatureHelp" => {
            lsp_mapper::LspRequest::SignatureHelpRequest(from_json(params)?)
        }
        "textDocument/linkedEditingRange" => {
            lsp_mapper::LspRequest::LinkedEditingRangeRequest(from_json(params)?)
        }
        "telemetry/rage" => lsp_mapper::LspRequest::RageRequest,
        "telemetry/ping" => lsp_mapper::LspRequest::PingRequest,
        "workspace/executeCommand" => {
            let mut params: lsp_types::ExecuteCommandParams = from_json(params)?;
            params.command = parse_command_name(&params.command);
            lsp_mapper::LspRequest::ExecuteCommandRequest(params)
        }
        _ => lsp_mapper::LspRequest::UnknownRequest(method_.to_string(), params.cloned()),
    })
}

fn parse_lsp_notification(
    method_: &str,
    params: Option<&serde_json::Value>,
) -> Result<lsp_mapper::LspNotification, FlowLspError> {
    Ok(match method_ {
        "$/cancelRequest" => {
            lsp_mapper::LspNotification::CancelRequestNotification(from_json(params)?)
        }
        "$/setTraceNotification" => lsp_mapper::LspNotification::SetTraceNotification,
        "$/logTraceNotification" => lsp_mapper::LspNotification::LogTraceNotification,
        "initialized" => lsp_mapper::LspNotification::InitializedNotification,
        "exit" => lsp_mapper::LspNotification::ExitNotification,
        "textDocument/didOpen" => {
            lsp_mapper::LspNotification::DidOpenNotification(from_json(params)?)
        }
        "textDocument/didClose" => {
            lsp_mapper::LspNotification::DidCloseNotification(from_json(params)?)
        }
        "textDocument/didSave" => {
            lsp_mapper::LspNotification::DidSaveNotification(from_json(params)?)
        }
        "textDocument/didChange" => {
            lsp_mapper::LspNotification::DidChangeNotification(from_json(params)?)
        }
        "workspace/didChangeConfiguration" => {
            lsp_mapper::LspNotification::DidChangeConfigurationNotification(from_json(params)?)
        }
        "workspace/didChangeWatchedFiles" => {
            lsp_mapper::LspNotification::DidChangeWatchedFilesNotification(from_json(params)?)
        }
        _ => lsp_mapper::LspNotification::UnknownNotification(method_.to_string(), params.cloned()),
    })
}

fn parse_lsp_result(
    request: &lsp_mapper::LspRequest,
    result: &serde_json::Value,
) -> Result<lsp_mapper::LspResult, FlowLspError> {
    let method_ = request_name_to_string(request);
    Ok(match request {
        lsp_mapper::LspRequest::ShowMessageRequestRequest(_) => {
            lsp_mapper::LspResult::ShowMessageRequestResult(
                result.get("title").and_then(|t| t.as_str()).map(|title| {
                    lsp_types::MessageActionItem {
                        title: title.to_string(),
                        properties: Default::default(),
                    }
                }),
            )
        }
        lsp_mapper::LspRequest::ShowStatusRequest(_) => lsp_mapper::LspResult::ShowStatusResult(
            result.get("title").and_then(|t| t.as_str()).map(|title| {
                lsp_types::MessageActionItem {
                    title: title.to_string(),
                    properties: Default::default(),
                }
            }),
        ),
        lsp_mapper::LspRequest::ApplyWorkspaceEditRequest(_) => {
            lsp_mapper::LspResult::ApplyWorkspaceEditResult(from_json(Some(result))?)
        }
        lsp_mapper::LspRequest::ConfigurationRequest(_) => {
            let items = match result {
                serde_json::Value::Array(items) => items.clone(),
                _ => vec![],
            };
            lsp_mapper::LspResult::ConfigurationResult(items)
        }
        lsp_mapper::LspRequest::RegisterCapabilityRequest(_) => {
            lsp_mapper::LspResult::RegisterCapabilityResult
        }
        _ => {
            return Err(parse_error_exception(format!(
                "Don't know how to parse LSP response {}",
                method_
            )));
        }
    })
}

pub fn lsp_fmt_parse_lsp(
    json: &serde_json::Value,
    outstanding: &dyn Fn(&LspId) -> Result<lsp_mapper::LspRequest, FlowLspError>,
) -> Result<lsp_prot::LspMessage, FlowLspError> {
    let id = match json.get("id") {
        Some(id) => Some(lsp_fmt_parse_id(id)?),
        None => None,
    };
    let method_opt = json.get("method").and_then(|v| v.as_str());
    let params = json.get("params");
    let result = json.get("result");
    let error = json.get("error");
    Ok(match (id, method_opt, result, error) {
        (None, Some(method_), _, _) => {
            lsp_prot::LspMessage::NotificationMessage(parse_lsp_notification(method_, params)?)
        }
        (Some(id), Some(method_), _, _) => {
            lsp_prot::LspMessage::RequestMessage(id, parse_lsp_request(method_, params)?)
        }
        (Some(id), _, Some(result), _) => {
            let request = outstanding(&id)?;
            lsp_prot::LspMessage::ResponseMessage(id, parse_lsp_result(&request, result)?)
        }
        (Some(id), _, _, Some(error)) => lsp_prot::LspMessage::ResponseMessage(
            id,
            lsp_mapper::LspResult::ErrorResult(parse_error(error), String::new()),
        ),
        _ => return Err(parse_error_exception("Not JsonRPC")),
    })
}

fn print_position(position: &lsp_types::Position) -> serde_json::Value {
    serde_json::json!({
        "line": position.line,
        "character": position.character,
    })
}

fn print_range(range: &lsp_types::Range) -> serde_json::Value {
    serde_json::json!({
        "start": print_position(&range.start),
        "end": print_position(&range.end),
    })
}

fn text_edit_fmt_to_json(edit: &lsp_types::TextEdit) -> serde_json::Value {
    let lsp_types::TextEdit { range, new_text } = edit;
    serde_json::json!({
        "range": print_range(range),
        "newText": new_text,
    })
}

fn insert_replace_edit_fmt_to_json(edit: &lsp_types::InsertReplaceEdit) -> serde_json::Value {
    let lsp_types::InsertReplaceEdit {
        new_text,
        insert,
        replace,
    } = edit;
    serde_json::json!({
        "newText": new_text,
        "insert": print_range(insert),
        "replace": print_range(replace),
    })
}

fn print_command_name(key: &str, name: &str) -> String {
    format!("{}:{}", name, key)
}

fn parse_command_name(name: &str) -> String {
    match name.split_once(':') {
        Some((name, _)) => name.to_string(),
        None => name.to_string(),
    }
}

fn key_command(key: &str, mut command: lsp_types::Command) -> lsp_types::Command {
    command.command = print_command_name(key, &command.command);
    command
}

fn print_command(key: &str, command: &lsp_types::Command) -> serde_json::Value {
    let name = print_command_name(key, &command.command);
    serde_json::json!({
        "title": command.title,
        "command": name,
        "arguments": command.arguments.clone().unwrap_or_default(),
    })
}

fn completion_item_label_details_fmt_to_json(
    details: &lsp_types::CompletionItemLabelDetails,
) -> serde_json::Value {
    let lsp_types::CompletionItemLabelDetails {
        description,
        detail,
    } = details;
    object_opt(vec![
        (
            "description",
            description
                .as_ref()
                .map(|x| serde_json::Value::String(x.clone())),
        ),
        (
            "detail",
            detail
                .as_ref()
                .map(|x| serde_json::Value::String(x.clone())),
        ),
    ])
}

fn string_of_marked_string(acc: String, marked: &lsp_types::MarkedString) -> String {
    match marked {
        lsp_types::MarkedString::LanguageString(ls) => {
            format!("{}```{}\n{}\n```\n", acc, ls.language, ls.value)
        }
        lsp_types::MarkedString::String(s) => format!("{}{}\n", acc, s),
    }
}

fn object_opt(entries: Vec<(&str, Option<serde_json::Value>)>) -> serde_json::Value {
    let mut map = serde_json::Map::new();
    for (key, value) in entries {
        if let Some(value) = value {
            map.insert(key.to_string(), value);
        }
    }
    serde_json::Value::Object(map)
}

pub fn lsp_fmt_completion_item_fmt_to_json(
    key: &str,
    item: &lsp_types::CompletionItem,
) -> serde_json::Value {
    object_opt(vec![
        ("label", Some(serde_json::Value::String(item.label.clone()))),
        (
            "labelDetails",
            item.label_details
                .as_ref()
                .map(completion_item_label_details_fmt_to_json),
        ),
        (
            "kind",
            item.kind.map(|x| {
                serde_json::to_value(x).expect("CompletionItemKind serializes as i32 — infallible")
            }),
        ),
        (
            "detail",
            item.detail
                .as_ref()
                .map(|s| serde_json::Value::String(s.clone())),
        ),
        (
            "documentation",
            item.documentation.as_ref().map(|doc| {
                let value = match doc {
                    lsp_types::Documentation::String(s) => string_of_marked_string(
                        String::new(),
                        &lsp_types::MarkedString::String(s.clone()),
                    ),
                    lsp_types::Documentation::MarkupContent(mc) => string_of_marked_string(
                        String::new(),
                        &lsp_types::MarkedString::String(mc.value.clone()),
                    ),
                };
                serde_json::json!({
                    "kind": "markdown",
                    "value": value.trim(),
                })
            }),
        ),
        (
            "tags",
            item.tags.as_ref().map(|tags| {
                serde_json::Value::Array(
                    tags.iter()
                        .map(|tag| {
                            serde_json::to_value(tag)
                                .expect("CompletionItemTag serializes as i32 — infallible")
                        })
                        .collect(),
                )
            }),
        ),
        (
            "preselect",
            if item.preselect.unwrap_or(false) {
                Some(serde_json::Value::Bool(true))
            } else {
                None
            },
        ),
        (
            "sortText",
            item.sort_text
                .as_ref()
                .map(|s| serde_json::Value::String(s.clone())),
        ),
        (
            "filterText",
            item.filter_text
                .as_ref()
                .map(|s| serde_json::Value::String(s.clone())),
        ),
        (
            "insertText",
            item.insert_text
                .as_ref()
                .map(|s| serde_json::Value::String(s.clone())),
        ),
        (
            "insertTextFormat",
            item.insert_text_format.map(|x| {
                serde_json::to_value(x).expect("InsertTextFormat serializes as i32 — infallible")
            }),
        ),
        (
            "textEdit",
            item.text_edit.as_ref().map(|te| match te {
                lsp_types::CompletionTextEdit::Edit(edit) => text_edit_fmt_to_json(edit),
                lsp_types::CompletionTextEdit::InsertAndReplace(edit) => {
                    insert_replace_edit_fmt_to_json(edit)
                }
            }),
        ),
        (
            "additionalTextEdits",
            match item.additional_text_edits.as_deref() {
                None | Some([]) => None,
                Some(l) => Some(serde_json::Value::Array(
                    l.iter().map(text_edit_fmt_to_json).collect(),
                )),
            },
        ),
        (
            "command",
            item.command.as_ref().map(|c| print_command(key, c)),
        ),
        ("data", item.data.clone()),
    ])
}

fn key_code_action_or_command(
    key: &str,
    code_action_or_command: lsp_types::CodeActionOrCommand,
) -> lsp_types::CodeActionOrCommand {
    match code_action_or_command {
        lsp_types::CodeActionOrCommand::Command(command) => {
            lsp_types::CodeActionOrCommand::Command(key_command(key, command))
        }
        lsp_types::CodeActionOrCommand::CodeAction(mut code_action) => {
            if let Some(command) = code_action.command.take() {
                code_action.command = Some(key_command(key, command));
            }
            lsp_types::CodeActionOrCommand::CodeAction(code_action)
        }
    }
}

pub fn lsp_fmt_print_code_action_result(
    key: &str,
    result: Vec<lsp_types::CodeActionOrCommand>,
) -> serde_json::Value {
    to_json(
        &result
            .into_iter()
            .map(|item| key_code_action_or_command(key, item))
            .collect::<Vec<_>>(),
    )
}

fn key_completion_item(
    key: &str,
    mut completion_item: lsp_types::CompletionItem,
) -> lsp_types::CompletionItem {
    if let Some(command) = completion_item.command.take() {
        completion_item.command = Some(key_command(key, command));
    }
    completion_item
}

fn key_completion_response(
    key: &str,
    completion_response: lsp_types::CompletionResponse,
) -> lsp_types::CompletionResponse {
    match completion_response {
        lsp_types::CompletionResponse::Array(items) => lsp_types::CompletionResponse::Array(
            items
                .into_iter()
                .map(|item| key_completion_item(key, item))
                .collect(),
        ),
        lsp_types::CompletionResponse::List(mut completion_list) => {
            completion_list.items = completion_list
                .items
                .into_iter()
                .map(|item| key_completion_item(key, item))
                .collect();
            lsp_types::CompletionResponse::List(completion_list)
        }
    }
}

fn key_code_lens(key: &str, mut code_lens: lsp_types::CodeLens) -> lsp_types::CodeLens {
    if let Some(command) = code_lens.command.take() {
        code_lens.command = Some(key_command(key, command));
    }
    code_lens
}

fn key_initialize_result(
    key: &str,
    mut initialize_result: lsp_types::InitializeResult,
) -> lsp_types::InitializeResult {
    if let Some(execute_command_provider) = initialize_result
        .capabilities
        .execute_command_provider
        .as_mut()
    {
        execute_command_provider.commands = execute_command_provider
            .commands
            .iter()
            .map(|command| print_command_name(key, command))
            .collect();
    }
    initialize_result
}

fn initialize_result_to_json(
    key: &str,
    initialize_result: lsp_types::InitializeResult,
) -> serde_json::Value {
    let mut json = to_json(&key_initialize_result(key, initialize_result));
    if let Some(capabilities) = json.get_mut("capabilities").and_then(|v| v.as_object_mut()) {
        capabilities.insert(
            "typeCoverageProvider".to_string(),
            serde_json::Value::Bool(true),
        );
        capabilities.insert("rageProvider".to_string(), serde_json::Value::Bool(true));
    }
    json
}

pub fn lsp_fmt_print_lsp_response(
    key: &str,
    id: &LspId,
    result: &lsp_mapper::LspResult,
) -> serde_json::Value {
    let _method_ = result_name_to_string(result);
    let json = match result {
        lsp_mapper::LspResult::InitializeResult(r) => initialize_result_to_json(key, r.clone()),
        lsp_mapper::LspResult::ShutdownResult => serde_json::Value::Null,
        lsp_mapper::LspResult::CodeLensResolveResult(r) => to_json(&key_code_lens(key, r.clone())),
        lsp_mapper::LspResult::HoverResult(r) => to_json(r),
        lsp_mapper::LspResult::CodeActionResult(r) => {
            lsp_fmt_print_code_action_result(key, r.clone())
        }
        lsp_mapper::LspResult::CompletionResult(r) => {
            to_json(&key_completion_response(key, r.clone()))
        }
        lsp_mapper::LspResult::CompletionItemResolveResult(r) => {
            to_json(&key_completion_item(key, r.clone()))
        }
        lsp_mapper::LspResult::ConfigurationResult(r) => to_json(r),
        lsp_mapper::LspResult::SelectionRangeResult(r) => to_json(r),
        lsp_mapper::LspResult::SignatureHelpResult(r) => to_json(r),
        lsp_mapper::LspResult::TextDocumentDiagnosticsResult(r) => to_json(r),
        lsp_mapper::LspResult::DefinitionResult(r) => to_json(r),
        lsp_mapper::LspResult::TypeDefinitionResult(r) => to_json(r),
        lsp_mapper::LspResult::WorkspaceSymbolResult(r) => match r {
            lsp_mapper::workspace_symbol_result::T::SymbolInformation(v) => to_json(v),
            lsp_mapper::workspace_symbol_result::T::WorkspaceSymbolInformation(v) => {
                let items: Vec<serde_json::Value> = v
                    .iter()
                    .map(|info| {
                        let mut obj = serde_json::Map::new();
                        obj.insert(
                            "name".to_string(),
                            serde_json::Value::String(info.name.clone()),
                        );
                        obj.insert("kind".to_string(), to_json(&info.kind));
                        obj.insert(
                            "location".to_string(),
                            serde_json::json!({
                                "uri": info.location.uri.to_string(),
                            }),
                        );
                        if let Some(ref cn) = info.container_name {
                            obj.insert(
                                "containerName".to_string(),
                                serde_json::Value::String(cn.clone()),
                            );
                        }
                        serde_json::Value::Object(obj)
                    })
                    .collect();
                serde_json::Value::Array(items)
            }
        },
        lsp_mapper::LspResult::DocumentSymbolResult(r) => match r {
            lsp_mapper::document_symbol_result::T::SymbolInformation(v) => to_json(v),
            lsp_mapper::document_symbol_result::T::DocumentSymbol(v) => to_json(v),
        },
        lsp_mapper::LspResult::FindReferencesResult(r) => to_json(r),
        lsp_mapper::LspResult::GoToImplementationResult(r) => to_json(r),
        lsp_mapper::LspResult::DocumentHighlightResult(r) => to_json(r),
        lsp_mapper::LspResult::TypeCoverageResult(r) => {
            let uncovered_ranges: Vec<serde_json::Value> = r
                .uncovered_ranges
                .iter()
                .map(|uncov| {
                    let mut obj = serde_json::Map::new();
                    obj.insert("range".to_string(), to_json(&uncov.range));
                    if let Some(ref msg) = uncov.message {
                        obj.insert(
                            "message".to_string(),
                            serde_json::Value::String(msg.clone()),
                        );
                    }
                    serde_json::Value::Object(obj)
                })
                .collect();
            serde_json::json!({
                "coveredPercent": r.covered_percent,
                "uncoveredRanges": uncovered_ranges,
                "defaultMessage": r.default_message,
            })
        }
        lsp_mapper::LspResult::DocumentFormattingResult(r) => to_json(r),
        lsp_mapper::LspResult::DocumentRangeFormattingResult(r) => to_json(r),
        lsp_mapper::LspResult::DocumentOnTypeFormattingResult(r) => to_json(r),
        lsp_mapper::LspResult::RageResult(r) => {
            let items: Vec<serde_json::Value> = r
                .iter()
                .map(|item| {
                    serde_json::json!({
                        "data": item.data,
                        "title": match &item.title {
                            None => serde_json::Value::Null,
                            Some(s) => serde_json::Value::String(s.clone()),
                        },
                    })
                })
                .collect();
            serde_json::Value::Array(items)
        }
        lsp_mapper::LspResult::PingResult(r) => {
            serde_json::json!({
                "startServerStatus": r.start_server_status,
            })
        }
        lsp_mapper::LspResult::PrepareRenameResult(r) => to_json(r),
        lsp_mapper::LspResult::RegisterCapabilityResult => {
            panic!("Don't know how to print result RegisterCapabilityResult");
        }
        lsp_mapper::LspResult::RenameResult(r) => to_json(r),
        lsp_mapper::LspResult::DocumentCodeLensResult(r) => to_json(
            &r.clone()
                .into_iter()
                .map(|code_lens| key_code_lens(key, code_lens))
                .collect::<Vec<_>>(),
        ),
        lsp_mapper::LspResult::ExecuteCommandResult(_) => serde_json::Value::Null,
        lsp_mapper::LspResult::ApplyWorkspaceEditResult(r) => to_json(r),
        lsp_mapper::LspResult::ShowMessageRequestResult(_) => {
            panic!("Don't know how to print result ShowMessageRequestResult");
        }
        lsp_mapper::LspResult::ShowStatusResult(_) => {
            panic!("Don't know how to print result ShowStatusResult");
        }
        lsp_mapper::LspResult::WillRenameFilesResult(r) => to_json(r),
        lsp_mapper::LspResult::AutoCloseJsxResult(r) => to_json(r),
        lsp_mapper::LspResult::PrepareDocumentPasteResult(r) => match r {
            lsp_mapper::document_paste::DataTransfer::ImportMetadata { imports } => {
                let import_items: Vec<serde_json::Value> = imports
                    .iter()
                    .map(|item| {
                        let import_type = match item.import_type {
                            lsp_mapper::document_paste::ImportType::ImportNamedValue => {
                                "ImportNamedValue"
                            }
                            lsp_mapper::document_paste::ImportType::ImportValueAsNamespace => {
                                "ImportValueAsNamespace"
                            }
                            lsp_mapper::document_paste::ImportType::ImportNamedType => {
                                "ImportNamedType"
                            }
                            lsp_mapper::document_paste::ImportType::ImportNamedTypeOf => {
                                "ImportNamedTypeOf"
                            }
                            lsp_mapper::document_paste::ImportType::ImportTypeOfAsNamespace => {
                                "ImportTypeOfAsNamespace"
                            }
                        };
                        let mut obj = serde_json::Map::new();
                        obj.insert(
                            "remoteName".to_string(),
                            serde_json::Value::String(item.remote_name.clone()),
                        );
                        if let Some(ref local_name) = item.local_name {
                            obj.insert(
                                "localName".to_string(),
                                serde_json::Value::String(local_name.clone()),
                            );
                        }
                        obj.insert(
                            "importType".to_string(),
                            serde_json::Value::String(import_type.to_string()),
                        );
                        obj.insert(
                            "importSource".to_string(),
                            serde_json::Value::String(item.import_source.clone()),
                        );
                        obj.insert(
                            "importSourceIsResolved".to_string(),
                            serde_json::Value::Bool(item.import_source_is_resolved),
                        );
                        serde_json::Value::Object(obj)
                    })
                    .collect();
                serde_json::json!({ "imports": import_items })
            }
        },
        lsp_mapper::LspResult::ProvideDocumentPasteResult(r) => to_json(r),
        lsp_mapper::LspResult::LinkedEditingRangeResult(r) => to_json(r),
        lsp_mapper::LspResult::RenameFileImportsResult(r) => to_json(r),
        lsp_mapper::LspResult::LLMContextResult(r) => {
            let files_processed: Vec<serde_json::Value> = r
                .files_processed
                .iter()
                .map(|s| serde_json::Value::String(s.clone()))
                .collect();
            serde_json::json!({
                "llmContext": r.llm_context,
                "filesProcessed": files_processed,
                "tokensUsed": r.tokens_used,
                "truncated": r.truncated,
            })
        }
        lsp_mapper::LspResult::ErrorResult(e, stack) => print_error(true, e, stack),
    };
    let result_key = match result {
        lsp_mapper::LspResult::ErrorResult(_, _) => "error",
        _ => "result",
    };
    serde_json::json!({
        "jsonrpc": "2.0",
        "id": print_id(id),
        result_key: json,
    })
}

pub fn lsp_fmt_print_lsp_notification(
    notification: &lsp_mapper::LspNotification,
) -> serde_json::Value {
    let method_ = notification_name_to_string(notification);
    let params = match notification {
        lsp_mapper::LspNotification::CancelRequestNotification(r) => to_json(r),
        lsp_mapper::LspNotification::PublishDiagnosticsNotification(r) => to_json(r),
        lsp_mapper::LspNotification::TelemetryNotification(r) => to_json(r),
        lsp_mapper::LspNotification::LogMessageNotification(r) => to_json(r),
        lsp_mapper::LspNotification::ShowMessageNotification(r) => to_json(r),
        lsp_mapper::LspNotification::ConnectionStatusNotification(r) => {
            serde_json::json!({ "isConnected": r.is_connected })
        }
        _ => panic!("Don't know how to print notification {}", method_),
    };
    serde_json::json!({
        "jsonrpc": "2.0",
        "method": method_,
        "params": params,
    })
}

pub fn lsp_fmt_print_lsp(key: &str, msg: &lsp_prot::LspMessage) -> serde_json::Value {
    match msg {
        lsp_prot::LspMessage::RequestMessage(id, request) => {
            let method_ = request_name_to_string(request);
            let params = match request {
                lsp_mapper::LspRequest::ShowMessageRequestRequest(r) => to_json(r),
                lsp_mapper::LspRequest::ShowStatusRequest(r) => {
                    serde_json::json!({
                        "request": to_json(&r.request),
                        "progress": r.progress,
                        "total": r.total,
                    })
                }
                lsp_mapper::LspRequest::ApplyWorkspaceEditRequest(r) => to_json(r),
                lsp_mapper::LspRequest::ConfigurationRequest(r) => to_json(r),
                lsp_mapper::LspRequest::RegisterCapabilityRequest(r) => {
                    let registrations: Vec<serde_json::Value> = r
                        .registrations
                        .iter()
                        .map(|reg| {
                            let register_options = match &reg.register_options {
                                lsp_mapper::register_capability::Options::DidChangeConfiguration => {
                                    serde_json::Value::Null
                                }
                                lsp_mapper::register_capability::Options::DidChangeWatchedFiles(
                                    opts,
                                ) => to_json(opts),
                            };
                            serde_json::json!({
                                "id": reg.id,
                                "method": reg.method,
                                "registerOptions": register_options,
                            })
                        })
                        .collect();
                    serde_json::json!({ "registrations": registrations })
                }
                _ => panic!("Don't know how to print request {}", method_),
            };
            serde_json::json!({
                "jsonrpc": "2.0",
                "id": print_id(id),
                "method": method_,
                "params": params,
            })
        }
        lsp_prot::LspMessage::ResponseMessage(id, result) => {
            lsp_fmt_print_lsp_response(key, id, result)
        }
        lsp_prot::LspMessage::NotificationMessage(notification) => {
            lsp_fmt_print_lsp_notification(notification)
        }
    }
}

pub fn lsp_fmt_parse_id(id: &serde_json::Value) -> Result<LspId, FlowLspError> {
    match id {
        serde_json::Value::Number(n) => {
            let s = n.to_string();
            s.parse::<i32>()
                .map(NumberOrString::Number)
                .map_err(|_| parse_error_exception(format!("float ids not allowed: {}", s)))
        }
        serde_json::Value::String(s) => Ok(NumberOrString::String(s.clone())),
        _ => Err(parse_error_exception(format!("not an id: {}", id))),
    }
}

pub fn lsp_fmt_denorm_message_to_string(msg: &lsp_prot::LspMessage) -> String {
    match msg {
        lsp_prot::LspMessage::RequestMessage(id, req) => {
            format!("request {:?} {:?}", id, req)
        }
        lsp_prot::LspMessage::NotificationMessage(notif) => {
            format!("notification {:?}", notif)
        }
        lsp_prot::LspMessage::ResponseMessage(id, result) => {
            format!("result {:?} {:?}", id, result)
        }
    }
}

pub fn lsp_fmt_error_of_exn(
    e: &dyn std::error::Error,
) -> flow_server_env::lsp_mapper::lsp_error::T {
    flow_server_env::lsp_mapper::lsp_error::T {
        code: flow_server_env::lsp_mapper::lsp_error::Code::InternalError,
        message: e.to_string(),
        data: None,
    }
}

pub fn lsp_helpers_supports_status(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .experimental
        .as_ref()
        .and_then(|experimental| experimental.get("window/status"))
        .and_then(|status| status.as_bool())
        .unwrap_or(false)
}

pub fn lsp_helpers_supports_connection_status(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .experimental
        .as_ref()
        .and_then(|experimental| experimental.get("telemetry/connectionStatus"))
        .and_then(|status| status.as_bool())
        .unwrap_or(false)
}

pub fn lsp_helpers_supports_code_action_kinds(params: &lsp_types::InitializeParams) -> Vec<String> {
    params
        .capabilities
        .text_document
        .as_ref()
        .and_then(|td| td.code_action.as_ref())
        .and_then(|ca| ca.code_action_literal_support.as_ref())
        .map(|lit| {
            lit.code_action_kind
                .value_set
                .iter()
                .map(|k| k.as_str().to_string())
                .collect()
        })
        .unwrap_or_default()
}

pub fn lsp_helpers_supports_hierarchical_document_symbol(
    params: &lsp_types::InitializeParams,
) -> bool {
    params
        .capabilities
        .text_document
        .as_ref()
        .and_then(|td| td.document_symbol.as_ref())
        .and_then(|ds| ds.hierarchical_document_symbol_support)
        .unwrap_or(false)
}

pub fn lsp_helpers_supports_experimental_snippet_text_edit(
    params: &lsp_types::InitializeParams,
) -> bool {
    params
        .capabilities
        .experimental
        .as_ref()
        .and_then(|exp| exp.get("snippetTextEdit"))
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

fn invalid_file_url_error(uri: &DocumentUri) -> FlowLspError {
    FlowLspError::LspException(flow_server_env::lsp_mapper::lsp_error::T {
        code: flow_server_env::lsp_mapper::lsp_error::Code::InvalidParams,
        message: format!("Not a valid file url '{}'", uri),
        data: None,
    })
}

pub fn lsp_helpers_lsp_uri_to_path(uri: &DocumentUri) -> Result<String, FlowLspError> {
    if uri.scheme() == "file" {
        uri.to_file_path()
            .map(|p| p.to_string_lossy().to_string())
            .map_err(|_| invalid_file_url_error(uri))
    } else {
        Err(invalid_file_url_error(uri))
    }
}

pub fn lsp_helpers_apply_changes_unsafe(
    text: &str,
    changes: &[lsp_types::TextDocumentContentChangeEvent],
) -> String {
    use flow_server_utils::file_content;
    let edits: Vec<file_content::TextEdit> = changes
        .iter()
        .map(|change| {
            let range = change.range.map(|r| file_content::Range {
                st: file_content::Position {
                    line: r.start.line as usize + 1,
                    column: r.start.character as usize + 1,
                },
                ed: file_content::Position {
                    line: r.end.line as usize + 1,
                    column: r.end.character as usize + 1,
                },
            });
            file_content::TextEdit {
                range,
                text: change.text.clone(),
            }
        })
        .collect();
    file_content::edit_file_unsafe(text, &edits)
}

#[allow(deprecated)] // root_path is deprecated in LSP but OCaml uses it as fallback
pub fn lsp_helpers_get_root(
    params: &lsp_types::InitializeParams,
) -> Result<FilePath, FlowLspError> {
    if let Some(ref uri) = params.root_uri {
        Ok(FilePath::from(lsp_helpers_lsp_uri_to_path(uri)?))
    } else if let Some(ref path) = params.root_path {
        Ok(FilePath::from(path))
    } else {
        Err(FlowLspError::LspException(
            flow_server_env::lsp_mapper::lsp_error::T {
                code: flow_server_env::lsp_mapper::lsp_error::Code::InternalError,
                message: "Initialize params missing root".to_string(),
                data: None,
            },
        ))
    }
}

pub fn lsp_helpers_supports_configuration(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .workspace
        .as_ref()
        .and_then(|ws| ws.configuration)
        .unwrap_or(false)
}

pub fn server_files_js_default_temp_dir() -> FilePath {
    flow_server_files::server_files_js::default_temp_dir()
}

fn connect_temp_dir(connect_params: &ConnectParams) -> String {
    connect_params.temp_dir.clone().unwrap_or_else(|| {
        server_files_js_default_temp_dir()
            .to_string_lossy()
            .to_string()
    })
}

fn io_error_is_server_not_running(kind: std::io::ErrorKind) -> bool {
    matches!(
        kind,
        std::io::ErrorKind::ConnectionRefused
            | std::io::ErrorKind::ConnectionReset
            | std::io::ErrorKind::ConnectionAborted
            | std::io::ErrorKind::BrokenPipe
            | std::io::ErrorKind::UnexpectedEof
            | std::io::ErrorKind::NotConnected
    )
}

fn persistent_canonical_root(root: &std::path::Path) -> FilePath {
    root.canonicalize().unwrap_or_else(|_| root.to_path_buf())
}

fn persistent_server_lock_is_held(lock_path: &str) -> bool {
    if !std::path::Path::new(lock_path).exists() {
        return false;
    }
    let file = match std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(lock_path)
    {
        Ok(file) => file,
        Err(_) => return true,
    };
    matches!(file.try_lock(), Err(std::fs::TryLockError::WouldBlock))
}

fn persistent_server_exists(flowconfig_name: &str, tmp_dir: &str, root: &std::path::Path) -> bool {
    let root = persistent_canonical_root(root);
    let lock_path = flow_server_files::server_files_js::lock_file(flowconfig_name, tmp_dir, &root);
    persistent_server_lock_is_held(&lock_path)
}

fn classify_persistent_connect(
    flowconfig_name: &str,
    env: &DisconnectedEnv,
) -> Result<ServerConn, ConnectError> {
    let tmp_dir = connect_temp_dir(&env.d_ienv.i_connect_params);
    let root = persistent_canonical_root(&env.d_ienv.i_root);
    let socket_path =
        flow_server_files::server_files_js::socket_file(flowconfig_name, &tmp_dir, &root);
    let server_exists_now = || persistent_server_exists(flowconfig_name, &tmp_dir, &root);
    let port_str = match std::fs::read_to_string(&socket_path) {
        Ok(port) => port,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            return if server_exists_now() {
                Err(ConnectError::ServerSocketMissing)
            } else {
                Err(ConnectError::ServerMissing)
            };
        }
        Err(_) => {
            return Err(ConnectError::ServerBusy(ServerBusyKind::NotResponding));
        }
    };
    let port: u16 = match port_str.trim().parse() {
        Ok(port) => port,
        Err(_) => {
            return if server_exists_now() {
                Err(ConnectError::ServerSocketMissing)
            } else {
                Err(ConnectError::ServerMissing)
            };
        }
    };
    let mut stream = match TcpStream::connect(("127.0.0.1", port)) {
        Ok(stream) => stream,
        Err(e) if io_error_is_server_not_running(e.kind()) => {
            return if server_exists_now() {
                Err(ConnectError::ServerSocketMissing)
            } else {
                Err(ConnectError::ServerMissing)
            };
        }
        Err(_) => return Err(ConnectError::ServerBusy(ServerBusyKind::NotResponding)),
    };
    let _ = stream.set_read_timeout(Some(Duration::from_secs(1)));
    let _ = stream.set_write_timeout(Some(Duration::from_secs(10)));
    let client_id = env.d_ienv.i_server_id + 1;
    let request = server_socket_rpc::ServerRequest::PersistentConnect {
        client_id,
        lsp_initialize_params: env.d_ienv.i_initialize_params.clone(),
    };
    server_socket_rpc::send_message(&mut stream, &request)
        .map_err(|_| ConnectError::ServerBusy(ServerBusyKind::NotResponding))?;
    match server_socket_rpc::receive_message(&mut stream) {
        Ok(server_socket_rpc::ServerResponse::PersistentConnected) => Ok(ServerConn {
            client_id,
            stream: Mutex::new(stream),
        }),
        Ok(server_socket_rpc::ServerResponse::PersistentBusy {
            server_status,
            watcher_status,
        }) => Err(ConnectError::ServerBusy(ServerBusyKind::FailOnInit((
            server_status,
            watcher_status,
        )))),
        Ok(_) => Err(ConnectError::ServerBusy(ServerBusyKind::NotResponding)),
        Err(e) if e.kind() == std::io::ErrorKind::TimedOut => {
            Err(ConnectError::ServerBusy(ServerBusyKind::NotResponding))
        }
        Err(e) if io_error_is_server_not_running(e.kind()) => {
            if server_exists_now() {
                Err(ConnectError::ServerBusy(ServerBusyKind::NotResponding))
            } else {
                Err(ConnectError::ServerMissing)
            }
        }
        Err(_) => Err(ConnectError::ServerBusy(ServerBusyKind::NotResponding)),
    }
}

fn persistent_rpc(
    conn: &ServerConn,
    request: server_socket_rpc::ServerRequest,
) -> Result<server_socket_rpc::ServerResponse, std::io::Error> {
    let mut stream = conn.stream.lock().unwrap();
    server_socket_rpc::send_message(&mut *stream, &request)?;
    server_socket_rpc::receive_message(&mut *stream)
}

fn flow_cli_command(
    subcommand: &str,
    flowconfig_name: &str,
    connect_params: &ConnectParams,
    root: &FilePath,
) -> std::process::Command {
    let root = persistent_canonical_root(root);
    let exe = std::env::args_os()
        .next()
        .unwrap_or_else(|| std::ffi::OsString::from("flow"));
    let mut command = std::process::Command::new(exe);
    command.arg(subcommand);
    command.arg("--flowconfig-name").arg(flowconfig_name);
    if let Some(from) = connect_params.from.as_deref() {
        command.arg("--from").arg(from);
    }
    if let Some(temp_dir) = connect_params.temp_dir.as_deref() {
        command.arg("--temp-dir").arg(temp_dir);
    }
    if subcommand == "start" {
        if let Some(shm_heap_size) = connect_params.shm_flags.shm_heap_size {
            command
                .arg("--sharedmemory-heap-size")
                .arg(shm_heap_size.to_string());
        }
        if let Some(shm_hash_table_pow) = connect_params.shm_flags.shm_hash_table_pow {
            command
                .arg("--sharedmemory-hash-table-pow")
                .arg(shm_hash_table_pow.to_string());
        }
        if connect_params.autostop {
            command.arg("--autostop");
        }
    }
    command.arg(root);
    command
}

fn start_flow_server(
    flowconfig_name: &str,
    connect_params: &ConnectParams,
    root: &FilePath,
) -> Result<(), String> {
    let output = flow_cli_command("start", flowconfig_name, connect_params, root)
        .output()
        .map_err(|e| format!("Failed to start Flow server: {}", e))?;
    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        if stderr.is_empty() {
            Err(format!("Flow start exited with status {}", output.status))
        } else {
            Err(stderr)
        }
    }
}

fn kill_stale_server(
    flowconfig_name: &str,
    connect_params: &ConnectParams,
    root: &FilePath,
) -> Result<(), String> {
    let output = flow_cli_command("stop", flowconfig_name, connect_params, root)
        .arg("--quiet")
        .output()
        .map_err(|e| format!("Failed to stop stale Flow server: {}", e))?;
    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        if stderr.is_empty() {
            Err(format!("Flow stop exited with status {}", output.status))
        } else {
            Err(stderr)
        }
    }
}

pub fn file_options_of_flowconfig(root: &FilePath, flowconfig: &FlowConfig) -> FileOptions {
    use flow_common::files;
    use flow_common::path_matcher::PathMatcher;
    use regex::Regex;
    let temp_dir = flow_server_files::server_files_js::default_temp_dir();
    let no_flowlib = true;
    let default_lib_dir = {
        let libdir = flow_flowlib::libdir(no_flowlib, &temp_dir);
        Some(match libdir {
            flow_flowlib::LibDir::Prelude(path) => files::LibDir::Prelude(path),
            flow_flowlib::LibDir::Flowlib(path) => files::LibDir::Flowlib(path),
        })
    };
    let ignores: Vec<((String, Option<String>), Regex)> = flowconfig
        .ignores
        .iter()
        .map(|(path, backup)| {
            let pattern = path.strip_prefix('!').unwrap_or(path.as_str());
            let expanded = files::expand_project_root_token(root, pattern);
            let reg = Regex::new(&expanded).unwrap_or_else(|_| Regex::new("$^").unwrap());
            ((path.clone(), backup.clone()), reg)
        })
        .collect();
    let untyped: Vec<(String, Regex)> = flowconfig
        .untyped
        .iter()
        .map(|s| {
            let pattern = s.strip_prefix('!').unwrap_or(s.as_str());
            let expanded = files::expand_project_root_token(root, pattern);
            let reg = Regex::new(&expanded).unwrap_or_else(|_| Regex::new("$^").unwrap());
            (s.clone(), reg)
        })
        .collect();
    let declarations: Vec<(String, Regex)> = flowconfig
        .declarations
        .iter()
        .map(|s| {
            let pattern = s.strip_prefix('!').unwrap_or(s.as_str());
            let expanded = files::expand_project_root_token(root, pattern);
            let reg = Regex::new(&expanded).unwrap_or_else(|_| Regex::new("$^").unwrap());
            (s.clone(), reg)
        })
        .collect();
    let lib_paths: Vec<(Option<String>, std::path::PathBuf)> = {
        let flowtyped_path = files::get_flowtyped_path(root);
        let mut has_explicit_flowtyped_lib = false;
        let mut config_libs: Vec<(Option<String>, std::path::PathBuf)> = flowconfig
            .libs
            .iter()
            .map(|(sp, lib)| {
                let abs_lib = files::make_path_absolute(root, lib);
                if abs_lib == flowtyped_path {
                    has_explicit_flowtyped_lib = true;
                }
                (sp.as_ref().map(|s| s.to_string()), abs_lib)
            })
            .collect();
        if !has_explicit_flowtyped_lib && flowtyped_path.exists() {
            config_libs.insert(0, (None, flowtyped_path));
        }
        config_libs
    };
    let implicitly_include_root = flowconfig.options.files_implicitly_include_root;
    let includes = {
        let mut path_matcher = PathMatcher::empty();
        for path in &flowconfig.includes {
            path_matcher.add_path(&files::make_path_absolute(root, path));
        }
        let mut implicitly_included: Vec<std::path::PathBuf> = if implicitly_include_root {
            vec![root.to_path_buf()]
        } else {
            vec![]
        };
        for (_, lp) in &lib_paths {
            implicitly_included.push(lp.clone());
        }
        implicitly_included.sort_by_key(|p| p.to_string_lossy().len());
        for path in &implicitly_included {
            if !path_matcher.matches(&path.to_string_lossy()) {
                path_matcher.add_path(path);
            }
        }
        path_matcher
    };
    let haste_paths_excludes: Vec<Regex> = flowconfig
        .options
        .haste_paths_excludes
        .iter()
        .map(|f| {
            let expanded = files::expand_project_root_token(root, f);
            Regex::new(&expanded).unwrap_or_else(|_| Regex::new("$^").unwrap())
        })
        .collect();
    let haste_paths_includes: Vec<Regex> = flowconfig
        .options
        .haste_paths_includes
        .iter()
        .map(|f| {
            let expanded = files::expand_project_root_token(root, f);
            Regex::new(&expanded).unwrap_or_else(|_| Regex::new("$^").unwrap())
        })
        .collect();
    let module_declaration_dirnames: Vec<String> = flowconfig
        .options
        .module_declaration_dirnames
        .iter()
        .map(|dir| files::expand_project_root_token(root, dir))
        .collect();
    FileOptions {
        default_lib_dir,
        ignores,
        untyped,
        declarations,
        implicitly_include_root,
        includes,
        haste_paths_excludes,
        haste_paths_includes,
        lib_paths,
        module_declaration_dirnames,
        module_file_exts: flowconfig.options.module_file_exts.clone(),
        module_resource_exts: flowconfig.options.module_resource_exts.clone(),
        multi_platform: flowconfig.options.multi_platform.unwrap_or(false),
        multi_platform_extensions: flowconfig.options.multi_platform_extensions.clone(),
        multi_platform_extension_group_mapping: flowconfig
            .options
            .multi_platform_extension_group_mapping
            .clone(),
        node_resolver_dirnames: flowconfig.options.node_resolver_dirnames.clone(),
    }
}

fn jsonrpc_log_notification(method: &str, level: i32, message: &str) -> serde_json::Value {
    serde_json::json!({
        "jsonrpc": "2.0",
        "method": method,
        "params": { "type": level, "message": message }
    })
}

pub fn lsp_writers_notify_connection_status(
    params: &lsp_types::InitializeParams,
    to_stdout: fn(&serde_json::Value),
    was_connected: bool,
    is_connected: bool,
) -> bool {
    if lsp_helpers_supports_connection_status(params) && was_connected != is_connected {
        let notification = serde_json::json!({
            "jsonrpc": "2.0",
            "method": "telemetry/connectionStatus",
            "params": { "isConnected": is_connected }
        });
        to_stdout(&notification);
    }
    is_connected
}

pub fn lsp_writers_log_info(to_stdout: fn(&serde_json::Value), msg: &str) {
    let notification = jsonrpc_log_notification("window/logMessage", 3, msg);
    to_stdout(&notification);
}

pub fn lsp_writers_telemetry_log(to_stdout: fn(&serde_json::Value), msg: &str) {
    let notification = jsonrpc_log_notification("telemetry/event", 4, msg);
    to_stdout(&notification);
}

pub fn lsp_writers_telemetry_error(to_stdout: fn(&serde_json::Value), msg: &str) {
    let notification = jsonrpc_log_notification("telemetry/event", 1, msg);
    to_stdout(&notification);
}

pub mod flow_event_logger {
    fn emit(event_name: &str, fields: serde_json::Value) {
        eprintln!(
            "{}",
            serde_json::json!({
                "event_name": event_name,
                "fields": fields,
            })
        );
    }

    pub fn live_non_parse_errors(request: &str, data: &str, wall_start: f64) {
        emit(
            "LIVE_NON_PARSE_ERRORS",
            serde_json::json!({
                "request": request,
                "data": data,
                "wall_start": wall_start,
            }),
        );
    }

    pub fn live_non_parse_errors_failed(request: &str, data: &str, wall_start: f64) {
        emit(
            "LIVE_NON_PARSE_ERRORS_FAILED",
            serde_json::json!({
                "request": request,
                "data": data,
                "wall_start": wall_start,
            }),
        );
    }

    #[allow(clippy::too_many_arguments)]
    pub fn persistent_command_success(
        metadata: &super::Metadata,
        request: &str,
        request_id: Option<&str>,
        persistent_delay: Option<&super::PersistentDelay>,
        error: Option<(&str, &str)>,
    ) {
        emit(
            "PERSISTENT_COMMAND_SUCCESS",
            serde_json::json!({
                "request": request,
                "request_id": request_id.unwrap_or(""),
                "method_name": metadata.lsp_method_name,
                "extra_data": metadata.extra_data,
                "server_profiling": metadata.server_profiling,
                "client_duration": metadata.client_duration,
                "server_logging_context": metadata.server_logging_context,
                "persistent_delay": persistent_delay,
                "activity_key": metadata.activity_key,
                "error": error,
                "wall_start": metadata.start_wall_time,
            }),
        );
    }

    pub fn persistent_command_failure(
        metadata: &super::Metadata,
        request: &str,
        persistent_delay: Option<&super::PersistentDelay>,
        error: (&str, &str),
    ) {
        emit(
            "PERSISTENT_COMMAND_FAILURE",
            serde_json::json!({
                "request": request,
                "method_name": metadata.lsp_method_name,
                "extra_data": metadata.extra_data,
                "server_profiling": metadata.server_profiling,
                "client_duration": metadata.client_duration,
                "server_logging_context": metadata.server_logging_context,
                "persistent_delay": persistent_delay,
                "activity_key": metadata.activity_key,
                "error": error,
                "wall_start": metadata.start_wall_time,
            }),
        );
    }

    pub fn persistent_expected_error(
        request: Option<&str>,
        activity_key: Option<&serde_json::Value>,
        error: (&str, &str),
    ) {
        emit(
            "PERSISTENT_EXPECTED_ERROR",
            serde_json::json!({
                "request": request.unwrap_or(""),
                "activity_key": activity_key,
                "error": error,
            }),
        );
    }

    pub fn persistent_unexpected_error(
        request: Option<&str>,
        activity_key: Option<&serde_json::Value>,
        error: (&str, &str),
    ) {
        emit(
            "PERSISTENT_UNEXPECTED_ERROR",
            serde_json::json!({
                "request": request.unwrap_or(""),
                "activity_key": activity_key,
                "error": error,
            }),
        );
    }
}

#[allow(dead_code)]
const PREAMBLE_SENTINEL: u8 = 0x8E;
const _PREAMBLE_CORE_SIZE: usize = 4;
#[allow(dead_code)]
const PREAMBLE_SIZE: usize = _PREAMBLE_CORE_SIZE + 1;

fn server_fatal_connection_exception(message: impl Into<String>) -> FlowLspError {
    FlowLspError::ServerFatalConnectionException(RemoteExceptionData {
        message: message.into(),
        stack: String::new(),
    })
}

fn make_lsp_exception(
    code: flow_server_env::lsp_mapper::lsp_error::Code,
    message: impl Into<String>,
) -> FlowLspError {
    FlowLspError::LspException(flow_server_env::lsp_mapper::lsp_error::T {
        code,
        message: message.into(),
        data: None,
    })
}

fn parse_error_exception(message: impl Into<String>) -> FlowLspError {
    make_lsp_exception(
        flow_server_env::lsp_mapper::lsp_error::Code::ParseError,
        message,
    )
}

fn internal_error_exception(message: impl Into<String>) -> FlowLspError {
    make_lsp_exception(
        flow_server_env::lsp_mapper::lsp_error::Code::InternalError,
        message,
    )
}

pub fn sys_utils_select_non_intr(
    _read_fds: &[RawFd],
    _write_fds: &[RawFd],
    _except_fds: &[RawFd],
    _timeout: f64,
) -> Result<(Vec<RawFd>, Vec<RawFd>, Vec<RawFd>), std::io::Error> {
    Err(std::io::Error::new(
        std::io::ErrorKind::Unsupported,
        "sys_utils_select_non_intr: requires unsafe for libc::poll syscall",
    ))
}

pub fn flow_version() -> &'static str {
    flow_common::flow_version::VERSION
}

pub fn read_flowconfig_from_disk(flowconfig_name: &str, root: &FilePath) -> FlowConfig {
    let config_path = flow_server_files::server_files_js::config_file(flowconfig_name, root);
    match flow_config::get(&config_path) {
        Ok((flowconfig, _warnings, _hash)) => flowconfig,
        Err(flow_config::Error(line, msg)) => {
            eprintln!(".flowconfig:{} {}", line, msg);
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::InvalidFlowconfig,
            );
        }
    }
}

pub fn lsp_interaction_init() {
    crate::lsp_interaction::init();
}

pub fn log_flusher_run() {}

pub fn command_utils_check_version(required_version: &Option<String>) -> Result<(), String> {
    match required_version {
        None => Ok(()),
        Some(version_constraint) => {
            match flow_common_semver::semver::satisfies(
                Some(true),
                version_constraint,
                flow_common::flow_version::VERSION,
            ) {
                Ok(true) => Ok(()),
                Ok(false) => {
                    let msg = format!(
                        "Wrong version of Flow. The config specifies version {} but this is version {}",
                        version_constraint,
                        flow_common::flow_version::VERSION
                    );
                    Err(msg)
                }
                Err(_) => {
                    let msg = format!(
                        "Wrong version of Flow. The config specifies version {} but this is version {}",
                        version_constraint,
                        flow_common::flow_version::VERSION
                    );
                    Err(msg)
                }
            }
        }
    }
}

fn jsonrpc_get_next_request_id() -> i32 {
    static NEXT_ID: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);
    NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

pub(crate) fn lsp_fmt_id_to_string(id: &LspId) -> String {
    match id {
        NumberOrString::Number(n) => n.to_string(),
        NumberOrString::String(s) => s.clone(),
    }
}

use crate::lsp_interaction;

fn sys_utils_realpath(path: &str) -> Option<String> {
    std::fs::canonicalize(path)
        .ok()
        .map(|p| p.to_string_lossy().to_string())
}

fn json_truncate(
    json: &serde_json::Value,
    max_string_length: usize,
    max_child_count: usize,
) -> serde_json::Value {
    match json {
        serde_json::Value::String(s) => {
            if s.len() > max_string_length {
                serde_json::Value::String(s[..max_string_length].to_string())
            } else {
                json.clone()
            }
        }
        serde_json::Value::Array(arr) => {
            let truncated: Vec<serde_json::Value> = arr
                .iter()
                .take(max_child_count)
                .map(|v| json_truncate(v, max_string_length, max_child_count))
                .collect();
            serde_json::Value::Array(truncated)
        }
        serde_json::Value::Object(map) => {
            let truncated: serde_json::Map<String, serde_json::Value> = map
                .iter()
                .take(max_child_count)
                .map(|(k, v)| {
                    (
                        k.clone(),
                        json_truncate(v, max_string_length, max_child_count),
                    )
                })
                .collect();
            serde_json::Value::Object(truncated)
        }
        _ => json.clone(),
    }
}

fn now() -> f64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs_f64()
}

pub fn string_of_server_state(state: &ServerState) -> &'static str {
    match state {
        ServerState::Disconnected(_) => "Disconnected",
        ServerState::Connected(_) => "Connected",
    }
}

pub fn string_of_state(state: &State) -> &'static str {
    match state {
        State::PreInit(_) => "Pre_init",
        State::Initialized(server_state) => string_of_server_state(server_state),
        State::PostShutdown => "Post_shutdown",
    }
}

fn to_stdout_fn() -> impl FnMut(serde_json::Value) {
    |json| to_stdout(&json)
}

fn to_stdout(json: &serde_json::Value) {
    let s = format!("{}\r\n\r\n", serde_json::to_string(json).unwrap());
    let content_length = s.len();
    let mut stdout = io::stdout().lock();
    write!(stdout, "Content-Length: {}\r\n\r\n{}", content_length, s).unwrap();
    stdout.flush().unwrap();
}

fn get_ienv(state: &ServerState) -> &InitializedEnv {
    match state {
        ServerState::Connected(cenv) => &cenv.c_ienv,
        ServerState::Disconnected(denv) => &denv.d_ienv,
    }
}

fn get_ienv_mut(state: &mut ServerState) -> &mut InitializedEnv {
    match state {
        ServerState::Connected(cenv) => &mut cenv.c_ienv,
        ServerState::Disconnected(denv) => &mut denv.d_ienv,
    }
}

fn update_ienv(state: &mut ServerState, f: impl FnOnce(&mut InitializedEnv)) {
    f(get_ienv_mut(state));
}

fn update_recent_summaries(cenv: &mut ConnectedEnv, summary: lsp_prot::TelemetryFromServer) {
    let new_time = now();
    cenv.c_recent_summaries.insert(0, (new_time, summary));
    cenv.c_recent_summaries
        .retain(|(t, _)| *t >= new_time - 120.0);
}

#[allow(dead_code)]
fn log_of_summaries(
    root: &FilePath,
    summaries: &[lsp_prot::TelemetryFromServer],
) -> PersistentDelay {
    let mut acc = PersistentDelay::default();
    for event in summaries {
        match event {
            lsp_prot::TelemetryFromServer::InitSummary { duration } => {
                acc.init_duration += duration;
            }
            lsp_prot::TelemetryFromServer::CommandSummary {
                name: cmd,
                duration,
            } => {
                let is_worst = match acc.command_worst_duration {
                    None => true,
                    Some(d) => *duration >= d,
                };
                if is_worst {
                    acc.command_worst = Some(cmd.clone());
                    acc.command_worst_duration = Some(*duration);
                }
                acc.command_count += 1;
                acc.command_duration += duration;
            }
            lsp_prot::TelemetryFromServer::RecheckSummary { stats, duration } => {
                let lsp_prot::RecheckStats {
                    dependent_file_count,
                    changed_file_count,
                    ref top_cycle,
                } = *stats;
                let is_worst = match acc.recheck_worst_duration {
                    None => true,
                    Some(d) => *duration >= d,
                };
                if is_worst {
                    acc.recheck_worst_duration = Some(*duration);
                    acc.recheck_worst_dependent_file_count = Some(dependent_file_count);
                    acc.recheck_worst_changed_file_count = Some(changed_file_count);
                    acc.recheck_worst_cycle_size = top_cycle.as_ref().map(|(_, size)| *size);
                    acc.recheck_worst_cycle_leader = top_cycle
                        .as_ref()
                        .map(|(f, _)| flow_common::files::relative_path(root, &f.to_absolute()));
                }
                acc.recheck_count += 1;
                acc.recheck_dependent_files += dependent_file_count;
                acc.recheck_changed_files += changed_file_count;
                acc.recheck_duration += duration;
            }
        }
    }
    acc
}

#[allow(dead_code)]
fn get_root(state: &ServerState) -> &FilePath {
    &get_ienv(state).i_root
}

fn get_flowconfig(state: &ServerState) -> &FlowConfig {
    &get_ienv(state).i_flowconfig
}

fn get_initialize_params(state: &ServerState) -> &lsp_types::InitializeParams {
    &get_ienv(state).i_initialize_params
}

fn command_key_of_ienv(ienv: &InitializedEnv) -> String {
    let path = ienv
        .i_initialize_params
        .root_uri
        .as_ref()
        .map(|u| u.path().to_string())
        .or_else(|| {
            #[allow(deprecated)]
            ienv.i_initialize_params.root_path.clone()
        })
        .unwrap_or_default();
    let file_url = lsp_types::Url::from_file_path(&path)
        .map(|u| u.to_string())
        .unwrap_or_else(|_| format!("file://{}", path));
    format!("org.flow:{}", file_url)
}

fn command_key_of_server_state(state: &ServerState) -> String {
    command_key_of_ienv(get_ienv(state))
}

fn command_key_of_state(state: &State) -> String {
    match state {
        State::PreInit(_) | State::PostShutdown => String::new(),
        State::Initialized(server_state) => command_key_of_server_state(server_state),
    }
}

fn get_open_files(state: &ServerState) -> &UriMap<OpenFileInfo> {
    &get_ienv(state).i_open_files
}

fn update_open_file(
    state: &mut ServerState,
    uri: DocumentUri,
    open_file_info: Option<OpenFileInfo>,
) {
    update_ienv(state, |ienv| match open_file_info {
        Some(info) => {
            ienv.i_open_files.insert(uri, info);
        }
        None => {
            ienv.i_open_files.remove(&uri);
        }
    });
}

fn update_errors(
    state: &mut ServerState,
    f: impl FnOnce(crate::lsp_errors::T) -> crate::lsp_errors::T,
) {
    update_ienv(state, |ienv| {
        let errors = std::mem::replace(&mut ienv.i_errors, crate::lsp_errors::empty());
        ienv.i_errors = f(errors);
    });
}

// Meta-specific property that is set on requests and notifications by our
// internal VS Code extension to provide end-to-end telemetry.
// https://fburl.com/code/wdoqo479
fn new_metadata(state: &State, message: &JsonrpcMessage) -> Metadata {
    let (start_lsp_state, start_lsp_state_reason, start_server_status, start_watcher_status) =
        match state {
            State::Initialized(ServerState::Connected(cenv)) => {
                let (s, w) = &cenv.c_server_status;
                (None, None, Some(s.clone()), w.clone())
            }
            State::Initialized(ServerState::Disconnected(denv))
                if denv.d_server_status.is_some() =>
            {
                let (s, w) = denv.d_server_status.as_ref().unwrap();
                (
                    Some(string_of_state(state).to_string()),
                    None,
                    Some(s.clone()),
                    Some(w.clone()),
                )
            }
            State::Initialized(ServerState::Disconnected(denv)) => (
                Some(string_of_state(state).to_string()),
                Some(denv.d_ienv.i_status.clone()),
                None,
                None,
            ),
            State::PreInit(_) | State::PostShutdown => {
                (Some(string_of_state(state).to_string()), None, None, None)
            }
        };
    let start_lsp_state_reason: Option<String> = match start_lsp_state_reason {
        None | Some(ShowStatusT::NeverShown) => None,
        Some(ShowStatusT::Shown(_, params)) => Some(params.request.message.clone()),
    };
    let lsp_id = message.id.clone();
    let activity_key = message.json.get("activityKey").cloned();
    Metadata {
        start_wall_time: message.timestamp,
        start_server_status,
        start_watcher_status,
        start_json_truncated: json_truncate(&message.json, 256, 4),
        start_lsp_state,
        start_lsp_state_reason,
        lsp_method_name: message.method_.clone(),
        lsp_id,
        activity_key,
        ..lsp_prot::empty_metadata()
    }
}

#[allow(dead_code)]
fn edata_of_exception(exn: &dyn std::error::Error) -> RemoteExceptionData {
    RemoteExceptionData {
        message: exn.to_string(),
        stack: String::new(),
    }
}

fn selectively_omit_errors(
    request_name: &str,
    response: lsp_prot::LspMessage,
) -> lsp_prot::LspMessage {
    use flow_server_env::lsp_mapper::LspMessage;
    use flow_server_env::lsp_mapper::LspResult;
    match &response {
        LspMessage::ResponseMessage(id, LspResult::ErrorResult(_, _)) => {
            let new_response = match request_name {
                "textDocument/completion" => Some(LspResult::CompletionResult(
                    lsp_types::CompletionResponse::List(lsp_types::CompletionList {
                        is_incomplete: false,
                        items: vec![],
                    }),
                )),
                "textDocument/signatureHelp" => Some(LspResult::SignatureHelpResult(None)),
                "textDocument/documentHighlight" => {
                    Some(LspResult::DocumentHighlightResult(vec![]))
                }
                _ => None,
            };
            match new_response {
                Some(result) => LspMessage::ResponseMessage(id.clone(), result),
                None => response,
            }
        }
        _ => response,
    }
}

#[allow(dead_code)]
fn get_next_event_from_server(
    _flowconfig_name: &str,
    cenv: &ConnectedEnv,
) -> Result<Option<Event>, FlowLspError> {
    match persistent_rpc(
        &cenv.c_conn,
        server_socket_rpc::ServerRequest::PersistentPoll {
            client_id: cenv.c_conn.client_id,
        },
    ) {
        Ok(server_socket_rpc::ServerResponse::PersistentPoll { message }) => {
            Ok(message.map(Event::ServerMessage))
        }
        Ok(server_socket_rpc::ServerResponse::Error { message }) => {
            Err(server_fatal_connection_exception(message))
        }
        Ok(response) => Err(server_fatal_connection_exception(format!(
            "Unexpected persistent poll response: {:?}",
            response
        ))),
        Err(err)
            if matches!(
                err.kind(),
                io::ErrorKind::TimedOut | io::ErrorKind::WouldBlock
            ) =>
        {
            Ok(None)
        }
        Err(err) => Err(server_fatal_connection_exception(err.to_string())),
    }
}

#[allow(dead_code)]
async fn get_next_event_from_client(
    state: &State,
    client: &JsonrpcQueue,
    parser: &dyn Fn(&JsonrpcMessage) -> Result<lsp_prot::LspMessage, FlowLspError>,
) -> Result<Event, FlowLspError> {
    let message = client.get_message()?;
    let lsp_message =
        parser(&message).map_err(|error| annotate_request_parse_error(&message, error))?;
    let metadata = new_metadata(state, &message);
    Ok(Event::ClientMessage(lsp_message, metadata))
}

fn get_next_event_sync(
    flowconfig_name: &str,
    state: &State,
    client: &JsonrpcQueue,
    parser: &dyn Fn(&JsonrpcMessage) -> Result<lsp_prot::LspMessage, FlowLspError>,
) -> Result<Event, FlowLspError> {
    let tick_deadline = std::time::Instant::now() + Duration::from_secs(1);
    loop {
        if let State::Initialized(ServerState::Connected(cenv)) = state {
            if let Some(event) = get_next_event_from_server(flowconfig_name, cenv)? {
                return Ok(event);
            }
        }

        let now = std::time::Instant::now();
        if now >= tick_deadline {
            return Ok(Event::Tick);
        }
        let timeout = std::cmp::min(Duration::from_millis(50), tick_deadline - now);
        match client.get_message_timeout(timeout) {
            Some(Ok(message)) => {
                let lsp_message = parser(&message)
                    .map_err(|error| annotate_request_parse_error(&message, error))?;
                let metadata = new_metadata(state, &message);
                return Ok(Event::ClientMessage(lsp_message, metadata));
            }
            Some(Err(e)) => return Err(e),
            None => {}
        }
    }
}

#[allow(dead_code)]
async fn get_next_event(
    flowconfig_name: &str,
    state: &State,
    client: &JsonrpcQueue,
    parser: &dyn Fn(&JsonrpcMessage) -> Result<lsp_prot::LspMessage, FlowLspError>,
) -> Result<Event, FlowLspError> {
    get_next_event_sync(flowconfig_name, state, client, parser)
}

fn convert_to_client_uris(state: &State, event: Event) -> Event {
    match state {
        State::PreInit(_) | State::PostShutdown => event,
        State::Initialized(server_state) => {
            let ienv = get_ienv(server_state);
            let i_initialize_params = &ienv.i_initialize_params;
            let i_root = &ienv.i_root;
            match event {
                Event::ServerMessage(msg) => {
                    let client_path = lsp_helpers_get_root(i_initialize_params)
                        .expect("initialized LSP state must have a root");
                    let client_root = format!(
                        "{}/",
                        lsp_types::Url::from_file_path(&client_path)
                            .unwrap_or_else(|_| lsp_types::Url::parse("file:///").unwrap())
                    );
                    let server_root = format!(
                        "{}/",
                        lsp_types::Url::from_file_path(i_root)
                            .unwrap_or_else(|_| lsp_types::Url::parse("file:///").unwrap())
                    );
                    let mut mapper = lsp_mapper::default_mapper();
                    let client_root_clone = client_root.clone();
                    let server_root_clone = server_root.clone();
                    mapper.of_document_uri = Box::new(move |_mapper, uri| {
                        let uri_str = uri.as_str().to_string();
                        if let Some(relative) = uri_str.strip_prefix(&server_root_clone) {
                            let new_uri_str = format!("{}{}", client_root_clone, relative);
                            lsp_types::Url::parse(&new_uri_str).unwrap_or(uri)
                        } else {
                            uri
                        }
                    });
                    #[allow(clippy::arc_with_non_send_sync)]
                    let mapper = std::sync::Arc::new(mapper);
                    let server_mapper = lsp_prot::default_message_from_server_mapper(mapper);
                    Event::ServerMessage((server_mapper.of_message_from_server)(
                        &server_mapper,
                        msg,
                    ))
                }
                Event::ClientMessage(msg, metadata) => Event::ClientMessage(msg, metadata),
                Event::Tick => Event::Tick,
            }
        }
    }
}

fn convert_to_server_uris(request: lsp_prot::Request) -> lsp_prot::Request {
    let server_uri_of_client_uri = |uri: DocumentUri| -> DocumentUri {
        match uri.to_file_path() {
            Ok(path) => {
                let canonical = std::fs::canonicalize(&path).unwrap_or(path);
                lsp_types::Url::from_file_path(&canonical).unwrap_or(uri)
            }
            Err(_) => uri,
        }
    };
    let mut client_to_server_mapper = lsp_mapper::default_mapper();
    client_to_server_mapper.of_document_uri =
        Box::new(move |_mapper, uri| server_uri_of_client_uri(uri));
    match request {
        lsp_prot::Request::Subscribe => lsp_prot::Request::Subscribe,
        lsp_prot::Request::LspToServer(msg) => {
            let mapped = (client_to_server_mapper.of_lsp_message)(&client_to_server_mapper, msg);
            lsp_prot::Request::LspToServer(mapped)
        }
        lsp_prot::Request::LiveErrorsRequest(uri) => {
            let mapped_uri =
                (client_to_server_mapper.of_document_uri)(&client_to_server_mapper, uri);
            lsp_prot::Request::LiveErrorsRequest(mapped_uri)
        }
    }
}

fn send_request_to_client(
    ienv: &mut InitializedEnv,
    id: LspId,
    request: lsp_mapper::LspRequest,
    on_response: Box<
        dyn FnOnce(lsp_mapper::LspResult, &mut ServerState) -> Result<(), FlowLspError>,
    >,
    on_error: Box<
        dyn FnOnce(lsp_mapper::lsp_error::T, String, &mut ServerState) -> Result<(), FlowLspError>,
    >,
) {
    let json = {
        let key = command_key_of_ienv(ienv);
        lsp_fmt_print_lsp(
            &key,
            &lsp_prot::LspMessage::RequestMessage(id.clone(), request.clone()),
        )
    };
    to_stdout(&json);

    let handlers = LspHandler {
        on_response,
        on_error,
    };
    ienv.i_outstanding_local_requests
        .insert(id.clone(), request);
    ienv.i_outstanding_local_handlers.insert(id, handlers);
}

fn show_status_params_eq(
    a: &lsp_mapper::show_status::Params,
    b: &lsp_mapper::show_status::Params,
) -> bool {
    a.request.typ == b.request.typ
        && a.request.message == b.request.message
        && a.progress == b.progress
        && a.total == b.total
        && a.short_message == b.short_message
}

fn should_send_status(
    ienv: &InitializedEnv,
    status: &lsp_mapper::show_status::Params,
) -> (bool, bool) {
    let use_status = lsp_helpers_supports_status(&ienv.i_initialize_params);
    let (will_dismiss_old, will_show_new) = match (use_status, &ienv.i_status, status) {
        (_, ShowStatusT::Shown(_, existing_status), status)
            if show_status_params_eq(existing_status, status) =>
        {
            (false, false)
        }
        (true, _, _) => (false, true),
        (false, ShowStatusT::Shown(_, existing), status)
            if existing.request.typ == MessageType::ERROR
                && status.request.typ == MessageType::ERROR =>
        {
            (false, false)
        }
        (false, ShowStatusT::Shown(id, _), status) if status.request.typ == MessageType::ERROR => {
            (id.is_some(), true)
        }
        (false, ShowStatusT::Shown(id, _), _) => (id.is_some(), false),
        (false, ShowStatusT::NeverShown, status) if status.request.typ == MessageType::ERROR => {
            (false, true)
        }
        (false, ShowStatusT::NeverShown, _) => (false, false),
    };
    (will_dismiss_old, will_show_new)
}

fn show_status(
    ienv: &mut InitializedEnv,
    type_: MessageType,
    message: &str,
    short_message: Option<&str>,
    progress: Option<i32>,
    total: Option<i32>,
    titles: &[&str],
    handler: Option<Box<dyn FnOnce(&str, &mut ServerState)>>,
    background_color: Option<lsp_mapper::show_status::ShowStatusBackgroundColor>,
) {
    let use_status = lsp_helpers_supports_status(&ienv.i_initialize_params);
    let actions: Vec<lsp_types::MessageActionItem> = titles
        .iter()
        .map(|title| lsp_types::MessageActionItem {
            title: title.to_string(),
            properties: Default::default(),
        })
        .collect();
    let params = lsp_mapper::show_status::Params {
        request: lsp_types::ShowMessageRequestParams {
            typ: type_,
            message: message.to_string(),
            actions: Some(actions),
        },
        short_message: short_message.map(|s| s.to_string()),
        progress,
        total,
        background_color,
    };
    let (will_dismiss_old, will_show_new) = should_send_status(ienv, &params);
    if will_dismiss_old {
        if let ShowStatusT::Shown(Some(id), existing_params) = &ienv.i_status {
            let existing_params = existing_params.clone();
            let id = id.clone();
            let notification =
                lsp_mapper::LspNotification::CancelRequestNotification(lsp_types::CancelParams {
                    id,
                });
            let json = {
                let key = command_key_of_ienv(ienv);
                lsp_fmt_print_lsp(
                    &key,
                    &lsp_prot::LspMessage::NotificationMessage(notification),
                )
            };
            to_stdout(&json);
            ienv.i_status = ShowStatusT::Shown(None, existing_params);
        }
    }
    if !will_show_new {
        return;
    }
    let id = NumberOrString::Number(jsonrpc_get_next_request_id());
    let request = if use_status {
        lsp_mapper::LspRequest::ShowStatusRequest(params.clone())
    } else {
        lsp_mapper::LspRequest::ShowMessageRequestRequest(params.request.clone())
    };
    let mark_ienv_shown = |id: LspId, future_ienv: &mut InitializedEnv| {
        if let ShowStatusT::Shown(Some(ref future_id), ref future_params) = future_ienv.i_status {
            if *future_id == id {
                let future_params = future_params.clone();
                future_ienv.i_status = ShowStatusT::Shown(None, future_params);
            }
        }
    };
    let on_error: Box<
        dyn FnOnce(lsp_mapper::lsp_error::T, String, &mut ServerState) -> Result<(), FlowLspError>,
    > = {
        let id = id.clone();
        Box::new(move |_e, _msg, state| {
            update_ienv(state, |ienv| mark_ienv_shown(id.clone(), ienv));
            Ok(())
        })
    };
    let on_response: Box<
        dyn FnOnce(lsp_mapper::LspResult, &mut ServerState) -> Result<(), FlowLspError>,
    > = {
        let id = id.clone();
        Box::new(move |result, state| {
            update_ienv(state, |ienv| mark_ienv_shown(id.clone(), ienv));
            let title = match &result {
                lsp_mapper::LspResult::ShowStatusResult(r) => {
                    r.as_ref().map(|item| item.title.clone())
                }
                lsp_mapper::LspResult::ShowMessageRequestResult(r) => {
                    r.as_ref().map(|item| item.title.clone())
                }
                _ => None,
            };
            if let Some(title) = title {
                if let Some(handler) = handler {
                    handler(&title, state);
                }
            }
            Ok(())
        })
    };
    send_request_to_client(ienv, id.clone(), request, on_response, on_error);
    ienv.i_status = ShowStatusT::Shown(Some(id), params);
}

// calls realpath on every DocumentUri
fn send_to_server(
    env: &ConnectedEnv,
    request: lsp_prot::Request,
    metadata: &Metadata,
) -> Result<(), FlowLspError> {
    let request = convert_to_server_uris(request);
    match persistent_rpc(
        &env.c_conn,
        server_socket_rpc::ServerRequest::PersistentRequest {
            client_id: env.c_conn.client_id,
            request: (request, metadata.clone()),
        },
    ) {
        Ok(server_socket_rpc::ServerResponse::PersistentAck) => Ok(()),
        Ok(server_socket_rpc::ServerResponse::Error { message }) => {
            Err(server_fatal_connection_exception(message))
        }
        Ok(response) => Err(server_fatal_connection_exception(format!(
            "Unexpected persistent request response: {:?}",
            response
        ))),
        Err(err) => Err(server_fatal_connection_exception(err.to_string())),
    }
}

fn send_lsp_to_server(
    cenv: &ConnectedEnv,
    metadata: &Metadata,
    message: lsp_prot::LspMessage,
) -> Result<(), FlowLspError> {
    send_to_server(cenv, lsp_prot::Request::LspToServer(message), metadata)
}

fn send_configuration_to_server(
    method_name: &str,
    settings: serde_json::Value,
    cenv: &ConnectedEnv,
) -> Result<(), FlowLspError> {
    use flow_server_env::lsp_mapper::LspMessage;
    use flow_server_env::lsp_mapper::LspNotification;
    let metadata = Metadata {
        start_wall_time: now(),
        start_json_truncated: serde_json::json!({"method": method_name}),
        lsp_method_name: method_name.to_string(),
        ..lsp_prot::empty_metadata()
    };
    let msg = LspMessage::NotificationMessage(LspNotification::DidChangeConfigurationNotification(
        lsp_types::DidChangeConfigurationParams { settings },
    ));
    send_lsp_to_server(cenv, &metadata, msg)
}

fn request_configuration(ienv: &mut InitializedEnv) {
    let id = NumberOrString::Number(jsonrpc_get_next_request_id());
    let request = lsp_mapper::LspRequest::ConfigurationRequest(lsp_types::ConfigurationParams {
        items: vec![lsp_types::ConfigurationItem {
            scope_uri: None,
            section: Some("flow".to_string()),
        }],
    });
    let on_response: Box<
        dyn FnOnce(lsp_mapper::LspResult, &mut ServerState) -> Result<(), FlowLspError>,
    > = Box::new(|result, state| {
        if let lsp_mapper::LspResult::ConfigurationResult(configs) = result {
            if configs.len() == 1 {
                let i_config = configs.into_iter().next().unwrap();
                let config_clone = i_config.clone();
                update_ienv(state, |ienv| {
                    ienv.i_config = config_clone;
                });
                if let ServerState::Connected(cenv) = state {
                    send_configuration_to_server(
                        "synthetic/didChangeConfiguration",
                        i_config,
                        cenv,
                    )?;
                }
            }
        }
        Ok(())
    });
    let on_error: Box<
        dyn FnOnce(lsp_mapper::lsp_error::T, String, &mut ServerState) -> Result<(), FlowLspError>,
    > = Box::new(|_e, _msg, _state| Ok(()));
    send_request_to_client(ienv, id, request, on_response, on_error);
}

fn subscribe_to_config_changes(ienv: &mut InitializedEnv) {
    let id = NumberOrString::Number(jsonrpc_get_next_request_id());
    let request = lsp_mapper::LspRequest::RegisterCapabilityRequest(
        lsp_mapper::register_capability::Params {
            registrations: vec![lsp_mapper::register_capability::Registration {
                id: "didChangeConfiguration".to_string(),
                method: "workspace/didChangeConfiguration".to_string(),
                register_options: lsp_mapper::register_capability::Options::DidChangeConfiguration,
            }],
        },
    );
    let on_response: Box<
        dyn FnOnce(lsp_mapper::LspResult, &mut ServerState) -> Result<(), FlowLspError>,
    > = Box::new(|_result, _state| Ok(()));
    let on_error: Box<
        dyn FnOnce(lsp_mapper::lsp_error::T, String, &mut ServerState) -> Result<(), FlowLspError>,
    > = Box::new(|_e, _msg, _state| Ok(()));
    send_request_to_client(ienv, id, request, on_response, on_error);
}

fn do_initialize(params: &lsp_types::InitializeParams) -> lsp_types::InitializeResult {
    let supported_kinds = lsp_helpers_supports_code_action_kinds(params);
    let supports_quickfixes = supported_kinds.iter().any(|k| k == "quickfix");
    let supports_refactor_extract = supported_kinds.iter().any(|k| k == "refactor.extract");
    let supports_source_actions = supported_kinds.iter().any(|k| k == "source");
    let mut supported_code_action_kinds = vec![];
    if supports_quickfixes {
        supported_code_action_kinds.push(lsp_types::CodeActionKind::QUICKFIX);
    }
    if supports_refactor_extract {
        supported_code_action_kinds.insert(0, lsp_types::CodeActionKind::REFACTOR_EXTRACT);
    }
    if supports_source_actions {
        supported_code_action_kinds.insert(
            0,
            lsp_types::CodeActionKind::new("source.organizeImports.flow"),
        );
        supported_code_action_kinds.insert(
            0,
            lsp_types::CodeActionKind::new("source.addMissingImports.flow"),
        );
    }
    let code_action_provider = if !supported_code_action_kinds.is_empty() {
        Some(lsp_types::CodeActionProviderCapability::Options(
            lsp_types::CodeActionOptions {
                code_action_kinds: Some(supported_code_action_kinds),
                ..Default::default()
            },
        ))
    } else {
        Some(lsp_types::CodeActionProviderCapability::Simple(false))
    };
    let text_document_sync = Some(lsp_types::TextDocumentSyncCapability::Options(
        lsp_types::TextDocumentSyncOptions {
            open_close: Some(true),
            change: Some(lsp_types::TextDocumentSyncKind::INCREMENTAL),
            will_save: Some(false),
            will_save_wait_until: Some(false),
            save: Some(lsp_types::TextDocumentSyncSaveOptions::SaveOptions(
                lsp_types::SaveOptions {
                    include_text: Some(false),
                },
            )),
        },
    ));
    let server_snippet_text_edit = lsp_helpers_supports_experimental_snippet_text_edit(params);
    let experimental = serde_json::json!({
        "strictCompletionOrder": true,
        "autoCloseJsx": true,
        "pasteProvider": true,
        "renameFileImports": true,
        "llmContextProvider": true,
        "snippetTextEdit": server_snippet_text_edit,
    });
    lsp_types::InitializeResult {
        capabilities: lsp_types::ServerCapabilities {
            text_document_sync,
            hover_provider: Some(lsp_types::HoverProviderCapability::Simple(true)),
            completion_provider: Some(lsp_types::CompletionOptions {
                resolve_provider: Some(false),
                trigger_characters: Some(vec![
                    ".".to_string(),
                    "[".to_string(),
                    " ".to_string(),
                    "<".to_string(),
                    "/".to_string(),
                    "\"".to_string(),
                    "'".to_string(),
                    "#".to_string(),
                    "*".to_string(),
                ]),
                completion_item: Some(lsp_types::CompletionOptionsCompletionItem {
                    label_details_support: Some(true),
                }),
                ..Default::default()
            }),
            signature_help_provider: Some(lsp_types::SignatureHelpOptions {
                trigger_characters: Some(vec!["(".to_string(), ",".to_string(), "{".to_string()]),
                ..Default::default()
            }),
            definition_provider: Some(lsp_types::OneOf::Left(true)),
            type_definition_provider: Some(lsp_types::TypeDefinitionProviderCapability::Simple(
                false,
            )),
            references_provider: Some(lsp_types::OneOf::Left(true)),
            document_highlight_provider: Some(lsp_types::OneOf::Left(true)),
            document_symbol_provider: Some(lsp_types::OneOf::Left(true)),
            workspace_symbol_provider: Some(lsp_types::OneOf::Left(true)),
            code_action_provider,
            code_lens_provider: None,
            document_formatting_provider: Some(lsp_types::OneOf::Left(false)),
            document_range_formatting_provider: Some(lsp_types::OneOf::Left(false)),
            document_on_type_formatting_provider: None,
            rename_provider: Some(lsp_types::OneOf::Right(lsp_types::RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: Default::default(),
            })),
            document_link_provider: None,
            execute_command_provider: Some(lsp_types::ExecuteCommandOptions {
                commands: vec![
                    "log".to_string(),
                    "source.addMissingImports".to_string(),
                    "source.organizeImports".to_string(),
                ],
                ..Default::default()
            }),
            implementation_provider: Some(lsp_types::ImplementationProviderCapability::Simple(
                false,
            )),
            selection_range_provider: Some(lsp_types::SelectionRangeProviderCapability::Simple(
                true,
            )),
            linked_editing_range_provider: Some(
                lsp_types::LinkedEditingRangeServerCapabilities::Simple(true),
            ),
            workspace: Some(lsp_types::WorkspaceServerCapabilities {
                file_operations: Some(Default::default()),
                ..Default::default()
            }),
            experimental: Some(experimental),
            ..Default::default()
        },
        server_info: Some(lsp_types::ServerInfo {
            name: "Flow".to_string(),
            version: Some(flow_version().to_string()),
        }),
    }
}

fn message_with_flow_and_root_name_prefix(flowconfig: &FlowConfig, msg: &str) -> String {
    match &flowconfig.options.root_name {
        None => format!("Flow: {}", msg),
        Some(root) => format!("Flow ({}): {}", root, msg),
    }
}

fn show_connected_status(cenv: &mut ConnectedEnv) {
    let flowconfig = cenv.c_ienv.i_flowconfig.clone();
    let message_with_prefix = |msg: &str| message_with_flow_and_root_name_prefix(&flowconfig, msg);
    let (type_, message, short_message, progress, total) = if cenv.c_is_rechecking {
        let (ref server_status, _) = cenv.c_server_status;
        if !server_status::is_free(server_status) {
            let (sm, progress, total) = server_status::get_progress(server_status);
            let message = message_with_prefix(&server_status::string_of_status(
                false,
                false,
                server_status,
            ));
            (
                MessageType::WARNING,
                message,
                sm.map(|s| message_with_prefix(&s)),
                progress,
                total,
            )
        } else {
            (
                MessageType::WARNING,
                message_with_prefix("Server is rechecking..."),
                None,
                None,
                None,
            )
        }
    } else {
        let (_, ref watcher_status) = cenv.c_server_status;
        match watcher_status {
            Some((_, file_watcher_status::StatusKind::Deferred { reason })) => {
                let message = format!("Waiting for {} to finish", reason);
                let short_message = Some(message_with_prefix("blocked"));
                (MessageType::WARNING, message, short_message, None, None)
            }
            _ => {
                let message = match &cenv.c_lazy_stats {
                    Some(stats) if stats.checked_files < stats.total_files && stats.lazy_mode => {
                        let checked_source = stats.checked_files - stats.checked_libdef_files;
                        let total_source = stats.total_files - stats.total_libdef_files;
                        let libdef_msg = format!(
                            " + {}/{} libdefs",
                            stats.checked_libdef_files, stats.total_libdef_files
                        );
                        format!(
                            "Flow is ready. Checking {}/{} source files{} (lazy mode)",
                            checked_source, total_source, libdef_msg,
                        )
                    }
                    _ => "Flow is ready.".to_string(),
                };
                let short_message = Some(message_with_prefix("ready"));
                (MessageType::INFO, message, short_message, None, None)
            }
        }
    };
    show_status(
        &mut cenv.c_ienv,
        type_,
        &message,
        short_message.as_deref(),
        progress,
        total,
        &[],
        None,
        None,
    );
}

// report that we're connected to telemetry/connectionStatus
// show green status
fn show_connected(env: &mut ConnectedEnv) {
    let i_is_connected = lsp_writers_notify_connection_status(
        &env.c_ienv.i_initialize_params,
        to_stdout,
        env.c_ienv.i_is_connected,
        true,
    );
    env.c_ienv.i_is_connected = i_is_connected;
    show_connected_status(env);
}

fn show_connecting(reason: &ConnectError, env: &mut DisconnectedEnv) {
    if *reason == ConnectError::ServerMissing {
        lsp_writers_log_info(to_stdout, "Starting Flow server");
    }
    let flowconfig = env.d_ienv.i_flowconfig.clone();
    let message_with_prefix = |msg: &str| message_with_flow_and_root_name_prefix(&flowconfig, msg);
    let (message, short_message, progress, total) = match (reason, &env.d_server_status) {
        (ConnectError::ServerMissing, _) => {
            (message_with_prefix("Server starting"), None, None, None)
        }
        (ConnectError::ServerSocketMissing, _) => {
            (message_with_prefix("Server starting?"), None, None, None)
        }
        (ConnectError::BuildIdMismatch(BuildIdMismatchKind::ServerExited), _) => (
            message_with_prefix("Server was wrong version and exited"),
            None,
            None,
            None,
        ),
        (ConnectError::BuildIdMismatch(BuildIdMismatchKind::ClientShouldError { .. }), _) => (
            message_with_prefix("Server is wrong version"),
            None,
            None,
            None,
        ),
        (ConnectError::ServerBusy(ServerBusyKind::TooManyClients), _) => {
            (message_with_prefix("Server busy"), None, None, None)
        }
        (ConnectError::ServerBusy(_), None) => {
            (message_with_prefix("Server busy"), None, None, None)
        }
        (ConnectError::ServerBusy(_), Some((server_status, watcher_status))) => {
            if !server_status::is_free(server_status) {
                let (sm, progress, total) = server_status::get_progress(server_status);
                (
                    message_with_prefix(&server_status::string_of_status(
                        false,
                        false,
                        server_status,
                    )),
                    sm,
                    progress,
                    total,
                )
            } else {
                (
                    message_with_prefix(&file_watcher_status::string_of_status(watcher_status)),
                    None,
                    None,
                    None,
                )
            }
        }
    };
    show_status(
        &mut env.d_ienv,
        MessageType::WARNING,
        &message,
        short_message.as_deref(),
        progress,
        total,
        &[],
        None,
        None,
    );
}

// report that we're disconnected to telemetry/connectionStatus
fn show_disconnected(
    code: Option<&FlowExitStatus>,
    message: Option<&str>,
    env: &mut DisconnectedEnv,
) {
    let i_is_connected = lsp_writers_notify_connection_status(
        &env.d_ienv.i_initialize_params,
        to_stdout,
        env.d_ienv.i_is_connected,
        false,
    );
    env.d_ienv.i_is_connected = i_is_connected;
    let flowconfig = env.d_ienv.i_flowconfig.clone();
    let message_with_prefix = |msg: &str| message_with_flow_and_root_name_prefix(&flowconfig, msg);
    let message = message
        .map(|m| m.to_string())
        .unwrap_or_else(|| message_with_prefix("server is stopped"));
    let message = match code {
        Some(code) => format!(
            "{} [{}]",
            message,
            flow_common_exit_status::to_string(*code)
        ),
        None => message,
    };
    let handler: Box<dyn FnOnce(&str, &mut ServerState)> =
        Box::new(|r: &str, state: &mut ServerState| {
            if let ServerState::Disconnected(e) = state {
                if r == "Restart" {
                    e.d_autostart = true;
                }
            }
        });
    show_status(
        &mut env.d_ienv,
        MessageType::ERROR,
        &message,
        None,
        None,
        None,
        &["Restart"],
        Some(handler),
        Some(lsp_mapper::show_status::ShowStatusBackgroundColor::Error),
    );
}

fn close_conn(env: &ConnectedEnv) {
    if let Ok(stream) = env.c_conn.stream.lock() {
        let _ = stream.shutdown(Shutdown::Both);
    }
}

fn track_to_server(
    state: &mut ServerState,
    c: &lsp_prot::LspMessage,
) -> Result<TrackEffect, FlowLspError> {
    use flow_server_env::lsp_mapper::LspMessage;
    use flow_server_env::lsp_mapper::LspNotification;
    let changed_live_uri = match c {
        LspMessage::NotificationMessage(LspNotification::DidOpenNotification(params)) => {
            let o_open_doc = params.text_document.clone();
            let uri = params.text_document.uri.clone();
            update_open_file(
                state,
                uri.clone(),
                Some(OpenFileInfo {
                    o_open_doc,
                    o_ast: None,
                    o_unsaved: false,
                }),
            );
            Some(uri)
        }
        LspMessage::NotificationMessage(LspNotification::DidCloseNotification(params)) => {
            let uri = params.text_document.uri.clone();
            update_open_file(state, uri.clone(), None);
            update_errors(state, |errors| {
                crate::lsp_errors::clear_all_live_errors_and_send(
                    &mut |v| to_stdout(&v),
                    &uri,
                    errors,
                )
            });
            None
        }
        LspMessage::NotificationMessage(LspNotification::DidChangeNotification(params)) => {
            let uri = params.text_document.uri.clone();
            let open_files = get_open_files(state);
            let o_open_doc = match open_files.get(&uri) {
                Some(info) => info.o_open_doc.clone(),
                None => return Err(FlowLspError::ChangedFileNotOpen(uri)),
            };
            let text = lsp_helpers_apply_changes_unsafe(&o_open_doc.text, &params.content_changes);
            let o_open_doc = lsp_types::TextDocumentItem {
                uri: uri.clone(),
                language_id: o_open_doc.language_id.clone(),
                version: params.text_document.version,
                text,
            };
            update_open_file(
                state,
                uri.clone(),
                Some(OpenFileInfo {
                    o_open_doc,
                    o_ast: None,
                    o_unsaved: true,
                }),
            );
            if let ServerState::Connected(_) = state {
                update_errors(state, |errors| {
                    crate::lsp_errors::update_errors_due_to_change_and_send(
                        &mut |v| to_stdout(&v),
                        params,
                        errors,
                    )
                });
            }
            Some(uri)
        }
        LspMessage::NotificationMessage(LspNotification::DidSaveNotification(params)) => {
            let uri = params.text_document.uri.clone();
            let open_files = get_open_files(state);
            let open_file = open_files.get(&uri).cloned();
            if let Some(mut open_file) = open_file {
                open_file.o_unsaved = false;
                update_open_file(state, uri.clone(), Some(open_file));
            }
            Some(uri)
        }
        _ => None,
    };
    match (state, c) {
        (ServerState::Connected(env), LspMessage::RequestMessage(id, _)) => {
            env.c_outstanding_requests_to_server.insert(id.clone());
        }
        (ServerState::Connected(env), LspMessage::ResponseMessage(id, _)) => {
            let wrapped = decode_wrapped(id);
            env.c_ienv
                .i_outstanding_requests_from_server
                .remove(&wrapped);
        }
        _ => {}
    }
    Ok(TrackEffect { changed_live_uri })
}

fn track_from_server(state: &mut ServerState, c: &lsp_prot::LspMessage) {
    use flow_server_env::lsp_mapper::LspMessage;
    match (state, c) {
        (ServerState::Connected(env), LspMessage::ResponseMessage(id, _)) => {
            env.c_outstanding_requests_to_server.remove(id);
        }
        (ServerState::Connected(env), LspMessage::RequestMessage(id, params)) => {
            let wrapped = WrappedId {
                server_id: env.c_ienv.i_server_id,
                message_id: id.clone(),
            };
            env.c_ienv
                .i_outstanding_requests_from_server
                .insert(wrapped, params.clone());
        }
        (_, _) => {}
    }
}

fn lsp_document_item_to_flow(
    open_doc: &lsp_types::TextDocumentItem,
) -> Result<FileInput, FlowLspError> {
    let uri = &open_doc.uri;
    let fn_ = lsp_helpers_lsp_uri_to_path(uri)?;
    let fn_ = sys_utils_realpath(&fn_).unwrap_or(fn_);
    Ok(FileInput::FileContent(Some(fn_), open_doc.text.clone()))
}

fn loc_to_lsp_range(loc: &flow_parser::loc::Loc) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_types::Position {
            line: loc.start.line.saturating_sub(1) as u32,
            character: loc.start.column.max(0) as u32,
        },
        end: lsp_types::Position {
            line: loc.end.line.saturating_sub(1) as u32,
            character: loc.end.column.max(0) as u32,
        },
    }
}

fn diagnostic_of_parse_error(
    loc: &flow_parser::loc::Loc,
    parse_error: &flow_parser::parse_error::ParseError,
) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic {
        range: loc_to_lsp_range(loc),
        severity: Some(lsp_types::DiagnosticSeverity::ERROR),
        code: Some(lsp_types::NumberOrString::String("ParseError".to_string())),
        source: Some("Flow".to_string()),
        tags: None,
        message: parse_error.to_string(),
        related_information: None,
        code_description: None,
        data: None,
    }
}

fn live_syntax_errors_enabled(state: &ServerState) -> bool {
    let params = get_initialize_params(state);
    params
        .initialization_options
        .as_ref()
        .and_then(|opts| opts.get("liveSyntaxErrors"))
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

fn parse_and_cache(
    state: &mut ServerState,
    uri: &DocumentUri,
) -> Result<
    (
        flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
        Option<Vec<lsp_types::Diagnostic>>,
    ),
    FlowLspError,
> {
    let flowconfig = get_flowconfig(state);
    let use_strict = flowconfig.options.modules_are_use_strict;
    let module_ref_prefix = flowconfig.options.haste_module_ref_prefix.clone();
    let assert_operator = flowconfig.options.assert_operator;
    let parse_options = Some(flow_parser::ParseOptions {
        use_strict,
        module_ref_prefix: module_ref_prefix.map(|s| s.into()),
        assert_operator: assert_operator.parse(),
        ..flow_parser::PERMISSIVE_PARSE_OPTIONS
    });

    let parse = |file: &FileInput| -> (
        flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>,
        Option<Vec<lsp_types::Diagnostic>>,
    ) {
        let content = file.content_of_file_input_unsafe();
        let filename_opt = file.path_of_file_input();
        let filekey = filename_opt.map(flow_parser::file_key::FileKey::source_file_of_absolute);
        let (program, errors) = match filekey {
            Some(fk) => flow_parser::parse_program_file::<()>(
                false,
                None,
                parse_options.clone(),
                fk,
                Ok(content.as_str()),
            ),
            None => flow_parser::parse_program_without_file(
                false,
                None,
                parse_options.clone(),
                Ok(content.as_str()),
            ),
        };
        (
            program,
            if live_syntax_errors_enabled(state) {
                Some(
                    errors
                        .iter()
                        .map(|(loc, err)| diagnostic_of_parse_error(loc, err))
                        .collect(),
                )
            } else {
                None
            },
        )
    };

    let open_files = get_open_files(state);
    let existing_open_file_info = open_files.get(uri).cloned();
    match existing_open_file_info {
        Some(OpenFileInfo {
            o_ast: Some(o_ast), ..
        }) => Ok(o_ast),
        Some(OpenFileInfo {
            o_open_doc,
            o_unsaved,
            ..
        }) => {
            let file = lsp_document_item_to_flow(&o_open_doc)?;
            let o_ast = parse(&file);
            let open_file_info = Some(OpenFileInfo {
                o_open_doc,
                o_ast: Some(o_ast.clone()),
                o_unsaved,
            });
            update_open_file(state, uri.clone(), open_file_info);
            Ok(o_ast)
        }
        None => {
            let fn_ = lsp_helpers_lsp_uri_to_path(uri)?;
            let fn_ = sys_utils_realpath(&fn_).unwrap_or(fn_);
            let file = FileInput::FileName(fn_);
            let (open_ast, _) = parse(&file);
            Ok((open_ast, None))
        }
    }
}

fn do_document_symbol(
    state: &mut ServerState,
    _id: &LspId,
    params: &lsp_types::DocumentSymbolParams,
) -> Result<(), FlowLspError> {
    use flow_server_env::lsp_mapper::LspMessage;
    use flow_server_env::lsp_mapper::LspResult;
    use flow_server_env::lsp_mapper::document_symbol_result;
    let uri = &params.text_document.uri;
    let (ast, _live_parse_errors) = parse_and_cache(state, uri)?;
    let supports_hierarchical =
        lsp_helpers_supports_hierarchical_document_symbol(get_initialize_params(state));
    let result = if supports_hierarchical {
        document_symbol_result::T::DocumentSymbol(
            crate::document_symbol_provider::provide_document_symbols(&ast),
        )
    } else {
        document_symbol_result::T::SymbolInformation(
            crate::document_symbol_provider::provide_symbol_information(uri, &ast),
        )
    };
    let response =
        LspMessage::ResponseMessage(_id.clone(), LspResult::DocumentSymbolResult(result));
    let key = command_key_of_server_state(state);
    let json = lsp_fmt_print_lsp(&key, &response);
    to_stdout(&json);
    Ok(())
}

fn do_selection_range(
    state: &mut ServerState,
    _id: &LspId,
    params: &lsp_types::SelectionRangeParams,
) -> Result<(), FlowLspError> {
    use flow_server_env::lsp_mapper::LspMessage;
    use flow_server_env::lsp_mapper::LspResult;
    let uri = &params.text_document.uri;
    let positions = &params.positions;
    let (ast, _live_parse_errors) = parse_and_cache(state, uri)?;
    let response = crate::selection_range_provider::provide_selection_ranges(positions, &ast);
    let lsp_result = match response {
        Ok(ranges) => LspResult::SelectionRangeResult(ranges),
        Err(msg) => LspResult::ErrorResult(
            lsp_mapper::lsp_error::T {
                code: lsp_mapper::lsp_error::Code::InternalError,
                message: msg,
                data: None,
            },
            String::new(),
        ),
    };
    let outgoing = LspMessage::ResponseMessage(_id.clone(), lsp_result);
    let key = command_key_of_server_state(state);
    let json = lsp_fmt_print_lsp(&key, &outgoing);
    to_stdout(&json);
    Ok(())
}

fn do_rage(_flowconfig_name: &str, state: &ServerState) -> Vec<RageItem> {
    let mut items: Vec<RageItem> = vec![];
    let _ienv = get_ienv(state);
    items.push(RageItem {
        title: None,
        data: format!(
            "LSP adapter state: {}\n",
            crate::rage_print::string_of_state(state)
        ),
    });
    items
}

fn parse_json(state: &State, msg: &JsonrpcMessage) -> Result<lsp_prot::LspMessage, FlowLspError> {
    let json = &msg.json;
    let outstanding = |id: &LspId| -> Result<lsp_mapper::LspRequest, FlowLspError> {
        let ienv = match state {
            State::PreInit(_) => {
                return Err(internal_error_exception(format!(
                    "Unexpected LSP response before init: {}",
                    json
                )));
            }
            State::PostShutdown => {
                return Err(internal_error_exception(format!(
                    "Unexpected LSP response after shutdown: {}",
                    json
                )));
            }
            State::Initialized(server_state) => get_ienv(server_state),
        };
        if let Some(msg) = ienv.i_outstanding_local_requests.get(id) {
            return Ok(msg.clone());
        }
        let wrapped = try_decode_wrapped(id)?;
        if let Some(msg) = ienv.i_outstanding_requests_from_server.get(&wrapped) {
            return Ok(msg.clone());
        }
        Err(internal_error_exception(format!(
            "Wasn't expecting a response to request {}",
            lsp_fmt_id_to_string(id)
        )))
    };
    lsp_fmt_parse_lsp(json, &outstanding)
}

fn with_timer<T>(f: impl FnOnce() -> T) -> (f64, T) {
    let start = now();
    let ret = f();
    let duration = now() - start;
    (duration, ret)
}

fn collect_interaction_state(state: &ServerState) -> InteractionState {
    let time = now();
    let files = get_open_files(state);
    let buffer_status = if files.is_empty() {
        InteractionBufferStatus::NoOpenBuffers
    } else if files.values().any(|file| file.o_unsaved) {
        InteractionBufferStatus::UnsavedBuffers
    } else {
        InteractionBufferStatus::NoUnsavedBuffers
    };
    let server_status = match state {
        ServerState::Disconnected(disconnected_env) => {
            if disconnected_env.d_server_status.is_none() {
                InteractionServerStatus::Stopped
            } else {
                InteractionServerStatus::Initializing
            }
        }
        ServerState::Connected(connected_env) => {
            if connected_env.c_is_rechecking {
                InteractionServerStatus::Rechecking
            } else {
                InteractionServerStatus::Ready
            }
        }
    };
    InteractionState {
        time,
        server_status,
        buffer_status,
    }
}

fn gc_pending_interactions(state: &State) {
    use std::sync::atomic::AtomicU64;
    use std::sync::atomic::Ordering;
    static NEXT_GC: AtomicU64 = AtomicU64::new(0);
    match state {
        State::PreInit(_) | State::PostShutdown => {}
        State::Initialized(server_state) => {
            let next_gc = f64::from_bits(NEXT_GC.load(Ordering::SeqCst));
            if now() >= next_gc {
                let new_next_gc = lsp_interaction::gc(&|| collect_interaction_state(server_state));
                NEXT_GC.store(new_next_gc.to_bits(), Ordering::SeqCst);
            }
        }
    }
}

fn start_interaction(trigger: InteractionTrigger, state: &ServerState) -> InteractionId {
    let start_state = collect_interaction_state(state);
    lsp_interaction::start(start_state, trigger)
}

fn log_interaction(ux: InteractionUx, state: &ServerState, id: InteractionId) {
    let end_state = collect_interaction_state(state);
    lsp_interaction::log(end_state, ux, id)
}

fn dismiss_tracks(state: &mut ServerState) {
    let key = command_key_of_server_state(state);
    let decline_request_to_server = |id: &LspId| {
        let e = flow_server_env::lsp_mapper::lsp_error::T {
            code: flow_server_env::lsp_mapper::lsp_error::Code::RequestCancelled,
            message: "Connection to server has been lost".to_string(),
            data: None,
        };
        let result = flow_server_env::lsp_mapper::LspResult::ErrorResult(e, String::new());
        let json = lsp_fmt_print_lsp_response(&key, id, &result);
        to_stdout(&json);
    };
    lsp_interaction::dismiss_tracks(collect_interaction_state(state));
    match state {
        ServerState::Connected(env) => {
            let server_id = env.c_ienv.i_server_id;
            for wrapped in env.c_ienv.i_outstanding_requests_from_server.keys() {
                if server_id == wrapped.server_id {
                    let id = encode_wrapped(wrapped);
                    let notification =
                        flow_server_env::lsp_mapper::LspNotification::CancelRequestNotification(
                            lsp_types::CancelParams { id },
                        );
                    let json = lsp_fmt_print_lsp_notification(&notification);
                    to_stdout(&json);
                }
            }
            for id in &env.c_outstanding_requests_to_server.clone() {
                decline_request_to_server(id);
            }
            env.c_outstanding_requests_to_server.clear();
            update_errors(state, |errors| {
                crate::lsp_errors::clear_all_errors_and_send(&mut |v| to_stdout(&v), errors)
            });
        }
        ServerState::Disconnected(_) => {}
    }
}

fn do_live_diagnostics(
    state: &mut ServerState,
    trigger: Option<InteractionTrigger>,
    metadata: &Metadata,
    uri: &DocumentUri,
) -> Result<(), FlowLspError> {
    let file_path = lsp_helpers_lsp_uri_to_path(uri)?;
    let (is_ignored, _) =
        flow_common::files::is_ignored(&get_ienv(state).i_file_options, &file_path);
    if is_ignored {
        return Ok(());
    }
    let trigger = trigger.unwrap_or(InteractionTrigger::UnknownTrigger);
    if let ServerState::Connected(cenv) = &*state {
        let mut metadata = metadata.clone();
        metadata.interaction_tracking_id = Some(start_interaction(trigger.clone(), state));
        if let Err(err) = send_to_server(
            cenv,
            lsp_prot::Request::LiveErrorsRequest(uri.clone()),
            &metadata,
        ) {
            eprintln!("{}", err);
        }
    }

    let interaction_id = start_interaction(trigger, state);
    let (_, live_parse_errors) = parse_and_cache(state, uri)?;
    let ux = match live_parse_errors {
        None => InteractionUx::ErroredPushingLiveParseErrors,
        Some(live_parse_errors) => {
            update_errors(state, |errors| {
                crate::lsp_errors::set_live_parse_errors_and_send(
                    &mut |v| to_stdout(&v),
                    uri,
                    live_parse_errors,
                    errors,
                )
            });
            InteractionUx::PushedLiveParseErrors(uri.clone())
        }
    };
    log_interaction(ux, state, interaction_id);
    Ok(())
}

fn get_local_request_handler(
    ienv: &mut InitializedEnv,
    id: &LspId,
    result: lsp_mapper::LspResult,
) -> Option<Box<dyn FnOnce(&mut ServerState) -> Result<(), FlowLspError>>> {
    if ienv.i_outstanding_local_handlers.contains_key(id) {
        let handler = ienv.i_outstanding_local_handlers.remove(id).unwrap();
        ienv.i_outstanding_local_requests.remove(id);
        match result {
            lsp_mapper::LspResult::ErrorResult(e, msg) => {
                Some(Box::new(move |state: &mut ServerState| {
                    (handler.on_error)(e, msg, state)
                }))
            }
            lsp_mapper::LspResult::RegisterCapabilityResult => {
                Some(Box::new(|_state: &mut ServerState| Ok(())))
            }
            result => Some(Box::new(move |state: &mut ServerState| {
                (handler.on_response)(result, state)
            })),
        }
    } else {
        None
    }
}

fn try_connect(
    version_mismatch_strategy: &socket_handshake::VersionMismatchStrategy,
    flowconfig_name: &str,
    env: &mut DisconnectedEnv,
) -> ServerState {
    let flowconfig = read_flowconfig_from_disk(flowconfig_name, &env.d_ienv.i_root);
    let current_version = flowconfig.version.clone();
    if env.d_ienv.i_version != current_version {
        let prev_version_str = env.d_ienv.i_version.as_deref().unwrap_or("[None]");
        let current_version_str = current_version.as_deref().unwrap_or("[None]");
        let message = format!(
            "\nVersion in flowconfig that spawned the existing flow server: {}\nVersion in flowconfig currently: {}\n",
            prev_version_str, current_version_str
        );
        lsp_writers_telemetry_log(to_stdout, &message);
        lsp_exit_bad();
    }
    let _start_env_flowconfig = &flowconfig;
    let _start_env_flowconfig_name = flowconfig_name;
    let _start_env_connect_params = &env.d_ienv.i_connect_params;
    let _start_env_root = &env.d_ienv.i_root;

    let _client_handshake = lsp_connect_params::persistent_client_handshake(
        version_mismatch_strategy,
        env.d_ienv.i_initialize_params.clone(),
    );

    let conn = classify_persistent_connect(flowconfig_name, env);

    #[allow(unreachable_code)]
    match conn {
        Ok(conn) => {
            let i_server_id = env.d_ienv.i_server_id + 1;
            let i_can_autostart_after_version_mismatch = true;
            let mut new_env = ConnectedEnv {
                c_ienv: InitializedEnv {
                    i_server_id,
                    i_can_autostart_after_version_mismatch,
                    ..env.d_ienv.clone()
                },
                c_conn: conn,
                c_server_status: (server_status::INITIAL_STATUS, None),
                c_about_to_exit_code: None,
                c_is_rechecking: false,
                c_lazy_stats: None,
                c_outstanding_requests_to_server: HashSet::new(),
                c_recent_summaries: vec![],
            };
            let server_status_for_metadata = new_env.c_server_status.0.clone();
            let watcher_status_for_metadata = new_env.c_server_status.1.clone();
            let make_metadata = |method_name: &str| -> Metadata {
                Metadata {
                    start_wall_time: std::time::SystemTime::now()
                        .duration_since(std::time::UNIX_EPOCH)
                        .unwrap_or_default()
                        .as_secs_f64(),
                    start_server_status: Some(server_status_for_metadata.clone()),
                    start_watcher_status: watcher_status_for_metadata.clone(),
                    start_json_truncated: serde_json::json!({"method": method_name}),
                    lsp_method_name: method_name.to_string(),
                    ..lsp_prot::empty_metadata()
                }
            };
            // send the initial messages to the server
            let metadata = make_metadata("synthetic/subscribe");
            if let Err(err) = send_to_server(&new_env, lsp_prot::Request::Subscribe, &metadata) {
                eprintln!("{}", err);
                close_conn(&new_env);
                let mut disconnected = DisconnectedEnv {
                    d_ienv: new_env.c_ienv.clone(),
                    d_autostart: false,
                    d_server_status: None,
                };
                show_disconnected(None, Some(&err.to_string()), &mut disconnected);
                return ServerState::Disconnected(disconnected);
            }
            let settings = new_env.c_ienv.i_config.clone();
            if let Err(err) =
                send_configuration_to_server("synthetic/configuration", settings, &new_env)
            {
                eprintln!("{}", err);
                close_conn(&new_env);
                let mut disconnected = DisconnectedEnv {
                    d_ienv: new_env.c_ienv.clone(),
                    d_autostart: false,
                    d_server_status: None,
                };
                show_disconnected(None, Some(&err.to_string()), &mut disconnected);
                return ServerState::Disconnected(disconnected);
            }
            let metadata = make_metadata("synthetic/open");
            for open_file_info in env.d_ienv.i_open_files.values() {
                let msg = lsp_mapper::LspMessage::NotificationMessage(
                    lsp_mapper::LspNotification::DidOpenNotification(
                        lsp_types::DidOpenTextDocumentParams {
                            text_document: open_file_info.o_open_doc.clone(),
                        },
                    ),
                );
                if let Err(err) = send_lsp_to_server(&new_env, &metadata, msg) {
                    eprintln!("{}", err);
                    close_conn(&new_env);
                    let mut disconnected = DisconnectedEnv {
                        d_ienv: new_env.c_ienv.clone(),
                        d_autostart: false,
                        d_server_status: None,
                    };
                    show_disconnected(None, Some(&err.to_string()), &mut disconnected);
                    return ServerState::Disconnected(disconnected);
                }
            }
            // close the old UI and bring up the new
            show_connected(&mut new_env);
            // Generate live errors for the newly opened files
            let mut state = ServerState::Connected(new_env);
            let open_uris: Vec<_> = env.d_ienv.i_open_files.keys().cloned().collect();
            for uri in open_uris {
                if let Err(err) = do_live_diagnostics(
                    &mut state,
                    Some(InteractionTrigger::ServerConnected),
                    &metadata,
                    &uri,
                ) {
                    eprintln!("{}", err);
                }
            }
            state
        }
        Err(ref reason @ ConnectError::ServerMissing) => {
            let d_autostart = env.d_autostart;
            env.d_autostart = false;
            env.d_server_status = None;
            if d_autostart {
                match start_flow_server(
                    flowconfig_name,
                    &env.d_ienv.i_connect_params,
                    &env.d_ienv.i_root,
                ) {
                    Ok(()) => show_connecting(reason, env),
                    Err(msg) => show_disconnected(None, Some(&msg), env),
                }
            } else {
                show_disconnected(None, None, env);
            }
            ServerState::Disconnected(env.clone())
        }
        Err(ref reason @ ConnectError::ServerSocketMissing) => {
            env.d_server_status = None;
            match kill_stale_server(
                flowconfig_name,
                &env.d_ienv.i_connect_params,
                &env.d_ienv.i_root,
            ) {
                Ok(()) => show_connecting(reason, env),
                Err(_) => {
                    let msg = "An old version of the Flow server is running. Please stop it.";
                    show_disconnected(None, Some(msg), env);
                }
            }
            ServerState::Disconnected(env.clone())
        }
        Err(ref reason @ ConnectError::BuildIdMismatch(BuildIdMismatchKind::ServerExited)) => {
            if env.d_autostart {
                env.d_server_status = None;
                show_connecting(reason, env);
            } else {
                env.d_server_status = None;
                let msg = message_with_flow_and_root_name_prefix(
                    &env.d_ienv.i_flowconfig,
                    "the server was the wrong version",
                );
                show_disconnected(None, Some(&msg), env);
            }
            ServerState::Disconnected(env.clone())
        }
        Err(ConnectError::BuildIdMismatch(BuildIdMismatchKind::ClientShouldError {
            ref server_version,
            ..
        })) => {
            let cmp = flow_common_semver::semver::compare(
                server_version,
                flow_common::flow_version::VERSION,
            );
            match cmp {
                Ok(n) if n < 0 => {
                    eprintln!(
                        "Flow: the running server is an older version of Flow ({}) than the LSP ({}), but we're not allowed to stop it",
                        server_version,
                        flow_common::flow_version::VERSION
                    );
                }
                Ok(0) => {
                    eprintln!(
                        "Flow: the running server is a different binary with the same version ({})",
                        flow_common::flow_version::VERSION
                    );
                }
                Ok(_) => {
                    eprintln!(
                        "Flow: the running server is a newer version of Flow ({}) than the LSP ({})",
                        server_version,
                        flow_common::flow_version::VERSION
                    );
                }
                Err(_) => {
                    eprintln!(
                        "Flow: unable to compare server version ({}) with LSP version ({})",
                        server_version,
                        flow_common::flow_version::VERSION
                    );
                }
            }
            eprintln!(
                "LSP is exiting. Hopefully the IDE will start an LSP with the same binary as the server"
            );
            lsp_exit_bad();
        }
        Err(ref reason @ ConnectError::ServerBusy(ServerBusyKind::FailOnInit(ref st))) => {
            env.d_server_status = Some(st.clone());
            show_connecting(reason, env);
            ServerState::Disconnected(env.clone())
        }
        Err(ref reason @ ConnectError::ServerBusy(ServerBusyKind::NotResponding))
        | Err(ref reason @ ConnectError::ServerBusy(ServerBusyKind::TooManyClients)) => {
            show_connecting(reason, env);
            ServerState::Disconnected(env.clone())
        }
    }
}

pub fn run(flowconfig_name: &str, connect_params: ConnectParams) {
    let client = JsonrpcQueue::new();
    let state = State::PreInit(connect_params);
    initial_lwt_thread_sync(flowconfig_name, &client, state);
}

#[allow(dead_code)]
async fn initial_lwt_thread(flowconfig_name: &str, client: JsonrpcQueue, state: State) {
    initial_lwt_thread_sync(flowconfig_name, &client, state);
}

fn initial_lwt_thread_sync(flowconfig_name: &str, client: &JsonrpcQueue, state: State) {
    std::panic::set_hook(Box::new(|info| {
        let msg = format!("Uncaught async exception: {}", info);
        eprintln!("{}", msg);
        std::process::exit(FlowExitStatus::UnknownError as i32);
    }));

    lsp_interaction_init();

    log_flusher_run();

    main_loop_sync(flowconfig_name, client, state);
}

#[allow(dead_code)]
async fn main_loop(flowconfig_name: &str, client: &JsonrpcQueue, state: State) {
    main_loop_sync(flowconfig_name, client, state);
}

fn main_loop_sync(flowconfig_name: &str, client: &JsonrpcQueue, mut state: State) {
    loop {
        gc_pending_interactions(&state);

        state = match get_next_event_sync(flowconfig_name, &state, client, &|message| {
            parse_json(&state, message)
        }) {
            Ok(event) => main_handle(flowconfig_name, state, event),
            Err(e) => {
                let err: Box<dyn std::error::Error> = Box::new(e);
                main_handle_error(err, state, None)
            }
        };
    }
}

#[allow(clippy::result_large_err)]
fn main_handle(flowconfig_name: &str, state: State, event: Event) -> State {
    let event_for_error = event.clone();
    let (client_duration, result) =
        with_timer(|| main_handle_unsafe(flowconfig_name, state, event));
    match result {
        Ok((state, LogNeeded::LogNeeded(mut metadata))) => {
            let client_duration_val = if metadata.client_duration.is_none() {
                Some(client_duration)
            } else {
                metadata.client_duration
            };
            metadata.client_duration = client_duration_val;
            main_log_command(&state, &metadata);
            state
        }
        Ok((state, _)) => state,
        Err((state, exn)) => main_handle_error(exn, state, Some(event_for_error)),
    }
}

fn main_handle_initialized_unsafe(
    flowconfig_name: &str,
    state: &mut ServerState,
    event: Event,
) -> Result<LogNeeded, Box<dyn std::error::Error>> {
    use flow_server_env::lsp_mapper::LspMessage;
    use flow_server_env::lsp_mapper::LspNotification;
    use flow_server_env::lsp_mapper::LspRequest;
    use flow_server_env::lsp_mapper::LspResult;
    use lsp_prot::MessageFromServer;
    use lsp_prot::NotificationFromServer;
    use lsp_prot::Response;
    match event {
        Event::ClientMessage(
            ref msg @ LspMessage::NotificationMessage(
                LspNotification::DidChangeConfigurationNotification(ref params),
            ),
            metadata,
        ) => {
            let settings = &params.settings;
            match settings {
                serde_json::Value::Null => {
                    update_ienv(state, |ienv| {
                        request_configuration(ienv);
                    });
                }
                i_config => {
                    let i_config = i_config.clone();
                    update_ienv(state, |ienv| {
                        ienv.i_config = i_config;
                    });
                    match state {
                        ServerState::Connected(cenv) => {
                            send_lsp_to_server(cenv, &metadata, msg.clone())?;
                        }
                        ServerState::Disconnected(_) => {}
                    }
                }
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ClientMessage(LspMessage::ResponseMessage(id, result), metadata) => {
            let ienv = get_ienv_mut(state);
            match get_local_request_handler(ienv, &id, result.clone()) {
                Some(handler) => {
                    handler(state)?;
                    Ok(LogNeeded::LogNotNeeded)
                }
                None => match state {
                    ServerState::Connected(_cenv) => {
                        let c = LspMessage::ResponseMessage(id.clone(), result.clone());
                        track_to_server(state, &c)?;
                        let wrapped = decode_wrapped(&id);
                        if let ServerState::Connected(cenv) = state {
                            if wrapped.server_id == cenv.c_ienv.i_server_id {
                                let c =
                                    LspMessage::ResponseMessage(wrapped.message_id.clone(), result);
                                send_lsp_to_server(cenv, &metadata, c)?;
                            }
                        }
                        Ok(LogNeeded::LogNotNeeded)
                    }
                    ServerState::Disconnected(_) => {
                        let msg = format!(
                            "Response {:?} has missing handler",
                            lsp_fmt_denorm_message_to_string(&LspMessage::ResponseMessage(
                                id, result
                            ))
                        );
                        Err(internal_error_exception(msg).into())
                    }
                },
            }
        }
        Event::ClientMessage(
            LspMessage::RequestMessage(id, LspRequest::DocumentSymbolRequest(params)),
            metadata,
        ) => {
            let interaction_id =
                start_interaction(InteractionTrigger::DocumentSymbol(id.clone()), state);
            do_document_symbol(state, &id, &params)?;
            log_interaction(InteractionUx::Responded, state, interaction_id);
            Ok(LogNeeded::LogNeeded(metadata))
        }
        Event::ClientMessage(
            LspMessage::RequestMessage(id, LspRequest::SelectionRangeRequest(params)),
            metadata,
        ) => {
            let interaction_id =
                start_interaction(InteractionTrigger::SelectionRange(id.clone()), state);
            do_selection_range(state, &id, &params)?;
            log_interaction(InteractionUx::Responded, state, interaction_id);
            Ok(LogNeeded::LogNeeded(metadata))
        }
        Event::ClientMessage(c, metadata) if matches!(state, ServerState::Connected(_)) => {
            let TrackEffect { changed_live_uri } = track_to_server(state, &c)?;
            let trigger = lsp_interaction::trigger_of_lsp_msg(&c);
            let interaction_tracking_id = trigger
                .as_ref()
                .map(|trigger| start_interaction(trigger.clone(), state));
            if let ServerState::Connected(cenv) = state {
                let mut fwd_metadata = metadata.clone();
                fwd_metadata.interaction_tracking_id = interaction_tracking_id;
                send_lsp_to_server(cenv, &fwd_metadata, c.clone())?;
            }
            if let Some(ref uri) = changed_live_uri {
                do_live_diagnostics(state, trigger, &metadata, uri)?;
            }
            Ok(LogNeeded::LogDeferred)
        }
        Event::ClientMessage(LspMessage::RequestMessage(id, LspRequest::RageRequest), metadata) => {
            let result = do_rage(flowconfig_name, state);
            let response = LspMessage::ResponseMessage(id, LspResult::RageResult(result));
            let key = command_key_of_server_state(state);
            let json = lsp_fmt_print_lsp(&key, &response);
            to_stdout(&json);
            Ok(LogNeeded::LogNeeded(metadata))
        }
        Event::ClientMessage(
            c @ LspMessage::NotificationMessage(
                LspNotification::DidOpenNotification(_)
                | LspNotification::DidChangeNotification(_)
                | LspNotification::DidSaveNotification(_)
                | LspNotification::DidCloseNotification(_),
            ),
            mut metadata,
        ) => {
            let trigger = lsp_interaction::trigger_of_lsp_msg(&c);
            let interaction_id = trigger
                .as_ref()
                .map(|trigger| start_interaction(trigger.clone(), state));
            let (client_duration, result) = with_timer(|| -> Result<(), FlowLspError> {
                let TrackEffect { changed_live_uri } = track_to_server(state, &c)?;
                if let Some(ref uri) = changed_live_uri {
                    do_live_diagnostics(state, trigger.clone(), &metadata, uri)?;
                }
                Ok(())
            });
            result?;
            if let Some(id) = interaction_id {
                log_interaction(InteractionUx::Responded, state, id);
            }
            metadata.client_duration = Some(client_duration);
            Ok(LogNeeded::LogNeeded(metadata))
        }
        Event::ClientMessage(
            LspMessage::NotificationMessage(LspNotification::CancelRequestNotification(_)),
            _metadata,
        ) if matches!(state, ServerState::Disconnected(_)) => Ok(LogNeeded::LogNotNeeded),
        Event::ClientMessage(c, mut metadata) if matches!(state, ServerState::Disconnected(_)) => {
            let interaction_id = lsp_interaction::trigger_of_lsp_msg(&c)
                .map(|trigger| start_interaction(trigger, state));
            track_to_server(state, &c)?;
            let method_ = lsp_fmt_denorm_message_to_string(&c);
            let err_msg = format!("Server not connected; can't handle {}", method_);
            if let Some(id) = interaction_id {
                log_interaction(InteractionUx::Errored, state, id);
            }
            match c {
                LspMessage::RequestMessage(id, _request) => {
                    let e = lsp_mapper::lsp_error::T {
                        code: lsp_mapper::lsp_error::Code::RequestCancelled,
                        message: err_msg.clone(),
                        data: None,
                    };
                    let outgoing =
                        LspMessage::ResponseMessage(id, LspResult::ErrorResult(e, String::new()));
                    let key = command_key_of_server_state(state);
                    to_stdout(&lsp_fmt_print_lsp(&key, &outgoing));
                    metadata.error_info =
                        Some((lsp_prot::ErrorKind::ExpectedError, err_msg, String::new()));
                    Ok(LogNeeded::LogNeeded(metadata))
                }
                _ => Err(err_msg.into()),
            }
        }
        Event::ServerMessage(MessageFromServer::NotificationFromServer(
            NotificationFromServer::ServerExit(exit_code),
        )) => {
            if let ServerState::Connected(cenv) = state {
                cenv.c_about_to_exit_code = Some(exit_code);
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ServerMessage(MessageFromServer::RequestResponse((
            Response::LspFromServer(msg),
            mut metadata,
        ))) => {
            let ux = match msg {
                None => InteractionUx::Responded,
                Some(outgoing) => {
                    track_from_server(state, &outgoing);
                    let (outgoing, ux) = match &outgoing {
                        LspMessage::RequestMessage(id, _request) => {
                            let server_id = if let ServerState::Connected(cenv) = &state {
                                cenv.c_ienv.i_server_id
                            } else {
                                0
                            };
                            let wrapped = WrappedId {
                                server_id,
                                message_id: id.clone(),
                            };
                            let encoded_outgoing = LspMessage::RequestMessage(
                                encode_wrapped(&wrapped),
                                _request.clone(),
                            );
                            (encoded_outgoing, InteractionUx::Responded)
                        }
                        LspMessage::ResponseMessage(id, LspResult::RageResult(items)) => {
                            metadata.client_duration = None;
                            let mut combined_items = items.clone();
                            combined_items.extend(do_rage(flowconfig_name, state));
                            let rage_outgoing = LspMessage::ResponseMessage(
                                id.clone(),
                                LspResult::RageResult(combined_items),
                            );
                            (rage_outgoing, InteractionUx::Responded)
                        }
                        LspMessage::ResponseMessage(_, LspResult::ErrorResult(e, _)) => {
                            let ux = if e.code == lsp_mapper::lsp_error::Code::RequestCancelled {
                                InteractionUx::Canceled
                            } else {
                                InteractionUx::Errored
                            };
                            (outgoing.clone(), ux)
                        }
                        _ => (outgoing.clone(), InteractionUx::Responded),
                    };
                    let outgoing = selectively_omit_errors(&metadata.lsp_method_name, outgoing);
                    let key = command_key_of_server_state(state);
                    to_stdout(&lsp_fmt_print_lsp(&key, &outgoing));
                    ux
                }
            };
            if let Some(id) = metadata.interaction_tracking_id {
                log_interaction(ux, state, id);
            }
            Ok(LogNeeded::LogNeeded(metadata))
        }
        Event::ServerMessage(MessageFromServer::RequestResponse((
            Response::UncaughtException {
                request,
                exception_constructor,
                stack,
            },
            mut metadata,
        ))) => {
            metadata.error_info = Some((
                lsp_prot::ErrorKind::UnexpectedError,
                exception_constructor.clone(),
                stack.clone(),
            ));
            let outgoing: Option<LspMessage> = match request {
                lsp_prot::Request::LspToServer(LspMessage::RequestMessage(id, _)) => {
                    let e = lsp_mapper::lsp_error::T {
                        code: lsp_mapper::lsp_error::Code::UnknownErrorCode,
                        message: "Flow encountered an unexpected error while handling this request. See the Flow logs for more details.".to_string(),
                        data: None,
                    };
                    Some(LspMessage::ResponseMessage(
                        id,
                        LspResult::ErrorResult(e, stack.clone()),
                    ))
                }
                lsp_prot::Request::Subscribe | lsp_prot::Request::LspToServer(_) => {
                    let code = lsp_mapper::lsp_error::Code::UnknownErrorCode;
                    let text = format!("{} [{:?}]\n{}", exception_constructor, code, stack);
                    Some(LspMessage::NotificationMessage(
                        LspNotification::TelemetryNotification(lsp_types::LogMessageParams {
                            typ: lsp_types::MessageType::ERROR,
                            message: text,
                        }),
                    ))
                }
                lsp_prot::Request::LiveErrorsRequest(_) => None,
            };
            let key = command_key_of_server_state(state);
            if let Some(outgoing) = outgoing {
                let outgoing = selectively_omit_errors(&metadata.lsp_method_name, outgoing);
                to_stdout(&lsp_fmt_print_lsp(&key, &outgoing));
            }
            if let Some(id) = metadata.interaction_tracking_id {
                log_interaction(InteractionUx::Errored, state, id);
            }
            Ok(LogNeeded::LogNeeded(metadata))
        }
        Event::ServerMessage(MessageFromServer::NotificationFromServer(
            NotificationFromServer::Errors {
                diagnostics,
                errors_reason,
            },
        )) => {
            let end_state = collect_interaction_state(state);
            lsp_interaction::log_pushed_errors(end_state, &errors_reason);
            let is_rechecking = if let ServerState::Connected(cenv) = &state {
                cenv.c_is_rechecking
            } else {
                false
            };
            if is_rechecking {
                update_errors(state, |errors| {
                    crate::lsp_errors::add_streamed_server_errors_and_send(
                        &mut to_stdout_fn(),
                        diagnostics,
                        errors,
                    )
                });
            } else {
                update_errors(state, |errors| {
                    crate::lsp_errors::set_finalized_server_errors_and_send(
                        &mut to_stdout_fn(),
                        diagnostics,
                        errors,
                    )
                });
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ServerMessage(MessageFromServer::RequestResponse((
            Response::LiveErrorsResponse(Ok(live_resp)),
            metadata,
        ))) => {
            let lsp_prot::LiveErrorsResponse {
                live_diagnostics,
                live_errors_uri: ref uri,
            } = live_resp;
            let file_is_still_open = get_open_files(state).contains_key(uri);
            if file_is_still_open {
                if let Some(id) = metadata.interaction_tracking_id {
                    log_interaction(
                        InteractionUx::PushedLiveNonParseErrors(uri.clone()),
                        state,
                        id,
                    );
                }
                let request =
                    serde_json::to_string(&metadata.start_json_truncated).unwrap_or_default();
                let data = serde_json::json!({
                    "uri": uri.to_string(),
                    "error_count": live_diagnostics.len(),
                })
                .to_string();
                flow_event_logger::live_non_parse_errors(&request, &data, metadata.start_wall_time);
                let uri_clone = uri.clone();
                update_errors(state, |errors| {
                    crate::lsp_errors::set_live_non_parse_errors_and_send(
                        &mut to_stdout_fn(),
                        &uri_clone,
                        live_diagnostics,
                        errors,
                    )
                });
            } else {
                if let Some(id) = metadata.interaction_tracking_id {
                    log_interaction(InteractionUx::ErroredPushingLiveNonParseErrors, state, id);
                }
                let request =
                    serde_json::to_string(&metadata.start_json_truncated).unwrap_or_default();
                let data = serde_json::json!({
                    "uri": uri.to_string(),
                    "reason": "File no longer open",
                })
                .to_string();
                flow_event_logger::live_non_parse_errors_failed(
                    &request,
                    &data,
                    metadata.start_wall_time,
                );
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ServerMessage(MessageFromServer::RequestResponse((
            Response::LiveErrorsResponse(Err(live_err)),
            metadata,
        ))) => {
            let ux = match live_err.live_errors_failure_kind {
                lsp_prot::ErrorResponseKind::CanceledErrorResponse => {
                    InteractionUx::CanceledPushingLiveNonParseErrors
                }
                lsp_prot::ErrorResponseKind::ErroredErrorResponse => {
                    InteractionUx::ErroredPushingLiveNonParseErrors
                }
            };
            if let Some(id) = metadata.interaction_tracking_id {
                log_interaction(ux, state, id);
            }
            let request = serde_json::to_string(&metadata.start_json_truncated).unwrap_or_default();
            let data = serde_json::json!({
                "uri": live_err.live_errors_failure_uri.to_string(),
                "reason": live_err.live_errors_failure_reason,
            })
            .to_string();
            flow_event_logger::live_non_parse_errors_failed(
                &request,
                &data,
                metadata.start_wall_time,
            );
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ServerMessage(MessageFromServer::NotificationFromServer(
            NotificationFromServer::StartRecheck,
        )) => {
            let start_state = collect_interaction_state(state);
            lsp_interaction::recheck_start(start_state);
            if let ServerState::Connected(cenv) = state {
                cenv.c_is_rechecking = true;
                cenv.c_lazy_stats = None;
                show_connected_status(cenv);
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ServerMessage(MessageFromServer::NotificationFromServer(
            NotificationFromServer::EndRecheck(lazy_stats),
        )) => {
            if let ServerState::Connected(cenv) = &mut *state {
                cenv.c_is_rechecking = false;
                cenv.c_lazy_stats = Some(lazy_stats);
                show_connected_status(cenv);
            }
            let open_file_uris: Vec<DocumentUri> = get_open_files(state).keys().cloned().collect();
            let method_name = "synthetic/endRecheck";
            if let ServerState::Connected(cenv) = &*state {
                let mut metadata = lsp_prot::empty_metadata();
                metadata.start_wall_time = now();
                metadata.start_server_status = Some(cenv.c_server_status.0.clone());
                metadata.start_watcher_status = cenv.c_server_status.1.clone();
                metadata.start_json_truncated = serde_json::json!({"method": method_name});
                metadata.lsp_method_name = method_name.to_string();
                for uri in open_file_uris {
                    if let Err(err) =
                        send_to_server(cenv, lsp_prot::Request::LiveErrorsRequest(uri), &metadata)
                    {
                        eprintln!("{}", err);
                    }
                }
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ServerMessage(MessageFromServer::NotificationFromServer(
            NotificationFromServer::Telemetry(telemetry_event),
        )) => {
            if let ServerState::Connected(cenv) = state {
                update_recent_summaries(cenv, telemetry_event);
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::ServerMessage(MessageFromServer::NotificationFromServer(
            NotificationFromServer::PleaseHold(server_status, watcher_status),
        )) => {
            if let ServerState::Connected(cenv) = state {
                cenv.c_server_status = (server_status, Some(watcher_status));
                show_connected_status(cenv);
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::Tick if matches!(state, ServerState::Disconnected(_)) => {
            let version_mismatch_strategy = socket_handshake::VersionMismatchStrategy::ErrorClient;
            if let ServerState::Disconnected(env) = &mut *state {
                let new_state = try_connect(&version_mismatch_strategy, flowconfig_name, env);
                *state = new_state;
            }
            Ok(LogNeeded::LogNotNeeded)
        }
        Event::Tick => Ok(LogNeeded::LogNotNeeded),
        _ => unreachable!(),
    }
}

#[allow(clippy::result_large_err)]
fn main_handle_unsafe(
    flowconfig_name: &str,
    state: State,
    event: Event,
) -> Result<(State, LogNeeded), (State, Box<dyn std::error::Error>)> {
    use flow_server_env::lsp_mapper::LspMessage;
    use flow_server_env::lsp_mapper::LspNotification;
    use flow_server_env::lsp_mapper::LspRequest;
    let event = convert_to_client_uris(&state, event);
    match (state, event) {
        (
            State::PreInit(i_connect_params),
            Event::ClientMessage(
                LspMessage::RequestMessage(id, LspRequest::InitializeRequest(i_initialize_params)),
                metadata,
            ),
        ) => {
            let error_state = State::PreInit(i_connect_params.clone());
            let i_root = match lsp_helpers_get_root(&i_initialize_params) {
                Ok(i_root) => i_root,
                Err(err) => return Err((error_state, Box::new(err))),
            };
            flow_parser::file_key::set_project_root(&i_root.to_string_lossy());

            let lsp_temp_dir: FilePath = i_connect_params
                .temp_dir
                .as_ref()
                .map(|s| FilePath::from(s.as_str()))
                .unwrap_or_else(server_files_js_default_temp_dir);

            match flow_flowlib::libdir(false, &lsp_temp_dir) {
                flow_flowlib::LibDir::Prelude(path) => {
                    flow_parser::file_key::set_flowlib_root(&path.to_string_lossy());
                }
                flow_flowlib::LibDir::Flowlib(path) => {
                    flow_parser::file_key::set_flowlib_root(&path.to_string_lossy());
                }
            }

            let flowconfig = read_flowconfig_from_disk(flowconfig_name, &i_root);

            let mut d_ienv = InitializedEnv {
                i_initialize_params: i_initialize_params.clone(),
                i_connect_params,
                i_root: i_root.clone(),
                i_version: flowconfig.version.clone(),
                i_can_autostart_after_version_mismatch: true,
                i_server_id: 0,
                i_outstanding_local_requests: HashMap::new(),
                i_outstanding_local_handlers: HashMap::new(),
                i_outstanding_requests_from_server: WrappedMap::new(),
                i_is_connected: false,
                i_status: ShowStatusT::NeverShown,
                i_open_files: UriMap::new(),
                i_errors: crate::lsp_errors::empty(),
                i_config: serde_json::Value::Null,
                i_flowconfig: flowconfig.clone(),
                i_file_options: file_options_of_flowconfig(&i_root, &flowconfig),
            };

            flow_interaction_logger::set_server_config(
                flowconfig.options.log_saving.get("timeout").copied(),
                flowconfig_name,
                i_root.clone(),
                flowconfig.options.root_name.as_deref(),
            );

            let required_version = flowconfig.version.clone();
            if let Err(msg) = command_utils_check_version(&required_version) {
                return Err((
                    error_state,
                    Box::new(FlowLspError::LspException(
                        flow_server_env::lsp_mapper::lsp_error::T {
                            code: flow_server_env::lsp_mapper::lsp_error::Code::ServerErrorStart,
                            message: msg,
                            data: Some(serde_json::json!({ "retry": false })),
                        },
                    )),
                ));
            }

            let result = do_initialize(&i_initialize_params);
            let response = lsp_mapper::LspResult::InitializeResult(result);
            let key = command_key_of_ienv(&d_ienv);
            let json = lsp_fmt_print_lsp_response(&key, &id, &response);
            to_stdout(&json);

            if lsp_helpers_supports_configuration(&i_initialize_params) {
                request_configuration(&mut d_ienv);
                subscribe_to_config_changes(&mut d_ienv);
            }

            let version_mismatch_strategy =
                socket_handshake::VersionMismatchStrategy::AlwaysStopServer;

            let mut env = DisconnectedEnv {
                d_ienv,
                d_autostart: true,
                d_server_status: None,
            };

            let server_state = try_connect(&version_mismatch_strategy, flowconfig_name, &mut env);
            Ok((
                State::Initialized(server_state),
                LogNeeded::LogNeeded(metadata),
            ))
        }
        (
            state,
            Event::ClientMessage(
                LspMessage::NotificationMessage(LspNotification::InitializedNotification),
                _metadata,
            ),
        ) => Ok((state, LogNeeded::LogNotNeeded)),
        (
            state,
            Event::ClientMessage(
                LspMessage::NotificationMessage(LspNotification::SetTraceNotification),
                _metadata,
            ),
        )
        | (
            state,
            Event::ClientMessage(
                LspMessage::NotificationMessage(LspNotification::LogTraceNotification),
                _metadata,
            ),
        ) => Ok((state, LogNeeded::LogNotNeeded)),
        (
            state,
            Event::ClientMessage(
                LspMessage::RequestMessage(id, LspRequest::ShutdownRequest),
                _metadata,
            ),
        ) => {
            if let State::Initialized(ServerState::Connected(env)) = &state {
                close_conn(env);
            }
            let key = command_key_of_state(&state);
            let response =
                lsp_prot::LspMessage::ResponseMessage(id, lsp_mapper::LspResult::ShutdownResult);
            let json = lsp_fmt_print_lsp(&key, &response);
            to_stdout(&json);
            Ok((State::PostShutdown, LogNeeded::LogNotNeeded))
        }
        (
            state,
            Event::ClientMessage(
                LspMessage::NotificationMessage(LspNotification::ExitNotification),
                _metadata,
            ),
        ) => {
            if matches!(state, State::PostShutdown) {
                lsp_exit_ok()
            } else {
                lsp_exit_bad()
            }
        }
        (state @ State::PreInit(_), Event::ClientMessage(_, _)) => {
            Err((state, "Server not initialized".into()))
        }
        (State::Initialized(mut server_state), event) => {
            match main_handle_initialized_unsafe(flowconfig_name, &mut server_state, event) {
                Ok(log_needed) => Ok((State::Initialized(server_state), log_needed)),
                Err(exn) => Err((State::Initialized(server_state), exn)),
            }
        }
        (state @ State::PostShutdown, Event::ClientMessage(_, _metadata)) => {
            Err((state, "Server shutting down".into()))
        }
        (state @ State::PreInit(_), Event::ServerMessage(_))
        | (state @ State::PostShutdown, Event::ServerMessage(_)) => {
            Ok((state, LogNeeded::LogNotNeeded))
        }
        (state, Event::Tick) => Ok((state, LogNeeded::LogNotNeeded)),
    }
}

fn main_log_command(_state: &State, metadata: &Metadata) {
    let request = serde_json::to_string(&metadata.start_json_truncated).unwrap_or_default();
    let request_id = metadata.lsp_id.as_ref().map(lsp_fmt_id_to_string);
    let persistent_delay = match _state {
        State::Initialized(ServerState::Connected(cenv)) => {
            let delays: Vec<_> = cenv
                .c_recent_summaries
                .iter()
                .map(|(_, summary)| summary.clone())
                .collect();
            if delays.is_empty() {
                None
            } else {
                Some(log_of_summaries(&cenv.c_ienv.i_root, &delays))
            }
        }
        State::PreInit(_)
        | State::PostShutdown
        | State::Initialized(ServerState::Disconnected(_)) => None,
    };
    match &metadata.error_info {
        None => {
            flow_event_logger::persistent_command_success(
                metadata,
                &request,
                request_id.as_deref(),
                persistent_delay.as_ref(),
                None,
            );
        }
        Some((lsp_prot::ErrorKind::ExpectedError, msg, stack)) => {
            flow_event_logger::persistent_command_success(
                metadata,
                &request,
                request_id.as_deref(),
                persistent_delay.as_ref(),
                Some((msg.as_str(), stack.as_str())),
            );
        }
        Some((lsp_prot::ErrorKind::UnexpectedError, msg, stack)) => {
            flow_event_logger::persistent_command_failure(
                metadata,
                &request,
                persistent_delay.as_ref(),
                (msg.as_str(), stack.as_str()),
            );
        }
    }
}

fn main_log_error(expected: bool, msg: &str, stack: &str, event: Option<&Event>) {
    let (request, activity_key) = match event {
        Some(Event::ClientMessage(_, metadata)) => (
            Some(serde_json::to_string(&metadata.start_json_truncated).unwrap_or_default()),
            metadata.activity_key.as_ref(),
        ),
        Some(Event::ServerMessage(_)) | Some(Event::Tick) | None => (None, None),
    };
    if expected {
        flow_event_logger::persistent_expected_error(
            request.as_deref(),
            activity_key,
            (msg, stack),
        );
    } else {
        flow_event_logger::persistent_unexpected_error(
            request.as_deref(),
            activity_key,
            (msg, stack),
        );
    }
}

// log the error
// report that we're disconnected to telemetry/connectionStatus
fn main_handle_error(
    exn: Box<dyn std::error::Error>,
    mut state: State,
    event: Option<Event>,
) -> State {
    let stack = format!("{:?}", exn);
    let _msg = exn.to_string();
    if let State::PostShutdown = &state {
        if exn
            .downcast_ref::<FlowLspError>()
            .is_some_and(|e| matches!(e, FlowLspError::ServerFatalConnectionException(_)))
        {
            return state;
        }
    }

    match exn.downcast::<FlowLspError>() {
        Ok(flow_err) => match *flow_err {
            FlowLspError::ServerFatalConnectionException(edata) => {
                let full_stack = format!("{}---\n{}", edata.stack, stack);
                main_log_error(
                    true,
                    &format!("[Server fatal] {}", edata.message),
                    &full_stack,
                    event.as_ref(),
                );

                if let State::Initialized(ServerState::Connected(ref mut env)) = state {
                    close_conn(env);
                    let i_is_connected = lsp_writers_notify_connection_status(
                        &env.c_ienv.i_initialize_params,
                        to_stdout,
                        env.c_ienv.i_is_connected,
                        false,
                    );
                    env.c_ienv.i_is_connected = i_is_connected;
                }

                let code = match &state {
                    State::Initialized(ServerState::Connected(cenv)) => cenv.c_about_to_exit_code,
                    _ => None,
                };
                let code_str = code
                    .map(flow_common_exit_status::to_string)
                    .unwrap_or_default();
                let report = format!(
                    "Server fatal exception: [{}] {}\n{}",
                    code_str, edata.message, full_stack
                );
                lsp_writers_telemetry_error(to_stdout, &report);

                if let State::Initialized(ref mut server_state) = state {
                    dismiss_tracks(server_state);
                }

                let (d_autostart, d_ienv) = match state {
                    State::Initialized(ServerState::Connected(ConnectedEnv {
                        c_ienv,
                        c_about_to_exit_code:
                            Some(
                                FlowExitStatus::FlowconfigChanged
                                | FlowExitStatus::ServerOutOfDate
                                | FlowExitStatus::LockStolen,
                            ),
                        ..
                    })) => {
                        let previous = c_ienv.i_can_autostart_after_version_mismatch;
                        let mut d_ienv = c_ienv;
                        d_ienv.i_can_autostart_after_version_mismatch = false;
                        (previous, d_ienv)
                    }
                    State::Initialized(ServerState::Connected(ConnectedEnv {
                        c_ienv,
                        c_about_to_exit_code: Some(FlowExitStatus::FileWatcherMissedChanges),
                        ..
                    })) => (true, c_ienv),
                    State::Initialized(ServerState::Connected(ConnectedEnv { c_ienv, .. })) => {
                        (false, c_ienv)
                    }
                    State::Initialized(ServerState::Disconnected(DisconnectedEnv {
                        d_ienv,
                        ..
                    })) => (false, d_ienv),
                    State::PreInit(_) | State::PostShutdown => {
                        panic!("Unexpected server error in inapplicable state")
                    }
                };
                let env = DisconnectedEnv {
                    d_ienv,
                    d_autostart,
                    d_server_status: None,
                };
                State::Initialized(ServerState::Disconnected(env))
            }
            FlowLspError::ClientRecoverableConnectionException(edata) => {
                let stack = format!("{}---\n{}", edata.stack, stack);
                main_log_error(
                    true,
                    &format!("[Client recoverable] {}", edata.message),
                    &stack,
                    event.as_ref(),
                );
                let report = format!("Client exception: {}\n{}", edata.message, stack);
                lsp_writers_telemetry_error(to_stdout, &report);
                state
            }
            FlowLspError::ClientFatalConnectionException(edata) => {
                let stack = format!("{}---\n{}", edata.stack, stack);
                main_log_error(
                    true,
                    &format!("[Client fatal] {}", edata.message),
                    &stack,
                    event.as_ref(),
                );
                eprintln!("Client fatal exception: {}\n{}", edata.message, stack);
                lsp_exit_bad()
            }
            FlowLspError::RequestLspException { id, error } => {
                main_log_error(
                    true,
                    &format!("[FlowLSP] {}", error.message),
                    &stack,
                    event.as_ref(),
                );
                let key = command_key_of_state(&state);
                let json = lsp_fmt_print_lsp_response(
                    &key,
                    &id,
                    &lsp_mapper::LspResult::ErrorResult(error, stack),
                );
                to_stdout(&json);
                state
            }
            FlowLspError::ChangedFileNotOpen(uri) => {
                main_log_error(
                    false,
                    &format!("Changed file not open: {}", uri),
                    &stack,
                    event.as_ref(),
                );
                state
            }
            FlowLspError::LspException(e) => {
                main_log_error(
                    true,
                    &format!("[FlowLSP] {}", e.message),
                    &stack,
                    event.as_ref(),
                );
                let text = format!(
                    "FlowLSP exception {} [{}]\n{}",
                    e.message, e.code as i32, stack
                );
                match event.as_ref() {
                    Some(Event::ClientMessage(
                        lsp_prot::LspMessage::RequestMessage(id, _request),
                        _metadata,
                    )) => {
                        let key = command_key_of_state(&state);
                        let json = lsp_fmt_print_lsp_response(
                            &key,
                            id,
                            &lsp_mapper::LspResult::ErrorResult(e, stack),
                        );
                        to_stdout(&json);
                    }
                    _ => {
                        lsp_writers_telemetry_error(to_stdout, &text);
                    }
                }
                state
            }
        },
        Err(other) => {
            let e = lsp_fmt_error_of_exn(&*other);
            main_log_error(
                true,
                &format!("[FlowLSP] {}", e.message),
                &stack,
                event.as_ref(),
            );
            let text = format!(
                "FlowLSP exception {} [{}]\n{}",
                e.message, e.code as i32, stack
            );
            match event.as_ref() {
                Some(Event::ClientMessage(
                    lsp_prot::LspMessage::RequestMessage(id, _request),
                    _metadata,
                )) => {
                    let key = command_key_of_state(&state);
                    let json = lsp_fmt_print_lsp_response(
                        &key,
                        id,
                        &lsp_mapper::LspResult::ErrorResult(e, stack),
                    );
                    to_stdout(&json);
                }
                _ => {
                    lsp_writers_telemetry_error(to_stdout, &text);
                }
            }
            state
        }
    }
}
