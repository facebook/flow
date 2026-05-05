/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;

use flow_server_env::lsp::DocumentUri;
use flow_server_env::lsp::LspId;
use flow_server_env::lsp::LspMessage;
use flow_server_env::lsp::LspNotification;
use flow_server_env::lsp::LspRequest;
use flow_server_env::lsp::LspResult;
use flow_server_env::lsp_prot;

pub type Id = i32;

#[derive(Debug, Clone)]
pub enum Trigger {
    CodeAction(LspId),
    Completion(LspId),
    Definition(LspId),
    DidChange,
    DidClose,
    DidOpen,
    DidSave,
    DocumentHighlight(LspId),
    DocumentSymbol(LspId),
    FindReferences(LspId),
    Hover(LspId),
    LLMContext(LspId),
    PrepareRename(LspId),
    PushedErrorsEndOfRecheck,
    PushedErrorsEnvChange,
    PushedErrorsNewSubscription,
    PushedErrorsRecheckStreaming,
    Rage(LspId),
    Rename(LspId),
    ServerConnected,
    SelectionRange(LspId),
    SignatureHelp(LspId),
    TextDocumentDiagnostics(LspId),
    TypeCoverage(LspId),
    ExecuteCommand(LspId),
    WillRenameFiles(LspId),
    AutoCloseJsx(LspId),
    PrepareDocumentPaste(LspId),
    ProvideDocumentPaste(LspId),
    LinkedEditingRange(LspId),
    RenameFileImports(LspId),
    UnknownTrigger,
}

#[derive(Debug, Clone)]
pub enum Source {
    Server,
    Client,
    UnknownSource,
}

#[derive(Debug, Clone)]
pub enum Ux {
    Canceled,
    CanceledPushingLiveNonParseErrors,
    Dismissed,
    Errored,
    ErroredPushingLiveNonParseErrors,
    ErroredPushingLiveParseErrors,
    PushedErrors,
    PushedLiveNonParseErrors(DocumentUri),
    PushedLiveParseErrors(DocumentUri),
    Responded,
    Timeout,
}

#[derive(Debug, Clone, Copy)]
pub enum ServerStatus {
    Stopped,
    Initializing,
    Rechecking,
    Ready,
}

#[derive(Debug, Clone, Copy)]
pub enum BufferStatus {
    NoOpenBuffers,
    NoUnsavedBuffers,
    UnsavedBuffers,
}

#[derive(Debug, Clone, Copy)]
pub struct State {
    pub time: f64,
    pub server_status: ServerStatus,
    pub buffer_status: BufferStatus,
}

fn string_of_trigger(trigger: &Trigger) -> &'static str {
    match trigger {
        Trigger::CodeAction(_) => "codeAction",
        Trigger::Completion(_) => "completion",
        Trigger::Definition(_) => "definition",
        Trigger::DidChange => "didChange",
        Trigger::DidClose => "didClose",
        Trigger::DidOpen => "didOpen",
        Trigger::DidSave => "didSave",
        Trigger::DocumentHighlight(_) => "documentHighlight",
        Trigger::DocumentSymbol(_) => "documentSymbol",
        Trigger::FindReferences(_) => "findReferences",
        Trigger::Hover(_) => "hover",
        Trigger::LLMContext(_) => "llmContext",
        Trigger::PrepareRename(_) => "PrepareRename",
        Trigger::PushedErrorsEndOfRecheck => "endOfRecheck",
        Trigger::PushedErrorsRecheckStreaming => "recheckStreaming",
        Trigger::PushedErrorsEnvChange => "envChange",
        Trigger::PushedErrorsNewSubscription => "newSubscription",
        Trigger::Rage(_) => "Rage",
        Trigger::Rename(_) => "Rename",
        Trigger::ServerConnected => "ServerConnected",
        Trigger::SelectionRange(_) => "SelectionRange",
        Trigger::SignatureHelp(_) => "SignatureHelp",
        Trigger::TextDocumentDiagnostics(_) => "TextDocumentDiagnostics",
        Trigger::TypeCoverage(_) => "TypeCoverage",
        Trigger::ExecuteCommand(_) => "ExecuteCommand",
        Trigger::WillRenameFiles(_) => "willRenameFiles",
        Trigger::AutoCloseJsx(_) => "AutoCloseJsx",
        Trigger::PrepareDocumentPaste(_) => "PrepareDocumentPaste",
        Trigger::ProvideDocumentPaste(_) => "ProvideDocumentPaste",
        Trigger::LinkedEditingRange(_) => "LinkedEditingRange",
        Trigger::RenameFileImports(_) => "RenameFileImports",
        Trigger::UnknownTrigger => "UnknownTrigger",
    }
}

fn lsp_id_of_trigger(trigger: &Trigger) -> Option<&LspId> {
    match trigger {
        Trigger::CodeAction(lsp_id)
        | Trigger::Completion(lsp_id)
        | Trigger::Definition(lsp_id)
        | Trigger::DocumentHighlight(lsp_id)
        | Trigger::DocumentSymbol(lsp_id)
        | Trigger::FindReferences(lsp_id)
        | Trigger::Hover(lsp_id)
        | Trigger::LLMContext(lsp_id)
        | Trigger::Rage(lsp_id)
        | Trigger::PrepareRename(lsp_id)
        | Trigger::Rename(lsp_id)
        | Trigger::SelectionRange(lsp_id)
        | Trigger::SignatureHelp(lsp_id)
        | Trigger::TextDocumentDiagnostics(lsp_id)
        | Trigger::TypeCoverage(lsp_id)
        | Trigger::ExecuteCommand(lsp_id)
        | Trigger::AutoCloseJsx(lsp_id)
        | Trigger::PrepareDocumentPaste(lsp_id)
        | Trigger::ProvideDocumentPaste(lsp_id)
        | Trigger::WillRenameFiles(lsp_id)
        | Trigger::LinkedEditingRange(lsp_id)
        | Trigger::RenameFileImports(lsp_id) => Some(lsp_id),
        Trigger::DidChange
        | Trigger::DidClose
        | Trigger::DidOpen
        | Trigger::DidSave
        | Trigger::PushedErrorsEndOfRecheck
        | Trigger::PushedErrorsRecheckStreaming
        | Trigger::PushedErrorsEnvChange
        | Trigger::PushedErrorsNewSubscription
        | Trigger::ServerConnected
        | Trigger::UnknownTrigger => None,
    }
}

fn string_of_ux(ux: &Ux) -> &'static str {
    match ux {
        Ux::Canceled => "Canceled",
        Ux::CanceledPushingLiveNonParseErrors => "CanceledPushingLiveNonParseErrors",
        Ux::Dismissed => "Dismissed",
        Ux::Errored => "Errored",
        Ux::ErroredPushingLiveNonParseErrors => "ErroredPushingLiveNonParseErrors",
        Ux::ErroredPushingLiveParseErrors => "ErroredPushingLiveParseErrors",
        Ux::PushedErrors => "PushedErrors",
        Ux::PushedLiveNonParseErrors(_) => "PushedLiveNonParseErrors",
        Ux::PushedLiveParseErrors(_) => "PushedLiveParseErrors",
        Ux::Responded => "Responded",
        Ux::Timeout => "Timeout",
    }
}

fn uri_of_ux(ux: &Ux) -> Option<&DocumentUri> {
    match ux {
        Ux::PushedLiveNonParseErrors(uri) | Ux::PushedLiveParseErrors(uri) => Some(uri),
        Ux::Canceled
        | Ux::CanceledPushingLiveNonParseErrors
        | Ux::Dismissed
        | Ux::Errored
        | Ux::ErroredPushingLiveNonParseErrors
        | Ux::ErroredPushingLiveParseErrors
        | Ux::PushedErrors
        | Ux::Responded
        | Ux::Timeout => None,
    }
}

fn string_of_server_status(status: ServerStatus) -> &'static str {
    match status {
        ServerStatus::Stopped => "Stopped",
        ServerStatus::Initializing => "Initializing",
        ServerStatus::Rechecking => "Rechecking",
        ServerStatus::Ready => "Ready",
    }
}

fn string_of_buffer_status(status: BufferStatus) -> &'static str {
    match status {
        BufferStatus::NoOpenBuffers => "NoOpenBuffers",
        BufferStatus::NoUnsavedBuffers => "NoUnsavedBuffers",
        BufferStatus::UnsavedBuffers => "UnsavedBuffers",
    }
}

fn source_of_trigger(trigger: &Trigger) -> Source {
    match trigger {
        Trigger::CodeAction(_)
        | Trigger::Completion(_)
        | Trigger::Definition(_)
        | Trigger::DidChange
        | Trigger::DidClose
        | Trigger::DidOpen
        | Trigger::DidSave
        | Trigger::DocumentHighlight(_)
        | Trigger::DocumentSymbol(_)
        | Trigger::FindReferences(_)
        | Trigger::Hover(_)
        | Trigger::LLMContext(_)
        | Trigger::Rage(_)
        | Trigger::PrepareRename(_)
        | Trigger::Rename(_)
        | Trigger::SelectionRange(_)
        | Trigger::SignatureHelp(_)
        | Trigger::TextDocumentDiagnostics(_)
        | Trigger::TypeCoverage(_)
        | Trigger::ExecuteCommand(_)
        | Trigger::AutoCloseJsx(_)
        | Trigger::PrepareDocumentPaste(_)
        | Trigger::ProvideDocumentPaste(_)
        | Trigger::WillRenameFiles(_)
        | Trigger::LinkedEditingRange(_)
        | Trigger::RenameFileImports(_) => Source::Client,
        Trigger::PushedErrorsEndOfRecheck
        | Trigger::PushedErrorsEnvChange
        | Trigger::PushedErrorsNewSubscription
        | Trigger::PushedErrorsRecheckStreaming
        | Trigger::ServerConnected => Source::Server,
        Trigger::UnknownTrigger => Source::UnknownSource,
    }
}

fn string_of_source(source: &Source) -> &'static str {
    match source {
        Source::Client => "Client",
        Source::Server => "Server",
        Source::UnknownSource => "UnknownSource",
    }
}

struct PendingInteraction {
    start_state: State,
    trigger: Trigger,
}

struct InternalState {
    next_id: i32,
    pending_interactions: BTreeMap<i32, PendingInteraction>,
    lowest_pending_id: i32,
    last_recheck_start_state: Option<State>,
}

use std::cell::RefCell;

thread_local! {
    static INTERNAL_STATE: RefCell<InternalState> = const { RefCell::new(InternalState {
        next_id: 0,
        pending_interactions: BTreeMap::new(),
        lowest_pending_id: 0,
        last_recheck_start_state: None,
    }) };
}

pub fn start(start_state: State, trigger: Trigger) -> Id {
    INTERNAL_STATE.with(|s| {
        let mut s = s.borrow_mut();
        let id = s.next_id;
        s.next_id += 1;
        let interaction = PendingInteraction {
            start_state,
            trigger,
        };
        s.pending_interactions.insert(id, interaction);
        id
    })
}

pub fn recheck_start(start_state: State) {
    INTERNAL_STATE.with(|s| {
        s.borrow_mut().last_recheck_start_state = Some(start_state);
    })
}

fn log_inner(ux: &Ux, trigger: &Trigger, start_state: &State, end_state: &State) {
    let is_timeout_ux = matches!(ux, Ux::Timeout);
    let lsp_id = lsp_id_of_trigger(trigger).map(flow_lsp::lsp_fmt::id_to_string);
    let uri = uri_of_ux(ux).map(|u| u.as_str().to_string());
    flow_interaction_logger::interaction(
        lsp_id.as_deref(),
        is_timeout_ux,
        string_of_source(&source_of_trigger(trigger)),
        uri.as_deref(),
        string_of_trigger(trigger),
        string_of_ux(ux),
        (start_state.time * 1000.0) as i64,
        (end_state.time * 1000.0) as i64,
        string_of_server_status(start_state.server_status),
        string_of_server_status(end_state.server_status),
        string_of_buffer_status(start_state.buffer_status),
        string_of_buffer_status(end_state.buffer_status),
    );
}

pub fn log_pushed_errors(end_state: State, errors_reason: &lsp_prot::ErrorsReason) {
    let (trigger, start_state) = match errors_reason {
        lsp_prot::ErrorsReason::EndOfRecheck => {
            let start = INTERNAL_STATE
                .with(|s| s.borrow().last_recheck_start_state)
                .unwrap_or(end_state);
            (Trigger::PushedErrorsEndOfRecheck, start)
        }
        lsp_prot::ErrorsReason::RecheckStreaming => {
            let start = INTERNAL_STATE
                .with(|s| s.borrow().last_recheck_start_state)
                .unwrap_or(end_state);
            (Trigger::PushedErrorsRecheckStreaming, start)
        }
        lsp_prot::ErrorsReason::EnvChange => (Trigger::PushedErrorsEnvChange, end_state),
        lsp_prot::ErrorsReason::NewSubscription => {
            (Trigger::PushedErrorsNewSubscription, end_state)
        }
    };
    log_inner(&Ux::PushedErrors, &trigger, &start_state, &end_state);
}

pub fn log(end_state: State, ux: Ux, id: Id) {
    INTERNAL_STATE.with(|s| {
        let mut s = s.borrow_mut();
        if let Some(interaction) = s.pending_interactions.remove(&id) {
            let PendingInteraction {
                start_state,
                trigger,
            } = interaction;
            log_inner(&ux, &trigger, &start_state, &end_state);
        }
    })
}

fn gc_inner(get_state: &dyn Fn() -> State, oldest_allowed: f64) -> Option<f64> {
    INTERNAL_STATE.with(|s| {
        let mut s = s.borrow_mut();
        if s.lowest_pending_id < s.next_id {
            match s.pending_interactions.get(&s.lowest_pending_id) {
                None => {
                    s.lowest_pending_id += 1;
                    drop(s);
                    gc_inner(get_state, oldest_allowed)
                }
                Some(interaction) => {
                    if interaction.start_state.time < oldest_allowed {
                        let lowest = s.lowest_pending_id;
                        let end_state = get_state();
                        if let Some(interaction) = s.pending_interactions.remove(&lowest) {
                            let PendingInteraction {
                                start_state,
                                trigger,
                            } = interaction;
                            log_inner(&Ux::Timeout, &trigger, &start_state, &end_state);
                        }
                        s.lowest_pending_id += 1;
                        drop(s);
                        gc_inner(get_state, oldest_allowed)
                    } else {
                        Some(interaction.start_state.time)
                    }
                }
            }
        } else {
            None
        }
    })
}

const MAX_AGE: f64 = 600.0;

pub fn gc(get_state: &dyn Fn() -> State) -> f64 {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs_f64();
    let oldest_remaining_interaction = gc_inner(get_state, now - MAX_AGE);
    match oldest_remaining_interaction {
        // If there are no pending interactions, then nothing will expire for at least max_age secs
        None => now + MAX_AGE,
        // Otherwise let's check back in when the oldest pending interaction is set to expire
        Some(start_time) => start_time + MAX_AGE,
    }
}

pub fn dismiss_tracks(end_state: State) {
    INTERNAL_STATE.with(|s| {
        loop {
            let should_continue = {
                let mut s = s.borrow_mut();
                if s.lowest_pending_id < s.next_id {
                    let lowest = s.lowest_pending_id;
                    if let Some(interaction) = s.pending_interactions.remove(&lowest) {
                        let PendingInteraction {
                            start_state,
                            trigger,
                        } = interaction;
                        log_inner(&Ux::Dismissed, &trigger, &start_state, &end_state);
                    }
                    s.lowest_pending_id += 1;
                    true
                } else {
                    false
                }
            };
            if !should_continue {
                break;
            }
        }
    })
}

pub fn init() {
    flow_interaction_logger::init();
}

pub async fn flush() {
    flow_interaction_logger::flush().await;
}

pub fn trigger_of_lsp_msg(msg: &LspMessage) -> Option<Trigger> {
    match msg {
        LspMessage::RequestMessage(lsp_id, LspRequest::CodeActionRequest(_)) => {
            Some(Trigger::CodeAction(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::CompletionRequest(_)) => {
            Some(Trigger::Completion(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::DefinitionRequest(_)) => {
            Some(Trigger::Definition(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::DocumentHighlightRequest(_)) => {
            Some(Trigger::DocumentHighlight(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::DocumentSymbolRequest(_)) => {
            Some(Trigger::DocumentSymbol(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::FindReferencesRequest(_)) => {
            Some(Trigger::FindReferences(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::HoverRequest(_)) => {
            Some(Trigger::Hover(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::LLMContextRequest(_)) => {
            Some(Trigger::LLMContext(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::RageRequest) => {
            Some(Trigger::Rage(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::PrepareRenameRequest(_)) => {
            Some(Trigger::PrepareRename(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::RenameRequest(_)) => {
            Some(Trigger::Rename(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::TypeCoverageRequest(_)) => {
            Some(Trigger::TypeCoverage(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::SelectionRangeRequest(_)) => {
            Some(Trigger::SelectionRange(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::SignatureHelpRequest(_)) => {
            Some(Trigger::SignatureHelp(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::TextDocumentDiagnosticsRequest(_)) => {
            Some(Trigger::TextDocumentDiagnostics(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::ExecuteCommandRequest(_)) => {
            Some(Trigger::ExecuteCommand(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::WillRenameFilesRequest(_)) => {
            Some(Trigger::WillRenameFiles(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::AutoCloseJsxRequest(_)) => {
            Some(Trigger::AutoCloseJsx(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::PrepareDocumentPasteRequest(_)) => {
            Some(Trigger::PrepareDocumentPaste(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::ProvideDocumentPasteRequest(_)) => {
            Some(Trigger::ProvideDocumentPaste(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::LinkedEditingRangeRequest(_)) => {
            Some(Trigger::LinkedEditingRange(lsp_id.clone()))
        }
        LspMessage::RequestMessage(lsp_id, LspRequest::RenameFileImportsRequest(_)) => {
            Some(Trigger::RenameFileImports(lsp_id.clone()))
        }
        LspMessage::RequestMessage(
            _,
            LspRequest::ApplyWorkspaceEditRequest(_)
            | LspRequest::CompletionItemResolveRequest(_)
            | LspRequest::ConfigurationRequest(_)
            | LspRequest::DocumentFormattingRequest(_)
            | LspRequest::DocumentOnTypeFormattingRequest(_)
            | LspRequest::DocumentRangeFormattingRequest(_)
            | LspRequest::InitializeRequest(_)
            | LspRequest::ShowMessageRequestRequest(_)
            | LspRequest::ShowStatusRequest(_)
            | LspRequest::ShutdownRequest
            | LspRequest::CodeLensResolveRequest(_)
            | LspRequest::DocumentCodeLensRequest(_)
            | LspRequest::PingRequest
            | LspRequest::TypeDefinitionRequest(_)
            | LspRequest::UnknownRequest(_, _)
            | LspRequest::WorkspaceSymbolRequest(_)
            | LspRequest::RegisterCapabilityRequest(_),
        ) => None,
        LspMessage::ResponseMessage(
            _,
            LspResult::InitializeResult(_)
            | LspResult::ShutdownResult
            | LspResult::CodeLensResolveResult(_)
            | LspResult::HoverResult(_)
            | LspResult::DefinitionResult(_)
            | LspResult::CompletionResult(_)
            | LspResult::CompletionItemResolveResult(_)
            | LspResult::ConfigurationResult(_)
            | LspResult::SelectionRangeResult(_)
            | LspResult::SignatureHelpResult(_)
            | LspResult::WorkspaceSymbolResult(_)
            | LspResult::DocumentSymbolResult(_)
            | LspResult::FindReferencesResult(_)
            | LspResult::GoToImplementationResult(_)
            | LspResult::DocumentHighlightResult(_)
            | LspResult::DocumentCodeLensResult(_)
            | LspResult::TextDocumentDiagnosticsResult(_)
            | LspResult::TypeCoverageResult(_)
            | LspResult::ExecuteCommandResult(_)
            | LspResult::ApplyWorkspaceEditResult(_)
            | LspResult::RegisterCapabilityResult
            | LspResult::TypeDefinitionResult(_)
            | LspResult::DocumentFormattingResult(_)
            | LspResult::DocumentRangeFormattingResult(_)
            | LspResult::DocumentOnTypeFormattingResult(_)
            | LspResult::ShowMessageRequestResult(_)
            | LspResult::ShowStatusResult(_)
            | LspResult::RageResult(_)
            | LspResult::PingResult(_)
            | LspResult::PrepareRenameResult(_)
            | LspResult::RenameResult(_)
            | LspResult::ErrorResult(_, _)
            | LspResult::CodeActionResult(_)
            | LspResult::AutoCloseJsxResult(_)
            | LspResult::PrepareDocumentPasteResult(_)
            | LspResult::ProvideDocumentPasteResult(_)
            | LspResult::WillRenameFilesResult(_)
            | LspResult::LinkedEditingRangeResult(_)
            | LspResult::RenameFileImportsResult(_)
            | LspResult::LLMContextResult(_),
        ) => None,
        LspMessage::NotificationMessage(LspNotification::DidOpenNotification(_)) => {
            Some(Trigger::DidOpen)
        }
        LspMessage::NotificationMessage(LspNotification::DidCloseNotification(_)) => {
            Some(Trigger::DidClose)
        }
        LspMessage::NotificationMessage(LspNotification::DidSaveNotification(_)) => {
            Some(Trigger::DidSave)
        }
        LspMessage::NotificationMessage(LspNotification::DidChangeNotification(_)) => {
            Some(Trigger::DidChange)
        }
        LspMessage::NotificationMessage(
            LspNotification::ExitNotification
            | LspNotification::CancelRequestNotification(_)
            | LspNotification::PublishDiagnosticsNotification(_)
            | LspNotification::LogMessageNotification(_)
            | LspNotification::TelemetryNotification(_)
            | LspNotification::ShowMessageNotification(_)
            | LspNotification::ConnectionStatusNotification(_)
            | LspNotification::InitializedNotification
            | LspNotification::SetTraceNotification
            | LspNotification::LogTraceNotification
            | LspNotification::UnknownNotification(_, _)
            | LspNotification::DidChangeConfigurationNotification(_)
            | LspNotification::DidChangeWatchedFilesNotification(_),
        ) => None,
    }
}
