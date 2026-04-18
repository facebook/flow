/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;

use lsp_types::*;

pub type DocumentUri = Url;

pub type UriMap<V> = std::collections::BTreeMap<Url, V>;

pub mod auto_close_jsx {
    pub type Params = lsp_types::TextDocumentPositionParams;
    pub type Result = Option<String>;
}

pub mod type_coverage {
    use lsp_types::Range;
    use lsp_types::TextDocumentIdentifier;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Params {
        pub text_document: TextDocumentIdentifier,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct UncoveredRange {
        pub range: Range,
        pub message: Option<String>,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Result {
        pub covered_percent: i32,
        pub uncovered_ranges: Vec<UncoveredRange>,
        pub default_message: String,
    }
}

pub mod connection_status {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Params {
        pub is_connected: bool,
    }
}

pub mod show_status {
    use lsp_types::MessageActionItem;
    use lsp_types::ShowMessageRequestParams;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Params {
        pub request: ShowMessageRequestParams,
        pub progress: Option<i32>,
        pub total: Option<i32>,
        pub short_message: Option<String>,
        pub background_color: Option<ShowStatusBackgroundColor>,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub enum ShowStatusBackgroundColor {
        Error,
        Warning,
    }

    pub type Result = Option<MessageActionItem>;
}

pub mod rename_files {
    use lsp_types::Url;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct FileRename {
        pub old_uri: Url,
        pub new_uri: Url,
    }
}

pub mod will_rename_files {
    use lsp_types::WorkspaceEdit;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Params {
        pub files: Vec<super::rename_files::FileRename>,
    }

    pub type Result = WorkspaceEdit;
}

pub mod rename_file_imports {
    use lsp_types::WorkspaceEdit;

    pub type Params = super::rename_files::FileRename;
    pub type Result = WorkspaceEdit;
}

pub mod llm_context {
    use lsp_types::WorkspaceFolder;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct EnvironmentDetails {
        pub workspace_folders: Vec<WorkspaceFolder>,
        pub os: String,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Params {
        pub edited_file_paths: Vec<String>,
        pub environment_details: EnvironmentDetails,
        pub token_budget: i32,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Result {
        pub llm_context: String,
        pub files_processed: Vec<String>,
        pub tokens_used: i32,
        pub truncated: bool,
    }
}

pub mod document_paste {
    use lsp_types::Range;
    use lsp_types::TextDocumentItem;
    use lsp_types::Url;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub enum ImportType {
        ImportNamedValue,
        ImportValueAsNamespace,
        ImportNamedType,
        ImportNamedTypeOf,
        ImportTypeOfAsNamespace,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct ImportItem {
        pub remote_name: String,
        pub local_name: Option<String>,
        pub import_type: ImportType,
        pub import_source: String,
        pub import_source_is_resolved: bool,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub enum DataTransfer {
        ImportMetadata { imports: Vec<ImportItem> },
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct PrepareParams {
        pub uri: Url,
        pub ranges: Vec<Range>,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct ProvideParams {
        pub text_document: TextDocumentItem,
        pub ranges: Vec<Range>,
        pub data_transfer: DataTransfer,
    }
}

pub mod text_document_diagnostics {
    use lsp_types::Diagnostic;
    use lsp_types::TextDocumentIdentifier;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Params {
        pub text_document: TextDocumentIdentifier,
    }

    pub type Result = Vec<Diagnostic>;
}

pub mod ping {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Result {
        pub start_server_status: Option<String>,
    }
}

pub mod rage {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct RageItem {
        pub title: Option<String>,
        pub data: String,
    }

    pub type Result = Vec<RageItem>;
}

pub mod lsp_error {
    #[derive(
        Debug,
        Clone,
        Copy,
        PartialEq,
        Eq,
        serde::Serialize,
        serde::Deserialize
    )]
    pub enum Code {
        ParseError,
        InvalidRequest,
        MethodNotFound,
        InvalidParams,
        InternalError,
        ServerErrorStart,
        ServerErrorEnd,
        ServerNotInitialized,
        UnknownErrorCode,
        RequestCancelled,
        ContentModified,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct T {
        pub code: Code,
        pub message: String,
        pub data: Option<serde_json::Value>,
    }
}

pub mod register_capability {
    use lsp_types::DidChangeWatchedFilesRegistrationOptions;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub enum Options {
        DidChangeConfiguration,
        DidChangeWatchedFiles(DidChangeWatchedFilesRegistrationOptions),
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Registration {
        pub id: String,
        pub method: String,
        pub register_options: Options,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct Params {
        pub registrations: Vec<Registration>,
    }
}

pub mod workspace_symbol_information {
    use lsp_types::SymbolKind;
    use lsp_types::TextDocumentIdentifier;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct T {
        pub name: String,
        pub kind: SymbolKind,
        pub location: TextDocumentIdentifier,
        pub container_name: Option<String>,
    }
}

pub mod workspace_symbol_result {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub enum T {
        SymbolInformation(Vec<lsp_types::SymbolInformation>),
        WorkspaceSymbolInformation(Vec<super::workspace_symbol_information::T>),
    }
}

pub mod document_symbol_result {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub enum T {
        SymbolInformation(Vec<lsp_types::SymbolInformation>),
        DocumentSymbol(Vec<lsp_types::DocumentSymbol>),
    }
}

pub type LspId = NumberOrString;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum LspRequest {
    InitializeRequest(InitializeParams),
    RegisterCapabilityRequest(register_capability::Params),
    ShutdownRequest,
    CodeLensResolveRequest(CodeLens),
    HoverRequest(HoverParams),
    DefinitionRequest(GotoDefinitionParams),
    TypeDefinitionRequest(GotoDefinitionParams),
    CodeActionRequest(CodeActionParams),
    CompletionRequest(CompletionParams),
    CompletionItemResolveRequest(CompletionItem),
    ConfigurationRequest(ConfigurationParams),
    SelectionRangeRequest(SelectionRangeParams),
    SignatureHelpRequest(SignatureHelpParams),
    TextDocumentDiagnosticsRequest(text_document_diagnostics::Params),
    WorkspaceSymbolRequest(WorkspaceSymbolParams),
    DocumentSymbolRequest(DocumentSymbolParams),
    FindReferencesRequest(ReferenceParams),
    DocumentHighlightRequest(DocumentHighlightParams),
    TypeCoverageRequest(type_coverage::Params),
    DocumentFormattingRequest(DocumentFormattingParams),
    DocumentRangeFormattingRequest(DocumentRangeFormattingParams),
    DocumentOnTypeFormattingRequest(DocumentOnTypeFormattingParams),
    ShowMessageRequestRequest(ShowMessageRequestParams),
    ShowStatusRequest(show_status::Params),
    RageRequest,
    PingRequest,
    PrepareRenameRequest(TextDocumentPositionParams),
    RenameRequest(RenameParams),
    DocumentCodeLensRequest(CodeLensParams),
    ExecuteCommandRequest(ExecuteCommandParams),
    ApplyWorkspaceEditRequest(ApplyWorkspaceEditParams),
    WillRenameFilesRequest(will_rename_files::Params),
    AutoCloseJsxRequest(auto_close_jsx::Params),
    PrepareDocumentPasteRequest(document_paste::PrepareParams),
    ProvideDocumentPasteRequest(document_paste::ProvideParams),
    LinkedEditingRangeRequest(LinkedEditingRangeParams),
    RenameFileImportsRequest(rename_file_imports::Params),
    LLMContextRequest(llm_context::Params),
    UnknownRequest(String, Option<serde_json::Value>),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum LspResult {
    InitializeResult(InitializeResult),
    ShutdownResult,
    CodeLensResolveResult(CodeLens),
    HoverResult(Option<Hover>),
    DefinitionResult(Vec<Location>),
    TypeDefinitionResult(Vec<Location>),
    CodeActionResult(Vec<CodeActionOrCommand>),
    CompletionResult(CompletionResponse),
    CompletionItemResolveResult(CompletionItem),
    ConfigurationResult(Vec<serde_json::Value>),
    SelectionRangeResult(Vec<SelectionRange>),
    SignatureHelpResult(Option<SignatureHelp>),
    TextDocumentDiagnosticsResult(text_document_diagnostics::Result),
    WorkspaceSymbolResult(workspace_symbol_result::T),
    DocumentSymbolResult(document_symbol_result::T),
    FindReferencesResult(Vec<Location>),
    GoToImplementationResult(Vec<Location>),
    DocumentHighlightResult(Vec<DocumentHighlight>),
    TypeCoverageResult(type_coverage::Result),
    DocumentFormattingResult(Vec<TextEdit>),
    DocumentRangeFormattingResult(Vec<TextEdit>),
    DocumentOnTypeFormattingResult(Vec<TextEdit>),
    ShowMessageRequestResult(Option<MessageActionItem>),
    ShowStatusResult(show_status::Result),
    RageResult(rage::Result),
    PingResult(ping::Result),
    PrepareRenameResult(Option<Range>),
    RegisterCapabilityResult,
    RenameResult(WorkspaceEdit),
    DocumentCodeLensResult(Vec<CodeLens>),
    ExecuteCommandResult(()),
    ApplyWorkspaceEditResult(ApplyWorkspaceEditResponse),
    WillRenameFilesResult(will_rename_files::Result),
    AutoCloseJsxResult(auto_close_jsx::Result),
    PrepareDocumentPasteResult(document_paste::DataTransfer),
    ProvideDocumentPasteResult(WorkspaceEdit),
    LinkedEditingRangeResult(Option<LinkedEditingRanges>),
    RenameFileImportsResult(rename_file_imports::Result),
    LLMContextResult(llm_context::Result),
    ErrorResult(lsp_error::T, String),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum LspNotification {
    ExitNotification,
    CancelRequestNotification(CancelParams),
    PublishDiagnosticsNotification(PublishDiagnosticsParams),
    DidOpenNotification(DidOpenTextDocumentParams),
    DidCloseNotification(DidCloseTextDocumentParams),
    DidSaveNotification(DidSaveTextDocumentParams),
    DidChangeNotification(DidChangeTextDocumentParams),
    DidChangeConfigurationNotification(DidChangeConfigurationParams),
    DidChangeWatchedFilesNotification(DidChangeWatchedFilesParams),
    LogMessageNotification(LogMessageParams),
    TelemetryNotification(LogMessageParams),
    ShowMessageNotification(ShowMessageParams),
    ConnectionStatusNotification(connection_status::Params),
    InitializedNotification,
    SetTraceNotification,
    LogTraceNotification,
    UnknownNotification(String, Option<serde_json::Value>),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum LspMessage {
    RequestMessage(LspId, LspRequest),
    ResponseMessage(LspId, LspResult),
    NotificationMessage(LspNotification),
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum TextEditOrInsertReplaceEdit {
    TextEdit(TextEdit),
    InsertReplaceEdit(InsertReplaceEdit),
}

pub struct T {
    pub of_apply_workspace_edit_params:
        Box<dyn Fn(&T, ApplyWorkspaceEditParams) -> ApplyWorkspaceEditParams>,
    pub of_apply_workspace_edit_result:
        Box<dyn Fn(&T, ApplyWorkspaceEditResponse) -> ApplyWorkspaceEditResponse>,
    pub of_auto_close_jsx_params: Box<dyn Fn(&T, auto_close_jsx::Params) -> auto_close_jsx::Params>,
    pub of_auto_close_jsx_result: Box<dyn Fn(&T, auto_close_jsx::Result) -> auto_close_jsx::Result>,
    pub of_cancel_request_params: Box<dyn Fn(&T, CancelParams) -> CancelParams>,
    pub of_code_action: Box<dyn Fn(&T, CodeAction) -> CodeAction>,
    pub of_code_action_context: Box<dyn Fn(&T, CodeActionContext) -> CodeActionContext>,
    pub of_code_action_request_params: Box<dyn Fn(&T, CodeActionParams) -> CodeActionParams>,
    pub of_code_action_result:
        Box<dyn Fn(&T, Vec<CodeActionOrCommand>) -> Vec<CodeActionOrCommand>>,
    pub of_code_lens: Box<dyn Fn(&T, CodeLens) -> CodeLens>,
    pub of_code_lens_resolve_params: Box<dyn Fn(&T, CodeLens) -> CodeLens>,
    pub of_code_lens_resolve_result: Box<dyn Fn(&T, CodeLens) -> CodeLens>,
    pub of_command: Box<dyn Fn(&T, Command) -> Command>,
    pub of_completion_item: Box<dyn Fn(&T, CompletionItem) -> CompletionItem>,
    pub of_completion_params: Box<dyn Fn(&T, CompletionParams) -> CompletionParams>,
    pub of_completion_result: Box<dyn Fn(&T, CompletionResponse) -> CompletionResponse>,
    pub of_configuration_params: Box<dyn Fn(&T, ConfigurationParams) -> ConfigurationParams>,
    pub of_configuration_result: Box<dyn Fn(&T, Vec<serde_json::Value>) -> Vec<serde_json::Value>>,
    pub of_connection_status_params:
        Box<dyn Fn(&T, connection_status::Params) -> connection_status::Params>,
    pub of_definition_params: Box<dyn Fn(&T, GotoDefinitionParams) -> GotoDefinitionParams>,
    pub of_definition_result: Box<dyn Fn(&T, Vec<Location>) -> Vec<Location>>,
    pub of_diagnostic: Box<dyn Fn(&T, Diagnostic) -> Diagnostic>,
    pub of_did_change_configuration_params:
        Box<dyn Fn(&T, DidChangeConfigurationParams) -> DidChangeConfigurationParams>,
    pub of_did_change_content_change_event:
        Box<dyn Fn(&T, TextDocumentContentChangeEvent) -> TextDocumentContentChangeEvent>,
    pub of_did_change_params:
        Box<dyn Fn(&T, DidChangeTextDocumentParams) -> DidChangeTextDocumentParams>,
    pub of_did_change_watched_files_params:
        Box<dyn Fn(&T, DidChangeWatchedFilesParams) -> DidChangeWatchedFilesParams>,
    pub of_did_close_params:
        Box<dyn Fn(&T, DidCloseTextDocumentParams) -> DidCloseTextDocumentParams>,
    pub of_did_open_params: Box<dyn Fn(&T, DidOpenTextDocumentParams) -> DidOpenTextDocumentParams>,
    pub of_did_save_params: Box<dyn Fn(&T, DidSaveTextDocumentParams) -> DidSaveTextDocumentParams>,
    pub of_document_code_lens_params: Box<dyn Fn(&T, CodeLensParams) -> CodeLensParams>,
    pub of_document_code_lens_result: Box<dyn Fn(&T, Vec<CodeLens>) -> Vec<CodeLens>>,
    pub of_document_formatting_params:
        Box<dyn Fn(&T, DocumentFormattingParams) -> DocumentFormattingParams>,
    pub of_document_formatting_result: Box<dyn Fn(&T, Vec<TextEdit>) -> Vec<TextEdit>>,
    pub of_document_highlight_params:
        Box<dyn Fn(&T, DocumentHighlightParams) -> DocumentHighlightParams>,
    pub of_document_highlight_result:
        Box<dyn Fn(&T, Vec<DocumentHighlight>) -> Vec<DocumentHighlight>>,
    pub of_document_on_type_formatting_params:
        Box<dyn Fn(&T, DocumentOnTypeFormattingParams) -> DocumentOnTypeFormattingParams>,
    pub of_document_on_type_formatting_result: Box<dyn Fn(&T, Vec<TextEdit>) -> Vec<TextEdit>>,
    pub of_document_paste_prepare_params:
        Box<dyn Fn(&T, document_paste::PrepareParams) -> document_paste::PrepareParams>,
    pub of_document_paste_provide_params:
        Box<dyn Fn(&T, document_paste::ProvideParams) -> document_paste::ProvideParams>,
    pub of_document_range_formatting_params:
        Box<dyn Fn(&T, DocumentRangeFormattingParams) -> DocumentRangeFormattingParams>,
    pub of_document_range_formatting_result: Box<dyn Fn(&T, Vec<TextEdit>) -> Vec<TextEdit>>,
    pub of_document_symbol_params: Box<dyn Fn(&T, DocumentSymbolParams) -> DocumentSymbolParams>,
    pub of_document_symbol_result:
        Box<dyn Fn(&T, document_symbol_result::T) -> document_symbol_result::T>,
    pub of_document_symbol: Box<dyn Fn(&T, DocumentSymbol) -> DocumentSymbol>,
    pub of_document_uri: Box<dyn Fn(&T, DocumentUri) -> DocumentUri>,
    pub of_execute_command_params: Box<dyn Fn(&T, ExecuteCommandParams) -> ExecuteCommandParams>,
    pub of_execute_command_result: Box<dyn Fn(&T, ())>,
    pub of_find_references_params: Box<dyn Fn(&T, ReferenceParams) -> ReferenceParams>,
    pub of_find_references_result: Box<dyn Fn(&T, Vec<Location>) -> Vec<Location>>,
    pub of_go_to_implementation_result: Box<dyn Fn(&T, Vec<Location>) -> Vec<Location>>,
    pub of_hover_params: Box<dyn Fn(&T, HoverParams) -> HoverParams>,
    pub of_hover_result: Box<dyn Fn(&T, Option<Hover>) -> Option<Hover>>,
    pub of_initialize_params: Box<dyn Fn(&T, InitializeParams) -> InitializeParams>,
    pub of_initialize_result: Box<dyn Fn(&T, InitializeResult) -> InitializeResult>,
    pub of_insert_replace_edit: Box<dyn Fn(&T, InsertReplaceEdit) -> InsertReplaceEdit>,
    pub of_linked_editing_range_params:
        Box<dyn Fn(&T, LinkedEditingRangeParams) -> LinkedEditingRangeParams>,
    pub of_linked_editing_range_result:
        Box<dyn Fn(&T, Option<LinkedEditingRanges>) -> Option<LinkedEditingRanges>>,
    pub of_log_message_params: Box<dyn Fn(&T, LogMessageParams) -> LogMessageParams>,
    pub of_lsp_message: Box<dyn Fn(&T, LspMessage) -> LspMessage>,
    pub of_lsp_notification: Box<dyn Fn(&T, LspNotification) -> LspNotification>,
    pub of_lsp_result: Box<dyn Fn(&T, LspResult) -> LspResult>,
    pub of_lsp_request: Box<dyn Fn(&T, LspRequest) -> LspRequest>,
    pub of_location: Box<dyn Fn(&T, Location) -> Location>,
    pub of_publish_diagnostics_params:
        Box<dyn Fn(&T, PublishDiagnosticsParams) -> PublishDiagnosticsParams>,
    pub of_rage_result: Box<dyn Fn(&T, rage::Result) -> rage::Result>,
    pub of_prepare_rename_params:
        Box<dyn Fn(&T, TextDocumentPositionParams) -> TextDocumentPositionParams>,
    pub of_prepare_rename_result: Box<dyn Fn(&T, Option<Range>) -> Option<Range>>,
    pub of_ping_result: Box<dyn Fn(&T, ping::Result) -> ping::Result>,
    pub of_range: Box<dyn Fn(&T, Range) -> Range>,
    pub of_register_capability_params:
        Box<dyn Fn(&T, register_capability::Params) -> register_capability::Params>,
    pub of_rename_params: Box<dyn Fn(&T, RenameParams) -> RenameParams>,
    pub of_rename_result: Box<dyn Fn(&T, WorkspaceEdit) -> WorkspaceEdit>,
    pub of_rename_file_imports_params:
        Box<dyn Fn(&T, rename_file_imports::Params) -> rename_file_imports::Params>,
    pub of_rename_file_imports_result:
        Box<dyn Fn(&T, rename_file_imports::Result) -> rename_file_imports::Result>,
    pub of_llm_context_params: Box<dyn Fn(&T, llm_context::Params) -> llm_context::Params>,
    pub of_llm_context_result: Box<dyn Fn(&T, llm_context::Result) -> llm_context::Result>,
    pub of_selection_range: Box<dyn Fn(&T, SelectionRange) -> SelectionRange>,
    pub of_selection_range_params: Box<dyn Fn(&T, SelectionRangeParams) -> SelectionRangeParams>,
    pub of_selection_range_result: Box<dyn Fn(&T, Vec<SelectionRange>) -> Vec<SelectionRange>>,
    pub of_signature_help_params: Box<dyn Fn(&T, SignatureHelpParams) -> SignatureHelpParams>,
    pub of_signature_help_result: Box<dyn Fn(&T, Option<SignatureHelp>) -> Option<SignatureHelp>>,
    pub of_text_document_diagnostics_params:
        Box<dyn Fn(&T, text_document_diagnostics::Params) -> text_document_diagnostics::Params>,
    pub of_text_document_diagnostics_result:
        Box<dyn Fn(&T, text_document_diagnostics::Result) -> text_document_diagnostics::Result>,
    pub of_show_message_params: Box<dyn Fn(&T, ShowMessageParams) -> ShowMessageParams>,
    pub of_show_message_request_params:
        Box<dyn Fn(&T, ShowMessageRequestParams) -> ShowMessageRequestParams>,
    pub of_show_message_request_result:
        Box<dyn Fn(&T, Option<MessageActionItem>) -> Option<MessageActionItem>>,
    pub of_show_status_params: Box<dyn Fn(&T, show_status::Params) -> show_status::Params>,
    pub of_show_status_result: Box<dyn Fn(&T, show_status::Result) -> show_status::Result>,
    pub of_symbol_information: Box<dyn Fn(&T, SymbolInformation) -> SymbolInformation>,
    pub of_workspace_symbol_information:
        Box<dyn Fn(&T, workspace_symbol_information::T) -> workspace_symbol_information::T>,
    pub of_text_document_identifier:
        Box<dyn Fn(&T, TextDocumentIdentifier) -> TextDocumentIdentifier>,
    pub of_text_document_item: Box<dyn Fn(&T, TextDocumentItem) -> TextDocumentItem>,
    pub of_text_document_position_params:
        Box<dyn Fn(&T, TextDocumentPositionParams) -> TextDocumentPositionParams>,
    pub of_text_edit: Box<dyn Fn(&T, TextEdit) -> TextEdit>,
    pub of_text_edit_or_insert_replace_edit:
        Box<dyn Fn(&T, TextEditOrInsertReplaceEdit) -> TextEditOrInsertReplaceEdit>,
    pub of_type_coverage_params: Box<dyn Fn(&T, type_coverage::Params) -> type_coverage::Params>,
    pub of_type_coverage_result: Box<dyn Fn(&T, type_coverage::Result) -> type_coverage::Result>,
    pub of_type_definition_params: Box<dyn Fn(&T, GotoDefinitionParams) -> GotoDefinitionParams>,
    pub of_type_definition_result: Box<dyn Fn(&T, Vec<Location>) -> Vec<Location>>,
    pub of_versioned_text_document_identifier:
        Box<dyn Fn(&T, VersionedTextDocumentIdentifier) -> VersionedTextDocumentIdentifier>,
    pub of_will_rename_files_params:
        Box<dyn Fn(&T, will_rename_files::Params) -> will_rename_files::Params>,
    pub of_will_rename_files_result:
        Box<dyn Fn(&T, will_rename_files::Result) -> will_rename_files::Result>,
    pub of_workspace_edit: Box<dyn Fn(&T, WorkspaceEdit) -> WorkspaceEdit>,
    pub of_workspace_symbol_params: Box<dyn Fn(&T, WorkspaceSymbolParams) -> WorkspaceSymbolParams>,
    pub of_workspace_symbol_result:
        Box<dyn Fn(&T, workspace_symbol_result::T) -> workspace_symbol_result::T>,
}

pub fn default_mapper() -> T {
    T {
        of_apply_workspace_edit_params: Box::new(|mapper, params| {
            let edit = (mapper.of_workspace_edit)(mapper, params.edit);
            ApplyWorkspaceEditParams {
                label: params.label,
                edit,
            }
        }),
        of_apply_workspace_edit_result: Box::new(|_mapper, result| result),
        of_auto_close_jsx_params: Box::new(|mapper, params| {
            (mapper.of_text_document_position_params)(mapper, params)
        }),
        of_auto_close_jsx_result: Box::new(|_mapper, result| result),
        of_cancel_request_params: Box::new(|_mapper, params| params),
        of_code_action: Box::new(|mapper, action| {
            let diagnostics = action.diagnostics.map(|diags| {
                diags
                    .into_iter()
                    .map(|d| (mapper.of_diagnostic)(mapper, d))
                    .collect()
            });
            let edit = action
                .edit
                .map(|edit| (mapper.of_workspace_edit)(mapper, edit));
            let command = action.command.map(|cmd| (mapper.of_command)(mapper, cmd));
            CodeAction {
                title: action.title,
                kind: action.kind,
                diagnostics,
                edit,
                command,
                is_preferred: action.is_preferred,
                disabled: action.disabled,
                data: action.data,
            }
        }),
        of_code_action_context: Box::new(|mapper, context| {
            let diagnostics = context
                .diagnostics
                .into_iter()
                .map(|d| (mapper.of_diagnostic)(mapper, d))
                .collect();
            CodeActionContext {
                diagnostics,
                only: context.only,
                trigger_kind: context.trigger_kind,
            }
        }),
        of_code_action_request_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            let range = (mapper.of_range)(mapper, params.range);
            let context = (mapper.of_code_action_context)(mapper, params.context);
            CodeActionParams {
                text_document,
                range,
                context,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_code_action_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|item| match item {
                    CodeActionOrCommand::Command(cmd) => {
                        CodeActionOrCommand::Command((mapper.of_command)(mapper, cmd))
                    }
                    CodeActionOrCommand::CodeAction(action) => {
                        CodeActionOrCommand::CodeAction((mapper.of_code_action)(mapper, action))
                    }
                })
                .collect()
        }),
        of_code_lens: Box::new(|mapper, lens| {
            let range = (mapper.of_range)(mapper, lens.range);
            let command = lens.command.map(|cmd| (mapper.of_command)(mapper, cmd));
            CodeLens {
                range,
                command,
                data: lens.data,
            }
        }),
        of_code_lens_resolve_params: Box::new(|mapper, t| (mapper.of_code_lens)(mapper, t)),
        of_code_lens_resolve_result: Box::new(|mapper, t| (mapper.of_code_lens)(mapper, t)),
        of_command: Box::new(|_mapper, cmd| cmd),
        of_completion_item: Box::new(|mapper, item| {
            let additional_text_edits = item.additional_text_edits.map(|edits| {
                edits
                    .into_iter()
                    .map(|e| (mapper.of_text_edit)(mapper, e))
                    .collect()
            });
            let command = item.command.map(|cmd| (mapper.of_command)(mapper, cmd));
            CompletionItem {
                additional_text_edits,
                command,
                ..item
            }
        }),
        of_completion_params: Box::new(|mapper, params| {
            let text_document_position = TextDocumentPositionParams {
                text_document: (mapper.of_text_document_identifier)(
                    mapper,
                    params.text_document_position.text_document,
                ),
                position: params.text_document_position.position,
            };
            CompletionParams {
                text_document_position,
                context: params.context,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_completion_result: Box::new(|mapper, result| match result {
            CompletionResponse::Array(items) => CompletionResponse::Array(
                items
                    .into_iter()
                    .map(|item| (mapper.of_completion_item)(mapper, item))
                    .collect(),
            ),
            CompletionResponse::List(list) => CompletionResponse::List(CompletionList {
                is_incomplete: list.is_incomplete,
                items: list
                    .items
                    .into_iter()
                    .map(|item| (mapper.of_completion_item)(mapper, item))
                    .collect(),
            }),
        }),
        of_configuration_params: Box::new(|mapper, params| {
            let items = params
                .items
                .into_iter()
                .map(|item| {
                    let scope_uri = item
                        .scope_uri
                        .map(|uri| (mapper.of_document_uri)(mapper, uri));
                    ConfigurationItem {
                        scope_uri,
                        section: item.section,
                    }
                })
                .collect();
            ConfigurationParams { items }
        }),
        of_configuration_result: Box::new(|_mapper, result| result),
        of_connection_status_params: Box::new(|_mapper, params| params),
        of_definition_params: Box::new(|mapper, params| {
            let text_document_position_params = (mapper.of_text_document_position_params)(
                mapper,
                params.text_document_position_params,
            );
            GotoDefinitionParams {
                text_document_position_params,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_definition_result: Box::new(|mapper, results| {
            results
                .into_iter()
                .map(|loc| (mapper.of_location)(mapper, loc))
                .collect()
        }),
        of_diagnostic: Box::new(|mapper, diag| {
            let range = (mapper.of_range)(mapper, diag.range);
            let related_information = diag.related_information.map(|infos| {
                infos
                    .into_iter()
                    .map(|info| {
                        let location = (mapper.of_location)(mapper, info.location);
                        DiagnosticRelatedInformation {
                            location,
                            message: info.message,
                        }
                    })
                    .collect()
            });
            Diagnostic {
                range,
                related_information,
                severity: diag.severity,
                code: diag.code,
                code_description: diag.code_description,
                source: diag.source,
                message: diag.message,
                tags: diag.tags,
                data: diag.data,
            }
        }),
        of_did_change_configuration_params: Box::new(|_mapper, params| params),
        of_did_change_content_change_event: Box::new(|mapper, event| {
            let range = event.range.map(|r| (mapper.of_range)(mapper, r));
            TextDocumentContentChangeEvent {
                range,
                range_length: event.range_length,
                text: event.text,
            }
        }),
        of_did_change_params: Box::new(|mapper, params| {
            let text_document =
                (mapper.of_versioned_text_document_identifier)(mapper, params.text_document);
            let content_changes = params
                .content_changes
                .into_iter()
                .map(|e| (mapper.of_did_change_content_change_event)(mapper, e))
                .collect();
            DidChangeTextDocumentParams {
                text_document,
                content_changes,
            }
        }),
        of_did_change_watched_files_params: Box::new(|mapper, params| {
            let changes = params
                .changes
                .into_iter()
                .map(|change| {
                    let uri = (mapper.of_document_uri)(mapper, change.uri);
                    FileEvent {
                        uri,
                        typ: change.typ,
                    }
                })
                .collect();
            DidChangeWatchedFilesParams { changes }
        }),
        of_did_close_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            DidCloseTextDocumentParams { text_document }
        }),
        of_did_open_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_item)(mapper, params.text_document);
            DidOpenTextDocumentParams { text_document }
        }),
        of_did_save_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            DidSaveTextDocumentParams {
                text_document,
                text: params.text,
            }
        }),
        of_document_code_lens_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            CodeLensParams {
                text_document,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_document_code_lens_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|lens| (mapper.of_code_lens)(mapper, lens))
                .collect()
        }),
        of_document_formatting_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            DocumentFormattingParams {
                text_document,
                options: params.options,
                work_done_progress_params: params.work_done_progress_params,
            }
        }),
        of_document_formatting_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|e| (mapper.of_text_edit)(mapper, e))
                .collect()
        }),
        of_document_highlight_params: Box::new(|mapper, params| {
            let text_document_position_params = (mapper.of_text_document_position_params)(
                mapper,
                params.text_document_position_params,
            );
            DocumentHighlightParams {
                text_document_position_params,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_document_highlight_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|hl| {
                    let range = (mapper.of_range)(mapper, hl.range);
                    DocumentHighlight {
                        range,
                        kind: hl.kind,
                    }
                })
                .collect()
        }),
        of_document_on_type_formatting_params: Box::new(|mapper, params| {
            let text_document_position =
                (mapper.of_text_document_position_params)(mapper, params.text_document_position);
            DocumentOnTypeFormattingParams {
                text_document_position,
                ch: params.ch,
                options: params.options,
            }
        }),
        of_document_on_type_formatting_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|e| (mapper.of_text_edit)(mapper, e))
                .collect()
        }),
        of_document_paste_prepare_params: Box::new(|mapper, params| {
            let uri = (mapper.of_document_uri)(mapper, params.uri);
            let ranges = params
                .ranges
                .into_iter()
                .map(|r| (mapper.of_range)(mapper, r))
                .collect();
            document_paste::PrepareParams { uri, ranges }
        }),
        of_document_paste_provide_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_item)(mapper, params.text_document);
            let ranges = params
                .ranges
                .into_iter()
                .map(|r| (mapper.of_range)(mapper, r))
                .collect();
            document_paste::ProvideParams {
                text_document,
                ranges,
                data_transfer: params.data_transfer,
            }
        }),
        of_document_range_formatting_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            let range = (mapper.of_range)(mapper, params.range);
            DocumentRangeFormattingParams {
                text_document,
                range,
                options: params.options,
                work_done_progress_params: params.work_done_progress_params,
            }
        }),
        of_document_range_formatting_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|e| (mapper.of_text_edit)(mapper, e))
                .collect()
        }),
        of_document_symbol_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            DocumentSymbolParams {
                text_document,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_document_symbol_result: Box::new(|mapper, result| match result {
            document_symbol_result::T::SymbolInformation(infos) => {
                document_symbol_result::T::SymbolInformation(
                    infos
                        .into_iter()
                        .map(|info| (mapper.of_symbol_information)(mapper, info))
                        .collect(),
                )
            }
            document_symbol_result::T::DocumentSymbol(syms) => {
                document_symbol_result::T::DocumentSymbol(
                    syms.into_iter()
                        .map(|sym| (mapper.of_document_symbol)(mapper, sym))
                        .collect(),
                )
            }
        }),
        #[allow(deprecated)]
        of_document_symbol: Box::new(|mapper, sym| {
            let range = (mapper.of_range)(mapper, sym.range);
            let selection_range = (mapper.of_range)(mapper, sym.selection_range);
            let children = sym.children.map(|children| {
                children
                    .into_iter()
                    .map(|child| (mapper.of_document_symbol)(mapper, child))
                    .collect()
            });
            #[allow(deprecated)]
            DocumentSymbol {
                name: sym.name,
                detail: sym.detail,
                kind: sym.kind,
                tags: sym.tags,
                deprecated: sym.deprecated,
                range,
                selection_range,
                children,
            }
        }),
        of_document_uri: Box::new(|_mapper, t| t),
        of_execute_command_params: Box::new(|_mapper, params| params),
        of_execute_command_result: Box::new(|_mapper, ()| ()),
        of_find_references_params: Box::new(|mapper, params| {
            let text_document_position =
                (mapper.of_text_document_position_params)(mapper, params.text_document_position);
            ReferenceParams {
                text_document_position,
                context: params.context,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_find_references_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|loc| (mapper.of_location)(mapper, loc))
                .collect()
        }),
        of_go_to_implementation_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|loc| (mapper.of_location)(mapper, loc))
                .collect()
        }),
        of_hover_params: Box::new(|mapper, params| {
            let text_document_position_params = (mapper.of_text_document_position_params)(
                mapper,
                params.text_document_position_params,
            );
            HoverParams {
                text_document_position_params,
                work_done_progress_params: params.work_done_progress_params,
            }
        }),
        of_hover_result: Box::new(|mapper, result| {
            result.map(|hover| {
                let range = hover.range.map(|r| (mapper.of_range)(mapper, r));
                Hover {
                    contents: hover.contents,
                    range,
                }
            })
        }),
        #[allow(deprecated)]
        of_initialize_params: Box::new(|mapper, params| {
            let root_uri = params
                .root_uri
                .map(|uri| (mapper.of_document_uri)(mapper, uri));
            InitializeParams { root_uri, ..params }
        }),
        of_initialize_result: Box::new(|_mapper, result| result),
        of_insert_replace_edit: Box::new(|mapper, edit| {
            let insert = (mapper.of_range)(mapper, edit.insert);
            let replace = (mapper.of_range)(mapper, edit.replace);
            InsertReplaceEdit {
                new_text: edit.new_text,
                insert,
                replace,
            }
        }),
        of_linked_editing_range_params: Box::new(|mapper, params| {
            let text_document_position_params = (mapper.of_text_document_position_params)(
                mapper,
                params.text_document_position_params,
            );
            LinkedEditingRangeParams {
                text_document_position_params,
                work_done_progress_params: params.work_done_progress_params,
            }
        }),
        of_linked_editing_range_result: Box::new(|mapper, result| {
            result.map(|ranges| {
                let mapped_ranges = ranges
                    .ranges
                    .into_iter()
                    .map(|r| (mapper.of_range)(mapper, r))
                    .collect();
                LinkedEditingRanges {
                    ranges: mapped_ranges,
                    word_pattern: ranges.word_pattern,
                }
            })
        }),
        of_log_message_params: Box::new(|_mapper, params| params),
        of_lsp_message: Box::new(|mapper, message| match message {
            LspMessage::RequestMessage(id, request) => {
                LspMessage::RequestMessage(id, (mapper.of_lsp_request)(mapper, request))
            }
            LspMessage::ResponseMessage(id, result) => {
                LspMessage::ResponseMessage(id, (mapper.of_lsp_result)(mapper, result))
            }
            LspMessage::NotificationMessage(notification) => {
                LspMessage::NotificationMessage((mapper.of_lsp_notification)(mapper, notification))
            }
        }),
        of_lsp_notification: Box::new(|mapper, t| match t {
            LspNotification::ExitNotification => LspNotification::ExitNotification,
            LspNotification::CancelRequestNotification(params) => {
                LspNotification::CancelRequestNotification((mapper.of_cancel_request_params)(
                    mapper, params,
                ))
            }
            LspNotification::PublishDiagnosticsNotification(params) => {
                LspNotification::PublishDiagnosticsNotification((mapper
                    .of_publish_diagnostics_params)(
                    mapper, params
                ))
            }
            LspNotification::DidOpenNotification(params) => {
                LspNotification::DidOpenNotification((mapper.of_did_open_params)(mapper, params))
            }
            LspNotification::DidCloseNotification(params) => {
                LspNotification::DidCloseNotification((mapper.of_did_close_params)(mapper, params))
            }
            LspNotification::DidSaveNotification(params) => {
                LspNotification::DidSaveNotification((mapper.of_did_save_params)(mapper, params))
            }
            LspNotification::DidChangeNotification(params) => {
                LspNotification::DidChangeNotification((mapper.of_did_change_params)(
                    mapper, params,
                ))
            }
            LspNotification::DidChangeConfigurationNotification(params) => {
                LspNotification::DidChangeConfigurationNotification((mapper
                    .of_did_change_configuration_params)(
                    mapper, params
                ))
            }
            LspNotification::DidChangeWatchedFilesNotification(params) => {
                LspNotification::DidChangeWatchedFilesNotification((mapper
                    .of_did_change_watched_files_params)(
                    mapper, params
                ))
            }
            LspNotification::LogMessageNotification(params) => {
                LspNotification::LogMessageNotification((mapper.of_log_message_params)(
                    mapper, params,
                ))
            }
            LspNotification::TelemetryNotification(params) => {
                LspNotification::TelemetryNotification((mapper.of_log_message_params)(
                    mapper, params,
                ))
            }
            LspNotification::ShowMessageNotification(params) => {
                LspNotification::ShowMessageNotification((mapper.of_show_message_params)(
                    mapper, params,
                ))
            }
            LspNotification::ConnectionStatusNotification(params) => {
                LspNotification::ConnectionStatusNotification((mapper.of_connection_status_params)(
                    mapper, params,
                ))
            }
            LspNotification::InitializedNotification => LspNotification::InitializedNotification,
            LspNotification::SetTraceNotification => LspNotification::SetTraceNotification,
            LspNotification::LogTraceNotification => LspNotification::LogTraceNotification,
            LspNotification::UnknownNotification(msg, json) => {
                LspNotification::UnknownNotification(msg, json)
            }
        }),
        of_lsp_result: Box::new(|mapper, result| match result {
            LspResult::InitializeResult(r) => {
                LspResult::InitializeResult((mapper.of_initialize_result)(mapper, r))
            }
            LspResult::ShutdownResult => LspResult::ShutdownResult,
            LspResult::CodeLensResolveResult(r) => {
                LspResult::CodeLensResolveResult((mapper.of_code_lens_resolve_result)(mapper, r))
            }
            LspResult::HoverResult(r) => {
                LspResult::HoverResult((mapper.of_hover_result)(mapper, r))
            }
            LspResult::DefinitionResult(r) => {
                LspResult::DefinitionResult((mapper.of_definition_result)(mapper, r))
            }
            LspResult::TypeDefinitionResult(r) => {
                LspResult::TypeDefinitionResult((mapper.of_type_definition_result)(mapper, r))
            }
            LspResult::CodeActionResult(r) => {
                LspResult::CodeActionResult((mapper.of_code_action_result)(mapper, r))
            }
            LspResult::CompletionResult(r) => {
                LspResult::CompletionResult((mapper.of_completion_result)(mapper, r))
            }
            LspResult::CompletionItemResolveResult(r) => {
                LspResult::CompletionItemResolveResult((mapper.of_completion_item)(mapper, r))
            }
            LspResult::ConfigurationResult(r) => {
                LspResult::ConfigurationResult((mapper.of_configuration_result)(mapper, r))
            }
            LspResult::SelectionRangeResult(r) => {
                LspResult::SelectionRangeResult((mapper.of_selection_range_result)(mapper, r))
            }
            LspResult::SignatureHelpResult(r) => {
                LspResult::SignatureHelpResult((mapper.of_signature_help_result)(mapper, r))
            }
            LspResult::TextDocumentDiagnosticsResult(r) => {
                LspResult::TextDocumentDiagnosticsResult((mapper
                    .of_text_document_diagnostics_result)(
                    mapper, r
                ))
            }
            LspResult::WorkspaceSymbolResult(r) => {
                LspResult::WorkspaceSymbolResult((mapper.of_workspace_symbol_result)(mapper, r))
            }
            LspResult::DocumentSymbolResult(r) => {
                LspResult::DocumentSymbolResult((mapper.of_document_symbol_result)(mapper, r))
            }
            LspResult::FindReferencesResult(r) => {
                LspResult::FindReferencesResult((mapper.of_find_references_result)(mapper, r))
            }
            LspResult::GoToImplementationResult(r) => LspResult::GoToImplementationResult((mapper
                .of_go_to_implementation_result)(
                mapper, r,
            )),
            LspResult::DocumentHighlightResult(r) => {
                LspResult::DocumentHighlightResult((mapper.of_document_highlight_result)(mapper, r))
            }
            LspResult::TypeCoverageResult(r) => {
                LspResult::TypeCoverageResult((mapper.of_type_coverage_result)(mapper, r))
            }
            LspResult::DocumentFormattingResult(r) => LspResult::DocumentFormattingResult((mapper
                .of_document_formatting_result)(
                mapper, r,
            )),
            LspResult::DocumentRangeFormattingResult(r) => {
                LspResult::DocumentRangeFormattingResult((mapper
                    .of_document_range_formatting_result)(
                    mapper, r
                ))
            }
            LspResult::DocumentOnTypeFormattingResult(r) => {
                LspResult::DocumentOnTypeFormattingResult((mapper
                    .of_document_on_type_formatting_result)(
                    mapper, r
                ))
            }
            LspResult::ShowMessageRequestResult(r) => LspResult::ShowMessageRequestResult((mapper
                .of_show_message_request_result)(
                mapper, r,
            )),
            LspResult::ShowStatusResult(r) => {
                LspResult::ShowStatusResult((mapper.of_show_status_result)(mapper, r))
            }
            LspResult::RageResult(r) => LspResult::RageResult((mapper.of_rage_result)(mapper, r)),
            LspResult::PingResult(r) => LspResult::PingResult((mapper.of_ping_result)(mapper, r)),
            LspResult::PrepareRenameResult(r) => {
                LspResult::PrepareRenameResult((mapper.of_prepare_rename_result)(mapper, r))
            }
            LspResult::RegisterCapabilityResult => LspResult::RegisterCapabilityResult,
            LspResult::RenameResult(r) => {
                LspResult::RenameResult((mapper.of_rename_result)(mapper, r))
            }
            LspResult::DocumentCodeLensResult(r) => {
                LspResult::DocumentCodeLensResult((mapper.of_document_code_lens_result)(mapper, r))
            }
            LspResult::ExecuteCommandResult(r) => {
                LspResult::ExecuteCommandResult((mapper.of_execute_command_result)(mapper, r))
            }
            LspResult::ApplyWorkspaceEditResult(r) => LspResult::ApplyWorkspaceEditResult((mapper
                .of_apply_workspace_edit_result)(
                mapper, r,
            )),
            LspResult::WillRenameFilesResult(r) => {
                LspResult::WillRenameFilesResult((mapper.of_will_rename_files_result)(mapper, r))
            }
            LspResult::AutoCloseJsxResult(r) => {
                LspResult::AutoCloseJsxResult((mapper.of_auto_close_jsx_result)(mapper, r))
            }
            LspResult::PrepareDocumentPasteResult(r) => LspResult::PrepareDocumentPasteResult(r),
            LspResult::ProvideDocumentPasteResult(r) => {
                LspResult::ProvideDocumentPasteResult((mapper.of_workspace_edit)(mapper, r))
            }
            LspResult::LinkedEditingRangeResult(r) => LspResult::LinkedEditingRangeResult((mapper
                .of_linked_editing_range_result)(
                mapper, r,
            )),
            LspResult::RenameFileImportsResult(r) => LspResult::RenameFileImportsResult((mapper
                .of_rename_file_imports_result)(
                mapper, r,
            )),
            LspResult::LLMContextResult(r) => {
                LspResult::LLMContextResult((mapper.of_llm_context_result)(mapper, r))
            }
            LspResult::ErrorResult(err, s) => LspResult::ErrorResult(err, s),
        }),
        of_lsp_request: Box::new(|mapper, request| match request {
            LspRequest::InitializeRequest(p) => {
                LspRequest::InitializeRequest((mapper.of_initialize_params)(mapper, p))
            }
            LspRequest::RegisterCapabilityRequest(p) => LspRequest::RegisterCapabilityRequest(
                (mapper.of_register_capability_params)(mapper, p),
            ),
            LspRequest::ShutdownRequest => LspRequest::ShutdownRequest,
            LspRequest::CodeLensResolveRequest(p) => {
                LspRequest::CodeLensResolveRequest((mapper.of_code_lens_resolve_params)(mapper, p))
            }
            LspRequest::HoverRequest(p) => {
                LspRequest::HoverRequest((mapper.of_hover_params)(mapper, p))
            }
            LspRequest::DefinitionRequest(p) => {
                LspRequest::DefinitionRequest((mapper.of_definition_params)(mapper, p))
            }
            LspRequest::TypeDefinitionRequest(p) => {
                LspRequest::TypeDefinitionRequest((mapper.of_type_definition_params)(mapper, p))
            }
            LspRequest::CodeActionRequest(p) => {
                LspRequest::CodeActionRequest((mapper.of_code_action_request_params)(mapper, p))
            }
            LspRequest::CompletionRequest(p) => {
                LspRequest::CompletionRequest((mapper.of_completion_params)(mapper, p))
            }
            LspRequest::CompletionItemResolveRequest(p) => {
                LspRequest::CompletionItemResolveRequest((mapper.of_completion_item)(mapper, p))
            }
            LspRequest::ConfigurationRequest(p) => {
                LspRequest::ConfigurationRequest((mapper.of_configuration_params)(mapper, p))
            }
            LspRequest::SelectionRangeRequest(p) => {
                LspRequest::SelectionRangeRequest((mapper.of_selection_range_params)(mapper, p))
            }
            LspRequest::SignatureHelpRequest(p) => {
                LspRequest::SignatureHelpRequest((mapper.of_signature_help_params)(mapper, p))
            }
            LspRequest::TextDocumentDiagnosticsRequest(p) => {
                LspRequest::TextDocumentDiagnosticsRequest((mapper
                    .of_text_document_diagnostics_params)(
                    mapper, p
                ))
            }
            LspRequest::WorkspaceSymbolRequest(p) => {
                LspRequest::WorkspaceSymbolRequest((mapper.of_workspace_symbol_params)(mapper, p))
            }
            LspRequest::DocumentSymbolRequest(p) => {
                LspRequest::DocumentSymbolRequest((mapper.of_document_symbol_params)(mapper, p))
            }
            LspRequest::FindReferencesRequest(p) => {
                LspRequest::FindReferencesRequest((mapper.of_find_references_params)(mapper, p))
            }
            LspRequest::DocumentHighlightRequest(p) => LspRequest::DocumentHighlightRequest(
                (mapper.of_document_highlight_params)(mapper, p),
            ),
            LspRequest::TypeCoverageRequest(p) => {
                LspRequest::TypeCoverageRequest((mapper.of_type_coverage_params)(mapper, p))
            }
            LspRequest::DocumentFormattingRequest(p) => LspRequest::DocumentFormattingRequest(
                (mapper.of_document_formatting_params)(mapper, p),
            ),
            LspRequest::DocumentRangeFormattingRequest(p) => {
                LspRequest::DocumentRangeFormattingRequest((mapper
                    .of_document_range_formatting_params)(
                    mapper, p
                ))
            }
            LspRequest::DocumentOnTypeFormattingRequest(p) => {
                LspRequest::DocumentOnTypeFormattingRequest((mapper
                    .of_document_on_type_formatting_params)(
                    mapper, p
                ))
            }
            LspRequest::ShowMessageRequestRequest(p) => LspRequest::ShowMessageRequestRequest(
                (mapper.of_show_message_request_params)(mapper, p),
            ),
            LspRequest::ShowStatusRequest(p) => {
                LspRequest::ShowStatusRequest((mapper.of_show_status_params)(mapper, p))
            }
            LspRequest::RageRequest => LspRequest::RageRequest,
            LspRequest::PingRequest => LspRequest::PingRequest,
            LspRequest::PrepareRenameRequest(p) => {
                LspRequest::PrepareRenameRequest((mapper.of_prepare_rename_params)(mapper, p))
            }
            LspRequest::RenameRequest(p) => {
                LspRequest::RenameRequest((mapper.of_rename_params)(mapper, p))
            }
            LspRequest::DocumentCodeLensRequest(p) => LspRequest::DocumentCodeLensRequest((mapper
                .of_document_code_lens_params)(
                mapper, p,
            )),
            LspRequest::ExecuteCommandRequest(p) => {
                LspRequest::ExecuteCommandRequest((mapper.of_execute_command_params)(mapper, p))
            }
            LspRequest::ApplyWorkspaceEditRequest(p) => LspRequest::ApplyWorkspaceEditRequest(
                (mapper.of_apply_workspace_edit_params)(mapper, p),
            ),
            LspRequest::WillRenameFilesRequest(p) => {
                LspRequest::WillRenameFilesRequest((mapper.of_will_rename_files_params)(mapper, p))
            }
            LspRequest::AutoCloseJsxRequest(p) => {
                LspRequest::AutoCloseJsxRequest((mapper.of_auto_close_jsx_params)(mapper, p))
            }
            LspRequest::PrepareDocumentPasteRequest(p) => LspRequest::PrepareDocumentPasteRequest(
                (mapper.of_document_paste_prepare_params)(mapper, p),
            ),
            LspRequest::ProvideDocumentPasteRequest(p) => LspRequest::ProvideDocumentPasteRequest(
                (mapper.of_document_paste_provide_params)(mapper, p),
            ),
            LspRequest::LinkedEditingRangeRequest(p) => LspRequest::LinkedEditingRangeRequest(
                (mapper.of_linked_editing_range_params)(mapper, p),
            ),
            LspRequest::RenameFileImportsRequest(p) => LspRequest::RenameFileImportsRequest(
                (mapper.of_rename_file_imports_params)(mapper, p),
            ),
            LspRequest::LLMContextRequest(p) => {
                LspRequest::LLMContextRequest((mapper.of_llm_context_params)(mapper, p))
            }
            LspRequest::UnknownRequest(req, json) => LspRequest::UnknownRequest(req, json),
        }),
        of_location: Box::new(|mapper, loc| {
            let uri = (mapper.of_document_uri)(mapper, loc.uri);
            Location {
                uri,
                range: loc.range,
            }
        }),
        of_publish_diagnostics_params: Box::new(|mapper, params| {
            let uri = (mapper.of_document_uri)(mapper, params.uri);
            let diagnostics = params
                .diagnostics
                .into_iter()
                .map(|d| (mapper.of_diagnostic)(mapper, d))
                .collect();
            PublishDiagnosticsParams {
                uri,
                diagnostics,
                version: params.version,
            }
        }),
        of_rage_result: Box::new(|_mapper, t| t),
        of_prepare_rename_params: Box::new(|mapper, params| {
            (mapper.of_text_document_position_params)(mapper, params)
        }),
        of_prepare_rename_result: Box::new(|mapper, result| {
            result.map(|r| (mapper.of_range)(mapper, r))
        }),
        of_ping_result: Box::new(|_mapper, t| t),
        of_range: Box::new(|_mapper, t| t),
        of_register_capability_params: Box::new(|_mapper, params| params),
        of_rename_params: Box::new(|mapper, params| {
            let text_document_position =
                (mapper.of_text_document_position_params)(mapper, params.text_document_position);
            RenameParams {
                text_document_position,
                new_name: params.new_name,
                work_done_progress_params: params.work_done_progress_params,
            }
        }),
        of_rename_result: Box::new(|mapper, result| (mapper.of_workspace_edit)(mapper, result)),
        of_rename_file_imports_params: Box::new(|mapper, params| {
            let old_uri = (mapper.of_document_uri)(mapper, params.old_uri);
            let new_uri = (mapper.of_document_uri)(mapper, params.new_uri);
            rename_files::FileRename { old_uri, new_uri }
        }),
        of_rename_file_imports_result: Box::new(|mapper, result| {
            (mapper.of_workspace_edit)(mapper, result)
        }),
        of_llm_context_params: Box::new(|_mapper, params| params),
        of_llm_context_result: Box::new(|_mapper, result| result),
        of_selection_range_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            SelectionRangeParams {
                text_document,
                positions: params.positions,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_selection_range: Box::new(|mapper, sr| {
            let range = (mapper.of_range)(mapper, sr.range);
            let parent = sr
                .parent
                .map(|p| Box::new((mapper.of_selection_range)(mapper, *p)));
            SelectionRange { range, parent }
        }),
        of_selection_range_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|sr| (mapper.of_selection_range)(mapper, sr))
                .collect()
        }),
        of_signature_help_params: Box::new(|mapper, params| {
            let text_document_position_params = (mapper.of_text_document_position_params)(
                mapper,
                params.text_document_position_params,
            );
            SignatureHelpParams {
                text_document_position_params,
                context: params.context,
                work_done_progress_params: params.work_done_progress_params,
            }
        }),
        of_signature_help_result: Box::new(|_mapper, t| t),
        of_text_document_diagnostics_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            text_document_diagnostics::Params { text_document }
        }),
        of_text_document_diagnostics_result: Box::new(|mapper, result| {
            result
                .into_iter()
                .map(|d| (mapper.of_diagnostic)(mapper, d))
                .collect()
        }),
        of_show_message_params: Box::new(|_mapper, params| params),
        of_show_message_request_params: Box::new(|_mapper, params| params),
        of_show_message_request_result: Box::new(|_mapper, t| t),
        of_show_status_params: Box::new(|_mapper, t| t),
        of_show_status_result: Box::new(|_mapper, t| t),
        #[allow(deprecated)]
        of_symbol_information: Box::new(|mapper, info| {
            let location = (mapper.of_location)(mapper, info.location);
            #[allow(deprecated)]
            SymbolInformation {
                name: info.name,
                kind: info.kind,
                location,
                container_name: info.container_name,
                tags: info.tags,
                deprecated: info.deprecated,
            }
        }),
        of_workspace_symbol_information: Box::new(|mapper, info| {
            let location = (mapper.of_text_document_identifier)(mapper, info.location);
            workspace_symbol_information::T {
                name: info.name,
                kind: info.kind,
                location,
                container_name: info.container_name,
            }
        }),
        of_text_document_identifier: Box::new(|mapper, id| {
            let uri = (mapper.of_document_uri)(mapper, id.uri);
            TextDocumentIdentifier { uri }
        }),
        of_text_document_item: Box::new(|mapper, item| {
            let uri = (mapper.of_document_uri)(mapper, item.uri);
            TextDocumentItem {
                uri,
                language_id: item.language_id,
                version: item.version,
                text: item.text,
            }
        }),
        of_text_document_position_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            TextDocumentPositionParams {
                text_document,
                position: params.position,
            }
        }),
        of_text_edit: Box::new(|mapper, edit| {
            let range = (mapper.of_range)(mapper, edit.range);
            TextEdit {
                range,
                new_text: edit.new_text,
            }
        }),
        of_text_edit_or_insert_replace_edit: Box::new(|mapper, edit| match edit {
            TextEditOrInsertReplaceEdit::TextEdit(e) => {
                TextEditOrInsertReplaceEdit::TextEdit((mapper.of_text_edit)(mapper, e))
            }
            TextEditOrInsertReplaceEdit::InsertReplaceEdit(e) => {
                TextEditOrInsertReplaceEdit::InsertReplaceEdit((mapper.of_insert_replace_edit)(
                    mapper, e,
                ))
            }
        }),
        of_type_coverage_params: Box::new(|mapper, params| {
            let text_document = (mapper.of_text_document_identifier)(mapper, params.text_document);
            type_coverage::Params { text_document }
        }),
        of_type_coverage_result: Box::new(|mapper, result| {
            let uncovered_ranges = result
                .uncovered_ranges
                .into_iter()
                .map(|ur| {
                    let range = (mapper.of_range)(mapper, ur.range);
                    type_coverage::UncoveredRange {
                        range,
                        message: ur.message,
                    }
                })
                .collect();
            type_coverage::Result {
                covered_percent: result.covered_percent,
                uncovered_ranges,
                default_message: result.default_message,
            }
        }),
        of_type_definition_params: Box::new(|mapper, params| {
            let text_document_position_params = (mapper.of_text_document_position_params)(
                mapper,
                params.text_document_position_params,
            );
            GotoDefinitionParams {
                text_document_position_params,
                work_done_progress_params: params.work_done_progress_params,
                partial_result_params: params.partial_result_params,
            }
        }),
        of_type_definition_result: Box::new(|mapper, results| {
            results
                .into_iter()
                .map(|loc| (mapper.of_location)(mapper, loc))
                .collect()
        }),
        of_versioned_text_document_identifier: Box::new(|mapper, id| {
            let uri = (mapper.of_document_uri)(mapper, id.uri);
            VersionedTextDocumentIdentifier {
                uri,
                version: id.version,
            }
        }),
        of_will_rename_files_params: Box::new(|mapper, params| {
            let files = params
                .files
                .into_iter()
                .map(|f| {
                    let old_uri = (mapper.of_document_uri)(mapper, f.old_uri);
                    let new_uri = (mapper.of_document_uri)(mapper, f.new_uri);
                    rename_files::FileRename { old_uri, new_uri }
                })
                .collect();
            will_rename_files::Params { files }
        }),
        of_will_rename_files_result: Box::new(|mapper, result| {
            (mapper.of_workspace_edit)(mapper, result)
        }),
        of_workspace_edit: Box::new(|mapper, edit| {
            let changes = edit.changes.map(|changes| {
                changes
                    .into_iter()
                    .fold(HashMap::new(), |mut acc, (uri, edits)| {
                        let uri = (mapper.of_document_uri)(mapper, uri);
                        let edits = edits
                            .into_iter()
                            .map(|e| (mapper.of_text_edit)(mapper, e))
                            .collect();
                        acc.insert(uri, edits);
                        acc
                    })
            });
            WorkspaceEdit {
                changes,
                document_changes: edit.document_changes,
                change_annotations: edit.change_annotations,
            }
        }),
        of_workspace_symbol_params: Box::new(|_mapper, params| params),
        of_workspace_symbol_result: Box::new(|mapper, result| match result {
            workspace_symbol_result::T::SymbolInformation(infos) => {
                workspace_symbol_result::T::SymbolInformation(
                    infos
                        .into_iter()
                        .map(|info| (mapper.of_symbol_information)(mapper, info))
                        .collect(),
                )
            }
            workspace_symbol_result::T::WorkspaceSymbolInformation(infos) => {
                workspace_symbol_result::T::WorkspaceSymbolInformation(
                    infos
                        .into_iter()
                        .map(|info| (mapper.of_workspace_symbol_information)(mapper, info))
                        .collect(),
                )
            }
        }),
    }
}
