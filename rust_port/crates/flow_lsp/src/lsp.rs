/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc::Position as FlowPosition;
use lsp_types::*;

pub type LspId = NumberOrString;

pub type DocumentUri = Url;

pub type UriMap<V> = std::collections::BTreeMap<Url, V>;

pub fn flow_position_to_lsp(line: i32, char: i32) -> Position {
    Position {
        line: std::cmp::max(0, line - 1) as u32,
        character: char as u32,
    }
}

pub fn lsp_position_to_flow(position: &Position) -> (i32, i32) {
    // Flow's line numbers are 1-indexed; LSP's are 0-indexed
    let line = position.line as i32 + 1;
    let char = position.character as i32;
    (line, char)
}

pub fn lsp_position_to_flow_position(p: &Position) -> FlowPosition {
    let (line, column) = lsp_position_to_flow(p);
    FlowPosition { line, column }
}

pub fn lsp_range_to_flow_loc(source: Option<FileKey>, range: &Range) -> Loc {
    Loc {
        source,
        start: lsp_position_to_flow_position(&range.start),
        end: lsp_position_to_flow_position(&range.end),
    }
}

pub fn loc_to_lsp_range(loc: &Loc) -> Range {
    let loc_start = &loc.start;
    let loc_end = &loc.end;
    let start = flow_position_to_lsp(loc_start.line, loc_start.column);
    let end_ = flow_position_to_lsp(loc_end.line, loc_end.column);
    Range { start, end: end_ }
}

pub mod workspace_symbol_information {
    use lsp_types::SymbolKind;
    use lsp_types::TextDocumentIdentifier;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct T {
        pub name: String,
        pub kind: SymbolKind,
        pub location: TextDocumentIdentifier,
        pub container_name: Option<String>,
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

pub mod ping {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Result {
        pub start_server_status: Option<String>,
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum TextEditOrInsertReplaceEdit {
    TextEdit(TextEdit),
    InsertReplaceEdit(InsertReplaceEdit),
}

pub mod text_document_diagnostics {
    use lsp_types::Diagnostic;
    use lsp_types::TextDocumentIdentifier;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Params {
        pub text_document: TextDocumentIdentifier,
    }

    pub type Result = Vec<Diagnostic>;
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

pub mod type_coverage {
    use lsp_types::Range;
    use lsp_types::TextDocumentIdentifier;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Params {
        pub text_document: TextDocumentIdentifier,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct UncoveredRange {
        pub range: Range,
        #[serde(skip_serializing_if = "Option::is_none")]
        pub message: Option<String>,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Result {
        pub covered_percent: i32,
        pub uncovered_ranges: Vec<UncoveredRange>,
        pub default_message: String,
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

pub mod connection_status {
    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Params {
        pub is_connected: bool,
    }
}

pub mod rename_files {
    use lsp_types::Url;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
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

// ErrorResponse
pub mod error {
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

    pub fn code_to_enum(code: Code) -> i64 {
        match code {
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
        }
    }

    pub fn code_of_enum(code: i64) -> Option<Code> {
        match code {
            -32700 => Some(Code::ParseError),
            -32600 => Some(Code::InvalidRequest),
            -32601 => Some(Code::MethodNotFound),
            -32602 => Some(Code::InvalidParams),
            -32603 => Some(Code::InternalError),
            -32099 => Some(Code::ServerErrorStart),
            -32000 => Some(Code::ServerErrorEnd),
            -32002 => Some(Code::ServerNotInitialized),
            -32001 => Some(Code::UnknownErrorCode),
            -32800 => Some(Code::RequestCancelled),
            -32801 => Some(Code::ContentModified),
            _ => None,
        }
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

pub mod auto_close_jsx {
    pub type Params = lsp_types::TextDocumentPositionParams;
    pub type Result = Option<String>;
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

    #[derive(Debug, Clone)]
    pub struct ImportItem {
        pub remote_name: String,
        pub local_name: Option<String>,
        pub import_type: ImportType,
        pub import_source: String,
        pub import_source_is_resolved: bool,
    }

    impl serde::Serialize for ImportItem {
        fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where
            S: serde::Serializer,
        {
            #[derive(serde::Serialize)]
            #[serde(rename_all = "camelCase")]
            struct Helper<'a> {
                remote_name: &'a str,
                #[serde(skip_serializing_if = "Option::is_none")]
                local_name: Option<&'a str>,
                import_type: &'a ImportType,
                import_source: String,
                import_source_is_resolved: bool,
            }

            let import_source = if self.import_source_is_resolved {
                lsp_types::Url::from_file_path(&self.import_source)
                    .map(|url| url.to_string())
                    .unwrap_or_else(|()| self.import_source.clone())
            } else {
                self.import_source.clone()
            };
            Helper {
                remote_name: &self.remote_name,
                local_name: self.local_name.as_deref(),
                import_type: &self.import_type,
                import_source,
                import_source_is_resolved: self.import_source_is_resolved,
            }
            .serialize(serializer)
        }
    }

    impl<'de> serde::Deserialize<'de> for ImportItem {
        fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de>,
        {
            #[derive(serde::Deserialize)]
            #[serde(rename_all = "camelCase")]
            struct Helper {
                remote_name: String,
                local_name: Option<String>,
                import_type: ImportType,
                import_source: String,
                import_source_is_resolved: bool,
            }
            let h = Helper::deserialize(deserializer)?;
            let import_source = if h.import_source_is_resolved {
                match lsp_types::Url::parse(&h.import_source) {
                    Ok(url) => match url.to_file_path() {
                        Ok(p) => p.to_string_lossy().into_owned(),
                        Err(()) => h.import_source,
                    },
                    Err(_) => h.import_source,
                }
            } else {
                h.import_source
            };
            Ok(ImportItem {
                remote_name: h.remote_name,
                local_name: h.local_name,
                import_type: h.import_type,
                import_source,
                import_source_is_resolved: h.import_source_is_resolved,
            })
        }
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(untagged)]
    pub enum DataTransfer {
        ImportMetadata { imports: Vec<ImportItem> },
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    pub struct PrepareParams {
        pub uri: Url,
        pub ranges: Vec<Range>,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct ProvideParams {
        #[serde(deserialize_with = "deserialize_lenient_text_document_item")]
        pub text_document: TextDocumentItem,
        pub ranges: Vec<Range>,
        pub data_transfer: DataTransfer,
    }

    fn deserialize_lenient_text_document_item<'de, D>(
        deserializer: D,
    ) -> Result<TextDocumentItem, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::Deserialize;
        #[derive(serde::Deserialize)]
        #[serde(rename_all = "camelCase")]
        struct Helper {
            uri: lsp_types::Url,
            #[serde(default)]
            language_id: String,
            #[serde(default)]
            version: i32,
            #[serde(default)]
            text: String,
        }
        let h = Helper::deserialize(deserializer)?;
        Ok(TextDocumentItem {
            uri: h.uri,
            language_id: h.language_id,
            version: h.version,
            text: h.text,
        })
    }
}

pub mod rename_file_imports {
    use lsp_types::WorkspaceEdit;

    pub type Params = super::rename_files::FileRename;
    pub type Result = WorkspaceEdit;
}

pub mod llm_context {
    use lsp_types::WorkspaceFolder;

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct EnvironmentDetails {
        pub workspace_folders: Vec<WorkspaceFolder>,
        pub os: String,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Params {
        pub edited_file_paths: Vec<String>,
        pub environment_details: EnvironmentDetails,
        pub token_budget: i32,
    }

    #[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
    #[serde(rename_all = "camelCase")]
    pub struct Result {
        pub llm_context: String,
        pub files_processed: Vec<String>,
        pub tokens_used: i32,
        pub truncated: bool,
    }
}

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
    RenameResult(WorkspaceEdit),
    DocumentCodeLensResult(Vec<CodeLens>),
    ExecuteCommandResult(()),
    ApplyWorkspaceEditResult(ApplyWorkspaceEditResponse),
    WillRenameFilesResult(will_rename_files::Result),
    RegisterCapabilityResult,
    AutoCloseJsxResult(auto_close_jsx::Result),
    PrepareDocumentPasteResult(document_paste::DataTransfer),
    ProvideDocumentPasteResult(WorkspaceEdit),
    LinkedEditingRangeResult(Option<LinkedEditingRanges>),
    RenameFileImportsResult(rename_file_imports::Result),
    LLMContextResult(llm_context::Result),
    ErrorResult(error::T, String),
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
