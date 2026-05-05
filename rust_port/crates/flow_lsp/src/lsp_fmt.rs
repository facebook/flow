/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use lsp_types::CancelParams;
use lsp_types::Diagnostic;
use lsp_types::LogMessageParams;
use lsp_types::MessageType;
use lsp_types::NumberOrString;
use lsp_types::PublishDiagnosticsParams;
use lsp_types::ShowMessageParams;
use lsp_types::ShowMessageRequestParams;

use crate::lsp;
use crate::lsp::LspId;
use crate::lsp::connection_status;
use crate::lsp::show_status;

fn parse_error_exception(message: impl Into<String>) -> lsp::error::T {
    lsp::error::T {
        code: lsp::error::Code::ParseError,
        message: message.into(),
        data: None,
    }
}

// *********************************************************************
// Miscellaneous LSP structures
// *********************************************************************
pub fn parse_id(id: &serde_json::Value) -> Result<LspId, lsp::error::T> {
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

pub fn print_id(id: &LspId) -> serde_json::Value {
    serde_json::to_value(id).expect("LSP id should serialize to JSON")
}

pub fn id_to_string(id: &LspId) -> String {
    match id {
        NumberOrString::Number(n) => n.to_string(),
        NumberOrString::String(s) => format!("\"{}\"", s),
    }
}

fn print_range(range: &lsp_types::Range) -> serde_json::Value {
    serde_json::to_value(range).expect("Range should serialize to JSON")
}

fn print_workspace_edit(r: &lsp_types::WorkspaceEdit) -> serde_json::Value {
    serde_json::to_value(r).expect("WorkspaceEdit should serialize to JSON")
}

fn print_command_name(key: &str, name: &str) -> String {
    format!("{}:{}", name, key)
}

fn print_command(key: &str, command: &lsp_types::Command) -> serde_json::Value {
    let mut command = command.clone();
    command.command = print_command_name(key, &command.command);
    if command.arguments.is_none() {
        command.arguments = Some(vec![]);
    }
    serde_json::to_value(command).expect("Command should serialize to JSON")
}

// Command names have to be globally unique, so [print_command_name] appends a
// `key`, delimited by a colon. This is the inverse of that.
fn parse_command_name(name: &str) -> String {
    match name.split_once(':') {
        Some((name, _)) => name.to_string(),
        None => name.to_string(),
    }
}

fn print_symbol_information(info: &lsp_types::SymbolInformation) -> serde_json::Value {
    serde_json::to_value(info).expect("SymbolInformation should serialize to JSON")
}

fn print_workspace_symbol_information(
    info: &lsp::workspace_symbol_information::T,
) -> serde_json::Value {
    let mut value =
        serde_json::to_value(info).expect("WorkspaceSymbolInformation should serialize to JSON");
    if let Some(obj) = value.as_object_mut() {
        if obj.get("containerName") == Some(&serde_json::Value::Null) {
            obj.remove("containerName");
        }
    }
    value
}

fn print_code_lens(key: &str, code_lens: &lsp_types::CodeLens) -> serde_json::Value {
    let mut code_lens = code_lens.clone();
    code_lens.command = code_lens.command.map(|mut command| {
        command.command = print_command_name(key, &command.command);
        if command.arguments.is_none() {
            command.arguments = Some(vec![]);
        }
        command
    });
    if code_lens.data.is_none() {
        code_lens.data = Some(serde_json::Value::Null);
    }
    serde_json::to_value(code_lens).expect("CodeLens should serialize to JSON")
}

// *********************************************************************
// shutdown request
// *********************************************************************
fn print_shutdown() -> serde_json::Value {
    serde_json::Value::Null
}

// *********************************************************************
// $/cancelRequest notification
// *********************************************************************
fn print_cancel_request(p: &CancelParams) -> serde_json::Value {
    serde_json::to_value(p).expect("CancelParams should serialize to JSON")
}

// *********************************************************************
// rage request
// *********************************************************************
fn print_rage(r: &[lsp::rage::RageItem]) -> serde_json::Value {
    serde_json::to_value(r).expect("Rage result should serialize to JSON")
}

// *********************************************************************
// ping request
// *********************************************************************
fn print_ping(r: &lsp::ping::Result) -> serde_json::Value {
    serde_json::to_value(r).expect("Ping result should serialize to JSON")
}

mod selection_range_fmt {
    pub fn json_of_result(r: &[lsp_types::SelectionRange]) -> serde_json::Value {
        serde_json::to_value(r).expect("SelectionRange result should serialize to JSON")
    }
}

// textDocument/signatureHelp notification
mod signature_help_fmt {
    pub fn to_json(r: &Option<lsp_types::SignatureHelp>) -> serde_json::Value {
        match r {
            None => serde_json::Value::Null,
            Some(r) => {
                let mut r = r.clone();
                r.active_signature = Some(r.active_signature.unwrap_or(0));
                r.active_parameter = Some(r.active_parameter.unwrap_or(0));
                serde_json::to_value(r).expect("SignatureHelp should serialize to JSON")
            }
        }
    }
}

// codeLens/resolve Request
mod code_lens_resolve_fmt {
    use super::*;
    pub fn json_of_result(key: &str, r: &lsp_types::CodeLens) -> serde_json::Value {
        print_code_lens(key, r)
    }
}

// textDocument/prepareRename Request
mod prepare_rename_fmt {
    use super::*;
    pub fn json_of_result(r: &Option<lsp_types::Range>) -> serde_json::Value {
        match r {
            None => serde_json::Value::Null,
            Some(range) => print_range(range),
        }
    }
}

// textDocument/rename Request
mod rename_fmt {
    use super::*;
    pub fn json_of_result(r: &lsp_types::WorkspaceEdit) -> serde_json::Value {
        print_workspace_edit(r)
    }
}

// textDocument/codeLens Request
mod document_code_lens_fmt {
    use super::*;
    pub fn json_of_result(key: &str, r: &[lsp_types::CodeLens]) -> serde_json::Value {
        let r: Vec<serde_json::Value> = r.iter().map(|cl| print_code_lens(key, cl)).collect();
        serde_json::to_value(r).expect("DocumentCodeLens result should serialize to JSON")
    }
}

// workspace/executeCommand Request
mod execute_command_fmt {
    pub fn json_of_result() -> serde_json::Value {
        serde_json::Value::Null
    }
}

// workspace/applyEdit server -> client request
mod apply_workspace_edit_fmt {
    pub fn json_of_params(r: &lsp_types::ApplyWorkspaceEditParams) -> serde_json::Value {
        serde_json::to_value(r).expect("ApplyWorkspaceEditParams should serialize to JSON")
    }

    pub fn json_of_result(r: &lsp_types::ApplyWorkspaceEditResponse) -> serde_json::Value {
        serde_json::to_value(r).expect("ApplyWorkspaceEditResponse should serialize to JSON")
    }
}

// *********************************************************************
// textDocument/publishDiagnostics notification
// *********************************************************************
pub fn print_diagnostics(r: &PublishDiagnosticsParams) -> serde_json::Value {
    let mut r = r.clone();
    for diagnostic in &mut r.diagnostics {
        if diagnostic.related_information.is_none() {
            diagnostic.related_information = Some(vec![]);
        }
    }
    serde_json::to_value(r).expect("PublishDiagnosticsParams should serialize to JSON")
}

// *********************************************************************
// textDocument/CodeAction result
// *********************************************************************
fn print_code_action(key: &str, c: &lsp_types::CodeAction) -> serde_json::Value {
    let mut c = c.clone();
    if c.diagnostics.is_none() {
        c.diagnostics = Some(vec![]);
    }
    if let Some(diagnostics) = &mut c.diagnostics {
        for diagnostic in diagnostics {
            if diagnostic.related_information.is_none() {
                diagnostic.related_information = Some(vec![]);
            }
        }
    }
    c.command = c.command.map(|mut command| {
        command.command = print_command_name(key, &command.command);
        command
    });
    serde_json::to_value(c).expect("CodeAction should serialize to JSON")
}

pub fn print_code_action_result(
    key: &str,
    result: &[lsp_types::CodeActionOrCommand],
) -> serde_json::Value {
    fn print_command_or_action(
        key: &str,
        item: &lsp_types::CodeActionOrCommand,
    ) -> serde_json::Value {
        match item {
            lsp_types::CodeActionOrCommand::Command(command) => print_command(key, command),
            lsp_types::CodeActionOrCommand::CodeAction(action) => print_code_action(key, action),
        }
    }
    let result: Vec<serde_json::Value> = result
        .iter()
        .map(|item| print_command_or_action(key, item))
        .collect();
    serde_json::to_value(result).expect("CodeAction result should serialize to JSON")
}

// *********************************************************************
// window/logMessage notification
// *********************************************************************
pub fn print_log_message(type_: MessageType, message: &str) -> serde_json::Value {
    let r = LogMessageParams {
        typ: type_,
        message: message.to_string(),
    };
    serde_json::to_value(r).expect("LogMessageParams should serialize to JSON")
}

// *********************************************************************
// window/showMessage notification
// *********************************************************************
fn print_show_message(type_: MessageType, message: &str) -> serde_json::Value {
    let r = ShowMessageParams {
        typ: type_,
        message: message.to_string(),
    };
    serde_json::to_value(r).expect("ShowMessageParams should serialize to JSON")
}

// *********************************************************************
// window/showMessage request
// *********************************************************************
fn print_show_message_request(r: &ShowMessageRequestParams) -> serde_json::Value {
    let mut r = r.clone();
    if r.actions.is_none() {
        r.actions = Some(vec![]);
    }
    serde_json::to_value(r).expect("ShowMessageRequestParams should serialize to JSON")
}

// *********************************************************************
// window/showStatus request
// *********************************************************************
fn print_show_status(r: &show_status::Params) -> serde_json::Value {
    #[derive(serde::Serialize)]
    struct Action<'a> {
        title: &'a str,
    }

    #[derive(serde::Serialize)]
    struct Progress {
        numerator: i32,
        #[serde(skip_serializing_if = "Option::is_none")]
        denominator: Option<i32>,
    }

    #[derive(serde::Serialize)]
    #[serde(rename_all = "camelCase")]
    struct Params<'a> {
        #[serde(rename = "type")]
        typ: MessageType,
        actions: Vec<Action<'a>>,
        message: &'a str,
        #[serde(skip_serializing_if = "Option::is_none")]
        short_message: Option<&'a str>,
        #[serde(skip_serializing_if = "Option::is_none")]
        progress: Option<Progress>,
        #[serde(skip_serializing_if = "Option::is_none")]
        background_color: Option<&'static str>,
    }

    let show_status::Params {
        request: rr,
        progress,
        total,
        short_message,
        background_color,
    } = r;
    let actions = rr
        .actions
        .as_deref()
        .unwrap_or(&[])
        .iter()
        .map(|action| Action {
            title: &action.title,
        })
        .collect();
    let progress = progress.map(|progress| Progress {
        numerator: progress,
        denominator: *total,
    });
    let background_color = background_color.as_ref().map(|bc| match bc {
        show_status::ShowStatusBackgroundColor::Error => "error",
        show_status::ShowStatusBackgroundColor::Warning => "warning",
    });
    serde_json::to_value(Params {
        typ: rr.typ,
        actions,
        message: &rr.message,
        short_message: short_message.as_deref(),
        progress,
        background_color,
    })
    .expect("ShowStatus params should serialize to JSON")
}

// *********************************************************************
// telemetry/connectionStatus notification
// *********************************************************************
pub fn print_connection_status(p: &connection_status::Params) -> serde_json::Value {
    serde_json::to_value(p).expect("ConnectionStatus params should serialize to JSON")
}

// *********************************************************************
// textDocument/hover request
// *********************************************************************
fn print_hover(r: &Option<lsp_types::Hover>) -> serde_json::Value {
    match r {
        None => serde_json::Value::Null,
        Some(r) => {
            let contents = match &r.contents {
                lsp_types::HoverContents::Scalar(item) => vec![item.clone()],
                lsp_types::HoverContents::Array(items) => items.clone(),
                lsp_types::HoverContents::Markup(mc) => {
                    vec![lsp_types::MarkedString::String(mc.value.clone())]
                }
            };
            let r = lsp_types::Hover {
                contents: lsp_types::HoverContents::Array(contents),
                range: r.range,
            };
            serde_json::to_value(r).expect("Hover should serialize to JSON")
        }
    }
}

// *********************************************************************
// textDocument/definition request
// *********************************************************************
mod definition_fmt {
    use super::*;
    pub fn json_of_result(r: &[lsp_types::Location]) -> serde_json::Value {
        print_locations(r)
    }
}

fn string_of_marked_string(acc: String, marked: &lsp_types::MarkedString) -> String {
    match marked {
        lsp_types::MarkedString::LanguageString(ls) => {
            format!("{}```{}\n{}\n```\n", acc, ls.language, ls.value)
        }
        lsp_types::MarkedString::String(s) => format!("{}{}\n", acc, s),
    }
}

pub mod completion_item_fmt {
    use super::*;
    pub fn to_json(key: &str, item: &lsp_types::CompletionItem) -> serde_json::Value {
        let mut item = item.clone();
        item.documentation = item.documentation.map(|doc| {
            let value = match doc {
                lsp_types::Documentation::String(s) => {
                    string_of_marked_string(String::new(), &lsp_types::MarkedString::String(s))
                }
                lsp_types::Documentation::MarkupContent(mc) => string_of_marked_string(
                    String::new(),
                    &lsp_types::MarkedString::String(mc.value),
                ),
            };
            lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
                kind: lsp_types::MarkupKind::Markdown,
                value: value.trim().to_string(),
            })
        });
        item.preselect = if item.preselect.unwrap_or(false) {
            Some(true)
        } else {
            None
        };
        if matches!(item.additional_text_edits.as_deref(), Some([])) {
            item.additional_text_edits = None;
        }
        item.command = item.command.map(|mut command| {
            command.command = print_command_name(key, &command.command);
            command
        });
        serde_json::to_value(item).expect("CompletionItem should serialize to JSON")
    }
}

// *********************************************************************
// textDocument/completion request
// *********************************************************************
mod completion_fmt {
    use super::*;
    pub fn json_of_result(key: &str, r: &lsp_types::CompletionResponse) -> serde_json::Value {
        #[derive(serde::Serialize)]
        #[serde(rename_all = "camelCase")]
        struct CompletionResult {
            is_incomplete: bool,
            items: Vec<serde_json::Value>,
        }

        match r {
            lsp_types::CompletionResponse::Array(items) => serde_json::to_value(CompletionResult {
                is_incomplete: false,
                items: items
                    .iter()
                    .map(|item| completion_item_fmt::to_json(key, item))
                    .collect(),
            })
            .expect("Completion result should serialize to JSON"),
            lsp_types::CompletionResponse::List(list) => serde_json::to_value(CompletionResult {
                is_incomplete: list.is_incomplete,
                items: list
                    .items
                    .iter()
                    .map(|item| completion_item_fmt::to_json(key, item))
                    .collect(),
            })
            .expect("Completion result should serialize to JSON"),
        }
    }
}

// workspace/configuration request
mod configuration_fmt {
    pub fn json_of_params(r: &lsp_types::ConfigurationParams) -> serde_json::Value {
        serde_json::to_value(r).expect("ConfigurationParams should serialize to JSON")
    }

    pub fn json_of_result(r: &[serde_json::Value]) -> serde_json::Value {
        serde_json::to_value(r).expect("Configuration result should serialize to JSON")
    }
}

// workspace/symbol request
mod workspace_symbol_fmt {
    use super::*;
    pub fn json_of_result(r: &lsp::workspace_symbol_result::T) -> serde_json::Value {
        match r {
            lsp::workspace_symbol_result::T::SymbolInformation(v) => {
                let v: Vec<serde_json::Value> = v.iter().map(print_symbol_information).collect();
                serde_json::to_value(v).expect("WorkspaceSymbol result should serialize to JSON")
            }
            lsp::workspace_symbol_result::T::WorkspaceSymbolInformation(v) => {
                let v: Vec<serde_json::Value> =
                    v.iter().map(print_workspace_symbol_information).collect();
                serde_json::to_value(v).expect("WorkspaceSymbol result should serialize to JSON")
            }
        }
    }
}

// textDocument/documentSymbol request
mod document_symbol_fmt {
    use super::*;
    #[allow(deprecated)]
    fn to_json(t: &lsp_types::DocumentSymbol) -> serde_json::Value {
        serde_json::to_value(t).expect("DocumentSymbol should serialize to JSON")
    }

    pub fn json_of_result(r: &lsp::document_symbol_result::T) -> serde_json::Value {
        match r {
            lsp::document_symbol_result::T::SymbolInformation(v) => {
                let v: Vec<serde_json::Value> = v.iter().map(print_symbol_information).collect();
                serde_json::to_value(v).expect("DocumentSymbol result should serialize to JSON")
            }
            lsp::document_symbol_result::T::DocumentSymbol(v) => {
                let v: Vec<serde_json::Value> = v.iter().map(to_json).collect();
                serde_json::to_value(v).expect("DocumentSymbol result should serialize to JSON")
            }
        }
    }
}

// textDocument/documentSymbol request
mod document_diagnostics_fmt {
    use super::*;
    pub fn json_of_result(r: &[Diagnostic]) -> serde_json::Value {
        #[derive(serde::Serialize)]
        struct Result {
            kind: &'static str,
            items: Vec<Diagnostic>,
        }

        let items = r
            .iter()
            .cloned()
            .map(|mut diagnostic| {
                if diagnostic.related_information.is_none() {
                    diagnostic.related_information = Some(vec![]);
                }
                diagnostic
            })
            .collect();
        serde_json::to_value(Result {
            kind: "full",
            items,
        })
        .expect("DocumentDiagnostics result should serialize to JSON")
    }
}

// *********************************************************************
// shared by textDocument/references and textDocument/implementation
// *********************************************************************
fn print_locations(r: &[lsp_types::Location]) -> serde_json::Value {
    serde_json::to_value(r).expect("Locations should serialize to JSON")
}

// *********************************************************************
// textDocument/documentHighlight request
// *********************************************************************
fn print_document_highlight(r: &[lsp_types::DocumentHighlight]) -> serde_json::Value {
    serde_json::to_value(r).expect("DocumentHighlight result should serialize to JSON")
}

// *********************************************************************
// textDocument/typeCoverage request
// *********************************************************************
fn print_type_coverage(r: &lsp::type_coverage::Result) -> serde_json::Value {
    serde_json::to_value(r).expect("TypeCoverage result should serialize to JSON")
}

// *********************************************************************
// textDocument/formatting request
// *********************************************************************
fn print_document_formatting(r: &[lsp_types::TextEdit]) -> serde_json::Value {
    serde_json::to_value(r).expect("DocumentFormatting result should serialize to JSON")
}

// *********************************************************************
// textDocument/rangeFormatting request
// *********************************************************************
fn print_document_range_formatting(r: &[lsp_types::TextEdit]) -> serde_json::Value {
    serde_json::to_value(r).expect("DocumentRangeFormatting result should serialize to JSON")
}

// *********************************************************************
// textDocument/onTypeFormatting request
// *********************************************************************
fn print_document_on_type_formatting(r: &[lsp_types::TextEdit]) -> serde_json::Value {
    serde_json::to_value(r).expect("DocumentOnTypeFormatting result should serialize to JSON")
}

// *********************************************************************
// initialize request
// *********************************************************************
fn print_initialize(key: &str, r: lsp_types::InitializeResult) -> serde_json::Value {
    let mut r = r;
    if let Some(execute_command_provider) = r.capabilities.execute_command_provider.as_mut() {
        execute_command_provider.commands = execute_command_provider
            .commands
            .iter()
            .map(|command| print_command_name(key, command))
            .collect();
    }
    let mut json = serde_json::to_value(&r).expect("Initialize.result should serialize to JSON");
    if let Some(capabilities) = json.get_mut("capabilities").and_then(|v| v.as_object_mut()) {
        capabilities.insert(
            "typeCoverageProvider".to_string(),
            serde_json::to_value(true).expect("bool should serialize to JSON"),
        );
        capabilities.insert(
            "rageProvider".to_string(),
            serde_json::to_value(true).expect("bool should serialize to JSON"),
        );
    }
    json
}

// workspace/willRenameFiles Request
mod will_rename_files_fmt {
    use super::*;
    pub fn json_of_result(r: &lsp_types::WorkspaceEdit) -> serde_json::Value {
        print_workspace_edit(r)
    }
}

// capabilities
mod register_capability_fmt {
    use super::*;
    fn json_of_options(
        register_options: &lsp::register_capability::Options,
    ) -> Option<&lsp_types::DidChangeWatchedFilesRegistrationOptions> {
        match register_options {
            lsp::register_capability::Options::DidChangeConfiguration => None,
            lsp::register_capability::Options::DidChangeWatchedFiles(register_options) => {
                Some(register_options)
            }
        }
    }

    pub fn json_of_params(params: &lsp::register_capability::Params) -> serde_json::Value {
        #[derive(serde::Serialize)]
        #[serde(rename_all = "camelCase")]
        struct Registration<'a> {
            id: &'a str,
            method: &'a str,
            #[serde(skip_serializing_if = "Option::is_none")]
            register_options: Option<&'a lsp_types::DidChangeWatchedFilesRegistrationOptions>,
        }

        #[derive(serde::Serialize)]
        struct Params<'a> {
            registrations: Vec<Registration<'a>>,
        }

        let registrations = params
            .registrations
            .iter()
            .map(|registration| Registration {
                id: &registration.id,
                method: &registration.method,
                register_options: json_of_options(&registration.register_options),
            })
            .collect();
        serde_json::to_value(Params { registrations })
            .expect("registerCapability params should serialize to JSON")
    }
}

mod auto_close_jsx_fmt {
    pub fn json_of_result(r: &Option<String>) -> serde_json::Value {
        serde_json::to_value(r).expect("AutoCloseJsx result should serialize to JSON")
    }
}

mod document_paste_fmt {
    use super::*;
    pub fn json_of_data_transfer(r: &lsp::document_paste::DataTransfer) -> serde_json::Value {
        serde_json::to_value(r).expect("DocumentPaste data transfer should serialize to JSON")
    }
}

mod linked_editing_range_fmt {
    pub fn json_of_result(r: &Option<lsp_types::LinkedEditingRanges>) -> serde_json::Value {
        match r {
            None => serde_json::Value::Null,
            Some(r) => {
                serde_json::to_value(r).expect("LinkedEditingRanges should serialize to JSON")
            }
        }
    }
}

mod rename_file_imports_fmt {
    use super::*;
    pub fn json_of_result(r: &lsp_types::WorkspaceEdit) -> serde_json::Value {
        print_workspace_edit(r)
    }
}

mod llm_context_fmt {
    use super::*;
    pub fn json_of_result(r: &lsp::llm_context::Result) -> serde_json::Value {
        serde_json::to_value(r).expect("LLMContext result should serialize to JSON")
    }
}

// *********************************************************************
// error response
// *********************************************************************
pub fn error_of_exn(e: &dyn std::error::Error) -> lsp::error::T {
    lsp::error::T {
        code: lsp::error::Code::InternalError,
        message: e.to_string(),
        data: None,
    }
}

pub fn print_error(
    include_error_stack_trace: bool,
    error: &lsp::error::T,
    stack: &str,
) -> serde_json::Value {
    #[derive(serde::Serialize)]
    struct Stack<'a> {
        stack: &'a str,
    }

    #[derive(serde::Serialize)]
    struct ErrorResponse<'a> {
        code: i64,
        message: &'a str,
        #[serde(skip_serializing_if = "Option::is_none")]
        data: Option<serde_json::Value>,
    }

    let code = lsp::error::code_to_enum(error.code);
    // We'd like to add a stack-trace. The only place we can fit it, that will
    // be respected by vscode-jsonrpc, is inside the 'data' field. And we can
    // do that only if data is an object. We can synthesize one if needed.
    let data = match (&error.data, include_error_stack_trace, stack) {
        (None, true, s) => {
            Some(serde_json::to_value(Stack { stack: s }).expect("stack should serialize to JSON"))
        }
        (Some(serde_json::Value::Object(m)), true, s) => {
            let mut merged = serde_json::Map::new();
            merged.insert(
                "stack".to_string(),
                serde_json::Value::String(s.to_string()),
            );
            for (k, v) in m {
                merged.insert(k.clone(), v.clone());
            }
            Some(serde_json::Value::Object(merged))
        }
        (Some(d), _, _) => Some(d.clone()),
        _ => None,
    };
    serde_json::to_value(ErrorResponse {
        code,
        message: &error.message,
        data,
    })
    .expect("Error response should serialize to JSON")
}

pub fn parse_error(error: &serde_json::Value) -> lsp::error::T {
    use lsp::error::Code;
    let code = error
        .get("code")
        .and_then(|v| v.as_i64())
        .and_then(lsp::error::code_of_enum)
        .unwrap_or(Code::UnknownErrorCode);
    let message = error
        .get("message")
        .and_then(|v| v.as_str())
        .unwrap_or("")
        .to_string();
    let data = error.get("data").cloned();
    lsp::error::T {
        code,
        message,
        data,
    }
}

// *********************************************************************
// universal parser+printer
// *********************************************************************
pub fn request_name_to_string(request: &lsp::LspRequest) -> String {
    match request {
        lsp::LspRequest::ShowMessageRequestRequest(_) => "window/showMessageRequest".to_string(),
        lsp::LspRequest::ShowStatusRequest(_) => "window/showStatus".to_string(),
        lsp::LspRequest::InitializeRequest(_) => "initialize".to_string(),
        lsp::LspRequest::RegisterCapabilityRequest(_) => "client/registerCapability".to_string(),
        lsp::LspRequest::ShutdownRequest => "shutdown".to_string(),
        lsp::LspRequest::CodeLensResolveRequest(_) => "codeLens/resolve".to_string(),
        lsp::LspRequest::HoverRequest(_) => "textDocument/hover".to_string(),
        lsp::LspRequest::CodeActionRequest(_) => "textDocument/codeAction".to_string(),
        lsp::LspRequest::CompletionRequest(_) => "textDocument/completion".to_string(),
        lsp::LspRequest::CompletionItemResolveRequest(_) => "completionItem/resolve".to_string(),
        lsp::LspRequest::ConfigurationRequest(_) => "workspace/configuration".to_string(),
        lsp::LspRequest::SelectionRangeRequest(_) => "textDocument/selectionRange".to_string(),
        lsp::LspRequest::SignatureHelpRequest(_) => "textDocument/signatureHelp".to_string(),
        lsp::LspRequest::TextDocumentDiagnosticsRequest(_) => {
            "textDocument/diagnostics".to_string()
        }
        lsp::LspRequest::DefinitionRequest(_) => "textDocument/definition".to_string(),
        lsp::LspRequest::TypeDefinitionRequest(_) => "textDocument/typeDefinition".to_string(),
        lsp::LspRequest::WorkspaceSymbolRequest(_) => "workspace/symbol".to_string(),
        lsp::LspRequest::DocumentSymbolRequest(_) => "textDocument/documentSymbol".to_string(),
        lsp::LspRequest::FindReferencesRequest(_) => "textDocument/references".to_string(),
        lsp::LspRequest::DocumentHighlightRequest(_) => {
            "textDocument/documentHighlight".to_string()
        }
        lsp::LspRequest::TypeCoverageRequest(_) => "textDocument/typeCoverage".to_string(),
        lsp::LspRequest::DocumentFormattingRequest(_) => "textDocument/formatting".to_string(),
        lsp::LspRequest::DocumentRangeFormattingRequest(_) => {
            "textDocument/rangeFormatting".to_string()
        }
        lsp::LspRequest::DocumentOnTypeFormattingRequest(_) => {
            "textDocument/onTypeFormatting".to_string()
        }
        lsp::LspRequest::RageRequest => "telemetry/rage".to_string(),
        lsp::LspRequest::PingRequest => "telemetry/ping".to_string(),
        lsp::LspRequest::PrepareRenameRequest(_) => "textDocument/prepareRename".to_string(),
        lsp::LspRequest::RenameRequest(_) => "textDocument/rename".to_string(),
        lsp::LspRequest::DocumentCodeLensRequest(_) => "textDocument/codeLens".to_string(),
        lsp::LspRequest::ExecuteCommandRequest(_) => "workspace/executeCommand".to_string(),
        lsp::LspRequest::ApplyWorkspaceEditRequest(_) => "workspace/applyEdit".to_string(),
        lsp::LspRequest::AutoCloseJsxRequest(_) => "flow/autoCloseJsx".to_string(),
        lsp::LspRequest::PrepareDocumentPasteRequest(_) => "flow/prepareDocumentPaste".to_string(),
        lsp::LspRequest::ProvideDocumentPasteRequest(_) => {
            "flow/provideDocumentPasteEdits".to_string()
        }
        lsp::LspRequest::LinkedEditingRangeRequest(_) => {
            "textDocument/linkedEditingRange".to_string()
        }
        lsp::LspRequest::WillRenameFilesRequest(_) => "workspace/willRenameFiles".to_string(),
        lsp::LspRequest::RenameFileImportsRequest(_) => "flow/renameFileImports".to_string(),
        lsp::LspRequest::LLMContextRequest(_) => "llm/context".to_string(),
        lsp::LspRequest::UnknownRequest(method_, _params) => method_.clone(),
    }
}

pub fn result_name_to_string(result: &lsp::LspResult) -> String {
    match result {
        lsp::LspResult::ShowMessageRequestResult(_) => "window/showMessageRequest".to_string(),
        lsp::LspResult::ShowStatusResult(_) => "window/showStatus".to_string(),
        lsp::LspResult::InitializeResult(_) => "initialize".to_string(),
        lsp::LspResult::ShutdownResult => "shutdown".to_string(),
        lsp::LspResult::CodeLensResolveResult(_) => "codeLens/resolve".to_string(),
        lsp::LspResult::HoverResult(_) => "textDocument/hover".to_string(),
        lsp::LspResult::CodeActionResult(_) => "textDocument/codeAction".to_string(),
        lsp::LspResult::CompletionResult(_) => "textDocument/completion".to_string(),
        lsp::LspResult::CompletionItemResolveResult(_) => "completionItem/resolve".to_string(),
        lsp::LspResult::ConfigurationResult(_) => "workspace/configuration".to_string(),
        lsp::LspResult::SelectionRangeResult(_) => "textDocument/selectionRange".to_string(),
        lsp::LspResult::SignatureHelpResult(_) => "textDocument/signatureHelp".to_string(),
        lsp::LspResult::TextDocumentDiagnosticsResult(_) => "textDocument/diagnostics".to_string(),
        lsp::LspResult::DefinitionResult(_) => "textDocument/definition".to_string(),
        lsp::LspResult::TypeDefinitionResult(_) => "textDocument/typeDefinition".to_string(),
        lsp::LspResult::WorkspaceSymbolResult(_) => "workspace/symbol".to_string(),
        lsp::LspResult::DocumentSymbolResult(_) => "textDocument/documentSymbol".to_string(),
        lsp::LspResult::FindReferencesResult(_) => "textDocument/references".to_string(),
        lsp::LspResult::GoToImplementationResult(_) => "textDocument/implementation".to_string(),
        lsp::LspResult::DocumentHighlightResult(_) => "textDocument/documentHighlight".to_string(),
        lsp::LspResult::TypeCoverageResult(_) => "textDocument/typeCoverage".to_string(),
        lsp::LspResult::DocumentFormattingResult(_) => "textDocument/formatting".to_string(),
        lsp::LspResult::DocumentRangeFormattingResult(_) => {
            "textDocument/rangeFormatting".to_string()
        }
        lsp::LspResult::DocumentOnTypeFormattingResult(_) => {
            "textDocument/onTypeFormatting".to_string()
        }
        lsp::LspResult::RageResult(_) => "telemetry/rage".to_string(),
        lsp::LspResult::PingResult(_) => "telemetry/ping".to_string(),
        lsp::LspResult::PrepareRenameResult(_) => "textDocument/prepareRename".to_string(),
        lsp::LspResult::RenameResult(_) => "textDocument/rename".to_string(),
        lsp::LspResult::DocumentCodeLensResult(_) => "textDocument/codeLens".to_string(),
        lsp::LspResult::ExecuteCommandResult(_) => "workspace/executeCommand".to_string(),
        lsp::LspResult::WillRenameFilesResult(_) => "workspace/willRenameFiles".to_string(),
        lsp::LspResult::ApplyWorkspaceEditResult(_) => "workspace/applyEdit".to_string(),
        lsp::LspResult::RegisterCapabilityResult => "client/registerCapability".to_string(),
        lsp::LspResult::AutoCloseJsxResult(_) => "flow/autoCloseJsx".to_string(),
        lsp::LspResult::PrepareDocumentPasteResult(_) => "flow/prepareDocumentPaste".to_string(),
        lsp::LspResult::ProvideDocumentPasteResult(_) => {
            "flow/provideDocumentPasteEdits".to_string()
        }
        lsp::LspResult::LinkedEditingRangeResult(_) => {
            "textDocument/linkedEditingRange".to_string()
        }
        lsp::LspResult::RenameFileImportsResult(_) => "flow/renameFileImports".to_string(),
        lsp::LspResult::LLMContextResult(_) => "llm/context".to_string(),
        lsp::LspResult::ErrorResult(e, _stack) => format!("ERROR/{}", e.message),
    }
}

pub fn notification_name_to_string(notification: &lsp::LspNotification) -> String {
    match notification {
        lsp::LspNotification::ExitNotification => "exit".to_string(),
        lsp::LspNotification::CancelRequestNotification(_) => "$/cancelRequest".to_string(),
        lsp::LspNotification::PublishDiagnosticsNotification(_) => {
            "textDocument/publishDiagnostics".to_string()
        }
        lsp::LspNotification::DidOpenNotification(_) => "textDocument/didOpen".to_string(),
        lsp::LspNotification::DidCloseNotification(_) => "textDocument/didClose".to_string(),
        lsp::LspNotification::DidSaveNotification(_) => "textDocument/didSave".to_string(),
        lsp::LspNotification::DidChangeNotification(_) => "textDocument/didChange".to_string(),
        lsp::LspNotification::DidChangeConfigurationNotification(_) => {
            "workspace/didChangeConfiguration".to_string()
        }
        lsp::LspNotification::DidChangeWatchedFilesNotification(_) => {
            "workspace/didChangeWatchedFiles".to_string()
        }
        lsp::LspNotification::TelemetryNotification(_) => "telemetry/event".to_string(),
        lsp::LspNotification::LogMessageNotification(_) => "window/logMessage".to_string(),
        lsp::LspNotification::ShowMessageNotification(_) => "window/showMessage".to_string(),
        lsp::LspNotification::ConnectionStatusNotification(_) => {
            "telemetry/connectionStatus".to_string()
        }
        lsp::LspNotification::InitializedNotification => "initialized".to_string(),
        lsp::LspNotification::SetTraceNotification => "$/setTraceNotification".to_string(),
        lsp::LspNotification::LogTraceNotification => "$/logTraceNotification".to_string(),
        lsp::LspNotification::UnknownNotification(method_, _params) => method_.clone(),
    }
}

pub fn message_name_to_string(message: &lsp::LspMessage) -> String {
    match message {
        lsp::LspMessage::RequestMessage(_, r) => request_name_to_string(r),
        lsp::LspMessage::NotificationMessage(n) => notification_name_to_string(n),
        lsp::LspMessage::ResponseMessage(_, r) => result_name_to_string(r),
    }
}

pub fn denorm_message_to_string(msg: &lsp::LspMessage) -> String {
    match msg {
        lsp::LspMessage::RequestMessage(id, req) => {
            format!(
                "request {} {}",
                id_to_string(id),
                request_name_to_string(req)
            )
        }
        lsp::LspMessage::NotificationMessage(
            notif @ lsp::LspNotification::CancelRequestNotification(CancelParams { id }),
        ) => {
            format!(
                "notification {} {}",
                notification_name_to_string(notif),
                id_to_string(id)
            )
        }
        lsp::LspMessage::NotificationMessage(notif) => {
            format!("notification {}", notification_name_to_string(notif))
        }
        lsp::LspMessage::ResponseMessage(id, lsp::LspResult::ErrorResult(e, _stack)) => {
            format!("error {} {}", id_to_string(id), e.message)
        }
        lsp::LspMessage::ResponseMessage(id, result) => {
            format!(
                "result {} {}",
                id_to_string(id),
                result_name_to_string(result)
            )
        }
    }
}

pub fn parse_lsp_request(
    method_: &str,
    params: Option<&serde_json::Value>,
) -> Result<lsp::LspRequest, lsp::error::T> {
    Ok(match method_ {
        "initialize" => {
            let mut initialize_params: lsp_types::InitializeParams =
                serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null))
                    .map_err(|e| {
                        parse_error_exception(format!("Failed to deserialize LSP params: {}", e))
                    })?;
            if let Some(status) = params
                .and_then(|params| params.get("capabilities"))
                .and_then(|capabilities| capabilities.get("window"))
                .and_then(|window| window.get("status"))
                .map(|status| status.is_object())
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
                .map(|connection_status| connection_status.is_object())
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
            lsp::LspRequest::InitializeRequest(initialize_params)
        }
        "shutdown" => lsp::LspRequest::ShutdownRequest,
        "codeLens/resolve" => lsp::LspRequest::CodeLensResolveRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/hover" => lsp::LspRequest::HoverRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/codeAction" => lsp::LspRequest::CodeActionRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/completion" => lsp::LspRequest::CompletionRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/definition" => lsp::LspRequest::DefinitionRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "workspace/symbol" => lsp::LspRequest::WorkspaceSymbolRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/documentSymbol" => lsp::LspRequest::DocumentSymbolRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/references" => {
            let mut params_obj = match params {
                Some(serde_json::Value::Object(map)) => map.clone(),
                _ => serde_json::Map::new(),
            };
            params_obj.entry("context".to_string()).or_insert_with(|| {
                serde_json::json!({
                    "includeDeclaration": true,
                    "includeIndirectReferences": false,
                })
            });
            let synthesized = serde_json::Value::Object(params_obj);
            lsp::LspRequest::FindReferencesRequest(serde_json::from_value(synthesized).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?)
        }
        "textDocument/prepareRename" => lsp::LspRequest::PrepareRenameRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/rename" => lsp::LspRequest::RenameRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/documentHighlight" => lsp::LspRequest::DocumentHighlightRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/formatting" => lsp::LspRequest::DocumentFormattingRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/rangeFormatting" => lsp::LspRequest::DocumentRangeFormattingRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/onTypeFormatting" => lsp::LspRequest::DocumentOnTypeFormattingRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/codeLens" => lsp::LspRequest::DocumentCodeLensRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/selectionRange" => lsp::LspRequest::SelectionRangeRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/signatureHelp" => lsp::LspRequest::SignatureHelpRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/linkedEditingRange" => lsp::LspRequest::LinkedEditingRangeRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/typeCoverage" => lsp::LspRequest::TypeCoverageRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "flow/autoCloseJsx" => lsp::LspRequest::AutoCloseJsxRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "flow/prepareDocumentPaste" => lsp::LspRequest::PrepareDocumentPasteRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "flow/provideDocumentPasteEdits" => lsp::LspRequest::ProvideDocumentPasteRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "flow/renameFileImports" => lsp::LspRequest::RenameFileImportsRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "llm/context" => lsp::LspRequest::LLMContextRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "workspace/willRenameFiles" => lsp::LspRequest::WillRenameFilesRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "telemetry/rage" => lsp::LspRequest::RageRequest,
        "telemetry/ping" => lsp::LspRequest::PingRequest,
        "workspace/executeCommand" => {
            let mut params: lsp_types::ExecuteCommandParams =
                serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null))
                    .map_err(|e| {
                        parse_error_exception(format!("Failed to deserialize LSP params: {}", e))
                    })?;
            params.command = parse_command_name(&params.command);
            lsp::LspRequest::ExecuteCommandRequest(params)
        }
        "textDocument/diagnostics" => lsp::LspRequest::TextDocumentDiagnosticsRequest(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        _ => lsp::LspRequest::UnknownRequest(method_.to_string(), params.cloned()),
    })
}

pub fn parse_lsp_notification(
    method_: &str,
    params: Option<&serde_json::Value>,
) -> Result<lsp::LspNotification, lsp::error::T> {
    Ok(match method_ {
        "$/cancelRequest" => lsp::LspNotification::CancelRequestNotification(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "$/setTraceNotification" => lsp::LspNotification::SetTraceNotification,
        "$/logTraceNotification" => lsp::LspNotification::LogTraceNotification,
        "initialized" => lsp::LspNotification::InitializedNotification,
        "exit" => lsp::LspNotification::ExitNotification,
        "textDocument/didOpen" => lsp::LspNotification::DidOpenNotification(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/didClose" => lsp::LspNotification::DidCloseNotification(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/didSave" => lsp::LspNotification::DidSaveNotification(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "textDocument/didChange" => lsp::LspNotification::DidChangeNotification(
            serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null)).map_err(
                |e| parse_error_exception(format!("Failed to deserialize LSP params: {}", e)),
            )?,
        ),
        "workspace/didChangeConfiguration" => {
            lsp::LspNotification::DidChangeConfigurationNotification(
                serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null))
                    .map_err(|e| {
                        parse_error_exception(format!("Failed to deserialize LSP params: {}", e))
                    })?,
            )
        }
        "workspace/didChangeWatchedFiles" => {
            lsp::LspNotification::DidChangeWatchedFilesNotification(
                serde_json::from_value(params.cloned().unwrap_or(serde_json::Value::Null))
                    .map_err(|e| {
                        parse_error_exception(format!("Failed to deserialize LSP params: {}", e))
                    })?,
            )
        }
        _ => lsp::LspNotification::UnknownNotification(method_.to_string(), params.cloned()),
    })
}

pub fn parse_lsp_result(
    request: &lsp::LspRequest,
    result: &serde_json::Value,
) -> Result<lsp::LspResult, lsp::error::T> {
    let method_ = request_name_to_string(request);
    Ok(match request {
        lsp::LspRequest::ShowMessageRequestRequest(_) => {
            lsp::LspResult::ShowMessageRequestResult(serde_json::from_value(result.clone()).ok())
        }
        lsp::LspRequest::ShowStatusRequest(_) => {
            lsp::LspResult::ShowStatusResult(serde_json::from_value(result.clone()).ok())
        }
        lsp::LspRequest::ApplyWorkspaceEditRequest(_) => lsp::LspResult::ApplyWorkspaceEditResult(
            serde_json::from_value(result.clone()).map_err(|e| {
                parse_error_exception(format!("Failed to deserialize LSP result: {}", e))
            })?,
        ),
        lsp::LspRequest::ConfigurationRequest(_) => {
            let items = match result {
                serde_json::Value::Array(items) => items.clone(),
                _ => vec![],
            };
            lsp::LspResult::ConfigurationResult(items)
        }
        lsp::LspRequest::RegisterCapabilityRequest(_) => lsp::LspResult::RegisterCapabilityResult,
        lsp::LspRequest::InitializeRequest(_)
        | lsp::LspRequest::ShutdownRequest
        | lsp::LspRequest::CodeLensResolveRequest(_)
        | lsp::LspRequest::HoverRequest(_)
        | lsp::LspRequest::CodeActionRequest(_)
        | lsp::LspRequest::CompletionRequest(_)
        | lsp::LspRequest::CompletionItemResolveRequest(_)
        | lsp::LspRequest::SelectionRangeRequest(_)
        | lsp::LspRequest::SignatureHelpRequest(_)
        | lsp::LspRequest::TextDocumentDiagnosticsRequest(_)
        | lsp::LspRequest::DefinitionRequest(_)
        | lsp::LspRequest::TypeDefinitionRequest(_)
        | lsp::LspRequest::WorkspaceSymbolRequest(_)
        | lsp::LspRequest::DocumentSymbolRequest(_)
        | lsp::LspRequest::FindReferencesRequest(_)
        | lsp::LspRequest::DocumentHighlightRequest(_)
        | lsp::LspRequest::TypeCoverageRequest(_)
        | lsp::LspRequest::DocumentFormattingRequest(_)
        | lsp::LspRequest::DocumentRangeFormattingRequest(_)
        | lsp::LspRequest::DocumentOnTypeFormattingRequest(_)
        | lsp::LspRequest::RageRequest
        | lsp::LspRequest::PingRequest
        | lsp::LspRequest::PrepareRenameRequest(_)
        | lsp::LspRequest::RenameRequest(_)
        | lsp::LspRequest::WillRenameFilesRequest(_)
        | lsp::LspRequest::DocumentCodeLensRequest(_)
        | lsp::LspRequest::ExecuteCommandRequest(_)
        | lsp::LspRequest::AutoCloseJsxRequest(_)
        | lsp::LspRequest::PrepareDocumentPasteRequest(_)
        | lsp::LspRequest::ProvideDocumentPasteRequest(_)
        | lsp::LspRequest::LinkedEditingRangeRequest(_)
        | lsp::LspRequest::RenameFileImportsRequest(_)
        | lsp::LspRequest::LLMContextRequest(_)
        | lsp::LspRequest::UnknownRequest(_, _) => {
            return Err(parse_error_exception(format!(
                "Don't know how to parse LSP response {}",
                method_
            )));
        }
    })
}

// parse_lsp: non-jsonrpc inputs - will raise an exception
// requests and notifications - will raise an exception if they're malformed,
// otherwise return Some
// responses - will raise an exception if they're malformed, will return None
// if they're absent from the "outstanding" map, otherwise return Some.
pub fn parse_lsp(
    json: &serde_json::Value,
    outstanding: &dyn Fn(&LspId) -> Result<lsp::LspRequest, lsp::error::T>,
) -> Result<lsp::LspMessage, lsp::error::T> {
    let id = match json.get("id") {
        Some(id) => Some(parse_id(id)?),
        None => None,
    };
    let method_opt = json.get("method").and_then(|v| v.as_str());
    let params = json.get("params");
    let result = json.get("result");
    let error = json.get("error");
    Ok(match (id, method_opt, result, error) {
        (None, Some(method_), _, _) => {
            lsp::LspMessage::NotificationMessage(parse_lsp_notification(method_, params)?)
        }
        (Some(id), Some(method_), _, _) => {
            lsp::LspMessage::RequestMessage(id, parse_lsp_request(method_, params)?)
        }
        (Some(id), _, Some(result), _) => {
            let request = outstanding(&id)?;
            lsp::LspMessage::ResponseMessage(id, parse_lsp_result(&request, result)?)
        }
        (Some(id), _, _, Some(error)) => lsp::LspMessage::ResponseMessage(
            id,
            lsp::LspResult::ErrorResult(parse_error(error), String::new()),
        ),
        _ => return Err(parse_error_exception("Not JsonRPC")),
    })
}

pub fn print_lsp_request(id: &LspId, request: &lsp::LspRequest) -> serde_json::Value {
    let method_ = request_name_to_string(request);
    let params = match request {
        lsp::LspRequest::ShowMessageRequestRequest(r) => print_show_message_request(r),
        lsp::LspRequest::ShowStatusRequest(r) => print_show_status(r),
        lsp::LspRequest::RegisterCapabilityRequest(r) => register_capability_fmt::json_of_params(r),
        lsp::LspRequest::ApplyWorkspaceEditRequest(r) => {
            apply_workspace_edit_fmt::json_of_params(r)
        }
        lsp::LspRequest::ConfigurationRequest(r) => configuration_fmt::json_of_params(r),
        lsp::LspRequest::InitializeRequest(_)
        | lsp::LspRequest::ShutdownRequest
        | lsp::LspRequest::HoverRequest(_)
        | lsp::LspRequest::CodeActionRequest(_)
        | lsp::LspRequest::CodeLensResolveRequest(_)
        | lsp::LspRequest::CompletionRequest(_)
        | lsp::LspRequest::CompletionItemResolveRequest(_)
        | lsp::LspRequest::SelectionRangeRequest(_)
        | lsp::LspRequest::SignatureHelpRequest(_)
        | lsp::LspRequest::TextDocumentDiagnosticsRequest(_)
        | lsp::LspRequest::DefinitionRequest(_)
        | lsp::LspRequest::TypeDefinitionRequest(_)
        | lsp::LspRequest::WorkspaceSymbolRequest(_)
        | lsp::LspRequest::DocumentSymbolRequest(_)
        | lsp::LspRequest::FindReferencesRequest(_)
        | lsp::LspRequest::DocumentHighlightRequest(_)
        | lsp::LspRequest::TypeCoverageRequest(_)
        | lsp::LspRequest::DocumentFormattingRequest(_)
        | lsp::LspRequest::DocumentRangeFormattingRequest(_)
        | lsp::LspRequest::DocumentOnTypeFormattingRequest(_)
        | lsp::LspRequest::RageRequest
        | lsp::LspRequest::PingRequest
        | lsp::LspRequest::PrepareDocumentPasteRequest(_)
        | lsp::LspRequest::PrepareRenameRequest(_)
        | lsp::LspRequest::ProvideDocumentPasteRequest(_)
        | lsp::LspRequest::RenameRequest(_)
        | lsp::LspRequest::WillRenameFilesRequest(_)
        | lsp::LspRequest::DocumentCodeLensRequest(_)
        | lsp::LspRequest::ExecuteCommandRequest(_)
        | lsp::LspRequest::AutoCloseJsxRequest(_)
        | lsp::LspRequest::LinkedEditingRangeRequest(_)
        | lsp::LspRequest::RenameFileImportsRequest(_)
        | lsp::LspRequest::LLMContextRequest(_)
        | lsp::LspRequest::UnknownRequest(_, _) => {
            panic!("Don't know how to print request {}", method_)
        }
    };
    #[derive(serde::Serialize)]
    struct Request {
        jsonrpc: &'static str,
        id: serde_json::Value,
        method: String,
        params: serde_json::Value,
    }
    serde_json::to_value(Request {
        jsonrpc: "2.0",
        id: print_id(id),
        method: method_,
        params,
    })
    .expect("LSP request should serialize to JSON")
}

pub fn print_lsp_response(
    include_error_stack_trace: bool,
    key: &str,
    id: &LspId,
    result: &lsp::LspResult,
) -> serde_json::Value {
    let method_ = result_name_to_string(result);
    let json = match result {
        lsp::LspResult::InitializeResult(r) => print_initialize(key, r.clone()),
        lsp::LspResult::ShutdownResult => print_shutdown(),
        lsp::LspResult::CodeLensResolveResult(r) => code_lens_resolve_fmt::json_of_result(key, r),
        lsp::LspResult::HoverResult(r) => print_hover(r),
        lsp::LspResult::CodeActionResult(r) => print_code_action_result(key, r),
        lsp::LspResult::CompletionResult(r) => completion_fmt::json_of_result(key, r),
        lsp::LspResult::ConfigurationResult(r) => configuration_fmt::json_of_result(r),
        lsp::LspResult::DefinitionResult(r) => definition_fmt::json_of_result(r),
        lsp::LspResult::TypeDefinitionResult(r) => definition_fmt::json_of_result(r),
        lsp::LspResult::WorkspaceSymbolResult(r) => workspace_symbol_fmt::json_of_result(r),
        lsp::LspResult::DocumentSymbolResult(r) => document_symbol_fmt::json_of_result(r),
        lsp::LspResult::FindReferencesResult(r) => print_locations(r),
        lsp::LspResult::GoToImplementationResult(r) => print_locations(r),
        lsp::LspResult::DocumentHighlightResult(r) => print_document_highlight(r),
        lsp::LspResult::TypeCoverageResult(r) => print_type_coverage(r),
        lsp::LspResult::DocumentFormattingResult(r) => print_document_formatting(r),
        lsp::LspResult::DocumentRangeFormattingResult(r) => print_document_range_formatting(r),
        lsp::LspResult::DocumentOnTypeFormattingResult(r) => print_document_on_type_formatting(r),
        lsp::LspResult::RageResult(r) => print_rage(r),
        lsp::LspResult::PingResult(r) => print_ping(r),
        lsp::LspResult::PrepareRenameResult(r) => prepare_rename_fmt::json_of_result(r),
        lsp::LspResult::RenameResult(r) => rename_fmt::json_of_result(r),
        lsp::LspResult::DocumentCodeLensResult(r) => document_code_lens_fmt::json_of_result(key, r),
        lsp::LspResult::ExecuteCommandResult(_) => execute_command_fmt::json_of_result(),
        lsp::LspResult::ApplyWorkspaceEditResult(r) => apply_workspace_edit_fmt::json_of_result(r),
        lsp::LspResult::SelectionRangeResult(r) => selection_range_fmt::json_of_result(r),
        lsp::LspResult::SignatureHelpResult(r) => signature_help_fmt::to_json(r),
        lsp::LspResult::TextDocumentDiagnosticsResult(r) => {
            document_diagnostics_fmt::json_of_result(r)
        }
        lsp::LspResult::WillRenameFilesResult(r) => will_rename_files_fmt::json_of_result(r),
        lsp::LspResult::AutoCloseJsxResult(r) => auto_close_jsx_fmt::json_of_result(r),
        lsp::LspResult::PrepareDocumentPasteResult(r) => {
            document_paste_fmt::json_of_data_transfer(r)
        }
        lsp::LspResult::ProvideDocumentPasteResult(r) => print_workspace_edit(r),
        lsp::LspResult::LinkedEditingRangeResult(r) => linked_editing_range_fmt::json_of_result(r),
        lsp::LspResult::RenameFileImportsResult(r) => rename_file_imports_fmt::json_of_result(r),
        lsp::LspResult::LLMContextResult(r) => llm_context_fmt::json_of_result(r),
        lsp::LspResult::RegisterCapabilityResult
        | lsp::LspResult::ShowMessageRequestResult(_)
        | lsp::LspResult::ShowStatusResult(_)
        | lsp::LspResult::CompletionItemResolveResult(_) => {
            panic!("Don't know how to print result {}", method_);
        }
        lsp::LspResult::ErrorResult(e, stack) => print_error(include_error_stack_trace, e, stack),
    };
    let result_key = match result {
        lsp::LspResult::ErrorResult(_, _) => "error",
        _ => "result",
    };
    #[derive(serde::Serialize)]
    struct Response {
        jsonrpc: &'static str,
        id: serde_json::Value,
        #[serde(skip_serializing_if = "Option::is_none")]
        result: Option<serde_json::Value>,
        #[serde(skip_serializing_if = "Option::is_none")]
        error: Option<serde_json::Value>,
    }
    serde_json::to_value(Response {
        jsonrpc: "2.0",
        id: print_id(id),
        result: (result_key == "result").then_some(json.clone()),
        error: (result_key == "error").then_some(json),
    })
    .expect("LSP response should serialize to JSON")
}

pub fn print_lsp_notification(notification: &lsp::LspNotification) -> serde_json::Value {
    let method_ = notification_name_to_string(notification);
    let params = match notification {
        lsp::LspNotification::ExitNotification
        | lsp::LspNotification::InitializedNotification
        | lsp::LspNotification::SetTraceNotification
        | lsp::LspNotification::LogTraceNotification
        | lsp::LspNotification::DidChangeConfigurationNotification(_)
        | lsp::LspNotification::DidChangeWatchedFilesNotification(_)
        | lsp::LspNotification::DidOpenNotification(_)
        | lsp::LspNotification::DidCloseNotification(_)
        | lsp::LspNotification::DidSaveNotification(_)
        | lsp::LspNotification::DidChangeNotification(_) => {
            panic!("Don't know how to print notification {}", method_)
        }
        lsp::LspNotification::CancelRequestNotification(r) => print_cancel_request(r),
        lsp::LspNotification::PublishDiagnosticsNotification(r) => print_diagnostics(r),
        lsp::LspNotification::TelemetryNotification(r) => print_log_message(r.typ, &r.message),
        lsp::LspNotification::LogMessageNotification(r) => print_log_message(r.typ, &r.message),
        lsp::LspNotification::ShowMessageNotification(r) => print_show_message(r.typ, &r.message),
        lsp::LspNotification::ConnectionStatusNotification(r) => print_connection_status(r),
        lsp::LspNotification::UnknownNotification(_, _) => {
            panic!("Don't know how to print notification {}", method_)
        }
    };
    #[derive(serde::Serialize)]
    struct Notification {
        jsonrpc: &'static str,
        method: String,
        params: serde_json::Value,
    }
    serde_json::to_value(Notification {
        jsonrpc: "2.0",
        method: method_,
        params,
    })
    .expect("LSP notification should serialize to JSON")
}

pub fn print_lsp(
    include_error_stack_trace: bool,
    key: &str,
    message: &lsp::LspMessage,
) -> serde_json::Value {
    match message {
        lsp::LspMessage::RequestMessage(id, request) => print_lsp_request(id, request),
        lsp::LspMessage::ResponseMessage(id, result) => {
            print_lsp_response(include_error_stack_trace, key, id, result)
        }
        lsp::LspMessage::NotificationMessage(notification) => print_lsp_notification(notification),
    }
}
