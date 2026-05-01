/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::PrintableError;
use flow_common_errors::error_utils::StdinFile;
use flow_lint_settings::severity::Severity;
use flow_parser::loc::Loc;

use crate::lsp_prot;
use crate::server_prot;

fn loc_to_lsp_range(loc: &Loc) -> lsp_types::Range {
    lsp_types::Range {
        start: lsp_types::Position {
            line: loc.start.line.saturating_sub(1).max(0) as u32,
            character: loc.start.column.max(0) as u32,
        },
        end: lsp_types::Position {
            line: loc.end.line.saturating_sub(1).max(0) as u32,
            character: loc.end.column.max(0) as u32,
        },
    }
}

pub fn error_to_lsp(
    unsaved_content: &StdinFile,
    should_include_vscode_detailed_diagnostics: &dyn Fn(&PrintableError<Loc>) -> bool,
    severity: Severity,
    printable_error: &PrintableError<Loc>,
) -> Option<(lsp_types::Url, lsp_types::Diagnostic)> {
    let has_detailed_diagnostics = should_include_vscode_detailed_diagnostics(printable_error);
    let lsp_error = flow_common_errors::error_utils::lsp_output::lsp_of_error(
        has_detailed_diagnostics,
        printable_error,
    );
    let loc = &lsp_error.loc;
    let source = loc.source.as_ref()?;
    let uri = lsp_types::Url::from_file_path(source.to_path_buf()).ok()?;
    let related_information: Vec<lsp_types::DiagnosticRelatedInformation> = lsp_error
        .related_locations
        .iter()
        .filter_map(|(loc, related_message)| {
            let source = loc.source.as_ref()?;
            let uri = lsp_types::Url::from_file_path(source.to_path_buf()).ok()?;
            Some(lsp_types::DiagnosticRelatedInformation {
                location: lsp_types::Location {
                    uri,
                    range: loc_to_lsp_range(loc),
                },
                message: related_message.clone(),
            })
        })
        .collect();
    let data = if has_detailed_diagnostics {
        let map_color = |color: &flow_utils_tty::RawColor| -> &'static str {
            match color {
                flow_utils_tty::RawColor::Default => "default",
                flow_utils_tty::RawColor::Black => "black",
                flow_utils_tty::RawColor::Red => "red",
                flow_utils_tty::RawColor::Green => "green",
                flow_utils_tty::RawColor::Yellow => "yellow",
                flow_utils_tty::RawColor::Blue => "blue",
                flow_utils_tty::RawColor::Magenta => "magenta",
                flow_utils_tty::RawColor::Cyan => "cyan",
                flow_utils_tty::RawColor::White => "white",
            }
        };
        let map_style = |style: &flow_utils_tty::Style| -> serde_json::Value {
            let (type_, color) = match style {
                flow_utils_tty::Style::Normal(color) => ("normal", map_color(color)),
                flow_utils_tty::Style::Bold(color) => ("bold", map_color(color)),
                flow_utils_tty::Style::Dim(color) => ("dim", map_color(color)),
                flow_utils_tty::Style::Underline(color) => ("underline", map_color(color)),
                flow_utils_tty::Style::BoldUnderline(color) => ("bold-underline", map_color(color)),
                flow_utils_tty::Style::DimUnderline(color) => ("dim-underline", map_color(color)),
                flow_utils_tty::Style::Italics(color)
                | flow_utils_tty::Style::BoldDim(color)
                | flow_utils_tty::Style::BoldItalics(color)
                | flow_utils_tty::Style::NormalWithBG(color, _)
                | flow_utils_tty::Style::BoldWithBG(color, _) => ("normal", map_color(color)),
            };
            serde_json::json!({ "type": type_, "color": color })
        };
        let rendered =
            flow_common_errors::error_utils::cli_output::format_single_styled_error_for_vscode(
                None,
                severity,
                unsaved_content,
                printable_error.clone(),
            )
            .into_iter()
            .map(|(style, text)| serde_json::json!({ "style": map_style(&style), "text": text }))
            .collect::<Vec<_>>();
        Some(serde_json::json!({ "version": 0, "rendered": rendered }))
    } else {
        None
    };
    Some((
        uri,
        lsp_types::Diagnostic {
            range: loc_to_lsp_range(loc),
            severity: match severity {
                Severity::Err => Some(lsp_types::DiagnosticSeverity::ERROR),
                Severity::Warn => Some(lsp_types::DiagnosticSeverity::WARNING),
                Severity::Off => None,
            },
            code: Some(lsp_types::NumberOrString::String(lsp_error.code.clone())),
            source: Some("Flow".to_string()),
            message: lsp_error.message.clone(),
            tags: None,
            related_information: if related_information.is_empty() {
                None
            } else {
                Some(related_information)
            },
            data,
            code_description: None,
        },
    ))
}

pub fn diagnostics_of_flow_errors(
    unsaved_content: &StdinFile,
    should_include_vscode_detailed_diagnostics: &dyn Fn(&PrintableError<Loc>) -> bool,
    errors: &ConcreteLocPrintableErrorSet,
    warnings: &ConcreteLocPrintableErrorSet,
) -> lsp_prot::UriMap<Vec<lsp_types::Diagnostic>> {
    let mut acc = lsp_prot::UriMap::new();
    for error in errors.iter() {
        match error_to_lsp(
            unsaved_content,
            should_include_vscode_detailed_diagnostics,
            Severity::Err,
            error,
        ) {
            Some((uri, diagnostic)) => acc.entry(uri).or_insert_with(Vec::new).push(diagnostic),
            None => {}
        }
    }
    for warning in warnings.iter() {
        match error_to_lsp(
            unsaved_content,
            should_include_vscode_detailed_diagnostics,
            Severity::Warn,
            warning,
        ) {
            Some((uri, diagnostic)) => acc.entry(uri).or_insert_with(Vec::new).push(diagnostic),
            None => {}
        }
    }
    acc
}

pub fn flow_completion_item_to_lsp(
    token: Option<&str>,
    autocomplete_session_length: Option<i32>,
    typed_len: Option<i32>,
    ac_type: &str,
    _is_snippet_supported: bool,
    is_tags_supported: &dyn Fn(lsp_types::CompletionItemTag) -> bool,
    is_preselect_supported: bool,
    is_label_detail_supported: bool,
    is_insert_replace_supported: bool,
    index: usize,
    item: server_prot::response::completion::CompletionItem,
) -> lsp_types::CompletionItem {
    let text_edit = item.text_edit.map(
        |server_prot::response::InsertReplaceEdit {
             newText,
             insert,
             replace,
         }| {
            if is_insert_replace_supported && insert != replace {
                lsp_types::CompletionTextEdit::InsertAndReplace(lsp_types::InsertReplaceEdit {
                    new_text: newText,
                    insert: loc_to_lsp_range(&insert),
                    replace: loc_to_lsp_range(&replace),
                })
            } else {
                lsp_types::CompletionTextEdit::Edit(lsp_types::TextEdit {
                    range: loc_to_lsp_range(&insert),
                    new_text: newText,
                })
            }
        },
    );
    let additional_text_edits: Vec<lsp_types::TextEdit> = item
        .additional_text_edits
        .into_iter()
        .map(|(loc, new_text)| lsp_types::TextEdit {
            range: loc_to_lsp_range(&loc),
            new_text,
        })
        .collect();
    let documentation = item.documentation.map(|doc| {
        lsp_types::Documentation::MarkupContent(lsp_types::MarkupContent {
            kind: lsp_types::MarkupKind::Markdown,
            value: doc.trim().to_string(),
        })
    });
    let tags = item.tags.and_then(|tags| {
        let tags: Vec<_> = tags
            .into_iter()
            .filter(|tag| is_tags_supported(tag.clone()))
            .collect();
        if tags.is_empty() { None } else { Some(tags) }
    });
    let command = Some(lsp_types::Command {
        title: "".to_string(),
        command: "log".to_string(),
        arguments: Some(vec![
            serde_json::Value::String("textDocument/completion".to_string()),
            serde_json::Value::String(item.log_info.clone()),
            serde_json::json!({
                "token": token,
                "index": index,
                "session_requests": autocomplete_session_length,
                "typed_length": typed_len,
                "completion": item.name,
                "ac_type": ac_type,
            }),
        ]),
    });
    let label_details = if is_label_detail_supported
        && (item.labelDetail.is_some() || item.description.is_some())
    {
        let detail = item.labelDetail.map(|detail| {
            let column_width = 80;
            if detail.len() < column_width {
                detail
            } else {
                format!("{}...", &detail[..column_width])
            }
        });
        Some(lsp_types::CompletionItemLabelDetails {
            detail,
            description: item.description.clone(),
        })
    } else {
        None
    };
    lsp_types::CompletionItem {
        label: item.name,
        label_details,
        kind: item.kind,
        detail: item.itemDetail,
        documentation,
        tags,
        preselect: if is_preselect_supported && item.preselect {
            Some(true)
        } else {
            None
        },
        sort_text: item.sort_text,
        filter_text: None,
        insert_text: None,
        insert_text_format: Some(item.insert_text_format),
        text_edit,
        additional_text_edits: if additional_text_edits.is_empty() {
            None
        } else {
            Some(additional_text_edits)
        },
        command,
        data: None,
        ..Default::default()
    }
}

pub fn flow_completions_to_lsp(
    token: Option<&str>,
    autocomplete_session_length: Option<i32>,
    typed_len: Option<i32>,
    ac_type: &str,
    is_snippet_supported: bool,
    is_tags_supported: &dyn Fn(lsp_types::CompletionItemTag) -> bool,
    is_preselect_supported: bool,
    is_label_detail_supported: bool,
    is_insert_replace_supported: bool,
    completions: server_prot::response::completion::T,
) -> lsp_types::CompletionResponse {
    let server_prot::response::completion::T {
        items,
        is_incomplete,
    } = completions;
    let items = items
        .into_iter()
        .enumerate()
        .map(|(index, mut item)| {
            item.sort_text = Some(format!("{index:020}"));
            flow_completion_item_to_lsp(
                token,
                autocomplete_session_length,
                typed_len,
                ac_type,
                is_snippet_supported,
                is_tags_supported,
                is_preselect_supported,
                is_label_detail_supported,
                is_insert_replace_supported,
                index,
                item,
            )
        })
        .collect();
    lsp_types::CompletionResponse::List(lsp_types::CompletionList {
        is_incomplete,
        items,
    })
}
