/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_server_utils::file_content;

fn lsp_position_to_fc(pos: lsp_types::Position) -> file_content::Position {
    file_content::Position {
        line: pos.line as usize + 1,
        column: pos.character as usize + 1,
    }
}

fn lsp_range_to_fc(range: lsp_types::Range) -> file_content::Range {
    file_content::Range {
        st: lsp_position_to_fc(range.start),
        ed: lsp_position_to_fc(range.end),
    }
}

fn lsp_edit_to_fc(edit: &lsp_types::TextDocumentContentChangeEvent) -> file_content::TextEdit {
    file_content::TextEdit {
        range: edit.range.map(lsp_range_to_fc),
        text: edit.text.clone(),
    }
}

pub fn apply_changes(
    text: &str,
    content_changes: &[lsp_types::TextDocumentContentChangeEvent],
) -> Result<String, (String, String)> {
    let edits: Vec<file_content::TextEdit> = content_changes.iter().map(lsp_edit_to_fc).collect();
    file_content::edit_file(text, &edits).map_err(|reason| (reason, String::new()))
}

pub fn supports_snippets(p: &lsp_types::InitializeParams) -> bool {
    p.capabilities
        .text_document
        .as_ref()
        .and_then(|text_document| text_document.completion.as_ref())
        .and_then(|completion| completion.completion_item.as_ref())
        .and_then(|completion_item| completion_item.snippet_support)
        .unwrap_or(false)
}

pub fn supports_tags(p: &lsp_types::InitializeParams, tag: lsp_types::CompletionItemTag) -> bool {
    p.capabilities
        .text_document
        .as_ref()
        .and_then(|text_document| text_document.completion.as_ref())
        .and_then(|completion| completion.completion_item.as_ref())
        .and_then(|completion_item| completion_item.tag_support.as_ref())
        .map(|tag_support| tag_support.value_set.contains(&tag))
        .unwrap_or(false)
}

pub fn supports_preselect(p: &lsp_types::InitializeParams) -> bool {
    p.capabilities
        .text_document
        .as_ref()
        .and_then(|text_document| text_document.completion.as_ref())
        .and_then(|completion| completion.completion_item.as_ref())
        .and_then(|completion_item| completion_item.preselect_support)
        .unwrap_or(false)
}

pub fn supports_completion_item_insert_replace(p: &lsp_types::InitializeParams) -> bool {
    p.capabilities
        .text_document
        .as_ref()
        .and_then(|text_document| text_document.completion.as_ref())
        .and_then(|completion| completion.completion_item.as_ref())
        .and_then(|completion_item| completion_item.insert_replace_support)
        .unwrap_or(false)
}

pub fn supports_completion_item_label_details(p: &lsp_types::InitializeParams) -> bool {
    p.capabilities
        .text_document
        .as_ref()
        .and_then(|text_document| text_document.completion.as_ref())
        .and_then(|completion| completion.completion_item.as_ref())
        .and_then(|completion_item| completion_item.label_details_support)
        .unwrap_or(false)
}
