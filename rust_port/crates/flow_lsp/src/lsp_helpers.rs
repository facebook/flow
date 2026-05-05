/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_server_utils::file_content;
use lsp_types::Position;
use lsp_types::Range;

use crate::lsp::DocumentUri;
use crate::lsp::error as lsp_error;

pub type FilePath = std::path::PathBuf;

/************************************************************************/
/* Conversions                                                          */
/************************************************************************/

fn invalid_file_url_error(uri: &DocumentUri) -> lsp_error::T {
    lsp_error::T {
        code: lsp_error::Code::InvalidParams,
        message: format!("Not a valid file url '{}'", uri),
        data: None,
    }
}

pub fn lsp_uri_to_path(uri: &DocumentUri) -> Result<String, lsp_error::T> {
    if uri.scheme() == "file" {
        uri.to_file_path()
            .map(|p| p.to_string_lossy().to_string())
            .map_err(|_| invalid_file_url_error(uri))
    } else {
        Err(invalid_file_url_error(uri))
    }
}

pub fn path_to_lsp_uri(path: &str, default_path: &str) -> DocumentUri {
    if path.is_empty() {
        DocumentUri::from_file_path(default_path).expect("path_to_lsp_uri: default_path invalid")
    } else {
        DocumentUri::from_file_path(path).expect("path_to_lsp_uri: path invalid")
    }
}

#[allow(non_snake_case)]
pub fn lsp_textDocumentIdentifier_to_filename(
    identifier: &lsp_types::TextDocumentIdentifier,
) -> Result<String, lsp_error::T> {
    lsp_uri_to_path(&identifier.uri)
}

pub fn lsp_position_to_fc(pos: lsp_types::Position) -> file_content::Position {
    file_content::Position {
        line: pos.line as usize + 1,
        column: pos.character as usize + 1,
    }
}

pub fn lsp_range_to_fc(range: lsp_types::Range) -> file_content::Range {
    file_content::Range {
        st: lsp_position_to_fc(range.start),
        ed: lsp_position_to_fc(range.end),
    }
}

pub fn lsp_edit_to_fc(edit: &lsp_types::TextDocumentContentChangeEvent) -> file_content::TextEdit {
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

pub fn get_char_from_lsp_position(content: &str, position: lsp_types::Position) -> u8 {
    let fc_position = lsp_position_to_fc(position);
    file_content::get_char(
        content.as_bytes(),
        file_content::get_offset(content, &fc_position),
    )
}

pub fn apply_changes_unsafe(
    text: &str,
    content_changes: &[lsp_types::TextDocumentContentChangeEvent],
) -> String {
    match apply_changes(text, content_changes) {
        Ok(r) => r,
        Err((e, _stack)) => panic!("{}", e),
    }
}

/************************************************************************/
/* Range calculations                                                   */
/************************************************************************/

pub fn pos_compare(p1: &Position, p2: &Position) -> i32 {
    if p1.line < p2.line {
        -1
    } else if p1.line > p2.line {
        1
    } else {
        p1.character as i32 - p2.character as i32
    }
}

pub fn ranges_overlap(a: &Range, b: &Range) -> bool {
    !(pos_compare(&a.end, &b.start) < 0 || pos_compare(&a.start, &b.end) > 0)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RangeOverlap {
    SelectionBeforeStartOfSquiggle,
    SelectionOverlapsStartOfSquiggle,
    SelectionCoversWholeSquiggle,
    SelectionInMiddleOfSquiggle,
    SelectionOverlapsEndOfSquiggle,
    SelectionAfterEndOfSquiggle,
}

pub fn get_range_overlap(selection: &Range, squiggle: &Range) -> RangeOverlap {
    let sel_start_leq_squiggle_start = pos_compare(&selection.start, &squiggle.start) <= 0;
    let sel_start_leq_squiggle_end = pos_compare(&selection.start, &squiggle.end) <= 0;
    let sel_end_lt_squiggle_start = pos_compare(&selection.end, &squiggle.start) < 0;
    let sel_end_lt_squiggle_end = pos_compare(&selection.end, &squiggle.end) < 0;
    match (
        sel_start_leq_squiggle_start,
        sel_start_leq_squiggle_end,
        sel_end_lt_squiggle_start,
        sel_end_lt_squiggle_end,
    ) {
        (true, true, true, true) => RangeOverlap::SelectionBeforeStartOfSquiggle,
        (true, true, false, true) => RangeOverlap::SelectionOverlapsStartOfSquiggle,
        (true, true, false, false) => RangeOverlap::SelectionCoversWholeSquiggle,
        (false, true, false, true) => RangeOverlap::SelectionInMiddleOfSquiggle,
        (false, true, false, false) => RangeOverlap::SelectionOverlapsEndOfSquiggle,
        (false, false, false, false) => RangeOverlap::SelectionAfterEndOfSquiggle,
        _ => panic!("invalid range overlap"),
    }
}

pub struct RangeReplace {
    pub remove_range: Range,
    pub insert_lines: u32,
    pub insert_chars_on_final_line: u32,
}

pub fn update_pos_due_to_prior_replace(p: &Position, replace: &RangeReplace) -> Position {
    if replace.remove_range.end.line < p.line {
        let line = p.line - (replace.remove_range.end.line - replace.remove_range.start.line)
            + replace.insert_lines;
        Position {
            line,
            character: p.character,
        }
    } else if replace.insert_lines > 0 {
        let line = p.line - (replace.remove_range.end.line - replace.remove_range.start.line)
            + replace.insert_lines;
        let character =
            replace.insert_chars_on_final_line + (p.character - replace.remove_range.end.character);
        Position { line, character }
    } else {
        let line = p.line - (replace.remove_range.end.line - replace.remove_range.start.line);
        let character = replace.remove_range.start.character
            + replace.insert_chars_on_final_line
            + (p.character - replace.remove_range.end.character);
        Position { line, character }
    }
}

pub fn update_range_due_to_replace(squiggle: &Range, replace: &RangeReplace) -> Option<Range> {
    match get_range_overlap(&replace.remove_range, squiggle) {
        RangeOverlap::SelectionBeforeStartOfSquiggle => {
            let start = update_pos_due_to_prior_replace(&squiggle.start, replace);
            let end = update_pos_due_to_prior_replace(&squiggle.end, replace);
            Some(Range { start, end })
        }
        RangeOverlap::SelectionOverlapsStartOfSquiggle => {
            let line = replace.remove_range.start.line + replace.insert_lines;
            let character = if replace.insert_lines == 0 {
                replace.remove_range.start.character + replace.insert_chars_on_final_line
            } else {
                replace.insert_chars_on_final_line
            };
            let start = Position { line, character };
            let end = update_pos_due_to_prior_replace(&squiggle.end, replace);
            Some(Range { start, end })
        }
        RangeOverlap::SelectionCoversWholeSquiggle => None,
        RangeOverlap::SelectionInMiddleOfSquiggle => {
            let start = squiggle.start;
            let end = update_pos_due_to_prior_replace(&squiggle.end, replace);
            Some(Range { start, end })
        }
        RangeOverlap::SelectionOverlapsEndOfSquiggle => {
            let start = squiggle.start;
            let end = replace.remove_range.start;
            Some(Range { start, end })
        }
        RangeOverlap::SelectionAfterEndOfSquiggle => Some(*squiggle),
    }
}

pub fn update_diagnostics_due_to_change(
    diagnostics: Vec<lsp_types::Diagnostic>,
    change: &lsp_types::DidChangeTextDocumentParams,
) -> Vec<lsp_types::Diagnostic> {
    fn replace_of_change(
        change: &lsp_types::TextDocumentContentChangeEvent,
    ) -> Option<RangeReplace> {
        match change.range {
            None => None,
            Some(remove_range) => {
                let text = &change.text;
                let insert_lines = text.chars().filter(|c| *c == '\n').count() as u32;
                let insert_chars_on_final_line = text
                    .rsplit('\n')
                    .next()
                    .map(|s| s.len() as u32)
                    .unwrap_or(text.len() as u32);
                Some(RangeReplace {
                    remove_range,
                    insert_lines,
                    insert_chars_on_final_line,
                })
            }
        }
    }
    fn apply_replace(
        diagnostic_opt: Option<lsp_types::Diagnostic>,
        replace_opt: &Option<RangeReplace>,
    ) -> Option<lsp_types::Diagnostic> {
        match (diagnostic_opt, replace_opt) {
            (Some(diagnostic), Some(replace)) => {
                let range = update_range_due_to_replace(&diagnostic.range, replace);
                range.map(|range| lsp_types::Diagnostic {
                    range,
                    ..diagnostic
                })
            }
            _ => None,
        }
    }
    let replaces: Vec<Option<RangeReplace>> = change
        .content_changes
        .iter()
        .map(replace_of_change)
        .collect();
    let apply_all_replaces = |diagnostic: lsp_types::Diagnostic| -> Option<lsp_types::Diagnostic> {
        replaces.iter().try_fold(diagnostic, |diagnostic, replace| {
            apply_replace(Some(diagnostic), replace)
        })
    };
    diagnostics
        .into_iter()
        .filter_map(apply_all_replaces)
        .collect()
}

/************************************************************************/
/* Accessors                                                            */
/************************************************************************/

#[allow(deprecated)] // root_path is deprecated in LSP but OCaml uses it as fallback
pub fn get_root(params: &lsp_types::InitializeParams) -> Result<FilePath, lsp_error::T> {
    if let Some(ref uri) = params.root_uri {
        Ok(FilePath::from(lsp_uri_to_path(uri)?))
    } else if let Some(ref path) = params.root_path {
        Ok(FilePath::from(path))
    } else {
        Err(lsp_error::T {
            code: lsp_error::Code::InternalError,
            message: "Initialize params missing root".to_string(),
            data: None,
        })
    }
}

pub fn supports_code_action_kinds(params: &lsp_types::InitializeParams) -> Vec<String> {
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

pub fn supports_experimental_snippet_text_edit(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .experimental
        .as_ref()
        .and_then(|exp| exp.get("snippetTextEdit"))
        .and_then(|v| v.as_bool())
        .unwrap_or(false)
}

pub fn supports_configuration(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .workspace
        .as_ref()
        .and_then(|ws| ws.configuration)
        .unwrap_or(false)
}

pub fn supports_status(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .experimental
        .as_ref()
        .and_then(|experimental| experimental.get("window/status"))
        .and_then(|status| status.as_bool())
        .unwrap_or(false)
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

pub fn supports_hierarchical_document_symbol(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .text_document
        .as_ref()
        .and_then(|td| td.document_symbol.as_ref())
        .and_then(|ds| ds.hierarchical_document_symbol_support)
        .unwrap_or(false)
}

pub fn supports_connection_status(params: &lsp_types::InitializeParams) -> bool {
    params
        .capabilities
        .experimental
        .as_ref()
        .and_then(|experimental| experimental.get("telemetry/connectionStatus"))
        .and_then(|status| status.as_bool())
        .unwrap_or(false)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn p(line: u32, character: u32) -> Position {
        Position { line, character }
    }

    fn r(start: Position, end: Position) -> Range {
        Range { start, end }
    }

    fn replace(
        remove_range: Range,
        insert_lines: u32,
        insert_chars_on_final_line: u32,
    ) -> RangeReplace {
        RangeReplace {
            remove_range,
            insert_lines,
            insert_chars_on_final_line,
        }
    }

    #[test]
    fn test_pos_compare() {
        let p1 = p(1, 3);
        let p2 = p(2, 0);
        let p3 = p(2, 1);
        assert!(pos_compare(&p1, &p2) < 0, "p1 < p2");
        assert!(pos_compare(&p2, &p1) > 0, "p2 > p1");
        assert_eq!(0, pos_compare(&p2, &p2), "p2 = p2");
        assert!(pos_compare(&p2, &p3) < 0, "p2 < p3");
    }

    #[test]
    fn test_range_overlap() {
        let p1 = p(1, 5);
        let p2 = p(2, 3);
        let p3 = p(2, 4);
        let p4 = p(3, 1);
        let p5 = p(3, 9);
        let p6 = p(4, 5);
        let p7 = p(4, 8);
        let p8 = p(5, 4);
        let sel11 = r(p1, p1);
        let sel12 = r(p1, p2);
        let sel13 = r(p1, p3);
        let sel14 = r(p1, p4);
        let sel15 = r(p1, p5);
        let sel16 = r(p1, p6);
        let sel17 = r(p1, p7);
        let sel18 = r(p1, p8);
        let sel22 = r(p2, p2);
        let sel23 = r(p2, p3);
        let sel24 = r(p2, p4);
        let sel25 = r(p2, p5);
        let sel26 = r(p2, p6);
        let sel27 = r(p2, p7);
        let sel28 = r(p2, p8);
        let sel33 = r(p3, p3);
        let sel34 = r(p3, p4);
        let sel35 = r(p3, p5);
        let sel36 = r(p3, p6);
        let sel37 = r(p3, p7);
        let sel38 = r(p3, p8);
        let sel44 = r(p4, p4);
        let sel45 = r(p4, p5);
        let sel46 = r(p4, p6);
        let sel47 = r(p4, p7);
        let sel48 = r(p4, p8);
        let sel55 = r(p5, p5);
        let sel56 = r(p5, p6);
        let sel57 = r(p5, p7);
        let sel58 = r(p5, p8);
        let sel66 = r(p6, p6);
        let sel67 = r(p6, p7);
        let sel68 = r(p6, p8);
        let sel77 = r(p7, p7);
        let sel78 = r(p7, p8);
        let sel88 = r(p8, p8);
        let squiggle = sel36;
        let cases = [
            (sel11, RangeOverlap::SelectionBeforeStartOfSquiggle, "sel11"),
            (sel12, RangeOverlap::SelectionBeforeStartOfSquiggle, "sel12"),
            (
                sel13,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel13",
            ),
            (
                sel14,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel14",
            ),
            (
                sel15,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel15",
            ),
            (sel16, RangeOverlap::SelectionCoversWholeSquiggle, "sel16"),
            (sel17, RangeOverlap::SelectionCoversWholeSquiggle, "sel17"),
            (sel18, RangeOverlap::SelectionCoversWholeSquiggle, "sel18"),
            (sel22, RangeOverlap::SelectionBeforeStartOfSquiggle, "sel22"),
            (
                sel23,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel23",
            ),
            (
                sel24,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel24",
            ),
            (
                sel25,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel25",
            ),
            (sel26, RangeOverlap::SelectionCoversWholeSquiggle, "sel26"),
            (sel27, RangeOverlap::SelectionCoversWholeSquiggle, "sel27"),
            (sel28, RangeOverlap::SelectionCoversWholeSquiggle, "sel28"),
            (
                sel33,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel33",
            ),
            (
                sel34,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel34",
            ),
            (
                sel35,
                RangeOverlap::SelectionOverlapsStartOfSquiggle,
                "sel35",
            ),
            (sel36, RangeOverlap::SelectionCoversWholeSquiggle, "sel36"),
            (sel37, RangeOverlap::SelectionCoversWholeSquiggle, "sel37"),
            (sel38, RangeOverlap::SelectionCoversWholeSquiggle, "sel38"),
            (sel44, RangeOverlap::SelectionInMiddleOfSquiggle, "sel44"),
            (sel45, RangeOverlap::SelectionInMiddleOfSquiggle, "sel45"),
            (sel46, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel46"),
            (sel47, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel47"),
            (sel48, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel48"),
            (sel55, RangeOverlap::SelectionInMiddleOfSquiggle, "sel55"),
            (sel56, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel56"),
            (sel57, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel57"),
            (sel58, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel58"),
            (sel66, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel66"),
            (sel67, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel67"),
            (sel68, RangeOverlap::SelectionOverlapsEndOfSquiggle, "sel68"),
            (sel77, RangeOverlap::SelectionAfterEndOfSquiggle, "sel77"),
            (sel78, RangeOverlap::SelectionAfterEndOfSquiggle, "sel78"),
            (sel88, RangeOverlap::SelectionAfterEndOfSquiggle, "sel88"),
        ];
        for (selection, expected, msg) in cases {
            assert_eq!(expected, get_range_overlap(&selection, &squiggle), "{msg}");
        }
    }

    #[test]
    fn test_update_pos_due_to_prior_replace() {
        let remove_many = r(p(1, 6), p(3, 3));
        let remove_line = r(p(1, 3), p(1, 6));
        let m_m = replace(remove_many, 3, 4);
        let m_l = replace(remove_many, 0, 4);
        let l_m = replace(remove_line, 3, 4);
        let l_l = replace(remove_line, 0, 4);
        let p1 = p(1, 11);
        let p3 = p(3, 8);
        let p5 = p(5, 1);
        assert_eq!(
            p(4, 4),
            update_pos_due_to_prior_replace(&remove_many.end, &m_m),
            "replace multiline->multiline, point at end of removal"
        );
        assert_eq!(
            p(4, 9),
            update_pos_due_to_prior_replace(&p3, &m_m),
            "replace multiline->multiline, point on same line"
        );
        assert_eq!(
            p(6, 1),
            update_pos_due_to_prior_replace(&p5, &m_m),
            "replace multiline->multiline, point on later line"
        );
        assert_eq!(
            p(1, 10),
            update_pos_due_to_prior_replace(&remove_many.end, &m_l),
            "replace multiline->one line, point at end of removal"
        );
        assert_eq!(
            p(1, 15),
            update_pos_due_to_prior_replace(&p3, &m_l),
            "replace multiline->one line, point on same line"
        );
        assert_eq!(
            p(3, 1),
            update_pos_due_to_prior_replace(&p5, &m_l),
            "replace multiline->one line, point on later line"
        );
        assert_eq!(
            p(4, 4),
            update_pos_due_to_prior_replace(&remove_line.end, &l_m),
            "replace one line->multiline, point at end of removal"
        );
        assert_eq!(
            p(4, 9),
            update_pos_due_to_prior_replace(&p1, &l_m),
            "replace one line->multiline, point on same line"
        );
        assert_eq!(
            p(6, 8),
            update_pos_due_to_prior_replace(&p3, &l_m),
            "replace one line->multiline, point on later line"
        );
        assert_eq!(
            p(1, 7),
            update_pos_due_to_prior_replace(&remove_line.end, &l_l),
            "replace one line->one line, point at end of removal"
        );
        assert_eq!(
            p(1, 12),
            update_pos_due_to_prior_replace(&p1, &l_l),
            "replace one line->one line, point on same line"
        );
        assert_eq!(
            p(3, 8),
            update_pos_due_to_prior_replace(&p3, &l_l),
            "replace one line->one line, point on later line"
        );
    }

    #[test]
    fn test_update_range_due_to_replace() {
        let squiggle = r(p(2, 3), p(2, 8));
        let cases = [
            (
                replace(r(p(0, 0), p(1, 0)), 0, 0),
                Some(r(p(1, 3), p(1, 8))),
                "delete line above squiggle",
            ),
            (
                replace(r(p(0, 0), p(0, 0)), 1, 0),
                Some(r(p(3, 3), p(3, 8))),
                "add line above squiggle",
            ),
            (
                replace(r(p(2, 2), p(2, 3)), 0, 0),
                Some(r(p(2, 2), p(2, 7))),
                "del char before squiggle",
            ),
            (
                replace(r(p(2, 3), p(2, 3)), 0, 1),
                Some(r(p(2, 4), p(2, 9))),
                "add char before squiggle",
            ),
            (
                replace(r(p(2, 5), p(2, 6)), 0, 0),
                Some(r(p(2, 3), p(2, 7))),
                "del char in squiggle",
            ),
            (
                replace(r(p(2, 5), p(2, 5)), 0, 1),
                Some(r(p(2, 3), p(2, 9))),
                "add char in squiggle",
            ),
            (
                replace(r(p(2, 8), p(3, 0)), 0, 0),
                Some(r(p(2, 3), p(2, 8))),
                "del line after squiggle",
            ),
            (
                replace(r(p(2, 8), p(2, 8)), 1, 0),
                Some(r(p(2, 3), p(2, 8))),
                "add line after squiggle",
            ),
            (
                replace(r(p(2, 8), p(2, 9)), 0, 0),
                Some(r(p(2, 3), p(2, 8))),
                "del char after squiggle",
            ),
            (
                replace(r(p(2, 8), p(2, 8)), 0, 1),
                Some(r(p(2, 3), p(2, 8))),
                "add char after squiggle",
            ),
            (replace(r(p(2, 3), p(2, 8)), 0, 0), None, "del squiggle"),
            (
                replace(r(p(2, 0), p(3, 0)), 0, 0),
                None,
                "del squiggle line",
            ),
            (
                replace(r(p(2, 0), p(3, 0)), 1, 10),
                None,
                "replace squiggle line",
            ),
            (
                replace(r(p(2, 0), p(2, 5)), 0, 1),
                Some(r(p(2, 1), p(2, 4))),
                "replace squiggle start",
            ),
            (
                replace(r(p(2, 5), p(2, 12)), 0, 4),
                Some(r(p(2, 3), p(2, 5))),
                "replace squiggle end",
            ),
        ];
        for (replace, expected, msg) in cases {
            assert_eq!(
                expected,
                update_range_due_to_replace(&squiggle, &replace),
                "{msg}"
            );
        }
    }
}
