/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module is how the Flow lsp stores and reasons about Flow errors. This is tricky because
// Flow errors might come from a few different places.
//
// Here's our general strategy:
//
// 1. If we get streamed server errors for a file, then we either
//    1a. Add them to the known server errors for the file, if we previously had streamed errors for
//        that file
//    1b. Replace the known server errors for the file, if we previously had finalized server errors
//        (e.g. from after the last recheck)
// 2. If we have live parse errors for a file (e.g. from running the parser locally on an open file)
//    then we replace the server's parse errors for that file with the live parse errors
// 3. If we have live non-parse errors for a file (e.g. from check-contents for an open file)
//    then we replace the server's non-parse errors for that file with the live non-parse errors.

use std::collections::BTreeMap;
use std::collections::BTreeSet;

use lsp_types::Diagnostic;
use lsp_types::DiagnosticSeverity;
use lsp_types::NumberOrString;
use lsp_types::Position;
use lsp_types::Range;
use lsp_types::Url;

type Errors = Vec<Diagnostic>;

#[derive(Debug, Clone)]
struct ParseErrors(Errors);

#[derive(Debug, Clone)]
struct NonParseErrors(Errors);

type SplitErrors = (ParseErrors, NonParseErrors);

#[derive(Debug, Clone)]
enum ServerErrors {
    Streamed(SplitErrors),
    Finalized(SplitErrors),
}

#[derive(Debug, Clone)]
struct PerFileErrors {
    live_parse_errors: Option<ParseErrors>,
    live_non_parse_errors: Option<NonParseErrors>,
    server_errors: ServerErrors,
}

#[derive(Debug, Clone)]
pub struct T {
    dirty_files: BTreeSet<Url>,
    file_to_errors_map: BTreeMap<Url, PerFileErrors>,
}

fn empty_per_file_errors() -> PerFileErrors {
    PerFileErrors {
        live_parse_errors: None,
        live_non_parse_errors: None,
        server_errors: ServerErrors::Finalized((ParseErrors(vec![]), NonParseErrors(vec![]))),
    }
}

// Returns true if we don't know about any errors for this file
fn file_has_no_errors(errors: &PerFileErrors) -> bool {
    let live_parse_empty = match &errors.live_parse_errors {
        None => true,
        Some(ParseErrors(v)) if v.is_empty() => true,
        _ => false,
    };
    let live_non_parse_empty = match &errors.live_non_parse_errors {
        None => true,
        Some(NonParseErrors(v)) if v.is_empty() => true,
        _ => false,
    };
    let server_empty = match &errors.server_errors {
        ServerErrors::Streamed((ParseErrors(p), NonParseErrors(np)))
        | ServerErrors::Finalized((ParseErrors(p), NonParseErrors(np))) => {
            p.is_empty() && np.is_empty()
        }
    };
    live_parse_empty && live_non_parse_empty && server_empty
}

pub fn empty() -> T {
    T {
        dirty_files: BTreeSet::new(),
        file_to_errors_map: BTreeMap::new(),
    }
}

// For the most part we don't sort errors, and leave it to the server and the IDE to figure that
// out. The one exception is in limit_errors, to ensure consistent results
fn sort_errors(errors: &mut Errors) {
    errors.sort_by(|d1, d2| {
        let s1 = &d1.range.start;
        let s2 = &d2.range.start;
        s1.line.cmp(&s2.line).then(s1.character.cmp(&s2.character))
    });
}

// If we have too many errors then limit them to the first N errors
fn limit_errors(errors: Errors) -> Errors {
    fn is_refinement_information(d: &Diagnostic) -> bool {
        match &d.data {
            Some(serde_json::Value::Object(obj)) => {
                obj.get("semanticDecorationType").and_then(|v| v.as_str()) == Some("refined-value")
            }
            _ => false,
        }
    }
    let (special_hidden_errors, errors): (Errors, Errors) =
        errors.into_iter().partition(is_refinement_information);

    let cap = 200;
    let is_below_cap = errors.len() <= cap;
    if is_below_cap {
        let mut result = errors;
        result.extend(special_hidden_errors);
        result
    } else {
        // Sort to make sure we're always sending the same errors
        let mut errors = errors;
        sort_errors(&mut errors);
        let discard = errors.split_off(cap);
        let retain = errors;
        if discard.is_empty() {
            retain
        } else {
            let discard_count = discard.len();
            let message = format!("[Only showing {}/{} diagnostics]", cap, cap + discard_count);
            // the following range displays fine in all editors, regardless of contents
            let diagnostic = Diagnostic {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: 0,
                        character: 0,
                    },
                },
                severity: Some(DiagnosticSeverity::INFORMATION),
                code: None,
                source: Some("Flow".to_string()),
                message,
                related_information: Some(vec![]),
                tags: Some(vec![]),
                code_description: None,
                data: None,
            };
            let mut result = vec![diagnostic];
            result.extend(retain);
            result.extend(special_hidden_errors);
            result
        }
    }
}

fn is_parse_error(d: &Diagnostic) -> bool {
    matches!(&d.code, Some(NumberOrString::String(s)) if s == "ParseError")
}

fn is_not_parse_error(d: &Diagnostic) -> bool {
    !is_parse_error(d)
}

fn split(errors: Errors) -> SplitErrors {
    let (parse_errors, non_parse_errors): (Errors, Errors) =
        errors.into_iter().partition(is_parse_error);
    (ParseErrors(parse_errors), NonParseErrors(non_parse_errors))
}

fn choose_errors(per_file: &PerFileErrors) -> (&Errors, &Errors) {
    let (server_parse_errors, server_non_parse_errors) = match &per_file.server_errors {
        ServerErrors::Streamed((ParseErrors(p), NonParseErrors(np)))
        | ServerErrors::Finalized((ParseErrors(p), NonParseErrors(np))) => (p, np),
    };
    // Prefer live parse errors over server parse errors
    let parse_errors = match &per_file.live_parse_errors {
        None => server_parse_errors,
        Some(ParseErrors(live)) => live,
    };
    let non_parse_errors = match &per_file.live_non_parse_errors {
        None => server_non_parse_errors,
        Some(NonParseErrors(live)) => live,
    };
    (parse_errors, non_parse_errors)
}

fn have_errors_changed(before: &PerFileErrors, after: &PerFileErrors) -> bool {
    let (before_parse, before_non_parse) = choose_errors(before);
    let (after_parse, after_non_parse) = choose_errors(after);
    // Structural equality for fast comparison. Will never get false negative
    before_parse != after_parse || before_non_parse != after_non_parse
}

// We need to send the errors for this file. This is when we need to decide exactly which errors to
// send.
fn send_errors_for_file(state: &T, send_json: &mut dyn FnMut(serde_json::Value), uri: &Url) {
    let default = empty_per_file_errors();
    let per_file = state.file_to_errors_map.get(uri).unwrap_or(&default);
    let (parse_errors, non_parse_errors) = choose_errors(per_file);
    let mut errors: Errors = parse_errors.clone();
    errors.extend(non_parse_errors.iter().cloned());
    let diagnostics = limit_errors(errors);
    let params = lsp_types::PublishDiagnosticsParams {
        uri: uri.clone(),
        diagnostics,
        version: None,
    };
    let notification = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "textDocument/publishDiagnostics",
        "params": serde_json::to_value(&params).unwrap_or_default(),
    });
    send_json(notification);
}

// For every dirty file (files for which the client likely has out-of-date errors), send the errors
// to the client
fn send_all_errors(send_json: &mut dyn FnMut(serde_json::Value), state: T) -> T {
    let dirty_files = state.dirty_files.clone();
    let state = T {
        dirty_files: BTreeSet::new(),
        ..state
    };
    for uri in &dirty_files {
        send_errors_for_file(&state, send_json, uri);
    }
    state
}

// Helper function to modify the data for a specific file
fn modify_per_file_errors(
    uri: &Url,
    state: T,
    f: impl FnOnce(PerFileErrors) -> PerFileErrors,
) -> T {
    let old_per_file_errors = state
        .file_to_errors_map
        .get(uri)
        .cloned()
        .unwrap_or_else(empty_per_file_errors);
    let new_per_file_errors = f(old_per_file_errors.clone());
    let dirty = have_errors_changed(&old_per_file_errors, &new_per_file_errors);
    // To keep this data structure small, let's filter out files with no live or server errors
    let mut file_to_errors_map = state.file_to_errors_map;
    if file_has_no_errors(&new_per_file_errors) {
        file_to_errors_map.remove(uri);
    } else {
        file_to_errors_map.insert(uri.clone(), new_per_file_errors);
    }
    let mut dirty_files = state.dirty_files;
    if dirty {
        dirty_files.insert(uri.clone());
    }
    T {
        dirty_files,
        file_to_errors_map,
    }
}

// Helper function to modify the server errors for a specific file
fn modify_server_errors(
    uri: &Url,
    new_errors: Errors,
    state: T,
    f: impl FnOnce(ServerErrors, SplitErrors) -> ServerErrors,
) -> T {
    modify_per_file_errors(uri, state, |per_file_errors| {
        let new_split = split(new_errors);
        let new_server_errors = f(per_file_errors.server_errors.clone(), new_split);
        PerFileErrors {
            server_errors: new_server_errors,
            ..per_file_errors
        }
    })
}

// We've parsed a file locally and now want to record the number of parse errors for this file
pub fn set_live_parse_errors_and_send(
    send_json: &mut dyn FnMut(serde_json::Value),
    uri: &Url,
    live_parse_errors: Errors,
    state: T,
) -> T {
    // If the caller passes in some non-parse errors then we'll just ignore them
    let live_parse_errors: Errors = live_parse_errors
        .into_iter()
        .filter(is_parse_error)
        .collect();
    let state = modify_per_file_errors(uri, state, |per_file_errors| PerFileErrors {
        live_parse_errors: Some(ParseErrors(live_parse_errors)),
        ..per_file_errors
    });
    send_all_errors(send_json, state)
}

// We've run check-contents on a modified open file and now want to record the errors reported by
// check-contents
pub fn set_live_non_parse_errors_and_send(
    send_json: &mut dyn FnMut(serde_json::Value),
    uri: &Url,
    live_non_parse_errors: Errors,
    state: T,
) -> T {
    // If the caller passes in some parse errors then we'll just ignore them
    let live_non_parse_errors: Errors = live_non_parse_errors
        .into_iter()
        .filter(is_not_parse_error)
        .collect();
    let state = modify_per_file_errors(uri, state, |per_file_errors| PerFileErrors {
        live_non_parse_errors: Some(NonParseErrors(live_non_parse_errors)),
        ..per_file_errors
    });
    send_all_errors(send_json, state)
}

// When we close a file we clear all the live parse errors or non-parse errors for that file, but we
// keep around the server errors
pub fn clear_all_live_errors_and_send(
    send_json: &mut dyn FnMut(serde_json::Value),
    uri: &Url,
    state: T,
) -> T {
    let state = modify_per_file_errors(uri, state, |per_file_errors| PerFileErrors {
        live_parse_errors: None,
        live_non_parse_errors: None,
        ..per_file_errors
    });
    send_all_errors(send_json, state)
}

// my_list @ [] returns a list which is no longer physically identical to my_list. This is a
// workaround
fn append(list_a: Errors, list_b: Errors) -> Errors {
    match (list_a.is_empty(), list_b.is_empty()) {
        (true, true) => vec![],
        (true, _) => list_b,
        (_, true) => list_a,
        _ => {
            let mut result = list_a;
            result.extend(list_b);
            result
        }
    }
}

// During recheck we stream in errors from the server. These will replace finalized server errors
// from a previous recheck or add to streamed server errors from this recheck
pub fn add_streamed_server_errors_and_send(
    send_json: &mut dyn FnMut(serde_json::Value),
    uri_to_error_map: BTreeMap<Url, Errors>,
    state: T,
) -> T {
    // When a recheck streams in new errors, we stop showing the old finalized errors
    // Streamed errors are additive
    let mut state = state;
    for (uri, new_errors_unsplit) in uri_to_error_map {
        state = modify_server_errors(
            &uri,
            new_errors_unsplit,
            state,
            |server_errors, new_errors| match server_errors {
                ServerErrors::Finalized(_) => ServerErrors::Streamed(new_errors),
                ServerErrors::Streamed(existing_errors) => {
                    let (
                        ParseErrors(existing_parse_errors),
                        NonParseErrors(existing_non_parse_errors),
                    ) = existing_errors;
                    let (ParseErrors(new_parse_errors), NonParseErrors(new_non_parse_errors)) =
                        new_errors;
                    ServerErrors::Streamed((
                        ParseErrors(append(existing_parse_errors, new_parse_errors)),
                        NonParseErrors(append(existing_non_parse_errors, new_non_parse_errors)),
                    ))
                }
            },
        );
    }
    send_all_errors(send_json, state)
}

// After recheck we get all the errors from the server. This replaces whatever server errors we
// already had.
pub fn set_finalized_server_errors_and_send(
    send_json: &mut dyn FnMut(serde_json::Value),
    uri_to_error_map: BTreeMap<Url, Errors>,
    state: T,
) -> T {
    // At the end of the recheck, the finalized errors will replace either the errors from
    // the previous recheck or the streamed errors.
    let mut state = state;
    let mut files_with_new_errors: BTreeSet<Url> = BTreeSet::new();
    for (uri, new_errors_unsplit) in uri_to_error_map {
        state = modify_server_errors(&uri, new_errors_unsplit, state, |_, new_errors| {
            ServerErrors::Finalized(new_errors)
        });
        files_with_new_errors.insert(uri);
    }
    // All the errors in uri_to_error_map have been added to state. But uri_to_error_map doesn't
    // include files which used to have >0 errors but now have 0 errors. So we need to go through
    // every file that used to have errors and clear them out
    let uris_to_clear: Vec<Url> = state
        .file_to_errors_map
        .keys()
        .filter(|uri| !files_with_new_errors.contains(*uri))
        .cloned()
        .collect();
    for uri in uris_to_clear {
        state = modify_server_errors(&uri, vec![], state, |_, cleared_errors| {
            ServerErrors::Finalized(cleared_errors)
        });
    }
    let extra_dirty: Vec<Url> = state
        .file_to_errors_map
        .iter()
        .filter_map(|(uri, per_file)| {
            let parse_from_server = per_file.live_parse_errors.is_none();
            let non_parse_from_server = per_file.live_non_parse_errors.is_none();
            let (parse, non_parse) = choose_errors(per_file);
            if (parse_from_server && !parse.is_empty())
                || (non_parse_from_server && !non_parse.is_empty())
            {
                Some(uri.clone())
            } else {
                None
            }
        })
        .collect();
    for uri in extra_dirty {
        state.dirty_files.insert(uri);
    }
    send_all_errors(send_json, state)
}

// When the Flow server dies, LSP must clear all the errors.
// TODO: Don't clear live parse errors. Those don't require the server, so we can still keep
//       providing them
pub fn clear_all_errors_and_send(send_json: &mut dyn FnMut(serde_json::Value), state: T) -> T {
    let uris: Vec<Url> = state.file_to_errors_map.keys().cloned().collect();
    let mut state = state;
    for uri in uris {
        state = modify_per_file_errors(&uri, state, |_| empty_per_file_errors());
    }
    send_all_errors(send_json, state)
}

fn pos_compare(p1: &Position, p2: &Position) -> i32 {
    if p1.line < p2.line {
        -1
    } else if p1.line > p2.line {
        1
    } else {
        p1.character as i32 - p2.character as i32
    }
}

enum RangeOverlap {
    SelectionBeforeStartOfSquiggle,
    SelectionOverlapsStartOfSquiggle,
    SelectionCoversWholeSquiggle,
    SelectionInMiddleOfSquiggle,
    SelectionOverlapsEndOfSquiggle,
    SelectionAfterEndOfSquiggle,
}

fn get_range_overlap(selection: &Range, squiggle: &Range) -> RangeOverlap {
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

struct RangeReplace {
    remove_range: Range,
    insert_lines: u32,
    insert_chars_on_final_line: u32,
}

fn update_pos_due_to_prior_replace(p: &Position, replace: &RangeReplace) -> Position {
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

fn update_range_due_to_replace(squiggle: &Range, replace: &RangeReplace) -> Option<Range> {
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

fn update_diagnostics_due_to_change(
    diagnostics: Errors,
    change: &lsp_types::DidChangeTextDocumentParams,
) -> Errors {
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
        diagnostic_opt: Option<Diagnostic>,
        replace_opt: &Option<RangeReplace>,
    ) -> Option<Diagnostic> {
        match (diagnostic_opt, replace_opt) {
            (Some(diagnostic), Some(replace)) => {
                let range = update_range_due_to_replace(&diagnostic.range, replace);
                range.map(|range| Diagnostic {
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
    let apply_all_replaces = |diagnostic: Diagnostic| -> Option<Diagnostic> {
        replaces.iter().try_fold(diagnostic, |diagnostic, replace| {
            apply_replace(Some(diagnostic), replace)
        })
    };
    diagnostics
        .into_iter()
        .filter_map(apply_all_replaces)
        .collect()
}

// Basically a best-effort attempt to update the locations of errors after a didChange
pub fn update_errors_due_to_change_and_send(
    send_json: &mut dyn FnMut(serde_json::Value),
    params: &lsp_types::DidChangeTextDocumentParams,
    state: T,
) -> T {
    let uri = &params.text_document.uri;
    let state = modify_per_file_errors(uri, state, |per_file_errors| {
        let PerFileErrors {
            live_parse_errors,
            live_non_parse_errors,
            server_errors,
        } = per_file_errors;
        let live_parse_errors = match live_parse_errors {
            None => None,
            Some(ParseErrors(v)) if v.is_empty() => Some(ParseErrors(v)),
            Some(ParseErrors(errs)) => {
                Some(ParseErrors(update_diagnostics_due_to_change(errs, params)))
            }
        };
        let live_non_parse_errors = match live_non_parse_errors {
            None => None,
            Some(NonParseErrors(v)) if v.is_empty() => Some(NonParseErrors(v)),
            Some(NonParseErrors(errs)) => Some(NonParseErrors(update_diagnostics_due_to_change(
                errs, params,
            ))),
        };
        let server_errors = match server_errors {
            ServerErrors::Streamed((ParseErrors(pe), NonParseErrors(npe)))
                if pe.is_empty() && npe.is_empty() =>
            {
                ServerErrors::Streamed((ParseErrors(pe), NonParseErrors(npe)))
            }
            ServerErrors::Finalized((ParseErrors(pe), NonParseErrors(npe)))
                if pe.is_empty() && npe.is_empty() =>
            {
                ServerErrors::Finalized((ParseErrors(pe), NonParseErrors(npe)))
            }
            ServerErrors::Streamed((
                ParseErrors(parse_errors),
                NonParseErrors(non_parse_errors),
            )) => {
                let parse_errors = update_diagnostics_due_to_change(parse_errors, params);
                let non_parse_errors = update_diagnostics_due_to_change(non_parse_errors, params);
                ServerErrors::Streamed((
                    ParseErrors(parse_errors),
                    NonParseErrors(non_parse_errors),
                ))
            }
            ServerErrors::Finalized((
                ParseErrors(parse_errors),
                NonParseErrors(non_parse_errors),
            )) => {
                let parse_errors = update_diagnostics_due_to_change(parse_errors, params);
                let non_parse_errors = update_diagnostics_due_to_change(non_parse_errors, params);
                ServerErrors::Finalized((
                    ParseErrors(parse_errors),
                    NonParseErrors(non_parse_errors),
                ))
            }
        };
        PerFileErrors {
            live_parse_errors,
            live_non_parse_errors,
            server_errors,
        }
    });
    send_all_errors(send_json, state)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
    struct Error {
        uri: Url,
        kind: String,
        msg: String,
    }

    fn mk_clear_error(uri: &Url) -> Error {
        Error {
            uri: uri.clone(),
            kind: "<FAKE ERROR>".to_string(),
            msg: "Errors cleared for file".to_string(),
        }
    }

    // Build a mock lsp diagnostic
    fn mk_diagnostic(error: &Error) -> Diagnostic {
        Diagnostic {
            range: Range {
                start: Position {
                    line: 10,
                    character: 20,
                },
                end: Position {
                    line: 10,
                    character: 30,
                },
            },
            severity: Some(DiagnosticSeverity::ERROR),
            code: Some(NumberOrString::String(error.kind.clone())),
            source: Some("Flow".to_string()),
            message: error.msg.clone(),
            related_information: Some(vec![]),
            tags: Some(vec![]),
            code_description: None,
            data: None,
        }
    }

    // Take the json output and convert it back into a list of errors
    fn error_list_of_json_response(json: serde_json::Value) -> Vec<Error> {
        let uri_str = json["params"]["uri"].as_str().unwrap();
        let uri = Url::parse(uri_str).unwrap();
        let diagnostics = json["params"]["diagnostics"].as_array().unwrap();
        if diagnostics.is_empty() {
            vec![mk_clear_error(&uri)]
        } else {
            diagnostics
                .iter()
                .map(|d| {
                    let kind = d["code"].as_str().unwrap().to_string();
                    let msg = d["message"].as_str().unwrap().to_string();
                    Error {
                        uri: uri.clone(),
                        kind,
                        msg,
                    }
                })
                .collect()
        }
    }

    // Wraps some lspErrors calls and records what json is sent. Then asserts that all the expected
    // errors were sent
    fn with_assert_errors_match(
        reason: &str,
        expected: &[Error],
        f: impl FnOnce(&mut dyn FnMut(serde_json::Value)) -> T,
    ) -> T {
        let mut actual: Vec<Error> = Vec::new();
        let ret = f(&mut |json| {
            actual.extend(error_list_of_json_response(json));
        });
        let mut expected_sorted: Vec<Error> = expected.to_vec();
        expected_sorted.sort();
        actual.sort();
        assert_eq!(expected_sorted, actual, "{}", reason);
        ret
    }

    // Assert that NO json is sent. This is different than asserting that we sent a list of 0 errors
    fn assert_no_send(reason: &str) -> Box<dyn FnMut(serde_json::Value) + '_> {
        Box::new(move |_json| {
            panic!("Expected no send, but got a send for {:?}", reason);
        })
    }

    // Given an error list, group it by uri and convert to diagnostics
    fn map_of_error_list(error_list: &[Error]) -> BTreeMap<Url, Errors> {
        let mut map: BTreeMap<Url, Errors> = BTreeMap::new();
        for error in error_list.iter().rev() {
            let entry = map.entry(error.uri.clone()).or_default();
            entry.insert(0, mk_diagnostic(error));
        }
        map
    }

    fn path_to_foo() -> Url {
        Url::parse("file:///path/to/foo.js").unwrap()
    }

    fn path_to_bar() -> Url {
        Url::parse("file:///path/to/bar.js").unwrap()
    }

    fn foo_infer_error_1() -> Error {
        Error {
            uri: path_to_foo(),
            kind: "InferError".to_string(),
            msg: "Your code is broken 1".to_string(),
        }
    }

    fn foo_infer_error_2() -> Error {
        Error {
            uri: path_to_foo(),
            kind: "InferError".to_string(),
            msg: "Your code is broken 2".to_string(),
        }
    }

    fn foo_parse_error_1() -> Error {
        Error {
            uri: path_to_foo(),
            kind: "ParseError".to_string(),
            msg: "Your code no parse 1".to_string(),
        }
    }

    fn foo_parse_error_2() -> Error {
        Error {
            uri: path_to_foo(),
            kind: "ParseError".to_string(),
            msg: "Your code no parse 2".to_string(),
        }
    }

    fn bar_infer_error_1() -> Error {
        Error {
            uri: path_to_bar(),
            kind: "InferError".to_string(),
            msg: "Your code is broken 1".to_string(),
        }
    }

    fn bar_infer_error_2() -> Error {
        Error {
            uri: path_to_bar(),
            kind: "InferError".to_string(),
            msg: "Your code is broken 2".to_string(),
        }
    }

    fn bar_parse_error_1() -> Error {
        Error {
            uri: path_to_bar(),
            kind: "ParseError".to_string(),
            msg: "Your code no parse 1".to_string(),
        }
    }

    #[expect(
        dead_code,
        reason = "Defined in OCaml source but unused in test bodies"
    )]
    fn bar_parse_error_2() -> Error {
        Error {
            uri: path_to_bar(),
            kind: "ParseError".to_string(),
            msg: "Your code no parse 2".to_string(),
        }
    }

    #[test]
    fn clearing_errors_from_empty_is_a_no_op() {
        let reason = "Clearing all the errors when there are no errors should not send anything";
        clear_all_errors_and_send(&mut *assert_no_send(reason), empty());
    }

    #[test]
    fn test_clear_all_live_errors_and_send() {
        let reason =
            "Clearing the live errors from a file with no live errors should not send anything";
        let errors =
            clear_all_live_errors_and_send(&mut *assert_no_send(reason), &path_to_foo(), empty());

        let reason = "Setting the live parse errors for foo.js to 0 errors won't trigger a send";
        let errors = set_live_parse_errors_and_send(
            &mut *assert_no_send(reason),
            &path_to_foo(),
            vec![],
            errors,
        );

        let reason = "Setting server errors to 0 streamed errors should not send anything";
        let mut map = BTreeMap::new();
        map.insert(path_to_foo(), vec![]);
        let errors = add_streamed_server_errors_and_send(&mut *assert_no_send(reason), map, errors);

        let reason = "Clearing the live errors from a file with 0 streamed errors should not send";
        let errors =
            clear_all_live_errors_and_send(&mut *assert_no_send(reason), &path_to_foo(), errors);

        let reason =
            "Setting the live parse errors for foo.js to 0 errors again should trigger a send";
        let errors = set_live_parse_errors_and_send(
            &mut *assert_no_send(reason),
            &path_to_foo(),
            vec![],
            errors,
        );

        let reason =
            "Setting server errors to be 1 streamed non-parse error should send that error";
        let expected = [foo_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason =
            "Clearing the live errors from a file with 0 streamed parse errors should not send";
        let errors =
            clear_all_live_errors_and_send(&mut *assert_no_send(reason), &path_to_foo(), errors);

        let reason = "Setting the live parse errors for foo.js to 0 errors one more time should trigger a send";
        let errors = set_live_parse_errors_and_send(
            &mut *assert_no_send(reason),
            &path_to_foo(),
            vec![],
            errors,
        );

        let reason = "Adding a parse error to server errors should not trigger a send since we have live parse errors";
        let to_send = [foo_parse_error_1()];
        let errors = add_streamed_server_errors_and_send(
            &mut *assert_no_send(reason),
            map_of_error_list(&to_send),
            errors,
        );

        let reason =
            "Clearing the live errors from a file with 1 streamed parse errors will trigger a send";
        let expected = [foo_parse_error_1(), foo_infer_error_1()];
        let _errors = with_assert_errors_match(reason, &expected, |send| {
            clear_all_live_errors_and_send(send, &path_to_foo(), errors)
        });
    }

    #[test]
    fn finalized_errors_in_isolation() {
        let reason = "Setting the finalized server errors to empty when there already were no errors should not send anything";
        let errors = set_finalized_server_errors_and_send(
            &mut *assert_no_send(reason),
            map_of_error_list(&[]),
            empty(),
        );

        let reason = "Setting finalized server errors when there were no errors before should send all the finalized errors";
        let expected = [foo_infer_error_1(), bar_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Setting the exact same finalized server errors again will resend all errors";
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason =
            "Setting the finalized server errors to be 0 errors will clear errors for all files";
        let expected = [
            mk_clear_error(&path_to_foo()),
            mk_clear_error(&path_to_bar()),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&[]), errors)
        });

        let reason = "Putting an error in each file again";
        let expected = [foo_infer_error_1(), bar_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Setting the finalized server errors to only include foo.js will clear the errors for bar.js";
        let expected = [foo_infer_error_1(), mk_clear_error(&path_to_bar())];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(
                send,
                map_of_error_list(&[foo_infer_error_1()]),
                errors,
            )
        });

        let reason = "Putting an error in each file again";
        let expected = [foo_infer_error_1(), bar_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Clearing all errors will clear errors for all files";
        let expected = [
            mk_clear_error(&path_to_foo()),
            mk_clear_error(&path_to_bar()),
        ];
        let _errors = with_assert_errors_match(reason, &expected, |send| {
            clear_all_errors_and_send(send, errors)
        });
    }

    #[test]
    fn streamed_errors_in_isolation() {
        let reason =
            "Streaming in 0 server errors when there were no errors before will not do anything";
        let errors = add_streamed_server_errors_and_send(
            &mut *assert_no_send(reason),
            map_of_error_list(&[]),
            empty(),
        );

        let reason = "Streaming in server errors when there were no errors before should send all the newly streamed errors";
        let expected = [foo_infer_error_1(), bar_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Streaming in the same server errors again will again resend the errors for those files, but now each error will appear twice";
        let expected = [
            foo_infer_error_1(),
            foo_infer_error_1(),
            bar_infer_error_1(),
            bar_infer_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(
                send,
                map_of_error_list(&[foo_infer_error_1(), bar_infer_error_1()]),
                errors,
            )
        });

        let reason =
            "Streaming in 1 more error for foo.js will cause all of foo's errors to be resent";
        let expected = [
            foo_infer_error_1(),
            foo_infer_error_1(),
            foo_infer_error_2(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(
                send,
                map_of_error_list(&[foo_infer_error_2()]),
                errors,
            )
        });

        let reason = "Streaming in 0 errors will not trigger a send";
        let mut map = BTreeMap::new();
        map.insert(path_to_foo(), vec![]);
        let errors = add_streamed_server_errors_and_send(&mut *assert_no_send(reason), map, errors);

        let reason = "Clearing all errors will clear errors for all files";
        let expected = [
            mk_clear_error(&path_to_foo()),
            mk_clear_error(&path_to_bar()),
        ];
        let _errors = with_assert_errors_match(reason, &expected, |send| {
            clear_all_errors_and_send(send, errors)
        });
    }

    #[test]
    fn streamed_and_finalized_server_errors() {
        let reason = "Setting finalized server errors when there were no errors before should send all the finalized errors";
        let expected = [foo_infer_error_1(), bar_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), empty())
        });

        let reason =
            "Sending streamed errors for foo.js should replace the finalized errors for that file";
        let expected = [foo_infer_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Sending finalized errors for foo.js should replace the streamed errors and the old finalized errors";
        let expected = [bar_infer_error_2(), mk_clear_error(&path_to_foo())];
        let _errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(
                send,
                map_of_error_list(&[bar_infer_error_2()]),
                errors,
            )
        });
    }

    #[test]
    fn live_parse_errors_override_finalized_errors() {
        let reason = "Setting parse errors to 0 parse errors when there are no server errors doesn't trigger a send";
        let errors = set_live_parse_errors_and_send(
            &mut *assert_no_send(reason),
            &path_to_foo(),
            vec![],
            empty(),
        );

        let reason = "Setting finalized server errors when there were no errors before should send all the finalized errors";
        let expected = [
            foo_infer_error_1(),
            foo_parse_error_1(),
            bar_infer_error_1(),
            bar_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Setting live parse errors for foo.js will replace the known parse errors for that file";
        let expected = [foo_infer_error_1(), foo_parse_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_live_parse_errors_and_send(
                send,
                &path_to_foo(),
                vec![mk_diagnostic(&foo_parse_error_2())],
                errors,
            )
        });

        let reason = "Setting finalized server errors will still use the live parse errors";
        let to_set = [
            foo_infer_error_2(),
            foo_parse_error_1(),
            bar_infer_error_1(),
            bar_parse_error_1(),
        ];
        let expected = [
            foo_infer_error_2(),
            foo_parse_error_2(),
            bar_infer_error_1(),
            bar_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&to_set), errors)
        });

        let reason =
            "Clearing the live parse errors for foo.js will resend the server errors for that file";
        let expected = [foo_infer_error_2(), foo_parse_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            clear_all_live_errors_and_send(send, &path_to_foo(), errors)
        });

        let reason = "Clearing live errors again won't do anything";
        let errors =
            clear_all_live_errors_and_send(&mut *assert_no_send(reason), &path_to_foo(), errors);

        let reason = "Setting finalized server errors to only non-parse errors for foo.js and some parse errors for bar.js";
        let expected = [
            foo_infer_error_1(),
            bar_infer_error_1(),
            bar_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Setting live parse errors for foo.js to [] won't trigger a send since there are no server parse errors for foo.js";
        let errors = set_live_parse_errors_and_send(
            &mut *assert_no_send(reason),
            &path_to_foo(),
            vec![],
            errors,
        );

        let reason = "Setting live parse errors for bar.js to [] will trigger a send since there are server parse errors for bar.js";
        let expected = [bar_infer_error_1()];
        let _errors = with_assert_errors_match(reason, &expected, |send| {
            set_live_parse_errors_and_send(send, &path_to_bar(), vec![], errors)
        });
    }

    #[test]
    fn live_parse_errors_override_streamed_errors() {
        let reason = "Setting streamed server errors when there were no errors before should send all the streamed errors";
        let expected = [
            foo_infer_error_1(),
            foo_parse_error_1(),
            bar_infer_error_1(),
            bar_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(send, map_of_error_list(&expected), empty())
        });

        let reason = "Setting live parse errors for foo.js will replace the known parse errors for that file";
        let expected = [foo_infer_error_1(), foo_parse_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_live_parse_errors_and_send(
                send,
                &path_to_foo(),
                vec![mk_diagnostic(&foo_parse_error_2())],
                errors,
            )
        });

        let reason = "Streaming in parse and type errors will ignore the parse errors for now";
        let to_send = [foo_infer_error_2(), foo_parse_error_1()];
        let expected = [
            foo_infer_error_1(),
            foo_infer_error_2(),
            foo_parse_error_2(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(send, map_of_error_list(&to_send), errors)
        });

        let reason =
            "Clearing the live parse errors for foo.js will resend the server errors for that file";
        let expected = [
            foo_infer_error_1(),
            foo_infer_error_2(),
            foo_parse_error_1(),
            foo_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            clear_all_live_errors_and_send(send, &path_to_foo(), errors)
        });

        let reason = "Clearing live errors again won't do anything";
        let _errors =
            clear_all_live_errors_and_send(&mut *assert_no_send(reason), &path_to_foo(), errors);
    }

    #[test]
    fn live_non_parse_errors_override_finalized_errors() {
        let reason = "Setting live errors to 0 live errors when there are no server errors doesn't trigger a send";
        let errors = set_live_non_parse_errors_and_send(
            &mut *assert_no_send(reason),
            &path_to_foo(),
            vec![],
            empty(),
        );

        let reason = "Setting finalized server errors when there were no errors before should send all the finalized errors";
        let expected = [foo_infer_error_1(), bar_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&expected), errors)
        });

        let reason = "Setting live non_parse errors for foo.js will replace the server non_parse errors for that file";
        let to_send = [foo_parse_error_1(), foo_infer_error_2()];
        let expected = [foo_infer_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            let diagnostics: Vec<Diagnostic> = to_send.iter().map(mk_diagnostic).collect();
            set_live_non_parse_errors_and_send(send, &path_to_foo(), diagnostics, errors)
        });

        let reason = "Setting finalized server errors will still use the live errors for foo.js and the server errors for bar.js. But since the live errors aren't changing, we won't resend errors for foo.js";
        let to_set = [foo_infer_error_1(), bar_infer_error_2()];
        let expected = [bar_infer_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_finalized_server_errors_and_send(send, map_of_error_list(&to_set), errors)
        });

        let reason =
            "Clearing the live errors for foo.js will resend the server errors for that file";
        let expected = [foo_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            clear_all_live_errors_and_send(send, &path_to_foo(), errors)
        });

        let reason = "Clearing live errors again won't do anything";
        let _errors =
            clear_all_live_errors_and_send(&mut *assert_no_send(reason), &path_to_foo(), errors);
    }

    #[test]
    fn live_non_parse_errors_override_streamed_errors() {
        let reason = "Setting streamed server errors when there were no errors before should send all the streamed errors";
        let expected = [
            foo_infer_error_1(),
            foo_parse_error_1(),
            bar_infer_error_1(),
            bar_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(send, map_of_error_list(&expected), empty())
        });

        let reason = "Setting live non_parse errors for foo.js will replace the server non_parse errors for that file";
        let to_send = [foo_parse_error_2(), foo_infer_error_2()];
        let expected = [foo_parse_error_1(), foo_infer_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            let diagnostics: Vec<Diagnostic> = to_send.iter().map(mk_diagnostic).collect();
            set_live_non_parse_errors_and_send(send, &path_to_foo(), diagnostics, errors)
        });

        let reason = "Streaming in more non_parse errors for foo.js will not trigger a send";
        let to_send = [foo_infer_error_1()];
        let errors = add_streamed_server_errors_and_send(
            &mut *assert_no_send(reason),
            map_of_error_list(&to_send),
            errors,
        );

        let reason = "Streaming in a parse error for foo.js will trigger a send";
        let to_send = [foo_parse_error_1()];
        let expected = [
            foo_infer_error_2(),
            foo_parse_error_1(),
            foo_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            add_streamed_server_errors_and_send(send, map_of_error_list(&to_send), errors)
        });

        let reason =
            "Clearing the live parse errors for foo.js will resend the server errors for that file";
        let expected = [
            foo_infer_error_1(),
            foo_infer_error_1(),
            foo_parse_error_1(),
            foo_parse_error_1(),
        ];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            clear_all_live_errors_and_send(send, &path_to_foo(), errors)
        });

        let reason = "Clearing live errors again won't do anything";
        let _errors =
            clear_all_live_errors_and_send(&mut *assert_no_send(reason), &path_to_foo(), errors);
    }

    #[test]
    fn live_parse_errors_and_live_non_parse_errors() {
        let reason = "Setting live non-parse errors for foo.js will replace all the server errors for that file and filter out parse errors";
        let to_send = [foo_infer_error_1(), foo_parse_error_1()];
        let expected = [foo_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            let diagnostics: Vec<Diagnostic> = to_send.iter().map(mk_diagnostic).collect();
            set_live_non_parse_errors_and_send(send, &path_to_foo(), diagnostics, empty())
        });

        let reason = "Setting live parse errors for foo.js will add to the live non-parse errors and filter out non-parse errors";
        let to_send = [foo_infer_error_2(), foo_parse_error_2()];
        let expected = [foo_infer_error_1(), foo_parse_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            let diagnostics: Vec<Diagnostic> = to_send.iter().map(mk_diagnostic).collect();
            set_live_parse_errors_and_send(send, &path_to_foo(), diagnostics, errors)
        });

        let reason = "Setting 0 live parse errors for foo.js will strip out the parse error that live errors shows";
        let expected = [foo_infer_error_1()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            set_live_parse_errors_and_send(send, &path_to_foo(), vec![], errors)
        });

        let reason = "Setting live errors for foo.js will not affect the live parse errors shown";
        let to_send = [foo_infer_error_2(), foo_parse_error_2()];
        let expected = [foo_infer_error_2()];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            let diagnostics: Vec<Diagnostic> = to_send.iter().map(mk_diagnostic).collect();
            set_live_non_parse_errors_and_send(send, &path_to_foo(), diagnostics, errors)
        });

        let reason = "Setting live errors for foo.js with ONLY parse errors when there are 0 live parse errors will clear the file";
        let to_send = [foo_parse_error_2()];
        let expected = [mk_clear_error(&path_to_foo())];
        let errors = with_assert_errors_match(reason, &expected, |send| {
            let diagnostics: Vec<Diagnostic> = to_send.iter().map(mk_diagnostic).collect();
            set_live_non_parse_errors_and_send(send, &path_to_foo(), diagnostics, errors)
        });

        let reason = "Setting live errors for foo.js with ONLY parse errors should NOT trigger a send if modulo parse errors there are 0 live errors before and 0 live errors after";
        let to_send = [foo_parse_error_2()];
        let diagnostics: Vec<Diagnostic> = to_send.iter().map(mk_diagnostic).collect();
        let _errors = set_live_non_parse_errors_and_send(
            &mut *assert_no_send(reason),
            &path_to_foo(),
            diagnostics,
            errors,
        );
    }
}
