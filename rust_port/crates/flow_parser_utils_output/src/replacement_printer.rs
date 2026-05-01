/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common_utils::list_utils;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::offset_utils::OffsetTable;
use flow_parser_utils::flow_ast_differ::NodeChanges;
use flow_server_utils::file_input::FileInput;

use crate::ast_diff_printer;
use crate::js_layout_generator::Opts;

pub type Patch = Vec<(usize, usize, String)>;

// Location patches retain all the information needed to send edits over the LSP
pub type LocPatch = Vec<(Loc, String)>;

pub fn show_patch(p: &Patch) -> String {
    list_utils::to_string(
        "",
        |(s, e, p): &(usize, usize, String)| {
            format!("Start: <{}> End: <{}> Patch: <{}>\n", s, e, p)
        },
        p,
    )
}

fn with_content_of_file_input<T>(file: &FileInput, f: impl FnOnce(&str) -> T) -> T {
    match file.content_of_file_input() {
        Ok(contents) => f(&contents),
        Err(_) => {
            let file_name = file.filename_of_file_input();
            let error_msg = format!(
                "Replacement_printer: Input file, \"{}\", couldn't be read.",
                file_name
            );
            panic!("{}", error_msg)
        }
    }
}

pub fn mk_loc_patch_ast_differ(opts: &Opts, diff: &NodeChanges) -> LocPatch {
    ast_diff_printer::edits_of_changes(opts, diff)
}

pub fn mk_patch_ast_differ(opts: &Opts, diff: &NodeChanges, content: &str) -> Patch {
    let offset_table = OffsetTable::make(content);
    let offset = |pos: Position| -> usize { offset_table.offset(pos).unwrap() as usize };
    mk_loc_patch_ast_differ(opts, diff)
        .into_iter()
        .map(|(loc, text)| (offset(loc.start), offset(loc.end), text))
        .collect()
}

pub fn mk_patch_ast_differ_unsafe(opts: &Opts, diff: &NodeChanges, file: &FileInput) -> Patch {
    with_content_of_file_input(file, |content| mk_patch_ast_differ(opts, diff, content))
}

pub fn print(patch: &Patch, content: &str) -> String {
    let mut patch_sorted = patch.clone();
    patch_sorted.sort_by(|(start_one, _, _), (start_two, _, _)| start_one.cmp(start_two));
    let file_end = content.len();
    // Apply the spans to the original text
    let (result_string_minus_end, last_span) = patch_sorted.iter().fold(
        (String::new(), 0usize),
        |(file, last), (start, end_, text)| {
            let file_curr = format!("{}{}{}", file, &content[last..*start], text);
            (file_curr, *end_)
        },
    );
    let last_span_to_end_size = file_end - last_span;
    if last_span_to_end_size == 0 {
        result_string_minus_end
    } else {
        format!(
            "{}{}",
            result_string_minus_end,
            &content[last_span..last_span + last_span_to_end_size]
        )
    }
}

pub fn print_unsafe(patch: &Patch, file: &FileInput) -> String {
    with_content_of_file_input(file, |content| print(patch, content))
}

pub fn loc_patch_to_patch(file_content: &str, loc_patch: &LocPatch) -> Patch {
    let offset_table = OffsetTable::make(file_content);
    let offset = |pos: Position| -> usize { offset_table.offset(pos).unwrap() as usize };
    loc_patch
        .iter()
        .map(|(loc, replacement)| (offset(loc.start), offset(loc.end), replacement.clone()))
        .collect()
}
