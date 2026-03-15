/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_parser::loc::Loc;

// Process jsx text normalize via the following rules:
//  - Each line of the JSXText is trimmed of whitespace
//  - Empty or pure whitespace lines are removed
//  - Tabs are replaces with spaces
//  - The lines are joined together with spaces
//
// This is hard for two main reasons:
// 1. We can't use Str
// 2. It's not enough to trim the text, we also need to figure out the line and
//    column for the start and end of the text
pub fn trim_jsx_text(loc: Loc, value: &str) -> Option<(Loc, String)> {
    // Removes all the spaces from the beginning of the string *)
    fn prefix_trim(s: &str) -> &str {
        s.trim_start_matches(' ')
    }

    // Removes all the spaces from the end of the string
    fn suffix_trim(s: &str) -> &str {
        s.trim_end_matches(' ')
    }

    // Tabs get turned into spaces
    let value = value.replace('\t', " ");

    // The algorithm is line based, so split the string into lines
    let lines: Vec<&str> = value.lines().collect();
    let last_line_idx = lines.len().saturating_sub(1);
    let trimmed_lines: Vec<String> = lines
        .iter()
        .enumerate()
        .map(|(idx, line)| {
            // Remove the leading whitespace from every line but the first
            let line = if idx != 0 { prefix_trim(line) } else { *line };
            // Remove the trailing whitespace from every line but the last
            if idx != last_line_idx {
                suffix_trim(line).to_string()
            } else {
                line.to_string()
            }
        })
        .collect();

    // Figure out the first and last non-empty line, if there are any
    let mut first_and_last_non_empty: Option<(usize, usize)> = None;
    for (idx, line) in trimmed_lines.iter().enumerate() {
        if !line.is_empty() {
            first_and_last_non_empty = match first_and_last_non_empty {
                None => Some((idx, idx)),
                Some((first, _)) => Some((first, idx)),
            };
        }
    }

    // match first_and_last_non_empty with
    match first_and_last_non_empty {
        None => None,
        Some((first_line, last_line)) => {
            // Filter out empty lines and turn newlines into spaces
            let trimmed: String = trimmed_lines
                .iter()
                .filter(|line| !line.is_empty())
                .cloned()
                .collect::<Vec<_>>()
                .join(" ");

            let start_line = loc.start.line + first_line as i32;
            let end_line = loc.start.line + last_line as i32;

            // We want to know the column and offset for the first and last
            // non-whitespace characters. We can do that by figuring out what those
            // characters are and using String.index and String.rindex to search for
            // them
            let first_trimmed_line = &trimmed_lines[first_line];
            let last_trimmed_line = &trimmed_lines[last_line];
            let first_char = first_trimmed_line.chars().next().unwrap();
            let last_char = last_trimmed_line.chars().last().unwrap();

            // For column we just do a search within the line
            let start_column = lines[first_line].find(first_char).unwrap_or(0) as i32;
            let end_column = lines[last_line].rfind(last_char).unwrap_or(0) as i32 + 1;

            let start_column = if first_line == 0 {
                start_column + loc.start.column
            } else {
                start_column
            };
            let end_column = if last_line == 0 {
                end_column + loc.start.column
            } else {
                end_column
            };

            let new_loc = Loc {
                start: flow_parser::loc::Position {
                    line: start_line,
                    column: start_column,
                },
                end: flow_parser::loc::Position {
                    line: end_line,
                    column: end_column,
                },
                ..loc
            };

            Some((new_loc, trimmed))
        }
    }
}
