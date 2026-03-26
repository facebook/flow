/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;

pub const SIGIL: &str = "AUTO332";

pub const SIGIL_LEN: usize = SIGIL.len();

pub mod canonical {
    use flow_parser::loc::Loc;

    pub struct Token {
        pub cursor: Loc,
        pub prefix_and_suffix: Option<(String, String)>,
    }

    pub fn cursor(token: &Token) -> &Loc {
        &token.cursor
    }

    pub fn mk(span: &Loc, prefix: String, suffix: String) -> Token {
        Token {
            cursor: span.start_loc(),
            prefix_and_suffix: Some((prefix, suffix)),
        }
    }

    pub fn to_relative_token(canon: Option<&Token>, token: &str) -> String {
        match canon {
            Some(Token {
                prefix_and_suffix: Some((prefix, suffix)),
                ..
            }) => format!("{}{}{}", prefix, token, suffix),
            _ => token.to_string(),
        }
    }
}

pub fn add(
    source: Option<&flow_parser::file_key::FileKey>,
    contents: &str,
    loc_line: i32,
    column: usize,
) -> (String, String, Option<canonical::Token>) {
    let line = (loc_line - 1) as usize;
    let (contents_with_sigil, canon_token) =
        match flow_common_utils::line::split_nth(contents, line) {
            None => (contents.to_string(), None),
            Some((pre, line_str, post)) => {
                let length = line_str.len();
                let line_str = if length >= column {
                    let start = &line_str[..column];
                    let end_ = &line_str[column..];
                    format!("{}{}{}", start, SIGIL, end_)
                } else {
                    line_str.to_string()
                };
                let (line_str, canon_token) =
                    match flow_parser::find_ident(|s| s.contains(SIGIL), &line_str) {
                        Some((loc, pattern)) => {
                            // Since we parsed this as a single line, we need to adjust line and source.
                            let index_of_pattern = pattern
                                .find(SIGIL)
                                .expect("sigil must be present in identifier");
                            let prefix = pattern[..index_of_pattern].to_string();
                            let suffix = pattern[index_of_pattern + SIGIL_LEN..].to_string();
                            let loc = {
                                Loc {
                                    source: source.cloned(),
                                    start: Position {
                                        line: loc_line,
                                        ..loc.start
                                    },
                                    end: Position {
                                        line: loc_line,
                                        ..loc.end
                                    },
                                }
                            };
                            (
                                line_str.replacen(pattern.as_str(), SIGIL, 1),
                                Some(canonical::mk(&loc, prefix, suffix)),
                            )
                        }
                        None => (line_str, None),
                    };
                (format!("{}{}{}", pre, line_str, post), canon_token)
            }
        };
    let broader_context = {
        let f = |(_, x, _)| x;
        let default = "";
        line.checked_sub(1)
            .and_then(|l| flow_common_utils::line::split_nth(&contents_with_sigil, l))
            .map(f)
            .unwrap_or(default)
            .to_string()
            + flow_common_utils::line::split_nth(&contents_with_sigil, line)
                .map(f)
                .unwrap_or(default)
            + flow_common_utils::line::split_nth(&contents_with_sigil, line + 1)
                .map(f)
                .unwrap_or(default)
    };
    (contents_with_sigil, broader_context, canon_token)
}

pub fn remove_from_loc(canon: Option<&canonical::Token>, loc: &Loc) -> Loc {
    if loc.start.line == loc.end.line {
        let pad_len = match canon {
            Some(canonical::Token {
                prefix_and_suffix: Some((prefix, suffix)),
                ..
            }) => prefix.len() as i32 + suffix.len() as i32,
            _ => 0,
        };
        Loc {
            source: loc.source.dupe(),
            start: loc.start,
            end: Position {
                column: loc.end.column - SIGIL_LEN as i32 + pad_len,
                ..loc.end
            },
        }
    } else {
        Loc {
            source: loc.source.dupe(),
            start: loc.start,
            end: loc.start,
        }
    }
}

pub fn remove_opt(s: &str) -> Option<(&str, &str)> {
    match s.find(SIGIL) {
        None => None,
        Some(idx) => {
            let before = &s[..idx];
            let after = &s[idx + SIGIL_LEN..];
            Some((before, after))
        }
    }
}

pub fn remove(s: &str) -> (&str, &str) {
    match remove_opt(s) {
        None => (s, ""),
        Some(split) => split,
    }
}

pub fn split_opt(s: &str) -> Option<(String, String)> {
    remove_opt(s).map(|(before, after)| (before.to_string(), format!("{}{}", SIGIL, after)))
}

pub fn extract_cursor(contents: &str) -> Option<(String, (i32, i32))> {
    match remove_opt(contents) {
        None => None,
        Some((before, after)) => {
            let offset = before.len();
            let cursor = flow_common_utils::line::position_of_offset(contents, offset)?;
            let contents = format!("{}{}", before, after);
            Some((contents, cursor))
        }
    }
}
