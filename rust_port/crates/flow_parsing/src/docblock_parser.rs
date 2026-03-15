/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::VecDeque;
use std::sync::Arc;

use dupe::Dupe;
use flow_common::docblock::Docblock;
use flow_common::docblock::FlowMode;
use flow_common::docblock::JsxPragma;
use flow_common::docblock::JsxRuntimePragma;
use flow_common::files::FileOptions;
use flow_common::files::platform_specific_extensions_and_indices_opt;
use flow_data_structure_wrapper::smol_str::FlowSmolStr;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::flow_lexer;
use flow_parser::lex_env::LexEnv;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser::logos_tokens::MainToken;
use flow_parser::parse_jsx_pragma_expression;
use flow_parser::token::TokenKind;
use lazy_static::lazy_static;
use logos::Logos;
use regex::Regex;

// Avoid lexing unbounded in perverse cases
pub const DOCBLOCK_MAX_TOKENS: usize = 10;

lazy_static! {
    static ref ATTRIBUTES_RX: Regex = Regex::new(r"[ \t\r\n\\*/]+").unwrap();
    static ref LINES_RX: Regex = Regex::new(r"(\r\n|\n|\r)").unwrap();
}

#[derive(Debug, Clone, PartialEq)]
pub enum DocblockErrorKind {
    MultipleFlowAttributes,
    InvalidFlowMode(String),
    MultipleJSXAttributes,
    InvalidJSXAttribute(Option<String>),
    MultipleJSXRuntimeAttributes,
    InvalidJSXRuntimeAttribute,
    InvalidSupportsPlatform(String),
    DisallowedSupportsPlatform,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DocblockError {
    pub loc: Loc,
    pub kind: DocblockErrorKind,
}

struct Attribute {
    loc: Loc,
    text: String,
}

fn extract_docblock(
    max_tokens: usize,
    file_options: &FileOptions,
    file_key: &FileKey,
    content: &str,
) -> (Vec<DocblockError>, Docblock) {
    // walks a list of words, returns a list of errors and the extracted info.
    // if @flow or @providesModule is found more than once, the first one is used
    // and an error is returned.
    fn parse_attributes(
        file_options: &FileOptions,
        file_key: &FileKey,
        info: &mut Docblock,
        errors: &mut Vec<DocblockError>,
        mut attributes: VecDeque<Attribute>,
    ) {
        while let Some(attr) = attributes.pop_front() {
            match attr.text.as_str() {
                "@flow" => {
                    if let Some(next_attr) = attributes.pop_front() {
                        if next_attr.text == "strict" {
                            if info.flow.is_some() {
                                errors.push(DocblockError {
                                    loc: attr.loc,
                                    kind: DocblockErrorKind::MultipleFlowAttributes,
                                });
                            } else {
                                info.flow = Some(FlowMode::OptInStrict);
                            }
                            continue;
                        } else if next_attr.text == "strict-local" {
                            if info.flow.is_some() {
                                errors.push(DocblockError {
                                    loc: attr.loc,
                                    kind: DocblockErrorKind::MultipleFlowAttributes,
                                });
                            } else {
                                info.flow = Some(FlowMode::OptInStrictLocal);
                            }
                            continue;
                        } else if attr.loc.lines_intersect(&next_attr.loc)
                            && !next_attr.text.starts_with('@')
                        {
                            if info.flow.is_some() {
                                errors.push(DocblockError {
                                    loc: attr.loc.dupe(),
                                    kind: DocblockErrorKind::MultipleFlowAttributes,
                                });
                            } else {
                                info.flow = Some(FlowMode::OptIn);
                            }
                            errors.push(DocblockError {
                                loc: next_attr.loc,
                                kind: DocblockErrorKind::InvalidFlowMode(next_attr.text.clone()),
                            });
                            continue;
                        } else {
                            // Put back the next attribute, we didn't consume it
                            attributes.push_front(next_attr);
                        }
                    }

                    if info.flow.is_some() {
                        errors.push(DocblockError {
                            loc: attr.loc,
                            kind: DocblockErrorKind::MultipleFlowAttributes,
                        });
                    } else {
                        info.flow = Some(FlowMode::OptIn);
                    }
                }
                "@noflow" => {
                    if info.flow.is_some() {
                        errors.push(DocblockError {
                            loc: attr.loc.dupe(),
                            kind: DocblockErrorKind::MultipleFlowAttributes,
                        });
                    } else {
                        info.flow = Some(FlowMode::OptOut);
                    }
                }
                "@preventMunge" => {
                    info.prevent_munge = true;
                }
                "@jsx" => {
                    if let Some(expr_attr) = attributes.pop_front() {
                        if info.jsx.is_some() {
                            errors.push(DocblockError {
                                loc: attr.loc.dupe(),
                                kind: DocblockErrorKind::MultipleJSXAttributes,
                            });
                        } else {
                            // The point of the padding is to make the parsed code line up
                            // with the comment in the original source
                            let padding = "\n".repeat((expr_attr.loc.start.line - 1) as usize)
                                + &" ".repeat(expr_attr.loc.start.column as usize);
                            let mut padded_expr = padding;
                            padded_expr.push_str(&expr_attr.text);

                            match parse_jsx_pragma_expression::<()>(
                                expr_attr.loc.source.dupe(),
                                &padded_expr,
                            ) {
                                Ok((jsx_expr, _)) => {
                                    info.jsx = Some(Arc::new(JsxPragma {
                                        raw: expr_attr.text,
                                        expression: jsx_expr,
                                    }));
                                }
                                Err(parse_errors) => {
                                    let first_error =
                                        parse_errors.first().map(|(_, e)| e.to_string());
                                    errors.push(DocblockError {
                                        loc: expr_attr.loc,
                                        kind: DocblockErrorKind::InvalidJSXAttribute(first_error),
                                    });
                                }
                            }
                        }
                    } else {
                        errors.push(DocblockError {
                            loc: attr.loc,
                            kind: DocblockErrorKind::InvalidJSXAttribute(None),
                        });
                    }
                }
                "@jsxRuntime" => {
                    if let Some(runtime_attr) = attributes.pop_front() {
                        if runtime_attr.text == "classic" {
                            if info.jsx_runtime.is_some() {
                                errors.push(DocblockError {
                                    loc: attr.loc,
                                    kind: DocblockErrorKind::MultipleJSXRuntimeAttributes,
                                });
                            } else {
                                info.jsx_runtime = Some(JsxRuntimePragma::Classic);
                            }
                        } else if runtime_attr.text == "automatic" {
                            if info.jsx_runtime.is_some() {
                                errors.push(DocblockError {
                                    loc: attr.loc,
                                    kind: DocblockErrorKind::MultipleJSXRuntimeAttributes,
                                });
                            } else {
                                info.jsx_runtime = Some(JsxRuntimePragma::Automatic);
                            }
                        } else {
                            errors.push(DocblockError {
                                loc: attr.loc,
                                kind: DocblockErrorKind::InvalidJSXRuntimeAttribute,
                            });
                        }
                    } else {
                        errors.push(DocblockError {
                            loc: attr.loc,
                            kind: DocblockErrorKind::InvalidJSXRuntimeAttribute,
                        });
                    }
                }
                "@supportsPlatform" => {
                    if let Some(platform_attr) = attributes.pop_front() {
                        if file_options.multi_platform {
                            let platform = &platform_attr.text;

                            if platform_specific_extensions_and_indices_opt(
                                file_options,
                                file_key.as_str(),
                            )
                            .is_some()
                            {
                                errors.push(DocblockError {
                                    loc: attr.loc.dupe(),
                                    kind: DocblockErrorKind::DisallowedSupportsPlatform,
                                });
                            } else if file_options
                                .multi_platform_extensions
                                .iter()
                                .any(|ext| ext.as_str() == format!(".{}", platform))
                            {
                                let platform_smol = FlowSmolStr::from(platform.as_str());
                                let mut existing_platforms =
                                    info.supports_platform.clone().unwrap_or_else(Vec::new);
                                if !existing_platforms.contains(&platform_smol) {
                                    existing_platforms.push(platform_smol);
                                }
                                info.supports_platform = Some(existing_platforms);
                            } else {
                                errors.push(DocblockError {
                                    loc: attr.loc.dupe(),
                                    kind: DocblockErrorKind::InvalidSupportsPlatform(
                                        platform.clone(),
                                    ),
                                });
                            }
                        }
                    }
                }
                _ => {}
            }
        }
    }

    fn calc_end(start: Position, s: &str) -> Position {
        let mut end = start;

        let mut last_pos = 0;
        for cap in LINES_RX.find_iter(s) {
            let text_len = (cap.start() - last_pos) as i32;
            end.column += text_len;
            let delim = cap.as_str();
            let line_incr = if delim == "\r" { 0 } else { 1 };
            end = Position {
                line: end.line + line_incr,
                column: 0,
            };

            last_pos = cap.end();
        }

        let remaining_len = (s.len() - last_pos) as i32;
        end.column += remaining_len;

        end
    }

    fn split(loc: Loc, text: &str) -> VecDeque<Attribute> {
        // Need to add 2 characters for the start of the comment
        let mut start = Position {
            line: loc.start.line,
            column: loc.start.column + 2,
        };
        let mut attributes = VecDeque::new();
        let mut last_pos = 0;

        for mat in ATTRIBUTES_RX.find_iter(text) {
            if mat.start() > last_pos {
                let part = &text[last_pos..mat.start()];
                let end = calc_end(start, part);
                attributes.push_back(Attribute {
                    loc: Loc {
                        source: loc.source.dupe(),
                        start,
                        end,
                    },
                    text: part.to_string(),
                });
                start = end;
            }
            let delim = mat.as_str();
            start = calc_end(start, delim);

            last_pos = mat.end();
        }

        if last_pos < text.len() {
            let part = &text[last_pos..];
            let end = calc_end(start, part);
            attributes.push_back(Attribute {
                loc: Loc {
                    source: loc.source.dupe(),
                    start,
                    end,
                },
                text: part.to_string(),
            });
        }

        attributes
    }

    fn string_of_comment(comment: &flow_parser::ast::Comment<Loc>) -> (Loc, Arc<str>) {
        (comment.loc.dupe(), comment.text.dupe())
    }

    // Consume tokens in the file until we get a comment. This is a hack to
    // support Nuclide, which needs 'use babel' as the first token due to
    // constraints with Atom (see https://github.com/atom/atom/issues/8416 for
    // more context). At some point this should change back to consuming only
    // the first token.
    let mut lexer = MainToken::lexer(content);
    let mut env = LexEnv::new(Some(file_key.dupe()), false);

    fn get_first_comment_contents(
        lexer: &mut logos::Lexer<MainToken>,
        env: &mut LexEnv,
        max_tokens: usize,
        i: usize,
    ) -> Option<Vec<(Loc, Arc<str>)>> {
        if i < max_tokens {
            let lex_result = flow_lexer::token(lexer, env);

            if !lex_result.comments.is_empty() {
                Some(
                    lex_result
                        .comments
                        .iter()
                        .take(max_tokens - i)
                        .map(string_of_comment)
                        .collect(),
                )
            } else {
                // Stop looking for docblocks if we see any tokens other than a
                // string or a semicolon (`"use babel";` or `"use strict";`) or
                // an interpreter directive (`#!/bin/env node`)
                match lex_result.token_kind {
                    TokenKind::TString(..)
                    | TokenKind::TSemicolon
                    | TokenKind::TInterpreter(..) => {
                        get_first_comment_contents(lexer, env, max_tokens, i + 1)
                    }
                    _ => None,
                }
            }
        } else {
            None
        }
    }

    match get_first_comment_contents(&mut lexer, &mut env, max_tokens, 0) {
        Some(comments) => {
            let mut errors = Vec::new();
            let mut info = Docblock::default();

            for (loc, text) in comments {
                let attrs = split(loc, &text);
                parse_attributes(file_options, file_key, &mut info, &mut errors, attrs);
            }

            (errors, info)
        }
        None => (Vec::new(), Docblock::default()),
    }
}

pub fn parse_docblock(
    max_tokens: usize,
    file_options: &FileOptions,
    file_key: &FileKey,
    content: &str,
) -> (Vec<DocblockError>, Docblock) {
    match file_key.inner() {
        FileKeyInner::ResourceFile(_) | FileKeyInner::JsonFile(_) => {
            (Vec::new(), Docblock::default())
        }
        _ => extract_docblock(max_tokens, file_options, file_key, content),
    }
}
