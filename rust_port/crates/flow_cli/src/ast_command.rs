/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::VecDeque;
use std::io::Read;
use std::path::Path;

use flow_parser::PERMISSIVE_PARSE_OPTIONS;
use flow_parser::ParseOptions;
use flow_parser::estree_translator;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_server_utils::file_input::FileInput;
use serde_json::json;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// ***********************************************************************
// flow ast command
// ***********************************************************************

#[derive(Clone)]
enum AstFileType {
    FileJson,
    FileJs,
}

#[derive(Clone)]
enum AstIncludeComments {
    CommentsTrue,
    CommentsFalse,
    CommentsDocblock,
}

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_spec::Spec::new(
        "ast",
        "Print the AST",
        format!(
            "Usage: {exe_name} ast [OPTION]... [FILE]\n\ne.g. {exe_name} ast foo.js\nor   {exe_name} ast < foo.js\n"
        ),
    )
    .flag(
        "--tokens",
        &arg_spec::truthy(),
        "Include a list of syntax tokens in the output",
        None,
    )
    .flag(
        "--pretty",
        &arg_spec::truthy(),
        "Pretty-print JSON output",
        None,
    )
    .flag(
        "--check",
        &arg_spec::truthy(),
        "Checks whether the file parses, returning any errors but not the AST",
        None,
    )
    .flag(
        "--debug",
        &arg_spec::truthy(),
        "", // undocumented
        None,
    )
    .flag(
        "--pattern",
        &arg_spec::truthy(),
        "Prints the AST structurally without locations to be used in pattern matching",
        None,
    )
    .flag(
        "--type",
        &arg_spec::optional(arg_spec::enum_flag(vec![
            ("js", AstFileType::FileJs),
            ("json", AstFileType::FileJson),
        ])),
        "Type of input file (js or json)",
        None,
    )
    .flag(
        "--strict",
        &arg_spec::truthy(),
        "Parse in strict mode",
        None,
    )
    .flag(
        "--no-enums",
        &arg_spec::truthy(),
        "Disable enum support",
        None,
    )
    .flag(
        "--no-component-syntax",
        &arg_spec::truthy(),
        "Disable support for component syntax",
        None,
    )
    .flag(
        "--include-comments",
        &arg_spec::required(
            Some(AstIncludeComments::CommentsTrue),
            arg_spec::enum_flag(vec![
                ("true", AstIncludeComments::CommentsTrue),
                ("false", AstIncludeComments::CommentsFalse),
                ("docblock-only", AstIncludeComments::CommentsDocblock),
            ]),
        ),
        "Include or drop comments in the output (true, false or docblock-only) (default: true)",
        None,
    )
    .flag(
        "--include-locs",
        &arg_spec::required(Some(true), arg_spec::bool_flag()),
        "Include or drop location information in the output (default: true)",
        None,
    );
    let spec = command_utils::add_offset_style_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_path_flag(spec);
    spec.anon("file", &arg_spec::optional(arg_spec::string()))
}

enum AstResultType {
    AstJson(flow_parser::ast::expression::Expression<flow_parser::loc::Loc, flow_parser::loc::Loc>),
    AstJs(flow_parser::ast::Program<flow_parser::loc::Loc, flow_parser::loc::Loc>),
}

fn get_file(path: Option<String>, filename: Option<String>) -> FileInput {
    match filename {
        Some(filename) => FileInput::FileName(command_utils::expand_path(&filename)),
        None => {
            let mut content = String::new();
            std::io::stdin()
                .read_to_string(&mut content)
                .expect("failed to read stdin");
            FileInput::FileContent(path, content)
        }
    }
}

fn offset(
    offset_table: &flow_parser::offset_utils::OffsetTable,
    position: flow_parser::loc::Position,
) -> usize {
    offset_table.offset(position).unwrap() as usize
}

fn translate_token(
    offset_table: &flow_parser::offset_utils::OffsetTable,
    token: &flow_parser::TokenSinkResult,
) -> serde_json::Value {
    let context = match token.token_context {
        flow_parser::LexMode::Normal => "normal",
        flow_parser::LexMode::Type => "type",
        flow_parser::LexMode::JsxTag => "jsxTag",
        flow_parser::LexMode::JsxChild => "jsxChild",
        flow_parser::LexMode::Regexp => "regexp",
    };
    json!({
        "type": token.token_kind.as_str(),
        "context": context,
        "loc": {
            "start": {
                "line": token.token_loc.start.line,
                "column": token.token_loc.start.column,
            },
            "end": {
                "line": token.token_loc.end.line,
                "column": token.token_loc.end.column,
            },
        },
        "range": [
            offset(offset_table, token.token_loc.start),
            offset(offset_table, token.token_loc.end),
        ],
        "value": token.token_kind.to_value(),
    })
}

fn main(args: &arg_spec::Values) {
    let include_tokens = command_spec::get(args, "--tokens", &arg_spec::truthy()).unwrap();
    let pretty = command_spec::get(args, "--pretty", &arg_spec::truthy()).unwrap();
    let check = command_spec::get(args, "--check", &arg_spec::truthy()).unwrap();
    let debug = command_spec::get(args, "--debug", &arg_spec::truthy()).unwrap();
    let pattern = command_spec::get(args, "--pattern", &arg_spec::truthy()).unwrap();
    let file_type_opt = command_spec::get(
        args,
        "--type",
        &arg_spec::optional(arg_spec::enum_flag(vec![
            ("js", AstFileType::FileJs),
            ("json", AstFileType::FileJson),
        ])),
    )
    .unwrap();
    let use_strict = command_spec::get(args, "--strict", &arg_spec::truthy()).unwrap();
    let no_enums = command_spec::get(args, "--no-enums", &arg_spec::truthy()).unwrap();
    let no_component_syntax =
        command_spec::get(args, "--no-component-syntax", &arg_spec::truthy()).unwrap();
    let include_comments = command_spec::get(
        args,
        "--include-comments",
        &arg_spec::required(
            Some(AstIncludeComments::CommentsTrue),
            arg_spec::enum_flag(vec![
                ("true", AstIncludeComments::CommentsTrue),
                ("false", AstIncludeComments::CommentsFalse),
                ("docblock-only", AstIncludeComments::CommentsDocblock),
            ]),
        ),
    )
    .unwrap();
    let include_locs = command_spec::get(
        args,
        "--include-locs",
        &arg_spec::required(Some(true), arg_spec::bool_flag()),
    )
    .unwrap();
    let offset_style = command_utils::get_offset_style(args);
    let path = command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let filename =
        command_spec::get(args, "file", &arg_spec::optional(arg_spec::string())).unwrap();

    let use_relative_path = filename
        .as_ref()
        .is_some_and(|filename| Path::new(filename).is_relative());
    let file = get_file(path.clone(), filename.clone());
    let content = file.content_of_file_input_unsafe();
    let file_type = match file_type_opt {
        Some(t) => t,
        None => match filename.as_deref() {
            Some(filename) if flow_common::files::is_json_file(filename) => AstFileType::FileJson,
            Some(_) | None => AstFileType::FileJs,
        },
    };

    let offset_kind = command_utils::offset_kind_of_offset_style(offset_style);
    let offset_style = match offset_style {
        None | Some(command_utils::OffsetStyle::Utf8Bytes) => {
            estree_translator::OffsetStyle::Utf8Bytes
        }
        Some(command_utils::OffsetStyle::JsIndices) => estree_translator::OffsetStyle::JsIndices,
    };
    let config = estree_translator::Config {
        include_locs,
        include_filename: true,
        offset_style,
    };

    // Record token stream into a list when the --tokens flag is passed.
    // Note that tokens stream in in order, so the list is constructed in reverse
    // order.
    let mut tokens = VecDeque::new();
    let offset_table =
        flow_parser::offset_utils::OffsetTable::make_with_kind(offset_kind, &content);
    let mut token_sink = |token_data: flow_parser::TokenSinkResult| {
        tokens.push_front(translate_token(&offset_table, &token_data));
    };
    let token_sink = if !include_tokens {
        None
    } else {
        Some(&mut token_sink as &mut dyn FnMut(flow_parser::TokenSinkResult))
    };

    // Make the parser as permissive as possible.
    // TODO: make these CLI flags
    let parse_options = Some(ParseOptions {
        components: !no_component_syntax,
        enums: !no_enums,
        use_strict,
        ..PERMISSIVE_PARSE_OPTIONS
    });

    let filename = file.path_of_file_input().map(str::to_owned);
    let filename = if use_relative_path {
        filename.as_ref().map(|filename| {
            flow_common::files::relative_path(
                &std::env::current_dir().expect("failed to get current directory"),
                filename,
            )
        })
    } else {
        filename
    };

    let (ast, errors) = match file_type {
        AstFileType::FileJs => {
            // flow ast is a standalone parser - use the raw constructor
            // so paths are stored as-is without root stripping.
            let filekey = filename
                .clone()
                .map(|filename| FileKey::new(FileKeyInner::SourceFile(filename)));
            let (ocaml_ast, errors) = match filekey.clone() {
                Some(filekey) => flow_parser::parse_program_file::<()>(
                    false,
                    token_sink,
                    parse_options,
                    filekey,
                    Ok(&content),
                ),
                None => flow_parser::parse_program_without_file(
                    false,
                    token_sink,
                    parse_options,
                    Ok(&content),
                ),
            };

            if debug {
                eprintln!("{:?}", ocaml_ast);
            }
            if pattern {
                eprintln!("{:?}", ocaml_ast);
            }
            (AstResultType::AstJs(ocaml_ast), errors)
        }
        AstFileType::FileJson => {
            let filekey = filename
                .clone()
                .map(|filename| FileKey::new(FileKeyInner::JsonFile(filename)));
            let (ocaml_ast, errors) = flow_parser::parse_json_file(
                false,
                token_sink,
                parse_options,
                filekey.clone(),
                Ok(&content),
            );

            if debug {
                eprintln!("{:?}", ocaml_ast);
            }
            if pattern {
                eprintln!("{:?}", ocaml_ast);
            }
            (AstResultType::AstJson(ocaml_ast), errors)
        }
    };

    let tokens = tokens.into_iter().collect::<Vec<_>>();
    let results = if check {
        let mut obj = serde_json::Map::new();
        obj.insert(
            "errors".to_string(),
            estree_translator::errors(&offset_table, config.include_filename, &errors),
        );
        obj.insert("tokens".to_string(), serde_json::Value::Array(tokens));
        serde_json::Value::Object(obj)
    } else {
        let translated_ast = match ast {
            AstResultType::AstJs(mut ast) => {
                match include_comments {
                    AstIncludeComments::CommentsTrue => {}
                    AstIncludeComments::CommentsFalse => {
                        flow_parser::comment_utils::strip_all_comments(&mut ast, false);
                    }
                    AstIncludeComments::CommentsDocblock => {
                        flow_parser::comment_utils::strip_all_comments(&mut ast, true);
                    }
                }
                estree_translator::program(&offset_table, &config, &ast)
            }
            AstResultType::AstJson(mut ast) => {
                match include_comments {
                    AstIncludeComments::CommentsTrue => {}
                    AstIncludeComments::CommentsFalse | AstIncludeComments::CommentsDocblock => {
                        flow_parser::comment_utils::strip_inlined_comments_expression(&mut ast);
                    }
                }
                estree_translator::expression(&offset_table, &config, &ast)
            }
        };
        match translated_ast {
            serde_json::Value::Object(mut params) => {
                let errors_value =
                    estree_translator::errors(&offset_table, config.include_filename, &errors);
                let tokens_value = serde_json::Value::Array(tokens);
                let mut properties = serde_json::Map::new();
                properties.insert("errors".to_string(), errors_value);
                properties.insert("tokens".to_string(), tokens_value);
                properties.append(&mut params);
                serde_json::Value::Object(properties)
            }
            _ => panic!("expected translated_ast to be a JSON object"),
        }
    };
    flow_hh_json::print_json_endline(pretty, &results);
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
