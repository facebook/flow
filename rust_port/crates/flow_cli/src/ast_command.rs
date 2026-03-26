/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

use std::io::Read;

use flow_parser::PERMISSIVE_PARSE_OPTIONS;
use flow_parser::ParseOptions;
use flow_parser::estree_translator;
use flow_parser::file_key::FileKey;
use flow_parser::file_key::FileKeyInner;
use flow_parser::offset_utils;

use crate::command_spec;

fn get_file_content(path: Option<String>, filename: Option<&str>) -> (Option<String>, String) {
    match filename {
        Some(filename) => {
            let content = std::fs::read_to_string(filename).unwrap_or_else(|e| {
                eprintln!("Error reading file {}: {}", filename, e);
                std::process::exit(1);
            });
            (Some(filename.to_string()), content)
        }
        None => {
            let mut content = String::new();
            std::io::stdin()
                .read_to_string(&mut content)
                .unwrap_or_else(|e| {
                    eprintln!("Error reading stdin: {}", e);
                    std::process::exit(1);
                });
            (path, content)
        }
    }
}

fn main_impl(args: &command_spec::Values) {
    let _include_tokens = command_spec::get(args, "--tokens", &command_spec::truthy()).unwrap();
    let pretty = command_spec::get(args, "--pretty", &command_spec::truthy()).unwrap();
    let check = command_spec::get(args, "--check", &command_spec::truthy()).unwrap();
    let _debug = command_spec::get(args, "--debug", &command_spec::truthy()).unwrap();
    let _pattern = command_spec::get(args, "--pattern", &command_spec::truthy()).unwrap();
    let file_type_opt = command_spec::get(
        args,
        "--type",
        &command_spec::optional(command_spec::enum_flag(vec![
            ("js", AstFileType::FileJs),
            ("json", AstFileType::FileJson),
        ])),
    )
    .unwrap();
    let use_strict = command_spec::get(args, "--strict", &command_spec::truthy()).unwrap();
    let no_enums = command_spec::get(args, "--no-enums", &command_spec::truthy()).unwrap();
    let no_component_syntax =
        command_spec::get(args, "--no-component-syntax", &command_spec::truthy()).unwrap();
    let include_comments = command_spec::get(
        args,
        "--include-comments",
        &command_spec::required(
            Some(AstIncludeComments::CommentsTrue),
            command_spec::enum_flag(vec![
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
        &command_spec::required(Some(true), command_spec::bool_flag()),
    )
    .unwrap();
    let path = command_spec::get(
        args,
        "--path",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let filename = command_spec::get(
        args,
        "file",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let use_relative_path = filename
        .as_ref()
        .is_some_and(|f| !std::path::Path::new(f).is_absolute());
    let (file_path, content) = get_file_content(path, filename.as_deref());

    let file_type = match file_type_opt {
        Some(t) => t,
        None => match &filename {
            Some(f) if f.ends_with(".json") => AstFileType::FileJson,
            _ => AstFileType::FileJs,
        },
    };

    let parse_options = Some(ParseOptions {
        components: !no_component_syntax,
        enums: !no_enums,
        use_strict,
        ..PERMISSIVE_PARSE_OPTIONS
    });

    let display_filename = if use_relative_path {
        file_path.map(|f| {
            let cwd = std::env::current_dir().unwrap_or_default();
            std::path::Path::new(&f)
                .strip_prefix(&cwd)
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or(f)
        })
    } else {
        file_path
    };

    let offset_table = offset_utils::OffsetTable::make(&content);
    let config = estree_translator::Config {
        include_locs,
        include_filename: true,
    };

    let results = match file_type {
        AstFileType::FileJs => {
            let filekey = display_filename
                .as_ref()
                .map(|s| FileKey::new(FileKeyInner::SourceFile(s.clone())));
            let (mut ast, errors) = match filekey {
                Some(fk) => flow_parser::parse_program_file::<()>(
                    false,
                    None,
                    parse_options,
                    fk,
                    Ok(&content),
                ),
                None => flow_parser::parse_program_without_file(
                    false,
                    None,
                    parse_options,
                    Ok(&content),
                ),
            };

            if check {
                serde_json::json!({
                    "errors": estree_translator::errors(&offset_table, config.include_filename, &errors),
                })
            } else {
                match include_comments {
                    AstIncludeComments::CommentsTrue => {}
                    AstIncludeComments::CommentsFalse => {
                        flow_parser::comment_utils::strip_all_comments(&mut ast, false);
                    }
                    AstIncludeComments::CommentsDocblock => {
                        flow_parser::comment_utils::strip_all_comments(&mut ast, true);
                    }
                }

                let translated_ast = estree_translator::program(&offset_table, &config, &ast);

                match translated_ast {
                    serde_json::Value::Object(mut obj) => {
                        obj.insert(
                            "errors".to_owned(),
                            estree_translator::errors(
                                &offset_table,
                                config.include_filename,
                                &errors,
                            ),
                        );
                        serde_json::Value::Object(obj)
                    }
                    _ => unreachable!("expected JSON object from program translation"),
                }
            }
        }
        AstFileType::FileJson => {
            let filekey = display_filename
                .as_ref()
                .map(|s| FileKey::new(FileKeyInner::JsonFile(s.clone())));
            let (mut ast, errors) =
                flow_parser::parse_json_file(false, None, parse_options, filekey, Ok(&content));

            if check {
                serde_json::json!({
                    "errors": estree_translator::errors(&offset_table, config.include_filename, &errors),
                })
            } else {
                match include_comments {
                    AstIncludeComments::CommentsTrue => {}
                    AstIncludeComments::CommentsFalse | AstIncludeComments::CommentsDocblock => {
                        flow_parser::comment_utils::strip_inlined_comments_expression(&mut ast);
                    }
                }

                let translated_ast = estree_translator::expression(&offset_table, &config, &ast);

                match translated_ast {
                    serde_json::Value::Object(mut obj) => {
                        obj.insert(
                            "errors".to_owned(),
                            estree_translator::errors(
                                &offset_table,
                                config.include_filename,
                                &errors,
                            ),
                        );
                        serde_json::Value::Object(obj)
                    }
                    _ => unreachable!("expected JSON object from expression translation"),
                }
            }
        }
    };

    if pretty {
        println!("{}", serde_json::to_string_pretty(&results).unwrap());
    } else {
        println!("{}", serde_json::to_string(&results).unwrap());
    }
}

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "ast",
        "Print the AST",
        "Usage: flow ast [OPTION]... [FILE]\n\nPrint the AST".to_string(),
    )
    .flag(
        "--tokens",
        &command_spec::truthy(),
        "Include a list of syntax tokens in the output",
        None,
    )
    .flag(
        "--pretty",
        &command_spec::truthy(),
        "Pretty-print JSON output",
        None,
    )
    .flag(
        "--check",
        &command_spec::truthy(),
        "Checks whether the file parses, returning any errors but not the AST",
        None,
    )
    .flag("--debug", &command_spec::truthy(), "", None)
    .flag(
        "--pattern",
        &command_spec::truthy(),
        "Prints the AST structurally without locations to be used in pattern matching",
        None,
    )
    .flag(
        "--type",
        &command_spec::optional(command_spec::enum_flag(vec![
            ("js", AstFileType::FileJs),
            ("json", AstFileType::FileJson),
        ])),
        "Type of input file (js or json)",
        None,
    )
    .flag(
        "--strict",
        &command_spec::truthy(),
        "Parse in strict mode",
        None,
    )
    .flag(
        "--no-enums",
        &command_spec::truthy(),
        "Disable enum support",
        None,
    )
    .flag(
        "--no-component-syntax",
        &command_spec::truthy(),
        "Disable support for component syntax",
        None,
    )
    .flag(
        "--include-comments",
        &command_spec::required(
            Some(AstIncludeComments::CommentsTrue),
            command_spec::enum_flag(vec![
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
        &command_spec::required(Some(true), command_spec::bool_flag()),
        "Include or drop location information in the output (default: true)",
        None,
    )
    .flag(
        "--offset-style",
        &command_spec::optional(command_spec::string()),
        "Offset style for JSON output",
        None,
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    .flag(
        "--path",
        &command_spec::optional(command_spec::string()),
        "Specify (fake) path to file when reading data from stdin",
        None,
    )
    .anon("file", &command_spec::optional(command_spec::string()))
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main_impl)
}
