/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_server_env::flow_lsp_conversions;
use flow_server_env::server_prot;
use flow_services_autocomplete::autocomplete_sigil;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn lsp_flag(spec: command_spec::Spec) -> command_spec::Spec {
    spec.flag(
        "--lsp",
        &arg_spec::truthy(),
        "Output results as LSP responses",
        None,
    )
}

// ***********************************************************************
// flow autocomplete command
// ***********************************************************************

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_utils::add_base_flags(command_spec::Spec::new(
        "autocomplete",
        "Queries autocompletion information",
        format!(
            "Usage: {exe_name} autocomplete [OPTION] [FILE] [LINE COLUMN]...\n\nQueries autocompletion information.\n\nIf line and column is specified, then the magic autocomplete token is\nautomatically inserted at the specified position.\n\nExample usage:\n\t{exe_name} autocomplete < foo.js\n\t{exe_name} autocomplete path/to/foo.js < foo.js\n\t{exe_name} autocomplete 12 35 < foo.js\n"
        ),
    ));
    let spec = command_utils::add_connect_and_json_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_wait_for_recheck_flag(spec);
    let spec = lsp_flag(spec);
    spec.flag(
        "--imports",
        &arg_spec::truthy(),
        "Include suggestions that can be imported from other files",
        None,
    )
    .flag(
        "--imports-ranked-usage",
        &arg_spec::truthy(),
        // experimental: rank imports by usage
        "",
        None,
    )
    .flag(
        "--show-ranking-info",
        &arg_spec::truthy(),
        "Show internal ranking info (for debugging)",
        None,
    )
    .anon("args", &arg_spec::list_of(arg_spec::string()))
}

// legacy editor integrations inserted the "AUTO332" token themselves. modern ones give us the
// cursor location. this function converts the legacy input to the modern input.
fn extract_cursor(
    input: flow_server_utils::file_input::FileInput,
) -> (flow_server_utils::file_input::FileInput, Option<(i32, i32)>) {
    let contents = input.content_of_file_input_unsafe();
    match autocomplete_sigil::extract_cursor(&contents) {
        None => (input, None),
        Some((contents, cursor)) => {
            let input = flow_server_utils::file_input::FileInput::FileContent(
                input.path_of_file_input().map(str::to_string),
                contents,
            );
            (input, Some(cursor))
        }
    }
}

fn file_input_from_stdin(filename: Option<&str>) -> flow_server_utils::file_input::FileInput {
    command_utils::get_file_from_filename_or_stdin(spec().name.as_str(), filename, None)
}

fn parse_args(
    args: Option<Vec<String>>,
) -> (flow_server_utils::file_input::FileInput, Option<(i32, i32)>) {
    match args.as_deref() {
        None | Some([]) => {
            let input = file_input_from_stdin(None);
            extract_cursor(input)
        }
        Some([filename]) => {
            let input = file_input_from_stdin(Some(filename.as_str()));
            extract_cursor(input)
        }
        Some([line, column]) => {
            let cursor = command_utils::convert_input_pos(
                line.parse().expect("invalid line"),
                column.parse().expect("invalid column"),
            );
            let input = file_input_from_stdin(None);
            (input, Some(cursor))
        }
        Some([filename, line, column]) => {
            let cursor = command_utils::convert_input_pos(
                line.parse().expect("invalid line"),
                column.parse().expect("invalid column"),
            );
            let input = file_input_from_stdin(Some(filename.as_str()));
            (input, Some(cursor))
        }
        _ => {
            eprintln!(
                "{}",
                command_spec::command(spec(), |_| {}).string_of_usage()
            );
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            );
        }
    }
}

fn autocomplete_result_to_json(
    strip_root: &Option<std::path::PathBuf>,
    item: &server_prot::response::completion::CompletionItem,
) -> serde_json::Value {
    let name = &item.name;
    let _ = strip_root;
    serde_json::json!({
        "name": name,
        "type": item.itemDetail.clone().unwrap_or_default(),
    })
}

fn autocomplete_response_to_json(
    strip_root: &Option<std::path::PathBuf>,
    response: &server_prot::response::AutocompleteResponse,
) -> serde_json::Value {
    match response {
        Err(error) => serde_json::json!({
            "error": error,
            // TODO: remove this? kept for BC
            "result": [],
        }),
        Ok((completion, _ac_type)) => {
            let results = completion
                .items
                .iter()
                .map(|item| autocomplete_result_to_json(strip_root, item))
                .collect::<Vec<_>>();
            serde_json::json!({ "result": results })
        }
    }
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let connect_flags = command_utils::get_connect_flags(args);
    let json_flags = command_utils::get_json_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let wait_for_recheck = command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let lsp = command_spec::get(args, "--lsp", &arg_spec::truthy()).unwrap();
    let imports = command_spec::get(args, "--imports", &arg_spec::truthy()).unwrap();
    let imports_ranked_usage =
        command_spec::get(args, "--imports-ranked-usage", &arg_spec::truthy()).unwrap();
    let show_ranking_info =
        command_spec::get(args, "--show-ranking-info", &arg_spec::truthy()).unwrap();
    let raw_args = command_spec::get(args, "args", &arg_spec::list_of(arg_spec::string())).unwrap();

    let (input, cursor_opt) = parse_args(raw_args);
    let root = command_utils::guess_root(
        &base_flags.flowconfig_name,
        match root_arg.as_deref() {
            Some(root) => Some(root),
            None => input.path_of_file_input(),
        },
    );
    let strip_root = if strip_root { Some(root.clone()) } else { None };
    match cursor_opt {
        None => {
            if json_flags.json || json_flags.pretty {
                let value = serde_json::json!({ "result": [] });
                flow_hh_json::print_json_endline(json_flags.pretty, &value);
            }
        }
        Some(cursor) => {
            let request = server_prot::request::Command::AUTOCOMPLETE {
                input,
                cursor,
                trigger_character: None,
                wait_for_recheck,
                imports,
                imports_ranked_usage,
                show_ranking_info,
            };
            let response = command_utils::connect_and_make_request(
                &base_flags.flowconfig_name,
                &connect_flags,
                &root,
                &request,
            );
            match response {
                server_prot::response::Response::AUTOCOMPLETE(result) => {
                    if lsp {
                        if let Ok((completion, ac_type)) = result {
                            for (index, item) in completion.items.into_iter().enumerate() {
                                let lsp_item = flow_lsp_conversions::flow_completion_item_to_lsp(
                                    None,
                                    None,
                                    None,
                                    &ac_type,
                                    true,
                                    &|_| true,
                                    true,
                                    true,
                                    true,
                                    index,
                                    item,
                                );
                                let json_value = serde_json::to_value(&lsp_item)
                                    .expect("failed to serialize completion item");
                                flow_hh_json::print_json_endline(true, &json_value);
                            }
                        }
                    } else if json_flags.json || json_flags.pretty {
                        let value = autocomplete_response_to_json(&strip_root, &result);
                        flow_hh_json::print_json_endline(json_flags.pretty, &value);
                    } else {
                        match result {
                            Err(error) => eprintln!("Error: {}", error),
                            Ok((completion, _ac_type)) => {
                                for item in completion.items {
                                    println!(
                                        "{} {}",
                                        item.name,
                                        item.itemDetail.as_deref().unwrap_or_default()
                                    );
                                }
                            }
                        }
                    }
                }
                response => {
                    command_utils::failwith_bad_response(&request, &response);
                }
            }
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
