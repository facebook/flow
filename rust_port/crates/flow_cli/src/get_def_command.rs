/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_common_errors::error_utils;
use flow_server_env::server_prot;
use serde_json::json;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// ***********************************************************************
// flow get-def command
// ***********************************************************************

fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_utils::add_base_flags(command_spec::Spec::new(
        "get-def",
        "Gets the definition location of a variable or property",
        format!(
            "Usage: {exe_name} get-def [OPTION]... [FILE] LINE COLUMN\n\ne.g. {exe_name} get-def foo.js 12 3\nor   {exe_name} get-def 12 3 < foo.js\n"
        ),
    ));
    let spec = command_utils::add_connect_and_json_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_path_flag(spec);
    let spec = command_utils::add_wait_for_recheck_flag(spec);
    spec.anon("args", &arg_spec::list_of(arg_spec::string()))
}

fn parse_args(
    path: Option<&str>,
    args: Vec<String>,
) -> (flow_server_utils::file_input::FileInput, i32, i32) {
    use flow_server_utils::file_input::FileInput;
    let (file, line, column) = match args.as_slice() {
        [file, line, column] => {
            let file = command_utils::expand_path(file);
            (
                FileInput::FileName(file),
                line.parse::<i32>().unwrap(),
                column.parse::<i32>().unwrap(),
            )
        }
        [line, column] => (
            command_utils::get_file_from_filename_or_stdin("get-def", None, path),
            line.parse::<i32>().unwrap(),
            column.parse::<i32>().unwrap(),
        ),
        _ => {
            let msg = command_spec::command(spec(), |_| {}).string_of_usage();
            flow_common_exit::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
                Some(&msg),
            );
        }
    };
    let (line, column) = command_utils::convert_input_pos(line, column);
    (file, line, column)
}

// get-def command handler.
// - json toggles JSON output
// - strip_root toggles whether output positions are relativized w.r.t. root
// - path is a user-specified path to use as incoming content source path
// - args is mandatory command args; see parse_args above
fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let connect_flags = command_utils::get_connect_flags(args);
    let json_flags = command_utils::get_json_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let path = command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let wait_for_recheck = command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let raw_args = command_spec::get(args, "args", &arg_spec::list_of(arg_spec::string()))
        .unwrap()
        .unwrap_or_default();
    let (input, line, column) = parse_args(path.as_deref(), raw_args);
    let root = command_utils::guess_root(
        &base_flags.flowconfig_name,
        match root_arg.as_deref() {
            Some(root) => Some(root),
            None => input.path_of_file_input(),
        },
    );
    let strip_root = if strip_root {
        Some(root.to_string_lossy().to_string())
    } else {
        None
    };
    let request = server_prot::request::Command::GET_DEF {
        input: input.clone(),
        line,
        r#char: column,
        wait_for_recheck,
    };
    let response = command_utils::connect_and_make_request(
        &base_flags.flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::GET_DEF(Ok(locs)) => {
            // format output
            if json_flags.json || json_flags.pretty {
                // TODO: this format is deprecated but can't be backwards-compatible.
                // should be replaced with just `Reason.json_of_loc loc`.
                let json = match locs.as_slice() {
                    [loc] => serde_json::Value::Object(
                        error_utils::deprecated_json_props_of_loc(strip_root.as_deref(), loc)
                            .into_iter()
                            .collect(),
                    ),
                    locs => serde_json::Value::Array(
                        locs.iter()
                            .map(|loc| {
                                serde_json::Value::Object(
                                    error_utils::deprecated_json_props_of_loc(
                                        strip_root.as_deref(),
                                        loc,
                                    )
                                    .into_iter()
                                    .collect(),
                                )
                            })
                            .collect(),
                    ),
                };
                flow_hh_json::print_json_endline(json_flags.pretty, &json);
            } else {
                let from = crate::flow_event_logger::get_from_i_am_a_clown();
                if from.as_deref() == Some("vim") || from.as_deref() == Some("emacs") {
                    for loc in &locs {
                        println!(
                            "{}",
                            error_utils::vim_emacs_output::string_of_loc(
                                strip_root.as_deref(),
                                loc,
                            )
                        );
                    }
                } else {
                    for loc in &locs {
                        println!(
                            "{}",
                            flow_common::reason::range_string_of_loc(strip_root.as_deref(), loc)
                        );
                    }
                }
            }
        }
        server_prot::response::Response::GET_DEF(Err(error)) => {
            if json_flags.json || json_flags.pretty {
                flow_hh_json::print_json_endline(json_flags.pretty, &json!({ "error": error }));
            } else {
                let file_str = match input.filename_of_file_input() {
                    "-" => "-".to_string(),
                    s => flow_common::reason::string_of_source(
                        strip_root.as_deref(),
                        &flow_parser::file_key::FileKey::source_file_of_absolute(s),
                    ),
                };
                eprintln!(
                    "Could not get definition for {}:{}:{}\n{}",
                    file_str, line, column, error
                );
            }
        }
        response => {
            command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
