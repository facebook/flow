/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Read;

use crate::command_connect;
use crate::command_connect::ServerRequest;
use crate::command_connect::ServerResponse;
use crate::command_spec;
use crate::command_utils;

//***********************************************************************
// flow check-contents command
//***********************************************************************

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "check-contents",
        "Run typechecker on contents from stdin",
        "Usage: flow check-contents [OPTION]... [FILE]\n\nRuns a flow check on the contents of stdin. If FILE is provided, then\ncheck-contents pretends that the contents of stdin come from FILE\n".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
    )
    .flag(
        "--strip-root",
        &command_spec::truthy(),
        "Print paths without the root",
        None,
    )
    .flag(
        "--ignore-version",
        &command_spec::truthy(),
        "Ignore the version constraint in .flowconfig",
        Some("FLOW_IGNORE_VERSION"),
    )
    .flag(
        "--no-flowlib",
        &command_spec::truthy(),
        "Do not use the bundled flowlib",
        Some("NO_FLOWLIB"),
    )
    .flag(
        "--root",
        &command_spec::optional(command_spec::string()),
        "Project root directory containing the .flowconfig",
        None,
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    .flag(
        "--wait-for-recheck",
        &command_spec::optional(command_spec::string()),
        "Wait for recheck before responding (accepted but ignored)",
        None,
    )
    .flag(
        "--all",
        &command_spec::truthy(),
        "Ignore absence of an @flow pragma",
        None,
    )
    .anon("filename", &command_spec::optional(command_spec::string()))
}

fn main(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &command_spec::truthy()).unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let no_flowlib = command_spec::get(args, "--no-flowlib", &command_spec::truthy()).unwrap();
    let root_arg = command_spec::get(
        args,
        "--root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let all = command_spec::get(args, "--all", &command_spec::truthy()).unwrap();
    let filename = command_spec::get(
        args,
        "filename",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let mut content = String::new();
    std::io::stdin()
        .read_to_string(&mut content)
        .expect("failed to read stdin");

    let root_hint = root_arg.as_deref().or(filename.as_deref());
    let root = command_utils::guess_root(&flowconfig_name, root_hint);
    let options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        command_utils::MakeOptionsOverrides::default(),
    );

    match command_connect::connect_and_make_request(
        &flowconfig_name,
        options.temp_dir.as_str(),
        &root,
        ServerRequest::CheckContents {
            filename,
            content,
            force: all,
            include_warnings: false,
            strip_root,
        },
    ) {
        Ok(ServerResponse::CheckContents {
            has_errors,
            warning_count,
            error_output,
            not_covered,
        }) => {
            if not_covered {
                println!("File is not @flow!");
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError);
            }
            if has_errors || warning_count > 0 {
                print!("{}", error_output);
                let empty = flow_common_errors::error_utils::ConcreteLocPrintableErrorSet::empty();
                let warnings =
                    flow_common_errors::error_utils::ConcreteLocPrintableErrorSet::empty();
                // Return a successful exit code if there were only warnings.
                let exit_status = if has_errors {
                    flow_common_exit_status::FlowExitStatus::TypeError
                } else {
                    command_utils::get_check_or_status_exit_code(&empty, &warnings, None)
                };
                flow_common_exit_status::exit(exit_status);
            }
            println!("No errors!");
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError);
        }
        Ok(ServerResponse::Error { message }) => {
            eprintln!("Error: {}", message);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Ok(response) => {
            eprintln!("Unexpected response from server: {:?}", response);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Err(err) => {
            eprintln!("{}", err);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
