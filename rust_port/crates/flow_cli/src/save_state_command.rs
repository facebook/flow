/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::command_connect;
use crate::command_connect::SaveStateOut;
use crate::command_connect::ServerRequest;
use crate::command_connect::ServerResponse;
use crate::command_spec;
use crate::command_utils;

//***********************************************************************
// flow save-state command
//***********************************************************************

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "save-state",
        "Tell the server to create a saved-state file",
        "Usage: flow save-state [OPTION]...\n\ne.g. flow save-state --root path/to/root --out path/to/my_saved_state\n".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
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
        "--scm",
        &command_spec::truthy(),
        "Write to the expected path for the SCM fetcher",
        None,
    )
    .flag(
        "--out",
        &command_spec::optional(command_spec::string()),
        "The path to the new saved-state file",
        None,
    )
}

fn main(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let no_flowlib = command_spec::get(args, "--no-flowlib", &command_spec::truthy()).unwrap();
    let root_arg = command_spec::get(
        args,
        "--root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let scm = command_spec::get(args, "--scm", &command_spec::truthy()).unwrap();
    let out = command_spec::get(
        args,
        "--out",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        command_utils::MakeOptionsOverrides::default(),
    );

    let out = match (scm, out) {
        (false, None) => {
            eprintln!("--out or --scm is required");
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            )
        }
        (true, Some(_)) => {
            eprintln!("--out and --scm are mutually exclusive");
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            )
        }
        (true, None) => SaveStateOut::Scm,
        (false, Some(out)) => SaveStateOut::File(out),
    };

    match command_connect::connect_and_make_request(
        &flowconfig_name,
        options.temp_dir.as_str(),
        &root,
        ServerRequest::SaveState { out },
    ) {
        Ok(ServerResponse::SaveState { result: Ok(msg) }) => {
            println!("{}", msg);
        }
        Ok(ServerResponse::SaveState {
            result: Err(message),
        })
        | Ok(ServerResponse::Error { message }) => {
            eprintln!("Error: {}", message);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Ok(response) => {
            eprintln!("Unexpected response from server: {:?}", response);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Err(command_connect::ConnectError::ServerNotRunning) => {
            eprintln!("There is no Flow server running in '{}'", root.display());
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoServerRunning)
        }
        Err(command_connect::ConnectError::ServerSocketMissing) => {
            eprintln!("There is no Flow server running in '{}'", root.display());
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoServerRunning)
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
