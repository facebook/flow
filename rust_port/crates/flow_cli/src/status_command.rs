/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Port of statusCommands.ml (simplified - only the explicit Status variant)
// The OCaml version uses functors (Impl/Status/Check/Default) but we only port the
// explicit "flow status" command. The "flow check" command is separately ported in check_commands.rs.

use crate::command_connect;
use crate::command_connect::ServerRequest;
use crate::command_connect::ServerResponse;
use crate::command_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "status",
        "(default) Shows current Flow errors by asking the Flow server",
        "Usage: flow status [OPTION]... [ROOT]\n\nShows current Flow errors by asking the Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\nA server will be started if none is running over ROOT.\n\nStatus command options:".to_string(),
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
        "--show-all-errors",
        &command_spec::truthy(),
        "Print all errors (the default is to truncate after 50 errors)",
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
        None,
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    // Flags accepted for compatibility but ignored:
    .flag(
        "--retries",
        &command_spec::optional(command_spec::string()),
        "Number of retries (accepted but ignored)",
        None,
    )
    .flag(
        "--retry-if-init",
        &command_spec::optional(command_spec::string()),
        "Retry if initializing (accepted but ignored)",
        None,
    )
    .flag(
        "--timeout",
        &command_spec::optional(command_spec::string()),
        "Timeout in seconds for the server to respond",
        None,
    )
    .flag(
        "--no-auto-start",
        &command_spec::truthy(),
        "Don't auto-start the server",
        None,
    )
    .flag(
        "--temp-dir",
        &command_spec::optional(command_spec::string()),
        "Temp directory (accepted but ignored)",
        None,
    )
    .flag(
        "--quiet",
        &command_spec::truthy(),
        "Quiet mode (accepted but ignored)",
        None,
    )
    .flag(
        "--lazy",
        &command_spec::truthy(),
        "Lazy mode (accepted but ignored)",
        None,
    )
    .flag(
        "--include-warnings",
        &command_spec::truthy(),
        "Include warnings in the output",
        None,
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

// (* let main base_flags connect_flags json pretty json_version offset_style
//        error_flags strip_root version root () = *)
fn main(args: &command_spec::Values) {
    // (* let flowconfig_name = base_flags.Base_flags.flowconfig_name in *)
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let strip_root = command_spec::get(args, "--strip-root", &command_spec::truthy()).unwrap();
    let _show_all_errors =
        command_spec::get(args, "--show-all-errors", &command_spec::truthy()).unwrap();
    let no_flowlib = command_spec::get(args, "--no-flowlib", &command_spec::truthy()).unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let no_auto_start =
        command_spec::get(args, "--no-auto-start", &command_spec::truthy()).unwrap();
    let root_arg = command_spec::get(
        args,
        "root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    // (* let root = guess_root flowconfig_name root in *)
    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        command_utils::MakeOptionsOverrides::default(),
    );

    // (* let request = ServerProt.Request.STATUS { include_warnings } in *)
    // (* check_status flowconfig_name args connect_flags *)
    let include_warnings =
        command_spec::get(args, "--include-warnings", &command_spec::truthy()).unwrap();
    let timeout_str = command_spec::get(
        args,
        "--timeout",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let timeout_secs: Option<u64> = timeout_str.as_deref().and_then(|s| s.parse().ok());
    let mut attempted_autostart = false;
    let response = loop {
        match command_connect::connect_and_make_request_with_timeout(
            &flowconfig_name,
            options.temp_dir.as_str(),
            &root,
            ServerRequest::Status {
                include_warnings,
                strip_root,
            },
            timeout_secs,
        ) {
            Err(command_connect::ConnectError::ServerNotRunning)
                if !no_auto_start && !attempted_autostart =>
            {
                attempted_autostart = true;
                let _ = crate::start_command::start_server(
                    &flowconfig_name,
                    no_flowlib,
                    ignore_version,
                    true,
                    None,
                    false,
                    false,
                    None,
                    &root,
                );
                continue;
            }
            response => break response,
        }
    };

    match response {
        Ok(ServerResponse::Status {
            has_errors,
            warning_count,
            error_output,
            lazy_stats,
            ..
        }) => {
            let lazy_msg = if lazy_stats.lazy_mode {
                Some(format!(
                    "The Flow server is currently in lazy mode and is only checking {}/{} files.\nTo learn more, visit flow.org/en/docs/lang/lazy-modes",
                    lazy_stats.checked_files, lazy_stats.total_files
                ))
            } else {
                None
            };
            let has_warnings = warning_count > 0;
            if has_errors || has_warnings {
                print!("{}", error_output);
                if let Some(msg) = &lazy_msg {
                    println!("\n{}", msg);
                }
                if has_errors {
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::TypeError,
                    )
                } else {
                    // Warnings only — exit success (0) like OCaml
                    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError)
                }
            } else {
                println!("No errors!");
                if let Some(msg) = &lazy_msg {
                    println!("\n{}", msg);
                }
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError)
            }
        }
        Ok(ServerResponse::Error { message }) => {
            eprintln!("Error: {}", message);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Ok(response) => {
            eprintln!("Unexpected response from server: {:?}", response);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Err(command_connect::ConnectError::Timeout) => {
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::OutOfTime)
        }
        Err(command_connect::ConnectError::ServerNotRunning) => {
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
