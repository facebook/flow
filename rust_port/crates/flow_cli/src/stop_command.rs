/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use flow_server_files::server_files_js;

use crate::command_connect;
use crate::command_spec;
use crate::command_utils;

//***********************************************************************
// flow stop command
//***********************************************************************

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "stop",
        "Stops a Flow server",
        "Usage: flow stop [OPTION]... [ROOT]\nStops a flow server\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be current directory if unspecified\n".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
    )
    .flag(
        "--temp-dir",
        &command_spec::optional(command_spec::string()),
        "Temp directory",
        None,
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    .flag(
        "--quiet",
        &command_spec::truthy(),
        "Quiet mode",
        None,
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

fn main(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let temp_dir = command_spec::get(
        args,
        "--temp-dir",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let quiet = command_spec::get(args, "--quiet", &command_spec::truthy()).unwrap();
    let root_arg = command_spec::get(
        args,
        "root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let tmp_dir = temp_dir.unwrap_or_else(|| {
        std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned())
    });

    if !quiet {
        eprintln!("Trying to connect to server for `{}`", root.display());
    }

    match command_connect::connect_and_make_request(
        &flowconfig_name,
        &tmp_dir,
        &root,
        command_connect::ServerRequest::Shutdown,
    ) {
        Ok(_) => {
            if !quiet {
                eprintln!(
                    "Told server for `{}` to die. Waiting for confirmation...",
                    root.display()
                );
            }
            let lock_path = server_files_js::lock_file(&flowconfig_name, &tmp_dir, &root);
            let socket_path = server_files_js::socket_file(&flowconfig_name, &tmp_dir, &root);
            let start = std::time::Instant::now();
            let timeout = std::time::Duration::from_secs(5);
            while (Path::new(&lock_path).exists() || Path::new(&socket_path).exists())
                && start.elapsed() <= timeout
            {
                std::thread::sleep(std::time::Duration::from_millis(50));
            }
            if Path::new(&lock_path).exists() || Path::new(&socket_path).exists() {
                eprintln!("Failed to kill server nicely for `{}`", root.display());
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::KillError)
            }
            if !quiet {
                eprintln!("Successfully killed server for `{}`", root.display());
            }
        }
        Err(command_connect::ConnectError::ServerNotRunning) => {
            if !quiet {
                eprintln!("Warning: no server to kill for `{}`", root.display());
            }
        }
        Err(command_connect::ConnectError::ServerSocketMissing) => {
            if !quiet {
                eprintln!("Warning: no server to kill for `{}`", root.display());
            }
        }
        Err(err) => {
            eprintln!("{}", err);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::KillError)
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
