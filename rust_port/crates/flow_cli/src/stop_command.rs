/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_server_env::socket_handshake;

use crate::command_connect_simple as CCS;
use crate::command_connect_simple::CCSError;
use crate::command_connect_simple::MismatchBehavior;
use crate::command_mean_kill;
use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// ***********************************************************************
// flow stop command
// ***********************************************************************

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "stop",
        "Stops a Flow server",
        command_spec::Visibility::Public,
        "Usage: flow stop [OPTION]... [ROOT]\nStops a flow server\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be current directory if unspecified\n".to_string(),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = spec.flag(
        "--temp-dir",
        &arg_spec::optional(arg_spec::string()),
        "Temp directory",
        None,
    );
    let spec = command_utils::add_from_flag(spec);
    spec.flag("--quiet", &arg_spec::truthy(), "Quiet mode", None)
        .anon("root", &arg_spec::optional(arg_spec::string()))
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let temp_dir =
        command_spec::get(args, "--temp-dir", &arg_spec::optional(arg_spec::string())).unwrap();
    let quiet = command_spec::get(args, "--quiet", &arg_spec::truthy()).unwrap();
    let root_arg =
        command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let root = root.canonicalize().unwrap_or(root);
    let tmp_dir = temp_dir.unwrap_or_else(|| {
        std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned())
    });

    if !quiet {
        eprintln!("Trying to connect to server for `{}`", root.display());
    }

    let client_handshake = (
        socket_handshake::ClientToMonitor1 {
            client_build_id: socket_handshake::build_revision(),
            client_version: flow_common::flow_version::VERSION.to_string(),
            is_stop_request: true,
            server_should_hangup_if_still_initializing: false,
            version_mismatch_strategy: socket_handshake::VersionMismatchStrategy::AlwaysStopServer,
        },
        socket_handshake::ClientToMonitor2 {
            client_type: socket_handshake::ClientType::Ephemeral,
        },
    );

    let attempt_mean_kill = || match command_mean_kill::mean_kill(&flowconfig_name, &tmp_dir, &root)
    {
        Ok(()) => {
            if !quiet {
                eprintln!("Successfully killed server for `{}`", root.display());
            }
        }
        Err(command_mean_kill::FailedToKill::Message(err)) => {
            if !quiet {
                match err {
                    Some(err) => eprintln!("{}", err),
                    None => {
                        eprintln!("Failed to kill server meanly for `{}`", root.display());
                        flow_common_exit_status::exit(
                            flow_common_exit_status::FlowExitStatus::KillError,
                        );
                    }
                }
            }
        }
    };

    match CCS::connect_once(&flowconfig_name, &client_handshake, &tmp_dir, &root) {
        Ok(_) => {
            if !quiet {
                eprintln!(
                    "Told server for `{}` to die. Waiting for confirmation...",
                    root.display()
                );
            }
            let mut i = 0;
            while command_mean_kill::server_exists(&flowconfig_name, &tmp_dir, &root) {
                i += 1;
                if i < 5 {
                    std::thread::sleep(std::time::Duration::from_secs(1));
                } else {
                    eprintln!("Failed to kill server nicely for `{}`", root.display());
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::KillError,
                    )
                }
            }
            if !quiet {
                eprintln!("Successfully killed server for `{}`", root.display());
            }
        }
        Err(CCSError::ServerMissing) => {
            if !quiet {
                eprintln!("Warning: no server to kill for `{}`", root.display());
            }
        }
        Err(CCSError::BuildIdMismatch(MismatchBehavior::ServerExited)) => {
            if !quiet {
                eprintln!("Successfully killed server for `{}`", root.display());
            }
        }
        Err(CCSError::BuildIdMismatch(MismatchBehavior::ClientShouldError { .. }))
        | Err(CCSError::ServerBusy(_))
        | Err(CCSError::ServerSocketMissing) => {
            if !quiet {
                eprintln!("Attempting to meanly kill server for `{}`", root.display());
            }
            attempt_mean_kill();
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
