/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// ***********************************************************************
// flow ast command
// ***********************************************************************

use std::sync::OnceLock;

use flow_common::flow_version;
use flow_common_xx as xx;
use flow_server_env::socket_handshake;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;
fn spec() -> command_spec::Spec {
    let exe_name = command_utils::exe_name();
    let spec = command_spec::Spec::new(
        "version",
        "Print version information",
        format!(
            "Usage: {} version [OPTION]... [ROOT]\n\ne.g. {} version\nor   {} version --json\nor   {} version /path/to/root\n",
            exe_name, exe_name, exe_name, exe_name
        ),
    );
    let spec = command_utils::add_json_flags(spec);
    let spec = command_utils::add_from_flag(spec);
    spec.flag(
        "--binary",
        &arg_spec::truthy(),
        "Return only the binary",
        None,
    )
    .flag(
        "--semver",
        &arg_spec::truthy(),
        "Return only the version number",
        None,
    )
    .anon("root", &arg_spec::optional(arg_spec::string()))
}

fn print_semver(json: bool, pretty: bool) {
    if json || pretty {
        let json = serde_json::json!({
            "semver": flow_version::VERSION,
        });
        flow_hh_json::print_json_endline(pretty, &json);
    } else {
        println!("{}", flow_version::VERSION);
    }
}

fn print_binary(json: bool, pretty: bool) {
    let binary = std::env::current_exe()
        .map(|p| p.to_string_lossy().to_string())
        .unwrap_or_else(|_| "<unknown>".to_string());
    if json || pretty {
        let json = serde_json::json!({
            "binary": binary,
        });
        flow_hh_json::print_json_endline(pretty, &json);
    } else {
        println!("{}", binary);
    }
}

fn main(args: &arg_spec::Values) {
    let json_flags = command_utils::get_json_flags(args);
    let json = json_flags.json;
    let pretty = json_flags.pretty;
    let binary = command_spec::get(args, "--binary", &arg_spec::truthy()).unwrap();
    let semver = command_spec::get(args, "--semver", &arg_spec::truthy()).unwrap();

    if semver {
        print_semver(json, pretty);
    } else if binary {
        print_binary(json, pretty);
    } else if json || pretty {
        let executable = std::env::current_exe().ok();
        let binary_path = executable
            .as_ref()
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_else(|| "<unknown>".to_string());
        let json = serde_json::json!({
            "semver": flow_version::VERSION,
            "binary": binary_path,
            "build_id": socket_handshake::build_revision(),
            "flow_build_id": executable.as_deref().map(get_build_id).unwrap_or_default(),
        });
        flow_hh_json::print_json_endline(pretty, &json);
    } else {
        command_utils::print_version();
    };
    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError);
}

fn get_build_id(executable: &std::path::Path) -> String {
    static BUILD_ID: OnceLock<String> = OnceLock::new();

    BUILD_ID
        .get_or_init(|| {
            let contents = std::fs::read(executable).unwrap_or_else(|err| {
                panic!(
                    "failed to read executable at {} for flow build id: {}",
                    executable.display(),
                    err
                )
            });
            let mut state = xx::State::new(0);
            state.update(&contents);
            format!("{:016x}", state.digest())
        })
        .clone()
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
