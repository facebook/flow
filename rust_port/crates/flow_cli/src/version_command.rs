/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::CStr;
use std::path::Path;
use std::sync::OnceLock;

use flow_common::flow_version;
use flow_common_xx as xx;

use crate::command_spec;

unsafe extern "C" {
    static BuildInfo_kRevision: *const std::ffi::c_char;
}

fn build_revision() -> String {
    unsafe {
        if BuildInfo_kRevision.is_null() {
            String::new()
        } else {
            CStr::from_ptr(BuildInfo_kRevision)
                .to_string_lossy()
                .into_owned()
        }
    }
}

fn flow_build_id(executable: &Path) -> String {
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

fn print_semver(json: bool, pretty: bool) {
    if json || pretty {
        let json = serde_json::json!({
            "semver": flow_version::VERSION,
        });
        if pretty {
            println!("{}", serde_json::to_string_pretty(&json).unwrap());
        } else {
            println!("{}", serde_json::to_string(&json).unwrap());
        }
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
        if pretty {
            println!("{}", serde_json::to_string_pretty(&json).unwrap());
        } else {
            println!("{}", serde_json::to_string(&json).unwrap());
        }
    } else {
        println!("{}", binary);
    }
}

fn main_impl(args: &command_spec::Values) {
    let json = command_spec::get(args, "--json", &command_spec::truthy()).unwrap();
    let pretty = command_spec::get(args, "--pretty", &command_spec::truthy()).unwrap();
    let binary = command_spec::get(args, "--binary", &command_spec::truthy()).unwrap();
    let semver = command_spec::get(args, "--semver", &command_spec::truthy()).unwrap();

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
            "build_id": build_revision(),
            "flow_build_id": executable.as_deref().map(flow_build_id).unwrap_or_default(),
        });
        if pretty {
            println!("{}", serde_json::to_string_pretty(&json).unwrap());
        } else {
            println!("{}", serde_json::to_string(&json).unwrap());
        }
    } else {
        crate::command_utils::print_version();
    };
    // Exit.(exit No_error)
    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError);
}

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "version",
        "Print version information",
        "Usage: flow version [OPTION]... [ROOT]\n\nPrint version information".to_string(),
    )
    .flag(
        "--json",
        &command_spec::truthy(),
        "Output results in JSON format",
        None,
    )
    .flag(
        "--pretty",
        &command_spec::truthy(),
        "Pretty-print JSON output (implies --json)",
        None,
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    .flag(
        "--binary",
        &command_spec::truthy(),
        "Return only the binary",
        None,
    )
    .flag(
        "--semver",
        &command_spec::truthy(),
        "Return only the version number",
        None,
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main_impl)
}
