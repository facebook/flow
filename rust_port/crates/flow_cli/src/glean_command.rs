/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use crate::codemod_command;
use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;
use crate::glean_runner;
fn spec() -> command_spec::Spec {
    codemod_command::codemod_common_spec(
        "glean",
        "",
        format!(
            "Usage: {} glean <path> --output-dir <dirname> --write-root <name>",
            command_utils::exe_name()
        ),
    )
    .flag(
        "--output-dir",
        &arg_spec::optional(arg_spec::string()),
        "Name of directory to output the JSON into",
        None,
    )
    .flag(
        "--write-root",
        &arg_spec::optional(arg_spec::string()),
        "Prefix to attach to file names (e.g. www)",
        None,
    )
    .flag(
        "--include-direct-deps",
        &arg_spec::truthy(),
        "Additionally index direct dependencies of input files",
        None,
    )
    .flag(
        "--include-reachable-deps",
        &arg_spec::truthy(),
        "Additionally index reachable dependencies of input files",
        None,
    )
    .flag(
        "--schema-version",
        &arg_spec::truthy(),
        "Show schema version used by the indexer",
        None,
    )
    .flag(
        "--glean-log",
        &arg_spec::truthy(),
        "Log extra information from Glean run",
        None,
    )
    .flag(
        "--glean-timeout",
        &arg_spec::required(Some("600".to_string()), arg_spec::string()),
        "Maximum time to wait per file, in seconds",
        None,
    )
}

fn main(args: &arg_spec::Values) {
    let output_dir_opt = command_spec::get(
        args,
        "--output-dir",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let write_root_opt = command_spec::get(
        args,
        "--write-root",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let include_direct_deps =
        command_spec::get(args, "--include-direct-deps", &arg_spec::truthy()).unwrap();
    let include_reachable_deps =
        command_spec::get(args, "--include-reachable-deps", &arg_spec::truthy()).unwrap();
    let show_schema_version =
        command_spec::get(args, "--schema-version", &arg_spec::truthy()).unwrap();
    let glean_log = command_spec::get(args, "--glean-log", &arg_spec::truthy()).unwrap();
    let glean_timeout = codemod_command::parse_i32_flag(
        Some(
            command_spec::get(
                args,
                "--glean-timeout",
                &arg_spec::required(Some("600".to_string()), arg_spec::string()),
            )
            .unwrap(),
        ),
        "--glean-timeout",
        600,
    );

    if show_schema_version {
        println!("{}", glean_runner::ALL_SCHEMA_VERSION);
        return;
    }

    if glean_timeout < 0 {
        eprintln!(
            "--glean-timeout must be a positive integer, or 0 to disable. Got {}",
            glean_timeout
        );
        flow_common_exit_status::exit(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
        );
    }

    let prepared = codemod_command::prepare_codemod(args, false);

    match (output_dir_opt, &write_root_opt) {
        (Some(output_dir), Some(write_root)) => {
            let output_dir = PathBuf::from(output_dir);
            if !output_dir.exists() || !output_dir.is_dir() {
                eprintln!("Output directory doesn't exist. Create it.");
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
                );
            } else if std::fs::read_dir(&output_dir).unwrap().next().is_some() {
                eprintln!("Output directory is nonempty. Empty it.");
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
                );
            } else {
                glean_runner::make(
                    Some(output_dir),
                    write_root.clone(),
                    include_direct_deps,
                    include_reachable_deps,
                    glean_log,
                    glean_timeout,
                    &prepared,
                );
            }
        }
        (None, _) => {
            println!("--output-dir is not specified. This will be a dry-run.");
            let write_root = write_root_opt.unwrap_or_else(|| "default".to_string());
            glean_runner::make(
                None,
                write_root,
                include_direct_deps,
                include_reachable_deps,
                glean_log,
                glean_timeout,
                &prepared,
            );
        }
        _ => {
            eprintln!("--output-dir and --write-root are required.");
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            );
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
