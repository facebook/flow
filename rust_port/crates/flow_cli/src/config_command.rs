/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::Path;

use crate::command_spec;
use crate::command_utils;
use crate::flowconfig;

fn config_file(flowconfig_name: &str, root: &Path) -> String {
    root.join(flowconfig_name).to_string_lossy().to_string()
}

fn flowconfig_multi_error(errors: &[(u32, String)]) -> ! {
    for (line, msg) in errors {
        eprintln!(".flowconfig:{} {}", line, msg);
    }
    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::InvalidFlowconfig);
}

fn json_of_issue(kind: &str, line: u32, msg: &str) -> serde_json::Value {
    serde_json::json!({
        "line": line,
        "message": msg,
        "level": kind,
    })
}

fn exit_with_json(pretty: bool, errors_json: serde_json::Value) -> ! {
    let json = serde_json::json!({
        "errors": errors_json,
        "flowVersion": flow_common::flow_version::VERSION,
        "exit": {
            "code": 8,
            "reason": "Invalid_flowconfig",
        },
    });
    if pretty {
        println!("{}", serde_json::to_string_pretty(&json).unwrap());
    } else {
        println!("{}", serde_json::to_string(&json).unwrap());
    }
    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::InvalidFlowconfig);
}

fn find_main(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let json = command_spec::get(args, "--json", &command_spec::truthy()).unwrap();
    let pretty = command_spec::get(args, "--pretty", &command_spec::truthy()).unwrap();
    let root_arg = command_spec::get(
        args,
        "root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let root = root.to_string_lossy().to_string();
    if json || pretty {
        let json = serde_json::json!({ "root": root });
        if pretty {
            println!("{}", serde_json::to_string_pretty(&json).unwrap());
        } else {
            println!("{}", serde_json::to_string(&json).unwrap());
        }
    } else {
        println!("{}", root);
    }
}

fn find_flowconfig(
    flowconfig_name: &str,
    root: Option<&str>,
    file: Option<&str>,
) -> (String, String) {
    match file {
        Some(file) => {
            if !Path::new(file).exists() {
                eprintln!("Could not find file {}", file);
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::CouldNotFindFlowconfig,
                );
            }
            let root = Path::new(file)
                .parent()
                .map(|p| p.to_string_lossy().to_string())
                .unwrap_or_else(|| ".".to_string());
            (file.to_string(), root)
        }
        None => {
            let root = command_utils::guess_root(flowconfig_name, root);
            let file = config_file(flowconfig_name, &root);
            let root = root.to_string_lossy().to_string();
            (file, root)
        }
    }
}

fn check_main(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let json = command_spec::get(args, "--json", &command_spec::truthy()).unwrap();
    let pretty = command_spec::get(args, "--pretty", &command_spec::truthy()).unwrap();
    let root = command_spec::get(
        args,
        "--root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let file = command_spec::get(
        args,
        "file",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let (file, _root) = find_flowconfig(&flowconfig_name, root.as_deref(), file.as_deref());
    match flowconfig::get(&file) {
        Ok((_config, warnings, _hash)) if warnings.is_empty() => if !ignore_version {},
        Ok((_config, warnings, _hash)) => {
            if ignore_version {
                if json || pretty {
                    let json = serde_json::json!({});
                    if pretty {
                        println!("{}", serde_json::to_string_pretty(&json).unwrap());
                    } else {
                        println!("{}", serde_json::to_string(&json).unwrap());
                    }
                }
            } else if json || pretty {
                let json = serde_json::Value::Array(
                    warnings
                        .iter()
                        .map(|flowconfig::Warning(line, msg)| json_of_issue("warning", *line, msg))
                        .collect(),
                );
                exit_with_json(pretty, json);
            } else {
                let errors = warnings
                    .iter()
                    .map(|flowconfig::Warning(line, msg)| (*line, msg.clone()))
                    .collect::<Vec<_>>();
                flowconfig_multi_error(&errors);
            }
        }
        Err(flowconfig::Error(line, msg)) => {
            if json || pretty {
                let json = serde_json::Value::Array(vec![json_of_issue("error", line, &msg)]);
                exit_with_json(pretty, json);
            } else {
                flowconfig_multi_error(&[(line, msg)]);
            }
        }
    }
}

fn find_spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "config find",
        "Return path to .flowconfig",
        "Usage: flow config find [ROOT]\nReturn the path to the .flowconfig file\n\ne.g. flow config find /path/to/root".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
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
    .anon("root", &command_spec::optional(command_spec::string()))
}

fn find_command() -> command_spec::Command {
    command_spec::command(find_spec(), find_main)
}

fn check_spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "config check",
        "Validates the .flowconfig file",
        "Usage: flow config check [FILE]\nValidates the .flowconfig file\n\ne.g. flow config check /path/to/.flowconfig".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
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
        "--root",
        &command_spec::optional(command_spec::string()),
        "Project root directory containing the .flowconfig",
        None,
    )
    .flag(
        "--ignore-version",
        &command_spec::truthy(),
        "Ignore the version constraint in .flowconfig",
        Some("FLOW_IGNORE_VERSION"),
    )
    .anon("file", &command_spec::optional(command_spec::string()))
}

fn check_command() -> command_spec::Command {
    command_spec::command(check_spec(), check_main)
}

#[derive(Clone)]
enum ConfigSubcommand {
    Check,
    Find,
}

fn main_impl(args: &command_spec::Values) {
    let (subcommand, argv) = command_spec::get(
        args,
        "subcommand",
        &command_spec::required(
            None,
            command_spec::command_flag(vec![
                ("check", ConfigSubcommand::Check),
                ("find", ConfigSubcommand::Find),
            ]),
        ),
    )
    .unwrap();
    match subcommand {
        ConfigSubcommand::Check => command_utils::run_command(&check_command(), &argv),
        ConfigSubcommand::Find => command_utils::run_command(&find_command(), &argv),
    }
}

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "config",
        "Read or write the .flowconfig file",
        "Usage: flow config SUBCOMMAND [ROOT]\nRead or write the .flowconfig file\n\nSUBCOMMANDS:\nfind: Return the path to the .flowconfig".to_string(),
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    .anon(
        "subcommand",
        &command_spec::required(
            None,
            command_spec::command_flag(vec![
                ("check", ConfigSubcommand::Check),
                ("find", ConfigSubcommand::Find),
            ]),
        ),
    )
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main_impl)
}
