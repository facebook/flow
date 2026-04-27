/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_server_files::server_files_js;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

#[derive(Clone)]
enum ConfigSubcommand {
    Check,
    Find,
}

fn find_subcommand() -> command_spec::Command {
    let spec = {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "config find",
            "Return path to .flowconfig",
            format!(
                "Usage: {} config find [ROOT]\nReturn the path to the .flowconfig file\n\ne.g. {} config find /path/to/root",
                exe_name, exe_name,
            ),
        );
        let spec = command_utils::add_flowconfig_name_flag(spec);
        let spec = command_utils::add_json_flags(spec);
        spec.anon("root", &arg_spec::optional(arg_spec::string()))
    };
    let main = |args: &arg_spec::Values| {
        let flowconfig_name = command_spec::get(
            args,
            "--flowconfig-name",
            &arg_spec::required(Some(".flowconfig".to_string()), arg_spec::string()),
        )
        .unwrap();
        let command_utils::JsonFlags { json, pretty } = command_utils::get_json_flags(args);
        let root =
            command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap();
        let root = command_utils::guess_root(&flowconfig_name, root.as_deref())
            .to_string_lossy()
            .to_string();
        flow_event_logger::set_root(Some(root.clone()));
        if json || pretty {
            let json = serde_json::json!({ "root": root });
            flow_hh_json::print_json_endline(pretty, &json);
        } else {
            println!("{}", root);
        }
    };
    command_spec::command(spec, main)
}

fn check_subcommand() -> command_spec::Command {
    let spec = {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "config check",
            "Validates the .flowconfig file",
            format!(
                "Usage: {} config check [FILE]\nValidates the .flowconfig file\n\ne.g. {} config check /path/to/.flowconfig",
                exe_name, exe_name,
            ),
        );
        let spec = command_utils::add_flowconfig_name_flag(spec);
        let spec = command_utils::add_json_flags(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_ignore_version_flag(spec);
        spec.anon("file", &arg_spec::optional(arg_spec::string()))
    };

    fn find_flowconfig(
        flowconfig_name: &str,
        root: Option<&str>,
        file: Option<&str>,
    ) -> (String, String) {
        match file {
            Some(file) => {
                if !std::path::Path::new(file).exists() {
                    let msg = format!("Could not find file {}", file);
                    eprintln!("{}", msg);
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::CouldNotFindFlowconfig,
                    );
                }
                let root = std::path::Path::new(file)
                    .parent()
                    .map(|p| p.to_string_lossy().to_string())
                    .unwrap_or_else(|| ".".to_string());
                (file.to_string(), root)
            }
            None => {
                let root = command_utils::guess_root(flowconfig_name, root);
                let file = server_files_js::config_file(flowconfig_name, &root);
                let root = root.to_string_lossy().to_string();
                (file, root)
            }
        }
    }

    fn json_of_issue(kind: &str, line: u32, msg: &str) -> serde_json::Value {
        serde_json::json!({
            "line": line,
            "message": msg,
            "level": kind,
        })
    }

    fn exit_with_json(pretty: bool, errors_json: serde_json::Value) -> ! {
        let code = flow_common_exit_status::FlowExitStatus::InvalidFlowconfig;
        let mut props = vec![("errors".to_string(), errors_json)];
        props.extend(flow_common_exit_status::json_props_of_t(code, None));
        let json = serde_json::Value::Object(props.into_iter().collect());
        flow_hh_json::print_json_endline(pretty, &json);
        flow_common_exit::unset_json_mode();
        flow_common_exit_status::exit(code);
    }

    let main = |args: &arg_spec::Values| {
        let flowconfig_name = command_spec::get(
            args,
            "--flowconfig-name",
            &arg_spec::required(Some(".flowconfig".to_string()), arg_spec::string()),
        )
        .unwrap();
        let command_utils::JsonFlags { json, pretty } = command_utils::get_json_flags(args);
        let root =
            command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
        let ignore_version =
            command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap();
        let file =
            command_spec::get(args, "file", &arg_spec::optional(arg_spec::string())).unwrap();
        let (file, root) = find_flowconfig(&flowconfig_name, root.as_deref(), file.as_deref());
        flow_event_logger::set_root(Some(root));
        match flow_config::get_with_ignored_version(&file, ignore_version) {
            Ok((config, warnings, _hash)) if warnings.is_empty() => {
                if !ignore_version {
                    command_utils::assert_version(&config);
                }
            }
            Ok((config, warnings, _hash)) => {
                if ignore_version {
                    if json || pretty {
                        let json = serde_json::json!({});
                        flow_hh_json::print_json_endline(pretty, &json);
                    }
                } else {
                    command_utils::assert_version(&config);
                    if json || pretty {
                        let json = serde_json::Value::Array(
                            warnings
                                .iter()
                                .map(|flow_config::Warning(line, msg)| {
                                    json_of_issue("warning", *line, msg)
                                })
                                .collect(),
                        );
                        exit_with_json(pretty, json);
                    } else {
                        let errors: Vec<(u32, String)> = warnings
                            .iter()
                            .map(|flow_config::Warning(line, msg)| (*line, msg.clone()))
                            .collect();
                        command_utils::flowconfig_multi_error(&errors);
                    }
                }
            }
            Err(flow_config::Error(line, msg)) => {
                if json || pretty {
                    let json = serde_json::Value::Array(vec![json_of_issue("error", line, &msg)]);
                    exit_with_json(pretty, json);
                } else {
                    command_utils::flowconfig_multi_error(&[(line, msg)]);
                }
            }
        }
    };
    command_spec::command(spec, main)
}

pub(crate) fn command() -> command_spec::Command {
    let spec = {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "config",
            "Read or write the .flowconfig file",
            format!(
                "Usage: {} config SUBCOMMAND [ROOT]\nRead or write the .flowconfig file\n\nSUBCOMMANDS:\nfind: Return the path to the .flowconfig\n",
                exe_name,
            ),
        );
        let spec = command_utils::add_from_flag(spec);
        spec.anon(
            "subcommand",
            &arg_spec::required(
                None,
                arg_spec::command_flag(vec![
                    ("check", ConfigSubcommand::Check),
                    ("find", ConfigSubcommand::Find),
                ]),
            ),
        )
    };
    let main = |args: &arg_spec::Values| {
        let (subcommand, argv) = command_spec::get(
            args,
            "subcommand",
            &arg_spec::required(
                None,
                arg_spec::command_flag(vec![
                    ("check", ConfigSubcommand::Check),
                    ("find", ConfigSubcommand::Find),
                ]),
            ),
        )
        .unwrap();
        let cmd = match subcommand {
            ConfigSubcommand::Check => check_subcommand(),
            ConfigSubcommand::Find => find_subcommand(),
        };
        command_utils::run_command(&cmd, &argv);
    };
    command_spec::command(spec, main)
}
