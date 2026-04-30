/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::path::PathBuf;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// ***********************************************************************
// flow init command
// ***********************************************************************

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "init",
        "Initializes a directory to be used as a flow root directory",
        command_spec::Visibility::Public,
        format!(
            "Usage: {} init [ROOT]\nInitializes a directory to be used as a flow root directory\n\ne.g. {} init /path/to/root\nor {} init\nor {} init --options \"optionA=123;optionB=456\"\nor {} init --lints \"lintA=on,lintB=off\"\n\nIf the root is not specified it is assumed to be the current working directory\n\nThis command will create and initialize /path/to/root/.flowconfig\n",
            command_utils::exe_name(),
            command_utils::exe_name(),
            command_utils::exe_name(),
            command_utils::exe_name(),
            command_utils::exe_name(),
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_flowconfig_flags(spec);
    spec.flag(
        "--options",
        &arg_spec::delimited(
            ";",
            arg_spec::key_value(
                "=",
                arg_spec::string(),
                arg_spec::optional(arg_spec::string()),
            ),
        ),
        "Semicolon-delimited list of key=value pairs",
        None,
    )
    .anon("root", &arg_spec::optional(arg_spec::string()))
}

fn error(errs: Vec<(u32, String)>) -> ! {
    let msg = errs
        .into_iter()
        .map(|(line, msg)| format!(".flowconfig:{} {}", line, msg))
        .collect::<Vec<_>>()
        .join("\n");
    eprintln!("{}", msg);
    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::InvalidFlowconfig);
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let options = command_spec::get(
        args,
        "--options",
        &arg_spec::delimited(
            ";",
            arg_spec::key_value(
                "=",
                arg_spec::string(),
                arg_spec::optional(arg_spec::string()),
            ),
        ),
    )
    .unwrap();
    let root = command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap();

    let root = match root {
        None => std::env::current_dir().unwrap(),
        Some(root) => PathBuf::from(root),
    };
    flow_event_logger::set_root(Some(root.to_string_lossy().to_string()));
    let options = match options {
        None => vec![],
        Some(options) => options
            .into_iter()
            .map(|(key, value)| match value {
                Some(value) => format!("{key}={value}"),
                None => key,
            })
            .collect(),
    };
    let ignores = command_utils::list_of_string_arg(
        command_spec::get(args, "--ignore", &arg_spec::optional(arg_spec::string())).unwrap(),
    );
    let untyped = command_utils::list_of_string_arg(
        command_spec::get(args, "--untyped", &arg_spec::optional(arg_spec::string())).unwrap(),
    );
    let declarations = command_utils::list_of_string_arg(
        command_spec::get(
            args,
            "--declaration",
            &arg_spec::optional(arg_spec::string()),
        )
        .unwrap(),
    );
    let includes = command_utils::list_of_string_arg(
        command_spec::get(args, "--include", &arg_spec::optional(arg_spec::string())).unwrap(),
    );
    let libs = command_utils::list_of_string_arg(
        command_spec::get(args, "--lib", &arg_spec::optional(arg_spec::string())).unwrap(),
    );
    let lints = command_utils::list_of_string_arg(
        command_spec::get(args, "--lints", &arg_spec::optional(arg_spec::string())).unwrap(),
    );
    let file = root.join(&flowconfig_name);
    if file.exists() {
        eprintln!("Error: \"{}\" already exists!", file.display());
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::InvalidFlowconfig);
    }

    let config = flow_config::init(
        ignores,
        untyped,
        declarations,
        includes,
        libs,
        options,
        lints,
    );
    let config = match config {
        Ok((config, warnings)) if warnings.is_empty() => config,
        Ok((_config, warnings)) => error(
            warnings
                .into_iter()
                .map(|flow_config::Warning(line, msg)| (line, msg))
                .collect(),
        ),
        Err(flow_config::Error(line, msg)) => error(vec![(line, msg)]),
    };
    let mut out = std::fs::File::create(&file)
        .unwrap_or_else(|err| panic!("failed to create {}: {}", file.display(), err));
    flow_config::write(&mut out, &config)
        .unwrap_or_else(|err| panic!("failed to write {}: {}", file.display(), err));
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
