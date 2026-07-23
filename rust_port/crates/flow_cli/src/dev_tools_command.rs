/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::path::PathBuf;

use flow_dev_tools::ErrorCheckCommand;
use flow_dev_tools::update_suppressions::Only;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

#[derive(Clone)]
enum DevToolsSubcommand {
    AddComments,
    UpdateSuppressions,
}

fn error_check_command_flag() -> arg_spec::FlagType<Option<ErrorCheckCommand>> {
    arg_spec::enum_flag(vec![
        ("check", ErrorCheckCommand::Check),
        ("full-check", ErrorCheckCommand::FullCheck),
        ("status", ErrorCheckCommand::Status),
    ])
}

fn only_flag() -> arg_spec::FlagType<Option<Only>> {
    arg_spec::enum_flag(vec![("add", Only::Add), ("remove", Only::Remove)])
}

fn get_optional_string(args: &arg_spec::Values, flag: &str) -> Option<String> {
    command_spec::get(args, flag, &arg_spec::optional(arg_spec::string())).unwrap()
}

fn get_flowconfig_name(args: &arg_spec::Values) -> String {
    if args.contains_key("--flowconfigName") {
        command_spec::get(
            args,
            "--flowconfigName",
            &arg_spec::required(Some(".flowconfig".to_string()), arg_spec::string()),
        )
        .unwrap()
    } else {
        command_utils::get_base_flags(args).flowconfig_name
    }
}

fn get_bin(args: &arg_spec::Values) -> String {
    get_optional_string(args, "--bin")
        .map(|bin| command_utils::expand_path(&bin))
        .unwrap_or_else(|| {
            std::env::current_exe()
                .map(|path| path.to_string_lossy().to_string())
                .unwrap_or_else(|_| "flow".to_string())
        })
}

fn get_check(args: &arg_spec::Values) -> ErrorCheckCommand {
    let flag = if args.contains_key("-c") {
        "-c"
    } else {
        "--check"
    };
    command_spec::get(
        args,
        flag,
        &arg_spec::required(Some(ErrorCheckCommand::Status), error_check_command_flag()),
    )
    .unwrap()
}

fn parse_single_root(args: &arg_spec::Values) -> PathBuf {
    let Some(root) = command_spec::get(args, "ROOT", &arg_spec::string()).unwrap() else {
        flow_common_exit_status::exit_with_msg(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            "Missing required ROOT argument",
        );
    };
    PathBuf::from(command_utils::expand_path(&root))
}

fn parse_roots(args: &arg_spec::Values) -> Vec<PathBuf> {
    let roots = command_spec::get(args, "ROOT...", &arg_spec::list_of(arg_spec::string()))
        .unwrap()
        .unwrap_or_default();
    if roots.is_empty() {
        flow_common_exit_status::exit_with_msg(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            "Missing required ROOT argument",
        );
    }
    roots
        .into_iter()
        .map(|root| PathBuf::from(command_utils::expand_path(&root)))
        .collect()
}

fn dev_tools_common_spec(name: &str, doc: &str, usage: String) -> command_spec::Spec {
    command_utils::add_base_flags(command_spec::Spec::new(
        name,
        doc,
        command_spec::Visibility::Public,
        usage,
    ))
    .flag(
        "--bin",
        &arg_spec::optional(arg_spec::string()),
        "Path to the flow binary",
        None,
    )
    .flag(
        "--flowconfigName",
        &arg_spec::required(Some(".flowconfig".to_string()), arg_spec::string()),
        "Name of the flowconfig to use in checking",
        Some("FLOW_CONFIG_NAME"),
    )
    .flag(
        "--check",
        &arg_spec::required(Some(ErrorCheckCommand::Status), error_check_command_flag()),
        "The flow command to check flow errors (check|full-check|status)",
        None,
    )
    .flag(
        "-c",
        &arg_spec::required(Some(ErrorCheckCommand::Status), error_check_command_flag()),
        "",
        None,
    )
}

fn add_comments_spec() -> command_spec::Spec {
    dev_tools_common_spec(
        "add-comments",
        "Adds flow comments",
        format!(
            "Usage: {} dev-tools add-comments [OPTION]... ROOT\n\nQueries Flow for the errors for ROOT. The errors automatically have a comment added on the line before them.\n",
            command_utils::exe_name()
        ),
    )
    .flag(
        "--comment",
        &arg_spec::optional(arg_spec::string()),
        "Comment to add before the selected errors",
        None,
    )
    .flag("--all", &arg_spec::truthy(), "DEPRECATED", None)
    .flag(
        "--code",
        &arg_spec::optional(arg_spec::string()),
        "Only add comments for a specific code",
        None,
    )
    .anon("ROOT", &arg_spec::string())
}

fn update_suppressions_spec() -> command_spec::Spec {
    dev_tools_common_spec(
        "update-suppressions",
        "Adds and removes suppression comments",
        format!(
            "Usage: {} dev-tools update-suppressions [OPTION]... ROOT [ROOT...]\n\nRemoves unnecessary, and adds necessary, error suppression comments for ROOT.\n",
            command_utils::exe_name()
        ),
    )
    .flag(
        "--include-flowtest",
        &arg_spec::truthy(),
        "Also remove comments from files that end in -flowtest.js or are in a __flowtests__ directory",
        None,
    )
    .flag(
        "--sites",
        &arg_spec::optional(arg_spec::string()),
        "Comma-delimited list of site names for each ROOT",
        None,
    )
    .flag(
        "--comment",
        &arg_spec::optional(arg_spec::string()),
        "Comment to include with the suppression. Automatically prepends $FlowFixMe",
        None,
    )
    .flag(
        "--only",
        &arg_spec::optional(only_flag()),
        "Use --only add to only add comments and --only remove to only remove comments",
        None,
    )
    .flag(
        "--diffBin",
        &arg_spec::optional(arg_spec::string()),
        "Path to another binary to use",
        None,
    )
    .flag(
        "--diff-bin",
        &arg_spec::optional(arg_spec::string()),
        "Path to another binary to use",
        None,
    )
    .anon("ROOT...", &arg_spec::list_of(arg_spec::string()))
}

fn root_spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "dev-tools",
        "Runs Flow developer tools",
        command_spec::Visibility::Public,
        format!(
            "Usage: {} dev-tools SUBCOMMAND [OPTION]...\n\nValid values for SUBCOMMAND:\n{}",
            command_utils::exe_name(),
            command_spec::format_two_columns(
                None,
                None,
                1,
                &[
                    ("add-comments".to_string(), "Adds flow comments".to_string(),),
                    (
                        "update-suppressions".to_string(),
                        "Adds and removes suppression comments".to_string(),
                    ),
                ],
            ),
        ),
    )
    .anon(
        "subcommand",
        &arg_spec::required(
            None,
            arg_spec::command_flag(vec![
                ("add-comments", DevToolsSubcommand::AddComments),
                (
                    "update-suppressions",
                    DevToolsSubcommand::UpdateSuppressions,
                ),
            ]),
        ),
    )
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(root_spec(), |args| {
        let (subcommand, argv) = command_spec::get(
            args,
            "subcommand",
            &arg_spec::required(
                None,
                arg_spec::command_flag(vec![
                    ("add-comments", DevToolsSubcommand::AddComments),
                    (
                        "update-suppressions",
                        DevToolsSubcommand::UpdateSuppressions,
                    ),
                ]),
            ),
        )
        .unwrap();
        let command = match subcommand {
            DevToolsSubcommand::AddComments => command_spec::command(add_comments_spec(), |args| {
                run_or_exit(run_add_comments(args))
            }),
            DevToolsSubcommand::UpdateSuppressions => {
                command_spec::command(update_suppressions_spec(), |args| {
                    run_or_exit(run_update_suppressions(args))
                })
            }
        };
        command_utils::run_command(&command, &argv);
    })
}

fn run_or_exit(result: io::Result<()>) {
    if let Err(err) = result {
        let msg = format!("dev-tools failed: {}", err);
        flow_common_exit_status::exit_with_msg(
            flow_common_exit_status::FlowExitStatus::UnknownError,
            &msg,
        );
    }
}

fn run_add_comments(args: &arg_spec::Values) -> io::Result<()> {
    flow_dev_tools::comment::add_comments::runner(flow_dev_tools::comment::add_comments::Args {
        bin: get_bin(args),
        flowconfig_name: get_flowconfig_name(args),
        comment: get_optional_string(args, "--comment"),
        error_code: get_optional_string(args, "--code"),
        error_check_command: get_check(args),
        root: parse_single_root(args),
    })
}

fn run_update_suppressions(args: &arg_spec::Values) -> io::Result<()> {
    let diff_bin = if args.contains_key("--diffBin") {
        get_optional_string(args, "--diffBin")
    } else {
        get_optional_string(args, "--diff-bin")
    }
    .map(|bin| command_utils::expand_path(&bin));

    flow_dev_tools::update_suppressions::runner(flow_dev_tools::update_suppressions::Args {
        bin: get_bin(args),
        diff_bin,
        flowconfig_name: get_flowconfig_name(args),
        error_check_command: get_check(args),
        comment: get_optional_string(args, "--comment").unwrap_or_default(),
        roots: parse_roots(args),
        root_names: get_optional_string(args, "--sites")
            .unwrap_or_default()
            .split(',')
            .map(|site| site.trim().to_string())
            .collect(),
        include_flowtest: command_spec::get(args, "--include-flowtest", &arg_spec::truthy())
            .unwrap(),
        only: command_spec::get(args, "--only", &arg_spec::optional(only_flag())).unwrap(),
    })
}
