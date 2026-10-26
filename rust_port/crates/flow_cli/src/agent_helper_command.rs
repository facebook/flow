/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_command_spec::arg_spec;
use flow_parser::loc::LOC_NONE;
use flow_parser::loc::Loc;
use flow_server_env::server_prot;

#[derive(Clone)]
enum AgentHelperSubcommand {
    InspectTypes,
}

const INSPECT_TYPES_DOC: &str = "Shows the hover type at each `// ^?` comment in a file";

fn subcommand_flag() -> arg_spec::FlagType<Option<(AgentHelperSubcommand, Vec<String>)>> {
    arg_spec::command_flag(vec![("inspect-types", AgentHelperSubcommand::InspectTypes)])
}

fn inspect_types_spec() -> flow_command_spec::Spec {
    let exe_name = flow_command_utils::exe_name();
    let spec = flow_command_spec::Spec::new(
        "inspect-types",
        INSPECT_TYPES_DOC,
        flow_command_spec::Visibility::Public,
        format!(
            "Usage: {exe_name} agent-helper inspect-types [OPTION]... [FILE]\n\nEach `// ^?` comment queries the column its `^` points at on the line above.\n\ne.g. {exe_name} agent-helper inspect-types foo.js\nor   {exe_name} agent-helper inspect-types < foo.js\n"
        ),
    );
    let spec = flow_command_utils::add_base_flags(spec);
    let spec = flow_command_utils::add_connect_flags(spec);
    let spec = flow_command_utils::add_root_flag(spec);
    let spec = flow_command_utils::add_strip_root_flag(spec);
    let spec = flow_command_utils::add_from_flag(spec);
    let spec = flow_command_utils::add_path_flag(spec);
    let spec = flow_command_utils::add_wait_for_recheck_flag(spec);
    spec.anon("file", &arg_spec::optional(arg_spec::string()))
}

// A query that matched nothing has no type loc, so report where the `^` pointed.
fn string_of_query_loc(strip_root: Option<&str>, cursor_loc: &Loc, type_loc: &Loc) -> String {
    if *type_loc != LOC_NONE {
        return flow_common::reason::string_of_loc(strip_root, type_loc);
    }
    let source = cursor_loc
        .source
        .as_ref()
        .map(|source| flow_common::reason::string_of_source(strip_root, source))
        .unwrap_or_default();
    format!(
        "{}:{}:{}",
        source,
        cursor_loc.start.line,
        cursor_loc.start.column + 1
    )
}

fn print_inspect_types_item(
    strip_root: Option<&str>,
    item: &server_prot::response::inspect_types::Item,
) {
    let server_prot::response::inspect_types::Item {
        cursor_loc,
        type_loc,
        type_str,
    } = item;
    println!("{}", string_of_query_loc(strip_root, cursor_loc, type_loc));
    println!("{}", type_str.as_deref().unwrap_or("(unknown)"));
}

fn run_inspect_types(args: &arg_spec::Values) {
    let base_flags = flow_command_utils::get_base_flags(args);
    let connect_flags = flow_command_utils::get_connect_flags(args);
    let root_arg =
        flow_command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = flow_command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let path =
        flow_command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let wait_for_recheck = flow_command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let filename =
        flow_command_spec::get(args, "file", &arg_spec::optional(arg_spec::string())).unwrap();

    let file = flow_command_utils::get_file_from_filename_or_stdin(
        "agent-helper inspect-types",
        filename.as_deref(),
        path.as_deref(),
    );
    let flowconfig_name = base_flags.flowconfig_name;
    let root = flow_command_utils::find_a_root(&flowconfig_name, root_arg.as_deref(), Some(&file));
    let request =
        server_prot::request::Command::INSPECT_TYPES(server_prot::inspect_types_options::T {
            input: file,
            wait_for_recheck,
        });
    let response = flow_command_utils::connect_and_make_request(
        &flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::INSPECT_TYPES(Err(err)) => {
            eprintln!("{}", err);
        }
        server_prot::response::Response::INSPECT_TYPES(Ok(items)) => {
            let strip_root = if strip_root {
                Some(root.to_string_lossy().to_string())
            } else {
                None
            };
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    println!();
                }
                print_inspect_types_item(strip_root.as_deref(), item);
            }
        }
        response => {
            flow_command_utils::failwith_bad_response(&request, &response);
        }
    }
}

fn root_spec() -> flow_command_spec::Spec {
    flow_command_spec::Spec::new(
        "agent-helper",
        "Commands for coding agents working with Flow",
        flow_command_spec::Visibility::Public,
        format!(
            "Usage: {} agent-helper SUBCOMMAND [OPTION]...\n\nValid values for SUBCOMMAND:\n{}",
            flow_command_utils::exe_name(),
            flow_command_spec::format_two_columns(
                None,
                None,
                1,
                &[("inspect-types".to_string(), INSPECT_TYPES_DOC.to_string())],
            ),
        ),
    )
    .anon("subcommand", &subcommand_flag())
}

pub(crate) fn command() -> flow_command_spec::Command {
    flow_command_spec::command(root_spec(), |args| {
        let (subcommand, argv) =
            match flow_command_spec::get(args, "subcommand", &subcommand_flag()) {
                Ok(Some(subcommand)) => subcommand,
                Ok(None) => {
                    flow_command_spec::usage(&root_spec());
                    flow_common_exit_status::exit(
                        flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
                    );
                }
                Err(err) => {
                    flow_command_utils::die_bad_args(&flow_command_spec::error_message(&err))
                }
            };
        let command = match subcommand {
            AgentHelperSubcommand::InspectTypes => {
                flow_command_spec::command(inspect_types_spec(), run_inspect_types)
            }
        };
        flow_command_utils::run_command(&command, &argv);
    })
}
