/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Write;

use flow_lsp::flow_lsp;
use flow_parser_utils_output::replacement_printer;
use flow_parser_utils_output::replacement_printer::Patch;
use flow_server_env::server_prot;
use flow_server_utils::file_input::FileInput;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// This module implements the flow command `apply-code-action`
// which exposes LSP code-actions via the CLI
fn handle_error(code: Option<flow_common_exit_status::FlowExitStatus>, msg: &str) -> ! {
    eprintln!("{}", msg);
    flow_common_exit_status::exit(
        code.unwrap_or(flow_common_exit_status::FlowExitStatus::UnknownError),
    )
}

#[derive(Clone)]
enum ApplyCodeActionSubcommand {
    Quickfix,
    SourceAddMissingImports,
    SuggestImports,
}

mod quickfix {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "Apply available quickfixes",
            "Runs all safe quickfixes. If requested, run one additional best effort quickfix",
            format!(
                "Usage: {exe_name} apply-code-action 'experimental.quickfix'  [OPTION]... FILE"
            ),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_connect_flags(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_path_flag(spec);
        let spec = command_utils::add_wait_for_recheck_flag(spec);
        spec.flag(
            "--in-place",
            &arg_spec::truthy(),
            "Overwrite the input file",
            None,
        )
        .flag(
            "--include-best-effort-fix",
            &arg_spec::truthy(),
            "Whether to include one best effort quickfix",
            None,
        )
        .anon("file", &arg_spec::required(None, arg_spec::string()))
    }

    fn handle_ok(in_place: bool, patch: &Patch, source_path: &str, input: &FileInput) {
        let write_patch = |content: &str| {
            let mut output_channel: Box<dyn Write> = if in_place {
                Box::new(std::fs::File::create(source_path).unwrap_or_else(|error| {
                    handle_error(None, &format!("{}: {}", source_path, error))
                }))
            } else {
                Box::new(std::io::stdout())
            };
            output_channel
                .write_all(replacement_printer::print(patch, content).as_bytes())
                .unwrap();
            output_channel.flush().unwrap();
        };
        match input.content_of_file_input() {
            Ok(content) => write_patch(&content),
            Err(msg) => handle_error(None, &msg),
        }
    }

    fn main(args: &arg_spec::Values) {
        let base_flags = command_utils::get_base_flags(args);
        let connect_params = command_utils::get_connect_flags(args);
        let root_arg =
            command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
        let path =
            command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
        let wait_for_recheck = command_spec::get(
            args,
            "--wait-for-recheck",
            &arg_spec::optional(arg_spec::bool_flag()),
        )
        .unwrap();
        let in_place = command_spec::get(args, "--in-place", &arg_spec::truthy()).unwrap();
        let include_best_effort_fix =
            command_spec::get(args, "--include-best-effort-fix", &arg_spec::truthy()).unwrap();
        let file =
            command_spec::get(args, "file", &arg_spec::required(None, arg_spec::string())).unwrap();

        let source_path = command_utils::expand_path(&file);
        let spec = spec();
        let input = command_utils::get_file_from_filename_or_stdin(
            &spec.name,
            Some(source_path.as_str()),
            path.as_deref(),
        );
        let root = command_utils::get_the_root(
            &base_flags.flowconfig_name,
            root_arg.as_deref(),
            Some(&input),
        );
        let flowconfig_name = base_flags.flowconfig_name;
        let request = server_prot::request::Command::APPLY_CODE_ACTION {
            input: input.clone(),
            action: server_prot::code_action::T::Quickfix {
                include_best_effort_fix,
            },
            wait_for_recheck,
        };
        let response = command_utils::connect_and_make_request(
            &flowconfig_name,
            &connect_params,
            &root,
            &request,
        );
        match response {
            server_prot::response::Response::APPLY_CODE_ACTION(Ok(patch)) => {
                handle_ok(in_place, &patch, source_path.as_str(), &input)
            }
            _ => handle_error(None, "Flow: invalid server response"),
        }
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod source_add_missing_imports {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "Add mising imports",
            "Runs the 'source.addMissingImports' code action",
            format!(
                "Usage: {exe_name} apply-code-action 'source.addMissingImports'  [OPTION]... FILE"
            ),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_connect_flags(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_path_flag(spec);
        let spec = command_utils::add_wait_for_recheck_flag(spec);
        spec.flag(
            "--in-place",
            &arg_spec::truthy(),
            "Overwrite the input file",
            None,
        )
        .anon("file", &arg_spec::required(None, arg_spec::string()))
    }

    fn handle_ok(in_place: bool, patch: &Patch, source_path: &str, input: &FileInput) {
        let write_patch = |content: &str| {
            let mut output_channel: Box<dyn Write> = if in_place {
                Box::new(std::fs::File::create(source_path).unwrap_or_else(|error| {
                    handle_error(None, &format!("{}: {}", source_path, error))
                }))
            } else {
                Box::new(std::io::stdout())
            };
            output_channel
                .write_all(replacement_printer::print(patch, content).as_bytes())
                .unwrap();
            output_channel.flush().unwrap();
        };
        match input.content_of_file_input() {
            Ok(content) => write_patch(&content),
            Err(msg) => handle_error(None, &msg),
        }
    }

    fn main(args: &arg_spec::Values) {
        let base_flags = command_utils::get_base_flags(args);
        let connect_params = command_utils::get_connect_flags(args);
        let root_arg =
            command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
        let path =
            command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
        let wait_for_recheck = command_spec::get(
            args,
            "--wait-for-recheck",
            &arg_spec::optional(arg_spec::bool_flag()),
        )
        .unwrap();
        let in_place = command_spec::get(args, "--in-place", &arg_spec::truthy()).unwrap();
        let file =
            command_spec::get(args, "file", &arg_spec::required(None, arg_spec::string())).unwrap();

        let source_path = command_utils::expand_path(&file);
        let spec = spec();
        let input = command_utils::get_file_from_filename_or_stdin(
            &spec.name,
            Some(source_path.as_str()),
            path.as_deref(),
        );
        let root = command_utils::get_the_root(
            &base_flags.flowconfig_name,
            root_arg.as_deref(),
            Some(&input),
        );
        let flowconfig_name = base_flags.flowconfig_name;
        let request = server_prot::request::Command::APPLY_CODE_ACTION {
            input: input.clone(),
            action: server_prot::code_action::T::SourceAddMissingImports,
            wait_for_recheck,
        };
        let response = command_utils::connect_and_make_request(
            &flowconfig_name,
            &connect_params,
            &root,
            &request,
        );
        match response {
            server_prot::response::Response::APPLY_CODE_ACTION(Ok(patch)) => {
                handle_ok(in_place, &patch, source_path.as_str(), &input)
            }
            _ => handle_error(None, "Flow: invalid server response"),
        }
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod suggest_imports {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "Show import suggestions",
            "Shows import suggestions for all unbound names in the file ranked by usage",
            format!("Usage: {exe_name} apply-code-action 'suggestImports'  [OPTION]... FILE"),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_connect_and_json_flags(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_path_flag(spec);
        let spec = command_utils::add_wait_for_recheck_flag(spec);
        spec.flag(
            "--in-place",
            &arg_spec::truthy(),
            "Overwrite the input file",
            None,
        )
        .anon("file", &arg_spec::required(None, arg_spec::string()))
    }

    fn handle_ok(
        pretty: bool,
        lsp_result: std::collections::BTreeMap<
            String,
            Vec<server_prot::response::CodeActionOrCommand>,
        >,
    ) {
        let result_json = serde_json::Value::Object(
            lsp_result
                .into_iter()
                .map(|(name, result)| {
                    (name, flow_lsp::lsp_fmt_print_code_action_result("", result))
                })
                .collect::<serde_json::Map<String, serde_json::Value>>(),
        );
        flow_hh_json::print_json_endline(pretty, &result_json);
    }

    fn main(args: &arg_spec::Values) {
        let base_flags = command_utils::get_base_flags(args);
        let connect_params = command_utils::get_connect_flags(args);
        let _json = command_spec::get(args, "--json", &arg_spec::truthy()).unwrap();
        let pretty = command_spec::get(args, "--pretty", &arg_spec::truthy()).unwrap();
        let root_arg =
            command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
        let path =
            command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
        let wait_for_recheck = command_spec::get(
            args,
            "--wait-for-recheck",
            &arg_spec::optional(arg_spec::bool_flag()),
        )
        .unwrap();
        let _in_place = command_spec::get(args, "--in-place", &arg_spec::truthy()).unwrap();
        let file =
            command_spec::get(args, "file", &arg_spec::required(None, arg_spec::string())).unwrap();

        let source_path = command_utils::expand_path(&file);
        let spec = spec();
        let input = command_utils::get_file_from_filename_or_stdin(
            &spec.name,
            Some(source_path.as_str()),
            path.as_deref(),
        );
        let root = command_utils::get_the_root(
            &base_flags.flowconfig_name,
            root_arg.as_deref(),
            Some(&input),
        );
        let flowconfig_name = base_flags.flowconfig_name;
        let connect_params = command_utils::ConnectParams {
            quiet: connect_params.quiet || _json || pretty,
            ..connect_params
        };
        let request = server_prot::request::Command::APPLY_CODE_ACTION {
            input,
            action: server_prot::code_action::T::SuggestImports,
            wait_for_recheck,
        };
        let response = command_utils::connect_and_make_request(
            &flowconfig_name,
            &connect_params,
            &root,
            &request,
        );
        match response {
            server_prot::response::Response::SUGGEST_IMPORTS(Ok(result_json)) => {
                let result: std::collections::BTreeMap<
                    String,
                    Vec<server_prot::response::CodeActionOrCommand>,
                > = serde_json::from_str(&result_json)
                    .unwrap_or_else(|e| handle_error(None, &format!("Flow: parse error: {}", e)));
                handle_ok(pretty, result)
            }
            _ => handle_error(None, "Flow: invalid server response"),
        }
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

fn spec() -> command_spec::Spec {
    let quickfix_command = quickfix::command();
    let source_add_missing_imports_command = source_add_missing_imports::command();
    let suggest_imports_command = suggest_imports::command();
    command_utils::subcommand_spec(
        "apply-code-action",
        "",
        vec![
            (
                "experimental.quickfix",
                ApplyCodeActionSubcommand::Quickfix,
                quickfix_command.doc().to_owned(),
            ),
            (
                "source.addMissingImports",
                ApplyCodeActionSubcommand::SourceAddMissingImports,
                source_add_missing_imports_command.doc().to_owned(),
            ),
            (
                "suggestImports",
                ApplyCodeActionSubcommand::SuggestImports,
                suggest_imports_command.doc().to_owned(),
            ),
        ],
    )
}

fn main(args: &arg_spec::Values) {
    let (cmd, argv) = command_spec::get(
        args,
        "subcommand",
        &arg_spec::required(
            None,
            arg_spec::command_flag(vec![
                ("experimental.quickfix", ApplyCodeActionSubcommand::Quickfix),
                (
                    "source.addMissingImports",
                    ApplyCodeActionSubcommand::SourceAddMissingImports,
                ),
                ("suggestImports", ApplyCodeActionSubcommand::SuggestImports),
            ]),
        ),
    )
    .unwrap();
    let cmd = match cmd {
        ApplyCodeActionSubcommand::Quickfix => quickfix::command(),
        ApplyCodeActionSubcommand::SourceAddMissingImports => source_add_missing_imports::command(),
        ApplyCodeActionSubcommand::SuggestImports => suggest_imports::command(),
    };
    command_utils::run_command(&cmd, &argv);
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
