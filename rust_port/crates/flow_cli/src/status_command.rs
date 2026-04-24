/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeSet;
use std::io::Write;
use std::path::PathBuf;

use flow_common_errors::error_utils::ConcreteLocPrintableErrorSet;
use flow_common_errors::error_utils::PrintableError;
use flow_common_errors::error_utils::cli_output;
use flow_common_errors::error_utils::json_output;
use flow_parser::loc::Loc;
use flow_server_env::server_prot;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;
use crate::command_utils::ConnectParams;
use crate::command_utils::OffsetStyle;

struct StatusArgs {
    root: PathBuf,
    output_json: bool,
    output_json_version: Option<json_output::JsonVersion>,
    offset_style: Option<OffsetStyle>,
    pretty: bool,
    error_flags: cli_output::ErrorFlags,
    strip_root: bool,
}

fn spec(name: &str, doc: &str, usage: String, explicit: bool) -> command_spec::Spec {
    let spec = command_spec::Spec::new(name, doc, usage);
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_and_json_flags(spec);
    let spec = command_utils::add_json_version_flag(spec);
    let spec = command_utils::add_offset_style_flag(spec);
    let spec = command_utils::add_error_flags(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = if explicit {
        spec
    } else {
        spec.flag(
            "--version",
            &arg_spec::truthy(),
            "Print version number and exit",
            None,
        )
    };
    spec.anon("root", &arg_spec::optional(arg_spec::string()))
}

fn check_status(flowconfig_name: &str, args: &StatusArgs, connect_flags: &ConnectParams) {
    let include_warnings = args.error_flags.include_warnings;
    let request = server_prot::request::Command::STATUS { include_warnings };
    let (response, lazy_stats) = match command_utils::connect_and_make_request(
        flowconfig_name,
        connect_flags,
        &args.root,
        &request,
    ) {
        server_prot::response::Response::STATUS {
            status_response,
            lazy_stats,
        } => (status_response, lazy_stats),
        response => command_utils::failwith_bad_response(&request, &response),
    };
    let strip_root = if args.strip_root {
        Some(args.root.clone())
    } else {
        None
    };
    let offset_kind = command_utils::offset_kind_of_offset_style(args.offset_style);
    let print_json =
        |errors: &ConcreteLocPrintableErrorSet,
         warnings: &ConcreteLocPrintableErrorSet,
         suppressed_errors: &[(PrintableError<Loc>, BTreeSet<Loc>)]| {
            let strip_root = strip_root
                .as_deref()
                .map(|root| root.to_string_lossy().into_owned());
            let stdout = std::io::stdout();
            let mut out = stdout.lock();
            flow_common_errors::error_utils::json_output::print_errors_with_offset_kind(
                &mut out,
                strip_root.as_deref(),
                suppressed_errors,
                args.pretty,
                args.output_json_version
                    .unwrap_or(json_output::JsonVersion::JsonV1),
                &None,
                offset_kind,
                errors,
                warnings,
            )
            .expect("failed to write json errors");
            out.flush().expect("failed to flush json errors");
        };
    let lazy_msg = if lazy_stats.lazy_mode {
        let checked_source = lazy_stats.checked_files - lazy_stats.checked_libdef_files;
        let total_source = lazy_stats.total_files - lazy_stats.total_libdef_files;
        let libdef_msg = format!(
            " (+ {}/{} libdefs)",
            lazy_stats.checked_libdef_files, lazy_stats.total_libdef_files
        );
        Some(format!(
            "The Flow server is currently in lazy mode and is only checking {}/{} source files{}.\nTo learn more, visit flow.org/en/docs/lang/lazy-modes",
            checked_source, total_source, libdef_msg
        ))
    } else {
        None
    };
    match response {
        server_prot::response::StatusResponse::ERRORS {
            errors,
            warnings,
            suppressed_errors,
        } => {
            let error_flags = &args.error_flags;
            let from = flow_event_logger::get_from_i_am_a_clown();
            if args.output_json {
                print_json(&errors, &warnings, &suppressed_errors)
            } else if matches!(from.as_deref(), Some("vim") | Some("emacs")) {
                let strip_root = strip_root
                    .as_deref()
                    .map(|root| root.to_string_lossy().into_owned());
                let stdout = std::io::stdout();
                let mut out = stdout.lock();
                flow_common_errors::error_utils::vim_emacs_output::print_errors(
                    strip_root.as_deref(),
                    &mut out,
                    &errors,
                    &warnings,
                )
                .expect("failed to write vim/emacs errors");
                out.flush().expect("failed to flush vim/emacs errors");
            } else {
                let mut cli_errors = errors.clone();
                for (error, _) in &suppressed_errors {
                    cli_errors.add(error.clone());
                }
                let stdout = std::io::stdout();
                let mut out = stdout.lock();
                flow_common_errors::error_utils::cli_output::print_errors(
                    &mut out,
                    error_flags,
                    &None,
                    strip_root.as_deref(),
                    &cli_errors,
                    &warnings,
                    lazy_msg.as_deref(),
                )
                .expect("failed to write cli errors");
                out.flush().expect("failed to flush cli errors");
            }
            flow_common_exit_status::exit(command_utils::get_check_or_status_exit_code(
                &errors,
                &warnings,
                error_flags.max_warnings,
            ))
        }
        server_prot::response::StatusResponse::NO_ERRORS => {
            if args.output_json {
                print_json(
                    &ConcreteLocPrintableErrorSet::empty(),
                    &ConcreteLocPrintableErrorSet::empty(),
                    &[],
                )
            } else {
                println!("No errors!");
                if let Some(msg) = &lazy_msg {
                    println!("\n{}", msg);
                }
            }
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError)
        }
        server_prot::response::StatusResponse::NOT_COVERED => {
            let msg = "Why on earth did the server respond with NOT_COVERED?";
            eprintln!("{}", msg);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
    }
}

fn main(args: &arg_spec::Values) {
    let version = command_spec::get(args, "--version", &arg_spec::truthy()).unwrap_or(false);
    if version {
        command_utils::print_version();
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError);
    }

    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;

    let root_arg =
        command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap();
    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());

    let (connect_flags, json, pretty) = command_utils::get_connect_and_json_flags(args);
    let json_version = command_utils::get_json_version(args);
    let offset_style = command_utils::get_offset_style(args);
    let json = json || json_version.is_some() || pretty;

    let error_flags_args = command_utils::get_error_flags_args(args);
    let error_flags = command_utils::collect_error_flags(
        error_flags_args.color,
        error_flags_args.include_warnings,
        error_flags_args.max_warnings,
        error_flags_args.one_line,
        error_flags_args.list_files,
        error_flags_args.show_all_errors,
        error_flags_args.show_all_branches,
        error_flags_args.unicode,
        error_flags_args.message_width,
    );
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();

    let status_args = StatusArgs {
        root,
        output_json: json,
        output_json_version: json_version,
        offset_style,
        pretty,
        error_flags,
        strip_root,
    };

    check_status(&flowconfig_name, &status_args, &connect_flags)
}

pub(crate) fn status_command() -> command_spec::Command {
    command_spec::command(
        spec(
            "status",
            "(default) Shows current Flow errors by asking the Flow server",
            format!(
                "Usage: {} status [OPTION]... [ROOT]\nShows current Flow errors by asking the Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\nA server will be started if none is running over ROOT.\n\nStatus command options:",
                command_utils::exe_name()
            ),
            true,
        ),
        main,
    )
}

pub(crate) fn check_command() -> command_spec::Command {
    command_spec::command(
        spec(
            "check",
            "(default) Shows current Flow errors by asking the Flow server",
            format!(
                "Usage: {} check [OPTION]... [ROOT]\nShows current Flow errors by asking the Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\nA server will be started if none is running over ROOT.\n\nCheck command options:",
                command_utils::exe_name()
            ),
            true,
        ),
        main,
    )
}

pub(crate) fn default_command(command_info: Vec<(String, String)>) -> command_spec::Command {
    let mut command_info = command_info;
    command_info.sort_by(|(a, _), (b, _)| a.cmp(b));
    let cmd_usage = command_spec::format_two_columns(None, None, 1, &command_info);
    command_spec::command(
        spec(
            "default",
            "",
            format!(
                "Usage: {} [COMMAND] \n\nValid values for COMMAND:\n{}\n\nDefault values if unspecified:\n COMMAND\tstatus\n\nStatus command options:",
                command_utils::exe_name(),
                cmd_usage
            ),
            false,
        ),
        main,
    )
}
