/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Write;

use flow_parser::file_key;
use flow_parser::file_key::FileKey;
use flow_parser::loc::Loc;
use flow_parser::loc::Position;
use flow_parser_utils_output::replacement_printer;
use flow_server_env::server_prot;
use flow_server_utils::file_input::FileInput;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// This module implements the flow command `autofix insert-type` which inserts
// a type annotation in a file at a position.
mod insert_type {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "insert type",
            "[EXPERIMENTAL] Insert type information at file and position",
            command_spec::Visibility::Public,
            format!(
                "Usage: {exe_name} autofix insert-type [OPTION]... [FILE] LINE COLUMN [END_LINE] [END_COLUMN]\n\ne.g. {exe_name} autofix insert-type foo.js 12 3\nor   {exe_name} autofix insert-type 12 3 < foo.js\n"
            ),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_connect_and_json_flags(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_strip_root_flag(spec);
        let spec = command_utils::add_verbose_flags(spec);
        let spec = command_utils::add_from_flag(spec);
        let spec = command_utils::add_path_flag(spec);
        let spec = command_utils::add_wait_for_recheck_flag(spec);
        spec.flag(
            "--strict-location",
            &arg_spec::truthy(),
            "Restrict the number of valid positions for each annotation",
            None,
        )
        .flag(
            "--in-place",
            &arg_spec::truthy(),
            "Overwrite the input file or file specified by the path flag",
            None,
        )
        .flag(
            "--omit-typearg-defaults",
            &arg_spec::truthy(),
            "Omit type arguments when defaults exist and match the provided type argument",
            None,
        )
        .anon("args", &arg_spec::list_of(arg_spec::string()))
    }

    fn handle_error(code: flow_common_exit_status::FlowExitStatus, msg: &str) -> ! {
        eprintln!("{}", msg);
        flow_common_exit_status::exit(code);
    }

    // Returns (loc, source_path_opt). The source path is a raw absolute string;
    // File_key construction is deferred until after roots are initialized.
    fn parse_args(args: &[String]) -> (Loc, Option<String>) {
        let parse_pos = |line: &str, col: &str| -> Position {
            let line: i32 = line.parse().unwrap_or_else(|_| {
                handle_error(
                    flow_common_exit_status::FlowExitStatus::UnknownError,
                    "flow autofix insert-type: failed to parse position",
                )
            });
            let col: i32 = col.parse().unwrap_or_else(|_| {
                handle_error(
                    flow_common_exit_status::FlowExitStatus::UnknownError,
                    "flow autofix insert-type: failed to parse position",
                )
            });
            let (line, column) = command_utils::convert_input_pos(line, col);
            Position { line, column }
        };
        match args {
            [start_line, start_col, end_line, end_col] => {
                let start = parse_pos(start_line, start_col);
                let end = parse_pos(end_line, end_col);
                (
                    Loc {
                        source: None,
                        start,
                        end,
                    },
                    None,
                )
            }
            [start_line, start_col] => {
                let start = parse_pos(start_line, start_col);
                (
                    Loc {
                        source: None,
                        start,
                        end: start,
                    },
                    None,
                )
            }
            [file, rest @ ..] if rest.len() == 2 || rest.len() == 4 => {
                let (loc, _) = parse_args(rest);
                (loc, Some(command_utils::expand_path(file)))
            }
            [] => handle_error(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                "flow autofix insert-type: No position given",
            ),
            _ => handle_error(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                "flow autofix insert-type: Invalid position given",
            ),
        }
    }

    fn select_output_channel(
        in_place: bool,
        path: Option<&str>,
        source_path: Option<&str>,
    ) -> Box<dyn Write> {
        match (in_place, path, source_path) {
            (false, _, _) => Box::new(std::io::stdout()),
            (true, Some(p), _) | (true, None, Some(p)) => {
                Box::new(std::fs::File::create(p).unwrap_or_else(|_| {
                    handle_error(
                        flow_common_exit_status::FlowExitStatus::PathIsNotAFile,
                        &format!("failed to open output file: {}", p),
                    )
                }))
            }
            (true, None, None) => handle_error(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                "Flow: --in-place flag used without input file or explicit path",
            ),
        }
    }

    fn handle_ok(
        patch: &replacement_printer::Patch,
        input: &FileInput,
        in_place: bool,
        path: Option<&str>,
        source_path: Option<&str>,
    ) {
        match input.content_of_file_input() {
            Ok(content) => {
                let mut out = select_output_channel(in_place, path, source_path);
                out.write_all(replacement_printer::print(patch, &content).as_bytes())
                    .unwrap();
                out.flush().unwrap();
            }
            Err(msg) => handle_error(flow_common_exit_status::FlowExitStatus::UnknownError, &msg),
        }
    }

    fn main(args: &arg_spec::Values) {
        let base_flags = command_utils::get_base_flags(args);
        let (connect_flags, json, _pretty) = command_utils::get_connect_and_json_flags(args);
        let root_arg =
            command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
        let strip_root_arg = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
        let verbose = command_utils::verbose_flags(args);
        let path =
            command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
        let wait_for_recheck = command_spec::get(
            args,
            "--wait-for-recheck",
            &arg_spec::optional(arg_spec::bool_flag()),
        )
        .unwrap();
        let location_is_strict =
            command_spec::get(args, "--strict-location", &arg_spec::truthy()).unwrap();
        let in_place = command_spec::get(args, "--in-place", &arg_spec::truthy()).unwrap();
        let omit_targ_defaults =
            command_spec::get(args, "--omit-typearg-defaults", &arg_spec::truthy()).unwrap();
        let raw_args = command_spec::get(args, "args", &arg_spec::list_of(arg_spec::string()))
            .unwrap()
            .unwrap_or_default();

        let (target, source_path) = parse_args(&raw_args);
        let input = command_utils::get_file_from_filename_or_stdin(
            &spec().name,
            source_path.as_deref(),
            path.as_deref(),
        );
        let root = command_utils::get_the_root(
            &base_flags.flowconfig_name,
            root_arg.as_deref(),
            Some(&input),
        );
        // Set root before constructing File_key so the suffix is root-relative,
        // matching the server's File_key values.
        file_key::set_project_root(&root.to_string_lossy());
        let mut target = target;
        match source_path.as_ref() {
            Some(abs) => {
                target.source = Some(FileKey::source_file_of_absolute(abs));
            }
            None => {}
        }
        // TODO Figure out how to implement root striping
        let _strip_root = if strip_root_arg {
            Some(root.clone())
        } else {
            None
        };
        let flowconfig_name = &base_flags.flowconfig_name;
        if !json && verbose.is_some() {
            eprintln!("NOTE: --verbose writes to the server log file");
        }
        let request = server_prot::request::Command::INSERT_TYPE {
            input: input.clone(),
            target,
            verbose,
            location_is_strict,
            wait_for_recheck,
            omit_targ_defaults,
        };
        let result = command_utils::connect_and_make_request(
            flowconfig_name,
            &connect_flags,
            &root,
            &request,
        );
        match result {
            server_prot::response::Response::INSERT_TYPE(Err(err)) => {
                handle_error(flow_common_exit_status::FlowExitStatus::UnknownError, &err)
            }
            // TODO implement a more useful set of error conditions
            server_prot::response::Response::INSERT_TYPE(Ok(resp)) => handle_ok(
                &resp,
                &input,
                in_place,
                path.as_deref(),
                source_path.as_deref(),
            ),
            _ => handle_error(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                "Flow: invalid server response",
            ),
        }
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod missing_local_annot {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "missing-local-annot",
            "[EXPERIMENTAL] automatically fix missing-local-annot errors",
            command_spec::Visibility::Public,
            format!("Usage: {exe_name} autofix missing-local-annot [OPTION]... [FILE]\n"),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_connect_and_json_flags(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_strip_root_flag(spec);
        let spec = command_utils::add_verbose_flags(spec);
        let spec = command_utils::add_from_flag(spec);
        let spec = command_utils::add_path_flag(spec);
        let spec = command_utils::add_wait_for_recheck_flag(spec);
        spec.flag(
            "--in-place",
            &arg_spec::truthy(),
            "Overwrite the input file or file specified by the path flag",
            None,
        )
        .anon("file", &arg_spec::required(None, arg_spec::string()))
    }

    fn handle_error(code: flow_common_exit_status::FlowExitStatus, msg: &str) -> ! {
        eprintln!("{}", msg);
        flow_common_exit_status::exit(code);
    }

    fn select_output_channel(
        in_place: bool,
        path: Option<&str>,
        source_path: &str,
    ) -> Box<dyn Write> {
        match (in_place, path) {
            (false, _) => Box::new(std::io::stdout()),
            (true, Some(p)) => Box::new(std::fs::File::create(p).unwrap_or_else(|_| {
                handle_error(
                    flow_common_exit_status::FlowExitStatus::PathIsNotAFile,
                    &format!("failed to open output file: {}", p),
                )
            })),
            (true, None) => Box::new(std::fs::File::create(source_path).unwrap_or_else(|_| {
                handle_error(
                    flow_common_exit_status::FlowExitStatus::PathIsNotAFile,
                    &format!("failed to open output file: {}", source_path),
                )
            })),
        }
    }

    #[allow(dead_code)]
    const AVG_ERROR_SIZE: usize = 100;

    fn handle_ok(
        patch: &replacement_printer::Patch,
        input: &FileInput,
        in_place: bool,
        path: Option<&str>,
        source_path: &str,
    ) {
        let write_patch = |content: &str| {
            let mut out = select_output_channel(in_place, path, source_path);
            out.write_all(replacement_printer::print(patch, content).as_bytes())
                .unwrap();
            out.flush().unwrap();
        };
        match input.content_of_file_input() {
            Ok(content) => write_patch(&content),
            Err(msg) => handle_error(flow_common_exit_status::FlowExitStatus::UnknownError, &msg),
        }
    }

    fn main(args: &arg_spec::Values) {
        let base_flags = command_utils::get_base_flags(args);
        let (connect_flags, json, _pretty) = command_utils::get_connect_and_json_flags(args);
        let root_arg =
            command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
        let _strip_root_arg = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
        let verbose = command_utils::verbose_flags(args);
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
        let input = command_utils::get_file_from_filename_or_stdin(
            &spec().name,
            Some(source_path.as_str()),
            path.as_deref(),
        );
        let root = command_utils::get_the_root(
            &base_flags.flowconfig_name,
            root_arg.as_deref(),
            Some(&input),
        );
        let flowconfig_name = &base_flags.flowconfig_name;
        if !json && verbose.is_some() {
            eprintln!("NOTE: --verbose writes to the server log file");
        }
        let request = server_prot::request::Command::AUTOFIX_MISSING_LOCAL_ANNOT {
            input: input.clone(),
            verbose,
            wait_for_recheck,
        };
        let result = command_utils::connect_and_make_request(
            flowconfig_name,
            &connect_flags,
            &root,
            &request,
        );
        match result {
            server_prot::response::Response::AUTOFIX_MISSING_LOCAL_ANNOT(Err(err)) => {
                handle_error(flow_common_exit_status::FlowExitStatus::UnknownError, &err)
            }
            server_prot::response::Response::AUTOFIX_MISSING_LOCAL_ANNOT(Ok(patch)) => {
                handle_ok(&patch, &input, in_place, path.as_deref(), &source_path)
            }
            _ => handle_error(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                "Flow: invalid server response",
            ),
        }
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

mod exports {
    use super::*;

    fn spec() -> command_spec::Spec {
        let exe_name = command_utils::exe_name();
        let spec = command_spec::Spec::new(
            "exports",
            "[EXPERIMENTAL] automatically fix signature verification errors",
            command_spec::Visibility::Public,
            format!("Usage: {exe_name} autofix exports [OPTION]... [FILE]\n"),
        );
        let spec = command_utils::add_base_flags(spec);
        let spec = command_utils::add_connect_and_json_flags(spec);
        let spec = command_utils::add_root_flag(spec);
        let spec = command_utils::add_strip_root_flag(spec);
        let spec = command_utils::add_verbose_flags(spec);
        let spec = command_utils::add_from_flag(spec);
        let spec = command_utils::add_path_flag(spec);
        let spec = command_utils::add_wait_for_recheck_flag(spec);
        spec.flag(
            "--in-place",
            &arg_spec::truthy(),
            "Overwrite the input file or file specified by the path flag",
            None,
        )
        .flag(
            "--force",
            &arg_spec::truthy(),
            "Write the results even if errors are encountered",
            None,
        )
        .anon("file", &arg_spec::required(None, arg_spec::string()))
    }

    fn handle_error(code: flow_common_exit_status::FlowExitStatus, msg: &str) -> ! {
        eprintln!("{}", msg);
        flow_common_exit_status::exit(code);
    }

    fn select_output_channel(
        in_place: bool,
        path: Option<&str>,
        source_path: &str,
    ) -> Box<dyn Write> {
        match (in_place, path) {
            (false, _) => Box::new(std::io::stdout()),
            (true, Some(p)) => Box::new(std::fs::File::create(p).unwrap_or_else(|_| {
                handle_error(
                    flow_common_exit_status::FlowExitStatus::PathIsNotAFile,
                    &format!("failed to open output file: {}", p),
                )
            })),
            (true, None) => Box::new(std::fs::File::create(source_path).unwrap_or_else(|_| {
                handle_error(
                    flow_common_exit_status::FlowExitStatus::PathIsNotAFile,
                    &format!("failed to open output file: {}", source_path),
                )
            })),
        }
    }

    #[allow(dead_code)]
    const AVG_ERROR_SIZE: usize = 100;

    fn append_errors(errors: &[String]) -> String {
        let mut buff = String::with_capacity(AVG_ERROR_SIZE * errors.len());
        for error in errors {
            buff.push_str(error);
        }
        buff
    }

    fn handle_ok(
        patch: &replacement_printer::Patch,
        errors: &[String],
        input: &FileInput,
        in_place: bool,
        forced: bool,
        path: Option<&str>,
        source_path: &str,
    ) {
        let write_patch = |content: &str| {
            let mut out = select_output_channel(in_place, path, source_path);
            out.write_all(replacement_printer::print(patch, content).as_bytes())
                .unwrap();
            out.flush().unwrap();
        };
        match (input.content_of_file_input(), errors, forced) {
            (Ok(content), [], _) | (Ok(content), _, true) => {
                eprint!("{}", append_errors(errors));
                write_patch(&content);
            }
            (Ok(_), errors, false) => handle_error(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                &append_errors(errors),
            ),
            (Err(msg), _, _) => {
                handle_error(flow_common_exit_status::FlowExitStatus::UnknownError, &msg)
            }
        }
    }

    fn main(args: &arg_spec::Values) {
        let base_flags = command_utils::get_base_flags(args);
        let (connect_flags, json, _pretty) = command_utils::get_connect_and_json_flags(args);
        let root_arg =
            command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
        let _strip_root_arg = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
        let verbose = command_utils::verbose_flags(args);
        let path =
            command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
        let wait_for_recheck = command_spec::get(
            args,
            "--wait-for-recheck",
            &arg_spec::optional(arg_spec::bool_flag()),
        )
        .unwrap();
        let in_place = command_spec::get(args, "--in-place", &arg_spec::truthy()).unwrap();
        let forced = command_spec::get(args, "--force", &arg_spec::truthy()).unwrap();
        let file =
            command_spec::get(args, "file", &arg_spec::required(None, arg_spec::string())).unwrap();

        let source_path = command_utils::expand_path(&file);
        let input = command_utils::get_file_from_filename_or_stdin(
            &spec().name,
            Some(source_path.as_str()),
            path.as_deref(),
        );
        let root = command_utils::get_the_root(
            &base_flags.flowconfig_name,
            root_arg.as_deref(),
            Some(&input),
        );
        let flowconfig_name = &base_flags.flowconfig_name;
        if !json && verbose.is_some() {
            eprintln!("NOTE: --verbose writes to the server log file");
        }
        let request = server_prot::request::Command::AUTOFIX_EXPORTS {
            input: input.clone(),
            verbose,
            wait_for_recheck,
        };
        let result = command_utils::connect_and_make_request(
            flowconfig_name,
            &connect_flags,
            &root,
            &request,
        );
        match result {
            server_prot::response::Response::AUTOFIX_EXPORTS(Err(err)) => {
                handle_error(flow_common_exit_status::FlowExitStatus::UnknownError, &err)
            }
            server_prot::response::Response::AUTOFIX_EXPORTS(Ok((patch, errors))) => handle_ok(
                &patch,
                &errors,
                &input,
                in_place,
                forced,
                path.as_deref(),
                &source_path,
            ),
            _ => handle_error(
                flow_common_exit_status::FlowExitStatus::UnknownError,
                "Flow: invalid server response",
            ),
        }
    }

    pub(super) fn command() -> command_spec::Command {
        command_spec::command(spec(), main)
    }
}

pub(crate) fn command() -> command_spec::Command {
    let main = |args: &arg_spec::Values| {
        let (cmd, argv) = command_spec::get(
            args,
            "subcommand",
            &arg_spec::required(
                None,
                arg_spec::command_flag(vec![
                    ("insert-type", "insert-type"),
                    ("missing-local-annot", "missing-local-annot"),
                    ("exports", "exports"),
                ]),
            ),
        )
        .unwrap();
        let cmd = match cmd {
            "insert-type" => insert_type::command(),
            "missing-local-annot" => missing_local_annot::command(),
            "exports" => exports::command(),
            _ => unreachable!(),
        };
        command_utils::run_command(&cmd, &argv);
    };
    let spec = command_utils::subcommand_spec(
        "autofix",
        "Automatically insert type annotations",
        command_spec::Visibility::Internal,
        vec![
            (
                "insert-type",
                "insert-type",
                insert_type::command().doc().to_owned(),
            ),
            (
                "missing-local-annot",
                "missing-local-annot",
                missing_local_annot::command().doc().to_owned(),
            ),
            ("exports", "exports", exports::command().doc().to_owned()),
        ],
    );
    command_spec::command(spec, main)
}
