/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_command_spec::arg_spec;
use flow_server_env::server_prot;

use crate::type_at_pos_command;

const CMD_NAME: &str = "inlay-hint_unstable_exposed_for_testing";

// ***********************************************************************
// flow type-at-pos command
// ***********************************************************************

fn spec() -> flow_command_spec::Spec {
    let exe_name = flow_command_utils::exe_name();
    let spec = flow_command_spec::Spec::new(
        CMD_NAME,
        "Compute all inlay hints available in a file (testing only, no stability guarantee)",
        flow_command_spec::Visibility::Experimental,
        format!(
            "Usage: {exe_name} {CMD_NAME} [OPTION]... [FILE]\n\ne.g. {exe_name} {CMD_NAME} foo.js\nor   {exe_name} {CMD_NAME} < foo.js\n"
        ),
    );
    let spec = flow_command_utils::add_base_flags(spec);
    let spec = flow_command_utils::add_connect_flags(spec);
    let spec = flow_command_utils::add_root_flag(spec);
    let spec = flow_command_utils::add_strip_root_flag(spec);
    let spec = flow_command_utils::add_verbose_flags(spec);
    let spec = flow_command_utils::add_from_flag(spec);
    let spec = flow_command_utils::add_path_flag(spec);
    let spec = flow_command_utils::add_wait_for_recheck_flag(spec);
    spec.flag(
        "--omit-typearg-defaults",
        &arg_spec::truthy(),
        "Omit type arguments when defaults exist and match the provided type argument",
        None,
    )
    .flag(
        "--max-depth",
        &arg_spec::required(Some(40), arg_spec::int()),
        "Maximum depth of type (default 40)",
        None,
    )
    .flag(
        "--verbose-normalizer",
        &arg_spec::truthy(),
        "Print verbose info during normalization",
        None,
    )
    .flag(
        "--do_not_use_typed_AST_for_imports",
        &arg_spec::truthy(),
        // internal flag for regression purposes
        "",
        None,
    )
    .anon("args", &arg_spec::list_of(arg_spec::string()))
}

fn handle_response(strip_root: Option<&str>, response: server_prot::response::inlay_hint::Item) {
    let server_prot::response::inlay_hint::Item {
        cursor_loc,
        type_loc,
        tys,
        refining_locs,
        refinement_invalidated: _,
        documentation,
    } = response;
    let file = cursor_loc.source.as_ref().expect("No source");
    let cursor_loc_str = format!(
        "At {}:{}:{}",
        flow_common::reason::string_of_source(strip_root, file),
        cursor_loc.start.line,
        cursor_loc.start.column + 1
    );
    println!("{}", cursor_loc_str);
    type_at_pos_command::handle_friendly_result(
        strip_root,
        &type_loc,
        &tys,
        &refining_locs,
        documentation.as_deref(),
    );
    println!();
}

fn main(args: &arg_spec::Values) {
    let base_flags = flow_command_utils::get_base_flags(args);
    let connect_flags = flow_command_utils::get_connect_flags(args);
    let root_arg =
        flow_command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = flow_command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let verbose = flow_command_utils::verbose_flags(args);
    let path =
        flow_command_spec::get(args, "--path", &arg_spec::optional(arg_spec::string())).unwrap();
    let wait_for_recheck = flow_command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let omit_targ_defaults =
        flow_command_spec::get(args, "--omit-typearg-defaults", &arg_spec::truthy()).unwrap();
    let max_depth = flow_command_spec::get(
        args,
        "--max-depth",
        &arg_spec::required(Some(40), arg_spec::int()),
    )
    .unwrap();
    let verbose_normalizer =
        flow_command_spec::get(args, "--verbose-normalizer", &arg_spec::truthy()).unwrap();
    let no_typed_ast_for_imports = flow_command_spec::get(
        args,
        "--do_not_use_typed_AST_for_imports",
        &arg_spec::truthy(),
    )
    .unwrap();
    let raw_args = flow_command_spec::get(args, "args", &arg_spec::list_of(arg_spec::string()))
        .unwrap()
        .unwrap_or_default();

    let file = match raw_args.as_slice() {
        [file] => {
            let file = flow_command_utils::expand_path(file);
            flow_server_utils::file_input::FileInput::FileName(file)
        }
        [] => flow_command_utils::get_file_from_filename_or_stdin(CMD_NAME, None, path.as_deref()),
        _ => {
            flow_command_spec::usage(&spec());
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            );
        }
    };
    let flowconfig_name = base_flags.flowconfig_name;
    let root = flow_command_utils::find_a_root(&flowconfig_name, root_arg.as_deref(), Some(&file));
    let request = server_prot::request::Command::INLAY_HINT(server_prot::inlay_hint_options::T {
        input: file,
        verbose,
        omit_targ_defaults,
        wait_for_recheck,
        verbose_normalizer,
        max_depth,
        no_typed_ast_for_imports,
    });
    let response = flow_command_utils::connect_and_make_request(
        &flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::INLAY_HINT(Err(err)) => {
            eprintln!("{}", err);
        }
        server_prot::response::Response::INLAY_HINT(Ok(response)) => {
            let strip_root = if strip_root {
                Some(root.to_string_lossy().to_string())
            } else {
                None
            };
            for item in response {
                handle_response(strip_root.as_deref(), item);
            }
        }
        response => {
            flow_command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> flow_command_spec::Command {
    flow_command_spec::command(spec(), main)
}
