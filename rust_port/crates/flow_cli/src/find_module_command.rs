/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_command_spec::arg_spec;
use flow_server_env::server_prot;

// ***********************************************************************
// flow find-module (get filename of module) command
// ***********************************************************************

fn spec() -> flow_command_spec::Spec {
    let spec = flow_command_spec::Spec::new(
        "find-module",
        "Resolves a module reference to a file",
        flow_command_spec::Visibility::Public,
        format!(
            "Usage: {} find-module [OPTION]... [FILE]...\n\nResolves a module reference to a file\n\nExample usage:\n\t{} find-module moduleref filename\n",
            flow_command_utils::exe_name(),
            flow_command_utils::exe_name()
        ),
    );
    let spec = flow_command_utils::add_base_flags(spec);
    let spec = flow_command_utils::add_connect_and_json_flags(spec);
    let spec = flow_command_utils::add_root_flag(spec);
    let spec = flow_command_utils::add_strip_root_flag(spec);
    let spec = flow_command_utils::add_from_flag(spec);
    let spec = flow_command_utils::add_wait_for_recheck_flag(spec);
    spec.flag(
        "--debug-show-considered-candidates",
        &arg_spec::truthy(),
        "Print all the candidates considered during module resolution to stderr.",
        None,
    )
    .anon("module", &arg_spec::required(None, arg_spec::string()))
    .anon("file", &arg_spec::required(None, arg_spec::string()))
}

fn main(args: &arg_spec::Values) {
    let base_flags = flow_command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let connect_flags = flow_command_utils::get_connect_flags(args);
    let json_flags = flow_command_utils::get_json_flags(args);
    let root_arg =
        flow_command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = flow_command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let wait_for_recheck = flow_command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let show_considered_candidates = flow_command_spec::get(
        args,
        "--debug-show-considered-candidates",
        &arg_spec::truthy(),
    )
    .unwrap();
    let moduleref = flow_command_spec::get(
        args,
        "module",
        &arg_spec::required(None, arg_spec::string()),
    )
    .unwrap();
    let filename =
        flow_command_spec::get(args, "file", &arg_spec::required(None, arg_spec::string()))
            .unwrap();

    let root = flow_command_utils::guess_root(
        &flowconfig_name,
        match root_arg.as_deref() {
            Some(root) => Some(root),
            None => Some(&filename),
        },
    );

    let filename = flow_command_utils::expand_path(&filename);
    let request = server_prot::request::Command::FIND_MODULE {
        moduleref,
        filename,
        wait_for_recheck,
    };

    let response = flow_command_utils::connect_and_make_request(
        &flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    let (resolution_result, failed_candidates) = match response {
        server_prot::response::Response::FIND_MODULE((Some(file_key), failed_candidates)) => {
            let file = file_key.to_absolute();
            if strip_root {
                (
                    flow_common::files::relative_path(&root, &file),
                    failed_candidates,
                )
            } else {
                (file, failed_candidates)
            }
        }
        server_prot::response::Response::FIND_MODULE((None, failed_candidates)) => {
            ("(unknown)".to_string(), failed_candidates)
        }
        response => {
            flow_command_utils::failwith_bad_response(&request, &response);
        }
    };

    if show_considered_candidates {
        eprintln!("The following candidates are considered during module resolution:");
        for candidate in &failed_candidates {
            eprintln!(" - {}", candidate);
        }
    }

    if json_flags.json || json_flags.pretty {
        flow_hh_json::print_json_endline(
            json_flags.pretty,
            &serde_json::json!({
                "file": resolution_result,
            }),
        );
    } else {
        println!("{}", resolution_result);
    }
}

pub(crate) fn command() -> flow_command_spec::Command {
    flow_command_spec::command(spec(), main)
}
