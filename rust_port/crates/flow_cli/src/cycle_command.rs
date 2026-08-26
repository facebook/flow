/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_command_spec::arg_spec;
use flow_server_env::server_prot;

pub(crate) fn spec() -> flow_command_spec::Spec {
    let spec = flow_command_spec::Spec::new(
        "cycle",
        "Output .dot file for cycle containing the given file",
        flow_command_spec::Visibility::Public,
        format!(
            "Usage: {} cycle [OPTION]...\n\ne.g. {} cycle path/to/file.js \n",
            flow_command_utils::exe_name(),
            flow_command_utils::exe_name()
        ),
    );
    let spec = flow_command_utils::add_base_flags(spec);
    let spec = flow_command_utils::add_connect_flags(spec);
    let spec = flow_command_utils::add_root_flag(spec);
    let spec = flow_command_utils::add_strip_root_flag(spec);
    spec.flag(
        "--types",
        &arg_spec::truthy(),
        "Only consider type dependencies",
        None,
    )
    .anon("FILE...", &arg_spec::required(None, arg_spec::string()))
}

pub(crate) fn run(args: &arg_spec::Values) {
    let base_flags = flow_command_utils::get_base_flags(args);
    let connect_flags = flow_command_utils::get_connect_flags(args);
    let root_arg =
        flow_command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let strip_root = flow_command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let types_only = flow_command_spec::get(args, "--types", &arg_spec::truthy()).unwrap();
    let file = flow_command_spec::get(
        args,
        "FILE...",
        &arg_spec::required(None, arg_spec::string()),
    )
    .unwrap();

    let file = flow_command_utils::expand_path(&file);
    let root = flow_command_utils::guess_root(&base_flags.flowconfig_name, root_arg.as_deref());
    let strip_root = |f: &str| {
        if strip_root {
            flow_common::files::relative_path(&root, f)
        } else {
            f.to_string()
        }
    };
    // connect to server
    let request = server_prot::request::Command::CYCLE {
        filename: file,
        types_only,
    };
    let response = flow_command_utils::connect_and_make_request(
        &base_flags.flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::CYCLE(Err(msg)) => {
            eprintln!("{}", msg);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError);
        }
        server_prot::response::Response::CYCLE(Ok(dep_graph)) => {
            // print .dot file to stdout
            println!("digraph {{");
            for (from, dep_fs) in dep_graph {
                for dep_f in dep_fs {
                    println!("  \"{}\" -> \"{}\"", strip_root(&from), strip_root(&dep_f));
                }
            }
            print!("}}");
        }
        response => {
            flow_command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> flow_command_spec::Command {
    flow_command_spec::command(spec(), run)
}
