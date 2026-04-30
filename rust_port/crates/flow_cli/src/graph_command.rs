/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use flow_server_env::server_prot;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;
use crate::cycle_command;

#[derive(Clone)]
enum GraphSubcommand {
    Cycle,
    DepGraph,
}

fn dep_graph_spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "dep-graph",
        "Output .dot file for the dependency graph of a repository",
        command_spec::Visibility::Public,
        format!(
            "Usage: {} graph dep-graph [OPTION]...\n\ne.g. {} graph dep-graph --out path/to/output --root path/to/root\ne.g. {} graph dep-graph --out path/to/output \nor   {} graph dep-graph --strip-root --out path/to/output --root path/to/root\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\nIf --strip-root is specified, the file paths in the output graph\nwill be relative to ROOT.\nThe graph will be output in FILE.\n\n",
            command_utils::exe_name(),
            command_utils::exe_name(),
            command_utils::exe_name(),
            command_utils::exe_name()
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_flags(spec);
    let spec = command_utils::add_strip_root_flag(spec);
    let spec = spec
        .flag(
            "--out",
            &arg_spec::required(None, arg_spec::string()),
            "Location to print the output file",
            None,
        )
        .flag(
            "--types",
            &arg_spec::truthy(),
            "Only consider type dependencies",
            None,
        );
    command_utils::add_root_flag(spec)
}

fn dep_graph_main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let connect_flags = command_utils::get_connect_flags(args);
    let strip_root = command_spec::get(args, "--strip-root", &arg_spec::truthy()).unwrap();
    let outfile =
        command_spec::get(args, "--out", &arg_spec::required(None, arg_spec::string())).unwrap();
    let types_only = command_spec::get(args, "--types", &arg_spec::truthy()).unwrap();
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();

    let root = command_utils::guess_root(&base_flags.flowconfig_name, root_arg.as_deref());
    // Create the outfile if it doesn't already exist
    let outfile = flow_common::files::imaginary_realpath(&outfile);
    // connect to server
    let request = server_prot::request::Command::GRAPH_DEP_GRAPH {
        root: root.to_string_lossy().to_string(),
        strip_root,
        outfile,
        types_only,
    };
    let response = command_utils::connect_and_make_request(
        &base_flags.flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::GRAPH_DEP_GRAPH(Ok(())) => {}
        server_prot::response::Response::GRAPH_DEP_GRAPH(Err(msg)) => {
            eprintln!("{}", msg);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError);
        }
        response => {
            command_utils::failwith_bad_response(&request, &response);
        }
    }
}

fn dep_graph_command() -> command_spec::Command {
    command_spec::command(dep_graph_spec(), dep_graph_main)
}

fn cycle_subcommand() -> command_spec::Command {
    let mut spec = cycle_command::spec();
    spec.usage = format!(
        "Usage: {} graph cycle [OPTION]...\n\ne.g. {} graph cycle path/to/file.js \n",
        command_utils::exe_name(),
        command_utils::exe_name()
    );
    command_spec::command(spec, cycle_command::run)
}

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "graph",
        "Outputs dependency graphs of flow repositories",
        command_spec::Visibility::Public,
        format!(
            "Usage: {} graph SUBCOMMAND [OPTIONS]...\nOutputs dependency graphs of flow repositories\n\nSUBCOMMANDS:\ncycle: Produces a graph of the dependency cycle containing the input file\ndep-graph: Produces the dependency graph of a repository\n",
            command_utils::exe_name()
        ),
    )
    .anon(
        "subcommand",
        &arg_spec::required(
            None,
            arg_spec::command_flag(vec![
                ("cycle", GraphSubcommand::Cycle),
                ("dep-graph", GraphSubcommand::DepGraph),
            ]),
        ),
    )
}

fn main(args: &arg_spec::Values) {
    let (subcommand, argv) = command_spec::get(
        args,
        "subcommand",
        &arg_spec::required(
            None,
            arg_spec::command_flag(vec![
                ("cycle", GraphSubcommand::Cycle),
                ("dep-graph", GraphSubcommand::DepGraph),
            ]),
        ),
    )
    .unwrap();
    match subcommand {
        GraphSubcommand::Cycle => command_utils::run_command(&cycle_subcommand(), &argv),
        GraphSubcommand::DepGraph => command_utils::run_command(&dep_graph_command(), &argv),
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
