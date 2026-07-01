/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Read;

use flow_server_env::server_prot;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

pub(crate) fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "query",
        "Queries per-file metadata as JSON",
        command_spec::Visibility::Internal,
        format!(
            "Usage: {} query [OPTION]... <json-query>\n\ne.g. {} query '{{\"fields\": [\"name\"]}}'\n",
            command_utils::exe_name(),
            command_utils::exe_name()
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    spec.flag("-j", &arg_spec::truthy(), "Read the query from stdin", None)
        .flag("--json-command", &arg_spec::truthy(), "Alias for -j", None)
        .anon("query", &arg_spec::optional(arg_spec::string()))
}

pub(crate) fn run(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let connect_flags = command_utils::get_connect_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let from_stdin = command_spec::get(args, "-j", &arg_spec::truthy()).unwrap()
        || command_spec::get(args, "--json-command", &arg_spec::truthy()).unwrap();
    let positional =
        command_spec::get(args, "query", &arg_spec::optional(arg_spec::string())).unwrap();

    // The query body comes from exactly one source: -j (stdin) or the positional argument.
    let query_json = match (from_stdin, positional) {
        (true, Some(_)) => flow_common_exit_status::exit_with_msg(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            "-j/--json-command and a positional query are mutually exclusive",
        ),
        (true, None) => {
            let mut buf = String::new();
            if let Err(e) = std::io::stdin().read_to_string(&mut buf) {
                flow_common_exit_status::exit_with_msg(
                    flow_common_exit_status::FlowExitStatus::InputError,
                    &format!("Failed to read query from stdin: {e}"),
                );
            }
            buf
        }
        (false, Some(json)) => json,
        (false, None) => flow_common_exit_status::exit_with_msg(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            "A query argument or -j is required",
        ),
    };

    let query: server_prot::request::Query = match serde_json::from_str(&query_json) {
        Ok(query) => query,
        Err(e) => flow_common_exit_status::exit_with_msg(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            &format!("Invalid JSON query: {e}"),
        ),
    };

    let root = command_utils::guess_root(&base_flags.flowconfig_name, root_arg.as_deref());
    let request = server_prot::request::Command::QUERY { query };
    let response = command_utils::connect_and_make_request(
        &base_flags.flowconfig_name,
        &connect_flags,
        &root,
        &request,
    );
    match response {
        server_prot::response::Response::QUERY(Err(msg)) => {
            eprintln!("{}", msg);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError);
        }
        server_prot::response::Response::QUERY(Ok(json)) => {
            println!("{}", json);
        }
        response => {
            command_utils::failwith_bad_response(&request, &response);
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), run)
}
