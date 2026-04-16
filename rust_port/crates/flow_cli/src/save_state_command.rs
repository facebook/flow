/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Write;

use flow_server_env::server_prot;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

//***********************************************************************
// flow save-state command
//***********************************************************************

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "save-state",
        "Tell the server to create a saved-state file",
        format!(
            "Usage: {} save-state [OPTION]...\n\ne.g. {} save-state --root path/to/root --out path/to/my_saved_state\n",
            command_utils::exe_name(),
            command_utils::exe_name()
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    spec.flag(
        "--scm",
        &arg_spec::no_arg(),
        "Write to the expected path for the SCM fetcher",
        None,
    )
    .flag(
        "--out",
        &arg_spec::optional(arg_spec::string()),
        "The path to the new saved-state file",
        None,
    )
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let connect_flags = command_utils::get_connect_flags(args);
    let root_arg =
        command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let scm = command_spec::get(args, "--scm", &arg_spec::no_arg()).unwrap();
    let out = command_spec::get(args, "--out", &arg_spec::optional(arg_spec::string())).unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());

    let out = match (scm, out) {
        (None, None) | (Some(false), None) => {
            eprintln!("--out or --scm is required");
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            )
        }
        (Some(true), Some(_)) => {
            eprintln!("--out and --scm are mutually exclusive");
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            )
        }
        (Some(true), None) => server_prot::request::SaveStateOut::Scm,
        (_, Some(out)) => server_prot::request::SaveStateOut::File(std::path::PathBuf::from(
            flow_common::files::imaginary_realpath(&out),
        )),
    };

    let request = server_prot::request::Command::SAVE_STATE { out };
    let response =
        command_utils::connect_and_make_request(&flowconfig_name, &connect_flags, &root, &request);
    match response {
        server_prot::response::Response::SAVE_STATE(Err(msg)) => {
            eprintln!("{}", msg);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        server_prot::response::Response::SAVE_STATE(Ok(msg)) => {
            println!("{}", msg);
            std::io::stdout().flush().expect("failed to flush stdout");
        }
        response => command_utils::failwith_bad_response(&request, &response),
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
