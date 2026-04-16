/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//***********************************************************************
// flow force-recheck
//***********************************************************************

use flow_server_env::server_prot;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "force-recheck",
        "Forces the server to recheck a given list of files",
        format!(
            "Usage: {} force-recheck [OPTION]... [FILES]\nForces the Flow server to recheck a given list of files.\n\nFILES may be omitted if and only if --input-file is used.\n",
            command_utils::exe_name()
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_connect_flags(spec);
    let spec = command_utils::add_root_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    spec
    .flag(
        "--missed-changes",
        &arg_spec::truthy(),
        "Invalidate the server's view of the file system (e.g. unknown changes occurred)",
        None,
    )
    .flag(
        "--changed-mergebase",
        &arg_spec::truthy(),
        "Notify the server that the SCM mergebase has changed",
        None,
    )
    .flag(
        "--focus",
        &arg_spec::truthy(),
        "If the server is running in lazy mode, force it to focus on these files",
        None,
    )
    .flag(
        "--input-file",
        &arg_spec::optional(arg_spec::string()),
        "File containing list of files to recheck, one per line. If -, list of files is read from the standard input.",
        None,
    )
    .anon("files", &arg_spec::list_of(arg_spec::string()))
}

fn find_parent_that_exists(path: &str) -> String {
    if std::path::Path::new(path).exists() {
        return path.to_string();
    }
    let newpath = std::path::Path::new(path).parent();
    // dirname called repeatedly should eventually return ".", which should
    // always exist. But no harm in being overly cautious. Let's detect
    // infinite recursion
    match newpath {
        Some(parent) if parent.to_string_lossy() != path => {
            find_parent_that_exists(&parent.to_string_lossy())
        }
        _ => path.to_string(),
    }
}

fn main(args: &arg_spec::Values) {
    let base_flags = command_utils::get_base_flags(args);
    let connect_flags = command_utils::get_connect_flags(args);
    let root = command_spec::get(args, "--root", &arg_spec::optional(arg_spec::string())).unwrap();
    let missed_changes = command_spec::get(args, "--missed-changes", &arg_spec::truthy()).unwrap();
    let changed_mergebase =
        command_spec::get(args, "--changed-mergebase", &arg_spec::truthy()).unwrap();
    let focus = command_spec::get(args, "--focus", &arg_spec::truthy()).unwrap();
    let input_file = command_spec::get(
        args,
        "--input-file",
        &arg_spec::optional(arg_spec::string()),
    )
    .unwrap();
    let files = command_spec::get(args, "files", &arg_spec::list_of(arg_spec::string()))
        .unwrap()
        .unwrap_or_default();

    if input_file.is_none() && files.is_empty() {
        command_spec::usage(&spec());
        let msg = "FILES may be omitted if and only if --input-file is used";
        eprintln!("{}", msg);
        flow_common_exit_status::exit(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
        )
    }

    let files = command_utils::get_filenames_from_input(true, input_file.as_deref(), Some(&files));

    let flowconfig_name = base_flags.flowconfig_name;
    let root_hint = match (&root, files.first()) {
        (Some(root), _) => Some(root.clone()),
        (None, Some(file)) => Some(find_parent_that_exists(file)),
        (None, None) => None,
    };
    let root = command_utils::guess_root(&flowconfig_name, root_hint.as_deref());

    let files: Vec<String> = files
        .iter()
        .map(|f| command_utils::get_path_of_file(f))
        .collect();

    let request = server_prot::request::Command::FORCE_RECHECK {
        files,
        focus,
        missed_changes,
        changed_mergebase,
    };
    let response =
        command_utils::connect_and_make_request(&flowconfig_name, &connect_flags, &root, &request);
    match response {
        server_prot::response::Response::FORCE_RECHECK => {}
        response => command_utils::failwith_bad_response(&request, &response),
    }

    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError)
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
