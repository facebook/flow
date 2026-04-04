/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Port of forceRecheckCommand.ml

use crate::command_connect;
use crate::command_connect::ServerRequest;
use crate::command_connect::ServerResponse;
use crate::command_spec;
use crate::command_utils;

// (* find_parent_that_exists *)
fn find_parent_that_exists(path: &str) -> String {
    let path = std::path::Path::new(path);
    if path.exists() {
        return path.to_string_lossy().into_owned();
    }
    match path.parent() {
        // (* dirname called repeatedly should eventually return ".", which should
        //  * always exist. But no harm in being overly cautious. Let's detect
        //  * infinite recursion *)
        Some(parent) if parent != path => find_parent_that_exists(&parent.to_string_lossy()),
        _ => path.to_string_lossy().into_owned(),
    }
}

// (* let spec = { CommandSpec.name = "force-recheck"; ... } *)
fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "force-recheck",
        "Forces the server to recheck a given list of files",
        "Usage: flow force-recheck [OPTION]... [FILES]\n\nForces the Flow server to recheck a given list of files.\n\nFILES may be omitted if and only if --input-file is used.\n".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
    )
    .flag(
        "--focus",
        &command_spec::truthy(),
        "If the server is running in lazy mode, force it to focus on these files",
        None,
    )
    .flag(
        "--missed-changes",
        &command_spec::truthy(),
        "Invalidate the server's view of the file system (e.g. unknown changes occurred)",
        None,
    )
    .flag(
        "--changed-mergebase",
        &command_spec::truthy(),
        "Notify the server that the SCM mergebase has changed",
        None,
    )
    .flag(
        "--input-file",
        &command_spec::optional(command_spec::string()),
        "File containing list of files to recheck, one per line. If -, list of files is read from the standard input.",
        None,
    )
    .flag(
        "--ignore-version",
        &command_spec::truthy(),
        "Ignore the version constraint in .flowconfig",
        Some("FLOW_IGNORE_VERSION"),
    )
    .flag(
        "--no-flowlib",
        &command_spec::truthy(),
        "Do not use the bundled flowlib",
        None,
    )
    .flag(
        "--root",
        &command_spec::optional(command_spec::string()),
        "Project root directory containing the .flowconfig",
        None,
    )
    .flag(
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    // Flags accepted for compatibility with the OCaml test harness but ignored:
    .flag(
        "--no-auto-start",
        &command_spec::truthy(),
        "Don't auto-start the server (accepted but ignored)",
        None,
    )
    .flag(
        "--lazy",
        &command_spec::truthy(),
        "Lazy mode (accepted but ignored)",
        None,
    )
    .flag(
        "--verbose",
        &command_spec::truthy(),
        "Verbose output (accepted but ignored)",
        None,
    )
    .flag(
        "--retries",
        &command_spec::optional(command_spec::string()),
        "Number of retries (accepted but ignored)",
        None,
    )
    .flag(
        "--retry-if-init",
        &command_spec::optional(command_spec::string()),
        "Retry if initializing (accepted but ignored)",
        None,
    )
    .flag(
        "--timeout",
        &command_spec::optional(command_spec::string()),
        "Timeout (accepted but ignored)",
        None,
    )
    .flag(
        "--temp-dir",
        &command_spec::optional(command_spec::string()),
        "Temp directory (accepted but ignored)",
        None,
    )
    .flag(
        "--quiet",
        &command_spec::truthy(),
        "Quiet mode (accepted but ignored)",
        None,
    )
    .anon("files", &command_spec::list_of(command_spec::string()))
}

// (* let main base_flags connect_flags root missed_changes changed_mergebase focus input_file files () = *)
fn main(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let focus = command_spec::get(args, "--focus", &command_spec::truthy()).unwrap();
    let missed_changes =
        command_spec::get(args, "--missed-changes", &command_spec::truthy()).unwrap();
    let changed_mergebase =
        command_spec::get(args, "--changed-mergebase", &command_spec::truthy()).unwrap();
    let input_file = command_spec::get(
        args,
        "--input-file",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let no_flowlib = command_spec::get(args, "--no-flowlib", &command_spec::truthy()).unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let root_flag = command_spec::get(
        args,
        "--root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let files = command_spec::get(
        args,
        "files",
        &command_spec::list_of(command_spec::string()),
    )
    .unwrap();

    // (* match (input_file, files) with
    //    | (None, (None | Some [])) -> ... *)
    if input_file.is_none() && files.is_empty() {
        eprintln!(
            "{}",
            command_spec::command(spec(), |_| {}).string_of_usage()
        );
        eprintln!("FILES may be omitted if and only if --input-file is used");
        flow_common_exit_status::exit(
            flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
        )
    }

    // (* let files = get_filenames_from_input ~allow_imaginary:true input_file files *)
    let filenames = command_utils::get_filenames_from_input_with_allow_imaginary(
        input_file.as_deref(),
        Some(&files),
        true,
    );

    // (* let root = guess_root flowconfig_name (match (root, files) with ...) *)
    let root = if let Some(root_flag) = &root_flag {
        command_utils::guess_root(&flowconfig_name, Some(root_flag))
    } else if let Some(first_file) = filenames.first() {
        let parent = find_parent_that_exists(first_file);
        command_utils::guess_root(&flowconfig_name, Some(&parent))
    } else {
        command_utils::guess_root(&flowconfig_name, None)
    };

    let options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        command_utils::MakeOptionsOverrides::default(),
    );

    // (* let files = Base.List.map ~f:get_path_of_file files *)
    // In Rust, filenames are already strings; the OCaml get_path_of_file
    // extracts the path string from File_input.t.
    let request = ServerRequest::ForceRecheck {
        files: filenames,
        focus,
        missed_changes,
        changed_mergebase,
    };

    match command_connect::connect_and_make_request(
        &flowconfig_name,
        options.temp_dir.as_str(),
        &root,
        request,
    ) {
        Ok(ServerResponse::ForceRecheck) => {
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoError)
        }
        Ok(ServerResponse::Error { message }) => {
            eprintln!("Error: {}", message);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Ok(response) => {
            eprintln!("Unexpected response from server: {:?}", response);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
        Err(command_connect::ConnectError::ServerNotRunning) => {
            eprintln!("There is no Flow server running in '{}'", root.display());
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::NoServerRunning)
        }
        Err(err) => {
            eprintln!("{}", err);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError)
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
