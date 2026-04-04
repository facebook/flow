/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Port of startCommand.ml
// The OCaml version constructs FlowServerMonitorOptions and calls FlowServerMonitor.daemonize.
// In Rust, we spawn a child process running the "server" subcommand.

use std::path::Path;

use flow_server_files::server_files_js;

use crate::command_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "start",
        "Starts a Flow server",
        "Usage: flow start [OPTION]... [ROOT]\n\nStarts a Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n".to_string(),
    )
    .flag(
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
        "Set the name of the flow configuration file. (default: .flowconfig)",
        Some("FLOW_CONFIG_NAME"),
    )
    .flag(
        "--no-flowlib",
        &command_spec::truthy(),
        "Do not use the bundled flowlib",
        None,
    )
    .flag(
        "--ignore-version",
        &command_spec::truthy(),
        "Ignore the version constraint in .flowconfig",
        Some("FLOW_IGNORE_VERSION"),
    )
    .flag(
        "--wait",
        &command_spec::truthy(),
        "Wait for the server to finish initializing",
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
        "--wait-for-recheck",
        &command_spec::optional(command_spec::string()),
        "Wait for recheck before responding (accepted but ignored)",
        None,
    )
    .flag(
        "--file-watcher",
        &command_spec::optional(command_spec::string()),
        "File watcher to use (accepted but ignored, no file watcher in Rust port)",
        None,
    )
    .flag(
        "--log-file",
        &command_spec::optional(command_spec::string()),
        "Path to the server log file",
        None,
    )
    .flag(
        "--monitor-log-file",
        &command_spec::optional(command_spec::string()),
        "Path to the monitor log file (accepted but ignored)",
        None,
    )
    .flag(
        "--long-lived-workers",
        &command_spec::optional(command_spec::string()),
        "Enable long-lived workers (accepted but ignored)",
        None,
    )
    .flag(
        "--no-cgroup",
        &command_spec::truthy(),
        "Don't automatically run in a cgroup (accepted but ignored)",
        None,
    )
    .flag(
        "--lazy-mode",
        &command_spec::optional(command_spec::string()),
        "Set the lazy mode (fs, ide, or none)",
        None,
    )
    .flag(
        "--lazy",
        &command_spec::truthy(),
        "Enable lazy mode (equivalent to --lazy-mode fs)",
        None,
    )
    .flag(
        "--autostop",
        &command_spec::truthy(),
        "Auto-stop when last client disconnects (accepted but ignored)",
        None,
    )
    .flag(
        "--sharedmemory-hash-table-pow",
        &command_spec::optional(command_spec::string()),
        "Shared memory hash table size (accepted but ignored)",
        None,
    )
    .flag(
        "--quiet",
        &command_spec::truthy(),
        "Quiet mode (accepted but ignored)",
        None,
    )
    .flag(
        "--verbose",
        &command_spec::truthy(),
        "Verbose output (accepted but ignored)",
        None,
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

fn main(args: &command_spec::Values) {
    let flowconfig_name = command_spec::get(
        args,
        "--flowconfig-name",
        &command_spec::required(Some(".flowconfig".to_string()), command_spec::string()),
    )
    .unwrap();
    let no_flowlib = command_spec::get(args, "--no-flowlib", &command_spec::truthy()).unwrap();
    let ignore_version =
        command_spec::get(args, "--ignore-version", &command_spec::truthy()).unwrap();
    let wait = command_spec::get(args, "--wait", &command_spec::truthy()).unwrap();
    let lazy_mode_flag = command_spec::get(
        args,
        "--lazy-mode",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let lazy_flag = command_spec::get(args, "--lazy", &command_spec::truthy()).unwrap();
    let verbose = command_spec::get(args, "--verbose", &command_spec::truthy()).unwrap();
    let log_file_flag = command_spec::get(
        args,
        "--log-file",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let root_arg = command_spec::get(
        args,
        "root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    // Determine lazy_mode override from --lazy-mode and --lazy flags
    let lazy_mode = if let Some(ref mode) = lazy_mode_flag {
        match mode.as_str() {
            "fs" | "ide" => Some(true),
            "none" => Some(false),
            _ => Some(true),
        }
    } else if lazy_flag {
        Some(true)
    } else {
        None
    };
    let overrides = command_utils::MakeOptionsOverrides {
        lazy_mode,
        ..Default::default()
    };

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let mut options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        overrides,
    );
    if verbose {
        use std::sync::Arc;
        Arc::make_mut(&mut options).verbose = Some(Arc::new(flow_common::verbose::Verbose {
            indent: 0,
            depth: 1,
            enabled_during_flowlib: false,
            focused_files: None,
        }));
    }

    let tmp_dir = std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned());

    let lock_path = server_files_js::lock_file(&flowconfig_name, &tmp_dir, &root);
    if Path::new(&lock_path).exists() {
        eprintln!(
            "Error: There is already a server running for {}",
            root.display()
        );
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::LockStolen);
    }

    let log_path = log_file_flag
        .unwrap_or_else(|| server_files_js::log_file(&flowconfig_name, &tmp_dir, &root));

    // Spawn the server as a child process using the current binary with --server-mode
    let socket_path = server_files_js::socket_file(&flowconfig_name, &tmp_dir, &root);
    let ready_path = format!("{}.ready", socket_path);
    let _ = std::fs::remove_file(&ready_path);

    let log_file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_path)
        .unwrap_or_else(|e| {
            eprintln!("Error: failed to open log file {}: {}", log_path, e);
            std::process::exit(1);
        });
    let log_file_err = log_file.try_clone().unwrap();

    let exe = std::env::current_exe().unwrap_or_else(|e| {
        eprintln!("Error: failed to get current exe: {}", e);
        std::process::exit(1);
    });

    let mut cmd = std::process::Command::new(&exe);
    cmd.arg("server");
    if no_flowlib {
        cmd.arg("--no-flowlib");
    }
    if ignore_version {
        cmd.arg("--ignore-version");
    }
    if let Some(ref mode) = lazy_mode_flag {
        cmd.arg("--lazy-mode").arg(mode);
    } else if lazy_flag {
        cmd.arg("--lazy");
    }
    cmd.arg("--flowconfig-name").arg(&flowconfig_name);
    cmd.arg("--signal-ready");
    cmd.arg(root.to_string_lossy().as_ref());
    cmd.env("FLOW_TEMP_DIR", &tmp_dir);
    if let Ok(workers) = std::env::var("FLOW_MAX_WORKERS") {
        cmd.env("FLOW_MAX_WORKERS", workers);
    }
    cmd.stdout(std::process::Stdio::from(log_file));
    cmd.stderr(std::process::Stdio::from(log_file_err));

    let child = cmd.spawn().unwrap_or_else(|e| {
        eprintln!("Failed to spawn server: {}", e);
        std::process::exit(1);
    });

    eprintln!("Spawned flow server (pid={})", child.id());
    eprintln!("Logs will go to {}", log_path);

    if wait {
        // Wait for the server to signal readiness via the ready file
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_secs(120);
        loop {
            if std::path::Path::new(&ready_path).exists() {
                let _ = std::fs::remove_file(&ready_path);
                break;
            }
            if start.elapsed() > timeout {
                eprintln!("Timeout waiting for server to start");
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
