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

use flow_common::options::SavedStateFetcher;
use flow_server_files::server_files_js;

use crate::command_spec;
use crate::command_utils;

fn saved_state_fetcher_flag() -> command_spec::FlagType<Option<SavedStateFetcher>> {
    command_spec::enum_flag(vec![
        ("none", SavedStateFetcher::DummyFetcher),
        ("local", SavedStateFetcher::LocalFetcher),
        ("scm", SavedStateFetcher::ScmFetcher),
        ("fb", SavedStateFetcher::FbFetcher),
    ])
}

pub(crate) fn start_server(
    flowconfig_name: &str,
    no_flowlib: bool,
    ignore_version: bool,
    wait: bool,
    lazy_mode_flag: Option<&str>,
    lazy_flag: bool,
    verbose: bool,
    log_file_flag: Option<&str>,
    saved_state_fetcher: Option<SavedStateFetcher>,
    saved_state_force_recheck: bool,
    saved_state_no_fallback: bool,
    saved_state_skip_version_check: bool,
    saved_state_verify: bool,
    root: &Path,
) -> Result<(), String> {
    // Determine lazy_mode override from --lazy-mode and --lazy flags
    let lazy_mode = if let Some(mode) = lazy_mode_flag {
        match mode {
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
        saved_state_fetcher,
        saved_state_force_recheck: Some(saved_state_force_recheck),
        saved_state_no_fallback: Some(saved_state_no_fallback),
        saved_state_skip_version_check: Some(saved_state_skip_version_check),
        saved_state_verify: Some(saved_state_verify),
        ..Default::default()
    };

    let mut options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        root,
        flowconfig_name,
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
    let lock_path = server_files_js::lock_file(flowconfig_name, &tmp_dir, root);
    if Path::new(&lock_path).exists() {
        return Err(format!(
            "There is already a server running for {}",
            root.display()
        ));
    }

    let log_path = log_file_flag
        .map(ToOwned::to_owned)
        .or_else(|| std::env::var("FLOW_LOG_FILE").ok())
        .unwrap_or_else(|| server_files_js::log_file(flowconfig_name, &tmp_dir, root));

    // Spawn the server as a child process using the current binary with --server-mode
    let socket_path = server_files_js::socket_file(flowconfig_name, &tmp_dir, root);
    let ready_path = server_files_js::ready_file(flowconfig_name, &tmp_dir, root);
    let _ = std::fs::remove_file(&ready_path);

    let log_file = std::fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&log_path)
        .map_err(|e| format!("failed to open log file {}: {}", log_path, e))?;
    let log_file_err = log_file
        .try_clone()
        .map_err(|e| format!("failed to clone log file {}: {}", log_path, e))?;

    let exe =
        std::env::current_exe().map_err(|e| format!("failed to get current executable: {}", e))?;

    let mut cmd = std::process::Command::new(&exe);
    cmd.arg("server");
    if no_flowlib {
        cmd.arg("--no-flowlib");
    }
    if ignore_version {
        cmd.arg("--ignore-version");
    }
    if let Some(mode) = lazy_mode_flag {
        cmd.arg("--lazy-mode").arg(mode);
    } else if lazy_flag {
        cmd.arg("--lazy");
    }
    if verbose {
        cmd.arg("--verbose");
    }
    if let Some(saved_state_fetcher) = saved_state_fetcher {
        let saved_state_fetcher = match saved_state_fetcher {
            SavedStateFetcher::DummyFetcher => "none",
            SavedStateFetcher::LocalFetcher => "local",
            SavedStateFetcher::ScmFetcher => "scm",
            SavedStateFetcher::FbFetcher => "fb",
        };
        cmd.arg("--saved-state-fetcher").arg(saved_state_fetcher);
    }
    if saved_state_force_recheck {
        cmd.arg("--saved-state-force-recheck");
    }
    if saved_state_no_fallback {
        cmd.arg("--saved-state-no-fallback");
    }
    if saved_state_skip_version_check {
        cmd.arg("--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED");
    }
    if saved_state_verify {
        cmd.arg("--saved-state-verify");
    }
    cmd.arg("--flowconfig-name").arg(flowconfig_name);
    cmd.arg("--signal-ready");
    cmd.arg(root.to_string_lossy().as_ref());
    cmd.env("FLOW_TEMP_DIR", &tmp_dir);
    if let Ok(workers) = std::env::var("FLOW_MAX_WORKERS") {
        cmd.env("FLOW_MAX_WORKERS", workers);
    }
    cmd.stdout(std::process::Stdio::from(log_file));
    cmd.stderr(std::process::Stdio::from(log_file_err));

    let mut child = cmd
        .spawn()
        .map_err(|e| format!("failed to spawn server: {}", e))?;

    let cleanup_startup_artifacts = || {
        let _ = std::fs::remove_file(&lock_path);
        let _ = std::fs::remove_file(&socket_path);
        let _ = std::fs::remove_file(&ready_path);
    };

    eprintln!("Spawned flow server (pid={})", child.id());
    eprintln!("Logs will go to {}", log_path);

    // Always wait for the socket file to exist so clients can connect
    // immediately after `start` returns. This matches the OCaml behavior
    // where the monitor writes the socket file before `start` returns.
    {
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_secs(30);
        loop {
            if std::path::Path::new(&socket_path).exists() {
                break;
            }
            if let Some(_status) = child
                .try_wait()
                .map_err(|e| format!("failed to wait for server process: {}", e))?
            {
                cleanup_startup_artifacts();
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::ServerStartFailed,
                );
            }
            if start.elapsed() > timeout {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
    }

    if wait {
        // Wait for the server to signal readiness via the ready file
        let start = std::time::Instant::now();
        let timeout = std::time::Duration::from_secs(120);
        loop {
            if std::path::Path::new(&ready_path).exists() {
                let _ = std::fs::remove_file(&ready_path);
                break;
            }
            if let Some(_status) = child
                .try_wait()
                .map_err(|e| format!("failed to wait for server process: {}", e))?
            {
                cleanup_startup_artifacts();
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::ServerStartFailed,
                );
            }
            if start.elapsed() > timeout {
                break;
            }
            std::thread::sleep(std::time::Duration::from_millis(50));
        }
    }

    Ok(())
}

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
        Some("NO_FLOWLIB"),
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
        "--saved-state-fetcher",
        &command_spec::optional(saved_state_fetcher_flag()),
        "Which saved state fetcher Flow should use (none, local, scm, fb)",
        None,
    )
    .flag(
        "--saved-state-force-recheck",
        &command_spec::truthy(),
        "Force a lazy server to recheck the changes since the saved state was generated",
        None,
    )
    .flag(
        "--saved-state-no-fallback",
        &command_spec::truthy(),
        "If saved state fails to load, exit instead of falling back to a cold start",
        None,
    )
    .flag(
        "--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED",
        &command_spec::truthy(),
        "",
        Some("FLOW_SAVED_STATE_SKIP_VERSION_CHECK_DO_NOT_USE_OR_YOU_WILL_BE_FIRED"),
    )
    .flag(
        "--saved-state-verify",
        &command_spec::truthy(),
        "Verifies that the saved state matches what is on disk",
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
    .flag(
        "--no-auto-restart",
        &command_spec::truthy(),
        "Don't auto-restart the server (accepted but ignored)",
        None,
    )
    .flag(
        "--all",
        &command_spec::truthy(),
        "Check all files, not just @flow files (accepted but ignored, use flowconfig all=true)",
        None,
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

#[allow(clippy::zombie_processes)] // Intentionally spawning a background daemon server
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
    let saved_state_fetcher = command_spec::get(
        args,
        "--saved-state-fetcher",
        &command_spec::optional(saved_state_fetcher_flag()),
    )
    .unwrap();
    let saved_state_force_recheck =
        command_spec::get(args, "--saved-state-force-recheck", &command_spec::truthy()).unwrap();
    let saved_state_no_fallback =
        command_spec::get(args, "--saved-state-no-fallback", &command_spec::truthy()).unwrap();
    let saved_state_skip_version_check = command_spec::get(
        args,
        "--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED",
        &command_spec::truthy(),
    )
    .unwrap();
    let saved_state_verify =
        command_spec::get(args, "--saved-state-verify", &command_spec::truthy()).unwrap();
    let root_arg = command_spec::get(
        args,
        "root",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    if let Err(msg) = start_server(
        &flowconfig_name,
        no_flowlib,
        ignore_version,
        wait,
        lazy_mode_flag.as_deref(),
        lazy_flag,
        verbose,
        log_file_flag.as_deref(),
        saved_state_fetcher,
        saved_state_force_recheck,
        saved_state_no_fallback,
        saved_state_skip_version_check,
        saved_state_verify,
        &root,
    ) {
        eprintln!("Error: {}", msg);
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::LockStolen);
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
