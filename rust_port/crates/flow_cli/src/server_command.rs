/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Port of serverCommand.ml
// The OCaml version constructs FlowServerMonitorOptions and calls FlowServerMonitor.start.
// In Rust, we directly run the FlowServer (no separate monitor process).

use std::sync::Arc;

use flow_common::options::SavedStateFetcher;
use flow_heap::parsing_heaps::SharedMem;
use flow_utils_concurrency::thread_pool::ThreadCount;
use flow_utils_concurrency::thread_pool::ThreadPool;

use crate::command_spec;
use crate::command_utils;
use crate::flow_server::FlowServer;

fn saved_state_fetcher_flag() -> command_spec::FlagType<Option<SavedStateFetcher>> {
    command_spec::enum_flag(vec![
        ("none", SavedStateFetcher::DummyFetcher),
        ("local", SavedStateFetcher::LocalFetcher),
        ("scm", SavedStateFetcher::ScmFetcher),
        ("fb", SavedStateFetcher::FbFetcher),
    ])
}

// (* let spec = { CommandSpec.name = "server"; doc = "Runs a Flow server in the foreground"; ... } *)
fn spec() -> command_spec::Spec {
    command_spec::Spec::new(
        "server",
        "Runs a Flow server in the foreground",
        "Usage: flow server [OPTION]... [ROOT]\n\nRuns a Flow server in the foreground.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n".to_string(),
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
        "--from",
        &command_spec::optional(command_spec::string()),
        "Specify who is calling this CLI command (used by logging)",
        None,
    )
    // Flags accepted for compatibility but ignored:
    .flag(
        "--wait-for-recheck",
        &command_spec::optional(command_spec::string()),
        "Wait for recheck before responding (accepted but ignored)",
        None,
    )
    .flag(
        "--file-watcher",
        &command_spec::optional(command_spec::string()),
        "File watcher to use (accepted but ignored)",
        None,
    )
    .flag(
        "--log-file",
        &command_spec::optional(command_spec::string()),
        "Path to the server log file (accepted but ignored in foreground mode)",
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
        "--signal-ready",
        &command_spec::truthy(),
        "Write a .ready file when server is initialized (used by start command)",
        None,
    )
    .flag(
        "--verbose",
        &command_spec::truthy(),
        "Enable verbose mode",
        None,
    )
    .anon("root", &command_spec::optional(command_spec::string()))
}

// (* let main base_flags lazy_mode options_flags saved_state_options_flags shm_flags
//        ignore_version server_log_file monitor_log_file no_restart file_watcher ... path_opt () = *)
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
    let lazy_mode_flag = command_spec::get(
        args,
        "--lazy-mode",
        &command_spec::optional(command_spec::string()),
    )
    .unwrap();
    let lazy_flag = command_spec::get(args, "--lazy", &command_spec::truthy()).unwrap();
    let signal_ready = command_spec::get(args, "--signal-ready", &command_spec::truthy()).unwrap();
    let verbose = command_spec::get(args, "--verbose", &command_spec::truthy()).unwrap();
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

    // Determine lazy_mode override from --lazy-mode and --lazy flags
    let lazy_mode = if let Some(ref mode) = lazy_mode_flag {
        match mode.as_str() {
            "true" | "fs" | "ide" | "watchman" => Some(true),
            "false" | "none" => Some(false),
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

    let root = command_utils::guess_root(&flowconfig_name, root_arg.as_deref());
    let mut options = crate::get_options_with_root_and_flowconfig_name(
        no_flowlib,
        ignore_version,
        &root,
        &flowconfig_name,
        overrides,
    );

    if verbose {
        Arc::make_mut(&mut options).verbose = Some(Arc::new(flow_common::verbose::Verbose {
            indent: 0,
            depth: 1,
            enabled_during_flowlib: false,
            focused_files: None,
        }));
    }

    let tmp_dir = std::env::var("FLOW_TEMP_DIR").unwrap_or_else(|_| "/tmp/flow".to_owned());

    let shared_mem = Arc::new(SharedMem::new());
    let pool = ThreadPool::with_thread_count(ThreadCount::NumThreads(
        std::num::NonZeroUsize::new(options.max_workers as usize)
            .expect("max_workers should be positive"),
    ));

    let server = FlowServer::new(options, shared_mem, pool, flowconfig_name, tmp_dir);
    server.run_with_signal_ready(signal_ready);
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
