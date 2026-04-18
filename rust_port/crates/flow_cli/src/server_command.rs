/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_server_monitor as monitor;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let spec = command_utils::add_shm_flags(command_utils::add_profile_flag(
        command_spec::Spec::new(
            "server",
            "Runs a Flow server in the foreground",
            "Usage: flow server [OPTION]... [ROOT]\n\nRuns a Flow server in the foreground.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n".to_string(),
        ),
    ));
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_ignore_version_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_wait_for_recheck_flag(spec);
    let spec = command_utils::add_no_cgroup_flag(spec);
    let spec = command_utils::add_log_file_flags(spec);
    let spec = command_utils::add_no_restart_flag(spec);
    let spec = command_utils::add_autostop_flag(spec);
    let spec = command_utils::add_file_watcher_flag(spec);
    let spec = command_utils::add_lazy_flags(spec);
    let spec = command_utils::add_saved_state_flags(spec);
    spec.flag(
        "--no-flowlib",
        &arg_spec::truthy(),
        "Do not use the bundled flowlib",
        Some("NO_FLOWLIB"),
    )
    .flag(
        "--include-suppressed",
        &arg_spec::truthy(),
        "Ignore any `suppress_comment` lines in .flowconfig",
        None,
    )
    .flag(
        "--all",
        &arg_spec::truthy(),
        "Check all files, not just @flow files",
        None,
    )
    .flag(
        "--long-lived-workers",
        &arg_spec::optional(arg_spec::bool_flag()),
        "Enable or disable long-lived workers",
        Some("FLOW_LONG_LIVED_WORKERS"),
    )
    .flag(
        "--max-workers",
        &arg_spec::optional(arg_spec::int()),
        "Maximum number of workers to create (capped by number of cores)",
        Some("FLOW_MAX_WORKERS"),
    )
    .flag(
        "--temp-dir",
        &arg_spec::optional(arg_spec::string()),
        "Temp directory",
        Some("FLOW_TEMP_DIR"),
    )
    .flag(
        "--no-cgroup",
        &arg_spec::truthy(),
        "Don't automatically run in a cgroup (if cgroups are available)",
        None,
    )
    .flag(
        "--verbose",
        &arg_spec::truthy(),
        "Enable verbose mode",
        None,
    )
    .flag("--quiet", &arg_spec::truthy(), "Quiet mode", None)
    .anon("root", &arg_spec::optional(arg_spec::string()))
}

fn validate_positive_flag(flag_name: &str, value: Option<i32>) -> Option<i32> {
    value.inspect(|&value| {
        if value <= 0 {
            eprintln!("Invalid value for {}: {}", flag_name, value);
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::CommandlineUsageError,
            );
        }
    })
}

fn main(args: &arg_spec::Values) {
    command_utils::maybe_run_in_cgroup(args);

    let base_flags = command_utils::get_base_flags(args);
    let flowconfig_name = base_flags.flowconfig_name;
    let no_flowlib = command_spec::get(args, "--no-flowlib", &arg_spec::truthy()).unwrap();
    let ignore_version = command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap();
    let include_suppressions =
        command_spec::get(args, "--include-suppressed", &arg_spec::truthy()).unwrap();
    let all = command_spec::get(args, "--all", &arg_spec::truthy()).unwrap();
    let profile = command_utils::get_profile_flag(args);
    let lazy_mode = command_utils::get_lazy_flags(args);
    let signal_ready = matches!(
        std::env::var("FLOW_SERVER_SIGNAL_READY").as_deref(),
        Ok("1") | Ok("true")
    );
    let verbose = command_spec::get(args, "--verbose", &arg_spec::truthy()).unwrap();
    let quiet = command_spec::get(args, "--quiet", &arg_spec::truthy()).unwrap();
    let (log_file_flag, monitor_log_file_flag) = command_utils::get_log_file_flags(args);
    let temp_dir_flag =
        command_spec::get(args, "--temp-dir", &arg_spec::optional(arg_spec::string())).unwrap();
    let long_lived_workers = command_spec::get(
        args,
        "--long-lived-workers",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let max_workers = validate_positive_flag(
        "--max-workers",
        command_spec::get(args, "--max-workers", &arg_spec::optional(arg_spec::int())).unwrap(),
    );
    let wait_for_recheck = command_spec::get(
        args,
        "--wait-for-recheck",
        &arg_spec::optional(arg_spec::bool_flag()),
    )
    .unwrap();
    let file_watcher_flags = command_utils::get_file_watcher_flags(args);
    let shm_flags = command_utils::get_shm_flags(args);
    let saved_state_flags = command_utils::get_saved_state_flags(args);
    let root_arg =
        command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap();
    let overrides = command_utils::MakeOptionsOverrides {
        include_suppressions: if include_suppressions {
            Some(true)
        } else {
            None
        },
        all: if all { Some(true) } else { None },
        lazy_mode,
        long_lived_workers,
        max_workers,
        profile: Some(profile),
        saved_state_fetcher: saved_state_flags.saved_state_fetcher,
        saved_state_force_recheck: Some(saved_state_flags.saved_state_force_recheck),
        saved_state_no_fallback: Some(saved_state_flags.saved_state_no_fallback),
        saved_state_skip_version_check: Some(saved_state_flags.saved_state_skip_version_check),
        saved_state_verify: Some(saved_state_flags.saved_state_verify),
        temp_dir: temp_dir_flag,
        wait_for_recheck,
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
    if quiet {
        Arc::make_mut(&mut options).quiet = true;
    }

    let flowconfig_path = root.join(&flowconfig_name);
    let flowconfig_path = flowconfig_path.to_string_lossy().to_string();
    let (flowconfig, _) =
        command_utils::read_config_and_hash_or_exit(&flowconfig_path, !ignore_version);
    let shared_mem_config = command_utils::shm_config(&shm_flags, &flowconfig);
    let chosen_file_watcher = command_utils::choose_file_watcher(
        &flowconfig,
        lazy_mode,
        file_watcher_flags.file_watcher,
        file_watcher_flags.file_watcher_debug,
        file_watcher_flags.file_watcher_sync_timeout,
    );
    let file_watcher = Some(command_utils::file_watcher_arg(chosen_file_watcher).to_owned());
    let vcs = flow_common_vcs::vcs::find(None, &root);
    let file_watcher_mergebase_with = Some(command_utils::choose_file_watcher_mergebase_with(
        &flowconfig,
        vcs,
        file_watcher_flags.file_watcher_mergebase_with.as_deref(),
    ));
    let file_watcher_timeout = command_utils::choose_file_watcher_timeout(
        &flowconfig,
        file_watcher_flags.file_watcher_timeout,
    )
    .map(|timeout| timeout as u32);
    let tmp_dir = options.temp_dir.clone();
    let server_log_file = log_file_flag
        .or_else(|| std::env::var("FLOW_LOG_FILE").ok())
        .unwrap_or_else(|| {
            command_utils::server_log_file(
                &flowconfig_name,
                tmp_dir.as_str(),
                options.root.as_path(),
            )
        });
    let monitor_log_file = monitor_log_file_flag
        .or_else(|| std::env::var("FLOW_MONITOR_LOG_FILE").ok())
        .unwrap_or_else(|| {
            command_utils::monitor_log_file(
                &flowconfig_name,
                tmp_dir.as_str(),
                options.root.as_path(),
            )
        });

    let no_restart = command_spec::get(args, "--no-auto-restart", &arg_spec::truthy()).unwrap();
    let autostop = command_spec::get(args, "--autostop", &arg_spec::truthy()).unwrap();
    if let Err(err) = monitor::start(
        options,
        monitor::StartArgs {
            flowconfig_name,
            signal_ready,
            server_log_file,
            monitor_log_file,
            wait_for_recheck,
            file_watcher,
            file_watcher_debug: file_watcher_flags.file_watcher_debug,
            file_watcher_timeout,
            file_watcher_mergebase_with,
            file_watcher_sync_timeout: file_watcher_flags.file_watcher_sync_timeout,
            shm_heap_size: Some(shared_mem_config.heap_size),
            shm_hash_table_pow: Some(shared_mem_config.hash_table_pow),
            from: flow_event_logger::get_from_i_am_a_clown(),
            autostop,
            no_restart,
        },
    ) {
        eprintln!("Error: {}", err);
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::ServerStartFailed);
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), main)
}
