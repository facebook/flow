/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_config::LazyMode;
use flow_server_files::server_files_js;
use flow_server_monitor as monitor;
use serde_json::json;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

// start command

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "start",
        "Starts a Flow server",
        format!(
            "Usage: {} start [OPTION]... [ROOT]\n\nStarts a Flow server.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\nA server will be started if none is running over ROOT.\n",
            command_utils::exe_name(),
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_options_and_json_flags(spec);
    let spec = command_utils::add_saved_state_flags(spec);
    let spec = command_utils::add_log_file_flags(spec);
    let spec = spec.flag(
        "--wait",
        &arg_spec::truthy(),
        "Wait for the server to finish initializing",
        None,
    );
    let spec = command_utils::add_lazy_flags(spec);
    let spec = command_utils::add_autostop_flag(spec);
    let spec = command_utils::add_shm_flags(spec);
    let spec = command_utils::add_ignore_version_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_no_restart_flag(spec);
    let spec = command_utils::add_file_watcher_flag(spec);
    let spec = command_utils::add_no_cgroup_flag(spec);
    spec.anon("root", &arg_spec::optional(arg_spec::string()))
}

fn main(
    base_flags: command_utils::BaseFlags,
    options_flags: command_utils::OptionsFlags,
    json: bool,
    pretty: bool,
    saved_state_options_flags: command_utils::SavedStateFlags,
    server_log_file: Option<String>,
    monitor_log_file: Option<String>,
    wait: bool,
    lazy_mode: Option<LazyMode>,
    autostop: bool,
    shm_flags: command_utils::SharedMemParams,
    ignore_version: bool,
    no_restart: bool,
    no_cgroup: bool,
    file_watcher: Option<flow_config::FileWatcher>,
    file_watcher_debug: bool,
    file_watcher_timeout: Option<u32>,
    file_watcher_mergebase_with: Option<String>,
    file_watcher_sync_timeout: Option<u32>,
    path_opt: Option<String>,
    (): (),
) {
    let flowconfig_name = base_flags.flowconfig_name;
    let root = command_utils::guess_root(&flowconfig_name, path_opt.as_deref());
    let flowconfig_path = server_files_js::config_file(&flowconfig_name, &root);
    let (flowconfig, flowconfig_hash) =
        command_utils::read_config_and_hash_or_exit(&flowconfig_path, !ignore_version);
    let overrides = command_utils::MakeOptionsOverrides {
        all: if options_flags.all { Some(true) } else { None },
        debug: options_flags.debug,
        distributed: options_flags.distributed,
        estimate_recheck_time: options_flags.estimate_recheck_time,
        flowconfig_flags: Some(options_flags.flowconfig_flags.clone()),
        include_suppressions: if options_flags.include_suppressions {
            Some(true)
        } else {
            None
        },
        include_warnings: options_flags.include_warnings,
        lazy_mode: lazy_mode.clone(),
        long_lived_workers: options_flags.long_lived_workers,
        max_warnings: options_flags.max_warnings,
        max_workers: options_flags.max_workers,
        merge_timeout: options_flags.merge_timeout,
        munge_underscore_members: options_flags.munge_underscore_members,
        no_autoimports: options_flags.no_autoimports,
        profile: Some(options_flags.profile),
        quiet: options_flags.quiet,
        saved_state_fetcher: saved_state_options_flags.saved_state_fetcher.clone(),
        saved_state_force_recheck: Some(saved_state_options_flags.saved_state_force_recheck),
        saved_state_no_fallback: Some(saved_state_options_flags.saved_state_no_fallback),
        saved_state_skip_version_check: Some(
            saved_state_options_flags.saved_state_skip_version_check,
        ),
        saved_state_verify: Some(saved_state_options_flags.saved_state_verify),
        slow_to_check_logging: Some(options_flags.slow_to_check_logging.clone()),
        strip_root: options_flags.strip_root,
        temp_dir: options_flags.temp_dir.clone(),
        verbose: options_flags.verbose.clone(),
        vpn_less: options_flags.vpn_less,
        wait_for_recheck: options_flags.wait_for_recheck,
        ..Default::default()
    };
    let mut options = command_utils::make_options(
        flowconfig.clone(),
        flowconfig_hash,
        flowconfig_name.clone(),
        root.clone(),
        command_utils::get_temp_dir(&options_flags.temp_dir),
        options_flags.no_flowlib,
        overrides,
    );
    options.debug = options_flags.debug;
    options.quiet = options_flags.quiet;
    options.verbose = options_flags.verbose.clone().map(Arc::new);
    let _init_id = crate::random_id_short_string();
    // initialize loggers before doing too much, especially anything that might exit

    if !ignore_version {
        command_utils::assert_version(&flowconfig);
    }

    let shared_mem_config = command_utils::shm_config(&shm_flags, &flowconfig);
    let server_log_file = match server_log_file {
        Some(server_log_file) => server_log_file,
        None => command_utils::server_log_file(&flowconfig_name, options.temp_dir.as_str(), &root),
    };
    let monitor_log_file = match monitor_log_file {
        Some(monitor_log_file) => monitor_log_file,
        None => command_utils::monitor_log_file(&flowconfig_name, options.temp_dir.as_str(), &root),
    };
    let on_spawn = |pid: u32| {
        if pretty || json {
            flow_hh_json::print_json_endline(
                pretty,
                &json!({
                    "pid": pid.to_string(),
                    "log_file": server_log_file.clone(),
                    "monitor_log_file": monitor_log_file.clone(),
                }),
            );
        } else if !options.quiet {
            eprintln!("Spawned flow server (pid={})", pid);
            eprintln!("Logs will go to {}", server_log_file);
            eprintln!("Monitor logs will go to {}", monitor_log_file);
        }
    };
    // A quiet `flow start` doesn't imply a quiet `flow server`
    let mut server_options = options.clone();
    server_options.quiet = false;
    let file_watcher = command_utils::choose_file_watcher(
        &flowconfig,
        lazy_mode,
        file_watcher,
        file_watcher_debug,
        file_watcher_sync_timeout,
    );
    let vcs = flow_common_vcs::vcs::find(None, &root);
    let file_watcher_mergebase_with = command_utils::choose_file_watcher_mergebase_with(
        &flowconfig,
        vcs,
        file_watcher_mergebase_with.as_deref(),
    );
    let file_watcher_timeout =
        command_utils::choose_file_watcher_timeout(&flowconfig, file_watcher_timeout)
            .map(|file_watcher_timeout| file_watcher_timeout as u32);
    match monitor::daemonize(monitor::DaemonizeArgs {
        flowconfig_name,
        no_flowlib: options_flags.no_flowlib,
        ignore_version,
        include_suppressions: options_flags.include_suppressions,
        all: options_flags.all,
        wait,
        lazy_mode: lazy_mode
            .map(command_utils::lazy_mode_arg)
            .map(ToOwned::to_owned),
        long_lived_workers: options_flags.long_lived_workers,
        max_workers: options_flags.max_workers,
        wait_for_recheck: options_flags.wait_for_recheck,
        file_watcher,
        file_watcher_debug,
        file_watcher_timeout,
        file_watcher_mergebase_with: Some(file_watcher_mergebase_with),
        file_watcher_sync_timeout,
        shm_heap_size: Some(shared_mem_config.heap_size),
        shm_hash_table_pow: Some(shared_mem_config.hash_table_pow),
        profile: options_flags.profile,
        debug: server_options.debug,
        quiet: server_options.quiet,
        verbose: server_options
            .verbose
            .as_ref()
            .map(|verbose| verbose.as_ref().clone()),
        server_log_file: server_log_file.clone(),
        monitor_log_file: monitor_log_file.clone(),
        from: flow_event_logger::get_from_i_am_a_clown(),
        saved_state_fetcher: saved_state_options_flags.saved_state_fetcher,
        saved_state_force_recheck: saved_state_options_flags.saved_state_force_recheck,
        saved_state_no_fallback: saved_state_options_flags.saved_state_no_fallback,
        saved_state_skip_version_check: saved_state_options_flags.saved_state_skip_version_check,
        saved_state_verify: saved_state_options_flags.saved_state_verify,
        no_restart,
        autostop,
        no_cgroup,
        root,
        temp_dir: server_options.temp_dir.to_string(),
    }) {
        Ok(pid) => on_spawn(pid),
        Err(err) => {
            eprintln!("Error: {}", err);
            if no_restart && err.contains("already a server running") {
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::LockStolen);
            }
            if err.contains("already a server running") {
                flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::LockStolen);
            }
            flow_common_exit_status::exit(
                flow_common_exit_status::FlowExitStatus::ServerStartFailed,
            );
        }
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), |args| {
        let json_flags = command_utils::get_json_flags(args);
        let options_flags = command_utils::get_options_flags(args);
        let options_flags = command_utils::OptionsFlags {
            quiet: options_flags.quiet || json_flags.json || json_flags.pretty,
            ..options_flags
        };
        let (server_log_file, monitor_log_file) = command_utils::get_log_file_flags(args);
        let file_watcher_flags = command_utils::get_file_watcher_flags(args);
        main(
            command_utils::get_base_flags(args),
            options_flags,
            json_flags.json,
            json_flags.pretty,
            command_utils::get_saved_state_flags(args),
            server_log_file,
            monitor_log_file,
            command_spec::get(args, "--wait", &arg_spec::truthy()).unwrap(),
            command_utils::get_lazy_flags(args),
            command_spec::get(args, "--autostop", &arg_spec::truthy()).unwrap(),
            command_utils::get_shm_flags(args),
            command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap(),
            command_spec::get(args, "--no-auto-restart", &arg_spec::truthy()).unwrap(),
            command_spec::get(args, "--no-cgroup", &arg_spec::truthy()).unwrap(),
            file_watcher_flags.file_watcher,
            file_watcher_flags.file_watcher_debug,
            file_watcher_flags.file_watcher_timeout,
            file_watcher_flags.file_watcher_mergebase_with,
            file_watcher_flags.file_watcher_sync_timeout,
            command_spec::get(args, "root", &arg_spec::optional(arg_spec::string())).unwrap(),
            (),
        )
    })
}
