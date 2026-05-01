/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_server_files::server_files_js;
use flow_server_monitor as monitor;

use crate::command_spec;
use crate::command_spec::arg_spec;
use crate::command_utils;

fn spec() -> command_spec::Spec {
    let spec = command_spec::Spec::new(
        "server",
        "Runs a Flow server in the foreground",
        command_spec::Visibility::Public,
        format!(
            "Usage: {} server [OPTION]... [ROOT]\n\nRuns a Flow server in the foreground.\n\nFlow will search upward for a .flowconfig file, beginning at ROOT.\nROOT is assumed to be the current directory if unspecified.\n",
            command_utils::exe_name(),
        ),
    );
    let spec = command_utils::add_base_flags(spec);
    let spec = command_utils::add_lazy_flags(spec);
    let spec = command_utils::add_options_flags(spec);
    let spec = command_utils::add_saved_state_flags(spec);
    let spec = command_utils::add_shm_flags(spec);
    let spec = command_utils::add_ignore_version_flag(spec);
    let spec = command_utils::add_from_flag(spec);
    let spec = command_utils::add_log_file_flags(spec);
    let spec = command_utils::add_no_restart_flag(spec);
    let spec = command_utils::add_file_watcher_flag(spec);
    let spec = command_utils::add_no_cgroup_flag(spec);
    spec.anon("root", &arg_spec::optional(arg_spec::string()))
}

fn main(
    base_flags: command_utils::BaseFlags,
    lazy_mode: Option<flow_config::LazyMode>,
    options_flags: command_utils::OptionsFlags,
    saved_state_options_flags: command_utils::SavedStateFlags,
    shm_flags: command_utils::SharedMemParams,
    ignore_version: bool,
    server_log_file: Option<String>,
    monitor_log_file: Option<String>,
    no_restart: bool,
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
    let options = Arc::new(command_utils::make_options(
        flowconfig_name.clone(),
        flowconfig_hash,
        flowconfig.clone(),
        lazy_mode,
        root.clone(),
        options_flags.clone(),
        saved_state_options_flags.clone(),
    ));

    // initialize loggers before doing too much, especially anything that might exit
    flow_logging_utils::init_loggers(&options, None);

    if !ignore_version {
        command_utils::assert_version(&flowconfig);
    }

    let shared_mem_config = command_utils::shm_config(&shm_flags, &flowconfig);
    let server_log_file = match server_log_file.or_else(|| std::env::var("FLOW_LOG_FILE").ok()) {
        Some(s) => s,
        None => command_utils::server_log_file(
            &flowconfig_name,
            options.temp_dir.as_str(),
            options.root.as_path(),
        ),
    };
    let monitor_log_file =
        match monitor_log_file.or_else(|| std::env::var("FLOW_MONITOR_LOG_FILE").ok()) {
            Some(s) => s,
            None => command_utils::monitor_log_file(
                &flowconfig_name,
                options.temp_dir.as_str(),
                options.root.as_path(),
            ),
        };
    let file_watcher = command_utils::choose_file_watcher(
        &flowconfig,
        lazy_mode,
        file_watcher,
        file_watcher_debug,
        file_watcher_sync_timeout,
    );
    let vcs = flow_common_vcs::vcs::find(None, &root);
    let file_watcher_mergebase_with = Some(command_utils::choose_file_watcher_mergebase_with(
        &flowconfig,
        vcs,
        file_watcher_mergebase_with.as_deref(),
    ));
    let file_watcher_timeout =
        command_utils::choose_file_watcher_timeout(&flowconfig, file_watcher_timeout)
            .map(|timeout| timeout as u32);
    if let Err(err) = monitor::start(
        options,
        monitor::StartArgs {
            flowconfig_name,
            server_log_file,
            monitor_log_file,
            lazy_mode: lazy_mode
                .map(command_utils::lazy_mode_arg)
                .map(ToOwned::to_owned),
            no_flowlib: options_flags.no_flowlib,
            ignore_version,
            wait_for_recheck: options_flags.wait_for_recheck,
            file_watcher,
            file_watcher_debug,
            file_watcher_timeout,
            file_watcher_mergebase_with,
            file_watcher_sync_timeout,
            shm_heap_size: Some(shared_mem_config.heap_size),
            shm_hash_table_pow: Some(shared_mem_config.hash_table_pow),
            from: flow_event_logger::get_from_i_am_a_clown(),
            autostop: false,
            no_restart,
            cli_overrides: flow_common::cli_overrides::CliOverrides {
                max_warnings: options_flags.max_warnings,
                no_autoimports: options_flags.no_autoimports,
                flowconfig_ignores: options_flags.flowconfig_flags.ignores.clone(),
                flowconfig_includes: options_flags.flowconfig_flags.includes.clone(),
                flowconfig_libs: options_flags.flowconfig_flags.libs.clone(),
                flowconfig_raw_lint_severities: options_flags
                    .flowconfig_flags
                    .raw_lint_severities
                    .clone(),
                flowconfig_untyped: options_flags.flowconfig_flags.untyped.clone(),
                flowconfig_declarations: options_flags.flowconfig_flags.declarations.clone(),
            },
        },
    ) {
        eprintln!("Error: {}", err);
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::ServerStartFailed);
    }
}

pub(crate) fn command() -> command_spec::Command {
    command_spec::command(spec(), |args| {
        command_utils::maybe_run_in_cgroup(args);
        let (server_log_file, monitor_log_file) = command_utils::get_log_file_flags(args);
        let file_watcher_flags = command_utils::get_file_watcher_flags(args);
        main(
            command_utils::get_base_flags(args),
            command_utils::get_lazy_flags(args),
            command_utils::get_options_flags(args),
            command_utils::get_saved_state_flags(args),
            command_utils::get_shm_flags(args),
            command_spec::get(args, "--ignore-version", &arg_spec::truthy()).unwrap(),
            server_log_file,
            monitor_log_file,
            command_spec::get(args, "--no-auto-restart", &arg_spec::truthy()).unwrap(),
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
