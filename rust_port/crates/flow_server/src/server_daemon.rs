/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::OnceLock;

use flow_common::options::Options;
use flow_common::options::SavedStateFetcher;
use flow_common::verbose::Verbose;
use flow_common_exit_status::FlowExitStatus;
use flow_daemon::ChannelPair;
use flow_daemon::Entry;
use flow_daemon::Handle;
use flow_daemon::StdioFd;
use flow_event_logger::LoggingContext;
use flow_server_files::server_files_js;

/// The fixed daemon entry name for the server master child process.
pub const SERVER_ENTRY_NAME: &str = "main_1";

pub type ServerOptionsBuilder = fn(&ServerDaemonArgs) -> Arc<Options>;

static SERVER_ENTRY: OnceLock<Entry<ServerEntryParam, (), ()>> = OnceLock::new();
static SERVER_OPTIONS_BUILDER: OnceLock<ServerOptionsBuilder> = OnceLock::new();

/// Serializable subset of `Options` needed to reconstruct the server child.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ServerDaemonArgs {
    pub flowconfig_name: String,
    pub no_flowlib: bool,
    pub ignore_version: bool,
    pub include_suppressions: bool,
    pub all: bool,
    pub debug: bool,
    pub quiet: bool,
    pub lazy_mode: Option<String>,
    pub long_lived_workers: Option<bool>,
    pub max_workers: Option<i32>,
    pub wait_for_recheck: Option<bool>,
    pub profile: bool,
    pub verbose: Option<Verbose>,
    pub saved_state_fetcher: Option<SavedStateFetcher>,
    pub saved_state_force_recheck: bool,
    pub saved_state_no_fallback: bool,
    pub saved_state_skip_version_check: bool,
    pub saved_state_verify: bool,
    pub root: PathBuf,
    pub temp_dir: String,
}

impl ServerDaemonArgs {
    pub fn of_options(
        options: &Options,
        lazy_mode: Option<String>,
        no_flowlib: bool,
        ignore_version: bool,
    ) -> Self {
        Self {
            flowconfig_name: options.flowconfig_name.to_string(),
            no_flowlib,
            ignore_version,
            include_suppressions: options.include_suppressions,
            all: options.all,
            debug: options.debug,
            quiet: options.quiet,
            lazy_mode,
            long_lived_workers: Some(options.long_lived_workers),
            max_workers: Some(options.max_workers),
            wait_for_recheck: Some(options.wait_for_recheck),
            profile: options.profile,
            verbose: options
                .verbose
                .as_ref()
                .map(|verbose| verbose.as_ref().clone()),
            saved_state_fetcher: Some(options.saved_state_fetcher),
            saved_state_force_recheck: options.saved_state_force_recheck,
            saved_state_no_fallback: options.saved_state_no_fallback,
            saved_state_skip_version_check: options.saved_state_skip_version_check,
            saved_state_verify: options.saved_state_verify,
            root: (*options.root).clone(),
            temp_dir: options.temp_dir.to_string(),
        }
    }
}

/// Serializable arguments shipped to the server master daemon child.
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct ServerEntryParam {
    pub daemon_args: ServerDaemonArgs,
    pub init_id: String,
    pub argv: Vec<String>,
    pub log_file: String,
    pub file_watcher_pid: Option<u32>,
    pub parent_pid: u32,
    pub logging_context: LoggingContext,
}

pub fn entry_point(build: ServerOptionsBuilder) -> &'static Entry<ServerEntryParam, (), ()> {
    let _ = SERVER_OPTIONS_BUILDER.set(build);
    SERVER_ENTRY
        .get_or_init(|| flow_daemon::register_entry_point(SERVER_ENTRY_NAME, server_entry_handler))
}

pub fn register(build: ServerOptionsBuilder) {
    entry_point(build);
}

fn registered_entry_point() -> &'static Entry<ServerEntryParam, (), ()> {
    SERVER_ENTRY.get().unwrap_or_else(|| {
        panic!(
            "server entry point not registered; call flow_server::server_daemon::register before \
             daemon startup"
        )
    })
}

fn server_entry_handler(param: ServerEntryParam, pair: ChannelPair<(), ()>) {
    let ServerEntryParam {
        daemon_args,
        init_id,
        argv,
        log_file: _log_file,
        file_watcher_pid,
        parent_pid,
        logging_context,
    } = param;
    let ChannelPair(in_chan, out_chan) = pair;

    flow_event_logger::restore_context(logging_context);
    flow_event_logger::set_command(Some("server".to_string()));
    flow_event_logger::init_flow_command(&init_id);

    let build = *SERVER_OPTIONS_BUILDER.get().unwrap_or_else(|| {
        panic!(
            "server options builder not registered; call flow_server::server_daemon::register \
             before daemon startup"
        )
    });
    let options = build(&daemon_args);

    set_hh_logger_min_level(&options);
    log::info!("argv={}", argv.join(" "));
    dump_server_options(&options);

    let pids_file = flow_server_files::server_files_js::pids_file(
        &options.flowconfig_name,
        options.temp_dir.as_str(),
        options.root.as_path(),
    );
    if let Err(e) = flow_daemon::pid_log::init(std::path::Path::new(&pids_file)) {
        log::warn!("server: pid_log init failed: {}", e);
    }
    flow_daemon::pid_log::log(Some("monitor"), false, parent_pid);
    if let Some(pid) = file_watcher_pid {
        flow_daemon::pid_log::log(Some("file_watcher"), false, pid);
    }
    flow_daemon::pid_log::log(Some("main"), false, std::process::id());

    let in_stream = flow_daemon::into_in_stream(in_chan);
    let out_stream = flow_daemon::into_out_stream(out_chan);
    let channels: flow_server_env::monitor_rpc::Channels = (in_stream, out_stream);

    crate::server::run_from_daemonize(&init_id, options, Some(channels));
}

fn hh_logger_level_of_env(env: &str) -> Option<log::LevelFilter> {
    match std::env::var(env).ok().as_deref() {
        Some("off") => Some(log::LevelFilter::Off),
        Some("fatal") => Some(log::LevelFilter::Error),
        Some("error") => Some(log::LevelFilter::Error),
        Some("warn") => Some(log::LevelFilter::Warn),
        Some("info") => Some(log::LevelFilter::Info),
        Some("debug") => Some(log::LevelFilter::Debug),
        Some(_) | None => None,
    }
}

pub fn set_hh_logger_min_level(options: &Options) {
    let level = if options.quiet {
        log::LevelFilter::Off
    } else if options.verbose.is_some() || options.debug {
        log::LevelFilter::Debug
    } else {
        match hh_logger_level_of_env("FLOW_LOG_LEVEL") {
            Some(level) => level,
            None => log::LevelFilter::Info,
        }
    };
    log::set_max_level(level);
}

pub fn dump_server_options(options: &Options) {
    let lazy_mode = if options.lazy_mode { "on" } else { "off" };
    log::info!("lazy_mode={}", lazy_mode);
    log::info!("max_workers={}", options.max_workers);
    log::info!("long_lived_workers={}", options.long_lived_workers);
    log::info!("debug={}", options.debug);
    for (method_name, log_saving) in options.log_saving.iter() {
        let limit_str = match log_saving.limit {
            None => "null".to_string(),
            Some(limit) => format!("{}", limit),
        };
        log::info!(
            "{} threshold_time_ms={} limit={} rate={}",
            method_name,
            log_saving.threshold_time_ms,
            limit_str,
            log_saving.rate
        );
    }
    for (r, g) in options.enabled_rollouts.iter() {
        log::info!("Rollout {:?} set to {:?}", r, g);
    }
}

pub fn try_open_log_file(file: &str) -> Result<fs::File, String> {
    if Path::new(file).exists() {
        let old_file = format!("{}.old", file);
        if let Err(e) = (|| -> std::io::Result<()> {
            if Path::new(&old_file).exists() {
                fs::remove_file(&old_file)?;
            }
            fs::rename(file, &old_file)?;
            Ok(())
        })() {
            eprintln!(
                "Log rotate: failed to move '{}' to '{}'\n{}",
                file, old_file, e
            );
        }
    }
    fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(file)
        .map_err(|e| format!("Failed to open log file '{}': {}", file, e))
}

pub fn open_log_file(file: &str) -> fs::File {
    try_open_log_file(file).unwrap_or_else(|e| panic!("{}", e))
}

pub fn daemonize(
    init_id: &str,
    log_file: &str,
    argv: &[String],
    lazy_mode: Option<String>,
    no_flowlib: bool,
    ignore_version: bool,
    options: Arc<Options>,
    file_watcher_pid: Option<u32>,
) -> Result<Handle<(), ()>, String> {
    let entry = registered_entry_point();

    let root = &options.root;
    let tmp_dir = &options.temp_dir;
    let flowconfig_name = &options.flowconfig_name;
    let lock = server_files_js::lock_file(flowconfig_name, tmp_dir, root);
    if !flow_common::lock::check(&lock) {
        let msg = format!(
            "Error: There is already a server running for {}",
            root.display()
        );
        eprintln!("{}", msg);
        flow_common_exit_status::exit(FlowExitStatus::LockStolen);
    }

    let name = format!("server master process watching {}", root.display());
    let param = ServerEntryParam {
        daemon_args: ServerDaemonArgs::of_options(&options, lazy_mode, no_flowlib, ignore_version),
        init_id: init_id.to_string(),
        argv: argv.to_vec(),
        log_file: log_file.to_string(),
        file_watcher_pid,
        parent_pid: std::process::id(),
        logging_context: flow_event_logger::get_context(),
    };

    let stdio = (
        StdioFd::Owned(flow_daemon::null_fd()),
        StdioFd::Owned(try_open_log_file(log_file)?),
        StdioFd::Owned(try_open_log_file(log_file)?),
    );

    flow_daemon::spawn(None, Some(&name), stdio, entry, param)
        .map_err(|e| format!("Failed to spawn server daemon '{}': {}", name, e))
}
