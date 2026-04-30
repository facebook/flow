/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;
use std::sync::OnceLock;

use flow_common::options::Options;
use flow_common_exit_status::FlowExitStatus;
use flow_common_exit_status::error_code;
use flow_common_exit_status::exit;
use flow_daemon::ChannelPair;
use flow_daemon::Entry;
use flow_daemon::Handle;
use flow_daemon::StdioFd;
use flow_daemon::null_fd;
use flow_daemon::shutdown_out_write;
use flow_daemon::spawn;
use flow_daemon::try_from_channel;
use flow_event_logger::LoggingContext;

use crate::flow_server_monitor::DaemonizeArgs;

// When `flow start --wait` daemonizes the Flow server monitor, it listens
// over a pipe and waits for the Flow server to finish initializing. These
// are the messages we send over that pipe.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum WaitMsg {
    /// Monitor is up. All `flow start` commands wait for this
    Starting,
    /// Server is done initializing. `flow start --wait` commands wait for this
    Ready,
}

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct MonitorEntryParam {
    pub daemonize_args: DaemonizeArgs,
    pub init_id: String,
    pub logging_context: LoggingContext,
}

pub type MonitorEntryBuilder =
    fn(&DaemonizeArgs) -> (Arc<Options>, crate::flow_server_monitor::StartArgs);

static MONITOR_ENTRY: OnceLock<Entry<MonitorEntryParam, (), WaitMsg>> = OnceLock::new();
static MONITOR_ENTRY_BUILDER: OnceLock<MonitorEntryBuilder> = OnceLock::new();

pub const MONITOR_ENTRY_NAME: &str = "monitor";

pub fn entry_point(build: MonitorEntryBuilder) -> &'static Entry<MonitorEntryParam, (), WaitMsg> {
    let _ = MONITOR_ENTRY_BUILDER.set(build);
    MONITOR_ENTRY.get_or_init(|| {
        flow_daemon::register_entry_point(MONITOR_ENTRY_NAME, monitor_entry_handler)
    })
}

pub fn register(build: MonitorEntryBuilder) {
    entry_point(build);
}

pub(crate) fn registered_entry_point() -> &'static Entry<MonitorEntryParam, (), WaitMsg> {
    MONITOR_ENTRY.get().unwrap_or_else(|| {
        panic!(
            "monitor entry point not registered; call \
             flow_server_monitor::flow_server_monitor_daemon::register before daemon startup"
        )
    })
}

fn monitor_entry_handler(param: MonitorEntryParam, pair: ChannelPair<(), WaitMsg>) {
    let MonitorEntryParam {
        daemonize_args,
        init_id,
        logging_context,
    } = param;
    let ChannelPair(in_chan, out_chan) = pair;

    #[cfg(unix)]
    {
        match nix::unistd::setsid() {
            Ok(_pid) => {}
            Err(nix::errno::Errno::EPERM) => {}
            Err(e) => {
                flow_hh_logger::debug!("setsid failed: {}", e);
            }
        }
    }

    drop(in_chan);

    flow_event_logger::restore_context(logging_context);
    flow_event_logger::set_command(Some("monitor".to_string()));
    flow_event_logger::init_flow_command(&init_id);

    let build = *MONITOR_ENTRY_BUILDER.get().unwrap_or_else(|| {
        panic!(
            "monitor entry builder not registered; call \
             flow_server_monitor::flow_server_monitor_daemon::register before daemon startup"
        )
    });
    let (options, start_args) = build(&daemonize_args);
    let waiting_fd = flow_daemon::into_out_writer(out_chan);

    if let Err(e) = crate::flow_server_monitor::start_in_daemon(waiting_fd, options, start_args) {
        eprintln!("monitor entry failed: {}", e);
        flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::ServerStartFailed);
    }
}

// The monitor can communicate with the process that spawned it over a pipe.
// The current scheme has it write a message when it starts up and has the
// lock and then write another message when it has finished initializing.
// It's up to the forking process whether it cares to wait for the
// initialization to complete
pub(crate) fn wait_loop(should_wait: bool, handle: &mut Handle<WaitMsg, ()>) {
    let msg: WaitMsg = match try_from_channel(&mut handle.channels.0, None) {
        Ok(msg) => msg,
        Err(_end_of_file) => {
            // The pipe broke before we got the all-clear from the monitor.
            // What kind of things could go wrong? Well we check the lock
            // before forking the monitor, but maybe by the time the monitor
            // started someone else had grabbed the lock, so it exited. I'm
            // sure there's a million other things that could have gone wrong.
            let child_pid = handle.child.id();
            let (pid, status): (u32, Option<std::process::ExitStatus>) =
                match handle.child.try_wait() {
                    Ok(None) => {
                        // Sometimes the End_of_file races the child process
                        // actually exiting. In case that's happening here,
                        // let's give the child 1 second more to die.
                        std::thread::sleep(std::time::Duration::from_secs(1));
                        match handle.child.try_wait() {
                            Ok(None) => (0, None),
                            Ok(Some(s)) => (child_pid, Some(s)),
                            Err(e) => {
                                eprintln!("Error: Failed to wait for server: {}", e);
                                std::process::exit(1);
                            }
                        }
                    }
                    Ok(Some(s)) => (child_pid, Some(s)),
                    Err(e) => {
                        eprintln!("Error: Failed to wait for server: {}", e);
                        std::process::exit(1);
                    }
                };
            let exit_code = FlowExitStatus::ServerStartFailed;
            let (msg, exit_code) = if pid == 0
            /* The monitor is still alive...not sure what happened */
            {
                (
                    "Error: Failed to start server for some unknown reason.".to_string(),
                    exit_code,
                )
            } else {
                // The monitor is dead. Shucks.
                let (reason, exit_code) = if let Some(status) = status {
                    if let Some(code) = status.code() {
                        if code == error_code(FlowExitStatus::LockStolen) {
                            // Sometimes when we actually go to start the
                            // monitor we find a monitor already running
                            // (race condition). If so, we can just forward
                            // that error code.
                            (
                                "There is already a server running.".to_string(),
                                FlowExitStatus::LockStolen,
                            )
                        } else if code == error_code(FlowExitStatus::OutOfSharedMemory) {
                            (
                                "The server is failed to allocate shared memory.".to_string(),
                                FlowExitStatus::OutOfSharedMemory,
                            )
                        } else {
                            (format!("exited prematurely with code {}.", code), exit_code)
                        }
                    } else {
                        #[cfg(unix)]
                        {
                            use std::os::unix::process::ExitStatusExt;
                            if let Some(signal) = status.signal() {
                                (
                                    format!(
                                        "The server was killed prematurely with signal {}.",
                                        signal
                                    ),
                                    exit_code,
                                )
                            } else if let Some(signal) = status.stopped_signal() {
                                (
                                    format!(
                                        "The server was stopped prematurely with signal {}.",
                                        signal
                                    ),
                                    exit_code,
                                )
                            } else {
                                ("exited prematurely.".to_string(), exit_code)
                            }
                        }
                        #[cfg(not(unix))]
                        {
                            ("exited prematurely.".to_string(), exit_code)
                        }
                    }
                } else {
                    ("exited prematurely.".to_string(), exit_code)
                };
                (
                    format!("Error: Failed to start server. {}", reason),
                    exit_code,
                )
            };
            eprintln!("{}", msg);
            exit(exit_code);
        }
    };
    if should_wait && msg != WaitMsg::Ready {
        wait_loop(should_wait, handle);
    }
}

pub fn daemonize(
    wait: bool,
    on_spawn: impl FnOnce(u32),
    init_id: String,
    monitor_options: crate::flow_server_monitor_options::MonitorOptions,
) {
    /* Daemon.spawn is creating a new process with /dev/null as both the stdout
     * and stderr. We are NOT leaking stdout and stderr. But the Windows
     * implementation of OCaml does leak stdout and stderr. This means any process
     * that waits for `flow start`'s stdout and stderr to close might wait
     * forever.
     *
     * On Windows 10 (and 8 I think), you can just call `set_close_on_exec` on
     * stdout and stderr and that seems to solve things. However, that call
     * fails on Windows 7. After poking around for a few hours, I can't think
     * of a solution other than manually implementing Unix.create_process
     * correctly.
     *
     * So for now let's make Windows 7 not crash. It seems like `flow start` on
     * Windows 7 doesn't actually leak stdio, so a no op is acceptable.
     */
    let root_str = monitor_options
        .server_options
        .root
        .to_string_lossy()
        .into_owned();
    let name = format!("monitor for {}", root_str);

    let server_options = &monitor_options.server_options;
    let args = DaemonizeArgs {
        flowconfig_name: server_options.flowconfig_name.to_string(),
        no_flowlib: monitor_options.no_flowlib,
        ignore_version: monitor_options.ignore_version,
        include_suppressions: server_options.include_suppressions,
        all: server_options.all,
        wait,
        no_restart: monitor_options.no_restart,
        autostop: monitor_options.autostop,
        lazy_mode: monitor_options.lazy_mode.clone().or_else(|| {
            if server_options.lazy_mode {
                Some("true".to_string())
            } else {
                None
            }
        }),
        long_lived_workers: Some(server_options.long_lived_workers),
        max_workers: Some(server_options.max_workers),
        wait_for_recheck: Some(server_options.wait_for_recheck),
        file_watcher: monitor_options.file_watcher.clone(),
        file_watcher_debug: false,
        file_watcher_timeout: monitor_options.file_watcher_timeout.map(|t| t as u32),
        file_watcher_mergebase_with: Some(monitor_options.file_watcher_mergebase_with.clone()),
        file_watcher_sync_timeout: None,
        shm_heap_size: Some(monitor_options.shared_mem_config.heap_size),
        shm_hash_table_pow: Some(monitor_options.shared_mem_config.hash_table_pow),
        profile: server_options.profile,
        debug: server_options.debug,
        quiet: server_options.quiet,
        verbose: server_options
            .verbose
            .as_ref()
            .map(|verbose| verbose.as_ref().clone()),
        server_log_file: monitor_options.server_log_file.clone(),
        monitor_log_file: monitor_options.log_file.clone(),
        from: flow_event_logger::get_from_i_am_a_clown(),
        saved_state_fetcher: Some(server_options.saved_state_fetcher),
        saved_state_force_recheck: server_options.saved_state_force_recheck,
        saved_state_no_fallback: server_options.saved_state_no_fallback,
        saved_state_skip_version_check: server_options.saved_state_skip_version_check,
        saved_state_verify: server_options.saved_state_verify,
        no_cgroup: false,
        root: (*server_options.root).clone(),
        temp_dir: server_options.temp_dir.to_string(),
    };

    let param = MonitorEntryParam {
        daemonize_args: args,
        init_id,
        logging_context: flow_event_logger::get_context(),
    };

    let entry = registered_entry_point();

    let stdio = (
        StdioFd::Owned(null_fd()),
        StdioFd::Owned(null_fd()),
        StdioFd::Owned(null_fd()),
    );

    let mut handle = match spawn(None, Some(&name), stdio, entry, param) {
        Ok(h) => h,
        Err(e) => {
            eprintln!("failed to spawn monitor: {}", e);
            exit(FlowExitStatus::ServerStartFailed);
        }
    };
    let pid = handle.child.id();

    on_spawn(pid);

    if let Err(e) = shutdown_out_write(&mut handle.channels.1) {
        flow_hh_logger::debug!("failed to shutdown monitor parent_out: {}", e);
    }

    // If wait is true, wait for the "Ready" message.
    // Otherwise, only wait for the "Starting" message.
    wait_loop(wait, &mut handle);
}
