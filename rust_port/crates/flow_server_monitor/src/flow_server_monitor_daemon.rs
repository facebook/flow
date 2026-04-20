/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::io::Write;
use std::sync::Mutex;
use std::sync::OnceLock;

use flow_common_exit_status::FlowExitStatus;
use flow_common_exit_status::error_code;
use flow_common_exit_status::exit;
use flow_event_logger::LoggingContext;

use crate::flow_server_monitor_options::MonitorOptions as FlowServerMonitorOptions;

pub type StartFunction = fn(Option<Box<dyn Write + Send>>, FlowServerMonitorOptions);

// When `flow start --wait` daemonizes the Flow server monitor, it listens over a pipe and waits
// for the Flow server to finish initializing. These are the messages we send over the pipe
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub enum WaitMsg {
    /// Monitor is up. All `flow start` commands wait for this
    Starting,
    /// Server is done initializing. `flow start --wait` commands wait for this
    Ready,
}

pub struct State {
    pub monitor_options: FlowServerMonitorOptions,
    pub init_id: String,
    pub logging_context: LoggingContext,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct EntryPoint {
    pub name: &'static str,
}

fn registry() -> &'static Mutex<HashMap<&'static str, StartFunction>> {
    static REGISTRY: OnceLock<Mutex<HashMap<&'static str, StartFunction>>> = OnceLock::new();
    REGISTRY.get_or_init(|| Mutex::new(HashMap::new()))
}

pub const MONITOR_ENTRY_NAME: &str = "monitor";

pub const ENTRY_POINT_ENV: &str = "FLOW_DAEMON_ENTRY_POINT";

// When the daemonized monitor process starts up, this is the first code it runs
pub fn register_entry_point(start: StartFunction) -> EntryPoint {
    registry().lock().unwrap().insert(MONITOR_ENTRY_NAME, start);
    EntryPoint {
        name: MONITOR_ENTRY_NAME,
    }
}

pub fn dispatch_entry_point(state: State, waiting_fd: Option<Box<dyn Write + Send>>) -> Option<()> {
    let name = std::env::var(ENTRY_POINT_ENV).ok()?;
    let entry = registry().lock().unwrap().get(name.as_str()).copied()?;
    let State {
        monitor_options,
        init_id,
        logging_context,
    } = state;

    // Disassociate this process with the process that spawned it
    #[cfg(unix)]
    {
        match nix::unistd::setsid() {
            Ok(_pid) => {}
            Err(nix::errno::Errno::EPERM) => {}
            Err(e) => {
                log::debug!("setsid failed: {}", e);
            }
        }
    }

    // We never read from this channel, so close it

    // Set up various logging related things
    flow_logging_utils::set_hh_logger_min_level(None, &monitor_options.server_options);
    flow_event_logger::restore_context(logging_context);
    flow_event_logger::set_command(Some("monitor".to_string()));
    flow_event_logger::init_flow_command(&init_id);

    entry(waiting_fd, monitor_options);
    Some(())
}

// The monitor can communicate with the process that spawned it over a pipe.
// The current scheme has it write a message when it starts up and has the
// lock and then write another message when it has finished initializing.
// It's up to the forking process whether it cares to wait for the
// initialization to complete
pub(crate) fn wait_loop(
    should_wait: bool,
    child: &mut std::process::Child,
    ic: &mut std::process::ChildStdout,
) {
    let msg: WaitMsg =
        match bincode::deserialize_from::<&mut std::process::ChildStdout, WaitMsg>(ic) {
            Ok(msg) => msg,
            Err(_end_of_file) => {
                // The pipe broke before we got the all-clear from the monitor. What kind
                // of things could go wrong? Well we check the lock before forking the
                // monitor, but maybe by the time the monitor started someone else had
                // grabbed the lock, so it exited. I'm sure there's a million other
                // things that could have gone wrong
                let child_pid = child.id();
                let (pid, status): (u32, Option<std::process::ExitStatus>) = match child.try_wait()
                {
                    Ok(None) => {
                        // Sometimes the End_of_file races the child process actually
                        // exiting. In case that's happening here, let's give the child 1
                        // second more to die
                        std::thread::sleep(std::time::Duration::from_secs(1));
                        match child.try_wait() {
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
                                // Sometimes when we actually go to start the monitor we find a
                                // monitor already running (race condition). If so, we can just
                                // forward that error code
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
        wait_loop(should_wait, child, ic);
    }
}

pub fn daemonize(
    wait: bool,
    on_spawn: impl FnOnce(u32),
    init_id: String,
    monitor_options: FlowServerMonitorOptions,
    entry_point: EntryPoint,
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
     * Windows 7 doesn't actually leak stdio, so a no op is acceptable
     */
    let _root_str = monitor_options
        .server_options
        .root
        .to_string_lossy()
        .into_owned();

    let _state = State {
        monitor_options: monitor_options.clone(),
        init_id: init_id.clone(),
        logging_context: flow_event_logger::get_context(),
    };

    let server_options = &monitor_options.server_options;
    let args = crate::flow_server_monitor::DaemonizeArgs {
        flowconfig_name: server_options.flowconfig_name.to_string(),
        no_flowlib: false,
        ignore_version: false,
        include_suppressions: server_options.include_suppressions,
        all: server_options.all,
        wait,
        no_restart: monitor_options.no_restart,
        autostop: monitor_options.autostop,
        lazy_mode: if server_options.lazy_mode {
            Some("true".to_string())
        } else {
            None
        },
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
        verbose: server_options.verbose.is_some(),
        server_log_file: monitor_options.server_log_file.clone(),
        monitor_log_file: monitor_options.log_file.clone(),
        from: flow_event_logger::get_from_i_am_a_clown(),
        saved_state_fetcher: Some(server_options.saved_state_fetcher.clone()),
        saved_state_force_recheck: server_options.saved_state_force_recheck,
        saved_state_no_fallback: server_options.saved_state_no_fallback,
        saved_state_skip_version_check: server_options.saved_state_skip_version_check,
        saved_state_verify: server_options.saved_state_verify,
        no_cgroup: false,
        root: (*server_options.root).clone(),
        temp_dir: server_options.temp_dir.to_string(),
    };

    // We never write to the child process so we can close this channel
    let (mut child, mut ic) =
        match crate::flow_server_monitor::daemonize_with_pipe(args, entry_point.name) {
            Ok(pair) => pair,
            Err(err) => {
                eprintln!("{}", err);
                exit(FlowExitStatus::ServerStartFailed);
            }
        };
    let pid = child.id();

    on_spawn(pid);

    // If wait is true, wait for the "Ready" message.
    // Otherwise, only wait for the "Starting message"
    wait_loop(wait, &mut child, &mut ic);
}
