/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io::Write;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::Mutex;

use flow_common::options::Options;
use flow_common::options::SavedStateFetcher;
use flow_common::verbose::Verbose;
pub use flow_server::standalone::LazyStats;
use flow_server_files::server_files_js;

use crate::flow_server_monitor_daemon::WaitMsg;
use crate::flow_server_monitor_options::FileWatcher;
use crate::flow_server_monitor_options::MonitorOptions;
use crate::flow_server_monitor_options::SharedMemConfig;
use crate::status_stream;

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct DaemonizeArgs {
    pub flowconfig_name: String,
    pub no_flowlib: bool,
    pub ignore_version: bool,
    pub include_suppressions: bool,
    pub all: bool,
    pub wait: bool,
    pub no_restart: bool,
    pub autostop: bool,
    pub lazy_mode: Option<String>,
    pub long_lived_workers: Option<bool>,
    pub max_workers: Option<i32>,
    pub wait_for_recheck: Option<bool>,
    pub file_watcher: FileWatcher,
    pub file_watcher_debug: bool,
    pub file_watcher_timeout: Option<u32>,
    pub file_watcher_mergebase_with: Option<String>,
    pub file_watcher_sync_timeout: Option<u32>,
    pub shm_heap_size: Option<u64>,
    pub shm_hash_table_pow: Option<u32>,
    pub profile: bool,
    pub debug: bool,
    pub quiet: bool,
    pub verbose: Option<Verbose>,
    pub server_log_file: String,
    pub monitor_log_file: String,
    pub from: Option<String>,
    pub saved_state_fetcher: Option<SavedStateFetcher>,
    pub saved_state_force_recheck: bool,
    pub saved_state_no_fallback: bool,
    pub saved_state_skip_version_check: bool,
    pub saved_state_verify: bool,
    pub no_cgroup: bool,
    pub root: PathBuf,
    pub temp_dir: String,
}

pub struct StartArgs {
    pub flowconfig_name: String,
    pub server_log_file: String,
    pub monitor_log_file: String,
    pub lazy_mode: Option<String>,
    pub no_flowlib: bool,
    pub ignore_version: bool,
    pub wait_for_recheck: Option<bool>,
    pub file_watcher: FileWatcher,
    pub file_watcher_debug: bool,
    pub file_watcher_timeout: Option<u32>,
    pub file_watcher_mergebase_with: Option<String>,
    pub file_watcher_sync_timeout: Option<u32>,
    pub shm_heap_size: Option<u64>,
    pub shm_hash_table_pow: Option<u32>,
    pub from: Option<String>,
    pub autostop: bool,
    pub no_restart: bool,
}

fn prepare_log_file(log_file: &str) -> Result<(), String> {
    flow_server::server_daemon::try_open_log_file(log_file).map(|_| ())
}

// `spawn_monitor_child` and `daemonize_with_pipe` (~150 lines of
// CLI-flag re-encoding + `Stdio::piped` plumbing for the WaitMsg) were
// removed in the daemon rewire (Step 3 of the daemon plan): the child is
// now spawned by `flow_daemon::spawn` with the `MonitorEntryParam` shipped
// via tempfile + bincode, and the `WaitMsg` flows over a typed
// `flow_daemon` channel rather than `child.stdout`. See `daemonize` below.

// We want to send a "Starting" message immediately and a "Ready" message when the Flow server is
// ready for the first time. This function sends the "Starting" message immediately and sets up a
// callback for when the server is ready
// let handle_waiting_start_command waiting_fd =
pub fn handle_waiting_start_command(waiting_fd: Box<dyn Write + Send>) {
    let shared: Arc<Mutex<Option<Box<dyn Write + Send>>>> = Arc::new(Mutex::new(Some(waiting_fd)));
    // Close the fd, but don't worry if it's already closed
    let close = {
        let shared = shared.clone();
        move || {
            let mut guard = shared.lock().unwrap();
            if let Some(file) = guard.take() {
                drop(file);
            }
        }
    };
    let send_message = {
        let shared = shared.clone();
        let close = close.clone();
        move |msg: &WaitMsg| {
            let mut guard = shared.lock().unwrap();
            if let Some(file) = guard.as_mut() {
                let write_err: Option<(std::io::ErrorKind, String)> =
                    match bincode::serialize_into(&mut *file, msg) {
                        Ok(()) => match file.flush() {
                            Ok(()) => None,
                            Err(e) => Some((e.kind(), e.to_string())),
                        },
                        Err(e) => {
                            let kind = if let bincode::ErrorKind::Io(io_err) = e.as_ref() {
                                io_err.kind()
                            } else {
                                std::io::ErrorKind::Other
                            };
                            Some((kind, e.to_string()))
                        }
                    };
                if let Some((kind, msg)) = write_err {
                    match kind {
                        std::io::ErrorKind::BrokenPipe | std::io::ErrorKind::InvalidInput => {
                            drop(guard);
                            close();
                        }
                        _ => {
                            log::error!(
                                "Unexpected exception when talking to waiting start command: {}",
                                msg
                            );
                            drop(guard);
                            close();
                        }
                    }
                }
            }
        }
    };
    //  Send a message to the fd, but don't worry if it's already closed
    send_message(&WaitMsg::Starting);
    let send_message_for_ready = send_message;
    let close_for_ready = close;
    status_stream::call_on_free(Box::new(move || {
        send_message_for_ready(&WaitMsg::Ready);
        close_for_ready();
    }));
}

fn fallback_error_handler(msg: &str, payload: Box<dyn std::any::Any + Send>) -> ! {
    let exn_str = if let Some(s) = payload.downcast_ref::<&'static str>() {
        (*s).to_string()
    } else if let Some(s) = payload.downcast_ref::<String>() {
        s.clone()
    } else {
        "unknown panic payload".to_string()
    };
    let msg = format!("{}: {}", msg, exn_str);
    log::error!("{}. Exiting", msg);
    eprintln!("{}. Exiting", msg);
    flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::UnknownError);
}

// This is the common entry point for both daemonize and start.
fn internal_start(
    is_daemon: bool,
    waiting_fd: Option<Box<dyn Write + Send>>,
    options: Arc<Options>,
    args: StartArgs,
) -> Result<(), String> {
    if matches!(args.file_watcher, FileWatcher::EdenFS(_)) {
        crate::startup_initializer::init();
    }
    let root = options.root.as_path();
    {
        let file_watcher =
            crate::flow_server_monitor_options::string_of_file_watcher(&args.file_watcher);
        let vcs = match flow_common_vcs::vcs::find(None, root) {
            None => "none",
            Some(flow_common_vcs::vcs::Vcs::Hg) => "hg",
            Some(flow_common_vcs::vcs::Vcs::Git) => "git",
        };
        flow_event_logger::set_monitor_options(file_watcher.to_string(), vcs.to_string());
        flow_event_logger::set_eden(Some(flow_common_vcs::eden::is_eden(root)));
        flow_logging_utils::set_server_options(&options);
    }
    let lock_path = server_files_js::lock_file(
        &args.flowconfig_name,
        options.temp_dir.as_str(),
        options.root.as_path(),
    );
    if !flow_common::lock::grab(&lock_path) {
        let msg = "Error: another server is already running?\n";
        flow_common_exit_status::exit_with_msg(
            flow_common_exit_status::FlowExitStatus::LockStolen,
            msg,
        );
    }
    // We can't open the log until we have the lock.
    //
    // The daemon wants to redirect all stderr to the log. So we can dup2
    // `flow server` wants to output to both stderr and the log, so we initialize Logger with this fd
    let log_fd = {
        let fd = flow_server::server_daemon::try_open_log_file(&args.monitor_log_file)?;
        if is_daemon { None } else { Some(fd) }
    };
    // Open up the socket immediately. When a client tries to connect to an
    // open socket, it will block. When a client tries to connect to a not-yet-open
    // socket, it will fail immediately. The blocking behavior is a little nicer
    let monitor_socket_fd =
        flow_common_socket::socket::init_tcp_socket(&server_files_js::socket_file(
            &args.flowconfig_name,
            options.temp_dir.as_str(),
            options.root.as_path(),
        ));
    let legacy2_socket_fd =
        flow_common_socket::socket::init_tcp_socket(&server_files_js::legacy2_socket_file(
            &args.flowconfig_name,
            options.temp_dir.as_str(),
            options.root.as_path(),
        ));
    let legacy1_socket_fd =
        flow_common_socket::socket::init_tcp_socket(&server_files_js::legacy1_socket_file(
            &args.flowconfig_name,
            options.temp_dir.as_str(),
            options.root.as_path(),
        ));
    // ************************* HERE BEGINS THE MAGICAL WORLD OF LWT *********************************
    crate::flow_server_monitor_logger::init_logger(log_fd);
    let StartArgs {
        flowconfig_name: _,
        server_log_file,
        monitor_log_file,
        lazy_mode,
        no_flowlib,
        ignore_version,
        wait_for_recheck: _,
        file_watcher,
        file_watcher_debug: _,
        file_watcher_timeout,
        file_watcher_mergebase_with,
        file_watcher_sync_timeout: _,
        shm_heap_size,
        shm_hash_table_pow,
        from: _,
        autostop,
        no_restart,
    } = args;
    let monitor_options = MonitorOptions {
        log_file: monitor_log_file,
        autostop,
        no_restart,
        server_log_file,
        server_options: (*options).clone(),
        lazy_mode,
        no_flowlib,
        ignore_version,
        shared_mem_config: SharedMemConfig {
            heap_size: shm_heap_size.unwrap_or(0),
            hash_table_pow: shm_hash_table_pow.unwrap_or(0),
        },
        argv: std::env::args().collect(),
        file_watcher,
        file_watcher_timeout: file_watcher_timeout.map(f64::from),
        file_watcher_mergebase_with: file_watcher_mergebase_with.unwrap_or_default(),
    };
    let acceptor_autostop = monitor_options.autostop;
    // We can start up the socket acceptor even before the server starts.
    // Spawn the accept threads BEFORE notifying the parent that we are
    // "Starting", and wait for each thread to confirm it has actually entered
    // the accept loop. This ensures that as soon as `flow start` returns,
    // clients have a live accept loop to connect to. OCaml's Lwt-based accept
    // loop runs as soon as the scheduler yields (after the current thread
    // calls `Lwt_unix.accept`), but Rust threads must be spawned explicitly
    // and may not be scheduled immediately under heavy parallel load.
    let (ready_tx_main, ready_rx) = std::sync::mpsc::sync_channel::<()>(0);
    let ready_tx_legacy2 = ready_tx_main.clone();
    let ready_tx_legacy1 = ready_tx_main.clone();
    std::thread::spawn(move || {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            // The receiver only disconnects if the parent panics, in which
            // case the fallback_error_handler will exit the process — so a
            // send error here is safe to ignore.
            match ready_tx_main.send(()) {
                Ok(()) => {}
                Err(_) => return,
            }
            crate::socket_acceptor::run(monitor_socket_fd, acceptor_autostop);
        })) {
            Ok(()) => {}
            Err(payload) => {
                fallback_error_handler("Uncaught exception in SocketAcceptor thread", payload);
            }
        }
    });
    std::thread::spawn(move || {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            match ready_tx_legacy2.send(()) {
                Ok(()) => {}
                Err(_) => return,
            }
            crate::socket_acceptor::run_legacy(legacy2_socket_fd);
        })) {
            Ok(()) => {}
            Err(payload) => {
                fallback_error_handler(
                    "Uncaught exception in SocketAcceptor legacy thread",
                    payload,
                );
            }
        }
    });
    std::thread::spawn(move || {
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            match ready_tx_legacy1.send(()) {
                Ok(()) => {}
                Err(_) => return,
            }
            crate::socket_acceptor::run_legacy(legacy1_socket_fd);
        })) {
            Ok(()) => {}
            Err(payload) => {
                fallback_error_handler(
                    "Uncaught exception in SocketAcceptor legacy thread",
                    payload,
                );
            }
        }
    });
    // Wait for all 3 accept threads to confirm they have started.
    // sync_channel(0) is a rendezvous: each `send` blocks until the receive
    // side runs `recv`, so by the time all 3 recvs return, all 3 threads have
    // executed at least up to the line right before `listener.accept()`.
    for _ in 0..3 {
        match ready_rx.recv() {
            Ok(()) => {}
            Err(_) => break,
        }
    }
    if let Some(fd) = waiting_fd {
        handle_waiting_start_command(fd);
    }
    // Don't start the server until we've set up the threads to handle the waiting channel
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        crate::flow_server_monitor_server::start(monitor_options);
    })) {
        Ok(()) => {}
        Err(payload) => {
            fallback_error_handler(
                "Uncaught exception in FlowServerMonitorServer thread",
                payload,
            );
        }
    }
    Ok(())
}

// The entry point for creating a daemonized flow server monitor (like from
// `flow start`). Spawns the monitor child process via `flow_daemon::spawn`,
// passing a serialized `MonitorEntryParam` (the bincode-friendly equivalent
// of OCaml's `Daemon.spawn ... entry_point state`). The previous Rust impl
// re-encoded `DaemonizeArgs` as ~30 CLI flags here; that is gone. The child
// dispatches via `HH_SERVER_DAEMON=monitor` (set by `flow_daemon::spawn` on
// the child's env) and runs the registered monitor entry handler in
// `flow_server_monitor_daemon`.
pub fn daemonize(args: DaemonizeArgs) -> Result<u32, String> {
    // Let's make sure this isn't all for naught before we fork
    let root = args
        .root
        .canonicalize()
        .unwrap_or_else(|_| args.root.clone());
    let lock_path = server_files_js::lock_file(&args.flowconfig_name, &args.temp_dir, &root);
    if !flow_common::lock::check(&lock_path) {
        return Err(format!(
            "There is already a server running for {}",
            root.display()
        ));
    }

    let wait = args.wait;
    let monitor_log_file = args.monitor_log_file.clone();
    let root_str = args.root.to_string_lossy().into_owned();

    prepare_log_file(&monitor_log_file)?;
    let log_for_stdout = flow_server::server_daemon::try_open_log_file(&monitor_log_file)?;
    let log_for_stderr = flow_server::server_daemon::try_open_log_file(&monitor_log_file)?;

    let entry = crate::flow_server_monitor_daemon::registered_entry_point();

    // Stdio: stdin from /dev/null, stdout/stderr to the monitor log file.
    // (`flow start` keeps the user's TTY clean by detaching stdio entirely.)
    let stdio = (
        flow_daemon::StdioFd::Owned(flow_daemon::null_fd()),
        flow_daemon::StdioFd::Owned(log_for_stdout),
        flow_daemon::StdioFd::Owned(log_for_stderr),
    );

    let init_id = std::env::var("FLOW_INIT_ID").unwrap_or_default();
    let param = crate::flow_server_monitor_daemon::MonitorEntryParam {
        daemonize_args: args,
        init_id,
        logging_context: flow_event_logger::get_context(),
    };

    let name = format!("monitor for {}", root_str);
    let mut handle = flow_daemon::spawn(None, Some(&name), stdio, entry, param)
        .map_err(|e| format!("failed to spawn monitor: {}", e))?;
    let pid = handle.child.id();

    // We never write to the child process so we can close this channel
    // (mirrors OCaml `Daemon.close_out oc`).
    if let Err(e) = flow_daemon::shutdown_out_write(&mut handle.channels.1) {
        log::debug!("failed to shutdown monitor parent_out: {}", e);
    }

    // If wait is true, wait for the "Ready" message.
    // Otherwise, only wait for the "Starting" message.
    crate::flow_server_monitor_daemon::wait_loop(wait, &mut handle);

    Ok(pid)
}

// The entry point for creating a non-daemonized flow server monitor (like
// from `flow server`). Always foreground: the previous `is_daemon` env-var
// check is gone because the daemonized path now dispatches via
// `flow_daemon::check_entry_point` BEFORE `start` is called, so there is no
// in-process daemonized invocation of `start` anymore.
pub fn start(options: Arc<Options>, args: StartArgs) -> Result<(), String> {
    // So this is a tricky situation. Technically this code is running in the `flow server` process.
    // However, we kind of want the actual Flow server to log using the "server" command, and we don't
    // want the monitor's logs to interfere. So instead, we'll pretend like the monitor was created
    // with some imaginary `flow monitor` command
    flow_event_logger::set_command(Some("monitor".to_string()));
    internal_start(false, None, options, args)
}

/// Foreground entry used by daemon child processes. Same as `start` but
/// passes a `waiting_fd` so the child can send `WaitMsg::Starting`/`Ready`
/// back to the parent over the pipe `flow_daemon::spawn` set up.
pub fn start_in_daemon(
    waiting_fd: Box<dyn Write + Send>,
    options: Arc<Options>,
    args: StartArgs,
) -> Result<(), String> {
    flow_event_logger::set_command(Some("monitor".to_string()));
    internal_start(true, Some(waiting_fd), options, args)
}
