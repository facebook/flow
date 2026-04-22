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
pub use flow_server::standalone::LazyStats;
use flow_server_files::server_files_js;

use crate::flow_server_monitor_daemon::WaitMsg;
use crate::flow_server_monitor_options::FileWatcher;
use crate::flow_server_monitor_options::MonitorOptions;
use crate::flow_server_monitor_options::SharedMemConfig;
use crate::status_stream;

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
    pub verbose: bool,
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

fn remove_artifact_if_present(path: &str) {
    match std::fs::remove_file(path) {
        Ok(()) => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
        Err(e) => {
            eprintln!("monitor: failed to remove {}: {}", path, e);
        }
    }
}

fn spawn_monitor_child(
    args: &DaemonizeArgs,
    extra_env: &[(&str, String)],
    stdout: std::process::Stdio,
) -> Result<std::process::Child, String> {
    let DaemonizeArgs {
        flowconfig_name,
        no_flowlib,
        ignore_version,
        include_suppressions,
        all,
        wait: _,
        no_restart,
        autostop,
        lazy_mode,
        long_lived_workers,
        max_workers,
        wait_for_recheck,
        file_watcher,
        file_watcher_debug,
        file_watcher_timeout,
        file_watcher_mergebase_with,
        file_watcher_sync_timeout,
        shm_heap_size,
        shm_hash_table_pow,
        profile,
        verbose,
        server_log_file,
        monitor_log_file,
        from,
        saved_state_fetcher,
        saved_state_force_recheck,
        saved_state_no_fallback,
        saved_state_skip_version_check,
        saved_state_verify,
        no_cgroup,
        root,
        temp_dir,
    } = args;

    prepare_log_file(monitor_log_file)?;

    // The daemon child IS the monitor process. Its stderr should be wired to the monitor log
    // file, not the server log file. The server (which runs as a thread inside the monitor
    // process) writes to the server log file via its own logger initialization.
    let log_file = flow_server::server_daemon::try_open_log_file(monitor_log_file)?;
    let exe = std::env::args_os()
        .next()
        .ok_or_else(|| "failed to get argv[0]".to_string())?;

    let mut cmd = std::process::Command::new(&exe);
    cmd.arg("server");
    if *no_flowlib {
        cmd.arg("--no-flowlib");
    }
    if *ignore_version {
        cmd.arg("--ignore-version");
    }
    if *include_suppressions {
        cmd.arg("--include-suppressed");
    }
    if *all {
        cmd.arg("--all");
    }
    if *no_restart {
        cmd.arg("--no-auto-restart");
    }
    if *autostop {
        cmd.arg("--autostop");
    }
    if let Some(mode) = lazy_mode {
        cmd.arg("--lazy-mode").arg(mode);
    }
    if let Some(long_lived_workers) = long_lived_workers {
        cmd.arg("--long-lived-workers")
            .arg(long_lived_workers.to_string());
    }
    if let Some(max_workers) = max_workers {
        cmd.arg("--max-workers").arg(max_workers.to_string());
    }
    if let Some(wait_for_recheck) = wait_for_recheck {
        cmd.arg("--wait-for-recheck")
            .arg(wait_for_recheck.to_string());
    }
    let file_watcher_str = match file_watcher {
        FileWatcher::NoFileWatcher => "none",
        FileWatcher::DFind => "dfind",
        FileWatcher::Watchman(_) => "watchman",
        FileWatcher::EdenFS(_) => "edenfs",
    };
    cmd.arg("--file-watcher").arg(file_watcher_str);
    if *file_watcher_debug {
        cmd.arg("--file-watcher-debug");
    }
    if let Some(file_watcher_timeout) = file_watcher_timeout {
        cmd.arg("--file-watcher-timeout")
            .arg(file_watcher_timeout.to_string());
    }
    if let Some(file_watcher_mergebase_with) = file_watcher_mergebase_with {
        cmd.arg("--file-watcher-mergebase-with")
            .arg(file_watcher_mergebase_with);
    }
    if let Some(file_watcher_sync_timeout) = file_watcher_sync_timeout {
        cmd.arg("--file-watcher-sync-timeout")
            .arg(file_watcher_sync_timeout.to_string());
    }
    if let Some(shm_heap_size) = shm_heap_size {
        cmd.arg("--sharedmemory-heap-size")
            .arg(shm_heap_size.to_string());
    }
    if let Some(shm_hash_table_pow) = shm_hash_table_pow {
        cmd.arg("--sharedmemory-hash-table-pow")
            .arg(shm_hash_table_pow.to_string());
    }
    if *profile {
        cmd.arg("--profile");
    }
    if *verbose {
        cmd.arg("--verbose");
    }
    if *no_cgroup {
        cmd.arg("--no-cgroup");
    }
    cmd.arg("--flowconfig-name").arg(flowconfig_name);
    cmd.arg("--log-file").arg(server_log_file);
    cmd.arg("--monitor-log-file").arg(monitor_log_file);
    cmd.arg("--temp-dir").arg(temp_dir);
    if let Some(from) = from {
        cmd.arg("--from").arg(from);
    }
    if let Some(fetcher) = saved_state_fetcher {
        let s = match fetcher {
            SavedStateFetcher::DummyFetcher => "none",
            SavedStateFetcher::LocalFetcher => "local",
            SavedStateFetcher::ScmFetcher => "scm",
            SavedStateFetcher::FbFetcher => "fb",
        };
        cmd.arg("--saved-state-fetcher").arg(s);
    }
    if *saved_state_force_recheck {
        cmd.arg("--saved-state-force-recheck");
    }
    if *saved_state_no_fallback {
        cmd.arg("--saved-state-no-fallback");
    }
    if *saved_state_skip_version_check {
        cmd.arg("--saved-state-skip-version-check-DO_NOT_USE_OR_YOU_WILL_BE_FIRED");
    }
    if *saved_state_verify {
        cmd.arg("--saved-state-verify");
    }
    for (k, v) in extra_env {
        cmd.env(k, v);
    }
    cmd.arg(root.to_string_lossy().as_ref());
    cmd.stdin(std::process::Stdio::null());
    cmd.stdout(stdout);
    cmd.stderr(std::process::Stdio::from(log_file));

    cmd.spawn()
        .map_err(|e| format!("failed to spawn server: {}", e))
}

pub fn daemonize_with_pipe(
    args: DaemonizeArgs,
    entry_point_name: &'static str,
) -> Result<(std::process::Child, std::process::ChildStdout), String> {
    let extra_env: [(&str, String); 1] = [(
        crate::flow_server_monitor_daemon::ENTRY_POINT_ENV,
        entry_point_name.to_string(),
    )];

    let mut child = spawn_monitor_child(&args, &extra_env, std::process::Stdio::piped())?;
    let input_channel = child
        .stdout
        .take()
        .ok_or_else(|| "child stdout pipe missing".to_string())?;

    Ok((child, input_channel))
}

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
    let lock_path = server_files_js::lock_file(
        &args.flowconfig_name,
        options.temp_dir.as_str(),
        options.root.as_path(),
    );
    if !flow_common::lock::grab(&lock_path) {
        return Err(format!(
            "There is already a server running for {}",
            options.root.display()
        ));
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

// The entry point for creating a daemonized flow server monitor (like from `flow start`)
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

    let socket_path = server_files_js::socket_file(&args.flowconfig_name, &args.temp_dir, &root);
    remove_artifact_if_present(&socket_path);

    let wait = args.wait;

    let (mut child, mut ic) =
        daemonize_with_pipe(args, crate::flow_server_monitor_daemon::MONITOR_ENTRY_NAME)?;
    let pid = child.id();

    // If wait is true, wait for the "Ready" message.
    // Otherwise, only wait for the "Starting message"
    crate::flow_server_monitor_daemon::wait_loop(wait, &mut child, &mut ic);
    drop(ic);

    Ok(pid)
}

// The entry point for creating a non-daemonized flow server monitor (like from `flow server`)
pub fn start(options: Arc<Options>, args: StartArgs) -> Result<(), String> {
    // So this is a tricky situation. Technically this code is running in the `flow server` process.
    // However, we kind of want the actual Flow server to log using the "server" command, and we don't
    // want the monitor's logs to interfere. So instead, we'll pretend like the monitor was created
    // with some imaginary `flow monitor` command
    flow_event_logger::set_command(Some("monitor".to_string()));

    let is_daemon = std::env::var_os(crate::flow_server_monitor_daemon::ENTRY_POINT_ENV).is_some();
    if is_daemon {
        let waiting_fd: Box<dyn Write + Send> = Box::new(std::io::stdout());
        internal_start(true, Some(waiting_fd), options, args)
    } else {
        //   internal_start ~is_daemon:false ?waiting_fd:None monitor_options
        internal_start(false, None, options, args)
    }
}
