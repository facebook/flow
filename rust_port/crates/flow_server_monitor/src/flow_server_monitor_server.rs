/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The Flow server monitor will start one or more Flow servers over its lifetime. This module is how
// the monitor interacts with the server. The basic idea is that there is a long-lived stream of
// requests, which outlives servers, and a ServerInstance.t that wraps a connection to the server.
//
// When a server is alive, the long lived stream of requests gets written to the server and stored
// in a RequestMap. When a response is received, we look up the client in the RequestMap and forward
// the response.
//
// When a server dies, the monitor decides whether or not to die with the server. If it doesn't die,
// it creates a new server. Any request that was written to the old server but never received a
// response will be written again to the new server
pub enum Command {
    WriteEphemeralRequest {
        request: flow_server_env::server_command_with_context::ServerCommandWithContext,
        client: std::sync::Arc<crate::flow_server_monitor_connection::EphemeralConnection>,
    },
    WritePersistentRequest {
        client_id: flow_server_env::lsp_prot::ClientId,
        request: flow_server_env::lsp_prot::RequestWithMetadata,
    },
    NotifyNewPersistentConnection {
        client_id: flow_server_env::lsp_prot::ClientId,
        lsp_init_params: lsp_types::InitializeParams,
    },
    NotifyDeadPersistentConnection {
        client_id: flow_server_env::lsp_prot::ClientId,
    },
    NotifyFileChanges,
}

// A wrapper for Stdlib.exit which gives other threads a second to handle their business
// before the monitor exits
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
static EXITING: AtomicBool = AtomicBool::new(false);

// A broadcast notification for monitor exit. Loops that need to wake up
// deterministically on exit (instead of polling the EXITING flag) clone
// `EXIT_SIGNAL.1` and select on it. When `signal_exit_to_loops` drops the
// sender, all cloned receivers see Disconnected on their next recv.
static EXIT_SIGNAL: std::sync::LazyLock<(
    std::sync::Mutex<Option<crossbeam::channel::Sender<()>>>,
    crossbeam::channel::Receiver<()>,
)> = std::sync::LazyLock::new(|| {
    let (tx, rx) = crossbeam::channel::unbounded();
    (std::sync::Mutex::new(Some(tx)), rx)
});

pub fn exit_signal_receiver() -> crossbeam::channel::Receiver<()> {
    EXIT_SIGNAL.1.clone()
}

fn signal_exit_to_loops() {
    *EXIT_SIGNAL.0.lock().unwrap() = None;
}

pub fn exit(
    _error: Option<(String, String)>,
    msg: &str,
    exit_status: flow_common_exit_status::FlowExitStatus,
) -> ! {
    if EXITING.swap(true, Ordering::SeqCst) {
        // We're already exiting, so there's nothing to do. But no one expects `exit` to return, so
        // let's just wait forever
        loop {
            std::thread::park();
        }
    }
    flow_hh_logger::info!("Monitor is exiting code {:?} ({})", exit_status, msg);

    flow_hh_logger::info!("Broadcasting to threads and waiting 1 second for them to exit");
    crate::exit_signal::SIGNAL.broadcast(exit_status, msg.to_string());
    signal_exit_to_loops();

    // Protect this thread from getting canceled
    std::thread::sleep(std::time::Duration::from_secs(1));
    flow_common_exit_status::exit(exit_status);
}

pub enum StopReason {
    /// `flow stop`
    Stopped,
    /// no more active connections
    Autostopped,
    /// very old client tried to connect
    LegacyClient,
}

pub fn stop(reason: StopReason) -> ! {
    let (msg, status) = match reason {
        StopReason::Stopped => (
            "Killed by `flow stop`. Exiting.",
            flow_common_exit_status::FlowExitStatus::NoError,
        ),
        StopReason::Autostopped => (
            "Autostop",
            flow_common_exit_status::FlowExitStatus::Autostop,
        ),
        StopReason::LegacyClient => (
            "Killed by legacy client. Exiting.",
            flow_common_exit_status::FlowExitStatus::BuildIdMismatch,
        ),
    };
    exit(None, msg, status);
}

// Exit after 7 days of no requests
pub mod doomsday {
    use std::sync::Mutex;

    const SEVEN_DAYS_IN_SECS: f64 = 3600.0 * 24.0 * 7.0;

    fn time_in_seven_days() -> f64 {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_secs_f64();
        now + SEVEN_DAYS_IN_SECS
    }

    static DOOMSDAY_TIME: Mutex<f64> = Mutex::new(0.0);

    pub fn postpone() {
        let mut time = DOOMSDAY_TIME.lock().unwrap();
        *time = time_in_seven_days();
    }

    pub fn start_clock() {
        {
            let mut time = DOOMSDAY_TIME.lock().unwrap();
            *time = time_in_seven_days();
        }
        loop {
            let time_til_doomsday = {
                let time = DOOMSDAY_TIME.lock().unwrap();
                let now = std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap()
                    .as_secs_f64();
                *time - now
            };
            if time_til_doomsday <= 0.0 {
                super::exit(
                    None,
                    "Exiting server. Last used >7 days ago",
                    flow_common_exit_status::FlowExitStatus::UnusedServer,
                );
            } else {
                std::thread::sleep(std::time::Duration::from_secs_f64(time_til_doomsday));
            }
        }
    }
}

// The long-lived stream of requests in the monitor that have arrived from client
// This is unbounded, because otherwise lspCommand might deadlock.
use std::sync::Mutex;

static COMMAND_STREAM: std::sync::LazyLock<(
    crossbeam::channel::Sender<Command>,
    Mutex<crossbeam::channel::Receiver<Command>>,
)> = std::sync::LazyLock::new(|| {
    let (sender, receiver) = crossbeam::channel::unbounded();
    (sender, Mutex::new(receiver))
});

fn push_to_command_stream(cmd: Command) {
    if let Err(e) = COMMAND_STREAM.0.send(cmd) {
        flow_hh_logger::warn!("Failed to push to command stream: {}", e);
    }
}

// ServerInstance.t is an individual Flow server instance. The code inside this module handles
// interacting with a Flow server instance
pub mod server_instance {
    use std::sync::Arc;
    use std::sync::Mutex;
    use std::sync::atomic::Ordering;
    use std::thread::JoinHandle;

    use flow_common_exit_status::FlowExitStatus;
    use flow_daemon::Handle;
    use flow_server_env::lsp_prot;
    use flow_server_env::monitor_prot;

    use crate::flow_server_monitor_connection::ServerConnection;

    pub struct ServerInstance {
        pub pid: i32,
        pub watcher: Arc<crate::file_watcher::AnyWatcher>,
        pub connection: Arc<ServerConnection>,
        pub command_loop: Option<JoinHandle<()>>,
        pub file_watcher_loop: Option<JoinHandle<()>>,
        pub on_exit_thread: Option<JoinHandle<()>>,
        pub file_watcher_exit_thread: Option<JoinHandle<()>>,
        // Holding the sender side keeps cloned receivers alive. Dropping this
        // (in `cleanup`) broadcasts Disconnected to every clone, letting waiters
        // in the command and file-watcher loops wake immediately.
        pub shutdown_tx: std::sync::Mutex<Option<crossbeam::channel::Sender<()>>>,
        pub daemon_handle: Arc<Mutex<Option<Handle<(), ()>>>>,
    }

    fn handle_response(
        msg: monitor_prot::ServerToMonitorMessage,
        _connection: &Arc<ServerConnection>,
    ) {
        match msg {
            monitor_prot::ServerToMonitorMessage::Response(request_id, response) => {
                flow_hh_logger::debug!(
                    "Read a response to request '{}' from the server!",
                    request_id
                );
                let request = crate::request_map::remove(&request_id);
                match request {
                    None => {
                        flow_hh_logger::error!("Failed to look up request '{}'", request_id);
                    }
                    Some((_req, client)) => {
                        let msg = monitor_prot::MonitorToClientMessage::Data(response);
                        if !client.write_and_close(msg) {
                            flow_hh_logger::debug!(
                                "Client for request '{}' is dead. Throwing away response",
                                request_id
                            );
                        }
                    }
                }
            }
            monitor_prot::ServerToMonitorMessage::RequestFailed(request_id, exn_str) => {
                flow_hh_logger::error!(
                    "Server threw exception when processing '{}': {}",
                    request_id,
                    exn_str
                );
                let request = crate::request_map::remove(&request_id);
                match request {
                    None => {
                        flow_hh_logger::error!("Failed to look up request '{}'", request_id);
                    }
                    Some((_req, client)) => {
                        let msg = monitor_prot::MonitorToClientMessage::ServerException(exn_str);
                        if !client.write_and_close(msg) {
                            flow_hh_logger::debug!(
                                "Client for request '{}' is dead. Throwing away response",
                                request_id
                            );
                        }
                    }
                }
            }
            monitor_prot::ServerToMonitorMessage::StatusUpdate(status) => {
                crate::status_stream::update(status);
            }
            monitor_prot::ServerToMonitorMessage::Telemetry(event) => {
                let clients = crate::persistent_connection_map::get_all_clients();
                for connection in clients {
                    let msg = lsp_prot::MessageFromServer::NotificationFromServer(
                        lsp_prot::NotificationFromServer::Telemetry(event.clone()),
                    );
                    connection.write(msg);
                }
            }
            monitor_prot::ServerToMonitorMessage::PersistentConnectionResponse(
                client_id,
                response,
            ) => match crate::persistent_connection_map::get(client_id) {
                None => {
                    flow_hh_logger::error!("Failed to look up persistent client #{}", client_id);
                }
                Some(connection) => {
                    if !connection.write(response) {
                        flow_hh_logger::debug!(
                            "Persistent client #{} is dead. Throwing away response",
                            client_id
                        );
                    }
                }
            },
        }
    }

    // Writes a message to the out-stream of the monitor, to be eventually
    // picked up by the server.
    fn send_request(msg: monitor_prot::MonitorToServerMessage, conn: &ServerConnection) {
        if !conn.write(msg) {
            // Another Lwt thread has already closed ServerConnection. We trust
            // that it will properly handle the server dying, so we can just drop
            // it here.
            flow_hh_logger::debug!("Server connection is closed. Throwing away request");
        }
    }

    fn file_watcher_notification_is_relevant(
        files: &std::collections::BTreeSet<String>,
        metadata: &Option<monitor_prot::FileWatcherMetadata>,
    ) -> bool {
        !files.is_empty()
            || metadata
                .as_ref()
                .is_some_and(|metadata| metadata.missed_changes)
    }

    // In order to try and avoid races between the file system and a command (like `flow status`),
    // we check for file system notification before sending a request to the server
    fn send_file_watcher_notification(
        watcher: &Arc<crate::file_watcher::AnyWatcher>,
        conn: &ServerConnection,
    ) {
        let watcher = watcher.clone();
        let (files, metadata, initial, debug) = crate::runtime::handle().block_on(async move {
            let (files, metadata, initial) = watcher.get_and_clear_changed_files().await;
            let debug = watcher.debug();
            (files, metadata, initial, debug)
        });
        if file_watcher_notification_is_relevant(&files, &metadata) {
            let count = files.len();
            let extra = if debug {
                format!(
                    " [{}]",
                    files.iter().cloned().collect::<Vec<_>>().join(", ")
                )
            } else {
                String::new()
            };
            flow_hh_logger::info!(
                "File watcher reported {} file{} changed{}",
                count,
                if count == 1 { "" } else { "s" },
                extra,
            );
            send_request(
                monitor_prot::MonitorToServerMessage::FileWatcherNotification {
                    files,
                    metadata,
                    initial,
                },
                conn,
            );
        } else {
            flow_hh_logger::debug!("Ignoring irrelevant file watcher notification");
        }
    }

    pub(super) enum CommandLoopOutcome {
        Continue,
        Stop,
    }

    pub(super) enum FileWatcherLoopOutcome {
        Continue,
        Stop,
    }

    fn command_loop_main(
        watcher: &Arc<crate::file_watcher::AnyWatcher>,
        conn: &ServerConnection,
        instance_shutdown: &crossbeam::channel::Receiver<()>,
        exit_signal: &crossbeam::channel::Receiver<()>,
    ) -> CommandLoopOutcome {
        let receiver = super::COMMAND_STREAM.1.lock().unwrap();
        let command = crossbeam::channel::select! {
            recv(receiver) -> result => match result {
                Ok(cmd) => cmd,
                Err(crossbeam::channel::RecvError) => return CommandLoopOutcome::Stop,
            },
            recv(instance_shutdown) -> _ => return CommandLoopOutcome::Stop,
            recv(exit_signal) -> _ => return CommandLoopOutcome::Stop,
        };
        drop(receiver);
        match command {
            super::Command::WriteEphemeralRequest { request, client } => {
                super::doomsday::postpone();
                if !client.is_closed() {
                    send_file_watcher_notification(watcher, conn);
                    let request_id = crate::request_map::add(request.clone(), client);
                    flow_hh_logger::debug!("Writing '{}' to the server connection", request_id);
                    send_request(
                        monitor_prot::MonitorToServerMessage::Request(request_id, request),
                        conn,
                    );
                } else {
                    flow_hh_logger::debug!("Skipping request from a dead ephemeral connection");
                }
            }
            super::Command::WritePersistentRequest { client_id, request } => {
                super::doomsday::postpone();
                send_file_watcher_notification(watcher, conn);
                let msg = monitor_prot::MonitorToServerMessage::PersistentConnectionRequest(
                    client_id, request,
                );
                send_request(msg, conn);
            }
            super::Command::NotifyNewPersistentConnection {
                client_id,
                lsp_init_params,
            } => {
                let msg = monitor_prot::MonitorToServerMessage::NewPersistentConnection(
                    client_id,
                    lsp_init_params,
                );
                send_request(msg, conn);
            }
            super::Command::NotifyDeadPersistentConnection { client_id } => {
                crate::persistent_connection_map::remove(client_id);
                let msg = monitor_prot::MonitorToServerMessage::DeadPersistentConnection(client_id);
                send_request(msg, conn);
            }
            super::Command::NotifyFileChanges => {
                send_file_watcher_notification(watcher, conn);
            }
        }
        CommandLoopOutcome::Continue
    }

    // The monitor is exiting. Let's try and shut down the server gracefully
    fn cleanup_on_exit(
        exit_status: FlowExitStatus,
        exit_msg: &str,
        connection: &ServerConnection,
        pid: i32,
    ) {
        let msg = monitor_prot::MonitorToServerMessage::PleaseDie(
            monitor_prot::PleaseDieReason::MonitorExiting(exit_status, exit_msg.to_string()),
        );
        if !connection.write(msg) {
            // Connection to the server has already closed. The server is likely already dead
        }
        // The monitor waits 1 second before exiting. So let's give the server .75 seconds to shutdown
        // gracefully.
        let server_status = wait_for_pid_with_timeout(pid, std::time::Duration::from_millis(750));

        connection.close_immediately();
        let pretty_pid = pid;
        let still_alive = match server_status {
            Some(WaitStatus::Exited(exit_code)) => {
                let exit_type = error_type_of_code(exit_code);
                if exit_type == Some(FlowExitStatus::KilledByMonitor) {
                    flow_hh_logger::info!("Successfully killed the server process");
                } else {
                    let exit_status_string = exit_type
                        .map(flow_common_exit_status::to_string)
                        .unwrap_or("Invalid_exit_code");
                    flow_hh_logger::error!(
                        "Tried to kill the server process ({}), which exited with the wrong exit code: {}",
                        pretty_pid,
                        exit_status_string
                    );
                }
                false
            }
            Some(WaitStatus::Signaled(signal)) => {
                flow_hh_logger::error!(
                    "Tried to kill the server process ({}), but for some reason it was killed with signal {}",
                    pretty_pid,
                    signal
                );
                false
            }
            Some(WaitStatus::Stopped(signal)) => {
                flow_hh_logger::error!(
                    "Tried to kill the server process ({}), but for some reason it was stopped with signal {}",
                    pretty_pid,
                    signal
                );
                true
            }
            None => {
                flow_hh_logger::error!(
                    "Tried to kill the server process ({}), but it didn't die",
                    pretty_pid
                );
                true
            }
        };
        if still_alive {
            #[cfg(unix)]
            {
                match nix::sys::signal::kill(
                    nix::unistd::Pid::from_raw(pid),
                    nix::sys::signal::Signal::SIGKILL,
                ) {
                    Ok(()) => {}
                    Err(nix::errno::Errno::ESRCH) => {
                        flow_hh_logger::info!("Server process ({}) no longer exists", pretty_pid);
                    }
                    Err(err) => {
                        flow_hh_logger::error!(
                            "Failed to send SIGKILL to server process ({}): {}",
                            pretty_pid,
                            err
                        );
                    }
                }
            }
            #[cfg(not(unix))]
            {
                flow_hh_logger::error!(
                    "Forced kill of server process ({}) is not supported on this platform",
                    pretty_pid
                );
            }
        }
    }

    pub fn cleanup(t: &mut ServerInstance) {
        // Drop the sender so every cloned receiver in the per-instance loops sees
        // Disconnected on its next recv and wakes up immediately.
        *t.shutdown_tx.lock().unwrap() = None;
        t.command_loop.take();
        t.file_watcher_loop.take();
        t.file_watcher_exit_thread.take();
        t.on_exit_thread.take();

        let watcher = t.watcher.clone();
        let connection = t.connection.clone();
        // Lwt.join will run these threads in parallel and only return when EVERY thread has returned
        // or failed
        crate::runtime::handle().block_on(async move {
            tokio::join!(
                async {
                    watcher.stop().await;
                },
                async {
                    connection.close_immediately();
                },
            );
        });
    }

    fn handle_file_watcher_exit(
        _error: Option<(String, String)>,
        msg: Option<&str>,
        code: FlowExitStatus,
        watcher_name: &str,
    ) -> ! {
        // TODO (glevi) - We probably don't need to make the monitor exit when the file watcher dies.
        // We could probably just restart it. For dfind, we'd also need to start a new server, but for
        // watchman we probably could just start a new watchman daemon and use the clockspec
        let msg = match msg {
            Some(m) => m.to_string(),
            None => format!("File watcher ({}) died", watcher_name),
        };
        super::exit(_error, &msg, code);
    }

    static SERVER_NUM: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

    // Spawn a brand new Flow server
    #[cfg(not(unix))]
    pub fn start(
        _monitor_options: &crate::flow_server_monitor_options::MonitorOptions,
        _restart_reason: Option<flow_server_env::server_status::RestartReason>,
    ) -> ServerInstance {
        panic!("Flow server monitor server spawn is not supported on this platform");
    }

    #[cfg(unix)]
    pub fn start(
        monitor_options: &crate::flow_server_monitor_options::MonitorOptions,
        restart_reason: Option<flow_server_env::server_status::RestartReason>,
    ) -> ServerInstance {
        flow_hh_logger::info!("Creating a new Flow server");
        let crate::flow_server_monitor_options::MonitorOptions {
            shared_mem_config: _shared_mem_config,
            server_options: _server_options,
            server_log_file: _log_file,
            lazy_mode,
            no_flowlib,
            ignore_version,
            argv: _argv,
            file_watcher,
            file_watcher_timeout: _file_watcher_timeout,
            file_watcher_mergebase_with: _mergebase_with,
            ..
        } = monitor_options;

        crate::status_stream::reset(file_watcher, restart_reason);

        use crate::file_watcher::Watcher as _;
        let any_watcher: crate::file_watcher::AnyWatcher = match file_watcher {
            crate::flow_server_monitor_options::FileWatcher::NoFileWatcher => {
                crate::file_watcher::AnyWatcher::Dummy(crate::file_watcher::Dummy::new())
            }
            crate::flow_server_monitor_options::FileWatcher::DFind => {
                crate::file_watcher::AnyWatcher::DFind(crate::file_watcher::DFind::new(
                    _mergebase_with.clone(),
                    _server_options.clone(),
                ))
            }
            crate::flow_server_monitor_options::FileWatcher::Watchman(watchman_options) => {
                crate::file_watcher::AnyWatcher::Watchman(
                    crate::file_watcher::watchman_file_watcher::Watchman::new(
                        _mergebase_with.clone(),
                        _server_options.clone(),
                        watchman_options.clone(),
                    ),
                )
            }
            crate::flow_server_monitor_options::FileWatcher::EdenFS(edenfs_options) => {
                // Try to initialize EdenFS watcher, fall back to Watchman on failure
                let edenfs_watcher = crate::file_watcher::edenfs_file_watcher::EdenFS::new(
                    _mergebase_with.clone(),
                    _server_options.clone(),
                    edenfs_options.clone(),
                );
                edenfs_watcher.start_init();
                // Wait for initialization, using the same file_watcher_timeout as Watchman.
                // This respects the file_watcher_timeout .flowconfig option (default 120s).
                let init_result = crate::runtime::handle()
                    .block_on(edenfs_watcher.wait_for_init(*_file_watcher_timeout));
                match init_result {
                    Ok(()) => {
                        flow_hh_logger::info!("EdenFS watcher initialized successfully");
                        crate::file_watcher::AnyWatcher::EdenFS(edenfs_watcher)
                    }
                    Err(msg) => {
                        flow_hh_logger::info!(
                            "EdenFS watcher init failed: {}. Falling back to Watchman.",
                            msg
                        );
                        let watchman_options = edenfs_options.edenfs_watchman_fallback.clone();
                        crate::file_watcher::AnyWatcher::Watchman(
                            crate::file_watcher::watchman_file_watcher::Watchman::new(
                                _mergebase_with.clone(),
                                _server_options.clone(),
                                watchman_options,
                            ),
                        )
                    }
                }
            }
        };
        let already_initialized = matches!(
            (&any_watcher, file_watcher),
            (
                crate::file_watcher::AnyWatcher::EdenFS(_),
                crate::flow_server_monitor_options::FileWatcher::EdenFS(_),
            )
        );

        let watcher_name = any_watcher.name().to_string();
        flow_hh_logger::info!("File watcher type: {}", watcher_name);

        // For watchers that haven't been initialized yet (non-EdenFS or EdenFS fallback), initialize now
        if !already_initialized {
            flow_hh_logger::debug!("Initializing file watcher ({})", watcher_name);
            any_watcher.start_init();
        }

        let file_watcher_pid = any_watcher.getpid();
        if let Some(pid) = file_watcher_pid {
            flow_hh_logger::info!("Spawned file watcher (pid={})", pid);
        }

        let init_id = format!(
            "{:016x}",
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_nanos()
                & 0xFFFFFFFFFFFFFFFFu128
        );
        let server_options_arc = std::sync::Arc::new(_server_options.clone());

        let server_handle = flow_server::server::daemonize(
            &init_id,
            _log_file,
            _argv,
            lazy_mode.clone(),
            *no_flowlib,
            *ignore_version,
            file_watcher_pid.map(|p| p as u32),
            server_options_arc,
        )
        .unwrap_or_else(|e| panic!("failed to spawn server daemon: {}", e));
        let pid: i32 = server_handle.child.id() as i32;
        // Cross-platform: `TcpStream::try_clone` duplicates the socket on
        // both Unix and Windows. The previous code used
        // `nix::unistd::dup(BorrowedFd)`, which is Unix-only.
        let in_stream = flow_daemon::descr_of_in_channel(&server_handle.channels.0)
            .try_clone()
            .expect("failed to dup server->monitor channel");
        let out_stream = flow_daemon::descr_of_out_channel(&server_handle.channels.1)
            .try_clone()
            .expect("failed to dup monitor->server channel");
        let daemon_handle = Arc::new(Mutex::new(Some(server_handle)));
        let close_daemon_handle = daemon_handle.clone();
        let close = move || {
            let mut guard = match close_daemon_handle.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            if let Some(handle) = guard.as_mut() {
                flow_daemon::close_noerr(handle);
            }
        };

        let server_num = SERVER_NUM.fetch_add(1, Ordering::SeqCst) + 1;
        let name = format!("server #{}", server_num);

        let (start_fn, connection) =
            ServerConnection::create(name.clone(), in_stream, out_stream, close, handle_response);
        start_fn();

        flow_hh_logger::info!("Spawned {} (pid={})", name, pid);

        // Close the connection to the server when we're about to exit
        let on_exit_connection = connection.clone();
        let on_exit_thread = Some(std::thread::spawn(move || {
            let (exit_status, exit_msg) = crate::exit_signal::SIGNAL.wait();
            cleanup_on_exit(exit_status, &exit_msg, &on_exit_connection, pid);
        }));

        if !already_initialized {
            // This may block for quite awhile. No messages will be sent to the server process until the
            // file watcher is up and running
            let init_result = crate::runtime::handle()
                .block_on(any_watcher.wait_for_init(*_file_watcher_timeout));
            match init_result {
                Ok(()) => {}
                Err(msg) => {
                    flow_hh_logger::error!("{}", msg);
                    handle_file_watcher_exit(
                        None,
                        Some(&msg),
                        FlowExitStatus::DfindDied,
                        &watcher_name,
                    );
                }
            }
        }

        flow_hh_logger::debug!("File watcher ({}) ready!", watcher_name);

        let any_watcher_arc = std::sync::Arc::new(any_watcher);

        let (shutdown_tx, shutdown_rx) = crossbeam::channel::unbounded::<()>();

        let watcher_name_for_exit = watcher_name.clone();
        let watcher_for_exit = any_watcher_arc.clone();
        let file_watcher_exit_thread: Option<JoinHandle<()>> =
            Some(std::thread::spawn(move || {
                let waitpid_fut = watcher_for_exit.waitpid_owned();
                let exit_reason = crate::runtime::handle().block_on(waitpid_fut);
                match exit_reason {
                    // file watcher was shut down intentionally, i.e. watcher#stop
                    crate::file_watcher::ExitReason::WatcherStopped => {}
                    crate::file_watcher::ExitReason::WatcherDied => {
                        handle_file_watcher_exit(
                            None,
                            None,
                            FlowExitStatus::DfindDied,
                            &watcher_name_for_exit,
                        );
                    }
                    crate::file_watcher::ExitReason::WatcherMissedChanges => {
                        let msg =
                            format!("File watcher ({}) missed changes", watcher_name_for_exit);
                        handle_file_watcher_exit(
                            None,
                            Some(&msg),
                            FlowExitStatus::FileWatcherMissedChanges,
                            &watcher_name_for_exit,
                        );
                    }
                }
            }));

        crate::status_stream::file_watcher_ready();

        let command_loop_connection = connection.clone();
        let command_loop_watcher = any_watcher_arc.clone();
        let command_loop_shutdown_rx = shutdown_rx.clone();
        let command_loop_exit_rx = super::exit_signal_receiver();
        let command_loop = Some(std::thread::spawn(move || {
            while let CommandLoopOutcome::Continue = command_loop_main(
                &command_loop_watcher,
                &command_loop_connection,
                &command_loop_shutdown_rx,
                &command_loop_exit_rx,
            ) {}
        }));

        let file_watcher_loop: Option<JoinHandle<()>> = if matches!(
            file_watcher,
            crate::flow_server_monitor_options::FileWatcher::NoFileWatcher
        ) {
            // Don't even bother
            None
        } else {
            let watcher_for_loop = any_watcher_arc.clone();
            let file_watcher_loop_shutdown_rx = shutdown_rx.clone();
            let file_watcher_loop_exit_rx = super::exit_signal_receiver();
            Some(std::thread::spawn(move || {
                // Race wait_for_changed_files (async, cancellable) against the two
                // crossbeam shutdown channels (sync). We bridge the async side into
                // a one-shot crossbeam channel and select! over all three. Aborting
                // the spawned tokio task on shutdown is what makes wait_for_changed_files
                // cancel cleanly; using spawn_blocking on the crossbeam recvs would leak
                // blocking-pool slots on every iteration since spawn_blocking can't be
                // aborted.
                loop {
                    let (changes_tx, changes_rx) = crossbeam::channel::bounded::<()>(1);
                    let watcher_for_iter = watcher_for_loop.clone();
                    let changes_handle = crate::runtime::handle().spawn(async move {
                        watcher_for_iter.wait_for_changed_files().await;
                        // Receiver may already be dropped if select! resolved on a
                        // shutdown branch; that's the expected race.
                        if changes_tx.send(()).is_err() {}
                    });
                    let outcome = crossbeam::channel::select! {
                        recv(changes_rx) -> _ => FileWatcherLoopOutcome::Continue,
                        recv(file_watcher_loop_shutdown_rx) -> _ => FileWatcherLoopOutcome::Stop,
                        recv(file_watcher_loop_exit_rx) -> _ => FileWatcherLoopOutcome::Stop,
                    };
                    // Always abort the spawned wait task so a Stop outcome doesn't
                    // leave it blocked inside the watcher forever.
                    changes_handle.abort();
                    match outcome {
                        FileWatcherLoopOutcome::Continue => {
                            super::push_to_command_stream(super::Command::NotifyFileChanges);
                        }
                        FileWatcherLoopOutcome::Stop => break,
                    }
                }
            }))
        };

        // Check for changed files, which processes any files that have changed since the mergebase
        // before we started up.
        super::push_to_command_stream(super::Command::NotifyFileChanges);

        ServerInstance {
            pid,
            watcher: any_watcher_arc,
            connection,
            command_loop,
            file_watcher_loop,
            on_exit_thread,
            file_watcher_exit_thread,
            shutdown_tx: std::sync::Mutex::new(Some(shutdown_tx)),
            daemon_handle,
        }
    }

    pub fn pid_of(t: &ServerInstance) -> i32 {
        t.pid
    }

    pub(super) enum WaitStatus {
        Exited(i32),
        Signaled(i32),
        Stopped(i32),
    }

    #[cfg(not(unix))]
    fn wait_for_pid_with_timeout(_pid: i32, _timeout: std::time::Duration) -> Option<WaitStatus> {
        panic!("wait_for_pid_with_timeout is not supported on this platform")
    }

    #[cfg(unix)]
    fn wait_for_pid_with_timeout(pid: i32, timeout: std::time::Duration) -> Option<WaitStatus> {
        use std::sync::mpsc;
        let (tx, rx) = mpsc::channel();
        let _handle = std::thread::spawn(move || {
            use nix::sys::wait;
            match wait::waitpid(nix::unistd::Pid::from_raw(pid), None) {
                Ok(wait::WaitStatus::Exited(_, code)) => {
                    if let Err(e) = tx.send(Some(WaitStatus::Exited(code))) {
                        flow_hh_logger::warn!("Failed to send wait status: {}", e);
                    }
                }
                Ok(wait::WaitStatus::Signaled(_, sig, _)) => {
                    if let Err(e) = tx.send(Some(WaitStatus::Signaled(sig as i32))) {
                        flow_hh_logger::warn!("Failed to send wait status: {}", e);
                    }
                }
                Ok(wait::WaitStatus::Stopped(_, sig)) => {
                    if let Err(e) = tx.send(Some(WaitStatus::Stopped(sig as i32))) {
                        flow_hh_logger::warn!("Failed to send wait status: {}", e);
                    }
                }
                Ok(_) => {
                    if let Err(e) = tx.send(None) {
                        flow_hh_logger::warn!("Failed to send wait status: {}", e);
                    }
                }
                Err(nix::errno::Errno::ECHILD) => {
                    flow_hh_logger::info!("Server process has already exited. No need to kill it");
                    if let Err(e) = tx.send(None) {
                        flow_hh_logger::warn!("Failed to send wait status: {}", e);
                    }
                }
                Err(_) => {
                    if let Err(e) = tx.send(None) {
                        flow_hh_logger::warn!("Failed to send wait status: {}", e);
                    }
                }
            }
        });
        rx.recv_timeout(timeout).unwrap_or_default()
    }

    pub(crate) fn error_type_of_code(code: i32) -> Option<FlowExitStatus> {
        use FlowExitStatus::*;
        match code {
            -6 => Some(Interrupted),
            0 => Some(NoError),
            1 => Some(WindowsKilledByTaskManager),
            2 => Some(TypeError),
            3 => Some(OutOfTime),
            4 => Some(KillError),
            5 => Some(UnusedServer),
            6 => Some(NoServerRunning),
            7 => Some(OutOfRetries),
            8 => Some(InvalidFlowconfig),
            9 => Some(BuildIdMismatch),
            10 => Some(InputError),
            11 => Some(LockStolen),
            12 => Some(CouldNotFindFlowconfig),
            13 => Some(ServerOutOfDate),
            15 => Some(OutOfSharedMemory),
            16 => Some(FlowconfigChanged),
            17 => Some(PathIsNotAFile),
            18 => Some(Autostop),
            19 => Some(KilledByMonitor),
            20 => Some(InvalidSavedState),
            21 => Some(Restart),
            22 => Some(CouldNotExtractFlowlibs),
            64 => Some(CommandlineUsageError),
            66 => Some(NoInput),
            78 => Some(ServerStartFailed),
            97 => Some(MissingFlowlib),
            98 => Some(SocketError),
            99 => Some(DfindDied),
            101 => Some(WatchmanError),
            102 => Some(HashTableFull),
            103 => Some(HeapFull),
            104 => Some(WatchmanFailed),
            105 => Some(FileWatcherMissedChanges),
            108 => Some(EventLoggerRestartOutOfRetries),
            110 => Some(UnknownError),
            111 => Some(EdenfsWatcherFailed),
            112 => Some(EdenfsWatcherLostChanges),
            _ => None,
        }
    }
}

// Monitor state that persists across server restarts
pub struct MonitorState {
    pub options: crate::flow_server_monitor_options::MonitorOptions,
    pub edenfs_watcher_retries: i32,
}

pub const MAX_EDENFS_WATCHER_RETRIES: i32 = 3;

// A loop who's job is to start a server and then wait for it to die
mod keep_alive_loop {
    use super::MAX_EDENFS_WATCHER_RETRIES;
    use super::MonitorState;
    use super::server_instance;

    // Given that a Flow server has just exited with this exit status, should the monitor exit too?
    //
    // Returns the tuple (should_monitor_exit_with_server, restart_reason, is_edenfs_watcher_failure)
    //
    // Note: For EdenFS watcher failures (Edenfs_watcher_failed, Edenfs_watcher_lost_changes),
    // this function returns (false, None, true) to allow the caller to implement retry logic.
    // The caller is responsible for checking retry counts and deciding whether to actually
    // restart or exit.
    pub(super) fn process_server_exit(
        monitor_options: &crate::flow_server_monitor_options::MonitorOptions,
        exit_status: flow_common_exit_status::FlowExitStatus,
    ) -> (
        bool,
        Option<flow_server_env::server_status::RestartReason>,
        bool,
    ) {
        if monitor_options.no_restart {
            return (true, None, false);
        }
        use flow_common_exit_status::FlowExitStatus;
        match exit_status {
            //*** Things the server might exit with that implies that the monitor should exit too ***
            FlowExitStatus::NoError
            // Server exited cleanly
            | FlowExitStatus::WindowsKilledByTaskManager
            // Windows task manager killed the server
            | FlowExitStatus::InvalidFlowconfig
            // Parse/version/etc error. Server will never start correctly.
            | FlowExitStatus::PathIsNotAFile
            // Required a file but privided path was not a file
            | FlowExitStatus::FlowconfigChanged
            // We could survive some config changes, but it's too hard to tell
            | FlowExitStatus::InvalidSavedState
            // The saved state file won't automatically recover by restarting
            | FlowExitStatus::UnusedServer
            // The server appears unused for long enough that it decided to just die
            | FlowExitStatus::UnknownError
            // Uncaught exn. We probably could survive this, but it's a little risky
            | FlowExitStatus::WatchmanError
            // We ran into an issue with Watchman
            | FlowExitStatus::WatchmanFailed
            // We ran into an issue with Watchman
            | FlowExitStatus::FileWatcherMissedChanges
            // Watchman restarted. We probably could survive this by recrawling
            | FlowExitStatus::OutOfSharedMemory
            // It's possible that restarting would GC enough to run for a while, but this
            // is a serious problem that should be investigated so as to not end up in a
            // crash loop.
            | FlowExitStatus::HashTableFull
            // The hash table is full. It accumulates cruft, so restarting _might_ help, but
            // if it's just too small, we could get stuck in a crash loop. Ideally we'd delete
            // unused keys so that it being full is definitely a permanent failure.
            | FlowExitStatus::HeapFull
            // The heap is full. Restarting might help clear out cruft, but it could also just
            // be too small, leading to a crash loop. We should limit how often we try restarting
            // before recovering from this.
            | FlowExitStatus::CouldNotExtractFlowlibs
            //*** Things that the server shouldn't use, but would imply that the monitor should exit ***
            | FlowExitStatus::Interrupted
            | FlowExitStatus::BuildIdMismatch
            // Client build differs from server build - only monitor uses this
            | FlowExitStatus::LockStolen
            // Lock lost - only monitor should use this
            | FlowExitStatus::SocketError
            // Failed to set up socket - only monitor should use this
            | FlowExitStatus::DfindDied // Any file watcher died (it's misnamed) - only monitor should use this
            => (true, None, false),
            //*** EdenFS watcher failures - allow retry logic to handle these ***
            FlowExitStatus::EdenfsWatcherFailed | FlowExitStatus::EdenfsWatcherLostChanges => {
                (false, None, true)
            }
            //*** Things the server might exit with which the monitor can survive ***
            FlowExitStatus::ServerOutOfDate /* Server needs to restart, but monitor can survive */ => (
                false,
                Some(flow_server_env::server_status::RestartReason::ServerOutOfDate),
                false,
            ),
            FlowExitStatus::KilledByMonitor /* The server died because we asked it to die */ => (false, None, false),
            FlowExitStatus::Restart /* The server asked to be restarted */ => (
                false,
                Some(flow_server_env::server_status::RestartReason::Restart),
                false,
            ),
            //*** Unrelated exit codes. If we see them then something is wrong ***
            FlowExitStatus::TypeError
            | FlowExitStatus::OutOfTime
            | FlowExitStatus::KillError
            | FlowExitStatus::NoServerRunning
            | FlowExitStatus::OutOfRetries
            | FlowExitStatus::EventLoggerRestartOutOfRetries
            | FlowExitStatus::InputError
            | FlowExitStatus::CouldNotFindFlowconfig
            | FlowExitStatus::CommandlineUsageError
            | FlowExitStatus::NoInput
            | FlowExitStatus::MissingFlowlib
            | FlowExitStatus::ServerStartFailed
            | FlowExitStatus::Autostop /* is used by monitor to exit, not server */ => (true, None, false),
        }
    }

    // Ephemeral commands are stateless, so they can survive a server restart. However a persistent
    // connection might have state, so it's wrong to allow it to survive. Maybe in the future we can
    // tell the persistent connection that the server has died and let it adjust its state, but for
    // now lets close all persistent connections
    pub(super) fn killall_persistent_connections(
        exit_type: flow_common_exit_status::FlowExitStatus,
    ) {
        let clients = crate::persistent_connection_map::get_all_clients();
        for conn in clients {
            let msg = flow_server_env::lsp_prot::MessageFromServer::NotificationFromServer(
                flow_server_env::lsp_prot::NotificationFromServer::ServerExit(exit_type),
            );
            // it's ok if the stream is already closed, we must be shutting down already
            conn.write(msg);
            // it's also ok if the flush fails because the socket is already closed
            conn.try_flush_and_close();
        }
    }

    pub(super) fn should_monitor_exit_with_signaled_server(signal: i32) -> bool {
        // While there are many scary things which can cause segfaults, in practice we've mostly seen
        // them when the Flow server hits some infinite or very deep recursion (like Base.List.map ~f:on a
        // very large list). Often, this is triggered by some ephemeral command, which is rerun when
        // the server starts back up, leading to a cycle of segfaulting servers.
        //
        // The easiest solution is for the monitor to exit as well when the server segfaults. This
        // will cause the bad command to consume retries and eventually exit. This doesn't prevent
        // future bad commands, but is better than the alternative.
        #[cfg(unix)]
        {
            signal == libc::SIGSEGV || signal == libc::SIGBUS
        }
        #[cfg(not(unix))]
        {
            signal == libc::SIGSEGV
        }
    }

    #[cfg(not(unix))]
    pub(super) fn wait_for_server_to_die(
        _monitor_state: MonitorState,
        _server: &mut server_instance::ServerInstance,
    ) -> (
        MonitorState,
        Option<flow_server_env::server_status::RestartReason>,
    ) {
        panic!("wait_for_server_to_die is not supported on this platform")
    }

    #[cfg(unix)]
    pub(super) fn wait_for_server_to_die(
        monitor_state: MonitorState,
        server: &mut server_instance::ServerInstance,
    ) -> (
        MonitorState,
        Option<flow_server_env::server_status::RestartReason>,
    ) {
        let pid = server_instance::pid_of(server);
        let daemon_handle = {
            let mut guard = match server.daemon_handle.lock() {
                Ok(guard) => guard,
                Err(poisoned) => poisoned.into_inner(),
            };
            guard.take()
        };
        let wait_status = match daemon_handle {
            Some(mut handle) => match handle.child.wait() {
                Ok(status) => {
                    use std::os::unix::process::ExitStatusExt;
                    match status.code() {
                        Some(exit_code) => server_instance::WaitStatus::Exited(exit_code),
                        None => {
                            if let Some(signal) = status.signal() {
                                server_instance::WaitStatus::Signaled(signal)
                            } else if let Some(signal) = status.stopped_signal() {
                                server_instance::WaitStatus::Stopped(signal)
                            } else {
                                flow_hh_logger::error!(
                                    "wait_for_server_to_die: unknown wait status for pid {}",
                                    pid
                                );
                                return (monitor_state, None);
                            }
                        }
                    }
                }
                Err(e) => {
                    flow_hh_logger::error!(
                        "wait_for_server_to_die: failed to wait on server pid {}: {}",
                        pid,
                        e
                    );
                    return (monitor_state, None);
                }
            },
            None => {
                flow_hh_logger::error!(
                    "wait_for_server_to_die: daemon_handle was None for pid {}",
                    pid
                );
                return (monitor_state, None);
            }
        };
        server_instance::cleanup(server);

        match wait_status {
            server_instance::WaitStatus::Exited(exit_code) => {
                let exit_type = server_instance::error_type_of_code(exit_code);
                let exit_status_string = exit_type
                    .map(flow_common_exit_status::to_string)
                    .unwrap_or("Invalid_exit_code");
                flow_hh_logger::error!(
                    "Flow server (pid {}) exited with code {} ({})",
                    pid,
                    exit_status_string,
                    exit_code
                );
                match exit_type {
                    None => {
                        super::exit(
                            None,
                            &format!("Flow server exited with invalid exit code ({})", exit_code),
                            flow_common_exit_status::FlowExitStatus::UnknownError,
                        );
                    }
                    Some(exit_type) => {
                        let (
                            should_monitor_exit_with_server,
                            restart_reason,
                            is_edenfs_watcher_failure,
                        ) = process_server_exit(&monitor_state.options, exit_type);
                        if is_edenfs_watcher_failure {
                            // EdenFS watcher failed - check retry count
                            if monitor_state.edenfs_watcher_retries < MAX_EDENFS_WATCHER_RETRIES {
                                flow_hh_logger::info!(
                                    "EdenFS watcher died. Restarting Flow server (attempt: {})",
                                    monitor_state.edenfs_watcher_retries + 1
                                );
                                killall_persistent_connections(exit_type);
                                let new_state = MonitorState {
                                    edenfs_watcher_retries: monitor_state.edenfs_watcher_retries
                                        + 1,
                                    ..monitor_state
                                };
                                (new_state, None)
                            } else {
                                flow_hh_logger::error!(
                                    "EdenFS watcher died {} times. Giving up.",
                                    MAX_EDENFS_WATCHER_RETRIES
                                );
                                super::exit(
                                    None,
                                    "EdenFS watcher failed too many times",
                                    exit_type,
                                );
                            }
                        } else if should_monitor_exit_with_server {
                            super::exit(None, "Dying along with server", exit_type);
                        } else {
                            killall_persistent_connections(exit_type);
                            (monitor_state, restart_reason)
                        }
                    }
                }
            }
            server_instance::WaitStatus::Signaled(signal) => {
                flow_hh_logger::error!(
                    "Flow server (pid {}) was killed with signal {}",
                    pid,
                    signal
                );
                if should_monitor_exit_with_signaled_server(signal) {
                    super::exit(
                        None,
                        "Dying along with signaled server",
                        flow_common_exit_status::FlowExitStatus::Interrupted,
                    );
                } else {
                    (monitor_state, None)
                }
            }
            server_instance::WaitStatus::Stopped(signal) => {
                // If a Flow server has been stopped but hasn't exited then what should we do? I suppose we
                // could try to signal it to resume. Or we could wait for it to start up again. But killing
                // it and starting a new server seems easier
                flow_hh_logger::error!(
                    "Flow server (pid {}) was stopped with signal {}. Sending sigkill",
                    pid,
                    signal
                );

                // kill is not a blocking system call, which is likely why it is missing from Lwt_unix
                if let Err(e) = nix::sys::signal::kill(
                    nix::unistd::Pid::from_raw(pid),
                    nix::sys::signal::Signal::SIGKILL,
                ) {
                    flow_hh_logger::warn!(
                        "Failed to send SIGKILL to server process ({}): {}",
                        pid,
                        e
                    );
                }
                (monitor_state, None)
            }
        }
    }

    // The RequestMap will contain all the requests which have been sent to the server but never
    // received a response. If we're starting up a new server, we can resend all these requests to
    // the new server
    pub(super) fn requeue_stalled_requests() {
        let requests = crate::request_map::remove_all();
        for (request, client) in requests {
            super::push_to_command_stream(super::Command::WriteEphemeralRequest {
                request,
                client,
            });
        }
    }

    pub(super) fn keep_alive_loop_main(
        monitor_state: MonitorState,
        restart_reason: Option<flow_server_env::server_status::RestartReason>,
    ) -> (
        MonitorState,
        Option<flow_server_env::server_status::RestartReason>,
    ) {
        requeue_stalled_requests();
        let mut server = server_instance::start(&monitor_state.options, restart_reason);
        let (new_state, restart_reason) = wait_for_server_to_die(monitor_state, &mut server);
        (new_state, restart_reason)
    }
}

#[cfg(unix)]
fn setup_signal_handlers() {
    let signals = [
        libc::SIGINT,  /* Interrupt - ctrl-c */
        libc::SIGTERM, /* Termination - like a nicer sigkill giving you a chance to cleanup */
        libc::SIGHUP,  /* Hang up - the terminal went away */
        libc::SIGQUIT, /* Dump core - Kind of a meaner sigterm */
    ];

    let signals_vec: Vec<i32> = signals.to_vec();
    std::thread::spawn(move || {
        let mut sigs = match signal_hook::iterator::Signals::new(&signals_vec) {
            Ok(s) => s,
            Err(err) => {
                flow_hh_logger::error!("Failed to create signal iterator: {}", err);
                return;
            }
        };
        if let Some(sig) = sigs.forever().next() {
            // Set EXITING first (matching `exit()`), then broadcast so any reader
            // observing the broadcast can also observe EXITING == true.
            EXITING.store(true, Ordering::SeqCst);
            crate::exit_signal::SIGNAL.broadcast(
                flow_common_exit_status::FlowExitStatus::Interrupted,
                format!("Received signal {}", sig),
            );
            signal_exit_to_loops();
        }
    });
}

#[cfg(not(unix))]
fn setup_signal_handlers() {
    // Signal-based interruption is not supported on this platform; the monitor will
    // exit only via explicit shutdown commands.
}

pub fn start(monitor_options: crate::flow_server_monitor_options::MonitorOptions) {
    std::thread::spawn(doomsday::start_clock);
    setup_signal_handlers();
    let initial_state = MonitorState {
        options: monitor_options,
        edenfs_watcher_retries: 0,
    };
    let mut state = initial_state;
    let mut restart_reason = None;
    loop {
        if EXITING.load(Ordering::SeqCst) {
            break;
        }
        let (new_state, new_restart_reason) =
            keep_alive_loop::keep_alive_loop_main(state, restart_reason);
        state = new_state;
        restart_reason = new_restart_reason;
    }
}

pub fn send_request(
    client: std::sync::Arc<crate::flow_server_monitor_connection::EphemeralConnection>,
    request: flow_server_env::server_command_with_context::ServerCommandWithContext,
) {
    flow_hh_logger::debug!("Adding request to the command stream");
    push_to_command_stream(Command::WriteEphemeralRequest { request, client });
}

pub fn send_persistent_request(
    client_id: flow_server_env::lsp_prot::ClientId,
    request: flow_server_env::lsp_prot::RequestWithMetadata,
) {
    flow_hh_logger::debug!("Adding request to the command stream");
    push_to_command_stream(Command::WritePersistentRequest { client_id, request });
}

pub fn notify_new_persistent_connection(
    client_id: flow_server_env::lsp_prot::ClientId,
    lsp_init_params: lsp_types::InitializeParams,
) {
    flow_hh_logger::debug!(
        "Adding notification that there's a new persistent client #{}",
        client_id
    );
    push_to_command_stream(Command::NotifyNewPersistentConnection {
        client_id,
        lsp_init_params,
    });
}

pub fn notify_dead_persistent_connection(client_id: flow_server_env::lsp_prot::ClientId) {
    flow_hh_logger::debug!(
        "Adding notification that persistent client #{} died",
        client_id
    );
    push_to_command_stream(Command::NotifyDeadPersistentConnection { client_id });
}
