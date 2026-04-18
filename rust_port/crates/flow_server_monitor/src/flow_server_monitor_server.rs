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
// response will be written again to the new server.
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
// before the monitor exits.
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;
static EXITING: AtomicBool = AtomicBool::new(false);

pub fn exit(
    _error: Option<(String, String)>,
    msg: &str,
    exit_status: flow_common_exit_status::FlowExitStatus,
) -> ! {
    if EXITING.swap(true, Ordering::SeqCst) {
        loop {
            std::thread::park();
        }
    }
    log::info!("Monitor is exiting code {:?} ({})", exit_status, msg);

    log::info!("Broadcasting to threads and waiting 1 second for them to exit");
    crate::exit_signal::SIGNAL.broadcast(exit_status, msg.to_string());

    std::thread::sleep(std::time::Duration::from_secs(1));
    flow_common_exit_status::exit(exit_status);
}

pub enum StopReason {
    Stopped,
    Autostopped,
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

// Exit after 7 days of no requests.
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

// The long-lived stream of requests in the monitor that have arrived from client.
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
        log::warn!("Failed to push to command stream: {}", e);
    }
}

// ServerInstance.t is an individual Flow server instance. The code inside this module handles
// interacting with a Flow server instance.
pub mod server_instance {
    use std::sync::Arc;
    use std::sync::atomic::AtomicBool;
    use std::sync::atomic::Ordering;
    use std::thread::JoinHandle;

    use flow_common_exit_status::FlowExitStatus;
    use flow_server_env::lsp_prot;
    use flow_server_env::monitor_prot;

    use crate::flow_server_monitor_connection::ServerConnection;

    pub struct ServerInstance {
        pub pid: i32,
        pub watcher: Arc<tokio::sync::Mutex<crate::file_watcher::AnyWatcher>>,
        pub connection: Arc<ServerConnection>,
        pub command_loop: Option<JoinHandle<()>>,
        pub file_watcher_loop: Option<JoinHandle<()>>,
        pub on_exit_thread: Option<JoinHandle<()>>,
        pub file_watcher_exit_thread: Option<JoinHandle<()>>,
        pub shutdown: Arc<AtomicBool>,
        pub server_thread: Option<JoinHandle<()>>,
    }

    fn handle_response(
        msg: monitor_prot::ServerToMonitorMessage,
        _connection: &Arc<ServerConnection>,
    ) {
        match msg {
            monitor_prot::ServerToMonitorMessage::Response(request_id, response) => {
                log::debug!(
                    "Read a response to request '{}' from the server!",
                    request_id
                );
                let request = crate::request_map::remove(&request_id);
                match request {
                    None => {
                        log::error!("Failed to look up request '{}'", request_id);
                    }
                    Some((_req, client)) => {
                        let msg = monitor_prot::MonitorToClientMessage::Data(response);
                        if !client.write_and_close(msg) {
                            log::debug!(
                                "Client for request '{}' is dead. Throwing away response",
                                request_id
                            );
                        }
                    }
                }
            }
            monitor_prot::ServerToMonitorMessage::RequestFailed(request_id, exn_str) => {
                log::error!(
                    "Server threw exception when processing '{}': {}",
                    request_id,
                    exn_str
                );
                let request = crate::request_map::remove(&request_id);
                match request {
                    None => {
                        log::error!("Failed to look up request '{}'", request_id);
                    }
                    Some((_req, client)) => {
                        let msg = monitor_prot::MonitorToClientMessage::ServerException(exn_str);
                        if !client.write_and_close(msg) {
                            log::debug!(
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
                    log::error!("Failed to look up persistent client #{}", client_id);
                }
                Some(connection) => {
                    if !connection.write(response) {
                        log::debug!(
                            "Persistent client #{} is dead. Throwing away response",
                            client_id
                        );
                    }
                }
            },
        }
    }

    fn send_request(msg: monitor_prot::MonitorToServerMessage, conn: &ServerConnection) {
        if !conn.write(msg) {
            log::debug!("Server connection is closed. Throwing away request");
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

    fn send_file_watcher_notification(
        watcher: &Arc<tokio::sync::Mutex<crate::file_watcher::AnyWatcher>>,
        conn: &ServerConnection,
    ) {
        let watcher = watcher.clone();
        let (files, metadata, initial, debug) = crate::runtime::handle().block_on(async move {
            let mut guard = watcher.lock().await;
            let (files, metadata, initial) = guard.get_and_clear_changed_files().await;
            let debug = guard.debug();
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
            log::info!(
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
            log::debug!("Ignoring irrelevant file watcher notification");
        }
    }

    fn command_loop_main(
        watcher: &Arc<tokio::sync::Mutex<crate::file_watcher::AnyWatcher>>,
        conn: &ServerConnection,
    ) {
        let receiver = super::COMMAND_STREAM.1.lock().unwrap();
        let command = match receiver.recv_timeout(std::time::Duration::from_millis(100)) {
            Ok(cmd) => cmd,
            Err(crossbeam::channel::RecvTimeoutError::Timeout) => return,
            Err(crossbeam::channel::RecvTimeoutError::Disconnected) => return,
        };
        drop(receiver);
        match command {
            super::Command::WriteEphemeralRequest { request, client } => {
                super::doomsday::postpone();
                if !client.is_closed() {
                    send_file_watcher_notification(watcher, conn);
                    let request_id = crate::request_map::add(request.clone(), client);
                    log::debug!("Writing '{}' to the server connection", request_id);
                    send_request(
                        monitor_prot::MonitorToServerMessage::Request(request_id, request),
                        conn,
                    );
                } else {
                    log::debug!("Skipping request from a dead ephemeral connection");
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
    }

    fn cleanup_on_exit(
        exit_status: FlowExitStatus,
        exit_msg: &str,
        connection: &ServerConnection,
        pid: i32,
    ) {
        let msg = monitor_prot::MonitorToServerMessage::PleaseDie(
            monitor_prot::PleaseDieReason::MonitorExiting(exit_status, exit_msg.to_string()),
        );
        if !connection.write(msg) {}
        let server_status = wait_for_pid_with_timeout(pid, std::time::Duration::from_millis(750));

        connection.close_immediately();
        let pretty_pid = pid;
        let still_alive = match server_status {
            Some(WaitStatus::Exited(exit_code)) => {
                let exit_type = error_type_of_code(exit_code);
                if exit_type == Some(FlowExitStatus::KilledByMonitor) {
                    log::info!("Successfully killed the server process");
                } else {
                    let exit_status_string = exit_type
                        .map(flow_common_exit_status::to_string)
                        .unwrap_or("Invalid_exit_code");
                    log::error!(
                        "Tried to kill the server process ({}), which exited with the wrong exit code: {}",
                        pretty_pid,
                        exit_status_string
                    );
                }
                false
            }
            Some(WaitStatus::Signaled(signal)) => {
                log::error!(
                    "Tried to kill the server process ({}), but for some reason it was killed with signal {}",
                    pretty_pid,
                    signal
                );
                false
            }
            Some(WaitStatus::Stopped(signal)) => {
                log::error!(
                    "Tried to kill the server process ({}), but for some reason it was stopped with signal {}",
                    pretty_pid,
                    signal
                );
                true
            }
            None => {
                log::error!(
                    "Tried to kill the server process ({}), but it didn't die",
                    pretty_pid
                );
                true
            }
        };
        if still_alive {
            match nix::sys::signal::kill(
                nix::unistd::Pid::from_raw(pid),
                nix::sys::signal::Signal::SIGKILL,
            ) {
                Ok(()) => {}
                Err(nix::errno::Errno::ESRCH) => {
                    log::info!("Server process ({}) no longer exists", pretty_pid);
                }
                Err(err) => {
                    log::error!(
                        "Failed to send SIGKILL to server process ({}): {}",
                        pretty_pid,
                        err
                    );
                }
            }
        }
    }

    pub fn cleanup(t: &mut ServerInstance) {
        t.shutdown.store(true, Ordering::SeqCst);
        t.command_loop.take();
        t.file_watcher_loop.take();
        t.file_watcher_exit_thread.take();
        t.on_exit_thread.take();
        t.server_thread.take();

        let watcher = t.watcher.clone();
        let connection = t.connection.clone();
        crate::runtime::handle().block_on(async move {
            tokio::join!(
                async {
                    let mut guard = watcher.lock().await;
                    guard.stop().await;
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
        let msg = match msg {
            Some(m) => m.to_string(),
            None => format!("File watcher ({}) died", watcher_name),
        };
        super::exit(_error, &msg, code);
    }

    fn close_if_open(fd: std::os::unix::io::RawFd) {
        match nix::unistd::close(fd) {
            Ok(()) => {}
            Err(nix::errno::Errno::EBADF) => {} // already closed
            Err(err) => {
                log::error!("Error closing fd {}: {}", fd, err);
            }
        }
    }

    static SERVER_NUM: std::sync::atomic::AtomicI32 = std::sync::atomic::AtomicI32::new(0);

    pub fn start(
        monitor_options: &crate::flow_server_monitor_options::MonitorOptions,
        restart_reason: Option<flow_server_env::server_status::RestartReason>,
    ) -> ServerInstance {
        log::info!("Creating a new Flow server");
        let crate::flow_server_monitor_options::MonitorOptions {
            shared_mem_config: _shared_mem_config,
            server_options: _server_options,
            server_log_file: _log_file,
            argv: _argv,
            file_watcher,
            file_watcher_timeout: _file_watcher_timeout,
            file_watcher_mergebase_with: _mergebase_with,
            ..
        } = monitor_options;

        crate::status_stream::reset(file_watcher, restart_reason);

        use crate::file_watcher::Watcher as _;
        let mut any_watcher: crate::file_watcher::AnyWatcher = match file_watcher {
            crate::flow_server_monitor_options::FileWatcher::NoFileWatcher => {
                crate::file_watcher::AnyWatcher::Dummy(crate::file_watcher::Dummy::new())
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
                let mut edenfs_watcher = crate::file_watcher::edenfs_file_watcher::EdenFS::new(
                    _mergebase_with.clone(),
                    _server_options.clone(),
                    edenfs_options.clone(),
                );
                edenfs_watcher.start_init();
                let init_result = crate::runtime::handle()
                    .block_on(edenfs_watcher.wait_for_init(*_file_watcher_timeout));
                match init_result {
                    Ok(()) => {
                        log::info!("EdenFS watcher initialized successfully");
                        crate::file_watcher::AnyWatcher::EdenFS(edenfs_watcher)
                    }
                    Err(msg) => {
                        log::info!(
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
        log::info!("File watcher type: {}", watcher_name);

        if !already_initialized {
            log::debug!("Initializing file watcher ({})", watcher_name);
            any_watcher.start_init();
        }

        let file_watcher_pid = any_watcher.getpid();
        if let Some(pid) = file_watcher_pid {
            log::info!("Spawned file watcher (pid={})", pid);
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
        let ready_path = if monitor_options.signal_ready {
            Some(flow_server_files::server_files_js::ready_file(
                &_server_options.flowconfig_name,
                &_server_options.temp_dir,
                _server_options.root.as_path(),
            ))
        } else {
            None
        };

        let (server_in_fd, monitor_out_fd) = nix::sys::socket::socketpair(
            nix::sys::socket::AddressFamily::Unix,
            nix::sys::socket::SockType::Stream,
            None,
            nix::sys::socket::SockFlag::empty(),
        )
        .expect("failed to create monitor->server socketpair");
        let (monitor_in_fd, server_out_fd) = nix::sys::socket::socketpair(
            nix::sys::socket::AddressFamily::Unix,
            nix::sys::socket::SockType::Stream,
            None,
            nix::sys::socket::SockFlag::empty(),
        )
        .expect("failed to create server->monitor socketpair");
        let server_thread = flow_server::server::daemonize(
            &init_id,
            _log_file,
            _argv,
            file_watcher_pid.map(|p| p as u32),
            server_options_arc,
            ready_path,
            Some((server_in_fd, server_out_fd)),
        );
        let pid: i32 = std::process::id() as i32;
        let in_fd = monitor_in_fd;
        let out_fd = monitor_out_fd;

        use std::os::fd::AsRawFd;
        let in_raw_fd: std::os::unix::io::RawFd = in_fd.as_raw_fd();
        let out_raw_fd: std::os::unix::io::RawFd = out_fd.as_raw_fd();
        let close = move || {
            close_if_open(in_raw_fd);
            close_if_open(out_raw_fd);
        };

        let server_num = SERVER_NUM.fetch_add(1, Ordering::SeqCst) + 1;
        let name = format!("server #{}", server_num);

        let (start_fn, connection) =
            ServerConnection::create(name.clone(), in_fd, out_fd, close, |msg, connection| {
                handle_response(msg, connection)
            });
        start_fn();

        log::info!("Spawned {} (pid={})", name, pid);

        let on_exit_connection = connection.clone();
        let on_exit_thread = Some(std::thread::spawn(move || {
            let (exit_status, exit_msg) = crate::exit_signal::SIGNAL.wait();
            cleanup_on_exit(exit_status, &exit_msg, &on_exit_connection, pid);
        }));

        if !already_initialized {
            let init_result = crate::runtime::handle()
                .block_on(any_watcher.wait_for_init(*_file_watcher_timeout));
            match init_result {
                Ok(()) => {}
                Err(msg) => {
                    log::error!("{}", msg);
                    handle_file_watcher_exit(
                        None,
                        Some(&msg),
                        FlowExitStatus::DfindDied,
                        &watcher_name,
                    );
                }
            }
        }

        log::debug!("File watcher ({}) ready!", watcher_name);

        let any_watcher_arc = std::sync::Arc::new(tokio::sync::Mutex::new(any_watcher));

        let shutdown = Arc::new(AtomicBool::new(false));

        let watcher_name_for_exit = watcher_name.clone();
        let watcher_for_exit = any_watcher_arc.clone();
        let file_watcher_exit_thread: Option<JoinHandle<()>> =
            Some(std::thread::spawn(move || {
                let waitpid_fut = {
                    let guard = watcher_for_exit.blocking_lock();
                    guard.waitpid_owned()
                };
                let exit_reason = crate::runtime::handle().block_on(waitpid_fut);
                match exit_reason {
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
        let command_loop_shutdown = shutdown.clone();
        let command_loop = Some(std::thread::spawn(move || {
            loop {
                if super::EXITING.load(Ordering::SeqCst) {
                    break;
                }
                if command_loop_shutdown.load(Ordering::SeqCst) {
                    break;
                }
                command_loop_main(&command_loop_watcher, &command_loop_connection);
            }
        }));

        let file_watcher_loop: Option<JoinHandle<()>> = if matches!(
            file_watcher,
            crate::flow_server_monitor_options::FileWatcher::NoFileWatcher
        ) {
            None
        } else {
            let watcher_for_loop = any_watcher_arc.clone();
            let file_watcher_loop_shutdown = shutdown.clone();
            Some(std::thread::spawn(move || {
                while !super::EXITING.load(Ordering::SeqCst)
                    && !file_watcher_loop_shutdown.load(Ordering::SeqCst)
                {
                    let watcher_for_iter = watcher_for_loop.clone();
                    crate::runtime::handle().block_on(async move {
                        let mut guard = watcher_for_iter.lock().await;
                        guard.wait_for_changed_files().await;
                    });
                    super::push_to_command_stream(super::Command::NotifyFileChanges);
                }
            }))
        };

        super::push_to_command_stream(super::Command::NotifyFileChanges);

        ServerInstance {
            pid,
            watcher: any_watcher_arc,
            connection,
            command_loop,
            file_watcher_loop,
            on_exit_thread,
            file_watcher_exit_thread,
            shutdown,
            server_thread: Some(server_thread),
        }
    }

    pub fn pid_of(t: &ServerInstance) -> i32 {
        t.pid
    }

    enum WaitStatus {
        Exited(i32),
        Signaled(i32),
        Stopped(i32),
    }

    fn wait_for_pid_with_timeout(pid: i32, timeout: std::time::Duration) -> Option<WaitStatus> {
        use std::sync::mpsc;
        let (tx, rx) = mpsc::channel();
        let _handle = std::thread::spawn(move || {
            use nix::sys::wait;
            match wait::waitpid(nix::unistd::Pid::from_raw(pid), None) {
                Ok(wait::WaitStatus::Exited(_, code)) => {
                    if let Err(e) = tx.send(Some(WaitStatus::Exited(code))) {
                        log::warn!("Failed to send wait status: {}", e);
                    }
                }
                Ok(wait::WaitStatus::Signaled(_, sig, _)) => {
                    if let Err(e) = tx.send(Some(WaitStatus::Signaled(sig as i32))) {
                        log::warn!("Failed to send wait status: {}", e);
                    }
                }
                Ok(wait::WaitStatus::Stopped(_, sig)) => {
                    if let Err(e) = tx.send(Some(WaitStatus::Stopped(sig as i32))) {
                        log::warn!("Failed to send wait status: {}", e);
                    }
                }
                Ok(_) => {
                    if let Err(e) = tx.send(None) {
                        log::warn!("Failed to send wait status: {}", e);
                    }
                }
                Err(nix::errno::Errno::ECHILD) => {
                    log::info!("Server process has already exited. No need to kill it");
                    if let Err(e) = tx.send(None) {
                        log::warn!("Failed to send wait status: {}", e);
                    }
                }
                Err(_) => {
                    if let Err(e) = tx.send(None) {
                        log::warn!("Failed to send wait status: {}", e);
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

pub struct MonitorState {
    pub options: crate::flow_server_monitor_options::MonitorOptions,
    pub edenfs_watcher_retries: i32,
}

pub const MAX_EDENFS_WATCHER_RETRIES: i32 = 3;

mod keep_alive_loop {
    use super::MAX_EDENFS_WATCHER_RETRIES;
    use super::MonitorState;
    use super::server_instance;

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
            FlowExitStatus::NoError
            | FlowExitStatus::WindowsKilledByTaskManager
            | FlowExitStatus::InvalidFlowconfig
            | FlowExitStatus::PathIsNotAFile
            | FlowExitStatus::FlowconfigChanged
            | FlowExitStatus::InvalidSavedState
            | FlowExitStatus::UnusedServer
            | FlowExitStatus::UnknownError
            | FlowExitStatus::WatchmanError
            | FlowExitStatus::WatchmanFailed
            | FlowExitStatus::FileWatcherMissedChanges
            | FlowExitStatus::OutOfSharedMemory
            | FlowExitStatus::HashTableFull
            | FlowExitStatus::HeapFull
            | FlowExitStatus::CouldNotExtractFlowlibs
            | FlowExitStatus::Interrupted
            | FlowExitStatus::BuildIdMismatch
            | FlowExitStatus::LockStolen
            | FlowExitStatus::SocketError
            | FlowExitStatus::DfindDied => (true, None, false),
            FlowExitStatus::EdenfsWatcherFailed | FlowExitStatus::EdenfsWatcherLostChanges => {
                (false, None, true)
            }
            FlowExitStatus::ServerOutOfDate => (
                false,
                Some(flow_server_env::server_status::RestartReason::ServerOutOfDate),
                false,
            ),
            FlowExitStatus::KilledByMonitor => (false, None, false),
            FlowExitStatus::Restart => (
                false,
                Some(flow_server_env::server_status::RestartReason::Restart),
                false,
            ),
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
            | FlowExitStatus::Autostop => (true, None, false),
        }
    }

    pub(super) fn killall_persistent_connections(
        exit_type: flow_common_exit_status::FlowExitStatus,
    ) {
        let clients = crate::persistent_connection_map::get_all_clients();
        for conn in clients {
            let msg = flow_server_env::lsp_prot::MessageFromServer::NotificationFromServer(
                flow_server_env::lsp_prot::NotificationFromServer::ServerExit(exit_type),
            );
            conn.write(msg);
            conn.try_flush_and_close();
        }
    }

    pub(super) fn should_monitor_exit_with_signaled_server(signal: i32) -> bool {
        signal == libc::SIGSEGV || signal == libc::SIGBUS
    }

    pub(super) fn wait_for_server_to_die(
        monitor_state: MonitorState,
        server: &mut server_instance::ServerInstance,
    ) -> (
        MonitorState,
        Option<flow_server_env::server_status::RestartReason>,
    ) {
        let pid = server_instance::pid_of(server);
        let server_thread = server.server_thread.take();
        let wait_status = match server_thread {
            Some(handle) => {
                let join_result = handle.join();
                match join_result {
                    Ok(()) => nix::sys::wait::WaitStatus::Exited(
                        nix::unistd::Pid::from_raw(pid),
                        flow_common_exit_status::FlowExitStatus::UnknownError as i32,
                    ),
                    Err(_) => nix::sys::wait::WaitStatus::Signaled(
                        nix::unistd::Pid::from_raw(pid),
                        nix::sys::signal::Signal::SIGABRT,
                        false,
                    ),
                }
            }
            None => {
                log::error!(
                    "wait_for_server_to_die: server_thread was None for pid {}",
                    pid
                );
                return (monitor_state, None);
            }
        };
        server_instance::cleanup(server);

        match wait_status {
            nix::sys::wait::WaitStatus::Exited(_, exit_code) => {
                let exit_type = server_instance::error_type_of_code(exit_code);
                let exit_status_string = exit_type
                    .map(flow_common_exit_status::to_string)
                    .unwrap_or("Invalid_exit_code");
                log::error!(
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
                            if monitor_state.edenfs_watcher_retries < MAX_EDENFS_WATCHER_RETRIES {
                                log::info!(
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
                                log::error!(
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
            nix::sys::wait::WaitStatus::Signaled(_, signal, _) => {
                let signal = signal as i32;
                log::error!(
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
            nix::sys::wait::WaitStatus::Stopped(_, signal) => {
                let signal = signal as i32;
                log::error!(
                    "Flow server (pid {}) was stopped with signal {}. Sending sigkill",
                    pid,
                    signal
                );
                if let Err(e) = nix::sys::signal::kill(
                    nix::unistd::Pid::from_raw(pid),
                    nix::sys::signal::Signal::SIGKILL,
                ) {
                    log::warn!("Failed to send SIGKILL to server process ({}): {}", pid, e);
                }
                (monitor_state, None)
            }
            _ => {
                log::error!("Flow server (pid {}) exited with unknown status", pid);
                (monitor_state, None)
            }
        }
    }

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

fn setup_signal_handlers() {
    let signals = [libc::SIGINT, libc::SIGTERM, libc::SIGHUP, libc::SIGQUIT];

    let signals_vec: Vec<i32> = signals.to_vec();
    std::thread::spawn(move || {
        let mut sigs = match signal_hook::iterator::Signals::new(&signals_vec) {
            Ok(s) => s,
            Err(err) => {
                log::error!("Failed to create signal iterator: {}", err);
                return;
            }
        };
        if let Some(sig) = sigs.forever().next() {
            crate::exit_signal::SIGNAL.broadcast(
                flow_common_exit_status::FlowExitStatus::Interrupted,
                format!("Received signal {}", sig),
            );
            EXITING.store(true, Ordering::SeqCst);
        }
    });
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
    log::debug!("Adding request to the command stream");
    push_to_command_stream(Command::WriteEphemeralRequest { request, client });
}

pub fn send_persistent_request(
    client_id: flow_server_env::lsp_prot::ClientId,
    request: flow_server_env::lsp_prot::RequestWithMetadata,
) {
    log::debug!("Adding request to the command stream");
    push_to_command_stream(Command::WritePersistentRequest { client_id, request });
}

pub fn notify_new_persistent_connection(
    client_id: flow_server_env::lsp_prot::ClientId,
    lsp_init_params: lsp_types::InitializeParams,
) {
    log::debug!(
        "Adding notification that there's a new persistent client #{}",
        client_id
    );
    push_to_command_stream(Command::NotifyNewPersistentConnection {
        client_id,
        lsp_init_params,
    });
}

pub fn notify_dead_persistent_connection(client_id: flow_server_env::lsp_prot::ClientId) {
    log::debug!(
        "Adding notification that persistent client #{} died",
        client_id
    );
    push_to_command_stream(Command::NotifyDeadPersistentConnection { client_id });
}
