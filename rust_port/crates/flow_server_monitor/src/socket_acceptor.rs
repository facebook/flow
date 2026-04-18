/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module listens to the socket, accepts new connections, and starts the
// FlowServerMonitorConnections which handle those connects.
use std::os::fd::AsRawFd;
use std::os::fd::IntoRawFd;
use std::os::fd::OwnedFd;
use std::sync::Arc;

use nix::sys::socket::Shutdown;
use tokio::sync::Notify;

use crate::flow_server_monitor_server as Server;

// Just forward requests to the server.
fn handle_ephemeral_request(
    msg: flow_server_env::server_command_with_context::ServerCommandWithContext,
    connection: Arc<crate::flow_server_monitor_connection::EphemeralConnection>,
) {
    Server::send_request(connection, msg);
}

// Just forward requests to the server.
fn handle_persistent_message(
    client_id: flow_server_env::lsp_prot::ClientId,
    msg: flow_server_env::lsp_prot::RequestWithMetadata,
    _connection: Arc<crate::flow_server_monitor_connection::MonitorPersistentConnection>,
) {
    log::debug!("Persistent connection #{} received a message!", client_id);
    Server::send_persistent_request(client_id, msg);
}

pub trait StatusWriter {
    type Connection;
    fn write(
        status: (
            flow_server_env::server_status::Status,
            flow_server_env::file_watcher_status::Status,
        ),
        conn: &Self::Connection,
    ) -> bool;
}

// A loop that sends the Server's busy status to a waiting connection every 0.5 seconds.
fn status_loop_run<W: StatusWriter>(conn: &W::Connection) {
    loop {
        // It is important that we not yield between wait_for_signficant_status and the
        // next iteration of this loop, where we wait again. If we are not waiting when
        // a status is sent, we'll miss it.
        let status = crate::status_stream::wait_for_signficant_status(0.5);
        if !W::write(status, conn) {
            // The connection closed its write stream, likely it is closed or closing.
            return;
        }
    }
}

struct EphemeralStatusWriter;
impl StatusWriter for EphemeralStatusWriter {
    type Connection = crate::flow_server_monitor_connection::EphemeralConnection;
    fn write(
        status: (
            flow_server_env::server_status::Status,
            flow_server_env::file_watcher_status::Status,
        ),
        conn: &Self::Connection,
    ) -> bool {
        conn.write(
            flow_server_env::monitor_prot::MonitorToClientMessage::PleaseHold(status.0, status.1),
        )
    }
}

struct PersistentStatusWriter;
impl StatusWriter for PersistentStatusWriter {
    type Connection = crate::flow_server_monitor_connection::MonitorPersistentConnection;
    fn write(
        status: (
            flow_server_env::server_status::Status,
            flow_server_env::file_watcher_status::Status,
        ),
        conn: &Self::Connection,
    ) -> bool {
        conn.write(
            flow_server_env::lsp_prot::MessageFromServer::NotificationFromServer(
                flow_server_env::lsp_prot::NotificationFromServer::PleaseHold(status.0, status.1),
            ),
        )
    }
}

fn create_ephemeral_connection(client_fd: OwnedFd, close: Arc<dyn Fn() + Send + Sync>) {
    log::debug!("Creating a new ephemeral connection");

    let in_fd = client_fd;
    let out_fd = nix::unistd::dup(&in_fd).expect("dup of socket fd failed");

    let close_for_create = close.clone();
    let (start, conn) = crate::flow_server_monitor_connection::EphemeralConnection::create(
        "some ephemeral connection".to_string(),
        in_fd,
        out_fd,
        move || close_for_create(),
        |msg, connection| {
            handle_ephemeral_request(msg, connection.clone());
        },
    );

    // On exit, do our best to send all pending messages to the waiting client.
    let conn_for_close_on_exit = conn.clone();
    let close_on_exit = move || {
        crate::exit_signal::SIGNAL.wait();
        conn_for_close_on_exit.try_flush_and_close();
    };

    // Lwt.pick returns the first thread to finish and cancels the rest.
    let conn_for_wait = conn.clone();
    let conn_for_loop = conn.clone();
    std::thread::spawn(move || {
        let done_notify = Arc::new(Notify::new());

        let done_close = done_notify.clone();
        std::thread::spawn(move || {
            close_on_exit();
            done_close.notify_one();
        });
        let done_wait = done_notify.clone();
        std::thread::spawn(move || {
            conn_for_wait.wait_for_closed();
            done_wait.notify_one();
        });
        let done_loop = done_notify.clone();
        std::thread::spawn(move || {
            status_loop_run::<EphemeralStatusWriter>(&conn_for_loop);
            done_loop.notify_one();
        });

        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("tokio runtime build failed");
        runtime.block_on(async {
            done_notify.notified().await;
        });
    });

    // Start the ephemeral connection.
    start();

    // Send the current server state immediate.
    let status = crate::status_stream::get_status();
    let msg = flow_server_env::monitor_prot::MonitorToClientMessage::PleaseHold(status.0, status.1);
    conn.write(msg);
}

// No lock needed, since the socket acceptor runs serially.
fn create_persistent_id() -> i32 {
    use std::sync::atomic::AtomicI32;
    use std::sync::atomic::Ordering;
    static LAST_PERSISTENT_ID: AtomicI32 = AtomicI32::new(0);
    LAST_PERSISTENT_ID.fetch_add(1, Ordering::SeqCst) + 1
}

fn create_persistent_connection(
    client_fd: OwnedFd,
    close: Arc<dyn Fn() + Send + Sync>,
    lsp_init_params: lsp_types::InitializeParams,
) {
    let client_id = create_persistent_id();
    log::debug!("Creating a persistent connection #{}", client_id);

    Server::notify_new_persistent_connection(client_id, lsp_init_params);

    let outer_close = close;
    let close: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
        Server::notify_dead_persistent_connection(client_id);
        outer_close();
    });

    let in_fd = client_fd;
    let out_fd = nix::unistd::dup(&in_fd).expect("dup of socket fd failed");
    let close_for_create = close.clone();
    let (start, conn) = crate::flow_server_monitor_connection::MonitorPersistentConnection::create(
        format!("persistent connection #{}", client_id),
        in_fd,
        out_fd,
        move || close_for_create(),
        move |msg, connection| {
            handle_persistent_message(client_id, msg, connection.clone());
        },
    );

    // On exit, do our best to send all pending messages to the waiting client.
    let conn_for_close_on_exit = conn.clone();
    let close_on_exit = move || {
        let (exit_status, _) = crate::exit_signal::SIGNAL.wait();
        // Notifies the client why the connection is closing. This can be useful to
        // the persistent client to decide if it should autostart a new monitor.
        conn_for_close_on_exit.write(
            flow_server_env::lsp_prot::MessageFromServer::NotificationFromServer(
                flow_server_env::lsp_prot::NotificationFromServer::ServerExit(exit_status),
            ),
        );
        conn_for_close_on_exit.try_flush_and_close();
    };

    // Don't start the connection until we add it to the persistent connection map.
    crate::persistent_connection_map::add(client_id, conn.clone());

    // Lwt.pick returns the first thread to finish and cancels the rest.
    let conn_for_wait = conn.clone();
    let conn_for_loop = conn.clone();
    std::thread::spawn(move || {
        let done_notify = Arc::new(Notify::new());

        let done_close = done_notify.clone();
        std::thread::spawn(move || {
            close_on_exit();
            done_close.notify_one();
        });
        let done_wait = done_notify.clone();
        std::thread::spawn(move || {
            conn_for_wait.wait_for_closed();
            done_wait.notify_one();
        });
        let done_loop = done_notify.clone();
        std::thread::spawn(move || {
            status_loop_run::<PersistentStatusWriter>(&conn_for_loop);
            done_loop.notify_one();
        });

        let runtime = tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("tokio runtime build failed");
        runtime.block_on(async {
            done_notify.notified().await;
        });
    });

    start();

    let status = crate::status_stream::get_status();
    let msg = flow_server_env::lsp_prot::MessageFromServer::NotificationFromServer(
        flow_server_env::lsp_prot::NotificationFromServer::PleaseHold(status.0, status.1),
    );
    conn.write(msg);
}

// Close the client_fd, regardless of whether or not we were able to shutdown the connection.
// This prevents fd leaks.
// To be perfectly honest, it's not clear whether the SHUTDOWN_ALL is really needed. I mean,
// shutdown is useful to shutdown one direction of the socket, but if you're about to close
// it, does shutting down first actually make any difference?
fn close(client_fd: OwnedFd) {
    log::debug!("Shutting down and closing a socket client fd");

    match nix::sys::socket::shutdown(client_fd.as_raw_fd(), Shutdown::Both) {
        Ok(()) => {}
        Err(errno) => match errno {
            nix::errno::Errno::EBADF
            | nix::errno::Errno::ENOTCONN
            | nix::errno::Errno::ECONNRESET
            | nix::errno::Errno::ECONNABORTED => {}
            // These errors happen when the connection is already closed, so we can
            // ignore them. Note that POSIX and Windows have different errors:
            // see https://man7.org/linux/man-pages/man2/shutdown.2.html
            // and https://docs.microsoft.com/en-us/windows/win32/api/winsock/nf-winsock-shutdown
            _ => {
                log::error!("Failed to shutdown socket client: {}", errno);
            }
        },
    }

    let raw = client_fd.into_raw_fd();
    match nix::unistd::close(raw) {
        Ok(()) => {}
        Err(errno) => match errno {
            // Already closed.
            nix::errno::Errno::EBADF => {}
            _ => {
                log::error!("Failed to close socket client fd: {}", errno);
            }
        },
    }
}

// Well...I mean this is a pretty descriptive function name. It performs the handshake and then
// returns the client's side of the handshake.
fn perform_handshake_and_get_client_handshake(
    client_fd: &OwnedFd,
) -> Option<flow_server_env::socket_handshake::ClientToMonitor2> {
    use flow_server_env::socket_handshake::*;

    let server_build_id = build_revision();
    let server_bin = std::env::current_exe()
        .expect("current_exe failed")
        .into_os_string()
        .into_string()
        .expect("server executable path is not valid UTF-8");

    // Handshake step 1: client sends handshake.
    let dup_in = nix::unistd::dup(client_fd).expect("dup of socket fd failed");
    let mut in_file = std::fs::File::from(dup_in);
    let wire: ClientHandshakeWire = match bincode::deserialize_from(&mut in_file) {
        Ok(w) => w,
        Err(e) => {
            log::error!("Malformed handshake preamble: {}", e);
            return None;
        }
    };

    let ClientToMonitor1 {
        client_build_id,
        client_version,
        is_stop_request,
        server_should_hangup_if_still_initializing,
        version_mismatch_strategy,
    } = match serde_json::from_str::<serde_json::Value>(&wire.0) {
        Ok(json) => json_to_client_to_monitor_1(&json),
        Err(e) => {
            log::error!(
                "Failed to parse JSON section of handshake: {}: {}",
                wire.0,
                e
            );
            default_client_to_monitor_1()
        }
    };

    let client = if client_build_id != server_build_id {
        None
    } else {
        match bincode::deserialize::<ClientToMonitor2>(&wire.1) {
            Ok(c) => Some(c),
            Err(e) => {
                log::error!("Failed to deserialize client_to_monitor_2: {}", e);
                None
            }
        }
    };

    // Handshake step 2: server sends back handshake.
    let respond = |server_intent: ServerIntent, server2: Option<MonitorToClient2>| {
        assert!(server2.is_none() || client_build_id == server_build_id);
        let server_version = flow_common::flow_version::VERSION.to_string();
        let server1 = MonitorToClient1 {
            server_build_id: server_build_id.clone(),
            server_bin: server_bin.clone(),
            server_intent,
            server_version,
        };
        let json = monitor_to_client_1_to_json(&server1);
        let server2_bytes = server2.map(|s| bincode::serialize(&s).expect("bincode serialize"));
        let wire: ServerHandshakeWire = (json.to_string(), server2_bytes);
        let dup_out = nix::unistd::dup(client_fd).expect("dup of socket fd failed");
        let mut out_file = std::fs::File::from(dup_out);
        if let Err(e) = bincode::serialize_into(&mut out_file, &wire) {
            log::error!("Failed to write server handshake: {}", e);
        }
    };

    let error_client =
        |respond: &dyn Fn(ServerIntent, Option<MonitorToClient2>)| -> Option<ClientToMonitor2> {
            respond(ServerIntent::ServerWillHangup, None);
            log::error!("Build mismatch, so rejecting attempted connection");
            None
        };

    let stop_server = |respond: &dyn Fn(ServerIntent, Option<MonitorToClient2>)| -> ! {
        respond(ServerIntent::ServerWillExit, None);
        let msg = "Client and server are different builds. Flow server is out of date. Exiting";
        log::error!("{}", msg);
        Server::exit(
            None,
            msg,
            flow_common_exit_status::FlowExitStatus::BuildIdMismatch,
        );
    };

    let fd_as_int: i32 = client_fd.as_raw_fd();

    // Stop request.
    if is_stop_request {
        respond(ServerIntent::ServerWillExit, None);
        let dup_for_close = nix::unistd::dup(client_fd).expect("dup of socket fd failed");
        close(dup_for_close);
        Server::stop(Server::StopReason::Stopped);
    } else if client_build_id != build_revision() {
        // Binary version mismatch.
        match version_mismatch_strategy {
            VersionMismatchStrategy::AlwaysStopServer => stop_server(&respond),
            VersionMismatchStrategy::StopServerIfOlder => {
                let cmp: std::cmp::Ordering = match (
                    semver::Version::parse(flow_common::flow_version::VERSION),
                    semver::Version::parse(&client_version),
                ) {
                    (Ok(server_v), Ok(client_v)) => Ord::cmp(&server_v, &client_v),
                    _ => Ord::cmp(flow_common::flow_version::VERSION, client_version.as_str()),
                };
                if cmp < std::cmp::Ordering::Equal {
                    stop_server(&respond)
                } else {
                    error_client(&respond)
                }
            }
            VersionMismatchStrategy::ErrorClient => error_client(&respond),
        }
    } else if cfg!(unix) && fd_as_int > 500 {
        // Too many clients.
        // We currently rely on using Unix.select, which doesn't work for fds >= FD_SETSIZE (1024).
        // So we can't have an unlimited number of clients. So if the new fd is too large, let's
        // reject it.
        // TODO(glevi): Figure out whether this check is needed for Windows.
        respond(
            ServerIntent::ServerWillHangup,
            Some(MonitorToClient2::ServerHasTooManyClients),
        );
        log::error!(
            "Too many clients, so rejecting new connection ({})",
            fd_as_int
        );
        None
    } else if !crate::status_stream::ever_been_free() {
        // Server still initializing.
        let client = client.expect("client is None despite matching build_id");
        let status = crate::status_stream::get_status();
        if server_should_hangup_if_still_initializing {
            respond(
                ServerIntent::ServerWillHangup,
                Some(MonitorToClient2::ServerStillInitializing(
                    status.0.clone(),
                    status.1.clone(),
                )),
            );
            // In the case of Persistent, lspCommand will retry a second later.
            // The message we log here solely goes to the logs, not the user.
            let (server_status, watchman_status) = status;
            log::info!(
                "Server still initializing -> hangup. server_status={} watchman_status={}",
                flow_server_env::server_status::string_of_status(false, false, &server_status),
                flow_server_env::file_watcher_status::string_of_status(&watchman_status)
            );
            None
        } else {
            respond(
                ServerIntent::ServerWillContinue,
                Some(MonitorToClient2::ServerStillInitializing(
                    status.0, status.1,
                )),
            );
            Some(client)
        }
    }
    // Success.
    else {
        let client = client.expect("client is None despite matching build_id");
        respond(
            ServerIntent::ServerWillContinue,
            Some(MonitorToClient2::ServerReady),
        );
        Some(client)
    }
}

pub trait Handler {
    fn create_socket_connection(autostop: bool, client_fd: OwnedFd);
    fn name() -> &'static str;
}

fn socket_acceptor_loop<H: Handler>(autostop: bool, socket_fd: &OwnedFd) {
    let dup_listener_fd = match nix::unistd::dup(socket_fd) {
        Ok(fd) => fd,
        Err(errno) => {
            log::error!(
                "Uncaught exception in the socket acceptor: dup failed: {}",
                errno
            );
            return;
        }
    };
    let listener = std::os::unix::net::UnixListener::from(dup_listener_fd);
    loop {
        log::debug!("Waiting for a new {}", H::name());
        let (client_stream, _addr) = match listener.accept() {
            Ok(c) => c,
            Err(e) => {
                log::error!("Uncaught exception in the socket acceptor: {}", e);
                return;
            }
        };
        let client_fd: OwnedFd = OwnedFd::from(client_stream);

        H::create_socket_connection(autostop, client_fd);
    }
}

pub mod autostop {
    use std::sync::Mutex;
    use std::sync::atomic::AtomicU64;
    use std::sync::atomic::Ordering;

    static CURRENT_COUNTDOWN: Mutex<Option<std::thread::JoinHandle<()>>> = Mutex::new(None);
    static GENERATION: AtomicU64 = AtomicU64::new(0);

    pub fn cancel_countdown() {
        GENERATION.fetch_add(1, Ordering::SeqCst);
    }

    pub fn start_countdown() {
        // Cancel any existing countdown to prevent orphaned timers that can't
        // be canceled via cancel_countdown.
        cancel_countdown();
        let my_generation = GENERATION.load(Ordering::SeqCst);
        let handle = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_secs(60));
            if GENERATION.load(Ordering::SeqCst) == my_generation {
                super::Server::stop(super::Server::StopReason::Autostopped);
            }
        });
        let mut guard = CURRENT_COUNTDOWN.lock().unwrap();
        *guard = Some(handle);
    }
}

struct MonitorSocketHandler;
impl Handler for MonitorSocketHandler {
    fn name() -> &'static str {
        "socket connection"
    }

    fn create_socket_connection(autostop: bool, client_fd: OwnedFd) {
        // Autostop is meant to be "edge-triggered", i.e. when we transition
        // from 1 connections to 0 connections then it might stop the server.
        // But when an attempt to connect has failed, we need to close without
        // triggering an autostop.
        let close_fd = std::sync::Mutex::new(Some(client_fd));
        let close_fd = std::sync::Arc::new(close_fd);
        let close_fd_for_close = close_fd.clone();
        let close_without_autostop: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
            if let Some(fd) = close_fd_for_close.lock().unwrap().take() {
                close(fd);
            }
        });

        let close_without_autostop_for_close = close_without_autostop.clone();
        let close: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
            close_without_autostop_for_close();
            let num_persistent: usize = crate::persistent_connection_map::cardinal();
            let num_ephemeral: usize = crate::request_map::cardinal();
            if autostop && num_persistent == 0 && num_ephemeral == 0 {
                autostop::start_countdown();
            }
        });

        let handshake_result = {
            let guard = close_fd.lock().unwrap();
            match guard.as_ref() {
                Some(fd) => perform_handshake_and_get_client_handshake(fd),
                None => None,
            }
        };
        match handshake_result {
            Some(client) => {
                use flow_server_env::socket_handshake::*;
                autostop::cancel_countdown();
                let owned_fd = match close_fd.lock().unwrap().take() {
                    Some(fd) => fd,
                    None => return,
                };
                match client {
                    ClientToMonitor2 {
                        client_type: ClientType::Ephemeral,
                    } => {
                        create_ephemeral_connection(owned_fd, close);
                    }
                    ClientToMonitor2 {
                        client_type: ClientType::Persistent { lsp_init_params },
                    } => {
                        create_persistent_connection(owned_fd, close, lsp_init_params);
                    }
                }
            }
            None => {
                close_without_autostop();
            }
        }
    }
}

pub fn run(monitor_socket_fd: &OwnedFd, autostop: bool) {
    socket_acceptor_loop::<MonitorSocketHandler>(autostop, monitor_socket_fd);
}

struct LegacySocketHandler;
impl Handler for LegacySocketHandler {
    fn name() -> &'static str {
        "legacy socket connection"
    }

    fn create_socket_connection(_autostop: bool, client_fd: OwnedFd) {
        close(client_fd);
        let msg = "Client and server are different builds. Flow server is out of date. Exiting";
        log::error!("{}", msg);
        Server::stop(Server::StopReason::LegacyClient);
    }
}

pub fn run_legacy(legacy_socket_fd: &OwnedFd) {
    socket_acceptor_loop::<LegacySocketHandler>(false, legacy_socket_fd);
}
