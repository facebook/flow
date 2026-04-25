/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module listens to the socket, accepts new connections, and starts the
// FlowServerMonitorConnections which handle those connects.
//
// Wire protocol:
// All socket I/O — handshake AND post-handshake messages — uses bincode-framed serde, the closest
// Rust analogue of OCaml's `Marshal_tools.{from,to}_fd_with_preamble`. The handshake exchanges
// `ClientHandshakeWire = (json_string, bincode_bytes(client2))` and
// `ServerHandshakeWire = (json_string, Option<bincode_bytes(server2)>)`. After the handshake, the
// ephemeral channel exchanges `ServerCommandWithContext` (client→server) /
// `MonitorToClientMessage` (server→client); the persistent channel exchanges
// `RequestWithMetadata` / `MessageFromServer`. All bincode-framed.
use std::sync::Arc;

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
// `cancel` is checked at the top of every iteration. The OCaml `LwtLoop.Make` body is
// implicitly cancellable at every `let%lwt`; here we settle for cooperative cancellation
// so the select! losers in `create_*_connection` can stop the loop without leaking the
// blocking thread. Worst-case latency is one iteration (~0.5s — the timeout passed to
// `wait_for_signficant_status`), the same suspension granularity OCaml has.
fn status_loop_run<W: StatusWriter>(conn: &W::Connection, cancel: &std::sync::atomic::AtomicBool) {
    loop {
        if cancel.load(std::sync::atomic::Ordering::SeqCst) {
            return;
        }
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

fn create_ephemeral_connection(
    client_stream: std::net::TcpStream,
    close: Arc<dyn Fn() + Send + Sync>,
) -> Arc<crate::flow_server_monitor_connection::EphemeralConnection> {
    log::debug!("Creating a new ephemeral connection");

    let read_stream = client_stream
        .try_clone()
        .expect("clone of TcpStream for read failed");
    let write_stream = client_stream;

    let close_for_create = close.clone();
    let (start, conn) = crate::flow_server_monitor_connection::EphemeralConnection::create(
        "some ephemeral connection".to_string(),
        read_stream,
        write_stream,
        move || close_for_create(),
        |msg, connection| {
            handle_ephemeral_request(msg, connection.clone());
        },
    );

    // On exit, do our best to send all pending messages to the waiting client.
    let conn_for_close_on_exit = conn.clone();
    let close_on_exit = async move {
        crate::exit_signal::SIGNAL.notified().await;
        tokio::task::spawn_blocking(move || {
            conn_for_close_on_exit.try_flush_and_close();
        })
        .await
        .expect("try_flush_and_close blocking task panicked");
    };

    // Lwt.pick returns the first thread to finish and cancels the rest. We approximate
    // that here: `close_on_exit` is an abortable async task (its `.await` on the exit
    // signal is cancellable by dropping the future). `status_loop_run` is a blocking
    // task that observes a cooperative cancel flag at the top of each iteration. After
    // the select! resolves we abort the close_on_exit task and set the status loop's
    // cancel flag, so the losers stop instead of running to completion.
    let conn_for_wait = conn.clone();
    let conn_for_loop = conn.clone();
    let wait_for_closed_notify = conn_for_wait.wait_for_closed_notify();
    let status_cancel = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let status_cancel_for_loop = status_cancel.clone();
    crate::runtime::handle().spawn(async move {
        let close_on_exit_handle = crate::runtime::handle().spawn(close_on_exit);
        let close_on_exit_abort = close_on_exit_handle.abort_handle();
        let status_loop_handle = tokio::task::spawn_blocking(move || {
            status_loop_run::<EphemeralStatusWriter>(&conn_for_loop, &status_cancel_for_loop);
        });
        tokio::select! {
            _ = close_on_exit_handle => {}
            _ = wait_for_closed_notify.notified() => {}
            _ = status_loop_handle => {}
        }
        close_on_exit_abort.abort();
        status_cancel.store(true, std::sync::atomic::Ordering::SeqCst);
    });

    // Start the ephemeral connection.
    start();

    // Send the current server state immediate.
    let status = crate::status_stream::get_status();
    let msg = flow_server_env::monitor_prot::MonitorToClientMessage::PleaseHold(status.0, status.1);
    conn.write(msg);

    conn
}

// No lock needed, since the socket acceptor runs serially.
fn create_persistent_id() -> i32 {
    use std::sync::atomic::AtomicI32;
    use std::sync::atomic::Ordering;
    static LAST_PERSISTENT_ID: AtomicI32 = AtomicI32::new(0);
    LAST_PERSISTENT_ID.fetch_add(1, Ordering::SeqCst) + 1
}

fn create_persistent_connection(
    client_stream: std::net::TcpStream,
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

    let close_for_disconnect = close.clone();
    let disconnect: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
        close_for_disconnect();
    });

    let writer_stream = client_stream
        .try_clone()
        .expect("clone of TcpStream for persistent writer failed");
    let reader = crate::flow_server_monitor_connection::PersistentReader {
        stream: client_stream,
        disconnect,
    };
    let writer = crate::flow_server_monitor_connection::PersistentWriter {
        stream: writer_stream,
    };

    let close_for_create = close.clone();
    let (start, conn) = crate::flow_server_monitor_connection::MonitorPersistentConnection::create(
        format!("persistent connection #{}", client_id),
        reader,
        writer,
        move || close_for_create(),
        move |msg, connection| {
            handle_persistent_message(client_id, msg, connection.clone());
        },
    );

    // On exit, do our best to send all pending messages to the waiting client.
    let conn_for_close_on_exit = conn.clone();
    let close_on_exit = async move {
        let (exit_status, _) = crate::exit_signal::SIGNAL.notified().await;
        tokio::task::spawn_blocking(move || {
            // Notifies the client why the connection is closing. This can be useful to
            // the persistent client to decide if it should autostart a new monitor.
            conn_for_close_on_exit.write(
                flow_server_env::lsp_prot::MessageFromServer::NotificationFromServer(
                    flow_server_env::lsp_prot::NotificationFromServer::ServerExit(exit_status),
                ),
            );
            conn_for_close_on_exit.try_flush_and_close();
        })
        .await
        .expect("try_flush_and_close blocking task panicked");
    };

    // Don't start the connection until we add it to the persistent connection map.
    crate::persistent_connection_map::add(client_id, conn.clone());

    // Lwt.pick returns the first thread to finish and cancels the rest. We approximate
    // that here: `close_on_exit` is an abortable async task (its `.await` on the exit
    // signal is cancellable by dropping the future). `status_loop_run` is a blocking
    // task that observes a cooperative cancel flag at the top of each iteration. After
    // the select! resolves we abort the close_on_exit task and set the status loop's
    // cancel flag, so the losers stop instead of running to completion.
    let conn_for_wait = conn.clone();
    let conn_for_loop = conn.clone();
    let wait_for_closed_notify = conn_for_wait.wait_for_closed_notify();
    let status_cancel = Arc::new(std::sync::atomic::AtomicBool::new(false));
    let status_cancel_for_loop = status_cancel.clone();
    crate::runtime::handle().spawn(async move {
        let close_on_exit_handle = crate::runtime::handle().spawn(close_on_exit);
        let close_on_exit_abort = close_on_exit_handle.abort_handle();
        let status_loop_handle = tokio::task::spawn_blocking(move || {
            status_loop_run::<PersistentStatusWriter>(&conn_for_loop, &status_cancel_for_loop);
        });
        tokio::select! {
            _ = close_on_exit_handle => {}
            _ = wait_for_closed_notify.notified() => {}
            _ = status_loop_handle => {}
        }
        close_on_exit_abort.abort();
        status_cancel.store(true, std::sync::atomic::Ordering::SeqCst);
    });

    start();

    let status = crate::status_stream::get_status();
    let msg = flow_server_env::lsp_prot::MessageFromServer::NotificationFromServer(
        flow_server_env::lsp_prot::NotificationFromServer::PleaseHold(status.0, status.1),
    );
    conn.write(msg);
}

// Close the client_stream, regardless of whether or not we were able to shutdown the connection.
// This prevents fd leaks.
// To be perfectly honest, it's not clear whether the SHUTDOWN_BOTH is really needed. I mean,
// shutdown is useful to shutdown one direction of the socket, but if you're about to close
// it, does shutting down first actually make any difference?
fn close(client_stream: std::net::TcpStream) {
    log::debug!("Shutting down and closing a socket client stream");

    match client_stream.shutdown(std::net::Shutdown::Both) {
        Ok(()) => {}
        Err(err) => match err.kind() {
            std::io::ErrorKind::NotConnected | std::io::ErrorKind::ConnectionReset => {}
            _ => log::error!("Failed to shutdown socket client: {}", err),
        },
    }
    drop(client_stream);
}

// Well...I mean this is a pretty descriptive function name. It performs the handshake and then
// returns the client's side of the handshake.
fn perform_handshake_and_get_client_handshake(
    client_stream: &mut std::net::TcpStream,
) -> Option<flow_server_env::socket_handshake::ClientToMonitor2> {
    use flow_server_env::socket_handshake::*;

    let server_build_id = build_revision();
    let server_bin = std::env::current_exe()
        .expect("current_exe failed")
        .into_os_string()
        .into_string()
        .expect("server executable path is not valid UTF-8");

    // Handshake step 1: client sends handshake (bincode-framed, OCaml-faithful).
    let wire: ClientHandshakeWire = match bincode::deserialize_from(&mut *client_stream) {
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

    // Decode the bincode tail of the handshake to recover ClientToMonitor2 (which carries the
    // ClientType). If the tail is empty or malformed, fall back to Ephemeral.
    let client = if client_build_id != server_build_id {
        None
    } else if wire.1.is_empty() {
        Some(ClientToMonitor2 {
            client_type: ClientType::Ephemeral,
        })
    } else {
        match bincode::deserialize::<ClientToMonitor2>(&wire.1) {
            Ok(c) => Some(c),
            Err(e) => {
                log::error!("Failed to decode bincode tail of client handshake: {}", e);
                Some(ClientToMonitor2 {
                    client_type: ClientType::Ephemeral,
                })
            }
        }
    };

    // Handshake step 2: server sends back handshake (bincode-framed, OCaml-faithful).
    let server_bin_for_respond = server_bin.clone();
    let server_build_id_for_respond = server_build_id.clone();
    let respond = |stream: &mut std::net::TcpStream,
                   server_intent: ServerIntent,
                   server2: Option<MonitorToClient2>| {
        assert!(server2.is_none() || client_build_id == server_build_id_for_respond);
        let server_version = flow_common::flow_version::VERSION.to_string();
        let server1 = MonitorToClient1 {
            server_build_id: server_build_id_for_respond.clone(),
            server_bin: server_bin_for_respond.clone(),
            server_intent,
            server_version,
        };
        let json = monitor_to_client_1_to_json(&server1);
        let server2_bytes = server2.map(|s| bincode::serialize(&s).expect("bincode serialize"));
        let wire: ServerHandshakeWire = (json.to_string(), server2_bytes);
        if let Err(e) = bincode::serialize_into(&mut *stream, &wire) {
            log::error!("Failed to write server handshake: {}", e);
        }
    };

    // Stop request.
    if is_stop_request {
        respond(client_stream, ServerIntent::ServerWillExit, None);
        Server::stop(Server::StopReason::Stopped);
    } else if client_build_id != build_revision() {
        // Binary version mismatch.
        match version_mismatch_strategy {
            VersionMismatchStrategy::AlwaysStopServer => {
                respond(client_stream, ServerIntent::ServerWillExit, None);
                let msg =
                    "Client and server are different builds. Flow server is out of date. Exiting";
                log::error!("{}", msg);
                Server::exit(
                    None,
                    msg,
                    flow_common_exit_status::FlowExitStatus::BuildIdMismatch,
                );
            }
            VersionMismatchStrategy::StopServerIfOlder => {
                let cmp: std::cmp::Ordering = match (
                    semver::Version::parse(flow_common::flow_version::VERSION),
                    semver::Version::parse(&client_version),
                ) {
                    (Ok(server_v), Ok(client_v)) => Ord::cmp(&server_v, &client_v),
                    _ => Ord::cmp(flow_common::flow_version::VERSION, client_version.as_str()),
                };
                if cmp < std::cmp::Ordering::Equal {
                    respond(client_stream, ServerIntent::ServerWillExit, None);
                    let msg = "Client and server are different builds. Flow server is out of date. Exiting";
                    log::error!("{}", msg);
                    Server::exit(
                        None,
                        msg,
                        flow_common_exit_status::FlowExitStatus::BuildIdMismatch,
                    );
                } else {
                    respond(client_stream, ServerIntent::ServerWillHangup, None);
                    log::error!("Build mismatch, so rejecting attempted connection");
                    None
                }
            }
            VersionMismatchStrategy::ErrorClient => {
                respond(client_stream, ServerIntent::ServerWillHangup, None);
                log::error!("Build mismatch, so rejecting attempted connection");
                None
            }
        }
    } else if !crate::status_stream::ever_been_free() {
        // Server still initializing.
        let client = client.expect("client is None despite matching build_id");
        let status = crate::status_stream::get_status();
        if server_should_hangup_if_still_initializing {
            respond(
                client_stream,
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
                client_stream,
                ServerIntent::ServerWillContinue,
                Some(MonitorToClient2::ServerStillInitializing(
                    status.0, status.1,
                )),
            );
            Some(client)
        }
    } else {
        // Success.
        let client = client.expect("client is None despite matching build_id");
        respond(
            client_stream,
            ServerIntent::ServerWillContinue,
            Some(MonitorToClient2::ServerReady),
        );
        Some(client)
    }
}

pub trait Handler {
    fn create_socket_connection(autostop: bool, client_stream: std::net::TcpStream);
    fn name() -> &'static str;
}

fn socket_acceptor_loop<H: Handler>(autostop: bool, listener: std::net::TcpListener) {
    loop {
        log::debug!("Waiting for a new {}", H::name());
        let (client_stream, _addr) = match listener.accept() {
            Ok(c) => c,
            Err(e) => {
                log::error!("Uncaught exception in the socket acceptor: {}", e);
                return;
            }
        };

        H::create_socket_connection(autostop, client_stream);
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

    fn create_socket_connection(autostop: bool, mut client_stream: std::net::TcpStream) {
        // Autostop is meant to be "edge-triggered", i.e. when we transition
        // from 1 connections to 0 connections then it might stop the server.
        // But when an attempt to connect has failed, we need to close without
        // triggering an autostop.
        let close_stream_slot = std::sync::Arc::new(std::sync::Mutex::new(Some(
            client_stream
                .try_clone()
                .expect("clone of TcpStream failed"),
        )));
        let close_stream_for_close = close_stream_slot.clone();
        let close_without_autostop: Arc<dyn Fn() + Send + Sync> = Arc::new(move || {
            if let Some(s) = close_stream_for_close.lock().unwrap().take() {
                close(s);
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

        let handshake_result = perform_handshake_and_get_client_handshake(&mut client_stream);
        match handshake_result {
            Some(client) => {
                use flow_server_env::socket_handshake::ClientType;
                autostop::cancel_countdown();
                // Disarm the close_stream_slot — we now own the stream.
                close_stream_slot.lock().unwrap().take();
                match client.client_type {
                    ClientType::Ephemeral => {
                        create_ephemeral_connection(client_stream, close);
                    }
                    ClientType::Persistent { lsp_init_params } => {
                        create_persistent_connection(client_stream, close, lsp_init_params);
                    }
                }
            }
            None => {
                close_without_autostop();
            }
        }
    }
}

pub fn run(monitor_socket_listener: std::net::TcpListener, autostop: bool) {
    socket_acceptor_loop::<MonitorSocketHandler>(autostop, monitor_socket_listener);
}

struct LegacySocketHandler;
impl Handler for LegacySocketHandler {
    fn name() -> &'static str {
        "legacy socket connection"
    }

    fn create_socket_connection(_autostop: bool, client_stream: std::net::TcpStream) {
        close(client_stream);
        let msg = "Client and server are different builds. Flow server is out of date. Exiting";
        log::error!("{}", msg);
        Server::stop(Server::StopReason::LegacyClient);
    }
}

pub fn run_legacy(legacy_socket_listener: std::net::TcpListener) {
    socket_acceptor_loop::<LegacySocketHandler>(false, legacy_socket_listener);
}
