/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::BTreeMap;
use std::net::SocketAddr;
use std::net::TcpStream;
use std::path::Path;
use std::sync::Mutex;

use flow_monitor_rpc::monitor_prot::MonitorToClientMessage;
use flow_monitor_rpc::server_command_with_context::ServerCommandWithContext;
use flow_monitor_rpc::server_prot;
use flow_server_env::file_watcher_status;
use flow_server_env::server_status;
use flow_server_env::socket_handshake;
use flow_server_files::server_files_js;

#[derive(Debug)]
pub(crate) enum BusyReason {
    TooManyClients,
    NotResponding,
    FailOnInit(server_status::Status, file_watcher_status::Status),
}

#[derive(Debug)]
pub(crate) enum MismatchBehavior {
    // The server exited due to the build id mismatch
    ServerExited,
    // The server is still alive but the client should error
    ClientShouldError {
        server_bin: String,
        server_version: String,
    },
}

#[derive(Debug)]
pub(crate) enum CCSError {
    BuildIdMismatch(MismatchBehavior),
    ServerBusy(BusyReason),
    ServerMissing,
    ServerSocketMissing,
}

// Logical request the client sends to the monitor. `Shutdown` is encoded
// purely as a handshake `is_stop_request: true` (no body); `Command` is
// encoded as a bincode `ServerCommandWithContext` after the handshake.
pub(crate) enum ConnectRequest {
    Command(ServerCommandWithContext),
    Shutdown,
}

// Logical response from the monitor. `ShutdownAck` is synthesized by the
// client when the handshake for a stop request succeeds; the rest mirror
// `MonitorProt.monitor_to_client_message` (minus the `PleaseHold` skip).
pub(crate) enum ConnectResponse {
    Data(server_prot::response::Response),
    ShutdownAck,
    ServerException(String),
}

// pre-server-monitor versions used a different socket

enum ConnectExn {
    Timeout,
    MissingSocket,
}

pub(crate) fn server_exists(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> bool {
    let lock_file = server_files_js::lock_file(flowconfig_name, tmp_dir, root);
    let lock_path = std::path::Path::new(&lock_file);
    if !lock_path.exists() {
        return false;
    }
    let file = match std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .open(&lock_file)
    {
        Ok(file) => file,
        Err(_) => return true,
    };
    matches!(file.try_lock(), Err(std::fs::TryLockError::WouldBlock))
}

fn wait_on_server_restart(stream: &mut TcpStream) {
    use std::io::Read;
    let mut buf = [0u8; 1];
    loop {
        match stream.read(&mut buf) {
            // Server has exited and hung up on us
            Ok(0) | Err(_) => break,
            Ok(_) => continue,
        }
    }
}

type SockMap = BTreeMap<SocketAddr, TcpStream>;

// We used to open a new connection every time we tried to connect to a socket
// and then shut it down if the connection failed or timed out. However on OSX
// the shutdown wouldn't really do anything and we were hitting the pending
// connection limit set by Unix.listen. So instead we can hang on to the
// connection, since there's nothing wrong with it.
static CONNECTIONS: Mutex<Option<SockMap>> = Mutex::new(None);

fn with_connections<F, R>(f: F) -> R
where
    F: FnOnce(&mut SockMap) -> R,
{
    let mut guard = CONNECTIONS.lock().unwrap();
    let map = guard.get_or_insert_with(SockMap::new);
    f(map)
}

fn open_connection(
    timeout: u64,
    client_handshake: &socket_handshake::ClientHandshake,
    sockaddr: SocketAddr,
) -> Result<TcpStream, ConnectExn> {
    let existing = with_connections(|conns| conns.get(&sockaddr).and_then(|s| s.try_clone().ok()));
    if let Some(conn) = existing {
        return Ok(conn);
    }
    let mut conn = TcpStream::connect_timeout(&sockaddr, std::time::Duration::from_secs(timeout))
        .map_err(|e| {
        if e.kind() == std::io::ErrorKind::NotFound {
            ConnectExn::MissingSocket
        } else {
            ConnectExn::Timeout
        }
    })?;
    // It's important that we only write this once per connection.
    //
    // The wire shape mirrors OCaml `SocketHandshake.client_handshake_wire`:
    //   (client1_json, bincode(client2))
    // - client1 is JSON-encoded so the monitor can read it before deciding
    //   whether the client/server build ids agree.
    // - client2 is bincode-encoded; the monitor only deserializes it after
    //   confirming build ids match.
    let (ref client1, ref client2) = *client_handshake;
    let client1_json =
        serde_json::to_string(&socket_handshake::client_to_monitor_1_to_json(client1))
            .map_err(|_| ConnectExn::Timeout)?;
    let client2_bytes = bincode::serde::encode_to_vec(client2, bincode::config::legacy())
        .map_err(|_| ConnectExn::Timeout)?;
    let wire: socket_handshake::ClientHandshakeWire = (client1_json, client2_bytes);
    {
        use std::io::Write;
        let mut buffered = std::io::BufWriter::new(&mut conn);
        bincode::serde::encode_into_std_write(&wire, &mut buffered, bincode::config::legacy())
            .map_err(|_| ConnectExn::Timeout)?;
        buffered.flush().map_err(|_| ConnectExn::Timeout)?;
    }
    let conn_clone = conn.try_clone().map_err(|_| ConnectExn::Timeout)?;
    with_connections(|conns| {
        conns.insert(sockaddr, conn_clone);
    });
    Ok(conn)
}

fn close_connection(sockaddr: SocketAddr) {
    with_connections(|conns| {
        if let Some(stream) = conns.remove(&sockaddr) {
            let _ = stream.shutdown(std::net::Shutdown::Both);
        }
    });
}

fn establish_connection(
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &Path,
    request: &ConnectRequest,
    timeout_secs: Option<u64>,
) -> Result<(SocketAddr, TcpStream, socket_handshake::ClientHandshake), ConnectExn> {
    let socket_path = flow_common_socket::socket::get_path(&server_files_js::socket_file(
        flowconfig_name,
        tmp_dir,
        root,
    ));

    let port_str = std::fs::read_to_string(&socket_path).map_err(|_| ConnectExn::MissingSocket)?;
    let port: u16 = port_str
        .trim()
        .parse()
        .map_err(|_| ConnectExn::MissingSocket)?;

    let sockaddr = SocketAddr::from(([127, 0, 0, 1], port));
    let is_stop = matches!(request, ConnectRequest::Shutdown);
    let client_handshake = (
        socket_handshake::ClientToMonitor1 {
            client_build_id: socket_handshake::build_revision(),
            client_version: flow_common::flow_version::VERSION.to_string(),
            is_stop_request: is_stop,
            server_should_hangup_if_still_initializing: false,
            version_mismatch_strategy: if is_stop {
                socket_handshake::VersionMismatchStrategy::AlwaysStopServer
            } else {
                socket_handshake::VersionMismatchStrategy::ErrorClient
            },
        },
        socket_handshake::ClientToMonitor2 {
            client_type: socket_handshake::ClientType::Ephemeral,
        },
    );
    let connect_timeout = timeout_secs.unwrap_or(1);
    let stream = open_connection(connect_timeout, &client_handshake, sockaddr)?;

    let read_timeout = timeout_secs.unwrap_or(1);
    stream
        .set_read_timeout(Some(std::time::Duration::from_secs(read_timeout)))
        .map_err(|_| ConnectExn::Timeout)?;
    stream
        .set_write_timeout(Some(std::time::Duration::from_secs(10)))
        .map_err(|_| ConnectExn::Timeout)?;

    Ok((sockaddr, stream, client_handshake))
}

fn get_handshake(
    timeout: u64,
    sockaddr: SocketAddr,
    stream: &mut TcpStream,
) -> Result<(SocketAddr, socket_handshake::ServerHandshake), ConnectExn> {
    stream
        .set_read_timeout(Some(std::time::Duration::from_secs(timeout)))
        .map_err(|_| ConnectExn::Timeout)?;
    let wire: socket_handshake::ServerHandshakeWire =
        bincode::serde::decode_from_std_read(&mut *stream, bincode::config::legacy()).map_err(
            |e| {
                if let bincode::error::DecodeError::Io { inner, .. } = &e {
                    if inner.kind() == std::io::ErrorKind::TimedOut {
                        // Timeouts are expected
                        return ConnectExn::Timeout;
                    }
                }
                close_connection(sockaddr);
                ConnectExn::Timeout
            },
        )?;
    let server1_json: serde_json::Value =
        serde_json::from_str(&wire.0).map_err(|_| ConnectExn::Timeout)?;
    let server1 = socket_handshake::json_to_monitor_to_client_1(&server1_json).map_err(|_| {
        close_connection(sockaddr);
        ConnectExn::Timeout
    })?;
    // Server invariant: it only sends us snd=Some if it knows client+server versions match
    let server2: Option<socket_handshake::MonitorToClient2> = match &wire.1 {
        Some(bytes) => match bincode::serde::decode_from_slice(bytes, bincode::config::legacy()) {
            Ok((s, _)) => Some(s),
            Err(_) => {
                close_connection(sockaddr);
                return Err(ConnectExn::Timeout);
            }
        },
        None => None,
    };
    let server_handshake: socket_handshake::ServerHandshake = (server1, server2);
    Ok((sockaddr, server_handshake))
}

fn verify_handshake(
    client_handshake: &socket_handshake::ClientHandshake,
    server_handshake: &socket_handshake::ServerHandshake,
    sockaddr: SocketAddr,
    stream: &mut TcpStream,
) -> Result<(), CCSError> {
    let (client1, _client2) = client_handshake;
    let (server1, server2) = server_handshake;
    // First, let's close the connection as needed
    match &server1.server_intent {
        socket_handshake::ServerIntent::ServerWillContinue => {}
        socket_handshake::ServerIntent::ServerWillHangup => close_connection(sockaddr),
        socket_handshake::ServerIntent::ServerWillExit => {
            // If the server will exit shortly, we wouldn't want subsequent connection
            // attempts on the Unix Domain Socket to succeed (only to be doomed to failure).
            // To avoid that fate, we'll wait for the connection to be closed.
            wait_on_server_restart(stream);
            let _ = stream.shutdown(std::net::Shutdown::Both);
        }
    }
    // Next, let's interpret the server's response into our own response code
    match (&server1.server_intent, server2) {
        (
            socket_handshake::ServerIntent::ServerWillContinue,
            Some(socket_handshake::MonitorToClient2::ServerReady),
        ) => Ok(()),
        (
            socket_handshake::ServerIntent::ServerWillContinue,
            Some(socket_handshake::MonitorToClient2::ServerStillInitializing(..)),
        ) => Ok(()),
        (
            socket_handshake::ServerIntent::ServerWillHangup,
            Some(socket_handshake::MonitorToClient2::ServerHasTooManyClients),
        ) => Err(CCSError::ServerBusy(BusyReason::TooManyClients)),
        (
            socket_handshake::ServerIntent::ServerWillHangup,
            Some(socket_handshake::MonitorToClient2::ServerStillInitializing(
                server_status,
                watcher_status,
            )),
        ) => Err(CCSError::ServerBusy(BusyReason::FailOnInit(
            server_status.clone(),
            watcher_status.clone(),
        ))),
        (socket_handshake::ServerIntent::ServerWillHangup, None) => {
            if client1.client_build_id != server1.server_build_id {
                Err(CCSError::BuildIdMismatch(
                    MismatchBehavior::ClientShouldError {
                        server_bin: server1.server_bin.clone(),
                        server_version: server1.server_version.clone(),
                    },
                ))
            } else {
                panic!("Don't know why server closed the connection")
            }
        }
        (socket_handshake::ServerIntent::ServerWillExit, None) => {
            if client1.is_stop_request {
                Ok(())
            } else {
                // either the build ids were different, or client1 wasn't valid for server
                Err(CCSError::BuildIdMismatch(MismatchBehavior::ServerExited))
            }
        }
        _ => panic!("Monitor sent incorrect handshake"),
    }
}

// Connects to the monitor via a socket.
pub(crate) fn connect_once(
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &Path,
    request: &ConnectRequest,
    timeout_secs: Option<u64>,
) -> Result<ConnectResponse, CCSError> {
    match establish_connection(flowconfig_name, tmp_dir, root, request, timeout_secs) {
        Err(ConnectExn::MissingSocket) => {
            if server_exists(flowconfig_name, tmp_dir, root) {
                Err(CCSError::ServerSocketMissing)
            } else {
                Err(CCSError::ServerMissing)
            }
        }
        Err(ConnectExn::Timeout) => {
            if server_exists(flowconfig_name, tmp_dir, root) {
                Err(CCSError::ServerBusy(BusyReason::NotResponding))
            } else {
                Err(CCSError::ServerMissing)
            }
        }
        Ok((sockaddr, mut stream, client_handshake)) => {
            let (_, server_handshake) =
                get_handshake(timeout_secs.unwrap_or(1), sockaddr, &mut stream).map_err(
                    |e| match e {
                        ConnectExn::MissingSocket => {
                            if server_exists(flowconfig_name, tmp_dir, root) {
                                CCSError::ServerSocketMissing
                            } else {
                                CCSError::ServerMissing
                            }
                        }
                        ConnectExn::Timeout => {
                            if server_exists(flowconfig_name, tmp_dir, root) {
                                CCSError::ServerBusy(BusyReason::NotResponding)
                            } else {
                                CCSError::ServerMissing
                            }
                        }
                    },
                )?;
            verify_handshake(&client_handshake, &server_handshake, sockaddr, &mut stream)?;

            let command = match request {
                ConnectRequest::Shutdown => return Ok(ConnectResponse::ShutdownAck),
                ConnectRequest::Command(command) => command,
            };

            let send_result = flow_parser::loc::with_full_source_serde(|| {
                bincode::serde::encode_into_std_write(
                    command,
                    &mut stream,
                    bincode::config::legacy(),
                )
            });
            if let Err(_e) = send_result {
                close_connection(sockaddr);
                return if server_exists(flowconfig_name, tmp_dir, root) {
                    Err(CCSError::ServerBusy(BusyReason::NotResponding))
                } else {
                    Err(CCSError::ServerMissing)
                };
            }

            loop {
                let recv: Result<MonitorToClientMessage, _> =
                    flow_parser::loc::with_full_source_serde(|| {
                        bincode::serde::decode_from_std_read(&mut stream, bincode::config::legacy())
                    });
                match recv {
                    Ok(MonitorToClientMessage::PleaseHold(..)) => continue,
                    Ok(MonitorToClientMessage::Data(response)) => {
                        return Ok(ConnectResponse::Data(response));
                    }
                    Ok(MonitorToClientMessage::ServerException(message)) => {
                        return Ok(ConnectResponse::ServerException(message));
                    }
                    Err(_e) => {
                        close_connection(sockaddr);
                        return if server_exists(flowconfig_name, tmp_dir, root) {
                            Err(CCSError::ServerBusy(BusyReason::NotResponding))
                        } else {
                            Err(CCSError::ServerMissing)
                        };
                    }
                }
            }
        }
    }
}

pub fn busy_reason_to_string(busy_reason: &BusyReason) -> String {
    match busy_reason {
        BusyReason::TooManyClients => "Too_many_clients".to_string(),
        BusyReason::NotResponding => "Not_responding".to_string(),
        BusyReason::FailOnInit(server_status, watcher_status) => {
            format!(
                "Fail_on_init(server_status={},watcher_status={})",
                server_status::string_of_status(false, false, server_status),
                file_watcher_status::string_of_status(watcher_status),
            )
        }
    }
}

pub fn error_to_string(error: &CCSError) -> String {
    match error {
        CCSError::BuildIdMismatch(MismatchBehavior::ServerExited) => {
            "Build_id_mismatch(Server_exited)".to_string()
        }
        CCSError::BuildIdMismatch(MismatchBehavior::ClientShouldError { .. }) => {
            "Build_id_mismatch(Client_should_error)".to_string()
        }
        CCSError::ServerBusy(busy_reason) => {
            format!("Server_busy({})", busy_reason_to_string(busy_reason))
        }
        CCSError::ServerMissing => "Server_missing".to_string(),
        CCSError::ServerSocketMissing => "Server_socket_missing".to_string(),
    }
}
