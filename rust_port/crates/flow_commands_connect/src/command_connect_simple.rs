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

use flow_server_env::file_watcher_status;
use flow_server_env::server_status;
use flow_server_env::socket_handshake;
use flow_server_files::server_files_js;

// Same value as `flow_server_monitor::flow_server_monitor_connection::MAX_FRAME_BYTES`. The
// monitor caps incoming bincode frames here to defend against pathologically-encoded length
// fields; the LSP / `flow_cli` side mirrors the cap so the two ends stay symmetric.
//
// bincode 2.x's `with_limit::<N>()` is a const-generic over `usize`, so this constant is `usize`
// (the monitor declares its own copy as `u64` because bincode 1.x's `with_limit` took a runtime
// `u64` — the byte value is the same).
pub const MAX_FRAME_BYTES: usize = 64 * 1024 * 1024;

#[derive(Debug)]
pub enum BusyReason {
    TooManyClients,
    NotResponding,
    FailOnInit(server_status::Status, file_watcher_status::Status),
}

#[derive(Debug)]
pub enum MismatchBehavior {
    // The server exited due to the build id mismatch
    ServerExited,
    // The server is still alive but the client should error
    ClientShouldError {
        server_bin: String,
        server_version: String,
    },
}

#[derive(Debug)]
pub enum CCSError {
    BuildIdMismatch(MismatchBehavior),
    ServerBusy(BusyReason),
    ServerMissing,
    ServerSocketMissing,
}

// pre-server-monitor versions used a different socket

enum ConnectExn {
    Timeout,
    MissingSocket,
}

struct MonitorMisbehaved;

pub fn server_exists(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> bool {
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
    //   (client1_json, json_bytes(client2))
    // - client1 is JSON-encoded so the monitor can read it before deciding
    //   whether the client/server build ids agree.
    // - client2 is JSON-encoded too: it embeds `ClientType::Persistent {
    //   lsp_init_params: InitializeParams }`, and `lsp_types::InitializeParams`
    //   uses `#[serde(flatten)]`, which bincode rejects with
    //   `Serde(SequenceMustHaveLength)`.
    let (ref client1, ref client2) = *client_handshake;
    let client1_json =
        serde_json::to_string(&socket_handshake::client_to_monitor_1_to_json(client1))
            .map_err(|_| ConnectExn::Timeout)?;
    let client2_bytes = serde_json::to_vec(client2).map_err(|_| ConnectExn::Timeout)?;
    let wire: socket_handshake::ClientHandshakeWire = (client1_json, client2_bytes);
    {
        use std::io::Write;
        let mut buffered = std::io::BufWriter::new(&mut conn);
        bincode::serde::encode_into_std_write(
            &wire,
            &mut buffered,
            bincode::config::legacy().with_limit::<MAX_FRAME_BYTES>(),
        )
        .map_err(|_| ConnectExn::Timeout)?;
        buffered.flush().map_err(|_| ConnectExn::Timeout)?;
    }
    let conn_clone = conn.try_clone().map_err(|_| ConnectExn::Timeout)?;
    with_connections(|conns| {
        conns.insert(sockaddr, conn_clone);
    });
    Ok(conn)
}

pub fn close_connection(sockaddr: SocketAddr) {
    with_connections(|conns| {
        if let Some(stream) = conns.remove(&sockaddr) {
            // OCaml: try Timeout.shutdown_connection ic with | _ -> ()
            // Errors from shutdown are intentionally ignored.
            match stream.shutdown(std::net::Shutdown::Both) {
                Ok(()) | Err(_) => (),
            }
        }
    });
}

fn establish_connection(
    flowconfig_name: &str,
    timeout: u64,
    client_handshake: &socket_handshake::ClientHandshake,
    tmp_dir: &str,
    root: &Path,
) -> Result<(SocketAddr, TcpStream), ConnectExn> {
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
    let stream = open_connection(timeout, client_handshake, sockaddr)?;

    stream
        .set_read_timeout(Some(std::time::Duration::from_secs(timeout)))
        .map_err(|_| ConnectExn::Timeout)?;
    stream
        .set_write_timeout(Some(std::time::Duration::from_secs(10)))
        .map_err(|_| ConnectExn::Timeout)?;

    Ok((sockaddr, stream))
}

fn get_handshake(
    timeout: u64,
    sockaddr: SocketAddr,
    stream: &mut TcpStream,
) -> Result<(SocketAddr, socket_handshake::ServerHandshake), ConnectExn> {
    stream
        .set_read_timeout(Some(std::time::Duration::from_secs(timeout)))
        .map_err(|_| ConnectExn::Timeout)?;
    let wire: socket_handshake::ServerHandshakeWire = bincode::serde::decode_from_std_read(
        &mut *stream,
        bincode::config::legacy().with_limit::<MAX_FRAME_BYTES>(),
    )
    .map_err(|e| {
        if let bincode::error::DecodeError::Io { inner, .. } = &e {
            if inner.kind() == std::io::ErrorKind::TimedOut {
                // Timeouts are expected
                return ConnectExn::Timeout;
            }
        }
        close_connection(sockaddr);
        ConnectExn::Timeout
    })?;
    let server1_json: serde_json::Value =
        serde_json::from_str(&wire.0).map_err(|_| ConnectExn::Timeout)?;
    let server1 = socket_handshake::json_to_monitor_to_client_1(&server1_json).map_err(|_| {
        close_connection(sockaddr);
        ConnectExn::Timeout
    })?;
    // Server invariant: it only sends us snd=Some if it knows client+server versions match.
    // JSON, not bincode: matches the server-side encode (see `socket_acceptor`).
    let server2: Option<socket_handshake::MonitorToClient2> = match &wire.1 {
        Some(bytes) => match serde_json::from_slice::<socket_handshake::MonitorToClient2>(bytes) {
            Ok(s) => Some(s),
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
) -> Result<Result<(), CCSError>, MonitorMisbehaved> {
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
            match stream.shutdown(std::net::Shutdown::Both) {
                Ok(()) | Err(_) => (),
            }
        }
    }
    // Next, let's interpret the server's response into our own response code
    match (&server1.server_intent, server2) {
        (
            socket_handshake::ServerIntent::ServerWillContinue,
            Some(socket_handshake::MonitorToClient2::ServerReady),
        ) => Ok(Ok(())),
        (
            socket_handshake::ServerIntent::ServerWillContinue,
            Some(socket_handshake::MonitorToClient2::ServerStillInitializing(..)),
        ) => Ok(Ok(())),
        (
            socket_handshake::ServerIntent::ServerWillHangup,
            Some(socket_handshake::MonitorToClient2::ServerHasTooManyClients),
        ) => Ok(Err(CCSError::ServerBusy(BusyReason::TooManyClients))),
        (
            socket_handshake::ServerIntent::ServerWillHangup,
            Some(socket_handshake::MonitorToClient2::ServerStillInitializing(
                server_status,
                watcher_status,
            )),
        ) => Ok(Err(CCSError::ServerBusy(BusyReason::FailOnInit(
            server_status.clone(),
            watcher_status.clone(),
        )))),
        (socket_handshake::ServerIntent::ServerWillHangup, None) => {
            if client1.client_build_id != server1.server_build_id {
                Ok(Err(CCSError::BuildIdMismatch(
                    MismatchBehavior::ClientShouldError {
                        server_bin: server1.server_bin.clone(),
                        server_version: server1.server_version.clone(),
                    },
                )))
            } else {
                Err(MonitorMisbehaved)
            }
        }
        (socket_handshake::ServerIntent::ServerWillExit, None) => {
            if client1.is_stop_request {
                Ok(Ok(()))
            } else {
                // either the build ids were different, or client1 wasn't valid for server
                Ok(Err(CCSError::BuildIdMismatch(
                    MismatchBehavior::ServerExited,
                )))
            }
        }
        _ => Err(MonitorMisbehaved),
    }
}

// Connects to the monitor via a socket.
pub fn connect_once(
    flowconfig_name: &str,
    client_handshake: &socket_handshake::ClientHandshake,
    tmp_dir: &str,
    root: &Path,
) -> Result<(SocketAddr, TcpStream), CCSError> {
    enum BroadCatch {
        MissingSocket,
        TimeoutOrOther,
    }
    let inner = || -> Result<Result<(SocketAddr, TcpStream), CCSError>, BroadCatch> {
        let (sockaddr, mut stream) =
            establish_connection(flowconfig_name, 1, client_handshake, tmp_dir, root).map_err(
                |e| match e {
                    ConnectExn::MissingSocket => BroadCatch::MissingSocket,
                    ConnectExn::Timeout => BroadCatch::TimeoutOrOther,
                },
            )?;
        let (sockaddr, server_handshake) =
            get_handshake(1, sockaddr, &mut stream).map_err(|e| match e {
                ConnectExn::MissingSocket => BroadCatch::MissingSocket,
                ConnectExn::Timeout => BroadCatch::TimeoutOrOther,
            })?;
        match verify_handshake(client_handshake, &server_handshake, sockaddr, &mut stream) {
            Ok(Ok(())) => Ok(Ok((sockaddr, stream))),
            Ok(Err(ccs_err)) => Ok(Err(ccs_err)),
            // OCaml: `failwith ...` raised inside `verify_handshake`, caught by `| _ ->`.
            Err(MonitorMisbehaved) => Err(BroadCatch::TimeoutOrOther),
        }
    };
    match inner() {
        Ok(Ok(conn)) => Ok(conn),
        Ok(Err(ccs_err)) => Err(ccs_err),
        Err(BroadCatch::MissingSocket) => {
            if server_exists(flowconfig_name, tmp_dir, root) {
                Err(CCSError::ServerSocketMissing)
            } else {
                Err(CCSError::ServerMissing)
            }
        }
        Err(BroadCatch::TimeoutOrOther) => {
            if server_exists(flowconfig_name, tmp_dir, root) {
                Err(CCSError::ServerBusy(BusyReason::NotResponding))
            } else {
                Err(CCSError::ServerMissing)
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
