/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Port of commandConnect.ml + commandConnectSimple.ml (simplified)
// The OCaml version has a complex retry/autostart/handshake system. This Rust version
// is a simplified direct connection without handshake or autostart. The OCaml architecture
// uses Marshal for serialization and a SocketHandshake protocol; we use JSON + length-prefix.

use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use std::path::Path;
use std::time::Duration;

pub(crate) use flow_server_env::server_prot::response::LazyStats;
use flow_server_files::server_files_js;
use serde::Deserialize;
use serde::Serialize;

#[derive(Debug, Serialize, Deserialize)]
pub(crate) enum SaveStateOut {
    File(String),
    Scm,
}

/// Requests sent from client to server.
/// Simplified wire types for the subset of ServerProt.Request.Command used by the Rust CLI.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) enum ServerRequest {
    CheckContents {
        filename: Option<String>,
        content: String,
        force: bool,
        include_warnings: bool,
        strip_root: bool,
    },
    Status {
        include_warnings: bool,
        strip_root: bool,
    },
    ForceRecheck {
        files: Vec<String>,
        focus: bool,
        missed_changes: bool,
        changed_mergebase: bool,
    },
    SaveState {
        out: SaveStateOut,
    },
    Shutdown,
}

/// Responses sent from server to client.
/// Simplified wire types for the subset of ServerProt.Response.Response used by the Rust CLI.
#[derive(Debug, Serialize, Deserialize)]
pub(crate) enum ServerResponse {
    CheckContents {
        has_errors: bool,
        warning_count: usize,
        error_output: String,
        not_covered: bool,
    },
    Status {
        has_errors: bool,
        error_count: usize,
        warning_count: usize,
        error_output: String,
        lazy_stats: LazyStats,
    },
    ForceRecheck,
    SaveState {
        result: Result<String, String>,
    },
    Error {
        message: String,
    },
}

/// Send a message over a stream using 4-byte big-endian length prefix + JSON.
pub(crate) fn send_message<W: Write, T: Serialize>(writer: &mut W, msg: &T) -> std::io::Result<()> {
    let json = serde_json::to_vec(msg)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
    let len = json.len() as u32;
    writer.write_all(&len.to_be_bytes())?;
    writer.write_all(&json)?;
    writer.flush()
}

/// Receive a message from a stream using 4-byte big-endian length prefix + JSON.
pub(crate) fn receive_message<R: Read, T: for<'de> Deserialize<'de>>(
    reader: &mut R,
) -> std::io::Result<T> {
    let mut len_buf = [0u8; 4];
    reader.read_exact(&mut len_buf)?;
    let len = u32::from_be_bytes(len_buf) as usize;
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf)?;
    serde_json::from_slice(&buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
}

pub(crate) enum ConnectError {
    ServerNotRunning,
    ServerSocketMissing,
    ConnectionFailed(String),
    CommunicationError(String),
    Timeout,
}

fn io_error_is_server_not_running(kind: std::io::ErrorKind) -> bool {
    matches!(
        kind,
        std::io::ErrorKind::ConnectionRefused
            | std::io::ErrorKind::ConnectionReset
            | std::io::ErrorKind::ConnectionAborted
            | std::io::ErrorKind::BrokenPipe
            | std::io::ErrorKind::UnexpectedEof
            | std::io::ErrorKind::NotConnected
    )
}

impl std::fmt::Display for ConnectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConnectError::ServerNotRunning => write!(f, "There is no Flow server running"),
            ConnectError::ServerSocketMissing => {
                write!(f, "The flow server lock exists but the socket is missing")
            }
            ConnectError::ConnectionFailed(msg) => {
                write!(f, "Could not connect to server: {}", msg)
            }
            ConnectError::CommunicationError(msg) => write!(f, "Communication error: {}", msg),
            ConnectError::Timeout => write!(f, "Timed out waiting for server response"),
        }
    }
}

fn server_exists(flowconfig_name: &str, tmp_dir: &str, root: &Path) -> bool {
    let lock_path = server_files_js::lock_file(flowconfig_name, tmp_dir, root);
    Path::new(&lock_path).exists()
}

pub(crate) fn remove_server_files(flowconfig_name: &str, tmp_dir: &str, root: &Path) {
    let lock_path = server_files_js::lock_file(flowconfig_name, tmp_dir, root);
    let socket_path = server_files_js::socket_file(flowconfig_name, tmp_dir, root);
    let ready_path = server_files_js::ready_file(flowconfig_name, tmp_dir, root);
    let _ = std::fs::remove_file(lock_path);
    let _ = std::fs::remove_file(socket_path);
    let _ = std::fs::remove_file(ready_path);
}

/// Connect to the Flow server and send a request, returning the response.
pub(crate) fn connect_and_make_request(
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &Path,
    request: ServerRequest,
) -> Result<ServerResponse, ConnectError> {
    connect_and_make_request_with_timeout(flowconfig_name, tmp_dir, root, request, None)
}

/// Connect to the Flow server and send a request, returning the response.
/// If `timeout_secs` is Some, the read timeout is set to that value.
pub(crate) fn connect_and_make_request_with_timeout(
    flowconfig_name: &str,
    tmp_dir: &str,
    root: &Path,
    request: ServerRequest,
    timeout_secs: Option<u64>,
) -> Result<ServerResponse, ConnectError> {
    let socket_path = server_files_js::socket_file(flowconfig_name, tmp_dir, root);
    let server_exists = server_exists(flowconfig_name, tmp_dir, root);

    // Read the port from the socket file (written by the server on startup)
    let port_str = std::fs::read_to_string(&socket_path).map_err(|e| {
        if e.kind() == std::io::ErrorKind::NotFound {
            if server_exists {
                ConnectError::ServerSocketMissing
            } else {
                ConnectError::ServerNotRunning
            }
        } else {
            ConnectError::ConnectionFailed(e.to_string())
        }
    })?;
    let port: u16 = port_str.trim().parse().map_err(|_| {
        if server_exists {
            ConnectError::ServerSocketMissing
        } else {
            ConnectError::ConnectionFailed("invalid port in socket file".to_string())
        }
    })?;

    // Connect via TCP to localhost
    let mut stream = TcpStream::connect(("127.0.0.1", port)).map_err(|e| {
        if io_error_is_server_not_running(e.kind()) {
            if server_exists {
                ConnectError::ServerSocketMissing
            } else {
                ConnectError::ServerNotRunning
            }
        } else {
            ConnectError::ConnectionFailed(e.to_string())
        }
    })?;

    // Set timeout
    let read_timeout = timeout_secs.unwrap_or(60);
    stream
        .set_read_timeout(Some(Duration::from_secs(read_timeout)))
        .ok();
    stream.set_write_timeout(Some(Duration::from_secs(10))).ok();

    // Send request
    send_message(&mut stream, &request).map_err(|e| {
        if io_error_is_server_not_running(e.kind()) {
            if server_exists {
                ConnectError::ServerSocketMissing
            } else {
                ConnectError::ServerNotRunning
            }
        } else {
            ConnectError::CommunicationError(e.to_string())
        }
    })?;

    // Receive response
    receive_message(&mut stream).map_err(|e| {
        if e.kind() == std::io::ErrorKind::WouldBlock || e.kind() == std::io::ErrorKind::TimedOut {
            ConnectError::Timeout
        } else if io_error_is_server_not_running(e.kind()) {
            if server_exists {
                ConnectError::ServerSocketMissing
            } else {
                ConnectError::ServerNotRunning
            }
        } else {
            ConnectError::CommunicationError(e.to_string())
        }
    })
}
