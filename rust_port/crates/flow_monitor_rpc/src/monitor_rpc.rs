/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::io;
use std::io::Read;
use std::io::Write;
use std::net::TcpStream;
use std::sync::Arc;
use std::sync::Mutex;

use crate::lsp_prot;
use crate::monitor_prot;
use crate::server_prot::response;
use crate::server_status;

#[derive(Debug)]
pub enum MonitorError {
    MonitorDied,
    Disabled,
}

// The monitor<->server channel pair. Each `TcpStream` is one direction of
// the cross-platform daemon transport set up in `flow_daemon::spawn`. We use
// `TcpStream` rather than `std::fs::File` so that this type compiles on both
// Unix and Windows -- on Windows, sockets are not files and cannot be
// converted to `File` via `OwnedFd`.
pub type Channels = (TcpStream, TcpStream);

enum State {
    Uninitialized,
    Initialized {
        infd: Arc<Mutex<TcpStream>>,
        outfd: Arc<Mutex<TcpStream>>,
    },
    Disabled,
}

static STATE: Mutex<State> = Mutex::new(State::Uninitialized);

fn with_channel<T>(
    select_channel: impl for<'a> FnOnce(
        &'a Arc<Mutex<TcpStream>>,
        &'a Arc<Mutex<TcpStream>>,
    ) -> &'a Arc<Mutex<TcpStream>>,
    on_disabled: impl FnOnce() -> T,
    f: impl FnOnce(&mut TcpStream) -> T,
) -> T {
    let channel = {
        let state = STATE.lock().unwrap();
        match &*state {
            // Probably means someone is calling this module from a worker thread
            State::Uninitialized => {
                panic!("MonitorRPC can only be used by the master thread");
            }
            // Probably means that this is a `flow check` and there is no server monitor
            State::Disabled => return on_disabled(),
            State::Initialized { infd, outfd } => select_channel(infd, outfd).clone(),
        }
    };
    let mut channel = channel.lock().unwrap();
    f(&mut channel)
}

fn with_infd<T>(on_disabled: impl FnOnce() -> T, f: impl FnOnce(&mut TcpStream) -> T) -> T {
    with_channel(|infd, _outfd| infd, on_disabled, f)
}

fn with_outfd<T>(on_disabled: impl FnOnce() -> T, f: impl FnOnce(&mut TcpStream) -> T) -> T {
    with_channel(|_infd, outfd| outfd, on_disabled, f)
}

// The main server process will initialize this with the channels to the monitor process
pub fn init(channels: Channels) {
    let (infd, outfd) = channels;
    let mut state = STATE.lock().unwrap();
    *state = State::Initialized {
        infd: Arc::new(Mutex::new(infd)),
        outfd: Arc::new(Mutex::new(outfd)),
    };
}

// If there is no monitor process (like in `flow check`), we can disable MonitorRPC
pub fn disable() {
    let mut state = STATE.lock().unwrap();
    *state = State::Disabled;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StateKind {
    Uninitialized,
    Initialized,
    Disabled,
}

pub fn state() -> StateKind {
    let state = STATE.lock().unwrap();
    match &*state {
        State::Uninitialized => StateKind::Uninitialized,
        State::Initialized { .. } => StateKind::Initialized,
        State::Disabled => StateKind::Disabled,
    }
}

// Cap any single inbound frame at 64 MiB. Mirrors `MAX_MESSAGE_BYTES` in
// `flow_server_env::server_socket_rpc` (we can't share the constant because
// `flow_monitor_rpc` is a lower layer than `flow_server_env`).
const MAX_FRAME_BYTES: usize = 64 * 1024 * 1024;

// Read a single message from the monitor.
//
// Length-prefixed JSON, not bincode: `MonitorToServerMessage` /
// `ServerToMonitorMessage` carry `RequestWithMetadata` which embeds
// `lsp_types` request/response structs that use `#[serde(flatten)]`.
// bincode rejects flatten with `Serde(SequenceMustHaveLength)`.
pub fn read() -> Result<monitor_prot::MonitorToServerMessage, MonitorError> {
    with_infd(
        || Err(MonitorError::Disabled),
        |infd| {
            flow_parser::loc::with_full_source_serde(|| {
                receive_message::<_, monitor_prot::MonitorToServerMessage>(infd)
                    .map_err(|_| MonitorError::MonitorDied)
            })
        },
    )
}

fn receive_message<R: Read, T: for<'de> serde::Deserialize<'de>>(
    reader: &mut R,
) -> std::io::Result<T> {
    let mut len_buf = [0u8; 4];
    reader.read_exact(&mut len_buf)?;
    let len = u32::from_be_bytes(len_buf) as usize;
    if len > MAX_FRAME_BYTES {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!(
                "RPC frame too large: {} bytes exceeds limit {}",
                len, MAX_FRAME_BYTES
            ),
        ));
    }
    let mut buf = vec![0u8; len];
    reader.read_exact(&mut buf)?;
    serde_json::from_slice(&buf)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))
}

fn send_message<W: Write, T: serde::Serialize>(writer: &mut W, msg: &T) -> std::io::Result<()> {
    let json = serde_json::to_vec(msg)
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::InvalidData, e))?;
    let len = json.len() as u32;
    writer.write_all(&len.to_be_bytes())?;
    writer.write_all(&json)?;
    writer.flush()
}

// Sends a message to the monitor.
//
// This is a no-op if the MonitorRPC is disabled. This allows the server to stream things like
// status updates without worrying whether or not there is a monitor
//
// Unliked read, this is synchronous. We don't currently have a use case for async sends, and it's a
// little painful to thread lwt through to everywhere we send data
fn send(msg: monitor_prot::ServerToMonitorMessage) {
    with_outfd(
        || {},
        |outfd| {
            if let Err(e) = flow_parser::loc::with_full_source_serde(|| send_message(outfd, &msg)) {
                if e.kind() == io::ErrorKind::BrokenPipe {
                    panic!("Monitor_died (EPIPE)");
                } else {
                    log::error!("MonitorRPC.send: write failed: {}", e);
                }
            }
        },
    );
}

// Respond to a request from an ephemeral client
pub fn respond_to_request(request_id: monitor_prot::RequestId, response: response::Response) {
    send(monitor_prot::ServerToMonitorMessage::Response(
        request_id, response,
    ));
}

// Exception while handling the request
pub fn request_failed(request_id: monitor_prot::RequestId, exn_str: String) {
    send(monitor_prot::ServerToMonitorMessage::RequestFailed(
        request_id, exn_str,
    ));
}

// Send a message to a persistent client
pub fn respond_to_persistent_connection(
    client_id: lsp_prot::ClientId,
    response: lsp_prot::MessageFromServer,
) {
    send(monitor_prot::ServerToMonitorMessage::PersistentConnectionResponse(client_id, response));
}

pub fn send_telemetry(t: lsp_prot::TelemetryFromServer) {
    send(monitor_prot::ServerToMonitorMessage::Telemetry(t));
}

// Send a status update to the monitor
pub fn status_update(event: server_status::Event) {
    // Remember the last status so that we only send updates when something changes
    static LAST_STATUS: Mutex<server_status::Status> = Mutex::new(server_status::INITIAL_STATUS);

    {
        let state = STATE.lock().unwrap();
        if !matches!(*state, State::Initialized { .. }) {
            return;
        }
    }
    let new_status = {
        let mut last_status = LAST_STATUS.lock().unwrap();
        let new_status = server_status::update(&event, &last_status);
        if new_status != *last_status {
            *last_status = new_status.clone();
            Some(new_status)
        } else {
            None
        }
    };
    if let Some(new_status) = new_status {
        send(monitor_prot::ServerToMonitorMessage::StatusUpdate(
            new_status,
        ));
    }
}
