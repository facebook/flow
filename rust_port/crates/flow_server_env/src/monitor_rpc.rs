/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Mutex;

use crate::lsp_prot;
use crate::monitor_prot;
use crate::server_prot::response;
use crate::server_status;

#[derive(Debug)]
pub enum MonitorError {
    MonitorDied,
    Disabled,
    IpcNotPorted,
}

type RawFd = std::os::raw::c_int;

pub type Channels = (RawFd, RawFd);

enum State {
    Uninitialized,
    Initialized { infd: RawFd, outfd: RawFd },
    Disabled,
}

static STATE: Mutex<State> = Mutex::new(State::Uninitialized);

fn with_channel<T>(
    select_channel: impl FnOnce((RawFd, RawFd)) -> RawFd,
    on_disabled: impl FnOnce() -> T,
    f: impl FnOnce(RawFd) -> T,
) -> T {
    let state = STATE.lock().unwrap();
    match *state {
        // Probably means someone is calling this module from a worker thread
        State::Uninitialized => {
            panic!("MonitorRPC can only be used by the master thread");
        }
        // Probably means that this is a `flow check` and there is no server monitor
        State::Disabled => on_disabled(),
        State::Initialized { infd, outfd } => f(select_channel((infd, outfd))),
    }
}

fn with_infd<T>(on_disabled: impl FnOnce() -> T, f: impl FnOnce(RawFd) -> T) -> T {
    with_channel(|(infd, _outfd)| infd, on_disabled, f)
}

fn with_outfd<T>(on_disabled: impl FnOnce() -> T, f: impl FnOnce(RawFd) -> T) -> T {
    with_channel(|(_infd, outfd)| outfd, on_disabled, f)
}

// The main server process will initialize this with the channels to the monitor process
pub fn init(channels: Channels) {
    let (infd, outfd) = channels;
    let mut state = STATE.lock().unwrap();
    *state = State::Initialized { infd, outfd };
}

// If there is no monitor process (like in `flow check`), we can disable MonitorRPC
pub fn disable() {
    let mut state = STATE.lock().unwrap();
    *state = State::Disabled;
}

// Read a single message from the monitor.
//
// The active Rust CLI path does not initialize real monitor IPC yet, so this
// still reports `IpcNotPorted` whenever the state is `Initialized`.
pub fn read() -> Result<monitor_prot::MonitorToServerMessage, MonitorError> {
    with_infd(
        || Err(MonitorError::Disabled),
        |_infd| Err(MonitorError::IpcNotPorted),
    )
}

// Sends a message to the monitor.
//
// This is a no-op if the MonitorRPC is disabled. This allows the server to
// stream things like status updates without worrying whether or not there is a
// monitor
//
// Unlike read, this is synchronous. We don't currently have a use case for
// async sends, and it's a little painful to thread lwt through to everywhere
// we send data
fn send(_msg: monitor_prot::ServerToMonitorMessage) {
    with_outfd(
        || {},
        |_outfd| {
            // We still don't have the OCaml monitor Marshal/preamble transport
            // wired up on the active Rust path.
            log::warn!("MonitorRPC.send called in Initialized state but IPC is not ported");
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
    static LAST_STATUS: Mutex<server_status::Status> = Mutex::new(server_status::INITIAL_STATUS);

    // Remember the last status so that we only send updates when something changes
    {
        let state = STATE.lock().unwrap();
        if !matches!(*state, State::Initialized { .. }) {
            return;
        }
    }
    let mut last_status = LAST_STATUS.lock().unwrap();
    let new_status = server_status::update(&event, &last_status);
    if new_status != *last_status {
        *last_status = new_status.clone();
        send(monitor_prot::ServerToMonitorMessage::StatusUpdate(
            new_status,
        ));
    }
}
