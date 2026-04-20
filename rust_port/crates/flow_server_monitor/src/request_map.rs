/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// The Flow server monitor sends requests to the server. When the server responds, we need to know
// which client to forward the response to. The RequestMap keeps track of this info.
//
// Every request in this map has been sent to the server and no reply has been processed yet

use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::Mutex;

use flow_server_env::monitor_prot;
use flow_server_env::server_command_with_context::ServerCommandWithContext;

use crate::flow_server_monitor_connection::EphemeralConnection;

struct State {
    map: BTreeMap<String, (ServerCommandWithContext, Arc<EphemeralConnection>)>,
    last_id: i32,
}

static STATE: std::sync::LazyLock<Mutex<State>> = std::sync::LazyLock::new(|| {
    Mutex::new(State {
        map: BTreeMap::new(),
        last_id: 0,
    })
});

pub fn add(
    request: ServerCommandWithContext,
    client: Arc<EphemeralConnection>,
) -> monitor_prot::RequestId {
    let mut state = STATE.lock().unwrap();
    state.last_id += 1;
    let request_id = format!("Request {}", state.last_id);
    state.map.insert(request_id.clone(), (request, client));
    request_id
}

pub fn remove(
    request_id: &monitor_prot::RequestId,
) -> Option<(ServerCommandWithContext, Arc<EphemeralConnection>)> {
    let mut state = STATE.lock().unwrap();
    state.map.remove(request_id)
}

pub fn remove_all() -> Vec<(ServerCommandWithContext, Arc<EphemeralConnection>)> {
    let mut state = STATE.lock().unwrap();
    std::mem::take(&mut state.map).into_values().collect()
}

pub fn cardinal() -> usize {
    STATE.lock().unwrap().map.len()
}
