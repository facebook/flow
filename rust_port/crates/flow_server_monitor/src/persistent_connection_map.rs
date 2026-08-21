/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// This module keeps track of the persistent connections. Since the Flow server monitor is designed
// to be super thin, the Flow server manages most of the persistent connection state. All the
// monitor really needs to do is keep track of which connection goes with which ID.

use std::collections::BTreeMap;
use std::sync::Arc;
use std::sync::Mutex;

use dupe::IterDupedExt;
use dupe::OptionDupedExt;
use flow_server_env::lsp_prot;

use crate::flow_server_monitor_connection::MonitorPersistentConnection;

static MAP: std::sync::LazyLock<
    Mutex<BTreeMap<lsp_prot::ClientId, Arc<MonitorPersistentConnection>>>,
> = std::sync::LazyLock::new(|| Mutex::new(BTreeMap::new()));

pub fn add(client_id: lsp_prot::ClientId, client: Arc<MonitorPersistentConnection>) {
    MAP.lock().unwrap().insert(client_id, client);
}

pub fn get(client_id: lsp_prot::ClientId) -> Option<Arc<MonitorPersistentConnection>> {
    MAP.lock().unwrap().get(&client_id).duped()
}

// Returns whether the client was still registered.
pub fn remove(client_id: lsp_prot::ClientId) -> bool {
    MAP.lock().unwrap().remove(&client_id).is_some()
}

pub fn contains(client_id: lsp_prot::ClientId) -> bool {
    MAP.lock().unwrap().contains_key(&client_id)
}

// Atomically empties the map, so no client id survives its connection.
pub fn take_all() -> Vec<Arc<MonitorPersistentConnection>> {
    std::mem::take(&mut *MAP.lock().unwrap())
        .into_values()
        .collect()
}

pub fn cardinal() -> usize {
    MAP.lock().unwrap().len()
}

pub fn get_all_clients() -> Vec<Arc<MonitorPersistentConnection>> {
    MAP.lock().unwrap().values().duped().collect()
}
