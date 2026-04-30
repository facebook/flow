/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use flow_utils_concurrency::worker_cancel;

use crate::monitor_prot::MonitorToServerMessage;
use crate::monitor_prot::PleaseDieReason;
use crate::monitor_rpc;
use crate::persistent_connection;
use crate::server_env::Genv;
use crate::server_monitor_listener_state;

pub struct CommandHandlerCallbacks {
    pub enqueue_or_handle_ephemeral: fn(
        &Arc<Genv>,
        (
            crate::monitor_prot::RequestId,
            crate::server_command_with_context::ServerCommandWithContext,
        ),
    ),
    pub enqueue_persistent:
        fn(&Arc<Genv>, crate::lsp_prot::ClientId, crate::lsp_prot::RequestWithMetadata),
}

// TODO: find a way to gracefully kill the workers. At the moment, if the
// workers are in the middle of a job this will lead to some log spew. We
// probably should send SIGTERM to each worker and set up a signal handler to
// kill the fork and exit gracefully. Might also want to use the
// SharedMem_js.cancel thingy.
fn kill_workers() {
    flow_hh_logger::info!("Killing the worker processes");
    worker_cancel::stop_workers();
}

fn handle_message(
    genv: &Arc<Genv>,
    callbacks: &CommandHandlerCallbacks,
    message: MonitorToServerMessage,
) {
    match message {
        MonitorToServerMessage::Request(request_id, command) => {
            (callbacks.enqueue_or_handle_ephemeral)(genv, (request_id, command));
        }
        MonitorToServerMessage::PersistentConnectionRequest(client_id, request) => {
            (callbacks.enqueue_persistent)(genv, client_id, request);
        }
        MonitorToServerMessage::NewPersistentConnection(client_id, lsp_init_params) => {
            // Immediately register the new client
            persistent_connection::add_client(client_id, lsp_init_params);
            server_monitor_listener_state::push_new_env_update(Box::new(move |mut env| {
                env.connections =
                    persistent_connection::add_client_to_clients(env.connections, client_id);
                env
            }));
        }
        MonitorToServerMessage::DeadPersistentConnection(client_id) => {
            // Immediately remove the dead client
            persistent_connection::remove_client(client_id);
            server_monitor_listener_state::push_new_env_update(Box::new(move |mut env| {
                env.connections =
                    persistent_connection::remove_client_from_clients(env.connections, client_id);
                env
            }));
        }
        MonitorToServerMessage::FileWatcherNotification {
            files: changed_files,
            metadata,
            initial,
        } => {
            if initial {
                server_monitor_listener_state::push_lazy_init(metadata, changed_files);
            } else {
                match metadata {
                    Some(metadata) => {
                        server_monitor_listener_state::push_files_to_recheck_with_metadata(
                            Some(metadata),
                            changed_files,
                        );
                    }
                    None => {
                        server_monitor_listener_state::push_files_to_recheck(changed_files);
                    }
                }
            }
        }
        MonitorToServerMessage::PleaseDie(please_die_reason) => {
            kill_workers();
            let msg = match please_die_reason {
                PleaseDieReason::MonitorExiting(monitor_exit_status, monitor_msg) => {
                    format!(
                        "Monitor is exiting with status {:?} ({})",
                        monitor_exit_status, monitor_msg
                    )
                }
            };
            flow_hh_logger::info!("{}", msg);
            flow_common_exit_status::exit(flow_common_exit_status::FlowExitStatus::KilledByMonitor);
        }
    }
}

// This thread keeps reading messages from the monitor process and adding them
// to a stream. It runs in parallel with the server loop that consumes it.
pub fn listen_for_messages(genv: &Arc<Genv>, callbacks: &CommandHandlerCallbacks) -> ! {
    loop {
        // Read a message from the monitor.
        let message = match monitor_rpc::read() {
            Ok(msg) => msg,
            Err(_) => {
                kill_workers();
                let msg = "Connection to monitor closed unexpectedly";
                flow_hh_logger::info!("{}", msg);
                flow_common_exit_status::exit(
                    flow_common_exit_status::FlowExitStatus::KilledByMonitor,
                );
            }
        };
        handle_message(genv, callbacks, message);
    }
}
