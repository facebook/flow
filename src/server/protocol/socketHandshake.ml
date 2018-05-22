(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* When a client connects over the socket, there is a handshake. The client writes a message
 * and the Flow server monitor responds *)

type build_id = string

let build_revision = match Build_id.build_revision with
 | "" -> Flow_version.version
 | x -> x

type client_type =
(* A new ephemeral client (one that sends a request, gets a response and disconnects) *)
| Ephemeral of { fail_on_init: bool }
(* A new persistent client *)
| Persistent of FlowEventLogger.logging_context
(* A new lsp client *)
| PersistentLsp of FlowEventLogger.logging_context * Lsp.Initialize.params
(* A flow stop that wants the Flow server to die *)
| StabbityStabStab

type client_to_monitor = {
  client_build_id: build_id;
  client_type: client_type;
}

type monitor_to_client =
  | Connection_ok
  | Build_id_mismatch of { server_build_id: build_id; server_bin: string }
  | Too_many_clients
  (* The server is still initializing. Sent to lsp clients or clients which used
   * --retry-if-init false *)
  | Still_initializing of (ServerStatus.status * FileWatcherStatus.status)
