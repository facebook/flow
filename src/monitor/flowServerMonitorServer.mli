(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val send_request :
  client:EphemeralConnection.t -> request:ServerProt.Request.command_with_context -> unit

val send_persistent_request :
  client_id:Persistent_connection_prot.client_id ->
  request:Persistent_connection_prot.request ->
  unit

val notify_new_persistent_connection :
  client_id:Persistent_connection_prot.client_id ->
  logging_context:FlowEventLogger.logging_context ->
  lsp:Lsp.Initialize.params option ->
  unit

val notify_dead_persistent_connection : client_id:Persistent_connection_prot.client_id -> unit

val start : FlowServerMonitorOptions.t -> unit Lwt.t

val exit : msg:string -> FlowExitStatus.t -> 'a Lwt.t
