(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val send_request :
  client:EphemeralConnection.t -> request:ServerProt.Request.command_with_context -> unit

val send_persistent_request :
  client_id:LspProt.client_id -> request:LspProt.request_with_metadata -> unit

val notify_new_persistent_connection :
  client_id:LspProt.client_id -> lsp_init_params:Lsp.Initialize.params -> unit

val notify_dead_persistent_connection : client_id:LspProt.client_id -> unit

val start : FlowServerMonitorOptions.t -> unit Lwt.t

val exit : msg:string -> FlowExitStatus.t -> 'a Lwt.t
