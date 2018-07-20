(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val handle_ephemeral:
  ServerEnv.genv ->
  ServerEnv.env ->
  MonitorProt.request_id * ServerProt.Request.command_with_context ->
  ServerEnv.env Lwt.t

val handle_persistent:
  ServerEnv.genv ->
  ServerEnv.env ->
  Persistent_connection_prot.client_id ->
  Persistent_connection_prot.request ->
  ServerEnv.env Lwt.t
