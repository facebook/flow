(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add :
  request:ServerProt.Request.command_with_context ->
  client:EphemeralConnection.t ->
  MonitorProt.request_id Lwt.t

val remove :
  request_id:MonitorProt.request_id ->
  (ServerProt.Request.command_with_context * EphemeralConnection.t) option Lwt.t

val remove_all :
  unit -> (ServerProt.Request.command_with_context * EphemeralConnection.t) list Lwt.t

val cardinal : unit -> int
