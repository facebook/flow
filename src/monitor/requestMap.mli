(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val add :
  request:ServerCommandWithContext.t -> client:EphemeralConnection.t -> MonitorProt.request_id Lwt.t

val remove :
  request_id:MonitorProt.request_id ->
  (ServerCommandWithContext.t * EphemeralConnection.t) option Lwt.t

val remove_all : unit -> (ServerCommandWithContext.t * EphemeralConnection.t) list Lwt.t

val cardinal : unit -> int
