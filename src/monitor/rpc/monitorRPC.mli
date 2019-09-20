(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type channels =
  ( MonitorProt.monitor_to_server_message,
    MonitorProt.server_to_monitor_message )
  Daemon.channel_pair

val init : channels:channels -> unit

val disable : unit -> unit

val read : unit -> MonitorProt.monitor_to_server_message Lwt.t

val respond_to_request :
  request_id:MonitorProt.request_id -> response:ServerProt.Response.response -> unit

val request_failed : request_id:MonitorProt.request_id -> exn_str:string -> unit

val respond_to_persistent_connection :
  client_id:LspProt.client_id -> response:LspProt.response -> unit

val status_update : event:ServerStatus.event -> unit
