(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type busy_reason =
  | Too_many_clients
  | Not_responding
  | Fail_on_init of (ServerStatus.status * FileWatcherStatus.status)

type error =
  | Build_id_mismatch
  | Server_busy of busy_reason
  | Server_missing
  | Server_socket_missing

val server_exists : flowconfig_name:string -> tmp_dir:string -> Path.t -> bool

val connect_once :
  flowconfig_name:string ->
  client_handshake: (SocketHandshake.client_handshake) ->
  tmp_dir:string ->
  Path.t ->
  (Timeout.in_channel * out_channel, error) result

val busy_reason_to_string : busy_reason -> string

val error_to_string : error -> string
