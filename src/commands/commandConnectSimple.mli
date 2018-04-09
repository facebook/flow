(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

 type busy_reason =
 | Too_many_clients
 | Not_responding
 | Fail_on_init

type error =
  | Build_id_mismatch
  | Server_busy of busy_reason
  | Server_missing
  | Server_socket_missing

val server_exists : tmp_dir:string -> Path.t -> bool

val connect_once :
  client_type: SocketHandshake.client_type ->
  tmp_dir:string ->
  Path.t ->
  (Timeout.in_channel * out_channel, error) result
