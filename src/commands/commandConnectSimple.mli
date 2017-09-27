(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type error =
  | Build_id_mismatch
  | Server_busy
  | Server_gcollecting
  | Server_initializing
  | Server_missing
  | Server_rechecking

val server_exists : tmp_dir:string -> Path.t -> bool

val connect_once : tmp_dir:string -> Path.t -> (Timeout.in_channel * out_channel, error) Result.t
