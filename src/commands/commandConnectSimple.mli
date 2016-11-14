(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
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
