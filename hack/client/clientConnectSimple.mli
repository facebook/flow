(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type error =
  | Server_missing
  | Server_initializing
  | Server_busy
  | Build_id_mismatch

val server_exists : Path.t -> bool

val connect_once : Path.t -> (Timeout.in_channel * out_channel, error) Result.t
