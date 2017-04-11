(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type t

val init :
  (Unix.file_descr * Unix.file_descr * Unix.file_descr) ->
  (string * Path.t list)
  -> t
val wait_until_ready : t -> unit
val pid : t -> int
val get_changes : ?timeout:Timeout.t -> t -> SSet.t
