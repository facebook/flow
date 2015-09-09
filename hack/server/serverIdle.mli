(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* Called whenever the server is idle *)
val go: unit -> unit

val async: (unit -> unit) -> unit

(* Called every time a client connects *)
val stamp_connection: unit -> unit

val init: Path.t -> unit
