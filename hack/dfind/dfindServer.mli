(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

val run_daemon: Path.t list -> (unit, SSet.t) Daemon.channel_pair -> unit
val entry_point: (Path.t list, unit, SSet.t) Daemon.entry
