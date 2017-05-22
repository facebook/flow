(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

type daemon_msg =
  | Starting
  | Ready

type waiting_channel
type entry_point

val register_entry_point :
  (?waiting_channel:waiting_channel -> Options.t -> unit) ->
  entry_point

val daemonize :
  wait:bool ->
  log_file:string ->
  options:Options.t ->
  ?on_spawn:(int -> unit) ->
  entry_point ->
  unit

val wakeup_client : waiting_channel option -> daemon_msg -> unit
val close_waiting_channel : waiting_channel option -> unit
