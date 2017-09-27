(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type daemon_msg =
  | Starting
  | Ready

type waiting_channel
type entry_point

val register_entry_point :
  (?waiting_channel:waiting_channel ->
    shared_mem_config:SharedMem_js.config ->
    Options.t ->
    unit) ->
  entry_point

val daemonize :
  wait:bool ->
  log_file:string ->
  shared_mem_config:SharedMem_js.config ->
  options:Options.t ->
  ?on_spawn:(int -> unit) ->
  entry_point ->
  unit

val wakeup_client : waiting_channel option -> daemon_msg -> unit
val close_waiting_channel : waiting_channel option -> unit
