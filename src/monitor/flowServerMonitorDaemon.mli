(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type start_function = ?waiting_fd:Unix.file_descr -> FlowServerMonitorOptions.t -> unit

type wait_msg =
  | Starting
  | Ready

type entry_point

val register_entry_point : start_function -> entry_point

val daemonize :
  wait:bool ->
  on_spawn:(int -> 'a) ->
  monitor_options:FlowServerMonitorOptions.t ->
  entry_point ->
  unit
