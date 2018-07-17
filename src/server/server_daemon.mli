(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)
type entry_point

val register_entry_point :
  (monitor_channels:MonitorRPC.channels ->
    shared_mem_config:SharedMem_js.config ->
    Options.t ->
    unit) ->
  entry_point

val open_log_file: string -> Unix.file_descr

val daemonize :
  log_file:string ->
  shared_mem_config:SharedMem_js.config ->
  argv: string array ->
  options:Options.t ->
  file_watcher_pid: int option ->
  entry_point ->
  (MonitorProt.server_to_monitor_message, MonitorProt.monitor_to_server_message) Daemon.handle
