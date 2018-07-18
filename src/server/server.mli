(**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val check_once :
  shared_mem_config:SharedMem_js.config ->
  client_include_warnings:bool ->
  ?focus_targets:Utils_js.FilenameSet.t ->
  Options.t ->
  Profiling_js.finished *
    Errors.ErrorSet.t * (* errors *)
    Errors.ErrorSet.t * (* warnings *)
    (Errors.error * Utils_js.LocSet.t) list (* suppressed errors *)

val daemonize :
  log_file:string ->
  shared_mem_config:SharedMem_js.config ->
  argv:string array ->
  file_watcher_pid: int option ->
  Options.t ->
  (MonitorProt.server_to_monitor_message, MonitorProt.monitor_to_server_message) Daemon.handle
