(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val check_once :
  init_id:string ->
  shared_mem_config:SharedMem.config ->
  format_errors:
    (Flow_errors_utils.ConcreteLocPrintableErrorSet.t
     * (* errors *)
       Flow_errors_utils.ConcreteLocPrintableErrorSet.t
     * (* warnings *)
     (Loc.t Flow_errors_utils.printable_error * Loc_collections.LocSet.t) list ->
    (* suppressed errors *) Profiling_js.finished ->
    unit (* print errors *)
    ) ->
  ?focus_targets:Utils_js.FilenameSet.t ->
  Options.t ->
  Flow_errors_utils.ConcreteLocPrintableErrorSet.t
  * (* errors *) Flow_errors_utils.ConcreteLocPrintableErrorSet.t

(* warnings *)

val daemonize :
  init_id:string ->
  log_file:string ->
  shared_mem_config:SharedMem.config ->
  argv:string array ->
  file_watcher_pid:int option ->
  Options.t ->
  (MonitorProt.server_to_monitor_message, MonitorProt.monitor_to_server_message) Daemon.handle
