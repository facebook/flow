(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val check_once :
  init_id:string ->
  shared_mem_config:SharedMem.config ->
  format_errors:
    (Errors.ConcreteLocPrintableErrorSet.t
     * (* errors *)
       Errors.ConcreteLocPrintableErrorSet.t
     * (* warnings *)
     (Loc.t Errors.printable_error * Loc_collections.LocSet.t) list ->
    (* suppressed errors *) Profiling_js.finished ->
    unit (* print errors *)
    ) ->
  ?focus_targets:Utils_js.FilenameSet.t ->
  Options.t ->
  Errors.ConcreteLocPrintableErrorSet.t * (* errors *) Errors.ConcreteLocPrintableErrorSet.t

(* warnings *)

val daemonize :
  init_id:string ->
  log_file:string ->
  shared_mem_config:SharedMem.config ->
  argv:string array ->
  file_watcher_pid:int option ->
  Options.t ->
  (MonitorProt.server_to_monitor_message, MonitorProt.monitor_to_server_message) Daemon.handle
