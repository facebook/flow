(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
*)

open ServerProcess
open ServerMonitorUtils

let check_exit_status proc_stat process monitor_config  =
  match proc_stat with
  | Unix.WEXITED 0 -> ()
  | _ ->
    let exit_kind, exit_code = Exit_status.unpack proc_stat in
    Hh_logger.log "typechecker %s with exit code %d\n" exit_kind exit_code;
    let is_oom = match proc_stat with
      | Unix.WEXITED i when i = (Exit_status.exit_code Exit_status.Worker_oomed) -> true
      | _ -> false
    in
    let is_oom = is_oom || try
      Sys_utils.check_dmesg_for_oom process.pid "hh_server" with _ -> false in
    let time_taken = Unix.time () -. process.start_t in
    HackEventLogger.bad_exit
      time_taken proc_stat
      (monitor_config.server_log_file,
      monitor_config.monitor_log_file)
      ~is_oom
