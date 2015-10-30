(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core
open Sys_utils

let find_oom_in_dmesg_output pid lines =
  let re = Str.regexp "Out of memory: Kill process \\([0-9]+\\) (hh_server)" in
  List.exists lines begin fun line ->
    try
      ignore @@ Str.search_forward re line 0;
      let pid_s = Str.matched_group 1 line in
      int_of_string pid_s = pid
    with Not_found -> false
  end

let check_dmesg_for_oom pid =
  let dmesg = exec_read_lines ~reverse:true "dmesg" in
  find_oom_in_dmesg_output pid dmesg

let go main_entry (options, log_file) (_ic, _oc) =
  ignore @@ Sys_utils.setsid ();
  let t = Unix.time () in
  let {Daemon.pid; _} =
    Daemon.spawn ~log_file main_entry options in
  let _pid, proc_stat = Unix.waitpid [] pid in
  (match proc_stat with
   | Unix.WEXITED 0 -> ()
   | _ ->
       let oc =
         open_out_gen [Open_creat; Open_append; Open_binary] 0o666 log_file in
       let exit_kind, exit_code = Exit_status.unpack proc_stat in
       Printf.fprintf oc "%s hh_server %s with exit code %d\n"
         (Hh_logger.timestamp_string ()) exit_kind exit_code;
       close_out oc;
       let is_oom = check_dmesg_for_oom pid in
       let time_taken = Unix.time () -. t in
       HackEventLogger.bad_exit time_taken proc_stat ~is_oom)
