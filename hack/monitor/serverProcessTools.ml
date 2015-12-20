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
open ServerProcess

let find_oom_in_dmesg_output process lines =
  let re = Str.regexp (Printf.sprintf
      "Out of memory: Kill process \\([0-9]+\\) (%s)" process.name) in
  List.exists lines begin fun line ->
    try
      ignore @@ Str.search_forward re line 0;
      let pid_s = Str.matched_group 1 line in
      int_of_string pid_s = process.pid
    with Not_found -> false
  end

let check_dmesg_for_oom process =
  let dmesg = Sys_utils.exec_read_lines ~reverse:true "dmesg" in
  find_oom_in_dmesg_output process dmesg

let check_exit_status proc_stat process =
  match proc_stat with
  | Unix.WEXITED 0 -> ()
  | _ ->
    let exit_kind, exit_code = Exit_status.unpack proc_stat in
    Hh_logger.log "%s %s with exit code %d\n" process.name exit_kind exit_code;
    let is_oom = try check_dmesg_for_oom process with _ -> false in
    let time_taken = Unix.time () -. process.start_t in
    HackEventLogger.bad_exit time_taken proc_stat ~is_oom
