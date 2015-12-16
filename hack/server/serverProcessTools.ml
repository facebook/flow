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

type typechecker_process =
  {
    (** Process ID. *)
    pid : int;
    start_t : float;
    (** Get occassional updates about status/busyness from typechecker here. *)
    in_fd: Unix.file_descr;
    (** Send client's File Descriptors to the typechecker over this. *)
    out_fd : Unix.file_descr;
    log_file: string;
    log_mode : Daemon.log_mode;
  }


let find_oom_in_dmesg_output pid lines =
  let re = Str.regexp
      "Out of memory: Kill process \\([0-9]+\\) (hh_server)" in
  List.exists lines begin fun line ->
    try
      ignore @@ Str.search_forward re line 0;
      let pid_s = Str.matched_group 1 line in
      int_of_string pid_s = pid
    with Not_found -> false
  end

let check_dmesg_for_oom pid =
  let dmesg = Sys_utils.exec_read_lines ~reverse:true "dmesg" in
  find_oom_in_dmesg_output pid dmesg

let check_exit_status proc_stat typechecker =
  match proc_stat with
  | Unix.WEXITED 0 -> ()
  | _ ->
    let exit_kind, exit_code = Exit_status.unpack proc_stat in
    match typechecker.log_mode with
    | Daemon.Log_file | Daemon.Log_append ->
      let oc = open_out_gen [Open_creat; Open_append; Open_binary] 0o666
        typechecker.log_file in
      Printf.fprintf oc "hh_server %s with exit code %d\n"
        exit_kind exit_code;
      close_out oc
    | Daemon.Parent_streams ->
      ();
    let is_oom = check_dmesg_for_oom typechecker.pid in
    let time_taken = Unix.time () -. typechecker.start_t in
    HackEventLogger.bad_exit time_taken proc_stat ~is_oom
