(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

let path_of_root root extension =
  (* TODO: move this to places that write this file *)
  Sys_utils.mkdir_no_fail GlobalConfig.tmp_dir;
  let root_part = Path.slash_escaped_string_of_path root in
  Filename.concat GlobalConfig.tmp_dir (spf "%s.%s" root_part extension)

let is_of_root root fn =
  let root_part = Path.slash_escaped_string_of_path root in
  str_starts_with fn (Filename.concat GlobalConfig.tmp_dir root_part)

(**
 * Lock on this file will be held after the server has finished initializing.
 * *)
let init_complete_file root = path_of_root root "init_complete"
let lock_file root = path_of_root root "lock"
let log_link root = path_of_root root "log"
let pids_file root = path_of_root root "pids"
let socket_file root = path_of_root root "sock"
let dfind_log root = path_of_root root "dfind"
let load_log root = path_of_root root "load"

let monitor_log_link root = path_of_root root "monitor_log"
let ide_log_link root = path_of_root root "ide_log"
