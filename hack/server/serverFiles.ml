(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let file_of_root root extension =
  (* TODO: move this to places that write this file *)
  Sys_utils.mkdir_no_fail GlobalConfig.tmp_dir;
  let root_part = Path.slash_escaped_string_of_path root in
  Printf.sprintf "%s%s.%s" GlobalConfig.tmp_dir root_part extension

let init_file root = file_of_root root "init"
let log_file root = file_of_root root "log"
let lock_file root = file_of_root root "lock"
let pids_file root = file_of_root root "pids"
let socket_file root = file_of_root root "sock"
