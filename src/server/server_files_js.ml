(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let add_dir_sep dir =
  let open Filename in
  if check_suffix dir dir_sep
  then dir
  else dir ^ dir_sep

let file_of_root extension ~tmp_dir root =
  let tmp_dir = tmp_dir |> Path.make |> Path.to_string |> add_dir_sep in
  let root_part = Path.slash_escaped_string_of_path root in
  Printf.sprintf "%s%s.%s" tmp_dir root_part extension

let config_file root =
  Path.to_string (Path.concat root ".flowconfig")

let init_file    = file_of_root "init"
let recheck_file = file_of_root "recheck"
let gc_file      = file_of_root "gc"
let lock_file    = file_of_root "lock"
let pids_file    = file_of_root "pids"
let socket_file  = file_of_root "sock"
let dfind_log_file = file_of_root "dfind"
let log_file ~tmp_dir root opts =
  match opts.FlowConfig.Opts.log_file with
  | Some x -> x
  | None -> Path.make (file_of_root "log" ~tmp_dir root)
