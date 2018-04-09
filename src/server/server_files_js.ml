(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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

let lock_file    = file_of_root "lock"
let pids_file    = file_of_root "pids"
let socket_file  = file_of_root "sockv2"
let legacy_socket_file = file_of_root "sock"
let dfind_log_file = file_of_root "dfind"
