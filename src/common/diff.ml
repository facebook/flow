(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Sys_utils

let diff_of_file_and_string file new_content =
  let new_file = Filename.temp_file "" "" in
  write_file new_file new_content;
  let patch_file = Filename.temp_file "" "" in
  let diff_cmd =
    if Sys.win32 then
      Printf.sprintf "fc %s %s > %s"
        file new_file patch_file
    else
      Printf.sprintf "diff -u --label old --label new %s %s > %s"
        file new_file patch_file in
  diff_cmd
  |> Sys.command |> ignore;
  cat patch_file
