(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
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
