(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let enabled_for_file (excludes : Str.regexp list) (file : File_key.t) =
  let path = File_key.to_string file |> Sys_utils.normalize_filename_dir_sep in
  not @@ List.exists (fun r -> Str.string_match r path 0) excludes

let module_prefix_for_file (includes : Str.regexp list) (file : File_key.t) = function
  | Some module_prefix ->
    let path = File_key.to_string file |> Sys_utils.normalize_filename_dir_sep in
    if List.exists (fun r -> Str.string_match r path 0) includes then
      Some module_prefix
    else
      None
  | None -> None
