(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

let port_file (file: string) : (string, exn) result =
  try
    let file = Path.to_string (Path.make file) in
    let ast = Parsing_service_js.get_ast_unsafe (File_key.SourceFile file) in
    let content = Sys_utils.cat file in
    let lines = Str.split_delim (Str.regexp "\n") content in
    let insertions = Comments_js.meta_program ast in
    let insertions = List.sort Pervasives.compare insertions in
    let new_content = Reason.do_patch lines insertions in
    let patch_content = Diff.diff_of_file_and_string file new_content in
    Ok patch_content
  with exn ->
    Error exn

let port_files (files: string list) =
  List.fold_left (fun result_map file ->
    SMap.add file (port_file file) result_map
  ) SMap.empty files
