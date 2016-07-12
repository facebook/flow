(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

let port_file (file: string) : (string, exn) ok_or_err =
  try
    let file = Path.to_string (Path.make file) in
    let ast = Parsing_service_js.get_ast_unsafe (Loc.SourceFile file) in
    let content = Sys_utils.cat file in
    let lines = Str.split_delim (Str.regexp "\n") content in
    let insertions = Comments_js.meta_program ast in
    let insertions = List.sort Pervasives.compare insertions in
    let new_content = Reason.do_patch lines insertions in
    let patch_content = Diff.diff_of_file_and_string file new_content in
    OK patch_content
  with exn ->
    Err exn

let port_files (files: string list) =
  List.fold_left (fun result_map file ->
    SMap.add file (port_file file) result_map
  ) SMap.empty files
