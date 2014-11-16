(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let setup line char =
  Find_refs.find_method_at_cursor_result := None;
  Find_refs.find_method_at_cursor_target := Some (line, char)
  
let restore() =
  Find_refs.find_method_at_cursor_result := None;
  Find_refs.find_method_at_cursor_target := None

let identify content line char =
  setup line char;
  let funs, classes = ServerIdeUtils.declare content in
  ServerIdeUtils.fix_file_and_def content;
  let result = !Find_refs.find_method_at_cursor_result in
  restore();
  ServerIdeUtils.revive funs classes;
  match result with
  | Some result -> Utils.strip_ns result.Find_refs.name
  | _ -> ""

let go content line char oc =
  let result = identify content line char in
  Printf.fprintf oc "%s\n" result; flush oc
