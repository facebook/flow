(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

let (file_parsed_hooks:
  (Relative_path.t -> Ast.program -> unit) list ref) = ref []

let (parse_task_completed_hooks:
  (Relative_path.t list -> Relative_path.Set.t -> unit) list ref) = ref []

let attach_file_parsed_hook hook =
  file_parsed_hooks := hook :: !file_parsed_hooks

let attach_parse_task_completed_hook hook =
  parse_task_completed_hooks := hook :: !parse_task_completed_hooks

let dispatch_file_parsed_hook fn ast =
  List.iter begin fun hook -> hook fn ast end !file_parsed_hooks

let dispatch_parse_task_completed_hook files php_files =
  List.iter begin fun hook ->
    hook files php_files
  end !parse_task_completed_hooks

let remove_all_hooks () =
  file_parsed_hooks := [];
  parse_task_completed_hooks := [];
