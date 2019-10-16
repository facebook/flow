(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type node_module_path_parts = {
  top_level_node_modules_index: int;
  top_level_package_name_index: int;
  package_root_index: int;
  file_name_index: int;
}

type states = {
  before_node_modules: int;
  node_modules: int;
  scope: int;
  package_content: int;
}

let states = { before_node_modules = 0; node_modules = 1; scope = 2; package_content = 3 }

(* Example of expected pattern: /base/path/node_modules/[@scope/otherpackage/@otherscope/node_modules/]package/[subdirectory/]file.js *)
(* Returns indices:                       ^            ^                                                      ^             ^ *)
let get_node_module_path_parts (full_path : string) (node_modules_path_part : string) :
    node_module_path_parts option =
  let top_level_node_modules_index = ref 0 in
  let top_level_package_name_index = ref 0 in
  let package_root_index = ref 0 in
  let file_name_index = ref 0 in
  let part_start = ref 0 in
  let part_end = ref 0 in
  let state = ref states.before_node_modules in
  while !part_end >= 0 do
    part_start := !part_end;
    part_end :=
      String_utils.substring_index ~start_pos:(!part_start + 1) Filename.dir_sep full_path;

    if !state == states.before_node_modules then (
      let start = !part_start in
      let index = String_utils.substring_index ~start_pos:start node_modules_path_part full_path in
      if index == start then (
        top_level_node_modules_index := start;
        top_level_package_name_index := !part_end;
        state := states.node_modules
      )
    ) else if !state == states.node_modules || !state == states.scope then
      let index = !part_start + 1 in
      let ch = full_path.[index] in
      let has_scope = ch == '@' in
      if !state == states.node_modules && has_scope then
        state := states.scope
      else (
        package_root_index := !part_end;
        state := states.package_content
      )
    else if !state == states.package_content then (
      let index =
        String_utils.substring_index ~start_pos:!part_start node_modules_path_part full_path
      in
      if index == !part_start then state := states.node_modules
    ) else
      state := states.package_content
  done;

  file_name_index := !part_start;

  if !state > states.node_modules then
    Some
      {
        top_level_node_modules_index = !top_level_node_modules_index;
        top_level_package_name_index = !top_level_package_name_index;
        package_root_index = !package_root_index;
        file_name_index = !file_name_index;
      }
  else
    None
