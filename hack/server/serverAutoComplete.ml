(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)


(*****************************************************************************)
(* Code for auto-completion *)
(*****************************************************************************)
open Core
open Utils

let auto_complete files_info content =
  AutocompleteService.attach_hooks();
  let funs, classes =
    ServerIdeUtils.declare Relative_path.default content in
  ServerIdeUtils.fix_file_and_def funs classes;
  let fun_names, class_names =
    files_info
    |> Relative_path.Map.values
    |> List.fold_left ~f:begin fun (f, c) { FileInfo.funs; classes; _ } ->
      let add_all ids init =
        List.fold_left ids ~init ~f:(fun s (_, x) -> SSet.add x s) in
      add_all funs f, add_all classes c
    end ~init:(funs, classes)
  in
  let fun_names = SSet.elements fun_names in
  let class_names = SSet.elements class_names in
  let result = AutocompleteService.get_results fun_names class_names in
  ServerIdeUtils.revive funs classes;
  AutocompleteService.detach_hooks();
  result
