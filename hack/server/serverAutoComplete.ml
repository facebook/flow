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
  let content_funs, content_classes =
    ServerIdeUtils.declare Relative_path.default content in
  ServerIdeUtils.fix_file_and_def content_funs content_classes;
  let fun_names, class_names =
    files_info
    |> Relative_path.Map.values
    |> List.fold_left ~f:begin fun (f, c) { FileInfo.funs; classes; _ } ->
      let add_all ids content_ids init =
        List.fold_left ids ~init ~f: begin fun acc (_, x) ->
          (* Duplicate class/function name is an error so should be rare, we
           * only need to avoid adding the names declared in content twice *)
          if SSet.mem x content_ids then acc else x::acc
        end in
      add_all funs content_funs f, add_all classes content_classes c
    end ~init:(SSet.elements content_funs, SSet.elements content_classes)
  in
  let result = AutocompleteService.get_results fun_names class_names in
  ServerIdeUtils.revive content_funs content_classes;
  AutocompleteService.detach_hooks();
  result
