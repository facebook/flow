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
open Reordered_argument_collections

let get_all_funs_and_classes files_info content_funs content_classes =
  files_info
  |> Relative_path.Map.values
  |> List.fold_left ~f:begin fun (f, c) { FileInfo.funs; classes; _ } ->
    let add_all ids content_ids init =
      List.fold_left ids ~init ~f: begin fun acc (_, x) ->
        (* Duplicate class/function name is an error so should be rare, we
         * only need to avoid adding the names declared in content twice *)
        if SSet.mem content_ids x then acc else x::acc
      end in
    add_all funs content_funs f, add_all classes content_classes c
  end ~init:(SSet.elements content_funs, SSet.elements content_classes)

let auto_complete tcopt files_info content =
  AutocompleteService.attach_hooks();
  let content_funs, content_classes =
    ServerIdeUtils.declare_and_check Relative_path.default content in
  let gen_funs_and_classes = (fun () ->
    get_all_funs_and_classes files_info content_funs content_classes) in
  let result = AutocompleteService.get_results tcopt gen_funs_and_classes in
  ServerIdeUtils.revive content_funs content_classes;
  AutocompleteService.detach_hooks();
  result
