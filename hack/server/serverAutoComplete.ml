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

let auto_complete tcopt files_info content =
  AutocompleteService.attach_hooks();
  let path = Relative_path.default in
  let content_funs, content_classes, content_typedefs =
    ServerIdeUtils.declare_and_check path content in
  let result = AutocompleteService.get_results
    tcopt content_funs content_classes in
  ServerIdeUtils.revive content_funs content_classes content_typedefs path;
  AutocompleteService.detach_hooks();
  result
