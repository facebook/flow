(**
 * Copyright (c) 2014, Facebook, Inc.
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
open Utils

let auto_complete nenv content =
  AutocompleteService.attach_hooks();
  let funs, classes = ServerIdeUtils.declare Relative_path.default content in
  let dummy_pos = Pos.none, Ident.foo in
  let ifuns =
    SSet.fold begin fun x (funmap, canon_names) ->
      SMap.add x dummy_pos funmap, SMap.add (Naming.canon_key x) x canon_names
    end funs nenv.Naming.ifuns
  in
  let iclasses =
    SSet.fold begin fun x (classmap, canon_names) ->
      SMap.add x dummy_pos classmap, SMap.add (Naming.canon_key x) x canon_names
    end classes nenv.Naming.iclasses
  in
  let nenv = { nenv with Naming.ifuns = ifuns; Naming.iclasses = iclasses } in
  ServerIdeUtils.fix_file_and_def Relative_path.default content;
  let fun_names = SMap.keys (fst nenv.Naming.ifuns) in
  let class_names = SMap.keys (fst nenv.Naming.iclasses) in
  let result = AutocompleteService.get_results fun_names class_names in
  ServerIdeUtils.revive funs classes;
  AutocompleteService.detach_hooks();
  result
