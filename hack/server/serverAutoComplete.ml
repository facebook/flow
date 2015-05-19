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
      let () = Hashtbl.replace canon_names (Naming.canon_key x) x in
      let () = Hashtbl.replace funmap x dummy_pos in
      funmap, canon_names
    end funs nenv.Naming.ifuns
  in
  let iclasses =
    SSet.fold begin fun x (classmap, canon_names) ->
      let () = Hashtbl.replace canon_names (Naming.canon_key x) x in
      let () = Hashtbl.replace classmap x dummy_pos in
      classmap, canon_names
    end classes nenv.Naming.iclasses
  in
  let nenv = { nenv with Naming.ifuns = ifuns; Naming.iclasses = iclasses } in
  ServerIdeUtils.fix_file_and_def Relative_path.default content;
  let hash_keys = fun ht -> Hashtbl.fold (fun key _ acc -> key :: acc) ht [] in
  let fun_names = hash_keys (fst nenv.Naming.ifuns) in
  let class_names = hash_keys (fst nenv.Naming.iclasses) in
  let result = AutocompleteService.get_results fun_names class_names in
  ServerIdeUtils.revive funs classes;
  AutocompleteService.detach_hooks();
  result
