(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils

module PosSet = Set.Make(Pos)

let handle_lvar target_ident ident_refs line char_pos ident id _ =
  let l, start, end_ = Pos.info_pos (fst id) in
  if l = line && start <= char_pos && char_pos <= end_
  then target_ident := Some ident;
  let current_set =
    match IMap.get ident !ident_refs with
    | Some refs -> refs
    | None -> PosSet.empty
  in
  ident_refs := IMap.add ident (PosSet.add (fst id) current_set) !ident_refs


let attach_hooks line char =
  let target_ident = ref None in
  let ident_refs = ref IMap.empty in
  let get_result () =
    match !target_ident with
    | Some ident -> PosSet.elements (IMap.find_unsafe ident !ident_refs)
    | None -> []
  in
  Naming_hooks.attach_lvar_hook (handle_lvar target_ident ident_refs line char);
  get_result

let detach_hooks () =
  Naming_hooks.remove_all_hooks ()
