(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

module IdentMap = Map.Make(Ident)

type result = {
  pos: string Pos.pos;
  type_: string;
  ident_: int;
}

(* Transform type_map from indexed by Pos into indexed by file:line *)
(* all the types on the same line will be chained into a list *)
let transform_map type_map =
  let line_map = SymbolUtils.LineMap.empty in
  Pos.Map.fold begin fun pos type_str acc ->
    let key = SymbolUtils.get_key pos in
    let item = (pos, type_str) in
    let value = begin match SymbolUtils.LineMap.get key acc with
      | Some value -> item :: value
      | None -> [item]
    end in
    SymbolUtils.LineMap.add key value acc;
  end type_map line_map

(* Find the best match type in the list sequentially *)
let find_match_pos_in_list match_pos types_list =
  let find_result =
    List.fold_left types_list ~f:begin fun acc (pos, type_str) ->
      match acc with
      | Some (type_pos, _) ->
          (* There is already a match one see if this is better *)
          if Pos.contains type_pos pos then
            Some (pos, type_str)
          else
            acc
      | None ->
          (* No match type yet, any match type will be used *)
          if match_pos = pos || Pos.contains pos match_pos then
            Some (pos, type_str)
          else
            None
    end ~init:None in
    match find_result with
    | Some (pos, value) -> value
    | None -> ""

(* Given all the idents for this file, make a rekeying map which *)
(* makes a new identifier which is consistent *)
let gen_ident_rekeying_map ident_list =
  let _, map = List.fold_right ident_list ~init:(0, IdentMap.empty)
    ~f:begin fun ident (index, ident_map) ->
      if IdentMap.mem ident ident_map then (index, ident_map)
      else (index + 1, IdentMap.add ident index ident_map)
    end in
  map

let lvar_list_map lvar_map =
  Pos.Map.fold begin fun pos ident map ->
    let file = Pos.filename pos in
    let ident_list = match Relative_path.Map.get file map with
      | Some ident_list -> ident_list
      | None -> []
    in
    Relative_path.Map.add file (ident :: ident_list) map
  end lvar_map Relative_path.Map.empty

(* For each local variable in lvar_map find its type in type_map.
 * Since the pos in both maps may not be identical we used
 * the following algorithm:
 * 1. Transform type_map into indexed by line
 * 2. For each local variable find all the types in that line
 * 3. Sequentially search each type to find the best match one *)
let generate_types lvar_map type_map =
  let line_map = transform_map type_map in
  let file_to_lvarlist_map = lvar_list_map lvar_map in
  let file_lvar_rekeying_map = Relative_path.Map.map
    begin fun v -> gen_ident_rekeying_map v end file_to_lvarlist_map in
  let lvar_pos_list = Pos.Map.keys lvar_map in
  List.rev_map lvar_pos_list begin fun lvar_pos ->
    let key = SymbolUtils.get_key lvar_pos in
    let ident = Pos.Map.find_unsafe lvar_pos lvar_map in
    let types_in_line = SymbolUtils.LineMap.get key line_map in
    let lvar_rekey_map = Relative_path.Map.find_unsafe
      (Pos.filename lvar_pos) file_lvar_rekeying_map in
    let lvar_type = match types_in_line with
    | Some types_list ->
        find_match_pos_in_list lvar_pos types_list
    | None -> "" in
    {
      pos = Pos.to_relative_string lvar_pos;
      type_ = lvar_type;
      ident_ = IdentMap.find ident lvar_rekey_map;
    }
  end

let process_symbol_type result_map type_ pos env =
  let type_str = Typing_print.strip_ns env type_ in
  result_map := Pos.Map.add pos type_str !result_map

let handle_lvar result_map ident id locals =
  let pos, name = id in
  result_map := Pos.Map.add pos ident !result_map

let attach_hooks type_map lvar_map =
  Typing_hooks.attach_infer_ty_hook (process_symbol_type type_map);
  Naming_hooks.attach_lvar_hook (handle_lvar lvar_map)

let detach_hooks () =
  Typing_hooks.remove_all_hooks ();
  Naming_hooks.remove_all_hooks ()
