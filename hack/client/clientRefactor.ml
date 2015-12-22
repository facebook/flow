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
open ClientEnv
open Utils

let compare_pos pos1 pos2 =
  let char_start1, char_end1 = Pos.info_raw pos1 in
  let char_start2, char_end2 = Pos.info_raw pos2 in
  if char_end1 <= char_start2
  then -1
  else if char_end2 <= char_start1
  then 1
  else 0

let get_pos = function
  | ServerRefactor.Insert patch
  | ServerRefactor.Replace patch -> patch.ServerRefactor.pos
  | ServerRefactor.Remove p -> p

let compare_result res1 res2 =
  compare_pos (get_pos res1) (get_pos res2)

let map_patches_to_filename acc res =
  let pos = get_pos res in
  let fn = Pos.filename pos in
  match SMap.get fn acc with
  | Some lst -> SMap.add fn (res :: lst) acc
  | None -> SMap.add fn [res] acc

let write_string_to_file fn str =
  let oc = open_out fn in
  output_string oc str;
  close_out oc

let write_patches_to_buffer buf original_content patch_list =
  let i = ref 0 in
  (* advances to requested character and adds the original content
     from the current position to that point to the buffer *)
  let add_original_content j =
    if j <= !i then () else
    let size = (j - !i + 1) in
    let str_to_write = String.sub original_content !i size in
    Buffer.add_string buf str_to_write;
    i := !i + size
  in
  List.iter patch_list begin fun res ->
    let pos = get_pos res in
    let char_start, char_end = Pos.info_raw pos in
    add_original_content (char_start - 1);
    match res with
      | ServerRefactor.Insert patch ->
          Buffer.add_string buf patch.ServerRefactor.text
      | ServerRefactor.Replace patch ->
          Buffer.add_string buf patch.ServerRefactor.text;
          i := char_end
      | ServerRefactor.Remove _ ->
          i := char_end
  end;
  add_original_content (String.length original_content - 1)

let apply_patches_to_file fn patch_list =
  let old_content = Sys_utils.cat fn in
  let buf = Buffer.create (String.length old_content) in
  let patch_list = List.sort compare_result patch_list in
  write_patches_to_buffer buf old_content patch_list;
  let new_file_contents = Buffer.contents buf in
  write_string_to_file fn new_file_contents

let input_prompt str =
  print_string str;
  flush stdout;
  input_line stdin

let apply_patches file_map =
  SMap.iter apply_patches_to_file file_map;
  print_endline
      ("Rewrote "^(string_of_int (SMap.cardinal file_map))^" files.")

let patch_to_json res =
  let type_, replacement = match res with
    | ServerRefactor.Insert patch ->
        "insert", patch.ServerRefactor.text
    | ServerRefactor.Replace patch ->
        "replace", patch.ServerRefactor.text
    | ServerRefactor.Remove _ ->
        "remove", ""
  in
  let pos = get_pos res in
  let char_start, char_end = Pos.info_raw pos in
  let line, start, end_ = Pos.info_pos pos in
  Hh_json.JSON_Object [
      "char_start",  Hh_json.int_ char_start;
      "char_end",    Hh_json.int_ char_end;
      "line",        Hh_json.int_ line;
      "col_start",   Hh_json.int_ start;
      "col_end",     Hh_json.int_ end_;
      "patch_type",  Hh_json.JSON_String type_;
      "replacement", Hh_json.JSON_String replacement;
  ]

let print_patches_json file_map =
  let entries = SMap.fold begin fun fn patch_list acc ->
    Hh_json.JSON_Object [
        "filename", Hh_json.JSON_String fn;
        "patches",  Hh_json.JSON_Array (List.map patch_list patch_to_json);
    ] :: acc
  end file_map [] in
  print_endline (Hh_json.json_to_string (Hh_json.JSON_Array entries))

let go conn args mode before after =
    let command = match mode with
    | "Class" -> ServerRefactor.ClassRename (before, after)
    | "Function" -> ServerRefactor.FunctionRename (before, after)
    | "Method" ->
      let befores = Str.split (Str.regexp "::") before in
      if (List.length befores) <> 2
        then failwith "Before string should be of the format class::method"
        else ();
      let afters = Str.split (Str.regexp "::") after in
      if (List.length afters) <> 2
        then failwith "After string should be of the format class::method"
        else ();
      let before_class = List.hd_exn befores in
      let before_method = List.hd_exn (List.tl_exn befores) in
      let after_class = List.hd_exn afters in
      let after_method = List.hd_exn (List.tl_exn afters) in
      if before_class <> after_class
      then begin
        Printf.printf "%s %s\n" before_class after_class;
        failwith "Before and After classname must match"
      end
      else
        ServerRefactor.MethodRename (before_class, before_method, after_method)
    | _ ->
        failwith "Unexpected Mode" in

    let patches = ServerCommand.rpc conn @@ ServerRpc.REFACTOR command in
    let file_map = List.fold_left patches
      ~f:map_patches_to_filename ~init:SMap.empty in
    if args.output_json
    then print_patches_json file_map
    else apply_patches file_map
