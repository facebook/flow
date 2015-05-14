(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open ClientEnv
open Utils
module Json = Hh_json

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

let read_file_to_string fn = 
  let ic = open_in fn in
  let len = in_channel_length ic in
  let buf = Buffer.create len in
  Buffer.add_channel buf ic len;
  Buffer.contents buf

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
  
  List.iter begin fun res ->
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
  end patch_list;
  add_original_content (String.length original_content - 1)

let apply_patches_to_file fn patch_list =
  let old_content = read_file_to_string fn in
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
  Json.JAssoc [ "char_start",  Json.JInt char_start;
                "char_end",    Json.JInt char_end;
                "line",        Json.JInt line;
                "col_start",   Json.JInt start;
                "col_end",     Json.JInt end_;
                "patch_type",  Json.JString type_;
                "replacement", Json.JString replacement;
              ]

let print_patches_json file_map =
  let entries = SMap.fold begin fun fn patch_list acc ->
    Json.JAssoc [ "filename", Json.JString fn;
                  "patches",  Json.JList (List.map patch_to_json patch_list);
                ] :: acc
  end file_map [] in
  print_endline (Json.json_to_string (Json.JList entries))

let go conn args =
  try
    print_endline ("WARNING: This tool will only refactor references in "^
        "typed, hack code. Its results should be manually verified. "^
        "Namespaces are not yet supported.");
    print_endline "What would you like to refactor:";
    print_endline "    1 - Class";
    print_endline "    2 - Function";
    print_endline "    3 - Method";
    print_string "Enter 1, 2, or 3: ";
    flush stdout;

    let refactor_type = input_line stdin in
    let command = match refactor_type with
    | "1" -> 
      let class_name = input_prompt "Enter class name: " in
      let new_name = input_prompt "Enter a new name for this class: " in
      ServerRefactor.ClassRename (class_name, new_name)
    | "2" ->
      let fun_name = input_prompt "Enter function name: " in
      let new_name = input_prompt "Enter a new name for this function: " in
      ServerRefactor.FunctionRename (fun_name, new_name)
    | "3" ->
      let class_name = input_prompt "Enter class name: " in
      let method_name = input_prompt "Enter method name: " in
      let new_name =
          input_prompt ("Enter a new name for this method: "^class_name^"::") in
      ServerRefactor.MethodRename (class_name, method_name, new_name)
    | _ -> raise Exit in
    
    let patches = ServerCommand.rpc conn @@ ServerRpc.REFACTOR command in
    let file_map = List.fold_left map_patches_to_filename SMap.empty patches in
    if args.output_json
    then print_patches_json file_map
    else apply_patches file_map
  with Exit ->
    print_endline "Invalid Input"
