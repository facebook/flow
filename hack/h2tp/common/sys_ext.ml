(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include Sys

module CE = Common_exns
module Str = Str_ext

open Utils

exception NotADirectory of string
let (=~) = Str.(=~)

let perms f =
  (Unix.stat f).Unix.st_perm

let is_file file =
  (Unix.stat file).Unix.st_kind = Unix.S_REG

let is_symlink file =
  (Unix.lstat file).Unix.st_kind = Unix.S_LNK

let copy_dir s d =
  if file_exists d
  then raise CE.Impossible
  else Unix.mkdir d (perms s)

let chop_dirsymbol = function
  | s when s =~ "\\(.*\\)/$" -> Str.matched_group 1 s
  | s -> s

let filename_without_leading_path prefix s =
  let prefix = chop_dirsymbol prefix in
  if s = prefix
  then "."
  else
    if s =~ ("^" ^ prefix ^ "/\\(.*\\)$")
    then Str.matched_group 1 s
    else raise CE.Impossible


let process_output_to_list2 command =
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)

let cmd_to_list command =
  let (l,exit_status) = process_output_to_list2 command in
  match exit_status with
  | Unix.WEXITED 0 -> l
  | _ -> raise (CE.CmdError (exit_status,
                         (Printf.sprintf "CMD = %s, RESULT = %s"
                             command (String.concat "\n" l))))


let recursive_file_pairs src dest =
  let escaped_src =
    cmd_to_list ("find \"" ^ src ^ "\" -maxdepth 0") |> List.hd in
  List.map begin fun f ->
    let suffix = filename_without_leading_path escaped_src f in
    if suffix = "." then (f, dest) else (f, dest ^ "/" ^ suffix)
    end (cmd_to_list ("find \"" ^ src ^ "\""))

let has_extension f ext =
  if ext.[0] <> '.'
  then failwith "I need an extension such as .c not just c";
  f =~ (".*\\" ^ ext ^ "$")

let set_extension f ext =
  if ext.[0] <> '.'
  then failwith "I need an extension such as .c not just c";
  if f =~ ("\\(.*\\)\\.[^\\.]+$")
  then (Str.matched_group 1 f) ^ ext
  else failwith "Regex match failure on extension"

let copy_file s d = ignore (command ("cp -p \"" ^ s ^ "\" \"" ^ d ^ "\""))

let rec mkdir_p = function
  | "" -> raise CE.Impossible
  | d when not (file_exists d) ->
    mkdir_p (Filename.dirname d);
    Unix.mkdir d 0o770;
  | d when is_directory d -> ()
  | d -> raise (NotADirectory d)

let write_file str f =
  if file_exists f
  then raise (CE.FileExists f);
  let oc = open_out f in
  output_string oc str;
  close_out oc

let die errors =
  let oc = stderr in
  List.iter begin fun (name, msg) ->
    output_string oc (name ^ ":\n");
    output_string oc (msg ^ "\n");
  end errors;
  close_out oc;
  exit 2
