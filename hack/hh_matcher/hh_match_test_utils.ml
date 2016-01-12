(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * Utils for the hack_sgrep tests
 *)
open Sys_utils

let pretty_print_test_output
      (code : string)
      (range : File_pos.t * File_pos.t) : unit =
  Printf.printf
    "%s\n\n=====\n\n"
    (Ast_code_extent.lexing_slice_to_string range code)

(* This allows one to fake having multiple files in one file. This
 * is used only in unit test files.
 * This is necessary because we have to test matching a pattern
 * against a source file or patching a source file with a
 * pattern and target.
 *)
let rec make_files = function
  | [] -> []
  | Str.Delim header :: Str.Text content :: rl ->
      let pattern = Str.regexp "////" in
      let header = Str.global_replace pattern "" header in
      let pattern = Str.regexp "[ ]*" in
      let filename = Str.global_replace pattern "" header in
      (filename, content) :: make_files rl
  | _ -> assert false

let parse_file (file : Relative_path.t) :
      (Relative_path.t * string * Parser_hack.parser_return) list =
  let abs_fn = Relative_path.to_absolute file in
  let content = cat abs_fn in
  let delim = Str.regexp "////.*" in
  if Str.string_match delim content 0
  then
    let contentl = Str.full_split delim content in
    let files = make_files contentl in
    List.rev
      (List.fold_left begin fun acc (sub_fn, content) ->
         let file =
           Relative_path.create Relative_path.Dummy (abs_fn^"--"^sub_fn) in
         (file, content, (Parser_hack.program file content)) :: acc
      end [] files)
  else
    [(file, content, Parser_hack.program file content)]
