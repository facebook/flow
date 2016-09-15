(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module SyntaxError = Full_fidelity_syntax_error
module SyntaxTree = Full_fidelity_syntax_tree
module SourceText = Full_fidelity_source_text

let usage = Printf.sprintf "Usage: %s folder_name loop-constant use-old\n"
  Sys.argv.(0)

let parse_old folder_name loop_const =
  let all_files = Array.to_list (Sys.readdir folder_name) in
  let folder_name =
    if String_utils.string_ends_with folder_name "/" then folder_name
    else folder_name ^ "/"
  in
  let loop filename =
    let filename = folder_name ^ filename in
    let file = Relative_path.create Relative_path.Dummy filename in
    let content =
      try Sys_utils.cat (Relative_path.to_absolute file) with _ -> ""
    in
    for l =  1 to loop_const do
      (* TODO how to make sure compiler do not optimize away pure functions? *)
      (* FIXME: Don't use default tcopt *)
      ignore (Parser_hack.program TypecheckerOptions.default file content)
    done
  in
  List.iter loop all_files

let parse_new folder_name loop_const =
  let all_files = Array.to_list (Sys.readdir folder_name) in
  let folder_name =
    if String_utils.string_ends_with folder_name "/" then folder_name
    else folder_name ^ "/"
  in
  let loop filename =
    let filename = folder_name ^ filename in
    let file = Relative_path.create Relative_path.Dummy filename in
    let source_file = SourceText.from_file file in
    for l = 1 to loop_const do
      ignore (SyntaxTree.make source_file)
    done
  in
  List.iter loop all_files


let main folder_name loop_const use_old =
  EventLogger.init (Daemon.devnull ()) 0.0;
  let _ = SharedMem.(init GlobalConfig.default_sharedmem_config) in
  if (String.length folder_name) = 0 then begin
    Printf.eprintf "%s" usage;
    exit 1
  end;
  if use_old then
    Unix.handle_unix_error parse_old folder_name loop_const
  else
    Unix.handle_unix_error parse_new folder_name loop_const

let () =
  let (folder_name, loop_const, use_old) =
    try
      (Sys.argv.(1), int_of_string(Sys.argv.(2)), bool_of_string(Sys.argv.(3)))
    with
    | _ -> (Printf.printf "%s" usage; exit 1)
  in
  main folder_name loop_const use_old
