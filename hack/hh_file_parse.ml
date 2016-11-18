(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
let usage = Printf.sprintf "Usage: %s filename\n" Sys.argv.(0)

let verbose = ref false

let print_error error = error
  |> Errors.to_absolute
  |> Errors.to_string
  |> output_string stderr


let parse_with_regular file content =
  let errorl, parser_ret, _ =
    Errors.do_ begin fun () ->
      Parser_hack.program ParserOptions.default file content
    end
  in
  let funs, classes, typedefs, consts =
    Ast_utils.get_defs parser_ret.Parser_hack.ast in
  let defs =
    {FileInfo.funs; classes; typedefs; consts;
     comments = Some []; file_mode = None;
     consider_names_just_for_autoload = false}
  in
    Printf.printf "FileInfo with regular parser:\n";
    if not (Errors.is_empty errorl) then
      Errors.iter_error_list print_error errorl
    else
      FileInfo.print_names (FileInfo.simplify defs)

let parse_and_print filename =
  let file = Relative_path.create Relative_path.Dummy filename in
  let content =
    try Sys_utils.cat (Relative_path.to_absolute file) with _ -> "" in
  if !verbose then Printf.printf "File contents:\n%s\n" content;
  let errorl, result, _ =
    Errors.do_ begin fun () ->
      Fileinfo_parser.program ParserOptions.default file content
    end
  in
  if !verbose then begin
    parse_with_regular file content;
    Printf.printf "FileInfo with fileInfo parser:\n"
  end;
  if not (Errors.is_empty errorl) then
    Errors.iter_error_list print_error errorl
  else
  FileInfo.print_names (FileInfo.simplify result)


let main filename =
  let _handle = SharedMem.init GlobalConfig.default_sharedmem_config in
  if (String.length filename) = 0 then begin
    Printf.eprintf "%s" usage;
    exit 1
  end;
  Unix.handle_unix_error parse_and_print filename

let () =
  Arg.parse ["-v", Arg.Set verbose, "Turn on verbose mode"] main usage;
