(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * Driver for tests of the hack AST visitor and the Ast_code_extent module.
 *)
module Ace = Ast_code_extent

let run_test file : unit =
  begin
  let parsed_files = Hh_match_test_utils.parse_file file in
  if List.length parsed_files != 2
  then failwith "Wrong number of files"
  else
  (* first file is text, second file is pattern *)
  let (t_file, t_content, t_parser_return) = List.hd parsed_files in
  let (_p_file, _p_content, p_parser_return) = List.hd (List.tl parsed_files) in
  print_endline
    (Matcher.format_matches
       (Matcher.find_matches
          t_parser_return.Parser_hack.ast
          t_file
          t_content
          p_parser_return)
       t_content);
  end

let _ =
  begin
  let fname = Sys.argv.(1) in
  let _handle = SharedMem.init_default () in
  Hhi.set_hhi_root_for_unit_test (Path.make "/tmp/hhi");
  run_test (Relative_path.create Relative_path.Dummy fname);
  end
