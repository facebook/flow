(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 * Driver for unit tests of the hack_spatch module.
 *)
let run_test (file : Relative_path.t) : unit =
  begin
  let parsed_files = Hh_match_test_utils.parse_file file in
  if List.length parsed_files != 2
  then failwith "Wrong number of files"
  else
  (* only need pattern for this tests *)
  let (p_file, p_content, p_parser_return) = List.hd parsed_files in
  let (t_file, t_content, t_parser_return) = List.hd (List.tl parsed_files) in
  let preproc_res =
    Patcher.preprocess_patch_file
      p_file p_content p_parser_return
      t_file t_content t_parser_return in
  let stmt_strings, expr_strings =
    Patcher.to_string_patch_maps
      preproc_res p_file p_content in
  List.fold_left
    (fun _ strn -> print_endline (strn ^ "\n\n=====\n\n")) () stmt_strings;
  List.fold_left
    (fun _ strn -> print_endline (strn ^ "\n\n=====\n\n")) () expr_strings
  end

let _ =
  begin
  let fname = Sys.argv.(1) in
  let _handle = SharedMem.init_default () in
  Hhi.set_hhi_root_for_unit_test (Path.make "/tmp/hhi");
  run_test (Relative_path.create Relative_path.Dummy fname);
  end
