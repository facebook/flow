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
  (* need text pattern and target, assumed to be in that order *)
  let (txt_file, txt_content, txt_parse_ret),
      (pat_file, pat_content, pat_parse_ret),
      (tgt_file, tgt_content, tgt_parse_ret) =
    match parsed_files with
    | txt :: pat :: tgt :: [] -> txt, pat, tgt
    | _ -> failwith "Wrong number of files" in
  let transformations =
    Patcher.preprocess_patch_file
      pat_file pat_content pat_parse_ret
      tgt_file tgt_content tgt_parse_ret in
  let new_source =
    Matcher.match_and_patch
      txt_parse_ret.Parser_hack.ast
      txt_file
      txt_content
      pat_parse_ret
      transformations
      ~use_hh_format:true in
  match new_source with
  | None -> print_endline "No Patch Applicable."
  | Some new_source -> print_endline new_source
  end

let _ =
  begin
  let fname = Sys.argv.(1) in
  let _handle = SharedMem.init_default () in
  Hhi.set_hhi_root_for_unit_test (Path.make "/tmp/hhi");
  run_test (Relative_path.create Relative_path.Dummy fname);
  end
