(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)
(*
 * This is basically the same thing as js_test_files.js but instead of running
 * the js version of the parser this runs the fast native version. It doesn't
 * do any ast verification, though, since the verification is all in js
 *
 * Usage: ./native_test_files [file1] [file2] ... [filen]
 *)

let rec run (passed, failed)= function
  | [] -> (passed, failed)
  | file :: rest ->
      Printf.printf "RUNNING %s%!" file;
      let ic = open_in file in
      let len = in_channel_length ic in
      let buf = Buffer.create len in
      Buffer.add_channel buf ic len;
      let content = Buffer.contents buf in
      close_in ic;
      let (passed, failed), status = (try
        ignore (Parser_flow.program content ~fail:true);
        (passed + 1, failed), "PASSED"
      with _ ->
        (passed, failed+1), "FAILED") in
      Printf.printf "\r%s %s   \n%!" status file;
      run (passed, failed) rest

let passed, failed = (match Array.to_list Sys.argv with
  | [] -> assert false
  | _cmd::files -> run (0, 0) files)
let () = Printf.printf "%d/%d passed\n%!" passed (passed+failed)
