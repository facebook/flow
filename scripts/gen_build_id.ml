(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

#use "scripts/utils.ml"

let () =
  let out_file = Sys.argv.(1) in
  let rev =
    try read_process_output "git" [|"git"; "rev-parse"; "HEAD"|]
    with Failure _ ->
      read_process_output "hg" [|"hg"; "id"; "-i"|]
  in
  let content =
    Printf.sprintf "const char* const BuildInfo_kRevision = %S;\n" rev in
  let do_dump =
    not (Sys.file_exists out_file) || string_of_file out_file <> content in
  if do_dump then
    with_out_channel out_file @@ fun oc ->
    output_string oc content
