(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

#use "utils.ml"

let () =
  let out_file = Sys.argv.(1) in
  let rev =
    try read_process_output "git" [|"git"; "rev-parse"; "HEAD"|]
    with Failure _ ->
      try read_process_output "hg" [|"hg"; "id"; "-i"|]
      with Failure _ -> ""
  in
  let time =
    try read_process_output "git" [|"git"; "log"; "-1"; "--pretty=tformat:%ct"|]
    with Failure _ ->
      try
        let raw = read_process_output "hg" [|"hg"; "log"; "-r"; "."; "-T"; "{date|hgdate}\\n"|] in
        String.sub raw 0 (String.index raw ' ')
      with
      | Failure _ -> "0"
      | Not_found -> "0"
  in
  let content = Printf.sprintf
    "const char* const BuildInfo_kRevision = %S;\nconst unsigned long BuildInfo_kRevisionCommitTimeUnix = %sul;\n"
    rev time in
  let do_dump =
    not (Sys.file_exists out_file) || string_of_file out_file <> content in
  if do_dump then
    with_out_channel out_file @@ fun oc ->
    output_string oc content
