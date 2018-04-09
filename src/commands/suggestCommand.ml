(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow suggest (infer types for file) command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "suggest";
  doc = "Shows type annotation suggestions for given files";
  usage = Printf.sprintf
    "Usage: %s suggest [OPTION]... [FILE]...\n\n\
      Suggests types in one or more files\n\n\
      Example usage:\n\
      \t%s suggest file1 file2\n"
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> from_flag
    |> anon "files" (required (list_of string)) ~doc:"Files"
  )
}

let parse_suggest_cmd file =
  let digits = "\\([0-9]+\\)" in
  let re = Printf.sprintf "\\(.*\\):%s:%s,%s:%s"
    digits digits digits digits in
  if Str.string_match (Str.regexp re) file 0
  then
    (Str.matched_group 1 file,
     List.map (fun i -> Str.matched_group i file) [2;3;4;5])
  else
    (file, [])

let main option_values root from files () =
  FlowEventLogger.set_from from;
  let files_and_regions = List.map parse_suggest_cmd files in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None ->
      begin match files_and_regions with
      | (file, _)::_ -> Some file
      | _ -> failwith "Expected at least one file"
      end
  ) in
  let files_and_regions = List.map (fun (file, region) ->
    expand_path file, region
  ) files_and_regions in
  let request = ServerProt.Request.SUGGEST files_and_regions in
  match connect_and_make_request option_values root request with
  | ServerProt.Response.SUGGEST suggestion_map ->
    SMap.iter (fun file result ->
      match result with
      | Ok insertions ->
        let content = Sys_utils.cat file in
        let lines = Str.split_delim (Str.regexp "\n") content in
        let new_content = Reason.do_patch lines insertions in
        let patch_content = Diff.diff_of_file_and_string file new_content in
        Printf.printf "%s\n%s" file patch_content
      | Error msg -> prerr_endlinef "Could not fill types for %s\n%s" file msg
    ) suggestion_map;
    flush stdout
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
