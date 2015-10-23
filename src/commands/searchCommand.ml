(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow search command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "search";
  doc = "Searches a pattern";
  usage = Printf.sprintf
    "Usage: %s search query\n\n\
    Search 'query' through the codebase\n\n"
    CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> json_flags
    |> anon "query" (required string) ~doc:"Query"
  )
}

module Json = Hh_json

let main option_values root use_json query () =
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> Some (Sys.getcwd ())
  ) in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc (ServerProt.SEARCH query);
  let results = Marshal.from_channel ic in
  if use_json
  then (
    let results =
      List.map SearchService_js.result_to_json results
    in
    print_endline (Json.json_to_string (Json.JList results))
  ) else (
    List.iter begin fun term ->
      let term = SearchService_js.result_to_string term in
      print_endline term
    end results
  )

let command = CommandSpec.command spec main
