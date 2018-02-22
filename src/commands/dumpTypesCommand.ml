(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow dump-types command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "dump-types";
  doc = ""; (* Outputs list of all types in the file *)
  usage = Printf.sprintf
    "Usage: %s dump-types [OPTION]... [FILE]\n\n\
      e.g. %s dump-types foo.js\n\
      or   %s dump-types < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_and_json_flags
    |> root_flag
    |> strip_root_flag
    |> from_flag
    |> path_flag
    |> anon "file" (optional string) ~doc:"[FILE]"
  )
}

let types_to_json types ~strip_root =
  let open Hh_json in
  let open Reason in
  let types_json = types |> List.map (fun (loc, t) ->
    let json_assoc = (
      ("type", JSON_String t) ::
      ("reasons", JSON_Array []) ::
      ("loc", json_of_loc ~strip_root loc) ::
      (Errors.deprecated_json_props_of_loc ~strip_root loc)
    ) in
    JSON_Object json_assoc
  ) in
  JSON_Array types_json

let handle_response types ~json ~pretty ~strip_root =
  if json
  then (
    let types_json = types_to_json types ~strip_root in
    Hh_json.print_json_endline ~pretty types_json
  ) else (
    let out = types
      |> List.map (fun (loc, str) ->
        (spf "%s: %s" (Reason.string_of_loc ~strip_root loc) str)
      )
      |> String.concat "\n"
    in
    print_endline out
  )

let handle_error err ~json ~pretty ~strip_root =
  if json
  then (
    let open Hh_json in
    let error_json = JSON_Object ["error", JSON_String err] in
    prerr_json_endline ~pretty error_json;
    (* also output an empty array on stdout, for JSON parsers *)
    handle_response [] ~json ~pretty ~strip_root
  ) else (
    prerr_endline err
  )

let main option_values json pretty root strip_root from path filename () =
  FlowEventLogger.set_from from;
  let json = json || pretty in
  let file = get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name)
    path filename in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in

  let strip_root = if strip_root then Some root else None in

  let request = ServerProt.Request.DUMP_TYPES file in

  match connect_and_make_request option_values root request with
  | ServerProt.Response.DUMP_TYPES (Error err) ->
    handle_error err ~json ~pretty ~strip_root
  | ServerProt.Response.DUMP_TYPES (Ok resp) ->
    handle_response resp ~json ~pretty ~strip_root
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
