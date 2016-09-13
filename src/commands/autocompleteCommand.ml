(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(***********************************************************************)
(* flow autocomplete command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "autocomplete";
  doc = "Queries autocompletion information";
  usage = Printf.sprintf
    "Usage: %s autocomplete [OPTION] [FILE] [LINE COLUMN]...\n\n\
      Queries autocompletion information.\n\n\
      If line and column is specified, then the magic autocomplete token is\n\
      automatically inserted at the specified position.\n\n\
      Example usage:\n\
      \t%s autocomplete < foo.js\n\
      \t%s autocomplete path/to/foo.js < foo.js
      \t%s autocomplete 12 35 < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> json_flags
    |> strip_root_flag
    |> anon "args" (optional (list_of string)) ~doc:"[FILE] [LINE COL]"
  )
}

let add_autocomplete_token contents line column =
  let (line, column) = convert_input_pos (line, column) in
  let line = line - 1 in
  Line.transform_nth contents line (fun line_str ->
    let length = String.length line_str in
    if length >= column
    then (
      let start = String.sub line_str 0 column in
      let end_ = String.sub line_str column (length - column) in
      start ^ Autocomplete_js.autocomplete_suffix ^ end_
    ) else line_str
  )

let parse_args = function
  | None
  | Some [] ->
      ServerProt.FileContent (None,
                              Sys_utils.read_stdin_to_string ())
  | Some [filename] ->
      let filename = get_path_of_file filename in
      ServerProt.FileContent (Some filename,
                              Sys_utils.read_stdin_to_string ())
  | Some [line; column] ->
      let line = int_of_string line in
      let column = int_of_string column in
      let contents = Sys_utils.read_stdin_to_string () in
      ServerProt.FileContent (None,
                              add_autocomplete_token contents line column)
  | Some [filename; line; column] ->
      let line = int_of_string line in
      let column = int_of_string column in
      let contents = Sys_utils.read_stdin_to_string () in
      let filename = get_path_of_file filename in
      ServerProt.FileContent (Some filename,
                              add_autocomplete_token contents line column)
  | _ ->
      CommandSpec.usage spec;
      FlowExitStatus.(exit Commandline_usage_error)

let main option_values root json pretty strip_root args () =
  let file = parse_args args in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> ServerProt.path_of_input file
  ) in
  let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
  let strip_root = strip_root || FlowConfig.(flowconfig.options.Opts.strip_root) in
  let loc_preprocessor = if strip_root
    then Reason.strip_root_from_loc root
    else fun loc -> loc in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc (ServerProt.AUTOCOMPLETE file);
  let results = (Timeout.input_value ic : ServerProt.autocomplete_response) in
  if json || pretty
  then (
    let open Hh_json in
    let json = match results with
    | Err error ->
      JSON_Object [
        "error", JSON_String error;
        "result", JSON_Array []; (* TODO: remove this? kept for BC *)
      ]
    | OK completions ->
      let results = List.map
        (AutocompleteService_js.autocomplete_result_to_json loc_preprocessor)
        completions
      in
      JSON_Object ["result", JSON_Array results]
    in
    print_endline (json_to_string ~pretty json)
  ) else (
    match results with
    | Err error ->
      prerr_endlinef "Error: %s" error
    | OK completions ->
      List.iter (fun res ->
        let name = res.AutocompleteService_js.res_name in
        let ty = res.AutocompleteService_js.res_ty in
        print_endline (Printf.sprintf "%s %s" name ty)
      ) completions
  )

let command = CommandSpec.command spec main
