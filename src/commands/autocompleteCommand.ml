(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow autocomplete command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec =
  {
    CommandSpec.name = "autocomplete";
    doc = "Queries autocompletion information";
    usage =
      Printf.sprintf
        "Usage: %s autocomplete [OPTION] [FILE] [LINE COLUMN]...\n\nQueries autocompletion information.\n\nIf line and column is specified, then the magic autocomplete token is\nautomatically inserted at the specified position.\n\nExample usage:\n\t%s autocomplete < foo.js\n\t%s autocomplete path/to/foo.js < foo.js
      \t%s autocomplete 12 35 < foo.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> root_flag
        |> strip_root_flag
        |> from_flag
        |> wait_for_recheck_flag
        |> anon "args" (optional (list_of string)));
  }

let parse_args = function
  | None
  | Some [] ->
    File_input.FileContent (None, Sys_utils.read_stdin_to_string ())
  | Some [filename] ->
    let filename = get_path_of_file filename in
    File_input.FileContent (Some filename, Sys_utils.read_stdin_to_string ())
  | Some [line; column] ->
    let line = int_of_string line in
    let column = int_of_string column in
    let contents = Sys_utils.read_stdin_to_string () in
    let (line, column) = convert_input_pos (line, column) in
    File_input.FileContent
      (None, AutocompleteService_js.add_autocomplete_token contents line column)
  | Some [filename; line; column] ->
    let line = int_of_string line in
    let column = int_of_string column in
    let contents = Sys_utils.read_stdin_to_string () in
    let filename = get_path_of_file filename in
    let (line, column) = convert_input_pos (line, column) in
    File_input.FileContent
      (Some filename, AutocompleteService_js.add_autocomplete_token contents line column)
  | _ ->
    CommandSpec.usage spec;
    FlowExitStatus.(exit Commandline_usage_error)

let main base_flags option_values json pretty root strip_root wait_for_recheck args () =
  let file = parse_args args in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match root with
      | Some root -> Some root
      | None -> File_input.path_of_file_input file)
  in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  let request = ServerProt.Request.AUTOCOMPLETE { input = file; wait_for_recheck } in
  let results =
    match connect_and_make_request flowconfig_name option_values root request with
    | ServerProt.Response.AUTOCOMPLETE response -> response
    | response -> failwith_bad_response ~request ~response
  in
  if json || pretty then
    results
    |> AutocompleteService_js.autocomplete_response_to_json ~strip_root
    |> Hh_json.print_json_endline ~pretty
  else
    match results with
    | Error error -> prerr_endlinef "Error: %s" error
    | Ok completions ->
      List.iter
        (fun res ->
          let name = res.ServerProt.Response.res_name in
          let (_ty_loc, ty) = res.ServerProt.Response.res_ty in
          print_endline (Printf.sprintf "%s %s" name ty))
        completions

let command = CommandSpec.command spec main
