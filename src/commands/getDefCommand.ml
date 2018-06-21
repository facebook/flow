(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow get-def command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "get-def";
  doc = "Gets the definition location of a variable or property";
  usage = Printf.sprintf
    "Usage: %s get-def [OPTION]... [FILE] LINE COLUMN\n\n\
      e.g. %s get-def foo.js 12 3\n\
      or   %s get-def 12 3 < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> connect_and_json_flags
    |> root_flag
    |> strip_root_flag
    |> from_flag
    |> path_flag
    |> anon "args" (required (list_of string))
  )
}

let parse_args path args =
  let (file, line, column) = match args with
  | [file; line; column] ->
      let file = expand_path file in
      File_input.FileName file, (int_of_string line), (int_of_string column)
  | [line; column] ->
      get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path None,
      (int_of_string line),
      (int_of_string column)
  | _ ->
      CommandSpec.usage spec;
      FlowExitStatus.(exit Commandline_usage_error)
  in
  let (line, column) = convert_input_pos (line, column) in
  file, line, column

(* get-def command handler.
   - json toggles JSON output
   - strip_root toggles whether output positions are relativized w.r.t. root
   - path is a user-specified path to use as incoming content source path
   - args is mandatory command args; see parse_args above
 *)
let main option_values json pretty root strip_root from path args () =
  FlowEventLogger.set_from from;
  let (file, line, column) = parse_args path args in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in
  let strip_root = if strip_root then Some root else None in

  let request = ServerProt.Request.GET_DEF (file, line, column) in

  match connect_and_make_request option_values root request with
  | ServerProt.Response.GET_DEF (Ok loc) ->
    (* format output *)
    if json || pretty
    then (
      (* TODO: this format is deprecated but can't be backwards-compatible.
         should be replaced with just `Reason.json_of_loc loc`. *)
      let open Hh_json in
      let json =
        JSON_Object (Errors.deprecated_json_props_of_loc ~strip_root loc) in
      print_json_endline ~pretty json
    ) else
      if option_values.from = "vim" || option_values.from = "emacs"
      then print_endline (Errors.Vim_emacs_output.string_of_loc ~strip_root loc)
      else print_endline (range_string_of_loc ~strip_root loc)
  | ServerProt.Response.GET_DEF (Error exn_msg) ->
      Utils_js.prerr_endlinef
        "Could not get definition for %s:%d:%d\n%s"
        (File_input.filename_of_file_input file) line column
        exn_msg
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
