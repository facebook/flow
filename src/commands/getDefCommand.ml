(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow get-def command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "get-def";
    doc = "Gets the definition location of a variable or property";
    usage =
      Printf.sprintf
        "Usage: %s get-def [OPTION]... [FILE] LINE COLUMN\n\ne.g. %s get-def foo.js 12 3\nor   %s get-def 12 3 < foo.js\n"
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
        |> path_flag
        |> wait_for_recheck_flag
        |> anon "args" (required (list_of string)));
  }

let parse_args path args =
  let (file, line, column) =
    match args with
    | [file; line; column] ->
      let file = expand_path file in
      (File_input.FileName file, int_of_string line, int_of_string column)
    | [line; column] ->
      ( get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path None,
        int_of_string line,
        int_of_string column )
    | _ ->
      CommandSpec.usage spec;
      Exit.(exit Commandline_usage_error)
  in
  let (line, column) = convert_input_pos (line, column) in
  (file, line, column)

(* get-def command handler.
   - json toggles JSON output
   - strip_root toggles whether output positions are relativized w.r.t. root
   - path is a user-specified path to use as incoming content source path
   - args is mandatory command args; see parse_args above
*)
let main base_flags option_values json pretty root strip_root path wait_for_recheck args () =
  let (file, line, column) = parse_args path args in
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
  let request =
    ServerProt.Request.GET_DEF { filename = file; line; char = column; wait_for_recheck }
  in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.GET_DEF (Ok loc) ->
    (* format output *)
    if json || pretty then
      (* TODO: this format is deprecated but can't be backwards-compatible.
         should be replaced with just `Reason.json_of_loc loc`. *)
      Hh_json.(
        let json = JSON_Object (Errors.deprecated_json_props_of_loc ~strip_root loc) in
        print_json_endline ~pretty json)
    else
      let from = FlowEventLogger.get_from_I_AM_A_CLOWN () in
      if from = Some "vim" || from = Some "emacs" then
        print_endline (Errors.Vim_emacs_output.string_of_loc ~strip_root loc)
      else
        print_endline (range_string_of_loc ~strip_root loc)
  | ServerProt.Response.GET_DEF (Error exn_msg) ->
    let file_str =
      match File_input.filename_of_file_input file with
      | "-" as s -> s
      | s -> File_key.SourceFile s |> Reason.string_of_source ~strip_root
    in
    Utils_js.prerr_endlinef "Could not get definition for %s:%d:%d\n%s" file_str line column exn_msg
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
