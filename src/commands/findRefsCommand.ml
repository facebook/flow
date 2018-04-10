(**
 * Copyright (c) 2014, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow find-refs command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "find-refs";
  doc = "Gets the reference locations of a variable or property";
  usage = Printf.sprintf
        "Usage: %s find-refs [OPTION]... [FILE] LINE COLUMN\n\n\
      e.g. %s find-refs foo.js 12 3\n\
      or   %s find-refs 12 3 < foo.js\n"
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
    |> flag "--global" no_arg ~doc:"Search for references in other files (beta)"
    |> anon "args" (required (list_of string))
  )
}

let parse_args path args =
  let (file, line, column) = match args with
    | [file; line; column] ->
      let file = expand_path file in
      File_input.FileName file, (int_of_string line), (int_of_string column)
    | [line; column] ->
      get_file_from_filename_or_stdin path ~cmd:CommandSpec.(spec.name) None,
      (int_of_string line),
      (int_of_string column)
    | _ ->
      CommandSpec.usage spec;
      FlowExitStatus.(exit Commandline_usage_error)
  in
  let (line, column) = convert_input_pos (line, column) in
  file, line, column

let print_json result ~pretty ~strip_root =
  let open Hh_json in
  let json = match result with
    | None -> JSON_Object ["kind", JSON_String "no-symbol-found"]
    | Some (name, locs) ->
      JSON_Object [
        "kind", JSON_String "symbol-found";
        "name", JSON_String name;
        "locs", JSON_Array (List.map (Reason.json_of_loc ~strip_root) locs)
      ]
  in
  print_json_endline ~pretty json

let to_string result option_values ~strip_root =
  let locs = match result with
    | None -> []
    | Some (_, locs) -> locs
  in
  String.concat "\n" @@
    if option_values.from = "vim" || option_values.from = "emacs"
    then List.map (Errors.Vim_emacs_output.string_of_loc ~strip_root) locs
    else List.map (range_string_of_loc ~strip_root) locs


    (* find-refs command handler.
   - json toggles JSON output
   - strip_root toggles whether output positions are relativized w.r.t. root
   - path is a user-specified path to use as incoming content source path
   - global indicates whether to search for references in different files (much slower)
   - args is mandatory command args; see parse_args above
    *)
let main option_values json pretty root strip_root from path global args () =
  FlowEventLogger.set_from from;
  let (file, line, column) = parse_args path args in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in
  let strip_root = if strip_root then Some root else None in

  let request = ServerProt.Request.FIND_REFS (file, line, column, global) in
  (* command result will be a position structure with full file path *)
  match connect_and_make_request option_values root request with
  | ServerProt.Response.FIND_REFS (Ok result) ->
    (* format output *)
    if json || pretty
    then print_json result ~pretty ~strip_root
    else print_endline (to_string result option_values ~strip_root)
  | ServerProt.Response.FIND_REFS (Error exn_msg) ->
    Utils_js.prerr_endlinef
      "Could not find refs for %s:%d:%d\n%s"
      (File_input.filename_of_file_input file) line column exn_msg
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
