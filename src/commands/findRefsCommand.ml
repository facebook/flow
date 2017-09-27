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
    |> server_flags
    |> root_flag
    |> json_flags
    |> strip_root_flag
    |> from_flag
    |> flag "--path" (optional string)
        ~doc:"Specify (fake) path to file when reading data from stdin"
    |> anon "args" (required (list_of string))
        ~doc:"[FILE] LINE COL"
  )
}

let parse_args path args =
  let (file, line, column) = match args with
    | [file; line; column] ->
      let file = expand_path file in
      File_input.FileName file, (int_of_string line), (int_of_string column)
    | [line; column] ->
      get_file_from_filename_or_stdin path None,
      (int_of_string line),
      (int_of_string column)
    | _ ->
      CommandSpec.usage spec;
      FlowExitStatus.(exit Commandline_usage_error)
  in
  let (line, column) = convert_input_pos (line, column) in
  file, line, column

    (* find-refs command handler.
   - json toggles JSON output
   - strip_root toggles whether output positions are relativized w.r.t. root
   - path is a user-specified path to use as incoming content source path
   - args is mandatory command args; see parse_args above
    *)
let main option_values root json pretty strip_root from path args () =
  FlowEventLogger.set_from from;
  let (file, line, column) = parse_args path args in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in
  let strip_root = if strip_root then Some root else None in
  (* connect to server *)
  let ic, oc = connect option_values root in
  (* dispatch command *)
  send_command oc (ServerProt.FIND_REFS (file, line, column));
  (* command result will be a position structure with full file path *)
  let response: ServerProt.find_refs_response = Timeout.input_value ic in
  match response with
  | Ok locs ->
    (* format output *)
    print_endline @@
      if json || pretty
      then Hh_json.(json_to_string ~pretty @@
        JSON_Array (List.map (Reason.json_of_loc ~strip_root) locs)
      )
      else String.concat "\n" @@
        if option_values.from = "vim" || option_values.from = "emacs"
        then List.map (Errors.Vim_emacs_output.string_of_loc ~strip_root) locs
        else List.map (range_string_of_loc ~strip_root) locs
  | Error exn_msg ->
    Utils_js.prerr_endlinef
      "Could not find refs for %s:%d:%d\n%s"
      (File_input.filename_of_file_input file) line column exn_msg


let command = CommandSpec.command spec main
