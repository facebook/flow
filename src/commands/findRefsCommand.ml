(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow find-refs command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "find-refs";
    doc = "Gets the reference locations of a variable or property";
    usage =
      Printf.sprintf
        "Usage: %s find-refs [OPTION]... [FILE] LINE COLUMN\n\ne.g. %s find-refs foo.js 12 3\nor   %s find-refs 12 3 < foo.js\n"
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
        |> flag "--global" no_arg ~doc:"Search for references in other files (beta)"
        |> flag
             "--multi-hop"
             no_arg
             ~doc:"Include references on related object types (implies `--global`; experimental)"
        |> anon "args" (required (list_of string)));
  }

let parse_args path args =
  let (file, line, column) =
    match args with
    | [file; line; column] ->
      let file = expand_path file in
      (File_input.FileName file, int_of_string line, int_of_string column)
    | [line; column] ->
      ( get_file_from_filename_or_stdin path ~cmd:CommandSpec.(spec.name) None,
        int_of_string line,
        int_of_string column )
    | _ ->
      CommandSpec.usage spec;
      FlowExitStatus.(exit Commandline_usage_error)
  in
  let (line, column) = convert_input_pos (line, column) in
  (file, line, column)

let print_json result ~stdin_file ~pretty ~strip_root =
  Hh_json.(
    let json =
      match result with
      | None -> JSON_Object [("kind", JSON_String "no-symbol-found")]
      | Some (name, locs) ->
        JSON_Object
          [ ("kind", JSON_String "symbol-found");
            ("name", JSON_String name);
            ( "locs",
              JSON_Array (Core_list.map ~f:(json_of_loc_with_offset ~stdin_file ~strip_root) locs)
            ) ]
    in
    print_json_endline ~pretty json)

let to_string result ~strip_root =
  let locs =
    match result with
    | None -> []
    | Some (_, locs) -> locs
  in
  let from = FlowEventLogger.get_from_I_AM_A_CLOWN () in
  String.concat "\n"
  @@
  if from = Some "vim" || from = Some "emacs" then
    Core_list.map ~f:(Errors.Vim_emacs_output.string_of_loc ~strip_root) locs
  else
    Core_list.map ~f:(range_string_of_loc ~strip_root) locs

(* find-refs command handler.
   - json toggles JSON output
   - strip_root toggles whether output positions are relativized w.r.t. root
   - path is a user-specified path to use as incoming content source path
   - global indicates whether to search for references in different files (much slower)
   - multi_hop indicates whether to include properties on related objects (even slower)
   - args is mandatory command args; see parse_args above
    *)
let main base_flags option_values json pretty root strip_root path global multi_hop args () =
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
    ServerProt.Request.FIND_REFS { filename = file; line; char = column; global; multi_hop }
  in
  (* command result will be a position structure with full file path *)
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.FIND_REFS (Ok result) ->
    (* format output *)
    if json || pretty then
      print_json result ~stdin_file:file ~pretty ~strip_root
    else
      print_endline (to_string result ~strip_root)
  | ServerProt.Response.FIND_REFS (Error exn_msg) ->
    Utils_js.prerr_endlinef
      "Could not find refs for %s:%d:%d\n%s"
      (File_input.filename_of_file_input file)
      line
      column
      exn_msg
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
