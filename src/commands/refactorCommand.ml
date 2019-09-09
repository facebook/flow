(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow refactor command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "refactor";
    doc = "Provides refactoring capabilities (early alpha)";
    usage =
      Printf.sprintf
        "Usage: %s refactor [OPTION]... [FILE] LINE COLUMN\n\ne.g. %s refactor foo.js 12 3 --rename newName\n"
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
        |> flag "--rename" (optional string) ~doc:"Renames the symbol to the name given"
        |> anon "args" (required (list_of string)));
  }

let usage error =
  print_endline error;
  print_newline ();
  CommandSpec.usage spec;
  FlowExitStatus.(exit Commandline_usage_error)

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
    ServerProt.Response.(
      let json_of_edit (loc, text) =
        JSON_Object
          [ ("oldRange", json_of_loc_with_offset ~stdin_file ~strip_root loc);
            ("newText", JSON_String text) ]
      in
      let json =
        match result with
        (* TODO might be nice to provide details when this happens *)
        | None -> JSON_Object [("kind", JSON_String "no-refactor-performed")]
        | Some { refactor_edits } ->
          JSON_Object
            [ ("kind", JSON_String "refactor-performed");
              ("edits", JSON_Array (Core_list.map ~f:json_of_edit refactor_edits)) ]
      in
      print_json_endline ~pretty json))

let to_string result ~strip_root =
  ServerProt.Response.(
    let edits =
      match result with
      | None -> []
      | Some { refactor_edits } -> refactor_edits
    in
    let string_of_loc =
      let from = FlowEventLogger.get_from_I_AM_A_CLOWN () in
      if from = Some "vim" || from = Some "emacs" then
        Errors.Vim_emacs_output.string_of_loc ~strip_root
      else
        range_string_of_loc ~strip_root
    in
    let string_of_edit (loc, new_text) = Printf.sprintf "%s: %s" (string_of_loc loc) new_text in
    String.concat "\n" @@ Core_list.map ~f:string_of_edit edits)

let main base_flags option_values json pretty root strip_root path rename args () =
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
  let refactor_variant =
    match rename with
    | Some new_name -> ServerProt.Request.RENAME new_name
    | None -> usage "The kind of refactor (e.g. rename) must be specified with a flag"
  in
  let request =
    ServerProt.Request.REFACTOR { input = file; line; char = column; refactor_variant }
  in
  (* command result will be a position structure with full file path *)
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.REFACTOR (Ok result) ->
    (* format output *)
    if json || pretty then
      print_json result ~stdin_file:file ~pretty ~strip_root
    else
      print_endline (to_string result ~strip_root)
  | ServerProt.Response.REFACTOR (Error exn_msg) ->
    Utils_js.prerr_endlinef
      "Could not refactor for %s:%d:%d\n%s"
      (File_input.filename_of_file_input file)
      line
      column
      exn_msg
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
