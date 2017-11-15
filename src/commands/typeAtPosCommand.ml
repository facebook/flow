(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow type-at-pos command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "type-at-pos";
  doc = "Shows the type at a given file and position";
  usage = Printf.sprintf
    "Usage: %s type-at-pos [OPTION]... [FILE] LINE COLUMN\n\n\
      e.g. %s type-at-pos foo.js 12 3\n\
      or   %s type-at-pos 12 3 < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_and_json_flags
    |> root_flag
    |> strip_root_flag
    |> verbose_flags
    |> from_flag
    |> flag "--path" (optional string)
        ~doc:"Specify (fake) path to file when reading data from stdin"
    |> anon "args" (required (list_of string)) ~doc:"[FILE] LINE COL"
  )
}

let exit () =
    CommandSpec.usage spec;
    FlowExitStatus.(exit Commandline_usage_error)

let parse_line_and_column line column =
    try (int_of_string line), (int_of_string column)
    with Failure(_) -> exit ()

let parse_args path args =
  let (file, line, column) = match args with
  | [file; line; column] ->
      let file = expand_path file in
      let line, column = parse_line_and_column line column in
      File_input.FileName file, line, column
  | [line; column] ->
      let line, column = parse_line_and_column line column in
      get_file_from_filename_or_stdin path None,
      line,
      column
  | _ ->
      exit ()
  in
  let (line, column) = convert_input_pos (line, column) in
  file, line, column

let handle_response (loc, t, reasons) ~json ~pretty ~strip_root =
  let ty = match t with
    | None -> "(unknown)"
    | Some str -> str
  in
  if json
  then (
    let open Hh_json in
    let open Reason in
    let json_assoc = (
        ("type", JSON_String ty) ::
        ("reasons", JSON_Array
          (List.map (fun r ->
              let r_loc = loc_of_reason r in
              JSON_Object (
                  ("desc", JSON_String (string_of_desc (desc_of_reason r))) ::
                  ("loc", json_of_loc ~strip_root r_loc) ::
                  (Errors.deprecated_json_props_of_loc ~strip_root r_loc)
                )
            ) reasons)) ::
        ("loc", json_of_loc ~strip_root loc) ::
        (Errors.deprecated_json_props_of_loc ~strip_root loc)
    ) in
    let json = JSON_Object json_assoc in
    print_endline (json_to_string ~pretty json)
  ) else (
    let range =
      if loc = Loc.none then ""
      else spf "\n%s" (range_string_of_loc ~strip_root loc)
    in
    let pty =
      if reasons = [] then ""
      else "\n\nSee the following locations:\n" ^ (
        reasons
        |> List.map (Reason.string_of_reason ~strip_root)
        |> String.concat "\n"
      )
    in
    print_endline (ty^range^pty)
  )

let handle_error err ~json ~pretty =
  if json
  then (
    let open Hh_json in
    let json = JSON_Object ["error", JSON_String err] in
    prerr_endline (json_to_string ~pretty json)
  ) else (
    prerr_endline err
  )

let main option_values json pretty root strip_root verbose from path args () =
  FlowEventLogger.set_from from;
  let json = json || pretty in
  let (file, line, column) = parse_args path args in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in
  let strip_root = if strip_root then Some root else None in

  if not json && (verbose <> None)
  then prerr_endline "NOTE: --verbose writes to the server log file";

  let request = ServerProt.Request.INFER_TYPE (file, line, column, verbose) in
  match connect_and_make_request option_values root request with
  | ServerProt.Response.INFER_TYPE (Error err) -> handle_error err ~json ~pretty
  | ServerProt.Response.INFER_TYPE (Ok resp) -> handle_response resp ~json ~pretty ~strip_root
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
