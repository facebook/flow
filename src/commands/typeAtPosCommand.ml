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
    |> server_flags
    |> root_flag
    |> json_flags
    |> strip_root_flag
    |> verbose_flags
    |> flag "--path" (optional string)
        ~doc:"Specify (fake) path to file when reading data from stdin"
    |> flag "--raw" no_arg
        ~doc:"Output raw represenation of type (implies --json)"
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
      ServerProt.FileName file, line, column
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

let handle_response (loc, t, raw_t, reasons) ~json ~pretty ~strip_root =
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
    let json_assoc = match raw_t with
      | None -> json_assoc
      | Some raw_t -> ("raw_type", JSON_String raw_t) :: json_assoc
    in
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

let handle_error (loc, err) ~json ~pretty ~strip_root =
  if json
  then (
    let open Hh_json in
    let json = JSON_Object (
      ("error", JSON_String err) ::
      ("loc", Reason.json_of_loc ~strip_root loc) ::
      (Errors.deprecated_json_props_of_loc ~strip_root loc)
    ) in
    prerr_endline (json_to_string ~pretty json)
  ) else (
    let loc = Reason.string_of_loc ~strip_root loc in
    prerr_endlinef "%s:\n%s" loc err
  )

let main option_values root json pretty strip_root verbose path include_raw args () =
  let json = json || pretty || include_raw in
  let (file, line, column) = parse_args path args in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> ServerProt.path_of_input file
  ) in
  let strip_root = if strip_root then Some root else None in

  if not json && (verbose <> None)
  then prerr_endline "NOTE: --verbose writes to the server log file";

  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc
    (ServerProt.INFER_TYPE (file, line, column, verbose, include_raw));
  match (Timeout.input_value ic : ServerProt.infer_type_response) with
  | Err err -> handle_error err ~json ~pretty ~strip_root
  | OK resp -> handle_response resp ~json ~pretty ~strip_root

let command = CommandSpec.command spec main
