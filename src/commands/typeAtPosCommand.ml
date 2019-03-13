(**
 * Copyright (c) Facebook, Inc. and its affiliates.
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
    |> base_flags
    |> connect_and_json_flags
    |> root_flag
    |> strip_root_flag
    |> verbose_flags
    |> from_flag
    |> path_flag
    |> wait_for_recheck_flag
    |> flag "--expand-json-output" no_arg
        ~doc:"Includes an expanded version of the returned JSON type (implies --json)"
    |> flag "--expand-type-aliases" no_arg
        ~doc:"Replace type aliases with their bodies"
    |> flag "--omit-typearg-defaults" no_arg
        ~doc:"Omit type arguments when defaults exist and match the provided type argument"
    |> anon "args" (required (list_of string))
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
      get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path None,
      line,
      column
  | _ ->
      exit ()
  in
  let (line, column) = convert_input_pos (line, column) in
  file, line, column

let handle_response (loc, t) ~file_contents ~json ~pretty ~strip_root ~expanded =
  let ty = match t with
    | None -> "(unknown)"
    | Some ty -> Ty_printer.string_of_t ty
  in
  if json
  then (
    let open Hh_json in
    let open Reason in
    let offset_table = Option.map file_contents ~f:Offset_utils.make in
    let json_assoc = (
        ("type", JSON_String ty) ::
        ("reasons", JSON_Array []) ::
        ("loc", json_of_loc ~strip_root ~offset_table loc) ::
        (Errors.deprecated_json_props_of_loc ~strip_root loc)
    ) in
    let json_assoc =
      if expanded then
        ("expanded_type", match t with
        | Some ty -> Ty_debug.json_of_t ~strip_root ty
        | None -> JSON_Null) :: json_assoc
      else json_assoc in
    let json = JSON_Object json_assoc in
    print_json_endline ~pretty json
  ) else (
    let range =
      if loc = Loc.none then ""
      else spf "\n%s" (range_string_of_loc ~strip_root loc)
    in
    print_endline (ty^range)
  )

let handle_error err ~json ~pretty =
  if json
  then (
    let open Hh_json in
    let json = JSON_Object ["error", JSON_String err] in
    prerr_json_endline ~pretty json
  ) else (
    prerr_endline err
  )

let main base_flags option_values json pretty root strip_root verbose path wait_for_recheck expanded
    expand_aliases omit_targ_defaults args () =
  let json = json || pretty || expanded in
  let (file, line, column) = parse_args path args in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = guess_root flowconfig_name (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in
  let strip_root = if strip_root then Some root else None in

  if not json && (verbose <> None)
  then prerr_endline "NOTE: --verbose writes to the server log file";

  let request = ServerProt.Request.INFER_TYPE {
    input = file;
    line;
    char = column;
    verbose;
    expand_aliases;
    omit_targ_defaults;
    wait_for_recheck;
  } in
  let file_contents = File_input.content_of_file_input file |> Core_result.ok in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.INFER_TYPE (Error err) -> handle_error err ~json ~pretty
  | ServerProt.Response.INFER_TYPE (Ok resp) ->
    handle_response resp ~file_contents ~json ~pretty ~strip_root ~expanded
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
