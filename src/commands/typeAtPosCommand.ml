(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow type-at-pos command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec =
  {
    CommandSpec.name = "type-at-pos";
    doc = "Shows the type at a given file and position";
    usage =
      Printf.sprintf
        "Usage: %s type-at-pos [OPTION]... [FILE] LINE COLUMN\n\ne.g. %s type-at-pos foo.js 12 3\nor   %s type-at-pos 12 3 < foo.js\n"
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
        |> verbose_flags
        |> from_flag
        |> path_flag
        |> wait_for_recheck_flag
        |> flag
             "--expand-json-output"
             truthy
             ~doc:"Includes an expanded version of the returned JSON type (implies --json)"
        |> flag
             "--omit-typearg-defaults"
             truthy
             ~doc:"Omit type arguments when defaults exist and match the provided type argument"
        |> flag "--max-depth" (required ~default:50 int) ~doc:"Maximum depth of type (default 50)"
        |> flag "--verbose-normalizer" truthy ~doc:"Print verbose info during normalization"
        |> flag
             "--do_not_use_typed_AST_for_imports"
             truthy
             ~doc:"" (* internal flag for regression purposes *)
        |> anon "args" (required (list_of string))
      );
  }

let handle_response ~file_contents ~pretty ~strip_root response =
  let (ServerProt.Response.Infer_type_response { loc; tys; documentation }) = response in
  match tys with
  | ServerProt.Response.Infer_type_JSON json ->
    let open Hh_json in
    let open Reason in
    let offset_table =
      Base.Option.map file_contents ~f:(Offset_utils.make ~kind:Offset_utils.Utf8)
    in
    let json_assoc =
      ("types", json)
      :: ("reasons", JSON_Array [])
      :: ("loc", json_of_loc ~strip_root ~offset_table loc)
      :: Flow_errors_utils.deprecated_json_props_of_loc ~strip_root loc
    in
    let json_assoc =
      match documentation with
      | Some doc -> ("documentation", JSON_String doc) :: json_assoc
      | None -> json_assoc
    in
    let json = JSON_Object json_assoc in
    print_json_endline ~pretty json
  | ServerProt.Response.Infer_type_string tys ->
    let ty =
      match tys with
      | Some result -> result
      | _ -> "(unknown)"
    in
    let doc =
      match documentation with
      | Some doc -> doc ^ "\n"
      | None -> ""
    in
    let range =
      if loc = Loc.none then
        ""
      else
        spf "\n%s" (Reason.range_string_of_loc ~strip_root loc)
    in
    print_endline (doc ^ ty ^ range)

let handle_error err ~json ~pretty =
  if json then
    Hh_json.(
      let json = JSON_Object [("error", JSON_String err)] in
      prerr_json_endline ~pretty json
    )
  else
    prerr_endline err

let main
    base_flags
    option_values
    json
    pretty
    root
    strip_root
    verbose
    path
    wait_for_recheck
    expanded
    omit_targ_defaults
    max_depth
    verbose_normalizer
    no_typed_ast_for_imports
    args
    () =
  let json = json || pretty || expanded in
  let (file, line, column) = parse_location_with_optional_filename spec path args in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = find_a_root ~base_flags ~input:file root in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  if (not json) && verbose <> None then
    prerr_endline "NOTE: --verbose writes to the server log file";
  let options =
    {
      ServerProt.Infer_type_options.input = file;
      line;
      char = column;
      verbose;
      omit_targ_defaults;
      wait_for_recheck;
      verbose_normalizer;
      max_depth;
      json;
      strip_root;
      expanded;
      no_typed_ast_for_imports;
    }
  in
  let request = ServerProt.Request.INFER_TYPE options in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.INFER_TYPE (Error err) -> handle_error err ~json ~pretty
  | ServerProt.Response.INFER_TYPE (Ok resp) ->
    let file_contents = File_input.content_of_file_input file |> Base.Result.ok in
    handle_response resp ~file_contents ~pretty ~strip_root
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
