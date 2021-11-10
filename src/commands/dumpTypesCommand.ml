(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow dump-types command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec =
  {
    CommandSpec.name = "dump-types";
    doc = "";
    (* Outputs list of all types in the file *)
    usage =
      Printf.sprintf
        "Usage: %s dump-types [OPTION]... [FILE]\n\ne.g. %s dump-types foo.js\nor   %s dump-types < foo.js\n"
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
        |> flag
             "--evaluate-type-destructors"
             no_arg
             ~doc:"Use the result of type destructor evaluation if available"
        |> anon "file" (optional string)
      );
  }

let types_to_json ~file_content types ~strip_root =
  Hh_json.(
    Reason.(
      let offset_table =
        Base.Option.map file_content ~f:(Offset_utils.make ~kind:Offset_utils.Utf8)
      in
      let types_json =
        types
        |> Base.List.map ~f:(fun (loc, t) ->
               let json_assoc =
                 ("type", JSON_String t)
                 ::
                 ("reasons", JSON_Array [])
                 ::
                 ("loc", json_of_loc ~strip_root ~offset_table loc)
                 :: Errors.deprecated_json_props_of_loc ~strip_root loc
               in
               JSON_Object json_assoc
           )
      in
      JSON_Array types_json
    )
  )

let handle_response types ~json ~file_content ~pretty ~strip_root =
  if json then
    let types_json = types_to_json ~file_content types ~strip_root in
    Hh_json.print_json_endline ~pretty types_json
  else
    let out =
      types
      |> Base.List.map ~f:(fun (loc, str) -> spf "%s: %s" (Reason.string_of_loc ~strip_root loc) str)
      |> String.concat "\n"
    in
    print_endline out

let handle_error err ~file_content ~json ~pretty ~strip_root =
  if json then (
    Hh_json.(
      let error_json = JSON_Object [("error", JSON_String err)] in
      prerr_json_endline ~pretty error_json;

      (* also output an empty array on stdout, for JSON parsers *)
      handle_response [] ~file_content ~json ~pretty ~strip_root
    )
  ) else
    prerr_endline err

let main
    base_flags
    option_values
    json
    pretty
    root
    strip_root
    path
    wait_for_recheck
    evaluate_type_destructors
    filename
    () =
  let json = json || pretty in
  let file = get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path filename in
  let file_content = File_input.content_of_file_input file |> Base.Result.ok in
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
    ServerProt.Request.DUMP_TYPES { input = file; evaluate_type_destructors; wait_for_recheck }
  in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.DUMP_TYPES (Error err) ->
    handle_error err ~file_content ~json ~pretty ~strip_root
  | ServerProt.Response.DUMP_TYPES (Ok resp) ->
    handle_response resp ~file_content ~json ~pretty ~strip_root
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
