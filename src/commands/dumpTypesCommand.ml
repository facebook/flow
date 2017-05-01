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
(* flow dump-types command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let spec = {
  CommandSpec.
  name = "dump-types";
  doc = ""; (* Outputs list of all types in the file *)
  usage = Printf.sprintf
    "Usage: %s dump-types [OPTION]... [FILE]\n\n\
      e.g. %s dump-types foo.js\n\
      or   %s dump-types < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> json_flags
    |> strip_root_flag
    |> flag "--path" (optional string)
        ~doc:"Specify (fake) path to file when reading data from stdin"
    |> flag "--raw" no_arg
        ~doc:"Output raw representation of types (implies --json)"
    |> anon "file" (optional string) ~doc:"[FILE]"
  )
}

let types_to_json types ~strip_root =
  let open Hh_json in
  let open Reason in
  let types_json = types |> List.map (fun (loc, _ctor, str, raw_t, reasons) ->
    let json_assoc = (
      ("type", JSON_String str) ::
      ("reasons", JSON_Array (List.map (fun r ->
        let r_loc = loc_of_reason r in
        let r_def_loc = def_loc_of_reason r in
        JSON_Object (
          ("desc", JSON_String (string_of_desc (desc_of_reason r))) ::
          ("loc", json_of_loc ~strip_root r_loc) ::
          ((if r_def_loc = r_loc then [] else [
            "def_loc", json_of_loc ~strip_root r_def_loc
          ]) @ (Errors.deprecated_json_props_of_loc ~strip_root r_loc))
        )
      ) reasons)) ::
      ("loc", json_of_loc ~strip_root loc) ::
      (Errors.deprecated_json_props_of_loc ~strip_root loc)
    ) in
    let json_assoc = match raw_t with
      | None -> json_assoc
      | Some raw_t -> ("raw_type", JSON_String raw_t) :: json_assoc
    in
    JSON_Object json_assoc
  ) in
  JSON_Array types_json

let handle_response types ~json ~pretty ~strip_root =
  if json
  then (
    let types_json = types_to_json types ~strip_root in
    print_endline (Hh_json.json_to_string ~pretty types_json)
  ) else (
    let out = types
      |> List.map (fun (loc, _, str, _, _) ->
        (spf "%s: %s" (Reason.string_of_loc ~strip_root loc) str)
      )
      |> String.concat "\n"
    in
    print_endline out
  )

let handle_error (loc, err) ~json ~pretty ~strip_root =
  if json
  then (
    let open Hh_json in
    let error_json = JSON_Object (
      ("error", JSON_String err) ::
      ("loc", Reason.json_of_loc ~strip_root loc) ::
      (Errors.deprecated_json_props_of_loc ~strip_root loc)
    ) in
    prerr_endline (json_to_string ~pretty error_json);
    (* also output an empty array on stdout, for JSON parsers *)
    handle_response [] ~json ~pretty ~strip_root
  ) else (
    let loc = Reason.string_of_loc ~strip_root loc in
    prerr_endlinef "%s:\n%s" loc err
  )

let main option_values root json pretty strip_root path include_raw filename () =
  let json = json || pretty || include_raw in
  let file = get_file_from_filename_or_stdin path filename in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> ServerProt.path_of_input file
  ) in

  let strip_root = if strip_root then Some root else None in

  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc
    (ServerProt.DUMP_TYPES (file, include_raw, strip_root));

  match (Timeout.input_value ic : ServerProt.dump_types_response) with
  | Err err ->
      handle_error err ~json ~pretty ~strip_root
  | OK resp ->
      handle_response resp ~json ~pretty ~strip_root

let command = CommandSpec.command spec main
