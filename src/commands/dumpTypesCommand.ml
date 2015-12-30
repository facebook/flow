(**
 * Copyright (c) 2015, Facebook, Inc.
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

let handle_error (loc, err) json strip =
  let loc = strip loc in
  if json
  then (
    let loc = Errors_js.json_of_loc loc in
    let json = Hh_json.JSON_Object (("error", Hh_json.JSON_String err) :: loc) in
    output_string stderr ((Hh_json.json_to_string json)^"\n");
  ) else (
    let loc = Reason_js.string_of_loc loc in
    output_string stderr (Utils.spf "%s:\n%s\n" loc err);
  );
  flush stderr

let handle_response types json strip =
  if json
  then (
    let lst = types |> List.map (fun (loc, ctor, str, raw_t, reasons) ->
      let loc = strip loc in
      let json_assoc = (
        ("type", Hh_json.JSON_String str) ::
        ("reasons", Hh_json.JSON_Array (List.map (fun r ->
          Hh_json.JSON_Object (
            ("desc", Hh_json.JSON_String (Reason_js.desc_of_reason r)) ::
            (Errors_js.json_of_loc (strip (Reason_js.loc_of_reason r)))
          )
        ) reasons)) ::
        (Errors_js.json_of_loc loc)
      ) in
      let json_assoc = match raw_t with
        | None -> json_assoc
        | Some raw_t -> ("raw_type", Hh_json.JSON_String raw_t) :: json_assoc
      in
      Hh_json.JSON_Object json_assoc
    ) in
    let json = Hh_json.json_to_string (Hh_json.JSON_Array lst) in
    output_string stdout (json^"\n")
  ) else (
    let out = types
      |> List.map (fun (loc, ctor, str, _, reasons) ->
        let loc = strip loc in
        (Utils.spf "%s: %s" (Reason_js.string_of_loc loc) str)
      )
      |> String.concat "\n"
    in
    output_string stdout (out^"\n")
  );
  flush stdout

let main option_values root json strip_root path include_raw filename () =
  let json = json || include_raw in
  let file = get_file_from_filename_or_stdin path filename in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> ServerProt.path_of_input file
  ) in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc
    (ServerProt.DUMP_TYPES (file, include_raw));

  match (Timeout.input_value ic) with
  | (Some err, None) -> handle_error err json (relativize strip_root root)
  | (None, Some resp) -> handle_response resp json (relativize strip_root root)
  | (_, _) -> assert false

let command = CommandSpec.command spec main
