(**
 * Copyright (c) 2014, Facebook, Inc.
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

module Json = Hh_json

open CommandUtils

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
    |> flag "--path" (optional string)
        ~doc:"Specify (fake) path to file when reading data from stdin"
    |> flag "--raw" no_arg
        ~doc:"Output raw represenation of type (implies --json)"
    |> anon "args" (required (list_of string)) ~doc:"[FILE] LINE COL"
  )
}

let parse_args path args =
  let (file, line, column) = match args with
  | [file; line; column] ->
      let file = expand_path file in
      ServerProt.FileName file, (int_of_string line), (int_of_string column)
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

let handle_response (loc, t, raw_t, reasons) json strip =
  let ty = match t with
    | None -> "(unknown)"
    | Some str -> str
  in
  let loc = strip loc in
  if json
  then (
    let loc = Errors_js.json_of_loc loc in
    let json_assoc = (
        ("type", Json.JString ty) ::
        ("reasons", Json.JList
          (List.map (fun r ->
              Json.JAssoc (
                  ("desc", Json.JString (Reason_js.desc_of_reason r)) ::
                  (Errors_js.json_of_loc (strip (Reason_js.loc_of_reason r)))
                )
            ) reasons)) ::
        loc
      ) in
    let json_assoc = match raw_t with
      | None -> json_assoc
      | Some raw_t -> ("raw_type", Json.JString raw_t) :: json_assoc
    in
    let json = Json.JAssoc json_assoc in
    let json = Json.json_to_string json in
    output_string stdout (json^"\n")
  ) else (
    let range =
      if loc = Loc.none then ""
      else Utils.spf "\n%s" (range_string_of_loc loc)
    in
    let pty =
      if reasons = [] then ""
      else "\n\nSee the following locations:\n" ^ (
        reasons
        |> List.map (fun r ->
             Reason_js.repos_reason (strip (Reason_js.loc_of_reason r)) r
           )
        |> List.map Reason_js.string_of_reason
        |> String.concat "\n"
      )
    in
    output_string stdout (ty^range^pty^"\n")
  );
  flush stdout

let handle_error (loc, err) json strip =
  let loc = strip loc in
  if json
  then (
    let loc = Errors_js.json_of_loc loc in
    let json = Json.JAssoc (("error", Json.JString err) :: loc) in
    output_string stderr ((Json.json_to_string json)^"\n");
  ) else (
    let loc = Reason_js.string_of_loc loc in
    output_string stderr (Utils.spf "%s:\n%s\n" loc err);
  );
  flush stderr

let main option_values root json strip_root path include_raw args () =
  let json = json || include_raw in
  let (file, line, column) = parse_args path args in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> ServerProt.path_of_input file
  ) in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc
    (ServerProt.INFER_TYPE (file, line, column, include_raw));
  match (Marshal.from_channel ic) with
  | (Some err, None) -> handle_error err json (relativize strip_root root)
  | (None, Some resp) -> handle_response resp json (relativize strip_root root)
  | (_, _) -> failwith "Oops"

let command = CommandSpec.command spec main
