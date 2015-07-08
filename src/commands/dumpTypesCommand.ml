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

module Json = Hh_json

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
    |> json_flags
    |> flag "--strip-root" no_arg
        ~doc:"Print paths without the root"
    |> flag "--path" (optional string)
        ~doc:"Specify (fake) path to file when reading data from stdin"
    |> anon "file" (optional string) ~doc:"[FILE]"
  )
}

let get_file path = function
  | Some filename ->
      ServerProt.FileName (expand_path filename)
  | None ->
      let contents = Sys_utils.read_stdin_to_string () in
      let filename = (match path with
        | Some ""
        | None -> None
        | Some str -> Some (get_path_of_file str)
      ) in
      ServerProt.FileContent (filename, contents)

let string_of_loc loc = Loc.(
  match loc.source with
  | None -> ""
  | Some file ->
    let line = loc.start.line in
    let start = loc.start.column + 1 in
    let end_ = loc._end.column in
    if line <= 0 then
      Utils.spf "%s:0:0" file
    else if line = loc._end.line && start - end_ = 1 then
      Utils.spf "%s:%d:%d" file line start
    else
      Utils.spf "%s:%d:%d-%d" file line start end_
)

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

let handle_response types json strip =
  if json
  then (
    let lst = types |> List.map (fun (loc, str, reasons) ->
      let loc = strip loc in
      Json.JAssoc (
        ("type", Json.JString str) ::
        ("reasons", Json.JList (List.map (fun r ->
          Json.JAssoc (
            ("desc", Json.JString (Reason_js.desc_of_reason r)) ::
            (Errors_js.json_of_loc (strip (Reason_js.loc_of_reason r)))
          )
        ) reasons)) ::
        (Errors_js.json_of_loc loc)
      )
    ) in
    let json = Json.json_to_string (Json.JList lst) in
    output_string stdout (json^"\n")
  ) else (
    let out = types
      |> List.map (fun (loc, str, reasons) ->
        let loc = strip loc in
        (Utils.spf "%s: %s" (string_of_loc loc) str)
      )
      |> String.concat "\n"
    in
    output_string stdout (out^"\n")
  );
  flush stdout

let main option_values json strip_root path filename () =
  let file = get_file path filename in
  let root = guess_root (ServerProt.path_of_input file) in
  let ic, oc = connect_with_autostart option_values root in
  ServerProt.cmd_to_channel oc
    (ServerProt.DUMP_TYPES file);

  match (Marshal.from_channel ic) with
  | (Some err, None) -> handle_error err json (relativize strip_root root)
  | (None, Some resp) -> handle_response resp json (relativize strip_root root)
  | (_, _) -> assert false

let command = CommandSpec.command spec main
