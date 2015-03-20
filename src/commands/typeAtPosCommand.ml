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

type env = {
  file : ServerProt.file_input;
  line : int;
  column : int;
  option_values : command_params;
}

let parse_args () =
  let option_values, options = create_command_options true in
  let path = ref "" in
  let options = (
    "--path", Arg.Set_string path,
      " Specify (fake) path to file when reading data from stdin"
    ) :: options in
  let options = sort_opts options in
  let usage = Printf.sprintf
    "Usage: %s type-at-pos [OPTION]... [FILE] LINE COLUMN\n\n\
      e.g. %s type-at-pos foo.js 12 3\n\
      or   %s type-at-pos 12 3 < foo.js\n\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name in
  let args = ClientArgs.parse_without_command options usage "type-at-pos" in
  let (file, line, column) = match args with
  | [file; line; column] ->
      let file = ClientCheck.expand_path file in
      ServerProt.FileName file, (int_of_string line), (int_of_string column)
  | [line; column] ->
      let contents = ClientUtils.read_stdin_to_string () in
      let filename =
        if not (!path = "")
        then Some (get_path_of_file !path)
        else None
      in
      ServerProt.FileContent (filename, contents),
      (int_of_string line),
      (int_of_string column)
  | _ ->
      Arg.usage options usage; exit 2 in
  let (line, column) = convert_input_pos (line, column) in
  { file; line; column; option_values; }

let handle_response (pos, t, reasons) option_values =
  let ty = match t with
    | None -> "(unknown)"
    | Some str -> str
  in
  if !(option_values.json)
  then (
    let pos = Errors_js.pos_to_json pos in
    let json = Json.JAssoc (
        ("type", Json.JString ty) ::
        ("reasons", Json.JList
          (List.map (fun r ->
              Json.JAssoc (
                  ("desc", Json.JString (Reason_js.desc_of_reason r)) ::
                  (Errors_js.pos_to_json (Reason_js.pos_of_reason r))
                )
            ) reasons)) ::
        pos
      ) in
    let json = Json.json_to_string json in
    output_string stdout (json^"\n")
  ) else (
    let range =
      if pos = Pos.none then ""
      else (
        let file = Relative_path.to_absolute Pos.(pos.pos_file) in
        let l0, c0, l1, c1 = Errors_js.pos_range pos in
        (Utils.spf "\n%s:%d:%d,%d:%d" file l0 c0 l1 c1)
      ) in
    let pty =
      if reasons = [] then ""
      else "\n\nSee the following locations:\n" ^ (
        reasons
        |> List.map Reason_js.string_of_reason
        |> String.concat "\n"
      )
    in
    output_string stdout (ty^range^pty^"\n")
  );
  flush stdout

let handle_error (pos, err) option_values =
  if !(option_values.json)
  then (
    let pos = Errors_js.pos_to_json pos in
    let json = Json.JAssoc (("error", Json.JString err) :: pos) in
    output_string stderr ((Json.json_to_string json)^"\n");
  ) else (
    let pos = Reason_js.string_of_pos pos in
    output_string stderr (Utils.spf "%s:\n%s\n" pos err);
  );
  flush stderr

let main {file; line; column; option_values;} =
  let root = guess_root (ServerProt.path_of_input file) in
  let ic, oc = connect_with_autostart option_values root in
  ServerProt.cmd_to_channel oc
    (ServerProt.INFER_TYPE (file, line, column));
  match (Marshal.from_channel ic) with
  | (Some err, None) -> handle_error err option_values
  | (None, Some resp) -> handle_response resp option_values
  | (_, _) -> failwith "Oops"

let name = "type-at-pos"
let doc = "Shows the type at a given file and position"
let run () = main (parse_args ())
