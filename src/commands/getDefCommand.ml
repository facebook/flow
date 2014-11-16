(**
 *  Copyright 2014 Facebook.
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *)

(***********************************************************************)
(* flow get-def command *)
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
    "Usage: %s get-def [OPTION]... [FILE] LINE COLUMN\n\n\
      e.g. %s get-def foo.js 12 3\n\
      or   %s get-def 12 3 < foo.js\n\n"
      Sys.argv.(0)
      Sys.argv.(0)
      Sys.argv.(0) in
  let args = ClientArgs.parse_without_command options usage "get-def" in
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

let main {file; line; column; option_values;} =
  let root = guess_root (ServerProt.path_of_input file) in
  let ic, oc = connect_with_autostart option_values root in
  ServerProt.cmd_to_channel oc
    (ServerProt.GET_DEF (file, line, column));
  let pos = Marshal.from_channel ic in
  if !(option_values.json)
  then (
    let json = Json.JAssoc (Errors_js.pos_to_json pos) in
    let json = Json.json_to_string json in
    print_endline json;
  ) else (
    let file = Pos.(pos.pos_file) in
    let l0, c0, l1, c1 = Errors_js.pos_range pos in
    print_endline (Utils.spf "%s:%d:%d,%d:%d" file l0 c0 l1 c1)
  )

let run () = main (parse_args ())
