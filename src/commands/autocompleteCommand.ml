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
(* flow autocomplete command *)
(***********************************************************************)

open CommandUtils

type env = {
  file: ServerProt.file_input;
  option_values : command_params;
}

let add_autocomplete_token contents line column =
  let (line, column) = convert_input_pos (line, column) in
  let line = line - 1 in
  Line.transform_nth contents line (fun line_str ->
    let length = String.length line_str in
    if length >= column
    then (
      let start = String.sub line_str 0 column in
      let end_ = String.sub line_str column (length - column) in
      start ^ Autocomplete_js.autocomplete_suffix ^ end_
    ) else line_str
  )

let parse_args () =
  let option_values, options = create_command_options true in
  let options = sort_opts options in
  let usage =  Printf.sprintf
    "Usage: %s autocomplete [OPTION] [FILE] [LINE COLUMN]...\n\n\
    Queries autocompletion information.\n\n\
    If line and column is specified, then the magic autocomplete token is\n\
    automatically inserted at the specified position.\n\n\
    Example usage:\n\
    \t%s autocomplete < foo.js\n\
    \t%s autocomplete path/to/foo.js < foo.js
    \t%s autocomplete 12 35 < foo.js"
    Sys.argv.(0)
    Sys.argv.(0)
    Sys.argv.(0)
    Sys.argv.(0) in
  let args = ClientArgs.parse_without_command options usage "autocomplete" in
  let file = match args with
  | [] ->
      ServerProt.FileContent (None,
                              ClientUtils.read_stdin_to_string ())
  | [filename] ->
      let filename = get_path_of_file filename in
      ServerProt.FileContent (Some filename,
                              ClientUtils.read_stdin_to_string ())
  | [line; column] ->
      let line = int_of_string line in
      let column = int_of_string column in
      let contents = ClientUtils.read_stdin_to_string () in
      ServerProt.FileContent (None,
                              add_autocomplete_token contents line column)
  | [filename; line; column] ->
      let line = int_of_string line in
      let column = int_of_string column in
      let contents = ClientUtils.read_stdin_to_string () in
      let filename = get_path_of_file filename in
      ServerProt.FileContent (Some filename,
                              add_autocomplete_token contents line column)
  | _ ->
      Arg.usage options usage; exit 2 in
  { file; option_values; }

module Json = Hh_json

let main { file; option_values; } =
  let root = guess_root (ServerProt.path_of_input file) in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc (ServerProt.AUTOCOMPLETE file);
  let completions = Marshal.from_channel ic in
  if !(option_values.json)
  then (
    let results =
      List.map AutocompleteService_js.autocomplete_result_to_json completions
    in
    print_endline (Json.json_to_string (Json.JList results))
  ) else (
    List.iter (fun res ->
      let name = res.AutocompleteService_js.res_name in
      let ty = res.AutocompleteService_js.res_ty in
      print_endline (name^" "^ty)
    ) completions
  )

let run () = main (parse_args ())
