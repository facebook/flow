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
(* flow check-contents command *)
(***********************************************************************)

module Json = Hh_json

open CommandUtils

type env = {
  file : ServerProt.file_input;
  option_values : command_params;
}

let parse_args () =
  let option_values, options = create_command_options true in
  let options = sort_opts options in
  let usage = Printf.sprintf
    "Usage: %s check-contents [OPTION]... [FILE]\n\n\
      e.g. %s check-contents < foo.js\n\n"
      Sys.argv.(0)
      Sys.argv.(0) in
  let args = ClientArgs.parse_without_command options usage "check-contents" in
  let file = match args with
    | [] ->
        let contents = ClientUtils.read_stdin_to_string () in
        ServerProt.FileContent (None, contents)
    | [file] ->
        let contents = ClientUtils.read_stdin_to_string () in
        ServerProt.FileContent ((Some (get_path_of_file file)), contents)
    | _ ->
        Arg.usage options usage; exit 2
  in
  { file; option_values; }

let main {file; option_values;} =
  let root = guess_root (ServerProt.path_of_input file) in
  let ic, oc = connect_with_autostart option_values root in
  ServerProt.cmd_to_channel oc (ServerProt.CHECK_FILE file);
  let response = ServerProt.response_from_channel ic in
  let use_json = !(option_values.json) in
  match response with
  | ServerProt.ERRORS e ->
      if use_json || !(option_values.from) <> ""
      then Errors_js.print_errorl use_json e stdout
      else (
        let show_all = !(option_values.show_all_errors) in
        Errors_js.print_error_summary (not show_all) e;
        exit 2
      )
  | ServerProt.NO_ERRORS ->
      Errors_js.print_errorl use_json [] stdout;
      exit 0
  | _ ->
      prerr_endline "Unexpected server response!";
      exit (-1)

let run () = main (parse_args ())
