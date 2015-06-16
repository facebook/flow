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

open CommandUtils

let spec = {
  CommandSpec.
  name = "check-contents";
  doc = "Run typechecker on contents from stdin";
  usage = Printf.sprintf
    "Usage: %s check-contents [OPTION]... [FILE]\n\n\
      e.g. %s check-contents < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> json_flags
    |> flag "--show-all-errors" no_arg
        ~doc:"Print all errors (the default is to truncate after 50 errors)"
    |> anon "filename" (optional string) ~doc:"Filename"
  )
}

let read_from_stdin file =
  match file with
    | None ->
        let contents = Sys_utils.read_stdin_to_string () in
        ServerProt.FileContent (None, contents)
    | Some file ->
        let contents = Sys_utils.read_stdin_to_string () in
        ServerProt.FileContent ((Some (get_path_of_file file)), contents)

let main option_values use_json show_all_errors file () =
  let file = read_from_stdin file in
  let root = guess_root (ServerProt.path_of_input file) in
  let ic, oc = connect_with_autostart option_values root in
  ServerProt.cmd_to_channel oc (ServerProt.CHECK_FILE file);
  let response = ServerProt.response_from_channel ic in
  match response with
  | ServerProt.ERRORS e ->
      if use_json || option_values.from <> ""
      then Errors_js.print_errorl use_json e stdout
      else (
        Errors_js.print_error_summary (not show_all_errors) e;
        exit 2
      )
  | ServerProt.NO_ERRORS ->
      Errors_js.print_errorl use_json [] stdout;
      exit 0
  | _ ->
      prerr_endline "Unexpected server response!";
      exit (-1)

let command = CommandSpec.command spec (collect_server_flags main)
