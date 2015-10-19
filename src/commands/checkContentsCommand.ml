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
    |> error_flags
    |> json_flags
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

let main option_values error_flags use_json file () =
  let file = read_from_stdin file in
  let root = guess_root (ServerProt.path_of_input file) in
  let ic, oc = connect option_values root in
  ServerProt.cmd_to_channel oc (ServerProt.CHECK_FILE file);
  let response = ServerProt.response_from_channel ic in
  match response with
  | ServerProt.ERRORS e ->
      if use_json
      then Errors_js.print_error_json stdout e
      else (
        let stdin_file = match file with
          | ServerProt.FileContent (None, contents) -> Some ("-", contents)
          | ServerProt.FileContent (Some (path), contents) -> Some (path, contents)
          | _ -> None
        in
        Errors_js.print_error_summary
          ~flags:error_flags
          ~stdin_file:stdin_file
          ~root
          e;
        FlowExitStatus.(exit Type_error)
      )
  | ServerProt.NO_ERRORS ->
      if use_json
      then Errors_js.print_error_json stdout []
      else Printf.printf "No errors!\n%!";
      FlowExitStatus.(exit Ok)
  | _ ->
      let msg = "Unexpected server response!" in
      FlowExitStatus.(exit ~msg Unknown_error)

let command = CommandSpec.command spec main
