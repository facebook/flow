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
(* flow check-contents command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "check-contents";
  doc = "Run typechecker on contents from stdin";
  usage = Printf.sprintf
    "Usage: %s check-contents [OPTION]... [FILE]\n\n\
      Runs a flow check on the contents of stdin. If FILE is provided, then\n\
      check-contents pretends that the contents of stdin come from FILE\n\n\
      e.g. %s check-contents < foo.js\n\
      or   %s check-contents foo.js < foo.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> server_flags
    |> root_flag
    |> error_flags
    |> strip_root_flag
    |> json_flags
    |> verbose_flags
    |> flag "--respect-pragma" no_arg
        ~doc:"Respect the presence or absence of an @flow pragma"
    |> anon "filename" (optional string) ~doc:"Filename"
  )
}

let main option_values root error_flags strip_root use_json verbose respect_pragma file () =
  let file = get_file_from_filename_or_stdin file None in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> ServerProt.path_of_input file
  ) in

  let flowconfig = FlowConfig.get (Server_files_js.config_file root) in
  let strip_root = strip_root || FlowConfig.(flowconfig.options.Opts.strip_root) in
  let ic, oc = connect option_values root in

  if not use_json && (verbose <> None)
  then prerr_endline "NOTE: --verbose writes to the server log file";

  ServerProt.cmd_to_channel oc (ServerProt.CHECK_FILE (file, verbose, respect_pragma));
  let response = ServerProt.response_from_channel ic in
  let stdin_file = match file with
    | ServerProt.FileContent (None, contents) ->
        Some (Path.make_unsafe "-", contents)
    | ServerProt.FileContent (Some path, contents) ->
        Some (Path.make path, contents)
    | _ -> None
  in
  match response with
  | ServerProt.ERRORS e ->
      if use_json
      then
        Errors.print_error_json ~root ~stdin_file stdout e
      else (
        Errors.print_error_summary
          ~flags:error_flags
          ~stdin_file
          ~strip_root
          ~root
          e;
        FlowExitStatus.(exit Type_error)
      )
  | ServerProt.NO_ERRORS ->
      if use_json
      then Errors.print_error_json ~root ~stdin_file stdout []
      else Printf.printf "No errors!\n%!";
      FlowExitStatus.(exit No_error)
  | _ ->
      let msg = "Unexpected server response!" in
      FlowExitStatus.(exit ~msg Unknown_error)

let command = CommandSpec.command spec main
