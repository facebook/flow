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
    |> from_flag
    |> flag "--respect-pragma" no_arg ~doc:"" (* deprecated *)
    |> flag "--all" no_arg ~doc:"Ignore absence of an @flow pragma"
    |> anon "filename" (optional string) ~doc:"Filename"
  )
}

let main option_values root error_flags strip_root json pretty verbose from
  respect_pragma all file () =
  FlowEventLogger.set_from from;
  let file = get_file_from_filename_or_stdin file None in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in

  let ic, oc = connect option_values root in

  (* pretty implies json *)
  let json = json || pretty in

  if not json && (verbose <> None)
  then prerr_endline "NOTE: --verbose writes to the server log file";

  if not json && all && respect_pragma then prerr_endline
    "Warning: --all and --respect-pragma cannot be used together. --all wins.";

  (* TODO: --respect-pragma is deprecated. We will soon flip the default. As a
     transition, --all defaults to enabled. To maintain the current behavior
     going forward, callers should add --all, which currently is a no-op.
     Once we flip the default, --respect-pragma will have no effect and will
     be removed. *)
  let all = all || not respect_pragma in

  let include_warnings = error_flags.Errors.Cli_output.include_warnings in

  send_command oc (ServerProt.CHECK_FILE (file, verbose, all, include_warnings));
  let response = wait_for_response ic in
  let stdin_file = match file with
    | File_input.FileContent (None, contents) ->
        Some (Path.make_unsafe "-", contents)
    | File_input.FileContent (Some path, contents) ->
        Some (Path.make path, contents)
    | _ -> None
  in
  let strip_root = if strip_root then Some root else None in
  let print_json = Errors.Json_output.print_errors
    ~out_channel:stdout ~strip_root ~pretty ~stdin_file
    ~suppressed_errors:([]) in
  match response with
  | ServerProt.ERRORS {errors; warnings} ->
      if json
      then
        print_json ~errors ~warnings ()
      else (
        Errors.Cli_output.print_errors
          ~out_channel:stdout
          ~flags:error_flags
          ~stdin_file
          ~strip_root
          ~errors
          ~warnings
          ();
        (* Return a successful exit code if there were only warnings. *)
        let open FlowExitStatus in
        if Errors.ErrorSet.is_empty errors then exit No_error else exit Type_error
      )
  | ServerProt.NO_ERRORS ->
      if json then
        print_json ~errors:Errors.ErrorSet.empty ~warnings:Errors.ErrorSet.empty ()
      else Printf.printf "No errors!\n%!";
      FlowExitStatus.(exit No_error)
  | ServerProt.NOT_COVERED ->
      if json then
        print_json ~errors:Errors.ErrorSet.empty ~warnings:Errors.ErrorSet.empty ()
      else Printf.printf "File is not @flow!\n%!";
      FlowExitStatus.(exit No_error)
  | _ ->
      let msg = "Unexpected server response!" in
      FlowExitStatus.(exit ~msg Unknown_error)

let command = CommandSpec.command spec main
