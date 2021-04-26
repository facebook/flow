(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow check-contents command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "check-contents";
    doc = "Run typechecker on contents from stdin";
    usage =
      Printf.sprintf
        "Usage: %s check-contents [OPTION]... [FILE]\n\nRuns a flow check on the contents of stdin. If FILE is provided, then\ncheck-contents pretends that the contents of stdin come from FILE\n\ne.g. %s check-contents < foo.js\nor   %s check-contents foo.js < foo.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> json_version_flag
        |> offset_style_flag
        |> root_flag
        |> error_flags
        |> strip_root_flag
        |> verbose_flags
        |> from_flag
        |> wait_for_recheck_flag
        |> flag "--respect-pragma" no_arg ~doc:"" (* deprecated *)
        |> flag "--all" no_arg ~doc:"Ignore absence of an @flow pragma"
        |> anon "filename" (optional string));
  }

let main
    base_flags
    option_values
    json
    pretty
    json_version
    offset_style
    root
    error_flags
    strip_root
    verbose
    wait_for_recheck
    respect_pragma
    all
    file
    () =
  let file = get_file_from_filename_or_stdin file ~cmd:CommandSpec.(spec.name) None in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match root with
      | Some root -> Some root
      | None -> File_input.path_of_file_input file)
  in
  (* pretty implies json *)
  let json = json || Base.Option.is_some json_version || pretty in
  let offset_kind = CommandUtils.offset_kind_of_offset_style offset_style in
  if (not option_values.quiet) && verbose <> None then
    prerr_endline "NOTE: --verbose writes to the server log file";

  if (not option_values.quiet) && all && respect_pragma then
    prerr_endline "Warning: --all and --respect-pragma cannot be used together. --all wins.";

  (* TODO: --respect-pragma is deprecated. We will soon flip the default. As a
     transition, --all defaults to enabled. To maintain the current behavior
     going forward, callers should add --all, which currently is a no-op.
     Once we flip the default, --respect-pragma will have no effect and will
     be removed. *)
  let all = all || not respect_pragma in
  let include_warnings = error_flags.Errors.Cli_output.include_warnings in
  let request =
    ServerProt.Request.CHECK_FILE
      { input = file; verbose; force = all; include_warnings; wait_for_recheck }
  in
  let response =
    match connect_and_make_request flowconfig_name option_values root request with
    | ServerProt.Response.CHECK_FILE response -> response
    | response -> failwith_bad_response ~request ~response
  in
  let stdin_file =
    match file with
    | File_input.FileContent (None, contents) -> Some (Path.make_unsafe "-", contents)
    | File_input.FileContent (Some path, contents) -> Some (Path.make path, contents)
    | _ -> None
  in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  let print_json =
    Errors.Json_output.print_errors
      ~out_channel:stdout
      ~strip_root
      ~pretty
      ?version:json_version
      ~offset_kind
      ~stdin_file
  in
  match response with
  | ServerProt.Response.ERRORS { errors; warnings; suppressed_errors } ->
    if json then
      print_json ~errors ~warnings ~suppressed_errors ()
    else (
      Errors.Cli_output.print_errors
        ~out_channel:stdout
        ~flags:error_flags
        ~stdin_file
        ~strip_root
        ~errors
        ~warnings
        ~lazy_msg:None
        ();

      (* Return a successful exit code if there were only warnings. *)
      Exit.(
        exit
          (get_check_or_status_exit_code errors warnings error_flags.Errors.Cli_output.max_warnings))
    )
  | ServerProt.Response.NO_ERRORS ->
    if json then
      print_json
        ~errors:Errors.ConcreteLocPrintableErrorSet.empty
        ~warnings:Errors.ConcreteLocPrintableErrorSet.empty
        ~suppressed_errors:[]
        ()
    else
      Printf.printf "No errors!\n%!";
    Exit.(exit No_error)
  | ServerProt.Response.NOT_COVERED ->
    if json then
      print_json
        ~errors:Errors.ConcreteLocPrintableErrorSet.empty
        ~warnings:Errors.ConcreteLocPrintableErrorSet.empty
        ~suppressed_errors:[]
        ()
    else
      Printf.printf "File is not @flow!\n%!";
    Exit.(exit No_error)
  | _ ->
    let msg = "Unexpected server response!" in
    Exit.(exit ~msg Unknown_error)

let command = CommandSpec.command spec main
