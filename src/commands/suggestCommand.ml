(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow suggest (infer types for file) command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "suggest";
    doc = "Provides type annotation suggestions for a given program";
    usage =
      Printf.sprintf
        "Usage: %s suggest [OPTION]... [FILE]\n\nPrints a prettified version of the input program with suggested type\nannotations filling in missing function parameters and return types.\n\ne.g. %s suggest file.js\nor   %s suggest < file.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_flags
        |> root_flag
        |> error_flags
        |> strip_root_flag
        |> from_flag
        |> path_flag
        |> wait_for_recheck_flag
        |> flag
             "--fail-on-tc-errors"
             no_arg
             ~doc:"Fail on typechecking errors (similar behavior to \"check\")"
        |> flag
             "--fail-on-suggest-warnings"
             no_arg
             ~doc:"Fail on suggest warnings (inferred empty type or normalizer failures)"
        |> anon "file" (optional string));
  }

let handle_error err =
  prerr_endline err;
  FlowExitStatus.(exit Unknown_error)

let handle_response strip_root error_flags fail_on_tc_errors fail_on_suggest_warnings content =
  let with_errors_and_warnings do_step errors warnings max_warnings next =
    let err () =
      Errors.Cli_output.print_errors
        ~out_channel:stderr
        ~flags:error_flags
        ~strip_root
        ~errors
        ~warnings
        ~lazy_msg:None
        ();
      FlowExitStatus.exit (get_check_or_status_exit_code errors warnings max_warnings)
    in
    if not do_step then
      next ()
    else if Errors.ConcreteLocPrintableErrorSet.is_empty errors then
      match max_warnings with
      | Some x when Errors.ConcreteLocPrintableErrorSet.cardinal warnings > x -> err ()
      | None
      | Some _ ->
        next ()
    else
      err ()
  in
  function
  | ServerProt.Response.Suggest_Ok { tc_errors; tc_warnings; suggest_warnings; file_patch } ->
    (* First, see if the command should fail on a typechecking error. Use
     * tc_errors and tc_warnings with the `with_errors_and_warnings` defined
     * eralier. *)
    with_errors_and_warnings
      fail_on_tc_errors
      tc_errors
      tc_warnings
      error_flags.Errors.Cli_output.max_warnings
    @@ fun () ->
    (* Then, check the case where we should fail on suggest-related warnings.
     * Use suggest_errors as warnings for `with_errors_and_warnings` and an
     * empty warning set. *)
    with_errors_and_warnings
      fail_on_suggest_warnings
      Errors.ConcreteLocPrintableErrorSet.empty
      suggest_warnings
      (Some 0)
    @@ fun () ->
    (* Finally, print the AST if no error has been flagged. *)
    print_string @@ Replacement_printer.print file_patch content
  | ServerProt.Response.Suggest_Error errors ->
    (* This is the case of a parse fail (no context is created). The `errors`
     * set ought to be non-empty. Otherwise, we throw an exception. If this
     * happens, see types_js.ml `typecheck_contents`. *)
    with_errors_and_warnings true errors Errors.ConcreteLocPrintableErrorSet.empty None @@ fun () ->
    failwith "SuggestCommand: Parsing failed with no errors"

let main
    base_flags
    option_values
    root
    error_flags
    strip_root
    path
    wait_for_recheck
    fail_on_tc_errors
    fail_on_suggest_warnings
    filename
    () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let file =
    get_file_from_filename_or_stdin
      ~cmd:CommandSpec.(spec.name)
      path
      (Base.Option.map ~f:expand_path filename)
  in
  File_input.(
    let content =
      match content_of_file_input file with
      | Ok content -> content
      (* If the File_input is from stdin we would have been in the previous line.
         If the File_input is from a file then expand_path verified the file exists. *)
      | _ ->
        let msg = Printf.sprintf "Failed to open file: %s" @@ filename_of_file_input file in
        FlowExitStatus.(exit ~msg Input_error)
    in
    let file = FileContent (path_of_file_input file, content) in
    let root = find_a_root ~base_flags ~input:file root in
    let strip_root =
      if strip_root then
        Some root
      else
        None
    in
    let request = ServerProt.Request.SUGGEST { input = file; wait_for_recheck } in
    match connect_and_make_request flowconfig_name option_values root request with
    | ServerProt.Response.SUGGEST (Ok result) ->
      handle_response
        strip_root
        error_flags
        fail_on_tc_errors
        fail_on_suggest_warnings
        content
        result;
      flush stdout
    | ServerProt.Response.SUGGEST (Error error) -> handle_error error
    | response -> failwith_bad_response ~request ~response)

let command = CommandSpec.command spec main
