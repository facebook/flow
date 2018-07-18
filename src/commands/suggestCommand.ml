(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow suggest (infer types for file) command *)
(***********************************************************************)

open CommandUtils

let spec = {
  CommandSpec.
  name = "suggest";
  doc = "Provides type annotation suggestions for a given program";
  usage = Printf.sprintf
    "Usage: %s suggest [OPTION]... [FILE]\n\n\
      Prints a prettified version of the input program with suggested type\n\
      annotations filling in missing function parameters and return types.\n\n\
      e.g. %s suggest file.js\n\
      or   %s suggest < file.js\n"
      CommandUtils.exe_name
      CommandUtils.exe_name
      CommandUtils.exe_name;
  args = CommandSpec.ArgSpec.(
    empty
    |> connect_flags
    |> root_flag
    |> error_flags
    |> strip_root_flag
    |> from_flag
    |> path_flag
    |> flag "--fail-on-tc-errors" no_arg
        ~doc:"Fail on typechecking errors (similar behavior to \"check\")"
    |> flag "--fail-on-suggest-warnings" no_arg
        ~doc:"Fail on suggest warnings (inferred empty type or normalizer failures)"
    |> anon "file" (optional string)
  )
}


let handle_error err =
  prerr_endline err;
  FlowExitStatus.(exit Unknown_error)

let layout_prettier ast =
  let attached_comments = Flow_prettier_comments.attach_comments ast in
  Js_layout_generator.with_attached_comments := Some attached_comments ;
  let layout = Js_layout_generator.program_simple ast in
  Js_layout_generator.with_attached_comments := None ;
  layout

let print_annotated_program annot_ast =
  annot_ast
  |> layout_prettier
  |> Pretty_printer.print ~source_maps:None
  |> Source.contents
  |> print_endline

let handle_response strip_root error_flags fail_on_tc_errors fail_on_suggest_warnings =
  let with_errors_and_warnings do_step errors warnings max_warnings next =
    let err () =
      Errors.Cli_output.print_errors ~out_channel:stderr ~flags:error_flags
        ~strip_root ~errors ~warnings ~lazy_msg:None ();
      FlowExitStatus.exit
        (get_check_or_status_exit_code errors warnings max_warnings)
    in
    if not do_step then next ()
    else if Errors.ErrorSet.is_empty errors then begin
      match max_warnings with
      | Some x when Errors.ErrorSet.cardinal warnings > x -> err ()
      | None | Some _ -> next ()
    end
    else err ()
  in
  function
  | ServerProt.Response.Suggest_Ok {
      tc_errors; tc_warnings; suggest_warnings; annotated_program
    } ->
    (* First, see if the command should fail on a typechecking error. Use
     * tc_errors and tc_warnings with the `with_errors_and_warnings` defined
     * eralier. *)
    with_errors_and_warnings fail_on_tc_errors tc_errors tc_warnings
      error_flags.Errors.Cli_output.max_warnings @@ fun () ->
    (* Then, check the case where we should fail on suggest-related warnings.
     * Use suggest_errors as warnings for `with_errors_and_warnings` and an
     * empty warning set. *)
    with_errors_and_warnings fail_on_suggest_warnings Errors.ErrorSet.empty
      suggest_warnings (Some 0) @@ fun () ->
    (* Finally, print the AST if no error has been flagged. *)
    print_annotated_program annotated_program
  | ServerProt.Response.Suggest_Error errors ->
    (* This is the case of a parse fail (no context is created). The `errors`
     * set ought to be non-empty. Otherwise, we throw an exception. If this
     * happens, see types_js.ml `typecheck_contents`. *)
    with_errors_and_warnings true errors Errors.ErrorSet.empty None @@ fun () ->
    failwith "SuggestCommand: Parsing failed with no errors"

let main option_values root error_flags strip_root from path
  fail_on_tc_errors fail_on_suggest_warnings filename () =
  FlowEventLogger.set_from from;
  let file = get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name)
    path (Option.map ~f:expand_path filename) in
  let root = guess_root (
    match root with
    | Some root -> Some root
    | None -> File_input.path_of_file_input file
  ) in
  let strip_root = if strip_root then Some root else None in
  let request = ServerProt.Request.SUGGEST file in
  match connect_and_make_request option_values root request with
  | ServerProt.Response.SUGGEST (Ok result) ->
    handle_response strip_root error_flags fail_on_tc_errors fail_on_suggest_warnings result;
    flush stdout
  | ServerProt.Response.SUGGEST (Error error) ->
    handle_error error
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
