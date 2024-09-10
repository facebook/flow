(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow type-at-pos command *)
(***********************************************************************)

open CommandUtils

let cmd_name = "inlay-hint_unstable_exposed_for_testing"

let spec =
  {
    CommandSpec.name = cmd_name;
    doc =
      "Compute all inlay hints available in a file. This command is only exposed for the purpose of testing Flow. There is no stability guarantee.";
    usage =
      Printf.sprintf
        "Usage: %s %s [OPTION]... [FILE]\n\ne.g. %s %s foo.js\nor   %s %s < foo.js\n"
        CommandUtils.exe_name
        cmd_name
        CommandUtils.exe_name
        cmd_name
        CommandUtils.exe_name
        cmd_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_flags
        |> root_flag
        |> strip_root_flag
        |> verbose_flags
        |> from_flag
        |> path_flag
        |> wait_for_recheck_flag
        |> flag
             "--omit-typearg-defaults"
             truthy
             ~doc:"Omit type arguments when defaults exist and match the provided type argument"
        |> flag "--max-depth" (required ~default:40 int) ~doc:"Maximum depth of type (default 40)"
        |> flag "--verbose-normalizer" truthy ~doc:"Print verbose info during normalization"
        |> flag
             "--do_not_use_typed_AST_for_imports"
             truthy
             ~doc:"" (* internal flag for regression purposes *)
        |> anon "args" (required (list_of string))
      );
  }

let handle_response ~strip_root response =
  let {
    ServerProt.Response.InlayHint.cursor_loc =
      { Loc.source; start = { Loc.line; column }; _end = _ };
    type_loc;
    tys;
    refining_locs;
    refinement_invalidated = _;
    documentation;
  } =
    response
  in
  let cursor_loc_str =
    match source with
    | None -> failwith "No source"
    | Some file ->
      Utils_js.spf "At %s:%d:%d" (Reason.string_of_source ~strip_root file) line (column + 1)
  in
  print_endline cursor_loc_str;
  TypeAtPosCommand.handle_friendly_result ~strip_root type_loc tys refining_locs documentation;
  print_newline ()

let main
    base_flags
    option_values
    root
    strip_root
    verbose
    path
    wait_for_recheck
    omit_targ_defaults
    max_depth
    verbose_normalizer
    no_typed_ast_for_imports
    args
    () =
  let file =
    match args with
    | [file] ->
      let file = expand_path file in
      File_input.FileName file
    | [] -> get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) path None
    | _ ->
      CommandSpec.usage spec;
      Exit.(exit Commandline_usage_error)
  in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = find_a_root ~base_flags ~input:file root in
  let options =
    {
      ServerProt.Inlay_hint_options.input = file;
      verbose;
      omit_targ_defaults;
      wait_for_recheck;
      verbose_normalizer;
      max_depth;
      no_typed_ast_for_imports;
    }
  in
  let request = ServerProt.Request.INLAY_HINT options in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.INLAY_HINT (Error err) -> prerr_endline err
  | ServerProt.Response.INLAY_HINT (Ok resp) ->
    let strip_root =
      if strip_root then
        Some root
      else
        None
    in
    Base.List.iter resp ~f:(handle_response ~strip_root)
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
