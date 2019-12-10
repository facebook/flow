(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open CommandSpec

(* This module implements the flow command `autofix insert-type` which inserts
   a type annotation in a file at a position. *)
module InsertType = struct
  let spec =
    Autofix_options.(
      let ambiguity_strategies_list = String.concat ", " @@ List.map fst ambiguity_strategies in
      {
        name = "insert type";
        doc = "[EXPERIMENTAL] Insert type information at file and position";
        usage =
          Printf.sprintf
            "Usage: %s autofix insert-type [OPTION]... [FILE] LINE COLUMN [END_LINE] [END_COLUMN]\n\ne.g. %s autofix insert-type foo.js 12 3\nor   %s autofix insert-type 12 3 < foo.js\n"
            exe_name
            exe_name
            exe_name;
        args =
          ArgSpec.(
            empty
            |> base_flags
            |> connect_and_json_flags
            |> root_flag
            |> strip_root_flag
            |> verbose_flags
            |> from_flag
            |> path_flag
            |> wait_for_recheck_flag
            |> flag
                 "--strict-location"
                 no_arg
                 ~doc:"Restrict the number of valid positions for each annotation"
            |> flag
                 "--strategy"
                 (required ~default:Generalize (enum ambiguity_strategies))
                 ~doc:
                   ( "Set how to resolve ambiguity in possible types ("
                   ^ ambiguity_strategies_list
                   ^ ")" )
            |> flag
                 "--in-place"
                 no_arg
                 ~doc:"Overwrite the input file or file specified by the path flag"
            |> flag "--expand-type-aliases" no_arg ~doc:"Replace type aliases with their bodies"
            |> flag
                 "--omit-typearg-defaults"
                 no_arg
                 ~doc:"Omit type arguments when defaults exist and match the provided type argument"
            |> anon "args" (required (list_of string)));
      })

  let handle_error ?(code = FlowExitStatus.Unknown_error) msg = FlowExitStatus.(exit ~msg code)

  let rec parse_args args : Loc.t =
    let parse_pos line col : Loc.position =
      let (line, column) =
        try convert_input_pos (int_of_string line, int_of_string col)
        with Failure _ -> handle_error "flow autofix insert-type: failed to parse position"
      in
      Loc.{ line; column }
    in
    match args with
    | [start_line; start_col; end_line; end_col] ->
      let start = parse_pos start_line start_col in
      let _end = parse_pos end_line end_col in
      Loc.{ source = None; start; _end }
    | [start_line; start_col] ->
      let start = parse_pos start_line start_col in
      Loc.{ source = None; start; _end = start }
    | file :: (([_; _] | [_; _; _; _]) as loc) ->
      let loc = parse_args loc in
      Loc.{ loc with source = Some (File_key.SourceFile (expand_path file)) }
    | [] -> handle_error "flow autofix insert-type: No position given"
    | _ -> handle_error "flow autofix insert-type: Invalid position given"

  let select_output_channel in_place path source_path =
    match (in_place, path, source_path) with
    | (false, _, _) -> stdout
    | (true, Some p, _)
    | (true, None, Some p) ->
      begin
        try open_out p
        with _ ->
          handle_error ~code:FlowExitStatus.Path_is_not_a_file
          @@ Printf.sprintf "failed to open output file: %s" p
      end
    | (true, None, None) ->
      handle_error "Flow: --in-place flag used without input file or explicit path"

  let handle_ok patch input in_place path source_path =
    match File_input.content_of_file_input input with
    | Ok content ->
      let out = select_output_channel in_place path source_path in
      output_string out @@ Replacement_printer.print patch content;
      close_out out
    | Error msg -> handle_error msg

  let main
      base_flags
      option_values
      json
      _pretty
      root_arg
      strip_root_arg
      verbose
      path
      wait_for_recheck
      location_is_strict
      ambiguity_strategy
      in_place
      expand_aliases
      omit_targ_defaults
      args
      () =
    let (Loc.{ source; _ } as target) = parse_args args in
    let source_path = Option.map ~f:File_key.to_string source in
    let input = get_file_from_filename_or_stdin ~cmd:spec.name path source_path in
    let root = get_the_root ~base_flags ~input root_arg in
    (* TODO Figure out how to implement root striping *)
    let _strip_root =
      if strip_root_arg then
        Some root
      else
        None
    in
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    if (not json) && verbose <> None then
      prerr_endline "NOTE: --verbose writes to the server log file";
    let request =
      ServerProt.Request.INSERT_TYPE
        {
          input;
          target;
          verbose;
          location_is_strict;
          ambiguity_strategy;
          wait_for_recheck;
          expand_aliases;
          omit_targ_defaults;
        }
    in
    let result = connect_and_make_request flowconfig_name option_values root request in
    match result with
    | ServerProt.Response.INSERT_TYPE (Error err) -> handle_error err
    (* TODO implement a more useful set of error conditions *)
    | ServerProt.Response.INSERT_TYPE (Ok resp) -> handle_ok resp input in_place path source_path
    | _ -> handle_error "Flow: invalid server response"

  let command = CommandSpec.command spec main
end

module Exports = struct
  let spec =
    {
      name = "exports";
      doc = "[EXPERIMENTAL] automatically fix signature verification errors";
      usage = Printf.sprintf "Usage: %s autofix exports [OPTION]... [FILE]\n" exe_name;
      args =
        ArgSpec.(
          empty
          |> base_flags
          |> connect_and_json_flags
          |> root_flag
          |> strip_root_flag
          |> verbose_flags
          |> from_flag
          |> path_flag
          |> wait_for_recheck_flag
          |> flag
               "--in-place"
               no_arg
               ~doc:"Overwrite the input file or file specified by the path flag"
          |> flag "--force" no_arg ~doc:"Write the results even if errors are encountered"
          |> anon "file" (required string));
    }

  let handle_error ?(code = FlowExitStatus.Unknown_error) msg = FlowExitStatus.(exit ~msg code)

  let select_output_channel in_place path source_path =
    match (in_place, path, source_path) with
    | (false, _, _) -> stdout
    | (true, Some p, _)
    | (true, None, p) ->
      begin
        try open_out p
        with _ ->
          handle_error ~code:FlowExitStatus.Path_is_not_a_file
          @@ Printf.sprintf "failed to open output file: %s" p
      end

  let avg_error_size = 100

  let append_errors errors =
    Buffer.(
      let buff = create (avg_error_size * List.length errors) in
      List.fold_left (fun () -> add_string buff) () errors;
      contents buff)

  let handle_ok patch errors input in_place forced path source_path =
    let write_patch content =
      let out = select_output_channel in_place path source_path in
      output_string out @@ Replacement_printer.print patch content;
      close_out out
    in
    match (File_input.content_of_file_input input, errors, forced) with
    | (Ok content, [], _)
    | (Ok content, _, true) ->
      output_string stderr (append_errors errors);
      write_patch content
    | (Ok _, errors, false) -> handle_error (append_errors errors)
    | (Error msg, _, _) -> handle_error msg

  let main
      base_flags
      option_values
      json
      _pretty
      root_arg
      _strip_root_arg
      verbose
      path
      wait_for_recheck
      in_place
      forced
      source_path
      () =
    let source_path = expand_path source_path in
    let input = get_file_from_filename_or_stdin ~cmd:spec.name path (Some source_path) in
    let root = get_the_root ~base_flags ~input root_arg in
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    if (not json) && verbose <> None then
      prerr_endline "NOTE: --verbose writes to the server log file";
    let request = ServerProt.Request.AUTOFIX_EXPORTS { input; verbose; wait_for_recheck } in
    let result = connect_and_make_request flowconfig_name option_values root request in
    match result with
    | ServerProt.Response.AUTOFIX_EXPORTS (Error err) -> handle_error err
    | ServerProt.Response.AUTOFIX_EXPORTS (Ok (patch, errors)) ->
      handle_ok patch errors input in_place forced path source_path
    | _ -> handle_error "Flow: invalid server response"

  let command = CommandSpec.command spec main
end

let command =
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  let spec =
    {
      CommandSpec.name = "autofix";
      doc = "";
      usage =
        Printf.sprintf
          "Usage: %s autofix SUBCOMMAND [OPTIONS]...\n\nSUBCOMMANDS:\nsuggest: Provides type annotation suggestions for a given program\ninsert-type: Insert type information at file and position\nexports: Automatically fix signature verification errors\n"
          CommandUtils.exe_name;
      args =
        CommandSpec.ArgSpec.(
          empty
          |> anon
               "subcommand"
               (required
                  (command
                     [
                       ("suggest", SuggestCommand.command);
                       ("insert-type", InsertType.command);
                       ("exports", Exports.command);
                     ])));
    }
  in
  CommandSpec.command spec main
