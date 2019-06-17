(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open CommandSpec

(* These are the flags that every autofix command will take *)
let base_arg_spec =
  CommandSpec.ArgSpec.(empty
    |> base_flags
    |> connect_and_json_flags
    |> root_flag
    |> strip_root_flag
    |> verbose_flags
    |> from_flag
    |> path_flag
    |> wait_for_recheck_flag)

(* This module implements the flow command `autofix insert-type` which inserts
   a type annotation in a file at a position. *)
module InsertType = struct
  let spec =
    { name = "insert type";
      doc = "[EXPERIMENTAL] Insert type information at file and position";
      usage = Printf.sprintf
        "Usage: %s autofix insert-type [OPTION]... [FILE] LINE COLUMN [END_LINE] [END_COLUMN]\n\n\
         e.g. %s autofix insert-type foo.js 12 3\n\
         or   %s autofix insert-type 12 3 < foo.js\n"
        exe_name exe_name exe_name;
      args = ArgSpec.(base_arg_spec
        |> flag "--expand-type-aliases" no_arg
            ~doc:"Replace type aliases with their bodies"
        |> flag "--omit-typearg-defaults" no_arg
            ~doc:"Omit type arguments when defaults exist and match the provided type argument"
        |> anon "args" (required (list_of string))
    )}
  let handle_error ?(code=FlowExitStatus.Unknown_error) msg =
    FlowExitStatus.(exit ~msg code)

  let handle_ok patch input =
    match File_input.content_of_file_input input with
    | Ok content -> print_string @@ Replacement_printer.print patch content
    | Error msg -> handle_error msg

  let rec parse_args args : Loc.t =
    let parse_pos line col : Loc.position =
      let (line, column) = try convert_input_pos (int_of_string line, int_of_string col)
        with | Failure _ -> handle_error "Failed to parse position" in
      Loc.{line; column;} in
    match args with
    | [start_line; start_col; end_line; end_col] ->
      let start = parse_pos start_line start_col in
      let _end = parse_pos end_line end_col in
      Loc.{source=None; start; _end;}
    | [start_line; start_col;] ->
      let start = parse_pos start_line start_col in
      Loc.{source=None; start;_end=start;}
    | file :: (([_;_]|[_;_;_;_]) as loc) ->
      let loc = parse_args loc in
      Loc.{loc with source=Some (File_key.SourceFile(expand_path file))}
    | [] -> handle_error "No position given"
    | _ -> handle_error "Invalid position given"

  let main base_flags option_values json _pretty root_arg strip_root_arg
        verbose path wait_for_recheck expand_aliases omit_targ_defaults args () =
    let (Loc.{source; _} as target) = parse_args args in
    let source_path = Option.map ~f:File_key.to_string source in
    let input = get_file_from_filename_or_stdin ~cmd:spec.name path source_path in
    let root = get_the_root ~base_flags ~input root_arg in
    (* TODO Figure out how to implement root striping *)
    let _strip_root = if strip_root_arg then Some root else None in
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    if not json && (verbose <> None) then
      prerr_endline "NOTE: --verbose writes to the server log file";
    let request = ServerProt.Request.INSERT_TYPE {
      input; target; verbose; wait_for_recheck; expand_aliases; omit_targ_defaults; } in
    let result = connect_and_make_request flowconfig_name option_values root request in
    match result with
    | ServerProt.Response.INSERT_TYPE (Error err) -> handle_error err
    (* TODO implement a more useful set of error conditions *)
    | ServerProt.Response.INSERT_TYPE (Ok resp) ->
      handle_ok resp input
    | _ -> prerr_endline "Flow: invalid server response"

    let command = CommandSpec.command spec main
end

let command =
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  let spec =
    { CommandSpec.
      name = "autofix";
      doc = "Modify code using Flow's analysis";
      usage = Printf.sprintf
        "Usage: %s autofix SUBCOMMAND [OPTIONS]...\n\
         Generate code using information available to Flow\n\n\
         SUBCOMMANDS:\n\
            suggest: Print a file filling in all missing type annotations\n"
        CommandUtils.exe_name;
      args = CommandSpec.ArgSpec.(
         empty
         |> anon "subcommand" (required (command [
           "suggest", SuggestCommand.command;
           "insert-type", InsertType.command]))
       )
     } in
  CommandSpec.command spec main
