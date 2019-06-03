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
        "Usage: %s autofix insert-type [OPTION]... [FILE] LINE COLUMN\n\n\
         e.g. %s autofix insert-type foo.js 12 3\n\
         or   %s autofix insert-type 12 3 < foo.js\n"
        exe_name exe_name exe_name;
      args = ArgSpec.(base_arg_spec
        |> flag "--expand-json-output" no_arg
            ~doc:"Includes an expanded version of the returned JSON type (implies --json)"
        |> flag "--expand-type-aliases" no_arg
            ~doc:"Replace type aliases with their bodies"
        |> flag "--omit-typearg-defaults" no_arg
            ~doc:"Omit type arguments when defaults exist and match the provided type argument"
        |> anon "args" (required (list_of string))
    )}

  let main base_flags option_values json pretty root_arg strip_root_arg
        verbose path wait_for_recheck expanded
        expand_aliases omit_targ_defaults args () =
    let (input, line, char) = parse_location_with_optional_filename spec path args in
    let root = get_the_root ~base_flags ~input root_arg in
    let strip_root = if strip_root_arg then Some root else None in
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    if not json && (verbose <> None) then
      prerr_endline "NOTE: --verbose writes to the server log file";
    (* The request for the server to infer the type at position,
       and using the TypeAtPosCommand code to handle the response
       is a temporary state for this code. It is only meant to test
       the boiler plate that occurs before. *)
    (* TODO implement a server request that takes the position of the
       annotation and return the patch needed to pretty print that
       code modification *)
    let request = ServerProt.Request.INFER_TYPE {
      input; line; char; verbose;
      expand_aliases; omit_targ_defaults; wait_for_recheck;} in
    let result = connect_and_make_request flowconfig_name option_values root request in
    match result with
    | ServerProt.Response.INFER_TYPE (Error err) ->
      (* TODO Replace this with an appropriate Error handler *)
      TypeAtPosCommand.handle_error err ~json ~pretty
    | ServerProt.Response.INFER_TYPE (Ok resp) ->
      (* TODO Once the server request is implemented it should return
         a file patch. Write a hander that uses the replacement printer
         to display the new file. *)
      let json = json || pretty in
      let file_contents = File_input.content_of_file_input input |> Core_result.ok in
      TypeAtPosCommand.handle_response resp ~file_contents ~json ~pretty ~strip_root ~expanded
    | _ -> failwith "Flow: invalid server response"

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
