(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open CommandUtils
open CommandSpec

(* This module implements the flow command `apply-code-action` which exposes LSP code-actions via the CLI *)
let handle_error ?(code = Exit.Unknown_error) msg = Exit.(exit ~msg code)

module SourceAddMissingImports = struct
  let spec =
    {
      name = "Add missing imports";
      doc = "Runs the 'source.addMissingImports' code action";
      usage =
        Printf.sprintf
          "Usage: %s apply-code-action 'source.addMissingImports'  [OPTION]... FILE"
          exe_name;
      args =
        ArgSpec.(
          empty
          |> base_flags
          |> connect_and_json_flags
          |> root_flag
          |> path_flag
          |> wait_for_recheck_flag
          |> flag "--in-place" truthy ~doc:"Overwrite the input file"
          |> anon "file" (required string)
        );
    }

  let handle_ok in_place patch source_path input =
    let write_patch content =
      let output_channel =
        if in_place then
          open_out source_path
        else
          stdout
      in
      output_string output_channel @@ Replacement_printer.print patch content;
      close_out output_channel
    in
    match File_input.content_of_file_input input with
    | Ok content -> write_patch content
    | Error msg -> handle_error msg

  let main base_flags connect_params _json _pretty root_arg path wait_for_recheck in_place file () =
    let source_path = expand_path file in
    let input = get_file_from_filename_or_stdin ~cmd:spec.name path (Some source_path) in
    let root = get_the_root ~base_flags ~input root_arg in
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    let request =
      ServerProt.Request.APPLY_CODE_ACTION
        { input; action = ServerProt.Code_action.SourceAddMissingImports; wait_for_recheck }
    in
    let result = connect_and_make_request flowconfig_name connect_params root request in
    match result with
    | ServerProt.Response.APPLY_CODE_ACTION (Ok patch) -> handle_ok in_place patch source_path input
    | _ -> handle_error "Flow: invalid server response"

  let command = CommandSpec.command spec main
end

module SuggestImports = struct
  let spec =
    {
      name = "Show import suggestions";
      doc = "Shows import suggestions for all unbound names in the file ranked by usage";
      usage =
        Printf.sprintf "Usage: %s apply-code-action 'suggestImports'  [OPTION]... FILE" exe_name;
      args =
        ArgSpec.(
          empty
          |> base_flags
          |> connect_and_json_flags
          |> root_flag
          |> path_flag
          |> wait_for_recheck_flag
          |> flag "--in-place" truthy ~doc:"Overwrite the input file"
          |> anon "file" (required string)
        );
    }

  let handle_ok pretty lsp_result =
    let result_json =
      Hh_json.JSON_Object
        (SMap.bindings lsp_result
        |> List.map (fun (name, result) -> (name, Lsp_fmt.print_codeActionResult ~key:"" result))
        )
    in
    print_endline (Hh_json.json_to_string ~sort_keys:pretty ~pretty result_json)

  let main base_flags connect_params _json pretty root_arg path wait_for_recheck _in_place file () =
    let source_path = expand_path file in
    let input = get_file_from_filename_or_stdin ~cmd:spec.name path (Some source_path) in
    let root = get_the_root ~base_flags ~input root_arg in
    let flowconfig_name = base_flags.Base_flags.flowconfig_name in
    let request =
      ServerProt.Request.APPLY_CODE_ACTION
        { input; action = ServerProt.Code_action.SuggestImports; wait_for_recheck }
    in
    let result = connect_and_make_request flowconfig_name connect_params root request in
    match result with
    | ServerProt.Response.SUGGEST_IMPORTS (Ok result) -> handle_ok pretty result
    | _ -> handle_error "Flow: invalid server response"

  let command = CommandSpec.command spec main
end

let command =
  let main (cmd, argv) () = CommandUtils.run_command cmd argv in
  let spec =
    CommandUtils.subcommand_spec
      ~name:"apply-code-action"
      ~doc:""
      [
        ("source.addMissingImports", SourceAddMissingImports.command);
        ("suggestImports", SuggestImports.command);
      ]
  in
  CommandSpec.command spec main
