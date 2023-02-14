(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow save-state command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "save-state";
    doc = "Tell the server to create a saved-state file";
    usage =
      Printf.sprintf
        "Usage: %s save-state [OPTION]...\n\ne.g. %s save-state --root path/to/root --out path/to/my_saved_state\n"
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_flags
        |> root_flag
        |> from_flag
        |> flag "--scm" no_arg ~doc:"Write to the expected path for the SCM fetcher"
        |> flag "--out" (optional string) ~doc:"The path to the new saved-state file"
      );
  }

let main base_flags option_values root scm out () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = guess_root flowconfig_name root in
  let out =
    match (scm, out) with
    | (None, None)
    | (Some false, None) ->
      raise
        (CommandSpec.Failed_to_parse
           { arg = "--out"; msg = "--out or --scm is required"; details = None }
        )
    | (Some true, Some _) ->
      raise
        (CommandSpec.Failed_to_parse
           { arg = "--out"; msg = "--out and --scm are mutually exclusive"; details = None }
        )
    | (Some true, None) -> `Scm
    | (_, Some out) -> `File (Path.make (Files.imaginary_realpath out))
  in
  let request = ServerProt.Request.SAVE_STATE { out } in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.SAVE_STATE (Error msg) -> Exit.(exit ~msg Unknown_error)
  | ServerProt.Response.SAVE_STATE (Ok msg) -> Printf.printf "%s\n%!" msg
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
