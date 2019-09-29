(**
 * Copyright (c) Facebook, Inc. and its affiliates.
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
        |> flag "--out" (required string) ~doc:"The path to the new saved-state file");
  }

let main base_flags option_values root out () =
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root = guess_root flowconfig_name root in
  let out = Path.make @@ Files.imaginary_realpath out in
  let out_str = Path.to_string out in
  Printf.printf "Asking server to create a saved-state file at `%s`\n%!" out_str;

  let request = ServerProt.Request.SAVE_STATE { outfile = out } in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.SAVE_STATE (Error err) ->
    Printf.printf "Failed to create saved-state file `%s`:\n%s\n%!" out_str err
  | ServerProt.Response.SAVE_STATE (Ok ()) ->
    Printf.printf "Created saved-state file `%s`\n%!" out_str
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
