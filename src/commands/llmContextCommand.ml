(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow llm-context command *)
(***********************************************************************)

open CommandUtils

let spec =
  {
    CommandSpec.name = "llm-context-experimental";
    doc = "";
    usage =
      Printf.sprintf
        "Usage: %s llm-context [OPTION]... [FILE]...\n\ne.g. %s llm-context foo.js bar.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> root_flag
        |> from_flag
        |> wait_for_recheck_flag
        |> flag
             "--token-budget"
             (required ~default:4000 int)
             ~doc:"Maximum token budget for context (default 4000)"
        |> anon "files" (list_of string)
      );
  }

let handle_response ~json ~pretty response =
  let open ServerProt.Response.LlmContext in
  let { llm_context; files_processed; tokens_used; truncated } = response in
  if json then
    let open Hh_json in
    let json =
      JSON_Object
        [
          ("llmContext", JSON_String llm_context);
          ("filesProcessed", JSON_Array (List.map (fun f -> JSON_String f) files_processed));
          ("tokensUsed", JSON_Number (string_of_int tokens_used));
          ("truncated", JSON_Bool truncated);
        ]
    in
    print_json_endline ~pretty json
  else begin
    print_endline llm_context;
    Printf.printf "\n--- Stats ---\n";
    Printf.printf "Files processed: %s\n" (String.concat ", " files_processed);
    Printf.printf "Tokens used: %d\n" tokens_used;
    if truncated then print_endline "Note: Output was truncated due to token budget"
  end

let handle_error err ~json ~pretty =
  if json then
    Hh_json.(
      let json = JSON_Object [("error", JSON_String err)] in
      prerr_json_endline ~pretty json
    )
  else
    prerr_endline err

let main base_flags option_values json pretty root wait_for_recheck token_budget files () =
  let files =
    match files with
    | Some fs -> fs
    | None ->
      prerr_endline "Error: At least one file must be specified";
      exit 1
  in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    match root with
    | Some r -> File_path.make r
    | None ->
      (match files with
      | file :: _ -> find_a_root ~base_flags ~input:(File_input.FileName file) None
      | [] ->
        prerr_endline "Error: Could not determine root";
        exit 1)
  in
  let full_paths =
    List.map
      (fun file ->
        if Filename.is_relative file then
          Filename.concat (Sys.getcwd ()) file
        else
          file)
      files
  in
  let options =
    { ServerProt.Llm_context_options.files = full_paths; token_budget; wait_for_recheck }
  in
  let request = ServerProt.Request.LLM_CONTEXT options in
  match connect_and_make_request flowconfig_name option_values root request with
  | ServerProt.Response.LLM_CONTEXT (Error err) -> handle_error err ~json ~pretty
  | ServerProt.Response.LLM_CONTEXT (Ok resp) -> handle_response resp ~json ~pretty
  | response -> failwith_bad_response ~request ~response

let command = CommandSpec.command spec main
