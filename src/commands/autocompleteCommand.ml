(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(***********************************************************************)
(* flow autocomplete command *)
(***********************************************************************)

open CommandUtils
open Utils_js

let lsp_flag prev =
  let open CommandSpec.ArgSpec in
  prev |> flag "--lsp" no_arg ~doc:"Output results as LSP responses"

let spec =
  {
    CommandSpec.name = "autocomplete";
    doc = "Queries autocompletion information";
    usage =
      Printf.sprintf
        "Usage: %s autocomplete [OPTION] [FILE] [LINE COLUMN]...\n\nQueries autocompletion information.\n\nIf line and column is specified, then the magic autocomplete token is\nautomatically inserted at the specified position.\n\nExample usage:\n\t%s autocomplete < foo.js\n\t%s autocomplete path/to/foo.js < foo.js
      \t%s autocomplete 12 35 < foo.js\n"
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name
        CommandUtils.exe_name;
    args =
      CommandSpec.ArgSpec.(
        empty
        |> base_flags
        |> connect_and_json_flags
        |> root_flag
        |> strip_root_flag
        |> from_flag
        |> wait_for_recheck_flag
        |> lsp_flag
        |> anon "args" (optional (list_of string)));
  }

(* legacy editor integrations inserted the "AUTO332" token themselves. modern ones give us the
   cursor location. this function finds the first occurrence of "AUTO332" and returns the
   contents with the token removed, along with the (line, column) cursor position. *)
let extract_cursor contents =
  let regexp = Str.regexp_string AutocompleteService_js.autocomplete_suffix in
  try
    let offset = Str.search_forward regexp contents 0 in
    let cursor = Line.position_of_offset contents offset in
    let contents =
      let prefix = String.sub contents 0 offset in
      let suffix =
        String.sub
          contents
          (offset + AutocompleteService_js.suffix_len)
          (String.length contents - (offset + 7))
      in
      prefix ^ suffix
    in
    (contents, Some cursor)
  with Not_found -> (contents, None)

let parse_args = function
  | None
  | Some [] ->
    let contents = Sys_utils.read_stdin_to_string () in
    let (contents, cursor) = extract_cursor contents in
    (None, contents, cursor)
  | Some [filename] ->
    let filename = get_path_of_file filename in
    let contents = Sys_utils.read_stdin_to_string () in
    let (contents, cursor) = extract_cursor contents in
    (Some filename, contents, cursor)
  | Some [line; column] ->
    let line = int_of_string line in
    let column = int_of_string column in
    let contents = Sys_utils.read_stdin_to_string () in
    let cursor = convert_input_pos (line, column) in
    (None, contents, Some cursor)
  | Some [filename; line; column] ->
    let line = int_of_string line in
    let column = int_of_string column in
    let contents = Sys_utils.read_stdin_to_string () in
    let filename = get_path_of_file filename in
    let cursor = convert_input_pos (line, column) in
    (Some filename, contents, Some cursor)
  | _ ->
    CommandSpec.usage spec;
    FlowExitStatus.(exit Commandline_usage_error)

let autocomplete_result_to_json ~strip_root result =
  let open ServerProt.Response.Completion in
  let name = result.name in
  Stdlib.ignore strip_root;
  Hh_json.JSON_Object
    [("name", Hh_json.JSON_String name); ("type", Hh_json.JSON_String result.detail)]

let autocomplete_response_to_json ~strip_root response =
  Hh_json.(
    match response with
    | Error error ->
      JSON_Object
        [
          ("error", JSON_String error);
          ("result", JSON_Array []);
          (* TODO: remove this? kept for BC *)
        ]
    | Ok completions ->
      let results = Base.List.map ~f:(autocomplete_result_to_json ~strip_root) completions in
      JSON_Object [("result", JSON_Array results)])

let main base_flags option_values json pretty root strip_root wait_for_recheck lsp args () =
  let (filename, contents, cursor_opt) = parse_args args in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match root with
      | Some root -> Some root
      | None -> filename)
  in
  let strip_root =
    if strip_root then
      Some root
    else
      None
  in
  match cursor_opt with
  | None ->
    if json || pretty then
      let open Hh_json in
      print_json_endline ~pretty (JSON_Object [("result", JSON_Array [])])
    else
      ()
  | Some cursor ->
    let request =
      ServerProt.Request.AUTOCOMPLETE
        { filename; contents; cursor; wait_for_recheck; trigger_character = None }
    in
    let results =
      match connect_and_make_request flowconfig_name option_values root request with
      | ServerProt.Response.AUTOCOMPLETE response -> response
      | response -> failwith_bad_response ~request ~response
    in
    if lsp then
      Base.Result.iter
        results
        ~f:
          (List.iter
             ( Flow_lsp_conversions.flow_completion_to_lsp
                 ~is_snippet_supported:true
                 ~is_preselect_supported:true
             %> Lsp_fmt.print_completionItem ~key:(Path.to_string root)
             %> Hh_json.print_json_endline ~pretty:true ))
    else if json || pretty then
      results |> autocomplete_response_to_json ~strip_root |> Hh_json.print_json_endline ~pretty
    else (
      match results with
      | Error error -> prerr_endlinef "Error: %s" error
      | Ok completions ->
        List.iter
          (fun res ->
            let name = res.ServerProt.Response.Completion.name in
            let detail = res.ServerProt.Response.Completion.detail in
            print_endline (Printf.sprintf "%s %s" name detail))
          completions
    )

let command = CommandSpec.command spec main
