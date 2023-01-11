(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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
  prev |> flag "--lsp" truthy ~doc:"Output results as LSP responses"

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
        |> flag "--imports" truthy ~doc:"Include suggestions that can be imported from other files"
        |> flag "--imports-ranked-usage" truthy ~doc:"" (* experimental: rank imports by usage *)
        |> anon "args" (optional (list_of string))
      );
  }

(* legacy editor integrations inserted the "AUTO332" token themselves. modern ones give us the
   cursor location. this function converts the legacy input to the modern input. *)
let extract_cursor input =
  let contents = File_input.content_of_file_input_unsafe input in
  match Autocomplete_sigil.extract_cursor contents with
  | None -> (input, None)
  | Some (contents, cursor) ->
    let input = File_input.(FileContent (path_of_file_input input, contents)) in
    (input, Some cursor)

let file_input_from_stdin filename =
  get_file_from_filename_or_stdin ~cmd:CommandSpec.(spec.name) filename None

let parse_args = function
  | None
  | Some [] ->
    let input = file_input_from_stdin None in
    extract_cursor input
  | Some [filename] ->
    let input = file_input_from_stdin (Some filename) in
    extract_cursor input
  | Some [line; column] ->
    let cursor = convert_input_pos (int_of_string line, int_of_string column) in
    let input = file_input_from_stdin None in
    (input, Some cursor)
  | Some [filename; line; column] ->
    let cursor = convert_input_pos (int_of_string line, int_of_string column) in
    let input = file_input_from_stdin (Some filename) in
    (input, Some cursor)
  | _ ->
    CommandSpec.usage spec;
    Exit.(exit Commandline_usage_error)

let autocomplete_result_to_json ~strip_root result =
  let open ServerProt.Response.Completion in
  let name = result.name in
  Stdlib.ignore strip_root;
  Hh_json.JSON_Object
    [
      ("name", Hh_json.JSON_String name);
      ("type", Hh_json.JSON_String (Base.Option.value ~default:"" result.itemDetail));
    ]

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
    | Ok { ServerProt.Response.Completion.items; is_incomplete = _ } ->
      let results = Base.List.map ~f:(autocomplete_result_to_json ~strip_root) items in
      JSON_Object [("result", JSON_Array results)]
  )

let main
    base_flags
    option_values
    json
    pretty
    root
    strip_root
    wait_for_recheck
    lsp
    imports
    imports_ranked_usage
    args
    () =
  let (input, cursor_opt) = parse_args args in
  let flowconfig_name = base_flags.Base_flags.flowconfig_name in
  let root =
    guess_root
      flowconfig_name
      (match root with
      | Some root -> Some root
      | None -> File_input.path_of_file_input input)
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
        { input; cursor; wait_for_recheck; trigger_character = None; imports; imports_ranked_usage }
    in
    let results =
      match connect_and_make_request flowconfig_name option_values root request with
      | ServerProt.Response.AUTOCOMPLETE response -> response
      | response -> failwith_bad_response ~request ~response
    in
    if lsp then
      Base.Result.iter
        results
        ~f:(fun { ServerProt.Response.Completion.items; is_incomplete = _ } ->
          List.iteri
            (fun index ->
              Flow_lsp_conversions.flow_completion_item_to_lsp
                ~is_snippet_supported:true
                ~is_tags_supported:(fun _ -> true)
                ~is_preselect_supported:true
                ~is_label_detail_supported:true
                ~is_insert_replace_supported:true
                ~index
              %> Lsp_fmt.print_completionItem ~key:"<PLACEHOLDER_PROJECT_URL>"
              %> Hh_json.print_json_endline ~pretty:true)
            items
      )
    else if json || pretty then
      results |> autocomplete_response_to_json ~strip_root |> Hh_json.print_json_endline ~pretty
    else (
      match results with
      | Error error -> prerr_endlinef "Error: %s" error
      | Ok { ServerProt.Response.Completion.items; is_incomplete = _ } ->
        List.iter
          (fun res ->
            let name = res.ServerProt.Response.Completion.name in
            let detail =
              Base.Option.value ~default:"" res.ServerProt.Response.Completion.itemDetail
            in
            print_endline (Printf.sprintf "%s %s" name detail))
          items
    )

let command = CommandSpec.command spec main
