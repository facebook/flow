(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Lsp
module List = Base.List

(* This module is how the Flow lsp stores and reasons about Flow errors. This is tricky because
 * Flow errors might come from a few different places.
 *
 * Here's our general strategy:
 *
 * 1. If we get streamed server errors for a file, then we either
 *    1a. Add them to the known server errors for the file, if we previously had streamed errors for
 *        that file
 *    1b. Replace the known server errors for the file, if we previously had finalized server errors
 *        (e.g. from after the last recheck)
 * 2. If we have live parse errors for a file (e.g. from running the parser locally on an open file)
 *    then we replace the server's parse errors for that file with the live parse errors
 * 3. If we have live non-parse errors for a file (e.g. from check-contents for an open file)
 *    then we replace the server's non-parse errors for that file with the live non-parse errors.
 *)
type errors = PublishDiagnostics.diagnostic list

type parse_errors = ParseErrors of errors

type non_parse_errors = NonParseErrors of errors

type split_errors = parse_errors * non_parse_errors

(* Server errors are the main errors that the Flow server reports to us. They come in two flavors *)
type server_errors =
  (* Streamed errors are sent down during rechecking. They are not necessarily complete yet.
   * TODO: Guarantee that the streamed errors are complete. That once we stream down errors for
   *       foo.js, that we'll never be streamed any more errors for foo.js during this recheck *)
  | Streamed of split_errors
  (* Finalized errors are sent after rechecking. This is a complete list of errors for this file. *)
  | Finalized of split_errors

type per_file_errors = {
  (* Live parse errors come from the lsp process running the parser on an open file. `None` means
   * we haven't run the parser on this file. `Some []` means we have run the parser but it said
   * there were 0 errors *)
  live_parse_errors: parse_errors option;
  (* Live non-parse errors come from us running check-contents on open files. `None` means we
   * haven't run check-contents on this file. `Some []` means we have run check-contents and it
   * found that there were 0 errors *)
  live_non_parse_errors: non_parse_errors option;
  (* Server errors come from the server. Duh. *)
  server_errors: server_errors;
}

type t = {
  dirty_files: Lsp.UriSet.t;
  (* The set of files for which we must update the IDE *)
  file_to_errors_map: per_file_errors Lsp.UriMap.t;
}

let empty_per_file_errors =
  {
    live_parse_errors = None;
    live_non_parse_errors = None;
    server_errors = Finalized (ParseErrors [], NonParseErrors []);
  }

(* Returns true if we don't know about any errors for this file *)
let file_has_no_errors = function
  | {
      live_parse_errors = None | Some (ParseErrors []);
      live_non_parse_errors = None | Some (NonParseErrors []);
      server_errors =
        Streamed (ParseErrors [], NonParseErrors []) | Finalized (ParseErrors [], NonParseErrors []);
    } ->
    true
  | _ -> false

let empty = { dirty_files = Lsp.UriSet.empty; file_to_errors_map = Lsp.UriMap.empty }

(* For the most part we don't sort errors, and leave it to the server and the IDE to figure that
 * out. The one exception is in limit_errors, to ensure consistent results *)
let sort_errors =
  PublishDiagnostics.(
    List.sort ~compare:(fun d1 d2 -> Lsp_helpers.pos_compare d1.range.start d2.range.start)
  )

(* If we have too many errors then limit them to the first N errors *)
let limit_errors errors =
  let cap = 200 in
  (* List.nth is O(cap) instead of O(n) *)
  let is_below_cap = List.nth errors cap = None in
  if is_below_cap then
    (* avoid O(nlogn) sort in this case *)
    errors
  else
    (* Sort to make sure we're always sending the same errors *)
    let errors = sort_errors errors in
    let (retain, discard) = List.split_n errors cap in
    match discard with
    | [] -> retain
    | discard ->
      let discard_count = List.length discard in
      let message = Printf.sprintf "[Only showing %i/%i diagnostics]" cap (cap + discard_count) in
      let diagnostic =
        PublishDiagnostics.
          {
            (* the following range displays fine in all editors, regardless of contents *)
            range = { start = { line = 0; character = 0 }; end_ = { line = 0; character = 0 } };
            severity = Some PublishDiagnostics.Information;
            code = NoCode;
            source = Some "Flow";
            message;
            relatedInformation = [];
            relatedLocations = [];
          }
        
      in

      diagnostic :: retain

let is_parse_error =
  let parse_code = PublishDiagnostics.StringCode "ParseError" in
  (fun d -> d.PublishDiagnostics.code = parse_code)

let is_not_parse_error d = not (is_parse_error d)

let split errors =
  let (parse_errors, non_parse_errors) = List.partition_tf errors is_parse_error in
  (ParseErrors parse_errors, NonParseErrors non_parse_errors)

let choose_errors
    {
      live_parse_errors;
      live_non_parse_errors;
      server_errors = Streamed server_errors | Finalized server_errors;
    } =
  let (ParseErrors server_parse_errors, NonParseErrors server_non_parse_errors) = server_errors in
  (* Prefer live parse errors over server parse errors *)
  let parse_errors =
    match live_parse_errors with
    | None -> server_parse_errors
    | Some (ParseErrors live_parse_errors) -> live_parse_errors
  in
  let non_parse_errors =
    match live_non_parse_errors with
    | None -> server_non_parse_errors
    | Some (NonParseErrors live_non_parse_errors) -> live_non_parse_errors
  in
  (parse_errors, non_parse_errors)

let have_errors_changed before after =
  let (before_parse_errors, before_non_parse_errors) = choose_errors before in
  let (after_parse_errors, after_non_parse_errors) = choose_errors after in
  (* Structural equality for fast comparison. Will never get false negative *)
  before_parse_errors != after_parse_errors || before_non_parse_errors != after_non_parse_errors

(* We need to send the errors for this file. This is when we need to decide exactly which errors to
 * send. *)
let send_errors_for_file state (send_json : Hh_json.json -> unit) (uri : Lsp.DocumentUri.t) =
  let (parse_errors, non_parse_errors) =
    Lsp.UriMap.find_opt uri state.file_to_errors_map
    |> Base.Option.value ~default:empty_per_file_errors
    |> choose_errors
  in
  let errors = parse_errors @ non_parse_errors in
  let diagnostics = limit_errors errors in
  PublishDiagnosticsNotification { PublishDiagnostics.uri; diagnostics }
  |> Lsp_fmt.print_lsp_notification
  |> send_json

(* For every dirty file (files for which the client likely has out-of-date errors), send the errors
 * to the client *)
let send_all_errors send_json state =
  let dirty_files = state.dirty_files in
  let state = { state with dirty_files = Lsp.UriSet.empty } in
  Lsp.UriSet.iter (send_errors_for_file state send_json) dirty_files;
  state

(* Helper function to modify the data for a specific file *)
let modify_per_file_errors (uri : Lsp.DocumentUri.t) state f =
  let old_per_file_errors =
    Lsp.UriMap.find_opt uri state.file_to_errors_map
    |> Base.Option.value ~default:empty_per_file_errors
  in
  let new_per_file_errors = f old_per_file_errors in
  let dirty = have_errors_changed old_per_file_errors new_per_file_errors in
  (* To keep this data structure small, let's filter out files with no live or server errors *)
  let file_to_errors_map =
    if file_has_no_errors new_per_file_errors then
      Lsp.UriMap.remove uri state.file_to_errors_map
    else
      Lsp.UriMap.add uri new_per_file_errors state.file_to_errors_map
  in
  {
    dirty_files =
      ( if dirty then
        Lsp.UriSet.add uri state.dirty_files
      else
        state.dirty_files
      );
    file_to_errors_map;
  }

(* Helper function to modify the server errors for a specific file *)
let modify_server_errors uri new_errors state f =
  modify_per_file_errors uri state (fun per_file_errors ->
      let (new_parse_errors, new_non_parse_errors) = split new_errors in
      let new_server_errors =
        f per_file_errors.server_errors (new_parse_errors, new_non_parse_errors)
      in
      { per_file_errors with server_errors = new_server_errors }
  )

(* We've parsed a file locally and now want to record the number of parse errors for this file *)
let set_live_parse_errors_and_send send_json uri live_parse_errors state =
  (* If the caller passes in some non-parse errors then we'll just ignore them *)
  let live_parse_errors = List.filter live_parse_errors ~f:is_parse_error in
  modify_per_file_errors uri state (fun per_file_errors ->
      { per_file_errors with live_parse_errors = Some (ParseErrors live_parse_errors) }
  )
  |> send_all_errors send_json

(* We've run check-contents on a modified open file and now want to record the errors reported by
 * check-contents *)
let set_live_non_parse_errors_and_send send_json uri live_non_parse_errors state =
  (* If the caller passes in some parse errors then we'll just ignore them *)
  let live_non_parse_errors = List.filter live_non_parse_errors ~f:is_not_parse_error in
  modify_per_file_errors uri state (fun per_file_errors ->
      { per_file_errors with live_non_parse_errors = Some (NonParseErrors live_non_parse_errors) }
  )
  |> send_all_errors send_json

(* When we close a file we clear all the live parse errors or non-parse errors for that file, but we
 * keep around the server errors *)
let clear_all_live_errors_and_send send_json uri state =
  modify_per_file_errors uri state (fun per_file_errors ->
      { per_file_errors with live_parse_errors = None; live_non_parse_errors = None }
  )
  |> send_all_errors send_json

(* my_list @ [] returns a list which is no longer physically identical to my_list. This is a
 * workaround *)
let append list_a list_b =
  match (list_a, list_b) with
  | ([], []) ->
    [] (* [] is always physically equal to other []. Technically this rule isn't needed *)
  | ([], _) -> list_b
  | (_, []) -> list_a
  | _ -> list_a @ list_b

(* During recheck we stream in errors from the server. These will replace finalized server errors
 * from a previous recheck or add to streamed server errors from this recheck *)
let add_streamed_server_errors_and_send send_json uri_to_error_map state =
  Lsp.UriMap.fold
    (fun uri new_errors_unsplit state ->
      modify_server_errors uri new_errors_unsplit state (fun server_errors new_errors ->
          match server_errors with
          | Finalized _ ->
            (* When a recheck streams in new errors, we stop showing the old finalized errors *)
            Streamed new_errors
          | Streamed existing_errors ->
            (* Streamed errors are additive *)
            let (ParseErrors existing_parse_errors, NonParseErrors existing_non_parse_errors) =
              existing_errors
            in
            let (ParseErrors new_parse_errors, NonParseErrors new_non_parse_errors) = new_errors in
            Streamed
              ( ParseErrors (append existing_parse_errors new_parse_errors),
                NonParseErrors (append existing_non_parse_errors new_non_parse_errors)
              )
      ))
    uri_to_error_map
    state
  |> send_all_errors send_json

(* After recheck we get all the errors from the server. This replaces whatever server errors we
 * already had. *)
let set_finalized_server_errors_and_send send_json uri_to_error_map state =
  let (state, files_with_new_errors) =
    Lsp.UriMap.fold
      (fun uri new_errors_unsplit (state, files_with_new_errors) ->
        let state =
          modify_server_errors uri new_errors_unsplit state (fun _ new_errors ->
              (* At the end of the recheck, the finialized errors will replace either the errors from
               * the previous recheck or the streamed errors *)
              Finalized new_errors
          )
        in
        let files_with_new_errors = Lsp.UriSet.add uri files_with_new_errors in
        (state, files_with_new_errors))
      uri_to_error_map
      (state, Lsp.UriSet.empty)
  in
  (* All the errors in uri_to_error_map have been added to state. But uri_to_error_map doesn't
   * include files which used to have >0 errors but now have 0 errors. So we need to go through
   * every file that used to have errors and clear them out *)
  Lsp.UriMap.fold
    (fun uri _ state ->
      if Lsp.UriSet.mem uri files_with_new_errors then
        state
      else
        modify_server_errors uri [] state (fun _ cleared_errors -> Finalized cleared_errors))
    state.file_to_errors_map
    state
  |> send_all_errors send_json

(* When the Flow server dies, LSP must clear all the errors.
 * TODO: Don't clear live parse errors. Those don't require the server, so we can still keep
 *       providing them *)
let clear_all_errors_and_send send_json state =
  Lsp.UriMap.fold
    (fun uri _ state -> modify_per_file_errors uri state (fun _ -> empty_per_file_errors))
    state.file_to_errors_map
    state
  |> send_all_errors send_json

(* Basically a best-effort attempt to update the locations of errors after a didChange *)
let update_errors_due_to_change_and_send send_json params state =
  let uri = params.DidChange.textDocument.VersionedTextDocumentIdentifier.uri in
  modify_per_file_errors uri state (fun per_file_errors ->
      let { live_parse_errors; live_non_parse_errors; server_errors } = per_file_errors in
      let live_parse_errors =
        match live_parse_errors with
        | None
        | Some (ParseErrors []) ->
          live_parse_errors
        | Some (ParseErrors live_parse_errors) ->
          Some (ParseErrors (Lsp_helpers.update_diagnostics_due_to_change live_parse_errors params))
      in
      let live_non_parse_errors =
        match live_non_parse_errors with
        | None
        | Some (NonParseErrors []) ->
          live_non_parse_errors
        | Some (NonParseErrors live_non_parse_errors) ->
          Some
            (NonParseErrors
               (Lsp_helpers.update_diagnostics_due_to_change live_non_parse_errors params)
            )
      in
      let server_errors =
        match server_errors with
        | Streamed (ParseErrors [], NonParseErrors [])
        | Finalized (ParseErrors [], NonParseErrors []) ->
          server_errors
        | Streamed (ParseErrors parse_errors, NonParseErrors non_parse_errors) ->
          let parse_errors = Lsp_helpers.update_diagnostics_due_to_change parse_errors params in
          let non_parse_errors =
            Lsp_helpers.update_diagnostics_due_to_change non_parse_errors params
          in
          Streamed (ParseErrors parse_errors, NonParseErrors non_parse_errors)
        | Finalized (ParseErrors parse_errors, NonParseErrors non_parse_errors) ->
          let parse_errors = Lsp_helpers.update_diagnostics_due_to_change parse_errors params in
          let non_parse_errors =
            Lsp_helpers.update_diagnostics_due_to_change non_parse_errors params
          in
          Finalized (ParseErrors parse_errors, NonParseErrors non_parse_errors)
      in
      { live_parse_errors; live_non_parse_errors; server_errors }
  )
  |> send_all_errors send_json
