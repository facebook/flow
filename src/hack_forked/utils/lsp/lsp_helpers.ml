(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* A few helpful wrappers around LSP *)

open Lsp
open Lsp_fmt

(************************************************************************)
(* Conversions                                                          *)
(************************************************************************)

let url_scheme_regex = Str.regexp "^\\([a-zA-Z][a-zA-Z0-9+.-]+\\):"

(* this requires schemes with 2+ characters, so "c:\path" isn't considered a scheme *)

let lsp_uri_to_path (uri : DocumentUri.t) : string =
  let uri = DocumentUri.to_string uri in
  if Str.string_match url_scheme_regex uri 0 then
    let scheme = Str.matched_group 1 uri in
    if scheme = "file" then
      File_url.parse uri
    else
      raise
        (Error.LspException
           {
             Error.code = Error.InvalidParams;
             message = Printf.sprintf "Not a valid file url '%s'" uri;
             data = None;
           })
  else
    uri

let path_to_lsp_uri (path : string) ~(default_path : string) : Lsp.DocumentUri.t =
  if path = "" then
    File_url.create default_path |> DocumentUri.of_string
  else
    File_url.create path |> DocumentUri.of_string

let lsp_textDocumentIdentifier_to_filename (identifier : Lsp.TextDocumentIdentifier.t) : string =
  Lsp.TextDocumentIdentifier.(lsp_uri_to_path identifier.uri)

let lsp_position_to_fc (pos : Lsp.position) : File_content.position =
  {
    File_content.line = pos.Lsp.line + 1;
    (* LSP is 0-based; File_content is 1-based. *)
    column = pos.Lsp.character + 1;
  }

let lsp_range_to_fc (range : Lsp.range) : File_content.range =
  { File_content.st = lsp_position_to_fc range.Lsp.start; ed = lsp_position_to_fc range.Lsp.end_ }

let lsp_edit_to_fc (edit : Lsp.DidChange.textDocumentContentChangeEvent) : File_content.text_edit =
  {
    File_content.range = Base.Option.map edit.DidChange.range ~f:lsp_range_to_fc;
    text = edit.DidChange.text;
  }

let apply_changes (text : string) (contentChanges : DidChange.textDocumentContentChangeEvent list) :
    (string, string * Utils.callstack) result =
  let edits = List.map lsp_edit_to_fc contentChanges in
  File_content.edit_file text edits

let get_char_from_lsp_position (content : string) (position : Lsp.position) : char =
  let fc_position = lsp_position_to_fc position in
  File_content.(get_char content (get_offset content fc_position))

let apply_changes_unsafe text (contentChanges : DidChange.textDocumentContentChangeEvent list) :
    string =
  match apply_changes text contentChanges with
  | Ok r -> r
  | Error (e, _stack) -> failwith e

(************************************************************************)
(* Range calculations                                                   *)
(************************************************************************)

(* We need to do intersection and other calculations on ranges.
 * The functions in the following module all assume LSP 0-based ranges,
 * and assume without testing that a range's start is equal to or before
 * its end. *)
let pos_compare (p1 : position) (p2 : position) : int =
  if p1.line < p2.line then
    -1
  else if p1.line > p2.line then
    1
  else
    p1.character - p2.character

(* Given a "selection" range A..B and a "squiggle" range a..b, how do they overlap?
 * There are 12 ways to order the four letters ABab, of which six
 * satisfy both A<=B and a<=b. Here they are. *)
type range_overlap =
  | Selection_before_start_of_squiggle (* ABab *)
  | Selection_overlaps_start_of_squiggle (* AaBb *)
  | Selection_covers_whole_squiggle (* AabB *)
  | Selection_in_middle_of_squiggle (* aABb *)
  | Selection_overlaps_end_of_squiggle (* aAbB *)
  (* abAB *)
  | Selection_after_end_of_squiggle

(* Computes how two ranges "selection" and "squiggle" overlap *)
let get_range_overlap (selection : range) (squiggle : range) : range_overlap =
  let selStart_leq_squiggleStart = pos_compare selection.start squiggle.start <= 0 in
  let selStart_leq_squiggleEnd = pos_compare selection.start squiggle.end_ <= 0 in
  let selEnd_lt_squiggleStart = pos_compare selection.end_ squiggle.start < 0 in
  let selEnd_lt_squiggleEnd = pos_compare selection.end_ squiggle.end_ < 0 in
  (* Q. Why does it test "<=" for the first two and "<" for the last two? *)
  (* Intuitively you can trust that it has something to do with how ranges are *)
  (* inclusive at their start and exclusive at their end. But the real reason *)
  (* is just that I did an exhaustive case analysis to look at all cases where *)
  (* A,B,a,b might be equal, and decided which outcome I wanted for each of them *)
  (* because of how I'm going to treat them in other functions, and retrofitted *)
  (* those answers into this function. For instance, if squiggleStart==selEnd, *)
  (* I'll want to handle it in the same way as squiggleStart<selEnd<squiggleEnd. *)
  (* The choices of "leq" and "lt" in this function embody those answers. *)
  match
    ( selStart_leq_squiggleStart,
      selStart_leq_squiggleEnd,
      selEnd_lt_squiggleStart,
      selEnd_lt_squiggleEnd )
  with
  | (true, true, true, true) -> Selection_before_start_of_squiggle
  | (true, true, false, true) -> Selection_overlaps_start_of_squiggle
  | (true, true, false, false) -> Selection_covers_whole_squiggle
  | (false, true, false, true) -> Selection_in_middle_of_squiggle
  | (false, true, false, false) -> Selection_overlaps_end_of_squiggle
  | (false, false, false, false) -> Selection_after_end_of_squiggle
  | (true, false, _, _) -> failwith "sel.start proves squiggle.start > squiggle.end_"
  | (_, _, true, false) -> failwith "sel.end proves squiggle.start > squiggle.end_"
  | (false, _, true, _) -> failwith "squiggle.start proves sel.start > sel.end_"
  | (_, false, _, true) -> failwith "squiggle.end_ proves sel.start > sel.end_"

(* this structure models a change where a certain range is replaced with
 * a block of text. For instance, if you merely insert a single character,
 * then remove_range.start==remove_range.end_ and insert_lines=0
 * and insert_chars_on_final_line=1. *)
type range_replace = {
  remove_range: range;
  insert_lines: int;
  insert_chars_on_final_line: int;
}

(* If you have a position "p", and some range before this point is replaced with
 * text of a certain number of lines, the last line having a certain number of characters,
 * then how will the position be shifted?
 * Note: this function assumes but doesn't verify that the range ends on or before
 * the position. *)
let update_pos_due_to_prior_replace (p : position) (replace : range_replace) : position =
  if replace.remove_range.end_.line < p.line then
    (* The replaced range doesn't touch the position, so position merely gets shifted up/down *)
    let line =
      p.line
      - (replace.remove_range.end_.line - replace.remove_range.start.line)
      + replace.insert_lines
    in
    { p with line }
  else if replace.insert_lines > 0 then
    (* The position is on the final line and multiple lines were inserted  *)
    let line =
      p.line
      - (replace.remove_range.end_.line - replace.remove_range.start.line)
      + replace.insert_lines
    in
    let character =
      replace.insert_chars_on_final_line + (p.character - replace.remove_range.end_.character)
    in
    { line; character }
  else
    (* The position is on the line where a few characters were inserted *)
    let line = p.line - (replace.remove_range.end_.line - replace.remove_range.start.line) in
    let character =
      replace.remove_range.start.character
      + replace.insert_chars_on_final_line
      + (p.character - replace.remove_range.end_.character)
    in
    { line; character }

(* If you have a squiggle, and some range in the document is replaced with a block
 * some lines long and with insert_chars on the final line, then what's the new
 * range of the squiggle? *)
let update_range_due_to_replace (squiggle : range) (replace : range_replace) : range option =
  match get_range_overlap replace.remove_range squiggle with
  | Selection_before_start_of_squiggle ->
    let start = update_pos_due_to_prior_replace squiggle.start replace in
    let end_ = update_pos_due_to_prior_replace squiggle.end_ replace in
    Some { start; end_ }
  | Selection_overlaps_start_of_squiggle ->
    let line = replace.remove_range.start.line + replace.insert_lines in
    let character =
      if replace.insert_lines = 0 then
        replace.remove_range.start.character + replace.insert_chars_on_final_line
      else
        replace.insert_chars_on_final_line
    in
    let start = { line; character } in
    let end_ = update_pos_due_to_prior_replace squiggle.end_ replace in
    Some { start; end_ }
  | Selection_covers_whole_squiggle -> None
  | Selection_in_middle_of_squiggle ->
    let start = squiggle.start in
    let end_ = update_pos_due_to_prior_replace squiggle.end_ replace in
    Some { start; end_ }
  | Selection_overlaps_end_of_squiggle ->
    let start = squiggle.start in
    let end_ = replace.remove_range.start in
    Some { start; end_ }
  | Selection_after_end_of_squiggle -> Some squiggle

(* Moves all diagnostics in response to an LSP change.
 * The change might insert text before a diagnostic squiggle (so the squiggle
 * has to be moved down or to the right); it might delete text before the squiggle;
 * it might modify text inside the squiggle; it might replace text that overlaps
 * the squiggle in which case the squiggle gets truncated/moved; it might replace
 * the squiggle in its entirety in which case the squiggle gets removed.
 * Note that an LSP change is actually a set of changes, applied in order. *)
let update_diagnostics_due_to_change
    (diagnostics : PublishDiagnostics.diagnostic list) (change : Lsp.DidChange.params) :
    PublishDiagnostics.diagnostic list =
  PublishDiagnostics.(
    let replace_of_change change =
      match change.DidChange.range with
      | None -> None
      | Some remove_range ->
        let offset = String.length change.DidChange.text in
        let pos = File_content.offset_to_position change.DidChange.text offset in
        (* 1-based *)
        let insert_lines = pos.File_content.line - 1 in
        let insert_chars_on_final_line = pos.File_content.column - 1 in
        Some { remove_range; insert_lines; insert_chars_on_final_line }
    in
    let apply_replace diagnostic_opt replace_opt =
      match (diagnostic_opt, replace_opt) with
      | (Some diagnostic, Some replace) ->
        let range = update_range_due_to_replace diagnostic.range replace in
        Base.Option.map range ~f:(fun range -> { diagnostic with range })
      | _ -> None
    in
    let replaces = Base.List.map change.DidChange.contentChanges ~f:replace_of_change in
    let apply_all_replaces diagnostic =
      Base.List.fold replaces ~init:(Some diagnostic) ~f:apply_replace
    in
    Base.List.filter_map diagnostics ~f:apply_all_replaces)

(************************************************************************)
(* Accessors                                                            *)
(************************************************************************)

let get_root (p : Lsp.Initialize.params) : string =
  Lsp.Initialize.(
    match (p.rootUri, p.rootPath) with
    | (Some uri, _) -> lsp_uri_to_path uri
    | (None, Some path) -> path
    | (None, None) -> failwith "Initialize params missing root")

let supports_codeActionKinds (p : Lsp.Initialize.params) : CodeActionKind.t list =
  let open Lsp.Initialize in
  let open Lsp.CodeActionClientCapabilities in
  match p.client_capabilities.textDocument.codeAction.codeActionLiteralSupport with
  | Some { CodeActionLiteralSupport.valueSet } -> valueSet
  | None -> []

let supports_status (p : Lsp.Initialize.params) : bool =
  Lsp.Initialize.(p.client_capabilities.window.status)

let supports_snippets (p : Lsp.Initialize.params) : bool =
  Lsp.Initialize.(p.client_capabilities.textDocument.completion.completionItem.snippetSupport)

let supports_preselect (p : Lsp.Initialize.params) : bool =
  Lsp.Initialize.(p.client_capabilities.textDocument.completion.completionItem.preselectSupport)

let supports_connectionStatus (p : Lsp.Initialize.params) : bool =
  Lsp.Initialize.(p.client_capabilities.telemetry.connectionStatus)

(************************************************************************)
(* Wrappers for some LSP methods                                        *)
(************************************************************************)

let telemetry (writer : Jsonrpc.writer) (level : MessageType.t) (message : string) : unit =
  print_logMessage level message |> Jsonrpc.notify writer "telemetry/event"

let telemetry_error (writer : Jsonrpc.writer) = telemetry writer MessageType.ErrorMessage

let telemetry_log (writer : Jsonrpc.writer) = telemetry writer MessageType.LogMessage

let log (writer : Jsonrpc.writer) (level : MessageType.t) (message : string) : unit =
  print_logMessage level message |> Jsonrpc.notify writer "window/logMessage"

let log_error (writer : Jsonrpc.writer) = log writer MessageType.ErrorMessage

let log_warning (writer : Jsonrpc.writer) = log writer MessageType.WarningMessage

let log_info (writer : Jsonrpc.writer) = log writer MessageType.InfoMessage

let dismiss_diagnostics (writer : Jsonrpc.writer) (diagnostic_uris : UriSet.t) : UriSet.t =
  let dismiss_one (uri : DocumentUri.t) : unit =
    let message = { Lsp.PublishDiagnostics.uri; diagnostics = [] } in
    message |> print_diagnostics |> Jsonrpc.notify writer "textDocument/publishDiagnostics"
  in
  UriSet.iter dismiss_one diagnostic_uris;
  UriSet.empty

let notify_connectionStatus
    (p : Lsp.Initialize.params) (writer : Jsonrpc.writer) (wasConnected : bool) (isConnected : bool)
    : bool =
  ( if supports_connectionStatus p && wasConnected <> isConnected then
    let message = { Lsp.ConnectionStatus.isConnected } in
    message |> print_connectionStatus |> Jsonrpc.notify writer "telemetry/connectionStatus" );
  isConnected
