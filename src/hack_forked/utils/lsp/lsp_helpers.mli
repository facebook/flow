(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This `.mli` file was generated automatically. It may include extra
   definitions that should not actually be exposed to the caller. If you notice
   that this interface file is a poor interface, please take a few minutes to
   clean it up manually, and then delete this comment once the interface is in
   shape. *)

type range_replace = {
  remove_range: Lsp.range;
  insert_lines: int;
  insert_chars_on_final_line: int;
}

val url_scheme_regex : Str.regexp

val lsp_uri_to_path : Lsp.DocumentUri.t -> string

val path_to_lsp_uri : string -> default_path:string -> Lsp.DocumentUri.t

val lsp_textDocumentIdentifier_to_filename : Lsp.TextDocumentIdentifier.t -> string

val lsp_position_to_fc : Lsp.position -> File_content.position

val lsp_range_to_fc : Lsp.range -> File_content.range

val lsp_edit_to_fc : Lsp.DidChange.textDocumentContentChangeEvent -> File_content.text_edit

val apply_changes :
  string ->
  Lsp.DidChange.textDocumentContentChangeEvent list ->
  (string, string * Utils.callstack) result

val get_char_from_lsp_position : string -> Lsp.position -> char

val apply_changes_unsafe : string -> Lsp.DidChange.textDocumentContentChangeEvent list -> string

val pos_compare : Lsp.position -> Lsp.position -> int

type range_overlap =
  | Selection_before_start_of_squiggle
  | Selection_overlaps_start_of_squiggle
  | Selection_covers_whole_squiggle
  | Selection_in_middle_of_squiggle
  | Selection_overlaps_end_of_squiggle
  | Selection_after_end_of_squiggle

val get_range_overlap : Lsp.range -> Lsp.range -> range_overlap

val update_pos_due_to_prior_replace : Lsp.position -> range_replace -> Lsp.position

val update_range_due_to_replace : Lsp.range -> range_replace -> Lsp.range option

val update_diagnostics_due_to_change :
  Lsp.PublishDiagnostics.diagnostic list ->
  Lsp.DidChange.params ->
  Lsp.PublishDiagnostics.diagnostic list

val get_root : Lsp.Initialize.params -> string

val supports_codeActionKinds : Lsp.Initialize.params -> Lsp.CodeActionKind.t list

val supports_status : Lsp.Initialize.params -> bool

val supports_snippets : Lsp.Initialize.params -> bool

val supports_preselect : Lsp.Initialize.params -> bool

val supports_connectionStatus : Lsp.Initialize.params -> bool

val telemetry : Jsonrpc.writer -> Lsp.MessageType.t -> string -> unit

val telemetry_error : Jsonrpc.writer -> string -> unit

val telemetry_log : Jsonrpc.writer -> string -> unit

val log : Jsonrpc.writer -> Lsp.MessageType.t -> string -> unit

val log_error : Jsonrpc.writer -> string -> unit

val log_warning : Jsonrpc.writer -> string -> unit

val log_info : Jsonrpc.writer -> string -> unit

val dismiss_diagnostics : Jsonrpc.writer -> Lsp.UriSet.t -> Lsp.UriSet.t

val notify_connectionStatus : Lsp.Initialize.params -> Jsonrpc.writer -> bool -> bool -> bool
