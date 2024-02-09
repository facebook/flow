(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module AcCompletion : sig
  type completion_item = {
    kind: Lsp.Completion.completionItemKind option;
    name: string;
    labelDetail: string option;  (** LSP's CompletionItemLabelDetails.detail *)
    description: string option;  (** LSP's CompletionItemLabelDetails.description *)
    itemDetail: string option;  (** LSP's CompletionItem.detail *)
    text_edit: ServerProt.Response.insert_replace_edit option;
    additional_text_edits: ServerProt.Response.textedit list;
    sort_text: string option;
    preselect: bool;
    documentation_and_tags: (string option * Lsp.CompletionItemTag.t list option) Lazy.t;
    log_info: string;
    insert_text_format: Lsp.Completion.insertTextFormat;
  }

  type t = {
    items: completion_item list;
    is_incomplete: bool;
  }

  val to_server_prot_completion_t : t -> ServerProt.Response.Completion.t
end

type ac_options = {
  imports: bool;
  imports_min_characters: int;
  imports_ranked_usage: bool;
  imports_ranked_usage_boost_exact_match_min_length: int option;
  show_ranking_info: bool;
}

type ac_result = {
  result: AcCompletion.t;
  errors_to_log: string list;
}

type typing = {
  options: Options.t;
  reader: Parsing_heaps.Reader.reader;
  cx: Context.t;
  file_sig: File_sig.t;
  ast: (Loc.t, Loc.t) Flow_ast.Program.t;
  typed_ast: (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t;
  exports: Export_search.t;
}

type autocomplete_service_result =
  | AcResult of ac_result
  | AcEmpty of string
  | AcFatalError of string

val autocomplete_get_results :
  typing ->
  ac_options ->
  string option ->
  Loc.t ->
  string option * ALoc.t option * string * autocomplete_service_result
