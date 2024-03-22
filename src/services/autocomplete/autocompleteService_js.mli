(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type ac_options = {
  imports: bool;
  imports_min_characters: int;
  imports_ranked_usage: bool;
  imports_ranked_usage_boost_exact_match_min_length: int option;
  show_ranking_info: bool;
}

type 'r ac_result = {
  result: 'r;
  errors_to_log: string list;
}

type typing

val mk_typing_artifacts :
  options:Options.t ->
  reader:Parsing_heaps.Reader.reader ->
  cx:Context.t ->
  file_sig:File_sig.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  available_ast:Typed_ast_utils.available_ast ->
  exports:Export_search.t ->
  typing

type 'r autocomplete_service_result_generic =
  | AcResult of 'r ac_result
  | AcEmpty of string
  | AcFatalError of string

type autocomplete_service_result =
  ServerProt.Response.Completion.t autocomplete_service_result_generic

val autocomplete_get_results :
  typing ->
  ac_options ->
  string option ->
  Loc.t ->
  string option * ALoc.t option * string * autocomplete_service_result
