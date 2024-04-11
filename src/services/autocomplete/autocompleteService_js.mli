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
  layout_options:Js_layout_generator.opts ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast_from_shared_mem:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  module_system_info:Lsp_module_system_info.t ->
  search_exported_values:(ac_options:ac_options -> string -> Export_search_types.search_results) ->
  search_exported_types:(ac_options:ac_options -> string -> Export_search_types.search_results) ->
  cx:Context.t ->
  file_sig:File_sig.t ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  available_ast:Typed_ast_utils.available_ast ->
  typing

type 'r autocomplete_service_result_generic =
  | AcResult of 'r ac_result
  | AcEmpty of string
  | AcFatalError of string

type autocomplete_service_result =
  ServerProt.Response.Completion.t autocomplete_service_result_generic

val equal_autocomplete_service_result :
  autocomplete_service_result -> autocomplete_service_result -> bool

val autocomplete_get_results :
  typing ->
  ac_options ->
  string option ->
  Loc.t ->
  Autocomplete_sigil.Canonical.token option ->
  string option * ALoc.t option * string * autocomplete_service_result
