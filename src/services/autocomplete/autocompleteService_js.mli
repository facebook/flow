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
[@@deriving eq, show]

type typing

val mk_typing_artifacts :
  file_options:Files.options ->
  layout_options:Js_layout_generator.opts ->
  haste_module_system:bool ->
  loc_of_aloc:(ALoc.t -> Loc.t) ->
  get_ast:(File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option) ->
  get_haste_name:(File_key.t -> string option) ->
  get_package_info:(File_key.t -> (Package_json.t, unit) result option) ->
  is_package_file:(string -> bool) ->
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
[@@deriving eq, show]

type autocomplete_service_result =
  ServerProt.Response.Completion.t autocomplete_service_result_generic
[@@deriving eq, show]

val autocomplete_get_results :
  typing ->
  ac_options ->
  string option ->
  Loc.t ->
  string option * ALoc.t option * string * autocomplete_service_result
