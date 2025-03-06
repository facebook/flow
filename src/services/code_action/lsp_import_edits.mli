(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val is_available_autoimport_result : Context.t -> name:string -> source:Export_index.source -> bool

(** Generates the 'from' part of 'import ... from ...' required to import [source] from
    a file in [src_dir] *)
val from_of_source :
  module_system_info:Lsp_module_system_info.t ->
  src_dir:string option ->
  Export_index.source ->
  string option

val text_edits_of_import :
  layout_options:Js_layout_generator.opts ->
  module_system_info:Lsp_module_system_info.t ->
  src_dir:string option ->
  ast:(Loc.t, Loc.t) Flow_ast.Program.t ->
  Export_index.kind ->
  string ->
  Export_index.source ->
  Code_action_text_edits.t option

module For_tests : sig
  val path_of_modulename :
    node_resolver_dirnames:string list ->
    get_package_info:(File_key.t -> (Package_json.t, unit) result option) ->
    resolves_to_real_path:(from:string -> to_real_path:string -> bool) ->
    string option ->
    File_key.t ->
    string option ->
    string option
end
