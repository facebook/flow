(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Generates the 'from' part of 'import ... from ...' required to import [source] from
    a file in [src_dir] *)
val from_of_source :
  options:Options.t ->
  get_haste_name:(File_key.t -> string option) ->
  get_package_info:(File_key.t -> (Package_json.t, 'a) result option) ->
  is_package_file:(string -> bool) ->
  src_dir:string option ->
  Export_index.source ->
  string option

val text_edits_of_import :
  options:Options.t ->
  get_haste_name:(File_key.t -> string option) ->
  get_package_info:(File_key.t -> (Package_json.t, unit) result option) ->
  is_package_file:(string -> bool) ->
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
    string option ->
    File_key.t ->
    string option ->
    string option
end
