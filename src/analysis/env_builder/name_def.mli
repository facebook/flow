(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Name_def_types

val pattern_has_annot : (ALoc.t, ALoc.t) Ast.Pattern.t -> bool

val show_scope_kind : scope_kind -> string

val expression_is_definitely_synthesizable :
  autocomplete_hooks:Env_api.With_ALoc.autocomplete_hooks ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  bool

val find_defs :
  autocomplete_hooks:Env_api.With_ALoc.autocomplete_hooks ->
  react_jsx:bool ->
  Env_api.env_info ->
  scope_kind ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  env_entries_map * hint_map
