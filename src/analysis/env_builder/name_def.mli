(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Name_def_types

val pattern_has_annot : (ALoc.t, ALoc.t) Ast.Pattern.t -> bool

val show_scope_kind : scope_kind -> string

val predicate_synthesizable :
  (ALoc.t, ALoc.t) Ast.Type.Predicate.t option ->
  (ALoc.t, ALoc.t) Ast.Function.body ->
  function_synth_kind

val expression_is_definitely_synthesizable :
  autocomplete_hooks:Env_api.With_ALoc.autocomplete_hooks ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  bool

val find_defs :
  autocomplete_hooks:Env_api.With_ALoc.autocomplete_hooks ->
  Env_api.env_entry Env_api.EnvMap.t ->
  Env_api.read Loc_sig.ALocS.LMap.t ->
  Provider_api.info ->
  scope_kind ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  env_entries_map * hint_map
