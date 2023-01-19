(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Name_def_types

val function_params_all_annotated :
  (ALoc.t, ALoc.t) Flow_ast.Function.Params.t -> (ALoc.t, ALoc.t) Flow_ast.Function.body -> bool

val expression_is_definitely_synthesizable :
  autocomplete_hooks:Env_api.With_ALoc.autocomplete_hooks ->
  (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
  bool

val find_defs :
  autocomplete_hooks:Env_api.With_ALoc.autocomplete_hooks ->
  Env_api.env_entry Env_api.EnvMap.t ->
  Env_api.read Loc_sig.ALocS.LMap.t ->
  Provider_api.info ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  env_entries_map * hint_map
