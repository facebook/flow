(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val lazily_resolve_hints : Context.t -> ALoc.t -> Name_def.ast_hints -> Type.lazy_hint_t

val resolve_component :
  Context.t ->
  (Name_def.def * Name_def.scope_kind * Name_def.class_stack * Reason.t) Env_api.EnvMap.t ->
  Name_def_ordering.result ->
  unit
