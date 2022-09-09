(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module TvarResolver : sig
  val resolved_t : Context.t -> Type.t -> Type.t
end

val resolve_component :
  Context.t ->
  (Name_def.def * Name_def.scope_kind * Name_def.class_stack * Reason.t) Env_api.EnvMap.t ->
  Name_def_ordering.result ->
  unit
