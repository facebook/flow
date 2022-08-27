(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

include module type of Name_def_sig

val default_of_binding : binding -> default Base.Option.t

val find_defs :
  Env_api.env_entry Env_api.EnvMap.t ->
  Provider_api.info ->
  (ALoc.t, ALoc.t) Flow_ast.Program.t ->
  map
