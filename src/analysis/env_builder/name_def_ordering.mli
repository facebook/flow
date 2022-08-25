(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason
open Dependency_sigs
module EnvMap = Env_api.EnvMap
module EnvSet = Env_api.EnvSet

type element =
  | Normal of Env_api.EnvKey.t
  | Resolvable of Env_api.EnvKey.t
  | Illegal of {
      loc: Env_api.EnvKey.t;
      reason: ALoc.t virtual_reason;
      recursion: ALoc.t Nel.t;
    }

type result =
  | Singleton of element
  | ResolvableSCC of element Nel.t
  | IllegalSCC of (element * ALoc.t virtual_reason * ALoc.t Nel.t) Nel.t

val string_of_component :
  (Name_def.def * 'a * ALoc.t list * ALoc.t Reason.virtual_reason) EnvMap.t -> result -> string

module type S = sig
  type cx

  val build_ordering :
    cx ->
    Env_api.env_info ->
    (Name_def.def * 'a * ALoc.t list * ALoc.t Reason.virtual_reason) EnvMap.t ->
    result Base.List.t
end

module Make (Context : C) (_ : F with type cx = Context.t) : S with type cx = Context.t

module Make_Test_With_Cx (Context : C) : S with type cx = Context.t
