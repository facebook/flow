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

type 'k blame = {
  payload: 'k;
  reason: ALoc.t virtual_reason;
  annot_locs: ALoc.t Env_api.annot_loc list;
  recursion: ALoc.t list;
}

type element =
  | Normal of Env_api.EnvKey.t
  | Resolvable of Env_api.EnvKey.t
  | Illegal of Env_api.EnvKey.t blame

type result =
  | Singleton of element
  | ResolvableSCC of element Nel.t
  | IllegalSCC of (element blame * bool) Nel.t

val string_of_component :
  (Name_def.def * 'a * ALoc.t list * ALoc.t Reason.virtual_reason) EnvMap.t -> result -> string

module type S = sig
  type cx

  val build_ordering :
    cx ->
    autocomplete_hooks:Env_api.With_ALoc.autocomplete_hooks ->
    Env_api.env_info ->
    (Name_def.def * 'a * ALoc.t list * ALoc.t Reason.virtual_reason) EnvMap.t ->
    result Base.List.t
end

module Make (Context : C) (_ : F with type cx = Context.t) : S with type cx = Context.t

module Make_Test_With_Cx (Context : C) : S with type cx = Context.t
