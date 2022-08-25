(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type C = Dependency_sigs.C

module type F = Dependency_sigs.F

module type S = sig
  module Env_api : Env_api.S with module L = Loc_sig.ALocS

  type cx

  type abrupt_kind

  exception AbruptCompletionExn of abrupt_kind

  val program_with_scope :
    cx ->
    ?lib:bool ->
    ?exclude_syms:NameUtils.Set.t ->
    (ALoc.t, ALoc.t) Flow_ast.Program.t ->
    abrupt_kind option * Env_api.env_info

  val program :
    cx ->
    ?lib:bool ->
    ?exclude_syms:NameUtils.Set.t ->
    (ALoc.t, ALoc.t) Flow_ast.Program.t ->
    Env_api.values * (int -> Env_api.refinement)
end

module Make_Test_With_Cx (Context : C) :
  S with module Env_api = Env_api.With_ALoc and type cx = Context.t

module Make_of_flow (Context : C) (_ : F with type cx = Context.t) :
  S with module Env_api = Env_api.With_ALoc and type cx = Context.t
