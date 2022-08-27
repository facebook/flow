(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module Env_api : Env_api.S with module L = Loc_sig.ALocS

  val visit_eq_test :
    on_type_of_test:
      (ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      string ->
      bool ->
      unit
      ) ->
    on_literal_test:
      (strict:bool ->
      sense:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      Env_api.Refi.refinement_kind ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      unit
      ) ->
    on_null_test:
      (sense:bool ->
      strict:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      unit
      ) ->
    on_void_test:
      (sense:bool ->
      strict:bool ->
      check_for_bound_undefined:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      unit
      ) ->
    on_member_eq_other:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> unit) ->
    on_other_eq_member:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> unit) ->
    is_switch_cond_context:bool ->
    on_other_eq_test:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> unit) ->
    strict:bool ->
    sense:bool ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    unit
end

module Make
    (Scope_api : Scope_api_sig.S with module L = Loc_sig.ALocS)
    (Ssa_api : Ssa_api.S with module L = Loc_sig.ALocS)
    (Env_api : Env_api.S
                 with module L = Loc_sig.ALocS
                  and module Scope_api = Scope_api
                  and module Ssa_api = Ssa_api) : S with module Env_api = Env_api
