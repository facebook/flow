(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type S = sig
  module Env_api : Env_api.S with module L = Loc_sig.ALocS

  val jsx_attributes_possible_sentinel_refinements :
    (ALoc.t, ALoc.t) Flow_ast.JSX.Opening.attribute list -> Hint_api.sentinel_refinement SMap.t

  val object_properties_possible_sentinel_refinements :
    (ALoc.t, ALoc.t) Flow_ast.Expression.Object.property list -> Hint_api.sentinel_refinement SMap.t

  val visit_eq_test :
    on_type_of_test:
      (ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      string ->
      bool ->
      'a
      ) ->
    on_literal_test:
      (strict:bool ->
      sense:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      Env_api.Refi.refinement_kind ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      'a
      ) ->
    on_null_test:
      (sense:bool ->
      strict:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      'a
      ) ->
    on_void_test:
      (sense:bool ->
      strict:bool ->
      check_for_bound_undefined:bool ->
      ALoc.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
      'a
      ) ->
    on_member_eq_other:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> 'a) ->
    on_other_eq_member:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> 'a) ->
    is_switch_cond_context:bool ->
    on_other_eq_test:
      ((ALoc.t, ALoc.t) Flow_ast.Expression.t -> (ALoc.t, ALoc.t) Flow_ast.Expression.t -> 'a) ->
    strict:bool ->
    sense:bool ->
    ALoc.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    (ALoc.t, ALoc.t) Flow_ast.Expression.t ->
    'a
end

module Make
    (Scope_api : Scope_api_sig.S with module L = Loc_sig.ALocS)
    (Ssa_api : Ssa_api.S with module L = Loc_sig.ALocS)
    (Env_api : Env_api.S
                 with module L = Loc_sig.ALocS
                  and module Scope_api = Scope_api
                  and module Ssa_api = Ssa_api) : S with module Env_api = Env_api
