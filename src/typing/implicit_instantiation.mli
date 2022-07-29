(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Check = Implicit_instantiation_check

module type OBSERVER = sig
  type output

  val on_constant_tparam : Context.t -> Subst_name.t -> Type.t -> output

  val on_pinned_tparam : Context.t -> Subst_name.t -> Type.t -> output

  val on_missing_bounds :
    Context.t ->
    Subst_name.t ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output

  val on_upper_non_t :
    Context.t ->
    Subst_name.t ->
    Type.use_t ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output
end

module type S = sig
  type output

  module Flow : Flow_common.S

  val solve_targs : Context.t -> Check.t -> output Subst_name.Map.t

  val fold :
    Context.t ->
    Context.master_context ->
    f:(Context.t -> 'acc -> Check.t -> output Subst_name.Map.t -> 'acc) ->
    init:'acc ->
    post:(init_cx:Context.t -> cx:Context.t -> unit) ->
    Check.t list ->
    'acc
end

module Make (Observer : OBSERVER) (Flow : Flow_common.S) :
  S with type output = Observer.output with module Flow = Flow

module Pierce (Flow : Flow_common.S) : S with type output = Type.t with module Flow = Flow
