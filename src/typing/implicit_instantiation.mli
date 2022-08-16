(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Check = Implicit_instantiation_check

module type OBSERVER = sig
  type output

  val on_constant_tparam : Context.t -> Subst_name.t -> Type.typeparam -> Type.t -> output

  val on_pinned_tparam : Context.t -> Subst_name.t -> Type.typeparam -> Type.t -> output

  val on_missing_bounds :
    Context.t ->
    Subst_name.t ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output

  val on_upper_non_t :
    Context.t ->
    Subst_name.t ->
    Type.use_t ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output
end

module type S = sig
  type output

  module Flow : Flow_common.S

  val solve_targs : Context.t -> Check.t -> output Subst_name.Map.t

  val run :
    Context.t ->
    Check.t ->
    on_completion:(Context.t -> output Subst_name.Map.t -> 'result) ->
    'result

  val fold :
    implicit_instantiation_cx:Context.t ->
    cx:Context.t ->
    f:(Context.t -> 'acc -> Check.t -> output Subst_name.Map.t -> 'acc) ->
    init:'acc ->
    post:(cx:Context.t -> implicit_instantiation_cx:Context.t -> unit) ->
    Check.t list ->
    'acc
end

module Make (Observer : OBSERVER) (Flow : Flow_common.S) :
  S with type output = Observer.output with module Flow = Flow

type inferred_targ = {
  tparam: Type.typeparam;
  inferred: Type.t;
}

module Pierce (Flow : Flow_common.S) : S with type output = inferred_targ with module Flow = Flow

module type KIT = sig
  module Flow : Flow_common.S

  module Instantiation_helper : Flow_js_utils.Instantiation_helper_sig

  val run :
    Context.t ->
    Implicit_instantiation_check.t ->
    has_context:bool ->
    ?cache:Reason.t list ->
    Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    Type.t
end

module Kit (Flow : Flow_common.S) (Instantiation_helper : Flow_js_utils.Instantiation_helper_sig) :
  KIT with module Flow = Flow with module Instantiation_helper = Instantiation_helper
