(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Check = Implicit_instantiation_check

type inferred_targ = {
  tparam: Type.typeparam;
  inferred: Type.t;
}

module type OBSERVER = sig
  val on_pinned_tparam : Context.t -> Type.typeparam -> Type.t -> inferred_targ

  val on_constant_tparam_missing_bounds : Context.t -> Type.typeparam -> inferred_targ

  val on_missing_bounds :
    Context.t ->
    use_op:Type.use_op ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    inferred_targ

  val on_upper_non_t :
    Context.t ->
    use_op:Type.use_op ->
    Type.use_t ->
    Type.typeparam ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    inferred_targ
end

module type S = sig
  module Flow : Flow_common.S

  val pin_type :
    Context.t ->
    use_op:Type.use_op ->
    Type.typeparam ->
    Polarity.t option ->
    default_bound:Type.t option ->
    Reason.reason ->
    Type.t ->
    inferred_targ

  val solve_targs :
    Context.t ->
    use_op:Type.use_op ->
    ?allow_underconstrained:bool ->
    ?return_hint:Type.t * Hint.hint_kind ->
    Check.t ->
    inferred_targ Subst_name.Map.t

  val solve_conditional_type_targs :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    reason:Reason.reason ->
    tparams:Type.typeparam list ->
    check_t:Type.t ->
    extends_t:Type.t ->
    true_t:Type.t ->
    Type.t Subst_name.Map.t option

  val fold :
    implicit_instantiation_cx:Context.t ->
    cx:Context.t ->
    f:(Context.t -> 'acc -> Check.t -> inferred_targ Subst_name.Map.t -> 'acc) ->
    init:'acc ->
    post:(cx:Context.t -> implicit_instantiation_cx:Context.t -> unit) ->
    Check.t list ->
    'acc
end

module Make (_ : OBSERVER) (Flow : Flow_common.S) : S with module Flow = Flow

module PinTypes (_ : Flow_common.S) : sig
  val pin_type : Context.t -> use_op:Type.use_op -> Reason.reason -> Type.t -> Type.t
end

module Pierce (Flow : Flow_common.S) : S with module Flow = Flow

module type KIT = sig
  module Flow : Flow_common.S

  module Instantiation_helper : Flow_js_utils.Instantiation_helper_sig

  val run_call :
    Context.t ->
    Implicit_instantiation_check.t ->
    return_hint:Type.lazy_hint_t ->
    ?cache:bool ->
    Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    Type.t

  val run_monomorphize :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    Type.typeparam Nel.t ->
    Type.t ->
    Type.t

  val run_conditional :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    reason:Reason.reason ->
    tparams:Type.typeparam list ->
    check_t:Type.t ->
    extends_t:Type.t ->
    true_t:Type.t ->
    false_t:Type.t ->
    Type.t

  val run_ref_extractor :
    Context.t -> use_op:Type.use_op -> reason:Reason.reason -> Type.t -> Type.t
end

module Kit (Flow : Flow_common.S) (Instantiation_helper : Flow_js_utils.Instantiation_helper_sig) :
  KIT with module Flow = Flow with module Instantiation_helper = Instantiation_helper
