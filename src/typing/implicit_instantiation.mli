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

module PinTypes (_ : Flow_common.S) : sig
  val pin_type : Context.t -> use_op:Type.use_op -> Reason.reason -> Type.t -> Type.t
end

module Make_instantiation_solver (_ : Flow_common.S) : sig
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
    ?has_syntactic_hint:bool ->
    ?return_hint:Type.t * Hint.hint_kind ->
    Check.t ->
    inferred_targ Subst_name.Map.t * (Type.t * Subst_name.Name.t) list
end

module type KIT = sig
  module Flow : Flow_common.S

  module Instantiation_helper : Flow_js_utils.Instantiation_helper_sig

  val run_call :
    Context.t ->
    Implicit_instantiation_check.t ->
    return_hint:Type.lazy_hint_t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    Type.t * (Type.t * Subst_name.Name.t) list

  val run_monomorphize :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    reason_op:Reason.reason ->
    reason_tapp:Reason.reason ->
    Type.typeparam Nel.t ->
    Type.t ->
    Type.t

  val run_conditional :
    Context.t ->
    Type.DepthTrace.t ->
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

  val run_render_extractor :
    Context.t -> use_op:Type.use_op -> reason:Reason.reason -> Type.t -> Type.t

  val run_await : Context.t -> use_op:Type.use_op -> reason:Reason.reason -> Type.t -> Type.t
end

module Kit (Flow : Flow_common.S) (Instantiation_helper : Flow_js_utils.Instantiation_helper_sig) :
  KIT with module Flow = Flow with module Instantiation_helper = Instantiation_helper
