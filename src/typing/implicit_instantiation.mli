(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Check = Context.Implicit_instantiation_check

module type OBSERVER = sig
  type output

  val on_constant_tparam : Context.t -> string -> output

  val on_pinned_tparam : Context.t -> string -> Type.t -> output

  val on_missing_bounds :
    Context.t ->
    string ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output

  val on_upper_non_t :
    Context.t ->
    string ->
    Type.use_t ->
    tparam_binder_reason:Reason.reason ->
    instantiation_reason:Reason.reason ->
    output
end

module type KIT = sig
  type output

  val fold :
    Context.t ->
    Context.master_context ->
    f:(Context.t -> 'acc -> Check.t -> output SMap.t -> 'acc) ->
    init:'acc ->
    post:(init_cx:Context.t -> cx:Context.t -> unit) ->
    Check.t list ->
    'acc
end

module Make (Observer : OBSERVER) : KIT with type output = Observer.output
