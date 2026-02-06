(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type INPUT = sig
  include Flow_common.BASE
end

module type OUTPUT = sig
  val try_union :
    Context.t ->
    ?on_success:(unit -> unit) ->
    Type.DepthTrace.t ->
    Type.use_op ->
    Type.t ->
    Reason.reason ->
    Type.UnionRep.t ->
    unit

  val try_intersection :
    Context.t -> Type.DepthTrace.t -> Type.use_t -> Reason.reason -> Type.InterRep.t -> unit

  val try_custom :
    Context.t ->
    ?use_op:Type.use_op ->
    ?use_t:Type.use_t ->
    ?default_resolve:(unit -> unit) ->
    no_match_error_loc:ALoc.t ->
    (unit -> unit) list ->
    unit

  val try_singleton_throw_on_failure :
    Context.t -> Type.DepthTrace.t -> Type.t -> Type.use_t -> unit

  val try_singleton_custom_throw_on_failure : Context.t -> (unit -> unit) -> unit

  val try_unify : Context.t -> Type.DepthTrace.t -> Type.t -> Type.use_op -> Type.t -> unit
end

module Make (_ : INPUT) : OUTPUT
