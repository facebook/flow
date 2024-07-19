(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Type

module type S = sig
  val predicate : Context.t -> DepthTrace.t -> tvar -> Type.t -> predicate -> unit

  val prop_exists_test_generic :
    string ->
    Reason.reason ->
    Context.t ->
    DepthTrace.t ->
    tvar ->
    Type.t ->
    bool ->
    predicate * predicate ->
    Type.t ->
    unit

  val sentinel_prop_test_generic :
    string -> Context.t -> DepthTrace.t -> tvar -> Type.t -> bool * Type.t * Type.t -> unit
end

module Make (_ : Flow_common.S) : S
