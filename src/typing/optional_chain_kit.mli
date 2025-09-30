(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Make (_ : Flow_common.S) : sig
  val run :
    Context.t ->
    Type.DepthTrace.t ->
    Type.t ->
    reason:Reason.reason ->
    lhs_reason:Reason.reason ->
    upper:Type.use_t ->
    voided_out_collector:Type.TypeCollector.t option ->
    unit
end
