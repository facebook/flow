(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type INPUT = sig
  include Flow_common.BASE

  include Flow_common.BUILTINS

  include Flow_common.SUBTYPING

  include Flow_common.REACT
end

module type S = sig
  val rec_renders_to_renders :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    (Reason.reason * Type.canonical_renders_form) * (Reason.reason * Type.canonical_renders_form) ->
    unit

  val non_renders_to_renders :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    Type.t ->
    Reason.reason * Type.canonical_renders_form ->
    unit
end

module Make (_ : INPUT) : S
