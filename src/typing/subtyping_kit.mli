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
end

module type OUTPUT = sig
  val rec_sub_t : Context.t -> Type.use_op -> Type.t -> Type.t -> Type.trace -> unit

  val rec_flow_p :
    Context.t ->
    ?trace:Type.trace ->
    use_op:Type.use_op ->
    ?report_polarity:bool ->
    Reason.reason ->
    Reason.reason ->
    Type.propref ->
    Type.property * Type.property ->
    unit
end

module Make (_ : INPUT) : OUTPUT
