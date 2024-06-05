(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type CUSTOM_FUN = sig
  val run :
    Context.t ->
    Type.DepthTrace.t ->
    use_op:Type.use_op ->
    return_hint:Type.lazy_hint_t ->
    Reason.t ->
    Type.custom_fun_kind ->
    Type.targ list option ->
    Type.t list ->
    Type.t option ->
    Type.t ->
    unit
end

module Kit (Flow : Flow_common.S) : CUSTOM_FUN = struct
  include Flow

  let run _cx _trace ~use_op:_ ~return_hint:_ _reason_op _kind _targs _args _spread_arg _tout =
    failwith "implemented elsewhere"
end
