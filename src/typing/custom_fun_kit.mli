(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type CUSTOM_FUN = sig
  val run :
    Context.t ->
    Type.trace ->
    use_op:Type.use_op ->
    return_hint:Type.lazy_hint_t ->
    Reason.t ->
    Type.custom_fun_kind ->
    Type.t list ->
    Type.t option ->
    Type.t ->
    unit
end

module Kit (_ : Flow_common.S) : CUSTOM_FUN
