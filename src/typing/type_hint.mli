(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val evaluate_hint :
  Context.t ->
  Reason.t ->
  (Type.t, Type.targ list option, (ALoc.t * Type.call_arg) list) Hint_api.hint ->
  Type.t option

val sandbox_flow_succeeds : Context.t -> Type.t * Type.t -> bool
