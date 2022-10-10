(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val resolve : Context.t -> require_resolution:bool -> Type.t -> unit

val resolved_t : Context.t -> require_resolution:bool -> Type.t -> Type.t

val resolved_fun_call_type :
  Context.t -> require_resolution:bool -> Type.funcalltype -> Type.funcalltype

val resolved_call_arg : Context.t -> require_resolution:bool -> Type.call_arg -> Type.call_arg

val resolved_typeparam : Context.t -> require_resolution:bool -> Type.typeparam -> Type.typeparam
