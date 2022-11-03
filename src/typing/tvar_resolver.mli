(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val has_placeholders : Context.t -> Type.t -> bool

val resolve : Context.t -> Type.t -> unit

val resolved_t : Context.t -> Type.t -> Type.t

val resolved_fun_call_type : Context.t -> Type.funcalltype -> Type.funcalltype

val resolved_call_arg : Context.t -> Type.call_arg -> Type.call_arg

val resolved_type_args : Context.t -> Type.targ list option -> Type.targ list option

val resolved_typeparam : Context.t -> Type.typeparam -> Type.typeparam
