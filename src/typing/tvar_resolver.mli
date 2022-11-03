(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type unconstrained_tvar_resolution_strategy =
  | Allow
  | Error
  | Exception

exception UnconstrainedTvarException of int

val has_placeholders : Context.t -> Type.t -> bool

val resolve :
  Context.t -> on_unconstrained_tvar:unconstrained_tvar_resolution_strategy -> Type.t -> unit

val resolved_t :
  Context.t -> on_unconstrained_tvar:unconstrained_tvar_resolution_strategy -> Type.t -> Type.t

val resolved_fun_call_type :
  Context.t ->
  on_unconstrained_tvar:unconstrained_tvar_resolution_strategy ->
  Type.funcalltype ->
  Type.funcalltype

val resolved_call_arg :
  Context.t ->
  on_unconstrained_tvar:unconstrained_tvar_resolution_strategy ->
  Type.call_arg ->
  Type.call_arg

val resolved_type_args :
  Context.t ->
  on_unconstrained_tvar:unconstrained_tvar_resolution_strategy ->
  Type.targ list option ->
  Type.targ list option

val resolved_typeparam :
  Context.t ->
  on_unconstrained_tvar:unconstrained_tvar_resolution_strategy ->
  Type.typeparam ->
  Type.typeparam
