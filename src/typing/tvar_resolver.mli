(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val resolve : Context.t -> ?no_lowers:(Reason.t -> Type.t) -> ?filter_empty:bool -> Type.t -> unit

val resolved_t :
  ?no_lowers:(Reason.t -> Type.t) -> ?filter_empty:bool -> Context.t -> Type.t -> Type.t

val mk_tvar_and_fully_resolve_where : Context.t -> Reason.reason -> (Type.t -> unit) -> Type.t

val mk_tvar_and_fully_resolve_no_wrap_where :
  Context.t -> Reason.t -> (Reason.t * int -> unit) -> Type.t
