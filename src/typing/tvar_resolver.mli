(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val has_placeholders : Context.t -> Type.t -> bool

val has_unresolved_tvars : Context.t -> Type.t -> bool

val resolve : ?no_lowers:(Reason.t -> Type.t) -> Context.t -> Type.t -> unit

val resolved_t : ?no_lowers:(Reason.t -> Type.t) -> Context.t -> Type.t -> Type.t

val mk_tvar_and_fully_resolve_where : Context.t -> Reason.reason -> (Type.t -> unit) -> Type.t

val mk_tvar_and_fully_resolve_no_wrap_where :
  Context.t -> Reason.t -> (Reason.t * int -> unit) -> Type.t
