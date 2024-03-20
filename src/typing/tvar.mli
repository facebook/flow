(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Reason

val mk_no_wrap : Context.t -> reason -> int

val mk : Context.t -> reason -> Type.t

val mk_where : Context.t -> reason -> (Type.t -> unit) -> Type.t

val mk_where_no_wrap : Context.t -> reason -> (Type.t -> unit) -> int

val mk_no_wrap_where : Context.t -> reason -> (reason * int -> unit) -> Type.t

val mk_fully_resolved_lazy : Context.t -> reason -> Type.t lazy_t -> Type.t

val mk_fully_resolved : Context.t -> reason -> Type.t -> Type.t

val mk_resolved : Context.t -> reason -> Type.t -> Type.t
