(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type userland [@@deriving show, ord]

val userland : string -> userland

val unwrap_userland : userland -> string

type t = Userland of userland [@@deriving show, ord]

val userland_specifier : string -> t

module Map : WrappedMap_sig.S with type key = t

module Set : Flow_set.S with type elt = t
