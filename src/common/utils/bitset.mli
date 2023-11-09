(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving show]

val all_zero : int -> t

val all_one : int -> t

val mem : int -> t -> bool

val set : t -> int -> unit

val unset : t -> int -> unit

val is_subset : t -> t -> bool

val no_overlap : t -> t -> bool
