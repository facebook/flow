(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving show]

val max_size : int

val all_zero : int -> t

val all_one : int -> t

val mem : int -> t -> bool

val set : int -> t -> t

val unset : int -> t -> t

val is_subset : t -> t -> bool

val no_overlap : t -> t -> bool

val equal : t -> t -> bool

val from_int_unchecked : int -> t

val to_int : t -> int

val compare : t -> t -> int

val to_string : t -> string
