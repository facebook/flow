(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving show]

val mk : module_name:string -> namespace_bitset:Bitset.t -> t

val module_name : t -> string

val namespace_bitset : t -> Bitset.t

val equal : t -> t -> bool

val compare : t -> t -> int

val to_string : t -> string
