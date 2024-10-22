(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving show]

val mk : module_name:string -> namespace_bitset:int -> t

val of_module_name : string -> t

val module_name : t -> string

val namespace_bitset : t -> int

val equal : t -> t -> bool

val compare : t -> t -> int

val to_string : t -> string
