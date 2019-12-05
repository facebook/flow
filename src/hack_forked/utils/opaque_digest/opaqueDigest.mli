(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t

val compare : t -> t -> int

val equal : t -> t -> bool

val string : string -> t

val bytes : bytes -> t

val substring : string -> int -> int -> t

val subbytes : bytes -> int -> int -> t

val channel : Pervasives.in_channel -> int -> t

val file : string -> t

val output : Pervasives.out_channel -> t -> unit

val input : Pervasives.in_channel -> t

val to_hex : t -> string

val from_hex : string -> t

val to_raw_contents : t -> string

val from_raw_contents : string -> t option
