(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val first_upto_n : int -> (int -> 'a option) -> 'a list -> 'a list

val ident_map : ('a -> 'a) -> 'a list -> 'a list

val zipi : 'a list -> 'b list -> (int * 'a * 'b) list

val dedup : 'a list -> 'a list

val to_string : string -> ('a -> string) -> 'a list -> string
