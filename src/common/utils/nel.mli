(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type 'a t = 'a * 'a list

val to_list: 'a t -> 'a list
val of_list: 'a list -> 'a t option
val one: 'a -> 'a t
val cons: 'a -> 'a t -> 'a t
val mem: 'a -> 'a t -> bool
val exists: ('a -> bool) -> 'a t -> bool
val iter: ('a -> unit) -> 'a t -> unit
val map: ('a -> 'b) -> 'a t -> 'b t
val ident_map: ('a -> 'a) -> 'a t -> 'a t
val concat: 'a t t -> 'a t
val map_concat: ('a -> 'b t) -> 'a t -> 'b t
val rev: 'a t -> 'a t
val rev_map: ('a -> 'b) -> 'a t -> 'b t
val rev_append: 'a t -> 'a t -> 'a t
val length: 'a t -> int
val fold_left: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val hd: 'a t -> 'a
val nth: 'a t -> int -> 'a

val result_all: ('a, 'b) result t -> ('a t, 'b) result
val cat_maybes: 'a option t -> 'a t option
