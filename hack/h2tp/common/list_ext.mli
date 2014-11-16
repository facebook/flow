(**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

include (module type of List)

val first : ('a -> bool) -> 'a list -> 'a option

(*
  returns the last element of a list. If there is no element, it raises
  Invalid_argument
*)
val lst_unsafe : 'a list -> 'a

val is_empty: 'a list -> bool

val not_empty: 'a list -> bool

val intersperse : 'a -> 'a list -> 'a list

val concatMap : ('a -> 'b list) -> 'a list -> 'b list
