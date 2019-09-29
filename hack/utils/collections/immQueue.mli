(*
 * Copyright (c) 2017, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(*
 * Immutable queue implementation. Modeled loosely after the mutable stdlib
 * Queue. push, pop, etc. are amortized O(1).
 *)

type 'a t

exception Empty

val empty : 'a t

val push : 'a t -> 'a -> 'a t

val pop : 'a t -> 'a option * 'a t

val peek : 'a t -> 'a option * 'a t

(* Raises Empty if the queue is empty *)
val pop_unsafe : 'a t -> 'a * 'a t

val is_empty : 'a t -> bool

val length : 'a t -> int

val exists : 'a t -> f:('a -> bool) -> bool

val iter : 'a t -> f:('a -> unit) -> unit

(* from_list: the head of the list is the first one to be popped *)
val from_list : 'a list -> 'a t

(* to_list: the head of the list is the first one to be popped *)
val to_list : 'a t -> 'a list

val concat : 'a t list -> 'a t
