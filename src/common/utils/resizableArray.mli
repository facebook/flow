(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* An array that automatically expands when needed *)

type 'a t

exception Out_of_bounds_set of string

(* `make x` creates a ResizableArray.t where the underlying array has the initial size of `x`.
 * However, this is purely for the purposes of optimization:
 * `ResizableArray.size (ResizableArray.make 5)` still * evaluates to `0`. *)
val make : int -> 'a t

(* `set arr i x` raises `Out_of_bounds_set` if `i >= ResizableArray.size arr`, or if `i < 0` *)
val set : 'a t -> int -> 'a -> unit

(* Expands the underlying array if necessary *)
val push : 'a t -> 'a -> unit

(* Shrinks the representation to match the number of elements stored *)
val shrink : 'a t -> unit

(* Returns None if the index is out of bounds. *)
val get : 'a t -> int -> 'a option

val size : 'a t -> int

(* Exposed only for white box testing. Do not use this. Really. *)
val underlying_array_size_do_not_use : 'a t -> int

val to_hashtbl : 'a t -> ('a, int) Hashtbl.t
