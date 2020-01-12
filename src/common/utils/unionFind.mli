(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Mutable union-find/disjoint-set data structure *)
type 'a t

(* CONSTRUCTORS *)

(* Create an empty forest *)
val make : unit -> 'a t

(* Create a forest initialized with the given items. More efficient than repeatedly calling `add` *)
val of_list : 'a list -> 'a t

(* MUTATORS *)

val add : 'a t -> 'a -> unit

(* Unions the two elements. If either (or both) element does not exist, add it. *)
val union : 'a t -> 'a -> 'a -> unit

(* ACCESSORS *)

(* Finds the root element of the given element. Raises `Not_found` if the given element is not
 * already present. *)
val find : 'a t -> 'a -> 'a

(* Returns all members in the same set as the given element. Raises `Not_found` if the given element
 * is not already present. *)
val members : 'a t -> 'a -> 'a list
