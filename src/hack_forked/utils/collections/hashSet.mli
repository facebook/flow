(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* HashSet is just a HashTable where the keys are actually the values, and we
 * ignore the actual values inside the HashTable. *)
type 'a t

val create : int -> 'a t

val clear : 'a t -> unit

val copy : 'a t -> 'a t

val add : 'a t -> 'a -> unit

val mem : 'a t -> 'a -> bool

val remove : 'a t -> 'a -> unit

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val length : 'a t -> int

val is_empty : 'a t -> bool
