(*
 * Copyright (c) 2019, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* This `.mli` file was generated automatically. It may include extra
definitions that should not actually be exposed to the caller. If you notice
that this interface file is a poor interface, please take a few minutes to
clean it up manually, and then delete this comment once the interface is in
shape. *)

type 'a t

val make_pair : 'a -> 'b -> 'a * 'b

val common_prefix : string -> string -> int

val drop : string -> int -> string

val take : string -> int -> string

val ( |> ) : 'a -> ('a -> 'b) -> 'b

val id : 'a -> 'a

type 'a return = { return: 'b. 'a -> 'b }

val with_return : ('a return -> 'a) -> 'a

val create : unit -> 'a t

exception Inconsistent_trie of string

val get_node : 'a t -> 'a t SMap.t ref

val get_leaf : 'a t -> 'a

val trie_assoc_partial : 'a t -> string -> (int * string * 'a t) option

val mem : 'a t -> string -> bool

val add_one : 'a t -> string -> 'a t -> unit

val split_key : 'a t -> string -> 'a t -> int -> 'a t

val add_leaf : 'a t -> string -> 'a -> unit

val add :
  ?if_exist:('b -> 'a -> unit) ->
  transform:('a -> 'b) ->
  'b t ->
  string ->
  'a ->
  unit

val to_list :
  int option -> 'b t -> (string -> 'a) -> ('a -> 'b -> 'c) -> 'c list

val find_impl :
  ?limit:int option -> bool -> 'a t -> string -> (string -> 'a -> 'c) -> 'c list

val find : 'a t -> string -> 'a

val find_prefix : 'a t -> string -> (string -> 'a -> 'b) -> 'b list

val find_prefix_limit : int -> 'a t -> string -> (string -> 'a -> 'b) -> 'b list

val remove_one : 'a t -> string -> unit

val remove_impl : bool -> 'a t -> string -> unit

val remove : 'a t -> string -> unit

val remove_prefix : 'a t -> string -> unit

val to_string_impl : Buffer.t -> 'a t -> unit

val to_string : 'a t -> string
