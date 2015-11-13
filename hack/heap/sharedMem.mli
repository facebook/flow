(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(*****************************************************************************)
(* The heap shared across all the processes.
 *
 * The Heap is not exposed directly to the user (cf shared.mli),
 * because we don't want to mix values of different types. Instead, we want
 * to use a functor.
 *)
(*****************************************************************************)
open Utils

type config = {
  global_size: int;
  heap_size : int;
}

val default_config : config

type handle = private {
  h_fd: Unix.file_descr;
  h_global_size: int;
  h_heap_size: int;
}

(*****************************************************************************)
(* Initializes the shared memory. Must be called before forking! *)
(*****************************************************************************)

val init: config -> string -> handle
val init_default: unit -> handle

(*****************************************************************************)
(* Connect a slave to the shared heap *)
(*****************************************************************************)

val connect: handle -> unit

(*****************************************************************************)
(* The shared memory garbage collector. It must be called every time we
 * free data (cf hh_shared.c for the underlying C implementation).
 *)
(*****************************************************************************)

val collect: [ `gentle | `aggressive ] -> unit

(*****************************************************************************)
(* Must be called after the initialization of the hack server is over.
 * (cf serverInit.ml).
 *)
(*****************************************************************************)

val init_done: unit -> unit

(*****************************************************************************)
(* Serializes the shared memory and writes it to a file *)
(*****************************************************************************)
val save: string -> unit

val save_dep_table: string -> unit

(*****************************************************************************)
(* Loads the shared memory by reading from a file *)
(*****************************************************************************)
val load: string -> unit

val load_dep_table: string -> unit

(*****************************************************************************)
(* The size of the dynamically allocated shared memory section *)
(*****************************************************************************)
val heap_size : unit -> int

(*****************************************************************************)
(* Stats of the statically sized hash / dep tables *)
(*****************************************************************************)

type table_stats = {
  used_slots : int;
  slots : int;
}

val dep_stats : unit -> table_stats

val hash_stats : unit -> table_stats

(*****************************************************************************)
(* Cache invalidation. *)
(*****************************************************************************)

val invalidate_caches: unit -> unit

(*****************************************************************************)
(* The signature of a shared memory hashtable.
 * To create one: SharedMem.NoCache(struct type = my_type_of_value end).
 * The call to Make will create a hashtable in shared memory (visible to
 * all the workers).
 * Use NoCache/WithCache if you want caching or not.
 * If you do, bear in mind that the cache must be maintained by the caller.
 * So you will have to invalidate the caches yourself.
 *)
(*****************************************************************************)

module type S = sig
  type key
  type t
  module KeySet : Set.S with type elt = key
  module KeyMap : MapSig with type key = key

  (* Safe for concurrent writes, the first writer wins, the second write
   * is dismissed.
   *)
  val add: key -> t -> unit

  (* Safe for concurrent reads, but not if interleaved with any operation
   * mutating the table (add, remove etc ..).
   *)
  val get: key -> t option
  val get_old: key -> t option
  val get_old_batch: KeySet.t -> t option KeyMap.t
  val remove_old_batch: KeySet.t -> unit
  val find_unsafe: key -> t
  val get_batch: KeySet.t -> t option KeyMap.t
  val remove_batch: KeySet.t -> unit

  (* Safe for concurrent access. *)
  val mem: key -> bool

  (* This function takes the elements present in the set and keep the "old"
   * version in a separate heap. This is useful when we want to compare 
   * what has changed. We will be in a situation for type-checking 
   * (cf typing/typing_redecl_service.ml) where we want to compare the type
   * of a class in the previous environment vs the current type.
   *)
  val oldify_batch: KeySet.t -> unit
  (* Reverse operation of oldify *)
  val revive_batch: KeySet.t -> unit
end

module type UserKeyType = sig
  type t
  val to_string : t -> string
  val compare : t -> t -> int
end

module NoCache :
  functor (UserKeyType : UserKeyType) ->
  functor (Value:Value.Type) ->
  S with type t = Value.t
    and type key = UserKeyType.t
    and module KeySet = Set.Make (UserKeyType)
    and module KeyMap = MyMap (UserKeyType)

module WithCache :
  functor (UserKeyType : UserKeyType) ->
  functor (Value:Value.Type) ->
  S with type t = Value.t
    and type key = UserKeyType.t
    and module KeySet = Set.Make (UserKeyType)
    and module KeyMap = MyMap (UserKeyType)
