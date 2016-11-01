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

type config = {
  global_size      : int;
  heap_size        : int;
  dep_table_pow    : int;
  hash_table_pow   : int;
  shm_dirs         : string list;
  shm_min_avail    : int;
  log_level        : int;
}

type handle = private {
  h_fd: Unix.file_descr;
  h_global_size: int;
  h_heap_size: int;
}

exception Out_of_shared_memory
exception Hash_table_full
exception Dep_table_full
exception Heap_full
exception C_assertion_failure of string

(*****************************************************************************)
(* Initializes the shared memory. Must be called before forking! *)
(*****************************************************************************)

val init: config -> handle

(*****************************************************************************)
(* Connect a slave to the shared heap *)
(*****************************************************************************)

val connect: handle -> is_master:bool -> unit

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
(* Serializes the dependency table and writes it to a file *)
(*****************************************************************************)
val save_dep_table: string -> int
val save_dep_table_sqlite: string -> int

(*****************************************************************************)
(* Loads the dependency table by reading from a file *)
(*****************************************************************************)
val load_dep_table: string -> int
val load_dep_table_sqlite: string -> int

(*****************************************************************************)
(* The size of the dynamically allocated shared memory section *)
(*****************************************************************************)
val heap_size : unit -> int

(*****************************************************************************)
(* Stats of the statically sized hash / dep tables *)
(*****************************************************************************)

type table_stats = {
  nonempty_slots : int;
  used_slots : int;
  slots : int;
}

val dep_stats : unit -> table_stats

val hash_stats : unit -> table_stats

val is_heap_overflow: unit -> bool

(*****************************************************************************)
(* Cache invalidation. *)
(*****************************************************************************)

val invalidate_caches: unit -> unit

(* Size of value in GC heap *)
val value_size: Obj.t -> int

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

module type NoCache = sig
  type key
  type t
  module KeySet : Set.S with type elt = key
  module KeyMap : MyMap.S with type key = key

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

  (**
   * When a new local change set is pushed, changes will not be reflected in
   * the shared memory until the local changes are popped off.
   **)
  module LocalChanges : sig
    (** Push a new local change environment **)
    val push_stack : unit -> unit
    (** Pop off the last local change environment **)
    val pop_stack : unit -> unit
    (** Reverts any changes associated with the set of keys **)
    val revert_batch : KeySet.t -> unit
    (**
     * Applies the current changes associated with the set of keys to the
     * previous environment. If there are no other active local changes
     * this will perform the actions on shared memory.
     **)
    val commit_batch : KeySet.t -> unit
    val revert_all : unit -> unit
    val commit_all : unit -> unit
  end
end

module type WithCache = sig
  include NoCache
  val write_through : key -> t -> unit
end

module type UserKeyType = sig
  type t
  val to_string : t -> string
  val compare : t -> t -> int
end

module NoCache :
  functor (UserKeyType : UserKeyType) ->
  functor (Value:Value.Type) ->
  NoCache with type t = Value.t
    and type key = UserKeyType.t
    and module KeySet = Set.Make (UserKeyType)
    and module KeyMap = MyMap.Make (UserKeyType)

module WithCache :
  functor (UserKeyType : UserKeyType) ->
  functor (Value:Value.Type) ->
  WithCache with type t = Value.t
    and type key = UserKeyType.t
    and module KeySet = Set.Make (UserKeyType)
    and module KeyMap = MyMap.Make (UserKeyType)

module type CacheType = sig
  type key
  type value

  val add: key -> value -> unit
  val get: key -> value option
  val remove: key -> unit
  val clear: unit -> unit
end

module LocalCache :
  functor (UserKeyType : UserKeyType) ->
  functor (Value : Value.Type) ->
  CacheType with type key = UserKeyType.t
    and type value = Value.t
