(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type config = {
  global_size: int;
  heap_size: int;
  dep_table_pow: int;
  hash_table_pow: int;
  shm_dirs: string list;
  shm_min_avail: int;
  log_level: int;
}

type handle = private {
  h_fd: Unix.file_descr;
  h_global_size: int;
  h_heap_size: int;
}

exception Out_of_shared_memory

exception Hash_table_full

(* TODO - Hack only? *)
exception Dep_table_full

exception Heap_full

(* TODO - Hack only? *)
exception Sql_assertion_failure of int

val connect : handle -> worker_id:int -> unit

(* TODO - hack only? *)
val allow_removes : bool -> unit

(* TODO - can we hide after inlining SharedMem_js? *)
val should_collect : [ `aggressive | `always_TEST | `gentle ] -> bool

val collect : [ `aggressive | `always_TEST | `gentle ] -> unit

type table_stats = {
  nonempty_slots: int;
  used_slots: int;
  slots: int;
}

val hash_stats : unit -> table_stats

val heap_size : unit -> int

val init : config -> num_workers:int -> handle

val init_done : unit -> unit

module type Key = sig
  type userkey

  type t

  type old

  type md5

  val make : Prefix.t -> userkey -> t

  val make_old : Prefix.t -> userkey -> old

  val to_old : t -> old

  val new_from_old : old -> t

  val md5 : t -> md5

  val md5_old : old -> md5

  val string_of_md5 : md5 -> string
end

module Immediate (Key : Key) (Value : Value.Type) : sig
  val add : Key.md5 -> Value.t -> unit

  val mem : Key.md5 -> bool

  val get : Key.md5 -> Value.t

  val remove : Key.md5 -> unit

  val move : Key.md5 -> Key.md5 -> unit
end

module type NoCache = sig
  type key

  type t

  module KeySet : Set.S with type elt = key

  module KeyMap : WrappedMap.S with type key = key

  val add : key -> t -> unit

  val get : key -> t option

  val get_old : key -> t option

  val get_old_batch : KeySet.t -> t option KeyMap.t

  val remove_old_batch : KeySet.t -> unit

  val find_unsafe : key -> t

  val get_batch : KeySet.t -> t option KeyMap.t

  val remove_batch : KeySet.t -> unit

  val string_of_key : key -> string

  val mem : key -> bool

  val mem_old : key -> bool

  val oldify_batch : KeySet.t -> unit

  val revive_batch : KeySet.t -> unit

  module LocalChanges : sig
    val has_local_changes : unit -> bool

    val push_stack : unit -> unit

    val pop_stack : unit -> unit

    val revert_batch : KeySet.t -> unit

    val commit_batch : KeySet.t -> unit

    val revert_all : unit -> unit

    val commit_all : unit -> unit
  end
end

module type DebugCacheType = sig
  val get_size : unit -> int
end

module type DebugLocalCache = sig
  module DebugL1 : DebugCacheType

  module DebugL2 : DebugCacheType
end

module type WithCache = sig
  include NoCache

  val write_around : key -> t -> unit

  val get_no_cache : key -> t option

  module DebugCache : DebugLocalCache
end

module type Raw = functor (Key : Key) (Value : Value.Type) -> sig
  val add : Key.md5 -> Value.t -> unit

  val mem : Key.md5 -> bool

  val get : Key.md5 -> Value.t

  val remove : Key.md5 -> unit

  val move : Key.md5 -> Key.md5 -> unit
end

module type UserKeyType = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int
end

module WithCache (Raw : Raw) (UserKeyType : UserKeyType) (Value : Value.Type) : sig
  include
    WithCache
      with type key = UserKeyType.t
       and type t = Value.t
       and module KeySet = Set.Make(UserKeyType)
       and module KeyMap = WrappedMap.Make(UserKeyType)
end

module NoCache (Raw : Raw) (UserKeyType : UserKeyType) (Value : Value.Type) : sig
  include
    NoCache
      with type key = UserKeyType.t
       and type t = Value.t
       and module KeySet = Set.Make(UserKeyType)
       and module KeyMap = WrappedMap.Make(UserKeyType)
end

val debug_value_size : Obj.t -> int

val debug_removed_count : unit -> int
