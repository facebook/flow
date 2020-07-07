(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type config = {
  heap_size: int;
  hash_table_pow: int;
  log_level: int;
}

type handle = Unix.file_descr

exception Out_of_shared_memory

exception Hash_table_full

exception Heap_full

val connect : handle -> worker_id:int -> unit

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

module type Value = sig
  type t

  val prefix : Prefix.t

  val description : string
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

module type UserKeyType = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int
end

module WithCache (UserKeyType : UserKeyType) (Value : Value) : sig
  include
    WithCache
      with type key = UserKeyType.t
       and type t = Value.t
       and module KeySet = Set.Make(UserKeyType)
       and module KeyMap = WrappedMap.Make(UserKeyType)
end

module NoCache (UserKeyType : UserKeyType) (Value : Value) : sig
  include
    NoCache
      with type key = UserKeyType.t
       and type t = Value.t
       and module KeySet = Set.Make(UserKeyType)
       and module KeyMap = WrappedMap.Make(UserKeyType)
end

val debug_value_size : Obj.t -> int

val debug_removed_count : unit -> int
