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

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(* Addresses are represented as integers, but are well-typed via a phantom
 * type parameter. The type checker will ensure that a `foo addr` is not
 * passed where a `bar addr` is expected. *)
type 'k addr [@@immediate]

type effort =
  [ `aggressive
  | `always_TEST
  ]

type serialized_tag = Serialized_resolved_requires

exception Out_of_shared_memory

exception Hash_table_full

exception Heap_full

val connect : handle -> worker_id:int -> unit

val on_compact : (unit -> unit -> unit) ref

val collect_slice : ?force:bool -> int -> bool

val collect_full : unit -> unit

val compact : unit -> unit

type table_stats = {
  nonempty_slots: int;
  used_slots: int;
  slots: int;
}

val hash_stats : unit -> table_stats

val heap_size : unit -> int

val init : config -> num_workers:int -> (handle, unit) result

module type Key = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int
end

module type Value = sig
  type t

  val description : string
end

module type SerializedTag = sig
  val value : serialized_tag
end

module type AddrValue = sig
  type t
end

module type NoCache = sig
  type key

  type value

  module KeySet : Flow_set.S with type elt = key

  val add : key -> value -> unit

  val get : key -> value option

  val get_old : key -> value option

  val remove_old_batch : KeySet.t -> unit

  val remove_batch : KeySet.t -> unit

  val mem : key -> bool

  val mem_old : key -> bool

  val oldify_batch : KeySet.t -> unit

  val revive_batch : KeySet.t -> unit
end

module type NoCacheTag = sig
  include NoCache

  val iter : (value -> unit) -> unit
end

module type DebugCacheType = sig
  val get_size : unit -> int
end

module type LocalCache = sig
  type key

  type value

  module DebugL1 : DebugCacheType

  module DebugL2 : DebugCacheType

  val add : key -> value -> unit

  val get : key -> value option

  val remove : key -> unit

  val clear : unit -> unit
end

module type CacheConfig = sig
  type key

  type value

  val capacity : int
end

module type WithCache = sig
  include NoCache

  val write_around : key -> value -> unit

  val get_no_cache : key -> value option

  module DebugCache : LocalCache with type key = key and type value = value
end

module LocalCache (Config : CacheConfig) :
  LocalCache with type key = Config.key and type value = Config.value

module WithCache (Key : Key) (Value : Value) :
  WithCache with type key = Key.t and type value = Value.t and module KeySet = Flow_set.Make(Key)

module NoCache (Key : Key) (Value : Value) :
  NoCache with type key = Key.t and type value = Value.t and module KeySet = Flow_set.Make(Key)

module NoCacheTag (Key : Key) (Value : Value) (_ : SerializedTag) :
  NoCacheTag with type key = Key.t and type value = Value.t and module KeySet = Flow_set.Make(Key)

module NoCacheAddr (Key : Key) (Value : AddrValue) :
  NoCache with type key = Key.t and type value = Value.t addr and module KeySet = Flow_set.Make(Key)

val debug_value_size : Obj.t -> int

module NewAPI : sig
  (* This module provides a type-safe and bounds-checked interface to shared
   * memory and specifies the exact layout of the objects which we want to
   * store.
   *
   * Heap objects are all prefixed with a header word which includes a tag and
   * the size of the object in words. The following words, depending on the tag,
   * include 0 or more fixed size fields and optionally one variable size field.
   *
   * For example, the "checked_file" object has several words containing addrs
   * pointing to other objects (things like exports, definitions exported from
   * the file, etc.). The "local def" object, which represents a definition
   * local to a checked file has a variable size string field which contains the
   * serialized signature of the definition. *)

  (* A chunk is an append-only cursor into the heap. To use a chunk, first
   * allocate the needed space, then call the appropriate write_* functions to
   * fill the chunk with data.
   *
   * The chunk API ensures that callers (1) do not write beyond the allocated
   * space and (2) consume all of the allocated space. *)
  type chunk

  (* Phantom type tag for string objects. *)
  type heap_string

  (* Phantom type tag for addr map objects, which are arrays of addresses to
   * another kind of object. For example, a `heap_string addr_tbl addr` is an
   * array of addresses to string objects. *)
  type 'a addr_tbl

  (* Phantom type tag for optional objects. *)
  type 'a opt

  (* Phantom type tag for docblock, which contains information contained in the
   * leading comment of a file. *)
  type docblock

  (* Phantom type tag for aloc table. An aloc table provides the concrete
   * location for a given keyed location in a signature. *)
  type aloc_table

  (* Phantom type tag for type sig, which contains a serialized representation
   * of the visible exports of a file. See Type_sig_bin *)
  type type_sig

  (* Phantom type tag for checked file objects. A checked file contains
   * references to the filename, any local definitions, exports, etc. *)
  type checked_file

  (* Before writing to the heap, we first calculate the required size (in words)
   * for all the heap objects we would like to write. We will pass this size
   * into the `alloc` function below, to get a chunk which we use to perform
   * writes. *)
  type size = int

  val header_size : size

  val string_size : string -> size

  val addr_tbl_size : 'a array -> size

  val docblock_size : string -> size

  val aloc_table_size : string -> size

  val type_sig_size : int -> size

  val checked_file_size : size

  (* Allocate the requested space (in words) in the heap. All writes must be
   * done within the provided callback, and the writes must fully consume all
   * allocated space. *)
  val alloc : size -> (chunk -> 'a) -> 'a

  (* write *)

  val write_string : chunk -> string -> heap_string addr

  val write_addr_tbl : (chunk -> 'a -> 'k addr) -> chunk -> 'a array -> 'k addr_tbl addr

  val write_opt : (chunk -> 'a -> 'k addr) -> chunk -> 'a option -> 'k opt addr

  val write_docblock : chunk -> string -> docblock addr

  val write_aloc_table : chunk -> string -> aloc_table addr

  val write_type_sig : chunk -> int -> (buf -> unit) -> type_sig addr

  val write_checked_file :
    chunk -> docblock addr -> aloc_table addr -> type_sig addr -> checked_file addr

  (* getters *)

  val file_docblock : checked_file addr -> docblock addr

  val file_aloc_table : checked_file addr -> aloc_table addr

  val file_type_sig : checked_file addr -> type_sig addr

  (* read *)

  val read_string : heap_string addr -> string

  val read_addr_tbl_generic :
    ('k addr -> 'a) -> 'k addr_tbl addr -> (int -> (int -> 'a) -> 'b) -> 'b

  val read_addr_tbl : ('k addr -> 'a) -> 'k addr_tbl addr -> 'a array

  val read_opt : ('a addr -> 'b) -> 'a opt addr -> 'b option

  val read_docblock : docblock addr -> string

  val read_aloc_table : aloc_table addr -> string

  val read_type_sig : type_sig addr -> (buf -> 'a) -> 'a

  (* buf *)

  val type_sig_buf : type_sig addr -> buf
end
