(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
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

val commit_transaction : unit -> unit

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

  val oldify : key -> unit

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

module NoCacheAddr (Key : Key) (Value : AddrValue) : sig
  include
    NoCache
      with type key = Key.t
       and type value = Value.t addr
       and module KeySet = Flow_set.Make(Key)

  val add : Key.t -> Value.t addr -> Value.t addr
end

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

  (* Phantom type tag for int64 data, like hashes *)
  type heap_int64

  (* Phantom type tag for entities, which can store both a "committed" and
   * "latest" value. *)
  type 'a entity

  (* Phantom type tag for addr map objects, which are arrays of addresses to
   * another kind of object. For example, a `heap_string addr_tbl addr` is an
   * array of addresses to string objects. *)
  type 'a addr_tbl

  (* Phantom type tag for optional objects. *)
  type 'a opt

  (* Phantom type tag for ASTs. *)
  type ast

  (* Phantom type tag for docblock, which contains information contained in the
   * leading comment of a file. *)
  type docblock

  (* Phantom type tag for aloc table. An aloc table provides the concrete
   * location for a given keyed location in a signature. *)
  type aloc_table

  (* Phantom type tag for type sig, which contains a serialized representation
   * of the visible exports of a file. See Type_sig_bin *)
  type type_sig

  (* Phantom type tag for file sig, which contains a serialized representation
   * of the imports/requires of a file. *)
  type file_sig

  type exports

  type unparse

  type parse

  type file

  (* Before writing to the heap, we first calculate the required size (in words)
   * for all the heap objects we would like to write. We will pass this size
   * into the `alloc` function below, to get a chunk which we use to perform
   * writes. *)
  type size = int

  (* Allocate the requested space (in words) in the heap. All writes must be
   * done within the provided callback, and the writes must fully consume all
   * allocated space. *)
  val alloc : size -> (chunk -> 'a) -> 'a

  (* headers *)

  val header_size : size

  val with_header_size : ('a -> size) -> 'a -> size

  (* strings *)

  val string_size : string -> size

  val write_string : chunk -> string -> heap_string addr

  val read_string : heap_string addr -> string

  (* hash *)

  val int64_size : size

  val write_int64 : chunk -> int64 -> heap_int64 addr

  val read_int64 : heap_int64 addr -> int64

  (* addr tbl *)

  val addr_tbl_size : 'a array -> size

  val write_addr_tbl : (chunk -> 'a -> 'k addr) -> chunk -> 'a array -> 'k addr_tbl addr

  val read_addr_tbl_generic :
    ('k addr -> 'a) -> 'k addr_tbl addr -> (int -> (int -> 'a) -> 'b) -> 'b

  val read_addr_tbl : ('k addr -> 'a) -> 'k addr_tbl addr -> 'a array

  (* opt *)

  val opt_size : ('a -> size) -> 'a option -> size

  val read_opt : ('a addr -> 'b) -> 'a opt addr -> 'b option

  val read_opt_exn : ('a addr -> 'b) -> 'a opt addr -> 'b

  val read_opt_bind : ('a addr -> 'b option) -> 'a opt addr -> 'b option

  val is_none : 'a opt addr -> bool

  val is_some : 'a opt addr -> bool

  (* entities *)

  val entity_size : int

  val write_entity : chunk -> 'k addr option -> 'k entity addr

  val entity_advance : 'k entity addr -> 'k addr option -> unit

  val entity_read_committed : 'k entity addr -> 'k opt addr

  val entity_read_latest : 'k entity addr -> 'k opt addr

  val entity_rollback : _ entity addr -> unit

  (* ast *)

  val prepare_write_ast : string -> size * (chunk -> ast addr)

  val read_ast : ast addr -> string

  (* file sig *)

  val prepare_write_file_sig : string -> size * (chunk -> file_sig addr)

  val read_file_sig : file_sig addr -> string

  (* exports *)

  val prepare_write_exports : string -> size * (chunk -> exports addr)

  val read_exports : exports addr -> string

  (* docblock *)

  val docblock_size : string -> size

  val write_docblock : chunk -> string -> docblock addr

  val read_docblock : docblock addr -> string

  (* aloc table *)

  val aloc_table_size : string -> size

  val write_aloc_table : chunk -> string -> aloc_table addr

  val read_aloc_table : aloc_table addr -> string

  (* type sig *)

  val type_sig_size : int -> size

  val write_type_sig : chunk -> int -> (buf -> unit) -> type_sig addr

  val read_type_sig : type_sig addr -> (buf -> 'a) -> 'a

  val type_sig_buf : type_sig addr -> buf

  (* unparse data *)

  val unparse_size : size

  val write_unparse : chunk -> heap_int64 addr -> heap_string addr option -> unparse addr

  val get_file_hash : unparse addr -> heap_int64 addr

  val get_module_name : unparse addr -> heap_string opt addr

  (* parse data *)

  val parse_size : size

  val write_parse : chunk -> exports addr -> parse addr

  val set_ast : parse addr -> ast addr -> unit

  val set_docblock : parse addr -> docblock addr -> unit

  val set_aloc_table : parse addr -> aloc_table addr -> unit

  val set_type_sig : parse addr -> type_sig addr -> unit

  val set_file_sig : parse addr -> file_sig addr -> unit

  val get_ast : parse addr -> ast opt addr

  val get_docblock : parse addr -> docblock opt addr

  val get_aloc_table : parse addr -> aloc_table opt addr

  val get_type_sig : parse addr -> type_sig opt addr

  val get_file_sig : parse addr -> file_sig opt addr

  val get_exports : parse addr -> exports addr

  (* checked file *)

  type file_kind =
    | Source_file
    | Json_file
    | Resource_file
    | Lib_file

  val file_size : size

  val write_file :
    chunk -> file_kind -> heap_string addr -> unparse entity addr -> parse entity addr -> file addr

  val get_file_kind : file addr -> file_kind

  val get_file_name : file addr -> heap_string addr

  val get_unparse : file addr -> unparse entity addr

  val get_parse : file addr -> parse entity addr
end
