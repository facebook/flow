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
type +'k addr [@@immediate]

val null_addr : [ `null ] addr

exception Out_of_shared_memory

exception Hash_table_full

exception Heap_full

exception Invalid_address of int * int64

exception Invalid_header of int * int64

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

val is_init_transaction : unit -> bool

module type Key = sig
  type t

  val to_string : t -> string

  val compare : t -> t -> int
end

module type Value = sig
  type t

  val description : string
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

  val remove : key -> unit

  val remove_batch : KeySet.t -> unit

  val mem : key -> bool
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

  type +'k parse_kind

  type 'k parse = [ `parse of 'k parse_kind ]

  type 'k entity = [ `entity of 'k ]

  type 'k sklist = [ `sklist of 'k ]

  type 'k sknode = [ `sknode of 'k ]

  type 'k tbl = [ `tbl of 'k ]

  type dependency =
    [ `haste_module
    | `file
    ]

  type resolved_module =
    [ `haste_module
    | `file
    | `string
    | `null
    ]

  type entity_reader = { read: 'a. 'a entity addr -> 'a addr option } [@@unboxed]

  (* Before writing to the heap, we first calculate the required size (in words)
   * for all the heap objects we would like to write. We will pass this size
   * into the `alloc` function below, to get a chunk which we use to perform
   * writes. *)
  type size = int

  type 'a prep = size * (chunk -> 'a)

  (* Allocate the requested space (in words) in the heap. All writes must be
   * done within the provided callback, and the writes must fully consume all
   * allocated space. *)
  val alloc : 'a prep -> 'a

  (* prepare *)

  val prepare_map : 'a prep -> ('a -> 'b) -> 'b prep

  val prepare_product : 'a prep -> 'b prep -> ('a * 'b) prep

  val prepare_const : 'a -> 'a prep

  val prepare_opt : ('a -> 'b prep) -> 'a option -> 'b option prep

  val prepare_iter : ('a -> unit prep) -> 'a array -> unit prep

  module Prepare_syntax : sig
    val ( let+ ) : 'a prep -> ('a -> 'b) -> 'b prep

    val ( and+ ) : 'a prep -> 'b prep -> ('a * 'b) prep
  end

  (* strings *)

  val prepare_write_string : string -> [ `string ] addr prep

  val read_string : [ `string ] addr -> string

  val compare_string : [ `string ] addr -> [ `string ] addr -> int

  (* hash *)

  val prepare_write_int64 : int64 -> [ `int64 ] addr prep

  val read_int64 : [ `int64 ] addr -> int64

  (* addr tbl *)

  val prepare_write_addr_tbl : 'k addr prep array -> 'k tbl addr prep

  val read_addr_tbl_generic : ('k addr -> 'a) -> 'k tbl addr -> (int -> (int -> 'a) -> 'b) -> 'b

  val read_addr_tbl : ('k addr -> 'a) -> 'k tbl addr -> 'a array

  (* skip lists *)

  val prepare_write_sklist : 'a sklist addr prep

  val prepare_write_sknode : unit -> ('a addr -> 'a sknode addr) prep

  val sklist_iter : ('a addr -> unit) -> 'a sklist addr -> unit

  val sklist_is_empty : 'a sklist addr -> bool

  (* entities *)

  val prepare_write_entity : ('k addr option -> 'k entity addr) prep

  val entity_advance : 'k entity addr -> 'k addr option -> unit

  val entity_read_committed : 'k entity addr -> 'k addr option

  val entity_read_latest : 'k entity addr -> 'k addr option

  val entity_rollback : _ entity addr -> unit

  val entity_changed : _ entity addr -> bool

  val entity_reader_committed : entity_reader

  val entity_reader_latest : entity_reader

  (* ast *)

  val prepare_write_serialized_ast : string -> [ `ast ] addr prep

  val read_ast : [ `ast ] addr -> string

  (* file sig *)

  val prepare_write_serialized_file_sig : string -> [ `file_sig ] addr prep

  val read_file_sig : [ `file_sig ] addr -> string

  (* exports *)

  val prepare_write_serialized_exports : string -> [ `exports ] addr prep

  val read_exports : [ `exports ] addr -> string

  (* requires *)

  val prepare_write_requires : string array -> size * (chunk -> [ `requires ] addr)

  val read_requires : [ `requires ] addr -> string array

  (* resolved requires *)

  val resolved_requires_size : size

  val prepare_write_resolved_requires :
    (resolved_module tbl addr -> dependency tbl addr -> [ `resolved_requires ] addr) prep

  val get_resolved_modules : [ `resolved_requires ] addr -> resolved_module tbl addr

  val get_phantom_dependencies : [ `resolved_requires ] addr -> dependency tbl addr

  val read_dependency :
    ([ `haste_module ] addr -> 'a) -> ([ `file ] addr -> 'a) -> dependency addr -> 'a

  val read_resolved_module :
    resolved_module addr -> (dependency addr, [ `string ] addr option) Result.t

  (* imports *)

  val prepare_write_serialized_imports : string -> [ `imports ] addr prep

  val read_imports : [ `imports ] addr -> string

  (* package info *)

  val prepare_write_package_info : string -> [ `package_info ] addr prep

  val read_package_info : [ `package_info ] addr -> string

  (* cas_digest *)

  val prepare_write_cas_digest : Cas_digest.t -> [ `cas_digest ] addr prep

  val read_cas_digest : [ `cas_digest ] addr -> Cas_digest.t

  (* docblock *)

  val prepare_write_docblock : string -> [ `docblock ] addr prep

  val read_docblock : [ `docblock ] addr -> string

  (* aloc table *)

  val prepare_write_aloc_table : string -> [ `aloc_table ] addr prep

  val read_aloc_table : [ `aloc_table ] addr -> string

  (* type sig *)

  val prepare_write_type_sig : int -> (buf -> unit) -> [ `type_sig ] addr prep

  val read_type_sig : [ `type_sig ] addr -> (buf -> 'a) -> 'a

  val type_sig_buf : [ `type_sig ] addr -> buf

  (* parse data *)

  val prepare_write_untyped_parse : ([ `int64 ] addr -> [ `untyped ] parse addr) prep

  val prepare_write_typed_parse :
    ([ `int64 ] addr ->
    [ `exports ] addr ->
    [ `requires ] addr ->
    [ `resolved_requires ] entity addr ->
    [ `imports ] addr ->
    [ `file ] entity addr ->
    [ `int64 ] entity addr ->
    [ `cas_digest ] addr option ->
    [ `typed ] parse addr
    )
    prep

  val prepare_write_package_parse :
    ([ `int64 ] addr -> [ `package_info ] addr -> [ `package ] parse addr) prep

  val is_typed : [> ] parse addr -> bool

  val is_package : [> ] parse addr -> bool

  val coerce_typed : [> ] parse addr -> [ `typed ] parse addr option

  val coerce_package : [> ] parse addr -> [ `package ] parse addr option

  val get_file_hash : [> ] parse addr -> [ `int64 ] addr

  val get_ast : [ `typed ] parse addr -> [ `ast ] addr option

  val get_docblock : [ `typed ] parse addr -> [ `docblock ] addr option

  val get_aloc_table : [ `typed ] parse addr -> [ `aloc_table ] addr option

  val get_type_sig : [ `typed ] parse addr -> [ `type_sig ] addr option

  val get_file_sig : [ `typed ] parse addr -> [ `file_sig ] addr option

  val get_exports : [ `typed ] parse addr -> [ `exports ] addr

  val get_requires : [ `typed ] parse addr -> [ `requires ] addr

  val get_resolved_requires : [ `typed ] parse addr -> [ `resolved_requires ] entity addr

  val get_imports : [ `typed ] parse addr -> [ `imports ] addr

  val get_leader : [ `typed ] parse addr -> [ `file ] entity addr

  val get_sig_hash : [ `typed ] parse addr -> [ `int64 ] entity addr

  val get_cas_digest : [ `typed ] parse addr -> [ `cas_digest ] addr option

  val get_package_info : [ `package ] parse addr -> [ `package_info ] addr

  val set_ast : [ `typed ] parse addr -> [ `ast ] addr -> unit

  val set_docblock : [ `typed ] parse addr -> [ `docblock ] addr -> unit

  val set_aloc_table : [ `typed ] parse addr -> [ `aloc_table ] addr -> unit

  val set_type_sig : [ `typed ] parse addr -> [ `type_sig ] addr -> unit

  val set_file_sig : [ `typed ] parse addr -> [ `file_sig ] addr -> unit

  val set_cas_digest : [ `typed ] parse addr -> [ `cas_digest ] addr -> unit

  (* haste info *)

  val prepare_write_haste_info : ([ `haste_module ] addr -> [ `haste_info ] addr) prep

  val get_haste_module : [ `haste_info ] addr -> [ `haste_module ] addr

  val haste_info_equal : [ `haste_info ] addr -> [ `haste_info ] addr -> bool

  (* file data *)

  type file_kind =
    | Source_file
    | Json_file
    | Resource_file
    | Lib_file

  val prepare_write_file :
    file_kind ->
    ([ `string ] addr ->
    [ `typed | `untyped | `package ] parse entity addr ->
    [ `haste_info ] entity addr ->
    [ `file ] sklist addr option ->
    [ `file ] addr
    )
    prep

  val get_file_kind : [ `file ] addr -> file_kind

  val get_file_name : [ `file ] addr -> [ `string ] addr

  val get_file_dependents : [ `file ] addr -> [ `file ] sklist addr option

  val get_haste_info : [ `file ] addr -> [ `haste_info ] entity addr

  val get_parse : [ `file ] addr -> [ `typed | `untyped | `package ] parse entity addr

  val files_equal : [ `file ] addr -> [ `file ] addr -> bool

  val file_changed : [ `file ] addr -> bool

  (* haste module *)

  val prepare_write_haste_module :
    ([ `string ] addr -> [ `file ] entity addr -> [ `file ] sklist addr -> [ `haste_module ] addr)
    prep

  val haste_modules_equal : [ `haste_module ] addr -> [ `haste_module ] addr -> bool

  val get_haste_name : [ `haste_module ] addr -> [ `string ] addr

  val get_haste_provider : [ `haste_module ] addr -> [ `file ] entity addr

  val get_haste_dependents : [ `haste_module ] addr -> [ `file ] sklist addr

  val add_haste_provider : [ `haste_module ] addr -> [ `file ] addr -> [ `haste_info ] addr -> unit

  val get_haste_all_providers_exclusive : [ `haste_module ] addr -> [ `file ] addr list

  val remove_haste_provider_exclusive : [ `haste_module ] addr -> [ `file ] addr -> unit

  (* file sets *)

  val file_set_add : [ `file ] sklist addr -> [ `file ] sknode addr -> bool

  val file_set_remove : [ `file ] sklist addr -> [ `file ] addr -> bool

  val file_set_mem : [ `file ] sklist addr -> [ `file ] addr -> bool
end
