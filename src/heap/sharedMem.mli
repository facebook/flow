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

(* Addresses are represented as integers, but are well-typed via a phantom
 * type parameter. The type checker will ensure that a `foo addr` is not
 * passed where a `bar addr` is expected. *)
type 'k addr [@@immediate]

type effort =
  [ `aggressive
  | `always_TEST
  ]

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

val init : config -> num_workers:int -> handle

val init_done : unit -> unit

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

  module KeySet : Set.S with type elt = key

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

module type DebugCacheType = sig
  val get_size : unit -> int
end

module type DebugLocalCache = sig
  module DebugL1 : DebugCacheType

  module DebugL2 : DebugCacheType
end

module type WithCache = sig
  include NoCache

  val write_around : key -> value -> unit

  val get_no_cache : key -> value option

  module DebugCache : DebugLocalCache
end

module WithCache (Key : Key) (Value : Value) :
  WithCache with type key = Key.t and type value = Value.t and module KeySet = Set.Make(Key)

module NoCache (Key : Key) (Value : Value) :
  NoCache with type key = Key.t and type value = Value.t and module KeySet = Set.Make(Key)

module NoCacheAddr (Key : Key) (Value : AddrValue) :
  NoCache with type key = Key.t and type value = Value.t addr and module KeySet = Set.Make(Key)

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

  (* Phantom type tag for checked file objects. A checked file contains
   * references to the filename, any local definitions, exports, etc. *)
  type checked_file

  (* Phantom type tag for exports objects. Exports describe the externally
   * visible interface to a checked file. *)
  type exports

  (* Phantom type tag for export definitions. Export definitions are the
   * CommonJS export value of a CJS module, or the default export of an ES
   * module. *)
  type export_def

  (* Phantom type tag for module references. A module reference is the string
   * argument to a require() call or import statement. This is needed to resolve
   * dependencies. *)
  type module_ref

  (* Phantom type tag for remote references. A remote reference is a local
   * binding that corresponds to an imported value. For example, the statement
   * `import {Foo} from 'bar'` will be represented by a remote ref object. *)
  type remote_ref

  (* Phantom type tag for local definitions. A local definition is a binding
   * that corresponds to a definition from a given file. For example, variable,
   * function, and class declarations are represented by local def objects. *)
  type local_def

  (* Phantom type tag for pattern definitions. A pattern definition is the
   * right-hand side of a destructuring definition. For example, the `e` in a
   * variable declaration of the form `var {a, b} = e` would be represented by a
   * pattern def object.
   *
   * Pattern defs are represented as separate heap objects because they might
   * have multiple references -- in the above example, from both `a` and `b` --
   * and should not be duplicated in the heap. *)
  type pattern_def

  (* Phantom type tag for pattern bindings. A pattern binding is the left-hand
   * side of a destructuring definition. For example, the `a` and `b` bindings
   * introduced by `var {a, b} = e` would be represented by pattern objects. *)
  type pattern

  (* Before writing to the heap, we first calculate the required size (in words)
   * for all the heap objects we would like to write. We will pass this size
   * into the `alloc` function below, to get a chunk which we use to perform
   * writes. *)
  type size = int

  val header_size : size

  val string_size : string -> size

  val addr_tbl_size : 'a array -> size

  val checked_file_size : size

  val module_ref_size : string -> size

  val remote_ref_size : string -> size

  val exports_size : string -> size

  val export_def_size : string -> size

  val local_def_size : string -> size

  val pattern_def_size : string -> size

  val pattern_size : string -> size

  (* Allocate the requested space (in words) in the heap. All writes must be
   * done within the provided callback, and the writes must fully consume all
   * allocated space. *)
  val alloc : size -> (chunk -> 'a) -> 'a

  (* write *)

  val write_string : chunk -> string -> heap_string addr

  val write_addr_tbl : (chunk -> 'a -> 'k addr) -> chunk -> 'a array -> 'k addr_tbl addr

  val write_opt : (chunk -> 'a -> 'k addr) -> chunk -> 'a option -> 'k opt addr

  val write_checked_file :
    chunk ->
    heap_string addr ->
    exports addr ->
    export_def opt addr ->
    module_ref addr_tbl addr ->
    local_def addr_tbl addr ->
    remote_ref addr_tbl addr ->
    pattern_def addr_tbl addr ->
    pattern addr_tbl addr ->
    checked_file addr

  val write_exports : chunk -> string -> exports addr

  val write_export_def : chunk -> string -> export_def addr

  val write_module_ref : chunk -> string -> module_ref addr

  val write_local_def : chunk -> string -> local_def addr

  val write_remote_ref : chunk -> string -> remote_ref addr

  val write_pattern_def : chunk -> string -> pattern_def addr

  val write_pattern : chunk -> string -> pattern addr

  (* getters *)

  val file_name : checked_file addr -> heap_string addr

  val file_exports : checked_file addr -> exports addr

  val file_export_def : checked_file addr -> export_def opt addr

  val file_module_refs : checked_file addr -> module_ref addr_tbl addr

  val file_local_defs : checked_file addr -> local_def addr_tbl addr

  val file_remote_refs : checked_file addr -> remote_ref addr_tbl addr

  val file_pattern_defs : checked_file addr -> pattern_def addr_tbl addr

  val file_patterns : checked_file addr -> pattern addr_tbl addr

  (* read *)

  val read_string : heap_string addr -> string

  val read_addr_tbl_generic :
    ('k addr -> 'a) -> 'k addr_tbl addr -> (int -> (int -> 'a) -> 'b) -> 'b

  val read_addr_tbl : ('k addr -> 'a) -> 'k addr_tbl addr -> 'a array

  val read_opt : ('a addr -> 'b) -> 'a opt addr -> 'b option

  val read_exports : exports addr -> string

  val read_module_ref : module_ref addr -> string

  val read_remote_ref : remote_ref addr -> string

  val read_local_def : local_def addr -> string

  val read_export_def : export_def addr -> string

  val read_pattern_def : pattern_def addr -> string

  val read_pattern : pattern addr -> string
end
