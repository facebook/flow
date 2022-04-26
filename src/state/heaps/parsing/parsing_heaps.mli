(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = SharedMem.NewAPI.file SharedMem.addr

type +'a parse_addr = 'a SharedMem.NewAPI.parse SharedMem.addr

type haste_info_addr = SharedMem.NewAPI.haste_info SharedMem.addr

type haste_module_addr = SharedMem.NewAPI.haste_module SharedMem.addr

type file_module_addr = SharedMem.NewAPI.file_module SharedMem.addr

type provider_addr = SharedMem.NewAPI.file SharedMem.NewAPI.entity SharedMem.addr

type resolved_requires = {
  resolved_modules: Modulename.t SMap.t;
  phantom_dependencies: SSet.t;
  hash: Xx.hash;
}
[@@deriving show]

val mk_resolved_requires :
  resolved_modules:Modulename.t SMap.t -> phantom_dependencies:SSet.t -> resolved_requires

val get_file_addr : File_key.t -> file_addr option

val get_file_addr_unsafe : File_key.t -> file_addr

val get_haste_module : string -> haste_module_addr option

val get_haste_module_unsafe : string -> haste_module_addr

val get_file_module_unsafe : File_key.t -> file_module_addr

val read_file_name : file_addr -> string

val read_file_key : file_addr -> File_key.t

val read_file_hash : [> ] parse_addr -> Xx.hash

val read_module_name : haste_info_addr -> string

val read_ast_unsafe : File_key.t -> [ `typed ] parse_addr -> (Loc.t, Loc.t) Flow_ast.Program.t

val read_docblock_unsafe : File_key.t -> [ `typed ] parse_addr -> Docblock.t

val read_aloc_table_unsafe : File_key.t -> [ `typed ] parse_addr -> ALoc.table

val read_type_sig_unsafe : File_key.t -> [ `typed ] parse_addr -> type_sig

val read_tolerable_file_sig_unsafe :
  File_key.t -> [ `typed ] parse_addr -> File_sig.With_Loc.tolerable_t

val read_file_sig_unsafe : File_key.t -> [ `typed ] parse_addr -> File_sig.With_Loc.t

val read_exports : [ `typed ] parse_addr -> Exports.t

module type READER = sig
  type reader

  val get_provider : reader:reader -> Modulename.t -> file_addr option

  val is_typed_file : reader:reader -> file_addr -> bool

  val get_parse : reader:reader -> file_addr -> [ `typed | `untyped ] parse_addr option

  val get_typed_parse : reader:reader -> file_addr -> [ `typed ] parse_addr option

  val get_haste_info : reader:reader -> file_addr -> haste_info_addr option

  val get_haste_name : reader:reader -> file_addr -> string option

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_aloc_table : reader:reader -> File_key.t -> ALoc.table option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_tolerable_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_type_sig : reader:reader -> File_key.t -> type_sig option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_parse_unsafe :
    reader:reader -> File_key.t -> file_addr -> [ `typed | `untyped ] parse_addr

  val get_typed_parse_unsafe : reader:reader -> File_key.t -> file_addr -> [ `typed ] parse_addr

  val get_resolved_requires_unsafe :
    reader:reader -> File_key.t -> [ `typed ] parse_addr -> resolved_requires

  val get_ast_unsafe : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t

  val get_aloc_table_unsafe : reader:reader -> File_key.t -> ALoc.table

  val get_docblock_unsafe : reader:reader -> File_key.t -> Docblock.t

  val get_exports_unsafe : reader:reader -> File_key.t -> Exports.t

  val get_tolerable_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t

  val get_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.t

  val get_type_sig_unsafe : reader:reader -> File_key.t -> type_sig

  val get_file_hash_unsafe : reader:reader -> File_key.t -> Xx.hash

  val loc_of_aloc : reader:reader -> ALoc.t -> Loc.t
end

module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val get_old_parse : reader:reader -> file_addr -> [ `typed | `untyped ] parse_addr option

  val get_old_typed_parse : reader:reader -> file_addr -> [ `typed ] parse_addr option

  val get_old_haste_info : reader:reader -> file_addr -> haste_info_addr option

  val get_old_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_old_exports : reader:reader -> File_key.t -> Exports.t option
end

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

(* For use by a worker process *)
type worker_mutator = {
  add_parsed:
    File_key.t ->
    file_addr option ->
    exports:Exports.t ->
    Xx.hash ->
    string option ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    File_sig.With_Loc.tolerable_t ->
    locs_tbl ->
    type_sig ->
    Modulename.Set.t;
  add_unparsed: File_key.t -> file_addr option -> Xx.hash -> string option -> Modulename.Set.t;
  clear_not_found: File_key.t -> Modulename.Set.t;
}

module Parse_mutator : sig
  val create : unit -> worker_mutator
end

module Reparse_mutator : sig
  type master_mutator (* Used by the master process *)

  val create : Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator

  val record_unchanged : master_mutator -> Utils_js.FilenameSet.t -> unit

  val record_not_found : master_mutator -> Utils_js.FilenameSet.t -> unit
end

module Commit_modules_mutator : sig
  type t

  val create : Transaction.t -> t

  val record_no_providers : t -> Modulename.Set.t -> unit
end

module Resolved_requires_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_resolved_requires : t -> file_addr -> [ `typed ] parse_addr -> resolved_requires -> bool
end

module From_saved_state : sig
  val add_parsed :
    File_key.t -> Xx.hash -> string option -> Exports.t -> resolved_requires -> Modulename.Set.t

  val add_unparsed : File_key.t -> Xx.hash -> string option -> Modulename.Set.t
end

val iter_resolved_requires : (file_addr -> resolved_requires -> unit) -> unit
