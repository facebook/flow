(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = [ `file ] SharedMem.addr

type +'a parse_addr = 'a SharedMem.NewAPI.parse SharedMem.addr

type haste_info_addr = [ `haste_info ] SharedMem.addr

type haste_module_addr = [ `haste_module ] SharedMem.addr

type provider_addr = [ `file ] SharedMem.NewAPI.entity SharedMem.addr

type resolved_requires_addr = [ `resolved_requires ] SharedMem.addr

type dependency_addr = SharedMem.NewAPI.dependency SharedMem.addr

type resolved_module_addr = SharedMem.NewAPI.resolved_module SharedMem.addr

type 'a resolved_module' = ('a, Flow_import_specifier.t option) result [@@deriving show]

type resolved_module = Modulename.t resolved_module' [@@deriving show]

type component_file = File_key.t * file_addr * [ `typed ] parse_addr

val get_file_addr : File_key.t -> file_addr option

val get_file_addr_unsafe : File_key.t -> file_addr

val get_haste_module : Haste_module_info.t -> haste_module_addr option

val get_haste_module_unsafe : Haste_module_info.t -> haste_module_addr

val get_dependency : Modulename.t -> dependency_addr option

val get_dependency_unsafe : Modulename.t -> dependency_addr

val iter_dependents : (file_addr -> unit) -> Modulename.t -> unit

val read_file_name : file_addr -> string

val read_file_key : file_addr -> File_key.t

val read_file_hash : [> ] parse_addr -> Xx.hash

val read_haste_module_info : haste_info_addr -> Haste_module_info.t

val read_dependency_name : dependency_addr -> string

val read_ast_unsafe : File_key.t -> [ `typed ] parse_addr -> (Loc.t, Loc.t) Flow_ast.Program.t

val read_docblock_unsafe : File_key.t -> [ `typed ] parse_addr -> Docblock.t

val read_aloc_table_unsafe : File_key.t -> [ `typed ] parse_addr -> ALoc.table

val read_type_sig_unsafe : File_key.t -> [ `typed ] parse_addr -> type_sig

val read_tolerable_file_sig_unsafe : File_key.t -> [ `typed ] parse_addr -> File_sig.tolerable_t

val read_file_sig_unsafe : File_key.t -> [ `typed ] parse_addr -> File_sig.t

val read_requires : [ `typed ] parse_addr -> Flow_import_specifier.t array

val read_exports : [ `typed ] parse_addr -> Exports.t

val read_imports : [ `typed ] parse_addr -> Imports.t

val read_package_info : [ `package ] parse_addr -> (Package_json.t, unit) result

val read_dependency : dependency_addr -> Modulename.t

val read_resolved_module : (dependency_addr -> 'a) -> resolved_module_addr -> 'a resolved_module'

val read_resolved_modules : (resolved_module_addr -> 'a) -> resolved_requires_addr -> 'a array

val read_phantom_dependencies : (dependency_addr -> 'a) -> resolved_requires_addr -> 'a array

module type READER = sig
  type reader

  val get_provider : reader:reader -> dependency_addr -> file_addr option

  val is_typed_file : reader:reader -> file_addr -> bool

  val is_package_file : reader:reader -> file_addr -> bool

  val get_parse : reader:reader -> file_addr -> [ `typed | `untyped | `package ] parse_addr option

  val get_typed_parse : reader:reader -> file_addr -> [ `typed ] parse_addr option

  val get_package_parse : reader:reader -> file_addr -> [ `package ] parse_addr option

  val get_haste_info : reader:reader -> file_addr -> haste_info_addr option

  val get_haste_module_info : reader:reader -> file_addr -> Haste_module_info.t option

  val get_leader : reader:reader -> [ `typed ] parse_addr -> file_addr option

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_aloc_table : reader:reader -> File_key.t -> ALoc.table option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_imports : reader:reader -> File_key.t -> Imports.t option

  val get_tolerable_file_sig : reader:reader -> File_key.t -> File_sig.tolerable_t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.t option

  val get_type_sig : reader:reader -> File_key.t -> type_sig option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_package_info : reader:reader -> File_key.t -> (Package_json.t, unit) result option

  val get_parse_unsafe :
    reader:reader -> File_key.t -> file_addr -> [ `typed | `untyped | `package ] parse_addr

  val get_typed_parse_unsafe : reader:reader -> File_key.t -> file_addr -> [ `typed ] parse_addr

  val get_package_parse_unsafe : reader:reader -> File_key.t -> file_addr -> [ `package ] parse_addr

  val get_resolved_requires_unsafe :
    reader:reader -> File_key.t -> [ `typed ] parse_addr -> resolved_requires_addr

  val get_resolved_modules_unsafe :
    reader:reader ->
    (dependency_addr -> 'a) ->
    File_key.t ->
    [ `typed ] parse_addr ->
    'a resolved_module' Flow_import_specifier.Map.t

  val get_leader_unsafe : reader:reader -> File_key.t -> [ `typed ] parse_addr -> file_addr

  val get_ast_unsafe : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t

  val get_aloc_table_unsafe : reader:reader -> File_key.t -> ALoc.table

  val get_docblock_unsafe : reader:reader -> File_key.t -> Docblock.t

  val get_exports_unsafe : reader:reader -> File_key.t -> Exports.t

  val get_imports_unsafe : reader:reader -> File_key.t -> Imports.t

  val get_tolerable_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.tolerable_t

  val get_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.t

  val get_type_sig_unsafe : reader:reader -> File_key.t -> type_sig

  val get_file_hash_unsafe : reader:reader -> File_key.t -> Xx.hash

  val loc_of_aloc : reader:reader -> ALoc.t -> Loc.t
end

module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val get_old_parse :
    reader:reader -> file_addr -> [ `typed | `untyped | `package ] parse_addr option

  val get_old_typed_parse : reader:reader -> file_addr -> [ `typed ] parse_addr option

  val get_old_haste_info : reader:reader -> file_addr -> haste_info_addr option

  val get_old_resolved_requires_unsafe :
    reader:reader -> File_key.t -> [ `typed ] parse_addr -> resolved_requires_addr

  val get_old_resolved_modules_unsafe :
    reader:reader ->
    (dependency_addr -> 'a) ->
    File_key.t ->
    [ `typed ] parse_addr ->
    'a resolved_module' Flow_import_specifier.Map.t

  val get_old_provider : reader:reader -> dependency_addr -> file_addr option

  val get_old_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_old_exports : reader:reader -> File_key.t -> Exports.t option

  val get_old_imports : reader:reader -> File_key.t -> Imports.t option

  val typed_component : reader:reader -> File_key.t Nel.t -> component_file Nel.t option
end

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

(* For use by a worker process *)
type worker_mutator = {
  add_parsed:
    File_key.t ->
    file_addr option ->
    exports:Exports.t ->
    imports:Imports.t ->
    Xx.hash ->
    Haste_module_info.t option ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    Flow_import_specifier.t array ->
    File_sig.tolerable_t ->
    locs_tbl ->
    type_sig ->
    Modulename.Set.t;
  add_unparsed:
    File_key.t -> file_addr option -> Xx.hash -> Haste_module_info.t option -> Modulename.Set.t;
  add_package:
    File_key.t ->
    file_addr option ->
    Xx.hash ->
    Haste_module_info.t option ->
    (Package_json.t, unit) result ->
    Modulename.Set.t;
  clear_not_found: File_key.t -> Haste_module_info.t option -> Modulename.Set.t;
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

module Resolved_requires_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_resolved_requires :
    t ->
    file_addr ->
    [ `typed ] parse_addr ->
    dependency_addr resolved_module' array ->
    dependency_addr option Modulename.Map.t ->
    unit
end

module Merge_context_mutator : sig
  type t

  val create : Transaction.t -> Utils_js.FilenameSet.t -> t

  val add_merge_on_diff : for_find_all_refs:bool -> t -> component_file Nel.t -> Xx.hash -> bool
end

module Saved_state_mutator : sig
  type master_mutator

  type worker_mutator

  val create : Transaction.t -> ((File_key.t -> unit) -> unit) -> master_mutator * worker_mutator

  val add_parsed :
    worker_mutator ->
    File_key.t ->
    file_addr option ->
    Xx.hash ->
    Haste_module_info.t option ->
    Exports.t ->
    Flow_import_specifier.t array ->
    resolved_module array ->
    Modulename.t array ->
    Imports.t ->
    Modulename.Set.t

  val add_unparsed :
    worker_mutator ->
    File_key.t ->
    file_addr option ->
    Xx.hash ->
    Haste_module_info.t option ->
    Modulename.Set.t

  val add_package :
    worker_mutator ->
    File_key.t ->
    file_addr option ->
    Xx.hash ->
    Haste_module_info.t option ->
    (Package_json.t, unit) result ->
    Modulename.Set.t

  val clear_not_found : worker_mutator -> File_key.t -> Modulename.Set.t

  val record_not_found : master_mutator -> Utils_js.FilenameSet.t -> unit
end
