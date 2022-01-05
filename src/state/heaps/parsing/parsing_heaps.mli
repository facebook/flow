(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = SharedMem.NewAPI.dyn_file SharedMem.addr

type checked_file_addr = SharedMem.NewAPI.checked_file SharedMem.addr

type unparsed_file_addr = SharedMem.NewAPI.unparsed_file SharedMem.addr

val is_checked_file : file_addr -> bool

val coerce_checked_file : file_addr -> checked_file_addr option

val read_checked_file_hash : checked_file_addr -> Xx.hash

val read_unparsed_file_hash : unparsed_file_addr -> Xx.hash

val read_module_name : file_addr -> string option

val read_checked_module_name : checked_file_addr -> string option

val read_unparsed_module_name : unparsed_file_addr -> string option

val read_ast_unsafe : File_key.t -> checked_file_addr -> (Loc.t, Loc.t) Flow_ast.Program.t

val read_docblock_unsafe : File_key.t -> checked_file_addr -> Docblock.t

val read_aloc_table_unsafe : File_key.t -> checked_file_addr -> ALoc.table

val read_type_sig_unsafe : File_key.t -> checked_file_addr -> type_sig

val read_tolerable_file_sig_unsafe :
  File_key.t -> checked_file_addr -> File_sig.With_Loc.tolerable_t

val read_file_sig_unsafe : File_key.t -> checked_file_addr -> File_sig.With_Loc.t

val read_exports : checked_file_addr -> Exports.t

module type READER = sig
  type reader

  val get_file_addr : reader:reader -> File_key.t -> file_addr option

  val get_checked_file_addr : reader:reader -> File_key.t -> checked_file_addr option

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_aloc_table : reader:reader -> File_key.t -> ALoc.table option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_tolerable_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_type_sig : reader:reader -> File_key.t -> type_sig option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_file_addr_unsafe : reader:reader -> File_key.t -> file_addr

  val get_checked_file_addr_unsafe : reader:reader -> File_key.t -> checked_file_addr

  val get_unparsed_file_addr_unsafe : reader:reader -> File_key.t -> unparsed_file_addr

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

  val get_old_file_addr : reader:Mutator_state_reader.t -> File_key.t -> file_addr option

  val get_old_checked_file_addr :
    reader:Mutator_state_reader.t -> File_key.t -> checked_file_addr option

  val get_old_file_hash : reader:Mutator_state_reader.t -> File_key.t -> Xx.hash option

  val get_old_exports : reader:Mutator_state_reader.t -> File_key.t -> Exports.t option
end

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

(* For use by a worker process *)
type worker_mutator = {
  add_parsed:
    File_key.t ->
    exports:Exports.t ->
    Xx.hash ->
    string option ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    File_sig.With_Loc.tolerable_t ->
    locs_tbl ->
    type_sig ->
    unit;
  add_unparsed: File_key.t -> Xx.hash -> string option -> unit;
}

module Parse_mutator : sig
  val create : unit -> worker_mutator
end

module Reparse_mutator : sig
  type master_mutator (* Used by the master process *)

  val create : Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator

  val revive_files : master_mutator -> Utils_js.FilenameSet.t -> unit
end

module From_saved_state : sig
  val add_parsed : File_key.t -> Xx.hash -> string option -> Exports.t -> unit

  val add_unparsed : File_key.t -> Xx.hash -> string option -> unit
end
