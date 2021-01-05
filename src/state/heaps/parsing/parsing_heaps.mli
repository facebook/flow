(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.t

module type READER = sig
  type reader

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_ast_unsafe : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t

  val get_sig_ast_unsafe : reader:reader -> File_key.t -> (ALoc.t, ALoc.t) Flow_ast.Program.t

  val get_sig_ast_aloc_table_unsafe : reader:reader -> File_key.t -> ALoc.table

  val get_sig_ast_aloc_table_unsafe_lazy : reader:reader -> ALoc.t -> ALoc.table Lazy.t

  val get_docblock_unsafe : reader:reader -> File_key.t -> Docblock.t

  val get_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.t

  val get_sig_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_ALoc.t

  val get_type_sig_unsafe : reader:reader -> File_key.t -> type_sig

  val get_file_hash_unsafe : reader:reader -> File_key.t -> Xx.hash
end

module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val get_old_file_hash : reader:Mutator_state_reader.t -> File_key.t -> Xx.hash option
end

module Reader : READER with type reader = State_reader.t

module Reader_dispatcher : READER with type reader = Abstract_state_reader.t

type sig_extra =
  | InitLibs
  | TypesFirst of {
      sig_ast: (ALoc.t, ALoc.t) Flow_ast.Program.t;
      sig_file_sig: File_sig.With_ALoc.t;
      aloc_table: ALoc.table option;
    }
  | TypeSig of type_sig * ALoc.table

(* For use by a worker process *)
type worker_mutator = {
  add_file:
    File_key.t ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t * File_sig.With_Loc.t ->
    sig_extra ->
    unit;
  add_hash: File_key.t -> Xx.hash -> unit;
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
  val add_file_sig : File_key.t -> File_sig.With_Loc.t -> unit

  val add_file_hash : File_key.t -> Xx.hash -> unit
end

(* Temporary API. This is needed for the types-first 2.0 demo, which produces
 * these tables separately from the parse phase. *)
val add_aloc_table : File_key.t -> ALoc.table -> unit
