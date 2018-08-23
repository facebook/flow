(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* For use by a worker process *)
type worker_mutator = {
  add_file: File_key.t -> (Loc.t, Loc.t) Flow_ast.program -> Docblock.t -> File_sig.t -> unit;
  add_hash: File_key.t -> Xx.hash -> unit
}

module Parse_mutator: sig
  val create: unit -> worker_mutator
end

module Reparse_mutator: sig
  type master_mutator (* Used by the master process *)
  val create: Transaction.t -> Utils_js.FilenameSet.t -> master_mutator * worker_mutator
  val revive_files: master_mutator -> Utils_js.FilenameSet.t -> unit
end


val has_ast: File_key.t -> bool
val has_old_ast: File_key.t -> bool

val get_ast: File_key.t -> (Loc.t, Loc.t) Flow_ast.program option
val get_docblock: File_key.t -> Docblock.t option
val get_file_sig: File_key.t -> File_sig.t option
val get_file_hash: File_key.t -> Xx.hash option
val get_old_file_hash: File_key.t -> Xx.hash option

(* after parsing, retrieves ast and docblock by filename (unsafe) *)
val get_ast_unsafe: File_key.t -> (Loc.t, Loc.t) Flow_ast.program
val get_docblock_unsafe: File_key.t -> Docblock.t
val get_file_sig_unsafe: File_key.t -> File_sig.t
val get_file_hash_unsafe: File_key.t -> Xx.hash

module From_saved_state: sig
  val add_file_sig: File_key.t -> File_sig.t -> unit
  val add_file_hash: File_key.t -> Xx.hash -> unit
end
