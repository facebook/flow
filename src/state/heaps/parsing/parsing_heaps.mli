(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val has_ast: File_key.t -> bool
val has_old_ast: File_key.t -> bool

val get_ast: File_key.t -> Loc.t Ast.program option
val get_docblock: File_key.t -> Docblock.t option
val get_file_sig: File_key.t -> File_sig.t option
val get_file_hash: File_key.t -> Xx.hash option
val get_old_file_hash: File_key.t -> Xx.hash option

(* after parsing, retrieves ast and docblock by filename (unsafe) *)
val get_ast_unsafe: File_key.t -> Loc.t Ast.program
val get_docblock_unsafe: File_key.t -> Docblock.t
val get_file_sig_unsafe: File_key.t -> File_sig.t
val get_file_hash_unsafe: File_key.t -> Xx.hash

(********* I plan to hide these almost immediately ************)
val add_hash: File_key.t -> Xx.hash -> unit
module ParsingHeaps: sig
  val add: File_key.t -> Loc.t Ast.program -> Docblock.t -> File_sig.t -> unit
  val oldify_batch: Utils_js.FilenameSet.t -> unit
  val remove_batch: Utils_js.FilenameSet.t -> unit
  val remove_old_batch: Utils_js.FilenameSet.t -> unit
  val revive_batch: Utils_js.FilenameSet.t -> unit
end
