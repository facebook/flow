(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* shared heap for parsed ASTs by filename *)
module ASTHeap = SharedMem_js.WithCache (File_key) (struct
    type t = Loc.t Ast.program
    let prefix = Prefix.make()
    let description = "AST"
    let use_sqlite_fallback () = false
end)

module DocblockHeap = SharedMem_js.WithCache (File_key) (struct
    type t = Docblock.t
    let prefix = Prefix.make()
    let description = "Docblock"
    let use_sqlite_fallback () = false
end)

module FileSigHeap = SharedMem_js.WithCache (File_key) (struct
    type t = File_sig.t
    let prefix = Prefix.make()
    let description = "Requires"
    let use_sqlite_fallback () = false
end)

(* Contains the hash for every file we even consider parsing *)
module FileHashHeap = SharedMem_js.WithCache (File_key) (struct
    (* In the future I imagine a system like this:
     *
     * type t = {
     *   since_version: Recheck.version;
     *   hash: Sha1.hash;
     * }
     *
     * Every time we notice files changing (via file_watcher or some other way) we bump the version
     * so a recheck is known to be from version N to version N+1. If the recheck gets cancelled
     * due to a file watcher event, then we're checking from version N to version N+2 (etc etc).
     *
     * Ideally we'd be able to ignore file watcher events that are either old & outdated or where
     * hash hasn't changed (watchman can provide the sha1).
     *
     * And ideally a cancelled recheck leading to a recheck from version N to N+2 will still
     * merge file foo.js, even if we parsed it at version N+1 & it's unchanged since version N+1.
     *)
    type t = Xx.hash
    let prefix = Prefix.make()
    let description = "FileHash"
    let use_sqlite_fallback () = false
end)

(* Groups operations on the multiple heaps that need to stay in sync *)
module ParsingHeaps = struct
  let add file ast info file_sig =
    ASTHeap.add file ast;
    DocblockHeap.add file info;
    FileSigHeap.add file file_sig

  let oldify_batch files =
    ASTHeap.oldify_batch files;
    DocblockHeap.oldify_batch files;
    FileSigHeap.oldify_batch files;
    FileHashHeap.oldify_batch files

  let remove_old_batch files =
    ASTHeap.remove_old_batch files;
    DocblockHeap.remove_old_batch files;
    FileSigHeap.remove_old_batch files;
    FileHashHeap.remove_old_batch files;
    SharedMem_js.collect `gentle

  let revive_batch files =
    ASTHeap.revive_batch files;
    DocblockHeap.revive_batch files;
    FileSigHeap.revive_batch files;
    FileHashHeap.revive_batch files
end

(* For use by a worker process *)
type worker_mutator = {
  add_file: File_key.t -> Loc.t Ast.program -> Docblock.t -> File_sig.t -> unit;
  add_hash: File_key.t -> Xx.hash -> unit
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator: sig
  val create: unit -> worker_mutator
end = struct
  let create () = { add_file = ParsingHeaps.add; add_hash = FileHashHeap.add }
end

(* Reparsing is more complicated than parsing, since we need to worry about transactions
 *
 * Will immediately oldify `files`. When committed, will remove the oldified files. When rolled
 * back, will revive the oldified files.
 *
 * If you revive some files before the transaction ends, then those won't be affected by
 * commit/rollback
 *)
module Reparse_mutator: sig
  type master_mutator (* Used by the master process *)
  val create: Transaction.t -> FilenameSet.t -> master_mutator * worker_mutator
  val revive_files: master_mutator -> FilenameSet.t -> unit
end = struct
  type master_mutator = FilenameSet.t ref

  let commit oldified_files =
    Hh_logger.debug "Committing parsing heaps";
    ParsingHeaps.remove_old_batch oldified_files;
    Lwt.return_unit

  let rollback oldified_files =
    Hh_logger.debug "Rolling back parsing heaps";
    ParsingHeaps.revive_batch oldified_files;
    Lwt.return_unit

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_file file ast info file_sig =
    ParsingHeaps.add file ast info file_sig

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_hash file hash =
    FileHashHeap.add file hash

  let create transaction files =
    let master_mutator = ref files in
    let worker_mutator = { add_file; add_hash } in

    ParsingHeaps.oldify_batch files;

    let commit () = commit (!master_mutator) in
    let rollback () = rollback (!master_mutator) in
    Transaction.add ~commit ~rollback transaction;

    master_mutator, worker_mutator

  let revive_files oldified_files files =
    (* Every file in files should be in the oldified set *)
    assert (FilenameSet.is_empty (FilenameSet.diff files (!oldified_files)));
    oldified_files := FilenameSet.diff (!oldified_files) files;
    ParsingHeaps.revive_batch files
end

let has_ast = ASTHeap.mem

let has_old_ast = ASTHeap.mem_old

let get_ast = ASTHeap.get

let get_docblock = DocblockHeap.get

let get_file_sig = FileSigHeap.get

let get_file_hash = FileHashHeap.get

let get_old_file_hash = FileHashHeap.get_old

exception Ast_not_found of string
let get_ast_unsafe file =
  try ASTHeap.find_unsafe file
  with Not_found -> raise (Ast_not_found (File_key.to_string file))

exception Docblock_not_found of string
let get_docblock_unsafe file =
  try DocblockHeap.find_unsafe file
  with Not_found -> raise (Docblock_not_found (File_key.to_string file))

exception Requires_not_found of string
let get_file_sig_unsafe file =
  try FileSigHeap.find_unsafe file
  with Not_found -> raise (Requires_not_found (File_key.to_string file))

exception Hash_not_found of string
let get_file_hash_unsafe file =
  try FileHashHeap.find_unsafe file
  with Not_found -> raise (Hash_not_found (File_key.to_string file))
