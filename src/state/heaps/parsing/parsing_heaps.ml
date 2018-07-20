(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

  let remove_batch files =
    ASTHeap.remove_batch files;
    DocblockHeap.remove_batch files;
    FileSigHeap.remove_batch files;
    FileHashHeap.remove_batch files;
    SharedMem_js.collect `gentle

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

let add_hash = FileHashHeap.add

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
