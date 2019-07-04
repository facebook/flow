(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* shared heap for parsed ASTs by filename *)
module ASTHeap = SharedMem_js.WithCache (SharedMem_js.Immediate) (File_key) (struct
    type t = (Loc.t, Loc.t) Flow_ast.program
    let prefix = Prefix.make()
    let description = "AST"
end)

module SigASTHeap = SharedMem_js.WithCache (SharedMem_js.Immediate) (File_key) (struct
    type t = (ALoc.t, ALoc.t) Flow_ast.program
    let prefix = Prefix.make()
    let description = "SigAST"
end)

module SigASTALocTableHeap = SharedMem_js.WithCache (SharedMem_js.Immediate) (File_key) (struct
    type t = ALoc.table
    let prefix = Prefix.make()
    let description = "ALocTable"
end)

(* There's some redundancy in the visitors here, but an attempt to avoid repeated code led,
 * inexplicably, to a shared heap size regression under types-first: D15481813 *)
let source_remover_loc = object(this)
  inherit [Loc.t, Loc.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

  method private remove_source loc = { loc with Loc.source = None }

  method on_loc_annot = this#remove_source
  method on_type_annot = this#remove_source
end

let source_remover_aloc = object(this)
  inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

  method private remove_source = ALoc.update_source (fun _ -> None)

  method on_loc_annot = this#remove_source
  method on_type_annot = this#remove_source
end

let remove_source_loc ast = source_remover_loc#program ast
let remove_source_aloc ast = source_remover_aloc#program ast

let source_adder_loc source = object(this)
  inherit [Loc.t, Loc.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

  method private add_source loc = { loc with Loc.source; }

  method on_loc_annot = this#add_source
  method on_type_annot = this#add_source
end

let source_adder_aloc source = object(this)
  inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

  method private add_source = ALoc.update_source (fun _ -> source)

  method on_loc_annot = this#add_source
  method on_type_annot = this#add_source
end

let add_source_loc file ast = (source_adder_loc (Some file))#program ast
let add_source_aloc file ast = (source_adder_aloc (Some file))#program ast

module DocblockHeap = SharedMem_js.WithCache (SharedMem_js.Immediate) (File_key) (struct
    type t = Docblock.t
    let prefix = Prefix.make()
    let description = "Docblock"
end)

module FileSigHeap = SharedMem_js.WithCache (SharedMem_js.Immediate) (File_key) (struct
    type t = File_sig.With_Loc.t
    let prefix = Prefix.make()
    let description = "Requires"
end)

module SigFileSigHeap = SharedMem_js.WithCache (SharedMem_js.Immediate) (File_key) (struct
    type t = File_sig.With_ALoc.t
    let prefix = Prefix.make()
    let description = "SigRequires"
end)

(* Contains the hash for every file we even consider parsing *)
module FileHashHeap = SharedMem_js.WithCache (SharedMem_js.Immediate)
 (File_key) (struct
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
end)

(* Groups operations on the multiple heaps that need to stay in sync *)
module ParsingHeaps = struct
  let add file info (ast, file_sig) sig_opt =
    ASTHeap.add file (remove_source_loc ast);
    DocblockHeap.add file info;
    FileSigHeap.add file file_sig;
    Option.iter sig_opt ~f:(fun (sig_ast, sig_file_sig, aloc_table) ->
      SigASTHeap.add file (remove_source_aloc sig_ast);
      Option.iter aloc_table ~f:(SigASTALocTableHeap.add file);
      SigFileSigHeap.add file sig_file_sig
    )

  let oldify_batch files =
    ASTHeap.oldify_batch files;
    SigASTHeap.oldify_batch files;
    SigASTALocTableHeap.oldify_batch files;
    DocblockHeap.oldify_batch files;
    FileSigHeap.oldify_batch files;
    SigFileSigHeap.oldify_batch files;
    FileHashHeap.oldify_batch files

  let remove_old_batch files =
    ASTHeap.remove_old_batch files;
    SigASTHeap.remove_old_batch files;
    SigASTALocTableHeap.remove_old_batch files;
    DocblockHeap.remove_old_batch files;
    FileSigHeap.remove_old_batch files;
    SigFileSigHeap.remove_old_batch files;
    FileHashHeap.remove_old_batch files;
    SharedMem_js.collect `gentle

  let revive_batch files =
    ASTHeap.revive_batch files;
    SigASTHeap.revive_batch files;
    SigASTALocTableHeap.revive_batch files;
    DocblockHeap.revive_batch files;
    FileSigHeap.revive_batch files;
    SigFileSigHeap.revive_batch files;
    FileHashHeap.revive_batch files
end

exception Ast_not_found of string
exception Sig_ast_not_found of string
exception Sig_ast_ALoc_table_not_found of string
exception Docblock_not_found of string
exception Requires_not_found of string
exception Sig_requires_not_found of string
exception Hash_not_found of string

module type READER = sig
  type reader

  val has_ast: reader:reader -> File_key.t -> bool

  val get_ast: reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.program option
  val get_docblock: reader:reader -> File_key.t -> Docblock.t option
  val get_file_sig: reader:reader -> File_key.t -> File_sig.With_Loc.t option
  val get_sig_file_sig: reader:reader -> File_key.t -> File_sig.With_ALoc.t option
  val get_file_hash: reader:reader -> File_key.t -> Xx.hash option

  val get_ast_unsafe: reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.program
  val get_sig_ast_unsafe: reader:reader -> File_key.t -> (ALoc.t, ALoc.t) Flow_ast.program
  val get_sig_ast_aloc_table_unsafe: reader:reader -> File_key.t -> ALoc.table
  val get_sig_ast_aloc_table_unsafe_lazy: reader:reader -> ALoc.t -> ALoc.table Lazy.t
  val get_docblock_unsafe: reader:reader -> File_key.t -> Docblock.t
  val get_file_sig_unsafe: reader:reader -> File_key.t -> File_sig.With_Loc.t
  val get_sig_file_sig_unsafe: reader:reader -> File_key.t -> File_sig.With_ALoc.t
  val get_file_hash_unsafe: reader:reader -> File_key.t -> Xx.hash
end

let make_lazy_aloc_table_fetcher ~get_sig_ast_aloc_table_unsafe =
  fun ~reader aloc -> lazy begin
    let source = match ALoc.source aloc with
    | None -> failwith "Expected `aloc` to have a `source`"
    | Some x -> x
    in
    get_sig_ast_aloc_table_unsafe ~reader source
  end

(* Init/recheck will use Mutator_reader to read the shared memory *)
module Mutator_reader: sig
  include READER with type reader = Mutator_state_reader.t

  val get_old_file_hash: reader:Mutator_state_reader.t -> File_key.t -> Xx.hash option
end = struct
  type reader = Mutator_state_reader.t

  let has_ast ~reader:_ = ASTHeap.mem

  let get_ast ~reader:_ key =
    let ast = ASTHeap.get key in
    Option.map ~f:(add_source_loc key) ast

  let get_docblock ~reader:_ = DocblockHeap.get

  let get_file_sig ~reader:_ = FileSigHeap.get

  let get_sig_file_sig ~reader:_ = SigFileSigHeap.get

  let get_file_hash ~reader:_ = FileHashHeap.get

  let get_old_file_hash ~reader:_ = FileHashHeap.get_old

  let get_ast_unsafe ~reader:_ file =
    try ASTHeap.find_unsafe file |> add_source_loc file
    with Not_found -> raise (Ast_not_found (File_key.to_string file))

  let get_sig_ast_unsafe ~reader:_ file =
    try SigASTHeap.find_unsafe file |> add_source_aloc file
    with Not_found -> raise (Sig_ast_not_found (File_key.to_string file))

  let get_sig_ast_aloc_table_unsafe ~reader:_ file =
    try SigASTALocTableHeap.find_unsafe file
    with Not_found -> raise (Sig_ast_ALoc_table_not_found (File_key.to_string file))

  let get_sig_ast_aloc_table_unsafe_lazy =
    make_lazy_aloc_table_fetcher ~get_sig_ast_aloc_table_unsafe

  let get_docblock_unsafe ~reader:_ file =
    try DocblockHeap.find_unsafe file
    with Not_found -> raise (Docblock_not_found (File_key.to_string file))

  let get_file_sig_unsafe ~reader:_ file =
    try FileSigHeap.find_unsafe file
    with Not_found -> raise (Requires_not_found (File_key.to_string file))

  let get_sig_file_sig_unsafe ~reader:_ file =
    try SigFileSigHeap.find_unsafe file
    with Not_found -> raise (Sig_requires_not_found (File_key.to_string file))

  let get_file_hash_unsafe ~reader:_ file =
    try FileHashHeap.find_unsafe file
    with Not_found -> raise (Hash_not_found (File_key.to_string file))
end

(* For use by a worker process *)
type worker_mutator = {
  add_file: File_key.t -> Docblock.t -> ((Loc.t, Loc.t) Flow_ast.program * File_sig.With_Loc.t) ->
            ((ALoc.t, ALoc.t) Flow_ast.program * File_sig.With_ALoc.t * ALoc.table option) option -> unit;
  add_hash: File_key.t -> Xx.hash -> unit
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator: sig
  val create: unit -> worker_mutator
end = struct
  let create () = { add_file = ParsingHeaps.add; add_hash = FileHashHeap.add; }
end

(* Reparsing is more complicated than parsing, since we need to worry about transactions
 *
 * Will immediately oldify `files`. When committed, will remove the oldified files. When rolled
 * back, will revive the oldified files.
 *
 * If you revive some files before the transaction ends, then those won't be affected by
 * commit/rollback
 *)
let currently_oldified_files: FilenameSet.t ref option ref = ref None
module Reparse_mutator: sig
  type master_mutator (* Used by the master process *)
  val create: Transaction.t -> FilenameSet.t -> master_mutator * worker_mutator
  val revive_files: master_mutator -> FilenameSet.t -> unit
end = struct
  type master_mutator = FilenameSet.t ref

  let commit oldified_files =
    Hh_logger.debug "Committing parsing heaps";
    ParsingHeaps.remove_old_batch oldified_files;
    currently_oldified_files := None;
    Lwt.return_unit

  let rollback oldified_files =
    Hh_logger.debug "Rolling back parsing heaps";
    ParsingHeaps.revive_batch oldified_files;
    currently_oldified_files := None;
    Lwt.return_unit

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_file file info (ast, file_sig) sig_opt =
    ParsingHeaps.add file info (ast, file_sig) sig_opt

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_hash file hash =
    FileHashHeap.add file hash

  let create transaction files =
    let master_mutator = ref files in
    currently_oldified_files := Some master_mutator;
    let worker_mutator = { add_file; add_hash; } in

    ParsingHeaps.oldify_batch files;

    let commit () = commit (!master_mutator) in
    let rollback () = rollback (!master_mutator) in
    Transaction.add ~singleton:"Reparse" ~commit ~rollback transaction;

    master_mutator, worker_mutator

  let revive_files oldified_files files =
    (* Every file in files should be in the oldified set *)
    assert (FilenameSet.is_empty (FilenameSet.diff files (!oldified_files)));
    oldified_files := FilenameSet.diff (!oldified_files) files;
    ParsingHeaps.revive_batch files
end

(* This peaks at the Reparse_mutator's state and uses that to determine whether to read from the
 * old or new heap. This is used by code outside of a init/recheck, like commands *)
module Reader: READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let should_use_oldified key =
    match !currently_oldified_files with
    | None -> false
    | Some oldified_files -> FilenameSet.mem key !oldified_files

  let has_ast ~reader:_ key =
    if should_use_oldified key
    then ASTHeap.mem_old key
    else ASTHeap.mem key

  let get_ast ~reader:_ key =
    let ast =
      if should_use_oldified key
      then ASTHeap.get_old key
      else ASTHeap.get key
    in
    Option.map ~f:(add_source_loc key) ast

  let get_sig_ast ~reader:_ key =
    let ast =
      if should_use_oldified key
      then SigASTHeap.get_old key
      else SigASTHeap.get key
    in
    Option.map ~f:(add_source_aloc key) ast

  let get_sig_ast_aloc_table ~reader:_ key =
    if should_use_oldified key
    then SigASTALocTableHeap.get_old key
    else SigASTALocTableHeap.get key

  let get_docblock ~reader:_ key =
    if should_use_oldified key
    then DocblockHeap.get_old key
    else DocblockHeap.get key

  let get_file_sig ~reader:_ key =
    if should_use_oldified key
    then FileSigHeap.get_old key
    else FileSigHeap.get key

  let get_sig_file_sig ~reader:_ key =
    if should_use_oldified key
    then SigFileSigHeap.get_old key
    else SigFileSigHeap.get key

  let get_file_hash ~reader:_ key =
    if should_use_oldified key
    then FileHashHeap.get_old key
    else FileHashHeap.get key

  let get_ast_unsafe ~reader file =
    match get_ast ~reader file with
    | Some ast -> ast
    | None -> raise (Ast_not_found (File_key.to_string file))

  let get_sig_ast_unsafe ~reader file =
    match get_sig_ast ~reader file with
    | Some ast -> ast
    | None -> raise (Sig_ast_not_found (File_key.to_string file))

  let get_sig_ast_aloc_table_unsafe ~reader file =
    match get_sig_ast_aloc_table ~reader file with
    | Some table -> table
    | None -> raise (Sig_ast_ALoc_table_not_found (File_key.to_string file))

  let get_sig_ast_aloc_table_unsafe_lazy =
    make_lazy_aloc_table_fetcher ~get_sig_ast_aloc_table_unsafe

  let get_docblock_unsafe ~reader file =
    match get_docblock ~reader file with
    | Some docblock -> docblock
    | None -> raise (Docblock_not_found (File_key.to_string file))

  let get_file_sig_unsafe ~reader file =
    match get_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_sig_file_sig_unsafe ~reader file =
    match get_sig_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Sig_requires_not_found (File_key.to_string file))

  let get_file_hash_unsafe ~reader file =
    match get_file_hash ~reader file with
    | Some file_hash -> file_hash
    | None -> raise (Hash_not_found (File_key.to_string file))
end

(* Reader_dispatcher is used by code which may or may not be running inside an init/recheck *)
module Reader_dispatcher: READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let has_ast ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.has_ast ~reader
    | State_reader reader -> Reader.has_ast ~reader

  let get_ast ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_ast ~reader
    | State_reader reader -> Reader.get_ast ~reader

  let get_docblock ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_docblock ~reader
    | State_reader reader -> Reader.get_docblock ~reader

  let get_file_sig ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_sig ~reader
    | State_reader reader -> Reader.get_file_sig ~reader

  let get_sig_file_sig ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_sig_file_sig ~reader
    | State_reader reader -> Reader.get_sig_file_sig ~reader

  let get_file_hash ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_hash ~reader
    | State_reader reader -> Reader.get_file_hash ~reader

  let get_ast_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_ast_unsafe ~reader
    | State_reader reader -> Reader.get_ast_unsafe ~reader

  let get_sig_ast_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_sig_ast_unsafe ~reader
    | State_reader reader -> Reader.get_sig_ast_unsafe ~reader

  let get_sig_ast_aloc_table_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_sig_ast_aloc_table_unsafe ~reader
    | State_reader reader -> Reader.get_sig_ast_aloc_table_unsafe ~reader

  let get_sig_ast_aloc_table_unsafe_lazy =
    make_lazy_aloc_table_fetcher ~get_sig_ast_aloc_table_unsafe

  let get_docblock_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_docblock_unsafe ~reader
    | State_reader reader -> Reader.get_docblock_unsafe ~reader

  let get_file_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_sig_unsafe ~reader
    | State_reader reader -> Reader.get_file_sig_unsafe ~reader

  let get_sig_file_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_sig_file_sig_unsafe ~reader
    | State_reader reader -> Reader.get_sig_file_sig_unsafe ~reader

  let get_file_hash_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_hash_unsafe ~reader
    | State_reader reader -> Reader.get_file_hash_unsafe ~reader
end

module From_saved_state: sig
  val add_file_sig: File_key.t -> File_sig.With_Loc.t -> unit
  val add_file_hash: File_key.t -> Xx.hash -> unit
end = struct
  let add_file_sig = FileSigHeap.add
  let add_file_hash = FileHashHeap.add
end
