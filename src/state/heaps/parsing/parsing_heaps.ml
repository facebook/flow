(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Parsing_heaps_exceptions

(* shared heap for parsed ASTs by filename *)
module ASTHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = (RelativeLoc.t, RelativeLoc.t) Flow_ast.Program.t

      let description = "AST"
    end)

module SigASTHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = (ALoc.t, ALoc.t) Flow_ast.Program.t

      let description = "SigAST"
    end)

module SigASTALocTableHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = ALoc.table

      let description = "ALocTable"
    end)

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.t

module TypeSigHeap =
  SharedMem.NoCache
    (File_key)
    (struct
      type t = type_sig

      let description = "TypeSig"
    end)

(* There's some redundancy in the visitors here, but an attempt to avoid repeated code led,
 * inexplicably, to a shared heap size regression under types-first: D15481813 *)
let loc_compactifier =
  object (this)
    inherit [Loc.t, Loc.t, RelativeLoc.t, RelativeLoc.t] Flow_polymorphic_ast_mapper.mapper

    method private compactify_loc loc = RelativeLoc.of_loc loc

    method on_loc_annot = this#compactify_loc

    method on_type_annot = this#compactify_loc
  end

let source_remover_aloc =
  object (this)
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

    method private remove_source = ALoc.update_source (fun _ -> None)

    method on_loc_annot = this#remove_source

    method on_type_annot = this#remove_source
  end

let compactify_loc ast = loc_compactifier#program ast

let remove_source_aloc ast = source_remover_aloc#program ast

let loc_decompactifier source =
  object (this)
    inherit [RelativeLoc.t, RelativeLoc.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method private decompactify_loc loc = RelativeLoc.to_loc loc source

    method on_loc_annot = this#decompactify_loc

    method on_type_annot = this#decompactify_loc
  end

let source_adder_aloc source =
  object (this)
    inherit [ALoc.t, ALoc.t, ALoc.t, ALoc.t] Flow_polymorphic_ast_mapper.mapper

    method private add_source = ALoc.update_source (fun _ -> source)

    method on_loc_annot = this#add_source

    method on_type_annot = this#add_source
  end

let decompactify_loc file ast = (loc_decompactifier (Some file))#program ast

let add_source_aloc file ast = (source_adder_aloc (Some file))#program ast

module DocblockHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = Docblock.t

      let description = "Docblock"
    end)

module FileSigHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = File_sig.With_Loc.t

      let description = "Requires"
    end)

module SigFileSigHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = File_sig.With_ALoc.t

      let description = "SigRequires"
    end)

(* Contains the hash for every file we even consider parsing *)
module FileHashHeap =
  SharedMem.WithCache
    (File_key)
    (struct
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

      let description = "FileHash"
    end)

module ExportsHeap =
  SharedMem.NoCache
    (File_key)
    (struct
      type t = Exports.t

      let description = "Exports"
    end)

type sig_extra =
  | TypesFirst of {
      sig_ast: (ALoc.t, ALoc.t) Flow_ast.Program.t;
      sig_file_sig: File_sig.With_ALoc.t;
      aloc_table: ALoc.table option;
    }
  | TypeSig of type_sig * ALoc.table

(* Groups operations on the multiple heaps that need to stay in sync *)
module ParsingHeaps = struct
  let add file ~exports info (ast, file_sig) extra =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.add file (compactify_loc ast);
        DocblockHeap.add file info;
        ExportsHeap.add file exports;
        FileSigHeap.add file file_sig;
        match extra with
        | TypesFirst { sig_ast; sig_file_sig; aloc_table } ->
          SigASTHeap.add file (remove_source_aloc sig_ast);
          Base.Option.iter aloc_table ~f:(SigASTALocTableHeap.add file);
          SigFileSigHeap.add file sig_file_sig
        | TypeSig (type_sig, aloc_table) ->
          TypeSigHeap.add file type_sig;
          SigASTALocTableHeap.add file aloc_table)

  let oldify_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.oldify_batch files;
        SigASTHeap.oldify_batch files;
        TypeSigHeap.oldify_batch files;
        SigASTALocTableHeap.oldify_batch files;
        DocblockHeap.oldify_batch files;
        ExportsHeap.oldify_batch files;
        FileSigHeap.oldify_batch files;
        SigFileSigHeap.oldify_batch files;
        FileHashHeap.oldify_batch files)

  let remove_old_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.remove_old_batch files;
        SigASTHeap.remove_old_batch files;
        TypeSigHeap.remove_old_batch files;
        SigASTALocTableHeap.remove_old_batch files;
        DocblockHeap.remove_old_batch files;
        ExportsHeap.remove_old_batch files;
        FileSigHeap.remove_old_batch files;
        SigFileSigHeap.remove_old_batch files;
        FileHashHeap.remove_old_batch files)

  let revive_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.revive_batch files;
        SigASTHeap.revive_batch files;
        TypeSigHeap.revive_batch files;
        SigASTALocTableHeap.revive_batch files;
        DocblockHeap.revive_batch files;
        ExportsHeap.revive_batch files;
        FileSigHeap.revive_batch files;
        SigFileSigHeap.revive_batch files;
        FileHashHeap.revive_batch files)
end

module type READER = sig
  type reader

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_ast_unsafe : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t

  val get_sig_ast_unsafe : reader:reader -> File_key.t -> (ALoc.t, ALoc.t) Flow_ast.Program.t

  val get_sig_ast_aloc_table_unsafe : reader:reader -> File_key.t -> ALoc.table

  val get_sig_ast_aloc_table_unsafe_lazy : reader:reader -> ALoc.t -> ALoc.table Lazy.t

  val get_docblock_unsafe : reader:reader -> File_key.t -> Docblock.t

  val get_exports_unsafe : reader:reader -> File_key.t -> Exports.t

  val get_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.t

  val get_sig_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_ALoc.t

  val get_type_sig_unsafe : reader:reader -> File_key.t -> type_sig

  val get_file_hash_unsafe : reader:reader -> File_key.t -> Xx.hash
end

let make_lazy_aloc_table_fetcher ~get_sig_ast_aloc_table_unsafe ~reader aloc =
  lazy
    begin
      let source =
        match ALoc.source aloc with
        | None -> failwith "Expected `aloc` to have a `source`"
        | Some x -> x
      in
      get_sig_ast_aloc_table_unsafe ~reader source
    end

(* Init/recheck will use Mutator_reader to read the shared memory *)
module Mutator_reader : sig
  include READER with type reader = Mutator_state_reader.t

  val get_old_file_hash : reader:Mutator_state_reader.t -> File_key.t -> Xx.hash option

  val get_old_exports : reader:Mutator_state_reader.t -> File_key.t -> Exports.t option
end = struct
  type reader = Mutator_state_reader.t

  let has_ast ~reader:_ = ASTHeap.mem

  let get_ast ~reader:_ key =
    let ast = ASTHeap.get key in
    Base.Option.map ~f:(decompactify_loc key) ast

  let get_docblock ~reader:_ = DocblockHeap.get

  let get_exports ~reader:_ = ExportsHeap.get

  let get_old_exports ~reader:_ = ExportsHeap.get_old

  let get_file_sig ~reader:_ = FileSigHeap.get

  let get_file_hash ~reader:_ = FileHashHeap.get

  let get_old_file_hash ~reader:_ = FileHashHeap.get_old

  let get_ast_unsafe ~reader file =
    match get_ast ~reader file with
    | Some ast -> ast
    | None -> raise (Ast_not_found (File_key.to_string file))

  let get_sig_ast_unsafe ~reader:_ file =
    match SigASTHeap.get file with
    | Some sig_ast -> add_source_aloc file sig_ast
    | None -> raise (Sig_ast_not_found (File_key.to_string file))

  let get_sig_ast_aloc_table_unsafe ~reader:_ file =
    match SigASTALocTableHeap.get file with
    | Some aloc_table -> aloc_table
    | None -> raise (Sig_ast_ALoc_table_not_found (File_key.to_string file))

  let get_sig_ast_aloc_table_unsafe_lazy =
    make_lazy_aloc_table_fetcher ~get_sig_ast_aloc_table_unsafe

  let get_docblock_unsafe ~reader file =
    match get_docblock ~reader file with
    | Some docblock -> docblock
    | None -> raise (Docblock_not_found (File_key.to_string file))

  let get_exports_unsafe ~reader:_ file =
    match ExportsHeap.get file with
    | Some exports -> exports
    | None -> raise (Exports_not_found (File_key.to_string file))

  let get_file_sig_unsafe ~reader file =
    match get_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_sig_file_sig_unsafe ~reader:_ file =
    match SigFileSigHeap.get file with
    | Some sig_file_sig -> sig_file_sig
    | None -> raise (Sig_requires_not_found (File_key.to_string file))

  let get_type_sig_unsafe ~reader:_ file =
    match TypeSigHeap.get file with
    | Some type_sig -> type_sig
    | None -> raise (Type_sig_not_found (File_key.to_string file))

  let get_file_hash_unsafe ~reader file =
    match get_file_hash ~reader file with
    | Some hash -> hash
    | None -> raise (Hash_not_found (File_key.to_string file))
end

(* For use by a worker process *)
type worker_mutator = {
  add_file:
    File_key.t ->
    exports:Exports.t ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t * File_sig.With_Loc.t ->
    sig_extra ->
    unit;
  add_hash: File_key.t -> Xx.hash -> unit;
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator : sig
  val create : unit -> worker_mutator
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
let currently_oldified_files : FilenameSet.t ref option ref = ref None

module Reparse_mutator : sig
  type master_mutator (* Used by the master process *)

  val create : Transaction.t -> FilenameSet.t -> master_mutator * worker_mutator

  val revive_files : master_mutator -> FilenameSet.t -> unit
end = struct
  type master_mutator = FilenameSet.t ref

  let commit oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing parsing heaps";
        ParsingHeaps.remove_old_batch oldified_files;
        currently_oldified_files := None);
    Lwt.return_unit

  let rollback oldified_files =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back parsing heaps";
        ParsingHeaps.revive_batch oldified_files;
        currently_oldified_files := None);
    Lwt.return_unit

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_file file ~exports info (ast, file_sig) sig_opt =
    ParsingHeaps.add file ~exports info (ast, file_sig) sig_opt

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_hash file hash = FileHashHeap.add file hash

  let create transaction files =
    WorkerCancel.with_no_cancellations (fun () ->
        let master_mutator = ref files in
        currently_oldified_files := Some master_mutator;
        let worker_mutator = { add_file; add_hash } in
        ParsingHeaps.oldify_batch files;

        let commit () = commit !master_mutator in
        let rollback () = rollback !master_mutator in
        Transaction.add ~singleton:"Reparse" ~commit ~rollback transaction;

        (master_mutator, worker_mutator))

  let revive_files oldified_files files =
    WorkerCancel.with_no_cancellations (fun () ->
        (* Every file in files should be in the oldified set *)
        assert (FilenameSet.is_empty (FilenameSet.diff files !oldified_files));
        oldified_files := FilenameSet.diff !oldified_files files;
        ParsingHeaps.revive_batch files)
end

(* This peaks at the Reparse_mutator's state and uses that to determine whether to read from the
 * old or new heap. This is used by code outside of a init/recheck, like commands *)
module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let should_use_oldified key =
    match !currently_oldified_files with
    | None -> false
    | Some oldified_files -> FilenameSet.mem key !oldified_files

  let has_ast ~reader:_ key =
    if should_use_oldified key then
      ASTHeap.mem_old key
    else
      ASTHeap.mem key

  let get_ast ~reader:_ key =
    let ast =
      if should_use_oldified key then
        ASTHeap.get_old key
      else
        ASTHeap.get key
    in
    Base.Option.map ~f:(decompactify_loc key) ast

  let get_sig_ast ~reader:_ key =
    let ast =
      if should_use_oldified key then
        SigASTHeap.get_old key
      else
        SigASTHeap.get key
    in
    Base.Option.map ~f:(add_source_aloc key) ast

  let get_sig_ast_aloc_table ~reader:_ key =
    if should_use_oldified key then
      SigASTALocTableHeap.get_old key
    else
      SigASTALocTableHeap.get key

  let get_docblock ~reader:_ key =
    if should_use_oldified key then
      DocblockHeap.get_old key
    else
      DocblockHeap.get key

  let get_exports ~reader:_ key =
    if should_use_oldified key then
      ExportsHeap.get_old key
    else
      ExportsHeap.get key

  let get_file_sig ~reader:_ key =
    if should_use_oldified key then
      FileSigHeap.get_old key
    else
      FileSigHeap.get key

  let get_sig_file_sig ~reader:_ key =
    if should_use_oldified key then
      SigFileSigHeap.get_old key
    else
      SigFileSigHeap.get key

  let get_type_sig ~reader:_ key =
    if should_use_oldified key then
      TypeSigHeap.get_old key
    else
      TypeSigHeap.get key

  let get_file_hash ~reader:_ key =
    if should_use_oldified key then
      FileHashHeap.get_old key
    else
      FileHashHeap.get key

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

  let get_exports_unsafe ~reader file =
    match get_exports ~reader file with
    | Some exports -> exports
    | None -> raise (Exports_not_found (File_key.to_string file))

  let get_file_sig_unsafe ~reader file =
    match get_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_sig_file_sig_unsafe ~reader file =
    match get_sig_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Sig_requires_not_found (File_key.to_string file))

  let get_type_sig_unsafe ~reader file =
    match get_type_sig ~reader file with
    | Some type_sig -> type_sig
    | None -> raise (Type_sig_not_found (File_key.to_string file))

  let get_file_hash_unsafe ~reader file =
    match get_file_hash ~reader file with
    | Some file_hash -> file_hash
    | None -> raise (Hash_not_found (File_key.to_string file))
end

(* Reader_dispatcher is used by code which may or may not be running inside an init/recheck *)
module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
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

  let get_exports ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_exports ~reader
    | State_reader reader -> Reader.get_exports ~reader

  let get_file_sig ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_sig ~reader
    | State_reader reader -> Reader.get_file_sig ~reader

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

  let get_exports_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_exports_unsafe ~reader
    | State_reader reader -> Reader.get_exports_unsafe ~reader

  let get_file_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_sig_unsafe ~reader
    | State_reader reader -> Reader.get_file_sig_unsafe ~reader

  let get_sig_file_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_sig_file_sig_unsafe ~reader
    | State_reader reader -> Reader.get_sig_file_sig_unsafe ~reader

  let get_type_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_type_sig_unsafe ~reader
    | State_reader reader -> Reader.get_type_sig_unsafe ~reader

  let get_file_hash_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_hash_unsafe ~reader
    | State_reader reader -> Reader.get_file_hash_unsafe ~reader
end

module From_saved_state : sig
  val add_file_sig : File_key.t -> File_sig.With_Loc.t -> unit

  val add_file_hash : File_key.t -> Xx.hash -> unit

  val add_exports : File_key.t -> Exports.t -> unit
end = struct
  let add_file_sig = FileSigHeap.add

  let add_file_hash = FileHashHeap.add

  let add_exports = ExportsHeap.add
end

let add_aloc_table = SigASTALocTableHeap.add
