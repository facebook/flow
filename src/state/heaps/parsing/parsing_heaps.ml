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

module Heap = SharedMem.NewAPI

module TypeSigHeap =
  SharedMem.NoCacheAddr
    (File_key)
    (struct
      type t = Heap.checked_file
    end)

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type checked_file_addr = Heap.checked_file SharedMem.addr

let write_type_sig docblock locs type_sig =
  let open Type_sig_collections in
  let serialize x = Marshal.to_string x [] in
  let docblock = serialize docblock in
  let aloc_table = Packed_locs.pack (Locs.length locs) (fun f -> Locs.iter f locs) in
  let (sig_bsize, write_sig) = Type_sig_bin.write type_sig in
  let open Heap in
  let size =
    (4 * header_size)
    + checked_file_size
    + docblock_size docblock
    + aloc_table_size aloc_table
    + type_sig_size sig_bsize
  in
  alloc size (fun chunk ->
      let docblock = write_docblock chunk docblock in
      let aloc_table = write_aloc_table chunk aloc_table in
      let type_sig = write_type_sig chunk sig_bsize write_sig in
      write_checked_file chunk docblock aloc_table type_sig
  )

let read_docblock file_addr =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  file_docblock file_addr |> read_docblock |> deserialize

let read_aloc_table file_key file_addr =
  let open Heap in
  let init = ALoc.ALocRepresentationDoNotUse.init_table file_key in
  file_aloc_table file_addr |> read_aloc_table |> Packed_locs.unpack (Some file_key) init

let read_type_sig file =
  let open Heap in
  read_type_sig (file_type_sig file) Type_sig_bin.read

(* There's some redundancy in the visitors here, but an attempt to avoid repeated code led,
 * inexplicably, to a shared heap size regression under types-first: D15481813 *)
let loc_compactifier =
  object (this)
    inherit [Loc.t, Loc.t, RelativeLoc.t, RelativeLoc.t] Flow_polymorphic_ast_mapper.mapper

    method private compactify_loc loc = RelativeLoc.of_loc loc

    method on_loc_annot = this#compactify_loc

    method on_type_annot = this#compactify_loc
  end

let compactify_loc ast = loc_compactifier#program ast

let loc_decompactifier source =
  object (this)
    inherit [RelativeLoc.t, RelativeLoc.t, Loc.t, Loc.t] Flow_polymorphic_ast_mapper.mapper

    method private decompactify_loc loc = RelativeLoc.to_loc loc source

    method on_loc_annot = this#decompactify_loc

    method on_type_annot = this#decompactify_loc
  end

let decompactify_loc file ast = (loc_decompactifier (Some file))#program ast

module FileSigHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = File_sig.With_Loc.tolerable_t

      let description = "Requires"
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

module ALocTableCache = SharedMem.LocalCache (struct
  type key = File_key.t

  type value = ALoc.table

  let capacity = 1000
end)

(* Groups operations on the multiple heaps that need to stay in sync *)
module ParsingHeaps = struct
  let add file ~exports info ast file_sig locs type_sig =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.add file (compactify_loc ast);
        ExportsHeap.add file exports;
        FileSigHeap.add file file_sig;
        TypeSigHeap.add file (write_type_sig info locs type_sig)
    )

  let oldify_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.oldify_batch files;
        TypeSigHeap.oldify_batch files;
        FilenameSet.iter ALocTableCache.remove files;
        ExportsHeap.oldify_batch files;
        FileSigHeap.oldify_batch files;
        FileHashHeap.oldify_batch files
    )

  let remove_old_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.remove_old_batch files;
        TypeSigHeap.remove_old_batch files;
        ExportsHeap.remove_old_batch files;
        FileSigHeap.remove_old_batch files;
        FileHashHeap.remove_old_batch files
    )

  let revive_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.revive_batch files;
        TypeSigHeap.revive_batch files;
        FilenameSet.iter ALocTableCache.remove files;
        ExportsHeap.revive_batch files;
        FileSigHeap.revive_batch files;
        FileHashHeap.revive_batch files
    )
end

module type READER = sig
  type reader

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_tolerable_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_type_sig : reader:reader -> File_key.t -> type_sig option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_ast_unsafe : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t

  val get_aloc_table_unsafe : reader:reader -> File_key.t -> ALoc.table

  val get_docblock_unsafe : reader:reader -> File_key.t -> Docblock.t

  val get_exports_unsafe : reader:reader -> File_key.t -> Exports.t

  val get_tolerable_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t

  val get_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.t

  val get_type_sig_addr_unsafe : reader:reader -> File_key.t -> checked_file_addr

  val get_type_sig_unsafe : reader:reader -> File_key.t -> type_sig

  val get_file_hash_unsafe : reader:reader -> File_key.t -> Xx.hash

  val loc_of_aloc : reader:reader -> ALoc.t -> Loc.t
end

let loc_of_aloc ~get_aloc_table_unsafe ~reader aloc =
  let table =
    lazy
      (let source =
         match ALoc.source aloc with
         | None -> failwith "Expected `aloc` to have a `source`"
         | Some x -> x
       in
       get_aloc_table_unsafe ~reader source
      )
  in
  ALoc.to_loc table aloc

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

  let get_type_sig_addr ~reader:_ = TypeSigHeap.get

  let get_docblock ~reader file =
    match get_type_sig_addr ~reader file with
    | Some addr -> Some (read_docblock addr)
    | None -> None

  let get_exports ~reader:_ = ExportsHeap.get

  let get_old_exports ~reader:_ = ExportsHeap.get_old

  let get_tolerable_file_sig ~reader:_ = FileSigHeap.get

  let get_file_sig ~reader file =
    match get_tolerable_file_sig ~reader file with
    | Some (file_sig, _tolerable_errors) -> Some file_sig
    | None -> None

  let get_type_sig ~reader:_ file =
    match TypeSigHeap.get file with
    | Some addr -> Some (read_type_sig addr)
    | None -> None

  let get_file_hash ~reader:_ = FileHashHeap.get

  let get_old_file_hash ~reader:_ = FileHashHeap.get_old

  let get_ast_unsafe ~reader file =
    match get_ast ~reader file with
    | Some ast -> ast
    | None -> raise (Ast_not_found (File_key.to_string file))

  let get_aloc_table_unsafe ~reader:_ file =
    match ALocTableCache.get file with
    | Some aloc_table -> aloc_table
    | None ->
      (match TypeSigHeap.get file with
      | Some addr ->
        let aloc_table = read_aloc_table file addr in
        ALocTableCache.add file aloc_table;
        aloc_table
      | None -> raise (ALoc_table_not_found (File_key.to_string file)))

  let get_docblock_unsafe ~reader file =
    match get_docblock ~reader file with
    | Some docblock -> docblock
    | None -> raise (Docblock_not_found (File_key.to_string file))

  let get_exports_unsafe ~reader:_ file =
    match ExportsHeap.get file with
    | Some exports -> exports
    | None -> raise (Exports_not_found (File_key.to_string file))

  let get_tolerable_file_sig_unsafe ~reader file =
    match get_tolerable_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_file_sig_unsafe ~reader file =
    match get_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_type_sig_addr_unsafe ~reader file =
    match get_type_sig_addr ~reader file with
    | Some addr -> addr
    | None -> raise (Type_sig_not_found (File_key.to_string file))

  let get_type_sig_unsafe ~reader file =
    match get_type_sig ~reader file with
    | Some type_sig -> type_sig
    | None -> raise (Type_sig_not_found (File_key.to_string file))

  let get_file_hash_unsafe ~reader file =
    match get_file_hash ~reader file with
    | Some hash -> hash
    | None -> raise (Hash_not_found (File_key.to_string file))

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe
end

(* For use by a worker process *)
type worker_mutator = {
  add_file:
    File_key.t ->
    exports:Exports.t ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    File_sig.With_Loc.tolerable_t ->
    locs_tbl ->
    type_sig ->
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
let currently_oldified_files : FilenameSet.t ref = ref FilenameSet.empty

module Reparse_mutator : sig
  type master_mutator (* Used by the master process *)

  val create : Transaction.t -> FilenameSet.t -> master_mutator * worker_mutator

  val revive_files : master_mutator -> FilenameSet.t -> unit
end = struct
  type master_mutator = unit

  let commit () =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Committing parsing heaps";
        ParsingHeaps.remove_old_batch !currently_oldified_files;
        currently_oldified_files := FilenameSet.empty
    );
    Lwt.return_unit

  let rollback () =
    WorkerCancel.with_no_cancellations (fun () ->
        Hh_logger.debug "Rolling back parsing heaps";
        ParsingHeaps.revive_batch !currently_oldified_files;
        currently_oldified_files := FilenameSet.empty
    );
    Lwt.return_unit

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_file = ParsingHeaps.add

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_hash = FileHashHeap.add

  let create transaction files =
    WorkerCancel.with_no_cancellations (fun () ->
        currently_oldified_files := files;
        ParsingHeaps.oldify_batch files;
        Transaction.add ~singleton:"Reparse" ~commit ~rollback transaction;
        ((), { add_file; add_hash })
    )

  let revive_files () files =
    WorkerCancel.with_no_cancellations (fun () ->
        (* Every file in files should be in the oldified set *)
        assert (FilenameSet.is_empty (FilenameSet.diff files !currently_oldified_files));
        currently_oldified_files := FilenameSet.diff !currently_oldified_files files;
        ParsingHeaps.revive_batch files
    )
end

(* This peaks at the Reparse_mutator's state and uses that to determine whether to read from the
 * old or new heap. This is used by code outside of a init/recheck, like commands *)
module Reader : READER with type reader = State_reader.t = struct
  type reader = State_reader.t

  let should_use_oldified key = FilenameSet.mem key !currently_oldified_files

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

  let get_aloc_table ~reader:_ key =
    if should_use_oldified key then
      match TypeSigHeap.get_old key with
      | Some addr -> Some (read_aloc_table key addr)
      | None -> None
    else
      match ALocTableCache.get key with
      | Some _ as cached -> cached
      | None ->
        (match TypeSigHeap.get key with
        | Some addr ->
          let aloc_table = read_aloc_table key addr in
          ALocTableCache.add key aloc_table;
          Some aloc_table
        | None -> None)

  let get_type_sig_addr ~reader:_ key =
    if should_use_oldified key then
      TypeSigHeap.get_old key
    else
      TypeSigHeap.get key

  let get_docblock ~reader key =
    match get_type_sig_addr ~reader key with
    | Some addr -> Some (read_docblock addr)
    | None -> None

  let get_exports ~reader:_ key =
    if should_use_oldified key then
      ExportsHeap.get_old key
    else
      ExportsHeap.get key

  let get_tolerable_file_sig ~reader:_ key =
    if should_use_oldified key then
      FileSigHeap.get_old key
    else
      FileSigHeap.get key

  let get_file_sig ~reader key =
    match get_tolerable_file_sig ~reader key with
    | Some (file_sig, _tolerable_errors) -> Some file_sig
    | None -> None

  let get_type_sig ~reader key =
    let addr_opt = get_type_sig_addr ~reader key in
    Base.Option.map ~f:read_type_sig addr_opt

  let get_file_hash ~reader:_ key =
    if should_use_oldified key then
      FileHashHeap.get_old key
    else
      FileHashHeap.get key

  let get_ast_unsafe ~reader file =
    match get_ast ~reader file with
    | Some ast -> ast
    | None -> raise (Ast_not_found (File_key.to_string file))

  let get_aloc_table_unsafe ~reader file =
    match get_aloc_table ~reader file with
    | Some table -> table
    | None -> raise (ALoc_table_not_found (File_key.to_string file))

  let get_docblock_unsafe ~reader file =
    match get_docblock ~reader file with
    | Some docblock -> docblock
    | None -> raise (Docblock_not_found (File_key.to_string file))

  let get_exports_unsafe ~reader file =
    match get_exports ~reader file with
    | Some exports -> exports
    | None -> raise (Exports_not_found (File_key.to_string file))

  let get_tolerable_file_sig_unsafe ~reader file =
    match get_tolerable_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_file_sig_unsafe ~reader file =
    match get_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_type_sig_addr_unsafe ~reader file =
    match get_type_sig_addr ~reader file with
    | Some addr -> addr
    | None -> raise (Type_sig_not_found (File_key.to_string file))

  let get_type_sig_unsafe ~reader file =
    match get_type_sig ~reader file with
    | Some type_sig -> type_sig
    | None -> raise (Type_sig_not_found (File_key.to_string file))

  let get_file_hash_unsafe ~reader file =
    match get_file_hash ~reader file with
    | Some file_hash -> file_hash
    | None -> raise (Hash_not_found (File_key.to_string file))

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe
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

  let get_tolerable_file_sig ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_tolerable_file_sig ~reader
    | State_reader reader -> Reader.get_tolerable_file_sig ~reader

  let get_file_sig ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_sig ~reader
    | State_reader reader -> Reader.get_file_sig ~reader

  let get_type_sig ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_type_sig ~reader
    | State_reader reader -> Reader.get_type_sig ~reader

  let get_file_hash ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_hash ~reader
    | State_reader reader -> Reader.get_file_hash ~reader

  let get_ast_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_ast_unsafe ~reader
    | State_reader reader -> Reader.get_ast_unsafe ~reader

  let get_aloc_table_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_aloc_table_unsafe ~reader
    | State_reader reader -> Reader.get_aloc_table_unsafe ~reader

  let get_docblock_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_docblock_unsafe ~reader
    | State_reader reader -> Reader.get_docblock_unsafe ~reader

  let get_exports_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_exports_unsafe ~reader
    | State_reader reader -> Reader.get_exports_unsafe ~reader

  let get_tolerable_file_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_tolerable_file_sig_unsafe ~reader
    | State_reader reader -> Reader.get_tolerable_file_sig_unsafe ~reader

  let get_file_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_sig_unsafe ~reader
    | State_reader reader -> Reader.get_file_sig_unsafe ~reader

  let get_type_sig_addr_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_type_sig_addr_unsafe ~reader
    | State_reader reader -> Reader.get_type_sig_addr_unsafe ~reader

  let get_type_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_type_sig_unsafe ~reader
    | State_reader reader -> Reader.get_type_sig_unsafe ~reader

  let get_file_hash_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_hash_unsafe ~reader
    | State_reader reader -> Reader.get_file_hash_unsafe ~reader

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe
end

module From_saved_state : sig
  val add_file_sig : File_key.t -> File_sig.With_Loc.tolerable_t -> unit

  val add_file_hash : File_key.t -> Xx.hash -> unit

  val add_exports : File_key.t -> Exports.t -> unit
end = struct
  let add_file_sig = FileSigHeap.add

  let add_file_hash = FileHashHeap.add

  let add_exports = ExportsHeap.add
end
