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

let write_type_sig locs type_sig =
  let open Type_sig_collections in
  let {
    Packed_type_sig.Module.module_kind;
    module_refs;
    local_defs;
    remote_refs;
    pattern_defs;
    patterns;
  } =
    type_sig
  in
  let serialize x = Marshal.to_string x [] in
  let aloc_table = Packed_locs.pack (Locs.length locs) (fun f -> Locs.iter f locs) in
  let module_refs = Module_refs.to_array module_refs in
  let local_defs = Local_defs.to_array_map serialize local_defs in
  let remote_refs = Remote_refs.to_array_map serialize remote_refs in
  let pattern_defs = Pattern_defs.to_array_map serialize pattern_defs in
  let patterns = Patterns.to_array_map serialize patterns in
  let open Heap in
  let array_size f xs =
    if Array.length xs = 0 then
      0
    else
      let size = header_size + addr_tbl_size xs in
      Array.fold_left (fun size x -> size + header_size + f x) size xs
  in
  let opt_size f = Base.Option.value_map ~default:0 ~f:(fun x -> header_size + f x) in
  let (module_size, write_module) =
    match module_kind with
    | Type_sig_pack.CJSModule { type_exports; exports; info } ->
      let type_exports = Array.map serialize type_exports in
      let exports = Option.map serialize exports in
      let info = serialize info in
      let size =
        (2 * header_size)
        + cjs_module_size
        + cjs_module_info_size info
        + opt_size cjs_exports_size exports
        + array_size type_export_size type_exports
      in
      let write chunk =
        let info = write_cjs_module_info chunk info in
        let exports = write_opt write_cjs_exports chunk exports in
        let type_exports = write_addr_tbl write_type_export chunk type_exports in
        let cjs_module = write_cjs_module chunk info exports type_exports in
        dyn_cjs_module cjs_module
      in
      (size, write)
    | Type_sig_pack.ESModule { type_exports; exports; info } ->
      let type_exports = Array.map serialize type_exports in
      let exports = Array.map serialize exports in
      let info = serialize info in
      let size =
        (2 * header_size)
        + es_module_size
        + es_module_info_size info
        + array_size es_export_size exports
        + array_size type_export_size type_exports
      in
      let write chunk =
        let info = write_es_module_info chunk info in
        let exports = write_addr_tbl write_es_export chunk exports in
        let type_exports = write_addr_tbl write_type_export chunk type_exports in
        let es_module = write_es_module chunk info exports type_exports in
        dyn_es_module es_module
      in
      (size, write)
  in
  let size =
    (2 * header_size)
    + checked_file_size
    + aloc_table_size aloc_table
    + module_size
    + array_size module_ref_size module_refs
    + array_size local_def_size local_defs
    + array_size remote_ref_size remote_refs
    + array_size pattern_def_size pattern_defs
    + array_size pattern_size patterns
  in
  alloc size (fun chunk ->
      let aloc_table = write_aloc_table chunk aloc_table in
      let dyn_module = write_module chunk in
      let module_refs = write_addr_tbl write_module_ref chunk module_refs in
      let local_defs = write_addr_tbl write_local_def chunk local_defs in
      let remote_refs = write_addr_tbl write_remote_ref chunk remote_refs in
      let pattern_defs = write_addr_tbl write_pattern_def chunk pattern_defs in
      let patterns = write_addr_tbl write_pattern chunk patterns in
      write_checked_file
        chunk
        aloc_table
        dyn_module
        module_refs
        local_defs
        remote_refs
        pattern_defs
        patterns)

let read_aloc_table file_key file_addr =
  let open Heap in
  let init = ALoc.ALocRepresentationDoNotUse.init_table file_key in
  file_aloc_table file_addr |> read_aloc_table |> Packed_locs.unpack (Some file_key) init

let read_type_sig file =
  let open Type_sig_collections in
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  let module_refs =
    let addr = file_module_refs file in
    read_addr_tbl_generic read_module_ref addr Module_refs.init
  in
  let module_kind =
    let cjs_module addr =
      let type_exports =
        let f addr = deserialize (read_type_export addr) in
        let addr = cjs_module_type_exports addr in
        read_addr_tbl f addr
      in
      let exports =
        let f addr = deserialize (read_cjs_exports addr) in
        let addr = cjs_module_exports addr in
        read_opt f addr
      in
      let info =
        let addr = cjs_module_info addr in
        deserialize (read_cjs_module_info addr)
      in
      Type_sig_pack.CJSModule { type_exports; exports; info }
    in
    let es_module addr =
      let type_exports =
        let f addr = deserialize (read_type_export addr) in
        let addr = es_module_type_exports addr in
        read_addr_tbl f addr
      in
      let exports =
        let f addr = deserialize (read_es_export addr) in
        let addr = es_module_exports addr in
        read_addr_tbl f addr
      in
      let info =
        let addr = es_module_info addr in
        deserialize (read_es_module_info addr)
      in
      Type_sig_pack.ESModule { type_exports; exports; info }
    in
    let addr = file_module file in
    read_dyn_module cjs_module es_module addr
  in
  let local_defs =
    let f addr = deserialize (read_local_def addr) in
    let addr = file_local_defs file in
    read_addr_tbl_generic f addr Local_defs.init
  in
  let remote_refs =
    let f addr = deserialize (read_remote_ref addr) in
    let addr = file_remote_refs file in
    read_addr_tbl_generic f addr Remote_refs.init
  in
  let pattern_defs =
    let f addr = deserialize (read_pattern_def addr) in
    let addr = file_pattern_defs file in
    read_addr_tbl_generic f addr Pattern_defs.init
  in
  let patterns =
    let f addr = deserialize (read_pattern addr) in
    let addr = file_patterns file in
    read_addr_tbl_generic f addr Patterns.init
  in
  {
    Packed_type_sig.Module.module_kind;
    module_refs;
    local_defs;
    remote_refs;
    pattern_defs;
    patterns;
  }

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

module DocblockHeap =
  SharedMem.NoCache
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
        DocblockHeap.add file info;
        ExportsHeap.add file exports;
        FileSigHeap.add file file_sig;
        TypeSigHeap.add file (write_type_sig locs type_sig))

  let oldify_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.oldify_batch files;
        TypeSigHeap.oldify_batch files;
        FilenameSet.iter ALocTableCache.remove files;
        DocblockHeap.oldify_batch files;
        ExportsHeap.oldify_batch files;
        FileSigHeap.oldify_batch files;
        FileHashHeap.oldify_batch files)

  let remove_old_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.remove_old_batch files;
        TypeSigHeap.remove_old_batch files;
        DocblockHeap.remove_old_batch files;
        ExportsHeap.remove_old_batch files;
        FileSigHeap.remove_old_batch files;
        FileHashHeap.remove_old_batch files)

  let revive_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        ASTHeap.revive_batch files;
        TypeSigHeap.revive_batch files;
        FilenameSet.iter ALocTableCache.remove files;
        DocblockHeap.revive_batch files;
        ExportsHeap.revive_batch files;
        FileSigHeap.revive_batch files;
        FileHashHeap.revive_batch files)
end

module type READER = sig
  type reader

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_type_sig : reader:reader -> File_key.t -> type_sig option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_ast_unsafe : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t

  val get_aloc_table_unsafe : reader:reader -> File_key.t -> ALoc.table

  val get_docblock_unsafe : reader:reader -> File_key.t -> Docblock.t

  val get_exports_unsafe : reader:reader -> File_key.t -> Exports.t

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
       get_aloc_table_unsafe ~reader source)
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

  let get_docblock ~reader:_ = DocblockHeap.get

  let get_exports ~reader:_ = ExportsHeap.get

  let get_old_exports ~reader:_ = ExportsHeap.get_old

  let get_file_sig ~reader:_ = FileSigHeap.get

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

  let get_file_sig_unsafe ~reader file =
    match get_file_sig ~reader file with
    | Some file_sig -> file_sig
    | None -> raise (Requires_not_found (File_key.to_string file))

  let get_type_sig_addr_unsafe ~reader:_ file =
    match TypeSigHeap.get file with
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
    File_sig.With_Loc.t ->
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
  let add_file file ~exports info ast file_sig sig_opt =
    ParsingHeaps.add file ~exports info ast file_sig sig_opt

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

  let get_type_sig_addr ~reader:_ key =
    if should_use_oldified key then
      TypeSigHeap.get_old key
    else
      TypeSigHeap.get key

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
  val add_file_sig : File_key.t -> File_sig.With_Loc.t -> unit

  val add_file_hash : File_key.t -> Xx.hash -> unit

  val add_exports : File_key.t -> Exports.t -> unit
end = struct
  let add_file_sig = FileSigHeap.add

  let add_file_hash = FileHashHeap.add

  let add_exports = ExportsHeap.add
end
