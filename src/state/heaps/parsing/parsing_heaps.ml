(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Parsing_heaps_exceptions
module Heap = SharedMem.NewAPI

module FileHeap =
  SharedMem.NoCacheAddr
    (File_key)
    (struct
      type t = Heap.dyn_file
    end)

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = Heap.dyn_file SharedMem.addr

type checked_file_addr = Heap.checked_file SharedMem.addr

type unparsed_file_addr = Heap.unparsed_file SharedMem.addr

(* Write parsed data for checked file to shared memory. If we loaded from saved
 * state, a checked file entry will already exist without parse data and this
 * function will update the existing entry in place. Otherwise, we will create a
 * new entry and add it to the shared hash table. *)
let add_checked_file file_key hash module_name docblock ast locs type_sig file_sig exports =
  let open Type_sig_collections in
  let serialize x = Marshal.to_string x [] in
  let ast = Flow_ast_marshal.serialize_ast ast in
  let docblock = serialize docblock in
  let file_sig = serialize file_sig in
  let aloc_table = Packed_locs.pack (Locs.length locs) (fun f -> Locs.iter f locs) in
  let (sig_bsize, write_sig) = Type_sig_bin.write type_sig in
  let (file_sig_size, write_file_sig) = Heap.prepare_write_file_sig file_sig in
  let (ast_size, write_ast) = Heap.prepare_write_ast ast in
  let open Heap in
  let size =
    (5 * header_size)
    + ast_size
    + docblock_size docblock
    + aloc_table_size aloc_table
    + type_sig_size sig_bsize
    + file_sig_size
  in
  let (size, write_file_maybe) =
    match FileHeap.get file_key with
    | Some addr ->
      (* If there is an existing file heap entry, it must be a checked file. How
       * do we know this? We should only hit this case via `ensure_parsed` and
       * we only call `ensure_parsed` on checked files. *)
      let addr = assert_checked_file addr in
      (size, Fun.const addr)
    | None ->
      let exports = serialize exports in
      let (exports_size, write_exports) = prepare_write_exports exports in
      let size =
        size
        + (3 * header_size)
        + checked_file_size
        + int64_size
        + opt_size (with_header_size string_size) module_name
        + exports_size
      in
      let write chunk =
        let hash = write_int64 chunk hash in
        let module_name = write_opt write_string chunk module_name in
        let exports = write_exports chunk in
        write_checked_file chunk hash module_name exports
      in
      (size, write)
  in
  let file_addr =
    alloc size (fun chunk ->
        let file = write_file_maybe chunk in
        let ast = write_ast chunk in
        let docblock = write_docblock chunk docblock in
        let aloc_table = write_aloc_table chunk aloc_table in
        let type_sig = write_type_sig chunk sig_bsize write_sig in
        let file_sig = write_file_sig chunk in
        set_file_ast file ast;
        set_file_docblock file docblock;
        set_file_aloc_table file aloc_table;
        set_file_type_sig file type_sig;
        set_file_sig file file_sig;
        file
    )
  in
  FileHeap.add file_key (dyn_checked_file file_addr)

let add_unparsed_file file_key hash module_name =
  let open Heap in
  let size =
    (2 * header_size)
    + unparsed_file_size
    + int64_size
    + opt_size (with_header_size string_size) module_name
  in
  let addr =
    alloc size (fun chunk ->
        let hash = write_int64 chunk hash in
        let module_name = write_opt write_string chunk module_name in
        write_unparsed_file chunk hash module_name
    )
  in
  FileHeap.add file_key (dyn_unparsed_file addr)

let is_checked_file = Heap.is_checked_file

let coerce_checked_file = Heap.coerce_checked_file

let read_file_hash file_addr =
  let open Heap in
  get_file_hash file_addr |> read_int64

let read_checked_file_hash file_addr = Heap.dyn_checked_file file_addr |> read_file_hash

let read_unparsed_file_hash file_addr = Heap.dyn_unparsed_file file_addr |> read_file_hash

let read_module_name file_addr =
  let open Heap in
  get_file_module_name file_addr |> read_opt read_string

let read_checked_module_name file_addr = Heap.dyn_checked_file file_addr |> read_module_name

let read_unparsed_module_name file_addr = Heap.dyn_unparsed_file file_addr |> read_module_name

let read_ast file_key file_addr =
  let open Heap in
  get_file_ast file_addr
  |> read_opt (fun addr -> Flow_ast_marshal.deserialize_ast (read_ast addr) file_key)

let read_ast_unsafe file_key file_addr =
  match read_ast file_key file_addr with
  | Some ast -> ast
  | None -> raise (Ast_not_found (File_key.to_string file_key))

let read_docblock file_addr =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_docblock file_addr |> read_opt (fun addr -> read_docblock addr |> deserialize)

let read_docblock_unsafe file_key file_addr =
  match read_docblock file_addr with
  | Some docblock -> docblock
  | None -> raise (Docblock_not_found (File_key.to_string file_key))

let read_aloc_table file_key file_addr =
  let open Heap in
  let init = ALoc.ALocRepresentationDoNotUse.init_table file_key in
  get_file_aloc_table file_addr
  |> read_opt (fun addr -> read_aloc_table addr |> Packed_locs.unpack (Some file_key) init)

let read_aloc_table_unsafe file_key file_addr =
  match read_aloc_table file_key file_addr with
  | Some aloc_table -> aloc_table
  | None -> raise (ALoc_table_not_found (File_key.to_string file_key))

let read_type_sig file_addr =
  let open Heap in
  get_file_type_sig file_addr |> read_opt (fun addr -> read_type_sig addr Type_sig_bin.read)

let read_type_sig_unsafe file_key file_addr =
  match read_type_sig file_addr with
  | Some type_sig -> type_sig
  | None -> raise (Type_sig_not_found (File_key.to_string file_key))

let read_tolerable_file_sig file_addr =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_sig file_addr |> read_opt (fun addr -> read_file_sig addr |> deserialize)

let read_file_sig file_addr = Option.map fst (read_tolerable_file_sig file_addr)

let read_tolerable_file_sig_unsafe file_key file_addr =
  match read_tolerable_file_sig file_addr with
  | Some file_sig -> file_sig
  | None -> raise (Requires_not_found (File_key.to_string file_key))

let read_file_sig_unsafe file_key file_addr = fst (read_tolerable_file_sig_unsafe file_key file_addr)

let read_exports file_addr : Exports.t =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_exports file_addr |> read_exports |> deserialize

module ASTCache = SharedMem.LocalCache (struct
  type key = File_key.t

  type value = (Loc.t, Loc.t) Flow_ast.Program.t

  let capacity = 1000
end)

module ALocTableCache = SharedMem.LocalCache (struct
  type key = File_key.t

  type value = ALoc.table

  let capacity = 1000
end)

(* Groups operations on the multiple heaps that need to stay in sync *)
module ParsingHeaps = struct
  let add_parsed file ~exports hash module_name docblock ast file_sig locs type_sig =
    WorkerCancel.with_no_cancellations (fun () ->
        add_checked_file file hash module_name docblock ast locs type_sig file_sig exports
    )

  let add_unparsed file hash module_name =
    WorkerCancel.with_no_cancellations (fun () -> add_unparsed_file file hash module_name)

  let oldify_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        FileHeap.oldify_batch files;
        FilenameSet.iter ASTCache.remove files;
        FilenameSet.iter ALocTableCache.remove files
    )

  let remove_old_batch files =
    WorkerCancel.with_no_cancellations (fun () -> FileHeap.remove_old_batch files)

  let revive_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        FileHeap.revive_batch files;
        FilenameSet.iter ASTCache.remove files;
        FilenameSet.iter ALocTableCache.remove files
    )
end

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
module Mutator_reader = struct
  type reader = Mutator_state_reader.t

  let ( let* ) = Option.bind

  let get_file_addr ~reader:_ = FileHeap.get

  let get_old_file_addr ~reader:_ = FileHeap.get_old

  let get_checked_file_addr ~reader file =
    let* addr = get_file_addr ~reader file in
    coerce_checked_file addr

  let get_old_checked_file_addr ~reader file =
    let* addr = get_old_file_addr ~reader file in
    coerce_checked_file addr

  let has_ast ~reader file =
    match get_checked_file_addr ~reader file with
    | None -> false
    | Some addr -> Heap.get_file_ast addr |> Heap.is_some

  let get_ast ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_ast file addr

  let get_aloc_table ~reader file =
    match ALocTableCache.get file with
    | Some _ as cached -> cached
    | None ->
      let* addr = get_checked_file_addr ~reader file in
      let* aloc_table = read_aloc_table file addr in
      ALocTableCache.add file aloc_table;
      Some aloc_table

  let get_docblock ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_docblock addr

  let get_exports ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    Some (read_exports addr)

  let get_old_exports ~reader file =
    let* addr = get_old_checked_file_addr ~reader file in
    Some (read_exports addr)

  let get_tolerable_file_sig ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_tolerable_file_sig addr

  let get_file_sig ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_file_sig addr

  let get_type_sig ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_type_sig addr

  let get_file_hash ~reader file =
    let* addr = get_file_addr ~reader file in
    Some (read_file_hash addr)

  let get_old_file_hash ~reader file =
    let* addr = get_old_file_addr ~reader file in
    Some (read_file_hash addr)

  let get_file_addr_unsafe ~reader file =
    match get_file_addr ~reader file with
    | Some addr -> addr
    | None -> raise (File_not_found (File_key.to_string file))

  let get_checked_file_addr_unsafe ~reader file =
    let addr = get_file_addr_unsafe ~reader file in
    Heap.assert_checked_file addr

  let get_unparsed_file_addr_unsafe ~reader file =
    let addr = get_file_addr_unsafe ~reader file in
    Heap.assert_unparsed_file addr

  let get_ast_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_ast_unsafe file addr

  let get_aloc_table_unsafe ~reader file =
    match ALocTableCache.get file with
    | Some aloc_table -> aloc_table
    | None ->
      let addr = get_checked_file_addr_unsafe ~reader file in
      let aloc_table = read_aloc_table_unsafe file addr in
      ALocTableCache.add file aloc_table;
      aloc_table

  let get_docblock_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_docblock_unsafe file addr

  let get_exports_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_exports addr

  let get_tolerable_file_sig_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_tolerable_file_sig_unsafe file addr

  let get_file_sig_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_file_sig_unsafe file addr

  let get_type_sig_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_type_sig_unsafe file addr

  let get_file_hash_unsafe ~reader file =
    let addr = get_file_addr_unsafe ~reader file in
    read_file_hash addr

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe
end

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

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator : sig
  val create : unit -> worker_mutator
end = struct
  let create () = { add_parsed = ParsingHeaps.add_parsed; add_unparsed = ParsingHeaps.add_unparsed }
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
  let add_parsed = ParsingHeaps.add_parsed

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_unparsed = ParsingHeaps.add_unparsed

  let create transaction files =
    WorkerCancel.with_no_cancellations (fun () ->
        currently_oldified_files := files;
        ParsingHeaps.oldify_batch files;
        Transaction.add ~singleton:"Reparse" ~commit ~rollback transaction;
        ((), { add_parsed; add_unparsed })
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

  let ( let* ) = Option.bind

  let should_use_oldified key = FilenameSet.mem key !currently_oldified_files

  let get_file_addr ~reader:_ key =
    if should_use_oldified key then
      FileHeap.get_old key
    else
      FileHeap.get key

  let get_checked_file_addr ~reader key =
    let* addr = get_file_addr ~reader key in
    coerce_checked_file addr

  let has_ast ~reader key =
    match get_checked_file_addr ~reader key with
    | None -> false
    | Some addr -> Heap.get_file_ast addr |> Heap.is_some

  let get_ast ~reader key =
    if should_use_oldified key then
      let* addr = get_checked_file_addr ~reader key in
      read_ast key addr
    else
      match ASTCache.get key with
      | Some _ as cached -> cached
      | None ->
        let* addr = get_checked_file_addr ~reader key in
        let* ast = read_ast key addr in
        ASTCache.add key ast;
        Some ast

  let get_aloc_table ~reader file =
    if should_use_oldified file then
      let* addr = get_checked_file_addr ~reader file in
      read_aloc_table file addr
    else
      match ALocTableCache.get file with
      | Some _ as cached -> cached
      | None ->
        let* addr = get_checked_file_addr ~reader file in
        let* aloc_table = read_aloc_table file addr in
        ALocTableCache.add file aloc_table;
        Some aloc_table

  let get_docblock ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    read_docblock addr

  let get_exports ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    Some (read_exports addr)

  let get_tolerable_file_sig ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    read_tolerable_file_sig addr

  let get_file_sig ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    read_file_sig addr

  let get_type_sig ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    read_type_sig addr

  let get_file_hash ~reader key =
    match get_file_addr ~reader key with
    | Some addr -> Some (read_file_hash addr)
    | None -> None

  let get_file_addr_unsafe ~reader file =
    match get_file_addr ~reader file with
    | Some addr -> addr
    | None -> raise (File_not_found (File_key.to_string file))

  let get_checked_file_addr_unsafe ~reader file =
    let addr = get_file_addr_unsafe ~reader file in
    Heap.assert_checked_file addr

  let get_unparsed_file_addr_unsafe ~reader file =
    let addr = get_file_addr_unsafe ~reader file in
    Heap.assert_unparsed_file addr

  let get_ast_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_ast_unsafe file addr

  let get_aloc_table_unsafe ~reader file =
    if should_use_oldified file then
      let addr = get_checked_file_addr_unsafe ~reader file in
      read_aloc_table_unsafe file addr
    else
      match ALocTableCache.get file with
      | Some aloc_table -> aloc_table
      | None ->
        let addr = get_checked_file_addr_unsafe ~reader file in
        let aloc_table = read_aloc_table_unsafe file addr in
        ALocTableCache.add file aloc_table;
        aloc_table

  let get_docblock_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_docblock_unsafe file addr

  let get_exports_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_exports addr

  let get_tolerable_file_sig_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_tolerable_file_sig_unsafe file addr

  let get_file_sig_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_file_sig_unsafe file addr

  let get_type_sig_unsafe ~reader file =
    let addr = get_checked_file_addr_unsafe ~reader file in
    read_docblock_unsafe file addr

  let get_file_hash_unsafe ~reader file =
    let addr = get_file_addr_unsafe ~reader file in
    read_file_hash addr

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe
end

(* Reader_dispatcher is used by code which may or may not be running inside an init/recheck *)
module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let get_file_addr ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_addr ~reader
    | State_reader reader -> Reader.get_file_addr ~reader

  let get_checked_file_addr ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_checked_file_addr ~reader
    | State_reader reader -> Reader.get_checked_file_addr ~reader

  let has_ast ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.has_ast ~reader
    | State_reader reader -> Reader.has_ast ~reader

  let get_ast ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_ast ~reader
    | State_reader reader -> Reader.get_ast ~reader

  let get_aloc_table ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_aloc_table ~reader
    | State_reader reader -> Reader.get_aloc_table ~reader

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

  let get_file_addr_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_addr_unsafe ~reader
    | State_reader reader -> Reader.get_file_addr_unsafe ~reader

  let get_checked_file_addr_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_checked_file_addr_unsafe ~reader
    | State_reader reader -> Reader.get_checked_file_addr_unsafe ~reader

  let get_unparsed_file_addr_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_unparsed_file_addr_unsafe ~reader
    | State_reader reader -> Reader.get_unparsed_file_addr_unsafe ~reader

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
  val add_parsed : File_key.t -> Xx.hash -> string option -> Exports.t -> unit

  val add_unparsed : File_key.t -> Xx.hash -> string option -> unit
end = struct
  let add_parsed file_key hash module_name exports =
    let exports = Marshal.to_string exports [] in
    let (exports_size, write_exports) = Heap.prepare_write_exports exports in
    let open Heap in
    let size =
      (3 * header_size)
      + checked_file_size
      + int64_size
      + opt_size (with_header_size string_size) module_name
      + exports_size
    in
    let addr =
      alloc size (fun chunk ->
          let hash = write_int64 chunk hash in
          let module_name = write_opt write_string chunk module_name in
          let exports = write_exports chunk in
          write_checked_file chunk hash module_name exports
      )
    in
    FileHeap.add file_key (dyn_checked_file addr)

  let add_unparsed = ParsingHeaps.add_unparsed
end
