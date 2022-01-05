(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
open Parsing_heaps_exceptions
module Heap = SharedMem.NewAPI

type info = {
  module_name: string option;
  checked: bool;  (** in flow? *)
  parsed: bool;  (** if false, it's a tracking record only *)
}

(** Maps filenames to info about a module, including the module's name.
    note: currently we may have many files for one module name. this is an issue. *)
module InfoHeap =
  SharedMem.WithCache
    (File_key)
    (struct
      type t = info

      let description = "Info"
    end)

module CheckedFileHeap =
  SharedMem.NoCacheAddr
    (File_key)
    (struct
      type t = Heap.checked_file
    end)

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type checked_file_addr = Heap.checked_file SharedMem.addr

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

(* Write parsed data for checked file to shared memory. If we loaded from saved
 * state, a checked file entry will already exist without parse data and this
 * function will update the existing entry in place. Otherwise, we will create a
 * new entry and add it to the shared hash table. *)
let add_checked_file file_key ast docblock locs type_sig file_sig exports =
  let open Type_sig_collections in
  let serialize x = Marshal.to_string x [] in
  let ast = serialize (compactify_loc ast) in
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
    match CheckedFileHeap.get file_key with
    | Some addr -> (size, (fun _ -> addr))
    | None ->
      let exports = serialize exports in
      let (exports_size, write_exports) = prepare_write_exports exports in
      let size = size + (2 * header_size) + exports_size + checked_file_size in
      let write chunk =
        let exports = write_exports chunk in
        write_checked_file chunk exports
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
  CheckedFileHeap.add file_key file_addr

let read_ast file_key file_addr =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_ast file_addr
  |> read_opt (fun addr -> read_ast addr |> deserialize |> decompactify_loc file_key)

let read_docblock file_addr =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_docblock file_addr |> read_opt (fun addr -> read_docblock addr |> deserialize)

let read_aloc_table file_key file_addr =
  let open Heap in
  let init = ALoc.ALocRepresentationDoNotUse.init_table file_key in
  get_file_aloc_table file_addr
  |> read_opt (fun addr -> read_aloc_table addr |> Packed_locs.unpack (Some file_key) init)

let read_type_sig file_addr =
  let open Heap in
  get_file_type_sig file_addr |> read_opt (fun addr -> read_type_sig addr Type_sig_bin.read)

let read_file_sig file_addr =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_sig file_addr |> read_opt (fun addr -> read_file_sig addr |> deserialize)

let read_exports file_addr : Exports.t =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_exports file_addr |> read_exports |> deserialize

(* Contains the hash for every file we even consider parsing *)
module FileHashHeap =
  SharedMem.NoCacheAddr
    (File_key)
    (struct
      type t = Heap.heap_int64
    end)

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
  let add_parsed file ~exports module_name docblock ast file_sig locs type_sig =
    let info = { module_name; checked = true; parsed = true } in
    WorkerCancel.with_no_cancellations (fun () ->
        InfoHeap.add file info;
        add_checked_file file ast docblock locs type_sig file_sig exports
    )

  let add_unparsed file module_name =
    let info = { module_name; checked = false; parsed = false } in
    WorkerCancel.with_no_cancellations (fun () -> InfoHeap.add file info)

  let add_hash file hash =
    let open Heap in
    let addr = alloc (header_size + int64_size) (fun chunk -> write_int64 chunk hash) in
    FileHashHeap.add file addr

  let oldify_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        CheckedFileHeap.oldify_batch files;
        FilenameSet.iter ASTCache.remove files;
        FilenameSet.iter ALocTableCache.remove files;
        FileHashHeap.oldify_batch files;
        InfoHeap.oldify_batch files
    )

  let remove_old_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        CheckedFileHeap.remove_old_batch files;
        FileHashHeap.remove_old_batch files;
        InfoHeap.remove_old_batch files
    )

  let revive_batch files =
    WorkerCancel.with_no_cancellations (fun () ->
        CheckedFileHeap.revive_batch files;
        FilenameSet.iter ASTCache.remove files;
        FilenameSet.iter ALocTableCache.remove files;
        FileHashHeap.revive_batch files;
        InfoHeap.revive_batch files
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

  val get_checked_file_addr_unsafe : reader:reader -> File_key.t -> checked_file_addr

  val get_type_sig_unsafe : reader:reader -> File_key.t -> type_sig

  val get_file_hash_unsafe : reader:reader -> File_key.t -> Xx.hash

  val loc_of_aloc : reader:reader -> ALoc.t -> Loc.t

  val get_info_unsafe : reader:reader -> (File_key.t -> info) Expensive.t

  val get_info : reader:reader -> (File_key.t -> info option) Expensive.t

  val is_tracked_file : reader:reader -> File_key.t -> bool
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

  val get_old_info : reader:reader -> (File_key.t -> info option) Expensive.t
end = struct
  type reader = Mutator_state_reader.t

  let ( let* ) = Option.bind

  let get_checked_file_addr ~reader:_ = CheckedFileHeap.get

  let has_ast ~reader file =
    match get_checked_file_addr ~reader file with
    | None -> false
    | Some addr -> Heap.get_file_ast addr |> Heap.is_some

  let get_ast ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_ast file addr

  let get_docblock ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_docblock addr

  let get_exports ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    Some (read_exports addr)

  let get_old_exports ~reader:_ file =
    let* addr = CheckedFileHeap.get_old file in
    Some (read_exports addr)

  let get_tolerable_file_sig ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_file_sig addr

  let get_file_sig ~reader file = Option.map fst (get_tolerable_file_sig ~reader file)

  let get_type_sig ~reader file =
    let* addr = get_checked_file_addr ~reader file in
    read_type_sig addr

  let get_file_hash ~reader:_ file =
    match FileHashHeap.get file with
    | Some addr -> Some (Heap.read_int64 addr)
    | None -> None

  let get_old_file_hash ~reader:_ file =
    match FileHashHeap.get_old file with
    | Some addr -> Some (Heap.read_int64 addr)
    | None -> None

  let get_ast_unsafe ~reader file =
    match get_ast ~reader file with
    | Some ast -> ast
    | None -> raise (Ast_not_found (File_key.to_string file))

  let get_aloc_table_unsafe ~reader file =
    match ALocTableCache.get file with
    | Some aloc_table -> aloc_table
    | None ->
      let aloc_table_opt =
        let* addr = get_checked_file_addr ~reader file in
        read_aloc_table file addr
      in
      (match aloc_table_opt with
      | Some aloc_table ->
        ALocTableCache.add file aloc_table;
        aloc_table
      | None -> raise (ALoc_table_not_found (File_key.to_string file)))

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

  let get_checked_file_addr_unsafe ~reader file =
    match get_checked_file_addr ~reader file with
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

  let get_info ~reader:_ = Expensive.wrap InfoHeap.get

  let get_old_info ~reader:_ = Expensive.wrap InfoHeap.get_old

  let get_info_unsafe ~reader ~audit f =
    match get_info ~reader ~audit f with
    | Some info -> info
    | None -> failwith (Printf.sprintf "module info not found for file %s" (File_key.to_string f))

  let is_tracked_file ~reader:_ = InfoHeap.mem
end

(* For use by a worker process *)
type worker_mutator = {
  add_parsed:
    File_key.t ->
    exports:Exports.t ->
    string option ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    File_sig.With_Loc.tolerable_t ->
    locs_tbl ->
    type_sig ->
    unit;
  add_unparsed: File_key.t -> string option -> unit;
  add_hash: File_key.t -> Xx.hash -> unit;
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator : sig
  val create : unit -> worker_mutator
end = struct
  let create () =
    {
      add_parsed = ParsingHeaps.add_parsed;
      add_unparsed = ParsingHeaps.add_unparsed;
      add_hash = ParsingHeaps.add_hash;
    }
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

  (* Ideally we'd assert that file was oldified and not revived, but it's too expensive to pass the
   * set of oldified files to the worker *)
  let add_hash = ParsingHeaps.add_hash

  let create transaction files =
    WorkerCancel.with_no_cancellations (fun () ->
        currently_oldified_files := files;
        ParsingHeaps.oldify_batch files;
        Transaction.add ~singleton:"Reparse" ~commit ~rollback transaction;
        ((), { add_parsed; add_unparsed; add_hash })
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

  let get_checked_file_addr ~reader:_ key =
    if should_use_oldified key then
      CheckedFileHeap.get_old key
    else
      CheckedFileHeap.get key

  let has_ast ~reader key =
    match get_checked_file_addr ~reader key with
    | None -> false
    | Some addr -> Heap.get_file_ast addr |> Heap.is_some

  let get_ast ~reader:_ key =
    if should_use_oldified key then
      let* addr = CheckedFileHeap.get_old key in
      read_ast key addr
    else
      match ASTCache.get key with
      | Some _ as cached -> cached
      | None ->
        let* addr = CheckedFileHeap.get key in
        let* ast = read_ast key addr in
        ASTCache.add key ast;
        Some ast

  let get_aloc_table ~reader:_ key =
    if should_use_oldified key then
      let* addr = CheckedFileHeap.get_old key in
      read_aloc_table key addr
    else
      match ALocTableCache.get key with
      | Some _ as cached -> cached
      | None ->
        let* addr = CheckedFileHeap.get key in
        let* aloc_table = read_aloc_table key addr in
        ALocTableCache.add key aloc_table;
        Some aloc_table

  let get_docblock ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    read_docblock addr

  let get_exports ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    Some (read_exports addr)

  let get_tolerable_file_sig ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    read_file_sig addr

  let get_file_sig ~reader key = Option.map fst (get_tolerable_file_sig ~reader key)

  let get_type_sig ~reader key =
    let* addr = get_checked_file_addr ~reader key in
    read_type_sig addr

  let get_file_hash ~reader:_ key =
    let addr_opt =
      if should_use_oldified key then
        FileHashHeap.get_old key
      else
        FileHashHeap.get key
    in
    Base.Option.map ~f:Heap.read_int64 addr_opt

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

  let get_checked_file_addr_unsafe ~reader file =
    match get_checked_file_addr ~reader file with
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

  let get_info ~reader:_ ~audit f =
    if should_use_oldified f then
      Expensive.wrap InfoHeap.get_old ~audit f
    else
      Expensive.wrap InfoHeap.get ~audit f

  let get_info_unsafe ~reader ~audit f =
    match get_info ~reader ~audit f with
    | Some info -> info
    | None -> failwith (Printf.sprintf "module info not found for file %s" (File_key.to_string f))

  let is_tracked_file ~reader:_ f =
    if should_use_oldified f then
      InfoHeap.mem_old f
    else
      InfoHeap.mem f
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

  let get_checked_file_addr_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_checked_file_addr_unsafe ~reader
    | State_reader reader -> Reader.get_checked_file_addr_unsafe ~reader

  let get_type_sig_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_type_sig_unsafe ~reader
    | State_reader reader -> Reader.get_type_sig_unsafe ~reader

  let get_file_hash_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_file_hash_unsafe ~reader
    | State_reader reader -> Reader.get_file_hash_unsafe ~reader

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe

  let get_info ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_info ~reader
    | State_reader reader -> Reader.get_info ~reader

  let get_info_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_info_unsafe ~reader
    | State_reader reader -> Reader.get_info_unsafe ~reader

  let is_tracked_file ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.is_tracked_file ~reader
    | State_reader reader -> Reader.is_tracked_file ~reader
end

module From_saved_state : sig
  val add_file_hash : File_key.t -> Xx.hash -> unit

  val add_info : File_key.t -> info -> unit

  val add_exports : File_key.t -> Exports.t -> unit
end = struct
  let add_file_hash = ParsingHeaps.add_hash

  let add_info = InfoHeap.add

  let add_exports file_key exports =
    let exports = Marshal.to_string exports [] in
    let (exports_size, write_exports) = Heap.prepare_write_exports exports in
    let open Heap in
    let size = (2 * header_size) + exports_size + checked_file_size in
    let file_addr =
      alloc size (fun chunk ->
          let exports = write_exports chunk in
          write_checked_file chunk exports
      )
    in
    CheckedFileHeap.add file_key file_addr
end
