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
      type t = Heap.dyn_file Heap.entity
    end)

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = Heap.dyn_file SharedMem.addr

type checked_file_addr = Heap.checked_file SharedMem.addr

type unparsed_file_addr = Heap.unparsed_file SharedMem.addr

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
let add_checked_file file_key hash module_name docblock ast locs type_sig file_sig exports =
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
  let saved_state_or_fresh_parse =
    match FileHeap.get file_key with
    | Some file ->
      (* If we loaded from a saved state, we will have some existing data with a
       * matching hash. In this case, we want to update the existing data with
       * parse information. *)
      let f addr =
        let recorded_hash = read_int64 (get_file_hash addr) in
        if Int64.equal recorded_hash hash then
          (* We know that file is checked (we are in add_checked_file) and the
           * existing record's hash matches, so the file must have been checked
           * before as well. *)
          Some (assert_checked_file addr)
        else
          None
      in
      (match read_opt_bind f (entity_read_latest file) with
      | Some data -> Either.Left data
      | None ->
        let write _ data = entity_advance file (Some data) in
        Either.Right (size, write))
    | None ->
      let size = size + header_size + entity_size in
      let write chunk data =
        let file = write_entity chunk (Some data) in
        assert (file = FileHeap.add file_key file)
      in
      Either.Right (size, write)
  in
  let (size, add_file_maybe) =
    match saved_state_or_fresh_parse with
    | Either.Left existing_data -> (size, Fun.const existing_data)
    | Either.Right (size, add_file_maybe) ->
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
        let module_name = Option.map (write_string chunk) module_name in
        let exports = write_exports chunk in
        let data = write_checked_file chunk hash module_name exports in
        add_file_maybe chunk (dyn_checked_file data);
        data
      in
      (size, write)
  in
  alloc size (fun chunk ->
      let data = add_file_maybe chunk in
      let ast = write_ast chunk in
      let docblock = write_docblock chunk docblock in
      let aloc_table = write_aloc_table chunk aloc_table in
      let type_sig = write_type_sig chunk sig_bsize write_sig in
      let file_sig = write_file_sig chunk in
      set_file_ast data ast;
      set_file_docblock data docblock;
      set_file_aloc_table data aloc_table;
      set_file_type_sig data type_sig;
      set_file_sig data file_sig
  )

let add_unparsed_file file_key hash module_name =
  let open Heap in
  let size =
    (2 * header_size)
    + unparsed_file_size
    + int64_size
    + opt_size (with_header_size string_size) module_name
  in
  let (size, write_file_maybe) =
    match FileHeap.get file_key with
    | Some file ->
      let write _chunk data =
        entity_advance file (Some data);
        file
      in
      (size, write)
    | None ->
      let size = size + header_size + entity_size in
      let write chunk data = write_entity chunk (Some data) in
      (size, write)
  in
  let file =
    alloc size (fun chunk ->
        let hash = write_int64 chunk hash in
        let module_name = Option.map (write_string chunk) module_name in
        let data = write_unparsed_file chunk hash module_name in
        write_file_maybe chunk (dyn_unparsed_file data)
    )
  in
  assert (file = FileHeap.add file_key file)

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
  let deserialize x = Marshal.from_string x 0 in
  get_file_ast file_addr
  |> read_opt (fun addr -> read_ast addr |> deserialize |> decompactify_loc file_key)

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

module Reader_cache : sig
  val get_ast : File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val add_ast : File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t -> unit

  val get_aloc_table : File_key.t -> ALoc.table option

  val add_aloc_table : File_key.t -> ALoc.table -> unit

  val remove_batch : FilenameSet.t -> unit
end = struct
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

  let get_ast = ASTCache.get

  let add_ast = ASTCache.add

  let get_aloc_table = ALocTableCache.get

  let add_aloc_table = ALocTableCache.add

  let remove file =
    ASTCache.remove file;
    ALocTableCache.remove file

  let remove_batch files = FilenameSet.iter remove files
end

module Mutator_cache : sig
  val get_aloc_table : File_key.t -> ALoc.table option

  val add_aloc_table : File_key.t -> ALoc.table -> unit

  val clear : unit -> unit
end = struct
  module ALocTableCache = SharedMem.LocalCache (struct
    type key = File_key.t

    type value = ALoc.table

    let capacity = 1000
  end)

  let get_aloc_table = ALocTableCache.get

  let add_aloc_table = ALocTableCache.add

  let clear = ALocTableCache.clear
end

let add_parsed file ~exports hash module_name docblock ast file_sig locs type_sig =
  WorkerCancel.with_no_cancellations (fun () ->
      add_checked_file file hash module_name docblock ast locs type_sig file_sig exports
  )

let add_unparsed file hash module_name =
  WorkerCancel.with_no_cancellations (fun () -> add_unparsed_file file hash module_name)

let clear_not_found file_key =
  WorkerCancel.with_no_cancellations (fun () ->
      match FileHeap.get file_key with
      | Some file -> Heap.entity_advance file None
      | None -> ()
  )

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

let loc_of_aloc ~reader ~get_aloc_table_unsafe aloc =
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

  let get_file_addr ~reader:_ file_key =
    let* file = FileHeap.get file_key in
    Heap.(read_opt Fun.id (entity_read_latest file))

  let get_old_file_addr ~reader:_ file_key =
    let* file = FileHeap.get file_key in
    let* data = Heap.(read_opt Fun.id (entity_read_committed file)) in
    Some data

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
    match Mutator_cache.get_aloc_table file with
    | Some _ as cached -> cached
    | None ->
      let* addr = get_checked_file_addr ~reader file in
      let* aloc_table = read_aloc_table file addr in
      Mutator_cache.add_aloc_table file aloc_table;
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
    match Mutator_cache.get_aloc_table file with
    | Some aloc_table -> aloc_table
    | None ->
      let addr = get_checked_file_addr_unsafe ~reader file in
      let aloc_table = read_aloc_table_unsafe file addr in
      Mutator_cache.add_aloc_table file aloc_table;
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
  clear_not_found: File_key.t -> unit;
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator : sig
  val create : unit -> worker_mutator
end = struct
  let create () =
    let clear_not_found _ = () in
    { add_parsed; add_unparsed; clear_not_found }
end

(* Reparsing is more complicated than parsing, since we need to worry about transactions.
 *
 * Modified files will be advanced based on the current transaction. Advancing a
 * file ensures that the previous committed data is still available. Committing
 * the transaction will publish the new data to all readers by bumping the
 * global transaction counter, see Mutator_state_reader.
 *
 * If the transaction is rolled back, we will revert changed entities. Ideally,
 * we would not need to roll back / undo any writes when a transaction rolls
 * back, assuming the next recheck is guaranteed to contain a superset of this
 * recheck's files.
 *
 * This assumption does not hold for priority rechecks, where we cancel a
 * recheck, schedule a minimal recheck to unblock the IDE request, then
 * re-start the original recheck.
 *)
module Reparse_mutator : sig
  type master_mutator (* Used by the master process *)

  val create : Transaction.t -> FilenameSet.t -> master_mutator * worker_mutator

  val record_unchanged : master_mutator -> FilenameSet.t -> unit

  val record_not_found : master_mutator -> FilenameSet.t -> unit
end = struct
  type master_mutator = unit

  (* We can conservatively invalidate caches for all `files`, but we can be a
   * bit more precise by only invalidating changed files. If parsing reveals
   * unchanged files, we remove them from this set. *)
  let changed_files = ref FilenameSet.empty

  (* When the transaction commits, we will remove these from the shared hash
   * table, so sharedmem GC can collect them. *)
  let not_found_files = ref FilenameSet.empty

  let rollback_changed () =
    FilenameSet.iter (fun key -> Option.iter Heap.entity_rollback (FileHeap.get key)) !changed_files

  let reset_refs () =
    changed_files := FilenameSet.empty;
    not_found_files := FilenameSet.empty

  let create transaction files =
    changed_files := files;

    let commit () =
      WorkerCancel.with_no_cancellations (fun () ->
          Hh_logger.debug "Committing parsing heaps";
          Mutator_cache.clear ();
          Reader_cache.remove_batch !changed_files;
          FileHeap.remove_batch !not_found_files;
          reset_refs ()
      );
      Lwt.return_unit
    in

    let rollback () =
      WorkerCancel.with_no_cancellations (fun () ->
          Hh_logger.debug "Rolling back parsing heaps";
          Mutator_cache.clear ();
          rollback_changed ();
          reset_refs ()
      );
      Lwt.return_unit
    in

    Transaction.add ~singleton:"Reparse" ~commit ~rollback transaction;

    ((), { add_parsed; add_unparsed; clear_not_found })

  let record_unchanged () unchanged = changed_files := FilenameSet.diff !changed_files unchanged

  let record_not_found () not_found = not_found_files := not_found
end

(* This uses `entity_read_committed` and can be used by code outside of a
 * init/recheck, like commands, to see a consistent snapshot of type state even
 * in the middle of a recheck. *)
module Reader = struct
  type reader = State_reader.t

  let ( let* ) = Option.bind

  let get_file_addr ~reader:_ key =
    let* file = FileHeap.get key in
    Heap.(read_opt Fun.id (entity_read_committed file))

  let get_checked_file_addr ~reader key =
    let* addr = get_file_addr ~reader key in
    coerce_checked_file addr

  let has_ast ~reader key =
    match get_checked_file_addr ~reader key with
    | None -> false
    | Some addr -> Heap.get_file_ast addr |> Heap.is_some

  let get_ast ~reader key =
    match Reader_cache.get_ast key with
    | Some _ as cached -> cached
    | None ->
      let* addr = get_checked_file_addr ~reader key in
      let* ast = read_ast key addr in
      Reader_cache.add_ast key ast;
      Some ast

  let get_aloc_table ~reader file =
    match Reader_cache.get_aloc_table file with
    | Some _ as cached -> cached
    | None ->
      let* addr = get_checked_file_addr ~reader file in
      let* aloc_table = read_aloc_table file addr in
      Reader_cache.add_aloc_table file aloc_table;
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
    match Reader_cache.get_aloc_table file with
    | Some aloc_table -> aloc_table
    | None ->
      let addr = get_checked_file_addr_unsafe ~reader file in
      let aloc_table = read_aloc_table_unsafe file addr in
      Reader_cache.add_aloc_table file aloc_table;
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
      (4 * header_size)
      + entity_size
      + checked_file_size
      + int64_size
      + opt_size (with_header_size string_size) module_name
      + exports_size
    in
    let file =
      alloc size (fun chunk ->
          let hash = write_int64 chunk hash in
          let module_name = Option.map (write_string chunk) module_name in
          let exports = write_exports chunk in
          let data = write_checked_file chunk hash module_name exports in
          write_entity chunk (Some (dyn_checked_file data))
      )
    in
    assert (file = FileHeap.add file_key file)

  let add_unparsed = add_unparsed
end
