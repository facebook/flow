(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js
module Heap = SharedMem.NewAPI
module MSet = Modulename.Set

module FileHeap =
  SharedMem.NoCacheAddr
    (File_key)
    (struct
      type t = Heap.file
    end)

module FileModuleHeap =
  SharedMem.NoCacheAddr
    (File_key)
    (struct
      type t = Heap.file_module
    end)

module HasteModuleHeap =
  SharedMem.NoCacheAddr
    (StringKey)
    (struct
      type t = Heap.haste_module
    end)

exception File_not_found of string

exception File_not_parsed of string

exception File_not_typed of string

exception Ast_not_found of string

exception ALoc_table_not_found of string

exception Docblock_not_found of string

exception Requires_not_found of string

exception Type_sig_not_found of string

exception Haste_module_not_found of string

exception File_module_not_found of string

exception Resolved_requires_not_found of string

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = Heap.file SharedMem.addr

type +'a parse_addr = 'a Heap.parse SharedMem.addr

type haste_info_addr = Heap.haste_info SharedMem.addr

type haste_module_addr = Heap.haste_module SharedMem.addr

type file_module_addr = Heap.file_module SharedMem.addr

type provider_addr = Heap.file Heap.entity SharedMem.addr

type resolved_requires = {
  resolved_modules: Modulename.t SMap.t;
  phantom_dependencies: SSet.t;
  hash: Xx.hash;
}
[@@deriving show]

let ( let* ) = Option.bind

let mk_resolved_requires ~resolved_modules ~phantom_dependencies =
  let state = Xx.init 0L in
  SMap.iter
    (fun reference modulename ->
      Xx.update state reference;
      Xx.update state (Modulename.to_string modulename))
    resolved_modules;
  SSet.iter (Xx.update state) phantom_dependencies;
  { resolved_modules; phantom_dependencies; hash = Xx.digest state }

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

let file_kind_and_name = function
  | File_key.Builtins -> invalid_arg "builtins"
  | File_key.SourceFile f -> (Heap.Source_file, f)
  | File_key.ResourceFile f -> (Heap.Resource_file, f)
  | File_key.JsonFile f -> (Heap.Json_file, f)
  | File_key.LibFile f -> (Heap.Lib_file, f)

let get_file_addr = FileHeap.get

let get_file_addr_unsafe file =
  match get_file_addr file with
  | Some addr -> addr
  | None -> raise (File_not_found (File_key.to_string file))

let get_haste_module = HasteModuleHeap.get

let get_file_module = FileModuleHeap.get

let get_haste_module_unsafe name =
  match get_haste_module name with
  | Some addr -> addr
  | None -> raise (Haste_module_not_found name)

let get_file_module_unsafe key =
  match get_file_module key with
  | Some addr -> addr
  | None -> raise (File_module_not_found (File_key.to_string key))

let get_provider_ent = function
  | Modulename.String name ->
    let* haste_module = get_haste_module name in
    Some (Heap.get_haste_provider haste_module)
  | Modulename.Filename file_key ->
    let* file_module = get_file_module file_key in
    Some (Heap.get_file_provider file_module)

let haste_modulename m = Modulename.String (Heap.read_string (Heap.get_haste_name m))

let prepare_add_file_module_maybe size file_key =
  match file_key with
  | File_key.LibFile _ -> (size, Fun.const None)
  | _ ->
    let file_module_key = Files.chop_flow_ext file_key in
    (match FileModuleHeap.get file_module_key with
    | Some _ as addr -> (size, Fun.const addr)
    | None ->
      let open Heap in
      let size = size + (2 * header_size) + file_module_size + entity_size in
      let write chunk =
        let provider = write_entity chunk None in
        let m = write_file_module chunk provider in
        Some (FileModuleHeap.add file_module_key m)
      in
      (size, write))

let prepare_add_haste_module_maybe size name =
  match HasteModuleHeap.get name with
  | Some addr -> (size, Fun.const addr)
  | None ->
    let open Heap in
    let size = size + (3 * header_size) + haste_module_size + string_size name + entity_size in
    let write chunk =
      let heap_name = write_string chunk name in
      let provider = write_entity chunk None in
      let m = write_haste_module chunk heap_name provider in
      HasteModuleHeap.add name m
    in
    (size, write)

let prepare_write_new_haste_info_maybe size old_haste_info = function
  | None -> (size, Fun.const None)
  | Some name ->
    let open Heap in
    let haste_module_unchanged old_info =
      let old_name = read_string (get_haste_name (get_haste_module old_info)) in
      String.equal name old_name
    in
    (match old_haste_info with
    | Some old_info when haste_module_unchanged old_info -> (size, Fun.const old_haste_info)
    | _ ->
      let size = size + header_size + haste_info_size in
      let (size, add_haste_module_maybe) = prepare_add_haste_module_maybe size name in
      let write chunk =
        let haste_module = add_haste_module_maybe chunk in
        Some (write_haste_info chunk haste_module)
      in
      (size, write))

(* Calculate the set of dirty modules and prepare those modules to be committed.
 *
 * If this file became a provider to a haste/file module, we add this file to
 * the module's "all providers" list and mark the module as dirty.
 *
 * If this file no longer providers a haste/file module, we do not remove the
 * file now, to avoid complexity around concurrent deletion. Instead, old
 * providers are "logically" deleted, the module is marked as dirty, and we
 * perform deferred deletions during commit_modules.
 *
 * We also mark modules as dirty even if the module itself does not need to be
 * committed -- that is, we do not need to pick a new provider. A module is also
 * considered dirty if the provider file's contents have changed.
 *
 * TODO: Regarding the above, we might profitably separate these two notions of
 * dirtiness! We can skip re-picking a provider for modules which keep the same
 * provider, but we still need to re-check its dependents.
 *)
let calc_dirty_modules file_key file haste_ent new_file_module =
  let open Heap in
  let (old_haste_info, new_haste_info, changed_haste_info) =
    let new_info = entity_read_latest haste_ent in
    if entity_changed haste_ent then
      let old_info = entity_read_committed haste_ent in
      (old_info, new_info, None)
    else
      (* Changing `file` does not cause `new_m`'s provider to be re-picked,
       * but the module is still dirty because `file` changed. (see TODO) *)
      (None, None, new_info)
  in
  let dirty_modules =
    match old_haste_info with
    | None -> MSet.empty
    | Some info ->
      let m = get_haste_module info in
      MSet.singleton (haste_modulename m)
  in
  let dirty_modules =
    match new_haste_info with
    | None -> dirty_modules
    | Some info ->
      let m = get_haste_module info in
      add_haste_provider m file info;
      MSet.add (haste_modulename m) dirty_modules
  in
  let dirty_modules =
    match changed_haste_info with
    | None -> dirty_modules
    | Some info ->
      let m = get_haste_module info in
      MSet.add (haste_modulename m) dirty_modules
  in
  Option.iter (fun m -> add_file_provider m file) new_file_module;
  (* Changing `file` does not cause the eponymous module's provider to be
   * re-picked, but it is still dirty because `file` changed. (see TODO) *)
  MSet.add (Files.eponymous_module file_key) dirty_modules

let prepare_create_file size file_key module_name =
  let open Heap in
  let (file_kind, file_name) = file_kind_and_name file_key in
  let size = size + (4 * header_size) + (2 * entity_size) + string_size file_name + file_size in
  let (size, write_new_haste_info_maybe) =
    prepare_write_new_haste_info_maybe size None module_name
  in
  let (size, add_file_module_maybe) = prepare_add_file_module_maybe size file_key in
  let write chunk parse =
    let file_name = write_string chunk file_name in
    let parse_ent = write_entity chunk (Some parse) in
    let haste_info = write_new_haste_info_maybe chunk in
    let haste_ent = write_entity chunk haste_info in
    let file_module = add_file_module_maybe chunk in
    let file = write_file chunk file_kind file_name parse_ent haste_ent file_module in
    if file = FileHeap.add file_key file then
      calc_dirty_modules file_key file haste_ent file_module
    else
      (* Two threads raced to add this file and the other thread won. We don't
       * need to mark any files as dirty; the other thread will have done that
       * for us. *)
      MSet.empty
  in
  (size, write)

let prepare_update_file size file_key file parse_ent module_name =
  let open Heap in
  let haste_ent = get_haste_info file in
  let old_haste_info = entity_read_latest haste_ent in
  let (size, write_new_haste_info_maybe) =
    prepare_write_new_haste_info_maybe size old_haste_info module_name
  in
  let new_file_module =
    (* If we are re-parsing an unparsed file, we need to re-add ourselves to the
     * file module's provider list. If the file is already parsed, then we are
     * certainly already a provider, so we don't need to re-add. *)
    match entity_read_latest parse_ent with
    | None -> get_file_module file
    | Some _ -> None
  in
  let write chunk parse =
    entity_advance parse_ent (Some parse);
    let new_haste_info = write_new_haste_info_maybe chunk in
    let () =
      match (old_haste_info, new_haste_info) with
      | (None, None) -> ()
      | (Some old_info, Some new_info) when haste_info_equal old_info new_info -> ()
      | _ -> entity_advance haste_ent new_haste_info
    in
    calc_dirty_modules file_key file haste_ent new_file_module
  in
  (size, write)

(* Write parsed data for checked file to shared memory. If we loaded from saved
 * state, a checked file entry will already exist without parse data and this
 * function will update the existing entry in place. Otherwise, we will create a
 * new entry and add it to the shared hash table. *)
let add_checked_file file_key file_opt hash module_name docblock ast locs type_sig file_sig exports
    =
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
  let unchanged_or_fresh_parse =
    match file_opt with
    | None -> Either.Right (prepare_create_file size file_key module_name)
    | Some file ->
      let parse_ent = get_parse file in
      (* If we loaded from a saved state, we will have some existing data with a
       * matching hash. In this case, we want to update the existing data with
       * parse information. *)
      let file_hash_unchanged parse =
        let old_hash = read_int64 (get_file_hash parse) in
        Int64.equal hash old_hash
      in
      (match entity_read_latest parse_ent with
      | Some existing_parse when file_hash_unchanged existing_parse ->
        (* We know that file is typed (we are in add_checked_file) and the
         * existing record's hash matches, so the file must have been typed
         * before as well. *)
        Either.Left (Option.get (coerce_typed existing_parse))
      | _ -> Either.Right (prepare_update_file size file_key file parse_ent module_name))
  in
  let (size, add_file_maybe) =
    match unchanged_or_fresh_parse with
    | Either.Left unchanged_parse -> (size, Fun.const (unchanged_parse, MSet.empty))
    | Either.Right (size, add_file_maybe) ->
      let exports = serialize exports in
      let (exports_size, write_exports) = prepare_write_exports exports in
      let size =
        size + (4 * header_size) + typed_parse_size + int64_size + exports_size + entity_size
      in
      let write chunk =
        let hash = write_int64 chunk hash in
        let exports = write_exports chunk in
        let resolved_requires = write_entity chunk None in
        let parse = write_typed_parse chunk hash exports resolved_requires in
        let dirty_modules = add_file_maybe chunk (parse :> [ `typed | `untyped ] parse_addr) in
        (parse, dirty_modules)
      in
      (size, write)
  in
  alloc size (fun chunk ->
      let (parse, dirty_modules) = add_file_maybe chunk in
      let ast = write_ast chunk in
      let docblock = write_docblock chunk docblock in
      let aloc_table = write_aloc_table chunk aloc_table in
      let type_sig = write_type_sig chunk sig_bsize write_sig in
      let file_sig = write_file_sig chunk in
      set_ast parse ast;
      set_docblock parse docblock;
      set_aloc_table parse aloc_table;
      set_type_sig parse type_sig;
      set_file_sig parse file_sig;
      dirty_modules
  )

let add_unparsed_file file_key file_opt hash module_name =
  let open Heap in
  let size = (2 * header_size) + untyped_parse_size + int64_size in
  let (size, add_file_maybe) =
    match file_opt with
    | None -> prepare_create_file size file_key module_name
    | Some file ->
      let parse_ent = get_parse file in
      prepare_update_file size file_key file parse_ent module_name
  in
  alloc size (fun chunk ->
      let hash = write_int64 chunk hash in
      let parse = write_untyped_parse chunk hash in
      add_file_maybe chunk (parse :> [ `typed | `untyped ] parse_addr)
  )

(* If this file used to exist, but no longer does, then it was deleted. Record
 * the deletion by clearing parse information. Deletion might also require
 * re-picking module providers, so we return dirty modules. *)
let clear_file file_key =
  let open Heap in
  match FileHeap.get file_key with
  | None -> MSet.empty
  | Some file ->
    let parse_ent = get_parse file in
    (match entity_read_latest parse_ent with
    | None -> MSet.empty
    | Some _ ->
      entity_advance parse_ent None;
      let dirty_modules =
        match get_file_module file with
        | None -> MSet.empty
        | Some _ -> MSet.singleton (Files.eponymous_module file_key)
      in
      let haste_ent = get_haste_info file in
      (match entity_read_latest haste_ent with
      | None -> dirty_modules
      | Some haste_info ->
        entity_advance haste_ent None;
        let m = get_haste_module haste_info in
        MSet.add (haste_modulename m) dirty_modules))

(* Rolling back a transaction requires that we undo changes we made to the file
 * as well as changes we made to modules affected by the file changes. Rolling
 * back changes to all_providers in particular is kind of tricky...
 *
 * If we added a file to a module's all_providers, we remove it. This case is
 * relatively simple.
 *
 * Recall that deletions are deferred. During parsing, we "logically" delete by
 * changing the file object itself and marking a module dirty. Later we perform
 * the deletions when re-picking a new provider for each dirty module.
 *
 * For a file module (M), a provider file (F) is logically deleted if its parse
 * entity's (E) latest data is null, meaning the file is deleted:
 *
 *  +---+---+---+   +---+---+---+
 *  | M | * |...|   | E | 0 |...|
 *  +---+---+---+   +---+---+---+
 *        |         ^     ^
 *    providers     |     |
 *        |       parse   +- latest data (null)
 *        v         |
 *        +---+---+---+---+
 *        | F |   | * |...|
 *        +---+---+---+---+
 *
 * For a haste module (M), a provider (F) is logically deleted if its haste info
 * entity's (E) latest data (H) no longer points back to the haste module. In
 * this case, the list continues from the committed haste info (H'):
 *
 *  +---+---+---+   +---+---+---+    +---+---+---+
 *  | M | * |...|   | E | * | * |--->| H'| M | * |---> next provider
 *  +---+---+---+   +---+---+---+    +---+---+---+
 *        |         ^     |
 *    providers     |     +---+
 *        |    haste_info     |     +--> haste module
 *        v         |         v     |    null / does not point to M
 *        +---+---+---+---+   +---+---+
 *        | F |   | * |...|   | H | * |
 *        +---+---+---+---+   +---+---+
 *
 * Both of the above rules depend on the latest/committed state of the parse and
 * haste entities, which also needs to be rolled back. We need to be careful
 * about when the parse and haste entities are rolled back.
 *
 * To deal with rolling back deferred deletions, we first ensure that any
 * deferred deletions are fully performed, which must happen before we roll back
 * parse/haste entities.
 *
 * We then can re-add the file to the all providers list, but this must happen
 * *after* we roll back the parse/haste data. Otherwise, the file will still
 * appear to be logically deleted.
 *
 * In addition to rolling back changes to the file and to dirty modules' all
 * providers list, we also roll back each dirty module's provider entity which
 * stores the committed provider.
 *)
let rollback_file =
  let open Heap in
  let get_haste_module_info info = (get_haste_module info, info) in
  let rollback_file file =
    let parse_ent = get_parse file in
    let (old_file_module, new_file_module) =
      match (entity_read_committed parse_ent, entity_read_latest parse_ent) with
      | (None, None)
      | (Some _, Some _) ->
        (None, None)
      | (None, Some _) -> (None, get_file_module file)
      | (Some _, None) -> (get_file_module file, None)
    in
    let haste_ent = get_haste_info file in
    let (old_haste_module, new_haste_module) =
      if entity_changed haste_ent then
        let old_info = entity_read_committed haste_ent in
        let new_info = entity_read_latest haste_ent in
        (Option.map get_haste_module_info old_info, Option.map get_haste_module new_info)
      else
        (None, None)
    in
    (* Remove new providers and process deferred deletions for old providers
     * before rolling back this file's parse and haste entities. *)
    old_file_module
    |> Option.iter (fun m ->
           entity_rollback (get_file_provider m);
           ignore (get_file_all_providers_exclusive m)
       );
    new_file_module
    |> Option.iter (fun m ->
           entity_rollback (get_file_provider m);
           remove_file_provider_exclusive m file
       );
    old_haste_module
    |> Option.iter (fun (m, _) ->
           entity_rollback (get_haste_provider m);
           ignore (get_haste_all_providers_exclusive m)
       );
    new_haste_module
    |> Option.iter (fun m ->
           entity_rollback (get_haste_provider m);
           remove_haste_provider_exclusive m file
       );
    (* Add back the deleted providers after rolling back the file's parse and
     * haste entities. *)
    entity_rollback parse_ent;
    entity_rollback haste_ent;
    old_file_module |> Option.iter (fun m -> add_file_provider m file);
    old_haste_module |> Option.iter (fun (m, info) -> add_haste_provider m file info)
  in
  fun file_key ->
    match FileHeap.get file_key with
    | None -> ()
    | Some file -> if file_changed file then rollback_file file

let read_file_name file =
  let open Heap in
  get_file_name file |> read_string

let read_file_key file =
  let open Heap in
  let fn = read_file_name file in
  match get_file_kind file with
  | Source_file -> File_key.SourceFile fn
  | Json_file -> File_key.JsonFile fn
  | Resource_file -> File_key.ResourceFile fn
  | Lib_file -> File_key.LibFile fn

let read_file_hash parse =
  let open Heap in
  get_file_hash parse |> read_int64

let read_module_name info =
  let open Heap in
  get_haste_module info |> get_haste_name |> read_string

let read_ast file_key parse =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_ast parse |> Option.map (fun addr -> read_ast addr |> deserialize |> decompactify_loc file_key)

let read_ast_unsafe file_key parse =
  match read_ast file_key parse with
  | Some ast -> ast
  | None -> raise (Ast_not_found (File_key.to_string file_key))

let read_docblock parse : Docblock.t option =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_docblock parse |> Option.map (fun addr -> read_docblock addr |> deserialize)

let read_docblock_unsafe file_key parse =
  match read_docblock parse with
  | Some docblock -> docblock
  | None -> raise (Docblock_not_found (File_key.to_string file_key))

let read_aloc_table file_key parse =
  let open Heap in
  let init = ALoc.ALocRepresentationDoNotUse.init_table file_key in
  get_aloc_table parse
  |> Option.map (fun addr -> read_aloc_table addr |> Packed_locs.unpack (Some file_key) init)

let read_aloc_table_unsafe file_key parse =
  match read_aloc_table file_key parse with
  | Some aloc_table -> aloc_table
  | None -> raise (ALoc_table_not_found (File_key.to_string file_key))

let read_type_sig parse =
  let open Heap in
  get_type_sig parse |> Option.map (fun addr -> read_type_sig addr Type_sig_bin.read)

let read_type_sig_unsafe file_key parse =
  match read_type_sig parse with
  | Some type_sig -> type_sig
  | None -> raise (Type_sig_not_found (File_key.to_string file_key))

let read_tolerable_file_sig parse : File_sig.With_Loc.tolerable_t option =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_file_sig parse |> Option.map (fun addr -> read_file_sig addr |> deserialize)

let read_file_sig parse = Option.map fst (read_tolerable_file_sig parse)

let read_tolerable_file_sig_unsafe file_key parse =
  match read_tolerable_file_sig parse with
  | Some file_sig -> file_sig
  | None -> raise (Requires_not_found (File_key.to_string file_key))

let read_file_sig_unsafe file_key parse = fst (read_tolerable_file_sig_unsafe file_key parse)

let read_exports parse : Exports.t =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_exports parse |> read_exports |> deserialize

let read_resolved_requires addr : resolved_requires =
  Marshal.from_string (Heap.read_resolved_requires addr) 0

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

let add_parsed file_key file_opt ~exports hash module_name docblock ast file_sig locs type_sig =
  WorkerCancel.with_no_cancellations (fun () ->
      add_checked_file
        file_key
        file_opt
        hash
        module_name
        docblock
        ast
        locs
        type_sig
        file_sig
        exports
  )

let add_unparsed file_key file_opt hash module_name =
  WorkerCancel.with_no_cancellations (fun () -> add_unparsed_file file_key file_opt hash module_name)

let clear_not_found file_key = WorkerCancel.with_no_cancellations (fun () -> clear_file file_key)

module type READER = sig
  type reader

  val get_provider : reader:reader -> Modulename.t -> file_addr option

  val is_typed_file : reader:reader -> file_addr -> bool

  val get_parse : reader:reader -> file_addr -> [ `typed | `untyped ] parse_addr option

  val get_typed_parse : reader:reader -> file_addr -> [ `typed ] parse_addr option

  val get_haste_info : reader:reader -> file_addr -> haste_info_addr option

  val get_haste_name : reader:reader -> file_addr -> string option

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_aloc_table : reader:reader -> File_key.t -> ALoc.table option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_tolerable_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_type_sig : reader:reader -> File_key.t -> type_sig option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_parse_unsafe :
    reader:reader -> File_key.t -> file_addr -> [ `typed | `untyped ] parse_addr

  val get_typed_parse_unsafe : reader:reader -> File_key.t -> file_addr -> [ `typed ] parse_addr

  val get_resolved_requires_unsafe :
    reader:reader -> File_key.t -> [ `typed ] parse_addr -> resolved_requires

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

  let read ~reader:_ addr = Heap.entity_read_latest addr

  let read_old ~reader:_ addr = Heap.entity_read_committed addr

  let get_provider ~reader m =
    let* provider = get_provider_ent m in
    read ~reader provider

  let is_typed_file ~reader file =
    match read ~reader (Heap.get_parse file) with
    | Some parse -> Heap.is_typed parse
    | None -> false

  let get_parse ~reader file = read ~reader (Heap.get_parse file)

  let get_typed_parse ~reader file =
    let* parse = get_parse ~reader file in
    Heap.coerce_typed parse

  let get_haste_info ~reader file = read ~reader (Heap.get_haste_info file)

  let get_haste_name ~reader file =
    let* info = get_haste_info ~reader file in
    Some (read_module_name info)

  let get_old_parse ~reader file = read_old ~reader (Heap.get_parse file)

  let get_old_typed_parse ~reader file =
    let* parse = get_old_parse ~reader file in
    Heap.coerce_typed parse

  let get_old_haste_info ~reader file = read_old ~reader (Heap.get_haste_info file)

  let has_ast ~reader file =
    let parse_opt =
      let* file_addr = get_file_addr file in
      get_typed_parse ~reader file_addr
    in
    match parse_opt with
    | None -> false
    | Some parse -> Heap.get_ast parse |> Option.is_some

  let get_ast ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_ast file parse

  let get_aloc_table ~reader file =
    match Mutator_cache.get_aloc_table file with
    | Some _ as cached -> cached
    | None ->
      let* addr = get_file_addr file in
      let* parse = get_typed_parse ~reader addr in
      let* aloc_table = read_aloc_table file parse in
      Mutator_cache.add_aloc_table file aloc_table;
      Some aloc_table

  let get_docblock ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_docblock parse

  let get_exports ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    Some (read_exports parse)

  let get_old_exports ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_old_typed_parse ~reader addr in
    Some (read_exports parse)

  let get_tolerable_file_sig ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_tolerable_file_sig parse

  let get_file_sig ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_file_sig parse

  let get_type_sig ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_type_sig parse

  let get_file_hash ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_parse ~reader addr in
    Some (read_file_hash parse)

  let get_old_file_hash ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_old_parse ~reader addr in
    Some (read_file_hash parse)

  let get_parse_unsafe ~reader file addr =
    match get_parse ~reader addr with
    | Some parse -> parse
    | None -> raise (File_not_parsed (File_key.to_string file))

  let get_typed_parse_unsafe ~reader file addr =
    let parse = get_parse_unsafe ~reader file addr in
    match Heap.coerce_typed parse with
    | Some parse -> parse
    | None -> raise (File_not_typed (File_key.to_string file))

  let get_resolved_requires_unsafe ~reader file parse =
    let resolved_requires = Heap.get_resolved_requires parse in
    match read ~reader resolved_requires with
    | Some resolved_requires -> read_resolved_requires resolved_requires
    | None -> raise (Resolved_requires_not_found (File_key.to_string file))

  let get_ast_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_ast_unsafe file parse

  let get_aloc_table_unsafe ~reader file =
    match Mutator_cache.get_aloc_table file with
    | Some aloc_table -> aloc_table
    | None ->
      let addr = get_file_addr_unsafe file in
      let parse = get_typed_parse_unsafe ~reader file addr in
      let aloc_table = read_aloc_table_unsafe file parse in
      Mutator_cache.add_aloc_table file aloc_table;
      aloc_table

  let get_docblock_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_docblock_unsafe file parse

  let get_exports_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_exports parse

  let get_tolerable_file_sig_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_tolerable_file_sig_unsafe file parse

  let get_file_sig_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_file_sig_unsafe file parse

  let get_type_sig_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_type_sig_unsafe file parse

  let get_file_hash_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_parse_unsafe ~reader file addr in
    read_file_hash parse

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe
end

(* For use by a worker process *)
type worker_mutator = {
  add_parsed:
    File_key.t ->
    file_addr option ->
    exports:Exports.t ->
    Xx.hash ->
    string option ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    File_sig.With_Loc.tolerable_t ->
    locs_tbl ->
    type_sig ->
    MSet.t;
  add_unparsed: File_key.t -> file_addr option -> Xx.hash -> string option -> MSet.t;
  clear_not_found: File_key.t -> MSet.t;
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator : sig
  val create : unit -> worker_mutator
end = struct
  let clear_not_found = Fun.const MSet.empty

  let create () = { add_parsed; add_unparsed; clear_not_found }
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

  let rollback_changed () = FilenameSet.iter rollback_file !changed_files

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

module Commit_modules_mutator = struct
  type t = unit

  let no_providers = ref MSet.empty

  let reset_refs () = no_providers := MSet.empty

  let remove_module = function
    | Modulename.String name -> HasteModuleHeap.remove name
    | Modulename.Filename file_key -> FileModuleHeap.remove file_key

  let commit () =
    WorkerCancel.with_no_cancellations (fun () ->
        MSet.iter remove_module !no_providers;
        reset_refs ()
    );
    Lwt.return_unit

  let rollback () =
    reset_refs ();
    Lwt.return_unit

  let create transaction = Transaction.add ~singleton:"Commit_modules" ~commit ~rollback transaction

  let record_no_providers () modules = no_providers := modules
end

module Resolved_requires_mutator = struct
  type t = unit

  let dirty_files = ref FilenameSet.empty

  let rollback_resolved_requires file_key =
    let open SharedMem.NewAPI in
    let entity =
      let* file = get_file_addr file_key in
      let* parse = entity_read_latest (get_parse file) in
      let* parse = coerce_typed parse in
      Some (get_resolved_requires parse)
    in
    Option.iter entity_rollback entity

  let commit () =
    dirty_files := FilenameSet.empty;
    Lwt.return_unit

  let rollback () =
    WorkerCancel.with_no_cancellations (fun () ->
        FilenameSet.iter rollback_resolved_requires !dirty_files
    );
    dirty_files := FilenameSet.empty;
    Lwt.return_unit

  let create transaction files =
    dirty_files := files;
    Transaction.add ~commit ~rollback transaction

  (* To detect whether resolved requires have changed, we load the old resolved
   * requires for this file and compare hashes. There are two interesting cases:
   *
   * 1. If this file is unchanged, then we advance the resolved requires entity
   *    of the latest parse. The "old" resolved requires is the latest version
   *    before updating.
   *
   * 2. If the file is changed, then the "old" resolved requires is the latest
   *    version of the *committed* parse, but we advance the resolved requires
   *    entity of the *latest* parse, as with case (1).
   *
   * If the hashes are unchanged, then for case (1) we can do nothing, but for
   * case (2) we need to advance the resolved requires entity of the latest
   * parse, because fresh parses start out without resolved requires.
   *)
  let add_resolved_requires () file parse resolved_requires =
    let module Heap = SharedMem.NewAPI in
    let old_resolved_requires =
      let* old_parse = Heap.entity_read_committed (Heap.get_parse file) in
      let* old_parse = Heap.coerce_typed old_parse in
      let old_ent = Heap.get_resolved_requires old_parse in
      match Heap.entity_read_latest old_ent with
      | Some addr ->
        let { hash; _ } = read_resolved_requires addr in
        Some (old_ent, hash, addr)
      | None -> None
    in
    let open Heap in
    let ent = get_resolved_requires parse in
    match old_resolved_requires with
    | Some (old_ent, hash, addr) when Int64.equal hash resolved_requires.hash ->
      if ent == old_ent then
        ()
      else
        entity_advance ent (Some addr);
      false
    | _ ->
      let resolved_requires = Marshal.to_string resolved_requires [] in
      let (size, write) = prepare_write_resolved_requires resolved_requires in
      alloc (header_size + size) (fun chunk ->
          let resolved_requires = write chunk in
          entity_advance ent (Some resolved_requires)
      );
      true
end

(* This uses `entity_read_committed` and can be used by code outside of a
 * init/recheck, like commands, to see a consistent snapshot of type state even
 * in the middle of a recheck. *)
module Reader = struct
  type reader = State_reader.t

  let read ~reader:_ addr = Heap.entity_read_latest addr

  let get_provider ~reader m =
    let* provider = get_provider_ent m in
    read ~reader provider

  let is_typed_file ~reader file =
    match read ~reader (Heap.get_parse file) with
    | Some parse -> Heap.is_typed parse
    | None -> false

  let get_parse ~reader file = read ~reader (Heap.get_parse file)

  let get_typed_parse ~reader file =
    let* parse = get_parse ~reader file in
    Heap.coerce_typed parse

  let get_haste_info ~reader file = read ~reader (Heap.get_haste_info file)

  let get_haste_name ~reader file =
    let* info = get_haste_info ~reader file in
    Some (read_module_name info)

  let has_ast ~reader file =
    let parse_opt =
      let* file_addr = get_file_addr file in
      get_typed_parse ~reader file_addr
    in
    match parse_opt with
    | None -> false
    | Some parse -> Heap.get_ast parse |> Option.is_some

  let get_ast ~reader file =
    match Reader_cache.get_ast file with
    | Some _ as cached -> cached
    | None ->
      let* addr = get_file_addr file in
      let* parse = get_typed_parse ~reader addr in
      let* ast = read_ast file parse in
      Reader_cache.add_ast file ast;
      Some ast

  let get_aloc_table ~reader file =
    match Reader_cache.get_aloc_table file with
    | Some _ as cached -> cached
    | None ->
      let* addr = get_file_addr file in
      let* parse = get_typed_parse ~reader addr in
      let* aloc_table = read_aloc_table file parse in
      Reader_cache.add_aloc_table file aloc_table;
      Some aloc_table

  let get_docblock ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_docblock parse

  let get_exports ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    Some (read_exports parse)

  let get_tolerable_file_sig ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_tolerable_file_sig parse

  let get_file_sig ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_file_sig parse

  let get_type_sig ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_type_sig parse

  let get_file_hash ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_parse ~reader addr in
    Some (read_file_hash parse)

  let get_parse_unsafe ~reader file addr =
    match get_parse ~reader addr with
    | Some parse -> parse
    | None -> raise (File_not_parsed (File_key.to_string file))

  let get_typed_parse_unsafe ~reader file addr =
    let parse = get_parse_unsafe ~reader file addr in
    match Heap.coerce_typed parse with
    | Some parse -> parse
    | None -> raise (File_not_typed (File_key.to_string file))

  let get_resolved_requires_unsafe ~reader file parse =
    let resolved_requires = Heap.get_resolved_requires parse in
    match read ~reader resolved_requires with
    | Some resolved_requires -> read_resolved_requires resolved_requires
    | None -> raise (Resolved_requires_not_found (File_key.to_string file))

  let get_ast_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_ast_unsafe file parse

  let get_aloc_table_unsafe ~reader file =
    match Reader_cache.get_aloc_table file with
    | Some aloc_table -> aloc_table
    | None ->
      let addr = get_file_addr_unsafe file in
      let parse = get_typed_parse_unsafe ~reader file addr in
      let aloc_table = read_aloc_table_unsafe file parse in
      Reader_cache.add_aloc_table file aloc_table;
      aloc_table

  let get_docblock_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_docblock_unsafe file parse

  let get_exports_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_exports parse

  let get_tolerable_file_sig_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_tolerable_file_sig_unsafe file parse

  let get_file_sig_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_file_sig_unsafe file parse

  let get_type_sig_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_type_sig_unsafe file parse

  let get_file_hash_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_parse_unsafe ~reader file addr in
    read_file_hash parse

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe
end

(* Reader_dispatcher is used by code which may or may not be running inside an init/recheck *)
module Reader_dispatcher : READER with type reader = Abstract_state_reader.t = struct
  type reader = Abstract_state_reader.t

  open Abstract_state_reader

  let get_provider ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_provider ~reader
    | State_reader reader -> Reader.get_provider ~reader

  let is_typed_file ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.is_typed_file ~reader
    | State_reader reader -> Reader.is_typed_file ~reader

  let get_parse ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_parse ~reader
    | State_reader reader -> Reader.get_parse ~reader

  let get_typed_parse ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_typed_parse ~reader
    | State_reader reader -> Reader.get_typed_parse ~reader

  let get_haste_info ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_haste_info ~reader
    | State_reader reader -> Reader.get_haste_info ~reader

  let get_haste_name ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_haste_name ~reader
    | State_reader reader -> Reader.get_haste_name ~reader

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

  let get_parse_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_parse_unsafe ~reader
    | State_reader reader -> Reader.get_parse_unsafe ~reader

  let get_typed_parse_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_typed_parse_unsafe ~reader
    | State_reader reader -> Reader.get_typed_parse_unsafe ~reader

  let get_resolved_requires_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_resolved_requires_unsafe ~reader
    | State_reader reader -> Reader.get_resolved_requires_unsafe ~reader

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

module From_saved_state = struct
  let add_parsed file_key hash module_name exports resolved_requires =
    let (file_kind, file_name) = file_kind_and_name file_key in
    let exports = Marshal.to_string exports [] in
    let resolved_requires = Marshal.to_string resolved_requires [] in
    let open Heap in
    let (exports_size, write_exports) = prepare_write_exports exports in
    let (resolved_requires_size, write_resolved_requires) =
      prepare_write_resolved_requires resolved_requires
    in
    let size =
      (9 * header_size)
      + (3 * entity_size)
      + string_size file_name
      + typed_parse_size
      + file_size
      + int64_size
      + exports_size
      + resolved_requires_size
    in
    let (size, add_file_module_maybe) = prepare_add_file_module_maybe size file_key in
    let (size, write_new_haste_info_maybe) =
      prepare_write_new_haste_info_maybe size None module_name
    in
    alloc size (fun chunk ->
        let file_name = write_string chunk file_name in
        let file_module = add_file_module_maybe chunk in
        let hash = write_int64 chunk hash in
        let haste_info = write_new_haste_info_maybe chunk in
        let haste_ent = write_entity chunk haste_info in
        let exports = write_exports chunk in
        let resolved_requires = write_resolved_requires chunk in
        let resolved_requires_ent = write_entity chunk (Some resolved_requires) in
        let parse = write_typed_parse chunk hash exports resolved_requires_ent in
        let parse_ent = write_entity chunk (Some (parse :> [ `typed | `untyped ] parse_addr)) in
        let file = write_file chunk file_kind file_name parse_ent haste_ent file_module in
        assert (file = FileHeap.add file_key file);
        calc_dirty_modules file_key file haste_ent file_module
    )

  let add_unparsed file_key = add_unparsed file_key None
end

let iter_resolved_requires f =
  SharedMem.NewAPI.iter_resolved_requires (fun file resolved_requires ->
      f file (read_resolved_requires resolved_requires)
  )
