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

exception Leader_not_found of string

exception Failed_to_read_haste_info of string (* filename *) * Exception.t

let () =
  let printf exn format =
    Printf.sprintf
      "%( %s %): %s\n%s"
      format
      (Exception.get_ctor_string exn)
      (Exception.get_backtrace_string exn)
  in
  Exception.register_printer (function
      | Failed_to_read_haste_info (file, exn) ->
        Some (printf exn "Failed to read haste info for %s" file)
      | _ -> None
      )

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = Heap.file SharedMem.addr

type +'a parse_addr = 'a Heap.parse SharedMem.addr

type haste_info_addr = Heap.haste_info SharedMem.addr

type haste_module_addr = Heap.haste_module SharedMem.addr

type file_module_addr = Heap.file_module SharedMem.addr

type provider_addr = Heap.file Heap.entity SharedMem.addr

type resolved_module = (Modulename.t, string) Result.t

type resolved_requires = {
  resolved_modules: resolved_module SMap.t;
  phantom_dependencies: Modulename.Set.t;
  hash: Xx.hash;
}

type component_file = File_key.t * file_addr * [ `typed ] parse_addr

let ( let* ) = Option.bind

let mk_resolved_requires ~resolved_modules ~phantom_dependencies =
  let state = Xx.init 0L in
  SMap.iter
    (fun mref resolved_module ->
      Xx.update_int state (String.length mref);
      Xx.update state mref;
      match resolved_module with
      | Ok mname ->
        Xx.update_int state 0;
        Xx.update state (Modulename.to_string mname)
      | Error name ->
        Xx.update_int state 1;
        Xx.update state name)
    resolved_modules;
  Modulename.Set.iter
    (fun mname -> Xx.update state (Modulename.to_string mname))
    phantom_dependencies;
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

let iter_dependents f mname =
  let dependents =
    match mname with
    | Modulename.String name ->
      let* haste_module = get_haste_module name in
      Some (Heap.get_haste_dependents haste_module)
    | Modulename.Filename file_key ->
      let* file_module = get_file_module file_key in
      Some (Heap.get_file_dependents file_module)
  in
  match dependents with
  | None -> ()
  | Some dependents -> Heap.sklist_iter f dependents

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

let read_imports parse : Imports.t =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_imports parse |> read_imports |> deserialize

let read_cas_digest parse : (string * int) option =
  let open Heap in
  get_cas_digest parse |> Option.map (fun addr -> read_cas_digest addr)

let read_cas_digest_unsafe parse : string * int =
  match read_cas_digest parse with
  | Some cas_digest -> cas_digest
  | None -> raise (Requires_not_found "cas_digest")

let read_resolved_requires addr : resolved_requires =
  Marshal.from_string (Heap.read_resolved_requires addr) 0

let haste_modulename m = Modulename.String (Heap.read_string (Heap.get_haste_name m))

let prepare_add_file_module key =
  let open Heap in
  let size = (3 * header_size) + file_module_size + entity_size + sklist_size in
  let write chunk =
    let provider = write_entity chunk None in
    let dependents = write_sklist chunk in
    let m = write_file_module chunk provider dependents in
    FileModuleHeap.add key m
  in
  (size, write)

let prepare_add_haste_module name =
  let open Heap in
  let size = (4 * header_size) + haste_module_size + string_size name + entity_size + sklist_size in
  let write chunk =
    let heap_name = write_string chunk name in
    let provider = write_entity chunk None in
    let dependents = write_sklist chunk in
    let m = write_haste_module chunk heap_name provider dependents in
    HasteModuleHeap.add name m
  in
  (size, write)

let prepare_add_file_module_maybe size file_key =
  match file_key with
  | File_key.LibFile _ -> (size, Fun.const None)
  | _ ->
    let file_module_key = Files.chop_flow_ext file_key in
    (match FileModuleHeap.get file_module_key with
    | Some _ as addr -> (size, Fun.const addr)
    | None ->
      let (module_size, write) = prepare_add_file_module file_module_key in
      (size + module_size, (fun chunk -> Some (write chunk))))

let prepare_add_haste_module_maybe size name =
  match HasteModuleHeap.get name with
  | Some addr -> (size, Fun.const addr)
  | None ->
    let (module_size, write) = prepare_add_haste_module name in
    (size + module_size, write)

let prepare_write_new_haste_info_maybe size old_haste_info = function
  | None -> (size, Fun.const None)
  | Some name ->
    let open Heap in
    let haste_module_unchanged old_info =
      try
        let old_name = read_string (get_haste_name (get_haste_module old_info)) in
        String.equal name old_name
      with
      | exn ->
        let exn = Exception.wrap exn in
        raise (Failed_to_read_haste_info (name, exn))
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

let prepare_write_aloc_table locs =
  let open Type_sig_collections in
  let serialized = Packed_locs.pack (Locs.length locs) (fun f -> Locs.iter f locs) in
  let size = Heap.aloc_table_size serialized in
  let write chunk = Heap.write_aloc_table chunk serialized in
  (size, write)

let prepare_write_ast (ast : (Loc.t, Loc.t) Flow_ast.Program.t) =
  let serialized = Marshal.to_string (compactify_loc ast) [] in
  Heap.prepare_write_serialized_ast serialized

let prepare_write_cas_digest_maybe cas_digest =
  match cas_digest with
  | None -> (0, (fun _ -> None))
  | Some (sha1, bytelen) ->
    let open Heap in
    let (size, write) = prepare_write_cas_digest sha1 bytelen in
    (header_size + size, (fun chunk -> Some (write chunk)))

let prepare_write_docblock (docblock : Docblock.t) =
  let serialized = Marshal.to_string docblock [] in
  let size = Heap.docblock_size serialized in
  let write chunk = Heap.write_docblock chunk serialized in
  (size, write)

let prepare_write_exports (exports : Exports.t) =
  let serialized = Marshal.to_string exports [] in
  Heap.prepare_write_serialized_exports serialized

let prepare_write_file_sig (file_sig : File_sig.With_Loc.tolerable_t) =
  let serialized = Marshal.to_string file_sig [] in
  Heap.prepare_write_serialized_file_sig serialized

let prepare_write_imports (imports : Imports.t) =
  let serialized = Marshal.to_string imports [] in
  Heap.prepare_write_serialized_imports serialized

let prepare_write_resolved_requires (resolved_requires : resolved_requires) =
  let serialized = Marshal.to_string resolved_requires [] in
  Heap.prepare_write_serialized_resolved_requires serialized

let prepare_write_resolved_requires_ent resolved_requires_opt =
  let open Heap in
  let (resolved_requires_size, write_resolved_requires_maybe) =
    match resolved_requires_opt with
    | None -> (0, Fun.const None)
    | Some resolved_requires ->
      let (size, write) = prepare_write_resolved_requires resolved_requires in
      (header_size + size, (fun chunk -> Some (write chunk)))
  in
  let size = header_size + entity_size + resolved_requires_size in
  let write chunk =
    let resolved_requires = write_resolved_requires_maybe chunk in
    write_entity chunk resolved_requires
  in
  (size, write)

let prepare_write_type_sig type_sig =
  let open Heap in
  let (sig_bsize, write_sig) = Type_sig_bin.write type_sig in
  let size = type_sig_size sig_bsize in
  let write chunk = write_type_sig chunk sig_bsize write_sig in
  (size, write)

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

  let get_haste_module info =
    try get_haste_module info with
    | exn ->
      let exn = Exception.wrap exn in
      raise (Failed_to_read_haste_info (File_key.to_string file_key, exn))
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

(* Given a file, it's old resolved requires, and new resolved requires, compute
 * the changes necessary to update the reverse dependency graph. *)
let prepare_update_revdeps =
  (* Combine successfully resolved modules and phantom modules into a sorted list *)
  let all_dependencies = function
    | None -> []
    | Some { resolved_modules; phantom_dependencies; _ } ->
      let xs =
        let f _ m acc =
          match m with
          | Ok m -> MSet.add m acc
          | Error _ -> acc
        in
        MSet.elements (SMap.fold f resolved_modules MSet.empty)
      in
      let ys = MSet.elements phantom_dependencies in
      Base.List.merge xs ys ~compare:Modulename.compare
  in
  (* Partition two sorted lists. Elements in both `xs` and `ys` are skipped.
   * Otherwise, elements in `xs` are passed to `f` while elements in `ys` are
   * passed to `g`.
   *
   * We will use this to separate removed dependencies from added dependencies,
   * so we can remove dependents in the former case and add dependents in the
   * latter. *)
  let rec partition_fold cmp f g acc = function
    | ([], []) -> acc
    | (xs, []) -> List.fold_left f acc xs
    | ([], ys) -> List.fold_left g acc ys
    | (x :: xs, y :: ys) ->
      let k = cmp x y in
      if k = 0 then
        partition_fold cmp f g acc (xs, ys)
      else if k < 0 then
        partition_fold cmp f g (f acc x) (xs, y :: ys)
      else
        partition_fold cmp f g (g acc y) (x :: xs, ys)
  in
  (* Remove `file` from the dependents of `mname`.
   *
   * TODO: Clean up modules which have no dependents and no providers. This is a
   * bit tricky because we might remove the last dependent in one step, then add
   * a dependent/provider in another step. One idea is to maintain a set of
   * dirty modules which is scanned on commit and deleted if possible. For now,
   * leaking these modules is not too bad.
   *
   * X-ref commit modules where a module can be updated to have no providers. *)
  let remove_old_dependent (size_acc, write_acc) mname =
    let dependents =
      match mname with
      | Modulename.String name -> Heap.get_haste_dependents (get_haste_module_unsafe name)
      | Modulename.Filename key -> Heap.get_file_dependents (get_file_module_unsafe key)
    in
    let write chunk file =
      write_acc chunk file;
      if not (Heap.file_set_remove dependents file) then
        Printf.ksprintf
          failwith
          "remove_old_dependent failed: %s is not in dependents of module %s"
          (read_file_name file)
          (Modulename.to_string mname)
    in
    (size_acc, write)
  in
  (* Add `file` to the dependents of `mname`. It is possible that a module
   * object for `mname` does not exist (for phantom dependencies) so we create
   * the module if necessary. *)
  let add_new_dependent (size_acc, write_acc) mname =
    let (module_size, get_dependents) =
      match mname with
      | Modulename.String name ->
        let (size, m) =
          match get_haste_module name with
          | Some m -> (0, Fun.const m)
          | None -> prepare_add_haste_module name
        in
        (size, (fun chunk -> Heap.get_haste_dependents (m chunk)))
      | Modulename.Filename key ->
        let (size, m) =
          match get_file_module key with
          | Some m -> (0, Fun.const m)
          | None -> prepare_add_file_module key
        in
        (size, (fun chunk -> Heap.get_file_dependents (m chunk)))
    in
    let (sknode_size, write_sknode) = Heap.prepare_write_sknode () in
    let write chunk file =
      write_acc chunk file;
      let dependents = get_dependents chunk in
      let sknode = write_sknode chunk file in
      if not (Heap.file_set_add dependents sknode) then
        Printf.ksprintf
          failwith
          "add_new_dependent failed: could not add %s to dependents of module %s"
          (read_file_name file)
          (Modulename.to_string mname)
    in
    (size_acc + module_size + Heap.header_size + sknode_size, write)
  in
  fun old_resolved_requires new_resolved_requires ->
    let old_dependencies = all_dependencies old_resolved_requires in
    let new_dependencies = all_dependencies new_resolved_requires in
    partition_fold
      Modulename.compare
      remove_old_dependent
      add_new_dependent
      (0, (fun _ _ -> ()))
      (old_dependencies, new_dependencies)

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

let prepare_write_typed_parse_ents size resolved_requires_opt =
  let open Heap in
  let (resolved_requires_size, write_resolved_requires_ent) =
    prepare_write_resolved_requires_ent resolved_requires_opt
  in
  let size = size + (2 * (header_size + entity_size)) + resolved_requires_size in
  let write chunk =
    (write_resolved_requires_ent chunk, write_entity chunk None, write_entity chunk None)
  in
  (size, write)

let prepare_set_maybe prepare_write setter opt =
  let open Heap in
  match opt with
  | None -> (0, (fun _ _ -> ()))
  | Some x ->
    let (size, write) = prepare_write x in
    let write chunk parse =
      let ast = write chunk in
      setter parse ast
    in
    (header_size + size, write)

let prepare_set_aloc_table_maybe = prepare_set_maybe prepare_write_aloc_table Heap.set_aloc_table

let prepare_set_ast_maybe = prepare_set_maybe prepare_write_ast Heap.set_ast

let prepare_set_docblock_maybe = prepare_set_maybe prepare_write_docblock Heap.set_docblock

let prepare_set_file_sig_maybe = prepare_set_maybe prepare_write_file_sig Heap.set_file_sig

let prepare_set_type_sig_maybe = prepare_set_maybe prepare_write_type_sig Heap.set_type_sig

(* Write parsed data for checked file to shared memory. If we loaded from saved
 * state, a checked file entry will already exist without parse data and this
 * function will update the existing entry in place. Otherwise, we will create a
 * new entry and add it to the shared hash table. *)
let add_checked_file
    file_key
    file_opt
    hash
    module_name
    docblock_opt
    ast_opt
    locs_opt
    type_sig_opt
    file_sig_opt
    exports
    imports
    cas_digest =
  let open Heap in
  let (ast_size, set_ast_maybe) = prepare_set_ast_maybe ast_opt in
  let (docblock_size, set_docblock_maybe) = prepare_set_docblock_maybe docblock_opt in
  let (aloc_table_size, set_aloc_table_maybe) = prepare_set_aloc_table_maybe locs_opt in
  let (type_sig_size, set_type_sig_maybe) = prepare_set_type_sig_maybe type_sig_opt in
  let (file_sig_size, set_file_sig_maybe) = prepare_set_file_sig_maybe file_sig_opt in
  let size = ast_size + docblock_size + aloc_table_size + type_sig_size + file_sig_size in
  let unchanged_or_fresh_parse =
    match file_opt with
    | None ->
      let (size, write_parse_ents) = prepare_write_typed_parse_ents size None in
      let (size, add_file) = prepare_create_file size file_key module_name in
      Either.Right (size, write_parse_ents, add_file)
    | Some file ->
      let parse_ent = get_parse file in
      let typed_parse =
        let* parse = entity_read_latest parse_ent in
        coerce_typed parse
      in
      (match typed_parse with
      | None ->
        let (size, write_parse_ents) = prepare_write_typed_parse_ents size None in
        let (size, update_file) = prepare_update_file size file_key file parse_ent module_name in
        Either.Right (size, write_parse_ents, update_file)
      | Some parse ->
        let old_hash = read_int64 (get_file_hash parse) in
        if Int64.equal hash old_hash then
          (* If we loaded from a saved state, we will have some existing data with a
           * matching hash. In this case, we want to update the existing data with
           * parse information. *)
          Either.Left parse
        else
          let (size, update_file) = prepare_update_file size file_key file parse_ent module_name in
          let parse_ents _ = (get_resolved_requires parse, get_leader parse, get_sig_hash parse) in
          Either.Right (size, parse_ents, update_file))
  in
  let (size, add_file_maybe) =
    match unchanged_or_fresh_parse with
    | Either.Left unchanged_parse -> (size, Fun.const (unchanged_parse, MSet.empty))
    | Either.Right (size, write_parse_ents_maybe, add_file_maybe) ->
      let (exports_size, write_exports) = prepare_write_exports exports in
      let (imports_size, write_imports) = prepare_write_imports imports in
      let (cas_digest_size, write_cas_digest) = prepare_write_cas_digest_maybe cas_digest in
      let size =
        size
        + (4 * header_size)
        + typed_parse_size
        + int64_size
        + exports_size
        + imports_size
        + cas_digest_size
      in
      let write chunk =
        let hash = write_int64 chunk hash in
        let exports = write_exports chunk in
        let imports = write_imports chunk in
        let cas_digest = write_cas_digest chunk in
        let (resolved_requires, leader, sig_hash) = write_parse_ents_maybe chunk in
        let parse =
          write_typed_parse chunk hash exports resolved_requires imports leader sig_hash cas_digest
        in
        let dirty_modules = add_file_maybe chunk (parse :> [ `typed | `untyped ] parse_addr) in
        (parse, dirty_modules)
      in
      (size, write)
  in
  alloc size (fun chunk ->
      let (parse, dirty_modules) = add_file_maybe chunk in
      set_ast_maybe chunk parse;
      set_docblock_maybe chunk parse;
      set_aloc_table_maybe chunk parse;
      set_type_sig_maybe chunk parse;
      set_file_sig_maybe chunk parse;
      dirty_modules
  )

let add_unparsed_file file_key file_opt hash module_name =
  let read_resolved_requires_caml = read_resolved_requires in
  let open Heap in
  let size = (2 * header_size) + untyped_parse_size + int64_size in
  let (size, add_file_maybe) =
    match file_opt with
    | None -> prepare_create_file size file_key module_name
    | Some file ->
      let parse_ent = get_parse file in
      let (size, update_file) = prepare_update_file size file_key file parse_ent module_name in
      let old_resolved_requires =
        let* parse = entity_read_latest parse_ent in
        let* parse = coerce_typed parse in
        let* addr = entity_read_latest (get_resolved_requires parse) in
        Some (read_resolved_requires_caml addr)
      in
      let (dep_size, update_revdeps) = prepare_update_revdeps old_resolved_requires None in
      let update chunk =
        update_revdeps chunk file;
        update_file chunk
      in
      (size + dep_size, update)
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
  let read_resolved_requires_caml = read_resolved_requires in
  let open Heap in
  match FileHeap.get file_key with
  | None -> MSet.empty
  | Some file ->
    let parse_ent = get_parse file in
    (match entity_read_latest parse_ent with
    | None -> MSet.empty
    | Some parse ->
      let () =
        let old_resolved_requires =
          let* parse = coerce_typed parse in
          let* addr = entity_read_latest (get_resolved_requires parse) in
          Some (read_resolved_requires_caml addr)
        in
        let (size, update) = prepare_update_revdeps old_resolved_requires None in
        alloc size (fun chunk -> update chunk file)
      in
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
  let read_resolved_requires_caml = read_resolved_requires in
  let open Heap in
  let get_haste_module_info info = (get_haste_module info, info) in
  let rollback_file file =
    let parse_ent = get_parse file in
    let (old_file_module, new_file_module, old_typed_parse, new_typed_parse) =
      match (entity_read_committed parse_ent, entity_read_latest parse_ent) with
      | (None, None) -> (None, None, None, None)
      | (Some old_parse, Some new_parse) ->
        (None, None, coerce_typed old_parse, coerce_typed new_parse)
      | (None, Some new_parse) -> (None, get_file_module file, None, coerce_typed new_parse)
      | (Some old_parse, None) -> (get_file_module file, None, coerce_typed old_parse, None)
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
    (* Roll back changes to reverse dependency graph. During parse, the only
     * changes we make are when a checked file is deleted or becomes unparsed.
     * Any other changes to the dependency graph happen when resolving requires,
     * and are rolled back as part of that step. *)
    let () =
      match (old_typed_parse, new_typed_parse) with
      | (Some old_parse, None) ->
        let old_resolved_requires =
          let* addr = entity_read_latest (get_resolved_requires old_parse) in
          Some (read_resolved_requires_caml addr)
        in
        let (size, update_revdeps) = prepare_update_revdeps None old_resolved_requires in
        alloc size (fun chunk -> update_revdeps chunk file)
      | _ -> ()
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

let add_parsed
    file_key
    file_opt
    ~exports
    ~imports
    hash
    module_name
    docblock
    ast
    file_sig
    locs
    type_sig
    cas_digest =
  WorkerCancel.with_no_cancellations (fun () ->
      add_checked_file
        file_key
        file_opt
        hash
        module_name
        (Some docblock)
        (Some ast)
        (Some locs)
        (Some type_sig)
        (Some file_sig)
        exports
        imports
        cas_digest
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

  val get_leader : reader:reader -> [ `typed ] parse_addr -> file_addr option

  val has_ast : reader:reader -> File_key.t -> bool

  val get_ast : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val get_aloc_table : reader:reader -> File_key.t -> ALoc.table option

  val get_docblock : reader:reader -> File_key.t -> Docblock.t option

  val get_exports : reader:reader -> File_key.t -> Exports.t option

  val get_imports : reader:reader -> File_key.t -> Imports.t option

  val get_tolerable_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t option

  val get_file_sig : reader:reader -> File_key.t -> File_sig.With_Loc.t option

  val get_type_sig : reader:reader -> File_key.t -> type_sig option

  val get_file_hash : reader:reader -> File_key.t -> Xx.hash option

  val get_cas_digest : reader:reader -> File_key.t -> (string * int) option

  val get_parse_unsafe :
    reader:reader -> File_key.t -> file_addr -> [ `typed | `untyped ] parse_addr

  val get_typed_parse_unsafe : reader:reader -> File_key.t -> file_addr -> [ `typed ] parse_addr

  val get_resolved_requires_unsafe :
    reader:reader -> File_key.t -> [ `typed ] parse_addr -> resolved_requires

  val get_leader_unsafe : reader:reader -> File_key.t -> [ `typed ] parse_addr -> file_addr

  val get_ast_unsafe : reader:reader -> File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t

  val get_aloc_table_unsafe : reader:reader -> File_key.t -> ALoc.table

  val get_docblock_unsafe : reader:reader -> File_key.t -> Docblock.t

  val get_exports_unsafe : reader:reader -> File_key.t -> Exports.t

  val get_imports_unsafe : reader:reader -> File_key.t -> Imports.t

  val get_tolerable_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.tolerable_t

  val get_file_sig_unsafe : reader:reader -> File_key.t -> File_sig.With_Loc.t

  val get_type_sig_unsafe : reader:reader -> File_key.t -> type_sig

  val get_file_hash_unsafe : reader:reader -> File_key.t -> Xx.hash

  val get_cas_digest_unsafe : reader:reader -> File_key.t -> string * int

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

  let get_old_provider ~reader m =
    let* provider = get_provider_ent m in
    read_old ~reader provider

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

  let get_leader ~reader parse = read ~reader (Heap.get_leader parse)

  let get_old_parse ~reader file = read_old ~reader (Heap.get_parse file)

  let get_old_typed_parse ~reader file =
    let* parse = get_old_parse ~reader file in
    Heap.coerce_typed parse

  let get_old_haste_info ~reader file = read_old ~reader (Heap.get_haste_info file)

  let get_old_resolved_requires_unsafe ~reader file parse =
    let resolved_requires = read_old ~reader (Heap.get_resolved_requires parse) in
    match resolved_requires with
    | Some resolved_requires -> read_resolved_requires resolved_requires
    | None -> raise (Resolved_requires_not_found (File_key.to_string file))

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

  let get_imports ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    Some (read_imports parse)

  let get_old_exports ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_old_typed_parse ~reader addr in
    Some (read_exports parse)

  let get_old_imports ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_old_typed_parse ~reader addr in
    Some (read_imports parse)

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

  let get_old_cas_digest ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_old_typed_parse ~reader addr in
    read_cas_digest parse

  let get_cas_digest ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_cas_digest parse

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

  let get_leader_unsafe ~reader file parse =
    match get_leader ~reader parse with
    | Some leader -> leader
    | None -> raise (Leader_not_found (File_key.to_string file))

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

  let get_imports_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_imports parse

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

  let get_cas_digest_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_cas_digest_unsafe parse

  let loc_of_aloc = loc_of_aloc ~get_aloc_table_unsafe

  (* We choose the head file as the leader, and the tail as followers. It is
   * always OK to choose the head as leader, as explained below.
   *
   * Note that cycles cannot happen between untyped files. Why? Because files
   * in cycles must have their dependencies recorded, yet dependencies are never
   * recorded for untyped files.
   *
   * It follows that when the head is untyped, there are no other files! We
   * don't have to worry that some other file may be typed when the head is
   * untyped.
   *
   * It also follows when the head is typed, the tail must be typed too! *)
  let typed_component ~reader (leader_key, rest) =
    let leader = get_file_addr_unsafe leader_key in
    let* leader_parse = get_typed_parse ~reader leader in
    let component_file key =
      let file = get_file_addr_unsafe key in
      let parse = get_typed_parse_unsafe ~reader key file in
      (key, file, parse)
    in
    Some ((leader_key, leader, leader_parse), List.map component_file rest)
end

(* For use by a worker process *)
type worker_mutator = {
  add_parsed:
    File_key.t ->
    file_addr option ->
    exports:Exports.t ->
    imports:Imports.t ->
    Xx.hash ->
    string option ->
    Docblock.t ->
    (Loc.t, Loc.t) Flow_ast.Program.t ->
    File_sig.With_Loc.tolerable_t ->
    locs_tbl ->
    type_sig ->
    (string * int) option ->
    MSet.t;
  add_unparsed: File_key.t -> file_addr option -> Xx.hash -> string option -> MSet.t;
  clear_not_found: File_key.t -> MSet.t;
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator = struct
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
module Reparse_mutator = struct
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
      Hh_logger.info "Committing reparse";
      Mutator_cache.clear ();
      Reader_cache.remove_batch !changed_files;
      FileHeap.remove_batch !not_found_files;
      reset_refs ()
    in

    let rollback () =
      Hh_logger.info "Rolling back reparse";
      Mutator_cache.clear ();
      rollback_changed ();
      reset_refs ()
    in

    Transaction.add ~commit ~rollback transaction;

    ((), { add_parsed; add_unparsed; clear_not_found })

  let record_unchanged () unchanged = changed_files := FilenameSet.diff !changed_files unchanged

  let record_not_found () not_found = not_found_files := not_found
end

module Resolved_requires_mutator = struct
  type t = unit

  let rollback_resolved_requires file_key =
    let read_resolved_requires_caml = read_resolved_requires in
    let open Heap in
    match get_file_addr file_key with
    | None -> ()
    | Some file ->
      let resolved_requires_ent =
        let* parse = entity_read_latest (get_parse file) in
        let* parse = coerce_typed parse in
        Some (get_resolved_requires parse)
      in
      (match resolved_requires_ent with
      | Some ent when entity_changed ent ->
        let old_resolved_requires =
          let* addr = entity_read_committed ent in
          Some (read_resolved_requires_caml addr)
        in
        let new_resolved_requires =
          let* addr = entity_read_latest ent in
          Some (read_resolved_requires_caml addr)
        in
        let (size, update_revdeps) =
          prepare_update_revdeps new_resolved_requires old_resolved_requires
        in
        alloc size (fun chunk -> update_revdeps chunk file);
        entity_rollback ent
      | _ -> ())

  let create transaction files =
    let commit () = Hh_logger.info "Committing resolved requires" in
    let rollback () =
      Hh_logger.info "Rolling back resolved requires";
      FilenameSet.iter rollback_resolved_requires files
    in
    Transaction.add ~commit ~rollback transaction

  let add_resolved_requires () file parse resolved_requires =
    let ent = Heap.get_resolved_requires parse in
    let old_resolved_requires =
      let* addr = Heap.entity_read_latest ent in
      Some (read_resolved_requires addr)
    in
    match old_resolved_requires with
    | Some { hash; _ } when Int64.equal hash resolved_requires.hash -> false
    | _ ->
      let (update_size, update_revdeps) =
        prepare_update_revdeps old_resolved_requires (Some resolved_requires)
      in
      let open Heap in
      let (resolved_requires_size, write_resolved_requires) =
        prepare_write_resolved_requires resolved_requires
      in
      WorkerCancel.with_no_cancellations (fun () ->
          alloc
            (update_size + header_size + resolved_requires_size)
            (fun chunk ->
              update_revdeps chunk file;
              let resolved_requires = write_resolved_requires chunk in
              entity_advance ent (Some resolved_requires));
          true
      )
end

module Merge_context_mutator = struct
  type t = unit

  let dirty_files = ref FilenameSet.empty

  let rollback_leader file_key =
    let open Heap in
    let parse =
      let* file = get_file_addr file_key in
      let* parse = entity_read_latest (get_parse file) in
      coerce_typed parse
    in
    match parse with
    | None -> ()
    | Some parse ->
      entity_rollback (get_leader parse);
      entity_rollback (get_sig_hash parse)

  let commit () =
    Hh_logger.info "Committing merge";
    dirty_files := FilenameSet.empty

  let rollback () =
    Hh_logger.info "Rolling back merge";
    FilenameSet.iter rollback_leader !dirty_files;
    dirty_files := FilenameSet.empty

  let create transaction files =
    dirty_files := files;
    Transaction.add ~commit ~rollback transaction

  let update_leader x (_, _, parse) = Heap.entity_advance (Heap.get_leader parse) x

  let update_sig_hash x (_, _, parse) = Heap.entity_advance (Heap.get_sig_hash parse) x

  let add_sig_hash parse sig_hash =
    let open Heap in
    let ent = get_sig_hash parse in
    let old_sig_hash =
      let* addr = entity_read_latest ent in
      Some (read_int64 addr)
    in
    match old_sig_hash with
    | Some old_sig_hash when Int64.equal old_sig_hash sig_hash -> false
    | _ ->
      alloc (header_size + int64_size) (fun chunk ->
          let sig_hash = write_int64 chunk sig_hash in
          entity_advance ent (Some sig_hash)
      );
      true

  let add_merge_on_diff () component sig_hash =
    let ((_, leader, leader_parse), rest) = component in
    Nel.iter (update_leader (Some leader)) component;
    let diff = add_sig_hash leader_parse sig_hash in
    if diff then List.iter (update_sig_hash None) rest;
    diff
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

  let get_leader ~reader parse = read ~reader (Heap.get_leader parse)

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

  let get_imports ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    Some (read_imports parse)

  let get_cas_digest ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_typed_parse ~reader addr in
    read_cas_digest parse

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

  let get_leader_unsafe ~reader file parse =
    match get_leader ~reader parse with
    | Some leader -> leader
    | None -> raise (Leader_not_found (File_key.to_string file))

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

  let get_imports_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_imports parse

  let get_cas_digest_unsafe ~reader file =
    let addr = get_file_addr_unsafe file in
    let parse = get_typed_parse_unsafe ~reader file addr in
    read_cas_digest_unsafe parse

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

  let get_leader ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_leader ~reader
    | State_reader reader -> Reader.get_leader ~reader

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

  let get_imports ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_imports ~reader
    | State_reader reader -> Reader.get_imports ~reader

  let get_cas_digest ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_cas_digest ~reader
    | State_reader reader -> Reader.get_cas_digest ~reader

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

  let get_leader_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_leader_unsafe ~reader
    | State_reader reader -> Reader.get_leader_unsafe ~reader

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

  let get_imports_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_imports_unsafe ~reader
    | State_reader reader -> Reader.get_imports_unsafe ~reader

  let get_cas_digest_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_cas_digest_unsafe ~reader
    | State_reader reader -> Reader.get_cas_digest_unsafe ~reader

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
  let add_parsed file_key hash module_name exports resolved_requires imports cas_digest =
    let (file_kind, file_name) = file_kind_and_name file_key in
    let open Heap in
    let (exports_size, write_exports) = prepare_write_exports exports in
    let (imports_size, write_imports) = prepare_write_imports imports in
    let (cas_digest_size, write_cas_digest) = prepare_write_cas_digest_maybe cas_digest in
    let (revdeps_size, update_revdeps) = prepare_update_revdeps None (Some resolved_requires) in
    let size =
      (8 * header_size)
      + (2 * entity_size)
      + string_size file_name
      + typed_parse_size
      + file_size
      + int64_size
      + exports_size
      + imports_size
      + cas_digest_size
      + revdeps_size
    in
    let (size, write_parse_ents) = prepare_write_typed_parse_ents size (Some resolved_requires) in
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
        let imports = write_imports chunk in
        let (resolved_requires_ent, leader_ent, sig_hash_ent) = write_parse_ents chunk in
        let cas_digest = write_cas_digest chunk in
        let parse =
          write_typed_parse
            chunk
            hash
            exports
            resolved_requires_ent
            imports
            leader_ent
            sig_hash_ent
            cas_digest
        in
        let parse_ent = write_entity chunk (Some (parse :> [ `typed | `untyped ] parse_addr)) in
        let file = write_file chunk file_kind file_name parse_ent haste_ent file_module in
        assert (file = FileHeap.add file_key file);
        update_revdeps chunk file;
        calc_dirty_modules file_key file haste_ent file_module
    )

  let add_unparsed file_key = add_unparsed file_key None
end
