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
      type t = [ `file ]
    end)

module HasteModuleHeap =
  SharedMem.NoCacheAddr
    (StringKey)
    (struct
      type t = [ `haste_module ]
    end)

exception File_not_found of string

exception File_not_parsed of string

exception File_not_typed of string

(** File was not a package.json *)
exception File_not_package of string

exception Ast_not_found of string

exception ALoc_table_not_found of string

exception Docblock_not_found of string

exception Requires_not_found of string

exception Type_sig_not_found of string

exception Haste_module_not_found of string

exception Resolved_requires_not_found of string

exception Leader_not_found of string

exception Failed_to_read_haste_info of string (* filename *) * Exception.t

exception Failed_to_read_haste_modulename of string (* filename *) * Exception.t

let () =
  let printf exn format =
    Printf.ksprintf
      (fun msg ->
        Printf.sprintf
          "%s: %s\n%s"
          msg
          (Exception.get_ctor_string exn)
          (Exception.get_backtrace_string exn))
      format
  in
  Exception.register_printer (function
      | Failed_to_read_haste_info (file, exn) ->
        Some (printf exn "Failed to read haste info for %s" file)
      | Failed_to_read_haste_modulename (file, exn) ->
        Some (printf exn "Failed to read haste modulename for %s" file)
      | _ -> None
      )

type locs_tbl = Loc.t Type_sig_collections.Locs.t

type type_sig = Type_sig_collections.Locs.index Packed_type_sig.Module.t

type file_addr = [ `file ] SharedMem.addr

type +'a parse_addr = 'a Heap.parse SharedMem.addr

type haste_info_addr = [ `haste_info ] SharedMem.addr

type haste_module_addr = [ `haste_module ] SharedMem.addr

type provider_addr = [ `file ] Heap.entity SharedMem.addr

type resolved_requires_addr = [ `resolved_requires ] SharedMem.addr

type dependency_addr = Heap.dependency SharedMem.addr

type resolved_module_addr = Heap.resolved_module SharedMem.addr

type resolved_module = (Modulename.t, string option) Result.t

type resolved_requires = resolved_module array * Modulename.Set.t

type component_file = File_key.t * file_addr * [ `typed ] parse_addr

let ( let* ) = Option.bind

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

let get_parsed_file_addr reader key =
  let* file = get_file_addr key in
  let* _ = reader.Heap.read (Heap.get_parse file) in
  Some file

let get_haste_module = HasteModuleHeap.get

let get_haste_module_unsafe name =
  match get_haste_module name with
  | Some addr -> addr
  | None -> raise (Haste_module_not_found name)

let read_provider reader m =
  match m with
  | Modulename.String name ->
    let* haste_module = get_haste_module name in
    reader.Heap.read (Heap.get_haste_provider haste_module)
  | Modulename.Filename file_key ->
    let decl_key = File_key.with_suffix file_key Files.flow_ext in
    (match get_parsed_file_addr reader decl_key with
    | Some _ as decl_provider -> decl_provider
    | None -> get_parsed_file_addr reader file_key)

let iter_dependents f mname =
  let dependents =
    match mname with
    | Modulename.String name ->
      let* haste_module = get_haste_module name in
      Some (Heap.get_haste_dependents haste_module)
    | Modulename.Filename file_key ->
      let* file_addr = get_file_addr file_key in
      Heap.get_file_dependents file_addr
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

let read_requires parse = Heap.get_requires parse |> Heap.read_requires

let read_exports parse : Exports.t =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_exports parse |> read_exports |> deserialize

let read_imports parse : Imports.t =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_imports parse |> read_imports |> deserialize

let read_cas_digest parse : Cas_digest.t option =
  let open Heap in
  get_cas_digest parse |> Option.map (fun addr -> read_cas_digest addr)

let read_cas_digest_unsafe parse : Cas_digest.t =
  match read_cas_digest parse with
  | Some cas_digest -> cas_digest
  | None -> raise (Requires_not_found "cas_digest")

let read_package_info parse : (Package_json.t, unit) result =
  let open Heap in
  let deserialize x = Marshal.from_string x 0 in
  get_package_info parse |> read_package_info |> deserialize

let read_dependency =
  let open Heap in
  read_dependency
    (fun addr -> Modulename.String (get_haste_name addr |> read_string))
    (fun addr -> Modulename.Filename (read_file_key addr))

let read_resolved_module addr =
  match Heap.read_resolved_module addr with
  | Ok dependency -> Ok (read_dependency dependency)
  | Error mapped_name -> Error (Option.map Heap.read_string mapped_name)

let read_resolved_modules resolved_requires : resolved_module array =
  let addr = Heap.get_resolved_modules resolved_requires in
  Heap.read_addr_tbl read_resolved_module addr

let read_phantom_dependencies resolved_requires : Modulename.Set.t =
  let addr = Heap.get_phantom_dependencies resolved_requires in
  let init n f =
    let i = ref 0 in
    let f () =
      let addr = f !i in
      incr i;
      addr
    in
    Modulename.Set.of_increasing_iterator_unchecked f n
  in
  Heap.read_addr_tbl_generic read_dependency addr init

let read_resolved_requires resolved_requires =
  (read_resolved_modules resolved_requires, read_phantom_dependencies resolved_requires)

let read_resolved_modules_map parse resolved_requires =
  let requires = read_requires parse in
  let resolved_modules = read_resolved_modules resolved_requires in
  let n = Array.length requires in
  let i = ref 0 in
  let f () =
    let mref = requires.(!i) in
    let m = resolved_modules.(!i) in
    incr i;
    (mref, m)
  in
  SMap.of_increasing_iterator_unchecked f n

let haste_modulename m = Modulename.String (Heap.read_string (Heap.get_haste_name m))

let prepare_find_or_add find add key =
  match find key with
  | Some x -> Heap.prepare_const x
  | None -> add key

open Heap.Prepare_syntax

let prepare_add_phantom_file file_key =
  let open Heap in
  let (file_kind, file_name) = file_kind_and_name file_key in
  let+ file_name = prepare_write_string file_name
  and+ parse_ent = prepare_write_entity
  and+ haste_ent = prepare_write_entity
  and+ dependents = prepare_write_sklist
  and+ file = prepare_write_file file_kind in
  let f = file file_name (parse_ent None) (haste_ent None) (Some dependents) in
  FileHeap.add file_key f

let prepare_find_or_add_phantom_file = prepare_find_or_add get_file_addr prepare_add_phantom_file

let prepare_add_haste_module name =
  let open Heap in
  let+ name' = prepare_write_string name
  and+ provider = prepare_write_entity
  and+ dependents = prepare_write_sklist
  and+ haste_module = prepare_write_haste_module in
  let m = haste_module name' (provider None) dependents in
  HasteModuleHeap.add name m

let prepare_find_or_add_haste_module = prepare_find_or_add get_haste_module prepare_add_haste_module

let prepare_write_haste_info_for_name name =
  let open Heap in
  let+ haste_module = prepare_find_or_add_haste_module name
  and+ haste_info = prepare_write_haste_info in
  haste_info haste_module

let prepare_update_haste_info haste_ent name =
  let+ haste_info = prepare_write_haste_info_for_name name in
  Heap.entity_advance haste_ent (Some haste_info)

let prepare_update_haste_info_if_changed haste_ent name_opt =
  let open Heap in
  let old_haste_info = entity_read_latest haste_ent in
  match (old_haste_info, name_opt) with
  | (None, None) -> prepare_const ()
  | (Some _, None) ->
    let+ () = prepare_const () in
    entity_advance haste_ent None
  | (None, Some name) -> prepare_update_haste_info haste_ent name
  | (Some old_info, Some name) ->
    let old_name =
      try read_module_name old_info with
      | exn ->
        let exn = Exception.wrap exn in
        raise (Failed_to_read_haste_info (name, exn))
    in
    if String.equal name old_name then
      prepare_const ()
    else
      prepare_update_haste_info haste_ent name

let prepare_write_aloc_table locs =
  let open Type_sig_collections in
  Packed_locs.pack (Locs.length locs) (fun f -> Locs.iter f locs) |> Heap.prepare_write_aloc_table

let prepare_write_ast (ast : (Loc.t, Loc.t) Flow_ast.Program.t) =
  Marshal.to_string (compactify_loc ast) [] |> Heap.prepare_write_serialized_ast

let prepare_write_docblock (docblock : Docblock.t) =
  Marshal.to_string docblock [] |> Heap.prepare_write_docblock

let prepare_write_exports (exports : Exports.t) =
  Marshal.to_string exports [] |> Heap.prepare_write_serialized_exports

let prepare_write_file_sig (file_sig : File_sig.With_Loc.tolerable_t) =
  Marshal.to_string file_sig [] |> Heap.prepare_write_serialized_file_sig

let prepare_write_imports (imports : Imports.t) =
  Marshal.to_string imports [] |> Heap.prepare_write_serialized_imports

let prepare_write_resolved_modules resolved_modules =
  let f = function
    | Ok (Modulename.String name) ->
      (prepare_find_or_add_haste_module name :> resolved_module_addr Heap.prep)
    | Ok (Modulename.Filename key) ->
      (prepare_find_or_add_phantom_file key :> resolved_module_addr Heap.prep)
    | Error None -> Heap.prepare_const (SharedMem.null_addr :> resolved_module_addr)
    | Error (Some name) -> (Heap.prepare_write_string name :> resolved_module_addr Heap.prep)
  in
  Array.map f resolved_modules |> Heap.prepare_write_addr_tbl

let prepare_write_phantom_dependencies phantom_dependencies =
  let f mname acc =
    let prep =
      match mname with
      | Modulename.String name ->
        (prepare_find_or_add_haste_module name :> dependency_addr Heap.prep)
      | Modulename.Filename key ->
        (prepare_find_or_add_phantom_file key :> dependency_addr Heap.prep)
    in
    prep :: acc
  in
  Modulename.Set.fold f phantom_dependencies []
  |> List.rev
  |> Array.of_list
  |> Heap.prepare_write_addr_tbl

let prepare_write_resolved_requires (resolved_modules, phantom_dependencies) =
  let+ write_resolved_requires = Heap.prepare_write_resolved_requires
  and+ resolved_modules = prepare_write_resolved_modules resolved_modules
  and+ phantom_dependencies = prepare_write_phantom_dependencies phantom_dependencies in
  write_resolved_requires resolved_modules phantom_dependencies

let prepare_write_type_sig type_sig =
  let (sig_bsize, write_sig) = Type_sig_bin.write type_sig in
  Heap.prepare_write_type_sig sig_bsize write_sig

(* Calculate the set of dirty modules and prepare those modules to be committed.
 *
 * If this file became a provider to a haste module, we add this file to the
 * module's "all providers" list and mark the module as dirty.
 *
 * If this file no longer providers a haste module, we do not remove the file
 * now, to avoid complexity around concurrent deletion. Instead, old providers
 * are "logically" deleted, the module is marked as dirty, and we perform
 * deferred deletions during commit_modules.
 *
 * We also mark modules as dirty even if the module itself does not need to be
 * committed -- that is, we do not need to pick a new provider. A module is also
 * considered dirty if the provider file's contents have changed.
 *
 * TODO: Regarding the above, we might profitably separate these two notions of
 * dirtiness! We can skip re-picking a provider for modules which keep the same
 * provider, but we still need to re-check its dependents.
 *)
let calc_dirty_modules file_key file haste_ent =
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

  let haste_modulename m =
    try haste_modulename m with
    | exn ->
      let exn = Exception.wrap exn in
      raise (Failed_to_read_haste_modulename (File_key.to_string file_key, exn))
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
  (* Changing `file` does not cause the eponymous module's provider to be
   * re-picked, but it is still dirty because `file` changed. (see TODO) *)
  MSet.add (Files.eponymous_module file_key) dirty_modules

(* Given a file, it's old resolved requires, and new resolved requires, compute
 * the changes necessary to update the reverse dependency graph. *)
let prepare_update_revdeps =
  (* Combine successfully resolved modules and phantom modules into a sorted list *)
  let all_dependencies = function
    | None -> []
    | Some (resolved_modules, phantom_dependencies) ->
      let f acc = function
        | Ok m -> MSet.add m acc
        | Error _ -> acc
      in
      MSet.elements (Array.fold_left f phantom_dependencies resolved_modules)
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
  let remove_old_dependent acc mname =
    let dependents =
      match mname with
      | Modulename.String name -> Heap.get_haste_dependents (get_haste_module_unsafe name)
      | Modulename.Filename key -> Option.get (Heap.get_file_dependents (get_file_addr_unsafe key))
    in
    let+ acc = acc in
    fun file ->
      acc file;
      if not (Heap.file_set_remove dependents file) then
        Printf.ksprintf
          failwith
          "remove_old_dependent failed: %s is not in dependents of module %s"
          (read_file_name file)
          (Modulename.to_string mname)
  in
  (* Add `file` to the dependents of `mname`. *)
  let add_new_dependent acc mname =
    let+ sknode = Heap.prepare_write_sknode () and+ acc = acc in
    fun file ->
      acc file;
      let dependents =
        match mname with
        | Modulename.String name -> get_haste_module_unsafe name |> Heap.get_haste_dependents
        | Modulename.Filename key ->
          get_file_addr_unsafe key |> Heap.get_file_dependents |> Option.get
      in
      if not (Heap.file_set_add dependents (sknode file)) then
        Printf.ksprintf
          failwith
          "add_new_dependent failed: could not add %s to dependents of module %s"
          (read_file_name file)
          (Modulename.to_string mname)
  in
  fun old_resolved_requires new_resolved_requires ->
    let old_dependencies = all_dependencies old_resolved_requires in
    let new_dependencies = all_dependencies new_resolved_requires in
    partition_fold
      Modulename.compare
      remove_old_dependent
      add_new_dependent
      (Heap.prepare_const (fun _ -> ()))
      (old_dependencies, new_dependencies)

let resolved_requires_equal
    (old_resolved_modules, old_phantom_dependencies) (new_resolved_modules, new_phantom_dependencies)
    =
  Int.equal (Array.length old_resolved_modules) (Array.length new_resolved_modules)
  && Array.for_all2 ( = ) old_resolved_modules new_resolved_modules
  && Modulename.Set.equal old_phantom_dependencies new_phantom_dependencies

(** The writer returns whether the resolved requires changed. *)
let prepare_update_resolved_requires_if_changed old_parse resolved_requires_opt =
  let old_resolved_requires =
    let* parse = old_parse in
    let* parse = Heap.coerce_typed parse in
    let ent = Heap.get_resolved_requires parse in
    let* addr = Heap.entity_read_latest ent in
    Some (read_resolved_requires addr)
  in
  match (old_resolved_requires, resolved_requires_opt) with
  | (None, None) -> Heap.prepare_const (fun _ _ -> ())
  | (Some old_resolved_requires, Some new_resolved_requires)
    when resolved_requires_equal old_resolved_requires new_resolved_requires ->
    Heap.prepare_const (fun _ _ -> ())
  | _ ->
    let+ resolved_requires_opt =
      Heap.prepare_opt prepare_write_resolved_requires resolved_requires_opt
    and+ update_revdeps = prepare_update_revdeps old_resolved_requires resolved_requires_opt in
    fun file parse ->
      update_revdeps file;
      (match (Heap.coerce_typed parse, resolved_requires_opt) with
      | (None, None) -> ()
      | (None, Some _) -> failwith "update_resolved_requires: unexpected untyped file"
      | (Some parse, _) ->
        let ent = Heap.get_resolved_requires parse in
        Heap.entity_advance ent resolved_requires_opt)

let prepare_create_file file_key module_name resolved_requires_opt =
  let open Heap in
  let (file_kind, file_name) = file_kind_and_name file_key in
  let+ file_name = prepare_write_string file_name
  and+ parse_ent = prepare_write_entity
  and+ haste_ent = prepare_write_entity
  and+ haste_info = prepare_opt prepare_write_haste_info_for_name module_name
  and+ update_resolved_requires =
    prepare_update_resolved_requires_if_changed None resolved_requires_opt
  and+ dependents =
    if Files.has_flow_ext file_key then
      let impl_key = Files.chop_flow_ext file_key in
      let+ (_file : file_addr) = prepare_find_or_add_phantom_file impl_key in
      None
    else
      let+ sklist = prepare_write_sklist in
      Some sklist
  and+ file = prepare_write_file file_kind in
  fun parse ->
    let parse_ent = parse_ent (Some parse) in
    let haste_ent = haste_ent haste_info in
    let file = file file_name parse_ent haste_ent dependents in
    let file' = FileHeap.add file_key file in
    if file = file' then
      let () = update_resolved_requires file parse in
      calc_dirty_modules file_key file haste_ent
    else
      let parse_ent' = get_parse file' in
      match entity_read_latest parse_ent' with
      | None ->
        (* Raced with phantom file *)
        let haste_ent' = get_haste_info file' in
        entity_advance parse_ent' (Some parse);
        entity_advance haste_ent' haste_info;
        let () = update_resolved_requires file parse in
        calc_dirty_modules file_key file' haste_ent'
      | Some _ ->
        (* Two threads raced to add this file and the other thread won. We don't
         * need to mark any files as dirty; the other thread will have done that
         * for us. *)
        MSet.empty

(**
 @param resolved_requires_opt_opt The resolved requires to update. This is a
    nested [option]: to update the resolved requires to [x], pass [Some (Some x)];
    to remove the resolved requires (e.g. for a deleted file), pass [Some None]; to
    leave them as is (e.g. when updating other parts of the file), pass [None].
 *)
let prepare_update_file file_key file parse_ent module_name resolved_requires_opt_opt =
  let open Heap in
  let haste_ent = get_haste_info file in
  let+ () = prepare_update_haste_info_if_changed haste_ent module_name
  and+ update_resolved_requires =
    match resolved_requires_opt_opt with
    | None -> prepare_const (fun _ _ -> ())
    | Some resolved_requires_opt ->
      let old_parse = entity_read_latest parse_ent in
      prepare_update_resolved_requires_if_changed old_parse resolved_requires_opt
  in
  fun parse ->
    entity_advance parse_ent (Some parse);
    let () = update_resolved_requires file parse in
    calc_dirty_modules file_key file haste_ent

let prepare_set_opt prepare_write setter opt =
  match opt with
  | None -> Heap.prepare_const (fun _ -> ())
  | Some x ->
    let+ x = prepare_write x in
    (fun parse -> setter parse x)

let prepare_set_aloc_table_opt = prepare_set_opt prepare_write_aloc_table Heap.set_aloc_table

let prepare_set_ast_opt = prepare_set_opt prepare_write_ast Heap.set_ast

let prepare_set_docblock_opt = prepare_set_opt prepare_write_docblock Heap.set_docblock

let prepare_set_file_sig_opt = prepare_set_opt prepare_write_file_sig Heap.set_file_sig

let prepare_set_type_sig_opt = prepare_set_opt prepare_write_type_sig Heap.set_type_sig

(* Write parsed data for checked file to shared memory. If we loaded from saved
 * state, a checked file entry will already exist without parse data and this
 * function will update the existing entry in place. Otherwise, we will create a
 * new entry and add it to the shared hash table. *)
let prepare_add_checked_file
    file_key
    file_opt
    hash
    module_name
    docblock_opt
    ast_opt
    locs_opt
    type_sig_opt
    file_sig_opt
    requires
    exports
    imports
    cas_digest
    resolved_requires_opt_opt =
  let open Heap in
  let prepare_set_parse_data () =
    let+ set_ast = prepare_set_ast_opt ast_opt
    and+ set_docblock = prepare_set_docblock_opt docblock_opt
    and+ set_aloc_table = prepare_set_aloc_table_opt locs_opt
    and+ set_type_sig = prepare_set_type_sig_opt type_sig_opt
    and+ set_file_sig = prepare_set_file_sig_opt file_sig_opt in
    fun parse ->
      set_ast parse;
      set_docblock parse;
      set_aloc_table parse;
      set_type_sig parse;
      set_file_sig parse
  in
  let prepare_create_parse_with_ents () =
    let+ hash = prepare_write_int64 hash
    and+ requires = prepare_write_requires requires
    and+ exports = prepare_write_exports exports
    and+ imports = prepare_write_imports imports
    and+ cas_digest = prepare_opt prepare_write_cas_digest cas_digest
    and+ parse = prepare_write_typed_parse
    and+ set_parse_data = prepare_set_parse_data () in
    fun resolved_requires leader sig_hash ->
      let parse =
        parse hash exports requires resolved_requires imports leader sig_hash cas_digest
      in
      set_parse_data parse;
      parse
  in
  let prepare_create_parse () =
    let+ resolved_requires_ent = prepare_write_entity
    and+ leader_ent = prepare_write_entity
    and+ sig_hash_ent = prepare_write_entity
    and+ create_parse = prepare_create_parse_with_ents () in
    create_parse (resolved_requires_ent None) (leader_ent None) (sig_hash_ent None)
  in
  match file_opt with
  | None ->
    let resolved_requires_opt = Option.join resolved_requires_opt_opt in
    let+ parse = prepare_create_parse ()
    and+ create_file = prepare_create_file file_key module_name resolved_requires_opt in
    create_file (parse :> [ `typed | `untyped | `package ] parse_addr)
  | Some file ->
    let parse_ent = get_parse file in
    let typed_parse =
      let* parse = entity_read_latest parse_ent in
      coerce_typed parse
    in
    (match typed_parse with
    | None ->
      let+ parse = prepare_create_parse ()
      and+ update_file =
        prepare_update_file file_key file parse_ent module_name resolved_requires_opt_opt
      in
      update_file (parse :> [ `typed | `untyped | `package ] parse_addr)
    | Some parse ->
      (* This file has been "parsed" before. If we loaded from a saved state,
         it wasn't truly parsed but we will have some existing data with a
         matching hash. In this case, we want to update the existing data with
         parse information. If the hash doesn't match, then it's a fresh parse
         of a changed file. *)

      (* when reloading from saved state, clear the previous leader
         and sig hash. normally, it's ok for these to be temporarily
         stale because we'll merge the changed components which will
         update them, but we don't do that during a reinit. *)
      let leader_ent = get_leader parse in
      let sig_hash_ent = get_sig_hash parse in
      let clear_leader =
        match type_sig_opt with
        | Some _ -> Fun.id
        | None ->
          fun () ->
            entity_advance leader_ent None;
            entity_advance sig_hash_ent None
      in

      let old_hash = read_int64 (get_file_hash parse) in
      if Int64.equal hash old_hash then
        (* the resolved requires can be affected by changes to other files. we
           have to update them even though this file is unchanged. *)
        let+ update_resolved_requires =
          match resolved_requires_opt_opt with
          | None -> prepare_const (fun _ _ -> ())
          | Some resolved_requires_opt ->
            prepare_update_resolved_requires_if_changed (Some parse) resolved_requires_opt
        and+ set_parse_data = prepare_set_parse_data () in
        let () = update_resolved_requires file parse in
        let () = set_parse_data parse in
        let () = clear_leader () in
        MSet.empty
      else
        (* reuse the existing resolved_requires ent. this enables
           [prepare_update_resolved_requires], called by [prepare_update_file],
           to determine whether [resolved_requires_opt_opt] have changed. *)
        let resolved_requires_ent = get_resolved_requires parse in
        let+ create_parse = prepare_create_parse_with_ents ()
        and+ update_file =
          prepare_update_file file_key file parse_ent module_name resolved_requires_opt_opt
        in
        let parse = create_parse resolved_requires_ent leader_ent sig_hash_ent in
        let () = clear_leader () in
        update_file (parse :> [ `typed | `untyped | `package ] parse_addr))

let prepare_add_unparsed_file file_key file_opt hash module_name =
  let open Heap in
  let+ hash = prepare_write_int64 hash
  and+ parse = prepare_write_untyped_parse
  and+ create_or_update_file =
    match file_opt with
    | None -> prepare_create_file file_key module_name None
    | Some file ->
      let parse_ent = get_parse file in
      prepare_update_file file_key file parse_ent module_name (Some None)
  in
  let parse = parse hash in
  create_or_update_file (parse :> [ `typed | `untyped | `package ] parse_addr)

let prepare_add_package_file file_key file_opt hash module_name package_info =
  let package_info = Marshal.to_string (package_info : (Package_json.t, unit) result) [] in
  let open Heap in
  let+ hash = prepare_write_int64 hash
  and+ package_info = prepare_write_package_info package_info
  and+ parse = prepare_write_package_parse
  and+ create_or_update_file =
    match file_opt with
    | None -> prepare_create_file file_key module_name None
    | Some file ->
      let parse_ent = get_parse file in
      prepare_update_file file_key file parse_ent module_name (Some None)
  in
  let parse = parse hash package_info in
  create_or_update_file (parse :> [ `typed | `untyped | `package ] parse_addr)

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
        alloc
          (let+ update_revdeps = prepare_update_revdeps old_resolved_requires None in
           update_revdeps file
          )
      in
      entity_advance parse_ent None;
      let dirty_modules = MSet.singleton (Files.eponymous_module file_key) in
      let haste_ent = get_haste_info file in
      (match entity_read_latest haste_ent with
      | None -> dirty_modules
      | Some haste_info ->
        entity_advance haste_ent None;
        let m = get_haste_module haste_info in
        MSet.add (haste_modulename m) dirty_modules))

let rollback_resolved_requires file ent =
  if Heap.entity_changed ent then (
    let old_resolved_requires =
      let* addr = Heap.entity_read_committed ent in
      Some (read_resolved_modules addr, read_phantom_dependencies addr)
    in
    let new_resolved_requires =
      let* addr = Heap.entity_read_latest ent in
      Some (read_resolved_modules addr, read_phantom_dependencies addr)
    in
    Heap.alloc
      (let+ update_revdeps = prepare_update_revdeps new_resolved_requires old_resolved_requires in
       update_revdeps file
      );
    Heap.entity_rollback ent
  )

let rollback_leader parse =
  let open Heap in
  entity_rollback (get_leader parse);
  entity_rollback (get_sig_hash parse)

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
 * The above depends on the latest/committed state of the haste entities, which
 * also needs to be rolled back. We need to be careful about when the haste
 * entities are rolled back.
 *
 * To deal with rolling back deferred deletions, we first ensure that any
 * deferred deletions are fully performed, which must happen before we roll back
 * haste entities.
 *
 * We then can re-add the file to the all providers list, but this must happen
 * *after* we roll back the haste data. Otherwise, the file will still appear to
 * be logically deleted.
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
    let old_typed_parse = Option.bind (entity_read_committed parse_ent) coerce_typed in
    let new_typed_parse = Option.bind (entity_read_latest parse_ent) coerce_typed in
    let haste_ent = get_haste_info file in
    let (old_haste_module, new_haste_module) =
      if entity_changed haste_ent then
        let old_info = entity_read_committed haste_ent in
        let new_info = entity_read_latest haste_ent in
        (Option.map get_haste_module_info old_info, Option.map get_haste_module new_info)
      else
        (None, None)
    in
    (* Roll back changes to dependency graph. The resolved requires usually
       don't get updated during parsing -- there's a separate step for resolving
       requires -- except when loading or reloading from saved state, or when
       a file goes from parsed to unparsed/deleted. We have to update both the
       resolved_requires ent on the parse record, and the revdeps. *)
    let () =
      match (old_typed_parse, new_typed_parse) with
      | (None, None) -> ()
      | (Some old_parse, None) ->
        (* the file was deleted or became untyped. to undo that, we have to restore
           the revdeps, but rolling back the parse ent will restore the
           resolved_requires ent. *)
        let old_resolved_requires =
          let* addr = entity_read_latest (get_resolved_requires old_parse) in
          Some (read_resolved_requires_caml addr)
        in
        alloc
          (let+ update_revdeps = prepare_update_revdeps None old_resolved_requires in
           update_revdeps file
          )
      | (None, Some new_parse) ->
        (* the file was added or became typed. to undo that, we have to remove
           the revdeps, but rolling back the parse ent will remove the
           resolved_requires ent. *)
        let new_resolved_requires =
          let* addr = entity_read_latest (get_resolved_requires new_parse) in
          Some (read_resolved_requires_caml addr)
        in
        alloc
          (let+ update_revdeps = prepare_update_revdeps new_resolved_requires None in
           update_revdeps file
          )
      | (Some _, Some new_parse) ->
        (* these ents are copied from the old parse to the new parse, so we can
           just roll back the new_parse's copy. *)

        (* this also updates the revdeps *)
        let requires_ent = get_resolved_requires new_parse in
        rollback_resolved_requires file requires_ent;

        (* this undoes the clear_leader mutation. although clear_leader only
           changes the leader during a saved state reinit, it's safe to
           unconditionally roll back here because rolling back is a no-op if
           it hasn't been advanced (or is already rolled back, if the
           merge mutator also advanced the leader and then rolled back). *)
        rollback_leader new_parse
    in
    (* Remove new providers and process deferred deletions for old providers
     * before rolling back this file's parse and haste entities. *)
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
    old_haste_module |> Option.iter (fun (m, info) -> add_haste_provider m file info)
  in
  fun file_key ->
    match FileHeap.get file_key with
    | None -> ()
    | Some file -> rollback_file file

module Reader_cache : sig
  val get_ast : File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t option

  val add_ast : File_key.t -> (Loc.t, Loc.t) Flow_ast.Program.t -> unit

  val get_aloc_table : File_key.t -> ALoc.table option

  val add_aloc_table : File_key.t -> ALoc.table -> unit

  val remove_batch : FilenameSet.t -> unit

  val clear : unit -> unit
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

  let clear () =
    ASTCache.clear ();
    ALocTableCache.clear ()
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
    cas_digest : MSet.t =
  let requires =
    let open File_sig.With_Loc in
    let ({ module_sig; _ }, _) = file_sig in
    require_set module_sig |> SSet.elements |> Array.of_list
  in
  WorkerCancel.with_no_cancellations (fun () ->
      Heap.alloc
        (prepare_add_checked_file
           file_key
           file_opt
           hash
           module_name
           (Some docblock)
           (Some ast)
           (Some locs)
           (Some type_sig)
           (Some file_sig)
           requires
           exports
           imports
           cas_digest
           None
        )
  )

let add_unparsed file_key file_opt hash module_name : MSet.t =
  WorkerCancel.with_no_cancellations (fun () ->
      Heap.alloc (prepare_add_unparsed_file file_key file_opt hash module_name)
  )

let add_package file_key file_opt hash module_name package_info : MSet.t =
  WorkerCancel.with_no_cancellations (fun () ->
      Heap.alloc (prepare_add_package_file file_key file_opt hash module_name package_info)
  )

let clear_not_found file_key = WorkerCancel.with_no_cancellations (fun () -> clear_file file_key)

module type READER = sig
  type reader

  val get_provider : reader:reader -> Modulename.t -> file_addr option

  val is_typed_file : reader:reader -> file_addr -> bool

  val is_package_file : reader:reader -> file_addr -> bool

  val get_parse : reader:reader -> file_addr -> [ `typed | `untyped | `package ] parse_addr option

  val get_typed_parse : reader:reader -> file_addr -> [ `typed ] parse_addr option

  val get_package_parse : reader:reader -> file_addr -> [ `package ] parse_addr option

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

  val get_cas_digest : reader:reader -> File_key.t -> Cas_digest.t option

  val get_package_info : reader:reader -> File_key.t -> (Package_json.t, unit) result option

  val get_parse_unsafe :
    reader:reader -> File_key.t -> file_addr -> [ `typed | `untyped | `package ] parse_addr

  val get_typed_parse_unsafe : reader:reader -> File_key.t -> file_addr -> [ `typed ] parse_addr

  val get_package_parse_unsafe : reader:reader -> File_key.t -> file_addr -> [ `package ] parse_addr

  val get_resolved_requires_unsafe :
    reader:reader -> File_key.t -> [ `typed ] parse_addr -> resolved_requires_addr

  val get_resolved_modules_unsafe :
    reader:reader -> File_key.t -> [ `typed ] parse_addr -> resolved_module SMap.t

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

  val get_cas_digest_unsafe : reader:reader -> File_key.t -> Cas_digest.t

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

  let get_provider ~reader:_ m = read_provider Heap.entity_reader_latest m

  let get_old_provider ~reader:_ m = read_provider Heap.entity_reader_committed m

  let is_typed_file ~reader file =
    match read ~reader (Heap.get_parse file) with
    | Some parse -> Heap.is_typed parse
    | None -> false

  let is_package_file ~reader file =
    match read ~reader (Heap.get_parse file) with
    | Some parse -> Heap.is_package parse
    | None -> false

  let get_parse ~reader file = read ~reader (Heap.get_parse file)

  let get_typed_parse ~reader file =
    let* parse = get_parse ~reader file in
    Heap.coerce_typed parse

  let get_package_parse ~reader file =
    let* parse = get_parse ~reader file in
    Heap.coerce_package parse

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
    | Some resolved_requires -> resolved_requires
    | None -> raise (Resolved_requires_not_found (File_key.to_string file))

  let get_old_resolved_modules_unsafe ~reader file parse =
    let resolved_requires = get_old_resolved_requires_unsafe ~reader file parse in
    read_resolved_modules_map parse resolved_requires

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

  let get_package_info ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_package_parse ~reader addr in
    Some (read_package_info parse)

  let get_parse_unsafe ~reader file addr =
    match get_parse ~reader addr with
    | Some parse -> parse
    | None -> raise (File_not_parsed (File_key.to_string file))

  let get_typed_parse_unsafe ~reader file addr =
    let parse = get_parse_unsafe ~reader file addr in
    match Heap.coerce_typed parse with
    | Some parse -> parse
    | None -> raise (File_not_typed (File_key.to_string file))

  let get_package_parse_unsafe ~reader file addr =
    let parse = get_parse_unsafe ~reader file addr in
    match Heap.coerce_package parse with
    | Some parse -> parse
    | None -> raise (File_not_package (File_key.to_string file))

  let get_resolved_requires_unsafe ~reader file parse =
    let resolved_requires = Heap.get_resolved_requires parse in
    match read ~reader resolved_requires with
    | Some resolved_requires -> resolved_requires
    | None -> raise (Resolved_requires_not_found (File_key.to_string file))

  let get_resolved_modules_unsafe ~reader file parse =
    let resolved_requires = get_resolved_requires_unsafe ~reader file parse in
    read_resolved_modules_map parse resolved_requires

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
    Cas_digest.t option ->
    MSet.t;
  add_unparsed: File_key.t -> file_addr option -> Xx.hash -> string option -> MSet.t;
  add_package:
    File_key.t ->
    file_addr option ->
    Xx.hash ->
    string option ->
    (Package_json.t, unit) result ->
    MSet.t;
  clear_not_found: File_key.t -> MSet.t;
}

(* Parsing is pretty easy - there is no before state and no chance of rollbacks, so we don't
 * need to worry about a transaction *)
module Parse_mutator = struct
  let clear_not_found = Fun.const MSet.empty

  let create () = { add_parsed; add_unparsed; add_package; clear_not_found }
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
      (* TODO: remove only if no dependents
         FileHeap.remove_batch !not_found_files;
      *)
      reset_refs ()
    in

    let rollback () =
      Hh_logger.info "Rolling back reparse";
      Mutator_cache.clear ();
      rollback_changed ();
      reset_refs ()
    in

    Transaction.add ~commit ~rollback transaction;

    ((), { add_parsed; add_unparsed; add_package; clear_not_found })

  let record_unchanged () unchanged = changed_files := FilenameSet.diff !changed_files unchanged

  let record_not_found () not_found = not_found_files := not_found
end

module Resolved_requires_mutator = struct
  type t = unit

  let rollback_resolved_requires file_key =
    let open Heap in
    match get_file_addr file_key with
    | None -> ()
    | Some file ->
      let resolved_requires_ent =
        let* parse = entity_read_latest (get_parse file) in
        let* parse = coerce_typed parse in
        Some (get_resolved_requires parse)
      in
      Option.iter (rollback_resolved_requires file) resolved_requires_ent

  let create transaction files =
    let commit () = Hh_logger.info "Committing resolved requires" in
    let rollback () =
      Hh_logger.info "Rolling back resolved requires";
      FilenameSet.iter rollback_resolved_requires files
    in
    Transaction.add ~commit ~rollback transaction

  let add_resolved_requires () file parse resolved_requires =
    WorkerCancel.with_no_cancellations (fun () ->
        Heap.alloc
          (let+ update_resolved_requires =
             prepare_update_resolved_requires_if_changed (Some parse) (Some resolved_requires)
           in
           update_resolved_requires file parse
          )
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
    | Some parse -> rollback_leader parse

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
      let* addr = entity_read_committed ent in
      Some (read_int64 addr)
    in
    match old_sig_hash with
    | Some old_sig_hash when Int64.equal old_sig_hash sig_hash -> false
    | _ ->
      alloc
        (let+ sig_hash = prepare_write_int64 sig_hash in
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

  let read ~reader:_ addr = Heap.entity_read_committed addr

  let get_provider ~reader:_ m = read_provider Heap.entity_reader_committed m

  let is_typed_file ~reader file =
    match read ~reader (Heap.get_parse file) with
    | Some parse -> Heap.is_typed parse
    | None -> false

  let is_package_file ~reader file =
    match read ~reader (Heap.get_parse file) with
    | Some parse -> Heap.is_package parse
    | None -> false

  let get_parse ~reader file = read ~reader (Heap.get_parse file)

  let get_typed_parse ~reader file =
    let* parse = get_parse ~reader file in
    Heap.coerce_typed parse

  let get_package_parse ~reader file =
    let* parse = get_parse ~reader file in
    Heap.coerce_package parse

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

  let get_package_info ~reader file =
    let* addr = get_file_addr file in
    let* parse = get_package_parse ~reader addr in
    Some (read_package_info parse)

  let get_parse_unsafe ~reader file addr =
    match get_parse ~reader addr with
    | Some parse -> parse
    | None -> raise (File_not_parsed (File_key.to_string file))

  let get_typed_parse_unsafe ~reader file addr =
    let parse = get_parse_unsafe ~reader file addr in
    match Heap.coerce_typed parse with
    | Some parse -> parse
    | None -> raise (File_not_typed (File_key.to_string file))

  let get_package_parse_unsafe ~reader file addr =
    let parse = get_parse_unsafe ~reader file addr in
    match Heap.coerce_package parse with
    | Some parse -> parse
    | None -> raise (File_not_package (File_key.to_string file))

  let get_resolved_requires_unsafe ~reader file parse =
    let resolved_requires = Heap.get_resolved_requires parse in
    match read ~reader resolved_requires with
    | Some resolved_requires -> resolved_requires
    | None -> raise (Resolved_requires_not_found (File_key.to_string file))

  let get_resolved_modules_unsafe ~reader file parse =
    let resolved_requires = get_resolved_requires_unsafe ~reader file parse in
    read_resolved_modules_map parse resolved_requires

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

  let is_package_file ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.is_package_file ~reader
    | State_reader reader -> Reader.is_package_file ~reader

  let get_parse ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_parse ~reader
    | State_reader reader -> Reader.get_parse ~reader

  let get_typed_parse ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_typed_parse ~reader
    | State_reader reader -> Reader.get_typed_parse ~reader

  let get_package_parse ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_package_parse ~reader
    | State_reader reader -> Reader.get_package_parse ~reader

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

  let get_package_info ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_package_info ~reader
    | State_reader reader -> Reader.get_package_info ~reader

  let get_parse_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_parse_unsafe ~reader
    | State_reader reader -> Reader.get_parse_unsafe ~reader

  let get_typed_parse_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_typed_parse_unsafe ~reader
    | State_reader reader -> Reader.get_typed_parse_unsafe ~reader

  let get_package_parse_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_package_parse_unsafe ~reader
    | State_reader reader -> Reader.get_package_parse_unsafe ~reader

  let get_resolved_requires_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_resolved_requires_unsafe ~reader
    | State_reader reader -> Reader.get_resolved_requires_unsafe ~reader

  let get_resolved_modules_unsafe ~reader =
    match reader with
    | Mutator_state_reader reader -> Mutator_reader.get_resolved_modules_unsafe ~reader
    | State_reader reader -> Reader.get_resolved_modules_unsafe ~reader

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

module Saved_state_mutator = struct
  type master_mutator = unit

  type worker_mutator = unit

  (** Saved state contains all files, so if we roll back, we need to undo every file. *)
  let iter_files = ref (Fun.const ())

  (** When loading a new saved state, some previously-known files may not exist in the
      new state (i.e. they were deleted). When the transaction commits, we will remove
      these from the shared hash table, so sharedmem GC can collect them. *)
  let not_found_files = ref FilenameSet.empty

  let add_parsed
      () file_key file_opt hash module_name exports requires resolved_requires imports cas_digest =
    Heap.alloc
      (prepare_add_checked_file
         file_key
         file_opt
         hash
         module_name
         None
         None
         None
         None
         None
         requires
         exports
         imports
         cas_digest
         (Some (Some resolved_requires))
      )

  let add_unparsed () = add_unparsed

  let add_package () = add_package

  let clear_not_found () = clear_file

  let record_not_found () not_found = not_found_files := not_found

  let reset_refs () =
    iter_files := Fun.const ();
    not_found_files := FilenameSet.empty

  let create transaction iter_files_ =
    iter_files := iter_files_;
    let commit () =
      Hh_logger.info "Committing saved state";
      Mutator_cache.clear ();
      Reader_cache.clear ();
      (* TODO: remove only if no dependents
         FileHeap.remove_batch !not_found_files;
      *)
      reset_refs ()
    in
    let rollback () =
      Hh_logger.info "Rolling back saved state";
      Mutator_cache.clear ();
      !iter_files rollback_file;
      FilenameSet.iter rollback_file !not_found_files;
      reset_refs ()
    in
    Transaction.add ~commit ~rollback transaction;
    ((), ())
end
