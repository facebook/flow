(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Export_index

let inferred_name_of_modulename module_name =
  (* for filenames: /foo/bar/baz.bliffl.js -> baz.bliff.js
     for strings: @example/foo -> foo *)
  let str = Filename.basename module_name in
  (* remove suffixes, e.g. baz.bliffl.js -> baz *)
  let stripped =
    match String.index_opt str '.' with
    | Some index -> String.sub str 0 index
    | None -> str
  in
  (* convert hyphens to camel case *)
  Utils_js.camelize stripped

(** In this module, we will generate a stream of resolved module information, for indexing star
  * re-exports.
  * The stream starts from the module we want to export. Given a mref with in the module, it will
  * resolve information to reach the imported module's export, and so on.
  * The stream is completely lazy. For modules without star re-exports, it will have zero cost. *)
module ModuleResolutionLazyStream = struct
  type stream_result = {
    parse: [ `typed ] Parsing_heaps.parse_addr;
    file_key: File_key.t;
    next: string -> stream_result option;
  }

  let get_dep_and_next_resolver ~reader ~old =
    let resolved_modules =
      if old then
        Parsing_heaps.Mutator_reader.get_old_resolved_modules_unsafe ~reader Fun.id
      else
        Parsing_heaps.Mutator_reader.get_resolved_modules_unsafe ~reader Fun.id
    in
    let get_provider =
      if old then
        Parsing_heaps.Mutator_reader.get_old_provider ~reader
      else
        Parsing_heaps.Mutator_reader.get_provider ~reader
    in
    let get_typed_parse =
      if old then
        Parsing_heaps.Mutator_reader.get_old_typed_parse ~reader
      else
        Parsing_heaps.Mutator_reader.get_typed_parse ~reader
    in
    let rec next (file_key, parse) mref : stream_result option =
      match
        Flow_import_specifier.Map.find_opt
          (Flow_import_specifier.userland_specifier mref)
          (resolved_modules file_key parse)
        |> Base.Option.bind ~f:Result.to_option
        |> Base.Option.bind ~f:get_provider
      with
      | None -> None
      | Some dep_file_addr ->
        (match get_typed_parse dep_file_addr with
        | None -> None
        | Some dep_parse ->
          let file_key = Parsing_heaps.read_file_key dep_file_addr in
          let next = next (file_key, dep_parse) in
          Some { parse = dep_parse; file_key; next })
    in
    next
end

let entries_of_exports =
  let rec helper
      ~module_name
      ~get_dep_and_next_resolver
      ~visited_deps
      ~include_values
      exports
      (acc : (string * Export_index.kind) list) =
    let (has_named, acc) =
      Base.List.fold exports ~init:(false, acc) ~f:(fun (has_named, acc) export ->
          match export with
          | Exports.DefaultType name_opt ->
            let name_from_modulename = inferred_name_of_modulename module_name in
            let acc = (name_from_modulename, DefaultType) :: acc in
            let acc =
              match name_opt with
              | Some name when name <> name_from_modulename -> (name, DefaultType) :: acc
              | _ -> acc
            in
            (has_named, acc)
          | Exports.Default name_opt ->
            if include_values then
              let name_from_modulename = inferred_name_of_modulename module_name in
              let acc = (name_from_modulename, Default) :: acc in
              let acc =
                match name_opt with
                | Some name when name <> name_from_modulename -> (name, Default) :: acc
                | _ -> acc
              in
              (has_named, acc)
            else
              (has_named, acc)
          | Exports.Named name ->
            if include_values then
              (true, (name, Named) :: acc)
            else
              (has_named, acc)
          | Exports.NamedType name -> (has_named, (name, NamedType) :: acc)
          | Exports.Module (module_name, exports) ->
            ( has_named,
              helper
                ~module_name:(Flow_import_specifier.display_userland module_name)
                ~get_dep_and_next_resolver
                ~visited_deps
                ~include_values
                exports
                acc
            )
          | Exports.ReExportModule mref ->
            ( has_named,
              with_reexports
                mref
                ~module_name
                ~get_dep_and_next_resolver
                ~visited_deps
                ~include_values
                acc
            )
          | Exports.ReExportModuleTypes mref ->
            ( has_named,
              with_reexports
                mref
                ~module_name
                ~get_dep_and_next_resolver
                ~visited_deps
                ~include_values:false
                acc
            )
      )
    in
    if has_named then
      (inferred_name_of_modulename module_name, Namespace) :: acc
    else
      acc
  and with_reexports mref ~module_name ~get_dep_and_next_resolver ~visited_deps ~include_values acc
      =
    match get_dep_and_next_resolver (Flow_import_specifier.display_userland mref) with
    | None -> acc
    | Some { ModuleResolutionLazyStream.parse; file_key; next } ->
      (* Guard against potential re-exports cycle *)
      if Utils_js.FilenameSet.mem file_key visited_deps then
        acc
      else
        helper
          ~module_name
          ~get_dep_and_next_resolver:next
          ~visited_deps:(Utils_js.FilenameSet.add file_key visited_deps)
          ~include_values
          (Parsing_heaps.read_exports parse)
          acc
  in
  fun ~module_name ~get_dep_and_next_resolver exports ->
    helper
      ~module_name
      ~get_dep_and_next_resolver
      ~visited_deps:Utils_js.FilenameSet.empty
      ~include_values:true
      exports
      []

(** [add_exports ~source ~module_name exports index] adds [exports] to [index].
    For default and namespace exports, [module_name] is used as the exported name
    (converted to a valid identifier firist). *)
let add_exports ~source ~module_name ~get_dep_and_next_resolver exports index =
  let names = entries_of_exports ~module_name ~get_dep_and_next_resolver exports in
  Base.List.fold names ~init:index ~f:(fun acc (name, kind) -> Export_index.add name source kind acc)

let add_imports imports resolved_modules provider (index : Export_index.t) =
  Base.List.fold imports ~init:index ~f:(fun acc (import : Imports.import) ->
      let open Imports in
      match import.source with
      | Imports.Global ->
        let name = import.export in
        let acc = Export_index.add name Export_index.Global Export_index.NamedType acc in
        Export_index.add name Export_index.Global Export_index.Named acc
      | Unresolved_source mref ->
        let result =
          Flow_import_specifier.Map.find_opt (Flow_import_specifier.Userland mref) resolved_modules
        in
        let kind_and_name module_name =
          match import.Imports.kind with
          | Imports.Default ->
            (Export_index.Default, Flow_import_specifier.display_userland module_name)
          | Imports.Named -> (Export_index.Named, import.export)
          | Imports.Namespace ->
            (Export_index.Namespace, Flow_import_specifier.display_userland module_name)
          | Imports.NamedType -> (Export_index.NamedType, import.export)
          | Imports.Unknown -> failwith "Unknown Kind"
        in
        (match result with
        | Some (Ok dependency) ->
          let module_name = Parsing_heaps.read_dependency dependency in
          let (source, module_name) =
            (module_name, inferred_name_of_modulename (Modulename.to_string module_name))
          in
          let (kind, name) = kind_and_name (Flow_import_specifier.userland module_name) in
          (match source with
          | Modulename.Filename fn ->
            let file_key = File_key fn in
            Export_index.add name file_key kind acc
          | Modulename.Haste _ ->
            (match provider dependency with
            | Some file ->
              let file_key = File_key (Parsing_heaps.read_file_key file) in
              Export_index.add name file_key kind acc
            | None -> acc))
        | Some (Error (Some (Flow_import_specifier.Userland mref))) ->
          let (kind, name) = kind_and_name mref in
          Export_index.add name (Builtin mref) kind acc
        | Some (Error (Some (Flow_import_specifier.HasteImportWithSpecifiedNamespace _))) ->
          (* We should not index synthetic imports. *)
          acc
        | Some (Error None) ->
          let (kind, name) = kind_and_name mref in
          Export_index.add name (Builtin mref) kind acc
        | None ->
          (*Could not find resolved_requires key for this unresolved_source*)
          acc)
  )

(** [add_exports_of_checked_file file_key parse haste_info index] extracts the
    exports of [file_key] from its [parse] entry in shared memory. [haste_info]
    is used to fetch the module name from [file_key]. *)
let add_exports_of_checked_file ~reader ~old file_key parse haste_info index =
  let source = Export_index.File_key file_key in
  let exports = Parsing_heaps.read_exports parse in
  let module_name =
    match haste_info with
    | Some info -> Modulename.Haste (Parsing_heaps.read_haste_module_info info)
    | None -> Modulename.Filename (Files.chop_flow_ext file_key)
  in
  let module_name = Modulename.to_string module_name in
  let get_dep_and_next_resolver =
    ModuleResolutionLazyStream.get_dep_and_next_resolver ~reader ~old (file_key, parse)
  in
  add_exports ~source ~module_name ~get_dep_and_next_resolver exports index

(** Adds builtins to [index]. See [Exports.of_builtins] for how libdefs
    are converted as if they "export" things. *)
let add_exports_of_builtins lib_exports index =
  Base.List.fold lib_exports ~init:index ~f:(fun acc export ->
      match export with
      | Exports.Module (module_name, exports) ->
        let source = Export_index.Builtin module_name in
        add_exports
          ~source
          ~module_name:(Flow_import_specifier.display_userland module_name)
          ~get_dep_and_next_resolver:(fun _ -> None)
          exports
          acc
      | Exports.Named name -> Export_index.add name Global Named acc
      | Exports.NamedType name -> Export_index.add name Global NamedType acc
      | Exports.DefaultType _ -> (* impossible *) acc
      | Exports.Default _ -> (* impossible *) acc
      | Exports.ReExportModule _ -> (* impossible *) acc
      | Exports.ReExportModuleTypes _ -> (* impossible *) acc
  )

let add_exports_of_builtins (lib_exports, scoped_lib_exports) index =
  let index = add_exports_of_builtins lib_exports index in
  Base.List.fold scoped_lib_exports ~init:index ~f:(fun index (_scoped_dir, lib_exports) ->
      add_exports_of_builtins lib_exports index
  )

(** [index_file ~reader (exports_to_add, exports_to_remove) file] reads the exports of [file] from
    shared memory and adds all of the current exports to [exports_to_add], and all of the
    previous exports to [exports_to_remove]. *)
let index_file
    ~reader (new_available_exports, old_available_exports, imports_to_add, imports_to_remove) =
  function
  | File_key.ResourceFile _f ->
    (* TODO: where does filename need to be searchable? *)
    (new_available_exports, old_available_exports, imports_to_add, imports_to_remove)
  | file_key ->
    (* TODO: when a file changes, the below removes the file entirely and then adds
        back the new info, even though much or all of it is probably still the same.
       instead, diff the old and new exports and make minimal changes. *)
    (match Parsing_heaps.get_file_addr file_key with
    | None -> (new_available_exports, old_available_exports, imports_to_add, imports_to_remove)
    | Some file ->
      let (old_available_exports, imports_to_remove) =
        (* get old exports so we can remove outdated entries *)
        match Parsing_heaps.Mutator_reader.get_old_typed_parse ~reader file with
        | Some parse ->
          let imports = Parsing_heaps.read_imports parse in
          let haste_info = Parsing_heaps.Mutator_reader.get_old_haste_info ~reader file in
          let resolved_modules =
            Parsing_heaps.Mutator_reader.get_old_resolved_modules_unsafe
              ~reader
              Fun.id
              file_key
              parse
          in
          let provider = Parsing_heaps.Mutator_reader.get_old_provider ~reader in
          ( add_exports_of_checked_file
              ~reader
              ~old:true
              file_key
              parse
              haste_info
              old_available_exports,
            add_imports imports resolved_modules provider imports_to_remove
          )
        | None ->
          (* if it wasn't checked before, there were no entries added *)
          (old_available_exports, imports_to_remove)
      in
      let (new_available_exports, imports_to_add) =
        match Parsing_heaps.Mutator_reader.get_typed_parse ~reader file with
        | Some parse ->
          let imports = Parsing_heaps.read_imports parse in
          let haste_info = Parsing_heaps.Mutator_reader.get_haste_info ~reader file in
          let resolved_modules =
            Parsing_heaps.Mutator_reader.get_resolved_modules_unsafe ~reader Fun.id file_key parse
          in
          let provider = Parsing_heaps.Mutator_reader.get_provider ~reader in
          ( add_exports_of_checked_file
              ~reader
              ~old:false
              file_key
              parse
              haste_info
              new_available_exports,
            add_imports imports resolved_modules provider imports_to_add
          )
        | None ->
          (* TODO: handle unchecked module names, maybe still parse? *)
          (new_available_exports, imports_to_add)
      in
      (new_available_exports, old_available_exports, imports_to_add, imports_to_remove))

(** Indexes all of the files in [parsed] and returns two [Export_index.t]'s: the first is
    all of the exports to add to the final index, and the second are to be removed.
    The latter is important because exports are indexed by the export name, not the
    filename; it would be expensive to walk the entire export index to check each exported
    name to see if a changed file used to export it. Instead, we re-index the previous
    version of the file to know what to remove. *)
let index ~workers ~reader parsed :
    (Export_index.t * Export_index.t * Export_index.t * Export_index.t) Lwt.t =
  let total_count = Utils_js.FilenameSet.cardinal parsed in
  let parsed = Utils_js.FilenameSet.elements parsed in

  let job ~reader files =
    let init = (Export_index.empty, Export_index.empty, Export_index.empty, Export_index.empty) in
    let (new_available_exports, old_available_exports, imports_to_add, imports_to_remove) =
      Base.List.fold files ~init ~f:(index_file ~reader)
    in
    let count = Base.List.length files in
    (new_available_exports, old_available_exports, imports_to_add, imports_to_remove, count)
  in

  Hh_logger.info "Indexing files: creating index...";
  MonitorRPC.status_update
    ~event:ServerStatus.(Indexing_progress { finished = 0; total = Some total_count });
  (* each job returns two Export_index.t's. we cons them onto lists and merge them all
     afterwards because the ~merge function blocks talking to workers so it needs to
     be as fast as possible in order to keep the workers busy. [Export_index.merge]
     was found to be slow enough for this to matter. *)
  let%lwt (new_available_exports, old_available_exports, imports_to_add, imports_to_remove, _count)
      =
    MultiWorkerLwt.call
      workers
      ~job:(job ~reader)
      ~neutral:([], [], [], [], 0)
      ~merge:
        (fun (new_available_exports, old_available_exports, imports_to_add, imports_to_remove, count)
             ( new_available_exports_acc,
               old_available_exports_acc,
               imports_to_add_acc,
               imports_to_remove_acc,
               finished
             ) ->
        let finished = finished + count in
        MonitorRPC.status_update
          ~event:ServerStatus.(Indexing_progress { total = Some total_count; finished });
        ( new_available_exports :: new_available_exports_acc,
          old_available_exports :: old_available_exports_acc,
          imports_to_add :: imports_to_add_acc,
          imports_to_remove :: imports_to_remove_acc,
          finished
        ))
      ~next:(MultiWorkerLwt.next workers parsed)
  in
  Hh_logger.info "Indexing files: indexing post-process...";
  MonitorRPC.status_update ~event:ServerStatus.Indexing_post_process;

  let new_available_exports =
    Base.List.fold new_available_exports ~init:Export_index.empty ~f:(fun acc index ->
        Export_index.merge index acc
    )
  in
  let old_available_exports =
    Base.List.fold old_available_exports ~init:Export_index.empty ~f:(fun acc index ->
        Export_index.merge index acc
    )
  in

  let imports_to_add =
    Base.List.fold imports_to_add ~init:Export_index.empty ~f:(fun acc index ->
        Export_index.merge index acc
    )
  in

  let imports_to_remove =
    Base.List.fold imports_to_remove ~init:Export_index.empty ~f:(fun acc index ->
        Export_index.merge index acc
    )
  in

  Lwt.return (new_available_exports, old_available_exports, imports_to_add, imports_to_remove)

(** Initializes an [Export_search.t] with the exports of all of the [parsed] files
    as well as the builtin libdefs. *)
let init ~workers ~reader ~libs parsed =
  let%lwt (new_available_exports, _old_available_exports, imports_to_add, _imports_to_remove) =
    index ~workers ~reader parsed
  in
  Hh_logger.info "Indexing files: adding exports of builtins...";
  let exports_to_add = add_exports_of_builtins libs new_available_exports in
  Hh_logger.info "Indexing files: merging exports-imports...";
  let final_export_index = Export_index.merge_export_import imports_to_add exports_to_add in
  (* TODO: assert that _exports_to_remove is empty? should be on init *)
  Hh_logger.info "Indexing files: initing...";
  let search = Export_search.init final_export_index in
  MonitorRPC.status_update ~event:ServerStatus.Indexing_end;
  Lwt.return search

(** [update ~changed previous] updates the exports for all of the [changed] files
    in the [previous] [Export_search.t]. *)
let update ~workers ~reader ~dirty_files previous : Export_search.t Lwt.t =
  let%lwt (new_available_exports, old_available_exports, imports_to_add, imports_to_remove) =
    index ~workers ~reader dirty_files
  in
  let result =
    previous
    |> Export_search.merge_available_exports old_available_exports new_available_exports
    |> Export_search.subtract_count imports_to_remove
    |> Export_search.merge_export_import imports_to_add
  in
  MonitorRPC.status_update ~event:ServerStatus.Indexing_end;
  Lwt.return result

module For_test = struct
  let inferred_name_of_modulename = inferred_name_of_modulename
end
