(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Export_index

let camelize str =
  match String.split_on_char '-' str with
  | [] -> str
  | [str] -> str
  | hd :: rest ->
    let parts = hd :: Base.List.map ~f:String.capitalize_ascii rest in
    String.concat "" parts

let string_of_modulename modulename =
  let str = Modulename.to_string modulename in
  (* for filenames: /foo/bar/baz.bliffl.js -> baz.bliff.js
     for strings: @example/foo -> foo *)
  let str = Filename.basename str in
  (* remove suffixes, e.g. baz.bliffl.js -> baz *)
  let stripped =
    match String.index_opt str '.' with
    | Some index -> String.sub str 0 index
    | None -> str
  in
  (* convert hyphens to camel case *)
  camelize stripped

let entries_of_exports =
  let rec helper ~module_name exports (acc : (string * Export_index.kind) list) =
    let (has_named, acc) =
      Base.List.fold exports ~init:(false, acc) ~f:(fun (has_named, acc) export ->
          match export with
          | Exports.Default -> (has_named, (string_of_modulename module_name, Default) :: acc)
          | Exports.Named name -> (true, (name, Named) :: acc)
          | Exports.NamedType name -> (has_named, (name, NamedType) :: acc)
          | Exports.Module (module_name, exports) ->
            let module_name = Modulename.String module_name in
            (has_named, helper ~module_name exports acc)
      )
    in
    if has_named then
      (string_of_modulename module_name, Namespace) :: acc
    else
      acc
  in
  (fun ~module_name exports -> helper ~module_name exports [])

(** [add_exports ~source ~module_name exports index] adds [exports] to [index].
    For default and namespace exports, [module_name] is used as the exported name
    (converted to a valid identifier firist). *)
let add_exports ~source ~module_name exports index =
  let names = entries_of_exports ~module_name exports in
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
        let result = SMap.find_opt mref resolved_modules in
        (match result with
        | Some (Ok module_name) ->
          let (source, module_name) = (module_name, string_of_modulename module_name) in
          let (kind, name) =
            match import.Imports.kind with
            | Imports.Default -> (Export_index.Default, module_name)
            | Imports.Named -> (Export_index.Named, import.export)
            | Imports.Namespace -> (Export_index.Namespace, module_name)
            | Imports.NamedType -> (Export_index.NamedType, import.export)
            | Imports.Unknown -> failwith "Unknown Kind"
          in
          (match source with
          | Modulename.Filename fn ->
            let file_key = File_key fn in
            Export_index.add name file_key kind acc
          | Modulename.String _ ->
            (match provider source with
            | Some file ->
              let file_key = File_key (Parsing_heaps.read_file_key file) in
              Export_index.add name file_key kind acc
            | None -> acc))
        | Some (Error mapped_name) ->
          let mref = Option.value mapped_name ~default:mref in
          let (kind, name) =
            match import.Imports.kind with
            | Imports.Default -> (Export_index.Default, mref)
            | Imports.Named -> (Export_index.Named, import.export)
            | Imports.Namespace -> (Export_index.Namespace, mref)
            | Imports.NamedType -> (Export_index.NamedType, import.export)
            | Imports.Unknown -> failwith "Unknown Kind"
          in
          Export_index.add name (Builtin mref) kind acc
        | None ->
          (*Could not find resolved_requires key for this unresolved_source*)
          acc)
  )

(** [add_exports_of_checked_file file_key parse haste_info index] extracts the
    exports of [file_key] from its [parse] entry in shared memory. [haste_info]
    is used to fetch the module name from [file_key]. *)
let add_exports_of_checked_file file_key parse haste_info index =
  let source = Export_index.File_key file_key in
  let exports = Parsing_heaps.read_exports parse in
  let module_name =
    match haste_info with
    | Some info ->
      let name = Parsing_heaps.read_module_name info in
      Modulename.String name
    | None -> Modulename.Filename (Files.chop_flow_ext file_key)
  in
  add_exports ~source ~module_name exports index

(** Adds builtins to [index]. See [Exports.of_builtins] for how libdefs
    are converted as if they "export" things. *)
let add_exports_of_builtins lib_exports index =
  Base.List.fold lib_exports ~init:index ~f:(fun acc export ->
      match export with
      | Exports.Module (module_name, exports) ->
        let source = Export_index.Builtin module_name in
        let module_name = Modulename.String module_name in
        add_exports ~source ~module_name exports acc
      | Exports.Named name -> Export_index.add name Global Named acc
      | Exports.NamedType name -> Export_index.add name Global NamedType acc
      | Exports.Default -> (* impossible *) acc
  )

(** [index_file ~reader (exports_to_add, exports_to_remove) file] reads the exports of [file] from
    shared memory and adds all of the current exports to [exports_to_add], and all of the
    previous exports to [exports_to_remove]. *)
let index_file ~reader (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove) =
  function
  | File_key.ResourceFile _f ->
    (* TODO: where does filename need to be searchable? *)
    (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove)
  | file_key ->
    (* TODO: when a file changes, the below removes the file entirely and then adds
        back the new info, even though much or all of it is probably still the same.
       instead, diff the old and new exports and make minimal changes. *)
    (match Parsing_heaps.get_file_addr file_key with
    | None -> (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove)
    | Some file ->
      let (exports_to_remove, imports_to_remove) =
        (* get old exports so we can remove outdated entries *)
        match Parsing_heaps.Mutator_reader.get_old_typed_parse ~reader file with
        | Some parse ->
          let imports = Parsing_heaps.read_imports parse in
          let haste_info = Parsing_heaps.Mutator_reader.get_old_haste_info ~reader file in
          let resolved_modules =
            Parsing_heaps.Mutator_reader.get_old_resolved_modules_unsafe ~reader file_key parse
          in
          let provider module_name =
            Parsing_heaps.Mutator_reader.get_old_provider ~reader module_name
          in
          ( add_exports_of_checked_file file_key parse haste_info exports_to_remove,
            add_imports imports resolved_modules provider imports_to_remove
          )
        | None ->
          (* if it wasn't checked before, there were no entries added *)
          (exports_to_remove, imports_to_remove)
      in
      let (exports_to_add, imports_to_add) =
        match Parsing_heaps.Mutator_reader.get_typed_parse ~reader file with
        | Some parse ->
          let imports = Parsing_heaps.read_imports parse in
          let haste_info = Parsing_heaps.Mutator_reader.get_haste_info ~reader file in
          let resolved_modules =
            Parsing_heaps.Mutator_reader.get_resolved_modules_unsafe ~reader file_key parse
          in
          let provider module_name =
            Parsing_heaps.Mutator_reader.get_provider ~reader module_name
          in
          ( add_exports_of_checked_file file_key parse haste_info exports_to_add,
            add_imports imports resolved_modules provider imports_to_add
          )
        | None ->
          (* TODO: handle unchecked module names, maybe still parse? *)
          (exports_to_add, imports_to_add)
      in
      (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove))

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
    let (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove) =
      Base.List.fold files ~init ~f:(index_file ~reader)
    in
    let count = Base.List.length files in
    (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove, count)
  in

  MonitorRPC.status_update
    ~event:ServerStatus.(Indexing_progress { finished = 0; total = Some total_count });
  (* each job returns two Export_index.t's. we cons them onto lists and merge them all
     afterwards because the ~merge function blocks talking to workers so it needs to
     be as fast as possible in order to keep the workers busy. [Export_index.merge]
     was found to be slow enough for this to matter. *)
  let%lwt (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove, _count) =
    MultiWorkerLwt.call
      workers
      ~job:(fun _neutral -> job ~reader)
      ~neutral:([], [], [], [], 0)
      ~merge:
        (fun (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove, count)
             ( exports_to_add_acc,
               exports_to_remove_acc,
               imports_to_add_acc,
               imports_to_remove_acc,
               finished
             ) ->
        let finished = finished + count in
        MonitorRPC.status_update
          ~event:ServerStatus.(Indexing_progress { total = Some total_count; finished });
        ( exports_to_add :: exports_to_add_acc,
          exports_to_remove :: exports_to_remove_acc,
          imports_to_add :: imports_to_add_acc,
          imports_to_remove :: imports_to_remove_acc,
          finished
        ))
      ~next:(MultiWorkerLwt.next workers parsed)
  in

  let exports_to_add =
    Base.List.fold exports_to_add ~init:Export_index.empty ~f:(fun acc index ->
        Export_index.merge index acc
    )
  in
  let exports_to_remove =
    Base.List.fold exports_to_remove ~init:Export_index.empty ~f:(fun acc index ->
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

  Lwt.return (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove)

(** Initializes an [Export_search.t] with the exports of all of the [parsed] files
    as well as the builtin libdefs. *)
let init ~workers ~reader ~libs parsed =
  let%lwt (exports_to_add, _exports_to_remove, imports_to_add, _imports_to_remove) =
    index ~workers ~reader parsed
  in
  let exports_to_add = add_exports_of_builtins libs exports_to_add in
  let final_export_index = Export_index.merge_export_import imports_to_add exports_to_add in
  (* TODO: assert that _exports_to_remove is empty? should be on init *)
  Lwt.return (Export_search.init final_export_index)

(** [update ~changed previous] updates the exports for all of the [changed] files
    in the [previous] [Export_search.t]. *)
let update ~workers ~reader ~update ~remove previous : Export_search.t Lwt.t =
  let dirty_files = Utils_js.FilenameSet.union update remove in
  let%lwt (exports_to_add, exports_to_remove, imports_to_add, imports_to_remove) =
    index ~workers ~reader dirty_files
  in
  previous
  |> Export_search.subtract exports_to_remove
  |> Export_search.merge exports_to_add
  |> Export_search.subtract_count imports_to_remove
  |> Export_search.merge_export_import imports_to_add
  |> Lwt.return

module For_test = struct
  let string_of_modulename = string_of_modulename
end
