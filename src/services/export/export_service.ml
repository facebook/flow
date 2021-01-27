(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Export_index
open Utils_js

let camelize str =
  match String.split_on_char '-' str with
  | [] -> str
  | [str] -> str
  | hd :: rest ->
    let parts = hd :: Base.List.map ~f:String.capitalize_ascii rest in
    String.concat "" parts

let string_of_modulename modulename =
  (* TODO: need to handle reserved words, like a file called package.js *)
  let str =
    match modulename with
    | Modulename.String str -> str
    | Modulename.Filename f -> Filename.basename (File_key.to_string f)
  in
  let stripped =
    match String.index_opt str '.' with
    | Some index -> String.sub str 0 index
    | None -> str
  in
  camelize stripped

let entry_of_export ~module_name = function
  | Exports.Default -> (string_of_modulename module_name, Default)
  | Exports.Named name -> (name, Named)
  | Exports.NamedType name -> (name, NamedType)

let entries_of_exports ~module_name exports =
  let (has_named, entries) =
    Base.List.fold_map
      ~f:(fun has_named export ->
        let entry = entry_of_export ~module_name export in
        let has_named =
          match entry with
          | (_, Named) -> true
          | _ -> has_named
        in
        (has_named, entry))
      ~init:false
      exports
  in
  if has_named then
    (string_of_modulename module_name, Namespace) :: entries
  else
    entries

let add_imports_of_exports ~file_key ~info ~exports index =
  let module_name = info.Module_heaps.module_name in
  let names = entries_of_exports ~module_name exports in
  Base.List.fold_left
    ~f:(fun acc (name, kind) -> Export_index.add name file_key kind acc)
    ~init:index
    names

let index ~workers ~reader parsed : (Export_index.t * Export_index.t) Lwt.t =
  let total_count = FilenameSet.cardinal parsed in
  let parsed = FilenameSet.elements parsed in
  let job =
    let f ~audit ~reader (to_add, to_remove) = function
      | File_key.ResourceFile _f ->
        (* TODO: where does filename need to be searchable? *)
        (to_add, to_remove)
      | file_key ->
        (* TODO: when a file changes, the below removes the file entirely and then adds
           back the new info, even though much or all of it is probably still the same.
          instead, diff the old and new exports and make minimal changes. *)
        let to_remove =
          (* get old exports so we can remove outdated entries *)
          match Module_heaps.Mutator_reader.get_old_info ~reader ~audit file_key with
          | Some info when info.Module_heaps.checked ->
            (match Parsing_heaps.Mutator_reader.get_old_exports ~reader file_key with
            | Some exports -> add_imports_of_exports ~file_key ~info ~exports to_remove
            | None -> to_remove)
          | _ ->
            (* if it wasn't checked before, there were no entries added *)
            to_remove
        in
        let to_add =
          match Module_heaps.Mutator_reader.get_info ~reader ~audit file_key with
          | Some info when info.Module_heaps.checked ->
            (match Parsing_heaps.Mutator_reader.get_exports ~reader file_key with
            | Some exports -> add_imports_of_exports ~file_key ~info ~exports to_add
            | None -> to_add)
          | _ ->
            (* TODO: handle unchecked module names, maybe still parse? *)
            to_add
        in
        (to_add, to_remove)
    in
    fun ~reader files ->
      let init = (Export_index.empty, Export_index.empty) in
      let audit = Expensive.ok in
      let (to_add, to_remove) = Base.List.fold_left ~f:(f ~audit ~reader) ~init files in
      let count = Base.List.length files in
      (to_add, to_remove, count)
  in

  MonitorRPC.status_update
    ServerStatus.(Indexing_progress { finished = 0; total = Some total_count });
  let%lwt (to_add, to_remove, _count) =
    MultiWorkerLwt.call
      workers
      ~job:(fun _neutral -> job ~reader)
      ~neutral:([], [], 0)
      ~merge:(fun (to_add, to_remove, count) (to_add_acc, to_remove_acc, finished) ->
        let finished = finished + count in
        MonitorRPC.status_update
          ServerStatus.(Indexing_progress { total = Some total_count; finished });
        (to_add :: to_add_acc, to_remove :: to_remove_acc, finished))
      ~next:(MultiWorkerLwt.next workers parsed)
  in

  let to_add =
    Base.List.fold_left
      ~f:(fun acc index -> Export_index.merge index acc)
      ~init:Export_index.empty
      to_add
  in
  let to_remove =
    Base.List.fold_left
      ~f:(fun acc index -> Export_index.merge index acc)
      ~init:Export_index.empty
      to_remove
  in

  Lwt.return (to_add, to_remove)

let init ~workers ~reader parsed =
  let%lwt (to_add, _to_remove) = index ~workers ~reader parsed in
  (* TODO: assert that _to_remove is empty? should be on init *)
  Lwt.return (Export_search.init to_add)

let update ~workers ~reader ~update ~remove previous : Export_search.t Lwt.t =
  let dirty_files = FilenameSet.union update remove in
  let%lwt (to_add, to_remove) = index ~workers ~reader dirty_files in
  previous |> Export_search.subtract to_remove |> Export_search.merge to_add |> Lwt.return

module For_test = struct
  let string_of_modulename = string_of_modulename
end
