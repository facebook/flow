(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type dependency_graph = FilenameSet.t FilenameMap.t

(* `closure graph files` returns all files in `graph` which are reachable from `files`, directly or
 * indirectly. *)
let closure =
  let rec helper graph =
    FilenameSet.fold (fun file acc ->
      match FilenameMap.get file graph with
        | Some files ->
          let files = FilenameSet.diff files acc in
          let acc = FilenameSet.union files acc in
          helper graph files acc
        | None -> acc
    )
  in
  fun graph files -> helper graph files files

let reverse graph =
  let acc = Hashtbl.create 0 in
  FilenameMap.iter (fun f -> FilenameSet.iter (fun f' ->
    Hashtbl.add acc f' f
  )) graph;
  FilenameMap.mapi (fun f _ ->
    FilenameSet.of_list @@ Hashtbl.find_all acc f
  ) graph

(* `calc_direct_dependencies graph files` will return the set of direct dependencies of
   `files`. This set includes `files`. *)
let calc_direct_dependencies dependency_graph files =
  FilenameSet.fold (fun file acc ->
    match FilenameMap.get file dependency_graph with
      | Some files -> FilenameSet.union files acc
      | None -> acc
  ) files files

(* `calc_all_dependencies graph files` will return the set of direct and transitive dependencies
 * of `files`. This set does include `files`.
 *)
let calc_all_dependencies dependency_graph files =
  closure dependency_graph files

(* `calc_all_dependents graph files` will return the set of direct and transitive dependents of
   `files`. This set include `files`.

   A file is a dependent of `files` whenever its code depends on any file whose *signature*, in
   turn, directly or transitively depends on `files`.  *)
let calc_all_dependents ~dependency_graph ~all_dependency_graph files =
  let rev_dependency_graph = reverse dependency_graph in
  let all_type_dependents = closure rev_dependency_graph files in
  FilenameMap.fold (fun f code_dependencies acc ->
    if not (FilenameSet.mem f all_type_dependents)
       && FilenameSet.exists (fun f' -> FilenameSet.mem f' all_type_dependents) code_dependencies
    then FilenameSet.add f acc
    else acc
  ) all_dependency_graph all_type_dependents

(* Returns a copy of the dependency graph with only those file -> dependency edges where file and
   dependency are in files *)
let filter_dependency_graph dependency_graph files =
  FilenameSet.fold (fun f ->
    let fs = FilenameMap.find_unsafe f dependency_graph |> FilenameSet.inter files in
    FilenameMap.add f fs
  ) files FilenameMap.empty
