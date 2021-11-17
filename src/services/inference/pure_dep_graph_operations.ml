(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type graph_direction =
  | Forward
  | Backward

(* `closure graph files` returns all files in `graph` which are reachable from `files`, directly or
 * indirectly. *)
let closure =
  let rec helper graph find_opt files acc =
    FilenameSet.fold
      (fun file acc ->
        let entry = find_opt file graph in
        match entry with
        | Some files ->
          let files = FilenameSet.diff files acc in
          let acc = FilenameSet.union files acc in
          helper graph find_opt files acc
        | None -> acc)
      files
      acc
  in
  fun graph direction files ->
    let find_opt =
      match direction with
      | Forward -> FilenameGraph.find_opt
      | Backward -> FilenameGraph.find_backward_opt
    in
    helper graph find_opt files files

(* `calc_direct_dependencies graph files` will return the set of direct dependencies of
   `files`. This set includes `files`. *)
let calc_direct_dependencies dependency_graph files =
  FilenameSet.fold
    (fun file acc ->
      match FilenameGraph.find_opt file dependency_graph with
      | Some files -> FilenameSet.union files acc
      | None -> acc)
    files
    files

(* `calc_all_dependencies graph files` will return the set of direct and transitive dependencies
 * of `files`. This set does include `files`.
 *)
let calc_all_dependencies dependency_graph files = closure dependency_graph Forward files

(** `calc_direct_dependents graph files` will return the set of direct dependents of
    `files`. This set includes `files`. *)
let calc_direct_dependents dependency_graph files =
  FilenameSet.fold
    (fun file acc ->
      match FilenameGraph.find_backward_opt file dependency_graph with
      | Some files -> FilenameSet.union files acc
      | None -> acc)
    files
    files

(* `calc_all_dependents graph files` will return the set of direct and transitive dependents of
   `files`. This set include `files`.

   A file is a dependent of `files` whenever its code depends on any file whose *signature*, in
   turn, directly or transitively depends on `files`.  *)
let calc_all_dependents ~sig_dependency_graph ~implementation_dependency_graph files =
  let all_sig_dependents = closure sig_dependency_graph Backward files in
  let all_dependents = calc_direct_dependents implementation_dependency_graph all_sig_dependents in
  (all_sig_dependents, all_dependents)

(* Returns a copy of the dependency graph with only those file -> dependency edges where file and
   dependency are in files *)
let filter_dependency_graph dependency_graph files =
  FilenameSet.fold
    (fun f ->
      let fs = FilenameGraph.find f dependency_graph |> FilenameSet.inter files in
      FilenameMap.add f fs)
    files
    FilenameMap.empty
