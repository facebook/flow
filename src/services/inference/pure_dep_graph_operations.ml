(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

(* `rdep_closure graph files` returns all files in `graph` which are reachable
 * from `files`, directly or indirectly. *)
let rdep_closure =
  let rec loop graph files acc =
    FilenameSet.fold
      (fun file acc ->
        let entry = FilenameGraph.find_backward_opt file graph in
        match entry with
        | Some files ->
          let files = FilenameSet.diff files acc in
          let acc = FilenameSet.union files acc in
          loop graph files acc
        | None -> acc)
      files
      acc
  in
  (fun graph files -> loop graph files files)

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

(** [calc_direct_dependents graph files] will return the set of direct dependents of
  [files]. This set includes [files]. *)
let calc_direct_dependents dependency_graph files =
  FilenameSet.fold
    (fun file acc ->
      match FilenameGraph.find_backward_opt file dependency_graph with
      | Some files -> FilenameSet.union files acc
      | None -> acc)
    files
    files

(** [calc_all_dependents graph files] will return the set of direct and transitive dependents of
  [files]. This set include [files].

  A file is a dependent of [files] whenever its code depends on any file whose *signature*, in
  turn, directly or transitively depends on [files]. *)
let calc_all_dependents ~sig_dependency_graph ~implementation_dependency_graph files =
  let all_sig_dependents = rdep_closure sig_dependency_graph files in
  let all_dependents = calc_direct_dependents implementation_dependency_graph all_sig_dependents in
  (all_sig_dependents, all_dependents)
