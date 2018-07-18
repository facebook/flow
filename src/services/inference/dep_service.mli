(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

val dependent_files:
  MultiWorkerLwt.worker list option -> (* workers *)
  unchanged:FilenameSet.t ->
  new_or_changed:FilenameSet.t ->
  changed_modules:Modulename.Set.t ->
  (* (transitive_dependents, direct_dependents) of changed_modules *)
  (FilenameSet.t * FilenameSet.t) Lwt.t

val calc_dependency_graph:
  MultiWorkerLwt.worker list option -> (* workers *)
  parsed:FilenameSet.t ->
  FilenameSet.t FilenameMap.t Lwt.t

val calc_partial_dependency_graph:
  MultiWorkerLwt.worker list option -> (* workers *)
  FilenameSet.t -> (* files *)
  parsed:FilenameSet.t ->
  FilenameSet.t FilenameMap.t Lwt.t

val filter_dependency_graph:
  FilenameSet.t FilenameMap.t -> (* dependency graph *)
  FilenameSet.t -> (* files *)
  FilenameSet.t FilenameMap.t

val calc_all_dependencies:
  FilenameSet.t FilenameMap.t -> (* dependency graph *)
  FilenameSet.t -> (* files *)
  FilenameSet.t

val calc_all_reverse_dependencies:
  FilenameSet.t FilenameMap.t ->
  FilenameSet.t ->
  FilenameSet.t
