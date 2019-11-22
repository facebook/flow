(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

val calc_direct_dependents :
  reader:Abstract_state_reader.t ->
  MultiWorkerLwt.worker list option ->
  candidates:(* workers *)
             FilenameSet.t ->
  root_files:FilenameSet.t ->
  root_modules:Modulename.Set.t ->
  (* direct_dependents of changed_modules *)
  FilenameSet.t Lwt.t

val dependency_info_of_dependency_graph :
  options:Options.t -> (FilenameSet.t * FilenameSet.t) FilenameMap.t -> Dependency_info.t

val calc_dependency_info :
  options:Options.t ->
  reader:Mutator_state_reader.t ->
  MultiWorkerLwt.worker list option ->
  parsed:(* workers *)
         FilenameSet.t ->
  Dependency_info.t Lwt.t

val calc_partial_dependency_graph :
  reader:Mutator_state_reader.t ->
  MultiWorkerLwt.worker list option ->
  (* workers *)
  FilenameSet.t ->
  parsed:(* files *)
         FilenameSet.t ->
  (FilenameSet.t * FilenameSet.t) FilenameMap.t Lwt.t
