(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

val implementation_file :
  reader:Mutator_state_reader.t -> (Modulename.t -> File_key.t option) Expensive.t

val calc_direct_dependents :
  MultiWorkerLwt.worker list option ->
  candidates:FilenameSet.t ->
  root_files:FilenameSet.t ->
  root_modules:Modulename.Set.t ->
  FilenameSet.t Lwt.t

val calc_dependency_info :
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
  Dependency_info.partial_dependency_graph Lwt.t
