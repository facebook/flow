(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

val init:
  profiling:Profiling_js.running ->
  workers:WorkerController.worker list option ->
  Options.t ->
  FilenameSet.t * SSet.t * bool * ServerEnv.errors

val calc_deps:
  options:Options.t ->
  profiling:Profiling_js.running ->
  workers:WorkerController.worker list option ->
  FilenameSet.t ->
  FilenameSet.t FilenameMap.t * File_key.t Nel.t FilenameMap.t

(* incremental typecheck entry point *)
val recheck:
  options:Options.t ->
  workers:WorkerController.worker list option ->
  updates:FilenameSet.t ->
  ServerEnv.env ->
  force_focus:bool ->
  Profiling_js.finished * ServerEnv.env

(* initial (full) check *)
val full_check:
  profiling:Profiling_js.running ->
  options:Options.t ->
  workers:WorkerController.worker list option ->
  focus_targets:FilenameSet.t option ->
  FilenameSet.t ->
  ServerEnv.errors ->
  CheckedSet.t * ServerEnv.errors

 val basic_check_contents:
   options: Options.t ->
   workers: WorkerController.worker list option ->
   env: ServerEnv.env ref ->
   profiling: Profiling_js.running ->
   string ->               (* contents *)
   File_key.t ->           (* fake file-/module name *)
   (Context.t *
    Docblock.t,
    string) result

val typecheck_contents:
  options: Options.t ->
  workers: WorkerController.worker list option ->
  env: ServerEnv.env ref ->
  profiling: Profiling_js.running ->
  string ->               (* contents *)
  File_key.t ->           (* fake file-/module name *)
  Errors.ErrorSet.t *     (* errors *)
  Errors.ErrorSet.t       (* warnings *)

val ensure_checked_dependencies:
  options: Options.t ->
  profiling: Profiling_js.running ->
  workers: WorkerController.worker list option ->
  env: ServerEnv.env ref ->
  Modulename.Set.t ->
  unit
