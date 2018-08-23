(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

val init:
  profiling:Profiling_js.running ->
  workers:MultiWorkerLwt.worker list option ->
  Options.t ->
  (bool (* libs_ok *) * ServerEnv.env) Lwt.t

val calc_deps:
  options:Options.t ->
  profiling:Profiling_js.running ->
  dependency_graph:FilenameSet.t FilenameMap.t ->
  components:File_key.t Nel.t list ->
  FilenameSet.t ->
  (FilenameSet.t FilenameMap.t * File_key.t Nel.t FilenameMap.t) Lwt.t

(* incremental typecheck entry point *)
val recheck:
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  updates:FilenameSet.t ->
  ServerEnv.env ->
  files_to_focus:FilenameSet.t ->
  (Profiling_js.finished * ServerStatus.summary * ServerEnv.env) Lwt.t

(* initial (full) check *)
val full_check:
  profiling:Profiling_js.running ->
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  focus_targets:FilenameSet.t option ->
  ServerEnv.env ->
  ServerEnv.env Lwt.t

val basic_check_contents:
  options: Options.t ->
  workers: MultiWorkerLwt.worker list option ->
  env: ServerEnv.env ref ->
  profiling: Profiling_js.running ->
  string ->               (* contents *)
  File_key.t ->           (* fake file-/module name *)
  (Context.t *
   Docblock.t *
   File_sig.t *
   (Loc.t, Loc.t * Type.t) Ast.program,
   string) result Lwt.t

val typecheck_contents:
  options: Options.t ->
  workers: MultiWorkerLwt.worker list option ->
  env: ServerEnv.env ref ->
  profiling: Profiling_js.running ->
  string ->                                 (* contents *)
  File_key.t ->                             (* fake file-/module name *)
  ((Context.t *
    (Loc.t, Loc.t) Ast.program *
    File_sig.t *
    (Loc.t, Loc.t * Type.t) Ast.program) option *
   Errors.ErrorSet.t *                      (* errors *)
   Errors.ErrorSet.t) Lwt.t                 (* warnings *)

val ensure_checked_dependencies:
  options: Options.t ->
  profiling: Profiling_js.running ->
  workers: MultiWorkerLwt.worker list option ->
  env: ServerEnv.env ref ->
  File_key.t ->
  File_sig.t ->
  unit Lwt.t
