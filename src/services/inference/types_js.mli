(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

val init:
  profiling:Profiling_js.running ->
  workers:MultiWorkerLwt.worker list option ->
  Options.t ->
  (bool (* libs_ok *) * ServerEnv.env * Recheck_stats.estimates option) Lwt.t

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
  files_to_force:CheckedSet.t ->
  file_watcher_metadata:MonitorProt.file_watcher_metadata ->
  recheck_reasons:Persistent_connection_prot.recheck_reason list ->
  will_be_checked_files:CheckedSet.t ref ->
  (Profiling_js.finished * ServerStatus.summary * ServerEnv.env) Lwt.t

(* initial (full) check *)
val full_check:
  profiling:Profiling_js.running ->
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  ?focus_targets:FilenameSet.t ->
  ServerEnv.env ->
  ServerEnv.env Lwt.t

val basic_check_contents:
  options: Options.t ->
  env: ServerEnv.env ->
  profiling: Profiling_js.running ->
  string ->               (* contents *)
  File_key.t ->           (* fake file-/module name *)
  (Context.t *
   Docblock.t *
   File_sig.With_Loc.t *
   (ALoc.t, ALoc.t * Type.t) Flow_ast.program,
   string) result Lwt.t

val typecheck_contents:
  options: Options.t ->
  env: ServerEnv.env ->
  profiling: Profiling_js.running ->
  string ->                                 (* contents *)
  File_key.t ->                             (* fake file-/module name *)
  ((Context.t *
    (Loc.t, Loc.t) Flow_ast.program *
    File_sig.With_Loc.t *
    (ALoc.t, ALoc.t * Type.t) Flow_ast.program) option *
   Errors.ConcreteLocPrintableErrorSet.t *                      (* errors *)
   Errors.ConcreteLocPrintableErrorSet.t) Lwt.t                 (* warnings *)

val ensure_checked_dependencies:
  options: Options.t ->
  reader: State_reader.t ->
  env: ServerEnv.env ->
  File_key.t ->
  File_sig.With_Loc.t ->
  unit Lwt.t
