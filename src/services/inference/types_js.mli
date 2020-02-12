(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

val init :
  profiling:Profiling_js.running ->
  workers:MultiWorkerLwt.worker list option ->
  Options.t ->
  (bool (* libs_ok *) * ServerEnv.env * Recheck_stats.estimates option) Lwt.t

val calc_deps :
  options:Options.t ->
  profiling:Profiling_js.running ->
  sig_dependency_graph:FilenameGraph.t ->
  components:File_key.t Nel.t list ->
  FilenameSet.t ->
  (FilenameSet.t FilenameMap.t * File_key.t Nel.t FilenameMap.t) Lwt.t

(* incremental typecheck entry point *)
val recheck :
  profiling:Profiling_js.running ->
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  updates:FilenameSet.t ->
  ServerEnv.env ->
  files_to_force:CheckedSet.t ->
  file_watcher_metadata:MonitorProt.file_watcher_metadata ->
  recheck_reasons:LspProt.recheck_reason list ->
  will_be_checked_files:CheckedSet.t ref ->
  ((profiling:Profiling_js.finished -> unit) * ServerStatus.summary_info * ServerEnv.env) Lwt.t

(* initial (full) check *)
val full_check :
  profiling:Profiling_js.running ->
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  ?focus_targets:FilenameSet.t ->
  ServerEnv.env ->
  (ServerEnv.env * string option) Lwt.t

val type_contents :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  string ->
  (* contents *)
  File_key.t ->
  (* fake file-/module name *)
  ( Context.t
    * Docblock.t
    * File_sig.With_Loc.t
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.program
    * (Loc.t * Parse_error.t) list,
    string )
  result
  Lwt.t

val typecheck_contents :
  options:Options.t ->
  env:ServerEnv.env ->
  profiling:Profiling_js.running ->
  string ->
  (* contents *)
  File_key.t ->
  (* fake file-/module name *)
  ( ( Context.t
    * (Loc.t, Loc.t) Flow_ast.program
    * File_sig.With_Loc.t
    * (ALoc.t, ALoc.t * Type.t) Flow_ast.program )
    option
  * Errors.ConcreteLocPrintableErrorSet.t
  * (* errors *)
    Errors.ConcreteLocPrintableErrorSet.t )
  Lwt.t

(* warnings *)

val ensure_checked_dependencies :
  options:Options.t ->
  reader:State_reader.t ->
  env:ServerEnv.env ->
  File_key.t ->
  File_sig.With_Loc.t ->
  unit Lwt.t

(* The following are exposed only for testing purposes. Not meant for general consumption. *)

type determine_what_to_recheck_result =
  | Determine_what_to_recheck_result of {
      to_merge: CheckedSet.t;
      to_check: CheckedSet.t;
      (* union of to_merge and to_check *)
      to_merge_or_check: CheckedSet.t;
      components: File_key.t Nel.t list;
      recheck_set: FilenameSet.t;
      sig_dependent_files: FilenameSet.t;
      all_dependent_files: FilenameSet.t;
    }

val debug_determine_what_to_recheck :
  profiling:Profiling_js.running ->
  options:Options.t ->
  is_file_checked:(File_key.t -> bool) ->
  ide_open_files:SSet.t Lazy.t ->
  sig_dependency_graph:FilenameGraph.t ->
  implementation_dependency_graph:FilenameGraph.t ->
  checked_files:CheckedSet.t ->
  freshparsed:FilenameSet.t ->
  unparsed_set:FilenameSet.t ->
  deleted:FilenameSet.t ->
  unchanged_checked:CheckedSet.t ->
  files_to_force:CheckedSet.t ->
  unchanged_files_to_force:CheckedSet.t ->
  direct_dependent_files:FilenameSet.t ->
  determine_what_to_recheck_result Lwt.t

val debug_include_dependencies_and_dependents :
  options:Options.t ->
  profiling:Profiling_js.running ->
  unchanged_checked:CheckedSet.t ->
  input:CheckedSet.t ->
  implementation_dependency_graph:FilenameGraph.t ->
  sig_dependency_graph:FilenameGraph.t ->
  sig_dependent_files:FilenameSet.t ->
  all_dependent_files:FilenameSet.t ->
  (CheckedSet.t * CheckedSet.t * CheckedSet.t * File_key.t Nel.t list * FilenameSet.t) Lwt.t

val include_dependencies_and_dependents :
  options:Options.t ->
  profiling:Profiling_js.running ->
  unchanged_checked:CheckedSet.t ->
  input:CheckedSet.t ->
  implementation_dependency_graph:Utils_js.FilenameGraph.t ->
  sig_dependency_graph:Utils_js.FilenameGraph.t ->
  sig_dependent_files:Utils_js.FilenameSet.t ->
  all_dependent_files:Utils_js.FilenameSet.t ->
  (CheckedSet.t * CheckedSet.t * CheckedSet.t * File_key.t Nel.t list * Utils_js.FilenameSet.t)
  Lwt.t
