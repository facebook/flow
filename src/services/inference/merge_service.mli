(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type duration = float

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type merge_result = Error_suppressions.t * duration

type check_type_result =
  Context.t * File_sig.With_ALoc.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

type check_error_result =
  Flow_error.ErrorSet.t
  * Flow_error.ErrorSet.t
  * Error_suppressions.t
  * Coverage_response.file_coverage
  * duration

type check_result = check_type_result * check_error_result

type sig_opts_data = {
  skipped_count: int;
  sig_new_or_changed: FilenameSet.t;
}

type 'a merge_results = (File_key.t * bool * 'a unit_result) list * sig_opts_data

type 'a merge_job =
  worker_mutator:Context_heaps.Merge_context_mutator.worker_mutator ->
  options:Options.t ->
  reader:Mutator_state_reader.t ->
  File_key.t Nel.t ->
  bool * 'a unit_result

val merge_context :
  options:Options.t ->
  reader:Abstract_state_reader.t ->
  Context.master_context ->
  File_key.t Nel.t ->
  Context.t

val check_contents_cache : New_check_cache.t

val check_contents_context :
  reader:State_reader.t ->
  Options.t ->
  File_key.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Docblock.t ->
  File_sig.With_Loc.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

val merge_runner :
  job:'a merge_job ->
  master_mutator:Context_heaps.Merge_context_mutator.master_mutator ->
  worker_mutator:Context_heaps.Merge_context_mutator.worker_mutator ->
  reader:Mutator_state_reader.t ->
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  sig_dependency_graph:FilenameSet.t FilenameMap.t ->
  component_map:File_key.t Nel.t FilenameMap.t ->
  recheck_set:FilenameSet.t ->
  'a merge_results Lwt.t

val merge :
  master_mutator:Context_heaps.Merge_context_mutator.master_mutator ->
  worker_mutator:Context_heaps.Merge_context_mutator.worker_mutator ->
  reader:Mutator_state_reader.t ->
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  sig_dependency_graph:FilenameSet.t FilenameMap.t ->
  component_map:File_key.t Nel.t FilenameMap.t ->
  recheck_set:FilenameSet.t ->
  merge_result option merge_results Lwt.t

val mk_check :
  Options.t ->
  reader:Module_heaps.Mutator_reader.reader ->
  unit ->
  File_key.t ->
  check_result option unit_result
