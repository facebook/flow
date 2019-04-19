(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type 'a merge_job_result = ('a, Error_message.t) result
type 'a merge_job_results = (File_key.t * 'a merge_job_result) list
type 'a merge_job =
  worker_mutator: Context_heaps.Merge_context_mutator.worker_mutator ->
  options:Options.t ->
  reader: Mutator_state_reader.t ->
  File_key.t Nel.t ->
  'a merge_job_result

type 'a merge_results = 'a merge_job_results * int (* skipped count *)

type merge_strict_context_result = {
  cx: Context.t;
  other_cxs: Context.t list;
  master_cx: Context.sig_t;
  file_sigs: File_sig.With_ALoc.t FilenameMap.t;
  typed_asts: (ALoc.t, ALoc.t * Type.t) Flow_ast.program FilenameMap.t;
  coverage_map: Coverage.file_coverage FilenameMap.t;
}

val merge_strict_context:
  options: Options.t ->
  reader: Abstract_state_reader.t ->
  File_key.t Nel.t ->
  merge_strict_context_result

val merge_contents_context:
  reader: State_reader.t ->
  Options.t ->
  File_key.t ->
  (Loc.t, Loc.t) Flow_ast.program ->
  Docblock.t ->
  File_sig.With_Loc.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.program

val check_file:
  Options.t ->
  reader:Module_heaps.Mutator_reader.reader ->
  File_key.t ->
  (Flow_error.ErrorSet.t * Flow_error.ErrorSet.t * Error_suppressions.t
    * Coverage.file_coverage FilenameMap.t)

val merge_runner:
  job: 'a merge_job ->
  master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
  worker_mutator: Context_heaps.Merge_context_mutator.worker_mutator ->
  reader: Mutator_state_reader.t ->
  intermediate_result_callback: ('a merge_job_results Lazy.t -> unit) ->
  options: Options.t ->
  workers: MultiWorkerLwt.worker list option ->
  FilenameSet.t FilenameMap.t ->
  (File_key.t Nel.t) FilenameMap.t ->
  bool FilenameMap.t ->
  'a merge_results Lwt.t

val merge_strict:
  master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
  worker_mutator: Context_heaps.Merge_context_mutator.worker_mutator ->
  reader: Mutator_state_reader.t ->
  intermediate_result_callback:
    ((Flow_error.ErrorSet.t * Flow_error.ErrorSet.t *
      Error_suppressions.t * Coverage.file_coverage FilenameMap.t) merge_job_results Lazy.t -> unit) ->
  options: Options.t ->
  workers: MultiWorkerLwt.worker list option ->
  FilenameSet.t FilenameMap.t ->
  (File_key.t Nel.t) FilenameMap.t ->
  bool FilenameMap.t ->
  (Flow_error.ErrorSet.t * Flow_error.ErrorSet.t  * Error_suppressions.t
    * Coverage.file_coverage FilenameMap.t) merge_results Lwt.t
