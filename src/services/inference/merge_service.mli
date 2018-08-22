(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type 'a merge_results = (File_key.t * ('a, Flow_error.error_message) result) list
type 'a merge_job =
  worker_mutator: Context_heaps.Merge_context_mutator.worker_mutator ->
  options:Options.t ->
  'a merge_results ->
  File_key.t Nel.t ->
  'a merge_results

type merge_strict_context_result = {
  cx: Context.t;
  other_cxs: Context.t list;
  master_cx: Context.sig_t;
  file_sigs: File_sig.t FilenameMap.t;
}

val merge_strict_context:
  options: Options.t ->
  File_key.t Nel.t ->
  merge_strict_context_result

val merge_contents_context:
  Options.t ->
  File_key.t ->
  (Loc.t, Loc.t) Ast.program ->
  Docblock.t ->
  File_sig.t ->
  Context.t * (Loc.t, Loc.t * Type.t) Ast.program

val merge_runner:
  job: 'a merge_job ->
  master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
  worker_mutator: Context_heaps.Merge_context_mutator.worker_mutator ->
  intermediate_result_callback: ('a merge_results Lazy.t -> unit) ->
  options: Options.t ->
  workers: MultiWorkerLwt.worker list option ->
  FilenameSet.t FilenameMap.t ->
  (File_key.t Nel.t) FilenameMap.t ->
  bool FilenameMap.t ->
  'a merge_results Lwt.t

val merge_strict:
  master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
  worker_mutator: Context_heaps.Merge_context_mutator.worker_mutator ->
  intermediate_result_callback:
    ((Errors.ErrorSet.t *
      Error_suppressions.t *
      ExactCover.lint_severity_cover) merge_results Lazy.t -> unit) ->
  options: Options.t ->
  workers: MultiWorkerLwt.worker list option ->
  FilenameSet.t FilenameMap.t ->
  (File_key.t Nel.t) FilenameMap.t ->
  bool FilenameMap.t ->
  (Errors.ErrorSet.t * Error_suppressions.t * ExactCover.lint_severity_cover) merge_results Lwt.t
