(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type 'a file_keyed_result = File_key.t * 'a unit_result

type error_acc =
  Flow_error.ErrorSet.t
  * Flow_error.ErrorSet.t
  * Error_suppressions.t
  * Coverage_response.file_coverage FilenameMap.t option
  * float

type type_acc =
  ( Context.t
  * File_sig.With_ALoc.t FilenameMap.t
  * (ALoc.t, ALoc.t * Type.t) Flow_ast.program Utils_js.FilenameMap.t )
  option

type acc = type_acc * error_acc

(* Time to check *)

type 'a merge_job_results = 'a file_keyed_result list

type 'a merge_job =
  worker_mutator:Context_heaps.Merge_context_mutator.worker_mutator ->
  options:Options.t ->
  reader:Mutator_state_reader.t ->
  File_key.t Nel.t ->
  'a unit_result

type sig_opts_data = {
  skipped_count: int;
  sig_new_or_changed: FilenameSet.t;
}

type 'a merge_results = 'a merge_job_results * sig_opts_data

type merge_context_result = {
  cx: Context.t;
  other_cxs: Context.t list;
  master_cx: Context.sig_t;
  file_sigs: File_sig.With_ALoc.t FilenameMap.t;
  typed_asts: (ALoc.t, ALoc.t * Type.t) Flow_ast.program FilenameMap.t;
  coverage_map: Coverage_response.file_coverage FilenameMap.t option;
}

val merge_context :
  options:Options.t -> reader:Abstract_state_reader.t -> File_key.t Nel.t -> merge_context_result

val merge_contents_context :
  reader:State_reader.t ->
  Options.t ->
  File_key.t ->
  (Loc.t, Loc.t) Flow_ast.program ->
  Docblock.t ->
  File_sig.With_Loc.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.program

val merge_runner :
  job:'a merge_job ->
  master_mutator:Context_heaps.Merge_context_mutator.master_mutator ->
  worker_mutator:Context_heaps.Merge_context_mutator.worker_mutator ->
  reader:Mutator_state_reader.t ->
  intermediate_result_callback:('a merge_job_results Lazy.t -> unit) ->
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
  intermediate_result_callback:(error_acc merge_job_results Lazy.t -> unit) ->
  options:Options.t ->
  workers:MultiWorkerLwt.worker list option ->
  sig_dependency_graph:FilenameSet.t FilenameMap.t ->
  component_map:File_key.t Nel.t FilenameMap.t ->
  recheck_set:FilenameSet.t ->
  error_acc merge_results Lwt.t

val check :
  Options.t -> reader:Module_heaps.Mutator_reader.reader -> File_key.t -> acc file_keyed_result
