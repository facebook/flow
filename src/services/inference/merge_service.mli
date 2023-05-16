(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type 'a unit_result = ('a, ALoc.t * Error_message.internal_error) result

type sig_opts_data = {
  skipped_count: int;
  sig_new_or_changed: FilenameSet.t;
}

type 'a merge_results = 'a list * sig_opts_data

type 'a merge_job =
  mutator:Parsing_heaps.Merge_context_mutator.t ->
  options:Options.t ->
  for_find_all_refs:bool ->
  reader:Mutator_state_reader.t ->
  File_key.t Nel.t ->
  bool * 'a

val sig_hash :
  check_dirty_set:bool ->
  root:Path.t ->
  reader:Mutator_state_reader.t ->
  Parsing_heaps.component_file Nel.t ->
  Xx.hash

val check_contents_cache : Check_cache.t

val check_contents_context :
  reader:State_reader.t ->
  Options.t ->
  Context.master_context ->
  File_key.t ->
  (Loc.t, Loc.t) Flow_ast.Program.t ->
  Docblock.t ->
  File_sig.t ->
  Context.t * (ALoc.t, ALoc.t * Type.t) Flow_ast.Program.t

val merge_runner :
  job:'a merge_job ->
  mutator:Parsing_heaps.Merge_context_mutator.t ->
  reader:Mutator_state_reader.t ->
  options:Options.t ->
  for_find_all_refs:bool ->
  workers:MultiWorkerLwt.worker list option ->
  sig_dependency_graph:FilenameGraph.t ->
  components:File_key.t Nel.t list ->
  recheck_set:FilenameSet.t ->
  'a merge_results Lwt.t

val merge :
  mutator:Parsing_heaps.Merge_context_mutator.t ->
  reader:Mutator_state_reader.t ->
  options:Options.t ->
  for_find_all_refs:bool ->
  workers:MultiWorkerLwt.worker list option ->
  sig_dependency_graph:FilenameGraph.t ->
  components:File_key.t Nel.t list ->
  recheck_set:FilenameSet.t ->
  Types_js_types.merge_result option merge_results Lwt.t

val mk_check :
  Options.t ->
  reader:Parsing_heaps.Mutator_reader.reader ->
  master_cx:Context.master_context ->
  def_info:GetDefUtils.def_info option ->
  unit ->
  File_key.t ->
  Types_js_types.check_result option unit_result
