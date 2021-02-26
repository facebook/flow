(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type element = Component of File_key.t Nel.t

type 'a merge_result = (File_key.t * 'a) list

type 'a t

val create :
  num_workers:int ->
  reader:Mutator_state_reader.t ->
  sig_dependency_graph:FilenameSet.t FilenameMap.t ->
  leader_map:File_key.t FilenameMap.t ->
  component_map:File_key.t Nel.t FilenameMap.t ->
  recheck_leader_set:FilenameSet.t ->
  intermediate_result_callback:('a merge_result Lazy.t -> unit) ->
  'a t

val update_server_status : 'a t -> unit

val next : 'a t -> unit -> element list Bucket.bucket

val merge :
  master_mutator:Context_heaps.Merge_context_mutator.master_mutator ->
  reader:Mutator_state_reader.t ->
  'a t ->
  'a merge_result ->
  'a merge_result ->
  'a merge_result

val total_files : 'a t -> int

val skipped_count : 'a t -> int

val sig_new_or_changed : 'a t -> FilenameSet.t
