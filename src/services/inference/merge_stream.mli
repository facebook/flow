(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type element = Component of File_key.t Nel.t

type merge_stats
val get_total_files: merge_stats -> int
val get_skipped_files: merge_stats -> int

type 'a merge_result = (File_key.t * 'a) list

type 'a merge_stream = {
  next: unit -> element list Bucket.bucket;
  merge:
    master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
    reader: Mutator_state_reader.t ->
    (* merged *)
    'a merge_result ->
    (* accumulator *)
    'a merge_result ->
    (* accumulated results *)
    'a merge_result;
  stats: merge_stats;
}

val make :
  num_workers: int ->
  dependency_graph: FilenameSet.t FilenameMap.t ->
  leader_map: File_key.t FilenameMap.t ->
  component_map: File_key.t Nel.t FilenameMap.t ->
  recheck_leader_map: bool FilenameMap.t ->
  intermediate_result_callback: ('a merge_result Lazy.t -> unit) ->
  'a merge_stream
