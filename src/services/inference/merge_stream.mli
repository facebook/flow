(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type element = Component of File_key.t Nel.t

type 'a merge_result = (File_key.t * 'a) list

type 'a merge_stream = {
  next: unit -> element list Bucket.bucket;
  merge:
    master_mutator: Context_heaps.Merge_context_mutator.master_mutator ->
    (* merged *)
    'a merge_result ->
    (* accumulator *)
    'a merge_result ->
    (* accumulated results *)
    'a merge_result
}

val make :
  dependency_graph: FilenameSet.t FilenameMap.t ->
  leader_map: File_key.t FilenameMap.t ->
  component_map: File_key.t Nel.t FilenameMap.t ->
  recheck_leader_map: bool FilenameMap.t ->
  intermediate_result_callback: ('a merge_result Lazy.t -> unit) ->
  'a merge_stream
