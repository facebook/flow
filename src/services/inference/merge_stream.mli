(**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type element = Component of File_key.t Nel.t

type 'a merge_result = (File_key.t * 'a) list

val make :
  (* dependency graph *)
  FilenameSet.t FilenameMap.t ->
  (* leader map *)
  File_key.t FilenameMap.t ->
  (* component map *)
  File_key.t Nel.t FilenameMap.t ->
  (* recheck_leader_map *)
  bool FilenameMap.t ->
  unit ->
  element list Bucket.bucket

val join :
  (* intermediate result callback *)
  ('a merge_result Lazy.t -> unit) ->
  (* merged *)
  'a merge_result ->
  (* accumulator *)
  'a merge_result ->
  (* accumulated results *)
  'a merge_result
