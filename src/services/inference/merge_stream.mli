(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js

type element =
| Skip of File_key.t
| Component of File_key.t list

type 'a merge_result = (File_key.t * 'a) list

val make :
  (* dependency graph *)
  FilenameSet.t FilenameMap.t ->
  (* leader map *)
  File_key.t FilenameMap.t ->
  (* component map *)
  File_key.t list FilenameMap.t ->
  (* recheck_leader_map *)
  bool FilenameMap.t ->
  unit ->
  element list MultiWorker.bucket

val join :
  (* intermediate result callback *)
  ('a merge_result Lazy.t -> unit) ->
  (* merged, unchanged *)
  'a merge_result * File_key.t list ->
  (* accumulators *)
  'a merge_result * File_key.t list ->
  (* accumulated results *)
  'a merge_result * File_key.t list
