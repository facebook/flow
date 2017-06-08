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
| Skip of filename
| Component of filename list

val make :
  (* dependency graph *)
  FilenameSet.t FilenameMap.t ->
  (* leader map *)
  filename FilenameMap.t ->
  (* component map *)
  filename list FilenameMap.t ->
  (* recheck_leader_map *)
  bool FilenameMap.t ->
  unit ->
  element list MultiWorker.bucket

val join :
  (* intermediate result callback *)
  (Errors.ErrorSet.t Lazy.t -> unit) ->
  (* merged, unchanged *)
  (filename * Errors.ErrorSet.t) list * filename list ->
  (* accumulators *)
  (filename * Errors.ErrorSet.t) list * filename list ->
  (* accumulated results *)
  (filename * Errors.ErrorSet.t) list * filename list
