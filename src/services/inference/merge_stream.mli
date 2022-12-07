(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Utils_js

type element = Component of File_key.t Nel.t

type 'a merge_result = (File_key.t * bool * 'a) list

type 'a t

val create :
  num_workers:int ->
  sig_dependency_graph:FilenameGraph.t ->
  components:File_key.t Nel.t list ->
  recheck_set:FilenameSet.t ->
  'a t

val update_server_status : 'a t -> unit

val next : 'a t -> unit -> element list Bucket.bucket

val merge : 'a t -> 'a merge_result -> 'a list -> 'a list

val total_files : 'a t -> int

val skipped_count : 'a t -> int

val sig_new_or_changed : 'a t -> FilenameSet.t
