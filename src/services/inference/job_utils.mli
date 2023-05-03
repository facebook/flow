(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val mk_job :
  mk_check:(unit -> File_key.t -> (('a * 'b) option, 'c) result) ->
  options:Options.t ->
  unit ->
  File_key.t list ->
  (File_key.t * ('b option, 'c) result) list * File_key.t list

val mk_next :
  intermediate_result_callback:('a list -> unit) ->
  max_size:int ->
  workers:MultiWorkerLwt.worker list option ->
  files:File_key.t list ->
  (unit -> File_key.t list Bucket.bucket) * ('a list * File_key.t list -> 'a list -> 'a list)
