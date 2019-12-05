(**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception Coalesced_failures of WorkerController.worker_failure list
(** If a worker process fails, this is raised.
 *
 * Note: When one worker process fails, the remaining in-progress workers are checked
 * for completion/failure, and all their failures (non-zero exit code) are coalesced
 * together into one of these exceptions.
 *
 * No further buckets are distributed to workers.
 *
 * Still-in-progress workers are left to their own accord. *)

val coalesced_failures_to_string : WorkerController.worker_failure list -> string

type interrupt_result =
  | Cancel
  | Continue

type 'env interrupt_handler = 'env -> 'env * interrupt_result

type 'env interrupt_config = {
  env: 'env;
  handlers: 'env -> (Unix.file_descr * 'env interrupt_handler) list;
}

type worker_id = int

val no_interrupt : 'a -> 'a interrupt_config

val call :
  WorkerController.worker list -> ('c -> 'a -> 'b) -> ('b -> 'c -> 'c) -> 'c -> 'a Bucket.next -> 'c
(** Can raise Coalesced_failures exception. *)

val call_with_worker_id :
  WorkerController.worker list ->
  (worker_id * 'c -> 'a -> 'b) ->
  (worker_id * 'b -> 'c -> 'c) ->
  'c ->
  'a Bucket.next ->
  'c
(** Invokes merge with a unique worker id.
    Can raise Coalesced_failures exception. *)

val call_with_interrupt :
  WorkerController.worker list ->
  ('c -> 'a -> 'b) ->
  ('b -> 'c -> 'c) ->
  'c ->
  'a Bucket.next ->
  ?on_cancelled:
    ((* [on_cancelled] should be specified if your [next] function ever returns
     [Bucket.Wait], and it should return the list of all jobs that haven't
     finished or started yet. *)
     unit ->
    'a list) ->
  'd interrupt_config ->
  'c * 'd * 'a list

val on_exception : (exn * Utils.callstack -> unit) -> unit
