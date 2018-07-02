(**
 * Copyright (c) 2018, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

type worker

val call :
  worker list option ->
  job:('c -> 'a -> 'b) ->
  merge:('b -> 'c -> 'c) -> neutral:'c ->
  next:'a Bucket.next ->
  'c Lwt.t

val next :
  ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
  ?max_size: int ->
  worker list option ->
  'a list ->
  'a list Bucket.next

(* Creates a pool of workers. *)
val make:
  (** See docs in WorkerController.worker for call_wrapper. *)
  ?call_wrapper: WorkerController.call_wrapper ->
  saved_state : 'a ->
  entry       : 'a WorkerController.entry ->
  nbr_procs   : int ->
  gc_control  : Gc.control ->
  heap_handle : SharedMem.handle ->
    worker list
