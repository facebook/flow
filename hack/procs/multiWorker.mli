(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* The protocol for a next function is to return a list of elements.
 * It will be called repeatedly until it returns an empty list.
 *)

module type CALLER = sig
  type 'a result

  val return: 'a -> 'a result

  val multi_threaded_call:
    WorkerController.worker list ->
    ('c -> 'a -> 'b) ->
    ('b -> 'c -> 'c) ->
    'c ->
    'a Bucket.next ->
    'c result
end

module CallFunctor : functor (Caller: CALLER) -> sig
  val call:
    WorkerController.worker list option ->
    job:('c -> 'a -> 'b) ->
    merge:('b -> 'c -> 'c) -> neutral:'c ->
    next:'a Bucket.next ->
    'c Caller.result
end

type worker

(* List of file descriptors that became ready (and triggered interruption),
 * returns whether current job should be cancelled *)
type interrupt_handler = MultiThreadedCall.interrupt_handler

val next :
  ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
  ?max_size: int ->
  worker list option ->
  'a list ->
  'a list Bucket.next

val call :
  worker list option ->
  job:('c -> 'a -> 'b) ->
  merge:('b -> 'c -> 'c) -> neutral:'c ->
  next:'a Bucket.next ->
  'c

val call_with_interrupt :
  worker list option ->
  job:('c -> 'a -> 'b) ->
  merge:('b -> 'c -> 'c) -> neutral:'c ->
  next:'a Bucket.next ->
  interrupt_fds:Unix.file_descr list ->
  interrupt_handler:interrupt_handler ->
  'c * 'a list

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
