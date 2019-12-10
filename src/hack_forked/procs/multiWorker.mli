(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Hh_bucket = Bucket
open Core_kernel

(* The protocol for a next function is to return a list of elements.
 * It will be called repeatedly until it returns an empty list.
 *)

module type CALLER = sig
  type 'a result

  val return : 'a -> 'a result

  val multi_threaded_call :
    WorkerController.worker list ->
    (WorkerController.worker_id * 'c -> 'a -> 'b) ->
    (WorkerController.worker_id * 'b -> 'c -> 'c) ->
    'c ->
    'a Hh_bucket.next ->
    'c result
end

module CallFunctor (Caller : CALLER) : sig
  val call :
    WorkerController.worker list option ->
    job:(WorkerController.worker_id * 'c -> 'a -> 'b) ->
    merge:(WorkerController.worker_id * 'b -> 'c -> 'c) ->
    neutral:'c ->
    next:'a Hh_bucket.next ->
    'c Caller.result
end

type worker

(* List of file descriptors that became ready (and triggered interruption),
 * returns whether current job should be cancelled *)
type 'a interrupt_config = 'a MultiThreadedCall.interrupt_config

val next :
  ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
  ?max_size:int ->
  worker list option ->
  'a list ->
  'a list Hh_bucket.next

(* Can raise MultiThreadedCall.Coalesced_failures unless in single-threaded mode. *)
val call :
  worker list option ->
  job:('c -> 'a -> 'b) ->
  merge:('b -> 'c -> 'c) ->
  neutral:'c ->
  next:'a Hh_bucket.next ->
  'c

(* Can raise MultiThreadedCall.Coalesced_failures unless in single-threaded mode. *)
val call_with_worker_id :
  worker list option ->
  job:(WorkerController.worker_id * 'c -> 'a -> 'b) ->
  merge:(WorkerController.worker_id * 'b -> 'c -> 'c) ->
  neutral:'c ->
  next:'a Hh_bucket.next ->
  'c

val call_with_interrupt :
  ?on_cancelled:
    ((* [on_cancelled] should be specified if your [next] function ever returns
     [Hh_bucket.Wait], and it should return the list of all jobs that haven't
     finished or started yet. *)
     unit ->
    'a list) ->
  worker list option ->
  job:('c -> 'a -> 'b) ->
  merge:('b -> 'c -> 'c) ->
  neutral:'c ->
  next:'a Hh_bucket.next ->
  interrupt:'d interrupt_config ->
  'c * 'd * 'a list

(* Creates a pool of workers. *)
val make :
  ?call_wrapper:
    (* See docs in WorkerController.worker for call_wrapper. *)
    WorkerController.call_wrapper ->
  saved_state:'a ->
  entry:'a WorkerController.entry ->
  nbr_procs:int ->
  gc_control:Gc.control ->
  heap_handle:SharedMem.handle ->
  worker list
