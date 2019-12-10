(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Hh_bucket = Bucket
open Core_kernel

type worker

val call :
  worker list option ->
  job:('c -> 'a -> 'b) ->
  merge:('b -> 'c -> 'c) ->
  neutral:'c ->
  next:'a Hh_bucket.next ->
  'c Lwt.t

val next :
  ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
  ?max_size:int ->
  worker list option ->
  'a list ->
  'a list Hh_bucket.next

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

val set_report_canceled_callback : (total:int -> finished:int -> unit) -> unit
