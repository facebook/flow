(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Hh_bucket = Bucket

type worker

val call :
  worker list option ->
  blocking:bool ->
  job:('a -> 'b) ->
  merge:('b -> 'c -> 'c) ->
  neutral:'c ->
  next:'a Hh_bucket.next ->
  'c Lwt.t

val fold :
  worker list option ->
  blocking:bool ->
  job:('b -> 'a -> 'b) ->
  merge:('b -> 'b -> 'b) ->
  neutral:'b ->
  next:'a list Hh_bucket.next ->
  'b Lwt.t

val iter :
  worker list option ->
  blocking:bool ->
  job:('a -> unit) ->
  next:'a list Hh_bucket.next ->
  unit Lwt.t

val next :
  ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
  ?max_size:int ->
  worker list option ->
  'a list ->
  'a list Hh_bucket.next

val next2 :
  ?max_size:int -> worker list option -> 'a list -> 'b list -> ('a list * 'b list) Hh_bucket.next

(* Creates a pool of workers. *)
val make :
  worker_mode:Worker.worker_mode ->
  channel_mode:[ `pipe | `socket ] ->
  saved_state:'a ->
  entry:'a WorkerController.entry ->
  nbr_procs:int ->
  gc_control:Stdlib.Gc.control ->
  heap_handle:SharedMem.handle ->
  worker list

val set_report_canceled_callback : (total:int -> finished:int -> unit) -> unit
