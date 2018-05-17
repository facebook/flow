(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the "hack" directory of this source tree.
 *
 *)

(* The general protocol for a next function is to return either Wait (indicating
   that workers should wait until more elements are added to the workload), or
   Job of a bucket, or Done to indicate there is no more work. *)
type 'a bucket =
  | Job of 'a
  | Wait
  | Done



type 'a next =
  unit -> 'a bucket

val set_max_bucket_size : int -> unit
val max_size : unit -> int

(* Makes a bucket out of a list, without regard for number of workers or the
   size of the list.  *)
val of_list : 'a list -> 'a list bucket

val make :
  num_workers:int ->
  ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
  ?max_size:int ->
  'a list ->
  'a list next

type 'a of_n = { work: 'a; bucket: int; total: int }

(**
 * Make n buckets (where n = "buckets").
 *
 * The "split" function provides the workload for the k'th bucket.
 *)
val make_n_buckets : buckets:int -> split:(bucket:int -> 'a) ->
  'a of_n next

(* Specialized version to split into lists only. *)
val make_list :
  num_workers:int ->
  ?progress_fn:(total:int -> start:int -> length:int -> unit) ->
  ?max_size:int ->
  'a list ->
  (unit -> 'a list)
