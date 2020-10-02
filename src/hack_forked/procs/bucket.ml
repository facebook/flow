(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base

(****************************************************************************)
(* Moduling Making buckets.
 * When we parallelize, we need to create "buckets" of tasks for the
 * workers.
 * Given a list of files, we want to split it up into buckets such that
 * every worker is busy long enough. If the bucket is too big, it hurts
 * load balancing, if it is too small, the overhead in synchronization time
 * hurts *)
(****************************************************************************)

type 'a bucket =
  | Job of 'a
  | Wait
  | Done

type 'a next = unit -> 'a bucket

let max_size_ref = ref 500

let max_size () = !max_size_ref

let set_max_bucket_size x = max_size_ref := x

let calculate_bucket_size ~num_jobs ~num_workers ~max_size =
  if num_jobs < num_workers * max_size then
    max 1 (1 + (num_jobs / num_workers))
  else
    max_size

let make_ progress_fn bucket_size jobs =
  let i = ref 0 in
  fun () ->
    let bucket_size = min (Array.length jobs - !i) bucket_size in
    progress_fn ~start:!i ~length:bucket_size;
    let result = Array.sub jobs !i bucket_size in
    i := bucket_size + !i;
    Array.to_list result

let make_list ~num_workers ?progress_fn ?max_size jobs =
  let progress_fn =
    Base.Option.value ~default:(fun ~total:_ ~start:_ ~length:_ -> ()) progress_fn
  in
  let max_size = Base.Option.value max_size ~default:!max_size_ref in
  let jobs = Array.of_list jobs in
  let bucket_size = calculate_bucket_size ~num_jobs:(Array.length jobs) ~num_workers ~max_size in
  make_ (progress_fn ~total:(Array.length jobs)) bucket_size jobs

let of_list = function
  | [] -> Done
  | wl -> Job wl

let make ~num_workers ?progress_fn ?max_size jobs =
  let max_size = Base.Option.value max_size ~default:!max_size_ref in
  let maker = make_list ~num_workers ?progress_fn ~max_size jobs in
  (fun () -> of_list (maker ()))

type 'a of_n = {
  work: 'a;
  bucket: int;
  total: int;
}

let make_n_buckets ~buckets ~split =
  let next_bucket = ref 0 in
  fun () ->
    let current = !next_bucket in
    Int.incr next_bucket;
    if current < buckets then
      Job { work = split ~bucket:current; bucket = current; total = buckets }
    else
      Done
