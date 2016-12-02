(**
 * Copyright (c) 2015, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

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

type 'a nextbucket_dynamic =
  unit -> 'a bucket

let make_ bucket_size jobs =
  let i = ref 0 in
  fun () ->
    let bucket_size = min (Array.length jobs - !i) bucket_size in
    let result = Array.sub jobs !i bucket_size in
    i := bucket_size + !i;
    Array.to_list result

let make ~num_workers ?(max_size=500) jobs =
  let jobs = Array.of_list jobs in
  let bucket_size =
    if Array.length jobs < num_workers * max_size
    then max 1 (1 + ((Array.length jobs) / num_workers))
    else max_size
  in
  make_ bucket_size jobs

let make_bucket ~num_workers ?(max_size=500) jobs =
  let make_list = make ~num_workers ~max_size jobs
  in
  fun () -> match make_list () with
      [] -> Done
    | wl -> Job wl
