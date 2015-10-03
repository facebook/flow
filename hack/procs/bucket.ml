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

type 'a t = unit -> 'a list

let make_ bucket_size jobs =
  let i = ref 0 in
  fun () ->
    let bucket_size = min (Array.length jobs - !i) bucket_size in
    let result = Array.sub jobs !i bucket_size in
    i := bucket_size + !i;
    Array.to_list result

let make ?(max_size=500) jobs =
  let jobs = Array.of_list jobs in
  let nbr_procs = GlobalConfig.nbr_procs in
  let bucket_size =
    if Array.length jobs < nbr_procs * max_size
    then max 1 (1 + ((Array.length jobs) / nbr_procs))
    else max_size
  in
  make_ bucket_size jobs
