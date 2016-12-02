(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

(* The general protocol for a next function is to return either Wait (indicating
   that workers should wait until more elements are added to the workload), or
   Job of a bucket, or Done to indicate there is no more work. *)
type 'a bucket =
  | Job of 'a
  | Wait
  | Done

type 'a nextbucket_dynamic =
  unit -> 'a bucket

val make_bucket : num_workers:int -> ?max_size:int -> 'a list ->
  'a list nextbucket_dynamic

(* Specialized version to split into lists only. *)
val make : num_workers:int -> ?max_size:int -> 'a list -> (unit -> 'a list)
