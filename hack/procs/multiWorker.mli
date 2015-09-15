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
type 'a nextlist =
  unit -> 'a list

val call :
  Worker.t list option ->
  job:('b -> 'a list -> 'b) ->
  merge:('b -> 'b -> 'b) -> neutral:'b ->
  next:'a nextlist ->
  'b

(* Variant of the above for dynamic workloads. The protocol for a next function
   is to return either None (indicating that workers should wait until more
   elements are added to the workload) or Some list of elements. It will be
   called repeatedly until it returns Some empty list.  *)
type 'a bucket =
| Job of 'a list
| Wait

type 'a nextlist_dynamic =
  unit -> 'a bucket

val call_dynamic :
  Worker.t list option ->
  job:('b -> 'a list -> 'b) ->
  merge:('b -> 'b -> 'b) -> neutral:'b ->
  next:'a nextlist_dynamic ->
  'b
