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

val next :
  ?max_size: int ->
  Worker.t list option ->
  'a list ->
  'a nextlist

val call :
  Worker.t list option ->
  job:('b -> 'a list -> 'b) ->
  merge:('b -> 'b -> 'b) -> neutral:'b ->
  next:'a nextlist ->
  'b

(* See definition in Bucket *)
type 'a bucket = 'a Bucket.bucket =
  | Job of 'a
  | Wait
  | Done

val call_dynamic :
  Worker.t list option ->
  job:('b -> 'a -> 'b) ->
  merge:('b -> 'b -> 'b) -> neutral:'b ->
  next:'a Bucket.nextbucket_dynamic ->
  'b
