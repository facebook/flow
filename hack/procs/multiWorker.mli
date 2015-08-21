(**
 * Copyright (c) 2014, Facebook, Inc.
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
