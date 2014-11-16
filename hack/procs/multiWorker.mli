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

val make_bucket: 'a list -> ('b -> 'a list)

val call :
  Worker.t list option ->
  job:('b -> 'a list -> 'b) ->
  merge:('b -> 'b -> 'b) -> neutral:'b ->
  next:'a nextlist ->
 'b

module type Proc = sig
  type env
  type input
  type output

  val neutral: output
  val job: env -> output -> input list -> output
  val merge: output -> output -> output
  val make_next: input list -> (unit -> input list)
end

module type S = sig
  type env
  type input
  type output

  val run: Worker.t list option -> env -> input list -> output
end

module Make: functor (Proc: Proc) -> S 
with type env = Proc.env
with type input = Proc.input 
with type output = Proc.output
