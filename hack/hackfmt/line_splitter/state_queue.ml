(**
 * Copyright (c) 2016, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "hack" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Core

let queue : Solve_state.t list ref = ref []

let add state =
  let q = state :: !queue in
  (*TODO: make this faster *)
  queue := List.sort q ~cmp:Solve_state.compare

let is_empty () = (List.length !queue) = 0

let peek () =
  match !queue with
    | hd :: tl -> hd
    | [] -> raise (Failure "queue is empty\n")

let get_next () =
  match !queue with
    | hd :: tl ->
      queue := tl;
      hd
    | [] -> raise (Failure "queue is empty\n")
