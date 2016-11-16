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

type t = {
  queue: Solve_state.t list;
}

let make q =
  {queue = q;}

let add t state =
  (* TODO: make this faster, currently O(n*log(n))
    could be O(log(n) as a priority queue *)
  { queue = (List.sort (state :: t.queue) ~cmp:Solve_state.compare); }

let is_empty t =
  (List.length t.queue) = 0

let peek t =
  match t.queue with
    | hd :: tl -> hd
    | [] -> raise (Failure "Queue is empty when calling peek\n")

let get_next t =
  match t.queue with
    | hd :: tl ->
      {queue = tl}, hd
    | [] -> raise (Failure "Queue is empty when calling get_next\n")
